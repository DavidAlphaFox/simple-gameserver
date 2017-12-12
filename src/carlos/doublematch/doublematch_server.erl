-module(doublematch_server).
-behaviour(gen_server).
-include("def_role.hrl").
-include("def_fight.hrl").
-include("def_doublematch.hrl").
-include("def_carlos.hrl").
-include("def_mail.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(once_max, 500). %% 一次最多处理多少条
%% -define(read_timeout, 900). %% 15m没有人读取数据的话，则
%% -define(write_timeout, 300).    %% 数据被修改，5m之后，则将数据写入数据库。注：十分钟内如果数据再次修改不刷新时间戳
-define(read_timeout, 900). %% 15m没有人读取数据的话，则
-define(write_timeout, 60).    %% 数据被修改，5m之后，则将数据写入数据库。注：十分钟内如果数据再次修改不刷新时间戳
-define(persist_interval, 45000). %% 20s检查一次数据是否有需要写入的

-define(pd_session,pd_session).  % 记录当前执行的赛季，如果未切换成功，依然保持之前赛季编号
-define(pd_local_rank,pd_local_rank).  % 记录当前赛季本服排名
-define(pd_1w_rank,pd_1w_rank).  % 记录非上榜名词

-define(match_state_standby,1).
-define(match_state_sign,2).

-define(req_type_rank,1).
-define(req_type_reclist,2).
-define(req_type_self_reclist,3).
-define(req_type_local_rank,4).

% process state,数据不保存，匹配服获取
-record(state, {session = 0     %暂时没用
               ,rank_version = 0
               ,rank_req_list = []
               ,rank_req_st = 0
               ,fightrec_req_list = []}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start( ) ->
    {ok,_}=
        supervisor:start_child(world_sup, 
                               {?MODULE,
                                {?MODULE, start_link, []},
                                permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% Game API Function Definitions
%% ------------------------------------------------------------------

% 获取个人数据
get_dm_info_msg()->
    erlang:send(?MODULE, {get_dm_info_msg,role_data:get_roleID()}).

get_dm_friend_rank(RoleID,FriendList)->
    erlang:send(?MODULE, {get_dm_friend_rank,RoleID,FriendList}).

% 当前跨服排名信息
get_dm_rank_info(BeginPosition,Length)->
    erlang:send(?MODULE, {get_rank_req,role_data:get_roleID(),?req_type_rank,BeginPosition,Length}).

get_dm_role_rank(BeginPosition,Length)->
    erlang:send(?MODULE, {get_rank_req,role_data:get_roleID(),?req_type_local_rank,BeginPosition,Length}).

get_dm_old_rank(BeginPosition0,Length)->
    BeginPosition = erlang:max(1, BeginPosition0),
    List1 = ets:tab2list(?ETS_DM_OLD_RANK),
%%     CurL = erlang:length(List1),
%%     List2 = if
%%         CurL < BeginPosition ->
%%             [];
%%         BeginPosition + Length > CurL ->
%%             lists:sublist(List1, BeginPosition, CurL - BeginPosition + 1);
%%         true ->
%%             lists:sublist(List1, BeginPosition, Length)
%%     end,
    send_rank(role_data:get_roleID(),?OLD_ALL_RANK,List1,BeginPosition0, Length).
%%     ?unicast(role_data:get_roleID(),#sc_doublematch_rankinfo{result=1,type=?OLD_ALL_RANK,position=BeginPosition,ranklist=List2,length=erlang:length(List2)}).

% 当前跨服录像简报
get_dm_rec_list(BeginIndex, 1, Length)->
    erlang:send(?MODULE, {get_rank_req, role_data:get_roleID(), ?req_type_reclist, BeginIndex, Length});
get_dm_rec_list(BeginIndex, 2, Length)->
    erlang:send(?MODULE, {get_rank_req, role_data:get_roleID(), ?req_type_self_reclist, BeginIndex, Length}).
%%     erlang:send(?MODULE, {get_rank_req,role_data:get_roleID(),?req_type_reclist}).

% 当前某个玩家的录像信息。其他服务器的且未进排行榜的返回undefined，注意处理undefined情况
% 以队长RoleID为索引，查看某个排名上玩家的战斗记录。
% 极端情况，点击的同时，排名可能已经变化。依然返回该名队长的参战的最后一场战斗的信息。
get_dm_fight_record(RecID)->
    RoleID = role_data:get_roleID(),
    if
        RecID < 100 andalso RecID > 0 ->
            DmData = case ets:lookup(?ETS_DM_INFO_LIST, RoleID) of
                [] ->
                    db_sql:get_doublematch_info(RoleID);
                [DmDbRecord] when erlang:is_record(DmDbRecord, dm_db_record) ->
                    DmDbRecord#dm_db_record.dm_data % write_ts maybe 0 or more than 0
            end,
            case DmData#dm_data.fight_rec of
                RecList when erlang:is_list(RecList) ->
                    RecListLength = erlang:length(RecList),
                    if
                        RecID > RecListLength ->
                            ?sendself(#sc_doublematch_replay{result=3
                                                   ,fight_info=#sc_fight_double_request{fighterList=[]
                                                                                       ,actionList=[]
                                                                                       ,result=false}
                                                   ,dice_numA=1
                                                   ,dice_numB=1});
                        true ->
                            {Last,_Ts} = lists:nth(RecID, RecList), %新版的战报不会有旧版的p_fight v260
                            ?sendself(Last)
                    end;
%%                 Rec when erlang:is_record(Rec, sc_doublematch_fight)->
%%                     ?sendself(Rec);
                _ ->
                    ?sendself(#sc_doublematch_replay{result=3
                                           ,fight_info=#sc_fight_double_request{fighterList=[]
                                                                               ,actionList=[]
                                                                               ,result=false}
                                           ,dice_numA=1
                                           ,dice_numB=1})
            end;
        true ->
            erlang:send(?MODULE, {get_rec_req,RoleID,RecID})
    end.
            
get_buy_time(RoleID)->
    DmData = case ets:lookup(?ETS_DM_INFO_LIST, RoleID) of
        [] ->
            db_sql:get_doublematch_info(RoleID);
        [DmDbRecord] when erlang:is_record(DmDbRecord, dm_db_record) ->
            DmDbRecord#dm_db_record.dm_data % write_ts maybe 0 or more than 0
    end,
    DmData#dm_data.already_buy.

% 获得当前玩家次数、积分、段位、录像信息
% 仅能获取本服玩家数据
convert_rank(DmData) ->
    RolePub = role_lib:get_rolePublic(DmData#dm_data.role_id),
    #dm_rank{role_id = DmData#dm_data.role_id
            ,server_id = DmData#dm_data.server_id
            ,name  = RolePub#rolePublic.roleName 
            ,index = DmData#dm_data.index 
            ,remain_time = get_time(DmData)
            ,already_buy = DmData#dm_data.already_buy 
            ,level = RolePub#rolePublic.level   
            ,rank = DmData#dm_data.rank  
            ,score = DmData#dm_data.score
            ,isMale=RolePub#rolePublic.isMale 
            ,title = RolePub#rolePublic.title  
            ,head =RolePub#rolePublic.head
            ,rec_id=0
            ,punit = []
            ,ts = 0
            ,score_change = 0
            ,vip=role_lib:cacl_vip_info(RolePub#rolePublic.viplevel, RolePub#rolePublic.svipLevel)}.

% 报名状态
doublematch_state(RoleID)->
    case ets:lookup(?ETS_DM_SIGN_LIST, RoleID) of
        [] ->
            ?match_state_standby;
        [_] ->
            ?match_state_sign
    end.

% 请求报名，仅在role_server进程被调用
% 包含两种情况，一个人报名，TeamId为负数，由匹配服匹配队友，匹配敌人
% 两个人报名，TeamId队伍ID，带布阵信息，由匹配服匹配敌人
% --类似于--carlos_server:send_carlos_war_msg({sign, RoleID, TeamId, TeamMember, Level});
% TeamMember : [{RoleId,#double_team{}}]
doublematch_sign(RoleID, TeamId, TeamMember)->
    ?INFO("doublematch_server:doublematch_sign ~w ",[[RoleID, TeamId, TeamMember]]),
    %TimerRef = role_doublematch:set_sign_timeout(), %% 暂时不用超时机制
    TimerRef = ?undefined,
    erlang:send(?MODULE, {dm_sign,RoleID, TeamId, TeamMember,TimerRef}).

doublematch_cancel(RoleID)->
    erlang:send(?MODULE, {dm_cancel,RoleID}).

doublematch_add_times(RoleID)->
    erlang:send(?MODULE, {add_times,RoleID}).

fix_20170103_reward(IsDebug)->
    erlang:send(?MODULE, {fix_20170103_reward,IsDebug}).

% call back msg 
% 1、返回匹配及战斗结果
% 2、匹配超时

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    process_flag(trap_exit,true),
    send_to_rank({get_rank_req,erlang:self(),0}),
    send_to_rank({get_only_old_rank_req,erlang:self()}),
    set_persist_interval(),
    case db_sql:get_etc(?DB_ETC_KEY_DOUBLEMATCH_SERVER) of
        {Session} ->
            put(?pd_session, Session);
        _ ->
            put(?pd_session, get_session())
    end,
    put(?pd_local_rank, db_sql:get_local_rank(get(?pd_session))),
    put(?pd_1w_rank, []),
    {ok, #state{rank_version = 0
               ,rank_req_list = []
               ,rank_req_st = 0
               ,fightrec_req_list = []}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info({get_dm_info_msg,RoleID}, State) ->
    #dm_data{rank=Rank,score=Score,already_buy=AlreadyBuyTimes,index=RankIndex,before_score=BeforeScore0} = RoleDmInfo = get_role_local_dm(RoleID),
    Cost = role_doublematch:get_buy_cost(AlreadyBuyTimes+1),
    TimeStamp = get_season_endtime(),
    MaxFreeTime = data_doublematch:get(daily_time),
    {_,GRank,GLevel,_,_} = data_doublematch:get({rank,Rank}),
    RankTop = data_doublematch:get(rank_top),
    {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
    RankScore = if
                    Rank >= RankTop ->
                        Score - TopScore;
                    true ->
                        Score rem 100
                end,
    LocalRank = case lists:keysearch(RoleID, #dm_rank.role_id, get(?pd_local_rank)) of
                    {value,LocalRankInfo} ->
                        LocalRankInfo#dm_rank.index;
                    false ->
                        0
                end,
    {BeforeIndex,BeforeRank,BeforeScore} = case ets:lookup(?ETS_DM_OLD_RANK,RoleID) of
                    [#dm_rank{role_id=RoleID,index=BI,rank  = BR,score = BScoreInRank}] ->
                        {BI,BR,BScoreInRank};
                    [] ->
                        {0,1,BeforeScore0}
                end,
    {_,BeforeGRank,BeforeGLevel,_,_} = data_doublematch:get({rank,BeforeRank}),
    BeforeRankScore = if
                    BeforeRank >= RankTop ->
                        BeforeScore - TopScore;
                    true ->
                        BeforeScore rem 100
                end,
    ?unicast(RoleID,#sc_doublematch_roleinfo{result=1,grade=GRank,score=RankScore,times=get_time(RoleDmInfo)
                                            ,next_buy_cost=Cost,rank=RankIndex,grade_level=GLevel
                                            ,timestamp=TimeStamp,max_free_time=MaxFreeTime,local_rank=LocalRank
                                            ,before_rank = BeforeIndex
                                            ,before_grade=BeforeGRank,before_grade_level=BeforeGLevel,before_score=BeforeRankScore}),
    {noreply, State};
handle_info({get_dm_friend_rank,RoleID,FriendList}, State) ->
    FriendRankUnitList = lists:foldl(fun(RID,Acc)->
        case doublematch_server:get_role_local_dm(RID) of
            ?undefined->
                Acc;
            DMInfo when  DMInfo#dm_data.rank =:= 0->
                Acc;
            DMInfo->
                [role_doublematch:transformDMRank2RankUnit(convert_rank(DMInfo))|Acc]
        end
    end,[],[RoleID|FriendList]),
    FriendRankUnitListSort = lists:sort(fun(A,B) -> 
                                      if
%%                                           A#p_doublematch_rank_unit.rank < B#p_doublematch_rank_unit.rank ->
%%                                               true;
%%                                           A#p_doublematch_rank_unit.rank > B#p_doublematch_rank_unit.rank ->
%%                                               false;
                                          A#p_doublematch_rank_unit.grade  > B#p_doublematch_rank_unit.grade  ->
                                              true;
                                          A#p_doublematch_rank_unit.grade  < B#p_doublematch_rank_unit.grade  ->
                                              false;
                                          A#p_doublematch_rank_unit.gradeLevel  < B#p_doublematch_rank_unit.gradeLevel  ->
                                              true;
                                          A#p_doublematch_rank_unit.gradeLevel  > B#p_doublematch_rank_unit.gradeLevel  ->
                                              false;
                                          A#p_doublematch_rank_unit.score  > B#p_doublematch_rank_unit.score  ->
                                              true;
                                          A#p_doublematch_rank_unit.score  < B#p_doublematch_rank_unit.score  ->
                                              false;
                                          A#p_doublematch_rank_unit.roleID < B#p_doublematch_rank_unit.roleID ->
                                              true;
                                          A#p_doublematch_rank_unit.roleID > B#p_doublematch_rank_unit.roleID ->
                                              false
                                      end
            end, FriendRankUnitList),
    ?unicast(RoleID,#sc_doublematch_rankinfo{result=1,type=?FRIEND_RANK,position=0
                                            ,ranklist=refresh_friend_rank(FriendRankUnitListSort)
                                            ,length=erlang:length(FriendRankUnitListSort)}),
    {noreply, State};
handle_info({get_only_old_rank_req,OldRankList}, State) ->
    ets:delete_all_objects(?ETS_DM_OLD_RANK),
    ets:insert(?ETS_DM_OLD_RANK,OldRankList),
    {noreply, State};
%% ---------------------------请求排名数据--------------------------------------------
handle_info({get_rank_req, RoleID, ?req_type_self_reclist, BeginIndex, Length}, State) ->
    DmData = get_role_local_dm(RoleID),
    case DmData#dm_data.fight_rec of
        ?undefined ->
            ?unicast(RoleID,#sc_doublematch_record_list{result=1,recordlist=[],beginindex=0,type=?ROLE_REC_LIST,length=0});
        FightRecList when erlang:is_list(FightRecList) ->
            {ListR,_} = lists:foldl(fun({Rec,Ts},{AccList,Index}) -> 
                                        {[#p_record_unit{recordID=Index
%%                                                        ,recordteammember=[E#p_doublematch_rank_unit{grade=min(5,E#p_doublematch_rank_unit.grade)}||E<-Rec#sc_doublematch_fight.rolelist]
                                                       ,recordteammember=Rec#sc_doublematch_fight.rolelist
                                                       ,timestamp=Ts}|AccList],Index+1} 
                                end, {[],1}, DmData#dm_data.fight_rec),
            ?unicast(RoleID,#sc_doublematch_record_list{result=1,recordlist=lists:reverse(ListR),beginindex=0,type=?ROLE_REC_LIST,length=erlang:length(ListR)})
    end,
    {noreply, State};
handle_info({get_rank_req, RoleID, ?req_type_local_rank, BeginIndex, Length}, State) ->
    WangzheList = [E#dm_rank.role_id||E<-ets:tab2list(?ETS_DM_RANK_DATA),E#dm_rank.rank =:= data_doublematch:get(rank_top) + 1],
    LocalRank = local_rank_set_rank(get(?pd_local_rank),WangzheList,get(?pd_1w_rank)),
    send_rank(RoleID,?LOCAL_REGION_RANK,LocalRank, BeginIndex, Length),
    {noreply, State};
handle_info({get_rank_req, RoleID, Type, BeginIndex, Length}, #state{rank_req_st = ST
                                                                    ,rank_req_list = ReqList} = State) ->
    Now = util:now(),
    Interval = data_doublematch:get(rank_sync_interval),
    NewState = if
        % 刚刚更新过，用缓存数据
        Now < ST + Interval ->
            RankList = ets:tab2list(?ETS_DM_RANK_DATA),
            case Type of
                ?req_type_rank ->
                    send_rank(RoleID,?All_REGION_RANK,RankList,BeginIndex, Length);
                ?req_type_reclist ->
                    send_rec_list(RoleID,RankList);
                _ ->
                    ignore
            end,
            State;
        % 空的话说明没有发送过请求
        ReqList =:= [] ->
            ?INFO("doublematch_server send get_rank_req to rank"),
            send_to_rank({get_rank_req,erlang:self(),State#state.rank_version}),
            State#state{rank_req_st = Now ,rank_req_list = [{RoleID,Type,BeginIndex, Length}]};
        % 非空的话说明发送过请求了，不用重复发，等待结果
        true ->
            State#state{rank_req_list = [{RoleID,Type,BeginIndex, Length}|ReqList]}
    end,       
    {noreply, NewState};
% 排行没变化用旧数据
handle_info({get_rank_res,?undefined}, #state{rank_req_list = ReqList} = State) ->
    ?INFO("get_rank_res undefined"),
    Now = util:now(),
    RankList = ets:tab2list(?ETS_DM_RANK_DATA),
    lists:foreach(fun({RID,Type,BeginPosition,Length}) ->
            case Type of
                ?req_type_rank ->
                    send_rank(RID,?All_REGION_RANK,RankList,BeginPosition,Length);
                ?req_type_reclist ->
                    send_rec_list(RID,RankList);
                _ ->
                    ignore
            end
        end, ReqList),
    {noreply, State#state{rank_req_st = Now ,rank_req_list = []}};
% 排行有变化用新数据
handle_info({get_rank_res,Version,RankList,RankList1w}, #state{rank_req_list = ReqList} = State) ->
    ?INFO("get_rank_res v:~w",[Version]),
    Now = util:now(),
    ets:delete_all_objects(?ETS_DM_RANK_DATA),
    ets:insert(?ETS_DM_RANK_DATA,RankList),    
    lists:foreach(fun({RID,Type,BeginPosition,Length}) -> 
            case Type of
                ?req_type_rank ->
                    send_rank(RID,?All_REGION_RANK,RankList,BeginPosition,Length);
                ?req_type_reclist ->
                    send_rec_list(RID,RankList);
                _ ->
                    ignore
            end
        end, ReqList),
    
    % 更新一下1000~10000的排名
    put(?pd_1w_rank, RankList1w),
%%     lists:foreach(fun(RID)->
%%             case lists:keytake(RID, 1, RankList1w) of
%%                 {value,{RID,I}} ->
%%                     DD = get_role_local_dm(RID),
%%                     NewDD = DD#dm_data{index=I},
%%                     set_role_local_dm(RID,NewDD);
%%                 false ->
%%                     ignore
%%             end
%%         end, ets:tab2list(?ETS_ROLE_ONLINE)),
    {noreply, State#state{rank_req_st = Now ,rank_req_list = [],rank_version=Version}};

%% ---------------------------请求录像详细数据--------------------------------------------
% TODO fightrec_req_list的元素里要加个时间戳，防止请求没有回复，请求不会重复发，会导致某个录像永远得不到了。
handle_info({get_rec_req,RoleID,RecID}, #state{fightrec_req_list = AllReqList} = State) ->
    NewAllReqList = case ets:lookup(?ETS_DM_FIGHT_REC, RecID) of
        []->
            case lists:keytake(RecID, 1, AllReqList) of
                % 非空的话说明发送过请求了，不用重复发，等待结果
                {value,{RecID,ReqList},OtherReqList} ->
                    [{RecID,[RoleID|ReqList]}|OtherReqList];
                false ->
                    send_to_rank({get_rec_req,erlang:self(),RecID}),
                    [{RecID,[RoleID]}|AllReqList]
            end;
        [{RecID,FightRecord,AtkRandInt,DefRandInt}] ->
            Msg = #sc_doublematch_replay{result=1
                                        ,fight_info=FightRecord
                                        ,dice_numA=AtkRandInt
                                        ,dice_numB=DefRandInt},
            ?unicast(RoleID, Msg),
            State#state.fightrec_req_list
    end,
    {noreply, State#state{fightrec_req_list = NewAllReqList}};
handle_info({get_rec_res,RecID,?undefined}, #state{fightrec_req_list = AllReqList} = State) ->
    NewAllReqList = case lists:keytake(RecID, 1, AllReqList) of
        {value,{RecID,ReqList},OtherReqList} ->
            Msg = #sc_doublematch_replay{result=2
                                        ,fight_info=#sc_fight_double_request{}
                                        ,dice_numA=1
                                        ,dice_numB=1},
            lists:foreach(fun(RID)-> 
                    ?unicast(RID, Msg)
                end, ReqList),
            OtherReqList;
        false ->
            State#state.fightrec_req_list
    end,
    {noreply, State#state{fightrec_req_list = NewAllReqList}};
handle_info({get_rec_res,RecID,FightRecord,AtkRandInt,DefRandInt}, #state{fightrec_req_list = AllReqList} = State) ->
    ets:insert(?ETS_DM_FIGHT_REC, {RecID,FightRecord,AtkRandInt,DefRandInt}),
    NewAllReqList = case lists:keytake(RecID, 1, AllReqList) of
        {value,{RecID,ReqList},OtherReqList} ->
            Msg = #sc_doublematch_replay{result=1
                                        ,fight_info=FightRecord
                                        ,dice_numA=AtkRandInt
                                        ,dice_numB=DefRandInt},
            lists:foreach(fun(RID)->
                    ?unicast(RID, Msg)
                end, ReqList),
            OtherReqList;
        false ->
            State#state.fightrec_req_list
    end,
    {noreply, State#state{fightrec_req_list = NewAllReqList}};

%% ---------------------------报名相关--------------------------------------------
handle_info({dm_sign,RoleID, TeamId, TeamMember,TimerRef}, State) ->
    TeamMemberL = erlang:length(TeamMember),
    RoleIDList =  [E||{E,_}<-TeamMember],
    [{LeaderRoleId,_}|_] = TeamMember,
    CheckRes = if
        % 只有队长才才能报名，而且对战要在队伍list最前面
        LeaderRoleId /= RoleID ->
            {false,3};
        % 人数不对
        TeamId < 0 andalso TeamMemberL /= 1 ->
            {false,3};
        % 人数不对
        TeamId > 0 ->
            if
                TeamMemberL =:= 1 ->
                    {false,3};
                true ->
                    [_,{FollowRoleId,_}] = TeamMember,
                    LS = get_role_local_dm(LeaderRoleId),
                    FS = get_role_local_dm(FollowRoleId),
                    case check_rank_for_match(LS#dm_data.rank,FS#dm_data.rank) of
                        true ->
                            check_team_sign(RoleIDList);
                        false ->
                            {false,5}
                    end
            end;
        true ->
            check_team_sign(RoleIDList)
    end,
    case CheckRes of
        true ->
            SignID = 
                case TeamId of
                    -1 ->
                        ?make_id(-RoleID, 1);
                    _ ->
                        ?make_id(TeamId, TeamMemberL)
                end,       
            SignInfoList = lists:foldr(fun({RID,Team},AccList)-> 
                            set_single_state(RID,?match_state_sign,SignID,RoleIDList,TimerRef),
                            ?unicast(RID, #sc_doublematch_sign{result=1}),
                            RolePub = role_lib:get_rolePublic(RID),
                            DmData = case get_role_local_dm(RID) of
                                         DmData0 when DmData0#dm_data.rank /= 0 ->
                                             DmData0;
                                         DmData0 when DmData0#dm_data.rank =:= 0 ->
                                             DmData0#dm_data{rank = 1}
                                     end,
                            Vip = role_lib:cacl_vip_info(RolePub#rolePublic.viplevel,RolePub#rolePublic.svipLevel),
                            [#dm_sign_info{role_id = RID
                                         ,double_team = Team
                                         ,server_id = data_setting:get(server_id)
                                         ,name = RolePub#rolePublic.roleName 
                                         ,level = RolePub#rolePublic.level  
                                         ,isMale= RolePub#rolePublic.isMale        %% 用于显示头像的性别信息
                                         ,title = RolePub#rolePublic.title         %% 用于显示头像
                                         ,head = RolePub#rolePublic.head          %% 用于显示头像
                                         ,dm_data = DmData,vip=Vip}|AccList]
                        end, [], TeamMember),
            send_to_match({dm_sign, SignInfoList});
        {false,Reason} ->
            ?unicast(RoleID, #sc_doublematch_sign{result=Reason})
    end,
    {noreply, State};
handle_info({dm_cancel,RoleID}, State) ->
    case ets:lookup(?ETS_DM_SIGN_LIST, RoleID) of
        [] ->
            case role_lib:is_online(RoleID) of
                true ->
                    ?unicast(RoleID,#sc_doublematch_sign_cancel{result = 1});
                false ->
                    ignore
            end;
        [{RoleID,_SignID,RoleIDList,_TimerRef}] ->
            [RoleLeader|_] = RoleIDList,
            if
                %队长才可以取消
                RoleLeader =:= RoleID ->
                    send_to_match({dm_cancel, RoleIDList,data_setting:get(server_id)});
                true ->
                    ?unicast(RoleID,#sc_doublematch_sign_cancel{result = 2})
            end     
    end,
    {noreply, State};
handle_info({dm_cancel_response,RoleIDList}, State) ->
    lists:foreach(fun(RID)->
                          case ets:lookup(?ETS_DM_SIGN_LIST, RID) of
                              []->
                                  ?ERR("dm_cancel_response ETS_DM_SIGN_LIST data lost ~w",[RID]);
                              [{RID,_SignID,RoleIDList,TimerRef}]  ->
                                  ets:delete(?ETS_DM_SIGN_LIST, RID),
                                  case role_lib:is_online(RID) of
                                      true when TimerRef /= ?undefined->
                                          catch role_lib:send_server(RID, {clear_dm_timer,TimerRef});
                                      _ ->
                                          ignore
                                  end
                          end,
                          case role_lib:is_online(RID) of
                              true ->
                                  ?unicast(RID,#sc_doublematch_sign_cancel{result = 1});
                              false->
                                  ignore
                          end
                  end, RoleIDList),
    {noreply, State};  
handle_info({fight_rec,RoleID,IsAtk,IsWin,FightRecord,ChangeScore,AtkRandInt,DefRandInt,PUnit,TS}, State) ->
    DmData = get_role_local_dm(RoleID),
    % 分辨进攻方
    NewSroce0 = case IsAtk of
                   true ->
                       DmData#dm_data.score + ChangeScore;
                   false ->
                       DmData#dm_data.score - ChangeScore
               end,
    NewSroce = erlang:max(0, NewSroce0),
    NewRank = check_rank_by_sroce(NewSroce), % 计算段位
%%     Msg = #sc_doublematch_fight{rolelist=[E#p_doublematch_rank_unit{grade=min(5,E#p_doublematch_rank_unit.grade)}||E<-PUnit]
    Msg = #sc_doublematch_fight{rolelist=PUnit
                               ,dice_numA=AtkRandInt
                               ,dice_numB=DefRandInt
                               ,fight_info=FightRecord
                               ,score_change=NewSroce0-DmData#dm_data.score},
    case role_lib:is_online(RoleID) of
        true ->
            ?unicast(RoleID,Msg);
        false ->
            ?ERR("offline too fast")
    end,
    case check_time(DmData) of
        {true, NewDmData0} ->
            NewFightRec0 = case NewDmData0#dm_data.fight_rec of
                               ?undefined ->
                                   [{Msg,TS}];
                               NewFightRec00 when erlang:is_list(NewFightRec00) ->
                                   [{Msg,TS}|NewFightRec00]
                           end,
            NewFightRecLength = erlang:length(NewFightRec0),
            MaxFightRecLength = data_doublematch:get(self_rec_length),
            NewFightRecList = if
                        MaxFightRecLength < NewFightRecLength ->
                            {NewFightRecList0,_} = lists:split(data_doublematch:get(self_rec_length), NewFightRec0),
                            NewFightRecList0;
                        true ->
                            NewFightRec0
                  end,                    
            NewDmData = NewDmData0#dm_data{score=NewSroce
                                          ,rank=NewRank
                                          ,fight_rec=NewFightRecList},
            set_role_local_dm(RoleID,NewDmData),
            #rolePublic{level = RoleLevel, roleName = RoleName, isMale = IsMale
                       ,title = Title, head = Head,viplevel=VipLevel,svipLevel=SVipLevel} = role_lib:get_rolePublic(RoleID),
            NewDmRank = 
                #dm_rank{role_id = RoleID
                        ,server_id = data_setting:get(server_id)
                        ,name  = RoleName
                        ,index = 0  % 稍后填写
                        ,remain_time = get_time(NewDmData)
                        ,already_buy = NewDmData#dm_data.already_buy
                        ,level = RoleLevel
                        ,rank = NewDmData#dm_data.rank
                        ,score = NewSroce
                        ,isMale = IsMale
                        ,title = Title
                        ,head = Head
                        ,rec_id=0   % 本地排名不需要录像回放
                        ,punit = PUnit
                        ,ts=TS
                        ,score_change = NewSroce0-DmData#dm_data.score
                        ,vip=role_lib:cacl_vip_info(VipLevel,SVipLevel)},
            refresh_local_rank(NewDmRank),
			?CATCH(role_task:send_dispach(RoleID,{dispach_task,double_fight_fight,1})),
            case ets:lookup(?ETS_DM_SIGN_LIST, RoleID) of
                []->
                    ?ERR("dm_cancel_response ETS_DM_SIGN_LIST data lost ~w",[RoleID]);
                [{RoleID,_SignID,_RoleIDList,TimerRef}]  ->
                    ets:delete(?ETS_DM_SIGN_LIST, RoleID),
                    case role_lib:is_online(RoleID) of
                        true when TimerRef /= ?undefined->
                            catch role_lib:send_server(RoleID, {clear_dm_timer,TimerRef});
                        _ ->
                            ignore
                    end
            end;
        false ->
            ?ERR("dm fight time less zero. old:~w",[DmData])
    end,
    {noreply, State};
handle_info({add_times,RoleID}, State) ->
    Dm = get_role_local_dm(RoleID),
    NewRemainTime = Dm#dm_data.remain_time_buy + data_doublematch:get(buy_get_time),
    NewAlreadyBuy = Dm#dm_data.already_buy + 1,
    set_role_local_dm(RoleID,Dm#dm_data{remain_time_buy = NewRemainTime
                                       ,already_buy = NewAlreadyBuy}),
    NextBuyCost = role_doublematch:get_buy_cost(NewAlreadyBuy+1),
    ?unicast(RoleID,#sc_doublematch_times_buy{result=1,cur_times=NewRemainTime + Dm#dm_data.remain_time,next_buy_cost=NextBuyCost}),
    {noreply, State};
handle_info({get_old_rank,CurSession,OldRankList}, State) ->
    ?INFO("~w",[{get_old_rank,CurSession,OldRankList}]),
    {OldSession,OldRank} = OldRankList,
    ?INFO("get_old_rank Cur:~w ReLength:~w",[CurSession,erlang:length(OldRank)]),
    LocalCurSession = get(?pd_session),
    if
        LocalCurSession /= CurSession ->
            ?WARNING("get_old_rank waring ~w local:~w",[CurSession,LocalCurSession]);
        true ->
            ignore
    end,
    if
        CurSession /= LocalCurSession ->
            do_top_reward(CurSession,OldSession,OldRank),
            put(?pd_session,CurSession);
        true ->
            ?ERR("get_old_rank repeated"),
            ignore
    end,
    put(?pd_local_rank,[]),
    ets:delete_all_objects(?ETS_DM_RANK_DATA),
    send_to_rank({get_rank_req,erlang:self(),-1}),
    ets:delete_all_objects(?ETS_DM_OLD_RANK),
    ets:insert(?ETS_DM_OLD_RANK,OldRank),
    put(?pd_1w_rank, []),
    {noreply, State#state{rank_version=-1}};
handle_info({check_rank_reward,RoleID},State) ->
    % 此处只是为了当客户端登录时，及时检查是否需要发送奖励。仅当本赛季初次登录时，有意义
    get_role_local_dm(RoleID), 
    {noreply, State};
handle_info(persist_tick, State) ->
    persist_state(),
    persist_check(),
    set_persist_interval(),
    update_local_sessoin(State),
    {noreply, State};
handle_info({t_set_score,R,S},State) ->
    t_set_score(R,S),
    {noreply, State};
handle_info({inet_reply,_S,_Status},State) ->
    ignore,
    {noreply, State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    ignore,
    {noreply,State};
% 针对5月1日，奖励少发的问题，临时加的修正接口，在2.6.0版本时把这个删去吧
handle_info({fix_reward,DmRank},State) ->
    ?ERR("fix_reward---> ~w",[DmRank]),
    do_top_reward3(DmRank),
    {noreply,State};

% 针对2017年1月1日，奖励少发的问题
handle_info({fix_20170103_reward,IsDebug},State) ->
    Sql = io_lib:format("select roleID,before_score from gDoublematch where session = ~w and before_score < ~w and before_score > 0;",[201701,2916]),
    RankList2 = 
        case db_sql:get_rows(Sql) of
            RankList1 when erlang:is_list(RankList1) ->
                [{A,B}||[A,B]<-RankList1];
            _ ->
                []
        end,
    RankList3 = 
        [{E#dm_data.role_id,E#dm_data.before_score}||{_,_,E,_,_}<-ets:tab2list(?ETS_DM_INFO_LIST)],
    RankList4 = lists:foldl(fun({R,S},AccList)-> 
                                    case lists:keymember(R, 1, RankList2) of
                                        true ->
                                            AccList;
                                        false ->
                                            [{R,S}|AccList]
                                    end
                            end, RankList2, RankList3),
    RankList5 = lists:filter(fun({R,S})->
            case db_sql:get_rows(io_lib:format("select id from t_item_add_2017_1 where roleID = ~w and type = 27 and argID = 1124;",[R])) of
                [] when S > 0 ->
                    case db_sql:get_rows(io_lib:format("select mailUID from gmail where recvID = ~w and mailType != 5 and mailTemplateID = 1124;",[R])) of
                        [] ->
                            true;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end
        end, RankList4),
    lists:foreach(fun({R,S})-> 
                        NewRank = check_rank_by_sroce(S),
                        {_,GRank,GLevel,RankName,Reward} = data_doublematch:get({rank,NewRank}),
                        RankTop = data_doublematch:get(rank_top),
                        RankScore = if
                                        NewRank >= RankTop ->
                                            {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
                                            S - TopScore;
                                        true ->
                                            S rem 100
                                    end,
                        Str = RankName ++ "-" ++ erlang:integer_to_list(RankScore) ++ "积分",
                        case IsDebug of
                            true ->
                                ?ERR("fix_20170103_reward ~w +++++ ~w",[{R,NewRank},{Str,201612,S}]);
                            false ->
                                mail_server:send_sys_mail(R, ?MAIL_DOUBLEMATCH_RANK_REWARD, [Str,201612,S], "", Reward)
                        end
                  end, RankList5),
    {noreply,State};

handle_info(_Info, State) ->
    ?WARNING("Unknown info ~w",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) -> 
    persist_state(),
    persist_immediately(),
    ok.

%%%===================================================================
%%% Internal functions - do_handle_info
%%%===================================================================

send_to_match(Msg)->
    send_msg:direct_by_name(doublematch_match, doublematch_match, Msg).

send_to_rank(Msg)->
    send_msg:direct_by_name(doublematch_match, doublematch_rank, Msg).

% #p_doublematch_rank_unit.rank 相当于 #dm_rank.index
send_rank(RoleID,Type,RankList0,BeginIndex0, Length)->
    CurL = erlang:length(RankList0),
    BeginIndex = erlang:max(1, BeginIndex0),
    ?INFO("send_rank ~w ~w ~w",[CurL,BeginIndex, Length]),
    SendRankUnitList = [role_doublematch:transformDMRank2RankUnit(DMRankInfo)||DMRankInfo<-RankList0],
    SendRankUnitListSort = lists:sort(fun(A,B) ->
                                      if
%%                                           A#p_doublematch_rank_unit.rank < B#p_doublematch_rank_unit.rank ->
%%                                               true;
%%                                           A#p_doublematch_rank_unit.rank > B#p_doublematch_rank_unit.rank ->
%%                                               false;
                                          A#p_doublematch_rank_unit.grade  > B#p_doublematch_rank_unit.grade  ->
                                              true;
                                          A#p_doublematch_rank_unit.grade  < B#p_doublematch_rank_unit.grade  ->
                                              false;
                                          A#p_doublematch_rank_unit.gradeLevel  < B#p_doublematch_rank_unit.gradeLevel  ->
                                              true;
                                          A#p_doublematch_rank_unit.gradeLevel  > B#p_doublematch_rank_unit.gradeLevel  ->
                                              false;
                                          A#p_doublematch_rank_unit.score  > B#p_doublematch_rank_unit.score  ->
                                              true;
                                          A#p_doublematch_rank_unit.score  < B#p_doublematch_rank_unit.score  ->
                                              false;
                                          A#p_doublematch_rank_unit.roleID < B#p_doublematch_rank_unit.roleID ->
                                              true;
                                          A#p_doublematch_rank_unit.roleID > B#p_doublematch_rank_unit.roleID ->
                                              false
                                      end
                            end, SendRankUnitList),
    RankList = if
        CurL < BeginIndex ->
            [];
        BeginIndex + Length > CurL ->
            lists:sublist(SendRankUnitListSort, BeginIndex, CurL - BeginIndex + 1);
        true ->
            lists:sublist(SendRankUnitListSort, BeginIndex, Length)
               end,
    ?unicast(RoleID,#sc_doublematch_rankinfo{result=1,type=Type,position=BeginIndex,ranklist=RankList,length=erlang:length(RankList)}).

send_rec_list(RoleID,RankList0)->
    RecordLength = data_doublematch:get(data_record_length),
    RankListSort = lists:sort(fun(A,B) ->
                                      if
                                          %%A#dm_rank.rank > B#dm_rank.rank ->
                                          %%    true;
                                          %%A#dm_rank.rank < B#dm_rank.rank ->
                                          %%    false;
                                          A#dm_rank.score  > B#dm_rank.score  ->
                                              true;
                                          A#dm_rank.score  < B#dm_rank.score  ->
                                              false;
                                          A#dm_rank.role_id < B#dm_rank.role_id ->
                                              true;
                                          A#dm_rank.role_id > B#dm_rank.role_id ->
                                              false
                                      end
                              end, RankList0),
    ListLength = erlang:length(RankListSort),
    if
        ListLength > RecordLength ->
            {RankList,_} = lists:split(RecordLength, RankListSort);
        true ->
            RankList = RankListSort
    end,
    ?INFO("send_rec_list ~w ~w",[RoleID,erlang:length(RankList)]),
    RecordList = send_rec_list2([],RankList),
    ?unicast(RoleID,#sc_doublematch_record_list{result=1,recordlist=RecordList,beginindex=0,type=?RANK_REC_LIST,length=erlang:length(RecordList)}).

send_rec_list2(RecList,[])->
    lists:reverse(RecList);
send_rec_list2(RecList,[Rank|OtherRankList])->
    case lists:keymember(Rank#dm_rank.rec_id, #p_record_unit.recordID, RecList) of
        true ->
            send_rec_list2(RecList,OtherRankList);
        false ->
            P = #p_record_unit{recordID=Rank#dm_rank.rec_id
                          ,recordteammember=Rank#dm_rank.punit     %#p_doublematch_rank_unit{}
                          ,timestamp=Rank#dm_rank.ts},
            send_rec_list2([P|RecList],OtherRankList)
    end.

%保留扩展
set_single_state(RoleID,?match_state_sign,SignID,RoleIDList,TimerRef0)->
    [LeaderID|_] = RoleIDList,
    TimerRef = if
                   LeaderID =:= RoleID ->
                       TimerRef0;
                   true ->
                       ?undefined
               end,    
    ets:insert(?ETS_DM_SIGN_LIST, {RoleID,SignID,RoleIDList,TimerRef}).        

check_team_sign(RoleIDList) ->
    check_team_sign2(true,RoleIDList).
check_team_sign2({false,Reason},_) ->
    {false,Reason};
check_team_sign2(true,[]) ->
    true;
check_team_sign2(true,[RoleID|OtherRoleIDList]) ->
    check_team_sign2(check_single_sign(RoleID),OtherRoleIDList).

check_single_sign(RoleID)->
    %检查次数还够不
    DmInfo = get_role_local_dm(RoleID),
    NowSession = get_session(),
    OpenLevel =  data_doublematch:get(data_open_level),
    #rolePublic{level = RoleLevel} = role_lib:get_rolePublic(DmInfo#dm_data.role_id),
    CurTime = get_time(DmInfo),
    if
        RoleLevel < OpenLevel ->
            {false,7};
        DmInfo#dm_data.session /= NowSession ->
            ?INFO("sesion error 赛季错误 ~w ~w",[DmInfo#dm_data.session, NowSession]),
            {false,2}; % 这里其实是赛季切换，导致的错误
        CurTime > 0 ->
            case ets:lookup(?ETS_DM_SIGN_LIST, RoleID) of
                [] ->
                    true;
                _ ->
                    {false,2}
            end;
        true ->
            {false,4}
    end.

%如果是未打过的玩家，作为青铜判断
check_rank_for_match(RankA0,RankB0)->
    RankA = erlang:max(1, RankA0),
    RankB = erlang:max(1, RankB0),
    {_,AGRank,_,_,_} = data_doublematch:get({rank,RankA}),
    {_,BGRank,_,_,_} = data_doublematch:get({rank,RankB}),
    LF =erlang:abs(AGRank - BGRank),
    if
        LF > 1 ->
            false;
        true ->
            true
    end.

get_time(RoleDmInfo)->
    RoleDmInfo#dm_data.remain_time + RoleDmInfo#dm_data.remain_time_buy.

check_time(RoleDmInfo)->
    if
        RoleDmInfo#dm_data.remain_time > 0 ->
            {true,RoleDmInfo#dm_data{remain_time = RoleDmInfo#dm_data.remain_time - 1}};
        RoleDmInfo#dm_data.remain_time_buy > 0 ->
            {true,RoleDmInfo#dm_data{remain_time_buy = RoleDmInfo#dm_data.remain_time_buy - 1}};
        true ->
            false
    end.
    

dirty_get_role_local_dm(RoleID) ->
	case ets:lookup(?ETS_DM_INFO_LIST, RoleID) of
		[] -> #dm_db_record{};
		[DmDbRecord] when is_record(DmDbRecord, dm_db_record) ->
			DmDbRecord
	end.

dirty_get_role_dm_grade(RoleID) ->
    Rank = case ets:lookup(?ETS_DM_INFO_LIST, RoleID) of [] -> read_dm_rank(RoleID);
             [DmDbRecord] when is_record(DmDbRecord, dm_db_record) -> 
                     #dm_data{rank=Rank1,index=_RankIndex}=DmDbRecord#dm_db_record.dm_data,Rank1
           end,
    {_,GRank,_GLevel,_,_} = data_doublematch:get({rank,Rank}),
    GRank.

read_dm_rank(RoleID) ->
    RoleDmInfo3 = db_sql:get_doublematch_info(RoleID),
    RankTop = data_doublematch:get(rank_top),
    case ets:lookup(?ETS_DM_RANK_DATA, RoleID) of
        [] when RoleDmInfo3#dm_data.rank =:= 0 -> 0;
        [] -> check_rank_by_sroce(RoleDmInfo3#dm_data.score);
        [DataInRank] ->
            case check_rank_by_sroce(RoleDmInfo3#dm_data.score) of
                NewRank0 when NewRank0 =:= RankTop ->DataInRank#dm_rank.rank;
                NewRank0 when NewRank0 < RankTop ->NewRank0
            end
    end.

% 会改写?ETS_DM_INFO_LIST数据，禁止在doublematch_server进程外调用，要确保，doublematch_server一个进程写，多个进程可读
get_role_local_dm(RoleID)->
    Now = util:now(),
    Date = erlang:date(),
    DmDbRecord = case ets:lookup(?ETS_DM_INFO_LIST, RoleID) of
                     [] ->
                         DmData = db_sql:get_doublematch_info(RoleID),
                         FightRec = DmData#dm_data.fight_rec,
                         FightRecT = if is_list(FightRec) -> FightRec; true -> [] end,
                         FightRec2 = [{FightRec1#sc_doublematch_fight{rolelist=doublematch_rank:update_p_unit([],FightRec1#sc_doublematch_fight.rolelist)},TTss}
                                     ||{FightRec1,TTss}<-FightRecT],
                         %FightRec2 = FightRec#sc_doublematch_fight{rolelist=InfoList},
                         #dm_db_record{role_id=RoleID
                                       ,dm_data=DmData#dm_data{fight_rec=FightRec2}
                                       ,write_ts=0
                                       ,read_ts=Now};
        [DmDbRecord0] when erlang:is_record(DmDbRecord0, dm_db_record) ->
            DmDbRecord0#dm_db_record{read_ts = Now} % write_ts maybe 0 or more than 0
    end,
    % 检查购买次数是否更新
    RoleDmInfo1 = DmDbRecord#dm_db_record.dm_data,
    RoleDmInfo2 = if
            RoleDmInfo1#dm_data.rf_date /= Date ->
                RoleDmInfo1#dm_data{already_buy = 0
                                   ,remain_time = erlang:max(RoleDmInfo1#dm_data.remain_time, data_doublematch:get(daily_time))
                                   ,rf_date = Date};
            true ->
                RoleDmInfo1
        end,
    % 检查赛季是否更替
    RoleDmInfo3 = check_rank_reward(RoleDmInfo2),
    % 从百强列表中取该玩家最新排名，?ETS_DM_RANK_DATA按照固定排名顺序存放
    RankTop = data_doublematch:get(rank_top),
    {Index,NewRank} = case ets:lookup(?ETS_DM_RANK_DATA, RoleID) of
                [] when RoleDmInfo3#dm_data.rank =:= 0 ->
                    {0,0};
                [] ->
                    Index0 = case lists:keysearch(RoleID, 1, get(?pd_1w_rank)) of
                                 {value,{RoleID,Index00}}->
                                     Index00;
                                 false ->
                                     0
                             end,
                    {Index0,check_rank_by_sroce(RoleDmInfo3#dm_data.score)};
                [DataInRank] ->
                    case check_rank_by_sroce(RoleDmInfo3#dm_data.score) of
                        NewRank0 when NewRank0 =:= RankTop ->
                            {DataInRank#dm_rank.index,DataInRank#dm_rank.rank};
                        NewRank0 when NewRank0 < RankTop ->
                            {DataInRank#dm_rank.index,NewRank0}
                    end
            end,
    RoleDmInfo4 = RoleDmInfo3#dm_data{index=Index,rank=NewRank},
    NewDmDbRecord = if
        %数据有变化，需要设定写入时间戳
        RoleDmInfo1 /= RoleDmInfo4 ->
            DmDbRecord#dm_db_record{dm_data = RoleDmInfo4
                                   ,write_ts = Now};
        %数据无变化，只是设定读入时间戳
        true ->
            DmDbRecord
    end,
    ets:insert(?ETS_DM_INFO_LIST, NewDmDbRecord),
    RoleDmInfo4.

%仅本进程内可调用，且RoleDmInfo必须来自于get_role_local_dm
set_role_local_dm(RoleID,RoleDmInfo)->
    Now = util:now(),
    NewDmDbRecord = #dm_db_record{role_id = RoleID
                                 ,dm_data = RoleDmInfo
                                 ,write_ts = Now
                                 ,read_ts = Now},
    ets:insert(?ETS_DM_INFO_LIST, NewDmDbRecord),
    %%异步调用下双排排行榜变化
    role_payGuide:asyn_trigger_task_change(RoleID,?DOUBLEMATCH_RANK_N,{0}).

check_rank_reward(RoleDmInfo)->
    CurSession = get(?pd_session),
    ?INFO("check_rank_reward do reward ~w ~w",[CurSession,RoleDmInfo#dm_data.session]),
    if
        CurSession /= RoleDmInfo#dm_data.session andalso CurSession /= ?undefined->
            LastSession = get_last_session(),
            if
                LastSession =:= RoleDmInfo#dm_data.session andalso RoleDmInfo#dm_data.rank /= 0 ->
                    do_reward(RoleDmInfo);
                true ->
                    ignore
            end,
            RoleDmInfo#dm_data{index = 0
                              ,session = CurSession
                              ,rank = 0
                              ,remain_time = data_doublematch:get(daily_time)
                              ,remain_time_buy = 0
                              ,score  = data_doublematch:get(init_score)
                              ,fight_rec = ?undefined
                              ,before_score = RoleDmInfo#dm_data.score};
        true ->
            RoleDmInfo
    end.
    
%%  <content tid = "1124" title = "{prop ffffff00-26}排位赛奖励" content = "{prop ffffffff-24}训练师，您在上个赛季的排位赛最终段位是：{prop ff00ff00-26}@(0)@{prop ffffffff-24}，以下是该段位奖励，请查收！"/>
do_reward(RoleDmInfo) when RoleDmInfo#dm_data.rank /= 0 ->
    {_,GRank,GLevel,RankName,Reward} = data_doublematch:get({rank,RoleDmInfo#dm_data.rank}),
    RankTop = data_doublematch:get(rank_top),
    RankScore = if
                    RoleDmInfo#dm_data.rank >= RankTop ->
                        {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
                        RoleDmInfo#dm_data.score - TopScore;
                    true ->
                        RoleDmInfo#dm_data.score rem 100
                end,
    Str = RankName ++ "-" ++ erlang:integer_to_list(RankScore) ++ "积分",
    mail_server:send_sys_mail(RoleDmInfo#dm_data.role_id, ?MAIL_DOUBLEMATCH_RANK_REWARD, [Str,RoleDmInfo#dm_data.session ,RoleDmInfo#dm_data.score], "", Reward).

%保持在本进程内，把王者的奖励发完
do_top_reward(CurSession,RecOldSession,OldRank) ->
    ServerId = data_setting:get(server_id),
    LocalRankList = lists:filter(fun(R) -> R#dm_data.server_id =:= ServerId end, OldRank),
    ?INFO("do_top_reward start all:~w~nreward:~w",[OldRank,LocalRankList]),
    lists:foreach(fun(R) ->
                DmData = get_role_local_dm(R#dm_rank.role_id),
                if
                    DmData#dm_data.rank /= 0 ->
                        if
                            RecOldSession =:= DmData#dm_data.session orelse RecOldSession =:= 0  ->    
                                NewDmData = DmData#dm_data{index = 0
                                                          ,session = CurSession
                                                          ,rank = 0
                                                          ,remain_time = data_doublematch:get(daily_time)
                                                          ,remain_time_buy = 0
                                                          ,score  = data_doublematch:get(init_score)
                                                          ,fight_rec = ?undefined
                                                          ,before_score = DmData#dm_data.score},
                                set_role_local_dm(R#dm_rank.role_id,NewDmData),
                                do_top_reward2(R,DmData);
                            true ->
                                % 没有bug的话，不该走到这个分支
                                ?ERR("do_top_reward fail 1 ~w ~w",[DmData, R]),
                                ignore
                        end;
                    true ->
                        % 没有bug的话，不该走到这个分支
                        ?ERR("do_top_reward 2 ~w",[DmData]),
                        ignore
                end
        end, LocalRankList).

do_top_reward2(DmRank,DmData) ->    
    Wangzhe = data_doublematch:get(rank_top) + 1,
    {_,_,_,RankName,Reward} = data_doublematch:get({rank,DmRank#dm_rank.rank}),
    if
        Wangzhe =:= DmRank#dm_rank.rank ->
            Str = RankName ++ "-第" ++ erlang:integer_to_list(DmRank#dm_rank.index) ++ "位",
            mail_server:send_sys_mail(DmRank#dm_rank.role_id, ?MAIL_DOUBLEMATCH_RANK_REWARD, [Str,DmData#dm_data.session ,DmRank#dm_rank.score], "", Reward);
        true ->
            RankTop = data_doublematch:get(rank_top),
            RankScore = if
                            DmData#dm_data.rank >= RankTop ->
                                {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
                                DmRank#dm_rank.score - TopScore;
                            true ->
                                DmRank#dm_rank.score rem 100
                        end,
            if
                DmRank#dm_rank.score =:= DmData#dm_data.score ->
                    Str = RankName ++ "-" ++ erlang:integer_to_list(RankScore) ++ "积分",
                    mail_server:send_sys_mail(DmRank#dm_rank.role_id, ?MAIL_DOUBLEMATCH_RANK_REWARD, [Str,DmRank#dm_data.session ,DmRank#dm_rank.score], "", Reward);
                true ->
                    % 没有bug的话，不该走到这个分支，DmRank,DmData的score应该是同步的
                    {_,_,_,RankName2,Reward2} = data_doublematch:get({rank,DmData#dm_data.rank}),
                    ?ERR("do_top_reward2 r:~w d:~w reward:~w",[{DmRank#dm_rank.role_id,DmRank#dm_rank.score,DmRank#dm_rank.rank}
                                                              ,{DmData#dm_data.role_id,DmData#dm_data.score,DmData#dm_data.rank}
                                                              ,Reward2]),
                    Str = RankName2 ++ "-" ++ erlang:integer_to_list(RankScore) ++ "积分",
                    mail_server:send_sys_mail(DmData#dm_data.role_id, ?MAIL_DOUBLEMATCH_RANK_REWARD, [Str,DmData#dm_data.session ,DmData#dm_data.score], "", Reward2)
            end
    end.

do_top_reward3(DmRank)->
    {_,_,_,RankName,Reward} = data_doublematch:get({rank,DmRank#dm_rank.rank}),
    Wangzhe = data_doublematch:get(rank_top) + 1,
    if
        Wangzhe =:= DmRank#dm_rank.rank ->
            ignore;
%%             Str = RankName ++ "-第" ++ erlang:integer_to_list(DmRank#dm_rank.index) ++ "位",
%%             mail_server:send_sys_mail(DmRank#dm_rank.role_id, ?MAIL_DOUBLEMATCH_RANK_REWARD, [Str,DmData#dm_data.session ,DmRank#dm_rank.score], "", Reward);
        true ->
            RankTop = data_doublematch:get(rank_top),
            RankScore = if
                            DmRank#dm_rank.rank >= RankTop ->
                                {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
                                DmRank#dm_rank.score - TopScore;
                            true ->
                                DmRank#dm_rank.score rem 100
                        end,
            ?ERR("do_top_reward3 ~w r:~w",[{DmRank#dm_rank.role_id,DmRank#dm_rank.score,DmRank#dm_rank.rank},Reward]),
            Str = RankName ++ "-" ++ erlang:integer_to_list(RankScore) ++ "积分",
            mail_server:send_sys_mail(DmRank#dm_rank.role_id, ?MAIL_DOUBLEMATCH_RANK_REWARD, [Str,DmRank#dm_data.session ,DmRank#dm_rank.score], "", Reward)
    end.

check_rank_by_sroce(NewSroce)->
    check_rank_by_sroce2(NewSroce,lists:reverse(lists:seq(1, data_doublematch:get(rank_top)))).
    
check_rank_by_sroce2(_NewSroce,[])->
    1;
check_rank_by_sroce2(NewSroce,[RankIndex|OtherRankList])->
    {RankScore,_,_,_,_} = data_doublematch:get({rank,RankIndex}),
    if
        RankScore =< NewSroce ->
            RankIndex;
        true ->
            check_rank_by_sroce2(NewSroce,OtherRankList)
    end.

refresh_local_rank(NewDmRank)->
    OldRank0 = get(?pd_local_rank),
    OldRank = lists:filter(fun(R)-> R#dm_rank.role_id /= NewDmRank#dm_rank.role_id end, OldRank0),
    NewRank = refresh_local_rank2(NewDmRank,[],1,OldRank),
    put(?pd_local_rank,NewRank).

% 只有开服或新一届有这种情况
refresh_local_rank2(NewDmRank,[],Index,[])->
    [NewDmRank#dm_rank{index = Index}];
% 排序未完，新元素已经插入
refresh_local_rank2(?undefined,AccRank,Index,[Front|OtherOldRankList])->
    refresh_local_rank2(?undefined,[Front#dm_rank{index=Index}|AccRank],Index+1,OtherOldRankList);
%排序已完，新元素已经插入
refresh_local_rank2(?undefined,AccRank,_Index,[])->
    CurLength = erlang:length(AccRank),
    MaxLocalLength = data_doublematch:get(data_local_rank_length),
    {_ScoreMin,NewRankList} = if 
                    CurLength < MaxLocalLength ->
                        {0,AccRank};
                    CurLength =:= (MaxLocalLength+1) ->
                        [_|NewRankList0] = AccRank,
                        [Min|_] = NewRankList0,
                        {Min#dm_rank.score,NewRankList0};
                    true ->
                        [Min|_] = AccRank,
                        {Min#dm_rank.score,AccRank}
               end,
    lists:reverse(NewRankList);
%排序已完，新元素未插入,有可能超长，有可能没到最大，也有可能刚到最大
refresh_local_rank2(NewDmRank,AccRank,Index,[]) when NewDmRank /= ?undefined->
    refresh_local_rank2(?undefined,[NewDmRank#dm_rank{index=Index}|AccRank],Index+1,[]);
%排序未完，新元素未插入,有可能超长，有可能没到最大，也有可能刚到最大
refresh_local_rank2(NewDmRank,AccRank,Index,[Front|OtherOldRankList]) when NewDmRank /= ?undefined->
    if
        NewDmRank#dm_rank.score > Front#dm_rank.score 
          orelse (NewDmRank#dm_rank.score =:= Front#dm_rank.score 
                 andalso NewDmRank#dm_rank.role_id < Front#dm_rank.role_id)-> %分数相等的话，晚达到的靠后
            refresh_local_rank2(?undefined,[NewDmRank#dm_rank{index = Index}|AccRank],Index+1,[Front|OtherOldRankList]);
        true ->
            refresh_local_rank2(NewDmRank,[Front#dm_rank{index=Index}|AccRank],Index+1,OtherOldRankList)
    end.

% 根据当前数据，刷新本地排名中的王者状态。
local_rank_set_rank(LocalRankList,WangzheList,GlobalRankList1W)->
    RankTop = data_doublematch:get(rank_top),
    local_rank_set_rank2([],LocalRankList,WangzheList,GlobalRankList1W,RankTop).

local_rank_set_rank2(AccList,[],_GlobalRankList1K,_GlobalRankList1W,_RankTop)->
    lists:reverse(AccList);
local_rank_set_rank2(AccList,[Rank|OtherList],WangzheList,GlobalRankList1W,RankTop) when Rank#dm_rank.rank < RankTop ->
    local_rank_set_rank2([Rank|AccList],OtherList,WangzheList,GlobalRankList1W,RankTop);
local_rank_set_rank2(AccList,[Rank|OtherList],WangzheList,GlobalRankList1W,RankTop)->
    case lists:member(Rank#dm_rank.role_id, WangzheList) of
        true ->
            % 逻辑走到这里说明 rank = RankTop
            local_rank_set_rank2([Rank#dm_rank{rank=RankTop + 1}|AccList],OtherList,WangzheList,GlobalRankList1W,RankTop);
        false ->
            local_rank_set_rank2([Rank|AccList],OtherList,WangzheList,GlobalRankList1W,RankTop)
    end.
  
refresh_friend_rank(RankList)->
    refresh_friend_rank([],RankList,1).

refresh_friend_rank(AccList,[],_)->
    lists:reverse(AccList);
refresh_friend_rank(AccList,[H|T],Index)->
    refresh_friend_rank([H#p_doublematch_rank_unit{rank=Index}|AccList],T,Index+1).
      
persist_state()->
    db_sql:set_etc(?DB_ETC_KEY_DOUBLEMATCH_SERVER, {get(?pd_session)}).
                      
persist_check()->
    Now = util:now(),
    WriteLimit = Now - ?write_timeout,
    %检查DmInfo写入
    DmInfoResW = 
        ets:select(?ETS_DM_INFO_LIST, [{{'_','$1','$2','$3','_'},
                                      [{'andalso',{'>','$3',0},{'<','$3',WriteLimit}}],
                                      [{{'$1','$2'}}]}]
                                ,?once_max),
    case DmInfoResW of
        {DmInfoNeedWriteList,_} ->
            ?INFO("DmInfoNeedWriteList is ~w",[erlang:length(DmInfoNeedWriteList)]),
            lists:foreach(fun({RoleID,DmInfoInfo})-> 
                                  db_sql:set_doublematch_info(RoleID,DmInfoInfo),
                                  ets:update_element(?ETS_DM_INFO_LIST, RoleID,{#dm_db_record.write_ts,0})
                          end, DmInfoNeedWriteList);
        _ ->
            ignore
    end,
    %检查DmInfo数据是否需要从缓存中删除
    ReadLimit = Now - ?read_timeout,
    DmInfoResR = 
        ets:select(?ETS_DM_INFO_LIST, [{{'_','$1','_','$2','$3'},
                                      [{'andalso',{'=:=','$2',0},{'<','$3',ReadLimit}}],
                                      [{{'$1'}}]}]
                                ,?once_max),
    case DmInfoResR of
        {NeedDelList1,_} ->
            ?INFO("NeedDelList1 is ~w",[erlang:length(NeedDelList1)]),
            lists:foreach(fun({RoleID})-> 
                                  ets:delete(?ETS_DM_INFO_LIST, RoleID)
                          end, NeedDelList1);
        _ ->
            ignore
    end.

%立即把所有数据写入数据库
persist_immediately()->
    DmInfoResW = ets:select(?ETS_DM_INFO_LIST
                              ,[{{'_','$1','$2','$3','_'},[{'>','$3',0}],[{{'$1','$2'}}]}]
                              ,?once_max),
    case DmInfoResW of
        {DmInfoNeedWriteList,_} ->
            lists:foreach(fun({RoleID,DmInfoInfo})-> 
                                  db_sql:set_doublematch_info(RoleID,DmInfoInfo),
                                  ets:update_element(?ETS_DM_INFO_LIST, RoleID,{#dm_db_record.write_ts,0})
                          end, DmInfoNeedWriteList);
        _ ->
            ignore
    end.

%设定下次timer
set_persist_interval()->
    erlang:send_after(?persist_interval, self(), persist_tick).

update_local_sessoin(_State)->
    CurSession = get(?pd_session),
    NowSession = get_session(),
    if
        CurSession /= NowSession ->
            ?INFO("get_old_rank update_local_sessoin ~w ~w",[CurSession,NowSession]),
            send_to_rank({get_old_rank,data_setting:get(server_id),CurSession,self()}); % 发送更新赛季的请求到跨服服务器
        true ->
            ignore
    end.
  
get_session()->
    {Y0,M0,D} = erlang:date(),
    SessionDay = data_doublematch:get(init_day),
    {Y,M} = if
        D < SessionDay ->
            if
                M0 =:= 1 ->
                    {Y0-1,12};
                true ->
                    {Y0,M0-1}
            end;
        true ->
            {Y0,M0}
    end,    
    Y*100 + M. 

get_last_session()->
    CurSession = get_session(),
    M = CurSession rem 100,
    if
        M > 1 ->
            CurSession -1;
        M =:= 1 ->
            CurSession - 100 + 11
    end.

get_season_endtime()->
    Day = data_doublematch:get(init_day),
    {Year,Month,NowDay} = erlang:date(),
    if
        NowDay >= Day andalso Month < 12 ->
            util:datetime_to_seconds({{Year,Month+1,Day},{0,0,0}});
        NowDay >= Day andalso Month =:= 12 ->
            util:datetime_to_seconds({{Year+1,1,Day},{0,0,0}});
        true ->
            util:datetime_to_seconds({{Year,Month,Day},{0,0,0}})
    end.    

%% ------------------------------------------------------------------
%% test Function Definitions
%% ------------------------------------------------------------------

test_show_role_dm(RoleID)->
    ets:lookup(?ETS_DM_INFO_LIST, RoleID).

test_show_rank_cache()->
    ets:tab2list(?ETS_DM_RANK_DATA).

test_doublematch_sign()->
    RoleID = 6511905 + util:random_int(1, 99999),
    Fighters =  [{ger,2000000004166,{gerBase,7220,20,100,4,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004109,{gerBase,7010,20,100,1,0,[]},{gerAttr,2403837,2500,10000,2000,203998,1882210,100,100,10,15,15,10,0,0,15,15,36,0,1,0,80852,2403837},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1882210,100,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004121,{gerBase,7050,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004127,{gerBase,7070,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
    Talent = [{12000,40},{13000,40},{22000,30},{23000,30},{21000,15},{24000,15},{32000,10},{31000,10},{33000,10},{34000,10},{41000,20},{43000,25},{52000,40},{53000,40},{51000,25},{61000,20},{62000,15},{54000,35},{64000,20},{71000,32},{72000,15},{11000,1},{73000,15},{74000,31},{93000,51},{92000,50},{81000,5},{94000,7},{91000,9},{63000,20},{42000,14}],
    Spe = {trSpecial,10001,1001,251,0,0},
    Skin = {skin_info,[],0},
    Lieu = {10,10},    
    T1 = #double_team{skin_info=Skin,talent=Talent,trainer=Spe,fighters=Fighters,lieu=Lieu},
    doublematch_sign(RoleID, -1, [{RoleID,T1}]).
    
test_doublematch_sign_score(S)->
    RoleID = 6511905 + util:random_int(1, 99999),
    Fighters =  [{ger,2000000004166,{gerBase,7220,20,100,4,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004109,{gerBase,7010,20,100,1,0,[]},{gerAttr,2403837,2500,10000,2000,203998,1882210,100,100,10,15,15,10,0,0,15,15,36,0,1,0,80852,2403837},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1882210,100,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004121,{gerBase,7050,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004127,{gerBase,7070,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
    Talent = [{12000,40},{13000,40},{22000,30},{23000,30},{21000,15},{24000,15},{32000,10},{31000,10},{33000,10},{34000,10},{41000,20},{43000,25},{52000,40},{53000,40},{51000,25},{61000,20},{62000,15},{54000,35},{64000,20},{71000,32},{72000,15},{11000,1},{73000,15},{74000,31},{93000,51},{92000,50},{81000,5},{94000,7},{91000,9},{63000,20},{42000,14}],
    Spe = {trSpecial,10001,1001,251,0,0},
    Skin = {skin_info,[],0},
    Lieu = {10,10},    
    T1 = #double_team{skin_info=Skin,talent=Talent,trainer=Spe,fighters=Fighters,lieu=Lieu},
    doublematch_sign(RoleID, -1, [{RoleID,T1}]).

test_robot_sign(RoleID)->
    Fighters =  [{ger,2000000004166,{gerBase,7220,20,100,4,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004109,{gerBase,7010,20,100,1,0,[]},{gerAttr,2403837,2500,10000,2000,203998,1882210,100,100,10,15,15,10,0,0,15,15,36,0,1,0,80852,2403837},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1882210,100,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004121,{gerBase,7050,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004127,{gerBase,7070,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
    Talent = [{12000,40},{13000,40},{22000,30},{23000,30},{21000,15},{24000,15},{32000,10},{31000,10},{33000,10},{34000,10},{41000,20},{43000,25},{52000,40},{53000,40},{51000,25},{61000,20},{62000,15},{54000,35},{64000,20},{71000,32},{72000,15},{11000,1},{73000,15},{74000,31},{93000,51},{92000,50},{81000,5},{94000,7},{91000,9},{63000,20},{42000,14}],
    Spe = {trSpecial,10001,1001,251,0,0},
    Skin = {skin_info,[],0},
    Lieu = {10,10},    
    T1 = #double_team{skin_info=Skin,talent=Talent,trainer=Spe,fighters=Fighters,lieu=Lieu},
    role_lib:send_server(RoleID, {test_dm_sign,[{RoleID,T1}]}).

tss(RoleID,S)->
    erlang:send(?MODULE, {t_set_score,RoleID,S}).

t_set_score(RoleID,S)->
    Dm = get_role_local_dm(RoleID),
    NewRank = check_rank_by_sroce(S), % 计算段位
    set_role_local_dm(RoleID,Dm#dm_data{score=S
                                       ,rank = NewRank
                                       ,remain_time=data_doublematch:get(daily_time)}).
    
t_sample_1000rank()->
    One = {p_doublematch_rank_unit,6010001,"robot82",true,9,0,60,5,4,4,4,2,4294967282},
    [One#p_doublematch_rank_unit{rank=E}||E<-lists:seq(1, 1000)].
