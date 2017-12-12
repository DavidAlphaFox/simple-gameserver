-module(dojangrank_server).
-behaviour(gen_server).
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/0]).
  
-define(master_server,dojangrank_world_master). 

-define(pd_world_history_rank,pd_world_history_rank).                       %% 不保存数据库
-define(pd_world_history_rank_req_list,pd_world_history_rank_req_list).     %% 不保存数据库
-define(pd_world_top_fightrec_local,pd_world_top_fightrec_local).           %% 不保存数据库
-define(pd_world_top_fightrec_remote,pd_world_top_fightrec_remote).         %% 不保存数据库
-define(pd_world_top_fightrec_req_list,pd_world_top_fightrec_req_list).     %% 不保存数据库,请求排名中的roleid

-define(pd_world_init_flag,pd_world_init_flag).                             %% 
-define(pd_refresh_world_rank_flag,pd_refresh_world_rank_flag).

-define(pd_last_req_world_timestamp,pd_last_req_world_timestamp).       %% 记录上一次请求排行榜的时间
-define(pd_req_world_role_list,pd_req_world_role_list).                 %% 请求排名中的roleid

-define(update_server_list_interval,28).        %% 激活列表的时间淘汰间隔,一定要和dojangrank_world_server中的同名宏保持一致
-define(persist_interval,2400).                   %% TODO TEMP  default 2400
-record(state,{reward_date = {0,0,0}
              ,current_world_session = 0
              ,world_timestamp_version = {0,0}}).   %% 慢更新机制用到

-define(show_rank_all_num,15).
-define(show_rank_rule_num,5).

%% 为了防止单次消息过长，战斗过程单独消息发送

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
    {ok,_}=
        supervisor:start_child(world_sup,
                               {?MODULE,
                                {?MODULE,start_link,[]},
                                permanent,600000,worker,[?MODULE]}).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

set_selected_gerType(RoleID,RankType,SelectedGerType)->
    erlang:send(?MODULE,{set_selected_gerType,RoleID,RankType,SelectedGerType}).

dojangrank_fight(RoleID,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,FightPower,{FighterList,RoleLieuAdd,Talent,TrSpecial})->
    erlang:send(?MODULE,{dojangrank_fight,RoleID,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,FightPower,{FighterList,RoleLieuAdd,Talent,TrSpecial}}).

get_dojangrank_world_top_replay_list(RoleID,RankType)->
    erlang:send(?MODULE,{get_dojangrank_world_top_replay_list,RoleID,RankType}).

get_dojangrank_world_top_replay_detail(RoleID,ReplayUID)->
    erlang:send(?MODULE,{get_dojangrank_world_top_replay_detail,RoleID,ReplayUID}).

get_history_other_detail(RoleID,RankType,TarRoleID)->
    erlang:send(?MODULE,{get_history_other_detail,RoleID,RankType,TarRoleID}).

test_check_status()->
    erlang:send_after(1000, ?MODULE, test_status),
    erlang:send_after(5000, ?MODULE, test_status_timeout),
    whereis(?MODULE).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
    process_flag(trap_exit,true),
    State = init_process_data(),
    erlang:send(?MODULE,persist_tick),
    DateTime = {date(),time()},
    StartTime = data_dojangrank:get(start_date),
    if
        0 /= State#state.current_world_session andalso DateTime >= StartTime ->
            erlang:send_after(3000,?MODULE,refresh_world_rank_start);
        true -> ignore
    end,     
    {ok,State}.

terminate(Reason,State) ->
    ?ERR("~w terminate,reason:~w",[?MODULE,Reason]),
    do_persist(State),
    ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.

handle_call({debug_get_pd,PdName},_From,State) ->
    ?ERR("debug_get_pd ~w:~w",[PdName,get(PdName)]),
    {reply,get(PdName),State};
handle_call({debug_put_pd,PdName,Value},_From,State) ->
    OldValue = get(PdName),
    ?ERR("debug_get_pd ~w:~w",[PdName,put(PdName,Value)]),
    {reply,{OldValue,get(PdName)},State};
handle_call(Request,_From,State) ->
    ?ERR("未知的Request:~w,From:~w",[Request,_From]),
    {reply,ok,State}.

handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info({dojangrank_fight,RoleID,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,FightPower,{FighterList,RoleLieuAdd,Talent,TrSpecial}},State) ->
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    AtkRankInfo = case ets:match_object(EtsName, #dojangrank{role_id=RoleID, _='_'}) of
                      [] ->
                          ?undefined;
                      [Info1] ->
                          Info1
                  end,
    DefRankInfo = case ets:match_object(EtsName, #dojangrank{role_id=EnemyRoleId, _='_'}) of
                      [] ->
                          ?undefined;
                      [Info2] ->
                          Info2
                  end,
    if
        DefRankInfo =:= ?undefined orelse EnemyRank /= DefRankInfo#dojangrank.rank ->
            ?INFO("dojangrank_fight ~w EnemyRank:~w",[DefRankInfo,EnemyRank]),
            ReplayInfo = ?undefined,
            Reward = ?undefined,
            ResultMsg = 
                #sc_dojangrank_fight{result = 3
                                    ,fightInfo = []
                                    ,p_dojangrank_rank_self = to_p_dojangrank_rank(RoleID)
                                    ,p_dojangrank_rank_list = get_ranklist_sub(RankType,RoleID)
                                    ,reward = #p_reward_info{coin       = 0
                                                            ,roleExp     = 0
                                                            ,gerExp      = 0
                                                            ,gold        = 0
                                                            ,reputation  = 0
                                                            ,itemList    = []
                                                            ,gerList     = []}};
        true ->
            {FightRecord,AtkFighterList} = 
                do_dojangrank_fight(RoleID,{FighterList,RoleLieuAdd,Talent,TrSpecial},EnemyRoleId,DefRankInfo#dojangrank.fighter_array_cfg),
            case FightRecord#sc_fight_request.result of
                true when AtkRankInfo =:= ?undefined orelse (AtkRankInfo#dojangrank.rank > DefRankInfo#dojangrank.rank)->
                    AttackerNewRank = DefRankInfo#dojangrank.rank,
                    DefenderNewRank = if AtkRankInfo =:= ?undefined ->1001;true -> AtkRankInfo#dojangrank.rank end,
                    IsWinNum = 1;
                true when AtkRankInfo#dojangrank.rank =< DefRankInfo#dojangrank.rank->
                    AttackerNewRank = if AtkRankInfo =:= ?undefined ->1001;true -> AtkRankInfo#dojangrank.rank end,
                    DefenderNewRank = DefRankInfo#dojangrank.rank,
                    IsWinNum = 2;
                _ -> 
                    AttackerNewRank = if AtkRankInfo =:= ?undefined ->1001;true -> AtkRankInfo#dojangrank.rank end,
                    DefenderNewRank = DefRankInfo#dojangrank.rank,
                    IsWinNum = 0
            end,
            EnemyPub = role_lib:get_rolePublic(EnemyRoleId),
            case db_sql:insert_new_dojangrank_fightrec(RoleID,EnemyRoleId,AttackerNewRank,DefenderNewRank
                                                      ,IsWinNum
                                                      ,FightRecord,RoleName,EnemyPub#rolePublic.roleName
                                                      ,RankType) of
                ?undefined ->
                    ?ERR("insert_new_dojangrank_fightrec fail",[]),
                    ReplayInfo = ?undefined,
                    Reward = ?undefined,
                    ResultMsg = 
                        #sc_dojangrank_fight{result = 4
                                            ,fightInfo = []
                                            ,p_dojangrank_rank_self = to_p_dojangrank_rank(RoleID)
                                            ,p_dojangrank_rank_list = get_ranklist_sub(RankType,RoleID)
                                            ,reward = #p_reward_info{coin       = 0
                                                                    ,roleExp     = 0
                                                                    ,gerExp      = 0
                                                                    ,gold        = 0
                                                                    ,reputation  = 0
                                                                    ,itemList    = []
                                                                    ,gerList     = []}};
                ReplayUID ->
                    ReplayInfo = 
                        #p_dojang_replay_info{attackerName = RoleName
                                             ,defenderName = EnemyPub#rolePublic.roleName
                                             ,attackerNewRank = AttackerNewRank
                                             ,defenderNewRank = DefenderNewRank
                                             ,replayUID = ReplayUID
                                             ,time = util:now()
                                             ,rank_type = RankType
                                             ,is_win = IsWinNum},
                    if
                        FightRecord#sc_fight_request.result =:= true 
                          andalso AtkRankInfo =:= ?undefined ->
                            %% 替代原有
                            SelectedGerType = 
                                lists:nth(1
                                         ,[E#ger.gerBase#gerBase.gerTypeID*256 + E#ger.gerBase#gerBase.gerQuality||E<-AtkFighterList]),
                            NewAtk = #dojangrank{role_id = RoleID
                                                ,rank = DefRankInfo#dojangrank.rank
                                                ,selected_ger_type  = SelectedGerType
                                                ,fighter_array_cfg = AtkFighterList
                                                ,fighter_power = FightPower},
                            ets:insert(EtsName,NewAtk);
                        FightRecord#sc_fight_request.result =:= true 
                          andalso AtkRankInfo /= ?undefined
                          andalso AtkRankInfo#dojangrank.rank > DefRankInfo#dojangrank.rank ->
                            NewAtk = 
                                AtkRankInfo#dojangrank{rank = DefRankInfo#dojangrank.rank
                                                      ,fighter_array_cfg = AtkFighterList
                                                      ,fighter_power = FightPower},
                            NewDef = 
                                DefRankInfo#dojangrank{rank = AtkRankInfo#dojangrank.rank},
                            ets:insert(EtsName,[NewAtk,NewDef]);
                        true andalso AtkRankInfo /= ?undefined->
                            NewAtk = AtkRankInfo#dojangrank{fighter_array_cfg = AtkFighterList
                                                                     ,fighter_power = FightPower},
                            ets:insert(EtsName,NewAtk);
                        true ->
                            SelectedGerType = 
                                lists:nth(1
                                         ,[E#ger.gerBase#gerBase.gerTypeID*256 + E#ger.gerBase#gerBase.gerQuality||E<-AtkFighterList]),
                            NewAtk = #dojangrank{role_id = RoleID
                                                ,rank = 1001
                                                ,selected_ger_type  = SelectedGerType
                                                ,fighter_array_cfg = AtkFighterList
                                                ,fighter_power = FightPower}
                    end,
                    PDojangrankRankSelf = to_p_dojangrank_rank(NewAtk),
                    Reward = if
                                 FightRecord#sc_fight_request.result ->
                                     data_dojangrank:get({battle_reward_win,RankType});
                                 true ->
                                     data_dojangrank:get({battle_reward_fail,RankType})
                             end,
                    ResultMsg = 
                        #sc_dojangrank_fight{result = 1
                                            ,fightInfo = [FightRecord]
                                            ,p_dojangrank_rank_self = PDojangrankRankSelf
                                            ,p_dojangrank_rank_list = get_ranklist_sub(RankType,RoleID)
                                            ,reward = activity_server:sell_reward2p_reward_info(Reward)}
            end
    end,
    case catch role_lib:send_server(RoleID, {dojangrank_fight_res,ResultMsg,ReplayInfo,Reward,PayType}) of
        {'EXIT',Error}->
            ?ERR("dojangrank_fight error",[Error]);
        _ ->
            ignore
    end,
    {noreply,State};

handle_info({get_history_other_detail,RoleID,RankType,TarRoleID},State) ->
    {_HistorySession,HistoryRankGroup} = get(?pd_world_history_rank),
    case lists:keysearch(TarRoleID, #p_dr_world_rank.roleID, lists:nth(RankType, HistoryRankGroup)) of
        {value,RankInfo} ->
            ?unicast(RoleID,#sc_dojangrank_world_ger_view_other{role_info = RankInfo
                                                               ,ger_info_list = RankInfo#p_dr_world_rank.ger_info_list});
        false ->
            ?ERR("get_history_other_detail error ~w",[])
    end,
    {noreply,State};

handle_info({set_selected_gerType,RoleID,RankType,SelectedGerType},State) ->
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    case ets:match_object(EtsName, #dojangrank{role_id=RoleID, _='_'}) of
        [] ->
            ignore;
        [Info] ->
            ets:insert(EtsName, Info#dojangrank{selected_ger_type=SelectedGerType})
    end,
    {noreply,State};

handle_info({get_dojangrank_world_top_replay_list,RoleID,RankType},State) ->
    RemoteVersion = get(?pd_world_top_fightrec_remote),
    {LocalVersion,LocalGroup} = get(?pd_world_top_fightrec_local),
    if
        RemoteVersion =:= LocalVersion ->
            ?unicast(RoleID,#sc_dojangrank_world_top_replay_list{infoList = [E||{_,_,E,_}<-lists:nth(RankType, LocalGroup)]});
        true ->
            dojangrank_world_server:get_dojangrank_world_top_replay_list(self()),
            put(?pd_world_top_fightrec_req_list,[{RoleID,RankType}|get(?pd_world_top_fightrec_req_list)])
    end,
    {noreply,State};

handle_info({get_dojangrank_world_top_replay_detail,RoleID,ReplayUID},State) ->
    {_,LocalGroup} = get(?pd_world_top_fightrec_local),
    case search_top_replay(ReplayUID,LocalGroup) of
        [] -> 
            ?unicast(RoleID,#sc_dojangrank_world_replay_detail{result = 2
                                                  ,fightInfo = []});
        [Replay] -> 
            ?unicast(RoleID,#sc_dojangrank_world_replay_detail{result = 1 
                                                  ,fightInfo = [Replay]})
    end,
    {noreply,State};

handle_info({get_dojangrank_world_top_replay_list_res,RemoteVersion,RemoteGroup},State) ->
    lists:foreach(fun({RoleID,RankType})->
            ?unicast(RoleID,#sc_dojangrank_world_top_replay_list{infoList = [E||{_,_,E,_}<-lists:nth(RankType, RemoteGroup)]})
        end, get(?pd_world_top_fightrec_req_list)),
    put(?pd_world_top_fightrec_req_list,[]),
    put(?pd_world_top_fightrec_local,{RemoteVersion,RemoteGroup}),
    OldRemoteVersion = get(?pd_world_top_fightrec_remote),
    if
        OldRemoteVersion < RemoteVersion ->
            put(?pd_world_top_fightrec_remote,RemoteVersion);
        true -> ignore
    end,
    {noreply,State};

%% 有三种情况会运行这个函数，
%% 第一种，在380版本更新后第一次启动后，该函数会起作用，自动初始化跨服pvp
%% 第二种，每次赛季切换的时候
%% 第三种，新服开服

handle_info(world_init,State) ->
    NowSession = dojangrank_world_server:get_now_session(),
    ServerID = data_setting:get(server_id),
    case get(?pd_world_init_flag) of
        ?undefined -> 
            if
                NowSession =:= State#state.current_world_session ->
                    ?INFO("world_init already sucess ~w",[NowSession]);
                true ->
                    ?ERR("world_init error ~w  <<<<<<>>>>>>> ~w",[NowSession, State#state.current_world_session])
            end;
        {RobotInfoList,RoleListGroup} when NowSession /= State#state.current_world_session ->
            clear_world_session_pd(),
            lists:foreach(fun(RankType)-> 
                                EtsName = dojangrank_world_server:get_ets_name(RankType),
                                ets:delete_all_objects(EtsName)
                          end, lists:seq(1, 8)),
            ?ERR("world_init ~w,~w",[length(RobotInfoList),[{E,[element(1,E2)||E2<-element(2, lists:nth(E,RoleListGroup))]}||E<-lists:seq(1, 8)]]),
            if
                [] =:= RobotInfoList ->
                    case get_world_robot_info() of
                        [] -> ?INFO("world_init waiting robot data",[]);    %% 开服后，机器人还没创建好
                        NewRobotInfoList ->
                            put(?pd_world_init_flag,{NewRobotInfoList,RoleListGroup}),
                            ?ERR("send update_top10 b RobotInfoList:~w",[length(NewRobotInfoList)]),
                            send_msg:direct_by_name(?master_server, dojangrank_world_server
                                                   ,{update_top10,ServerID,NewRobotInfoList,RoleListGroup,self()})
                    end;
                true ->
                    ?ERR("send update_top10 a RobotInfoList:~w",[length(RobotInfoList)]),
                    send_msg:direct_by_name(?master_server, dojangrank_world_server
                                           ,{update_top10,ServerID,RobotInfoList,RoleListGroup,self()})
            end,
            erlang:send_after(600*1000, ?MODULE, world_init)  %% 十分钟后尝试下一次  TODO 改成宏
    end,
    {noreply,State};

handle_info({world_init_req,NewSession,TopHero},State) ->
    NewState = 
        case dojangrank_world_server:get_now_session() of
            NewSession when State#state.current_world_session /= NewSession -> 
                ?ERR("world_init_req ok ~w ~w",[NewSession,TopHero]),
                do_world_reward(TopHero),
                erlang:erase(?pd_world_init_flag),
                State#state{current_world_session = NewSession};
            _ -> 
                ?ERR("world_init_req error ~w loacl:~w",[dojangrank_world_server:get_now_session(),State#state.current_world_session]),
                State
        end,
    {noreply,NewState};
        
%% 取历史数据
handle_info({req_dojangrank_world_rank,RoleID,RankType,Start,Length,2},State) ->
    {HistorySession,HistoryRankGroup} = get(?pd_world_history_rank),
    if
        State#state.current_world_session /= HistorySession ->
            case get(?pd_world_history_rank_req_list) of
                [] ->
                    put(?pd_world_history_rank_req_list,[{RoleID,RankType,Start,Length}]),
                    dojangrank_world_server:get_dojangrank_world_history_list(self());
                ReqList ->
                    put(?pd_world_history_rank_req_list,[{RoleID,RankType,Start,Length}|ReqList])
            end;
        true ->
            RankMax = data_dojangrank:get(init_robot_num_max),
            TypeRankList = lists:nth(RankType, HistoryRankGroup),
            SubList = 
                lists:sublist(TypeRankList
                             , min(Start,RankMax)
                             , max(1,min(Length,RankMax - Start + 1))),
            DojangrankRankSelf = 
                case lists:keysearch(RoleID, #p_dr_world_rank.roleID, TypeRankList) of
                    {value,DRS} ->
                        DRS;
                    false ->
                        Pub = role_lib:get_rolePublic(RoleID),
                        #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(RoleID),
                        {FighterList,_,_,_} = role_data:get_otherRoleFighter(RoleID),
                        SelectedGerType = 
                            lists:nth(1
                                     ,[E#ger.gerBase#gerBase.gerTypeID*256 + E#ger.gerBase#gerBase.gerQuality||E<-FighterList]),
                        #p_dr_world_rank{roleID = RoleID
                                        ,fightPower = Pub#rolePublic.fightPower 
                                        ,isMale = Pub#rolePublic.isMale 
                                        ,title = Pub#rolePublic.title 
                                        ,head = Pub#rolePublic.head 
                                        ,level = Pub#rolePublic.level 
                                        ,roleName = Pub#rolePublic.roleName 
                                        ,vip = Pub#rolePublic.viplevel
                                        ,rank = 0
                                        ,selected_ger_type = SelectedGerType
                                        ,skinID = SkinID
                                        ,serverID = data_setting:get(server_id)
                                        ,ger_info_list = to_dr_ger_info(FighterList)
                                        ,win_num = 0
                                        ,lost_num = 0
                                        ,total_num = 0
                                        ,rank_score = 0}
                end,    
            ?unicast(RoleID,#sc_dojangrank_world_rank{index = RankType
                                       ,p_dojangrank_rank_self = role_dojangrank:check_rank_out(DojangrankRankSelf)
                                       ,p_dojangrank_rank_list = role_dojangrank:check_rank_out(SubList)})
    end,
    {noreply,State};
    
%% 本届数据，或者本服，或者全服
handle_info({req_dojangrank_world_rank,RoleID,RankType,Start,Length,Type},State) ->
    Now = util:now(),
    LastTs = get(?pd_last_req_world_timestamp),
    WaitList = get(?pd_req_world_role_list),  %% TODO 要注意维护这个变量,同一个用户只保留一份请求
    if
        Now > LastTs + ?update_server_list_interval ->  %% 需要执行慢请求
            case WaitList of
                []->
                    put(?pd_req_world_role_list,[{RoleID,RankType,Start,Length,Type}]),
                    dojangrank_world_server:req_rank_slow(State#state.world_timestamp_version
                                                         ,data_setting:get(server_id),self());
                _ ->
                    put(?pd_req_world_role_list,[{RoleID,RankType,Start,Length,Type}|WaitList])
            end;
        true ->     %% 处于激活列表中，直接使用内存数据
            DojangrankRankSelf = get_world_dojangrank(RoleID,RankType),
            ?unicast(RoleID,#sc_dojangrank_world_rank{index = RankType
                                       ,p_dojangrank_rank_self = role_dojangrank:check_rank_out(DojangrankRankSelf)
                                       ,p_dojangrank_rank_list = role_dojangrank:check_rank_out(get_world_sub_rank(RankType,Start,Length,Type))})
    end,
    {noreply,State};

%% 专门用来修复错误的数据
handle_info({fix_world_rank,[]},State) ->
    ?ERR("fix_world_rank finish"),
    {noreply,State};
handle_info({fix_world_rank,[{RankType,FixRankList}|OtherFixList]},State) ->
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    case lists:all(fun(R)-> erlang:is_record(R, p_dr_world_rank) end, FixRankList) of
        true -> 
            ?ERR("fix_world_rank do ~w",[length(FixRankList)]),
            ets:insert(EtsName, FixRankList);
        false -> 
            ?ERR("fix_world_rank finish")
    end,
    erlang:send(?MODULE, {fix_world_rank,OtherFixList}),
    {noreply,State};

%% 补偿次数
handle_info({fix_pay_time,FixDataList},State) ->
    lists:foreach(fun({RoleID,AddList})->
                        case role_lib:is_online(RoleID) of
                            true -> 
                                catch role_lib:send_server(RoleID, {fix_pay_time, AddList});
                            false ->
                                Dojangrank = db_sql:get_role_dojangrank(RoleID),
                                NewWorldPaidTimeList = 
                                    lists:map(fun(RankType)-> 
                                                    AddValue = lists:nth(RankType, AddList),
                                                    lists:nth(RankType, Dojangrank#role_dojangrank.world_paid_time) + AddValue  
                                              end, lists:seq(1, 8)),
                                db_sql:set_role_dojangrank(RoleID
                                                          ,Dojangrank#role_dojangrank{world_paid_time = NewWorldPaidTimeList}),
                                ?ERR("fix_pay_time1 (~w)~w",[RoleID,AddList])
                        end 
                  end, FixDataList),
    {noreply,State};

handle_info(refresh_world_rank_start,State) ->
    put(?pd_refresh_world_rank_flag,true),
    ?INFO("refresh_world_rank start",[]),
    dojangrank_world_server:get_dojangrank_world_history_list(self()),
    erlang:send(self(), refresh_world_rank),
    {noreply,State};
%% 请求全部世界排名,并不会把自己加入激活列表,
handle_info(refresh_world_rank,State) ->
    ?INFO("refresh_world_rank(~w)",[get(?pd_refresh_world_rank_flag)]),
    case get(?pd_refresh_world_rank_flag) of
        true ->
            NeedInit = 
                lists:any(fun(RankType)->
                                EtsName = dojangrank_world_server:get_ets_name(RankType),
                                0 =:= ets:info(EtsName,size)
                          end, lists:seq(1, 8)),
            if
                NeedInit ->
                    dojangrank_world_server:req_rank_slow({0,0},data_setting:get(server_id),self()),
                    erlang:send_after(60*1000, ?MODULE, refresh_world_rank);
                true ->
                    erlang:erase(?pd_refresh_world_rank_flag)
            end;
        ?undefined ->
            ?INFO("refresh_world_rank end",[]),
            ignore
    end,
    {noreply,State};

%% CurrentSession 是当前赛季
handle_info({get_dojangrank_world_history_list_req,CurrentSession,HistoryRankGroupBin},State) ->
    HistoryRankGroup = lists:map(fun(Bin)-> db_sql:uncompress_decode(Bin) end,HistoryRankGroupBin),
    if
        CurrentSession /= State#state.current_world_session ->
            ?ERR("get_dojangrank_world_history_list_req session error ~w",[{CurrentSession,State#state.current_world_session
                                                                          ,dojangrank_world_server:get_now_session()}]);
        true -> ignore
    end,
    put(?pd_world_history_rank,{State#state.current_world_session,HistoryRankGroup}),
    RankMax = data_dojangrank:get(init_robot_num_max),
    lists:foreach(fun({RoleID,RankType,Start,Length})-> 
            TypeRankList = lists:nth(RankType, HistoryRankGroup),
            SubList = 
                lists:sublist(TypeRankList
                             , min(Start,RankMax)
                             , max(1,min(Length,RankMax - Start + 1))),
            DojangrankRankSelf = get_world_dojangrank(RoleID,RankType),
            ?unicast(RoleID,#sc_dojangrank_world_rank{index = RankType
                                       ,p_dojangrank_rank_self = role_dojangrank:check_rank_out(DojangrankRankSelf)
                                       ,p_dojangrank_rank_list = role_dojangrank:check_rank_out(SubList)})
        end, get(?pd_world_history_rank_req_list)),
    {noreply,State};

%% 得到全部世界排名,更新最新的时间戳和数据版本记录,并不会把自己加入激活列表
handle_info({notice_update_rank,{NewPointTimestamp,NewPointVersion},IsAll,UpdateListGroup,TopRecVersion},State)->
    ?INFO("notice_update_rank ~w:~w",[{NewPointTimestamp,NewPointVersion},erlang:length(UpdateListGroup)]),
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    ServerID = data_setting:get(server_id),
    lists:foreach(fun({RankType,RankList})-> 
                EtsName = dojangrank_world_server:get_ets_name(RankType),
                if IsAll ->
                       ets:delete_all_objects(EtsName);
                   true -> ignore end,
                case lists:all(fun(R)-> erlang:is_record(R, p_dr_world_rank) end, RankList) of
                    true ->
                        RankListFiltered = 
                            lists:filter(fun(RankInfo)-> 
                                    RankInfo#p_dr_world_rank.rank =< InitRobotNum 
                                         orelse RankInfo#p_dr_world_rank.serverID =:= ServerID 
                                end, RankList),
                        %% 删除本服的旧数据，其实主要是删除未上榜的旧数据
                        RankListFilteredLocal = 
                            lists:filter(fun(RI0)-> 
                                    RI0#p_dr_world_rank.serverID =:= ServerID 
                                end, RankListFiltered),
                        lists:foreach(fun(RI1)-> 
                                              ets:match_delete(EtsName, #p_dr_world_rank{roleID=RI1#p_dr_world_rank.roleID, _='_'}) 
                                      end, RankListFilteredLocal),
                        ets:insert(EtsName, RankListFiltered);
                    false -> ?ERR("notice_update_rank error",[])
                end
        end, UpdateListGroup),
    if
        IsAll -> erlang:erase(?pd_refresh_world_rank_flag);
        true -> ignore
    end,
    lists:foreach(fun({RoleID,RankType,Start,Length,Type})-> 
            DojangrankRankSelf = get_world_dojangrank(RoleID,RankType),
            ?unicast(RoleID,#sc_dojangrank_world_rank{index = RankType
                                       ,p_dojangrank_rank_self = role_dojangrank:check_rank_out(DojangrankRankSelf)
                                       ,p_dojangrank_rank_list = role_dojangrank:check_rank_out(get_world_sub_rank(RankType,Start,Length,Type))})
        end,  get(?pd_req_world_role_list)),
    put(?pd_last_req_world_timestamp,NewPointTimestamp),
    put(?pd_req_world_role_list,[]),
    put(?pd_world_top_fightrec_remote,max(get(?pd_world_top_fightrec_remote),TopRecVersion)),
    {noreply,State#state{world_timestamp_version = {NewPointTimestamp,NewPointVersion}}};
handle_info({quick_notice_rank,NewestVersion,Timestamp,GroupList,TopRecVersion},State) ->
    ?INFO("quick_notice_rank:~w  v:~w    ----->   new:~w",[[{T,length(L)}||{T,L}<-GroupList]
                                                          ,State#state.world_timestamp_version
                                                          ,{Timestamp,NewestVersion}]),
    ServerID = data_setting:get(server_id),
    RankMax = data_dojangrank:get(init_robot_num_max),
    lists:foreach(fun({T,L0})->
                    L = [E||E<-L0,E#p_dr_world_rank.serverID =:= ServerID orelse E#p_dr_world_rank.rank =< RankMax],
                    EtsName = dojangrank_world_server:get_ets_name(T),
                    ets:insert(EtsName, L)
                  end, GroupList),
%%     put(?pd_last_req_world_timestamp,Timestamp),   %% 这里不能 刷新时间，主从服都是主动的情况才刷新
    put(?pd_world_top_fightrec_remote,max(get(?pd_world_top_fightrec_remote),TopRecVersion)),
    {noreply,State#state{world_timestamp_version = {Timestamp,NewestVersion}}};
handle_info({debug_force_init_session,_FromPid},State) ->
    RobotInfoList = get_world_robot_info(),
    RoleListGroup = get_top10_group(),
    ?ERR("----- debug_force_init_session ----->>>>~w,~w  <<<<<<<<>>>>>>>>    ~w",[length(RobotInfoList)
                                                                                 ,[length(element(2, lists:nth(E,RoleListGroup)))||E<-lists:seq(1, 8)]
                                                                                 ,RoleListGroup]),
    put(?pd_world_init_flag,{RobotInfoList,RoleListGroup}),
    erlang:send(?MODULE, world_init),
    {noreply,State#state{current_world_session = 0
                        ,world_timestamp_version = {0,0}}};
%% 战斗结果回复， 游戏服的排名数据，只有此处不更新版本和时间戳，所以也可能极为短暂的出现，排名只有单个更新的情况
%% UpdateList 只有两种情况，只更新了挑战者；或者被挑战者也是本服的，两个人都更新了
%% [dojangrank_world_fight_res,6,[],undefined,undefined,1,1,[],{false,0}]
handle_info({dojangrank_world_fight_res,Result,_,?undefined,_,PayType,RankType,UpdateListAll,_},State) ->
    [NewAtkRankInfo|OtherUpdate] = UpdateListAll,
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    case ets:match_object(EtsName, #p_dr_world_rank{roleID=NewAtkRankInfo#p_dr_world_rank.roleID, _='_'}) of
        [DojangrankRankSelf] ->
            AtkRoleID = DojangrankRankSelf#p_dr_world_rank.roleID,
            case catch role_lib:send_server(AtkRoleID, {dojangrank_world_fight_res,Result,[],?undefined
                                                       ,DojangrankRankSelf,?undefined,PayType,RankType}) of
                {'EXIT',Error}-> ?ERR("dojangrank_fight error 1 ~w",[Error]);
                _ -> ok
            end;
        Error -> 
            ?ERR("dojangrank_fight error -fail- ~w",[Error])
    end,
    %% NewAtkRankInfo 千万不能写入，并不是真实数据，只是传递roleID
    ets:insert(EtsName,OtherUpdate),
    {noreply,State};
handle_info({dojangrank_world_fight_res,Result,FightInfo,ReplayInfo0,Reward,PayType,RankType,UpdateListAll,{_IsTopRec,TopFightRecUid}},State) ->
    ?INFO("dojangrank_world_fight_res  -------start------   ~w",[{dojangrank_world_fight_res,Result,FightInfo,ReplayInfo0,Reward,PayType,RankType,UpdateListAll}]),
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    ServerID = data_setting:get(server_id),
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    [NewAtkRankInfo,NewDefRankInfo] = UpdateListAll,
    AtkRoleID = NewAtkRankInfo#p_dr_world_rank.roleID,
    DefRoleID = NewDefRankInfo#p_dr_world_rank.roleID,
    AtkServerID = NewAtkRankInfo#p_dr_world_rank.serverID,
    DefServerID = NewDefRankInfo#p_dr_world_rank.serverID,
    %% 战斗完毕，不管成功失败，先请求一次数据
    Now = util:now(),
    LastTs = get(?pd_last_req_world_timestamp),
    if
        Now > LastTs + ?update_server_list_interval ->  %% 需要执行慢请求
            dojangrank_world_server:req_rank_slow(State#state.world_timestamp_version
                                                 ,data_setting:get(server_id),self());
        true -> ignore 
    end,
    if
        Result =:= 1 andalso FightInfo /= [] -> 
            FightRecUid = db_sql:insert_new_dojangrank_world_fightrec(AtkRoleID,DefRoleID,AtkServerID,DefServerID
                                                                     ,ReplayInfo0#p_dr_dojang_replay_info.is_win
                                                                     ,lists:nth(1,FightInfo)
                                                                     ,ReplayInfo0#p_dr_dojang_replay_info.attacker_info
                                                                     ,ReplayInfo0#p_dr_dojang_replay_info.defender_info
                                                                     ,RankType),
            ReplayInfo = ReplayInfo0#p_dr_dojang_replay_info{replayUID = FightRecUid},
            ?INFO("dojangrank_world_fight_res udpate fighter ~w",[UpdateListAll]),
            UpdateListAllFiltered = 
                [E||E<-UpdateListAll,E#p_dr_world_rank.serverID =:= ServerID orelse E#p_dr_world_rank.rank =< InitRobotNum],
            %% 删除本服的旧数据，其实主要是删除未上榜的旧数据
            RankListFilteredLocal = 
                lists:filter(fun(RI0)-> 
                        RI0#p_dr_world_rank.serverID =:= ServerID 
                    end, UpdateListAllFiltered),
            lists:foreach(fun(RI1)-> 
                                  ets:match_delete(EtsName, #p_dr_world_rank{roleID=RI1#p_dr_world_rank.roleID, _='_'}) 
                          end, RankListFilteredLocal),            
            ets:insert(EtsName,UpdateListAllFiltered),
            if
                TopFightRecUid /= 0 ->
                    put(?pd_world_top_fightrec_remote,max(get(?pd_world_top_fightrec_remote),TopFightRecUid));
                true ->
                    ignore
            end,
            {BeforeRank,AfterRank} = if
                                         1 =:= ReplayInfo0#p_dr_dojang_replay_info.is_win ->
                                             {NewDefRankInfo#p_dr_world_rank.rank,NewAtkRankInfo#p_dr_world_rank.rank};
                                         true ->
                                             {NewAtkRankInfo#p_dr_world_rank.rank,NewDefRankInfo#p_dr_world_rank.rank}
                                     end,
            ?INFO("dojangrank_world_fight_res rank change ~w  ---------   ~w",[{BeforeRank,AfterRank},[util:now(),AtkServerID,"",AtkRoleID
                                               ,NewAtkRankInfo#p_dr_world_rank.level
                                               ,NewAtkRankInfo#p_dr_world_rank.fightPower
                                               ,NewAtkRankInfo#p_dr_world_rank.vip
                                               ,RankType,ReplayInfo0#p_dr_dojang_replay_info.is_win,BeforeRank,AfterRank
                                               ,DefServerID,"",DefRoleID
                                               ,NewDefRankInfo#p_dr_world_rank.level
                                               ,NewDefRankInfo#p_dr_world_rank.fightPower
                                               ,NewDefRankInfo#p_dr_world_rank.vip]]),
            behavior_dojankrank_world_fight:log(util:now(),AtkServerID,"",AtkRoleID
                                               ,NewAtkRankInfo#p_dr_world_rank.level
                                               ,NewAtkRankInfo#p_dr_world_rank.fightPower
                                               ,NewAtkRankInfo#p_dr_world_rank.vip
                                               ,RankType,ReplayInfo0#p_dr_dojang_replay_info.is_win,BeforeRank,AfterRank
                                               ,DefServerID,"",DefRoleID
                                               ,NewDefRankInfo#p_dr_world_rank.level
                                               ,NewDefRankInfo#p_dr_world_rank.fightPower
                                               ,NewDefRankInfo#p_dr_world_rank.vip);
        FightInfo =:= [] ->
            ReplayInfo = ReplayInfo0,
            dojangrank_world_server:req_rank_slow(State#state.world_timestamp_version
                                                 ,data_setting:get(server_id),self());
        true -> 
            ReplayInfo = ReplayInfo0
    end,
    if
        ServerID =:= AtkServerID ->
            case catch role_lib:send_server(AtkRoleID, {dojangrank_world_fight_res,Result,FightInfo,ReplayInfo
                                                       ,ReplayInfo0#p_dr_dojang_replay_info.attacker_info,Reward,PayType,RankType}) of
                {'EXIT',Error}-> ?ERR("dojangrank_fight error 1 ~w",[Error]);
                _ -> ignore
            end;
        true -> ignore
    end,
    if
        ServerID =:= DefServerID ->
            case role_lib:is_online(DefRoleID) of
                true -> 
                    %% PayType:0 被打的
                    case catch role_lib:send_server(DefRoleID, {dojangrank_world_fight_res,Result,FightInfo,ReplayInfo
                                                               ,ReplayInfo0#p_dr_dojang_replay_info.attacker_info,Reward,0,RankType}) of
                        {'EXIT',Error0}-> ?ERR("dojangrank_fight error 2~w",[Error0]);
                        _ -> ignore
                    end;
                false -> ignore
            end;
        true -> ignore
    end,
    {noreply,State};
handle_info({set_world_select_ger_type_res,Result,RankType,SelectedGerType,RoleID},State) ->
    if
        1 =:= Result ->
            Now = util:now(),
            LastTs = get(?pd_last_req_world_timestamp),
            if
                Now > LastTs + ?update_server_list_interval ->  %% 需要执行慢请求
                    dojangrank_world_server:req_rank_slow(State#state.world_timestamp_version
                                                         ,data_setting:get(server_id),self());
                true -> ignore 
            end,
            EtsName = dojangrank_world_server:get_ets_name(RankType),
            [DojangrankRankSelf] = ets:match_object(EtsName, #p_dr_world_rank{roleID=RoleID, _='_'}),
            NewRankInfo = DojangrankRankSelf#p_dr_world_rank{selected_ger_type = SelectedGerType},
            ets:insert(EtsName,NewRankInfo);
        true ->
            ignore
    end,
    ?unicast(RoleID,#sc_dojangrank_world_select_ger_type{index = RankType
                              ,selected_ger_type = SelectedGerType
                              ,result = Result}),
    {noreply,State};
handle_info(test_status,State) ->
    RobotIdLen = erlang:length(db_sql:get_roleIDList_Robot()),
    LocalRankStatus = 
        lists:all(fun(RankType)->
                EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
                ets:info(EtsName, size) > 0
            end, lists:seq(1, 8)),
    if
        RobotIdLen /= 9999 ->
            ?ERR("跨服pvp异常，机器人不足"),io:format("跨服pvp异常，机器人不足");
        LocalRankStatus /= true ->
            ?ERR("本服领域大师赛数异常，无法进行跨服pvp"),io:format("本服领域大师赛数异常，无法进行跨服pvp");
        true ->
            put(test_status_flag,true),
            send_msg:direct_by_name(?master_server, dojangrank_world_server, {test_status,self()}),
            StatusString = io_lib:format("start time:~w~nversion:~w~nrank_data:~w~ncurrent_session_state:~w~n"
                                        , [data_dojangrank:get(start_date)
                                          ,State#state.world_timestamp_version
                                          ,dojangrank_world_server:debug_show_rank_num()
                                          ,role_dojangrank:get_open_state()]),
            ?ERR("~s",[StatusString]),io:format("~s",[StatusString])
    end,
    {noreply,State};
handle_info({test_status_res,ResString},State) ->
    erase(test_status_flag),
    ?ERR("~s",[ResString]),io:format("~s",[ResString]),
    {noreply,State};
handle_info(test_status_timeout,State) ->
    case get(test_status_flag) of
        ?undefined -> ignore;
        _ -> ?ERR("lost center server"),io:format("lost center server. 跨服服务器链接异常")
    end,
    erase(test_status_flag),
    {noreply,State};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info(persist_tick,State) ->
    set_persist_interval(State),
    do_persist(State),
    StateNew = check_state(State),
    {noreply,StateNew};
handle_info({client_msg,RoleID,Msg},State) ->
    NewState = do_handle_msg(Msg,RoleID,State),
    {noreply,NewState};
handle_info(test_crash,State) ->
    A = 1,B = 2,
    A = B,
    {noreply,State};
handle_info(test_check_reward,State) ->
    StateNew = check_state(State),
    ?INFO("test_check_reward ~w->~w",[State,StateNew]),
    {noreply,StateNew};
handle_info(test_do_reward,State) ->
    do_reward(),
    ?INFO("do_reward ",[]),
    {noreply,State};
handle_info(Info,State) ->
    ?ERR("未知的Info:~w",[Info]),
    {noreply,State}.

%% ====================================================================
%% do_handle_msg functions
%% ====================================================================

do_handle_msg(Info,_RoleID,State) ->
    ?ERR("unknown Msg:~w",[Info]),
    State.

%% ====================================================================
%% Internal functions
%% ====================================================================

do_dojangrank_fight(RoleIDA,{FighterListA,RoleLieuAddA,TalentA,TrSpecialA},RoleIDB,DefFighterArrayCfg)->
    {_,RoleLieuAddB,TalentB,TrSpecialB} = role_data:get_otherRoleFighter(RoleIDB),
    SkinInfoA = role_data:get_otherRoleSkinInfo(RoleIDA),
    SkinInfoB = role_data:get_otherRoleSkinInfo(RoleIDB),
    
    EquipListA = role_data:get_otherRoleItemEquips(RoleIDA),
    GerEquipListA = role_item:assort_ger_equiplist(EquipListA),
    LegendAddListA = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipListA],
    
    EquipListB = role_data:get_otherRoleItemEquips(RoleIDB),
    GerEquipListB = role_item:assort_ger_equiplist(EquipListB),
    LegendAddListB = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipListB],
    {_Result, FightRecord, _State} = 
        role_fight:new(FighterListA,DefFighterArrayCfg,RoleLieuAddA,RoleLieuAddB
                      ,TalentA,TalentB,TrSpecialA,TrSpecialB
                      ,SkinInfoA,SkinInfoB,LegendAddListA,LegendAddListB),
    {FightRecord,FighterListA}.
      
check_state(State0)->
    State1 = check_reward(State0),
    check_world(State1).

check_reward(State)->
    Now = util:now(),
    Day = erlang:date(),
    NextSecond = util:datetime_to_seconds({Day,{22,00,00}}),
    if
        State#state.reward_date /= Day andalso NextSecond =< Now ->
            do_reward(),
            State#state{reward_date = Day};
        true -> State
    end.

set_persist_interval(State)->
    Now = util:now(),
    Day = erlang:date(),
    NextSecond1 = util:datetime_to_seconds({Day,{22,00,00}}),
    NextSecond2 = role_dojangrank:get_season_endtime(),
    TodayZero = util:datetime_to_seconds({Day,{0,0,0}}),
    NextSecond_w_1 = TodayZero + data_dojangrank:get(init_step_1),
    NextSecond_w_2 = TodayZero + data_dojangrank:get(init_step_2),
    PersistInterval1 = 
        if
            State#state.reward_date /= Day 
              andalso NextSecond1 > Now 
              andalso (Now + ?persist_interval) > NextSecond1 ->
                NextSecond1 - Now;
            true ->
                ?persist_interval
        end,
    PersistInterval2 = 
        if
            NextSecond2 > Now -> NextSecond2 - Now;
            true -> ?persist_interval 
        end,
    TodayZero = util:datetime_to_seconds({Day,{0,0,0}}),
    PersistInterval_w_1 = 
        if
            NextSecond_w_1 > Now -> NextSecond_w_1 - Now;
            true -> ?persist_interval 
        end,
    PersistInterval_w_2 = 
        if
            NextSecond_w_2 > Now -> NextSecond_w_2 - Now;
            true -> ?persist_interval 
        end,
    erlang:send_after(lists:min([PersistInterval1,PersistInterval2,PersistInterval_w_1,PersistInterval_w_2])*1000,self(),persist_tick).

do_persist(State)->
    lists:foreach(fun(T)->
                    db_sql:set_dojangrank_rank(T,ets:tab2list(list_to_atom(lists:concat(['ets_dojangrank_',T]))))
            end,lists:seq(1,8)),
    db_sql:set_etc(?DB_ETC_KEY_DOJANGRANK,[State]),
    erlang:garbage_collect().

%% 有三种情况会运行这个函数，
%% 第一种，在380版本更新后第一次启动后，该函数会起作用，自动初始化跨服pvp
%% 第二种，每次赛季切换的时候
%% 第三种，新服开服
check_world(State)->
    OpenState = role_dojangrank:get_open_state(),
    NowSession = dojangrank_world_server:get_now_session(),
    PdInitFlag = get(?pd_world_init_flag),
    if
        PdInitFlag /= ?undefined ->
            State;
        0 =:= State#state.current_world_session ->
            DateTime = {date(),time()},
            StartTime = data_dojangrank:get(start_date),
            if
                %% 第三种，新服开服, 不需要做什么事情，既不需要提供机器人，也没有符合参赛资格的玩家
                DateTime >= StartTime ->
                    ?ERR("----- init_world_data 3 ----- do nothing"),
                    State#state{current_world_session = dojangrank_world_server:get_now_session()};
                %% 第一种，在380版本更新后第一次启动后，该函数会起作用，自动初始化跨服pvp
                true ->
                    RobotInfoList = get_world_robot_info(),
                    RoleListGroup = get_top10_group(),
                    ?ERR("----- init_world_data 1 ----->>>>  Robot >>>>>>>>> ~w  RoleList >>>>>>>> ~w"
                        ,[length(RobotInfoList),[{E,[element(1,E2)||E2<-element(2, lists:nth(E,RoleListGroup))]}||E<-lists:seq(1, 8)]]),        %% 每次初始化，都输出到日志里防止意外
                    put(?pd_world_init_flag,{RobotInfoList,RoleListGroup}),
                    erlang:send(?MODULE, world_init),
                    State
            end;
        %% 第二种，普通赛季切换
        NowSession /= State#state.current_world_session 
          andalso 1 =:= OpenState ->
            RobotInfoList = get_world_robot_info(),
            RoleListGroup = get_top10_group(),
            ?ERR("----- init_world_data 2 ----->>>>  Robot >>>>>>>>> ~w  RoleList >>>>>>>> ~w"
                ,[length(RobotInfoList),erlang:length(RoleListGroup)]),        %% 每次初始化，都输出到日志里防止意外
            put(?pd_world_init_flag,{RobotInfoList,RoleListGroup}),
            %% 和前面的if分支相比，只是增加了延迟
            Now = util:now(),
            NextSecond = util:datetime_to_seconds({erlang:date(),{0,0,0}}) 
                        + data_dojangrank:get(init_step_1)                                   %% 同步的起始时间
                        + (data_setting:get(server_id) rem 600) + util:random_int(1,60),    %% TODO TEMP 避免所有服务器同时上传数据   default 600   1,60
            if
                NextSecond > Now ->
                    erlang:send_after((NextSecond - Now)*1000, ?MODULE, world_init);
                true ->
                    erlang:send(?MODULE, world_init)
            end,
            State;
        true ->
            State
    end.
    
get_world_robot_info()->
    ServerID = data_setting:get(server_id),
    RobotNum = data_dojangrank:get(init_robot_num_per),
    case erlang:length(db_sql:get_roleIDList_Robot()) of
        9999 ->
            lists:map(fun(Rank)->
                        RoleID0 = gen_account:gen_roleID(Rank),
                        %% 针对专服2区，机器人roleid比正常少了1000000，原因不可考证。加入以下代码解决问题
                        RoleID = case role_lib:get_rolePublic(RoleID0) of
                                     [] ->
                                         RoleID0 - 1000000;
                                     _ ->
                                         RoleID0
                                 end,
                        Pub = role_lib:get_rolePublic(RoleID),
                        #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(RoleID),
                        #p_dr_world_rank{roleID = RoleID
                                        ,fightPower = 0
                                        ,isMale = Pub#rolePublic.isMale 
                                        ,title = Pub#rolePublic.title 
                                        ,head = Pub#rolePublic.head 
                                        ,level = Pub#rolePublic.level 
                                        ,roleName = Pub#rolePublic.roleName 
                                        ,vip = 1
                                        ,rank = 0
                                        ,selected_ger_type = 0
                                        ,skinID = SkinID
                                        ,serverID = ServerID
                                        ,ger_info_list = []     %% robot,由world服务器赋值
                                        ,win_num = 0
                                        ,lost_num = 0
                                        ,total_num = 0
                                        ,rank_score = 0}        %% robot,由world服务器赋值
                end,lists:seq(1,min(9999,RobotNum)));
        _ ->
            []
       end.
    
get_top10_group()->
    ServerID = data_setting:get(server_id),
    lists:map(fun(RankType)-> 
            EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
            List1 = ets:tab2list(EtsName),
            Len = erlang:length(List1),
            List2 = 
                if
                    10 >= Len ->
                        List1;
                    true ->
                        {SplitList2,_} = lists:split(10, lists:sort(List1)),
                        SplitList2
                end,
            List3 = 
                lists:filter(fun(DR)->
                        IsRobot = tk_id:is_robot(DR#dojangrank.role_id),
                        IsRobot =:= false 
                    end, List2),
            {RankType,lists:map(fun(DR)-> 
                        RoleID = DR#dojangrank.role_id,
                        Pub = role_lib:get_rolePublic(RoleID),
                        #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(RoleID),
                        FighterList = DR#dojangrank.fighter_array_cfg, 
                        PDR = 
                            #p_dr_world_rank{roleID = RoleID
                                            ,fightPower = DR#dojangrank.fighter_power 
                                            ,isMale = Pub#rolePublic.isMale 
                                            ,title = Pub#rolePublic.title 
                                            ,head = Pub#rolePublic.head 
                                            ,level = Pub#rolePublic.level 
                                            ,roleName = Pub#rolePublic.roleName 
                                            ,vip = Pub#rolePublic.viplevel
                                            ,rank = 0
                                            ,selected_ger_type = DR#dojangrank.selected_ger_type 
                                            ,skinID = SkinID
                                            ,serverID = ServerID
                                            ,ger_info_list = to_dr_ger_info(FighterList)
                                            ,win_num = 0
                                            ,lost_num = 0
                                            ,total_num = 0
                                            ,rank_score = data_dojangrank:get(init_score)},
                        {_,RoleLieuAdd,Talent,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
                        SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
                        EquipList = role_data:get_otherRoleItemEquips(RoleID),
                        GerEquipList = role_item:assort_ger_equiplist(EquipList),
                        LegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList0)}||{GerID,EquipList0}<-GerEquipList],
                        %%{FighterListA,RoleLieuAddA,TalentA,TrSpecialA,SkinInfoA,LegendAddListA}
                        {PDR,{FighterList,RoleLieuAdd,Talent,TrSpecial,SkinInfo,LegendAddList}}
                end, List3)}
        end, lists:seq(1, 8)).

do_reward()->
    RewardInfoList = 
        lists:foldl(fun(T,AccList)->
                CfgList = data_dojangrank:get({mail_reward,T}),
%%                 MailStr = data_dojangrank:get({mail_str,T}),
                EtsName = list_to_atom(lists:concat(['ets_dojangrank_',T])),
                lists:foldl(fun(R,SubAccList)-> 
                        case tk_id:is_robot(R#dojangrank.role_id) of
                            false ->
                                Reward = get_reward_cfg(R#dojangrank.rank,CfgList),
                                case lists:keytake(R#dojangrank.role_id, 1, SubAccList) of
                                    {value,{_,OtherReward},Other} ->
                                        [{R#dojangrank.role_id,do_reward_insert_reward({T,R,Reward},OtherReward)}|Other];
                                    false ->
                                        [{R#dojangrank.role_id,do_reward_insert_reward({T,R,Reward},do_reward_insert_blank())}|SubAccList]
                                end;
                            true ->
                                SubAccList
                        end
                    end, AccList, ets:tab2list(EtsName))
            end,[],lists:seq(1,8)),
    lists:foreach(fun({RoleID,RewardList})->
            {StrList,AllReward} = 
                lists:foldr(fun
                        ({_T,?undefined,?undefined},{AccStr,AccReward})->
                             {[data_dojangrank:get(mail_str_norank)|AccStr]
                             ,AccReward};
                        ({_T,R,Reward},{AccStr,AccReward})->
                             {[io_lib:format("~s~w~s",[data_dojangrank:get(mail_str_front)
                                                        ,R#dojangrank.rank
                                                        ,data_dojangrank:get(mail_str_rear)])|AccStr]
                             ,role_reward:reward_plus_reward(AccReward, Reward)}
                    end, {[],{sell_reward,0,0,0,0,[],0,[]}}, lists:sort(RewardList)),
            ?INFO("do_reward(~w) ~w  AllReward:~w",[RoleID,StrList,AllReward]),
            mail_server:send_sys_mail(RoleID, ?MAIL_DOJANGRANK_REWARD, StrList, "", AllReward)
        end, RewardInfoList),
    erlang:garbage_collect().

do_reward_insert_reward({T,R,Reward},RewardList)->
    case lists:keytake(T, 1, RewardList) of
        {value,{T,?undefined,?undefined},OtherReward} ->
            [{T,R,Reward}|OtherReward];
        {value,Err,OtherReward} ->
            ?ERR("do_reward repeated reward (~w) ~w",[R#dojangrank.role_id,{Err,R}]),
            [{T,R,Reward}|OtherReward]
    end.

do_reward_insert_blank()->
    [{T,?undefined,?undefined}||T<-lists:seq(1, 8)].

get_reward_cfg(_Rank,[]) ->
    {sell_reward,0,0,0,0,[],0,[]};
get_reward_cfg(Rank,[{{Min,Max},R}|Other])->
    if
        Min =< Rank andalso Rank =< Max ->
            R;
        true ->
            get_reward_cfg(Rank,Other)
    end.

do_world_reward([])->
    ignore;
do_world_reward([{RankType,TopHeroes}|TopHeroOther])->
    Config = data_dojangrank:get(world_reward_pool),
    ServerID = data_setting:get(server_id),
    ?INFO("do_world_reward -1- (~w)    <<<>>>     ~w",[RankType,TopHeroes]),
    SignleRewardList = data_dojangrank:get({world_mail_reward,RankType}), %%  {sell_reward,9999,0,0,0,[{new_item,10619,10,1,0}],0,[]},
    {TotalA,TotalB} =
        lists:foldl(fun(R,{AccSum1,AccSum2})->
                    case tk_id:is_robot(R#p_dr_world_rank.roleID) of
                        false when ServerID =:= R#p_dr_world_rank.serverID ->
                            case get_reward_cfg(R#p_dr_world_rank.rank,SignleRewardList) of
                                {sell_reward,0,0,0,0,[],0,[]} -> ignore;
                                SignleReward ->
                                    mail_server:send_sys_mail(R#p_dr_world_rank.roleID, 1154+RankType, [R#p_dr_world_rank.rank], "", SignleReward)
                            end,
                            case get_reward_cfg(R#p_dr_world_rank.rank,Config) of
                                {A,B} ->
                                    {AccSum1 + A,AccSum2 + B};
                                Err ->
                                    ?ERR("do_world_reward error ~w",[Err]),
                                    {AccSum1,AccSum2}
                            end;
                        true -> {AccSum1,AccSum2}
                    end
            end, {0,0}, TopHeroes),
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    ?INFO("do_world_reward -2- ~w",[{TotalA,TotalB}]),
    lists:foreach(fun({RankIndex,RewardPercent})-> 
            ItemType = data_dojangrank:get({world_reward_item_type,RankType}),
            case ets:lookup(EtsName, RankIndex) of
                [] -> ignore;
                [LocalRole] ->
                    NumA = (TotalA * RewardPercent) div 10000,
                    NumB = (TotalB * RewardPercent) div 10000,
                    ?INFO("do_world_reward -3- ~w ~w ~w ~w",[LocalRole#dojangrank.role_id
                                                   ,tk_id:is_robot(LocalRole#dojangrank.role_id)
                                                   ,NumA
                                                   ,NumB]),
                    case tk_id:is_robot(LocalRole#dojangrank.role_id) of
                        false ->
                            if
                                NumA > 0 orelse NumB > 0 ->
                                    Reward = {sell_reward,0,0,0,NumB,[{new_item,ItemType,NumA,1,0}],0,[]},
                                    ?INFO("do_world_reward -4- ~w",[{LocalRole#dojangrank.role_id,NumA,NumB,Reward}]),
                                    mail_server:send_sys_mail(LocalRole#dojangrank.role_id, 1162+RankType, [RankIndex], "", Reward);
                                true -> ignore
                            end;
                        true -> ignore
                    end
            end
        end, data_dojangrank:get(world_reward_percent)),
    %% 清除上赛季数据
    %% TODO HERE 写入文件
    ets:delete_all_objects(dojangrank_world_server:get_ets_name(RankType)),
    do_world_reward(TopHeroOther).

init_process_data()->
    clear_world_session_pd(),
    All = ets:all(),
    % 初始化跨服排名列表
    lists:foreach(fun(RankType)->
                    EtsName = dojangrank_world_server:get_ets_name(RankType),
                    case lists:member(EtsName, All) of
                        true -> ignore;
                        false -> 
                            %% 注意！虽然ets名字和world进程相同，但内部数据结构不同
                            %% {排名,展示数据,阵容数据} 
                            ets:new(EtsName, [{keypos, #p_dr_world_rank.rank}, set, public, named_table])
                    end
        end, lists:seq(1, 8)),
    % 初始化本地排名数据
    lists:foreach(fun(T)->
                    L = db_sql:get_dojangrank_rank(T),
                    ets:insert(list_to_atom(lists:concat(['ets_dojangrank_',T])),L)
            end,lists:seq(1,8)),
%% 保险起见，还是不要在IsNeedInit 为true时，初始化ets了，正常情况不会出现这种异常，遇到异常手动处理
%%     IsNeedInit = lists:any(fun(RankType)-> 0 =:= ets:info(list_to_atom(lists:concat(['ets_dojangrank_',RankType])),size) end, lists:seq(1, 8)),
    case db_sql:get_etc(?DB_ETC_KEY_DOJANGRANK) of
%%         [State0] when false =:= IsNeedInit ->
        [State0] ->
            State = 
                case State0 of
                    {state,RewardDate}->
                        #state{reward_date = RewardDate};
                    _ when erlang:is_record(State0, state)->
                        State0;
                    _ ->
                        ?ERR("init_process_data ~w",[State0]),
                        #state{}
                end,
            case State of
                {state,RewardDate1} ->
                    {state,RewardDate1,0,{0,0}};
                {state,RewardDate2,CurrentWorldSession,_UpdateWorldDate} ->
                    {state,RewardDate2,CurrentWorldSession,{0,0}}       %% 设定为0是因为本地缓存的ets并没有保存
            end;
        X -> %% 需要填充机器人
            ?ERR("init_process_data ~w",[X]),
            lists:foreach(fun(T)-> init_robot(T) end,lists:seq(1,8)),
            #state{}
    end.

init_robot(RankType)->
    case ets:info(list_to_atom(lists:concat(['ets_dojangrank_',RankType])),size) of
        0 ->
            %% RobotConfig = [{{1,1000},{2,[4,5],1,0}}],
            RobotConfig = data_dojangrank:get(robot_init),
            AllGer = [data_ger:get(ID) || ID<-data_ger:get_list(), ID < 20000],
            RankTypeGerList = lists:filter(fun(GerCfg)-> 
                                                   GerCfg#data_ger.gerProperty =:= RankType 
                                           end, AllGer),
            ?INFO("init_robot(~w) >>> ~w",[RankType,[{E#data_ger.gerTypeID,E#data_ger.gerProperty,E#data_ger.gerStar}||E<-RankTypeGerList]]),
            init_robot2(RankType,RobotConfig,RankTypeGerList);
        Err ->
            ?ERR("init_robot error(~w) size:~w  ets表里有数据，停止robot初始化",[RankType,Err]),
            ignore
    end.

init_robot2(_,[],_)->
    ignore;
init_robot2(RankType,[{{Rank1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality}}|Other],AllGer)->
    ItemDict = {[],[],[],[],[],[]},    %% no used
    EquipLevel = 1,                    %% no used
    init_robot3(RankType,{Rank1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality},AllGer,EquipLevel,ItemDict),
    init_robot2(RankType,Other,AllGer).

init_robot3(_,{Rank1,Rank2},_,_,_,_) when Rank1 - 1 >= Rank2->
    ignore;
init_robot3(RankType,{Rank1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality},AllGer,EquipLevel,ItemDict)->
    RoleID = gen_account:gen_roleID(Rank1*8+RankType-1),
%%     %% 针对专服2区，机器人roleid比正常少了1000000，原因不可考证。加入以下代码解决问题
%%     RoleID = case role_lib:get_rolePublic(RoleID0) of
%%                  [] ->
%%                      RoleID0 - 1000000;
%%                  _ ->
%%                      RoleID0
%%              end,
    GerDict = [E||#data_ger{gerStar=GerStar, gerTypeID=GerTypeID}=E<-AllGer, lists:member(GerStar, GerStarList), GerTypeID > 1000, GerTypeID < 10000, GerTypeID rem 10=:=0],
    ?INFO("init_robot3(~w)",[{Rank1,Rank2}]),
%%     ?INFO("init_robot3(~w)",[{[{E#data_ger.gerTypeID,E#data_ger.gerProperty,E#data_ger.gerStar}||E<-GerDict],GerNum,GerLevel,EquipLevel,ItemDict,GerQuality}]),
    FighterList = gen_account:random_ger_list(GerDict,min(erlang:length(GerDict),GerNum),GerLevel,EquipLevel,ItemDict,GerQuality),
    FightPower = lists:sum([E#ger.gerAttr#gerAttr.gerFightPower||E<-FighterList]),
    SelectedGerType = 
        lists:nth(1
                 ,[E#ger.gerBase#gerBase.gerTypeID*256 + E#ger.gerBase#gerBase.gerQuality||E<-FighterList]),
    DojangRank = #dojangrank{rank = Rank1
                            ,role_id = RoleID
                            ,selected_ger_type = SelectedGerType
                            ,fighter_array_cfg = FighterList
                            ,fighter_power = FightPower},
    ets:insert(list_to_atom(lists:concat(['ets_dojangrank_',RankType])),DojangRank),
    init_robot3(RankType,{Rank1+1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality},AllGer,EquipLevel,ItemDict).

get_ranklist_sub(RankType,RoleID)->
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    Rank = case ets:match_object(EtsName, #dojangrank{role_id=RoleID, _='_'}) of
               [] ->
                   1001;
               [SelfDojangRank] ->
                   SelfDojangRank#dojangrank.rank
           end,
    SubRankList0 = get_ranklist_sub1(RankType,Rank,[]),
    Max = max(min(?show_rank_all_num,Rank),10),
    SubRankList = get_ranklist_sub12(SubRankList0,lists:seq(1,?show_rank_all_num+?show_rank_rule_num+1),Max),
    ?INFO("get_ranklist_sub (~w) ~w",[SubRankList,RoleID]),
    lists:map(fun(R)->
%%                       if
%%                           R =:= 1001 ->
%%                               to_p_dojangrank_rank(RoleID);
%%                           true ->
                              [DojangRank] = ets:lookup(EtsName,R),
                              to_p_dojangrank_rank(DojangRank)
%%                       end
              end,lists:sort(SubRankList)).

get_ranklist_sub1(RankType,Rank,AccList) when Rank > 1->
    NextRank =
        if
            1 < Rank andalso Rank =< 50 ->
                Rank-1;
            51 =< Rank andalso Rank =< 100 ->
                Rank-util:random_int(1, 2);
            101 =< Rank andalso Rank =< 200 ->
                Rank-util:random_int(1, 5);
            201 =< Rank andalso Rank =< 400 ->
                Rank-util:random_int(1, 10);
            401 =< Rank andalso Rank =< 600 ->
                Rank-util:random_int(1, 15);
            601 =< Rank ->
                Rank-util:random_int(1, 20)
        end,
    NewAccList = [NextRank|AccList],
    case erlang:length(NewAccList) of
        ?show_rank_rule_num ->
            NewAccList;
        _ ->
            get_ranklist_sub1(RankType,NextRank,NewAccList)
    end;
get_ranklist_sub1(_RankType,_Rank,AccList)->
    AccList.

get_ranklist_sub12(SubRankList0,[],_Max)->
    SubRankList0;
get_ranklist_sub12(SubRankList0,[H|T],Max)->
    case lists:member(H,SubRankList0) of
        true ->
            get_ranklist_sub12(SubRankList0,T,Max);
        false ->
            NewList = [H|SubRankList0],
            case erlang:length(NewList) of
                Max ->
                    NewList;
                _ ->
                    get_ranklist_sub12([H|SubRankList0],T,Max)
            end
    end.

search_top_replay(_ReplayUID,[])->
    [];
search_top_replay(ReplayUID,[H|OtherLocalGroup])->
    case lists:keysearch(ReplayUID, 1, H) of
        {value,{_,_,_,Replay}} ->
            [Replay];
        false ->
            search_top_replay(ReplayUID,OtherLocalGroup)
    end.

to_p_dojangrank_rank(Role,RankType,SelectedGerTypeList) when erlang:is_record(Role, role) andalso erlang:is_list(SelectedGerTypeList)->
    SelectedGerType = lists:nth(RankType, SelectedGerTypeList),
    #p_dojangrank_rank{roleID     = Role#role.roleID 
                      ,fightPower = Role#role.fightPower 
                      ,isMale     = Role#role.isMale 
                      ,title      = Role#role.title 
                      ,head       = Role#role.head 
                      ,level      = Role#role.level 
                      ,roleName   = Role#role.roleName 
                      ,vip        = Role#role.vipLevel 
                      ,rank       = 1001
                      ,selected_ger_type = SelectedGerType}.

to_p_dojangrank_rank(RoleID) when erlang:is_integer(RoleID)->
    Pub = role_lib:get_rolePublic(RoleID),
%%     {FighterList,_,_,_} = role_data:get_otherRoleFighter(RoleID),
%%     ?INFO("to_p_dojangrank_rank ~w ~w",[RoleID,erlang:length(FighterList)]),
%%     SelectedGerType = 
%%         lists:nth(1
%%                  ,[E#ger.gerBase#gerBase.gerTypeID*256 + E#ger.gerBase#gerBase.gerQuality||E<-FighterList]),
    #p_dojangrank_rank{roleID     = RoleID
                      ,fightPower = Pub#rolePublic.fightPower 
                      ,isMale     = Pub#rolePublic.isMale 
                      ,title      = Pub#rolePublic.title 
                      ,head       = Pub#rolePublic.head 
                      ,level      = Pub#rolePublic.level 
                      ,roleName   = Pub#rolePublic.roleName 
                      ,vip        = Pub#rolePublic.viplevel 
                      ,rank       = 1001
                      ,selected_ger_type = 0};
to_p_dojangrank_rank(DojangRank) when erlang:is_record(DojangRank, dojangrank)->
    Pub = role_lib:get_rolePublic(DojangRank#dojangrank.role_id),
    if
        Pub =:= [] ->
            ?ERR("to_p_dojangrank_rank ~w",[DojangRank]);
        true -> ignore
    end,
    #p_dojangrank_rank{roleID     = DojangRank#dojangrank.role_id
                      ,fightPower = DojangRank#dojangrank.fighter_power
                      ,isMale     = Pub#rolePublic.isMale 
                      ,title      = Pub#rolePublic.title 
                      ,head       = Pub#rolePublic.head 
                      ,level      = Pub#rolePublic.level 
                      ,roleName   = Pub#rolePublic.roleName 
                      ,vip        = Pub#rolePublic.viplevel 
                      ,rank       = DojangRank#dojangrank.rank
                      ,selected_ger_type = DojangRank#dojangrank.selected_ger_type}.
    
get_world_dojangrank(RoleID,RankType)->
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    case ets:match_object(EtsName, #p_dr_world_rank{roleID=RoleID, _='_'}) of
        [] ->
            Pub = role_lib:get_rolePublic(RoleID),
            #p_dr_world_rank{roleID = RoleID
                            ,fightPower = Pub#rolePublic.fightPower 
                            ,isMale = Pub#rolePublic.isMale 
                            ,title = Pub#rolePublic.title 
                            ,head = Pub#rolePublic.head 
                            ,level = Pub#rolePublic.level 
                            ,roleName = Pub#rolePublic.roleName 
                            ,vip = Pub#rolePublic.viplevel
                            ,rank = 0
                            ,selected_ger_type = 0
                            ,skinID = 0
                            ,serverID = data_setting:get(server_id)
                            ,ger_info_list = []     %% blank data
                            ,win_num = 0
                            ,lost_num = 0
                            ,total_num = 0
                            ,rank_score = 0};
        [PDrWorldRank|_] ->
            InitRobotNum = data_dojangrank:get(init_robot_num_max),
            if
                PDrWorldRank#p_dr_world_rank.rank > InitRobotNum ->
                    PDrWorldRank#p_dr_world_rank{rank = 0};
                true ->
                    PDrWorldRank
            end
    end.

get_world_sub_rank(RankType,Start,Length,Type) when Length >= 1 andalso (0 =:= Type orelse 2 =:= Type)->
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    End = min(Start + Length,InitRobotNum),
%%     Filter = ets:fun2ms(fun(R) when R#p_dr_world_rank.rank >= Start andalso R#p_dr_world_rank.rank =< End -> R end),
    Filter = 
        [{#p_dr_world_rank{roleID = '_',fightPower = '_',
                       isMale = '_',title = '_',head = '_',level = '_',
                       roleName = '_',vip = '_',rank = '_',selected_ger_type = '_',
                       skinID = '_',serverID = '_',ger_info_list = '_',
                       win_num = '_',lost_num = '_',total_num = '_',
                       rank_score = '_'},
              [{'andalso',{'and',{'orelse',true,fail},
                                 {'>=',{element,10,'$_'},Start}},
                          {'=<',{element,10,'$_'},End}}],
              ['$_']}],
    ets:select(EtsName, Filter);
get_world_sub_rank(RankType,_,_,1)->
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    ServerID = data_setting:get(server_id),
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
%%     Filter = ets:fun2ms(fun(R) when R#p_dr_world_rank.serverID =:= 1 -> R end),
    Filter = 
        [{#p_dr_world_rank{roleID = '_',fightPower = '_',
                   isMale = '_',title = '_',head = '_',level = '_',
                   roleName = '_',vip = '_',rank = '_',selected_ger_type = '_',
                   skinID = '_',serverID = '_',ger_info_list = '_',
                   win_num = '_',lost_num = '_',total_num = '_',
                   rank_score = '_'},
          [{'and',{'orelse',true,fail},{'=:=',{element,13,'$_'},ServerID}}],
          ['$_']}],
    lists:foldr(fun(PDrWorldRank,AccList)->
                    case tk_id:is_robot(PDrWorldRank#p_dr_world_rank.roleID) of
                        true ->
                            AccList;
                        false ->
                            if
                                PDrWorldRank#p_dr_world_rank.rank > InitRobotNum ->
                                    [PDrWorldRank#p_dr_world_rank{rank = 0}|AccList];
                                true ->
                                    [PDrWorldRank|AccList]
                            end
                    end
              end, [], ets:select(EtsName, Filter)).  

to_dr_ger_info(FighterList)->
    lists:map(fun(F)-> 
                    #dr_ger_info{ger_type = F#ger.gerBase#gerBase.gerTypeID
                                ,ger_quality = F#ger.gerBase#gerBase.gerQuality
                                ,ger_level = F#ger.gerBase#gerBase.gerLevel}
              end, FighterList).

clear_world_session_pd()->
    put(?pd_req_world_role_list,[]),
    put(?pd_last_req_world_timestamp,0),
    put(?pd_world_top_fightrec_remote,0),
    put(?pd_world_top_fightrec_local,{0,lists:duplicate(8, [])}),
    put(?pd_world_top_fightrec_req_list,[]),
    put(?pd_world_history_rank,{0,lists:duplicate(8, [])}),
    put(?pd_world_history_rank_req_list,[]).
    

%% ====================================================================
%% Debug functions
%% ====================================================================

set_rank(RoleID,RankType,NewRank)->
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    case ets:match_object(EtsName, #dojangrank{role_id=RoleID, _='_'}) of
       [] ->
            {FighterListA,_RoleLieuAddA,_TalentA,_TrSpecialA} = role_data:get_otherRoleFighter(RoleID),
            SelectedGerType = 
                lists:nth(1
                         ,[E#ger.gerBase#gerBase.gerTypeID*256 + E#ger.gerBase#gerBase.gerQuality||E<-FighterListA]),
            NewAtk = #dojangrank{role_id = RoleID
                                ,rank = NewRank
                                ,selected_ger_type  = SelectedGerType
                                ,fighter_array_cfg = FighterListA
                                ,fighter_power = 999999},
           ets:insert(EtsName,NewAtk);
       [_] ->
           ignore
    end.

check_rank_fighter()->
    lists:map(fun(T)->
            EtsName = list_to_atom(lists:concat(['ets_dojangrank_',T])),
            lists:filter(fun(R)-> 
                    R#dojangrank.fighter_array_cfg == []
                end, ets:tab2list(EtsName))
        end,lists:seq(1,8)).

check_world_server()->
    Res1 = 
        lists:map(fun(RankType)-> 
                EtsName = dojangrank_world_server:get_ets_name(RankType),
                [E||E<-ets:tab2list(EtsName),false =:= erlang:is_record(E, p_dr_world_rank)]
            end, lists:seq(1, 8)),
%%     Filter = ets:fun2ms(fun(R) -> element(2,R) end),
    Res2 = 
        lists:map(fun(RankType)-> 
                EtsName = dojangrank_world_server:get_ets_name(RankType),
                {RankType,lists:foldr(fun(RoleID,AccList)->
                        case ets:match_object(EtsName, #p_dr_world_rank{roleID=RoleID, _='_'}) of
                            [A1|[_A2|_]] = ErrList->
                                [{A1#p_dr_world_rank.serverID,A1#p_dr_world_rank.roleID,length(ErrList)}|AccList];
                            _ ->
                                AccList
                        end 
                    end, [], ets:select(EtsName,[{'$1',[],[{element,2,'$1'}]}]))}
            end, lists:seq(1, 8)),
    {Res1,Res2}.

clear_8ets()->
    lists:map(fun(RankType)-> 
            EtsName = dojangrank_world_server:get_ets_name(RankType),
            ets:delete_all_objects(EtsName)
        end,lists:seq(1,8)).

debug_get_from_ets(RankType,Index)->
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    ets:lookup(EtsName, Index).
    




