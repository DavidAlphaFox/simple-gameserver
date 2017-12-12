-module(doublematch_match).
-behaviour(gen_server).
-include("def_role.hrl").
-include("def_fight.hrl").
-include("def_doublematch.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {wait_s_q = []
               ,wait_d_q = []
               ,session = 0}). %% 这里的session 来自于rank进程

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    process_flag(trap_exit,true),
    gen_server:cast(doublematch_rank, sync_session),
    {ok, #state{session = 0
               ,wait_s_q = []
               ,wait_d_q = []}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast({sync_session,Session}, State) -> 
    ?INFO("doublematch_match sync_session ~w",[Session]),
    NewState = change_session(State,Session),
    {noreply, NewState};
handle_cast(_Msg, State) -> 
    ?WARNING("Unknown cast ~w",[_Msg]),
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% 请求匹配
% SignInfoList : [#dm_sign_info{}]
handle_info({dm_sign, SignInfoList}, State) ->
    case check_session(SignInfoList,State#state.session,State#state.wait_s_q,State#state.wait_d_q) of 
        true ->                                
            case check_match(SignInfoList,State#state.wait_s_q,State#state.wait_d_q) of
                {true,NewDoubleTeam,Enemy,{NewSingleQ,NewDoubleQ}} ->
                    dm_fight_process(State#state.session,NewDoubleTeam++Enemy);
                {false,{NewSingleQ,NewDoubleQ}} ->
                    ignore,
                    State
            end;
        false ->
            ?INFO("check_session fail ~w",[SignInfoList]),
		    % 返回匹配失败
            SI1 = lists:nth(1, SignInfoList),
            send_msg:direct(SI1#dm_sign_info.server_id
                           ,doublematch_server
                           ,{dm_cancel_response,[SI#dm_sign_info.role_id||SI<-SignInfoList]}),
			% 不变更匹配队列
            NewSingleQ = State#state.wait_s_q,
            NewDoubleQ = State#state.wait_d_q
    end,
    {noreply, State#state{wait_s_q=NewSingleQ,wait_d_q=NewDoubleQ}};
% 取消匹配
handle_info({dm_cancel ,RoleIDList,ServerID}, State) ->
    {NewSq,NewDq} = remove_sign(RoleIDList,State#state.wait_s_q,State#state.wait_d_q),
    NewState = State#state{wait_s_q = NewSq
                          ,wait_d_q = NewDq},
    send_msg:direct(ServerID
                   ,doublematch_server
                   ,{dm_cancel_response,RoleIDList}),
    {noreply, NewState};
handle_info(show_sign_list, State) ->
    S = [{R#dm_sign_info.role_id,R#dm_sign_info.dm_data#dm_data.score}||R<-State#state.wait_s_q],
    D = [{A#dm_sign_info.role_id,A#dm_sign_info.dm_data#dm_data.score
         ,B#dm_sign_info.role_id,B#dm_sign_info.dm_data#dm_data.score}||[A,B]<-State#state.wait_d_q],
    ?INFO("show_sign_list:~n~w~n~w~n", [S,D]),
    {noreply, State};
% 切换赛季，取消所有报名,暂时没用这个接口
handle_info({session_change,NewSession}, State) ->
    NewState = change_session(State,NewSession),
    {noreply, NewState};
handle_info(test_crash, State) ->
    A = 1,
    B = 2,
    A = B,
    {noreply, State};
handle_info(_Info, State) ->
    ?WARNING("Unknown info ~w",[_Info]),
    {noreply, State}.

%%%===================================================================
%%% Internal functions 
%%%===================================================================

change_session(State,NewSession) when State#state.session =:= NewSession->
    State;
change_session(State,NewSession) when State#state.session /= NewSession->
    lists:foreach(fun(SI)-> 
            send_msg:direct(SI#dm_sign_info.server_id
                           ,doublematch_server
                           ,{dm_cancel_response,[SI#dm_sign_info.role_id]})
        end, State#state.wait_s_q),
    lists:foreach(fun([SI1,SI2])-> 
            send_msg:direct(SI1#dm_sign_info.server_id
                           ,doublematch_server
                           ,{dm_cancel_response,[SI1#dm_sign_info.role_id]}),
            send_msg:direct(SI2#dm_sign_info.server_id
                           ,doublematch_server
                           ,{dm_cancel_response,[SI2#dm_sign_info.role_id]})
        end, State#state.wait_d_q),
    State#state{session=NewSession
               ,wait_s_q = []
               ,wait_d_q = []}.

%% 这个方法现在不止检查赛季，还要检查是否重复报名，防止游戏服某种原因，导致发送重复请求到匹配服
check_session(SignInfoList,CurSession,SingleList,DoubleList)->
    AlreadySignList = 
        [R#dm_sign_info.role_id||R<-SingleList] 
          ++ lists:foldl(fun([A,B],AccList)-> [A#dm_sign_info.role_id|[B#dm_sign_info.role_id|AccList]] end, [], DoubleList),
    lists:all(fun(SI) -> 
                      IsSigned = lists:member(SI#dm_sign_info.role_id, AlreadySignList),
                      SI#dm_sign_info.dm_data#dm_data.session =:= CurSession andalso false == IsSigned
              end, SignInfoList).

% 单人报名先匹配队友，得到队友后等同于双人报名的匹配流程
% 四种返回
check_match(SignInfoList,OldSingleQ,OldDoubleQ) ->
    case SignInfoList of
        [SignInfoSingle] ->
            ?INFO("check_friendly success ~w~n",[SignInfoSingle#dm_sign_info.dm_data#dm_data.score]),
            case check_friendly(SignInfoSingle,OldSingleQ) of
                {true,NewDoubleTeam,NewSingleQ} ->
                    ?INFO("check_friendly success ~w~n",[[E#dm_sign_info.dm_data#dm_data.score||E<-NewDoubleTeam]]),
                    case check_enemy(NewDoubleTeam,OldDoubleQ) of
                        {true,Enemy,NewDoubleQ} ->
                            {true,NewDoubleTeam,Enemy,{NewSingleQ,NewDoubleQ}};   %单人匹配，找到了队友和对手，NewSingleQ,NewDoubleQ
                        {false,NewDoubleQ} ->
                            {false,{NewSingleQ,NewDoubleQ}}         %单人匹配，找到了队友，没找到对手，NewSingleQ,NewDoubleQ
                    end;
                {false,NewSingleQ} ->
                    {false,{NewSingleQ,OldDoubleQ}}                 %单人匹配，队友和对手都没找到，NewSingleQ,OldDoubleQ
            end;
        [_SignInfoLeader|_]  = NewDoubleTeam ->
            case check_enemy(NewDoubleTeam,OldDoubleQ) of
                {true,Enemy,NewDoubleQ} ->
                    {true,NewDoubleTeam,Enemy,{OldSingleQ,NewDoubleQ}};           %双人匹配，找到了对手，OldSingleQ,NewDoubleQ
                {false,NewDoubleQ} ->
                    {false,{OldSingleQ,NewDoubleQ}}                 %双人匹配，没到对手，OldSingleQ,NewDoubleQ
            end
    end.

% SignInfoSingle : #dm_sign_info{}
check_friendly(SignInfoSingle,OldSingleQ)->
    ScoreMin = erlang:max(0, SignInfoSingle#dm_sign_info.dm_data#dm_data.score - data_doublematch:get(match_step_score)),
    ScoreMax = SignInfoSingle#dm_sign_info.dm_data#dm_data.score + data_doublematch:get(match_step_score),
    % reverse是为了放置，队列中符合积分的人太多，
    % filter_friendly 不会筛选出所有符合的，当筛选结果，差不多的时候，就不再筛选了
    ?INFO("check_friendly ~w",[SignInfoSingle#dm_sign_info.dm_data#dm_data.rank]),
    {Num,ResList,OtherList}=filter_friendly(SignInfoSingle#dm_sign_info.dm_data#dm_data.rank,ScoreMin,ScoreMax,0,[],[], lists:reverse(OldSingleQ),data_doublematch:get(match_step_time)),
    if
        Num =:= 0 ->
            {false,[SignInfoSingle|OldSingleQ]};
        true ->
            Friend = util:random_one_from_list(ResList),
            Rest = ResList -- [Friend],
            NewTeam = util:random_list([SignInfoSingle,Friend]),
            {true,NewTeam,Rest++OtherList}
    end.

% 并不是遇到第一个符合条件的就立即认定匹配，而是从符合条件的人中进行筛选，暂定最多选出4个符合条件的进行随机
filter_friendly(_TeamMaxRank,_ScoreMin,_ScoreMax,Num,ResAccList,OtherAccList,_OldSingleQ,_) when Num =:= 4 -> % 匹配的数量够了，4个中随机一个
    {Num,ResAccList,OtherAccList};
filter_friendly(_TeamMaxRank,_ScoreMin,_coreMax,Num,ResAccList,OtherAccList,[],1)->   % 范围已经扩大到了最大
    {Num,ResAccList,OtherAccList};
filter_friendly(TeamMaxRank,ScoreMin,ScoreMax,Num,ResAccList,OtherAccList,[],MatchLevel)-> % MatchLevel > 0 %一轮匹配完，未能得到4个符合条件的结果
    if
        Num >= 1 ->
            {Num,ResAccList,OtherAccList};
        true ->
            OldSingleQ = ResAccList++OtherAccList,
            NewScoreMin = erlang:max(0, ScoreMin - data_doublematch:get(match_step_score)),
            NewScoreMax = ScoreMax + data_doublematch:get(match_step_score),
            filter_friendly(TeamMaxRank,NewScoreMin,NewScoreMax,0,[],[],OldSingleQ,MatchLevel-1)
    end;
filter_friendly(TeamMaxRank,ScoreMin,ScoreMax,OldNum,ResAccList,OtherAccList,[SignInfo|OtherOldSingleQ],MatchLevel)->
    if
        SignInfo#dm_sign_info.dm_data#dm_data.score >= ScoreMin
          andalso SignInfo#dm_sign_info.dm_data#dm_data.score =< ScoreMax ->
            RankA = doublematch_server:check_rank_by_sroce(SignInfo#dm_sign_info.dm_data#dm_data.score),
            RankB = TeamMaxRank,
            ?INFO("filter_friendly s:~w-~w(~w) r:~w-~w",[ScoreMin,ScoreMax,SignInfo#dm_sign_info.dm_data#dm_data.score
                                                        ,RankA,RankB]),
            case doublematch_server:check_rank_for_match(RankA,RankB) of
                true ->
                    filter_friendly(TeamMaxRank,ScoreMin,ScoreMax,OldNum+1,[SignInfo|ResAccList],OtherAccList,OtherOldSingleQ,MatchLevel);
                false ->
                    filter_friendly(TeamMaxRank,ScoreMin,ScoreMax,OldNum,ResAccList,[SignInfo|OtherAccList],OtherOldSingleQ,MatchLevel)
            end;
        true ->
            filter_friendly(TeamMaxRank,ScoreMin,ScoreMax,OldNum,ResAccList,[SignInfo|OtherAccList],OtherOldSingleQ,MatchLevel)
    end.

check_enemy([Leader,Follow]=NewTeam,OldDoubleQ)->
    %以队伍中最大分数作为基准
    TeamMaxScore = erlang:max(Leader#dm_sign_info.dm_data#dm_data.score, Follow#dm_sign_info.dm_data#dm_data.score),
    TeamMaxRank = erlang:max(Leader#dm_sign_info.dm_data#dm_data.rank, Follow#dm_sign_info.dm_data#dm_data.rank),
    ScoreMin = erlang:max(0, TeamMaxScore - data_doublematch:get(match_step_score)),
    ScoreMax = TeamMaxScore + data_doublematch:get(match_step_score),
    % reverse是为了放置，队列中符合积分的人太多，
    % filter_friendly 不会筛选出所有符合的，当筛选结果，差不多的时候，就不再筛选了
    {Num,ResList,OtherList}=filter_enemy(TeamMaxRank,ScoreMin,ScoreMax,0,[],[], lists:reverse(OldDoubleQ),data_doublematch:get(match_step_time)),
    if
        Num =:= 0 ->
            {false,[NewTeam|OldDoubleQ]};
        true ->
            Enemy = util:random_one_from_list(ResList),
            Rest = ResList -- [Enemy],
            {true,Enemy,Rest++OtherList}
    end.

filter_enemy(_TeamMaxRank,_ScoreMin,_ScoreMax,Num,ResAccList,OtherAccList,_OldDoubleQ,_) when Num =:= 4 -> % 匹配的数量够了，4个中随机一个
    {Num,ResAccList,OtherAccList};
filter_enemy(_TeamMaxRank,_ScoreMin,_ScoreMax,Num,ResAccList,OtherAccList,[],1)->   % 范围已经扩大到了最大
    {Num,ResAccList,OtherAccList};
filter_enemy(TeamMaxRank,ScoreMin,ScoreMax,Num,ResAccList,OtherAccList,[],MatchLevel)-> % MatchLevel > 0 %一轮匹配完，未能得到4个符合条件的结果
    if
        Num >= 1 ->
            {Num,ResAccList,OtherAccList};
        true ->
            OldDoubleQ = ResAccList++OtherAccList,
            ?INFO("filter_enemy improve match space ~w",[OldDoubleQ]),
            NewScoreMin = erlang:max(0, ScoreMin - data_doublematch:get(match_step_score)),
            NewScoreMax = ScoreMax + data_doublematch:get(match_step_score),
            filter_enemy(TeamMaxRank,NewScoreMin,NewScoreMax,0,[],[],OldDoubleQ,MatchLevel-1)
    end;
filter_enemy(TeamMaxRank,ScoreMin,ScoreMax,OldNum,ResAccList,OtherAccList,[TeamInfo|OtherOldDoubleQ],MatchLevel)->
    [Leader,Follow]=TeamInfo,
    %以队伍中最大分数作为基准
    TeamMaxScore = erlang:max(Leader#dm_sign_info.dm_data#dm_data.score, Follow#dm_sign_info.dm_data#dm_data.score),
    if
        TeamMaxScore >= ScoreMin
          andalso TeamMaxScore =< ScoreMax ->
            RankA = doublematch_server:check_rank_by_sroce(TeamMaxScore),
            RankB = TeamMaxRank,
            ?INFO("filter_enemy s:~w-~w(~w) r:~w-~w",[ScoreMin,ScoreMax,TeamMaxScore
                                                     ,RankA,RankB]),
            case doublematch_server:check_rank_for_match(RankA,RankB) of
                true ->
                    filter_enemy(TeamMaxRank,ScoreMin,ScoreMax,OldNum+1,[TeamInfo|ResAccList],OtherAccList,OtherOldDoubleQ,MatchLevel);
                false ->
                    filter_enemy(TeamMaxRank,ScoreMin,ScoreMax,OldNum,ResAccList,[TeamInfo|OtherAccList],OtherOldDoubleQ,MatchLevel)
            end;
        true ->
            filter_enemy(TeamMaxRank,ScoreMin,ScoreMax,OldNum,ResAccList,[TeamInfo|OtherAccList],OtherOldDoubleQ,MatchLevel)
    end.

random_first_atk()->
    AtkRandInt = util:random_int(1, 6),
    DefRandInt = util:random_int(1, 6),
    if
        AtkRandInt =:= DefRandInt ->
            random_first_atk();
        true ->
            {AtkRandInt, DefRandInt}
    end.

%异步战斗
dm_fight_process(Session,TeamList)->
    ?INFO("dm_fight_process start ~w",[TeamList]),
    FightList = [E#dm_sign_info.double_team||E<-TeamList],
    [Team1Leader,Team1Fellow,Team2Leader,Team2Fellow] = FightList,
    [AtkLeader,AtkFollow,DefLeader,DefFollow] = [E#dm_sign_info.role_id||E<-TeamList],
    AtkIdList = [AtkLeader,AtkFollow],
    DefIdList = [DefLeader,DefFollow],
    {AtkRandInt, DefRandInt} = random_first_atk(),
    Ref = erlang:make_ref(),%%防止收到其他来自其他战斗的战斗结果,加上REF后,确保后面取到的消息是该次战斗请求的结果
    double_fight:new(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,self(),Ref,AtkRandInt < DefRandInt),
    get_dm_result(Ref,Session,TeamList,AtkIdList,DefIdList,AtkRandInt,DefRandInt).
    
% 前面的是进攻方
get_dm_result(Ref,Session,TeamList,AtkIdList,_DefIdList,AtkRandInt,DefRandInt) ->
    receive
        {fight_result,Ref,FightResult} ->
            {IsWin,FightRecord,{SrcGerStateList,DGerStateList,SrcGerList,DGerList,_,_}} = FightResult,
            {AtkDeadNum,DefDeadNum} = if
                              AtkRandInt > DefRandInt ->
                                  {erlang:length(lists:filter(fun({_,Hp,_})-> Hp =:= 0 end, SrcGerStateList))
                                  ,erlang:length(lists:filter(fun({_,Hp,_})-> Hp =:= 0 end, DGerStateList))};
                              true ->
                                  {erlang:length(lists:filter(fun({_,Hp,_})-> Hp =:= 0 end, DGerStateList))
                                  ,erlang:length(lists:filter(fun({_,Hp,_})-> Hp =:= 0 end, SrcGerStateList))}
                          end,    
            [AtkTeamLeader,_,DefTeamLeader,_] = TeamList,
            %DEBUG
            ?INFO("DM fight_result ~w-~w ~n~w~n(~w) ~w~n(~w) ~w~n",[AtkRandInt,DefRandInt,IsWin
                                                             ,erlang:length(lists:filter(fun({_,Hp,_})-> Hp > 0 end, SrcGerStateList))
                                                             ,SrcGerStateList
                                                             ,erlang:length(lists:filter(fun({_,Hp,_})-> Hp > 0 end, DGerStateList))
                                                             ,DGerStateList]),
            ?INFO("get_dm_result fight_result ~w", [FightResult]),
            Now = util:now(),
            RankTop = 6,
            {CheckRankList,PUnit} = lists:foldr(fun(SignInfo,{AccList,PAccL})-> 
                        IsAtk = lists:member(SignInfo#dm_sign_info.role_id, AtkIdList),
                        NewSroce = calc_check_sorce(SignInfo,IsWin,IsAtk,AtkDeadNum,DefDeadNum),
                        ?INFO("get_dm_result IsAtk:~w (~w:~w) s:~w->~w top:~w~n",[IsAtk,{SignInfo#dm_sign_info.server_id,SignInfo#dm_sign_info.role_id},IsWin
                                                                           ,SignInfo#dm_sign_info.dm_data#dm_data.score,NewSroce,RankTop]),
                        {[{SignInfo,NewSroce}|AccList]
                        ,[sign_info_to_punit(SignInfo,NewSroce - SignInfo#dm_sign_info.dm_data#dm_data.score)|PAccL]}
                end, {[],[]}, TeamList),
            lists:foreach(fun({SignInfo,NewSroce})-> 
                            IsAtk = lists:member(SignInfo#dm_sign_info.role_id, AtkIdList),
                            % 这里这样是为了兼容游戏服文件替换时，参数意义不变，而特意转换的，260版本可以统一，没有反复转正负
                            NewSroceOnMsg = if IsAtk =:= true -> 
                                                   NewSroce - SignInfo#dm_sign_info.dm_data#dm_data.score; 
                                               true -> 
                                                   SignInfo#dm_sign_info.dm_data#dm_data.score - NewSroce 
                                            end,  
                            send_msg:direct(SignInfo#dm_sign_info.server_id
                                           ,doublematch_server
                                           ,{fight_rec,SignInfo#dm_sign_info.role_id
                                            ,IsAtk,IsWin,FightRecord
                                            ,NewSroceOnMsg,AtkRandInt,DefRandInt
                                            ,PUnit,Now})
                          end, CheckRankList),
            %% 筛选出，需要重新排名的数据
            CheckRankList2 = lists:filter(fun({SignInfo,NewSroce})-> 
                            % 分辨进攻方
                            % 计算段位
                            OldRank = doublematch_server:check_rank_by_sroce(SignInfo#dm_sign_info.dm_data#dm_data.score),
                            NewRank = doublematch_server:check_rank_by_sroce(NewSroce),
                            NewRank >= RankTop 
                              orelse OldRank >= RankTop 
                              orelse SignInfo#dm_sign_info.dm_data#dm_data.rank >= RankTop
                end, CheckRankList),
            if
                CheckRankList2 /= [] ->
                    DeadNumRec = if IsWin =:= true -> AtkDeadNum ; true -> DefDeadNum end,
                    erlang:send(doublematch_rank,{update_rank,Session,Now,PUnit,FightRecord,AtkRandInt,DefRandInt,CheckRankList2,DeadNumRec});
                true ->
                    ignore
            end
    after 10000 ->
        ?INFO("get_dm_result timeout") %内网暂时不能出现这种情况,出现的话，可能是战斗报错
    end.

remove_sign([],OldSQ,OldDQ)->
    {OldSQ,OldDQ};
remove_sign([RoleID|OtherRoleIDList],OldSQ,OldDQ) ->
    case lists:keytake(RoleID, #dm_sign_info.role_id, OldSQ) of
        {value,_,NewSQ}->
            remove_sign(OtherRoleIDList,NewSQ,OldDQ);
        false ->
            {NewSQ,NewDQ}=remove_sign2_double(RoleID,OldSQ,[],OldDQ),
            remove_sign(OtherRoleIDList,NewSQ,NewDQ)
    end.

remove_sign2_double(RoleID,OldSQ,RestDQ,[])->
    {OldSQ,RestDQ};
remove_sign2_double(RoleID,OldSQ,RestDQ,[[L,F]|OtherDQ])->
    if
        RoleID =:= L#dm_sign_info.role_id ->
            {[F|OldSQ],OtherDQ};
        RoleID =:= F#dm_sign_info.role_id ->
            {[L|OldSQ],OtherDQ};
        true ->
            remove_sign2_double(RoleID,OldSQ,[[L,F]|RestDQ],OtherDQ)
    end.
    
calc_check_sorce(SignInfo,IsWin,IsAtk,AtkDeadNum,DefDeadNum)->
    {_RScore,GRank,_GLevel,_,_} = data_doublematch:get({rank,SignInfo#dm_sign_info.dm_data#dm_data.rank}),
    ChangeScore = if IsWin =:= true -> AtkDeadNum div 3; true -> DefDeadNum  div 3 end,
    NewSroce0 = if
        (IsAtk =:= true andalso IsWin =:= true)
          orelse (IsAtk =:= false andalso IsWin =:= false) ->
            %Win
            {WinScore,_} = data_doublematch:get({cacl_score,GRank}),
            SignInfo#dm_sign_info.dm_data#dm_data.score + WinScore - ChangeScore;
        true ->
            %Lose
            {_,LoseScore} = data_doublematch:get({cacl_score,GRank}),
            SignInfo#dm_sign_info.dm_data#dm_data.score - LoseScore + ChangeScore
    end,
    ?INFO("calc_check_sorce ~w",[{IsWin,IsAtk,AtkDeadNum,DefDeadNum,ChangeScore,SignInfo#dm_sign_info.role_id,SignInfo}]),
    erlang:max(0, NewSroce0).

% 这里是战斗前统计四个参加者的数据，打完，还要在rank进程重新刷新排名grade,gradeLevel,rank
sign_info_to_punit(SignInfo,ScoreChange)->
    #dm_sign_info{role_id=RoleID,server_id=ServerID,name=Name,level=Level,isMale=IsMale,title=Title,head=Head,dm_data=DmData,vip=Vip}=SignInfo,
    #dm_data{index=Index,rank=Rank,score=Score} = DmData,
    {_,GRank,GLevel,_,_} = data_doublematch:get({rank,Rank}),
    RankTop = data_doublematch:get(rank_top),
    RankScore = if
                    DmData#dm_data.rank >= RankTop ->
                        {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
                        Score - TopScore;
                    true ->
                        Score rem 100
                end,
    #p_doublematch_rank_unit{roleID=RoleID,roleName=Name,isMale=IsMale,title=Title,head=Head,level=Level,serverID=ServerID
                            ,grade=GRank, gradeLevel=GLevel,score=RankScore,rank=Index,score_change=ScoreChange,vip=Vip}.

%% ------------------------------------------------------------------
%% test Function Definitions
%% ------------------------------------------------------------------

test_check_match(Time)->
    random:seed(util:gen_random_seed()),
    test_check_match2(Time).
    
test_check_match2(0)->
    ignore;             
test_check_match2(Time)->
    SignInfoList = 
        case util:random_int(1, 2) of
            1 ->
                [random_single()];
            2 ->
                random_double()
        end,
    OldSingleQ = [random_single()||_<-lists:seq(1, util:random_int(1, 6))],
    OldDoubleQ = [random_double()||_<-lists:seq(1, util:random_int(1, 6))],
    W = lists:nth(1, SignInfoList),
    case check_match(SignInfoList,OldSingleQ,OldDoubleQ) of
        {true,T1,T2,{NewSingleQ,NewDoubleQ}} ->
            %?INFO("test_check_match ~w~n",[NewSingleQ]),
            [X|_]=T1,
            [Y|_]=T2,
            ?INFO("test_check_match --Time-- (~w) ~w,~w ++++ ~w,~w~n"
                     , [W#dm_sign_info.dm_data#dm_data.score
                       ,X#dm_sign_info.dm_data#dm_data.score
                       ,Y#dm_sign_info.dm_data#dm_data.score
                       ,test_show_sorce1(NewSingleQ),test_show_sorce2(NewDoubleQ)]),
            dm_fight_process(doublematch_server:get_session(),T1++T2);
        {false,{NewSingleQ,NewDoubleQ}} ->
            %?INFO("test_check_match ~w~n",[NewSingleQ]),
            ?INFO("test_check_match --Time-- (~w) ~w,~w~n"
                     , [W#dm_sign_info.dm_data#dm_data.score
                       ,test_show_sorce1(NewSingleQ),test_show_sorce2(NewDoubleQ)])
    end,
    test_check_match2(Time-1).

random_single()->
    Fighters =  [{ger,2000000004166,{gerBase,7220,20,100,4,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004109,{gerBase,7010,20,100,1,0,[]},{gerAttr,2403837,2500,10000,2000,203998,1882210,100,100,10,15,15,10,0,0,15,15,36,0,1,0,80852,2403837},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1882210,100,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004121,{gerBase,7050,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
                ,{ger,2000000004127,{gerBase,7070,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
    Talent = [{12000,40},{13000,40},{22000,30},{23000,30},{21000,15},{24000,15},{32000,10},{31000,10},{33000,10},{34000,10},{41000,20},{43000,25},{52000,40},{53000,40},{51000,25},{61000,20},{62000,15},{54000,35},{64000,20},{71000,32},{72000,15},{11000,1},{73000,15},{74000,31},{93000,51},{92000,50},{81000,5},{94000,7},{91000,9},{63000,20},{42000,14}],
    Spe = {trSpecial,10001,1001,251,0,0},
    Skin = {skin_info,[],0},
    Lieu = {10,10},    
    T1 = #double_team{skin_info=Skin,talent=Talent,trainer=Spe,fighters=Fighters,lieu=Lieu},
    #dm_sign_info{role_id = 6511905 + util:random_int(1, 99999)
                  ,server_id = 5
                  ,double_team = T1
                  ,name = "sample_data"
                  ,level = 99
                  ,isMale=true       %% 用于显示头像的性别信息
                  ,title = 0         %% 用于显示头像
                  ,head = 0          %% 用于显示头像
                  ,dm_data = #dm_data{score = util:random_int(1001, 2000)}}.
    
random_double()->
    [random_single(),random_single()].
    
test_show_sorce1(NewSingleQ)->
    [E#dm_sign_info.dm_data#dm_data.score||E<-NewSingleQ].
test_show_sorce2(NewDoubleQ)->
    [E#dm_sign_info.dm_data#dm_data.score||[E|_]<-NewDoubleQ].
    

sample_res1()->
    {sc_fight_double_request
       ,[{p_fighter,42953967928297,0,1,0,0,0,0,0,0,0,0,[],0}
        ,{p_fighter,42953967928297,0,2,0,0,0,0,0,0,0,0,[],0}
        ,{p_fighter,42953967928297,0,-1,0,0,0,0,0,0,0,0,[],0}
        ,{p_fighter,42953967928297,0,-2,0,0,0,0,0,0,0,0,[],0}
        ,{p_fighter,2000000004166,7220,4,27418360,27418360,2543493,2543493,0,100,20,0,[],0}
        ,{p_fighter,2000000004109,7010,1,22295995,22295995,2403837,2403837,100,100,20,0,[],0}
        ,{p_fighter,2000000004121,7050,3,25134693,25134693,2465117,2465117,100,200,20,0,[],0}
        ,{p_fighter,2000000004127,7070,2,22907685,22907685,2428329,2428329,100,100,20,0,[],0}
        ,{p_fighter,2000000004166,7220,14,27418360,27418360,2543493,2543493,0,100,20,0,[],0}
        ,{p_fighter,2000000004109,7010,11,22295995,22295995,2403837,2403837,100,100,20,0,[],0}
        ,{p_fighter,2000000004121,7050,13,25134693,25134693,2465117,2465117,100,200,20,0,[],0}
        ,{p_fighter,2000000004127,7070,12,22907685,22907685,2428329,2428329,100,100,20,0,[],0}
        ,{p_fighter,2000000004166,7220,-4,27418360,27418360,2543493,2543493,0,100,20,0,[],0}
        ,{p_fighter,2000000004109,7010,-1,22295995,22295995,2403837,2403837,100,100,20,0,[],0}
        ,{p_fighter,2000000004121,7050,-3,25134693,25134693,2465117,2465117,100,200,20,0,[],0}
        ,{p_fighter,2000000004127,7070,-2,22907685,22907685,2428329,2428329,100,100,20,0,[],0}
        ,{p_fighter,2000000004166,7220,-14,27418360,27418360,2543493,2543493,0,100,20,0,[],0}
        ,{p_fighter,2000000004109,7010,-11,22295995,22295995,2403837,2403837,100,100,20,0,[],0}
        ,{p_fighter,2000000004121,7050,-13,25134693,25134693,2465117,2465117,100,200,20,0,[],0}
        ,{p_fighter,2000000004127,7070,-12,22907685,22907685,2428329,2428329,100,100,20,0,[],0}]
       ,[{p_action,12,101,[],0,4069137,0,2},{p_action,13,101,[],0,4464724,0,2},{p_action,14,101,[],0,4870377,0,2},{p_action,2,101,[],0,4069137,0,2},{p_action,3,101,[],0,4464724,0,2},{p_action,4,101,[],0,4386937,0,2},{p_action,-14,16,[-14],0,0,0,0},{p_action,-4,4,[],0,0,0,0},{p_action,-4,53,[],25,-36030350,0,80},{p_action,4,23,[-4],50,0,0,0},{p_action,12,101,[],0,4069137,0,2},{p_action,13,101,[],0,4464724,0,2},{p_action,14,101,[],0,4870377,0,2},{p_action,2,101,[],0,4069137,0,2},{p_action,3,101,[],0,4464724,0,2},{p_action,4,101,[],0,4386937,0,2},{p_action,-12,16,[-12],0,0,0,0},{p_action,-2,4,[],0,0,0,0},{p_action,-2,64,[],0,-51989008,0,80},{p_action,3,34,[-2],-100,0,0,0},{p_action,4,9,[],0,27418360,0,0},{p_action,-2,2,[],0,8133052,0,0},{p_action,4,53,[],25,-32532211,0,88},{p_action,-2,23,[4],50,0,0,0},{p_action,2,2,[],0,3535575,0,0},{p_action,-4,53,[],25,-12738516,0,8},{p_action,2,23,[-4],50,0,0,0},{p_action,-4,101,[],0,5483672,0,2},{p_action,-2,101,[],0,4581537,0,2},{p_action,-14,101,[],0,6087972,0,2},{p_action,-12,101,[],0,5086422,0,2},{p_action,4,101,[],0,6087972,0,2},{p_action,3,101,[],0,5580906,0,2},{p_action,2,101,[],0,5086422,0,2},{p_action,14,101,[],0,6087972,0,2},{p_action,13,101,[],0,5580906,0,2},{p_action,12,101,[],0,5086422,0,2},{p_action,-4,102,[],0,-16,0,2},{p_action,-2,102,[],0,-16,0,2},{p_action,-14,102,[],0,-16,0,2},{p_action,-12,102,[],0,-16,0,2},{p_action,4,102,[],0,-16,0,2},{p_action,3,102,[],0,-16,0,2},{p_action,2,102,[],0,-16,0,2},{p_action,14,102,[],0,-16,0,2},{p_action,13,102,[],0,-16,0,2},{p_action,12,102,[],0,-16,0,2},{p_action,4,103,[],0,50,0,2},{p_action,3,103,[],0,50,0,2},{p_action,2,103,[],0,50,0,2},{p_action,14,103,[],0,50,0,2},{p_action,13,103,[],0,50,0,2},{p_action,12,103,[],0,50,0,2},{p_action,-4,103,[],0,50,0,2},{p_action,-2,103,[],0,50,0,2},{p_action,-14,103,[],0,50,0,2},{p_action,-12,103,[],0,50,0,2},{p_action,-12,101,[],0,1608119,0,2},{p_action,-12,113,[],0,100,0,2},{p_action,-2,74,[-12],2,0,0,128},{p_action,3,101,[],0,1764455,0,2},{p_action,3,113,[],0,100,0,2},{p_action,2,74,[3],2,0,0,128},{p_action,-14,101,[],0,1924768,0,2},{p_action,-14,113,[],0,100,0,2},{p_action,-1,74,[-14],2,0,0,128},{p_action,13,101,[],0,1764455,0,2},{p_action,13,113,[],0,100,0,2},{p_action,1,74,[13],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,-12,101,[],0,4069137,0,2},{p_action,-14,101,[],0,4870377,0,2},{p_action,-2,101,[],0,3665229,0,2},{p_action,-4,101,[],0,4386937,0,2},{p_action,11,16,[11],0,0,0,0},{p_action,1,4,[],0,0,0,0},{p_action,1,53,[],0,-24110186,-2403837,80},{p_action,-4,23,[1],50,0,0,0},{p_action,-2,9,[],0,22907685,0,0},{p_action,-2,53,[],0,-30243469,0,80},{p_action,4,23,[-2],50,0,0,0},{p_action,-2,54,[],0,-21762210,-2428329,64},{p_action,3,24,[-2],50,0,0,0},{p_action,-2,2,[],0,7408127,0,0},{p_action,4,53,[],25,-26156665,-2543493,72},{p_action,-2,23,[4],50,0,0,0},{p_action,-4,9,[],0,27418360,0,0},{p_action,2,2,[],0,7837999,0,0},{p_action,-4,53,[],25,-27822052,-2543493,88},{p_action,2,23,[-4],50,0,0,0},{p_action,12,101,[],0,8138274,0,2},{p_action,13,101,[],0,4464724,0,2},{p_action,11,101,[],0,4157043,0,2},{p_action,14,101,[],0,4870377,0,2},{p_action,2,101,[],0,4069137,0,2},{p_action,3,101,[],0,4464724,0,2},{p_action,1,101,[],0,4157043,0,2},{p_action,4,101,[],0,4870377,0,2},{p_action,-11,16,[-11],0,0,0,0},{p_action,-1,4,[],0,0,0,0},{p_action,-13,16,[-13],0,0,0,0},{p_action,-3,4,[],0,0,0,0},{p_action,1,2,[],0,21562673,0,0},{p_action,-1,52,[],25,-46047035,-2403837,88},{p_action,-3,52,[],25,-28670975,-2465117,88},{p_action,1,22,[-3,-1],50,0,0,0},{p_action,3,72,[],0,0,0,0},{p_action,3,71,[],0,0,0,0},{p_action,-2,70,[3],2,0,0,128},{p_action,-13,72,[],0,0,0,0},{p_action,-13,71,[],0,0,0,0},{p_action,2,70,[-13],2,0,0,128},{p_action,2,72,[],0,0,0,0},{p_action,2,71,[],0,0,0,0},{p_action,-1,70,[2],2,0,0,128},{p_action,-1,72,[],0,0,0,0},{p_action,-1,71,[],0,0,0,0},{p_action,1,70,[-1],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,-4,101,[],0,6087972,0,2},{p_action,-1,101,[],0,5196304,0,2},{p_action,-3,101,[],0,5580906,0,2},{p_action,-2,101,[],0,5086422,0,2},{p_action,-14,101,[],0,6087972,0,2},{p_action,-11,101,[],0,5196304,0,2},{p_action,-13,101,[],0,5580906,0,2},{p_action,-12,101,[],0,5086422,0,2},{p_action,4,101,[],0,6087972,0,2},{p_action,1,101,[],0,5196304,0,2},{p_action,3,101,[],0,5580906,0,2},{p_action,2,101,[],0,5086422,0,2},{p_action,14,101,[],0,6087972,0,2},{p_action,11,101,[],0,5196304,0,2},{p_action,13,101,[],0,5580906,0,2},{p_action,12,101,[],0,5086422,0,2},{p_action,-4,102,[],0,-16,0,2},{p_action,-1,102,[],0,-16,0,2},{p_action,-3,102,[],0,-16,0,2},{p_action,-2,102,[],0,-16,0,2},{p_action,-14,102,[],0,-16,0,2},{p_action,-11,102,[],0,-16,0,2},{p_action,-13,102,[],0,-16,0,2},{p_action,-12,102,[],0,-16,0,2},{p_action,4,102,[],0,-16,0,2},{p_action,1,102,[],0,-16,0,2},{p_action,3,102,[],0,-16,0,2},{p_action,2,102,[],0,-16,0,2},{p_action,14,102,[],0,-16,0,2},{p_action,11,102,[],0,-16,0,2},{p_action,13,102,[],0,-16,0,2},{p_action,12,102,[],0,-16,0,2},{p_action,4,103,[],0,50,0,2},{p_action,1,103,[],0,50,0,2},{p_action,3,103,[],0,50,0,2},{p_action,2,103,[],0,50,0,2},{p_action,14,103,[],0,50,0,2},{p_action,11,103,[],0,50,0,2},{p_action,13,103,[],0,50,0,2},{p_action,12,103,[],0,50,0,2},{p_action,-4,103,[],0,50,0,2},{p_action,-1,103,[],0,50,0,2},{p_action,-3,103,[],0,50,0,2},{p_action,-2,103,[],0,50,0,2},{p_action,-14,103,[],0,50,0,2},{p_action,-11,103,[],0,50,0,2},{p_action,-13,103,[],0,50,0,2},{p_action,-12,103,[],0,50,0,2},{p_action,4,104,[],0,-700,0,2},{p_action,1,104,[],0,-700,0,2},{p_action,3,104,[],0,-700,0,2},{p_action,2,104,[],0,-700,0,2},{p_action,14,104,[],0,-700,0,2},{p_action,11,104,[],0,-700,0,2},{p_action,13,104,[],0,-700,0,2},{p_action,12,104,[],0,-700,0,2},{p_action,4,105,[],0,-900,0,2},{p_action,1,105,[],0,-900,0,2},{p_action,3,105,[],0,-900,0,2},{p_action,2,105,[],0,-900,0,2},{p_action,14,105,[],0,-900,0,2},{p_action,11,105,[],0,-900,0,2},{p_action,13,105,[],0,-900,0,2},{p_action,12,105,[],0,-900,0,2},{p_action,-4,104,[],0,-700,0,2},{p_action,-1,104,[],0,-700,0,2},{p_action,-3,104,[],0,-700,0,2},{p_action,-2,104,[],0,-700,0,2},{p_action,-14,104,[],0,-700,0,2},{p_action,-11,104,[],0,-700,0,2},{p_action,-13,104,[],0,-700,0,2},{p_action,-12,104,[],0,-700,0,2},{p_action,-4,105,[],0,-900,0,2},{p_action,-1,105,[],0,-900,0,2},{p_action,-3,105,[],0,-900,0,2},{p_action,-2,105,[],0,-900,0,2},{p_action,-14,105,[],0,-900,0,2},{p_action,-11,105,[],0,-900,0,2},{p_action,-13,105,[],0,-900,0,2},{p_action,-12,105,[],0,-900,0,2}]
       ,true}.

sample_res2()->
 {true,sample_res1()
 ,{[{2000000004166,27418360,100}
   ,{2000000004121,25134693,50}
   ,{2000000004127,22907685,100}
   ,{2000000004109,22295995,50}]
  ,[{2000000004166,27418360,50}
   ,{2000000004121,25134693,50}
   ,{2000000004127,22907685,50}
   ,{2000000004109,22295995,50}]
  ,[{ger,2000000004166,{gerBase,7220,20,100,4,0,[]},{gerAttr,2543493,2500,10000,2000,2938854,27418360,0,100,785,555,35,30,0,0,40,15,1097,650,1097,650,84374,2543493},{add_attr,27583,99533,0,0,775,540,20,20,0,0,0,0,1080,650,1080,650,105507,102507,0,0,0,0,0,0,0,0},27418360,100,0,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
   ,{ger,2000000004121,{gerBase,7050,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,2704329,25134693,100,200,760,555,35,30,0,3500,15,15,1081,650,1081,700,70314,2465117},{add_attr,27583,99533,100,0,750,540,20,20,0,0,0,0,1080,650,1080,700,106507,102507,0,0,0,0,0,0,0,0},25134693,50,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
   ,{ger,2000000004127,{gerBase,7070,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,2823704,22907685,100,100,760,555,35,55,2500,0,15,15,1081,650,1081,650,91846,2428329},{add_attr,27583,99533,100,0,750,540,20,45,0,0,0,0,1080,650,1080,650,106507,102507,0,0,0,0,0,0,0,0},22907685,100,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
   ,{ger,2000000004109,{gerBase,7010,20,100,1,0,[]},{gerAttr,2403837,2500,10000,2000,2674922,22295995,100,100,760,555,35,30,2325,0,15,15,1132,650,1097,650,80852,2403837},{add_attr,27583,99533,0,0,750,540,20,20,2500,0,0,0,1080,650,1080,650,105507,102507,0,0,0,0,0,0,0,0},22295995,50,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}]
  ,[{ger,2000000004166,{gerBase,7220,20,100,-4,0,[]},{gerAttr,2543493,2500,10000,2000,2913629,27418360,0,100,785,555,35,30,0,0,40,15,1113,650,1113,650,84374,2543493},{add_attr,27583,99533,0,0,775,540,20,20,0,0,0,0,1080,650,1080,650,105507,102507,0,0,0,0,0,0,0,0},27418360,50,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
   ,{ger,2000000004121,{gerBase,7050,20,100,-3,0,[]},{gerAttr,2465117,2500,10000,2000,2658301,25134693,100,200,760,555,35,30,0,3185,15,15,1097,650,1097,700,70314,2465117},{add_attr,27583,99533,100,0,750,540,20,20,0,0,0,0,1080,650,1080,700,105507,102507,0,0,0,0,0,0,0,0},25134693,50,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
   ,{ger,2000000004127,{gerBase,7070,20,100,-2,0,[]},{gerAttr,2428329,2500,10000,2000,2775644,22907685,100,100,760,555,35,55,2325,0,15,15,1113,650,1113,650,91846,2428329},{add_attr,27583,99533,100,0,750,540,20,45,0,0,0,0,1080,650,1080,650,105507,102507,0,0,0,0,0,0,0,0},22907685,50,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
   ,{ger,2000000004109,{gerBase,7010,20,100,-1,0,[]},{gerAttr,2403837,2500,10000,2000,2674922,22295995,100,100,760,555,35,30,2325,0,15,15,1132,650,1097,650,80852,2403837},{add_attr,27583,99533,0,0,750,540,20,20,2500,0,0,0,1080,650,1080,650,105507,102507,0,0,0,0,0,0,0,0},22295995,50,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}]}}.
