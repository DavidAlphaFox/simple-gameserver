-module(doublematch_rank).
-behaviour(gen_server).
-include("def_role.hrl").
-include("def_fight.hrl").
-include("def_doublematch.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(pd_ref_timer_session,pd_ref_timer_session).  % 记录切换赛季的timer
-define(persist_interval, 3600). %% 每小时保存一次数据
-define(next_rec_id_step,100).

-record(state, {session = 0
               ,rank_version = 0
               ,score_min = 0
               ,rank_list = []
               ,next_rec_id = ?next_rec_id_step
               ,old_rank_list = {0,[]}}). % rank_list 排名信息，需要保存数据库

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    process_flag(trap_exit,true),
    State = case db_sql:get_etc(?DB_ETC_KEY_DOUBLEMATCH_MATCH) of
        #state{} = X ->
            % 新版本格式
            NewRankList = update_record_db_rank_list(X#state.rank_list),
            {OldV,OldVersionOldRankList} = X#state.old_rank_list,
            NewVersionOldRankList = update_record_db_rank_list(OldVersionOldRankList),
            X#state{rank_list = NewRankList
                   ,old_rank_list = {OldV,NewVersionOldRankList}};
        _ ->
%%             RankTop = data_doublematch:get(rank_top),
            {RScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,6}),
            #state{session = doublematch_server:get_session()
                  ,rank_version = 0
                  ,score_min = RScore
                  ,rank_list = []
                  ,next_rec_id = 100}
    end,
    erlang:send(?MODULE, persist_tick),
%%     set_persist_interval(),
%%     NewState = update_sessoin(State), 
%%     gen_server:cast(doublematch_rank, sync_session),
    {ok, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(sync_session, State) -> 
    gen_server:cast(doublematch_match, {sync_session,State#state.session}),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% 非本赛季无效
handle_info({update_rank,Session,_TS,_PUnit,_FightRecord,_AtkRandInt,_DefRandInt,_CheckRankList,_DeadNum}, State) when State#state.session /= Session->
    {noreply, State};
handle_info({update_rank,Session,TS,PUnit,FightRecord,AtkRandInt,DefRandInt,CheckRankList,DeadNum}, State) when State#state.session =:= Session->
    %%     RankTop = data_doublematch:get(rank_top),
    {RScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,6}),
    DataRankMaxLength = data_doublematch:get(data_rank_length),
    #state{rank_version = OldVersion
           ,next_rec_id = NextRecId} = State,
    {IsChange,NewState} = lists:foldr(fun({SignInfo,NewSroce},{AccIsChange,AccState})-> 
                                              #state{score_min = OldScoreMin
                                                     ,rank_list = OldRankList} = AccState,
                                              RankListL = erlang:length(OldRankList),
                                              ScoreMin0 = if
                                                              RankListL < DataRankMaxLength ->
                                                                  RScore;
                                                              true ->
                                                                  OldScoreMin
                                                          end,
                                              if
                                                  NewSroce =< ScoreMin0 ->
                                                      case lists:keytake(SignInfo#dm_sign_info.role_id, #dm_rank.role_id, OldRankList) of
                                                          {value,_,RestOldRankList} ->
                                                              {true,AccState#state{score_min = ScoreMin0
                                                                                   ,rank_list = RestOldRankList}};
                                                          false ->
                                                              {AccIsChange,AccState}
                                                      end;
                                                  true ->
                                                      {ScoreMin,NewRankList,_NewRankIndex} = sort_rank(SignInfo,PUnit,TS,NewSroce,NextRecId,OldRankList),
                                                      RankListL2 = erlang:length(NewRankList),
                                                      ScoreMin2 = if
                                                                      RankListL2 < DataRankMaxLength ->
                                                                          RScore;
                                                                      true ->
                                                                          ScoreMin
                                                                  end,
                                                      {true,AccState#state{score_min = ScoreMin2
                                                                           ,rank_list = NewRankList}} % 如果第二次第三次进入这里，设置的录像ID是相同的
                                              end
                                      end, {false,State}, CheckRankList),
    if
        IsChange =:= true ->
            % 这里本想更新所有录像排名是实时的，但运算压力太大，所以录像排名保持战前的
%%             NewRankList2 = lists:foldr(fun(Rank,AccRankList)->
%%                     OldPunit = Rank#dm_rank.punit,
%%                     NewPunit = lists:foldr(fun(P,AccPList)->
%%                             NewP = case lists:keysearch(P#p_doublematch_rank_unit.roleID, #dm_rank.role_id, RankList1k) of
%%                                        {value, DmRank} ->
%%                                            P#p_doublematch_rank_unit{rank = DmRank#dm_rank.index};
%%                                        false ->
%%                                            P
%%                                    end,
%%                             [NewP|AccPList]
%%                         end, [], OldPunit),
%%                     [Rank#dm_rank{punit=NewPunit}|AccRankList]
%%                 end, [], RankList1k),
            NewVersion = OldVersion + 1,
            NewState2 = NewState#state{rank_version=NewVersion
                                      ,next_rec_id = NextRecId+?next_rec_id_step},
            DataRankSyncLength = data_doublematch:get(data_sync_rank_length),
            RankListLength = erlang:length(NewState2#state.rank_list),
            IsRec = lists:foldl(fun({SignInfo,_},AccIsRec)->
                    {RankList1k,RankList1w} = if
                        RankListLength > DataRankSyncLength ->
                            {RankList1k0, RankList1w0} = lists:split(DataRankSyncLength, NewState2#state.rank_list),
                            {RankList1k0,[{E#dm_rank.role_id,E#dm_rank.index}||E<-RankList1w0,E#dm_rank.server_id =:= SignInfo#dm_sign_info.server_id]};
                        true ->
                            {NewState2#state.rank_list,[]}
                    end,
                    %% 立即通知参赛队伍，排名的变化，以便对他们来说，排名是实时的
                    send_msg:direct(SignInfo#dm_sign_info.server_id, doublematch_server, {get_rank_res,NewVersion,RankList1k,RankList1w}),
                    IsRec0 = lists:keymember(SignInfo#dm_sign_info.role_id, #dm_rank.role_id, RankList1k),
                    AccIsRec orelse IsRec0
                end, false, CheckRankList),
            if IsRec =:= true -> save_fight_rec(NextRecId,TS,FightRecord,AtkRandInt,DefRandInt,DeadNum,PUnit); true -> ignore end,
            {noreply, NewState2};
        true ->
            {noreply, State}
    end;
%% 游戏服发送的请求最新排名
handle_info({get_rank_req,ServerPid,CurVersion}, #state{rank_list = RankList, rank_version = Version}=State) ->
    ?INFO("doublematch_rank get_rank_req ~w ~w",[ServerPid,CurVersion]),
    if
        CurVersion /= Version ->
            DataRankSyncLength = data_doublematch:get(data_sync_rank_length),
            RankListLength = erlang:length(RankList),
            {RankList1k,RankList1w} = if
                RankListLength > DataRankSyncLength ->
                    {RankList1k0, RankList1w0} = lists:split(DataRankSyncLength, RankList),
                    {RankList1k0,[{E#dm_rank.role_id,E#dm_rank.index}||E<-RankList1w0,E#dm_rank.server_id =:= ServerPid]};
                true ->
                    {RankList,[]}
            end,
            erlang:send(ServerPid, {get_rank_res,Version,RankList1k,RankList1w});
        true ->
            %% 数据版本号一致，无需更新
            erlang:send(ServerPid, {get_rank_res,?undefined})
    end,    
    {noreply, State};
handle_info({get_rec_req,ServerPid,RecID}, State) ->
    case ets:lookup(?ETS_DM_FIGHT_REC, RecID) of
        []->
            case  db_sql:get_dm_fight_rec(RecID) of
                ?undefined ->
                    ?INFO("get_rec_req ~w ~w undefined",[ServerPid,RecID]),
                    erlang:send(ServerPid,{get_rec_res,RecID,?undefined});
                Rec ->
                    ets:insert(?ETS_DM_FIGHT_REC, {RecID,Rec#sc_doublematch_replay.fight_info
                                                  ,Rec#sc_doublematch_replay.dice_numA
                                                  ,Rec#sc_doublematch_replay.dice_numB}),
                    erlang:send(ServerPid,{get_rec_res,RecID,Rec#sc_doublematch_replay.fight_info
                                          ,Rec#sc_doublematch_replay.dice_numA
                                          ,Rec#sc_doublematch_replay.dice_numB})
            end;
        [{RecID,FightRecord,AtkRandInt,DefRandInt}] ->
            ?INFO("get_rec_req ~w ~w ok",[ServerPid,RecID]),
            erlang:send(ServerPid,{get_rec_res,RecID,FightRecord,AtkRandInt,DefRandInt})
    end,
    {noreply, State};
handle_info({get_old_rank,ServID,ReqSession,SPid}, State) ->
    ?INFO("~w",[{get_old_rank,ServID,ReqSession,SPid}]),
    {RecOldSession,_} = State#state.old_rank_list,
    if
        RecOldSession /= ReqSession andalso RecOldSession /= 0 ->
            ?ERR("get_old_rank error. req(~w):~w old:~w",[ServID,ReqSession,RecOldSession]);
        true ->
            ignore
    end,
    erlang:send(SPid,{get_old_rank, State#state.session, State#state.old_rank_list}),
    {noreply, State};
handle_info({get_only_old_rank_req,ServerPid}, State) ->
    erlang:send(ServerPid,{get_only_old_rank_req, erlang:element(2, State#state.old_rank_list)}),
    {noreply, State};
handle_info({timeout, TimerRef, session_change}, State) ->
    case get(?pd_ref_timer_session) of
        TimerRef ->
            ignore;
        Err ->
            ?ERR("TimerRef error ~w ~w s:~w",[Err,TimerRef,State])
    end,
    erlang:erase(?pd_ref_timer_session),
    NewState = change_session(State,true),
    gen_server:cast(doublematch_match, {sync_session,NewState#state.session}),
    {noreply, NewState};
handle_info(persist_tick, #state{next_rec_id=NextRecId} = State) ->
    db_sql:backup_dm_rank(State),
    % 清理缓存旧数据
    DelIdList = ets:select(?ETS_DM_FIGHT_REC, [{{'$1','_','_','_'},[{'<','$1',NextRecId - 20000}],['$1']}]),    
    lists:foreach(fun(Id)-> ets:delete(?ETS_DM_FIGHT_REC, Id) end,DelIdList),
    persist_check(State),
    NewState = update_sessoin(State),   % 可能会设置赛季结束timer
    set_persist_interval(),
    gen_server:cast(doublematch_match, {sync_session,NewState#state.session}),
    {noreply, NewState};
handle_info(i, State) ->
    ?INFO("state:~w, curl:~w",[State,get_time_limit(State) - util:now()]),
    {noreply, State};
handle_info(fix_reward,State) ->
    fix_reward(State#state.old_rank_list),
    {noreply, State};
handle_info(test_crash, State) ->
    A = 1,
    B = 2,
    A = B,
    {noreply, State};
%% handle_info({test_set_old,{OldS,OldL}},State) when erlang:is_list(OldL)->
%%     {noreply, State#state{old_rank_list={OldS,OldL}}};
handle_info(_Info, State) ->
    ?WARNING("Unknown info ~w",[_Info]),
    {noreply, State}.

terminate(_Reason, State) -> 
    persist_check(State).

%%%===================================================================
%%% Internal functions 
%%%===================================================================

    % 根据CurSession算出当前赛季结束时间
get_time_limit(State)->
    CurSession = State#state.session,
    Day = data_doublematch:get(init_day),
    {NowYear,NowMonth,NowDay} = erlang:date(),
    Year = CurSession div 100,
    Month = CurSession rem 100,
    % 根据CurSession算出当前赛季结束时间
    if
        NowDay >= Day andalso Month < 12 ->
            util:datetime_to_seconds({{Year,Month+1,Day},{0,0,0}});
        NowDay >= Day andalso Month =:= 12 ->
            util:datetime_to_seconds({{Year+1,1,Day},{0,0,0}});
        NowDay < Day ->
            util:datetime_to_seconds({{NowYear,NowMonth,Day},{0,0,0}})
    end.

update_sessoin(State)->
    CurTimeLimit = get_time_limit(State),
    % 取消旧的timer下面需要的话会重新设定
    case get(?pd_ref_timer_session) of
        ?undefined ->
            ignore;
        OldRef  when erlang:is_reference(OldRef)->
            erlang:cancel_timer(OldRef),
            erlang:erase(?pd_ref_timer_session)
    end,
    % 检查现在状态
    Now = util:now(),
    if
        % 赛季已经结束了
        CurTimeLimit < Now ->
            NewState = change_session(State,false),
            NewState;
        CurTimeLimit =< Now + ?persist_interval ->
            Ref = erlang:start_timer((CurTimeLimit - Now + 1)*1000, self(), session_change),
            put(?pd_ref_timer_session,Ref),
            State;
        true ->
            State
    end.

%执行赛季交替，备份户数，清空排行榜
change_session(State,IsCheckAgain)->
    NowSession = doublematch_server:get_session(),
    if
        State#state.session /= NowSession ->
            db_sql:backup_dm_rank(State),
            db_sql:del_old_dm_fight_rec(),
            NewState = State#state{session = doublematch_server:get_session()
                       ,rank_version = State#state.rank_version+1
                       ,score_min = 0
                       ,rank_list = []
                       ,old_rank_list = {State#state.session,State#state.rank_list}},
            ?ERR("change_session~nold:~w~nnew:~w",[State,NewState]),
            NewState; % 保存上一赛季数据，用于发放奖励
        IsCheckAgain =:= true ->
            update_sessoin(State);
        true ->
            ignore
    end.

sort_rank(SignInfo,PUnit,TS,NewSroce,RecId,OldRankList)->
    ?INFO("sort_rank ~w ~w~n+++++++++++++++++ ~w",[NewSroce,SignInfo,OldRankList]),
    OldRankList2 = lists:filter(fun(S)-> SignInfo#dm_sign_info.role_id /= S#dm_rank.role_id end, OldRankList),
    NewDmRank = 
        #dm_rank{role_id = SignInfo#dm_sign_info.role_id
                ,server_id = SignInfo#dm_sign_info.server_id
                ,name  = SignInfo#dm_sign_info.name
                ,index = 0
                ,remain_time = 0
                ,already_buy = 0
                ,level = SignInfo#dm_sign_info.level
%%                 ,rank = set_rank_onlist(NewSroce)
                ,rank = doublematch_server:check_rank_by_sroce(NewSroce)
                ,score = NewSroce
                ,isMale = SignInfo#dm_sign_info.isMale
                ,title = SignInfo#dm_sign_info.title
                ,head =SignInfo#dm_sign_info.head
                ,rec_id=RecId
                ,punit = PUnit
                ,ts=TS
                ,vip=SignInfo#dm_sign_info.vip
                ,score_change = NewSroce - SignInfo#dm_sign_info.dm_data#dm_data.score},
    sort_rank2(NewDmRank,[],1,OldRankList2,0).

%只有开服或新一届有这种情况
sort_rank2(NewDmRank,[],Index,[],_RankNum)-> 
    {0,[NewDmRank#dm_rank{index = Index
                         ,rank = set_rank_onlist(NewDmRank#dm_rank.score,Index)}],Index};
%排序未完，新元素已经插入
sort_rank2(?undefined,NewRankList,Index,[Front|OtherOldRankList],RankNum)->
    sort_rank2(?undefined,[Front#dm_rank{index=Index
                                        ,rank = set_rank_onlist(Front#dm_rank.score,Index)}|NewRankList],Index+1,OtherOldRankList,RankNum);
%排序已完，新元素已经插入
sort_rank2(?undefined,NewRankList,_Index,[],RankNum)->
    CurLength = erlang:length(NewRankList),
    MaxLength = data_doublematch:get(data_rank_length),
    {ScoreMin,NewRankList2} = if 
                    CurLength < MaxLength ->
                        {0,NewRankList};
                    CurLength =:= (MaxLength+1) ->
                        [_|NewRankList0] = NewRankList,
                        [Min|_] = NewRankList0,
                        {Min#dm_rank.score,NewRankList0};
                    true ->
                        [Min|_] = NewRankList,
                        {Min#dm_rank.score,NewRankList}
               end,
    {ScoreMin,lists:reverse(NewRankList2),RankNum};
%排序已完，新元素未插入,有可能超长，有可能没到最大，也有可能刚到最大
sort_rank2(NewDmRank,NewRankList,Index,[],_RankNum) when NewDmRank /= ?undefined->
    sort_rank2(?undefined,[NewDmRank#dm_rank{index = Index
                                            ,rank = set_rank_onlist(NewDmRank#dm_rank.score,Index)}|NewRankList],Index+1,[],Index);
% 排序未完，新元素未插入,有可能超长，有可能没到最大，也有可能刚到最大
sort_rank2(NewDmRank,NewRankList,Index,[Front|OtherOldRankList],RankNum) when NewDmRank /= ?undefined->
    if
        NewDmRank#dm_rank.score > Front#dm_rank.score 
          orelse (NewDmRank#dm_rank.score =:= Front#dm_rank.score andalso NewDmRank#dm_rank.role_id < Front#dm_rank.role_id) -> %分数相等的话，晚达到的靠后
            sort_rank2(?undefined,[NewDmRank#dm_rank{index = Index
                                                    ,rank = set_rank_onlist(NewDmRank#dm_rank.score,Index)}|NewRankList],Index+1,[Front|OtherOldRankList],Index);
        true ->
            sort_rank2(NewDmRank,[Front#dm_rank{index=Index
                                               ,rank = set_rank_onlist(Front#dm_rank.score,Index)}|NewRankList],Index+1,OtherOldRankList,RankNum)
    end.

save_fight_rec(NextRecId,TS,FightRecord,AtkRandInt,DefRandInt,DeadNum,PUnit)->
    db_sql:insert_dm_fight_rec(NextRecId,TS,#sc_doublematch_replay{result=1
                                                               ,fight_info=FightRecord
                                                               ,dice_numA=AtkRandInt
                                                               ,dice_numB=DefRandInt},DeadNum,PUnit),
    ets:insert(?ETS_DM_FIGHT_REC, {NextRecId,FightRecord,AtkRandInt,DefRandInt}).
         
% 上榜的人，如果达到超凡，那么应该是王者
set_rank_onlist(Score,Index)->
    RankTop = data_doublematch:get(rank_top),
	WangZheNum = data_doublematch:get(wangzhe_num),
    case doublematch_server:check_rank_by_sroce(Score) of
        NewRank0 when NewRank0 =:= RankTop andalso WangZheNum >= Index->
            RankTop+1;
        NewRank0 ->
            NewRank0
    end.

persist_check(State)->
    db_sql:set_etc(?DB_ETC_KEY_DOUBLEMATCH_MATCH, State).
  
%设定下次timer
set_persist_interval()->
    erlang:send_after(?persist_interval*1000, self(), persist_tick).

show_rank()->
    erlang:send(?MODULE, i).

% 针对5月1日，奖励少发的问题，临时加的修正接口，在2.6.0版本时把这个删去吧
fix_reward({Session,OldRank}) when Session =:= 201604 ->
    fix_reward2(OldRank,0);
fix_reward({Session,_OldRank}) ->
    ?ERR("fix_reward fail ~w",[Session]),
    ignore.

fix_reward2([],Count)->
    ?ERR("fix_reward2 finish ~w",[Count]);
fix_reward2([CurRank|OtherRank],AccCount)->
    NewAccCount = if
        CurRank#dm_rank.score < 2500 ->
            send_msg:direct(CurRank#dm_rank.server_id, doublematch_server, {fix_reward,CurRank}),
            AccCount + 1;
        true ->
            AccCount
    end,
    fix_reward2(OtherRank,NewAccCount).

% 2.5.0数据格式转为2.6.0
update_record_db_rank_list(L)->
    update_record_db_rank_list2(L,[]).

update_record_db_rank_list2([],AccList)->
    lists:reverse(AccList);
update_record_db_rank_list2([H|T],AccList)->
    update_record_db_rank_list2(T,[update_record_db_rank(H)|AccList]).
    
update_record_db_rank(R) when erlang:is_record(R, dm_rank)->
    OldPList = R#dm_rank.punit,
    NewPList = update_p_unit([],OldPList),
    R#dm_rank{punit = NewPList};
update_record_db_rank({dm_rank,RoleID,ServerID,Name,Index,RemainTime,AlreadyBuy,Level
                      ,Rank,Score,IsMale,Title,Head,RecID,Punit,Ts})->
    #dm_rank{role_id = RoleID
            ,server_id = ServerID
            ,name  = Name
            ,index = Index
            ,remain_time = RemainTime
            ,already_buy = AlreadyBuy
            ,level = Level
            ,rank = Rank
            ,score = Score
            ,isMale = IsMale
            ,title = Title
            ,head = Head
            ,rec_id = RecID
			,punit = update_p_unit([],Punit)
            ,ts=Ts
            ,score_change = 0};
update_record_db_rank({dm_rank,RoleID,ServerID,Name,Index,RemainTime,AlreadyBuy,Level
                      ,Rank,Score,IsMale,Title,Head,RecID,Punit,Ts,ScoreChange})->
    #dm_rank{role_id = RoleID
            ,server_id = ServerID
            ,name  = Name
            ,index = Index
            ,remain_time = RemainTime
            ,already_buy = AlreadyBuy
            ,level = Level
            ,rank = Rank
            ,score = Score
            ,isMale = IsMale
            ,title = Title
            ,head = Head
            ,rec_id = RecID
            ,punit = update_p_unit([],Punit)
            ,ts=Ts
            ,score_change = ScoreChange}.


update_p_unit(AccList,[])->
    lists:reverse(AccList);
update_p_unit(AccList,[T|H])->
    update_p_unit([update_p_unit2(T)|AccList],H).

update_p_unit2({p_doublematch_rank_unit,RoleID,Name,IsMale,Title,Head,Level,ServerID,GRank,GLevel,RankScore,Index,ScoreChange,Vip})->
    {p_doublematch_rank_unit,RoleID,Name,IsMale,Title,Head,Level,ServerID,GRank,GLevel,RankScore,Index,ScoreChange,Vip};
update_p_unit2({p_doublematch_rank_unit,RoleID,Name,IsMale,Title,Head,Level,ServerID,GRank,GLevel,RankScore,Index,ScoreChange})->
    {p_doublematch_rank_unit,RoleID,Name,IsMale,Title,Head,Level,ServerID,GRank,GLevel,RankScore,Index,ScoreChange,1};
update_p_unit2({p_doublematch_rank_unit,RoleID,Name,IsMale,Title,Head,Level,ServerID,GRank,GLevel,RankScore,Index})->
    {p_doublematch_rank_unit,RoleID,Name,IsMale,Title,Head,Level,ServerID,GRank,GLevel,RankScore,Index,0,1}.
    


