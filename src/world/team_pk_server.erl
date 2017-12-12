-module(team_pk_server).

-behaviour(gen_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_team_pk.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SYNC_RANK_NUM_LIMIT,50).         %%同步排行榜的个数
-define(set_share_open_time(Time),  ets:insert(?ETS_ETC, {team_pk_server, Time})).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-export([
         start/0
         ]).


start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {count=0,session=0,lastOpenTimestamp=0,lastCloseTimestamp=0,status=0, roleIDList=[], rankRoleList=[], recordList=[]}).

-define(STATUS_CLOSE, 0).
-define(STATUS_OPEN, 1).

-define(team1v3ReplayRecord, team1v3ReplayRecord).
-define(next_open_time, next_open_time).
-define(next_close_time, next_close_time).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(CLEAR_TIME, {5, 0, 0}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    process_flag(trap_exit,true),
    case db_sql:get_etc(?DB_ETC_KEY_TEAM_PK) of
        [{state,State}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            State = #state{}
    end,
    #state{lastOpenTimestamp=LastOpenTimestamp} = NewState = init_state(State#state{count=0}),
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    RoleIDList = db_sql:get_level_role_id_list(data_team_pk:get(get_data_level), 999),
	erlang:send_after(?DUMP_INTERVAL, self(), do_hibernate),
    ?set_share_open_time(LastOpenTimestamp),
    {ok, NewState#state{roleIDList=lists:sublist(util:random_list_quick(RoleIDList), 50)
                       ,rankRoleList = update_team_pk_rank_format(NewState#state.rankRoleList)}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(get_session, _From, #state{session=Session}=State) ->
    Reply = {ok, Session},
    {reply, Reply, State};
handle_call({cs_team_pk_info, RoleID},  _From, State) ->
    Reply = cs_team_pk_info(RoleID, State),
    {reply, Reply, State};
handle_call({update_rank_and_record, RoleInfo, OtherDeadCount, NewTeamRecord, DelReplayUIDList}, _From, State) ->
    {ok, OldRank, NewRank, NewState} = update_rank_and_record(RoleInfo, OtherDeadCount, NewTeamRecord, DelReplayUIDList, State),
    {reply, {ok, OldRank, NewRank}, NewState};
handle_call(get_status, _From, #state{status=Status}=State) ->
    Reply = {ok, Status},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_info(do_hibernate,State)->
	erlang:send_after(?DUMP_INTERVAL, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, State) ->
    do_dump(State).


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info({gen_team_data, RoleID}, State) ->
    erlang:spawn(fun() -> gen_team_data_as(RoleID, State) end),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_team_new_status{}}, #state{status=Status}=State) ->
    case Status of
        ?STATUS_OPEN ->
            ?unicast(RoleID, #sc_team_new_status{isOpen=true});
        ?STATUS_CLOSE ->
            ?unicast(RoleID, #sc_team_new_status{isOpen=false})
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_team_record{}}, #state{recordList=RecordList}=State) ->
    ?unicast(RoleID, #sc_team_record{recordList=trans2precord(RecordList)}),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_team_fight_replay{replayUIDList=ReplayUIDList}}, #state{recordList=RecordList}=State) ->
    get_team_fight_replay(RoleID, ReplayUIDList, RecordList),
    {noreply, State};
do_handle_info({cs_team_rank, RoleInfo}, #state{rankRoleList=RankRoleList}=State) ->
    ?DEBUG("cs_team_rank"),
    #role{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level
          ,roleName=RoleName,vipLevel=Vip,svipLevel=SVip}=RoleInfo,
    case lists:keyfind(RoleID, #team_pk_rank.roleID, RankRoleList) of
        false ->
            SelfRank = 0,
            SelfTeamPkRank = #team_pk_rank{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head
                                           ,level=Level,roleName=RoleName,vip=role_lib:cacl_vip_info(Vip,SVip)},
            RankRoleList2 = lists:sublist(RankRoleList, ?SYNC_RANK_NUM_LIMIT) ++ [SelfTeamPkRank]; 
        #team_pk_rank{rank=SelfRank}=SelfTeamPkRank ->
            case SelfRank =< ?SYNC_RANK_NUM_LIMIT of
                true ->
                    RankRoleList2 = lists:sublist(RankRoleList, ?SYNC_RANK_NUM_LIMIT);
                false ->
                    RankRoleList2 = lists:sublist(RankRoleList, ?SYNC_RANK_NUM_LIMIT) ++ [SelfTeamPkRank]
            end
    end,
    DataRecord = #sc_team_rank{rankList=trans_rank_role_list(RankRoleList2),selfRank=SelfRank},
    ?unicast(RoleID, DataRecord),
    {noreply, State};
do_handle_info(set_status_open, #state{status=?STATUS_CLOSE,session=Session}=State) ->
    ?ERR("set_status_open"),
    Now = util:now(),
    erase_replay_record(),
    erlang:send_after((?ONE_DAY_SECONDS * data_team_pk:get(open_day_num))*1000, self(), set_status_close),
    erlang:send_after(get_send_after_seconds(?CLEAR_TIME) * 1000, erlang:self(), del_replay),
    erlang:put(?next_close_time, Now+?ONE_DAY_SECONDS * data_team_pk:get(open_day_num)),
    catch broadcast_server:bc(#sc_team_new_status{isOpen=true}),
    %% 重置挑战次数
    spawn(fun() ->
            lists:foreach(fun(E) -> 
                             catch role_lib:send_server(E, reset_teamPkTimes)
                          end, ets:tab2list(?ETS_ROLE_ONLINE))
        end),
    ?set_share_open_time(Now),
    {noreply, State#state{session=Session+1,status=?STATUS_OPEN,lastOpenTimestamp=Now,rankRoleList=[], recordList=[]}};
do_handle_info(set_status_close, #state{status=?STATUS_OPEN,rankRoleList=RankRoleList}=State) ->
    erlang:send_after(10000, self(), {send_reward, RankRoleList}),
    ?ERR("set_status_close"),
    Now = util:now(),
    erlang:send_after((?ONE_DAY_SECONDS * data_team_pk:get(close_day_num))*1000, self(), set_status_open),
    erlang:put(?next_open_time, Now + ?ONE_DAY_SECONDS * data_team_pk:get(close_day_num)),
    catch broadcast_server:bc(#sc_team_new_status{isOpen=false}),
    {noreply, State#state{status=?STATUS_CLOSE,lastCloseTimestamp=Now}};
do_handle_info({send_reward, RankRoleList}, State) ->
    do_send_reward(RankRoleList),
    {noreply, State};
do_handle_info(dump_data, #state{count=Count}=State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    RoleIDList = db_sql:get_level_role_id_list(data_team_pk:get(get_data_level), 999),
    NewState = State#state{roleIDList=lists:sublist(util:random_list_quick(RoleIDList), 50),count=Count+1},
    do_dump(NewState),
    case Count rem 6 of
        0 ->
            erase_replay_record();
        _ ->
            next
    end,
    {noreply, NewState};
do_handle_info(del_replay, State) ->
    del_replay(),
    {noreply, State};
do_handle_info({update_role_name, RoleID, Name}, #state{rankRoleList=RankRoleList}=State) ->
    case lists:keyfind(RoleID, #team_pk_rank.roleID, RankRoleList) of
        false ->
            NewRankRoleList = RankRoleList;
        Info ->
            NewRankRoleList = lists:keyreplace(RoleID, #team_pk_rank.roleID, RankRoleList, Info#team_pk_rank{roleName=Name})
    end,
    {noreply, State#state{rankRoleList=NewRankRoleList}};
do_handle_info({update_role_vip, RoleID, Vip}, #state{rankRoleList=RankRoleList}=State) ->
    NewRankRoleList = case lists:keyfind(RoleID, #team_pk_rank.roleID, RankRoleList)of
                          false -> RankRoleList;
                          Info -> lists:keyreplace(RoleID, #team_pk_rank.roleID, RankRoleList, Info#team_pk_rank{vip=Vip})
                      end,
    {noreply, State#state{rankRoleList=NewRankRoleList}};
do_handle_info(update_state, State=#state{rankRoleList=RankRoleList}) ->
    NewRankRoleList = lists:foldl(fun({V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11},Acc)->
                                          [{V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,1}|Acc]
                                    end, [], RankRoleList),
    {noreply, State#state{rankRoleList=NewRankRoleList}};
do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.


do_dump(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_TEAM_PK,Info2).

is_persist({?next_open_time,_}) ->
    true;
is_persist({?next_close_time,_}) ->
    true;
is_persist(_) ->
    false.

init_state(#state{session=Session,lastOpenTimestamp=LastOpenTimestamp,lastCloseTimestamp=LastCloseTimestamp,status=Status}=State) ->
    case fix_after_merge(State) of 
        {true,NewState} ->
            NewState;
        _ ->
		    case Status of
		        ?STATUS_CLOSE ->
		            case LastCloseTimestamp =:= 0 of
		                true ->
		                    LocalDateTime = erlang:localtime(),
		                    {ServerOpenDate, ServerOpenTime} = data_setting:get(serverOpenTime),
		                    case LocalDateTime =< {ServerOpenDate, ServerOpenTime} of
		                        true ->
		                            OpenTimestamp = util:datetime_to_seconds({ServerOpenDate,data_team_pk:get(open_time)}) + ?ONE_DAY_SECONDS,
		                            erlang:send_after((OpenTimestamp - util:now())*1000, self(), set_status_open),
		                            erlang:put(?next_open_time, OpenTimestamp),
		                            State;
		                        false ->
		                            OpenTimestamp = util:datetime_to_seconds({erlang:date(), data_team_pk:get(open_time)})+ ?ONE_DAY_SECONDS,
		                            erlang:send_after((OpenTimestamp - util:now())*1000, self(), set_status_open),
		                            erlang:put(?next_open_time, OpenTimestamp),
		                            State
		                    end;
		                false ->
		                    ShouldOpenTimestamp = LastCloseTimestamp + ?ONE_DAY_SECONDS * data_team_pk:get(close_day_num),
		                    case ShouldOpenTimestamp =< util:now() of
		                        true ->
		                            OpenTimestamp = util:datetime_to_seconds({erlang:date(), data_team_pk:get(open_time)}),
		                            CloseTimestamp = OpenTimestamp + ?ONE_DAY_SECONDS * data_team_pk:get(close_day_num),
		                            erlang:send_after((CloseTimestamp - util:now())*1000, self(), set_status_close),
		                            erlang:spawn(fun() -> db_sql:del_replay_with_type(?REPLAY_TYPE_3V3) end),
		                            erlang:put(?next_close_time, CloseTimestamp),
		                            State#state{session=Session+1,status=?STATUS_OPEN,lastOpenTimestamp=OpenTimestamp,rankRoleList=[], recordList=[]};
		                        false ->
		                            erlang:send_after((ShouldOpenTimestamp - util:now())*1000, self(), set_status_open),
		                            erlang:put(?next_open_time, ShouldOpenTimestamp),
		                            State
		                    end
		            end;
		        ?STATUS_OPEN ->
		            ShouldCloseTimestamp = LastOpenTimestamp + ?ONE_DAY_SECONDS * data_team_pk:get(open_day_num),
		            case ShouldCloseTimestamp =< util:now() of
		                true ->
		                    case erlang:time() < data_team_pk:get(open_time) of
		                        true ->
		                            CloseTimestamp = util:datetime_to_seconds({erlang:date(), data_team_pk:get(open_time)}),
		                            erlang:send_after((CloseTimestamp - util:now())*1000, self(), set_status_close),
		                            erlang:put(?next_close_time, CloseTimestamp),
		                            State;
		                        false ->
		                            CloseTimestamp = util:datetime_to_seconds({erlang:date(), data_team_pk:get(open_time)}),
		                            NewOpenTimestamp = CloseTimestamp + ?ONE_DAY_SECONDS * data_team_pk:get(close_day_num),
		                            erlang:send_after((NewOpenTimestamp - util:now())*1000, self(), set_status_open),
		                            erlang:put(?next_open_time, NewOpenTimestamp),
		                            State#state{status=?STATUS_CLOSE,lastCloseTimestamp=CloseTimestamp}
		                    end;
		                false ->
		                    erlang:send_after((ShouldCloseTimestamp - util:now())*1000, self(), set_status_close),
		                    erlang:put(?next_close_time, ShouldCloseTimestamp),
		                    State
		            end
		    end
        end.

gen_team_data(RoleID, State) ->
    #state{roleIDList=RoleIDList,rankRoleList=RankRoleList} = State,
    case random_select_role_id_list(RoleID, lists:sublist(RoleIDList, 50), RankRoleList) of
        [FID1, FID2, EID1, EID2, EID3] ->
            {[FID1,FID2], [EID1, EID2, EID3]};
        _ ->
            {[], []}
    end.

random_select_role_id_list(RoleID, RoleIDList, RankRoleList) ->
    RoleIDList2 = lists:delete(RoleID, RoleIDList),
    case lists:keytake(RoleID, #team_pk_rank.roleID, RankRoleList) of
        false ->
            RankRoleList2 = RankRoleList,
            RoleScore = 0;
        {value, #team_pk_rank{score=RoleScore}, RankRoleList2} ->
            next
    end,
    RankRoleIDList = [ID||#team_pk_rank{roleID=ID}<-RankRoleList2],
    NoRankRoleIDList = RoleIDList2 -- RankRoleIDList,
    NoRankRoleList = [#team_pk_rank{roleID=ID}||ID<-NoRankRoleIDList],
    AllRankRoleList = util:random_list_quick(RankRoleList2 ++ NoRankRoleList),
    InitVal = data_team_pk:get(arg_init_val),
    AddVal = data_team_pk:get(arg_add_val),
    ScoreLeft = erlang:max(0, RoleScore - InitVal),
    ScoreRight = RoleScore + InitVal,
    ScoreRankList = get_score_list(AllRankRoleList, ScoreLeft, ScoreRight, AddVal),
    case erlang:length(ScoreRankList) >= 5 of
        true ->
            [ID||#team_pk_rank{roleID=ID}<-lists:sublist(ScoreRankList, 5)];
        false ->
            []
    end.

get_score_list(AllRankRoleList, ScoreLeft, ScoreRight, AddVal) ->
    FilterList = filter_score_list(AllRankRoleList, ScoreLeft, ScoreRight),
    case FilterList of
        AllRankRoleList ->
            AllRankRoleList;
        _ ->
            case erlang:length(FilterList) >= 50 of
                true ->
                    FilterList;
                false ->
                    get_score_list(AllRankRoleList, erlang:max(0, ScoreLeft - AddVal), ScoreRight + AddVal, AddVal)
            end
    end.

filter_score_list(RankRoleList, ScoreLeft, ScoreRight) ->
    lists:filter(fun(#team_pk_rank{score=Score}) ->
                         Score >= ScoreLeft andalso Score =< ScoreRight;
                    ({_V1,_V2,V3,_V4,_V5,_V6,_V7,_V8,_V9,_V10,_V11}) ->
                         V3 >= ScoreLeft andalso V3 =< ScoreRight
                 end, RankRoleList).

do_send_reward(RankRoleList) ->
    RankRewardList = data_team_pk:get(rank_reward_list),
    RewardRoleIDList = 
        lists:foldr(fun({Min, Max, SellReward}, Acc) ->
                            RoleIDList = filter_for_interval(Min, Max, RankRoleList),
                            [{SellReward, RoleIDList}|Acc]
                    end, [], RankRewardList),
    lists:foreach(fun({SellReward, RoleIDList}) ->
                          lists:foreach(
                            fun({RoleID, Rank, Score}) ->
                                    mail_server:send_sys_mail(RoleID, ?MAIL_3V3_REWARD, [Rank, Score], "", SellReward)
                            end, RoleIDList)
                  end, RewardRoleIDList),
    lists:foreach(fun(#team_pk_rank{roleID=RoleID, rank=Rank}) ->
                        case role_lib:is_online(RoleID) of
                            true ->
                                role_task:send_dispach(RoleID, {dispach_task,role_3v3_rank_change,Rank});
                            _ ->
                                role_task_trigger:offline_team_pk_rank_change(RoleID,Rank)
                        end
                  end, RankRoleList).

filter_for_interval(Min, Max, RankRoleList) ->
    lists:foldr(fun(#team_pk_rank{roleID=RoleID, rank=Rank, score=Score}, Acc) ->
                        case Rank >= Min andalso Rank =< Max of
                            true ->
                                [{RoleID, Rank, Score}|Acc];
                            false ->
                                Acc
                        end
                end, [], RankRoleList).

cs_team_pk_info(RoleID, #state{status=Status, rankRoleList=RankRoleList}) ->
    case lists:keyfind(RoleID, #team_pk_rank.roleID, RankRoleList) of
        false ->
            Score = 0,
            Rank = 0;
        #team_pk_rank{score=Score,rank=Rank} ->
            next
    end,
    case Status of
        ?STATUS_OPEN ->
            {ok, erlang:get(?next_close_time), Rank, Score};
        ?STATUS_CLOSE ->
            {ok, erlang:get(?next_open_time), lists:sublist(RankRoleList, 3), Rank, Score}
    end.


update_rank_and_record(RoleInfo, OtherDeadCount, NewTeamRecord, DelReplayUIDList, State) ->
    #state{rankRoleList=RankRoleList, recordList=RecordList}=State,
    {NewRankRoleList,NewRank,OldRank} = gen_new_rank_list(RoleInfo, OtherDeadCount, RankRoleList),
    case NewTeamRecord of
        ?undefined ->
            NewRecordList = RecordList;
        _ ->
            NewRecordList = lists:sublist([NewTeamRecord|RecordList], 20)
    end,
    case lists:keyfind(DelReplayUIDList, #team_record.replayUIDList, NewRecordList) of
        false ->
            RepalyIDList2 = string:join(lists:map(fun(DelReplayUID) -> erlang:integer_to_list(DelReplayUID) end, DelReplayUIDList), ","),
            erlang:spawn(fun() -> db_sql:del_fightReplayList(RepalyIDList2) end);
        _ ->
            next
    end,
    {ok, OldRank, NewRank, State#state{rankRoleList=NewRankRoleList, recordList=NewRecordList}}.

gen_new_rank_list(RoleInfo, OtherDeadCount, RankRoleList) ->
    #role{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,vipLevel=VipLevel,svipLevel=SVipLevel}=RoleInfo,
    case lists:keyfind(RoleID, #team_pk_rank.roleID, RankRoleList) of
        false ->
            RankRoleList2 = [#team_pk_rank{roleID=RoleID,score=OtherDeadCount,timestamp=util:now(),rank=0,fightPower=FightPower,
                                           isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,vip=role_lib:cacl_vip_info(VipLevel,SVipLevel)}|RankRoleList];
        #team_pk_rank{score=Score}=OldRankRole ->
            RankRoleList2 =
                lists:keyreplace(RoleID, #team_pk_rank.roleID, RankRoleList,
                                 OldRankRole#team_pk_rank{score=Score+OtherDeadCount,timestamp=util:now(),fightPower=FightPower,isMale=IsMale,
                                                          title=Title,head=Head,level=Level,roleName=RoleName})
    end,
    sort_and_return(RoleID, RankRoleList2).

%% 排名的代码独立出来,合服时需要
sort_and_return(RoleID, RankRoleList2) ->
    RankRoleList3 = lists:sort(fun(#team_pk_rank{score=ScoreA,timestamp=TimestampA}, #team_pk_rank{score=ScoreB,timestamp=TimestampB}) ->
                                       if
                                           ScoreA > ScoreB ->
                                               true;
                                           ScoreA =:= ScoreB ->
                                               if
                                                   TimestampA < TimestampB ->
                                                       true;
                                                   true ->
                                                       false
                                               end;
                                           true ->
                                               false
                                       end
                               end, RankRoleList2),
    {RankRoleList4,_,NewRank,OldRank} =
        lists:foldr(fun(#team_pk_rank{roleID=ElemRoleID,rank=ElemOldRank}=Elem, {Acc, AccRank,AccNewRank,AccOldRank}) ->
                            case ElemRoleID of
                                RoleID ->
                                    {[Elem#team_pk_rank{rank=AccRank}|Acc], AccRank-1,AccRank,ElemOldRank};
                                _ ->
                                    {[Elem#team_pk_rank{rank=AccRank}|Acc], AccRank-1,AccNewRank,AccOldRank}
                            end
                    end, {[],erlang:length(RankRoleList3),0,0}, RankRoleList3),
    {RankRoleList4,NewRank,OldRank}.

trans_rank_role_list(RankRoleList) ->
    lists:map(fun(#team_pk_rank{score=Score,roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,
                                head=Head,level=RoleLevel,roleName=RoleName,rank=Rank,vip=Vip}) ->
                      #p_team_member3{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,
                                      level=RoleLevel,roleName=RoleName,score=Score,rank=Rank,vip=Vip} 
              end, RankRoleList).

%%战报查询缓存
get_replay_record(ReplayUID)->
    case erlang:get({?team1v3ReplayRecord, ReplayUID}) of
        undefined ->
            case db_sql:get_fightReplay(ReplayUID) of
                []->
                    case ReplayUID of
                        0 ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 1};
                        _ ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 2}
                    end;
                Rec->
                    erlang:put({?team1v3ReplayRecord, ReplayUID}, Rec),
                    {Rec, 1}
            end;
        Cached ->
            {Cached, 1}
    end.

%% 清除所有的战报缓存
erase_replay_record() ->
    lists:foreach(fun({{?team1v3ReplayRecord, ReplayUID},_}) ->
                          erlang:erase({?team1v3ReplayRecord, ReplayUID});
                     (_) ->
                          next
                  end, erlang:get()).

%% 清除指定战报缓存
erase_replay_record(ReplayUIDList) ->
    lists:foreach(fun(ReplayUID) ->
                          erlang:erase({?team1v3ReplayRecord, ReplayUID})
                  end, ReplayUIDList).

get_team_fight_replay(RoleID, ReplayUIDList, RecordList) ->
    case lists:keyfind(ReplayUIDList, #team_record.replayUIDList, RecordList) of
        false ->
            ?unicast(RoleID, #sc_team_fight_replay{result=2,fightInfoList=[]});
        #team_record{selfList=PSelfList,otherList=POtherList} ->
            {Result, FightInfoList} = get_team_fight_replay(ReplayUIDList, []),
            ?unicast(RoleID, #sc_team_fight_replay{result=Result,fightInfoList=FightInfoList,selfList=PSelfList,otherList=POtherList})
    end.

get_team_fight_replay([], FightInfoList) ->
    {1, lists:reverse(FightInfoList)};
get_team_fight_replay([ReplayUID|ReplayUIDList], FightInfoList) ->
    case get_replay_record(ReplayUID) of
        {FightInfo, 1} ->
            get_team_fight_replay(ReplayUIDList, [FightInfo|FightInfoList]);
        {_FightInfo, 2} ->
            {2, []}
    end.

trans2precord(List) ->
    lists:map(fun(#team_record{isWin=IsWin ,timestamp=Timestamp ,roleName= RoleName ,godName= GodName ,replayUIDList=ReplayUIDList }) ->
                      #p_team_record{isWin=IsWin ,timestamp=Timestamp ,roleName= RoleName ,godName= GodName ,replayUIDList=ReplayUIDList}
              end, List).


gen_team_data_as(RoleID, #state{status=Status}=State) ->
    case Status of
        ?STATUS_OPEN ->
            random:seed(erlang:now()),
            {NewSelfRoleIDList, NewOtherRoleIDList} = gen_team_data(RoleID, State);
        ?STATUS_CLOSE ->
            NewSelfRoleIDList = [],
            NewOtherRoleIDList = []
    end,
    role_lib:send_server(RoleID, {ok, {NewSelfRoleIDList, NewOtherRoleIDList}}).

get_next_timestamp(Time) ->
    {CurDate, CurTime} = erlang:localtime(),
    case Time > CurTime of
        false ->
            util:datetime_to_seconds({CurDate, Time}) + ?ONE_DAY_SECONDS;
        true ->
            util:datetime_to_seconds({CurDate, Time})
    end.

get_send_after_seconds(Time) ->
    NextTimestamp = get_next_timestamp(Time),
    CurTimestamp = util:now(),
    NextTimestamp - CurTimestamp.

del_replay() ->
    erlang:spawn(fun() -> db_sql:del_spec_type_and_time_replay(?REPLAY_TYPE_3V3, 1) end).

%% 更新v400数据格式
update_team_pk_rank_format(OldList)->
    update_team_pk_rank_format(OldList,[]).

update_team_pk_rank_format([],AccList)->
    AccList;
update_team_pk_rank_format([{team_pk_rank,RoleID,B,C,D,E,F,G,H,I,J}|T],AccList)->
    RolePub = role_lib:get_rolePublic(RoleID),
    update_team_pk_rank_format(T,[{team_pk_rank,RoleID,B,C,D,E,F,G,H,I,J
                                  ,role_lib:cacl_vip_info(RolePub#rolePublic.viplevel, RolePub#rolePublic.svipLevel)}|AccList]);
update_team_pk_rank_format([TeamPkRank|T],AccList)->
    update_team_pk_rank_format(T,[TeamPkRank|AccList]).

fix_after_merge(State) ->
    case file:read_file_info("./merge.touch") of
        {ok, _} ->
            %% 调整开始时间
            OpenTime = data_team_pk:get(open_time), 
            {{M,Y,D},_} = erlang:localtime(),
            OpenTimestamp = util:datetime_to_seconds({{M,Y,D+1},OpenTime}),
            LeftTime = OpenTimestamp - util:now(),
            erlang:send_after(LeftTime*1000,self(),set_status_open),
            erlang:put(?next_open_time,OpenTimestamp),
            NewState = State#state{status=?STATUS_CLOSE},

            %% 更新排行榜中的名称(可能有重名的,合服时不方便处理)
            #state{rankRoleList=RankRoleList} = NewState,
            NewRankRoleList = 
                lists:foldl(fun(#team_pk_rank{roleID=RoleID}=Ranker, Acc) ->
                                RoleName = db_sql:get_role_name(RoleID),
                                NewRanker = Ranker#team_pk_rank{roleName=RoleName},
                                [NewRanker|Acc]
                            end, [], RankRoleList),
            case State#state.status of
                ?STATUS_OPEN ->
                    erlang:send_after(1000, self(), {send_reward, lists:reverse(NewRankRoleList)});
                _ ->
                    ignore
            end,
            NewState2 = NewState#state{rankRoleList=lists:reverse(NewRankRoleList), recordList=[]},
            ?ERR("~w fix after merge, next open time is:~p, open left time is:~p.~n", [?MODULE, OpenTimestamp, LeftTime]),
            {true, NewState2};
        _ ->
            false
    end.

test_set_open()->
    erlang:send(team_pk_server,set_status_open).
