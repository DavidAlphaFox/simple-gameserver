%% 华丽大赛server by dongquan
-module(race_server).
-behaviour(gen_server).
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_race.hrl").
-include("def_mail.hrl").


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
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

get_champion() ->
    gen_server:call(?MODULE, get_champion).

update_role_info(RoleInfo) ->
    case erlang:is_record(RoleInfo, role) of
        true ->
            erlang:send(?MODULE, {update_role_info, RoleInfo});
        false ->
            ignore     
    end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {session_id=0, status=0, next_status_timestamp=0,
                sign_list=[], fighter_list=[], win_list=[], left_round=0, left_fight_times=0,
                bc_list=[], champion_name= <<"">>, champion_id=0, group_win_list=[], pos_info_list=[],
                group1=[], group2=[], group3=[], group4=[], group5=[], group6=[], group7=[], group8=[], group_id}).

-record(role_race, {role_info, group_id=0, guess_role_id=0,guess_coin=0}).
-record(role_info, {roleID=0,roleName= <<"">>,isMale=true,title=0,fightPower=0,head=0,level=0}).

-define(FIGHT_LOOP_INTERVAL, 1500).
-define(DUMP_INTERVAL, (1000 * 60 * 5)).
-define(GROUP_NUM, 8).
-define(ONE_WEEK_SECONDS, (7 * 24 * 3600)).

-define(is_auto, is_auto).
-define(role_race, role_race).
-define(cur_history, cur_history).
-define(replayRecord, replayRecord).
-define(fight_loop_msg, fight_loop_msg).

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
    case db_sql:get_etc(?DB_ETC_KEY_RACE) of
        [{state,State}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            State = #state{}
    end,
    case fix_after_merge(State) of 
        {true, NewState} ->
            next;
        _ ->
        case erlang:get(?fight_loop_msg) of
            ?undefined ->
                {noreply, NewState} = schedule_to_next(State);
            Msg ->
                erlang:send(erlang:self(), Msg),
                NewState = State
        end
    end,
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
	erlang:send_after(?DUMP_INTERVAL, self(),do_hibernate),
    {ok, NewState}.


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
handle_call(get_champion, _From, #state{champion_id=ChampionID, champion_name=ChampionName}=State)->
    {reply, {ChampionID, ChampionName}, State};
handle_call(Request, From, State) ->
    ?ERR("未知的Request:~w, From:~w", [Request, From]),
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
terminate(Reason, State) ->
    ?ERR("~w terminate, reason:~w", [?MODULE, Reason]),
    do_persist(State),
    ok.


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

do_handle_info({inet_reply,_,_}, State) ->
    {noreply, State};
do_handle_info({update_role_info, #role{roleID=RoleID}=RoleInfo}, State) ->
    update_role_race(RoleID, RoleInfo),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_info{}}, State) ->
    do_cs_race_info(RoleID, State),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_guess_info{}}, State) ->
    do_cs_race_guess_info(RoleID),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_enter{}}, State) ->
    do_enter_channel(RoleID, State);
do_handle_info({client_msg, RoleID, #cs_race_leave{}}, State) ->
    do_leave_channel(RoleID, State);
do_handle_info({client_msg, RoleID, #cs_race_replay{replay_id=ReplayID}}, State) ->
    do_cs_race_replay(RoleID, ReplayID),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_history{round=Round, group_id=GroupID, start=Start, num=Num}}, State) ->
    do_cs_race_history(RoleID, Round, GroupID, Start, Num),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_self_history{}}, State) ->
    do_cs_race_self_history(RoleID),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_fight_list{group_id=GroupID}}, State) ->
    do_cs_race_fight_list(RoleID, GroupID, State),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_pos_history{pos=Pos}}, State) ->
    do_cs_race_pos_history(RoleID, Pos, State),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_race_auto_unsign{}}, State) ->
    do_cs_race_auto_unsign(RoleID),
    {noreply, State};
do_handle_info({do_race_guess, RoleID, GuessCoin, GuessRoleID}, #state{status=Status,group_win_list=GroupWinList}=State) ->
    do_race_guess(RoleID, GuessCoin, GuessRoleID, Status, GroupWinList),
    {noreply, State};
do_handle_info({do_fight_loop, LimitRound, GroupID}, State) ->
    do_fight_loop(State, LimitRound, GroupID);
do_handle_info({role_offline, RoleID}, State) ->
    do_leave_channel(RoleID, State);
do_handle_info({client_msg, RoleID, #cs_race_is_open{}}, #state{session_id=SessionID,status=Status}=State) ->
    case SessionID =:= 0 andalso Status =:= 0 of
        false ->
            ?unicast2(RoleID, #sc_race_is_open{is_open=true});
        true ->
            ?unicast2(RoleID, #sc_race_is_open{is_open=false})
    end,
    {noreply, State};
do_handle_info({do_role_sign, RoleID, RoleInfo}, State) ->
    do_role_sign(RoleID, RoleInfo, State);
do_handle_info({do_role_sign_auto, RoleID, RoleInfo}, State) ->
    do_role_sign_auto(RoleID, RoleInfo, State);
%% ---------------------------------------------活动各个阶段需要做的消息处理 START------------------------------------------------------------------
do_handle_info({change_to_this_status, NewStatus}, #state{status=NowStatus}=State) ->
    ?ERR("NewStatus:~w, NowStatus:~w", [NewStatus, NowStatus]),
    case NewStatus =:= NowStatus + 1 orelse (NowStatus =:= ?STATUS_FINAL_FIGHT andalso NewStatus =:= ?STATUS_NOT_OPEN) of
        true ->
            case NewStatus of
                ?STATUS_NOT_OPEN ->
                    ?ERR("关闭大赛", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_SIGN ->
                    ?ERR("开始报名", []),
%%                    erase_role_race(),
%%                     {ok, SignList} = auto_sign_roles(),
%%                    lists:foreach(fun(Val) ->
%%                                          erase_cur_round_history(Val) 
%%                                  end, lists:seq(0, 8)),
%%                    erase_replay_record(),
                    erlang:erase(),
                    db_sql:del_replay_with_type(?REPLAY_TYPE_RACE),
                    case State#state.session_id of
                        0 ->
                            catch broadcast_server:bc(#sc_race_is_open{is_open=true});
                        _ ->
                            next
                    end,
                    schedule_to_next(State#state{session_id = State#state.session_id + 1,sign_list=[],
                                                 status=NewStatus, group_win_list=[], pos_info_list=[],
                                                 group1=[], group2=[], group3=[], group4=[],
                                                 group5=[], group6=[], group7=[], group8=[]});
                ?STATUS_WAIT_PRE_FIHGT1 ->
                    ?ERR("等待小组赛1", []),
                    {ok, NewState} = do_divide_group(State#state{status=NewStatus}),
                    schedule_to_next(NewState);
                ?STATUS_PRE_FIGHT1 ->
                    ?ERR("开始预选赛1", []),
                    do_pre_fight(State#state{status=NewStatus}, 1);
                ?STATUS_WAIT_PRE_FIHGT2 ->
                    ?ERR("等待小组赛2", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_PRE_FIGHT2 ->
                    ?ERR("开始预选赛2", []),
                    do_pre_fight(State#state{status=NewStatus}, 2);
                ?STATUS_WAIT_PRE_FIHGT3 ->
                    ?ERR("等待小组赛3", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_PRE_FIGHT3 ->
                    ?ERR("开始预选赛3", []),
                    do_pre_fight(State#state{status=NewStatus}, 3);
                ?STATUS_WAIT_PRE_FIHGT4 ->
                    ?ERR("等待小组赛4", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_PRE_FIGHT4 ->
                    ?ERR("开始预选赛4", []),
                    do_pre_fight(State#state{status=NewStatus}, 4);
                ?STATUS_WAIT_PRE_FIHGT5 ->
                    ?ERR("等待小组赛5", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_PRE_FIGHT5 ->
                    ?ERR("开始预选赛5", []),
                    do_pre_fight(State#state{status=NewStatus}, 5);
                ?STATUS_WAIT_PRE_FIHGT6 ->
                    ?ERR("等待小组赛6", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_PRE_FIGHT6 ->
                    ?ERR("开始预选赛6", []),
                    do_pre_fight(State#state{status=NewStatus}, 6);
                ?STATUS_WAIT_PRE_FIHGT7 ->
                    ?ERR("等待小组赛7", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_PRE_FIGHT7 ->
                    ?ERR("开始预选赛7", []),
                    do_pre_fight(State#state{status=NewStatus}, 7);
                ?STATUS_WAIT_PRE_FIHGT8 ->
                    ?ERR("等待小组赛8", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_PRE_FIGHT8 ->
                    ?ERR("开始预选赛8", []),
                    do_pre_fight(State#state{status=NewStatus}, 8);
                ?STATUS_WAIT_FOUR_FIGHT ->
                    ?ERR("等待选四强", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_FOUR_FIGHT ->
                    ?ERR("选四强", []),
                    do_four_fight(State#state{status=NewStatus});
                ?STATUS_WAIT_TWO_FIGHT ->
                    ?ERR("等待半决赛", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_TWO_FIGHT ->
                    ?ERR("半决赛", []),
                    do_two_fight(State#state{status=NewStatus});
                ?STATUS_WAIT_FINAL_FIGHT ->
                    ?ERR("等待决赛", []),
                    schedule_to_next(State#state{status=NewStatus});
                ?STATUS_FINAL_FIGHT ->
                    ?ERR("决赛", []),
                    do_final_fight(State#state{status=NewStatus})
            end;
        false ->
            {noreply, State}
    end;
%% -------------------------------------------活动各个阶段需要做的消息处理 END----------------------------------------------------------------------------
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    {ok, NewState} = update_group(State),
    do_persist(NewState),
    case erlang:time() of
        {5,_,_} ->
            erase_replay_record();
        _ ->
            next
    end,
    {noreply, NewState};
do_handle_info(change_to_next_status, State) ->
    change_to_next_status(State),
    ?ERR("change_to_next_status", []),
    {noreply, State};
do_handle_info(send_guess_reward, State) ->
    send_guess_reward(0,<<"小勇哥">>),
    {noreply, State};
do_handle_info({update_role_name,RoleID,Name}, #state{champion_id=ChampionID}=State) ->
    case get_role_race(RoleID) of
        ?undefined ->
            ignore;
        #role_race{role_info=RoleInfo}=Info ->
            NewRoleInfo = RoleInfo#role_info{roleName=Name},
            set_role_race(RoleID, Info#role_race{role_info=NewRoleInfo})
    end,
    NewState = 
        case RoleID =:= ChampionID of
            true ->
                State#state{champion_name=Name};
            _ ->
                State
        end,        
    {noreply, NewState};
do_handle_info({test_set_champion, RoleID}, State) ->
    case db_sql:get_roleInfo(RoleID) of
        #role{roleName=RoleName} ->
            {noreply, State#state{champion_name=RoleName, champion_id=RoleID}};
        _ ->
            {noreply, State}
    end;

do_handle_info(Info, State) ->
    ?ERR("未知的Info:~w", [Info]),
    {noreply, State}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_RACE,Info2).

is_persist({{?replayRecord, _}, _}) ->
    false;
is_persist(_) ->
    true.

change_to_next_status(State) ->
    #state{status=Status} = State,
    case Status >= ?STATUS_FINAL_FIGHT of
        true ->
            NewStatus = ?STATUS_NOT_OPEN;
        false ->
            NewStatus = Status + 1
    end,
    erlang:send(erlang:self(), {change_to_this_status, NewStatus}).

schedule_to_next(#state{status=NowStatus}=State) ->
    {NextStatus, LeftSeconds} = next_status_and_left_seconds(NowStatus),
    erlang:send_after(LeftSeconds * 1000, erlang:self(), {change_to_this_status, NextStatus}),
    NewState = State#state{next_status_timestamp=util:now() + LeftSeconds},
    ?ERR("NowStatus:~w,NextStatus:~w,~w", [NowStatus, NextStatus, NewState#state.next_status_timestamp]),
    catch bc_state(NewState),
    {noreply, NewState}.

next_status_and_left_seconds(NowStatus) ->
    NextStatus =
        case NowStatus of
            ?STATUS_FINAL_FIGHT ->
                ?STATUS_NOT_OPEN;
            _ ->
                NowStatus + 1
        end,
    {Date, Time} = erlang:localtime(),

    {N, StartTime} = data_race:get({start_time, NextStatus}),
    case NowStatus of
		?STATUS_NOT_OPEN ->
			AddSeconds = ?ONE_DAY_SECONDS * (N - 1);
        ?STATUS_SIGN ->
            case Time < StartTime of
                true ->
                    AddSeconds = ?ONE_DAY_SECONDS;
                false ->
                    AddSeconds = 0
            end;
        _ ->
            AddSeconds = 0
    end,
    case Time < StartTime of
        true ->
            LeftSeconds =  util:datetime_to_seconds({Date, StartTime}) -  util:datetime_to_seconds({Date, Time});
        false ->
            LeftSeconds =  util:datetime_to_seconds({Date, StartTime}) +?ONE_DAY_SECONDS -  util:datetime_to_seconds({Date, Time})
    end,
    {NextStatus, LeftSeconds + AddSeconds}.

get_role_name(RoleID) ->
    case get_role_race(RoleID) of
        ?undefined ->
            <<"">>;
        #role_race{role_info=RoleInfo} ->
            case RoleInfo of
                #role_info{roleName=RoleName} ->
                    RoleName;
                _ ->
                    <<"">>
            end
    end.

get_role_race(0) ->
    #role_race{role_info=#role_info{roleName= <<"">>, isMale=true, title=0}};
get_role_race(RoleID) ->
    erlang:get({?role_race, RoleID}).

set_role_race(RoleID, RoleRace) ->
    erlang:put({?role_race, RoleID}, RoleRace).

update_role_race(RoleID, #role{roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}=RoleInfo) when erlang:is_record(RoleInfo, role)->
    case get_role_race(RoleID) of
        RoleRace when erlang:is_record(RoleRace, role_race) ->
            set_role_race(RoleID, RoleRace#role_race{role_info=#role_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}});
        _ ->
            ignore
    end.

get_is_auto(RoleID) ->
    case erlang:get({?is_auto, RoleID}) of
        IsAuto when erlang:is_boolean(IsAuto) ->
            IsAuto;
        _ ->
            false
    end.

set_is_auto(RoleID, IsAuto) when erlang:is_boolean(IsAuto) ->
    erlang:put({?is_auto, RoleID}, IsAuto).

update_role_group_id(RoleID, GroupID) ->
    RoleRace = erlang:get({?role_race, RoleID}),
    erlang:put({?role_race, RoleID}, RoleRace#role_race{group_id=GroupID}).

erase_role_race() ->
    lists:foreach(fun(Val) ->
                         case Val of
                             {{?role_race, RoleID}, _} ->
                                 erlang:erase({?role_race, RoleID});
                             _ ->
                                 next
                         end
                  end, erlang:get()).

do_role_sign_auto(RoleID, RoleInfo, State) ->
%%     case catch check_can_sign(RoleID, RoleInfo, State) of
%%         {true, NewState}->
%%             #role{roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level} = RoleInfo,
%%             case get_role_race(RoleID) of
%%                 ?undefined ->
%%                     set_role_race(RoleID, #role_race{role_info=#role_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}});
%%                 OldRoleRace ->
%%                     set_role_race(RoleID, OldRoleRace#role_race{role_info=#role_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}})
%%             end,
%%             set_is_auto(RoleID, true),
%%             ?unicast2(RoleID, #sc_race_auto_sign{reason_code=0}),
%%             ?unicast2(RoleID, #sc_race_sign{reason_code=0}),
%% 			behavior_race_sign:log( RoleID);
%%         {false, ReasonCode} ->
%%             case ReasonCode =:= ?REASON_CODE_SIGN_NUM_IS_MAX orelse
%%                       ReasonCode =:= ?REASON_CODE_ROLE_ALREADY_SIGN of
%%                 true ->
%%                     set_is_auto(RoleID, true),
                    ?unicast2(RoleID, #sc_race_auto_sign{reason_code=0}),
%%                     ?unicast2(RoleID, #sc_race_sign{reason_code=ReasonCode});
%%                 false ->
%%                     ?unicast2(RoleID, #sc_race_auto_sign{reason_code=ReasonCode})
%%             end,
%%             NewState = State
%%     end,
    {noreply, State}.

do_role_sign(RoleID, RoleInfo, State) ->
%%     case catch check_can_sign(RoleID, RoleInfo, State) of
%%         {true, NewState}->
%%             #role{roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level} = RoleInfo,
%%             case get_role_race(RoleID) of
%%                 ?undefined ->
%%                     set_role_race(RoleID, #role_race{role_info=#role_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}});
%%                 OldRoleRace ->
%%                     set_role_race(RoleID, OldRoleRace#role_race{role_info=#role_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}})
%%             end,
%% 			?CATCH(role_task:send_dispach(RoleID, {dispach_task,role_sign_race_times})),
            ?unicast2(RoleID, #sc_race_sign{reason_code=0}),
%% 			behavior_race_sign:log( RoleID);
%%         {false, ReasonCode} ->
%%             ?unicast2(RoleID, #sc_race_sign{reason_code=ReasonCode}),
%%             NewState = State
%%     end,
    {noreply, State}.

check_can_sign(RoleID, RoleInfo, State) ->
    case State#state.status =:= ?STATUS_SIGN of
        true ->
            next;
        false ->
            erlang:throw({false, ?REASON_CODE_NOT_SIGN_TIME})
    end,
    case RoleInfo of
        #role{} ->
            next;
        _ ->
            erlang:throw({false, ?REASON_CODE_CAN_NOT_GET_ROLE_INFO_FOR_RACE_SIGN})
    end,
    case RoleInfo#role.level >= data_race:get(role_level_limit) of
        true ->
            next;
        false ->
            erlang:throw({false, ?REASON_CODE_ROLE_LEVEL_NOT_ENOUGH})
    end,
    MaxSignNum = data_race:get(sign_num_limit),
    case erlang:length(State#state.sign_list) < MaxSignNum of
        true ->
            next;
        false ->
            erlang:throw({false, ?REASON_CODE_SIGN_NUM_IS_MAX})
    end,
    case lists:member(RoleID, State#state.sign_list) of
        false ->
            NewState = State#state{sign_list=[RoleID|State#state.sign_list]},
            {true, NewState};
        true ->
            erlang:throw({false, ?REASON_CODE_ROLE_ALREADY_SIGN})
    end.

do_four_fight(State) ->
    {LimitRound, _} = get_round_and_times(?FIRST_FOUR),
    do_start_fight(State, LimitRound, 0).

do_two_fight(State) ->
    {ok, NewState} = update_fighters_for_two_fight(State),
    continue_fight(?FIRST_TWO, NewState),
    {noreply, NewState}.

update_fighters_for_two_fight(State) ->
    [A, B, C, D] = State#state.fighter_list,
    {ok, State#state{fighter_list=[A, C, B, D]}}.

do_final_fight(State) ->
    continue_fight(?FIRST_ONE, State),
    {noreply, State}.

continue_fight(FighterNum, State) ->
    catch bc_state(State),
    FighterList = State#state.fighter_list,
    {LimitRound, _} = get_round_and_times(FighterNum),
    case FighterList =/= [] andalso erlang:length(FighterList) > FighterNum of
        true ->
            erlang:send(erlang:self(), {do_fight_loop, LimitRound, 0}),
            erlang:put(?fight_loop_msg, {do_fight_loop, LimitRound, 0});
        false ->
            next
    end.

do_pre_fight(State, GroupID) ->
    do_start_fight(State, 0, GroupID).

do_start_fight(State, LimitRound, GroupID) ->
    {FighterNum, FighterList} = get_fighter_list(State, GroupID),
    {RoundNum, CurRoundTimes} = get_round_and_times(FighterNum),
    NewState = State#state{fighter_list=FighterList,left_fight_times=CurRoundTimes,left_round=RoundNum,win_list=[]},
    catch bc_state(NewState),
    case RoundNum > LimitRound of
        true ->
            erlang:send(erlang:self(), {do_fight_loop, LimitRound, GroupID}),
            erlang:put(?fight_loop_msg, {do_fight_loop, LimitRound, GroupID}),
            {noreply, NewState};
        false ->
            do_no_fight(NewState, GroupID, LimitRound)
    end.

get_group_list(State, GroupID) ->
    case GroupID of
        1 ->
            State#state.group1;
        2 ->
            State#state.group2;
        3 ->
            State#state.group3;
        4 ->
            State#state.group4;
        5 ->
            State#state.group5;
        6 ->
            State#state.group6;
        7 ->
            State#state.group7;
        8 ->
            State#state.group8
    end.

set_group_list(State, GroupID, List) ->
    case GroupID of
        1 ->
            State#state{group1=List};
        2 ->
            State#state{group2=List};
        3 ->
            State#state{group3=List};
        4 ->
            State#state{group4=List};
        5 ->
            State#state{group5=List};
        6 ->
            State#state{group6=List};
        7 ->
            State#state{group7=List};
        8 ->
            State#state{group8=List}
    end.

auto_sign_roles() ->
    RoleInfoList =
        lists:foldr(
          fun({{?is_auto, RoleID}, true}, Acc) ->
                  case get_role_race(RoleID) of
                      #role_race{role_info=RoleInfo} ->
                          [RoleInfo|Acc];
                      _ ->
                          case db_sql:get_roleInfo(RoleID) of
                              #role{roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level} ->
                                  [#role_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}|Acc];
                              _ ->
                                  Acc
                          end
                  end;
             (_, Acc) ->
                  Acc
          end, [], erlang:get()),
    SortRoleInfoList =
        lists:sort(fun(#role_info{fightPower=FightPower1}, #role_info{fightPower=FightPower2}) ->
                           FightPower1 > FightPower2
                   end, RoleInfoList),
    SortRoleInfoList2 = lists:sublist(SortRoleInfoList, data_race:get(sign_num_limit)),
    SignList = [RoleID||#role_info{roleID=RoleID}<-SortRoleInfoList2],
    erase_role_race(),
    lists:foreach(fun(#role_info{roleID=RoleID}=RoleInfo) ->
                          set_role_race(RoleID, #role_race{role_info=RoleInfo})
                  end, SortRoleInfoList2),
    {ok, SignList}.
 
do_divide_group(State) ->
    #state{sign_list=SignList} = State,
    RoleInfoList =
        lists:foldr(
          fun(SignRoleID, Acc) ->
                  case get_role_race(SignRoleID) of
                      #role_race{role_info=RoleInfo} ->
                          [RoleInfo|Acc];
                      _ ->
                          Acc
                  end
          end, [], SignList),
    SortRoleInfoList =
        lists:sort(fun(#role_info{fightPower=FightPower1}, #role_info{fightPower=FightPower2}) ->
                           FightPower1 > FightPower2
                   end, RoleInfoList),
    SortSignList = [RoleID||#role_info{roleID=RoleID}<-SortRoleInfoList],
    {NewState, _} =
        lists:foldl(
          fun(SignRoleID, {AccState, [GroupID|AccRandomGroupList]}) ->
                  NewAccState = set_group_list(AccState, GroupID, [SignRoleID|get_group_list(AccState, GroupID)]),
                  NewAccRandomGroupList =
                      case AccRandomGroupList of
                          [] ->
                              random_list(lists:seq(1, 8), 8, []);
                          _ ->
                              AccRandomGroupList
                      end,
                  update_role_group_id(SignRoleID, GroupID),
                  {NewAccState, NewAccRandomGroupList}
          end, {State, random_list(lists:seq(1, 8), 8, [])}, SortSignList),
    {ok, NewState}.

get_fighter_list(State, GroupID) ->
    case GroupID =:= 0 of
        false ->
            GroupRoleIDList = get_group_list(State, GroupID),
            get_fighter_list2(GroupRoleIDList);
        true ->
            GroupWinList = State#state.group_win_list,
            {erlang:length(GroupWinList), lists:reverse(GroupWinList)}
    end.

get_fighter_list2(RoleIDList) ->
    RoleInfoList =
        lists:foldr(
          fun(RoleID, Acc) ->
                  case get_role_race(RoleID) of
                      #role_race{role_info=RoleInfo} ->
                          [RoleInfo|Acc];
                      _ ->
                          Acc
                  end
          end, [], RoleIDList),
    SortRoleInfoList = random_list(RoleInfoList, erlang:length(RoleInfoList), []),
%%         lists:sort(fun(#role_info{fightPower=FightPower1}, #role_info{fightPower=FightPower2}) ->
%%                            FightPower1 > FightPower2
%%                    end, RoleInfoList),
    SortRoleIDList = [RoleID||#role_info{roleID=RoleID}<-SortRoleInfoList],
    Len = erlang:length(SortRoleIDList),
    {FighterList, _} =
        lists:foldl(fun(_, {AccFighterList, AccSortRoleIDList}) ->
                            case erlang:length(AccSortRoleIDList) >= 2 of
                                true ->
                                    RoleID1 = lists:nth(1, AccSortRoleIDList),
                                    RoleID2 = lists:last(AccSortRoleIDList),
                                    NewAccSortRoleIDList = lists:delete(RoleID2, lists:delete(RoleID1, AccSortRoleIDList)),
                                    {[RoleID2, RoleID1|AccFighterList], NewAccSortRoleIDList};
                                false ->
                                    {AccSortRoleIDList ++ AccFighterList, []}
                            end
                    end, {[], SortRoleIDList}, lists:duplicate(Len div 2 + Len rem 2, 0)),
    {Len, lists:reverse(FighterList)}.

random_list([], 0, RandomList) ->
    RandomList;
random_list(List, Len, AccRandomList) ->
    Elem = lists:nth(random:uniform(Len), List),
    random_list(lists:delete(Elem, List), Len - 1, [Elem|AccRandomList]).

do_fight(AttackerID,DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender,AtkTalent,DefTalent,TrSpecialA,TrSpecialD, GroupID,SkinInfoA,SkinInfoB) ->
    LimitMaxWinCount =
        case GroupID > 0 of
            true ->
                1;
            false ->
                3
        end,
    if
        AttackerID =/= 0 andalso DefenderID =/= 0 ->
            do_fight2(AttackerID,DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender,AtkTalent,DefTalent,TrSpecialA,TrSpecialD, LimitMaxWinCount, [], 0, 0, [], true,SkinInfoA,SkinInfoB);
        AttackerID =:= 0 andalso DefenderID =:= 0 ->
            {[], LimitMaxWinCount, 0, gen_win_list(LimitMaxWinCount)};
        true ->
            if
                AttackerID =:= 0 ->
                    {[], 0, LimitMaxWinCount, gen_lose_list(LimitMaxWinCount)};
                true ->
                    {[], LimitMaxWinCount, 0, gen_win_list(LimitMaxWinCount)}
            end
    end.

gen_lose_list(Count) ->
    {List, _} =
        lists:foldl(fun(_, {Acc1, Acc2}) ->
                            case Acc2 of
                                0 ->
                                    {[false|Acc1], 1};
                                1 ->
                                    {[true|Acc1], 0}
                            end
                    end, {[], 0}, lists:duplicate(Count, nil)),
    lists:reverse(List).

gen_win_list(Count) ->
    {List, _} =
        lists:foldl(fun(_, {Acc1, Acc2}) ->
                            case Acc2 of
                                0 ->
                                    {[true|Acc1], 1};
                                1 ->
                                    {[false|Acc1], 0}
                            end
                    end, {[], 0}, lists:duplicate(Count, nil)),
    lists:reverse(List).

do_fight2(AttackerID,DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender, AtkTalent, DefTalent,TrSpecialA,TrSpecialD, LimitMaxWinCount, ReplayUIDList, AtkWinCount, DefWinCount, WinLoseList, true,SkinInfoA,SkinInfoB) ->
    ?ERR("华丽大赛使用了新版本，此处应该不会调用，故传奇装备的加成传递为默认值 ~n"),
    case catch role_fight:new(AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender, AtkTalent, DefTalent,TrSpecialA,TrSpecialD,SkinInfoA,SkinInfoB,[],[]) of
        {IsWin, FightRecord0, _} ->
            FighterList2 = role_data:get_FighterList_with_effect(AttackerID,DefenderID, FightRecord0#sc_fight_request.fighterList),
            FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
            ReplayUID = tk_id:gen_replayUID(),
            catch db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_RACE);
        _ ->
            ReplayUID = 0,
            IsWin = true
    end,
    case IsWin of
        true ->
            NewAtkWinCount = AtkWinCount + 1,
            NewDefWinCount = DefWinCount;
        false ->
            NewAtkWinCount = AtkWinCount,
            NewDefWinCount = DefWinCount + 1
    end,
    case NewAtkWinCount >= LimitMaxWinCount orelse NewDefWinCount >= LimitMaxWinCount of
        true ->
            {lists:reverse([ReplayUID|ReplayUIDList]), NewAtkWinCount, NewDefWinCount, lists:reverse([IsWin|WinLoseList])};
        false ->
            do_fight2(AttackerID,DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender, AtkTalent, DefTalent,TrSpecialA,TrSpecialD, LimitMaxWinCount, [ReplayUID|ReplayUIDList], NewAtkWinCount, NewDefWinCount, [IsWin|WinLoseList], false,SkinInfoA,SkinInfoB)
    end;
do_fight2(AttackerID,DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender, AtkTalent, DefTalent,TrSpecialA,TrSpecialD, LimitMaxWinCount, ReplayUIDList, AtkWinCount, DefWinCount, WinLoseList, false,SkinInfoA,SkinInfoB) ->
    ?ERR("华丽大赛使用了新版本，此处应该不会调用，故传奇装备的加成传递为默认值 ~n"),
    case catch role_fight:new(DefenderList,AttackerList,LieuAddDefender,LieuAddAttacker,DefTalent,AtkTalent,TrSpecialD,TrSpecialA,SkinInfoB,SkinInfoA,[],[]) of
        {IsWin, FightRecord0, _} ->
            FighterList2 = role_data:get_FighterList_with_effect(AttackerID,DefenderID, FightRecord0#sc_fight_request.fighterList),
            FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
            ReplayUID = tk_id:gen_replayUID(),
            catch db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_RACE);
        _ ->
            ReplayUID = 0,
            IsWin = false
    end,
    case IsWin of
        true ->
            NewAtkWinCount = AtkWinCount,
            NewDefWinCount = DefWinCount + 1;
        false ->
            NewAtkWinCount = AtkWinCount + 1,
            NewDefWinCount = DefWinCount
    end,
    case NewAtkWinCount >= LimitMaxWinCount orelse NewDefWinCount >= LimitMaxWinCount of
        true ->
            {lists:reverse([ReplayUID|ReplayUIDList]), NewAtkWinCount, NewDefWinCount, lists:reverse([IsWin|WinLoseList])};
        false ->
            do_fight2(AttackerID,DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender, AtkTalent, DefTalent,TrSpecialA,TrSpecialD, LimitMaxWinCount, [ReplayUID|ReplayUIDList], NewAtkWinCount, NewDefWinCount, [IsWin|WinLoseList], true,SkinInfoA,SkinInfoB)
    end.

update_fight_power(AttackerID,DefenderID, FightPowerAtk, FightPowerDef) ->
    case AttackerID =:= 0 of
        false ->
            #role_race{role_info=RoleInfoAtk} = RoleRaceAtk = get_role_race(AttackerID),
            NewRoleInfoAtk = RoleInfoAtk#role_info{fightPower=FightPowerAtk},
            set_role_race(AttackerID, RoleRaceAtk#role_race{role_info=NewRoleInfoAtk});
        true ->
            next
    end,
    case DefenderID =:= 0 of
        false ->
            #role_race{role_info=RoleInfoDef} = RoleRaceDef = get_role_race(DefenderID),
            NewRoleInfoDef = RoleInfoDef#role_info{fightPower=FightPowerDef},
            set_role_race(DefenderID, RoleRaceDef#role_race{role_info=NewRoleInfoDef});
        true ->
            next
    end.

do_fight_loop(State, LimitRound, GroupID) ->
    #state{left_round=LeftRound,left_fight_times=LeftFightTimes, bc_list=BcList, fighter_list=FighterList, win_list=WinList, pos_info_list=OldPosInfoList} = State,
    [RoleIDA,RoleIDB|FighterList2] = FighterList,
    {FightListA,LieuAddA,TalentListA,TrSpecialA} = role_data:get_otherRoleFighter(RoleIDA),
    SkinInfoA = role_data:get_otherRoleSkinInfo(RoleIDA),
    SkinInfoB = role_data:get_otherRoleSkinInfo(RoleIDB),
    {FightListB,LieuAddB,TalentListB,TrSpecialB} = role_data:get_otherRoleFighter(RoleIDB),
    #role_race{role_info=#role_info{roleName=NameA, isMale=IsMaleA, title=TitleA, head=HeadA}} = get_role_race(RoleIDA),
    #role_race{role_info=#role_info{roleName=NameB, isMale=IsMaleB, title=TitleB, head=HeadB}} = get_role_race(RoleIDB),
    
    {AttackerID,DefenderID, AttackerList, DefenderList, AttackerName, DefenderName, 
        LieuAddAttacker, LieuAddDefender, FightPowerAtk, FightPowerDef, AtkTalent, DefTalent,AtkTrSpecial,DfdTrSpecial,AtkSkinInfo,DfdSkinInfo}
        = check_attack_and_defend(RoleIDA,RoleIDB,FightListA,FightListB, NameA,NameB, LieuAddA, LieuAddB, TalentListA, TalentListB,TrSpecialA,TrSpecialB,SkinInfoA,SkinInfoB),
    %% 顺便更新下战斗力的数据
    update_fight_power(AttackerID,DefenderID, FightPowerAtk, FightPowerDef),
    {ReplayUIDList, AtkWinCount, DefWinCount, WinLoseList} = do_fight(AttackerID,DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender,AtkTalent,DefTalent,AtkTrSpecial,DfdTrSpecial, GroupID,AtkSkinInfo,DfdSkinInfo),
    WaitTime = erlang:length(ReplayUIDList) * ?FIGHT_LOOP_INTERVAL,
    if AtkWinCount > DefWinCount ->
           WinnerID = AttackerID;
       true ->
           WinnerID = DefenderID
    end,
    case RoleIDA =:= AttackerID of
        true ->
            AtkIsMale = IsMaleA,
            AtkTitle = TitleA,
            AtkHead = HeadA,
            DefIsMale = IsMaleB,
            DefTitle = TitleB,
            DefHead = HeadB;
        false ->
            AtkIsMale = IsMaleB,
            AtkTitle = TitleB,
            AtkHead = HeadB,
            DefIsMale = IsMaleA,
            DefTitle = TitleA,
            DefHead = HeadA
    end,
    WinList2 = [WinnerID|WinList],
    FightBC = #p_race_rec{atk_name=AttackerName,def_name=DefenderName,replay_uid_list=ReplayUIDList,
                          atk_role_id=AttackerID, def_role_id=DefenderID, round=LeftRound, group_id=GroupID,
                          atk_fight_power=FightPowerAtk, def_fight_power=FightPowerDef, win_or_lose_list=WinLoseList,
                          atk_is_male=AtkIsMale, def_is_male=DefIsMale, atk_title=AtkTitle, def_title=DefTitle,
                          atk_head=AtkHead, def_head=DefHead},
    %% 广播新的战斗
    case AttackerID =/= 0 andalso DefenderID =/= 0 of
        true ->
            BcRecord = #sc_race_new_fight{new_fight=FightBC},
            bc(BcList, BcRecord);
        false ->
            next
    end,
    %% 记录战报
    set_cur_round_history(LeftRound, GroupID, [FightBC| get_cur_round_history(LeftRound, GroupID)]),
    case GroupID =:= 0 andalso LeftFightTimes =:= 1 of
        true ->
            PosInfoList = gen_pos_info_list(State#state.group_win_list);
        false ->
            case GroupID > 0 andalso LeftRound =:= 1 of
                true ->
                    NewPos = #p_race_pos{role_id=AttackerID,role_name=AttackerName,is_male=AtkIsMale,title=AtkTitle,head=AtkHead,pos=GroupID},
                    catch broadcast_server:bc(#sc_race_new_first{new_pos=NewPos}),
                    PosInfoList = gen_pos_info_list2([WinnerID|State#state.group_win_list]);
                false ->
                    PosInfoList = OldPosInfoList
            end
    end,
    %% 判断本轮是否结束
    if LeftRound =:= 1 ->
           erlang:erase(?fight_loop_msg),
           finish_fight(State#state{pos_info_list=PosInfoList}, GroupID);
       true ->
           if LeftFightTimes =:= 1 ->
                  NewState = get_fight_loop_new_state(State, lists:append(lists:reverse(WinList2), FighterList2), util:pow(LeftRound - 2), LeftRound-1, []);
              true ->
                  NewState = get_fight_loop_new_state(State, FighterList2, LeftFightTimes-1, LeftRound, WinList2)
           end,
           case loop_tick(NewState, LimitRound, GroupID, WaitTime) of
               stop ->
                   erlang:erase(?fight_loop_msg),
                   {noreply, NewState2} = schedule_to_next(NewState#state{pos_info_list=PosInfoList, status=NewState#state.status + 1}),
                   catch broadcast_server:bc(#sc_race_new_status{status=NewState2#state.status, timestamp=NewState2#state.next_status_timestamp}),
                   {noreply, NewState2};
               _ ->
                   {noreply, NewState#state{pos_info_list=PosInfoList}}
           end
    end.


get_fight_loop_new_state(State, FighterList, LeftFightTimes, LeftRound, WinList) ->
    State#state{fighter_list=FighterList,left_fight_times=LeftFightTimes,left_round=LeftRound,win_list=WinList}.

loop_tick(State, LimitRound, GroupID, WaitTime) ->
    case State#state.left_round =:= LimitRound of
        false ->
            loop_tick2({do_fight_loop, LimitRound, GroupID}, WaitTime);
        true ->
            stop
    end.

loop_tick2(Msg, 0) ->
    erlang:send(erlang:self(), Msg);
loop_tick2(Msg, WaitTime) ->
    erlang:send_after(WaitTime, erlang:self(), Msg).

do_no_fight(#state{status=Status}=State, GroupID, _LimitRound) ->
    case GroupID > 0 of
        true ->
            case State#state.fighter_list of
                [WinRoleID] ->
                    State2 = State#state{bc_list=[], group_win_list=[WinRoleID|State#state.group_win_list]};
                [] ->
                    State2 = State#state{bc_list=[], group_win_list=[0|State#state.group_win_list]}
            end,
            {noreply, State3} = schedule_to_next(State2#state{status=Status + 1}),
            PosInfoList = gen_pos_info_list2(State2#state.group_win_list),
            {noreply, State3#state{pos_info_list=PosInfoList}};
        false ->
            {noreply, State}
    end.

finish_fight(#state{status=Status}=State, GroupID) ->
    RankList = get_cur_rank_list(GroupID),
    case RankList of
        [{0, WinRoleID, WinRoleName, _WinRoleInfo}|_] ->
            next;
        _ ->
            WinRoleName = <<"">>,
            WinRoleID = 0
    end,
    ?ERR("GroupID:~w,WinRoleID:~w", [GroupID,WinRoleID]),
    case GroupID =:= 0 of
        false ->
            send_reward_group(RankList);
        true ->
            send_reward_last(RankList),
            send_guess_reward(WinRoleID,WinRoleName)
    end,
    case GroupID =:= 0 of
        false ->
            State2 = State#state{bc_list=[], group_win_list=[WinRoleID|State#state.group_win_list]};
        true ->
            State2 = State#state{bc_list=[], champion_name=WinRoleName, champion_id=WinRoleID},
            catch role_lib:send_every_server_msg({route, role_activity, #cs_activity_sign_emperor_info{}})
    end,
    case Status of
        ?STATUS_FINAL_FIGHT ->
            NewStatus = ?STATUS_NOT_OPEN;
        _ ->
            NewStatus = Status + 1
    end,
    {noreply, State3} = schedule_to_next(State2#state{status=NewStatus}),
    catch broadcast_server:bc(#sc_race_new_status{status=State3#state.status, timestamp=State3#state.next_status_timestamp}),
    {noreply, State3}.

to_win_lose_info(#p_race_rec{atk_role_id=AtkRoleID, atk_name=AtkName, def_role_id=DefRoleID, def_name=DefName, win_or_lose_list=WinLoseList}) ->
    Len = erlang:length(WinLoseList),
    {LenWin, _} = lists:foldl(fun(WinOrLose, {AccWin, AccCount}) ->
                                      case AccCount rem 2 of
                                          0 ->
                                              IsWin = WinOrLose;
                                          1 ->
                                              IsWin = not WinOrLose
                                      end,
                                      case IsWin of
                                          true ->
                                              {AccWin+1, AccCount+1};
                                          false ->
                                              {AccWin, AccCount+1}
                                      end
                              end, {0, 0}, WinLoseList),
    case LenWin >= Len - LenWin of
        true ->
            {AtkRoleID, AtkName, DefRoleID, DefName};
        false ->
            {DefRoleID, DefName, AtkRoleID, AtkName}
    end.

get_cur_rank_list(GroupID) ->
    RaceRecList =
        lists:foldr(fun(Round, Acc) ->
                            List = get_cur_round_history(Round, GroupID) ,
                            lists:append(List, Acc)
                    end, [], lists:seq(1, 10)),
    lists:foldr(fun(#p_race_rec{round=Round}=PRaceRec, Acc) ->
                        {WinRoleID, WinName, LoseRoleID, LoseName} = to_win_lose_info(PRaceRec),
                        case Round of
                            1 ->
                                #role_race{role_info=WinRoleInfo} = get_role_race(WinRoleID),
                                #role_race{role_info=LoseRoleInfo} = get_role_race(LoseRoleID),
                                [{0, WinRoleID, WinName, WinRoleInfo}, {Round, LoseRoleID, LoseName, LoseRoleInfo} |Acc];
                            _ ->
                                #role_race{role_info=LoseRoleInfo} = get_role_race(LoseRoleID),
                                [{Round, LoseRoleID, LoseName, LoseRoleInfo}|Acc]
                        end
                end, [], RaceRecList).

%% 小组赛的发奖处理
send_reward_group(RankList) ->
    {SignRewardList, FirstFourRewardList} =
        lists:foldr(fun({Round, RoleID, _RoleName, _RoleInfo}, {AccSignRewardList, AccFirstFourRewardList}) ->
                            case Round =< 2 of
                                true ->
                                    {AccSignRewardList, [RoleID|AccFirstFourRewardList]};
                                false ->
                                    {[RoleID|AccSignRewardList], AccFirstFourRewardList}
                             end
                    end, {[], []}, RankList),
    RewardSign = data_race:get(reward_sign),
    mail_server:send_role_id_list_sys_mail(SignRewardList, ?MAIL_RACE_GROUP_SIGN_REWARD, [], "", RewardSign),
    RewardGroupFirstFour = data_race:get(reward_group_first_four),
    mail_server:send_role_id_list_sys_mail(FirstFourRewardList, ?MAIL_RACE_GROUP_FIRST_FOUR_REWARD, [], "", RewardGroupFirstFour).

%% 正赛发奖
send_reward_last(RankList) ->
    lists:foreach(
      fun({Round, RoleID, _RoleName, _RoleInfo}) ->
                          case Round of
                              3 ->
                                  RewardFirstEight = data_race:get(reward_first_eight),
                                  mail_server:send_sys_mail(RoleID, ?MAIL_RACE_FIRST_EIGHT_REWARD, [], "", change_sell_reward_by_sex(RewardFirstEight,RoleID));
                              2 ->
                                  RewardFirstFour = data_race:get(reward_first_four),
                                  mail_server:send_sys_mail(RoleID, ?MAIL_RACE_FIRST_FOUR_REWARD, [], "", change_sell_reward_by_sex(RewardFirstFour,RoleID));
                              1 ->
                                  RewardSecond = data_race:get(reward_second),
                                  mail_server:send_sys_mail(RoleID, ?MAIL_RACE_SECOND_REWARD, [], "", change_sell_reward_by_sex(RewardSecond,RoleID));
                              0 ->
                                  RewardFirst = data_race:get(reward_first),
                                  mail_server:send_sys_mail(RoleID, ?MAIL_RACE_FIRST_REWARD, [], "", change_sell_reward_by_sex(RewardFirst,RoleID))
                          end
                  end, RankList).

%% 竞猜发奖
send_guess_reward(WinRoleID,WinRoleName) ->
    GuessInfoList =
        lists:foldr(fun({{?role_race, RoleID}, #role_race{guess_coin=GuessCoin,guess_role_id=GuessRoleID}}, Acc) ->
                            case GuessCoin > 0 of
                                true ->
                                    [{RoleID,GuessCoin,GuessRoleID}|Acc];
                                false ->
                                    Acc
                            end;
                       (_, Acc) ->
                            Acc
                    end, [], erlang:get()),
    lists:foreach(
      fun({RoleID,GuessCoin,GuessRoleID}) ->
              case GuessRoleID of
                  WinRoleID ->
                      GetCoin = erlang:trunc(GuessCoin*data_race:get(guess_right)),
                      mail_server:send_sys_mail(RoleID, 1057, [WinRoleName,GuessCoin,GetCoin-GuessCoin,GetCoin], "",
                                                #sell_reward{coin=GetCoin});
                  _ ->
                      GetCoin = erlang:trunc(GuessCoin*data_race:get(guess_error)),
                      mail_server:send_sys_mail(RoleID, 1058, [get_role_name(GuessRoleID),GuessCoin,GetCoin], "",
                                                #sell_reward{coin=GetCoin})
              end
      end, GuessInfoList).

%% 获取指定轮次的战报
get_cur_round_history(Round, GroupID) ->
    case erlang:get({?cur_history, GroupID, Round}) of
        List when is_list(List) ->
            List;
        _ ->
            []
    end.

set_cur_round_history(Round,  GroupID, History) ->
    erlang:put({?cur_history, GroupID, Round}, History).

erase_cur_round_history(GroupID) ->
    lists:foreach(fun(Round) ->
                          List1 = erlang:erase({?cur_history, GroupID, Round}),
                          clear_replay_data(List1)
                  end, lists:seq(1, 20)).

clear_replay_data(List) when erlang:is_list(List) ->
    RepalyIDList =
        lists:foldr(fun(#p_race_rec{replay_uid_list=RList}, Acc) ->
                            case erlang:is_list(RList) andalso RList =/= [] of
                                true ->
                                    RList2 =
                                        lists:map(fun(E) ->
                                                          erlang:integer_to_list(E)
                                                  end, RList),
                                    lists:append(RList2, Acc);
                                false ->
                                    Acc
                            end
                    end, [], List),
    RepalyIDList2 = string:join(RepalyIDList, ","),
    erlang:spawn(fun() -> db_sql:del_fightReplayList(RepalyIDList2) end);
clear_replay_data(_) ->
    ok.


%% 广播一场战斗
bc(RoleIDList, BcRecord) ->
    Bin = proto:encode(BcRecord),
    lists:foreach(fun(RoleID) -> ?unicast2(RoleID, Bin) end, RoleIDList).


%% 判断谁是攻击方
check_attack_and_defend(RoleIDA,RoleIDB,FightListA,FightListB, NameA,NameB,LieuAddA, LieuAddB,TalentListA,TalentListB,TrSpecialA,TrSpecialB,SkinInfoA,SkinInfoB) ->
    PowerA = get_role_fightPower(RoleIDA,FightListA,LieuAddA),
    PowerB = get_role_fightPower(RoleIDB,FightListB,LieuAddB),
    if PowerA >= PowerB ->
           {RoleIDA,RoleIDB,FightListA,FightListB, NameA,NameB, LieuAddA, LieuAddB, PowerA, PowerB,TalentListA,TalentListB,TrSpecialA,TrSpecialB,SkinInfoA,SkinInfoB};
       true ->
           {RoleIDB,RoleIDA,FightListB,FightListA, NameB,NameA, LieuAddB, LieuAddA, PowerB, PowerA,TalentListB,TalentListA,TrSpecialB,TrSpecialA,SkinInfoB,SkinInfoA}
    end.

get_role_fightPower(0,_FighterList,_LieuAdd)->
    0;
get_role_fightPower(RoleID,FighterList,LieuAdd)->
    RolePublic = role_lib:get_rolePublic(RoleID),
    if RolePublic =/= [] ->
           RolePublic#rolePublic.fightPower;
       true ->
           get_total_power(FighterList,LieuAdd)
    end.

%% 获取玩家总战斗力
get_total_power(FighterList, {AtkAdd, HpAdd}) ->
    FighterList2 = ger_attr:refresh_other_fightPower(FighterList, AtkAdd, HpAdd),
    lists:foldl(fun(Ger, Acc) ->
                        Ger#ger.gerAttr#gerAttr.gerFightPower + Acc
                end, 0, FighterList2).

%% 计算淘汰赛需要进行的轮数和每轮需要打的次数
get_round_and_times(Num) when Num > 512->
    {10, Num-512};
get_round_and_times(Num) when Num > 256->
    {9, Num-256};
get_round_and_times(Num) when Num > 128 ->
    {8, Num-128};
get_round_and_times(Num) when Num > 64 ->
    {7, Num-64};
get_round_and_times(Num) when Num > 32 ->
    {6, Num-32};
get_round_and_times(Num) when Num > 16 ->
    {5, Num-16};
get_round_and_times(Num) when Num > 8 ->
    {4, Num-8};
get_round_and_times(Num) when Num > 4 ->
    {3, Num-4};
get_round_and_times(Num) when Num > 2 ->
    {2, Num-2};
get_round_and_times(Num) when Num > 1 ->
    {1, Num-1};
get_round_and_times(_) ->
    {0, 0}.

do_enter_channel(RoleID, State) ->
    #state{status=Status} = State,
    case Status >= ?STATUS_PRE_FIGHT1 of
        true ->
            BCList = State#state.bc_list,
            NewState = State#state{bc_list=[RoleID|lists:delete(RoleID, BCList)]},
            {noreply, NewState};
        false ->
            {noreply, State}
    end.

do_leave_channel(RoleID, State) ->
    #state{bc_list=BCList} = State,
    NewState = State#state{bc_list=lists:delete(RoleID, BCList)},
    {noreply, NewState}.

do_cs_race_pos_history(RoleID, Pos, _State) ->
    {Round, Nth} = pos_to_round_and_nth(Pos),
    case Round < 4 of
        true ->
            RoundHistoryList = get_cur_round_history(Round, 0),
            case erlang:length(RoundHistoryList) >= Nth of
                true ->
                    RaceRec = lists:nth(Nth, lists:reverse(RoundHistoryList));
                false ->
                    RaceRec = #p_race_rec{atk_name= <<"">>,def_name= <<"">>,replay_uid_list=[],atk_role_id=0,def_role_id=0,atk_fight_power=0,def_fight_power=0,round=0,
                                          group_id=0,atk_is_male=false,def_is_male=false,atk_title=0,def_title=0,atk_head=0,def_head=0,win_or_lose_list=[]}
            end;
        false ->
            RaceRec = #p_race_rec{atk_name= <<"">>,def_name= <<"">>,replay_uid_list=[],atk_role_id=0,def_role_id=0,atk_fight_power=0,def_fight_power=0,round=0,
                                  group_id=0,atk_is_male=false,def_is_male=false,atk_title=0,def_title=0,atk_head=0,def_head=0,win_or_lose_list=[]}
    end,
    ?unicast2(RoleID, #sc_race_pos_history{race_rec=RaceRec, pos=Pos}).

do_cs_race_fight_list(RoleID, GroupID, State) ->
    case erlang:is_integer(GroupID) andalso GroupID >= 1 andalso GroupID =< 8 of
        true ->
            GroupList = get_group_list(State, GroupID),
            GroupList2 = lists:sublist(lists:reverse(GroupList), 20),
            List =
                lists:map(fun(GroupRoleID) ->
                                  #role_race{role_info=#role_info{roleName=RoleName,fightPower=FightPower,level=RoleLevel,isMale=IsMale,title=Title,head=Head}} = get_role_race(GroupRoleID),
                                  #p_race_fighter{role_id=GroupRoleID,role_name=RoleName,fight_power=FightPower,role_level=RoleLevel,is_male=IsMale,title=Title,head=Head}
                          end, GroupList2),
            ?unicast2(RoleID, #sc_race_fight_list{group_id=GroupID, fighter_list=lists:keysort(#p_race_fighter.fight_power, List)});
        false ->
            ignore
    end.

do_race_guess(RoleID, GuessCoin, GuessRoleID, Status, GroupWinList) ->
%%     case catch check_race_guess(RoleID, GuessCoin, GuessRoleID, Status, GroupWinList) of
%%         {ok, NewRoleRace} ->
%%             set_role_race(RoleID, NewRoleRace),
%%             role_lib:send_server(RoleID, {race_guess_succ, GuessCoin}),
            ?unicast2(RoleID, #sc_race_guess{result=0}).
%%         {false, Reason} ->
%%             ?unicast2(RoleID, #sc_race_guess{result=Reason})
%%     end.

check_race_guess(RoleID, GuessCoin, GuessRoleID, Status, GroupWinList) ->
    case Status of
        ?STATUS_WAIT_FOUR_FIGHT ->
            next;
        _ ->
            erlang:throw({false, 2})
    end,
	case GuessRoleID of
		0 ->
			erlang:throw({false,4});
		_ ->
			next
	end,
    case lists:member(GuessRoleID, GroupWinList) of
        true ->
            next;
        false ->
            erlang:throw({false, 4})
    end,
    case get_role_race(RoleID) of
        ?undefined ->
            NewRoleRace = #role_race{role_info=#role_info{roleID=RoleID},guess_role_id=GuessRoleID,guess_coin=GuessCoin};
        #role_race{guess_coin=OldGuessCoin} =RoleRace when OldGuessCoin =:= 0 ->
            NewRoleRace = RoleRace#role_race{guess_role_id=GuessRoleID,guess_coin=GuessCoin};
        NewRoleRace ->
            erlang:throw({false, 3})
    end,
    {ok, NewRoleRace}.

do_cs_race_guess_info(RoleID) ->
    CoinValList = data_race:get(coin_val_list),
    case get_role_race(RoleID) of
        ?undefined ->
            GuessCoin = 0,
            GuessRoleID = 0;
        #role_race{guess_coin=GuessCoin,guess_role_id=GuessRoleID} ->
            next
    end,
    ?unicast2(RoleID, #sc_race_guess_info{coinValList=CoinValList,roleID=GuessRoleID,guessCoin=GuessCoin}).

do_cs_race_info(RoleID, State) ->
    #state{status=Status, next_status_timestamp=Timestamp, session_id=SessionID, sign_list=SignList, champion_id=ChampionID, champion_name=ChampionName, pos_info_list=PosInfoList}=State,
    case get_role_race(RoleID) of
        ?undefined ->
            SelfGroupID = 0;
        #role_race{group_id=SelfGroupID} ->
            next
    end,
    Record = #sc_race_info{status=Status, timestamp=Timestamp, session_id=SessionID,
                           is_sign=lists:member(RoleID, SignList), list=PosInfoList,
                           champion_id=ChampionID,champion_name=ChampionName, self_group_id=SelfGroupID, is_auto=get_is_auto(RoleID)},
%%     print_pos_info(PosInfoList),
    ?unicast2(RoleID, Record).

print_pos_info(PosInfoList) ->
    lists:foreach(fun(#p_race_pos{role_id=RoleID,pos=Pos}) ->
                          ?ERR("RoleID:~w, Pos:~w", [RoleID, Pos])
                  end, PosInfoList).

gen_pos_info_list(GroupWinList) ->
    lists:foldr(
      fun(Round, Acc0) ->
              case Round =:= 4 of
                  false ->
                      RaceRecList = get_cur_round_history(Round, 0),
                      {NewAcc0, _} =
                          lists:foldr(
                            fun(PRaceRec, {Acc1, AccPos}) ->
                                    {WinRoleID, WinRoleName, _LoseRoleID, _LoseName} = to_win_lose_info(PRaceRec),
                                    #role_race{role_info=#role_info{title=Title,isMale=IsMale,head=Head}} = get_role_race(WinRoleID),
                                    {[#p_race_pos{role_id=WinRoleID,role_name=WinRoleName,is_male=IsMale,title=Title,head=Head,pos=AccPos}|Acc1], AccPos + 1}
                            end, {Acc0, gen_start_pos(Round)}, RaceRecList),
                      NewAcc0;
                  true ->
                      {NewAcc0, _} =
                          lists:foldr(fun(WinRoleID, {Acc1, AccPos}) ->
                                              #role_race{role_info=#role_info{roleName=WinRoleName,title=Title,isMale=IsMale,head=Head}} = get_role_race(WinRoleID),
                                              {[#p_race_pos{role_id=WinRoleID,role_name=WinRoleName,is_male=IsMale,title=Title,head=Head,pos=AccPos}|Acc1], AccPos + 1}
                                      end, {Acc0, 1}, GroupWinList),
                      NewAcc0
              end
      end, [], lists:seq(1, 4)).

gen_pos_info_list2(GroupWinList) ->
    {NewAcc, _} =
        lists:foldr(fun(WinRoleID, {Acc, AccPos}) ->
                            #role_race{role_info=#role_info{roleName=WinRoleName,title=Title,isMale=IsMale,head=Head}} = get_role_race(WinRoleID),
                            {[#p_race_pos{role_id=WinRoleID,role_name=WinRoleName,is_male=IsMale,title=Title,head=Head,pos=AccPos}|Acc], AccPos + 1}
                    end, {[], 1}, GroupWinList),
    NewAcc.

gen_start_pos(Round) ->
    case Round of
        4 ->
            1;
        3 ->
            9;
        2 ->
            13;
        1 ->
            15
    end.

pos_to_round_and_nth(Pos) when erlang:is_integer(Pos), Pos >= 1, Pos =< 15 ->
    if
        Pos =:= 15 ->
            {1, 1};
        Pos >=13 ->
            {2, Pos - 12};
        Pos >=9 ->
            {3, Pos - 8};
        true ->
            {4, Pos}
    end.

cut_list(List, Start, Num) ->
    List2 =
        lists:filter(fun(#p_race_rec{atk_role_id=AtkRoleID, def_role_id=DefRoleID}) ->
                             AtkRoleID =/= 0 andalso DefRoleID =/= 0
                     end, List),
    Len = erlang:length(List2),
    case Len >= Start + 1 of
        true ->
            lists:sublist(List2, Start + 1, Num);
        false ->
            []
    end.

do_cs_race_history(RoleID, _Round, GroupID, Start, Num) ->
    List = get_history_list(GroupID, 1, 6),
    List2 = cut_list(List, Start, Num),
    Record = #sc_race_history{round=0, group_id=GroupID, history_list=List2},
%%     print_history_data(List2),
    ?unicast2(RoleID, Record).

do_cs_race_self_history(RoleID) ->
    case get_role_race(RoleID) of
        #role_race{group_id=GroupID} when GroupID =/= 0 ->
            List = get_history_list(GroupID, 1, 9),
            List2 = lists:filter(fun(#p_race_rec{atk_role_id=AtkRoleID, def_role_id=DefRoleID}) ->
                                         AtkRoleID =:= RoleID andalso DefRoleID =/= 0 orelse DefRoleID =:= RoleID andalso AtkRoleID =/= 0
                                 end, List),
            ?unicast2(RoleID, #sc_race_self_history{history_list=List2});
        _ ->
            ?unicast2(RoleID, #sc_race_self_history{history_list=[]})
    end.

print_history_data(List) ->
    lists:foreach(fun(#p_race_rec{atk_name=Aname,def_name=Dname,atk_role_id=Aroleid,def_role_id=DroleID,round=Round,group_id=GroupID}) ->
                          ?ERR("Round:~w,GroupID:~w,Aroleid:~w,DroleID:~w,Aname:~w,Dname:~w", [Round,GroupID,Aroleid,DroleID,Aname,Dname])
                  end, List).

get_history_list(GroupID, StartRound, EndRound) when StartRound =< EndRound ->
    lists:foldr(fun(Round, Acc) ->
                        List = get_cur_round_history(Round, GroupID),
                        lists:append(List, Acc)
                end, [], lists:seq(StartRound, EndRound)).

do_cs_race_replay(RoleID, ReplayUID) ->
    {Info, Result} = get_replay_record(ReplayUID),
    Record = #sc_race_replay{result=Result, fight_info=Info},
    ?unicast2(RoleID, Record).

%%战报查询缓存
get_replay_record(ReplayUID)->
    case erlang:get({?replayRecord, ReplayUID}) of
        undefined ->
            case db_sql:get_fightReplay(ReplayUID) of
                []->
                    {#sc_fight_request{actionList=[],fighterList=[],result=true}, 2};
                Rec->
                    erlang:put({?replayRecord, ReplayUID}, Rec),
                    {Rec, 1}
            end;
        Cached ->
            {Cached, 1}
    end.

%% 清除所有的战报缓存
erase_replay_record() ->
    lists:foreach(fun({{?replayRecord, ReplayUID},_}) ->
                          erlang:erase({?replayRecord, ReplayUID});
                     (_) ->
                          next
                  end, erlang:get()).

do_cs_race_auto_unsign(RoleID) ->
    case get_is_auto(RoleID) of
        true ->
            set_is_auto(RoleID, false),
            ?unicast2(RoleID, #sc_race_auto_unsign{reason_code=0});
        false ->
            ?unicast2(RoleID, #sc_race_auto_unsign{reason_code=1})
    end.

%% 更新小组的战斗力排序
update_group(#state{status=Status}=State) ->
    case Status >= ?STATUS_WAIT_PRE_FIHGT1 andalso Status =< ?STATUS_PRE_FIGHT8 of
        true ->
            lists:foldr(fun(GroupID, {ok, AccState}) ->
                                List = get_group_list(AccState, GroupID),
                                NewList = lists:sort(fun(RoleID1, RoleID2) ->  
                                                             #role_race{role_info=#role_info{fightPower=FightPower1}}=get_role_race(RoleID1),
                                                             #role_race{role_info=#role_info{fightPower=FightPower2}}=get_role_race(RoleID2),
                                                             FightPower1 < FightPower2
                                                     end, List),
                                NewAccState = set_group_list(AccState, GroupID, NewList),
                                {ok, NewAccState}
                        end, {ok, State}, lists:seq(1, 8));
        false ->
            {ok, State}
    end.

bc_state(State) ->
    [do_cs_race_info(RoleID, State)||{RoleID,_}<-ets:tab2list(?ETS_ROLE_ONLINE)].

%%----------------------------------------test code--------------------------------------------------------------------
test_sign_niubi(N) when erlang:is_integer(N) andalso N > 0 ->
    lists:foreach(fun(RoleID) ->
                          test_sign(RoleID)
                  end, get_niubi(N)).

get_niubi(N) ->
    Sql = io_lib:format("select roleID from gRole order by fightPower desc limit ~w;", [N]),
    case db_sql:get_all(Sql) of
        [_|_]=List ->
            [E||[E]<-List];
        _ ->
            []
    end.

test_sign(RoleID) ->
    RoleInfo = db_sql:get_roleInfo(RoleID),
    case erlang:is_record(RoleInfo, role) of
        true ->
            erlang:send(?MODULE, {do_role_sign_auto, RoleID, RoleInfo});
        false ->
            ignore
    end.

test_sign_others(Max) when Max >= 1, Max =< 9999 ->
    lists:foreach(fun(RoleID) ->
                          test_sign(RoleID)   
                  end, lists:seq(?ROLE_ID_BASE + 1, ?ROLE_ID_BASE + Max)).

to_next() ->
    erlang:send(?MODULE, change_to_next_status).

fix_after_merge(State) ->
    case file:read_file_info("./merge.touch") of
        {ok, _} ->
            {_,StartTime} = data_race:get({start_time,?STATUS_SIGN}),
            {{M,Y,D},_} = erlang:localtime(),
            NextTimeStamp = util:datetime_to_seconds({{M,Y,D+1},StartTime}),
            LeftTime = NextTimeStamp - util:now(),
            erlang:send_after(LeftTime*1000,self(),change_to_next_status),
            ?ERR("~w fix after merge, to sign left time is:~p.~n", [?MODULE,LeftTime]),
            {true,State#state{status=?STATUS_NOT_OPEN, next_status_timestamp=NextTimeStamp, sign_list=[], group_win_list=[], pos_info_list=[],
                                group1=[], group2=[], group3=[], group4=[], group5=[], group6=[], group7=[], group8=[]}};
        _ ->
           false
    end.

test_set_champion(RoleID) ->
    erlang:send(?MODULE, {test_set_champion, RoleID}).

change_sell_reward_by_sex(SellReward,RoleID) when is_record(SellReward,sell_reward)->
    case role_lib:get_rolePublic(RoleID) of
        []->
            ?ERR("can not find rolePublic info RoleID:~w ~n",[RoleID]),
            SellReward;
        #rolePublic{isMale=IsMale}->
            #sell_reward{item=ItemList}=SellReward,
            NewItemList = [change_item_by_sex(Item,IsMale)||Item<-ItemList],
            SellReward#sell_reward{item=NewItemList}
    end;
change_sell_reward_by_sex(SellReward,_RoleID)->
    ?ERR("illegal sell_reward:~w ~n",[SellReward]),
    SellReward.

change_item_by_sex(Item,IsMale) when is_record(Item,new_item)->
    case IsMale of
        true->
            change_item_to_male(Item);
        false->
            change_item_to_female(Item)
    end;
change_item_by_sex(Item,_IsMale)->
    ?ERR("illegal new_item:~w ~n",[Item]),
    Item.
change_item_to_male(Item)->
    case data_race:get(data_sex_changelist) of
        ?undefined->
            ?ERR("can not find data_sex_changelist in data_race.config"),
            Item;
        List->
            case lists:keyfind(Item#new_item.itemTypeID,2,List) of
                false->
                    Item;
                {MaleID,_FemaleID}->
                    Item#new_item{itemTypeID=MaleID}
            end
    end.
change_item_to_female(Item)->
    case data_race:get(data_sex_changelist) of
        ?undefined->
            ?ERR("can not find data_sex_changelist in data_race.config"),
            Item;
        List->
            case lists:keyfind(Item#new_item.itemTypeID,1,List) of
                false->
                    Item;
                {_MaleID,FemaleID}->
                    Item#new_item{itemTypeID=FemaleID}
            end
    end.