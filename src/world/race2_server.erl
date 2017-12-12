-module(race2_server).
-behaviour(gen_server).
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

-define(check_interval, 45000).
-define(persist_interval, 180000).
-define(fight_timeout, 5).  %% 最多等待战斗过程5秒
-define(group_fight_normal, 6). %% 小组赛费加时赛的场次数量

-define(gamble_fight, 1).
-define(gamble_right, 2).
-define(gamble_wrong, 3).

%%%%%===========需要固化保存的数据
-define(pd_sign_list, pd_sign_list).        %% 报名数据
-define(pd_arena_list, pd_arena_list).      %%　第二阶段擂台赛
-define(pd_group_role_list, pd_group_role_list).      %%　第三阶段循环赛
-define(pd_group_fight_record, pd_group_fight_record).      %%　记录小组赛比赛结果
-define(pd_fight_step, pd_fight_step).          %%　记录小组赛比赛轮次，对应两组进程字典数据
-define(pd_knockout_list, pd_knockout_list).    %%　第四五六阶段淘汰赛
-define(pd_gamble_list, pd_gamble_list).        %%　押注记录
-define(pd_final_list, pd_final_list).        %%　最后一二三名次
-define(pd_last_list, pd_last_list).        %%　最后一二三名次
-define(pd_cancel_time, pd_cancel_time).        %%　最后一二三名次
%%%%%===========不需要固化保存的数据
%% -define(pd_top_list, pd_top_list).              %%　报名前四名
-define(pd_sign_num_list, pd_sign_num_list).    %% 记录报名人数，避免每次计算length
-define(pd_open_list, pd_open_list).            %%　广播列表
-define(pd_fight_lock, pd_fight_lock).          %%　战斗异步锁
-define(pd_debug, pd_debug).          %%

-define(zone_list, [1,2,3,4]).
-define(pos_list, [11,12,13,14
                  ,21,22,23,24
                  ,31,32,33,34
                  ,41,42,43,44]).
-define(group_rule, [{1,2},{1,3},{1,4},{2,3},{2,4},{3,4}]).
-define(knockout_loop, [4,5,6]).
-define(gamble_type, [31,32,33,34,4,5,62,61]).

-record(state, {step = 0}).

-record(fight_step, {timestamp = 0, fight_type = 0, fighter=[0,0], rec = []}).

%% 为了防止单次消息过长，战斗过程单独消息发送

%% ====================================================================
%% API functions
%% ====================================================================

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

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
    process_flag(trap_exit,true),
    init_proc_dict(),
    %% 初始化固化数据
    case db_sql:get_etc(?DB_ETC_KEY_RACE2) of
        {{state,State},ProcDictList} when erlang:is_record(State, state)->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, ProcDictList);
        _ ->
            put(?pd_last_list,[]),
            State = #state{}
    end,
    %% 初始化非固化数据
    lists:foreach(fun(Z) -> 
            put({?pd_sign_num_list,Z},erlang:length([A||{A,B}<-get(?pd_sign_list),B =:= Z]))
        end, ?zone_list),
    put(?pd_open_list,[]),
    [put({?pd_fight_lock,P},{0,0,0})||P<-?pos_list],
    NewState = do_check2(State),
    set_persist_interval(),
    set_check_interval(),
%%     IsV310 = case get({?pd_fight_step,1}) of
%%                  L when erlang:is_list(L) ->
%%                      Len = erlang:length(L),
%%                      L >= 18;
%%                  _ ->
%%                      false
%%              end,
%%     NewState2 = 
%%         if
%%             IsV310 =:= true andalso NewState#state.step =:= 3 ->
%%                 ?ERR("update data from v310 old:~w",[get({?pd_fight_step,1})]),
%%                 do_next_step(3,NewState);
%%             true ->
%%                 NewState
%%         end,
    {ok, NewState}.

terminate(Reason, State) ->
    ?ERR("~w terminate, reason:~w", [?MODULE, Reason]),
    do_persist(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_champion, _From, State) ->
    {ChampionID, ChampionName} = 
        case get(?pd_last_list) of
            [{1,First1}|[{2,First2}|[{3,First3}|_]]] ->
                RolePub = role_lib:get_rolePublic(First1),
                {First1,RolePub#rolePublic.roleName};
            _ ->
                {0,<<"">>}
        end,
    {reply, {ChampionID, ChampionName}, State};
handle_call(debug_current_fight, _From, State) ->
    if 
        State#state.step =:= 2 ->
            Reply = [get({?pd_arena_list,Z})||Z<-?zone_list];
        State#state.step =:= 3 ->
            Group = [get({?pd_group_fight_record,Z})||Z<-?zone_list],
            Step = [get({?pd_fight_step,Z2})||Z2<-?zone_list],
            Reply = {{group,Group},{step,Step}};
        4 =< State#state.step andalso State#state.step =< 6->
            Finish = [get({?pd_knockout_list,Z})||Z<-?knockout_loop],
            Ready = get(?pd_fight_step),
            Reply = {{ready,Ready},{finish,Finish}};
        true ->
            Reply = State
    end,
    {reply, Reply, State};
handle_call({debug_get_pd,PdName}, _From, State) ->
    ?ERR("debug_get_pd ~w:~w",[PdName, get(PdName)]),
    {reply, get(PdName), State};
handle_call({check_group_score,Zone}, _From, State) ->
    {reply, check_group_score(Zone), State};
handle_call(Request, _From, State) ->
    ?ERR("未知的Request:~w, From:~w", [Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info(check_tick, State) ->
    set_check_interval(),
    L = if
            State#state.step =:= 3 ->
                erlang:length(lists:merge([get({?pd_fight_step,Zone})||Zone<-?zone_list]));
            4 =< State#state.step andalso State#state.step =< 6  ->
                erlang:length(get(?pd_fight_step));
            true ->
                0
        end,
%%     if
%%         L =:= 0 ->
%%             NewState = do_check(State);
%%         true ->
%%             NewState = State
%%     end,
    {noreply,do_check2(State)};
handle_info(persist_tick, State) ->
    set_persist_interval(),
    do_persist(State),
    {noreply,State};
handle_info({role_offline, RoleID},State) ->
    set_close(RoleID),
    {noreply,State};
handle_info({do_race2_fight_res,FightResult, AtkRoleID, TargetRoleId, {2,_TimeStamp,Pos}},State) ->
    Now = util:now(),
    {FightTs,AtkRoleID0,TargetRoleId0} = get({?pd_fight_lock,Pos}),
    IsSwitch = FightResult#sc_race2_fight.dice_numB > FightResult#sc_race2_fight.dice_numA,
    if
        Now =< (FightTs + ?fight_timeout) andalso AtkRoleID =:= AtkRoleID0 andalso TargetRoleId =:= TargetRoleId0 ->
            [FightRec] = FightResult#sc_race2_fight.fight_rec,
            %% do_group_fight_loop 中还有出相同逻辑，修改的话一起修改
            if
                FightRec#sc_fight_request.result /= IsSwitch -> %% 挑战成功
                    Zone = Pos div 10,
                    ZIndex = Pos rem 10,
                    OldZoneRIDList = get({?pd_arena_list,Zone}),
                    case lists:member(AtkRoleID, OldZoneRIDList) of
                        false ->
                            NewZoneRIDList = util:nth_replace(ZIndex, OldZoneRIDList, AtkRoleID);
                        true ->
                            NewZoneRIDList = 
                                replace_on_arena1(AtkRoleID,TargetRoleId,OldZoneRIDList)
                    end,
                    put({?pd_arena_list,Zone},NewZoneRIDList),
%%                     ?ERR("do_race2_fight_res ~w ---~w(~w)---> ~w",[OldZoneRIDList,AtkRoleID,ZIndex,NewZoneRIDList]),
                    send_arena_fighter_list(get(?pd_open_list));
                true ->
                    ignore
            end,
            send_client_msg(AtkRoleID,FightResult),
            put({?pd_fight_lock,Pos},{0,0,0});
        Now > (FightTs + ?fight_timeout) andalso AtkRoleID =:= AtkRoleID0 andalso TargetRoleId =:= TargetRoleId0 ->
            put({?pd_fight_lock,Pos},{0,0,0}),
            send_client_msg(AtkRoleID,#sc_race2_fight{result = 4
                                           ,dice_numA = 0
                                           ,dice_numB = 0
                                           ,fight_rec = []});
        true ->
            send_client_msg(AtkRoleID,#sc_race2_fight{result = 4
                                           ,dice_numA = 0
                                           ,dice_numB = 0
                                           ,fight_rec = []})
    end,
    {noreply,State};
%% handle_info({do_race2_fight_res,FightResult, AtkRoleID, TargetRoleId, {3,TimeStamp,Pos}},State) ->
%%     {noreply, State};
handle_info({client_msg, RoleID, Msg}, State) ->
    NewState = do_handle_msg(Msg, RoleID, State),
    {noreply, NewState};
handle_info(debug_clear_data,State) ->
    {noreply,do_next_step(1,State)};
handle_info(debug_get_info,State) ->
    ?ERR("debug_get_info----------~npd:~w~ns:~w",[get(),State]),
    {noreply,State};
handle_info(debug_next_step,State) ->
    L = if
            State#state.step =:= 3 ->
                erlang:length(lists:merge([get({?pd_fight_step,Zone})||Zone<-?zone_list]));
            4 =< State#state.step andalso State#state.step =< 6  ->
                erlang:length(get(?pd_fight_step));
            true ->
                0
        end,
    NewState = case State#state.step+1 of
                  _ when L > 0 ->
                      ?ERR("战斗还没打完 ~w",[get(?pd_fight_step)]),
                      State;
                  8 ->
                      do_next_step(1,State);
                  OtherNum ->
                      do_next_step(OtherNum,State)
              end,
    {noreply,NewState};
handle_info(debug_next_fight,State) when State#state.step =:= 3 ->
    lists:foreach(fun(Zone) -> 
            case get({?pd_fight_step,Zone}) of
                [] ->
                    ignore;
                [FightStep|Other] ->
                    AddStep = do_group_fight_loop(Zone,FightStep),
                    NewStepList = lists:sort(AddStep++Other),
                    put({?pd_fight_step,Zone},NewStepList),
                    case NewStepList of
                        [] ->
                            case check_extra_fight(Zone) of
                                ignore ->
                                    ignore;        %% 检查是不是需要加时
                                AddStepList when erlang:is_list(AddStepList)->
                                    put({?pd_fight_step,Zone},AddStepList)
                            end;
                        _ ->
                            put({?pd_fight_step,Zone},NewStepList)
                    end,
                    send_knockout_fighter_list(State#state.step)
            end
        end, ?zone_list),
    {noreply,State};
handle_info(debug_next_fight,State) when State#state.step > 3 ->
    erlang:send(?MODULE, {debug_next_fight,5}),
    {noreply,State};
handle_info({debug_next_fight,Num},State) when State#state.step > 3 ->
    if
        Num > 0 ->
            case get(?pd_fight_step) of
                [] ->
                    ignore;
                [FightStep|Other] ->
                    do_group_fight_loop(0,FightStep),
                    put(?pd_fight_step,Other),
                    send_knockout_fighter_list(State#state.step)
            end,
            erlang:send(?MODULE, {debug_next_fight,Num-1});
        true ->
            ignore
    end,
    {noreply,State};
handle_info(test_mail,State) ->
    send_final_reward(),
    {noreply,State};
handle_info({debug_set_pd,PdName,Value},State) ->
    ?ERR("debug_get_pd ~w:~w new:~w",[PdName,get(PdName),put(PdName,Value)]),
    {noreply,State};
handle_info(test_crash,State) ->
    A = 1,
    B = 2,
    A = B,
    {noreply,State};
handle_info(Info, State) ->
    ?ERR("未知的Info:~w", [Info]),
    {noreply, State}.

%% ====================================================================
%% do_handle_msg functions
%% ====================================================================

do_handle_msg(#cs_race2_info{}, RoleID, State) ->
    IsSign = case lists:keysearch(RoleID, 1, get(?pd_sign_list)) of
                 {value,{RoleID,SelectZone}} ->
                     SelectZone;
                 false ->
                     0
             end,
    GroupNum = if
                   State#state.step >= 3 ->
                       get_self_group_num(RoleID);
                   true ->
                       0
               end,
    set_open(RoleID),
    TopList = get_top_list(State#state.step),
    {_Type,CostNum} = data_race2:get(cancel_sign_cost),
    CancelTime = erlang:length([R||R<-get(?pd_cancel_time),R =:= RoleID]),
    send_client_msg(RoleID,#sc_race2_info{race_state = State#state.step
                                  ,sign_num = [get({?pd_sign_num_list,Z})||Z<-?zone_list]
                                  ,self_sign_zone = IsSign
                                  ,group_zone = GroupNum
                                  ,top_list = TopList
                                  ,cancel_cost = (CancelTime + 1) * CostNum }),
    State;
do_handle_msg(#cs_race2_sign{select_zone=SelectZone}, RoleID, State) ->
    OldList = get(?pd_sign_list),
    case lists:member(SelectZone, ?zone_list) of
        true ->
            case lists:keymember(RoleID, 1, OldList) of
                false ->
                    if
                        State#state.step =:= 1 ->
                            put(?pd_sign_list,[{RoleID,SelectZone}|OldList]),
                            OldNum = get({?pd_sign_num_list,SelectZone}),
                            put({?pd_sign_num_list,SelectZone},OldNum+1),
                            send_client_msg(RoleID,#sc_race2_sign{result=1});
                        true ->
                            send_client_msg(RoleID,#sc_race2_sign{result=3})
                    end;
                true ->
                    send_client_msg(RoleID,#sc_race2_sign{result=2})
            end;
        false ->
            send_client_msg(RoleID,#sc_race2_sign{result=3})
    end,
    State;
do_handle_msg(#cs_race2_openclose{openclose=IsOpen}, RoleID, State) ->
    case IsOpen of
        1 ->
            set_open(RoleID);
        2 ->
            set_close(RoleID)
    end,
    send_client_msg(RoleID,#sc_race2_openclose{result=1}),
    State;
do_handle_msg(#cs_race2_arena_fighter{}, RoleID, State) ->
    case State#state.step of
        2 ->
            set_open(RoleID),
            send_arena_fighter_list(RoleID);
        _ ->
            send_client_msg(RoleID,#sc_race2_arena_fighter{arena_fighter_list=[]})
    end,    
    State;
do_handle_msg(#cs_race2_fight{role_id=TargetRoleId,pos=Pos}, RoleID, State) ->
    case State#state.step of
        2 ->
            IsCheckPos = lists:member(Pos, ?pos_list),
            Now = util:now(),
            case get({?pd_fight_lock,Pos}) of
                _ when IsCheckPos =:= false ->
                    ?ERR("cs_race2_fight pos error. pos:~w",[IsCheckPos]),
                    send_client_msg(RoleID,#sc_race2_fight{result = 4
                                                   ,dice_numA = 0
                                                   ,dice_numB = 0
                                                   ,fight_rec = []});
                {X,_,_} when Now > (X + ?fight_timeout) -> % 要么X等于0彻底无锁，要么X值较小，说明上次战斗超时了
                    SignList = get(?pd_sign_list),
                    case lists:keysearch(RoleID, 1, SignList) of
                        {value,{RoleID,SelectZone}} ->
                            Zone = Pos div 10,
                            ZIndex = Pos rem 10,
                            OldZoneRIDList = get({?pd_arena_list,Zone}),
                            Target = lists:nth(ZIndex, OldZoneRIDList),
                            RoleIDIndex = get_index(RoleID,OldZoneRIDList),
                            if
                                RoleIDIndex < ZIndex andalso RoleIDIndex /= 0 ->
                                    send_client_msg(RoleID,#sc_race2_fight{result = 7
                                                                   ,dice_numA = 0
                                                                   ,dice_numB = 0
                                                                   ,fight_rec = []});
                                Zone /= SelectZone ->
                                    send_client_msg(RoleID,#sc_race2_fight{result = 6
                                                                   ,dice_numA = 0
                                                                   ,dice_numB = 0
                                                                   ,fight_rec = []});
                                TargetRoleId =:= Target ->
                                    Now = util:now(),
                                    role_lib:send_server(RoleID, {route, role_race, {do_race2_fight, TargetRoleId, RoleID,{2, Now, Pos}}}),
                                    put({?pd_fight_lock,Pos},{Now,RoleID,TargetRoleId});
                                true ->
                                    send_client_msg(RoleID,#sc_race2_fight{result = 4
                                                                   ,dice_numA = 0
                                                                   ,dice_numB = 0
                                                                   ,fight_rec = []})
                            end;
                        false ->
                            send_client_msg(RoleID,#sc_race2_fight{result = 5
                                                           ,dice_numA = 0
                                                           ,dice_numB = 0
                                                           ,fight_rec = []})
                    end;
                {X,_,_} when X /= 0 ->
                    send_client_msg(RoleID,#sc_race2_fight{result = 3
                                                   ,dice_numA = 0
                                                   ,dice_numB = 0
                                                   ,fight_rec = []})
            end;
        _ ->
            send_client_msg(RoleID,#sc_race2_fight{result = 2
                                           ,dice_numA = 0
                                           ,dice_numB = 0
                                           ,fight_rec = []})
    end,
    State;
do_handle_msg(#cs_race2_knockout_fighter{step_num=3}, RoleID, State) ->
    L = lists:foldl(fun(Zone,AccList) ->
            SubList = lists:foldl(fun(StepInfo,Acc) when erlang:is_record(StepInfo, fight_step)-> 
                                      case StepInfo#fight_step.fighter of
                                          [X,Y] when erlang:is_integer(X) andalso erlang:is_integer(Y) ->
                                              [step_info_to_msg(StepInfo,3*10+Zone,erlang:length(Acc)+1)|Acc];
                                          [{_,_},{X,Y}] when erlang:is_integer(X) andalso erlang:is_integer(Y) andalso StepInfo#fight_step.rec =:= []
                                            andalso (StepInfo#fight_step.fight_type =:= 211 orelse StepInfo#fight_step.fight_type =:= 221) ->
                                              [step_info_to_msg(StepInfo#fight_step{fighter=[X,Y]},3*10+Zone,erlang:length(Acc)+1)|Acc];
                                          [{X, Y},_] when erlang:is_integer(X) andalso erlang:is_integer(Y) andalso StepInfo#fight_step.rec =:= [] ->
                                              [step_info_to_msg(StepInfo#fight_step{fighter=[X,Y]},3*10+Zone,erlang:length(Acc)+1)|Acc];
                                          [A1,{X, Y}] when erlang:is_integer(A1) andalso erlang:is_integer(X) andalso erlang:is_integer(Y) andalso StepInfo#fight_step.rec =:= [] ->
                                              [step_info_to_msg(StepInfo#fight_step{fighter=[X,Y]},3*10+Zone,erlang:length(Acc)+1)|Acc];
                                          _ ->
                                              Acc
                                      end;
                                     (_,Acc)->
                                          Acc
                            end, [], get({?pd_group_fight_record,Zone})++get({?pd_fight_step,Zone})),
            SubListSorted = lists:sort(fun(RA,RB)-> 
                                    RA#race2_fight_info.fight_timestamp < RB#race2_fight_info.fight_timestamp 
                            end, SubList),
            SubListSorted ++ AccList
        end, [], lists:reverse(?zone_list)),
    send_client_msg(RoleID,#sc_race2_knockout_fighter{fight_info_list = L}),
    State;
do_handle_msg(#cs_race2_knockout_fighter{step_num=StepNum}, RoleID, State) when 4 =< StepNum andalso StepNum =< 6 andalso State#state.step < StepNum->
    List = 
        case StepNum of
                4 ->
                        [#race2_fight_info{fighter = []
                                         ,fight_timestamp = util:datetime_to_seconds({erlang:date(),lists:nth(N, data_race2:get(knockout_fight_timestamp))}) 
                                                                + 3600*24*(StepNum - State#state.step)
                                         ,left_score = 0
                                         ,right_score = 0
                                         ,fight_rec_id = 0
                                         ,winner = 0
                                         ,fight_type = 4
                                         ,dice_numA = 0
                                         ,dice_numB = 0}||N<-lists:seq(1, 20)];
                5 ->
                        [#race2_fight_info{fighter = []
                             ,fight_timestamp = util:datetime_to_seconds({erlang:date(),lists:nth(N, data_race2:get(knockout_fight_timestamp))}) 
                                                                + 3600*24*(StepNum - State#state.step)
                             ,left_score = 0
                             ,right_score = 0
                             ,fight_rec_id = 0
                             ,winner = 0
                             ,fight_type = 5
                             ,dice_numA = 0
                             ,dice_numB = 0}||N<-lists:seq(1, 10)];
                6 ->
                        [#race2_fight_info{fighter = []
                             ,fight_timestamp = util:datetime_to_seconds({erlang:date(),lists:nth(N, data_race2:get(knockout_fight_timestamp))}) 
                                                                + 3600*24*(StepNum - State#state.step)
                             ,left_score = 0
                             ,right_score = 0
                             ,fight_rec_id = 0
                             ,winner = 0
                             ,fight_type = 6
                             ,dice_numA = 0
                             ,dice_numB = 0}||N<-lists:seq(1, 10)]
        end,
    send_client_msg(RoleID,#sc_race2_knockout_fighter{fight_info_list = List}),
    State;
do_handle_msg(#cs_race2_knockout_fighter{step_num=StepNum}, RoleID, State) when 4 =< StepNum andalso StepNum =< 6 andalso State#state.step >= StepNum->
    FightStep1 = if
                    StepNum =:= State#state.step ->
                        get(?pd_fight_step);
                    true ->
                        []
                end,
    FightStep2 = lists:sort(fun(RA,RB)-> 
                                    RA#fight_step.timestamp < RB#fight_step.timestamp 
                            end, FightStep1 ++ get({?pd_knockout_list,StepNum})),
    L = lists:foldl(fun(StepInfo,Acc)->
                        [step_info_to_msg(StepInfo,StepNum,erlang:length(Acc)+1)|Acc] 
            end, [], FightStep2),
    send_client_msg(RoleID,#sc_race2_knockout_fighter{fight_info_list = lists:reverse(L)}),
    State;
do_handle_msg(#cs_race2_knockout_fighter{step_num=_StepNum}, RoleID, State) ->
    send_client_msg(RoleID,#sc_race2_knockout_fighter{fight_info_list = []}),
    State;
do_handle_msg(#cs_race2_get_fight_rec{fight_rec_id=FightRecId}, RoleID, State) ->
    Type = FightRecId div 100,
    Index = FightRecId rem 100,
    Rec = 
        if
            Type >= 4 andalso Type =< 6 ->
                All = lists:sort(fun(RA,RB)-> 
                                RA#fight_step.timestamp < RB#fight_step.timestamp 
                        end, get({?pd_knockout_list,Type})),
                AllLength = erlang:length(All),
                if
                    Index =< AllLength ->
                        L = lists:nth(Index,All),
                        L#fight_step.rec;
                    true ->
                        []
                end;
            Type >= 31 andalso Type =< 34 ->
                All = lists:sort(fun(RA,RB)-> 
                                RA#fight_step.timestamp < RB#fight_step.timestamp 
                        end, get({?pd_group_fight_record,Type rem 10})),
                AllLength = erlang:length(All),
                if
                    Index =< AllLength ->
                        L = lists:nth(Index,All),
                        L#fight_step.rec;
                    true ->
                        []
                end;
            true ->
                []
        end,
    case Rec of
        [] ->
            send_client_msg(RoleID,#sc_race2_get_fight_rec{fight_rec_id = FightRecId
                                   ,dice_numA = 0
                                   ,dice_numB = 0
                                   ,fight_rec = []});
        _ ->
            send_client_msg(RoleID,#sc_race2_get_fight_rec{fight_rec_id = FightRecId
                                   ,dice_numA = Rec#sc_race2_fight.dice_numA
                                   ,dice_numB = Rec#sc_race2_fight.dice_numB
                                   ,fight_rec = Rec#sc_race2_fight.fight_rec})
    end,
    State;
do_handle_msg(#cs_race2_get_gamble{}, RoleID, State) ->
    L = lists:filter(fun({R,_,_,_,_,_}) -> R =:= RoleID end, get(?pd_gamble_list)),
    MyGamble = lists:foldl(fun({_R,Winner,Gold,GambleType,GI,GS},Acc)-> 
            [#gamble_info{gamble_type = GambleType
                         ,role_id = Winner
                         ,gold = Gold   
                         ,gamble_timestamp = util:datetime_to_seconds({erlang:date(),GI})  %% GIndex改成了#fight_step.timestamp，发给客户端的是时间戳
                         ,gamble_state = GS}|Acc]
        end, [], L),
    LimitList = lists:foldl(fun(T,AccList)-> 
                                    data_race2:get({gamble_limit,T})++AccList 
                            end, [], lists:reverse(?gamble_type)),
    Msg = #sc_race2_get_gamble{gamble_info_list = MyGamble
                        ,gamble_limit = LimitList},
    send_client_msg(RoleID,Msg),
    State;
do_handle_msg(#cs_race2_do_gamble{gamble_type=GambleType0
                                 ,role_id=Winner
                                 ,gold=Gold}, RoleID, State) ->
    case check_gamble(RoleID,GambleType0,Winner,Gold,State#state.step) of
        {true,GIndex,GambleType} -> %% GIndex改成了#fight_step.timestamp，发给客户端的是时间戳
            OldGambleList = get(?pd_gamble_list),
            AGL = lists:filter(fun({RID,WID,_Gold,GType,GI,S})->
                                    RID =:=  RoleID andalso GType =:= GambleType andalso GI =:= GIndex andalso S =:= ?gamble_fight
                        end, OldGambleList),
            {NewGambleNum,AlGambleRole} = 
                case AGL of
                    [] -> {Gold,0};
                    [{ARID,AWID,AGold,AGType,AlG,_}] -> {AGold + Gold,AWID}
                end,
            MaxGambleNum = data_race2:get(gamble_limit),
            if
                AlGambleRole /= 0 andalso AlGambleRole /= Winner ->
                    send_client_msg(RoleID,#sc_race2_do_gamble{result = 3});
                %% 改为压过了就不能再押
                NewGambleNum > MaxGambleNum orelse NewGambleNum =/= Gold ->
                    send_client_msg(RoleID,#sc_race2_do_gamble{result = 4});
                true ->
                    case catch role_lib:call_server(RoleID, {deduct_gold,coin,Gold,?MONEY_DEC_TYPE_RACE_GUESS,Winner}, 500) of
                        ok ->
                            %% TODO
                            NewGambleList = [{RoleID,Winner,NewGambleNum,GambleType,GIndex,?gamble_fight}|OldGambleList -- AGL],
                            put(?pd_gamble_list,NewGambleList),
                            send_client_msg(RoleID,#sc_race2_do_gamble{result = 1});
                        fail ->
                            send_client_msg(RoleID,#sc_race2_do_gamble{result = 6});
                        _ ->
                            send_client_msg(RoleID,#sc_race2_do_gamble{result = 7})
                    end
            end;
        {false,Reason} ->
            send_client_msg(RoleID,#sc_race2_do_gamble{result = Reason})
    end,
    State;
do_handle_msg(#cs_race2_final_info{}, RoleID, State) ->
    case get(?pd_last_list) of
        [{1,First1}|[{2,First2}|[{3,First3}|_]]] ->
            Final = 
                lists:map(fun(RID)->
                        #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(RID),
                        RolePub = role_lib:get_rolePublic(RID),
                        #arena_fighter{role_id = RolePub#rolePublic.roleID 
                                       ,role_name = RolePub#rolePublic.roleName
                                       ,is_male = RolePub#rolePublic.isMale 
                                       ,title = RolePub#rolePublic.title 
                                       ,head = RolePub#rolePublic.head
                                       ,power = RolePub#rolePublic.fightPower
                                       ,pos = 0
                                       ,level = RolePub#rolePublic.level
                                       ,skinID = SkinID
                                       ,trSpecialID=RolePub#rolePublic.trSpecial
                                       ,total_gamble = 0}
                    end, [First1,First2,First3]);
        _ ->
            Final = []
    end,
    send_client_msg(RoleID,#sc_race2_final_info{winner_list=Final}),
    State;
do_handle_msg(#cs_race2_cancel_sign{}, RoleID, State) ->
    case lists:keytake(RoleID, 1, get(?pd_sign_list)) of
        _ when State#state.step /= 1 ->
            send_client_msg(RoleID,#sc_race2_cancel_sign{result=4});
        {value,{RoleID,SelectZone},Other} ->
            {Type,CostNum} = data_race2:get(cancel_sign_cost),
            CancelTime = erlang:length([R||R<-get(?pd_cancel_time),R =:= RoleID]),
            Cost = (CancelTime + 1) * CostNum,
            case catch role_lib:call_server(RoleID, {deduct_gold,Type,Cost,?MONEY_DEC_TYPE_RACE_CANCEL_SIGN,0}, 500) of
                ok ->
                    put(?pd_sign_list,Other),
                    OldNum = get({?pd_sign_num_list,SelectZone}),
                    put({?pd_sign_num_list,SelectZone},OldNum-1),
                    put(?pd_cancel_time,[RoleID|get(?pd_cancel_time)]),
                    send_client_msg(RoleID,#sc_race2_cancel_sign{result=1});
                _ ->
                    send_client_msg(RoleID,#sc_race2_cancel_sign{result=2})
            end;
        _ ->
            send_client_msg(RoleID,#sc_race2_cancel_sign{result=3})
    end,
    State;
do_handle_msg(Info, _RoleID, State) ->
    ?ERR("unknown Msg:~w", [Info]),
    State.

%% ====================================================================
%% Other Internal functions
%% ====================================================================

set_persist_interval()->
    erlang:send_after(?persist_interval, self(), persist_tick).

set_check_interval()->
    erlang:send_after(?check_interval, self(), check_tick).

is_persist({?pd_sign_list,_}) ->
    true;
is_persist({{?pd_arena_list,_},_}) ->
    true;
is_persist({{?pd_group_role_list,_},_}) ->
    true;
is_persist({{?pd_group_fight_record,_},_}) ->
    true;
is_persist({?pd_fight_step,_}) ->
    true;
is_persist({{?pd_fight_step,_},_}) ->
    true;
is_persist({{?pd_knockout_list,_},_}) ->
    true;
is_persist({?pd_gamble_list,_}) ->
    true;
is_persist({?pd_final_list,_}) ->
    true;
is_persist({?pd_last_list,_}) ->
    true;   
is_persist({?pd_cancel_time,_}) ->
    true;   
is_persist(_) ->
    false.

do_persist(State) ->
    ProcDictList = [E || E<-erlang:get(), is_persist(E) ],
    db_sql:set_etc(?DB_ETC_KEY_RACE2,{{state,State},ProcDictList}).

do_check(State)->
    L = if
            State#state.step =:= 3 ->
                [case check_extra_fight(Zone) of
                    ignore ->
                        ignore;        %% 检查是不是需要加时
                    AddStepList when erlang:is_list(AddStepList)->
                        put({?pd_fight_step,Zone},AddStepList)
                end||Zone<-?zone_list],
                erlang:length(lists:merge([get({?pd_fight_step,Zone})||Zone<-?zone_list]));
            4 =< State#state.step andalso State#state.step =< 6  ->
                erlang:length(get(?pd_fight_step));
            true ->
                0
        end,
    do_check2(State).

do_check2(State)->
    WeekNum = calendar:day_of_the_week(date()),
    Time = erlang:time(),
    PdDebug = get(?pd_debug),
    L = if
            State#state.step =:= 3 ->
                erlang:length(lists:merge([get({?pd_fight_step,Zone})||Zone<-?zone_list]));
            4 =< State#state.step andalso State#state.step =< 6  ->
                erlang:length(get(?pd_fight_step));
            true ->
                0
        end,
    NowState = 
        if
            State#state.step =:= 0 ->
                if
                    WeekNum =:= 1 ->
                        do_next_step(WeekNum,State);       %% 赛季最开始
                    true ->
                        State   %% 开服后赛季还没开始
                end;
            WeekNum =:= State#state.step + 1 andalso PdDebug =:= ?undefined andalso L =:= 0 ->
                do_next_step(WeekNum,State);
            State#state.step =:= 7 andalso WeekNum =:= 1 andalso PdDebug =:= ?undefined andalso L =:= 0 ->
                do_next_step(WeekNum,State);
            State#state.step =:= 3 ->
                lists:foreach(fun(Zone) ->
                            case get({?pd_fight_step,Zone}) of
                                [] ->
                                    case check_extra_fight(Zone) of
                                        ignore ->
                                            ignore;        %% 检查是不是需要加时
                                        AddStepList when erlang:is_list(AddStepList)->
                                            %% 一般不会进入这段代码
                                            ?ERR("check_extra_fight ~w",[AddStepList]),
                                            put({?pd_fight_step,Zone},AddStepList)
                                    end;
                                [FightStep|Other] ->
                                    if
                                        FightStep#fight_step.timestamp < Time ->  %% 需要进行一轮比赛
                                            AddStep = do_group_fight_loop(Zone,FightStep),
                                            NewStepList = lists:sort(AddStep++Other),
                                            ?INFO("do_check2 -3- ~w l:~w ~w ~w",[AddStep,erlang:length(NewStepList),FightStep,Other]),
                                            case NewStepList of
                                                [] ->
                                                    case check_extra_fight(Zone) of
                                                        ignore ->
                                                            put({?pd_fight_step,Zone},NewStepList);
                                                        AddStepList when erlang:is_list(AddStepList)->
                                                            ?WARNING("check_extra_fight ~w",[AddStepList]),
                                                            put({?pd_fight_step,Zone},AddStepList)
                                                    end;
                                                _ ->
                                                    put({?pd_fight_step,Zone},NewStepList)
                                            end,
                                            send_knockout_fighter_list(State#state.step);
                                        true ->
                                            ignore
                                    end
                            end
                    end, ?zone_list),
                State;
            State#state.step =:= 4 ->
                case get(?pd_fight_step) of
                    [] ->
                        State;
                    [FightStep|Other] ->
                        if
                            FightStep#fight_step.timestamp < Time ->  %% 需要进行一轮比赛
                                do_group_fight_loop(0,FightStep),
                                put(?pd_fight_step,Other),
                                send_knockout_fighter_list(State#state.step),
                                do_check2(State);       %% 可能是连续的比赛
                            true ->
                                State
                        end
                end;
            State#state.step =:= 5 ->
                case get(?pd_fight_step) of
                    [] ->
                        State;
                    [FightStep|Other] ->
                        if
                            FightStep#fight_step.timestamp < Time ->  %% 需要进行一轮比赛
                                do_group_fight_loop(0,FightStep),
                                put(?pd_fight_step,Other),
                                send_knockout_fighter_list(State#state.step),
                                do_check2(State);       %% 可能是连续的比赛
                            true ->
                                State
                        end
                end;
            State#state.step =:= 6 ->
                case get(?pd_fight_step) of
                    [] ->
                        State;
                    [FightStep|Other] ->
                        if
                            FightStep#fight_step.timestamp < Time ->  %% 需要进行一轮比赛
                                do_group_fight_loop(0,FightStep),
                                put(?pd_fight_step,Other),
                                send_knockout_fighter_list(State#state.step),
                                do_check2(State);       %% 可能是连续的比赛
                            true ->
                                State
                        end
                end;
            true ->
%%                 ?ERR("do_check err step:~w now:~w",[State,WeekNum]),
                State
        end,
    NowState.

do_next_step(1,State)->
    init_proc_dict(),
    RobotList = db_sql:get_roleIDList_Robot(),
    RobotList16 = util:random_list(RobotList, 16),
    lists:foreach(fun(Z)->
            ZoneRoleList = lists:sublist(RobotList16, Z*4 - 3, 4),
            lists:foreach(fun(R)->
                    put(?pd_sign_list,[{R,Z}|get(?pd_sign_list)])
                end, ZoneRoleList),
            put({?pd_arena_list,Z},ZoneRoleList),
            put({?pd_sign_num_list,Z},4)
        end, ?zone_list),
    State#state{step=1};
do_next_step(2,State)->
    [put({?pd_knockout_list,Z},[])||Z<-?knockout_loop],
    [put({?pd_group_role_list,Z},[0,0,0,0])||Z<-?zone_list],
    put(?pd_gamble_list,[]),
    State#state{step=2};
do_next_step(3,State)->
    case get(?pd_debug) of
        10 ->
            lists:foreach(fun(Zone)-> 
                            put({?pd_group_role_list,Zone},get({?pd_arena_list,Zone}))
                end, ?zone_list);
        _ ->
            [T1|Other1] = get({?pd_arena_list,1}),
            [T2|Other2] = get({?pd_arena_list,2}),
            [T3|Other3] = get({?pd_arena_list,3}),
            [T4|Other4] = get({?pd_arena_list,4}),
            AllT = util:random_list2([T1,T2,T3,T4]),
            AllOther = util:random_list2(Other1 ++ Other2 ++ Other3 ++ Other4),
            lists:foldl(fun(Zone,{AccT,AccOther})-> 
                                [H|TT] = AccT,
                                {Tail,O} = lists:split(3, AccOther),
                                put({?pd_group_role_list,Zone},[H|Tail]),
                                {TT,O}
                        end, {AllT,AllOther}, ?zone_list)
    end,
    init_group_fight_loop(),
    [put({?pd_group_fight_record,Z},[])||Z<-?zone_list],
    [put({?pd_knockout_list,Z},[])||Z<-?knockout_loop],
    put(?pd_gamble_list,[]),
    State#state{step=3};
do_next_step(4,State)->
    {SeedList,OtherList,LoserList} = 
        lists:foldl(fun(Zone,{Acc1,Acc2,Acc3})-> 
                AllList = check_group_score(Zone),
                SortList = 
                    lists:sort(fun({_RoleA,ScoreA},{_RoleB,ScoreB})-> 
                            ScoreA > ScoreB
                        end, AllList),
                ?INFO("do_next_step ~w",[SortList]),
                {R1,_} = lists:nth(1, SortList),
                {R2,_} = lists:nth(2, SortList),
                {R3,_} = lists:nth(3, SortList),
                {R4,_} = lists:nth(4, SortList),
                {[R1|Acc1],[R2|Acc2],[R3,R4]++Acc3}
            end, {[],[],[]}, ?zone_list),
    put(?pd_final_list,[{9,R}||R<-LoserList]),
    {FighterCouple,_,_} = lists:foldl(fun(_,{AccList,AccSeedList,AccOtherList})-> 
                [T|NewSeedList] = util:random_list2(AccSeedList),
                [O|NewOtherList] = util:random_list2(AccOtherList),
                {[[T,O]|AccList],NewSeedList,NewOtherList}
        end, {[],SeedList,OtherList}, lists:seq(1, 4)),
    KnockoutFightTimestamp = data_race2:get(knockout_fight_timestamp),
    KnockoutStep = 
        lists:foldl(fun(Fighter2,AccList)->
                L = erlang:length(AccList),
                [#fight_step{timestamp = lists:nth(E+L, KnockoutFightTimestamp)
                            ,fight_type = 4
                            ,fighter = Fighter2}||E<-lists:seq(1, 5)] ++ AccList
            end, [], FighterCouple),
    SortedKnockoutStep = lists:sort(fun(RA,RB)-> RA#fight_step.timestamp < RB#fight_step.timestamp end, KnockoutStep),
    put(?pd_fight_step,SortedKnockoutStep),
    State#state{step=4};
do_next_step(5,State)->
    SortedR = lists:sort(fun(RA,RB)-> RA#fight_step.timestamp > RB#fight_step.timestamp end, get({?pd_knockout_list,4})),
    {[R1,R2,R3,R4],LoserList1} = get_winner5(SortedR,{[],[]}),        %% 四个胜出者无顺序
    LoserList2 = [{5,R}||R<-LoserList1],
    put(?pd_final_list,LoserList2++get(?pd_final_list)),
    FighterCouple = [[R1,R2],[R3,R4]],
    KnockoutFightTimestamp = data_race2:get(knockout_fight_timestamp),
    KnockoutStep = 
        lists:foldl(fun(Fighter2,AccList)->
                L = erlang:length(AccList),
                [#fight_step{timestamp = lists:nth(E+L, KnockoutFightTimestamp)
                            ,fight_type = 5
                            ,fighter = Fighter2}||E<-lists:seq(1, 5)] ++ AccList
            end, [], FighterCouple),    
    
    SortedKnockoutStep = lists:sort(fun(RA,RB)-> RA#fight_step.timestamp < RB#fight_step.timestamp end, KnockoutStep),
    put(?pd_fight_step,SortedKnockoutStep),
    State#state{step=5};
do_next_step(6,State)->
    SortedR = lists:sort(fun(RA,RB)-> RA#fight_step.timestamp > RB#fight_step.timestamp end, get({?pd_knockout_list,5})),
    {[R1,R2],LoserList} = get_winner5(SortedR,{[],[]}),  %% 两个胜出者无顺序
    %% 亚军先进行比赛
    FighterCouple = [LoserList,[R1,R2]],
    KnockoutFightTimestamp = data_race2:get(knockout_fight_timestamp),
    KnockoutStep = 
        lists:foldl(fun(Fighter2,AccList)->
                L = erlang:length(AccList),
                [#fight_step{timestamp = lists:nth(E+L, KnockoutFightTimestamp)
                            ,fight_type = 6
                            ,fighter = Fighter2}||E<-lists:seq(1, 5)] ++ AccList
            end, [], FighterCouple),
    SortedKnockoutStep = lists:sort(fun(RA,RB)-> RA#fight_step.timestamp < RB#fight_step.timestamp end, KnockoutStep),
    
    put(?pd_fight_step,SortedKnockoutStep),
    State#state{step=6};
do_next_step(7,State)->
    ResultList = lists:sort(get({?pd_knockout_list,6})),
    {[First,Third],_} = get_winner5(ResultList,{[],[]}),     %% 胜出者有顺序，因为先进行季军赛，然后顺序被get_winner5颠倒，第一个就是冠军
    AllFighter = lists:usort(lists:merge([R#fight_step.fighter||R<-ResultList])),
    Res1th = lists:nth(1, ResultList),
    ResLast = lists:last(ResultList),
    [Forth] = Res1th#fight_step.fighter -- [Third],
    [Second] = ResLast#fight_step.fighter -- [First],
    put(?pd_final_list,[{1,First},{2,Second},{3,Third},{4,Forth}]++get(?pd_final_list)),
    put(?pd_last_list,get(?pd_final_list)),
    send_final_reward(),
    State#state{step=7};
do_next_step(Step,State)->
    ?ERR("do_next_step err:(~w)~w pd:~w",[Step,State,get()]),
    State.

init_proc_dict()->
    put(?pd_sign_list,[]),
    [put({?pd_arena_list,Z},[0,0,0,0])||Z<-?zone_list],
    [put({?pd_group_role_list,Z},[0,0,0,0])||Z<-?zone_list],
    [put({?pd_group_fight_record,Z},[0,0,0,0])||Z<-?zone_list],
    put(?pd_fight_step,[]),
    [put({?pd_fight_step,Z},[])||Z<-?zone_list],
    [put({?pd_knockout_list,Z},[])||Z<-?knockout_loop],
    put(?pd_gamble_list,[]),
    put(?pd_final_list,[]),
    put(?pd_cancel_time,[]).

set_open(RoleID)->
    NewList = lists:delete(RoleID, get(?pd_open_list)),
    put(?pd_open_list,[RoleID|NewList]).

set_close(RoleID)->
    NewList = lists:delete(RoleID, get(?pd_open_list)),
    put(?pd_open_list,NewList).

check_gamble(RoleID,GambleType,Winner,Gold,Step) ->
    NowTime = erlang:time(),
    [MinTime,MaxTime] = data_race2:get(gamble_time),
%%     FightRec = if
%%         Step =:= 3 ->
%%             get({?pd_group_fight_record,GambleType rem 10});
%%         4 =< Step andalso  Step =< 6 ->
%%             get({?pd_knockout_list,Step});
%%         true ->
%%             ?undefined
%%     end,
    if
          MinTime =< NowTime andalso NowTime =< MaxTime ->
            case lists:member(GambleType, check_gamble_step(Step)) of
                true ->
                    R = check_gamble2(RoleID,GambleType,Winner),
                    ?INFO("check_gamble ~w",[R]),
                    R;
                false ->
                    {false,3}
            end;
        true ->
            {false,2}
    end.

check_gamble_step(3)->
    [31,32,33,34];
check_gamble_step(4)->
    [4];
check_gamble_step(5)->
    [5];
check_gamble_step(6)->
    [6];
check_gamble_step(_)->
    [].

check_gamble2(RoleID,GambleType,Winner) when 31 =< GambleType andalso GambleType =< 34->
    case lists:member(Winner, get({?pd_group_role_list,GambleType rem 30})) of
        false ->
            {false,3};
        true ->
            case get({?pd_fight_step,GambleType rem 30}) of
                [FightStep|_Other] when FightStep#fight_step.fight_type =:= 21 orelse FightStep#fight_step.fight_type =:= 22 ->
                    [{A,B},_] = FightStep#fight_step.fighter,
                    case lists:member(Winner,[A,B]) of
                        false ->
                            {false,3};
                        true ->
                            {true,FightStep#fight_step.timestamp,GambleType}
                    end;
                [FightStep|_Other] when FightStep#fight_step.fight_type =:= 211 orelse FightStep#fight_step.fight_type =:= 221 ->
                    [_,{A,B}] = FightStep#fight_step.fighter,
                    case lists:member(Winner,[A,B]) of
                        false ->
                            {false,3};
                        true ->
                            {true,FightStep#fight_step.timestamp,GambleType}
                    end;
                [FightStep|_Other] ->
                    case lists:member(Winner,FightStep#fight_step.fighter) of
                        false ->
                            {false,3};
                        true ->
                            {true,FightStep#fight_step.timestamp,GambleType}
                    end;
                [] ->
                    {false,3}
            end
    end;
check_gamble2(RoleID,GambleType,Winner) when 4 =:= GambleType orelse GambleType =:= 5->
    AllGroupIDList = lists:merge([get({?pd_group_role_list,I})||I<-lists:seq(1, 4)]),
    {ShouldLength,ShouldList} = if
                                    GambleType =:= 4 ->
                                        {4,AllGroupIDList 
                                            -- [RID||{T,RID}<-get(?pd_final_list),T =:= 9]};
                                    GambleType =:= 5 ->
                                        {2,AllGroupIDList 
                                            -- [RID||{T,RID}<-get(?pd_final_list),(T =:= 9 orelse T =:= 5)]}
                                end,
    case lists:member(Winner,ShouldList) of
        false ->
            {false,3};
        true ->
            case get(?pd_fight_step) of
                [FightStep|_Other] ->
                    case lists:member(Winner,FightStep#fight_step.fighter) of
                        false ->
                            {false,3};
                        true ->
                            {true,FightStep#fight_step.timestamp,GambleType}
                    end;    
                [] ->
                    {false,3}
            end
    end;
check_gamble2(RoleID,GambleType0,Winner) when 6 =:= GambleType0 ->
    {WinnerList,LoserList} = get_winner5(get({?pd_knockout_list,5}),{[],[]}),
    IsWinner = lists:member(Winner, WinnerList),
    GambleType = if true =:= IsWinner -> 61; true -> 62 end,
    ShouldList = if 61 =:= GambleType -> WinnerList; true -> LoserList end,
    case lists:member(Winner, ShouldList) of
        false ->
            {false,3};
        true ->
            case get(?pd_fight_step) of
                [FightStep|_Other] ->
                    case lists:member(Winner,FightStep#fight_step.fighter) of
                        false ->
                            {false,3};
                        true ->
                            {true,FightStep#fight_step.timestamp,GambleType}
                    end;    
                [] ->
                    {false,3}
            end
%% 改成不限制
%%             SelfGamble = lists:filter(fun({RID,_WID,_Gold,GType})->
%%                             RID =:=  RoleID andalso GType =:= GambleType
%%                 end, get(?pd_gamble_list)),
%%             L = erlang:length(SelfGamble),
%%             if
%%                 L >= 1 ->
%%                     {false,5};
%%                 true ->
%%                     true
%%             end
    end.

send_arena_fighter_list(RoleID) when erlang:is_integer(RoleID)->
    ArenaFighterList = 
        lists:foldl(fun(Z,AccList)->
                {FighterList,_} = 
                    lists:foldl(fun(RID,{AccSubList,Index})-> 
                            RolePub = role_lib:get_rolePublic(RID),
                            #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(RID),
                            F = #arena_fighter{role_id = RID
                                              ,role_name = RolePub#rolePublic.roleName
                                              ,is_male = RolePub#rolePublic.isMale 
                                              ,title = RolePub#rolePublic.title 
                                              ,head = RolePub#rolePublic.head
                                              ,power = RolePub#rolePublic.fightPower
                                              ,pos = Z*10+Index
                                              ,level = RolePub#rolePublic.level
                                              ,skinID = SkinID
                                              ,trSpecialID=RolePub#rolePublic.trSpecial
                                       ,total_gamble = 0},
                            {[F|AccSubList],Index+1}
                        end, {[],1}, get({?pd_arena_list,Z})),
                FighterList ++ AccList
            end, [], ?zone_list),
    send_client_msg(RoleID,#sc_race2_arena_fighter{arena_fighter_list=ArenaFighterList});
send_arena_fighter_list(RoleIDList) when erlang:is_list(RoleIDList)->
    ArenaFighterList = 
        lists:foldl(fun(Z,AccList)->
                {FighterList,_} = 
                    lists:foldl(fun(RID,{AccSubList,Index})-> 
                            RolePub = role_lib:get_rolePublic(RID),
                            #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(RID),
                            F = #arena_fighter{role_id = RID
                                              ,role_name = RolePub#rolePublic.roleName
                                              ,is_male = RolePub#rolePublic.isMale 
                                              ,title = RolePub#rolePublic.title 
                                              ,head = RolePub#rolePublic.head
                                              ,power = RolePub#rolePublic.fightPower
                                              ,pos = Z*10+Index
                                              ,level = RolePub#rolePublic.level
                                              ,skinID = SkinID
                                              ,trSpecialID=RolePub#rolePublic.trSpecial
                                       ,total_gamble = 0},
                            {[F|AccSubList],Index+1}
                        end, {[],1}, get({?pd_arena_list,Z})),
                FighterList ++ AccList
            end, [], ?zone_list),
    lists:foreach(fun(BRID)-> 
                          send_client_msg(BRID,#sc_race2_arena_fighter{arena_fighter_list=ArenaFighterList})
                end, RoleIDList).

send_knockout_fighter_list(StepNum)->
    FightStepLen = erlang:length(get(?pd_fight_step)),
    if
        (FightStepLen rem 5) =:= 0 orelse StepNum =:= 3 ->
            L = case StepNum of
                    3 ->
                        lists:foldl(fun(Zone,AccList) ->
                                SubList = lists:foldl(fun(StepInfo,Acc)-> 
                                                      case StepInfo#fight_step.fighter of
                                                          [X,Y] when erlang:is_integer(X) andalso erlang:is_integer(Y) ->
                                                              [step_info_to_msg(StepInfo,3*10+Zone,erlang:length(Acc)+1)|Acc];
                                                          [{_,_},{X, Y}] when erlang:is_integer(X) andalso erlang:is_integer(Y) andalso StepInfo#fight_step.rec =:= []
                                                            andalso (StepInfo#fight_step.fight_type =:= 211 orelse StepInfo#fight_step.fight_type =:= 221) ->
                                                              [step_info_to_msg(StepInfo#fight_step{fighter=[X,Y]},3*10+Zone,erlang:length(Acc)+1)|Acc];
                                                          [{X, Y},_] when erlang:is_integer(X) andalso erlang:is_integer(Y) andalso StepInfo#fight_step.rec =:= [] ->
                                                              [step_info_to_msg(StepInfo#fight_step{fighter=[X,Y]},3*10+Zone,erlang:length(Acc)+1)|Acc];
                                                          [A1,{X, Y}] when erlang:is_integer(A1) andalso erlang:is_integer(X) andalso erlang:is_integer(Y) andalso StepInfo#fight_step.rec =:= [] ->
                                                              [step_info_to_msg(StepInfo#fight_step{fighter=[X,Y]},3*10+Zone,erlang:length(Acc)+1)|Acc];
                                                          _ ->
                                                              Acc
                                                      end
                                                end, [], get({?pd_group_fight_record,Zone})++get({?pd_fight_step,Zone})),
                                ListSorted = lists:sort(fun(RA,RB)-> 
                                                        RA#race2_fight_info.fight_timestamp < RB#race2_fight_info.fight_timestamp 
                                                end, SubList),
                                ListSorted ++ AccList
                            end, [], lists:reverse(?zone_list));
                    _ ->
                        SubList = lists:foldl(fun(StepInfo,Acc)->
                                        [step_info_to_msg(StepInfo,StepNum,erlang:length(Acc)+1)|Acc] 
                            end, [], get({?pd_knockout_list,StepNum})++get(?pd_fight_step)),
                        lists:sort(fun(RA,RB)-> 
                                        RA#race2_fight_info.fight_timestamp < RB#race2_fight_info.fight_timestamp 
                            end, SubList)
                end,
            Msg = #sc_race2_knockout_fighter{fight_info_list = L},
            send_knockout_fighter_list(Msg,get(?pd_open_list));
        true ->
            ignore
    end.
send_knockout_fighter_list(_Msg,[])->
    ok;
send_knockout_fighter_list(Msg,[H|T])->
    send_client_msg(H,Msg),
    send_knockout_fighter_list(Msg,T).
    

%% FightInfoType,3阶段是FightInfoType范围是31~34,4~6阶段时，FightInfoType范围是4~6
%% 没有发生战斗的
step_info_to_msg(StepInfo,FightInfoType,_Index) when StepInfo#fight_step.rec =:= []->
    Fighter = 
        lists:map(fun(R)->
                RolePub = role_lib:get_rolePublic(R),
                #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(R),
                TotalGamble = 
                    lists:foldl(fun({_RID,WID,Gold,GType,_GI,S},AccValue) when WID =:= R ->
                                    if
                                        (FightInfoType =:= 6 andalso (GType =:= 61 orelse GType =:= 62))
                                          orelse FightInfoType =:= GType ->
                                            AccValue + Gold;
                                        true ->
                                            AccValue
                                    end;
                                (_,AccValue) ->
                                    AccValue
                        end, 0, get(?pd_gamble_list)),
                ?INFO("step_info_to_msg TotalGamble:(~w)~w",[FightInfoType,TotalGamble]),
                #arena_fighter{role_id = R
                              ,role_name = RolePub#rolePublic.roleName
                              ,is_male = RolePub#rolePublic.isMale 
                              ,title = RolePub#rolePublic.title 
                              ,head = RolePub#rolePublic.head
                              ,power = RolePub#rolePublic.fightPower
                              ,pos = 0
                              ,level = RolePub#rolePublic.level
                              ,skinID = SkinID
                              ,trSpecialID=RolePub#rolePublic.trSpecial
                              ,total_gamble = TotalGamble}
            end, StepInfo#fight_step.fighter),
    #race2_fight_info{fighter = Fighter
                     ,fight_timestamp = util:datetime_to_seconds({erlang:date(),StepInfo#fight_step.timestamp}) 
                     ,left_score = 0
                     ,right_score = 0
                     ,fight_rec_id = 0
                     ,winner = 0
                     ,fight_type = FightInfoType
                     ,dice_numA = 0
                     ,dice_numB = 0};
%% 已经发生战斗的。 FightInfoType:31~34,4~6
step_info_to_msg(StepInfo,FightInfoType,Index) when StepInfo#fight_step.rec /= []->
    FightR = StepInfo#fight_step.rec,
    [A,B] = StepInfo#fight_step.fighter,
    IsSwitch = FightR#sc_race2_fight.dice_numB > FightR#sc_race2_fight.dice_numA,
    [FightRec] = FightR#sc_race2_fight.fight_rec,
    Winner = if FightRec#sc_fight_request.result /= IsSwitch -> A;
                true -> B end,
    Fighter = 
        lists:map(fun(R)->
                RolePub = role_lib:get_rolePublic(R),
                #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(R),
                TotalGamble = 
                    lists:foldl(fun({_RID,WID,Gold,GType,_GI,S},AccValue) when WID =:= R->
                                    if
                                        (FightInfoType =:= 6 andalso (GType =:= 61 orelse GType =:= 62))
                                          orelse FightInfoType =:= GType ->
                                            AccValue + Gold;
                                        true ->
                                            AccValue
                                    end;
                                (_,AccValue) ->
                                    AccValue
                        end, 0, get(?pd_gamble_list)),
                ?INFO("step_info_to_msg TotalGamble:(~w)~w",[FightInfoType,TotalGamble]),
                #arena_fighter{role_id = R
                              ,role_name = RolePub#rolePublic.roleName
                              ,is_male = RolePub#rolePublic.isMale 
                              ,title = RolePub#rolePublic.title 
                              ,head = RolePub#rolePublic.head
                              ,power = RolePub#rolePublic.fightPower
                              ,pos = 0
                              ,level = RolePub#rolePublic.level
                              ,skinID = SkinID
                              ,trSpecialID=RolePub#rolePublic.trSpecial
                              ,total_gamble = TotalGamble}
            end, StepInfo#fight_step.fighter),
    {ScoreL,ScoreR} = if
                4 =< FightInfoType andalso FightInfoType =< 6 ->
                    {0,0};
                Winner =:= A andalso Index > ?group_fight_normal ->
                    {1,-1};
                Winner =:= A andalso Index =< ?group_fight_normal ->
                    {1,0};
                Winner /= A andalso Index > ?group_fight_normal ->
                    {-1,1};
                Winner /= A andalso Index =< ?group_fight_normal ->
                    {0,1}
            end,
    #race2_fight_info{fighter = Fighter
                     ,fight_timestamp = util:datetime_to_seconds({erlang:date(),StepInfo#fight_step.timestamp}) 
                     ,left_score = ScoreL
                     ,right_score = ScoreR
                     ,fight_rec_id = FightInfoType*100 + Index
                     ,winner = Winner
                     ,fight_type = FightInfoType
                     ,dice_numA = FightR#sc_race2_fight.dice_numA
                     ,dice_numB = FightR#sc_race2_fight.dice_numB}.

init_group_fight_loop()->
    Interval = data_race2:get(group_fight_interval),
    StepModelT = 
        lists:foldl(fun(LoopTime,AccList)-> 
                S_LoopTime = calendar:time_to_seconds(LoopTime),
                {SubList,_} = 
                    lists:foldl(fun({A,B},{AccL,Index})-> 
                            NewStep = #fight_step{timestamp = calendar:seconds_to_time(S_LoopTime + (Index*Interval))
                                                 ,fight_type = 1
                                                 ,fighter = [A,B]},
                            {[NewStep|AccL],Index+1}
                        end, {[],0}, util:random_list(?group_rule)),    %% 每轮顺序要不一样
                SubList ++ AccList
            end, [], data_race2:get(group_fight_timestamp)),
    StepModel = lists:sort(lists:reverse(StepModelT)),
    lists:foreach(fun(Z) -> 
           StepList = 
                lists:foldr(fun(Step,AccList)-> 
                                    [A,B] = Step#fight_step.fighter,
                                    RoleA = lists:nth(A,get({?pd_group_role_list,Z})),
                                    RoleB = lists:nth(B,get({?pd_group_role_list,Z})),
                                    [Step#fight_step{fighter = [RoleA,RoleB]}|AccList]
                            end, [], StepModel),
            put({?pd_fight_step,Z},StepList)
        end, ?zone_list).

%% 普通循环赛，胜者加一分，负者不扣分
do_group_fight_loop(Zone,FightStep) when FightStep#fight_step.fight_type =:= 1 ->
    [RoleIDA, RoleIDB] = FightStep#fight_step.fighter,
    FightResult = do_group_fight(RoleIDA, RoleIDB),
    OldOtherRecList = get({?pd_group_fight_record,Zone}),
    NewRecList = lists:sort(fun(RA,RB)-> 
                                    RA#fight_step.timestamp < RB#fight_step.timestamp 
                            end, [FightStep#fight_step{rec = FightResult}|OldOtherRecList]),
    Winner = get_winner(FightStep#fight_step{rec = FightResult}),
    [Loser] = [RoleIDA, RoleIDB] -- [Winner],
    close_gamble(Winner,Loser),
    put({?pd_group_fight_record,Zone},NewRecList),
    [];
%% 循环赛后的加赛，加赛负者也要扣一分
do_group_fight_loop(Zone,FightStep) when FightStep#fight_step.fight_type =:= 21 orelse FightStep#fight_step.fight_type =:= 22 ->
    ?INFO("do_group_fight_loop extra ~w",[FightStep]),
    [{RoleIDA, RoleIDB},R3] = FightStep#fight_step.fighter,     %% TODO
    FightResult = do_group_fight(RoleIDA, RoleIDB),
    OldOtherRecList = get({?pd_group_fight_record,Zone}),
    FightStep2 = FightStep#fight_step{fighter = [RoleIDA, RoleIDB],rec = FightResult},
    NewRecList = lists:sort(fun(RA,RB)-> 
                                    RA#fight_step.timestamp < RB#fight_step.timestamp 
                            end, [FightStep2|OldOtherRecList]),
    Winner = get_winner(FightStep2),
    [Loser] = [RoleIDA, RoleIDB] -- [Winner],
    close_gamble(Winner,Loser),
    put({?pd_group_fight_record,Zone},NewRecList),
    NewT = calendar:seconds_to_time(calendar:time_to_seconds(FightStep#fight_step.timestamp) + data_race2:get(group_fight_interval)),
    [FightStep#fight_step{timestamp  = NewT
                         ,fighter = [{Winner,Loser},R3]
                         ,fight_type = (FightStep#fight_step.fight_type*10)+1}];
do_group_fight_loop(Zone,FightStep) when FightStep#fight_step.fight_type =:= 211 
                                    orelse FightStep#fight_step.fight_type =:= 221 ->
    ?INFO("do_group_fight_loop extra ~w",[FightStep]),
    [{R1,R2},{RoleIDA, RoleIDB}] = FightStep#fight_step.fighter,     %% TODO
    FightResult = do_group_fight(RoleIDA, RoleIDB),
    OldOtherRecList = get({?pd_group_fight_record,Zone}),
    FightStep2 = FightStep#fight_step{fighter = [RoleIDA, RoleIDB],rec = FightResult},
    NewRecList = lists:sort(fun(RA,RB)-> 
                                    RA#fight_step.timestamp < RB#fight_step.timestamp 
                            end, [FightStep2|OldOtherRecList]),
    Winner = get_winner(FightStep2),
    [Loser] = [RoleIDA, RoleIDB] -- [Winner],
    close_gamble(Winner,Loser),
    put({?pd_group_fight_record,Zone},NewRecList),
    Winner = get_winner(FightStep2),
    if
    %% 是RoleIDA, RoleIDB当中输的那个
        R1 =:= Loser ->
            [];
    %% 是RoleIDA, RoleIDB当中赢的那个
        R1 =:= Winner ->
            NewT = calendar:seconds_to_time(calendar:time_to_seconds(FightStep#fight_step.timestamp) + data_race2:get(group_fight_interval)),
            [FightStep#fight_step{timestamp  = NewT
                                 ,fighter = [R2,Loser]
                                 ,fight_type = (FightStep#fight_step.fight_type*10)+2}];
    %% 不是RoleIDA, RoleIDB当中任何一个
        true ->
            NewT = calendar:seconds_to_time(calendar:time_to_seconds(FightStep#fight_step.timestamp) + data_race2:get(group_fight_interval)),
            [FightStep#fight_step{timestamp  = NewT
                                 ,fighter = [R1,Winner]
                                 ,fight_type = (FightStep#fight_step.fight_type*10)+1}]
    end;
do_group_fight_loop(Zone,FightStep) when FightStep#fight_step.fight_type =:= 2111
                                    orelse FightStep#fight_step.fight_type =:= 2211 
                                    orelse FightStep#fight_step.fight_type =:= 2112
                                    orelse FightStep#fight_step.fight_type =:= 2212 ->
    ?INFO("do_group_fight_loop extra ~w",[FightStep]),
    [RoleIDA, RoleIDB] = FightStep#fight_step.fighter,
    FightResult = do_group_fight(RoleIDA, RoleIDB),
    OldOtherRecList = get({?pd_group_fight_record,Zone}),
    NewRecList = lists:sort(fun(RA,RB)-> 
                                    RA#fight_step.timestamp < RB#fight_step.timestamp 
                            end, [FightStep#fight_step{rec = FightResult}|OldOtherRecList]),
    Winner = get_winner(FightStep#fight_step{rec = FightResult}),
    [Loser] = [RoleIDA, RoleIDB] -- [Winner],
    close_gamble(Winner,Loser),
    put({?pd_group_fight_record,Zone},NewRecList),
    [];
do_group_fight_loop(Zone,FightStep) when FightStep#fight_step.fight_type =:= 23 
                                    orelse FightStep#fight_step.fight_type =:= 24 ->
    ?INFO("do_group_fight_loop extra ~w",[FightStep]),
    [RoleIDA, RoleIDB] = FightStep#fight_step.fighter,
    FightResult = do_group_fight(RoleIDA, RoleIDB),
    OldOtherRecList = get({?pd_group_fight_record,Zone}),
    NewRecList = lists:sort(fun(RA,RB)-> 
                                    RA#fight_step.timestamp < RB#fight_step.timestamp 
                            end, [FightStep#fight_step{rec = FightResult}|OldOtherRecList]),
    Winner = get_winner(FightStep#fight_step{rec = FightResult}),
    [Loser] = [RoleIDA, RoleIDB] -- [Winner],
    close_gamble(Winner,Loser),
    put({?pd_group_fight_record,Zone},NewRecList),
    [];
do_group_fight_loop(0,FightStep) when FightStep#fight_step.fight_type =:= 4 
                                orelse FightStep#fight_step.fight_type =:= 5 
                                orelse FightStep#fight_step.fight_type =:= 6 ->
    [RoleIDA, RoleIDB] = FightStep#fight_step.fighter,
    FightResult = do_group_fight(RoleIDA, RoleIDB),
    OldOtherRecList = get({?pd_knockout_list,FightStep#fight_step.fight_type}),
    NewRecList = lists:sort(fun(RA,RB)-> 
                                    RA#fight_step.timestamp < RB#fight_step.timestamp 
                            end, [FightStep#fight_step{rec = FightResult}|OldOtherRecList]),
    IsNeedCheckGamble = (erlang:length(NewRecList) rem 5) =:= 0,
    if
        IsNeedCheckGamble ->
            {First5,_O} = lists:split(5, lists:reverse(NewRecList)),
            {[Winner],[Loser]} = get_winner5(First5,{[],[]}),
            close_gamble(Winner,Loser);
        true ->
            ignore
    end,
    put({?pd_knockout_list,FightStep#fight_step.fight_type},NewRecList),
    [].

do_group_fight(RoleIDA, RoleIDB)->
    {FighterListA,RoleLieuAddA,TalentA,TrSpecialA} = role_data:get_otherRoleFighter(RoleIDA),
    {FighterListB,RoleLieuAddB,TalentB,TrSpecialB} = role_data:get_otherRoleFighter(RoleIDB),
    SkinInfoA = role_data:get_otherRoleSkinInfo(RoleIDA),
    SkinInfoB = role_data:get_otherRoleSkinInfo(RoleIDB),
%%     {DiceNumA,DiceNumB} = doublematch_match:random_first_atk(),
    EquipListA = role_data:get_otherRoleItemEquips(RoleIDA),
    GerEquipListA = role_item:assort_ger_equiplist(EquipListA),
    LegendAddListA = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipListA],
    EquipListB = role_data:get_otherRoleItemEquips(RoleIDB),
    GerEquipListB = role_item:assort_ger_equiplist(EquipListB),
    LegendAddListB = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipListB],
    {DiceNumA,DiceNumB} = {6,1},
    IsSwitch = DiceNumB>DiceNumA,
    {_Result, FightRecord, _State} = 
        case IsSwitch of
            false ->
                role_fight:new(FighterListA, FighterListB, RoleLieuAddA, RoleLieuAddB
                              , TalentA, TalentB,TrSpecialA,TrSpecialB
                              ,SkinInfoA,SkinInfoB,LegendAddListA,LegendAddListB);
            true ->
                role_fight:new(FighterListB,FighterListA,RoleLieuAddB,RoleLieuAddA
                              ,TalentB,TalentA,TrSpecialB,TrSpecialA
                              ,SkinInfoB,SkinInfoA,LegendAddListB,LegendAddListA)
        end,
    #sc_race2_fight{result = 1
                   ,dice_numA = DiceNumA
                   ,dice_numB = DiceNumB
                   ,fight_rec = [FightRecord]}.

check_extra_fight(Zone)->
    AllList = check_group_score(Zone),
    SortList = 
        lists:sort(fun({_RoleA,ScoreA},{_RoleB,ScoreB})-> 
                ScoreA > ScoreB
            end, AllList),
    ExtraFightInfo = 
        case SortList of
            [{R1,S1},{R2,S2},{R3,S3},{_R4,S4}] when S1 =:= S2 andalso S2 =:= S3 andalso 0 /= S1+S2+S3+S4 ->
                {21,[{R1,R2},{R2,R3}]};
            [{_R1,S1},{R2,S2},{R3,S3},{R4,S4}] when S2 =:= S3 andalso S3 =:= S4 andalso 0 /= S1+S2+S3+S4 ->
                {22,[{R2,R3},{R3,R4}]};
            [{R1,S1},{R2,S2},{_R3,S3},{_R4,S4}] when S1 =:= S2 andalso S2 =/= S3 andalso 0 /= S1+S2+S3+S4 ->
                {23,[R1,R2]};
            [{_R1,S1},{R2,S2},{R3,S3},{_R4,S4}] when S2 =:= S3 andalso S1 =/= S2 andalso S3 =/= S4 andalso 0 /= S1+S2+S3+S4 ->
                {24,[R2,R3]};
            _ ->
                ignore
        end,
    ?INFO("check_extra_fight (~w)~w----~w",[Zone,AllList,ExtraFightInfo]),
    case ExtraFightInfo of
        ignore ->
            ignore;
        {Type,ExtraFight} ->
            Last = lists:last(lists:sort(fun(RA,RB)-> 
                            RA#fight_step.timestamp < RB#fight_step.timestamp 
                    end, get({?pd_group_fight_record,Zone}))),
            TimeS = calendar:time_to_seconds(Last#fight_step.timestamp),
            [#fight_step{timestamp = calendar:seconds_to_time(TimeS + data_race2:get(group_fight_interval))
                        ,fight_type = Type
                        ,fighter = ExtraFight}]
    end.

check_group_score(Zone)->
    All = get({?pd_group_fight_record,Zone}),
    BlankList1 = lists:usort(lists:merge([E#fight_step.fighter||E<-All])),
    BlankList2 = [{R,0}||R<-BlankList1],
    lists:foldl(fun(R,AccList) when R#fight_step.rec  =:= [] -> 
                        AccList;
                   (R,AccList) when R#fight_step.fight_type =:= 1 -> 
                        Winner = get_winner(R),
                        case lists:keytake(Winner, 1, AccList) of
                            {value,{Winner,OldScore1},OtherAccList1} ->
                                [{Winner,OldScore1+10}|OtherAccList1];
                            false ->
                                [{Winner,0}|AccList]
                        end;
                   (R,AccList) -> 
                        ScoreChange = 1,
                        Winner = get_winner(R),
                        [Loser] = R#fight_step.fighter -- [Winner],
                        ?INFO("pd_group_fight_record ~w-~w----~w",[Winner,Loser,R]),
                        NewList0 = case lists:keytake(Winner, 1, AccList) of
                            {value,{Winner,OldScore1},OtherAccList1} ->
                                [{Winner,OldScore1+ScoreChange}|OtherAccList1];
                            false ->
                                [{Winner,ScoreChange}|AccList]
                        end,
                        case lists:keytake(Loser, 1, NewList0) of
                            {value,{Loser,OldScore2},OtherAccList2} ->
                                [{Loser,OldScore2-ScoreChange}|OtherAccList2];
                            false ->
                                [{Loser,-ScoreChange}|NewList0]
                        end
        end, BlankList2, All).

get_winner(R)->
    FightR = R#fight_step.rec,
    [A,B] = case R#fight_step.fighter of    %% TODO
                [{RoleIDA, RoleIDB},_R3] ->
                    [RoleIDA, RoleIDB];
                [R1,{RoleIDA, RoleIDB}] when erlang:is_integer(R1) ->
                    [RoleIDA, RoleIDB];
                [A0,B0] ->
                    [A0,B0]
            end,
    IsSwitch = FightR#sc_race2_fight.dice_numB > FightR#sc_race2_fight.dice_numA,
    [FightRec] = FightR#sc_race2_fight.fight_rec,
    if
        FightRec#sc_fight_request.result /= IsSwitch ->
            A;
        true ->
            B
    end.

get_winner5([],{AccListW,AccListL})->
    {AccListW,AccListL};
get_winner5([R1|[R2|[R3|[R4|[R5|Other]]]]],{AccListW,AccListL})->
    {SA,SB} = 
        lists:foldl(fun(R,{Acc1,Acc2})->
                [A0,_B0] = R#fight_step.fighter,
                Winner = get_winner(R),
                if
                    A0 =:= Winner ->
                        {Acc1+1,Acc2};
                    true ->
                        {Acc1,Acc2+1}
                end
            end, {0,0}, [R1,R2,R3,R4,R5]),
    [A,B] = R1#fight_step.fighter,
    if
        SA > SB ->
            get_winner5(Other,{[A|AccListW],[B|AccListL]});
        true ->
            get_winner5(Other,{[B|AccListW],[A|AccListL]})
    end.

send_client_msg(RoleID,Msg)->
    catch ?unicast(RoleID,Msg).

close_gamble(Winner,Loser) when erlang:is_integer(Winner) andalso erlang:is_integer(Loser) ->
    put(?pd_gamble_list,close_gamble(Winner,Loser,get(?pd_gamble_list),[])).
close_gamble(_Winner,_Loser,[],AccList)->
    AccList;
close_gamble(Winner,Loser,[{RoleID,WinnerGamble,Gold,GambleType,GIndex,GambleState}|Other],AccList) 
  when GambleState /= ?gamble_fight orelse (WinnerGamble =/= Winner andalso WinnerGamble =/= Loser)->
    close_gamble(Winner,Loser,Other,[{RoleID,WinnerGamble,Gold,GambleType,GIndex,GambleState}|AccList]);
close_gamble(Winner,Loser,[Gamble|Other],AccList)->
    {RoleID,WinnerGamble,Gold,GambleType,GIndex,_} = Gamble,
    WinnerRolePub = role_lib:get_rolePublic(WinnerGamble),
    IsXiaozusai = lists:member(GambleType, [31,32,33,34]),
    IsRight = WinnerGamble =:= Winner,
    case IsRight of
        true when GambleType =:= 61 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_GUANJUN_RIGHT
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,Gold,Gold*2], ""
                                     ,{sell_reward,Gold*2,0,0,0,[],0,[]});
        true when GambleType =:= 62 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_JIJUN_RIGHT
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,Gold,Gold*2], ""
                                     ,{sell_reward,Gold*2,0,0,0,[],0,[]});
        true when GambleType =:= 5 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_BANJUESAI_RIGHT
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,Gold,Gold*2], ""
                                     ,{sell_reward,Gold*2,0,0,0,[],0,[]});
        true when GambleType =:= 4 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_BAQIANG_RIGHT
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,Gold,Gold*2], ""
                                     ,{sell_reward,Gold*2,0,0,0,[],0,[]});
        true when IsXiaozusai =:= true ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_XIAOZUSAI_RIGHT
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,Gold,Gold*2], ""
                                     ,{sell_reward,Gold*2,0,0,0,[],0,[]});
        false when GambleType =:= 61 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_GUANJUN_WRONG
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,0], ""
                                     ,[]);
        false when GambleType =:= 62 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_JIJUN_WRONG
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,0], ""
                                     ,[]);
        false when GambleType =:= 5 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_BANJUESAI_WRONG
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,0], ""
                                     ,[]);
        false when GambleType =:= 4 ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_BAQIANG_WRONG
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,0], ""
                                     ,[]);
        false when IsXiaozusai =:= true ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RACE2_GAMBLE_XIAOZUSAI_WRONG
                                     ,[WinnerRolePub#rolePublic.roleName, Gold,0], ""
                                     ,[]);
        _ ->
            ignore
    end,
    GambleRes = if IsRight -> ?gamble_right; true -> ?gamble_wrong end,
    close_gamble(Winner,Loser,Other,[{RoleID,WinnerGamble,Gold,GambleType,GIndex,GambleRes}|AccList]).

send_final_reward()->
    lists:foreach(fun
                     ({1,RoleID})-> 
                        mail_server:send_sys_mail(RoleID, ?MAIL_RACE_FIRST_REWARD, [], ""
                                                 ,change_sell_reward_by_sex(data_race2:get({final_reward,1}),RoleID));
                     ({2,RoleID})-> 
                        mail_server:send_sys_mail(RoleID, ?MAIL_RACE_SECOND_REWARD, [], ""
                                                 ,change_sell_reward_by_sex(data_race2:get({final_reward,2}),RoleID));
                     ({3,RoleID})-> 
                        mail_server:send_sys_mail(RoleID, ?MAIL_RACE_THIRD_REWARD, [], ""
                                                 ,change_sell_reward_by_sex(data_race2:get({final_reward,3}),RoleID));
                     ({4,RoleID})-> 
                        mail_server:send_sys_mail(RoleID, ?MAIL_RACE_FIRST_FOUR_REWARD, [], ""
                                                 ,change_sell_reward_by_sex(data_race2:get({final_reward,4}),RoleID));
                     ({5,RoleID})-> 
                        mail_server:send_sys_mail(RoleID, ?MAIL_RACE_FIRST_EIGHT_REWARD, [], ""
                                                 ,change_sell_reward_by_sex(data_race2:get({final_reward,5}),RoleID));
                     ({9,RoleID})-> 
                        mail_server:send_sys_mail(RoleID, ?MAIL_RACE_GROUP_FIRST_FOUR_REWARD, [], ""
                                                 ,change_sell_reward_by_sex(data_race2:get({final_reward,9}),RoleID))
        end, get(?pd_final_list)),
    AllList = [A||{A,_B}<-get(?pd_sign_list)] -- [RID||{_,RID}<-get(?pd_final_list)],
    lists:foreach(fun(RID)-> 
                        mail_server:send_sys_mail(RID, ?MAIL_RACE_GROUP_SIGN_REWARD, [], ""
                                                 ,change_sell_reward_by_sex(data_race2:get({final_reward,0}),RID))
                  end, AllList).

get_self_group_num(RoleID)->
    get_self_group_num(RoleID,?zone_list).
get_self_group_num(_,[])->
    0;
get_self_group_num(RoleID,[H|T])->
    case get_self_group_num2(RoleID,H,get({?pd_group_fight_record,H})++get({?pd_fight_step,H})) of
        0 ->
            get_self_group_num(RoleID,T);
        Num ->
            Num
    end.
get_self_group_num2(_RoleID,_Zone,[])->
    0;
get_self_group_num2(RoleID,Zone,[H|T])->
    case lists:member(RoleID, H#fight_step.fighter) of
        false ->
            get_self_group_num2(RoleID,Zone,T);
        true ->
            Zone
    end.

replace_on_arena1(A,B,OldList) ->
    OldList2 = replace_on_arena2(A,OldList,0),
    OldList3 = replace_on_arena2(B,OldList2,A),
    replace_on_arena2(0,OldList3,B).
replace_on_arena2(X,OldList,Y) ->
    replace_on_arena3(X,OldList,Y,[]).
replace_on_arena3(_X,[],_Y,NewList) ->
    lists:reverse(NewList);
replace_on_arena3(X,[H|T],Y,NewList) ->
    if
        H =:= X ->
            replace_on_arena3(X,T,Y,[Y|NewList]);
        true ->
            replace_on_arena3(X,T,Y,[H|NewList])
    end.

get_index(E,L)->
    get_index(E,L,1).
get_index(_E,[],_Index)->
    0;
get_index(E,[H|T],Index)->
    if
        E =:= H ->
            Index;
        true ->
            get_index(E,T,Index+1)
    end.
    
init_top_list2(Z)->
    PubList = [role_lib:get_rolePublic(R)||{R,Z0}<-get(?pd_sign_list),Z0 =:= Z],
    PubListSorted = 
        lists:sort(fun(A,B)-> A#rolePublic.fightPower > B#rolePublic.fightPower end, PubList),
    PubListSortedLen = erlang:length(PubListSorted),
    if
        PubListSortedLen > 4 -> %% 因为至少有四个机器人，所以不怕
            PubListSortedTop4 = lists:sublist(PubListSorted, 4);
        true ->
            PubListSortedTop4 = PubListSorted
    end,
    lists:map(fun(P)->
                #skin_info{equip=SkinID} = role_data:get_otherRoleSkinInfo(P#rolePublic.roleID),
                #arena_fighter{role_id = P#rolePublic.roleID 
                              ,role_name = P#rolePublic.roleName
                              ,is_male = P#rolePublic.isMale 
                              ,title = P#rolePublic.title 
                              ,head = P#rolePublic.head
                              ,power = P#rolePublic.fightPower
                              ,pos = Z
                              ,level = P#rolePublic.level
                              ,skinID = SkinID
                              ,trSpecialID = P#rolePublic.trSpecial
                              ,total_gamble = 0}      
        end, PubListSortedTop4).
  
get_top_list(_)->
    lists:merge([init_top_list2(Z)||Z<-?zone_list]).

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
    case data_race2:get(data_sex_changelist) of
        ?undefined->
            ?ERR("can not find data_sex_changelist in data_race2.config"),
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
    case data_race2:get(data_sex_changelist) of
        ?undefined->
            ?ERR("can not find data_sex_changelist in data_race2.config"),
            Item;
        List->
            case lists:keyfind(Item#new_item.itemTypeID,1,List) of
                false->
                    Item;
                {_MaleID,FemaleID}->
                    Item#new_item{itemTypeID=FemaleID}
            end
    end.

%% ====================================================================
%% Debug functions
%% ====================================================================

%% 重置到周一
debug_clear_data()->
    erlang:send(?MODULE, debug_clear_data).

%% 取出所有进程字典和state
debug_get_info()->
    erlang:send(?MODULE, debug_get_info).

%% 看下当前战斗情况
debug_current_fight()->
    gen_server:call(?MODULE, debug_current_fight).

%% 进入到次日，注意今天的淘汰赛必须完成
debug_next_step()->
    erlang:send(?MODULE, debug_next_step).

%% 进行下一场淘汰赛
debug_next_fight()->
    erlang:send(?MODULE, debug_next_fight).

%% 查看某个特定的进程字典
debug_get_pd(PdName)->
    gen_server:call(?MODULE, {debug_get_pd,PdName}).

%% 修改某个特定的进程字典
debug_set_pd(PdName,Value)->
    erlang:send(?MODULE, {debug_set_pd,PdName,Value}).

%% 测试一下邮件，只能在第七天测试
debug_test_mail()->
    erlang:send(?MODULE, test_mail).

%% 测试一下崩溃
debug_test_crash()->
    erlang:send(?MODULE, test_crash).

%% 测试一下崩溃
debug_test_check_group_score(Zone)->
    gen_server:call(?MODULE, {check_group_score,Zone}).
