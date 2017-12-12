%% carlos目录下所有类型的match匹配完毕后都需要进入这儿等待,因为每个match的数据结构可能并不一样
%% 所以这儿match需要遵守下面的约定: 
%% 1.需要将自己的匹配数据转化为room中使用的数据,需要在carlos_aux中实现一个parse_rq2members函数
%% 2.需要给room提供一个成功时的回调,接受RequestList做为参数
%%
-module(match_room).

-behaviour(gen_fsm).

%% API
%%-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-compile(export_all).

-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").
-define(tick_interval, 5000).
-define(sub_record(I,T,V), setelement(I, T, element(I, T) - V)).
-define(add_record(I,T,V), setelement(I, T, element(I, T) + V)).

-record(state, {
                    id=0,   %% 房间id
                    ready_num=0, %% 已准备人数
                    total_num=0, %% 总人数
                    team_list=[], %% 队伍列表  [{TeamID, [RoleIDs]}...] 
                    bc_list=[], %% 广播列表
                    members=[], %% 成员列表
                    from = undefined, %% 由那个match创建的
                    request_list=[],    %% 发过来的匹配列表,解散房间时重新放回去进行匹配
                    succ_callback=undefined,  %% 都准备好后的成功回调
                    router = undefined      %% 记录队伍ID->房间ID、房间ID->Pid的表(因为同时存在的房间不会太多,所以两个用一个ets表来记录)
                }).

%%%===================================================================
%%% API
%%%===================================================================
start(ID, RequestNum, RequestList, From, SuccCall, Router) ->
    gen_fsm:start(?MODULE, [ID, RequestNum, RequestList, From, SuccCall, Router], []).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
%%start_link(Args) ->
%%    gen_fsm:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([ID, RequestNum, RequestList, From, SuccCallBack, Router]) ->
    erlang:process_flag(trap_exit, true),
    match_room_manager:set_room_router(Router, ID, self()),
    gen_fsm:send_event(self(), {init_self, ID, RequestNum, RequestList, From, SuccCallBack, Router}),
    {ok, init_self, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
init_self({init_self, ID, RequestNum, RequestList, From, SuccCallBack, Router}, State) ->
    {TeamList, Members} = carlos_aux:parse_rq2members(RequestList),
    %%?ERR("TeamList:~p~n Members:~p~n", [TeamList, Members]),
    BcList = parse_to_bc_list(Members),
    Num = erlang:length(Members),
    NewState = State#state{
                            id=ID, 
                            total_num=Num, 
                            team_list=TeamList, 
                            bc_list=BcList, 
                            members=Members, 
                            from = From,
                            request_list=RequestList,
                            succ_callback=SuccCallBack,
                            router=Router},
    behavior_carlos:log(get_role_list(BcList), ?get_room_type(ID) + 1, ?carlos_op_enter_room, ID),
    gen_fsm:send_event(self(), enter_room),
    {next_state, wait, NewState}.

wait(enter_room, #state{members=Members}=State) ->
    PMembers = parse2p_members(Members),
    WaitTime = data_carlos:get(match_room_wait_time),
    CloseTime = util:now() + erlang:round(WaitTime / 1000),
    bc(State, {in_room, PMembers, CloseTime}),
    erlang:send_after(WaitTime, self(), time_out),
    {next_state, wait, State};

wait({ready, RoleID}, #state{ready_num=Num, total_num=TotalNum, members=Members}=State) -> 
    %%?ERR("ready:~p~nMembers:~p~n", [RoleID, Members]),
    case lists:keytake(RoleID, #room_member.roleID, Members) of
        false ->
            %% 异常,不处理
            ?ERR("event:wait, state:ready, 没有找到对应的玩家", []),
            {next_state, wait, State};
        {value, #room_member{status=Status} = RoomMember, Rest} ->
            case Status =:= ?wait_status_ready of
                true ->
                    {next_state, wait, State};
                _ ->
                    NewNum = Num + 1,
                    %% 广播状态改变
                    bc(State, {ready, RoleID}),
                    NewMembers = [RoomMember#room_member{status=?wait_status_ready, tick=0}|Rest],
                    NewState = State#state{ready_num=NewNum, members=NewMembers},
                    StateName = 
                        case NewNum =:= TotalNum of
                            true ->
                                %% 记得发送事件,触发状态
                                gen_fsm:send_event(self(), enter_success), 
                                success;
                            _ ->
                                wait 
                        end,
                    {next_state, StateName, NewState} 
            end
    end;

wait({cancel, RoleID}, #state{ready_num=Num, members=Members}=State) ->
    case lists:keytake(RoleID, #room_member.roleID, Members) of
        false ->
            %% 异常,不处理
            ?ERR("event:wait, state:cancel, 没有找到对应的玩家", []),
            {next_state, wait, State};
        {value, #room_member{status=Status}=RoomMember, Rest} ->
            case Status =:= ?wait_status_cancel of
                true ->
                    {next_state, wait, State};
                _ ->    
                    NewNum = Num - 1,
                    NewMembers = [RoomMember#room_member{status=?wait_status_cancel, tick=0}|Rest],
                    NewState = State#state{ready_num=NewNum, members=NewMembers},
                    %% 广播状态改变
                    bc(State, {cancel, RoleID}),
                    {next_state, wait, NewState}
            end
    end;

wait({exit, TeamID}, #state{id=RoomID, team_list=TeamList, request_list=RequestList, from=From} = State) ->
    %%?ERR("exit:~p~n TeamList:~p~n", [TeamID, TeamList]),
    case lists:keytake(TeamID, 1, TeamList) of
        false ->
            %% 异常,不处理
            ?ERR("event:wait, state:cancel, 没有找到对应的玩家", []),
            {next_state, wait, State};
        {value, {_, RoleIDList}, _} ->
            Type = ?get_room_type(RoomID),
            RequestList2 = 
                case Type of
                    ?room_type_twins ->
                        lists:map(fun(E) -> lists:keydelete(TeamID, #tw_request.id, E) end, RequestList);
                    _ ->
                        lists:map(fun(E) -> lists:keydelete(TeamID, #request.id, E) end, RequestList) 
                end,
                NewState = State#state{request_list=RequestList2},
                behavior_carlos:log(RoleIDList, Type + 1, ?carlos_op_exit_room, RoomID),
                bc(NewState, {exit, RoleIDList}),
                lists:foreach(fun(E) -> erlang:send(From, {re_request, E}) end, lists:flatten(RequestList2)),
                {stop, {shutdown, exit}, NewState} 
    end.

%% 全部准备完了,进入战场
success(_, #state{id=RoomID, from=From, bc_list=BcList, request_list=RequestList, succ_callback=SuccCallBack} = State) ->
    %%?ERR("success CampTuple:~p~n", [RequestList]),
    SuccCallBack(From, RequestList),
    behavior_carlos:log(get_role_list(BcList), ?get_room_type(RoomID) + 1, ?carlos_op_room_success, RoomID),
    {stop, normal, State}.

%% 结束
fail(_, State) ->
    {stop, {shutdown, fail}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
%state_name(_Event, _From, State) ->
%    Reply = ok,
%    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    case catch do_handle_info(Info, StateName, State) of
        {next_state, StateName2, NewState} ->
            {next_state, StateName2, NewState};
        {stop, Reason, NewState} ->
            {stop, Reason, NewState}; 
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n StateName:~w State:~w~n", [Exeption, Info, StateName, State]),
            {next_state, StateName, State} 
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{id=RoomID, team_list=TeamList, router=Router}) ->
    match_room_manager:clear_room_router(Router, RoomID),
    lists:foreach(fun({TeamID, _}) ->
                    match_room_manager:delete_team_roomid(Router, TeamID)
                end, TeamList),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%检测超时,因为获取系统时间比较耗时,而且这里超时就自动退出，对时间精度要求
%%并不高，而且即使记录绝对秒数，由于进程调度等，退出时间并不能精确多少，所以 
%%这儿不记录绝对秒数，采用tick数来判断是否超时
do_handle_info(check_tick, StateName, #state{id=RoomID, members=Members} = State) ->
    WaitTime = data_carlos:get(match_room_wait_time),
    Limit = erlang:round(WaitTime / ?tick_interval),
    {Members2, ToLeave} = 
        lists:foldl(fun(#room_member{status=Status, tick=Tick, teamID=TeamID, serverID=ServerID} = RM, {Acc, Leave}) ->
                        case Status of
                            ?wait_status_ready ->
                                {[RM|Acc], Leave};
                            _ ->
                                NewTick = Tick + 1,
                                NewRm = RM#room_member{tick=NewTick},
                                NewAcc = [NewRm|Acc],
                                case NewTick >= Limit of
                                    true ->
                                        case lists:member(TeamID, Leave) of
                                            true ->
                                                {NewAcc, Leave};
                                            _ ->
                                                {NewAcc, [{ServerID,TeamID}|Leave]} 
                                        end;
                                    _ ->
                                        {NewAcc, Leave}
                                end 
                        end 
                    end, {[], []}, Members),
                                        
    %% 有人离开时,最终会触发{exit, TeamID}事件,会触发一次check,所以不需要主动check
    %% 当没有人离开时,主动触发一次check,去检测当前状态
    %% 然后每次有人加入时也触发check
    case ToLeave of
        [] ->
            %% 只有人不够时(want状态)才需要触发check事件
            case StateName of
                want ->
                    gen_fsm:send_event(self(), want_check);
                _ ->
                    ignore
            end;
        _ ->
            Type = ?get_room_type(RoomID),
            SendFun =  
                case Type of
                    ?room_type_carlos ->
                        fun notcie_carlos_exit/1;
                    ?room_type_relic ->
                        fun notcie_relic_exit/1;
                    ?room_type_galactica ->
                        fun notcie_galactica_exit/1;
                    ?room_type_twins ->
                        fun notcie_twins_exit/1 
                end,
            lists:foreach(fun(E) -> SendFun(E) end, ToLeave) 
    end,
    {next_state, StateName, State#state{members=Members2}};

do_handle_info(time_out, success, State) ->
    success(time_out, State);

%% 超时后提出没有准备的,然后重新进行匹配
%% 这里不能用被注释掉的exit的流程,即notice_xxx_exit通知match退出,然后match再通知这边,
%% 因为这个事件后,房间就关闭了,所以不能用那个流程
do_handle_info(time_out, _StateName, #state{from=From, id=RoomID, team_list=TeamList, request_list=RequestList, members=Members} = State) ->
    %%?ERR("from:~p, re request lists:~p.~n", [From, lists:flatten(RequestList)]),
    Type = ?get_room_type(RoomID),
    %% 先找到所有需要踢掉的组
    ExitTeams = 
        lists:foldl(fun(#room_member{teamID=TeamID, serverID=ServerID, status=Status}, TeamIDAcc) ->
                            case Status =/= ?wait_status_ready of
                                true ->
                                    case lists:keyfind(TeamID, 2, TeamIDAcc) of   
                                        false ->
                                            [{ServerID, TeamID}|TeamIDAcc];
                                        _ ->
                                                TeamIDAcc 
                                    end;
                                _ ->
                                    TeamIDAcc
                            end
                    end, [], Members),

    %% 合并所有没有准备的小组里面的成员
    ExitRoles = 
        lists:foldl(fun({_, TeamID}, RoleIDAcc) ->
                        case lists:keyfind(TeamID, 1, TeamList) of
                            false ->
                                RoleIDAcc;
                            {_, List} ->
                                List ++ RoleIDAcc
                        end
                    end, [], ExitTeams),
    
    KeyPos =
        case Type of
            ?room_type_twins ->
                #tw_request.id;
            _ ->
                #request.id
        end,

    %% 需要重新进行匹配的队伍
    RequestList2 = 
        lists:foldl(fun({_, TeamID}, Acc) ->
                        case lists:keytake(TeamID, KeyPos, Acc) of
                            false ->
                                Acc;
                            {value, _, Rest} ->
                                Rest
                        end
                    end, lists:flatten(RequestList), ExitTeams),
                        
    %% 通知房间关闭,并下发被剔除的玩家
    bc(State, {close_and_exit, 0, ExitRoles}),

    %%TODO 下面两个消息可以合并的
    erlang:send(From, {kick_room_teams, ExitTeams}),
    lists:foreach(fun(E) -> erlang:send(From, {re_request, E}) end, RequestList2),
    {stop, {shutdown, time_out}, State};

do_handle_info(Info, StateName, State) ->
    ?ERR("未知的消息,Info:~w~n StateName:~w~n State:~w~n", [Info, StateName, State]),
    {next_state, StateName, State}.

%% 维护一个按照服务器分片的bc_list,可以减少广播时的发送次数
parse_to_bc_list(Members) ->
    parse_to_bc_list(Members, []).

parse_to_bc_list(Members, BcList) ->
    lists:foldl(fun(#room_member{serverID=ServerID, roleID=RoleID}, Acc) ->
                    case lists:keytake(ServerID, 1, Acc) of
                        false ->
                            [{ServerID, [RoleID]}|Acc];
                        {value, {_, NowList} , Rest} ->
                            [{ServerID, [RoleID|NowList]}|Rest]
                    end 
                end, BcList, Members).

%% world那层对应卡洛斯 巨龙 双子等功能,所以需要一个独立模块来处理数据
bc(#state{bc_list=BcList, id=RoomID}, Msg) ->
    bc(BcList, RoomID, Msg).

bc(BcList, RoomID, Msg) ->
    lists:foreach(fun({ServerID, RoleIDList}) ->
                        send_msg:direct(ServerID, match_room_server, #room_bc_msg{roomID=RoomID, roleIDs=RoleIDList, data=Msg})
                    end, BcList).

binary_foldl(_, Acc, [], _) ->
    Acc;
binary_foldl(Fun, Acc, [FH|FT], [SH|ST]) ->
    binary_foldl(Fun, Fun(FH, SH, Acc), FT, ST).

notcie_carlos_exit({ServerID, ID}) ->
    carlos_match:send_to_me({unrequest, ServerID, ID}).

notcie_relic_exit({ServerID, ID}) ->
    lists:foreach(fun(LevelRank)->
                    send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {unrequest, ServerID, ID})                                      
                    end, [1,2,3,4,5]).

notcie_galactica_exit({ServerID, ID}) ->
    galactica_match:send_to_me({unrequest, ServerID, ID}).

notcie_twins_exit({ServerID, ID}) ->
    twins_match:send_to_me({unrequest, ServerID, ID}). 

parse2p_members(Members) ->
    lists:foldl(fun(#room_member{roleID=RoleID, head=Head, camp=Camp, status=Status, isMale=IsMale, title=Title, level=Level}, Acc) ->
                    [#p_member_info{ roleID=RoleID, head=Head, camp=Camp, status=Status, isMale=IsMale, title=Title, level=Level}|Acc]
                end, [], Members).

del_member_by_roleids(RoleIDList, Members) ->
    del_member_by_roleids(RoleIDList, Members, {[], []}).

del_member_by_roleids([], Members, ToDel) ->
    {Members, ToDel};
del_member_by_roleids([H|T], Members, ToDel) ->
    case lists:keytake(H, #room_member.roleID, Members) of
        false ->
            del_member_by_roleids(T, Members, ToDel); 
        {value, RM, Rest} ->
            del_member_by_roleids(T, Rest, [RM|ToDel])
    end.
           
get_role_list(BcList) ->
    lists:foldl(fun({_, RoleIDList}, Acc) ->
                    RoleIDList ++ Acc
                end, [], BcList).
                    
