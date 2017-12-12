-module(match_room_manager).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-compile(export_all).

-record(state, {type=undefine, sup=undefine, router=undefine}).

-include("common.hrl").
-include("def_carlos.hrl").

-define(get_last_room_id(), erlang:get(last_room_id)).
-define(set_last_room_id(ID), erlang:put(last_room_id, ID)).
-define(RoomId2Pid, roomId2Pid).
-define(TeamId2RoomId, teamId2RoomId).

%%%===================================================================
%%% API
%%%===================================================================
start(Type, Sup) ->
    ManagerName = erlang:list_to_atom("match_room_manager_" ++ erlang:atom_to_list(Type)),
    {ok,_} = 
    supervisor:start_child(Sup,
                            {ManagerName,
                             {?MODULE, start_link,[ManagerName, Type, Sup]},
                             permanent, 600000, worker, [?MODULE]}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ManagerName, Type, Sup) ->
    gen_server:start_link({local, ManagerName}, ?MODULE, [Type, Sup], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Type, Sup]) ->
    ?set_last_room_id(0),
	Router = ets:new(?MODULE ,[{keypos,1},set, public]),
    {ok, #state{sup=Sup, type=Type, router=Router}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_info({create_room, RequestNum, RequestList, From, SuccCall}, #state{type=Type, router=Router} = State) ->
    ID = gen_id(Type),
    match_room:start(ID, RequestNum, RequestList, From, SuccCall, Router),
    init_team_room_id(Router, RequestList, ID),
    {noreply, State};

do_handle_info({client_msg, RoomID, Msg}, #state{router=Router} = State) ->
    send_to_room(Router, RoomID, Msg),
    {noreply, State};

do_handle_info({unrequest, TeamID}, #state{router=Router} = State) ->
    case get_team_roomid(Router, TeamID) of
        undefine ->
            ignore;
        RoomID ->
            send_to_room(Router, RoomID, {exit, TeamID})
    end,
    {noreply, State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
    {noreply, State}.

gen_id(Type) ->
    NumType = get_num_type(Type),
    ID = ?get_last_room_id(),
    NewID = ID + 1,
    ?set_last_room_id(NewID),
    NumType bsl 28 bor NewID.

send_to_room(Router, RoomID, Msg) ->
    case ets:lookup(Router, {?RoomId2Pid, RoomID}) of
        [{_, Pid}] ->
            catch gen_fsm:send_event(Pid, Msg);
        _ ->
            ignore
    end.

clear_room_router(Router, RoomID) ->
    ets:delete(Router, {?RoomId2Pid, RoomID}).

set_room_router(Router, RoomID, Pid) ->
    ets:insert(Router, {{?RoomId2Pid, RoomID}, Pid}).

send_to_me(NumType, Msg) ->
    ManagerName = get_room_manager_name(NumType),
    send_msg:direct_by_name(carlos_match, ManagerName, Msg).

%% 使用闭包函数开销不大,但流程比消息回调简单些,也比使用函数+回调参数的方式
%% 简单些
create_room(NumType, RequestNum, RequestList, SuccCall) ->
   erlang:send(get_room_manager_name(NumType), {create_room, RequestNum, RequestList, self(), SuccCall}). 

send_unrequest(TeamID, NumType) ->
    erlang:send(get_room_manager_name(NumType), {unrequest, TeamID}).

get_team_roomid(Router, TeamID) ->
    case ets:lookup(Router, {?TeamId2RoomId, TeamID}) of
        [] ->
            undefine;
        [{_, RoomID}] ->
            RoomID
    end.

set_team_roomid(Router, TeamID, RoomID) ->
    ets:insert(Router, {{?TeamId2RoomId, TeamID}, RoomID}). 

delete_team_roomid(Router, TeamID) ->
    ets:delete(Router, {?TeamId2RoomId, TeamID}).

%% 记录队伍的room id
init_team_room_id(Router, RequestList, RoomID) ->
    lists:foreach(fun(TE) ->
                    lists:foreach(fun(Request) ->
                                    case Request of
                                        #request{id=TeamID} ->
                                            next;
                                        #tw_request{id=TeamID} ->
                                            next
                                    end,
                                    set_team_roomid(Router, TeamID, RoomID)
                                end, TE)
                end, RequestList).

get_room_manager_name(?room_type_carlos) ->
    match_room_manager_carlos;

get_room_manager_name(?room_type_relic) ->
    match_room_manager_relic;

get_room_manager_name(?room_type_galactica) ->
    match_room_manager_galactica;

get_room_manager_name(?room_type_twins) ->
    match_room_manager_twins.

get_num_type(carlos) ->
    ?room_type_carlos;

get_num_type(relic) ->
    ?room_type_relic;

get_num_type(galactica) ->
    ?room_type_galactica;

get_num_type(twins) ->
    ?room_type_twins.
