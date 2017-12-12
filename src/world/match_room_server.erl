-module(match_room_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-compile(export_all).

-record(state, {}).
-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start( ) ->
    {ok,_} = 
    supervisor:start_child(world_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{}}.

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
    case catch do_distribute_info(Info, State) of
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
do_distribute_info({inet_reply,_,_}, State) ->
    {noreply, State};

do_distribute_info(#room_bc_msg{roomID=RoomID, data=Msg, roleIDs=RoleIDList}, State) ->
    do_room_bc_info(Msg, RoleIDList, RoomID, State);

do_distribute_info(Msg, State) ->
    do_normal_msg(Msg, State).
    
do_room_bc_info({in_room, Members, CloseTime}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_init{roomID=RoomID, members= Members, close_timestamp=CloseTime}),
    {noreply, State};
    
do_room_bc_info({ready, RoleID}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_ready{roomID=RoomID, roleID=RoleID}),
    {noreply, State};

do_room_bc_info({cancel, RoleID}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_cancel{roomID=RoomID, roleID=RoleID}),
    {noreply, State};

do_room_bc_info({kick, RoleID}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_kick{roomID=RoomID, roleID=RoleID}),
    {noreply, State};

do_room_bc_info({exit, ExitRoles}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_exit{roomID=RoomID, idlist=ExitRoles}),
    {noreply, State};

do_room_bc_info({new_in_room, NewMembers}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_new{roomID=RoomID, members=NewMembers}),
    {noreply, State};

do_room_bc_info({close, Reason}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_close{roomID=RoomID, reason=Reason}),
    {noreply, State};

%% TODO 其实这儿可以合并成一条协议
do_room_bc_info({close_and_exit, Reason, ExitRoles}, RoleIDList, RoomID, State) ->
    bc(RoleIDList, #sc_matchRoom_exit{roomID=RoomID, idlist=ExitRoles}),
    bc(RoleIDList, #sc_matchRoom_close{roomID=RoomID, reason=Reason}),
    {noreply, State};

do_room_bc_info(Info, _, _, State) ->
    ?ERR("未知的广播消息:~p.~n", [Info]),
    {noreply, State}.

do_normal_msg({client_msg, ready, RoomID, RoleID}, State) ->
    Type = ?get_room_type(RoomID),
    match_room_manager:send_to_me(Type, {client_msg, RoomID, {ready, RoleID}}),
    {noreply, State};

do_normal_msg({client_msg, cancel, RoomID, RoleID}, State) ->
    Type = ?get_room_type(RoomID),
    match_room_manager:send_to_me(Type, {client_msg, RoomID, {cancel, RoleID}}),
    {noreply, State};

do_normal_msg({client_msg, exit, RoomID, SignID}, State) ->
    Type = ?get_room_type(RoomID),
    case Type of
        ?room_type_carlos ->
            erlang:send(carlos_server, {unrequest, SignID});
        ?room_type_relic ->
            erlang:send(relic_server, {unrequest, SignID});
        ?room_type_galactica ->
            erlang:send(galactica_server, {unrequest, SignID});
        ?room_type_twins ->
            erlang:send(twins_server, {unrequest, SignID})
    end,
    {noreply, State};

do_normal_msg(Msg, State) ->
    ?ERR("未知的普通消息:~p.~n", [Msg]),
    {noreply, State}.

parse_room_member(#room_member{roleID=RoleID, head=Head, camp=Camp, status=Status, level=Level}) ->
    #p_member_info{roleID=RoleID, head=Head, camp=Camp, status=Status, level=Level}.
    
bc(RoleIDList, Msg) ->
    lists:foreach(fun(R) -> ?unicast(R, Msg) end, RoleIDList).
