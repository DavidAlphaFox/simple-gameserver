%% @author crimoon-17
%% @doc @todo Add description to twins_router.


-module(twins_router).
-behaviour(gen_server).
-include("common.hrl").
-include("def_carlos.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-record(state, {}).
-define(route, route).


send_twins({WarID,CarlosServerID},Msg,RoleID)->
	send_msg:direct(CarlosServerID,twins_router,{route_twins, WarID,Msg,data_setting:get(server_id),RoleID}).

send_role(ServerID,RoleID,Msg) ->
	send_msg:direct(ServerID, twins_router, {route_role, Msg,RoleID}).
send_client(ServerID,RoleID,Msg) ->
	send_msg:direct(ServerID, twins_router, {unicast_client,  Msg,RoleID}).
	%send_msg:direct(ServerID, twins_router, {route_role, {?route, role_twins, {send_client, Msg}},RoleID}).
unicast_client(ServerID,RoleID,Msg) ->
	send_msg:direct(ServerID, twins_router, {unicast_client,  Msg,RoleID}).
%{send_client, {route,failed}}

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start() ->
	{ok, _} =
		supervisor:start_child(twins_sup, 
							   {?MODULE,
								{?MODULE, start_link, []},
								permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    {ok, #state{}}.


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
handle_info({route_twins, WarID,Msg,ServerID,RoleID},State) ->
	case ets:lookup(ets_twins_info,WarID) of
		[#ets_twins_info{pid=Pid}] ->
			catch( erlang:send(Pid, {Msg, {WarID,RoleID,ServerID}}));
		_ ->
			send_msg:direct(ServerID, twins_router, {route_role, {?route, role_twins, {send_client, {route,2}}}, RoleID})
	end,
	{noreply, State};
handle_info({route_role, Msg,RoleID},State) ->
	catch(role_lib:send_server(RoleID,Msg)),
	{noreply,State};
handle_info({unicast_client,  Msg,RoleID},State) ->
	?unicast(RoleID,Msg),
	{noreply,State};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
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


