%% @author crimoon-17
%% @doc @todo Add description to war_manager_server.


-module(twins_war_manager_server).
-behaviour(gen_server).
-include("common.hrl").
-include("def_carlos.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

new_war(WarID,Attackers,Type,Level) ->
	erlang:send(?MODULE, {new_war,WarID,Attackers,Type,Level}).
send_to_me({new_war, WarID, Team1, Type,Level}) ->
	{N,IDList} = data_twins:get(war_servers),
	ID = lists:nth(random:uniform(N), IDList),
	send_msg:direct(ID,twins_war_manager_server,{new_war_base, WarID, Team1, Type,Level}).

war_stoped(WarID) ->
	erlang:send(?MODULE, {war_stopped, WarID}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).


start() ->
	case data_setting:get(server_type) of
		carlos ->
			{ok, _} =
				supervisor:start_child(twins_sup,
									   {twins_war_sup,
										{twins_war_sup, start_link, []},
										permanent, infinity, supervisor, [twins_war_sup]}),
			{ok, _} =
				supervisor:start_child(twins_sup, 
									   {?MODULE,
										{?MODULE, start_link, []},
										permanent, 600000, worker, [?MODULE]});
		_ ->
			ok
	end.

%%--------------------------------------------------------------------
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
	process_flag(trap_exit,true),
    ets:new(?ETS_TWINS_INFO, [named_table, set, public,{keypos,2}]),
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
handle_info({new_war, WarID,Attackers,Type,Level},State) ->
	twins_war_server:start([WarID,Attackers,Type,Level]),
	{noreply,State};
handle_info({war_stopped, WarID}, State) ->
	ets:delete(?ETS_TWINS_INFO,WarID),
	{noreply,State};
handle_info({new_war_base, WarID, Team1, Type,Level},State) ->
	F = fun()-> 
				P = self(),
				[send_msg:direct(ServerID,twins_server, {get_player, RoleID, WarID,data_setting:get(server_id),P,attacker})
				||#member_base_info{serverID=ServerID,roleID=RoleID}<-Team1],
				Players = wait_msg([],length(Team1),Team1),
				new_war(WarID,Players,Type,Level)
				end,
	spawn(F),
	{noreply,State};
handle_info(_Info, State) ->
	?ERR("unknown msg:~w",[_Info]),
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

wait_msg(A,0,_) ->
	A;
wait_msg(A,LA,Team1) ->
	receive 
		{attacker,Player} ->
			wait_msg([Player|A],LA-1,Team1)
	after 60000 ->
			?ERR("wait msg failed...,team1=~w,get_playerA:~w",[Team1,A]),
			A
	end.
		

