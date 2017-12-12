%% @author crimoon-17
%% @doc @todo Add description to war_manager_server.


-module(war_manager_server).
-behaviour(gen_server).
-include("common.hrl").
-include("def_carlos.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

new_war(WarID,Attackers,Defenders) ->
	erlang:send(?MODULE, {new_war,WarID,Attackers,Defenders}).

send_to_me({new_war, WarID, Team1, Team2}) ->
    ?WARNING("war_manager_server >>>> ~w",[{new_war, WarID, [M#member_base_info.level||M<-Team1], [M#member_base_info.level||M<-Team2]}]),
	{N,IDList} = data_carlos:get(war_servers),
	ID = lists:nth(random:uniform(N), IDList),
	send_msg:direct(ID,war_manager_server,{new_war_base, WarID, Team1, Team2}).

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
				supervisor:start_child(carlos_sup,
									   {war_sup,
										{war_sup, start_link, []},
										permanent, infinity, supervisor, [war_sup]}),
			{ok, _} =
				supervisor:start_child(carlos_sup, 
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
    ets:new(?ETS_CARLOS_INFO, [named_table, set, public,{keypos,2}]),
	init_carlos_pids(),
    {ok, #state{}}.

init_carlos_pids() ->
	List = db_sql:get_carlos_id_list(),
	lists:foreach(fun([WarID]) -> war_server:start(WarID) end,List).

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
handle_info({new_war, WarID,Attackers,Defenders},State) ->
	war_server:start([WarID,Attackers,Defenders]),
	{noreply,State};
handle_info({war_stopped, WarID}, State) ->
	ets:delete(?ETS_CARLOS_INFO,WarID),
	{noreply,State};
handle_info({new_war_base, WarID, Team1, Team2},State) ->
	F = fun()-> 
				P = self(),
				[send_msg:direct(ServerID,carlos_server, {get_player, RoleID, WarID,data_setting:get(server_id),P,attacker})||#member_base_info{serverID=ServerID,roleID=RoleID}<-Team1],
				[send_msg:direct(ServerID,carlos_server, {get_player, RoleID, WarID,data_setting:get(server_id),P,defender})||#member_base_info{serverID=ServerID,roleID=RoleID}<-Team2],
				{Attackers,Defenders} = wait_msg([],[],length(Team1),length(Team2),Team1,Team2),
				% ?INFO("Attackers:~w Defenders:~w ~n",[Attackers,Defenders]),
				new_war(WarID,Attackers,Defenders)
				end,
	spawn(F),
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

wait_msg(A,B,0,0,_,_) ->
	{A,B};
wait_msg(A,B,LA,LB,Team1,Team2) ->
	receive 
		{attacker,Player} ->
			wait_msg([Player|A],B,LA-1,LB,Team1,Team2);
		{defender,Player} ->
			wait_msg(A,[Player|B],LA,LB-1,Team1,Team2)
	after 60000 ->
			?ERR("wait msg failed...,team1=~w,team2=~w,get_playerA:~w,get_playerB=~w",[Team1,Team2,A,B]),
			{A,B}
	end.
		
test_t()->
		erlang:send(?MODULE,{new_war_base, 1
						, [{1,15,16010017},{1,15,16010003}]
						, [{1,15,16010009},{1,15,16010019}]
						}).
%% 	erlang:send(?MODULE,{new_war_base, 1
%% 						, [{1,15,16010017},{1,15,16010003},{1,15,16010005},{1,15,16010006},{1,15,16010007}
%% 						  ,{1,15,160100011},{1,15,16010013},{1,15,16010015},{1,15,16010016},{1,15,16010035}
%% 						  ,{1,15,16010002},{1,15,16010001}]
%% 						, [{1,15,16010009},{1,15,16010019},{1,15,16010020},{1,15,16010022},{1,15,16010024}
%% 						  ,{1,15,16010025},{1,15,16010027},{1,15,16010029},{1,15,16010031},{1,15,16010033}
%% 						  ,{1,15,16010037}]
%% 						}).

