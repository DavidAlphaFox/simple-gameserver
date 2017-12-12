%% @author crimoon-17
%% @doc @todo Add description to war_manager_server.


-module(conquerisland_replay_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

get_replay(ReplayUID, WarServerID,RoleID) ->
	send_msg:direct(WarServerID, conquerisland_replay_server, {get_replay, ReplayUID,WarServerID, RoleID,data_setting:get(server_id)}).

record_replay(ReplayUID,FightRecord) ->
	erlang:send(?MODULE,{record_replay, ReplayUID, FightRecord}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).


start() ->
	{ok, _} =
		supervisor:start_child(conquerisland_sup, 
							   {?MODULE,
								{?MODULE, start_link, []},
								permanent, 600000, worker, [?MODULE]}).

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
	case data_setting:get(server_type) of
		normal->
			start_server();
		carlos->
			start_server();
		_->
			ignore
	end.

start_server()->
	process_flag(trap_exit,true),
    delete_old_replay(),
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
handle_info({get_replay, ReplayUID,_WarServerID,RoleID,ServerID},State) ->
	Rep = db_sql:get_fightReplay(ReplayUID),
	send_msg:direct(ServerID, conquerisland_replay_server, {get_replay, RoleID,Rep}),
	{noreply,State};
handle_info({get_replay, RoleID,Rep},State) ->
	?unicast(RoleID, #sc_conquerisland_replay{result=1,fightInfo=[Rep]}),
	{noreply,State};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info({record_replay, ReplayUID, FightRecord},State) ->
	db_sql:set_fightReplay(ReplayUID, FightRecord,?REPLAY_TYPE_CARLOS),
	{noreply,State};
handle_info(delete_old_replay, State) ->
    delete_old_replay(),
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
delete_old_replay() ->
    {Time, Day} = data_carlos:get(reply_del_info),
	erlang:send_after(db_manage_server:get_send_after_seconds(Time) * 1000, erlang:self(), delete_yesterday_replay),
    db_sql:del_spec_type_and_time_replay(?REPLAY_TYPE_CARLOS, Day).

