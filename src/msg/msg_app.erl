%% @author lixinglong
%% @doc 


-module(msg_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("ets_name.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
	Return = 'msg_sup':start_link(),
	ets:new(amqp_info_table,[{keypos,1},named_table,set,public]),
	ets:new(?ETS_NODE_INFO_TABLE, [{keypos,1},set, public,named_table]),
	ets:new(?ETS_NODE_INFO_TABLE2, [{keypos,1},set, public,named_table]),
    start_(send_msg),
	start_(node_info_server),
	Return.

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_(ServerName) ->
	catch(ServerName:start()).

