%% @author lixinglong
%% @doc application of world node
%% Created 2013-3-7


-module(family_world_app).
-behaviour(application).
-include("common.hrl").
-export([start/2, stop/1]).

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
    Return = 'family_world_sup':start_link(),
    random:seed(now()),
	start_(family_manager_server),
	
	start_(family_cross_fight_master),
	start_(family_cross_fight_manager),
	start_(family_cross_fight_router),
	start_(family_cross_replay_server),
	start_(family_cross_fight_server),

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
	?CATCH(ServerName:start()).

