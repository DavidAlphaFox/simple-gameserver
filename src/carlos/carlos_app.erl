%% @author lxl


-module(carlos_app).
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
    Return = 'plane_sup':start_link(),
    start2(_Type,_StartArgs),
    Return.
start2(_Type,_StartArgs)->
	% 'galactica_sup':start_link(),
	% 'twins_sup':start_link(),
	% anubis_sup:start_link(),
	% conquerisland_sup:start_link(),
    lists:foreach(fun(SubSup)->
    	supervisor:start_child(plane_sup,
                            {SubSup,
                             {SubSup, start_link,[]},
                             permanent, 600000, worker, [SubSup]})
    end,[carlos_sup,galactica_sup,twins_sup,anubis_sup,conquerisland_sup]),
    random:seed(now()),
	start_(war_manager_server),
	start_(carlos_router),
	start_(carlos_replay_server),
	start_(carlos_server),
 	start_(carlos_match),
	start_(carlos_rank),
	
	start_(galactica_war_manager_server),
	start_(galactica_server),
	start_(galactica_match),
	start_(galactica_router),
	start_(galactica_replay_server),
	
	start_(twins_war_manager_server),
	start_(twins_server),
	start_(twins_match),
	start_(twins_router),
	start_(anubis_server),
%%     start_(doublematch_rank),
    start_(anubis_rank_server),

	start_(conquerisland_server),
    start_(conquerisland_war_manager_server),
    start_(conquerisland_match),
    start_(conquerisland_router),
    start_(conquerisland_replay_server),

    %% 房间管理器
    match_room_manager:start(carlos, carlos_sup),
    match_room_manager:start(relic, carlos_sup),
    match_room_manager:start(galactica, galactica_sup),
    match_room_manager:start(twins, twins_sup),
	match_room_manager:start(conquerisland,conquerisland_sup),
    
    redpacketworld_server:start(),
    
    case data_setting:get(server_type) of
        carlos_match ->
            % 巨龙遗迹分配服
            relic_match:start(1),
            relic_match:start(2),
            relic_match:start(3),
            relic_match:start(4),
            relic_match:start(5),
            relic_match:start(6),
            % 巨龙遗迹战斗服
            start_(relic_war_manager);
        _ ->
            ignore
    end.
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

