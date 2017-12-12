-module(conquerisland_war_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    AChild = {conquerisland_war_server,{conquerisland_war_server,start_link,[]},
	      temporary,600000,worker,[]},
    {ok,{{simple_one_for_one,10,10}, [AChild]}}.