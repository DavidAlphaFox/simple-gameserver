-module(relic_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RelicWarManager = {relic_war_manager, {relic_war_manager, start_link, []}, permanent, 600000, worker, [relic_war_manager]},
    RelicWarSup = {relic_war_sup, {relic_war_sup, start_link, []}, permanent, 600000, supervisor, [relic_war_sup]},
    {ok,{{one_for_one,3,10}, [RelicWarSup,RelicWarManager]}}.
