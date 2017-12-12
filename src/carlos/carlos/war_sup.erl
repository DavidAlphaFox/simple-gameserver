-module(war_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    AChild = {'war_server',{'war_server',start_link,[]},
	      temporary,600000,worker,['war_server']},
    {ok,{{simple_one_for_one,10,10}, [AChild]}}.