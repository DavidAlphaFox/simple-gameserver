-module(family_cross_fight_battle_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link()-> supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	AChild = {'family_cross_fight_battle', {'family_cross_fight_battle', start_link,[]},
			  temporary, 600000, worker, ['family_cross_fight_battle']},
	{ok, {{simple_one_for_one, 10,10},[AChild]}}.



