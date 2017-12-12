-module(behavior_plunder_fight).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([write/1
		,log/6]).

%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_plunder_fight_add(State),
	{ok,[]}.

log(Time,RoleID,RoleName,Level,Power,Vip) ->
	erlang:send(?MODULE, {add, {Time,RoleID,RoleName,Level,Power,Vip}}).
