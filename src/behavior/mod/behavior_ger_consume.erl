%% @author caohongyang
%% @doc @todo 武将消耗表


-module(behavior_ger_consume).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/7
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_ger_consume(State),
	{ok,[]}.

log(RoleID, List, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, List, Date, Time, Type, ArgID, Desc}}).