%% @author caohongyang
%% @doc @todo 武将升品记录表


-module(behavior_ger_uprank).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/12
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_ger_uprank(State),
	{ok,[]}.

log(RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, NewRank, FoodGerUID, Date, Time) ->
	erlang:send(?MODULE, {add, {RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, NewRank, FoodGerUID, Date, Time}}).