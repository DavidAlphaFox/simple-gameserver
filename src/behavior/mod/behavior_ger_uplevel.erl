%% @author caohongyang
%% @doc @todo 武将升级记录表


-module(behavior_ger_uplevel).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/10
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_ger_uplevel(State),
	{ok,[]}.

log(RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, Date, Time) ->
	erlang:send(?MODULE, {add, {RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, Date, Time}}).