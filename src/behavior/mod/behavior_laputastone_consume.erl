%% @author caohongyang
%% @doc @todo 声望消耗表


-module(behavior_laputastone_consume).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/8
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_laputastone_consume(State),
	{ok,[]}.

log(RoleID, Repu, CurRepu, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, Repu, CurRepu, Date, Time, Type, ArgID, Desc}}).