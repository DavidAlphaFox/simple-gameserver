%% @author caohongyang
%% @doc @todo 武将获得表


-module(behavior_ger_add).
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
	db_sql:log_ger_add(State),
	{ok,[]}.

log(RoleID, List, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, List, Date, Time, Type, ArgID, Desc}}).