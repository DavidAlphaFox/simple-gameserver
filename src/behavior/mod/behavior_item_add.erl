%% @author caohongyang
%% @doc @todo 道具获得表


-module(behavior_item_add).
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
	db_sql:log_item_add(State),
	{ok,[]}.

log(RoleID, List, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, List, Date, Time, Type, ArgID, Desc}}).