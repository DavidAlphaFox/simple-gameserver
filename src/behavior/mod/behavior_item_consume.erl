%% @author caohongyang
%% @doc @todo 道具消耗表


-module(behavior_item_consume).
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
	db_sql:log_item_consume(State),
	{ok,[]}.

log(_, [], _, _, _, _, _) ->
	ignore;
log(RoleID, List, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, List, Date, Time, Type, ArgID, Desc}}).