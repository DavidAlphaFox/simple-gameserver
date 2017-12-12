%% @author caohongyang
%% @doc @todo 装备升品记录表


-module(behavior_item_uprank).
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
	db_sql:log_item_uprank(State),
	{ok,[]}.

log(RoleID,ItemUID,ItemTypeID,CurLevel,NewLevel,CurRank,NewRank,FoodItemUID,Date,Time) ->
	erlang:send(?MODULE, {add, {RoleID,ItemUID,ItemTypeID,CurLevel,NewLevel,CurRank,NewRank,FoodItemUID,Date,Time}}).