%% @author caohongyang
%% @doc @todo 装备升级记录表


-module(behavior_item_uplevel).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/9
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_item_uplevel(State),
	{ok,[]}.

log(RoleID,ItemUID,ItemTypeID,AddLevel,NewLevel,AddTimes,Coin,Date,Time) ->
	erlang:send(?MODULE, {add, {RoleID,ItemUID,ItemTypeID,AddLevel,NewLevel,AddTimes,Coin,Date,Time}}).