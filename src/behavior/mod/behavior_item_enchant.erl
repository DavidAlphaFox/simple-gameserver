%% @author caohongyang
%% @doc @todo 装备升级记录表


-module(behavior_item_enchant).
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
	db_sql:log_item_enchant(State),
	{ok,[]}.

log(RoleID,ItemUID,ItemTypeID,OldEnchantType,OldEnchantLevel,NewEnchantType,NewEnchantLevel,Date,Time) ->
	erlang:send(?MODULE, {add, {RoleID,ItemUID,ItemTypeID,OldEnchantType,OldEnchantLevel,NewEnchantType,NewEnchantLevel,Date,Time}}).