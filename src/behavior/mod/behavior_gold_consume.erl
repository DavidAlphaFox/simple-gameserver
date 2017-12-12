%% @author caohongyang
%% @doc @todo 元宝消耗表


-module(behavior_gold_consume).
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
	db_sql:log_gold_consume(State),
	{ok,[]}.
log(RoleID, VipLevel, Gold, GoldBonus, CurGold, CurGoldBonus, Date, Time, Type, ArgID, Desc,VLog) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Gold, GoldBonus, CurGold, CurGoldBonus, Date, Time, Type, ArgID, Desc,VLog}}).