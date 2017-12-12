%% @author caohongyang
%% @doc @todo 赠送元宝获得表


-module(behavior_gold_bonus_add).
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
	db_sql:log_gold_bonus_add(State),
	{ok,[]}.

log(RoleID, VipLevel, GoldBonus, CurGoldBonus, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, GoldBonus, CurGoldBonus, Date, Time, Type, ArgID, Desc}}).