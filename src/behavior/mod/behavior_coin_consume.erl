%% @author caohongyang
%% @doc @todo 银两消耗表


-module(behavior_coin_consume).
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
	db_sql:log_coin_consume(State),
	{ok,[]}.

log(RoleID, VipLevel, Coin, CurCoin, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Coin, CurCoin, Date, Time, Type, ArgID, Desc}}).