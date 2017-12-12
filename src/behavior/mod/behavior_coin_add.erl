%% @author caohongyang
%% @doc @todo 银两获得表


-module(behavior_coin_add).
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
	db_sql:log_coin_add(State),
	{ok,[]}.

log(RoleID, VipLevel, Coin, CurCoin, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Coin, CurCoin, Date, Time, Type, ArgID, Desc}}).