-module(behavior_home_resource_add).
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
	db_sql:log_home_resource_add(State),
	{ok,[]}.

log(RoleID, VipLevel, Coin, CurCoin, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Coin, CurCoin, Date, Time, Type, ArgID, Desc}}).