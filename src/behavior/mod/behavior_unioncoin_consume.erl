%% @author lijunfeng
%% @doc @todo 公会货币消耗日志表


-module(behavior_unioncoin_consume).
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
	db_sql:log_unioncoin_consume(State),
	{ok,[]}.

log(RoleID, VipLevel, Unioncoin, CurUnioncoin, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Unioncoin, CurUnioncoin, Date, Time, Type, ArgID, Desc}}).