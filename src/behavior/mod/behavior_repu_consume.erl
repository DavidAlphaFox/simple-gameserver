%% @author caohongyang
%% @doc @todo 声望消耗表


-module(behavior_repu_consume).
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
	db_sql:log_repu_consume(State),
	{ok,[]}.

log(RoleID, VipLevel, Repu, CurRepu, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Repu, CurRepu, Date, Time, Type, ArgID, Desc}}).