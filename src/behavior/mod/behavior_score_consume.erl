%% @author caohongyang
%% @doc @todo 积分消耗表


-module(behavior_score_consume).
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
	db_sql:log_score_consume(State),
	{ok,[]}.

log(RoleID, VipLevel, Score, CurScore, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Score, CurScore, Date, Time, Type, ArgID, Desc}}).