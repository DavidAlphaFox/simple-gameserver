%% @author crimoon26
%% @doc 好友送体力日志


-module(behavior_world_boss).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_world_boss(State),
	{ok,[]}.

log(RoleID, Type,Harm) ->
	{Date,Time} = erlang:localtime(),
	erlang:send(?MODULE, {add, {RoleID, Date, Time, Type,Harm}}).