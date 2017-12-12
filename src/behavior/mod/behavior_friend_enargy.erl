%% @author crimoon26
%% @doc 好友送体力日志


-module(behavior_friend_enargy).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_friend_enargy(State),
	{ok,[]}.

log( RoleID,DRoleID,Type,Value) ->
	{Date,Time} = erlang:localtime(),
	erlang:send(?MODULE, {add, {RoleID, Date, Time,DRoleID,Type,Value}}).