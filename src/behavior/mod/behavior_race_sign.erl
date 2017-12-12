%% @author crimoon26
%% @doc 好友送体力日志


-module(behavior_race_sign).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_race_sign(State),
	{ok,[]}.

log( RoleID) ->
	{Date,Time} = erlang:localtime(),
	erlang:send(?MODULE, {add, {RoleID, Date, Time}}).