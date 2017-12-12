%% @author crimoon26
%% @doc 家园交配日志


-module(behavior_homestead_mating).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_homestead_mating(State),
	{ok,[]}.

log( RoleID) ->
	{Date,Time} = erlang:localtime(),
	erlang:send(?MODULE, {add, {RoleID, Date, Time}}).