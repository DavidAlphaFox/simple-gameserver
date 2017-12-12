%% @author crimoon26
%% @doc 好友送体力日志


-module(behavior_dungen_fight).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_dungen_fight(State),
	{ok,[]}.

log( RoleID, DungenID,Result,Type,T) ->
	{Date,Time} = erlang:localtime(),
	erlang:send(?MODULE, {add, {RoleID, DungenID,Result,Date, Time,Type,T}}).