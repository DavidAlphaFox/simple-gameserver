%% @author crimoon26
%% @doc 好友送体力日志


-module(behavior_pvp_fight).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_pvp_fight(State),
	{ok,[]}.

log( RoleID,Result,Rank) ->
	{Date,Time} = erlang:localtime(),
	erlang:send(?MODULE, {add, {RoleID, Date, Time,Result,Rank}}).