
-module(behavior_ger_downrank).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
         write/1
        ,log/7
        ]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
    db_sql:log_ger_downrank(State),
    {ok,[]}.

log(RoleID, GerUID, GerTypeID, GerLevel, GerRank, NewRank, Time) ->
    erlang:send(?MODULE, {add, {RoleID, GerUID, GerTypeID, GerLevel, GerRank, NewRank, Time}}).