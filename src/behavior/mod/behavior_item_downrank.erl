
-module(behavior_item_downrank).
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
    db_sql:log_item_downrank(State),
    {ok,[]}.

log(RoleID, GerUID, ItemType, ItemLevel, ItemRank, NewRank, Time) ->
    erlang:send(?MODULE, {add, {RoleID, GerUID, ItemType, ItemLevel, ItemRank, NewRank, Time}}).