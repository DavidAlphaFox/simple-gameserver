%% @author lijunfeng
%% @doc @todo 公会货币获得日志表


-module(behavior_unioncoin_add).
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
    db_sql:log_unioncoin_add(State),
    {ok,[]}.

log(RoleID, VipLevel, Unioncoin, CurUnioncoin, Date, Time, Type, ArgID, Desc) ->
    erlang:send(?MODULE, {add, {RoleID, VipLevel, Unioncoin, CurUnioncoin, Date, Time, Type, ArgID, Desc}}).