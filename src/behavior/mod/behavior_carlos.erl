%% @author crimoon26
%% @doc 飞机相关日志


-module(behavior_carlos).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_carlos(State),
	{ok,[]}.

log(IDList, CarlosType, OpType) ->
    log(IDList, CarlosType, OpType, 0).

log(IDList, CarlosType, OpType, Extra) ->
    case data_carlos:get(log_switch) of
        true ->
	        DateTime = erlang:localtime(),
            erlang:send(?MODULE, {add, {IDList, CarlosType, OpType, DateTime, Extra}});
        _ ->
            ignore
    end.

