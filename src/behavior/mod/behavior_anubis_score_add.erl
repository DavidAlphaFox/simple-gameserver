%% @author crimoon26
%% @doc 飞机相关日志


-module(behavior_anubis_score_add).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


write(State) ->
	db_sql:log_anubis_score_add(State),
	{ok,[]}.

log(FamilyID,OldFamilyKillNum,OldFamilyScore,FamilyKillNum,FamilyScore,Time,OperateType) ->
    case data_anubis:get(log_switch) of
        true ->
            erlang:send(?MODULE, {add, {FamilyID,OldFamilyKillNum,OldFamilyScore,FamilyKillNum,FamilyScore,Time,OperateType}});
        _ ->
            ignore
    end.

