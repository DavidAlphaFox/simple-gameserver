%% @author caohongyang
%% @doc @todo 武将获得表


-module(behavior_ger_awake).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/13
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_ger_awake(State),
	{ok,[]}.

log(RoleID,GerUID,GerTypeID,AwakeStep,OldSkillID,NewSkillID,OldOptionalSkillID,NewOptionalSkillID,OldRecastTimes,NewRecastTimes,Date,Time,OperationCode)->
	erlang:send(?MODULE, {add, {RoleID,GerUID,GerTypeID,AwakeStep,OldSkillID,NewSkillID,OldOptionalSkillID,NewOptionalSkillID,OldRecastTimes,NewRecastTimes,Date,Time,OperationCode}}).