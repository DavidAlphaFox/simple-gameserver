%% @author caohongyang
%% @doc @todo 武将获得表


-module(behavior_ger_crystal).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/15
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_ger_crystal(State),
	{ok,[]}.


log(RoleID,GerUID,GerTypeID,CrystalType,OldQuality,NewQuality,OldLevel,NewLevel,OldExp,NewExp,OldRankExp,NewRankExp,Date,Time,OperationCode)->
	erlang:send(?MODULE, {add, {RoleID,GerUID,GerTypeID,CrystalType,OldQuality,NewQuality,OldLevel,NewLevel,OldExp,NewExp,OldRankExp,NewRankExp,Date,Time,OperationCode}}).