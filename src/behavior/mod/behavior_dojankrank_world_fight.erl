-module(behavior_dojankrank_world_fight).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([write/1
		,log/17]).

%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_djr_world_add(State),
	{ok,[]}.

log(Time,AtkServerID,AtkDevID,AtkRoleID,AtkLevel,AtkFightPower,AtkVip
   ,RankType,WinType,BeforeRank,AfterRank
   ,DefServerID,DefDevID,DefRoleID,DefLevel,DefFightPower,DefVip) ->
	erlang:send(?MODULE, {add, {Time,AtkServerID,AtkDevID,AtkRoleID,AtkLevel,AtkFightPower,AtkVip
                           ,RankType,WinType,BeforeRank,AfterRank
                           ,DefServerID,DefDevID,DefRoleID,DefLevel,DefFightPower,DefVip}}).
