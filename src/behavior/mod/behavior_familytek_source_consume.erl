%% @author caohongyang
%% @doc @todo 公会科技捐献资源消耗表


-module(behavior_familytek_source_consume).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/8
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_familytek_source_consume(State),
	{ok,[]}.

log(FamilyID, Cost, Wallet, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {FamilyID, Cost, Wallet, Date, Time, Type, ArgID, Desc}}).