%% @author lixinglong

-module(behavior_create_role).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/6
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_create_role(State),
	{ok,[]}.

log(Accid, RoleID, DevID, IP, Sex, Result) ->
	{Date,Time} = erlang:localtime(),
	erlang:send(?MODULE, {add, {Accid, RoleID, DevID, IP, Sex, Result, Date, Time}}).