%% @author lixinglong

-module(behavior_login_log).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/4
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_login(State),
	{ok,[]}.

log(Accid, RoleID, DevID, IP) ->
	DateTime = erlang:localtime(),
	{LogoutTime,LoginTime} = db_sql:get_role_lastLogoutTime(RoleID),   
	Duration=LogoutTime-LoginTime,
	if Duration >= 0 ->
		   erlang:send(?MODULE, {add, {Accid, RoleID, DevID, IP, DateTime,Duration}});
	   true->
		   erlang:send(?MODULE, {add, {Accid, RoleID, DevID, IP, DateTime,0}})
	end.