%% @author caohongyang
%% @doc @todo 武将获得表


-module(behavior_magicBook_update).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/5
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_magicBook_update(State),
	{ok,[]}.

log(RoleID,_Date,Time,BookID,PictureStr) ->
	erlang:send(?MODULE, {add, {RoleID,Time,BookID,PictureStr}}).