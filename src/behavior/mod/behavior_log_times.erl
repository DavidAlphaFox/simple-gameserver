%% @author crimoon26
%% @doc @todo Add description to behavior_log_times.


-module(behavior_log_times).

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
	db_sql:log_buyTimes(State),
	{ok,[]}.

log(RoleID, VipLevel, Seq, NewValue, Add,Type) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Seq, NewValue, Add,Type}}).