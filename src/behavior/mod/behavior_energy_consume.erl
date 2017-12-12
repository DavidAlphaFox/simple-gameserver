%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十一月 2017 11:04
%%%-------------------------------------------------------------------
-module(behavior_energy_consume).
-author("chenlong").

%% API
-export([write/1,log/9]).


write(State) ->
	db_sql:log_energy_consume(State),
	{ok,[]}.

log(RoleID, VipLevel, Energy, CurEnergy, Date, Time, Type, ArgID, Desc) ->
	erlang:send(?MODULE, {add, {RoleID, VipLevel, Energy, CurEnergy, Date, Time, Type, ArgID, Desc}}).