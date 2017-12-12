%% @author : lixinglong
%% @doc : 赏金副本

-module(bounty_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-compile(export_all).

-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).


-record(state, {lastCalcDate=0,type=0}).

-define(TICK_STATE_INTERVAL,60).

i() ->
	gen_server:call(?MODULE, i).

start() ->
	{ok,_}=
	supervisor:start_child(world_sup, 
						   {?MODULE,
							{?MODULE, start_link, []},
							permanent, 600000, worker, [?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	random:seed(util:gen_random_seed()),
	State=get_db_info(),
	process_flag(trap_exit,true),
	dump_tick(),
	State2 = update_state(State),
	ets:insert(?ETS_BOUNTY, State2),
	{ok, State2}.
 
update_state(State) ->
	#state{lastCalcDate=LastCalcDate,type=Type} = State,
	case Type of
		0 ->
			new_state(0);
		_ ->
			LastDay = calendar:date_to_gregorian_days(LastCalcDate),
			Today = calendar:date_to_gregorian_days(erlang:date()),
			if Today - LastDay > 2 ->
				   new_state(Type);
			   true ->
				   State
			end
	end.

new_state(Type) ->
	NewType = Type rem 3 + 1,
	#state{lastCalcDate=erlang:date(),type=NewType}.

handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(dump_tick, State)->
	do_write_db(State),
	NewState = update_state(State),
	ets:insert(?ETS_BOUNTY, NewState),
	dump_tick(),
	{noreply, NewState, hibernate};

handle_info({test_set_date,Date},State) ->
	NewState = State#state{lastCalcDate=Date},
	{noreply,NewState};

handle_info(_Info, State) ->
	%?CATCH(do_handle_info(Info)),
	{noreply, State}.

terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	do_write_db(State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

dump_tick() ->
	Sec = ?TICK_STATE_INTERVAL - (util:now() rem ?TICK_STATE_INTERVAL) - 1, 
	erlang:send_after(Sec*1000, self(), dump_tick).

get_db_info()->
	case db_sql:get_etc(?DB_ETC_KEY_BOUNTY) of
		X when erlang:is_record(X, state) ->
			X;
		_ ->
			#state{}
	end.

do_write_db(State) ->
	db_sql:set_etc(?DB_ETC_KEY_BOUNTY, State).

