%% @author crimoon-17
%% @doc @todo Add description to activityVipServer.


-module(activityVipServer).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
%-record(state, {}).
-define(TICK_STATE_INTERVAL,600).

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	State1 = init_state(),
	insert_ets({state,State1}),
	tick_state(),
	{ok, State1}.

tick_state()->
	Sec = ?TICK_STATE_INTERVAL - (util:now() rem ?TICK_STATE_INTERVAL) - 1, 
	erlang:send_after(Sec*1000 + 700 + random:uniform(600),self(),tick_state).

time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).

insert_ets(Value) ->
	ets:insert(?ETS_VIP_ACTIVITY, Value).
delete_ets(Key) ->
    ets:delete(?ETS_VIP_ACTIVITY, Key).

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(tick_state,_State) ->
	tick_state(),
	State1 = init_state(),
	insert_ets({state,State1}),
	{noreply,State1,hibernate};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
write_config(Out, [], _, _) ->
    file:close(Out),
    tk_config:reload_config(data_activity_vip);

write_config(Out, [CID|T], ID, DataActivity) ->
    TDataActivity = 
        case CID =:= ID of
            true ->
                DataActivity;
            _ ->
                data_activity_vip:get(CID) 
        end,
    file:write(Out, io_lib:format("~p.~n", [TDataActivity])),
    write_config(Out, T, ID, DataActivity).

write_config(ID, DataActivity) ->
    {ok, Out} = file:open("config/activityconfig/data_activity_vip.config", write),
    write_config(Out, data_activity_vip:get_list(), ID, DataActivity ). 

init_state([], _) ->
    delete_ets(restart_timestamp),
    [#data_activity_vip{activityID=0,startTime=0,endTime=0,items=[]}];
init_state([ID|T], Now) -> 
    #data_activity_vip{auto_restart=AutoRestart,startTime=StartTime, endTime=EndTime} = DataActivity =  data_activity_vip:get(ID),
    {StartDay,StartSec} = StartTime,
    if erlang:is_tuple(StartDay) ->
        Start = util:datetime_to_seconds(StartTime),
        End = util:datetime_to_seconds(EndTime);
    true ->
        {ServerOpenDate,_} = data_setting:get(serverOpenTime),
        Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay - 1) * ?ONE_DAY_SECONDS,
        {EndDay, EndSec} = EndTime,
        End = util:datetime_to_seconds({ServerOpenDate,EndSec}) + (EndDay - 1) * ?ONE_DAY_SECONDS
	end,
    if Now > Start , Now < End ->
        delete_ets(restart_timestamp),
		[DataActivity#data_activity_vip{startTime=Start,endTime=End}];
    true ->
        case AutoRestart of
            0 ->
                init_state(T, Now);
            _ ->
                RStart  = Now,
                REnd = Now + AutoRestart,
                write_config(ID, DataActivity#data_activity_vip{startTime=util:seconds_to_datetime(RStart), endTime=util:seconds_to_datetime(REnd)}),
                insert_ets({restart_timestamp, RStart}),
		        [DataActivity#data_activity_vip{startTime=RStart,endTime=REnd}] 
        end 
	end.

init_state() ->
	Now = util:now(),
    init_state(data_activity_vip:get_list(), Now).
