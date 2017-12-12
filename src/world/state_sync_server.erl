%% @author caohongyang
%% @doc 跟帐号服务器同步服务器状态
%% Created 2013-3-28



-module(state_sync_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0, sync_state/1, get_state/0]).

-define(state_stop, 1).%维护
-define(state_new, 2).%新
-define(state_crowd,3).%爆满
-define(state_busy,4).%拥挤
-define(state_fluency,5).%流畅
-define(SYNC_SUCCESS,1).
-define(SYNC_FAILED,0).
-define(SYNC_INTERVAL, 5*60*1000).

i() ->
	gen_server:call(?MODULE, i).

get_state() ->
	case data_setting:get(is_new_server) of
		true ->
			?state_new;
		_ ->
			OnlineNum = ets:info(?ETS_ROLE_ONLINE,size),
			MaxOnlineNum = data_setting:get(max_online_num),
			Ratio = OnlineNum/MaxOnlineNum,
			if Ratio >= 0.5 ->
				   ?state_crowd;
			   Ratio >= 0.25 ->
				   ?state_busy;
			   true ->
				   ?state_fluency
			end
	end.

start() ->
	{ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([]) ->
	random:seed(util:gen_random_seed()), 
	process_flag(trap_exit,true),
	inets:start(),
	do_tick(),
    {ok, []}.

tick() ->
	erlang:send_after(?SYNC_INTERVAL, self(), tick).

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
%% @doc gen_server:init/1
handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.


-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_cast/2
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_info/2
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(tick, State) ->
	do_tick(),
	{noreply, State};

handle_info(sync_state_success,State)->
	put(sync_state_success,?SYNC_SUCCESS),
	{noreply,State};

handle_info(killpid,State)->
	case get(sync_state_success) of
		?SYNC_SUCCESS->
			?INFO("state_sync_success~n");
		_ ->
			case get(state_sync_pid) of
				?undefined->
					?INFO("state_sync_pid is undefined~n");
				Pid ->
					case lists:member(Pid,erlang:processes()) of
						true ->
							user_default:kill(Pid);
						false->
							ignore
					end
			end
	end,
	{noreply,State};

handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.

do_tick() ->
	tick(),
	State = get_state(),
	put(sync_state_success,?SYNC_FAILED),
	sync_state(State).

%% sync_state(State) ->
%%     OldServerID = data_setting:get(server_id),
%%     ServerIDList =
%%         case data_setting:get(merge_server_id_list) of
%%             List when erlang:is_list(List) ->
%%                 [OldServerID|List];
%%             _ ->
%%                 [OldServerID]
%%         end,
%%     lists:foreach(fun(ServerID) ->
%%                           case catch gen_server:call({global, util:get_platform_server()}, {update_state, ServerID, State}) of
%%                               ok ->
%%                                   ok;
%%                               Error ->
%%                                   ?ERR("Error:~w", [Error])
%%                           end
%%                   end, ServerIDList).

sync_state(State)->
    AccountServerAddr = data_setting:get(account_server_url),
    OldServerID = data_setting:get(server_id),
    ServerIDList =
        case data_setting:get(merge_server_id_list) of
            List when erlang:is_list(List) ->
                [OldServerID|List];
            _ ->
                [OldServerID]
        end,
    Pid = spawn(fun()->
    	    lists:foreach(fun(ServerID) ->
                    URL = lists:flatten(io_lib:format("~s/state?serverID=~w&state=~w", [AccountServerAddr,ServerID,State])),
                    httpc:request(get, {URL, []}, [{timeout, 3000}], [])
                    end, ServerIDList),
    	    	erlang:send(?MODULE,sync_state_success)
    	   	end),
    put(state_sync_pid,Pid),
   	erlang:send_after(5000,self(),killpid).


-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	sync_state(?state_stop),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[broadcast_server, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.



-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


