%% @author caohongyang
%% @doc supervisor of tcp acceptor
%% Created 2013-2-20


-module(gateway_tcp_acceptor).
-behaviour(gen_server).
-include("common.hrl").
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).


-record(state, {listen_socket, ref}).


start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%% @doc gen_server:init/1
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
init([LSock]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{listen_socket=LSock}}.


%% @doc gen_server:handle_call/3
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
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.


%% @doc gen_server:handle_cast/2
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


%% @doc gen_server:handle_info/2
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_info({event, start}, State) ->
    accept(State);

handle_info({inet_async, LSock, Ref, {ok, Sock}},
    State = #state{listen_socket = LSock, ref = Ref}) ->
    %% patch up the socket so it looks like one we got from
    %% gen_tcp:accept/1
    {ok, Mod} = inet_db:lookup_socket(LSock),
    inet_db:register_socket(Sock, Mod),
	?INFO("inte_sync...",[]),
	inet:setopts(Sock,[{high_watermark,131072},{low_watermark, 65536}]),
    try        
        %% report
        {ok, {Address, Port}} = inet:sockname(LSock),
        {ok, {PeerAddress, PeerPort}} = inet:peername(Sock),
        ?DEBUG("accepted TCP connection on ~s:~p from ~s:~p~n",
                    [inet_parse:ntoa(Address), Port,
                     inet_parse:ntoa(PeerAddress), PeerPort]),
        gw:start_client(Sock)
    catch Error:Reason ->
            ?ERR("unable to accept TCP connection: ~p ~p~n", [Error, Reason]),
            gen_tcp:close(Sock)
    end,
    accept(State);
handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{listen_socket=LSock, ref=Ref}) ->
    %% It would be wrong to attempt to restart the acceptor when we
    %% know this will fail.
    {stop, normal, State};

handle_info({'EXIT', _, shutdown}, State) ->    
    {stop, normal, State};
handle_info({'EXIT', _, _Reason}, State) ->
    {noreply, State};
handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.

%% @doc gen_server:terminate/2
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
terminate(Reason, State) ->
	if Reason == shutdown orelse Reason == normal ->
		   ignore;
	   true ->	
		   ?ERR("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))])
	end,
	ok.


%% @doc gen_server:code_change/3
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
accept(State = #state{listen_socket=LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> 
            {noreply, State#state{ref=Ref}};
        _Error ->             
            {noreply, State}
    end.

