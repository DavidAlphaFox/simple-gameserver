%% @author caohongyang
%% @doc 行为日志模版进程
%% Created 2013-7-11


-module(behavior_mod).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("def_behavior.hrl").

-export([start_link/1,start/1,stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0,func/2]).



%% ===================Dict Key Begin =========================

%% ===================Dict Key End   =========================



%% @doc 获取进程状态
i() ->
	gen_server:call(?MODULE, i).

%% @doc 执行函数
func(F,Args) ->
	gen_server:call(?MODULE, {func, F,Args}).

%% @doc 关闭本进程 
stop() ->
%% 	gen_server:call(?MODULE, stop).
%%     supervisor:terminate_child(behavior_sup, ?MODULE),
%%     supervisor:delete_child(behavior_sup, ?MODULE).
ok.
	
start(Mod) ->
    {ok,_}=supervisor:start_child(behavior_sup, 
                           {Mod,
                            {?MODULE, start_link, [Mod]},
                            permanent, 600000, worker, [Mod]}).

start_link(Mod) ->
    gen_server:start_link({local, Mod}, ?MODULE, [Mod], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([Mod]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	set_mod(Mod),
	?INFO("start behavior mod =~w",[Mod]),
	State = init_state(),
	tick(), 
    {ok, State}.

set_mod(Mod)->
	put(mod, Mod).

get_mod() ->
	get(mod).

tick() ->
	erlang:send_after(2000, self(), tick).
 
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
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(i, _From, State) ->
	{reply, State, State};
handle_call({func, F, Args}, _From, State) ->
	Result = ?CATCH(apply(F,Args)),
	{reply, Result, State};
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
handle_info({add, Ele},State) ->
	{noreply, [Ele|State]};
handle_info(tick, State) ->
	{ok, State2} = work(State),
	tick(),
	{noreply, State2};
handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.



-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
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

%% @doc 初始化进程状态
init_state() ->
	[].

work(State) ->
	Mod = get_mod(),
    case catch Mod:write(State) of
        {ok, _} = NewState ->
            NewState;
        _ ->
            {ok, State}
    end.
