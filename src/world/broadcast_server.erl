%% @author caohongyang
%% @doc 协议广播进程
%% Created 2013-3-7

%% 已优化点：按帧发送帧内收到的所有广播消息

-module(broadcast_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-export([start_link/0,start/0,stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([bc/1,bc_msg/1,bc_msgID/1,bc_msgID/2]).

-export([join/1]).

-export([i/0,func/2]).


%% ===================Dict Key Begin =========================

%% ===================Dict Key End   =========================


%% @doc 广播一条协议
bc(Msg) ->
	erlang:send(?MODULE, {broadcast, Msg}).

%% @doc 置顶公告
bc_msg(Msg) ->
	erlang:send(?MODULE, {broadcast, #sc_message_bc{msg=Msg}}).

bc_msgID(MsgID) ->
	erlang:send(?MODULE, {broadcast, #sc_message_bc_id{msgID=MsgID}}).

bc_msgID(MsgID, ArgList) ->
	erlang:send(?MODULE, {broadcast, #sc_message_bc_id2{msgID=MsgID,paramList=ArgList}}).


%% @doc 加入广播队列
join(Pid) ->
	erlang:send(?MODULE, {join, Pid}).


%% @doc 获取进程状态
i() ->
	gen_server:call(?MODULE, i).

%% @doc 执行函数
func(F,Args) ->
	gen_server:call(?MODULE, {func, F,Args}).

%% @doc 关闭本进程 
stop() ->
%% 	gen_server:call(?MODULE, stop).
%%     supervisor:terminate_child(world_sup, ?MODULE),
%%     supervisor:delete_child(world_sup, ?MODULE).
ok.
	
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
	%% @todo 从在线玩家列表中读取在线玩家,防止进程重启后，状态丢失
	State = init_state(),
	tick(), 
    {ok, State}.

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
handle_info(tick, State) ->
	State2 = [E||E<-State, erlang:port_info(E,id) =/= undefined],
	tick(),
	{noreply, State2};
handle_info({join, Pid}, State) ->
	case lists:member(Pid,State) of
		true ->
			{noreply, [State]};
		_ ->
			{noreply, [Pid|State]}
	end;
handle_info({broadcast, Msg}, State) ->
	%% 等待2000ms，按帧发送广播
	timer:sleep(2000),
	{MsgList, NewJoin} = collect_msg([Msg], []),
	%% @todo 合并多条协议
	Bin = [proto:encode(E)|| E <- lists:reverse(MsgList)],
	%% @todo 执行广播
	do_broadcast(Bin, State),
	State2 = NewJoin ++ State,
	{noreply, State2};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
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
%% @doc 广播
do_broadcast(Msg, List) ->
	%% lists:foreach(fun(Pid) -> catch tk_misc:send_sock_force(Pid, Msg) end,List).
	%lists:foreach(fun(Socket) -> catch tk_misc:send_sock(Socket, Msg) end,List). 
	lists:foreach(fun(Socket) -> catch tk_misc:send_sock_async(Socket, Msg) end,List). 

%% @doc 取出队列中的所有消息，减少遍历次数
collect_msg(List, Join) ->
	receive 
		{broadcast, Msg} ->
			collect_msg([Msg|List], Join);
		{join, Pid} ->
			collect_msg(List, [Pid|Join])
	after 0 ->
		{List, Join}
end.

%% @doc 初始化进程状态
init_state() ->
	[Pid||{RoleID,_}<-ets:tab2list(?ETS_ROLE_ONLINE), Pid <- [role_lib:get_sock(RoleID)], Pid=/=?undefined].