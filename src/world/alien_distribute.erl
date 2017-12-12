-module(alien_distribute).

-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). 
-compile(export_all).
-include("common.hrl").
-include("data.hrl").
-include("def_alien.hrl").

%% 分配服务器现在将控制各个主服的状态切换
-record(state2, {status=?STATUS_CLOSE, is_all_master_connect=false, is_distributed=false, is_all_notice=false, next_status_time_stamp=0, 
                    master_list=[], slave_list=[], notice_list=[], finals_node=[], finals_name=[] }).

-record(state3, {status=?STATUS_CLOSE, is_all_master_connect=false, is_distributed=false, is_all_notice=false, next_status_time_stamp=0, 
                    master_list=[], slave_list=[], notice_list=[], finals_node=[], finals_name=[], distribute_list=[]}).

-define(CHECK_INTERVAL, 1000 * 10).
-define(CHECK_INTERVAL2, 1000 * 100).
-define(DUMP_INTERVAL, (1000 * 60 * 10)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start( ) ->
    {ok,_} = 
    supervisor:start_child(world_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    erlang:set_cookie(erlang:node(),data_setting:get(cookie)),
    NewState = case db_sql:get_etc(?DB_ETC_KEY_ALIEN_DISTRIBUTE) of
		        [{state,_State}] ->
                    %% 从state迁移到state2时,先设置为关闭状态,并且加消息锁,然后再手动修正 
                    put(message_block, true),
                    #state2{next_status_time_stamp=util:now() + data_alien:get({left_seconds, ?STATUS_CLOSE})}; 
		        [{state2,State2}] ->
                    [_|List] = erlang:tuple_to_list(State2),
                    erlang:list_to_tuple([state3] ++ List ++ [[]]);
                [{state3,State3}] ->
                    State3;
		        _ ->
                    %% 第一次开启,计算下个状态的时间戳
                    StartTime = data_alien:get(start_time),
                    {Date, NowTime} = erlang:localtime(),
                    CloseTime = data_alien:get({left_seconds, ?STATUS_CLOSE}),
                    NextOpenTimeStamp = 
                        case NowTime =< StartTime of
                            true ->
                                util:datetime_to_seconds({Date,StartTime}) + CloseTime;
                            _ ->
                                util:datetime_to_seconds({Date,StartTime}) + CloseTime + ?ONE_DAY_SECONDS 
                        end,
                    #state3{next_status_time_stamp=NextOpenTimeStamp}
		    end,
    put(master_list, get_master_server_list( )),
    put(slave_list, get_slave_server_list( )),
    FinalsNode = alien_finals:get_alien_finals_node(),
    FinalsName = alien_finals:get_alien_finals_name(),
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    erlang:send_after(?CHECK_INTERVAL, erlang:self(),check_connection),
	erlang:send_after(?DUMP_INTERVAL, self(),do_hibernate),
    {ok, NewState#state3{finals_node=FinalsNode, finals_name=FinalsName,is_all_master_connect=false}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(do_hibernate,State)->
	erlang:send_after(?DUMP_INTERVAL, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    do_persist(State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%%%%% alien_server 断线,alien_server请求master_status,按照alien_server重启处理  ..slave挂掉,没任何影响,master的广播消息会在mq中。。
%% alien_master 断线,alien_master请求alien_distribute状态,alien_distribute 用update_status回复
do_handle_info({request_distribute_status,ServerID},#state3{status=Status,next_status_time_stamp=TimeStamp}=State)->
	send_msg:direct(ServerID,alien_master_server,{update_status, Status, TimeStamp}),
	{noreply,State};

%% alien_final 断线,alien_final不需要请求alien_distribute状态,分配服发出去的消息会在mq中.
%% alien_distribute 断线,alien_distribute 对alien_master广播update_status,对alien_final广播当前状态
do_handle_info(notice_request_status,#state3{master_list=MasterList,slave_list=SlaveList,status=Status,next_status_time_stamp=TimeStamp,finals_name=FinalsName,distribute_list=DisList}=State) ->
	Msg = {update_status, Status, TimeStamp},
	lists:foreach(fun({MastName,DisSlaves}) -> 
						  send_msg:direct_by_name(MastName,alien_master_server,Msg),
                          send_msg:direct_by_name(MastName,alien_master_server,{update_server_list,reconnect,DisSlaves})
				  end, DisList),
    %% 只在决赛时通知(防止决赛开始的消息没有发出去)
    case Status =:= ?STATUS_FINAL of
        true ->
            DistributeName = get_alien_distribute_name(),
            DistributeNode = get_alien_distribute_node( ),
	        send_msg:direct_by_name(FinalsName,alien_finals,{notice_status_is_final, DistributeName, DistributeNode, MasterList, SlaveList});
        _ ->
            ignore
    end,
	{noreply,State};

do_handle_info(check_connection, State) ->
    NewState = check_connection(State),
    {noreply, NewState};

do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    {noreply, State};

do_handle_info(change_to_next_status, State) ->
    #state3{status=NewStatus, master_list=MasterList, next_status_time_stamp=NewNextTimestamp} = NewState = change_status(State),
    bc_msg_to_master(MasterList, {update_status, NewStatus, NewNextTimestamp}), 
    {noreply, NewState};

do_handle_info({set_time_stamp, TimeStamp}, #state3{master_list=MasterList, status=Status}=State) ->
    NewState = State#state3{next_status_time_stamp=TimeStamp},
    bc_msg_to_master(MasterList, {update_status, Status, TimeStamp}), 
    {noreply, NewState};

do_handle_info({set_message_block, IsBlock}, State) ->
    put(message_block, IsBlock),
    {noreply, State};

%%决赛服在断线重连后发送过来的状态切换信息
do_handle_info(notice_alien_finals_finish, #state3{status=Status}=State) ->
    case Status =:= ?STATUS_FINAL of
        true ->
            erlang:send(?MODULE, alien_finals_finish);
        _ ->
            ignore
    end,
    {noreply, State};

do_handle_info(alien_finals_finish, #state3{status=Status}=State) ->
    case Status =:= ?STATUS_FINAL of
        false ->
            ?ERR("警告: 分配服在非决赛期间收到了决赛结束的消息", []),
            NewState = State;
        _ ->
		    #state3{status=NewStatus, master_list=MasterList, next_status_time_stamp=NewNextTimestamp} = NewState = change_status(State),
		        
		    %% 广播到各个master
		    bc_msg_to_master(MasterList, {update_status, NewStatus, NewNextTimestamp}), 

            %% 更新主服和从服列表
            ?ERR("更新主服、从服列表.~n", []),
            put(master_list, get_master_server_list( )),
            put(slave_list, get_slave_server_list( ))
    end,
    {noreply, NewState};

do_handle_info({set_status_and_time_stamp, NewStatus, NewTimeStamp, NeedNotice}, #state3{master_list=MasterList}=State) ->
    NewState = State#state3{status=NewStatus, next_status_time_stamp=NewTimeStamp},
    case NeedNotice of
        false ->
            ignore;
        _ ->
            bc_msg_to_master(MasterList, {update_status, NewStatus, NewTimeStamp}) 
    end,
    {noreply, NewState};

do_handle_info({check_server_alive,Ref,DistributeServer},State) ->
	send_msg:direct_by_name(DistributeServer,alien_distribute,{server_alive, Ref}),
	{noreply,State};

do_handle_info({alien_server_get_master_server_list,ServerID}, State) ->
	send_msg:direct(ServerID,alien_server,{alien_server_master_server_list,[E||{_,E}<-get_master_server_list()],alien_finals:get_alien_finals_name()}),
	{noreply,State};

do_handle_info({set_master_list, MasterList}, State) ->
    {noreply, State#state3{master_list=MasterList}}.


check_server_alive(ServerName,DistributeServer,Router) ->
	Ref = erlang:make_ref(),
	case send_msg:direct_by_name(ServerName,Router, {check_server_alive,Ref,DistributeServer}) of
		?undefined ->
			msg_queue_down;
		_ ->
			receive 
				{server_alive, Ref} ->
					true
			after 3000 ->
					wait_msg_failed
			end
	end.

check_connection(#state3{is_distributed=false,finals_node=_FinalsNode,finals_name=FinalsName,is_all_master_connect=IsConnectedAllJudge}=State) ->
	erlang:send_after(?CHECK_INTERVAL, self(), check_connection),
	NeedCheck = (util:now() rem 1000) == 0,
    IsAllConnect = 
	    if not(IsConnectedAllJudge) orelse NeedCheck->
		    DistributeServer = get_alien_distribute_name(),
		    case get(master_list) of
			    ?undefined ->
                    false;
			    MList ->
				    lists:foldl(fun({_,MastName}, Acc) ->
								    case check_server_alive(MastName,DistributeServer,alien_master_server) of
								        true ->
                                            Acc andalso true;
									    Reason ->
											?ERR("master server unconnected:~w,reason:~w",[MastName,Reason]),
                                            false
								    end
							    end, true, MList) 
		   end,
		   case check_server_alive(FinalsName,	DistributeServer ,alien_finals) of
			   true ->
				   ignore;
			   Reason ->
				   ?ERR("finals server unconnected:~w, reason:~w",[FinalsName,Reason])
		   end;
	    true ->
                true
	    end,
	check_to_next(State#state3{is_all_master_connect=IsAllConnect,master_list=get(master_list),slave_list = get(slave_list)});
%%	check_to_next(State#state3{master_list=MasterList, slave_list=SlaveList, is_all_master_connect=IsAllConnect});
%% 没有分配前 检查每个配置的节点 并且更新当前连通了的节点

%% I don't think check is needed.. what we need is just go to next..
check_connection(#state3{is_distributed=true}=State) ->
	erlang:send_after(?CHECK_INTERVAL2, self(), check_connection),
	check_to_next(State).
%% 分配后,只检测已经分配了节点, 并且不再更新节点列表

get_slave_server_list( ) ->
    SlaveList = data_alien_distribute:get(slave),
    lists:foldl(fun({ServerID, Platform, IP}, Acc) ->
                    ServerName = alien_master_server:get_cross_server_name(ServerID, false, Platform),
                    NodeName = lists:concat([Platform,ServerID,'@',IP]),
                    [{erlang:list_to_atom(NodeName),ServerName}|Acc] end,[], SlaveList).

get_master_server_list( ) ->
    MasterList = data_alien_distribute:get(master),
    lists:foldl(fun({ServerID, Platform, IP}, Acc ) ->
                    ServerName = alien_master_server:get_cross_server_name(ServerID, true, Platform),
                    NodeName = alien_master_server:get_master_node(IP,ServerID,Platform),
                    [{NodeName,ServerName}|Acc] end,[], MasterList).

do_distribute() ->
    MasterList = erlang:get(master_list),
    SlaveList = erlang:get(slave_list),
    MasterNum = erlang:length(MasterList),
    SlaveNum = erlang:length(SlaveList),
    EachNum = erlang:max(1, SlaveNum div MasterNum), 
    do_distribute(MasterNum,util:random_list2(MasterList),util:random_list2(SlaveList),EachNum,[]).
    
do_distribute(_,_,[],_,Acc) ->
    Acc;
do_distribute(1,[{_,MasterName}|_],SlaveList,_,Acc) ->
    ?ERR("分配从服务器:~p到主服务器:~p.~n",[SlaveList,MasterName]),
    send_msg_to_master(MasterName,{update_server_list, SlaveList}),
    [{MasterName,SlaveList}|Acc];
do_distribute(Num,[{_,MasterName}|T],SlaveList,EachNum,Acc) ->
    List = lists:sublist(SlaveList,EachNum),
    ?ERR("分配从服务器:~p到主服务器:~p.~n",[List,MasterName]),
    send_msg_to_master(MasterName, {update_server_list, List}),
    do_distribute(Num-1,T,SlaveList -- List,EachNum,[{MasterName,List}|Acc]).

send_msg_to_master(MasterServer,Msg) ->
    case get(message_block) of
        true ->
            ignore;
        _ ->
			send_msg:direct_by_name(MasterServer,alien_master_server,Msg)
    end.
bc_msg_to_master(MasterList,Msg) ->
    case get(message_block) of
        true ->
            ignore;
        _ ->
            lists:foreach(fun({_,E}) -> 
									send_msg:direct_by_name(E,alien_master_server,Msg)
								  end, MasterList)
    end.
    
%% 状态结束时, 通知决赛服等待、分配子服、进入报名状态
change_status(#state3{status=?STATUS_CLOSE, next_status_time_stamp=NextTimestamp,finals_name=FinalsName}=State) ->
    NewStatus = ?STATUS_SIGN,
    NewNextTimestamp = NextTimestamp + data_alien:get({left_seconds,NewStatus}),
    %% 报名时通知决赛服,进入等待状态
	send_msg:direct_by_name(FinalsName,alien_finals,change_status_to_wait),
    DisList = do_distribute(),
	?ERR("分配完毕,开始报名.~n",[]),
	State#state3{status=NewStatus, is_distributed=true, next_status_time_stamp=NewNextTimestamp,distribute_list=DisList};  

%% 总决赛结束后,重置状态
change_status(#state3{status=?STATUS_FINAL}=State) ->
    NewStatus = ?STATUS_CLOSE,
    %% 计算下次开启的时间
    StartTime = data_alien:get(start_time),
    {Date, NowTime} = erlang:localtime(),
    CloseTime = data_alien:get({left_seconds, ?STATUS_CLOSE}),
    NextOpenTimeStamp = 
           case NowTime =< StartTime of
               true ->
                    util:datetime_to_seconds({Date,StartTime}) + CloseTime;
               _ ->
                    util:datetime_to_seconds({Date,StartTime}) + CloseTime + ?ONE_DAY_SECONDS 
           end,
    State#state3{status=NewStatus, is_distributed=false, is_all_notice=false, notice_list=[], next_status_time_stamp=NextOpenTimeStamp};

%% 决赛期间的时间和状态控制,交由决赛服处理,这里时间戳直接设在为0
change_status(#state3{status=?STATUS_FIGHT, master_list=MasterList, slave_list=SlaveList, finals_name=FinalsName}=State) ->
    NewStatus = ?STATUS_FINAL,
    DistributeName = get_alien_distribute_name(),
    DistributeNode = get_alien_distribute_node( ),
	send_msg:direct_by_name(FinalsName,alien_finals,{pre_open_finals, DistributeName, DistributeNode, MasterList, SlaveList}),
    State#state3{status=NewStatus, next_status_time_stamp=0}; 

change_status(#state3{status=Status, next_status_time_stamp=NextTimestamp}=State) ->
    NewStatus = Status + 1,
    NewNextTimestamp = NextTimestamp + data_alien:get({left_seconds,NewStatus}),
    State#state3{status=NewStatus, next_status_time_stamp=NewNextTimestamp}.

check_to_next(#state3{status=Status, next_status_time_stamp=NextTimestamp,master_list=MasterList,notice_list=NoticeList, is_all_notice=IsAllNotice}=State) ->
    %% 决赛期间,分配服交出控制权,直到决赛服返回结束通知
    %% 在决赛期间, 从服将会和决赛服直接沟通
    case Status =:= ?STATUS_FINAL of
        true ->
            State;
        _ ->
		    Now = util:now(),
		    case NextTimestamp > Now of
		        true ->
                    %% 向没有收到更新消息的主服发送当前状态
		            case IsAllNotice of
		                true ->
		                    State;
		                _ ->
		                    NeedNotice = MasterList -- NoticeList,
		                    case NeedNotice == [] of
		                        true ->
		                            State;
		                        _ ->
		                            bc_msg_to_master(NeedNotice, {update_status, Status, NextTimestamp}), 
		                            NewNoticeList = NoticeList ++ NeedNotice,
		                            NewIsAllNotice = length(NewNoticeList) =:= length(get(master_list)),
		                            State#state3{notice_list=NewNoticeList, is_all_notice=NewIsAllNotice}
		                    end
		            end;
		        _ ->
		            %% 改变状态
		            #state3{status=NewStatus, master_list=MasterList, next_status_time_stamp=NewNextTimestamp} = NewState = change_status(State),
		        
		            %% 广播到各个master
                    ?ERR("通知master切换状态到:~w,时间戳:~w~n,master列表:~w", [NewStatus, NewNextTimestamp, MasterList]),
		            bc_msg_to_master(MasterList, {update_status, NewStatus, NewNextTimestamp}), 
		            NewState
		    end
    end.

do_persist(State) ->
    Info = [{state3,State}],
    db_sql:set_etc(?DB_ETC_KEY_ALIEN_DISTRIBUTE, Info).

get_alien_distribute_name() ->
    {ServerID, Platform, _IP} = data_alien_distribute:get(distribute),
    get_alien_distribute_name(ServerID,Platform).

get_alien_distribute_name(ServerID,Platform) ->
    erlang:list_to_atom(lists:concat(['alien_distribute_', Platform, ServerID])).

get_alien_distribute_node() ->
    {ServerID, Platform, IP} = data_alien_distribute:get(distribute),
    erlang:list_to_atom(lists:concat([Platform, '_distribute_', ServerID, '@', IP])).


%%------------------------------------------------------------------------------
%%管理命令
%% 切换到下个状态(安全)
change_to_next_status() ->
    erlang:send(?MODULE, change_to_next_status).

%% 修改下个状态的开始时间戳(安全)
set_time_stamp(TimeStamp) ->
    erlang:send(?MODULE, {set_time_stamp, TimeStamp}).

%% 设置消息锁
%% 用途:
%% 需要对分配服进行修正,且不相影响主服时,可以先把消息通知锁住 set_message_block(IsBlock) ->
set_message_block(IsBlock) ->
    erlang:send(?MODULE, {set_message_block, IsBlock}).

%% 设置当前的状态和下一个时间戳
%% 用途:
%% 1. 状态切换功能移到分配服之后,第一次更新后用来将分配服的状态修正为主服的状态,并校正时间
%% 2. 合服后用来调整下次开启的时间
%% 因为该命令直接修改状态,不会执行状态切换时的相关操作,因此需要小心使用
set_status_and_time_stamp(Status, TimeStamp, NeedNotice) ->
    erlang:send(?MODULE, {set_status_and_time_stamp, Status, TimeStamp, NeedNotice}).

%% 设置主服列表(异常时修正数据用)
set_master_list(MasterList) ->
    erlang:send(?MODULE, {set_master_list, MasterList}).

%% 断线后重新给主服分发数据
renotice_master() ->
    erlang:send(?MODULE, notice_request_status).
