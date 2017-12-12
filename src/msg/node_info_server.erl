%% @author : lixinglong
%% @doc : node_info_server

-module(node_info_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").

-compile(export_all).

-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/0,start_link/0]).


-record(state, {}).
-define(tryConnectDuration,60).
-define(sync_server_info_tick,2000).
-define(checkMasterConnectionInterval,200).
%-define(checkMasterConnection,checkMasterConnection).
-define(tryConnect,tryConnect).
-define(sync_server_info,sync_server_info).
-define(check_slave,check_slave).

start() ->
    {ok,_}=
            supervisor:start_child(msg_sup,
                                   {?MODULE,
                                    {?MODULE,start_link,[]},
                                    permanent,600000,worker,[?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit,true),
	insert_self_info(),
	case data_setting:get(server_type) == familyfight of
		true ->
			check_slave(),
			{ok,#state{}};
		_ ->
			erlang:send_after(?sync_server_info_tick * 1000, self(), ?sync_server_info),
			hibernate_tick(),
			connect_master(),
			check_slave(),
			{ok,#state{}}
	end.

hibernate_tick()-> erlang:send_after(30000, self(), do_hibernate).

handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

%% 回收自己的内存，以及其他程序内存，
handle_info(do_hibernate, State) ->
	hibernate_tick(),
	Self = erlang:self(),
	spawn(fun()->lists:foreach(fun(Pid) -> 
						  case Pid of
							  Self -> ignore;
							  _ ->
								  case process_info(Pid, memory) of
									  {_,Mem} when Mem > 10000000 -> 
										  erlang:garbage_collect(Pid);
									  _ ->
										  ignore
								  end
						  end
						  end, erlang:processes()) end),
	{noreply, State, hibernate};
	
handle_info({update_node_info, ServerList,Node,true},State)->
    IsChange1 = 
        lists:foldl(fun(ServerID,AccIsChange)-> 
                            case ets:lookup(?ETS_NODE_INFO_TABLE, ServerID) of
                                [{ServerID,Node}] ->
                                    AccIsChange;
                                _ ->  % ?undefined or changed
                                    ets:insert(?ETS_NODE_INFO_TABLE,{ServerID,Node}),
                                    true
                            end
                     end, false, ServerList),
    IsChange2 = case ets:lookup(?ETS_NODE_INFO_TABLE2, Node) of
                    [{Node,ServerList}] ->
                        false;
                    _ ->  % ?undefined or changed
                        ets:insert(?ETS_NODE_INFO_TABLE2,{Node,ServerList}),
                        true
                end,
	%% 收到信息以后，不一定要广播，但是一定要回复
	InfoList2 = ets:tab2list(?ETS_NODE_INFO_TABLE2),
	Info = {update_node_info,master,ets:tab2list(?ETS_NODE_INFO_TABLE),InfoList2},
    if
        IsChange1 =:= true orelse IsChange2 =:= true ->
        	lists:foreach(fun({ToNode,_})-> send_msg_to_node(ToNode,Info) end, InfoList2);
        true ->
            send_msg_to_node(Node,Info)     %% 至少给请求者回复节点数据
	end,
	{noreply,State};
handle_info({update_node_info, ServerList,Node},State)->
	lists:foreach(fun(ServerID)->
						  ets:insert(?ETS_NODE_INFO_TABLE,{ServerID,Node})
				  end, ServerList),
	send_msg_to_node(Node, {update_node_info,master,ets:tab2list(?ETS_NODE_INFO_TABLE),ets:tab2list(?ETS_NODE_INFO_TABLE2)}),
	{noreply,State};
%%  把主服务器数据获取来更新,但是并不删除数据.已经失效的节点,在节点自检时,因为没在nodes()中而被清理掉.
handle_info({update_node_info,master,NodeInfos,InfoList2},State)->
%% 	lists:foreach(fun(Info) -> ets:insert(?ETS_NODE_INFO_TABLE,Info) end, NodeInfos),
%% 	lists:foreach(fun(Info2) -> ets:insert(?ETS_NODE_INFO_TABLE2,Info2) end, InfoList2),
    ets:insert(?ETS_NODE_INFO_TABLE,NodeInfos),
    ets:insert(?ETS_NODE_INFO_TABLE2,InfoList2),
	{noreply,State};
handle_info({route,Router,_Time,Msg},State) ->
	catch erlang:send(Router,Msg),
	{noreply,State};

%% handle_info(?checkMasterConnection,State)->
%% 	check_master_connection(),
%% 	{noreply, State,hibernate};

handle_info(?tryConnect,State)->
	connect_master(),
	{noreply, State};

%% 节点无效时候，不删除，删除node-id的映射，不删除id-node的映射
%% master进行node更新的时候，会更新id-node映射
%% 节点监控与master的连接，当master连接断开，会尝试进行重连，重连成功，会把本地node信息更新到master
%% master将自己已知的node信息广播给所有自己连接的node
handle_info(check_slave,State) ->
	check_slave(),
	Nodes =[erlang:node()|nodes()],
	Need = 
	lists:foldl(fun({Node,_ServerList},ST)->
						  case lists:member(Node,Nodes) of
							  true ->
								  ST;
							  _ ->
								  ets:delete(?ETS_NODE_INFO_TABLE2,Node),
%% 								  lists:foreach(fun(ServerID)->
%% 														ets:delete(?ETS_NODE_INFO_TABLE,ServerID)
%% 												end,ServerList),
								  true
						  end 
				  end,false,ets:tab2list(?ETS_NODE_INFO_TABLE2)),
	if Need ->
		   spawn(fun()->
						 timer:sleep(20000),
						 sync_server_info2()
				 end);
	   true ->
		   ignore
	end,
	{noreply,State};
	

 %{'DOWN',#Ref<0.0.9.72098>,process,{aaa,'aaa@10.10.3.217'},noconnection}
%% 这里没有死循环
%% 收到nodedown之后，会connect_master，但是节点不通，所以会进入try_connect状态
%% try_connect会等待一段时间，再次尝试connect_master
%% 这里相信如果一个节点能被connect到，那么这个节点至少会monitor成功，并且连接不会高频断开
handle_info({nodedown,Node},State) ->
	?ERR("get monitor node down msg...:~w",[Node]),
	case get_master_node() of
		Node ->
%% 			case get(connect_master) of
%% 				X when is_integer(X) ->
%% 					Now = util:now(),
%% 					if Now > X ->
%% 						   connect_master();
%% 					   true ->
%% 						   erlang:send_after(?tryConnectDuration * 1000, self(), ?tryConnect)
%% 					end;
%% 				_ ->
%% 					connect_master()
%% 			end;
		connect_master();
		_ ->
			ignore
	end,
	{noreply,State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



insert_self_info()->
	Node = erlang:node(),
	lists:foreach(fun(ServerID)-> 
						  ets:insert(?ETS_NODE_INFO_TABLE, {ServerID,Node})
				  end,[data_setting:get(server_id)|data_setting:get(merge_server_id_list)]),
	ok.

check_slave()->
	erlang:send_after(?checkMasterConnectionInterval * 1000, self(), check_slave).
connect_master()->
	MasterNode = get_master_node(),
	case net_kernel:connect(MasterNode) of
		true ->
			monitor_master(),
%%			check_master_connection(),
			sync_server_info2();
		Err ->
			erlang:send_after(?tryConnectDuration * 1000, self(), ?tryConnect),
			?ERR("connect to master ~w failed with reason ~w.",[MasterNode,Err])
	end.

monitor_master() ->
	MasterNode = get_master_node(),
	erlang:monitor_node(MasterNode,true).
		

%% check_master_connection() ->
%% 	case lists:member(get_master_node(), erlang:nodes()) of
%% 		true ->
%% 			erlang:send_after(?checkMasterConnectionInterval * 1000, self(), ?checkMasterConnection),
%% 			ok;
%% 		_ ->
%% 			connect_master()
%% 	end.

%% sync_server_info()->
%% 	erlang:send_after(?sync_server_info_tick * 1000, self(), ?sync_server_info),
%% 	sync_server_info2().
sync_server_info2()->
	ServerID = data_setting:get(server_id),
	MasterNode = get_master_node(),
	send_msg_to_node(MasterNode,{update_node_info, [ServerID|data_setting:get(merge_server_id_list)],erlang:node(),true}). %% need Broadcast

get_master_node()->
	list_to_atom(data_setting:get(node_info_master)).

get_node_info(ServerID) ->
	case ets:lookup(?ETS_NODE_INFO_TABLE,ServerID) of
		[{ServerID,Node}] ->
			Node;
		_ ->
			ignore
	end.

send_msg_to_node(ServerID,Msg) when is_integer(ServerID) ->
	case get_node_info(ServerID) of
		ignore ->
			ignore;
		Node ->
			{node_info_server, Node} ! Msg
	end;
send_msg_to_node(Node,Msg) ->
	{node_info_server,Node} ! Msg.


direct_send(PID,ServerID,Msg) when is_integer(ServerID) ->
	case get_node_info(ServerID) of
		ignore ->
			ignore;
		Node ->
			direct_send2(PID,Node,Msg)
	end;
direct_send(PID,Node,Msg) ->
	direct_send2(PID,Node,Msg).

direct_send2(PID,Node,Msg) when is_atom(PID) ->
	{PID,Node} ! Msg;
direct_send2(_,_,_) ->
	ignore.



