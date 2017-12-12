%% @author crimoon-17
%% @doc @todo Add description to send_msg.


-module(send_msg).
-behaviour(gen_server).
%-include("amqp_client.hrl").
-include("common.hrl").
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/0,start_link/0]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([broadcast/2,direct/3,direct_by_name/3]).
-compile(export_all).

-record(state,{connection,producerChannel,consumerChannel,directPublish,fanoutPublish,get}).

-define(BroadCastExchange,<<"pm_f">>).
-define(DirectExchange,<<"pm_d">>).

i()->
    gen_server:call(?MODULE,get_state).

%% broadcast_old(Router,Msg) ->
%%     [#state{producerChannel=Channel,fanoutPublish=Publish}] = ets:lookup(amqp_info_table,state),
%%     amqp_channel:cast(Channel,Publish,#amqp_msg{props = #'P_basic'{delivery_mode = 2}
%%                                                 ,payload= term_to_binary({Router,util:now(),Msg})}).
broadcast(Router,Msg) ->
	Now = util:now(),
	ets:foldl(fun({Node,_},Acc)-> node_info_server:send_msg_to_node(Node,{route,Router,Now,Msg}), Acc end, [], ?ETS_NODE_INFO_TABLE2).
direct(ServerID,Router,Msg) ->
	node_info_server:send_msg_to_node(ServerID,{route,Router,util:now(),Msg}).
%% direct_old(ServerID,Router,Msg) ->
%%     [#state{producerChannel=Channel,directPublish=Publish}] = ets:lookup(amqp_info_table,state),
%%     Key = get_routing_key(ServerID),
%%     amqp_channel:cast(Channel,Publish#'basic.publish'{routing_key=Key},#amqp_msg{props = #'P_basic'{delivery_mode = 2},
%%                                                                                  payload= term_to_binary({Router,util:now(),Msg})}).

direct_by_name(Name,Router,Msg) ->
    case data_svrname_map:get(Name) of
        undefined ->
            undefined;
        ServerID ->
            direct(ServerID,Router,Msg),
            ok
    end.

start() ->
    {ok,_}=
            supervisor:start_child(msg_sup,
                                   {?MODULE,
                                    {?MODULE,start_link,[]},
                                    permanent,600000,worker,[?MODULE]}).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok,State}
    | {ok,State,Timeout}
    | {ok,State,hibernate}
    | {stop,Reason :: term()}
    | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	case data_setting:get(node_info_server) of
		true ->
			ignore;
		_ ->
%% 			case ets:lookup(amqp_info_table,state) of
%% 				[#state{connection = ConnectionOld}] ->
%% 					amqp_connection:close(ConnectionOld);
%% 				_ ->
%% 					ignore
%% 			end,
%% 			{UserName,Password,VirtualHost,Port,Host,HeartBeat} = get_amqp_env(),
%% 			{ok,Connection} = amqp_connection:start(#amqp_params_network{username= UserName
%% 																		 ,password= Password
%% 																		 ,virtual_host= VirtualHost
%% 																		 ,host= Host
%% 																		 ,port=Port
%% 																		 ,heartbeat = HeartBeat}),
%% 			{ok,ProducerChannel} = amqp_connection:open_channel(Connection),
%% 			{ok,ConsumerChannel} = amqp_connection:open_channel(Connection),
%% 			amqp_channel:call(ConsumerChannel,#'basic.qos'{prefetch_count = 1}),
%% 			QueueName = get_queue_name(),
%% 			DirectPublish = #'basic.publish'{exchange= ?DirectExchange,routing_key = QueueName},
%% 			FanoutPublish = #'basic.publish'{exchange= ?BroadCastExchange,routing_key = QueueName},
%% 			Get = #'basic.get'{queue = QueueName,no_ack=true},
%% 			do_base_create(ConsumerChannel),
%% 			
%% 			State = #state{connection=Connection,producerChannel=ProducerChannel,consumerChannel=ConsumerChannel
%% 						   ,directPublish=DirectPublish,fanoutPublish=FanoutPublish,get=Get},
%% 			ets:insert(amqp_info_table,State),
%% 			SubScribe = #'basic.consume'{queue= QueueName,no_ack=false,consumer_tag = get_tag()},
%% 			amqp_channel:subscribe(ConsumerChannel,SubScribe,self()),
%% 			erlang:monitor(process,Connection),
			{ok,#state{}}
	end.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(),From :: {pid(),Tag :: term()},State :: term()) -> Result when
    Result :: {reply,Reply,NewState}
    | {reply,Reply,NewState,Timeout}
    | {reply,Reply,NewState,hibernate}
    | {noreply,NewState}
    | {noreply,NewState,Timeout}
    | {noreply,NewState,hibernate}
    | {stop,Reason,Reply,NewState}
    | {stop,Reason,NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(get_state,_From,State) ->
    {reply,State,State};
handle_call(_Request,_From,State) ->
    Reply = ok,
    {reply,Reply,State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(),State :: term()) -> Result when
    Result :: {noreply,NewState}
    | {noreply,NewState,Timeout}
    | {noreply,NewState,hibernate}
    | {stop,Reason :: term(),NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg,State) ->
    {noreply,State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(),State :: term()) -> Result when
    Result :: {noreply,NewState}
    | {noreply,NewState,Timeout}
    | {noreply,NewState,hibernate}
    | {stop,Reason :: term(),NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
%% handle_info({#'basic.deliver'{consumer_tag = CTag,delivery_tag = DTag},#amqp_msg{payload = Payload}},#state{consumerChannel=Channel}=State) ->
%%     case get_tag() of
%%         CTag ->
%%             amqp_channel:cast(Channel,#'basic.ack'{delivery_tag = DTag}),
%%             do_route_msg(Payload);
%%         _ ->
%%             amqp_channel:call(Channel,#'basic.cancel'{consumer_tag = CTag}),
%%             SubScribe = #'basic.consume'{queue= get_queue_name(),no_ack=false,consumer_tag = get_tag()},
%%             amqp_channel:subscribe(Channel,SubScribe,self())
%%     end,
%%     {noreply,State};
%% 
%% handle_info({'DOWN',_MonitorRef,_Type,_Connection,_Info},State) ->
%%     erlang:send(erlang:self(),{reconnect,1}),
%%     {noreply,State};
%% 
%% handle_info({reconnect,N},State) ->
%%     State2 = try_reconnect(N,State),
%%     {noreply,State2};

handle_info(_Info,State) ->
    {noreply,State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason,State :: term()) -> Any :: term() when
    Reason :: normal
    | shutdown
    | {shutdown,term()}
    | term().
%% ====================================================================
terminate(_Reason,#state{}) ->
%terminate(_Reason,#state{connection=Connection,producerChannel=Channel1,consumerChannel=Channel2}) ->
%%     amqp_channel:close(Channel1),
%%     amqp_channel:close(Channel2),
%%     amqp_connection:close(Connection),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn,State :: term(),Extra :: term()) -> Result when
    Result :: {ok,NewState :: term()} | {error,Reason :: term()},
    OldVsn :: Vsn | {down,Vsn},
    Vsn :: term(). 
%% ====================================================================
code_change(_OldVsn,State,_Extra) ->
    {ok,State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% 断线重连,有两种实现:1,每次尝试失败后,都间隔相同的时间重连;2,每次尝试失败后,把上次的重连时间翻倍
%% 暂定使用第一种方式
%% try_reconnect(N,State) ->
%%     case ets:lookup(amqp_info_table,state) of
%%         [#state{connection = ConnectionOld}] ->
%%             catch amqp_connection:close(ConnectionOld);
%%         _ ->
%%             ignore
%%     end,
%%     {UserName,Password,VirtualHost,Port,Host,HeartBeat} = get_amqp_env(),
%%     Result= amqp_connection:start(#amqp_params_network{username= UserName
%%                                                        ,password= Password
%%                                                        ,virtual_host= VirtualHost
%%                                                        ,host= Host
%%                                                        ,port=Port
%%                                                        ,heartbeat = HeartBeat}),
%%     case Result of
%%         {ok,Connection} -> 
%%             {ok,ProducerChannel} = amqp_connection:open_channel(Connection),
%%             {ok,ConsumerChannel} = amqp_connection:open_channel(Connection),
%%             amqp_channel:call(ConsumerChannel,#'basic.qos'{prefetch_count = 1}),
%%             QueueName = get_queue_name(),
%%             DirectPublish = #'basic.publish'{exchange= ?DirectExchange,routing_key = QueueName},
%%             FanoutPublish = #'basic.publish'{exchange= ?BroadCastExchange,routing_key = QueueName},
%%             Get = #'basic.get'{queue = QueueName,no_ack=true},
%%             do_base_create(ConsumerChannel),
%% 
%%             State2 = #state{connection=Connection,producerChannel=ProducerChannel,consumerChannel=ConsumerChannel
%%                             ,directPublish=DirectPublish,fanoutPublish=FanoutPublish,get=Get},
%%             ets:insert(amqp_info_table,State2),
%%             SubScribe = #'basic.consume'{queue= QueueName,no_ack=false,consumer_tag = get_tag()},
%%             amqp_channel:subscribe(ConsumerChannel,SubScribe,self()),
%%             erlang:monitor(process,Connection),
%%             notice_request_status(),
%%             State2;
%%         _ ->
%%             erlang:send_after(N * 100 ,self(),{reconnect,1}),
%%             State
%%     end.
%% 
%% do_base_create(ConsumerChannel)->
%%     QueueName = get_queue_name(),
%%     DQ = #'queue.declare'{queue = QueueName,durable = true,auto_delete=false},
%%     amqp_channel:call(ConsumerChannel,DQ),
%%     case data_setting:get(server_type) of
%%         normal ->
%%             Bind2 = #'queue.bind'{exchange = ?BroadCastExchange,queue = QueueName},
%%             amqp_channel:call(ConsumerChannel,Bind2);
%%         _ ->
%%             ignore
%%     end,
%%     Bind1 = #'queue.bind'{exchange = ?DirectExchange,queue = QueueName,routing_key = QueueName},
%%     amqp_channel:call(ConsumerChannel,Bind1). 
%% 
%% %% 创建交换机的接口
%% create_exchange() ->
%%     {UserName,Password,VirtualHost,Port,Host,HeartBeat} = get_amqp_env(),
%%     {ok,Connection} = amqp_connection:start(#amqp_params_network{username= UserName
%%                                                                  ,password= Password
%%                                                                  ,virtual_host= VirtualHost
%%                                                                  ,host= Host
%%                                                                  ,port=Port
%%                                                                  ,heartbeat = HeartBeat}),
%%     {_,Channel} = amqp_connection:open_channel(Connection),
%%     BroadCastExchangeDeclare = #'exchange.declare'{exchange= ?BroadCastExchange,type = <<"fanout">>,durable = true,auto_delete=false},
%%     amqp_channel:call(Channel,BroadCastExchangeDeclare),
%%     DirectExchangeDeclare = #'exchange.declare'{exchange= ?DirectExchange,type = <<"direct">>
%%                                                 ,durable = true,auto_delete=false},
%%     amqp_channel:call(Channel,DirectExchangeDeclare).
%% 
%% do_route_msg(Payload) ->
%%     {Router,TimeStamp,MsgInfo} =Msg = binary_to_term(Payload),
%%     %%?ERR("router=~w and ts = ~w and msg = ~w~n",[Router,TimeStamp,Msg]),
%%     case whereis(Router) of
%%         undefined ->
%%             ?ERR("error get msg:~w",[Msg]);
%%         _ ->
%%             do_route_msg(Router,TimeStamp,MsgInfo)
%%     end.
%% 
%% do_route_msg(mail_server,_,MsgInfo) ->
%%     erlang:send(mail_server,MsgInfo);
%% do_route_msg(Router,TimeStamp,MsgInfo) ->
%%     case util:now() > TimeStamp + 4 of
%%         true ->
%%             ?ERR("Msg timeout :~w,~w,~w",[Router,TimeStamp,MsgInfo]);
%%         _ ->
%%             erlang:send(Router,MsgInfo)
%%     end.
%% 
%% get_queue_name()->
%%     list_to_binary(atom_to_list(data_setting:get(platform))++"_"++integer_to_list(data_setting:get(server_id))).
%% 
%% get_routing_key(ServerIDT) ->
%%     ServerID = 
%%                case data_serverID_map:get(ServerIDT) of
%%                     Any when is_number(Any) ->
%%                         Any;
%%                     _ ->
%%                         ServerIDT
%%                 end,
%%     list_to_binary(atom_to_list(data_setting:get(platform))++"_"++integer_to_list(ServerID)).
%% 
%% get_tag() ->
%%     list_to_binary("tag_"++integer_to_list(data_setting:get(server_id))).
%% 
%% get_amqp_env()->
%%     List = data_setting:get(msg_env),
%%     UserName = proplists:get_value(username,List),
%%     Password = proplists:get_value(password,List),
%%     VirtualHost = proplists:get_value(virtualhost,List),
%%     Port = proplists:get_value(port,List),
%%     Host = proplists:get_value(host,List),
%%     HeartBeat = proplists:get_value(heartbeat,List),
%%     {UserName,Password,VirtualHost,Port,Host,HeartBeat}.

%% 断线重连后通知相应模块处理
notice_request_status()->
    lists:foreach(fun(Module) -> 
                case erlang:whereis(Module) of
                    undefined ->
                        ignore;
                    Pid ->
                        erlang:send(Pid, notice_request_status)
                end
        end, [family_fight_master_server, alien_master_server, alien_finals, alien_distribute]),
    ok.
