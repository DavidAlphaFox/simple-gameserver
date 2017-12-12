-module(galactica_match).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-compile(export_all).
-include("common.hrl").
-include("def_carlos.hrl").
-define(SUB_PROCESS_TIMEOUT, 3000).
-define(SUB_PROCESS_RESTART_INTERVAL, 5000). 
-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(pd_balance_optimize_list, pd_balance_optimize_list). %% 等待优化等级平衡的队伍
-define(pd_balance_optimize_timestamp, pd_balance_optimize_timestamp). %% 保存优化等级平衡的timer时间戳

-record(counter, {l1=0,l2=0,l3=0,l4=0,l5=0}).
-record(state, {ets=undefined, process=[], warid=0}).
-record(loop_info, {ets=0, level=0, trans=false, time=0, lock=false}).

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
    supervisor:start_child(galactica_sup,
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
	case data_setting:get(server_type) of
			carlos_match ->
			process_flag(trap_exit,true),
			Ets = ets:new(ets_galactica_match, []),
			ets:insert(Ets, {{total_counter, 1}, 0}),
			ets:insert(Ets, {{total_counter, 2}, 0}),
			ets:insert(Ets, {counter, {#counter{},#counter{}}}),
			lists:foreach(fun(E) ->
								  ets:insert(Ets, {{request_queue, E}, {queue:new(), queue:new()}})
						  end, lists:seq(1, 5)),
			
			%% 目前state的数据没有存盘的必要,但先预留好位置
			case db_sql:get_etc(?DB_ETC_KEY_GALACTICA) of
				[{state, #state{warid=WarID}}, {ets, EtsInfo}, Info] ->
					lists:foreach(fun(KV) -> ets:insert(Ets, KV) end, EtsInfo),
					lists:foreach(fun({K, V}) -> put(K, V) end, Info);
				_ ->
					WarID = 0
			end,
			erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
            LoopInfo1 = #loop_info{ets=Ets, level=1},
            LoopInfo2 = #loop_info{ets=Ets, level=2, trans = true},
            put(?pd_balance_optimize_list,[]),
            put(?pd_balance_optimize_timestamp,0),
			{ok, #state{ets=Ets, process={start_loop(LoopInfo1), start_loop(LoopInfo2)}, warid=WarID}};
		_->
			?ERR("not galactica match server"),
			ignore
	end.

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
handle_call({get_pd,PdName}, _From, State) ->
    Reply = get(PdName),
    {reply, Reply, State};
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
%%% 匹配成功
do_handle_info({match, Team1, Team2, Counter, Level}, #state{ets=Ets, warid=WarID} = State) ->
    %% 目前除了这个消息外,这个gen_server只处理下面3个消息,预测以分配请求为主,
    %% 重启分配程序几乎不会发生,unrequest相对于request频率小很多.而request不涉及
    %% 大量运算,所以除非请求数据量很大,否则是不会出现消息堆积的情况,因此分配的处理
    %% 能放在这儿来做.其次,分配子程序和gen_server是并行的,如果在子程序中分配,则有
    %% 可能出现分配成功,但实际unrequest导致无法分配的情况,而在gen_server里面,消息
    %% 是串行的,不存在这个问题
    [{_, CounterSeq}] = ets:lookup(Ets, counter),
    EtsCounter = ?get_lvl_info(CounterSeq, Level),
    case check_match(EtsCounter, Counter) of
        false ->
            {noreply, State};
        _ ->
            AllMatchList = do_match(Ets, Team1, Team2, Level), 
            ?INFO("match AllMatchList:~w",[AllMatchList]),
            NewBalanceOptList = [{AllMatchList,Team1,Team2}|get(?pd_balance_optimize_list)],%% 取得实际玩家阵容等级数据，AllMatchList是[{1,X1},{2,X2},{3,X3},{4,X4},{5,X5}],Xn是报名的实际数据[#request{}]
            BOLength = erlang:length(NewBalanceOptList),
            MatchBalanceNum = data_galactica:get(match_balance_num),
            BOTimestamp = get(?pd_balance_optimize_timestamp),
            if
                BOLength >= MatchBalanceNum ->  % 已经达到优化需要的队伍数量
                    NewWarID = bo_get_match_result(NewBalanceOptList,Ets,WarID),
                    put(?pd_balance_optimize_list,[]),
                    put(?pd_balance_optimize_timestamp,0);
                BOLength < MatchBalanceNum ->  % 没有达到优化需要的队伍数量
                    if
                        BOTimestamp =:= 0 -> % 没有启动计时器的话，启动一下
                            erlang:send_after(data_galactica:get(match_balance_time)*1000, self(), bo_tick);
                        true ->
                            ignore
                    end,
                    put(?pd_balance_optimize_list,NewBalanceOptList),
                    NewBOTimestamp = util:now() + data_galactica:get(match_balance_time),
                    put(?pd_balance_optimize_timestamp,NewBOTimestamp),
                    NewWarID = WarID
            end,
            {noreply, State#state{warid=NewWarID}} 
    end;
do_handle_info(bo_tick, #state{ets=Ets, warid=WarID} = State) ->
    Now = util:now(),
    BOTimestamp = get(?pd_balance_optimize_timestamp),
    if
        Now < BOTimestamp ->
            erlang:send_after((BOTimestamp - Now)*1000, self(), bo_tick),
            NewWarID = WarID;
        true -> %% 时间已经到了，不等了，依照现有队列分配对手。注意BOTimestamp为0时，也是这个逻辑分支
            ?INFO("bo_tick timeout pd_balance_optimize_list:~w",[erlang:length(get(?pd_balance_optimize_list))]),
            NewWarID = bo_get_match_result(get(?pd_balance_optimize_list),Ets,WarID),
            put(?pd_balance_optimize_list,[]),
            put(?pd_balance_optimize_timestamp,0)
    end,    
    {noreply, State#state{warid=NewWarID}};
do_handle_info({match_want, Team1, Team2, Counter, Level, {_, RoomID, WantList}}, #state{ets=Ets} = State) ->
    [{_, CounterSeq}] = ets:lookup(Ets, counter),
    EtsCounter = ?get_lvl_info(CounterSeq, Level),
    case check_match(EtsCounter, Counter) of
        false ->
            match_room_manager:send_to_room(RoomID, {get, fail});
        _ ->
            #match{team1=Members1, team2=Members2} = do_match(Ets, Team1, Team2, Level), 
            match_room_manager:send_to_room(RoomID, {get, WantList, [Members1, Members2]})
    end,
    {noreply, State};

%% 分配请求 
do_handle_info({request, #request{serverID=ServerID,id=ID,level=Lvl,members=Members}=Request}, #state{ets=Ets, process=Process, warid=WarID} = State) ->
    ?INFO("request ~w",[{ServerID,ID,?team_len(ID),[M#member_base_info.level||M<-Members]}]),
    case node_info_server:get_node_info(ServerID) of
        ignore ->
            NewWarID = WarID,
            Result = 8;
        Node ->
            case net_adm:ping(Node) of
                 pang ->
                    NewWarID = WarID,
                    Result = 8;
                _ ->
                    Len = ?team_len(ID),
                    MemberLen = erlang:length(Members),
                    AllLen = 2 * data_galactica:get(match_need),
                    case get_sign(ID) of
                        _ when AllLen =:= MemberLen andalso Len =:= 1 ->
                            ?INFO("---ai--- request ~w",[Request]),
                            send_msg:direct(ServerID, galactica_server, {match_success, ID}),
                            {Members1,Members2} = lists:split(data_galactica:get(match_need), Members),
                            Msg = {new_war, WarID, Members1, Members2},
                            galactica_war_manager_server:send_to_me(Msg),
                            NewWarID = WarID + 1,
                            Result = 0;
                        ?undefined ->
                            Level = ?get_level(Lvl),
                            [{_, RequestSeq}] = ets:lookup(Ets, {request_queue, Len}),
                            RequestQ = ?get_lvl_info(RequestSeq, Level),
                            NewRequestQ = queue:in(Request, RequestQ),
                            NewRequestSeq = ?set_lvl_info(RequestSeq,Level,NewRequestQ),
                            ets:insert(Ets, {{request_queue, Len}, NewRequestSeq}),
                            
                            [{_, CounterSeq}] = ets:lookup(Ets, counter),
                            Counter = ?get_lvl_info(CounterSeq, Level),
                            NewCounter = ?incf_counter(Counter,Len), 
                            ets:insert(Ets, {counter, ?set_lvl_info(CounterSeq,Level,NewCounter)}), 
                
                            ets:update_counter(Ets, {total_counter, Level}, Len),
                            set_sign(ID, #match_sign_info{data=Level}),
                            NewWarID = WarID,
                            Result = 0,
                            %% 触发匹配事件
                            Pid = element(Level, Process),
                            erlang:send(Pid, request_event);
                        _ ->
                            NewWarID = WarID,
                            Result = 4  %% 重复报名
                    end 
            end
    end,
    %% 给客户端的请求应答
    send_msg:direct(ServerID, galactica_server, {sign_result, ID, Result}),
    {noreply, State#state{warid=NewWarID}};

%% 重新分配请求 
do_handle_info({re_request, #request{id=ID,level=Lvl}=Request}, #state{ets=Ets, process=Process} = State) ->
    %%?ERR("re_request Request:~p~n", [Request]),
    case get_sign(ID) of
        %% 已经取消匹配的不处理
        ?undefined ->
            ignore;
        #match_sign_info{is_in_room=IsInRoom} ->
            case IsInRoom of
                false ->
                    %% 不在房间的不处理
                    ignore;
                _ ->
                    update_sign(ID, {#match_sign_info.is_in_room, false}),
                    Len = ?team_len(ID),
                    Level = ?get_level(Lvl),
                    [{_, RequestSeq}] = ets:lookup(Ets, {request_queue, Len}),
                    RequestQ = ?get_lvl_info(RequestSeq, Level),
                    NewRequestQ = queue:in(Request, RequestQ),
                    NewRequestSeq = ?set_lvl_info(RequestSeq,Level,NewRequestQ),
                    ets:insert(Ets, {{request_queue, Len}, NewRequestSeq}),
                    
                    [{_, CounterSeq}] = ets:lookup(Ets, counter),
                    Counter = ?get_lvl_info(CounterSeq, Level),
                    NewCounter = ?incf_counter(Counter,Len), 
                    ets:insert(Ets, {counter, ?set_lvl_info(CounterSeq,Level,NewCounter)}), 
        
                    ets:update_counter(Ets, {total_counter, Level}, Len),
                    set_sign(ID, #match_sign_info{data=Level}),
                    %%?ERR("re_request, ets:~p ~n", [ets:tab2list(Ets)]),
        
                    %% 触发匹配事件
                    Pid = element(Level, Process),
                    erlang:send(Pid, request_event) 
            end
    end,
    {noreply, State};

%% 清除退出标记
do_handle_info({delete_sign, TeamIDs}, State) ->
    lists:foreach(fun(ID) -> delete_sign(ID) end, TeamIDs),
    {noreply, State};

%% 取消匹配
do_handle_info({unrequest, ServerID, ID} = Unrequest, #state{ets=Ets} = State) ->
    case get_sign(ID) of
        ?undefined ->
            ignore;
        #match_sign_info{is_in_room=IsInRoom, data=Level} = Sign ->
            case IsInRoom of
                false ->
                    case check_balance_optimize_list(ID) of
                        true ->
                            ignore;
                        false ->
                            Len = ?team_len(ID),
                            [{_, RequestSeq}] = ets:lookup(Ets, {request_queue, Len}),
                            {R,F} = ?get_lvl_info(RequestSeq, Level),
                
                            %% 一定要有实际删除,才修改计数器
                            {Result, R2, F2} = 
                                case lists:keytake(ID, #request.id, F) of
                                    false ->
                                        case lists:keytake(ID, #request.id, R) of
                                            false ->
                                                ?ERR("unrequest时异常~nunrequest:~p~nsign:~p~n:", [Unrequest, Sign]),
                                                {false, R, F};
                                            {value, _, Rest} ->
                                                {true, Rest, F}
                                        end;
                                    {value, _, Rest} ->
                                        {true, R, Rest}
                                end,
                        
                            %% 更新数据
                            case Result of
                                false ->
                                    ignore;
                                _ ->
                                    RequestQ2 = {R2, F2},
                                    NewRequestSeq = ?set_lvl_info(RequestSeq, Level, RequestQ2),
                                    ets:insert(Ets, {{request_queue, Len}, NewRequestSeq}),
                                
                                    %% 更新数据
                                    [{_, CounterSeq}] = ets:lookup(Ets, counter),
                                    Counter = ?get_lvl_info(CounterSeq, Level),
                                    NewCounter = ?decf_counter(Counter, Len + 1, 1),
                                    ets:insert(Ets, {counter, ?set_lvl_info(CounterSeq, Level, NewCounter)}),
                                    ets:update_counter(Ets, {total_counter, Level}, -Len)  
                            end
                    end;
                _ ->
                    %% 通知房间
                    match_room_manager:send_unrequest(ID, ?room_type_galactica)  
            end,
            delete_sign(ID) 
        end,
    send_msg:direct(ServerID, galactica_server, {reply_unrequest, ID}),
    {noreply, State};

%% 在房间中被踢走
do_handle_info({kick_room_teams, TeamList}, State) ->
    lists:foreach(fun({ServerID, ID}) ->
                    delete_sign(ID),
                    send_msg:direct(ServerID, galactica_server, {reply_unrequest, ID}) 
                 end, TeamList),
    {noreply, State};

%% 数据转移.这里涉及到了3个进程:galactica_match, src、dst,在转移时src已经被锁了,所以不会触发匹配,
%% 因此也不会通知galactica_match进行分配.由于数据的写操作都是在galactica_match里面进行的,而trans_info和
%% match两个不会并行,因此加锁其实意义不大,这里为了保险起见加了锁.
%% 这个操作就是把src处理的数据,转移到dst去,虽然此时dst没有加锁,但mathc和trans_info不会并行,应该这里
%% 的数据转移是安全的
do_handle_info({trans_info, Level}, #state{ets=Ets,process=Process} = State) ->
    trans_info(Level, Level - 1, Ets),
    Pid = erlang:element(Level, Process), 
    erlang:send(Pid, unlock),
    {noreply, State};

%% room的申请
do_handle_info({want, RoomID, WantList}, #state{process=Process}=State) ->
    %% TODO 低段的人多,先默认去低端找吧,之后在看看要不要调调
    Pid = element(1, Process),
    erlang:send(Pid, {want, RoomID, WantList}),
    {noreply, State};

%% 子程序重启
do_handle_info({restart_sub_process, Pid, Ets}, #state{ets=Ets,process=Process}=State) ->
    Level = find_process_level(Process, Pid, 1),
    NewPid =
        case erlang:is_process_alive(Pid) of
            false ->
                start_loop(#loop_info{ets=Ets, level=Level, trans=Level > 1});
            _ ->
                Pid
        end,
    erlang:send(NewPid, request_queue),
    NewProcess = setelement(Level, Process, NewPid),
    {noreply, State#state{process=NewProcess}};

%% 处理程序退出
do_handle_info({'EXIT', From, Reason}, #state{ets=Ets} = State) ->
    case Reason of
        shutdown ->
            ignore;
        normal ->
            ignore;
        _ ->
            erlang:send_after(?SUB_PROCESS_RESTART_INTERVAL, ?MODULE, {restart_sub_process, From, Ets}) 
    end,
    {noreply, State};

%% 存盘
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    {noreply, State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
    {noreply, State}.

%% 开启匹配循环
start_loop(LoopInfo) ->
    spawn_link(fun() -> start_process_loop(LoopInfo) end).

%% 判断该分配方案是否还有效
check_match(EtsCounter, Counter) ->
    lists:all(fun(E) -> 
                element(E + 1, Counter) =< element(E + 1, EtsCounter)
            end, lists:seq(1, 5)).

check_balance_optimize_list(CancelID)->
    OldBoList = get(?pd_balance_optimize_list),
    {NewBoList,CancelList} =
        lists:foldl(fun({[{1,X1},{2,X2},{3,X3},{4,X4},{5,X5}],_T1,_T2} = Bo,{AccNew,[]})-> %% 只会找到一个带有取消者id的Bo
                            AllList = X1++X2++X3++X4++X5,
                            case lists:keytake(CancelID, #request.id, AllList) of
                                false ->
                                    {[Bo|AccNew],[]};
                                {value,CancelReq,OtherReqList} ->
                                    {AccNew,OtherReqList}
                            end;
                       (Bo,{AccNew,AccCancelList}) ->
                            {[Bo|AccNew],AccCancelList}
            end, {[],[]}, OldBoList),
    ?INFO("check_balance_optimize_list. new length:~w --cancel--~w",[erlang:length(NewBoList),CancelList]),
    if
        CancelList =:= [] ->    %% 取消者没有在优化队列中
            false;
        true ->
            put(?pd_balance_optimize_list, NewBoList),
            if
                NewBoList =:= [] ->
                    put(?pd_balance_optimize_timestamp,0);
                true ->
                    ignore
            end,    
            lists:foreach(fun(Request)->
                                update_sign(Request#request.id, {#match_sign_info.is_in_room, balance_optimize}),  %% 标志位不为false才能够正常重新报名
                                erlang:send(self(), {re_request, Request}) 
                          end, CancelList),
            true
    end.

%% 进行分配
do_match(Ets, Team1, Team2, Level) ->
    Match = do_match(Team1, Ets, Level, #match{}, #match.team1, 5),
    Match2 = do_match(Team2, Ets, Level, Match, #match.team2, 5),
    #match{team1=Members1, team2=Members2} = Match2,
    lists:foldl(fun({X,R},[{1,Y1},{2,Y2},{3,Y3},{4,Y4},{5,Y5}])-> 
                    case X of
                        1 -> [{1,[R|Y1]},{2,Y2},{3,Y3},{4,Y4},{5,Y5}];
                        2 -> [{1,Y1},{2,[R|Y2]},{3,Y3},{4,Y4},{5,Y5}];
                        3 -> [{1,Y1},{2,Y2},{3,[R|Y3]},{4,Y4},{5,Y5}];
                        4 -> [{1,Y1},{2,Y2},{3,Y3},{4,[R|Y4]},{5,Y5}];
                        5 -> [{1,Y1},{2,Y2},{3,Y3},{4,Y4},{5,[R|Y5]}]
                    end
        end, [{1,[]},{2,[]},{3,[]},{4,[]},{5,[]}], Members1 ++ Members2).

do_match(_, _, _, Match, _, 0) ->
    Match;
do_match(Counter, Ets, Level, Match, Pos, X) ->
    [{_, RequestSeq}] = ets:lookup(Ets, {request_queue, X}),
    RequestQ = ?get_lvl_info(RequestSeq, Level),
    CounterPos = X + 1,
    Need = element(CounterPos, Counter),
    case Need of
        0 ->
            do_match(Counter, Ets, Level, Match, Pos, X - 1);
        _ ->
            CurList = erlang:element(Pos, Match),
            {NewList, RequestQ2} = 
                lists:foldl(fun(_, {NewAcc, RequestQAcc}) ->
                                {{value, New}, RequestQAcc2} = queue:out(RequestQAcc),
                                {[{X,New}|NewAcc], RequestQAcc2}
                                end, {CurList, RequestQ}, lists:seq(1, Need)),

            %% 更新队列和计数器
            NewRequestSeq = ?set_lvl_info(RequestSeq, Level, RequestQ2),
            ets:insert(Ets, {{request_queue, X}, NewRequestSeq}),

            [{_,CounterSeq}] = ets:lookup(Ets, counter),
            EtsCounter = ?get_lvl_info(CounterSeq, Level),
            NewCounter = ?decf_counter(EtsCounter, CounterPos, Need),
            ets:insert(Ets, {counter, ?set_lvl_info(CounterSeq, Level, NewCounter)}),

            ets:update_counter(Ets, {total_counter, Level}, -X * Need),

            %% 更新分组数据
            NewMatch = setelement(Pos, Match, NewList),
            do_match(Counter, Ets, Level, NewMatch, Pos, X - 1)
    end.

%% BOList = {AllMatchList,Team1,Team2}
bo_get_match_result([],_Ets,WarID) ->
    WarID;
bo_get_match_result(BOList,Ets,WarID) ->
    ?INFO("bo_get_match_result>>>>>>>>>>>~w",[[BOList,Ets,WarID]]),
    AllMatchList = lists:foldl(fun({[{1,X1},{2,X2},{3,X3},{4,X4},{5,X5}],_T1,_T2},[{1,Y1},{2,Y2},{3,Y3},{4,Y4},{5,Y5}])-> 
                                       [{1,attr_sort(X1++Y1)}
                                       ,{2,attr_sort(X2++Y2)}
                                       ,{3,attr_sort(X3++Y3)}
                                       ,{4,attr_sort(X4++Y4)}
                                       ,{5,attr_sort(X5++Y5)}]
                               end, [{1,[]},{2,[]},{3,[]},{4,[]},{5,[]}], BOList),
    TList = [{T1,T2}||{_A,T1,T2}<-BOList],
    bo_get_match_result2(TList,AllMatchList,Ets,WarID).

bo_get_match_result2([],AllMatchList,_Ets,WarID) ->
    ?INFO("bo_get_match_result2 End AllMatchList:~w",[erlang:length(AllMatchList)]),
    WarID;
bo_get_match_result2([{Team1,Team2}|Other],AllMatchList,Ets,WarID) ->
    {#match{team1=Members1, team2=Members2},NewAllMatchList} = get_match_result(AllMatchList,Team1,Team2),        %% 根据需求分配为两个队伍
    case data_galactica:get(need_in_room) of
        false ->
            Msg = {new_war, WarID, parse_request_team2(Members1), parse_request_team2(Members2)},       
            galactica_war_manager_server:send_to_me(Msg);  
        _ ->
            Need = data_galactica:get(match_need),
            mark_in_room([Members1, Members2]),
            match_room_manager:create_room(?room_type_galactica, {Need, Need}, [Members1, Members2],
                                            fun(From, [M1, M2]) ->
                                                    send_delete_sign(From, [M1, M2]),
                                                    Msg = {new_war, WarID, parse_request_team(M1), parse_request_team(M2)},       
                                                    galactica_war_manager_server:send_to_me(Msg)  
                                            end)
    end,
    ?INFO("bo_get_match_result2 Next AllMatchList:~w,~w",[erlang:length(Other),erlang:length(NewAllMatchList)]),
    bo_get_match_result2(Other,NewAllMatchList,Ets,WarID+1).


attr_sort(List) -> 
    lists:sort(fun(#request{members=MembersA},#request{members=MembersB})->
                    TotalA = lists:sum([M#member_base_info.level||M<-MembersA]),
                    TotalB = lists:sum([M#member_base_info.level||M<-MembersB]),
                    TotalA > TotalB 
        end, List).

%% OtherMList 会把没用完的AllMatchList收集起来，下个递归使用
get_match_result(AllMatchList,Team1,Team2) ->
    {T1,T2,FinalLvl1,FinalLvl2,OtherMList} = 
        lists:foldl(fun(X,{Match1,Match2,Level1,Level2,AccMList}) -> 
                            get_match_res(X,Match1,Match2,Team1,Team2,Level1,Level2,AccMList)
                    end, {[],[],0,0,[]}, AllMatchList),
    ?INFO("get_match_result ~w--~w Other:~w",[FinalLvl1,FinalLvl2,erlang:length(OtherMList)]),
    {#match{team1=T1,team2=T2},lists:sort(OtherMList)}.    %% 确保OtherMList是1~5排序，OtherMList是剩下的报名者数据

%% XMatchList = [#request{}]
get_match_res({X,XMatchList},Match1,Match2,Team1,Team2,Level1,Level2,AccMList) ->
    M1 = element(X+1,Team1),
    M2 = element(X+1,Team2),
    {M1L,M2L,NewLevel1,NewLevel2,OtherXMatchList} = get_match_res2(M1,M2,[],[],Level1,Level2,XMatchList),
    NewAccMList = [{X,OtherXMatchList}|AccMList],
    {M1L++Match1,M2L++Match2,NewLevel1,NewLevel2,NewAccMList}.
    
get_match_res2(0,0,M1LA,M2LA,Lvl1,Lvl2,XMatchList)->
    {M1LA,M2LA,Lvl1,Lvl2,XMatchList};
get_match_res2(M1A,M2A,M1LA,M2LA,Lvl1,Lvl2,[Mat|OtherXMatchList])->
    RI =  random:uniform(10),
    TotalLvl = lists:sum([M#member_base_info.level||M<-Mat#request.members]),
    if Lvl1 > Lvl2 andalso M2A >= 1 -> get_match_res2(M1A,M2A-1, M1LA,[Mat|M2LA],Lvl1,Lvl2+TotalLvl,OtherXMatchList);
       Lvl1 < Lvl2  andalso M1A >= 1 -> get_match_res2(M1A-1, M2A,[Mat|M1LA],M2LA,Lvl1+TotalLvl,Lvl2,OtherXMatchList);
       M1A == 0 andalso M2A >= 1 -> get_match_res2(M1A,M2A-1, M1LA,[Mat|M2LA],Lvl1,Lvl2+TotalLvl,OtherXMatchList);
       M2A == 0 andalso M1A >= 1 -> get_match_res2(M1A-1, M2A,[Mat|M1LA],M2LA,Lvl1+TotalLvl,Lvl2,OtherXMatchList);
       RI >= 6 andalso M2A >= 1 -> get_match_res2(M1A,M2A-1, M1LA,[Mat|M2LA],Lvl1,Lvl2+TotalLvl,OtherXMatchList);
       M1A >= 1 -> get_match_res2(M1A-1, M2A,[Mat|M1LA],M2LA,Lvl1+TotalLvl,Lvl2,OtherXMatchList);
       true -> ?ERR("get_match_res2 ERROR ~n~w~n",[[M1A,M2A,M1LA,M2LA,Lvl1,Lvl2,[Mat|OtherXMatchList]]]),
               {M1LA,M2LA,Lvl1,Lvl2,[Mat|OtherXMatchList]}
    end.

%% 存盘
do_persist(#state{ets=Ets}=State) ->
    SInfo = {state, State},
    EInfo = {ets, ets:tab2list(Ets)},
    db_sql:set_etc(?DB_ETC_KEY_GALACTICA, [SInfo, EInfo, get()]).

%%-------------------------------------------------------------------------------- 
%% 专门的分配进程
start_process_loop(LoopInfo) ->
    init_loop(),
    loop(LoopInfo).

init_loop() ->
    %% 1放在前面, 可以减少失败匹配的次数
    put(5, [[1,4],[3,2]]),
    put(4, [[1,3], [2,2]]),
    put(3, [[1,2]]),
    put(2, [[1,1]]).

%% 等待数据转移完成
loop(#loop_info{lock=true} = LoopInfo) ->
    receive 
        unlock ->
            loop(LoopInfo#loop_info{lock=false})
    end;

%% 进行一次匹配
loop(LoopInfo) ->
    #loop_info{time=Time1,trans=Trans,level=Level} = LoopInfo2 = 
        receive 
            request_event ->
                request_event(LoopInfo);
            {want, _RoomID, _WantList} = WantData ->
                want_event(LoopInfo, WantData)
            %% 在空闲时间匹配匹配看看(正常应该是匹配不到的,但合理利用CPU,防止出现异常的长时间匹配)
            after ?SUB_PROCESS_TIMEOUT ->
                request_event(LoopInfo) 
        end,

    %% 如果在max_wait_sec内都没有成功匹配,则把数据转移到上一个进程去
    TimeDiff = util:now() - Time1,
    LoopInfo3 = 
        case Trans andalso TimeDiff >= ?max_wait_sec of
            true ->
                erlang:send(?MODULE, {trans_info, Level}),
                LoopInfo2#loop_info{time=util:now(),lock=true};
            _ ->
                LoopInfo2
        end,
    loop(LoopInfo3).

request_event(#loop_info{ets=Ets, level=Level} = LoopInfo) ->
    [{_,Total}] = ets:lookup(Ets, {total_counter, Level}),
    case Total < 2 * data_galactica:get(match_need) of
        true ->
            %% 没有人时,就没必要进行trans
            case Total =:= 0 of
                true ->
                    LoopInfo#loop_info{time=util:now()};
                _ ->
                    LoopInfo 
            end;
        _ ->
            [{_,CounterSeq}] = ets:lookup(Ets, counter),
            Counter = ?get_lvl_info(CounterSeq, Level),
            case dfs_try_match(Counter) of
                undefined ->
                    LoopInfo;
                {Team1, Team2} ->
                    %% 将匹配好的分组发给gen_server
                    erlang:send(?MODULE, {match, statistic([Team1]), statistic([Team2]), statistic([Team1, Team2]),Level}), 
                    LoopInfo#loop_info{time=util:now()}
            end
    end.

%% 处理room的申请
want_event(#loop_info{ets=Ets, level=Level}=LoopInfo, {want, RoomID, WantList} = WantData) ->
    [{_,CounterSeq}] = ets:lookup(Ets, counter),
    Counter = ?get_lvl_info(CounterSeq, Level),
    case dfs_try_match(Counter, WantList) of
        undefined ->
            %%因为匹配失败了,并不涉及到数据分配,所以这儿可以直接给room发消息,不用先转发给主进程
            match_room_manager:send_to_room(RoomID, {get, fail});
        {Team1, Team2} ->
            %% 将匹配好的分组发给gen_server
            erlang:send(?MODULE, {match_want, statistic([Team1]), statistic([Team2]), statistic([Team1, Team2]), Level, WantData})  
    end,
    LoopInfo.

%% 尝试匹配两个组,要求优先匹配队伍,所以这儿用bfs更符合些,
%% 但这儿的bfs实现要复杂些,且一般情况下,两个的结果不会有没太大区别
%% 注意Counter不是常量,也是acc来的,需要进行更新
dfs_try_match(Counter) ->
    NeedList = data_galactica:get(match_need_list),
    case dfs_try_match(Counter, NeedList) of
        [] ->
            undefined;
        {Counter2, Team1} ->
            case dfs_try_match(Counter2, NeedList) of
                [] ->
                    undefined;
                {_, Team2} ->
                    {Team1, Team2}
            end
   end.

%% 注意:dfs_try_match和dfs_try_match_list相互调用,所以两个
%% 的返回值是相互影响的,1是要注意保证返回类型和数量的一致性,
%% 2是要保证能正确的退出递归调用
dfs_try_match(Counter, List) ->
    dfs_try_match(Counter, List, []).

%% 尝试找到某个数的所有组合
dfs_try_match(_, [], _) ->
    [];             %% 返回空,表示没有找到可行的分配方式
dfs_try_match(Counter, [H|T], Acc) ->
    case dfs_try_match_list(Counter,H,Acc) of
        [] ->
            %% 回溯
            dfs_try_match(Counter, T, Acc);
        Info -> %{Counter2, Acc2} 
            Info
    end.

%% 尝试对某个组合进行匹配
dfs_try_match_list(Counter, [], Acc) ->
    {Counter, Acc};
dfs_try_match_list(Counter, [H|T], Acc) ->
    Count = element(H + 1, Counter),
    %% 需要的数数量不足
    case Count < 1 of
        true ->
            %% 找到合成这个数的所有组合
            case get(H) of
                %% 1不能再被分解,如果1不足,则该组合无法匹配成功
                undefined ->
                    [];
                List -> 
                    %% 尝试匹配到能够合成这个数的组合
                    case dfs_try_match(Counter, List, []) of
                        [] ->
                            [];
                        {Counter2, Acc2} ->
                            dfs_try_match_list(Counter2, T, Acc2 ++ Acc)
                    end
            end;
        _ ->
            %% 数量足够则分配
            Counter2 = setelement(H + 1, Counter, Count - 1),
            dfs_try_match_list(Counter2, T, [H|Acc])
    end.

%% 统计匹配上的数据分布
statistic(List) ->
    lists:foldl(fun(SubList, Acc) ->
                    lists:foldl(fun(E, Acc2) -> 
                                    Pos = E + 1, 
                                    setelement(Pos, Acc2, 1 + element(Pos, Acc2)) 
                                end, 
                            Acc, SubList)
                end, #counter{}, List).

%% 给匹配服发消息(跨服发消息用的接口)
send_to_me(Msg) ->
    send_msg:direct_by_name(galactica_match, galactica_match, Msg).

%% 将request list转换为member_base_info list
parse_request_team(Team) ->
    lists:foldl(fun(#request{serverID=ServerID, id=ID, members=Info}, Acc) -> 
                        send_msg:direct(ServerID, galactica_server, {match_success, ID}),
                        Info ++ Acc 
                    end, [], Team).

parse_request_team2(Team) ->
    lists:foldl(fun(#request{serverID=ServerID, id=ID, members=Info}, Acc) -> 
                        delete_sign(ID),
                        send_msg:direct(ServerID, galactica_server, {match_success, ID}),
                        Info ++ Acc 
                    end, [], Team).

send_delete_sign(From, Teams) ->
    TeamIDs = 
        lists:foldl(fun(Team, Acc) ->
                        lists:foldl(fun(#request{id=ID}, Acc2) ->
                                        [ID|Acc2]
                                    end, Acc, Team)
                    end, [], Teams),
    erlang:send(From, {delete_sign, TeamIDs}).

mark_in_room(Teams) ->
    lists:foreach(fun(Team) ->
                    lists:foreach(fun(#request{id=ID}) ->
                                    update_sign(ID, {#match_sign_info.is_in_room, true})
                                end, Team)
                end, Teams).

find_process_level(_, _, ?processSeqNum + 1) ->
    undefined;
find_process_level(ProcessSeq, Pid, N) -> 
    PidT = element(N, ProcessSeq),
    case PidT =:= Pid of
        true ->
            N;
        _ ->
            find_process_level(ProcessSeq, Pid, N + 1)
    end.

trans_info(Level, Prev, Ets) ->
    lists:foreach(fun(E) ->
                    %% 把后一个层级的队列数据放入上一个层级去
                    [{_, RequestSeq}] = ets:lookup(Ets, {request_queue, E}),
                    {R, F} =  ToDel = ?get_lvl_info(RequestSeq, Level),

                    %% 这里记得更新ID所在的层级
                    lists:foreach(fun(#request{id=ID}) -> update_sign(ID, {#match_sign_info.data, Prev}) end, R ++ F),
                    ToIn = ?get_lvl_info(RequestSeq, Prev),
                    NewRequestSeq = ?set_lvl_info(RequestSeq, Level, queue:new()),
                    NewRequestSeq2 = ?set_lvl_info(NewRequestSeq, Prev, queue:join(ToIn, ToDel)),
                    ets:insert(Ets, {{request_queue, E}, NewRequestSeq2}) 
                end, lists:seq(1, 5)),
        
    %% 更新计数器
    [{_, CounterSeq}] = ets:lookup(Ets, counter),
    ToDelc = ?get_lvl_info(CounterSeq, Level),
    ToInc = ?get_lvl_info(CounterSeq, Prev),
    NewCounterSeq = ?set_lvl_info(CounterSeq, Level, #counter{}),
    NewCounterSeq2 = ?set_lvl_info(NewCounterSeq, Prev, counter_add(ToDelc, ToInc)),
    ets:insert(Ets, {counter, NewCounterSeq2}),

    %% 更新总匹配人数计数器
    [{_,ToDelTc}] = ets:lookup(Ets, {total_counter, Level}),
    [{_,ToInTc}] = ets:lookup(Ets, {total_counter, Prev}),
    ets:insert(Ets, {{total_counter, Level}, 0}), 
    ets:insert(Ets, {{total_counter, Prev}, ToInTc + ToDelTc}). 

counter_add(Counter1, Counter2) ->
    lists:foldl(fun(E, Acc) ->
                    E1 = element(E, Acc),
                    E2 = element(E, Counter2),
                    setelement(E, Acc, E1 + E2)
                end, Counter1, lists:seq(2,6)).

%% 压力测试
test(Step, StepNum, Interval) ->
    spawn(fun() -> do_test(Step, StepNum, Interval) end).

do_test(0, _, _) ->
    ignore;
do_test(Step, StepNum, Internal) ->
    lists:foreach(fun(_) ->
                    TID = random:uniform(10000000),
                    Len = 
                        case (random:uniform(7) rem 5) of
                            0 ->
                                1;
                            X ->
                                X
                        end,
                    Level = random:uniform(200),
                    Info = 
                        lists:foldl(fun(_, Acc) ->
                                        [#member_base_info{serverID=5, roleID=0,level=random:uniform(200)}|Acc]
                                    end, [], lists:seq(1, Len)),
                    Request = #request{serverID=5, id=?make_id(TID,Len), members=Info, level=Level},
                    erlang:send(?MODULE, {request, Request})
                end, lists:seq(1, StepNum)),
    receive 
        after Internal * 1000 ->
            do_test(Step - 1, StepNum, Internal)
    end.

get_sign(ID) ->
    erlang:get({sign, ID}).

set_sign(ID, Info) ->
    erlang:put({sign, ID}, Info).

delete_sign(ID) ->
    erlang:erase({sign, ID}).

update_sign(ID, {Pos, Value}) ->
    case get_sign(ID) of
        ?undefined ->
            ?ERR("update_sign exeption:~p.~n", [ID]); 
        Info ->
            Info2 = erlang:setelement(Pos, Info, Value),
            set_sign(ID, Info2) 
    end.
