-module(relic_match).

-behaviour(gen_server).

%% API
-export([start_link/1]).

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

-record(counter, {l1=0,l2=0,l3=0,l4=0,l5=0}).
-record(state, {ets=undefined   %本进程数据
               ,pid=undefined   %匹配
               ,warid=0
               ,level_rank=0}).

%%%===================================================================
%%% API
%%%===================================================================

start(LevelRank) ->
    {ok,_} = 
    supervisor:start_child(carlos_sup,
                            {?RELIC_MATCH_SERVER_NAME(LevelRank),
                             {?MODULE, start_link,[LevelRank]},
                             permanent, 600000, worker, [?MODULE]}).

start_link(LevelRank) ->
    gen_server:start_link({local, ?RELIC_MATCH_SERVER_NAME(LevelRank)}, ?MODULE, [LevelRank], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LevelRank]) ->
	case data_setting:get(server_type) of
		carlos_match ->
			process_flag(trap_exit,true),
			Ets = ets:new(ets_carlos_relic_match, []),
			ets:insert(Ets, {total_counter, 0}),
			ets:insert(Ets, #counter{}),
			lists:foreach(fun(E) ->
								  ets:insert(Ets, {{request_queue, E}, queue:new()})
						  end, lists:seq(1, 5)),
			%% 目前state的数据没有存盘的必要,但先预留好位置
			case db_sql:get_etc(?DB_ETC_KEY_CARLOS_RELIC+LevelRank-1) of
				[{state, #state{warid=WarID}}, {ets, EtsInfo}, Info] ->
					lists:foreach(fun(KV) -> ets:insert(Ets, KV) end, EtsInfo),
					lists:foreach(fun({K, V}) -> put(K, V) end, Info);
				_ ->
					WarID = LevelRank
			end,
			erlang:send_after(?DUMP_INTERVAL, self(), dump_data),
			{ok, #state{ets=Ets, pid=start_loop(Ets,self()), warid=WarID,level_rank=LevelRank}};
		_->
			?ERR("not carlos match server"),
			ignore
	end.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({inet_reply,_S,_Status},State) -> {noreply,State};
    
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.

terminate(_Reason, State) ->
    do_persist(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%% 匹配成功
do_handle_info({relic_match_result, RelicTeam}, #state{ets=Ets, warid=WarID, level_rank=LevelRank} = State) ->
    %{match, Team1, Team2, Counter}
    
    %% 目前除了这个消息外,这个gen_server只处理下面3个消息,预测以分配请求为主,
    %% 重启分配程序几乎不会发生,unrequest相对于request频率小很多.而request不涉及
    %% 大量运算,所以除非请求数据量很大,否则是不会出现消息堆积的情况,因此分配的处理
    %% 能放在这儿来做.其次,分配子程序和gen_server是并行的,如果在子程序中分配,则有
    %% 可能出现分配成功,但实际unrequest导致无法分配的情况,而在gen_server里面,消息
    %% 是串行的,不存在这个问题
    [EtsCounter] = ets:lookup(Ets, counter),
    case check_match(EtsCounter, RelicTeam) of
        false ->
            {noreply, State};
        _ ->
            Members = do_match(Ets, RelicTeam),
            case data_relic:get(need_in_room) of
                false ->
                    ParseRequestTeam = parse_request_team2(Members,WarID),
                    relic_war_manager:send_to_me({new_relic_war, WarID, ParseRequestTeam, LevelRank});
                _ ->
                    Need = data_relic:get(relic_member_num),
                    mark_in_room([Members]),
                    match_room_manager:create_room(?room_type_relic, {Need}, [Members], 
                                                  fun(From, [M]) ->
                                                     send_delete_sign(From, [M]),
                                                     ParseRequestTeam = parse_request_team(M,WarID),
                                                     relic_war_manager:send_to_me({new_relic_war, WarID, ParseRequestTeam, LevelRank})
                                                 end)
            end,
            {noreply, State#state{warid=WarID + 100}} %个位和十位表示难度，所以需要需要递增以100为单位
    end;

do_handle_info({match_want, RelicTeam, {_, RoomID, WantList}}, #state{ets=Ets} = State) ->
    [EtsCounter] = ets:lookup(Ets, counter),
    case check_match(EtsCounter, RelicTeam) of
        false ->
            match_room_manager:send_to_room(RoomID, {get, fail});
        _ ->
            Members = do_match(Ets, RelicTeam), 
            match_room_manager:send_to_room(RoomID, {get, WantList, [Members]})
    end,
    {noreply, State};

%% 分配请求 
do_handle_info({request, #request{serverID=ServerID,id=ID}=Request}, #state{ets=Ets, pid=Pid} = State) ->
    Len = ?team_len(ID), %取出队伍类型
    ?INFO("request team (~w)~w",[Len,ID]),
    case get_sign(ID) of
        ?undefined ->
            [{_, RequestQ}] = ets:lookup(Ets, {request_queue, Len}), %取得该队伍类型对应的请求队列
            NewRequestQ = queue:in(Request, RequestQ),               %插入新的请求
            [Counter] = ets:lookup(Ets, counter),                    %取得旧的counter数据
            ets:insert(Ets, {{request_queue, Len}, NewRequestQ}),    %保存新的队伍类型请求队列
            ets:insert(Ets, setelement(Len + 1, Counter, element(Len + 1, Counter) + 1)), %保存新的counter数
            ets:update_counter(Ets, total_counter, Len),            %保存新的total_counter??
            set_sign(ID, #match_sign_info{}),
            Result = 0,
            %% 触发匹配事件
            erlang:send(Pid, request_event);
        _ ->
            Result = 4  %% 重复报名
    end,
    %% 给客户端的请求应答
    send_msg:direct(ServerID, relic_server, {sign_result, ID, Result}),
    {noreply, State};

%% 重新分配请求 
do_handle_info({re_request, #request{id=ID}=Request}, #state{ets=Ets, pid=Pid} = State) ->
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
                    Len = ?team_len(ID), %取出队伍类型
                    [{_, RequestQ}] = ets:lookup(Ets, {request_queue, Len}), %取得该队伍类型对应的请求队列
                    NewRequestQ = queue:in(Request, RequestQ),               %插入新的请求
                    [Counter] = ets:lookup(Ets, counter),                    %取得旧的counter数据
                    ets:insert(Ets, {{request_queue, Len}, NewRequestQ}),    %保存新的队伍类型请求队列
                    ets:insert(Ets, setelement(Len + 1, Counter, element(Len + 1, Counter) + 1)), %保存新的counter数
                    ets:update_counter(Ets, total_counter, Len),            %保存新的total_counter??
                    set_sign(ID, #match_sign_info{}),
                    %% 触发匹配事件
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
%%     io:format("do_handle_info ~w~n", [{unrequest, ServerID, ID}]),
    case get_sign(ID) of
        ?undefined ->
            ignore;
        #match_sign_info{is_in_room=IsInRoom} = Sign ->
            case IsInRoom of
                false ->
                    Len = ?team_len(ID),
                    [{_, {R,F}}] = ets:lookup(Ets, {request_queue, Len}),
        
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
        
                    case Result of
                        false ->
                            ignore;
                        _ ->
                            RequestQ2 = {R2, F2},
                        
                            %% 更新数据
                            [Counter] = ets:lookup(Ets, counter),
                            ets:insert(Ets, {{request_queue, Len}, RequestQ2}),
                            ets:insert(Ets, setelement(Len + 1, Counter, element(Len + 1, Counter) - 1)),
                            ets:update_counter(Ets, total_counter, -Len)  
                    end;
                _ ->
                    %% 通知房间
                    match_room_manager:send_unrequest(ID, ?room_type_relic) 
            end,
            delete_sign(ID) 
    end,
    send_msg:direct(ServerID, relic_server, {reply_unrequest, ID}),
    {noreply, State};

%% 在房间中被踢走
do_handle_info({kick_room_teams, TeamList}, State) ->
    lists:foreach(fun({ServerID, ID}) ->
                    delete_sign(ID),
                    send_msg:direct(ServerID, relic_server, {reply_unrequest, ID}) 
                 end, TeamList),
    {noreply, State};

%% room的申请
do_handle_info({want, RoomID, WantList}, #state{pid=Pid}=State) ->
    erlang:send(Pid, {want, RoomID, WantList}),
    {noreply, State};

%% 子程序重启
do_handle_info(restart_sub_process, #state{ets=Ets,pid=Pid}=State) ->
    NewPid =
        case erlang:is_process_alive(Pid) of
            false ->
                start_loop(Ets,self());
            _ ->
                Pid
        end,
    erlang:send(NewPid, request_queue),
    {noreply, State#state{pid=NewPid}};

%% 处理程序退出
do_handle_info({'EXIT', From, Reason}, State) ->
    case Reason of
        shutdown ->
            ignore;
        normal ->
            ignore;
        _ ->
            %% 这儿不能一直重启,可能会导致无限重启
            erlang:send_after(?SUB_PROCESS_RESTART_INTERVAL, self(), restart_sub_process) 
    end,
    {noreply, State};

%% 存盘
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    {noreply, State};

do_handle_info({check_server_status,ServerID}, #state{ets=Ets}=State) ->
    ?INFO("recv check_server_status~w~n", [ServerID]),
    send_msg:direct(ServerID, relic_server, {check_server_status, ets:tab2list(Ets)}),
    {noreply, State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
%%     io:format("未知的消息:~p.~n", [Info]),
    {noreply, State}.

%% 开启匹配循环
start_loop(Ets,ParentPid) ->
    spawn_link(fun() -> start_process_loop(Ets,ParentPid) end).

%% 判断该分配方案是否还有效
check_match(EtsCounter, Counter) ->
    lists:all(fun(E) -> 
                element(E + 1, Counter) =< element(E + 1, EtsCounter)
            end, lists:seq(1, 5)).

%% 进行分配
%% 针对巨龙遗迹，返回值改为一个list
do_match(Ets, RelicTeam) ->
    do_match(RelicTeam, Ets, [], 5).

do_match(_, _, Members, 0) ->
    Members;
do_match(Counter, Ets, Members, X) ->
    [{_, RequestQ}] = ets:lookup(Ets, {request_queue, X}),
    CounterPos = X + 1,
    Need = element(CounterPos, Counter),
    case Need of
        0 ->
            do_match(Counter, Ets, Members, X - 1);
        _ ->
            {NewMembers, RequestQ2} = 
                lists:foldl(fun(_, {NewAcc, RequestQAcc}) ->
                                {{value, New}, RequestQAcc2} = queue:out(RequestQAcc),
                                {[New|NewAcc], RequestQAcc2}
                                end, {Members, RequestQ}, lists:seq(1, Need)),

                %% 更新队列和计数器
                ets:insert(Ets, {{request_queue, X}, RequestQ2}),
                [EtsCounter] = ets:lookup(Ets, counter),
                ets:insert(Ets, setelement(CounterPos, EtsCounter, element(CounterPos, EtsCounter) - Need)),
                ets:update_counter(Ets, total_counter, -X * Need),

                %% 更新分组数据
                do_match(Counter, Ets, NewMembers, X - 1)
    end.

%% 存盘
do_persist(#state{ets=Ets,level_rank=LevelRank}=State) ->
    SInfo = {state, State},
    EInfo = {ets, ets:tab2list(Ets)},
    db_sql:set_etc(?DB_ETC_KEY_CARLOS_RELIC+LevelRank-1, [SInfo, EInfo, get()]).

%%-------------------------------------------------------------------------------- 
%% 专门的分配进程
start_process_loop(Ets,ParentPid) ->
    init_loop(),
    loop(Ets,ParentPid).

init_loop() ->
    %% 1放在前面, 可以减少失败匹配的次数
    put(5, [[1,4],[3,2]]),
    put(4, [[1,3], [2,2]]),
    put(3, [[1,2]]),
    put(2, [[1,1]]).

loop(Ets,ParentPid) ->
    receive 
        request_event ->
            request_event(Ets,ParentPid);
        {want, _RoomID, _WantList} = WantData ->
            want_event(Ets, WantData) 
        %% 在空闲时间匹配匹配看看(正常应该是匹配不到的,但合理利用CPU,防止出现异常的长时间匹配)
        after ?SUB_PROCESS_TIMEOUT ->
            request_event(Ets,ParentPid)
    end,
    loop(Ets,ParentPid).

% 执行匹配逻辑
request_event(Ets,ParentPid) ->
    [{_, Total}] = ets:lookup(Ets, total_counter),
    case Total < data_relic:get(relic_member_num) of
        true ->
            ignore;
        _ ->
            [Counter] = ets:lookup(Ets, counter),
            case dfs_try_match(Counter) of
                undefined ->
                    ignore;
                {RelicTeam} ->
                    %% 将匹配好的分组发给gen_server
%%                     io:format("request_event ok ~w",[statistic([RelicTeam])]),
                    erlang:send(ParentPid, {relic_match_result, statistic([RelicTeam])}) 
            end
    end.

%% 处理room的申请
want_event(Ets, {want, RoomID, WantList} = WantData) ->
    [Counter] = ets:lookup(Ets, counter),
    case dfs_try_match(Counter, WantList) of
        undefined ->
            %%因为匹配失败了,并不涉及到数据分配,所以这儿可以直接给room发消息,不用先转发给主进程
            match_room_manager:send_to_room(RoomID, {get, fail});
        {RelicTeam} ->
            %% 将匹配好的分组发给gen_server
            erlang:send(?MODULE, {match_want, statistic([RelicTeam]), WantData})  
    end.

%% 尝试匹配两个组,要求优先匹配队伍,所以这儿用bfs更符合些,
%% 但这儿的bfs实现要复杂些,且一般情况下,两个的结果不会有没太大区别
%% 注意Counter不是常量,也是acc来的,需要进行更新
dfs_try_match(Counter) ->
    NeedList = data_relic:get(match_need_list),
    case dfs_try_match(Counter, NeedList) of
        [] ->
            undefined;
        {_Counter2, RelicTeam} ->
            {RelicTeam}
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
    send_msg:direct_by_name(carlos_match, self(), Msg).

%% 将request list转换为member_base_info list
parse_request_team(Team,WarID) ->
    lists:foldl(fun(#request{serverID=ServerID, id=ID, members=Info}, Acc) -> 
                        send_msg:direct(ServerID, relic_server, {relic_match_success, ID, WarID}),
                        Info ++ Acc 
                    end, [], Team).

parse_request_team2(Team,WarID) ->
    lists:foldl(fun(#request{serverID=ServerID, id=ID, members=Info}, Acc) -> 
                        delete_sign(ID),
                        send_msg:direct(ServerID, relic_server, {relic_match_success, ID, WarID}),
                        Info ++ Acc 
                    end, [], Team).

mark_in_room(Teams) ->
    lists:foreach(fun(Team) ->
                    lists:foreach(fun(#request{id=ID}) ->
                                    update_sign(ID, {#match_sign_info.is_in_room, true})
                                end, Team)
                end, Teams).

send_delete_sign(From, Teams) ->
    TeamIDs = 
        lists:foldl(fun(Team, Acc) ->
                        lists:foldl(fun(#request{id=ID}, Acc2) ->
                                        [ID|Acc2]
                                    end, Acc, Team)
                    end, [], Teams),
    erlang:send(From, {delete_sign, TeamIDs}).

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
