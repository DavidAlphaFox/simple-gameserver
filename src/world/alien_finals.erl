-module(alien_finals).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_alien.hrl").

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

-record(state, {status=0,next_status_time_stamp=0,slave_node_list=[],slave_server_list=[], rest_notice_list=[], cur_round=0, left_game=0, guess_list=[], champion_id=0, distribute_node=[], distribute_name=[], master_node_list=[], master_server_list=[],finals_node=[],finals_server=[]}).
-compile(export_all).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(FIGHT_LOOP_INTERVAL, 2000). 
-define(GROUP_MATCH_DELAY_TIME, 3000).              %% 分完小组后,延迟广播的时间
-define(CHECK_STATUS_INTERVAL, 1000).
-define(ROLE_INFO_KEY, role_info_key).              %% 玩家信息缓存
-define(MATCH_CACHE_LIST, match_cache_list).        %% 战斗序列缓存
-define(WIN_CACHE_LIST, win_cache_list).            %% 获胜列表缓存
-define(SIGN_CACHE_LIST, sign_cache_list).          %% 参赛列表缓存(预开始阶段使用)
-define(MATCH_LIST_KEY, match_list_key).            %% 战斗序列列表(查看玩家列表用)
-define(MATCH_RECORD_KEY, match_record_key).        %% 战报缓存列表
-define(GUESSED_ROLE_ID, guessed_role_id).          %% 参与了竞猜的玩家ID信息(服务器ID、档次, 被押的玩家ID) 
-define(REPLAY_RECORD, replay_record).              %% 战斗回放缓存

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    {ok,_} = 
    supervisor:start_child(world_sup, 
                            {?MODULE, 
                              {?MODULE, start_link, []},
                              permanent, 600000, worker, [?MODULE]}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
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
    process_flag(trap_exit, true),
    erlang:set_cookie(erlang:node(), data_setting:get(cookie)),
    case db_sql:get_etc(?DB_ETC_KEY_ALIEN_FINALS) of
        [{state,State}|DicInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, DicInfo);
        _ ->
            State = #state{status=?STATUS_FINALS_WAIT}
    end,
    FinalsNode = get_alien_finals_node(),
    FinalsServer = get_alien_finals_name(),
    %%% 启动后先广播当前server_name node给所有子服
    erlang:send(erlang:self(), bc_status),
    erlang:send(erlang:self(), check_node_connection),
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
	erlang:send_after(?DUMP_INTERVAL,self(),do_hibernate),
    {ok, State#state{finals_node=FinalsNode, finals_server=FinalsServer}}.

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

handle_info(bc_status, #state{finals_node=FinalsNode, finals_server=FinalsServer}=State) ->
    bc_msg_to_server([],{finals_update_node_info, FinalsNode, FinalsServer}),
    {noreply, State};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};    

handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
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
%%% 客户端消息
do_handle_info({ServerID, {client_msg, RoleID, #cs_alien_finals_info{}}}, #state{status=Status, cur_round=CurRound, left_game=LeftGame, next_status_time_stamp=NextTimeStamp}=State) ->
    CurRound2 = 
        case Status of
            ?STATUS_FINALS_FINAL ->
                CurRound;
            _ ->
                LeftGame
        end,
    send_msg_to_slave_sever_id(ServerID, {finals_finals_info, RoleID, Status, CurRound2, NextTimeStamp}), 
    {noreply, State};

do_handle_info({ServerID, {client_msg, RoleID, #cs_alien_finals_fight_replay{replayUID=ReplayUID}}}, State) ->
    send_msg_to_slave_sever_id(ServerID, {finals_fight_replay, RoleID, get_replay_record(ReplayUID)}),
    {noreply, State};

do_handle_info({ServerID, {client_msg, RoleID, #cs_alien_finals_guess{guessID=GuessID, rank=Rank}}}, #state{guess_list=GuessList}=State) ->
    case check_can_guess(State,RoleID, GuessID) of
        {false, Result} ->
            send_msg_to_slave_sever_id(ServerID, {finals_guess_return, false, RoleID, Result, Rank}),
            NewState = State;
        _ ->
            NewGuessList = do_guess_champion(ServerID, RoleID, GuessID, Rank, GuessList),
            send_msg_to_slave_sever_id(ServerID, {finals_guess_return, true, RoleID}),
            NewState = State#state{guess_list=NewGuessList}
    end,
    {noreply, NewState};

do_handle_info({ServerID, {client_msg, RoleID, #cs_alien_finals_list{type=Type, groupID=GroupID}}}, State) ->
    send_msg_to_slave_sever_id(ServerID, {finals_finals_list, RoleID, Type, GroupID, get_match_list(Type, GroupID, State)}),
    {noreply, State};

do_handle_info({ServerID, {client_msg, RoleID, #cs_alien_finals_records{type=Type, groupID=GroupID}}}, State) ->
    send_msg_to_slave_sever_id(ServerID, {finals_finals_records, RoleID, Type, GroupID, get_record_list(Type, GroupID, State)}),
    {noreply, State};

do_handle_info({ServerID, {client_msg, RoleID, #cs_alien_finals_self_info{}}}, State) ->
    GuessRank = case erlang:get({?GUESSED_ROLE_ID, RoleID}) of
                    ?undefined ->
                        GuessID = 0,
                        0;
                    {_, Rank, GuessID} ->
                        Rank
                end,
    #role_alien_final{groupID=GroupID} = get_role_info(RoleID),
    send_msg_to_slave_sever_id(ServerID, {finals_self_info, RoleID, GuessID, GuessRank, GroupID}),
    {noreply, State};

%%% 服务器数据
%% 断线冲连,如果此时是结束状态,需要通知分配服(防止状态切换信息没有发出去)
do_handle_info(notice_request_status, #state{status=Status, next_status_time_stamp=NextTimeStamp}=State)->
    case Status =:= ?STATUS_FINALS_FINAL andalso NextTimeStamp =:= 0 of
        true ->
            send_msg:direct_by_name(alien_distribute, alien_distribute,notice_alien_finals_finish);
        _ ->
            ignore
    end,
	{noreply,State};

do_handle_info({set_next_timestamp, NextTimeStamp},State) ->
    {noreply, State#state{next_status_time_stamp=NextTimeStamp}};

do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    {noreply, State};

do_handle_info(check_node_connection, State) ->
    erlang:send_after(?CHECK_STATUS_INTERVAL, erlang:self(), check_node_connection),
    NewState = check_status(State),
    {noreply, NewState};

do_handle_info(change_status_to_wait, #state{status=?STATUS_FINALS_FINAL,next_status_time_stamp=0}=State) ->
    %% 清空所有缓存数据, 这个消息分配服不会确保决赛服必然会收到
    erlang:erase(),
    {noreply, State#state{status=?STATUS_FINALS_WAIT, champion_id=0}};

do_handle_info(change_status_to_wait, State) ->
    {noreply, State};

%% 收到分配服发过来的断线重连数据
do_handle_info({notice_status_is_final, DistributeName, DistributeNode, MasterList, SlaveList}, #state{status=Status}=State) ->
    %% 只有当前状态是等待时,才需要处理(防止分配服发送过来的切换状态丢失)
    case Status =:= ?STATUS_FINALS_WAIT of
        true ->
            erlang:send(?MODULE, {pre_open_finals, DistributeName, DistributeNode, MasterList, SlaveList});
        _ ->
            ignore
    end,
    {noreply, State};

%% 收到分配服消息, 装备开始
do_handle_info({pre_open_finals, DistributeName, DistributeNode, MasterList, SlaveList}, #state{finals_node=FinalsNode, finals_server=FinalsServer}=State) ->
    % 决赛开始前广播server name, server node给所有子服.这里不用erlang:send,因为由可能在这个
    % 消息前有几个其他消息
    bc_msg_to_server([],{finals_update_node_info, FinalsNode, FinalsServer}),
    NewStatus = ?STATUS_FINALS_PREOPEN,
    {MasterNodes, MasterNames} = lists:foldl(fun({MNode, MName}, {MNodeList, MNameList}) ->
                                                {[MNode|MNodeList], [MName|MNameList]}
                                             end, {[],[]}, MasterList),
    {SlaveNodes, SlaveNames} = lists:foldl(fun({SNode, SName}, {SNodeList, SNameList}) ->
                                                {[SNode|SNodeList], [SName|SNameList]}
                                             end, {[],[]}, SlaveList),
                            
    %% 清空所有缓存数据
    erlang:erase(), 

    %% 清空旧的战报
    db_sql:del_replay_with_type(?REPLAY_TYPE_CROSS),

    %% 计算开始时间
    StartTime = data_alien_finals:get(group_match_start),
    NewTimeStamp = util:now() + StartTime,

    %% 这里left_game = 1代表尚未从全部主服取得数据
    {noreply, State#state{status=NewStatus, master_node_list=MasterNodes, master_server_list=MasterNames, slave_node_list=SlaveNodes, 
                            slave_server_list=SlaveNames, rest_notice_list=SlaveNames, left_game=1, champion_id=0, distribute_node=DistributeNode,
                            distribute_name=DistributeName, guess_list=[], next_status_time_stamp=NewTimeStamp}};

do_handle_info(do_fight_loop, State) ->
    NewState = do_fight_loop(State),
    {noreply, NewState};

do_handle_info(finals_update_status, #state{status=Status, cur_round=CurRound, left_game=LeftGame, next_status_time_stamp=NextTimeStamp, slave_server_list=SlaveServerList}=State) ->
    CurRound2 = 
        case Status of
            ?STATUS_FINALS_FINAL ->
                CurRound;
            _ ->
                LeftGame
        end,
    bc_msg_to_server(SlaveServerList, {finals_update_status, Status, CurRound2, NextTimeStamp}), 
    {noreply, State};

do_handle_info({check_server_alive,Ref,DistributeServer},State) ->
	send_msg:direct_by_name(DistributeServer,alien_distribute,{server_alive, Ref}),
	{noreply,State};

do_handle_info({reload_fighter_info,RoleID},State)->
    get_roleFightInfo(RoleID),
    {noreply,State};

do_handle_info(Msg, _) ->
    ?ERR("Error unknow message:~w.~n", [Msg]).

do_persist(State) ->
    Info = [E || E <- erlang:get(), is_persist(E)],
    Info2 = [{state,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_ALIEN_FINALS, Info2).

get_master_finals_match_list(ServerName,FinalsServer)->
	Ref = erlang:make_ref(),
	case send_msg:direct_by_name(ServerName, alien_master_server,{get_finals_match_list,Ref,FinalsServer}) of
		?undefined ->
			msg_queue_down;
		_ ->
			receive 
				{get_finals_match_result,Ref, List} ->
					{ok,List}
			after 3000 ->
					wait_msg_failed
			end
	end.

%% 状态切换规则:
%% 1. 决赛服收到分配服通知后,转入预开始状态,此时时间戳为0，这个状态将一直持续到决赛服从所有主服上获得数据后
%% 2. 决赛服获取所有参赛数据后, 计算开启时间, 进行分组, 设置小组赛局数
%% 3. 当当前时间超过时间戳后, 判断是否还有下局, 有则修改时间戳, 进行下一局比赛。没有则切换到下个状态,并通知
%%    所有的从服务器
%% 4. 决赛结束后, 修改时间戳为0, 表示决赛结束, 通知分配服交还控制权
%% 5. 下次异星报名时, 分配服会通知决赛服转入等待状态

%% 从各个master获得参赛列表, 如果有服务器没有应答, 将一直等待下去
%% left_game != 0 代表尚未从各个主服取得全部数据

%% 等待操作时, 决赛服不做任何处理
check_status(#state{status=?STATUS_FINALS_WAIT}=State) ->
    State;

check_status(#state{status=?STATUS_FINALS_PREOPEN, left_game=1, master_server_list=MasterList,finals_server=FinalsServer}=State) ->
    CallList = case erlang:get(?SIGN_CACHE_LIST) of
                    ?undefined ->
                        List = [],
                        MasterList;
                    List ->
                       lists:foldl(fun({MasterName, _MatchList}, Acc) ->
                                        Acc -- [MasterName]
                                   end, MasterList, List)
               end,
    NewList = lists:foldl(fun(E, ListAcc) ->
									case get_master_finals_match_list(E,FinalsServer) of
                                        {ok, MatchList} ->
                                            [{E, MatchList}|ListAcc];       
                                        Error ->
                                            ?ERR("call 主服务器:~w时发生错误:~w.~n", [E, Error]),
                                            ListAcc
                                    end
                         end, List, CallList),
    erlang:put(?SIGN_CACHE_LIST, NewList),
    case length(NewList) =:= length(MasterList) of
        false ->
            %% 还未收到全部数据,继续等待
            State;
        _ ->
            %% 分配参赛小组, 并计算下次开启时间
            NewMatchList = do_assign(NewList), 
            init_match_info(?STATUS_FINALS_GROUP_MATCH, NewMatchList), 
            init_match_cache(NewMatchList), 
            
            %%% 清除报名缓存
            erlang:erase(?SIGN_CACHE_LIST),

            %% 分完组 先等服务器ping会儿,在发通知
            erlang:send_after(?GROUP_MATCH_DELAY_TIME, ?MODULE, finals_update_status),
            State#state{left_game=0}
    end;

%% 决赛结束到下一次报名期间,不做任何处理,但和从服务器保持连接,供查看数据用
check_status(#state{status=?STATUS_FINALS_FINAL, next_status_time_stamp=0}=State) ->
    State;

%% 在这里处理的状态有:分配后的预开启状态、小组赛、四强赛、决赛进行中
check_status(#state{status=Status, left_game=LeftGame, next_status_time_stamp=NextTimeStamp, cur_round=CurRound, slave_server_list=SlaveServerList, distribute_name=DistributeName}=State) ->
    Now = util:now(),
    case NextTimeStamp > Now of
        true ->
            State;
        _ ->
            %% 每局时间到了才会进入这里, 因此当LeftGame=1表明最后一局已经结束
		    case LeftGame > 1 of
		        true ->
                    OffsetList = data_alien_finals:get({offset_start_time, Status}), 
                    Total = length(OffsetList),
                    NewLeftGame = LeftGame - 1,
                    CurGame = Total - NewLeftGame + 1,
                    NextTimeOffset = lists:nth(CurGame, OffsetList),
                    NewTimeStamp = NextTimeOffset + NextTimeStamp,
                    bc_msg_to_server(SlaveServerList, {finals_update_status, Status, CurRound, NewTimeStamp}), 
                    
                    %% 开始打下一局(cur_round不进行重置,生成战报时分组用)
                    erlang:send_after(?FIGHT_LOOP_INTERVAL, erlang:self(), do_fight_loop),
                    State#state{next_status_time_stamp=NewTimeStamp, left_game=NewLeftGame};
		        _ ->
                    %% 决赛结束后, 决赛服直接将消息发送给分配服, 交还控制权
                    %% 其他状态下, 则通知从服务器状态发生了改变
                    case Status =:= ?STATUS_FINALS_FINAL of
                        true ->
                            %% 决赛时间结束时,如果还没有决出胜负,那么将一直等待下去
                            case LeftGame =:= 1 of
                                true ->
                                    ?ERR("警告:异星决赛时间已经结束,但依然未决出胜负.~n",[]),
                                    State;
                                _ ->
                                    %send_msg_to_server(DistributeName, alien_finals_finish),
									send_msg_to_distribute(DistributeName, alien_finals_finish),
                                    %% 因为需求要求决赛界面到下次报名时才关闭,因此比赛结束后通过设置时间戳来代表决赛结束
                                    State#state{next_status_time_stamp=0}
                            end;
                        _ ->
                            NewStatus = Status + 1,
                            OffsetList = data_alien_finals:get({offset_start_time, NewStatus}), 
                            NewLeftGame = length(OffsetList),
                            NextTimeOffset = hd(OffsetList),
                            NewTimeStamp = NextTimeOffset + NextTimeStamp,
                            bc_msg_to_server(SlaveServerList, {finals_update_status, NewStatus, 1, NewTimeStamp}),
                            
                            %% 开始打下一状态的比赛
                            erlang:send_after(?FIGHT_LOOP_INTERVAL, erlang:self(), do_fight_loop),
                            State#state{status=NewStatus, left_game=NewLeftGame, next_status_time_stamp=NewTimeStamp, cur_round=1}
                    end
		    end
    end.

send_msg_to_server(SlaveName, Msg) ->
	send_msg:direct_by_name(SlaveName,alien_master_server,Msg).
send_msg_to_distribute(ServerName,Msg) ->
	send_msg:direct_by_name(ServerName, alien_distribute, Msg).

bc_msg_to_server(_SlaveList, Msg) ->
	send_msg:broadcast(alien_server,Msg).

send_msg_to_slave_sever_id(ServerID, Msg) ->
    case ServerID of
        0 ->
            ignore;
        _ ->
            alien_master_server:send_msg_to_slave_sever_id(ServerID, Msg)
    end.

%% 根据分配规则分配小组
do_assign(NewList) ->
    MatchList = lists:foldl(fun({_,List},Acc) -> List ++ Acc end, [], NewList),
    MatchList2 = lists:reverse(lists:keysort(#role_alien_final.fightPower, MatchList)),
    SeedNumber = data_alien_finals:get(seed_player_number),
    GroupNumber = data_alien_finals:get(group_number),
    %% 取出种子选手, 这里不用split, 是因为split没有sublist安全
    SeedList = lists:sublist(MatchList2, SeedNumber), 
    RestList = MatchList2 -- SeedList,
    SeedList2 = util:random_list(SeedList),
    RestList2 = util:random_list(RestList),
    EachNumber = erlang:max(1, erlang:length(RestList2) div GroupNumber),
    AssignList2 =  case do_assign(SeedList2, RestList2, 1, GroupNumber, EachNumber, []) of
                        {[], AssignList} ->
                            AssignList;
                        {RestList3, AssignList} ->
                            do_assign2(RestList3, AssignList, erlang:length(AssignList))
                    end,

    %% 缓存参赛玩家数据, 并按照小组生成对战ID列表
    lists:foldl(fun({GroupID, PlayerList}, Acc) ->
                    GroupIDList = lists:foldl(fun(#role_alien_final{roleID=RoleID}=RoleFinal, {GroupID2, IDList}) ->
                                                set_role_info(RoleID, RoleFinal#role_alien_final{groupID=GroupID}),
                                                {GroupID2, [RoleID|IDList]} 
                                            end, {GroupID, []}, PlayerList),
                    [GroupIDList|Acc] 
                end, [], AssignList2).

%% Acc: {Rest, Assign}

%% 参赛人数比小组数少的情况
do_assign(SeedList, [], 1, _, _, _) ->
    {[], [{1, SeedList}]};

%% 种子选手比其余选手人数更多时的情况
do_assign(SeedList, [], _, _, _, Acc) ->
    {SeedList, Acc};

%% 分配至最后一组
do_assign(SeedList, RestList, GroupNumber, GroupNumber, EachNumber, Acc) ->
    Assign2 = lists:sublist(RestList, EachNumber),
    RestList2 = RestList -- Assign2,
    case SeedList of
        [] ->            
            Assign3 = Assign2,
            SeedList2 = [];
        _ ->
            Assign3 = [erlang:hd(SeedList)|Assign2],
            SeedList2 = lists:nthtail(1, SeedList)
    end,
    Assign4 = util:random_list(Assign3),
    {RestList2 ++ SeedList2, [{GroupNumber, Assign4}|Acc]};

do_assign(SeedList, RestList, GroupID , GroupNumber, EachNumber, Acc) ->
    Assign2 = lists:sublist(RestList, EachNumber),
    case SeedList of
        [] ->
            Assign3 = Assign2,
            SeedList2 = [];
         _ ->
            Assign3 = [erlang:hd(SeedList)|Assign2],
            SeedList2 = lists:nthtail(1, SeedList)
    end,
    Assign4 = util:random_list(Assign3),
    RestList2 = RestList -- Assign2,
    do_assign(SeedList2, RestList2, GroupID + 1, GroupNumber, EachNumber, [{GroupID, Assign4}|Acc]).

%% 将剩余人数依此分到各个组,这里没必要随机了,
%% 因为剩余的列表本身就随机过了
do_assign2([], AssignList, _ ) ->
    AssignList;

do_assign2([H|T], AssignList, Pos) ->
    {GroupID, MatchList} = lists:nth(Pos, AssignList),
    AssignList2 = lists:keyreplace(GroupID, 1, AssignList, {GroupID, [H|MatchList]}),
    NewPos = case Pos of
                1 ->
                    erlang:length(AssignList);
                _ ->
                    Pos - 1 
             end,
    do_assign2(T, AssignList2, NewPos).

%% 小组赛:
%%  按照小组依次来进行比赛,每次选取两人进行比赛,获胜者放入在?WIN_CACHE_LIST
%%  中对应的小组内。当小组的?MATCH_CACHE_LIST为空时,如果?WIN_CACHE_LIST人数
%%  大于1,则将?WIN_CACHE_LIST移入到?MATCH_CACHE_LIST中继续比赛.如果为1人,则
%%  进行下组的比赛
%%
%%  八强、四强、半决赛能看作是小组赛的特化, 也走这个流程
%%
%%  决赛也是小组赛的特化, 也走这个流程.但决赛是多局获胜制,所以其结束后的处理和其他状态不一样
do_fight_loop(#state{status=Status,cur_round=CurRound}=State) ->
    case erlang:get(?MATCH_CACHE_LIST) of
        [] ->
            do_stage_final(State);
        [{GroupID,MatchList}|_] = GroupMatchList ->
            case MatchList of
                [FighterID|[]] -> 
                    %% 轮空不需要战报
                    add_group_win_role(GroupID, FighterID),
                    refresh_group_match(GroupID, [], GroupMatchList, State);
                [FighterID1,FighterID2|FT]  ->
                   {WinnerID,Record}=do_fight(FighterID1, FighterID2),
                   add_group_win_role(GroupID, WinnerID),
                   add_group_record(Status, GroupID, CurRound, Record),
                   refresh_group_match(GroupID, FT, GroupMatchList, State);
                Any ->
                    ?ERR("异星决赛,当前状态:~p, 比赛数据异常:~w.~n",[Status, Any]),
                    State
            end
    end.

%% 添加一个玩家到小组赛每轮获胜列表中
add_group_win_role(GroupID, RoleID) ->
    WinCacheList = erlang:get(?WIN_CACHE_LIST),
    case lists:keytake(GroupID, 1, WinCacheList)  of
        false ->
            NewWinCacheList = [{GroupID, [RoleID]} | WinCacheList];
        {value, {GroupID, IDList}, RestList} ->
            NewWinCacheList = [{GroupID, [RoleID|IDList]} | RestList]
    end,
    erlang:put(?WIN_CACHE_LIST, NewWinCacheList).

%% 添加一条战报
add_group_record(Status, GroupID, CurRound, Record) ->
    RecordList = erlang:get({?MATCH_RECORD_KEY, Status}),
    NewRecordList = case lists:keytake(GroupID, 1, RecordList) of
				        false ->
				            [{GroupID, [{CurRound, [Record]}]} | RecordList];
				        {value, {GroupID, RoundList}, RestList} ->
				            case lists:keytake(CurRound, 1, RoundList) of
				                false ->
				                    NewRoundList = {CurRound, [Record]},
				                    NewGroupList = {GroupID, [NewRoundList|RoundList]},
				                    [NewGroupList|RestList];
				                {value, {CurRound, CurRoundList}, RestRoundList} ->
				                    NewRoundList = {CurRound, [Record|CurRoundList]},
				                    NewGroupList = {GroupID, [NewRoundList|RestRoundList]},
				                    [NewGroupList|RestList]
				            end
				    end,
    erlang:put({?MATCH_RECORD_KEY, Status}, NewRecordList).

%% 更新小组赛的对战序列和获胜缓存 
refresh_group_match(GroupID, RestMatchList, GroupMatchList, #state{status=Status,cur_round=CurRound}=State) ->
    case RestMatchList of
        [] ->
            WinCacheList = erlang:get(?WIN_CACHE_LIST),
            case lists:keyfind(GroupID, 1, WinCacheList) of 
                false ->
                    ?ERR("异星决赛异常,组:~p没有玩家.~n", [GroupID]),
                    NewRound = CurRound,
                    NewGroupMatchList = [];
                {GroupID, WinList} = Info ->
                    case length(WinList) of 
                        1 ->
                            %% 打下一组, 下组有可能为空
                            NewGroupMatchList = lists:nthtail(1, GroupMatchList),
                            %% 决赛继续做个特例, 打完后加轮数, 代表当前是第几局
                            case Status of
                                ?STATUS_FINALS_FINAL ->
                                    NewRound = CurRound + 1;
                                _ ->
                                    NewRound = 1 
                            end;
                        _ ->
                            %% 继续打这组
                            NewGroupMatchList = lists:keyreplace(GroupID, 1, GroupMatchList, Info),
                            NewRound = CurRound + 1,
                            %% 打下一轮, 需要清空该小组的获胜缓存
                            NewWinCacheList = lists:keyreplace(GroupID, 1, WinCacheList, {GroupID, []}),
                            erlang:put(?WIN_CACHE_LIST, NewWinCacheList)
                    end
            end;
        _ ->
            NewRound = CurRound,
            NewGroupMatchList = lists:keyreplace(GroupID, 1, GroupMatchList, {GroupID, RestMatchList})
    end,
    erlang:put(?MATCH_CACHE_LIST, NewGroupMatchList),
    erlang:send_after(?FIGHT_LOOP_INTERVAL, erlang:self(), do_fight_loop),
    State#state{cur_round=NewRound}.

%% 获得角色的:
%% 战斗力, 上阵列表, 加成
get_roleFightInfo(0) ->
    ?ERR("警告:异星总决赛,尝试获取不存在的玩家数据",[]),
    {0,[],{0,0},[],[],#trSpecial{},#skin_info{}};
%% 进行更新, 当获取失败时 使用缓存的数据

get_roleFightInfo(RoleID) ->
    #role_alien_final{serverID=ServerID} = get_role_info(RoleID),
%%    Platform = data_setting:get(platform),
%%    SlaveServerName = alien_master_server:get_cross_server_name(ServerID, false, Platform),
	case wait_msg(ServerID,RoleID) of
		{false,Reason} ->
			?ERR("call ~w时发生错误:~w, 玩家:~w数据获取失败", [ServerID,Reason, RoleID]),
			#role_alien_final{fightPower=FightPower, fighterList=FighterList, lieuAdd = LieuAdd,equiped_list=CurEquipedList, talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo} = get_role_info(RoleID),
            {FightPower, FighterList, LieuAdd,CurEquipedList,TalentList,TrSpecial,SkinInfo};
		{FightPower,FighterList,LieuAdd,TalentList,TrSpecial,NewEquipedList,SkinInfo,Vip} ->
			RoleInfo = get_role_info(RoleID),
			set_role_info(RoleID, RoleInfo#role_alien_final{fightPower=FightPower, fighterList=FighterList, lieuAdd = LieuAdd,vip=Vip,
															equiped_list=NewEquipedList, talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo}),
			{FightPower, FighterList, LieuAdd, NewEquipedList, TalentList,TrSpecial,SkinInfo}
	end.

wait_msg(ServerID,RoleID) ->
	Ref = erlang:make_ref(),
	case send_msg:direct(ServerID,alien_server,{get_roleFightInfo,Ref,RoleID}) of
		?undefined ->
			{false,msg_queue_down};
		_ ->
			receive 
				{get_roleFightInfoBack,Ref, {FightPower, FighterList, LieuAdd, TalentList, NewEquipedList,TrSpecial,SkinInfo,Vip}} = Msg ->
					?ERR("get info back:~w",[Msg]),
					{FightPower,FighterList,LieuAdd,TalentList,TrSpecial,NewEquipedList,SkinInfo,Vip}
			after 3000 ->
					{false,wait_msg_failed}
			end
	end.

get_role_info(RoleID) ->
    case erlang:get({?ROLE_INFO_KEY, RoleID}) of
        #role_alien_final{} = Result ->
            Result;
        _ ->
            #role_alien_final{roleID=0,roleName= <<"">>,isMale=true,title=0,fightPower=0,head=0,level=0,vip=0, trSpecial=#trSpecial{},
                                serverID=0, groupID=0,fighterList=[],lieuAdd={0,0},equiped_list=[],talentList=[],skin_info=#skin_info{}}
    end.

set_role_info(RoleID, RoleInfo) ->
	FighterList2 = [begin
						#ger{gerAttr=#gerAttr{gerHpMax=MaxHp,gerProHpMax=GerProHpMax}}=F,
						F#ger{gerHp=MaxHp,gerProHp=GerProHpMax}
					end||F<-RoleInfo#role_alien_final.fighterList],
	RoleInfo2 = RoleInfo#role_alien_final{fighterList=FighterList2},
	erlang:put({?ROLE_INFO_KEY, RoleID}, RoleInfo2).

%% 两个玩家进行比赛, 返回获胜者ID以及战报
do_fight(FighterID1, FighterID2) ->
    {FightPower1, FighterList1, LieuAdd1,EquipedList1, TalentList1,TrSpecial1,SkinInfo1} = get_roleFightInfo(FighterID1),
    {FightPower2, FighterList2, LieuAdd2,EquipedList2, TalentList2,TrSpecial2,SkinInfo2} = get_roleFightInfo(FighterID2),
    {AttackerID, DefenderID, AttackerList, DefenderList, LieuAddAttacker, LieuAddDefender, AtkTalent, DefTalent,TrSpecialA,TrSpecialD,SkinInfoA,SkinInfoD,EquipedListA,EquipedListD} 
        = check_attack_and_defend(FighterID1,FighterID2,FighterList1,FighterList2, LieuAdd1, LieuAdd2, FightPower1, FightPower2, TalentList1, TalentList2,TrSpecial1,TrSpecial2,SkinInfo1,SkinInfo2,EquipedList1,EquipedList2),
    {IsWin, ReplayUID} = do_fight(AttackerID, DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender,AtkTalent,DefTalent,EquipedListA,EquipedListD,TrSpecialA,TrSpecialD,SkinInfoA,SkinInfoD),
    #role_alien_final{roleName=AttackerName} = get_role_info(AttackerID),
    #role_alien_final{roleName=DefenderName} = get_role_info(DefenderID),
    WinnerID = case IsWin of
                    true -> 
                        AttackerID;
                    _ ->
                        DefenderID
                end,
     {WinnerID, #p_alien_finals_record{atkName=AttackerName, atkID=AttackerID, defName=DefenderName, defID=DefenderID, isatkwin=IsWin, replayUID=ReplayUID}}.

%% 返回攻击者是否获胜, 以及回放ID 
do_fight(AttackerID, DefenderID, AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender,AtkTalent,DefTalent,EquipedList1,EquipedList2,TrSpecialA,TrSpecialD,SkinInfoA,SkinInfoD) ->
    GerEquipList1 = role_item:assort_ger_equiplist(EquipedList1),
    LegendAddList1 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList1],
    GerEquipList2 = role_item:assort_ger_equiplist(EquipedList2),
    LegendAddList2 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList2],
    case catch role_fight:new(AttackerList, DefenderList, LieuAddAttacker, LieuAddDefender,AtkTalent,DefTalent,TrSpecialA,TrSpecialD,SkinInfoA,SkinInfoD,LegendAddList1,LegendAddList2) of
        {IsWin, FightRecord0, _} ->
            FighterList2 = role_data:get_FighterList_with_effect(EquipedList1,EquipedList2,FightRecord0#sc_fight_request.fighterList),
            FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},                      
            ReplayUID = tk_id:gen_replayUID(),
            erlang:put({?REPLAY_RECORD, ReplayUID}, FightRecord),
            catch db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_CROSS);
        _ ->
            ?ERR("玩家:~w,~w比赛时对战异常.~n",[AttackerID, DefenderID]),
            IsWin = false,
            ReplayUID = 0
    end,
    {IsWin, ReplayUID}.

%% 战斗力高的先出手
check_attack_and_defend(FighterID1,FighterID2,FighterList1,FighterList2, LieuAdd1, LieuAdd2, FightPower1, FightPower2, TalentList1, TalentList2,TrSpecial1,TrSpecial2,SkinInfo1,SkinInfo2,EquipedList1,EquipedList2) ->
    case FightPower1 > FightPower2 of
        true ->
            {FighterID1, FighterID2, FighterList1, FighterList2, LieuAdd1, LieuAdd2, TalentList1, TalentList2,TrSpecial1,TrSpecial2,SkinInfo1,SkinInfo2,EquipedList1,EquipedList2};
        _ ->
            {FighterID2, FighterID1, FighterList2, FighterList1, LieuAdd2, LieuAdd1, TalentList2, TalentList1,TrSpecial2,TrSpecial1,SkinInfo2,SkinInfo1,EquipedList2,EquipedList1}
	end.
        
%% 决赛多局获胜制度,这里走特殊流程 
%% 决赛只有一组, 这里用组2作为获胜次数缓存
do_stage_final(#state{status=?STATUS_FINALS_FINAL, slave_server_list=SlaveList, guess_list=GuessList, cur_round=CurRound, next_status_time_stamp=NextTimeStamp}=State) ->
    %% 多局获胜制的话,局数必然是个奇数,所以可以用trunc()+1来做ceil
    GameList = data_alien_finals:get({offset_start_time, ?STATUS_FINALS_FINAL}),
    WinNeedTimes = erlang:trunc(erlang:length(GameList) / 2) + 1,
    WinCacheList = erlang:get(?WIN_CACHE_LIST),
    [WinnerID] = proplists:get_value(1, WinCacheList),
    case proplists:get_value(2, WinCacheList) of
        ?undefined ->
            erlang:put(?WIN_CACHE_LIST, [{2, [WinnerID]}]),
            erlang:put(?MATCH_CACHE_LIST, erlang:get({?MATCH_LIST_KEY, ?STATUS_FINALS_FINAL})),
            bc_msg_to_server(SlaveList, {finals_update_status, ?STATUS_FINALS_FINAL, CurRound, NextTimeStamp}),
            State;
        List ->
            NewList = [WinnerID|List],
            WinTimes = lists:foldl(fun(E,Acc) -> 
                                        case E =:= WinnerID of
                                            true ->
                                                Acc + 1;
                                            _ ->
                                                Acc
                                        end
                                    end, 0, NewList),
            case WinTimes >= WinNeedTimes of
                false ->
                    erlang:put(?MATCH_CACHE_LIST, erlang:get({?MATCH_LIST_KEY, ?STATUS_FINALS_FINAL})),
                    erlang:put(?WIN_CACHE_LIST, [{2, NewList}]),
                    bc_msg_to_server(SlaveList, {finals_update_status, ?STATUS_FINALS_FINAL, CurRound, NextTimeStamp}),
                    State;
                _ ->
                    %% 先通知各个子服务器,比赛结束
                    bc_msg_to_server(SlaveList, {finals_update_status, ?STATUS_FINALS_FINAL, 0, NextTimeStamp}),
                    #role_alien_final{serverID=ServerID, roleName=RoleName} = get_role_info(WinnerID),

                    %% 发送冠、亚军邮件(亚军的邮件用半决赛的状态标识)
                    [{_, MatchList}] = erlang:get({?MATCH_LIST_KEY, ?STATUS_FINALS_FINAL}),
                    send_msg_to_slave_sever_id(ServerID, {finals_match_reward, WinnerID, ?STATUS_FINALS_FINAL}),
                    SecondPlace = lists:delete(WinnerID, MatchList),
                    case SecondPlace of
                        %% 冠军是轮空产生的不发邮件
                        [] ->
                            ignore;
                        [SecondPlaceID] ->
                            #role_alien_final{serverID=SPServerID} = get_role_info(SecondPlaceID),
                            send_msg_to_slave_sever_id(SPServerID, {finals_match_reward, SecondPlaceID, ?STATUS_FINALS_SEMIFINALS})
                    end,

                    %% 发送冠军福利(因为合服的原因,这里不能直接用serverID)
                    bc_msg_to_server(SlaveList, {finals_champion_welfare, util:calc_server_id(roleID, WinnerID), RoleName, WinnerID}),  

                    %% 发送竞猜奖励
                    send_guess_reward(WinnerID, GuessList), 

                    %% 将剩余局数和当前轮数都设在为0(在决赛结束后,应该推送0给客户端代表决赛已经结束)
                    %% 在非决赛期间推送left_game给客户端,在决赛期间推送cur_round给客户端,所以这里cur_round也需要为0
                    State#state{left_game=0, cur_round=0, champion_id=WinnerID}
            end        
    end;

%% 其他赛打完发奖励, 并且生成下场比赛数据
do_stage_final(#state{status=Status, next_status_time_stamp=NextTimeStamp, slave_server_list=SlaveList}=State) ->
    %% 发放奖励
    WinCacheList = erlang:get(?WIN_CACHE_LIST),
    RoleList = lists:foldl(fun({_, IDList}, Acc) -> IDList ++ Acc end, [], WinCacheList),
    lists:map(fun(RoleID) ->
                case erlang:get({?ROLE_INFO_KEY, RoleID}) of
                    ?undefined ->
                        ignore;
                    %仅用于适应1.2.5版本升级至1.3.0版本时遗留的旧格式数据，1.3.0升级后此代码无用。
                    #role_alien_final{serverID=ServerID} ->
                        %% 四强赛后,不发送亚军邮件
                        case Status =:= ?STATUS_FINALS_SEMIFINALS of
                            false ->
                                send_msg_to_slave_sever_id(ServerID, {finals_match_reward, RoleID, Status});
                            _ ->
                                ignore
                        end
                end
              end, RoleList),
    %% 生成后续比赛数据
    gen_next_status_match(Status + 1, WinCacheList),
    bc_msg_to_server(SlaveList, {finals_update_status, Status, 0, NextTimeStamp}),
    State#state{left_game=0}.

%% 生成下场比赛的数据
gen_next_status_match(Status, WinCacheList) ->
    %% 打完比赛后,获胜缓存为倒序,这里改为升序,按照顺序分组
    WinCacheList2 = lists:reverse(WinCacheList),
    NewMatchList = do_group_assign(WinCacheList2, 1, []),
    init_match_info(Status, NewMatchList),
    init_match_cache(NewMatchList).

%% 两人一组
do_group_assign([GA,GB|T], ID, Acc) ->
    {_, [RoleID1]} = GA,
    {_, [RoleID2]} = GB,
    do_group_assign(T, ID+1, [{ID, [RoleID1,RoleID2]}|Acc]);

%% 最后一个轮空了
do_group_assign([GA], ID, Acc) ->
    {_, [RoleID1]} = GA,
    lists:reverse([{ID, [RoleID1]}|Acc]);

%% 分配完毕,倒转为升序
do_group_assign([], _, Acc) ->
    lists:reverse(Acc).

%% 初始化比赛数据(对战序列需要是升序, 供查看用)
init_match_info(Status, MatchList) ->
    erlang:put({?MATCH_LIST_KEY, Status}, MatchList),
    erlang:put({?MATCH_RECORD_KEY, Status}, []).

%% 初始化比赛缓存数据(供程序内比赛用)
init_match_cache(MatchList) ->
    erlang:put(?MATCH_CACHE_LIST, MatchList), 
    erlang:put(?WIN_CACHE_LIST, []).

%% 竞猜时间段为小组赛结束 到 四强赛开始   
check_can_guess(#state{status=?STATUS_FINALS_GROUP_MATCH,left_game=0}, RoleID, GuessID) ->
    case erlang:get({?GUESSED_ROLE_ID, RoleID}) of
        ?undefined ->
            Quarterlist = erlang:get({?MATCH_LIST_KEY, ?STATUS_FINALS_QUARTERFINAL}),
            Quarterlist2 = lists:foldl(fun({_GroupID, RoleIDList}, Acc) -> RoleIDList ++ Acc end, [], Quarterlist),
            case lists:member(GuessID,Quarterlist2) of
                 false ->
                      {false, 4};
                 _ ->
                    true
            end;
        _ ->
            {false, 3}
    end;

%% 不在时间段内
check_can_guess(_,_,_) ->
    {false, 2}.

%% 这里用GuessID构造竞猜列表, 存放押了该玩家获胜的所有玩家ID
%% 下注的玩家信息存在进程字段里面,也存一个GuessID冗余信息,用于客户端显示
do_guess_champion(ServerID, RoleID, GuessID, Rank, GuessList) ->
    erlang:put({?GUESSED_ROLE_ID, RoleID}, {ServerID, Rank, GuessID}),
    case lists:keytake(GuessID, 1, GuessList) of
        false ->
            [{GuessID, [RoleID]}|GuessList];
        {value, {GuessID, OldList}, RestList} ->
            [{GuessID, [RoleID|OldList]}|RestList]
    end. 

%% 数据都从data_alien_finals这个配置里面取,减少复杂度 
get_alien_finals_name() ->
    {ServerID, Platform, _IP} = data_alien_distribute:get(finals),
    get_alien_finals_name(ServerID, Platform).

get_alien_finals_name(ServerID, Platform) ->
    erlang:list_to_atom(lists:concat(['alien_finals_', Platform, ServerID])).

get_alien_finals_node() ->
    {ServerID, Platform, IP} = data_alien_distribute:get(finals),
    erlang:list_to_atom(lists:concat([Platform, '_finals_', ServerID, '@', IP])).

%% 取战斗回放
get_replay_record(ReplayUID) ->
    case erlang:get({?REPLAY_RECORD, ReplayUID}) of
        ?undefined ->
            case db_sql:get_fightReplay(ReplayUID) of
                [] ->
                    #sc_fight_request{actionList=[], fighterList=[], result=true};
                Rec ->
                    erlang:put({?REPLAY_RECORD, ReplayUID}, Rec),
                    Rec
            end;
        Cached ->
            Cached
    end.

%% 取参赛名单(冠军数据是特例,决赛打完才会有冠军)
get_match_list(5, _GroupID, #state{status=Status, left_game=LeftGame, champion_id=ChampionId}) ->
    case Status =:= ?STATUS_FINALS_FINAL andalso LeftGame =:= 0 of
        false ->
            [];
        _ ->
            gen_match_info_list([ChampionId])
    end;

get_match_list(Type, GroupID, State) ->
    ViewStatus = proto_type2status(Type), 
    %% 判断查看的比赛名单是否存在, ViewStatus可以和Status相等
    case check_have_match_list(ViewStatus, State) of
        false ->
            [];
        _ ->
            case erlang:get({?MATCH_LIST_KEY, ViewStatus}) of
                ?undefined ->
                    [];
                List ->
                    %% 小组赛时需要用到GroupID
                    case ViewStatus of
                        ?STATUS_FINALS_GROUP_MATCH ->
                            case lists:keyfind(GroupID, 1, List) of
                                false ->
                                    [];
                                {GroupID, MatchList} ->
                                    gen_match_info_list(MatchList)
                            end;
                        _ ->
                            %% 将各个组的名单合并
                            %% 注意这里用foldr保证组内的数据有序性
                            MatchList = lists:foldr(fun({_GroupID, RoleList}, Acc) ->
                                                        RoleList ++ Acc
                                                    end, [], List),
                            %% 重新修正为升序
                            gen_match_info_list(MatchList)
                    end
            end
    end.

%% 协议里面的类型到状态的转换
proto_type2status(Type) ->
    case Type of 
                1 ->
                    ?STATUS_FINALS_GROUP_MATCH;
                2 ->
                    ?STATUS_FINALS_QUARTERFINAL;
                3 ->
                    ?STATUS_FINALS_SEMIFINALS;
                4 -> 
                    ?STATUS_FINALS_FINAL;
                _ ->
                    10000 
    end.

%% 生成列表后,做下倒排,保证数据的有序性
gen_match_info_list(MatchList) ->
    lists:reverse(lists:foldl(fun(RoleID,Acc) ->
                    case erlang:get({?ROLE_INFO_KEY, RoleID}) of
                        ?undefined ->
                            Acc;
                        RoleInfo ->
                            #role_alien_final{roleID=RoleID, roleName=RoleName, fightPower=FightPower, level=RoleLevel, isMale=IsMale,title=Title,head=Head,vip=Vip}=RoleInfo,
                            [#p_alien_finals_role_info{role_id=RoleID, role_name=RoleName, fight_power=FightPower, role_level=RoleLevel, is_male=IsMale,title=Title,head=Head,vip=Vip}|Acc]
                    end
            end, [], MatchList)).

%% 获得战报
get_record_list(Type, GroupID, State) ->
    ViewStatus = proto_type2status(Type),
    case check_have_records(ViewStatus, State) of
        false ->
            [];
        _ ->
            case erlang:get({?MATCH_RECORD_KEY, ViewStatus}) of
                ?undefined ->
                    [];
                List ->
                    case lists:keyfind(GroupID, 1, List) of
                        false ->
                            [];
                        %% RoundList是降序
                        {GroupID, RoundList} ->
                            %% 修正为升序
                            lists:foldl(fun({RoundID, RecordList}, Acc) ->
                                            [#p_alien_finals_round_record{round=RoundID, records=lists:reverse(RecordList)}|Acc]
                                        end, [], RoundList)

                    end 
            end
    end.

%% 判断是否有对应状态的比赛名单
%% 查看的状态小于等于当前状态,则肯定有对战列表
%% 查看的是下个状态时,如果当前状态的比赛已经打完了,则也会有下个状态的对战列表
check_have_match_list(ViewStatus, #state{status=Status, left_game=LeftGame}) ->
    case ViewStatus of
        X when(X =:= Status + 1) ->
            LeftGame =:= 0;
        X when (X =< Status) ->
            true;
        _ ->
            false
    end.

%% 判断能否查看这个状态的战报
check_have_records(ViewStatus, #state{status=Status,left_game=LeftGame}) ->
    case ViewStatus of
        X when(X > Status) ->
            false;
        X when(X =:= Status) ->
            %% 没打完前是没有战报的
            case LeftGame of
                0 ->
                    true;
                _ ->
                    %% 决赛是个特例,要打多局,每局打完后会有较长的间隔,这个时候需要让玩家看到战报
                    ViewStatus =:= ?STATUS_FINALS_FINAL 
            end;
        _ ->
            true
    end.

is_persist({{?ROLE_INFO_KEY, _}, _}) ->
    true;
is_persist({?MATCH_CACHE_LIST, _}) ->
    true;
is_persist({?WIN_CACHE_LIST, _}) ->
    true;
is_persist({?SIGN_CACHE_LIST, _}) ->
    true;
is_persist({{?MATCH_LIST_KEY, _}, _}) ->
    true;
is_persist({{?MATCH_RECORD_KEY, _}, _}) ->
    true;
is_persist({{?GUESSED_ROLE_ID, _}, _}) ->
    true;
is_persist({{?REPLAY_RECORD, _}, _}) ->
    true;
is_persist(_) ->
    false.

%% 发送竞猜福利
%% 为了避免网络拥堵,需要将在同一个服务器上的玩家都放在一个列表中,
%% 这样只需要给每个服务器发送一条消息即刻。其次为了节省网络流量,在给
%% 每个发送的消息中,将押中和没押中的分别放在两个列表中。因此数据的格式
%% 为:[{ServerID, {押中的列表,没有押中的列表}}*]
send_guess_reward(ChampionId, GuessList) ->
    %% 获得需要发送的数据
    MsgList = 
        lists:foldl(fun({GuessID, List}, Acc) ->
                            IsHit = ChampionId =:= GuessID,
                            SrcServerID = util:calc_server_id(roleID, GuessID),
                            #role_alien_final{roleName=GuessName} = get_role_info(GuessID),
                            lists:foldl(fun(RoleID, Acc2) ->
                                            case erlang:get({?GUESSED_ROLE_ID, RoleID}) of
                                                ?undefined ->
                                                    Acc2;
                                                {ServerID, Rank, _} ->
                                                    case lists:keytake(ServerID, 1, Acc2) of
                                                        false ->
                                                            case IsHit of
                                                                true ->
                                                                    [{ServerID, {[{RoleID, SrcServerID, GuessName, Rank}],[]}} | Acc2];
                                                                _ ->
                                                                    [{ServerID, {[],[{RoleID, SrcServerID, GuessName, Rank}]}} | Acc2]
                                                            end;
                                                        {value, {ServerID, {HitList, MissList}}, RestList} ->
                                                            case IsHit of
                                                                true ->
                                                                    [{ServerID, {[{RoleID, SrcServerID, GuessName, Rank}|HitList], MissList}} | RestList];
                                                                _ ->
                                                                    [{ServerID, {HitList, [{RoleID, SrcServerID, GuessName, Rank}|MissList]}} | RestList]
                                                            end
                                                    end
                                            end
                                        end, Acc, List)
                            end, [], GuessList),
    
    %% 发送消息
    case MsgList of
        [] ->
            ignore;
        _ ->
            lists:foreach(fun({ServerID, MsgBody}) -> send_msg_to_slave_sever_id(ServerID, {finals_guess_reward, MsgBody}) end, MsgList)
    end.

%%------------------------------------------------------------------------------
%%管理命令
set_next_timestamp(NextTimeStamp) ->
    erlang:send(?MODULE, {set_next_timestamp, NextTimeStamp}).

test_reload_fighterInfo(RoleID)->
    erlang:send(?MODULE,{reload_fighter_info,RoleID}).
