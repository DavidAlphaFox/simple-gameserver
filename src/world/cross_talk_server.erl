-module(cross_talk_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").
-behaviour(gen_server).

%% API
-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {message_queue=[], recent_talk_data=[], retry_queue=[], wait_lock=false}).
%% 待发送的数据在message_queue中,如果第一次发送失败,将会被放入retry_queue中,再次发送失败
%% 将被丢弃.但出现发送失败的可能性很定
-define(DUMP_INTERVAL, 300 * 1000).     %%写数据库的时间间隔
-define(RETRY_INTERVAL, 300 * 1000).    %%两次重新发送的时间间隔
%%%===================================================================
%%% API
%%%===================================================================
start() ->
    {ok, _} = 
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
    NewState = 
        case db_sql:get_etc(?DB_ETC_KEY_CROSS_TALK) of
            [{state, State}] ->
                update_trumpet_message_info(State);
            _ ->
                #state{message_queue=queue:new(), retry_queue=queue:new()}
        end,
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    erlang:send_after(data_trumpet:get({trumpet_push_interval, ?CROSS_TRUMPET}), erlang:self(), send_data),
    erlang:send_after(?RETRY_INTERVAL, erlang:self(), retry_data),
	erlang:send_after(?DUMP_INTERVAL, self(), do_hibernate),
    {ok, NewState}.

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

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
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
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    {noreply, State};

do_handle_info(send_data, State) ->
    NewState = do_send(State),
    erlang:send_after(data_trumpet:get({trumpet_push_interval, ?CROSS_TRUMPET}), erlang:self(), send_data),
    {noreply, NewState};

do_handle_info(retry_data, State) ->
    NewState = do_retry(State),
    erlang:send_after(?RETRY_INTERVAL, erlang:self(), retry_data),
    {noreply, NewState};

%% 策划需求第一条喇叭,不需要等待,通过wait_lock来控制,这样要通用些
do_handle_info({talk_message, {ServerID, RoleID, BonusP, Data2}}, #state{message_queue=Queue, wait_lock=WaitLock, recent_talk_data=RecordList, retry_queue=RetryQueue}=State) ->
    %% 跨服红包需要在这儿生成一份红包数据
    Data = 
        case BonusP of
            false ->
                BonusID = 0,
                Data2;
            _ ->
                BonusID = role_trumpet:gen_new_bonus(?CROSS_TRUMPET),
                Data2#sc_trumpet_message_info{bonus_id=BonusID}
        end,
    case WaitLock of
        true ->
            NewQueue = queue:in(Data, Queue),
            NewState = State#state{message_queue=NewQueue};
        _ ->
            NewState = 
                case bc_msg({bc_cross_msg, Data}) of
                    false ->
                        State#state{wait_lock=true, retry_queue=queue:in(Data, RetryQueue)};
                    _ ->
                        State#state{wait_lock=true, recent_talk_data=make_recent_talk_data(Data, RecordList)}
                end
    end,
    send_msg_to_server(ServerID, {talk_message_return, RoleID, BonusID}),
    {noreply, NewState};

%% 拆红包
do_handle_info({get_bonus, ServerID, RoleID, RoleName, BonusID}, State) ->
    BonusInfo = db_sql:get_bonusInfo(BonusID),
    case role_trumpet:get_bonus(ServerID, RoleID, RoleName, BonusInfo, ?CROSS_TRUMPET) of
        {false, Reason} ->
            send_msg_to_server(ServerID, {get_cross_bonus_failed, RoleID, BonusID, Reason, BonusInfo});
        {true, Amount, NewBonusInfo} ->
            db_sql:set_bonusInfo(BonusID, NewBonusInfo),
            send_msg_to_server(ServerID, {get_cross_bonus_success, RoleID, BonusID, Amount, NewBonusInfo})
    end,
    {noreply, State};

do_handle_info({get_recent_talk_data, ServerID, RoleID}, #state{recent_talk_data=RecordList}=State) ->
    send_msg_to_server(ServerID, {return_recent_talk_data, RoleID, RecordList}),
    {noreply, State}.

do_persist(State) ->
    Info = {state, State},
    db_sql:set_etc(?DB_ETC_KEY_CROSS_TALK, [Info]).

do_send(#state{message_queue=Queue, retry_queue=RetryQueue, recent_talk_data=RecordList}=State) ->
    case queue:is_empty(Queue) of
        true ->
            State;
        _ ->
            {{value, Msg}, NewQueue} = queue:out(Queue),
            case bc_msg({bc_cross_msg, Msg}) of
                false ->
                    State#state{message_queue=NewQueue, retry_queue=queue:in(Msg, RetryQueue)};
                _ ->
                    State#state{message_queue=NewQueue, recent_talk_data=make_recent_talk_data(Msg, RecordList)}
            end
    end.

do_retry(#state{retry_queue=Queue}=State) ->
    case queue:is_empty(Queue) of
        true ->
            State;
        _ ->
            {{value, Msg}, NewQueue} = queue:out(Queue),
            case bc_msg({bc_cross_msg, Msg}) of
                false ->
                    ?ERR("消息:~w重新发送失败",[Msg]),
                    State;
                _ ->
                    State#state{recent_talk_data=make_recent_talk_data(Msg, State), retry_queue=NewQueue}
            end
    end.

send_msg_to_server(ServerID, Msg) ->
    send_msg:direct(ServerID,trumpet_server, Msg).

bc_msg(Msg) ->
	send_msg:broadcast(trumpet_server, Msg).

make_recent_talk_data(Msg, RecordList) ->
    NewRewardList = [Msg|RecordList],
    Length = erlang:length(NewRewardList),
    Limit = data_trumpet:get({trumpet_log_limit, ?CROSS_TRUMPET}), 
    case Length > Limit of
        false ->
            NewRewardList;
        _ ->
            lists:sublist(NewRewardList, Limit)
    end.

%% 数据升级
update_trumpet_message_info(#state{message_queue=MQ, recent_talk_data=RTD, retry_queue=RQ}=State) ->
    MQList = queue:to_list(MQ),
    RQList = queue:to_list(RQ),
    MQ2 = role_trumpet:update_trumpet_message_info(MQList),
    RQ2 = role_trumpet:update_trumpet_message_info(RQList),
    RTD2 = role_trumpet:update_trumpet_message_info(RTD),
    State#state{message_queue=queue:from_list(MQ2), recent_talk_data=RTD2, retry_queue=queue:from_list(RQ2)}.
