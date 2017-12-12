-module(trumpet_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-compile(export_all).

-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-define(DUMP_INTERVAL, 300 * 1000).     %写数据库的间隔,单位：秒
-define(RETRY_INTERVAL, 300 * 1000).    %%两次重新发送的时间间隔

-record(state, {message_queue=[], local_talk_data=[], cross_talk_data=[], wait_lock=false}).

start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
        {?MODULE,
            {?MODULE, start_link, []},
            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    random:seed(util:gen_random_seed()),
    process_flag(trap_exit,true),
    NewState = 
        case db_sql:get_etc(?DB_ETC_KEY_CROSS_TALK) of
            [{state, State}] ->
                update_trumpet_message_info(State); 
            _ ->
                #state{message_queue=queue:new()}
    end,
    erlang:send_after(?DUMP_INTERVAL, self(), dump_tick),
    erlang:send_after(data_trumpet:get({trumpet_push_interval, ?LOCAL_TRUMPET}), erlang:self(), send_data),
    {ok, NewState}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

%% 策划需求第一条喇叭,不需要等待,通过wait_lock来控制,这样要通用些
do_handle_info({local_trumpet, BonusP, Data2}, #state{message_queue=Queue, wait_lock=WaitLock}=State) ->
    %% 红包需要在这儿生成一份红包数据, 如果在role_trumpet中生成的话,可能会导致数据库线程
    %% 被玩家server大量占用的情况
    Data = 
        case BonusP of
            false ->
                BonusID = 0,
                Data2;
            _ ->
                BonusID = role_trumpet:gen_new_bonus(?LOCAL_TRUMPET),
                Data2#sc_trumpet_message_info{bonus_id=BonusID}
        end,
    case WaitLock of
        true ->
            NewQueue = queue:in(Data, Queue),
            NewState = State#state{message_queue=NewQueue};
        _ ->
            broadcast_server:bc(Data),
            NewState = State#state{wait_lock=true, local_talk_data=make_local_talk_data(Data, State)}
    end,
    #sc_trumpet_message_info{roleID=RoleID} = Data,
    ?unicast(RoleID, #sc_trumpet_message{result=1, type=?LOCAL_TRUMPET, bonus_id=BonusID}),
    %% 红包不触发喇叭成就,然后最好还是在发送成功后触发成就
    case BonusP of
        false ->
            catch(role_task:send_dispach(RoleID, {dispach_task, use_local_trumpet}));
        _ ->
            ignore
    end,
    {noreply, NewState};

do_handle_info({client_msg, RoleID, #cs_trumpet_recent_list{type=Type}}, #state{local_talk_data=List, cross_talk_data=CrossList}=State)->
    case Type of
        ?LOCAL_TRUMPET ->
            ?unicast(RoleID, #sc_trumpet_recent_list{list=lists:reverse(List), type=?LOCAL_TRUMPET});
        ?CROSS_TRUMPET ->
            case CrossList of
                [] ->
                    ServerID = data_setting:get(server_id),
					send_msg:direct_by_name(family_fight_server:get_master_server_name(), cross_talk_server, {get_recent_talk_data,ServerID,RoleID}); 
                _ ->
                    ?unicast(RoleID, #sc_trumpet_recent_list{list=lists:reverse(CrossList), type=?CROSS_TRUMPET})
            end;
        _ ->
            ignore
    end,
    {noreply, State};

do_handle_info(dump_tick, State)->
    do_persist(State),
    erlang:send_after(?DUMP_INTERVAL, self(), dump_tick),
    {noreply, State};

do_handle_info(send_data, State) ->
    NewState = do_send(State),
    erlang:send_after(data_trumpet:get({trumpet_push_interval, ?LOCAL_TRUMPET}), erlang:self(), send_data),
    {noreply, NewState};

%% 发送跨服聊天成功(暂时不会有失败的情况出现)
do_handle_info({talk_message_return, RoleID, BonusID}, State) ->
    ?unicast(RoleID, #sc_trumpet_message{result=1, type=?CROSS_TRUMPET, bonus_id=BonusID}),
    %% 红包不触发喇叭成就
    case BonusID =:= 0 of
        true ->
            catch(role_task:send_dispach(RoleID, {dispach_task, use_cross_trumpet}));
        _ ->
            ignore
    end,
    {noreply, State};

%% 广播跨服聊天消息
do_handle_info({bc_cross_msg, Msg}, #state{cross_talk_data=CrossTalkData}=State) ->
    broadcast_server:bc(Msg),
    NewState = State#state{cross_talk_data=cross_talk_server:make_recent_talk_data(Msg, CrossTalkData)},
    {noreply, NewState};

%% 拆红包
do_handle_info({get_bonus, ServerID, RoleID, RoleName, BonusID}, State) ->
    BonusInfo = db_sql:get_bonusInfo(BonusID),
    case role_trumpet:get_bonus(ServerID, RoleID, RoleName, BonusInfo, ?LOCAL_TRUMPET) of
        {false, Reason} ->
            get_bonus_failed(RoleID, ?LOCAL_TRUMPET, BonusID, Reason, BonusInfo);
        {true, Amount, NewBonusInfo} ->
            db_sql:set_bonusInfo(BonusID, NewBonusInfo),
            get_bonus_success(RoleID, ?LOCAL_TRUMPET, BonusID, Amount, NewBonusInfo)
    end,
    {noreply, State};

%% 大红包领取失败
do_handle_info({get_cross_bonus_failed, RoleID, BonusID, Reason, BonusInfo}, State) ->
    get_bonus_failed(RoleID, ?CROSS_TRUMPET, BonusID, Reason, BonusInfo),
    {noreply, State};

%% 大红包领取成功
do_handle_info({get_cross_bonus_success, RoleID, BonusID, Amount, NewBonusInfo}, State) -> 
    get_bonus_success(RoleID, ?CROSS_TRUMPET, BonusID, Amount, NewBonusInfo),
    {noreply, State};

do_handle_info({return_recent_talk_data, RoleID, RecordList}, State) ->
    ?unicast(RoleID, #sc_trumpet_recent_list{list=lists:reverse(RecordList), type=?CROSS_TRUMPET}),
    NewState = State#state{cross_talk_data=RecordList},
    {noreply, NewState}.

terminate(_Reason, State) ->
    do_persist(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_persist(State) ->
    Info = {state, State},
    db_sql:set_etc(?DB_ETC_KEY_CROSS_TALK, [Info]).

do_send(#state{message_queue=Queue}=State) ->
    case queue:is_empty(Queue) of
        true ->
            State;
        _ ->
            {{value, Msg}, NewQueue} = queue:out(Queue),
            broadcast_server:bc(Msg),
            State#state{message_queue=NewQueue, local_talk_data=make_local_talk_data(Msg, State)}
    end.

make_local_talk_data(Msg, #state{local_talk_data=RecordList}) ->
    NewRewardList = [Msg|RecordList],
    Length = erlang:length(NewRewardList),
    Limit = data_trumpet:get({trumpet_log_limit, ?LOCAL_TRUMPET}), 
    case Length > Limit of
        false ->
            NewRewardList;
        _ ->
            lists:sublist(NewRewardList, Limit)
    end.

%% 通知拆红包失败
get_bonus_failed(RoleID, Type, BonusID, Reason, BonusInfo) ->
    ?unicast(RoleID, #sc_trumpet_get_bonus_failed{bonus_type=Type, bonus_id=BonusID, reason=Reason, info=BonusInfo}).

%% 拆红包成功
get_bonus_success(RoleID, Type, BonusID, Amount, BonusInfo) ->
    role_lib:send_server(RoleID, {get_bonus_success, Type, BonusID, Amount, BonusInfo}).

%% 数据升级
update_trumpet_message_info(#state{message_queue=MQ, local_talk_data=LTD, cross_talk_data=CTD}=State) ->
    MQList = queue:to_list(MQ),
    MQ2 = role_trumpet:update_trumpet_message_info(MQList),
    LTD2 = role_trumpet:update_trumpet_message_info(LTD),
    CTD2 = role_trumpet:update_trumpet_message_info(CTD),
    State#state{message_queue=queue:from_list(MQ2), local_talk_data=LTD2, cross_talk_data=CTD2}.
    
