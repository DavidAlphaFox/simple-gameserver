-module(panic_buy_server).

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

-record(state, {}).

-define(dump_interval, 300 ).     %%写数据库的时间间隔
-define(check_interval, 300 ).    %%两次重新发送的时间间隔
-define(tryConnect,tryConnect).
-define(check_tick,check_tick).
-define(dump_tick,dump_tick).
-define(panic_buy_info,panic_buy_info).
-define(panic_buy_once_result,panic_buy_once_result).
-define(panic_buy_request,panic_buy_request).
-define(timeout,5000).

-define(activityover,2).          %%返回给panic_buy_server活动结束
-define(out_of_buy_total_time,3).       %%抢购次数超额
-define(request_success,1).             %%一次抢购成功
-define(out_of_buy_person_time,4).      %%抢购个人次数超限

-define(ETS_PANICBUY_ACTIVITY_INFO,ets_panicbuy_activity_info).
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

%%保存全区抢购活动的配置信息
% -record(panic_buy_activity_info,{activityID=0,activityServerList=[],beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0}).

init([]) ->
    process_flag(trap_exit,true),
    % 订阅节点状态变化信息
    ets:new(?ETS_PANICBUY_ACTIVITY_INFO, [{keypos, 2}, set, public, named_table]),
    Panic_Buy_Activity_Info = db_sql:get_etc(?DB_ETC_KEY_PANIC_BUY_ACTIVITY_INFO),
    ?INFO("从数据库中读出PanicBuyActivityInfo:~w ~n",[Panic_Buy_Activity_Info]),
    NewPanic_Buy_Activity_Info = add_panic_buy_info_from_config(Panic_Buy_Activity_Info,false),
    ?INFO("NewPanicBuyActivityInfo:~w ~n",[NewPanic_Buy_Activity_Info]),
    
    ets:insert(?ETS_PANICBUY_ACTIVITY_INFO,NewPanic_Buy_Activity_Info),
    erlang:send(self(), ?check_tick),
    dump_tick(),
	erlang:send(self(), do_hibernate),
    {ok, #state{}}.




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
	erlang:send_after(600000, self(),do_hibernate),
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
    do_persist(),
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

do_handle_info(?check_tick,State)->
    check_tick(),
    {noreply,State};

do_handle_info(?dump_tick,State)->
    dump_tick(),
    {noreply,State};

do_handle_info({?panic_buy_info,PanicBuyActivityInfo},State)->
    ?INFO("get PanicBuyActivityInfo: ~w ~n",[PanicBuyActivityInfo]),
    ets:insert(?ETS_PANICBUY_ACTIVITY_INFO,PanicBuyActivityInfo),
    {noreply,State};

do_handle_info({?panic_buy_once_result,RoleID,ID,Result,PanicBuyActivityInfo},State)->
    ?INFO("请求一次抢购返回数据：RoleID:~w ID: ~w Result:~w PanicBuyActivityInfo:~w ~n",[RoleID,ID,Result,PanicBuyActivityInfo]),
    case Result of
        ?activityover ->
            ?INFO("抢购结束~n");
        ?out_of_buy_total_time ->
            ?INFO("抢购次数超过总数~n");
        ?out_of_buy_person_time->
            ?INFO("抢购超过个人最大抢购次数");
        ?request_success ->
            ?INFO("抢购成功")
    end,
    role_lib:send_server(RoleID,{panic_buy_response,{ID,Result,PanicBuyActivityInfo}}),
    {noreply,State};

do_handle_info({?panic_buy_request,RoleID,ID},State)->
    ?INFO("收到玩家RoleID:~w 的全区抢购活动请求 ID:~w ~n",[RoleID,ID]),
    request_panic_buy_once(RoleID,ID),
    {noreply,State};

do_handle_info({update_panic_buy_info,NewPanicBuyInfo},State)->
    update_panic_buy_info(NewPanicBuyInfo),
    {noreply,State};

do_handle_info({decreaseopen,ID},State)->
    ?INFO("全区抢购活动ID: ~w 次数衰减开始~n",[ID]),
    add_ref(ID),
    panicbuydecrease(ID),
    {noreply,State};

do_handle_info({decreaseclose,ID},State)->
    ?INFO("全区抢购活动ID: ~w 次数衰减关闭~n",[ID]),
    case delete_ref(ID) of
        {false,?undefined} ->
            ?INFO("查找不到Ref：ID：~w~n",[ID]);
        {true,NewList} ->
            ?INFO("已经删除ID:~w NewList:~w ~n",[ID,NewList])
    end,
    {noreply,State};

do_handle_info({decrease,ID},State)->
    ?INFO("接收到再次衰减请求：~w ~n",[ID]),
    panicbuydecrease(ID),
    {noreply,State};

do_handle_info({reload_config,NeedSweep},State)->
    reload_config(NeedSweep),
    {noreply,State};

do_handle_info(Info,State)->
    ?INFO("undefined Info: ~w  State:~w ~n",[Info,State]),
    {noreply,State}.

check_tick()->
    do_tick(),
    erlang:send_after(?check_interval * 1000, self(), ?check_tick).

dump_tick()->
    do_persist(),
    erlang:send_after(?dump_interval * 1000, self(), ?dump_tick).

%%持续化全区抢购数据
do_persist()->
    db_sql:set_etc(?DB_ETC_KEY_PANIC_BUY_ACTIVITY_INFO,ets:tab2list(?ETS_PANICBUY_ACTIVITY_INFO)),
    ok.

%%定期处理
do_tick()->
    request_panic_buy_info(),
    ok.

send_msg_to_master_server(Msg)->
    send_msg:direct_by_name(all_panic_master_server, all_panic_buy_server, Msg).

%%向全区抢购服务器发送一次请求抢购的消息，并返回最新的全区抢购数据信息
request_panic_buy_once(RoleID,ID)->
    %% 取消向跨服服务器发送抢购信息
    % send_msg_to_master_server_with_timeout({request_panic_buy_once,RoleID,ID}).
    {PanicBuyActivityInfo,Other} = get_panic_buy_info_unit_by_id(ID),
    case PanicBuyActivityInfo of 
        ?undefined->
            Result=?activityover;
        _ ->            
            NowTimeStamp = util:now(),
            case NowTimeStamp >= PanicBuyActivityInfo#panic_buy_activity_info.beginTime andalso NowTimeStamp =< PanicBuyActivityInfo#panic_buy_activity_info.endTime of
                true ->
                    case PanicBuyActivityInfo#panic_buy_activity_info.totalBuyTime >0 of
                        true ->
                            Result = ?request_success,
                            NewPanicBuyActivityInfo = PanicBuyActivityInfo#panic_buy_activity_info{totalBuyTime=PanicBuyActivityInfo#panic_buy_activity_info.totalBuyTime-1},
                            ets:insert(?ETS_PANICBUY_ACTIVITY_INFO,NewPanicBuyActivityInfo);
                        false ->
                            Result = ?out_of_buy_total_time
                    end;
                false ->
                    Result = ?activityover
            end
    end,
    role_lib:send_server(RoleID,{panic_buy_response,{ID,Result,get_panicbuy_info()}}).
    
%%向全区抢购服务器发送请求
send_msg_to_master_server_with_timeout(Msg)->
    ServerID = data_setting:get(server_id),
    send_msg_to_master_server({Msg,ServerID}).

request_panic_buy_info()->
    send_msg_to_master_server_with_timeout(get_panic_buy_info).

get_panicbuy_info()->
    ets:tab2list(?ETS_PANICBUY_ACTIVITY_INFO).

%%从配置文件中加载新的全区抢购活动信息
%-record(panic_buy_activity_info,{activityID=0,beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0,nextdecreasetime=0,decreasedtime=0}).
%{data_panic_buy,{101,{reward,0,0,0,[{item,20011,7,1,0},{item,20028,1,1,0},{item,30005,99,1,0}],[]},{reward,0,0,0,[{item,20011,7,1,0},{item,20028,1,1,0},{item,30005,99,1,0}],[]},{{2014,1,1},{0,0,0}},{{2016,1,1},{0,0,0}},20,500,20,5,5,10}}.
add_panic_buy_info_from_config(Panic_Buy_Activity_Info,NeedSweep)->
    NewPanicBuyActivityInfo1 = 
    case NeedSweep of
        true ->
           [];
        false->
            Panic_Buy_Activity_Info
    end,
    Config = data_panic_buy:get(data_panic_buy),
    PanicBuyInfoList = lists:foldl(fun(ConfigUnit,Acc)->
        {ID,_NeedConfig,_RewardConfig,BeginTime,EndTime,PersonBuyTime,TotalBuyTime,DecreaseInterval,_DecreasePerTime,_DecreaseTime,_DecreaseLimit} = ConfigUnit,
        PanicBuyInfo = #panic_buy_activity_info{activityID=ID,beginTime=util:datetime_to_seconds(BeginTime),endTime=util:datetime_to_seconds(EndTime),personBuyTime=PersonBuyTime,totalBuyTime=TotalBuyTime,nextdecreasetime=util:now()+DecreaseInterval,decreasedtime=0},
        [PanicBuyInfo|Acc]
    end,[],Config),
    % PanicBuyAllInfo = [#panic_buy_all_info{decreaseconfig=#panic_buy_decrease{nexttick=util:now()+DecreaseInterval},panic_buy_activity_info=#panic_buy_activity_info{activityID=ID,activityServerList=ServerList,beginTime=util:datetime_to_seconds(BeginTime),endTime=util:datetime_to_seconds(EndTime),personBuyTime=PersonBuyTime,totalBuyTime=TotalBuyTime}}||{ID,ServerList,NeedConfig,RewardConfig,BeginTime,EndTime,PersonBuyTime,TotalBuyTime,DecreaseInterval,DecreasePerTime,DecreaseTime,_DecreaseLimit}<-Config],
    ?INFO("PanicBuyInfoList:~w ~n",[PanicBuyInfoList]),
     merge_panic_buy_activity_info(NewPanicBuyActivityInfo1,PanicBuyInfoList).

%%合并panic_buy_activity_info,若两者具有相同ID，直接返回前者，若两者ID不同，直接返回后者
merge_panic_buy_activity_info(PanicBuyActivityInfo1,PanicBuyActivityInfo2)->
    EffectList = lists:foldl(fun(ConfigUnit,Acc)->
        case lists:keyfind(ConfigUnit#panic_buy_activity_info.activityID,#panic_buy_activity_info.activityID,PanicBuyActivityInfo2) of
            false ->
                Acc;
            FindOne->
                [ConfigUnit|Acc]
        end
    end,[],PanicBuyActivityInfo1),
    ?INFO("旧的在新的配置表中的配置活动：~w~n",[EffectList]),
    NewConfigList = lists:foldl(fun(ConfigUnit,Acc)->
        case lists:keyfind(ConfigUnit#panic_buy_activity_info.activityID,#panic_buy_activity_info.activityID,EffectList) of
            false ->
                [ConfigUnit|Acc];
            _FindOne->
                Acc
        end
    end,[],PanicBuyActivityInfo2),
    ?INFO("新增加的配置项：~w ~n",[NewConfigList]),

    %%删除所有时间过期的活动配置
    CurrentTimeStamp = util:now(),
    ResultList = lists:foldl(fun(ConfigUnit,Acc)->
        case ConfigUnit#panic_buy_activity_info.endTime >= CurrentTimeStamp of
            true->
                [ConfigUnit|Acc];
            false->
                Acc
        end
    end,[],EffectList++NewConfigList),
    ResultList.


get_panic_buy_info_unit_by_id(ID)->
    PanicBuyActivityInfo = get_panicbuy_info(),
    case lists:keytake(ID,#panic_buy_activity_info.activityID,PanicBuyActivityInfo) of
        false ->
            ?INFO("未能找到全区抢购活动配置数据：ID:~w PanicBuyAllInfo:~w ~n",[ID,PanicBuyActivityInfo]),
            {?undefined,PanicBuyActivityInfo};
        {_,FindOne,Other}->
            {FindOne,Other}
    end.


%%向所有在线玩家广播全区抢购活动
update_panic_buy_info(NewPanicBuyInfo)->
    %%更新panic_buy_server的ets表中的数据
    ets:delete_all_objects(?ETS_PANICBUY_ACTIVITY_INFO),
    ets:insert(?ETS_PANICBUY_ACTIVITY_INFO,NewPanicBuyInfo),
    RoleIDList = lists:foldl(fun({E,_}, Acc)->
                                   [E|Acc]
                           end, [], ets:tab2list(?ETS_ROLE_ONLINE)),
    Msg = {update_panic_buy_info,NewPanicBuyInfo},
    lists:foreach(fun(RoleID) ->spawn(fun()-> catch role_lib:send_server(RoleID,Msg) end) end, RoleIDList).


add_ref(ID)->
    RefList = 
    case get(currentdecreaseref) of
        ?undefined->
            [];
        List ->
            List
    end,
    NewRefList = 
    case lists:member(ID,RefList) of
        false ->
            [ID|RefList];
        true ->
            ?INFO("出现添加相同id：~w 的ref~n",[ID]),
            RefList
    end,
    put(currentdecreaseref,NewRefList).

delete_ref(ID)->
    case get(currentdecreaseref) of
        ?undefined->
            ?INFO("查找ref ID：~w 出现undefined~n",[ID]),
            {false,?undefined};
        RefList->
            case lists:member(ID,RefList) of
                false ->
                    ?INFO("不能查找到ref ID：~w RefList:~w ~n",[ID,RefList]),
                    {false,RefList};
                true->
                    ?INFO("RefList: ~w ~n",[RefList]),
                    put(currentdecreaseref,RefList--[ID]),
                    {true,get(currentdecreaseref)}
            end
    end.

%%定期衰减全区抢购次数
%%保存全区抢购活动的配置信息
%-record(panic_buy_activity_info,{activityID=0,activityServerList=[],beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0,nextdecreasetime=0,decreasedtime=0}).
panicbuydecrease(ID)->
    case lists:member(ID,get(currentdecreaseref)) of 
        true ->            
            {Panic_Buy_Activity_Info,Other} = get_panic_buy_info_unit_by_id(ID),
            case Panic_Buy_Activity_Info of
                ?undefined->
                    ignore;
                _ ->
                    delete_ref(ID),        
                    ?INFO("衰减前：~w ~n",[Panic_Buy_Activity_Info]),
                    {_ID,_NeedConfig,_RewardConfig,_BeginTime,_EndTime,_PersonBuyTime,_TotalBuyTime,DecreaseInterval,DecreasePerTime,_DecreaseTime,DecreaseLimit} = get_panicbuy_info_by_id(ID),
                    if
                        Panic_Buy_Activity_Info#panic_buy_activity_info.totalBuyTime >= DecreasePerTime+ DecreaseLimit ->
                            NewNextDecreaseTime=util:now()+DecreaseInterval,
                            NewDecreasedTime = Panic_Buy_Activity_Info#panic_buy_activity_info.decreasedtime+1,
                            NewTotalBuyTimes = Panic_Buy_Activity_Info#panic_buy_activity_info.totalBuyTime-DecreasePerTime,
                    
                            NewPanic_Buy_Activity_Info = Panic_Buy_Activity_Info#panic_buy_activity_info{decreasedtime=NewDecreasedTime,nextdecreasetime=NewNextDecreaseTime,totalBuyTime=NewTotalBuyTimes},
                            % set_panic_buy_info([NewPanic_Buy_Activity_Info|Other]),
                            ets:insert(?ETS_PANICBUY_ACTIVITY_INFO,NewPanic_Buy_Activity_Info),
                            ?INFO("nextdecreasetime:~w ~n",[NewNextDecreaseTime]),
                            send_decrease_info(NewNextDecreaseTime,ID),
                            % Ref = timer_wheel:add_plan(NewNextDecreaseTime, fun ?MODULE:panicbuydecrease/0),
                            add_ref(ID);
                        Panic_Buy_Activity_Info#panic_buy_activity_info.totalBuyTime > DecreaseLimit->
                            NewNextDecreaseTime=util:now()+DecreaseInterval,
                            NewDecreasedTime = Panic_Buy_Activity_Info#panic_buy_activity_info.decreasedtime+1,
                            NewTotalBuyTimes = DecreaseLimit,

                            NewPanic_Buy_Activity_Info = Panic_Buy_Activity_Info#panic_buy_activity_info{decreasedtime=NewDecreasedTime,nextdecreasetime=NewNextDecreaseTime,totalBuyTime=NewTotalBuyTimes},
                            ets:insert(?ETS_PANICBUY_ACTIVITY_INFO,NewPanic_Buy_Activity_Info),
                            ?INFO("衰减到下限:~w~n",[NewPanic_Buy_Activity_Info]);
                        true ->
                            ignore
                    end
            end,
            ?INFO("衰减后：~w ~n",[get_panicbuy_info()]);
            % update_panic_buy_info();
        false ->
            ?INFO("删除了ID：~w 之后取消了衰减,剩余的reflist:~w ~n",[ID,get(currentdecreaseref)]),
            ignore
    end.    

%%根据ID在配置表中查找对应的配置项
get_panicbuy_info_by_id(ConfigID)->
  PanicBuyInfoList = data_panic_buy:get(data_panic_buy),
  FindList = [{ID,NeedConfig,RewardConfig,BeginTime,EndTime,PersonBuyTime,TotalBuyTime,DecreaseInterval,DecreasePerTime,DecreaseTime,DecreaseLimit}||{ID,NeedConfig,RewardConfig,BeginTime,EndTime,PersonBuyTime,TotalBuyTime,DecreaseInterval,DecreasePerTime,DecreaseTime,DecreaseLimit}<-PanicBuyInfoList,ID=:=ConfigID],
  if
      length(FindList) =:=1 ->
        hd(FindList);
      length(FindList) =:=0 ->
        ?INFO("出现无法找到的配置：ID:~w 配置列表：~w ~n",[ConfigID,PanicBuyInfoList]),
        ?undefined;
      length(FindList) >1 ->
        ?INFO("出现相同配置编号：ID：~w 配置列表：~w ~n",[ConfigID,PanicBuyInfoList]),
        hd(FindList);
      true ->
        ?INFO("出现啥子坑爹问题了：ID:~w 配置列表：~w ~n",[ConfigID,PanicBuyInfoList]),
        ?undefined
  end. 

send_decrease_info(NewNextDecreaseTime,ID)->
    CurrentTimeStamp = util:now(),
    Interval = NewNextDecreaseTime - CurrentTimeStamp,
    ?INFO("Interval:~w ~n",[Interval]),
    NewInterval = Interval*1000,
    ?INFO("NewInterval:~w ~n",[NewInterval]),
    case Interval >0 of
        true ->
            erlang:send_after(NewInterval, self(), {decrease,ID});
        false->
            ignore
    end. 


reload_config(NeedSweep)->
    PanicBuyActivityInfo = ets:tab2list(?ETS_PANICBUY_ACTIVITY_INFO),
    NewPanicBuyActivityInfo = add_panic_buy_info_from_config(PanicBuyActivityInfo,NeedSweep),
    update_panic_buy_info(NewPanicBuyActivityInfo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%重新加载配置文件
test_reload_config(NeedSweep)->
    erlang:send(?MODULE,{reload_config,NeedSweep}).

% %%启动次数衰减
test_decrease_open(ID)->
    erlang:send(?MODULE,{decreaseopen,ID}).    

%%关闭次数衰减
test_decrease_close(ID)->
    erlang:send(?MODULE,{decreaseclose,ID}). 