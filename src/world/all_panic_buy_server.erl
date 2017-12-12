-module(all_panic_buy_server).

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
-define(panic_buy_activity_info,panic_buy_activity_info).
-define(activityover,2).          %%返回给panic_buy_server活动结束
-define(out_of_buy_total_time,3).       %%抢购次数超额
-define(request_success,1).             %%一次抢购成功
-define(out_of_buy_person_time,4).      %%抢购个人次数超限

-record(decreaseRef,{activityid=0,ref=0}).
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
    Panic_Buy_Activity_Info = db_sql:get_etc(?DB_ETC_KEY_PANIC_BUY_ACTIVITY_INFO),
    ?INFO("从数据库中读出PanicBuyActivityInfo:~w ~n",[Panic_Buy_Activity_Info]),
    NewPanic_Buy_Activity_Info = add_panic_buy_info_from_config(Panic_Buy_Activity_Info,false),
    ?INFO("NewPanicBuyActivityInfo:~w ~n",[NewPanic_Buy_Activity_Info]),
    set_panic_buy_info(NewPanic_Buy_Activity_Info),
    timer_wheel:init(),
    erlang:send(self(), check_tick),
    erlang:send(self(), dump_tick),
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
	erlang:send_after(?dump_interval * 1000, self(),do_hibernate),
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
            ?ERR("Exception:~p ~n Info:~w~n State:~w~n", [Exeption, Info, State]),
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

do_handle_info(check_tick,State)->
    check_tick(),
    {noreply,State};

do_handle_info(dump_tick,State)->
    dump_tick(),
    {noreply,State};

do_handle_info({decrease,ID},State)->
    ?INFO("接收到再次衰减请求：~w ~n",[ID]),
    panicbuydecrease(ID),
    {noreply,State};

do_handle_info({get_panic_buy_info,ServerID},State)->
    PanicBuyAllInfo = get_panic_buy_info(),
    Msg = {panic_buy_info,PanicBuyAllInfo},
    send_msg:direct(ServerID,panic_buy_server,Msg),
    {noreply,State};    

do_handle_info({reload_config,NeedSweep},State)->
    reload_config(NeedSweep),
    {noreply,State};

%%处理全区抢购一次请求
% -record(panic_buy_activity_info,{activityID=0,activityServerList=[],beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0}).
do_handle_info({{request_panic_buy_once,RoleID,ID},ServerID},State)->
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
                            set_panic_buy_info([NewPanicBuyActivityInfo|Other]);
                        false ->
                            Result = ?out_of_buy_total_time
                    end;
                false ->
                    Result = ?activityover
            end
    end,
    Msg = {panic_buy_once_result,RoleID,ID,Result,get_panic_buy_info()},
    send_msg_to_server(ServerID,Msg),
    update_panic_buy_info(),
    {noreply,State};

do_handle_info({timer_wheel_tick, LastTick}, State) ->
    timer_wheel:work(LastTick),
    {noreply, State};

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

do_handle_info(Info,State)->
    ?INFO("undefined Info: ~p  State:~w ~n",[Info,State]),
    {noreply,State}.

check_tick()->
    do_tick(),
    erlang:send_after(?check_interval * 1000, self(), check_tick).

dump_tick()->
    do_persist(),
    erlang:send_after(?dump_interval * 1000, self(), dump_tick).

%%持续化全区抢购数据
do_persist()->
    PanicBuyActivityInfo = get_panic_buy_info(),
    ?INFO("持久化数据：~w~n",[PanicBuyActivityInfo]),
    db_sql:set_etc(?DB_ETC_KEY_PANIC_BUY_ACTIVITY_INFO, PanicBuyActivityInfo).

%%定期处理
do_tick()->
    PanicBuyActivityInfo = get_panic_buy_info(),
    CurrentTimeStamp = util:now(),
    NewPanicBuyActivityInfo = 
    lists:foldl(fun(Unit,Acc)->
        case Unit#panic_buy_activity_info.endTime >= CurrentTimeStamp of
            true->
                [Unit|Acc];
            false->
                Acc
        end
    end,[],PanicBuyActivityInfo),
    set_panic_buy_info(NewPanicBuyActivityInfo).

%%保存全区抢购数据到进程字典中
set_panic_buy_info(PanicBuyActivityInfo)->
    put(?panic_buy_activity_info,PanicBuyActivityInfo).

%%从进程字典中获取全区抢购数据
get_panic_buy_info()->
    get(?panic_buy_activity_info).

send_msg_to_server(ServerID,Msg) when is_integer(ServerID) ->
    send_msg:direct(ServerID, panic_buy_server, Msg).

%%从配置文件中加载新的全区抢购活动信息
%-record(panic_buy_activity_info,{activityID=0,activityServerList=[],beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0,nextdecreasetime=0,decreasedtime=0}).
%{data_panic_buy,{101,[30],{reward,0,0,0,[{item,20011,7,1,0},{item,20028,1,1,0},{item,30005,99,1,0}],[]},{reward,0,0,0,[{item,20011,7,1,0},{item,20028,1,1,0},{item,30005,99,1,0}],[]},{{2014,1,1},{0,0,0}},{{2016,1,1},{0,0,0}},20,500,20,5,5,10}}.
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
                [FindOne|Acc]
        end
    end,[],PanicBuyActivityInfo1),
    ?INFO("旧的有效的配置活动：~w~n",[EffectList]),
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
    ResultList2 = lists:foldl(fun(ConfigUnit,Acc)->
        case ConfigUnit#panic_buy_activity_info.nextdecreasetime >= CurrentTimeStamp of
                true->
                    [ConfigUnit|Acc];
                false -> 
                    [ConfigUnit#panic_buy_activity_info{nextdecreasetime = util:now()+20}|Acc]
        end
    end,[],ResultList),
    ?INFO("最终整合得到的配置信息：~w ~n",[ResultList2]),
    ResultList2.

reload_config(NeedSweep)->
    PanicBuyActivityInfo = get_panic_buy_info(),
    NewPanicBuyActivityInfo = add_panic_buy_info_from_config(PanicBuyActivityInfo,NeedSweep),
    set_panic_buy_info(NewPanicBuyActivityInfo),
    update_panic_buy_info().

%%向所有的游戏服发出更新通知
update_panic_buy_info()->
    PanicBuyAllInfo = get_panic_buy_info(),
    UpdateMsg = {update_panic_buy_info,PanicBuyAllInfo},
    send_msg:broadcast(panic_buy_server, UpdateMsg).

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
                {_ID,_ServerList,_NeedConfig,_RewardConfig,_BeginTime,_EndTime,_PersonBuyTime,_TotalBuyTime,DecreaseInterval,DecreasePerTime,_DecreaseTime,DecreaseLimit} = get_panicbuy_info_by_id(ID),
                if
                    Panic_Buy_Activity_Info#panic_buy_activity_info.totalBuyTime >= DecreasePerTime+ DecreaseLimit ->
                        NewNextDecreaseTime=util:now()+DecreaseInterval,
                        NewDecreasedTime = Panic_Buy_Activity_Info#panic_buy_activity_info.decreasedtime+1,
                        NewTotalBuyTimes = Panic_Buy_Activity_Info#panic_buy_activity_info.totalBuyTime-DecreasePerTime,
                    
                        NewPanic_Buy_Activity_Info = Panic_Buy_Activity_Info#panic_buy_activity_info{decreasedtime=NewDecreasedTime,nextdecreasetime=NewNextDecreaseTime,totalBuyTime=NewTotalBuyTimes},
                        set_panic_buy_info([NewPanic_Buy_Activity_Info|Other]),
                        send_decrease_info(NewNextDecreaseTime,ID),
                        % Ref = timer_wheel:add_plan(NewNextDecreaseTime, fun ?MODULE:panicbuydecrease/0),
                        add_ref(ID);
                    Panic_Buy_Activity_Info#panic_buy_activity_info.totalBuyTime > DecreaseLimit->
                        NewNextDecreaseTime=util:now()+DecreaseInterval,
                        NewDecreasedTime = Panic_Buy_Activity_Info#panic_buy_activity_info.decreasedtime+1,
                        NewTotalBuyTimes = DecreaseLimit,

                        NewPanic_Buy_Activity_Info = Panic_Buy_Activity_Info#panic_buy_activity_info{decreasedtime=NewDecreasedTime,nextdecreasetime=NewNextDecreaseTime,totalBuyTime=NewTotalBuyTimes},
                        set_panic_buy_info([NewPanic_Buy_Activity_Info|Other]),
                        ?INFO("衰减到下限:~w~n",[NewPanic_Buy_Activity_Info]);
                    true ->
                        ignore
                end
            end,
            ?INFO("衰减后：~w ~n",[get_panic_buy_info()]),
            update_panic_buy_info();
        false ->
            ?INFO("删除了ID：~w 之后取消了衰减,剩余的reflist:~w ~n",[ID,get(currentdecreaseref)]),
            ignore
    end.


get_panic_buy_info_unit_by_id(ID)->
    PanicBuyActivityInfo = get_panic_buy_info(),
    case lists:keytake(ID,#panic_buy_activity_info.activityID,PanicBuyActivityInfo) of
        false ->
            ?INFO("未能找到全区抢购活动配置数据：ID:~w PanicBuyAllInfo:~w ~n",[ID,PanicBuyActivityInfo]),
            {?undefined,PanicBuyActivityInfo};
        {_,FindOne,Other}->
            {FindOne,Other}
    end.

%-record(decreaseRef,{activityid=0,ref=0}).
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

get_panicbuy_info_by_id(ConfigID)->
  PanicBuyInfoList = data_panic_buy:get(data_panic_buy),
  FindList = [{ID,NeedConfig,RewardConfig,BeginTime,EndTime,PersonBuyTime,TotalBuyTime,DecreaseInterval,DecreasePerTime,DecreaseTime,DecreaseLimit}||{ID,ServerList,NeedConfig,RewardConfig,BeginTime,EndTime,PersonBuyTime,TotalBuyTime,DecreaseInterval,DecreasePerTime,DecreaseTime,DecreaseLimit}<-PanicBuyInfoList,ID=:=ConfigID],
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
    
