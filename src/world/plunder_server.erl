%% @author lijunfeng
%% @doc 符文碎片争夺战：服务进程
%% Created 2014-11-19

-module(plunder_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_state/0]).

-record(state,{is_open}).
%% change_state 0表示不需要更新mysql，1表示需要更新mysql
%% protectEndTime保护结束时间，是绝对时间，restAttackTimes剩余攻击次数，buyTimes当日购买次数，lastPlunderTimestamp上次战斗的时间戳，用于隔天更新，change_state该数据是否和数据库一直。
%% recoverTimestamp 表示距离上次回复或消费攻击次数的时间。0表示该玩家攻击次数满，不需要回复。 
-record(plunder_role_info,{role_id,protectEndTime,restAttackTimes,buyTimes,lastPlunderTimestamp,change_state,recoverTimestamp}).

%% 作为缓冲，保存该服务器所有碎片信息
-define(robot_list, robot_list).
-define(recover_list,recover_list).
-define(persist_interval_second, 3*60). %% 30s
-define(recover,recover).

%% ===================Spec Begin =========================
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().

-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ===================Spec End   =========================

%% 调试用
get_state() ->
    gen_server:call(?MODULE, get_state).

%% 获得某个符文碎片的数量，可以在其他进程直接调用
get_stonechip_num(RoleID,StonechipTypeID)->
    case data_patch_parts:get(StonechipTypeID) of
        undefined ->
            0;
        {NewProductTypeID,Position}->
            get_stonechip_num(RoleID,NewProductTypeID,Position)
    end.
get_stonechip_num(RoleID,TypeID,Position)->
    case ets:lookup(?ETS_CACHE_STONECHIP_LIST, {RoleID,TypeID}) of
        [StonechipInfo] ->
            erlang:element(Position+1, StonechipInfo);
        _ ->
            0
    end.

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
    %% 数据初始化
    init_ets(),
    State = init_state(),
    put(?robot_list,db_sql:get_roleIDList_Robot()),
    reset_tick_interval(),
    erlang:send_after(1000, self(), check_recover_tick),
	erlang:send_after(?persist_interval_second * 1000, self(), do_hibernate),
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(test_show_robot, _From, State) ->
    {reply, get(?robot_list), State};
handle_call({test_get_target_list,StoneType,Position}, _From, State) ->
    {reply, get_RoleInfoList_byStonechipType(StoneType,Position,State#state.is_open), State};
handle_call(Request, _From, State) ->
    ?ERR("handle_call function clause:request=~w",[Request]),
    Reply = ok,
    {reply, Reply, State}.
%% 测试用方法，发送符文碎片
handle_cast({test_generate_random_stonechip,RoleID}, State) ->
    do_test_generate_random_stonechip(RoleID),
    {noreply, State};
%% 测试用方法，清除碎片
handle_cast({test_clean_stonechip,RoleID}, State) ->
    do_test_clean_stonechip(RoleID),
    {noreply, State};
handle_cast({test_add_msg,RoleID}, State) ->
    add_recover_check(RoleID,util:now()+30),
    {noreply, State};
handle_cast(Msg, State) ->
    ?ERR("handle_cast function clause:request=~w",[Msg]),
    {noreply, State}.

handle_info(do_hibernate,State)->
	erlang:send_after(?persist_interval_second * 1000, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};
    
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
%% 请求界面信息
handle_info({request_plunder_info,RoleID}, State) ->
    sync_plunder_info(RoleID),
    {noreply, State};
%% 请求界面信息
handle_info({cs_plunder_info,RoleID, _RoleVip, RestProtectTimes}, State=#state{is_open=IsOpen}) ->
    #plunder_role_info{protectEndTime=ProtectEndTime,restAttackTimes=RestAttackTimes,recoverTimestamp=RecoverTimestamp}=get_role_plunder_info(RoleID),
    StonechipStateList = get_p_stonechip_list_byrole(RoleID),
    ReplyMsg = #sc_plunder_info{isOpen=IsOpen,protectEndTime=ProtectEndTime,restAttackTimes=RestAttackTimes
                                ,maxAttackTimes = data_stonechip_fight:get(init_attack_times),nextRestTime = RecoverTimestamp
                                ,restProtectTimes=RestProtectTimes,stonechipStateList=StonechipStateList
                                ,tenfight_unlock_level = data_stonechip_fight:get(ten_fight_need_level)},
    ?INFO("cs_plunder_info 22 ~w ~w",[RoleID, ReplyMsg]),
    ?unicast(RoleID, ReplyMsg),
    {noreply, State};
%% 碎片合成
handle_info({client_msg, RoleID, #cs_plunder_compose{targetTypeID=TargetTypeID}}, State) ->
    case check_plunder_compose(RoleID,TargetTypeID) of 
        {ok,NewStonechipStateList} ->
            NewItem = #new_item{
               itemTypeID=TargetTypeID
               ,itemNum=1
               ,itemLevel=1
               ,itemRank=0},
            case catch role_lib:send_server(RoleID, {plunder_product, NewItem}) of
                {'EXIT', {_, _}} ->
                    %% 发生异常，无法添加合成好的符文，其实这个地方应该用call来做，不确定有什么特殊影响，就暂时不用call了
                    ReplyMsg = #sc_plunder_compose{result=3,newStonechipState=[]};
                _ ->
                    set_stonechip_info(NewStonechipStateList),
                    ReplyMsg = #sc_plunder_compose{result=1,newStonechipState=to_p_stonechip(NewStonechipStateList)}
            end;
        {fail,Reason}->
            ReplyMsg = #sc_plunder_compose{result=Reason,newStonechipState=[]}
    end,
    ?unicast(RoleID, ReplyMsg),
    {noreply, State};
handle_info({client_msg, RoleID, #cs_plunder_multi_compose{targetTypeID=TargetTypeID,num=Num}}, State) ->
    case check_plunder_multi_compose(RoleID,TargetTypeID,Num) of 
        {ok,NewStonechipStateList} ->
            NewItem = #new_item{
               itemTypeID=TargetTypeID
               ,itemNum=1
               ,itemLevel=1
               ,itemRank=0},
            case catch role_lib:send_server(RoleID, {plunder_multi_product, NewItem,Num}) of
                {'EXIT', {_, _}} ->
                    %% 发生异常，无法添加合成好的符文，其实这个地方应该用call来做，不确定有什么特殊影响，就暂时不用call了
                    ReplyMsg = #sc_plunder_multi_compose{result=3,newStonechipState=[]};
                _ ->
                    set_stonechip_info(NewStonechipStateList),
                    ReplyMsg = #sc_plunder_multi_compose{result=1,newStonechipState=to_p_stonechip(NewStonechipStateList)}
            end;
        {fail,Reason}->
            ReplyMsg = #sc_plunder_multi_compose{result=Reason,newStonechipState=[]}
    end,
    ?unicast(RoleID, ReplyMsg),
    {noreply, State};
%% 获取战斗列表 
handle_info({client_msg, RoleID, #cs_plunder_get_target{targetTypeID=TargetTypeID,position=Position}}, State) ->
%%     #plunder_role_info{restAttackTimes=AttackTimes}=get_role_plunder_info(RoleID), %% TODO 这里需要检测是否具有资格获取列表
    NewIsOpen = check_open(),
    TargetList = case catch get_RoleInfoList_byStonechipType(TargetTypeID,Position,State#state.is_open) of
                    List when erlang:is_list(List) ->
                        List;
                    _ ->
                        []
    end,    
    ?unicast(RoleID, #sc_plunder_get_target{result=1,targetList=TargetList}), 
    {noreply, State#state{is_open = NewIsOpen}};
%% 请求夺宝战斗
handle_info({cs_plunder_fight,RoleID,RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID, TargetTypeID,Position,SkinInfo,LegendAddList}, State) ->
    IsOpen = check_open(),
    case check_plunder_fight(RoleID,TarRoleID,TargetTypeID,Position,IsOpen) of
        {fail,2} ->
            #plunder_role_info{buyTimes=BuyTimes} =  get_role_plunder_info(RoleID),
            VipLevel = role_lib:get_vip_level(RoleID),
            VipBuyMax = data_stonechip_fight:get({vip,VipLevel}),
            Replay = #sc_plunder_fight{roleID=TarRoleID,result=2,fightInfo=[],rewardtargetTypeID=0
                                              ,add_times = data_stonechip_fight:get(buy_attack_times)
                                              ,buy_price = data_stonechip_fight:get(buy_attack_price)
                                              ,can_buy=lists:max([VipBuyMax-BuyTimes,0])
                                              ,position = Position},
            ?INFO("cs_plunder_fight 攻击次数不足提示购买 ~w ~w ~w ~w",[BuyTimes,VipLevel,VipBuyMax,Replay]),
            ?unicast(RoleID, Replay);
        {fail,Reason} ->
            FightRecord = #sc_fight_request{actionList=[],fighterList=[],result=true},
            Replay = #sc_plunder_fight{roleID=TarRoleID,result=Reason,fightInfo=[FightRecord],rewardtargetTypeID=0
                                              ,add_times=0,buy_price=0,can_buy=0,position=Position},
            ?unicast(RoleID, Replay);
        ok -> %% NewTarStonechipInfo 是 {RoleID,'_'},'_','_','_','_','_'}
            %% 战斗需要等待role_server返回，此处不能堵塞，所以用新进程处理
            %% TODO 这里需要加入战斗超时的处理
            spawn(fun()->
                      {Result,FightRecord} = do_plunder_fight(RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID,SkinInfo,LegendAddList),
                      FighterList2 = role_data:get_FighterList_with_effect(RoleID,TarRoleID,FightRecord#sc_fight_request.fighterList),
                      FightRecord2 = FightRecord#sc_fight_request{fighterList = FighterList2},
                      erlang:send(plunder_server,{plunder_fight_result,RoleID,TarRoleID,Position,TargetTypeID,Result,FightRecord2})
                  end)
    end,
    {noreply, State#state{is_open=IsOpen}};
%% 接受异步战斗的结果
handle_info({plunder_fight_result,RoleID,TarRoleID,Position,TargetTypeID,Result,FightRecord}, State) ->
    IsDrop = stonechip_random_drop(TarRoleID,TargetTypeID,Position),
    ?INFO("plunder_fight_result ~w ~w",[IsDrop,Result]),
    catch role_lib:send_server(RoleID, {do_mystery_box,stonechip_fight}),
    catch(role_task:send_dispach(RoleID, {dispach_task, plunder_fight})),
    Pub = role_lib:get_rolePublic(RoleID),
    behavior_log_times:log(util:now(),RoleID,Pub#rolePublic.roleName,Pub#rolePublic.level,Pub#rolePublic.fightPower,Pub#rolePublic.viplevel),
    case Result of
        true when IsDrop =:= true-> %% 获胜了，需要检查并更新碎片信息
            NewTarStonechip = try_reduce_stonechip(TarRoleID,TargetTypeID,Position),
            ?INFO("NewTarStonechip:~w",[NewTarStonechip]),
            if
                fail =:= NewTarStonechip ->
                    ?unicast(RoleID, #sc_plunder_fight{roleID=TarRoleID,result=4,fightInfo=[FightRecord],rewardtargetTypeID=0
                                                      ,add_times=0,buy_price=0,can_buy=0,position=Position});
                true ->
                    NewAttackerStonechipInfo = add_stonechip(RoleID,TargetTypeID,Position), %% {RoleID,'_'},'_','_','_','_','_'}
                    ?INFO("获胜后的碎片信息~w",[NewAttackerStonechipInfo]),
                    case NewTarStonechip of
                        robot ->
                            ignore;
                        _ ->
                            set_stonechip_info(NewTarStonechip)
                    end, 
                    set_plunder_info_attacked(RoleID,1),
                    ?unicast(RoleID, #sc_plunder_fight{roleID=TarRoleID,result=1,fightInfo=[FightRecord],rewardtargetTypeID=TargetTypeID
                                                      ,add_times=0,buy_price=0,can_buy=0,position=Position})
            end;
        true ->
            set_plunder_info_attacked(RoleID,1),
            ?unicast(RoleID, #sc_plunder_fight{roleID=TarRoleID,result=1,fightInfo=[FightRecord]
                                              ,rewardtargetTypeID=0,add_times=0,buy_price=0,can_buy=0,position=Position});
        _ -> %% 输了，碎片信息不变化
            set_plunder_info_attacked(RoleID,1),
            ?unicast(RoleID, #sc_plunder_fight{roleID=TarRoleID,result=1,fightInfo=[FightRecord]
                                              ,rewardtargetTypeID=0,add_times=0,buy_price=0,can_buy=0,position=Position})
    end,
    {noreply, State};

%% 请求夺宝战斗,十连夺
handle_info({cs_plunder_fight_ten,RoleID,Level,RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID, TargetTypeID,Position,SkinInfo,LegendAddListA}, State) ->
    IsOpen = check_open(),
    case check_plunder_fight_ten(RoleID,Level,TarRoleID,TargetTypeID,Position,IsOpen) of
        {fail,2} ->
            #plunder_role_info{buyTimes=BuyTimes} =  get_role_plunder_info(RoleID),
            VipLevel = role_lib:get_vip_level(RoleID),
            VipBuyMax = data_stonechip_fight:get({vip,VipLevel}),
            Replay = #sc_plunder_fight_ten{roleID=TarRoleID,result=2,fight_result=[],rewardtargetTypeID=0
                                              ,add_times = data_stonechip_fight:get(buy_attack_times)
                                              ,buy_price = data_stonechip_fight:get(buy_attack_price)
                                              ,can_buy=lists:max([VipBuyMax-BuyTimes,0])
                                              ,position = 0,mystery_list=[]},
            ?INFO("cs_plunder_fight 攻击次数不足提示购买 ~w ~w ~w ~w",[BuyTimes,VipLevel,VipBuyMax,Replay]),
            ?unicast(RoleID, Replay);
        {fail,Reason} ->
            Replay = #sc_plunder_fight_ten{roleID=TarRoleID,result=Reason,fight_result=[],rewardtargetTypeID=0
                                              ,add_times=0,buy_price=0,can_buy=0,position=0,mystery_list=[]},
            ?unicast(RoleID, Replay);
        {ok,CanAttackTime} ->
            spawn(fun()->
                        random:seed(util:gen_random_seed()),
                        do_plunder_fight_ten(RoleID,RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID,Position,TargetTypeID,[],10,false,CanAttackTime,[],SkinInfo,LegendAddListA)
%%                       {Result,FightRecord} = do_plunder_fight(RoleFighterList,RoleLieuAdd,TarRoleID),
%%                       FighterList2 = role_data:get_FighterList_with_effect(RoleID,TarRoleID,FightRecord#sc_fight_request.fighterList),
%%                       FightRecord2 = FightRecord#sc_fight_request{fighterList = FighterList2},
%%                       erlang:send(plunder_server,{plunder_fight_result,RoleID,TarRoleID,Position,TargetTypeID,Result,FightRecord2})
                  end)
    end,
    {noreply, State#state{is_open=IsOpen}};

%% 接受异步战斗的结果,十连夺
handle_info({plunder_fight_ten_result,RoleID,TarRoleID,Position,TargetTypeID,ResultList,Time,IsDrop,CanAttackTime,MysteryReward}, State) ->
    RolePlunderInfo = get_role_plunder_info(RoleID),
    RestAttackTimes = RolePlunderInfo#plunder_role_info.restAttackTimes,
    AttackTime = 10 - Time,
    Replay = if
        RestAttackTimes >= AttackTime ->
            set_plunder_info_attacked(RoleID,AttackTime), %减少攻击次数
            catch role_lib:send_server(RoleID, {do_mystery_box_rewards,MysteryReward}),
            P_reward_viewList = lists:foldr(fun(Reward,Acc)->
                                    [#p_plunder_fight_mystery{reward=role_reward:transform2p_reward_view(Reward, [])}|Acc]
                                end, [], MysteryReward),
            case IsDrop of
                true ->
                    add_stonechip(RoleID,TargetTypeID,Position), %
                    #sc_plunder_fight_ten{roleID=TarRoleID,result=1,fight_result=ResultList,rewardtargetTypeID=TargetTypeID
                                        ,add_times=0,buy_price=0,can_buy=0,position=Position,mystery_list=P_reward_viewList};
                false ->
                    #sc_plunder_fight_ten{roleID=TarRoleID,result=1,fight_result=ResultList,rewardtargetTypeID=0
                                        ,add_times=0,buy_price=0,can_buy=0,position=0,mystery_list=P_reward_viewList}
            end;
        true->
            #sc_plunder_fight_ten{roleID=TarRoleID,result=12,fight_result=[],rewardtargetTypeID=0
                                ,add_times=0,buy_price=0,can_buy=0,position=0,mystery_list=[]}
    end,
    ?unicast(RoleID, Replay),
    {noreply, State};

%% 购买攻击次数
handle_info({do_buy_attacktime,RoleID,VIPlevel}, State) ->
    PlunderInfo = get_role_plunder_info(RoleID),
    NewRestAttackTimes = PlunderInfo#plunder_role_info.restAttackTimes+data_stonechip_fight:get(buy_attack_times),
    NewBuyTimes = PlunderInfo#plunder_role_info.buyTimes+1,
    ets:insert(?ETS_CACHE_ROLE_PLUNDER
               ,PlunderInfo#plunder_role_info{buyTimes=NewBuyTimes
                                             ,restAttackTimes=NewRestAttackTimes
                                             ,change_state=1}),
    behavior_log_times:log(RoleID, VIPlevel,NewBuyTimes, NewRestAttackTimes, data_stonechip_fight:get(buy_attack_times),?MONEY_DEC_TYPE_BUY_ATTACK),
    %% sync_plunder_info(RoleID), role_server自行把buytimes加一
    {noreply, State};
%% 使用保护令牌
handle_info({cs_plunder_use_protect,RoleID}, State) ->
    do_use_protect(RoleID),
    {noreply, State};
%% 增加碎片
handle_info({add_stonechip,RoleID,TypeID,Position}, State) ->
    add_stonechip(RoleID,TypeID,Position),
    {noreply, State};
%% 定周期处理
handle_info(interval, State) ->
    do_persist(),
    reset_tick_interval(),
    {noreply, State#state{is_open=check_open()}};
%% 特殊处理，将全部缓存内容写入数据库
handle_info(fix_stonechip_cache1, State) ->
    fix_stonechip_cache1(),
    {noreply, State};
%% 定时发送来的回复攻击次数的请求
handle_info({recover_attacktime,RoleID}, State) ->
    %%io:format("recover_attacktime ~w \n", [RoleID]),
    case ets:lookup(?ETS_CACHE_ROLE_PLUNDER, RoleID) of        
        [Plunder_role_info] when Plunder_role_info#plunder_role_info.recoverTimestamp > 0 ->
            recover_attacktimes(Plunder_role_info);
        _ ->
            ignore
    end,
    {noreply, State};

handle_info(check_recover_tick,State)->
    erlang:send_after(1000, self(), check_recover_tick),
    do_check_recover(),
    {noreply,State};
%% 保存机器人信息，数据是检索数据库得来的
handle_info(init_robot_data, State) ->
    put(?robot_list,db_sql:get_roleIDList_Robot()),
    {noreply, State};
handle_info(Info, State) ->
    ?ERR("can't handle_info message:~w State:~w",[Info,State]),
    {noreply, State}.

terminate(Reason, State) ->
    do_persist(),
    ?ERR("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 使用了两个ets表格，都是用来缓冲数据的。由于表格owner不是该进程，所以每次启动该进程，需要初始化表格。
init_ets()->
    ?INFO("L-init_ets ----START---- ~w",[now()]),
    ets:delete_all_objects(?ETS_CACHE_STONECHIP_LIST),
    lists:foreach(fun([RoleId,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum])->
                       ets:insert(?ETS_CACHE_STONECHIP_LIST, {{RoleId,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,0})
                end, db_sql:get_stonechip_all()),
    ?INFO("L-init_ets >>>>>END<<<< ~w size:~w mem:~w",[now(),ets:info(?ETS_CACHE_STONECHIP_LIST,size),ets:info(?ETS_CACHE_STONECHIP_LIST,memory)]),
    ets:delete_all_objects(?ETS_CACHE_ROLE_PLUNDER). %% 只是清除一下
    
init_state()->
    #state{is_open=check_open()}.

do_persist() ->
    %%保存两个ets表格中缓存的数据
    persist_stonechip_info(),
    persist_plunder_info().

%% 设置下一次定周期触发
reset_tick_interval()->
    erlang:send_after(?persist_interval_second*1000, self(), interval).

%% 三种情况调用该方法。1、role_server初始化；2、发生攻击buyTimes变化；3、隔日buyTimes刷新
sync_plunder_info(RoleID)->
    case get_plunder_info(RoleID) of
        [#plunder_role_info{buyTimes=BuyTimes}|_] ->
            case catch role_lib:send_server(RoleID, {sync_plunder_info, BuyTimes}) of
                {'EXIT', Error} ->
                    ?INFO("sync_plunder_info for request_plunder_info is fail. maybe role is offline ~w",[Error]);
                _ ->
                    ok
            end;
        _ ->
            ignore
    end.

%% 检查活动是否开启
check_open()->
    NowTime = time(),
    {StartTime,EndTime} = data_stonechip_fight:get(opentime),
    StartTime =< NowTime andalso NowTime =< EndTime.

%% 调用check_recover，如果需要，保存回复后的攻击次数
recover_attacktimes(PlunderInfo)->
    NewPlunderInfo = check_recover(PlunderInfo),
    if
        NewPlunderInfo =/= PlunderInfo ->
            ets:insert(?ETS_CACHE_ROLE_PLUNDER,NewPlunderInfo#plunder_role_info{change_state=1});
        true ->
            igorne
    end.

do_check_recover()->
    Now = util:now(),
    List2 = lists:foldl(fun({RoleID,Ts}=Info,Acc)->
                          if Ts =< Now ->
                                %%io:format("do_check_recover ~p \n",[RoleID]),
                                 erlang:send(self(), {recover_attacktime,RoleID}),
                                    erase({?recover,RoleID}),
                                 Acc;
                             true ->
                                 [Info|Acc]
                          end
                          end, [],get_recover_list()),
    set_recover_list(List2).

add_recover_check(RoleID,NextRecoverTimeStamp)->
    case get({?recover,RoleID}) of
        ?undefined ->
            %%io:format("DO add_recover_check ~w ~w \n",[RoleID,NextRecoverTimeStamp - util:now()]),
            put({?recover,RoleID},1),
            add_role_recover(RoleID,NextRecoverTimeStamp);
        _ ->
            %%io:format("UNDO add_recover_check ~w ~w \n",[RoleID,NextRecoverTimeStamp]),
            ignore
    end.

add_role_recover(RoleID, Next)->
    set_recover_list([{RoleID,Next}|get_recover_list()]).

get_recover_list()->
    case get(?recover_list) of
        X when is_list(X) ->
            X;
        _ ->
            []
    end.

set_recover_list(List) ->
    put(?recover_list,List).

%% 检查一下是不是因为时间的流逝，状态需要更新。
check_refresh_and_save(PlunderRoleInfo)->
    %% 检查是不是跨天了，需要更新攻击次数，购买次数
    NowDate = date(),
    RoleID = PlunderRoleInfo#plunder_role_info.role_id,
    PlunderRoleInfo2 = if
        PlunderRoleInfo#plunder_role_info.lastPlunderTimestamp /= NowDate ->
            sync_plunder_info(RoleID),
            PlunderRoleInfo#plunder_role_info{lastPlunderTimestamp = NowDate,buyTimes=0};
        true ->
            PlunderRoleInfo
    end,
    %% 检查是不是需要更新保护时间
    NowSecond = util:now(),
    PlunderRoleInfo3 = if
        PlunderRoleInfo2#plunder_role_info.protectEndTime =< NowSecond ->
            PlunderRoleInfo2#plunder_role_info{protectEndTime = 0};
        true ->
            PlunderRoleInfo2
    end,
    %% 检查是不是需要回复攻击次数
    MaxAttackTimes = data_stonechip_fight:get(init_attack_times),
    PlunderRoleInfo4 = if 
        PlunderRoleInfo3#plunder_role_info.recoverTimestamp == 0->
            PlunderRoleInfo3;
        PlunderRoleInfo3#plunder_role_info.restAttackTimes >= MaxAttackTimes->
            PlunderRoleInfo3#plunder_role_info{recoverTimestamp=0};
        true ->
            check_recover(PlunderRoleInfo3)
    end,

    FinalPlunderRoleInfo = if
        %% 如果数据变更，或之前的数据需要保存
        PlunderRoleInfo4 =/= PlunderRoleInfo orelse PlunderRoleInfo#plunder_role_info.change_state =:= 1 ->
            PlunderRoleInfo4#plunder_role_info{change_state = 1};
        %% 数据没有变化，但需要将change_state设为0，作为使用标记，放置被淘汰。
        true ->
            PlunderRoleInfo#plunder_role_info{change_state = 0}
    end,
    ets:insert(?ETS_CACHE_ROLE_PLUNDER, FinalPlunderRoleInfo),
    FinalPlunderRoleInfo.

%% 根据时间检查是否回复攻击次数的具体逻辑，包括主动推送消息给客户端
check_recover(PlunderInfo)->
    RoleID = PlunderInfo#plunder_role_info.role_id,
    NowSecond = util:now(),
    OldRecoverTimestamp = PlunderInfo#plunder_role_info.recoverTimestamp,
    Passtime = NowSecond - OldRecoverTimestamp,
    if
        OldRecoverTimestamp =:= 0->
            PlunderInfo;
        Passtime < 0 ->
            %erlang:send_after((OldRecoverTimestamp - NowSecond + 1)*1000,self(),{recover_attacktime,RoleID}),
            add_recover_check(RoleID,OldRecoverTimestamp),
            PlunderInfo;
        true ->
            #rolePublic{familyID=FamilyID} = role_lib:get_rolePublic(RoleID), %由于不能很好的传递familyID到符文争夺世界进程，故在public中添加了FamilyID字段
            FamilyTekPlunderAdd = role_lib:calculate_familyTek_addbuff(FamilyID,6,1),%此处计算对应公会的符文急速科技加成
            AttackTimesRecover2 = data_stonechip_fight:get(attack_times_recover), %来源于配置
            AttackTimesRecover = role_lib:calculate_familyTekeffectTime(AttackTimesRecover2,FamilyTekPlunderAdd),
            MaxRestAttackTimes = data_stonechip_fight:get(init_attack_times),    
            CanRecoverTimes = Passtime div AttackTimesRecover + 1, %当前回复次数
            NewRecoverTimestamp = Passtime rem AttackTimesRecover, %下次回复时间
            if
                %已经回复满的情况
                CanRecoverTimes >= (MaxRestAttackTimes - PlunderInfo#plunder_role_info.restAttackTimes) -> 
                    ?unicast(RoleID,{sc_plunder_notice_attacktime,MaxRestAttackTimes,0}),
                    PlunderInfo#plunder_role_info{restAttackTimes=MaxRestAttackTimes
                                                  ,recoverTimestamp=0
                                                  ,change_state = 1};
                %未回复满的情况
                true ->
                    NewRestAttackTimes = PlunderInfo#plunder_role_info.restAttackTimes + CanRecoverTimes, %玩家新的次数
                    % 下次恢复的相对时间，相当于设定timer ,20m
                    NextTime = if
                                   %刚好此刻回复了1次攻击，下次攻击次数根据配置来设置
                                   NewRecoverTimestamp =:= 0 ->
                                       AttackTimesRecover;
                                   %异常，根据配置来进行
                                   NewRecoverTimestamp < 0 ->
                                       ?ERR("check_recover Time can be less then Zero"),
                                       AttackTimesRecover;
                                   true ->
                                       NewRecoverTimestamp
                               end,
                    % 需要传给客户端的绝对时间 [{2015,1,25}，{15,34,10}]
                    NewRecoverTimestamp2 = OldRecoverTimestamp + CanRecoverTimes*AttackTimesRecover,
                    ?unicast(RoleID,{sc_plunder_notice_attacktime,NewRestAttackTimes,NewRecoverTimestamp2}),
                    %erlang:send_after((NextTime+1)*1000,self(),{recover_attacktime,PlunderInfo#plunder_role_info.role_id}),
                    add_recover_check(RoleID,util:now()+NextTime),
                    PlunderInfo#plunder_role_info{restAttackTimes=NewRestAttackTimes
                                                ,recoverTimestamp=NewRecoverTimestamp2,change_state = 1}
            end
    end.

check_plunder_compose(RoleID,TargetTypeID) ->
    case lists:member(TargetTypeID, data_stonechip_fight:get(fight_stone)) of
        false ->
            ?INFO("数量不足"),
            {fail,5};
        true ->
            case get_stonechip_info_byrole_bytype(RoleID,TargetTypeID) of
                {{_,_},FirstNum,SecondNum,ThirdNum,FourthNum,_}
                        when FirstNum>0 andalso SecondNum>0 andalso ThirdNum>0 andalso FourthNum>0 ->
                    {ok,{{RoleID,TargetTypeID},FirstNum-1,SecondNum-1,ThirdNum-1,FourthNum-1,1}};
                _ ->
                    ?INFO("数量不足"),
                    {fail,2}
            end
    end.

check_plunder_multi_compose(RoleID,TargetTypeID,Num) ->
    case lists:member(TargetTypeID, data_stonechip_fight:get(fight_stone)) of
        false ->
            ?INFO("数量不足"),
            {fail,5};
        true ->
            case get_stonechip_info_byrole_bytype(RoleID,TargetTypeID) of
                {{_,_},FirstNum,SecondNum,ThirdNum,FourthNum,_}
                        when FirstNum>=Num andalso SecondNum>=Num andalso ThirdNum>=Num andalso FourthNum>=Num ->
                    {ok,{{RoleID,TargetTypeID},FirstNum-Num,SecondNum-Num,ThirdNum-Num,FourthNum-Num,1}};
                _ ->
                    ?INFO("数量不足"),
                    {fail,2}
            end
    end.

%% 检查是否具备攻击条件
check_plunder_fight(RoleID,TarRoleID,TargetTypeID,Position,IsOpen)->
    RolePlunderInfo = get_role_plunder_info(RoleID),
    TarRolePlunderInfo = get_role_plunder_info(TarRoleID),
%% ?INFO("check_plunder_fight a ~w ~w ~w ~w",[RolePlunderInfo,TarRolePlunderInfo,RolePlunderInfo#plunder_role_info.restAttackTimes,tk_id:is_robot(TarRoleID)]),
    IsRobot = tk_id:is_robot(TarRoleID),
    NowSecond = util:now(),
    {_,X,Y,Z,W,_} = StonechipInfo = get_stonechip_info_byrole_bytype(RoleID,TargetTypeID),
    IsRobotStone = lists:member(TargetTypeID, data_stonechip_fight:get(fight_stone_just_robot)),
%%     StonechipNum = element(Position+1,StonechipInfo),
    if
        X+Y+Z+W =< 0->
            ?INFO("攻击者没有碎片"),
            {fail,9};
        %% Position参数错误，视作碎片没有了
        Position > 4 orelse Position < 1->
            ?INFO("碎片位置参数错误"),
            {fail,4};
        element(Position+1,StonechipInfo) =/= 0 -> %% 已经有这个碎片了
            ?INFO("已经有这个碎片了 ~w ~w",[TargetTypeID,Position]),
            {fail,8};
        %% 攻击的是非机器人，且不在攻击开启期间
        IsRobot  =:= false andalso IsOpen =:= false ->
            {fail,7};
        RolePlunderInfo#plunder_role_info.restAttackTimes =:= 0 ->
            ?INFO("次数不足？？ ~w",[RolePlunderInfo#plunder_role_info.restAttackTimes]),
            {fail,2};
        IsRobot ->
            ?INFO("攻击了机器人"),
            ok;
        % 不是机器人，且抢夺目标是仅机器人能抢夺的碎片
        IsRobotStone =:= true ->
            {fail,4};
        TarRolePlunderInfo#plunder_role_info.protectEndTime > NowSecond ->
            {fail,3};
        %% 不是机器人，且通过前面的检查
        true ->
            Stonechip = get_stonechip_info_byrole_bytype(TarRoleID,TargetTypeID),
            case element(Position+1,Stonechip) of
                0 -> %% 这块碎片没有，不能抢夺 
                    {fail,4};
                _ ->
                    {_,A,B,C,D,_} = Stonechip,
                    case A+B+C+D of
                        1 -> %% 这个符文对应的碎片总共只有一个,也不能抢夺
                            {fail,4};
                        _ ->
                            ok
                    end
            end
    end.

%% 检查是否具备攻击条件
check_plunder_fight_ten(RoleID,Level,TarRoleID,TargetTypeID,Position,IsOpen)->
    RolePlunderInfo = get_role_plunder_info(RoleID),
    TarRolePlunderInfo = get_role_plunder_info(TarRoleID),
    IsRobot = tk_id:is_robot(TarRoleID),
    NowSecond = util:now(),
    {_,X,Y,Z,W,_} = StonechipInfo = get_stonechip_info_byrole_bytype(RoleID,TargetTypeID),
    NeedLevel = data_stonechip_fight:get(ten_fight_need_level),
    if
        NeedLevel > Level ->
            {fail,10};
        IsRobot =:= false ->
            {fail,11};
        X+Y+Z+W =< 0->
            ?INFO("攻击者没有碎片"),
            {fail,9};
        Position > 4 orelse Position < 1->
            ?INFO("碎片位置参数错误"),
            {fail,4};
        element(Position+1,StonechipInfo) =/= 0 -> %% 已经有这个碎片了
            ?INFO("已经有这个碎片了 ~w ~w",[TargetTypeID,Position]),
            {fail,8};
        RolePlunderInfo#plunder_role_info.restAttackTimes =:= 0 ->
            ?INFO("次数不足？？ ~w",[RolePlunderInfo#plunder_role_info.restAttackTimes]),
            {fail,2};
        true ->
            {ok,RolePlunderInfo#plunder_role_info.restAttackTimes}
    end.

do_plunder_fight(RoleFighterList,RoleLieuAdd,TalentList,TrSpecialA,TarRoleID,SkinInfoA,LegendAddListA)->
    {Result, FightRecord, _State} = case erlang:whereis(role_lib:regName(TarRoleID)) of
        ?undefined ->
            {TarFighterList,TarLieuAdd,TarTalentList,TrSpecialD} = role_data:get_otherRoleFighter(TarRoleID),
            SkinInfoD = role_data:get_otherRoleSkinInfo(TarRoleID),
            TarEquipList = role_data:get_otherRoleItemEquips(TarRoleID),
            %%将数据库中获得的精灵装备列表按照精灵分类
            GerEquipList = role_item:assort_ger_equiplist(TarEquipList),
            TarLegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
            role_fight:new(RoleFighterList, TarFighterList, RoleLieuAdd, TarLieuAdd, TalentList, TarTalentList,TrSpecialA,TrSpecialD,SkinInfoA,SkinInfoD,LegendAddListA,TarLegendAddList);
        _ ->
            Ref = erlang:make_ref(),
            Info = {pvp_attack, RoleFighterList,RoleLieuAdd,TalentList,TrSpecialA,SkinInfoA,LegendAddListA,self(), Ref},
            role_lib:send_server(TarRoleID, Info),
            role_fight:get_result(Ref)
    end,
    {Result,FightRecord}.

do_plunder_fight_ten(RoleID,RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID,Position,TargetTypeID,ResultList,Time,IsDrop,CanAttackTime,MysteryReward,SkinInfo,LegendAddListA) when IsDrop =:= true orelse Time =:= 0 orelse CanAttackTime =:= 0 ->
    erlang:send(plunder_server,{plunder_fight_ten_result,RoleID,TarRoleID,Position,TargetTypeID,ResultList,Time,IsDrop,CanAttackTime,MysteryReward});
do_plunder_fight_ten(RoleID,RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID,Position,TargetTypeID,ResultList,Time,_IsDrop,CanAttackTime,MysteryReward,SkinInfo,LegendAddListA) ->
    {CurResult,_} = do_plunder_fight(RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID,SkinInfo,LegendAddListA),
    catch(role_task:send_dispach(RoleID, {dispach_task, plunder_fight})),
    Reward = lists:merge(role_box:random_reward(data_stonechip_fight:get(extra_reward),1,[])),
    case CurResult of
        true ->
            NewIsDrop = stonechip_random_drop(TarRoleID,TargetTypeID,Position),
            do_plunder_fight_ten(RoleID,RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID,Position,TargetTypeID,[true|ResultList],Time-1,NewIsDrop,CanAttackTime-1,[Reward|MysteryReward],SkinInfo,LegendAddListA);
        false ->
            do_plunder_fight_ten(RoleID,RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,TarRoleID,Position,TargetTypeID,[false|ResultList],Time-1,false,CanAttackTime-1,[Reward|MysteryReward],SkinInfo,LegendAddListA)
    end.

do_use_protect(RoleID)->
    #plunder_role_info{protectEndTime=ProtectEndTime} = PlunderInfo = get_role_plunder_info(RoleID),
    ?INFO("do_use_protect ~w ~w ~w",[RoleID,ProtectEndTime,PlunderInfo]),
    NewProtectEndTime = if
        ProtectEndTime =:=0 ->
            util:now()+data_stonechip_fight:get(onetime_protect);
        true ->
            ProtectEndTime + data_stonechip_fight:get(onetime_protect)
    end,
    ets:insert(?ETS_CACHE_ROLE_PLUNDER, PlunderInfo#plunder_role_info{protectEndTime=NewProtectEndTime,change_state=1}).
    

get_robot_id_list(RobotNum)->
    case get(?robot_list) of
        undefined ->
            [];
        List when length(List) > RobotNum->
            util:random_list(List,RobotNum);
        List when erlang:is_list(List)-> %% 机器人数量少于需要返回的数量
            List;
        _ ->
            []
    end.

get_role_id_list(RoleList,Num)->
    case RoleList of
        undefined ->
            [];
        List when length(List) > Num->
            util:random_list(List,Num);
        List when erlang:is_list(List)-> %% 机器人数量少于需要返回的数量
            List;
        _ ->
            []
    end.

get_role_plunder_info(RoleID)->
    case get_plunder_info(RoleID) of
        [PlunderRoleInfo] ->
            check_refresh_and_save(PlunderRoleInfo);
        [] ->
            case db_sql:get_role_plunder(RoleID) of
                [PlunderRoleInfo2] ->
                    PlunderRoleInfo3 = check_refresh_and_save(PlunderRoleInfo2),
                    %% 从数据库中读取了数据后，需要设定一下恢复攻击次数的时间。
                    case PlunderRoleInfo3#plunder_role_info.recoverTimestamp of
                        RecoverTimestamp when RecoverTimestamp /= 0 ->
%%                             erlang:send_after((RecoverTimestamp rem data_stonechip_fight:get(attack_times_recover))*1000
%%                                              ,self(),{recover_attacktime,PlunderRoleInfo3#plunder_role_info.role_id});
                                add_recover_check(RoleID,RecoverTimestamp);
                        _ ->
                            ignore
                    end,
                    PlunderRoleInfo3;
                [] -> 
                    case db_sql:if_role_exist(RoleID) of
                        true ->
                            %此处相当于初始化玩家的碎片争夺战信息
                            NewPlunderInfo = #plunder_role_info{role_id=RoleID,protectEndTime=0,restAttackTimes=data_stonechip_fight:get(init_attack_times)
                                                                                ,buyTimes=0,lastPlunderTimestamp=date(),change_state=0,recoverTimestamp = 0},
                            ets:insert(?ETS_CACHE_ROLE_PLUNDER, NewPlunderInfo),
                            db_sql:replace_role_plunder(NewPlunderInfo),
                            NewPlunderInfo;
                        false ->
                            ?ERR("get_role_plunder_info RoleID(~w) is not Exist!!",[RoleID]),
                            #plunder_role_info{role_id=0,protectEndTime=0,restAttackTimes=0
                                         ,buyTimes=0,lastPlunderTimestamp=date(),change_state=0,recoverTimestamp = 0}
                    end
            end
    end.
        
get_p_stonechip_list_byrole(RoleID)->
    [{p_stonechip,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum}
        ||{{_,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,_}<-ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{RoleID,'_'},'_','_','_','_','_'})
            ,FirstNum>0 orelse SecondNum>0 orelse ThirdNum>0 orelse FourthNum>0].    

get_stonechip_info_byrole_bytype(RoleID,Type)->
    case ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{RoleID,Type},'_','_','_','_','_'}) of
        [] ->
            ?INFO("数据未查询到1 ~w ~w",[RoleID,Type]),            
            {{RoleID,Type},0,0,0,0,0};
        [T|_] ->
            T
    end.

%% 尝试减少该玩家特定碎片数，如果不可减少，则返回错误，成功，则返回减少后的record
try_reduce_stonechip(RoleID,TypeID,Position)->
    case tk_id:is_robot(RoleID) of
        false ->
                StonechipInfo = get_stonechip_info_byrole_bytype(RoleID,TypeID),
                RestChipNum = erlang:element(Position+1, StonechipInfo),
                if 
                    RestChipNum < 1 ->
                        fail;
                    true ->
                        NewStonechipInfo = setelement(Position+1, StonechipInfo,RestChipNum-1),
                        setelement(6, NewStonechipInfo,1)
                end;
        true ->
            robot
    end.

%% 增加玩家某块碎片，并保存，如果超过100个，则发邮件
add_stonechip(RoleID,TypeID,Position)->
    NewAttackerStonechipInfo0 = get_stonechip_info_byrole_bytype(RoleID,TypeID), %% {RoleID,'_'},'_','_','_','_','_'}
    CurStonechipNum = element(Position+1,NewAttackerStonechipInfo0),
    MaxNum = data_stonechip_fight:get(each_max_num),
    if
        CurStonechipNum < MaxNum ->
            NewAttackerStonechipInfo1 = setelement(Position+1, NewAttackerStonechipInfo0,CurStonechipNum+1);
        %碎片大于100
        true ->
            {{_,_},FirstNum,SecondNum,ThirdNum,FourthNum,_} = NewAttackerStonechipInfo0,
            % 注意这里是 2~5
            RandList = [Index||{Index,Num}<-[{2,FirstNum},{3,SecondNum},{4,ThirdNum},{5,FourthNum}],Num < 100],
            if
                RandList /= [] ->
                    NewPosition2 = util:random_one_from_list(RandList),
                    CurStonechipNum2 = element(NewPosition2,NewAttackerStonechipInfo0),
                    NewAttackerStonechipInfo1 = setelement(NewPosition2, NewAttackerStonechipInfo0,CurStonechipNum2+1);
                true ->
                    NewAttackerStonechipInfo1 = NewAttackerStonechipInfo0
            end
    end,
    NewAttackerStonechipInfo2 = setelement(6, NewAttackerStonechipInfo1,1),
    set_stonechip_info(NewAttackerStonechipInfo2),
    NewAttackerStonechipInfo2.
%%         true ->
%%             ?INFO("碎片超过了100个"),
%%             case data_patch_product:get({TypeID,Position}) of
%%                                         undefined ->
%%                                             ignore;
%%                                         NewProductTypeID->
%%                                             Reward = {sell_reward,0,0,0,0,[{new_item,NewProductTypeID,1,1,0}],0,[]},
%%                                             mail_server:send_sys_mail(RoleID, ?MAIL_PLUNDER_STONECHIP_RECOVER, [], "", Reward)
%%                                     end,
%%             NewAttackerStonechipInfo0
%%     end.
    
%% 进行了一次攻击后，需要变更角色碎片状态信息
%% 攻击机器人不会使保护时间清零
%% 由于是操作ets，所以需要在server进程中调用
set_plunder_info_attacked(RoleID,ReduceNum)->
    sync_plunder_info(RoleID),
    InitAttackTimes = data_stonechip_fight:get(init_attack_times),
    % RecoverAttackTimes2 = data_stonechip_fight:get(attack_times_recover),
    #rolePublic{familyID=FamilyID} = role_lib:get_rolePublic(RoleID), %由于不能很好的传递familyID到符文争夺世界进程，故在public中添加了FamilyID字段
    FamilyTekPlunderAdd = role_lib:calculate_familyTek_addbuff(FamilyID,6,1),%此处计算对应公会的符文急速科技加成
    AttackTimesRecover2 = data_stonechip_fight:get(attack_times_recover), %来源于配置
    RecoverAttackTimes = role_lib:calculate_familyTekeffectTime(AttackTimesRecover2,FamilyTekPlunderAdd),
    IsRobot = tk_id:is_robot(RoleID),
    case get_role_plunder_info(RoleID) of
        %% 攻击机器人，且之前攻击回复时间为0
        {plunder_role_info,RoleID,ProtectEndTime,RestAttackTimes,BuyTimes,LastPlunderTimestamp,_,0}
                when IsRobot andalso RestAttackTimes-ReduceNum < InitAttackTimes ->
            ets:insert(?ETS_CACHE_ROLE_PLUNDER, {plunder_role_info,RoleID,ProtectEndTime,lists:max([RestAttackTimes-ReduceNum,0]),BuyTimes,LastPlunderTimestamp,1
                                                ,util:now()+RecoverAttackTimes}),
%%             erlang:send_after(RecoverAttackTimes*1000,self(),{recover_attacktime,RoleID});
                add_recover_check(RoleID,util:now()+RecoverAttackTimes);
        %% 攻击机器人，且已经处于攻击回复倒计时
        {plunder_role_info,RoleID,ProtectEndTime,RestAttackTimes,BuyTimes,LastPlunderTimestamp,_,RecoverTimestamp} when IsRobot->
            ets:insert(?ETS_CACHE_ROLE_PLUNDER, {plunder_role_info,RoleID,ProtectEndTime,lists:max([RestAttackTimes-ReduceNum,0]),BuyTimes,LastPlunderTimestamp,1
                                                ,RecoverTimestamp});
        %% 攻击玩家，且之前攻击回复时间为0
        {plunder_role_info,RoleID,_,RestAttackTimes,BuyTimes,LastPlunderTimestamp,_,0} 
                when RestAttackTimes-ReduceNum < InitAttackTimes ->
            ets:insert(?ETS_CACHE_ROLE_PLUNDER, {plunder_role_info,RoleID,0,lists:max([RestAttackTimes-ReduceNum,0]),BuyTimes,LastPlunderTimestamp,1
                                                ,util:now()+RecoverAttackTimes}),
%%             erlang:send_after(RecoverAttackTimes*1000,self(),{recover_attacktime,RoleID});
                add_recover_check(RoleID,util:now()+RecoverAttackTimes);
        %% 攻击玩家，且之前攻击回复时间为0
        {plunder_role_info,RoleID,_,RestAttackTimes,BuyTimes,LastPlunderTimestamp,_,RecoverTimestamp} ->
            ets:insert(?ETS_CACHE_ROLE_PLUNDER, {plunder_role_info,RoleID,0,lists:max([RestAttackTimes-ReduceNum,0]),BuyTimes,LastPlunderTimestamp,1
                                                ,RecoverTimestamp});
        E ->
            ?ERR("不应该执行到这里 数据类型错误~w",[E]),
            ingore
    end.

stonechip_random_drop(RoleID,TargetTypeID,Position)->
    {data_drop_stonechip,_,RandomList} = data_drop_stonechip:get(TargetTypeID),
    {RoleArgument,RobotArgument} = lists:nth(Position, RandomList),
    RandomArgument = case tk_id:is_robot(RoleID) of
                         true ->                             
                            RobotArgument;
                         false ->
                            RoleArgument
                     end,
    RandomValue = util:random_int(1, 10000),
    ?INFO("碎片掉落随机结果 ~w ~w >= ~w",[RandomArgument >= RandomValue,RandomArgument,RandomValue]),
    RandomArgument >= RandomValue.    

%% 玩家是不是被保护阶段
is_protect(RoleID)->
    NowSecond = util:now(),
    #plunder_role_info{protectEndTime = ProtectEndTime} = get_role_plunder_info(RoleID),
    ProtectEndTime /= 0 andalso NowSecond >= ProtectEndTime.

%% 从ets中读取
get_plunder_info(RoleID)->
    case ets:lookup(?ETS_CACHE_ROLE_PLUNDER, RoleID) of
        [Plunder_role_info] when Plunder_role_info#plunder_role_info.change_state >= 0 ->
            [Plunder_role_info];
        %% 如果该记录没有变化，则将淘汰计数change_state重置为0，确保在使用的缓存数据不会被淘汰掉
        [Plunder_role_info] when Plunder_role_info#plunder_role_info.change_state < 0->
            ets:update_element(?ETS_CACHE_ROLE_PLUNDER, RoleID, {#plunder_role_info.change_state, 0}),
            [Plunder_role_info];
        _ ->
            []
    end.
    
%% 返回拥有特定碎片的玩家ID可攻击列表,
get_RoleIDList_byStonechipType(StoneType,Position)->
    StonechipList =ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{'_',StoneType},'_','_','_','_','_'}),
    lists:foldl(fun({{RoleID,_},A,B,C,D,_},Acc)->
                         if
                             Position =:= 0 andalso Position > 4 ->
                                 Acc;
                             A =:= 0 andalso Position =:= 1->
                                 Acc;
                             B =:= 0 andalso Position =:= 2->
                                 Acc;
                             C =:= 0 andalso Position =:= 3->
                                 Acc;
                             D =:= 0 andalso Position =:= 4->
                                 Acc;
                             A+B+C+D =< 1 ->
                                 Acc;
                             true ->
                                 [RoleID|Acc]
                         end
    end, [],StonechipList).

%% 返回拥有特定碎片的玩家信息列表，其中包括机器人。
get_RoleInfoList_byStonechipType(StoneType,Position,IsOpen)->
    ?INFO("get_RoleInfoList_byStonechipType ~w",[IsOpen]),
    RealRole=case IsOpen of
               true ->
                    RoleListAll = get_RoleIDList_byStonechipType(StoneType,Position),
                    RoleListAll2 =lists:filter(fun(RoleIDfilter) ->
                                                       is_protect(RoleIDfilter) =:= false
                                               end, RoleListAll),
                    get_role_id_list(RoleListAll2
                                    ,data_stonechip_fight:get(plunder_list_num)-data_stonechip_fight:get(plunder_list_robot));
               false ->
                   []
    end,
%%     RobotList = get_robot_id_list(data_stonechip_fight:get(plunder_list_num)-length(RealRole)),
    RoleIDList = case lists:member(StoneType, data_stonechip_fight:get(fight_stone_just_robot)) of
                     % 如果是特定碎片，只能抢夺机器人的
                     true ->
                        get_robot_id_list(data_stonechip_fight:get(plunder_list_num));
                     false ->
                        RealRole ++ get_robot_id_list(data_stonechip_fight:get(plunder_list_num)-length(RealRole))
                 end,
    ?INFO("get_RoleInfoList_byStonechipType RoleIDList ~w",[RoleIDList]),
    {data_drop_stonechip,_,RandomList} = data_drop_stonechip:get(StoneType),
    {HumanProbability,RobotProbability} = lists:nth(Position, RandomList),    
    RobotIDMax = tk_id:robot_roleID_max(),
    RoleInfoList = lists:foldr(fun(RoleID,Acc)->
                            case role_lib:get_rolePublic(RoleID) of
                                #rolePublic{roleName=RoleName,roleID=RoleID,level=Level,
                                        isMale=IsMale,title=Title,fightPower=FightPower,head=Head}->                                    
                                    Probability = case tk_id:is_robot(RoleID) of
                                                          false ->
                                                              HumanProbability;
                                                          true ->
                                                              RobotProbability
                                                  end,
                                    [{_,_,ProbabilityType}|_] = lists:filter(fun({Min,Max,_})->
                                                                        Min =< Probability andalso Probability =< Max
                                                                    end, data_stonechip_fight:get(probability_type)),
                                    ?INFO("get_RoleInfoList_byStonechipType Role:~w, Probability:~w, ProbabilityType:~w"
                                            ,[RoleID,Probability,ProbabilityType]),
                                    [#p_plunder_tar{roleID=RoleID,isMale=IsMale,level=Level
                                                   ,roleName=RoleName,title=Title,head=Head
                                                   ,fightPower=FightPower,probabilityType=ProbabilityType
                                                   ,isRobot = tk_id:is_robot(RoleID)}|Acc];
                                [] -> %% 特殊情况，数据可能取不到玩家数据
                                    Acc
                            end
                    end, [], RoleIDList),
    lists:sublist(RoleInfoList, data_stonechip_fight:get(plunder_list_num)).

%% 除了初始化之外，修改ETS_STONECHIP_LIST的必经之路
set_stonechip_info(RoleID,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum)->
    ets:insert(?ETS_CACHE_STONECHIP_LIST, {{RoleID,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,1}).
set_stonechip_info({{RoleID,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,_}) ->
    set_stonechip_info(RoleID,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum).

to_p_stonechip({{_,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,_})->
    [{p_stonechip,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum}].

persist_stonechip_info()->
    NeedSaveList = ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{'_','_'},'_','_','_','_',1}),
    ?INFO("persist_stonechip_info NeedSaveList:~w",[NeedSaveList]),
    lists:foreach(fun
                     ({{RoleID,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,_})
                       when FirstNum=:=0 andalso SecondNum=:=0 andalso ThirdNum=:=0 andalso FourthNum=:=0 ->
                          db_sql:del_stonechip(RoleID,StoneType),
                          ets:delete(?ETS_CACHE_STONECHIP_LIST, {RoleID,StoneType});
                     ({{_,_},FirstNum,SecondNum,ThirdNum,FourthNum,_})
                       when FirstNum<0 orelse SecondNum<0 orelse ThirdNum<0 orelse FourthNum<0 ->
                          ?ERR("persist_stonechip_info error:~w",[persist_stonechip_info]);
                     ({{RoleID,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,_}) ->
                          db_sql:replace_stonechip(RoleID,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum),
                          ets:insert(?ETS_CACHE_STONECHIP_LIST, {{RoleID,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,0})
                    end, NeedSaveList).

persist_plunder_info()->
    PlunderInfoCache = ets:match(?ETS_CACHE_ROLE_PLUNDER,'$1'),
    ?INFO("persist_plunder_info PlunderInfoCache:~w",[PlunderInfoCache]),
    lists:foreach(fun([PlunderInfo])->
                         #plunder_role_info{role_id=RoleID,change_state=ChangeState} = PlunderInfo,
                        if
                            %% 需要写入数据库
                            ChangeState =:= 1 ->
                                db_sql:replace_role_plunder(PlunderInfo),
                                ets:update_element(?ETS_CACHE_ROLE_PLUNDER, RoleID, {#plunder_role_info.change_state, 0});
                            %% 长时间不适用的数据，需要从缓存中淘汰
                            ChangeState < -3600 -> %% 数据长时间没有变化，没有读取，从缓存中删除该数据，确保不浪费内存
                                ets:delete(?ETS_CACHE_ROLE_PLUNDER, RoleID);
                            %% 使用中且不需要保存的数据，
                            ChangeState =< 0 ->
                                ets:update_element(?ETS_CACHE_ROLE_PLUNDER, RoleID, {#plunder_role_info.change_state, ChangeState-?persist_interval_second});
                            true ->
                                ignore
                        end
                  end, PlunderInfoCache).


fix_stonechip_cache1()->
    NeedSaveList = ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{'_','_'},'_','_','_','_','_'}),
    NeedDelList = [5051,5052,5053,5054,5055,5056,5057,5058,5031,5032,5033,
                   5034,5035,5036,5037,5038,5039,5040,5041,5042,5043,5044,5045,5046],
    lists:foreach(fun({{RoleID,StoneType},FirstNum,SecondNum,ThirdNum,FourthNum,_}) ->
                          case lists:member(StoneType, NeedDelList) of
                              true ->
                                  db_sql:del_stonechip(RoleID,StoneType),
                                  ets:delete(?ETS_CACHE_STONECHIP_LIST, {RoleID,StoneType});
                              false ->
                                  ignore
                          end
                    end, NeedSaveList).

%%-------------------------For Test-------------------------------------------------------------------------------
test_generate_random_stonechip(RoleID)->
    gen_server:cast(plunder_server,{test_generate_random_stonechip,RoleID}).

test_clean_stonechip(RoleID)->
    gen_server:cast(plunder_server,{test_clean_stonechip,RoleID}).

test_show_ets() ->
    {ets:match(?ETS_CACHE_STONECHIP_LIST,'$1')
    ,ets:match(?ETS_CACHE_ROLE_PLUNDER,'$1')}.

test_show_robot() ->
    gen_server:call(plunder_server,test_show_robot).

test_get_target_list(StoneType,Position) ->
    gen_server:call(plunder_server,{test_get_target_list,StoneType,Position}).

test_show_ets_stonechip_role(RoleID) ->
    ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{RoleID,'_'},'_','_','_','_','_'}).

test_set_attacktime(RoleID,Num) ->
    ets:update_element(?ETS_CACHE_ROLE_PLUNDER, RoleID, {#plunder_role_info.restAttackTimes, Num}),
    ets:update_element(?ETS_CACHE_ROLE_PLUNDER, RoleID, {#plunder_role_info.change_state, 1}).

test_add_msg(RoledID)->
    gen_server:cast(plunder_server,{test_add_msg,RoledID}).

do_test_generate_random_stonechip(RoleID)->
    %% 先获取所有的符文
    {ok,ItemAll}=file:consult("config/data_item.config"),
    ItemIdAll = 
        lists:foldr(fun(Item,Acc) ->
                        case Item of
                            #data_item{itemTypeID=ItemID,itemType=stonephydefbite} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stonephydef} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stonemagdefbite} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stonemagdef} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stone} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stoneessencehp} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stoneessencedr} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stoneessenceab} ->
                                [ItemID|Acc];
                            #data_item{itemTypeID=ItemID,itemType=stoneessencemb} ->
                                [ItemID|Acc];
                            _ ->
                                Acc
                        end
        end,[], ItemAll),
    lists:foreach(fun(StoneType) ->
                          add_stonechip(RoleID,StoneType,util:random_int(1,4))
%%                         test_add_stonechip(RoleID,StoneType,util:random_int(0,3),util:random_int(0,3),util:random_int(0,3),util:random_int(0,3))
                  end, ItemIdAll).

do_test_clean_stonechip(RoleID)->
    lists:foreach(fun({{_,StoneType},_,_,_,_,_})->
%%                     ets:delete(?ETS_CACHE_STONECHIP_LIST, {RoleID,StoneType})
                       ets:insert(?ETS_CACHE_STONECHIP_LIST, {{RoleID,StoneType},0,0,0,0,1})
                end, ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{RoleID,'_'},'_','_','_','_','_'})).

    

%% %% 增加碎片的方法和减少碎片的方法有意分开来写，以便分析bug
%% test_add_stonechip(RoleID,StoneType,FirstNumAdd,SecondNumAdd,ThirdNumAdd,FourthNumAdd) 
%%             when FirstNumAdd>=0 andalso SecondNumAdd>=0 andalso ThirdNumAdd>=0 andalso FourthNumAdd>=0 ->
%%     case ets:match_object(?ETS_CACHE_STONECHIP_LIST, {{RoleID,StoneType},'_','_','_','_','_'}) of
%%         [{{_,_},FirstNum,SecondNum,ThirdNum,FourthNum,_}] ->
%%             set_stonechip_info(RoleID,StoneType,FirstNum+FirstNumAdd,SecondNum+SecondNumAdd,ThirdNum+ThirdNumAdd,FourthNum+FourthNumAdd);
%%         [] ->
%%             set_stonechip_info(RoleID,StoneType,FirstNumAdd,SecondNumAdd,ThirdNumAdd,FourthNumAdd)
%%     end.
