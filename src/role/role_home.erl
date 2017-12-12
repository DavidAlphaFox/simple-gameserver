-module(role_home).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  
-include("def_mail.hrl").  

-export([cs_home_info/1
        ,cs_home_build/1
        ,cs_home_up_stage/1
        ,cs_home_task_info/1
        ,init_home_task/0
%%         ,get_reward_info/1
%%         ,get_reward_itemTypeID/1
        ,cancel_del_ger/1
        ,cacl_win_rate/1]).

-define(pd_task_gerid, pd_task_gerid). % 缓存已经执行任务的gerid，该数据无需保存
%% -define(opt_ts, opt_ts). % 防止频繁操作

-define(op_accpet, 1).  
-define(op_dounty, 2). 
-define(op_cancel, 3). 

-define(cirrus_energy_set, 1).      % 体力挂机  
-define(cirrus_diamond_set, 2).     % 钻石挂机
-define(cirrus_get_reward, 3).      % 收获

-define(cirrus_cost_energy, 1). 
-define(cirrus_cost_diamond, 2).

%% ====================================================================
%% external functions
%% ====================================================================

init_home_task()->
    % 1、初始化基本参数
    Now = util:now(),
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName} = role_data:get_roleInfo(),
    HomeInfo = home_server:get_home(RoleID),
    Park = home_server:get_park_level(RoleID),
    TaskList = home_server:get_home_task(RoleID),
    put(?pd_task_gerid,[]),
    UnacceptTaskTimeout = data_home:get(unaccept_task_refresh_timeout),
    % 2、检查一下是否需要补充新的任务,主要是针对版本更新
    TaskNum = 5 + (Park div 5),
    TaskListLen = erlang:length([E||E<-TaskList,E#home_task.onwer_role_id =:= RoleID 
                                andalso (E#home_task.status =:= ?ht_normal_unaccept 
                                         orelse E#home_task.status =:= ?ht_normal_accepted
                                         orelse E#home_task.status =:= ?ht_dounty_unaccept)]),
    % TaskListLen  = erlang:length(TaskList),
    TimerNum = erlang:length(HomeInfo#home_info.new_task_timer),
    if
        TaskNum > (TaskListLen+TimerNum)->
            #role{level=RoleLevel} = role_data:get_roleInfo(),
            AddTaskList1 = [add_home_task(0,RoleLevel,RoleID,RoleName,HomeInfo#home_info.stage,Park)
                            ||_<-lists:seq(1, TaskNum-(TaskListLen+TimerNum))];
        true ->
            AddTaskList1 = [],
            TaskList
    end,
    % 3、 检查离线期间是否有新增任务。离线完成任务是home_server处理
    {NewTimerList,AddTaskList2}
        = lists:foldl(fun(AddTaskTs,{AccTimer,AccNewTask})->
                        if
                            AddTaskTs > Now ->
                                home_start_timer((AddTaskTs - Now)*1000,{timer_add_new_home_task,AddTaskTs}),
                                {[AddTaskTs|AccTimer],AccNewTask};
                            true ->
                                NewTask = add_home_task(0,RoleLevel,RoleID,RoleName,HomeInfo#home_info.stage,Park),
                                {AccTimer,[NewTask|AccNewTask]}
                        end
                  end, {[],[]}, HomeInfo#home_info.new_task_timer),
    % 4、结合前两部分需要补充的任务，执行补充，并增加刷新任务的timer
    AddTaskList = AddTaskList1++AddTaskList2,
    if
        AddTaskList /= [] ->
            erlang:send(home_server, {add_new_home_task,RoleID,AddTaskList});
        true -> ignore
    end,
    home_server:set_home(RoleID,HomeInfo#home_info{new_task_timer=NewTimerList}),
    % 5、根据汇总的任务列表，启动任务内容刷新timer
    RefreshL = lists:foldl(fun(Task,AccRefresh)->
                    if
                        %需要直接刷新任务类型, 并启动定时器
                        Task#home_task.status =:= ?ht_normal_unaccept
                          andalso Task#home_task.timtout_ts < Now ->
                            NextTime = UnacceptTaskTimeout - ((Now-Task#home_task.timtout_ts) rem UnacceptTaskTimeout),
                            home_start_timer(NextTime * 1000,{refresh_home_task,Task#home_task.id,?ht_normal_unaccept}),
                            NewRefreshTs = Now + NextTime,
                            [add_home_task(Task#home_task.id,RoleLevel,RoleID,RoleName,HomeInfo#home_info.stage,Park,NewRefreshTs)|AccRefresh];
                        %启动定时器
                        Task#home_task.status =:= ?ht_normal_unaccept ->
                            NextTime = Task#home_task.timtout_ts - Now ,
                            home_start_timer(NextTime * 1000,{refresh_home_task,Task#home_task.id,?ht_normal_unaccept}),
                            AccRefresh;
                        (Task#home_task.status =:= ?ht_normal_accepted orelse (Task#home_task.status =:= ?ht_dounty_accepted andalso Task#home_task.role_id =:= RoleID)) 
                          andalso Task#home_task.onwer_role_id =:= RoleID
                          andalso Task#home_task.ger_id /= []->
                            put(?pd_task_gerid,Task#home_task.ger_id++get(?pd_task_gerid)),
                            AccRefresh;
                        true ->
                            AccRefresh
                    end
        end, [],AddTaskList1++AddTaskList2++TaskList),
    % 6、刷新已经到时的timer
    erlang:send(home_server, {refersh_task_type,RoleID,RefreshL,?ht_normal_accepted}),
    ?INFO("--------------HomeInitFinish(~w),add:~w,Timer:~w,Old(~w):~w"
         ,[RoleID,length(AddTaskList),length(NewTimerList),length(TaskList),TaskList]),
    ok.

get_build_type_level(RoleID,Type)->
    HomeInfo = home_server:get_home(RoleID),
    {value,{Type,Level}} = lists:keysearch(Type, 1, HomeInfo#home_info.constr_list),
    Level.

cs_home_info(_)->
    RoleID = role_data:get_roleID(),
    HomeInfo = home_server:get_home(RoleID),
    Stage = HomeInfo#home_info.stage,
    MsgConstrList = lists:foldr(fun({T,L},AccList)-> 
                                        {MaxLevel,_NeedStage,_NeedNum,_NeedItem,_NeedConstr} = data_home:get({c_info,T,L}),
%%                                         NeedItem2 = role_reward:reward_plus_reward(NeedItem,{sell_reward,0,0,0,0,[{new_item,?home_item,NeedNum,1,0}],0,[]}),
                                        CInfo = #c_info{type = T
                                                       ,cur_level = L
                                                       ,max_level = MaxLevel},
                                        [CInfo|AccList] 
                                end, [], HomeInfo#home_info.constr_list),
	?sendself(#sc_home_info{stage=Stage
                           ,constr_list= MsgConstrList
                           ,dounty_need_win_rate=(data_home:get(dounty_need_win_rate) div 10)}).

cs_home_build(#cs_home_build{type=Type})->
    case check_home_build(Type) of
        {ok,NextLevel,NeedNum,{BagItem2,_BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu}} ->
            #role{roleID=RoleID,level=RoleLevel,roleName=RoleName} = RoleInfo = role_data:get_roleInfo(),
            if NeedNum > 0 ->
                   role_lib:deduct_home_resource_f(RoleInfo,NeedNum,?MONEY_DEC_TYPE_HOME_BUILD,Type,"");
               true ->
                   ignore
            end,
            if  NeedGold > 0 ->
                    role_lib:deduct_gold_2_f(role_data:get_roleInfo(),NeedGold,?MONEY_DEC_TYPE_HOME_BUILD,Type,"");
                true ->
                    ignore
            end,
            if NeedCoin > 0 ->
                   role_lib:deduct_coin_f(role_data:get_roleInfo(),NeedCoin,?MONEY_DEC_TYPE_HOME_BUILD,Type,"");
               true ->
                   ignore
            end,
            if NeedRepu > 0 ->
                   role_lib:deduct_reputation_f(role_data:get_roleInfo(),NeedRepu,?MONEY_DEC_TYPE_HOME_BUILD,Type,"");
               true ->
                   ignore
            end,
            {Date, _} = Time = erlang:localtime(),
            if
                DelList /= [] orelse UpdateList /= [] ->
                    LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
                    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_HOME_BUILD, Type, ""),
                    role_data:set_bagItem(BagItem2),
                    DelItemIDList = [E||#item{itemUID=E}<-DelList],
                    UpdateList2 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateList],
                    %% 提醒客户端更新物品
                    ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
                    ?sendself(#sc_item_update{updateList=UpdateList2});
                true ->
                    ignore
            end,
            
            HomeInfo = home_server:get_home(RoleID),
            #home_info{constr_list=ConstrList} = HomeInfo,
            NewConstrList = lists:keyreplace(Type, 1, ConstrList, {Type,NextLevel}),
            home_server:set_home(RoleID,HomeInfo#home_info{constr_list=NewConstrList}),
            if
                Type =:= ?constr_type_park andalso (NextLevel rem 5) =:= 0 ->
                    %% 任务上限增加，追加一条任务
                    Park = home_server:get_park_level(RoleID),
                    NewTask = add_home_task(0,RoleLevel,RoleID,RoleName,HomeInfo#home_info.stage,Park),
                    erlang:send(home_server, {add_new_home_task,RoleID,[NewTask]}),
                    NextTime = NewTask#home_task.timtout_ts - util:now(),
                    home_start_timer(NextTime * 1000,{refresh_home_task,NewTask#home_task.id,?ht_normal_unaccept});
                %%FIX 升级了行政大厅之后，要重算精灵属性
                Type =:= ?constr_type_office ->
                    PosList = role_data:get_posList(),
                    role_data:init_posList(PosList);
                true ->
                    ignore
            end,    
            MsgConstrList = lists:foldr(fun({T,L},AccList)-> 
                                                {MaxLevel,_NeedStage,_NeedNumNext,_NeedItem,_NeedConstr} = data_home:get({c_info,T,L}),
%%                                                 NeedItem2 = role_reward:reward_plus_reward(NeedItem,{sell_reward,0,0,0,0,[{new_item,?home_item,NeedNum,1,0}],0,[]}),
                                                CInfo = #c_info{type = T
                                                               ,cur_level = L
                                                               ,max_level = MaxLevel},
                                                [CInfo|AccList] 
                                        end, [], [{Type,NextLevel}]),
            ?unicast(RoleID,#sc_home_build{result=1,constr_list=MsgConstrList}),
            %%触发对七日任务建筑升级任务的判断
            role_payGuide:trigger_task_change(?BUIDING_X_LEVEL_N,{0});
        {fail,Reason} ->
            ?sendself(#sc_home_build{result=Reason
                                    ,constr_list=[]})
    end.    

cs_home_up_stage(_)->
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName} = RoleInfo =role_data:get_roleInfo(),
    HomeInfo = home_server:get_home(RoleID),
    case check_up_stage(HomeInfo,RoleInfo) of
        {ok,NeedNum} ->
            NewStage = HomeInfo#home_info.stage + 1,
            if NeedNum > 0 ->
                   role_lib:deduct_home_resource_f(RoleInfo,NeedNum,?MONEY_DEC_TYPE_HOME_STAGE_UP,NewStage,"");
               true ->
                   ignore
            end,
            home_server:set_home(RoleID,HomeInfo#home_info{stage=NewStage}),
            ?sendself(#sc_home_up_stage{result=1}),
            MsgConstrList = lists:foldr(fun({T,L},AccList)-> 
                                                {MaxLevel,_NeedStage,_NeedNum,_NeedItem,_NeedConstr} = data_home:get({c_info,T,L}),
%%                                                 NeedItem2 = role_reward:reward_plus_reward(NeedItem,{sell_reward,0,0,0,0,[{new_item,?home_item,NeedNum,1,0}],0,[]}),
                                                CInfo = #c_info{type = T
                                                               ,cur_level = L
                                                               ,max_level = MaxLevel},
                                                [CInfo|AccList] 
                                        end, [], HomeInfo#home_info.constr_list),
            ?sendself(#sc_home_info{stage=NewStage
                                   ,constr_list= MsgConstrList
                                   ,dounty_need_win_rate=(data_home:get(dounty_need_win_rate) div 10)});
        {fail,Reason} ->
            ?sendself(#sc_home_up_stage{result=Reason})
    end.

cs_home_get_friend(#cs_home_get_friend{role_id=FriendId})->
    HomeInfo = home_server:get_home(FriendId),
    Stage = HomeInfo#home_info.stage,
    MsgConstrList = lists:foldr(fun({T,L},AccList)-> 
                                        {MaxLevel,_NeedStage,_NeedNum,_NeedItem,_NeedConstr} = data_home:get({c_info,T,L}),
%%                                         NeedItem2 = role_reward:reward_plus_reward(NeedItem,{sell_reward,0,0,0,0,[{new_item,?home_item,NeedNum,1,0}],0,[]}),
                                        CInfo = #c_info{type = T
                                                       ,cur_level = L
                                                       ,max_level = MaxLevel},
                                        [CInfo|AccList] 
                                end, [], HomeInfo#home_info.constr_list),
    #rolePublic{level=Lv} = role_lib:get_rolePublic(FriendId),
    ?sendself(#sc_home_get_friend{stage=Stage,constr_list= MsgConstrList,level=Lv}).
cs_home_task_info(_)->
    RoleID = role_data:get_roleID(),
    ?sendself(#sc_home_task_info{task_list= home_server:filter_task_type(home_server:get_home_task(RoleID))}).

cs_home_bounty_task_info(_)->
    RoleID = role_data:get_roleID(),
    erlang:send(home_server,{cs_home_bounty_task_info,RoleID}).

cs_home_task_operate(#cs_home_task_operate{operate_type=Type,task_id=TaskId,ger_id=GerIdList,role_id=TaskOwner0})->
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName} = Role = role_data:get_roleInfo(),
    TaskOwner = if TaskOwner0 =:= 0 -> RoleID; true -> TaskOwner0 end,
    HomeInfo = home_server:get_home(RoleID),
    TaskList = home_server:get_home_task(TaskOwner),
    Now = util:now(),
    case Type of
        ?op_accpet ->
            case check_accept(TaskId,GerIdList,TaskList,RoleID,TaskOwner) of
                {ok,Task,ResList1,ResList2,ResList3,ResList4} ->
                    TNum = erlang:length(ResList1),
                    NeedTime = (2+TNum*2+Task#home_task.quality*2) * data_home:get(finish_time_unit), 
                    NextNeedTime = Now+NeedTime,
                    NewTask = Task#home_task{role_id=RoleID
                                            ,timtout_ts=NextNeedTime
                                            ,status=if Task#home_task.status =:= ?ht_normal_unaccept -> ?ht_normal_accepted; 
                                                       Task#home_task.status =:= ?ht_dounty_unaccept -> ?ht_dounty_accepted end
                                            ,ger_id=ResList1
                                            ,ger_type=ResList2
                                            ,ger_quality=ResList3
                                            ,ger_level=ResList4
                                            ,role_level=RoleLevel
                                            ,atk_name=RoleName},
                    WinRate = cacl_win_rate(NewTask),
                    NeedWinRate = data_home:get(dounty_need_win_rate),
                    if
                        WinRate < NeedWinRate andalso Task#home_task.status =:= ?ht_dounty_unaccept ->
                            ?sendself(#sc_home_task_operate{result= 5});
                        true ->
                            NewTaskWait = NeedTime+random:uniform(10000),
                            if
                                Task#home_task.status =:= ?ht_dounty_unaccept ->
                                    case role_lib:is_online(TaskOwner) of
                                        true -> 
                                            catch role_lib:send_server(TaskOwner, {set_new_home_task_timer,NewTaskWait});
                                        false -> 
                                            OwnerHomeInfo = home_server:get_home(TaskOwner),
                                            NewTaskTimer = [NewTaskWait+Now|OwnerHomeInfo#home_info.new_task_timer],
                                            home_server:set_home(TaskOwner, OwnerHomeInfo#home_info{new_task_timer=NewTaskTimer})
                                    end;
                                true ->
                                    home_start_timer(NewTaskWait*1000,{timer_add_new_home_task,NewTaskWait+Now}),
                                    NewTaskTimer = [NewTaskWait+Now|HomeInfo#home_info.new_task_timer],
                                    home_server:set_home(RoleID,HomeInfo#home_info{new_task_timer=NewTaskTimer})
                            end,
                            erlang:send(home_server, {accept_task,RoleID,RoleLevel,NewTask,TaskOwner}),
                            put(?pd_task_gerid,ResList1++get(?pd_task_gerid)),
%%                             put(?opt_ts,util:now()),
                            ?sendself(#sc_home_task_operate{result= 1})
                    end;
                {fail,Reason} ->
                    ?sendself(#sc_home_task_operate{result= Reason})
            end;
        ?op_dounty when TaskOwner =:= RoleID->
            case lists:keytake(TaskId, #home_task.id, TaskList) of
                {value,TaskInfo,_OtherTaskList} when TaskInfo#home_task.status =:= ?ht_normal_unaccept ->
                    HomeItem  = case TaskId rem 2 of
                                    0 -> ?home_stone;
                                    1 -> ?home_wood end,
                    NeedNum = (TaskInfo#home_task.base_reward_num div 2) div ?home_exchange_rate,
                    BagItem = role_data:get_bagItem(),
                    case item_lib:check_need_list(Role, [{item,HomeItem,NeedNum}], BagItem,[]) of
                        {true,{NewBagItem,_Role,_BagEquip, DelItem, UpdateItem, LogItemList, _NeedCoin, _NeedGold, _NeedRepu}} ->
%%                             role_lib:deduct_home_resource_f(Role,NeedNum,?MONEY_DEC_TYPE_HOME_TASK,TaskId,""),
                            {Date, _} = Time = erlang:localtime(),
                            if DelItem == [] andalso UpdateItem == [] ->
                                   ignore;
                               true ->
                                   %% 写道具日志
                                   behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_HOME_TASK, TaskId, ""),
                                   role_data:set_bagItem(NewBagItem),
                                   DelItemIDList = [E||#item{itemUID=E}<-DelItem],
                                   UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem],
                                   ?INFO("cs_home_task_operate ~w",[{{item,HomeItem,NeedNum},UpdateList,UpdateItem}]),
                                   %% 提醒客户端更新物品
                                   ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
                                   ?sendself(#sc_item_update{updateList=UpdateList})
                            end,
                            erlang:send(home_server, {set_bounty,RoleID,TaskId});
                        false ->
                            ?INFO("need res:~w have:~w",[NeedNum,Role#role.home_resource]),
                            ?sendself(#sc_home_task_operate{result= 4})
                    end;
                _ ->
                    ?sendself(#sc_home_task_operate{result= 2})
            end;
        ?op_cancel when TaskOwner =:= RoleID->
            case lists:keytake(TaskId, #home_task.id, TaskList) of
                {value,TaskInfo,_OtherTaskList} when TaskInfo#home_task.status =:= ?ht_normal_accepted ->
                    erlang:send(home_server, {set_cancel,RoleID,TaskId});
                _ ->
                    ?sendself(#sc_home_task_operate{result= 2})
            end;
        _ ->
            ?sendself(#sc_home_task_operate{result= 2})
    end.

%%--------------------------魔藤相关-------------------------------------
cs_home_cirrus_info(#cs_home_cirrus_info{role_id = RoleId})->
    CirrusNodeList = case RoleId of 
                         0 ->
                             get_cirrus_list();
                         _ ->
                             get_friend_cirrus_list(RoleId)
                     end,    
    CirrusLR = lists:foldl(fun(CirrusIndex,AccList)->
            {CirrusLvl,RoleLvl,Diamond,Energy,_} = data_home:get({home_cirrus,CirrusIndex}),
            case lists:keyfind(CirrusIndex, #home_cirrus.index, CirrusNodeList) of
                false ->
                    IsFirst = lists:keymember(0, #cirrus_node.is_win, AccList),
                    IsWin = case IsFirst of true -> 3; false -> 0 end,
                    CirrusNode = #cirrus_node{cirrus_index = CirrusIndex
                                         ,need_role_lvl = RoleLvl
                                         ,need_cirrus_lvl = CirrusLvl
                                         ,is_win = IsWin
                                         ,doing_timestamp=0
                                         ,doing_ger_id=0
                                         ,cost_type=0
                                         ,cirrus_reward=#p_reward_info{coin=0
                                                                      ,roleExp=0
                                                                      ,gerExp=0
                                                                      ,gold=0
                                                                      ,reputation=0
                                                                      ,itemList=[]
                                                                      ,gerList=[]}
                                         ,need_energy = Energy
                                         ,need_diamond = Diamond
                                         ,doing_ger_type= 0
                                         ,doing_ger_quality = 0},
                    [CirrusNode|AccList];
                Cirrus ->
                    EndTs = case Cirrus#home_cirrus.cost_type of
                                ?cirrus_cost_energy ->
                                    Cirrus#home_cirrus.doing_timestamp 
                                    + (data_home:get(cirrus_energy_period)*data_home:get(cirrus_energy_times));
                                ?cirrus_cost_diamond ->
                                    Cirrus#home_cirrus.doing_timestamp 
                                    + (data_home:get(cirrus_diamond_period)*data_home:get(cirrus_diamond_times));
                                _ ->
                                    0
                            end,
                    CirrusNode = #cirrus_node{cirrus_index = CirrusIndex
                                         ,need_role_lvl = RoleLvl
                                         ,need_cirrus_lvl = CirrusLvl
                                         ,is_win = Cirrus#home_cirrus.is_win
                                         ,doing_timestamp=EndTs
                                         ,doing_ger_id=Cirrus#home_cirrus.doing_ger_id
                                         ,cost_type=Cirrus#home_cirrus.cost_type
                                         ,cirrus_reward=activity_server:sell_reward2p_reward_info(Cirrus#home_cirrus.cirrus_reward)
                                         ,need_energy = Energy
                                         ,need_diamond = Diamond
                                         ,doing_ger_type = Cirrus#home_cirrus.doing_ger_type
                                         ,doing_ger_quality = Cirrus#home_cirrus.doing_ger_quality},
                    [CirrusNode|AccList]
            end
        end, [], data_home:get(home_cirrus_id_list)),
    CirrusL = lists:reverse(CirrusLR),
    ?sendself(#sc_home_cirrus_info{role_id = RoleId, cirrus_node_list= CirrusL}).

cs_home_cirrus_fight(#cs_home_cirrus_fight{cirrus_index=CirrusIndex})->
    case lists:member(CirrusIndex, data_home:get(home_cirrus_id_list)) of
        true ->
            #role{roleID=RoleID} = role_data:get_roleInfo(),
            HomeInfo = home_server:get_home(RoleID),
            CirrusNodeList = get_cirrus_list(),
            case lists:keytake(CirrusIndex, #home_cirrus.index, CirrusNodeList) of
                {value, CirrusInfo, OtherCirrusNodeList} when CirrusInfo#home_cirrus.is_win /= 1->
                    ?sendself(#sc_home_cirrus_fight{result= 10,fightInfo=[]});
                {value, CirrusInfo, OtherCirrusNodeList} when CirrusInfo#home_cirrus.is_win =:= 1->
                    ?ERR("cs_home_cirrus_fight can't be here"), %目前的逻辑，不会走到这里，如果没打赢过，就不会保存
                    ?sendself(#sc_home_cirrus_fight{result= 11,fightInfo=[]});
                false -> 
                    CirrusNodeListLength = erlang:length(CirrusNodeList),
                    if
                        CirrusIndex =< 8 andalso 
                          (CirrusIndex =:= CirrusNodeListLength orelse CirrusIndex =:= 8) ->
                            {Result, FightRecord} = cirrus_reward_fight(RoleID,CirrusIndex),
                            IsWin = if Result =:= true ->
                                           NewHomeCirrus = #home_cirrus{index = CirrusIndex
                                                                       ,is_win = 1
                                                                       ,doing_timestamp = 0
                                                                       ,doing_ger_id = 0
                                                                       ,cost_type = 0
                                                                       ,cirrus_reward = #sell_reward{}
                                                                       ,reward_already = 0
                                                                       ,is_get_big = false
                                                                       ,doing_ger_type = 0
                                                                       ,doing_ger_quality = 0},
                                            NewHomeInfo = HomeInfo#home_info{cirrus_list=[NewHomeCirrus|CirrusNodeList]},
                                            home_server:set_home(RoleID, NewHomeInfo),
                                            1;
                                       true -> 
                                            0
                                    end,
                            ?sendself(#sc_home_cirrus_fight{result= IsWin,fightInfo=[FightRecord]});
                        true ->
                            ?sendself(#sc_home_cirrus_fight{result= 10,fightInfo=[]})
                    end     
            end;
        false ->            
            ?sendself(#sc_home_cirrus_fight{result= 10,fightInfo=[]})
    end.

cs_home_cirrus_operate(#cs_home_cirrus_operate{operate_type=OperateType,cirrus_index=CirrusIndex,ger_id=GerId})->
    case lists:member(CirrusIndex, data_home:get(home_cirrus_id_list)) of
        true ->
            #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
            HomeInfo = home_server:get_home(RoleID),
            CirrusNodeList = get_cirrus_list(),
            case lists:keytake(CirrusIndex, #home_cirrus.index, CirrusNodeList) of
                {value, Cirrus, OtherCirrusNodeList} when Cirrus#home_cirrus.is_win =:= 1->
                    if
                        % 开始挂机
                        (OperateType =:= ?cirrus_energy_set orelse OperateType =:= ?cirrus_diamond_set) 
                          andalso Cirrus#home_cirrus.cost_type =:= 0
                          andalso Cirrus#home_cirrus.doing_timestamp =:= 0
                          andalso Cirrus#home_cirrus.doing_ger_id =:= 0 ->
                            case check_cirrus_ger(GerId) of
                                false ->
                                    ?sendself(#sc_home_cirrus_operate{result= 4});
                                {true,GerTypeId,GerQuality} ->                                    
                                    UpGerTypeIDList = [E#home_cirrus.doing_ger_type||E<-CirrusNodeList,E#home_cirrus.doing_ger_id /= 0,E#home_cirrus.doing_ger_type /= 0],
                                    case lists:member(GerTypeId, UpGerTypeIDList) of
                                        true ->
                                            ?sendself(#sc_home_cirrus_operate{result= 4});
                                        false ->
                                            UpGerIDList = [E#home_cirrus.doing_ger_id||E<-CirrusNodeList,E#home_cirrus.doing_ger_id /= 0,E#home_cirrus.doing_ger_type /= 0],
                                            case lists:member(GerId, UpGerIDList) of
                                                true ->
                                                    ?sendself(#sc_home_cirrus_operate{result= 4});
                                                false ->
                                                    case check_cirrus_cost(RoleInfo,OperateType,CirrusIndex) of
                                                        false ->
                                                            ?sendself(#sc_home_cirrus_operate{result= 3});
                                                        true ->
                                                            NewCirrusInfo = Cirrus#home_cirrus{doing_ger_id = GerId
                                                                                              ,doing_timestamp = util:now()
                                                                                              ,cost_type=OperateType
                                                                                              ,cirrus_reward = #sell_reward{} 
                                                                                              ,reward_already = 0 
                                                                                              ,is_get_big = false
                                                                                              ,doing_ger_type = GerTypeId
                                                                                              ,doing_ger_quality = GerQuality},
                                                            NewCirrusNodeList = [NewCirrusInfo|OtherCirrusNodeList],
                                                            NewHomeInfo = HomeInfo#home_info{cirrus_list=NewCirrusNodeList},
                                                            home_server:set_home(RoleID, NewHomeInfo),
                                                            ?sendself(#sc_home_cirrus_operate{result= 1})
                                                    end
                                            end
                                    end
                            end;
                        % 收获
                        OperateType =:= ?cirrus_get_reward 
                          andalso Cirrus#home_cirrus.cost_type > 0
                          andalso Cirrus#home_cirrus.doing_timestamp > 0
                          andalso Cirrus#home_cirrus.doing_ger_id > 0 ->
                            role_reward:handle_sell_reward_f(RoleInfo, Cirrus#home_cirrus.cirrus_reward, ?MONEY_ADD_TYPE_CURRIS, CirrusIndex, ""),
                            NewCirrusInfo = Cirrus#home_cirrus{doing_ger_id = 0
                                                              ,doing_timestamp = 0
                                                              ,cost_type=0
                                                              ,cirrus_reward = #sell_reward{} 
                                                              ,reward_already = 0 
                                                              ,is_get_big = false 
                                                              ,doing_ger_type = 0 
                                                              ,doing_ger_quality = 0},
                            NewCirrusNodeList = [NewCirrusInfo|OtherCirrusNodeList],
                            NewHomeInfo = HomeInfo#home_info{cirrus_list=NewCirrusNodeList},
                            home_server:set_home(RoleID, NewHomeInfo),
                            ?sendself(#sc_home_cirrus_operate{result= 1});
                        true ->
                            ?sendself(#sc_home_cirrus_operate{result= 2})
                    end;
                _ ->
                    ?sendself(#sc_home_cirrus_operate{result= 2})
            end;
        false ->
            ?sendself(#sc_home_cirrus_operate{result= 2})
    end.

cs_home_exchange(#cs_home_exchange{wood_num = WoodNum
                                  ,stone_num = StoneNum})->
    #role{home_resource=HomeResource} = RoleInfo = role_data:get_roleInfo(),
    NeedHomeResource = WoodNum*100 + StoneNum*100,
    if
        HomeResource >= NeedHomeResource -> 
            role_reward:handle_sys_reward(RoleInfo,[{?REWARD_ITEM,20072,WoodNum},{?REWARD_ITEM,20073,StoneNum}]
                                         ,?MONEY_ADD_TYPE_HOME_EXCHANGE, 0, ""),
            role_lib:deduct_home_resource_f(RoleInfo,NeedHomeResource,?MONEY_DEC_TYPE_HOME_EXCHANGE,0,""),
            ?sendself(#sc_home_exchange{result= 1});
        true -> ?sendself(#sc_home_exchange{result= 2})
    end.

do_home_timer_msg({finish_home_task,_TaskId,GerList})->
    ?INFO("finish_home_task(~w)del ger ~w",[_TaskId,GerList]),
    put(?pd_task_gerid,get(?pd_task_gerid)--GerList);
% 未接取任务，刷新任务内容
% 领取任务、悬赏任务、撤销任务的逻辑中，没有删除刷新任务的定时器，所以，任务领取后依然可能触发刷新，需要检查一下任务是不是被领取了
do_home_timer_msg({refresh_home_task,TaskId,TaskStatus})->
    ?INFO("refresh_home_task ~w",[TaskId]),
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName} = role_data:get_roleInfo(),
    TaskList = home_server:get_home_task(RoleID),
    case lists:keysearch(TaskId, #home_task.id, TaskList) of
        {value,TaskInfo} when TaskInfo#home_task.status =:= TaskStatus ->
            Park = home_server:get_park_level(RoleID),
            HomeInfo = home_server:get_home(RoleID),
            RefreshL = [add_home_task(TaskId,RoleLevel,RoleID,RoleName,HomeInfo#home_info.stage,Park)],
            erlang:send(home_server, {refersh_task_type,RoleID,RefreshL,TaskStatus}),
            home_start_timer(data_home:get(unaccept_task_refresh_timeout) * 1000,{refresh_home_task,TaskId,?ht_normal_unaccept});
        _ ->
            ignore
    end;
% _TaskWait 表示添加任务的时间点，由于消息延迟，可能处理该消息时，已经过了这个时间点
% 如果需要严格控制任务刷新时间，可能根据这个_TaskWait修正下一次timer
do_home_timer_msg({timer_add_new_home_task,_TaskWait})->
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName} = role_data:get_roleInfo(),
    Park = home_server:get_park_level(RoleID),
    HomeInfo = home_server:get_home(RoleID),
    NewTask = add_home_task(0,RoleLevel,RoleID,RoleName,HomeInfo#home_info.stage,Park),
    erlang:send(home_server, {add_new_home_task,RoleID,[NewTask]}),
    % 增加刷新任务内容的计时器
    home_start_timer(data_home:get(unaccept_task_refresh_timeout) * 1000,{refresh_home_task,NewTask#home_task.id,?ht_normal_unaccept}).

%% %% 从宝箱奖励中提取类型，数量是实时计算
%% get_reward_itemTypeID(Reward)->
%%     case Reward#p_reward_info.itemList of
%%         [Item]->
%%             Item#p_item_view.itemTypeID;
%%         _ ->
%%             0
%%     end.

%% %% 基础奖励
%% get_reward_info(Num) ->
%%     activity_server:sell_reward2p_reward_info({sell_reward,0,0,0,0,[{new_item,?home_item,Num,1,0}],0,[]}).

cancel_del_ger(GerIdList)->
    put(?pd_task_gerid,get(?pd_task_gerid)--GerIdList).
    
cacl_win_rate(Task)->
    WinRate0 = Task#home_task.role_level + 50 - Task#home_task.level,
    WinRate1 = if WinRate0 < 0 -> 0; true -> WinRate0*10 end, %转化为千分比
    Num = erlang:length(Task#home_task.ger_type),
    % WinRate2 A B C 是千分比
    WinRate2 = lists:foldl(fun(I,Acc)->
                       TT = lists:nth(I, Task#home_task.tgt_ger_type),
                       T = lists:nth(I, Task#home_task.ger_type),
                       Q = lists:nth(I, Task#home_task.ger_quality),
                       GL = lists:nth(I, Task#home_task.ger_level),
                       #data_ger{gerProperty=TgtGerProp} = data_ger:get(TT),
                       #data_ger{gerProperty=AptGerProp} = data_ger:get(T),
                       % A B C 都是千分比
                       A = Q * 5,
                       %max｛0，（精灵等级 + 30 - 任务等级）/目标数量｝
                       B0 = ((GL +50 - Task#home_task.level)*10) / Num,
                       B = if B0 < 0 -> 0; true -> B0 end,
                       C0 = if
                                AptGerProp =<4 andalso TgtGerProp >=5 -> 10;
                                TgtGerProp =<4 andalso AptGerProp >=5 -> 10;
                                (AptGerProp - TgtGerProp) =:= -1 -> 20;
                                (AptGerProp - TgtGerProp) =:= 1 -> 5;
                                (AptGerProp - TgtGerProp) =:= 3 -> 20; % AptGerProp 4 TgtGerProp 1
                                (AptGerProp - TgtGerProp) =:= -3 -> 5; % AptGerProp 1 TgtGerProp 4
                                true -> 10
                            end,                           
%%                        C0 = case ((TgtGerProp - AptGerProp + 4) rem 4) of
%%                                    1 -> 20;
%%                                    3 -> 5;
%%                                    _ -> 10
%%                                end,
                       C = ((T div 1000)*5*C0) / Num,
                       ?INFO("WinRate2 ~w + ~w + ~w = ~w total:~w   ~w<<>>~w-->~w",[A,B,C,A+B+C+Acc,Acc,TgtGerProp,AptGerProp,C0]),
                       A + B + C + Acc
               end, 0, lists:seq(1, Num)),
    ?INFO("WinRate2 final ~w+~w=~w",[WinRate1,WinRate2,WinRate1+WinRate2]),
    erlang:trunc(WinRate1+WinRate2).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_cirrus_list()->
    RoleID = role_data:get_roleID(),
    HomeInfo = home_server:get_home(RoleID),
    CirrusNodeList0 = HomeInfo#home_info.cirrus_list,
    NewCirrusNodeList0 = lists:foldr(fun(Cirrus,AccList)->
            NewCirrus = if
                Cirrus#home_cirrus.is_win =:= 1 
                  andalso Cirrus#home_cirrus.cost_type > 0
                  andalso Cirrus#home_cirrus.doing_timestamp > 0
                  andalso Cirrus#home_cirrus.doing_ger_id > 0 ->
                    update_cirrus_reward(Cirrus);
                true ->
                    Cirrus
            end,
            [NewCirrus|AccList]
        end, [], CirrusNodeList0),
    % 补充默认的8号位图腾，默认开启
    NewCirrusNodeList = case lists:keytake(8, #home_cirrus.index , NewCirrusNodeList0) of
                            {value,N8,OtherCirrusNodeList} when N8#home_cirrus.is_win =:= 0 ->
                                [N8#home_cirrus{is_win = 0}|OtherCirrusNodeList];
                            {value,N8,OtherCirrusNodeList} when N8#home_cirrus.is_win /= 0 ->
                                NewCirrusNodeList0;
                            false ->
                                Cirrus8 = #home_cirrus{index = 8
                                                      ,is_win = 1 % 默认开启
                                                      ,doing_timestamp = 0
                                                      ,doing_ger_id = 0
                                                      ,cost_type = 0
                                                      ,cirrus_reward = #sell_reward{}
                                                      ,reward_already = 0
                                                      ,is_get_big = false
                                                      ,doing_ger_type = 0
                                                      ,doing_ger_quality = 0},
                                [Cirrus8|NewCirrusNodeList0]
                        end,
    if
        NewCirrusNodeList /= CirrusNodeList0 ->
            NewHomeInfo = HomeInfo#home_info{cirrus_list=NewCirrusNodeList},
            home_server:set_home(RoleID, NewHomeInfo);
        true ->
            ignore
    end,
    NewCirrusNodeList.

get_friend_cirrus_list(RoleID)->
    HomeInfo = home_server:get_home(RoleID),
    NewCirrusNodeList0 = HomeInfo#home_info.cirrus_list,
    % 补充默认的8号位图腾，默认开启
    NewCirrusNodeList = case lists:keytake(8, #home_cirrus.index , NewCirrusNodeList0) of
                            {value,N8,OtherCirrusNodeList} when N8#home_cirrus.is_win =:= 0 ->
                                [N8#home_cirrus{is_win = 0}|OtherCirrusNodeList];
                            {value,N8,OtherCirrusNodeList} when N8#home_cirrus.is_win /= 0 ->
                                NewCirrusNodeList0;
                            false ->
                                Cirrus8 = #home_cirrus{index = 8
                                                      ,is_win = 1 % 默认开启
                                                      ,doing_timestamp = 0
                                                      ,doing_ger_id = 0
                                                      ,cost_type = 0
                                                      ,cirrus_reward = #sell_reward{}
                                                      ,reward_already = 0
                                                      ,is_get_big = false
                                                      ,doing_ger_type = 0
                                                      ,doing_ger_quality = 0},
                                [Cirrus8|NewCirrusNodeList0]
                        end,
    NewCirrusNodeList.

update_cirrus_reward(Cirrus)->
    Now = util:now(),
    {MaxRewardTime,CirrusPeriod} = case Cirrus#home_cirrus.cost_type of
                    ?cirrus_cost_energy ->
                        {data_home:get(cirrus_energy_times)
                        ,data_home:get(cirrus_energy_period)};
                    ?cirrus_cost_diamond ->
                        {data_home:get(cirrus_diamond_times)
                        ,data_home:get(cirrus_diamond_period)};
                    0 ->
                        ?ERR("update_cirrus_reward can't be here1"),
                        {0,3600}
                end,
    NeedRewardTime = erlang:min(MaxRewardTime, (Now - Cirrus#home_cirrus.doing_timestamp) div CirrusPeriod),
    if
        Cirrus#home_cirrus.reward_already > NeedRewardTime ->
            ?ERR("update_cirrus_reward can't be here2. ~w  Now:~w",[Cirrus,Now]),
            Cirrus;
        Cirrus#home_cirrus.reward_already =:= NeedRewardTime ->
            Cirrus;
        true ->
            %do_cirrus_reward 中会传入玩家此次挂机是否有获大奖的信息
            {IsBig,AddReward} = do_cirrus_reward(Cirrus,NeedRewardTime-Cirrus#home_cirrus.reward_already),
            NewReward = role_reward:reward_plus_reward(Cirrus#home_cirrus.cirrus_reward, AddReward),
            ?INFO("update_cirrus_reward ~w~n~w~n", [AddReward,NewReward]),
            if
                true =:= IsBig ->
                    ?INFO("大奖 大奖  大奖  ~w ~w",[AddReward,Cirrus]),
                    home_server:add_cirrus_big_reward_count(),
                    Cirrus#home_cirrus{reward_already = NeedRewardTime
                                      ,is_get_big = IsBig
                                      ,cirrus_reward=NewReward};
                true ->
                    Cirrus#home_cirrus{reward_already = NeedRewardTime
                                      ,cirrus_reward=NewReward}
            end
    end.

do_cirrus_reward(Cirrus,RewardTime) ->
    do_cirrus_reward2({sell_reward,0,0,0,0,[],0,[]},Cirrus
                     ,Cirrus#home_cirrus.reward_already+1
                     ,Cirrus#home_cirrus.is_get_big 
                     ,RewardTime).
do_cirrus_reward2(FinalReward,Cirrus,_,IsBig,0)->
    {IsBig,FinalReward};
do_cirrus_reward2(AccReward,Cirrus,CurTime,IsBig,RewardTime)->
    CirrusId = Cirrus#home_cirrus.index,
    if
        CirrusId /= 8 ->
            {TypeA,TypeB,TypeC,TypeD,TypeAB,TypeE} = {cirrus_reward_a,cirrus_reward_b,cirrus_reward_c,cirrus_reward_d,cirrus_reward_ab,cirrus_reward_e};
        true ->
            {TypeA,TypeB,TypeC,TypeD,TypeAB,TypeE} = {cirrus_reward_a8,cirrus_reward_b8,cirrus_reward_c8,cirrus_reward_d8,cirrus_reward_ab8,cirrus_reward_e8}
    end,
    {NewIsBig,AddReward} = case lists:member(CurTime, data_home:get(TypeA)) of
        true ->
            %（固定）
            {false,cirrus_reward(CirrusId,TypeA)};
        false ->
            case lists:member(CurTime, data_home:get(TypeB)) of
                true ->
                    %（普通）
                    {false,cirrus_reward_b(Cirrus#home_cirrus.doing_ger_id,CirrusId)};
                false ->
                    case lists:member(CurTime, data_home:get(TypeC)) of
                        true ->
                            %（普通特产）
                            {false,cirrus_reward(CirrusId,TypeC)};
                        false ->
                            case lists:member(CurTime, data_home:get(TypeD)) of
                                true ->
                                    %（特殊特产）
                                    {false,cirrus_reward(CirrusId,TypeD)};
                                false ->
                                    case lists:member(CurTime, data_home:get(TypeAB)) of
                                        true ->
                                            %（普通或普通特产）
                                            {false,util:random_one_from_list([cirrus_reward_b(Cirrus#home_cirrus.doing_ger_id,CirrusId)
                                                                             ,cirrus_reward(CirrusId,TypeC)])};
                                        false ->
                                            case lists:member(CurTime, data_home:get(TypeE)) of
                                                true ->
                                                    %（大奖）
                                                    case util:random_int(1,data_home:get(cirrus_big_reward_rate)) of
                                                        1 when IsBig =:= false -> %没中大奖过才能中
                                                            case home_server:can_cirrus_big_reward() of
                                                                true ->
                                                                    {true,cirrus_reward(CirrusId,TypeE)};
                                                                false ->
                                                                    % 大奖未中就发固定奖
                                                                    ?INFO("cirrus_reward a",[]), 
                                                                    {false,cirrus_reward(CirrusId,TypeA)}
                                                            end;    
                                                        _ ->
                                                            % 大奖未中就发固定奖
                                                            ?INFO("cirrus_reward a",[]),
                                                            {false,cirrus_reward(CirrusId,TypeA)}
                                                    end;    
                                                false ->
                                                    ?ERR("do_cirrus_reward2 can't be here ~w ~w",[Cirrus,CurTime]),
                                                    {false,cirrus_reward(CirrusId,TypeA)}
                                            end
                                    end
                            end
                    end
            end
    end,
%%    io:format("do_cirrus_reward2 ~w ~w", [NewIsBig,AddReward]),
    NewReward = role_reward:reward_plus_reward(AccReward, AddReward),
    do_cirrus_reward2(NewReward,Cirrus,CurTime+1,IsBig orelse NewIsBig,RewardTime-1).

cirrus_reward(CirrusId,Type)->
    R = util:random_one_from_weigh_list(data_home:get({home_cirrus_reward,CirrusId,Type})),
    ?INFO("cirrus_reward Type:~w R:~w",[Type,R]),
    R.

%M是mega的数量，N是键石的数量
%与放入精灵相同的mega石碎片＊1，与放入精灵相同的键石碎片＊1（不能mega精灵则直接抽取键石碎片）
cirrus_reward_b(GerId,CirrusId)->
    if
        CirrusId /= 8 ->
            TypeB =  cirrus_reward_b;
        true ->
            TypeB =  cirrus_reward_b8
    end,        
    {M,N} = data_home:get({home_cirrus_reward,CirrusId,TypeB}),
    PosList = role_data:get_posList(),  %% 获取出战列表
    GerBag = role_data:get_gerBag(),  %% 保存武将背包
    LPosList = role_data:get_lieuposList(),  %% 副将武将列表,对应皮卡丘中的小伙伴
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,_}}  = role_data:get_mirror(),
    GerBagWithMirror = case GerSimpleInfo#gerSimple.gerID of
        0 ->
            GerBag;
        _ ->
            [GerSimpleInfo|GerBag]
    end,
    {TM,TN} = case role_ger:take_ger(GerId, PosList, LPosList, GerBagWithMirror) of
        {value, Ger, _, _, _, _}->
            GerConfig = case Ger of
                #ger{} ->
                    GerBase = Ger#ger.gerBase,
                    data_ger:get(GerBase#gerBase.gerTypeID);
                #gerSimple{} ->
                    data_ger:get(Ger#gerSimple.gerTypeID)
            end,
            MT0 =  case data_awake:get({data_ger_mega_map,GerConfig#data_ger.gerTypeID}) of
                       ?undefined ->
                           util:random_one_from_list(data_home:get(mega_stone_list));
                       MT00 ->
                           MT00
                   end,
            {MT0,data_awake:get({data_property_stone_map,GerConfig#data_ger.gerProperty})};
        _ ->
            {util:random_one_from_list(data_home:get(mega_stone_list))
            ,util:random_one_from_list(data_home:get(property_stone_list))}
    end,
    R = {sell_reward,0,0,0,0,[{new_item,TM,M,1,0},{new_item,TN,N,1,0}],0,[]},
    ?INFO("cirrus_reward Type:~w R:~w",[cirrus_reward_b,R]),
    R.

cirrus_reward_fight(RoleID,CirrusIndex)->
    {_,_,_,_,GerConfigList} = data_home:get({home_cirrus,CirrusIndex}),
    MonTeam = 
        lists:foldl(fun({GerTypeID,GerLevel,GerQuality,GerPos},AccList)-> 
                        Boss0 = ger_attr:new_ger(GerTypeID, GerLevel, GerQuality, [], []),
                        Boss = Boss0#ger{gerBase=((Boss0#ger.gerBase)#gerBase{gerPos=GerPos})},
                        [Boss|AccList]
                    end , [], GerConfigList),
    RoleFighterList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
    SkinInfo = role_skin:get_skin_info(),
    TrSpecialA = role_data:get_trSpecial(),
    ?INFO("cirrus_reward_fight ~w~n~w",[RoleFighterList, MonTeam]),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
    RoleFighterList2 = role_xbattle:get_add_buff(role_data:get_xbattle_data(),RoleFighterList),
    {Result, FightRecord, {_FinalState,_,_,_}} = role_fight:new(RoleID,RoleFighterList, MonTeam, RoleLieuAdd,#add_attr{},TalentList,[],TrSpecialA,#trSpecial{},false,SkinInfo,#skin_info{},LegendAddList,[]),
    {Result, FightRecord}.

check_cirrus_cost(Role,OperateType,CirrusIndex)->
    case OperateType of
        ?cirrus_energy_set ->
            #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
            {_,_,_NeedGold,NeedEnergy,_} = data_home:get({home_cirrus,CirrusIndex}),
            case Energy >= NeedEnergy of
                false ->
                    false;
                true ->
                    role_lib:deduct_energy_f(Role,RoleTimes, NeedEnergy),
                    true
            end;
        ?cirrus_diamond_set ->
            {_,_,NeedGold,_NeedEnergy,_} = data_home:get({home_cirrus,CirrusIndex}),
            case role_lib:check_money(Role, gold, NeedGold) of
                false ->
                    false;
                true ->
                    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_CURRIS, 0, ""),
                    true
            end     
    end.

check_home_build(Type)->
    case lists:member(Type, data_home:get(c_info_type)) of
        true ->
            RoleID = role_data:get_roleID(),
            HomeInfo = home_server:get_home(RoleID),
            {value,{Type,Level}} = lists:keysearch(Type, 1, HomeInfo#home_info.constr_list),
            {MaxLevel,_NeedStage,NeedNum,NeedItem,NeedConstr} = data_home:get({c_info,Type,Level}),
            case check_home_build_level(HomeInfo#home_info.constr_list,NeedConstr) of
                true ->
                    if
                        MaxLevel =:= Level ->
                            {fail,2};
                        true ->
                            Role = role_data:get_roleInfo(),
                            case role_lib:check_money(Role,home_resource,NeedNum) of
                                true ->
                                    Role= role_data:get_roleInfo(),
                                    case role_tvcard:check_cost(Role,NeedItem) of
                                        {true,{BagItem2,#role{roleID=RoleID},_BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu}} ->
                                            {ok,Level+1,NeedNum,{BagItem2,_BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu}};
                                        _ ->
                                            {fail,3}
                                    end;    
                                false ->
                                    {fail,3}
                            end
                    end;
                false ->
                    {fail,2}
            end;    
        false ->
            ?ERR("check_home_build fail Type:~w",[Type]),
            {fail,2}
    end.

check_home_build_level(CurConstrList,[])->
    true;
check_home_build_level(CurConstrList,[{Type,NeedLevel}|OtherNeedConstrList])->
    {value,{Type,Level}} = lists:keysearch(Type, 1, CurConstrList),
    if
        NeedLevel =< Level ->
            check_home_build_level(CurConstrList,OtherNeedConstrList);
        true ->
            false
    end.

check_up_stage(HomeInfo,RoleInfo)->
    case data_home:get({home_need,HomeInfo#home_info.stage}) of
        {BuildList,UpNeed} ->
            IsBuildOk = lists:all(fun({Type,NLvl})-> 
                    {value,{Type,L}} = lists:keysearch(Type, 1, HomeInfo#home_info.constr_list),
                    L >= NLvl
                end, BuildList),
            if
                IsBuildOk /= true ->
                    {fail,3};
                true ->
                    case role_lib:check_money(RoleInfo,home_resource,UpNeed) of
                        true ->
                            {ok,UpNeed};
                        false ->
                            {fail,4}
                    end
            end;
        _ ->
            {fail,2}
    end.

check_accept(TaskId,GerIdList,TaskList,RoleID,TaskOwner)->
    DountyNum = home_server:get_dounty_ac_num(RoleID),
    DountyMax = data_home:get(dounty_ac_max),
    AcceptOptLimit = data_home:get(accept_opt_limit),
    Now = util:now(),
%%     OptTs = erlang:get(?opt_ts),
%%     if
%%         OptTs =:= ?undefined orelse (AcceptOptLimit + OptTs) < Now ->
    case lists:keytake(TaskId, #home_task.id, TaskList) of 
        {value,Task,_OtherTaskList} when Task#home_task.status =:= ?ht_dounty_unaccept andalso DountyMax =< DountyNum ->
            {fail,7};
        {value,Task,_OtherTaskList} 
          when ((Task#home_task.status =:= ?ht_normal_unaccept andalso RoleID =:= TaskOwner)
               orelse (Task#home_task.status =:= ?ht_dounty_unaccept andalso RoleID =/= TaskOwner)) 
               andalso Task#home_task.ger_id =:= []->
            TgtL = erlang:length(Task#home_task.tgt_ger_type),
            GerIdListL = erlang:length(GerIdList),
            GerIdListL2 = erlang:length(lists:usort(GerIdList)), %检查是否有重复项
            if
                TgtL =:= GerIdListL andalso GerIdListL =:= GerIdListL2->
                    case check_accept_ger(GerIdList,[],[],[],[]) of
                        {fail,Reason} ->
                            ?INFO("already ger ~w",[get(?pd_task_gerid)]),
                            {fail,Reason};
                        {ResList1,ResList2,ResList3,ResList4} ->
                            {ok,Task,ResList1,ResList2,ResList3,ResList4}
                    end;
                true ->
                    {fail,2}
            end;
        false ->
            {fail,6};
        _ ->
            {fail,3}
    end.
%%         true ->
%%             {fail,8}
%%     end.


check_accept_ger([],ResList1,ResList2,ResList3,ResList4)->
    {ResList1,ResList2,ResList3,ResList4};
check_accept_ger([GerId|OtherList],AccResList1,AccResList2,AccResList3,AccResList4)->
    PosList = role_data:get_posList(),  %% 获取出战列表
    GerBag = role_data:get_gerBag(),  %% 保存武将背包
    LPosList = role_data:get_lieuposList(),  %% 副将武将列表,对应皮卡丘中的小伙伴
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,_}}  = role_data:get_mirror(),
    GerBagWithMirror = case GerSimpleInfo#gerSimple.gerID of
        0 ->
            GerBag;
        _ ->
            [GerSimpleInfo|GerBag]
    end,
    case role_ger:take_ger(GerId, PosList, LPosList, GerBagWithMirror) of
        {value, Ger, _, _, _, _}->
            case lists:member(GerId, get(?pd_task_gerid)) of
                false ->
                    case Ger of
                        #ger{} ->
                            GerBase = Ger#ger.gerBase,
                            if GerBase#gerBase.gerTypeID > 5000->
                                    check_accept_ger(OtherList
                                                    ,[GerId|AccResList1]
                                                    ,[GerBase#gerBase.gerTypeID|AccResList2]
                                                    ,[GerBase#gerBase.gerQuality|AccResList3]
                                                    ,[GerBase#gerBase.gerLevel|AccResList4]);
                               true ->
                                    {fail,2}
                            end;
                        #gerSimple{}  when Ger#gerSimple.gerTypeID > 5000->
                            check_accept_ger(OtherList
                                            ,[GerId|AccResList1]
                                            ,[Ger#gerSimple.gerTypeID|AccResList2]
                                            ,[Ger#gerSimple.gerQuality|AccResList3]
                                            ,[Ger#gerSimple.gerLevel|AccResList4]);
                        _ ->
                            {fail,2}
                    end;
                _ ->
                    ?INFO("HomeTask ~w ~w",[get(?pd_task_gerid),[E||E<-home_server:get_home_task(role_data:get_roleID())
                                                                ,E#home_task.status =:= ?ht_normal_accepted
                                                                             orelse E#home_task.status =:= ?ht_dounty_accepted]]),
                    {fail,2}
            end;
        _ ->
            {fail,2}
    end.

check_cirrus_ger(GerId)->
    PosList = role_data:get_posList(),  %% 获取出战列表
    GerBag = role_data:get_gerBag(),  %% 保存武将背包
    LPosList = role_data:get_lieuposList(),  %% 副将武将列表,对应皮卡丘中的小伙伴
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,_}}  = role_data:get_mirror(),
    GerBagWithMirror = case GerSimpleInfo#gerSimple.gerID of
        0 ->
            GerBag;
        _ ->
            [GerSimpleInfo|GerBag]
    end,
    case role_ger:take_ger(GerId, PosList, LPosList, GerBagWithMirror) of
        {value, Ger, _, _, _, _}->
            ?INFO("check_cirrus_ger ~w",[Ger]),
            case Ger of
                #ger{} ->
                    GerBase = Ger#ger.gerBase,
                    if GerBase#gerBase.gerTypeID > 7000->
                            {true,GerBase#gerBase.gerTypeID,GerBase#gerBase.gerQuality};
                       true ->
                            false
                    end;
                #gerSimple{}  when Ger#gerSimple.gerTypeID > 7000->
                    {true,Ger#gerSimple.gerTypeID,Ger#gerSimple.gerQuality};
                _ ->
                    false
            end;
        _ ->
            false
    end.

add_home_task(TaskId,RoleLevel,RoleID,RoleName,StageLevel,ParkLevel)->
    TimtoutTs = util:now() + data_home:get(unaccept_task_refresh_timeout),
    add_home_task(TaskId,RoleLevel,RoleID,RoleName,StageLevel,ParkLevel,TimtoutTs).

add_home_task(TaskId,RoleLevel,RoleID,RoleName,StageLevel,ParkLevel,TimtoutTs)->
    ?INFO("add_home_task(~w)~w",[TaskId,RoleID]),
    TaskTypeId = util:random_one_from_list(data_home:get(task_id_list)),
    TQuality = util:random_one_from_weigh_list(data_home:get(task_quality_random)),
    {LevelAmend,RandNumList,RandRTList} = data_home:get({task_quality, TQuality}),
    MaxItemLevel = data_common:get(max_role_level),
    TLevel0 = RoleLevel + LevelAmend,
    TLevel = if
                 TLevel0 < 1 -> 1;
                 TLevel0 > MaxItemLevel -> MaxItemLevel;
                 true -> TLevel0
             end,
    ?INFO("add_home_task TLevel:~w",[TLevel]),
    TNum = util:random_one_from_weigh_list(RandNumList),
    RewardNum = util:floor((TLevel+10*(2+TNum*2+TQuality*2))*(1+0.1*ParkLevel+0.5+0.5*StageLevel+0.3+0.6*TNum+0.6+0.7*TQuality)),
    GerTypeList = [util:random_one_from_list(data_home:get({tgt_ger_type,TQuality}))||_<-lists:seq(1, TNum)],
    RewardType = util:random_one_from_list(RandRTList),
    BoxReward = role_home:cacl_reward(RewardType,TQuality,TLevel),
    NewTask = #home_task{id = TaskId
              ,quality = TQuality
              ,level = TLevel
              ,tgt_ger_type=GerTypeList
              ,timtout_ts=TimtoutTs
              ,finish_time=(2+TNum*2+TQuality*2) *3600
              ,base_reward_num = RewardNum
              ,box_reward = BoxReward
              ,status=?ht_normal_unaccept
              ,role_id=0
              ,ger_id=[]
              ,ger_type=[]
              ,ger_quality=[]
              ,ger_level=[]
              ,onwer_role_id=RoleID
              ,task_type=TaskTypeId
              ,role_level=0
              ,owner_name=RoleName
              ,atk_name=[]}, % 这个是接受任务的人的等级，不是owner的，不要此处赋值
    if
        TaskId =:= 0 ->
            NewId = db_sql:add_home_task_info(NewTask),
            NewTask#home_task{id = NewId};
        true ->
            NewTask
    end.

%% WaitTime is milliseconds
home_start_timer(WaitTime,Msg)->
    erlang:start_timer(WaitTime, self(), {home,Msg}).

cacl_reward(equip_1,Q,L)->
    List = data_home:get(equip_1),
    Num = util:ceil(Q + L/50),
    EType = util:random_one_from_list(List),
    ItemList = [{p_item_view,EType,1,0,1}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=ItemList,reputation=0,roleExp=0};
cacl_reward(equip_2,Q,L)->
    List = data_home:get(equip_2),
    Num = util:ceil(Q/2 + L/80),
    EType = util:random_one_from_list(List),
    ItemList = [{p_item_view,EType,1,0,1}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=ItemList,reputation=0,roleExp=0};
cacl_reward(equip_3,Q,L)->
    List = data_home:get(equip_3),
    Num = util:ceil(Q/3 + L/110),
    EType = util:random_one_from_list(List),
    ItemList = [{p_item_view,EType,1,0,1}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=ItemList,reputation=0,roleExp=0};
cacl_reward(equip_4,Q,L)->
    List = data_home:get(equip_4),
    Num = util:ceil(Q/4 + L/140),
    EType = util:random_one_from_list(List),
    ItemList = [{p_item_view,EType,1,0,1}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=ItemList,reputation=0,roleExp=0};
cacl_reward(equip_5,Q,L)->
    List = data_home:get(equip_5),
    Num = util:ceil(Q/5 + L/170),
    EType = util:random_one_from_list(List),
    ItemList = [{p_item_view,EType,1,0,1}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=ItemList,reputation=0,roleExp=0};
cacl_reward(equip_6,Q,L)->
    List = data_home:get(equip_6),
    Num = util:ceil(Q/6+ L/200),
    EType = util:random_one_from_list(List),
    ItemList = [{p_item_view,EType,1,0,1}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=ItemList,reputation=0,roleExp=0};
cacl_reward(equip_7,Q,_L)->
    List = data_home:get(equip_7),
    Num = util:ceil(Q/7),
    EType = util:random_one_from_list(List),
    ItemList = [{p_item_view,EType,1,0,1}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=ItemList,reputation=0,roleExp=0};

cacl_reward(ger_2,Q,L)->
    [{_,List}]= ets:lookup(?ETS_CACHE_CONFIG_ID, ger_2),
    Num = util:ceil(Q/2 + L/80),
    GType = util:random_one_from_list(List),
    GerList = [{p_ger_view,0,1,GType}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=GerList,gold=0,itemList=[],reputation=0,roleExp=0};
cacl_reward(ger_3,Q,L)->
    [{_,List}]= ets:lookup(?ETS_CACHE_CONFIG_ID, ger_3),
    Num = util:ceil(Q/3 + L/110),
    GType = util:random_one_from_list(List),
    GerList = [{p_ger_view,0,1,GType}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=GerList,gold=0,itemList=[],reputation=0,roleExp=0};
cacl_reward(ger_4,Q,L)->
    [{_,List}]= ets:lookup(?ETS_CACHE_CONFIG_ID, ger_4),
    Num = util:ceil(Q/4 + L/140),
    GType = util:random_one_from_list(List),
    GerList = [{p_ger_view,0,1,GType}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=GerList,gold=0,itemList=[],reputation=0,roleExp=0};
cacl_reward(ger_5,Q,L)->
    [{_,List}]= ets:lookup(?ETS_CACHE_CONFIG_ID, ger_5),
    Num = util:ceil(Q/5 + L/170),
    GType = util:random_one_from_list(List),
    GerList = [{p_ger_view,0,1,GType}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=GerList,gold=0,itemList=[],reputation=0,roleExp=0};
cacl_reward(ger_6,Q,L)->
    [{_,List}]= ets:lookup(?ETS_CACHE_CONFIG_ID, ger_6),
    Num = util:ceil(Q/6+ L/200),
    GType = util:random_one_from_list(List),
    GerList = [{p_ger_view,0,1,GType}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=GerList,gold=0,itemList=[],reputation=0,roleExp=0};
cacl_reward(ger_7,Q,_L)->
    [{_,List}]= ets:lookup(?ETS_CACHE_CONFIG_ID, ger_7),
    Num = util:ceil(Q/7),
    GType = util:random_one_from_list(List),
    GerList = [{p_ger_view,0,1,GType}||_<-lists:seq(1, Num)],
    #p_reward_info{coin=0,gerExp=0,gerList=GerList,gold=0,itemList=[],reputation=0,roleExp=0};

cacl_reward(seed_2,Q,L)->
    EType = util:random_one_from_list(data_home:get(seed_2)),
    Num = util:ceil(Q+ L/20),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};
cacl_reward(seed_3,Q,L)->
    EType = util:random_one_from_list(data_home:get(seed_3)),
    Num = util:ceil(Q/2+ L/40),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};
cacl_reward(seed_4,Q,L)->
    EType = util:random_one_from_list(data_home:get(seed_4)),
    Num = util:ceil(Q/4+ L/60),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};
cacl_reward(seed_5,Q,L)->
    EType = util:random_one_from_list(data_home:get(seed_5)),
    Num = util:ceil(Q/5+ L/80),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};

cacl_reward(coin,Q,L)->
    Num = util:ceil(Q*50000+ L*1000),
    #p_reward_info{coin=Num,gerExp=0,gerList=[],gold=0,itemList=[],reputation=0,roleExp=0};
cacl_reward(reputation,Q,L)->
    Num = util:ceil(Q*100+ L*3),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[],reputation=Num,roleExp=0};
cacl_reward(diamond,Q,L)->
    Num = util:ceil(Q*10+ L/10),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=Num,itemList=[],reputation=0,roleExp=0};

cacl_reward(rare,Q,L)->
    EType = util:random_one_from_list(data_home:get(rare)),
    Num = util:ceil(Q+ L/75),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};

cacl_reward(stone_2,Q,L)->
    EType = util:random_one_from_list(data_home:get(stone_2)),
    Num = util:ceil(Q/2+ L/80),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};
cacl_reward(stone_3,Q,L)->
    EType = util:random_one_from_list(data_home:get(stone_3)),
    Num = util:ceil(Q/3+ L/110),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};
cacl_reward(stone_4,Q,L)->
    EType = util:random_one_from_list(data_home:get(stone_4)),
    Num = util:ceil(Q/4+ L/140),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};
cacl_reward(stone_5,Q,_L)->
    EType = util:random_one_from_list(data_home:get(stone_5)),
    Num = util:ceil(Q/7),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};

cacl_reward(mega_stone,Q,_L)->
    EType = util:random_one_from_list(data_home:get(mega_stone)),
    Num = if
              Q > 5 ->
                  Q -4;
              true ->
                  1
          end,
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[{p_item_view,EType,1,0,Num}],reputation=0,roleExp=0};

cacl_reward(T,Q,L)->
    ?ERR("cacl_reward error ~w ~w ~w",[T,Q,L]),
    #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[],reputation=0,roleExp=0}.

hook_role_levelup(NewLevel)->
    %%会重新刷新上阵精灵
    case NewLevel =:= data_home:get(constr_type_office_open_role_level) of
        true->
            PosList = role_data:get_posList(),
            role_data:init_posList(PosList);
        false->
            ignore
    end.