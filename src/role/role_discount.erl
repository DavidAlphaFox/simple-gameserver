%% @author zcl
%% @doc 限时打折活动玩家信息处理
%% Created 2015-03-28
-module(role_discount).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").

-define(LoginTimeCondition,1).                                %%最后登陆时间
-define(PayTimeCondition,2).                                  %%充值时间条件
-define(RegisterCondition,3).                                 %%注册时间条件
-define(LevelCondition,4).                                    %%等级条件
-define(ChannelCondition,5).                                  %%渠道条件
-define(GoldCondition,6).                                     %%钻石条件
-define(CoinCondition,7).                                     %%金币条件
-define(FightPowerCondition,8).                               %%战斗力条件
-define(ServerCondition,9).                                   %%区服条件
-define(LoginTimeAccCondition,10).                            %%登陆时间累积条件
-define(GoldAccCondition,11).                                 %%累积充值钻石条件
-define(CurrentDiscountActivity,currentdiscountactivity).     %%玩家当前能够参加的打折活动
-define(CurrentDiscountActivityRecord,discountactivityrecord).%%玩家当前打折活动的进行情况
-define(DefaultTick,300).                                     %%默认活动检查时间
-define(PAYACTIVITYTYPE,1).                                   %%充值活动类型
-define(EXCHANGEACTIVITYTYPE,2).                              %%兑换活动类型
-define(UNDEFINEDTYPE,3).                                     %%未定义活动类型

%% ====================================================================
%% API functions
%% ====================================================================


-record(configunit,{id=0,need=0,reward=0,times=0}).
-record(discount_activity_unit,{id=0,totaltimes=0,times=0}).

% {10101,500,{reward,100,100,100,[{item,21001,10,1,0}],[{ger,6090,1,1,0}]},1},
init_discount_info(RoleID) ->
  DiscountInfo = discount_server:get_discount_info(),
  {_,ActivityList} = check_activity_condition(DiscountInfo),

  
  DiscountActivityInfo = db_sql:get_discount_activity_info(RoleID),

  % 获取上次玩家能够参加的活动，防止玩家上次更新自身的数据之后不能参加本可以参加的活动
  LastDiscountActivityInfo = db_sql:get_last_discount_activity_info(RoleID),
  
  ?INFO("LastDiscountActivityInfo: ~w ~n",[LastDiscountActivityInfo]),
   LastDiscountActivityInfo1 = lists:foldl(fun(ActivityUnit,Acc)->
    case lists:keyfind(ActivityUnit#p_activity_unit.activityID,#d_discount_activity_unit.activityID,DiscountInfo#d_discount_info.activitylist) of
      false ->
        Acc;
      Find -> 
        [ActivityUnit|Acc]
    end
  end,[],LastDiscountActivityInfo),
  ?INFO("OldActivityList1:~w ~n",[LastDiscountActivityInfo1]),

  {NextTick,LastActivityList} = merge_discount_activity(delete_out_time_activity(LastDiscountActivityInfo1), delete_out_time_activity2(ActivityList)),
  ?INFO("LastActivityList:~w ~n NextTick ~w ~n",[LastActivityList,NextTick]),
  set_last_discount_activity_info(LastActivityList),
  case NextTick =/= 0 of
    true ->
      timer_wheel:add_plan(NextTick, fun ?MODULE:update_discount_info/0);
    false ->
      ignore
  end,

  CurrentAllActivityConfig = lists:foldl(fun(ActivityUnit,Acc)->
      ActivityContent = data_discount:get({data_discount_activity,ActivityUnit#p_activity_unit.real_activityID}),
      {_,List} = ActivityContent,
      List2 = [#configunit{id=Id,need=Need,reward=Reward,times=Times}||{Id,Need,Reward,Times}<-List],
      List2++Acc
  end,[],get_last_discount_activity_info()),
  
  %%将数据库中记录的活动记录中已经没有开启的活动的记录去除
  DiscountActivityInfo2 = lists:foldl(fun(Discount_Activity_Unit,Acc)->
    case lists:keyfind(Discount_Activity_Unit#discount_activity_unit.id,#configunit.id,CurrentAllActivityConfig) of
        false ->
          Acc;
        _ ->
          [Discount_Activity_Unit|Acc]
    end
  end,[],DiscountActivityInfo),

  %%将配置文件中新的活动加入到活动记录中
  DiscountActivityInfo3 = lists:foldl(fun(ConfigUnit,Acc)->
    case lists:keyfind(ConfigUnit#configunit.id,#discount_activity_unit.id,DiscountActivityInfo2) of
      false ->
        [#discount_activity_unit{id=ConfigUnit#configunit.id,totaltimes=ConfigUnit#configunit.times,times=0}|Acc];
      _ ->
        Acc
    end
  end,DiscountActivityInfo2,CurrentAllActivityConfig),

  set_discount_activity_record(DiscountActivityInfo3),
  send_discount_activity_list().

persist_role_discount_info(RoleID) ->
  Record = get_discount_activity_record(),
  db_sql:set_discount_activity_info(RoleID,Record),
  ActivityList = get_last_discount_activity_info(),
  db_sql:set_last_discount_activity_info(RoleID,delete_out_time_activity(ActivityList)).

set_discount_activity_record(DiscountActivityRecordInfo)->
  put(?CurrentDiscountActivityRecord,DiscountActivityRecordInfo).

get_discount_activity_record()->
  get(?CurrentDiscountActivityRecord).

get_last_discount_activity_info()->
  get(?CurrentDiscountActivity).

set_last_discount_activity_info(DiscountActivityInfo)->
  NewDiscountActivityInfo = lists:keysort(#p_activity_unit.begintime,DiscountActivityInfo),
  put(?CurrentDiscountActivity,NewDiscountActivityInfo).
%% 查看限时打折活动
cs_discount_activity_list(#cs_discount_activity_list{})->
    % ?INFO("All DiscountInfo ~w ~n ",[discount_server:get_discount_info()]).
    send_discount_activity_list().


%%查看兑换活动的信息
cs_discount_exchange_info(#cs_discount_exchange_info{})->
  ExchangeActivityIDList = data_discount:get(data_exchange_activity),
  Discount_Activity_List = get_last_discount_activity_info(),
  Delete_Out_time_Activity_list = get_current_activity(delete_out_time_activity2(Discount_Activity_List)),
  ?INFO("Delete_Out_time_Activity_list:~w ~n",[Delete_Out_time_Activity_list]),
  CurrentActivityID = 
    if 
      length(Delete_Out_time_Activity_list) >= 1 ->
          Temp = hd(Delete_Out_time_Activity_list),
          Temp#p_activity_unit.real_activityID;
      true ->
          -1
    end,
  case lists:member(CurrentActivityID,ExchangeActivityIDList) of
    false ->
      ?sendself(#sc_discount_exchange_info{result=2,configlist=[],title=list_to_binary([]),real_activityID=0});
    _ ->
      ActivityContent = data_discount:get({data_discount_activity,CurrentActivityID}),
      {Title,ExchangeActivityConfigList} = ActivityContent,
      ExchangeConfigList = [#p_exchange_config_unit{id=ConfigID,needconfig=transfer2rewardunit(NeedConfig),getconfig=transfer2rewardunit(RewardConfig),times=get_exchange_time(ConfigID,TotalTimes),totaltimes=TotalTimes}||{ConfigID,NeedConfig,RewardConfig,TotalTimes}<-ExchangeActivityConfigList],
      ?sendself(#sc_discount_exchange_info{result=1,configlist=ExchangeConfigList,title=list_to_binary(Title),real_activityID=CurrentActivityID})
  end.

%%兑换某个档位的奖励
cs_discount_exchange(#cs_discount_exchange{id=ID,real_activityID=RealActivityID})->
  case get_discount_activity_record() of
    ?undefined ->
      ?sendself(#sc_discount_exchange{result=5,times=0,id=ID,rewardList=[]});
    DiscountActivityRecord ->
      case lists:keytake(ID,#discount_activity_unit.id,DiscountActivityRecord) of
        false ->
          ?sendself(#sc_discount_exchange{result=2,times=0,id=ID,rewardList=[]});
        {_,FindUnit,Other} ->
          case FindUnit#discount_activity_unit.times >= FindUnit#discount_activity_unit.totaltimes of
            true ->
              ?sendself(#sc_discount_exchange{result=3,times=0,id=ID,rewardList=[]});
            false ->
              case lists:member(RealActivityID,data_discount:get(data_exchange_activity)) of
                false ->
                  ?sendself(#sc_discount_exchange{result=2,times=0,id=ID,rewardList=[]});
                true ->
                  ActivityContent = data_discount:get({data_discount_activity,RealActivityID}),
                  {_,ExchangeActivityConfigList} = ActivityContent,
                  ExchangeConfigList = [#p_exchange_config_unit{id=ConfigID,needconfig=transfer2rewardunit(NeedConfig),getconfig=transfer2rewardunit(RewardConfig),times=get_exchange_time(ConfigID,TotalTimes),totaltimes=TotalTimes}||{ConfigID,NeedConfig,RewardConfig,TotalTimes}<-ExchangeActivityConfigList],
                  case lists:keyfind(ID,#p_exchange_config_unit.id,ExchangeConfigList) of
                    false ->
                      ?sendself(#sc_discount_exchange{result=5,times=0,id=ID,rewardList=[]});
                    FindConfigUnit ->
                      case check_and_add_reward(FindConfigUnit) of
                        {true,NewItemList,NewLastGerList,NewEquipList,_,_,_,NewCoin,NewGold,NewGoldBonus,NewReputation} ->
                          role_item:update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,?MONEY_DEC_TYPE_DISCOUNT_EXCHANGE),
                          Role = role_data:get_roleInfo(),
                          Reward = transfer2rewardlist(FindConfigUnit),
                          role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_DISCOUNT_EXCHANGE, 0, ""),
                          ActivityRecordList = get_discount_activity_record(),
                          {_,FindRecord,Other} = lists:keytake(ID,#discount_activity_unit.id,ActivityRecordList),
                          NewTimes= FindRecord#discount_activity_unit.times +1,
                          NewRecordList = [FindRecord#discount_activity_unit{times=NewTimes}|Other],
                          set_discount_activity_record(NewRecordList),
                          ?sendself(#sc_discount_exchange{result=1,times=NewTimes,id=ID,rewardList=role_reward:transform2p_reward_view(Reward,[])});
                        false->
                          ?sendself(#sc_discount_exchange{result=4,times=0,id=ID,rewardList=[]})
                      end
                  end
              end
          end
      end
  end.

%%获取充值兑换活动信息
cs_discount_pay_activity_info(#cs_discount_pay_activity_info{})->
  PayActivityIDList = data_discount:get(data_pay_activity),
  Discount_Activity_List = get_last_discount_activity_info(),
  Delete_Out_time_Activity_list = get_current_activity(delete_out_time_activity2(Discount_Activity_List)),
  CurrentActivityID = 
    if 
      length(Delete_Out_time_Activity_list) >=1 ->
          Temp = hd(Delete_Out_time_Activity_list),
          Temp#p_activity_unit.real_activityID;
      true ->
          -1
    end,
  case lists:member(CurrentActivityID,PayActivityIDList) of
    false ->
      ?sendself(#sc_discount_pay_activity_info{result=2,configlist=[],title=list_to_binary([]),real_activityID=0});
    _ ->
      PayActivityContent = data_discount:get({data_discount_activity,CurrentActivityID}),
      {Title,PayActivityConfigList} = PayActivityContent,
      PayConfigList = [#p_pay_config_unit{id=ConfigID,payrank=NeedConfig,getconfig=transfer2rewardunit(RewardConfig),times=get_exchange_time(ConfigID,TotalTimes),totaltimes=TotalTimes}||{ConfigID,NeedConfig,RewardConfig,TotalTimes}<-PayActivityConfigList],
      ?sendself(#sc_discount_pay_activity_info{result=1,configlist=PayConfigList,title=list_to_binary(Title),real_activityID=CurrentActivityID})
  end.

%%领取某个充值奖励
cs_discount_pay_activity(#cs_discount_pay_activity{id=ID,real_activityID=RealActivityID})->
    case get_discount_activity_record() of
    ?undefined ->
      ?sendself(#sc_discount_pay_activity{result=5,times=0,id=ID,rewardList=[]});
    DiscountActivityRecord ->
      case lists:keytake(ID,#discount_activity_unit.id,DiscountActivityRecord) of
        false ->
          ?sendself(#sc_discount_pay_activity{result=2,times=0,id=ID,rewardList=[]});
        {_,FindUnit,Other} ->
           case FindUnit#discount_activity_unit.times >= FindUnit#discount_activity_unit.totaltimes of
            true ->
              ?sendself(#sc_discount_pay_activity{result=3,times=0,id=ID,rewardList=[]});
            false ->
              case lists:member(RealActivityID,data_discount:get(data_pay_activity)) of 
                false ->
                  ?sendself(#sc_discount_pay_activity{result=2,times=0,id=ID,rewardList=[]});
                true ->
                  PayActivityContent = data_discount:get({data_discount_activity,RealActivityID}),
                  {_,PayActivityConfigList} = PayActivityContent,
                  PayConfigList = [#p_pay_config_unit{id=ConfigID,payrank=NeedConfig,getconfig=transfer2rewardunit(RewardConfig),times=get_exchange_time(ConfigID,TotalTimes),totaltimes=TotalTimes}||{ConfigID,NeedConfig,RewardConfig,TotalTimes}<-PayActivityConfigList],
                  case lists:keyfind(ID,#p_pay_config_unit.id,PayConfigList) of
                    false ->
                      ?sendself(#sc_discount_pay_activity{result=5,times=0,id=ID,rewardList=[]});
                    FindConfigUnit ->
                      case get_currentactivity_pay(RealActivityID) of
                        {false,_} ->
                          ?sendself(#sc_discount_pay_activity{result=2,times=0,id=ID,rewardList=[]});
                        {true,TotalPay}->
                          case TotalPay >= FindConfigUnit#p_pay_config_unit.payrank of 
                            true ->
                              Role = role_data:get_roleInfo(),
                              Reward = transfer2rewardlist(FindConfigUnit),
                              role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_DISCOUNT_PAY, 0, ""),
                              ActivityRecordList = get_discount_activity_record(),
                              {_,FindRecord,Other} = lists:keytake(ID,#discount_activity_unit.id,ActivityRecordList),
                              NewTimes= FindRecord#discount_activity_unit.times +1,
                              NewRecordList = [FindRecord#discount_activity_unit{times=NewTimes}|Other],
                              set_discount_activity_record(NewRecordList),
                              ?sendself(#sc_discount_pay_activity{result=1,times=NewTimes,id=ID,rewardList=role_reward:transform2p_reward_view(Reward,[])}),
                              erlang:send(discount_server, {deleterole,Role#role.roleID,ID,RealActivityID});
                            false->
                              ?sendself(#sc_discount_pay_activity{result=4,times=0,id=ID,rewardList=[]})
                          end
                      end
                  end
              end
           end
      end
    end.


%%===================================================================
%%将奖励Config转换成reward list 
transfer2rewardlist(ConfigUnit) when is_record(ConfigUnit,p_pay_config_unit) ->
  #p_pay_config_unit{getconfig=GetConfig} = ConfigUnit,
  transfer2rewardlist2(GetConfig);

transfer2rewardlist(ConfigUnit)->
  #p_exchange_config_unit{getconfig=GetConfig}=ConfigUnit,
  transfer2rewardlist2(GetConfig).
transfer2rewardlist2(GetConfig)->
  ItemList = [{6, ItemTypeID, ItemNum}||#p_item_unit{itemtypeID=ItemTypeID,itemnum=ItemNum}<-GetConfig#p_sell_reward_unit.itemlist],
  GerList = [{7,GerTypeID,GerNum}||#p_ger_unit{gertypeID=GerTypeID,gernum=GerNum}<-GetConfig#p_sell_reward_unit.gerlist],
  ResultList1 = if 
    GetConfig#p_sell_reward_unit.coinnum >0 ->
      [{1,GetConfig#p_sell_reward_unit.coinnum}];
    true ->
      []
  end,
  ResultList2 = if
    GetConfig#p_sell_reward_unit.goldnum >0 ->
      [{2,GetConfig#p_sell_reward_unit.goldnum}|ResultList1];
    true ->
      ResultList1
  end,
  ResultList3 = if
    GetConfig#p_sell_reward_unit.reputationnum >0 ->
      [{3,GetConfig#p_sell_reward_unit.reputationnum}|ResultList2];
    true ->
      ResultList2
  end,
  ResultList3++ItemList++GerList.
%%检查兑换活动的配置，去除消耗以及加上奖励
check_and_add_reward(ConfigUnit)->
  GerList = role_data:get_gerBag(),
  BagItem = role_data:get_bagItem(),
  BagEquip = role_data:get_bagEquip(),
  #role{gold=Gold,goldBonus=GoldBonus,reputation=Reputaion,coin=Coin} = role_data:get_roleInfo(),
  #p_exchange_config_unit{needconfig=NeedConfig} = ConfigUnit,
  case delete_ger_list(GerList,NeedConfig#p_sell_reward_unit.gerlist) of
        {true,NewLastGerList,LastDeleteList}->
          case delete_item_list(BagItem,BagEquip,NeedConfig#p_sell_reward_unit.itemlist) of
            {true,NewItemList,NewEquipList,DeleteItemList,DeleteEquipList}->
              case Gold+GoldBonus >= NeedConfig#p_sell_reward_unit.goldnum andalso Coin >= NeedConfig#p_sell_reward_unit.coinnum andalso Reputaion>=NeedConfig#p_sell_reward_unit.reputationnum of
                true ->
                  NewCoin = Coin - NeedConfig#p_sell_reward_unit.coinnum,
                  {NewGold,NewGoldBonus} = 
                    case Gold >= NeedConfig#p_sell_reward_unit.goldnum of 
                      true -> 
                        {Gold-NeedConfig#p_sell_reward_unit.goldnum,GoldBonus};
                      false ->
                        {0,GoldBonus-(NeedConfig#p_sell_reward_unit.goldnum-Gold)}
                    end,
                  NewReputation = Reputaion-NeedConfig#p_sell_reward_unit.reputationnum,
                  {true,NewItemList,NewLastGerList,NewEquipList,LastDeleteList,DeleteItemList,DeleteEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation};
                false ->
                  false
              end;
            {false,_,_,_,_}->
              false
          end;
        {false,_,_}->
          false
  end.

delete_ger_list(GerList,DelGerList)->
  {LastGerList,LastDeleteList,LastResultList} = lists:foldl(fun(DeleteInfo,{ResultGerList,DeleteList,ResultList})->
      case delete_ger(ResultGerList,DeleteInfo,DeleteList) of
        {true,NewResultGerList,NewDeleteList}->
          {NewResultGerList,NewDeleteList,ResultList};
        {false,NewResultGerList,NewDeleteList}->
          {NewResultGerList,NewDeleteList,[false|ResultList]}
      end
  end,{GerList,[],[]},DelGerList),
  case length(LastResultList) =:= 0 of
    true ->
      ?INFO("完成精灵的删除:删除前：~w 删除配置：~w  删除列表：~w 删除后结果：~w ~n",[GerList,DelGerList,LastDeleteList,LastGerList]),
      {true,LastGerList,LastDeleteList};
    false ->
      {false,LastGerList,LastDeleteList}
  end.

%%删除某类精灵并返回删除后的精灵列表以及删除的列表O
delete_ger(GerList,DeletGerInfo,DeleteGerList)->
  #role{roleID=RoleID} = role_data:get_roleInfo(),
  HomeSteadGerID = homestead_server:get_homestead_ger(RoleID),
  #p_ger_unit{gertypeID=NeedGerTypeID,gernum=NeedGerNum,gerlevel=NeedGerLevel,gerquality=NeedGerRank} = DeletGerInfo,
  {LastResultList,LastDeleteList,LastAcc} = lists:foldl(fun(Ger,{ResultList,DeleteList,Acc})->
    #gerSimple{gerTypeID=GerTypeID,gerQuality=GerQuality,gerPos=GerPos,gerID=GerID,gerLevel=GerLevel} = Ger,
      case Acc >0 of
        true ->
          case GerTypeID == NeedGerTypeID andalso GerQuality == NeedGerRank andalso GerLevel == NeedGerLevel andalso GerPos == 0 andalso GerID =/= HomeSteadGerID of
            true ->
              {ResultList,[Ger|DeleteList],Acc-1};
            false->
              {[Ger|ResultList],DeleteList,Acc}
          end;
        false ->
          {[Ger|ResultList],DeleteList,0}
      end
    end,{[],DeleteGerList,NeedGerNum},GerList),
  case LastAcc =:= 0 of 
    true->
      {true,LastResultList,LastDeleteList};
    false ->
      {false,GerList,[]}
  end.

  delete_item_list(LastItemList,LastEquipList,ItemCost)->
  {LastResultItemList,LastResultEquipList,LastDeleteItemList,LastDeleteEquipList,LastResult} = lists:foldl(fun(DeleteInfo,{ResultLastItemList,ResultLastEquipList,DeleteItemList,DeleteEquipList,Result})->
    case delete_item(ResultLastItemList,ResultLastEquipList,DeleteItemList,DeleteEquipList,DeleteInfo) of
      {true,NewResultLastItemList,NewResultLastEquipList,NewDeleteItemList,NewDeleteEquipList}->
        {NewResultLastItemList,NewResultLastEquipList,NewDeleteItemList,NewDeleteEquipList,Result};
      {false,_,_,_,_}->
        {ResultLastItemList,ResultLastEquipList,DeleteItemList,DeleteEquipList,[false|Result]}
    end
  end,{LastItemList,LastEquipList,[],[],[]},ItemCost),
  case length(LastResult) =:= 0 of
    true->
      ?INFO("完成装备道具的删除：删除前：ItemList：~w  EquipList：~w  删除后：ItemList:~w   EquipList:~w   删除部分：ItemList:~w  EquipList：~w 删除配置：Itemcost：~w ~n",[LastItemList,LastEquipList,LastResultItemList,LastResultEquipList,LastDeleteItemList,LastDeleteEquipList,ItemCost]),
      {true,LastResultItemList,LastResultEquipList,LastDeleteItemList,LastDeleteEquipList};
    false ->
      {false,LastItemList,LastEquipList,[],[]}
  end.
        
delete_item(ItemList,EquipList,DeleteItemList,DeleteEquipList,DeleteInfo)->
  % {item,DelItemTypeID,DelItemNum,DelItemLevel,DelItemRank} = DeleteInfo,
  #p_item_unit{itemtypeID=DelItemTypeID,itemnum=DelItemNum,itemlevel=DelItemLevel,itemrank=DelItemRank} = DeleteInfo,
  {LastItemList,LastDeleteItemList,Result} = lists:foldl(fun(Item,{ResultItemlist,DeleteItemListAcc,Acc})->
    case Acc > 0 of
      true->
        #item{itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank,itemNum=Num,itemPos=Pos}=Item,
        case ItemTypeID=:=DelItemTypeID andalso Level =:=DelItemLevel andalso Rank=:=DelItemRank andalso Pos=:=0 of
          true->
            if
                Acc > Num -> 
                {ResultItemlist,[Item|DeleteItemListAcc],Acc-Num};
              Acc =:= Num ->
                {ResultItemlist,[Item|DeleteItemListAcc],0};
              true ->
                {[Item#item{itemNum=Num-Acc}|ResultItemlist],[Item#item{itemNum=Acc}|DeleteItemList],0}
            end;
          false ->
            {[Item|ResultItemlist],DeleteItemListAcc,Acc}
        end;
      false ->
        {[Item|ResultItemlist],DeleteItemListAcc,Acc}
    end
  end,{[],DeleteItemList,DelItemNum},ItemList),
  case Result =:= 0 of
    true ->
      {true,LastItemList,EquipList,LastDeleteItemList,DeleteEquipList};
    false ->
      {LastEquipList,LastDeleteEquipList,Result2} = lists:foldl(fun(Item,{ResultEquipList,DeleteEquipListAcc,Acc})->
        case Acc > 0 of
          true ->
            #item{itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank,itemNum=Num,itemPos=Pos}=Item,
            case ItemTypeID=:=DelItemTypeID andalso Level =:=DelItemLevel andalso Rank=:=DelItemRank andalso Pos=:=0 of
              true ->
                if 
                  Acc > Num ->
                    {ResultEquipList,[Item|DeleteEquipListAcc],Acc-Num};
                  Acc =:= Num ->
                    {ResultEquipList,[Item|DeleteEquipListAcc],0};
                  true ->
                    {[Item#item{itemNum=Num-Acc}],[Item#item{itemNum=Acc}|DeleteEquipListAcc],0}
                end;
              false ->
                {[Item|ResultEquipList],DeleteEquipListAcc,Acc}
            end;
          false->
            {[Item|ResultEquipList],DeleteEquipListAcc,Acc}
        end
      end,{[],DeleteEquipList,Result},EquipList),

      case Result2 =:= 0 of
        true ->
          {true,LastItemList,LastEquipList,LastDeleteItemList,LastDeleteEquipList};
        false ->
          {false,ItemList,EquipList,DeleteItemList,DeleteEquipList}
      end
  end.

%%将配置的需求结构{reward,100,100,100,[{item,21001,10,1,0}],[{ger,5010,1,1,0}]}转换成p_sell_reward_unit结构
transfer2rewardunit(NeedConfig)->
  {reward,NeedGold,NeedCoin,NeedReputation,NeedItemList,NeedGerList} = NeedConfig,
  ItemUnitList = [#p_item_unit{itemtypeID=ItemTypeID,itemnum=ItemNum,itemlevel=ItemLevel,itemrank=ItemRank}||{item,ItemTypeID,ItemNum,ItemLevel,ItemRank}<-NeedItemList],
  GerUnitList = [#p_ger_unit{gertypeID=GerTypeID,gernum=GerNum,gerlevel=GerLevel,gerquality=GerQuality}||{ger,GerTypeID,GerNum,GerLevel,GerQuality}<-NeedGerList],
  #p_sell_reward_unit{goldnum=NeedGold,coinnum=NeedCoin,reputationnum=NeedReputation,itemlist=ItemUnitList,gerlist=GerUnitList}.

%%根据兑换活动档位号获取当前已经兑换的次数
get_exchange_time(ConfigID,TotalTimes)->
  RecordList = get_discount_activity_record(),
  case lists:keyfind(ConfigID,#discount_activity_unit.id,RecordList) of
    false ->
      ?INFO("发现没有兑换次数的兑换档位：ConfigID:~w AllRecordList:~w ~n ",[ConfigID,RecordList]),
      TotalTimes;
    FindUnit ->
      FindUnit#discount_activity_unit.times
  end.

%%检查玩家满足的限时打折活动,显示所有未来的能够参加的活动
check_activity_condition(DiscountInfo)->
  ?INFO("DiscountInfo: ~w ~n",[DiscountInfo]),
  case is_record(DiscountInfo,d_discount_info) of
    true ->
      LastActivity = lists:foldl(fun(ActivityUnit,Acc)->
        CurrentTimestamp = util:now(),
        % case CurrentTimestamp >= ActivityUnit#d_discount_activity_unit.begintime andalso CurrentTimestamp =< ActivityUnit#d_discount_activity_unit.endtime of
        case CurrentTimestamp =< ActivityUnit#d_discount_activity_unit.endtime of 
          true->
            case check_condition(ActivityUnit#d_discount_activity_unit.activity_condition) of
                true ->
                  ?INFO("ActivityUnit ID :~w ~n",[ActivityUnit#d_discount_activity_unit.activityID]),
                  PayActivityList = data_discount:get(data_pay_activity),
                  ExchangeActivityList = data_discount:get(data_exchange_activity),
                  ActivityTypeID = 
                  case lists:member(ActivityUnit#d_discount_activity_unit.real_activityID,PayActivityList) of
                    true ->
                      ?PAYACTIVITYTYPE;
                    false ->
                      case lists:member(ActivityUnit#d_discount_activity_unit.real_activityID,ExchangeActivityList) of 
                        true ->
                          ?EXCHANGEACTIVITYTYPE;
                        false ->
                          ?INFO("出现不属于限时打折活动列表的活动ID:~w ~n",[ActivityUnit#d_discount_activity_unit.real_activityID]),
                          ?UNDEFINEDTYPE
                      end
                  end,
                  case ActivityTypeID =:=?UNDEFINEDTYPE of 
                    true ->
                      Acc;
                    false ->
                      [#p_activity_unit{real_activityID=ActivityUnit#d_discount_activity_unit.real_activityID,activityID=ActivityUnit#d_discount_activity_unit.activityID,begintime=ActivityUnit#d_discount_activity_unit.begintime,endtime=ActivityUnit#d_discount_activity_unit.endtime,activityType=ActivityTypeID,activitypay=0}|Acc]
                  end;
                false ->
                  Acc
            end;
          false ->
            Acc
        end
      end,[],DiscountInfo#d_discount_info.activitylist),
      
      {1,LastActivity};
    false ->
      {2,[]}
  end.

% 检查玩家是否符合活动开启条件
check_condition([])->
  false;
check_condition(ActivityCondition)->
  Result = lists:foldl(fun(ConditionUnit,Acc)->
        case check_condition2(ConditionUnit) of
          true->
            Acc;
          false ->
            [false|Acc]
        end
  end,[],ActivityCondition),
  case length(Result)=:=0 of
    true ->
      true;
    false ->
      false
  end.

check_condition2(ConditionUnit)->
  {Type,Args} = ConditionUnit,
  check_condition3(Type,Args).

%具体检查对应条件是否符合条件
check_condition3(?LoginTimeCondition,LastLoginTime)->
  LoginTimeStamp = util:datetime_to_seconds(LastLoginTime),
  #daily{lastLoggedLoginDate=LastLoginDate}= role_data:get_dailyInfo(),
  LastLoginDate1 = {LastLoginDate,{0,0,0}},
  LastLoginDate2 = util:datetime_to_seconds(LastLoginDate1),
  LoginTimeStamp >= LastLoginDate2;

check_condition3(?PayTimeCondition,{BeginTime,LastTime})->
  BeginTimeStamp = util:datetime_to_seconds(BeginTime),
  EndTimeStamp = util:datetime_to_seconds(LastTime),
  #daily{lastPayTime=LastPayTime} = role_data:get_dailyInfo(),
  LastPayTime >=BeginTimeStamp andalso LastPayTime =< EndTimeStamp;

check_condition3(?RegisterCondition,{BeginTime,LastTime})->
  ?INFO("注册时间条件没有处理 BeginTiem: ~w EndTime:~w ~n ",[BeginTime,LastTime]),
  true;

check_condition3(?LevelCondition,{BeginLevel,LastLevel})->
  #role{level=Level} = role_data:get_roleInfo(),
  Level >= BeginLevel andalso Level =< LastLevel;

check_condition3(?ChannelCondition,ChannelList)->
  #role{srcType=SrcType} = role_data:get_roleInfo(),
  lists:member(SrcType,ChannelList);

check_condition3(?GoldCondition,{BeginGold,EndGold})->
  #role{gold=Gold,goldBonus=GoldBonus} = role_data:get_roleInfo(),
  (Gold+GoldBonus) >= BeginGold andalso (Gold+GoldBonus)=<EndGold;

check_condition3(?CoinCondition,{BeginCoin,EndCoin})->
  #role{coin=Coin} = role_data:get_roleInfo(),
  Coin >= BeginCoin andalso Coin =< EndCoin;

check_condition3(?FightPowerCondition,{BeginFightPower,EndFightPower})->
  #role{fightPower=FightPower} = role_data:get_roleInfo(),
  FightPower >= BeginFightPower andalso FightPower =< EndFightPower;

check_condition3(?ServerCondition,ServerList)->
  #role{roleID=RoleID} = role_data:get_roleInfo(),
  ServerID = util:calc_server_id(roleID, RoleID),
  lists:member(ServerID,ServerList);

check_condition3(?LoginTimeAccCondition,{BeginNum,EndNum})->
  #daily{loginDays=LoginDay} = role_data:get_dailyInfo(),
  LoginDay >= BeginNum andalso LoginDay =< EndNum;

check_condition3(?GoldAccCondition,{BeginGold,EndGold})->
  #role{goldTotalPaid=GoldTotalPay} = role_data:get_roleInfo(),
  GoldTotalPay >= BeginGold andalso GoldTotalPay =< EndGold;

check_condition3(ConditionType,Args)->
  ?INFO("出现不能处理的条件：~w 参数：~w~n",[ConditionType,Args]),
  false.

%更新当前时间的活动列表，并且向客户端推送当前玩家能够参与的活动
update_discount_info()->
  OldActivityList = get_last_discount_activity_info(),
  DiscountInfo = discount_server:get_discount_info(),
  {_,ActivityList} = check_activity_condition(DiscountInfo),
  ?INFO("OldActivityList: ~w ~n ActivityList: ~w ~n",[OldActivityList,ActivityList]),
  OldActivityList1 = lists:foldl(fun(ActivityUnit,Acc)->
    case lists:keyfind(ActivityUnit#p_activity_unit.activityID,#d_discount_activity_unit.activityID,DiscountInfo#d_discount_info.activitylist) of
      false ->
        Acc;
      Find -> 
        [ActivityUnit|Acc]
    end
  end,[],OldActivityList),
  ?INFO("OldActivityList1:~w ~n",[OldActivityList1]),
  {NextTick,LastActivityList} = merge_discount_activity(delete_out_time_activity(OldActivityList1),delete_out_time_activity(ActivityList)),
  set_last_discount_activity_info(LastActivityList),
  ?INFO("LastActivityList:~w NextTick:~w ~n",[LastActivityList,NextTick]),
  case NextTick =/= 0 of
    true ->
      timer_wheel:add_plan(NextTick, fun ?MODULE:update_discount_info/0);
    false ->
      ignore
  end,

  DiscountActivityInfo = get_discount_activity_record(),
  CurrentAllActivityConfig = lists:foldl(fun(ActivityUnit,Acc)->
      ActivityContent = data_discount:get({data_discount_activity,ActivityUnit#p_activity_unit.real_activityID}),
      {_,List} = ActivityContent,
      List2 = [#configunit{id=Id,need=Need,reward=Reward,times=Times}||{Id,Need,Reward,Times}<-List],
      List2++Acc
  end,[],get_last_discount_activity_info()),
  
  %%将数据库中记录的活动记录中已经没有开启的活动的记录去除
  DiscountActivityInfo2 = lists:foldl(fun(Discount_Activity_Unit,Acc)->
    case lists:keyfind(Discount_Activity_Unit#discount_activity_unit.id,#configunit.id,CurrentAllActivityConfig) of
        false ->
          Acc;
        FindActivityConfig ->
          [Discount_Activity_Unit#discount_activity_unit{totaltimes=FindActivityConfig#configunit.times}|Acc]
    end
  end,[],DiscountActivityInfo),

  %%将配置文件中新的活动加入到活动记录中
  DiscountActivityInfo3 = lists:foldl(fun(ConfigUnit,Acc)->
    case lists:keyfind(ConfigUnit#configunit.id,#discount_activity_unit.id,DiscountActivityInfo2) of
      false ->
        [#discount_activity_unit{id=ConfigUnit#configunit.id,totaltimes=ConfigUnit#configunit.times,times=0}|Acc];
      _ ->
        Acc
    end
  end,DiscountActivityInfo2,CurrentAllActivityConfig),

  set_discount_activity_record(DiscountActivityInfo3),
  send_discount_activity_list().

%%计算活动结束最早的时间戳
get_early_end_timestamp(ActivityUnitList)->
  ActivityUnitList1 = lists:filter(fun(A)->
    CurrentTimestamp = util:now(),
    if
      A#p_activity_unit.endtime > CurrentTimestamp ->
        true;
      true ->
        false
    end
  end,ActivityUnitList),
  NewActivityUnitList = lists:sort(fun(A,B)->
    A#p_activity_unit.endtime < B#p_activity_unit.endtime
  end,ActivityUnitList1),
  ?INFO("经过排序之后的结束时间信息 ~w ~n",[NewActivityUnitList]),
  case NewActivityUnitList of
    [] ->
      0;
    _ ->      
      ActivityUnit = hd(NewActivityUnitList),
      ActivityUnit#p_activity_unit.endtime
  end.

get_early_begin_timestamp(ActivityUnitList)->
 ActivityUnitList1 = lists:filter(fun(A)->
    CurrentTimestamp = util:now(),
    if
      A#p_activity_unit.begintime > CurrentTimestamp ->
        true;
      true ->
        false
    end
  end,ActivityUnitList),
  NewActivityUnitList = lists:sort(fun(A,B)->
  A#p_activity_unit.begintime < B#p_activity_unit.begintime
  end,ActivityUnitList1),
  ?INFO("经过排序之后的开始时间信息 ~w ~n",[NewActivityUnitList]),
  case NewActivityUnitList of
    [] ->
      0;
    _ ->      
      ActivityUnit = hd(NewActivityUnitList),
      ActivityUnit#p_activity_unit.begintime
  end.

% 整合能够参加的活动，返回整合后的活动，以开始时间的升序方式排列，并且返回最早的结束时间
merge_discount_activity(OldActivityList,NewActivityList)->
  ?INFO("OldActivityList:~w NewActivityList: ~w ~n",[OldActivityList,NewActivityList]),
  ActivityList1 = lists:foldl(fun(ActivityUnit,Acc)->
    case lists:keyfind(ActivityUnit#p_activity_unit.activityID,#p_activity_unit.activityID,OldActivityList) of
        false ->
          [ActivityUnit|Acc];
        _ ->
          Acc
    end
  end,OldActivityList,NewActivityList),
  ActivityList2 = lists:sort(fun(A,B)->
    A#p_activity_unit.begintime < B#p_activity_unit.begintime
  end,ActivityList1),
      ?INFO("ActivityList1: ~w ~n",[ActivityList1]),
      EarlyEndTime = get_early_end_timestamp(ActivityList1),
      EarlyBeginTime = get_early_begin_timestamp(ActivityList1),
      LastEarlyTime = 
      if
        EarlyBeginTime =/=0 andalso EarlyEndTime =/=0->
          if 
            EarlyEndTime > EarlyBeginTime ->
              EarlyBeginTime;
            true ->
              EarlyEndTime
          end;
        EarlyBeginTime =:=0 andalso EarlyEndTime =/=0 ->
          EarlyEndTime;
        EarlyBeginTime =/=0 andalso EarlyEndTime =:=0 ->
          0;
        EarlyEndTime =:= 0 andalso EarlyBeginTime =:= 0 ->
          0;
        true ->
          0
      end,
      {LastEarlyTime,ActivityList2}.

%%重新整理活动，去除当前时间不满足的活动
delete_out_time_activity(DiscountActivityInfo)->
  CurrentTimestamp = util:now(),
  case DiscountActivityInfo of 
    [] ->
      [];
    _ ->
      lists:foldl(fun(ActivityUnit,Acc)->
        % case ActivityUnit#p_activity_unit.endtime >= CurrentTimestamp andalso ActivityUnit#p_activity_unit.begintime =< CurrentTimestamp of
        case ActivityUnit#p_activity_unit.endtime > CurrentTimestamp of
          true ->
            [ActivityUnit|Acc];
          false ->
            Acc
        end
      end,[],DiscountActivityInfo)
  end.

delete_out_time_activity2(DiscountActivityInfo)->
  CurrentTimestamp = util:now(),
  NewDiscountActivityInfo = case DiscountActivityInfo of 
    [] ->
      [];
    _ ->
      lists:foldl(fun(ActivityUnit,Acc)->
        case ActivityUnit#p_activity_unit.endtime >= CurrentTimestamp andalso ActivityUnit#p_activity_unit.begintime =< CurrentTimestamp of
          true ->
            [ActivityUnit|Acc];
          false ->
            Acc
        end
      end,[],DiscountActivityInfo)
  end,
  lists:reverse(NewDiscountActivityInfo).
%%向前端发送限时打折活动的列表
send_discount_activity_list()->
  ActivityList = get_last_discount_activity_info(),
  ?INFO("ActivityList:~w ~n",[ActivityList]),
    NewActivityList = lists:foldl(fun(ActivityUnit,Acc)->
      NowTimeStamp = util:now(),
      case NowTimeStamp >= ActivityUnit#p_activity_unit.begintime andalso NowTimeStamp =< ActivityUnit#p_activity_unit.endtime of
        true ->
          [ActivityUnit|Acc];
        false ->
          Acc
      end
    end,[],ActivityList),
    NewActivityList1 = lists:reverse(NewActivityList),
    ?sendself(#sc_discount_activity_list{result=1,activitylist=get_current_activity(NewActivityList1)}).

%%将玩家充值的金额加入到玩家对应活动的记录中
add_pay_info(PayNumber)->
  ?INFO("Pay Gold :~w ~n",[PayNumber]),
  case get_current_activity(get_last_discount_activity_info()) of
    [] ->
      ignore;
    [ActivityUnit|_] ->
      NowTimeStamp = util:now(),
      NewCurrentActivityUnit = 
      case NowTimeStamp >= ActivityUnit#p_activity_unit.begintime andalso NowTimeStamp =< ActivityUnit#p_activity_unit.endtime of
        true ->
          ActivityUnit1 = ActivityUnit#p_activity_unit{activitypay = ActivityUnit#p_activity_unit.activitypay + PayNumber},
          case get_satisfied_pay_config(ActivityUnit1) of
            {true,SatisfiedConfigList} ->
              ?INFO("达到充值兑换的兑换条件 Pay：~w ConfigList:~w ~n",[ActivityUnit1#p_activity_unit.activitypay,SatisfiedConfigList]),
              Role = role_data:get_roleInfo(),
              erlang:send(discount_server, {addrole,Role#role.roleID,SatisfiedConfigList});
            false ->
              ?INFO("没有达到充值兑换的兑换条件 Pay：~w ~n",[ActivityUnit1#p_activity_unit.activitypay])
          end,
          ActivityUnit1;
        false ->
          ActivityUnit
      end,
      {value,_,Other} = lists:keytake(NewCurrentActivityUnit#p_activity_unit.activityID,#p_activity_unit.activityID,get_last_discount_activity_info()),
      set_last_discount_activity_info([NewCurrentActivityUnit|Other]),
      send_discount_activity_list()
  end.

%获取当前活动期间，玩家充值的金额
get_currentactivity_pay(RealActivityID)->
  CurrentDiscountActivity = delete_out_time_activity2(get_last_discount_activity_info()),
  if
    length(CurrentDiscountActivity) =:=1 ->
      Temp = hd(CurrentDiscountActivity),
      case Temp#p_activity_unit.real_activityID =:=RealActivityID of 
        true ->
          {true,Temp#p_activity_unit.activitypay};
        false ->
          {false,0}
      end;
    length(CurrentDiscountActivity) =:=0 ->
      {false,0};
    true ->
      ?INFO("当前时间存在多个活动：~w ~n",[CurrentDiscountActivity]),
      case lists:keyfind(RealActivityID,#p_activity_unit.real_activityID,CurrentDiscountActivity) of
        false ->
          {false,0};
        FindDiscountActivityUnit ->
          {true,FindDiscountActivityUnit#p_activity_unit.activitypay}
      end
  end.

%%获得满足充值兑换条件的充值兑换配置,返回-record(satisfied_discount_pay_info,{activityID=0,ranklist=[]}).结构
get_satisfied_pay_config(ActivityUnit)->
  case ActivityUnit#p_activity_unit.activityType =:=1 of %%1定义为充值活动，2为兑换活动
    false ->
      false;
    true ->  
      PayActivityContent = data_discount:get({data_discount_activity,ActivityUnit#p_activity_unit.real_activityID}),
      {_,PayActivityConfigList} = PayActivityContent,
      PayConfigList = [#p_pay_config_unit{id=ConfigID,payrank=NeedConfig,getconfig=transfer2rewardunit(RewardConfig),times=get_exchange_time(ConfigID,TotalTimes),totaltimes=TotalTimes}||{ConfigID,NeedConfig,RewardConfig,TotalTimes}<-PayActivityConfigList],
      SatisfiedConfigList = lists:foldl(fun(PayConfigUnit,Acc)->
      case PayConfigUnit#p_pay_config_unit.payrank =< ActivityUnit#p_activity_unit.activitypay of
        true ->
          [PayConfigUnit#p_pay_config_unit.id|Acc];
        false ->
          Acc
      end
      end,[],PayConfigList),
      case length(SatisfiedConfigList) =/= 0 of
        true ->
          {true,#satisfied_discount_pay_info{activityID=ActivityUnit#p_activity_unit.real_activityID,ranklist=SatisfiedConfigList}};
        false ->
          false
      end
  end.

%%返回玩家当前时间只能够看到的活动，根据所有活动开始时间排序，返回最早开启时间的那个活动，由于get_last_discount_activity_info中保存的是玩家所有能够参加的活动
get_current_activity(ActivityList)->
  CurrentActivity = case ActivityList of
    []->
      [];
    _ ->
      get_current_activity2(ActivityList)
  end,
  ?INFO("当前活动：~w~n",[CurrentActivity]),
  CurrentActivity.


get_current_activity2(ActivityList)->
  ActivityRecordList = get_discount_activity_record(),
  ActivityList2 = lists:foldl(fun(ActivityUnit,Acc)->
      case check_discount_activity_satisfied_by_id(ActivityUnit#p_activity_unit.real_activityID,ActivityRecordList) of
        true->
          Acc;
        false ->
          [ActivityUnit|Acc]
      end
  end,[],ActivityList),
  case ActivityList2 of
    []->
      [];
    _ ->
      [hd(lists:keysort(#p_activity_unit.begintime,ActivityList2))]
  end.


%%判断某个活动的所有兑换档都完成了兑换
check_discount_activity_satisfied_by_id(ID,ActivityRecordList)->
  lists:all(fun(Elem)->
    case lists:keyfind(Elem#p_pay_config_unit.id,#discount_activity_unit.id,ActivityRecordList) of
      false ->
        false;
      FindOne ->
        FindOne#discount_activity_unit.totaltimes =:= FindOne#discount_activity_unit.times
    end
  end,get_all_config_unit(ID)).

%%获得某个活动的所有配置项
get_all_config_unit(ID)->
  ?INFO("ID:~w ~n",[ID]),
  ActivityContent = data_discount:get({data_discount_activity,ID}),
  {Title,PayActivityConfigList} = ActivityContent,
  [#p_pay_config_unit{id=ConfigID,payrank=NeedConfig,getconfig=transfer2rewardunit(RewardConfig),times=get_exchange_time(ConfigID,TotalTimes),totaltimes=TotalTimes}||{ConfigID,NeedConfig,RewardConfig,TotalTimes}<-PayActivityConfigList]. 
%-----------------------------------------------------------------------------
%%使用gpay数据表中的最近充值时间加入到groleextra的lastpaytime中去,需要在未开服的情况下使用,不然数据会回写
test_add_lastpaytime()->
  Sql = "select roleID from gPay;",
  RoleList = case db_sql:get_all(Sql) of
    [_|_]=List ->
      [Role||[Role]<-List];
    _ ->
      []
  end,
  NewRoleList = lists:foldl(fun(RoleID,Acc)->
    case lists:member(RoleID,Acc) of
      true ->
        Acc;
      false->
        [RoleID|Acc]
    end
  end,[],RoleList),
  lists:foreach(fun(RoleID)->
    Sql1 = io_lib:format("select time from gPay where roleID=~w order by time desc limit 1;",[RoleID]),
    case db_sql:get_row(Sql1) of
      []->
        ignore;
      [Time] ->
        {datetime,Time2} = Time,
        TimeStamp = util:datetime_to_seconds(Time2),
        Sql2 = io_lib:format("update gRoleExtra set lastPayTime=~w where roleID=~w;",[TimeStamp,RoleID]),
        db_sql:sql_execute_with_log(Sql2)
    end
  end,NewRoleList).