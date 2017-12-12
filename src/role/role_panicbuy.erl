%% @author zcl
%% @doc 全区抢购活动玩家信息处理
%% Created 2015-03-28
-module(role_panicbuy).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").

-define(PanicBuyActivityInfo,panicbuyactivityinfo).           %%玩家当前能够参加的打折活动
-define(DefaultTick,300).                                     %%默认活动检查时间
-define(timeout,3000).

-define(activityover,2).                %%返回给panic_buy_server活动结束
-define(out_of_buy_total_time,3).       %%抢购次数超额
-define(request_success,1).             %%一次抢购成功
-define(out_of_buy_person_time,4).      %%抢购个人次数超限
%#p_panic_buy_config{id=0,needconfig=#p_sell_reward_unit{goldnum=0,coinnum=0,reputationnum=0,itemlist=[],gerlist=[]},rewardconfig=#p_sell_reward_unit{goldnum=0,coinnum=0,reputationnum=0,itemlist=[],gerlist=[]},buytime=0,personbuytime=0,totaltime=0,begintime=0,endtime=0}
%% ====================================================================
%% API functions
%% ====================================================================




%%{panic_buy_activity_info,101,[30],1388505600,1451577600,20,499} 
init_panicbuy_info(RoleID) ->
  NewPanicBuyInfo = panic_buy_server:get_panicbuy_info(),
  OldPanicBuyRecord = db_sql:get_panicbuy_activity_record(RoleID),
  NewPanicBuyRecord = merge_panicbuy_info(OldPanicBuyRecord,check_match(NewPanicBuyInfo)),
  ?INFO("NewPanicBuyRecord:~w ~n",[NewPanicBuyRecord]),
  set_panicbuy_record(NewPanicBuyRecord).



persist_role_panicbuy_info(RoleID) ->
  PanicBuyActivityRecord = get_panicbuy_record(),
  ?INFO("persist_role_panicbuy_info PanicBuyActivityRecord:~w ~n",[PanicBuyActivityRecord]),
  db_sql:set_panicbuy_activity_record(RoleID,PanicBuyActivityRecord).


set_panicbuy_record(PanicBuyActivityRecord)->
  put(?PanicBuyActivityInfo,PanicBuyActivityRecord).

get_panicbuy_record()->
  get(?PanicBuyActivityInfo).

%%----------------------------------------------------------协议部分--------------------------------------------------
%%{panic_buy_activity_record,101,1388505600,1451577600,0,20,0}
%%处理玩家对全区抢购活动信息的请求
cs_panicbuy_info(#cs_panicbuy_info{})->
  case do_cs_panicbuy_info() of
    {'EXIT',_Reason}->
      ?sendself(#sc_panicbuy_info{result=3,panicbuyinfo=[],title=[]});
    {error,Reason}->
      ?sendself(#sc_panicbuy_info{result=Reason,panicbuyinfo=[],title=[]});
    {ok,Msg}->
      ?sendself(Msg)
  end.

%%处理玩家的全区抢购请求
cs_panicbuy_once(#cs_panicbuy_once{id=ID})->
  case do_cs_panicbuy_once(ID) of
    {'EXIT',_Reason}->
      ?sendself(#sc_panicbuy_once{result=6,rewardList=[]});
    {error,Reason}->
      ?sendself(#sc_panicbuy_once{result=Reason,rewardList=[]});
    {ok,Msg}->
      ?sendself(Msg);
    ignore->
      ignore
  end.
%%--------------------------------------------------------协议部分结束------------------------------------------------
%%实际处理全区抢购活动信息请求
do_cs_panicbuy_info()->
  case get_panicbuy_record() of
    []->
      {error,2};
    CurrentPanicBuyRecord ->
      NowTimeStamp = util:now(),
      ConfigList = lists:foldl(fun(RecordUnit,Acc)->
        case RecordUnit#panic_buy_activity_record.begintime =< NowTimeStamp andalso RecordUnit#panic_buy_activity_record.endtime >= NowTimeStamp of
          true->
            NewPanicBuyInfo = panic_buy_server:get_panicbuy_info(),
            #panic_buy_activity_record{id=ID,begintime=BeginTime,endtime=EndTime,buytime=BuyTime,personbuytime=PersonBuyTime,totalbuytime=TotalBuyTime} = RecordUnit,
            {_ID,NeedConfig,RewardConfig,_BeginTime,_EndTime,_PersonBuyTime,_TotalBuyTime,_DecreaseInterval,_DecreasePerTime,_DecreaseTime,_DecreaseLimit} = get_panicbuy_info_by_id(ID),
            TotalBuy = case lists:keyfind(ID,#panic_buy_activity_info.activityID,NewPanicBuyInfo) of
              false ->
                TotalBuyTime;
              FindOne->
                FindOne#panic_buy_activity_info.totalBuyTime
            end,
            NeedRewardUnit = role_discount:transfer2rewardunit(NeedConfig),
            RewardUnit = role_discount:transfer2rewardunit(RewardConfig),
            [#p_panic_buy_config{id=ID,needconfig=NeedRewardUnit,rewardconfig=RewardUnit,buytime=BuyTime,personbuytime=PersonBuyTime,totaltime=TotalBuy,begintime=BeginTime,endtime=EndTime}|Acc];
          false ->
            Acc
        end
      end,[],CurrentPanicBuyRecord),
      ?INFO("record:~w Config:~w ~n",[CurrentPanicBuyRecord,ConfigList]),
      Title = data_panic_buy:get(data_panic_buy_title),
      ?INFO("Title:~w ~n",[Title]),
      case length(ConfigList)>0 of
        true->
          {ok,#sc_panicbuy_info{result=1,panicbuyinfo=ConfigList,title=list_to_binary(Title)}};
        false ->
          {error,2}
      end
  end.
% message sc_panicbuy_once[id=20405]{
%   required                 int8                                      result        =1;  //抢购结果
%   //1->成功
%   //2->抢购需要的物品不足
%   //3->个人抢购次数已满
%   //4->全区抢购次数已满
%   //5->抢购活动已经结束
%   //6->系统出现错误
%   repeated                 p_reward_view                             rewardList    =2;  //抢购所获得的物品列表
% }
%   618  %%全区抢购活动玩家记录
%   619: -record(panic_buy_activity_record,{id=0,begintime=0,endtime=0,buytime=0,personbuytime=0,totalbuytime=0}).
%%实际处理全区抢购请求
do_cs_panicbuy_once(ID)->
  CurrentPanicBuyRecordList = get_panicbuy_record(),
  ?INFO("CurrentPanicBuyRecordList:~w ID:~w ~n",[CurrentPanicBuyRecordList,ID]),
  case CurrentPanicBuyRecordList of
    ?undefined->
      #role{roleID = RoleID} = role_data:get_roleInfo(),
      ?INFO("玩家:~w 的全区抢购活动为undefined~n",[RoleID]),
      {error,6};
    _->
      NowTimeStamp = util:now(),
      case lists:keyfind(ID,#panic_buy_activity_record.id,CurrentPanicBuyRecordList) of
        false ->
          {error,6};
        FindOne->
          case FindOne#panic_buy_activity_record.begintime =< NowTimeStamp andalso FindOne#panic_buy_activity_record.endtime >= NowTimeStamp of
            false ->
              {error,5};
            true->
              case FindOne#panic_buy_activity_record.totalbuytime >0 of
                true->
                  case FindOne#panic_buy_activity_record.buytime < FindOne#panic_buy_activity_record.personbuytime of 
                    true->
                      check_need(ID);
                    false->
                     {error,3}
                  end;
                false->
                  {error,4}
              end
          end
      end
  end.


%%保存全区抢购活动的配置信息
%%-record(panic_buy_activity_record,{id=0,begintime=0,endtime=0,buytime=0,personbuytime=0,totalbuytime=0}).
%-record(panic_buy_activity_info,{activityID=0,activityServerList=[],beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0,nextdecreasetime=0,decreasedtime=0}).
%%根据新的全区抢购活动的信息，更新玩家的全区抢购活动记录
merge_panicbuy_info(OldPanicBuyRecordList,NewPanicBuyInfoList)->
  ?INFO("OldPanicBuyInfoList:~w NewPanicBuyInfoList:~w ~n",[OldPanicBuyRecordList,NewPanicBuyInfoList]),
  case OldPanicBuyRecordList of 
    [] ->
      case NewPanicBuyInfoList of
        []->
          [];
        _ ->
          [#panic_buy_activity_record{id=ID,begintime=BeginTime,endtime=EndTime,personbuytime=PersonBuyTime,buytime=0,totalbuytime=TotalBuyTime}||#panic_buy_activity_info{activityID=ID,beginTime=BeginTime,endTime=EndTime,personBuyTime=PersonBuyTime,totalBuyTime=TotalBuyTime}<-NewPanicBuyInfoList]
      end;
    _ ->
      case NewPanicBuyInfoList of
        []->
          [];
        _->
          %%删除已经没有在配置文件中的活动记录
          OldPanicBuyRecordList1 = lists:foldl(fun(RecordUnit,Acc)->
              case lists:keyfind(RecordUnit#panic_buy_activity_record.id,#panic_buy_activity_info.activityID,NewPanicBuyInfoList) of
                false ->
                  Acc;
                FindOne->
                  [RecordUnit#panic_buy_activity_record{totalbuytime=FindOne#panic_buy_activity_info.totalBuyTime}|Acc]
              end
          end,[],OldPanicBuyRecordList),

          %%将新的配置活动转换成玩家的活动记录
          NewPanicBuyRecord = lists:foldl(fun(InfoUnit,Acc)->
            case lists:keyfind(InfoUnit#panic_buy_activity_info.activityID,#panic_buy_activity_record.id,OldPanicBuyRecordList1) of
              false ->
                #panic_buy_activity_info{activityID=ID,beginTime=BeginTime,endTime=EndTime,personBuyTime=PersonBuyTime,totalBuyTime=TotalBuyTime} = InfoUnit,
                [#panic_buy_activity_record{id=ID,begintime=BeginTime,endtime=EndTime,personbuytime=PersonBuyTime,buytime=0,totalbuytime=TotalBuyTime}|Acc];
              FindOne->
                Acc
            end
          end,[],NewPanicBuyInfoList),
          OldPanicBuyRecordList1++NewPanicBuyRecord
      end
  end.

%%检查玩家的道具是否满足全区抢购的需要，如果满足,将会向全区抢购服务器发送抢购消息
check_need(ID)->
  {_ID,NeedConfig,_RewardConfig,_BeginTime,_EndTime,_PersonBuyTime,_TotalBuyTIme,_DecreaseInterval,_DecreasePerTime,_DecreaseTime,_DecreaseLimit} = get_panicbuy_info_by_id(ID),
  NeedRewardUnit = role_discount:transfer2rewardunit(NeedConfig),
  BagGer = role_data:get_gerBag(),
  BagItem = role_data:get_bagItem(),
  BagEquip = role_data:get_bagEquip(),
  #role{gold=Gold,goldBonus=GoldBonus,reputation=Reputaion,coin=Coin} = role_data:get_roleInfo(),
  case deletegoods_from_p_sell_reward_unit_config(NeedRewardUnit,BagGer,BagItem,BagEquip,Gold,GoldBonus,Reputaion,Coin) of
    false ->
      {error,2};
    {true,_NewItemList,_NewGerList,_NewEquipList,_DeleteGerList,_DeleteItemList,_DeleteEquipList,_NewCoin,_NewGold,_NewGoldBonus,_NewReputation}->
      %%判断玩家具有足够的兑换物之后，需要首先向all_panic_buy_server发送抢购请求,防止因为延迟出现本地能够抢购，服务器不能够抢购的情况
      #role{roleID=RoleID} = role_data:get_roleInfo(),
      send_panic_buy_request(RoleID,ID)
      % role_item:update_role_info(NewItemList,NewGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,?MONEY_DEC_TYPE_PANICBUY_COST)
  end.

%%Config参数为p_sell_reward_unit结构,返回{true,NewItemList,NewGerList,NewEquipList,DeleteGerList,DeleteItemList,DeleteEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation};
deletegoods_from_p_sell_reward_unit_config(Config,BagGer,BagItem,BagEquip,Gold,GoldBonus,Reputaion,Coin)->
  ?INFO("Config:~w ~n",[Config]),
  case role_discount:delete_ger_list(BagGer,Config#p_sell_reward_unit.gerlist) of
        {true,NewLastGerList,LastDeleteList}->
          case role_discount:delete_item_list(BagItem,BagEquip,Config#p_sell_reward_unit.itemlist) of
            {true,NewItemList,NewEquipList,DeleteItemList,DeleteEquipList}->
              case Gold+GoldBonus >= Config#p_sell_reward_unit.goldnum andalso Coin >= Config#p_sell_reward_unit.coinnum andalso Reputaion>=Config#p_sell_reward_unit.reputationnum of
                true ->
                  NewCoin = Coin - Config#p_sell_reward_unit.coinnum,
                  {NewGold,NewGoldBonus} = 
                    case Gold >= Config#p_sell_reward_unit.goldnum of 
                      true -> 
                        {Gold-Config#p_sell_reward_unit.goldnum,GoldBonus};
                      false ->
                        {0,GoldBonus-(Config#p_sell_reward_unit.goldnum-Gold)}
                    end,
                  NewReputation = Reputaion-Config#p_sell_reward_unit.reputationnum,
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

%%根据p_sell_reward_unit结构向玩家发送奖励
% add_reward

%%向全区抢购服务器发送抢购请求
send_panic_buy_request(RoleID,ID)->
  erlang:send(panic_buy_server,{panic_buy_request,RoleID,ID}).

%%处理全区抢购请求返回数据
%{3,{panic_buy_activity_info,101,[30],1388505600,1451577600,20,0}}
  % //1->成功
  % //2->抢购需要的物品不足
  % //3->个人抢购次数已满
  % //4->全区抢购次数已满
  % //5->抢购活动已经结束
  % //6->系统出现错误
deal_panic_buy_response(Response)->
  ?INFO("全区抢购请求返回数据：~w ~n ",[Response]),
  {ID,Result,NewPanicBuyInfo} = Response,
  {OldPanicBuyRecord,Other} = get_panicbuy_record_by_id(ID),
  PanicBuyUnit = get_panicbuy_info_unit_by_id(ID,NewPanicBuyInfo),
  case OldPanicBuyRecord of
    ?undefined->
      ?sendself(#sc_panicbuy_once{result=6,rewardList=[]});
    _ ->
      case Result of
        ?activityover->
          ?sendself(#sc_panicbuy_once{result=5,rewardList=[]}),
          TempPanicBuyRecord = OldPanicBuyRecord;
        ?out_of_buy_total_time->
          TempPanicBuyRecord = OldPanicBuyRecord#panic_buy_activity_record{buytime=OldPanicBuyRecord#panic_buy_activity_record.buytime,totalbuytime=PanicBuyUnit#panic_buy_activity_info.totalBuyTime},
          ?sendself(#sc_panicbuy_once{result=4,rewardList=[]});
        ?out_of_buy_person_time->
          TempPanicBuyRecord = OldPanicBuyRecord,
          ?sendself(#sc_panicbuy_once{result=3,rewardList=[]});
        ?request_success->
          TempPanicBuyRecord = OldPanicBuyRecord#panic_buy_activity_record{buytime=OldPanicBuyRecord#panic_buy_activity_record.buytime+1,totalbuytime=PanicBuyUnit#panic_buy_activity_info.totalBuyTime},
          delete_and_reward(ID);
        _ ->
          TempPanicBuyRecord = OldPanicBuyRecord,
          ?INFO("出现什么坑爹返回结果了:~w ~n",[Result]),
          ?sendself(#sc_panicbuy_once{result=6,rewardList=[]})
      end,
      set_panicbuy_record([TempPanicBuyRecord|Other]),
      update_panic_buy_info(NewPanicBuyInfo),
      PanicBuyRecord = get_panicbuy_record(),
      NewPanicBuyRecord = merge_panicbuy_info(PanicBuyRecord,NewPanicBuyInfo)
  end.

delete_and_reward(ID)->
  {_ID,NeedConfig,RewardConfig,_BeginTime,_EndTime,_PersonBuyTime,_TotalBuyTIme,_DecreaseInterval,_DecreasePerTime,_DecreaseTime,_DecreaseLimit} = get_panicbuy_info_by_id(ID),
  NeedRewardUnit = role_discount:transfer2rewardunit(NeedConfig),
  RewardUnit = role_discount:transfer2rewardunit(RewardConfig),
  BagGer = role_data:get_gerBag(),
  BagItem = role_data:get_bagItem(),
  BagEquip = role_data:get_bagEquip(),
  #role{gold=Gold,goldBonus=GoldBonus,reputation=Reputaion,coin=Coin} = role_data:get_roleInfo(),
  case deletegoods_from_p_sell_reward_unit_config(NeedRewardUnit,BagGer,BagItem,BagEquip,Gold,GoldBonus,Reputaion,Coin) of
    false ->
      ?sendself(#sc_panicbuy_once{result=2,rewardList=[]});
    {true,NewItemList,NewGerList,NewEquipList,_DeleteGerList,_DeleteItemList,_DeleteEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation}->
      %%删除玩家抢购需要的物品
      role_item:update_role_info(NewItemList,NewGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,?MONEY_DEC_TYPE_PANICBUY_COST),
      %%向玩家发放获得物品
      Role = role_data:get_roleInfo(),
      Reward = role_discount:transfer2rewardlist2(RewardUnit),
      role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_PANIC_BUY, 0, ""),
      ?sendself(#sc_panicbuy_once{result=1,rewardList=role_reward:transform2p_reward_view(Reward,[])})
  end.

update_panic_buy_info(NewPanicBuyInfo)->
  PanicBuyRecord = get_panicbuy_record(),
  NewPanicBuyRecord = merge_panicbuy_info(PanicBuyRecord,check_match(NewPanicBuyInfo)),
  ?INFO("NewPanicBuyRecord:~w ~n",[NewPanicBuyRecord]),
  set_panicbuy_record(NewPanicBuyRecord),
  send_panic_buy_info().

%%保存全区抢购活动的配置信息
% -record(panic_buy_activity_info,{activityID=0,activityServerList=[],beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0}).
%%判断玩家能够参加全区抢购活动,如果不能够,返回undefined
check_match(PanicBuyActivityInfoList)->
  ?INFO("获得的全区抢购信息：~w ~n",[PanicBuyActivityInfoList]),
  PanicBuyActivityInfoList.

  %%向前端发送全区抢购活动信息
send_panic_buy_info()->
  case get_panicbuy_record() of
    ?undefined->
      ?sendself(#sc_panicbuy_info{result=2,panicbuyinfo=#p_panic_buy_config{id=0,needconfig=#p_sell_reward_unit{goldnum=0,coinnum=0,reputationnum=0,itemlist=[],gerlist=[]},rewardconfig=#p_sell_reward_unit{goldnum=0,coinnum=0,reputationnum=0,itemlist=[],gerlist=[]},buytime=0,personbuytime=0,totaltime=0,begintime=0,endtime=0}});
    PanicBuyRecordList->
      NowTimeStamp = util:now(),
      NewPanicBuyRecordConfig = lists:foldl(fun(CurrentPanicBuyRecord,Acc)->
        ?INFO("begin:~w endTime:~w currenttime:~w ~n",[CurrentPanicBuyRecord#panic_buy_activity_record.begintime,CurrentPanicBuyRecord#panic_buy_activity_record.endtime,NowTimeStamp]),
        case CurrentPanicBuyRecord#panic_buy_activity_record.begintime =< NowTimeStamp andalso CurrentPanicBuyRecord#panic_buy_activity_record.endtime >= NowTimeStamp of
          false ->
            ?INFO("CurrentPanicBuyRecord:~w ~n",[CurrentPanicBuyRecord]),
            Acc;
          true->
            ?INFO("CurrentPanicBuyRecord:~w ~n",[CurrentPanicBuyRecord]),
            #panic_buy_activity_record{id=ID,begintime=BeginTime,endtime=EndTime,buytime=BuyTime,personbuytime=PersonBuyTime,totalbuytime=TotalBuyTime} = CurrentPanicBuyRecord,
            {_ID,NeedConfig,RewardConfig,_BeginTime,_EndTime,_PersonBuyTime,_TotalBuyTime,DecreaseInterval,DecreasePerTime,DecreaseTime,DecreaseLimit} = get_panicbuy_info_by_id(ID),
            NeedRewardUnit = role_discount:transfer2rewardunit(NeedConfig),
            RewardUnit = role_discount:transfer2rewardunit(RewardConfig),
            [#p_panic_buy_config{id=ID,needconfig=NeedRewardUnit,rewardconfig=RewardUnit,buytime=BuyTime,personbuytime=PersonBuyTime,totaltime=TotalBuyTime,begintime=BeginTime,endtime=EndTime}|Acc]
        end
      end,[],PanicBuyRecordList),
      Title = data_panic_buy:get(data_panic_buy_title),
      ?INFO("record:~w Config:~w ~n",[PanicBuyRecordList,NewPanicBuyRecordConfig]),
      ?INFO("Title:~w ~n",[Title]),
      case length(NewPanicBuyRecordConfig)>0 of
        true->
          ?sendself(#sc_panicbuy_info{result=1,panicbuyinfo=NewPanicBuyRecordConfig,title=list_to_binary(Title)});
        false ->
          ?sendself(#sc_panicbuy_info{result=2,panicbuyinfo=[],title=list_to_binary([])})
      end
  end.

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

get_panicbuy_record_by_id(ID)->
  CurrentPanicBuyRecordList = get_panicbuy_record(),
  case lists:keytake(ID,#panic_buy_activity_record.id,CurrentPanicBuyRecordList) of
    false->
      ?INFO("未能找到玩家全区抢购记录：ID:~w  所有抢购记录：~w ~n ",[ID,CurrentPanicBuyRecordList]),
      {?undefined,CurrentPanicBuyRecordList};
    {_,FindOne,Other}->
      {FindOne,Other}
  end.

get_panicbuy_info_unit_by_id(ID,PanicBuyInfo) when is_list(PanicBuyInfo)->
  case lists:keyfind(ID,#panic_buy_activity_info.activityID,PanicBuyInfo) of
    false ->
      ?undefined;
    FindOne ->
      FindOne
  end;
get_panicbuy_info_unit_by_id(ID,PanicBuyInfo)->
  ?INFO("PanicBuyInfo is not list~n"),
  ?undefined.
