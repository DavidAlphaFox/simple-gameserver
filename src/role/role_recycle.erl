-module(role_recycle).

-compile(export_all).

-include("def_role.hrl").

-define(RECYCLE_GER_TYPE,1).
-define(RECYCLE_EQUIP_TYPE,2).
-define(REAL_RECYCLE_TYPE,1).
-define(SCAN_RECYCLE_TYPE,2).
-define(DIVNUM,2100000000).
%%===============================协议处理接口=========================================%%
cs_recycle_restore(#cs_recycle_restore{type=Type,gerID=GerID,equipID=EquipID,isRealRecycl=IsRealRecycl})->
    case do_cs_recycle_restore(Type,GerID,EquipID,IsRealRecycl) of
        {false,Reason}->
            ?sendself(#sc_recycle_restore{result=Reason,gerID=GerID,equipID=EquipID,reward=[],isRealRecycl=IsRealRecycl});
        {true,RewardList}->
            ?sendself(#sc_recycle_restore{result=1,gerID=GerID,equipID=EquipID,reward=RewardList,isRealRecycl=IsRealRecycl});
        {true,RewardList,UpdateEquip}->
            ?sendself(#sc_recycle_restore{result=1,gerID=GerID,equipID=EquipID,reward=RewardList,isRealRecycl=IsRealRecycl}),
            %%此处进行更新是由于前端有bug，需要后端帮忙更新下
            item_lib:notify_item(UpdateEquip,GerID)
    end.
%%=============================协议内部处理函数=======================================%%
do_cs_recycle_restore(Type,GerID,EquipID,IsRealRecycl)->
    Result = if
        IsRealRecycl=:=?REAL_RECYCLE_TYPE ->
            do_cs_recycle_restore2(Type,GerID,EquipID);
        IsRealRecycl=:=?SCAN_RECYCLE_TYPE->
            do_scan_recycle_restore(Type,GerID,EquipID);
        true->
            {false,2} 
    end,
    case Result of
        {true,PRewardInfo}->
            {true,transformPRewardInfo2PRewardInfoList(PRewardInfo,[])};
        {true,PRewardInfo,UpdateEquip}->
            {true,transformPRewardInfo2PRewardInfoList(PRewardInfo,[]),UpdateEquip};
        _ ->
            Result
    end.

%%完成实际的还原操作
do_cs_recycle_restore2(Type,GerID,EquipID)->
    if
        Type=:=?RECYCLE_GER_TYPE ->
            do_recycle_ger(GerID);
        Type=:=?RECYCLE_EQUIP_TYPE->
            do_recycle_equip(GerID,EquipID);
        true->
            {false,2} 
    end.

%%只是计算出还原能够获得物品
do_scan_recycle_restore(Type,GerID,EquipID)->
    if
        Type=:=?RECYCLE_GER_TYPE ->
            case role_data:get_ger(GerID) of
                {value,Ger,_PosList,_LPosList,_GerBag,_Type}->
                    AllReward1 = calculate_ger_all_return_reward(Ger),
                    AllReward = recycle_reward_loss(AllReward1),
                    {true,activity_server:sell_reward2p_reward_info(AllReward)};
                false->
                    {false,3}
            end;
        Type=:=?RECYCLE_EQUIP_TYPE->
            case role_item:take_item(GerID,EquipID) of
                {value,Equip,_Other}->
                    case Equip#item.itemRank>10 of
                        false->
                            AllReward1=calculate_equip_all_return_reward(Equip),
                            AllReward=recycle_reward_loss(AllReward1),
                            {true,activity_server:sell_reward2p_reward_info(AllReward)};
                        true->
                            item_lib:notify_item(Equip#item{itemRank=10},GerID),
                            AllReward1=calculate_equip_all_return_reward(Equip),
                            AllReward=recycle_reward_loss(AllReward1),
                            {true,activity_server:sell_reward2p_reward_info(AllReward),Equip}
                    end;
                false->
                    {false,3}
            end;
        true->
            {false,2}
    end.
%======================================================================================%
do_recycle_equip(GerID,EquipID)->
    case role_item:take_item(GerID,EquipID) of
        false ->
            {false, 3};
        {_Value,FindOne,Other}->
            case check_equip_recycle_limit(FindOne) of
                false->
                    {false,4};
                true->
                    {ItemTypeID,Num} = data_recycle:get({recycle_cost,?RECYCLE_EQUIP_TYPE}),
                    case role_item:delete_sell_reward(#sell_reward{item=[#new_item{itemTypeID=ItemTypeID,itemNum=Num,itemLevel=1,itemRank=0}]},[EquipID],[]) of
                        {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
                            role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_RECYCLE,EquipID,""),
                            AllReward1 = calculate_equip_all_return_reward(FindOne),
                            AllReward=recycle_reward_loss(AllReward1),
                            Role=role_data:get_roleInfo(),
                            role_reward:handle_sys_reward_with_return(Role,AllReward,?MONEY_ADD_TYPE_RECYCLE,FindOne#item.itemTypeID,""),
                            RewardViewList = activity_server:sell_reward2p_reward_info(AllReward),
                            NewItemRank = case FindOne#item.itemRank>10 of true->10;false->FindOne#item.itemRank end,
                            NewEquip = FindOne#item{itemRank=NewItemRank,itemLevel=1,itemenchantType=0,itemenchantLevel=0},
                            case GerID of
                                0->
                                    BagEquip = role_data:get_bagEquip(),
                                    case lists:keytake(EquipID,#item.itemUID,BagEquip) of
                                        false->
                                            erlang:throw(false);
                                        {_Value,_FindOne,Other1}->
                                            role_data:set_bagEquip([NewEquip|Other1])
                                    end;
                                _ ->
                                    OldEquipList = role_data:get_equip(GerID),
                                    %%此处需要重新刷新装备的各种属性
                                    NewEquip1 = role_item:refresh_item(NewEquip),
                                    role_data:set_equip(GerID, [NewEquip1|Other]),
                                    role_lvlSgAttr:on_equip_lvl_up(GerID, OldEquipList),
                                    role_lvlSgAttr:on_equip_rank_up(GerID, OldEquipList),
                                    ger_attr:recacl_f(GerID)
                            end,
                            item_lib:notify_item(NewEquip,GerID),
                            {true,RewardViewList};
                        false->
                            {false,5}
                    end
            end
    end.


do_recycle_ger(GerUID)->
    case role_data:get_ger(GerUID) of
        {value,Ger,PosList2,LPosList2,_GerBag2,Type}->
            case Type of
                bag->
                    case do_recycle_ger2(Ger) of
                        {true,NewGer,RewardList}->
                            GerBag = role_data:get_gerBag(),
                            case lists:keytake(NewGer#gerSimple.gerID,#gerSimple.gerID,GerBag) of
                                false-> 
                                    ?ERR("error"),
                                    {false,6};
                                {value,_Find,Other}->
                                    role_data:set_gerBag([NewGer|Other]),
                                    {true,RewardList}
                            end;
                        {false,Reason}->
                            {false,Reason}
                    end;
                lieu->
                    case do_recycle_ger2(Ger) of
                        {true,NewGer,RewardList}->
                            role_data:set_lieuposList([NewGer|LPosList2]),
                            {true,RewardList};
                        {false,Reason}->
                            {false,Reason}
                    end;
                ger->
                    case do_recycle_ger2(Ger) of
                        {true,NewGer,RewardList}->
                            NewGer2 = ger_attr:recacl(NewGer,PosList2),        
                            PosList3 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
                            role_data:set_posList(PosList3),
                            {true,RewardList};
                        {false,Reason}->
                            {false,Reason}
                    end;
                _->
                    {false,3}
            end;
        _Msg->
            {false,3}
    end.
do_recycle_ger2(Ger)->
    {GerID1,GerCrystalInfo,GerAwakeInfo,GerLevel,NewGer,GerTypeID1,GerExp1} = if
        is_record(Ger,ger)->
            #ger{gerID=GerID,gerBase=GerBase}=Ger,
            #gerBase{gerCrystalInfo=CrystalInfo,gerAwakeInfo=AwakeInfo,gerLevel=Level,gerTypeID=GerTypeID,gerExp=GerExp}=GerBase,
            NewGerBase = GerBase#gerBase{gerCrystalInfo=-1,gerAwakeInfo=[],gerLevel=1,gerExp=0},
            Ger1 = Ger#ger{gerBase=NewGerBase},
            Ger2 = role_crystal:refresh_gercrystal(Ger1),
            {GerID,CrystalInfo,AwakeInfo,Level,Ger2,GerTypeID,GerExp};
        is_record(Ger,gerSimple)->
            #gerSimple{gerID=GerID,gerCrystalInfo=CrystalInfo,gerAwakeInfo=AwakeInfo,gerLevel=Level,gerTypeID=GerTypeID,gerExp=GerExp}=Ger,
            Ger1 = Ger#gerSimple{gerCrystalInfo=-1,gerAwakeInfo=[],gerLevel=1,gerExp=0},
            {GerID,CrystalInfo,AwakeInfo,Level,Ger1,GerTypeID,GerExp};
        true->
            {0,-1,[],1,#ger{},0,0}
    end,
    case check_ger_recycle_limit(GerCrystalInfo,GerAwakeInfo,GerLevel,GerExp1) of
        false->
            {false,4};
        true->
            {ItemTypeID,Num} = data_recycle:get({recycle_cost,?RECYCLE_GER_TYPE}),
            case role_item:delete_sell_reward(#sell_reward{item=[#new_item{itemTypeID=ItemTypeID,itemNum=Num,itemLevel=1,itemRank=0}]},[],[GerID1]) of
                {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
                    role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_RECYCLE,GerID1,""),
                    AllReward1 = calculate_ger_all_return_reward(GerAwakeInfo,GerCrystalInfo,GerLevel,GerTypeID1,GerExp1),
                    AllReward=recycle_reward_loss(AllReward1),
                    Role=#role{roleID=RoleID}=role_data:get_roleInfo(),
                    {Date,_} = Time = erlang:localtime(),
                    F = fun(#crystal{crystaltype=FCrystalType,crystalquality=FCrystalQuality,crystallevel=FCrystalLevel,crystalexp=FCrystalExp,crystalrankexp=FCrystalRankExp})->
                        behavior_ger_crystal:log(RoleID,GerID1,GerTypeID1,FCrystalType,FCrystalQuality,0,FCrystalLevel,0,FCrystalExp,0,FCrystalRankExp,0,
                      Date,Time,6) 
                    end,
                    %%因为精灵还原导致晶体降低，添加晶体变化日志
                    case is_list(GerCrystalInfo) of true->lists:foreach(F,GerCrystalInfo);false->ignore end,
                    role_reward:handle_sys_reward_with_return(Role,AllReward,?MONEY_ADD_TYPE_RECYCLE,GerTypeID1,""),
                    RewardViewList = activity_server:sell_reward2p_reward_info(AllReward),
                    #data_ger{gerStar=GerStarLevel}=data_ger:get(GerTypeID1),
                    role_lvlSgAttr:on_ger_lvl_up(GerStarLevel, GerLevel, 1),
                    ger_lib:notify_update(NewGer),
                    {true,NewGer,RewardViewList};
                false->
                    {false,5}
            end
    end.
%%判断精灵是否有还原的必要
check_ger_recycle_limit(GerCrystalInfo,GerAwakeInfo,GerLevel,GerExp)->
    GerAwakeInfo=/=[] orelse check_recycle_crystal_limit(GerCrystalInfo) orelse GerLevel>1 orelse GerExp>0.

%%判断装备是否有还原的必要
check_equip_recycle_limit(Equip)->
    ((Equip#item.itemRank>10 orelse Equip#item.itemenchantType=/=0 orelse Equip#item.itemLevel>1) andalso not lists:member(Equip#item.itemTypeID,data_recycle:get(un_recycle_item_list))) .

%%判断精灵晶体是否有还原必要
check_recycle_crystal_limit(-1)->
    false;
check_recycle_crystal_limit(GerCrystalInfo)->
    lists:any(fun(Crystal)->
        #crystal{crystalquality=CrystalQuality,crystallevel=CrystalLevel,crystalexp=CrystalExp,crystalrankexp=CrystalRankExp} = Crystal,
        CrystalQuality>1 orelse CrystalLevel>1 orelse CrystalExp>0 orelse CrystalRankExp>0
    end,GerCrystalInfo).

%%计算精灵还原需要返还的物品
calculate_ger_all_return_reward(Ger)->
    if
        is_record(Ger,ger) ->
            #ger{gerBase=GerBase}=Ger,
            #gerBase{gerCrystalInfo=CrystalInfo,gerAwakeInfo=AwakeInfo,gerLevel=Level,gerTypeID=GerTypeID,gerExp=GerExp}=GerBase,
            calculate_ger_all_return_reward(AwakeInfo,CrystalInfo,Level,GerTypeID,GerExp);
        is_record(Ger,gerSimple)->
            #gerSimple{gerCrystalInfo=CrystalInfo,gerAwakeInfo=AwakeInfo,gerLevel=Level,gerTypeID=GerTypeID,gerExp=GerExp}=Ger,
            calculate_ger_all_return_reward(AwakeInfo,CrystalInfo,Level,GerTypeID,GerExp);
        true->
            #sell_reward{}  
    end.

calculate_ger_all_return_reward(AwakeInfo,CrystalInfo,Level,GerTypeID,GerExp)->
    AwakeRecycleReward=calculate_ger_awake_return_reward(AwakeInfo,GerTypeID),
    CrystalRecycleReward=calculate_ger_crystal_return_reward(CrystalInfo),
    LevelRecycleReward=calculate_ger_level_return_reward(Level,GerExp),
    lists:foldl(fun(SR,Acc)->
        role_reward:reward_plus_reward(SR,Acc)
    end,#sell_reward{},[AwakeRecycleReward,CrystalRecycleReward,LevelRecycleReward]).

%%计算精灵觉醒还原需要返回的物品
calculate_ger_awake_return_reward(AwakeInfo,GerTypeID)->
    calculate_ger_awake_return_reward(AwakeInfo,GerTypeID,#sell_reward{}).
calculate_ger_awake_return_reward([],_GerTypeID,Acc)->
    Acc;
calculate_ger_awake_return_reward([H|T],GerTypeID,Acc)->
    SingleReward = calculate_ger_single_awake_return_reward(H,GerTypeID),
    calculate_ger_awake_return_reward(T,GerTypeID,role_reward:reward_plus_reward(SingleReward,Acc)).

%%计算精灵单个觉醒阶段需要返还的物品
calculate_ger_single_awake_return_reward(Awake,GerTypeID)->
    {BaseReward1=#sell_reward{newGer=NewGerList},GerNum} = case role_awake:check_ger_mega(GerTypeID) of
        false->
            data_awake:get({awake_step_unmega,Awake#awake.step});
        true->
            data_awake:get({awake_step_mega,Awake#awake.step})
    end,
    %%此处存在闪光精灵的重生，需要还原返还普通精灵
    GerTypeID2 = case data_ger:get(GerTypeID) of
        #data_ger{sameType=GerTypeID}->
            GerTypeID;
        #data_ger{sameType=NGerTypeID}->
            NGerTypeID
    end,
    ReturnGerList = lists:duplicate(GerNum,#new_ger{gerTypeID=GerTypeID2,gerLevel=1,gerQuality=0}),
    BaseReward1#sell_reward{newGer=NewGerList++ReturnGerList}.

%%计算精灵晶体还原需要返还的物品
calculate_ger_crystal_return_reward(CrystalInfo)->
    calculate_ger_crystal_return_reward(CrystalInfo,#sell_reward{}).
calculate_ger_crystal_return_reward([],Acc)->
    Acc;
calculate_ger_crystal_return_reward(-1,Acc)->
    Acc;
calculate_ger_crystal_return_reward([H|T],Acc)->
    SingleReward=calculate_ger_single_crystal_return_reward(H),
    calculate_ger_crystal_return_reward(T,role_reward:reward_plus_reward(SingleReward,Acc)).

%%计算精灵单个晶体类型需要返还的物品
calculate_ger_single_crystal_return_reward(#crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel,crystalexp=CrystalExp,crystalrankexp=CrystalRankExp})->
    {LevelExp,RankExp} = role_crystal:calculate_crystal_exp_cost(CrystalType,CrystalQuality,CrystalLevel),
    LastLevelExp = LevelExp+CrystalExp,
    LastRankExp = RankExp+CrystalRankExp,
    %%此处直接使用了1->2品质需要的道具类型，当前使用的道具都是晶簇，如果有不同，此处需要修改
    {ItemTypeID,_RankExp}=data_crystal:get({data_crystal_uprank_cost,CrystalType,1}),
    case LastRankExp=/=0 of
        true->
            #sell_reward{coin=LastLevelExp,item=[#new_item{itemTypeID=ItemTypeID,itemNum=LastRankExp,itemLevel=1,itemRank=0}]};
        false->
            #sell_reward{coin=LastLevelExp}
    end.

%%计算精灵等级返还的物品,经验的损耗在这里实现，防止转换成能量块之后不能够处理
calculate_ger_level_return_reward(_Level,GerExp)->
    {Rate,_DeductList}=data_recycle:get(return_back_rate),
    NewGerExp=recycle_normal_loss(GerExp,Rate),
    transform_gerexp2energyblock(NewGerExp).

%%此处需要将GerExp转换成各种合适的能量块
transform_gerexp2energyblock(GerExp)->
    FruitList = [{ItemTypeID,data_fruit:get(K)}||{fruit_exp,ItemTypeID}=K<-data_fruit:get_list()],
    SortFruitList = lists:reverse(lists:keysort(2,FruitList)),
    transform_gerexp2energyblock(SortFruitList,GerExp,#sell_reward{}).
transform_gerexp2energyblock([],_GerExp,Acc)->
    Acc;
transform_gerexp2energyblock(_Map,0,Acc)->
    Acc;
transform_gerexp2energyblock([H|T],GerExp,Acc)->
    {FruitTypeID,FruitExp} = H,
    case GerExp >= FruitExp of
        true->
            Num=GerExp div FruitExp,
            RemainExp=GerExp rem FruitExp,
            RS = [#new_item{itemTypeID=FruitTypeID,itemNum=Num,itemLevel=1,itemRank=0}],
            NewAcc = role_reward:reward_plus_reward(#sell_reward{item=RS},Acc),
            transform_gerexp2energyblock(T,RemainExp,NewAcc);
        false->
            transform_gerexp2energyblock(T,GerExp,Acc)
    end.

%%计算出装备还原需要返还的物品
calculate_equip_all_return_reward(Equip)->
    LevelRecycleReward=calculate_equip_level_return_reward(Equip),
    EnchantRecycleReward=calculate_equip_enchant_return_reward(Equip),
    PerfectRetineRecycleReward=calculate_equip_perfectRetine_return_reward(Equip),
    % ?ERR("LevelR:~w EnchantR:~w PerfectR:~w ~n",[LevelRecycleReward,EnchantRecycleReward,PerfectRetineRecycleReward]),
    lists:foldl(fun(SR,Acc)->
        role_reward:reward_plus_reward(SR,Acc)
    end,#sell_reward{},[LevelRecycleReward,EnchantRecycleReward,PerfectRetineRecycleReward]).

%%计算装备等级返还的物品
calculate_equip_level_return_reward(Equip)->
    #item{itemTypeID=ItemTypeID,itemType=ItemType,itemLevel=ItemLevel} = Equip,
    #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
    IncStep = get_equip_level_increase_step(ItemLevel),
    calculate_equip_level_return_reward2(ItemType,ItemLevel-1,1,ItemStar,0,IncStep).

calculate_equip_level_return_reward2(ItemType,ItemMaxLevel,CurrentLevel,ItemStar,Acc,IncStep)->
    case CurrentLevel > ItemMaxLevel of
        true->
            case ItemMaxLevel=:= 0 of
                true->
                    NeedCoin = role_item:cacl_reinforce_coin_ger(ItemType, 1, ItemStar),
                    #sell_reward{coin=Acc+NeedCoin};
                false->
                    #sell_reward{coin=Acc}
            end;
        false->
            NewLevel = CurrentLevel + IncStep,
            NeedCoin = get_equip_level_reinforce_coin(ItemType,CurrentLevel,ItemStar), 
            calculate_equip_level_return_reward2(ItemType,ItemMaxLevel,NewLevel,ItemStar,Acc+NeedCoin,IncStep)
    end.

%%计算装备附魔返还的物品
calculate_equip_enchant_return_reward(Equip)->
    #item{itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel} = Equip,
    calculate_equip_enchant_return_reward2(ItemEnchantType,ItemEnchantLevel,#sell_reward{}).
calculate_equip_enchant_return_reward2(_ItemEnchantType,0,Acc)->
    Acc;
calculate_equip_enchant_return_reward2(ItemEnchantType,ItemEnchantLevel,Acc)->
    SR = data_item_enchant:get({data_item_enchant_cost,ItemEnchantType,ItemEnchantLevel}),
    NewAcc= role_reward:reward_plus_reward(SR,Acc),
    calculate_equip_enchant_return_reward2(ItemEnchantType,ItemEnchantLevel-1,NewAcc).

%%计算装备完美精炼返的物品
calculate_equip_perfectRetine_return_reward(Equip)->
    calculate_equip_perfectRetine_return_reward2(Equip#item.itemTypeID,Equip#item.itemRank,#sell_reward{}).
calculate_equip_perfectRetine_return_reward2(_ItemTypeID,ItemRank,Acc) when ItemRank=<10 ->
    Acc;
calculate_equip_perfectRetine_return_reward2(ItemTypeID,ItemRank,Acc)->
    {NeedCoin,{NeedItemTypeID,NeedItemNum},EquipNum,_AddAttr} = data_item_pft_uprank:get({ItemTypeID,ItemRank}),
    NeedEquipList = lists:duplicate(EquipNum,#new_item{itemTypeID=ItemTypeID,itemNum=1,itemLevel=1,itemRank=0}),
    NeedSR = #sell_reward{coin=NeedCoin,item=[#new_item{itemTypeID=NeedItemTypeID,itemNum=NeedItemNum,itemLevel=1,itemRank=0}]++NeedEquipList},
    calculate_equip_perfectRetine_return_reward2(ItemTypeID,ItemRank-1,role_reward:reward_plus_reward(NeedSR,Acc)).

%% 将特殊的道具转化成对应道具，比如键石转换成键石碎片
special_transform(Reward)->
    #sell_reward{item=ItemList}=Reward,
    Reward#sell_reward{item=special_transform2(ItemList)}.
special_transform2(ItemList)->
    special_transform2(ItemList,[]).
special_transform2([],Result)->
    Result;
special_transform2([H|T],Result)->
    SpecialBackList = data_recycle:get(special_back_list),
    #new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank}=H,
    case lists:keyfind(ItemTypeID,1,SpecialBackList) of
        false->
            special_transform2(T,[H|Result]);
        {_ItemTypeID,ReplaceTypeID,Rate}->
            ReplaceItem = #new_item{itemTypeID=ReplaceTypeID,itemNum=ItemNum*Rate,itemLevel=ItemLevel,itemRank=ItemRank},
            special_transform2(T,[ReplaceItem|Result])
    end.

%%将奖励损失部分      
recycle_reward_loss(Reward1)->
    Reward= special_transform(Reward1),
    #sell_reward{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,item=ItemList,newGer=_GerList,reputation=Reputation} = Reward,
    {Rate,_DeductList}=data_recycle:get(return_back_rate),
    NewItemList = recycle_item_loss(ItemList),
    NewCoin = recycle_normal_loss(Coin,Rate),
    NewRoleExp=recycle_normal_loss(RoleExp,Rate),
    NewGerExp=recycle_normal_loss(GerExp,Rate),
    NewGold=recycle_normal_loss(Gold,Rate),
    NewReputation=recycle_normal_loss(Reputation,Rate),
    Reward#sell_reward{coin=NewCoin,roleExp=NewRoleExp,gerExp=NewGerExp,gold=NewGold,item=NewItemList,reputation=NewReputation}.

recycle_normal_loss(Value,Rate)->
    trunc(Value*Rate/100).

%%损失道具
recycle_item_loss(ItemList)->
    {Rate,DeductList}=data_recycle:get(return_back_rate),
    lists:foldl(fun(Item,Acc)->
        #new_item{itemTypeID=ItemTypeID,itemNum=ItemNum} = Item,
        case lists:member(ItemTypeID,DeductList) of
            false->
                [Item|Acc];
            true->
                case recycle_normal_loss(ItemNum,Rate) of
                    0->
                        Acc;
                    NewItemNum->
                        [Item#new_item{itemNum=NewItemNum}|Acc]
                end
        end
    end,[],ItemList).

%%返回装备等级对应的等级增长步距
% {data_equip_level_up,[{{1,100},1},{{101,200},2.4},{{201,300},2.4}]}.
get_equip_level_increase_step(ItemLevel)->
    List = data_recycle:get(data_equip_level_up),
    {_Field,Step} = util:fun_find(fun({{Begin,End},_Step})->
        ItemLevel >=Begin andalso ItemLevel =< End
    end,List),
    Step.

%%计算Level等级装备升级所需
get_equip_level_reinforce_coin(ItemType,Level,ItemStar)->
    IntLevel = trunc(Level*10) div 10,
    FloatLevel = Level-IntLevel,
    IntCoin = role_item:cacl_reinforce_coin_ger(ItemType, IntLevel, ItemStar),
    FloatCoin = role_item:cacl_reinforce_coin_ger(ItemType, IntLevel+1, ItemStar),
    trunc(IntCoin*(1-FloatLevel)+FloatCoin*FloatLevel).

%%由于p_reward_info结构体中定义的金币，经验，钻石，徽章等的长度是32位的，此处可能会超过32位，故需修改成多个
transformPRewardInfo2PRewardInfoList(PRewardInfo,Acc)->
    #p_reward_info{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,reputation=Reputation,itemList=ItemList} = PRewardInfo,
    if
        Coin > ?DIVNUM ->
            NewPRewardInfo = PRewardInfo#p_reward_info{coin=Coin-?DIVNUM},
            AddPRewardInfo = #p_reward_info{coin=?DIVNUM,roleExp=0,gerExp=0,gold=0,reputation=0},
            transformPRewardInfo2PRewardInfoList(NewPRewardInfo,[AddPRewardInfo|Acc]);
        RoleExp>?DIVNUM->
            NewPRewardInfo = PRewardInfo#p_reward_info{roleExp=RoleExp-?DIVNUM},
            AddPRewardInfo = #p_reward_info{coin=0,roleExp=?DIVNUM,gerExp=0,gold=0,reputation=0},
            transformPRewardInfo2PRewardInfoList(NewPRewardInfo,[AddPRewardInfo|Acc]);
        GerExp>?DIVNUM->
            NewPRewardInfo = PRewardInfo#p_reward_info{gerExp=GerExp-?DIVNUM},
            AddPRewardInfo = #p_reward_info{coin=0,roleExp=0,gerExp=?DIVNUM,gold=0,reputation=0},
            transformPRewardInfo2PRewardInfoList(NewPRewardInfo,[AddPRewardInfo|Acc]);
        Gold>?DIVNUM->
            NewPRewardInfo = PRewardInfo#p_reward_info{gold=Gold-?DIVNUM},
            AddPRewardInfo = #p_reward_info{coin=0,roleExp=0,gerExp=0,gold=?DIVNUM,reputation=0},
            transformPRewardInfo2PRewardInfoList(NewPRewardInfo,[AddPRewardInfo|Acc]);
        Reputation>?DIVNUM->
            NewPRewardInfo = PRewardInfo#p_reward_info{reputation=Reputation-?DIVNUM},
            AddPRewardInfo = #p_reward_info{coin=0,roleExp=0,gerExp=0,gold=0,reputation=?DIVNUM},
            transformPRewardInfo2PRewardInfoList(NewPRewardInfo,[AddPRewardInfo|Acc]);
        true-> 
            NewPItemViewList = transformPItemView2PItemViewList(ItemList,[]),
            [PRewardInfo#p_reward_info{itemList=NewPItemViewList}|Acc]
    end.

%%v3.3.0 协议p_item_view结构中使用的itemNum长度是16位的，前端修改协议存在困难，故此处折中处理，将超过长度的p_item_view改成多个
transformPItemView2PItemViewList([],Acc)->
    Acc;
transformPItemView2PItemViewList([#p_item_view{itemNum=ItemNum}=H|T],Acc)->
    case ItemNum > 65535 of
        true->
            NewH = H#p_item_view{itemNum=ItemNum-65535},
            transformPItemView2PItemViewList([NewH|T],[H#p_item_view{itemNum=65535}|Acc]);
        false->
            transformPItemView2PItemViewList(T,[H|Acc])
    end.
    
