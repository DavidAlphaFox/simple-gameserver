%% @author dongquan
%% @doc 卡牌和装备合成系统
%% Created 2014-3-3


-module(role_combine).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_item.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  

-define(CombineTypeNormal, 1). 	%%固定合成
-define(CombineTypeRandom, 2).	%%随机合成
-define(CombineType72, 3).  %%随机合成
-define(CombinMulti, 4).       %%批量合成
-define(CombinStone, 5).       %%符文合成
-define(CombineGer, 1).		%%合成卡牌
-define(CombineEquip, 2).		%%合成装备
-define(CombineTrainerEquip,3). %%合成训练师装备
-define(CombineAccelerateEquip,4). %%合成加速装备
-define(RandomCombineNeedNum, 5).

-define(CombineSucc, 0).		                     %%合成成功
-define(CombineFailBadCombineTypeID, 1).		     %%合成失败，错误的配方ID
-define(CombineFailNotEnough, 2).		             %%合成失败，材料不足
-define(CombineFailBadCombine, 3).		             %%合成失败，合成的材料组合与配方不一致
-define(CombineFailNeedFormulaItem, 4).		         %%合成失败，缺少配方道具
-define(CombineFailCoinNotEnough, 5).                %%合成失败，coin不足
-define(CombineFailRandomCombineNeedNumError, 6).    %%合成失败，合成所需的材料数量不对
-define(CombineFailStarLevelNotSame, 7).             %%合成失败，参与合成的材料星等不一致
-define(CombineFailStarLevelLimit, 8).               %%合成失败，参与合成的材料超过最大星等限制
-define(CombineFailExpGerCanNotCombine, 9).          %%合成失败，经验卡牌不能参与合成
-define(CombineFailHomesteadGerNot, 10).             %%合成失败，家园守护神不能参与合成
-define(CombineFailQualityLimit, 11).                %%72合成失败，不是2转
-define(CombineFailMirror, 12).                      %%合成失败，家园守护神不能参与合成
-define(ComBineFailStarWa, 13).                      %%合成失败,批量合成用的星级不对
-define(ComBineFailStarNotAllow,14).                 %%合成失败，参与合成的精灵星级不允许合成
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_combine_do(#cs_combine_do{combineType=CombineType,combineTypeID=CombineTypeID,combineOutType=CombineOutType,uIDList=UIDList}) ->
	case catch check_can_combine(CombineType, CombineTypeID, CombineOutType, UIDList) of
		{normal_combine_ger, NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo} ->
            do_normal_combine_ger(NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo);
        {normal_combine_equip, NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo} ->
            do_normal_combine_equip(NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo);
        {random_combine_ger, BoxID, DelGerList, NewGerBagList,NeedCoin, GerStarLevel} ->
            do_random_combine_ger(CombineType,BoxID, DelGerList, NewGerBagList,NeedCoin, GerStarLevel);
        {random_combine_equip, BoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel} ->
            do_random_combine_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin,EquipStarLevel);
        {random_combine_trainer_equip, BoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel} ->
            do_random_combine_trainer_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin,EquipStarLevel);
	    {multi_combine_ger, BoxID, DelGerList, NewGerBagList, NeedCoin, Group} ->
            do_multi_combine_ger(BoxID, DelGerList, NewGerBagList, NeedCoin, Group);
        {multi_combine_equip, BoxID, DelEquipList, NewEquipBagList, NeedCoin, Group} ->
            do_multi_combine_equip(BoxID, DelEquipList, NewEquipBagList, NeedCoin, Group);
        {combine_stone_3to1, DelEquipList, NewEquipBagList, NeedCoin, NewStoneList, EquipExpAddition} ->
            do_stone(DelEquipList, NewEquipBagList, NeedCoin, NewStoneList, EquipExpAddition);
        {random_combine_accelerate_equip, BoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel} ->
            do_random_combine_accelerate_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel);
		{false, FailCode} ->
%%             ?ERR("FailCode:~w", [FailCode]),
			?sendself(#sc_combine_fail{result=FailCode})
	end.

do_random_combine_ger(CombineType,BoxID, DelGerList, NewGerBagList,NeedCoin,GerStarLevel) ->
    {Date, _} = Time = erlang:localtime(),
	%% newLevel -> max level
    {LogGerList,TotalQuality, NewLevel} = lists:foldl(fun(#gerSimple{gerID=GerID, gerTypeID=GerTypeID, gerQuality=GerQuality, gerLevel=GerLevel},{Acc,Sum,AccLevel}) ->
																NewLevel = 
																	if GerLevel > AccLevel ->
																		   GerLevel;
																	   true ->
															               AccLevel
																	end,
                                   {[[GerID,GerTypeID,GerLevel,GerQuality]|Acc] ,Sum+GerQuality,NewLevel}  
                           end, {[],0,0},DelGerList),
%    NewLevel = TotalLevel div length(DelGerList),
    %%?ERR("~w", [{BoxID, role_data:get_mainGerTypeID()}]),
    [RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    Crt = case CombineType of
              ?CombineType72 ->
              get_combine_crt(ger_72,GerStarLevel);
                _ ->
              get_combine_crt(ger,GerStarLevel)
          end,
    NewQuality = case CombineType of
                ?CombineType72 ->
                    20;
                _ ->
                    TotalQuality div ?RandomCombineNeedNum
          end,
    {AddGerList, ViewList,_} = lists:foldr(fun(_, {Acc,ViewAcc,X}) ->
                                     {?REWARD_GER,GerTypeID,_} = util:random_one_from_weigh_list(RConfig),
									 NewLevel2 = case X of
													 0->
														 NewLevel;
													 _ ->
														 1
												 end,
                                     NewAcc = [#new_ger{gerLevel=NewLevel2,gerQuality=NewQuality,gerTypeID=GerTypeID}|Acc],
                                     NewViewAcc = [#p_ger_view{gerQuality=NewQuality,gerLevel=NewLevel2,gerTypeID=GerTypeID}|ViewAcc],
                                    {NewAcc,NewViewAcc,1} 
            end, {[],[],0}, lists:seq(1, Crt)),
	#role{roleName=RoleName} = RoleInfo = role_data:get_roleInfo(),
    %%爆击公告
    CrtNumList = 
        case Crt of 
            2 ->
                broadcast_server:bc_msgID(10037, [RoleName]++ViewList),
                [1,0];
            3 ->
                broadcast_server:bc_msgID(10038, [RoleName]++ViewList),
                [0,1];
            _ ->
                [0,0]
        end,
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
	%%都没问题后，才扣除消耗，添加武将
	behavior_ger_consume:log(role_data:get_roleID(), LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_RANDOM_GER, 0, ""),
        ?sendself(#sc_ger_del{gerIDList=lists:map(fun(#gerSimple{gerID=GerUID}) -> GerUID end, DelGerList)}),
	role_data:set_gerBag(NewGerBagList),
	role_reward:handle_ger_f(AddGerList,  ?MONEY_ADD_TYPE_COMBINE_RANDOM_GER, 0, ""),
	role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_RANDOM_GER, 0, ""),
    notify_random_reward(AddGerList, ger, CrtNumList),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,random_combine_ger,1})),
	role_homestead:hook_ger_delete(-1,0,0,[GID||#gerSimple{gerID=GID}<-DelGerList]).

do_random_combine_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin,EquipStarLevel) ->
    {Date, _} = Time = erlang:localtime(),
    {LogItemList,TotalQuality,NewLevel} = 
        lists:foldl(fun(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum,itemRank=IR,itemLevel=ItemLevel},{Acc,Sum,AccLevel}) ->
						  NewLevel = 
						  if ItemLevel > AccLevel ->
								 ItemLevel;
							 true ->
								 AccLevel
						  end,
                        {[[EquipUID,EquipTypeID,EquipNum,EquipNum]|Acc],Sum+IR,NewLevel}
				  end, {[],0,0},DelEquipList),
	%%?ERR("~w", [{BoxID, role_data:get_mainGerTypeID()}]),
	%NewLevel = TotalLevel div length(DelEquipList),
	NewQuality = TotalQuality div ?RandomCombineNeedNum,
	[RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    Crt = get_combine_crt(equip,EquipStarLevel),
    {AddItemList, ViewList,_} =
        lists:foldr(fun(_, {Acc,ViewAcc,X}) ->
                            {?REWARD_ITEM,ItemTypeID,Num} = util:random_one_from_weigh_list(RConfig),
							NewLevel3 =
							case X of
								0->
									NewLevel;
								_ ->
									1
							end,							
                            NewAcc=[#new_item{itemLevel=NewLevel3,itemNum=Num,itemRank=NewQuality,itemTypeID=ItemTypeID}|Acc],
                            NewViewAcc = [#p_item_view{itemLevel=NewLevel3,itemNum=Num,itemRank=NewQuality,itemTypeID=ItemTypeID}|ViewAcc],
                            {NewAcc,NewViewAcc,1}
            end, {[],[],0}, lists:seq(1, Crt)),
	#role{roleName=RoleName} = RoleInfo = role_data:get_roleInfo(),
    CrtNumList = 
        case Crt of 
            2 ->
                broadcast_server:bc_msgID(10039,[RoleName]++ViewList),
                [1,0];
            3 ->
                broadcast_server:bc_msgID(10040,[RoleName]++ViewList),
                [0,1];
            _ ->
                [0,0]
        end,
    %%都没问题后,才扣除资源，添加物品
    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    role_data:set_bagEquip(NewEquipBagList),
	role_reward:handle_item_f(RoleInfo, AddItemList, ?MONEY_ADD_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
	role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    notify_random_reward(AddItemList, equip, CrtNumList),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,random_combine_equip,1})).

do_random_combine_trainer_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin,EquipStarLevel) ->
    {Date, _} = Time = erlang:localtime(),
    {LogItemList,TotalQuality,TotalLevel} = lists:foldl(fun(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum,itemRank=IR,itemLevel=ItemLevel},{Acc,Sum,AccLevel}) ->
                                    {[[EquipUID,EquipTypeID,EquipNum,EquipNum]|Acc],Sum+IR,AccLevel+ItemLevel}
                                                      end, {[],0,0},DelEquipList),
    [RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    InheritQuality = TotalQuality div ?RandomCombineNeedNum,
    InheritLevel = TotalLevel div ?RandomCombineNeedNum,
    Crt = get_combine_crt(trainer_equip,EquipStarLevel), 
    {AddItemList, ViewList,_} =
        lists:foldr(fun(_, {Acc,ViewAcc,X}) ->
                            {?REWARD_ITEM,ItemTypeID,Num} = util:random_one_from_weigh_list(RConfig),
                            NewLevel3 =
                            case X of
                                0->
                                    InheritLevel;
                                _ ->
                                    1
                            end,                            
                            NewAcc=[#new_item{itemLevel=NewLevel3,itemNum=Num,itemRank=InheritQuality,itemTypeID=ItemTypeID}|Acc],
                            NewViewAcc = [#p_item_view{itemLevel=NewLevel3,itemNum=Num,itemRank=InheritQuality,itemTypeID=ItemTypeID}|ViewAcc],
                            {NewAcc,NewViewAcc,1}
            end, {[],[],0}, lists:seq(1, Crt)),
    #role{roleName=RoleName} = RoleInfo = role_data:get_roleInfo(),
    CrtNumList = 
        case Crt of
            2 ->
                [1,0];
            3 ->
                broadcast_server:bc_msgID(10040,[RoleName]++ViewList),
                [0,1];
            _ ->
                [0,0]
        end,
    %%都没问题后,才扣除资源，添加物品
    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    role_data:set_bagEquip(NewEquipBagList),
    role_reward:handle_item_f(RoleInfo, AddItemList, ?MONEY_ADD_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    notify_random_reward(AddItemList, equip, CrtNumList).
    % ?CATCH(role_task_trigger:handle({dispach_task,role_combime,random_combine_equip,1})).

do_random_combine_accelerate_equip(BoxID, DelEquipList, NewEquipBagList,NeedCoin,EquipStarLevel) ->
    {Date, _} = Time = erlang:localtime(),
    {LogItemList,TotalQuality,TotalLevel} = lists:foldl(fun(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum,itemRank=IR,itemLevel=ItemLevel},{Acc,Sum,AccLevel}) ->
                                    {[[EquipUID,EquipTypeID,EquipNum,EquipNum]|Acc],Sum+IR,AccLevel+ItemLevel}
                                                      end, {[],0,0},DelEquipList),
    [RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    InheritQuality = TotalQuality div ?RandomCombineNeedNum,
    InheritLevel = TotalLevel div ?RandomCombineNeedNum,
    Crt = get_combine_crt(accelerate_equip,EquipStarLevel), 
    {AddItemList, ViewList,_} =
        lists:foldr(fun(_, {Acc,ViewAcc,X}) ->
                            {?REWARD_ITEM,ItemTypeID,Num} = util:random_one_from_weigh_list(RConfig),
                            NewLevel3 =
                            case X of
                                0->
                                    InheritLevel;
                                _ ->
                                    1
                            end,                            
                            NewAcc=[#new_item{itemLevel=NewLevel3,itemNum=Num,itemRank=InheritQuality,itemTypeID=ItemTypeID}|Acc],
                            NewViewAcc = [#p_item_view{itemLevel=NewLevel3,itemNum=Num,itemRank=InheritQuality,itemTypeID=ItemTypeID}|ViewAcc],
                            {NewAcc,NewViewAcc,1}
            end, {[],[],0}, lists:seq(1, Crt)),
    #role{roleName=RoleName} = RoleInfo = role_data:get_roleInfo(),
    CrtNumList = 
        case Crt of
            2 ->
                [1,0];
            3 ->
                broadcast_server:bc_msgID(10040,[RoleName]++ViewList),
                [0,1];
            _ ->
                [0,0]
        end,
    %%都没问题后,才扣除资源，添加物品
    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    role_data:set_bagEquip(NewEquipBagList),
    role_reward:handle_item_f(RoleInfo, AddItemList, ?MONEY_ADD_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_RANDOM_EQUIP, 0, ""),
    notify_random_reward(AddItemList, equip, CrtNumList).
    % ?CATCH(role_task_trigger:handle({dispach_task,role_combime,random_combine_equip,1})).

notify_random_reward(List, Type)->
    notify_random_reward(List, Type, [0,0]).
notify_random_reward(List, Type, [Crt2,Crt3]) 
  when erlang:is_integer(Crt2) andalso erlang:is_integer(Crt3) ->
    case Type of
        equip ->
            notify_random_reward_equip(List, [Crt2,Crt3]);
        ger ->
            notify_random_reward_ger(List, [Crt2,Crt3])
    end;
notify_random_reward(List, Type, _) ->
    notify_random_reward(List, Type, [0,0]).

notify_random_reward_equip(NewItem, CrtNumList) ->
    case erlang:is_list(NewItem) of
        true ->
            List =
                lists:map(fun(#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank}) ->
                                  #p_newEquip{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank}
                          end, NewItem);
        false ->
            #new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank} = NewItem,
            List = [#p_newEquip{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank}]
    end,
    ?sendself(#sc_combine_equip{newEquip=List,crt_num_list=CrtNumList}).


notify_random_reward_ger(NewGer, CrtNumList) ->
    case erlang:is_list(NewGer) of
        true ->
            List =
                lists:map(fun(#new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}) ->
                                  #p_newGer{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}
                          end, NewGer);
        false ->
            #new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality} = NewGer,
            List = [#p_newGer{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}]
    end,
    ?sendself(#sc_combine_ger{newGer=List,crt_num_list=CrtNumList}).

do_normal_combine_ger(NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, #role{roleID=RoleID}=RoleInfo) ->
    {Date, _} = Time = erlang:localtime(),
    role_data:set_bagItem(NewItemBagList),
    notify_bagItem_and_log(RoleID, UpdateItemBagList, DelItemBagList, ?MONEY_DEC_TYPE_COMBINE_NORMAL_GER, Date, Time),
    
    role_data:set_gerBag(NewBagGerList),
    LogGerList = lists:map(fun(#gerSimple{gerID=GerID, gerTypeID=GerTypeID, gerQuality=GerQuality, gerLevel=GerLevel}) ->
                                [GerID,GerTypeID,GerLevel,GerQuality]   
                           end, DelGerList),
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
    behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_NORMAL_GER, 0, ""),
    ?sendself(#sc_ger_del{gerIDList=lists:map(fun(#gerSimple{gerID=GerUID}) -> GerUID end, DelGerList)}),
    
    role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_NORMAL_GER, 0, ""),
    
    NewGer = #new_ger{gerTypeID=CombineDestTypeID, gerLevel=NewLevel, gerQuality=NewRank},
    ger_lib:add_ger_list([NewGer], ?MONEY_ADD_TYPE_COMBINE_NORMAL_GER, 0, ""),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,normal_combine_ger})),
    ?sendself(#sc_combine_ger{newGer=[#p_newGer{gerTypeID=CombineDestTypeID, gerLevel=NewLevel, gerQuality=NewRank}]}),
	role_homestead:hook_ger_delete(-1,0,0,[GID||#gerSimple{gerID=GID}<-DelGerList]).

do_normal_combine_equip(NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, #role{roleID=RoleID}=RoleInfo) ->
    {Date, _} = Time = erlang:localtime(),
    role_data:set_bagItem(NewItemBagList),
    notify_bagItem_and_log(RoleID, UpdateItemBagList, DelItemBagList, ?MONEY_DEC_TYPE_COMBINE_NORMAL_EQUIP, Date, Time),
    
    role_data:set_bagEquip(NewBagEquipList),
    LogItemList = lists:map(fun(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum}) ->
                                    [EquipUID,EquipTypeID,EquipNum,EquipNum]
                            end, DelEquipList),
    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_NORMAL_EQUIP, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    
    role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_NORMAL_EQUIP, 0, ""),
    
    NewEquip = #new_item{itemTypeID=CombineDestTypeID,itemNum=1,itemLevel=NewLevel,itemRank=NewRank},
    item_lib:add_item_f([NewEquip], ?MONEY_ADD_TYPE_COMBINE_NORMAL_EQUIP, 0, ""),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,normal_combine_equip})),
    ?sendself(#sc_combine_equip{newEquip=[#p_newEquip{itemTypeID=CombineDestTypeID, itemNum=1, itemLevel=NewLevel, itemRank=NewRank}]}).

do_stone(DelEquipList, NewEquipBagList, NeedCoin, NewStoneList,EquipExpAddition)->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    {Date, _} = Time = erlang:localtime(),
    role_data:set_bagEquip(NewEquipBagList),
    LogItemList = lists:map(fun(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum}) ->
                                    [EquipUID,EquipTypeID,EquipNum,EquipNum]
                            end, DelEquipList),
    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_STONE_3TO1, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    
   	role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_STONE_3TO1, 0, ""),
    item_lib:add_item_f(NewStoneList, ?MONEY_ADD_TYPE_STONE_3TO1, 0, "", true,EquipExpAddition),
    NewEquipList = [#p_newEquip{itemTypeID=CombineDestTypeID, itemNum=1, itemLevel=NewLevel, itemRank=NewRank}
                   ||#new_item{itemTypeID=CombineDestTypeID,itemNum=1,itemLevel=NewLevel,itemRank=NewRank}<-NewStoneList],
    ?sendself(#sc_combine_equip{newEquip=NewEquipList}).

notify_bagItem_and_log(RoleID, UpdateItemBagList, DelItemBagList, ConsumeType, Date, Time) ->
    case UpdateItemBagList of
        [] ->
            next;
        _ ->
%%             ?ERR("~w", [UpdateItemBagList]),
            UpdateList = lists:map(fun(#item{itemUID=UID, itemNum=Num}) ->
                                      #p_item_num_update{itemNum=Num, itemUID=UID}     
                                   end, UpdateItemBagList),
            ?sendself(#sc_item_update{updateList=UpdateList}),
            
            LogList1 = lists:map(fun(#item{itemUID=UID, itemTypeID=ItemTypeID, itemNum=Num}) ->
                                         [UID, ItemTypeID, 1, Num + 1]
                                 end, UpdateItemBagList),
            behavior_item_consume:log(RoleID, LogList1, Date, Time, ConsumeType, 0, "")
    end,
    case DelItemBagList of
        [] ->
            next;
        _ ->
%%             ?ERR("~w", [DelItemBagList]),
            UIDList = lists:map(fun(#item{itemUID=UID}) ->
                                        UID
                                end, DelItemBagList),
            ?sendself(#sc_item_delete_notify{itemUIDList=UIDList}),
            
            LogList2 = lists:map(fun(#item{itemUID=UID, itemTypeID=ItemTypeID, itemNum=Num}) ->
                                         [UID, ItemTypeID, Num, Num]
                                 end, DelItemBagList),
            behavior_item_consume:log(RoleID, LogList2, Date, Time, ConsumeType, 0, "")
    end.

check_can_combine(CombineType, CombineTypeID, CombineOutType, UIDList) ->
    ?INFO("CombineType:~w, CombineTypeID:~w, CombineOutType:~w, UIDList:~w", [CombineType, CombineTypeID, CombineOutType, UIDList]),
	case CombineType of
		?CombineTypeNormal ->
            check_combine_normal(CombineTypeID, UIDList);
		?CombineTypeRandom ->
			check_combine_random(CombineOutType, UIDList);
        ?CombineType72->
            check_combine_72(UIDList);
        ?CombinMulti ->
            check_combine_multi(CombineOutType, UIDList);
        ?CombinStone ->
            check_combine_stone(UIDList)
	end.

check_combine_random(CombineOutType, UIDList) ->
    case CombineOutType of
        ?CombineGer ->
            GerBagList = role_data:get_gerBag(),
            {DelGerList, NewGerBagList} =
                lists:foldr(fun(UID, {AccDelGerList, AccGerBagList}) ->
                                    case lists:keytake(UID, #gerSimple.gerID, AccGerBagList) of
                                        {value, #gerSimple{gerTypeID=DelGerTypeID}=DelGer, NewAccGerBagList} ->
                                            #data_ger{gerStar=Star} = data_ger:get(DelGerTypeID),
                                            case util:is_exp_card(DelGerTypeID) of
                                                false ->
                                                    case lists:member(Star,data_combine_random:get(random_combine_star_list)) of
                                                        true-> 
                                                            {[DelGer|AccDelGerList], NewAccGerBagList};
                                                        false->
                                                            erlang:throw({false,?ComBineFailStarNotAllow})
                                                    end;
                                                true ->
                                                    erlang:throw({false, ?CombineFailExpGerCanNotCombine})
                                            end;
                                        false ->
                                            case role_ger:is_mirror_ger(UID) of
                                                false ->
                                                    erlang:throw({false, ?CombineFailNotEnough});
                                                true ->
                                                    erlang:throw({false, ?CombineFailMirror})
                                            end     
                                    end
                            end, {[], GerBagList}, UIDList),
            case erlang:length(DelGerList) =:= ?RandomCombineNeedNum of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
            end,
            GerStarLevel = check_ger_star_level(DelGerList),
            case GerStarLevel =< data_combine_random:get(max_ger_star_level) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailStarLevelLimit})
            end,
            {BoxID, NeedCoin} = data_combine_random:get({ger, GerStarLevel}),
            case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailCoinNotEnough})
            end,
			RoleID = role_data:get_roleID(),
			case homestead_server:has_homestead_ger(RoleID, DelGerList) of
				true->
					erlang:throw({false, ?CombineFailHomesteadGerNot});
				false->
					{random_combine_ger, BoxID, DelGerList, NewGerBagList,NeedCoin, GerStarLevel}
			end;
        ?CombineEquip ->
            EquipBagList = role_data:get_bagEquip(),
            {DelEquipList, NewEquipBagList} =
                lists:foldr(fun(UID, {AccDelEquipList, AccEquipBagList}) ->
                                    case lists:keytake(UID, #item.itemUID, AccEquipBagList) of
                                        {value, #item{itemType=ItemType}=DelEquip, NewAccEquipBagList} ->
                                            case lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE) of
                                                true->
                                                    {[DelEquip|AccDelEquipList], NewAccEquipBagList};
                                                false->
                                                    erlang:throw({false, ?CombineFailNotEnough})
                                            end;
                                        false ->
                                            erlang:throw({false, ?CombineFailNotEnough})
                                    end
                            end, {[], EquipBagList}, UIDList),
            case erlang:length(DelEquipList) =:= ?RandomCombineNeedNum of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
            end,
            EquipStarLevel = check_equip_star_level(DelEquipList),
            case EquipStarLevel =< data_combine_random:get(max_equip_star_level) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailStarLevelLimit})
            end,
			{BoxID, NeedCoin} = data_combine_random:get({equip, EquipStarLevel}),
			LBoxID = case EquipStarLevel of 3 -> XCount = role_box:update_tab_limitInfo_value(2),
												 case data_fixed_box_withitem:get({combine2,XCount}) of
													 ?undefined -> BoxID;
													 GBoxID -> GBoxID
												 end;
						 _ ->
							 BoxID
					 end,
            case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailCoinNotEnough})
            end,
            {random_combine_equip, LBoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel};
        ?CombineTrainerEquip->
            EquipBagList = role_data:get_bagEquip(),
            {DelEquipList, NewEquipBagList} =
                lists:foldr(fun(UID, {AccDelEquipList, AccEquipBagList}) ->
                                    case lists:keytake(UID, #item.itemUID, AccEquipBagList) of
                                        {value, #item{itemType=ItemType} = DelEquip, NewAccEquipBagList} ->
                                            case lists:member(ItemType,?TRAINER_EQUIP_TYPE) of
                                                true->
                                                    {[DelEquip|AccDelEquipList], NewAccEquipBagList};
                                                false->
                                                    erlang:throw({false, ?CombineFailNotEnough})
                                            end;
                                        false ->
                                            erlang:throw({false, ?CombineFailNotEnough})
                                    end
                            end, {[], EquipBagList}, UIDList),
            case erlang:length(DelEquipList) =:= ?RandomCombineNeedNum of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
            end,
            EquipStarLevel = check_equip_star_level(DelEquipList),
            case EquipStarLevel =< data_combine_random:get(max_trainer_equip_star_level) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailStarLevelLimit})
            end,
            {BoxID, NeedCoin} = data_combine_random:get({trainer_equip, EquipStarLevel}),
            % LBoxID = case EquipStarLevel of 2 -> XCount = role_box:update_tab_limitInfo_value(2),
            %                                      case data_fixed_box_withitem:get({combine2,XCount}) of
            %                                          ?undefined -> BoxID;
            %                                          GBoxID -> GBoxID
            %                                      end;
            %              _ ->
            %                  BoxID
            %          end,
            LBoxID = BoxID,
            case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailCoinNotEnough})
            end,
            {random_combine_trainer_equip, LBoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel};
        ?CombineAccelerateEquip->
            do_combine_accelerate_equip(UIDList)
    end.

do_combine_accelerate_equip(UIDList)->
            EquipBagList = role_data:get_bagEquip(),
            {DelEquipList, NewEquipBagList} =
                lists:foldr(fun(UID, {AccDelEquipList, AccEquipBagList}) ->
                                    case lists:keytake(UID, #item.itemUID, AccEquipBagList) of
                                        {value, #item{itemType=ItemType} = DelEquip, NewAccEquipBagList} ->
                                            case item_lib:is_accelerate_equip(ItemType) of
                                                true->
                                                    {[DelEquip|AccDelEquipList], NewAccEquipBagList};
                                                false->
                                                    erlang:throw({false, ?CombineFailNotEnough})
                                            end;
                                        false ->
                                            erlang:throw({false, ?CombineFailNotEnough})
                                    end
                            end, {[], EquipBagList}, UIDList),
            case erlang:length(DelEquipList) =:= ?RandomCombineNeedNum of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
            end,
            EquipStarLevel = check_equip_star_level(DelEquipList),
            case EquipStarLevel =< data_combine_random:get(max_accelerate_equip_star_level) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailStarLevelLimit})
            end,
            {LBoxID, NeedCoin} = data_combine_random:get({accelerate_equip, EquipStarLevel}),
            case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
                true ->
                    next;
                false ->
                    erlang:throw({false, ?CombineFailCoinNotEnough})
            end,
            {random_combine_accelerate_equip, LBoxID, DelEquipList, NewEquipBagList,NeedCoin, EquipStarLevel}.

check_combine_72(UIDList) ->
        GerBagList = role_data:get_gerBag(),
        %检查材料是不是存在
        {DelGerList, NewGerBagList} =
            lists:foldr(fun(UID, {AccDelGerList, AccGerBagList}) ->
                                case lists:keytake(UID, #gerSimple.gerID, AccGerBagList) of
                                    {value, #gerSimple{gerTypeID=DelGerTypeID,gerQuality=DelGerQuality}=DelGer, NewAccGerBagList} ->
                                        case util:is_exp_card(DelGerTypeID) of
                                            false ->
                                                % #data_ger{gerStar=GerStar} = data_ger:get(DelGerTypeID),
                                                if
                                                    DelGerQuality =:= 20 ->
                                                        {[DelGer|AccDelGerList], NewAccGerBagList};
                                                    true ->
                                                        erlang:throw({false, ?CombineFailQualityLimit})
                                                end;
                                            true ->
                                                erlang:throw({false, ?CombineFailExpGerCanNotCombine})
                                        end;
                                    false ->
                                        case role_ger:is_mirror_ger(UID) of
                                            false ->
                                                erlang:throw({false, ?CombineFailNotEnough});
                                            true ->
                                                erlang:throw({false, ?CombineFailMirror})
                                        end
                                end
                        end, {[], GerBagList}, UIDList),
        %检查材料数量对不对
        case erlang:length(DelGerList) =:= 3 of
            true ->
                next;
            false ->
                erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
        end,
        GerStarLevel = check_ger_star_level(DelGerList),
        case 5 =< GerStarLevel andalso GerStarLevel =< data_combine_random:get(max_ger_star_level) of
            true ->
                next;
            false ->
                erlang:throw({false, ?CombineFailStarLevelLimit})
        end,
        {BoxID, NeedCoin} = data_combine_random:get({ger_72, GerStarLevel}),
        case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
            true ->
                next;
            false ->
                erlang:throw({false, ?CombineFailCoinNotEnough})
        end,
        RoleID = role_data:get_roleID(),
        case homestead_server:has_homestead_ger(RoleID, DelGerList) of
            true->
                erlang:throw({false, ?CombineFailHomesteadGerNot});
            false->
                {random_combine_ger, BoxID, DelGerList, NewGerBagList,NeedCoin, GerStarLevel}
        end.

check_ger_star_level([#gerSimple{gerTypeID=GerTypeID}|GerList]) ->
    #data_ger{gerStar=SpecStarLevel} = data_ger:get(GerTypeID),
    check_ger_star_level(GerList, SpecStarLevel).

check_ger_star_level([], SpecStarLevel) ->
    SpecStarLevel;
check_ger_star_level([#gerSimple{gerTypeID=GerTypeID}|GerList], SpecStarLevel) ->
    #data_ger{gerStar=StarLevel} = data_ger:get(GerTypeID),
    case StarLevel of
        SpecStarLevel ->
            check_ger_star_level(GerList, SpecStarLevel);
        _ ->
            erlang:throw({false, ?CombineFailStarLevelNotSame})
    end.

check_equip_star_level([#item{itemTypeID=ItemTypeID}|EquipList]) ->
    #data_item{itemStar=SpecStarLevel} = data_item:get(ItemTypeID),
    check_equip_star_level(EquipList, SpecStarLevel).

check_equip_star_level([], SpecStarLevel) ->
    SpecStarLevel;
check_equip_star_level([#item{itemTypeID=ItemTypeID}|EquipList], SpecStarLevel) ->
    #data_item{itemStar=StarLevel} = data_item:get(ItemTypeID),
    case StarLevel of
        SpecStarLevel ->
            check_equip_star_level(EquipList, SpecStarLevel);
        _ ->
            erlang:throw({false, ?CombineFailStarLevelNotSame})
    end.

check_combine_normal(CombineTypeID, UIDList) ->
	DataCombine = data_combine:get(CombineTypeID),
    case erlang:is_record(DataCombine, data_combine) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailBadCombineTypeID})
    end,
    #data_combine{coin=NeedCoin, formula_itemID=FormulaItemTypeID, combine_ID=CombineDestTypeID, combine_out_type=OutType} = DataCombine,
    ItemBagList = role_data:get_bagItem(),
%%     ?ERR("ItemBagList:~w", [ItemBagList]),
%%     ?ERR("~w", [role_data:get_bagEquip()]),
    {NewItemBagList, UpdateItemBagList, DelItemBagList} =
        case lists:keytake(FormulaItemTypeID, #item.itemTypeID, ItemBagList) of
            false ->
%%                 ?ERR("FormulaItemTypeID:~w", [FormulaItemTypeID]),
                erlang:throw({false, ?CombineFailNeedFormulaItem});
            {value, #item{itemNum=FormulaItemNum}=FormulaItem, ItemBagList2} ->
                case FormulaItemNum > 1 of
                    true ->
                        {[FormulaItem#item{itemNum=FormulaItemNum-1}|ItemBagList2], [FormulaItem#item{itemNum=FormulaItemNum-1}], []};
                    false ->
                        {ItemBagList2, [], [FormulaItem]}
                end
        end,
    RoleInfo = role_data:get_roleInfo(),
    case role_lib:check_money(RoleInfo, coin, NeedCoin) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailCoinNotEnough})
    end,
    case OutType of
        ?CombineGer ->
            check_combine_normal_ger(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine);
        ?CombineEquip ->
            check_combine_normal_equip(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine)
    end.

check_combine_normal_ger(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine) ->
    {NewBagGerList, DelGerList} = check_ger_enough(DataCombine, UIDList),
    NewRank = get_new_ger_rank(DelGerList),
    NewLevel = get_new_ger_level(DelGerList),
	RoleID = role_data:get_roleID(),
	case homestead_server:has_homestead_ger(RoleID, DelGerList) of
		true->
			erlang:throw({false, ?CombineFailHomesteadGerNot});
		false->
    		{normal_combine_ger, NewBagGerList, DelGerList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo}
	end.

check_combine_normal_equip(NewItemBagList, UpdateItemBagList, DelItemBagList, CombineDestTypeID, RoleInfo, NeedCoin, UIDList, DataCombine) ->
    {NewBagEquipList, DelEquipList} = check_equip_enough(DataCombine, UIDList),
    NewRank = get_new_equip_rank(DelEquipList),
    NewLevel = get_new_equip_level(DelEquipList),
    {normal_combine_equip, NewBagEquipList, DelEquipList, NewItemBagList, UpdateItemBagList, DelItemBagList, NeedCoin, CombineDestTypeID, NewRank, NewLevel, RoleInfo}.
    
get_new_ger_rank(GerList) ->
    RankSum = lists:foldr(fun(#gerSimple{gerQuality=Rank}, Acc) ->
                                  Acc + Rank
                          end, 0, GerList),
    erlang:trunc(RankSum / ?RandomCombineNeedNum).

get_new_ger_level(GerList) ->
    TotalLevel = lists:foldr(fun(#gerSimple{gerLevel=GerLevel}, AccLevel) ->
            AccLevel + GerLevel
                end, 0, GerList),
    TotalLevel div length(GerList).

get_new_equip_rank(EquipList) ->
    {RankSum, _Num} = lists:foldr(fun(#item{itemRank=Rank, itemNum=Num}, {AccRankSum, AccNum}) ->
                                         {AccRankSum + Rank * Num, AccNum + Num}
                                 end, {0, 0}, EquipList),
    erlang:trunc(RankSum / ?RandomCombineNeedNum).

get_new_equip_level(EquipList) ->
    TotalLevel=lists:foldr(fun(#item{itemLevel=ItemLevel}, AccLevel) ->
            AccLevel + ItemLevel
                end, 0, EquipList),
    TotalLevel div length(EquipList).

check_ger_enough(DataCombine, UIDList) ->
    #data_combine{need_item_list=NeedGerTypeIDList} = DataCombine,
    BagGerList = role_data:get_gerBag(),
    {NewBagGerList, DelGerList} =
        lists:foldr(fun(GerUID, {AccBagGerList, AccDelGerList}) ->
                            case lists:keytake(GerUID, #gerSimple.gerID, AccBagGerList) of
                                {value, DelGer, NewAccBagGerList} ->
                                    {NewAccBagGerList, [DelGer|AccDelGerList]};
                                false ->
                                    case role_ger:is_mirror_ger(GerUID) of
                                        false ->
                                            erlang:throw({false, ?CombineFailNotEnough});
                                        true ->
                                            erlang:throw({false, ?CombineFailMirror})
                                    end
                            end
                    end, {BagGerList, []}, UIDList),
    GerTypeIDList =
        lists:map(fun(#gerSimple{gerTypeID=GerTypeID}) ->
                          GerTypeID
                  end, DelGerList),
    case lists:sort(NeedGerTypeIDList) =:= lists:sort(GerTypeIDList) of
        true ->
            {NewBagGerList, DelGerList};
        false ->
            erlang:throw({false, ?CombineFailBadCombine})
    end.

check_equip_enough(DataCombine, UIDList) ->
    #data_combine{need_item_list=NeedEquipTypeIDList} = DataCombine,
    BagEquipList = role_data:get_bagEquip(),
    {NewBagEquipList, DelEquipList} =
        lists:foldr(fun(EquipUID, {AccBagEquipList, AccDelEquipList}) ->
                            case lists:keyfind(EquipUID, #item.itemUID, AccBagEquipList) of
                                false ->
%%                                     ?ERR("EquipUID:~w", [EquipUID]),
                                    erlang:throw({false, ?CombineFailNotEnough});
                                #item{itemNum=Num}=NeedDelEquip ->
                                    case Num > 1 of
                                        true ->
                                            NewAccBagEquipList = lists:keyreplace(EquipUID, #item.itemUID, AccBagEquipList, NeedDelEquip#item{itemNum=Num-1});
                                        false ->
                                            NewAccBagEquipList = lists:keydelete(EquipUID, #item.itemUID, AccBagEquipList)
                                    end,
                                    NewAccDelEquipList = 
                                        case lists:keyfind(EquipUID, #item.itemUID, AccDelEquipList) of
                                            false ->
                                                [NeedDelEquip#item{itemNum=1}|AccDelEquipList];
                                            #item{itemNum=DelNum}=DelEquip ->
                                                [DelEquip#item{itemNum=DelNum+1}|AccDelEquipList]
                                        end,
                                    {NewAccBagEquipList, NewAccDelEquipList}
                            end
                    end, {BagEquipList, []}, UIDList),
    EquipTypeIDList =
        lists:foldr(fun(#item{itemTypeID=ItemTypeID,itemNum=ItemNum}, AccEquipTypeIDList) ->
                            lists:duplicate(ItemNum, ItemTypeID) ++ AccEquipTypeIDList
                    end, [], DelEquipList),
    case lists:sort(NeedEquipTypeIDList) =:= lists:sort(EquipTypeIDList) of
        true ->
            {NewBagEquipList, DelEquipList};
        false ->
            erlang:throw({false, ?CombineFailBadCombine})
    end.

cs_combine_info(_) ->
    EndTime = case get_cur_open_time_interval( ) of 
       {0,0,_} ->
           0;
       {_,End,_} ->
           util:datetime_to_seconds(End)
        end, 
    ?sendself(#sc_combine_info{stopTime=EndTime,
                               content=data_combine_crt:get(content),
                               gerStarList=get_star_list(ger),
                               equipStarList=get_star_list(equip)}).

cs_combine_mage_info(_)->
    {MageList}=data_combine_mage:get(mage_list),
    ConfigList = 
        lists:map(fun(ConfigId)->
                        {{NeedOneGerType,NeedOneGerQuality},NeedList} = data_combine_mage:get(ConfigId),
                        #sell_reward{coin=Coin,gold=Gold,item=ItemList,reputation=Reputation,newGer=NewGer} = NeedList,
                        ItemUnitList = [#p_item_unit{itemtypeID=ItemTypeID,itemnum=ItemNum,itemlevel=ItemLevel,itemrank=ItemRank}||{new_item,ItemTypeID,ItemNum,ItemLevel,ItemRank}<-ItemList],
                        GerUnitList0 = [#p_ger_unit{gertypeID=GerTypeID,gernum=1,gerlevel=GerLevel,gerquality=GerQuality}||{new_ger,GerTypeID,GerLevel,GerQuality}<-NewGer],
                        GerUnitList = [#p_ger_unit{gertypeID=NeedOneGerType,gernum=1,gerlevel=0,gerquality=NeedOneGerQuality}|GerUnitList0],
                        {p_mage_config,ConfigId,#p_sell_reward_unit{goldnum=Gold,coinnum=Coin,reputationnum=Reputation,itemlist=ItemUnitList,gerlist=GerUnitList}}
                    end, MageList),
    ?INFO("cs_combine_mage_info ~w",[ConfigList]),
    ?sendself(#sc_combine_mage_info{config_list=ConfigList}).

cs_combine_do_mage(#cs_combine_do_mage{gerTypeID=GerTypeID,gerUidID=GerUidID})->
    case do_cs_combine_do_mage(GerTypeID,GerUidID) of
        {false,Reason}->
            ?sendself(#sc_combine_do_mage{result=Reason,pos=0,newGerID=0,return_reward = #p_reward_info{coin=0,roleExp=0,gerExp=0,gold=0,reputation=0,itemList=[],gerList=[]}});
        {true,Pos,NewGerID,_ReturnReward}->
            % ItemList = [#new_item{itemLevel=1,itemNum=4500,itemRank=0,itemTypeID=20071}],
            % Role=role_data:get_roleInfo(),
            % role_reward:handle_sys_reward(Role, ReturnReward, ?MONEY_ADD_TYPE_AWEAK_RETURN_RESOURCE, GerTypeID, integer_to_list(GerUidID)),
            % ?sendself(#sc_combine_do_mage{result=1,pos=Pos,newGerID=NewGerID
            %                              ,return_reward= activity_server:sell_reward2p_reward_info(ReturnReward)})
            %v410版本取消返回材料
            ?sendself(#sc_combine_do_mage{result=1,pos=Pos,newGerID=NewGerID
                                         ,return_reward= #p_reward_info{coin=0,roleExp=0,gerExp=0,gold=0,reputation=0}})
    end.

do_cs_combine_do_mage(GerTypeID,GerUidID)->
    case role_ger:is_mirror_ger(GerUidID) of
        true->
            {false,4};
        false->
            case role_data:get_ger(GerUidID) of
                {value, SrcGer, PosList2, LPosList2, GerBag2, Type}->
                    case do_cs_combine_do_mage2(SrcGer,GerBag2,GerTypeID,Type) of
                        {true,OtherBagItem, DelItem, UpdataItem, UpdateItemLog,DelGerList,NewGerBag,NeedList,AddGer}->
                            % Role=role_data:get_roleInfo(),
                            role_payGuide:do_mega(AddGer#new_ger.gerTypeID),
                            %%发放mega后的精灵
                            [#gerSimple{gerID=NewGerID,gerTypeID=NewGerTypeID,gerLevel=NewGerLevel,gerQuality=NewGerQuality}=NewGer] = role_reward:handle_ger_f([AddGer],?MONEY_ADD_TYPE_COMBINE_MAGE, 0, ""),
                            %%更新背包列表
                            role_data:set_bagItem(OtherBagItem),
                            %%更新玩家的货币
                            RoleInfo=#role{roleID=RoleID}=role_data:get_roleInfo(),
                            #sell_reward{coin=Coin,gold=Gold,reputation=Reputation} = NeedList,
                            RoleInfo2 =role_lib:deduct_coin_f(RoleInfo, Coin, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, ""),
                            RoleInfo3 =role_lib:deduct_gold_f(RoleInfo2, Gold, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, ""),
                            RoleInfo4 =role_lib:deduct_reputation_f(RoleInfo3, Reputation, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, ""),
                            role_data:set_roleInfo(RoleInfo4),
                            case Type of
                                ger->
                                    %%穿戴神器宝石的精灵，mega之后，神器等级清零，宝石放回背包
                                    role_diamond:demount_all_diamond_for_ger(SrcGer),
                                    %%更新玩家的背包精灵
                                    role_data:set_gerBag(NewGerBag),
                                    OldGerEquipList = role_data:get_equip(GerUidID),
                                    role_data:set_equip(NewGerID,NewGerTypeID,OldGerEquipList),
                                    NewGerSimple = role_crystal:refresh_gercrystal(NewGer),
                                    OldGerPos = SrcGer#ger.gerBase#gerBase.gerPos,
                                    NewGer2 =  ger_attr:recacl(NewGerSimple#gerSimple{gerPos=OldGerPos}, PosList2),
                                    PosList3 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
                                    NewPos = OldGerPos,
                                    role_data:set_posList(PosList3),
                                    role_payGuide:trigger_formation_ger_change(0);
                                lieu->
                                    %%穿戴神器宝石的精灵，mega之后，神器等级清零，宝石放回背包
                                    role_diamond:demount_all_diamond_for_ger(SrcGer),
                                    %%更新玩家的背包精灵
                                    role_data:set_gerBag(NewGerBag),
                                    NewGerSimple = role_crystal:refresh_gercrystal(NewGer),
                                    OldGerPos = SrcGer#ger.gerBase#gerBase.gerPos,
                                    NewGer2=ger_attr:recacl_lieu(NewGerSimple#gerSimple{gerPos=OldGerPos}, LPosList2),
                                    LPosList3 = [NewGer2|LPosList2],
                                    OldGerTypeID = SrcGer#ger.gerBase#gerBase.gerTypeID,
                                    if 
                                        GerTypeID == OldGerTypeID ->
                                            IsNeedCalc = false,
                                            LPosList4 = LPosList3;
                                        true ->
                                            %% 重算天命
                                            LPosList4 = [NewGer2 | [role_ger:destiny_impact_lieu(E,OldGerTypeID, GerTypeID,LPosList3)||E<-LPosList2]],
                                            IsNeedCalc = true
                                    end,
                                    role_data:set_lieuposList(LPosList4),
                                    case IsNeedCalc of
                                        true ->
                                            role_ger:calc_destiny_num( );
                                        _ ->
                                            ignore
                                    end,
                                    % OldGerQuality = SrcGer#ger.gerBase#gerBase.gerQuality,
                                    OldGerTypeID = SrcGer#ger.gerBase#gerBase.gerTypeID,
                                    %%此处增加newlieuGerPos方便前端区分是否为小伙伴mega
                                    NewPos = OldGerPos+?newlieuGerPos;
                                bag->
                                    %%背包精灵不能穿戴神器宝石，故不需要返回背包
                                    %更新玩家的背包精灵
                                    NewGerSimple = role_crystal:refresh_gercrystal(NewGer),
                                    NewPos=0,
                                    role_data:set_gerBag([NewGerSimple|NewGerBag])
                            end,
                            {Date, _} = Time = erlang:localtime(),
                            #role{roleID=RoleID} = role_data:get_roleInfo(),
                            role_gather:hook_add_ger_list([{GerTypeID,0}]),
                            role_gather:hook_add_ger_list_for_manual([{GerTypeID,0}]),
                            if 
                                UpdateItemLog =:= [] andalso DelItem =:= [] ->
                                    ignore;
                                true ->
                                    %% 写道具日志
                                    LogItemList = role_item:itemList2logItemList(DelItem, UpdateItemLog),
                                    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MAGE, 1, ""),
                                    DelItemIDList = [E||#item{itemUID=E}<-DelItem],
                                    UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdataItem],
                                    %% 提醒客户端更新物品
                                    ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
                                    ?sendself(#sc_item_update{updateList=UpdateList})
                            end,

                            %%更新家园守护兽
                            case homestead_server:has_homestead_ger(RoleID, [GerUidID]) of
                                false->
                                    ignore;
                                true->
                                    homestead_server:homestead_change_ger(RoleID,NewGerID,NewGerTypeID,NewGerLevel,NewGerQuality)
                            end,
                            LogGerList = role_ger:gerList2logGerList(DelGerList),
                            [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
                            behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, ""),
                            DelGerIDList = lists:foldl(fun(G,Acc)->case is_record(G,gerSimple)of true->[G#gerSimple.gerID|Acc];false->[G#ger.gerID|Acc] end end,[],DelGerList),
                            ?sendself(#sc_ger_del{gerIDList=DelGerIDList}),
                            % %%此处mega要返还原始精灵升阶消耗的材料（金币不返还）
                            % #data_ger{gerStar=Star} = data_ger:get(OldGerTypeID),
                            % ReturnReward = role_item:calculate_ger_quality_decompose_obtain(Star,OldGerQuality,#sell_reward{},ger_lib:is_blink_ger(OldGerTypeID),OldGerTypeID),
                            % ?ERR("ReturnReward:~w Star:~w GerTypeID:~w OldGerQuality:~w ~n",[ReturnReward,Star,GerTypeID,OldGerQuality]),
                            {true,NewPos,NewGerID,#sell_reward{}};
                            % 410取消材料返回
                            % {true,NewPos,NewGerID,ReturnReward#sell_reward{coin=0}};
                        {false,Reason}->
                            {false,Reason}
                    end;
                _->
                    {false,3}
            end
    end.
do_cs_combine_do_mage2(_SrcGer,_GerBag,_GerTypeID,lieu)->
    {false,6};
do_cs_combine_do_mage2(SrcGer,GerBag,GerTypeID,Type)->
    PosList = role_data:get_posList(),
    IsSame = case Type of
        ger->
            role_buddy:check_gerlist_same_type(PosList,GerTypeID);
        _->
            false
    end, 
    case IsSame andalso is_record(SrcGer,ger) of
        false->
            case data_combine_mage:get(GerTypeID) of
                {{NeedOneGerType,NeedOneGerQuality},NeedList}->
                    #sell_reward{coin=Coin,gold=Gold,item=NeedItemList,reputation=Reputation,newGer=NewGer}=NeedList,
                    %检查钻石金币贡献值
                    RoleInfo = role_data:get_roleInfo(),
                    IsCurrencyEnough = lists:all(fun({MoneyType,NeedValue})->
                            role_lib:check_money(RoleInfo,MoneyType,NeedValue) end,[{coin,Coin},{gold,Gold},{reputation,Reputation}]),
                    case IsCurrencyEnough of
                        false ->
                            ?INFO("check_combine_do_mage 货币不足"),
                            {false,3};
                        true ->
                            %检查道具材料是否齐全
                            {OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog} =
                            if
                                NeedItemList =:= []->
                                    {?undefined, true, [],[],[]};
                                true ->
                                    RoleBag = role_data:get_bagItem(),
                                    lists:foldl(fun(NewItem,{BagItemAcc, ResultAcc, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc})->
                                        if
                                            ResultAcc =:= false ->
                                                {RoleBag, false, [],[],[]};
                                            true ->
                                                #new_item{itemTypeID=ItemTypeID,itemNum=ItemNum} = NewItem,
                                                case item_lib:check_material2(BagItemAcc, ItemTypeID, ItemNum) of
                                                    {BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateItemLogList} ->
                                                        {BagItemAcc2, true, DelItemList++DelItemAcc, UpdateItemList++UpdataItemAcc, UpdateItemLogList++UpdateItemLogAcc};
                                                    _ ->
                                                        {RoleBag, false, [],[],[]}
                                                end
                                        end
                                    end, {RoleBag, true, [], [], []}, NeedItemList)
                            end,
                            case Result of
                                false ->
                                    ?INFO("check_combine_do_mage 道具不足"),
                                    {false,3};
                                true ->
                                    %检查精灵材料是否齐全
                                    case is_record(SrcGer,gerSimple) of
                                        true->
                                            #gerSimple{gerQuality=OldGerQuality,gerTypeID=OldGerTypeID,gerLevel=OldGerLevel}=SrcGer,
                                            DeleteList = [SrcGer];
                                        false->
                                            #ger{gerBase=OldGerBase} = SrcGer,
                                            #gerBase{gerQuality=OldGerQuality,gerLevel=OldGerLevel,gerTypeID=OldGerTypeID} = OldGerBase,
                                            DeleteList = [SrcGer]
                                    end,
                                    case OldGerQuality=:=NeedOneGerQuality andalso OldGerTypeID=:=NeedOneGerType of
                                        true->
                                            {Result2,NewGerList,DelGerList} = lists:foldl(fun(NeedGer,{AccR,AccNew,AccDel})->
                                            if
                                                AccR =:= true ->
                                                    case lists:keytake(NeedGer#new_ger.gerTypeID, #gerSimple.gerTypeID, AccNew) of
                                                        {value,DelGer2,AccNew2}->
                                                            {true,AccNew2,[DelGer2|AccDel]};
                                                        false ->
                                                            {false,[],[]}
                                                    end;
                                                true ->
                                                    {false,[],[]}
                                            end
                                            end, {true,GerBag,DeleteList}, NewGer),
                                            case Result2 of
                                                false->
                                                    {false,3};
                                                true->
                                                    AddGer = #new_ger{gerLevel=OldGerLevel,gerQuality=0,gerTypeID=GerTypeID},
                                                    {true,OtherBagItem, DelItem, UpdataItem, UpdateItemLog,DelGerList,NewGerList,NeedList,AddGer}
                                            end;
                                        false ->
                                            ?INFO("check_combine_do_mage 主材料不足 ~w",[GerBag]),
                                            {false,3}
                                    end
                            end
                        end;
                ?undefined ->
                    {false,2} %没找到配方
            end;
        true->
            {false,5}
    end.



% cs_combine_do_mage(#cs_combine_do_mage{gerTypeID=GerTypeID,gerUidID=GerUidID})->
%     case check_combine_do_mage(GerTypeID,GerUidID) of
%         {true,OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList,RoleInfo}->
%             ?INFO("cs_combine_do_mage ~n~w",[{OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList}]),
%             do_mage(GerTypeID,OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList,RoleInfo),
%             ?sendself(#sc_combine_do_mage{result=1});
%         {fail,Reason} ->
%             ?sendself(#sc_combine_do_mage{result=Reason})
%     end.

check_combine_do_mage(GerTypeID,GerUidID)->
    RoleInfo = role_data:get_roleInfo(),
    {{NeedOneGerType,NeedOneGerQuality},NeedList} = data_combine_mage:get(GerTypeID),
    case NeedList of
        #sell_reward{coin=Coin,gold=Gold,item=NeedItemList,reputation=Reputation,newGer=NewGer}->
            %检查钻石金币贡献值
            CheckRes1 = 
                lists:foldl(fun({Type,NeedValue},AccRes)->
                                if
                                    AccRes =:= false->
                                        false;
                                    true ->
                                        role_lib:check_money(RoleInfo,Type,NeedValue)
                                end
                            end, true, [{coin,Coin},{gold,Gold},{reputation,Reputation}]),
            case CheckRes1 of
                false ->
                    ?INFO("check_combine_do_mage 货币不足"),
                    false;
                true ->
                    %检查道具材料是否齐全
                    {OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog} = if
                        NeedItemList =:= []->
                            {?undefined, true, [],[],[]};
                        true ->
                            RoleBag = role_data:get_bagItem(),
                            lists:foldl(fun(NewItem,{BagItemAcc, ResultAcc, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc})->
                                                if
                                                    ResultAcc =:= false ->
                                                        {RoleBag, false, [],[],[]};
                                                    true ->
                                                        #new_item{itemTypeID=ItemTypeID,itemNum=ItemNum} = NewItem,
                                                        case item_lib:check_material2(BagItemAcc, ItemTypeID, ItemNum) of
                                                            {BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateItemLogList} ->
                                                                {BagItemAcc2, true, DelItemList++DelItemAcc, UpdateItemList++UpdataItemAcc, UpdateItemLogList++UpdateItemLogAcc};
                                                            _ ->
                                                                {RoleBag, false, [],[],[]}
                                                        end
                                                end
                                        end, {RoleBag, true, [], [], []}, NeedItemList)
                    end,
                    case Result of
                        false ->
                            ?INFO("check_combine_do_mage 道具不足"),
                            {fail,3};
                        true ->
                            %检查道具材料是否齐全
                            GerList = role_data:get_gerBag(),
                            case lists:keytake(GerUidID, #gerSimple.gerID, GerList) of
                                {value,GerSimple,OtherGerList}
                                  when GerSimple#gerSimple.gerTypeID =:= NeedOneGerType 
                                  andalso GerSimple#gerSimple.gerQuality =:= NeedOneGerQuality ->
                                    {Result2,NewGerList,DelGerList} = lists:foldl(fun(NeedGer,{AccR,AccNew,AccDel})->
                                                    if
                                                        AccR =:= true ->
                                                            case lists:keytake(NeedGer#new_ger.gerTypeID, #gerSimple.gerTypeID, AccNew) of
                                                                {value,DelGer2,AccNew2}->
                                                                    {true,AccNew2,[DelGer2|AccDel]};
                                                                false ->
                                                                    {false,[],[]}
                                                            end;
                                                        true ->
                                                            {false,[],[]}
                                                    end
                                                end, {true,OtherGerList,[GerSimple]}, NewGer),
                                    if
                                        Result2 =:= false ->
                                            ?INFO("check_combine_do_mage 辅助材料不足"),
                                            {fail,3};
                                        true ->
                                            {true,OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewGerList,DelGerList,NeedList,RoleInfo}
                                    end;
                                _ ->
                                    ?INFO("check_combine_do_mage 主材料不足 ~w",[GerList]),
                                    {fail,3}
                            end
                    end
            end;
        ?undefined ->
            {fail,2} %没找到配方
    end.

% cs_combine_do_mage2(#cs_combine_do_mage2{gerTypeID=GerTypeID,gerUidID=GerUidID})->
%     case check_combine_do_mage2(GerTypeID,GerUidID) of
%         {true,OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList,RoleInfo,Pos,EquipList}->
%             ?INFO("cs_combine_do_mage ~n~w",[{OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList}]),
%             NewGerGerID = do_mage2(GerTypeID,OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList,RoleInfo,EquipList,Pos),
%             ?sendself(#sc_combine_do_mage2{result=1,pos=Pos,newGerID = NewGerGerID});
%         {fail,Reason} ->
%             ?sendself(#sc_combine_do_mage2{result=Reason,pos=0,newGerID=0}),
% 			if Reason == 4 -> ?sendself(#sc_ger_standup{gerPos=0,gerID=0, result=4});true -> ignore end
%     end.

check_combine_do_mage2(GerTypeID,GerUidID)->
	LieuPosList = role_data:get_lieuposList(),
	GerList = role_data:get_posList(),
	case role_ger:is_mirror_ger(GerUidID)    of
		true ->
			{false, 2};
		false ->
			case [XGerTypeID||#ger{gerBase=#gerBase{gerTypeID=XGerTypeID}}<-GerList++LieuPosList,XGerTypeID == GerTypeID] of
				[] ->
					RoleInfo = role_data:get_roleInfo(),
					{{NeedOneGerType,NeedOneGerQuality},NeedList} = data_combine_mage:get(GerTypeID),
					case NeedList of
						#sell_reward{coin=Coin,gold=Gold,item=NeedItemList,reputation=Reputation,newGer=_NewGer}->
							%检查钻石金币贡献值
							CheckRes1 = 
								lists:foldl(fun({Type,NeedValue},AccRes)->
													if
														AccRes =:= false->
															false;
														true ->
															role_lib:check_money(RoleInfo,Type,NeedValue)
													end
											end, true, [{coin,Coin},{gold,Gold},{reputation,Reputation}]),
							case CheckRes1 of
								false ->
									?INFO("check_combine_do_mage 货币不足"),
									false;
								true ->
									%检查道具材料是否齐全
									{OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog} =
										if
											NeedItemList =:= []->
												{?undefined, true, [],[],[]};
											true ->
												RoleBag = role_data:get_bagItem(),
												lists:foldl(fun(NewItem,{BagItemAcc, ResultAcc, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc})->
																	if
																		ResultAcc =:= false ->
																			{RoleBag, false, [],[],[]};
																		true ->
																			#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum} = NewItem,
																			case item_lib:check_material2(BagItemAcc, ItemTypeID, ItemNum) of
																				{BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateItemLogList} ->
																					{BagItemAcc2, true, DelItemList++DelItemAcc, UpdateItemList++UpdataItemAcc, UpdateItemLogList++UpdateItemLogAcc};
																				_ ->
																					{RoleBag, false, [],[],[]}
																			end
																	end
															end, {RoleBag, true, [], [], []}, NeedItemList)
										end,
									case Result of
										false ->
											?INFO("check_combine_do_mage 道具不足"),
											{fail,3};
										true ->
											%检查道具材料是否齐全
											
											case lists:keytake(GerUidID, #ger.gerID, GerList) of
												{value,GerSimple=#ger{gerBase=GerBase},NewGerList}
												  when GerBase#gerBase.gerTypeID =:= NeedOneGerType 
																		   andalso GerBase#gerBase.gerQuality =:= NeedOneGerQuality ->
													Pos = GerBase#gerBase.gerPos,
													EquipList = role_data:get_equip(GerUidID),
													{true,OtherBagItem, Result, DelItem, UpdataItem, UpdateItemLog,NewGerList,[GerSimple],NeedList,RoleInfo,Pos,EquipList};
												_ ->
													?INFO("check_combine_do_mage 主材料不足 ~w",[GerList]),
													{fail,3}
											end
									end
							end;
						?undefined ->
							{fail,2} %没找到配方
					end;
				_ ->
					{fail,4}
			end
	end.

do_mage2(GerTypeID,OtherBagItem, _Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList,RoleInfo,EquipList,GerPos)->
    RoleID = RoleInfo#role.roleID,
    #sell_reward{coin=Coin,gold=Gold,reputation=Reputation} = NeedList,
    RoleInfo2 = if
        Coin > 0 ->
            role_lib:deduct_coin_f(RoleInfo, Coin, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, "");
        true ->
            RoleInfo
    end,
    RoleInfo3 = if
        Gold > 0 ->
            role_lib:deduct_gold_f(RoleInfo2, Gold, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, "");
        true ->
            RoleInfo2
    end,
    if
        Reputation > 0 ->
            role_lib:deduct_reputation_f(RoleInfo3, Reputation, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, "");
        true ->
            RoleInfo3
    end,
    {Date, _} = Time = erlang:localtime(),
    if 
        UpdateItemLog =:= [] andalso DelItem =:= [] ->
           ignore;
        true ->
           %% 写道具日志
           LogItemList = role_item:itemList2logItemList(DelItem, UpdateItemLog),
           behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MAGE, 1, ""),
           role_data:set_bagItem(OtherBagItem),
           DelItemIDList = [E||#item{itemUID=E}<-DelItem],
           UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdataItem],
           %% 提醒客户端更新物品
           ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
           ?sendself(#sc_item_update{updateList=UpdateList})
    end,
        

    LogGerList = role_ger:gerList2logGerList(LastDeleteList),
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
    behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, ""),
    ?sendself(#sc_ger_del{gerIDList=lists:map(fun(#ger{gerID=GerUID}) -> GerUID end, LastDeleteList)}),
    AddGerList = [#new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}],
	role_payGuide:do_mega(GerTypeID),
    [#gerSimple{gerID=NewGetGerID}=UpGer3] = role_reward:handle_ger_f(AddGerList,  ?MONEY_ADD_TYPE_COMBINE_MAGE, 0, ""),
	role_data:set_equip(NewGetGerID,GerTypeID,EquipList),
	NewGer =  ger_attr:recacl(UpGer3#gerSimple{gerPos=GerPos}, NewLastGerList),
	NowPosList = [NewGer|NewLastGerList],
		GerBag = role_data:get_gerBag(),
	GerBag2 = [G||G=#gerSimple{gerID=XGerUid}<-GerBag,XGerUid /= NewGetGerID],
	role_data:set_gerBag(GerBag2),
	    role_data:set_posList(NowPosList),
	NewGetGerID.

do_mage(GerTypeID,OtherBagItem, _Result, DelItem, UpdataItem, UpdateItemLog,NewLastGerList,LastDeleteList,NeedList,RoleInfo)->
    RoleID = RoleInfo#role.roleID,
    #sell_reward{coin=Coin,gold=Gold,reputation=Reputation} = NeedList,
    RoleInfo2 = if
        Coin > 0 ->
            role_lib:deduct_coin_f(RoleInfo, Coin, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, "");
        true ->
            RoleInfo
    end,
    RoleInfo3 = if
        Gold > 0 ->
            role_lib:deduct_gold_f(RoleInfo2, Gold, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, "");
        true ->
            RoleInfo2
    end,
    if
        Reputation > 0 ->
            role_lib:deduct_reputation_f(RoleInfo3, Reputation, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, "");
        true ->
            RoleInfo3
    end,
    {Date, _} = Time = erlang:localtime(),
    if 
        UpdateItemLog =:= [] andalso DelItem =:= [] ->
           ignore;
        true ->
           %% 写道具日志
           LogItemList = role_item:itemList2logItemList(DelItem, UpdateItemLog),
           behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MAGE, 1, ""),
           role_data:set_bagItem(OtherBagItem),
           DelItemIDList = [E||#item{itemUID=E}<-DelItem],
           UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdataItem],
           %% 提醒客户端更新物品
           ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
           ?sendself(#sc_item_update{updateList=UpdateList})
    end,
        
    LogGerList = role_ger:gerList2logGerList(LastDeleteList),
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
    behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MAGE, GerTypeID, ""),
    ?sendself(#sc_ger_del{gerIDList=lists:map(fun(#gerSimple{gerID=GerUID}) -> GerUID end, LastDeleteList)}),
    role_data:set_gerBag(NewLastGerList),
    AddGerList = [#new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}],
	role_payGuide:do_mega(GerTypeID),
    [_R] = role_reward:handle_ger_f(AddGerList,  ?MONEY_ADD_TYPE_COMBINE_MAGE, 0, "").

get_star_list(Type) ->
   case data_combine_crt:get(Type) of 
        [] ->
            [];
        undefined ->
            [];
        List ->
            {_,_,ID} = get_cur_open_time_interval(),
            case lists:keyfind(ID,1,List) of
                false ->
                    [];
                {ID,List2} ->
                   lists:foldr(fun({Star,_},Acc) -> [Star|Acc] end, [], List2)
            end 
    end.

%%获得当其时间所在的时间段,如果当其
%%时间不在开放时间内,返回0
get_cur_open_time_interval( ) ->
    Now = calendar:local_time( ),
    case data_combine_crt:get(datetime) of
        [] ->
            {0,0,0};
        undefined ->
            {0,0,0};
        List ->
            case util:fun_find(fun([Begin,End,_ID]) ->
                        Begin =< Now andalso Now =< End end, List) of 
                false ->
                    {0,0,0};
                [Begin,End,ID] ->
                    {Begin,End,ID}
            end
    end.

%%获得某类型对应星级的暴击倍数
get_combine_crt(Type,StarLevel) ->
    case data_combine_crt:get(Type) of 
        [] ->
            1;
        undefined ->
            1;
        List ->
            {_,_,ID} = get_cur_open_time_interval( ),
            case lists:keyfind(ID,1,List) of
                false ->
                    1;
                {ID,List2} ->
                    case lists:keyfind(StarLevel,1,List2) of 
                        false ->
                            1;
                        {StarLevel,WeightList} ->
                            CrtRate = data_home:get({constr_type_combine,role_home:get_build_type_level(role_data:get_roleID(),?constr_type_combine)}),
                            ?INFO("get_combine_crt    ~w  ,,,  ~w",[CrtRate,WeightList]),
                            WeightList2 = 
                                lists:foldr(fun({R,C},AccList)->
                                                case lists:keysearch(C, 1, CrtRate) of
                                                    {value,{C,AddRate}} ->
                                                        [{R+AddRate,C}|AccList];
                                                    false ->
                                                        [{R,C}|AccList]
                                                end
                                            end, [], WeightList),
                            util:random_one_from_weigh_list(WeightList2)
                    end
            end
    end.

check_combine_multi(Type, UIDList) ->
    case Type =:= ?CombineGer of
        true ->
            check_combine_multi_ger(UIDList);
        _ ->
            check_combine_multi_equip(UIDList) 
    end.
           
check_combine_multi_ger(UIDList) ->
    %% 批量合成长度判断
    Len = erlang:length(UIDList),
    Group = erlang:trunc(Len / ?RandomCombineNeedNum),
    DelNum = Group * ?RandomCombineNeedNum,
    case DelNum >= ?RandomCombineNeedNum of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
    end,

    GerBagList = role_data:get_gerBag(),
    %% 精灵合法性判断
    {DelGerList, NewGerBagList} =
        lists:foldr(fun(UID, {AccDelGerList, AccGerBagList}) ->
                            case lists:keytake(UID, #gerSimple.gerID, AccGerBagList) of
                                {value, #gerSimple{gerTypeID=DelGerTypeID}=DelGer, NewAccGerBagList} ->
                                    case util:is_exp_card(DelGerTypeID) of
                                        false ->
                                            #data_ger{gerStar=Star} = data_ger:get(DelGerTypeID),
                                            case lists:member(Star,data_combine_random:get(random_combine_star_list)) of
                                                true->
                                                    {[DelGer|AccDelGerList], NewAccGerBagList};
                                                false->
                                                    erlang:throw({false,?ComBineFailStarNotAllow})
                                            end;
                                        true ->
                                            erlang:throw({false, ?CombineFailExpGerCanNotCombine})
                                    end;
                                false ->
                                    case role_ger:is_mirror_ger(UID) of
                                        false ->
                                            erlang:throw({false, ?CombineFailNotEnough});
                                        true ->
                                            erlang:throw({false, ?CombineFailMirror})
                                    end     
                            end
                    end, {[], GerBagList}, lists:sublist(UIDList,DelNum)),

    %% 星级合法性判断
    GerStarLevel = check_ger_star_level(DelGerList),
    case GerStarLevel =< data_combine_random:get(max_ger_star_level) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailStarLevelLimit})
    end,
    {Low, High} = data_combine_random:get(ger_multi_range),
    case GerStarLevel >= Low andalso GerStarLevel =< High of
        true ->
            next;
        _ ->
            erlang:throw({false, ?ComBineFailStarWa})
    end,

    %% 金币判断
    {BoxID, NeedCoin} = data_combine_random:get({ger, GerStarLevel}),
    NeedCoin2 = NeedCoin * Group,
    case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin2) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailCoinNotEnough})
    end,
    RoleID = role_data:get_roleID(),
    case homestead_server:has_homestead_ger(RoleID, DelGerList) of
        true->
	        erlang:throw({false, ?CombineFailHomesteadGerNot});
        false->
	        {multi_combine_ger, BoxID, DelGerList, NewGerBagList, NeedCoin2, Group}
    end.

check_combine_multi_equip(UIDList) ->
    %% 批量合成长度判断
    Len = erlang:length(UIDList),
    Group = erlang:trunc(Len / ?RandomCombineNeedNum),
    DelNum = Group * ?RandomCombineNeedNum,
    case DelNum >= ?RandomCombineNeedNum of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailRandomCombineNeedNumError})
    end,

    %% 材料合法性判断
    EquipBagList = role_data:get_bagEquip(),
    {DelEquipList, NewEquipBagList} =
        lists:foldr(fun(UID, {AccDelEquipList, AccEquipBagList}) ->
                            case lists:keytake(UID, #item.itemUID, AccEquipBagList) of
                                {value, DelEquip, NewAccEquipBagList} ->
                                    {[DelEquip|AccDelEquipList], NewAccEquipBagList};
                                false ->
                                    erlang:throw({false, ?CombineFailNotEnough})
                            end
                    end, {[], EquipBagList}, lists:sublist(UIDList, DelNum)),

    %% 星级合法性判断
    EquipStarLevel = check_equip_star_level(DelEquipList),
    case EquipStarLevel =< data_combine_random:get(max_equip_star_level) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailStarLevelLimit})
    end,
    {Low, High} = data_combine_random:get(equip_multi_range),
    case EquipStarLevel >= Low andalso EquipStarLevel =< High of
        true ->
            next;
        _ ->
            erlang:throw({false, ?ComBineFailStarWa})
    end,

    %% 金币判断
    {BoxID, NeedCoin} = data_combine_random:get({equip, EquipStarLevel}),
    NeedCoin2 = NeedCoin * Group,
    case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin2) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailCoinNotEnough})
    end,
    {multi_combine_equip, BoxID, DelEquipList, NewEquipBagList, NeedCoin2, Group}.

do_multi_combine_ger(BoxID, DelGerList, NewGerBagList, NeedCoin, Group) ->
    {Date, _} = Time = erlang:localtime(),
    {LogGerList, GroupList} = gen_combine_group(DelGerList, ?RandomCombineNeedNum),
    [RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    {AddGerList,CrtNumList} = 
        lists:foldl(fun({NewQuality, NewLevel}, {AccAddGerList,[AccCrt2,AccCrt3]}) ->
                        {?REWARD_GER,GerTypeID,_} = util:random_one_from_weigh_list(RConfig),
                        #data_ger{gerStar=NewGerStarLevel} = data_ger:get(GerTypeID),
                        CrtNum = get_combine_crt(ger,NewGerStarLevel-1),
                        NewCrtNumList = case CrtNum of
                                            1 ->
                                                [AccCrt2,AccCrt3];
                                            2 ->
                                                [AccCrt2+1,AccCrt3];
                                            3 ->
                                                [AccCrt2,AccCrt3+1]
                                        end,
                        {lists:foldl(fun(Times,AccList)->
                                %%此处将暴击获得超过一个的那部分的等级设置成1级
                                NewLevel1 = case Times > 1 of true->1;false->NewLevel end,
                                {?REWARD_GER,GerID,_} = util:random_one_from_weigh_list(RConfig),
                                [#new_ger{gerLevel=NewLevel1,gerQuality=NewQuality,gerTypeID=GerID}|AccList]
                            end, [], lists:seq(1, CrtNum))  ++ AccAddGerList
                        ,NewCrtNumList}
                    end, {[],[0,0]}, GroupList),
	RoleInfo = role_data:get_roleInfo(),
	
	%%都没问题后，才扣除消耗，添加武将
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
	behavior_ger_consume:log(role_data:get_roleID(), LogGerList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MULTI_GER, 0, ""),
    ?sendself(#sc_ger_del{gerIDList=lists:map(fun(#gerSimple{gerID=GerUID}) -> GerUID end, DelGerList)}),
	role_data:set_gerBag(NewGerBagList),
	role_reward:handle_ger_f(AddGerList,  ?MONEY_ADD_TYPE_COMBINE_MULTI_GER, 0, ""),
	role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_MULTI_GER, 0, ""),
    notify_random_reward(AddGerList, ger,CrtNumList),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,random_combine_ger,Group})),
	role_homestead:hook_ger_delete(-1,0,0,[GID||#gerSimple{gerID=GID}<-DelGerList]).

do_multi_combine_equip(BoxID, DelEquipList, NewEquipBagList, NeedCoin, Group) -> 
    {Date, _} = Time = erlang:localtime(),
    {LogItemList, GroupList} = gen_combine_group(DelEquipList, ?RandomCombineNeedNum),
    %?ERR("LogItemList:~p~nGroupList:~p~n",[LogItemList,GroupList]),
    [RConfig|_] =  data_box:get({BoxID, role_data:get_mainGerTypeID()}),
    {AddItemList,CrtNumList} =
        lists:foldl(fun({NewQuality,NewLevel}, {AccAddEquipList,[AccCrt2,AccCrt3]}) ->
                        {?REWARD_ITEM,ItemTypeID,Num} = util:random_one_from_weigh_list(RConfig),
                        #data_item{itemStar=NewGerStarLevel} = data_item:get(ItemTypeID),
                        CrtNum = get_combine_crt(equip,NewGerStarLevel-1),
                        NewCrtNumList = case CrtNum of
                                            1 ->
                                                [AccCrt2,AccCrt3];
                                            2 ->
                                                [AccCrt2+1,AccCrt3];
                                            3 ->
                                                [AccCrt2,AccCrt3+1]
                                        end,
                        {lists:foldl(fun(Times,AccList)->
                                            %%此处将暴击获得超过一个的那部分的等级设置成1级
                                            NewLevel1 = case Times > 1 of true->1;false->NewLevel end,
                                            {?REWARD_ITEM,ItemID,Num} = util:random_one_from_weigh_list(RConfig),
                                            [#new_item{itemLevel=NewLevel1,itemNum=Num,itemRank=NewQuality,itemTypeID=ItemID}|AccList]
                                    end, [], lists:seq(1, CrtNum))++ AccAddEquipList
                        ,NewCrtNumList}
                    end, {[],[0,0]}, GroupList),
	RoleInfo = role_data:get_roleInfo(),
    %%都没问题后,才扣除资源，添加物品
    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMBINE_MULTI_EQUIP, 0, ""),
    ?sendself(#sc_item_delete_notify{itemUIDList=lists:map(fun(#item{itemUID=EquipUID}) -> EquipUID end, DelEquipList)}),
    role_data:set_bagEquip(NewEquipBagList),
	role_reward:handle_item_f(RoleInfo, AddItemList, ?MONEY_ADD_TYPE_COMBINE_MULTI_EQUIP, 0, ""),
	role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_COMBINE_MULTI_EQUIP, 0, ""),
    notify_random_reward(AddItemList, equip,CrtNumList),
	?CATCH(role_task_trigger:handle({dispach_task,role_combime,random_combine_equip,Group})).

gen_combine_group(List, Num) ->
    gen_combine_group(List, 0, Num, {0, 0}, {[], []}).

gen_combine_group([], Cur, _, {QualityAcc, MaxLevel}, {LogAcc, QLAcc}) ->
    NewQuality = erlang:trunc(QualityAcc / Cur),
    {LogAcc, [{NewQuality, MaxLevel}|QLAcc]};
gen_combine_group(T, Num, Num, {QualityAcc, MaxLevel}, {LogAcc, QLAcc}) ->
    NewQuality = erlang:trunc(QualityAcc / Num),
    gen_combine_group(T, 0, Num, {0, 0}, {LogAcc, [{NewQuality, MaxLevel}|QLAcc]});
gen_combine_group([H|T], Cur, Num, {QualityAcc, MaxLevel}, {LogAcc, QLAcc}) ->
    {LogE, Quality, Level} = get_element_info(H),
    NewQualityAcc = Quality + QualityAcc,
    NewMaxLevel = erlang:max(MaxLevel, Level),
    gen_combine_group(T, Cur + 1, Num, {NewQualityAcc, NewMaxLevel}, {[LogE|LogAcc], QLAcc}).

get_element_info(#gerSimple{gerID=GerID, gerTypeID=GerTypeID, gerQuality=GerQuality, gerLevel=GerLevel}) ->
    {[GerID,GerTypeID,GerLevel,GerQuality], GerQuality, GerLevel};

get_element_info(#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum,itemRank=IR,itemLevel=ItemLevel}) ->
    {[EquipUID, EquipTypeID, EquipNum, EquipNum], IR, ItemLevel}.

check_combine_stone(UIDList)->
    EquipBagList = role_data:get_bagEquip(),
    %检查材料是不是存在
    {DelEquipList, NewEquipBagList,StoneType,TotalExp,TotalQ} =
        lists:foldr(fun(UID, {AccDelEquipList, AccEquipBagList,AccType,AccExp,AccQ}) ->
                            case lists:keytake(UID, #item.itemUID , AccEquipBagList) of
                                {value, #item{itemTypeID=DelEquipTypeID,itemExp=AddExp,itemRank=AddRank}=DelEquip, NewAccEquipBagList} ->
                                    {_,_,DelTypeListS5,_NewTypeListS5} = data_item_stone:get({stone_3to1,5}), % 先试一下星5
                                    case lists:member(DelEquipTypeID, DelTypeListS5) of
                                        true when AccType =:= 5 orelse AccType =:= 0 ->
                                            {[DelEquip|AccDelEquipList], NewAccEquipBagList, 5,AccExp+AddExp,AccQ+AddRank};
                                        false ->
                                            {_,_,DelTypeListS6,_NewTypeListS6} = data_item_stone:get({stone_3to1,6}), % 先试一下星6
                                            case lists:member(DelEquipTypeID, DelTypeListS6) of
                                                true when AccType =:= 6 orelse AccType =:= 0 ->
                                                    {[DelEquip|AccDelEquipList], NewAccEquipBagList, 6,AccExp+AddExp,AccQ+AddRank};
                                                false ->
                                                    {_,_,DelTypeListS15,_NewTypeListS15} = data_item_stone:get({stone_3to1,15}), % 先试一下星15
                                                    case lists:member(DelEquipTypeID, DelTypeListS15) of
                                                        true when AccType =:= 15 orelse AccType =:= 0 ->
                                                            {[DelEquip|AccDelEquipList], NewAccEquipBagList, 15,AccExp+AddExp,AccQ+AddRank};
                                                        false ->
                                                            {_,_,DelTypeListS16,_NewTypeListS16} = data_item_stone:get({stone_3to1,16}), % 再试一下星16，早知道策划加这么多，写成循环就好了，现在懒得改了
                                                            case lists:member(DelEquipTypeID, DelTypeListS16) of
                                                                true when AccType =:= 16 orelse AccType =:= 0 ->
                                                                    {[DelEquip|AccDelEquipList], NewAccEquipBagList, 16,AccExp+AddExp,AccQ+AddRank};
                                                                false ->
                                                                    erlang:throw({false, ?CombineFailNotEnough})
                                                            end
                                                    end
                                            end
                                    end;
                                false ->
                                    ?INFO("check_combine_stone fail -1- ~w",[EquipBagList]),
                                    erlang:throw({false, ?CombineFailNotEnough})
                            end
                    end, {[], EquipBagList,0,0,0}, UIDList),
    {NeedNum,NeedCoin,_,NewTypeList} = data_item_stone:get({stone_3to1,StoneType}), % StoneType = 5 or 6
    DelLength = erlang:length(UIDList),
    if
        DelLength /= NeedNum ->
            erlang:throw({false, ?CombineFailNotEnough});
        true ->
            ignore
    end,
    case role_lib:check_money(role_data:get_roleInfo(), coin, NeedCoin) of
        true ->
            next;
        false ->
            erlang:throw({false, ?CombineFailCoinNotEnough})
    end,
    NewRank = TotalQ div NeedNum,
    Star = (StoneType rem 10 )+1,
    Crt = get_combine_crt(combine_stone_3to1,StoneType),
    AddNewTypeList = [util:random_one_from_weigh_list(NewTypeList)||_<-lists:seq(1, Crt)],
    [FirstNewType|_] = AddNewTypeList,
    % 同类符文只能合出同类符文，精华合精华，普通和普通，所以经验表一样，
    #data_item{itemType=StoneTypeName} = data_item:get(FirstNewType),
    ExpLevelConfig = case item_lib:is_itemType_stone(StoneTypeName) of
        true->
            data_stone_exp:get(Star);
        false->
            case item_lib:is_itemType_essence_stone(StoneTypeName) of
                true->
                    data_essence_stone_exp:get(Star);
                false->
                    []
            end
    end,
    {NewLevelExp,NewLevel} = lists:foldl(fun({Exp,Level},Acc)->
                                     if 
                                         Exp =< TotalExp andalso Level =:= 10 ->  %% 如果等级上限变化，这里需要更新一下
                                             {Exp,Level};
                                         Exp =< TotalExp ->  
                                             {Exp,Level};
                                         true ->
                                             Acc
                                     end
                             end, {0,0}, ExpLevelConfig),
    EquipExpAddition = if
                           NewLevel /= 10 ->
                                TotalExp - NewLevelExp;
                           true ->
                               0
                       end,
    NewStoneList = [#new_item{itemTypeID=NewType,itemNum=1,itemLevel=NewLevel,itemRank=NewRank}||NewType<-AddNewTypeList],
    {combine_stone_3to1, DelEquipList, NewEquipBagList, NeedCoin, NewStoneList,EquipExpAddition}.





