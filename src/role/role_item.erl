%% @author caohongyang
%% @doc 背包、装备、道具功能
%% Created 2013-3-15


-module(role_item).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_item.hrl").
-include("def_reward.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

-define(EnergyBlockTypeIDList,[2,3,4,5,6]).
-define(ExperienceStoneList,[5029,5030,5031,5032,5033]).  %定义经验符文ItemTypeID列表
-define(NormalStoneEat,1).
-define(EssenceStoneEat,2).
-define(STONE_EAT,1).
-define(STONE_UPRANK,2).
-define(GEREQUIP_REINFORCE,1).
-define(TRAINEREQUIP_REINFORCE,2).

-record(decompose_box_info,{boxlist=[],gold=0,coin=0,reputation=0,item=[],ger=[]}).
%% ====================================================================
%% API functions
%% ====================================================================
cs_item_use_info(_) ->
    ItemUseList = role_data:get_itemUseList(),
    PItemUseInfoList =
        lists:map(fun(#item_use_info{itemTypeID=ItemTypeID,useTimes=UseTimes}) ->
                          #data_item_use{maxTimes=MaxTimes} = data_item_use:get(ItemTypeID),
                          #p_item_use_info{type_id=ItemTypeID, left_times=erlang:max(MaxTimes - UseTimes, 0)}
                  end, ItemUseList),
    ?sendself(#sc_item_use_info{use_info_list=PItemUseInfoList}).

cs_item_bag(_) ->
	BagEquip = role_data:get_bagEquip(),
	BagItem = role_data:get_bagItem(),
%% 	RoleID = role_data:get_roleID(),
%% 	Patch = plunder_server:get_treasure_patch(RoleID),
%% 	PatchItem = lists:foldl(fun(E,Acc)->
%% 									if Patch =:= [] ->
%% 										   Acc;
%% 									   true ->
%% 										   M = patch2p_item(E, Patch),
%% 										   if M =:= [] ->
%% 												  Acc;
%% 											  true ->
%% 												  [M|Acc]
%% 										   end
%% 									end
%% 							end, [], all_patch_typeID()),
 	AllItem = [item_lib:item2p_item(E)||E<-(BagEquip ++ BagItem)],
%% 	?sendself(#sc_item_bag{allItem=AllItem}).
	BagLength = erlang:length(AllItem),
	SeqLength = data_common:get(send_client_pak_cnt),
	SeqCnt = 
		case BagLength of
			0 ->
				1;
			_ ->
				2 + BagLength div SeqLength
		end,
	?sendself(#sc_item_bag2{seq=SeqCnt,subSeq=1,itemList=[]}),
	lists:foldl(fun(Cnt,Acc)->
						case Cnt of
							SeqCnt ->
								?sendself(#sc_item_bag2{seq=SeqCnt,subSeq=Cnt,itemList=Acc});
							_ ->
								{Head,Tail} = lists:split(SeqLength, Acc),
								?sendself(#sc_item_bag2{seq=SeqCnt,subSeq=Cnt,itemList=Head}),
								Tail
						end
				end,AllItem,lists:seq(2,SeqCnt)).

cs_item_equip(_) ->
	EquipedList = [item_lib:item2p_equip(E, GerID) || {GerID,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	?sendself(#sc_item_equip{allEquip=EquipedList}).
	


%% @doc 卖道具
%% cs_item_sell(#cs_item_sell{itemUIDList=ItemIDList}) ->
%% 	case check_sell(ItemIDList) of
%% 		{true, DelItemList, BagEquip2, BagOther2} ->
%% 			do_sell3(DelItemList, BagEquip2, BagOther2);
%% 		{false, Reason} ->
%% 			?sendself(#sc_item_sell{result=Reason,reward=[]})
%% 	end.
cs_item_sell(#cs_item_sell{itemUIDList=ItemUIDList})->
	case check_sell_item(ItemUIDList) of
		{false,Reason}->
			?sendself(#sc_item_sell{result=Reason,gold=0,reward=[]});
		{true,NewBagItem,NewBagEquip,SellItemList,SellEquipList,SellP}->
			#role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
			role_lib:add_coin_f(RoleInfo, SellP, ?MONEY_ADD_TYPE_SELL_ITEM, 0, ""),
			role_data:set_bagEquip(NewBagEquip),
			role_data:set_bagItem(NewBagItem),
			DelItemIDList = [E||#item{itemUID=E}<-SellItemList]++[E||#item{itemUID=E}<-SellEquipList],
			?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
			LogItemList1 = role_item:itemList2logItemList(SellItemList, []),
			LogItemList = role_item:itemList2logItemList(SellEquipList, LogItemList1),
			{Date, _} = Time = erlang:localtime(),
			behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_SELL_ITEM, 0, ""),
			?sendself(#sc_item_sell{result=1,gold=SellP,reward=[#p_reward_view3{type=1, typeID=21100, num=SellP}]})
	end.


%% @doc 卸下道具
cs_item_down_equip(#cs_item_down_equip{gerID=GerID,itemPos=ItemPos}) ->
%%     IsEquip = check_pos_equip(ItemPos),
    IsStone = check_pos_stone(GerID,ItemPos) orelse check_pos_essence_stone(GerID,ItemPos),
    if
        IsStone ->
            case check_down_stone(GerID, ItemPos) of
                {true, Item, EquipList2} ->
                    do_down_equip(GerID, ItemPos, Item, EquipList2);
                {false, Reason} ->
                    ?sendself(#sc_item_down_equip{result=Reason, gerID=GerID,itemPos=ItemPos})
            end;
%% 禁止装备卸载
%%      IsEquip->
%%          case check_down_equip(GerID, ItemPos) of
%%              {true, Item, EquipList2}->
%%                  do_down_equip(GerID, ItemPos, Item, EquipList2);
%%              {false, Reason} ->
%%                  ?sendself(#sc_item_down_equip{result=Reason})
%%          end;
        true ->
            %% //2=> 失败-装备穿的位置非法
            ?sendself(#sc_item_down_equip{result=2, gerID=GerID,itemPos=ItemPos}),
            ?INFO("cs_item_down_equip. itemPos(~w) is wrong!!",[ItemPos])
    end.    
    
    
    

%% @doc 穿上道具
cs_item_up_equip(#cs_item_up_equip{gerID=GerID,itemPos=ItemPos, itemUID=ItemUID, itemGerID= ItemGerID}) when ItemGerID /= 1000->
    IsEquip = check_pos_equip(GerID,ItemPos),
    IsStone = check_pos_stone(GerID,ItemPos),
    IsTrainerEquip = check_pos_trainer_equip(GerID,ItemPos),
    IsTrainerStone = check_pos_trainer_stone(GerID,ItemPos),
    IsEssenceStone = check_pos_essence_stone(GerID,ItemPos),
    IsAccelerateEquip = check_pos_accelerate_equip(GerID,ItemPos),
    if
        IsEquip -> %宠物装备
            case check_up_equip(GerID, ItemPos, ItemUID, ItemGerID) of
        		{true, UpItem, BagEquipList2, EquipList} ->
        			do_up_equip(GerID, ItemPos, ItemUID, UpItem, BagEquipList2, EquipList, ItemGerID),
        			%%触发精灵普通装备变化统计
        			role_payGuide:trigger_formation_ger_equip_change(GerID);
        		{false, Reason} ->
        			?sendself(#sc_item_up_equip{result=Reason, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID})
        	end;
        IsStone orelse IsTrainerStone -> %宠物符文,或训练师能量石
            case check_up_stone(GerID, ItemPos, ItemUID, ItemGerID) of
                {true, UpItem, BagEquipList2, EquipList} ->
                    do_up_equip(GerID, ItemPos, ItemUID, UpItem, BagEquipList2, EquipList, ItemGerID),
                    %%触发上阵精灵符文数量统计
                    role_payGuide:trigger_task_change(?FORMATION_GER_STONE_TN,{GerID}),
                    ?CATCH(role_task_trigger:handle({dispach_task, equip_stone_num, GerID}));
                {false, Reason} ->
                    ?sendself(#sc_item_up_equip{result=Reason, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID})
            end;
        IsTrainerEquip ->
            #role{level = Lvl} =role_data:get_roleInfo(),
            UnlockLevel = data_trainer:get(trainer_unlock_level),
            if
                Lvl >= UnlockLevel ->
                    case check_up_equip(GerID, ItemPos, ItemUID, ItemGerID) of
                        {true, UpItem, BagEquipList2, EquipList} ->
                            do_up_equip(GerID, ItemPos, ItemUID, UpItem, BagEquipList2, EquipList, ItemGerID);
                        {false, Reason} ->
                            ?sendself(#sc_item_up_equip{result=Reason, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID})
                    end;
                true ->
                    ?INFO("Level is too low(~w-~w)， can not equip trainer equip",[Lvl,UnlockLevel]),
                    ?sendself(#sc_item_up_equip{result=8, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID})
            end;
        IsEssenceStone->
        	case check_up_essence_stone(GerID,ItemPos,ItemUID,ItemGerID) of
        		{true,UpItem,BagEquipList2,EquipList}->
 					do_up_equip(GerID,ItemPos,ItemUID,UpItem,BagEquipList2,EquipList,ItemGerID),
 					%%触发上阵精灵符文数量统计
 					role_payGuide:trigger_task_change(?FORMATION_GER_STONE_TN,{GerID}),
 					?CATCH(role_task_trigger:handle({dispach_task, equip_stone_num, GerID}));
 				{false,Reason}->
 					?sendself(#sc_item_up_equip{result=Reason, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID})
 			end;
 		IsAccelerateEquip ->
 			case check_up_accelerate_equip(GerID,ItemPos,ItemUID,ItemGerID) of
 				{true,UpItem,BagEquipList2,EquipList}->
 				 	do_up_equip(GerID,ItemPos,ItemUID,UpItem,BagEquipList2,EquipList,ItemGerID);
 				{false,Reason}->
 					?sendself(#sc_item_up_equip{result=Reason, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID})	
 			end;
        true ->
            %% //2=> 失败-装备穿的位置非法
            ?sendself(#sc_item_up_equip{result=2, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID}),
            ?INFO("cs_item_up_equip. itemPos(~w) is wrong!!",[ItemPos])
    end;
cs_item_up_equip(#cs_item_up_equip{gerID=GerID,itemPos=ItemPos, itemUID=ItemUID, itemGerID= ItemGerID}) ->
    ?INFO("不能移动训练师的装备",[ItemGerID]),
    ?sendself(#sc_item_up_equip{result=2, gerID=GerID,itemPos=ItemPos,itemUID=ItemUID}).

%% @doc 功能道具
cs_item_use(#cs_item_use{itemNum=ItemNum,itemUID=ItemUID}) ->
	case check_use_item(ItemNum, ItemUID) of
		{true, #item{itemTypeID=ItemTypeID,itemNum=CurNum}=Item, BagOther}->
			#data_item{itemEffect=EffectFun, itemEffectArg=EffectArg} = data_item:get(ItemTypeID),
			case catch item_effect:EffectFun(EffectArg,ItemNum) of
				{ok, UseNum} when UseNum < CurNum->
					BagOther2 = [Item#item{itemNum=CurNum-UseNum}|BagOther],
					role_data:set_bagItem(BagOther2),
					?sendself(#sc_item_use{itemNum=UseNum,itemUID=ItemUID,result=1});
				{ok, UseNum} ->
					BagOther2 = BagOther,
					role_data:set_bagItem(BagOther2),
					?sendself(#sc_item_use{itemNum=UseNum,itemUID=ItemUID,result=1});
                {false, Reason} ->
                    ?sendself(#sc_item_use{itemNum=ItemNum,itemUID=ItemUID,result=Reason});
				Error ->
					throw({item_effect_error, Error})
			end;
		{false, Reason} ->
			?sendself(#sc_item_use{itemNum=ItemNum,itemUID=ItemUID,result=Reason})
	end.

%% @doc 装备强化
cs_item_reinforce(#cs_item_reinforce{gerID=GerID,itemUID=ItemUID}) ->
	case check_reinforce(ItemUID,GerID) of
		{true, OwnerType, NeedValue, Item, EquipList2, Role, DataItem} ->
			do_reinforce(ItemUID, GerID, NeedValue, Item, EquipList2, Role, DataItem, OwnerType);
		{false,Reason} ->
			?sendself(#sc_item_reinforce{itemUID=ItemUID,result=Reason,newLevel=0})
	end.

%% @doc 装备最大强化
cs_item_max_reinforce(#cs_item_max_reinforce{gerID=GerID, itemUID=ItemUID}) ->
	%% 此处先判断是否能进行一次强化
	case check_reinforce(ItemUID, GerID) of
		{true, OwnerType, NeedValue, Item, EquipList2, Role, DataItem} ->
			TempLevelList = do_max_reinforce(ItemUID, GerID, NeedValue, Item, EquipList2, Role, DataItem, OwnerType),
			?sendself(#sc_item_max_reinforce{itemUID=ItemUID,result=1,tempLevelList=TempLevelList});
		{false, Reason} ->
			?sendself(#sc_item_max_reinforce{itemUID=ItemUID,result=Reason,tempLevelList=[]})
	end.

% @doc 装备一键强化
cs_item_max_reinforce_for_ger(#cs_item_max_reinforce_for_ger{gerID=GerID,type=Type})->
	case role_data:get_equip(GerID) of
		[]->
			?sendself(#sc_item_max_reinforce_for_ger{gerID=GerID,resultlist=[]});
		EquipList->
			%%ReinForceEquipL中的装备是强化之后但是没有进行刷新属性的装备，只是为了用于日志记录
			Role = role_data:get_roleInfo(),
			{ResultList,NewEquipList,NewRole,CurrencyUseL,ReinForceEquipL,TarEquipL} = do_item_max_reinforce_for_ger(Role,EquipList,Type),
			% ?ERR("Role:~w ~n NewRole:~w ~n currentRole:~w ~n ResultList:~w ~n",[Role,NewRole,ResultList,role_data:get_roleInfo()]),
			add_max_reinforce_for_ger_log(ReinForceEquipL,TarEquipL,ResultList),
			%%此处需要设置对应精灵的道具
			if 
       	 		GerID =:= 1000 -> %训练师装备强化
           			role_data:set_equip(GerID, NewEquipList),
           			?INFO("trainer equip reinforce, re cacl attr"),
           			ger_attr:recacl_gers(); 
	   			true ->
           			OldEquipList = role_data:get_equip(GerID),
		   			role_data:set_equip(GerID,NewEquipList),
           			role_lvlSgAttr:on_equip_lvl_up(GerID, OldEquipList),
		   			ger_attr:recacl_f(GerID),
		   			%%触发上阵精灵装备等级变化
		   			role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_LEVEL_TN,{GerID}),
		   			role_maintask:do_main_task(?FORMATION_GER_EQUIP_LEVEL_N_TN_TN,{GerID})
			end,
			PReinforceResult = [#p_reinforce_result{equipID=ItemUID,equipTypeID=ItemTypeID,result=Reason,tempLevelList=lists:reverse(LevelList)}||{ItemUID,Reason,ItemTypeID,LevelList,_CurrencyUse}<-ResultList],
			?sendself(#sc_item_max_reinforce_for_ger{gerID=GerID,resultlist=PReinforceResult})	
	end.

%%将需要强化的装备区分出
do_item_max_reinforce_for_ger(_Role,[],_Type)->
	{[],[]};
do_item_max_reinforce_for_ger(Role,EquipList,Type)->
	%%FinishEquipL1是除去能够强化装备外的其他所有装备（可能包括符文等，在设置精灵装备时，需要）,FinishEquipL3是对应强化类型下，已经达到最大等级的那些装备（在构造返回的时候使用）
	{TarEquipL,FinishEquipL1,FinishEquipL3} = lists:foldl(fun(Item=#item{itemType=ItemType,itemLevel=ItemLevel},{TarAcc,Finish1Acc,Finish3Acc})->
		case Type of
			?GEREQUIP_REINFORCE->
				case item_lib:is_itemType_equip(ItemType) of
					true->
						case is_equip_level_extend_limit(Item,Role,Type) of
							true->
								{TarAcc,[Item|Finish1Acc],[Item|Finish3Acc]};
							false->
								{[Item|TarAcc],Finish1Acc,Finish3Acc}
						end;
					false->
						{TarAcc,[Item|Finish1Acc],Finish3Acc}
				end;
			?TRAINEREQUIP_REINFORCE->
				case item_lib:is_itemType_trainer_equip(ItemType)  of
					true->
						case is_equip_level_extend_limit(Item,Role,Type) of
							true->
								{TarAcc,[Item|Finish1Acc],[Item|Finish3Acc]};
							false->
								{[Item|TarAcc],Finish1Acc,Finish3Acc}
						end;
					false->
						{TarAcc,[Item|Finish1Acc],Finish3Acc}
				end
		end
	end,{[],[],[]},EquipList),
	%%在强化的过程中只修改了装备的等级，未根据装备等级计算新的属性，故此处获得的装备还需要刷新对应的属性
	{ResultL1,FinishEquipL2,NewRole,CurrencyUseL} = do_item_max_reinforce_for_ger2(Role,TarEquipL,[],Type,[],[]),
	FinishEquipL = lists:foldl(fun(Item,Acc)->	[refresh_item(Item)|Acc] end,FinishEquipL1,FinishEquipL2),
	% ?ERR("TarEquip:~w ~nFinishEquipL1:~w ~n FinishEquipL3:~w ~n FinishEquipL2:~w ~n FinishEquipL:~w ~n",[TarEquipL,FinishEquipL1,FinishEquipL3,FinishEquipL2,FinishEquipL]),
	%%加上等级限制的装备结果
	MaxLevel = data_reinforce_trainer:get(base_reputation_max),
	F = fun(#item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemLevel=ItemLevel},Acc)->
		case ItemLevel >= MaxLevel of 
			true->
				[{ItemUID,10,ItemTypeID,[],{0,0}}|Acc];
			false->
				[{ItemUID,3,ItemTypeID,[],{0,0}}|Acc]
		end
	end,
	ResultL = lists:foldl(F,ResultL1,FinishEquipL3),
	{ResultL,FinishEquipL,NewRole,CurrencyUseL,FinishEquipL2,TarEquipL}.

do_item_max_reinforce_for_ger2(Role,[],FinishEquipL,Type,ResultL,CurrencyAcc)->
	{ResultL,FinishEquipL,Role,CurrencyAcc};
do_item_max_reinforce_for_ger2(Role,TarEquipL,FinishEquipL,Type,ResultL,CurrencyAcc)->
	{TarEquip,RemainL} = choose_reinforce_equip(TarEquipL),
	case do_equip_reinforce_without_log(TarEquip,Role,Type) of
		false->
			%%货币不够完成对应装备的强化
			Reason = case Type of ?GEREQUIP_REINFORCE->2;?TRAINEREQUIP_REINFORCE->9 end,
			%%金币不足的情况下，有可能是第一次强化就不足，需要给前端以结果，故此处需要加一次额外的空结果
			NewResultL = add_reinforce_result(TarEquip,[],ResultL,[]),
			do_item_max_reinforce_for_ger2(Role,RemainL,[TarEquip|FinishEquipL],Type,add_reinforce_reason(NewResultL,Reason,TarEquip),CurrencyAcc);
		{true,NewEquip,NewRole,AddResult,CurrencyUse}->
			case is_equip_level_extend_limit(NewEquip,NewRole,Type) of
				true->
					%%强化之后，超过等级限制了，将会不能参加下一轮强化，放入完成列表
					NewResultL = add_reinforce_result(NewEquip,AddResult,ResultL,CurrencyUse),
					do_item_max_reinforce_for_ger2(NewRole,RemainL,[NewEquip|FinishEquipL],Type,add_reinforce_reason(NewResultL,3,NewEquip),add_currency_use(CurrencyUse,CurrencyAcc));
				false->
					%%未超过等级限制，放入待强化列表，等待下次强化
					do_item_max_reinforce_for_ger2(NewRole,[NewEquip|RemainL],FinishEquipL,Type,add_reinforce_result(NewEquip,AddResult,ResultL,CurrencyUse),add_currency_use(CurrencyUse,CurrencyAcc))
			end
	end.

do_equip_reinforce_without_log(TarEquip,Role,Type)->
	#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=ItemLevel} = TarEquip,
	#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
    {NeedValue,CurrencyType} = case Type of
    	?GEREQUIP_REINFORCE->
            {cacl_reinforce_coin_ger(ItemType,ItemLevel,ItemStar),coin};
        ?TRAINEREQUIP_REINFORCE ->
            {cacl_reinforce_reputation_trainer(ItemType,ItemLevel,ItemStar),reputation}
    end,
    case role_lib:check_money(Role,CurrencyType,NeedValue) of
    	true->
    		#role{coin=Coin,reputation=Reputation} = Role, 
    		AddLevel = random_add_level(Role#role.vipLevel),
    		{NewRole,CurrencyUse} = case CurrencyType of
    			coin->
    				{Role#role{coin=Coin-NeedValue},{coin,NeedValue}};
    			reputation->
    				{Role#role{reputation = Reputation-NeedValue},{reputation,NeedValue}}
    		end,
    		
    		%%由于存在暴击的情况，可能加上AddLevel之后，装备的等级会大于最大等级，故此处需要判断下
    		LastLevel = erlang:min(ItemLevel+AddLevel,data_reinforce_trainer:get(base_reputation_max)), 
    		NewEquip = TarEquip#item{itemLevel=LastLevel},
    		{true,NewEquip,NewRole,LastLevel-ItemLevel,CurrencyUse};
    	false->
    		false
    end.
%%将装备锻造的结果加入到结果列表中
add_reinforce_reason(ResultL,Reason,Equip)->
	#item{itemUID=ItemUID} = Equip,
	case lists:keytake(ItemUID,1,ResultL) of
		false->
			?ERR("not find ItemUID:~w in resultlist:~w~n",[ItemUID,ResultL]),
			ResultL;
		{_Value,F,Other}->
			[setelement(2,F,Reason)|Other]
	end.

%%将升级过程加入到结果列表
add_reinforce_result(Item,[],ResultL,[])->
	#item{itemTypeID=ItemTypeID,itemUID=ItemUID} = Item,
	case lists:keytake(ItemUID,1,ResultL) of
		false->
			[{ItemUID,1,ItemTypeID,[],{0,0}}|ResultL];
		{_Value,{ItemUID,Reason,ItemTypeID,ItemResult,{CurrencyType,OldValue}},Other}->
			ResultL
	end;
add_reinforce_result(Item,AddResult,ResultL,{CurrencyType,Value})->
	#item{itemTypeID=ItemTypeID,itemUID=ItemUID} = Item,
	case lists:keytake(ItemUID,1,ResultL) of
		false->
			[{ItemUID,1,ItemTypeID,[AddResult],{CurrencyType,Value}}|ResultL];
		{_Value,{ItemUID,Reason,ItemTypeID,ItemResult,{CurrencyType,OldValue}},Other}->
			[{ItemUID,Reason,ItemTypeID,[AddResult|ItemResult],{CurrencyType,OldValue+Value}}|Other]
	end.
%%增加消耗统计
add_currency_use({Type,Value}=E,CurrencyAcc)->
	case lists:keytake(Type,1,CurrencyAcc) of
		false->
			[E|CurrencyAcc];
		{_Value,{Type,OldValue},Other}->
			[{Type,OldValue+Value}|Other]
	end.
%%根据装备强化情况，记录消耗日志
add_max_reinforce_for_ger_log([],OldEquipList,Result)->
	ignore;
add_max_reinforce_for_ger_log(ReinForceEquipL,OldEquipList,ResultList)->
	% ?ERR("ReinForceEquipL:~w ~n",[ReinForceEquipL]),
	{Date, _} = Time = erlang:localtime(),
	lists:foreach(fun(#item{itemUID=ItemUID,itemLevel=NewLevel,itemTypeID=ItemTypeID}=E)->
		case lists:keyfind(ItemUID,#item.itemUID,OldEquipList) of
			false->
				?ERR("after reinforce NewEquip:~w is not exist in OldList:~w ~n",[E,OldEquipList]),
				ignore;
			#item{itemLevel=OldLevel}->
				case lists:keyfind(ItemUID,1,ResultList) of
					false->
						?ERR("after reinforce NewEquip:~w result is not exist in resultlist:~w ~n",[E,ResultList]),
						ignore;
					%%这个情况是装备从最开始就不能够进行强化
					{ItemUID,_Reason,_ItemTypeID,AddLevelResult,{0,0}}->
						ignore;
					{ItemUID,_Reason,_ItemTypeID,AddLevelResult,{CurrencyType,Value}}->
						%%此处取消了对装备类型的判定，需要确认是否装备强化任务触发正常
						ReinforceTimes = length(AddLevelResult),
						Role = role_data:get_roleInfo(),
						?CATCH(role_task_trigger:handle({dispach_task,equip_strong,Role#role.roleID, ItemUID, ItemTypeID,NewLevel,ReinforceTimes})),
						%%添加货币消耗
						role_lib:deduct_money_f(Role, CurrencyType, Value,?MONEY_DEC_TYPE_ITEM_MAX_REINFORCE,ItemTypeID,integer_to_list(OldLevel)++"|"++integer_to_list(NewLevel)),
						%%添加装备升级日志
						behavior_item_uplevel:log(Role#role.roleID, ItemUID, ItemTypeID, NewLevel-OldLevel, NewLevel,ReinforceTimes,Value, Date, Time)
				end
		end
	end,ReinForceEquipL).

%%选择强化的装备
choose_reinforce_equip(EquipList)->
	choose_reinforce_equip2(EquipList,not_equip,[]).
choose_reinforce_equip2([],Min,Remain)->
	{Min,Remain};
choose_reinforce_equip2([H|T],Min,Acc)->
	case compare_reinforce_equip(H,Min) of
		true->
			choose_reinforce_equip2(T,Min,[H|Acc]);
		false->
			case Min of
				not_equip->
					choose_reinforce_equip2(T,H,Acc);
				_->
					choose_reinforce_equip2(T,H,[Min|Acc])
			end
	end.
compare_reinforce_equip(H,not_equip)->
	false;
compare_reinforce_equip(#item{itemLevel=ItemLevel1},#item{itemLevel=ItemLevel2})->
	ItemLevel1 > ItemLevel2.

%%判断装备是否超过了玩家等级以及训练师等级
is_equip_level_extend_limit(Item,Role,_Type)->
	MaxLevel = data_reinforce_trainer:get(base_reputation_max),
	Item#item.itemLevel >= MaxLevel orelse Item#item.itemLevel >= Role#role.level.

%% @doc 装备升品
cs_item_up_rank(#cs_item_up_rank{srcItemGerID=SrcItemGerID,foodItemGerID=FoodItemGerID,foodItemUID=FoodItemUID,srcItemUID=SrcItemUID})->
	case check_up_rank(SrcItemUID, SrcItemGerID, FoodItemUID, FoodItemGerID) of
		{true, SrcItem, SrcList, FoodItem, FoodList2, DataItem, NeedCoin, NeedRep, NewRank, Role} ->
			%% 扣银两
			#role{roleName=RoleName,level=_RoleLevel,roleID=RoleID} = Role,
            role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_ITEM_UP_RANK, 0, ""),
            role_lib:deduct_reputation_f(Role, NeedRep, ?MONEY_DEC_TYPE_ITEM_UP_RANK, 0, ""),
			%% 计算新道具
			NowSec = timer_wheel:nowsec(),
			#item{itemLevel=ItemLevel, itemDecay=ItemDecay, itemTypeID=ItemTypeID, itemRank=CurItemRank,itemType=ItemType} = SrcItem,
			% #data_item{itemStar=Star} = data_item:get(ItemTypeID),
			%% 合并强化等级
			NewLevel = erlang:max(ItemLevel, FoodItem#item.itemLevel),
			#data_item{isDecay=IsDecay}=DataItem,
			ItemDecay2 = item_lib:item_decay(item_lib:next_decay_sec(IsDecay, NewRank, NowSec), SrcItemUID),
			SrcItem2 = refresh_item(SrcItem#item{itemRank=NewRank,itemDecay=ItemDecay2, itemLevel=NewLevel}),
			item_lib:cancel_decay(ItemDecay),
			item_lib:cancel_decay(FoodItem#item.itemDecay),
            OldEquipList = 
                case SrcItemGerID =/= 1000 andalso SrcItemGerID =/= 0 of
                    true ->
                        role_data:get_equip(SrcItemGerID);
                    _ ->
                        []
                end,

			if FoodItemGerID =:= SrcItemGerID ->
				   set_itemList(SrcItemGerID, [SrcItem2|FoodList2]);
			   true ->
				   set_itemList(SrcItemGerID, [SrcItem2|SrcList]),
				   set_itemList(FoodItemGerID,FoodList2)
			end,
			if 
                SrcItemGerID =:= 1000 ->
                    ?INFO("trainer equip up rank, re cacl attr"),
                    ger_attr:recacl_gers(); %训练师装备升阶
                SrcItemGerID =/= 0 ->
                   	role_lvlSgAttr:on_equip_lvl_up(SrcItemGerID, OldEquipList),
                   	role_lvlSgAttr:on_equip_rank_up(SrcItemGerID, OldEquipList),
                   	case item_lib:is_accelerate_equip(ItemType) of
                   		false->
				   			?CATCH(role_task_trigger:handle({dispach_task,equip_up_quality,RoleID,SrcItemUID,ItemTypeID,NewRank}));
				   		true->
				   			ignore
				   	end,
				   	ger_attr:recacl_f(SrcItemGerID);
			   	FoodItemGerID =/=0 andalso FoodItemGerID =/= SrcItemGerID ->
			   		case item_lib:is_accelerate_equip(ItemType) of
			   			false->
							?CATCH(role_task_trigger:handle({dispach_task,equip_up_quality,RoleID,SrcItemUID,ItemTypeID,NewRank}));
						true->
							ignore
					end,
					ger_attr:recacl_f(FoodItemGerID);
			   true ->
				   	case item_lib:is_accelerate_equip(ItemType) of
			   			false->
							?CATCH(role_task_trigger:handle({dispach_task,equip_up_quality,RoleID,SrcItemUID,ItemTypeID,NewRank}));
						true->
							ignore
					end
			end,			
			%%确定是否需要触发上阵精灵装备变化
			case (SrcItemGerID=/=1000 andalso SrcItemGerID=/=0) orelse (FoodItemGerID=/=1000 andalso  SrcItemGerID=/=0) of 
				true->
					role_payGuide:trigger_formation_ger_change(SrcItemGerID),
					%%触发精灵普通装备品阶统计
        			role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_RANK_TN,{SrcItemGerID});
				false->
					ignore
			end,
			LogGerList = [[FoodItem#item.itemUID,FoodItem#item.itemTypeID,FoodItem#item.itemLevel,FoodItem#item.itemRank]],
			{Date, _} = Time = erlang:localtime(),
			%% 写道具消耗日志，和装备升品日志
			behavior_item_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_ITEM_UP_RANK, 0, integer_to_list(SrcItemUID)),
			behavior_item_uprank:log(RoleID, SrcItemUID, ItemTypeID, ItemLevel, NewLevel, CurItemRank, NewRank, FoodItemUID, Date, Time),
			?sendself(#sc_item_up_rank{foodItemUID=FoodItemUID,result=1,srcItemUID=SrcItemUID,newItemLevel=NewLevel,newItemRank=NewRank}),
			?sendself(#sc_item_update_rank{itemUID=SrcItemUID,newItemDecay=item_lib:itemDecay(ItemDecay2),newItemRank=NewRank}),
            case data_item:get(SrcItem2#item.itemTypeID) of
                #data_item{itemStar=ItemStarLevel} when ItemStarLevel >= 5 ->
                    broadcast_server:bc(#sc_message_item_uprank{itemInfo=item_lib:item2p_item_view(SrcItem2),roleName=RoleName});
                _ ->
                    ok
            end;
		{false, Reason} ->
			?sendself(#sc_item_up_rank{foodItemUID=FoodItemUID,result=Reason,srcItemUID=SrcItemUID,newItemLevel=0,newItemRank=0})
	end.

cs_item_compound(#cs_item_compound{typeID=ItemTypeID}) ->
	case check_compound(ItemTypeID) of
		{true, BagOther2, Product, _NeedNum, DelAcc, _UpdateAcc, UpdateLogList} ->
			role_data:set_bagItem(BagOther2),
			RoleID = role_data:get_roleID(),
			%% 写道具日志
			LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
			{Date, _} = Time = erlang:localtime(),
			behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ITEM_COMPOUND, ItemTypeID, ""),
			
			if is_record(Product, new_item) ->
				   item_lib:add_item_f(Product, ?MONEY_ADD_TYPE_ITEM_COMPOUND, 0, "");
			   true ->
				   ger_lib:add_ger(Product, ?MONEY_ADD_TYPE_ITEM_COMPOUND, 0, "")
			end,
			?sendself(#sc_item_compound{typeID=ItemTypeID, result=1});
		{false, Reason} ->
			?sendself(#sc_item_compound{typeID=ItemTypeID, result=Reason})
	end.

%宝物升品
cs_item_eat(#cs_item_eat{itemID=ItemID, itemGerID = ItemGerID, foodItemIDList = FoodItemIDList}) ->
	case check_treasure_eat(ItemID,ItemGerID, FoodItemIDList) of
		{true, SrcItem, SrcList, ItemList, ItemBag3}->
			%%计算新道具
			#item{itemExp = SrcItemExp,itemTypeID=ItemTypeID} = SrcItem,
		    DataItem = data_item:get(ItemTypeID),
			ExpWillBe = lists:foldl(fun(E, Acc) -> Acc + item_lib:cacl_treasure_exp(E) end, SrcItemExp, ItemList),
			if DataItem#data_item.itemStar =:= 5 ->
				   ExpFix = erlang:min(ExpWillBe, max_treasure_exp2()),
				   NewRank = data_treasure_exp2:get(ExpFix);
			   true ->
				   ExpFix = erlang:min(ExpWillBe,max_treasure_exp()),
				   NewRank = data_treasure_exp:get(ExpFix)			   
			end,
			SrcItemNew = refresh_item(SrcItem#item{itemExp = ExpFix, itemRank = NewRank}),
			%%回写
			if ItemGerID =:= 0 ->
				   set_itemList(ItemGerID, [SrcItemNew|ItemBag3]);
			   true->
				   set_itemList(ItemGerID, [SrcItemNew|SrcList]),
				   set_itemList(0, ItemBag3),
				   ger_attr:recacl_f(ItemGerID)
			end,
			%% 写道具日志
			LogItemList = role_item:itemList2logItemList(ItemList, []),
			{Date, _} = Time = erlang:localtime(),
			RoleID = role_data:get_roleID(),
			behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_TREASURE_EAT, ItemTypeID, integer_to_list(ItemID)),
			?sendself(#sc_item_eat{result = 1, itemID = ItemID, itemExp = ExpFix, newItemRank = NewRank});
		{false, Reason} ->
			?sendself(#sc_item_eat{result = Reason, itemID = ItemID, itemExp = 0, newItemRank = 0})
	end.


%%再次分解
cs_item_decompose_again(#cs_item_decompose_again{decomposeID=DecomposeID})->
	% case get(last_decompose_reward) of
	% ?INFO("last_decompose_reward: ~w ~n",[get(last_decompose_reward)]),
	case get(last_decompose_reward) of
		?undefined ->
		% _ ->
			?sendself(#sc_item_decompose_again{result=2,decomposeID=DecomposeID,rewardList=[]});
		LastRecord ->
			case is_record(LastRecord,decompose_reward)	of
				true ->
					case DecomposeID =:= LastRecord#decompose_reward.recordID of
						true ->
							case check_cost_and_last_reward(LastRecord) of
								true ->
									Role = role_data:get_roleInfo(),
									#decompose_reward{decomposegerlist=DecomposeGerList,decomposegerequiplist=DecomposeGerEquipList,decomposestonelist=DecomposeStoneList,decomposetrainerlist=DecomposeTrainerEquipeList} = LastRecord,
									BoxInfo = new_decompose_get_boxlist(DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList),
									Reward1 = get_reward(BoxInfo#decompose_box_info.boxlist),
									Reward4 = get_last_reward(Reward1,BoxInfo),
									?INFO("BoxInfo:~w Reward1:~w ~n",[BoxInfo,Reward4]),
									Reward = role_reward:handle_sys_reward_with_return(Role, Reward4, ?MONEY_ADD_TYPE_EQUIP_DECOMPOSE, 0, "", false),
									?INFO("Reward is ~w ~n ",[Reward]),
									DecompseID = util:random_int(100000000, 999999999),
									?sendself(#sc_item_decompose_again{result = 1,decomposeID=DecompseID,rewardList=role_reward:transform2p_reward_view(Reward4,[])}),
									DecomposeReward = transform2decompose_reward(Reward),
									DecomposeReward2 = DecomposeReward#decompose_reward{recordID=DecompseID,decomposegerlist=DecomposeGerList,decomposegerequiplist=DecomposeGerEquipList,decomposestonelist=DecomposeStoneList,decomposetrainerlist=DecomposeTrainerEquipeList},
									put(last_decompose_reward,DecomposeReward2);
								{false,Reason}->
									?sendself(#sc_item_decompose_again{result=Reason,decomposeID=DecomposeID,rewardList=[]})
							end;
						false ->
							?sendself(#sc_item_decompose_again{result=3,decomposeID=DecomposeID,rewardList=[]})
					end;
				false ->
					?sendself(#sc_item_decompose_again{result=6,decomposeID=DecomposeID,rewardList=[]})
			end
	end.


%%使用钻石进行装备分解
% cs_item_decompose_by_money(_)->
% 	Role = role_data:get_roleInfo(),
% 	#role{gold=Gold,goldBonus=GoldBonus} = Role,
% 	% ?INFO("Need: ~w  Gold:~w GoldBonus: ~w ",[data_equip_decompose:get(decompose_cost),Gold,GoldBonus]),
% 	case data_equip_decompose:get(decompose_cost) =< (Gold + GoldBonus) of
% 		true ->
% 		    % role_lib:deduct_gold_2_f(RoleInfo, DonateNum, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
% 			Role2 = role_lib:deduct_gold_2_f(Role, data_equip_decompose:get(decompose_cost), ?MONEY_DEC_TYPE_DECOMPOSE_EQUIPMENT_BY_MONEY, 0, ""),
% 			Reward = decompose_get_reward(data_equip_decompose:get(decompose_value)),
% 			role_reward:handle_sys_reward_with_return(Role2, Reward, ?MONEY_ADD_TYPE_EQUIP_DECOMPOSE, 0, ""),
% 			?sendself(#sc_item_decompose_by_money{result=1,rewardList=role_reward:transform2p_reward_view(Reward,[])});
% 		false ->
% 			?sendself(#sc_item_decompose_by_money{result=2,rewardList=[]})
% 	end.

%%装备分解
cs_item_decompose(#cs_item_decompose{itemUnitList=ItemUnitList})->
    [First|_] = ItemUnitList,
    if
        First#p_item_decompose_unit.decomposetype =:= ?MAGE_DECOMPOSE -> %下面代码，每次消息只会分解一个
            do_mega_decompose(First,erlang:length(ItemUnitList));
        true ->
	       do_item_decompose(ItemUnitList)
    end.

do_mega_decompose(MegaStone,Num)->
    BagItem = role_data:get_bagItem(),
    case lists:keytake(MegaStone#p_item_decompose_unit.itemUID,#item.itemUID,BagItem) of
        false ->
            ?sendself(#sc_item_decompose{result = 2,decomposeID=0,rewardList=[]});
        {value,CostItem,OtherBagItem} when Num =< CostItem#item.itemNum ->
            ?INFO("cs_item_decompose ~w ~w",[CostItem,OtherBagItem]),
            HomeComposeList 
                = lists:filter(fun({CostTypeId,_CostNum,_RewardNum,RewardItem})-> 
                					case is_record(RewardItem,new_item) of
                						true->
                                    		RewardItem#new_item.itemTypeID =:= CostItem#item.itemTypeID 
                                    		andalso CostTypeId < 30000; %是键石或mage石，但不是种子
                                    	false->
                                    		false
                                    end
                               end, data_homestead:get(data_seedcompose)),
            case erlang:length(HomeComposeList) of 
                1 ->
                    [{CostTypeId2,CostNum2,RewardNum2,_}] = HomeComposeList,
                    NewBagItem = if
                        CostItem#item.itemNum > Num ->
                            [CostItem#item{itemNum=CostItem#item.itemNum -Num}|OtherBagItem];
                        true ->
                            OtherBagItem
                    end,
                    DeleteItemList = [CostItem#item{itemNum= Num}],
                    LogItemList = role_item:itemList2logItemList(DeleteItemList, []),
                    if
                        CostItem#item.itemNum > Num ->
                            UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-NewBagItem,MegaStone#p_item_decompose_unit.itemUID =:= ItemUID],
                            ?sendself(#sc_item_update{updateList=UpdateList});
                        true ->
                            DeletItemUIDList = [ItemUID||#item{itemUID=ItemUID}<-DeleteItemList],
                            ?sendself(#sc_item_delete_notify{itemUIDList=DeletItemUIDList})
                    end,
                    role_data:set_bagItem(NewBagItem),
                    {Date, _} = Time = erlang:localtime(),
                    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_DECOMPOSE_EQUIPMENT, 0, ""),
                    Reward = [#new_item{itemTypeID=CostTypeId2,itemNum=(CostNum2 * Num) div RewardNum2,itemLevel =1,itemRank =0}],
                    role_reward:handle_sys_reward(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_EQUIP_DECOMPOSE, 0, ""),
                    DecompseID = util:random_int(100000000, 999999999),
                    ?sendself(#sc_item_decompose{result = 1,decomposeID=DecompseID,rewardList=role_reward:transform2p_reward_view(Reward,[])});
                _ ->
                    ?sendself(#sc_item_decompose{result = 2,decomposeID=0,rewardList=[]})
            end;
        _ ->
            ?sendself(#sc_item_decompose{result = 2,decomposeID=0,rewardList=[]})
    end.
  
do_item_decompose(ItemUnitList)->
	Role = role_data:get_roleInfo(),
	case check_decompose(ItemUnitList) of
		{false,_,_,_,_} ->
			?sendself(#sc_item_decompose{result = 2,decomposeID=0,rewardList=[]});
		{true,DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList}->
			deduct_role_cost_and_log(DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList),
			BoxInfo = new_decompose_get_boxlist(DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList),
			Reward1 = get_reward(BoxInfo#decompose_box_info.boxlist),
			Reward4 = get_last_reward(Reward1,BoxInfo),
			Reward = role_reward:handle_sys_reward_with_return(Role, Reward4, ?MONEY_ADD_TYPE_EQUIP_DECOMPOSE, 0, ""),
			DecompseID = util:random_int(100000000, 999999999),
			?sendself(#sc_item_decompose{result = 1,decomposeID=DecompseID,rewardList=role_reward:transform2p_reward_view(Reward4,[])}),
			DecomposeReward = transform2decompose_reward(Reward),
			DecomposeReward2 = DecomposeReward#decompose_reward{recordID=DecompseID,decomposegerlist=DecomposeGerList,decomposegerequiplist=DecomposeGerEquipList,decomposestonelist=DecomposeStoneList,decomposetrainerlist=DecomposeTrainerEquipeList},
			put(last_decompose_reward,DecomposeReward2)
	end.

%%扣除玩家分解的道具并且记录日志
deduct_role_cost_and_log(DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList)->
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	BagEquip = role_data:get_bagEquip(),
	BagGer = role_data:get_gerBag(),
	NewBagEquip = ((BagEquip -- DecomposeGerEquipList) -- DecomposeStoneList)--DecomposeTrainerEquipeList,
	NewBagGer = BagGer -- DecomposeGerList,
	role_data:set_bagEquip(NewBagEquip),
	role_data:set_gerBag(NewBagGer),
	DeleteItemList = DecomposeGerEquipList ++ DecomposeStoneList ++ DecomposeTrainerEquipeList,
	LogItemList = role_item:itemList2logItemList(DeleteItemList, []),
	case [ItemUID||#item{itemUID=ItemUID}<-DeleteItemList] of
		[]->
			ignore;
		DeletItemUIDList->
			?sendself(#sc_item_delete_notify{itemUIDList=DeletItemUIDList})
	end,
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_DECOMPOSE_EQUIPMENT, 0, ""),
	LogItemList2 = role_ger:gerList2logGerList(DecomposeGerList),
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogItemList2],
	case [GerID||#gerSimple{gerID = GerID}<-DecomposeGerList] of
		[]->
			ignore;
		DeleteGerIDList->
			?sendself(#sc_ger_del{gerIDList=DeleteGerIDList})
	end,
	% {Date, _} = Time = erlang:localtime(),
	behavior_ger_consume:log(RoleID, LogItemList2, Date, Time, ?MONEY_DEC_TYPE_DECOMPOSE_EQUIPMENT, 0, "").

%%获取钻石分解的消耗
cs_item_decompose_by_money_cost(#cs_item_decompose_by_money_cost{})->
	case data_equip_decompose:get(decompose_cost) of
		?undefined ->
			?sendself(#sc_item_decompose_by_money_cost{result=2,cost=0});
		Cost ->
			?sendself(#sc_item_decompose_by_money_cost{result=1,cost=Cost})
	end.


%%获取再次分解需要消耗的物品列表
cs_item_decompose_again_cost(#cs_item_decompose_again_cost{})->
	case data_equip_decompose:get(decompose_again_cost) of
		?undefined->
			?sendself(#sc_item_decompose_again_cost{result=2,costlist=[]});
		DecomposeAgainCost ->
			?sendself(#sc_item_decompose_again_cost{result=1,costlist=transform2p_reward_view(DecomposeAgainCost)})
	end.

cs_item_enchant(#cs_item_enchant{type=EnchantType,gerID=GerID,itemUID=ItemUID})->
	case catch do_item_enchant(EnchantType,GerID,ItemUID) of
		{error,Reason}->
			?sendself(#sc_item_enchant{result=Reason,equip=#p_equip2{itemUID=0,itemTypeID=0,itemLevel=0,itemRank=0,itemGerID=0,itemPos=0,itemDecay=0,itemExp=0,itemenchantType=0,itemenchantLevel=0}});
		{ok,NewEquip}->
			?sendself(#sc_item_enchant{result=1,equip=transform_item2p_equip2(NewEquip,GerID)})
	end.

cs_item_x_get(#cs_item_x_get{targetTypeID=TarTypeID,itemIDs=ItemIDs}) ->
	case check_x_get(TarTypeID,ItemIDs) of
		{true,Role,NewEquipList,DelList,CostCoin,CostTicket,MaxLevel} ->
			do_x_get(Role,NewEquipList,DelList,CostCoin,CostTicket,TarTypeID,MaxLevel);
        {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
        	role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_ITEM_X_GET),
        	Reward = [#new_item{itemTypeID=TarTypeID,itemNum=1,itemLevel =1,itemRank =0}],
			item_lib:add_item_f(Reward, ?MONEY_ADD_TYPE_ITEM_X_GET, TarTypeID, ""),
			?sendself(#sc_item_x_get{result=7});	
		{false,Reason} ->
			?sendself(#sc_item_x_get{result=Reason});
		false->
			?sendself(#sc_item_x_get{result=4})
	end.

cs_item_down_rank(#cs_item_down_rank{itemUID=SrcUID,srcGerUID=SrcGerUID}) ->
	case check_item_down_rank(SrcUID,SrcGerUID) of
		{true,Role,Item,OtherEquipList, NeedGold} ->
			do_item_down_rank(Role,Item,OtherEquipList, NeedGold,SrcGerUID);
		{false,Reason} ->
			?sendself(#sc_item_down_rank{result=Reason})
	end.

cs_item_pft_uprank(#cs_item_pft_uprank{srcItemUID=SrcItemUID,srcItemGerID=SrcItemGerID,foodItemUID=Foods}) ->
	case check_item_pft_uprank(SrcItemUID,SrcItemGerID,Foods) of
		{true,Role,SrcItem,List1,List2,DelList,UpdateAcc,UpdateLogList,CostCoin,BagOther2} ->
			do_item_pft_uprank(Role,SrcItemGerID,SrcItem,List1,List2,DelList,UpdateAcc,UpdateLogList,CostCoin,BagOther2,Foods);
		{false,Reason} ->
			?sendself(#sc_item_pft_uprank{result=Reason})
	end.

cs_item_stone_uprank(#cs_item_stone_uprank{srcStoneUID=SrcStoneUID,srcStoneGerID=SrcStoneGerID,foodStoneUID=FoodStoneUID})->
	case check_item_stone_uprank(SrcStoneUID,SrcStoneGerID,FoodStoneUID) of
		{true,SrcStoneGerID,SrcStone,NewBagEquip,NewGerEquipList,CostCoin,Food}->
			do_stone_uprank(SrcStoneGerID,SrcStone,NewBagEquip,NewGerEquipList,CostCoin,Food);
		{false,Reason}->
			?sendself(#sc_item_stone_uprank{result=Reason,newstone=#p_item{itemUID=0,itemTypeID=0,itemLevel=0,itemRank=0,itemNum=0,itemDecay=0,itemExp=0,itemenchantType=0,itemenchantLevel=0,itemLegendRank=0},srcStoneUID=SrcStoneUID,foodStoneUID=FoodStoneUID})
	end.

cs_item_make_legend(#cs_item_make_legend{srcItemUID=SrcItemUID,srcItemGerID=SrcItemGerID})->
    case check_cs_item_make_legend(SrcItemUID,SrcItemGerID) of
        {true,Item,EquipList2,CostList} ->
            ?INFO("before do_item_make_legend ~w ~w",[Item,CostList]),
            do_item_make_legend(SrcItemUID,SrcItemGerID,Item,EquipList2,CostList);
        {false,Result} ->
            ?sendself(#sc_item_make_legend{result=Result,newLegendItemUID=SrcItemUID,equipTypeID=0})
    end.

cs_item_stone_legend(#cs_item_stone_legend{srcItemUID=SrcItemUID,srcItemGerID=SrcItemGerID})->
    case check_item_stone_legend(SrcItemUID,SrcItemGerID) of
        {true,Item,EquipList2,CostList} ->
            do_item_stone_legend(SrcItemUID,SrcItemGerID,Item,EquipList2,CostList);
        {false,Reason} ->
            ?sendself(#sc_item_stone_legend{result=Reason,newItemID=0,srcItemID = SrcItemUID,equipPos=0,itemGerID=0})
    end.


cs_item_legend_uprank(#cs_item_legend_uprank{itemUID=ItemUID,itemGerID=GerID})->
	IsOpen = data_legendary:get(is_legend_uprank_open) =:= true,
	case {IsOpen,do_cs_item_legend_uprank(ItemUID,GerID)} of
		{false,_}->
			?sendself(#sc_item_legend_uprank{result=0,itemUID=ItemUID});
		{true,{false,Reason}}->
			?sendself(#sc_item_legend_uprank{result=Reason,itemUID=ItemUID});
		{true,{true,NewLegendRank}}->
			?sendself(#sc_item_legend_uprank{result=1,itemUID=ItemUID,itemLegendRank=NewLegendRank})
	end.

do_cs_item_legend_uprank(ItemUID,GerID)->
	case take_item(GerID,ItemUID) of
		false->
			{false,2};
		{_Value,Equip,OtherEquip}->
			case check_legend_uprank(Equip) of
				{false,R}->
					{false,R};
				{true,Cost,NextLegendRank}->
					GerBag = role_data:get_gerBag(),
					ItemBag = role_data:get_bagItem(),
					EquipBag = case GerID=:= 0 of true-> OtherEquip;_->role_data:get_bagEquip() end,
					case role_item:delete_sell_reward(Cost,[ItemUID],[],GerBag,ItemBag,EquipBag) of
						{true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
							NewEquip = Equip#item{itemLegendRank = NextLegendRank},
							case GerID of
								0->
									%%此处在删除sell_reward的时候，传入的背包装备已经除去了被升级的装备，故更新的时候要将该装备放回背包
            						role_item:update_role_info2(NewItemList,NewLastGerList,[NewEquip|NewEquipList],DeleteCurrencyList,?MONEY_DEC_TYPE_LEGEND_UPRANK,NextLegendRank,integer_to_list(ItemUID));
            					_->
            						%%此处被升阶的装备来源于精灵，故直接更新装备背包即可
            						role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_LEGEND_UPRANK,NextLegendRank,integer_to_list(ItemUID)),
									role_data:set_equip(GerID, [NewEquip|OtherEquip]),
									ger_attr:recacl_f(GerID)
							end,
							{true,NextLegendRank};
            			false->
            				{false,6}
            		end
            end
    end.

check_legend_uprank(#item{itemRank=Rank,itemLegendRank=ItemLegendRank,itemTypeID=ItemTypeID}=Equip)->
	case lists:member(ItemTypeID,?LEGENDLIST) of
		false->
			{false,3};
		true->
			case ItemLegendRank < data_legendary:get(max_legend_rank) of
				false->
					{false,4};
				true->
					NextLegendRank = ItemLegendRank + 1,
					IsMagic = lists:member(ItemTypeID,data_legendary:get(magic)),
					{RankNeed,Cost,_Add} = data_legendary:get({legend_rank,NextLegendRank,IsMagic}),
					case RankNeed > Rank of
						true->
							{false,5};
						false->
							{true,Cost,NextLegendRank}
					end
			end
	end.



do_item_pft_uprank(Role,SrcItemGerID,SrcItem,List1,List2,DelList,UpdateAcc,UpdateLogList,CostCoin,BagOther2,Foods)->
	#item{itemRank=Rank,itemUID=ItemUID,itemLevel=ItemLevel,itemTypeID=ItemTypeID} = SrcItem,
	role_data:set_bagItem(BagOther2),
	#role{roleID=RoleID,roleName=RoleName}=
			 role_lib:deduct_money_f(Role,coin,CostCoin,?MONEY_DEC_TYPE_ITEM_PFT_UPRANK, Rank, integer_to_list(ItemUID)),
	LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
	DeletItemUIDList = [DItemUID||#item{itemUID=DItemUID}<-DelList],
	?sendself(#sc_item_delete_notify{itemUIDList=DeletItemUIDList}),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ITEM_PFT_UPRANK, 0, ""),
	NewItem = SrcItem#item{itemRank=Rank+1},
	case SrcItemGerID of
		0 ->
			OldEquipList = 
				case SrcItemGerID =/= 1000 andalso SrcItemGerID =/= 0 of
					true ->
						role_data:get_equip(SrcItemGerID);
					_ ->
						[]
				end,
			role_lvlSgAttr:on_equip_rank_up(SrcItemGerID, OldEquipList),
			role_data:set_bagEquip([NewItem|List1]);
		_ ->
			role_data:set_bagEquip(List1),
			role_data:set_equip(SrcItemGerID, [NewItem|List2]),
			ger_attr:recacl_f(SrcItemGerID),
			role_maintask:do_main_task(?FORMATION_GER_EQUIP_RANK_N_TN_TN,{SrcItemGerID}),
			role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_RANK_TN,{SrcItemGerID})
	end,
	behavior_item_uprank:log(RoleID, ItemUID, ItemTypeID, ItemLevel, ItemLevel, Rank, Rank+1, 0, Date, Time),
	?CATCH(role_task_trigger:handle({dispach_task,pft_equip_up_quality,RoleID,ItemUID,ItemTypeID,Rank+1})),
	case [#p_item_num_update{itemUID=UItemUID,itemNum=UItemNum}||#item{itemUID=UItemUID,itemNum=UItemNum}<-UpdateAcc] of
		[]->
			ignore;
		Update_item_Unit_list->
			?sendself(#sc_item_update{updateList=Update_item_Unit_list})
	end,
    broadcast_server:bc(#sc_message_item_uprank{itemInfo=item_lib:item2p_item_view(NewItem),roleName=RoleName}),
	?sendself(#sc_item_update_rank{itemUID=ItemUID,newItemDecay=0,newItemRank=Rank+1}),
	?sendself(#sc_item_pft_uprank{result=1,newRank=Rank+1,srcUID=ItemUID,foodUIDs=Foods}).	
	
	

check_item_pft_uprank(SrcItemUID,SrcItemGerID,Foods)->
	case take_item(SrcItemGerID,SrcItemUID) of
		false ->
			{false,2};
		{value, SrcItem,SrcList} ->
			#item{itemRank=ItemRank,itemTypeID=SrcItemTypeID} = SrcItem,
			if  ItemRank < 10 ->
					{false, 7};
				true ->
					case data_item_pft_uprank:get({SrcItemTypeID,ItemRank+1}) of
						?undefined ->
							case data_item_pft_uprank:get({SrcItemTypeID,11}) of
								?undefined ->
									{false,8};
								_ ->
									{false,5}
							end;
						{CostCoin,{CostStoneID,CostStoneNum},CostItemNum,_} ->
							Role = role_data:get_roleInfo(),
							case role_lib:check_money(Role,coin,CostCoin) of
								false ->
									{false,6};
								true ->
									case item_lib:check_material(CostStoneID, CostStoneNum) of
										false ->
											{false,6};
										{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
											case check_pft_foods(Foods,SrcItemGerID,SrcList,SrcItemTypeID,CostItemNum) of
												{false,R} ->
													{false,R};
												{true,List1,List2,DelList} ->
													{true,Role,SrcItem,List1,List2,DelAcc++DelList,UpdateAcc,UpdateLogList,CostCoin,BagOther2}
											end
									end
							end
					end
			end
	end.
			
check_pft_foods(Foods,0,SrcList,ItemTypeID,CostItemNum) ->
	check_pft_foods(Foods,SrcList,[],ItemTypeID,CostItemNum);
check_pft_foods(Foods,GerID,SrcList,ItemTypeID,CostItemNum) when is_integer(GerID)->
	check_pft_foods(Foods,role_data:get_bagEquip(),SrcList,ItemTypeID,CostItemNum);
check_pft_foods(Foods,List1,List2,ItemTypeID,CostItemNum) ->
	{List1_2,DelList,Result} = 
	lists:foldl(fun(FoodID,{ListAcc,DelAcc,ResultAcc})->
						case lists:keytake(FoodID,#item.itemUID,ListAcc) of
							false ->
								{ListAcc,DelAcc,merge_result(ResultAcc,3)};
							{value,Item,OtherList} ->
								#item{itemRank=Rank,itemTypeID=FItemTypeID} = Item,
								if Rank >= 1 ->
									   {ListAcc,DelAcc,merge_result(ResultAcc,4)};
								   true ->
                                        IsLegend = lists:member(ItemTypeID, ?LEGENDLIST),
                                        if
                                            (IsLegend =:= false andalso FItemTypeID =:= ItemTypeID)
                                              orelse 
                                            (IsLegend =:= true andalso FItemTypeID + ?LEGENDIDSHIFT =:= ItemTypeID)->
											   {OtherList,[Item|DelAcc],merge_result(ResultAcc,0)};
										   true ->
											   {ListAcc,DelAcc,merge_result(ResultAcc,3)}
									   end
								end
						end
				end,{List1,[],0},Foods),
	case Result of
		0 ->
			case length(DelList) of
				CostItemNum ->
					{true,List1_2,List2,DelList};
				_ ->
					{false,6}
			end;
		_ ->
			{false,Result}
	end.

%%对玩家数据不做任何操作，只是返回主符文和材料符文，以及去除这些符文之后的装备背包以及对应精灵的装备列表(去除主符文)
check_item_stone_uprank(SrcStoneUID,SrcStoneGerID,FoodStoneUID)->
	case take_item(SrcStoneGerID,SrcStoneUID) of
		false->
			{false,2};
		{value,SrcStone,SrcOtherEquip}->
			#item{itemTypeID=ItemTypeID,itemType=ItemType,itemLevel=_ItemLevel,itemRank=ItemRank,itemPos=_ItemPos,itemExp=_ItemExp} = SrcStone,
			#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
			case item_lib:is_itemType_stone(ItemType) orelse item_lib:is_itemType_essence_stone(ItemType) of
				true->
					StoneMaxRank = data_item_stone:get(stone_max_uprank),
					if
						ItemRank >= StoneMaxRank->
							{false,4};
						true->
							{BagEquip,GerEquipList} = case SrcStoneGerID =:= 0 of
								true->
									{SrcOtherEquip,[]};
								false->
									{role_data:get_bagEquip(),SrcOtherEquip}
							end,
							case lists:keytake(FoodStoneUID,#item.itemUID,BagEquip) of
								false->
									{false,5};
								{value,#item{itemLevel=_FoodStoneLevel,itemRank=FoodStoneRank,itemTypeID=FoodStoneTypeID}=FoodStone,OtherBagEquip}->
									case check_item_stone_uprank_type(ItemTypeID,FoodStoneTypeID) of
										false->
											{false,6};
										true->
											case get_stone_uprank_cost(ItemRank,FoodStoneRank,ItemStar) of
												false->
													?ERR("undefined uprank config:  Star :~w ItemRank:	~w ~n",[ItemStar,ItemRank]),
													{false,8};
												CoinCost->
													Role = role_data:get_roleInfo(),
													case role_lib:check_money(Role,coin,CoinCost) of
														false->
															{false,7};
														true->
															{true,SrcStoneGerID,SrcStone,OtherBagEquip,GerEquipList,CoinCost,FoodStone}
													end
											end
									end
							end
					end;
				false->
					{false,3}
			end
	end.

% 同星阶符文可以精炼
check_item_stone_uprank_type(ItemTypeID,FoodStoneTypeID)->
    check_item_stone_uprank_type2(ItemTypeID,FoodStoneTypeID,data_item_stone:get(stone_uprank_type)).
check_item_stone_uprank_type2(_ItemTypeID,_FoodStoneTypeID,[])->
    false;
check_item_stone_uprank_type2(ItemTypeID,FoodStoneTypeID,[HList|OtherConfigGroup])->
    A = lists:member(ItemTypeID, HList),
    B = lists:member(FoodStoneTypeID, HList),
    if
        A =:= true andalso B =:= true ->
            true;
        A =:= false andalso B =:= false ->
            check_item_stone_uprank_type2(ItemTypeID,FoodStoneTypeID,OtherConfigGroup);
        true ->
            false
    end.    

%%符文精炼可以分为两个阶段，1->根据材料符文与主符文的品阶信息，生成主符文新的品阶等级，2->调用符文吞噬(升级)的接口，使主符文继承材料符文的经验等级等信息
do_stone_uprank(SrcStoneGerID,SrcStone,NewBagEquip,NewGerEquipList,CostCoin,Food)->
	#item{itemUID=ItemUID,itemTypeID=ItemTypeID} = SrcStone1 = motify_stone_rank(SrcStone,Food),
	Role = role_data:get_roleInfo(),
	#role{roleID=RoleID} = role_lib:deduct_money_f(Role,coin,CostCoin,?MONEY_DEC_TYPE_STONE_UPRANK, SrcStone1#item.itemRank, integer_to_list(ItemUID)),
	LogItemList = role_item:itemList2logItemList([Food], []),
	?sendself(#sc_item_delete_notify{itemUIDList=[Food#item.itemUID]}),
	{Date, _} = Time = erlang:localtime(),
	% behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_STONE_UPRANK, 0, ""),
	SrcStone2 = do_stone_eat(SrcStone1, NewGerEquipList,[Food],NewBagEquip,SrcStoneGerID,?MONEY_DEC_TYPE_STONE_UPRANK,?STONE_UPRANK),
	case SrcStoneGerID of
		0 ->
			role_data:set_bagEquip([SrcStone2|NewBagEquip]);
		_ ->
			role_data:set_bagEquip(NewBagEquip),
			role_data:set_equip(SrcStoneGerID, [SrcStone2|NewGerEquipList]),
			ger_attr:recacl_f(SrcStoneGerID),
			role_payGuide:trigger_task_change(?FORMATION_GER_STONE_TN,{SrcStoneGerID})
	end,
	behavior_item_uprank:log(RoleID, ItemUID, ItemTypeID, SrcStone2#item.itemLevel, SrcStone2#item.itemLevel, SrcStone#item.itemRank,SrcStone1#item.itemRank, 0, Date, Time),
	% ?CATCH(role_task_trigger:handle({dispach_task,equip_up_quality,RoleID,ItemUID,ItemTypeID,SrcStone1#item.itemRank})),
	NewStone = item_lib:item2p_item(SrcStone2),
	?sendself(#sc_item_stone_uprank{result=1,newstone=NewStone,srcStoneUID=SrcStone2#item.itemUID,foodStoneUID=Food#item.itemUID}).
	%%前端p_item解析错误，此处将再次发送升级后的符文等级同步
	%%推送新符文品阶
	% ?sendself(#sc_item_update_rank{itemUID=SrcStone2#item.itemUID,newItemDecay=item_lib:itemDecay(SrcStone2#item.itemDecay),newItemRank=SrcStone2#item.itemRank}).

down_rank_item_type(TypeID) ->
	case data_item:get(TypeID) of
		?undefined ->
			11;
		#data_item{itemStar=Star} ->
			10+Star
	end.

do_item_down_rank(#role{roleID=RoleID}=Role,#item{itemUID=ItemUID,itemRank=Rank}=Item,OtherEquipList,NeedGold,SrcGerUID) ->
	Role2 = role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_ITEM_DOWN_RANK, Rank, integer_to_list(ItemUID)),
	%Reward = [#new_item{itemTypeID=Item#item.itemTypeID,itemNum=1,itemLevel =1,itemRank =0}],
	EggID = down_rank_item_type(Item#item.itemTypeID),
	Reward = [#new_item{itemTypeID=EggID,itemNum=1,itemLevel=1,itemRank=0}],
    role_reward:handle_item_f(Role2,Reward,?MONEY_ADD_TYPE_ITEM_DOWN_RANK,Rank,integer_to_list(ItemUID)),
%	item_lib:add_item_f(Reward, ?MONEY_ADD_TYPE_ITEM_DOWN_RANK, Rank, integer_to_list(ItemUID)),
	NewItem = Item#item{itemRank=Rank-1},
	case SrcGerUID of
		0 ->
			role_data:set_bagEquip([NewItem|OtherEquipList]);
		_ ->
			OldEquipList = role_data:get_equip(SrcGerUID),
			role_data:set_equip(SrcGerUID, [NewItem|OtherEquipList]),
			role_lvlSgAttr:on_equip_lvl_up(SrcGerUID, OldEquipList),
            role_lvlSgAttr:on_equip_rank_up(SrcGerUID, OldEquipList),
			ger_attr:recacl_f(SrcGerUID)
	end,
	behavior_item_downrank:log(RoleID, SrcGerUID, Item#item.itemTypeID, Item#item.itemLevel, Rank, Rank-1, erlang:localtime()),
	?sendself(#sc_item_down_rank{result=1,add_item_list=role_reward:transform2p_reward_view([{6,EggID,1}],[]),itemUID=ItemUID,rank=Rank-1}).	

check_item_down_rank(SrcItemUID,SrcItemGerID) ->
	case take_item(SrcItemGerID, SrcItemUID) of
        false ->
            {false, 3};
        {value, SrcItem, SrcList} ->
            #item{itemRank=ItemRank,itemType=ItemType} = SrcItem,
            IsLegend = lists:member(ItemType, ?LEGENDLIST),
			case item_lib:is_itemType_equip(ItemType) of
				false ->
					{false,5};
                true when IsLegend =:= true->
                    {false,5};
				true->
					if ItemRank >=1 andalso ItemRank < 11 ->
						   Role = role_data:get_roleInfo(),
						   NeedGold = data_common:get(down_item_need_gold),
						   case role_lib:check_money(Role,gold, NeedGold) of
							   true ->
								   {true,Role,SrcItem,SrcList,NeedGold};
							   _->
								   {false,4}
						   end;
					   true ->
						   {false,2}
					end
			end
	end.

do_x_get(#role{roleID=RoleID}=Role,NewEquipList,DelList,CostCoin,CostTicket,TarTypeID,MaxLevel) ->
	Role2 = role_lib:deduct_money_f(Role, coin, CostCoin, ?MONEY_DEC_TYPE_ITEM_X_GET, TarTypeID, ""),
	role_lib:deduct_money_f(Role2, ticket,CostTicket,?MONEY_DEC_TYPE_ITEM_X_GET, TarTypeID, ""),
	role_data:set_bagEquip(NewEquipList),
	LogItemList = role_item:itemList2logItemList(DelList, []),
	DeletItemUIDList = [ItemUID||#item{itemUID=ItemUID}<-DelList],
	?sendself(#sc_item_delete_notify{itemUIDList=DeletItemUIDList}),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ITEM_X_GET, 0, ""),
	Reward = [#new_item{itemTypeID=TarTypeID,itemNum=1,itemLevel =MaxLevel,itemRank =0}],
	item_lib:add_item_f(Reward, ?MONEY_ADD_TYPE_ITEM_X_GET, TarTypeID, ""),
	?sendself(#sc_item_x_get{result=1}).	

merge_result(0,R) -> R;
merge_result(L,_) -> L. 
check_x_get(TarTypeID, ItemIDs) ->
	case data_item_x_get:get(TarTypeID) of
		{CostStar,CostItemNum,CostCoin,CostTicket,CostItemType} ->
			case length(ItemIDs) of
				CostItemNum ->
					Role = role_data:get_roleInfo(),
					CheckMoneyEnough = role_lib:check_money(Role,coin,CostCoin) andalso role_lib:check_money(Role,ticket,CostTicket),
					case CheckMoneyEnough of
						true ->
							EquipList = role_data:get_bagEquip(),
							{NewEquipList,DelList,Result,MaxLevel} = 
								lists:foldl(fun(ItemID,{EquipListAcc,DelListAcc,LR,ML})->
													case lists:keytake(ItemID, #item.itemUID, EquipListAcc) of
														false ->
															{EquipListAcc,DelListAcc,merge_result(3,LR),ML};
														{value,Item,EquipListAcc2}->
															#item{itemTypeID=ItemTypeID,itemRank=ItemRank,itemLevel=ItemLevel,itemType=ItemType}=Item,
															CheckCostItemType = (item_lib:is_itemType_equip(ItemType) andalso CostItemType == 1) orelse
																					((item_lib:is_itemType_trainer_equip(ItemType) orelse item_lib:is_itemType_trainer_stone(ItemType)) andalso CostItemType == 2),
															case CheckCostItemType of
																true ->
																	#data_item{itemStar=ItemStar}=data_item:get(ItemTypeID),
																	case ItemStar of
																		CostStar ->
																			if ItemRank >= 1 ->
																				   {EquipListAcc,DelListAcc,merge_result(6,LR),ML};
																			   true ->
																				   {EquipListAcc2,[Item|DelListAcc],merge_result(0,LR),max(ML,ItemLevel)}
																			end;
																		_ ->
                                                                            ?INFO("CheckCostItemType ~w ~w",[ItemStar,CostStar]),
																			{EquipListAcc, DelListAcc,merge_result(5,LR),ML}
																	end;
																_ ->
                                                                    ?INFO("CheckCostItemType ~w ~w ~w",[CheckCostItemType,ItemType,CostItemType]),
																	{EquipListAcc,DelListAcc, merge_result(5,LR),ML}
															end
													end
											end,{EquipList,[],0,1},ItemIDs),
							case length(DelList) of
								CostItemNum ->
									{true,Role,NewEquipList,DelList,CostCoin,CostTicket,MaxLevel};
								_ ->
									if Result == 0 ->
										   {false,3};
									   true ->
										   {false,Result}
									end
							end;
						_ ->
							{fale,4}
					end;
				_ ->
					{false,4}
			end;
		{#sell_reward{item=ItemList}=SellReward,KeyStoneNum}->
			Result = get_real_stonelist(ItemIDs,{0,[]},true),
			case Result of
				false->
					{false,4};
				{Num,StoneList}->
					case Num =:= KeyStoneNum of
						true->
							ItemStoneList = [#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=1,itemRank=0}||{ItemTypeID,ItemNum}<-StoneList],
							AllSellReward =  SellReward#sell_reward{item=ItemList++ItemStoneList},
							role_item:delete_sell_reward(AllSellReward,[],[]);
						false->
							{false,4}
					end
			end;
		_ ->
			{false,2}
	end.
get_real_stonelist(_ItemIDs,{_NumAcc,_StoneAcc},false)->
	false;
get_real_stonelist([],{NumAcc,StoneAcc},true)->
	{NumAcc,StoneAcc};
get_real_stonelist([ID|T],{NumAcc,StoneAcc},true)->
	Num = ID band 16#FFFFFFFF,
	ItemTypeID = (ID bsr 32) band 16#FFFFFFFF,
	case check_arg(Num,ItemTypeID) of
		true->
			NewStoneAcc = case lists:keytake(ItemTypeID,1,StoneAcc) of
				false->
					[{ItemTypeID,Num}|StoneAcc];
				{_Value,{ItemTypeID,FindNum},Other}->
					[{ItemTypeID,FindNum+Num}|Other]
			end,
			get_real_stonelist(T,{NumAcc+Num,NewStoneAcc},true);
		false->
			get_real_stonelist(T,{NumAcc,StoneAcc},false)
	end.
check_arg(Num,ItemTypeID)->
	Num > 0 andalso lists:member(ItemTypeID,data_item_x_get:get(keystonelist)).

do_item_enchant(EnchantType,GerID,ItemUID)->
	#role{level=Level} = role_data:get_roleInfo(),
	case Level >= data_item_enchant:get(min_enchant_limit) andalso data_item_enchant:get(enchant_is_open) of
		true->
			next;
		false->
			erlang:throw({error,7})
	end,
	case lists:member(EnchantType,?ENCHANT_TYPE) of
		false->
			erlang:throw({error,3});
		true->
			next
	end,
	Item = case is_equip_exist(GerID,ItemUID) of
		false ->
			erlang:throw({error,2});
		{FindOne,_Other}->
			FindOne
	end,
	case check_enchant_valid(Item,EnchantType) of
		false->
			erlang:throw({error,4});
		true->
			next
	end,
	case is_enchant_cost_satisfied(Item,EnchantType,GerID) of
		false->
			erlang:throw({error,5});
		{true,NewEquip}->
			{ok,NewEquip}
	end.

is_equip_exist(GerID,ItemUID)->
	case GerID =:= 0 of 
		true->
			false;
		_ ->
			case take_item(GerID,ItemUID) of
				false->
					false;
				{_,FindOne,Other}->
					{FindOne,Other}
			end
	end.

check_enchant_valid(Item,EnchantType)->
	case Item#item.itemenchantType=:=EnchantType of
		false ->
			true;
		true->
			Item#item.itemenchantLevel < data_item_enchant:get(max_enchant_level)
	end.

is_enchant_cost_satisfied(Item,EnchantType,GerID)->
	EnchantLevel=case EnchantType=:= Item#item.itemenchantType of
					true->
						Item#item.itemenchantLevel + 1;
					false->
						1
				end,
	case data_item_enchant:get({data_item_enchant_cost,EnchantType,EnchantLevel}) of
		?undefined->
			false;
		Cost ->
			case catch delete_sell_reward(Cost,[Item#item.itemUID],[]) of
				false ->
					false;
				{true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
					Item2 = case EnchantType =:= Item#item.itemenchantType of
						true->
							Item#item{itemenchantLevel=Item#item.itemenchantLevel+1};
						false->
							Item#item{itemenchantType=EnchantType,itemenchantLevel=1}
						end,
					#role{roleID=RoleID} = role_data:get_roleInfo(),
					{Date,_} = Time = erlang:localtime(),
					SrcItem2 = refresh_item(Item2),
					if 
						GerID =/=0 ->
							GerEquipList = get_itemList(GerID),
							case lists:keytake(SrcItem2#item.itemUID,#item.itemUID,GerEquipList) of
								{_,_FindOne,Other}->
				   					set_itemList(GerID, [SrcItem2|Other]),
				   					behavior_item_enchant:log(RoleID,SrcItem2#item.itemUID,SrcItem2#item.itemTypeID,Item#item.itemenchantType,Item#item.itemenchantLevel,EnchantType,EnchantLevel,Date,Time),
				   					update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_ECHANT_COST,Item#item.itemUID,"");
				   				false->
				   					?ERR("在精灵装备列表中未查找到对应的道具：~w ~n",[SrcItem2]),
				   					false
				   			end;
			   			true ->
			   				case lists:keytake(SrcItem2#item.itemUID,#item.itemUID,NewEquipList) of
			   					{_,_FindOne,Other}->
			   						NewEquipList2 = [SrcItem2|Other],
				   					behavior_item_enchant:log(RoleID,SrcItem2#item.itemUID,SrcItem2#item.itemTypeID,Item#item.itemenchantType,Item#item.itemenchantLevel,EnchantType,EnchantLevel,Date,Time),
									update_role_info2(NewItemList,NewLastGerList,NewEquipList2,DeleteCurrencyList,?MONEY_DEC_TYPE_ECHANT_COST,Item#item.itemUID,"");
			   					false->
			   						?ERR("处理完成之后居然找不到玩家的道具~w 。。。~n",[SrcItem2]),
			   						false
			   				end
					end,
					if 
                		GerID =:= 1000 ->
                    		?INFO("trainer equip up rank, re cacl attr"),
                    		ger_attr:recacl_gers(); %训练师装备升阶
                		GerID =/= 0 ->
				   			ger_attr:recacl_f(GerID);
			   			true ->
				   			ignore
					end,
					{true,SrcItem2}
			end			
	end.



check_compound(ItemTypeID) ->
	case data_compound:get(ItemTypeID) of
		#data_compound{needNum=NeedNum,product=Product,baseNeedNum=BaseNeedNum,otherList=OtherList} ->
			case item_lib:check_material(ItemTypeID, NeedNum) of
				false ->
					case check_compound_need_material(ItemTypeID, NeedNum, BaseNeedNum, OtherList) of
						false->
							{false, 2};
						{true, BagOther3, DelAcc, UpdateAcc, UpdateLogList} ->
							{true, BagOther3, Product, NeedNum, DelAcc, UpdateAcc, UpdateLogList}
					end;
				{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
					{true, BagOther2, Product, NeedNum, DelAcc, UpdateAcc, UpdateLogList}
			end;			
		_ ->
			{false, 3}
	end.

check_compound_need_material(ItemTypeID, NeedNum, BaseNeedNum, OtherList) ->
	BagOther = role_data:get_bagItem(),
	case item_lib:check_material2(BagOther, ItemTypeID, BaseNeedNum) of
		{_, 0, _, _, _} ->
			{BagOther2, RestNum1, DelAcc, UpdateAcc, UpdateLogList} =
				item_lib:check_material2(BagOther, ItemTypeID, NeedNum),
			{BagOtherT, RestNumT, DelAccT, UpdateAccT, UpdateLogListT} = 
				lists:foldl(fun(_ItemTypeIDM, {BagOtherAcc, 0,DelAccAcc,UpdateAccAcc,UpdateLogAccAcc})->
									{BagOtherAcc, NeedNum, DelAccAcc,UpdateAccAcc,UpdateLogAccAcc};
							   (ItemTypeIDM, {BagOtherAcc, RestNumAcc,DelAccAcc,UpdateAccAcc,UpdateLogAccAcc})->
									{BagOtherAccT, RestNumAccT, DelAccAccT, UpdateAccAccT, UpdateLogAccAccT} = 
									item_lib:check_material2(BagOtherAcc, ItemTypeIDM, RestNumAcc),
									{BagOtherAccT, RestNumAccT, DelAccAccT ++ DelAccAcc, UpdateAccAccT ++ UpdateAccAcc, UpdateLogAccAccT ++ UpdateLogAccAcc}
							end, {BagOther2, RestNum1, DelAcc, UpdateAcc, UpdateLogList}, OtherList),
			if RestNumT =:= 0 ->
				   {true ,BagOtherT, DelAccT, UpdateAccT, UpdateLogListT};
			   true ->
				   false
			end;
		_ ->
			false
	end.


take_item(0=_ItemGerID, ItemUID) ->
	BagEquip = role_data:get_bagEquip(),
	lists:keytake(ItemUID, #item.itemUID, BagEquip);
take_item(ItemGerID, ItemUID) ->
	GerEquip = role_data:get_equip(ItemGerID),
	lists:keytake(ItemUID, #item.itemUID, GerEquip).

get_itemList(0) ->
	role_data:get_bagEquip();
get_itemList(ItemGerID) ->
	role_data:get_equip(ItemGerID).
set_itemList(0=_ItemGerID, List) ->
	role_data:set_bagEquip(List);
set_itemList(ItemGerID, List) ->
	role_data:set_equip(ItemGerID, List).

%% 计算宠物装备升品需要的银两
cacl_up_rank_coin_ger(NewQuality, Quality1, Quality2, GerStar) ->
	get_up_rank_coin(NewQuality, GerStar) - get_up_rank_coin(Quality1, GerStar) - get_up_rank_coin(Quality2, GerStar).

get_up_rank_coin(0, _GerStar) ->
	0;
get_up_rank_coin(Quality, GerStar) ->
	data_item_up_rank:get({GerStar, Quality}).

%% 计算训练师装备升品需要的徽章
cacl_up_rank_reputation_trainer(NewQuality, Quality1, Quality2, GerStar) ->
    get_trainer_up_rank_coin(NewQuality, GerStar) - get_trainer_up_rank_coin(Quality1, GerStar) - get_trainer_up_rank_coin(Quality2, GerStar).

get_trainer_up_rank_coin(0, _GerStar) ->
    0;
get_trainer_up_rank_coin(Quality, GerStar) ->
    data_item_trainer_up_rank:get({GerStar, Quality}).

%%计算加速装备需要消耗的值
cacl_up_rank_reputation_accelerate_equip(NewRank,FoodRank,ItemRank,SrcItemStar)->
	get_accelerate_equip_up_rank_reputation(NewRank,SrcItemStar) - get_accelerate_equip_up_rank_reputation(FoodRank,SrcItemStar) - get_accelerate_equip_up_rank_reputation(ItemRank,SrcItemStar).

get_accelerate_equip_up_rank_reputation(0,_ItemStar)->
	0;
get_accelerate_equip_up_rank_reputation(Rank,ItemStar)->
	data_item_up_rank:get({accelerate_equip,ItemStar,Rank}).

check_up_rank(SrcItemUID, SrcItemGerID, FoodItemUID, FoodItemGerID) ->
    case take_item(SrcItemGerID, SrcItemUID) of
        false ->
            {false, 3};
        {value, SrcItem, SrcList} ->
            #item{itemRank=ItemRank,itemTypeID=SrcItemTypeID,itemType=ItemType} = SrcItem,
            IsLegend = lists:member(SrcItemTypeID, ?LEGENDLIST),
            case item_lib:is_equip(SrcItem) orelse item_lib:is_accelerate_equip(ItemType) of
                false ->
                    {false, 7};
                true when IsLegend =:= true->
                    {false,7};
                true ->
                    %#data_item{itemMaxRank=MaxRank} = 
					MaxRank = 10,
					DataItemSrc = data_item:get(SrcItemTypeID),
                    if ItemRank >= MaxRank -> %这里只是检查宠物装备的品阶上限，训练师的检查在后面			
                           {false, 2};
                       true ->
                           if SrcItemGerID =:= FoodItemGerID ->
                                  FoodList = SrcList;
                              true ->
                                  FoodList = get_itemList(FoodItemGerID)
                           end,
                           case lists:keytake(FoodItemUID, #item.itemUID, FoodList) of
                               false ->
                                   {false, 4};
                               {value, FoodItem, FoodList2} ->
                                   #item{itemTypeID=FoodItemTypeID,itemRank=FoodRank} = FoodItem,
                                   #data_item{itemType=SrcItemType, itemStar=SrcItemStar} = DataItemSrc,
                                   #data_item{itemType=FoodItemType, itemStar=FoodItemStar} = data_item:get(FoodItemTypeID),
								   if FoodRank > MaxRank ->
										  {false,10};
                                      SrcItemType =/= FoodItemType ->
                                          {false, 5};
                                      SrcItemStar =/= FoodItemStar ->
                                          {false, 8};
                                      true ->
                                          NewRank = erlang:min(FoodRank+ItemRank+1, MaxRank),
                                          IsGerEquip = lists:member(SrcItemType, ?EQUIP_TYPE_LIST_NO_STONE),
                                          IsTrainerEquip = lists:member(SrcItemType, ?TRAINER_EQUIP_TYPE),
                                          IsAccelerateEquip = item_lib:is_accelerate_equip(SrcItemType),
                                          TrainerMaxRank = data_reinforce_trainer:get(rank_reputation_max),
                                          if
                                              IsGerEquip->
                                                  NeedCoinT = cacl_up_rank_coin_ger(NewRank, FoodRank, ItemRank, SrcItemStar),
                                                  case NeedCoinT < 0 of
                                                    true ->
                                                        NeedCoin = 0;
                                                    false ->
                                                        NeedCoin = NeedCoinT
                                                  end,
                                                  Role = role_data:get_roleInfo(),
                                                  ?INFO("check_up_rank ger ~w ~w ~w",[Role#role.coin,NeedCoin,NeedCoinT]),
                                                  if Role#role.coin >= NeedCoin ->
                                                         {true, SrcItem, SrcList, FoodItem, FoodList2, DataItemSrc, NeedCoin, 0, NewRank, Role};
                                                     true ->
                                                         {false, 6}
                                                  end;
                                              IsAccelerateEquip ->
                                              	   NeedReputationT = cacl_up_rank_reputation_accelerate_equip(NewRank,FoodRank,ItemRank,SrcItemStar),
                                              	   case NeedReputationT < 0 of
                                              	   		true->
                                              	   			NeedReputation = 0;
                                              	   		false->
                                              	   			NeedReputation = NeedReputationT
                                              	   	end,
                                              	   	Role = role_data:get_roleInfo(),
                                                    ?INFO("check_up_rank accelerate_equip ~w ~w ~w",[Role#role.reputation,NeedReputation,NeedReputationT]),
                                                    if Role#role.reputation >= NeedReputation ->
                                                         {true, SrcItem, SrcList, FoodItem, FoodList2, DataItemSrc, 0, NeedReputation, NewRank, Role};
                                                     true ->
                                                         {false, 9}
                                                    end;
                                              IsTrainerEquip andalso FoodItemTypeID /= SrcItemTypeID->
                                                  {false, 5};
                                              IsTrainerEquip andalso TrainerMaxRank < NewRank-> %检查是否超过训练师装备品阶上限
                                                  ?INFO("up rank ~w ~w",[NewRank,TrainerMaxRank]),
                                                  {false, 2};
                                              IsTrainerEquip andalso FoodItemTypeID =:= SrcItemTypeID->
                                                  NeedRepT = cacl_up_rank_reputation_trainer(NewRank, FoodRank, ItemRank, SrcItemStar),
                                                  case NeedRepT < 0 of
                                                    true ->
                                                        NeedRep = 0;
                                                    false ->
                                                        NeedRep = NeedRepT
                                                  end,
                                                  Role = role_data:get_roleInfo(),
                                                  ?INFO("check_up_rank trainer ~w ~w ~w",[Role#role.reputation,NeedRep,NeedRepT]),
                                                  if Role#role.reputation >= NeedRep ->
                                                         {true, SrcItem, SrcList, FoodItem, FoodList2, DataItemSrc, 0, NeedRep, NewRank, Role};
                                                     true ->
                                                         {false, 9}
                                                  end;
                                              true ->
                                                  {false, 8}
                                          end
                                              
                                          
                                          
                                   end
                           end
                    end
            end
    end.


max_treasure_exp() ->
	data_treasure_rank:get(?MAX_RANK_OF_TREASURE).

max_treasure_exp2() ->
	data_treasure_rank2:get(?MAX_RANK_OF_TREASURE).

					
check_treasure_eat(SrcItemUID, SrcItemGerID, FoodItemIDList) ->
	case length(FoodItemIDList) > 4 of
		true ->
			{false, 3};
		false->
			case take_item(SrcItemGerID, SrcItemUID) of
				false->
					{false, 5};
				{value, SrcItem, SrcList} ->
					#item{itemType=ItemType,itemExp=SrcItemExp} = SrcItem,
					case item_lib:is_treasure(ItemType) of
						false->
							{false, 2};
						true->
							DataItemSrc = data_item:get(ItemType),
							MaxExp = 
								if DataItemSrc#data_item.itemStar =:= 4 ->
									   max_treasure_exp();
								   true ->
									   max_treasure_exp2()
								end,
							if SrcItemExp >= MaxExp ->
								   {false, 4};
							   true->
								   if SrcItemGerID =:= 0 ->
										  FoodList = SrcList;
									  true->
										  FoodList = get_itemList(0)
								   end,
								   case util:foldl(fun(E, {ItemBagAcc,FoodAcc,JudgeAcc}) -> 
														   case lists:keytake(E, #item.itemUID, ItemBagAcc) of
															   false ->
																   {return, false};
															   {value,ItemT,ItemBagAcc2} ->
																   DataItem = data_item:get(ItemT#item.itemTypeID),
																   if DataItem#data_item.itemStar =:= 5 ->
																		  {ItemBagAcc2, [ItemT|FoodAcc], false};
																	  true ->
																		  {ItemBagAcc2, [ItemT|FoodAcc],true andalso JudgeAcc}
																   end
														   end
												   end, {FoodList,[], true}, FoodItemIDList) of
									   false ->
										   {false, 6};
									   {ItemBag3, ItemList, true} ->
										   {true, SrcItem, SrcList, ItemList, ItemBag3};
									   {_ItemBag3, _ItemList, false} ->
										   {false, 7}
								   end
							end
					end
			end
	end.		

do_max_reinforce(ItemUID, GerID, NeedValue, Item, EquipList2, Role, DataItem, OwnerType) ->
	#item{itemLevel=ItemLevel,
		  itemType=ItemType ,
		  itemTypeID=ItemTypeID
		 % itemRank=ItemRank
		 } = Item,
	#data_item{itemStar=ItemStar} = DataItem,
	#role{vipLevel=VipLevel, coin=Coin, level=RoleLevel, roleID=RoleID, reputation=Reputation} = Role,
	AddLevel = random_add_level(VipLevel),
	ItemLevel2T = ItemLevel+AddLevel,
    ConfigureMaxLevel = data_reinforce_trainer:get(base_reputation_max),
	ItemLevel2 = if ItemLevel2T >=  ConfigureMaxLevel -> ConfigureMaxLevel ; true -> ItemLevel2T end,
    case OwnerType of
        ger ->
            Coin2 = Coin - NeedValue,
            ?INFO("max_reinforce-1-:~w",[[ger,Coin2, VipLevel, ItemLevel2, ItemType, ItemStar, [ItemLevel2], RoleLevel,1,ConfigureMaxLevel]]),
            {NewCoin, ItemNewLevel, TempList, Num} = max_reinforce(ger,Coin2, VipLevel, ItemLevel2, ItemType, ItemStar, [ItemLevel2], RoleLevel,1,ConfigureMaxLevel), %不限制实际的强化上限，即暴击可超过角色等级
            ?INFO("max_reinforce-2-:~w",[[NewCoin, ItemNewLevel, TempList, Num]]),
            DeductValue = Coin-NewCoin,
            role_lib:deduct_coin_f(Role, DeductValue, ?MONEY_DEC_TYPE_ITEM_MAX_REINFORCE, 0, "");
        trainer ->
            Reputation2 = Reputation - NeedValue,
            {NewReputation, ItemNewLevel, TempList, Num} = max_reinforce(trainer,Reputation2, VipLevel, ItemLevel2, ItemType, ItemStar, [ItemLevel2], RoleLevel,1,ConfigureMaxLevel),
            DeductValue = Reputation-NewReputation,
            role_lib:deduct_reputation_f(Role, DeductValue, ?MONEY_DEC_TYPE_ITEM_MAX_REINFORCE, 0, "");
        accelerate_equip->
        	Reputation2 = Reputation - NeedValue,
            {NewReputation, ItemNewLevel, TempList, Num} =  max_reinforce(accelerate_equip,Reputation2, VipLevel, ItemLevel2, ItemType, ItemStar, [ItemLevel2], RoleLevel,1,ConfigureMaxLevel),
            DeductValue = Reputation-NewReputation,
            role_lib:deduct_reputation_f(Role, DeductValue, ?MONEY_DEC_TYPE_ITEM_MAX_REINFORCE, 0, "")
    end,
	NewItem2 = refresh_item(Item#item{itemLevel=ItemNewLevel}),
	EquipList3 = [NewItem2|EquipList2],
	if 
        GerID =:= 0 ->
		    role_data:set_bagEquip(EquipList3);
        GerID =:= 1000 -> %训练师装备强化
           role_data:set_equip(GerID, EquipList3),
           ?INFO("trainer equip reinforce, re cacl attr"),
           ger_attr:recacl_gers(); 
        true ->
           OldEquipList = role_data:get_equip(GerID),   
		   role_data:set_equip(GerID, EquipList3),
           role_lvlSgAttr:on_equip_lvl_up(GerID, OldEquipList),
		   ger_attr:recacl_f(GerID) 
	end,
	
	%% 写道具日志
	{Date, _} = Time = erlang:localtime(),
	behavior_item_uplevel:log(RoleID, ItemUID, ItemTypeID, ItemNewLevel-ItemLevel, ItemNewLevel, length(TempList), DeductValue, Date, Time),
	?CATCH(role_task_trigger:handle({dispach_task,equip_strong,RoleID,ItemUID,ItemTypeID,ItemNewLevel,Num})),
	role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_LEVEL_TN,{GerID}),
	role_maintask:do_main_task(?FORMATION_GER_EQUIP_LEVEL_N_TN_TN,{GerID}),
	TempList.


%MaxLevel 用来防止暴击导致装备等级超过配置范围，目前仅针对训练师装备
max_reinforce(Owner, Value, VipLevel, ItemLevel, ItemType, ItemStar, List, RoleLevel,Num,MaxLevel) ->
    if ItemLevel >= RoleLevel orelse ItemLevel >= MaxLevel ->
		   {Value, lists:min([MaxLevel,ItemLevel]), lists:reverse(List),Num};
	   true ->
           NeedValue = case Owner of
                ger ->
                    cacl_reinforce_coin_ger(ItemType, ItemLevel, ItemStar);
                trainer ->
                    cacl_reinforce_reputation_trainer(ItemType, ItemLevel, ItemStar);
                accelerate_equip->
                	cacl_reinforce_coin_accelerate_equip(ItemType,ItemLevel,ItemStar)
           end,
		   if NeedValue > Value ->
				  {Value, ItemLevel, lists:reverse(List),Num};
			  true ->
				  AddLevel = random_add_level(VipLevel),
				  NewLevel = ItemLevel+AddLevel,
                  NewLevel2 = lists:min([MaxLevel,NewLevel]),
				  max_reinforce(Owner, Value-NeedValue, VipLevel, NewLevel2, ItemType, ItemStar, [NewLevel2|List], RoleLevel,Num+1,MaxLevel)
		   end
	end.

do_reinforce(ItemUID, GerID, NeedValue, Item, EquipList2, Role, _DataItem,OwnerType) ->
	#item{itemLevel=ItemLevel,itemTypeID=ItemTypeID} = Item,
	%#data_item{addAttr=AddAttr,itemStar=ItemStar} = DataItem,
	#role{vipLevel=VipLevel,roleID=RoleID} = Role,
    case OwnerType of
        ger ->
            role_lib:deduct_coin_f(Role, NeedValue, ?MONEY_DEC_TYPE_ITEM_REINFORECE, 0, "");
        trainer ->
            role_lib:deduct_reputation_f(Role, NeedValue, ?MONEY_DEC_TYPE_ITEM_REINFORECE, 0, "");
        accelerate_equip->
        	role_lib:deduct_reputation_f(Role,NeedValue,?MONEY_DEC_TYPE_ITEM_REINFORECE,0,"")
    end,
	AddLevel0 = random_add_level(VipLevel),
    TrainerMaxLevel = data_reinforce_trainer:get(base_reputation_max),
    ItemNewLevel0 = ItemLevel + AddLevel0,
%% 		ConfigureMaxLevel = data_common:get(max_role_level),
%% 	ItemNewLevel0 = if ItemNewLevel0T >= ConfigureMaxLevel -> ConfigureMaxLevel; true -> ItemNewLevel0T end,
	{ItemNewLevel,_AddLevel} = if
                        TrainerMaxLevel < ItemNewLevel0->
                            {TrainerMaxLevel,TrainerMaxLevel - ItemLevel};
                        true ->
                            {ItemNewLevel0,AddLevel0}
                    end,
	NewItem2 = refresh_item(Item#item{itemLevel=ItemNewLevel}),
	EquipList3 = [NewItem2|EquipList2],
	if 
        GerID =:= 0 ->
		   role_data:set_bagEquip(EquipList3);
        GerID =:= 1000 -> %训练师装备强化
           role_data:set_equip(GerID, EquipList3),
           ?INFO("trainer equip reinforce, re cacl attr"),
           ger_attr:recacl_gers(); 
	   true ->
           OldEquipList = role_data:get_equip(GerID),
		   role_data:set_equip(GerID, EquipList3),
           role_lvlSgAttr:on_equip_lvl_up(GerID, OldEquipList),
		   ger_attr:recacl_f(GerID),
		   %%触发上阵精灵装备等级变化
		   role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_LEVEL_TN,{GerID}),
		   role_maintask:do_main_task(?FORMATION_GER_EQUIP_LEVEL_N_TN_TN,{GerID})
	end,

	%% 写道具日志
	{Date, _} = Time = erlang:localtime(),
	behavior_item_uplevel:log(RoleID, ItemUID, ItemTypeID, ItemNewLevel-ItemLevel, ItemNewLevel, 1, NeedValue, Date, Time),
	case OwnerType of
		accelerate_equip->
			%%此处加速装备暂时不触发成就
			ignore;
		_->
			?CATCH(role_task_trigger:handle({dispach_task,equip_strong,RoleID,ItemUID,ItemTypeID,ItemNewLevel,1}))
	end,
	?sendself(#sc_item_reinforce{itemUID=ItemUID,result=1,newLevel=ItemNewLevel}).

random_add_level(VipLevel) ->
	ProbList = data_reinforce:get(prob),
	RandomConfig = 
		util:foldl(fun({L,C}, Acc) ->
						   if VipLevel >= L ->
								  {return,C};
							  true ->
								  Acc
						   end
				   end, [1,0,0,0,0], ProbList),
    % ?INFO("random_add_level ~w ~w",[ProbList,RandomConfig]),
	Total = lists:sum(RandomConfig),
	RandomValue = Total*random:uniform(),
	util:foldl(fun(E,{Acc,Num}) ->
					   NewAcc = Acc+E,
					   if NewAcc >= RandomValue ->
							  {return,Num+1};
						  true ->
							  {Acc+E, Num+1}
					   end
			   end, {0, 0}, RandomConfig).

check_reinforce(ItemUID, GerID) ->
	if GerID =:= 0 ->
		   EquipList = role_data:get_bagEquip();
	   true ->
		   EquipList = role_data:get_equip(GerID)
	end,
	%% 判断装备是否存在
	case lists:keytake(ItemUID, #item.itemUID, EquipList) of
		false ->
			{false, 4};
		{value, Item, EquipList2}->
			#role{level=RoleLevel, coin=Coin,reputation=Reputation} = Role = role_data:get_roleInfo(),
			#item{itemLevel=ItemLevel, itemTypeID=ItemTypeID,itemType=ItemType} = Item,
            TrainerMaxLevel = data_reinforce_trainer:get(base_reputation_max),
            %% 判断等级是否超过
            if
                ItemLevel >= RoleLevel ->
                    {false, 3};
                ItemLevel >= TrainerMaxLevel ->
                    {false, 10};
                true->
                    %% 判断道具配置是否存在
                    case data_item:get(ItemTypeID) of
                       #data_item{itemStar=ItemStar}=DataItem ->
                            if 
                                ItemType =:= ?weapon orelse ItemType =:= ?armor orelse ItemType =:= ?wing 
                                orelse ItemType =:= ?headwear orelse ItemType =:= ?totem orelse ItemType =:= ?runestone->   
                                    NeedCoin = cacl_reinforce_coin_ger(ItemType, ItemLevel, ItemStar),
                                    %% 判断银两是否足够
                                    case Coin >= NeedCoin of
                                        false ->
                                            {false, 2};
                                        true ->
                                            {true, ger, NeedCoin, Item, EquipList2, Role, DataItem}
                                        end;
                                ItemType =:= ?trainer_weapon orelse ItemType =:= ?trainer_gloves orelse ItemType =:= ?trainer_armor 
                                orelse ItemType =:= ?trainer_glasses orelse ItemType =:= ?trainer_watch orelse ItemType =:= ?trainer_boost
                                orelse ItemType =:= ?trainer_stone_ruby orelse ItemType =:= ?trainer_stone_topaz orelse ItemType =:= ?trainer_stone_sapphire ->   
                                    NeedReputation = cacl_reinforce_reputation_trainer(ItemType, ItemLevel, ItemStar),
                                    %% 判断银两是否足够
                                    case Reputation >= NeedReputation of
                                        false ->
                                            {false, 9};
                                        true ->
                                            {true, trainer, NeedReputation, Item, EquipList2, Role, DataItem}
                                    end;
                                true ->
                                	case item_lib:is_accelerate_equip(ItemType) of
                                		true->
                                			NeedReputation = cacl_reinforce_coin_accelerate_equip(ItemType,ItemLevel,ItemStar),
                                			case Reputation >= NeedReputation of
                                				false->
                                					{false,9};
                                				true->
                                					{true,accelerate_equip,NeedReputation,Item,EquipList2,Role,DataItem}
                                			end;
                                			%%加速装备强化
                                		false->
                                    		{false, 6}
                                    end
                            end;
                       _ ->
                           {false, 5}
                    end
            end    
	end.

%% 计算强化需要的银两
cacl_reinforce_coin_ger(?weapon,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(weapon_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin_ger(?armor,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(armor_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin_ger(?wing,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(wing_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin_ger(?headwear,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(headwear_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin_ger(?totem,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(totem_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel}));
cacl_reinforce_coin_ger(?runestone,ItemLevel, ItemStar) ->
	trunc(data_reinforce:get(runestone_coin_ratio)
			  *data_reinforce:get({star_coin_ratio,ItemStar})
			  *data_reinforce:get({base_coin, ItemLevel})).

cacl_reinforce_coin_accelerate_equip(_,ItemLevel,ItemStar)->
	trunc(data_reinforce:get(accelerate_equip_reputation_ratio)
		*data_reinforce:get({accelerate_equip_star_reputation_ratio,ItemStar})
		*data_reinforce:get({accelerate_equip_base_reputation,ItemLevel})).

cacl_reinforce_reputation_trainer(_,ItemLevel,ItemStar) ->
    trunc(data_reinforce_trainer:get(trainer_equip_reputation_ratio)
              *data_reinforce_trainer:get({star_reputation_ratio,ItemStar})
              *data_reinforce_trainer:get({base_reputation, ItemLevel})).

check_use_item(ItemNum,ItemUID) ->
	BagOther = role_data:get_bagItem(),
	case lists:keytake(ItemUID, #item.itemUID, BagOther) of
		false ->
			{false,2};
		{value, Item, BagOther2} ->
			if Item#item.itemNum >= ItemNum ->
				   {true ,Item, BagOther2};
			   true ->
				   {false, 2}
			end
	end.

%% EquipList是该宠物之前的装备信息
do_up_equip(GerID, ItemPos, ItemUID, UpItem, SrcEquipList, EquipList, ItemGerID) ->
    ?INFO("do_up_equip ~w",[{GerID, ItemPos, ItemUID, UpItem, SrcEquipList, EquipList, ItemGerID}]),
    %计算装备属性
	UpItem2 = item_lib:recacl(UpItem#item{itemPos=ItemPos}),
    %更新背包数据
	if ItemGerID == 0 ->
		   BagEquipList2 = SrcEquipList;
	   true ->
		   BagEquipList2 = role_data:get_bagEquip()
	end,
    %卸载同位置的装备至背包中 
	case lists:keytake(ItemPos, #item.itemPos, EquipList) of
		false ->
			EquipList2=EquipList,
			BagEquipList3=BagEquipList2;
		{value, DownItem, EquipList2} ->
			DownItem2 = DownItem#item{itemPos=0,addAttr=0},
			BagEquipList3=[DownItem2|BagEquipList2]
	end,
    %卸载同类型的装备至背包中 
	case lists:keytake(UpItem#item.itemType, #item.itemType, EquipList2) of
		false ->
			EquipList3 = EquipList2,
			BagEquipList4 = BagEquipList3;
		{value, AnotherDownItem, EquipList3}->
			AnotherDownItem2 = AnotherDownItem#item{itemPos=0,addAttr=0},
			BagEquipList4 = [AnotherDownItem2|BagEquipList3]
	end,
	role_data:set_bagEquip(BagEquipList4),
	NewEquipList = [UpItem2|EquipList3],
	role_data:set_equip(GerID, NewEquipList),
	if ItemGerID =:= 0 ->
		   ignore;
	   true ->
		   role_data:set_equip(ItemGerID, SrcEquipList),
		   ger_attr:recacl_f(ItemGerID)
	end,
	?CATCH(role_task_trigger:handle({dispach_task,equip_up_equip,ItemUID,UpItem#item.itemTypeID})),
    if
        GerID =:= 1000 ->
            ?INFO("trainer equip up, re cacl attr"),
            ger_attr:recacl_gers(); %训练师装备穿戴
        true ->
           role_lvlSgAttr:on_equip_change(GerID, EquipList),
	       ger_attr:recacl_f(GerID) 
    end,
	?sendself(#sc_item_up_equip{gerID=GerID,itemPos=ItemPos,itemUID=ItemUID,result=1}).


%% GerID 即将穿戴装备的宠物
%% ItemPos 穿戴的位置 1~6普通装备 11~13符文
%% ItemUID 穿上的装备的UID
%% ItemGerID 这件装备所在的武将ID，0=在背包里面
check_up_equip(GerID, ItemPos, ItemUID, ItemGerID) ->
    case lists:keyfind(GerID, #ger.gerID, role_data:get_posListT()) of
        false when GerID /= 1000->
            {false, 7}; %% //7=> 该武将未上阵，或不存在
		_ ->
			if 
                ItemGerID == GerID ->
                    {false, 5};
                true ->
				   EquipList = role_data:get_equip(GerID),
				   case take_item(ItemGerID, ItemUID) of
					   false ->
						   {false, 3};
					   {value, UpItem, BagEquipList2}->
						   ItemType = UpItem#item.itemType,
                           case check_pos(ItemType, ItemPos) of
                               false ->
                                   {false, 2};
                               true when GerID /= 1000-> %如果是宠物的装备
                                   case item_lib:is_itemType_equip(ItemType) of
                                       false ->
                                           {false, 4};
                                       true ->
                                           case check_treasure_type(ItemType, EquipList) of
                                               false ->
                                                   {true, UpItem, BagEquipList2, EquipList};
                                               true ->
                                                   {false, 6}
                                           end
                                   end;
                               true when GerID == 1000-> %如果是训练师的装备
                                   case item_lib:is_itemType_trainer_equip(ItemType) of
                                       false ->
                                           {false, 4};
                                       true ->
                                           case check_treasure_type(ItemType, EquipList) of
                                               false ->
                                                   {true, UpItem, BagEquipList2, EquipList};
                                               true ->
                                                   {false, 6}
                                           end
                                   end                                 
                           end                   
				   end
			end
	end.


%% GerID 即将穿戴装备的宠物
%% ItemPos 穿戴的位置 1~6普通装备 11~13符文
%% ItemUID 穿上的装备的UID
%% ItemGerID 这件装备所在的武将ID，0=在背包里面
check_up_stone(GerID, ItemPos, ItemUID, ItemGerID) ->
    case lists:keyfind(GerID, #ger.gerID, role_data:get_posListT()) of
        false when GerID /= 1000->
            {false, 7}; %% //7=> 该武将未上阵，或不存在
        _ ->
            if
                ItemGerID == GerID ->
                    {false, 5}; %% //7=> 该符文已经装备在同一个宠物身上
                true ->
                    case take_item(ItemGerID, ItemUID) of %% 检查欲装备物品是否存在
                        false ->
                            {false, 3};  %% //3=> 失败-装备不存在
                        {value, UpItem, BagEquipList2}->
                            ItemType = UpItem#item.itemType,
                            EquipList = role_data:get_equip(GerID),
                            if
                                GerID /= 1000 -> %宠物符文
                                    case item_lib:is_itemType_stone(ItemType) orelse item_lib:is_itemType_normal_legend(ItemType) of
                                        false ->
                                            {false, 4}; %% //4=> 失败-不是符文
                                        true ->
                                            %% 条件符合，返回参数，继续执行符文装备
                                            OtherEquipedItemType = [OldItemType||#item{itemType=OldItemType,itemPos=OldPos}<-EquipList
                                                                      ,OldPos =:= ?ITEM_POS_STONE_FIRST
                                                                            orelse OldPos =:= ?ITEM_POS_STONE_SECOND
                                                                            orelse OldPos =:= ?ITEM_POS_STONE_THREE
                                                                      , OldPos =/= ItemPos],
                                            OtherEquipedItemType2 = get_equiped_type_list(OtherEquipedItemType),
                                            IsSame = lists:member(ItemType, OtherEquipedItemType2),
                                            if
                                                IsSame ->
                                                    {false, 6}; %% //6=> 宝物冲突
                                                true ->
                                                    {true, UpItem, BagEquipList2, EquipList}
                                            end
                                    end;
                                GerID == 1000 -> % 训练师能量石
                                    case item_lib:is_itemType_trainer_stone(ItemType) of
                                        false ->
                                            {false, 4}; %% //4=> 失败-不是能量石
                                        true ->
                                            %% 条件符合，返回参数，继续执行能量石装备
                                            OtherEquipedItemType = [OldItemType||#item{itemType=OldItemType,itemPos=OldPos}<-EquipList
                                                                      ,OldPos =:= ?ITEMPOS_TRAINER_STONE_FIRST
                                                                            orelse OldPos =:= ?ITEMPOS_TRAINER_STONE_SECOND
                                                                            orelse OldPos =:= ?ITEMPOS_TRAINER_STONE_THREE
                                                                      , OldPos =/= ItemPos],
                                            IsSame = lists:member(ItemType, OtherEquipedItemType),
                                            if
                                                IsSame ->
                                                    {false, 6}; %% //6=> 和已有能量石冲突
                                                true ->
                                                    {true, UpItem, BagEquipList2, EquipList}
                                            end
                                    end
                            end
                    end
            end
    end.
check_up_essence_stone(GerID, ItemPos, ItemUID, ItemGerID)->
	case lists:keyfind(GerID, #ger.gerID, role_data:get_posListT()) of
        false when GerID /= 1000->
            {false, 7}; %% //7=> 该武将未上阵，或不存在
        _ ->
            if
                ItemGerID == GerID ->
                    {false, 5}; %% //7=> 该符文已经装备在同一个宠物身上
                true ->
                    case take_item(ItemGerID, ItemUID) of %% 检查欲装备物品是否存在
                        false ->
                            {false, 3};  %% //3=> 失败-装备不存在
                        {value, UpItem, BagEquipList2}->
                        	case check_essencestone_up_condition(GerID,ItemPos) of
                        		true->
                            		ItemType = UpItem#item.itemType,
                            		EquipList = role_data:get_equip(GerID),
                            		case item_lib:is_itemType_essence_stone(ItemType) orelse item_lib:is_itemType_essence_legend(ItemType) of
                                		false ->
                                    		{false, 4}; %% //4=> 失败-不是符文
                                		true ->
                                    		%% 条件符合，返回参数，继续执行符文装备
                                            OtherEquipedItemType = [OldItemType||#item{itemType=OldItemType,itemPos=OldPos}<-EquipList
                                                                      ,OldPos =:= ?ITEM_POS_STONE_FOURTH
                                                                            orelse OldPos =:= ?ITEM_POS_STONE_FIFTH
                                                                            orelse OldPos =:= ?ITEM_POS_STONE_SIXTH
                                                                      , OldPos =/= ItemPos],
                                            OtherEquipedItemType2 = get_equiped_type_list(OtherEquipedItemType),
                                            IsSame = lists:member(ItemType, OtherEquipedItemType2),
                                            if
                                                IsSame ->
                                                    {false, 6}; %% //6=> 宝物冲突
                                                true ->
                                    				{true, UpItem, BagEquipList2, EquipList}
                                    		end
                            		end;
                            	false->
                            		{false,8}
                            end
                    end
            end
    end.
check_up_accelerate_equip(GerID,ItemPos,ItemUID,ItemGerID)->
    case lists:keyfind(GerID, #ger.gerID, role_data:get_posListT()) of
        false when GerID /= 1000->
            {false, 7}; %% //7=> 该武将未上阵，或不存在
		_ ->
			if 
                ItemGerID == GerID ->
                    {false, 5};
                true ->
				  	EquipList = role_data:get_equip(GerID),
				    case take_item(ItemGerID, ItemUID) of
					   false ->
						   {false, 3};
					   {value, UpItem, BagEquipList2}->
						   	ItemType = UpItem#item.itemType,
                           	case check_pos(ItemType, ItemPos) of
                               false ->
                                   {false, 2};
                               true when GerID /= 1000-> %如果是宠物的装备
                                    {true, UpItem, BagEquipList2, EquipList};
                               true->
                                    {false, 6}                              
                          	end                   
				    end
			end
	end.	

%% 取装备规则列表,若该装备在玩家的装备规则列表中,则装备动作返回失败,否则进入穿装备的流程
check_treasure_type(ItemType, EquipList) ->
	lists:foldl(fun(E,Acc)->
						check_treasure_rule(E#item.itemTypeID, ItemType) orelse Acc
				end, false, EquipList).

check_treasure_rule(ItemTypeID, ItemTypeID2)->
	RuleList = 
		case data_trea_equip_rule:get(ItemTypeID) of
			?undefined ->
				[];
			X ->
				X
		end,
	lists:member(ItemTypeID2, RuleList).

check_pos(?weapon, Pos) ->
	Pos =:= ?ITEM_POS_WEAPON;
check_pos(?headwear, Pos) ->
	Pos =:= ?ITEM_POS_HEADWEAR;
check_pos(?armor, Pos) ->
	Pos =:= ?ITEM_POS_ARMOR;
check_pos(?wing, Pos) ->
	Pos =:= ?ITEM_POS_WING;
check_pos(?runestone, Pos) ->
	Pos =:= ?ITEM_POS_RUNESTONE;
check_pos(?totem, Pos) ->
	Pos =:= ?ITEM_POS_TOTEM;

check_pos(?trainer_weapon, Pos) ->
    Pos =:= ?ITEMPOS_TRAINER_WEAPON;
check_pos(?trainer_gloves, Pos) ->
    Pos =:= ?ITEMPOS_TRAINER_HEADWEAR;
check_pos(?trainer_armor, Pos) ->
    Pos =:= ?ITEMPOS_TRAINER_ARMOR;
check_pos(?trainer_glasses, Pos) ->
    Pos =:= ?ITEMPOS_TRAINER_WING;
check_pos(?trainer_watch, Pos) ->
    Pos =:= ?ITEMPOS_TRAINER_RUNESTONE;
check_pos(?trainer_boost, Pos) ->
    Pos =:= ?ITEMPOS_TRAINER_TOTEM;
check_pos(?acceleratestone,?ITEM_POS_ACCELERATE_STONE)->
	true;
check_pos(?accelerateslate,?ITEM_POS_ACCELERATE_SLATE)->
	true;
check_pos(_, _) ->
	false.

check_pos_stone(GerID,Pos) ->
    if
        GerID /= 1000
        andalso (Pos =:= ?ITEM_POS_STONE_FIRST
        orelse Pos =:= ?ITEM_POS_STONE_SECOND
        orelse Pos =:= ?ITEM_POS_STONE_THREE) ->
            #role{level=RoleLevel}=role_data:get_roleInfo(),
            NeedLevelList = data_item_stone:get(needLevel),
            case lists:keyfind(Pos-?ITEM_POS_STONE_FIRST+1,1,NeedLevelList) of
                 {_, NeedLevel}->
                     if 
                         NeedLevel =< RoleLevel ->
                             true;
                         true->
                             false
                     end;
                _ ->
                    false
            end;
        true ->
            false
    end.

check_pos_trainer_stone(GerID,Pos) ->
    if
        GerID == 1000
        andalso (Pos =:= ?ITEMPOS_TRAINER_STONE_FIRST
        orelse Pos =:= ?ITEMPOS_TRAINER_STONE_SECOND
        orelse Pos =:= ?ITEMPOS_TRAINER_STONE_THREE) ->
            true;
        true ->
            false
    end.

check_pos_equip(GerID,Pos) ->
    GerID /= 1000
    andalso (Pos =:= ?ITEM_POS_WEAPON
    orelse Pos =:= ?ITEM_POS_HEADWEAR
    orelse Pos =:= ?ITEM_POS_TOTEM
    orelse Pos =:= ?ITEM_POS_ARMOR
    orelse Pos =:= ?ITEM_POS_WING
    orelse Pos =:= ?ITEM_POS_RUNESTONE).

check_pos_trainer_equip(GerID,Pos) ->
    GerID == 1000
    andalso (Pos =:= ?ITEMPOS_TRAINER_WEAPON
    orelse Pos =:= ?ITEMPOS_TRAINER_HEADWEAR
    orelse Pos =:= ?ITEMPOS_TRAINER_ARMOR
    orelse Pos =:= ?ITEMPOS_TRAINER_WING
    orelse Pos =:= ?ITEMPOS_TRAINER_RUNESTONE
    orelse Pos =:= ?ITEMPOS_TRAINER_TOTEM).

check_pos_essence_stone(GerID,Pos)->
	GerID /= 1000 
	andalso (Pos =:= ?ITEM_POS_STONE_FOURTH
	orelse Pos=:=?ITEM_POS_STONE_FIFTH
	orelse Pos=:=?ITEM_POS_STONE_SIXTH).
check_pos_accelerate_equip(GerID,Pos)->
	GerID /= 1000 
	andalso (Pos =:= ?ITEM_POS_ACCELERATE_STONE
	orelse Pos=:=?ITEM_POS_ACCELERATE_SLATE).

get_pos(?weapon)->
	?ITEM_POS_WEAPON;
get_pos(?headwear)->
	?ITEM_POS_HEADWEAR;
get_pos(?armor)->
	?ITEM_POS_ARMOR;
get_pos(?wing)->
	?ITEM_POS_WING;
get_pos(?runestone)->
	?ITEM_POS_RUNESTONE;
get_pos(?totem)->
	?ITEM_POS_TOTEM;

get_pos(?trainer_weapon)->
    ?ITEMPOS_TRAINER_WEAPON;
get_pos(?trainer_gloves)->
    ?ITEMPOS_TRAINER_HEADWEAR;
get_pos(?trainer_armor)->
    ?ITEMPOS_TRAINER_ARMOR;
get_pos(?trainer_glasses)->
    ?ITEMPOS_TRAINER_WING;
get_pos(?trainer_watch)->
    ?ITEMPOS_TRAINER_RUNESTONE;
get_pos(?trainer_boost)->
    ?ITEMPOS_TRAINER_TOTEM;

get_pos(?trainer_stone_ruby)->
    ?ITEMPOS_TRAINER_STONE_FIRST;
get_pos(?trainer_stone_topaz)->
    ?ITEMPOS_TRAINER_STONE_SECOND;
get_pos(?trainer_stone_sapphire)->
    ?ITEMPOS_TRAINER_STONE_THREE;

get_pos(_)->
	0.


do_down_equip(GerID, ItemPos, Item, EquipList2) ->
	Item2 = Item#item{itemPos=0,addAttr=0},
	role_data:set_equip(GerID, EquipList2),
	BagEquip = role_data:get_bagEquip(),
	role_data:set_bagEquip([Item2|BagEquip]),
	Reply = #sc_item_down_equip{gerID=GerID, itemPos=ItemPos, result=1},
    if
        GerID =:= 1000 ->
            ?INFO("trainer equip down, re cacl attr"),
            ger_attr:recacl_gers(); %训练师装备卸下
        true ->
           ger_attr:recacl_f(GerID) 
    end,
	?sendself(Reply).


check_down_equip(GerID, ItemPos) ->
	case role_data:get_equip(GerID) of
		[] ->
			{false, 2};
		[_|_]=EquipList ->
			case lists:keytake(ItemPos, #item.itemPos, EquipList) of
				false ->
					{false, 2};
				{value, Item, EquipList2} ->
					{true, Item, EquipList2}
			end
	end.

do_down_stone(GerID, ItemPos, Item, EquipList2) ->
    Item2 = Item#item{itemPos=0,addAttr=0},
    role_data:set_equip(GerID, EquipList2),
    BagEquip = role_data:get_bagEquip(),
    role_data:set_bagEquip([Item2|BagEquip]),
    Reply = #sc_item_down_equip{gerID=GerID, itemPos=ItemPos, result=1},
    ger_attr:recacl_f(GerID),
    ?sendself(Reply).

check_down_stone(GerID, ItemPos) ->
    case role_data:get_equip(GerID) of
        [_|_]=EquipList ->
            case lists:keytake(ItemPos, #item.itemPos, EquipList) of
                false ->
                    {false, 2};
                {value, Item, EquipList2} ->
                    {true, Item, EquipList2}
            end;
        _ ->
            {false, 2}
    end.

cacl_coin([Item|DelItemList],Acc) ->
	#item{itemLevel=Level,itemRank=Rank, itemNum=Num,itemTypeID=ItemTypeID}=Item,
	case data_item:get(ItemTypeID) of
		#data_item{itemCost=Cost} ->
			Cost2 = Num * trunc(Cost * (1+ Level*0.1) *(1+ Rank*0.1)),
			cacl_coin(DelItemList, Acc+Cost2);
		_ ->
			cacl_coin(DelItemList, Acc)
	end;
cacl_coin([], Acc) ->
	Acc.

itemList2logItemList(ItemList, UpdateItemLogList) ->
	lists:foldl(fun(Item, Acc) ->
						[[Item#item.itemUID,Item#item.itemTypeID,Item#item.itemNum,Item#item.itemNum]|Acc]
				end, UpdateItemLogList, ItemList).

%% do_sell3(DelItemList, BagEquip, BagOther)->
%% 	role_data:set_bagItem(BagOther),
%% 	role_data:set_bagEquip(BagEquip),
%% 	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
%% 	LogItemList = role_item:itemList2logItemList(DelItemList, []),
%% 	{Date, _} = Time = erlang:localtime(),
%% 	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ITEM_SELL, 0, ""),
%% 	{Coin, ItemList} = calc_split(DelItemList),
%% 	role_lib:add_coin_f(Role, Coin, ?MONEY_ADD_TYPE_SELL_ITEM, 0, ""),
%% 	item_lib:add_item_f(ItemList, ?MONEY_ADD_TYPE_SELL_ITEM, 0, ""),
%% 	P_reward_list = [#p_reward_view3{type=1,typeID=TypeID,num=Num}||#new_item{itemTypeID=TypeID, itemNum=Num}<-ItemList],
%% 	?sendself(#sc_item_sell{result=1, reward=[#p_reward_view3{type=1, typeID=21100, num=Coin}]++P_reward_list}).

%% calc_split(DelItemList)->
%% 	lists:foldl(fun(#item{itemTypeID=ItemTypeID, itemRank=ItemRank, itemLevel=ItemLevel}, {CoinAcc, ItemAcc})->
%% 						#data_item{itemCost=SplitID} = data_item:get(ItemTypeID),
%% 						#data_card_split{coin=Coin, itemList=ItemList} = data_card_split:get(SplitID),
%% 						{CoinT, NewItemList} = get_item_split(Coin, ItemList, ItemRank+1, ItemAcc, ItemRank, ItemLevel),
%% 						{CoinAcc + CoinT, NewItemList }
%% 				end, {0, []}, DelItemList).

get_item_split(Coin, ItemList, Times, ItemAcc, ItemRank, ItemLevel)->
	%% v 4.0.0开启了精灵的出售功能，但是只有能量块能（不能升级，进化）出售，故直接返回配置中的coin  
	% CoinT = Coin * (1 + erlang:trunc((ItemRank + ItemLevel - 1) / 10) ),
	CoinT = Coin,
	ItemList2 = get_item_split2(ItemList, Times, ItemAcc),
	{CoinT, ItemList2}.
get_item_split2(_ItemList, 0, A)->
	A;
get_item_split2(ItemList, Times, List)->
	List2 = lists:foldl(fun({ItemTypeID, RandList}, ListAcc)->
								Num = util:random_one_from_weigh_list(RandList),
								if Num =:= 0->
									   ListAcc;
								   true->
									   case lists:keytake(ItemTypeID, #new_item.itemTypeID, ListAcc)of
										   false ->
											   [#new_item{itemTypeID=ItemTypeID, itemNum=Num, itemLevel=1, itemRank=0}|ListAcc];
										   {value, NewItem, ListAcc2}->
											   [NewItem#new_item{itemNum=Num + NewItem#new_item.itemNum}|ListAcc2]
									   end
								end
						end, List, ItemList),
	get_item_split2(ItemList, Times-1, List2).

%%找出出售的道具信息
% cacl_sell(ItemList, SUIDList, DelList) ->
% 	lists:foldl(fun(Item, {SellIDAcc, DelAcc, EquipAcc}) ->
% 						#item{itemUID=ItemUID} = Item,
% 						case lists:member(ItemUID, SellIDAcc) of
% 							true ->
% 								{lists:delete(ItemUID, SellIDAcc), [Item|DelAcc],EquipAcc};
% 							false ->
% 								{SellIDAcc, DelAcc, [Item|EquipAcc]}
% 						end
% 				end, {SUIDList, DelList, []}, ItemList).

check_decompose(ItemUnitList) ->
	% ?INFO("ItemUnitList: ~w ~n ",[ItemUnitList]),
	BagEquip = role_data:get_bagEquip(),
	BagGer = role_data:get_gerBag(),
	{DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList,ResultList} = 
	lists:foldl(fun(ItemUnit,{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,ResultListAcc})->
		case ItemUnit#p_item_decompose_unit.decomposetype of
			?GER_DECOMPOSE ->
				case lists:keyfind(ItemUnit#p_item_decompose_unit.itemUID,#gerSimple.gerID,BagGer) of
					false ->
						?INFO("未发现能够分解的精灵：~w BagGer:~w ~n",[ItemUnit#p_item_decompose_unit.itemUID,BagGer]),
						{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]};
					FindGer ->
						#role{roleID = RoleID} = role_data:get_roleInfo(),
						HomeSteadGerID = homestead_server:get_homestead_ger(RoleID),
						case HomeSteadGerID =/=FindGer#gerSimple.gerID of
							true ->
								case lists:member(FindGer#gerSimple.gerTypeID,?EnergyBlockTypeIDList) of
									true ->
										?INFO("发现分解能量块：~w ~n",[FindGer#gerSimple.gerID]),
										{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]};
									false ->
										case check_item_can_decompose(?GER_DECOMPOSE,FindGer) of
											true->
												{[FindGer|GerListAcc],GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,ResultListAcc};
											false->
												{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]}
										end
								end;
							false->
								?INFO("发现分解家园系统守护兽精灵：~w ~n",[FindGer#gerSimple.gerID]),
								{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]}
						end
				end;
			?GER_EQUIPMENT_DECOMPOSE ->          
            	case lists:keyfind(ItemUnit#p_item_decompose_unit.itemUID,#item.itemUID,BagEquip) of
            		false ->
            			?INFO("未发现能够分解的精灵装备：~w BagEquip:~w ~n",[ItemUnit#p_item_decompose_unit.itemUID,BagEquip]),
            			{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]};
            		FindGerEquip ->
            			case check_item_can_decompose(?GER_EQUIPMENT_DECOMPOSE,FindGerEquip) of
            				true->
            					{GerListAcc,[FindGerEquip|GerEquipListAcc],StoneListAcc,TrainerEquipListAcc,ResultListAcc};
            				false->
            					{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]}
            			end
            	end;
            ?STONE_DECOMPOSE ->                  
            	case lists:keyfind(ItemUnit#p_item_decompose_unit.itemUID,#item.itemUID,BagEquip) of
            		false ->
            			?INFO("未发现能够分解的符文：~w BagEquip:~w ~n",[ItemUnit#p_item_decompose_unit.itemUID,BagEquip]),
            			{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]};
            		FindStone ->
            			case check_item_can_decompose(?STONE_DECOMPOSE,FindStone) of
            				false ->
            					{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]};
            				true ->
            					{GerListAcc,GerEquipListAcc,[FindStone|StoneListAcc],TrainerEquipListAcc,ResultListAcc}
            			end
            	end;
            ?TRAINER_EQUIPMENT_DECOMPOSE ->
            	case lists:keyfind(ItemUnit#p_item_decompose_unit.itemUID,#item.itemUID,BagEquip) of
            		false ->
            			?INFO("未发现能够分解的训练师装备：~w BagEquip:~w ~n",[ItemUnit#p_item_decompose_unit.itemUID,BagEquip]),
            			{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]};
            		FindTrainerEquip ->
            			case check_item_can_decompose(?TRAINER_EQUIPMENT_DECOMPOSE,FindTrainerEquip) of 
            				true->
            					{GerListAcc,GerEquipListAcc,StoneListAcc,[FindTrainerEquip|TrainerEquipListAcc],ResultListAcc};
            				false->
            					{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]}
            			end	
            	end;
            Other ->
            	?INFO("发现未定义的分解方式： ~w ~n",[Other]),
            	{GerListAcc,GerEquipListAcc,StoneListAcc,TrainerEquipListAcc,[false|ResultListAcc]}
        end
    end,{[],[],[],[],[]},ItemUnitList),
	case length(ResultList) of
		0 ->
			{true,DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList};
		_ ->
			{false,DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList}
	end.

%%判断物品是否能够分解
check_item_can_decompose(Type,FindItem)->
	case Type of
		?GER_DECOMPOSE->                    %%精灵分解 
			#data_ger{gerStar=GerStar} = data_ger:get(FindItem#gerSimple.gerTypeID),
			lists:member(GerStar,data_equip_decompose:get({data_decompose_quality,?GER_DECOMPOSE}));
		?GER_EQUIPMENT_DECOMPOSE->          %%精灵装备分解(其中包括加速装备)
			#data_item{itemStar=ItemStar} = data_item:get(FindItem#item.itemTypeID),
			case item_lib:is_accelerate_equip(FindItem#item.itemType) of
				false->
					lists:member(ItemStar,data_equip_decompose:get({data_decompose_quality,?GER_EQUIPMENT_DECOMPOSE}));
				true->
					lists:member(ItemStar,data_equip_decompose:get({data_decompose_quality,?ACCELERATE_EQUIPMENT_DECOMPOSE}))
			end;
		?STONE_DECOMPOSE->                  %%符文分解
			#data_item{itemStar=ItemStar} = data_item:get(FindItem#item.itemTypeID),
			case lists:member(ItemStar,data_equip_decompose:get({data_decompose_quality,?STONE_DECOMPOSE})) of
				true ->
					not lists:member(FindItem#item.itemTypeID,?ExperienceStoneList);
				false->
					false
			end;
		?TRAINER_EQUIPMENT_DECOMPOSE->      %%训练师装备分解
			#data_item{itemStar=ItemStar} = data_item:get(FindItem#item.itemTypeID),
			lists:member(ItemStar,data_equip_decompose:get({data_decompose_quality,?TRAINER_EQUIPMENT_DECOMPOSE}))
	end.


check_sell_item(ItemUIDList)->
	case ItemUIDList of
		[]->
			{false,4};
		_->
			BagItem = role_data:get_bagItem(),
			BagEquip = role_data:get_bagEquip(),
			check_sell_item(ItemUIDList,BagItem,BagEquip,[],[],0)
	end.

check_sell_item([],BagItem,BagEquip,SellItemAcc,SellEquipAcc,SellP)->
	{true,BagItem,BagEquip,SellItemAcc,SellEquipAcc,SellP};
check_sell_item([ItemUID|ItemList],BagItem,BagEquip,SellItemAcc,SellEquipAcc,SellP)->
	case get_item(ItemUID,BagItem,BagEquip) of
		{true,Item,NewBugItem,NewBugEquip}->
			#item{itemType=ItemType,itemTypeID=ItemTypeID,itemNum=ItemNum} = Item,
			SellPrice = ((data_item:get(ItemTypeID))#data_item.itemCost) * ItemNum,
			case SellPrice > 0 of
				true->
					case item_lib:is_itemType_equip(ItemType) of
						true->
							check_sell_item(ItemList,NewBugItem,NewBugEquip,SellItemAcc,[Item|SellEquipAcc],SellP+SellPrice);
						false->
							check_sell_item(ItemList,NewBugItem,NewBugEquip,[Item|SellItemAcc],SellEquipAcc,SellP+SellPrice)
					end;
				false->
					{false,3}
			end;
		{false,Reason}->
			{false,Reason}
	end.

get_item(ItemUID,BagItem,BagEquip)->
	case lists:keytake(ItemUID, #item.itemUID, BagItem) of
		false ->
			case lists:keytake(ItemUID, #item.itemUID, BagEquip) of
				false ->
					{false,2};
				{value, Item, BagEquip2}->
					{true,Item,BagItem,BagEquip2}
			end;
		{value, Item, BagItem2}->
			{true,Item,BagItem2,BagEquip}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
patch2p_item(TypeID, [Patch]) ->
	{_,Num} = role_data:calc_patch(TypeID, 0, Patch),
	if Num =:= 0 ->
		   [];
	   true->
		   #p_item{itemUID=TypeID,itemLevel=0,itemNum=Num,itemRank=0,itemTypeID=TypeID,itemDecay=0,itemExp=0}
	end.


all_patch_typeID()->
	[21201,21202,21203,21204,21205,21211,21212,21213,21214,21215,21216,21217,21218,21219,21220,21231,21232,21233,21234,21235,
	 21241,21242,21243,21244,21245,21251,21252,21253,21254,21255,21261,21262,21263,21264,21265,21266,21267,21268,21269,21270].

best_equip() ->
	Equip = [11010,12004, 13004],
	Treasure = [14002, 14003,14004,14005,14006,14010,14012,14014],
	List =
		[hd(item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_MAIN_EQUIP,itemTypeID=E}, data_item:get(E)))||E<-Equip] ++
			[hd(item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_TREASURE,itemTypeID=E}, data_item:get(E)))||E<-Treasure],
	{_,L}=lists:foldl(fun(E,{Pos, Acc}) ->
							  {Pos+1, [E#item{itemPos=Pos}|Acc]}
					  end, {1, []}, List),
	L.

refresh_item(Item) ->
	#item{itemPos=ItemPos} = Item,
	if ItemPos > 0 ->
		   item_lib:recacl(Item);
	   true ->
		   Item
	end.

sc_item_auto_up_equip(Msg) ->
    ?sendself(Msg).

cs_item_auto_up_equip(#cs_item_auto_up_equip{gerID=GerID,type=AutoType}) ->
    if
        AutoType=:=1 orelse AutoType=:=2 -> %% 装备自动装备或符文
            case catch check_can_auto_equip_up(GerID,AutoType) of
                {ok, RoleID, AutoList} ->
                    lists:foreach(fun({ItemUID, ItemPos}) ->
                                          ?INFO("auto equip ItemUID:~w ItemPos:~w ",[ItemUID, ItemPos]),
                                          role_lib:send_server(RoleID, {route, role_item, #cs_item_up_equip{gerID=GerID,itemPos=ItemPos, itemUID=ItemUID, itemGerID=0}})
                                  end, AutoList),
                    role_lib:send_server(RoleID, {route, role_item, #sc_item_auto_up_equip{result=0}});
                {false, Reason} ->
                    ?sendself(#sc_item_auto_up_equip{result=Reason})
            end;
        true ->
            ?sendself(#sc_item_auto_up_equip{result=4})
    end.

%% 检查并生成需要自动装备的装备列表
check_can_auto_equip_up(1000,AutoType) ->
    GerEquipList = role_data:get_equip(1000),
    AutoList = case AutoType of
        1 -> %% 训练师装备
            GerEquipListNoStone = [E||E<-GerEquipList,E#item.itemPos>=121,E#item.itemPos=<126],
            NeedEquipTypeList = ?TRAINER_EQUIP_TYPE_NO_STONE,
            gen_auto_equip_list(NeedEquipTypeList,[],GerEquipListNoStone);
        2 -> %% 符文
        	%% 训练师的符文宝石依然能够使用准备相同的逻辑，主要是对应的装备有单独唯一的穿戴位置
        	GerEquipListNoStone = [E||E<-GerEquipList,E#item.itemPos>=131,E#item.itemPos=<133],
        	NeedEquipTypeList = ?TRAINER_STONE_TYPE_LIST,
        	gen_auto_equip_list(NeedEquipTypeList,[],GerEquipListNoStone)
    end,
    ?INFO("check_can_auto_equip_up ~w ~w 即将装备：~w",[1000,AutoType,AutoList]),
    case AutoList of
        [] ->
            {false, 3};
        _ ->
            {ok, role_data:get_roleID(), AutoList}
    end;
check_can_auto_equip_up(GerID,AutoType) ->
    Ger = role_data:get_ger(GerID),
    case Ger of
        false ->
			GerTypeID=0,
            erlang:throw({false, 1});
        {_,_,_,_,_,bag} ->
			GerTypeID=0,
            erlang:throw({false, 1});
        {_, #ger{gerBase=#gerBase{gerTypeID=GerTypeID}}, _, _, _, _} ->
            next
    end,
    GerEquipList = role_data:get_equip(GerID),
    AutoList = case AutoType of
        1 -> %% 装备
            NeedEquipTypeList = ?EQUIP_TYPE_LIST_NO_STONE,% -- EquipedTypeListNoStone,
			DestinyEquipList = ger_lib:get_destiny_equip(GerTypeID),
            gen_auto_equip_list(NeedEquipTypeList,DestinyEquipList,GerEquipList);
        2 -> %% 符文
            GerEquipListNoStone = [E||E<-GerEquipList,E#item.itemPos>=11,E#item.itemPos=<17],
            ?INFO("当前的装备列表 cur equip list ~w",[GerEquipListNoStone]),
            case erlang:length(GerEquipListNoStone) < 6 of
                true ->
                    next;
                false ->
                    erlang:throw({false, 2})
            end,           
			EquipedPosListNoStone0 = [?ITEM_POS_STONE_FIRST,?ITEM_POS_STONE_SECOND,?ITEM_POS_STONE_THREE,?ITEM_POS_STONE_FOURTH,?ITEM_POS_STONE_FIFTH,?ITEM_POS_STONE_SIXTH] -- [ItemPos||#item{itemPos=ItemPos}<-GerEquipListNoStone],
            EquipedPosListNoStone = [E||E<-EquipedPosListNoStone0,check_pos_stone(GerID,E) orelse check_pos_essence_stone(GerID,E)],
            case EquipedPosListNoStone of
                [_|_]->
                    ignore;
                []->
                    erlang:throw({false, 2})
            end,            
            AlreadyEquipedTypeList = get_equiped_type_list([ItemType||#item{itemType=ItemType}<-GerEquipListNoStone]),
            EquipedTypeListNoStone = (?STONE_TYPE_LIST++?ESSENCE_STONE_TYPE++?STONE_LEGEND_LIST) -- AlreadyEquipedTypeList,
            ?INFO("check_can_auto_equip_up Stone ~w ~w ~w ",[EquipedTypeListNoStone,EquipedPosListNoStone,EquipedPosListNoStone0]),            
            
            AutoStoneList = lists:foldl(fun(StoneType,Acc)->
                                OneTypeList = [E||E<-role_data:get_bagEquip(),E#item.itemType =:=StoneType],
                                case OneTypeList of
                                    [_|_] ->
                                        [get_auto_stone(OneTypeList)|Acc];
                                    []->
                                        Acc
                                end
                        end, [], EquipedTypeListNoStone),
            
            AutoStoneList2 = lists:sort(fun(A,B)->
                          #item{itemTypeID=ItemTypeID1,itemLevel=ItemLevel1,itemRank=ItemRank1} = A,
                          #item{itemTypeID=ItemTypeID2,itemLevel=ItemLevel2,itemRank=ItemRank2} = B,
                          #data_item{itemStar=ItemStar1} = data_item:get(ItemTypeID1),
                          #data_item{itemStar=ItemStar2} = data_item:get(ItemTypeID2),
                          if 
                              ItemStar1 > ItemStar2 ->
                                  true;
                              ItemStar1 < ItemStar2 ->
                                  false;
                              ItemRank1 > ItemRank2 ->
                              	  true;
                              ItemRank1 < ItemRank2 ->
                              	  false;
                              ItemLevel1 >= ItemLevel2 ->
                                  true;
                              true ->
                                  false
                          end
            end, AutoStoneList),
            match_pos_stone(AutoStoneList2,lists:sort(EquipedPosListNoStone))
    end,
    ?INFO("check_can_auto_equip_up ~w ~w 即将装备：~w",[GerID,AutoType,AutoList]),
    case AutoList of
        [] ->
            {false, 3};
        _ ->
            {ok, role_data:get_roleID(), AutoList}
    end.

get_equiped_type_list(TypeList) -> get_equiped_type_list(TypeList,[]).
get_equiped_type_list([],A) -> A;
get_equiped_type_list([H|TypeList],A) ->
    [{C,D}] = [X||{E,F}=X<-data_item_stone:get(sameTypeList),E==H orelse F == H],
    get_equiped_type_list(TypeList,[C,D|A]).
    

%%StoneList按照星等排序，PosList升序
match_pos_stone(StoneList,PosList)->
    {MatchResult,RemainResult,RemainPosAcc} = 
        lists:foldl(fun(Pos,{MatchAcc,RemainStoneAcc,RemainPosAcc})->
                            M = specail_match(Pos,RemainStoneAcc),
                            case M of
                                {true,Stone}->
                                    {[{Stone#item.itemUID,Pos}|MatchAcc],RemainStoneAcc--[Stone],RemainPosAcc};
                                false->
                                    {MatchAcc,RemainStoneAcc,[Pos|RemainPosAcc]}
                            end
                    end,{[],StoneList,[]},PosList),
    {Match2,_RemainPos2}=
        lists:foldl(fun(Pos,{Acc,RAcc})->
                            case RAcc of
                                []->
                                    {Acc,RAcc};
                                [H|T] ->
                                    {[{H#item.itemUID,Pos}|Acc],T}
                            end
                    end,{[],lists:sort([E||E<-RemainResult,false =:= item_lib:is_itemType_essence_stone(E#item.itemType)])},lists:sort(RemainPosAcc)),
    MatchResult++Match2.

specail_match(_Pos,[])->
	false;
specail_match(Pos,StoneList) when is_list(StoneList)->
	case data_item_stone:get(Pos) of
		?undefined->
			false;
		TypeList ->
			get_most_suit_stone(TypeList,StoneList)
	end;
specail_match(_Pos,_StoneList)->
	false.

get_most_suit_stone(TypeList,StoneList)->
	SuitStoneList = lists:foldl(fun(#item{itemType=ItemType}=E,Acc)->
		case lists:member(ItemType,TypeList) of
			true->
				[E|Acc];
			false->
				Acc
		end 
	end,[],StoneList),
	case SuitStoneList of
		[]->
			false;
		_ ->
    		SuitStoneList2 = lists:sort(fun(A,B)->
                          #item{itemTypeID=ItemTypeID1,itemLevel=ItemLevel1,itemRank=ItemRank1} = A,
                          #item{itemTypeID=ItemTypeID2,itemLevel=ItemLevel2,itemRank=ItemRank2} = B,
                          #data_item{itemStar=ItemStar1} = data_item:get(ItemTypeID1),
                          #data_item{itemStar=ItemStar2} = data_item:get(ItemTypeID2),
                          if 
                              ItemStar1 > ItemStar2 ->
                                  true;
                              ItemStar1 < ItemStar2 ->
                                  false;
                              ItemRank1 > ItemRank2 ->
                              	  true;
                              ItemRank1 < ItemRank2 ->
                              	  false;
                              ItemLevel1 >= ItemLevel2 ->
                                  true;
                              true ->
                                  false
                          end
            end, SuitStoneList),
    		{true,hd(SuitStoneList2)}
    end.
gen_auto_equip_list(NeedEquipTypeList) ->
	gen_auto_equip_list(NeedEquipTypeList,[],[]).
gen_auto_equip_list(NeedEquipTypeList,DestinyEquipList,GerEquipList) ->
    gen_auto_equip_list(NeedEquipTypeList, [],DestinyEquipList,GerEquipList).

gen_auto_equip_list([], AutoList,_,_) ->
    AutoList;
gen_auto_equip_list([NeedEquipType|NeedEquipTypeList], AutoList,DestinyEquipList,GerEquipList) ->
    case find_auto_equip(NeedEquipType,DestinyEquipList,GerEquipList) of
        {ok, ItemUID, ItemPos} ->
            gen_auto_equip_list(NeedEquipTypeList, [{ItemUID, ItemPos}|AutoList],DestinyEquipList,GerEquipList);
        false ->
            gen_auto_equip_list(NeedEquipTypeList, AutoList,DestinyEquipList,GerEquipList)
    end.
find_auto_equip(NeedEquipType,DestinyEquipList,GerEquipList) ->
    BagEquipList = role_data:get_bagEquip(),
    NeedTypeBagEquipList =
        lists:filter(fun(#item{itemType=ItemType}) ->
                             ItemType =:= NeedEquipType
                     end, BagEquipList),
    find_auto_equip2(NeedTypeBagEquipList,DestinyEquipList,[E||E<-GerEquipList,E#item.itemType == NeedEquipType]).

find_auto_equip2([],_,_) ->
    false;
find_auto_equip2(EquipList,DestinyEquipList,[Equip=#item{itemUID=EquipItemUID,itemType=EquipItemType}]=Equiped)->
	[#item{itemUID=ItemUID,itemType=ItemType}|_] = sort_for_auto_nostone(Equiped++EquipList,DestinyEquipList),
	case ItemUID =:= EquipItemUID of
		true->
			%%说明选中的新的装备和已经穿戴的装备一致，不用更换，减少不必要的装备穿戴判断
			false;
		_->
			{ok, ItemUID, get_pos(ItemType)}
	end;
find_auto_equip2(EquipList,DestinyEquipList,Equiped) ->
    [#item{itemUID=ItemUID, itemType=ItemType}|_] = sort_for_auto_nostone(Equiped++EquipList,DestinyEquipList),
    {ok, ItemUID, get_pos(ItemType)}.

get_all_equip_num(AllEquipDataList, ItemTypeID) ->
    case lists:keyfind(ItemTypeID, 1, AllEquipDataList) of
        false ->
            0;
        {ItemTypeID, Num} ->
            Num
    end.

sort_for_auto_nostone(EquipList,DestinyEquipList) ->
    AllEquipDataList = [],%gen_all_equip_data(),
    lists:sort(
      fun(#item{itemTypeID=ItemTypeID1,itemLevel=ItemLevel1,itemRank=ItemRank1},
          #item{itemTypeID=ItemTypeID2,itemLevel=ItemLevel2,itemRank=ItemRank2}) ->
              #data_item{itemStar=ItemStar1} = data_item:get(ItemTypeID1),
              #data_item{itemStar=ItemStar2} = data_item:get(ItemTypeID2),
			  case check_destiny(ItemTypeID1,ItemTypeID2,DestinyEquipList) of
				  continue ->
					  if 
						  ItemStar1 > ItemStar2 ->
							  true;
						  ItemStar1 =:= ItemStar2 ->
							  Num1 = get_all_equip_num(AllEquipDataList, ItemTypeID1),
							  Num2 = get_all_equip_num(AllEquipDataList, ItemTypeID2),
							  if
								  Num1 > Num2 ->
									  true;
								  Num1 =:= Num2 ->
									  if
										  ItemRank1 > ItemRank2 ->
											  true;
										  ItemRank1 =:= ItemRank2 ->
											  if
												  ItemLevel1 > ItemLevel2 ->
													  true;
												  ItemLevel1 =:= ItemLevel2 ->
													  true;
												  true ->
													  false
											  end;
										  true ->
											  false
									  end;
								  true ->
									  false
							  end;
						  true ->
							  false
					  end;
				  Res ->
					  Res
			  end
	  end, EquipList).

check_destiny(ItemTypeID1,ItemTypeID2,DestinyEquipList)->
	IsID1 = lists:member(ItemTypeID1, DestinyEquipList),
	IsID2 = lists:member(ItemTypeID2, DestinyEquipList),
	if IsID1 ->
		   if IsID2 ->
				  continue;
			  true ->
				  true
		   end;
	   true ->
		   if IsID2 ->
				  false;
			  true ->
				  continue
		   end
	end.

get_auto_stone([H|EquipList]) ->
    lists:foldl(
      fun(New,Old) ->
              #item{itemTypeID=ItemTypeID1,itemLevel=ItemLevel1,itemRank = ItemRank1} = New,
              #item{itemTypeID=ItemTypeID2,itemLevel=ItemLevel2,itemRank = ItemRank2} = Old,
              #data_item{itemStar=ItemStar1} = data_item:get(ItemTypeID1),
              #data_item{itemStar=ItemStar2} = data_item:get(ItemTypeID2),
              if 
                  ItemStar1 > ItemStar2 ->
                      New;
                  ItemStar1 < ItemStar2 ->
                      Old;
                  ItemStar1 == ItemStar2 ->
                      if ItemTypeID1 >= 5059 andalso ItemTypeID2 =< 5066 andalso ItemTypeID2 =< 5059 -> New;
                         ItemTypeID1 =< 5059 andalso ItemTypeID2 >= 5059 andalso ItemTypeID2 =< 5066 -> Old;
                         ItemLevel1 > ItemLevel2 ->
                             New;
                         ItemLevel1 < ItemLevel2 ->
                             Old;
                         ItemRank1 > ItemRank2 ->
                             New;
                         ItemRank1 < ItemRank2 ->
                             Old;
                         true ->
                             Old
                      end
              end
      end, H,EquipList).

gen_all_equip_data() ->
    lists:foldr(fun(AllEquipID, Acc) ->
                        #data_all_equipment{all_equipment_part_list=PartList} = data_all_equipment:get(AllEquipID),
                        PartNum = erlang:length(PartList),
                        [{Part, PartNum}||Part<-PartList] ++ Acc
                end, [], data_all_equipment:get_list()).

cs_item_stone_eat(#cs_item_stone_eat{stoneID=StoneID,gerID=GerID,foodStoneIDList=FoodStoneIDList}) ->
    case check_stone_eat(StoneID,GerID,FoodStoneIDList) of
        {ok, StoneInfo, RestEquipList,FoodStoneInfoList,RestBagEquips} ->
            NewStoneInfo = do_stone_eat(StoneInfo, RestEquipList,FoodStoneInfoList,RestBagEquips,GerID,?MONEY_DEC_TYPE_TREASURE_EAT,?STONE_EAT),
            ?sendself(#sc_item_stone_eat{result=1,stone_info=item_lib:item2p_item(NewStoneInfo),itemGerID=GerID,itemPos=NewStoneInfo#item.itemPos}),
            ?CATCH(role_task_trigger:handle({dispach_task, eat_stone}));
        {fail, Reason} ->
            ?sendself(#sc_item_stone_eat{result=Reason
                                   ,stone_info=#p_item{itemUID=0,itemLevel=0,itemNum=0,itemRank=0,itemTypeID=0,itemDecay=0,itemExp=0
                                                      ,itemenchantType=0,itemenchantLevel=0,itemLegendRank=0}
                                   ,itemGerID=GerID,itemPos=0})
    end.
check_stone_eat(StoneID,GerID,FoodStoneIDList)->
    if GerID =:= 0 ->
           EquipList = role_data:get_bagEquip();
       true ->
           EquipList = role_data:get_equip(GerID)
    end,
    IsDuplicate = lists:member(StoneID, FoodStoneIDList),
    if
        IsDuplicate ->
            {fail, 2};
        FoodStoneIDList == [] orelse length(FoodStoneIDList) > 5 ->
            {fail, 3};
        true ->
            ?INFO("L-check_stone_eat ~w ~w",[StoneID,EquipList]),
            case lists:keytake(StoneID, #item.itemUID, EquipList) of
                false ->
                    {fail, 5};
                %% 注意item经验的上限是65535，因为itemExp是2字节的
                {value, #item{itemLevel=ItemLevel}, _} when ItemLevel >= 'data_stone_exp:get(level_max)'->
                    {fail, 6};
                {value, StoneInfo, RestEquipList} ->
                    #item{itemType=ItemType} = StoneInfo,
                    case item_lib:is_itemType_stone(ItemType) orelse item_lib:is_itemType_essence_stone(ItemType) 
                        orelse item_lib:is_itemType_legend(ItemType) of
                        %% 下面是分离被吞噬的符文和背包中的其他物品。特别注意，还需要剔除被升级的装备。升级后的装备如果原本是背包中，还会被合并回去。
                        true when GerID =:= 0-> %% 即将升级的符文在背包中RestEquipList已经剔除了被升级的符文
                            case check_stone_eat_foodStoneIDList(FoodStoneIDList,RestEquipList) of
                                {true,FoodStoneInfoList,RestBagEquips} ->
                                    {ok,StoneInfo, RestEquipList,FoodStoneInfoList,RestBagEquips};
                                {false,[],[]} ->
                                    {fail, 4}
                            end;
                        true when GerID /= 0-> %% 即将被升级的符文在装备中，
                            case check_stone_eat_foodStoneIDList(FoodStoneIDList,role_data:get_bagEquip()) of
                                {true,FoodStoneInfoList,RestBagEquips} ->
                                    {ok,StoneInfo, RestEquipList,FoodStoneInfoList,RestBagEquips};
                                {false,[],[]} ->
                                    {fail, 4}
                            end;
                        _ ->
                            ?INFO("不能升级经验符文"),
                            {fail, 8}
                    end
            end
    end.

%% 返回的格式为{Result，FoodStoneInfoList，RestBagEquips}
%% FoodStoneInfoList为即将吞噬的item详细信息，RestBagEquips为剩余的背包信息
check_stone_eat_foodStoneIDList(FoodStoneIDList,RestEquipList)->
    lists:foldl(fun %% FoodStoneIDList传递进来的是即将吞噬 的ID的List，FoodStoneInfoList收集返回的是吞噬的详细信息
                    (FoodStoneID,{Result,FoodStoneInfoList,RestBagEquips}) when Result =:= true andalso RestBagEquips =/= [] ->
                        %% 检查一下背包中剩余的东西中是否有即将被吞噬的东西
                        case lists:keytake(FoodStoneID,#item.itemUID,RestBagEquips) of
                            {value, Tuple, TupleList2}->
                            	case item_lib:is_itemType_stone(Tuple#item.itemType,true) orelse item_lib:is_itemType_essence_stone(Tuple#item.itemType) of 
                            		true->
                                		{true,[Tuple|FoodStoneInfoList],TupleList2};
                                	false->
                                		{false,[],[]}
                                end;
                            false ->
                                {false,[],[]}
                        end;
                    %% 之前的遍历返回false或者背包已经空了，都算是失败，继续返回false让foldl循环结束
                    (_,_) ->
                        {false,[],[]}
                end, {true,[],RestEquipList}, FoodStoneIDList).

do_stone_eat(StoneInfo, RestEquipList,FoodStoneInfoList,RestBagEquips,GerID,ConsumeType,Type)->
    #item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemExp=OldItemExp,itemLevel=OldItemLevel,itemType=OldItemType} = StoneInfo,
    AddExp = inherit_exp(FoodStoneInfoList,OldItemType,Type),
    NewExp =  lists:min([OldItemExp+AddExp,65535]),
    #data_item{itemStar=ItemStar} = _ItemConfig = data_item:get(ItemTypeID),
    ExpLevelConfig = 
        case item_lib:is_itemType_legend(OldItemType) of
            false ->
                case item_lib:is_itemType_essence_stone(OldItemType) of
                    false->          data_stone_exp:get(ItemStar);
                    true->           data_essence_stone_exp:get(ItemStar)
                end;
            true ->
                case item_lib:is_itemType_essence_legend(OldItemType) of
                    false -> data_stone_exp:get(normal_legend_stone);
                    true -> data_stone_exp:get(essence_legend_stone)
                end
        end,
    {NewLevel,NewExp2} = lists:foldl(fun({Exp,Level},Acc)->
                                             if 
                                                 Exp =< NewExp andalso Level =:= 10 ->  %% 如果等级上限变化，这里需要更新一下
                                                     {Level,Exp};
                                                 Exp =< NewExp ->  
                                                     {Level,NewExp};
                                                 true ->
                                             Acc
                                     end
                             end, {0,0}, ExpLevelConfig),
    NewStoneInfo = refresh_item(StoneInfo#item{itemLevel=NewLevel,itemExp=NewExp2}),
    if GerID =:= 0 ->
            NewBagEquips = [NewStoneInfo|RestBagEquips],
           role_data:set_bagEquip(NewBagEquips);
       true ->
            NewEquipList = [NewStoneInfo|RestEquipList],
            role_data:set_equip(GerID, NewEquipList),
            role_data:set_bagEquip(RestBagEquips),
            ger_attr:recacl_f(GerID)
    end,
    %% 写道具日志
    {Date, _} = Time = erlang:localtime(),
    case NewLevel >= data_stone_exp:get(level_max) of
        true ->
            ?CATCH(role_task_trigger:handle({dispach_task, stone_max_level, ItemStar}));
        _ ->
            ignore
    end,
	LogItemList = role_item:itemList2logItemList(FoodStoneInfoList, []),
	RoleID = role_data:get_roleID(),
    behavior_item_uplevel:log(RoleID, ItemUID, ItemTypeID, lists:max([NewLevel-OldItemLevel,0]), NewLevel, 1, 0, Date, Time),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ConsumeType, ItemTypeID, integer_to_list(ItemUID)),

    NewStoneInfo.
%%     ?CATCH(role_task_trigger:handle({dispach_task,equip_strong,RoleID,ItemUID,ItemTypeID,NewLevel,1})).

%%新的计算分解的box
new_decompose_get_boxlist(DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList)->
	#decompose_box_info{boxlist=GerBoxList,gold=GerGold,coin=GerCoin,reputation=GerReputation} = new_get_box_list(DecomposeGerList),
	#decompose_box_info{boxlist=GerEquipBoxList,gold=GerEquipGold,coin=GerEquipCoin,reputation=GerEquipReputation} = new_get_box_list(DecomposeGerEquipList),
	#decompose_box_info{boxlist=StoneBoxList,gold=StoneGold,coin=StoneCoin,reputation=StoneReputation} = new_get_box_list(DecomposeStoneList),
	#decompose_box_info{boxlist=TrainerBoxList,gold=TrainerGold,coin=TrainerCoin,reputation=TrainerReputation} = new_get_box_list(DecomposeTrainerEquipeList),
	LastBoxList = GerBoxList++GerEquipBoxList++StoneBoxList++TrainerBoxList,
	LastGold = GerGold+GerEquipGold+StoneGold+TrainerGold,
	LastCoin = GerCoin+GerEquipCoin+StoneCoin+TrainerCoin,
	LastReputation=GerReputation+GerEquipReputation+StoneReputation+TrainerReputation,
	#decompose_box_info{boxlist=LastBoxList,gold=LastGold,coin=LastCoin,reputation=LastReputation}.

new_get_box_list([])->
	#decompose_box_info{};
new_get_box_list(L)->
	new_get_box_list(L,#decompose_box_info{}).

new_get_box_list([],Acc)->
	Acc;
new_get_box_list([H|T],#decompose_box_info{boxlist=OldBoxList,gold=OldGold,coin=OldCoin,reputation=OldReputation})->
	#decompose_box_info{boxlist=BoxList,gold=Gold,coin=Coin,reputation=Reputation} =  new_get_single_box_list(H),
	new_get_box_list(T,#decompose_box_info{boxlist=BoxList++OldBoxList,gold=OldGold+Gold,coin=OldCoin+Coin,reputation=OldReputation+Reputation}).

new_get_single_box_list(Ger) when is_record(Ger,gerSimple)->
	#gerSimple{gerQuality=Quality,gerLevel=_Level,gerTypeID=GerTypeID,gerExp=GerExp}=Ger,
	#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
	BaseBoxList = data_equip_decompose:get({decompose_ger,GerStar}),
	GetBoxList = [{BoxID,Num+Quality}||{BoxID,Num}<-BaseBoxList],
	{GetGold,GetCoin,GetReputation} = get_decompose_extra(?GER_DECOMPOSE,GerExp),
	#decompose_box_info{boxlist=GetBoxList,gold=GetGold,coin=GetCoin,reputation=GetReputation};
new_get_single_box_list(Item) when is_record(Item,item)->
	case lists:member(Item#item.itemType,?EQUIP_TYPE_LIST_NO_STONE) of
		true->
			new_get_single_box_list_for_ger_equip(Item);
		false->
			case lists:member(Item#item.itemType,?STONE_TYPE) of
				true->
					new_get_single_box_list_for_ger_stone(Item);
				false->
					case lists:member(Item#item.itemType,?TRAINER_EQUIP_TYPE) of
						true->
							new_get_single_box_list_for_trainer_equip(Item);
						false->
							case item_lib:is_accelerate_equip(Item#item.itemType) of
								true->
									new_get_single_box_list_for_accelerate_equip(Item);
								false->
									?ERR("undefined item decompose type:~w~n",[Item#item.itemType]),
									#decompose_box_info{}
							end
					end
			end
	end.

new_get_single_box_list_for_ger_equip(Item)->
	#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank} = Item,
	#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
	BaseBoxList = data_equip_decompose:get({decompose_ger_equip,ItemStar}),
	GetBoxList = [{BoxID,Num+Rank}||{BoxID,Num}<-BaseBoxList],
	{GetGold,GetCoin,GetReputation} = get_decompose_extra(?GER_EQUIPMENT_DECOMPOSE,Level),
	#decompose_box_info{boxlist=GetBoxList,gold=GetGold,coin=GetCoin,reputation=GetReputation}.
new_get_single_box_list_for_ger_stone(Item)->
	#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank,itemExp=ItemExp} = Item,
	#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
	BaseBoxList1 = data_equip_decompose:get({decompose_ger_stone,ItemStar}),
	BaseBoxList = [{BaseBoxID,Num*(Rank+1)}||{BaseBoxID,Num}<-BaseBoxList1],
	ExpBoxList = get_stone_expirence_box(ItemStar,Level,ItemExp),
	#decompose_box_info{boxlist=BaseBoxList++ExpBoxList}.
new_get_single_box_list_for_trainer_equip(Item)->
	#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank} = Item,
	#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
	BaseBoxList = data_equip_decompose:get({decompose_trainer_equipment,ItemType,ItemStar}),
	GetBoxList = [{BoxID,Num*(1+Rank)}||{BoxID,Num}<-BaseBoxList],
	%%此处对传入get_decompose_extra的参数做扩展，方便对训练师装备分解做特殊处理
	{GetGold,GetCoin,GetReputation} = get_decompose_extra(?TRAINER_EQUIPMENT_DECOMPOSE,{ItemType,Level,ItemStar}),
	#decompose_box_info{boxlist=GetBoxList,gold=GetGold,coin=GetCoin,reputation=GetReputation}.
new_get_single_box_list_for_accelerate_equip(Item)->
	#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=Rank} = Item,
	#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
	BaseBoxList = data_equip_decompose:get({decompose_accelerate_equipment,ItemType,ItemStar}),
	GetBoxList = [{BoxID,Num*(1+Rank)}||{BoxID,Num}<-BaseBoxList],
	{GetGold,GetCoin,GetReputation} = get_decompose_extra(?ACCELERATE_EQUIPMENT_DECOMPOSE,{ItemType,ItemLevel,ItemStar}),
	#decompose_box_info{boxlist=GetBoxList,gold=GetGold,coin=GetCoin,reputation=GetReputation}.

%%v4.0增加，对精灵品阶返回对应的升阶材料(不返还转生材料)（mega返回中有调用该接口，故不能直接在这个接口中添加返回比例修改）
calculate_ger_quality_decompose_obtain(_GerStar,0,Acc,_IsLight,_GerTypeID)->
	Acc;
calculate_ger_quality_decompose_obtain(GerStar,Quality,Acc,false,GerTypeID)->
	Type = case role_ger:is_specail_ger(GerTypeID) of
	        true->
	            2;
	        false->
	            case role_awake:check_ger_mega(GerTypeID) of
	                true->
	                    3;
	                false->
	                    1;
	                ?undefined->
	                	1
	            end
	end,
	case data_ger_up_rank:get({GerStar,Quality,Type}) of
		?undefined->
			Acc;
		{Need,false}->
			calculate_ger_quality_decompose_obtain(GerStar,Quality-1,role_reward:reward_plus_reward(Need,Acc),false,GerTypeID);
		{Need,true}->
			calculate_ger_quality_decompose_obtain(GerStar,Quality-1,Acc,false,GerTypeID)
	end;
calculate_ger_quality_decompose_obtain(GerStar,Quality,Acc,true,GerTypeID)->
	Type = case role_ger:is_specail_ger(GerTypeID) of
	        true->
	            2;
	        false->
	            case role_awake:check_ger_mega(GerTypeID) of
	                true->
	                    3;
	                false->
	                    1;
	                ?undefined->
	                	1
	            end
	end,
	case data_ger_up_rank_light:get({GerStar,Quality,Type}) of
		?undefined->
			Acc;
		{Need,false}->
			calculate_ger_quality_decompose_obtain(GerStar,Quality-1,role_reward:reward_plus_reward(Need,Acc),true,GerTypeID);
		{Need,true}->
			calculate_ger_quality_decompose_obtain(GerStar,Quality-1,Acc,true,GerTypeID)
	end.

%%返还精灵经验应该获得fruit
calculate_ger_exp_decompose_obtain(GerExp)->
	LoseRate = data_equip_decompose:get(ger_exp_lose_rate),
	role_recycle:transform_gerexp2energyblock(trunc(GerExp*LoseRate/10000)).

get_decompose_extra(Type,Value)->
	?INFO("Type:~w Value:~w ~n",[Type,Value]),
	Result = case Type of
		?GER_DECOMPOSE->
			{0,0,trunc(Value/10000*3*3)};
		?GER_EQUIPMENT_DECOMPOSE->
			UseCoin = data_reinforce:get({base_coin,Value}),
			{0,0,trunc(UseCoin*0.17/10000*3*2)};
		?STONE_DECOMPOSE->
			{0,0,0};
		?TRAINER_EQUIPMENT_DECOMPOSE->
			{ItemType,ItemLevel,ItemStar} = Value,
			IncStep =get_trainer_equip_level_increase_step(ItemLevel),
    		UseReputation = calculate_equip_level_return(ItemType,ItemLevel-1,1,ItemStar,0,IncStep,?TRAINER_EQUIPMENT_DECOMPOSE),
			{0,0,trunc(UseReputation*0.25)};
		?ACCELERATE_EQUIPMENT_DECOMPOSE->
			{ItemType,ItemLevel,ItemStar} = Value,
			IncStep =get_trainer_equip_level_increase_step(ItemLevel),
    		UseReputation = calculate_equip_level_return(ItemType,ItemLevel-1,1,ItemStar,0,IncStep,?ACCELERATE_EQUIPMENT_DECOMPOSE),
			{0,0,trunc(UseReputation*0.25)}
	end,
	?INFO("Result:~w ~n ",[Result]),
	Result.

get_trainer_equip_level_increase_step(ItemLevel)->
    List = data_recycle:get(data_trainer_equip_level_up),
    {_Field,Step} = util:fun_find(fun({{Begin,End},_Step})->
        ItemLevel >=Begin andalso ItemLevel =< End
    end,List),
    Step.

calculate_equip_level_return(ItemType,ItemMaxLevel,CurrentLevel,ItemStar,Acc,IncStep,Type)->
    case CurrentLevel > ItemMaxLevel of
        true->
        	Acc;
        false->
            NeedReputation = 
            	case Type of
            		?TRAINER_EQUIPMENT_DECOMPOSE->
            			cacl_reinforce_reputation_trainer(ItemType, trunc(CurrentLevel), ItemStar);
            		?ACCELERATE_EQUIPMENT_DECOMPOSE->
            			cacl_reinforce_coin_accelerate_equip(ItemType,trunc(CurrentLevel),ItemStar);
            		_->
            			?ERR("undefine type:~w ~n",[Type]),
            			0
            	end,
            NewLevel = CurrentLevel+IncStep,
            calculate_equip_level_return(ItemType,ItemMaxLevel,NewLevel,ItemStar,Acc+NeedReputation,IncStep,Type)
    end.

get_stone_decompose_time(Star,_Level,ItemExp)->
	%%每个星等对应的比例不同
	Rate = data_equip_decompose:get({ger_stone_rate,Star}),
	trunc(ItemExp/Rate)+1.

%%计算分解的物品的总value值
caculate_total_decompose_value(DecomposeGerList,DecomposeGerEquipList,DecomposeStoneList,DecomposeTrainerEquipeList) ->
	{caculate_decompose_value(DecomposeGerList),caculate_decompose_value(DecomposeGerEquipList),caculate_decompose_value(DecomposeStoneList),caculate_decompose_value(DecomposeTrainerEquipeList)}.

%%计算具体某种类型的物品的总value值
caculate_decompose_value([]) -> 0;

caculate_decompose_value([Head|_] = DecomposeUnitList) ->
	if  is_record(Head,gerSimple)->
		    lists:foldl(fun(Ger,Acc) ->
				#gerSimple{gerQuality=Quality,gerLevel=Level,gerTypeID=GerTypeID}=Ger,
				#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
				BaseValue = data_equip_decompose:get({ger,GerStar}),
				LastValue = calculate_ger_value(BaseValue,Quality,Level),
				Acc+LastValue
			end,0,DecomposeUnitList);
		is_record(Head,item)->
			%%此处缺少对训练师装备的处理
			case lists:member(Head#item.itemType,?EQUIP_TYPE_LIST_NO_STONE) of
				true->
					lists:foldl(fun(Item,Acc)->
					#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank} = Item,
					#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
					case lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE) of
						true ->
							BaseValue = data_equip_decompose:get({gerEquip,ItemStar}),
							?INFO("BaseValue: ~w ~n",[BaseValue]),
							LastValue = calculate_ger_equip_value(BaseValue,Rank,Level),
							Acc+LastValue;
						false ->
							?INFO("分解的精灵装备列表中出现非装备：Type:~w TypeID:~w list: ~w ~n",[ItemType,ItemTypeID,DecomposeUnitList]),
							Acc
					end
			    	end,0,DecomposeUnitList);
				false->
			    	case lists:member(Head#item.itemType,?STONE_TYPE) of 
			    		true ->
							lists:foldl(fun(Item,Acc)->
							#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank} = Item,
							#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
							case lists:member(ItemType,?STONE_TYPE) of
								true ->
									BaseValue = data_equip_decompose:get({gerStone,ItemStar}),
									LastValue = calculate_ger_stone_value(BaseValue,Rank,Level),
									Acc+LastValue;
								false ->
									?INFO("分解的精灵符文列表中混入了奇怪的东西：Type:~w TypeID:~w,list:~w ~n",[ItemType,ItemTypeID,DecomposeUnitList]),
									Acc
							end
							end,0,DecomposeUnitList);
						false ->
							case lists:member(Head#item.itemType,?TRAINER_EQUIP_TYPE) of
								true->
									lists:foldl(fun(Item,Acc)->
									#item{itemType=ItemType,itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank} = Item,
									#data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
									case lists:member(ItemType,?TRAINER_EQUIP_TYPE) of
										true ->
											BaseValue = data_equip_decompose:get({trainerEquip,ItemStar}),
											LastValue = calculate_trainer_equip_value(BaseValue,Rank,Level),
											Acc+LastValue;
										false ->
											?INFO("分解的训练师装备列表中混入了奇怪的东西：Type:~w TypeID:~w,list:~w ~n",[ItemType,ItemTypeID,DecomposeUnitList]),
											Acc
									end
									end,0,DecomposeUnitList);
								false->
									?INFO("分解的装备中出现未知的类型：Type:~w ~n",[Head#item.itemType])
							end
					end
			end;
		true ->
			?INFO("混入了一些异常的东西？？？？"),
			0
	end.



%%对精灵计算value值
calculate_ger_value(BaseValue,Quality,_Level) ->
	case is_number(BaseValue) of 
		true ->
			BaseValue*(Quality+1);
		false ->
			?ERR("出现精灵基础value值不是数字的情况：~w ~n",[BaseValue]),
			1
	end.
%%对精灵装备计算value值
calculate_ger_equip_value(BaseValue,Rank,_Level)->
	case is_number(BaseValue) of 
		true ->
			BaseValue*(Rank+1);
		false ->
			?ERR("出现精灵装备基础value值不是数字的情况：~w ~n",[BaseValue]),
			1
	end.

%%对精灵符文计算value值
calculate_ger_stone_value(BaseValue,Rank,_Level)->
	case is_number(BaseValue) of 
		true ->
			BaseValue*(Rank+1);
		false ->
			?ERR("出现精灵符文基础value值不是数字的情况：~w ~n",[BaseValue]),
			1
	end.


%%对训练师装备计算value值
calculate_trainer_equip_value(BaseValue,Rank,_Level)->
	case is_number(BaseValue) of 
		true ->
			BaseValue*(Rank+1);
		false ->
			?ERR("出现训练师装备基础value值不是数字的情况：~w ~n",[BaseValue]),
			1
	end.

%%将奖励转换成decompose_reward以保存本次分解获得的物品
transform2decompose_reward(Reward)->
	?INFO("Reward: ~w  in transform2decompose_reward~n",[Reward]),
	{AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,ProfoundCrystal,_Honor,GenerateGer,GenerateItem} = Reward,
	NewItemList = case GenerateItem of 
		ignore ->
			[];
		_ ->
			GenerateItem
	end,
	NewGenerateGer = case GenerateGer of 
		ignore ->
			[];
		_ ->
			GenerateGer
	end,
	#decompose_reward{addcoin=AddCoin,addgold=AddGold,addgerexp=AddGerExp,addroleexp=AddRoleExp,addreputation=Reputation,addprofoundCrystal=ProfoundCrystal,gerlist=NewGenerateGer,itemlist=NewItemList}.

%%根据value值计算集体的奖励
decompose_get_reward(Money_decompose_value)->
	Money_decomposeList = data_equip_decompose:get(money_decompose),
	Money_decompose_box_list = get_box_list(Money_decomposeList,Money_decompose_value),
	?INFO("Money_decompose_box_list: ~w ~n ",[Money_decompose_box_list]),
	Reward = get_reward(Money_decompose_box_list),
	?INFO("reward : ~w ~n ",[Reward]),
	Reward.
decompose_get_reward(GerTotalValue,GerEquipTotalValue,StoneTotalValue,TrainerEquipTotalValue)->
	DecomposeList = data_equip_decompose:get(decompose),
	?INFO("DecomposeList:~w ~n ",[DecomposeList]),
	Decompose_Box_list = get_box_list(DecomposeList,GerTotalValue+GerEquipTotalValue+StoneTotalValue+TrainerEquipTotalValue),
	RewardList1 = get_reward(Decompose_Box_list),
	?INFO("RewardList1: ~w ~n ",[RewardList1]),
	RewardList1.

%%根据value以及type对应list获取对应的boxid以及次数列表
get_box_list(List,TotalValue)->
	?INFO("TotalValue:~w ~n",[TotalValue]),
	case TotalValue =:= 0 of
		true ->
			[];
		false->
			Result = util:foldl(fun(Unit,_)->
				{Value,E} = Unit,
				if 	TotalValue >Value  ->
					ignore;
				true ->
					{return,E}
				end
			end,0,List),
			case Result of 
				ignore->
					{_,E} = hd(lists:reverse(List)),
					E;
				_ ->
					Result
			end
	end.

%%抽奖
random_reward(Config, Times, List) when Times > 0->
	R = util:random_one_from_weigh_list(Config),
	random_reward(Config, Times-1, [R|List]);
random_reward(_, _, List) ->
	List.

%%获取奖励列表
get_reward(BoxUnitList) ->
	lists:foldl(fun(BoxUnit,Acc) ->
		{BoxID,Time} = BoxUnit,
		Config = data_equip_decompose:get(BoxID),
		random_reward(Config,Time,Acc)
		end,[],BoxUnitList).

%%加上金币，徽章等奖励
get_last_reward(Reward1,BoxInfo)->
	Reward2 = case BoxInfo#decompose_box_info.gold ==0 of
		false ->
			[{?REWARD_GOLD,BoxInfo#decompose_box_info.gold}|Reward1];
		true->
			Reward1
	end,
	Reward3 = case BoxInfo#decompose_box_info.coin ==0 of
		false ->
			[{?REWARD_COIN,BoxInfo#decompose_box_info.coin}|Reward2];
		true->
			Reward2
	end,
	case BoxInfo#decompose_box_info.reputation ==0 of 
		false ->
			[{?REWARD_REPU,BoxInfo#decompose_box_info.reputation}|Reward3];
		true->
			Reward3
	end.
%%检查是否满足再次分解的条件
check_cost_and_last_reward(LastRecord)->
	% #decompose_reward{addcoin=AddCoin,addgold=AddGold,addgerexp=AddGerExp,addroleexp=AddRoleExp,addreputation=AddReputation,gerlist=GerList,gerequiplist=GerEquipList,stonelist=StoneList,trainerequiplist=TrainerEquipList} = LastRecord,
	% {CoinCost,GoldCost,ReputationCost,ItemCost,GerCost} = data_equip_decompose:get(decompose_again_cost),
	% RoleInfo = role_data:get_roleInfo(),
	% #role{coin=Coin,reputation=Reputation,gold=Gold,goldBonus=GoldBonus} = RoleInfo,
	case check_reward_satisfy(LastRecord) of
		{true,LastItemList,LastGerList,LastEquipList,DeleteCurrencyList} ->
			case check_cost_satisfy(LastItemList,LastGerList,LastEquipList,DeleteCurrencyList) of
				{true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteGerList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList1} ->
					update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList1,?MONEY_DEC_TYPE_DECOMPOSE_EQUIPMENT_AGAIN),
					true;
				false->
					{false,5}
			end;
		false ->
			{false,4}
	end.

check_reward_satisfy(LastRecord)->
	case is_record(LastRecord,decompose_reward) of
		true->
			GerList = role_data:get_gerBag(),
			{LastGerList,ResultList} = lists:foldl(fun(GerInfo,{GerListAcc,ResultListAcc})->
				case lists:keytake(GerInfo#gerSimple.gerID,#gerSimple.gerID,GerListAcc) of
					false ->
						{GerListAcc,[false|ResultListAcc]};
					{_,_,Other} ->
						{Other,ResultListAcc}
				end
			end,{GerList,[]},LastRecord#decompose_reward.gerlist),
			case length(ResultList) =:= 0 of 
				false ->
					false;
				true ->
					{RealItemList,SpecialItemList} = lists:foldl(fun(E,{RealItemListAcc,SpecialItemListAcc}) when is_record(E,item)->{[E|RealItemListAcc],SpecialItemListAcc};(E,{RealItemListAcc,SpecialItemListAcc})->{RealItemListAcc,[E|SpecialItemListAcc]} end,{[],[]},LastRecord#decompose_reward.itemlist),
					BagItem = role_data:get_bagItem(),
					BagEquip = role_data:get_bagEquip(),
					{LastItemList,LastEquipList,Result} = lists:foldl(fun(ItemInfo,{ItemListAcc,EquipListAcc,ResultListAcc}) when is_record(ItemInfo,item)->
						case lists:keytake(ItemInfo#item.itemUID,#item.itemUID,EquipListAcc) of
							false ->
								%%背包装备存在叠加的情况，此处没法像装备一样进行删除，需要递归的处理
								case delete_item(ItemListAcc,[],[],[],{item,ItemInfo#item.itemTypeID,ItemInfo#item.itemNum,ItemInfo#item.itemLevel,ItemInfo#item.itemRank}) of
									{true,NewItemList,NewEquipList,_DeleteItemList,_DeleteEquipList}->
										{NewItemList,EquipListAcc,ResultListAcc};
									_ ->
										{ItemListAcc,EquipListAcc,[false|ResultListAcc]}
								end;
							{_,FindItem,OtherEquip} ->
								if 
									FindItem#item.itemNum > ItemInfo#item.itemNum -> 
										{ItemListAcc,[FindItem#item{itemNum=FindItem#item.itemNum-ItemInfo#item.itemNum}|OtherEquip],ResultListAcc};
									FindItem#item.itemNum =:= ItemInfo#item.itemNum ->
										{ItemListAcc,OtherEquip,ResultListAcc};
									true ->
										{ItemListAcc,EquipListAcc,[false|ResultListAcc]}
								end
						end;
						(_ItemInfo,{ItemListAcc,EquipAcc,ResultAcc})->
							{ItemListAcc,EquipAcc,[false|ResultAcc]}
					end,{BagItem,BagEquip,[]},RealItemList),
					case length(Result) =:= 0 of
						true ->
								%%没有处理玩家经验和精灵经验
								#decompose_reward{addcoin=AddCoin,addgold=AddGold,addreputation=AddReputation,addprofoundCrystal=AddProfoundCrystal} = LastRecord,
								DeleteCurrencyList =merge_currencylist([{?ITEM_TYPE_GOLD,AddGold},{?ITEM_TYPE_COIN,AddCoin},{?ITEM_TYPE_REPUTATION,AddReputation},{?ITEM_TYPE_PROFOUNDCRYSTAL,AddProfoundCrystal}],SpecialItemList),
								Role = role_data:get_roleInfo(),
								case check_role_currency_enough(Role,DeleteCurrencyList) of
									false->
										false;
									_ ->
										{true,LastItemList,LastGerList,LastEquipList,DeleteCurrencyList}
								end;
						false ->
							false
					end
			end;
		false->
			false
	end.



check_cost_satisfy(LastItemList,LastGerList,LastEquipList,DeleteCurrencyList)->
	{GoldCost,CoinCost,ReputationCost,ItemCostList,GerCostList} = data_equip_decompose:get(decompose_again_cost),
	NewDeleteCurrencyList = merge_currencylist(DeleteCurrencyList,[{?ITEM_TYPE_GOLD,GoldCost},{?ITEM_TYPE_COIN,CoinCost},{?ITEM_TYPE_REPUTATION,ReputationCost}]),
	Role = role_data:get_roleInfo(),
	case check_role_currency_enough(Role,NewDeleteCurrencyList) of
		false->
			false;
		_ ->
			case delete_ger_list(LastGerList,GerCostList) of
				{true,NewLastGerList,LastDeleteList}->
					case delete_item_list(LastItemList,LastEquipList,ItemCostList) of
						{true,NewItemList,NewEquipList,DeleteItemList,DeleteEquipList}->
							{true,NewItemList,NewLastGerList,NewEquipList,LastDeleteList,DeleteItemList,DeleteEquipList,NewDeleteCurrencyList};
						{false,_,_,_,_}->
							false
					end;
				{false,_,_}->
					false
			end
	end.

merge_currencylist(OriginCurrencyList,AddCurrencyList)->
	lists:foldl(fun({K,V}=E,Acc)->
		case lists:keytake(K,1,Acc) of
			false->
				[E|Acc];
			{_Value,{K,OV},Other}->
				[{K,OV+V}|Other]
		end
	end,OriginCurrencyList,AddCurrencyList).

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
			% ?INFO("完成精灵的删除:删除前：~w 删除配置：~w  删除列表：~w 删除后结果：~w ~n",[GerList,DelGerList,LastDeleteList,LastGerList]),
			{true,LastGerList,LastDeleteList};
		false ->
			{false,LastGerList,LastDeleteList}
	end.

%%删除某类精灵并返回删除后的精灵列表以及删除的列表O
%%在此处做了一个特殊处理，如果需要删除的精灵的等级为负数，则不用考虑等级一致
delete_ger(GerList,DeletGerInfo,DeleteGerList)->
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	HomeSteadGerID = homestead_server:get_homestead_ger(RoleID),
	{ger,NeedGerTypeID,NeedGerNum,NeedGerLevel,NeedGerRank} = DeletGerInfo,
	{LastResultList,LastDeleteList,LastAcc} = lists:foldl(fun(Ger,{ResultList,DeleteList,Acc})->
		#gerSimple{gerTypeID=GerTypeID,gerQuality=GerQuality,gerPos=GerPos,gerID=GerID,gerLevel=GerLevel} = Ger,
			case Acc >0 of
				true ->
					case GerTypeID == NeedGerTypeID andalso GerQuality == NeedGerRank andalso (GerLevel == NeedGerLevel orelse NeedGerLevel < 0) andalso GerPos == 0 andalso GerID =/= HomeSteadGerID of
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
	{item,DelItemTypeID,DelItemNum,DelItemLevel,DelItemRank} = DeleteInfo,
	%%此处对需要被删除的道具进行排序，数量从小到大,防止出现多个数量不足最大值的道具
	SortItemList = lists:sort(fun(#item{itemTypeID=ItemTypeIDA,itemNum=ItemNumA},#item{itemTypeID=ItemTypeIDB,itemNum=ItemNumB})->
		case ItemTypeIDA=:=DelItemTypeID of
			true->
				case ItemTypeIDB=:=DelItemTypeID of
					true->
						ItemNumA =< ItemNumB;
					false->
						true
				end;
			false->
				ItemTypeIDB =:= DelItemTypeID
		end
	end,ItemList),
	{LastItemList,LastDeleteItemList,Result} = lists:foldl(fun(Item,{ResultItemlist,DeleteItemListAcc,Acc})->
		case Acc > 0 of
			true->
				#item{itemTypeID=ItemTypeID,itemLevel=Level,itemRank=Rank,itemNum=Num,itemPos=Pos}=Item,
				case ItemTypeID=:=DelItemTypeID andalso (Level =:=DelItemLevel orelse lists:member(ItemTypeID,?KEYSTONELIST)) andalso Rank=:=DelItemRank andalso Pos=:=0 of
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
	end,{[],DeleteItemList,DelItemNum},SortItemList),
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

%%根据新的数据跟新玩家信息
update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,Type)->
	update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,Type,0,"").
update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,Type,ArgID,Desc)->
	BagItem = role_data:get_bagItem(),
	BagEquip = role_data:get_bagEquip(),
	GerList = role_data:get_gerBag(),
	%DelItem = BagItem--NewItemList,
	DelItem = lists:foldl(fun(Item,Acc)->
		case lists:keyfind(Item#item.itemUID,#item.itemUID,NewItemList) of
				false->
					[Item|Acc];
				FindOne ->
					case FindOne#item.itemNum=/= Item#item.itemNum of
						true->
							[Item#item{itemNum=Item#item.itemNum-FindOne#item.itemNum}|Acc];
						false->
							Acc
					end
		end end,[],BagItem),
	% DelEquip = BagEquip--NewEquipList,出现道具的UID没有变化，但是其他属性发生变化的情况下，道具会删除的情况，所以进行修改
	DelEquip = lists:foldl(fun(Equip,Acc)->
		case lists:keyfind(Equip#item.itemUID,#item.itemUID,NewEquipList) of
				false->
					[Equip|Acc];
				_ ->
					Acc
		end end,[],BagEquip),
	%%此处可能出现背包中的精灵的属性有变化，故不能直接使用--来筛选
	DelGer = lists:foldl(fun(G=#gerSimple{gerID=GerID},Acc)->
		case lists:keyfind(GerID,#gerSimple.gerID,NewLastGerList) of
			false->
				[G|Acc];
			_->
				Acc
		end end,[],GerList),
	RoleInfo = role_data:get_roleInfo(),
	#role{coin=Coin,gold=Gold,goldBonus=GoldBonus,reputation=Reputation,roleID=RoleID,profoundCrystal=ProfoundCrystal,honor=Honor,unioncoin=UnionCoin,home_resource=HomeResource} = RoleInfo,
	role_data:set_bagEquip(NewEquipList),
	role_data:set_bagItem(NewItemList),

	%% 由于出现道具的使用不是全部使用，所以DelItem需要处理掉不是删除道具的物品
	{LastUpdateItemList,LastDeleteItemList} = lists:foldl(fun(Item,{UpdateAcc,DeleteAcc})->
		#item{itemUID=ItemUID} = Item,
			case lists:keyfind(ItemUID,#item.itemUID,NewItemList) of
				false ->
					{UpdateAcc,[Item|DeleteAcc]};
				FindItem ->
					{[FindItem|UpdateAcc],DeleteAcc}
			end
		end,{[],[]},DelItem),
	?INFO("LastUpdateItemList:~w LastDeleteItemList: ~w DelItem: ~w ~n",[LastUpdateItemList,LastDeleteItemList,DelItem]),
	LogEquipList = role_item:itemList2logItemList(DelEquip++DelItem, []),
	case [ItemUID||#item{itemUID=ItemUID}<-(DelEquip++LastDeleteItemList)] of
		[]->
			ignore;
		DeletItemUIDList->
			?sendself(#sc_item_delete_notify{itemUIDList=DeletItemUIDList})
	end,

	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogEquipList, Date, Time, Type, ArgID, Desc),
	case [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-LastUpdateItemList] of
		[]->
			ignore;
		Update_item_Unit_list->
    		?sendself(#sc_item_update{updateList=Update_item_Unit_list})
    end,


	role_data:set_gerBag(NewLastGerList),
	LogGerList = role_ger:gerList2logGerList(DelGer),
	case [GerID||#gerSimple{gerID = GerID}<-DelGer] of
		[]->
			ignore;
		DeleteGerIDList->
			?sendself(#sc_ger_del{gerIDList=DeleteGerIDList})
	end,
    
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
	behavior_ger_consume:log(RoleID, LogGerList, Date, Time, Type, ArgID,Desc),
	% RoleInfo2 = role_lib:deduct_coin_f(RoleInfo, Coin-NewCoin, Type, 0, ""),
	% RoleInfo3 = role_lib:deduct_gold_2_f(RoleInfo2, Gold+GoldBonus-NewGold-NewGoldBonus, Type, 0, ""),
	% RoleInfo4 =role_lib:deduct_reputation_f(RoleInfo3, Reputation-NewReputation, Type, 0, ""),
	% RoleInfo5 =role_lib:deduct_profoundCrystal_f(RoleInfo4,ProfoundCrystal-NewProfoundCrystal,Type,0,""),
	% RoleInfo6 =role_lib:deduct_honor_f(RoleInfo5,Honor-NewHonor,Type,0,""),
 %    RoleInfo7 =role_lib:deduct_home_resource_f(RoleInfo6,HomeResource-NewHomeResource,Type,0,""),
	% role_lib:deduct_unioncoin_f(RoleInfo7,UnionCoin-NewUnionCoin,Type,0,"").
	role_lib:deduct_currency(RoleInfo,DeleteCurrencyList,Type,ArgID,Desc).

%%兼容以前旧的接口，v315之后最好使用新的接口
update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,Type)->
	#role{profoundCrystal=ProfoundCrystal} = role_data:get_roleInfo(),
	update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,ProfoundCrystal,Type).

update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,ProfoundCrystal,Type)->
	#role{unioncoin=UnionCoin,honor=Honor,home_resource=HomeResource} = role_data:get_roleInfo(),
	update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,ProfoundCrystal,UnionCoin,Honor,HomeResource,Type).

update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,NewProfoundCrystal,NewUnionCoin,NewHonor,Type)->
    #role{home_resource=HomeResource} = role_data:get_roleInfo(),
    update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,NewProfoundCrystal,NewUnionCoin,NewHonor,HomeResource,Type).   
update_role_info(NewItemList,NewLastGerList,NewEquipList,NewCoin,NewGold,NewGoldBonus,NewReputation,NewProfoundCrystal,NewUnionCoin,NewHonor,NewHomeResource,Type)->
	BagItem = role_data:get_bagItem(),
	BagEquip = role_data:get_bagEquip(),
	GerList = role_data:get_gerBag(),
	%DelItem = BagItem--NewItemList,
	DelItem = lists:foldl(fun(Item,Acc)->
		case lists:keyfind(Item#item.itemUID,#item.itemUID,NewItemList) of
				false->
					[Item|Acc];
				FindOne ->
					case FindOne#item.itemNum=/= Item#item.itemNum of
						true->
							[Item#item{itemNum=Item#item.itemNum-FindOne#item.itemNum}|Acc];
						false->
							Acc
					end
		end end,[],BagItem),
	% DelEquip = BagEquip--NewEquipList,出现道具的UID没有变化，但是其他属性发生变化的情况下，道具会删除的情况，所以进行修改
	DelEquip = lists:foldl(fun(Equip,Acc)->
		case lists:keyfind(Equip#item.itemUID,#item.itemUID,NewEquipList) of
				false->
					[Equip|Acc];
				_ ->
					Acc
		end end,[],BagEquip),
	DelGer = GerList--NewLastGerList,

	RoleInfo = role_data:get_roleInfo(),
	#role{coin=Coin,gold=Gold,goldBonus=GoldBonus,reputation=Reputation,roleID=RoleID,profoundCrystal=ProfoundCrystal,honor=Honor,unioncoin=UnionCoin,home_resource=HomeResource} = RoleInfo,
	role_data:set_bagEquip(NewEquipList),
	role_data:set_bagItem(NewItemList),

	%% 由于出现道具的使用不是全部使用，所以DelItem需要处理掉不是删除道具的物品
	{LastUpdateItemList,LastDeleteItemList} = lists:foldl(fun(Item,{UpdateAcc,DeleteAcc})->
		#item{itemUID=ItemUID} = Item,
			case lists:keyfind(ItemUID,#item.itemUID,NewItemList) of
				false ->
					{UpdateAcc,[Item|DeleteAcc]};
				FindItem ->
					{[FindItem|UpdateAcc],DeleteAcc}
			end
		end,{[],[]},DelItem),
	?INFO("LastUpdateItemList:~w LastDeleteItemList: ~w DelItem: ~w ~n",[LastUpdateItemList,LastDeleteItemList,DelItem]),
	LogEquipList = role_item:itemList2logItemList(DelEquip++DelItem, []),
	case [ItemUID||#item{itemUID=ItemUID}<-(DelEquip++LastDeleteItemList)] of
		[]->
			ignore;
		DeletItemUIDList->
			?sendself(#sc_item_delete_notify{itemUIDList=DeletItemUIDList})
	end,

	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogEquipList, Date, Time, Type, 0, ""),
	case [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-LastUpdateItemList] of
		[]->
			ignore;
		Update_item_Unit_list->
    		?sendself(#sc_item_update{updateList=Update_item_Unit_list})
    end,


	role_data:set_gerBag(NewLastGerList),
	LogGerList = role_ger:gerList2logGerList(DelGer),
	case [GerID||#gerSimple{gerID = GerID}<-DelGer] of
		[]->
			ignore;
		DeleteGerIDList->
			?sendself(#sc_ger_del{gerIDList=DeleteGerIDList})
	end,
    
    [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
	behavior_ger_consume:log(RoleID, LogGerList, Date, Time, Type, 0, ""),
	RoleInfo2 = role_lib:deduct_coin_f(RoleInfo, Coin-NewCoin, Type, 0, ""),
	RoleInfo3 = role_lib:deduct_gold_2_f(RoleInfo2, Gold+GoldBonus-NewGold-NewGoldBonus, Type, 0, ""),
	RoleInfo4 =role_lib:deduct_reputation_f(RoleInfo3, Reputation-NewReputation, Type, 0, ""),
	RoleInfo5 =role_lib:deduct_profoundCrystal_f(RoleInfo4,ProfoundCrystal-NewProfoundCrystal,Type,0,""),
	RoleInfo6 =role_lib:deduct_honor_f(RoleInfo5,Honor-NewHonor,Type,0,""),
    RoleInfo7 =role_lib:deduct_home_resource_f(RoleInfo6,HomeResource-NewHomeResource,Type,0,""),
	role_lib:deduct_unioncoin_f(RoleInfo7,UnionCoin-NewUnionCoin,Type,0,"").

transform2p_reward_view(Config)->
	{GoldCost,CoinCost,ReputationCost,ItemCostList,GerCostList} = Config,
	ItemList = [{?REWARD_ITEM,ItemTypeID,ItemNum}||{item,ItemTypeID,ItemNum,_ItemLevel,_ItemRank}<-ItemCostList],
	GerList = [{?REWARD_GER,GerTypeID,GerNum}||{ger,GerTypeID,GerNum,_GerLevel,_GerRank}<-GerCostList],
	ResultList1 = if
		GoldCost =/= 0 ->
			[{?REWARD_GOLD,GoldCost}];
		true ->
			[]
	end,
	ResultList2 = if
		CoinCost =/= 0 ->
			[{?REWARD_COIN,CoinCost}|ResultList1];
		true ->
			ResultList1
	end,
	ResultList3 = if
		ReputationCost =/= 0 ->
			[{?REWARD_REPU,ReputationCost}|ResultList2];
		true ->
			ResultList2
	end,
	Result = ResultList3++ItemList++GerList,
	role_reward:transform2p_reward_view(Result,[]).
%%扣除玩家物品，物品配置使用sell_reward结构,不能扣除出战精灵经验和玩家经验
delete_sell_reward(SellReward,BandItemIDList,BandGerIDList,GerList1,ItemList1,EquipList1) when is_record(SellReward,sell_reward)->
	#sell_reward{coin=NeedCoin,gold=NeedGold,reputation=NeedReputation,item=NeedItem,newGer=NeedGer2} = SellReward,
	{CurrencyList,NewNeedItem1} = special_coin(NeedItem++[#new_item{itemTypeID=?ITEM_TYPE_REPUTATION,itemNum=NeedReputation,itemRank=0,itemLevel=1},#new_item{itemTypeID=?ITEM_TYPE_COIN,itemNum=NeedCoin,itemRank=0,itemLevel=1},#new_item{itemTypeID=?ITEM_TYPE_GOLD,itemNum=NeedGold,itemLevel=1,itemRank=0}]),
	% GerList1 = role_data:get_gerBag(),
	% ItemList1 = role_data:get_bagItem(),
	% EquipList1 = role_data:get_bagEquip(),
	{ItemList,EquipList,ItemBandList,EquipBandList}=case band_item_list(ItemList1,EquipList1,BandItemIDList) of
		{ok,NewItemList1,NewEquipList1,ItemBandList1,EquipBandList1}->
			{NewItemList1,NewEquipList1,ItemBandList1,EquipBandList1};
		false->
			erlang:throw(false)
	end,
	{GerList,GerBandList} = case band_ger_list(GerList1,BandGerIDList) of
		{ok,NewGerList,GerBandList1}->
			{NewGerList,GerBandList1};
		false->
			erlang:throw(false)
	end,
    %%准备使用delete_ger_list函数，但是当时删除的结构是{ger,5010,1,1,0}和{item,20011,7,1,0}，所以此处需要转换下,自作孽不可活呀，自己挖坑自己跳。。。。
    NewNeedItem = [{item,ItemTypeID,ItemNum,ItemLevel,ItemRank}||#new_item{itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=ItemRank,itemNum=ItemNum}<-NewNeedItem1],
	NeedGer = [{ger,NeedGerTypeID,1,NeedGerLevel,NeedGerRank}||#new_ger{gerTypeID=NeedGerTypeID,gerLevel=NeedGerLevel,gerQuality=NeedGerRank}<-NeedGer2],
	Role = role_data:get_roleInfo(),
	case check_role_currency_enough(Role,CurrencyList) of
		false ->
			false;
		_->
			case delete_ger_list(GerList,NeedGer) of
				{true,NewLastGerList,LastDeleteList}->
					case delete_item_list(ItemList,EquipList,NewNeedItem) of
						{true,NewItemList,NewEquipList,DeleteItemList,DeleteEquipList}->
							{true,NewItemList++ItemBandList,NewLastGerList++GerBandList,NewEquipList++EquipBandList,LastDeleteList,DeleteItemList,DeleteEquipList,CurrencyList};
						{false,_,_,_,_}->
							false
					end;
				{false,_,_}->
					false
			end
	end.

delete_sell_reward(SellReward,BandItemIDList,BandGerIDList) when is_record(SellReward,sell_reward)->
	GerList1 = role_data:get_gerBag(),
	ItemList1 = role_data:get_bagItem(),
	EquipList1 = role_data:get_bagEquip(),
	delete_sell_reward(SellReward,BandItemIDList,BandGerIDList,GerList1,ItemList1,EquipList1);
delete_sell_reward(_SellReward,_BandItemIDList,_BandGerIDList)->
	?ERR("使用sell_reward扣除玩家物品，出现非sell_reward结构"),
	false.

check_role_currency_enough(Role,CurrencyList)->
	check_role_currency_enough(Role,CurrencyList,[]).
check_role_currency_enough(Role,[],NewCurrencyList)->
	NewCurrencyList;
check_role_currency_enough(Role,[{CurrencyType,CurrencyValue}=H|T],NewCurrencyList)->
	case lists:keyfind(CurrencyType,1,NewCurrencyList) of
		false->
			case role_lib:check_role_single_currency_enough(Role,H) of
				true->
					check_role_currency_enough(Role,T,[H|NewCurrencyList]);
				false->
					false
			end;
		_ ->
			?ERR("check_role_currency_enough more than one currencytype:~w exist in CurrentcyList:~w ",[CurrencyType,NewCurrencyList]),
			false
	end.


band_item_list(ItemList,EquipList,BandItemIDList)->
	band_item_list(ItemList,EquipList,[],[],BandItemIDList).
band_item_list(ItemList,EquipList,ItemBandList,EquipBandList,[])->
	{ok,ItemList,EquipList,ItemBandList,EquipBandList};
band_item_list(ItemList,EquipList,ItemBandList,EquipBandList,[BandItem|RemainBandList])->
	?INFO("ItemList:~w BandItem:~w ~n",[ItemList,BandItem]),
	case lists:keytake(BandItem,#item.itemUID,ItemList) of
		{_,FindOne,Other}->
			band_item_list(Other,EquipList,[FindOne|ItemBandList],EquipBandList,RemainBandList);
		false ->
			case lists:keytake(BandItem,#item.itemUID,EquipList) of
				{_,FindOne,Other}->
					band_item_list(ItemList,Other,ItemBandList,[FindOne|EquipBandList],RemainBandList);
				false ->
					band_item_list(ItemList,EquipList,ItemBandList,EquipBandList,RemainBandList)
			end
	end.
band_ger_list(GerList,BandGer)->
	band_ger_list(GerList,[],BandGer).
band_ger_list(GerList,GerBandList,[])->
	{ok,GerList,GerBandList};
band_ger_list(GerList,GerBandList,[BandGer|RemainBandGerList])->
	case lists:keytake(BandGer,#gerSimple.gerID,GerList) of
		{_,FindOne,Other}->
			band_ger_list(Other,[FindOne|GerBandList],RemainBandGerList);
		false->
			band_ger_list(GerList,GerBandList,RemainBandGerList)
	end.

%%由于在Item列表中配置了奥义，荣耀，公会等货币，此处对这些特殊货币进行统计
special_coin(ItemList) ->
	special_coin(ItemList,[],[]).
special_coin([],SpecialCurrencyList,NormalItemList)->
	{SpecialCurrencyList,NormalItemList};
special_coin([Item|Other],SpecialCurrencyList,NormalItemList) when is_record(Item,new_item)->
	#new_item{itemTypeID=ItemTypeID,itemNum=ItemNum} = Item,
	case lists:member(ItemTypeID,?SPECIAL_CURRENCY_LIST) of
		false->
			special_coin(Other,SpecialCurrencyList,[Item|NormalItemList]);
		true->
			case lists:keytake(ItemTypeID,1,SpecialCurrencyList) of
				false->
					special_coin(Other,[{ItemTypeID,ItemNum}|SpecialCurrencyList],NormalItemList);
				{_Value,{_ItemTypeID,OldNum},OtherCurrency}->
					special_coin(Other,[{ItemTypeID,erlang:max(OldNum+ItemNum,0)}|OtherCurrency],NormalItemList)
			end
	end;
special_coin([{ItemTypeID,Num}=H|T],SpecialCurrencyList,NormalItemList)->
	case lists:member(ItemTypeID,?SPECIAL_CURRENCY_LIST) of
		false->
			?ERR("出现非特殊货币类型 ~w ~n",[H]),
			special_coin(T,SpecialCurrencyList,NormalItemList);
		true->
			case lists:keytake(ItemTypeID,1,SpecialCurrencyList) of
				false->
					special_coin(T,[H|SpecialCurrencyList],NormalItemList);
				{_Value,{ItemTypeID,OldNum},OtherCurrency}->
					special_coin(T,[{ItemTypeID,OldNum+Num}|OtherCurrency],NormalItemList)
			end
	end;
special_coin([Item|Other],SpecialCurrencyList,NormalItemList)->
	?ERR("sell_reward结构中出现非new_item结构 ~w ~n",[Item]),
	special_coin(Other,SpecialCurrencyList,NormalItemList).

transform_item2p_equip2(Item,GerID) when is_record(Item,item)->
	#item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=ItemRank,itemNum=_ItemNum,itemPos=ItemPos,itemDecay=ItemDecay,itemExp=ItemExp,itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel}=Item,
	#p_equip2{itemUID=ItemUID,itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=ItemRank,itemGerID=GerID,itemPos=ItemPos,itemDecay=ItemDecay,itemExp=ItemExp,itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel};
transform_item2p_equip2(_Item,_GerID)->
	#p_equip2{itemUID=0,itemTypeID=0,itemLevel=0,itemRank=0,itemGerID=0,itemPos=0,itemDecay=0,itemExp=0,itemenchantType=0,itemenchantLevel=0}.
get_stone_expirence_box(_ItemStar,_Level,ItemExp)->
	{ExpBoxIDList,Rate} = data_equip_decompose:get(data_stone_decompose_exp_box),
	Time = ItemExp div Rate,
	[{ExpBoxID,Time}||ExpBoxID<-ExpBoxIDList].

%%判断精灵是否能够装备精华符文
check_essencestone_up_condition(_GerID,ItemPos)->
	RoleInfo = role_data:get_roleInfo(),
	case data_item_stone:get(essence_needLevel) of
		?undefined->
			false;
		LevelList->
			case lists:keyfind(ItemPos,1,LevelList) of
				false->
					false;
				{Pos,NeedLevel}->
					NeedLevel=<RoleInfo#role.level
			end		
	end.

%%此处需要修改，确定被吃的符文的经验如何转换到新的符文中去。
inherit_exp(FoodStoneInfoList,_OldItemType,Type)->
	case Type of
		?STONE_EAT->	
   			lists:foldl(fun(#item{itemTypeID=FoodTypeID,itemType=ItemType,itemExp=ItemExp,itemRank=ItemRank},ExpAcc)->                            
    			DataItem_ = data_item:get(FoodTypeID),
        		ExpAcc + (data_stone_applyexp:get({DataItem_#data_item.itemStar,ItemType,?NormalStoneEat}))*(ItemRank+1)+ItemExp
        		end, 0, FoodStoneInfoList);
        ?STONE_UPRANK->
   			lists:foldl(fun(#item{itemTypeID=FoodTypeID,itemType=ItemType,itemExp=ItemExp},ExpAcc)->                            
    			DataItem_ = data_item:get(FoodTypeID),
        		ExpAcc +ItemExp
        		end, 0, FoodStoneInfoList)
    end.	

%%主要根据主符文和材料符文的信息，修改主符文的品阶
motify_stone_rank(SrcStone,Food)->
	#item{itemRank=SrcItemRank} = SrcStone,
	#item{itemRank=FoodItemRank} = Food,
	StoneMaxRank = data_item_stone:get(stone_max_uprank),
	SrcStone#item{itemRank=min(StoneMaxRank,SrcItemRank+FoodItemRank+1)}.

get_stone_uprank_cost(SrcRank,FoodRank,Star)->
	StoneMaxRank = data_item_stone:get(stone_max_uprank),
	FinalRank = min(SrcRank+FoodRank+1,StoneMaxRank),
	DesCost = get_stone_uprank_cost2(Star,0,FinalRank),
	SrcCost = get_stone_uprank_cost2(Star,0,SrcRank),
	FoodCost = get_stone_uprank_cost2(Star,0,FoodRank),
	Cost = DesCost - SrcCost - FoodCost,
	case Cost >0 of
		true->
			Cost;
		false->
			0
	end.
get_stone_uprank_cost2(Star,BeginRank,EndRank) when BeginRank =<EndRank->
	get_stone_uprank_cost2(Star,BeginRank,EndRank,0);
get_stone_uprank_cost2(Star,BeginRank,EndRank)->
	0.
get_stone_uprank_cost2(Star,EndRank,EndRank,Cost)->
	Cost;
get_stone_uprank_cost2(Star,BeginRank,EndRank,Cost)->
	TempCost = 
		case data_item_stone:get({uprank_cost,Star,BeginRank+1}) of
			?undefined->
				0;
			FindCost->
				FindCost
		end,
	get_stone_uprank_cost2(Star,BeginRank+1,EndRank,Cost+TempCost).

check_item_stone_legend(_SrcItemUID,GerID) when GerID == 1000 -> {false,6};
check_item_stone_legend(SrcItemUID,GerID) ->
    EquipList = if GerID == 0 -> role_data:get_bagEquip();
                   true -> role_data:get_equip(GerID)
                end,
    case lists:keytake(SrcItemUID, #item.itemUID, EquipList) of
        false -> {false,2};
        {value,Item,EquipList2} ->
            case check_stone_can_legend(Item) of
                {false,R} -> {false,R};
                true -> case check_stone_legend_cost(Item#item.itemTypeID) of
                            {true, CostList} -> {true,Item,EquipList2,CostList};
                            false -> {false,6}
                        end
            end
    end.

check_stone_can_legend(#item{itemTypeID=ItemTypeID,itemRank=ItemRank}) ->
    case lists:member(ItemTypeID, ?STONE_CAN_LEGEND_LIST) of 
        true -> case ItemRank == 10 of
                    true -> true;
                    false -> {false,4}
                end;
        false -> {false,4}
    end.

check_stone_legend_cost(NewItemTypeID)->
    case data_legendary:get({data_legendary_cost,NewItemTypeID}) of
        NeedList when erlang:is_list(NeedList) ->
            Role = role_data:get_roleInfo(),
            BagItem = role_data:get_bagItem(),
            ?INFO("check_make_legend_cost NeedList:~w",[NeedList]),
            case item_lib:check_need_list(Role, NeedList, BagItem, []) of
                false ->
                    false;
                {true, CostList} -> %% {BagItem2,Role,BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu}
                    {true, CostList}
            end;
        _ ->
            false
    end.

do_item_stone_legend(SrcItemUID,GerID,Item,EquipList2,{NewBagItem,Role,_BagEquip, DelItem, UpdateItem, LogItemList, NeedCoin, NeedGold, NeedRepu})->
    #role{roleID=RoleID} = Role,
    #item{itemUID=NewItemID,itemTypeID=NewItemTypeID,itemPos=EquipPos}=NewItem = new_legend_stone(RoleID,Item),
    LogStr = io_lib:format("~w(~w,~w,~w) to ~w", [Item#item.itemTypeID ,Item#item.itemLevel,Item#item.itemRank,SrcItemUID, NewItemID]),
    Role2 = role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_STONE_LEGEND, SrcItemUID, LogStr),
    Role3 = role_lib:deduct_reputation_f(Role2, NeedRepu, ?MONEY_DEC_TYPE_STONE_LEGEND, SrcItemUID, LogStr),
    _Role4 = role_lib:deduct_gold_f(Role3, NeedGold, ?MONEY_DEC_TYPE_STONE_LEGEND, SrcItemUID, LogStr),
    {Date, _} = Time = erlang:localtime(),
    if DelItem == [] andalso UpdateItem == [] ->
           behavior_item_consume:log(RoleID, [SrcItemUID], Date, Time, ?MONEY_DEC_TYPE_STONE_LEGEND, NewItemTypeID, LogStr),
           DelNotify = [SrcItemUID];
           %?sendself(#sc_item_delete_notify{itemUIDList=[SrcItemUID]});
       true ->
           %% 写道具日志
%%            LogItemList = role_item:itemList2logItemList(DelItem, LogItemList),
           behavior_item_consume:log(RoleID, [SrcItemUID|LogItemList], Date, Time, ?MONEY_DEC_TYPE_STONE_LEGEND, NewItemTypeID, LogStr),
           role_data:set_bagItem(NewBagItem),
           DelItemIDList = [E||#item{itemUID=E}<-DelItem],
           UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem],
           %% 提醒客户端更新物品
           %?sendself(#sc_item_delete_notify{itemUIDList=[SrcItemUID|DelItemIDList]}),
            DelNotify = [SrcItemUID|DelItemIDList],
           ?sendself(#sc_item_update{updateList=UpdateList})
    end,
    NewEquipList = [NewItem|EquipList2],
    if GerID =:= 0 ->
           role_data:set_bagEquip(NewEquipList);
       true ->
           EquipList = role_data:set_equip(GerID,NewEquipList),
           role_lvlSgAttr:on_equip_change(GerID, EquipList),
           ger_attr:recacl_f(GerID)
    end,
    ?sendself(#sc_item_stone_legend{result=1,newItemID=NewItemID,srcItemID=SrcItemUID,equipPos=EquipPos,itemGerID=GerID}),
    ?sendself(#sc_item_delete_notify{itemUIDList=DelNotify}).

new_legend_stone(RoleID,#item{itemTypeID=ItemTypeID,itemUID=OldUID}=OldItem) ->
    ItemUID = tk_id:gen_itemUID(),
    NewItemTypeID = data_legendary:get({stone_legend_exchange,ItemTypeID}),
    {Date, _} = Time = erlang:localtime(),
    behavior_item_add:log(RoleID, [[ItemUID,NewItemTypeID,1,0]], Date, Time, ?MONEY_ADD_TYPE_STONE_LEGEND, NewItemTypeID, integer_to_list(OldUID)),
    Item = item_lib:recacl(OldItem#item{itemTypeID=NewItemTypeID,itemUID=ItemUID}),
    ?sendself(#sc_item_new{newItemList=[item_lib:item2p_item(Item)]}),
    Item.


check_cs_item_make_legend(SrcItemUID,GerID) when GerID =:= 1000->
    {false, 6};
check_cs_item_make_legend(SrcItemUID,GerID)->
    if GerID =:= 0 ->
           EquipList = role_data:get_bagEquip();
       true ->
           EquipList = role_data:get_equip(GerID)
    end,
    %% 判断装备是否存在
    case lists:keytake(SrcItemUID, #item.itemUID, EquipList) of
        false ->
            {false, 2};
        {value, Item, EquipList2}->
            case lists:member(Item#item.itemTypeID + ?LEGENDIDSHIFT, ?LEGENDLIST) of
                false ->
                    {false, 4};
                true when Item#item.itemRank >= 10 ->
                    case check_make_legend_cost(Item#item.itemTypeID) of
                        {true,CostList} ->
                            {true,Item,EquipList2,CostList};
                        false ->
                            {false, 6}
                    end
            end
    end.

check_make_legend_cost(NewItemTypeID)->
    case data_legendary:get({data_legendary_cost,NewItemTypeID}) of
        NeedList when erlang:is_list(NeedList) ->
            Role = role_data:get_roleInfo(),
            BagItem = role_data:get_bagItem(),
            ?INFO("check_make_legend_cost NeedList:~w",[NeedList]),
            case item_lib:check_need_list(Role, NeedList, BagItem, []) of
                false ->
                    false;
                {true, CostList} -> %% {BagItem2,Role,BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu}
                    {true, CostList}
            end;
        _ ->
            false
    end.

do_item_make_legend(SrcItemUID,GerID,Item,EquipList2,{NewBagItem,Role,_BagEquip, DelItem, UpdateItem, LogItemList, NeedCoin, NeedGold, NeedRepu})->
    #role{roleID=RoleID} = Role,
    LogStr = io_lib:format("~w(~w) legend to ~w", [Item#item.itemTypeID ,SrcItemUID, Item#item.itemTypeID + ?LEGENDIDSHIFT]),
    Role2 = role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_MAKE_LEGEND, SrcItemUID, LogStr),
    Role3 = role_lib:deduct_reputation_f(Role2, NeedRepu, ?MONEY_DEC_TYPE_MAKE_LEGEND, SrcItemUID, LogStr),
    _Role4 = role_lib:deduct_gold_f(Role3, NeedGold, ?MONEY_DEC_TYPE_MAKE_LEGEND, SrcItemUID, LogStr),
    {Date, _} = Time = erlang:localtime(),
    if DelItem == [] andalso UpdateItem == [] ->
           ignore;
       true ->
           %% 写道具日志
%%            LogItemList = role_item:itemList2logItemList(DelItem, LogItemList),
           behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_MAKE_LEGEND, SrcItemUID, LogStr),
           role_data:set_bagItem(NewBagItem),
           DelItemIDList = [E||#item{itemUID=E}<-DelItem],
           UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem],
           %% 提醒客户端更新物品
           ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
           ?sendself(#sc_item_update{updateList=UpdateList})
    end,
    NewItem = item_lib:recacl(new_legend_equip(Item)),
    NewEquipList = [NewItem|EquipList2],
    if GerID =:= 0 ->
           EquipList = role_data:set_bagEquip(NewEquipList);
       true ->
           EquipList = role_data:set_equip(GerID,NewEquipList),
           role_lvlSgAttr:on_equip_change(GerID, EquipList),
           ger_attr:recacl_f(GerID)
    end,
    ?sendself(#sc_item_make_legend{result=1,newLegendItemUID=SrcItemUID,equipTypeID=NewItem#item.itemTypeID}).

new_legend_equip(Item)->
    Item#item{itemTypeID=Item#item.itemTypeID + ?LEGENDIDSHIFT}.

assort_ger_equiplist(EquipList)->
    lists:foldl(fun([_ItemUID,ItemTypeID,_ItemPos,_ItemLevel,_ItemRank,ItemGerID|T]=E,Acc)->
        case lists:keytake(ItemGerID,1,Acc) of
            false-> 
                [{ItemGerID,[E]}|Acc];
            {_Value,{ItemGerID,L},Other}->
                [{ItemGerID,[E|L]}|Other]
        end
    end,[],EquipList).

