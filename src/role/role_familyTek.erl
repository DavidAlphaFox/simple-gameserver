%% @author lixinglong
%% @doc @todo Add description to role_familyfight.


-module(role_familyTek).

-include("def_role.hrl").
-include("def_family.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

cs_familyTek_info(_) ->
	#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {family_extra,{cs_familyTek_info,RoleID}});
		false ->
			?sendself(#sc_familyTek_info{tekList=[]})
	end.

cs_familyTek_upLevel(#cs_familyTek_upLevel{tekID=TekID})->
	#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {family_extra,{cs_familyTek_upLevel,RoleID,TekID}});
		false ->
			?sendself(#sc_familyTek_upLevel{result=2,tekID=0,level=0})
	end.

cs_familyTek_wallet(#cs_familyTek_wallet{tekID = TekWalletID}) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID,{family_extra,{cs_familyTek_wallet,RoleID,TekWalletID}});
		false ->
			?sendself(#sc_familyTek_wallet{wallet= #p_reward_info2{coin =0,roleExp=0,gerExp=0,gold = 0,itemList=[],reputation=0,gerList=[]}})
	end.
cs_familyTek_cost(#cs_familyTek_cost{tekID =TekID,tekLevel=TekLevel})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case TekLevel > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID,{family_extra,{cs_familyTek_cost,TekID,TekLevel,RoleID}});
		false->
			?sendself(#sc_familyTek_cost{tekID= TekID,tekLevel= TekLevel,tekCost={tek_cost, 0, 0, 0, 0, [], 0, []}})
	end.

cs_familyTek_donate(#cs_familyTek_donate{itemtype=ItemType,itemTypeID =ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case check_donate(ItemType,ItemNum,ItemTypeID,TekID,TekLevel) of
		true ->
			% #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
			% if  ItemType == 4 orelse ItemType ==5 -> 
			% 		SellItemList = [Item],
			% 		LogItemList = role_item:itemList2logItemList(SellItemList, []),
			% 		{Date, _} = Time = erlang:localtime(),
			% 		behavior_ger_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "");
			% 	true ->
			% 		ignore
			% end,
			% role_data:set_bagEquip(NewBagEquip),
			% role_data:set_gerBag(NewBagItem),
			%%?sendself(#sc_item_sell{result=1,gold=SellP,reward=[#p_reward_view3{type=1, typeID=21100, num=SellP}]})
			family_misc:router_to_family_process(FamilyID,{family_extra,{cs_familyTek_donate,TekID,TekLevel,RoleID,ItemType,ItemNum,ItemTypeID}});
		{false, Reason} ->
			%?ERR("role_familyTek:check_donate_item() false ~w",[Reason]),
			?sendself(#sc_familyTek_donate{itemNum=ItemNum,itemTypeID=ItemTypeID,result=Reason,tekID= TekID,tekLevel=TekLevel,tekWallet=#p_reward_info2{coin=0,roleExp=0,gerExp=0,gold=0,reputation=0,itemList=[],gerList=[]}})
	end.
			
%% ====================================================================
%% Internal functions
%% ====================================================================
check_donate(ItemType,ItemNum,ItemTypeID,TekID,TekLevel)->
	%?ERR("ItemNum~w~n",[ItemNum]),
	case ItemNum == 0 of
		true ->
			{false,6};
		false ->
			check_donate2(ItemType,ItemNum,ItemTypeID,TekID,TekLevel)
	end.

check_donate2(ItemType,ItemNum,ItemTypeID,TekID,TekLevel) ->
Role = role_data:get_roleInfo(),
BagGer = role_data:get_gerBag(),
BagEquip = role_data:get_bagEquip(),
#role{coin=Coin,roleID=RoleID,exp=RoleExp,gold=Gold,reputation=Reputation,goldBonus=GoldBonus} = Role,
case ItemType of
	1 ->
		case Coin >= ItemNum of
			true->
				case check_is_in_cost(ItemType,ItemTypeID,TekID,TekLevel) of
					true ->
						% role_lib:deduct_coin_f(Role, ItemNum, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
						true;
					false ->
						{false,7}
				end;
			false ->
				{false,2}
		end;
	2->
		case Gold+GoldBonus >= ItemNum of
			true ->
				case check_is_in_cost(ItemType,ItemTypeID,TekID,TekLevel) of
					true ->
						% role_lib:deduct_gold_f(Role, ItemNum, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
						true;
					false ->
						{false,7}
				end;
			false ->
				{false,2}
		end;
	3->
		case Reputation >= ItemNum of	
			true ->
				case check_is_in_cost(ItemType,ItemTypeID,TekID,TekLevel) of
					true ->
						% role_lib:deduct_reputation_f(Role, ItemNum, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
						true;
					false ->
						{false,7}
				end;
			false ->
				{false,2}
		end;
	4->	
		case ItemNum of
			0->
				{false,6};
			_->
				case check_donate_item(ItemTypeID,ItemNum,BagEquip) of
					true ->
						case check_is_in_cost(ItemType,ItemTypeID,TekID,TekLevel) of
							true ->
								true;
							false ->
								{false,7}
						end;
					{false,Reason}->
						{false,Reason}
				end
		end;
	5->
		case ItemNum of
			0->
				{false,6};
			_->
				HomeSteadGerID = homestead_server:get_homestead_ger(role_data:get_roleID()),
				BagGer2 = lists:keydelete(HomeSteadGerID,#gerSimple.gerID,BagGer),
				case check_donate_ger(ItemTypeID,ItemNum,BagGer2) of
					true ->
						case check_is_in_cost(ItemType,ItemTypeID,TekID,TekLevel) of
							true ->
								true;
							false ->
								{false,7}
						end;
					{false,Reason}->
						{false,Reason}
				end
		end
end.
%%计算背包中TypeID为ItemTypeID的装备的数量是否大于ItemNum
check_donate_item(ItemTypeID,ItemNum,BagEquip) ->
	%?ERR("当前背包装备列表：~w ~n",[BagEquip]),
	ResultNum = lists:foldl(fun(Item,Acc) ->
		#item{itemTypeID=FindItemTypeID,itemPos=FindItemPos,itemRank=FindItemRank,itemNum=FindItemNum} = Item,
		case FindItemTypeID == ItemTypeID andalso FindItemPos == 0 andalso FindItemRank == 0 of
			true ->
				Acc+FindItemNum;
			false ->
				Acc
		end
		end,0,BagEquip),
	% ?ERR("ResultNum ~w NeedNum is ~w ~n",[ResultNum,ItemNum]),
	case ResultNum >= ItemNum of
		true ->
			true;
		false ->
			% ?ERR("背包中没有足够的装备用于捐献~n"),
			{false,2}
	end.
%%计算背包中TypeID为GerTypeID的武将的数量是否大于GerNum
check_donate_ger(GerTypeID,GerNum,BagGer) ->
	ResultNum = lists:foldl(fun(Ger,Acc) ->
		#gerSimple{gerTypeID=FindGerTypeID,gerPos=FindGerPos,gerQuality=FindGerQuality} = Ger,
		case FindGerTypeID == GerTypeID andalso FindGerPos == 0 andalso FindGerQuality == 0 of
			true ->
				Acc+1;
			false ->
				Acc
		end
		end,0,BagGer),
	case ResultNum >= GerNum of
		true ->
			true;
		false ->
			% ?ERR("背包中没有足够的精灵用于捐献~n"),
			{false,2}
	end. 

% get_item(ItemUID,ItemNum,BagItem,BagEquip)->
% 	case lists:keytake(ItemUID, 2, BagItem) of
% 		false ->
% 			case lists:keytake(ItemUID, 2, BagEquip) of
% 				false ->
% 					{false,2};
% 				{value, Item, BagEquip2}->

% 				 	FindItemNum = Item#item.itemNum,
% 					if 	FindItemNum > ItemNum ->
% 							Item#item{itemNum = FindItemNum-ItemNum},
% 				   			{true,Item,BagItem,[Item|BagEquip2]};
%              			FindItemNum == ItemNum ->
%              				{true,Item,BagItem,BagEquip2};
% 			   			true ->
% 				   			{false, 2}
% 				end
% 			end;
% 		{value, Item, BagItem2}->
% 			if  ItemNum == 1 ->
% 					{true,Item,BagItem2,BagEquip};
% 				true ->
% 					{false,2}
% 			end
% 	end.	
%%检查捐献的类型是否在科技升级需要的物品类型中
check_is_in_cost(ItemType,ItemTypeID,TekID,TekLevel) ->
	{Type,UnlockLevel,Level2,Cost2,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank,TekID}),
	case Cost2 of
		 {tek_cost,Coin, RoleExp,GerExp, Gold, ItemList, Reputation, GerList} ->
		 	case ItemType of
		 		 4-> 
		 			case lists:keyfind(ItemTypeID, 2, ItemList) of
		 				false ->
		 					false;
		 				_ ->
		 					true
		 			end; 
		 		5->
		 			case lists:keyfind(ItemTypeID,2,GerList) of
		 				false ->
		 					false;
		 				_->
		 					true
		 			end;
		 		1->
		 			if
		 				Coin =/= 0 ->
		 					true;
		 				true ->
                        	false
		 			end;
		 		2->
		 			if
		 				Gold =/= 0 ->
		 					true;
		 				true ->
		 					false
		 			end;
		 		3->
		 			if
		 				Reputation =/= 0 ->
		 					true;
		 				true ->
		 					false
		 			end
		 	end;
		 _ ->
		 	false
	end.

%%修改公会科技等级
test_familyTek_level(FamilyID,TekID,Level)->
	OldTekInfo = ets:lookup(?ETS_FAMILY_TECHNOLOGY, FamilyID),
	case OldTekInfo of
		[] ->
			?ERR("can not find OldTekInfo in ETS_FAMILY_TECHNOLOGY ~n");
		[{_,TekList}] ->
			TekWithOutLevelList = [{TekID2 div 1000,TekLevel}||{TekID2,TekLevel}<-TekList],
			case lists:keyfind(TekID,1,TekWithOutLevelList) of
				false ->
					?ERR("can not find TekInfo with ~w ~n",[TekID]);
				{TekIDWithOutLevel,OldLevel}->
					NewTekID = TekIDWithOutLevel*1000+OldLevel,
					family_misc:router_to_family_process(FamilyID,{fix_familyTek_level,FamilyID,NewTekID,Level})
			end
	end.