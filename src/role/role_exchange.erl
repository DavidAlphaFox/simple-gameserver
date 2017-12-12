%% @author caohongyang
%% @doc 兑换活动
%% Created 2013-6-21


-module(role_exchange).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
%%新增加对condition是sell_reward情况下的处理，
handle_exchange(RoleID, ActivityID, DrawID, Condition, Reward) when is_record(Condition,sell_reward)->
	GerList = role_data:get_gerBag(),
	ItemList = role_data:get_bagItem(),
	EquipList = role_data:get_bagEquip(),
	case role_item:delete_sell_reward(Condition,[],[],GerList,ItemList,EquipList) of
		false->
			?sendself(#sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2});
		{true,NewItemList,NewGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
			role_item:update_role_info2(NewItemList,NewGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_EXCHANGE,ActivityID,integer_to_list(DrawID)),
			%%发放奖励
			Role = role_data:get_roleInfo(),
			role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_EXCHANGE, ActivityID, integer_to_list(DrawID)),
			activity_server:finish_exchange_condition(RoleID, ActivityID, DrawID)
	end;
handle_exchange(RoleID, ActivityID, DrawID, Condition, Reward) ->
	case check_condition(Condition) of
		{true, Role, DeductCoin, DeductGold, DeductRepu,GerBag2,BagEquip2, BagItem2, DelGer2, DelItem2, UpdateItem2, UpdateItemLogList} ->
			{Date, _} = Time = erlang:localtime(),
			if DeductCoin > 0 ->
				   Role2 = role_lib:deduct_coin_f(Role, DeductCoin, ?MONEY_DEC_TYPE_EXCHANGE, ActivityID, "");
			   true ->
				   Role2 = Role
			end,
			if DeductGold > 0 ->
				   Role3 = role_lib:deduct_gold_2_f(Role2, DeductGold, ?MONEY_DEC_TYPE_EXCHANGE, ActivityID, "",role_reward:transform2normal_reward(Reward));
			   true ->
				   Role3 = Role2
			end,
			if DeductRepu > 0 ->
				   Role4 = role_lib:deduct_reputation_f(Role3, DeductRepu, ?MONEY_DEC_TYPE_EXCHANGE, ActivityID, "");
			   true ->
				   Role4 = Role3
			end,
			if DelGer2 == [] ->
				   ignore;
			   true ->
				   LogGerList= [ [GerID,GerSimple#gerSimple.gerTypeID,GerSimple#gerSimple.gerLevel,GerSimple#gerSimple.gerQuality]  || #gerSimple{gerID=GerID}=GerSimple<- DelGer2],
				   
				   behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_EXCHANGE, ActivityID, ""),
				   role_data:set_gerBag(GerBag2),
				   DelGerIDList = [E||#gerSimple{gerID=E} <- DelGer2],
				   ?sendself(#sc_ger_del{gerIDList=DelGerIDList})
			end,
			if DelItem2 == [] andalso UpdateItem2 == [] ->
				   ignore;
			   true ->
				   %% 写道具日志
				   LogItemList = role_item:itemList2logItemList(DelItem2, UpdateItemLogList),
				   behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_EXCHANGE, ActivityID, ""),
				   role_data:set_bagEquip(BagEquip2),
				   role_data:set_bagItem(BagItem2),
				   DelItemIDList = [E||#item{itemUID=E}<-DelItem2],
				   UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem2],
				   ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
				   ?sendself(#sc_item_update{updateList=UpdateList})
			end,
			role_reward:handle_sell_reward_f(Role4, Reward, ?MONEY_ADD_TYPE_EXCHANGE, ActivityID, ""),
			activity_server:finish_exchange_condition(RoleID, ActivityID, DrawID);		
		{false,Reason} ->
			?sendself(#sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=Reason})
			end.
	
			
%% ====================================================================
%% Internal functions
%% ====================================================================
check_condition(Condition) ->
	#role{coin=Coin,gold=Gold,goldBonus=GoldBonus,reputation=Repu} = Role = role_data:get_roleInfo(),
	GoldInit = Gold+GoldBonus,
	GerBag = role_data:get_gerBag(),
	BagEquip = role_data:get_bagEquip(),
	BagItem = role_data:get_bagItem(),
	Result = 
	util:foldl(fun({?REWARD_COIN,NeedCoin}, {CoinAcc, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc}) ->
					   if CoinAcc >= NeedCoin ->
							  {CoinAcc-NeedCoin, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc};
						  true ->
							  {return, false}
					   end;
				  ({?REWARD_GOLD,NeedGold}, {CoinAcc, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc}) ->
					   if GoldAcc >= NeedGold ->
							  {CoinAcc, GoldAcc-NeedGold, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc};
						  true ->
							  {return, false}
					   end;
				  ({?REWARD_REPU,NeedRepu}, {CoinAcc, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc}) ->
					   if RepuAcc >= NeedRepu ->
							  {CoinAcc, GoldAcc, RepuAcc-NeedRepu, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc};
						  true ->
							  {return, false}
					   end;
				  ({?REWARD_GER,GerTypeID,Num}, {CoinAcc, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc}) ->
					   case ger_lib:check_ger_material(GerBagAcc, GerTypeID, Num) of
						   false ->
							   {return, false};
						   {true, GerBagAcc2, DelGerList} ->
							   {CoinAcc, GoldAcc, RepuAcc, GerBagAcc2, BagEquipAcc, BagItemAcc, DelGerList++DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc}
					   end;
				  ({?REWARD_ITEM,ItemTypeID,Num}, {CoinAcc, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc, DelGerAcc, DelItemAcc, UpdateItemAcc, UpdateItemLogAcc}) ->
					   #data_item{itemType=ItemType} = data_item:get(ItemTypeID),
					   case item_lib:is_main_equip(ItemType) of
						   true ->
							   case item_lib:check_equip_num(BagEquipAcc, ItemTypeID, Num) of
								   false ->
									   {return, false};
								   {true, BagEquipAcc2, DelEquipList} ->
									   {CoinAcc, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc2, BagItemAcc, DelGerAcc, DelEquipList++DelItemAcc, UpdateItemAcc, UpdateItemLogAcc}
							   end;
						   false ->
							   case item_lib:check_material2(BagItemAcc, ItemTypeID, Num) of
								   {BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateLogList} ->
									   {CoinAcc, GoldAcc, RepuAcc, GerBagAcc, BagEquipAcc, BagItemAcc2, DelGerAcc, DelItemList++DelItemAcc, UpdateItemList++UpdateItemAcc, UpdateLogList++UpdateItemLogAcc};
								   _ ->
									   {return, false}
							   end
					   end
			   end, {Coin, GoldInit, Repu, GerBag, BagEquip, BagItem, [], [], [], []}, Condition),
	case Result of
		false ->
			{false, 2};
		{Coin2, Gold2, Repu2, GerBag2, BagEquip2, BagItem2, DelGer2, DelItem2, UpdateItem2, UpdateItemLogList} ->
			{true, Role, Coin-Coin2, GoldInit-Gold2, Repu-Repu2,GerBag2,BagEquip2, BagItem2, DelGer2, DelItem2, UpdateItem2, UpdateItemLogList}
	end.
									   
									   
%% do_deduct( {true, Role, DeductCoin, DeductGold, DeductRepu,GerBag2,BagEquip2, BagItem2, DelGer2, DelItem2, UpdateItem2, UpdateItemLogList},DecType)->
%% 	{Date, _} = Time = erlang:localtime(),
%% 	if DeductCoin > 0 ->
%% 		   Role2 = role_lib:deduct_coin_f(Role, DeductCoin, DecType, ActivityID, "");
%% 	   true ->
%% 		   Role2 = Role
%% 	end,
%% 	if DeductGold > 0 ->
%% 		   Role3 = role_lib:deduct_gold_f(Role2, DeductGold, DecType, ActivityID, "");
%% 	   true ->
%% 		   Role3 = Role2
%% 	end,
%% 	if DeductRepu > 0 ->
%% 		   Role4 = role_lib:deduct_reputation_f(Role3, DeductRepu, DecType, ActivityID, "");
%% 	   true ->
%% 		   Role4 = Role3
%% 	end,
%% 	if DelGer2 == [] ->
%% 		   ignore;
%% 	   true ->
%% 		   LogGerList= [ [GerID,GerSimple#gerSimple.gerTypeID,GerSimple#gerSimple.gerLevel,GerSimple#gerSimple.gerQuality]  || #gerSimple{gerID=GerID}=GerSimple<- DelGer2],
%% 		   
%% 		   behavior_ger_consume:log(RoleID, LogGerList, Date, Time, DecType, ActivityID, ""),
%% 		   role_data:set_gerBag(GerBag2),
%% 		   DelGerIDList = [E||#gerSimple{gerID=E} <- DelGer2],
%% 		   ?sendself(#sc_ger_del{gerIDList=DelGerIDList})
%% 	end,
%% 	if DelItem2 == [] andalso UpdateItem2 == [] ->
%% 		   ignore;
%% 	   true ->
%% 		   %% 写道具日志
%% 		   LogItemList = role_item:itemList2logItemList(DelItem2, UpdateItemLogList),
%% 		   behavior_item_consume:log(RoleID, LogItemList, Date, Time, DecType, ActivityID, ""),
%% 		   role_data:set_bagEquip(BagEquip2),
%% 		   role_data:set_bagItem(BagItem2),
%% 		   DelItemIDList = [E||#item{itemUID=E}<-DelItem2],
%% 		   UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem2],
%% 		   ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
%% 		   ?sendself(#sc_item_update{updateList=UpdateList})
%% 	end.
%% 	
%% 
