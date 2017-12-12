%% @author lixinglong
%% @doc @todo Add description to family_lib.


-module(family_lib).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


check_family_cost(Cost,WalletID)->
	#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(WalletID),
	%?ERR("Wallet :~w ~n",[Wallet]),
	#p_reward_info2{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,reputation=Reputation,itemList=ItemList,gerList=GerList}=Wallet,
	{tek_cost,NeedCoin , NeedRoleExp, NeedGerExp, NeedGold, NeedItemList, NeedReputation, NeedGerList} = Cost,
	Result = Coin>=NeedCoin andalso RoleExp>= NeedRoleExp andalso GerExp>=NeedGerExp andalso Gold>=NeedGold andalso Reputation>=NeedReputation,
	case Result of
	    true ->
	 		case check_item_list(ItemList,NeedItemList) of
	 			{true,NewItemList} ->
	 				%?ERR("Item check success NewItemList ~w",[NewItemList]),
	 				case check_ger_list(GerList,NeedGerList) of
	 					{true,NewGerList} ->
	 						Wallet2=#p_reward_info2{coin=Coin-NeedCoin,roleExp=RoleExp-NeedRoleExp,gerExp=GerExp-NeedGerExp,gold=Gold-NeedGold,reputation=Reputation-NeedReputation,itemList=NewItemList,gerList=NewGerList},
	 						%?ERR("wallet ~w",[Wallet]),
	 						{true,Wallet2};
	 					false ->
	 						%?ERR("Ger not enough"),
	 						false
	 				end;
	 			false ->
	 				%?ERR("Item not enough"),
	 				false
	 		end;
	 	false ->
	 		%?ERR("not enough ~w,~w,~w,~w,~w,~w,~w,~w,~w,~w",[NeedCoin,NeedRoleExp,NeedGerExp,NeedGold,NeedReputation,Coin,RoleExp,GerExp,Gold,Reputation]),
	 		false 
	end. 


deduct_family_wallet(rice,Cost,Wallet,FamilyID,Type,ArgID,Desc)->
	Wallet2 = #sc_familyTek_wallet{wallet= Wallet},
	{Date, _} = Time = erlang:localtime(),
	behavior_familytek_source_consume:log(FamilyID, Cost, Wallet, Date, Time, Type, ArgID, Desc),
	WalletID = ArgID,
	%?ERR("WalletID :~w",[WalletID]),
	family_data:set_family_wallet(WalletID,Wallet2),
	family_server:do_bc_msg_except(#sc_familyTek_wallet{wallet= Wallet},[]),
	Wallet2.

%%向wallet中添加对应的物品
add_family_wallet(TekID,TekLevel,RoleID,ItemType,ItemNum,ItemTypeID,FamilyID,Type,ArgID,Desc)->
	% ?ERR("TekID is ~w,TekLevel:~w,Item:~w,FamilyID:~w,ItemType ~w,ItemNum~w,ItemUID ~w",[TekID,TekLevel,Item,FamilyID,ItemType,ItemNum,ItemUID]),
	
	#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
	% ?ERR("get wallet ~w",[Wallet]),
	#p_reward_info2{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,reputation=Reputation,itemList=ItemList,gerList=GerList}=Wallet,
	case ItemType of
		1 ->
			Wallet2 = #sc_familyTek_wallet{wallet=Wallet#p_reward_info2{coin=Coin+ItemNum}},
			#sc_familyTek_wallet{wallet = Info} = Wallet2,
			family_data:set_family_wallet(TekID,Wallet2),
			?unicast(RoleID,#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Info}),
			%family_server:do_bc_msg_except(#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Info},[]),
			true;
		2 ->
			Wallet2 = #sc_familyTek_wallet{wallet=Wallet#p_reward_info2{gold=Gold+ItemNum}},
			#sc_familyTek_wallet{wallet = Info} = Wallet2,
			family_data:set_family_wallet(TekID,Wallet2),
			?unicast(RoleID,#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Info}),
			%family_server:do_bc_msg_except(#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Info},[]),
			true;
		3 ->
			Wallet2 = #sc_familyTek_wallet{wallet=Wallet#p_reward_info2{reputation=Reputation+ItemNum}},
			#sc_familyTek_wallet{wallet = Info} = Wallet2,
			family_data:set_family_wallet(TekID,Wallet2),
			?unicast(RoleID,#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Info}),
			%family_server:do_bc_msg_except(#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Info},[]),
			true;
		4 ->
			Wallet2 = add_item(Wallet,ItemTypeID,ItemNum),
			Wallet3 = #sc_familyTek_wallet{wallet = Wallet2},
			family_data:set_family_wallet(TekID,Wallet3),
			?unicast(RoleID,#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet2}),
			%family_server:do_bc_msg_except(#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet2},[]),
			true;
		5 ->
			Wallet2 = add_ger(Wallet,ItemTypeID,ItemNum),
			Wallet3 = #sc_familyTek_wallet{wallet = Wallet2},
			family_data:set_family_wallet(TekID,Wallet3),
			?unicast(RoleID,#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet2}),
			%family_server:do_bc_msg_except(#sc_familyTek_donate{result=1,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet2},[]),
			% ?ERR("after TekID ~w,Wallet:~w",[TekID,family_data:get_family_wallet(TekID)]),
			true;
		_ -> false
	end.
%%向科技对应wallet中添加装备
add_item(Wallet,ItemTypeID,ItemNum)->
		#p_reward_info2{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,reputation=Reputation,itemList=ItemList,gerList=GerList}=Wallet,
		case lists:keytake(ItemTypeID,#p_item_view.itemTypeID,ItemList) of
			{_,Item2,Other} ->
				% ?ERR("Find Item ~w",[Item2]),
				#p_item_view{itemNum = FindItemNum} = Item2,
				ItemList2 = [Item2#p_item_view{itemNum = FindItemNum + ItemNum}|Other],
				Wallet#p_reward_info2{itemList=ItemList2};
			false ->
				ItemList3 = [#p_item_view{itemTypeID = ItemTypeID,itemNum = ItemNum,itemLevel=0,itemRank=0}|ItemList],
				Wallet#p_reward_info2{itemList=ItemList3}
		end.
%%向科技对应wallet中添加精灵
add_ger(Wallet,GerTypeID,ItemNum)->
		#p_reward_info2{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,reputation=Reputation,itemList=ItemList,gerList=GerList}=Wallet,
		case lists:keytake(GerTypeID,#p_ger_view2.gerTypeID,GerList) of
			{_,Ger,Other} ->
				% ?ERR("Find Ger ~w",[Ger]),
				#p_ger_view2{gerTypeID= FindGerTypeID,gerNum=FindGerNum} = Ger,
				GerList3 = Ger#p_ger_view2{gerNum=FindGerNum+ItemNum},
				Wallet#p_reward_info2{gerList = [GerList3|Other]};
			false ->
				GerList3 = #p_ger_view2{gerTypeID= GerTypeID,gerQuality=1,gerLevel=1,gerNum=ItemNum},
				Wallet#p_reward_info2{gerList = [GerList3|GerList]}
		end.
%%检查wallet中装备需求列表是否满足升级科技条件
check_item_list(List,NeedList) ->
	{FindResultList1,ItemResultList1} =lists:foldl(fun(Item,{FindList,ResultList})->
		{item,ItemTypeID,ItemNum,_,_} = Item,
		case lists:keytake(ItemTypeID, 2, ResultList) of
			{_,FindItem={A,FindItemTypeID,B,C,FindItemNum},Other} ->
				if
					FindItemNum > ItemNum ->
						FindItem2 = {A,FindItemTypeID,B,C,FindItemNum-ItemNum},
						{FindList,[FindItem2|Other]};
					FindItemNum == ItemNum ->
						{FindList,Other};
					true->
						{[false|FindList],ResultList}								
				end;
			false ->
				{[false|FindList],ResultList}
		end
	end,{[],List},NeedList),
	if
		length(FindResultList1) == 0 ->
			{true,ItemResultList1};
			true ->
				false
	end.
%%检查wallet中精灵需求列表是否满足升级科技条件
check_ger_list(List,NeedList) ->
		{FindResultList1,ItemResultList1} =lists:foldl(fun(Item,{FindList,ResultList})->
				{ger,GerTypeID, GerNum, _,_} = Item,
				% ?ERR("ResultList :~w FindList ~w,GerTypeID ~w",[ResultList,FindList,GerTypeID]),
				case lists:keytake(GerTypeID, 2, ResultList) of
					{_,FindItem={A,FindItemTypeID,B,C,FindItemNum},Other} ->
						if
							FindItemNum > GerNum ->
								FindItem2 ={A,FindItemTypeID,B,C,FindItemNum-GerNum},
								{FindList,[FindItem2|Other]};
							FindItemNum == GerNum ->
								{FindList,Other};
							true ->
								{[false|FindList],ResultList}								
						end;
					false ->
						{[false|FindList],ResultList}
				end
				end,{[],List},NeedList),
		if
			length(FindResultList1) == 0 ->
				{true,ItemResultList1};
			true ->
				false
		end.

%% add_family_log(AddLog,FamilyID)->
%% 	family_misc:router_to_family_process(FamilyID, {add_family_log,AddLog}).

