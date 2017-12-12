%% @author lixinglong
%% @doc @todo Add description to family_extra_handler.


-module(family_extra_handler).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").
-include("def_mail.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


handle_info({cs_familyTek_info,RoleID},_)->
	?unicast(RoleID,#sc_familyTek_info{tekList=family_data:get_family_tek()});
	
handle_info({cs_familyTek_upLevel,RoleID,TekID},#family_info{members=Members,level=FamilyLevel,family_id=FamilyID}=FamilyInfo)->
	%%do_upLevel_familyTek(RoleID,TekID,Members,FamilyLevel,FamilyID).
	ignore;

handle_info({family_add_rice, Type,Rice},_)->
	do_add_rice(Type,Rice);
	
handle_info({cs_family_storage_info,RoleID},#family_info{level=FamilyLevel})->
	{Len,Storage} = family_data:get_family_storage(),
	%#family_wallet{rice=Rice} = family_data:get_family_wallet(),
	MaxLen = data_family:get({max_storage_len,FamilyLevel}),
	?unicast(RoleID,#sc_family_storage_info{result=1,level=0,itemLen=Len,itemList=Storage,maxLen=MaxLen});



handle_info({family_boss_reward,Pos,Level,Rewards}, _)-> %Reward {#sell_reward{}, rice}
	%lists:foreach(fun({_,Reward})->do_reward_family_boss(Pos,Level,Reward)end,Rewards);
	do_reward_family_boss(Pos,Level,Rewards);

handle_info({cs_familyTek_wallet,RoleID,TekID},#family_info{members=Members,level=FamilyLevel,family_id=FamilyID}=FamilyInfo) ->
	do_get_family_wallet_info(RoleID,TekID,FamilyID);

handle_info({cs_familyTek_cost,TekID,TekLevel,RoleID},_) ->
	do_get_family_cost_info(RoleID,TekID,TekLevel);

handle_info({cs_familyTek_donate,TekID,TekLevel,RoleID,ItemType,ItemNum,ItemTypeID},#family_info{members=Members,level=FamilyLevel,family_id=FamilyID}=FamilyInfo) ->
	case check_is_in_tekList(TekID,TekLevel,FamilyID,FamilyLevel) of
		true ->	
			do_add_family_wallet(TekID,TekLevel,RoleID,ItemType,ItemNum,ItemTypeID,FamilyID,Members,FamilyLevel);
		{false,Reason} ->
			?ERR("donate Reason: ~w~n",[Reason]),
			#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
			?unicast(RoleID,#sc_familyTek_donate{result=Reason,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet})
	end;

handle_info(Info,_)->
	?ERR("can't handle family extra info:~w",[Info]),
	ok. 


%% ====================================================================
%% Internal functions
%% ====================================================================
do_add_family_wallet(TekID,TekLevel,RoleID,ItemType,ItemNum,ItemTypeID,FamilyID,Members,FamilyLevel) ->
	case check_donate_need(TekID,TekLevel,ItemType,ItemNum,ItemTypeID) of
		{all_donate,ItemNum2} ->
			% ?ERR("all_donate ~w",[ItemNum2]),
			case family_lib:add_family_wallet(TekID,TekLevel,RoleID,ItemType,ItemNum2,ItemTypeID,FamilyID,?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "") of
				true ->
					donate_back_to_role(RoleID,ItemType,0,ItemNum2,ItemTypeID),
					#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
					family_server:do_bc_msg_except(#sc_familyTek_donate{result=11,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet}, [RoleID]);
				false ->
					#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
					?unicast(RoleID,#sc_familyTek_donate{result=6,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet})
			end;	
		{part_donate,ItemNum3,ItemNum4} ->
			%?ERR("part_donate ~w,back ~w",[ItemNum3,ItemNum4]),
			case family_lib:add_family_wallet(TekID,TekLevel,RoleID,ItemType,ItemNum3,ItemTypeID,FamilyID,?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "") of
				true ->
					case do_upLevel_familyTek(TekID,FamilyLevel,FamilyID,Members) of
						true ->
							ignore;
						{false,Reason} ->
							#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
							?unicast(RoleID,#sc_familyTek_donate{result=8,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet})
					end,
					#sc_familyTek_wallet{wallet = Wallet2} = family_data:get_family_wallet(TekID),
					family_server:do_bc_msg_except(#sc_familyTek_donate{result=11,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet2},[RoleID]),
					donate_back_to_role(RoleID,ItemType,ItemNum4,ItemNum3,ItemTypeID);
				false ->
					#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
					?unicast(RoleID,#sc_familyTek_donate{result=6,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet})
			end;	
		{no_donate,ItemNum5} ->
			case do_upLevel_familyTek(TekID,FamilyLevel,FamilyID,Members) of
				true->
					ignore;
				{false,Reason} ->
					#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
					?unicast(RoleID,#sc_familyTek_donate{result=8,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet})
			end;
		{error,Reason} ->
			?ERR("Error ~w",[Reason]),
			#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
			?unicast(RoleID,#sc_familyTek_donate{result=5,itemTypeID=ItemTypeID,itemNum=ItemNum,tekID=TekID,tekLevel=TekLevel,tekWallet=Wallet})
	end.
do_get_family_cost_info(RoleID,TekID,TekLevel) ->
	Max_Family_Level = data_family_technology:get(max_family_level),
	Coefficient = data_family_technology:get(max_tek_level_coefficient),
	case TekLevel> Max_Family_Level*Coefficient of
		false -> 
			Cost = family_data:get_familytek_cost(TekID,TekLevel),
			?unicast(RoleID,#sc_familyTek_cost{tekID = TekID,tekLevel=TekLevel,tekCost = Cost});
		true ->
			?unicast(RoleID,#sc_familyTek_cost{tekID = TekID,tekLevel=TekLevel,tekCost = {tek_cost, 0, 0, 0, 0, [], 0, []}})
	end.
do_get_family_wallet_info(RoleID,TekID,FamilyID) ->
	ItemList = family_data:get_family_wallet(TekID),
	?unicast(RoleID,#sc_familyTek_wallet{wallet=ItemList}).

do_reward_family_boss(Pos,Level,RewardList)->
	{ItemList,Rice} = lists:foldl(fun({_,{SellReward,AddRice}},{ItemListAcc,RiceAcc})->
										  {sell_reward2p_family_storage(SellReward)++ItemListAcc,RiceAcc+AddRice}
								  end, {[],0}, RewardList),
	{Date,_}=Time=erlang:localtime(),
	FamilyID=family_data:get_familyID(),
	Wallet = family_data:get_family_wallet(),
	family_lib:add_family_wallet(rice, Rice,Wallet,FamilyID,?MONEY_ADD_TYPE_FAMILY_BOSS,Pos,integer_to_list(Level)),
	behavior_storage_add:log(FamilyID,storage_item2logstorage(ItemList),Date,Time,?MONEY_ADD_TYPE_FAMILY_BOSS,Pos,integer_to_list(Level)),
	{_,Storage}=family_data:get_family_storage(),
	family_data:set_family_storage(ItemList ++ Storage),
	{_Head,_Body,_Sp} = data_family:get(boss_reward),
	%#data_ger{gerName = BossName} = data_ger:get(family_boss_server:get_boss_typeID(Pos,Level)),
    
%% 此处是旧的记录公会活动，公会活动格式变了，boss的格式还没确定，此处暂时注释掉
%% 	AddLog = lists:foldr(fun(#p_family_storage{itemTypeID=TypeID},Acc)->
%% 								 #data_item{itemName=Name} = data_item:get(TypeID),
%% 								 Acc++Name++Sp
%% 								 end,Head++BossName++Body,ItemList),
%% 	family_lib:add_family_log(AddLog,FamilyID),
	lists:foreach(fun(#p_family_storage{itemUID=ItemUID,type=Type,itemTypeID=ItemTypeID})->
						  <<Info:32>> = <<ItemTypeID:16,Type:16>>,
						  family_server:do_bc_msg_except(#sc_family_storage_update{itemUID=ItemUID,type=1,reqRoleIDList=[Info]},[])
				  end, ItemList).

%% do_reward_family_boss(Pos,Level,{SellReward,Rice})->	
%% 	{Date,_}=Time=erlang:localtime(),
%% 	FamilyID=family_data:get_familyID(),
%% 	Wallet = family_data:get_family_wallet(),
%% 	family_lib:add_family_wallet(rice, Rice,Wallet,FamilyID,?MONEY_ADD_TYPE_FAMILY_BOSS,Pos,integer_to_list(Level)),
%% 	{_,Storage}=family_data:get_family_storage(),
%% 	ItemList = sell_reward2p_family_storage(SellReward),
%% 	behavior_storage_add:log(FamilyID,storage_item2logstorage(ItemList),Date,Time,?MONEY_ADD_TYPE_FAMILY_BOSS,Pos,integer_to_list(Level)),
%% 	family_data:set_family_storage(ItemList ++ Storage).


storage_item2logstorage(ItemList)->
	[[ItemUID,ItemTypeID]||#p_family_storage{itemUID=ItemUID,itemTypeID=ItemTypeID}<-ItemList].

sell_reward2p_family_storage(#sell_reward{item=ItemList,newGer=GerList})->
	ItemList2 = lists:foldl(fun(#new_item{itemTypeID=TypeID,itemNum=Num},Acc)->
									[#p_family_storage{itemUID=tk_id:gen_itemUID(),itemTypeID=TypeID,type=1}||_<-lists:duplicate(1,Num)]++Acc
							end,[],ItemList),
	lists:foldl(fun(#new_ger{gerTypeID=TypeID},Acc)->
						[#p_family_storage{itemUID=tk_id:gen_itemUID(),itemTypeID=TypeID,type=2}|Acc]
				end,ItemList2,GerList).


do_add_rice(Type,Rice)->
	FamilyID = family_data:get_familyID(),
	Wallet = family_data:get_family_wallet(),
	family_lib:add_family_wallet(rice, Rice,Wallet,FamilyID,Type,0,"").

do_familyTek_upLevel(Cost,Wallet,TekID,OtherTek,TekLevel,FamilyID,Members)->
	family_lib:deduct_family_wallet(rice,Cost,Wallet,family_data:get_familyID(),?MONEY_DEC_FAMILYTEK_UPLEVEL,TekID,integer_to_list(TekLevel+1)),
	% ?ERR("do_familyTek_upLevel : TekID: ~w",[TekID]),
	{Type,UnlockLevel,Level2,Cost2,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank,TekID+1}),
	NewTekInfo = [#p_familyTekDtl{tekID=TekID+1,tekLevel=Level2,tekType=Type,tekWalletinfo=#p_reward_info2{coin =0,roleExp=0,gerExp=0,gold = 0,itemList=[],reputation=0,gerList=[]} ,tekFinish=0}|OtherTek],
	family_data:set_family_tek(NewTekInfo),
	family_server:do_bc_msg_except(#sc_familyTek_upLevel{result=1, tekID=TekID+1,level=Level2}, []),
	TekList =family_data:get_family_tek(),
    %%将新的公会科技等级发送到family_manager_server的共享ETS表中，更新其中的结果。
	TekLevelList = lists:map(fun(TekDetail) ->
			#p_familyTekDtl{tekID = NewTekID ,tekLevel = NewTekLevel} = TekDetail,
			{NewTekID,NewTekLevel}
			end,TekList),
	OldTekInfo = ets:lookup(?ETS_FAMILY_TECHNOLOGY, FamilyID),
	%?ERR("the old Family:~w FamilyTekList is ~w ~n",[FamilyID,OldTekInfo]),
	ets:insert(?ETS_FAMILY_TECHNOLOGY,{FamilyID,TekLevelList}),
	spawn(fun()->update_member_tekAttr(Members)end).

cacl_tek_add(Level,TekAddList)->
	cacl_tek_add(Level,TekAddList,0).
cacl_tek_add(_,[],AddAcc) ->
	AddAcc;
cacl_tek_add(Level,[{Level,Add,_}|_],AddAcc) ->
	AddAcc + Add;
cacl_tek_add(Level,[{_,Add,_}|Tail],AddAcc)->
	cacl_tek_add(Level,Tail,AddAcc+Add);

cacl_tek_add(Level,_,AddAcc)->
	AddAcc.

update_member_tekAttr(Members)->
	lists:foreach(fun(#family_member_info{role_id=RoleID})->
						  update_role_tekInfo(RoleID)
				  end, Members).

 update_role_tekInfo(RoleID) ->
	case role_lib:is_online(RoleID) of
		true ->
			case catch role_lib:send_server(RoleID,{update_family_tekAdd}) of
				{'EXIT',_}->
					%db_sql:set_family_tekAdd(RoleID,NewTekInfo);
					ignore;
				_ ->
					ignore
			end;
		_ ->
			%db_sql:set_family_tekAdd(RoleID,NewTekInfo)
			ignore								  
	end.

check_familyTek_upLevel(TekID,Level,FamilyID)->
	case check_tek_islock(TekID,Level) of
		true ->
			{value,#p_familyTekDtl{tekID=TekID,tekLevel=TekLevel},OtherTek} = lists:keytake(TekID, #p_familyTekDtl.tekID, family_data:get_family_tek()),
			{Type,UnlockLevel,Level2,Cost,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank,TekID}),
			case family_lib:check_family_cost(Cost,TekID) of
				{true,Wallet} ->
					{true,Cost,Wallet,TekID,OtherTek,TekLevel};
				false ->
					{false,5}
			end;
		false ->
			{false,6}
	end.



check_tek_islock(TekID,Level) ->
 	{Type,UnlockLevel,Level2,Cost,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank,TekID+1}),
 	case UnlockLevel > Level of
 		true ->
 			false;
 		false ->
 			true
 	end.

%%计算科技捐献实际需要的ItemType的数量,防止玩家过量捐献
check_donate_need(TekID,TekLevel,ItemType,ItemNum,ItemTypeID) ->
	{Type,UnlockLevel,Level2,Cost,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank,TekID}),
	{tek_cost,NeedCoin , NeedRoleExp, NeedGerExp, NeedGold, NeedItemList, NeedReputation, NeedGerList} = Cost,
	#sc_familyTek_wallet{wallet = Wallet} = family_data:get_family_wallet(TekID),
	#p_reward_info2{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,reputation=Reputation,itemList=ItemList,gerList=GerList}=Wallet,
	case ItemType of
		%% 1 =>金币捐献
		1 ->
			RealNeedCoin = NeedCoin - Coin,
			if  RealNeedCoin > ItemNum ->
					{all_donate,ItemNum};
				RealNeedCoin =< 0 ->
					{no_donate,ItemNum};
				RealNeedCoin =< ItemNum ->
					{part_donate,RealNeedCoin,ItemNum-RealNeedCoin}
			end;
		%% 2 =>钻石捐献
		2 ->
			RealNeedGold = NeedGold - Gold,
			if  RealNeedGold > ItemNum ->
					{all_donate,ItemNum};
				RealNeedGold =< 0 ->
					{no_donate,ItemNum};
				RealNeedGold =< ItemNum ->
					{part_donate,RealNeedGold,ItemNum-RealNeedGold}
			end;
		%% 3 =>徽章捐献
		3 ->
			RealNeedReputation = NeedReputation - Reputation,
			if  RealNeedReputation > ItemNum ->
					{all_donate,ItemNum};
				RealNeedReputation =< 0 ->
					{no_donate,ItemNum};
				RealNeedReputation =< ItemNum ->
					{part_donate,RealNeedReputation,ItemNum-RealNeedReputation}
			end;
		%% 4 =>装备捐献
		4 ->
			case lists:keyfind(ItemTypeID,2, NeedItemList) of
				{_,FindItemTypeID,FindItemNeedNum,A,B} ->
					case lists:keyfind(ItemTypeID,2,ItemList) of
						{_,FindItemTypeID2,_,_,FindItemNeedNum2} ->
							RealNeedItemNum = FindItemNeedNum - FindItemNeedNum2,
							if RealNeedItemNum < 0 ->
								RealNeedItemNum2 = 0;
								true ->
									RealNeedItemNum2 = RealNeedItemNum
							end,
							% ?ERR("RealNeedItem Num is ~w Need ItemTypeID is ~w",[RealNeedItemNum2,ItemTypeID]),
							if  RealNeedItemNum2 > ItemNum ->
									{all_donate,ItemNum};
								RealNeedItemNum2 =< 0 ->
									{no_donate,ItemNum};
								RealNeedItemNum2 =< ItemNum ->
									{part_donate,RealNeedItemNum2,ItemNum-RealNeedItemNum2}
							end;
						false ->
							if 	FindItemNeedNum > ItemNum ->
									{all_donate,ItemNum};
								FindItemNeedNum =< ItemNum ->
									{part_donate,FindItemNeedNum,ItemNum-FindItemNeedNum}
							end
			        end;
				false ->
					{error,"捐献道具不在科技升级需求列表中~n"}
			end;
		%% 5 =>精灵捐献
		5 ->
			GerTypeID = ItemTypeID,
			case lists:keyfind(GerTypeID,2, NeedGerList) of
				{_,FindGerTypeID,FindGerNeedNum,A,B} ->
					case lists:keyfind(GerTypeID,2,GerList) of
						{_,FindGerTypeID2,_,_,FindGerNeedNum2} ->
							RealNeedGerNum = FindGerNeedNum - FindGerNeedNum2,
							if RealNeedGerNum < 0 ->
									RealNeedGerNum2 = 0;
								true ->
									RealNeedGerNum2 = RealNeedGerNum
							end,
							% ?ERR("FindGerNeedNum ~w, FindGerNeedNum2 ~w RealNeedGer Num is ~w Need GerTypeID is ~w",[FindGerNeedNum,FindGerNeedNum2,RealNeedGerNum,GerTypeID]),
							if  RealNeedGerNum2 > ItemNum ->
									{all_donate,ItemNum};
								RealNeedGerNum2 =< 0 ->
									{no_donate,ItemNum};
								RealNeedGerNum2 =< ItemNum ->
									{part_donate,RealNeedGerNum2,ItemNum-RealNeedGerNum2}
							end;
						false ->
							if 	FindGerNeedNum > ItemNum ->
									{all_donate,ItemNum};
								FindGerNeedNum =< ItemNum ->
									{part_donate,FindGerNeedNum,ItemNum-FindGerNeedNum}
							end
			        end;
				false ->
					{error,"捐献精灵不在科技升级需求列表中~n"}
			end
	end.
%%向玩家进程发送扣除道具的信息
donate_back_to_role(RoleID,DonateType,BackNum,DonateNum,ItemTypeID) ->
	family_server:update_contribute_record(RoleID,DonateType,DonateNum,ItemTypeID),
	case catch role_lib:send_server(RoleID, {familyTek_donate_back, DonateType,BackNum,DonateNum,ItemTypeID}) of
				{'EXIT',_}->
					db_sql:set_family_donate_back(RoleID,DonateType,BackNum,DonateNum,ItemTypeID);
				_ ->
					ignore							  
	end.

%%完成公会科技升级
do_upLevel_familyTek(TekID,FamilyLevel,FamilyID,Members)->
	case check_familyTek_upLevel(TekID,FamilyLevel,FamilyID) of
		{true,NextCost,Wallet,TekID,OtherTek,TekLevel} ->
			do_familyTek_upLevel(NextCost,Wallet,TekID,OtherTek,TekLevel,FamilyID,Members),
			true;
		{false,Reason}->
			%?unicast(RoleID, #sc_familyTek_upLevel{result=Reason,tekID=TekID,level=TekID rem 1000})
			{false,Reason}
	end.
%%检查当前捐献科技是否是公会当前科技
check_is_in_tekList(TekID,TekLevel,FamilyID,FamilyLevel)->
	FamilyTekList = ets:lookup(?ETS_FAMILY_TECHNOLOGY,FamilyID),
	%?ERR("FamilyTekList: ~w FamilyID :~w ~n",[FamilyTekList,FamilyID]),
	case FamilyTekList of
		[] ->
			{false,5};
		_ ->
			%?ERR("FamilyTekList: ~w TekID ~w ~n",[FamilyTekList,TekID]),
			[{_,FamilyTekInfoList}] = FamilyTekList, 
			case lists:keyfind(TekID,1,FamilyTekInfoList) of
				false ->
					{false,6};
				{FamilyTekID,FamilyTekLevel} ->
					if
						FamilyTekLevel == TekLevel ->
							case check_next_tek_is_unlock(TekID,TekLevel,FamilyID,FamilyLevel) of
								true ->
									true;
								{false,Reason} ->
									{false,Reason}
							end;
						true ->
							{false,6}
					end
			end
	end.
	% {Type,UnlockLevel,Level2,Cost,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank, ID2}),

%%公会科技捐献时，判断当前公会等级是否能够捐献（通过判断下一级科技解锁的公会等级与当前等级是否符合配置）
check_next_tek_is_unlock(TekID,TekLevel,FamilyID,FamilyLevel)->
	FamilyTekLevelLimitList = data_family:get(tekLevelLimit),
	case lists:keyfind(TekID div 1000,1,FamilyTekLevelLimitList) of
		{TekIDWithOutLevel,LimitLevel} ->
			if  TekLevel>=LimitLevel ->
					{false,9};
				true ->
					{Type,UnlockLevel,Level2,Cost,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank,TekID+1}),
					if  UnlockLevel>FamilyLevel ->
							{false,10};
						true ->
							true
					end
			end;
		false ->
			{false,6}
	end.
	

check_role_can_uplevel_tek(1)->
	true;
check_role_can_uplevel_tek(0)->
	{_,PowerList} = data_family:get(familyMemberSetting),
	lists:member(11,PowerList);
check_role_can_uplevel_tek(2)->
	{_,PowerList} = data_family:get(familyDeputySetting),
	lists:member(11,PowerList);
check_role_can_uplevel_tek(_)->
	false.

reward_family_rice(FamilyID,Rice)->
	family_misc:router_to_family_process(FamilyID, {family_extra,{family_add_rice,?MONEY_ADD_TYPE_REWARD_FAMILY_RICE,Rice}}).

% add_item()
