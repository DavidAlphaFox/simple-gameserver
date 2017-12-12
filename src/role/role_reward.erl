%% @author caohongyang
%% @doc 处理奖励接口
%% Created 2013-3-7


-module(role_reward).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_item.hrl").
%% API functions
-export([
		]).


%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

typed_reward_transform_normal(RewardList)->
	transform2normal_reward( [typed_reward_transform_normal(Type,Value,Count)||{Type,Value,Count}<-RewardList]).
typed_reward_transform_normal(1,Count,Value) ->
	{7,Value,Count};
typed_reward_transform_normal(2,Count,Value)->
	{6,Value,Count};
typed_reward_transform_normal(3,Value,Count) ->
	{2,Value*Count};
typed_reward_transform_normal(4,Value,Count)->
	{1,Value*Count};
typed_reward_transform_normal(5,Value,Count) ->
	{3,Value*Count};
typed_reward_transform_normal(6,Value,Count)->
	{4,Value*Count}.


%% 点将系统的奖励
handle_card_reward_f(1, GerTypeID) ->
	ger_lib:add_ger(#new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(2, ItemTypeID) ->
	item_lib:add_white_item_f(ItemTypeID, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(3, Gold) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_gold_f(Role, Gold, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(4, Coin) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_coin_f(Role, Coin, ?MONEY_ADD_TYPE_DRAW_CARD,0,"");
handle_card_reward_f(5, Reputation) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_reputation_f(Role, Reputation, ?MONEY_ADD_TYPE_DRAW_CARD,0,"").

%% 精灵宝藏的奖励
handle_treaHouse_reward_f(1, GerTypeID, Type, Count) ->
	#data_ger{gerStar=Star} = data_ger:get(GerTypeID), 
	if Star >= 5 ->
		   #role{roleName=RoleName} = role_data:get_roleInfo(),
		   GerView=#p_ger_view{gerQuality=0, gerLevel=1, gerTypeID=GerTypeID},
		   broadcast_server:bc_msgID(10045, [RoleName, GerView, erlang:integer_to_list(Count)]);
	   true ->
		   ignore
	end,
    ger_lib:add_ger_list(lists:duplicate(Count, #new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}), ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE, Type, "");
handle_treaHouse_reward_f(2, ItemTypeID, Type, Count) ->
	#data_item{itemStar=Star} = data_item:get(ItemTypeID),
    %% 精灵球和装备神石是5星物品，但是不进行广播。这些特例可能会是坑
    case Star >= 5 andalso not lists:member(ItemTypeID, [20001, 20002]) of
        true ->
		   #role{roleName=RoleName} = role_data:get_roleInfo(),
		   ItemView=#p_item_view{itemTypeID=ItemTypeID, itemLevel=1, itemRank=0, itemNum=1},
		   broadcast_server:bc_msgID(10046, [RoleName,ItemView ,erlang:integer_to_list(Count)]);
	   _ ->
		   ignore
	end,
    lists:foreach(fun(_) ->
	                item_lib:add_white_item_f(ItemTypeID, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE, Type, "")
                  end, lists:seq(1, Count));
handle_treaHouse_reward_f(3, Gold, Type, Count) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_gold_f(Role, Gold * Count, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE, Type, "");
handle_treaHouse_reward_f(4, Coin, Type, Count) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_coin_f(Role, Coin * Count, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE, Type, "");
handle_treaHouse_reward_f(5, Reputation, Type, Count) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_reputation_f(Role, Reputation * Count, ?MONEY_ADD_TYPE_TREAHOUSE_EXPLORE, Type, "");
handle_treaHouse_reward_f(6, Exp, _Type, Count) ->
	Role = role_data:get_roleInfo(),
    case role_lib:add_exp(Role, Exp * Count) of
    {level_up, Level2, Exp2} ->
        Role1 = role_lib:hook_level_up(Role, Level2),
	    ?notify_update(?ra_exp(Exp2)),
	    ?notify_update(?ra_level(Level2)),
        %再加一级天赋点数，是因为下面的方法取到，还是升级前的等级
        ?INFO("new Level2 ~w",[Level2]),
        ?notify_update(?ra_remains_point(role_talent:get_remains_point()+data_trainer:get(point_per_level)));
	{level_not_up, _Level2, Exp2} ->
        Role1 = Role,
	    ?notify_update(?ra_exp(Exp2));
	{level_max,_Level2,_Exp2} ->
	    Role1 = Role
    end,
    role_data:set_roleInfo(Role1).
	
% 幸运转盘奖励
handle_lucky_roll_reward_f(1, GerTypeID, Type, Count) ->
    ger_lib:add_ger_list(lists:duplicate(Count, #new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID}), ?MONEY_ADD_TYPE_LUCKY_ROLL_EXPLORE, Type, "");
handle_lucky_roll_reward_f(2, ItemTypeID, Type, Count) ->
    lists:foreach(fun(_) ->
	                item_lib:add_white_item_f(ItemTypeID, ?MONEY_ADD_TYPE_LUCKY_ROLL_EXPLORE, Type, "")
                  end, lists:seq(1, Count));
handle_lucky_roll_reward_f(3, Gold, Type, Count) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_gold_f(Role, Gold * Count, ?MONEY_ADD_TYPE_LUCKY_ROLL_EXPLORE, Type, "");
handle_lucky_roll_reward_f(4, Coin, Type, Count) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_coin_f(Role, Coin * Count, ?MONEY_ADD_TYPE_LUCKY_ROLL_EXPLORE, Type, "");
handle_lucky_roll_reward_f(5, Reputation, Type, Count) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_reputation_f(Role, Reputation * Count, ?MONEY_ADD_TYPE_LUCKY_ROLL_EXPLORE, Type, "");
handle_lucky_roll_reward_f(6, Exp, _Type, Count) ->
	Role = role_data:get_roleInfo(),
    case role_lib:add_exp(Role, Exp * Count) of
    {level_up, Level2, Exp2} ->
        Role1 = role_lib:hook_level_up(Role, Level2),
	    ?notify_update(?ra_exp(Exp2)),
	    ?notify_update(?ra_level(Level2)),
        %再加一级天赋点数，是因为下面的方法取到，还是升级前的等级
        ?INFO("new Level2 ~w",[Level2]),
        ?notify_update(?ra_remains_point(role_talent:get_remains_point()+data_trainer:get(point_per_level)));
	{level_not_up, _Level2, Exp2} ->
        Role1 = Role,
	    ?notify_update(?ra_exp(Exp2));
	{level_max,_Level2,_Exp2} ->
	    Role1 = Role
    end,
    role_data:set_roleInfo(Role1).

calc_maxLevel_exp(Exp,Level) when Level < 10 ->
	Exp;
calc_maxLevel_exp(Exp, Level)->
	{LvX,Diff} = data_common:get(world_lv),
	MaxLv = tk_id:exlv_get_maxLevel(),
	Diff2 = MaxLv - Level,
	if Diff2 > Diff ->
		   trunc(Exp*(1+Diff2/LvX));
	   true ->
		   Exp
	end.

%% @doc 给玩家发送奖励
handle_dungeon_reward_f(#role{level=Level}=Role, Reward, Type, ArgID, Desc) ->
	#reward{coin=AddCoin0,gerExp=AddGerExp,roleExp=AddRoleExp0,gold=AddGold,dropList=DropList,reputation=AddRepu}=Reward,
    {_AddRoleExp,_AddCoin,LevelExp} = if 
        Type =:= ?MONEY_ADD_TYPE_BATTLE ->  %% 是冒险配型，需要检查是否处于奖励倍增活动中
            {CoinIncrementRate,ExpIncrementRate} = getIncrementRate(Role#role.level),
			AddRoleExp1 = trunc(AddRoleExp0*ExpIncrementRate),
			AddCoin1 = trunc(AddCoin0*CoinIncrementRate),
			% %% 计算公会科技对产出金币与经验的影响
    		#role{familyID = FamilyID} = role_data:get_roleInfo(),
    		FamilyTek_Battleout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,9,1),
    		% ?ERR("FamilyTek_Battleout_Add ~w~n",[FamilyTek_Battleout_Add]),
    		AddCoin2 = role_lib:calculate_familyTekeffectGenerate(AddCoin1,FamilyTek_Battleout_Add),
    		AddExp2_0 = role_lib:calculate_familyTekeffectGenerate(AddRoleExp1,FamilyTek_Battleout_Add),
			AddExp2 = calc_maxLevel_exp(AddExp2_0,Level),
            {AddExp2,AddCoin2,AddExp2-AddExp2_0};
        true ->
        	% %% 计算公会科技对产出金币与经验的影响
    		#role{familyID = FamilyID} = role_data:get_roleInfo(),
    		FamilyTek_Battleout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,9,1),
    		% ?ERR("FamilyTek_Battleout_Add ~w~n",[FamilyTek_Battleout_Add]),
    		AddCoin2 = role_lib:calculate_familyTekeffectGenerate(AddCoin0,FamilyTek_Battleout_Add),
    		AddExp2 = role_lib:calculate_familyTekeffectGenerate(AddRoleExp0,FamilyTek_Battleout_Add),
            {AddExp2,AddCoin2,0}
    end,
	{Role2, GerAddExpList} = reward_normal(Role, AddCoin2, AddGold,  AddGerExp, AddExp2, AddRepu, Type, ArgID, Desc),
    %% DropList是data_drop ID的列表
    %% ArgID 是 DungeonID 
	RandomSelect = random_drop(DropList),
    RandomSelectBonus = getDropBonus(ArgID),
	{RewardItemList, RewardGerList} = partition_drop(RandomSelectBonus++RandomSelect),
	Role3 = handle_item_f(Role2, RewardItemList, Type, ArgID, Desc),
	handle_ger_f(RewardGerList, Type, ArgID, Desc),
	role_data:set_roleInfo(Role3),
	{Role3, GerAddExpList, RewardItemList, RewardGerList,LevelExp}.

handle_dungeon_reward_f2(#role{level=Level}=Role, N,Reward, Type, ArgID, Desc) ->
	#reward{coin=AddCoin0,gerExp=AddGerExp,roleExp=AddRoleExp0,gold=AddGold,dropList=DropList,reputation=AddRepu}=Reward,
    {_AddRoleExp,_AddCoin,LevelExp} = if 
        Type =:= ?MONEY_ADD_TYPE_BATTLE ->  %% 是冒险配型，需要检查是否处于奖励倍增活动中
            {CoinIncrementRate,ExpIncrementRate} = getIncrementRate(Role#role.level),
			AddRoleExp1 = trunc(AddRoleExp0*ExpIncrementRate),
			AddCoin1 = trunc(AddCoin0*CoinIncrementRate),
			% %% 计算公会科技对产出金币与经验的影响
    		#role{familyID = FamilyID} = role_data:get_roleInfo(),
    		FamilyTek_Battleout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,9,1),
    		% ?ERR("FamilyTek_Battleout_Add ~w~n",[FamilyTek_Battleout_Add]),
    		AddCoin2 = role_lib:calculate_familyTekeffectGenerate(AddCoin1,FamilyTek_Battleout_Add),
    		AddExp2_0 = role_lib:calculate_familyTekeffectGenerate(AddRoleExp1,FamilyTek_Battleout_Add),
			AddExp2 = calc_maxLevel_exp(AddExp2_0,Level),
            {AddExp2,AddCoin2,AddExp2-AddExp2_0};
        true ->
        	% %% 计算公会科技对产出金币与经验的影响
    		#role{familyID = FamilyID} = role_data:get_roleInfo(),
    		FamilyTek_Battleout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,9,1),
    		% ?ERR("FamilyTek_Battleout_Add ~w~n",[FamilyTek_Battleout_Add]),
    		AddCoin2 = role_lib:calculate_familyTekeffectGenerate(AddCoin0,FamilyTek_Battleout_Add),
    		AddExp2 = role_lib:calculate_familyTekeffectGenerate(AddRoleExp0,FamilyTek_Battleout_Add),
            {AddExp2,AddCoin2,0}
    end,
	{Role2, GerAddExpList} = reward_normal(Role, AddCoin2*N, AddGold*N,  AddGerExp*N, AddExp2*N, AddRepu*N, Type, ArgID, Desc),
    %% DropList是data_drop ID的列表
    %% ArgID 是 DungeonID 
%	RandomSelect = random_drop(DropList),
%    RandomSelectBonus = getDropBonus(ArgID),
	{RandomSelect,RandomSelectBonus} = lists:foldl(fun(_,{L1,L2})-> {random_drop(DropList)++L1,getDropBonus(ArgID)++L2} end, {[],[]}, lists:seq(1,N)),
	{RewardItemList, RewardGerList} = partition_drop(RandomSelectBonus++RandomSelect),
	Role3 = handle_item_f(Role2, RewardItemList, Type, ArgID, Desc),
	handle_ger_f(RewardGerList, Type, ArgID, Desc),
	role_data:set_roleInfo(Role3),
	{Role3, GerAddExpList, RewardItemList, RewardGerList,LevelExp}.

handle_sell_reward_f(Role, Reward, Type, ArgID, Desc) ->
	#sell_reward{coin=AddCoin,gerExp=AddGerExp,gold=AddGold,item=ItemList,reputation=AddReputation,roleExp=AddRoleExp,newGer=NewGer} = Reward,
	{AddProfoundCryStal,AddHonor,AddHomeResource,AddPvpPoint,NewItemList} = lists:foldl(fun(Item,{ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc,ItemListAcc})->
		if is_record(Item,new_item) ->
    			case Item#new_item.itemTypeID of
    		   		?ITEM_TYPE_PROFOUNDCRYSTAL ->
    		   			?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item.itemNum]),
    		   			{ProfoundCryStalAcc+Item#new_item.itemNum,HonorAcc,HomeResourceAcc,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_HONOR ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc+Item#new_item.itemNum,HomeResourceAcc,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_PVPPOINT ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc+Item#new_item.itemNum,ItemListAcc};
                    ?ITEM_TYPE_HOME_RESOURCE ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc+Item#new_item.itemNum,PvpPointAcc,ItemListAcc};
                    _ ->
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc,[Item|ItemListAcc]}
                end;
		   true->
		   		?INFO("sellreward的item列表中出现非new_item配置：Item:~w ~n",[Item]),
		   		{ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc,ItemListAcc}
		end
	end,{0,0,0,0,[]},ItemList),
	?INFO("ProfoundCryStal:~w ~n",[AddProfoundCryStal]),
    %%充值的时候传递过来的Role数据中包含了是否领取了双倍的标志，所以需要保存一次
    role_data:set_roleInfo(Role),
	case AddProfoundCryStal > 0 of
		true ->
			role_lib:add_profoundCrystal_f(Role, AddProfoundCryStal, Type, ArgID, Desc);
		false->
            ignore
	end,
    case AddHonor > 0 of
        true ->
            role_lib:add_honor_f(role_data:get_roleInfo(), AddHonor, Type, ArgID, Desc);
        false->
            ignore            
    end,
    case AddHomeResource > 0 of
        true ->
            role_lib:add_home_resource_f(role_data:get_roleInfo(), AddHomeResource, Type, ArgID, Desc);
        false->
            ignore            
    end,
    case AddPvpPoint > 0 of
        true ->
            role_lib:add_pvppoint_f(role_data:get_roleInfo(), AddHonor, Type, ArgID, Desc);
        false->
            ignore            
    end,
	NewRole = role_data:get_roleInfo(),
	{Role2, _GerAddExpList} = reward_normal(NewRole, AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation, Type, ArgID, Desc),
	handle_ger_f(NewGer, Type, ArgID, Desc),
	Role3 = handle_item_f(Role2, NewItemList, Type, ArgID, Desc),
	role_data:set_roleInfo(Role3).

handle_daily_reward_f(Role, Reward, Type, ArgID, Desc) ->
	#daily_reward{coin=AddCoin,gold=AddGold,item=ItemList,newGer=NewGer,reputation=AddReputation, vip=Vip} = Reward,
	{Role2, _GerAddExpList} = reward_normal(Role, AddCoin, AddGold,  0, 0, AddReputation, Type, ArgID, Desc),
	Role3=handle_role_vip_f(Role2, Vip),
	handle_ger_f(NewGer, Type, ArgID, Desc),
	Role4 = handle_item_f(Role3, ItemList, Type, ArgID, Desc),
	role_data:set_roleInfo(Role4),
	Role3.
  
%% 领取系统奖励
handle_sys_reward(Role, Reward, Type, ArgID, Desc) ->
	?INFO("reward:~w ~n",[Reward]),
	RewardTuple = transform2normal_reward(Reward),
	?INFO("Length:(~w) ~w ~n ",[tuple_size(RewardTuple),RewardTuple]),
    {AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer} = RewardTuple,
	{Role2, GerExp} = reward_normal(Role, AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation
                                   ,ProfoundCryStal,Honor,HomeResource,PvpPoint, Type, ArgID, Desc,true),
	handle_ger_f(NewGer, Type, ArgID, Desc),
	Role3 = handle_item_f(Role2, NewItem, Type, ArgID, Desc),
    role_data:set_roleInfo(Role3),
	{Role3,GerExp}.

handle_sys_reward_with_return(Role,Reward,Type,ArgID,Desc) ->
    handle_sys_reward_with_return(Role,Reward,Type,ArgID,Desc,true). 

handle_sys_reward_with_return(Role,Reward,Type,ArgID,Desc,NeedTrigerP) ->
	RewardTuple = transform2normal_reward(Reward),
	?INFO("Length:~w ~n ",[tuple_size(RewardTuple)]),
    {AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer} = RewardTuple,
	{Role2, _GerExp} = reward_normal(Role, AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation
                                    , ProfoundCryStal, Honor,HomeResource,PvpPoint,Type, ArgID, Desc, NeedTrigerP),
	role_data:set_roleInfo(Role2),
	GenerateGer = handle_ger_f(NewGer, Type, ArgID, Desc,NeedTrigerP),
	BagEquip1 = role_data:get_bagEquip(),
	role_data:get_bagItem(),
	Role3 = handle_item_f(Role2, NewItem, Type, ArgID, Desc, NeedTrigerP),
	role_data:set_roleInfo(Role3),
	BagEquip2 = role_data:get_bagEquip(),
	BagOther2= role_data:get_bagItem(),
	%%此处将货币类道具提出直接记录到道具消耗列表中，防止在下面的处理过程中直接进入道具中而出现再次分解不能扣除的情况
	{SpecailCoin,NewItem1} = role_item:special_coin(NewItem),
	{IsOk,Result1} = get_real_bagitem_reward(BagOther2,NewItem1),
	Result2 = BagEquip2 -- BagEquip1,
	%%此处判断是否所有获得的道具都已经记录完成，如果没有记录完成，直接加入一个false来导致再次分解失败
	case IsOk of
		true->
			{AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,ProfoundCryStal,Honor,GenerateGer,Result1++Result2++SpecailCoin};
		false->
			{AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,ProfoundCryStal,Honor,GenerateGer,Result1++Result2++[false]++SpecailCoin}
	end.


handle_role_vip_f(Role, Vip)->
    if Role#role.vipLevel < Vip ->
           VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(Vip),
           RoleTimes=role_data:get_roleTimes(),
           VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(Vip),
           NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(Role#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
           RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
                                            challengeGodBuyTimes=VipChallengeGodBuyTimes
                                           },
           role_data:set_roleTimes(RoleTimes2),
           ?notify_update(?ra_vipLevel(Vip, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
           MaxDscv = role_lib:get_max_dscv_times(Vip),
           MaxEnergy = role_lib:get_max_energy(Vip),
           ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
           Role#role{vipLevel=Vip};
       true ->
           Role
    end.
%%添加不触发成就的加精灵
handle_ger_f(NewGer,Type,ArgID,Desc) ->
	handle_ger_f(NewGer,Type,ArgID,Desc,true).

handle_ger_f(0, _Type, _ArgID, _Desc,_NeedTriger) ->
	ignore;
handle_ger_f([], _Type, _ArgID, _Desc,_NeedTriger) ->
	ignore;
handle_ger_f(NewGer, Type, ArgID, Desc,NeedTriger) -> 
	ger_lib:add_ger_list(NewGer, Type, ArgID, Desc,NeedTriger).
	
handle_item_f(Role,ItemList,Type,ArgID,Desc)->
	handle_item_f(Role,ItemList,Type,ArgID,Desc,true).

handle_item_f(Role, 0, _Type, _ArgID, _Desc,_NeedTriger) ->
	Role;
handle_item_f(Role, [], _Type, _ArgID, _Desc,_NeedTriger) ->
	Role;
handle_item_f(Role, ItemList, Type, ArgID, Desc,NeedTrigerP) ->
    {Date, _} = Time = erlang:localtime(),
    {NewRole,NewItemList} = 
        lists:foldr(fun(Item,{AccRole,AccItemList})->
                            case Item of
                                #new_item{itemTypeID = 20027,itemNum = AddUnioncoin}->
                                    NewUnioncoin = AddUnioncoin + AccRole#role.unioncoin,
                                    behavior_unioncoin_add:log(AccRole#role.roleID, AccRole#role.vipLevel, AddUnioncoin, AccRole#role.unioncoin, Date, Time, Type, ArgID, Desc),
                                    ?notify_update(?ra_unioncoin(NewUnioncoin)),
                                    {AccRole#role{unioncoin=NewUnioncoin},AccItemList};
								#new_item2{itemTypeID = 20027,itemNum = AddUnioncoin}->
                                    NewUnioncoin = AddUnioncoin + AccRole#role.unioncoin,
                                    behavior_unioncoin_add:log(AccRole#role.roleID, AccRole#role.vipLevel, AddUnioncoin, AccRole#role.unioncoin, Date, Time, Type, ArgID, Desc),
                                    ?notify_update(?ra_unioncoin(NewUnioncoin)),
                                    {AccRole#role{unioncoin=NewUnioncoin},AccItemList};
								#new_item{itemTypeID = 20050,itemNum = AddTicket} ->
									NewTicket = AddTicket + AccRole#role.ticket,
									behavior_ticket_add:log(AccRole#role.roleID, AccRole#role.vipLevel,AddTicket,AccRole#role.ticket,Date, Time, Type, ArgID, Desc),
									?notify_update(?ra_ticket(NewTicket)),
									{AccRole#role{ticket=NewTicket},AccItemList};
								#new_item2{itemTypeID = 20050,itemNum = AddTicket} ->
									NewTicket = AddTicket + AccRole#role.ticket,
									behavior_ticket_add:log(AccRole#role.roleID, AccRole#role.vipLevel,AddTicket,AccRole#role.ticket,Date, Time, Type, ArgID, Desc),
									?notify_update(?ra_ticket(NewTicket)),
									{AccRole#role{ticket=NewTicket},AccItemList};
								#new_item{itemTypeID = 20051,itemNum = AddLaputaStone} ->
									NewLaputaStone = AddLaputaStone + AccRole#role.laputastone,
									behavior_laputastone_add:log(AccRole#role.roleID,AddLaputaStone,AccRole#role.laputastone,Date, Time, Type, ArgID, Desc),
									?notify_update(?ra_laputastone(NewLaputaStone)),
									{AccRole#role{laputastone=NewLaputaStone},AccItemList};
								#new_item2{itemTypeID = 20051,itemNum = AddLaputaStone} ->
									NewLaputaStone = AddLaputaStone + AccRole#role.laputastone,
									behavior_laputastone_add:log(AccRole#role.roleID,AddLaputaStone,AccRole#role.laputastone,Date, Time, Type, ArgID, Desc),
									?notify_update(?ra_laputastone(NewLaputaStone)),
									{AccRole#role{laputastone=NewLaputaStone},AccItemList};
                                _ ->
                                    {AccRole,[Item|AccItemList]}
                            end
                        end, {Role,[]}, ItemList),
	?INFO("ItemList: ~w Type: ~w ArgID:~w Desc:~w ~n",[NewItemList,Type,ArgID,Desc]),
	item_lib:add_item_f(NewItemList, Type, ArgID, Desc, NeedTrigerP),
	NewRole.

%旧的reward_normal函数少量个参数，做一个适配
reward_normal(Role,AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,Type,ArgID,Desc)-> % len:9
    reward_normal(Role,AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,0,0,0,0,Type,ArgID,Desc,true).

%% reward_normal(Role, AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal, Honor,PvpPoint,Type, ArgID, Desc) ->
%%     reward_normal(Role, AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal, Honor,0,PvpPoint,Type, ArgID, Desc,true).


%% reward_normal(Role
%%              ,AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,ProfoundCryStal,Honor,PvpPoint
%%              ,Type,ArgID,Desc, NeedTrigerP) ->
%%     reward_normal(Role
%%              ,AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,ProfoundCryStal,Honor,0,PvpPoint
%%              ,Type,ArgID,Desc, NeedTrigerP).

reward_normal(Role
             ,AddCoin,AddGold,AddGerExp,AddRoleExp,Reputation,ProfoundCryStal,Honor,HomeResource,PvpPoint
             ,Type,ArgID,Desc, NeedTrigerP) ->
	{Date, _} = Time = erlang:localtime(),
	%% 主公加经验
	if AddRoleExp == 0 ->
		   #role{level=Level2,exp=Exp2}=Role,
		   Role1 = Role;
	   true ->
		   case role_lib:add_exp(Role, AddRoleExp) of
			   {level_up, Level2, Exp2} -> % 可能出现连续升级的情况
                   Role1 = 
                       lists:foldl(fun(NextLevel,AccRole)-> 
                                           role_lib:hook_level_up(AccRole, NextLevel)
                                   end, Role, lists:seq(Role#role.level + 1, Level2)),
%% 				   Role1 = role_lib:hook_level_up(Role, Level2),
				   ?notify_update(?ra_exp(Exp2)),
				   ?notify_update(?ra_level(Level2)),
                    %再加一级天赋点数，是因为下面的方法取到，还是升级前的等级
                    ?INFO("new Level2 ~w",[Level2]),
                    ?notify_update(?ra_remains_point(role_talent:get_remains_point()+data_trainer:get(point_per_level)));
			   {level_not_up, Level2, Exp2} ->
				   Role1 = Role,
				   ?notify_update(?ra_exp(Exp2));
			   {level_max,Level2,Exp2} ->
				   Role1 = Role
		   end
	end,
	#role{vipLevel=VipLevel, roleID=RoleID, coin=CurCoin, goldBonus=CurGoldBonus
         ,reputation=Repu,profoundCrystal=Prof,honor=Hon,home_resource=HomeRes,pvppoint=Pp}=Role1,
	%% 加钱
	if AddCoin > 0 ->
		   Coin2 = CurCoin + AddCoin,
           case NeedTrigerP of
                true ->
		            ?CATCH_1( role_task_trigger:handle({dispach_task,role_add_reward,AddCoin,?REWARD_COIN}),E1);
                _ ->
                    next
           end,
		   behavior_coin_add:log(RoleID, VipLevel, AddCoin, CurCoin, Date, Time, Type, ArgID, Desc),
		   ?notify_update(?ra_coin(Coin2));
	   true ->
		   Coin2 = CurCoin
	end,
	%% 加元宝
	if AddGold > 0 ->
		   GoldBonus2 = CurGoldBonus+ AddGold,
		   behavior_gold_bonus_add:log(RoleID, VipLevel, AddGold, CurGoldBonus, Date, Time, Type, ArgID, Desc),
           case NeedTrigerP of
                true ->
		            ?CATCH_1(role_task_trigger:handle({dispach_task,role_add_reward,AddGold,?REWARD_GOLD}),E2);
                _ ->
                    next
           end,
		   ?notify_update(?ra_goldBonus(GoldBonus2));
	   true ->
		   GoldBonus2 = CurGoldBonus
	end,
	%% 加声望
	if Reputation > 0 ->
		   Reputation2 = Repu + Reputation,
		   behavior_repu_add:log(RoleID, VipLevel, Reputation, Repu, Date, Time, Type, ArgID, Desc),
           case NeedTrigerP of
                true ->
		            ?CATCH_1(role_task_trigger:handle({dispach_task,role_add_reward,Reputation,?REWARD_REPU}),E3);
                _ ->
                    next
            end,
		   ?notify_update(?ra_reputation(Reputation2));
	   true ->
		   Reputation2 = Repu
	end,
    %% 加奥义水晶
    if ProfoundCryStal > 0 ->
           ProfoundCryStal2 = Prof + ProfoundCryStal,
           behavior_profoundCrystal_add:log(RoleID, VipLevel, ProfoundCryStal, Prof, Date, Time, Type, ArgID, Desc),
           ?notify_update(?ra_profoundCrystal(ProfoundCryStal2));
       true ->
           ProfoundCryStal2 = Prof
    end,
    %% 加荣誉
    if Honor > 0 ->
           Honor2 = Hon + Honor,
           behavior_honor_add:log(RoleID, VipLevel, Honor, Hon, Date, Time, Type, ArgID, Desc),
           ?notify_update(?ra_honor(Honor2));
       true ->
           Honor2 = Hon
    end,
    %% 加建筑资源
    if HomeResource > 0 ->
           HomeResource2 = HomeRes + HomeResource,
           behavior_home_resource_add:log(RoleID, VipLevel, HomeResource, HomeRes, Date, Time, Type, ArgID, Desc),
           ?notify_update(?ra_home_resource(HomeResource2));
       true ->
           HomeResource2 = HomeRes
    end,
    %% 加竞技场点数
    if PvpPoint > 0 ->
           PvpPoint2 = Pp + PvpPoint,
           behavior_pvppoint_add:log(RoleID, VipLevel, PvpPoint, Pp, Date, Time, Type, ArgID, Desc),
           ?notify_update(?ra_pvppoint(PvpPoint2));
       true ->
           PvpPoint2 = Pp
    end,
	%% 出战武将加经验
	if AddGerExp == 0 ->
		   GerAddExpList =[];
	   true ->
%% 		   io:format("exp:~w",[AddGerExp]),
		   PosList = role_data:get_posList(),
		   {GerAddExpList,GerList} = lists:foldl(fun(Ger,{EAcc, GAcc}) ->
														 %% 以新的主公等级来计算武将经验
														 #ger{gerBase=#gerBase{gerPos=GerPos}} = Ger,
														 {IsLevelUpgraded, NewGer, RealAddExp} = ger_lib:add_exp_and_notify(Ger, AddGerExp, Level2,PosList),
														 {[#p_ger_add_exp{gerPos=GerPos, addExp=RealAddExp, isUpgraded=IsLevelUpgraded}|EAcc], [NewGer|GAcc]}
												 end, {[],[]}, PosList),
		   role_data:set_posList(GerList)
	end,
	
	Role2 = Role1#role{coin=Coin2,goldBonus=GoldBonus2,level=Level2,exp=Exp2,reputation=Reputation2
                      ,profoundCrystal=ProfoundCryStal2,honor=Honor2,home_resource=HomeResource2,pvppoint=PvpPoint2},
	{Role2, GerAddExpList}.

transform2normal_reward(#sell_reward{coin=AddCoin,gerExp=AddGerExp,gold=AddGold,item=ItemList,reputation=AddReputation,roleExp=AddRoleExp,newGer=NewGer}) ->
	{AddProfoundCryStal,AddHonor,AddHomeResource,AddPvpPoint,NewItemList} = lists:foldl(fun(Item,{ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc,ItemListAcc})->
		if 
            is_record(Item,new_item) ->
                case Item#new_item.itemTypeID of
    				?ITEM_TYPE_PROFOUNDCRYSTAL ->
                        ?INFO("transform2normal_reward~w ~w ~n",[Item,Item#new_item.itemNum]),
                        {ProfoundCryStalAcc+Item#new_item.itemNum,HonorAcc,HomeResourceAcc,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_HONOR ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc+Item#new_item.itemNum,HomeResourceAcc,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_HOME_RESOURCE ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc+Item#new_item.itemNum,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_PVPPOINT ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc+Item#new_item.itemNum,ItemListAcc};
    		   		_ ->
    		   			?INFO("Item:~w ~n",[Item]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc,[Item|ItemListAcc]}
                end;
            is_record(Item,new_item2)->
            	% {ProfoundCryStalAcc,HonorAcc,PvpPointAcc,[Item|ItemListAcc]};
            	case Item#new_item2.itemTypeID of
    				?ITEM_TYPE_PROFOUNDCRYSTAL ->
                        ?INFO("transform2normal_reward ~w ~w ~n",[Item,Item#new_item2.itemNum]),
                        {ProfoundCryStalAcc+Item#new_item2.itemNum,HonorAcc,HomeResourceAcc,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_HONOR ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item2.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc+Item#new_item2.itemNum,HomeResourceAcc,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_HOME_RESOURCE ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item2.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc+Item#new_item2.itemNum,PvpPointAcc,ItemListAcc};
                    ?ITEM_TYPE_PVPPOINT ->
                        ?INFO("handle_sell_reward_f add:~w(~w) ~n",[Item,Item#new_item2.itemNum]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc+Item#new_item2.itemNum,ItemListAcc};
    		   		_ ->
    		   			?INFO("Item:~w ~n",[Item]),
                        {ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc,[Item|ItemListAcc]}
                end;
            true->
		   		?INFO("sellreward的item列表中出现非new_item配置：Item:~w ~n",[Item]),
		   		{ProfoundCryStalAcc,HonorAcc,HomeResourceAcc,PvpPointAcc,ItemListAcc}
		end
	end,{0,0,0,0,[]},ItemList),
	{AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation,AddProfoundCryStal,AddHonor,AddHomeResource,AddPvpPoint,NewItemList,NewGer};
transform2normal_reward(Reward) ->
    ?INFO("transform2normal_reward ~w",[Reward]),
	transform2normal_reward(Reward, 0, 0, 0, 0, 0, 0, 0, 0, 0,[], []).


transform2normal_reward([],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	{AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer};
transform2normal_reward([{?REWARD_GOLD,Gold}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold+Gold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_COIN,Coin}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin+Coin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_REPU,Repu}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation+Repu, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_ITEM,?ITEM_TYPE_PROFOUNDCRYSTAL,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal+Num,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_ITEM,?ITEM_TYPE_HONOR,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor+Num,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_ITEM,?ITEM_TYPE_HOME_RESOURCE,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource+Num,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_ITEM,?ITEM_TYPE_PVPPOINT,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint+Num,NewItem, NewGer);
transform2normal_reward([{?REWARD_ITEM,ItemTypeID,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,[#new_item{itemLevel=1,itemNum=Num,itemRank=0,itemTypeID=ItemTypeID}|NewItem], NewGer);
transform2normal_reward([{?REWARD_GER,GerTypeID,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	Ger = #new_ger{gerLevel=1,gerQuality=0,gerTypeID=GerTypeID},
	AddGer = lists:duplicate(Num, Ger),
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, AddGer++NewGer);
transform2normal_reward([{?REWARD_GER_EXP,GerExp}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation,ProfoundCryStal,Honor,HomeResource,PvpPoint, NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp+GerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_ROLE_EXP,RoleExp}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp+RoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_PROFOUNDCRYSTAL,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal+Num,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_PROFOUNDCRYSTAL,_,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal+Num,Honor,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_HONOR,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor+Num,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_HONOR,_,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor+Num,HomeResource,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_HOME_RESOURCE,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource+Num,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_HOME_RESOURCE,_,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource+Num,PvpPoint,NewItem, NewGer);
transform2normal_reward([{?REWARD_PVPPOINT,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint+Num,NewItem, NewGer);
transform2normal_reward([{?REWARD_PVPPOINT,_,Num}|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer)->
    transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint+Num,NewItem, NewGer);
transform2normal_reward([#new_item{}=E|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation,ProfoundCryStal,Honor,HomeResource,PvpPoint, [E|NewItem], NewGer);
transform2normal_reward([#new_ger{}=E|List],AddCoin, AddGold,  AddGerExp, AddRoleExp, Reputation, ProfoundCryStal,Honor,HomeResource,PvpPoint,NewItem, NewGer) ->
	transform2normal_reward(List, AddCoin,AddGold, AddGerExp, AddRoleExp, Reputation,ProfoundCryStal,Honor,HomeResource,PvpPoint, NewItem, [E|NewGer]).

transform2p_mail_reward([]) ->
	#p_mail_reward{gerList=[],itemList=[]};
transform2p_mail_reward(#sell_reward{coin=Coin,gerExp=GerExp,gold=Gold,item=Item1,newGer=NewGer,reputation=Reputation,roleExp=RoleExp}) ->
	Item = case is_list(Item1) of
		true->
			[item_lib:tranfor_newitem22newitem(E)||E<-Item1];
		false->
			item_lib:tranfor_newitem22newitem(Item1)
	end,
	if is_record(Item, new_item) ->
		   Item3 = [#p_id_num{num=Item#new_item.itemNum,typeID=Item#new_item.itemTypeID}];
	   is_list(Item) ->		
		   %Item3 = [#p_id_num{num=A,typeID=B}||#new_item{itemNum=A,itemTypeID=B}<-Item];
		   Item3 = lists:foldl(fun(#new_item{itemNum=A, itemTypeID=B}, Acc)->
									   case lists:keytake(B, #p_id_num.typeID, Acc) of
										   false ->
											   [#p_id_num{num=A,typeID=B}|Acc];
										   {value, #p_id_num{num=Num}=Info, Acc2}->
											   [Info#p_id_num{num=A+Num}|Acc2]
									   end
							   end, [], Item);
	   true ->
		   Item3 = []
	end,
	
	if Coin > 0 ->
		   Item4 = [#p_id_num{num=Coin,typeID=20007}|Item3];
	   true ->
		   Item4 = Item3
	end,
	if Gold > 0 ->
		   Item5 = [#p_id_num{num=Gold,typeID=20008}|Item4];
	   true ->
		   Item5 = Item4
	end,
	if Reputation > 0 ->
		   Item6 = [#p_id_num{num=Reputation,typeID=20006}|Item5];
	   true ->
		   Item6 = Item5
	end,
	if RoleExp > 0 ->
		   Item7 = [#p_id_num{num=RoleExp,typeID=20005}|Item6];
	   true ->
		   Item7 = Item6
	end,
	if GerExp > 0 ->
		   Item8 = [#p_id_num{num=GerExp,typeID=20004}|Item7];
	   true ->
		   Item8 = Item7
	end,
	if is_record(NewGer, new_ger) ->
		   Ger3 = [#p_id_num{num=1,typeID=NewGer#new_ger.gerTypeID}];
	   is_list(NewGer) ->
		   %Ger3 = [#p_id_num{num=1,typeID=A}||#new_ger{gerTypeID=A}<-NewGer];
		   Ger3 = lists:foldl(fun(#new_ger{gerTypeID=GerTypeID}, Acc)->
							   case lists:keytake(GerTypeID, #p_id_num.typeID, Acc) of
								   false ->
									   [#p_id_num{num=1, typeID=GerTypeID}|Acc];
								   {value, #p_id_num{num=Num}=Info, Acc2} ->
									   [Info#p_id_num{num=Num+1}|Acc2]
							   end
					   end, [], NewGer);
	   true ->
		   Ger3 = []
	end,
	#p_mail_reward{gerList=Ger3,itemList=Item8};		   
transform2p_mail_reward(Reward) ->
	{ItemList, GerList} = transform2p_mail_reward(Reward, [], []),
	#p_mail_reward{gerList=GerList,itemList=ItemList}.

transform2p_mail_reward([], ItemList, GerList) ->
	{ItemList,GerList};
transform2p_mail_reward([{?REWARD_GOLD,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=20008}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_COIN,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=20007}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_REPU,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=20006}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_ROLE_EXP,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=20005}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_GER_EXP,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=20004}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_ITEM,TypeID,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, [#p_id_num{num=Gold,typeID=TypeID}|ItemList],GerList);
transform2p_mail_reward([{?REWARD_GER,TypeID,Gold}|List], ItemList, GerList) ->
	transform2p_mail_reward(List, ItemList, [#p_id_num{num=Gold,typeID=TypeID}|GerList]);
transform2p_mail_reward([#new_item{itemTypeID=TypeID,itemNum=Num}|List], ItemList, GerList) ->
	case lists:keytake(TypeID, #new_item.itemTypeID, ItemList) of
		false ->
			transform2p_mail_reward(List, [#p_id_num{num=Num,typeID=TypeID}|ItemList],GerList);
		{value, #p_id_num{num=NumA}=Info, ItemList2} ->
			transform2p_mail_reward(List, [Info#p_id_num{num=Num+NumA}|ItemList2], GerList)
	end;
transform2p_mail_reward([#new_ger{gerTypeID=TypeID}|List], ItemList, GerList) ->
	case lists:keytake(TypeID, #new_ger.gerTypeID, GerList) of
		false ->
			transform2p_mail_reward(List, ItemList, [#p_id_num{num=1,typeID=TypeID}|GerList]);
		{value, #p_id_num{num=Num}=Info, GerList2} ->
			transform2p_mail_reward(List, ItemList, [Info#p_id_num{num=Num+1}|GerList2])
	end.

transform2p_reward_view([], List) ->
	List;
transform2p_reward_view([{?REWARD_GOLD,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_GOLD,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_COIN,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_COIN,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_REPU,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_REPU,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_PROFOUNDCRYSTAL,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ITEM,value=?ITEM_TYPE_PROFOUNDCRYSTAL bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_PROFOUNDCRYSTAL,_,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ITEM,value=?ITEM_TYPE_PROFOUNDCRYSTAL bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_HONOR,Gold}|List], RewardViewList) ->
    transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ITEM,value=?ITEM_TYPE_HONOR bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_HONOR,_,Gold}|List], RewardViewList) ->
    transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ITEM,value=?ITEM_TYPE_HONOR bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_HOME_RESOURCE,Gold}|List], RewardViewList) ->
    transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ITEM,value=?ITEM_TYPE_HOME_RESOURCE bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_HOME_RESOURCE,_,Gold}|List], RewardViewList) ->
    transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ITEM,value=?ITEM_TYPE_HOME_RESOURCE bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_ROLE_EXP,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ROLE_EXP,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_GER_EXP,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_GER_EXP,value=Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_ITEM,TypeID,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_ITEM,value=TypeID bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([{?REWARD_GER,TypeID,Gold}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view{type=?REWARD_GER,value=TypeID bsl 16 bor Gold}|RewardViewList]);
transform2p_reward_view([#new_item{itemTypeID=TypeID,itemNum=Num}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view2{type=?REWARD_ITEM,typeID=TypeID,num=Num}|RewardViewList]);
transform2p_reward_view([#new_ger{gerTypeID=TypeID}|List], RewardViewList) ->
	transform2p_reward_view(List, [#p_reward_view2{type=?REWARD_GER,typeID=TypeID,num=1}|RewardViewList]);
transform2p_reward_view(#sell_reward{coin=AddCoin,gerExp=_AddGerExp,gold=AddGold,item=ItemList,reputation=AddReputation,roleExp=AddRoleExp,newGer=NewGer}
                       ,[]) ->
    List1 = if
                AddCoin > 0 ->
                    [{?REWARD_COIN,AddCoin}];
                true ->
                    []
            end,
    List2 = if
                AddGold > 0 ->
                    [{?REWARD_GOLD,AddGold}|List1];
                true ->
                    List1
            end,
    List3 = if
                AddReputation > 0 ->
                    [{?REWARD_REPU,AddReputation}|List2];
                true ->
                    List2
            end,
    List4 = if
                AddRoleExp > 0 ->
                    [{?REWARD_ROLE_EXP,AddRoleExp}|List3];
                true ->
                    List3
            end,
    List5 = if
                ItemList /= [] ->
                    ItemList ++ List4;
                true ->
                    List4
            end,
    List6 = if
                NewGer /= [] ->
                    NewGer ++ List5;
                true ->
                    List5
            end,
    transform2p_reward_view(List6, []).
	
%% ====================================================================
%% Internal functions
%% ====================================================================

%% 随机掉落
random_drop([]) ->
	[];
random_drop(DropList) ->
	GlobalDropRate = data_common:get(global_drop_rate),
	random_drop(DropList, [], GlobalDropRate).

random_drop([0|DropList], Result, GlobalDropRate) ->
	random_drop(DropList, Result, GlobalDropRate);
random_drop([DropID|DropList], Result, GlobalDropRate) ->
	#data_drop{randomList=RandomList} = data_drop:get(DropID),
	if (not is_list(RandomList)) orelse RandomList =:= [] ->
		   random_drop(DropList, Result, GlobalDropRate);
	   true ->
		   random_drop(DropList, random_drop2(RandomList,GlobalDropRate) ++ Result , GlobalDropRate)
	end;
random_drop([],Result, _GlobalDropRate) ->
	Result.

random_drop2([{R0,_}|List]=RandomList, GlobalDropRate) ->
	Num = R0 + lists:foldl(fun({N,_}, Acc) -> N*GlobalDropRate+Acc end, 0, List),
	Value = random:uniform() * Num,
	ItemList =
	util:foldl(fun({R,I}, Acc) ->
						Acc2 = Acc + R,
						if Acc2 >= Value ->
							   {return,I};
						   true ->
							   Acc2
						end
			   end, 0, RandomList),
	case ItemList of
		[_|_] ->
			[util:random_one_from_list(ItemList)];
		_ ->
			[]
	end.

partition_drop(List) ->
	lists:partition(fun(E) ->is_record(E, new_item) end, List).

getIncrementRate(Level)->
    OpenTimeList = data_dungeon_reward_crt:get(open_time),
    {IsOpen,FinalActivityID} = lists:foldl(fun({[StartTime,EndTime],ActivityID},{AccResult,_}) when AccResult =:= false ->
                    NowSecond = util:now(),  
                    StartTimeSecond = util:datetime_to_seconds(StartTime),
                    EndTimeSecond = util:datetime_to_seconds(EndTime),
                    case StartTimeSecond =< NowSecond andalso NowSecond =< EndTimeSecond of
                        true ->
                            {true,ActivityID};
                        _ ->
                            {false,0}
                    end;
                    (_,ACC) ->
                         ACC
                end, {false,0}, OpenTimeList),
    case IsOpen of
        true ->
            {CoinIncrementRate,ExpIncrementRate} = data_dungeon_reward_crt:get(FinalActivityID),
            case (is_integer(CoinIncrementRate) orelse is_float(CoinIncrementRate)) 
                 andalso (is_integer(ExpIncrementRate) orelse is_float(ExpIncrementRate)) 
                 andalso Level >= 10 of
                true ->
                    {CoinIncrementRate,ExpIncrementRate};
                false ->
                    {1,1}
            end;
        false ->
            {1,1}
    end.

getDropBonus(DungeonID)->
    OpenTimeList = data_drop_bonus:get(open_time),
    if
        OpenTimeList =:= undefined ->
                            ?ERR("Can not found open_time in data_drop_bonus"),
                            [];
        true ->
            NowSecond = util:now(),  
            lists:foldl(fun({[StartTime,EndTime],OpenID},Acc)->
                            StartTimeSecond = util:datetime_to_seconds(StartTime),
                            EndTimeSecond = util:datetime_to_seconds(EndTime),
                            DropBonusList = data_drop_bonus:get(OpenID),
                            if
                                DropBonusList =:= undefined ->
                                    ?ERR("Can not found ~w in data_drop_bonus",[OpenID]),
                                    Acc;
                                StartTimeSecond =< NowSecond andalso NowSecond =< EndTimeSecond ->
                                    getDropBonusByDungeonId(DungeonID,DropBonusList)++Acc;
                                true ->
                                    Acc
                            end
                        end, [], OpenTimeList)
    end.

getDropBonusByDungeonId(DungeonID,DropBonusList)->
    lists:foldl(fun({_,[DungeonMin,DungeonMax],DropList},Acc)->
                        if
                            DungeonMin =< DungeonID andalso DungeonID =< DungeonMax ->
                                random_drop2(DropList,data_common:get(global_drop_rate))++Acc;
                            true ->
                                Acc
                        end
                end, [], DropBonusList).

%%由于背包奖励发放时，存在box属性，导致物品可能出现叠加
%%根据奖励信息，从背包物品中得出实际获得的奖励,此处的奖励只是bagItem中的奖励，不包含bagEquip中的奖励

get_real_bagitem_reward(ItemBag,ItemList)->
	lists:foldl(fun(NewItem,Acc)->
		case Acc of
			{false,_Acc}->
				{false,_Acc};
			{true,_Acc}->
				#new_item{itemTypeID=NewItemTypeID,itemNum=AddNum} = NewItem,
				#data_item{itemType=ItemType} = data_item:get(NewItemTypeID),
				case lists:member(ItemType,?BagItemType) of
					true ->
						{NeedNum1,Result1} = lists:foldl(fun(Item,{NeedNum,Acc0})->
						case NeedNum > 0 of
							true ->
								#item{itemTypeID=ItemTypeID,itemNum=ItemNum}=Item,
								case ItemTypeID =:= NewItemTypeID of
									true ->
										if
									 		ItemNum >= NeedNum ->
												{0,[Item#item{itemNum=NeedNum}|Acc0]};
											true ->
												{NeedNum-ItemNum,[Item|Acc0]}
										end;
									false ->
										{NeedNum,Acc0}
								end;
							false ->
								{NeedNum,Acc0}
						end
						end,{AddNum,_Acc},ItemBag),
						case NeedNum1 =:= 0 of
							true->
								{true,Result1};
							false->
								{false,Result1}
						end;
					false ->
						{false,_Acc}
				end
		end
	end,{true,[]},ItemList).

reward_plus_reward(RewardA,RewardB) 
  when erlang:is_record(RewardA, sell_reward) 
  andalso erlang:is_record(RewardB, sell_reward)->
    NewItemList = lists:foldr(fun(NewItem,AccList)->
                                      case lists:keytake(NewItem#new_item.itemTypeID, #new_item.itemTypeID, AccList) of
                                          {value, OldItem, OtherAccList}
                                            when OldItem#new_item.itemLevel =:= NewItem#new_item.itemLevel
                                            andalso OldItem#new_item.itemRank  =:= NewItem#new_item.itemRank ->
                                              NewNum = OldItem#new_item.itemNum + NewItem#new_item.itemNum,
                                              [OldItem#new_item{itemNum=NewNum}|OtherAccList];
                                          _ ->
                                              [NewItem|AccList]
                                      end
                              end, [], RewardA#sell_reward.item ++ RewardB#sell_reward.item),
    #sell_reward{coin=RewardA#sell_reward.coin+RewardB#sell_reward.coin
                ,roleExp=RewardA#sell_reward.roleExp+RewardB#sell_reward.roleExp
                ,gerExp=RewardA#sell_reward.gerExp+RewardB#sell_reward.gerExp
                ,gold=RewardA#sell_reward.gold+RewardB#sell_reward.gold
                ,item=NewItemList
                ,reputation=RewardA#sell_reward.reputation+RewardB#sell_reward.reputation
                ,newGer=RewardA#sell_reward.newGer++RewardB#sell_reward.newGer}.

reward_reward_double(Reward) 
  when erlang:is_record(Reward, sell_reward) ->
    #sell_reward{coin=Reward#sell_reward.coin*2
                ,roleExp=Reward#sell_reward.roleExp*2
                ,gerExp=Reward#sell_reward.gerExp*2
                ,gold=Reward#sell_reward.gold*2
                ,item=[E#new_item{itemNum=E#new_item.itemNum*2}||E<-Reward#sell_reward.item]
                ,reputation=Reward#sell_reward.reputation*2
                ,newGer=Reward#sell_reward.newGer++Reward#sell_reward.newGer}.

itemlist_merge(L)->
	itemlist_merge(L,[]).
itemlist_merge([],Acc)->
	Acc;
itemlist_merge([H|T],Acc) when is_list(H)->
	itemlist_merge(T,itemlist_merge(H,Acc));
itemlist_merge([H|T],Acc) when is_record(H,new_item)->
	case lists:keytake(H#new_item.itemTypeID,#new_item.itemTypeID,Acc) of
		false->
			itemlist_merge(T,[H|Acc]);
		{_Value,F=#new_item{itemNum=OldNum},Other}->
			itemlist_merge(T,[F#new_item{itemNum=OldNum+H#new_item.itemNum}|Other])
	end;
itemlist_merge([H|T],Acc) when is_record(H,item)->
	case lists:keytake(H#item.itemTypeID,#item.itemTypeID,Acc) of
		false->
			itemlist_merge(T,[H|Acc]);
		{_Value,F=#item{itemNum=OldNum},Other}->
			itemlist_merge(T,[F#item{itemNum=OldNum+H#item.itemNum}|Other])
	end.
%%合并所有{type,Num}的类型的奖励
merge_reward(RewardList)->
	merge_reward(RewardList,[]).
merge_reward([],Acc)->
	Acc;
merge_reward([{Type,Num}=H|T]=R,Acc)->
	NewAcc = case lists:keytake(Type,1,Acc) of
		false->
			[H|Acc];
		{_,{Type,OldNum},Other}->
			[{Type,OldNum+Num}|Other];
		{_,F,Other}->
			?ERR("exist error type：~w addReward:~w R:~w Acc:~w ~n",[F,H,R,Acc])
	end,
	merge_reward(T,NewAcc);
merge_reward([{Type,TypeID,Num}=H|T]=R,Acc)->
	Exist = [E||{EType,ETypeID,ENum}=E<-Acc,EType=:=Type,ETypeID=:=TypeID],
	NewAcc = case Exist of
		[]->
			[H|Acc];
		[{OType,OTypeID,ONum}]->
			Other = Acc--Exist,
			[{OType,OTypeID,ONum+Num}|Other];
		_->
			?ERR("exist error type:~w H:~w R:~w Acc:~w ~n",[Exist,H,R,Acc])
	end,
	merge_reward(T,NewAcc).
