%% @author caohongyang
%% @doc 背包、装备、道具功能
%% Created 2013-3-15


-module(role_shop).
-compile(export_all).

-include("def_role.hrl").
-include("def_homestead.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

-define(SHOP_TYPE_NORMAL, 1).
-define(SHOP_TYPE_EXPLORE,2).
-define(SHOP_TYPE_UNIONCOIN,3).
-define(SHOP_TYPE_DECOMPOSE,4).
-define(SHOP_TYPE_HONOR,5).
-define(SHOP_TYPE_PVPPOINT,6).  %竞技场点数
-define(SHOP_TYPE_GOLDENEGG,7). %金蛋商店
-define(SHOP_TYPE_HOMESTEAD_SEED,8). %金蛋商店

-define(SHOP_ID_JU_XIAN_ZHUANG, 6666).
-define(SHOP_ID_QI_HUO_PU, 8888).
-define(SHOP_ID_UNIONCOIN, 50000).
-define(SHOP_ID_DECOMPOSE, 10002).
-define(SHOP_ID_ITEM, 10000).
-define(SHOP_ID_GOLDENEGG,53002).
%% ====================================================================
%% API functions
%% ====================================================================
cs_shop_buy_num(_) ->
	do_get_shopNumList().


cs_shop_buy(#cs_shop_buy{shopID=ShopID,sellID=SellID,num=Num}) ->
	?INFO("REcv,sellID=~w,shopID=~w,num=~w",[SellID,ShopID,Num]),
	case check_buy(Num, SellID, ShopID) of
		{true, RoleInfo, CostType, TotalCost, DataSell, ShopType} ->
			do_buy(RoleInfo, CostType, Num, TotalCost, DataSell, ShopType);
		{true, RoleInfo, CostType, TotalCost, DataSell, ShopNumList2, ShopType} ->
			role_data:set_shopNumList(ShopNumList2),
			do_buy(RoleInfo, CostType, Num, TotalCost, DataSell, ShopType);
        {true, RoleInfo, TotalCost, Type, Num} ->
            ItemType = data_homestead_seed:get({seed_item,Type}),
            ?INFO("cs_shop_buy seed ~w",[ItemType]),
            Reward = 
                #sell_reward{coin=0,gerExp=0,gold=0,item=[#new_item{itemTypeID=ItemType
                                                                   ,itemNum =Num
                                                                   ,itemLevel =1
                                                                   ,itemRank =0}],reputation=0,roleExp=0,newGer=[]},
            role_lib:deduct_gold_2_f(RoleInfo,TotalCost,?MONEY_DEC_TYPE_HOMESTEAD_SEEDING,ItemType,""),
            role_reward:handle_sell_reward_f(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_SHOP_BUY, ItemType, ""),
            ?sendself(#sc_shop_buy{result=1,reward=role_reward:transform2p_reward_view(Reward,[])});
		{false, Reason} ->
			?sendself(#sc_shop_buy{result=Reason,reward=[]})
	end.

cs_shop_encounter(#cs_shop_encounter{}) ->
	List = role_data:get_randomShopList(),
	?sendself(#sc_shop_encounter{shopList=transform_to_proto(List),unioncoin_refresh = data_shop_etc:get(unioncoin_shop_refresh_need)}).

cs_shop_refresh(#cs_shop_refresh{shopID=ShopID}) ->
	case check_refresh(ShopID) of
		{true, Role, BagOther2, NeedGold, OldShop, RandomShopList2, DataShop, DelAcc, UpdateItemLogList} ->
			do_refresh(Role, BagOther2, NeedGold, OldShop, RandomShopList2, ShopID, DataShop, DelAcc, UpdateItemLogList);
        {true, Role, NeedGold, OldShop, RandomShopList2, DataShop} ->
            do_refresh(Role, NeedGold, OldShop, RandomShopList2, ShopID, DataShop);
		{false, Reason} ->
			?sendself(#sc_shop_refresh{result=Reason,newShop=[]})
			end.

cs_shop_family_limit_info(_) ->
	   #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID >0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_shop_family_limit_info,RoleID});
        false ->
            ?sendself(#sc_shop_family_limit_info{result=2,shop=[]})
    end.

cs_shop_family_limit_buy(#cs_shop_family_limit_buy{shopID=ShopID}) ->
		   #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID >0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_shop_family_limit_buy,RoleID,ShopID});
        false ->
            ?sendself(#sc_shop_family_limit_buy{result=6})
    end.

cs_shop_seed_info(_)->
    #role{level=RoleLevel} = role_data:get_roleInfo(),
    List = lists:foldr(fun(Type,AccList)-> 
                case data_homestead_seed:get({Type,RoleLevel}) of
                    ?undefined ->
                        {Harvest,_Time,Cost} = data_homestead_seed:get({Type,get_need_level(Type)}),
                        New = #seed_sell_info{type=Type
                                             ,cost_num=Cost
                                             ,seed_item=data_homestead_seed:get({seed_sellid,Type})
                                             ,harvest_num=Harvest},
                        [New|AccList];
                    {Harvest,_Time,Cost} ->
                        New = #seed_sell_info{type=Type
                                             ,cost_num=Cost
                                             ,seed_item=data_homestead_seed:get({seed_sellid,Type})
                                             ,harvest_num=Harvest},
                        [New|AccList]
                end
        end, [], lists:seq(1, 5)),
    ?sendself(#sc_shop_seed_info{shop_ist=List}).

get_need_level(1)->
    25;
get_need_level(2)->
    25;
get_need_level(3)->
    60;
get_need_level(4)->
    80;
get_need_level(5)->
    80.

do_shop_family_limit_buy({do_shop_family_limit_buy,ShopID,Cost,Reward})->
	#role{familyID=FamilyID,roleID=RoleID} = Role=role_data:get_roleInfo(),
	case role_lib:check_money(Role,unioncoin,Cost) of
		true ->
			Role2 = role_lib:deduct_money_f(Role,unioncoin,Cost,?MONEY_DEC_TYPE_FAMILY_LIMIT_SHOP,ShopID,""),
			role_reward:handle_sell_reward_f(Role2,Reward,?MONEY_ADD_TYPE_FAMILY_LIMIT_SHOP,ShopID,""),
			%family_misc:router_to_family_process(FamilyID,{do_shop_family_limit_buy,RoleID,ShopID}),
			?sendself(#sc_shop_family_limit_buy{result=1});
		_ ->
			family_misc:router_to_family_process(FamilyID,{do_shop_family_limit_buy,RoleID,ShopID,false}),
			?sendself(#sc_shop_family_limit_buy{result=5})
	end.
		

%% init_randomShopList([]) ->
%%     #role{level=RoleLevel} = role_data:get_roleInfo(),   %% 获得角色等级
%%     ShopID = ?SHOP_ID_JU_XIAN_ZHUANG,					%% 获得shopID 6666
%%     #data_shop{refreshSec=RefreshSec} = DataShop = data_shop:get(ShopID),	%% 从配置数据中获得刷新时间 
%%     RandomList1 = random_shop(DataShop, RoleLevel),		%% 根据等级和配置数据，产生商店数据
%%     RandomList = fix_refresh(RandomList1),				%% 这里会分别是否是新手
%%     NowSec = util:now(),
%%     TimerRef = timer_wheel:add_plan(NowSec+RefreshSec, fun(TSec) ->planned_refresh(ShopID, TSec) end),
%%     NewShop = #p_shop_random{refreshSec = TimerRef, sellIDList=RandomList,shopID=ShopID},
%%     RandomShopList=[NewShop],
%%     role_data:set_randomShopList(RandomShopList);
%% init_randomShopList(RandomShopList) when is_list(RandomShopList)->
%% 	NowSec = timer_wheel:nowsec(),
%% 	#role{level=RoleLevel} = role_data:get_roleInfo(),
%% 	RandomShopList2 = 
%% 		lists:map(fun(#p_shop_random{refreshSec=RefreshSec,shopID=ShopID} = ShopRandom) ->
%% 						  if RefreshSec > NowSec ->
%% 								 TimerRef = timer_wheel:add_plan(RefreshSec, fun(TSec) -> planned_refresh(ShopID, TSec) end),
%% 								 ShopRandom#p_shop_random{refreshSec=TimerRef};
%% 							 true ->
%% 								 #data_shop{refreshSec=RefreshInterval} = DataShop = data_shop:get(ShopID),
%% 								 RandomList = random_shop(DataShop, RoleLevel),
%% 								 TimerRef = timer_wheel:add_plan(NowSec+RefreshInterval, fun(TSec) ->planned_refresh(ShopID, TSec) end),
%% 								 del_shop_num_by_shopID(ShopID),
%% 								 ShopRandom#p_shop_random{refreshSec=TimerRef,sellIDList=RandomList}
%% 						  end							 
%% 				  end, RandomShopList),
%% 	role_data:set_randomShopList(RandomShopList2);
%% init_randomShopList(_)-> ignore.


init_randomShopList(RandomShopList) when is_list(RandomShopList)->
    NowSec = util:now(),
    #role{level=RoleLevel} = role_data:get_roleInfo(),   %% 获得角色等级
    %公会商店
    UnioncoinShop = case lists:keymember(?SHOP_ID_UNIONCOIN, 2, RandomShopList) of
        false ->
            [init_ShopList(?SHOP_ID_UNIONCOIN,RoleLevel)];
        true->
            []
    end,
    %神秘商店
    JuXianZhuangShop = case lists:keymember(?SHOP_ID_JU_XIAN_ZHUANG, 2, RandomShopList) of
        false ->
            [init_ShopList(?SHOP_ID_JU_XIAN_ZHUANG,RoleLevel)];
        true->
            []
    end,
    RandomShopList2 = 
        lists:map(fun(#p_shop_random{refreshSec=RefreshSec,shopID=ShopID} = ShopRandom) ->
                          if RefreshSec > NowSec ->
                                 TimerRef = timer_wheel:add_plan(RefreshSec, fun(TSec) -> planned_refresh(ShopID, TSec) end),
                                 ShopRandom#p_shop_random{refreshSec=TimerRef};
                             true ->
                                 #data_shop{refreshSec=RefreshInterval} = DataShop = data_shop:get(ShopID),
                                 RandomList = random_shop(DataShop, RoleLevel),
                                 TimerRef = timer_wheel:add_plan(NowSec+RefreshInterval, fun(TSec) ->planned_refresh(ShopID, TSec) end),
                                 del_shop_num_by_shopID(ShopID),
                                 ShopRandom#p_shop_random{refreshSec=TimerRef,sellIDList=RandomList}
                          end                            
                  end, RandomShopList),
    ?INFO("init_randomShopList ~w ======= ~w ======== ~w",[UnioncoinShop,JuXianZhuangShop,RandomShopList2]),
    role_data:set_randomShopList(UnioncoinShop++(JuXianZhuangShop++RandomShopList2)).

init_ShopList(ShopID,RoleLevel)->
    NowSec = util:now(),
    #data_shop{refreshSec=RefreshSec} = DataShop = data_shop:get(ShopID),   %% 从配置数据中获得刷新时间 
    RandomList1 = random_shop(DataShop, RoleLevel),     %% 根据等级和配置数据，产生商店数据
    RandomList = if
                ShopID =:= ?SHOP_ID_JU_XIAN_ZHUANG ->  %% 针对新手指导
                    fix_refresh(RandomList1);
                true ->
                    RandomList1
    end,
    TimerRef = timer_wheel:add_plan(NowSec+RefreshSec, fun(TSec) ->planned_refresh(ShopID, TSec) end),
    NewShop = #p_shop_random{refreshSec = TimerRef, sellIDList=RandomList,shopID=ShopID},
    NewShop.

%% 由冒险触发刷新神秘商店
new_randomShop(ShopID) ->
	RandomShopList = role_data:get_randomShopList(),
	case lists:keyfind(ShopID, #p_shop_random.shopID, RandomShopList) of
		false ->
			case data_shop:get(ShopID) of
				#data_shop{refreshSec=RefreshSec,shopType=ShopType} = DataShop ->
					case ShopType of
						?SHOP_TYPE_EXPLORE ->
							#role{level=RoleLevel} = role_data:get_roleInfo(),
							RandomList = random_shop(DataShop, RoleLevel),
							NowSec = util:now(),
							TimerRef = timer_wheel:add_plan(NowSec+RefreshSec, fun(TSec) ->planned_refresh(ShopID, TSec) end),
							NewShop = #p_shop_random{refreshSec = TimerRef, sellIDList=RandomList,shopID=ShopID},
							RandomShopList2=[NewShop|RandomShopList],
							del_shop_num_by_shopID(ShopID),
							role_data:set_randomShopList(RandomShopList2),
							?sendself(#sc_shop_new{newShop=transform_to_proto(NewShop)});
						_ ->
							ignore
					end;
				_ ->
					ignore
			end;			
		_ ->
			ignore
	end.
		
%% ====================================================================
%% Internal functions
%% ====================================================================
false_reason(unioncoin) ->
    10;
false_reason(coin) ->
	3;
false_reason(gold) ->
	2;
false_reason(reputation) ->
	4;
false_reason(score) ->
	5;
false_reason(profoundCrystal)->
	11;
false_reason(honor)->
    15;
false_reason(pvppoint)->
    16;
false_reason(home_resource)->
    17;
false_reason(eggscore)->
	12;
false_reason(laputastone)->
    13;
false_reason(Type) ->
	?INFO("Type:~w ~n",[Type]),
	6.

seed_sellid_to_type(20050)->
    1;
seed_sellid_to_type(20051)->
    2;
seed_sellid_to_type(20052)->
    3;
seed_sellid_to_type(20053)->
    4;
seed_sellid_to_type(20054)->
    5;
seed_sellid_to_type(_)->
    ?undefined.

random_shop(#data_shop{shopID=?SHOP_ID_JU_XIAN_ZHUANG}=DataShop, RoleLevel)  ->
	List1 = random_shop2(DataShop, RoleLevel),
    DataShop2 = data_shop:get(?SHOP_ID_QI_HUO_PU),
    List2 = random_shop2(DataShop2, RoleLevel),
    List1 ++ List2;
random_shop(DataShop, RoleLevel) when DataShop#data_shop.shopID =:= ?SHOP_ID_UNIONCOIN->
    random_shop2(DataShop, RoleLevel);
random_shop(DataShop, RoleLevel) ->
    random_shop2(DataShop, RoleLevel).

random_shop2(DataShop, RoleLevel) ->
    #data_shop{sellList=SellList,sellNum=SellNum} = DataShop,
    case util:fun_find(fun(#data_sell_random{roleMaxLevel=MaxLevel}) -> RoleLevel =< MaxLevel end, SellList) of
        #data_sell_random{randomList=RandomPool} ->
            List = util:random_weigh_list(RandomPool, SellNum),
            List2 = [E||{_Weigh, E} <- List],
            List2;
        Err ->
            ?ERR("random_shop err:~p~n",[Err]),
            []
    end.


check_refresh(ShopID) when ShopID =:= ?SHOP_ID_UNIONCOIN->
    Role = role_data:get_roleInfo(),
    NeedGold = data_shop_etc:get(unioncoin_shop_refresh_need),
    case role_lib:check_money(Role, unioncoin, NeedGold) of
        true ->
            RandomShopList = role_data:get_randomShopList(),
            case lists:keytake(ShopID, #p_shop_random.shopID, RandomShopList) of
                false ->
                    {false, 3};
                {value, OldShop, RandomShopList2} ->                            
                    case data_shop:get(ShopID) of
                        #data_shop{}=DataShop ->
                            {true, Role, NeedGold, OldShop, RandomShopList2, DataShop};
                                _ ->
                                    {false, 4}
                    end
            end;
        false ->
            {false,2}
    end;
check_refresh(ShopID) ->
	{ItemTypeID, ItemNum, GoldUnit} = data_shop_etc:get(explore_shop_refresh_need),
	BagOther = role_data:get_bagItem(),
	case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
		{BagOther2, Rest, DelAcc, _UpdateAcc, UpdateItemLogList} ->
			NeedGold = Rest*GoldUnit,
			Role = role_data:get_roleInfo(),
			case role_lib:check_money(Role, gold, NeedGold) of
				true ->
					RandomShopList = role_data:get_randomShopList(),
					case lists:keytake(ShopID, #p_shop_random.shopID, RandomShopList) of
						false ->
							{false, 3};
						{value, OldShop, RandomShopList2} ->							
							case data_shop:get(ShopID) of
								#data_shop{}=DataShop ->
									{true, Role, BagOther2, NeedGold, OldShop, RandomShopList2, DataShop, DelAcc, UpdateItemLogList};
										_ ->
											{false, 4}
							end
					end;
				false ->
					{false,2}
			end
	end.

do_refresh(Role, BagOther2, NeedGold, OldShop, RandomShopList2, ShopID, DataShop, DelAcc, UpdateItemLogList) ->
	#role{roleID=RoleID, level=RoleLevel} = Role,
	if NeedGold > 0 ->
			role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_REFRESH_ENCOUNTER_SHOP, ShopID, "");
	   true ->
		   ignore
	end,

	if DelAcc == [] andalso UpdateItemLogList == [] ->
		   ignore;
	   true ->
		   %% 写道具日志
		   LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogList),
		   {Date, _} = Time = erlang:localtime(),
		   behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_REFRESH_ENCOUNTER_SHOP, ShopID, "")
	end,
	
	role_data:set_bagItem(BagOther2),
	#p_shop_random{refreshSec=TimerRef} = OldShop,
	timer_wheel:cancel_plan(TimerRef),
	NewShop = do_refresh2(ShopID, RandomShopList2, RoleLevel, DataShop),
	?sendself(#sc_shop_refresh{result=1,newShop=[transform_to_proto(NewShop)]}),
    trigger_refresh_task(ShopID).

do_refresh(Role, NeedGold, OldShop, RandomShopList2, ShopID, DataShop) ->
    #role{level=RoleLevel} = Role,
    if NeedGold > 0 ->
		   	role_lib:deduct_money_f(Role, unioncoin, NeedGold, ?MONEY_DEC_TYPE_REFRESH_ENCOUNTER_SHOP, ShopID, "") ;
            %role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_REFRESH_ENCOUNTER_SHOP, ShopID, "");
       true ->
           ignore
    end,
    #p_shop_random{refreshSec=TimerRef} = OldShop,
    timer_wheel:cancel_plan(TimerRef),
    NewShop = do_refresh2(ShopID, RandomShopList2, RoleLevel, DataShop),
    ?sendself(#sc_shop_refresh{result=1,newShop=[transform_to_proto(NewShop)]}),
    trigger_refresh_task(ShopID).

transform_to_proto(RandomShopList) when is_list(RandomShopList) ->
	[transform_to_proto(E) || E<-RandomShopList];
transform_to_proto(RandomShop) ->
	RandomShop#p_shop_random{refreshSec = timer_wheel:tarSec(RandomShop#p_shop_random.refreshSec)}.

planned_refresh(ShopID, Sec) ->
	ShopList = role_data:get_randomShopList(),
	case lists:keytake(ShopID, #p_shop_random.shopID, ShopList) of
		false ->
			ignore;
		{value, #p_shop_random{refreshSec=TimerRef}, ShopList2} ->
			?DEBUG("planned_refresh:~w\n",[TimerRef]),
			case timer_wheel:tarSec(TimerRef) of
				Sec ->
					case data_shop:get(ShopID) of
						#data_shop{} = DataShop ->
							#role{level=RoleLevel} = role_data:get_roleInfo(),
							NewShop = do_refresh2(ShopID, ShopList2, RoleLevel, DataShop),
							?sendself(#sc_shop_auto_refresh{updateShop=transform_to_proto(NewShop)});
						_ ->
							ignore
					end;
				_ ->
					ignore
			end
	end.

do_refresh2(ShopID, ShopList2, RoleLevel, DataShop) ->
	#data_shop{refreshSec=RefreshInterval} = DataShop,
	RandomList1 = random_shop(DataShop, RoleLevel),
    RandomList = if
                ShopID =:= ?SHOP_ID_JU_XIAN_ZHUANG ->  %% 针对新手指导
                    fix_refresh(RandomList1);
                true ->
                    RandomList1
    end,
	NowSec = util:now(),
	TimerRef = timer_wheel:add_plan(NowSec+RefreshInterval, fun(Sec) -> planned_refresh(ShopID, Sec) end), 
	NewShop = #p_shop_random{shopID=ShopID,refreshSec=TimerRef,sellIDList=RandomList},
	del_shop_num_by_shopID(ShopID),
	ShopList3 = [NewShop|ShopList2],
	role_data:set_randomShopList(ShopList3),
	NewShop.

fix_refresh(RandomList) ->
    case db_sql:get_guideState(role_data:get_roleID()) of
        Val when is_integer(Val)->
            CVal = data_shop_etc:get(shop_refresh_guide_num),
            case Val > CVal of
                true->
                    RandomList;
                false->
                    fix_refresh_data(RandomList)
            end;
        _->
		?ERR("fix_refresh error: get_guideState is failed",[]),
		%% 数据库读失败，为了确保新手引导的场合，新手引导能够正常进行，此处按照新手引导的情况来处理
		fix_refresh_data(RandomList)
    end.

fix_refresh_data(RandomList) ->
    case RandomList of   
        [E1,E2,E3,E4,E5,E6,E7,E8] ->
            case lists:keytake(21068, 1, [{E5},{E6},{E7},{E8}]) of
                {value, {SpecE}, [{LeftE1},{LeftE2},{LeftE3}]} ->
                    [E1,E2,E3,E4,SpecE|[LeftE1,LeftE2,LeftE3]];
                false ->
                    [E1,E2,E3,E4,21068|[E6,E7,E8]]
            end;
        _ ->
            RandomList
    end.
	
do_buy(RoleInfo, CostType, Num, TotalCost, DataSell, ShopType) ->	
	#data_sell{sellID=SellID} = DataSell,
	Reward2 = split_reward(DataSell#data_sell.sellReward,Num),
	case CostType of
		gold ->
			role_lib:deduct_gold_2_f(RoleInfo,TotalCost,?MONEY_DEC_TYPE_SHOP_BUY,SellID,"",role_reward:transform2normal_reward(Reward2));
		_ ->
			_Role2 = role_lib:deduct_money_f(RoleInfo, CostType, TotalCost, ?MONEY_DEC_TYPE_SHOP_BUY, SellID, "")
	end,
	role_reward:handle_sell_reward_f(role_data:get_roleInfo(), Reward2, ?MONEY_ADD_TYPE_SHOP_BUY, SellID, ""),
	?sendself(#sc_shop_buy{result=1,reward=role_reward:transform2p_reward_view(Reward2,[])}),
    case ShopType of
        ?SHOP_TYPE_UNIONCOIN ->
            ?CATCH(role_task_trigger:handle({dispach_task,buy_in_unioncoin}));
        ?SHOP_TYPE_HONOR ->
            do_get_shopNumList();
        _ ->
            ignore
    end.

split_reward(#sell_reward{coin=AddCoin,reputation=AddReputation,gold=AddGold,item=ItemList,newGer=Gers}=Reward,Num)->
	{Gold,Coin,Repu,ItemList2}=split_reward2(0,0,0,[],ItemList,Num),
	Ger2 = lists:foldl(fun(Ger,Acc)->lists:duplicate(Num,Ger)++Acc end,[],Gers),
	Reward#sell_reward{coin=AddCoin*Num+Coin,gold=AddGold*Num+Gold,reputation=AddReputation*Num+Repu, item=ItemList2,newGer=Ger2}.

split_reward2(Gold,Coin,Repu,ItemList,[],_)->
	{Gold,Coin,Repu,ItemList};
split_reward2(Gold,Coin,Repu,ItemList,[{new_item,20008,Num,_,_}|List],Num2)->
	split_reward2(Gold+Num*Num2,Coin,Repu,ItemList, List,Num2);
split_reward2(Gold,Coin,Repu,ItemList,[{new_item,20007,Num,_,_}|List],Num2)->
	split_reward2(Gold,Coin+Num*Num2,Repu,ItemList, List,Num2);
split_reward2(Gold,Coin,Repu,ItemList,[{new_item,20006,Num,_,_}|List],Num2)->
	split_reward2(Gold,Coin,Repu+Num*Num2,ItemList, List,Num2);
split_reward2(Gold,Coin,Repu,ItemList,[Item=#new_item{itemNum=Num}|List],Num2)->
	split_reward2(Gold,Coin,Repu,[Item#new_item{itemNum=Num*Num2}|ItemList], List,Num2).

check_buy(Num, SellID, ShopID) ->
	%% 判断非法数据
	if Num > 0 ->
		   %% 读取配置
		   case data_shop:get(ShopID) of
			   #data_shop{shopType=ShopType,sellList=SellList}  ->
				   %% 判断商店类型
				   case ShopType of
					   ?SHOP_TYPE_NORMAL ->
						   case lists:member(SellID, SellList) of
							   true ->
								   check_buy2(Num, ShopID, SellID, ShopType);
							   false ->
								   {false, 13}
						   end;
					   ?SHOP_TYPE_EXPLORE ->
						  %% 奇遇商店
						   RandomShopList = role_data:get_randomShopList(),
						   case lists:keyfind(ShopID, #p_shop_random.shopID, RandomShopList) of
							   false ->
								   {false, 14};
							   #p_shop_random{sellIDList = SellList2} ->
								   ?INFO("神秘商店sellIDList=~w,id=~w,",[SellList2,SellID]),
								   case lists:member(SellID, SellList2) of
									   true ->
										   check_buy2(Num, ShopID, SellID, ShopType);
									   false ->
										   {false ,11}
								   end
						   end;
                       ?SHOP_TYPE_UNIONCOIN when ShopID =:= ?SHOP_ID_FAMILY_FIXED ->
                           case lists:member(SellID, SellList) of
                               true ->
                                   check_buy2(Num, ShopID, SellID, ShopType);
                               false ->
                                   {false, 13}
                           end;
                       ?SHOP_TYPE_UNIONCOIN ->
                          %% 公会货币商店
                           RandomShopList = role_data:get_randomShopList(),
                           case lists:keyfind(ShopID, #p_shop_random.shopID, RandomShopList) of
                               false ->
                                   {false, 14};
                               #p_shop_random{sellIDList = SellList2} ->
                                   ?INFO("公会货币商店sellIDList=~w,id=~w,",[SellList2,SellID]),
                                   case lists:member(SellID, SellList2) of
                                       true ->
                                           check_buy2(Num, ShopID, SellID, ShopType);
                                       false ->
                                           {false ,11}
                                   end
                           end;
                        ?SHOP_TYPE_DECOMPOSE->
                        	%%分解商店
                         	case lists:member(SellID, SellList) of
							   true ->
								   check_buy2(Num, ShopID, SellID, ShopType);
							   false ->
								   {false, 13}
						   end;
                       ?SHOP_TYPE_HONOR ->
                           case lists:member(SellID, SellList) of
                               true ->
                                   check_buy2(Num, ShopID, SellID, ShopType);
                               false ->
                                   {false, 13}
                           end;
                       ?SHOP_TYPE_PVPPOINT ->
                           case lists:member(SellID, SellList) of
                               true ->
                                   check_buy2(Num, ShopID, SellID, ShopType);
                               false ->
                                   {false, 13}
                           end;
                       ?SHOP_TYPE_GOLDENEGG->
                       	   GoodsID = goldenegg_server:get_current_goodslist(),
                       	   case lists:member(SellID,GoodsID) of
                       	   		true->
                       	   			case lists:member(SellID, SellList) of
                               			true ->
                                   			check_buy2(Num, ShopID, SellID, ShopType);
                               			false ->
                                   			{false, 13}
                                   	end;
                                false->
                                	{false,13}
                           end;
                       ?SHOP_TYPE_HOMESTEAD_SEED->
                           case seed_sellid_to_type(SellID) of
                                ?undefined->
                                    {false,13};
                                SeedType->
                                    #role{level=RoleLevel} = RoleInfo = role_data:get_roleInfo(),
                                    case data_homestead_seed:get({SeedType,RoleLevel}) of
                                        ?undefined ->
                                            {false,13};
                                        {_Harvest,_Time,Cost} ->
                                            TotalCost = Cost*Num,
                                            case role_lib:check_money(RoleInfo, gold, TotalCost) of
                                                false ->
                                                    {false, false_reason(gold)};
                                                true ->
                                                    {true, RoleInfo, TotalCost, SeedType, Num}
                                            end
                                    end
                           end                       
                    end;
			   _ ->
				   {false,10}
		   end;
	   true ->
		   {false, 12}
	end.

check_vip_need(NeedVip,_, SVipLevel) when NeedVip > 100 andalso SVipLevel > 0 -> NeedVip - 100 =< SVipLevel;
check_vip_need(NeedVip,VipLevel,_) -> NeedVip =< VipLevel.


check_buy2(Num, ShopID, SellID, ShopType) ->
	case data_sell:get(SellID) of
		#data_sell{needVipLevel=NeedVipLevel} = DataSell ->
			RoleInfo = role_data:get_roleInfo(),
			%% 判断vip等级是否足够
            %?INFO("check_buy2 ~w ~w ~w",[RoleInfo#role.vipLevel,RoleInfo#role.svipLevel , NeedVipLevel]),
            CheckVip = check_vip_need(NeedVipLevel,RoleInfo#role.vipLevel,RoleInfo#role.svipLevel),
			if CheckVip ->
                %RoleInfo#role.vipLevel >= NeedVipLevel ->
				   #data_sell{costType=CostType, costNum=CostNum} = DataSell,
                   OpenLevel = data_home:get(constr_type_office_open_role_level),
                   Discount = if
                                  OpenLevel =< RoleInfo#role.level ->
                                    data_home:get({constr_type_shop,role_home:get_build_type_level(role_data:get_roleID(),?constr_type_shop)});
                                  true ->
                                    0
                              end,
                   TotalCost = (CostNum * Num * (10000 - Discount) + 9999) div 10000,   %% +9999 
				   %% 判断货币是否足够
				   case role_lib:check_money(RoleInfo, CostType, TotalCost) of
					   false ->
						   {false, false_reason(CostType)};
					   true ->
						   MaxBuyNum = DataSell#data_sell.maxBuyNum,
						   %% 判断是否有购买次数限制
						   case MaxBuyNum < 0 of
							   true ->
								   {true, RoleInfo, CostType, TotalCost, DataSell, ShopType};
							   false ->
								   ShopNumList = role_data:get_shopNumList(),
								   %% 读取已购买次数
								   ?INFO("ShopNumList:~w ~n",[ShopNumList]),
								   case util:fun_take(fun(E) ->
															  E#p_shop_num.shopID=:= ShopID andalso E#p_shop_num.sellID=:= SellID
													  end, ShopNumList) of
									   false ->
										   ShopNumList2=ShopNumList,
										   Num2 = Num;
									   {value, #p_shop_num{buyNum=BuyedNum}, ShopNumList2} ->
										   Num2 = Num + BuyedNum
								   end,
                                  {RefreshType,MaxNum} = case DataSell#data_sell.maxBuyNum of
                                                -1 ->
                                                    {0,0};
                                                BuyNum when erlang:is_integer(BuyNum) andalso BuyNum >= 0 ->
                                                    {1,BuyNum};
                                                {week,WeekDay,BuyNum} ->
                                                    {10+WeekDay,BuyNum}
                                              end,
								   %% 判断能否继续购买
								   if Num2 =< MaxNum ->
                                                                                  DiscCost = (CostNum * (10000 - Discount) + 9999) div 10000,   %% +9999 
										  ShopNumList3 = [#p_shop_num{buyNum=Num2,sellID=SellID,shopID=ShopID
                                                                     ,buyMax=MaxNum,refresh_type=RefreshType}|ShopNumList2],
										  {true, RoleInfo, CostType, TotalCost, DataSell, ShopNumList3, ShopType};
									  true ->
										  {false, 8}
								   end
						   end
				   end;
			   
			   true ->
				   {false, 7}
			end;
		_ ->
			{false, 9}
	end.


del_shop_num_by_shopID(ShopID) ->
	ShopNumList = role_data:get_shopNumList(),
	ShopNumList2 = lists:filter(fun(E) -> E#p_shop_num.shopID =/= ShopID end, ShopNumList),
	role_data:set_shopNumList(ShopNumList2),
	ShopNumList2.

daily_refresh_item_shop(DayPassNum) ->
	ShopNumList = 
		lists:foldl(fun(#p_shop_num{shopID=ShopID}=E,Acc)->
			case ShopID of
				?SHOP_ID_ITEM->
					[E#p_shop_num{buyNum=0}|Acc];
				?SHOP_ID_GOLDENEGG->
					[E#p_shop_num{buyNum=0}|Acc];
				_->
					[E|Acc]
			end
		end, [], role_data:get_shopNumList()),
    %竞技场商店每周刷新
    NowWeekNum = calendar:day_of_the_week(erlang:date()),
    ?INFO("daily_refresh_item_shop ~w ~w ",[NowWeekNum,DayPassNum]),
    ShopNumList2 = 
        lists:foldl(fun(#p_shop_num{sellID=SellID}=E,Acc)-> 
                        SellData = data_sell:get(SellID),
                        case SellData of
                            #data_sell{ sellID = SellID} ->
                                case SellData#data_sell.maxBuyNum of
									DayNum when erlang:is_integer(DayNum) andalso SellID > 30000 andalso SellID < 30017 ->
                                        [E|Acc];
                                    DayNum when erlang:is_integer(DayNum) ->
                                        Acc;
                                    {week,_,_} when DayPassNum >= 7 ->
                                        Acc;
                                    {week,NowWeekNum,_}-> % RefreshWeekNum =:= NowWeekNum
                                        Acc;
                                    {week,RefreshWeekNum,_}
                                      when ((NowWeekNum+7 - RefreshWeekNum) rem 7) < DayPassNum->
                                        Acc;
                                    _ ->
                                        [E|Acc]
                                end;
                            _ ->
                                ?WARNING("SellID(~w) config is wrong. ~w",[SellID,SellData]),
                                Acc
                        end
                    end, [], ShopNumList),
	?sendself(#sc_shop_buy_num{shopNumList=ShopNumList2}),
	role_data:set_shopNumList(ShopNumList2).
	
do_get_shopNumList() ->
	ShopNumList = role_data:get_shopNumList(),
    OpenLevel = data_home:get(constr_type_office_open_role_level),
    RoleInfo = role_data:get_roleInfo(),
    Discount = if
                  OpenLevel =< RoleInfo#role.level ->
                    data_home:get({constr_type_shop,role_home:get_build_type_level(role_data:get_roleID(),?constr_type_shop)});
                  true ->
                    0
              end,
    ShopNumList3 = lists:foldr(fun(ShopId,SellListAcc)-> 
                    #data_shop{sellList=SellConfigList}=data_shop:get(ShopId),
                    lists:foldl(fun(SellId,AccList)->
                                    case lists:keyfind(SellId, #p_shop_num.sellID, AccList) of
                                        false ->
                                            DataSell = data_sell:get(SellId),
                                            case DataSell of
                                                #data_sell{costNum=CostNum} ->
                                                    {RefreshType,MaxBuyNum} = case DataSell#data_sell.maxBuyNum of
                                                                    -1 ->
                                                                        {0,0};
                                                                    Num when erlang:is_integer(Num) andalso Num >= 0 ->
                                                                        {1,Num};
                                                                    {week,WeekDay,Num} ->
                                                                        {10+WeekDay,Num}
                                                                  end,
                                                    DiscCost = (CostNum * (10000 - Discount) + 9999) div 10000,   %% +9999 
                                                    [#p_shop_num{shopID=ShopId,sellID=SellId,buyNum=0
                                                                ,buyMax=MaxBuyNum,refresh_type=RefreshType}|AccList];
                                                _ ->
                                                    ?WARNING("SellID(~w) config is wrong. ~w",[SellId,DataSell]),
                                                    AccList
                                            end;
                                        _ ->
                                            AccList
                                    end
                               end, SellListAcc, SellConfigList)
                end, ShopNumList, [?SHOP_ID_HONOR,?SHOP_ID_PVPPOINT,?SHOP_ID_FAMILY_FIXED,?SHOP_ID_GOLDENEGG]),
	?sendself(#sc_shop_buy_num{shopNumList=ShopNumList3}).

trigger_refresh_task(ShopID) ->
    case data_shop:get(ShopID) of
        #data_shop{shopType=ShopType} ->
            case ShopType of
                ?SHOP_TYPE_UNIONCOIN ->
                    ?CATCH(role_task_trigger:handle({dispach_task,refresh_unioncoin}));
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

get_buy_num(ShopID,SellID)->
	ShopNumList = role_data:get_shopNumList(),
	case util:fun_take(fun(E) ->
		E#p_shop_num.shopID=:= ShopID andalso E#p_shop_num.sellID=:= SellID
	end, ShopNumList) of
		false ->
			0;
		{value, #p_shop_num{buyNum=BuyedNum}, _ShopNumList2} ->
			BuyedNum
	end.
