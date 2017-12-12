%% @author shujunjie
%% @doc 放爆竹玩家信息处理
%% Created 2014-01-08
-module(role_firecracker).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

cs_firecracker_rewardList(_)->
	#data_activity_fire{randomReward=RandomReward,hiddenReward=HiddenReward,coin=_Coin} = fire_server:get_activity_info(),
	Rewards = lists:foldl(fun({_,X},Acc)->
								  case hd(X) of
									  {_,Y} ->
										  Y++Acc;
									  _ ->
										  X++Acc
								  end
						  end,[],HiddenReward++RandomReward),
	{Items,Gers}=lists:foldl(fun(#new_ger{}=NewGer,{ItemAcc,GerAcc})->
								   case lists:member(NewGer,GerAcc) of
									   true ->
										   {ItemAcc,GerAcc};
									   _ ->
										   {ItemAcc,[NewGer|GerAcc]}
								   end;
							  (#new_item{}=NewItem,{ItemAcc,GerAcc}) ->
								   case lists:member(NewItem,ItemAcc) of
									   true ->
										   {ItemAcc,GerAcc} ;
									   _ ->
										   {[NewItem|ItemAcc],GerAcc}
								   end
						   
								   end, {[],[]}, Rewards),
	RewardList = activity_server:sell_reward2p_reward_info(#sell_reward{item=Items,newGer=Gers}),
	?sendself(#sc_firecracker_rewardList{allrewards=RewardList}).
	

%% 检查角色VIP等级
check_vip_level({vip, Lower, Upper}, VipLevel)->
%% 	VipLevel>=Lower andalso VipLevel=<Upper. %% 不做上限检查
    VipLevel>=Lower.
%% 检查角色等级
check_role_level({level, Lower, Upper}, RoleLevel)->
%% 	RoleLevel>=Lower andalso RoleLevel=<Upper. %% 不做上限检查
    RoleLevel>=Lower.

%% 查看鞭炮信息
cs_firecracker_open(#cs_firecracker_open{})->
	case fire_server:get_activity_info() of
		?undefined->
			?sendself(#sc_firecracker_open{status=0,startTime=0,rewardTime=0,closeTime=0
                                          ,total=0,markedPrice=0,tradedPrice=0,count=0
                                          ,rank=0,canReward=0,returnGold=0,minLevel=0,minVip=0});
		#data_activity_fire{activityName=Name,description=Description,iconSrc=Icon,vip=Vip,level=Level}->
			#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
			case check_vip_level(Vip, VipLevel) andalso check_role_level(Level, RoleLevel) of
				true->
                    fire_server:open(RoleID,Name,Description,Icon);
				false->
                    {level,MinLevel,_} = Level,
                    {vip,MinVip,_} = Vip,
					?sendself(#sc_firecracker_open{status=0,startTime=0,rewardTime=0,closeTime=0
                                                  ,total=0,markedPrice=0,tradedPrice=0,count=0,rank=0,canReward=0,returnGold=0,minLevel=MinLevel,minVip=MinVip})
			end
	end.

%% 燃放鞭炮，对应 皮卡丘的祈祷
cs_firecracker_setoff(#cs_firecracker_setoff{type=Type})->
	case fire_server:get_activity_status() of
		1 ->
			#data_activity_fire{vip=Vip,level=Level} = fire_server:get_activity_info(),
			#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel,gold=Gold,goldBonus=GoldBonus} = role_data:get_roleInfo(),
			case check_vip_level(Vip, VipLevel) andalso check_role_level(Level, RoleLevel) of
				true->
					Times =
						case Type of
							1->
								1;
							2->
								10;
							_->
								?undefined
						end,
					case Times of
						?undefined->
							?sendself(#sc_firecracker_setoff{result=4,count=0,returnGold=0,canReward=0});
						_ when Times =:= 1->
                            fire_server:calc_need_gold(RoleID,Times,Gold+GoldBonus,1);
                        _ -> %% 这个逻辑是为了，只有一次的钱，也不能进行十连祈福
                            fire_server:calc_need_gold(RoleID,Times,Gold+GoldBonus,2)
					end;
				false->
					?sendself(#sc_firecracker_setoff{result=2,count=0,returnGold=0,canReward=0})
			end;
		_->
			?sendself(#sc_firecracker_setoff{result=2,count=0,returnGold=0,canReward=0})
	end.

%% 发送奖励
%% Rewards= [#new_item{itemTypeID = 16,itemNum = 1,itemLevel = 1,itemRank = 0},
%%           #new_item{itemTypeID = 26,itemNum = 1,itemLevel = 1,itemRank = 0},
%%           #new_ger{gerTypeID = 5101,gerLevel = 1,gerQuality = 0}]
%%  #sell_reward{coin = 0,roleExp = 0,gerExp = 0,gold = 0,
%%              item = [],reputation = 0,newGer = []}
do_fire_reward(Rewards)->
    #role{roleName=RoleName} = role_data:get_roleInfo(),
%%     lists:foldl(fun(X,{PrewardInfoList,{sell_reward, Coin, RoleExp, GerExp, Gold, ItemList, Reputation, NewGerList} = SellReward})->
    lists:foldl(fun(X,{PrewardInfoList,SellReward})->
        case X of
            %% 如果是徽章的话
            #new_item{itemTypeID = ItemTypeID,itemNum = ItemNum} when ItemTypeID =:= 20006->
                PrewardInfo = #p_reward_info{coin=0,gerExp=0
                                       ,gerList=[]
                                       ,gold=0,itemList=[]
                                       ,reputation=ItemNum,roleExp=0},
                {[PrewardInfo|PrewardInfoList],SellReward#sell_reward{reputation=ItemNum+SellReward#sell_reward.reputation}};
            %% 如果是金币的话
            #new_item{itemTypeID = ItemTypeID,itemNum = ItemNum} when ItemTypeID =:= 20007->
                PrewardInfo = #p_reward_info{coin=ItemNum,gerExp=0
                                       ,gerList=[]
                                       ,gold=0,itemList=[]
                                       ,reputation=0,roleExp=0},
                {[PrewardInfo|PrewardInfoList],SellReward#sell_reward{coin=ItemNum+SellReward#sell_reward.coin}};
            %% 如果是钻石的话
            #new_item{itemTypeID = ItemTypeID,itemNum = ItemNum} when ItemTypeID =:= 20008->
                PrewardInfo = #p_reward_info{coin=0,gerExp=0
                                       ,gerList=[]
                                       ,gold=ItemNum,itemList=[]
                                       ,reputation=0,roleExp=0},
                {[PrewardInfo|PrewardInfoList],SellReward#sell_reward{gold=ItemNum+SellReward#sell_reward.gold}};
            %% 如果是物品的话
            #new_item{itemTypeID = ItemTypeID,itemNum = ItemNum,itemLevel = _ItemLevel,itemRank = _ItemRank}->
                #data_item{itemStar=ItemStar,itemType=ItemType} = data_item:get(ItemTypeID),
                ItemView=item_lib:new_item2p_item_view(X),
                IsEquip = item_lib:is_itemType_equip(ItemType),
                IsStone = item_lib:is_itemType_stone(ItemType), 
                %% 需要
                case (ItemStar>=5 andalso IsEquip) %装备
                        orelse (ItemStar>=4 andalso IsStone) %符文
                        orelse (ItemStar>=4 andalso ItemType =:= ?stone)  %经验符文
                        orelse (ItemStar>=4 andalso ItemType =:= ?stonechip)  of %碎片
                    true ->
                        broadcast_server:bc_msgID(10043, [RoleName, ItemView,erlang:integer_to_list(ItemNum)]);
                    _ ->
                        next
                end,
                PrewardInfo = #p_reward_info{coin=0,gerExp=0
                                       ,gerList=[]
                                       ,gold=0,itemList=[ItemView]
                                       ,reputation=0,roleExp=0},
                {[PrewardInfo|PrewardInfoList],SellReward#sell_reward{item=[X|SellReward#sell_reward.item]}};
            %%如果是宠物的话
            #new_ger{gerTypeID = GerTypeID,gerLevel = _GerLevel,gerQuality = _GerQuality}->
                #data_ger{gerStar=GerStar}=data_ger:get(GerTypeID),
                GerView=ger_lib:new_ger2p_ger_view(X),
                if GerStar>=5 ->
                        broadcast_server:bc_msgID(10044, [RoleName, GerView, "1"]);
                    true ->
                        next
                end,
                PrewardInfo = #p_reward_info{coin=0,gerExp=0
                                       ,gerList=[GerView]
                                       ,gold=0,itemList=[]
                                       ,reputation=0,roleExp=0},
                {[PrewardInfo|PrewardInfoList],SellReward#sell_reward{newGer=[X|SellReward#sell_reward.newGer]}};
            _ ->
                {PrewardInfoList,SellReward}
        end
    end,{[],{sell_reward, 0, 0, 0, 0, [], 0, []}}, Rewards).

need_to_broadcast(ItemStar, ItemType)->
	if ItemStar > 4 ->
		   true;
	   ItemStar =:= 4->
		   case item_lib:is_main_equip(ItemType) of
			   true ->
				   true;
			   false ->
				   false
		   end;
	   true ->
		   false
	end.

get_list(List)	->
	if	List =:= 0	->
			[];
		is_tuple(List) ->
			[List];
		true	->
			List
	end.

%% 领取排行奖励
cs_firecracker_get_reward(#cs_firecracker_get_reward{})->
	case fire_server:get_activity_status() of
		2 ->
			RoleID = role_data:get_roleID(),
            fire_server:get_reward(RoleID);
%% 			case fire_server:get_reward(RoleID) of
%% 				{true, SellReward} ->
%% 					RoleInfo = role_data:get_roleInfo(),
%% 					role_reward:handle_sell_reward_f(RoleInfo, SellReward, ?MONEY_ADD_TYPE_FIRECRACKER_RANK, 0, ""),
%% 					?sendself(#sc_firecracker_get_reward{result=1, reward=[activity_server:sell_reward2p_reward_info(SellReward)]});
%% 				{false, ErrCode} ->
%% 					?sendself(#sc_firecracker_get_reward{result=ErrCode})
%% 			end;
		_ ->
			?sendself(#sc_firecracker_get_reward{result=2})
	end.

fire_open_reuslt(Total,Count,Rank,CanReward,ReturnGold,Name,Description,Icon)->
    #data_activity_fire{startTime=StartTime_,stopTime=StopTime_,closeTime=CloseTime_,vip=Vip,level=Level} = fire_server:get_activity_info(),
    {level,MinLevel,_} = Level,
    {vip,MinVip,_} = Vip,
    StartTime = util:datetime_to_seconds(StartTime_),
    RewardTime = StartTime + fire_server:relative_time(StopTime_),
    CloseTime = RewardTime + fire_server:relative_time(CloseTime_),
    Record = #sc_firecracker_open{status=1, name=Name, description=Description, icon=Icon,
                              startTime=StartTime, rewardTime=RewardTime, closeTime=CloseTime, 
                              total=Total, markedPrice=fire_server:get_marked_price(), tradedPrice=fire_server:get_traded_price(Total),
                              count=Count, rank=Rank, canReward=CanReward, returnGold=ReturnGold,
                              discounts=[#p_discount{amount=Amount, discount=Discount} || {Amount, Discount}<-fire_server:get_discounts()]
                             ,minLevel=MinLevel,minVip=MinVip},
    ?sendself(Record).

fire_calc_need_gold_reuslt(DeductGold, Times)->
    RoleInfo = role_data:get_roleInfo(),
    case role_lib:check_money(RoleInfo, gold, DeductGold) of
        true when DeductGold /= 0 ->
            fire_server:setoff(RoleInfo#role.roleID, DeductGold, Times);
        _ ->
            ?sendself(#sc_firecracker_setoff{result=3,count=0,returnGold=0,canReward=0})
    end.

fire_setoff_reuslt(RewardList, NewCount, ReturnGold, IsCanGetReward, DeductGold)->
    RoleInfo = role_data:get_roleInfo(),
   {PrewardInfoList, SellReward} = do_fire_reward(RewardList),
    NewRoleInfo = role_lib:deduct_gold_f(RoleInfo, DeductGold, ?MONEY_DEC_TYPE_FIRECRACKER, NewCount, "",role_reward:transform2normal_reward(SellReward)),
    ?INFO("L-cs_firecracker_setoff RewardList:~w",[RewardList]),

    ?INFO("L-cs_firecracker_setoff PrewardInfoList:~w ==== SellReward:~w",[PrewardInfoList, SellReward]),
    ?sendself(#sc_firecracker_setoff{result=1,count=NewCount,returnGold=ReturnGold,canReward=IsCanGetReward,
                                     rewardInfo=PrewardInfoList}),
    role_reward:handle_sell_reward_f(NewRoleInfo, SellReward, ?MONEY_ADD_TYPE_FIRECRACKER, 0, "").

fire_get_reward_reuslt(Result)->
    case Result of
        {true, SellReward} ->
            RoleInfo = role_data:get_roleInfo(),
            role_reward:handle_sell_reward_f(RoleInfo, SellReward, ?MONEY_ADD_TYPE_FIRECRACKER_RANK, 0, ""),
            ?sendself(#sc_firecracker_get_reward{result=1, reward=[activity_server:sell_reward2p_reward_info(SellReward)]});
        {false, ErrCode} ->
            ?sendself(#sc_firecracker_get_reward{result=ErrCode})
    end.
