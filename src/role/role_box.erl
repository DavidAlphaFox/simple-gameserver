%% @author caohongyang
%% @doc 宝箱功能
%% Created 2013-6-20


-module(role_box).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_item.hrl").
-include("def_weibo_share.hrl").
-include("def_homestead.hrl").  
%% API functions
-export([]).

%% Internal functions 
-export([]).

-define(TAB_TYPE_SPIRIT,24001).%24001：召唤精灵   
-define(TAB_TYPE_EQUIP,24002).%24002：祈祷装备
-define(TAB_TYPE_TRAINER,24100).%24100：抽取训练师装备
-define(OPEN_BOX_ITEM,0).
-define(OPEN_BOX_GOLD_1,1).
-define(OPEN_BOX_GOLD_10,2).
-define(OPEN_BOX_ITEM_10,3).
-define(OPEN_BOX_ITEM_100,4).

-define(upcounter_limitInfo(Filed),begin
                                       LimitInfo = role_data:get_limitInfo(),
                                       Count = LimitInfo#limitInfo.Filed,
                                       NewCount = Count+1,
                                       LimitInfo2 = LimitInfo#limitInfo{Filed=NewCount},
                                       role_data:set_limitInfo(LimitInfo2),
                                       NewCount
                                   end).
-define(upcounter_limitInfo(Filed1,Field2),begin
                                               LimitInfo = role_data:get_limitInfo(),
                                               Count = LimitInfo#limitInfo.Filed1,
                                               Count2 = LimitInfo#limitInfo.Field2,
                                               NewCount = Count+1,
                                               LimitInfo2 = LimitInfo#limitInfo{Filed1=NewCount},
                                               role_data:set_limitInfo(LimitInfo2),
                                               NewCount+Count2
                                           end).
-define(upcounter_limitInfo(Filed1,Field2,Filed3),begin
                                                      LimitInfo = role_data:get_limitInfo(),
                                                      Count = LimitInfo#limitInfo.Filed1,
                                                      Count2 = LimitInfo#limitInfo.Field2,
                                                      NewCount = Count+1,
                                                      LimitInfo2 = LimitInfo#limitInfo{Filed1=NewCount,Filed3=0},
                                                      role_data:set_limitInfo(LimitInfo2),
                                                      NewCount+Count2
                                                  end).
-define(update_counter_refresh(Filed3),begin
                                           LimitInfo = role_data:get_limitInfo(),
                                           LimitInfo2 = LimitInfo#limitInfo{Filed3=1},
                                           role_data:set_limitInfo(LimitInfo2)
                                       end).

%% ====================================================================
%% API functions
%% ====================================================================

cs_box_shopBoxDtl(_) ->
    ?sendself(#sc_box_shopBoxDtl{}).
%#shop_box_card{cdInfo=CDInfo}=role_data:get_shopBoxData(),
%case CDInfo of 0 ->	?sendself(#sc_box_shopBoxDtl{});_ -> ?sendself(CDInfo) end.

cs_box_shop_view(#cs_box_shop_view{tab=Tab,type=Type})->
    ?sendself(#sc_box_shopBoxDtl{}).
%#shop_box_card{openedCardInfo=OpenCardInfo,cdInfo=CDInfo} =CardInfo= role_data:get_shopBoxData(),
%case [X||{Tab0,Type0,_}=X<-OpenCardInfo,Tab0 == Tab,Type0==Type] of
%	[] -> CDInfo2 = case CDInfo of 0 -> #sc_box_shopBoxDtl{}; _ -> CDInfo end,
%		  role_data:set_shopBoxData(CardInfo#shop_box_card{cdInfo=erlang:setelement(get_cd_ele_num(Tab,Type), CDInfo2, 0)}),
%		  ?sendself(#sc_box_shop_view{result=2,reward=[]});
%	[{_,_,V}] -> ?sendself(#sc_box_shop_view{result=1,reward=role_reward:transform2p_reward_view(V,[])})
%end.

cs_box_shop_refresh(#cs_box_shop_refresh{tab=Tab,type=Type}) ->
    ?sendself(#sc_box_shop_refresh{result=2}).
% #shop_box_card{cdInfo=CDInfo} =CardInfo= role_data:get_shopBoxData(),
% CDInfo2 = case CDInfo of 0 -> #sc_box_shopBoxDtl{}; _ -> CDInfo end,
% Now = util:now(),
% EleN = get_cd_ele_num(Tab,Type),
% case erlang:element(EleN,CDInfo2) of
% 	X when is_integer(X) andalso X > Now -> ?sendself(#sc_box_shop_refresh{result=2});
% 	_ -> do_refresh_box_shop(EleN,CardInfo,Tab,Type)
% end.

%%get_cd_ele_num(24001,1) -> 3;
%%get_cd_ele_num(24001,2) -> 4;
%%get_cd_ele_num(24001,_) -> 2;
%%get_cd_ele_num(24002,1) -> 6;
%%get_cd_ele_num(24002,2) -> 7;
%%get_cd_ele_num(24002,_) -> 5;
%%get_cd_ele_num(24100,1) -> 9;
%%get_cd_ele_num(24100,2) -> 10;
%%get_cd_ele_num(_,_) -> 8.
%%	
%%update_tab_counter(Tab) ->
%% 		case Tab of
%% 			?TAB_TYPE_SPIRIT->	?upcounter_limitInfo(spiritGoldBonusBoxCount,spiritGoldBoxCount,spiritRefresh);
%% 			?TAB_TYPE_EQUIP->	?upcounter_limitInfo(equipGoldBonusBoxCount,equipGoldBoxCount,equipRefresh);
%% 			?TAB_TYPE_TRAINER->	?upcounter_limitInfo(trainerGoldBonusBoxCount,trainerGoldBoxCount,trainerRefresh)								
%% 		end.
%%
%%get_tab_counter(Tab) ->
%%	LimitInfo = role_data:get_limitInfo(),
%%	case Tab of 
%%		?TAB_TYPE_SPIRIT -> {LimitInfo#limitInfo.spiritGoldBonusBoxCount + LimitInfo#limitInfo.spiritGoldBoxCount,LimitInfo#limitInfo.spiritRefresh};
%%		?TAB_TYPE_EQUIP -> {LimitInfo#limitInfo.equipGoldBonusBoxCount + LimitInfo#limitInfo.equipGoldBoxCount,LimitInfo#limitInfo.equipRefresh};
%%		?TAB_TYPE_TRAINER -> {LimitInfo#limitInfo.trainerGoldBonusBoxCount + LimitInfo#limitInfo.trainerGoldBoxCount,LimitInfo#limitInfo.trainerRefresh}
%%	end.
%%
%%get_tab_counter_item(Tab) ->
%%	LimitInfo = role_data:get_limitInfo(),
%%	case Tab of
%%		?TAB_TYPE_SPIRIT->LimitInfo#limitInfo.spiritItemBoxCount;
%%		?TAB_TYPE_EQUIP->LimitInfo#limitInfo.equipItemBoxCount;
%%		?TAB_TYPE_TRAINER->LimitInfo#limitInfo.trainerItemBoxCount
%%	end.
%%
update_tab_limitInfo_value(2) ->	?upcounter_limitInfo(combine2StarCount);
update_tab_limitInfo_value(_) -> 0.
%%
%%update_tab_counter_refresh(Tab) ->
%%	case Tab of
%%		?TAB_TYPE_SPIRIT -> ?update_counter_refresh(spiritRefresh);
%%		?TAB_TYPE_EQUIP -> ?update_counter_refresh(equipRefresh);
%%		?TAB_TYPE_TRAINER -> ?update_counter_refresh(trainerRefresh)
%%	end.
%%
%%get_reward(Tab,1) ->
%%	{Count,Refresh} = get_tab_counter(Tab),
%%	MainGerTypeID = role_data:get_main_gerTypeID(),
%%	BoxID0 = 
%%		if Count == 0 ->
%%			   IsCrt0 = false,
%%			   update_tab_counter_refresh(Tab),
%%			   data_fixed_box_withitem:get({first_select_box,Tab,MainGerTypeID});
%%		   true -> IsCrt0 = true,?undefined
%%		end,
%%	BoxID = case BoxID0 of ?undefined ->
%%							   case Count rem 10 =:= 0 andalso Refresh == 0 of 
%%								   true -> case data_fixed_box_withitem:get({five_star_box,Tab}) of
%%											   ?undefined -> IsCrt= true, Tab;
%%											   B -> update_tab_counter_refresh(Tab),
%%													IsCrt = false,
%%													B  %% 五次必出,不管这次是否领取,下一次刷新肯定是原始流程,所以需要刷新数据次数
%%										   end;
%%								   _ -> IsCrt = true, Tab
%%							   end;
%%				_ -> IsCrt = IsCrt0,BoxID0
%%			end,
%%	[R1|_] = 
%%		case data_box:get({BoxID, MainGerTypeID}) of
%%			?undefined -> ?ERR("not find box:~w",[[BoxID,MainGerTypeID]]),?undefined;
%%			DataX -> DataX
%%		end,
%%	RewardList2= [util:random_one_from_weigh_list(R1)],
%%	if IsCrt ->	do_random_amount(gold,RewardList2,Tab);
%%	   true -> RewardList2
%%	end;
%%get_reward(Tab,2) ->
%%	MainGerTypeID = role_data:get_main_gerTypeID(),
%%	[R1, R2, R3, R4, R5] = data_box:get({Tab, MainGerTypeID}),
%%	{R1T, R2T, R3T, R4T, R5T} = data_box:get(open_box_10),
%%	RL1 = random_reward(R1, R1T, []),
%%	RL2 = random_reward(R2, R2T, RL1),
%%	RL3 = random_reward(R3, R3T, RL2),
%%	RL4 = random_reward(R4, R4T, RL3),
%%	RewardList1 = random_reward(R5, R5T, RL4),
%%	RewardList2 = util:random_list2(RewardList1),
%%	do_random_amount(gold,RewardList2,Tab);
%%get_reward(Tab,Type) ->
%%	MainGerTypeID = role_data:get_main_gerTypeID(),
%%	CT = get_tab_counter_item(Tab),
%%	case Type of
%%		?OPEN_BOX_ITEM->
%%			ISGet = true,
%%			BoxIDList = get_boxid_list_by_item(Tab,[],1);
%%		?OPEN_BOX_ITEM_10 ->
%%			{item,ItemTypeID,RealNum} = data_box:get({shopBoxCost,Tab,Type}),
%%			BagOther = role_data:get_bagItem(),
%%			case item_lib:check_material2(BagOther,ItemTypeID, RealNum) of
%%				{_BagOther2, RealNum, _DelAcc, _UpdateAcc, _UpdateLogList}->
%%					ISGet = false,
%%					BoxIDList=[],
%%					{false,3};
%%				{_BagOther2, RestNum, _DelAcc, _UpdateAcc, _UpdateLogList}->
%%					ISGet = true,
%%					BoxIDList = get_boxid_list_by_item(Tab,[],10-RestNum)
%%			end;
%%		?OPEN_BOX_ITEM_100 ->
%%			{item,ItemTypeID,RealNum} = data_box:get({shopBoxCost,Tab,Type}),
%%			BagOther = role_data:get_bagItem(),
%%			case item_lib:check_material2(BagOther,ItemTypeID, RealNum) of
%%				{_BagOther2, RealNum, _DelAcc, _UpdateAcc, _UpdateLogList}->
%%					ISGet = false,
%%					BoxIDList=[],
%%					{false,3};
%%				{_BagOther2, RestNum, _DelAcc, _UpdateAcc, _UpdateLogList}->
%%					ISGet = true,
%%					BoxIDList = get_boxid_list_by_item(Tab,[],100-RestNum)
%%			end
%%	end,
%%	if ISGet ->
%%		   ConfigList = lists:foldl(fun(BoxID,Acc)->
%%											[R|_] = 
%%												case data_box:get({BoxID,MainGerTypeID}) of
%%													?undefined -> ?ERR("not find box:~w",[[BoxID,MainGerTypeID]]) ,?undefined;
%%													DataX -> DataX
%%												end,
%%											[R|Acc]
%%									end,[],BoxIDList),
%%		   RewardList2 = [util:random_one_from_weigh_list(R1)||R1<-ConfigList],
%%		   case CT < 2 of true -> RewardList2;
%%			   _-> do_random_amount(item,RewardList2,Tab)
%%		   end;
%%	   true -> []
%%	end.
%%
%%do_refresh_box_shop(EleN,CardInfo,Tab,Type) ->
%%	Reward = get_reward(Tab,Type),
%%	#shop_box_card{openedCardInfo=OpenCardInfo,cdInfo=CDInfo}=CardInfo,
%%		CDInfo2 = case CDInfo of 0 -> #sc_box_shopBoxDtl{}; _ -> CDInfo end,
%%	Now = util:now(),
%%	CD = data_box:get(box_shop_cd),
%%	Type2 = if Type == 3 -> 0;Type==4 -> 0;true->Type end,
%%	OL = [X||{Tab0,Type0,_}=X<-OpenCardInfo,Tab0 /= Tab orelse Type0/=Type2],
%%	Result = case Reward of 
%%				 []-> NewCDInfo = CDInfo2,OpenCardInfo2 = OL,3;
%%				 _ ->NewCDInfo = erlang:setelement(EleN, CDInfo2, Now+CD),OpenCardInfo2=[{Tab,Type2,Reward}|OL], 1  
%%			 end,
%%	
%%	role_data:set_shopBoxData(CardInfo#shop_box_card{openedCardInfo=OpenCardInfo2,cdInfo=NewCDInfo}),
%%	?sendself(#sc_box_shop_refresh{result=Result,reward=role_reward:transform2p_reward_view(Reward,[]),ts=Now+CD}).



cs_box_shop_get(#cs_box_shop_get{tab=_Tab,type=_Type}) ->	
    ?sendself(#sc_box_shop_get{result=2}).
%case check_can_get_box(Tab,Type) of
%	{true,CardInfo,Info,Cost} -> 
%		case Tab of
%			24001 ->LogType = {?MONEY_DEC_TYPE_SHOP_SPIRIT_BOX,?MONEY_ADD_TYPE_SHOP_SPIRITE_BOX};			
%			24002 ->LogType = {?MONEY_DEC_TYPE_SHOP_EQUIP_BOX,?MONEY_ADD_TYPE_SHOP_EQUIP_BOX};
%			24100 -> LogType = {?MONEY_DEC_TYPE_SHOP_TRAINER_EQUIP_BOX,?MONEY_ADD_TYPE_SHOP_TRAINER_EQUIP_BOX}
%		end,
%		do_get_box_reward(CardInfo,Info,Cost,LogType);
%	{false,Reason} -> ?sendself(#sc_box_shop_get{result=Reason})
%end.

%%check_can_get_box(Tab,Type) ->
%%	#shop_box_card{openedCardInfo=OpenCardInfo} =CardInfo= role_data:get_shopBoxData(),
%%	case [X||{Tab0,Type0,_}=X<-OpenCardInfo,Tab0 == Tab,Type0==Type] of
%%		[] -> {false,2};
%%		[{_,_,Reward}=V] -> Cost = data_box:get({shopBoxCost,Tab,Type}),
%%			   case check_cost(Cost,Tab,Type,Reward) of
%%				   {true,gold,NeedGold} -> {true,CardInfo,V,NeedGold};
%%				   {true,item,Args} -> {true,CardInfo,V,Args};
%%				   false -> {false,3}
%%			   end
%%	end.
%%
%%check_cost(gold,Tab,Type,_) -> 
%%	case data_box_price:get(Tab) of
%%		#data_box_price2{} = DataBoxPrice ->
%%			NeedGold = open_need_gold(DataBoxPrice, Type),
%%			case role_lib:check_money(role_data:get_roleInfo(), gold, NeedGold) of
%%				true -> {true,gold,NeedGold};
%%				_ -> false
%%			end;
%%		_ ->
%%			false
%%	end;
%%check_cost({item,ItemTypeID,_Num},_,_,Reward) ->
%%	Num = length(Reward),
%%	BagOther = role_data:get_bagItem(),
%%	case item_lib:check_material2(BagOther,ItemTypeID, Num) of
%%		{BagOther2, 0, DelAcc, UpdateAcc, UpdateLogList}->	{true,item,{BagOther2,Num,DelAcc,UpdateAcc,UpdateLogList}};
%%		_ -> false 
%%	end;
%%check_cost(_,_,_,_) -> false.
%%do_get_box_reward(CardInfo,{Tab,Type,Reward},NeedGold,{LogDecType,LogAddType}) when is_integer(NeedGold) ->
%%	role_lib:deduct_gold_2_f(role_data:get_roleInfo(), NeedGold, LogDecType, Type, "",role_reward:transform2normal_reward(Reward)),
%%	do_get_box_reward2(CardInfo,Tab,Type,Reward,LogAddType);
%%do_get_box_reward(CardInfo,{Tab,Type,Reward},{BagOther,_,DelAcc,UpdateAcc,UpdateLogList},{LogDecType,LogAddType})-> 
%%	#role{roleID=RoleID} = role_data:get_roleInfo(),
%%	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
%%	{Date, _} = Time = erlang:localtime(),
%%		behavior_item_consume:log(RoleID, LogItemList, Date, Time, LogDecType, Type, ""),
%%		case UpdateAcc =/= [] of
%%			true ->
%%				UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
%%				?sendself(#sc_item_update{updateList=UpdateList});
%%			_ -> ignore
%%		end,
%%	DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
%%	?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
%%	role_data:set_bagItem(BagOther),
%%	do_get_box_reward2(CardInfo,Tab,Type,Reward,LogAddType).
%%do_get_box_reward2(CardInfo,Tab,Type,Reward,LogType)->
%%	EleN = get_cd_ele_num(Tab,Type),
%%	#shop_box_card{openedCardInfo=OpenCardInfo,cdInfo=CDInfo}=CardInfo,
%%	CDInfo2 = case CDInfo of 0 -> #sc_box_shopBoxDtl{}; _ -> CDInfo end,
%%	NewCDInfo = erlang:setelement(EleN, CDInfo2, 0),
%%	NewOpenedCard2 = [X||{Tab0,Type0,_}=X<-OpenCardInfo,Tab0 /= Tab orelse Type0/=Type],
%%	if Type == 1 -> update_tab_counter(Tab); true -> ignore end,%% 普通刷新领取之后,更新次数 
%%	role_data:set_shopBoxData(CardInfo#shop_box_card{openedCardInfo=NewOpenedCard2,cdInfo=NewCDInfo}),
%%	%RewardView = role_reward:transform2p_reward_view(Reward, []),
%%	Role = role_data:get_roleInfo(),
%%	role_reward:handle_sys_reward(Role, Reward, LogType, Type, ""),
%%	?sendself(#sc_box_shop_get{result=1}),
%%	broadcast_box_reward(Role#role.roleName, Reward).


cs_box_free_get(#cs_box_free_get{type=_Type}) ->
    ?sendself(#sc_box_free_get{result=2}).
%%	#box_open_info{lastGerTime=LastGerTime,lastItemTime=LastItemTime,lastTrainerTime=LastTrainerTime} = role_data:get_free_box_info(),
%%	Now = util:now(),
%%	case check_box_free_open(Type,LastGerTime,LastItemTime,LastTrainerTime,Now) of
%%		true ->#shop_box_card{openedCardInfo=OpenCardInfo} =CardInfo= role_data:get_shopBoxData(),
%%			   case [X||{Tab0,Type0,_}=X<-OpenCardInfo,Tab0 == 24000+Type,Type0==1] of
%%				   [] -> ?sendself(#sc_box_free_get{result=3});
%%				   [V] ->
%%					   do_box_free_get(Type,LastGerTime,LastItemTime,LastTrainerTime,Now,V,CardInfo)
%%			   end;
%%		{false,Reason} -> ?sendself(#sc_box_free_get{result=Reason})
%%	end.
%%
%%do_box_free_get(Type, LastGerTime, LastItemTime,LastTrainerTime, Now,{_,_,Reward},CardInfo) ->
%%	Tab = 24000+Type,
%%		case Type of
%%			1 ->
%%				LogType = ?MONEY_ADD_TYPE_SHOP_SPIRITE_BOX,
%%				Info =#box_open_info{lastGerTime=Now, lastItemTime=LastItemTime,lastTrainerTime=LastTrainerTime};
%%			2 ->
%%				LogType = ?MONEY_ADD_TYPE_SHOP_EQUIP_BOX,
%%				Info =#box_open_info{lastGerTime=LastGerTime, lastItemTime=Now,lastTrainerTime=LastTrainerTime};
%%            100 ->
%%                LogType = ?MONEY_ADD_TYPE_SHOP_TRAINER_EQUIP_BOX,
%%                Info =#box_open_info{lastGerTime=LastGerTime, lastItemTime=LastItemTime,lastTrainerTime=Now}
%%		end,
%%	update_tab_counter(Tab),%% 普通刷新领取之后,更新次数
%%	EleN = get_cd_ele_num(Tab,1),
%%	#shop_box_card{openedCardInfo=OpenCardInfo,cdInfo=CDInfo}=CardInfo,
%%	CDInfo2 = case CDInfo of 0 -> #sc_box_shopBoxDtl{}; _ -> CDInfo end,
%%	NewCDInfo = erlang:setelement(EleN, CDInfo2, 0),
%%	NewOpenedCard2 = [X||{Tab0,Type0,_}=X<-OpenCardInfo,Tab0 /= Tab orelse Type0/=1],
%%	role_data:set_shopBoxData(CardInfo#shop_box_card{openedCardInfo=NewOpenedCard2,cdInfo=NewCDInfo}),
%%	Role = role_data:get_roleInfo(),
%%	role_reward:handle_sys_reward(Role, Reward, LogType, Type, ""),
%%	role_data:set_free_box_info(Info),
%%	
%%	?sendself(#sc_box_free_get{result=1}),
%%	cs_box_get_spirit_equip_count(#cs_box_get_spirit_equip_count{}),
%%	?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,1}),E1),
%%	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_SHOP_BOX).


cs_box_get_spirit_equip_count(_)->
    #limitInfo{spiritGoldBoxCount = SpiritGoldBoxCount
               ,spiritGoldBonusBoxCount=SpiritGoldBonusBoxCount
               ,equipGoldBoxCount = EquipGoldBoxCount
               ,equipGoldBonusBoxCount=EquipGoldBonusBoxCount
               ,trainerGoldBoxCount = TrainerGoldBoxCount
               ,trainerGoldBonusBoxCount=TrainerGoldBonusBoxCount} = role_data:get_limitInfo(),
    Count1 =10 -((SpiritGoldBoxCount+SpiritGoldBonusBoxCount) rem 10),
    Count2 =10-((EquipGoldBoxCount+EquipGoldBonusBoxCount) rem 10),
    Count3 =10-((TrainerGoldBoxCount+TrainerGoldBonusBoxCount) rem 10),
    {Type1,Num1} = data_fixed_box_withitem:get({need_item,?TAB_TYPE_SPIRIT}),
    {Type2,Num2} = data_fixed_box_withitem:get({need_item,?TAB_TYPE_EQUIP}),
    {Type3,Num3} = data_fixed_box_withitem:get({need_item,?TAB_TYPE_TRAINER}),

    RecordData = #sc_box_get_spirit_equip_count{count1=Count1,count2=Count2,needItemTypeID1=Type1,needNum1=Num1,needItemTypeID2=Type2,needNum2=Num2
                                                ,count3=Count3,needItemTypeID3=Type3,needNum3=Num3},
    ?DEBUG("玩家次数信息:~w",[{RecordData,SpiritGoldBoxCount+SpiritGoldBonusBoxCount,EquipGoldBoxCount+EquipGoldBonusBoxCount}]),
    ?sendself(RecordData).

cs_box_item(#cs_box_item{itemTypeID=ItemTypeID}) ->
    do_open_box(ItemTypeID, 1).

cs_box_item_multi(#cs_box_item_multi{itemTypeID=ItemTypeID,itemNum = Num}) ->
    ItemNum = item_lib:get_material_num(ItemTypeID),
    Max = data_box:get(multi_open_num),
    Num2 = if
               Num < Max ->Num;
               true -> Max
           end,
    Num3 = if
               Num2 < ItemNum ->Num2;
               true -> ItemNum
           end,
    do_open_box(ItemTypeID, Num3).

do_open_box(ItemTypeID, Num) ->
    CheckFun = 
    %% 区分普通宝箱和多个随机宝箱
    case is_more_box(ItemTypeID) of
        true ->
            fun check_more_box_item/2;
        _ ->
            fun check_box_item/2 
    end,
    case erlang:apply(CheckFun, [ItemTypeID, Num]) of
        {true, BagOther2, Reward, DelAcc, UpdateItemLogAcc} ->
            do_box_item(BagOther2, Reward, ItemTypeID, DelAcc, UpdateItemLogAcc,Num);
        {false, Reason} ->
            case Num of
                1 ->
                    ?sendself(#sc_box_item{result=Reason,reward=[],itemTypeID=ItemTypeID});
                _ ->
                    ?sendself(#sc_box_item_multi{result=Reason,reward=[],itemTypeID=ItemTypeID})
            end
    end.

cs_box_shop(#cs_box_shop{tab=Tab,type=Type}) ->
    case check_box_shop(Tab,Type) of
        {?OPEN_BOX_ITEM,BagOther,_,DelAcc,UpdataAcc,UpdateLogList}->
            do_box_shop_by_item(Tab,Type,BagOther,1,DelAcc,UpdataAcc,UpdateLogList);
        {?OPEN_BOX_ITEM_10,BagOther,RealNum,DelAcc,UpdataAcc,UpdateLogList}->
            do_box_shop_by_item(Tab,Type,BagOther,RealNum,DelAcc,UpdataAcc,UpdateLogList);
        {?OPEN_BOX_ITEM_100,BagOther,RealNum,DelAcc,UpdataAcc,UpdateLogList}->
            do_box_shop_by_item(Tab,Type,BagOther,RealNum,DelAcc,UpdataAcc,UpdateLogList);
        {_, Role, NeedGold,Use} ->
            do_box_shop_by_gold(Tab,Type, Role, NeedGold,Use);
        {false, Reason} ->
            ?DEBUG("========错误===~w",[Reason]),
            ?sendself(#sc_box_shop{result=Reason, reward=[]})
    end.

cs_box_shop_info(_) ->
    List = [data_box_price:get(E)||E<-data_box_price:get_list()],
    ShopInfo = [#p_shop_box_info{itemTypeID=ItemTypeID,valueOne=ValueOne,valueTen=ValueTen,discount=Discount,endtime=EndTime,isOpenActivity=IsOpenActivity} || #data_box_price2{itemTypeID=ItemTypeID,oncePrice=ValueOne,tenTimesPrice=ValueTen,discount=Discount,endtime=EndTime,isOpenActivity=IsOpenActivity}<-List],
    ?sendself(#sc_box_shop_info{info=ShopInfo}),
    cs_box_get_spirit_equip_count(#cs_box_get_spirit_equip_count{}).

cs_box_free_info(_) ->
    %#box_open_info{lastGerTime=LastGerTime,lastItemTime=LastItemTime,lastTrainerTime=LastTrainerTime} = role_data:get_free_box_info(),
    %FreeInterval = data_box:get(box_free_interval),
    %?sendself(#sc_box_free_info{lastBoxGerTime=LastGerTime,lastBoxItemTime=LastItemTime,lastTrainerTime=LastTrainerTime, interval=FreeInterval}).
    #box_open_info{gerTime1=GT1,gerTime2=GT2,itemTime1=IT1,itemTime2=IT2,trainerTime1=TT1,trainerTime2=TT2}=role_data:get_free_box_info(),
    {ITM,BTM} = data_box:get(free_box_times),
    ?sendself(#sc_box_free_info{gerTime1=GT1,gerTime2=GT2,itemTime1=IT1,itemTime2=IT2,trainerTime1=TT1
                                ,trainerTime2=TT2,itemfreeTimeMax=ITM,buyfreeTimeMax=BTM}).

cs_box_free_open(#cs_box_free_open{type=Type}) ->
    FreeBoxInfo = role_data:get_free_box_info(),
    case check_box_free_open(Type,FreeBoxInfo) of
        %	case check_box_free_open(Type,LastGerTime, LastItemTime, LastTrainerTime,Now) of
        {true,EleN,NowTimes,OpenTimes} ->
            do_box_free_open(Type,EleN,NowTimes,OpenTimes,FreeBoxInfo);
        {false, Reason} ->
            ?sendself(#sc_box_free_open{result=Reason, reward=[]})
    end.

cs_box_ticket_info(_) ->
    ?sendself(#sc_box_ticket_info{cost1=data_ticket_box:get(ticket_box_cost)
                                  ,cost10 = data_ticket_box:get(ticket_box_10_cost)}).

cs_box_ticket_shop(#cs_box_ticket_shop{type=Type}) ->
    case check_ticket_shop(Type) of
        {true,Times,OneTimeCost,Role} ->
            RewardList = get_box_reward(Times),
            TotalCost = 
            case Times of 10 -> data_ticket_box:get(ticket_box_10_cost);
                          _ -> Times * OneTimeCost
            end,
            Role2 = role_lib:deduct_ticket_f(Role,TotalCost,?MONEY_DEC_TYPE_TICKET_BOX,Type,erlang:integer_to_list(Times)),
            role_reward:handle_sys_reward(Role2, RewardList, ?MONEY_ADD_TYPE_SHOP_TICKET_BOX, Type,erlang:integer_to_list(Times)),
            RewardView = role_reward:transform2p_reward_view(RewardList, []),
            ?sendself(#sc_box_ticket_shop{result=1, reward=util:random_list(RewardView)}),
            broadcast_box_reward(Role#role.roleName, RewardList),
            ?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,1}),E1),
            role_invite:do_add_weibo_share_mark(?SHARE_TYPE_SHOP_BOX);
        {false,Reason} ->
            ?sendself(#sc_box_ticket_shop{result=Reason,reward=[]})
    end.

check_ticket_shop(Type) ->
    Max = case Type of 1 -> 1; 2 -> 10 end,
    #role{ticket=RoleTicket}=Role = role_data:get_roleInfo(),
    OneTimeCost = data_ticket_box:get(ticket_box_cost),
    TenCost = data_ticket_box:get(ticket_box_10_cost),
    MaxCost = 
    case Type of 1 -> OneTimeCost;
                 2 -> TenCost
    end,
    if RoleTicket >= MaxCost -> {true,Max,OneTimeCost,Role};
       true -> {false,2}
    end.
%% 	if RoleTicket >= MaxCost -> {true,Max,OneTimeCost,Role};
%% 	   true -> if RoleTicket < OneTimeCost -> {false,2};
%% 				  true -> {true, RoleTicket div OneTimeCost , OneTimeCost,Role}
%% 			   end
%% 	end.

get_box_reward(10)->
    N = ?upcounter_limitInfo(ticketBoxCount2),
    case data_fixed_ticket_box:get({2,N}) of
        ?undefined -> get_box_reward2(10);
        IDList ->
            [util:random_one_from_weigh_list(
               util:random_one_from_weigh_list(
                 data_ticket_box:get(BoxID)))||BoxID<-IDList]
    end;				
get_box_reward(Num) -> get_box_reward2(Num).

get_box_reward2(Num) ->
    [begin
         N = ?upcounter_limitInfo(ticketBoxCount),
         N2 = if N =< 10 -> {1,N-1}; true -> {3,N rem 10} end, 
         BoxID =  case data_fixed_ticket_box:get(N2) of
                      ?undefined -> data_ticket_box:get(default_box_id);
                      X -> X
                  end,
         util:random_one_from_weigh_list(
           util:random_one_from_weigh_list(
             data_ticket_box:get(BoxID)))
     end||_<-lists:seq(1,Num)].


%% ====================================================================
%% Internal functions
%% ====================================================================
%%do_box_free_open(Type, LastGerTime, LastItemTime,LastTrainerTime, Now) ->
do_box_free_open(Type,EleN,NowTimes ,OpenTimes,FreeBoxInfo)->
    MainGerTypeID = role_data:get_mainGerTypeID(),
    LogType =
        case Type rem 10 of
            1 -> ?MONEY_ADD_TYPE_SHOP_SPIRITE_BOX;
            2 -> ?MONEY_ADD_TYPE_SHOP_EQUIP_BOX;
            _ -> ?MONEY_ADD_TYPE_SHOP_TRAINER_EQUIP_BOX
        end,
    %Type2 = case Type div 10 rem 10 == 1 of true -> Type - 10; _ -> Type end,
    %BoxID = 24000+Type2,
    BoxID = get_free_box_id(Type),
    Info = erlang:setelement(EleN,FreeBoxInfo,NowTimes+OpenTimes),
    ?INFO("BoxID:~w,MainGerTypeID:~w  , ~w",[BoxID, MainGerTypeID,OpenTimes]),
	[R1|_] = data_box:get({BoxID, MainGerTypeID}),
	RewardList = [util:random_one_from_weigh_list(R1)||_<-lists:seq(1, OpenTimes)],
	RewardView = role_reward:transform2p_reward_view(RewardList, []),
	Role = role_data:get_roleInfo(),
	role_data:set_free_box_info(Info),
	role_reward:handle_sys_reward(Role, RewardList, LogType, Type, ""),
	?sendself(#sc_box_free_open{result=1, reward=RewardView}),
	cs_box_get_spirit_equip_count(#cs_box_get_spirit_equip_count{}),
	broadcast_box_reward(Role#role.roleName, RewardList),
	?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,1}),E1),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_SHOP_BOX).

get_free_box_id(Type) ->
    %Type2 = Type rem 100 ,
    Type3 = 24000 + case Type div 10 rem 10 == 1 of true -> Type - 10; _ -> Type end,
    if Type > 10 andalso Type /= 100 ->
           NewCount1 =
               case Type3 of
                   ?TAB_TYPE_SPIRIT-> ?upcounter_limitInfo(spiritGoldBonusBoxCount,spiritGoldBoxCount);
                   ?TAB_TYPE_EQUIP-> ?upcounter_limitInfo(equipGoldBonusBoxCount,equipGoldBoxCount);
                   ?TAB_TYPE_TRAINER-> ?upcounter_limitInfo(trainerGoldBonusBoxCount,trainerGoldBoxCount)
               end,
           case NewCount1 rem 10 =:= 0 of
               true->
                   case data_fixed_box_withitem:get({five_star_box,Type3}) of
                       ?undefined-> Type3;
                       B->B
                   end;
               _ ->
                   case data_fixed_goldbonus_box:get({NewCount1,Type3}) of
                       ?undefined -> Type3;
                       B -> B
                   end
           end;
       true -> Type3
    end.

refresh_box_free()->
    role_data:set_free_box_info(#box_open_info{}).

get_box_info_elementN(1) -> 2;
get_box_info_elementN(2) -> 4;
get_box_info_elementN(11)-> 3;
get_box_info_elementN(12)-> 5;
get_box_info_elementN(100)->6;
get_box_info_elementN(110)->7;
get_box_info_elementN(_Type)-> 2.

get_box_free_max_free_times(Type) ->
    {IFM, BFM} = data_box:get(free_box_times),
    Check = Type div 10 rem 10,
    if Check == 1 -> BFM; true -> IFM end.
	
check_box_free_open(Type,FreeBoxInfo)->
    EleN = get_box_info_elementN(Type), 
    MaxTimes = get_box_free_max_free_times(Type),
    NowTimes = element(EleN,FreeBoxInfo),
    if NowTimes >= MaxTimes -> {false,2};
       true -> {true,EleN,NowTimes,MaxTimes - NowTimes}
    end.

%%check_box_free_open(Type, LastGerTime,LastItemTime, LastTrainerTime, Now) ->
%%	Interval = data_box:get(box_free_interval),
%%	case Type of
%%		1 ->
%%			if Now - LastGerTime >= Interval ->
%%				   true;
%%			   true ->
%%				   {false,2}
%%			end;
%%		2 ->
%%			if Now - LastItemTime >= Interval ->
%%				   true ;
%%			   true ->
%%				   {false, 2}
%%			end;
%%        100 ->
%%            if Now - LastTrainerTime >= Interval ->
%%                   true ;
%%               true ->
%%                   {false, 2}
%%            end;
%%		_ ->
%%			{false,3}
%%	end.

open_need_gold(DataBoxPrice, ?OPEN_BOX_GOLD_1) ->
	DataBoxPrice#data_box_price2.oncePrice;
open_need_gold(DataBoxPrice, ?OPEN_BOX_GOLD_10) ->
	DataBoxPrice#data_box_price2.tenTimesPrice.


random_reward(Config, Times, List) when Times > 0->
	R = util:random_one_from_weigh_list(Config),
	random_reward(Config, Times-1, [R|List]);
random_reward(_, _, List) ->
	List.

										  

do_box_shop_by_gold(Tab,Type, Role, NeedGold,Use) ->
	MainGerTypeID = role_data:get_mainGerTypeID(),
    case Tab of
        ?TAB_TYPE_SPIRIT->
            LogItemType1 = ?MONEY_DEC_TYPE_SHOP_SPIRIT_BOX,
            LogType = ?MONEY_ADD_TYPE_SHOP_SPIRITE_BOX;
        ?TAB_TYPE_EQUIP->
            LogItemType1 = ?MONEY_DEC_TYPE_SHOP_EQUIP_BOX,
            LogType = ?MONEY_ADD_TYPE_SHOP_EQUIP_BOX;
        ?TAB_TYPE_TRAINER->
            LogItemType1 = ?MONEY_DEC_TYPE_SHOP_TRAINER_EQUIP_BOX,
            LogType = ?MONEY_ADD_TYPE_SHOP_TRAINER_EQUIP_BOX
    end,
	case Type of
		?OPEN_BOX_GOLD_1 ->
			?DEBUG("之前次数信息:~w",[role_data:get_limitInfo()]),
			NewCount1 =
				case Role#role.gold=:=0 of
					true->
						case Tab of
							?TAB_TYPE_SPIRIT->
								?upcounter_limitInfo(spiritGoldBonusBoxCount,spiritGoldBoxCount);
							?TAB_TYPE_EQUIP->
								?upcounter_limitInfo(equipGoldBonusBoxCount,equipGoldBoxCount);
                            ?TAB_TYPE_TRAINER->
                                ?upcounter_limitInfo(trainerGoldBonusBoxCount,trainerGoldBoxCount)
						end;
					false->
						case Tab of
							?TAB_TYPE_SPIRIT->
								?upcounter_limitInfo(spiritGoldBoxCount,spiritGoldBonusBoxCount);
							?TAB_TYPE_EQUIP->
								?upcounter_limitInfo(equipGoldBoxCount,equipGoldBonusBoxCount);
                            ?TAB_TYPE_TRAINER->
                                ?upcounter_limitInfo(trainerGoldBoxCount,trainerGoldBonusBoxCount)
                        end
                end,
            ?DEBUG("之后次数信息:~w",[role_data:get_limitInfo()]),
            BoxID =
                case NewCount1 rem 10 =:= 0 of
                    true->
                        case data_fixed_box_withitem:get({five_star_box,Tab}) of
                            ?undefined->
                                Tab;
                            B->
                                B
                        end;
                    _->
                        case Use of
                            goldBonus->
                                case data_fixed_goldbonus_box:get({NewCount1, Tab}) of
                                    ?undefined ->
                                        Tab;
                                    B1 ->
                                        B1
                                end;
                            gold->
                                case data_fixed_gold_box:get({NewCount1, Tab}) of
                                    ?undefined ->
                                        Tab;
                                    B1 ->
                                        B1
                                end
                        end
                end,
            %%             ExtraReward = case Tab of
            %%                 ?TAB_TYPE_SPIRIT ->
            %%                     get_box_extra_reward(ger_by_gold);
            %%                 ?TAB_TYPE_EQUIP ->
            %%                     get_box_extra_reward(equip_by_gold);
            %%                 ?TAB_TYPE_TRAINER ->
            %%                     get_box_extra_reward(trainer_by_gold)
            %%             end,
            %% 			?DEBUG("抽取的宝箱:~w",[{BoxID,MainGerTypeID}]),
            [R1|_] = data_box:get({BoxID, MainGerTypeID}),
            RewardList2 = [util:random_one_from_weigh_list(R1)];
        ?OPEN_BOX_GOLD_10->
            [R1, R2, R3, R4, R5,R6] = data_box:get({Tab, MainGerTypeID}),
            {R1T, R2T, R3T, R4T, R5T,R6T} = data_box:get(open_box_10),
            RL1 = random_reward(R1, R1T, []),
            RL2 = random_reward(R2, R2T, RL1),
            RL3 = random_reward(R3, R3T, RL2),
            RL4 = random_reward(R4, R4T, RL3),
            %RewardList1 = random_reward(R5, R5T, RL4),
            RL5 = random_reward(R5, R5T, RL4),
            RewardList1 = random_reward(R6, R6T, RL5),
            RewardList2 = util:random_list2(RewardList1),
            
            %%触发成就、获得额外奖励
            _ExtraReward = case Tab of 
                ?TAB_TYPE_SPIRIT ->
                    ?CATCH(role_task_trigger:handle({dispach_task, role_box_ger_10})),
                    get_box_extra_reward(ger_by_gold_10);
                ?TAB_TYPE_EQUIP ->
                    ?CATCH(role_task_trigger:handle({dispach_task, role_box_equip_10})),
                    get_box_extra_reward(equip_by_gold_10);
                ?TAB_TYPE_TRAINER ->
                    ?CATCH(role_task_trigger:handle({dispach_task, role_box_trainer_10})),
                    get_box_extra_reward(trainer_by_gold_10)
            end
	end,
	%RewardList = ExtraReward ++ do_random_amount(gold,RewardList2,Tab),
	%RewardView = role_reward:transform2p_reward_view(RewardList -- ExtraReward, []),
    RewardList = do_random_amount(gold,RewardList2,Tab),
    RewardView = role_reward:transform2p_reward_view(RewardList, []),
    %%扣除消耗
	Role2 = role_lib:deduct_gold_2_f(Role, NeedGold, LogItemType1, Type, "",role_reward:transform2normal_reward(RewardList)),
	role_reward:handle_sys_reward(Role2, RewardList, LogType, Type, ""),
	?sendself(#sc_box_shop{result=1, reward=RewardView}),
	cs_box_get_spirit_equip_count(#cs_box_get_spirit_equip_count{}),
	broadcast_box_reward(Role2#role.roleName, RewardList),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_SHOP_BOX),
    catch role_task_trigger:handle({dispach_task,role_box_by_gold}),
	case Type of
		?OPEN_BOX_GOLD_1->
			?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,1}),E1);
		?OPEN_BOX_GOLD_10->
			?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,10}),E2)
	end.

do_box_shop_by_item(Tab,Type,BagOther,RealNum,DelAcc,UpdateAcc,UpdateLogList)->
	MainGerTypeID = role_data:get_mainGerTypeID(),
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	%% 写道具日志
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
	{Date, _} = Time = erlang:localtime(),
	{_ExtraReward,LogDecType,LogAddType} = case Tab of
		?TAB_TYPE_SPIRIT->
            catch role_task_trigger:handle({dispach_task,mixing_condition,1}),
            {get_total_reward(get_box_extra_reward(ger_by_item),Type,RealNum)
            ,?MONEY_DEC_TYPE_SHOP_SPIRIT_BOX
             ,?MONEY_ADD_TYPE_SHOP_SPIRITE_BOX};
		?TAB_TYPE_EQUIP->
            catch role_task_trigger:handle({dispach_task,mixing_condition,2}),
            {get_total_reward(get_box_extra_reward(equip_by_item),Type,RealNum),
			?MONEY_DEC_TYPE_SHOP_EQUIP_BOX,
			?MONEY_ADD_TYPE_SHOP_EQUIP_BOX};
        ?TAB_TYPE_TRAINER->
            {get_total_reward(get_box_extra_reward(equip_by_item),Type,RealNum),
            ?MONEY_DEC_TYPE_SHOP_TRAINER_EQUIP_BOX,
            ?MONEY_ADD_TYPE_SHOP_TRAINER_EQUIP_BOX}
	end,
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, LogDecType, Type, ""),
	case Type of
		?OPEN_BOX_ITEM->
			BoxIDList = get_boxid_list_by_item(Tab,[],1);
		?OPEN_BOX_ITEM_10 ->
			BoxIDList = get_boxid_list_by_item(Tab,[],RealNum);
		?OPEN_BOX_ITEM_100 ->
			BoxIDList = get_boxid_list_by_item(Tab,[],RealNum)
	end,
	ConfigList = lists:foldl(fun(BoxID,Acc)->
		[R|_] = data_box:get({BoxID,MainGerTypeID}),
		[R|Acc]
	end,[],BoxIDList),
	RewardList2 = [util:random_one_from_weigh_list(R1)||R1<-ConfigList],
	?INFO("RewardList:~w ~n",[RewardList2]),
    %RewardList = ExtraReward ++ do_random_amount(item,RewardList2,Tab),
    RewardList = do_random_amount(item,RewardList2,Tab),
    %%更新物品相关数据
    case UpdateAcc =/= [] of
		true ->
			UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
			?sendself(#sc_item_update{updateList=UpdateList});
		_ ->
			ignore
	end,
	DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
	?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
	?DEBUG("之前次数信息:~w",[role_data:get_limitInfo()]),

	role_data:set_bagItem(BagOther),
	?DEBUG("奖励信息:~w",[{LogAddType,RewardList}]),
	role_reward:handle_sys_reward(Role, RewardList, LogAddType, Type, ""),
    %?sendself(#sc_box_shop{result=1,reward=role_reward:transform2p_reward_view(RewardList -- ExtraReward, [])}),
    ?sendself(#sc_box_shop{result=1,reward=role_reward:transform2p_reward_view(RewardList, [])}),
	broadcast_box_reward(Role#role.roleName, RewardList),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_BAG_BOX),
    case Type of
        ?OPEN_BOX_ITEM->
            ?CATCH(role_task_trigger:handle({dispach_task,role_extract_card,1}));
        ?OPEN_BOX_ITEM_10->
            ?CATCH(role_task_trigger:handle({dispach_task,role_extract_card,RealNum}));
	?OPEN_BOX_ITEM_100->
            ?CATCH(role_task_trigger:handle({dispach_task,role_extract_card,RealNum}))
    end.

broadcast_box_reward(RoleName, RewardList)	->
%将奖励列表读取出来，然后对4星以上武将进行广播   
	?INFO("Reward in broadcast_box_reward is ~w ~n",[RewardList]),
    RewardList2 = role_reward:transform2normal_reward(RewardList),
    RewardListL = erlang:size(RewardList2),
    ItemList = erlang:element(RewardListL - 1, RewardList2),
    GerList = erlang:element(RewardListL , RewardList2),
%% 	{_, _, _, _, _, _,ItemList, GerList} = role_reward:transform2normal_reward(RewardList),
	lists:foreach(fun(X)->#new_ger{gerTypeID=GerTypeId, gerLevel=GerLevel, gerQuality=GerQuality}=X,
						  #data_ger{gerStar=GerStar}=data_ger:get(GerTypeId),
						  if	GerStar >= 5	->
									GerView=#p_ger_view{gerQuality=GerQuality, gerLevel=GerLevel, gerTypeID=GerTypeId},
									broadcast_server:bc_msgID(10013, [RoleName, GerView, "1"]);
								true	->
									ok
						  end
				  end, GerList),
	lists:foreach(fun(X)->
						  #new_item{itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=ItemRank,itemNum=ItemNum}=X,
						  #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
						  if	ItemStar >= 6	->
									ItemView = #p_item_view{itemLevel=ItemLevel,itemNum=ItemNum,itemRank=ItemRank,itemTypeID=ItemTypeID},
									broadcast_server:bc_msgID(10032, [RoleName, ItemView, "1"]);
								true	->
									ok
						  end
				  end, ItemList).

check_box_shop(Tab,Type) ->
	case check_box_shop_param(Tab,Type) of
		true->
			case lists:member(Type,[?OPEN_BOX_ITEM,?OPEN_BOX_ITEM_10,?OPEN_BOX_ITEM_100]) of
				true->
					case data_fixed_box_withitem:get({need_item,Tab}) of
						{ItemTypeID,Num}->
						 	RealNum = 
						 		case Type of
						 			?OPEN_BOX_ITEM ->
						 				Num;
						 			?OPEN_BOX_ITEM_10 ->
						 				data_fixed_box_withitem:get(need_item_number);
									?OPEN_BOX_ITEM_100 ->
										data_fixed_box_withitem:get(need_item_number_100)
						 		end,
                            BagOther = role_data:get_bagItem(),
							case item_lib:check_material2(BagOther,ItemTypeID, RealNum) of
								{_BagOther2, RealNum, _DelAcc, _UpdateAcc, _UpdateLogList}->
									?DEBUG("======>>>>>~w",[role_data:get_bagItem()]),
									{false,3};
								{BagOther2, RestNum, DelAcc, UpdateAcc, UpdateLogList}->
									case Type of
                                        ?OPEN_BOX_ITEM ->
                                            {Type,BagOther2,1,DelAcc,UpdateAcc,UpdateLogList};
										?OPEN_BOX_ITEM_10 ->
											{Type,BagOther2,10-RestNum,DelAcc,UpdateAcc,UpdateLogList};
										?OPEN_BOX_ITEM_100 ->
											{Type,BagOther2,100-RestNum,DelAcc,UpdateAcc,UpdateLogList}
									end
							end;
						_->
							{false,4}
					end;
				false->
					case data_box_price:get(Tab) of
						#data_box_price2{} = DataBoxPrice ->
							NeedGold = open_need_gold(DataBoxPrice, Type),
							Role = role_data:get_roleInfo(),
							case role_lib:check_money(Role, gold, NeedGold) of
								true ->
									case Role#role.gold=:=0 of
										true->
											{Type, Role, NeedGold,goldBonus};
										false->
											{Type, Role, NeedGold,gold}
									end;
								false ->
									{false, 2}
							end;
						_->
							{false,4}
					end
			end;
		false->
			{false,4}
	end.

check_box_shop_param(Tab,Type)->
	(Tab=:=?TAB_TYPE_SPIRIT orelse Tab=:=?TAB_TYPE_EQUIP orelse Tab=:=?TAB_TYPE_TRAINER) 
        andalso (Type=:=?OPEN_BOX_ITEM orelse Type=:=?OPEN_BOX_GOLD_1 
                 orelse Type=:=?OPEN_BOX_GOLD_10 orelse Type=:=?OPEN_BOX_ITEM_10
				 orelse Type==?OPEN_BOX_ITEM_100).

do_box_item(BagOther2, Reward, ItemTypeID, DelAcc, UpdateItemLogAcc,Num) ->
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	%% 写道具日志
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_BAG_BOX, ItemTypeID, ""),

	role_data:set_bagItem(BagOther2),
	role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_BAG_BOX, ItemTypeID, ""),
	case Num of 
        1 ->
			?sendself(#sc_box_item{result=1,reward=role_reward:transform2p_reward_view(Reward, []),itemTypeID=ItemTypeID});
        _ ->
			?sendself(#sc_box_item_multi{result=1,reward=role_reward:transform2p_reward_view(Reward, []),itemTypeID=ItemTypeID}) 
	end,
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_BAG_BOX).

check_box_item(ItemTypeID,Num) ->
	case data_item:get(ItemTypeID) of   %% 获得box的配置信息
		#data_item{itemType=?box} ->
			case item_lib:check_material(ItemTypeID, Num) of  %% 判断数量是否够多
				{true, BagOther2, DelAcc, _UpdateAcc, UpdateLogList} ->  %% BagOther2是处理后的物品列表
					MainGerTypeID = role_data:get_mainGerTypeID(),
					case data_box:get({ItemTypeID,MainGerTypeID}) of
						[RConfig|_] ->
							Reward = random_reward(RConfig, Num, []),
%% 							Reward = util:random_one_from_weigh_list(RConfig),
							{true, BagOther2, Reward, DelAcc, UpdateLogList};
						_ ->
							{false, 4}
					end;
				false ->
					{false, 2}
			end;
		_ ->
			{false,3}
	end.

check_more_box_item(ItemTypeID,Num) ->
	case data_item:get(ItemTypeID) of   %% 获得box的配置信息
		#data_item{itemType=?box} ->
			case item_lib:check_material(ItemTypeID, Num) of  %% 判断数量是否够多
				{true, BagOther2, DelAcc, _UpdateAcc, UpdateLogList} ->  %% BagOther2是处理后的物品列表
					case data_box_more:get(ItemTypeID) of
						List when is_list(List) ->
							Reward = 
                                lists:foldl(fun(_, AccT) ->
                                                lists:foldl(fun({Weight, RID}, Acc) ->
                                                                Random = random:uniform(10000),
                                                                case Weight >= Random of
                                                                    true ->
                                                                        [RID|Acc];
                                                                    _ ->
                                                                        Acc
                                                                end
                                                            end, AccT, List) 
                                            end, [], lists:seq(1, Num)),
							{true, BagOther2, Reward, DelAcc, UpdateLogList};
						_ ->
							{false, 4}
					end;
				false ->
					{false, 2}
			end;
		_ ->
			{false,3}
	end.

config_format_va(List) ->
	lists:foldl(fun({E,Value},Acc) when is_tuple(E),is_integer(element(1,E))->
						L = tl(tuple_to_list(E)),
						ItemTypeID = element(1,E),
						AddList = [{{ItemTypeID, F},Value}||F<-L],
						AddList++Acc;
				   (E,Acc) ->
						[E|Acc]
				end, [], List).
				
%%随机数量
do_random_amount(Type,RewardList,Tab) ->
    case get_weightlist(Type,Tab) of
        [] ->
            RewardList;
        WeightList ->
            TotalWeight = util:get_total_weight( WeightList ),
            do_random_amount( RewardList, TotalWeight, WeightList, [] )
    end.

do_random_amount( [], _, _, Acc ) ->  Acc;
do_random_amount( [{_,_,Num}=H|T], TotalWeight, WeightList, Acc ) ->
    Random = random:uniform( TotalWeight ),
    Multi = chose_amount(WeightList,Random,0),
    NewNum = Num * Multi,
    NewInfo = setelement( 3, H, NewNum ),
    do_random_amount( T, TotalWeight, WeightList, [NewInfo|Acc] ).

chose_amount( [], Random, Acc ) -> 
	?ERR( "Random:~p is bigger than the total,Acc:~p.~n", [Random,Acc] ),
	1;  %% 异常情况,此时不改变商品倍数
chose_amount( [{ Weight, Num }|T], Random, Acc ) ->
    NewAcc = Acc + Weight,
    if 
        NewAcc >= Random ->
            Num;
        true ->
            chose_amount( T, Random, NewAcc )
    end.

%%获得对应的权值列表
get_weightlist(Type,Tab) ->
    Now = calendar:local_time( ),
    case data_box_crt:get(open_time) of 
        [] ->
            [];
        undefined ->
            [];
        List ->
            case util:fun_take(fun([Begin,End,_]) ->
                            Begin =< Now andalso Now =< End
                            end, List) of
                {value,[_,_,ID],_} ->
                    get_weightlist(Type,Tab,ID); 
                false ->
                    get_weightlist(Type,Tab,0)
            end
    end.

get_weightlist(Type,Tab,ID) ->
    CfgName = case Type of 
        item ->
            case Tab of 
                ?TAB_TYPE_SPIRIT ->
                    open_spirit_amount_by_item;
                ?TAB_TYPE_EQUIP ->
                    open_equip_amount_by_item;
                ?TAB_TYPE_TRAINER ->
                    open_trainer_amount_by_item;
                _ ->
                    undefined
                end;
        gold ->
            case Tab of
                ?TAB_TYPE_SPIRIT ->
                    open_spirit_amount;
                ?TAB_TYPE_EQUIP ->
                    open_equip_amount;
                ?TAB_TYPE_TRAINER ->
                    open_trainer_amount;
                _ ->
                    undefined
                end;
        _ ->
            undefined
    end,
    case List = data_box_crt:get(CfgName) of 
        [] ->
            [];
        undefined ->
            [];
        List ->
            case lists:keyfind(ID,1,List) of
                {ID,WeightList} ->
                    CrtRate = data_home:get({constr_type_card,role_home:get_build_type_level(role_data:get_roleID(),?constr_type_card)}),
                    ?INFO("get_weightlist    ~w  ,,,  ~w",[CrtRate,WeightList]),
                    lists:foldr(fun({R,C},AccList)->
                                        case lists:keysearch(C, 1, CrtRate) of
                                            {value,{C,AddRate}} ->
                                                [{R+AddRate,C}|AccList];
                                            false ->
                                                [{R,C}|AccList]
                                        end
                    end, [], WeightList);
                false ->
                    []
            end
    end.

%% 获得抽卡额外奖励
get_box_extra_reward(Type) ->
    case data_box:get(reward) of
        undefined ->
            [];
        [] ->
            [];
        List ->
            case lists:keyfind(Type,1,List) of
                {Type,Reward} ->
                    [Reward];
                _ ->
                    []
            end
    end.
	
do_mystery_box(Type)->
    Role= role_data:get_roleInfo(),
    {Title,Content,RandomList,LogArg} = case Type of
                     stonechip_fight ->
                         {data_stonechip_fight:get(extra_reward_title)
                         ,data_stonechip_fight:get(extra_reward_content)
                         ,data_stonechip_fight:get(extra_reward),1};
                     pvp ->
                         {data_pvp_mystery:get(extra_reward_title)
                         ,data_pvp_mystery:get(extra_reward_content)
                         ,data_pvp_mystery:get(extra_reward),2};
                     team_pk ->
                         {data_team_pk:get(extra_reward_title)
                         ,data_team_pk:get(extra_reward_content)
                         ,data_team_pk:get(extra_reward),3};
                     _->
                         {"","",[],Type}
                end,
    Reward = lists:merge(random_reward(RandomList,1,[])),
    ?INFO("do_mystery_box:~w",[Reward]),
    case Reward of
        [_|_] ->
            role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_BAG_BOX, LogArg, ""),
            ?sendself(#sc_box_mystery_notice{title=Title,content=Content,reward=role_reward:transform2p_reward_view(Reward, [])});
        _->
            ignore
    end.
 
 %%获得连续抽卡额外奖励,Type表示为10连抽和单抽的类型
 get_total_reward([],_,_) ->
 	[];
 get_total_reward([BaseReward],Type,Num)->
 	case lists:member(Type,[?OPEN_BOX_ITEM_10]) of
 		true ->
 			{RewardType,Reward} = BaseReward,
 			[{RewardType,Reward*Num}];
 		false ->
 			[BaseReward]
 	end.

%%获得道具抽卡对应的箱子ID列表
get_boxid_list_by_item(Tab,ExistBoxIDList,BoxNum)->
	case BoxNum =< 0 of
		true ->
			ExistBoxIDList;
		false ->
			NewCount1 =
				case Tab of
					?TAB_TYPE_SPIRIT->
						?upcounter_limitInfo(spiritItemBoxCount);
					?TAB_TYPE_EQUIP->
						?upcounter_limitInfo(equipItemBoxCount);
					?TAB_TYPE_TRAINER->
						?upcounter_limitInfo(trainerItemBoxCount)
				end,
			BoxID =
				case data_fixed_box_withitem:get({NewCount1,Tab}) of
					?undefined->
						case data_fixed_box_withitem:get({defualt_box,Tab}) of
							?undefined->
								Tab;
							C->
								C
						end;
					B->
						B
				end,
			get_boxid_list_by_item(Tab,[BoxID|ExistBoxIDList],BoxNum-1)
	end.

is_more_box(BoxId) ->
    BoxIdList = data_box_more:get(box_id),
    lists:member(BoxId, BoxIdList).
