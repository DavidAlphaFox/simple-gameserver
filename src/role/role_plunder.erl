%% @author caohongyang
%% @doc 夺宝
%% Created 2013-4-10

-module(role_plunder).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([]).

-define(itemTypeID_protect, 20030). %% 这里需要修改为免战令牌的TypeID
%% 用于缓存自己的碎片争夺战数据，数据格式为{0}
-define(plunder_info,plunder_info). 

request_plunder_info()->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    erlang:send(plunder_server, {request_plunder_info,RoleID}).
set_plunder_info(BuyTimes)->
    put(?plunder_info,{BuyTimes}).
get_plunder_info()->
    get(?plunder_info).

%% 获得基本信息，将vip传给plunder_server,然后传回客户端
cs_plunder_info(_)->
    #role{roleID=RoleID,vipLevel=RoleVip} = role_data:get_roleInfo(),
    RestProtectTimes = lists:sum([E#item.itemNum||E<-role_data:get_bagItem(),E#item.itemTypeID == ?itemTypeID_protect]),
    ?INFO("cs_plunder_info ~w ~w ~w",[RoleID, RoleVip, RestProtectTimes]),
    erlang:send(plunder_server, {cs_plunder_info,RoleID, RoleVip, RestProtectTimes}).
%% 发送战斗请求，将自己的阵容信息发给plunder_server,然后完成战斗
%% 发送战斗请求，将自己的阵容信息发给plunder_server,然后完成战斗
cs_plunder_fight(#cs_plunder_fight{tarRoleID=TarRoleID,targetTypeID=TargetTypeID,position=Position})->
	case check_plunder_sec() of
		true ->
			CanFightStoneList = data_stonechip_fight:get(fight_stone),
			case lists:member(TargetTypeID,CanFightStoneList) of
				false ->
					FightRecord = #sc_fight_request{actionList=[],fighterList=[],result=true},
					Replay = #sc_plunder_fight{roleID=TarRoleID,result=4,fightInfo=[FightRecord],rewardtargetTypeID=0
											   ,add_times=0,buy_price=0,can_buy=0,position=Position},
					?sendself(Replay);
				true ->
					#role{roleID=RoleID} = role_data:get_roleInfo(),
					RoleFighterList = role_data:get_fighter_list(),
					RoleLieuAdd = role_data:get_lieu_add_attr(),
					TrSpecial = role_data:get_trSpecial(),
					#add_attr{gerAttackAddtion=AtkAdd,gerHpMaxAddtion=HpAdd} = RoleLieuAdd,
					{AtkAdd2,HpAdd2} =role_lib:calculate_familyTekHpAndAtkBuff(AtkAdd,HpAdd), 
                    RoletotalAdd = RoleLieuAdd#add_attr{gerAttackAddtion=AtkAdd2,gerHpMaxAddtion=HpAdd2},
					TalentList = role_talent:get_active_talent_list(),
					SkinInfo = role_skin:get_skin_info(),
                    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
					erlang:send(plunder_server, {cs_plunder_fight,RoleID,RoleFighterList,RoletotalAdd,TalentList,TrSpecial,TarRoleID, TargetTypeID,Position,SkinInfo,LegendAddList})
			end;
		_ ->
			Replay = #sc_plunder_fight_ten{roleID=TarRoleID,result=10,fight_result=[],rewardtargetTypeID=0
										   ,add_times=0,buy_price=0,can_buy=0,position=0,mystery_list=[]},
			?sendself(Replay)
	end.

%% 发送战斗请求，将自己的阵容信息发给plunder_server,然后完成战斗，十连夺
cs_plunder_fight_ten(#cs_plunder_fight_ten{tarRoleID=TarRoleID,targetTypeID=TargetTypeID,position=Position})->
	case check_plunder_sec() of
		true ->
    CanFightStoneList = data_stonechip_fight:get(fight_stone),
    case lists:member(TargetTypeID,CanFightStoneList) of
        false ->
            Replay = #sc_plunder_fight_ten{roleID=TarRoleID,result=4,fight_result=[],rewardtargetTypeID=0
                                              ,add_times=0,buy_price=0,can_buy=0,position=0,mystery_list=[]},
            ?sendself(Replay);
        true ->
            #role{roleID=RoleID,level = Level} = role_data:get_roleInfo(),
            RoleFighterList = role_data:get_fighter_list(),
            RoleLieuAdd = role_data:get_lieu_add_attr(),
        	TrSpecial = role_data:get_trSpecial(),
            #add_attr{gerAttackAddtion=AtkAdd,gerHpMaxAddtion=HpAdd} = RoleLieuAdd,
            {AtkAdd2,HpAdd2} =role_lib:calculate_familyTekHpAndAtkBuff(AtkAdd,HpAdd), 
            RoletotalAdd = RoleLieuAdd#add_attr{gerAttackAddtion=AtkAdd2,gerHpMaxAddtion=HpAdd2},
            TalentList = role_talent:get_active_talent_list(),
            SkinInfo = role_skin:get_skin_info(),
            LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
RoleFighterList2 = role_xbattle:get_add_buff(role_data:get_xbattle_data(),RoleFighterList),
            erlang:send(plunder_server, {cs_plunder_fight_ten,RoleID,Level,RoleFighterList2,RoletotalAdd,TalentList,TrSpecial,TarRoleID, TargetTypeID,Position,SkinInfo,LegendAddList})
    end;
    _ ->
                Replay = #sc_plunder_fight_ten{roleID=TarRoleID,result=4,fight_result=[],rewardtargetTypeID=0
                                              ,add_times=0,buy_price=0,can_buy=0,position=0,mystery_list=[]},
            ?sendself(Replay)
    end.

check_plunder_sec()->
	Now = util:now(),
	case erlang:get(last_plunder_fight_sec) of
		X when is_integer(X) ->
			if X < Now ->
				   put(last_plunder_fight_sec,Now+1),
				  true ;
			   true ->
				   
				   false
			end;
		_ ->
			put(last_plunder_fight_sec, Now+1),
			true
	end.

%% 使用保护令牌，令牌从自己的背包中取出。
cs_plunder_use_protect(_)->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    case item_lib:check_material(?itemTypeID_protect,1) of
        false ->
            ?sendself(#sc_plunder_use_protect{result=2});
        {true, BagOther, DelAcc, UpdateAcc, UpdateLogList} ->
            erlang:send(plunder_server, {cs_plunder_use_protect,RoleID}),
            ?sendself(#sc_plunder_use_protect{result=1}),
            role_data:set_bagItem(BagOther),
            LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
            {Date,_} = Time = erlang:localtime(),
            behavior_item_consume:log(RoleID, LogItemList, Date, Time,?MONEY_DEC_TYPE_PLUNDER_PROTECT,0,""),
            if UpdateAcc =/= [] ->
                UpdateInfoList = 
                    lists:map(fun(Update) ->
                                #p_item_num_update{itemNum=Update#item.itemNum, itemUID=Update#item.itemUID}
                             end, UpdateAcc),
                    ?sendself(#sc_item_update{updateList=UpdateInfoList});
                true ->
                    next
            end,
            DelItemIDList = [E||#item{itemUID=E} <- DelAcc],
            ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList})
    end.
%% 购买攻击次数
cs_plunder_buy_attacktime(_)->
    #role{roleID=RoleID,vipLevel=VIPlevel} = RoleInfo = role_data:get_roleInfo(), 
    VipBuyMax = data_stonechip_fight:get({vip,VIPlevel}),
    NeedGold = data_stonechip_fight:get(buy_attack_price),
    {CurBuyTimes} = get_plunder_info(),
    
    if 
        VipBuyMax > CurBuyTimes ->
            case role_lib:check_money(RoleInfo, gold, NeedGold) of
                true ->
                    NewBuyTimes = CurBuyTimes+1,
                    role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_BUY_ATTACK, 0, ""),
                    erlang:send(plunder_server, {do_buy_attacktime,RoleID,VIPlevel}),
                    set_plunder_info(NewBuyTimes),
                    ?sendself(#sc_plunder_buy_attacktime{result=1});
                false ->
                    ?sendself(#sc_plunder_buy_attacktime{result=2})
            end;
        true ->
            ?sendself(#sc_plunder_buy_attacktime{result=3})
    end.

cs_plunder_protect_time(_) ->
	{StartH,StopH} = data_common:get(plunder_protect_hour),
	?sendself(#sc_plunder_protect_time{startH=StartH,stopH=StopH}).
	
