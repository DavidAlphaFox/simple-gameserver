%% @author caohongyang
%% @doc 主公信息基础协议
%% Created 2013-3-4


-module(role_role).
-compile(export_all).
-include("def_role.hrl").
-include("def_mail.hrl").
-include("def_battle.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_role_change_location(#cs_role_change_location{location=Location})->
	DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(Location),[]),
	Len = length(DecodeList),
	case Len>29 of
		true->
			ignore;
		false->
			RoleInfo = role_data:get_roleInfo(),
			NewRoleInfo = RoleInfo#role{location=Location},
			role_data:set_roleInfo(NewRoleInfo)
	end.

cs_role_change_head(#cs_role_change_head{head=Head})->
	case check(Head) of
		true->
			#role{roleID=RoleID,familyID=FamilyID} = RoleInfo = role_data:get_roleInfo(),
			NewRoleInfo = RoleInfo#role{head=Head},
			role_data:set_roleInfo(NewRoleInfo),
			role_lib:update_rolePublic(NewRoleInfo,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data()),
			pvp_server:update_head(RoleID,Head),
			role_generalteam:update_roleinfo(NewRoleInfo),
			family_misc:router_to_family_process(FamilyID,{role_change_head,RoleID,Head}),
			?CATCH(role_task_trigger:handle({dispach_task,role_change_head_times})),
			?sendself(#sc_role_change_head{head=Head});
		{false,Reason}->
			?sendself(#sc_role_change_head{result=Reason})
	end.

check(Head)->
	case Head of
		0->
			true;
        ?SHAPE_BASE + 10->  %% 转生男
            true;
        ?SHAPE_BASE + 11->  %% 转生女
            true;
		_->
			GatherSet = role_data:get_gatherInfo(?GATHER_TYPE_GER),
			GerTypeID = Head rem ?SHAPE_BASE,
			case lists:member(Head,GatherSet) of
				true->
					#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
					case GerStar>=2 of
						true->
							true;
						false->
							{false,3}
					end;
				false->
					case role_skin:has_skinshape(GerTypeID) of
						true->
							true;
						false->
							{false,2}
					end
			end
	end.

cs_role_info(_) ->
	?INFO("request roleInfo:~w",[get(roleID)]),
	%do_exchange_coin(),
	RoleInfo = role_data:get_roleInfo(),
	RoleTimes = role_data:get_roleTimes(),
	RoleTimes2 = refresh_roleTimes(RoleTimes),
	LimitInfo = role_data:get_limitInfo(),
	PushInfo = push_server:get_db_push(RoleInfo#role.roleID),
	MainGerTypeID = role_data:get_main_gerTypeID(),
	Reply = roleInfo2sc_role_info(RoleInfo,RoleTimes2,LimitInfo,PushInfo),
	?sendself(Reply#sc_role_info{main_gertypeid=MainGerTypeID}),
	%% 将服务器基本配置常数发给客户端
	VipLevel = RoleInfo#role.vipLevel,
	MaxDscv = role_lib:get_max_dscv_times(VipLevel),
	MaxEnergy = role_lib:get_max_energy(VipLevel),
	?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
	RoleID=get(roleID),
	role_data:cacl_roleFightPower(),
	hula_server:highlight_push(RoleID),
	nanm_server:highlight_push(RoleID).


do_exchange_coin()->
	RoleInfo = role_data:get_roleInfo(),
	case RoleInfo#role.coin > 2000000000 of
		true ->
			do_exchange_coin(RoleInfo);
		_ ->
			ok
	end.
do_exchange_coin(RoleInfo=#role{coin=Coin}) ->
	if Coin > 1000000000 ->
		   ?ERR("do exchange coin,role:~w,coin:~w",[RoleInfo#role.roleID,Coin]),
		   Reward = #sell_reward{item = [#new_item{itemTypeID = 21038,itemNum=1,itemLevel=1,itemRank=0}]},
		   role_reward:handle_sell_reward_f(RoleInfo,Reward,?MONEY_ADD_TYPE_BUY_COIN_H,0,""),
		   RoleInfo2 = role_lib:deduct_coin_f(RoleInfo,1000000000,?MONEY_DEC_TYPE_BUY_COIN_H,0,""),
		   do_exchange_coin(RoleInfo2);
	   true ->
		   ok
	end.

refresh_roleTimes(RoleTimes) ->
	#roleTimes{lastBuyTimesRefreshDate=Date} = RoleTimes,
	Today = erlang:date(),
	if Today > Date ->
		   RoleTimes#roleTimes{energyBuyTimes=0,coinBuyTimes=0,dscvBuyTimes=0,talentstudyBuyTimes=0,pvpBuyTimes=0,lastBuyTimesRefreshDate=Today,teamPkBuyTimes=0};
	   true ->
		   RoleTimes
	end.

-define(BUY_TYPE_ENERGY,1).
-define(BUY_TYPE_DSCV,2).
-define(BUY_TYPE_PVP,3).
-define(BUY_TYPE_COIN,5).
-define(BUY_TYPE_TALENTSTUDY,7).   %% TODO 购买冷却此处需要梳理函数，删除掉不用的
-define(BUY_TYPE_TEAMPK,8).  %% 3V3 次数
-define(BUY_SINGLE,1).  %% 单个购买
-define(BUY_MULTI,2).   %% 批量购买
cs_role_buy_energy(#cs_role_buy_energy{type=Type}) ->
	do_buy_energy(Type,1,?BUY_SINGLE).

cs_role_multi_buy_energy(#cs_role_multi_buy_energy{type=Type,times=Times}) ->
	do_buy_energy(Type,Times,?BUY_MULTI).

random_reward_crit(Prob) ->
	Random = random:uniform(),
	util:foldl(fun({E,UnitWeigh},Acc) ->
					   if UnitWeigh + Acc > Random ->
							  {return,E};
						  true ->
							  Acc + UnitWeigh
					   end
			   end,0,Prob).

%% 创角时赠送武将
cs_role_select_ger(#cs_role_select_ger{gerTypeID=GerTypeID}) ->
	case check_select_ger(GerTypeID) of
		{true,PosList,LogList,GatherList} ->
			role_data:set_posList(PosList),
			%% 添加主公到图鉴
			role_gather:hook_add_ger_list(GatherList),
			role_gather:hook_add_ger_list_for_manual(GatherList),
			RoleID = role_data:get_roleID(),
			db_sql:log_selectGer(RoleID,GerTypeID),
			role_data:set_main_gerTypeID(GerTypeID),
			db_sql:set_gerList(RoleID,PosList,[],[]),
			{Date,_} = Time = erlang:localtime(),
			behavior_ger_add:log(RoleID,LogList,Date,Time,?MONEY_ADD_TYPE_CREATE_ROLE,0,""),
			?sendself(#sc_ger_update_standlist{posList=ger_lib:gerList2p_ger_pos(PosList)}),
			% 成功
			?sendself(#sc_role_select_ger{result=1}),
			#role{accid=Accid} = role_data:get_roleInfo(),
			gw:post_create_process_to_platform(Accid,RoleID,1),
			
			RoleInfo = role_data:get_roleInfo(),
			pvp_server:init_role_pvp(RoleInfo),
			role_lib:insert_rolePublic(RoleInfo,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data()),
            role_data:add_create_bagData(RoleInfo),
			%?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,10000,[20015]}),E1), %% 刷新腰缠万贯成就
			?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,1,[20040]}),E2), %% 刷新人海战术成就
			%?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,1,[20036]}),E3);  %% 刷新精灵王成就
			%%触发获取精灵获取任务
			role_payGuide:trigger_formation_ger_change(0);
		{false,Reason} ->
			?sendself(#sc_role_select_ger{result=Reason})
	end.

gen_init_ger_data(List) ->
	{PosList,LogList,GatherList} =
		lists:foldr(fun({TypeID,Pos},{AccPosList,AccLogList,AccGatherList}) ->
							#ger{gerBase=#gerBase{gerQuality=GerQuality}} = GerInfo = ger_attr:new_ger(TypeID,1,0,[],[]),
							NewGerInfo = ?change_pos(GerInfo,Pos),
							{[NewGerInfo|AccPosList],[[NewGerInfo#ger.gerID,TypeID,1,0]|AccLogList],[{TypeID,GerQuality}|AccGatherList]}
					end,{[],[],[]},List),
	{true,PosList,LogList,GatherList}.

check_select_ger(GerTypeID) ->
	%% 检查是否是创角状态
	case (role_data:get_posList() =:= [] andalso role_data:get_gerBag() == []) of
		true ->
			case data_common:get({create_ger_list,GerTypeID}) of
				?undefined ->
					{false,3};
				List when erlang:is_list(List) ->
					gen_init_ger_data(List)
			end;
		false ->
			{false,2}
	end.

%% 演示战斗
cs_role_demo_fight(#cs_role_demo_fight{type=Type}) ->
	case data_demo_fight:get(Type) of
		#data_demo_fight{gerID1=MonList} ->
			case data_demo_fight:get(Type + 1) of
				#data_demo_fight{gerID1=MonList2} ->
					{_,FightRecord,_} = role_fight:new(MonList2,MonList,#add_attr{},#add_attr{}),
					?sendself(#sc_role_demo_fight{type=Type,fightInfo=FightRecord});
				_ ->
					FighterList = role_data:get_fighter_list(),
					{_,FightRecord,_} = role_fight:new(FighterList,MonList,#add_attr{},#add_attr{}),
					?sendself(#sc_role_demo_fight{type=Type,fightInfo=FightRecord})
			end;
		_ ->
			ignore 
	end.

%% 新的token
cs_role_token(#cs_role_token{token=Token}) ->
	Token2 = list_to_integer(Token,16),
	Token3 = << Token2:256/integer>>,
	RoleID = role_data:get_roleID(),
	push_server:update_token(RoleID,Token3).

cs_role_buy_coin_value(_)->
	#role{level=Level,vipLevel=VIPlevel} = role_data:get_roleInfo(),
	DataVIP = data_vip:get(VIPlevel),
	Value = get_coin_role_can_buy(Level,1),
	?sendself(#sc_role_buy_coin_value{value=Value,times=DataVIP#data_vip.exchange_money_num}).

check_buy_energy(?BUY_TYPE_COIN, TimesT) ->
	Today = erlang:date(),
	#role{vipLevel=VIPlevel} = RoleInfo = role_data:get_roleInfo(),	
	DataVIP = data_vip:get(VIPlevel),
	#roleTimes{lastBuyTimesRefreshDate=Date} = RoleTimesT = role_data:get_roleTimes(),
	RoleTimes = 
		case Today > Date of
			true ->
				RoleTimesT#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=0,pvpBuyTimes=0,talentstudyBuyTimes=0,teamPkBuyTimes=0};
			_ ->
				RoleTimesT
		end,
	%% 判断次数有没有超过vip上限
	case check_vip_times(?BUY_TYPE_COIN, DataVIP, TimesT) of
		false ->
			{false, 5};
		_ ->
			%% 判断当前还能购买几次
			AlreadyBuy = RoleTimes#roleTimes.coinBuyTimes,
			CanBuyMaxTimes = can_buy_times(DataVIP, ?BUY_TYPE_COIN),
			TimesT1 = erlang:min(CanBuyMaxTimes - AlreadyBuy, TimesT),
			
			%% 已经没有可用次数了
			case TimesT1 =< 0 of
				true ->
					{false, 3};
				_ ->
					%% 判断当前钻石能购买几次
					BuyMoneyList = buy_energy_gold_list(?BUY_TYPE_COIN),
					{NeedGold,CanBuyTimes} = lists:foldl(fun(E, {NeedAcc, _TimesAcc} = Acc) -> 
																 Need = lists:nth(AlreadyBuy + E, BuyMoneyList),
																 NewNeedAcc = NeedAcc + Need,
																 case NewNeedAcc > RoleInfo#role.gold + RoleInfo#role.goldBonus of  
																	 true ->
																		 Acc;
																	 _ ->
																		 {NewNeedAcc, E}
																 end
														 end, {0,0}, lists:seq(1,TimesT1)),
					%% 次数为0说明钱不够
					case CanBuyTimes =:= 0 of
						true ->
							{false, 2};
						_ ->
							NewBuyTimes = AlreadyBuy + CanBuyTimes,
							{true,RoleTimes,RoleInfo,NeedGold,Today,CanBuyTimes,NewBuyTimes,RoleTimes#roleTimes{coinBuyTimes=NewBuyTimes},0,0} 
					end
			end
	end;

check_buy_energy(Type,Times) ->
	case lists:member(Type,[?BUY_TYPE_DSCV,?BUY_TYPE_ENERGY,?BUY_TYPE_PVP,?BUY_TYPE_TALENTSTUDY,?BUY_TYPE_TEAMPK]) of
		true ->
			Today = erlang:date(),
			#role{vipLevel=VIPlevel} = RoleInfo = role_data:get_roleInfo(),	
			DataVIP = data_vip:get(VIPlevel),
			RoleTimes = role_data:get_roleTimes(),
			
			case check_vip_times(Type, DataVIP, Times) of
				true ->
					case new_buy_times(RoleTimes,Type,Today,DataVIP,Times) of
						false ->
							{false,3};
						{true,NewBuyTimes,RoleTimes2,NewValue,Add} when Type =:= ?BUY_TYPE_TALENTSTUDY ->
							{true,RoleTimes,RoleInfo,0,Today,NewBuyTimes,RoleTimes2,NewValue,Add};
						{true,NewBuyTimes,RoleTimes2,NewValue,Add} ->
							BuyMoneyList = buy_energy_gold_list(Type),
							% 现在除了招财,其他一次只能购买一次,所以这儿判断所需金额不用做累加
							% 如果是批量购买,需要从已购买次数起累加到新的已购买次数
							NeedGold = buy_energy_gold(NewBuyTimes-Times+1,NewBuyTimes,BuyMoneyList),%lists:nth(NewBuyTimes, BuyMoneyList),
							case role_lib:check_money(RoleInfo,gold,NeedGold) of
								true ->
									{true,RoleTimes,RoleInfo,NeedGold,Today,NewBuyTimes,RoleTimes2,NewValue,Add};
								false ->
									{false,2}
							end
					end;
				_ ->
					{false, 5}
			end;
		false ->
			{false,4}
	end.

buy_energy_gold(Times,NewBuyTimes,BuyMoneyList) ->
    lists:foldl(fun(N,C) -> lists:nth(N, BuyMoneyList) + C end, 0, lists:seq(Times,NewBuyTimes)).

buy_energy_gold_list(?BUY_TYPE_ENERGY)  -> data_common:get(buy_energy_gold);
buy_energy_gold_list(?BUY_TYPE_DSCV)   -> data_common:get(buy_dscv_gold);
buy_energy_gold_list(?BUY_TYPE_PVP)   -> data_common:get(buy_pvp_gold);
buy_energy_gold_list(?BUY_TYPE_COIN)   -> data_common:get(buy_coin_gold);
buy_energy_gold_list(?BUY_TYPE_TEAMPK)   -> data_common:get(buy_teampk_gold).

%% 探险没超过上限就能买
new_buy_times(RoleTimes,?BUY_TYPE_DSCV,Today,DataVIP,Times) ->
	CanBuyTimes = can_buy_times(DataVIP,?BUY_TYPE_DSCV),
	case CanBuyTimes >= RoleTimes#roleTimes.dscvBuyTimes of
		true ->
			#roleTimes{lastBuyTimesRefreshDate=Date} = RoleTimes,
			{RoleTimes2,NewAlreadyBuy,NewValue,Add} = buy_times(RoleTimes,?BUY_TYPE_DSCV,Today,Date,Times),
			?INFO("CanBuyTimes:~w NewAlreadyBuy:~w",[CanBuyTimes,NewAlreadyBuy]),
			{true,NewAlreadyBuy,RoleTimes2,NewValue,Add};
		_ ->
			false
	end;

new_buy_times(RoleTimes,Type,Today,DataVIP,Times) ->
	#roleTimes{lastBuyTimesRefreshDate=Date} = RoleTimes,
	{RoleTimes2,NewAlreadyBuy,NewValue,Add} = buy_times(RoleTimes,Type,Today,Date,Times),
	CanBuyTimes = can_buy_times(DataVIP,Type),
	?INFO("CanBuyTimes:~w NewAlreadyBuy:~w",[CanBuyTimes,NewAlreadyBuy]),
	if CanBuyTimes >= NewAlreadyBuy ->
		   {true,NewAlreadyBuy,RoleTimes2,NewValue,Add};
	   true ->
		   false
	end.

%% 判断次数是否在vip允许的次数内
%% v3.1.0取消vip允许次数限制
% check_vip_times(?BUY_TYPE_COIN, DataVIP, Times) ->
% 	DataVIP#data_vip.exchange_money_num >= Times;
check_vip_times(_, _, _) ->
	true.

can_buy_times(DataVIP,?BUY_TYPE_COIN)  -> DataVIP#data_vip.coinBuyTimes;
can_buy_times(DataVIP,?BUY_TYPE_ENERGY)  -> DataVIP#data_vip.energyBuyTimes;
can_buy_times(DataVIP,?BUY_TYPE_DSCV)  -> DataVIP#data_vip.dscvBuyTimes;
can_buy_times(DataVIP,?BUY_TYPE_PVP)  -> DataVIP#data_vip.pvpBuyTimes;
can_buy_times(DataVIP,?BUY_TYPE_TALENTSTUDY)  -> DataVIP#data_vip.talentstudyBuyTimes;
can_buy_times(DataVIP,?BUY_TYPE_TEAMPK) -> DataVIP#data_vip.teamPkBuyTimes.


%% 现在就招财需要批量,所以除了招财,其他几个不用处理Times
buy_times(RoleTimes,?BUY_TYPE_ENERGY,Today,Date,Times) -> 		
	Add=data_common:get(buy_energy_recover),
	R=RoleTimes#roleTimes.energy + Add * Times,
	if Today > Date ->
		   T=Times,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=T,dscvBuyTimes=0,pvpBuyTimes=0,talentstudyBuyTimes=0,energy=R,teamPkBuyTimes=0};
	   true ->	
		   T=RoleTimes#roleTimes.energyBuyTimes + Times,
		   RoleTimes2 = RoleTimes#roleTimes{energyBuyTimes=T,energy=R}
	end,
	?CATCH(role_task_trigger:handle({dispach_task,role_buy_energy,1})),
	{RoleTimes2,T,R,Add};
buy_times(RoleTimes,?BUY_TYPE_DSCV,Today,Date,_Times) -> 	
	Add=data_common:get(buy_dscv_recover),		
	R=RoleTimes#roleTimes.discoveryTimes + Add,
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=T,pvpBuyTimes=0,talentstudyBuyTimes=0,discoveryTimes=R,teamPkBuyTimes=0};
	   true ->	
		   T=RoleTimes#roleTimes.dscvBuyTimes + 1,
		   RoleTimes2 = RoleTimes#roleTimes{dscvBuyTimes =T,discoveryTimes=R}
	end,
	{RoleTimes2,T,R,Add};
buy_times(RoleTimes,?BUY_TYPE_PVP,Today,Date,_Times) -> 
	Add=data_common:get(buy_pvp_recover),	
	R=RoleTimes#roleTimes.pvpTimes + Add,	
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=0,pvpBuyTimes=T,talentstudyBuyTimes=0,pvpTimes=R,teamPkBuyTimes=0};
	   true ->	
		   T=RoleTimes#roleTimes.pvpBuyTimes + 1,
		   RoleTimes2 = RoleTimes#roleTimes{pvpBuyTimes =T,pvpTimes=R}
	end,
	{RoleTimes2,T,R,Add};
buy_times(RoleTimes,?BUY_TYPE_TALENTSTUDY,Today,Date,_Times) -> 
	if Today > Date ->
		   T=1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=0,pvpBuyTimes=0,talentstudyBuyTimes=T,teamPkBuyTimes=0};
	   true ->  
		   T=RoleTimes#roleTimes.talentstudyBuyTimes + 1,
		   RoleTimes2 = RoleTimes#roleTimes{talentstudyBuyTimes = T}
	end,
	{RoleTimes2,T,0,0};
buy_times(RoleTimes,?BUY_TYPE_TEAMPK,Today,Date,_Times) -> 
	Add = data_common:get(buy_teampk_point),		
	R = RoleTimes#roleTimes.teamPkTimes + Add,
	if Today > Date ->
		   T = 1,
		   RoleTimes2 = RoleTimes#roleTimes{lastBuyTimesRefreshDate=Today,teamPkTimes=R,coinBuyTimes=0,energyBuyTimes=0,dscvBuyTimes=0,pvpBuyTimes=0,talentstudyBuyTimes=0,teamPkBuyTimes=T};
	   true ->  
		   T= RoleTimes#roleTimes.teamPkBuyTimes + 1,
		   RoleTimes2 = RoleTimes#roleTimes{teamPkTimes = R, teamPkBuyTimes = T}
	end,
	{RoleTimes2,T,R,Add}.

cs_role_get_guide_state(_) ->
	RoleID = role_data:get_roleID(),
	GuideState = db_sql:get_guideState(RoleID),
	case GuideState of
		0 ->
			#role{accid=Accid} = role_data:get_roleInfo(),
			gw:post_create_process_to_platform(Accid,RoleID,2);
		_ ->
			next
	end,
	?sendself(#sc_role_get_guide_state{value=GuideState}).

cs_role_set_guide_state(#cs_role_set_guide_state{value=Value}) ->
	RoleID = role_data:get_roleID(),
	case db_sql:set_guideState(RoleID,Value) of
		{ok,_} ->
			?sendself(#sc_role_set_guide_state{result=1});
		_ ->
			?sendself(#sc_role_set_guide_state{result=2})
	end.

cs_role_log_guide_state(#cs_role_log_guide_state{value=Value}) ->
	erlang:put(?guideVal,Value).

cs_role_setting(_)->
	SettingList = data_common:get(client_setting),
	Reply = #sc_role_setting{idList=SettingList},
	?sendself(Reply).

cs_role_get_energy(#cs_role_get_energy{energy_step=EnergyStep})->
	RoleID = role_data:get_roleID(),
	activity_server:role_energy_activity(RoleID,EnergyStep).

cs_role_weixin_share(_) ->
	#roleTimes{lastWeiXinShareSec=LastWXSec} = RoleTimes = role_data:get_roleTimes(),
	case check_weixin_share_reward(LastWXSec) of
		true ->
			role_data:set_roleTimes(RoleTimes#roleTimes{lastWeiXinShareSec=util:now()}),
			Reward = data_common:get(weixin_share_reward),
			mail_server:send_sys_mail(role_data:get_roleID(),?MAIL_WEIXIN_SHARE_REWARD,[],"",Reward);
		_ ->
			ignore
	end.

cs_role_do_transmigration(_) ->
    OldRoleInfo = role_data:get_roleInfo(),
     case check_do_transmigration_new(OldRoleInfo) of
         {true,_} -> NewRoleInfo  = OldRoleInfo#role{transmigration=1},
                 role_data:set_roleInfo(NewRoleInfo),
                             NextLevel = data_common:get(transmigration_level) + 1,
            role_gm:set_level({set_level,NextLevel}),
                             ?sendself(#sc_role_do_transmigration{result=1}),
            PAttr = setelement(1, data_trainer:get(transmigration_attr_buff), p_lvlSgAttr_attr),
            ?sendself(#sc_lvlSgAttr_inc{type=6,inc=1,attr=PAttr});
         {false,Reason,_} -> ?sendself(#sc_role_do_transmigration{result=Reason})
     end.

cs_role_can_transmigration(_) ->
    OldRoleInfo = role_data:get_roleInfo(),
    case check_do_transmigration_new(OldRoleInfo) of
        {true,Chapter} -> ?sendself(#sc_role_can_transmigration{is_can=1,battle_normal=Chapter, need_level=data_common:get(transmigration_level)
                                                                ,need_ger=data_common:get(transmigration_ger),need_battle_hard=0,battle_hard=0
                                                                ,need_battle_normal=data_common:get(transmigration_battle_normal)});
        {false,_, Chapter} -> ?sendself(#sc_role_can_transmigration{is_can=2,battle_normal=Chapter, need_level=data_common:get(transmigration_level)
                                                                    ,need_ger=data_common:get(transmigration_ger),need_battle_hard=0,battle_hard=0
                                                                    ,need_battle_normal=data_common:get(transmigration_battle_normal)})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
roleInfo2sc_role_info(RoleInfo,RoleTimes,LimitInfo,PushInfo) ->
	case role_data:get_roleAlienInfo() of
		?undefined ->
			AlienTimes = 0,
			LastRecoverTime = 0;
		#alien_info{times=AlienTimes,lastRecoverTime=LastRecoverTime} ->
			next
	end,
	?INFO("RoleTimes:~p",[RoleTimes]),
	#sc_role_info{
				  roleID = RoleInfo#role.roleID 
				  ,roleName = RoleInfo#role.roleName 
				  ,isMale = RoleInfo#role.isMale 
				  ,description = RoleInfo#role.description 
				  ,familyID = RoleInfo#role.familyID 
				  ,level = RoleInfo#role.level 
				  ,exp = RoleInfo#role.exp 
				  ,coin = RoleInfo#role.coin 
				  ,reputation = RoleInfo#role.reputation 
				  ,gold = RoleInfo#role.gold 
				  ,goldBonus = RoleInfo#role.goldBonus 
				  ,goldUsed = RoleInfo#role.goldUsed 
				  ,vipLevel = RoleInfo#role.vipLevel 
				  ,goldTotalPaid = RoleInfo#role.goldTotalPaid 
				  ,energy = RoleTimes#roleTimes.energy
				  ,energyBuyTimes =RoleTimes#roleTimes.energyBuyTimes
				  ,challengeGodEnergy = RoleTimes#roleTimes.challengeGodEnergy
				  ,challengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes
				  ,nextEnergyTime =RoleTimes#roleTimes.lastEnergyTime %+ get(?currentEnergyIntercal)
				  ,discoveryTimes =RoleTimes#roleTimes.discoveryTimes
				  ,nextDscvTime =RoleTimes#roleTimes.lastDscvTime + get(?currentdiscoveryInterval)
				  ,pvpTimes =RoleTimes#roleTimes.pvpTimes
				  ,plunderTimes = 0 %未使用
				  ,randomPVPTimes =0
				  ,singlePVPTimes =0
				  ,title=RoleInfo#role.title
				  ,encounterFreeNum = LimitInfo#limitInfo.encounterNum
				  ,isPVPPushOpen = PushInfo#d_push.isPVPPushOpen
				  ,isPushNightMute = PushInfo#d_push.isPushNightMute
				  ,dscvBuyTimes = RoleTimes#roleTimes.dscvBuyTimes
				  ,pvpBuyTimes = RoleTimes#roleTimes.pvpBuyTimes
				  ,plunderBuyTimes = 0 %未使用
				  ,coinBuyTimes = RoleTimes#roleTimes.coinBuyTimes
				  ,weiboCount = role_data:get_weibo_count()
				  ,nextPvpTime = RoleTimes#roleTimes.lastPvpTime + get(?currentPvpInterval)
				  ,nextPlunderTime = 0 %未使用
				  ,lastWeiXinShareSec = RoleTimes#roleTimes.lastWeiXinShareSec
				  ,head=RoleInfo#role.head
				  ,payExtReward = RoleInfo#role.payExtReward
				  ,isFailed = RoleInfo#role.isFailed
				  ,alienTimes=AlienTimes
				  ,lastAlienTime=LastRecoverTime + role_lib:to_sec(data_common:get(alien_recover_interval))
				  ,unioncoin = RoleInfo#role.unioncoin
				  ,talentstudyBuyTimes =RoleTimes#roleTimes.talentstudyBuyTimes
				  ,profoundCrystal = RoleInfo#role.profoundCrystal 
				  ,honor = RoleInfo#role.honor 
                  ,pvppoint = RoleInfo#role.pvppoint 
                  ,teamPkTimes = RoleTimes#roleTimes.teamPkTimes 
                  ,teamPkBuyTimes = RoleTimes#roleTimes.teamPkBuyTimes 
                  ,nextTeamPkTime = RoleTimes#roleTimes.lastTeamPkTime + get(?currentTeamPkIntercal) 
                  ,home_resource = RoleInfo#role.home_resource 
				  ,ticket=RoleInfo#role.ticket
                  ,laputastone=RoleInfo#role.laputastone
                  ,transmigration=RoleInfo#role.transmigration
                  ,sGoldTotalPaid=RoleInfo#role.sGoldTotalPaid
                  ,svipLevel=RoleInfo#role.svipLevel
				 }.

get_coin_role_can_buy(RoleLevel,Crit) ->
	case data_coin_buy:get(RoleLevel) of
		Value when erlang:is_integer(Value) ->
			Value * Crit;
		_ ->
			0
	end.


%% 演示战斗配置预处理
data_demo_fight_transform_list(List) ->
	[data_demo_fight_transform(E)||E<-List].

data_demo_fight_transform(E) ->
	#data_demo_fight{gerID1=D1,gerID2=D2,gerID3=D3,gerID4=D4,gerID5=D5,gerID6=D6}=E,
	MonList = role_battle:gen_mon_list(D1,D2,D3,D4,D5,D6),
	E#data_demo_fight{gerID1=MonList,gerID2=0,gerID3=0,gerID4=0,gerID5=0,gerID6=0}.

check_weixin_share_reward(LastWXSec)->
	{LastDate,_} = util:seconds_to_datetime(LastWXSec),
	LastWeekDay = calendar:day_of_the_week(LastDate),
	{RefreshD,RefreshC} = data_common:get(weixin_share_time),
	RefreshSec = util:datetime_to_seconds({LastDate,RefreshC}) + (RefreshD - LastWeekDay) * ?ONE_DAY_SECONDS,
	case RefreshSec > util:now() of 
		true ->
			false;
		_ ->
			true
	end.

cs_role_suggest_open(#cs_role_suggest_open{}) ->
	?sendself(#sc_role_suggest_open{is_open=data_common:get(is_open_suggest)}).

cs_role_suggest(#cs_role_suggest{title=Title,content=Content}) ->
	case catch check_role_suggest(Title,Content) of
		{ok,Accid,RoleID,RoleName} ->
			db_sql:log_suggest(RoleID,Accid,RoleName,Title,Content,erlang:localtime()),
			?sendself(#sc_role_suggest{result=0});
		{error,Reason} ->
			?sendself(#sc_role_suggest{result=Reason})
	end.

cs_role_teamPkTimes_info(_) ->
    Cost = data_common:get(buy_teampk_gold),
    Times = data_common:get(buy_teampk_point),
    Limit = data_common:get(max_teamPkTimes),
    ?sendself(#sc_role_teamPkTimes_info{cost=Cost,times=Times,limit=Limit}).

check_role_suggest(Title,Content) ->
	#role{accid=Accid,roleID=RoleID,roleName=RoleName} = role_data:get_roleInfo(),
	case data_common:get(is_open_suggest) of
		true ->
			next;
		false ->
			erlang:throw({error,1})
	end,
	case db_sql:get_recent_suggest_datetime(RoleID) of
		?undefined ->
			next;
		Timestamp ->
			Now = util:now(),
			case Now - Timestamp > data_common:get(suggest_interval_seconds) of
				true ->
					next;
				false ->
					erlang:throw({error,2})
			end
	end,
	case Title of
		[] ->
			erlang:throw({error,3});
		_ ->
			case erlang:length(Title) > data_common:get(suggest_title_length) of
				false ->
					next;
				true ->
					erlang:throw({error,4})
			end
	end,
	case Content of
		[] ->
			erlang:throw({error,5});
		_ ->
			case erlang:length(Content) > data_common:get(suggest_content_length) of
				false ->
					next;
				true ->
					erlang:throw({error,6})
			end
	end,
	{ok,Accid,RoleID,RoleName}.

do_buy_energy(Type,Times,BuyAmount) ->
	case check_buy_energy(Type, Times) of
		
		{true,_RoleTimes,RoleInfo,NeedGold,_Today,NewBuyTimes,RoleTimes2,NewValue,Add}when Type =/=?BUY_TYPE_COIN ->
			role_data:set_roleTimes(RoleTimes2),%% RoleTimes2是一个元组
			role_lib:deduct_gold_f(RoleInfo,NeedGold,?MONEY_DEC_TYPE_BUY_ENERGY,Type,""),
			#role{roleID=RoleID,vipLevel=VipLevel} = role_data:get_roleInfo(),
			behavior_log_times:log(RoleID,VipLevel,NewBuyTimes,NewValue,Add,Type),
			?sendself(#sc_role_buy_energy{result=1,newBuyTimes=NewBuyTimes,newEnergy=NewValue,type=Type,crit=1});
		
		%% 招财现在改为批量式了
		{true,_RoleTimes,RoleInfo,NeedGold,_Today,CanBuyTimes,NewBuyTimes,RoleTimes2,_NewValue,_Add}->
			role_data:set_roleTimes(RoleTimes2),
			%% 增加金币
			CritProb = data_common:get(buy_coin_crit_prob),
			{Result, IncCoin} = 
				lists:foldl(fun(_E, {ListAcc, AmoutAcc}) ->
									Crit = random_reward_crit(CritProb),
									BuyCoin = get_coin_role_can_buy(RoleInfo#role.level,Crit),
									{[#p_multi_buy_energy{value=BuyCoin, crit=Crit}|ListAcc], AmoutAcc + BuyCoin}
							end, {[], 0}, lists:seq(1, CanBuyTimes)),
			RoleInfo2 = role_lib:deduct_gold_2_f(RoleInfo,NeedGold,?MONEY_DEC_TYPE_BUY_COIN,0,""
											   ,role_reward:transform2normal_reward(#sell_reward{coin=IncCoin})),
			#role{coin=NewCoin} = role_lib:add_coin_f(RoleInfo2,IncCoin,?MONEY_ADD_TYPE_BUY_COIN,CanBuyTimes,""),
			?CATCH(role_task_trigger:handle({dispach_task,role_buy_coin_times,CanBuyTimes})),
			#role{roleID=RoleID,vipLevel=VipLevel} = role_data:get_roleInfo(),
			behavior_log_times:log(RoleID,VipLevel,CanBuyTimes,NewCoin,IncCoin,Type),
			?sendself(#sc_role_multi_buy_energy{result=1,type=Type,newBuyTimes=NewBuyTimes,newEnergy=0,list=Result});
		
		{false,Reason} ->
			case BuyAmount of
				?BUY_SINGLE ->
					?sendself(#sc_role_buy_energy{result=Reason,newEnergy=0,newBuyTimes=0,type=Type,crit=1});
				_ ->
					?sendself(#sc_role_multi_buy_energy{result=Reason,newEnergy=0,newBuyTimes=0,type=Type,list=[]}) 
			end
	end.

check_do_transmigration_new(OldRoleInfo) ->
    NeedLevel = data_common:get(transmigration_level),
    {IsFinish,NowChapter} = check_xbattle_transmigration(),
    if OldRoleInfo#role.transmigration /= 0 ->{false,2,NowChapter};
       OldRoleInfo#role.level < NeedLevel -> {false,2,NowChapter};
       IsFinish /= false ->
           PosList = [E#ger.gerBase#gerBase.gerQuality||E<-role_data:get_posList(),E#ger.gerBase#gerBase.gerQuality >= 30],  %% 获取出战列表
           LPosList = [E#ger.gerBase#gerBase.gerQuality||E<-role_data:get_lieuposList(),E#ger.gerBase#gerBase.gerQuality >= 30],  %% 副将武将列表,对应皮卡丘中的小伙伴
           GerBag = [E#gerSimple.gerQuality||E<-role_data:get_gerBag(),E#gerSimple.gerQuality >= 30],  %% 保存武将背包
           L = erlang:length(PosList ++ LPosList ++ GerBag),
           ?INFO("check_do_transmigration PosList:~w LPosList:~w GerBag:~w",[PosList,LPosList,GerBag]),
           NeedGer = data_common:get(transmigration_ger),
           if L >= NeedGer -> {true, NowChapter};
              true -> {false,2,NowChapter}
           end;
       true -> {false,2, NowChapter}
    end.

check_xbattle_transmigration() ->
    #role_xbattle{chapterID=ChapterID} = role_data:get_xbattle_data(),
    if ChapterID < 10266 -> 
           {false,erlang:min(ChapterID rem 1000 - 1 ,266)};
       true -> 
            #xbattle_chapter{passDungeons=Dungeons} = role_xbattle:get_chapter(10266),
            #data_xbattle_chapter{dungeonCount=DungeonCount} = data_xbattle_chapter:get(10266),
            case length(Dungeons) of DungeonCount -> {ok, 266};
                                     _ -> {false,265}
            end
    end.

%% check_do_transmigration_normal(RoleID)->
%%     CurNormalProgress = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_NORMAL),
%%     if
%%         CurNormalProgress < 31000 ->
%%             {fail,CurNormalProgress-1};
%%         true ->
%%             Chapter = role_battle:get_chapter(RoleID, 1100),
%%             case Chapter of
%%                 #chapter{dungeonList=DungeonInfo}->
%%                     case lists:keysearch(31000, #p_dungeon.dungeonID, DungeonInfo) of
%%                         {value,Pdungeon} when Pdungeon#p_dungeon.bestScore =:= ?MAX_DUNGEON_SCORE->
%%                             {ok,CurNormalProgress};
%%                         _ ->
%%                             {fail,CurNormalProgress-1}
%%                     end;
%%                 _ ->
%%                     {fail,CurNormalProgress-1}
%%             end
%%     end.
%% check_do_transmigration_hard(RoleID)->
%%     CurNormalProgress = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_HARD),
%%     if
%%         CurNormalProgress < 10400 ->
%%             {fail,CurNormalProgress-1};
%%         true ->
%%             Chapter = role_battle:get_chapter(RoleID, 2040),
%%             case Chapter of
%%                 #chapter{dungeonList=DungeonInfo}->
%%                     case lists:keysearch(10400, #p_dungeon.dungeonID, DungeonInfo) of
%%                         {value,Pdungeon} when Pdungeon#p_dungeon.bestScore =:= ?MAX_DUNGEON_SCORE->
%%                             {ok,CurNormalProgress};
%%                         _ ->
%%                             {fail,CurNormalProgress-1}
%%                     end;
%%                 _ ->
%%                     {fail,CurNormalProgress-1}
%%             end
%%     end.

