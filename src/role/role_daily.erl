%% @author caohongyang
%% @doc 每日工资
%% Created 2013-4-15


-module(role_daily).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% API functions
-export([do_get_list/0]).

%% Internal functions
-export([]).


%% ====================================================================
%% API functions
%% ====================================================================
cs_daily_get_list(_) ->
%% 	Reply = {sc_daily_get_list,[
%% 								{p_daily,1,3,[{p_daily_reward,2,10},{p_daily_reward,1,100}]},
%% 								{p_daily,2,7,[{p_daily_reward,3,11006},{p_daily_reward,5,11114}]}
%% 							   ]},
%% 	?sendself(Reply).
	do_get_list().

cs_daily_draw(#cs_daily_draw{type=Type}) ->
	do_draw(Type).

%% ====================================================================
%% Internal functions
%% ====================================================================

hook_zero_clock() ->
	#daily{loginDays=CurLoginDays} = Daily = role_data:get_dailyInfo(),
	NowDate = erlang:date(),
    LastLoggedDate = Daily#daily.lastLoggedLoginDate,
	Daily2 = Daily#daily{loginDays=CurLoginDays+1,lastLoggedLoginDate=NowDate},
	role_data:set_dailyInfo(Daily2),
	DailyList = get_list(Daily2),
	if DailyList =:= [] ->
		   ignore;
	   true ->
		   ?sendself(#sc_daily_get_list{dailyList = DailyList})
	end,
    DayPassNum = calendar:date_to_gregorian_days(NowDate) - calendar:date_to_gregorian_days(LastLoggedDate),
	role_shop:daily_refresh_item_shop(DayPassNum),
	%% 推送神将录次数更新
	update_challengeGodInfo(),
	%% 推送汉帝宝库更新
	update_treaHouseInfo(),
	%% 推送副将刷新信息
	update_refreshLieuInfo(),
	%% 遭遇战自动降阶
	update_encounter_monRank_info(),
	update_monthVIP_info(),
	update_itemUseInfo(),
	update_lucky_roll(),
	push_carlos_times(),
	update_bounty_info(),
	update_home_boss_times(),
    role_xbattle:refresh_xbattle(),
	role_exBoss:refresh_exBoss(),
        role_box:refresh_box_free(),
    refresh_dSign_data(),
	% activity_server主动推送每日充值活动的更新
	activity_server:refresh_daily_activity(role_data:get_roleID()).

update_home_boss_times()->
	Times = role_homeBoss:init_times(),
	role_data:set_homeBoss_times(Times). 

refresh_dSign_data() ->
    DSignData = role_data:get_dSignData(),
    DSignData2 = role_dSign:init_dSignData(DSignData),
    role_data:set_dSignData(DSignData2),
    role_dSign:cs_dSign_info(1).

push_carlos_times()->
	erlang:send_after(3000, self(), {route, role_carlos, {cs_carlos_info}}),
	erlang:send_after(3000, self(), {route, role_twins, {cs_twins_info}}).

update_lucky_roll()->
	LuckyRoll = role_data:get_lucky_roll(),
	role_data:set_lucky_roll(LuckyRoll#lucky_roll{free_times=0}),
	role_luckyRoll:cs_luckyRoll_get_list(0).

calc_biggest_num(Num1,Num2) ->
	Num3 = Num1-Num2,
	if Num3 > 0 ->
		   Num3;
	   true ->
		   0
	end.

update_bounty_info()->
	role_bounty:cs_bounty_self_info(1).

update_monthVIP_info()->
	#monthVIP_info{restBigDays=RestBigDays,restLittleDays=RestLittleDays} = 
		MonthVipInfo = role_data:get_role_monthVIP_info(),
	MonthVipInfo2 = MonthVipInfo#monthVIP_info{restBigDays=calc_biggest_num(RestBigDays,1)
											  ,restLittleDays=calc_biggest_num(RestLittleDays,1)
											  ,todayPayBig=0,todayPayLittle=0},
	role_data:set_role_monthVIP_info(MonthVipInfo2),
	role_monthVIP:cs_monthVIP_info(#cs_monthVIP_info{}).

update_itemUseInfo()->
	ItemUseList = role_data:get_itemUseList(),
	Date = erlang:date(),
    NewItemUseList2 =
        lists:foldr(fun(ItemTypeID, Acc) ->
                            case lists:keyfind(ItemTypeID, #item_use_info.itemTypeID, ItemUseList) of
                                false ->
                                    [#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=0}|Acc];
                                #item_use_info{} ->
									[#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=0}|Acc]
                            end
                    end, [], data_item_use:get_list()),
	role_data:set_itemUseList(NewItemUseList2),
	PItemUseInfoList =
        lists:map(fun(#item_use_info{itemTypeID=ItemTypeID,useTimes=UseTimes}) ->
                          #data_item_use{maxTimes=MaxTimes} = data_item_use:get(ItemTypeID),
                          #p_item_use_info{type_id=ItemTypeID, left_times=erlang:max(MaxTimes - UseTimes, 0)}
                  end, NewItemUseList2),
    ?sendself(#sc_item_use_info{use_info_list=PItemUseInfoList}).

update_refreshLieuInfo()->
	RoleTimes = role_data:get_roleTimes(),
	FreeTimes = data_lieu_clo_setting:get(daily_free_refresh),
	role_data:set_roleTimes(RoleTimes#roleTimes{refreshLieuTimes=FreeTimes}),
	?sendself(#sc_ger_lieu_refresh_freeTimes{times=FreeTimes}).
		
update_challengeGodInfo()->
	RoleTimes = role_data:get_roleTimes(),
	%ChallengeGodEnergy = data_common:get(challengeGodTimes),
	Role=role_data:get_roleInfo(),
	ChallengeGodEnergy = role_lib:get_max_challengeGodFreeTimes(Role#role.vipLevel),
	role_data:set_roleTimes(RoleTimes#roleTimes{challengeGodEnergy=ChallengeGodEnergy,challengeGodBuyTimes=0, lastChallengeGodDate=erlang:date()}),
	ChallengePos = role_data:get_challengeGod_pos(),
	Price = data_common:get(buy_challengeGod_gold),
	?notify_update(#sc_challengeGod_info{freeTimes=ChallengeGodEnergy,buyTimes=0,gerPos=ChallengePos, price=Price}).

update_treaHouseInfo()->
	TreaHouseInfo = role_data:get_treaHouseInfo(),
	if TreaHouseInfo#treaHouseInfo.card_list =:= [] ->
		   ignore;
	   true ->
		   FreeTimes = data_treasure_box:get(treasure_house_free_times),
		   TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{free_times=FreeTimes},
		   BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
		   role_data:set_treaHouseInfo(TreaHouseInfo2),
		   {EndTime, StopTime} = role_treaHouse:get_end_time(),
		   {_,OneTimeNeedGold} = data_treasure_box:get(oneTimeCost),
		   RefreshNeedCoin = data_treasure_box:get(refresh_cost),
		   _IsOpen = 
			   case role_treaHouse:check_enter_treaHouse() of
				   true ->
					   ?notify_update(#sc_treaHouse_get_list{isOpen=1,endTime=EndTime,boxProcess = BoxProcess,stopTime=StopTime,
															 cardList=role_treaHouse:treaHouseCard2p_treaHouse_card(TreaHouseInfo2#treaHouseInfo.card_list),
															 freeTimes = FreeTimes,boxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
															 oneTimeNeedGold=OneTimeNeedGold, refreshNeedCoin=RefreshNeedCoin,
															 valueinfo=TreaHouseInfo2#treaHouseInfo.value_info});
				   _ ->
					   ignore
			   end
	end.

update_encounter_monRank_info()->
	{MonRank, Info}=role_data:get_roleEncounterInfo(),
	role_data:update_encounter_monRank_info2(MonRank, Info).

do_get_list() ->
	Daily = role_data:get_dailyInfo(),
	DailyList = get_list(Daily),
	?sendself(#sc_daily_get_list{dailyList = DailyList}).

get_continous_login_daily(Daily) ->
	#daily{lastDrawLoginRewardDays=LastDrawDays,
		   loginDays=LoginDays0} = Daily, 
	LoginDays = login_days2(LoginDays0),
	if LastDrawDays < LoginDays0 ->
		   case get_login_reward_config(LoginDays) of
			   #daily_reward{} ->
				   [#p_daily{type=2,value=LoginDays,isDrawed=false}];
			   _ ->
				   []
		   end;
	   LastDrawDays =:= LoginDays0 ->
		   [#p_daily{type=2,value=LoginDays,isDrawed=true}];
	   true ->
		   []
	end.

get_title_daily(Daily, Role) ->
	NowDate = erlang:date(),
	#daily{lastDrawTitle=LastDrawTitle,
		   lastTitleRewardDate=LastTitleDate} = Daily, 
	#role{title=Title} = Role,
		if LastTitleDate < NowDate ->		   
			   case data_title_reward:get(Title) of
				   #daily_reward{} ->
					   [#p_daily{type=1,value=Title,isDrawed=false}];
				   _ ->
					   []
			   end;
		   LastTitleDate =:= NowDate ->
			   [#p_daily{type=1,value=LastDrawTitle,isDrawed=true}];
		   true ->
			   []
		end.

get_levelup_daily(Daily, #role{level=RoleLevel}) ->
	#daily{lastDrawLevelUpLevel=LastLevel} = Daily,
	RewardLevelList = data_levelup_reward:get_list(),
	case list_next(LastLevel, RewardLevelList) of
		false ->
			if LastLevel == 0 ->
				   [];
			   true ->
				   [#p_daily{type=3,value=LastLevel,isDrawed=true}]
			end;
		{value, Nextlevel} ->
			if Nextlevel =< RoleLevel ->
				   [#p_daily{type=3,value=Nextlevel,isDrawed=false}];
			   true ->
				   if LastLevel == 0 ->
						  [];
					  true ->
						  [#p_daily{type=3,value=LastLevel, isDrawed=true}]
				   end
			end
	end.

get_list(Daily) ->
	DailyLogin = get_continous_login_daily(Daily),
		
	Role = role_data:get_roleInfo(),	

	DailyTitle = get_title_daily(Daily, Role),
	
	LevelUpDaily = get_levelup_daily(Daily, Role),
	DailyTitle ++ DailyLogin ++ LevelUpDaily.
	

login_days2(LoginDays) ->
%% 	((LoginDays-1) rem (data_login_reward:get(max))) +1.
	if LoginDays > 31 ->
		   (LoginDays - 31) rem (data_login_reward:get(max) - 30) + 31;
	   true ->
		   LoginDays
	end.

get_login_reward_config(LoginDays) 
  when is_integer(LoginDays) andalso LoginDays > 0->
	case data_login_reward:get(LoginDays) of
		#daily_reward{}=Reward ->
			Reward;
		_ ->
			data_login_reward:get(max)
	end;
get_login_reward_config(_) ->
	?undefined.

do_draw(1) ->
	#daily{lastTitleRewardDate=LastTitleDate} = DailyInfo = role_data:get_dailyInfo(),
	NowDate = erlang:date(),
			   #role{title=Title} = Role2 = role_data:get_roleInfo(),	
	TitleRewardFlag = 
		if LastTitleDate < NowDate ->		   
			   case data_title_reward:get(Title) of
				   #daily_reward{}=TitleReward ->
					   role_reward:handle_daily_reward_f(Role2, TitleReward, ?MONEY_ADD_TYPE_DAILY_TITLE_REWARD, Title, ""),
					   LastTitleDate2 = NowDate,
					   true;
				   _ ->
					   LastTitleDate2 = LastTitleDate, 
					   false
			   end;
		   true ->
			   LastTitleDate2 = LastTitleDate,
			   false
		end,
	DailyInfo2 = DailyInfo#daily{lastTitleRewardDate=LastTitleDate2,lastDrawTitle=Title},
	role_data:set_dailyInfo(DailyInfo2),
	if TitleRewardFlag->
		   ?sendself(#sc_daily_draw{result=1,newDaily = #p_daily{type=1,value=Title,isDrawed=true}});
	   true ->
		   ?sendself(#sc_daily_draw{result=2,newDaily = #p_daily{type=1,value=0,isDrawed=false}})		   
	end;
do_draw(2) ->
	#daily{lastDrawLoginRewardDays=LastDrawDays,
		   loginDays=LoginDays0} = DailyInfo = role_data:get_dailyInfo(),
	LoginDays = login_days2(LoginDays0),
	LoginRewardFlag = 
		if LastDrawDays < LoginDays0 ->
			   case get_login_reward_config(LoginDays) of
				   #daily_reward{}=Reward   ->
					   Role = role_data:get_roleInfo(),
					   role_reward:handle_daily_reward_f(Role, Reward, ?MONEY_ADD_TYPE_CON_LOGIN_REWARD, LoginDays, ""),
					   LastLoginDrawDays2 = LoginDays0, 
					   true;
				   _ ->
					   LastLoginDrawDays2 = LastDrawDays,
					   false
			   end;
		   true ->
			   LastLoginDrawDays2 = LastDrawDays,
			   false
		end,
	DailyInfo2 = DailyInfo#daily{lastDrawLoginRewardDays=LastLoginDrawDays2},
	role_data:set_dailyInfo(DailyInfo2),
	if LoginRewardFlag->
		   ?sendself(#sc_daily_draw{result=1,newDaily = #p_daily{type=2,value=LoginDays,isDrawed=true}});
	   true ->
		   ?sendself(#sc_daily_draw{result=2,newDaily = #p_daily{type=2,value=0,isDrawed=false}})		   
	end;
do_draw(3) ->
	#daily{lastDrawLevelUpLevel=LastLevel} = DailyInfo = role_data:get_dailyInfo(),
	RewardLevelList = data_levelup_reward:get_list(),
	case list_next(LastLevel, RewardLevelList) of
		false ->
			?sendself(#sc_daily_draw{result=2, newDaily=#p_daily{type=3,value=LastLevel,isDrawed=true}});
		{value, NextLevel} ->
			#role{level=Level} = RoleInfo = role_data:get_roleInfo(),
			if NextLevel =< Level ->
				   DailyInfo2 = DailyInfo#daily{lastDrawLevelUpLevel=NextLevel},
				   role_data:set_dailyInfo(DailyInfo2),
				   Reward = data_levelup_reward:get(NextLevel),
				   role_reward:handle_sell_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_LEVEL_UP_GIFT, NextLevel, ""),
				   ?sendself(#sc_daily_draw{result=1, newDaily=#p_daily{type=3,value=NextLevel,isDrawed=true}});
			   true ->
				   ?sendself(#sc_daily_draw{result=2, newDaily=#p_daily{type=3,value=LastLevel,isDrawed=true}})
			end
	end.

list_next(E, [R|_]) when R > E->
	{value,R};
list_next(E, [_|L]) ->
	list_next(E,L);
list_next(_E,[]) ->
	false.

transform_list(List) ->
	{MaxKey,_} = util:keymax(List, 1),
	[{max,MaxKey}|List].
	
	

can_draw_loginreward()->
	#daily{lastDrawLoginRewardDays=LastDrawDays,
		   loginDays=LoginDays0} = role_data:get_dailyInfo(), 
	if LastDrawDays < LoginDays0 ->
				LoginDays = login_days2(LoginDays0),
				{true,LoginDays};
			true->
				false
	end.

