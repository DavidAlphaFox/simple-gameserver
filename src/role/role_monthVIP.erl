%% @author lixinglong
%% @doc 月卡功能
%% Created 2015-5-21


-module(role_monthVIP).
-compile(export_all).
-include("def_role.hrl").
%% API functions

%% Internal functions


%% ====================================================================
%% API functions
%% ====================================================================
cs_monthVIP_info(_) ->
	#monthVIP_info{buyBigSec=BuyBigSec,buyLittleSec=BuyLittleSec,restBigDays=RestBigDays
				  ,restLittleDays=RestLittleDays,lastGetBigSec=LastGetBigSec,lastGetLittleSec=LastGetLittleSec
				  ,todayPayBig=TodayPayBig,todayPayLittle=TodayPayLittle}
		= role_data:get_role_monthVIP_info(),
	{StartTime,EndTime,NeedLevel,BuyBigGold,BuyLittleGold,BigTime,LittleTime
	,BigEverydayGold,LittleEverydayGold,BigReward,LittleReward} = get_config_info(),
	?sendself(#sc_monthVIP_info{bigNeedGold=BuyBigGold,littleNeedGold=BuyLittleGold,restBigDays=RestBigDays
							   ,restLittleDays=RestLittleDays,lastBuyBigTime=BuyBigSec,lastBuyLittleTime=BuyLittleSec
							   ,lastGetBigTime=LastGetBigSec,lastGetLittleTime=LastGetLittleSec,openLevel=NeedLevel
							   ,startTime=StartTime,endTime=EndTime,onceBigDays=BigTime,onceLittleDays=LittleTime
							   ,bigRewardInfo=activity_server:sell_reward2p_reward_info(BigReward)
							   ,littleRewardInfo=activity_server:sell_reward2p_reward_info(LittleReward)
							   ,everydayBigGold=BigEverydayGold,everydayLittleGold=LittleEverydayGold
							   ,todayPayBig=TodayPayBig,todayPayLittle=TodayPayLittle}).


cs_monthVIP_get_reward(#cs_monthVIP_get_reward{type=Type}) ->
	case check_role_can_get_monthVIP_reward(Type) of
		{true, MonthVipInfo,Reward} ->
			do_get_reward(MonthVipInfo, Reward,Type);
		{false,Reason}->
			?sendself(#sc_monthVIP_get_reward{result=Reason,gold=0})
	end.

cs_monthVIP_buy(#cs_monthVIP_buy{type=Type}) ->
	case check_can_buy_monthVIP(Type) of
		{true,MonthVIPInfo,RestDays,Reward,Role,NeedGold}->
			do_buy_monthVIP(Type,MonthVIPInfo,RestDays,Reward,Role,NeedGold);
		{false,Reason} ->
			?sendself(#sc_monthVIP_buy{result=Reason,days=0,reward=[]})
	end.


cs_monthVIP_get_growth_fund_info(#cs_monthVIP_get_growth_fund_info{})->
    #growth_fund_record{is_buy=IsBuy,reward_get_list=RewardGetList}
        = role_data:get_growth_fund_record(),
    #role{level=RoleLevel} = role_data:get_roleInfo(),
    StateList = get_growth_fund_state_list(data_monthVIP:get(growth_fund),RoleLevel,IsBuy,RewardGetList,[]),
    ?sendself(#sc_monthVIP_get_growth_fund_info{is_buy=IsBuy,state_list=StateList}).    

cs_monthVIP_buy_growth_fund(#cs_monthVIP_buy_growth_fund{})->
    #growth_fund_record{is_buy=IsBuy} = role_data:get_growth_fund_record(),
    #role{level=RoleLevel,vipLevel=RoleVip} = RoleInfo = role_data:get_roleInfo(),
    {LMin,LMax}=data_monthVIP:get(growth_fund_level),
    VipL=data_monthVIP:get(growth_fund_vip),
    if
        IsBuy /= 0 ->
            ?sendself(#sc_monthVIP_buy_growth_fund{result=4});
        RoleLevel < LMin orelse RoleLevel > LMax orelse VipL > RoleVip->
            ?sendself(#sc_monthVIP_buy_growth_fund{result=3});
        true ->
            NeedGold = data_monthVIP:get(growth_fund_cost),
            case role_lib:check_money(RoleInfo,gold,NeedGold) of
                true -> 
                    role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_GROWTH_FUND, 0, ""),
                    role_data:set_growth_fund_record(#growth_fund_record{is_buy=1}),
                    ?sendself(#sc_monthVIP_buy_growth_fund{result=1});
                false ->
                    ?sendself(#sc_monthVIP_buy_growth_fund{result=2})
            end
    end.

cs_monthVIP_get_growth_reward(#cs_monthVIP_get_growth_reward{level=GetLevel})->
    #growth_fund_record{is_buy=IsBuy,reward_get_list=RewardGetList}
        = GrowthInfo = role_data:get_growth_fund_record(),
    #role{level=RoleLevel} = RoleInfo = role_data:get_roleInfo(),
    case lists:member(GetLevel, RewardGetList) of
        false when GetLevel =< RoleLevel andalso IsBuy =:= 1 -> %这个等级没领取过，而且小于等于玩家当前等级
            case lists:keysearch(GetLevel, 1, data_monthVIP:get(growth_fund)) of
                {value , {GetLevel,Reward}} ->
                    role_reward:handle_sys_reward(RoleInfo, Reward, ?MONEY_ADD_TYPE_GROWTH_FUND, GetLevel, ""),
                    role_data:set_growth_fund_record(GrowthInfo#growth_fund_record{reward_get_list=[GetLevel|RewardGetList]}),
                    ?sendself(#sc_monthVIP_get_growth_reward{result=1});
                false ->
                    ?sendself(#sc_monthVIP_get_growth_reward{result=2})
            end;    
        _ ->
            ?sendself(#sc_monthVIP_get_growth_reward{result=2})
    end.




%% ====================================================================
%% Internal functions
%% ====================================================================

do_buy_monthVIP(Type,MonthVIPInfo,RestDays,Reward,Role,NeedGold) ->
	Role2 = role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_MONTHVIP_BUY, Type, integer_to_list(RestDays)),
	role_reward:handle_sell_reward_f(Role2, Reward, ?MONEY_ADD_TYPE_MONTHVIP_BUY, Type, ""),
	role_data:set_role_monthVIP_info(MonthVIPInfo),
	?sendself(#sc_monthVIP_buy{result=1,days=RestDays,reward=[activity_server:sell_reward2p_reward_info(Reward)]}).
	

check_can_buy_monthVIP(Type) ->
	#monthVIP_info{restBigDays=RestBigDays,restLittleDays=RestLittleDays
				   ,todayPayBig=TodayPayBig,todayPayLittle=TodayPayLittle,buyLittleSec=LastBuyLittle}
					  =MonthVIP=role_data:get_role_monthVIP_info(),
	Now = util:now(),
	Role = role_data:get_roleInfo(),
	case Type of
		1 ->
			if RestBigDays > 0 ->
				   {false,3};
			   true ->
				   if TodayPayBig == 1 ->
						  BigDays = data_monthVIP:get(bigTime),
						  Reward = data_monthVIP:get(bigReward),
						  NeedGold = data_monthVIP:get(buyBigGold),
						  case role_lib:check_money(Role,gold,NeedGold) of
							  true ->
						  {true, MonthVIP#monthVIP_info{restBigDays=BigDays,buyBigSec=Now},BigDays,Reward,Role,NeedGold};
							  _ ->
								  {false,5}
						  end;
					  true ->
						  {false,4}
				   end
			end;
		2 ->
			{LastBuyLittleDate,_} = util:seconds_to_datetime(LastBuyLittle),
			case erlang:date() of
				LastBuyLittleDate ->
					{false,2};
				_ ->
					if TodayPayLittle == 1 ->
						   LittleDays = data_monthVIP:get(littleTime),
						   Reward = data_monthVIP:get(littleReward),
						   NeedGold = data_monthVIP:get(buyLittleGold),
						   RestDays = LittleDays+RestLittleDays,
						   case role_lib:check_money(Role,gold,NeedGold) of
							   true ->
								   {true, MonthVIP#monthVIP_info{restLittleDays=RestDays,buyLittleSec=Now},RestDays,Reward,Role,NeedGold};
							   _ ->
								   {false,5}
						   end;
					   true ->
						   {false,4}
					end
			end
	end.



do_get_reward(MonthVipInfo, Reward,Type)->
	role_data:set_role_monthVIP_info(MonthVipInfo),
	role_lib:add_gold_f(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_MONTHVIP_EVERYDAY, Type, ""),
	?sendself(#sc_monthVIP_get_reward{result=1,gold=Reward}).

check_role_can_get_monthVIP_reward(1) ->
	#monthVIP_info{buyBigSec=BuyBigSec,lastGetBigSec=LastGetBigSec,restBigDays=RestBigDays} =
					  MonthVip = role_data:get_role_monthVIP_info(),
	case BuyBigSec of
		0 ->
			{false,4};
		_ ->
			case RestBigDays > 0 of
				false ->
					{false,2};
				_ ->
					{LastGetDate,_}=util:seconds_to_datetime(LastGetBigSec),
					case erlang:date() of
						LastGetDate ->
							{false,5};
						_ ->
							MonthVip2 = MonthVip#monthVIP_info{lastGetBigSec=util:now()},
					{true, MonthVip2,data_monthVIP:get(bigEverydayGold)}
					end
			end
	end;
check_role_can_get_monthVIP_reward(2) ->
	#monthVIP_info{buyLittleSec=BuyLittleSec,lastGetLittleSec=LastGetLittleSec,restLittleDays=RestLittleDays}=
					  MonthVip = role_data:get_role_monthVIP_info(),
	case BuyLittleSec of
		0 ->
			{false,4};
		_ ->
			case RestLittleDays > 0 of
				false ->
					{false,2};
				_ ->
					{LastGetDate,_} = util:seconds_to_datetime(LastGetLittleSec),
					case erlang:date() of
						LastGetDate ->
							{false,5};
						_ ->
							MonthVip2 = MonthVip#monthVIP_info{lastGetLittleSec=util:now()},
							{true, MonthVip2, data_monthVIP:get(littleEverydayGold)}
					end
			end
	end;
check_role_can_get_monthVIP_reward(_) ->
	{false,3}.

get_config_info()->
	{StartTime,EndTime} = data_monthVIP:get(activity_time),
	NeedLevel = data_monthVIP:get(needLevel),
	BuyBigGold = data_monthVIP:get(buyBigGold),
	BuyLittleGold = data_monthVIP:get(buyLittleGold),
	BigTime = data_monthVIP:get(bigTime),
	LittleTime = data_monthVIP:get(littleTime),
	BigEverydayGold = data_monthVIP:get(bigEverydayGold),
	LittleEverydayGold = data_monthVIP:get(littleEverydayGold),
	BigReward = data_monthVIP:get(bigReward),
	LittleReward = data_monthVIP:get(littleReward),
	{util:datetime_to_seconds(StartTime),util:datetime_to_seconds(EndTime)
	,NeedLevel,BuyBigGold,BuyLittleGold,BigTime,LittleTime
	,BigEverydayGold,LittleEverydayGold,BigReward,LittleReward}.

update_pay_info(PayGold)->
	MonthVIPInfo = role_data:get_role_monthVIP_info(),
	Type = data_monthVIP:get({monthVIPPayGold,PayGold}),
	MonthVIPInfo2 = 
		case Type of 
			1 ->
				MonthVIPInfo#monthVIP_info{todayPayBig=1};
			2 ->
				MonthVIPInfo#monthVIP_info{todayPayLittle=1};
			_ ->
				MonthVIPInfo
		end,
	role_data:set_role_monthVIP_info(MonthVIPInfo2).

get_growth_fund_state_list([],_RoleLevel,_IsBuy,_LastRewardLevel,AccList)->
    lists:sort(AccList);
get_growth_fund_state_list([{L,R}|OtherList],RoleLevel,0,LastRewardLevel,AccList)->
    get_growth_fund_state_list(OtherList,RoleLevel,0,LastRewardLevel
                              ,[{p_growth_fund_state,L,1
                                ,activity_server:sell_reward2p_reward_info(R)}|AccList]);
get_growth_fund_state_list([{L,R}|OtherList],RoleLevel,IsBuy,LastRewardLevel,AccList)->
    case lists:member(L, LastRewardLevel) of
        true ->
            get_growth_fund_state_list(OtherList,RoleLevel,IsBuy,LastRewardLevel
                                      ,[{p_growth_fund_state,L,3
                                        ,activity_server:sell_reward2p_reward_info(R)}|AccList]);
        false when L > RoleLevel->
            get_growth_fund_state_list(OtherList,RoleLevel,IsBuy,LastRewardLevel
                                      ,[{p_growth_fund_state,L,1
                                        ,activity_server:sell_reward2p_reward_info(R)}|AccList]);
        false when L =< RoleLevel->
            get_growth_fund_state_list(OtherList,RoleLevel,IsBuy,LastRewardLevel
                                      ,[{p_growth_fund_state,L,2
                                        ,activity_server:sell_reward2p_reward_info(R)}|AccList])
    end.



