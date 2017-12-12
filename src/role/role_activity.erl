%% @author zxystar
%% @doc 活动 玩家处理模块
%% Created 2013-12-7


-module(role_activity).
-compile(export_all).
-include("def_role.hrl").

%% API functions
-export([]).

%% Internal functions 
-export([]).
-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
-define(get_activity_last_time,get_activity_last_time).
%% ====================================================================
%% API functions
%% ====================================================================

cs_activity_get_list(#cs_activity_get_list{}) ->
	#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
	erlang:send(activity_server, {get_list,RoleID,RoleLevel,VipLevel}).

cs_activity_info(#cs_activity_info{activityID=ID}) ->
	RoleID = role_data:get_roleID(),
	erlang:send(activity_server, {get_info,RoleID,ID}).

cs_activity_draw(#cs_activity_draw{activityID=ActivityID,drawID=DrawID,choseReward=Chose}) ->
	case check_draw_qos() of
		true ->
			#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),  %% 传递vip和等级用于检测是否有资格
			erlang:send(activity_server, {activity_draw,RoleID,ActivityID,DrawID,VipLevel,RoleLevel,Chose});
		false ->
			?sendself(#sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2})
	end.

cs_activity_first_pay(_) ->
	#role{firstPayStatus=ID} = role_data:get_roleInfo(),
	get_now_firstPayStatus(ID).

get_now_firstPayStatus(ID)->
	RewardList = [begin #data_first_pay{id=PID,reward=Reward,disc=Disc}=data_first_pay:get(MID),
				  #p_activity_pay_reward{id=PID,reward=activity_server:sell_reward2p_reward_info(Reward),disc=Disc}
				  end
				  ||MID<-data_first_pay:get_list()],
	?sendself(#sc_activity_first_pay{displayID=ID,rewardList=RewardList}).

%% isEmperor: 帝王换届后,上一届帝王和本届帝王都可以领取帝王的连续签到奖励,这里用isEmperor记录,当第一次签到的时候,记录下玩家是否是帝王
%% 当玩家领取奖励的时候,再次获取玩家是否是帝王,然后给玩家发奖.玩家领取连续签到的宝箱后,清理isEmperor的状态。
cs_activity_sign_emperor_info(_)->
	{LastSignDate, SignedDays,_, IsGetBox} = role_data:get_sign_emperor_info(),
	IsSigned = 
		case LastSignDate =:= erlang:date() of
			true ->
			   1;
		   _ ->
			   2
		end,
	{EmperorID, EmperorName} = race2_server:get_champion(),
	RoleID = role_data:get_roleID(),
	IsEmperor =
		if RoleID =:= EmperorID ->
			   1;
		   true ->
			   2
		end,
	%?ERR("id:~w,~w,~w",[RoleID, EmperorID,LastSignDate]),
	LastSignDate2 = calendar:date_to_gregorian_days(LastSignDate),
	Today = calendar:date_to_gregorian_days(erlang:date()),
	SignedDays2 = 
		if Today - LastSignDate2 > 1 ->
			0;
		   true ->
			   if SignedDays =:= 5 andalso Today - LastSignDate2 =:= 1 ->
					  0;
				  true ->
			   SignedDays
			   end
		end,
	?sendself(#sc_activity_sign_emperor_info{isSign=IsSigned, signDays=SignedDays2,isGetBox=IsGetBox, isEmperor=IsEmperor, emperorName=EmperorName}).

cs_activity_sign_get_reward(_)->
	{_, SignedDays,_, IsGetBox} =Info= role_data:get_sign_emperor_info(),
	case check_sign_get_reward(SignedDays, IsGetBox) of
		{false, Reason} ->
			?sendself(#sc_activity_sign_get_reward{result=Reason, reward=[]});
		true ->
			do_sign_get_reward(Info)
	end.

cs_activity_sign_up(_)->
	{LastSignDate, SignedDays,_, _} =Info= role_data:get_sign_emperor_info(),
	case check_sign_emperor(LastSignDate, SignedDays) of
		{false, Reason}->
			?sendself(#sc_activity_sign_up{result=Reason, reward=[]});
		{true, Reason, Gold}->
			do_sign_emperor(Reason, Info, Gold)
	end.

%% 查询冲级活动的排名信息和时间
cs_activity_levelRank_open(_)->
	{ServerDate, _} = data_setting:get(serverOpenTime),
	TimePoint = data_levelRank:get(timePoint),
	ActivityPeriod = data_levelRank:get(activityPeriod),
	ShowResultPeriod = data_levelRank:get(showPeriod),
	ActivityTime = util:datetime_to_seconds({ServerDate,TimePoint}) + ActivityPeriod * ?ONE_DAY_SECONDS,
	ShowResultTime = util:datetime_to_seconds({ServerDate,TimePoint}) + (ActivityPeriod + ShowResultPeriod) * ?ONE_DAY_SECONDS,
	RankLength = data_levelRank:get(rankNum),
	
	case util:now() > ShowResultTime of 
		true ->  %% 活动关闭
			?sendself(#sc_activity_levelRank_open{stopTime=ActivityTime,endTime=ShowResultTime,rankerInfoList=[],rankLength=RankLength,myRank = 0});
		_ ->   
			levelRank_server:get_rank_info(ActivityTime, ShowResultTime, role_data:get_roleID(), RankLength)
	end.

%% 查询冲级活动的排名信息，不包括时间
cs_activity_levelRank_refresh(_) ->
	{ServerDate,_} = data_setting:get(serverOpenTime),
	TimePoint = data_levelRank:get(timePoint),
	ActivityPeriod = data_levelRank:get(activityPeriod),
	ShowResultPeriod = data_levelRank:get(showPeriod),
	ShowResultTime = util:datetime_to_seconds({ServerDate, TimePoint}) + (ActivityPeriod + ShowResultPeriod) * ?ONE_DAY_SECONDS,
								   
	case util:now() > ShowResultTime of
		true ->		%% 活动关闭
			?sendself(#sc_activity_levelRank_refresh{rankerInfoList=[]});
		_ ->
			levelRank_server:get_rank_info(role_data:get_roleID())
	end.

cs_activity_get_payExt_info(_) ->
	#role{payExtReward=PayExtRewardT,extRdActTime=LastActSec} = role_data:get_roleInfo(),
	NowActSec = 
		case data_pay_reward:get(activity_start) of
			?undefined ->
				0;
			DateTime ->
				Sec2 = util:datetime_to_seconds(DateTime),
				case Sec2 >= util:now() of
					true ->
						0;
					_ ->
						Sec2
				end
		end,
	PayExtReward = role_lib:update_payExtReward_value(PayExtRewardT, LastActSec,NowActSec),
	?sendself(#sc_role_update_pay_ext{pay_ext =PayExtReward}),
	#role{profoundCrystal=ProfoundCrystal} = role_data:get_roleInfo(),
	?notify_update(?ra_profoundCrystal(ProfoundCrystal)).

cs_activity_vip_shop(_)->
	#vip_shop{activityID=ActivityID,items=Items,timestamp=Timestamp} = role_data:get_vip_shop(),
	case ets:lookup(?ETS_VIP_ACTIVITY,state) of
		[] ->
			?sendself(#sc_activity_vip_shop{endTime=0,shop=[]});
		[{state,[#data_activity_vip{activityID=ActivityID2,endTime=EndTime,items=Items2}]}] ->
			case ActivityID2 of
				ActivityID ->
                    case ets:lookup(?ETS_VIP_ACTIVITY,restart_timestamp) of
                        [] ->
                            %% 没有重启
					        ?sendself(#sc_activity_vip_shop{endTime=EndTime,shop=to_p2(Items,Items2)});
                        [{_, RT}] ->
                            case Timestamp =< RT of
                                true ->
                                    %%  有重启,且没有刷新
					                role_data:set_vip_shop(#vip_shop{activityID=ActivityID,items=Items2,timestamp=util:now()}),
					                ?sendself(#sc_activity_vip_shop{endTime=EndTime,shop=to_p(Items2)});
                                _ ->
                                    %% 有重启,且已经刷新了
					                ?sendself(#sc_activity_vip_shop{endTime=EndTime,shop=to_p2(Items,Items2)}) 
                            end
                    end;
				_ ->
					role_data:set_vip_shop(#vip_shop{activityID=ActivityID2,items=Items2}),
					?sendself(#sc_activity_vip_shop{endTime=EndTime,shop=to_p(Items2)})
			end
	end.

cs_activity_vip_shop_buy(#cs_activity_vip_shop_buy{vip=Vip,sell=Shop}) ->
	#vip_shop{activityID=ActivityID,items=Items} = role_data:get_vip_shop(),
	#role{vipLevel=RVip,svipLevel=SRVip} = Role = role_data:get_roleInfo(),
	if RVip < Vip ->
		   ?sendself(#sc_activity_vip_shop_buy{type=5});
	   true ->
		   case ets:lookup(?ETS_VIP_ACTIVITY,state) of
			   [] ->
				   ?sendself(#sc_activity_vip_shop_buy{type=4});
			   [{state,[#data_activity_vip{activityID=ActivityID2,items=Items2}]}] ->
				   case ActivityID2 of
					   ActivityID ->
						   Items3 = Items;
					   _ ->
						   Items3 = Items2
				   end,
				   Item = [I||#data_vip_shop{vip=Vip0,id=ID0}=I<-Items3,Vip0 == Vip orelse Vip0 == (SRVip+100) , Shop == ID0],
				   case Item of
					   [] ->
						   ?sendself(#sc_activity_vip_shop_buy{type=4});
					   [#data_vip_shop{total=Total,used=Used,now=Now,type=Type,reward=Reward}=ItemI] ->
						   if Total =< Used ->
								  ?sendself(#sc_activity_vip_shop_buy{type=3});
							  true ->
								  CostType = get_costType(Type),
								  case role_lib:check_money(Role,CostType,Now) of
									  false ->
										  ?sendself(#sc_activity_vip_shop_buy{type=2});
									  true ->
										  OItems = [I||#data_vip_shop{vip=Vip0,id=ID0}=I<-Items3,Vip0 /= Vip orelse Shop /= ID0],
										  NItem =  ItemI#data_vip_shop{used=Used+1},
										  Items4 = [NItem|OItems],
										  Role2 = role_lib:deduct_money_f(Role,CostType,Now,?MONEY_DEC_TYPE_VIP_SHOP_BUY
																		  ,ActivityID,integer_to_list(Vip)++"_"++integer_to_list(Shop)),
										  role_reward:handle_sell_reward_f(Role2,Reward,?MONEY_ADD_TYPE_VIP_SHOP_BUY
																		   ,ActivityID,integer_to_list(Vip)++"_"++integer_to_list(Shop)),
										  role_data:set_vip_shop(#vip_shop{activityID=ActivityID2,items=Items4}),
										  ?sendself(#sc_activity_vip_shop_buy{type=1})
								  end
						   end
				   end
		   end
	end.

cs_activity_consume_reback(#cs_activity_consume_reback{})->
	case consume_back_server:is_consume_back_open() of
		false->
			?sendself(#sc_activity_consume_reback{result=0});
		true->
			#role{roleID=RoleID} = role_data:get_roleInfo(),
			Msg = consume_back_server:get_role_consume_back_info(RoleID),
			?sendself(Msg)
	end.

cs_activity_consume_state(#cs_activity_consume_state{})->
	case consume_back_server:is_consume_back_open() of
		false->
			?sendself(#sc_activity_consume_state{state=0});
		true->
			?sendself(#sc_activity_consume_state{state=1})
	end.



get_costType(1) ->
	gold;
get_costType(2) ->
	coin;
get_costType(3) ->
	reputation.
				
to_p2([],Items) ->
	?ERR("no items...refresh"),
	to_p(Items);
to_p2(Items,_) ->
	to_p(Items).
to_p(Items) ->
	[#p_activity_vip_shop{vip=Vip,sell=ID,costType=Type,total=Total,used=Used,gold=Gold,now=Now,boxName=BoxName
						 ,boxIcon=BoxID,reward=activity_server:sell_reward2p_reward_info(Reward)}
	||#data_vip_shop{vip=Vip,id=ID,type=Type,total=Total,used=Used,gold=Gold,now=Now,reward=Reward,boxIconID=BoxID,boxName=BoxName}<-Items].
			

cs_activity_energy_pac_info(_) ->
    #roleTimes{energyPac=E} =  role_data:get_roleTimes(),
    Max = data_common:get(max_energy_pac),
    ?sendself(#sc_activity_energy_pac_info{energyPac=E,max=Max}).
cs_activity_energy_pac_use(_) ->
    #roleTimes{energy=Energy,energyPac=E,lastEnergyTime=NowSec} =RoleTimes=  role_data:get_roleTimes(),
    if E =< 0 -> ?sendself(#sc_activity_energy_pac_use{result=2});
       true ->          
           E2 = get(?currentEnergyIntercal),
           ?notify_update(?ra_energy(Energy+E, NowSec+E2)),
            RoleTimes2 = RoleTimes#roleTimes{energy=Energy+E,energyPac=0},
           role_data:set_roleTimes(RoleTimes2),
           ?sendself(#sc_activity_energy_pac_use{result=1})
    end.
           

%% ====================================================================
%% Internal functions
%% ====================================================================

check_draw_qos() ->
	Now = util:now(),
	Res = 
		case get(?get_activity_last_time) of
			Now ->
				false;
			_ ->
				true
		end,
	put(?get_activity_last_time,Now),
	Res.

do_sign_get_reward({LastSignDate, SignedDays, IsEmperor, _})->
	{EmperorID, _} = race2_server:get_champion(),
	RoleID = role_data:get_roleID(),
	BoxID = 
		if RoleID =:= EmperorID orelse IsEmperor ->
			   data_common:get(signEmperorBoxID5);
		   true ->
			   data_common:get(signEmperorBoxID6)
		end,
	
%% 	Reward = {sell_reward,0,0,0,0,[{new_item,BoxID,1,1,0}],0,[]},
	Role = role_data:get_roleInfo(),
%% 	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_SIGN_EMPEROR, 1, ""),
	case data_box:get({BoxID,0}) of
		[RConfig|_] ->
			Reward = util:random_one_from_weigh_list(RConfig),
			role_reward:handle_sys_reward(Role, [Reward], ?MONEY_ADD_TYPE_SIGN_EMPEROR, 1, ""),
			role_data:set_sign_emperor_info({LastSignDate,SignedDays,0,1}),
			?sendself(#sc_activity_sign_get_reward{result=1, reward=role_reward:transform2p_reward_view([Reward], [])});
		_ ->
			?sendself(#sc_activity_sign_get_reward{result=4, reward=[]})
	end.
	

check_sign_get_reward(SignedDays, IsGetBox)->
	if SignedDays =:= 5 ->
		   if IsGetBox =:= 1 ->
				  {false, 2};
			  true ->
				  #role{level=Level} = role_data:get_roleInfo(),
				  case Level >= data_common:get(signEmperorNeedLevel) of
					  true ->
						  true;
					  _ ->
						  {false, 3}
				  end
		   end;
	   true ->
		   {false, 3}
	end.

do_sign_emperor(0, {D,_,_,_}, Gold)->
	{EmperorID, _} = race2_server:get_champion(),
	RoleID = role_data:get_roleID(),
	A = if RoleID =:= EmperorID ->
			   1;
		   true ->
			   0
		end, 
	do_sign_emperor(1,{D,0,A,0},Gold);
do_sign_emperor(1, {_,S,A,B},Gold)->
	Type = S rem 5,
	{EmperorID, _} = race2_server:get_champion(),
	RoleID = role_data:get_roleID(),
	JudgeGold = data_common:get(signEmperorJudgeGold),
	BoxIDList = 
		if EmperorID =:= RoleID ->
			   if Gold =< JudgeGold ->
					  data_common:get({signEmperorBoxID1,Type});
				  true ->
					  data_common:get({signEmperorBoxID2,Type})
			   end;
		   true ->
			   if Gold =< JudgeGold ->
					  data_common:get({signEmperorBoxID3,Type});
				  true ->
					  data_common:get({signEmperorBoxID4,Type})
			   end
		end,
%% 	Reward = {sell_reward,0,0,0,0,[{new_item,BoxID,1,1,0}],0,[]},
	Role = role_data:get_roleInfo(),
	Reward = get_reward(BoxIDList),
	?INFO("Reward :~w ~n",[Reward]),
%% 	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_SIGN_EMPEROR, 0, ""),
	% case data_box:get({BoxID,0}) of
	% 	[RConfig|_] ->
	% 		Reward = util:random_one_from_weigh_list(RConfig),
			role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_SIGN_EMPEROR, 0, ""),
			role_data:set_sign_emperor_info({erlang:date(),S+1,A,B}),
	 	?CATCH(role_task_trigger:handle({dispach_task,role_emperor,1})),
			?sendself(#sc_activity_sign_up{result=1, reward=role_reward:transform2p_reward_view(Reward, [])}).
	% 	X ->
	% 	?ERR("err when get sign emperor box :~w,~w",[X, BoxID]),
	% 		?sendself(#sc_activity_sign_up{result=4, reward=[]})
	% end.

check_sign_emperor(LastSignDate, SignedDays)->
	LastSignDate2 = calendar:date_to_gregorian_days(LastSignDate),
	Today = calendar:date_to_gregorian_days(erlang:date()),
	if Today =:= LastSignDate2 ->
		   {false, 2};
	   true ->
		   #role{level=Level, gold=Gold,goldBonus=GoldBonus} = role_data:get_roleInfo(),
		   case Level >= data_common:get(signEmperorNeedLevel) of
			   true ->
				   if Today - LastSignDate2 =:= 1 ->
						  if SignedDays =:= 5 ->
								 {true ,0, Gold+GoldBonus};%%5, restart
							 true ->
								 {true, 1, Gold+GoldBonus}
						  end;
					  true ->
						  {true, 0, Gold+GoldBonus}
				   end;
			   _ ->
				   {false, 3}
		   end
	end.

%% 检查角色VIP等级
check_rebate_vip_level({vip, Lower, Upper}, VipLevel)->
	VipLevel>=Lower andalso VipLevel=<Upper.
%% 检查角色等级
check_rebate_role_level({level, Lower, Upper}, RoleLevel)->
	RoleLevel>=Lower andalso RoleLevel=<Upper.

%% 获取返利信息
cs_activity_rebate_info(#cs_activity_rebate_info{}) ->
	case rebate_server:get_rebate_activity_info() of
		?undefined->
			?sendself(#sc_rebate_info{status=0, startTime=0, closeTime=0});
		#data_activity_rebate{vip=Vip, level=Level, activityName=Name, description=Description, iconSrc=Icon, startTime=StartTime_, closeTime=CloseTime_} ->
			#role{roleID=RoleID, vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
			case check_rebate_vip_level(Vip, VipLevel) andalso check_rebate_role_level(Level, RoleLevel) of
				true->
					StartTime = util:datetime_to_seconds(StartTime_),
					CloseTime = StartTime + rebate_server:relative_time(CloseTime_),
					RebateList = rebate_server:get_rebate_list(RoleID),
					?sendself(#sc_rebate_info{status=1, name=Name, description=Description, icon=Icon, startTime=StartTime,closeTime=CloseTime, rebateList=RebateList});
				false->
					?sendself(#sc_rebate_info{status=0, startTime=0, closeTime=0})
			end
	end.

%% 领取返利奖励
cs_activity_rebate_get_reward(#cs_activity_rebate_get_reward{rebateID=RebateID}) ->
	RoleID = role_data:get_roleID(),
	RebateInfo = rebate_server:get_roleinfo(RoleID),
	case rebate_server:get_reward(RebateID, RebateInfo) of
		{1, Reward} ->
			#p_rebate_reward{coin=Coin, gold=Gold, reputation=Reputation} = Reward,
			RoleInfo = role_data:get_roleInfo(),
			
			SellReward = #sell_reward{coin=Coin,gerExp=0,gold=Gold,item=0,newGer=0,reputation=Reputation,roleExp=0},
			role_reward:handle_sell_reward_f(RoleInfo, SellReward, ?MONEY_ADD_TYPE_REBATE, RebateID, ""),
			
			update_reward_status(RebateID, RebateInfo),
			?sendself(#sc_rebate_get_reward{result=1, reward=Reward});
		{X, _} ->
			?sendself(#sc_rebate_get_reward{result=X})
	end.

%% 标记奖励已经领取
update_reward_status(RebateID, RebateInfo)->
	RoleID = role_data:get_roleID(),
	case lists:keytake(RebateID, #rebate_info.id, RebateInfo) of
		false ->
			ignore;
		{value, Info, RebateInfo2}->
			NewRebateInfo = [Info#rebate_info{get=true} | RebateInfo2],
			rebate_server:set_roleinfo(RoleID, NewRebateInfo)
	end.

%%获取奖励列表
get_reward(BoxUnitList) ->
	?INFO("BoxList:~w ~n",[BoxUnitList]),
	lists:foldl(fun(BoxUnit,Acc) ->
		{BoxID,Time} = BoxUnit,
		[Config|_] = data_box:get({BoxID,0}),
		role_item:random_reward(Config,Time,Acc)
		end,[],BoxUnitList).

%%===================================================================================

update_rebateInfo_value([C,G,R], 1, Value)->
	[C+Value,G,R];
update_rebateInfo_value([C,G,R], 2, Value)->
	[C,G+Value,R];
update_rebateInfo_value([C,G,R], 3, Value)->
	[C,G,R+Value];
update_rebateInfo_value(Amount,_,_)->
	Amount.

update_rebate_info(Type, Value)->
	case rebate_server:get_rebate_activity_info() of
		?undefined ->
			ignore;
		#data_activity_rebate{vip=Vip, level=Level} ->
			#role{vipLevel=VipLevel, level=RoleLevel} = role_data:get_roleInfo(),
			case check_rebate_vip_level(Vip, VipLevel) andalso check_rebate_role_level(Level, RoleLevel) of
				true->
					RebateID = rebate_server:get_enabled_rebate_id(),
					update_rebate_info({update_rebate_info, RebateID, Type, Value});
				false->
					ignore
			end
	end.

%%	RebateInfo = [ #rebate_info{id=0,amount=[100,200,300], get=true}, #rebate_info{id=1,amount=[100,200,300], get=false}]
update_rebate_info({update_rebate_info, RebateID, Type, Delta})->
	RoleID = role_data:get_roleID(),
	RebateInfo = rebate_server:get_roleinfo(RoleID),
	NewRebateInfo=
		case lists:keytake(RebateID, #rebate_info.id, RebateInfo) of
			false ->
				[#rebate_info{id=RebateID, amount=update_rebateInfo_value([0,0,0],Type,Delta), get=false}|RebateInfo];
			{value, #rebate_info{amount=Amount}=Info, RebateInfo2}->
				[Info#rebate_info{amount=update_rebateInfo_value(Amount,Type,Delta)}|RebateInfo2]
		end,
	rebate_server:set_roleinfo(RoleID, NewRebateInfo),
	?sendself(#sc_rebate_update{}).
