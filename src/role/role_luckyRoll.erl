%% @author lixinglong
%% @doc 幸运大转盘
%% Created 2015-5-21


-module(role_luckyRoll).
-compile(export_all).
-include("def_role.hrl").
%% API functions

%% Internal functions

-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
%% ====================================================================
%% API functions
%% ====================================================================
cs_luckyRoll_get_list(_)->
	case get_activity_info() of
		false ->
			Info = activity_close();
		{ActivityID,StopTime,EndTime} ->
			#lucky_roll{activityID=ActivityID2}=LuckyRoll=role_data:get_lucky_roll(),
			case ActivityID2 of
				ActivityID ->
					#lucky_roll{baseBoxInfo=BaseBoxInfo,outer=Outer,inner=Inner,pos=Pos,mark=Mark,free_times=FreeTimes}=LuckyRoll;
				_ ->
					LuckyRoll2 = init_luckyRoll(ActivityID),
					#lucky_roll{baseBoxInfo=BaseBoxInfo,outer=Outer,inner=Inner,pos=Pos,mark=Mark,free_times=FreeTimes}=LuckyRoll2
			end,
			Rank = case ets:lookup(?ETS_LUCKY_ROLL,{rank,role_data:get_roleID()}) of
					   [] ->
						   0;
					   [{_,#p_luckyRoll_ranker{rankNum=RankNum}}] ->
						   RankNum
				   end,
			
			Info = #sc_luckyRoll_get_list{status=1,outers=outer2p(Outer),inners=Inner,pos=Pos,stopTime=StopTime,endTime=EndTime
										  ,freeTimes=data_lucky_roll:get(free_times)-FreeTimes,boxOpenProcess=BaseBoxInfo
										  ,mark=Mark,rank=Rank,oneTimeNeedGold=data_lucky_roll:get(oneTimeCost)
										  ,refreshNeedCoin=data_lucky_roll:get(refresh_cost)}
	end,
	?sendself(Info).

cs_luckyRoll_explore_one(_)->
	case get_activity_info() of
		false ->
			?sendself(#sc_luckyRoll_explore_one{type=2,mark=0});
		{_ID,StopTime,_} ->
			Now = util:now(),
			if Now >= StopTime ->
				   ?sendself(#sc_luckyRoll_explore_one{type=2,mark=0});
			   true ->
				   #role{level=Level,roleID=RoleID,roleName=RoleName} = Role = role_data:get_roleInfo(),
				   case data_lucky_roll:get(level_limit) > Level of
					   true ->
						   ?sendself(#sc_luckyRoll_explore_one{type=2,mark=0});
					   _ ->
						   #lucky_roll{free_times=FreeTimes,free_count=FC,buy_count=BC,mark=Mark} = LuckyRoll = role_data:get_lucky_roll(),
						   %AddMark = role_luckyRoll:get(add_mark),
						   case FreeTimes < data_lucky_roll:get(free_times) of
							   true ->
								   {Reward,Info,Outer} = do_explore_one(free,FC+1,LuckyRoll),
								   role_data:set_lucky_roll(LuckyRoll#lucky_roll{free_times=FreeTimes+1,free_count=FC+1,mark=Mark+1,outer=Outer}),
								   lucky_roll_server:add_mark(RoleID,RoleName,Mark+1),
								   do_reward(Reward),
								   ?sendself(#sc_luckyRoll_explore_one{type=1,mark=Mark+1,info=[Info]});
							   _ ->
								   NeedGold = data_lucky_roll:get(oneTimeCost),
								   case role_lib:check_money(Role,gold,NeedGold) of
									   true ->
										   {Reward,Info,Outer}=do_explore_one(gold, BC+1,LuckyRoll),
										   role_lib:deduct_gold_2_f(Role, NeedGold, ?MONEY_DEC_TYPE_LUCKY_ROLL_EXPLORE
																   , BC, "",role_reward:typed_reward_transform_normal([X||{_,X}<-Reward])),
										   role_data:set_lucky_roll(LuckyRoll#lucky_roll{buy_count=BC+1,mark=Mark+1,outer=Outer}),
										   lucky_roll_server:add_mark(RoleID,RoleName,Mark+1),
										   do_reward(Reward),
										   ?sendself(#sc_luckyRoll_explore_one{type=1,mark=Mark+1,info=[Info]});
									   _ ->
										   ?sendself(#sc_luckyRoll_explore_one{type=3,mark=Mark})
								   end
						   end
				   end
			end
	end.



cs_luckyRoll_explore_ten(_)->
	case get_activity_info() of
		false ->
			?sendself(#sc_luckyRoll_explore_ten{type=2,mark=0});
		{_ID,StopTime,_} ->
			Now = util:now(),
			if Now >= StopTime ->
				   ?sendself(#sc_luckyRoll_explore_ten{type=2,mark=0});
			   true ->
				   #role{level=Level,roleID=RoleID,roleName=RoleName} = Role = role_data:get_roleInfo(),
				   case data_lucky_roll:get(level_limit) > Level of
					   true ->
						   ?sendself(#sc_luckyRoll_explore_ten{type=2,mark=0});
					   _ ->
						   #lucky_roll{buy_count=BC,mark=Mark} = LuckyRoll = role_data:get_lucky_roll(),
						   %AddMark = role_luckyRoll:get(add_mark),
						   NeedGold = data_lucky_roll:get(oneTimeCost) * 10,
						   case role_lib:check_money(Role,gold,NeedGold) of
							   true ->
								   {Outer,Info,Reward}=do_explore_ten(gold, BC+1,LuckyRoll),
								   role_lib:deduct_gold_2_f(Role, NeedGold, ?MONEY_DEC_TYPE_LUCKY_ROLL_EXPLORE
														   , BC, "10",role_reward:typed_reward_transform_normal([X||{_,X}<-Reward])),
								   role_data:set_lucky_roll(LuckyRoll#lucky_roll{buy_count=BC+10,mark=Mark+10,outer=Outer}),
								   lucky_roll_server:add_mark(RoleID,RoleName,Mark+10),
								   do_reward(Reward),
								   ?sendself(#sc_luckyRoll_explore_ten{type=1,mark=Mark+10,info=Info});
							   _ ->
								   ?sendself(#sc_luckyRoll_explore_ten{type=3,mark=Mark})
						   end
				   end
			end
	end.

cs_luckyRoll_refresh(_)->
	case get_activity_info() of
		false ->
			?sendself(#sc_luckyRoll_refresh{type=2,outer=[]});
		{_ActivityID,StopTime,EndTime} ->
			Now = util:now(),
			if Now > StopTime andalso Now < EndTime ->
				   ?sendself(#sc_luckyRoll_refresh{type=4});
			   true ->
				   #role{level=Level} = Role = role_data:get_roleInfo(),
				   case Level < data_lucky_roll:get(level_limit) of
					   true ->
						   ?sendself(#sc_luckyRoll_refresh{type=2});
					   _ ->
						   NeedMoney = data_lucky_roll:get(refresh_cost),
						   case role_lib:check_money(Role, coin, NeedMoney) of
							   true ->
								   #lucky_roll{outer=Outer} =LuckyRoll= role_data:get_lucky_roll(),
								   Outer2 = [get_one_box(P, [MaskList])||{P,MaskList}<-Outer],
								   role_lib:deduct_money_f(Role, coin, NeedMoney, ?MONEY_DEC_TYPE_REFRESH_LUCKY_ROLL, 0, ""),
								   role_data:set_lucky_roll(LuckyRoll#lucky_roll{outer=Outer2}),
								   ?sendself(#sc_luckyRoll_refresh{type=1,outer=outer2p(Outer2)});
							   _ ->
								   ?sendself(#sc_luckyRoll_refresh{type=2})
						   end
				   end
			end
	end.

cs_luckyRoll_open_base_box(#cs_luckyRoll_open_base_box{pos=Pos}) ->
	case get_activity_info() of
		false ->
			?sendself(#sc_luckyRoll_open_base_box{type=4});
		_ ->
			#lucky_roll{baseBoxInfo=BaseBoxInfo,mark=Mark} = LuckyRoll = role_data:get_lucky_roll(),
			
			case lists:keytake(Pos,#p_baseBoxInfo.id,BaseBoxInfo) of
				false ->
					?sendself(#sc_luckyRoll_open_base_box{type=3});
				{value,#p_baseBoxInfo{isOpen=IsOpen,need=Need}=P,Others} ->
					
					if IsOpen == 1 ->
						   ?sendself(#sc_luckyRoll_open_base_box{type=2});
					   true ->
						   if Mark < Need ->
								  ?sendself(#sc_luckyRoll_open_base_box{type=3});
							  true ->
								  Role = role_data:get_roleInfo(),
								  Boxs = data_lucky_roll:get(base_reward),
								  {_,_,Reward} = lists:keyfind(Pos,1,Boxs),
								  role_reward:handle_sell_reward_f(Role,Reward,?MONEY_ADD_TYPE_LUCKY_BASE_REWARD,Pos,""),
								  NewOpenInfo = [P#p_baseBoxInfo{isOpen=1}|Others],
								  role_data:set_lucky_roll(LuckyRoll#lucky_roll{baseBoxInfo=NewOpenInfo}),
								  ?sendself(#sc_luckyRoll_open_base_box{type=1,boxOpenProcess=NewOpenInfo})
						   end
					end
			end
	end.

cs_luckyRoll_get_rankInfo(_) ->
	case get_activity_info() of
		false ->
			PSInfo = #p_luckyRoll_ranker{type=2,rankNum=0,mark=0,roleName=""
										 ,rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)},
			?sendself(#sc_luckyRoll_get_rankInfo{type=2,isGetRankReward=0,selfInfo=PSInfo,rankInfoList=[]});
		_ ->
			#lucky_roll{mark=Mark,isGetRankReward=IsGet}=role_data:get_lucky_roll(),
			#role{roleName=RoleName} = role_data:get_roleInfo(),
			[{_,RKList}] = ets:lookup(?ETS_LUCKY_ROLL, firstNRank),
			case ets:lookup(?ETS_LUCKY_ROLL, {rank,role_data:get_roleID()}) of
				[] ->
					PSInfo = #p_luckyRoll_ranker{type=2,rankNum=0,mark=Mark,roleName=RoleName
												 ,rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)},
					?sendself(#sc_luckyRoll_get_rankInfo{type=1,isGetRankReward=IsGet,selfInfo=PSInfo,rankInfoList=RKList});
				[{_,Info}] ->
					?sendself(#sc_luckyRoll_get_rankInfo{type=1,isGetRankReward=IsGet,selfInfo=Info,rankInfoList=RKList})
			end
	end.


draw_luckyRoll_rank_reward({draw_luckyRoll_rank_reward, Reward, Rank})->
	#lucky_roll{isGetRankReward=IsGet} = LuckyRoll = role_data:get_lucky_roll(),
	case IsGet of
		1 ->
			?sendself(#sc_luckyRoll_get_rank_Reward{type=5,rank=Rank,rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)});
		_ ->
			Role = role_data:get_roleInfo(),
			role_reward:handle_sell_reward_f(Role,Reward,?MONEY_ADD_TYPE_LUCKY_RANK_REWARD,Rank,""),
			role_data:set_lucky_roll(LuckyRoll#lucky_roll{isGetRankReward=1}),
			?sendself(#sc_luckyRoll_get_rank_Reward{type=1,rank=Rank,rewardInfo=activity_server:sell_reward2p_reward_info(Reward)})
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

init_luckyRoll(ActivityID)->
	Outer=role_luckyRoll:refresh_outers()
	,Inner=role_luckyRoll:refresh_inners()
	,Pos=#p_lucky_role_card_p{pos1=0,pos2=0}
	,BaseBoxInfo=role_luckyRoll:refresh_basebox()
	,LuckyRoll2=#lucky_roll{activityID=ActivityID,baseBoxInfo=BaseBoxInfo,outer=Outer,inner=Inner,pos=Pos},
	role_data:set_lucky_roll(LuckyRoll2),
	LuckyRoll2.

get_activityID() ->
	case  ets:lookup(?ETS_LUCKY_ROLL, state) of
		[{_,[#data_activity_lucky{activityID=ActivityID}]}]  ->
			ActivityID;
		_ ->
			0
	end.

get_activity_info() ->
	case  ets:lookup(?ETS_LUCKY_ROLL, state) of
		[{_,[#data_activity_lucky{activityID=ActivityID,stopTime=StopTime,endTime=EndTime}]}]  ->
			{ActivityID,StopTime,EndTime};
		_ ->
			false
	end.

activity_close()->
	#sc_luckyRoll_get_list{status=0,outers=[],inners=[],pos=#p_lucky_role_card_p{pos1=0,pos2=0},rank=0,mark=0
						   ,freeTimes=0,boxOpenProcess=[],stopTime=0,endTime=0,oneTimeNeedGold=0,refreshNeedCoin=0}.

refresh_outers()->
	[get_one_luckyRoll(P)||P<-data_lucky_roll:get(pos_list)].
refresh_inners()->
	[#p_luckyRoll_card_inner{pos2=E,id=E}||E<-data_lucky_roll:get(inner)].
refresh_basebox()->
	[#p_baseBoxInfo{id=ID,isOpen=0,need=Need,rewardInfo = activity_server:sell_reward2p_reward_info(Reward)}
				   ||{ID,Need,Reward}<-data_lucky_roll:get(base_reward)].

do_explore_one(gold, Count,#lucky_roll{outer=Outer})->
	Pos = case data_fixed_lucky_roll_gold:get(Count) of
			  ?undefined -> util:random_one_from_weigh_list(data_lucky_roll:get(pos_random));
			  X ->X
		  end,
	Pos2 = case data_fixed_lucky_roll_inner_gold:get(Count) of
			   ?undefined -> util:random_one_from_weigh_list(data_lucky_roll:get(random_inners));
			   X1 ->X1
		   end,
	%% Pos 就是倍数
	{value, {_,ValI}=Val, CardList2} = lists:keytake(Pos, 1, Outer),
	Reward = calc_reward(Pos2, Val),
	NewCloset = get_one_box(Pos,[ValI]),
	%	LuckyRoll2 = LuckyRoll#lucky_roll{outer=[NewCloset|CardList2],pos=#p_lucky_role_card_p{pos1=Pos,pos2=Pos2}},
	Info = cacl_explore_one_return(Val, NewCloset,Pos2),
	{Reward,Info,[NewCloset|CardList2]} ;
do_explore_one(free,Count,#lucky_roll{outer=Outer}) ->
	Pos = case data_fixed_lucky_roll_free:get(Count) of
			  ?undefined -> util:random_one_from_weigh_list(data_lucky_roll:get(pos_random));
			  X ->X
		  end,
	Pos2 = case data_fixed_lucky_roll_inner_free:get(Count) of
			   ?undefined -> util:random_one_from_weigh_list(data_lucky_roll:get(random_inners));
			   X1 ->X1
		   end,
	{value, {_,ValI}=Val, CardList2} = lists:keytake(Pos, 1, Outer),
	Reward = calc_reward(Pos2, Val),
	NewCloset = get_one_box(Pos,[ValI]),
	Info = cacl_explore_one_return(Val, NewCloset,Pos2),
	{Reward,Info,[NewCloset|CardList2]}.

do_explore_ten(gold,Count,#lucky_roll{outer=Outer}) ->
	lists:foldl(fun(CNT,{OuterAcc,OpenAcc,RewardAcc})->
						Pos = case data_fixed_lucky_roll_free:get(CNT) of
								  ?undefined -> util:random_one_from_weigh_list(data_lucky_roll:get(pos_random));
								  X ->X
							  end,
						Pos2 = case data_fixed_lucky_roll_inner_free:get(CNT) of
								   ?undefined -> util:random_one_from_weigh_list(data_lucky_roll:get(random_inners));
								   X1 ->X1
							   end,
						{value, {_,ValI}=Val, CardList2} = lists:keytake(Pos, 1, OuterAcc),
						Reward = calc_reward(Pos2, Val),
						NewCloset = get_one_box(Pos,[ValI]),
						{[NewCloset|CardList2],[cacl_explore_one_return(Val,NewCloset,Pos2)|OpenAcc],Reward++RewardAcc}
				end,{Outer,[],[]},lists:seq(Count,Count+9)).

calc_reward(P,E) ->
	lists:duplicate(P,E).

cacl_explore_one_return(Val,New,Pos2) ->
	#p_luckyRoll_card_oneTime{openCardList = outer2p([Val]),
							  newCardList = outer2p([New]),
							  pos2=Pos2							   
							 }.

do_reward(RewardList)->
	RewardList2 = 
		lists:foldl(fun({_, {R, C, V}=Val},Acc) ->
							case lists:member(Val, Acc) of
								false ->
									[Val|Acc];
								_ ->
									Acc2 = lists:delete(Val, Acc),
									[{R, C + C, V}|Acc2]
							end
					end, [], RewardList),
	lists:foreach(fun({RewardType, Count, Value}) ->
						  %%?ERR("奖品是:~p.~n", [{RewardType, Count, Value}]),
						  role_reward:handle_lucky_roll_reward_f(RewardType, Value, 1,Count)
				  end, RewardList2).


get_one_luckyRoll(P) ->
	get_one_box(P, []).

get_one_box(Pos, MaskList) ->
	RandomList = data_lucky_roll_setting:get(Pos),
	BoxList = util:random_one_from_weigh_list(RandomList),	
	BoxList2 = BoxList -- MaskList,
	
	BoxList3 =
		case BoxList2 of
			[] ->
				BoxList;
			_ ->
				BoxList2
		end,
	Box = util:random_one_from_list(BoxList3),
	{Pos, Box}.

outer2p(Outer)->
	[#p_luckyRoll_card_outer{pos1=P,type=A,count=B,cardType=C}||{P,{A,B,C}}<-Outer].


