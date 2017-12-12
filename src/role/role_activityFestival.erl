-module(role_activityFestival).

-compile(export_all).

-include("def_role.hrl").

%%玩家成长计划信息
%%===============================协议处理接口=========================================%%
cs_activityFestival_info(_) ->
	{ID,StartDate,EndDate} = data_activityFestival:get(openDate),
	DispID = data_activityFestival:get(displayID),
	Pos = data_activityFestival:get(pos),
	SignDesp = generate_sign_desp(data_activityFestival:get(sign_info)),
	BoxDesp = generate_box_desp(data_activityFestival:get(box_reward)),
	StartTime = util:datetime_to_seconds({StartDate,{0,0,1}}),
	EndTime = util:datetime_to_seconds({EndDate,{23,59,59}}),
	?sendself(#sc_activityFestival_info{startTime=StartTime,endTime=EndTime,displayID=DispID
									   ,activityID=ID,data=SignDesp,box=BoxDesp,pos=Pos}).


generate_sign_desp(List) ->
	[#p_activityFestival_data{day=Day,needGold=Need
							  ,reward=activity_server:sell_reward2p_reward_info(Reward)
							  }||{Day,Reward,Need} <-List].

generate_box_desp(List) ->
	[#p_activityFestival_box{id=ID
							,reward=activity_server:sell_reward2p_reward_info(Reward)
							,need=Need
							}||{ID,Reward,Need}<-List].


cs_activityFestival_self(_) ->
	#activity_festival{id=ID,sign=Sign,box=Box}=role_data:get_activityFestival(),
	{IDx,_,_} = data_activityFestival:get(openDate),
	if ID /= IDx ->
		   reset_activityFestival(),
		   ?sendself(#sc_activityFestival_self{activityID=IDx,self=[],box=[]});
	   true ->
		   Sign2 = generate_sign(Sign),
		   Box2 = generate_box(Box),
		   ?sendself(#sc_activityFestival_self{activityID=ID,self=Sign2,box=Box2})
	end.
	
generate_sign(List) ->
	[#p_activityFestival_self{day=ID,isSign=IsSign}||{ID,IsSign}<-List].

generate_box(List) ->
	[#p_activityFestival_box_get{id=ID,isGet=IsGet}||{ID,IsGet}<-List].


cs_activityFestival_sign(_) ->
	case check_sign() of
		 {true,ActivityFestival,Sign,XDay}->
			do_sign(ActivityFestival,Sign,XDay);
		{false,Reason} ->
			?sendself(#sc_activityFestival_sign{result=Reason,reward=[]})
			end.

check_sign()->
	#activity_festival{id=ID,sign=Sign}=ActivityFestival = role_data:get_activityFestival(),
	{NID,StartDate,EndDate} = data_activityFestival:get(openDate),
	if ID /= NID andalso ID /= 0 ->
		   reset_activityFestival(),
		   {false,2};
	   true ->
		   	StartTime = util:datetime_to_seconds({StartDate,{0,0,1}}),
			EndTime = util:datetime_to_seconds({EndDate,{23,59,59}}),
			Now = util:now(),
			if Now > StartTime andalso Now < EndTime ->
				   XDay = calc_day(StartTime,Now),
				   case lists:keyfind(XDay, 1, Sign) of
					   false ->
						   {true,ActivityFestival,Sign,XDay};
					   _->
						   {false,3}
				   end;
			   true ->
				   {false,4}
			end
 	end.

do_sign(ActivityFetival,Sign,XDay) ->
	Sign2 = [{XDay,1}|Sign],
	SignInfo = data_activityFestival:get(sign_info),
	case lists:keyfind(XDay, 1, SignInfo) of
		false ->
			?sendself(#sc_activityFestival_sign{result=3,reward=[]});
		{_,Reward,_} ->
			role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_ACTIVITYFESTIVAL,XDay,"0"),
			ActivityFestival2 = ActivityFetival#activity_festival{sign=Sign2},
			role_data:set_activityFestival(ActivityFestival2),
			?sendself(#sc_activityFestival_sign{result=1
											   ,reward=[activity_server:sell_reward2p_reward_info(Reward)]})
	end.
			

calc_day(StartTime,Now) ->
	Diff = Now - StartTime,
	trunc(Diff / ?ONE_DAY_SECONDS) + 1.
	
cs_activityFestival_box_get(#cs_activityFestival_box_get{id=ID}) ->
	case check_get(ID)of
		 {true,ActivityFestival,Box,Reward}->
			do_box_get(ActivityFestival,Box,ID,Reward);
		{false,Reason} ->
			?sendself(#sc_activityFestival_box_get{result=Reason,reward=[]})
	end.

check_get(GETID) ->
	#activity_festival{id=ID,box=Box,sign=Sign}=ActivityFestival = role_data:get_activityFestival(),
	{NID,StartDate,EndDate} = data_activityFestival:get(openDate),
	if ID /= NID andalso ID /= 0 ->
		   reset_activityFestival(),
		   {false,2};
	   true ->
		   	StartTime = util:datetime_to_seconds({StartDate,{0,0,1}}),
			EndTime = util:datetime_to_seconds({EndDate,{23,59,59}}),
			Now = util:now(),
			if Now > StartTime andalso Now < EndTime ->
				   case lists:keyfind(GETID, 1, Box) of
					   false ->
						   BoxInfo = data_activityFestival:get(box_reward),
						   case lists:keyfind(GETID, 1, BoxInfo) of
							   false ->
								   {false,3};
							   {_,Reward,Need} ->
								   SignDay = erlang:length(Sign),
								   if SignDay >= Need ->
										  {true,ActivityFestival,Box,Reward};
									  true ->
										  {false,5}
								   end
						   end;
					   _->
						   {false,3}
				   end;
			   true ->
				   {false,4}
			end
 	end.

do_box_get(ActivityFestival,Box,ID,Reward) ->
	Box2 = [{ID,1}|Box],
	role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_ACTIVITYFESTIVAL_BOX,ID,"1"),
	ActivityFestival2 = ActivityFestival#activity_festival{box=Box2},
	role_data:set_activityFestival(ActivityFestival2),
	?sendself(#sc_activityFestival_box_get{result=1
										  ,reward=[activity_server:sell_reward2p_reward_info(Reward)]}).

cs_activityFestival_sign2(#cs_activityFestival_sign2{day=Day}) ->
	case check_sign2(Day) of
		{true,ActivityFestival,Sign,Role,Need,Reward}->
			do_sign2(ActivityFestival,Sign,Role,Need,Reward,Day);
		{false,Reason} ->
			?sendself(#sc_activityFestival_sign2{result=Reason, reward=[]})
	end.

check_sign2(Day) ->
	#activity_festival{id=ID,sign=Sign}=ActivityFestival = role_data:get_activityFestival(),
	{NID,StartDate,EndDate} = data_activityFestival:get(openDate),
	if ID /= NID andalso ID /= 0 ->
		   reset_activityFestival(),
		   {false,2};
	   true ->
		   StartTime = util:datetime_to_seconds({StartDate,{0,0,1}}),
		   EndTime = util:datetime_to_seconds({EndDate,{23,59,59}}),
		   Now = util:now(),
		   if Now > StartTime andalso Now < EndTime ->
				  XDay = calc_day(StartTime,Now),
				  if XDay =< Day ->
						 {false, 6};
					 true ->
						 case lists:keyfind(Day, 1, Sign) of
							 false ->
								 SignInfo = data_activityFestival:get(sign_info),
								 case lists:keyfind(Day, 1, SignInfo) of
									 false ->
										 {false,3};
									 {_,Reward,Need} ->
										 Role = role_data:get_roleInfo(),
										 case role_lib:check_money(Role, gold, Need) of
											 true ->
												 {true,ActivityFestival,Sign,Role,Need,Reward};
											 _ ->
												 {false,5}
										 end
								 end;
							 _->
								 {false,3}
						 end
				  end;
			  true ->
				  {false,4}
		   end
	end.
		
do_sign2(ActivityFestival,Sign,Role,Need,Reward,Day) ->
	NewRoleInfo = role_lib:deduct_money_f(Role, gold, Need, ?MONEY_DEC_TYEP_ACTIVITYFESTIVAL_SIGN2, Day, "3"),
	role_reward:handle_sell_reward_f(NewRoleInfo,Reward,?MONEY_ADD_TYPE_ACTIVITYFESTIVAL_SIGN2,Day,"0"),
	Sign2 = [{Day,1}|Sign],
	ActivityFestival2 = ActivityFestival#activity_festival{sign=Sign2},
	role_data:set_activityFestival(ActivityFestival2),
	?sendself(#sc_activityFestival_sign2{result=1,reward
										=[activity_server:sell_reward2p_reward_info(Reward)]}).

reset_activityFestival()->
	{NID,_,_} = data_activityFestival:get(openDate),
	role_data:set_activityFestival(#activity_festival{id=NID}).
