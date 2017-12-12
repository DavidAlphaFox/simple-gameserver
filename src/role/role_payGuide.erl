-module(role_payGuide).

-compile(export_all).

-include("def_role.hrl").
-include("def_item.hrl").
-include("def_fight.hrl").
-include("def_doublematch.hrl").

-define(OUT_PERIOD,-1).

%%========任务状态==========
-define(TASK_UNFINISH,0).
-define(TASK_FINISH,1).
-define(TASK_REWARD_HAS_GAIN,2).
%%========任务类型=========
-define(ALLOW_ASYNC_TYPE_LIST,[?CARLOS_WIN_N,?RELIC_DIFFICULTY_N_WIN_N,?CONQUERISLAND_BOSS_DEMAGE_TN,?TWINS_DIFFICULTY_N_WIN_N,?DOUBLEMATCH_RANK_N,?PLANTATION_HAS_N]).
%%=======================================================================================



%%玩家成长计划信息
% -record(payGuideInfo,{currenttaskid=0,unaccepttasklist=[],acceptedtaskIDList=[]}).
%%===============================协议处理接口=========================================%%
cs_payGuide_info(_) ->
	MainGerTypeID = role_data:get_main_gerTypeID(),
	#payGuideInfo{unaccepttasklist=UnAcceptTaskList,showtaskID=ShowTaskID} = role_data:get_payGuide(),
	case lists:keyfind(ShowTaskID,#p_payGuide_unit.task1ID,UnAcceptTaskList) of
		false->
			?ERR("can not find task in UnAcceptTaskList:~w with taskid:~w ~n",[UnAcceptTaskList,ShowTaskID]),
			?sendself(#sc_payGuide_info{mainGerTypeID=MainGerTypeID,unit=#p_payGuide_unit{openState=0,task1ID=0,task2ID=0,task1State=0,task2State=0,task2Value=0,task1Reward=[],task2Reward=[]}});
		#p_payGuide_unit{task1ID=Task1ID,task2ID=Task2ID}=PayGuideData->
			#data_payGuide{reward=Task1Rewards} = data_payGuide:get(Task1ID),
			#data_payGuide{reward=Task2Rewards} = data_payGuide:get(Task2ID),
			Task1Reward = get_reward(Task1Rewards,MainGerTypeID),
			Task2Reward = get_reward(Task2Rewards,MainGerTypeID),
			?sendself(#sc_payGuide_info{mainGerTypeID=MainGerTypeID
							   ,unit=PayGuideData#p_payGuide_unit{task1Reward=[activity_server:sell_reward2p_reward_info(Task1Reward)]
																 ,task2Reward=[activity_server:sell_reward2p_reward_info(Task2Reward)]}})
	end.

%%领奖协议只减少可领取的任务项以及增加对应的领取任务项
cs_payGuide_get_reward(#cs_payGuide_get_reward{taskID=TaskID}) ->
	#payGuideInfo{acceptedtaskIDList=AcceptedTaskIDList}=PayGuideInfo= role_data:get_payGuide(),
	case check_payGuide_get_reward(TaskID) of
		{true,task1,_PayGuideUnit,OtherPayGuideUnitList} ->
			#data_payGuide{nextID=NextID,reward=Rewards} = data_payGuide:get(TaskID),
			% #data_payGuide{openState=OpenState,task2ID=NewTask2ID} = data_payGuide:get(NextID),
			Reward = get_reward(Rewards,role_data:get_main_gerTypeID()),
			role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_PAYGUIDE,TaskID,""),
			%%去掉当前领取了的任务，添加对应任务到已领取列表中
			role_data:set_payGuide(PayGuideInfo#payGuideInfo{unaccepttasklist=OtherPayGuideUnitList,acceptedtaskIDList=[TaskID|AcceptedTaskIDList],showtaskID=NextID}),
			?sendself(#sc_payGuide_get_reward{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward)]}),
			cs_payGuide_info(1);
		{true,task2,PayGuideUnit,OtherPayGuideUnitList} -> 
			% role_data:set_payGuide(PayGuideUnit#p_payGuide_unit{task2State=2}),
			NewPayGuideUnit = PayGuideUnit#p_payGuide_unit{task2State=2},
			#data_payGuide{reward=Rewards}=data_payGuide:get(TaskID),
			Reward = get_reward(Rewards,role_data:get_main_gerTypeID()),
			role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_PAYGUIDE,TaskID,""),
			role_data:set_payGuide(PayGuideInfo#payGuideInfo{acceptedtaskIDList=[TaskID|AcceptedTaskIDList],unaccepttasklist=[NewPayGuideUnit|OtherPayGuideUnitList]}),
			?sendself(#sc_payGuide_get_reward{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward)]}),
			cs_payGuide_info(1);
		{false,Reason} ->
			?sendself(#sc_payGuide_get_reward{result=Reason})
	end.
get_reward(Rewards,ID) ->
	case lists:keyfind(ID, 1, Rewards) of
		false -> {sell_reward,0,0,0,0,[],0,[]};
		{_,Reward} -> Reward
	end.

new_payGuideUnit() ->
	#data_payGuide{taskID=TaskID,task2ID=NewTask2ID,openState=OpenState} = data_payGuide:get(hd(data_payGuide:get_list())),
	new_payGuideUnit(OpenState,TaskID,NewTask2ID,0).
new_payGuideUnit(OpenState,NextID,NewTask2ID,Task2Value)->
	#data_payGuide{need=Need} = data_payGuide:get(NewTask2ID),
	Task2State = if Task2Value >= Need -> 1; true -> 0 end,
	#p_payGuide_unit{openState=OpenState,task1ID=NextID,task2ID=NewTask2ID,task1State=0,task2State=Task2State,task2Value=Task2Value}.

check_payGuide_get_reward(TaskID) ->
	#payGuideInfo{currenttaskid=_CTaskID,unaccepttasklist=UnAcceptTaskList,acceptedtaskIDList=AcceptedTaskIDList} = role_data:get_payGuide(),
	case lists:member(TaskID,AcceptedTaskIDList) of
		false->
			case lists:keytake(TaskID,#p_payGuide_unit.task1ID,UnAcceptTaskList) of
				false->
					case lists:keytake(TaskID,#p_payGuide_unit.task2ID,UnAcceptTaskList) of
						false->
							{false,2};
						{_Value,#p_payGuide_unit{task2State=Task2State}=PayGuideData,Other}->
							case Task2State of
								0->{false,4};
								2->{false,3};
								1->{true,task2,PayGuideData,Other}
							end
					end;
				{_Value,#p_payGuide_unit{task1State=Task1State}=PayGuideData,Other}->
					case Task1State of
						0->{false,4};
						2->{false,3};
						1->{true,task1,PayGuideData,Other}
					end
			end;
		true->
			{false,2}
	end.	
	
update_uprank(_TypeID,Rank) when Rank == 0 -> ignore;
update_uprank(TypeID,Rank) -> 
	MTypeID = role_data:get_main_gerTypeID(),
	#payGuideInfo{currenttaskid=CTaskID,unaccepttasklist=UnAcceptTaskList}=PayGuideData = role_data:get_payGuide(),
	case lists:keytake(CTaskID,#p_payGuide_unit.task1ID,UnAcceptTaskList) of
		{_Value,#p_payGuide_unit{task1ID=Task1ID,task1State=Task1State,task2Value=Task2Value}=PayGuideUnit,Other}->
			case (MTypeID == TypeID andalso Task1ID < 1003) orelse (data_common:get({gerTypeIDMega,MTypeID}) == TypeID andalso Task1ID > 1003)of
				true ->	
					#data_payGuide{need=Need,nextID=NextTaskID} = data_payGuide:get(Task1ID),
					if Rank == Need andalso Task1State ==0 ->
						#data_payGuide{openState=OpenState,task2ID=NewTask2ID} = data_payGuide:get(NextTaskID),
				   		% role_data:set_payGuide(PayGuideData#p_payGuide_unit{task1State=1}),
				   		AddPayGuideUnit = new_payGuideUnit(OpenState,NextTaskID,NewTask2ID,Task2Value),
				   		NewPayGuideUnit = PayGuideUnit#p_payGuide_unit{task1State=1},
				   		NewPayGuideData = PayGuideData#payGuideInfo{unaccepttasklist=[AddPayGuideUnit,NewPayGuideUnit|Other],currenttaskid=NextTaskID},
				   		role_data:set_payGuide(NewPayGuideData),
				   		cs_payGuide_info(1);
			   		true ->
				   		ignore
					end;
				_ ->
					ignore
			end;
		false->
			ignore
	end.
	
do_mega(TypeID) ->
	MTypeID = role_data:get_main_gerTypeID(),
	case data_common:get({gerTypeIDMega,MTypeID}) == TypeID of
		true ->	
			% #p_payGuide_unit{task1ID=Task1ID,task1State=Task1Status}=PayGuideData = role_data:get_payGuide(),
			#payGuideInfo{currenttaskid=CTaskID,unaccepttasklist=UnAcceptTaskList}=PayGuideData = role_data:get_payGuide(),
			case lists:keytake(CTaskID,#p_payGuide_unit.task1ID,UnAcceptTaskList) of
				{_Value,#p_payGuide_unit{task1ID=Task1ID,task1State=Task1State,task2Value=Task2Value}=PayGuideUnit,Other}->
					#data_payGuide{need=Need,nextID=NextTaskID} = data_payGuide:get(Task1ID),
					if 0 == Need andalso Task1State ==0 ->
						#data_payGuide{openState=OpenState,task2ID=NewTask2ID} = data_payGuide:get(NextTaskID),
				   		% role_data:set_payGuide(PayGuideData#p_payGuide_unit{task1State=1}),
				   		AddPayGuideUnit = new_payGuideUnit(OpenState,NextTaskID,NewTask2ID,Task2Value),
						NewPayGuideUnit = PayGuideUnit#p_payGuide_unit{task1State=1},
				   		NewPayGuideData = PayGuideData#payGuideInfo{unaccepttasklist=[AddPayGuideUnit,NewPayGuideUnit|Other],currenttaskid=NextTaskID},
				   		% role_data:set_payGuide(PayGuideData#p_payGuide_unit{task1State=1}),
				   		role_data:set_payGuide(NewPayGuideData),
				   		cs_payGuide_info(1);
			   		true ->
				   		ignore
					end;
				false->
					ignore
			end;
		_ ->
			ignore
	end.

%%根据前端显示的任务ID,依次修改该任务后面的所有UnAccept任务的task2的状态以及对应value
pay_gold(Gold) ->
	%%添加两个七日活动任务的触发
	trigger_task_change(?RECHARGE_ONCE_N,{Gold}),
	trigger_task_change(?RECHARGE_ACC_TN,{Gold}),
	#payGuideInfo{unaccepttasklist=UnAcceptTaskList,showtaskID=ShowTaskID}=PayGuideData = role_data:get_payGuide(),
	NewUnAcceptTaskList = update_task2_accvalue(UnAcceptTaskList,ShowTaskID,Gold),
	NewPayGuideData = PayGuideData#payGuideInfo{unaccepttasklist=NewUnAcceptTaskList},
	role_data:set_payGuide(NewPayGuideData).

update_task2_accvalue(UnAcceptTaskList,0,_Gold)->
	UnAcceptTaskList;
update_task2_accvalue(UnAcceptTaskList,Task1ID,Gold)->
	case lists:keytake(Task1ID,#p_payGuide_unit.task1ID,UnAcceptTaskList) of
		false->
			UnAcceptTaskList;
		{_Value,#p_payGuide_unit{task2ID=Task2ID,task2State=Task2State,openState=OpenState,task2Value=Task2Value}=PayGuideUnit,Other}->
			if 
				OpenState=:=0->
					UnAcceptTaskList;
				true->
					#data_payGuide{nextID=NextTaskID} = data_payGuide:get(Task1ID),
					#data_payGuide{need=Need} = data_payGuide:get(Task2ID),
					case Need >0 of
						true->
							Task2Value2 = Gold+Task2Value,
							Task2State2 = 
							if
								Task2State =:= 2 ->
									Task2State;
								true ->
									case Task2Value2 >= Need of
										true->
											1;
										false->
											Task2State
									end
							end,
							NewPayGuideUnit = PayGuideUnit#p_payGuide_unit{task2Value=Task2Value2,task2State=Task2State2},
							NewUnAcceptTaskList = [NewPayGuideUnit|Other],
							update_task2_accvalue(NewUnAcceptTaskList,NextTaskID,Gold);
						false->
						UnAcceptTaskList
					end
			end
	end.


%%=======================================================HeadSeven相关代码==============================================================%%

%%=============================================处理前端协议请求===========================================================
cs_payGuide_seven_info(#cs_payGuide_seven_info{})->
	#head_seven{begintime=BeginTime,period=Period,isfirst=IsFirst}=HS = role_data:get_headSeven(),
	case Period of
		?OUT_PERIOD->
			?sendself(#sc_payGuide_seven_info{state=0});
		_->
			EndTime1 = BeginTime + (data_headSeven:get(dotask_max_period))*data_headSeven:get(period_length),
			EndTime2 = BeginTime + (data_headSeven:get(reward_max_period))*data_headSeven:get(period_length),
			PeriodStateList = summary_period_state(),
			%%此处将首次登陆的标志设置成0,
			role_data:set_headSeven(HS#head_seven{isfirst=0}),
			?sendself(#sc_payGuide_seven_info{state=1,begintime=BeginTime,endtime1=EndTime1,endtime2=EndTime2,period=Period,periodState=PeriodStateList,isFirst=IsFirst})
	end.

cs_payGuide_seven_period_info(#cs_payGuide_seven_period_info{period=Period})->
	#head_seven{period=CurrentPeriod,doingtask=DoingTask,finishtask=FinishTask} = role_data:get_headSeven(),
	case CurrentPeriod of
		?OUT_PERIOD->
			?sendself(#sc_payGuide_seven_period_info{result=2,period=Period});
		_->
			case Period =<	data_headSeven:get(reward_max_period) of
				false->
					?sendself(#sc_payGuide_seven_period_info{result=3,period=Period});
				true->
					case Period =< CurrentPeriod of
						%%
						false->
							?sendself(#sc_payGuide_seven_period_info{result=4,period=Period});
						true->
							%%累计充值所有任务从第一天开始，但是前段是每天显示一个，故此处需要将所有的累计充值的任务都发送到前端，前端自行根据配置显示对应的一个						
							DoingTaskForPeriod = [E||#head_seven_task_unit{period=Period1,type=Type}=E<-DoingTask,Period1=:=Period orelse Type=:=?RECHARGE_ACC_TN],
							FinishTaskForPeriod = [E||#head_seven_task_unit{period=Period1,type=Type}=E<-FinishTask,Period1=:=Period orelse Type=:=?RECHARGE_ACC_TN],
							SortTaskUnitList = lists:sort(fun(A,B)->compare_task_unit(A,B) end,(DoingTaskForPeriod++FinishTaskForPeriod)),
							PTaskUnit = [transformtaskunit2ptaskunit(Unit)||Unit<-SortTaskUnitList],
							?sendself(#sc_payGuide_seven_period_info{result=1,tasklist=PTaskUnit,period=Period})
					end
			end
	end.

cs_payGuide_seven_draw(#cs_payGuide_seven_draw{taskID=TaskID})->
	#head_seven{finishtask=FinishTask,finishrewardtask=FinishRewardTask}= HeadSeven = role_data:get_headSeven(),
	case lists:keytake(TaskID,#head_seven_task_unit.taskID,FinishTask) of
		false->
			?sendself(#sc_payGuide_seven_draw{result=2});
		{_,FindTask,OtherTask}->
			%%check_headseven_draw函数中对钻石购买任务将会直接扣除玩家钻石
			case check_headseven_draw(FindTask) of
				{false,R}->
					?sendself(#sc_payGuide_seven_draw{result=R});
				{true,Reward}->
					Role = role_data:get_roleInfo(),
					%%发放对应的奖励
					role_reward:handle_sys_reward_with_return(Role,Reward,?MONEY_ADD_TYPE_HEADSEVEN,TaskID,"",true),
					%%更新HeadSeven
					NewFindTask = FindTask#head_seven_task_unit{state=?TASK_REWARD_HAS_GAIN},
					NewHeadSeven = HeadSeven#head_seven{finishtask=OtherTask,finishrewardtask=[NewFindTask|FinishRewardTask]},
					role_data:set_headSeven(NewHeadSeven),
					update_task_change_to_client(NewFindTask),
					update_period_summary(),
					PRewardInfo = role_reward:transform2p_reward_view(Reward,[]),
					?sendself(#sc_payGuide_seven_draw{result=1,reward=PRewardInfo})
			end
	end.

%%============================================================================================================================================================================
init_role_headSeven(RoleID)->
	%%此处获得的HeadSevenInfo中的下时段时间以及下时段时间的定时器都没有初始化，需要在此处进一步添加
	HeadSevenInfo1 =#head_seven{begintime=BeginTimeStamp,period=OldPeriod} = db_sql:get_role_headSeven(RoleID),
	{HeadSevenInfo,NeedTriggerAll} = case caculate_role_period(BeginTimeStamp) of
		?OUT_PERIOD->
			%%已经过了整个headSeven活动时间,直接设置设置阶段以及开始时间
			{#head_seven{period=?OUT_PERIOD,begintime=BeginTimeStamp},false};
		NowPeriod ->
			{update_headSeven_info(HeadSevenInfo1,NowPeriod),OldPeriod=/=NowPeriod}
	end,
	role_data:set_headSeven(HeadSevenInfo),
	put(headseven_need_trigger,NeedTriggerAll).

check_headSeven_and_trigger()->
	%%触发离线状态下可能出现的变化
	trigger_all_task_offline_change(),
	case get(headseven_need_trigger) of
		false->
			ignore;
		_->
			trigger_all_task_change()
	end.

update_headSeven_info(HeadSevenInfo=#head_seven{period=OldPeriod,doingtask=DoingTask,begintime=BeginTimeStamp,finishtask=FinishTask},Period)->
	case Period of
		?OUT_PERIOD->
			#head_seven{period=?OUT_PERIOD,begintime=BeginTimeStamp};
		_->
			NowSec = util:now(),
			NextPeriodTime = BeginTimeStamp + Period*data_headSeven:get(period_length),
			Ref = erlang:send_after((NextPeriodTime-NowSec)*1000,self(),{route,role_payGuide,{period_tick,Period+1}}),
			%%加入新旧时期间的所有任务
			{NewDoingTask,NewFinishTask,AddDoingTask} = add_headseven_task(OldPeriod+1,Period,DoingTask,FinishTask,[]),
			HeadSevenInfo#head_seven{period=Period,doingtask=NewDoingTask,nextperiodtime=NextPeriodTime,nextperiod_ref=Ref,finishtask=NewFinishTask}
	end.


%%包括BeginPeriod和EndPeriod两个时期的任务都会被加入
add_headseven_task(BeginPeriod,EndPeriod,DoingTask,FinishTask,AddDoingTask) when EndPeriod < BeginPeriod->
	{DoingTask,FinishTask,AddDoingTask};
add_headseven_task(BeginPeriod,EndPeriod,DoingTask,FinishTask,AddDoingTask)->
	TaskKeyList = [K||K<-data_headSeven:get_list(),is_number(K)],
	{NewDoingTask,NewFinishTask,NewAddTask} = lists:foldl(fun(K,{DoingAcc,FinishAcc,AddAcc})->
		E = #headSevenTask{taskPeriod=BeginPeriod1,taskType=Type,taskID=TaskID} = data_headSeven:get(K),
		case BeginPeriod1=:= BeginPeriod of
			true->
				case Type of
					?N_GOLD_PURCHASE->
						%%钻石购买任务直接设置成任务完成，可以兑换奖励，在兑换奖励时，来特殊处理此类型，消耗对应的钻石
						case is_task_already_exist(TaskID,FinishAcc) of
							false->
								Unit = #head_seven_task_unit{taskID=TaskID,type=Type,state=?TASK_FINISH,period=BeginPeriod1},
								{DoingAcc,[Unit|FinishAcc],[Unit|AddAcc]};
							true->
								?ERR("exist TaskID:~w in add_headseven_task ~n",[TaskID]),
								{DoingAcc,FinishAcc,AddAcc}
						end;
					?LOGIN_REWARD->
						%%登录奖励任务也直接设置成任务完成状态，直接可以领取
						case is_task_already_exist(TaskID,FinishAcc) of
							false->
								Unit = #head_seven_task_unit{taskID=TaskID,type=Type,state=?TASK_FINISH,period=BeginPeriod1},
								{DoingAcc,[Unit|FinishAcc],[Unit|AddAcc]};
							true->
								?ERR("exist TaskID:~w in add_headseven_task ~n",[TaskID]),
								{DoingAcc,FinishAcc,AddAcc}
						end;
					_->
						case is_task_already_exist(TaskID,DoingAcc) orelse is_task_already_exist(TaskID,FinishAcc) of
							false->
								Unit = #head_seven_task_unit{taskID=TaskID,type=Type,state=?TASK_UNFINISH,period=BeginPeriod1},
								{[Unit|DoingAcc],FinishAcc,[Unit|AddAcc]};
							true->
								?ERR("exist TaskID:~w in add_headseven_task ~n",[TaskID]),
								{DoingAcc,FinishAcc,AddAcc}
						end
				end;
			false->
				{DoingAcc,FinishAcc,AddAcc}
		end
	end,{DoingTask,FinishTask,AddDoingTask},TaskKeyList),
	add_headseven_task(BeginPeriod+1,EndPeriod,NewDoingTask,NewFinishTask,NewAddTask).

is_task_already_exist(TaskID,TaskList)->
	case lists:keyfind(TaskID,#head_seven_task_unit.taskID,TaskList) of
		false->
			false;
		_->
			true
	end.
%%计算玩家当前处于的活动阶段0表示已经超过了活动结束时间
caculate_role_period(BeginTimeStamp)->
	NowSec = util:now(),
	caculate_role_period2(BeginTimeStamp,NowSec).
caculate_role_period2(BeginTimeStamp,NowSec)->
	case BeginTimeStamp =:= NowSec of
		%%此处需要针对初次登陆游戏的玩家做处理
		true->
			1;
		_->
			Interval = NowSec - BeginTimeStamp,
			PeriodLength = data_headSeven:get(period_length),
    		Div = Interval div PeriodLength,
    		Rem = Interval rem PeriodLength,
    		NewPeriod = case Rem of
    			0->
    				Div;
    			_ ->
    				Div+1
    		end,
    		case is_period_valid(NewPeriod) of
    			true->
    				NewPeriod;
    			false->
    				?OUT_PERIOD
    		end
    end.

%%处理period切换,可能传递过来的Period是无效的
period_tick({_,Period})->
	#head_seven{doingtask=DoingTask,period=OldPeriod,begintime=BeginTimeStamp,finishtask=FinishTask}=HeadSeven = role_data:get_headSeven(),
	NowSec = util:now(),
	{NewHeadSeven,NeedTrigger} =  
		case is_period_valid(Period) of
			true->
				%%已经进入Period阶段，此处的下一阶段要增加1
				NextPeriodTime = BeginTimeStamp+Period*data_headSeven:get(period_length),
				Ref = erlang:send_after((NextPeriodTime-NowSec)*1000,self(),{route,role_payGuide,{period_tick,Period+1}}),
				{NewDoingTask,NewFinishTask,AddDoingTask} = add_headseven_task(OldPeriod+1,Period,DoingTask,FinishTask,[]),
				%%推送跨越period之后新增加的能够查看的活动
				PTaskUnit = [transformtaskunit2ptaskunit(Unit)||Unit<-AddDoingTask],
				% ?sendself(#sc_payGuide_seven_period_info{result=1,tasklist=PTaskUnit,period=Period}),
				{HeadSeven#head_seven{doingtask=NewDoingTask,period=Period,nextperiodtime=NextPeriodTime,nextperiod_ref=Ref,finishtask=NewFinishTask},true};
			false->
				%%活动已经结束
				{#head_seven{begintime=BeginTimeStamp,period=?OUT_PERIOD},false}
		end,
	role_data:set_headSeven(NewHeadSeven),
	%%此处调用一下该函数，向前端推送新的Period。
	%?ERR("cs_payGuide_seven_info running ~n"),
	cs_payGuide_seven_info(#cs_payGuide_seven_info{}),
	case NeedTrigger of true->trigger_all_task_change();false->ignore end.

transform_persist2term(L)->
	lists:foldl(fun({TaskID,FinishProgress,State},Acc)->
		case data_headSeven:get(TaskID) of
			?undefined->
				?ERR("exist undefined headseven task :~w ~n",[TaskID]),
				Acc;
			#headSevenTask{taskType=Type,taskPeriod=Period}->
				[#head_seven_task_unit{taskID=TaskID,finishprogress=FinishProgress,type=Type,state=State,period=Period}|Acc]
		end
	end,[],L).

transform_term2persist(L)->
	[{TaskID,FinishProgress,State}||#head_seven_task_unit{taskID=TaskID,finishprogress=FinishProgress,state=State}<-L].

transformtaskunit2ptaskunit(#head_seven_task_unit{taskID=TaskID,finishprogress=FinishProgress,state=State})->
	case data_headSeven:get(TaskID) of
		#headSevenTask{taskType=?ROLE_LEVEL_N}->
			%%玩家等级任务，存在转生和未转生两种情况，客户端大爷要求区分两种数据显示
			#p_task_unit{taskID=TaskID,taskFinish=change_role_level(FinishProgress),taskState=State};
		_->
			#p_task_unit{taskID=TaskID,taskFinish=FinishProgress,taskState=State}
	end.

is_period_valid(Period)->
	Period >=0 andalso Period =< data_headSeven:get(reward_max_period).

check_headseven_draw(#head_seven_task_unit{state=?TASK_FINISH,taskID=TaskID})->
	case data_headSeven:get(TaskID) of
		?undefined->
			{false,2};
		#headSevenTask{taskReward=Reward,taskCondition=Condition,taskType=Type}->
			case Type of
				%%钻石购买活动，初始化的时候就设置成了完成状态，在此处需要直接根据配置来消耗对应的钻石，然后发放奖励即可
				?N_GOLD_PURCHASE->
					NeedGold = element(1,Condition),
					Role = role_data:get_roleInfo(),
					case role_lib:check_money(Role,gold,NeedGold) of
						true->
							Role1 = role_lib:deduct_gold_f(Role,NeedGold,?MONEY_DEC_TYPE_HEADSEVEN,TaskID,""),
							role_data:set_roleInfo(Role1),
							{true,Reward};
						false->
							{false,6}
					end;
				_->
					{true,Reward}
			end
	end.

%%这个函数只是处理其他进程发送过来的异步触发的消息
trigger_task_change_asyn({_,Type,Args})->
	trigger_task_change(Type,Args).


trigger_task_change(Type,Args)->
	%%此处将各种变化推送到主任务模块中
	role_maintask:do_main_task(Type,Args),
	HeadSeven = role_data:get_headSeven(),
	%%此处可能会存在HeadSeven未初始化完成就开始进行任务触发的情况（目前是战斗力任务）
	case HeadSeven of
		?undefined->
			ignore;
		_->
			%?ERR("HeadSeven:~w ~n",[HeadSeven]),
			case trigger_task_change2(HeadSeven,Type,Args) of
				ignore->
					ignore;
				{NewHeadSeven,NewFinishTask,UpdateTask}->
					%%更新七日活动信息
					role_data:set_headSeven(NewHeadSeven),
					% case UpdateTask of
					% 	[]->
					% 		ignore;
					% 	_->
					% 		ignore
					% 		%%任务更新取消向前端发送变化，进入界面时会重新请求数据，故不需要推送
					% 		% update_task_change_to_client(UpdateTask)
					% end,
					case NewFinishTask of
						[]->
							ignore;
						_->
							update_period_summary()
					end
			end
	end.

%%增加这个函数主要是想在离线状态下也能通过数据库来触发玩家任务变化,由于某些任务有获取玩家进程字典的情况，Type一定要筛选过。
trigger_task_change2(HeadSeven,Type,Args)->
	#head_seven{doingtask=DoingTask,finishtask=FinishTask,period=CPeriod}= HeadSeven,	
	case CPeriod > data_headSeven:get(dotask_max_period) orelse CPeriod=:=?OUT_PERIOD of
		true->
			ignore;
		_->
			case trigger_task_change3(Type,Args,DoingTask) of
				{NewDoingTask,NewFinishTask=[],UpdateTask}->
					{HeadSeven#head_seven{doingtask=NewDoingTask},NewFinishTask,UpdateTask};
				{NewDoingTask,NewFinishTask,UpdateTask}->
					NewFinishTask1 = merge_insert_task(FinishTask,NewFinishTask),
					{HeadSeven#head_seven{doingtask=NewDoingTask,finishtask=NewFinishTask1},NewFinishTask,UpdateTask}
			end
	end.

trigger_task_change3(Type,Args,TaskList)->
	trigger_task_change4(Type,Args,TaskList,{[],[],[]}).

trigger_task_change4(Type,Args,[],{DoingAcc,FinishAcc,UpdateAcc})->
	{DoingAcc,FinishAcc,UpdateAcc};
trigger_task_change4(Type,Args,[H|T],{DoingAcc,FinishAcc,UpdateAcc})->
	case trigger_single_task_change(Type,Args,H) of
		%%doing表示没有更新
		{doing,NewTask}->
			trigger_task_change4(Type,Args,T,{[NewTask|DoingAcc],FinishAcc,UpdateAcc});
		{update,NewTask}->
			trigger_task_change4(Type,Args,T,{[NewTask|DoingAcc],FinishAcc,[NewTask|UpdateAcc]});			
		{finish,NewTask}->
			trigger_task_change4(Type,Args,T,{DoingAcc,[NewTask|FinishAcc],[NewTask|UpdateAcc]})
	end.

trigger_single_task_change(Type,Args,Task=#head_seven_task_unit{type=TaskType,taskID=TaskID})->
	case Type =:= TaskType of
		false->
			%%不更新
			{doing,Task};
		true->
			case data_headSeven:get(TaskID) of
				?undefined->
					?ERR("undefined task:~w ~n",[TaskID]),
					%%不更新
					{doing,Task};
				HeadSevenTaskConfigUnit->
					trigger_single_task_change2(Type,Args,Task,HeadSevenTaskConfigUnit)
			end
	end.

trigger_single_task_change2(Type,Args,Task=#head_seven_task_unit{finishprogress=OldFinishProgress},#headSevenTask{taskCondition=Condition})->
	%%此处有特殊的情况，比如排行榜进前N名，小于N的应该都能够触发，这与其他的情况刚好相反,故返回两者的负数来进行比较
	{NewFinishProgress,FinishProgress} = increase_finishprogress(Type,Args,Condition,OldFinishProgress),
	case NewFinishProgress >= FinishProgress of
		true->
			case Type of
				?PVP_RANK_N->
					NewTask = Task#head_seven_task_unit{finishprogress=abs(NewFinishProgress),state=?TASK_FINISH};
				?RECHARGE_ONCE_N->
					%%应前端要求，把单笔充值的进度扩大10倍。
					NewTask = Task#head_seven_task_unit{finishprogress=abs(FinishProgress)*10,state=?TASK_FINISH};
				_->
					NewTask = Task#head_seven_task_unit{finishprogress=abs(FinishProgress),state=?TASK_FINISH}
			end,
			%%更新
			{finish,NewTask};
		false->
			NewTask = Task#head_seven_task_unit{finishprogress=abs(NewFinishProgress)},
			case abs(NewFinishProgress)=:=abs(OldFinishProgress) of
				true->
					%%进度一致，不更新
					{doing,NewTask};
				false->
					%%更新
					{update,NewTask}
			end
	end.

%%将TaskList2中的Task插入TaskList1中，不重复
merge_insert_task(TaskList1,[])->
	lists:reverse(TaskList1);
merge_insert_task(TaskList1,[H|T])->
	case lists:keyfind(#head_seven_task_unit.taskID,H#head_seven_task_unit.taskID,TaskList1) of
		false->
			merge_insert_task([H|TaskList1],T);
		_->
			merge_insert_task(TaskList1,T)
	end.

%%针对每一个任务做特殊处理
%%处理钻石购买
increase_finishprogress(?LOGIN_REWARD,Args,Condition,OldFinishProgress)->
	?ERR("登录奖励不应该出现在未完成任务列表中~n"),
	{0,999};
increase_finishprogress(?N_GOLD_PURCHASE,Args,Condition,OldFinishProgress)->
	?ERR("钻石购买不应该出现在未完成的任务列表中~n"),
	{0,999};

%%单次充值需要完全的相等，不能使用increase_finishprogress_normal函数
increase_finishprogress(?RECHARGE_ONCE_N,{ChargeNum},Condition,OldFinishProgress)->
	NeedValue = element(1,Condition),
	FinishProgress = get_task_finish_progress(?RECHARGE_ONCE_N,Condition),
	IncProgress = case ChargeNum=:=NeedValue of true->1;false->0 end,
	{IncProgress+OldFinishProgress,FinishProgress};

increase_finishprogress(?RECHARGE_ACC_TN,{ChargeNum},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?RECHARGE_ACC_TN,Condition),
	{ChargeNum+OldFinishProgress,FinishProgress};
increase_finishprogress(?ROLE_LEVEL_N,{RoleLevel},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?ROLE_LEVEL_N,Condition),
	{RoleLevel,FinishProgress};	
increase_finishprogress(?FORMATION_GER_STAR_N_QUALITY_N_ISMEGA_TN,_Args,Condition,OldFinishProgress)->
	NeedStarList = element(1,Condition),
	NeedQuality = element(2,Condition),
	IsNeedMega = element(3,Condition),
	FinishProgress = get_task_finish_progress(?FORMATION_GER_STAR_N_QUALITY_N_ISMEGA_TN,Condition),
	PosList = role_data:get_posList(),
	SumNum = lists:foldl(fun(#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerQuality=GerQuality}},AccNum)->
			#data_ger{gerStar=Star} = data_ger:get(GerTypeID),
			case IsNeedMega of
				ignore->
					case lists:member(Star,NeedStarList) andalso GerQuality >= NeedQuality of
						true->
							AccNum+1;
						false->
							AccNum
					end;
				_ ->
					IsGerMega = role_awake:check_ger_mega(GerTypeID),
					case IsNeedMega=:=IsGerMega andalso lists:member(Star,NeedStarList) andalso GerQuality >= NeedQuality of
						true->
							AccNum+1;
						false->
							AccNum
					end	
			end	
		end,0,PosList),
	{SumNum,FinishProgress};
%%针对所有上阵统计的都直接获取数据来处理，不使用传递来的参数，并且随着状态的变化，可能新的进度会比旧的进度小，但是一旦完成这个任务，将会不会更新对应的进度
increase_finishprogress(?FORMATION_GER_LEVEL_TN,_Args,Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FORMATION_GER_LEVEL_TN,Condition),
	PosList = role_data:get_posList(),
	LevelList = [Level||#ger{gerBase=#gerBase{gerLevel=Level}}<-PosList],
	SumLevel = lists:sum(LevelList), 
	%?ERR("SumLevel:~w LevelList:~w FinishProgress:~w ~n",[SumLevel,LevelList,FinishProgress]),
	{SumLevel,FinishProgress};

%%修改战役完成不通过参数来修改，直接读取玩家的信息
increase_finishprogress(?BATTLE_FINISH,{ChapterID},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?BATTLE_FINISH,Condition),
	NeedChapterID = element(1,Condition),
	% %%章节ID以10000作为起始ID，故此处需要减去10000来表示具体的章节数量
	% ChapterNum = erlang:max(ChapterID - 10000,0),
	% Progress = case ChapterNum >= NeedChapterID of true->1;false->0 end,
	% {Progress,FinishProgress};
	#role_xbattle{chapterID=ChapterIDNow}=Xbattle=role_data:get_xbattle_data(),
	#xbattle_chapter{isGetReward=IsGetReward}=ChapterData=role_xbattle:get_chapter(ChapterIDNow),
	case data_xbattle_chapter:get(ChapterIDNow) of
		?undefined->
			Progress = OldFinishProgress;
		#data_xbattle_chapter{dungeonCount=DungeonCount}->
			case IsGetReward > 1 of
	    		true->
	    			ChapterNum = erlang:max(ChapterIDNow - 10000,0);
	    		false->
	        		ChapterNum = erlang:max(ChapterIDNow-10000-1,0)
			end,
			Progress = case ChapterNum >= NeedChapterID of true->1;false->0 end
	end,
	{Progress,FinishProgress};

increase_finishprogress(?FORMATION_GER_EQUIP_SUIT_X_TN,_Args,Condition,_OldFinishProgress)->
	NeedSuitIDList = element(1,Condition),
	FinishProgress = get_task_finish_progress(?FORMATION_GER_EQUIP_SUIT_X_TN,Condition),
	PosList = role_data:get_posList(),
	SumNum = lists:foldl(fun(#ger{gerID=GerID},AccNum)->
			SuitNum = check_ger_suit(GerID,NeedSuitIDList),
			% case SuitNum=:=length(NeedSuitIDList) of
			%%此处修改为满足任意套装即符合条件
			case SuitNum > 0 of
				true->AccNum+1;
				false->AccNum
			end
		end,0,PosList),
	{SumNum,FinishProgress};
increase_finishprogress(?FORMATION_GER_EQUIP_LEVEL_TN,_Args,Condition,_OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FORMATION_GER_EQUIP_LEVEL_TN,Condition),
	PosList = role_data:get_posList(),
	SumNum = lists:foldl(fun(#ger{gerID=GerID},AccNum)->
			GerEquipList = role_data:get_equip(GerID),
			GerNormalEquipLevelList = [Level||#item{itemLevel=Level,itemType=ItemType}<-GerEquipList,lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE)],
			SumEquipLevel = lists:sum(GerNormalEquipLevelList),
			AccNum+SumEquipLevel
		end,0,PosList),
	{SumNum,FinishProgress};
increase_finishprogress(?FORMATION_GER_DESTINY_TN,_Args,Condition,_OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FORMATION_GER_DESTINY_TN,Condition),
	PosList = role_data:get_posList(),
	PosGerTypeIDList = [GerTypeID||#ger{gerBase=#gerBase{gerTypeID=GerTypeID}}<-PosList],
	LieuList = role_data:get_lieuposList(),
	LieuGerTypeIDList = [GerTypeID||#ger{gerBase=#gerBase{gerTypeID=GerTypeID}}<-LieuList],
	%%此处直接使用了所有上阵精灵和小伙伴的typeID
	ArmedGerTypeIDList = lists:append([PosGerTypeIDList,LieuGerTypeIDList]),
	SumDestinyNum = lists:foldl(fun(#ger{gerBase=#gerBase{gerTypeID=GerTypeID},gerID=GerID},AccNum)->
		GerEquipList = role_data:get_equip(GerID),
		SingleGerDestinyNum = calculate_single_ger_destiny(GerTypeID,ArmedGerTypeIDList,GerEquipList),
		% ?ERR("SingleGerDestinyNum:~w GerEquipList:~w GerTypeID:~w ArmedGerTypeIDList:~w ~n",[SingleGerDestinyNum,GerEquipList,GerTypeID,ArmedGerTypeIDList]),
		AccNum+SingleGerDestinyNum
	end,0,PosList),
	{SumDestinyNum,FinishProgress};
increase_finishprogress(?BUIDING_X_LEVEL_N,_Args,Condition,OldFinishProgress)->
	NeedBuildType = element(1,Condition),
	FinishProgress = get_task_finish_progress(?BUIDING_X_LEVEL_N,Condition),
	Level =  role_home:get_build_type_level(role_data:get_roleID(),NeedBuildType),
	{Level,FinishProgress};

increase_finishprogress(?FORMATION_GER_EQUIP_STAR_TN,_Args,Condition,_OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FORMATION_GER_EQUIP_STAR_TN,Condition),
	PosList = role_data:get_posList(),
	SumNum = lists:foldl(fun(#ger{gerID=GerID},AccNum)->
			GerEquipList = role_data:get_equip(GerID),
			GerNormalEquipStarList = [begin #data_item{itemStar=Star} = data_item:get(ItemTypeID), Star end||#item{itemTypeID=ItemTypeID,itemType=ItemType}<-GerEquipList,lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE)],
			SumEquipStar = lists:sum(GerNormalEquipStarList),
			AccNum+SumEquipStar
		end,0,PosList),
	{SumNum,FinishProgress};

increase_finishprogress(?FORMATION_GER_STAR_TN,_Args,Condition,_OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FORMATION_GER_STAR_TN,Condition),
	PosList = role_data:get_posList(),
	StarList = [begin #data_ger{gerStar=Star} = data_ger:get(GerTypeID),Star end||#ger{gerBase=#gerBase{gerTypeID=GerTypeID}}<-PosList],
	SumStar = lists:sum(StarList),
	{SumStar,FinishProgress};
%%此处的TalentID需要包括对应天赋的等级，不然不同等级的同种天赋会造成条件满足,后续可能会修改,
increase_finishprogress(?ACTIVATE_TALENT,_Args,Condition,OldFinishProgress)->
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	NeedTalentIDList = element(1,Condition),
	FinishProgress = get_task_finish_progress(?ACTIVATE_TALENT,Condition),
    TalentList = role_data:get_trainer_info(),
    SatifyTalentTypeList = [Type||{Type,_Level,_CD}<-TalentList,lists:member(Type,NeedTalentIDList)],
    {OldFinishProgress+length(SatifyTalentTypeList),FinishProgress};

increase_finishprogress(?FORMATION_GER_EQUIP_RANK_TN,_Args,Condition,_OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FORMATION_GER_EQUIP_RANK_TN,Condition),
	PosList = role_data:get_posList(),
	SumRank = lists:foldl(fun(#ger{gerID=GerID},AccNum)->
		EquipList = role_data:get_equip(GerID),
		EquipRankList = [EquipRank||#item{itemRank=EquipRank,itemType=ItemType}<-EquipList,lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE)],
		AccNum + lists:sum(EquipRankList)
	end,0,PosList),
	{SumRank,FinishProgress};
%%符文数量获得改成对上阵精灵装备以及背包中符文数量的统计
increase_finishprogress(?GAIN_STONE_STAR_N_TN,_Args,Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?GAIN_STONE_STAR_N_TN,Condition),
	NeedStar = element(1,Condition),
	IsEssenceStone = element(2,Condition),
	BagStone = comulat_stone(role_data:get_bagEquip(),NeedStar,IsEssenceStone),
    PosGerIDList = [GerID||#ger{gerID=GerID}<-role_data:get_posList()],
    PosGerStoneNumList = [length(comulat_stone(role_data:get_equip(GerID),NeedStar,IsEssenceStone))||GerID<-PosGerIDList],
    {lists:sum([length(BagStone)|PosGerStoneNumList]),FinishProgress};

increase_finishprogress(?FORMATION_GER_STONE_TN,_Args,Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FORMATION_GER_STONE_TN,Condition),
	IsEssenceStone = element(1,Condition),
	PosList = role_data:get_posList(),
	SumStoneNum = lists:foldl(fun(#ger{gerID=GerID},AccNum)->
		EquipList = role_data:get_equip(GerID),
		%%此处不对符文星级做限制
		StoneTypeList = comulat_stone(EquipList,-1,IsEssenceStone),
		AccNum + length(StoneTypeList)
	end,0,PosList),
	{SumStoneNum,FinishProgress};
increase_finishprogress(?FIGHTPOWER,{FightPower},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?FIGHTPOWER,Condition),
	{FightPower,FinishProgress};

%%此处返回的是两个的负数(主要是和其他的进度的比较刚好相反，所以使用负数)，在设置进度的时候要取绝对值
increase_finishprogress(?PVP_RANK_N,_Args,Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?PVP_RANK_N,Condition),
	case role_data:get_roleInfo() of
		?undefined->
			{-OldFinishProgress,-FinishProgress};
		#role{roleID=RoleID}->
			CurrentPvPRank = role_data:get_pvp_rank(RoleID),
			{-CurrentPvPRank,-FinishProgress}
	end;
increase_finishprogress(?CHALLENGE_KILL_BOSS_N,{Times},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?CHALLENGE_KILL_BOSS_N,Condition),
	{OldFinishProgress+Times,FinishProgress};
increase_finishprogress(?CARLOS_WIN_N,{Times},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?CARLOS_WIN_N,Condition),
	{OldFinishProgress+Times,FinishProgress};
increase_finishprogress(?RELIC_DIFFICULTY_N_WIN_N,{DifficultType,_RewardRank,Times},Condition,OldFinishProgress)->
	NeedDifficultyType = element(1,Condition),
	FinishProgress = get_task_finish_progress(?RELIC_DIFFICULTY_N_WIN_N,Condition),
	case DifficultType=:=NeedDifficultyType of
		true->
			{OldFinishProgress+1,FinishProgress};
		false->
			{OldFinishProgress,FinishProgress}
	end;
%%无尽深渊使用玩家的历史最高关卡
increase_finishprogress(?HRON_REACH_N,_Args,Condition,OldFinishProgress)->
	BestHronScore = case [Score||#hron_history_info{bestscore=Score}<-role_data:get_hronhistory()] of 
		[]->
			0;
		L ->lists:max(L)
	end,
	FinishProgress = get_task_finish_progress(?HRON_REACH_N,Condition),
	{BestHronScore,FinishProgress};
increase_finishprogress(?PLANTATION_HAS_N,_Args,Condition,OldFinishProgress)->
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	FinishProgress = get_task_finish_progress(?PLANTATION_HAS_N,Condition),
	MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
	{length(MachineList),FinishProgress};
increase_finishprogress(?CONQUERISLAND_BOSS_DEMAGE_TN,{Demage},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?CONQUERISLAND_BOSS_DEMAGE_TN,Condition),
	{Demage,FinishProgress};
increase_finishprogress(?TWINS_DIFFICULTY_N_WIN_N,{DifficultType,Times},Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?TWINS_DIFFICULTY_N_WIN_N,Condition),
	NeedDifficultyType = element(1,Condition),
	case DifficultType=:=NeedDifficultyType of
		true->
			{OldFinishProgress+Times,FinishProgress};
		false->
			{OldFinishProgress,FinishProgress}
	end;
increase_finishprogress(?DOUBLEMATCH_RANK_N,_Args,Condition,OldFinishProgress)->
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	FinishProgress = get_task_finish_progress(?DOUBLEMATCH_RANK_N,Condition),
	NeedRank = element(1,Condition),
	case ets:lookup(?ETS_DM_INFO_LIST, RoleID) of
        [] ->
        	?ERR("not find DOUBLEMATCH_RANK~n"),
        	{0,FinishProgress};
        [DmDbRecord0] when erlang:is_record(DmDbRecord0, dm_db_record) ->
        	#dm_db_record{dm_data=#dm_data{rank=Rank}} = DmDbRecord0,
        	{_TopScore,GRank,_GLevel,_,_} = data_doublematch:get({rank,Rank}),
        	%%此处使用大段位是因为超凡大师和王者段位的特殊性，找策划了解
        	Progress = case GRank >= NeedRank of true->1;false->0 end,
        	{Progress,FinishProgress}
    end;
increase_finishprogress(?PLANE_LEVEL_N,_Args,Condition,OldFinishProgress)->
	FinishProgress = get_task_finish_progress(?PLANE_LEVEL_N,Condition),
	NeedPlaneLevel = element(1,Condition),
	#role{plane_level=PlaneLevel} = role_data:get_roleInfo(),
	NewFinishProgress = case NeedPlaneLevel =< PlaneLevel of true->1;false->0 end, 
	{NewFinishProgress,FinishProgress};
increase_finishprogress(Type,Args,Condition,OldFinishProgress)->
	?ERR("undefined Type:~w Args:~w Condition:~w ~n",[Type,Args,Condition]),
	{0,999}.
calculate_single_ger_destiny(GerTypeID,ArmedGerTypeIDList,GerEquipList)->
	case data_ger:get(GerTypeID) of
		?undefined->
			?ERR("undefined GerTypeID:~w ~n",[GerTypeID]),
			0;
		#data_ger{destinyIDList=DesIDList1}->
			%%#data_ger结构中的destinyIDList表示的已经不光是原始的精灵天命，有精灵装备羁绊的什么鬼哦，要过滤掉
			DesIDList = [DesID||DesID<-DesIDList1,not lists:member(DesID,data_headSeven:get(destiny_filter_list))],
			GerEquipTypeIDList = [EquipTypeID||#item{itemTypeID=EquipTypeID}<-GerEquipList], 
			lists:foldl(fun(DesID,AccNum)->
				#data_destiny{destinyType=DesType, destinyNeedList=DesNeedList} = data_destiny:get(DesID),
					case ger_attr:check_destiny_add(DesType, ArmedGerTypeIDList, GerEquipTypeIDList, DesNeedList) of
						false->
							AccNum;
						true->
							AccNum+1
					end
			end,0,DesIDList)
	end.

get_task_finish_progress(?LOGIN_REWARD,Condition)->
	1;
get_task_finish_progress(?N_GOLD_PURCHASE,Condition)->
	1;
get_task_finish_progress(?RECHARGE_ONCE_N,Condition)->
	element(2,Condition);
get_task_finish_progress(?RECHARGE_ACC_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?ROLE_LEVEL_N,Condition)->
	element(1,Condition);
get_task_finish_progress(?FORMATION_GER_STAR_N_QUALITY_N_ISMEGA_TN,Condition)->
	element(4,Condition);
get_task_finish_progress(?FORMATION_GER_LEVEL_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?BATTLE_FINISH,Condition)->
	element(2,Condition);
get_task_finish_progress(?FORMATION_GER_EQUIP_SUIT_X_TN,Condition)->
	element(2,Condition);
get_task_finish_progress(?FORMATION_GER_EQUIP_LEVEL_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?FORMATION_GER_DESTINY_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?BUIDING_X_LEVEL_N,Condition)->
	element(2,Condition);
get_task_finish_progress(?FORMATION_GER_EQUIP_STAR_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?FORMATION_GER_STAR_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?ACTIVATE_TALENT,Condition)->
	length(element(1,Condition));
get_task_finish_progress(?FORMATION_GER_EQUIP_RANK_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?GAIN_STONE_STAR_N_TN,Condition)->
	element(3,Condition);
get_task_finish_progress(?FORMATION_GER_STONE_TN,Condition)->
	element(2,Condition);
get_task_finish_progress(?FIGHTPOWER,Condition)->
	element(1,Condition);
get_task_finish_progress(?PVP_RANK_N,Condition)->
	element(1,Condition);
get_task_finish_progress(?CHALLENGE_KILL_BOSS_N,Condition)->
	element(1,Condition);
get_task_finish_progress(?CARLOS_WIN_N,Condition)->
	element(1,Condition);
get_task_finish_progress(?RELIC_DIFFICULTY_N_WIN_N,Condition)->
	element(2,Condition);
get_task_finish_progress(?HRON_REACH_N,Condition)->
	element(1,Condition);
get_task_finish_progress(?PLANTATION_HAS_N,Condition)->
	element(1,Condition);
get_task_finish_progress(?CONQUERISLAND_BOSS_DEMAGE_TN,Condition)->
	element(1,Condition);
get_task_finish_progress(?TWINS_DIFFICULTY_N_WIN_N,Condition)->
	element(2,Condition);
get_task_finish_progress(?DOUBLEMATCH_RANK_N,Condition)->
	element(2,Condition);
get_task_finish_progress(?PLANE_LEVEL_N,Condition)->
	element(2,Condition);
get_task_finish_progress(Type,Condition)->
	?ERR("undefined type:~w Condition:~w ~n",[Type,Condition]),
	0.
update_task_change_to_client(TaskList) when is_list(TaskList)->
	L = [transformtaskunit2ptaskunit(T)||T<-TaskList],
	?sendself(#sc_payGuide_seven_task_update{updateTask=L});
update_task_change_to_client(Task)->
	update_task_change_to_client([Task]).

update_period_summary()->
	L = summary_period_state(),
	?sendself(#sc_payGuide_seven_period_summary{periodState=L}).

%%检查精灵的装备是否某个套装
%%检测的时候只需要检查是否有满足套装列表中的任意一个就返回了，减少不必要的计算
%%故不能返回实际的满足的套装数量
check_ger_suit(GerID,NeedSuitIDList)->
	EquipList = role_data:get_equip(GerID),
	check_ger_suit2(EquipList,NeedSuitIDList,0).

check_ger_suit2(EquipList,[],SumNum)->
	SumNum;
check_ger_suit2(EquipList,NeedSuitIDList,SumNum) when SumNum >0->
	SumNum;
check_ger_suit2(EquipList,[H|T],SumNum)->
	case role_allequipment:check_equip_satify_suit(EquipList,H) of
		true->
			check_ger_suit2(EquipList,T,SumNum+1);
		false->
			check_ger_suit2(EquipList,T,SumNum)
	end.

trigger_formation_ger_change(GerID)->
	%%触发上阵精灵等级变化
	role_payGuide:trigger_task_change(?FORMATION_GER_LEVEL_TN,{GerID}),
	%%触发上阵精灵统计
	role_payGuide:trigger_task_change(?FORMATION_GER_STAR_N_QUALITY_N_ISMEGA_TN,{GerID}),
	%%触发精灵天命统计变化
	role_payGuide:trigger_task_change(?FORMATION_GER_DESTINY_TN,{GerID}),
	%%触发精灵星级统计变化
	role_payGuide:trigger_task_change(?FORMATION_GER_STAR_TN,{GerID}),
	%%增加一个对主线任务的触发
	role_maintask:do_main_task(?FORMATION_GER_LEVEL_N_TN,{GerID}),
	%%增加一个对精灵装备触发
	role_maintask:do_main_task(?FORMATION_GER_EQUIP_RANK_N_TN_TN,{GerID}).

trigger_formation_ger_equip_change(GerID)->
	%%触发精灵套装统计
	role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_SUIT_X_TN,{GerID}),
	%%触发精灵装备等级统计
	role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_LEVEL_TN,{GerID}),
	%%触发精灵装备星级统计
	role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_STAR_TN,{GerID}),
	%%触发精灵装备品阶统计
	role_payGuide:trigger_task_change(?FORMATION_GER_EQUIP_RANK_TN,{GerID}),
	%%触发精灵符文数量统计
	role_payGuide:trigger_task_change(?FORMATION_GER_STONE_TN,{GerID}),
	%%增加一个对主线任务的精灵装备统计触发
	role_maintask:do_main_task(?FORMATION_GER_EQUIP_RANK_N_TN_TN,{GerID}),
	role_maintask:do_main_task(?FORMATION_GER_EQUIP_LEVEL_N_TN_TN,{GerID}).

filter_stone_to_trigger(NewEquipInfoList)->
	% ?ERR("trigger stone EquipList:~w ~n",[NewEquipInfoList]),
	case role_data:get_headSeven() of
		?undefined->
			ignore;
		#head_seven{doingtask=DoingTask,finishtask=FinishTask,period=CPeriod}= HeadSeven->			
			case CPeriod > data_headSeven:get(dotask_max_period) orelse CPeriod=:=?OUT_PERIOD of
				true->
					ignore;
				_->
					StoneList = [E||#item{itemType=ItemType}=E<-NewEquipInfoList,item_lib:is_itemType_stone(ItemType) orelse item_lib:is_itemType_essence_stone(ItemType)],
					case StoneList of
						[]->
							ignore;
						_->
							% ?ERR("trigger_task_change~n"),
							trigger_task_change(?GAIN_STONE_STAR_N_TN,{0})
					end
			end
	end.

deal_headSeven_config(L)->
	deal_headSeven_config(L,[]).
deal_headSeven_config([],Acc)->
	Acc;
deal_headSeven_config([H|T],Acc)->
	case tuple_size(H) =:= 2 of
		false->
			deal_headSeven_config(T,[{element(2,H),H}|Acc]);
		true->
			deal_headSeven_config(T,[H|Acc])
	end.

%%针对玩家已经达到任务完成条件，然后没法再来完成任务的情况下，触发任务的完成
trigger_all_task_change()->
	%%触发玩家等级任务
	#role{level=RoleLevel,fightPower=FightPower} = role_data:get_roleInfo(),
	trigger_task_change(?ROLE_LEVEL_N,{RoleLevel}),
	%%触发上阵精灵相关统计
	trigger_formation_ger_change(0),
	%%触发上阵精灵装备相关统计
	trigger_formation_ger_equip_change(0),
	%%NEED FIX触发关卡完成
	trigger_task_change(?BATTLE_FINISH,{0}),
	%%触发建筑等级
	trigger_task_change(?BUIDING_X_LEVEL_N,{0}),
	%%触发天赋激活
	% [trigger_task_change(?ACTIVATE_TALENT,{Type,Level})||{Type,Level,Time}<-role_data:get_trainer_info()],
	trigger_task_change(?ACTIVATE_TALENT,{0}),
	%%触发符文获得
	trigger_task_change(?GAIN_STONE_STAR_N_TN,{0}),
	%%触发战斗力
	trigger_task_change(?FIGHTPOWER,{FightPower}),
	%%触发玩家无尽深渊关卡任务
	trigger_task_change(?HRON_REACH_N,{0}),
	%%触发家园土地数量变化
	trigger_task_change(?PLANTATION_HAS_N,{0}),
	%%触发飞机等级变化
	trigger_task_change(?PLANE_LEVEL_N,{0}),
	%%触发双排变化
	trigger_task_change(?DOUBLEMATCH_RANK_N,{0}).

%%触发可能在离线状态下被动变化的任务
trigger_all_task_offline_change()->
	trigger_task_change(?DOUBLEMATCH_RANK_N,{0}),
	trigger_task_change(?PVP_RANK_N,{0}).

comulat_stone(EquipList,NeedStar,IsEssenceStone)->
	EquipList1 = case IsEssenceStone of
		true->
			[E||E=#item{itemType=ItemType}<-EquipList,item_lib:is_itemType_essence_stone(ItemType)];
		false->
			[E||E=#item{itemType=ItemType}<-EquipList,item_lib:is_itemType_stone(ItemType)];
		ignore->
			[E||E=#item{itemType=ItemType}<-EquipList,item_lib:is_itemType_stone(ItemType) orelse item_lib:is_itemType_essence_stone(ItemType)]
	end,
	comulat_stone2(EquipList1,NeedStar).


%%-1的情况下表示没有星级要求
comulat_stone2(StoneList,-1)->
	StoneList;
comulat_stone2(StoneList,NeedStar)->
	lists:foldl(fun(#item{itemTypeID=ItemTypeID}=E,Acc)->
		#data_item{itemStar=Star} = data_item:get(ItemTypeID),
			case Star =:= NeedStar of
				false->
					Acc;
				true->
					[E|Acc]
			end
	end,[],StoneList).

summary_period_state()->
	#head_seven{finishtask=FinishTask,period=CPeriod}= HeadSeven = role_data:get_headSeven(),	
	case CPeriod of
		?OUT_PERIOD->
			[];
		_->
			lists:foldl(fun(#head_seven_task_unit{period=Period,state=State,type=Type,taskID=TaskID},Acc)->
				case Type of
					%%钻石兑换由于一直处于完成状态，故不对其进行红点统计
					?N_GOLD_PURCHASE->
						Acc;
					%%此处对累计充值活动特殊处理，由于后端累计充值设定成第一天的活动，但是前端显示要分开到不同天上，故此处需要将累计充值的领取状态分散到前端对应的period上
					?RECHARGE_ACC_TN->
						case data_headSeven:get({recharge_acc,TaskID}) of
							?undefined->
								case lists:keyfind(Period,#period_state.period,Acc) of
									false->
										[#period_state{period=Period,state=State}|Acc];
									_->
										Acc
								end;
							NewPeriod->
								case NewPeriod =< CPeriod of
									true->
										case lists:keyfind(NewPeriod,#period_state.period,Acc) of
											false->
												[#period_state{period=NewPeriod,state=State}|Acc];
											_->
												Acc
										end;
									false->
										Acc
								end
						end;
					_->
						case lists:keyfind(Period,#period_state.period,Acc) of
							false->
								[#period_state{period=Period,state=State}|Acc];
							_->
								Acc
						end
				end
			end,[],FinishTask)
	end.

deal_test_add_task({_,TaskID,Type})->
	#head_seven{finishtask=FinishTask,doingtask=DoingTask,period=CPeriod}=HS = role_data:get_headSeven(),
	case CPeriod of
		?OUT_PERIOD->
			ignore;
		_->
			case data_headSeven:get(TaskID) of
				?undefined->
					ignore;
				#headSevenTask{taskType=TaskType,taskCondition=Condition,taskPeriod=Period}->
					case Type of
						{doing,Progress}->
							case [E||E=#head_seven_task_unit{taskID=ExistTaskID}<-DoingTask,ExistTaskID=:=TaskID] of
								[]->
									FinishProgress = erlang:min(get_task_finish_progress(TaskType,Condition),Progress),
									AddTask = #head_seven_task_unit{taskID=TaskID,finishprogress=FinishProgress,type=TaskType,state=?TASK_UNFINISH,period=Period},
									NewHeadSeven = HS#head_seven{doingtask=[AddTask|DoingTask]},
									role_data:set_headSeven(NewHeadSeven),
									% update_task_change_to_client(AddTask),
									update_period_summary();
								_->
									ignore
							end;
						finish->
							case [E||E=#head_seven_task_unit{taskID=ExistTaskID}<-FinishTask,ExistTaskID=:=TaskID] of
								[]->
									FinishProgress = get_task_finish_progress(TaskType,Condition),
									AddTask = #head_seven_task_unit{taskID=TaskID,finishprogress=FinishProgress,type=TaskType,state=?TASK_FINISH,period=Period},
									NewHeadSeven = HS#head_seven{finishtask=[AddTask|FinishTask]},
									role_data:set_headSeven(NewHeadSeven),
									% update_task_change_to_client(AddTask),
									update_period_summary();
								_->
									ignore
							end
					end
			end
	end.

%%异步状态变化触发,在玩家进程外触发对应玩家的任务变化 （针对PVP排名变化，未上线的玩家对应的状态不会触发，只能在玩家上线时主动触发一次PVP排名变化）
asyn_trigger_task_change(RoleID,Type,Args)->
	% ?ERR("Type:~w ~n",[Type]),
	case lists:member(Type,?ALLOW_ASYNC_TYPE_LIST) of
		true->
			case role_lib:is_online(RoleID) of
				true->
					case catch role_lib:send_server(RoleID,{route,role_payGuide,{trigger_task_change_asyn,Type,Args}}) of
			  			{'EXIT',_}->
			    			?ERR("异步触发任务变化失败：roleID:~w Type:	~w Args:~w ~n",[RoleID,Type,Args]);
			  			_->
			    			ignore
					end;
				false->
					%%使用数据库触发任务完成
					HeadSeven = db_sql:get_role_headSeven(RoleID),
					case trigger_task_change2(HeadSeven,Type,Args) of
						ignore->
							ignore;
						{NewHeadSeven,_NewFinishTask,_UpdateTask}->
							db_sql:set_role_headSeven(RoleID,NewHeadSeven)
					end
			end;
		false->
			?ERR("not allow async type:~w RoleID:~w Args:~w ~n",[Type,RoleID,Args])
	end.	

compare_task_unit(A,B)->
	#head_seven_task_unit{taskID=TaskIDA,state=StateA} = A,
	#head_seven_task_unit{taskID=TaskIDB,state=StateB} = B,
	if
	 	StateA=:=?TASK_FINISH andalso StateB=/=?TASK_FINISH ->
			true;
		StateA=/=?TASK_FINISH andalso StateB=:=?TASK_FINISH ->
			false;
		true->
			TaskIDA < TaskIDB 
	end. 

change_role_level(RoleLevel)->
	TransmigrationLevel = data_common:get(max_role_level_base),
	case RoleLevel > TransmigrationLevel of
		true->
			RoleLevel-TransmigrationLevel;
		false->
			RoleLevel
	end.



%%=============================test fun===================================
test_clear_headSeven(RoleID)->
	Sql = io_lib:format("delete from gHeadSeven where roleID=~w;",[RoleID]),
	db_sql:sql_execute_with_log(Sql).

test_add_task(RoleID,TaskID,{doing,Progress})->
	case catch role_lib:send_server(RoleID,{route,role_payGuide,{deal_test_add_task,TaskID,{doing,Progress}}}) of
	  {'EXIT',_}->
	    ?INFO(" 修改headSeven doing roleID：~w TaskID:~w Progress:~w ~n",[RoleID,TaskID,Progress]);
	  _->
	    ok
	end;
test_add_task(RoleID,TaskID,finish)->
	case catch role_lib:send_server(RoleID,{route,role_payGuide,{deal_test_add_task,TaskID,finish}}) of
	  {'EXIT',_}->
	    ?INFO(" 修改headSeven finish roleID：~w TaskID:~w~n",[RoleID,TaskID]);
	  _->
	    ok
	end.

%%触发对当前所有直接获取玩家状态的任务进行重新计算
test_asyn_trigger_task_change(RoleID,Type,Args)->
	case not lists:member(Type,?ALLOW_ASYNC_TYPE_LIST) of
		true->
			case role_lib:is_online(RoleID) of
				true->
					case catch role_lib:send_server(RoleID,{route,role_payGuide,{trigger_task_change_asyn,Type,Args}}) of
			  			{'EXIT',_}->
			    			?ERR("异步触发任务变化失败：roleID:~w Type:	~w Args:~w ~n",[RoleID,Type,Args]);
			  			_->
			    			ignore
					end;
				false->
					%%使用数据库触发任务完成
					HeadSeven = db_sql:get_role_headSeven(RoleID),
					case trigger_task_change2(HeadSeven,Type,Args) of
						ignore->
							ignore;
						{NewHeadSeven,_NewFinishTask,_UpdateTask}->
							db_sql:set_role_headSeven(RoleID,NewHeadSeven)
					end
			end;
		false->
			?ERR("not allow async type:~w RoleID:~w Args:~w ~n",[Type,RoleID,Args])
	end.	

