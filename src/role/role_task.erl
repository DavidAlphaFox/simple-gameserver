%% @author crimoon26
%% @doc @todo Add description to role_task.


-module(role_task).
-include("def_reward.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-include("def_role.hrl").
-include("def_task.hrl").
-include("def_item.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================
send_dispach(RoleID,Msg) when is_integer(RoleID)->
	role_lib:send_server(RoleID, {dispach_task,Msg});
send_dispach(RoleServerPid, Msg) when is_pid(RoleServerPid)->
	RoleServerPid ! {dispach_task,Msg}.

cs_task_get_info(_)->
	{MainTaskList,TodayTaskList,AchTaskList,FamilyTodayTaskList,WorTaskList} = role_data:get_task_list_group(),
	NewAchTaskList1 = filter_ach_task_list(AchTaskList),
	NewMainTaskList = [r_task2p_task(E)||E<-MainTaskList],
	NewTodayTaskList = [r_task2p_task(E)||E<-TodayTaskList,E#r_task.status=/=?TASK_STATUS_COMMIT
                                                          ,E#r_task.trigger_id =/= ?TASK_TRIGGER_ID_GET_ACTIVITY
                                                          ,check_level(E#r_task.task_id)],
    NewActivityTaskList = [r_task2p_task(E)||E<-TodayTaskList,E#r_task.trigger_id =:= ?TASK_TRIGGER_ID_GET_ACTIVITY
                                                             ,check_level(E#r_task.task_id)],
    ?INFO("TodayTaskList:~w",[TodayTaskList]),
	NewAchTaskList = [r_task2p_task(E)||E<-NewAchTaskList1],
    %% ?INFO("L-cs_task_get_info NewTodayTaskList:~w",[FamilyTodayTaskList]),
    #role{familyID = FamilyID} = role_data:get_roleInfo(),
    if 
        FamilyID > 0 ->
            NewFamilyTodayTaskList = [r_task2p_task(E)||E<-FamilyTodayTaskList,E#r_task.status=/=?TASK_STATUS_COMMIT],
            %% 获得公会默认任务列表+挑战钻石兽奖励值
            {NewWorTaskList, FightReward} = lists:foldl(fun(#r_task{status=Status,trigger_id=TriggerID,trigger_notes=TriggerNotes}=E, {ListAcc,NumAcc}=Acc) ->
		                                                        case Status of
		                                                            ?TASK_STATUS_WAS_ACCEPT ->
		                                                                {[r_task2p_task(E)|ListAcc], NumAcc};
		                                                            ?TASK_STATUS_FINISH ->
		                                                                case TriggerID =:= ?TASK_TRIGGER_ID_FAMILY_WORSHIP_FIGHT of
		                                                                    true ->
		                                                                        case is_number(TriggerNotes) of
		                                                                            true ->
		                                                                                {[r_task2p_task(E)|ListAcc], NumAcc+TriggerNotes};
		                                                                            false ->
		                                                                                {[r_task2p_task(E)|ListAcc], NumAcc}
		                                                                        end;
		                                                                    false ->
		                                                                        {[r_task2p_task(E)|ListAcc], NumAcc}
		                                                                end;
		                                                            _ ->
		                                                                Acc
		                                                        end
                                                        end, {[],0}, WorTaskList);  
        true ->
            NewFamilyTodayTaskList = [],
            NewWorTaskList = [],
            FightReward = 0
    end,
    %一个特殊处理
    %任务要求，装备和精灵各用道具抽取一次，客户端设置的完成进度为1，服务器这边用位运算表示的，为了玩家看起来不奇怪
    %任务完成时，显示为1，未完成时显示为0
    %
    NewTodayTaskList2 = check_special_todaytask(NewTodayTaskList),
	?sendself(#sc_task_get_info{main_task_list=NewMainTaskList,today_task_list=NewTodayTaskList2,ach_task_list=NewAchTaskList
                               ,family_today_task_list=NewFamilyTodayTaskList,wor_task_list=NewWorTaskList
                               ,activity_task_list=NewActivityTaskList}),
    %% 发送钻石兽奖励值
    case FightReward of
        0 ->
            ignore;
        _ ->
            ?sendself(#sc_family_worship_refresh_fight_reward{reward=FightReward})
    end,

    %% 处理vip等级成就
    List = get_trigger_task_id(?TASK_TRIGGER_ID_VIP_LEVEL),
    #role{vipLevel=VipLevel} = role_data:get_roleInfo(),
    trigger_vip_level_task(VipLevel, List).

cs_task_operate(#cs_task_operate{operate_code=OperateCode,task_id=TaskID})->
	case check_operate_task(OperateCode,TaskID) of
		{ok,Task,TRoleTask}->
			case OperateCode of
				1->
					do_change_task_status(Task,TRoleTask);
				2->
					do_commit_task(Task,TRoleTask)
			end;
		{error,ErrorCode}->
			?sendself(#sc_task_error{result=ErrorCode})
	end.

%% 任务状态改变的处理函数,前面已经认定了任务状态改变,才会调用这个函数.任务状态包括从未完成到完成.
do_change_task_status(Task,TRoleTask)->
	Fun = fun()->
				  {NewTask,RewardView,Add} = change_task_status(Task,TRoleTask,1),
				  {NewTask,RewardView,Add}
		  end,
	case transaction(Fun) of
		{atomic,{NewTask,RewardView,Add}}->
			apply_success_fun(),
			put(?interval_persist_flag,true),
			#r_task{task_id=TaskID,status=Status} = NewTask,
			#data_task{task_type=TaskType} = data_task:get(TaskID),
			case RewardView of
				[]->
                    case TaskType of 
                        %% 公会膜拜任务在完成时始终需要通知客户端
                        ?TASK_TYPE_FAMILY_WORSHIP ->
					        ?sendself(#sc_task_operate{task_type=TaskType,task_id=TaskID,status=Status,reward=RewardView});
                        _ ->
					        ignore
                    end;
				_->
					?sendself(#sc_task_operate{task_type=TaskType,task_id=TaskID,status=Status,reward=RewardView})
			end,
			send_notify_change(TaskType,[NewTask|Add],[]);
%% 			?sendself(#sc_task_notify_change{task_type=TaskType,updata_task_list=[r_task2p_task(NewTask)],del_task_id=[]});
		{abort,Reason}->
			erase_success_fun(),
			?ERR("任务状态改变时发生错误:~w~n任务数据为:~w",[Reason,Task]),
			?sendself(#sc_task_error{result=5})
	end.

do_commit_task(Task,TRoleTask)->
	Fun = fun()->do_commit_task_1(Task,TRoleTask) end,
	case transaction(Fun) of
		{atomic,{TaskID,RewardView,AddTaskList,DeleteTaskIDList}}->
			apply_success_fun(),
			put(?interval_persist_flag,true),
			#data_task{task_type=TaskType} = data_task:get(TaskID),
			case RewardView of
				[]->
					ignore;
				_->
                    %%公会膜拜任务提交后,即被删除,不更新任务
                    %%因此这里返回的状态为已提交,界面通过这个状态显示奖励
                    #r_task{task_id=TaskID} = Task,
                    #data_task{task_type=TaskType} = data_task:get(TaskID),
                    Status = case TaskType of
                        ?TASK_TYPE_FAMILY_WORSHIP ->
                            ?TASK_STATUS_COMMIT;
                        _ ->
                            ?TASK_STATUS_FINISH
                    end,
					?sendself(#sc_task_operate{task_type=TaskType,task_id=TaskID,status=Status,reward=RewardView})
			end,
			role_task:send_notify_change(TaskType,AddTaskList,DeleteTaskIDList);
		{abort,Reason}->
			erase_success_fun(),
			?ERR("提交时发生错误:~w~n任务数据为:~w",[Reason,Task]),
			?sendself(#sc_task_error{result=5})
		end.

do_commit_task_1(Task,TRoleTask)->
	{TaskID,TaskType,DeleteTaskIDList,AddTaskList,RewardWithFamily} = commit_task(Task,TRoleTask),
	Role = role_data:get_roleInfo(),
	
	%% 此处需要把公会活跃值，从奖励列表中分离出来。
	Role2 = case lists:keytake(?REWARD_FAMILY_CONTRIBUTION, 1, RewardWithFamily) of
		{value, {_,AddFamilyContribution}, TupleList2}->
			Reward = TupleList2,
			FamilyID = Role#role.familyID,
			case FamilyID > 0 of
				true ->
					add_exp_to_family(FamilyID,Role,AddFamilyContribution);
				false ->
					Role
			end;
		false ->
			Reward = RewardWithFamily,
            Role
	end,

    #r_task{trigger_id=TriggerID, trigger_notes=TriggerNotes} = Task,

    %%对公会膜拜任务进行特殊处理
    case TaskType of 
        ?TASK_TYPE_FAMILY_WORSHIP ->
            case TriggerID of
                ?TASK_TRIGGER_ID_FAMILY_WORSHIP_FIGHT  ->
                %% 挑战钻石兽的奖励是动态计算的,放在trigger_notes里面的
                    case is_number(TriggerNotes) of
                        true ->
                            Reward1 = [{?REWARD_GOLD,TriggerNotes}];
                        _ ->
                            ?ERR("玩家:~p,挑战钻石兽奖品错误:~p.~n", [role_data:get_roleID(), TriggerNotes]), 
                            Reward1 = []
                    end;
		        ?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_GER ->
		        %扣除所需的精灵
		            #data_task{trigger_num=NeedNum,trigger_int_list=InitList} = data_task:get(TaskID),
		            case InitList of
		                [NeedStar, NeedRank] ->
		                    deduct_collect_ger_cost(NeedNum,NeedStar,NeedRank);
		                _ ->
		                    ignore
		            end,
		            Reward1 = Reward;
		        ?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_EQUIP ->
		        %% 扣除收集所需的装备
		            #data_task{trigger_num=NeedNum,trigger_int_list=InitList} = data_task:get(TaskID),
		            case InitList of
		                [NeedStar, NeedRank] ->
		                    deduct_collect_equip_cost(NeedNum,NeedStar,NeedRank);
		                _ ->
		                    ignore
		            end,
		            Reward1 = Reward;
                _ ->
                    Reward1 = Reward
            end,
            family_misc:router_to_family_process(Role2#role.familyID, {commit_family_worship_task, Role2#role.roleID});
        _ ->
            Reward1 = Reward
    end,
	
    %% 添加奖励信息
	RewardView =
		case Reward1 of
			[]->
				[];
			Any->
				LogType = ?MONEY_ADD_TYPE_TASK_COMMIT,
				RewardFun = fun()->
									role_reward:handle_sys_reward(Role2, Any, LogType, TaskID, "")
							end,
				add_success_fun(RewardFun),
				role_reward:transform2p_reward_view(Any, [])
		end,

	{TaskID,RewardView,AddTaskList,DeleteTaskIDList}.

commit_task(Task,TRoleTask)->
	#r_task{task_id=TaskID} = Task,
	#data_task{task_type=TaskType,trigger_task_id_list=TriggerTaskIDList,reward=Reward} = data_task:get(TaskID),
	AddTaskList = add_task(TriggerTaskIDList),
	{NewAddTaskList,DeleteTaskIDList,NewTRoleTask} =
		case TaskType of
			?TASK_TYPE_ACH->
				{[Task#r_task{status=?TASK_STATUS_COMMIT}|AddTaskList],[],TRoleTask};
            ?TASK_TYPE_TODAY->
                {[Task#r_task{status=?TASK_STATUS_COMMIT}|AddTaskList],[TaskID],TRoleTask};
			?TASK_TYPE_FAMILY_TODAY->
				{[Task#r_task{status=?TASK_STATUS_COMMIT}|AddTaskList],[TaskID],TRoleTask};
            ?TASK_TYPE_FAMILY_WORSHIP ->
                {AddTaskList,[TaskID],TRoleTask -- [Task]};
			_->
				{AddTaskList,[TaskID],TRoleTask}
		end,
	role_data:set_task_list(TaskType,NewAddTaskList++NewTRoleTask),
	{TaskID,TaskType,DeleteTaskIDList,NewAddTaskList,Reward}.

add_exp_to_family(FamilyID,Role,AddFamilyContribution)->
	family_misc:router_to_family_process(FamilyID, {add_family_exp,Role#role.roleID ,AddFamilyContribution}),
    role_lib:add_unioncoin_f(Role, AddFamilyContribution div 10, ?MONEY_ADD_TYPE_TASK_FAMILY_UNIONCOIN, 0, "t_u").

change_task_status(Task,TRoleTask,Step)->
	#r_task{task_id=TaskID,status=Status,trigger_id=TriggerID} = Task,
	#data_task{trigger_id=TriggerID,trigger_int_list=TriggerIntList,max_step=MaxStep,trigger_num=Num,step_reward=StepReward,task_type=TaskType,type=Type} = data_task:get(TaskID),
	%% 检查该任务是否完成
    NextStatus = get_status(erlang:min(Status+Step,?TASK_STATUS_FINISH),MaxStep),
	case NextStatus of
		?TASK_STATUS_FINISH->
			remove_trigger_task_id(TriggerID, TaskID),
			RewardView1 = [],
            %% 公会膜拜任务-挑战钻石兽通过trigger_notes来记录奖励,所以这里不能重置这个字段
            case TaskType of
                ?TASK_TYPE_FAMILY_WORSHIP ->
                        case TriggerID of
                            ?TASK_TRIGGER_ID_FAMILY_WORSHIP_FIGHT ->
			                    NewTask = Task#r_task{status=NextStatus,trigger_num=Num};
                            _ ->
			                    NewTask = Task#r_task{status=NextStatus,trigger_num=Num,trigger_notes=[]}
                        end,
					    Add=[];
                ?TASK_TYPE_ACH ->
			        NewTask = Task#r_task{status=NextStatus,trigger_num=Num,trigger_notes=[]},
					case role_data:get_ach_task_next(TaskID) of
						?undefined->
							Add = [];
						TID->
							{value,NextTask,_} = util:fun_take(fun(#r_task{task_id=TID1})->TID1=:=TID end, TRoleTask),
							NextTask = lists:keyfind(TID, #r_task.task_id, TRoleTask),
							role_data:set_ach_curr_taskID(Type, TID),
							case NextTask#r_task.status >= ?TASK_STATUS_FINISH of
								true->
									Add = [];
								false->
									Add = [NextTask]
							end
					end;
                _ ->
			        NewTask = Task#r_task{status=NextStatus,trigger_num=Num,trigger_notes=[]},
					Add=[]
            end;
		?TASK_STATUS_WAS_ACCEPT->
			Add = [],
			case role_task_trigger:get_trigger_num(TriggerID, Num, TriggerIntList) of
				finish->
					RewardView1 =add_step_reward_fun(TaskID,?TASK_STATUS_FINISH,StepReward),
					NewTask = Task#r_task{status=?TASK_STATUS_FINISH,trigger_num=Num,trigger_notes=[]};
				{finish,_TriggerNotes}->
					RewardView1 =add_step_reward_fun(TaskID,?TASK_STATUS_FINISH,StepReward),
					NewTask = Task#r_task{status=?TASK_STATUS_FINISH,trigger_num=Num,trigger_notes=[]};
				CurrNum when is_integer(CurrNum)->
					RewardView1 = [],
					NewTask = Task#r_task{status=NextStatus,trigger_num=CurrNum},
					add_trigger_task_id(TriggerID, TaskID);
				{CurrNum,TriggerNotes}->
					RewardView1 = [],
					NewTask = Task#r_task{status=NextStatus,trigger_num=CurrNum,trigger_notes=TriggerNotes},
					add_trigger_task_id(TriggerID, TaskID)
			end
	end,
	RewardView = add_step_reward_fun(TaskID, NextStatus, StepReward),
	role_data:set_task_list(TaskType,[NewTask|TRoleTask]),
	{NewTask,RewardView++RewardView1,Add}.

send_notify_change(TaskType,UpdateTaskList,DelIDList)->
    %% ?ERR("L-send_notify_change ~w ~w ~w",[TaskType,UpdateTaskList,DelIDList]),
	List = lists:foldl(fun(#r_task{task_id=TaskID,status=Status}=Task,Acc)->
						case is_notify_change(TaskID,Status) of
							true->
								[r_task2p_task(Task)|Acc];
							false->
								Acc
						end
				 end, [],UpdateTaskList),
    if
        List =:= [] andalso DelIDList =/= [] ->
            ?sendself(#sc_task_notify_change{task_type=TaskType,updata_task_list=[],del_task_id=DelIDList});
        List =/= [] ->            
            %下面的代码是这个特殊处理，需要把活跃度任务，单独以类型7发送给客户端，供客户端筛选
            [TaskInfo|_] = List,
            DelIDList2 = [DelIDL||DelIDL <-DelIDList, DelIDL < 70000 orelse DelIDL > 70050],%DelIDList -- [70001,70002,70003,70004],
            List2 = check_special_todaytask(List),
            {ActivityList,OtherList} = lists:foldr(fun(#p_task{task_id=LLTaskID}=T,{AccL1,AccL2})->
                                                        case LLTaskID > 70000 andalso LLTaskID < 70050 of
                                                            true ->
                                                                {[T|AccL1],AccL2};
                                                            false ->
                                                                {AccL1,[T|AccL2]}
                                                        end
                                                   end,{[],[]},List2),
            if
                OtherList =/= [] orelse DelIDList2 =/= [] ->
                    ?sendself(#sc_task_notify_change{task_type=TaskType,updata_task_list=OtherList,del_task_id=DelIDList2});
                true ->
                    ignore
            end,
            if
                ActivityList =/= [] ->
                    ?sendself(#sc_task_notify_change{task_type=7,updata_task_list=ActivityList,del_task_id=[]});
                true ->
                    ignore
            end;
        true ->
            ignore
    end.

check_special_todaytask(NewTodayTaskList)->
    lists:foldr(fun(Task,AccList) ->
                    TaskConfig = data_task:get(Task#p_task.task_id),
                     if
                        TaskConfig#data_task.trigger_id =:= ?TASK_TRIGGER_ID_GER_TIMES_BOTH ->
%%                          Task#p_task.task_id =:= 20008
%%                            orelse Task#p_task.task_id =:= 20392 ->
                             ?INFO("check_special_todaytask ~w",[Task]),
                             if
                                 Task#p_task.status =:= 3 ->
                                     [Task#p_task{trigger_num = 1}|AccList];
                                 true ->
                                     [Task#p_task{trigger_num = 0}|AccList]
                             end;
                         true ->
                             [Task|AccList]
                     end
                end, [], NewTodayTaskList).

add_step_reward_fun(TaskID,NextStatus,StepReward)->
	case lists:keyfind(NextStatus, 1, StepReward) of
			false->
				[];
			{_,Reward}->
				case Reward of
					[]->
						[];
					_->
						add_reward_fun(TaskID,Reward)
				end
		end.

add_step_fun_1(TaskID,TaskType,Status,StepReward)->
	case lists:keyfind(Status, 1, StepReward) of
			false->
				[];
			{_,Reward}->
				case Reward of
					[]->
						[];
					_->
						LogType = ?MONEY_ADD_TYPE_TASK_COMMIT,
						Role = role_data:get_roleInfo(),
						RewardView = role_reward:transform2p_reward_view(Reward, []),
						RewardFun = fun()->
											role_reward:handle_sys_reward(Role, Reward, LogType, TaskID, ""),
											?sendself(#sc_task_operate{task_type=TaskType,task_id=TaskID,status=Status,reward=RewardView})
									end,
						add_success_fun(RewardFun)
				end
		end.

add_reward_fun(TaskID,Reward)->
	case Reward of
		[]->
			[];
		_->
			LogType = ?MONEY_ADD_TYPE_TASK_COMMIT,
			Role = role_data:get_roleInfo(),
			RewardFun = fun()->
								role_reward:handle_sys_reward(Role, Reward, LogType, TaskID, "")
						end,
			add_success_fun(RewardFun),
			role_reward:transform2p_reward_view(Reward, [])
	end.

transaction(Fun)->
	case get(is_in_transaction) of
		?undefined->
			task_data_backup(),
			try
				Value = Fun(),
				{atomic,Value}
			catch
				_:Reason->
					task_data_recover(),
					{abort,Reason}
			after
					erase_backup_data()
			end;
		_->
			throw(was_in_transaction)
	end.

add_success_fun(Fun)->
	case get(apply_fun_list) of
		?undefined->
			put(apply_fun_list,[Fun]);
		L->
			put(apply_fun_list,[Fun|L])
	end.
erase_success_fun()->
	erase(apply_fun_list).

apply_success_fun()->
	case erase(apply_fun_list) of
		?undefined->
			ok;
		L->
			lists:foreach(fun(Fun)->
								  Fun()
						  end, lists:reverse(L))
	end.


r_task2p_task(#r_task{task_id=TaskID,status=Status,trigger_num=Num})->
	#p_task{task_id=TaskID,status=Status,trigger_num=Num}.

add_task(TaskID) when is_integer(TaskID)->
	add_task([TaskID]);
add_task([])->
	[];
add_task(TaskIDList) when is_list(TaskIDList)->
	add_task_1(TaskIDList,[]).

add_task_1([],Acc)->
	Acc;
add_task_1([TaskID|TailTaskID],Acc)->
	#data_task{task_type=TaskType,trigger_id=TriggerID,auto_accept=AutoAccept,max_step=MaxStep,step_reward=StepReward} = data_task:get(TaskID),
	NewTask = 
		case is_auto_accept(TaskType,AutoAccept) of
			false->
				Status = get_status(?TASK_STATUS_NOT_ACCEPT,MaxStep),
				#r_task{task_id=TaskID,status=Status,trigger_id=TriggerID,trigger_num=0};
			true->
				Status = get_status(?TASK_STATUS_WAS_ACCEPT,MaxStep),
				#r_task{task_id=TaskID,status=Status,trigger_id=TriggerID,trigger_num=0}
		end,
	case Status  of
		?TASK_STATUS_WAS_ACCEPT->
			add_trigger_task_id(TriggerID, TaskID);
		?TASK_STATUS_FINISH->
			add_step_fun_1(TaskID,TaskType,?TASK_STATUS_WAS_ACCEPT,StepReward);
		_->
			ignore
	end,
	add_step_fun_1(TaskID,TaskType,Status,StepReward),
	add_task_1(TailTaskID,[NewTask|Acc]).
%%配置了autoaccept的或非主线任务都自动接受
is_auto_accept(TaskType,AutoAccept)->
	case AutoAccept of
		true->
			true;
		false->
			TaskType=/=1
	end.
get_status(Status,MaxStep)->
	case Status>MaxStep of
		true->
			?TASK_STATUS_FINISH;
		false->
			Status
	end.
hook_zero_clock()->
    RoleInfo = role_data:get_roleInfo(),
    Level = RoleInfo#role.level,
    role_data:set_roleInfo(RoleInfo#role{tasklevel=Level}),
%%     role_shop:del_shop_num_by_shopID(?SHOP_ID_HONOR), %由daily_refresh_item_shop刷新
    role_shop:do_get_shopNumList(),
    % 每日0时清除活跃度
    clean_daily_activity(),
    OldTodayTaskList = role_data:get_task_list(?TASK_TYPE_TODAY),
	AllTodayTaskList = add_all_today_task(),
	role_data:set_task_list(?TASK_TYPE_TODAY,AllTodayTaskList),
    AllFamilyTodayTaskList = add_all_family_today_task(),
    role_data:set_task_list(?TASK_TYPE_FAMILY_TODAY,AllFamilyTodayTaskList),
    FamilyWorshipTaskList = role_data:get_task_list(?TASK_TYPE_FAMILY_WORSHIP),
    role_data:set_task_list(?TASK_TYPE_FAMILY_WORSHIP, []),
    ?INFO("hook_zero_clock ~w > ~w",[OldTodayTaskList,AllTodayTaskList]),
	send_notify_change(?TASK_TYPE_TODAY,AllTodayTaskList,[T1ID||#r_task{task_id=T1ID}<-OldTodayTaskList] -- [T2ID||#r_task{task_id=T2ID}<-AllTodayTaskList]),
    send_notify_change(?TASK_TYPE_FAMILY_TODAY,AllFamilyTodayTaskList,[]),
    send_notify_change(?TASK_TYPE_FAMILY_WORSHIP,[],[TaskID||#r_task{task_id=TaskID} <-FamilyWorshipTaskList]),
    %% 刷新钻石兽奖励值
    ?sendself(#sc_family_worship_refresh_fight_reward{reward=0}).
%% 	?sendself(#sc_task_notify_change{task_type=?TASK_TYPE_TODAY,updata_task_list=remove_not_show_task_add_2p_task(AllTodayTaskList),del_task_id=[]}).

init_task_data(RoleID,LastLogoutTime)->
	DBList = db_sql:get_task(RoleID),
	{RoleTaskList,TodayRoleTaskList,AchTaskList,FamilyTodayRoleTaskList,WorTaskList} =
		case DBList of
			[]->
				TodayTaskList = add_all_today_task(),
				AList = add_all_ach_task(),
                FamilyTodayTaskList = add_all_family_today_task(),
				case  [ID||ID<-data_task:get_list(),(data_task:get(ID))#data_task.task_type=:=?TASK_TYPE_MAIN] of
					[FirstTaskID|_]->
						AddTaskList = add_task(FirstTaskID),
						{AddTaskList,TodayTaskList,AList,FamilyTodayTaskList,[]};
					[]->
						{[],TodayTaskList,AList,FamilyTodayTaskList,[]}
				end;
			_->
				lists:foldl(fun(Task,{MAcc,TAcc,AList,Flist,MList})->
                                  case data_task:get(Task#r_task.task_id) of
                                      #data_task{task_type=TaskType} ->
        								  case TaskType of
        									  ?TASK_TYPE_TODAY->
        										  {MAcc,[Task|TAcc],AList,Flist,MList};
                                              ?TASK_TYPE_FAMILY_TODAY->
                                                  {MAcc,TAcc,AList,[Task|Flist],MList};
        									  ?TASK_TYPE_MAIN->
        										  {[Task|MAcc],TAcc,AList,Flist,MList};
        									  ?TASK_TYPE_ACH->
        										  {MAcc,TAcc,[Task|AList],Flist,MList};
                                              ?TASK_TYPE_FAMILY_WORSHIP ->
                                                  {MAcc,TAcc,AList,Flist,[Task|MList]}
        								  end;
                                      ?undefined->
                                          {MAcc,TAcc,AList,Flist,MList}
                                  end
						  end, {[],[],[],[],[]}, DBList)
		end,
    ?INFO("init_task_data"),
	NewTodayRoleTaskList = refresh_today_task(TodayRoleTaskList,LastLogoutTime),
    NewWorTaskList = refresh_worship_task(WorTaskList, LastLogoutTime),
    NewFamilyTodayRoleTaskList = refresh_family_today_task(FamilyTodayRoleTaskList,LastLogoutTime),
    NewTaskList = RoleTaskList++NewTodayRoleTaskList++AchTaskList++NewWorTaskList++NewFamilyTodayRoleTaskList,
	init_trigger_task(NewTaskList),
    %% 公会任务等待，公会进程发送数据同步
    role_data:set_task_list_group({RoleTaskList,NewTodayRoleTaskList,AchTaskList,NewFamilyTodayRoleTaskList,NewWorTaskList}),
    role_data:clear_refresh_task_flag( ),
    #role{familyID = FamilyID} = role_data:get_roleInfo(),
    if 
        FamilyID > 0 ->
            family_misc:router_to_family_process(FamilyID, {request_task, RoleID});
        true ->
            next
    end,
	ok.

%% 从所有未完成任务中筛选出,当前进行的任务,即需要激活检查的
init_trigger_task(TaskList)->
    %?INFO("TaskList:~w",[TaskList]),
	List = lists:foldl(fun(#r_task{trigger_id=TriggerID,task_id=TaskID,status=Status},Acc)->
						case Status of
							?TASK_STATUS_WAS_ACCEPT->
								case util:fun_take(fun({TID,_})->TriggerID=:=TID end,Acc) of
									false->
										[{TriggerID,[TaskID]}|Acc];
									{value,{_,L},TailAcc}->
										[{TriggerID,[TaskID|L]}|TailAcc]
								end;
							_->
								Acc
						end
				end, [], TaskList),
	init_trigger_task_1(List).

init_trigger_task_1([])->
	ignore;
init_trigger_task_1([{TriggerID,TaskIDList}|TailList])->
	case TriggerID of
		0->
			ignore;
		_->
            %?INFO("trigger_task_id_list opeartion init~w ~w",[TriggerID,TaskIDList]),
			put({?TRIGGER_TASK_ID_LIST,TriggerID},lists:sort(fun(X1,X2)->X1>X2 end, TaskIDList))
	end,
	init_trigger_task_1(TailList).

%%备份
task_data_backup()->
	put(role_task_data_backup,role_data:get_task_list_group()).
erase_backup_data()->
	erase(role_task_data_backup).
%%从备份恢复
task_data_recover()->
	case erase(role_task_data_backup) of
		?undefined->
			ingore;
		Tup->
			role_data:set_task_list_group(Tup),
			TaskList = role_data:get_task_list(),
			init_trigger_task(TaskList)
	end.


refresh_today_task(TodayRoleTaskList,_LastLogoutTime)->
    ?INFO("refresh_today_task"),
	case role_data:get_refresh_task_flag( ) of
			true->
                % 每日0时清除活跃度
                RoleInfo = role_data:get_roleInfo(),
                Level = RoleInfo#role.level,
                role_data:set_roleInfo(RoleInfo#role{tasklevel=Level}),
%%                 role_shop:del_shop_num_by_shopID(?SHOP_ID_HONOR),  %由daily_refresh_item_shop刷新
                role_shop:do_get_shopNumList(),
                clean_daily_activity(),
				add_all_today_task();
			false->
				lists:foreach(fun(#r_task{task_id=TaskID,trigger_id=TriggerID,status=Status})->
									  case Status of
											  ?TASK_STATUS_WAS_ACCEPT->
												   add_trigger_task_id(TriggerID, TaskID);
											  _->
												  ignore
										  end
							  end, TodayRoleTaskList),
				TodayRoleTaskList
	end.

refresh_family_today_task(TodayFamilyRoleTaskList,_LastLogoutTime)->
    case role_data:get_refresh_task_flag( ) of
            true->
                add_all_family_today_task();
            false->
%% 公会任务没有使用trigger List
%%                 lists:foreach(fun(#r_task{task_id=TaskID,trigger_id=TriggerID,status=Status})->
%%                                       case Status of
%%                                               ?TASK_STATUS_WAS_ACCEPT->
%%                                                    add_trigger_task_id(TriggerID, TaskID);
%%                                               _->
%%                                                   ignore
%%                                           end
%%                               end, TodayRoleTaskList),
                TodayFamilyRoleTaskList
    end.

add_all_today_task()->
    ?INFO("add_all_today_task"),
	TaskIDList = data_task:get_list(),
	TodayTaskIDList =lists:filter(fun(TaskID)->
						 (data_task:get(TaskID))#data_task.task_type=:=?TASK_TYPE_TODAY
                            andalso check_level(TaskID)
				 end, TaskIDList),
	add_task(TodayTaskIDList).

add_all_family_today_task()->
    TaskIDList = data_task:get_list(),
    FamilyTodayTaskIDList =lists:filter(fun(TaskID)->
                         (data_task:get(TaskID))#data_task.task_type=:=?TASK_TYPE_FAMILY_TODAY
                 end, TaskIDList),
    add_task(FamilyTodayTaskIDList).

add_all_ach_task()->
    TaskIDList = data_task:get_list(),
    TodayTaskIDList =lists:filter(fun(TaskID)->
                         (data_task:get(TaskID))#data_task.task_type=:=?TASK_TYPE_ACH
                 end, TaskIDList),
    add_task(TodayTaskIDList).

persist_task_data(RoleID)->
	RoleTaskList = role_data:get_task_list(),
	db_sql:set_task(RoleID, RoleTaskList).

check_operate_task(OperateCode,TaskID)->
	case check_operate_code(OperateCode) of
		true->
			case check_has_task(TaskID) of
				{error,ErrorCode}->
					{error,ErrorCode};
				{ok,Task,TRoleTask}->
					case check_task_operate_status(OperateCode, Task) of
						true->
%% 							case check_level(TaskID) of
%% 								true->
									{ok,Task,TRoleTask};
%% 								false->
%% 									{error,2}%%等级不足
%% 							end;
						false->
							{error,4}%%不合法操作
					end
			end;
		{error,ErrorCode}->
			{error,ErrorCode}
	end.

check_operate_code(OperateCode)->
	case OperateCode=:=1 orelse OperateCode=:=2 of
		true->
			true;
		false->
			{error,3}%%参数错误
	end.

check_level(TaskID)->
	#role{tasklevel=TaskLevel} = role_data:get_roleInfo(),
	#data_task{level_limit=MinTaskLevel,level_close=MaxTaskLevel} = data_task:get(TaskID),
	TaskLevel>=MinTaskLevel andalso TaskLevel<MaxTaskLevel.
	
check_has_task(TaskID)->
	case data_task:get(TaskID) of
		?undefined->
			{error,1};%%任务不存在
		#data_task{task_type=TaskType,type=Type}->
			RoleTaskList = role_data:get_task_list(TaskType),
			CheckFun = fun(#r_task{task_id=TID}) ->
							   TID =:= TaskID
					   end,
			case util:fun_take(CheckFun, RoleTaskList) of
				false->
					{error,1};
				{value,Task,TRoleTask}->
					case TaskType of
						?TASK_TYPE_ACH->
							case role_data:get_ach_curr_taskID(Type)=:=TaskID orelse Task#r_task.status=:=?TASK_STATUS_FINISH of
								true->
									{ok,Task,TRoleTask};
								false->
									{error,1}
							end;
						_->
							{ok,Task,TRoleTask}
					end
			end
	end.

check_task_operate_status(OperateCode,#r_task{task_id=TaskID,status=Status,trigger_id=TriggerID,trigger_num=TriggerNum})->
	case OperateCode of
		1->
			Status =:= ?TASK_STATUS_NOT_ACCEPT;
		2->
            %% 公会膜拜收集装备和精灵的状态不会发生改变,提交时根据触发数来判断是否能进行提交
            if TriggerID =:= ?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_GER orelse
               TriggerID =:= ?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_EQUIP ->
                    #data_task{trigger_num=NeedNum} = data_task:get(TaskID),
                    TriggerNum >= NeedNum;
               true ->
			        Status =:= ?TASK_STATUS_FINISH
            end
	end.

get_trigger_task_id(TriggerID)->
	case get({?TRIGGER_TASK_ID_LIST,TriggerID}) of
		?undefined->
			[];
		L->
			L
	end.

add_trigger_task_id(0,_)->
	ignore;
add_trigger_task_id(TriggerID,TaskID)->
	List = get_trigger_task_id(TriggerID),
    %?INFO("trigger_task_id_list opeartion add ~w",[List]),
	put({?TRIGGER_TASK_ID_LIST,TriggerID},lists:sort(fun(X1,X2)->X1>X2 end, [TaskID|lists:delete(TaskID,List)])).

remove_trigger_task_id(TriggerID,TaskID)->
	List = get_trigger_task_id(TriggerID),
    ?INFO("trigger_task_id_list opeartion remove ~w",[List]),
	case lists:delete(TaskID, List) of
		[]->
			erlang:erase({?TRIGGER_TASK_ID_LIST,TriggerID});
		L->
			put({?TRIGGER_TASK_ID_LIST,TriggerID},lists:sort(fun(X1,X2)->X1>X2 end, L))
	end.


is_notify_change(TaskID,Status)->
	#data_task{task_type=TaskType,type=Type} = data_task:get(TaskID),
	case TaskType of
		?TASK_TYPE_ACH->
			Status>=?TASK_STATUS_FINISH orelse role_data:get_ach_curr_taskID(Type) =:= TaskID;
		_->
			true
	end.

%%成就显示过滤
filter_ach_task_list(TaskList)->
	List = lists:foldl(fun(Task,Acc)->
						#r_task{task_id=TaskID} = Task,
						#data_task{type=Type} = data_task:get(TaskID),
						case util:fun_take(fun({T,_})->T=:=Type end, Acc) of
							false->
								[{Type,[Task]}|Acc];
							{value,{_,L},TailAcc1}->
								[{Type,[Task|L]}|TailAcc1]
						end
				end, [], TaskList),
	ShowTaskIDList = lists:map(fun({Type,L})->
						  NewL = lists:keysort(#r_task.task_id, L),
						  CurrTaskID = curr_task_id(NewL,0),
						  role_data:set_ach_curr_taskID(Type, CurrTaskID),
						  set_ach_task_relation(NewL),
						  CurrTaskID
				  end, List),
	lists:filter(fun(#r_task{task_id=TaskID,status=Status})->
						 Status>=?TASK_STATUS_FINISH orelse lists:member(TaskID, ShowTaskIDList)
				 end, TaskList).

curr_task_id([#r_task{task_id=TaskID,status=Status}|Tail],_)->
	case Status>= ?TASK_STATUS_FINISH of
		true->
			curr_task_id(Tail,TaskID);
		_->
			TaskID
	end;
curr_task_id([], TaskID)->
	TaskID.

set_ach_task_relation(List)->
	set_ach_task_relation(List,undefined).

set_ach_task_relation([],_)->
	ignore;
set_ach_task_relation([T|List],undefined)->
	set_ach_task_relation(List,T);
set_ach_task_relation([T|List],#r_task{task_id=TaskID})->
	#r_task{task_id=NextTaskID} = T,
	role_data:set_ach_task_next(TaskID, NextTaskID),
	set_ach_task_relation(List,T).

%刷新公会膜拜任务
refresh_worship_task(TaskList, _LastLogoutTime) ->
	case role_data:get_refresh_task_flag( ) of
        true ->
            [];
        false ->
            lists:foreach(fun(#r_task{task_id=TaskID, trigger_id=TriggerID, status = Status}) ->
                            case Status of
                                ?TASK_STATUS_WAS_ACCEPT ->
                                    add_trigger_task_id(TriggerID, TaskID);
                                _ ->
                                    ignore
                            end
                          end, TaskList),
            TaskList
    end.

%添加公会膜拜任务
add_family_worship_task(Type,NeedCost,TaskID) ->
	#data_task{task_type=TaskType,trigger_id=TriggerID,step_reward=StepReward} = data_task:get(TaskID),
    %% 1 请求新任务 2 刷新任务
    WorTaskList = role_data:get_task_list(?TASK_TYPE_FAMILY_WORSHIP),
    case Type of
        1 -> 
            ?CATCH(role_task_trigger:handle({dispach_task, join_worship_task})),
            NewWorTaskList = WorTaskList,
            ToDelList = [];
        2 ->
            ToDelList = lists:foldl(fun(#r_task{task_id=TaskIDT,trigger_id=TriggerIDT},Acc) ->
                                                remove_trigger_task_id(TriggerIDT, TaskIDT),
                                                [TaskIDT|Acc]
                                            end,[],WorTaskList),
            %role_lib:deduct_gold_f(role_data:get_roleInfo(),NeedCost,?MONEY_DEC_TYEP_WORSHIP_REFRESH_COST,0,""),
			role_lib:deduct_money_f(role_data:get_roleInfo(),unioncoin,NeedCost,?MONEY_DEC_TYEP_WORSHIP_REFRESH_COST,0,""),
            NewWorTaskList = []
    end,
    add_family_worship_task_add(TaskID,TaskType,TriggerID,StepReward,NewWorTaskList,ToDelList).

add_family_worship_task_add(TaskID,TaskType,TriggerID,StepReward,WorTaskList,ToDelList) ->
    case lists:keyfind(TaskID, #r_task.task_id, WorTaskList) of
        false ->
            %% 公会膜拜任务的trigger_notes默认改为0,防止在存盘时被过滤掉
		    case TriggerID of 
		        ?TASK_TRIGGER_ID_FAMILY_WORSHIP_AUTOFINISH ->
		            Status = ?TASK_STATUS_FINISH,
		            NewTask = #r_task{task_id=TaskID,status=Status,trigger_id=TriggerID,trigger_num=1,trigger_notes=0},
		            add_step_fun_1(TaskID,TaskType,?TASK_STATUS_WAS_ACCEPT,StepReward);
		        _ ->
		            Status = ?TASK_STATUS_WAS_ACCEPT,
		            NewTask = #r_task{task_id=TaskID,status=Status,trigger_id=TriggerID,trigger_num=0,trigger_notes=0},
		            add_trigger_task_id(TriggerID, TaskID)
		    end,
		    
		    add_step_fun_1(TaskID,TaskType,Status,StepReward),
            role_data:set_task_list(?TASK_TYPE_FAMILY_WORSHIP, [NewTask|WorTaskList]),
			role_task:send_notify_change(TaskType,[NewTask],ToDelList),
            
            % 接受收集精灵/装备任务时,手动触发一次初始化
            case TriggerID of 
                %2是收集精灵任务
                ?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_GER ->
                    ?CATCH(role_task_trigger:handle({dispach_task, role_family_collect_ger}));
                %3是收集装备任务 
                ?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_EQUIP ->
                     ?CATCH(role_task_trigger:handle({dispach_task, role_family_collect_equip}));
                _ ->
                     ignore
            end;
              
        _ ->
            ?ERR("错误:公会膜拜任务出现重复添加的情况!")
    end.

deduct_collect_ger_cost(NeedNum,NeedStar,NeedRank) ->
    RoleID = role_data:get_roleID(),
    HomesteadInfo = homestead_server:get_ets_homestead_base(RoleID),
    CurrGerID = HomesteadInfo#p_homestead.gerID, 
    {Date, _} = Time = erlang:localtime(),  
    {DelGers, NewGerBag} = deduct_collect_ger_cost(NeedNum,NeedStar,NeedRank,CurrGerID,lists:keysort(#gerSimple.gerLevel,role_data:get_gerBag()),{[],[]}),
	%% 写日志
	if
        DelGers == [] ->
            ignore;
        true ->
            LogGerList= [[GerID,GerSimple#gerSimple.gerTypeID,GerSimple#gerSimple.gerLevel,GerSimple#gerSimple.gerQuality]|| #gerSimple{gerID=GerID}=GerSimple<- DelGers],
            [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
            behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MOMEY_DEC_TYPE_WORSHIP_COLLECT_GER, 0, ""),
            role_data:set_gerBag(NewGerBag),
            DelGerIDList = [E||#gerSimple{gerID=E} <- DelGers],
            ?sendself(#sc_ger_del{gerIDList=DelGerIDList})
    end.

deduct_collect_ger_cost(NeedNum,_,_,_,[],Acc) when NeedNum > 0 ->
    ?ERR("钻石兽交换精灵时,扣除异常",[]),
    Acc;
deduct_collect_ger_cost(0,_,_,_,RestList,{Cost,Other}) ->
    {Cost, RestList ++ Other};
deduct_collect_ger_cost(NeedNum,NeedStar,NeedRank,MaskID,[#gerSimple{gerID=GerID,gerTypeID=GerTypeID,gerQuality=Quality}=H|T],{Cost,Other}) ->
    #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
    case GerStar =:= NeedStar andalso NeedRank =:= Quality andalso GerID =/= MaskID andalso not lists:member(GerTypeID, ?COLLECT_GER_MASK_TYPE_IDS) of
        true ->
            deduct_collect_ger_cost(NeedNum-1,NeedStar,NeedRank,MaskID,T,{[H|Cost],Other}); 
        false ->
            deduct_collect_ger_cost(NeedNum,NeedStar,NeedRank,MaskID,T,{Cost, [H|Other]}) 
    end.

deduct_collect_equip_cost(NeedNum,NeedStar,NeedRank) ->
    {Date,_} = Time = erlang:localtime(),
    {DelEquips,NewEquipBag} = deduct_collect_equip_cost(NeedNum,NeedStar,NeedRank,lists:keysort(#item.itemLevel,role_data:get_bagEquip()),{[],[]}),
	%% 写日志
    if 
        DelEquips == [] ->
            ignore;
		true ->
            LogEquipList = [[EquipUID,EquipTypeID,EquipNum,Quality]||#item{itemUID=EquipUID, itemTypeID=EquipTypeID, itemNum=EquipNum,itemRank=Quality} <- DelEquips],
			behavior_item_consume:log(role_data:get_roleID(), LogEquipList, Date, Time, ?MOMEY_DEC_TYPE_WORSHIP_COLLECT_EQUIP, 0, ""),
			role_data:set_bagEquip(NewEquipBag),
			DelItemIDList = [E||#item{itemUID=E}<-DelEquips],
			?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList})
	end.

deduct_collect_equip_cost(NeedNum,_,_,[],Acc) when NeedNum > 0 ->
    ?ERR("钻石兽交换精灵时,扣除异常",[]),
    Acc;
deduct_collect_equip_cost(0,_,_,RestList,{Cost,Other}) ->
    {Cost,RestList ++ Other};
deduct_collect_equip_cost(NeedNum,NeedStar,NeedRank,[#item{itemTypeID=ItemTypeID,itemRank=Quality}=H|T],{Cost,Other}) ->
    #data_item{itemType=ItemType, itemStar=ItemStar}=data_item:get(ItemTypeID),
    case lists:member(ItemType, [?weapon,?headwear,?armor,?wing,?runestone,?totem]) 
        andalso ItemStar =:= NeedStar andalso Quality =:= NeedRank of
        true ->
            deduct_collect_equip_cost(NeedNum-1,NeedStar,NeedRank,T,{[H|Cost],Other}); 
        false ->
            deduct_collect_equip_cost(NeedNum,NeedStar,NeedRank,T,{Cost, [H|Other]}) 
    end.

% 每日0时清除活跃度
clean_daily_activity()->
    ?INFO("clean_daily_activity"),
    BagItem = role_data:get_bagItem(),
    case lists:keytake(?ITEM_ID_ACTIVITY, #item.itemTypeID, BagItem) of
        false ->
            ignore;
        {value,Item,OtherBagItem}->
            role_data:set_bagItem(OtherBagItem),
            ?sendself(#sc_item_delete_notify{itemUIDList=[Item#item.itemUID]})            
    end.

% 处理vip等级成就
trigger_vip_level_task(_, []) ->
    ignore;
trigger_vip_level_task(VipLevel, [H|T]) ->
    #data_task{trigger_num=TriggerNum} = data_task:get(H),
    case TriggerNum >= VipLevel of
        true ->
            ?CATCH(role_task_trigger:handle({dispach_task, role_vip_level, VipLevel})),
            trigger_vip_level_task(VipLevel, T);
        _ ->
            ignore
    end.
