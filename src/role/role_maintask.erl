-module(role_maintask).
-compile(export_all).

-include("def_role.hrl").
-include("def_item.hrl").
-include("def_fight.hrl").
-include("def_doublematch.hrl").

-define(MAIN_TASK_UNFINISH,0).
-define(MAIN_TASK_FINISH,1).
-define(MAIN_TASK_FINISHREWARD,2).
-define(MAIN_TASK_TYPE_LIST,[?ROLE_LEVEL_N,?BATTLE_FINISH,?FORMATION_GER_STAR_N_QUALITY_N_ISMEGA_TN,?FIGHTPOWER,?BUIDING_X_LEVEL_N,?FORMATION_GER_LEVEL_N_TN,?FORMATION_GER_EQUIP_RANK_N_TN_TN,?FORMATION_GER_EQUIP_LEVEL_N_TN_TN]).

%%此处会再次检测上次的最后那个任务时候有后续任务给出
set_maintask_persist({MainTaskID,Finish,State})->
	set_maintask(generate_next_maintask(#main_task_info{maintaskID=MainTaskID,state=State,finish=Finish}));
set_maintask_persist(R)->
	?ERR("undefined:~w R~n",[R]),
	FirstMainTaskID = data_maintask:get(first_main_task),
	set_maintask(#main_task_info{maintaskID=FirstMainTaskID}).

get_maintask_persist()->
	#main_task_info{maintaskID=MainTaskID,state=State,finish=Finish} = get_maintask(),
	{MainTaskID,Finish,State}.

set_maintask(MainTask)->
	put(?maintask,MainTask).
	
get_maintask()->
	get(?maintask).

%%数据初始化完成之后将会调用这个函数来触发对主线任务的状态检测
check_maintask_and_trigger()->
	#main_task_info{maintaskID=MainTaskID} = get_maintask(),
	do_trigger_main_task_type(MainTaskID).

do_main_task(Type,Args)->
	MainTaskInfo = get_maintask(),
	do_main_task2(MainTaskInfo,Type,Args).

do_main_task2(?undefined,Type,Args)->
	ignore;
do_main_task2(#main_task_info{state=?MAIN_TASK_FINISH},Type,Args)->
	ignore;
do_main_task2(#main_task_info{state=?MAIN_TASK_FINISHREWARD},Type,Args)->
	ignore;
do_main_task2(#main_task_info{maintaskID=MainTaskID}=MainTaskInfo,Type,Args)->
	case lists:member(Type,?MAIN_TASK_TYPE_LIST) of
		false->
			ignore;
		true->
			case data_maintask:get(MainTaskID) of
				?undefined->
					?ERR("undefined MainTaskID:~w ~n",[MainTaskID]),
					ignore;
				#maintask{type=Type,condition=Condition}->
					case do_main_task3(MainTaskInfo,Type,Args,Condition) of
						{finish,NewMainTask}->
							update_maintask(NewMainTask);
						{update,NewMainTask}->
							update_maintask(NewMainTask);
						{doing,_NewMainTask}->
							ignore
					end;
				_->
					ignore
			end
	end.

%%更新任务不推送任务
do_main_task3(#main_task_info{maintaskID=MainTaskID,finish=OldFinishProgress}=MainTask,Type,Args,Condition)->
	{NewFinishProgress,FinishProgress} = increase_finishprogress(Type,Args,Condition,OldFinishProgress),
	case NewFinishProgress >= FinishProgress of
		true->
			NewMainTask = MainTask#main_task_info{finish=abs(FinishProgress),state=?MAIN_TASK_FINISH},
			set_maintask(NewMainTask),
			{finish,NewMainTask};
		false->
			NewMainTask = MainTask#main_task_info{finish=abs(NewFinishProgress)},
			set_maintask(NewMainTask),
			case abs(NewFinishProgress)=:=abs(OldFinishProgress) of
				true->
					%%进度一致，不更新
					{doing,NewMainTask};
				false->
					%%更新
					{update,NewMainTask}
			end
	end.


cs_maintask_info(#cs_maintask_info{})->
	M = get_maintask(),
	?sendself(#sc_maintask_info{maintask=to_p_task_unit(M)}).

cs_maintask_draw(#cs_maintask_draw{taskID=TaskID})->
	#main_task_info{maintaskID=MainTaskID,state=State}=M = get_maintask(),
		case check_maintask_draw(M,TaskID) of
			{false,R}->
				?sendself(#sc_maintask_draw{result=R});
			{true,Reward}->
				Role = role_data:get_roleInfo(),
				%%发放对应的奖励
				role_reward:handle_sys_reward_with_return(Role,Reward,?MONEY_ADD_TYPE_MAINTASK,TaskID,"",true),
				%%更新HeadSeven
				NewMainTask = generate_next_maintask(M#main_task_info{state=?MAIN_TASK_FINISHREWARD}),
				set_maintask(NewMainTask),
				case do_trigger_main_task_type(NewMainTask#main_task_info.maintaskID) of
					ignore->
						%%没有推送过
						update_maintask(get_maintask());
					_->
						ignore
				end,
				PRewardInfo = role_reward:transform2p_reward_view(Reward,[]),
				?sendself(#sc_maintask_draw{result=1,reward=PRewardInfo})
	end.

check_maintask_draw(#main_task_info{maintaskID=MainTaskID,state=State},TarTaskID)->
	case MainTaskID=:=TarTaskID of
		false->
			{false,2};
		true->
			if
				State=:=?MAIN_TASK_UNFINISH ->
					{false,3};
				State=:=?MAIN_TASK_FINISHREWARD->
				    {false,4};
				true->
					#maintask{reward=R} = data_maintask:get(MainTaskID),
					{true,R}
			end
	end.

generate_next_maintask(#main_task_info{maintaskID=MainTaskID,state=?MAIN_TASK_FINISHREWARD}=M)->
	case data_maintask:get(MainTaskID) of
		?undefined->
			?ERR("undefined M:~w ~n",[M]),
			M;
		#maintask{nexttaskID=NextMainTaskID}->
			case data_maintask:get(NextMainTaskID) of
				?undefined->
					M;
				_->
					#main_task_info{maintaskID=NextMainTaskID}
			end
	end;
generate_next_maintask(M)->
	M.

to_p_task_unit(#main_task_info{maintaskID=MainTaskID,finish=Finish,state=State})->
	case data_maintask:get(MainTaskID) of
		#maintask{type=?ROLE_LEVEL_N}->
			#p_task_unit{taskID=MainTaskID,taskFinish=role_payGuide:change_role_level(Finish),taskState=State};
		_->
			#p_task_unit{taskID=MainTaskID,taskFinish=Finish,taskState=State}
	end.			


increase_finishprogress(?FORMATION_GER_LEVEL_N_TN,Args,Condition,OldFinishProgress)->
	NeedLevel = element(1,Condition),
	FinishProgress = element(2,Condition),
	PosList = role_data:get_posList(),
	SumNum = lists:foldl(fun(#ger{gerBase=#gerBase{gerLevel=GerLevel}},AccNum)->
		case GerLevel >= NeedLevel of
			true->
				AccNum+1;
			false->
				AccNum
		end
	end,0,PosList),
	{SumNum,FinishProgress};
increase_finishprogress(?FORMATION_GER_EQUIP_RANK_N_TN_TN,Args,Condition,OldFinishProgress)->
	NeedRank = element(1,Condition),
	NeedEquipNum = element(2,Condition),
	FinishProgress = element(3,Condition),
	PosList = role_data:get_posList(),
	SumNum = lists:foldl(fun(#ger{gerID=GerID},AccNum)->
			GerEquipList = role_data:get_equip(GerID),
			GerNormalEquipRankList = [Rank||#item{itemRank=Rank,itemType=ItemType}<-GerEquipList,lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE) andalso Rank>=NeedRank],
			case length(GerNormalEquipRankList) >= NeedEquipNum of
				true->
					AccNum+1;
				false->
					AccNum
			end
		end,0,PosList),
	{SumNum,FinishProgress};
increase_finishprogress(?FORMATION_GER_EQUIP_LEVEL_N_TN_TN,Args,Condition,OldFinishProgress)->
	NeedLevel = element(1,Condition),
	NeedEquipNum = element(2,Condition),
	FinishProgress = element(3,Condition),
	PosList = role_data:get_posList(),
	SumNum = lists:foldl(fun(#ger{gerID=GerID},AccNum)->
			GerEquipList = role_data:get_equip(GerID),
			GerNormalEquipRankList = [Level||#item{itemLevel=Level,itemType=ItemType}<-GerEquipList,lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE) andalso Level>=NeedLevel],
			case length(GerNormalEquipRankList) >= NeedEquipNum of
				true->
					AccNum+1;
				false->
					AccNum
			end
		end,0,PosList),
	{SumNum,FinishProgress};
increase_finishprogress(Type,Args,Condition,OldFinishProgress)->
	role_payGuide:increase_finishprogress(Type,Args,Condition,OldFinishProgress).

do_all_main_task()->
	lists:foreach(fun(Type)->
			case Type of
				?ROLE_LEVEL_N->
					#role{roleID=RID} = role_data:get_roleInfo(),
					#rolePublic{level=RoleLevel} = role_lib:get_rolePublic(RID),
					do_main_task(?ROLE_LEVEL_N,{RoleLevel});
				?BATTLE_FINISH->
					#role_xbattle{chapterID=ChapterIDNow}=Xbattle=role_data:get_xbattle_data(),
					do_main_task(?BATTLE_FINISH,{ChapterIDNow});
				_->
					do_main_task(Type,{0})
			end
	end,?MAIN_TASK_TYPE_LIST).


%%针对具体的任务类型触发对应的任务类型判断
do_trigger_main_task_type(MainTaskID)->
	case data_maintask:get(MainTaskID) of
		?undefined->
			?ERR("undefined MainTaskID:~w ~n",[MainTaskID]);
		#maintask{type=Type}->
			case Type of
				?ROLE_LEVEL_N->
					#role{roleID=RID} = role_data:get_roleInfo(),
					#rolePublic{level=RoleLevel} = role_lib:get_rolePublic(RID),
					do_main_task(?ROLE_LEVEL_N,{RoleLevel});
				?BATTLE_FINISH->
					#role_xbattle{chapterID=ChapterIDNow}=Xbattle=role_data:get_xbattle_data(),
					#xbattle_chapter{isGetReward=IsGetReward}=role_xbattle:get_chapter(ChapterIDNow),
					case data_xbattle_chapter:get(ChapterIDNow) of
						?undefined->
							%%判定当前章节未完成，倒退一个章节
							do_main_task(?BATTLE_FINISH,{ChapterIDNow-1});
						#data_xbattle_chapter{}->
							case IsGetReward > 1 of  %% 0 not active 1 not fight win 2 not get reward 3 all pass
					    		true->
					        		%%触发新战役章节完成任务
					        		do_main_task(?BATTLE_FINISH,{ChapterIDNow});
					    		false->
					    			%%判定当前章节未完成，倒退一个章节
					        		do_main_task(?BATTLE_FINISH,{ChapterIDNow-1})
							end
					end;
				?FIGHTPOWER->
					#role{fightPower=FightPower} = role_data:get_roleInfo(),
					do_main_task(?FIGHTPOWER,{FightPower});
				_->
					do_main_task(Type,{0})
			end
	end.

update_maintask(M)->
	?sendself(#sc_maintask_info{maintask=to_p_task_unit(M)}).