%% @author crimoon26
%% @doc @todo Add description to role_task_trigger.


-module(role_task_trigger).

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

handle({dispach_task,role_up_level,RoleID,Level})->
    do_src_levelup_record(RoleID,Level),
	?CATCH(dispach_task(?TASK_TRIGGER_ID_ROLE_UP_LEVLE,Level));
handle({dispach_task,ger_up_level,_GerID,GerTypeID,Level})->
	dispach_task(?TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES,GerTypeID),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES,1),
	dispach_task(?TASK_TRIGGER_ID_GER_UP_LEVEL,Level);
handle({dispach_task,dungeon_pass,_RoleID,DungeonID,Type,Times})->
	case Type of
		?BATTLE_DUNGEON_TYPE_NORMAL->
			dispach_task(?TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS,{DungeonID,Times}),
            family_dispach_task(?FAMILY_TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS,Times);
		?BATTLE_DUNGEON_TYPE_HARD->
			dispach_task(?TASK_TRIGGER_ID_DUNGEON_HARD_PASS,{DungeonID,Times});
		?BATTLE_DUNGEON_TYPE_FAST_HARD->
			dispach_task(?TASK_TRIGGER_ID_DUNGEON_FAST_HARD_PASS,{DungeonID,Times});
		_->
			ignore
	end,
	dispach_task(?TASK_TRIGGER_ID_DUNGEON_PASS,{Type,Times});
handle({dispach_task,chapter_pass,_RoleID,ChapterID,Type}) ->
	case Type of ?BATTLE_DUNGEON_TYPE_NORMAL -> dispach_task(?TASK_TRIGGER_ID_CHAPTER_NORMAL_PASS,{ChapterID,1});
		_ -> ignore
	end;
handle({dispach_task,kill_monster,[]})->
	ignore;
handle({dispach_task,kill_monster,KillMonsterList})->
	dispach_task(?TASK_TRIGGER_ID_KILL_MONSTER,KillMonsterList);
handle({dispach_task,equip_strong,_RoleID,EquipUID,EquipType,Level,Num})->
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG,{EquipType,Level}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG_TIMES,{Num,EquipType}),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_EQUIP_STRONG_TIMES,Num),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG_1,{EquipUID,Level});
handle({dispach_task,equip_up_quality,_RoleID,EquipUID,EquipTypeID,Quality})->
    #data_item{itemStar=ItemStar}=data_item:get(EquipTypeID),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY,{EquipTypeID,Quality}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_TIMES,EquipTypeID),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1,{EquipUID,Quality}),
	if Quality == 10 ->
		    #data_item{itemStar=ItemStar}=data_item:get(EquipTypeID),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_3,{EquipUID,ItemStar});   
	   true -> ignore
	end,
    dispach_task(?TASK_TRIGGER_ID_REFIE_EQUIP,ItemStar),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_REFIE_EQUIP,1);
handle({dispach_task,pft_equip_up_quality,_RoleID,EquipUID,EquipTypeID,Quality})->
	if Quality > 10 ->
    #data_item{itemStar=ItemStar}=data_item:get(EquipTypeID),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_2,{EquipUID,ItemStar})
	end;
handle({dispach_task,ger_up_quality,GerUID,GerTypeID,Quality,Type})->
	dispach_task(?TASK_TRIGGER_ID_GER_UP_QUALITY_TIMES,GerTypeID),
	dispach_task(?TASK_TRIGGER_ID_GER_UP_QUALITY,{GerUID,Quality}),
    #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
    case Type of
        evolution ->
			dispach_task(?TASK_TRIGGER_ID_EVOLUTION_PET, GerStar),
			family_dispach_task(?FAMILY_TASK_TRIGGER_ID_EVOLUTION_PET, 1); 
		transform1 ->
			dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL1,GerStar);
		transform2 ->
			dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL2,GerStar);
		transform3 ->
			dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL3,GerStar);
		transform4 ->
			dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL4,GerStar);
        _ ->
            ignore
    end;
handle({dispach_task,equip_up_equip,EquipUID,EquipType})->
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_EQUIP,{EquipUID,EquipType});
handle({dispach_task,gain_ger,GerUID,GerTypeID,Star,Quality,Level})->
	case lists:member(GerTypeID, [2,3,4,5,6,7]) of
		true->
			ignore;
		false->
			dispach_task(?TASK_TRIGGER_ID_GER_UP_LEVEL,Level),
			dispach_task(?TASK_TRIGGER_ID_GER_UP_QUALITY,{GerUID,Quality}),
			dispach_task(?TASK_TRIGGER_ID_GER_GAIN_GER,{GerUID,GerTypeID}),
			dispach_task(?TASK_TRIGGER_ID_GER_GAIN_STAR_GER,{GerUID,Star})
	end;
handle({dispach_task,create_equip,EquipUID,EquipType,Star,Quality,Level})->%%创建物品的时候可能很多操作
	dispach_task(?TASK_TRIGGER_ID_EQUIP_TOTAL_EQUIP,{EquipUID,EquipType}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP,{EquipUID,Star}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY,{EquipUID,Quality}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1,{EquipUID,Quality}),
	if  Quality == 10 -> dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_3,{EquipUID,Star});
		Quality > 10 ->dispach_task(?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_2,{EquipUID,Star});
	   true -> ignore
	end,
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG_1,{EquipUID,Level}),
	dispach_task(?TASK_TRIGGER_ID_EQUIP_STRONG,{EquipUID,Level});
handle({dispach_task,role_explore,N})->
	dispach_task(?TASK_TRIGGER_ID_EXPLORE_TIMES, N),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_EXPLORE_TIMES, N);
handle({dispach_task, role_use_gold, N}) ->
	dispach_task(?TASK_TRIGGER_ID_USE_GOLD,N);
handle({dispach_task,role_combime,Type})->
    handle({dispach_task,role_combime,Type,1});

handle({dispach_task,role_combime,Type,Num})->
	dispach_task(?TASK_TRIGGER_ID_COMBINE_TIMES, Num),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_COMBINE_TIMES, Num),
	case Type of
		random_combine_equip->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES, Num);
		normal_combine_equip->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES, undefined);
		random_combine_ger->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES, Num),
			dispach_task(?TASK_TRIGGER_ID_COMBINE_GER_RANDOM_TIMES, Num);
		normal_combine_ger->
			dispach_task(?TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES, undefined)
	end;
handle({dispach_task,role_add_reward,Num,Type})->
    case Type of
        1 ->
            dispach_task(?TASK_TRIGGER_ID_ADD_REWARD,{Num,Type});
        2 ->
            dispach_task(?TASK_TRIGGER_ID_ADD_REWARD_GOLD,{Num,Type});
        3 ->
            dispach_task(?TASK_TRIGGER_ID_ADD_REWARD_REPU,{Num,Type})
    end;
handle({dispach_task,role_pay_gold,PayGold})->
	dispach_task(?TASK_TRIGGER_ID_PAY_GOLD, 1);
handle({dispach_task,role_add_friend,FriendRoleID,IsDeffSex})->
	dispach_task(?TASK_TRIGGER_ID_ADD_FRIEND_NUM,FriendRoleID),
	case IsDeffSex of
		true->
			dispach_task(?TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND,FriendRoleID);
		false->
			ignore
	end;
handle({dispach_task,role_extract_card,Times})->
	dispach_task(?TASK_TRIGGER_ID_EXTRACT_CARD_TIMES,Times),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_EXTRACT_CARD_TIMES,Times);
handle({dispach_task,role_extract_card,Times,TaskIDList})->
	disapach_task(?TASK_TRIGGER_ID_EXTRACT_CARD_TIMES,Times,TaskIDList);
handle({dispach_task,role_join_hron})->
	dispach_task(?TASK_TRIGGER_ID_JOIN_HRON_TIMES,1),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_JOIN_HRON_TIMES,1);
handle({dispach_task,role_join_hula})->
	dispach_task(?TASK_TRIGGER_ID_JOIN_HULA_TIMES,1),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_JOIN_HULA_TIMES,1);
handle({dispach_task,role_join_nanm})->
	dispach_task(?TASK_TRIGGER_ID_JOIN_NANM_TIMES,1),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_JOIN_NANM_TIMES,1);
handle({dispach_task,role_add_enargy_to_friend})->
	dispach_task(?TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES,1),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES,1);
handle({dispach_task,role_mating_to_friend})->
	dispach_task(?TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES,1),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES,1);
handle({dispach_task,role_fight_power,FightPower})->
	dispach_task(?TASK_TRIGGER_ID_ROLE_FIGHT_POWER,FightPower);
handle({dispach_task,role_pvp_fight,Rank})->
	case Rank of
		0->
			ignore;
		_->
			dispach_task(?TASK_TRIGGER_ID_ROLE_PVP_RANK,Rank)
	end,
	dispach_task(?TASK_TRIGGER_ID_PVP_FIGHT_TIMES,1),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_PVP_FIGHT_TIMES,1);
handle({dispach_task,role_pass_hron,N})->
	dispach_task(?TASK_TRIGGER_ID_PASS_HRON_DUNGEON,N);
handle({dispach_task,role_allequipment,N})->
	dispach_task(?TASK_TRIGGER_ID_ALLEQUIPMENT,N);
handle({dispach_task,role_up_ger_num,N})->
	dispach_task(?TASK_TRIGGER_ID_UP_GER_NUM,N);
handle({dispach_task,role_ger_move_pos_times})->
	dispach_task(?TASK_TRIGGER_ID_REPLACE_UP_GER_TIMES,1);
handle({dispach_task,role_chat_times})->
	dispach_task(?TASK_TRIGGER_ID_CHAT_TIMES,1);
handle({dispach_task,role_sign_race_times})->
	dispach_task(?TASK_TRIGGER_ID_SIGN_RACE_TIMES,1);
handle({dispach_task,role_change_head_times})->
	dispach_task(?TASK_TRIGGER_ID_CHANGE_HEAD_TIMES,1);
handle({dispach_task,role_buy_coin_times,Times})->
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_FAMILY_BUY_COIN, Times),
	dispach_task(?TASK_TRIGGER_ID_BUY_COIN_TIMES,Times);
handle({dispach_task,role_active_destiny_num,Num})->
	dispach_task(?TASK_TRIGGER_ID_ACTIVE_DESTINY_NUM,Num);
handle({dispach_task,role_finished_3v3,Inc}) ->
	dispach_task(?TASK_TRIGGER_ID_FINISHED_3V3,Inc);
handle({dispach_task,role_3v3_rank_change,Rank}) ->
	dispach_task(?TASK_TRIGGER_ID_3V3_RANK_CHANGE,Rank);
handle({dispach_task,role_sign_alien,Inc}) ->
    dispach_task(?TASK_TRIGGER_ID_SIGN_ALIEN,Inc);
handle({dispach_task,role_bet_alien,Inc}) ->
    dispach_task(?TASK_TRIGGER_ID_BET_ALIEN,Inc);
handle({dispach_task,role_alien_figth_res, RoleKillContinuousNum}) ->
	dispach_task(?TASK_TRIGGER_ID_KILLED_ALIEN, RoleKillContinuousNum);
handle({dispach_task,role_box_ger_10}) ->
    dispach_task(?TASK_TRIGGER_ID_BOX_GER_10,1);
handle({dispach_task,role_box_equip_10})->
    dispach_task(?TASK_TRIGGER_ID_BOX_EQUIP_10,1);
handle({dispach_task,role_box_trainer_10})->
    dispach_task(?TASK_TRIGGER_ID_BOX_EQUIP_10,1);
handle({dispach_task,role_gen_match}) ->
    dispach_task(?TASK_TRIGGER_ID_GEN_MATCH,1);
handle({dispach_task,role_hula_rank,Rank}) ->
    dispach_task(?TASK_TRIGGER_ID_DMG_RANK_HULA,Rank);
handle({dispach_task,role_nanm_rank,Rank}) ->
    dispach_task(?TASK_TRIGGER_ID_DMG_RANK_NANM,Rank);
handle({dispach_task,role_up_lieu_ger_num,Num}) ->
    dispach_task(?TASK_TRIGGER_ID_AMIGO_BATTLE,Num);
handle({dispach_task, role_family_worship_fight, Reward}) ->
    dispach_task(?TASK_TRIGGER_ID_FAMILY_WORSHIP_FIGHT,Reward);
handle({dispach_task, role_family_collect_ger}) ->
    dispach_task(?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_GER,1);
handle({dispach_task, role_family_collect_equip}) ->
    dispach_task(?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_EQUIP,1);
handle({dispach_task, plunder_fight}) ->
    dispach_task(?TASK_TRIGGER_ID_PLUNDER_FIGHT, 1),
    dispach_task(?TASK_TRIGGER_ID_PLUNDER_FIGHT_ACH, 1),
    family_dispach_task(?FAMILY_TASK_TRIGGER_ID_PLUNDER_FIGHT, 1);
handle({dispach_task,buy_in_unioncoin}) ->
    dispach_task(?FAMILY_TASK_TRIGGER_ID_BUY_IN_SHOP, 1);
handle({dispach_task,refresh_unioncoin}) ->
    dispach_task(?FAMILY_TASK_TRIGGER_ID_REFRESH_SHOP, 1);
handle({dispach_task,use_local_trumpet}) ->
    dispach_task(?TASK_TRIGGER_ID_LOCAL_TRUMPET,1);
handle({dispach_task,use_cross_trumpet}) ->
    dispach_task(?TASK_TRIGGER_ID_CROSS_TRUMPET,1);
handle({dispach_task, join_worship_task}) ->
    dispach_task(?TASK_TRIGGER_ID_JOIN_WORSHIP_TASK,1);
handle({dispach_task, get_stone, ItemStar}) ->
    dispach_task(?TASK_TRIGGER_ID_GET_STONE, ItemStar);
handle({dispach_task, eat_stone}) ->
    dispach_task(?TASK_TRIGGER_ID_EAT_STONE, 1);
handle({dispach_task, compose_stone}) ->
    dispach_task(?TASK_TRIGGER_ID_COMPOSE_STONE, 1);
handle({dispach_task, equip_stone_num, GerID}) ->
    dispach_task(?TASK_TRIGGER_ID_ALL_STONE_IN_POS, get_total_stone_in_pos()),
    dispach_task(?TASK_TRIGGER_ID_STONE_TOTAL_STAR, get_stone_total_star(GerID));
handle({dispach_task, stone_max_level, ItemStar}) ->
    dispach_task(?TASK_TRIGGER_ID_STONE_MAX_LEVEL, {ItemStar, 1});
handle({dispach_task, family_fight_result, GetStar, IsWin, Period}) ->
    dispach_task(?TASK_TRIGGER_ID_JOIN_FAMILY_FIGHT, Period),
    dispach_task(?TASK_TRIGGER_ID_GET_STAR, GetStar),
    %% 公会战中1代表防守方获胜, 2代表攻击方获胜
    case IsWin =:= 2 of
        true ->
            dispach_task(?TASK_TRIGGER_ID_WIN_FAMILY_FIGHT, 1);
        _ ->
            ignore
    end;
handle({dispach_task,mixing_condition,ConditionType})->
    ?INFO("mixing_condition ~w",[ConditionType]),
    dispach_task(?TASK_TRIGGER_ID_GER_TIMES_BOTH,ConditionType);
handle({dispach_task,role_box_by_gold}) ->
    dispach_task(?TASK_TRIGGER_ID_EXTRACT_CARD_TIMES_GOLD,1);
handle({dispach_task,role_enargy_give_reward}) ->
    ?INFO("dispach_task role_enargy_give_reward"),
    dispach_task(?TASK_TRIGGER_ID_ENARGY_GIVE_REWARD,1),
	family_dispach_task(?FAMILY_TASK_TRIGGER_ID_FAMILY_SEND_ENERGY,1);
handle({dispach_task, get_activity_value, ItemNum}) ->
    dispach_task(?TASK_TRIGGER_ID_GET_ACTIVITY, ItemNum);
handle({dispach_task, role_vip_level, Level}) ->
    dispach_task(?TASK_TRIGGER_ID_VIP_LEVEL, Level);
handle({dispach_task,role_emperor,N}) ->
	dispach_task(?TASK_TRIGGER_ID_EMPEROR, N),
	family_dispach_task(?FAMILY_TASK_TRIGGER_ID_FAMILY_EMPEROR,N);
handle({dispach_task,role_get_energy,N}) ->
	dispach_task(?TASK_TRIGGER_ID_GET_ENERGY,N),
	family_dispach_task(?FAMILY_TASK_TRIGGER_ID_FAMILY_GET_ENERGY,N);
handle({dispach_task, role_buy_energy,N}) ->
	dispach_task(?TASK_TRIGGER_ID_BUY_ENERGY,N);
handle({dispach_task,home_seed,N}) ->
	dispach_task(?TASK_TRIGGER_ID_HOME_SEED,N),
	family_dispach_task(?FAMILY_TASK_TRIGGER_ID_FAMILY_HOME_SEED,N);
handle({dispach_task, home_boss,N}) ->
	dispach_task(?TASK_TRIGGER_ID_HOME_BOSS,N),
	family_dispach_task(?FAMILY_TASK_TRIGGER_ID_FAMILY_HOME_BOSS,N);
handle({dispach_task,carlos_fight,N}) ->
	dispach_task(?TASK_TRIGGER_ID_CARLOS,N);
handle({dispach_task,galactica_fight,N}) ->
	dispach_task(?TASK_TRIGGER_ID_GALACTICA,N);
handle({dispach_task,relic_fight,N}) ->
	dispach_task(?TASK_TRIGGER_ID_RELIC,N);
handle({dispach_task,twins_fight,N}) ->
	dispach_task(?TASK_TRIGGER_ID_TWINS,N);
handle({dispach_task,double_fight_fight,N}) ->
	dispach_task(?TASK_TRIGGER_ID_DOUBLE_MATCH,N).

add_ger_trigger(GerSimpleList)->
	lists:foreach(fun(#gerSimple{gerTypeID=GerTypeID,gerID=GerID,gerQuality=Quality,gerLevel=Level})->
						  #data_ger{gerStar=GerStar, breakthroughIDList=BreakThroughIDList} = data_ger:get(GerTypeID),
                          case role_ger:get_breakthrough_Limit(BreakThroughIDList) of
							  LvList when is_list(LvList) ->
								  trigger_lv_info(GerStar,Quality,LvList,0);
%%                             [Lv1,Lv2] -> 
%%                                 case Quality > Lv1#data_ger_breakthrough.rank_condition of
%%                                     true ->
%%                                         dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL1,GerStar),
%%                                         case Quality > Lv2#data_ger_breakthrough.rank_condition of
%%                                             true ->
%%                                                 dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL2,GerStar);
%%                                             _ ->
%%                                                 ignore
%%                                         end;
%%                                     _ ->
%%                                         ignore
%%                                 end;
                            _ ->
                                ignore
                          end,
						  role_task_trigger:handle({dispach_task,gain_ger,GerID,GerTypeID,GerStar,Quality,Level})
				  end, GerSimpleList).

trigger_lv_info(GerStar,_,[],Count) -> do_lv_info_dispatch(GerStar,Count);
trigger_lv_info(GerStar,Quality,[H|T],Count) ->
	JLevel = H#data_ger_breakthrough.rank_condition,
	if Quality =< JLevel ->
        ?INFO("trigger_lv_info 1 ~w",[{Quality,JLevel,Count}]),
        do_lv_info_dispatch(GerStar,Count);
	   true ->
        ?INFO("trigger_lv_info 2 ~w",[{Quality,JLevel,Count}]),
        trigger_lv_info(GerStar,Quality,T,Count+1)
	end.
do_lv_info_dispatch(GerStar,1) -> dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL1,GerStar);
do_lv_info_dispatch(GerStar,2) -> dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL2,GerStar);
do_lv_info_dispatch(GerStar,3) -> dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL3,GerStar);
do_lv_info_dispatch(GerStar,4) -> dispach_task(?TASK_TRIGGER_ID_TRANSFORM_LVL4,GerStar);
do_lv_info_dispatch(_,_) -> ignore.
	
add_item_trigger(ItemList)->
	lists:foreach(fun(#item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemType=ItemType,itemRank=Quality,itemLevel=ItemLevel,itemNum=ItemNum})->
						  case item_lib:is_itemType_equip(ItemType) of
							  true->
								  #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
								  role_task_trigger:handle({dispach_task,create_equip,ItemUID,ItemTypeID,ItemStar,Quality,ItemLevel});
							  false->
                                    case item_lib:is_itemType_stone(ItemType, true) orelse item_lib:is_itemType_essence_stone(ItemType) of
                                        true ->
								            #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
								            role_task_trigger:handle({dispach_task, get_stone, ItemStar});
                                        _ ->
                                            ignore
                                    end    
						  end  
				  end, ItemList).
	
%% TaskIDList是整数list
dispach_task(TriggerID,TriggerData)->
	TaskIDList = role_task:get_trigger_task_id(TriggerID),
    ?INFO("TaskIDList:~w",[{TriggerID,TriggerData,TaskIDList}]),
	disapach_task(TriggerID,TriggerData,TaskIDList).
	
disapach_task(_TriggerID,_TriggerData,[])->
	ignore;
disapach_task(TriggerID,TriggerData,[TaskID|TailTaskIDList])->
	trigger_task(TriggerID,TaskID,TriggerData),
	disapach_task(TriggerID,TriggerData,TailTaskIDList).

family_dispach_task(TriggerID,TriggerData)->
    #role{familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {family_dispach_task, TriggerID, TriggerData});
        _ ->
            ignore
    end.

family_dispach_task(FamilyID, TriggerID, TriggerData)->
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {family_dispach_task, TriggerID, TriggerData});
        _ ->
            ignore
    end.

%% 判断TaskID任务状态是否完成
trigger_task(TriggerID,TaskID,TriggerData)->
    %% 取得配置信息
	#data_task{trigger_num=Num,trigger_int_list=IntList,task_type=TaskType} = data_task:get(TaskID),
	AllTaskList = role_data:get_task_list(TaskType),
	CheckFun = fun(#r_task{task_id=TID}) ->TID =:= TaskID end,
    %% 从 AllTaskList 中取出 TaskID对应的元素, TailTaskList是其余的任务
    %?INFO("trigger_task ~w",[{TriggerID,TaskID,TriggerData}]),
	case util:fun_take(CheckFun, AllTaskList) of
		false->
			ignore;
		{value,Task,TailTaskList}->
			#r_task{status=Status,trigger_num=CurrNum,trigger_notes=TriggerNotes} = Task,
			case Status of
                %% 仅处理接受了的任务
				?TASK_STATUS_WAS_ACCEPT when TaskType /= ?TASK_TYPE_FAMILY_TODAY ->
					case check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) of
						true->
							role_task:do_change_task_status(Task,TailTaskList);
						{true,NewNum}->
							NewTask = Task#r_task{trigger_num=NewNum},
							role_task:do_change_task_status(NewTask,TailTaskList);
						{true,NewNum,NewTriggerNotes} ->
							NewTask = Task#r_task{trigger_notes=NewTriggerNotes,trigger_num=NewNum},
							role_task:do_change_task_status(NewTask,TailTaskList);
						{false,NewNum}->
							case role_task:transaction(fun()->
													 NewTask = Task#r_task{trigger_num=NewNum},
													 role_data:set_task_list(TaskType,[NewTask|TailTaskList]),
													 NewTask
											 end) of
								{atomic,NewTask}->
									put(?interval_persist_flag,true),
									role_task:send_notify_change(TaskType,[NewTask],[]);
%% 									?sendself(#sc_task_notify_change{task_type=TaskType,updata_task_list=[role_task:r_task2p_task()],del_task_id=[]});
								{abort,Reason}->
									?ERR("触发任务时发生错误:~w~n任务数据为:~w",[Reason,Task]),
									ok
							end;
						{false,NewNum,NewTriggerNotes}->
							case role_task:transaction(fun()->
													 NewTask = Task#r_task{trigger_num=NewNum,trigger_notes=NewTriggerNotes},
													 role_data:set_task_list(TaskType,[NewTask|TailTaskList]),
													 NewTask
											 end) of
								{atomic,NewTask}->
									put(?interval_persist_flag,true),
									role_task:send_notify_change(TaskType,[NewTask],[]);
%% 									?sendself(#sc_task_notify_change{task_type=TaskType,updata_task_list=[role_task:r_task2p_task(NewTask)],del_task_id=[]});
								{abort,Reason}->
									?ERR("触发任务时发生错误:~w~n任务数据为:~w",[Reason,Task]),
									ok
							end;
						false->
								ignore
					end;
				_->
					ignore
			end
	end.
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS 
																		    orelse TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_HARD_PASS 
																		    orelse TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_FAST_HARD_PASS 
																		    orelse TriggerID=:=?TASK_TRIGGER_ID_DUNGEON_PASS
																		    orelse TriggerID=:=?TASK_TRIGGER_ID_CHAPTER_NORMAL_PASS
                                                                            orelse TriggerID =:=?TASK_TRIGGER_ID_STONE_MAX_LEVEL
                                                                            ->
    NewCurrNum = get_next_num_7(TriggerData,CurrNum,IntList),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_GER_UP_QUALITY_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_REFIE_EQUIP
																			 orelse TriggerID=:=?TASK_TRIGGER_ID_EVOLUTION_PET
																			 orelse TriggerID=:=?TASK_TRIGGER_ID_TRANSFORM_LVL1
																			 orelse TriggerID=:=?TASK_TRIGGER_ID_TRANSFORM_LVL2
																			 orelse TriggerID=:=?TASK_TRIGGER_ID_TRANSFORM_LVL3
																			 orelse TriggerID=:=?TASK_TRIGGER_ID_TRANSFORM_LVL4
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_GET_STONE
                                                                         ->
	NewCurrNum = get_next_num_2(TriggerData,CurrNum,IntList),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_KILL_MONSTER->
	MonsterID = 
		case IntList of
			[]->
					0;
			[MID]->
				MID
		end,
		Sum = lists:foldl(fun({MID,N},T)->
							  case MonsterID=:=0 orelse MonsterID=:=MID of
								  true->
									  T+N;
								  false->
									  T
							  end
					  end, 0, TriggerData),
	NewCurrNum = CurrNum + Sum,
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when  TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG
														 orelse	 TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY->
	NewCurrNum = get_next_num_1(TriggerData,Num,IntList),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_UP_QUALITY
																		orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_1
																		orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1 ->
	{NewCurrNum,NewTriggerNotes} = get_next_num_3(TriggerData,CurrNum,TriggerNotes,IntList),
	return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_GAIN_STAR_GER
																		orelse TriggerID =:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_2
																		orelse TriggerID =:= ?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_3
                                                                        orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP->
    {NewCurrNum,NewTriggerNotes} = get_next_num_3_2(TriggerData,CurrNum,TriggerNotes,IntList),
    return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_EQUIP
																		orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_TOTAL_EQUIP
																		orelse TriggerID=:=?TASK_TRIGGER_ID_GER_GAIN_GER->
	{NewCurrNum,NewTriggerNotes} = get_next_num_4(TriggerData,CurrNum,TriggerNotes,IntList),
	return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_ROLE_UP_LEVLE
																		   orelse TriggerID=:=?TASK_TRIGGER_ID_GER_UP_LEVEL
                                                                           orelse TriggerID=:=?TASK_TRIGGER_ID_STONE_TOTAL_STAR 
                                                                           orelse TriggerID=:=?TASK_TRIGGER_ID_ALL_STONE_IN_POS
                                                                           orelse TriggerID =:= ?TASK_TRIGGER_ID_VIP_LEVEL
																		   ->
	return_2(TriggerData,CurrNum,Num);

check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_ADD_REWARD
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_ADD_REWARD_GOLD
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_ADD_REWARD_REPU
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_TIMES
																		 ->
	NewCurrNum = get_next_num_5(TriggerData, CurrNum, IntList),
	return_2(NewCurrNum,CurrNum,Num);
	
check_data(TriggerID,_TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_REPLACE_UP_GER_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_CHAT_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_SIGN_RACE_TIMES
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_CHANGE_HEAD_TIMES
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_BOX_GER_10
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_BOX_EQUIP_10
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_GEN_MATCH
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_PLUNDER_FIGHT
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_PLUNDER_FIGHT_ACH
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_JOIN_WORSHIP_TASK
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_EAT_STONE
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_COMPOSE_STONE
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_WIN_FAMILY_FIGHT
                                                                         orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_PLUNDER_FIGHT
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_LOCAL_TRUMPET
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_CROSS_TRUMPET
                                                                         orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_BUY_IN_SHOP
                                                                         orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_REFRESH_SHOP
                                                                         orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_REFIE_EQUIP
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_EXTRACT_CARD_TIMES_GOLD
                                                                         orelse TriggerID=:=?TASK_TRIGGER_ID_ENARGY_GIVE_REWARD
																		   	orelse TriggerID=:=?TASK_TRIGGER_ID_EMPEROR
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_GET_ENERGY
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_BUY_ENERGY
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_PAY_GOLD
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_HOME_BOSS
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_CARLOS
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_GALACTICA
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_RELIC
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_TWINS
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_DOUBLE_MATCH
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_FAMILY_HOME_BOSS
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_FAMILY_EMPEROR
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_FAMILY_SEND_ENERGY
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_FAMILY_GET_ENERGY
																		   ->
    NewCurrNum = get_next_num(CurrNum),
    ?INFO("check_data ~w ~w",[NewCurrNum,CurrNum]),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,_IntList) when TriggerID =:=?TASK_TRIGGER_ID_ADD_FRIEND_NUM
																		 orelse TriggerID=:=?TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND->
	{NewCurrNum, NewTriggerNotes} = get_next_num_6(TriggerData,CurrNum,TriggerNotes),
	return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID =:=?TASK_TRIGGER_ID_EXTRACT_CARD_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_EXPLORE_TIMES
																		      orelse TriggerID =:=?TASK_TRIGGER_ID_USE_GOLD
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_JOIN_HRON_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_JOIN_HULA_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_JOIN_NANM_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_PVP_FIGHT_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_COMBINE_GER_RANDOM_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_COMBINE_TIMES
																			  orelse TriggerID=:=?TASK_TRIGGER_ID_BUY_COIN_TIMES
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS %% 10001 -公会-普通关卡 
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES  %% 10002 -公会-升级X次宠物
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_EVOLUTION_PET       %% 10003 -公会-累计进化N次精灵
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_EQUIP_STRONG_TIMES  %% 10004 -公会-装备强化次数
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_REFIE_EQUIP         %% 10005 -公会-累计精炼N次装备
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_EXPLORE_TIMES       %% 10006 -公会-探索次数
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_COMBINE_TIMES       %% 10007 -公会-合成次数
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_EXTRACT_CARD_TIMES  %% 10008 -公会-抽卡N次
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_JOIN_HRON_TIMES   %% 10009 -公会-无尽深渊通过第N关卡
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_JOIN_HULA_TIMES     %% 10010 -公会-参加N次玲玲塔战斗
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_JOIN_NANM_TIMES     %% 10011 -公会-参加N次研究所战斗
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES      %% 10012 -公会-充能次数
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES          %% 10013 -公会-交配次数
																			  orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_PVP_FIGHT_TIMES     %% 10014 -公会-竞技场战斗次数
                                                                              orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_FAMILY_BUY_COIN     %% 10024 - 全民招财
                                                                              orelse TriggerID=:=?FAMILY_TASK_TRIGGER_ID_FAMILY_HOME_SEED
  ->
	NewCurrNum = get_next_num_8(TriggerData,CurrNum),
	return_2(NewCurrNum,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_ROLE_PVP_RANK->
	return_4(TriggerData, CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_3V3_RANK_CHANGE
                                                                          orelse TriggerID=:=?TASK_TRIGGER_ID_DMG_RANK_HULA 
                                                                          orelse TriggerID=:=?TASK_TRIGGER_ID_DMG_RANK_NANM
                                                                           ->
	return_6(TriggerData,CurrNum,Num);
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_FINISHED_3V3 
                                                                     orelse TriggerID=:=?TASK_TRIGGER_ID_GET_STAR
                                                                     ->
	NewNum = TriggerData + CurrNum,
	return_2( NewNum, CurrNum, Num );
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_SIGN_ALIEN ->
	NewNum = TriggerData + CurrNum,
	return_2( NewNum, CurrNum, Num );
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_BET_ALIEN ->
	NewNum = TriggerData + CurrNum,
	return_2( NewNum, CurrNum, Num );
check_data(TriggerID,RoleKillContinuousNum,CurrNum,TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_KILLED_ALIEN ->
    return_5(RoleKillContinuousNum, CurrNum, Num, TriggerNotes);

%% 因为收集任务的数据会一直变动,所以按照正常方式，状态会一直切换。如果这样写代码复杂，对原有流程改动较大
%% 现在和客户端约定，这几个收集任务一直为未完成状态，客户端和服务器都通过当前的trigger_num数来进行判断
check_data(TriggerID,_TriggerData,CurrNum,_TriggerNotes,_Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_GER ->
    RoleID = role_data:get_roleID( ),
    HomesteadInfo = homestead_server:get_ets_homestead_base(RoleID),
    CurrGerID = HomesteadInfo#p_homestead.gerID,
    case IntList of
        [_NeedStar, _NeedRank] ->
		    NewNum = calc_satisfy_number(fun check_is_collect_ger/2, [CurrGerID|IntList], role_data:get_gerBag( ), 0);
        _ ->
            NewNum = CurrNum
    end,
    {false, NewNum};

%% 这里的收集装备代码和上面的收集精灵的模式时一样的,但没有必要写成一个函数
check_data(TriggerID,_TriggerData,CurrNum,_TriggerNotes,_Num,IntList) when TriggerID=:=?TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_EQUIP ->
    case IntList of
        [_NeedStar,_NeedRank] ->
            NewNum = calc_satisfy_number(fun check_is_collect_equip/2, IntList, role_data:get_bagEquip( ), 0);
        _ ->
            NewNum = CurrNum
    end,
    {false, NewNum};

check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_FAMILY_WORSHIP_FIGHT ->
    NewCurrNum = get_next_num(CurrNum),
    OldData = case is_number(TriggerNotes) of
        true ->
            TriggerNotes;
        false ->
            0
    end,
    return_3(NewCurrNum, CurrNum, Num, OldData + TriggerData);

%% 这个成就每次公会战只计算一次
check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_JOIN_FAMILY_FIGHT ->
    case TriggerData =:= TriggerNotes of
        true ->
            NewCurrNum = CurrNum,
            NewTriggerNotes = TriggerNotes;
        _ ->
            NewCurrNum = CurrNum + 1,
            NewTriggerNotes = TriggerData
    end,
    return_3(NewCurrNum, CurrNum, Num, NewTriggerNotes);

%%针对抽精灵抽装备各一次的任务完成条件，
check_data(TriggerID,TriggerData,CurrNum,_TriggerNotes,_Num,_IntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_TIMES_BOTH ->
    NewBits = (1 bsl (TriggerData - 1)) bor CurrNum,
    ?INFO("NewBits:~w(~w)",[NewBits,CurrNum]),
    return_bit(NewBits,3);

%%默认比大小
check_data(_TriggerID,TriggerData,CurrNum,_TriggerNotes,Num,_IntList) ->
	return_2(TriggerData,CurrNum,Num).
						
get_next_num(CurrNum)->
	CurrNum+1.

%%TriggerData====>>>{T,L}
%%IntList不为空时：当T在列表中且L大于等于ConfigNum时返回L
%%IntList为空时：当L大于等于ConfigNum时返回L
get_next_num_1(TriggerData,ConfigNum,IntList)->
	{T,Num} = TriggerData,
	case IntList of
		[]->
			Num;
		[T]->
			erlang:min(Num, ConfigNum);
		_->
			0
	end.
%%TriggerData=====>>>T
%%IntList不为空时：当T在列表中时返回CurrNum+1
%%IntList为空时直接返回CurrNum+1
get_next_num_2(TriggerData,CurrNum,IntList)->
	case IntList of
		[]->
			CurrNum+1;
		[TriggerData]->
			CurrNum+1;
		_->
			CurrNum
	end.
%%TriggerData=====>>>{T,L}
%%IntList=========>>>[J]
%%T在TriggerNotes里返回CurrNum和TriggerNotes
%%T没有在TriggerNotes里且L大于等于J时返回CurrNum+1和[T|TriggerNotes]
get_next_num_3(TriggerData,CurrNum,TriggerNotes,IntList)->
	{Type,Level} = TriggerData,
	[ConfigLevel] = IntList,
	case lists:member(Type,TriggerNotes) of
		true->
			{CurrNum,TriggerNotes};
		false->
				case Level>=ConfigLevel of
					true->
						NewCurrNum =CurrNum+1,
						{NewCurrNum,[Type|TriggerNotes]};
					false->
						{CurrNum,TriggerNotes}
				end
	end.

get_next_num_3_2(TriggerData,CurrNum,TriggerNotes,IntList)->
    {UID,StarLevel} = TriggerData,
    [ConfigStarLevel] = IntList,
    case lists:member(UID,TriggerNotes) of
        true->
            {CurrNum,TriggerNotes};
        false->
                case StarLevel =:= ConfigStarLevel of
                    true->
                        NewCurrNum =CurrNum+1,
                        {NewCurrNum,[UID|TriggerNotes]};
                    false->
                        {CurrNum,TriggerNotes}
                end
    end.

%%TriggerData=====>>>{T,L}
%%IntList=========>>>[J]
%%T在TriggerNotes里返回CurrNum和TriggerNotes
%%T没有在TriggerNotes里且(IntList为空或L在IntList里)时返回CurrNum+1和[T|TriggerNotes]
get_next_num_4(TriggerData,CurrNum,TriggerNotes,IntList)->
	{UID,Type} = TriggerData,
	case lists:member(UID,TriggerNotes) of
		true->
			{CurrNum,TriggerNotes};
		false->
				case IntList=:=[] orelse lists:member(Type,IntList) of
					true->
						NewCurrNum =CurrNum+1,
						{NewCurrNum,[UID|TriggerNotes]};
					false->
						{CurrNum,TriggerNotes}
				end
	end.

get_next_num_5(TriggerData,CurrNum,IntList)->
	{AddNum,Type} = TriggerData,
	case IntList of
		[Type]->
			CurrNum+AddNum;
		[]->
			CurrNum+AddNum;
		_->
			CurrNum
	end.

get_next_num_6(TriggerData,CurrNum,TriggerNotes)->
	case lists:member(TriggerData, TriggerNotes) of
		true->
			{CurrNum, TriggerNotes};
		false->
			{CurrNum+1, [TriggerData|TriggerNotes]}
	end.

%%TriggerData=====>>>{T,N}
%%IntList不为空时：当T在列表中时返回CurrNum+N
%%IntList为空时直接返回CurrNum+N
get_next_num_7(TriggerData,CurrNum,IntList)->
	{T,N} = TriggerData,
	case IntList of
		[]->
			CurrNum+N;
		[T]->
			CurrNum+N;
		_->
			CurrNum
	end.

get_next_num_8(TriggerData,CurrNum)->
	CurrNum+TriggerData.

%%NewCurrNum大于等于Num是返回改变状态
return_2(NewCurrNum,CurrNum,Num)->
	if
		NewCurrNum>=Num->
			{true,Num};
		NewCurrNum>CurrNum->
			{false,NewCurrNum};
		true->
			false
	end.

return_3(NewCurrNum,CurrNum,Num,NewTriggerNotes)->
	if
		NewCurrNum>=Num->
			{true,Num,NewTriggerNotes};
		NewCurrNum>CurrNum->
			{false,NewCurrNum,NewTriggerNotes};
		true->
			false
	end.

return_4(NewCurrNum,CurrNum,Num)->
	if
		NewCurrNum=<Num->
			{true,NewCurrNum};
		NewCurrNum<CurrNum->
			{false,NewCurrNum};
		CurrNum=:=0->
			{false,NewCurrNum};
		true->
			false
	end.

%%成立条件是新值大于等于需求的值
return_5(NewCurrNum,_CurrNum,Num,TriggerNotes)->
    if
        NewCurrNum>=Num->
           {true,Num,TriggerNotes};
        true->
           {false,NewCurrNum,TriggerNotes}
	end.

%%成立条件是新值小于等于需求的值
return_6(NewCurrNum,_CurrNum,Num)->
    if 
        NewCurrNum=<Num->
           {true,Num};
       true->
           {false,NewCurrNum}
    end.

return_bit(NewCurrNum,Num)->
    if 
        (NewCurrNum band Num) =:= Num->
           {true,Num};
       true->
           {false,NewCurrNum}
    end.

%% %%得到更大值
%% get_lager_num(TriggerData,CurrNum,ConfigNum,IntList)->
%% 	{T,Num} = TriggerData,
%% 	case IntList of
%% 		[]->
%% 			CurrNum;
%% 		_->
%% 			case lists:member(T, IntList) of
%% 				true->
%% 					erlang:min(Num, ConfigNum);
%% 				false->
%% 					CurrNum
%% 			end
%% 	end.

get_trigger_num(TriggerID,TriggerNum,_TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_ROLE_UP_LEVLE->
	#role{level=Level} = role_data:get_roleInfo(),
	case Level>=TriggerNum of
		true->
			finish;
		_->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,_TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_UP_LEVEL->
	case lists:any(fun(GerSimple)->
					  GerSimple#gerSimple.gerLevel>=TriggerNum
			  end,role_data:get_gerBag()) of
		true->
			finish;
		false->
			0
	end;

get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_GET_ACTIVITY->
    BagEquip = role_data:get_bagEquip(),
    % 先从背包中找
    case lists:keytake(20038, #item.itemTypeID, BagEquip) of
        false ->
            0;
        {value, Item, BagEquip2} ->
            #item{itemNum= CurNum} = Item,
            if
                CurNum >= TriggerNum ->
                    finish;
                true ->
                    CurNum
            end
    end;

get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	IsOK =
		case TriggerIntList of
			[]->
				lists:any(fun(#item{itemLevel=ItemLevel})->
								  ItemLevel>=TriggerNum
						  end,AllEquipList);
			[Type]->
				lists:any(fun(#item{itemLevel=ItemLevel,itemTypeID=ITI})->
								  ItemLevel>=TriggerNum andalso Type=:=ITI
						  end,AllEquipList)
		end,
	case IsOK of
		true->
			finish;
		false->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	IsOK =
		case TriggerIntList of
			[]->
				lists:any(fun(#item{itemRank=ItemRank})->
								  ItemRank>=TriggerNum
						  end,AllEquipList);
			[Type]->
				lists:any(fun(#item{itemRank=ItemRank,itemTypeID=ItemTypeID})->
								   ItemRank>=TriggerNum andalso Type=:=ItemTypeID
						  end,AllEquipList)
		end,
	case IsOK of
		true->
			finish;
		false->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_EQUIP->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	IsOK =
		case TriggerIntList of
			[]->
				length(EquipedList)>=TriggerNum;
			[Type]->
				Len = length(lists:filter(fun(#item{itemTypeID=ITI})->
												  Type=:=ITI
										  end,EquipedList)),
				Len>=TriggerNum
		end,
	case IsOK of
		true->
			finish;
		false->
			0
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_TIMES->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	Sum =
		case TriggerIntList of
			[]->
				 lists:foldl(fun(#item{itemLevel=ItemLevel},N)->
									ItemLevel+N
							end,0,AllEquipList);
			[Type]->
				lists:foldl(fun(#item{itemLevel=ItemLevel,itemTypeID=ITI},N)->
									case ITI =:= Type of
										true->
											ItemLevel+N;
										false->
											N
									end
							end,0,AllEquipList)
		end,
	case Sum>=TriggerNum of
		true->
			finish;
		false->
			Sum
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_GAIN_STAR_GER->
	[L] = TriggerIntList,
	case lists:foldl(fun(GerSimple,S)->
							 GerTypeID = GerSimple#gerSimple.gerTypeID,
							 case lists:member(GerTypeID, [2,3,4,5,6,7]) of
								 true->
									 S;
								 false->
									 case  (data_ger:get(GerTypeID))#data_ger.gerStar =:= L of
										 true->
											 S+1;
										 false->
											 S
									 end
							 end
			  end,0,role_data:get_gerBag()) of
		N when N>=TriggerNum->
			finish;
		N->
			N
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP->
    [ConfigStarLevel] = TriggerIntList,
    EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
    BagEquipList = role_data:get_bagEquip(),
    AllEquipList = EquipedList++BagEquipList,
    Sum =
        lists:foldl(
          fun(#item{itemTypeID=ItemTypeID},N)->
                  #data_item{itemStar=ItemStarLevel} = data_item:get(ItemTypeID),
                  case ItemStarLevel =:= ConfigStarLevel of
                      true ->
                          N + 1;
                      false ->
                          N
                  end
          end,0,AllEquipList),
    case Sum >= TriggerNum of
        true->
            finish;
        false->
            Sum
    end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_GER_UP_QUALITY->
	[L] = TriggerIntList,
	case lists:foldl(fun(#gerSimple{gerID=GerID,gerQuality=Quality},{S,Acc}=Info)->
						  case  Quality >=L of
							  true->
								  {S+1,[GerID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},role_data:get_gerBag()) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	[L] = TriggerIntList,
	case lists:foldl(fun(#item{itemUID=ItemUID,itemRank=ItemRank},{S,Acc}=Info)->
						  case  ItemRank >=L of
							  true->
								  {S+1,[ItemUID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},AllEquipList) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_2->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	[L,TriggerRank] = TriggerIntList,
	case lists:foldl(fun(#item{itemUID=ItemUID,itemRank=ItemRank,itemTypeID=ItemTypeID},{S,Acc}=Info)->
							 #data_item{itemStar=ItemStar} = data_item:get(itemTypeID),
						  case  ItemRank ==TriggerRank andalso ItemStar == L of
							  true->
								  {S+1,[ItemUID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},AllEquipList) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_UP_QUALITY_3->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	[L] = TriggerIntList,
	case lists:foldl(fun(#item{itemUID=ItemUID,itemRank=ItemRank,itemTypeID=ItemTypeID},{S,Acc}=Info)->
							 #data_item{itemStar=ItemStar} = data_item:get(itemTypeID),
						  case  ItemRank == 10 andalso ItemStar == L of
							  true->
								  {S+1,[ItemUID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},AllEquipList) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(TriggerID,TriggerNum,TriggerIntList) when TriggerID=:=?TASK_TRIGGER_ID_EQUIP_STRONG_1->
	EquipedList = [E || {_,EquipList}<-role_data:get_equiped_list(), E<-EquipList],
	BagEquipList = role_data:get_bagEquip(),
	AllEquipList = EquipedList++BagEquipList,
	[L] = TriggerIntList,
	case lists:foldl(fun(#item{itemUID=ItemUID,itemLevel=ItemLevel},{S,Acc}=Info)->
						  case  ItemLevel >=L of
							  true->
								  {S+1,[ItemUID|Acc]};
							  false->
								  Info
						  end
			  end,{0,[]},AllEquipList) of
		{N,Acc} when N>=TriggerNum->
			{finish,Acc};
		{N,Acc}->
			{N,Acc}
	end;
get_trigger_num(_TriggerID,_TriggerNum,_TriggerIntList)->
	0.

%%=====================================================================
%%添加好友
offline_add_friend(RoleID,FriendRoleID,IsDiffSex)->
	IDList = get_taskid_config_4_triggerid(?TASK_TRIGGER_ID_ADD_FRIEND_NUM),
	IDList1 = 
		case IsDiffSex of
			true->
				get_taskid_config_4_triggerid(?TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND);
			false->
				[]
		end,
	case IDList=:=[] andalso IDList1=:=[] of
		[]->
			ignore;
		_->
			spawn(fun()->
						  offline_add_friend_1(RoleID,FriendRoleID,IDList++IDList1)
				  end)
	end.
offline_add_friend_1(RoleID,FriendRoleID,IDList)->
    lists:foreach(
      fun(TaskID)->
              case db_sql:get_task_by_id(RoleID, TaskID) of
                  ?undefined ->
                      ignore;
                  #r_task{status=Status,trigger_num=TriggerNum,trigger_notes=TriggerNotes} = Task ->
                      case Status=:=?TASK_STATUS_WAS_ACCEPT andalso not lists:member(FriendRoleID,TriggerNotes) of
                          true->
                              NewCurrNum = TriggerNum+1,
                              NewTriggerNotes = [FriendRoleID|TriggerNotes],
                              #data_task{trigger_num=Num} = data_task:get(TaskID),
                              NewStatus =
                                  case NewCurrNum>=Num of
                                      true->
                                          Status+1;
                                      false->
                                          Status
                                  end,
                              NewTask = Task#r_task{status=NewStatus,trigger_num=NewCurrNum,trigger_notes=NewTriggerNotes},
                              db_sql:set_task_by_id(RoleID, NewTask);
                          false->
                              ignore
                      end
              end
      end, IDList).

get_taskid_config_4_triggerid(TriggerID)->
	TaskList = data_task:get_list(),
	lists:filter(fun(TaskID)->
						 #data_task{trigger_id=TID} = data_task:get(TaskID),
						 TID =:= TriggerID
				 end, TaskList).

%%=====================================================================
%%异星战场比赛结果
offline_alien_fight_res(RoleID,RoleKillContinuousNum) ->
	IDList = get_taskid_config_4_triggerid(?TASK_TRIGGER_ID_KILLED_ALIEN),
	if 
        IDList =:= [] ->
            ignore;
        true ->
            spawn(fun() -> offline_alien_fight_res1(RoleID, RoleKillContinuousNum, IDList) end)
    end.
offline_alien_fight_res1(RoleID, RoleKillContinuousNum, IDList) ->
    lists:foreach(
        fun(TaskID) ->
                case db_sql:get_task_by_id(RoleID,TaskID) of
                    ?undefined ->
                        ignore;
                    #r_task{status=Status,trigger_num=TriggerNum} = Task ->
                        case Status =:= ?TASK_STATUS_WAS_ACCEPT of 
                            true ->
                                NewNum = 
                                    case RoleKillContinuousNum >= TriggerNum of
                                        true ->
                                            RoleKillContinuousNum;
                                        _ ->
                                            TriggerNum
                                    end,

                                #data_task{trigger_num=Num} = data_task:get(TaskID),
                                NewStatus = 
                                    case NewNum >= Num of
                                        true ->
                                            Status + 1;
                                        _ ->
                                            Status
                                    end,
	                            NewTask = Task#r_task{status=NewStatus,trigger_num=NewNum},
	                            db_sql:set_task_by_id(RoleID, NewTask);
                            false ->
                                ignore
                        end
                end
        end, IDList).

%% 3v3排名发生变化
offline_team_pk_rank_change(RoleID,Rank) ->
    common_offline_rank_change(RoleID,Rank,?TASK_TRIGGER_ID_3V3_RANK_CHANGE).

%% hula伤害排名发生变化
offline_hula_rank(RoleID,Rank) ->
    common_offline_rank_change(RoleID,Rank,?TASK_TRIGGER_ID_DMG_RANK_HULA).

%% nanm伤害排名发生变化
offline_nanm_rank(RoleID,Rank) ->
    common_offline_rank_change(RoleID,Rank,?TASK_TRIGGER_ID_DMG_RANK_NANM).

%% 通用离线排名任务触发
common_offline_rank_change(RoleID,Rank,TriggerID) ->
    case get_taskid_config_4_triggerid(TriggerID) of
        [] ->
            ignore;
        IDList ->
            spawn(fun() -> common_offline_rank_change_1(RoleID, Rank, IDList) end)
    end.

common_offline_rank_change_1(RoleID, Rank, IDList)  ->
   lists:foreach(
        fun(TaskID) ->
                case db_sql:get_task_by_id(RoleID,TaskID) of
                    undefined ->
                        ignore;
                    #r_task{status=Status,trigger_num=_TriggerNum} = Task ->
                        case Status =:= ?TASK_STATUS_WAS_ACCEPT of
                            true ->
                                #data_task{trigger_num=Num} = data_task:get(TaskID),
                                NewStatus = case Rank =< Num of
                                    true ->
                                        Status+1;
                                    false ->
                                        Status
                                    end,
                                NewTask = Task#r_task{status=NewStatus,trigger_num=Rank},
                                db_sql:set_task_by_id(RoleID,NewTask);
                            _ ->
                                ignore
                            end
                end
        end, IDList).

calc_satisfy_number(_, _, [], Acc) ->
    Acc;
calc_satisfy_number(Fun, IntList, [H|T], Acc) ->
    case Fun(H, IntList) of
        true ->
            calc_satisfy_number(Fun, IntList, T, Acc + 1);
        false ->
            calc_satisfy_number(Fun, IntList, T, Acc)
    end.

check_is_collect_ger(E,[SteadGerID,NeedStar,NeedRank])  ->
	case E of 
	   #gerSimple{gerID=GerID, gerTypeID=GerTypeID,gerQuality=Quality} ->
	       next;
	   _ ->
           GerID=0,
	       GerTypeID=0,
           Quality=0,
	       %%?ERR("精灵收集任务,异常:~p.~n", [E])
	       ignore
	end,
	case not lists:member(GerTypeID,?COLLECT_GER_MASK_TYPE_IDS) andalso GerID =/= SteadGerID of
	   true ->
	       #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
	       GerStar =:= NeedStar andalso NeedRank =:= Quality;
	   _ ->
	       false
	end.

check_is_collect_equip(#item{itemTypeID=ItemTypeID,itemRank=ItemRank}, [NeedStar, NeedRank]) ->
    case data_item:get(ItemTypeID) of
        undefined ->
            false;
        #data_item{itemType=ItemType, itemStar=ItemStar} ->
            %% 符文不能被任务消耗掉
            item_lib:is_itemType_equip(ItemType) andalso ItemStar =:= NeedStar andalso ItemRank =:= NeedRank          
    end.

get_total_stone_in_pos() ->
    lists:foldl(fun(#ger{gerID=GerID}, Acc) ->
                    EquipList = role_data:get_equip(GerID),
                    lists:foldl(fun(#item{itemPos=ItemPos}, Acc2) ->
                                    case role_item:check_pos_stone(GerID,ItemPos) orelse role_item:check_pos_essence_stone(GerID,ItemPos) of
                                         true ->
                                             Acc2 + 1;
                                         _ ->
                                             Acc2
                                    end
                                end, Acc, EquipList)
                end, 0, role_data:get_posList()).

get_stone_total_star(GerID) ->
    lists:foldl(fun(#item{itemTypeID=EquipTypeID, itemPos=ItemPos}, Acc) ->
                    case role_item:check_pos_stone(GerID,ItemPos) orelse role_item:check_pos_essence_stone(GerID,ItemPos) of
                        true ->
                            #data_item{itemStar=ItemStar}=data_item:get(EquipTypeID),
                            Acc + ItemStar;
                        _ ->
                            Acc
                    end
                end, 0, role_data:get_equip(GerID)).
                        

do_src_levelup_record(RoleID,Level) ->
    #role{roleName = RoleName,gold=G,goldBonus=GB,goldUsed=GU,accid=AccountID,srcType=Type} = role_data:get_roleInfo(),
    SignSec = role_data:get_signSec(),
    UrlMap = [{27,"upload_vivo_level_data"},{3,"upload_uc_level_data"},{7,"upload_uc_level_data"}],
    Member = lists:keyfind(Type,1, UrlMap),
    case Member of 
        {_,Url} ->
            spawn(fun() -> 
                          ServerID = RoleID div 1000000 - 1,
                          Accid = AccountID rem 10000000000,
                          AccountServerAddr = data_setting:get(account_server_url),
                          Data = ejson:encode([ServerID,Accid,RoleID,Level,G+GB,GU,RoleName,SignSec,Type]),
                          URL = lists:flatten(io_lib:format("~s/~s?data=~s"
                                                            , [AccountServerAddr,Url,Data])),
                          %?ERR("url:~s",[URL]),
                          httpc:request(get, {URL, []}, [{timeout, 3000},{connect_timeout,3000}], [])
                  
                  end);
        _->
            ignore
    end.

