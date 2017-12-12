%% @author lihuachao
%% @doc @todo 家园


-module(role_homestead).


-include("def_role.hrl").
-include("def_homestead.hrl").
-include("def_reward.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-compile(export_all).

%% ====================================================================
%% Internal functions
%% ====================================================================
cs_homestead_get_info(_)->
	case is_open_homestead() of
		true->
			RoleID = role_data:get_roleID(),
			HomesteadInfo = homestead_server:get_ets_homestead_base(RoleID),
			MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
			?sendself(#sc_homestead_get_info{baseinfo=HomesteadInfo,machineList=MachineList});
		_->
			?sendself(#sc_homestead_error{reason_code=?HOMESTEAD_ERROR_NOT_OPEN})
	end.

cs_homestead_get_log(_)->
	case is_open_homestead() of
		true->
			RoleID = role_data:get_roleID(),
			LogList = homestead_server:get_ets_homestead_logList(RoleID),
			?sendself(#sc_homestead_get_log{list=LogList});
		_->
			?sendself(#sc_homestead_error{reason_code=?HOMESTEAD_ERROR_NOT_OPEN})
	end.

cs_homestead_unlock_machine(#cs_homestead_unlock_machine{num=Num})->
	RoleID = role_data:get_roleID(),
	case check_unlock_machine(RoleID,Num) of
		{true,RoleInfo,NeedGold}->
			unlock_machine(RoleID,Num,RoleInfo,NeedGold);
		{false,ErrorCode}->
			?sendself(#sc_homestead_error{reason_code=ErrorCode})
	end.

cs_homestead_uproot_seed(#cs_homestead_uproot_seed{num=Num})->
	RoleID = role_data:get_roleID(),
 	case check_uproot_seed(RoleID,Num) of
		{true,Machine,_TailMachineList}->
			NewMachine = Machine#p_homestead_machine{num=Num,endSecond=0,seedItemID=0,addEnergyEndS=0},
			refresh_homestead_machne_timer(NewMachine),
			homestead_server:homestead_uproot_seed(RoleID, Num);
		{false,ErrorCode}->
			?sendself(#sc_homestead_error{reason_code=ErrorCode})
	end.

cs_homestead_harvest(#cs_homestead_harvest{num=0})->
    case is_open_homestead() of
        true->
            RoleID = role_data:get_roleID(),
            MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
            lists:foreach(fun(E) ->
                                if
                                    E#p_homestead_machine.harvest /= 0 andalso E#p_homestead_machine.seedItemID /= 0 ->
                                        homestead_server:homestead_harvest(RoleID, E#p_homestead_machine.num);
                                    true ->
                                        ignore
                                end
                          end, MachineList);
        false->
            ?sendself(#sc_homestead_error{reason_code=?HOMESTEAD_ERROR_NOT_OPEN})
    end;
cs_homestead_harvest(#cs_homestead_harvest{num=Num})->
	RoleID = role_data:get_roleID(),
	case check_homestead_harvest(RoleID,Num) of
		true->
			homestead_server:homestead_harvest(RoleID, Num);
		{false,ErrorCode}->
			?sendself(#sc_homestead_error{reason_code=ErrorCode})
	end.
%%收成
homestead_harvest({homestead_harvest,HarvestType,CanHarvest0,Num,NewMachine})->
		RoleInfo = role_data:get_roleInfo(),
        CanHarvest = case activity_server:is_activity(?homestead_reward_double) of
                          true ->
                              CanHarvest0*2;
                          false ->
                              CanHarvest0
                      end,
        % 1金币2徽章3神奇果实4觉醒石5进阶石
        Reward1 = case HarvestType of
                     1 ->
                        [{?REWARD_COIN,CanHarvest}];
                     2 ->
                        [{?REWARD_REPU,CanHarvest}];
                     3 ->
                        [{?REWARD_ITEM,20035,CanHarvest}];
                     4 ->
                        [{?REWARD_ITEM,20043,CanHarvest}];
                     5 ->
                        [{?REWARD_ITEM,20044,CanHarvest}]
                 end,    
        role_reward:handle_sys_reward(RoleInfo, Reward1, ?MONEY_ADD_TYPE_HOMESTEAD_HARVEST, Num, ""),
        [Reward2|_] = role_reward:transform2p_reward_view(Reward1,[]),
        ?INFO("homestead_harvest ~w",[Reward2]),
		?sendself(#sc_homestead_harvest{updata_machine=NewMachine,reward=Reward2}).

%%播种(未播种时所有其他玩家都不会操作这个机器)
cs_homestead_seeding(#cs_homestead_seeding{num=0,seedItemID=SeedItemID})->
    RoleID = role_data:get_roleID(),
    case check_seeding_auto(SeedItemID) of
        {true,RoleInfo,BagOther,DelAcc,UpdateAcc,UpdateLogList,NewMachineList,TailMachineList} ->
            role_data:set_bagItem(BagOther),
            LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
            {Date, _} = Time = erlang:localtime(),
            behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_HOMESTEAD_SEEDING, SeedItemID, ""),
            %% 通知道具数量更新
            if UpdateAcc =/= [] ->
                   UpdateInfoList = 
                       lists:map(fun(Update) ->
                                         #p_item_num_update{itemNum=Update#item.itemNum,itemUID=Update#item.itemUID}
                                 end, UpdateAcc),
                   ?sendself(#sc_item_update{updateList=UpdateInfoList});
               true ->
                   ignore
            end,
            DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
            ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
            lists:foreach(fun(M)-> 
                            refresh_homestead_machne_timer(M)
                          end, NewMachineList),
			?CATCH(role_task_trigger:handle({dispach_task,home_seed,erlang:length(NewMachineList)})),
            erlang:send(homestead_server, {homestead_update_machine_auto,RoleID,NewMachineList});
        {false,ErrorCode} ->
            ?sendself(#sc_homestead_error{reason_code=ErrorCode})
    end;
cs_homestead_seeding(#cs_homestead_seeding{num=Num,seedItemID=SeedItemID})->
	RoleID = role_data:get_roleID(),
 	case check_seeding(RoleID,Num,SeedItemID) of
		{needItem,_,BagOther,DelAcc,UpdateAcc,UpdateLogList,NewMachine,_TailMachineList}->
			role_data:set_bagItem(BagOther),
			LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
			{Date, _} = Time = erlang:localtime(),
			behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_HOMESTEAD_SEEDING, SeedItemID, ""),
			%% 通知道具数量更新
			if UpdateAcc =/= [] ->
				   UpdateInfoList = 
					   lists:map(fun(Update) ->
										 #p_item_num_update{itemNum=Update#item.itemNum,itemUID=Update#item.itemUID}
								 end, UpdateAcc),
				   ?sendself(#sc_item_update{updateList=UpdateInfoList});
			   true ->
				   ignore
			end,
			DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
			?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
			refresh_homestead_machne_timer(NewMachine),
						?CATCH(role_task_trigger:handle({dispach_task,home_seed,1})),
			homestead_server:homestead_update_machine(RoleID, Num, NewMachine);
		{needGold,RoleInfo,NeedGold,NewMachine,_TailMachineList}->
			case NeedGold>0 of
				true->
					role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_HOMESTEAD_SEEDING, SeedItemID, "");
				false->
					ignore
			end,
			refresh_homestead_machne_timer(NewMachine),
			?CATCH(role_task_trigger:handle({dispach_task,home_seed,1})),
			homestead_server:homestead_update_machine(RoleID, Num, NewMachine);
		{false,ErrorCode}->
			?sendself(#sc_homestead_error{reason_code=ErrorCode})
	end.

cs_homestead_change_ger(#cs_homestead_change_ger{gerID=GerID})->
	RoleID = role_data:get_roleID(),
	case check_change_ger(RoleID,GerID) of
		{true,GerTypeID,GerLevel,GerQuality}->
%%             ?ERR("RoleID:~w,GerID:~w,GerTypeID:~w,GerLevel:~w,GerQuality:~w", [RoleID,GerID,GerTypeID,GerLevel,GerQuality]),
			homestead_server:homestead_change_ger(RoleID, GerID, GerTypeID, GerLevel, GerQuality);
		false->
			?sendself(#sc_homestead_error{reason_code=?HOMESTEAD_ERROR_GER_ALREADY_ON});
		{false,ErrorCode}->
			?sendself(#sc_homestead_error{reason_code=ErrorCode})
	end.

%% 种子合成
cs_homestead_compose(#cs_homestead_compose{itemTypeID=TypeID, num=ComposeNum}) ->
    SeedComposeList = data_homestead:get(data_seedcompose),
    case lists:keyfind(TypeID, 1, SeedComposeList) of
        false ->
            ?sendself(#sc_homestead_compose{result=2});
        {_, Num, GetNum, NewItem} ->
            case item_lib:check_material(TypeID, Num * ComposeNum) of
                false ->
                    ?sendself(#sc_homestead_compose{result=1});
                {true, BagOther, DelAcc, UpdateAcc, UpdateLogList} ->
                    role_data:set_bagItem(BagOther),
                    LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
                    {Date,_} = Time = erlang:localtime(),
                    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time,?MONEY_DEC_TYPE_SEED_COMPOSE,0,""),
                    %% 通知更新道具数量
                    if UpdateAcc =/= [] ->
                        UpdateInfoList = 
                            lists:map(fun(Update) ->
                                        #p_item_num_update{itemNum=Update#item.itemNum, itemUID=Update#item.itemUID}
                                     end, UpdateAcc),
                            ?sendself(#sc_item_update{updateList=UpdateInfoList});
                        true ->
                            ignore
                    end,
                    DelItemIDList = [E||#item{itemUID=E} <- DelAcc],
                    ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
                    if 
                    	is_record(NewItem,new_item)->
                    		NewItemList = [NewItem#new_item{itemNum=GetNum * ComposeNum}],%lists:foldl(fun(_, Acc) -> [NewItem|Acc] end, [], lists:seq(1, GetNum * ComposeNum)),
                    		item_lib:add_item_f(NewItemList, ?MONEY_DEC_TYPE_SEED_COMPOSE, 0, "");
                    		%%v3.4.0增加精灵碎片合成精灵
                    	is_record(NewItem,new_ger)->
                    		NewGerList = lists:duplicate(GetNum,NewItem),
                    		ger_lib:add_ger_list(NewGerList,?MONEY_DEC_TYPE_SEED_COMPOSE,0,"");
                    	true->
                    		?ERR("undefined Item:~w ~n",[NewItem])
                    end,
                    ?sendself(#sc_homestead_compose{result=0})
            end
    end.

%% 获得种子合成配方列表
cs_homestead_compose_list(_) ->
    SeedComposeList = data_homestead:get(data_seedcompose),
    InfoList = [{p_compose_info, CostTypeID, NeedNum, GetNum, TargetID} || {CostTypeID, NeedNum, GetNum, {new_item, TargetID, _, _, _}} <- SeedComposeList],
    ?sendself(#sc_homestead_compose_list{infolist=InfoList}).

%%自己给好友充能
homestead_addenergy_reward({homestead_addenergy_reward,FriendRoleID,MachineNum,NewS,NewAdd4Energy,HarvestType,BaseReward,NewEnergyTimes})->
	{_,_,_,Rate} = data_homestead:get(init_add_energy_data),
	Reward = ((BaseReward * Rate) div 100),
	LogType = ?MONEY_ADD_TYPE_HOMESTEAD_MATING,
	RewardList = [{HarvestType,Reward}],
	RewardView = role_reward:transform2p_reward_view(RewardList, []),
	RoleInfo = role_data:get_roleInfo(),
	role_reward:handle_sys_reward(RoleInfo, RewardList, LogType, 0, ""),
	?sendself(#sc_homestead_addenergy{roleID=FriendRoleID,add4Energy=NewAdd4Energy,addEnergyEndS=NewS,energyTimes=NewEnergyTimes,num=MachineNum,rewardList=RewardView}),
	?CATCH(role_task_trigger:handle({dispach_task,role_add_enargy_to_friend})).
homestead_mating({homestead_mating,FriendRoleID,NewMatingTimes,NewFCT,NewFAdd4Mating,GerTypeID,Quality,FGerTypeID,FQuality})->
	#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
	#data_ger{gerStar=FGerStar} = data_ger:get(FGerTypeID),
	Sum = GerStar + (Quality / 10) + FGerStar + (FQuality / 10),
	BreedList = data_homestead:get(homestead_mating_breed),
	BoxID = get_min_suitable(BreedList, Sum),
	MainGerTypeID = role_data:get_mainGerTypeID(),
	[R1|_] = data_box:get({BoxID, MainGerTypeID}),
	LogType = ?MONEY_ADD_TYPE_HOMESTEAD_MATING,
	RewardList = [util:random_one_from_weigh_list(R1)],
	RewardView = role_reward:transform2p_reward_view(RewardList, []),
	#role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
	role_reward:handle_sys_reward(RoleInfo, RewardList, LogType, 0, ""),
	?sendself(#sc_homestead_mating{fRoleID=FriendRoleID,matingCoolSecond=NewFCT,matingTimes=NewMatingTimes, rewardList=RewardView,add4mating=NewFAdd4Mating}),
	behavior_homestead_mating:log(RoleID),
	?CATCH(role_task_trigger:handle({dispach_task,role_mating_to_friend})).

hook_ger_delete(GerID,NewQualiry,NewLevel,DGerIDList)->
	RoleID = role_data:get_roleID(),
	case is_open_homestead() of
		true->
			#p_homestead{gerID=GerID1} = homestead_server:get_ets_homestead_base(RoleID),
			case GerID1 of
				GerID->
					homestead_server:homestead_ger_upquality(RoleID, NewQualiry, NewLevel);
				_->
					case lists:member(GerID1, DGerIDList) of
						true->
							homestead_server:homestead_ger_delete(RoleID);
						false->
							ignore
					end
			end;
		false->
			ignore
	end.
	

%%家园是否开启
is_open_homestead(Level)->
	MinLevel = data_homestead:get(homestead_open_level),
	Level>=MinLevel.
is_open_homestead()->
	#role{level=Level} = role_data:get_roleInfo(),
	MinLevel = data_homestead:get(homestead_open_level),
	Level>=MinLevel.

%%角色升级处理(自动解锁)
hook_role_levelup(RoleID,OldLevel,NewLevel)->
	case is_open_homestead(NewLevel) of
		true->
			case is_open_homestead(OldLevel) of
				true->%%新加入机器
					OldMachineList = homestead_server:get_ets_homestead_machineList(RoleID),
					AddMachineList = get_can_open_machine_list(OldMachineList,OldLevel, NewLevel),
					case AddMachineList of
						[]->
							ignore;
						_->
							?DEBUG("======新增========>>>>>~p",[AddMachineList]),
							lists:foreach(fun(AddMachine)->
												  homestead_server:homestead_add_machine(RoleID, AddMachine),
												  ?sendself(#sc_homestead_unlock_machine{machine=AddMachine})
										  end, AddMachineList)
					end;
				false->%%初始化家园数据
					#role{roleName=RoleName} = role_data:get_roleInfo(),
					HomesteadInfo = init_homestead_info(RoleName),
					AddMachineList = get_can_open_machine_list([],0, NewLevel),
					homestead_server:homestead_init_data(RoleID, HomesteadInfo,AddMachineList),
                    ?unicast(RoleID, #sc_homestead_get_info{baseinfo=HomesteadInfo,machineList=AddMachineList})
			end;
		false->
			ignore
	end.
%%武将升级
hook_ger_levelup(GerID,Level)->
	#role{roleID=RoleID,level=RoleLevel} = role_data:get_roleInfo(),
	case is_open_homestead(RoleLevel) of
		true->
			HomesteadInfo = homestead_server:get_ets_homestead_base(RoleID),
			case HomesteadInfo#p_homestead.gerID of
				GerID->
					RoleID = role_data:get_roleID(),
					homestead_server:homestead_ger_uplevel(RoleID,Level);
				_->
					ignore
			end;
		false->
			ignore
	end.

%%初始化家园数据
init_role_homestead(#role{roleID=RoleID,level=Level},LastLogoutTime)->
	case is_open_homestead(Level) of
		true->
			case homestead_server:get_ets_homestead_base(RoleID) of
				?undefined->%%其实这个分支可以没有
					hook_role_levelup(RoleID, 0, Level);
				#p_homestead{}->
					Date = erlang:date(),
					{NewHour,_,_} = time(),
					{OldDate,{OldHour,_,_}} = util:seconds_to_datetime(LastLogoutTime),
					S = util:datetime_to_seconds({Date,{0,0,0}}),
					S1 = util:datetime_to_seconds({OldDate,{0,0,0}}),
					IsRefreshMatingAdd = can_refresh_mating_add(OldHour,S-S1,NewHour),
					homestead_server:homestead_role_online(RoleID,Level,IsRefreshMatingAdd)
			end;
		false->
			ignore
	end.

hook_hour(Hour)->
	#role{roleID=RoleID,level=Level} = role_data:get_roleInfo(),
	case Hour of
		0->
			case is_open_homestead(Level) of
				true->
					homestead_server:homestead_refresh_1(RoleID);
				false->
					ignore
			end;
		_->
			ignore
	end,
	case lists:member(Hour, ?HOMESTEAD_REFRESH_MATING_TIMS_HOUR) of
		true->
			case is_open_homestead(Level) of
				true->
					homestead_server:homestead_refresh_2(RoleID);
				false->
					ignore
			end;
		false->
			ignore
	end.

%%计算是否刷新交配次数
can_refresh_mating_add(OldHour,InvDay,NewHour)->
	case InvDay of
		0->
			can_refresh_mating_add_1(?HOMESTEAD_REFRESH_MATING_TIMS_HOUR, OldHour, NewHour);
		1->
			case lists:any(fun(X)->
							  NewHour>=X
					  end, ?HOMESTEAD_REFRESH_MATING_TIMS_HOUR) of
				true->
					true;
				false->
					lists:any(fun(X)->
							  OldHour<X
					  end, ?HOMESTEAD_REFRESH_MATING_TIMS_HOUR)
			end;
		86400->
			case lists:any(fun(X)->
							  NewHour>=X
					  end, ?HOMESTEAD_REFRESH_MATING_TIMS_HOUR) of
				true->
					true;
				false->
					lists:any(fun(X)->
							  OldHour<X
					  end, ?HOMESTEAD_REFRESH_MATING_TIMS_HOUR)
			end;
		_->
			true
	end.

can_refresh_mating_add_1([],_,_)->
	false;
can_refresh_mating_add_1([H|TailList],OldHour,NewHour)->
	case NewHour>=H andalso OldHour < H of
		true->
			true;
		false->
			can_refresh_mating_add_1(TailList,OldHour,NewHour)
	end.

refresh_homestead_machine({refresh_homestead_machine,Machine})->
	refresh_homestead_machne_timer(Machine).

%%根据机器数据删除或加入定时器
refresh_homestead_machne_timer(#p_homestead_machine{num=Num,endSecond=EndSecond}=Machine)->
	case erlang:erase({homestead_machine_timer_ref,Num}) of
		?undefined->
			ignore;
		{Sec,Ref}->
			timer_wheel:cancel_plan({Sec,Ref})
	end,
	case EndSecond of
		0->
			Machine;
		_->
			Now = util:now(),
			#role{roleID=RoleID,level=Level} = role_data:get_roleInfo(),
			case EndSecond > Now of
				true->
					?DEBUG("=====加入成熟计时器==============",[]),
					{NewSec,NewRef} = timer_wheel:add_plan(EndSecond, fun()->
																			  homestead_server:homestead_machine_seed_mature(RoleID,Level,Num),
																			  erlang:erase({homestead_machine_timer_ref,Num})
														   end),
					erlang:put({homestead_machine_timer_ref,Num}, {NewSec,NewRef}),
					Machine;
				false->
					homestead_server:homestead_machine_seed_mature(RoleID, Level,Num),
					erlang:erase({homestead_machine_timer_ref,Num})
			end
	end.


%%====privete=======================================================

%%得到最小大于等于的之
get_min_suitable([],_)->
	0;
get_min_suitable([{Lv,R}|TailList],Level)->
    case Level=<Lv of
        true->
            R;
        false->
            case TailList of
                [] ->
                    R;
                _ ->
                    get_min_suitable(TailList,Level)
            end
    end.

init_homestead_info(RoleName)->
	{EnergyTimes,_,_,_} = data_homestead:get(init_add_energy_data),
	{MatingTimes,_,_} = data_homestead:get(init_mating_data),
	#p_homestead{roleName=RoleName,energyTimes=EnergyTimes,matingTimes=MatingTimes,refreshMatingSecond=util:now()}.

%%得到将要开启的
get_can_open_machine_list(OldMachineList,OldLevel,Level)->
	List = data_homestead:get(homestead_machine_list),
	lists:foldr(fun({Num,CL,_},Acc)->
						case CL>OldLevel andalso CL=<Level of
							true->
								case lists:keyfind(Num, #p_homestead_machine.num, OldMachineList) of
									false->
										[#p_homestead_machine{num=Num}|Acc];
									_->
										Acc
								end;
							false->
								Acc
						end
				end, [], List).

unlock_machine(RoleID,Num,RoleInfo,NeedGold)->
	role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_HOMESTEAD_UNLOCK, Num, ""),
	Machine = #p_homestead_machine{num=Num},
	homestead_server:homestead_add_machine(RoleID,Machine),
	%%机器的变化触发玩家机器数量变化任务
	% role_payGuide:trigger_task_change(?PLANTATION_HAS_N,{0}),
	?sendself(#sc_homestead_unlock_machine{machine=Machine}).

check_unlock_machine(RoleID,Num)->
	case is_open_homestead() of
		true->
			case get_machine_config(Num) of
				?undefined->
					{false,?HOMESTEAD_ERROR_NOT_MACHINE};
				{_,_,NeedGold}->
					MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
					case lists:keyfind(Num, #p_homestead_machine.num, MachineList) of
						false->
							case get_next_open_machine(RoleID) of
								Num->
									RoleInfo = role_data:get_roleInfo(),
									case role_lib:check_money(RoleInfo, gold, NeedGold) of
										true->
											{true,RoleInfo,NeedGold};
										false->
											{false,?HOMESTEAD_ERROR_GOLD_NOT_ENOUGH}
									end;
								_->
									{false,?HOMESTEAD_ERROR_NOT_UNLOCK_MACHINE}
							end;
						_->
							{false,?HOMESTEAD_ERROR_MACHINE_WAS_UNLOCK}
					end
			end;
		false->
			{false,?HOMESTEAD_ERROR_NOT_OPEN}
	end.


check_uproot_seed(RoleID,Num)->
	case is_open_homestead() of
		true->
			case get_machine_config(Num) of
				?undefined->
					{false,?HOMESTEAD_ERROR_NOT_MACHINE};
				_->
					MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
					case util:fun_take(fun(#p_homestead_machine{num=N})->N=:=Num end, MachineList) of
						false->
							{false,?HOMESTEAD_ERROR_MACHINE_NOT_UNLOCK};
						{value,#p_homestead_machine{harvest=Harvest,seedItemID=SeedItemID}=Machine,TailMachineList}->
							if
								SeedItemID=:=0 ->
									{false,?HOMESTEAD_ERROR_MACHINE_NOT_SEED};
								Harvest=/=0 ->
									{false,?HOMESTEAD_ERROR_UPROOT_SEED_HAS_HARVEST};
								true->
									{true,Machine,TailMachineList}
							end
					end
			end;
		false->
			{false,?HOMESTEAD_ERROR_NOT_OPEN}
	end.

check_seeding(RoleID,Num,SeedItemID)->
	case is_open_homestead() of
		true->
			case get_machine_config(Num) of
				?undefined->
					{false,?HOMESTEAD_ERROR_NOT_MACHINE};
				_->
                    RoleInfo = role_data:get_roleInfo(),
                    #role{familyID = FamilyID,level = RoleLevel} = RoleInfo,
                    Type = data_homestead_seed:get({seed_type,SeedItemID}),
					case data_homestead_seed:get({Type,RoleLevel}) of
						?undefined->
							{false,?HOMESTEAD_ERROR_NO_SEED};
						{_Harvest,Second,NeedGold}->
							% ?ERR("SeedItemID: ~w  NeedGold ~w ,Second ~w",[SeedItemID,NeedGold,Second]),
							MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
							case util:fun_take(fun(#p_homestead_machine{num=N})->N=:=Num end, MachineList) of
								false->
									{false,?HOMESTEAD_ERROR_MACHINE_NOT_UNLOCK};
								{value,#p_homestead_machine{harvest=Harvest,seedItemID=SID}=Machine,TailMachineList}->
									case Harvest=:=0 of
										true->
											case SID =:= 0 of
												true->
													%这个地方可以修改second实现种子成熟时间
													%此处修改产出时间
%% 													{_,_,Type,_} = data_homestead:get({homestead_machine_seed,SeedItemID}),
													FamilyTekAdd = case Type of
													%%金币产出
														1 ->
                                                            role_lib:calculate_familyTek_addbuff(FamilyID,2,1);
														%徽章产出
														2 ->
                                                            role_lib:calculate_familyTek_addbuff(FamilyID,1,1);
                                                        _ ->
                                                            0
													end,
													Second2 = role_lib:calculate_familyTekeffectTime(Second,FamilyTekAdd),
                                                    %Second2 = 120,
													Now=util:now(),
													NewMachine = Machine#p_homestead_machine{startSecond=Now,seedItemID=SeedItemID,endSecond=Now+Second2},
													check_sending_1(SeedItemID,NeedGold,TailMachineList,NewMachine);
												false->
													{false,?HOMESTEAD_ERROR_MACHINE_WAS_SEEDING}
											end;
										false->
											{false,?HOMESTEAD_ERROR_MACHINE_HAS_HARVEST}
									end
							end
					end
			end;
		false->
			{false,?HOMESTEAD_ERROR_NOT_OPEN}
	end.

check_seeding_auto(SeedItemID)->
    case is_open_homestead() of
        true->
            #role{roleID = RoleID,familyID = FamilyID,level = RoleLevel} = RoleInfo = role_data:get_roleInfo(),
            Type = data_homestead_seed:get({seed_type,SeedItemID}),
            case data_homestead_seed:get({Type,RoleLevel}) of
                ?undefined->
                    {false,?HOMESTEAD_ERROR_NO_SEED};
                {_Harvest,Second,NeedGold}->
                    % ?ERR("SeedItemID: ~w  NeedGold ~w ,Second ~w",[SeedItemID,NeedGold,Second]),
                    MachineList0 = homestead_server:get_ets_homestead_machineList(RoleID),
                    MachineList = lists:filter(fun(#p_homestead_machine{harvest=Harvest,seedItemID=SID})->
                                                       Harvest =:= 0 andalso SID =:= 0
                                               end, MachineList0),
                    MachineNum = erlang:length(MachineList),
                    SeedNum = item_lib:get_material_num(SeedItemID),
                    if
                        MachineNum =:= 0 ->
                            {false,?HOMESTEAD_ERROR_MACHINE_NOT_UNLOCK};
                        SeedNum =:= 0 ->
                            {false,?HOMESTEAD_ERROR_NO_SEED};
                        true ->
                            CanSeedNum = erlang:min(SeedNum, MachineNum),
                            %这个地方可以修改second实现种子成熟时间
                            %此处修改产出时间
                            FamilyTekAdd = case Type of
                            %%金币产出
                                1 ->
                                    role_lib:calculate_familyTek_addbuff(FamilyID,2,1);
                                %徽章产出
                                2 ->
                                    role_lib:calculate_familyTek_addbuff(FamilyID,1,1);
                                _ ->
                                    0
                            end,
%%                             Second2 = 120,
                            Second2 = role_lib:calculate_familyTekeffectTime(Second,FamilyTekAdd),
                            Now=util:now(),
                            {NewMachineList,TailMachineList,_} = 
                                lists:foldl(fun(M,{NewAccList,TailAccList,AccCanSeedNum}) -> 
                                                 #p_homestead_machine{harvest=Harvest,seedItemID=SID} = M,
                                                 if
                                                    Harvest =:= 0 andalso SID =:= 0 andalso AccCanSeedNum > 0->
                                                        {[M#p_homestead_machine{startSecond=Now,seedItemID=SeedItemID,endSecond=Now+Second2}|NewAccList]
                                                        ,TailAccList
                                                        ,AccCanSeedNum - 1};
                                                    true ->
                                                        {NewAccList,[M|TailAccList],AccCanSeedNum}
                                                 end
                                            end
                                         , {[],[],CanSeedNum}, MachineList0),
                            {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} = item_lib:check_material(SeedItemID, CanSeedNum),
                            {true,RoleInfo,BagOther2,DelAcc,UpdateAcc,UpdateLogList,NewMachineList,TailMachineList}
                    end
            end;
        false->
            {false,?HOMESTEAD_ERROR_NOT_OPEN}
    end.

check_sending_1(SeedItemID,NeedGold,TailMachineList,NewMachine)->
	RoleInfo = role_data:get_roleInfo(),
	case  item_lib:check_material(SeedItemID, 1) of
		{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList}->
			{needItem,RoleInfo,BagOther2,DelAcc,UpdateAcc,UpdateLogList,NewMachine,TailMachineList};
		false->
			case role_lib:check_money(RoleInfo, gold, NeedGold) of
				true->
					{needGold,RoleInfo,NeedGold,NewMachine,TailMachineList};
				false->
					{false,?HOMESTEAD_ERROR_GOLD_NOT_ENOUGH}
			end
	end.

check_homestead_harvest(RoleID,Num)->
	case is_open_homestead() of
		true->
			case get_machine_config(Num) of
				?undefined->
					{false,?HOMESTEAD_ERROR_NOT_MACHINE};
				_->
					MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
					case util:fun_take(fun(#p_homestead_machine{num=N})->N=:=Num end, MachineList) of
						false->
							{false,?HOMESTEAD_ERROR_MACHINE_NOT_UNLOCK};
						{value,#p_homestead_machine{harvest=Harvest,seedItemID=SeedItemID},_TailMachineList}->
							case SeedItemID=/=0 of
								true->
									case Harvest>0 of
										true->
												true;
										false->
											{false,?HOMESTEAD_ERROR_NO_MORE_HARVEST}
									end;
								false->
									{false,?HOMESTEAD_ERROR_MACHINE_NOT_SEED}
							end
					end
			end;
		false->
			{false,?HOMESTEAD_ERROR_NOT_OPEN}
	end.

check_change_ger(RoleID,GerID)->
	case is_open_homestead() of
		true->
			HomesteadInfo = homestead_server:get_ets_homestead_base(RoleID),
			CurrGerID = HomesteadInfo#p_homestead.gerID,
			case GerID of
				CurrGerID->
					false;%%返回false不处理
				0->
					{true,0,0,0};
				_->
					case role_data:get_ger(GerID) of
						false->
                            case role_ger:is_mirror_ger(GerID) of
                                false ->
                                    {false,?HOMESTEAD_ERROR_CHANGE_GER_NOT_GER};
                                true ->
                                    {false,?HOMESTEAD_ERROR_MIRROR}
                            end;
						{value, #ger{gerBase=GerBase}, _, _, _, _}->
							#gerBase{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality} = GerBase,
							{true,GerTypeID,GerLevel,GerQuality};
						{value, #gerSimple{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}, _, _, _, _}->
							{true,GerTypeID,GerLevel,GerQuality}
					end
			end;
		false->
			{false,?HOMESTEAD_ERROR_NOT_OPEN}
	end.

%%当前应该开启的机器(当前开启的机器最大编号+1)  如果这个编号在配置中找不到则全部已经开启
get_next_open_machine(RoleID)->
	OList = homestead_server:get_ets_homestead_machineList(RoleID),
	lists:max([Num||#p_homestead_machine{num=Num}<-OList])+1.


get_machine_config(Num)->
	ConfigMList = data_homestead:get(homestead_machine_list),
	case lists:keyfind(Num,1,ConfigMList) of
		false->
			?undefined;
		Info->
			Info
	end.


%%===============gm测试=================================

test_refresh_machine_endSecond(RoleID,Num,S)->
	role_lib:send_server(RoleID, {route, ?MODULE,{refresh_machine_endSecond,RoleID,Num,S}}).
test_refresh_machine_addEnergyEndS(RoleID,Num,S)->
	role_lib:send_server(RoleID, {route,?MODULE,{refresh_machine_addEnergyEndS,RoleID,Num,S}}).
test_refresh_matingCoolSecond(RoleID,S)->
	role_lib:send_server(RoleID, {route,?MODULE,{refresh_matingCoolSecond,RoleID,S}}).

%%重置所有次数
test_refresh_times(RoleID)->
	homestead_server:homestead_refresh_times(RoleID).
%%设置机器种子成熟时间为当前时间加S
refresh_machine_endSecond({refresh_machine_endSecond,RoleID,Num,S})->
	MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
	case lists:keyfind(Num, #p_homestead_machine.num, MachineList) of
		false->
			ignore;
		Machine->
			#p_homestead_machine{seedItemID=SeedItemID,endSecond=EndS} = Machine,
			case SeedItemID=/=0 andalso EndS=/=0 of
				true->
					NewMachine = Machine#p_homestead_machine{endSecond=util:now()+S},
					refresh_homestead_machne_timer(NewMachine),
					homestead_server:homestead_update_machine(RoleID, Num, NewMachine);
				false->
					ignore
			end
	end.
%%设置机器充能冷却结束时间为当前时间加S
refresh_machine_addEnergyEndS({refresh_machine_addEnergyEndS,RoleID,Num,S})->
	MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
	case lists:keyfind(Num, #p_homestead_machine.num, MachineList) of
		false->
			ignore;
		Machine->
			#p_homestead_machine{addEnergyEndS=AddEnergyEndS} = Machine,
			Now = util:now(),
			case AddEnergyEndS > Now+S of
				true->
					NewMachine = Machine#p_homestead_machine{addEnergyEndS=Now+S},
					refresh_homestead_machne_timer(NewMachine),
					homestead_server:homestead_update_machine(RoleID, Num, NewMachine);
				false->
					ignore
			end
	end.
%%设置交配冷却结束时间为当前时间加20
refresh_matingCoolSecond({refresh_matingCoolSecond,RoleID,S})->
	HomesteadInfo = homestead_server:get_ets_homestead_base(RoleID),
	#p_homestead{matingCoolSecond=MatingCoolSecond} = HomesteadInfo,
	Now = util:now(),
	case MatingCoolSecond > Now+S of
		true->
			NewHomestead = HomesteadInfo#p_homestead{matingCoolSecond=Now+S},
			homestead_server:homestead_update_base(RoleID, NewHomestead);
		false->
			ignore
	end.
