%% @author lihuachoa
%% @doc @todo 家园离线数据


-module(homestead_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_homestead.hrl").						

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).


start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit,true),
	dump_data(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
    {noreply, State}.

terminate(_Reason, _State) ->
	persist_change_homestead(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%机器新加入
homestead_add_machine(RoleID,Machine)->
	erlang:send(?MODULE, {homestead_add_machine,RoleID,Machine}).
%%铲除
homestead_uproot_seed(RoleID,Num)->
	erlang:send(?MODULE, {homestead_uproot_seed,RoleID,Num}).
%%收成
homestead_harvest(RoleID,Num)->
	erlang:send(?MODULE,{homestead_harvest,RoleID,Num}).
%%更新机器
homestead_update_machine(RoleID,Num,NewMachine)->
	erlang:send(?MODULE, {homestead_update_machine,RoleID,Num,NewMachine}).
%%换守护神
homestead_change_ger(RoleID,GerID,GerTypeID,GerLevel,GerQuality)->
	erlang:send(?MODULE, {homestead_change_ger,RoleID,GerID,GerTypeID,GerLevel,GerQuality}).

%%更新机器数据  主要用于测试
homestead_update_base(RoleID,HomesteadInfo)->
	erlang:send(?MODULE, {homestead_update_base,RoleID,HomesteadInfo}).
%%重置充能次数和交配次数  主要用于测试
homestead_refresh_times(RoleID)->
	erlang:send(?MODULE, {homestead_refresh_times,RoleID}).

%%守护神升阶升级或删除
homestead_ger_upquality(RoleID,NewQualiry,NewLevel)->
	erlang:send(?MODULE, {homestead_ger_upquality,RoleID, NewQualiry, NewLevel}).
homestead_ger_delete(RoleID)->
	erlang:send(?MODULE,{homestead_ger_delete,RoleID}).
homestead_ger_uplevel(RoleID,Level)->
	erlang:send(?MODULE, {homestead_ger_uplevel,RoleID,Level}).

%%初始化家园数据
homestead_init_data(RoleID, HomesteadInfo,AddMachineList)->
	erlang:send(?MODULE, {homestead_init_data,RoleID, HomesteadInfo,AddMachineList}).

%%上线
homestead_role_online(RoleID,RoleLevel,IsRefreshMatingAdd)->
	case role_homestead:is_open_homestead(RoleLevel) of
		true->
			erlang:send(?MODULE, {homestead_role_online,RoleID,RoleLevel,IsRefreshMatingAdd});
		false->
			ignore
	end.
homestead_role_offline(RoleID)->
	case role_homestead:is_open_homestead() of
		true->
			erlang:send(?MODULE, {homestead_role_offline,RoleID});
		false->
			ignore
	end.

homestead_machine_seed_mature(RoleID,RoleLevel,Num)->
	erlang:send(?MODULE, {homestead_machine_seed_mature,RoleID,RoleLevel,Num}).
%%充能
homestead_addenergy(RoleID,FriendRoleID,MachineNum)->
	erlang:send(?MODULE,{homestead_addenergy,RoleID,FriendRoleID,MachineNum}).
%%交配
homestead_mating(RoleID,FriendRoleID)->
	erlang:send(?MODULE,{homestead_mating,RoleID,FriendRoleID}).
%%重置充能次数和交配成果
homestead_refresh_1(RoleID)->
	erlang:send(?MODULE,{homestead_refresh_1,RoleID}).
%%重置交配次数
homestead_refresh_2(RoleID)->
	erlang:send(?MODULE,{homestead_refresh_2,RoleID}).
%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info({homestead_init_data,RoleID, HomesteadInfo,AddMachineList})->
	?DEBUG("===============~p",[{RoleID, HomesteadInfo,AddMachineList}]),
	update_ets_homestead_base(RoleID, HomesteadInfo),
	update_ets_homestead_machine(RoleID, AddMachineList),
	update_ets_homestead_log(RoleID, []),
	role_payGuide:asyn_trigger_task_change(RoleID,?PLANTATION_HAS_N,{0});
do_handle_info({homestead_add_machine,RoleID,Machine})->
	MachineList = get_ets_homestead_machineList(RoleID),
	NewMachineList = [Machine|MachineList],
	update_ets_homestead_machine(RoleID, NewMachineList),
	role_payGuide:asyn_trigger_task_change(RoleID,?PLANTATION_HAS_N,{0});
do_handle_info({homestead_uproot_seed,RoleID,Num})->
	MachineList = get_ets_homestead_machineList(RoleID),
	{value,Machine,TailMachineList} = util:fun_take(fun(#p_homestead_machine{num=N})->N=:=Num end, MachineList),
	NewMachine = Machine#p_homestead_machine{num=Num,endSecond=0,seedItemID=0,addEnergyEndS=0,add4Energy=0},
	NewMachineList = [NewMachine|TailMachineList],
	update_ets_homestead_machine(RoleID, NewMachineList),
	sync_add_enagy_to_friend(RoleID,MachineList,NewMachineList),
	?unicast(RoleID,#sc_homestead_uproot_seed{num=Num});
do_handle_info({homestead_harvest,RoleID,Num})->
	MachineList = get_ets_homestead_machineList(RoleID),
	{value,Machine,TailMachineList} = util:fun_take(fun(#p_homestead_machine{num=N})->N=:=Num end, MachineList),
	#p_homestead_machine{harvest=Harvest,seedItemID=SeedItemID,get_count=GetCount} = Machine,
	case SeedItemID=/=0 andalso Harvest>0 of
		true->
			NewGetCount = GetCount+1,
			case NewGetCount>=?HOMESTEAD_GET_HARVEST_COUNT of
				true->
					NewMachine = Machine#p_homestead_machine{harvest=0,seedItemID=0},
					CanHarvest = Harvest;
				false->
					CanHarvest = util:floor((1 / (?HOMESTEAD_GET_HARVEST_COUNT-GetCount)) * Harvest),
					NewMachine = Machine#p_homestead_machine{harvest=Harvest - CanHarvest,get_count=NewGetCount}
			end,
			NewMachineList = [NewMachine|TailMachineList],
			update_ets_homestead_machine(RoleID, NewMachineList),
%% 			{_,_,HarvestType,_} = data_homestead:get({homestead_machine_seed,SeedItemID}),
            HarvestType = data_homestead_seed:get({seed_type,SeedItemID}),
			role_lib:send_server(RoleID, {route, role_homestead,{homestead_harvest,HarvestType,CanHarvest,Num,NewMachine}});
		false->
			ignore
	end;
	
do_handle_info({homestead_update_machine,RoleID,Num,NewMachine})->
	MachineList = get_ets_homestead_machineList(RoleID),
	NewMachineList = lists:keyreplace(Num, #p_homestead_machine.num, MachineList, NewMachine),
	update_ets_homestead_machine(RoleID, NewMachineList),
	?unicast(RoleID,#sc_homestead_update_machine{updata_machine=NewMachine}),
	sync_add_enagy_to_friend(RoleID,MachineList,NewMachineList);

do_handle_info({homestead_update_machine_auto,RoleID,ChangeMachineList})->
    MachineList = get_ets_homestead_machineList(RoleID),
%%     NewMachineList = lists:keyreplace(Num, #p_homestead_machine.num, MachineList, NewMachine),
    NewMachineList 
        = lists:foldl(fun(M,AccList)-> 
                              case lists:keysearch(M#p_homestead_machine.num, #p_homestead_machine.num, ChangeMachineList) of
                                  {value,ChangeMachine} ->
                                      ?unicast(RoleID,#sc_homestead_update_machine{updata_machine=ChangeMachine}),
                                      [ChangeMachine|AccList];
                                  false ->
                                      [M|AccList]
                              end
                      end, [], MachineList),
    update_ets_homestead_machine(RoleID, NewMachineList),
    sync_add_enagy_to_friend(RoleID,MachineList,NewMachineList);

do_handle_info({homestead_change_ger,RoleID,GerID,GerTypeID,GerLevel,GerQuality})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	NewHomesteadInfo = HomesteadInfo#p_homestead{gerTypeID=GerTypeID,gerID=GerID,quality=GerQuality,level=GerLevel},
	update_ets_homestead_base(RoleID, NewHomesteadInfo),
	?unicast(RoleID,#sc_homestead_change_ger{gerID=GerID}),
	Record = #sc_homestead_sync_ger{roleID=RoleID,gerTypeID=GerTypeID,gerQuality=GerQuality},
    role_task:send_dispach(RoleID, {dispach_task, role_family_collect_ger}),
	?CATCH(friend_server:send_to_friend(RoleID, Record));
 do_handle_info({homestead_ger_upquality,RoleID, NewQualiry, NewLevel})->
	 HomesteadInfo = get_ets_homestead_base(RoleID),
	 NewHomesteadInfo = HomesteadInfo#p_homestead{quality=NewQualiry,level=NewLevel},
	 #p_homestead{gerTypeID=GerTypeID} = HomesteadInfo,
	 Record = #sc_homestead_sync_ger{roleID=RoleID,gerTypeID=GerTypeID,gerQuality=NewQualiry},
	?CATCH(friend_server:send_to_friend(RoleID, Record)),
	 update_ets_homestead_base(RoleID, NewHomesteadInfo);
 do_handle_info({homestead_ger_delete,RoleID})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	 NewHomesteadInfo = HomesteadInfo#p_homestead{gerID=0,gerTypeID=0,quality=0,level=0},
	 update_ets_homestead_base(RoleID, NewHomesteadInfo),
	Record = #sc_homestead_sync_ger{roleID=RoleID,gerTypeID=0,gerQuality=0},
	?CATCH(friend_server:send_to_friend(RoleID, Record));
do_handle_info({homestead_ger_uplevel,RoleID,Level})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	NewHomesteadInfo = HomesteadInfo#p_homestead{level=Level},
	update_ets_homestead_base(RoleID, NewHomesteadInfo);

 do_handle_info({homestead_machine_seed_mature,RoleID,RoleLevel,Num})->
	MachineList = get_ets_homestead_machineList(RoleID),
	{value,Machine,TailMachineList} = util:fun_take(fun(#p_homestead_machine{num=N})->N=:=Num end, MachineList),
	HomesteadInfo = get_ets_homestead_base(RoleID),
	NewMachine = machine_seed_mature(RoleLevel,HomesteadInfo,Machine,RoleID),
	NewMachineList = [NewMachine|TailMachineList],
	?DEBUG("=========种子成熟=======>>>~p",[NewMachine]),
	update_ets_homestead_machine(RoleID,NewMachineList),
	?unicast(RoleID,#sc_homestead_update_machine{updata_machine=NewMachine});
 do_handle_info(dump_data)->
	dump_data(),
	persist_change_homestead();
do_handle_info({homestead_role_online,RoleID,RoleLevel,IsRefreshMatingAdd})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	MachineList = get_ets_homestead_machineList(RoleID),
	NewMachineList = refresh_homestead_machine(RoleID,RoleLevel,HomesteadInfo,MachineList),
	IsRefresh = is_need_refresh_mating(HomesteadInfo#p_homestead.refreshMatingSecond),
	NewHomesteadInfo1 =
		case IsRefresh of
			true->
				refresh_homestead_1(HomesteadInfo);
			false->
				HomesteadInfo
		end,
	NewHomesteadInfo =
		case IsRefreshMatingAdd of
			true->
				refresh_homestead_2(NewHomesteadInfo1);
			false->
				NewHomesteadInfo1
		end,
	case IsRefresh orelse IsRefreshMatingAdd of
		true->
			update_ets_homestead_base(RoleID, NewHomesteadInfo),
			?unicast(RoleID,#sc_homestead_get_info{baseinfo=NewHomesteadInfo,machineList=NewMachineList});
		false->
			ignore
	end,
	update_ets_homestead_machine(RoleID, NewMachineList);
do_handle_info({homestead_role_offline,RoleID})->
	persist_role_homestead(RoleID),
	%%暂时可以不用
	%erase_ets_homestead(RoleID),
	ok;
do_handle_info({homestead_addenergy,RoleID,FriendRoleID,MachineNum})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	#p_homestead{roleName=RoleName,energyTimes=EnergyTimes} = HomesteadInfo,
	FriendMachineList = get_ets_homestead_machineList(FriendRoleID),
	case check_homestead_addenergy(EnergyTimes, MachineNum, FriendMachineList) of
		{true,FMachine,FTailMachineList}->
			{_,S,Add,_} = data_homestead:get(init_add_energy_data),
			Now = util:now(),
			NewS = Now + S,
			#p_homestead_machine{add4Energy=Add4Energy,seedItemID=SeedItemID} = FMachine,
			NewAdd4Energy = Add4Energy + Add,
			NewFMachine = FMachine#p_homestead_machine{add4Energy=NewAdd4Energy,addEnergyEndS=NewS},
			NewFMachineList = [NewFMachine|FTailMachineList],
			FLogList = get_ets_homestead_logList(FriendRoleID),
			Log = #p_homestead_log{second=Now,add=Add,roleName=RoleName,type=1,machine=MachineNum,gerName=""},
			NewFLogList = lists:sublist([Log|FLogList],20),
			
			NewEnergyTimes = EnergyTimes - 1,
			NewHomesteadInfo = HomesteadInfo#p_homestead{energyTimes=NewEnergyTimes},
			
			homestead_server:update_ets_homestead_base(RoleID, NewHomesteadInfo),
			homestead_server:update_ets_homestead_machine(FriendRoleID, NewFMachineList),
			homestead_server:update_ets_homestead_log(FriendRoleID, NewFLogList),
			
			#rolePublic{level=Level} = role_lib:get_rolePublic(RoleID),
			BaseReward = calculate_homestead_seed_base(SeedItemID,Level),
			{_,_,HarvestType,_} = data_homestead:get({homestead_machine_seed,SeedItemID}),
			?unicast(FriendRoleID,#sc_homestead_addenergy_to_friend{num=MachineNum,log=Log,addEnergyEndS=NewS,add4Energy=NewAdd4Energy}),
			role_lib:send_server(RoleID, {route, role_homestead, {homestead_addenergy_reward,FriendRoleID,MachineNum,NewS,NewAdd4Energy,HarvestType,BaseReward,NewEnergyTimes}}),
			sync_add_enagy_to_friend(FriendRoleID,FriendMachineList,NewFMachineList);
		{false,ErrorCode}->
			?unicast(RoleID,#sc_homestead_error{reason_code=ErrorCode})
	end;
do_handle_info({homestead_mating,RoleID,FriendRoleID})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	FHomesteadInfo = get_ets_homestead_base(FriendRoleID),
	#p_homestead{gerID=GerID,matingTimes=MatingTimes,gerTypeID=GerTypeID,quality=Quality,roleName=RoleName} = HomesteadInfo,
	#p_homestead{gerID=FGerID,matingCoolSecond=FMatingCoolSecond,gerTypeID=FGerTypeID,quality=FQuality} = FHomesteadInfo,
	case check_homestead_mating(GerID, MatingTimes, FMatingCoolSecond, FGerID) of
		true->
			Now = util:now(),
			case is_need_refresh_mating(FHomesteadInfo#p_homestead.refreshMatingSecond) of
				false ->
					FHomesteadInfo2=FHomesteadInfo;
				true ->
					FHomesteadInfo2=refresh_homestead_1(FHomesteadInfo)
			end,
			#p_homestead{add4mating=FAdd4Mating} = FHomesteadInfo2,
			NewMatingTimes = MatingTimes - 1,
			{_,S,Add} = data_homestead:get(init_mating_data),
			NewFAdd4Mating = FAdd4Mating + Add,
			NewFCT = Now + S,
			NewHomesteadInfo = HomesteadInfo#p_homestead{matingTimes=NewMatingTimes},
			NewFHomesteadInfo = FHomesteadInfo2#p_homestead{add4mating=NewFAdd4Mating,matingCoolSecond=NewFCT},
			% 日志显示功能已经在客户端删除了 v260
			LogRecord = #p_homestead_log{second=Now,add=Add,roleName=RoleName,type=2,gerName=homestead_server:get_gerName(FGerTypeID, FQuality),machine=0},
			FLogList = get_ets_homestead_logList(FriendRoleID),
			NewFLogList = lists:sublist([LogRecord|FLogList],20),
			
			update_ets_homestead_base(RoleID, NewHomesteadInfo),
			update_ets_homestead_base(FriendRoleID, NewFHomesteadInfo),
			update_ets_homestead_log(FriendRoleID, NewFLogList),
			Record = #sc_homestead_sync_mating_cool_second{roleID=FriendRoleID,matingCoolSecond=NewFCT},
			?CATCH(friend_server:send_to_friend(FriendRoleID, Record)),
			?unicast(FriendRoleID,#sc_homestead_mating_to_friend{log=LogRecord,matingCoolSecond=NewFCT,add4mating=NewFAdd4Mating}),
			role_lib:send_server(RoleID, {route, role_homestead, {homestead_mating,FriendRoleID,NewMatingTimes,NewFCT,NewFAdd4Mating,GerTypeID,Quality,FGerTypeID,FQuality}}),

            %%触发成就
            role_task:send_dispach(RoleID,{dispach_task,role_gen_match});
		{false,ErrorCode}->
			?unicast(RoleID,#sc_homestead_error{reason_code=ErrorCode})
	end;
do_handle_info({homestead_refresh_1,RoleID})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	NewHomesteadInfo = refresh_homestead_1(HomesteadInfo),
	update_ets_homestead_base(RoleID, NewHomesteadInfo),
	MachineList = get_ets_homestead_machineList(RoleID),
	?unicast(RoleID,#sc_homestead_get_info{baseinfo=NewHomesteadInfo,machineList=MachineList});
do_handle_info({homestead_refresh_2,RoleID})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	NewHomesteadInfo = refresh_homestead_2(HomesteadInfo),
	update_ets_homestead_base(RoleID, NewHomesteadInfo),
	MachineList = get_ets_homestead_machineList(RoleID),
	?unicast(RoleID,#sc_homestead_get_info{baseinfo=NewHomesteadInfo,machineList=MachineList});
do_handle_info({homestead_update_base,RoleID,HomesteadInfo})->
	update_ets_homestead_base(RoleID, HomesteadInfo),
	MachineList = get_ets_homestead_machineList(RoleID),
	?unicast(RoleID,#sc_homestead_get_info{baseinfo=HomesteadInfo,machineList=MachineList});
do_handle_info({homestead_refresh_times,RoleID})->
	HomesteadInfo = get_ets_homestead_base(RoleID),
	{EnergyTimes,_,_,_} = data_homestead:get(init_add_energy_data),
	{MatingTimes,_,_} = data_homestead:get(init_mating_data),
	NewHomesteadInfo = HomesteadInfo#p_homestead{energyTimes=EnergyTimes,matingTimes=MatingTimes},
	update_ets_homestead_base(RoleID, NewHomesteadInfo),
	MachineList = get_ets_homestead_machineList(RoleID),
	?unicast(RoleID,#sc_homestead_get_info{baseinfo=NewHomesteadInfo,machineList=MachineList});
do_handle_info({update_role_name, RoleID, Name}) ->
    case ets:lookup(?ETS_HOMESTEAD_DATA_BASE_TABLE, RoleID) of
        [] ->
            ignore;
        [{_,HomesteadInfo}] ->
            NewHomesteadInfo = HomesteadInfo#p_homestead{roleName=Name},
            update_ets_homestead_base(RoleID, NewHomesteadInfo)
    end;
do_handle_info(_Info)->
	?ERR("无效消息:~p",[_Info]).

%%重置充能次数和交配成果
refresh_homestead_1(HomesteadInfo)->
	{EnergyTimes,_,_,_} = data_homestead:get(init_add_energy_data),
	HomesteadInfo#p_homestead{add4mating=0,energyTimes=EnergyTimes,refreshMatingSecond=util:now()}.
%%重置交配次数
refresh_homestead_2(HomesteadInfo)->
	{MatingTimes,_,_} = data_homestead:get(init_mating_data),
	HomesteadInfo#p_homestead{matingTimes=MatingTimes}.

%%根据机器数据删除或加入定时器
refresh_homestead_machine(RoleID,RoleLevel,HomesteadInfo,MachineList)->
	Fun = fun(#p_homestead_machine{endSecond=EndSecond}=Machine)->
				  case EndSecond of
					  0->
						  Machine;
					  _->
						  Now = util:now(),
						  case EndSecond > Now of
							  true->
								  %%通知玩家进程加入计时器
								  ?CATCH(role_lib:send_server(RoleID, {route, role_homestead,{refresh_homestead_machine,Machine}})),
								  Machine;
							  false->
								  machine_seed_mature(RoleLevel,HomesteadInfo,Machine,RoleID)
						  end
				  end
		  end,
	[Fun(Machine)||Machine<-MachineList].

get_gerName(GerTypeID,Quality)->
	#data_ger{gerName=GerName} = data_ger:get(GerTypeID),
	case Quality of
		0->
			GerName;
		_->
			GerName ++ "+"++integer_to_list(Quality)
	end.
%%=================================================================
%% 持久化改变了的
dump_data() ->
	erlang:send_after(60*1000, self(), dump_data).

%%得到缓存的交配冷却时间（缓存所有开启家园的交配冷却时间）
get_ets_homestead_mating_cooltime(RoleID,Level)->
	case role_homestead:is_open_homestead(Level) of
		true->
			case get_ets_homestead_base(RoleID) of
				?undefined->
					-1;
				#p_homestead{matingCoolSecond=MCS,gerID=GerID}->
					case GerID of
						0->
							-1;
						_->
							MCS
					end
			end;
		false->
			-1
	end.

get_ets_homestead_friend_info(RoleID,Level)->
	case role_homestead:is_open_homestead(Level) of
		true->
			case get_ets_homestead_base(RoleID) of
				?undefined->
					{0,0,0,0,0,0,0};
				HomesteadInfo->
					#p_homestead{matingCoolSecond=MCS,gerTypeID=GerTypeID,quality=GerQuality} = HomesteadInfo,
					MachineList = get_ets_homestead_machineList(RoleID),
					 {BeginGold,EndGold,BeginBadge,EndBadge} = get_ets_homestead_enargy_time(MachineList),
					{MCS,GerTypeID,GerQuality,BeginGold,EndGold,BeginBadge,EndBadge}
			end;
		false->
			{0,0,0,0,0,0,0}
	end.

sync_add_enagy_to_friend(RoleID,OldMachieList,NewMachieList)->
	Info1 = get_ets_homestead_enargy_time(OldMachieList),
	{NewBeginGold,NewEndGold,NewBeginBadge,NewEndBadge} = Info2 = get_ets_homestead_enargy_time(NewMachieList),
	case Info1 =/= Info2 of
		true->
			Record = #sc_homestead_sync_add_enagy{roleID=RoleID,beginBadge=NewBeginBadge,beginGold=NewBeginGold,endBadge=NewEndBadge,endGold=NewEndGold},
			?CATCH(friend_server:send_to_friend(RoleID, Record));
		false->
			ignore
	end.
	

get_ets_homestead_enargy_time(MachineList)->
	lists:foldl(fun(#p_homestead_machine{seedItemID=SeedItemID,harvest=Harvest,addEnergyEndS=AddEnargyEndS,endSecond=EndSecond},{B1,N1,B2,N2}=Acc)->
						if
							SeedItemID=:=0 orelse Harvest=/=0->
								Acc;
							true->
								case data_homestead:get({homestead_machine_seed,SeedItemID}) of
									?undefined->
										Acc;
									{_,_,HarvestType,_}->
										case HarvestType of
											1->
												NewB1 = 
													case B1=:=0 of
														true->
															AddEnargyEndS;
														false->
															erlang:min(AddEnargyEndS, B1)
													end,
												NewN1 = erlang:max(EndSecond, N1),
												{NewB1,NewN1,B2,N2};
											3->
												NewB2 = 
													case B2=:=0 of
														true->
															AddEnargyEndS;
														false->
															erlang:min(AddEnargyEndS, B2)
													end,
												NewN2 = erlang:max(EndSecond, N2),
												{B1,N1,NewB2,NewN2}
										end
								end
						end
				end, {0,0,0,0}, MachineList).

%%检测GerList是否包含了守护武将
has_homestead_ger(RoleID,GerList)->
    case get_ets_homestead_base(RoleID) of
        ?undefined ->
            false;
        #p_homestead{gerID=GerID} ->
            case GerID of
                0->
                    false;
                _->
                    lists:member(GerID, GerList)
            end
    end.

get_homestead_ger(RoleID)->
	case get_ets_homestead_base(RoleID) of
        ?undefined ->
            false;
        #p_homestead{gerID=GerID} ->
			GerID
	end.

%%得到缓存家园基础数据
get_ets_homestead_base(RoleID)->
	case ets:lookup(?ETS_HOMESTEAD_DATA_BASE_TABLE, RoleID) of
		[]->
			case db_sql:get_homestead(RoleID) of
				?undefined->
					?undefined;
				{HomesteadInfo,MachineList0,LogList} ->
                    TimeFilter = data_homestead_seed:get(time_filter),
                    MachineList = lists:foldr(fun(M,AccList)-> 
                                                      SeedConfig = data_homestead_seed:get({seed_type,M#p_homestead_machine.seedItemID}),
                                                      if
                                                          SeedConfig =:= ?undefined ->
                                                              [#p_homestead_machine{num=M#p_homestead_machine.num}|AccList];
                                                          M#p_homestead_machine.startSecond /= 0 andalso
                                                            M#p_homestead_machine.startSecond =< TimeFilter ->
                                                              [#p_homestead_machine{num=M#p_homestead_machine.num}|AccList];
                                                          true ->
                                                              [M|AccList]
                                                      end
                                              end, [], MachineList0),
					ets:insert(?ETS_HOMESTEAD_DATA_BASE_TABLE, {RoleID,HomesteadInfo}),
					ets:insert(?ETS_HOMESTEAD_DATA_MACHINE_TABLE, {RoleID,MachineList}),
					ets:insert(?ETS_HOMESTEAD_DATA_LOG_TABLE, {RoleID,LogList}),
					HomesteadInfo
			end;
		[{_,HomesteadInfo}]->
			HomesteadInfo
	end.
get_ets_homestead_machineList(RoleID)->
	case ets:lookup(?ETS_HOMESTEAD_DATA_MACHINE_TABLE, RoleID) of
		[]->
			[];
		[{_,MachineList0}]->
            TimeFilter = data_homestead_seed:get(time_filter),
			lists:foldr(fun(M,AccList)-> 
	                      SeedConfig = data_homestead_seed:get({seed_type,M#p_homestead_machine.seedItemID}),
	                      if
	                          SeedConfig =:= ?undefined ->
	                              [#p_homestead_machine{num=M#p_homestead_machine.num}|AccList];
	                          M#p_homestead_machine.startSecond /= 0 andalso
	                            M#p_homestead_machine.startSecond =< TimeFilter ->
	                              [#p_homestead_machine{num=M#p_homestead_machine.num}|AccList];
	                          true ->
	                              [M|AccList]
	                      end
	              end, [], MachineList0)
	end.
get_ets_homestead_logList(RoleID)->
	case ets:lookup(?ETS_HOMESTEAD_DATA_LOG_TABLE, RoleID) of
		[]->
			[];
		[{_,LogList}]->
			LogList
	end.

is_need_refresh_mating(Timestamp)->
	{NowDate,_}=util:seconds_to_datetime(util:now()),
 	util:datetime_to_seconds({NowDate,{0,0,0}}) > Timestamp.

update_ets_homestead_base(RoleID,HomesteadInfo)->
	ets:insert(?ETS_HOMESTEAD_DATA_BASE_TABLE, {RoleID,HomesteadInfo}),
	mark_role_homestead_change(RoleID).
update_ets_homestead_machine(RoleID,MachineList)->
	ets:insert(?ETS_HOMESTEAD_DATA_MACHINE_TABLE, {RoleID,MachineList}),
	mark_role_homestead_change(RoleID).
update_ets_homestead_log(RoleID,LogList)->
	ets:insert(?ETS_HOMESTEAD_DATA_LOG_TABLE, {RoleID,LogList}),
	mark_role_homestead_change(RoleID).

erase_ets_homestead(RoleID)->
	ets:delete(?ETS_HOMESTEAD_DATA_BASE_TABLE,RoleID),
	ets:delete(?ETS_HOMESTEAD_DATA_MACHINE_TABLE,RoleID),
	ets:delete(?ETS_HOMESTEAD_DATA_LOG_TABLE,RoleID).


mark_role_homestead_change(RoleID)->
	case get(?HOMESTEAD_DATA_CHANGE_ROLEID_LIST) of
		?undefined->
			put(?HOMESTEAD_DATA_CHANGE_ROLEID_LIST,[RoleID]);
		List->
			case lists:member(RoleID,List) of
				true->
					true;
				false->
					put(?HOMESTEAD_DATA_CHANGE_ROLEID_LIST,[RoleID|List])
			end
	end.

persist_change_homestead()->
		case erase(?HOMESTEAD_DATA_CHANGE_ROLEID_LIST) of
			?undefined->
				ignore;
			[]->
				ignore;
			List->
				Fun = fun()->
							  lists:map(fun(RoleID)->
												HomesteadInfo = get_ets_homestead_base(RoleID),
												MachineList = get_ets_homestead_machineList(RoleID),
												LogList = get_ets_homestead_logList(RoleID),
												db_sql:set_homestead(RoleID, {HomesteadInfo,MachineList,LogList})
										end, List)
					  end,
				spawn(Fun)
		end.

persist_role_homestead(RoleID)->
	case get(?HOMESTEAD_DATA_CHANGE_ROLEID_LIST) of
		?undefined->
			ignore;
		[]->
			ignore;
		List->
			case lists:member(RoleID, List) of
				true->
					put(?HOMESTEAD_DATA_CHANGE_ROLEID_LIST,lists:delete(RoleID, List)),
					HomesteadInfo = get_ets_homestead_base(RoleID),
					MachineList = get_ets_homestead_machineList(RoleID),
					LogList = get_ets_homestead_logList(RoleID),
					ets:delete(?ETS_HOMESTEAD_DATA_BASE_TABLE, RoleID),
					ets:delete(?ETS_HOMESTEAD_DATA_MACHINE_TABLE, RoleID),
					ets:delete(?ETS_HOMESTEAD_DATA_LOG_TABLE, RoleID),
					db_sql:set_homestead(RoleID, {HomesteadInfo,MachineList,LogList});
				false->
					ignore
			end
	end.



machine_seed_mature(Level,HomesteadInfo,Machine,RoleID)->
	#p_homestead_machine{seedItemID=SeedItemID,add4Energy=Add4Energy} = Machine,
    Type = data_homestead_seed:get({seed_type,SeedItemID}),
%% 	{_,_,Type,_} = data_homestead:get({homestead_machine_seed,SeedItemID}),
    #rolePublic{familyID =FamilyID} = role_lib:get_rolePublic(RoleID),
	FamilyTek_Add = case Type of
		%%金币产出
		1 ->
            calculate_familyTek_addbuff(FamilyID,11,1);
		%徽章产出
		2 ->
            calculate_familyTek_addbuff(FamilyID,10,1);
        _ ->
            0
	end,
	Base = calculate_homestead_seed_base(SeedItemID,Level),
	BaseAdd = calculate_homestead_base_add(HomesteadInfo),
%% 2.3.0:
%% 最终产出=基础产量＊（1+守护加成+充能加成+建筑等级加成）＊（1+公会科技加成）
%% 建筑等级加成=建筑等级＊家园阶段＊N%    其中暂定：N=0.5
    HomesteadBuff0 = home_server:get_homestead_buff(RoleID),
    HomesteadBuff = if
            erlang:is_integer(HomesteadBuff0) andalso HomesteadBuff0 >= 0 ->
                HomesteadBuff0;
            true ->
                0
        end,
    %HomeBossAdd = home_boss_server:get_homestead_buff(RoleID),
    ?INFO("machine_seed_mature ~w",[{Base,BaseAdd,Add4Energy,HomesteadBuff,FamilyTek_Add}]),
	Harvest = Base + erlang:trunc(Base * (BaseAdd + Add4Energy + HomesteadBuff) div 100),
	Harvest2 = role_lib:calculate_familyTekeffectGenerate(Harvest,FamilyTek_Add),
	Machine#p_homestead_machine{endSecond=0,harvest=Harvest2,get_count=0,add4Energy=0,addEnergyEndS=0}.

%%计算种子固定产量
calculate_homestead_seed_base(SeedItemID,Level)->
%% 	{_,_,_,AddList} = data_homestead:get({homestead_machine_seed,SeedItemID}),
%% 	role_homestead:get_min_suitable(AddList, Level).
    Type = data_homestead_seed:get({seed_type,SeedItemID}),
    {H,_,_} = data_homestead_seed:get({Type,Level}),
    H.

%%计算守护神加成、爱心加成、充能加成总和
calculate_homestead_base_add(#p_homestead{add4mating=Add4Mating,gerTypeID=GerTypeID})->
	case GerTypeID of
		0->
			Add4Mating;
		_->
			case data_ger:get(GerTypeID) of
				?undefined->
					Add4Mating;
				#data_ger{gerStar=Star}->
					case data_homestead:get({homestead_ger_output_add,Star}) of
						?undefined->
							Add4Mating;
						Add->
							Add+Add4Mating
					end
			end
	end.

check_homestead_addenergy(EnergyTimes,MachineNum,FriendMachineList)->
	case EnergyTimes =< 0 of
		true->
			{false,?HOMESTEAD_ERROR_ADDENERGY_TIMES_NOT_ENOUGH};
		false->
			case util:fun_take(fun(#p_homestead_machine{num=N})->N=:=MachineNum end, FriendMachineList) of
				false->
					{false,?HOMESTEAD_ERROR_FRIEND_MACHIE_UNLOCK};
				{value,#p_homestead_machine{addEnergyEndS=AddEnergyEnds,endSecond=EndSecond,harvest=Harvest}=Machine,TailMachineList}->
					Now = util:now(),
					case AddEnergyEnds < Now of
						true->
							case EndSecond < Now orelse Harvest>0 of
								true->
									{false,?HOMESTEAD_ERROR_NOT_ADDENERGY_NO_SEED};
								false->
									{true,Machine,TailMachineList}
							end;
						false->
							{false,?HOMESTEAD_ERROR_FRIEND_MACHINE_BUSY}
					end
			end
	end.

check_homestead_mating(GerID,MatingTimes,FMatingCoolSecond,FGerID)->
	case GerID of
		0->
			{false,?HOMESTEAD_ERROR_NOT_GER};
		_->
			case MatingTimes =< 0 of
				true->
					{false,?HOMESTEAD_ERROR_MATING_TIMES_NOT_ENOUGH};
				false->
					case FGerID of
						0->
							{false,?HOMESTEAD_ERROR_FRIEND_NOT_GER};
						_->
							case util:now()<FMatingCoolSecond of
								true->
									{false,?HOMESTEAD_ERROR_FRIEND_MATING_BUSY};
								false->
									true
							end
					end
			end
	end.
%%计算公会科技对产出结果的影响系数
%%Pos是配置中定义的对应影响系数的位置
%%AddType是影响的类型，1为产出等影响，2为武将属性的影响
calculate_familyTek_addbuff(FamilyID,Pos,AddType) ->
	case FamilyID of
		0 ->
			0;
		?undefined ->
			0;
		_ ->
			FamilyTekInfo = ets:lookup(?ETS_FAMILY_TECHNOLOGY,FamilyID),
			case FamilyTekInfo of
				[] ->
					0;
				[{_,FamilyTekInfoList}] ->
					{Acc,_} = lists:foldl(fun(FamilyTekInfoItem,{Acc0,AddType0})->
						{TekID,_} = FamilyTekInfoItem,
						{_,_,_,_,Add_attr,Add_buff} = data_family_technology:get({data_technology_rank, TekID}),
						case AddType0 of
							1 ->
								Addcoefficient = element(Pos+1,Add_attr),
								{Acc0+Addcoefficient,AddType0};
							2 ->
								Addcoefficient = element(Pos+1,Add_buff),
								{Acc0+Addcoefficient,AddType0}
						end
					end,{0,AddType},FamilyTekInfoList),
					Acc
			end
	end.
	
test()->
	Now = util:now(),
	io:format("Now===>>>~p~n",[Now]),
	Machine1 = #p_homestead_machine{num=1,seedItemID=30001,endSecond=Now+10},
	Machine2 = #p_homestead_machine{num=2,seedItemID=0,endSecond=0},
	io:format("====~p~n",[get_ets_homestead_enargy_time([Machine1,Machine2])]),
	Machine3 = #p_homestead_machine{num=1,seedItemID=30001,endSecond=Now+30,addEnergyEndS=Now+5},
	io:format("====~p~n",[get_ets_homestead_enargy_time([Machine3,Machine2])]),
	Machine4 = #p_homestead_machine{num=2,seedItemID=30006,endSecond=Now+10},
	io:format("====~p~n",[get_ets_homestead_enargy_time([Machine3,Machine4])]),
	 Machine5 = #p_homestead_machine{num=2,seedItemID=30006,endSecond=Now+20,addEnergyEndS=Now+30},
    io:format("====~p~n",[get_ets_homestead_enargy_time([Machine3,Machine5])]).
