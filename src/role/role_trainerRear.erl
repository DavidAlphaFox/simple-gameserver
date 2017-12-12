-module(role_trainerRear).

-compile(export_all).

-include("def_role.hrl").

-define(TRAINERREAR_MACHINE_PRODUCE_TYPE,1).%%产蛋器
-define(TRAINERREAR_MACHINE_INCUBATION_TYPE,2).%%孵化器

-define(TRAINERREAR_MACHINE_TYPE_LIST,[?TRAINERREAR_MACHINE_INCUBATION_TYPE,?TRAINERREAR_MACHINE_PRODUCE_TYPE]).

-define(machinelist,machinelist).

-define(REAR_MACHINE_STATE_FREE,0).
-define(REAR_MACHINE_STATE_REARING,1).
-define(REAR_MACHINE_STATE_MATURED,2).

%%这两个状态主要是用于通知前端的
-define(MACHINE_TOTAL_STATE_UNMATRUE,1).
-define(MACHINE_TOTAL_STATE_MATURE,2).
% -record(trainer_rear_machine,{machineID=0,type=0,objectList=[],matureTime=0,isMature=false}).
%% 培育相关协议
cs_trainerRear_brief(#cs_trainerRear_brief{})->
	ProduceState = get_total_machine_state(?TRAINERREAR_MACHINE_PRODUCE_TYPE),
	IncubationState = get_total_machine_state(?TRAINERREAR_MACHINE_INCUBATION_TYPE),
	?sendself(#sc_trainerRear_brief{produceState=ProduceState,incubationState=IncubationState}).

cs_trainerRear_info(#cs_trainerRear_info{type=Type})->
	case check_rear_type(Type) of
		false->
			?sendself(#sc_trainerRear_info{result=2,type=Type});
		_->
			RearMachineList = get_trainerrear_info(Type),
			PRearMachineList = [rear_machine2p_rear_machine(E)||E<-RearMachineList],
			?sendself(#sc_trainerRear_info{result=1,rearList=PRearMachineList,type=Type})
	end.

cs_trainerRear_insert(#cs_trainerRear_insert{machineID=MachineID,type=Type,objectIDList=ObjectIDList})->
	case check_insert(Type,MachineID) of
		{false,Reason}->
			?sendself(#sc_trainerRear_insert{result=Reason,machine=#p_rear_machine{}});
		{true,TarMachine,OtherMachine}->
			case do_trainerRear_insert(Type,TarMachine,ObjectIDList) of
				{false,Reason}->
					?sendself(#sc_trainerRear_insert{result=Reason,machine=#p_rear_machine{}});
				{true,NewMachine,_MatureTime}->
					set_trainerrear_info([NewMachine|OtherMachine],Type),
					?sendself(#sc_trainerRear_insert{result=1,machine=rear_machine2p_rear_machine(NewMachine)})
			end
	end.

cs_trainerRear_mature(#cs_trainerRear_mature{type=Type,machineID=MachineID})->
	case check_trainerRear_mature(Type,MachineID) of
		{false,Reason}->
			?sendself(#sc_trainerRear_mature{result=Reason,type=Type,machineID=MachineID});
		{true,Machine,OtherMachine}->
			{Reward,NewMachine} = gain_machine_harvest(Machine),
			set_trainerrear_info([NewMachine|OtherMachine],Type),
			%%此处确认一次是否有brife状态变化，从而通知前端
			case get_total_machine_state(Type) of
				?MACHINE_TOTAL_STATE_MATURE->
					ignore;
				?MACHINE_TOTAL_STATE_UNMATRUE->
					cs_trainerRear_brief(#cs_trainerRear_brief{})
			end,
			case Reward of
				[]->
					ignore;
				_->
					Role = role_data:get_roleInfo(),
					role_reward:handle_sys_reward(Role, Reward,?MONEY_ADD_TYPE_TRAINERREAR_GAIN,MachineID,"")
			end,
			?sendself(#sc_trainerRear_mature{result=1,type=Type,machineID=MachineID,gain=role_reward:transform2p_reward_view(Reward,[])})
	end.


cs_trainerRear_accelerate(#cs_trainerRear_accelerate{type=Type,machineID=MachineID,accelerateMin=AccelerateNum})->
	case check_trainerRear_accelerate(Type,MachineID,AccelerateNum) of
		{false,Reason}->
			?sendself(#sc_trainerRear_accelerate{result=Reason,type=Type,machineID=MachineID});
		{true,#trainer_rear_machine{machineID=MachineID}=Machine,OtherMachine,RealAccelerateNum,GoldCost}->
			%%扣钻石
			Role = role_data:get_roleInfo(),
			role_lib:deduct_money_f(Role, gold,GoldCost, ?MONEY_ADD_TYPE_TRAINERREAR_ACCELERATE,MachineID,""),
			NewMachine = do_trainerRear_accelerate(Machine,RealAccelerateNum),
			set_trainerrear_info([NewMachine|OtherMachine],Type),
			?sendself(#sc_trainerRear_accelerate{result=1,type=Type,machineID=MachineID,matureTime=NewMachine#trainer_rear_machine.matureTime})
	end.

check_trainerRear_accelerate(Type,MachineID,AccelerateNum)->
	case check_rear_type(Type) of
		false->
			{false,2};
		true->
			TrainerRearInfo = get_trainerrear_info(Type),
			case lists:keytake(MachineID,#trainer_rear_machine.machineID,TrainerRearInfo) of
				false->
					{false,3};
				{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_FREE},_Other}->
					{false,4};
				{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_MATURED},_Other}->
					{false,5};
				{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_REARING}=M,Other}->
					case check_trainerRear_accelerate2(M,AccelerateNum) of
						{true,RealAccelerateNum,TotalGoldCost}-> 
							{true,M,Other,RealAccelerateNum,TotalGoldCost};
						R->
							R
					end
			end
	end.

check_trainerRear_accelerate2(#trainer_rear_machine{matureTime=MatureTime},AccelerateNum)->
	Now = util:now(),
	case Now >= MatureTime of
		true->
			{false,5};
		false->
			MaxAccelerateNum = util:ceil((MatureTime - Now) / data_trainerRear:get(accelerate_unit)),
			RealAccelerateNum = lists:min([MaxAccelerateNum,AccelerateNum]),
			TotalGoldCost = data_trainerRear:get(accelerate_unit_gold_cost) * RealAccelerateNum,
			Role = role_data:get_roleInfo(),
			case role_lib:check_money(Role,gold,TotalGoldCost) of
				true->
					{true,RealAccelerateNum,TotalGoldCost};
				false->
					{false,6}
			end
	end.

check_trainerRear_mature(Type,MachineID)->
	case check_rear_type(Type) of
		false->
			{false,2};
		true->
			TrainerRearInfo = get_trainerrear_info(Type),
			case lists:keytake(MachineID,#trainer_rear_machine.machineID,TrainerRearInfo) of 
				false->
					{false,3};
				{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_FREE},_Other}->
					{false,4};
				{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_REARING},_Other}->
					{false,5};
				{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_MATURED}=M,Other}->
					{true,M,Other}
			end
	end.

do_trainerRear_accelerate(#trainer_rear_machine{matureTime=MatureTime,matureTimeRef=MatureTimeRef,type=Type,machineID=MachineID} = Machine,RealAccelerateNum)->
	AccelerateSecond = RealAccelerateNum * data_trainerRear:get(accelerate_unit),
	Now = util:now(),
	NewMatureTime = MatureTime-AccelerateSecond,
	erlang:cancel_timer(MatureTimeRef),
	case NewMatureTime > Now of
		true->
			NewMatureTimeRef = add_mature_timer(Type,MachineID,NewMatureTime),
			Machine#trainer_rear_machine{matureTime=NewMatureTime,matureTimeRef=NewMatureTimeRef};
		false->
			send_rear_brife(Type),
			Machine#trainer_rear_machine{matureTime=NewMatureTime,matureTimeRef=?undefined,state=?REAR_MACHINE_STATE_MATURED}
	end.
	
gain_machine_harvest(Machine)->
	Reward1 = calculate_mature_harvest(Machine),
	Reward = role_reward:merge_reward(Reward1),
	NewMachine = Machine#trainer_rear_machine{objectList=[],state=?REAR_MACHINE_STATE_FREE,matureTime=0,matureCostTime=0},
	{Reward,NewMachine}.

%%FIX
calculate_mature_harvest(#trainer_rear_machine{type=?TRAINERREAR_MACHINE_PRODUCE_TYPE,objectList=[{GerTypeIDA,_GerQualityA,_GerLevelA},{GerTypeIDB,_GerQualityB,_GerLevelB}|_T]})->
	#data_ger{gerStar=StarA} = data_ger:get(GerTypeIDA),
	#data_ger{gerStar=StarB} = data_ger:get(GerTypeIDB),
	case  data_trainerRear:get({produce,StarA,StarB}) of
		?undefined->
			Box1Config = data_trainerRear:get({produce,StarB,StarA});
		C ->
			Box1Config = C
	end,
	case util:random_one_from_weigh_list(Box1Config) of
		{Box2ID,Times}->
			Box2Config = data_trainerRear:get({produce_box,Box2ID}),
			[util:random_one_from_weigh_list(Box2Config)||_<-lists:seq(1,Times)];
		null->
			?ERR("null Reward BoxID:~n"),
			[]
	end;

calculate_mature_harvest(#trainer_rear_machine{type=?TRAINERREAR_MACHINE_INCUBATION_TYPE,objectList=[ItemTypID|_T]})->
	BoxConfig1 = data_trainerRear:get({incubation,ItemTypID}),
	TrainerProf=role_trainerProf:get_role_trainerProf(?REAR_TRAINER_TYPE),
	BoxConfig = fix_boxconfig(BoxConfig1,TrainerProf),
	case util:random_one_from_weigh_list(BoxConfig) of
		{Box2ID,Times,true}->
			case check_special_limist(Box2ID) of
				false->
					Box2Config = data_trainerRear:get({incubation_box,Box2ID}),
					[util:random_one_from_weigh_list(Box2Config)||_<-lists:seq(1,Times)];
				true->
					?ERR("unique limit boxID:~w~n",[Box2ID]),
					[]
			end;
		{Box2ID,Times,false}->
			Box2Config = data_trainerRear:get({incubation_box,Box2ID}),
			[util:random_one_from_weigh_list(Box2Config)||_<-lists:seq(1,Times)];
		null->
			?ERR("null Reward BoxID:~n"),
			[]
	end.

%%判断是否特殊宝箱已经被开启
check_special_limist(Box2ID)->
	not trainerRear_server:gain_unique_box(Box2ID).

%%该函数中只对要添加的机器的状态等做判断，对具体插入的精灵的判断放到do_trainerRear_insert中
check_insert(Type,MachineID)->
	case check_rear_type(Type) of
		false->
			{false,2};
		true->
			case get_trainerrear_info(Type) of
				[]->
					{false,3};
				L ->
					case lists:keytake(MachineID,#trainer_rear_machine.machineID,L) of
						{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_FREE}=M,Other}->
							{true,M,Other};
						{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_MATURED},_Other}->
							{false,5};
						{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_REARING},_Other}->
							{false,4};
						_->
							{false,3}
					end
			end
	end.

%%产蛋器的objectList中包括对应精灵的{GerTypeID,GerQaulity,GerLevel}
%%孵化器中包括对应蛋的ItemTypeID
do_trainerRear_insert(Type,Machine,ObjectIDList)->
	case check_object_and_delete(Type,ObjectIDList) of
		{false,Reason}->
			{false,Reason};
		{true,ObjectList}->
			MatureCostTime = calculate_rear_maturetime(Type,ObjectList),
			MatureTime = util:now()+MatureCostTime,
			Ref = add_mature_timer(Type,Machine#trainer_rear_machine.machineID,MatureTime),
			NewMachine = Machine#trainer_rear_machine{objectList=ObjectList,matureTime=MatureTime,state=?REAR_MACHINE_STATE_REARING,matureTimeRef=Ref,matureCostTime=MatureCostTime},
			{true,NewMachine,MatureTime}
	end.

%%检查object是否符合条件，如果符合就删除
check_object_and_delete(?TRAINERREAR_MACHINE_PRODUCE_TYPE,ObjectIDList)->
	case length(ObjectIDList) =:= data_trainerRear:get(produce_object_length) of
		true->
			GerBag = role_data:get_gerBag(),
			Result = lists:foldl(fun(GerID,{DelAcc,RemAcc,TResult})->
				case TResult of
					false->
						{DelAcc,RemAcc,false};
					true->
						case lists:keytake(GerID,#gerSimple.gerID,RemAcc) of
							false->
								{DelAcc,RemAcc,false};
							{_V,Find,Other}->
								case Find#gerSimple.gerQuality > 0 of
									true->
										%%限制品阶大于0的不能消耗
										{DelAcc,RemAcc,false};
									false->	
										case util:is_exp_card(Find#gerSimple.gerTypeID) of
											false->
												case ger_lib:is_blink_ger(Find#gerSimple.gerTypeID) of
													true->
														{DelAcc,RemAcc,false};
													false->
														{[Find|DelAcc],Other,true}
												end;
											true->
												%%限制能量块
												{DelAcc,RemAcc,false}
										end
								end
						end
				end
			end,{[],GerBag,true},ObjectIDList),
			case Result of
				{DelList,RemList,true}->
					%%删除精灵
					role_data:set_gerBag(RemList),
					#role{roleID=RoleID} = role_data:get_roleInfo(),
					{Date, _} = Time = erlang:localtime(),
					LogGerList = role_ger:gerList2logGerList(DelList),
					[erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
					behavior_ger_consume:log(RoleID, LogGerList, Date, Time,?MONEY_DEC_TYPE_TRAINERREAR,0,""),
					ger_lib:ra_ger_delete(DelList),
					ObjectList2 = [{GerTypeID,GerQuality,GerLevel}||#gerSimple{gerTypeID=GerTypeID,gerQuality=GerQuality,gerLevel=GerLevel}<-DelList],
					{true,ObjectList2};
				{_,_,false}->
					{false,7}
			end;
		false->
			{false,8}
	end;

%%此处传递过来的道具ID可能有多个，但是只扣除了第一个道具1个(孵蛋功能一次就一种蛋)
check_object_and_delete(?TRAINERREAR_MACHINE_INCUBATION_TYPE,ObjectIDList)->
	case check_rear_available(ObjectIDList) of
		false->
			{false,9};
		true->
			case item_lib:check_material(hd(ObjectIDList),1) of 
				false->
					{false,8};
				{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList}->
					%%删除道具
					#role{roleID=RoleID} = role_data:get_roleInfo(),
					role_data:set_bagItem(BagOther2),
					LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
					{Date, _} = Time = erlang:localtime(),
					behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_TRAINERREAR, 0, ""),
					item_lib:ra_item_delete(DelAcc),
					item_lib:ra_item_update_num(UpdateAcc),
					{true,ObjectIDList}
			end
	end;
check_object_and_delete(Type,ObjectIDList)->
	?ERR("undefine Type:~w ObjectIDList:~w ~n",[Type,ObjectIDList]),
	{false,2}.

check_rear_available(ObjectIDList)->
	check_rear_available(ObjectIDList,true).
check_rear_available(_ObjectIDList,false)->
	false;
check_rear_available([],Result)->
	Result;
check_rear_available([H|T],Result)->
	IsAvailable = lists:member(H,data_trainerRear:get(incubationIDList)),
	check_rear_available(T,IsAvailable andalso Result).

%%计算孵化时间
calculate_rear_maturetime(?TRAINERREAR_MACHINE_PRODUCE_TYPE,[{GerTypeIDA,GerQualityA,_GerLevelA},{GerTypeIDB,GerQualityB,_GerLevelB}|_T])->
	#data_ger{gerStar=StarA} = data_ger:get(GerTypeIDA),
	#data_ger{gerStar=StarB} = data_ger:get(GerTypeIDB),	
	Max = lists:max([StarA,StarB]),
	Min = lists:min([StarA,StarB]),
	SolidTime = trunc((4+((Max-2)*(Max-1)/2+Min-2)*0.5)*3600),
	DeductTime = trunc((GerQualityA*(StarA-1)+GerQualityB*(StarB-1))*data_trainerRear:get(produce_deduct_unit)),
	erlang:max(SolidTime - DeductTime,0);
calculate_rear_maturetime(?TRAINERREAR_MACHINE_INCUBATION_TYPE,[ItemTypeID|_T])->
	{Day,Hour,Min} = data_trainerRear:get({incubation_time,ItemTypeID}),
	MatureTime = Day*?ONE_DAY_SECONDS+Hour*?ONE_HOUR_SECONDS+Min*60,
	MatureTime.

add_mature_timer(Type,MachineID,MatureTime)->
	Now = util:now(),
	erlang:send_after((MatureTime-Now)*1000,self(),{route,role_trainerRear,{rear_mature,Type,MachineID}}).

rear_mature({_,Type,MachineID})->
	TrainerRearInfo = get_trainerrear_info(Type),
	case lists:keytake(MachineID,#trainer_rear_machine.machineID,TrainerRearInfo) of
		false->
			?ERR("undefined MachineID:~w ~n",[MachineID]),
			ignore;
		{_V,#trainer_rear_machine{state=?REAR_MACHINE_STATE_REARING}=E,Other}->
			NE = E#trainer_rear_machine{state=?REAR_MACHINE_STATE_MATURED},
			set_trainerrear_info([NE|Other],Type),
			send_rear_brife(Type);
		{_V,E,_Other}->
			?ERR("error state Machine:~w ~n",[E]),
			ignore
	end. 

%%default值设定成-1，将会根据当前玩家的训练师等级，初始化对应的数据
init_trainerrear_info(RoleID)->
	case db_sql:get_trainerRear(RoleID) of
		[]->
			init_trainerrear_info1(-1);
		PersistForm->
			init_trainerrear_info1(PersistForm)
	end.

init_trainerrear_info1(-1)->
	TrainerProf=role_trainerProf:get_role_trainerProf(?REAR_TRAINER_TYPE),
	% ?ERR("TrainerProf:~w ~n",[TrainerProf]),
	{ProduceInfoList,IncubationInfoList} = generate_trainerrear_info2(TrainerProf,[],[]),
	set_trainerrear_info(ProduceInfoList,?TRAINERREAR_MACHINE_PRODUCE_TYPE),
	set_trainerrear_info(IncubationInfoList,?TRAINERREAR_MACHINE_INCUBATION_TYPE);

init_trainerrear_info1([ProducePersistList,IncubationPersistList])->
	ProduceInfoList1 = [persistform2termform(E,?TRAINERREAR_MACHINE_PRODUCE_TYPE)||E<-ProducePersistList],
	IncubationInfoList1 = [persistform2termform(E,?TRAINERREAR_MACHINE_INCUBATION_TYPE)||E<-IncubationPersistList],
	%%重新计算所有机器状态,增加机器
	TrainerProf=role_trainerProf:get_role_trainerProf(?REAR_TRAINER_TYPE),
	{ProduceInfoList,IncubationInfoList} = generate_trainerrear_info2(TrainerProf,ProduceInfoList1,IncubationInfoList1),
	set_trainerrear_info(ProduceInfoList,?TRAINERREAR_MACHINE_PRODUCE_TYPE),
	set_trainerrear_info(IncubationInfoList,?TRAINERREAR_MACHINE_INCUBATION_TYPE).

persist_trainerRear(RoleID)->
	ProduceInfo = get_trainerrear_info(?TRAINERREAR_MACHINE_PRODUCE_TYPE),
	IncubationInfo = get_trainerrear_info(?TRAINERREAR_MACHINE_INCUBATION_TYPE),
	db_sql:set_trainerRear(RoleID,[termform2persistform(ProduceInfo),termform2persistform(IncubationInfo)]).	

%%根据玩家职业信息成对应的培育室信息
generate_trainerrear_info2(TrainerProf,ProduceInfo,IncubationInfo)->
	{generate_trainerrear_produce_info(TrainerProf,ProduceInfo),generate_trainerrear_incubation_info(TrainerProf,IncubationInfo)}.

generate_trainerrear_produce_info(#trainer_prof{level=Level},ExistProduceInfo)->
	{ProduceNum,_IncubationNum} = data_trainerRear:get({data_rear_slot_num,Level}),
	TotalProduceList = add_machine(ExistProduceInfo,ProduceNum,?TRAINERREAR_MACHINE_PRODUCE_TYPE),
	[check_machine_state(E)||E<-TotalProduceList].

generate_trainerrear_incubation_info(#trainer_prof{level=Level},ExistIncubationInfo)->
	{_ProduceNum,IncubationNum} = data_trainerRear:get({data_rear_slot_num,Level}),
	TotalProduceList = add_machine(ExistIncubationInfo,IncubationNum,?TRAINERREAR_MACHINE_INCUBATION_TYPE),
	[check_machine_state(E)||E<-TotalProduceList].

%%根据总的机器数，返回所有的机器，不足的直接初始化,多了的机器没有减少
add_machine(ExistMachineList,TotalMachineNum,Type)->
	ExistMachineNum = length(ExistMachineList),
	case TotalMachineNum > ExistMachineNum of
		true->
			lists:foldl(fun(E,Acc)->
				case lists:keyfind(E,#trainer_rear_machine.machineID,Acc) of
					false->
						[#trainer_rear_machine{machineID=E,type=Type,state=?REAR_MACHINE_STATE_FREE}|Acc];
					_->
						Acc
				end
			end,ExistMachineList,lists:seq(1,TotalMachineNum));
		false->
			ExistMachineList
	end.

check_machine_state(#trainer_rear_machine{type=Type,machineID=MachineID,objectList=ObjectIDList,matureTime=MatureTime,matureTimeRef=MatureTimeRef}=E)->
	Now = util:now(),
	case ObjectIDList of
		[]->
			E#trainer_rear_machine{objectList=[],matureTime=0,state=?REAR_MACHINE_STATE_FREE,matureTimeRef=?undefined};
		_->
			if
				Now < MatureTime ->
					TimeRef = add_mature_timer(Type,MachineID,MatureTime),
					E#trainer_rear_machine{state=?REAR_MACHINE_STATE_REARING,matureTimeRef=TimeRef};
				true->
					case MatureTimeRef of
						?undefined->
							ignore;
						_->
							erlang:cancel_timer(MatureTimeRef)
					end,
					E#trainer_rear_machine{state=?REAR_MACHINE_STATE_MATURED,matureTimeRef=?undefined}
			end
	end.
termform2persistform(L) when is_list(L)->
	[termform2persistform(E)||E<-L];
termform2persistform(#trainer_rear_machine{machineID=MachineID,objectList=ObjectList,matureTime=MatureTime,matureCostTime=MatureCostTime})->
	{MachineID,ObjectList,MatureTime,MatureCostTime}.

set_trainerrear_info(MachineList,Type)->
	put({?machinelist,Type},MachineList).

get_trainerrear_info(Type)->
	get({?machinelist,Type}).

persistform2termform({MachineID,ObjectList,MatureTime,MatureCostTime},Type)->
	#trainer_rear_machine{machineID=MachineID,type=Type,objectList=ObjectList,matureTime=MatureTime,matureCostTime=MatureCostTime}.

% rear_machine2p_rear_machine(#trainer_rear_machine{machineID=MachineID,type=?TRAINERREAR_MACHINE_PRODUCE_TYPE,objectList=ObjectList,matureTime=MatureTime,matureCostTime=MatureCostTime})->
% 	#p_rear_machine{machineID=MachineID,type=?TRAINERREAR_MACHINE_PRODUCE_TYPE,objectList=object2pobjectunit(ObjectList),matureTime=MatureTime,totalTime=MatureCostTime};

rear_machine2p_rear_machine(#trainer_rear_machine{machineID=MachineID,type=Type,objectList=ObjectList,matureTime=MatureTime,matureCostTime=MatureCostTime})->
	ObjectUnitList = lists:reverse([object2pobjectunit(E)||E<-ObjectList]),
	#p_rear_machine{machineID=MachineID,type=Type,objectList=ObjectUnitList,matureTime=MatureTime,totalTime=MatureCostTime}.

check_rear_type(Type)->
	lists:member(Type,?TRAINERREAR_MACHINE_TYPE_LIST).

refresh_machine(?REAR_TRAINER_TYPE)->
	ProduceInfoList1 = get_trainerrear_info(?TRAINERREAR_MACHINE_PRODUCE_TYPE),
	IncubationInfoList1 = get_trainerrear_info(?TRAINERREAR_MACHINE_INCUBATION_TYPE),
	TrainerProf = role_trainerProf:get_role_trainerProf(?REAR_TRAINER_TYPE),
	{ProduceInfoList,IncubationInfoList} = generate_trainerrear_info2(TrainerProf,ProduceInfoList1,IncubationInfoList1),
	case ProduceInfoList =:= ProduceInfoList1 of
		true->
			ignore;
		_->
			PPRearMachineList = [rear_machine2p_rear_machine(E)||E<-ProduceInfoList],
			?sendself(#sc_trainerRear_info{result=1,rearList=PPRearMachineList,type=?TRAINERREAR_MACHINE_PRODUCE_TYPE})
	end,
	case IncubationInfoList =:= IncubationInfoList1 of
		true->
			ignore;
		_->
			PIRearMachineList = [rear_machine2p_rear_machine(E)||E<-IncubationInfoList],
			?sendself(#sc_trainerRear_info{result=1,rearList=PIRearMachineList,type=?TRAINERREAR_MACHINE_INCUBATION_TYPE})
	end,
	set_trainerrear_info(ProduceInfoList,?TRAINERREAR_MACHINE_PRODUCE_TYPE),
	set_trainerrear_info(IncubationInfoList,?TRAINERREAR_MACHINE_INCUBATION_TYPE);
refresh_machine(_)->
	ignore.

%%向玩家推送状态变化消息
send_rear_brife(?TRAINERREAR_MACHINE_INCUBATION_TYPE)->
	?sendself(#sc_trainerRear_brief{incubationState=2});
send_rear_brife(?TRAINERREAR_MACHINE_PRODUCE_TYPE)->
	?sendself(#sc_trainerRear_brief{produceState=2}).

object2pobjectunit({GerTypeID,GerQuality,_GerLevel})->
	#p_object_unit{objectID=GerTypeID,quality=GerQuality};
object2pobjectunit(ObjectID)->
	#p_object_unit{objectID=ObjectID}.

deduct_incubation_null_rate(#trainer_prof{level=Level})->
	case data_trainerRear:get({trainerRear_level_deduct_null,Level}) of
		?undefined->
			0;
		R->
			R
	end.

fix_boxconfig(BoxConfig,TrainerProf)->
	DeductRate = deduct_incubation_null_rate(TrainerProf),
	case lists:keytake(null,2,BoxConfig) of
		{_,{OldRate,null},Other}->
			NewRate = erlang:max(0,(OldRate-DeductRate)),
			case NewRate of
				0->
					Other;
				_->
					[{NewRate,null}|Other]
			end;
		_->
			BoxConfig
	end.

get_total_machine_state(Type)->
	MachineList = get_trainerrear_info(Type),
	case [E||#trainer_rear_machine{state=?REAR_MACHINE_STATE_MATURED}=E<-MachineList] of
		[]->
			?MACHINE_TOTAL_STATE_UNMATRUE;
		_->
			?MACHINE_TOTAL_STATE_MATURE
	end.