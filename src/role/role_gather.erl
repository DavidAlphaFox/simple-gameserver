%% @author caohongyang
%% @doc 图鉴功能
%% Created 2013-5-30


-module(role_gather).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([hook_add_item_list/1,hook_add_ger_list/1,hook_add_equip_list/1]).

%% Internal functions
-export([]).

-define(SHAPE_1, 1).
-define(SHAPE_2, 2).
-define(SHAPE_3, 3).
-define(SHAPE_4, 4).

-define(QUALITY_TO_SHAPE_4, 30).
-define(QUALITY_TO_SHAPE_3, 20).
-define(QUALITY_TO_SHAPE_2, 10).

%%===================新图鉴相关=========================%%
-define(manual,manual).

-define(MANUAL_UNIT_UNFINISH,0).
-define(MANUAL_UNIT_FINISH,1).
-define(MANUAL_UNIT_FINISHREWARD,2).

-define(COLLECT_TAG_TOTAL,0).
%%=====================新图鉴END========================%%

%% ====================================================================
%% API functions
%% ====================================================================
cs_gather_get_list(#cs_gather_get_list{type=Type}) ->
	if Type =:= ?GATHER_TYPE_GER orelse Type =:= ?GATHER_TYPE_ITEM orelse Type =:= ?GATHER_TYPE_EQUIP ->
		   List = role_data:get_gatherInfo(Type),
		   ?sendself(#sc_gather_get_list{type=Type,idList=List});
	   true ->
		   ?sendself(#sc_gather_get_list{type=Type,idList=[]})
		   end.


%% ====================================================================
%% Internal functions
%% ====================================================================
hook_add_item_list(ItemTypeIDList) ->
	hook_gather(?GATHER_TYPE_ITEM, ItemTypeIDList).

hook_add_ger_list(GerList) ->
    GerTypeIDList = lists:map(fun({GerTypeID, GerQuality}) ->
                                  ?SHAPE_BASE * quality_to_shape(GerQuality) + GerTypeID
                              end, GerList),
	hook_gather(?GATHER_TYPE_GER, GerTypeIDList).

quality_to_shape(GerQuality) when GerQuality >= ?QUALITY_TO_SHAPE_4 ->
    ?SHAPE_4;
quality_to_shape(GerQuality) when GerQuality >= ?QUALITY_TO_SHAPE_3 ->
    ?SHAPE_3;
quality_to_shape(GerQuality) when GerQuality >= ?QUALITY_TO_SHAPE_2 ->
    ?SHAPE_2;
quality_to_shape(_GerQuality) ->
    ?SHAPE_1.

hook_add_equip_list(EquipTypeIDList) ->
    hook_gather(?GATHER_TYPE_EQUIP, EquipTypeIDList).

hook_gather(Type, TypeIDList) ->
	GatherSet = role_data:get_gatherInfo(Type),
	{GatherSet2, NewList} =
	lists:foldl(fun(E, {L,NL}=Acc) ->
						case lists:member(E,L) of
							true ->
								Acc;
							false ->
								{[E|L],[E|NL]}
						end
				end, {GatherSet,[]}, TypeIDList),
	if NewList =/= [] ->		   
		   role_data:set_gatherInfo(Type, GatherSet2),		
		   ?sendself(#sc_gather_new{type=Type,newIDList=NewList}),
		   db_sql:add_gatherInfo(role_data:get_roleID(), Type, NewList);
	   true ->
		   ignore
	end.
						
%%%======================================================新图鉴部分====================================================%%
%%新图鉴将图鉴收集以及收集任务分割成两部分保存，分别作为图鉴单元列表和图鉴收集任务单元
%%manual_unit可能存在不存在的情况，collect_task一定存在(初始化的时候读取所有的任务)
%%图鉴收集部分不区分
cs_gather_manual_info(#cs_gather_manual_info{})->
	#manual_info{manual_collect_task_list=ManualCollectTaskList} = get_role_manual(),
	{TotalCollectUnitL,TagFinishL} = lists:foldl(fun(#manual_collect_task{tag=Tag,state=State}=E,{CollectAcc,TagAcc})->
		case Tag =:= ?COLLECT_TAG_TOTAL of
			true->
				{[transform_collect_task2p_collect_task(E)|CollectAcc],TagAcc};
			false->
				case State =:= ?MANUAL_UNIT_FINISH of
					true->
						{CollectAcc,[Tag|TagAcc]};
					false->
						{CollectAcc,TagAcc}
				end
		end
	end,{[],[]},ManualCollectTaskList),
	?sendself(#sc_gather_manual_info{totalcollect=TotalCollectUnitL,tagfinish=TagFinishL}).

cs_gather_manual_info_for_tag(#cs_gather_manual_info_for_tag{tag=TarTag})->
	#manual_info{manual_collect_task_list=ManualCollectTaskList,manual_unit_list=ManualUnitList} = get_role_manual(),
	PGerManualUnitL = [transform_manual_unit2p_manual_unit(E)||#manual_unit{tag=Tag}=E<-ManualUnitList,Tag=:=TarTag],
	PCollectUnitL = [transform_collect_task2p_collect_task(E)||#manual_collect_task{tag=Tag}=E<-ManualCollectTaskList,Tag=:=TarTag],
	?sendself(#sc_gather_manual_info_for_tag{tag=TarTag,manuallist=PGerManualUnitL,collecttask=PCollectUnitL}).

cs_gather_manual_collect_draw(#cs_gather_manual_collect_draw{taskID=TaskID})->
	ManualInfo = get_role_manual(),
	case do_cs_gather_manual_collect_draw(TaskID,ManualInfo) of
		{false,R}->
			?sendself(#sc_gather_manual_collect_draw{result=R});
		{true,Reward,NewManualInfo,NewCollectTask}->
			set_role_manual(NewManualInfo),
			send_manual_info_update([],[NewCollectTask]),
			PRewardInfo = role_reward:transform2p_reward_view(Reward,[]),
			?sendself(#sc_gather_manual_collect_draw{result=1,reward=PRewardInfo})
	end.

do_cs_gather_manual_collect_draw(TaskID,ManualInfo)->
	#manual_info{manual_collect_task_list=ManualCollectTaskList} = ManualInfo,
	case lists:keytake(TaskID,#manual_collect_task.taskID,ManualCollectTaskList) of
		false->
			{false,2};
		{_V,#manual_collect_task{state=?MANUAL_UNIT_UNFINISH},_Other}->
			{false,3};
		{_V,#manual_collect_task{state=?MANUAL_UNIT_FINISH,taskID=TaskID}=M,Other}->
			#data_collect_task{reward=Reward} = data_manual:get(TaskID),
			Role = role_data:get_roleInfo(),
			%%发放对应的奖励
			role_reward:handle_sys_reward_with_return(Role,Reward,?MONEY_ADD_TYPE_MANUAL,TaskID,"",true),
			%%更新ManualInfo
			NewCollectTask = M#manual_collect_task{state=?MANUAL_UNIT_FINISHREWARD},
			NewManualInfo = ManualInfo#manual_info{manual_collect_task_list=[NewCollectTask|Other]},
			{true,Reward,NewManualInfo,NewCollectTask};
		_->
			{false,4}
	end.

get_role_manual()->
	case get(?manual) of
		?undefined->
			#role{roleID=RoleID} = role_data:get_roleInfo(),
			X = db_sql:get_manualInfo(RoleID),
			set_role_manual(X),
			X;
		X->
			X
	end.

set_role_manual(Manual)->
	put(?manual,Manual).

update_role_manual(GerList)->
	ManualInfo = get_role_manual(),
	% ?INFO("ManualInfo:~w GerList~w ~n",[ManualInfo,GerList]),
	NewManualInfo = update_role_manual2(ManualInfo,GerList),
	set_role_manual(NewManualInfo).

update_role_manual2(#manual_info{manual_unit_list=MUL,manual_collect_task_list=MCTL}=M,GerList)->
	{NewMUL,EffectTaskList,UpdateManualUnitL} = update_role_manual_unit(MUL,GerList),
	{NewMCTL,UpdateMCTL} = case EffectTaskList of
		[]->
			{MCTL,[]};
		_->
			update_role_manual_collect_task(EffectTaskList,MCTL)
	end,
	send_manual_info_update(UpdateManualUnitL,UpdateMCTL),
	M#manual_info{manual_unit_list=NewMUL,manual_collect_task_list=NewMCTL}.

%%更改玩家收集列表中的对应精灵的收集状态
update_role_manual_unit(ManualUnitList,GerList)->
	update_role_manual_unit2(ManualUnitList,GerList,[],[]).

update_role_manual_unit2(ManualUnitList,[],EffectTaskList,UpdateManualUnitL)->
	{ManualUnitList,EffectTaskList,UpdateManualUnitL};
update_role_manual_unit2(ManualUnitList,[{GerTypeID,GerQuality}|T],EffectTaskList,UpdateManualUnitL)->
	case update_role_manual_unit3(ManualUnitList,GerTypeID,GerQuality) of
		false->
			update_role_manual_unit2(ManualUnitList,T,EffectTaskList,UpdateManualUnitL);
		{true,NewManualUnitList,AddEffectTaskList,UpdateManualUnit}->
			NewUpdateManualUnitL = case lists:keytake(UpdateManualUnit#manual_unit.gerTypeID,#manual_unit.gerTypeID,UpdateManualUnitL) of
				false->
					[UpdateManualUnit|UpdateManualUnitL];
				{_V,_OF,Other}->
					[UpdateManualUnit|Other]
			end,
			update_role_manual_unit2(NewManualUnitList,T,EffectTaskList++AddEffectTaskList,NewUpdateManualUnitL)
	end.

update_role_manual_unit3(ManualUnitList,GerTypeID,GerQuality)->
	case data_manual:get({data_ger_collect,GerTypeID}) of
		?undefined->
			?ERR("undefined:gerTypeID:~w ~n",[GerTypeID]),
			false;
		{_EffectTaskList,Tag,MaxShape}->
			{TarManualUnit,RemainManualUnit} = case lists:keytake(GerTypeID,#manual_unit.gerTypeID,ManualUnitList) of
				false->
					%%对应的收集单元没有创建，创建之后修改
					{#manual_unit{gerTypeID=GerTypeID,collect=0,tag=Tag},ManualUnitList};
				{_V,F,Other}->
					{F,Other}
			end,
			#manual_unit{collect=OldCollect} = TarManualUnit,
			Shape = quality_to_shape(GerQuality),
			case Shape>MaxShape of
				true->
					false;
				false->
					NewCollect1 = (1 bsl (Shape-1)),
					NewCollect = NewCollect1 bor OldCollect,
					case NewCollect =:= OldCollect of
						true->
							%%没有更改收集结果
							false;
						false->
							{EffectTaskList,_Tag,_MaxShape1} = data_manual:get({data_ger_collect,GerTypeID}),
							NewManualUnit = TarManualUnit#manual_unit{collect=NewCollect},
							{true,[NewManualUnit|RemainManualUnit],EffectTaskList,NewManualUnit}
					end
			end
	end.

%%增加对应收集任务的数量
update_role_manual_collect_task(EffectTaskList,ManualCollectTaskList)->
	EffectTaskList1 = lists:foldl(fun(H,Acc)->
		case lists:keytake(H,1,Acc) of
			false->
				[{H,1}|Acc];
			{_V,{H,Times},Other}->
				[{H,Times+1}|Other]
		end
	end,[],EffectTaskList),
	update_role_manual_collect_task(EffectTaskList1,ManualCollectTaskList,[]).

update_role_manual_collect_task([],ManualCollectTaskList,UpdateColletL)->
	{ManualCollectTaskList,UpdateColletL};
update_role_manual_collect_task([{TaskID,_Times}=H|T],ManualCollectTaskList,UpdateColletL)->
	case lists:keytake(TaskID,#manual_collect_task.taskID,ManualCollectTaskList) of
		false->
			?ERR("exit not find taskID：~w~n",[H]),
			update_role_manual_collect_task(T,ManualCollectTaskList,UpdateColletL);
		{_V,F,Other}->
			NF = update_role_single_manual_collect_task(F,H),
			update_role_manual_collect_task(T,[NF|Other],[NF|UpdateColletL])
	end.
update_role_single_manual_collect_task(#manual_collect_task{taskID=TaskID,finish=OldFinish,state=?MANUAL_UNIT_UNFINISH}=M,{_TaskID,Times})->
	#data_collect_task{totalfinish=TotalFinish} = data_manual:get(TaskID),
	NewFinish = OldFinish+Times,
	case NewFinish>=TotalFinish of
		true->
			M#manual_collect_task{finish=TotalFinish,state=?MANUAL_UNIT_FINISH};
		false->
			M#manual_collect_task{finish=NewFinish}
	end;
update_role_single_manual_collect_task(M,_H)->
	M.

init_manual_collect_task()->
	lists:foldl(fun(K,Acc) when is_number(K)->
		#data_collect_task{taskID=TaskID,tag=Tag,type=Type} = data_manual:get(K),
		[#manual_collect_task{taskID=TaskID,tag=Tag,finish=0,state=?MANUAL_UNIT_UNFINISH,type=Type}|Acc];
		(_K,Acc)->
			Acc
	end,[],data_manual:get_list()).

transform_collect_task2p_collect_task(#manual_collect_task{taskID=TaskID,finish=Finish,state=State,type=Type,tag=Tag})->
	#p_collect_unit{taskID=TaskID,finish=Finish,state=State,type=Type,tag=Tag}.

transform_manual_unit2p_manual_unit(#manual_unit{gerTypeID=GerTypeID,collect=Collect,tag=Tag})->
	#p_ger_manual_unit{gerTypeID=GerTypeID,gerManual=Collect,tag=Tag}.


send_manual_info_update([],[])->
	ignore;
send_manual_info_update(ManualUnitL,CollectTaskL)->
	PGerManualUnitL = [transform_manual_unit2p_manual_unit(E)||E<-ManualUnitL],
	PCollectUnitL = [transform_collect_task2p_collect_task(E)||E<-CollectTaskL],
	?sendself(#sc_gather_manual_update{updatemanual=PGerManualUnitL,updatecollect=PCollectUnitL}).

manual_unit_persit2term([],Acc)->
	Acc;
manual_unit_persit2term([{GerTypeID,Collect}|T],Acc)->
	case data_manual:get({data_ger_collect,GerTypeID}) of
		?undefined->
			?ERR("undefined gerTypeID:~w Collect:~w ~n",[GerTypeID,Collect]),
			manual_unit_persit2term(T,Acc);
		{_EffectTaskList,Tag,_MaxShape}->
			manual_unit_persit2term(T,[#manual_unit{gerTypeID=GerTypeID,collect=Collect,tag=Tag}|Acc])
	end.

manual_unit_term2persit([],Acc)->
	Acc;
manual_unit_term2persit([#manual_unit{gerTypeID=GerTypeID,collect=Collect}|T],Acc)->
	manual_unit_term2persit(T,[{GerTypeID,Collect}|Acc]).

manual_collect_task_persist2term([],Acc)->
	Acc;
manual_collect_task_persist2term([{TaskID,Finish,State}|T],Acc)->
	case data_manual:get(TaskID) of
		?undefined->
			?ERR("undefined collect task TaskID:~w Finish:~w State:~w ~n",[TaskID,Finish,State]),
			manual_collect_task_persist2term(T,Acc);
		#data_collect_task{tag=Tag,type=Type}->
			manual_collect_task_persist2term(T,[#manual_collect_task{taskID=TaskID,tag=Tag,finish=Finish,state=State,type=Type}|Acc])
	end.
manual_collect_task_term2persist([],Acc)->
	Acc;
manual_collect_task_term2persist([#manual_collect_task{taskID=TaskID,finish=Finish,state=State}|T],Acc)->
	manual_collect_task_term2persist(T,[{TaskID,Finish,State}|Acc]).


%%增加闪光精灵图鉴收集
add_blink_ger_manual(GerList)->
	BlinkGerList = [E||{GerTypeID,_GerQuality}=E<-GerList,ger_lib:is_blink_ger(GerTypeID)],
	update_role_manual(BlinkGerList).

%%增加不同精灵图鉴收集
%%传递过来的GerTypeID可能是怪物ID，需要转换下
add_normal_ger_manual(GerList)->
	NormalGerList = lists:foldl(fun({GerTypeID,GerQuality},Acc)->
		case data_ger:get(GerTypeID) of
			?undefined->
				?ERR("undefined gerTypeID:~w ~n",[GerTypeID]),
				Acc;
			#data_ger{baseTypeID=BaseGerTypeID}->	
				case ger_lib:is_blink_ger(BaseGerTypeID) of
					false->
						[{BaseGerTypeID,GerQuality}|Acc];
					true->
						Acc
				end
		end
	end,[],GerList),
	update_role_manual(NormalGerList).

add_ger_manual(GerList)->
    NormalGerList = 
        lists:foldl(fun({GerTypeID,GerQuality},Acc)->
                            case data_ger:get(GerTypeID) of
                                ?undefined->
                                    ?ERR("undefined gerTypeID:~w ~n",[GerTypeID]),
                                    Acc;
                                #data_ger{baseTypeID=BaseGerTypeID}->   
                                    [{BaseGerTypeID,GerQuality}|Acc]
                            end
                    end,[],GerList),
    update_role_manual(NormalGerList).

%%v410版本，取消闪光精灵通过获得精灵的方式完成冒险图鉴
hook_add_ger_list_for_manual(GerList)->
	% add_blink_ger_manual(GerList).
	ignore.
asyn_update_role_manual({_,GerList})->
	update_role_manual(GerList).
%%%===================================test=======================================
test_gather_manual(RoleID,[])->
	GerList = lists:foldl(fun(GerTypeID,Acc)->
		case data_ger:get(GerTypeID) of
			?undefined->
				Acc;
			#data_ger{sameType=SameType}->
				case SameType=/=0 of
					true->
						[{GerTypeID,1},{GerTypeID,11},{GerTypeID,21},{GerTypeID,30}|Acc];
					false->
						Acc
				end
		end
	end,[],data_ger:get_list()),
	test_gather_manual(RoleID,GerList);
test_gather_manual(RoleID,GerList)->
	case role_lib:is_online(RoleID) of
		true->
			case catch role_lib:send_server(RoleID,{route,role_gather,{asyn_update_role_manual,GerList}}) of
			  	{'EXIT',_}->
			    	?ERR("异步更新玩家图鉴失败：roleID:~w GerList:~w  ~n",[RoleID,GerList]);
			  	_->
			    	ignore
			end;
		false->
			?ERR("RoleID:~w is not noline~n",[RoleID])
	end.