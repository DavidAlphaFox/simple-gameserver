%% @author zcl
%% @doc 聚宝盆活动
%% Created 2017-8-11
-module(treasurebowl_server).
-behaviour(gen_server).
-compile(export_all).
-include("def_mail.hrl").
-include("def_reward.hrl").
-include("def_role.hrl").
-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================
-define(dump_interval,300).
-define(TREASUREBOWLSTATE,treasurebowlstate).
%%异常时期
-define(OUT_TREASUREBOWL_PERIOD,0).

%%整个活动的阶段
-define(TREASUREBOWLSTATUS_UNBEGINE,0).
-define(TREASUREBOWLSTATUS_TASK,1).
% -define(TREASUREBOWLSTATUS_REWARD,2).
-define(TREASUREBOWLSTATUS_TASK_FINISH,3).
-define(TREASUREBOWLSTATUS_WHOLE_FINISH,4).

%%单一活动状态
-define(TREASUREBOWL_ACTIVITY_FINISH,1).
-define(TREASUREBOWL_ACTIVITY_UNFINISH,2).

%%活动类型
-define(TREASUREBOWL_ACTIVITY_TYPE_RECHARGE,1).
-define(TREASUREBOWL_ACTIVITY_TYPE_EXCHANGE,2).

%%具体某条活动下的某个奖励领取状态
-define(TREASUREBOWL_DRAW_STATUS_FINISH,1).
-define(TREASUREBOWL_DRAW_STATUS_UNFINISH,2).

-define(ROLE_TREASUREBOWL,role_treasurebowl).

%% ===================Dict Key End   =========================

%% ===================record define begin=====================
-record(state,{wholeactivityID=0,status=?TREASUREBOWLSTATUS_UNBEGINE,statusRef=?undefined,period=?OUT_TREASUREBOWL_PERIOD,periodRef=?undefined,isFinishClearRole=false}).
%% ===================record define end=======================



%% =========================================================================================

%% ===================================================================


start() ->
	{ok,_}=supervisor:start_child(world_sup, 
								  {?MODULE,
								   {?MODULE, start_link, []},
								   permanent, 600000, worker, [?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	init_treasurebowl(),
	erlang:send_after(?dump_interval*1000,self(),dump_data),
	erlang:send_after(?dump_interval*1000, self(), do_hibernate),
	{ok,[]}.

handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info(do_hibernate,State)->
	erlang:send_after(?dump_interval*1000, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(dump_data,State)->
	erlang:send_after(?dump_interval*1000,self(),dump_data),
	check_config_change(),
	do_persist(),
	{noreply,State};

handle_info(Info, State) ->
    case catch do_handle_info(State,Info) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exception:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.

terminate(Reason, State) ->
	do_persist(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

do_handle_info(State,{activity_begin,WholeTreasureBowlID})->
	case get_treasurebowl_state() of
		#state{wholeactivityID=WholeTreasureBowlID}=TState->
			TaskEndTime = util:datetime_to_seconds(data_treasurebowl:get(taskendtime)),
			Ref = erlang:send_after((TaskEndTime-util:now())*1000,?MODULE,{activity_end,WholeTreasureBowlID}),
			NewTState = TState#state{status=?TREASUREBOWLSTATUS_TASK,statusRef=Ref},
			set_treasurebowl_state(NewTState),
			broadcast_treasurebowl_msg(#sc_treasurebowl_open{state=?TREASUREBOWLSTATUS_TASK});
		S->
			?ERR("exist activity_begin msg WholeTreasureBowlID:~w is not equial to Exist ID:~w~n",[WholeTreasureBowlID,S#state.wholeactivityID])
	end,
	{noreply,State};

do_handle_info(State,{activity_end,WholeTreasureBowlID})->
	case get_treasurebowl_state() of
		#state{wholeactivityID=WholeTreasureBowlID}=TState->
			fresh_role_ets(),
			NewTState = TState#state{status=?TREASUREBOWLSTATUS_TASK_FINISH,statusRef=?undefined,isFinishClearRole=true},
			set_treasurebowl_state(NewTState);
		S->
			?ERR("exist activity_end msg WholeTreasureBowlID:~w is not equial to Exist ID:~w~n",[WholeTreasureBowlID,S#state.wholeactivityID])
	end,
	{noreply,State};

do_handle_info(State,{period_change,NewPeriod})->
	TState=#state{status=Status} = get_treasurebowl_state(),
	PeriodList = data_treasurebowl:get(period_list),
	{NextRef,RealNewPeriod} = case lists:keyfind(NewPeriod+1,1,PeriodList) of
		false->
			NewStatus = ?TREASUREBOWLSTATUS_WHOLE_FINISH,
			{?undefined,?OUT_TREASUREBOWL_PERIOD};
		{NextPeriod,Time}->
			Now = util:now(),
			TimeStamp = util:datetime_to_seconds(Time),
			Ref = erlang:send_after((TimeStamp-Now)*1000,?MODULE,{period_change,NextPeriod}),
			NewStatus = Status,
			{Ref,NewPeriod}
	end,
	NewTState = TState#state{period=RealNewPeriod,periodRef=NextRef,status=NewStatus},
	set_treasurebowl_state(NewTState),
	broadcast_treasurebowl_period_update(NewTState),
	{noreply,State};

do_handle_info(State,_Info)->
	{noreply,State}.

do_persist()->
	List = ets:tab2list(?ETS_TREASURE_BOWL),
	db_sql:set_etc(?DB_ETC_KEY_TREASUREBOWL,List).

%====================================================================================================================
init_treasurebowl()->
	{StateData,RoleData} = case db_sql:get_etc(?DB_ETC_KEY_TREASUREBOWL) of
		List when is_list(List)->
			lists:foldl(fun(Elem,{StateAcc,RoleDataAcc})->
				case Elem of
					{?TREASUREBOWLSTATE,V}->
						{V,RoleDataAcc};
					_->
						{StateAcc,[Elem|RoleDataAcc]}
				end
			end,{[],[]},List);
		_->
			{[],[]}
	end,
	init_treasurebowl2(StateData,RoleData).

init_treasurebowl2(#state{wholeactivityID=CurrentWholeTreasureBowlID}=StateData,RoleData) ->
	ConfigTreasureBowlWholeID = data_treasurebowl:get(wholeactivityID),
	if 
		% WholeTreasureBowlID < CurrentTreasureBowlWholeID ->
		CurrentWholeTreasureBowlID < ConfigTreasureBowlWholeID->
			%%当前活动ID小于配置ID，将会清除当前所有的数据，进入下一个活动时间段内
			init_treasurebowl3(#state{},[]);
		true ->
			%%没有新的配置出现，依然维持当前活动数据，需要根据当前时间确定活动阶段
			init_treasurebowl3(StateData,RoleData)
	end;
init_treasurebowl2([],[])->
	init_treasurebowl3(#state{},[]).

%%存在记录的情况下，根据当前时间状态，更新状态
init_treasurebowl3(#state{statusRef=OldStatusRef,periodRef=OldPeriodRef,isFinishClearRole=IsFinishClearRole},RoleData)->
	%%首先集体删除所有的定时器,防止旧的定时器对新的活动的影响
	case OldStatusRef of ?undefined->ignore;_->erlang:cancel_timer(OldStatusRef) end,
	case OldPeriodRef of ?undefined->ignore;_->erlang:cancel_timer(OldPeriodRef) end,

	TaskBeginTime = util:datetime_to_seconds(data_treasurebowl:get(taskbegintime)),
	TaskEndTime = util:datetime_to_seconds(data_treasurebowl:get(taskendtime)),
	Now = util:now(),
	WholeTreasureBowlID = data_treasurebowl:get(wholeactivityID),
	{Status,StatusRef} = 
	if
		Now < TaskBeginTime ->
			SRef = erlang:send_after((TaskBeginTime-Now)*1000,?MODULE,{activity_begin,WholeTreasureBowlID}),
			{?TREASUREBOWLSTATUS_UNBEGINE,SRef};
		Now >= TaskBeginTime andalso Now < TaskEndTime ->
		    SRef = erlang:send_after((TaskEndTime-Now)*1000,?MODULE,{activity_end,WholeTreasureBowlID}),
		    {?TREASUREBOWLSTATUS_TASK,SRef};
		true->
			{?TREASUREBOWLSTATUS_TASK_FINISH,?undefined}
	end,
	{Period,PeriodRef} = case get_period(Now) of
		false->
			{?OUT_TREASUREBOWL_PERIOD,?undefined};
		{FPeriod,FTimeStamp}->
			case FTimeStamp of
				?undefined ->
					PRef = ?undefined;
				_->
					PRef = erlang:send_after((FTimeStamp-Now)*1000,?MODULE,{period_change,FPeriod+1})
			end,
			{FPeriod,PRef}
	end,
	State1 = #state{wholeactivityID=WholeTreasureBowlID,status=Status,statusRef=StatusRef,period=Period,periodRef=PeriodRef},
	ets:delete_all_objects(?ETS_TREASURE_BOWL),
	RoleData1 = case IsFinishClearRole=:=false andalso Status=:=?TREASUREBOWLSTATUS_TASK_FINISH of 
		true->
			State = State1#state{isFinishClearRole=true},
			filter_valid_role(RoleData);
		false->
			State = State1,
			RoleData
	end,
	ets:insert(?ETS_TREASURE_BOWL,{?TREASUREBOWLSTATE,State}),
	[ets:insert(?ETS_TREASURE_BOWL,Pair)||Pair<-RoleData1].

get_period(TimeStamp)->
	Fun = fun({_Period,TarTime})->
		TarTimeStamp = util:datetime_to_seconds(TarTime),
		TarTimeStamp > TimeStamp
    end,
    PeriodList = data_treasurebowl:get(period_list),
    {_FirstPeriod,FirstTime} = hd(PeriodList),
    FirstTimeStamp = util:datetime_to_seconds(FirstTime),
    case TimeStamp < FirstTimeStamp of
    	true->
    		{?OUT_TREASUREBOWL_PERIOD,FirstTimeStamp};
    	false->
    		case util:fun_find(Fun,PeriodList) of
    			false->
    				false;
    			{FPeriod,FTarTime}->
    				{FPeriod-1,util:datetime_to_seconds(FTarTime)}
    		end
   	end.

get_treasurebowl_state()->
	case ets:lookup(?ETS_TREASURE_BOWL,?TREASUREBOWLSTATE) of
		[]->
			?undefined;
		[{_,X}]->
			X
	end.
set_treasurebowl_state(State)->
	ets:insert(?ETS_TREASURE_BOWL,{?TREASUREBOWLSTATE,State}).


get_role_treasurebowl_info(RoleID)->
	#state{status=Status} = get_treasurebowl_state(),
	case ets:lookup(?ETS_TREASURE_BOWL,{?ROLE_TREASUREBOWL,RoleID}) of
		[]->
			case Status of
				%%任务阶段时，会初始化玩家的聚宝盆数据，加入所有的活动
				?TREASUREBOWLSTATUS_TASK->
					RoleTreasureBowlInfo = generate_role_treasurebowl_info(),
					set_role_treasurebowl_info(RoleID,RoleTreasureBowlInfo),
					RoleTreasureBowlInfo;
				_->
					?undefined
			end;
		[{_,X}]->
			X
	end.

set_role_treasurebowl_info(RoleID,TreasurebowlInfo)->
	ets:insert(?ETS_TREASURE_BOWL,{{?ROLE_TREASUREBOWL,RoleID},TreasurebowlInfo}).

broadcast_treasurebowl_period_update(#state{period=Period})->
	Msg = #sc_treasurebowl_update{period=Period},
	broadcast_treasurebowl_msg(Msg).


broadcast_treasurebowl_msg(Msg)->
	RoleList = ets:tab2list(?ETS_ROLE_ONLINE),
	[?unicast(RoleID,Msg)||{RoleID,_}<-RoleList].

check_config_change()->
	#state{wholeactivityID=OldWholeID,statusRef=OldStatusRef,periodRef=OldPeriodRef} = get_treasurebowl_state(),
	case data_treasurebowl:get(wholeactivityID) of
		?undefined->
			ignore;
		NewWholeID->
			if 
				NewWholeID > OldWholeID ->
					%%此处增加对旧活动定时器的取消，防止就活动定时器对新活动的影响
					case OldStatusRef of ?undefined->ignore;_->erlang:cancel_timer(OldStatusRef) end,
					case OldPeriodRef of ?undefined->ignore;_->erlang:cancel_timer(OldPeriodRef) end,
					init_treasurebowl3(#state{},[]);
				true->
					ignore
			end
	end.

add_pay_info(RoleID,PayGold)->
	case get_treasurebowl_state() of
		#state{status=?TREASUREBOWLSTATUS_TASK}->
			TreasurebowlActivityL = get_role_treasurebowl_info(RoleID),
			case update_role_treasurebowl(TreasurebowlActivityL,PayGold) of
				{update,NewFA,Other}->
					set_role_treasurebowl_info(RoleID,[NewFA|Other]);
				_->
					ignore
			end;
		_->
			ignore
	end.

%%活动任务阶段完成之后，将会调用该函数来删除ETS表中没有参加活动的玩家数据
fresh_role_ets()->
	EtsList = ets:tab2list(?ETS_TREASURE_BOWL),
	lists:foreach(fun({Key,Value}=Pair)->
		case Key of
			{?ROLE_TREASUREBOWL,RoleID}->
				case [E||#treasurebowl_activity{activitystate=?TREASUREBOWL_ACTIVITY_FINISH}=E<-Value] of
					[]->
						ets:delete(?ETS_TREASURE_BOWL,Key);
					L ->
						set_role_treasurebowl_info(RoleID,L)
				end;
			_->
				ets:insert(?ETS_TREASURE_BOWL,Pair)
		end
	end,EtsList).

filter_valid_role(RoleDataL) ->
	lists:filter(fun({_Key,Value})-> [E||#treasurebowl_activity{activitystate=?TREASUREBOWL_ACTIVITY_FINISH}=E<-Value] =/= [] end,RoleDataL).


update_role_treasurebowl(TreasurebowlActivityL,PayGold)->
	{F,Other} = lists:foldl(fun(#treasurebowl_activity{activityID=ActivityID,activitystate=ActivityState}=E,{FAcc,OAcc})->
			case FAcc of
				{return,_E}->
					{FAcc,[E|OAcc]};
				_->
					case data_treasurebowl:get({data_treasurebowl_activity,ActivityID}) of
						{?TREASUREBOWL_ACTIVITY_TYPE_RECHARGE,PayGold,_Reward}->
							case ActivityState of
								?TREASUREBOWL_ACTIVITY_UNFINISH->
									{{return,E},OAcc};
								?TREASUREBOWL_ACTIVITY_FINISH->
									{FAcc,[E|OAcc]}
							end;
						_->
							{FAcc,[E|OAcc]}
					end
			end
		end,{[],[]},TreasurebowlActivityL),	
	case F of
		{return,FA}->
			{update,FA#treasurebowl_activity{activitystate=?TREASUREBOWL_ACTIVITY_FINISH},Other};
		_->
			false
	end.

%%根据当前配置，默认生成一个玩家的所有活动数据
generate_role_treasurebowl_info()->
	ActivityKeyL = [Key||{data_treasurebowl_activity,_}=Key<-data_treasurebowl:get_list()],
	lists:foldl(fun({_,ActivityID} =ActivityKey,Acc)->
		{_Type,_Condition,DrawList} = data_treasurebowl:get(ActivityKey),
			TreasurebowlDrawL = generate_role_treasurebowl_draw(DrawList),
			[#treasurebowl_activity{activityID=ActivityID,drawlist=TreasurebowlDrawL,activitystate=?TREASUREBOWL_ACTIVITY_UNFINISH}|Acc]
		end,[],ActivityKeyL).

generate_role_treasurebowl_draw(L) when is_list(L)->
	[generate_role_treasurebowl_draw(E)||E<-L];
generate_role_treasurebowl_draw(DrawID)->
	{Period,_Reward} = data_treasurebowl:get({data_treasurebowl_draw,DrawID}),
	#treasurebowl_drawunit{drawID=DrawID,period=Period,state=?TREASUREBOWL_DRAW_STATUS_UNFINISH}.

%%将role_treasurebowl模块中的协议处理函数转移到此处，方便使用treasurebowl模块中定义的各种宏，该部分的处理依然在玩家进程中
cs_treasurebowl_info()->
	RoleID = role_data:get_roleID(),
	#state{period=Period} = treasurebowl_server:get_treasurebowl_state(),
	TreasurebowlActivityL = treasurebowl_server:get_role_treasurebowl_info(RoleID),
	ActivityList = role_treasurebowl:treasurebowl_activity2p_treasurebowl_activity(TreasurebowlActivityL),
	PeriodList = data_treasurebowl:get(period_list),
	PeriodEndTimeList = [util:datetime_to_seconds(Time)||{_Period,Time}<-PeriodList],
	TaskBeginTime = util:datetime_to_seconds(data_treasurebowl:get(taskbegintime)),
	TaskEndTime = util:datetime_to_seconds(data_treasurebowl:get(taskendtime)),
	Content = data_treasurebowl:get(content),
	%%根据前端需求，如果玩家没哟参加活动，period设置成0
	NewPeriod = case length(ActivityList) of 0->0;_->Period end,
	?sendself(#sc_treasurebowl_info{result=1,activitylist=ActivityList,period=NewPeriod,periodendtime=PeriodEndTimeList,activitybegin=TaskBeginTime,activityend=TaskEndTime,content=list_to_binary(Content)}).


cs_treasurebowl_exchange(ActivityID)->
	case get_treasurebowl_state() of
		#state{status=?TREASUREBOWLSTATUS_TASK,wholeactivityID=WholeTreasureBowlID}->
			Role = #role{roleID=RoleID} = role_data:get_roleInfo(),
			TreasurebowlActivityL = get_role_treasurebowl_info(RoleID),
			case lists:keytake(ActivityID,#treasurebowl_activity.activityID,TreasurebowlActivityL) of
				false->
					{false,3};
				{_,#treasurebowl_activity{activitystate=?TREASUREBOWL_ACTIVITY_FINISH},_Other}->
					{false,4};
				{_,#treasurebowl_activity{activitystate=?TREASUREBOWL_ACTIVITY_UNFINISH}=F,Other}->
					case data_treasurebowl:get({data_treasurebowl_activity,ActivityID}) of
						{?TREASUREBOWL_ACTIVITY_TYPE_EXCHANGE,Condition,_Reward}->
							case role_lib:check_money(Role,gold,Condition) of
								false->
									{false,5};
								true->
									role_lib:deduct_money_f(Role, gold,Condition,?MONEY_DEC_TYPE_TREASUREBOWL,WholeTreasureBowlID,integer_to_list(ActivityID)),
									NF = F#treasurebowl_activity{activitystate=?TREASUREBOWL_ACTIVITY_FINISH},
									set_role_treasurebowl_info(RoleID,[NF|Other]),
									true
							end;
						_->
							{false,6}
					end
			end;
		_->
			{false,2}
	end. 

cs_treasurebowl_draw(ActivityID,DrawID)->
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	#state{period=Period} = get_treasurebowl_state(),
	TreasurebowlActivityL = get_role_treasurebowl_info(RoleID),
	case lists:keytake(ActivityID,#treasurebowl_activity.activityID,TreasurebowlActivityL) of
		false->
			{false,2};
		{_,#treasurebowl_activity{activitystate=?TREASUREBOWL_ACTIVITY_FINISH,drawlist=DrawList}=AF,OtherActivity}->
			case lists:keytake(DrawID,#treasurebowl_drawunit.drawID,DrawList) of
				false->
					{false,2};
				{_,#treasurebowl_drawunit{period=Period,state=?TREASUREBOWL_DRAW_STATUS_UNFINISH}=DF,OtherDraw}->
					{_Period,Reward} = data_treasurebowl:get({data_treasurebowl_draw,DrawID}),
					role_reward:handle_sys_reward(Role,Reward, ?MONEY_ADD_TYPE_TREASUREBOWL, ActivityID,integer_to_list(DrawID)),
					NDF = DF#treasurebowl_drawunit{state=?TREASUREBOWL_DRAW_STATUS_FINISH},
					NAF = AF#treasurebowl_activity{drawlist=[NDF|OtherDraw]},
					NewTreasurebowlActivityL = [NAF|OtherActivity],
					set_role_treasurebowl_info(RoleID,NewTreasurebowlActivityL),
					{true,Reward};
				{_,#treasurebowl_drawunit{state=?TREASUREBOWL_DRAW_STATUS_UNFINISH},_Other}->
					{false,3};
				{_,#treasurebowl_drawunit{period=Period},_Other}->
					{false,5};
				_->
					{false,6}
			end;
		_->			
			{false,4}
	end.