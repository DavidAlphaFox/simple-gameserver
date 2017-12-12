%% @author zcl
%% @doc 签到
%% Created 2015-11-30
-module(consume_back_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================
-define(dump_interval,300).
-define(CONSUME_ACTIVITY_STATE,activitystate).
-define(begintimeref,begintimeref).
-define(REBACKTYPE_GOLD,0).
-define(REBACKTYPE_COIN,1).
-define(REBACKTYPE_TICKET,2).
%% ===================Dict Key End   =========================

%% ===================record define begin=====================
-record(state,{activityid=0,cleartimeref=?undefined,begintimestamp=0,endtimestamp=0,ratelist=[],rebacktype=0,activitytype=0}).
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
	%%将当前活动的状态配置放入ets表中，方便玩家进程访问，不放入进程状态中
	init_consume_back_data(),
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

% %% ====================================================================
do_persist()->
	List = ets:tab2list(?ETS_CONSUME_REABCK),
	db_sql:set_etc(?DB_ETC_KEY_CONSUME_BACK,List).

do_handle_info(State,{send_consume_back_reward,ActivityID})->
	?INFO("begin to send reback reward~n"),
	send_back_reward_by_ets(ActivityID),
	clear_consume_back_data(ActivityID),
	{noreply,#state{}};
do_handle_info(State,{begin_consume_activity,ActivityID})->
	?INFO("broacast consume back begin message~n"),
	broacast_consume_begin(ActivityID),
	{noreply,State};
do_handle_info(State,Info)->
	{noreply,State}.

%%=======================================================================
%%初始化消费返利持久化数据
init_consume_back_data()->
	{NewActivityData,NewRoleData} = case db_sql:get_etc(?DB_ETC_KEY_CONSUME_BACK) of
		List when is_list(List)->
			lists:foldl(fun(Elem,{StateAcc,RoleDataAcc})->
				case Elem of
					{?CONSUME_ACTIVITY_STATE,V}->
						{V,RoleDataAcc};
					_->
						{StateAcc,[Elem|RoleDataAcc]}
				end
			end,{[],[]},List);
		_->
			{[],[]}
	end,
	State = init_consume_back_data2(NewActivityData,NewRoleData),
	ets:insert(?ETS_CONSUME_REABCK,{activitystate,State}).

%%没有历史活动，直接读取配置确定是否开启返利活动
init_consume_back_data2([],[])->
	{ActivityBeginTime,ActivityEndTime} = data_consume_back:get(open_time),
	ActivityConfigEndTimeStamp = util:datetime_to_seconds(ActivityEndTime),
	ActivityConfigBeginTimeStamp = util:datetime_to_seconds(ActivityBeginTime),
	ActivityConfigID = data_consume_back:get(activity_id),
	ActivityRateList = data_consume_back:get(reback_rate),
	RebackType = data_consume_back:get(rebacktype),
	ActivityType= data_consume_back:get(activity_type),
	NowSec = util:now(),
	%%在提前2个持久化检测时间前就加入活动，防止活动开启时没能按时开启
	case NowSec >= ActivityConfigBeginTimeStamp-2*?dump_interval andalso NowSec =< ActivityConfigEndTimeStamp of
		true->
			init_consume_back_data3(ActivityConfigID,ActivityConfigEndTimeStamp,[],ActivityRateList,ActivityConfigBeginTimeStamp,RebackType,ActivityType);
		false->
			#state{}
	end;
init_consume_back_data2([],RoleData)->
	?ERR("illegal result: RoleData:~w ~n",[RoleData]),
	init_consume_back_data2([],[]);

%%有历史返利活动,需要判断旧的活动是否需要关闭并且重新开始新的活动
init_consume_back_data2(ActivityData,RoleData) when is_record(ActivityData,state)->
	#state{activityid=ActivityID,cleartimeref=EndTimeRef,begintimestamp=OldBeginTimeStamp,endtimestamp=OldEndTimeStamp,ratelist=OldRateList,rebacktype=OldRebackType,activitytype=OldActivityType} = ActivityData,
	case EndTimeRef of
		?undefined->
			ignore;
		_ ->
			erlang:cancel_timer(EndTimeRef)
	end,
	{ActivityBeginTime,ActivityEndTime} = data_consume_back:get(open_time),
	ActivityConfigEndTimeStamp = util:datetime_to_seconds(ActivityEndTime),
	ActivityConfigBeginTimeStamp = util:datetime_to_seconds(ActivityBeginTime),
	ActivityConfigID = data_consume_back:get(activity_id),
	ActivityRateList = data_consume_back:get(reback_rate),
	RebackType = data_consume_back:get(rebacktype),
	ActivityType = data_consume_back:get(activity_type),
	NowSec = util:now(),
	case ActivityID =:= ActivityConfigID of
		false->
			%%配置文件和当前存在的活动不一致
			case NowSec < OldEndTimeStamp of
				true->
					%%旧的活动还没有完成,将会维持原来的活动
					init_consume_back_data3(ActivityID,OldEndTimeStamp,RoleData,OldRateList,OldBeginTimeStamp,OldRebackType,OldActivityType);
				false->
					%%旧的活动已经完成，需要发送奖励
					send_back_reward(RoleData),
					clear_consume_back_data(ActivityID),
					%%直接开启新的返利活动（从配置文件中读取初始化)
					case NowSec =< ActivityConfigEndTimeStamp andalso NowSec >= ActivityConfigBeginTimeStamp-2*?dump_interval of
						true->
							init_consume_back_data3(ActivityConfigID,ActivityConfigEndTimeStamp,[],ActivityRateList,ActivityConfigBeginTimeStamp,RebackType,ActivityType);
						false->
							#state{}
					end
			end;
		true->
			case NowSec >= OldEndTimeStamp of
				true->
					%%活动已经结束，发送返还钻石
					send_back_reward(RoleData),
					clear_consume_back_data(ActivityID),
					#state{};
				false->
					init_consume_back_data3(ActivityConfigID,OldEndTimeStamp,RoleData,OldRateList,OldBeginTimeStamp,OldRebackType,OldActivityType)
			end
	end;
init_consume_back_data2(ActivityData,RoleData)->
	{state,ActivityID,CleartimeRef,Begintimestamp,Endtimestamp,Ratelist} = ActivityData,
	init_consume_back_data2(#state{activityid=ActivityID,cleartimeref=CleartimeRef,begintimestamp=Begintimestamp,endtimestamp=Endtimestamp,ratelist=Ratelist,rebacktype=0,activitytype=0},RoleData).

init_consume_back_data3(ActivityID,EndTimeStamp,RoleData,RateList,BeginTimeStamp,RebackType,ActivityType)->
	NowSec = util:now(),
	EndInterval = EndTimeStamp - NowSec,
	EndTimeRef = erlang:send_after(EndInterval * 1000,self(),{send_consume_back_reward,ActivityID}),
	case get_begin_timeref() of
		?undefined->
			ignore;
		BeginTimeRef->
			erlang:cancel_timer(BeginTimeRef)
	end,
	BeginInterval = BeginTimeStamp-NowSec,
	case BeginInterval >= 0 of
		true->
			TimeRef = erlang:send_after(BeginInterval*1000,self(),{begin_consume_activity,ActivityID});
		false->
			TimeRef = ?undefined
	end,
	set_begin_timeref(TimeRef),
	lists:foreach(fun(Elem)->
		ets:insert(?ETS_CONSUME_REABCK,Elem)
	end,RoleData),
	#state{activityid=ActivityID,cleartimeref=EndTimeRef,begintimestamp=BeginTimeStamp,endtimestamp=EndTimeStamp,ratelist=RateList,rebacktype=RebackType,activitytype=ActivityType}.

send_back_reward(RoleData)->
	lists:foreach(fun(RoleElem)->
		send_back_reward_to_role(RoleElem)
	end,RoleData).

send_back_reward_to_role({?CONSUME_ACTIVITY_STATE,State})->
	ignore;
send_back_reward_to_role(RoleElem)->
	State = get_consume_state(),
	{RoleID,ConsumeList} = RoleElem,
	RebackList = [RebackNum||{_Time,_ConsumeNum,RebackNum}<-ConsumeList],
	RebackSum = lists:sum(RebackList),
	Reward = generate_reback_reward(RebackSum,State#state.rebacktype),
	StateMsg = #sc_activity_consume_state{state=0},
	?unicast(RoleID,StateMsg),
	mail_server:send_sys_mail(RoleID,?MAIL_CONSUME_BACK, [RebackSum], "", Reward).

generate_reback_reward(Num,RebackType)->
	case RebackType of
		?REBACKTYPE_GOLD->
			#sell_reward{gold=Num};
		?REBACKTYPE_COIN->
			#sell_reward{coin=Num};
		?REBACKTYPE_TICKET->
			#sell_reward{item=[#new_item{itemTypeID=20050,itemNum=Num,itemRank=0,itemLevel=1}]};
		_->
			?ERR("undefined rebacktype:~w ~n",[RebackType]),
			#sell_reward{}
	end.
clear_consume_back_data(ActivityID)->
	#state{activityid=EtsActivityID} = get_consume_state(),
	case EtsActivityID=:= ActivityID of
		true->
			ets:delete_all_objects(?ETS_CONSUME_REABCK);
		false->
			?ERR("clear consume back data error existID:~w clearID:~w ~n",[EtsActivityID,ActivityID])
	end.

is_consume_back_open()->
	State = get_consume_state(),
	case State of
		#state{begintimestamp=?undefined,endtimestamp=?undefined}->
			false;
		#state{begintimestamp=BeginTimeStamp,endtimestamp=EndTimeStamp}->
			NowSec = util:now(),
			NowSec >= BeginTimeStamp andalso NowSec =< EndTimeStamp
	end.
add_consume_record(RoleID,Num)->
	add_consume_record(RoleID,Num,?REBACKTYPE_GOLD).
add_consume_record(RoleID,Num,ConsumeType)->
	State = get_consume_state(),
	case {is_consume_back_open(),State#state.rebacktype=:=ConsumeType} of
		{false,_}->
			ignore;
		{true,false}->
			ignore;
		{true,true}->
			Date = erlang:date(),
			Rate = get_consume_back_rate(Date),
			case ets:lookup(?ETS_CONSUME_REABCK,RoleID) of
				[]->
					ets:insert(?ETS_CONSUME_REABCK,{RoleID,[{date2int(Date),Num,trunc(Num*Rate/100)}]});
				[{RoleID,ConsumeList}]->
					case lists:keytake(date2int(Date),1,ConsumeList) of
						false->
							ets:insert(?ETS_CONSUME_REABCK,{RoleID,[{date2int(Date),Num,trunc(Num*Rate/100)}|ConsumeList]});
						{_Value,{_Date,OldNum,OldReback},Other}->
							NewNum = OldNum+Num,
							ets:insert(?ETS_CONSUME_REABCK,{RoleID,[{date2int(Date),NewNum,trunc(NewNum*Rate/100)}|Other]})
					end
			end
	end.

get_consume_back_rate(Date)->
	#state{ratelist=RateList,begintimestamp=BeginTimeStamp} = get_consume_state(),
	if
		is_number(RateList) ->
			RateList;
		true->
			case RateList of
				[]->
					10;
				_->
					{D,_} = util:seconds_to_datetime(BeginTimeStamp),
					BeginZero = util:datetime_to_seconds({D,{0,0,0}}),
					DateZero = util:datetime_to_seconds({Date,{0,0,0}}),
					Index = ((DateZero - BeginZero) div ?ONE_DAY_SECONDS)+1,
					lists:nth(Index,RateList)
			end
	end.

%%定期读取配置文件  确定配置文件是否有更改，如果出现配置文件更改，则更改相应状态
check_config_change()->
	?INFO("load data_consume_back config~n"),
	#state{activityid=CBID,cleartimeref=ClearTimeRef,endtimestamp=EndTimeStamp}=get_consume_state(),
	case data_consume_back:get(activity_id) of
		?undefined->
			ignore;
		ActivityID->
			case ActivityID=:=CBID of
				true->
					ignore;
				false->
					NowSec = util:now(),
					{ActivityBeginTime,ActivityEndTime} = data_consume_back:get(open_time),
					ActivityConfigEndTimeStamp = util:datetime_to_seconds(ActivityEndTime),
					ActivityConfigBeginTimeStamp = util:datetime_to_seconds(ActivityBeginTime),
					ActivityRateList = data_consume_back:get(reback_rate),
					RebackType = data_consume_back:get(rebacktype),
					ActivityType = data_consume_back:get(activity_type),
					NowSec = util:now(),
					case NowSec >=ActivityConfigBeginTimeStamp-2*?dump_interval andalso NowSec =< ActivityConfigEndTimeStamp of
						true->
							%%新活动进入开启阶段了，直接关闭旧的活动
							case NowSec < EndTimeStamp of
								true->
									%%旧的活动还没有结束
									send_back_reward_by_ets(CBID),
									clear_consume_back_data(CBID),
									case ClearTimeRef of
										?undefined->
											ignore;
										_->
											erlang:cancel_timer(ClearTimeRef)
									end;
								false->
									%%旧的活动已经完成
									clear_consume_back_data(CBID)
							end,
							set_consume_state(init_consume_back_data3(ActivityID,ActivityConfigEndTimeStamp,[],ActivityRateList,ActivityConfigBeginTimeStamp,RebackType,ActivityType));
						false->
							%%新活动未开启，不需要修改当前的状态
							ignore
					end
			end
	end.	

%%依据ETS表中的信息，向对应玩家发送奖励
send_back_reward_by_ets(ActivityID)->
	#state{activityid=EtsActivityID} = get_consume_state(),
	case EtsActivityID=:=ActivityID of
		true->
			List = ets:tab2list(?ETS_CONSUME_REABCK),
			send_back_reward(List);
		false->
			?ERR("send back reward EtsActivityID:~w ActivityID:~w ~n",[EtsActivityID,ActivityID])
	end.

get_consume_back_idlist()->
	{ActivityBegin,ActivityEnd} = data_consume_back:get(open_time),
	get_consume_back_idlist2(ActivityBegin,ActivityEnd).
get_consume_back_idlist2({BeginDate,_}=BeginTime,{EndDate,_}=EndTime)->
	EndTimeStamp = util:datetime_to_seconds({EndDate,{23,59,59}}),
	BeginTimeStampZero = util:datetime_to_seconds({BeginDate,{0,0,0}}),
	case util:datetime_to_seconds(EndTime)>= util:datetime_to_seconds(BeginTime) of
		true->
			get_consume_back_idlist3(BeginTimeStampZero,EndTimeStamp,[]);
		false->
			[]
	end.
get_consume_back_idlist3(BeginZeroTimeStamp,EndTimeStamp,Acc)->
	case BeginZeroTimeStamp < EndTimeStamp of
		true->
			{Date,_} = util:seconds_to_datetime(BeginZeroTimeStamp),
			get_consume_back_idlist3(BeginZeroTimeStamp+?ONE_DAY_SECONDS,EndTimeStamp,[date2int(Date)|Acc]);
		false->
			lists:reverse(Acc)
	end.

%%构造玩家的消费返利数据,会根据开启和关闭时间 构造所有的数据，没有的情况下，设置默认值
get_role_consume_back_info(RoleID)->
	{RoleID,ConsumeList} = 
		case get_role_consume_back_from_ets(RoleID) of
			[]->
				{RoleID,[]};
			X ->
				X
		end,
	% OmitConsumeList = get_consume_back_idlist(),
	#state{begintimestamp=Begintimestamp,endtimestamp=EndTimeStamp,ratelist=Ratelist,rebacktype=RebackType,activitytype=ActivityType} = get_consume_state(),
	OmitConsumeList = get_consume_back_idlist2(util:seconds_to_datetime(Begintimestamp),util:seconds_to_datetime(EndTimeStamp)),
	{PConsumeUnitList,TotalConsume,TotalReback} = lists:foldr(fun(ID,{UnitAcc,ConsumeAcc,RebackAcc})->
		%%此处存在一个bug，如果加载进入系统中的活动和配置活动不一致的情况下，这个特殊显示将会出现错误,感觉不好处理了
		{PictureID,Description} = case data_consume_back:get({special_day,ID}) of
			?undefined->
				%%默认图片ID是1000
				{1000,<<>>};
			{DescriptionContent,PictureIDConfig}->
				{PictureIDConfig,list_to_binary(DescriptionContent)}
		end,
		case lists:keyfind(ID,1,ConsumeList) of
			false->
				{[#p_consume_unit{unitid=ID,consumenum=0,rebacknum=0,pictureid=PictureID,description=Description}|UnitAcc],ConsumeAcc,RebackAcc};
			{ID,UnitConsume,UnitReback}->
				{[#p_consume_unit{unitid=ID,consumenum=UnitConsume,rebacknum=UnitReback,pictureid=PictureID,description=Description}|UnitAcc],ConsumeAcc+UnitConsume,RebackAcc+UnitReback}
		end
	end,{[],0,0},OmitConsumeList),
	% {ActivityBeginTime,ActivityEndTime} = data_consume_back:get(open_time),
	EntrancePos  = data_consume_back:get(entrancepos),
	% RateList = data_consume_back:get(reback_rate),
	RebackRate = case is_list(Ratelist) of true->hd(Ratelist);false->Ratelist end,
	% ActivityType = data_consume_back:get(activity_type),
	#sc_activity_consume_reback{result=1,activitytype=ActivityType,consumelist=PConsumeUnitList,totalconsume=TotalConsume,totalreback=TotalReback,rebackrate=RebackRate,
		entrancepos=EntrancePos,begintime=Begintimestamp,endtime=EndTimeStamp,rebacktype=RebackType}.

date2int({Y,M,D})->

	Y*10000+M*100+D.

get_role_consume_back_from_ets(RoleID)->
	case ets:lookup(?ETS_CONSUME_REABCK,RoleID) of
		[]->
			[];
		[X]->
			X
	end.

get_consume_state()->
	case ets:lookup(?ETS_CONSUME_REABCK,?CONSUME_ACTIVITY_STATE) of
		[]->
			#state{};
		[{?CONSUME_ACTIVITY_STATE,X}] ->
			X
	end.
set_consume_state(X)->
	ets:insert(?ETS_CONSUME_REABCK,{?CONSUME_ACTIVITY_STATE,X}).

get_begin_timeref()->
	get(?begintimeref).

set_begin_timeref(TimeRef)->
	put(?begintimeref,TimeRef).

broacast_consume_begin(ActivityID)->
	#state{activityid=CAID} = get_consume_state(),
	case ActivityID=:=CAID of
		false->
			?INFO("broacast consume back begin with activity not accord ActivityID:~w ExistActivityID:~w ~n",[ActivityID,CAID]);
		true->
			Msg = #sc_activity_consume_state{state=1},
			broacast_consume_msg(Msg)
	end.

broacast_consume_msg(Msg)->
	RoleList = ets:tab2list(?ETS_ROLE_ONLINE),
	[?unicast(RoleID,Msg)||{RoleID,_}<-RoleList].

test_close_consume_reback()->
	#state{activityid=CBID,endtimestamp=EndTimeStamp,cleartimeref=ClearTimeRef} = get_consume_state(),
	case CBID =/=?undefined andalso EndTimeStamp > util:now() of
		true->
			send_back_reward_by_ets(CBID),
			clear_consume_back_data(CBID),
			case ClearTimeRef of
				?undefined->
					ignore;
				_->
					erlang:cancel_timer(ClearTimeRef)
			end,
			Msg = #sc_activity_consume_state{state=0},
			broacast_consume_msg(Msg);
		false->
			ignore
	end.