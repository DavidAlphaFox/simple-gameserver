%% @author zcl
%% @doc 消费 充值排行榜
%% Created 2017-12-6
%%================================================
%%消费和充值排行榜都只保存前50的玩家数据，所有的玩家充值消费数据都将单独存储在ETS中(考虑到前50的列表会经常被用来做显示以及排序，故两个列表页保留玩家的详细信息)
%%更新玩家数据时，首先根据玩家ID找到对应的数据，然后修改更新然后确定是否需要更新前50的数据
%%================================================
-module(consumerank_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
 -include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================
-define(STATUS_CLOSE,0).    %%该阶段不存在对应的活动
-define(STATUS_OPEN,1).     %%可以进行充值消费统计阶段(奖励的发放在OPEN状态切换成FINISH状态时发放，完成发放之后切换成FINISH状态)
-define(STATUS_FINISH,2).   %%最后的展示阶段
-define(PAY_RANK_TYPE,1).   
-define(CONSUME_RANK_TYPE,2).

-define(dump_interval,300). %%持久化间隙
-define(CONSUMERANK_STATE,consumerank_state).
-define(CONSUMERANKLIST,consumeranklist).
-define(PAYRANKLIST,payranklist).
%% ===================Dict Key End   =========================

%% ===================record define begin=====================
-record(state,{status=?STATUS_CLOSE,finishTimeRef=?undefined,finishTime=0}).
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
	%%将当前活动的状态配置以及玩家数据放入ets表中，方便玩家进程访问，不放入进程状态中
	init_consumerank_data(),
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
	do_persist(),
	{noreply,State};

handle_info(Info, State) ->
    case catch do_handle_info(State,Info) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exception ->
            ?ERR("Exception:~w~n Info:~w~n State:~w~n", [Exception, Info, State]),
            {noreply, State}
    end.

terminate(Reason, State) ->
	do_persist(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
%%=======================================================================
do_handle_info(State,{status_change,Status})->
	case get_consumerank_state() of
		#state{status=Status}=S->
			NowSec = util:now(),
			case Status of
				?STATUS_OPEN->
					%%从开始阶段切换到完成阶段
					send_reward(),
					NextStatusBeginTimeStamp = get_consumerank_status_begin_timestamp(?STATUS_CLOSE),
					FinishTimeRef = erlang:send_after((NextStatusBeginTimeStamp-NowSec)*1000,self(),{status_change,?STATUS_FINISH}),
					NS = S#state{status=?STATUS_FINISH,finishTimeRef=FinishTimeRef,finishTime=NextStatusBeginTimeStamp},
					set_consumerank_state(NS);
				?STATUS_FINISH->
					%%从FINISH状态切换到CLOSE状态
					NS = S#state{status=?STATUS_CLOSE,finishTimeRef=?undefined,finishTime=0},
					set_consumerank_state(NS);
				Status->
					?ERR("unexpect change_status:~w ~w~n",[Status,S])
			end;
		S->
			?ERR("status_change S:~w Status:~w ~n",[S,Status])
	end,
	{noreply,Status};
do_handle_info(State,Info)->
	?ERR("undefined Info:~w ~w ~n",[Info,State]).
do_persist()->
	% State = get_consumerank_state(),
	% ConsumeList = get_consumerank_list(),
	% PayList = get_payrank_list(),
	% MatchSpec = ets:fun2ms(fun({M,N}) when is_number(M) -> N end),
	% RoleData = ets:select(?ETS_CONSUMERANK,MatchSpec),
	% db_sql:set_etc(?DB_ETC_KEY_CONSUMERANK,{State,RoleData,ConsumeList,PayList}).
	List = ets:tab2list(?ETS_CONSUMERANK),
	db_sql:set_etc(?DB_ETC_KEY_CONSUMERANK,List).
%%=======================================================================
%%初始化消费排行的数据(包括活动状态以及玩家数据，将对应的数据放入到ETS表中)
init_consumerank_data()->
	{DBState,DBRoleDate,DBConsumeList,DBPayList} = case db_sql:get_etc(?DB_ETC_KEY_CONSUMERANK) of
		List when is_list(List)->
			lists:foldl(fun(Elem,{StateAcc,RoleDataAcc,ConsumeRankAcc,PayRankAcc})->
				case Elem of
					{?CONSUMERANK_STATE,V}->
						{V,RoleDataAcc,ConsumeRankAcc,PayRankAcc};
					{?CONSUMERANKLIST,V}->
						{StateAcc,RoleDataAcc,V,PayRankAcc};
					{?PAYRANKLIST,V}->
						{StateAcc,RoleDataAcc,ConsumeRankAcc,V};
					_->
						{StateAcc,[Elem|RoleDataAcc],ConsumeRankAcc,PayRankAcc}
				end
			end,{[],[],[],[]},List);
		_->
			{[],[],[],[]}
	end,
	init_consumerank_data2(DBState,DBRoleDate,DBConsumeList,DBPayList).

%%判断数据状态是否是已经完成活动状态，如果是只初始化状态，不初始化玩家数据以及两个排行榜
init_consumerank_data2(DBState,DBRoleDate,DBConsumeList,DBPayList)->
	{RealState,RoleData,ConsumeList,PayList} = init_consumerank_state(DBState,DBRoleDate,DBConsumeList,DBPayList),
	set_consumerank_state(RealState),
	set_consumerank_list(ConsumeList),
	set_payrank_list(PayList),
	init_consume_roledata(RoleData).

init_consumerank_state(State,DBRoleDate,DBConsumeList,DBPayList) when is_record(State,state)->
	#state{status=Status,finishTimeRef=FinishTimeRef} = State,
	%%首先取消所有的状态切换定时器
	case FinishTimeRef of
		?undefined->
			ignore;
		_->
			erlang:cancel_timer(FinishTimeRef)
	end,
	case Status of
		?STATUS_CLOSE->
			%%对应活动已经关闭
			{State,[],[],[]};
		?STATUS_OPEN->
			NowSec = util:now(),
			case calculate_consumerank_status(NowSec) of
				?STATUS_CLOSE->
					%%关服期间已经跳过了整个活动,补发所有的玩家奖励，进入完成阶段
					send_consumerank_reward(DBRoleDate,DBConsumeList,DBPayList),
					{State#state{status=?STATUS_CLOSE,finishTimeRef=?undefined},[],[],[]};
				{?STATUS_OPEN,FinishTime}->
					NewFinishTimeRef = erlang:send_after((FinishTime-NowSec)*1000,self(),{status_change,?STATUS_OPEN}),
					{State#state{finishTimeRef=NewFinishTimeRef,finishTime=FinishTime},DBRoleDate,DBConsumeList,DBPayList};
				{?STATUS_FINISH,FinishTime}->
					%%关服期间已经跳过了OPEN状态，发放奖励
					{RoleDate1,ConsumeList1,PayList1} = send_consumerank_reward(DBRoleDate,DBConsumeList,DBPayList),
					NewFinishTimeRef = erlang:send_after((FinishTime-NowSec)*1000,self(),{status_change,?STATUS_FINISH}),
					{State#state{status=?STATUS_FINISH,finishTimeRef=NewFinishTimeRef,finishTime=FinishTime},RoleDate1,ConsumeList1,PayList1}
			end;
		?STATUS_FINISH->
			NowSec = util:now(),
			case calculate_consumerank_status(NowSec) of
				{?STATUS_FINISH,FinishTime}->
					NewFinishTimeRef = erlang:send_after((FinishTime-NowSec)*1000,self(),{status_change,?STATUS_FINISH}),
					{State#state{finishTimeRef=NewFinishTimeRef,finishTime=FinishTime},DBRoleDate,DBConsumeList,DBPayList};
				?STATUS_CLOSE->
					{State#state{status=?STATUS_CLOSE},[],[],[]};
				S->
					?ERR("unexpect Status : ~w ~w ~w ~w ~w ~n",[?STATUS_FINISH,S,DBRoleDate,DBConsumeList,DBPayList]),
					{#state{status=?STATUS_CLOSE},[],[],[]}
			end
	end;
%%版本更新前的旧服或者新的新服
init_consumerank_state([],[],[],[])->
	NowSec = util:now(),
	case calculate_consumerank_status(NowSec) of
		{?STATUS_OPEN,FinishTime}->
			%%这种情况应该是新服开服第一次
			FinishTimeRef = erlang:send_after((FinishTime-NowSec)*1000,self(),{status_change,?STATUS_OPEN}),
			{#state{finishTimeRef=FinishTimeRef,status=?STATUS_OPEN,finishTime=FinishTime},[],[],[]};
		{?STATUS_FINISH,FinishTime}=E->
			%%该情况应该是异常了
			?ERR("unexpect State:~w ~n",[E]),
			FinishTimeRef = erlang:send_after((FinishTime-NowSec)*1000,self(),{status_change,?STATUS_FINISH}),
			{#state{finishTimeRef=FinishTimeRef,status=?STATUS_FINISH,finishTime=FinishTime},[],[],[]};
		?STATUS_CLOSE->
			%%这种情况应该对应的是老区服，自始至终都没有这个活动
			{#state{status=?STATUS_CLOSE},[],[],[]}
	end.

%%初始化所有玩家的充值消费记录
init_consume_roledata(RoleData)->
	RoleData1 = [{RoleID,E}||#consume_pay_unit{roleID=RoleID}=E<-RoleData],
	ets:insert(?ETS_CONSUMERANK,RoleData1).

get_server_open_time()->
	util:datetime_to_seconds(data_setting:get(serverOpenTime)).

calculate_consumerank_status(TimeStamp)->
	ServerOpenTime = get_server_open_time(),
	{CloseRelTime,FinishRelTime} = data_consumerank:get(time_period),
	CloseRelTimeStamp = ServerOpenTime + util:dateTimeIntervel_to_second(CloseRelTime),
	FinishRelTimeStamp = CloseRelTimeStamp + util:dateTimeIntervel_to_second(FinishRelTime),
	if
		TimeStamp < CloseRelTimeStamp ->
			{?STATUS_OPEN,CloseRelTimeStamp};
		TimeStamp >= CloseRelTimeStamp andalso TimeStamp < FinishRelTimeStamp ->
		    {?STATUS_FINISH,FinishRelTimeStamp};
		TimeStamp >= FinishRelTimeStamp ->
			?STATUS_CLOSE 
	end.

%%获取对应阶段的开始时间
get_consumerank_status_begin_timestamp(?STATUS_OPEN)->
	get_server_open_time();
get_consumerank_status_begin_timestamp(?STATUS_FINISH)->
	ServerOpenTime = get_server_open_time(),
	{CloseRelTime,_FinishRelTime} = data_consumerank:get(time_period),
	ServerOpenTime + util:dateTimeIntervel_to_second(CloseRelTime);
get_consumerank_status_begin_timestamp(?STATUS_CLOSE)->
	ServerOpenTime = get_server_open_time(),
	{CloseRelTime,FinishRelTime} = data_consumerank:get(time_period),
	ServerOpenTime + util:dateTimeIntervel_to_second(CloseRelTime)+util:dateTimeIntervel_to_second(FinishRelTime).

%%设置活动状态
set_consumerank_state(RealState)->
	ets:insert(?ETS_CONSUMERANK,{?CONSUMERANK_STATE,RealState}).
get_consumerank_state()->
	case ets:lookup(?ETS_CONSUMERANK,?CONSUMERANK_STATE) of
		[]->
			?undefined;
		[{_,X}]->
			X
	end.
%%设置消费排行榜
set_consumerank_list(ConsumeList)->
	ets:insert(?ETS_CONSUMERANK,{?CONSUMERANKLIST,ConsumeList}).
get_consumerank_list()->
	case ets:lookup(?ETS_CONSUMERANK,?CONSUMERANKLIST) of
		[]->
			[];
		[{_,X}]->
			X
	end.
%%设置充值排行榜
set_payrank_list(PayList)->
	ets:insert(?ETS_CONSUMERANK,{?PAYRANKLIST,PayList}).
get_payrank_list()->
	case ets:lookup(?ETS_CONSUMERANK,?PAYRANKLIST) of
		[]->
			[];
		[{_,X}]->
			X
	end.
%%设置玩家的充值消费记录
set_role_consumepay_data(#consume_pay_unit{roleID=RoleID}=R)->
	ets:insert(?ETS_CONSUMERANK,{RoleID,R}).
get_role_consumepay_data(RoleID)->
	case ets:lookup(?ETS_CONSUMERANK,RoleID) of
		[]->
			?undefined;
		[{_,X}]->
			X
	end.
add_role_pay_num(RoleID,IsMale,Level,Title,RoleName,FightPower,Head,Vip,PayNum)->
	case get_role_consumepay_data(RoleID) of
		?undefined->
			RoleConsumePayUnit = #consume_pay_unit{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower,head=Head
				,vip=Vip,consume=0,pay=PayNum,rank=0},
			set_role_consumepay_data(RoleConsumePayUnit);
		#consume_pay_unit{pay=OldPay}=RCPU->
			%%此处顺便更新下玩家的个人信息
			RoleConsumePayUnit = RCPU#consume_pay_unit{isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower,head=Head
				,vip=Vip,pay=OldPay+PayNum},
			set_role_consumepay_data(RoleConsumePayUnit)
	end,
	%%只更新了充值排行榜
	update_consumerank_pay_list(RoleConsumePayUnit).

add_role_consume_num(RoleID,IsMale,Level,Title,RoleName,FightPower,Head,Vip,ConsumeNum)->
  	?ERR("run~n"),
	case get_role_consumepay_data(RoleID) of
		?undefined->
			RoleConsumePayUnit = #consume_pay_unit{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower,head=Head
				,vip=Vip,consume=ConsumeNum,pay=0,rank=0},
			set_role_consumepay_data(RoleConsumePayUnit);
		#consume_pay_unit{consume=OldConsume}=RCPU->
			%%此处顺便更新下玩家的个人信息
			RoleConsumePayUnit = RCPU#consume_pay_unit{isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower,head=Head
				,vip=Vip,consume=OldConsume+ConsumeNum},
			set_role_consumepay_data(RoleConsumePayUnit)
	end,
	%%只更新消费排行榜
	update_consumerank_consume_list(RoleConsumePayUnit).
%%比较消费Unit
compare_consume_unit(A,B)->
	#consume_pay_unit{level=LevelA,fightPower=FightPowerA,vip=VipA,consume=ConsumeA} = A,
	#consume_pay_unit{level=LevelB,fightPower=FightPowerB,vip=VipB,consume=ConsumeB} = B,
	(ConsumeA > ConsumeB) orelse
	((ConsumeA =:= ConsumeB) andalso (VipA>VipB)) orelse
	((ConsumeA =:= ConsumeB) andalso (VipA =:= VipB) andalso(LevelA > LevelB)) orelse
	((ConsumeA =:= ConsumeB) andalso (VipA =:= VipB) andalso(LevelA =:= LevelB) andalso (FightPowerA > FightPowerB)).

%%比较充值Unit
compare_pay_unit(A,B)->
	#consume_pay_unit{level=LevelA,fightPower=FightPowerA,vip=VipA,pay=PayNumA} = A,
	#consume_pay_unit{level=LevelB,fightPower=FightPowerB,vip=VipB,pay=PayNumB} = B,
	(PayNumA > PayNumB) orelse
	((PayNumA =:= PayNumB) andalso (VipA>VipB)) orelse
	((PayNumA =:= PayNumB) andalso (VipA =:= VipB) andalso(LevelA > LevelB)) orelse
	((PayNumA =:= PayNumB) andalso (VipA =:= VipB) andalso(LevelA =:= LevelB) andalso (FightPowerA > FightPowerB)).

%%更新消费排行榜
update_consumerank_consume_list(RoleConsumePayUnit)->
	ConsumeList = get_consumerank_list(),
	NewConsumeList = update_consumerank_list(ConsumeList,RoleConsumePayUnit,fun compare_consume_unit/2),
	set_consumerank_list(NewConsumeList).

%%更新充值排行榜
update_consumerank_pay_list(RoleConsumePayUnit)->
	PayList = get_payrank_list(),
	NewPayList = update_consumerank_list(PayList,RoleConsumePayUnit,fun compare_pay_unit/2),
	set_payrank_list(NewPayList).

update_consumerank_list(ConsumeList,#consume_pay_unit{roleID=RoleID}=E,CompareFun)->
	NewConsumeList = case lists:keytake(RoleID,#consume_pay_unit.roleID,ConsumeList) of
		false->
			[E|ConsumeList];
		{_,F,Other}->
			[E|Other]
	end,
	SortList = lists:sort(CompareFun,NewConsumeList),
	{SortList2,Index} = lists:foldl(fun(Elem,{Acc,Index})->{[Elem#consume_pay_unit{rank=Index}|Acc],Index+1} end,{[],1},SortList),
	MaxRankNum = data_consumerank:get(ranknum),
	case Index-1 >  MaxRankNum of
		true->
			lists:sublist(lists:reverse(SortList2),MaxRankNum);
		false->
			lists:reverse(SortList2)
	end.

%%该函数主要用来刷新玩家的个人信息(目前使用应该是只针对等级变化调用该方法)
fresh_consumerank_role_info(RoleID,IsMale,Level,Title,RoleName,FightPower,Head,Vip)->
	case get_role_consumepay_data(RoleID) of
		[]->
			ignore;
		E ->
			NE = E#consume_pay_unit{isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower,head=Head,vip=Vip},
			set_role_consumepay_data(NE),
			%%更新充值列表
			update_consumerank_pay_list(NE),
			%%更新消费列表
			update_consumerank_consume_list(NE)
	end.

%%发放消费和充值奖励
send_reward()->
	MatchSpec = ets:fun2ms(fun({M,{_,_,_,_,_,_,_,_,_,ConsumeNum,PayNum,_}}) when is_number(M) ->{M,PayNum,ConsumeNum} end),
	RoleList = ets:select(?ETS_CONSUMERANK,MatchSpec),
	RoleIDList = [RoleID||{RoleID,_,_}<-RoleList],
	ConsumeRankList = get_consumerank_list(),
	send_consume_reward(RoleIDList,ConsumeRankList),
	PayRankList = get_payrank_list(),
	send_pay_reward(RoleIDList,PayRankList),
	?ERR("finish send_reward ~nRoleData:~w~n,ConsumeList:~w~n,PayRankList:~w ~n",[RoleList,ConsumeRankList,PayRankList]).
%%发放消费奖励
send_consume_reward(RoleIDList,ConsumeList)->
	% %%首先发放排行榜上榜玩家奖励
	ArgList = [unicode:characters_to_binary("消费")],
	ConsumeRankReward = data_consumerank:get(consume_rank_reward),
	send_rank_reward(ConsumeList,ConsumeRankReward,ArgList,?CONSUME_RANK_TYPE),
	ConsumeRankRoleIDList = [RoleID||#consume_pay_unit{roleID=RoleID}<-ConsumeList],
	ConsumeNormalReward = data_consumerank:get(consume_normal_reward),
	send_normal_reward(RoleIDList,ConsumeRankRoleIDList,ConsumeNormalReward,?CONSUME_RANK_TYPE,ArgList).
%%发放充值奖励
send_pay_reward(RoleIDList,PayList)->
	% %%首先发放排行榜上榜玩家奖励
	ArgList = [unicode:characters_to_binary("充值")],
	PayRankReward = data_consumerank:get(pay_rank_reward),
	send_rank_reward(PayList,PayRankReward,ArgList,?PAY_RANK_TYPE),
	PayRankRoleIDList = [RoleID||#consume_pay_unit{roleID=RoleID}<-PayList],
	PayNormalReward = data_consumerank:get(pay_normal_reward),
	send_normal_reward(RoleIDList,PayRankRoleIDList,PayRankRoleIDList,?PAY_RANK_TYPE,ArgList).
%%发放普通奖励
send_normal_reward([],_BanRoleIDList,_Reward,Type,_ArgList)->
	?ERR("normal reward finish type:~w ~n",[Type]);
send_normal_reward([H|T],BanRoleIDList,Reward,Type,ArgList)->
	case lists:member(H,BanRoleIDList) of
		true->
			send_normal_reward(T,BanRoleIDList,Reward,Type,ArgList);
		false->
			case get_role_consumepay_data(H) of
				?undefined->
					?ERR("send normal reward not find roledata~w~n",[H]);
				#consume_pay_unit{pay=PayNum,consume=ConsumeNum}->
					%%consume_pay_unit里面保存了充值以及消费的数据，需要根据类型判断是否确实发奖
					case ((Type =:= ?PAY_RANK_TYPE) andalso(PayNum>0)) orelse((Type =:= ?CONSUME_RANK_TYPE)andalso(ConsumeNum>0)) of
						true->
							mail_server:send_sys_mail(H,?MAIL_CONSUMERANK_NO_RANK,ArgList, "",Reward);
						false->
							ignore
					end
			end,
			send_normal_reward(T,BanRoleIDList,Reward,Type,ArgList)
	end.

%%发放排行榜奖励
send_rank_reward([],_RankReward,_ArgList,Type)->
	?ERR("rank_reward type:~w finish~n",[Type]);
send_rank_reward([H|T],RankReward,ArgList,Type)->
	#consume_pay_unit{roleID=RoleID,rank=Rank} = H,
	Reward = calculate_rank_reward(Rank,RankReward),
	ArgList1 = lists:reverse([Rank|ArgList]),
	mail_server:send_sys_mail(RoleID,?MAIL_CONSUMERANK_RANK,ArgList1,"",Reward),
	send_rank_reward(T,RankReward,ArgList,Type).

%%根据Rank值在奖励列表中查找对应的奖励
calculate_rank_reward(Rank,RankReward)->
	case calculate_rank_reward2(Rank,RankReward) of
		?undefined->
			?ERR("not find rank:~w reward list:~w ~n",[Rank,RankReward]),
			#sell_reward{};
		R->
			R
	end.
calculate_rank_reward2(Rank,[])->
	?undefined;
calculate_rank_reward2(Rank,[H|T])->
	{{RankMin,RankMax},Reward} = H,
	case Rank >= RankMin andalso Rank =< RankMax of 
		true->
			Reward;
		false->
			calculate_rank_reward2(Rank,T)
	end.

%%通过离线数据发放奖励
send_consumerank_reward(DBRoleDate,DBConsumeList,DBPayList)->
	RoleIDList = [RoleID||#consume_pay_unit{roleID=RoleID}<-DBRoleDate],
	RoleList = [{RoleID,PayNum,ConsumeNum}||#consume_pay_unit{roleID=RoleID,pay=PayNum,consume=ConsumeNum}<-DBRoleDate],
	send_consume_reward(RoleIDList,DBConsumeList),
	send_pay_reward(RoleIDList,DBPayList),
	?ERR("finish send_reward ~nRoleData:~w~n,ConsumeList:~w~n,PayRankList:~w ~n",[RoleList,DBConsumeList,DBPayList]).
%%=======================================================================
