%% @author caohongyang
%% @doc 玩家逻辑进程
%% Created 2013-2-20


-module(role_server).
-compile(export_all).
-behaviour(gen_server).
-include("def_role.hrl").
-include("def_weibo_share.hrl").
-include("def_family.hrl").
-include("def_task.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  
-include("def_fight.hrl").
-include("def_doublematch.hrl").
-include("def_mail.hrl").
-include("def_battle.hrl").

-export([start_link/6,start/1,stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

-define(interval_dump_tick, 5*60).
-define(CURRENTSEASON,1).
-define(PRESEASON,2).

stop(RoleServerPid) when is_pid(RoleServerPid) ->
	supervisor:delete_child(role_sup, RoleServerPid),
	catch gen_server:cast(RoleServerPid, stop); 
stop(RoleID) when is_integer(RoleID)->
	stop(role_lib:pid(RoleID)).

start(Args) ->
    supervisor:start_child(role_sup, Args).

start_link(RoleID, GatewayClientPid, Socket, MacAddr, Ip, DeviceID) ->	
	?DEBUG("role_server start, ID=~w",[RoleID]),
    gen_server:start_link(?MODULE, [RoleID,  GatewayClientPid, Socket, MacAddr, Ip, DeviceID], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([RoleID, GatewayClientPid, Socket, _MacAddr, _Ip, _DeviceID]=Params) ->
	?INFO("init role_server=~w",[RoleID]),
	process_flag(trap_exit, true),
	random:seed(util:gen_random_seed()),
	%% 开始时间轮
	timer_wheel:init(),
	%% 先注册名称，并加入在线玩家表，避免从数据库读到脏数据
	case role_state:login(RoleID, self()) of
		false ->
			{stop, not_exit};
		true ->
			role_lib:join_online_table(RoleID, GatewayClientPid, Socket),
			erlang:send(self(), {load_data,Params}),
			erlang:send_after(10000, self(), check_loading),
			{ok,loading}
	end.
%% init([RoleID, GatewayClientPid, Socket, MacAddr, Ip, DeviceID]) ->
%% 	?INFO("init role_server=~w",[RoleID]),
%% 	process_flag(trap_exit, true),
%% 	random:seed(util:gen_random_seed()),
%% 	%% 开始时间轮
%% 	timer_wheel:init(),
%% 	%% 先注册名称，并加入在线玩家表，避免从数据库读到脏数据
%% 	case role_state:login(RoleID, self()) of
%% 		false ->
%% 			{stop, not_exit};
%% 		true ->
%% 			role_lib:join_online_table(RoleID, GatewayClientPid, Socket),
%% 			
%% 			%% 判断加载数据是否成功
%% 			case ?CATCH(init_role_data(RoleID, GatewayClientPid, Socket, MacAddr, Ip,DeviceID)) of
%% 				{'EXIT',_} ->
%% 					role_state:logoff_without_flush(RoleID),
%% 					{stop, load_data_error2};
%% 				{RoleInfo,LastLogoutTime} ->
%% 					
%% 					
%% 					%% 当前秒数
%% 					NowSec = timer_wheel:nowsec(),
%% 					%% 秒循环
%% 					timer_wheel:add_plan(NowSec+1,fun sec_loop/1),
%% 					
%% 					%% 凌晨hook
%% %% 					NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
%% %% 					timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
%% 					{Hour,_,_} = time(),
%% 					NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{Hour,0,1}}) + ?ONE_HOUR_SECONDS,
%% 					timer_wheel:add_plan(NextZeroClockSec, fun hook_hour/0),
%% 					
%% 					%% link
%% 					erlang:link(GatewayClientPid),
%% 					
%% 					%% 定时持久化
%% 					interval_dump_tick(RoleID),
%% 					notice_family_online(true),
%% 					role_homestead:init_role_homestead(RoleInfo,LastLogoutTime),
%%                     role_talent:init_recacl_talent_timer(),
%% 					enargy_server:enargy_role_online(RoleID),
%% 					catch etc_server:role_online(RoleID),
%% 					{ok, RoleID}
%% 			end
%% 	end.

interval_dump_tick(RoleID) ->
	Now = timer_wheel:nowsec(),
	TarSec = Now+?interval_dump_tick,
	timer_wheel:add_plan(TarSec, 
						 fun() ->
								 case get(?interval_persist_flag2) of
									 true ->
										 role_persist:do_interval_persist2(RoleID),
										 erase(?interval_persist_flag2);
									 _ ->
										 ignore
								 end,
								 case get(?interval_persist_flag) of
									 true ->
										 put(?interval_persist_flag2,true),
										 role_persist:do_interval_persist(RoleID),
										 role_data:clear_interval_persist_flag();
									 _ ->
										 ignore
								 end,
								 interval_dump_tick(RoleID)
						 end).

cacl_deduct_gold(RoleInfo, DeductGold) ->
	#role{gold=G,goldBonus=GB,goldUsed=GU} = RoleInfo,
	if GB >= DeductGold ->
		   {G, GB-DeductGold, DeductGold+GU};
	   true ->
		   {G+GB-DeductGold, 0, DeductGold+GU}
	end.

init_role_data(RoleID, GatewayClientPid, Socket, MacAddr, Ip, DeviceID) ->
	{RoleInfo, RoleExtra,GerList, PosList, ListOfGerEquipList, BagEquip, BagItem, LPosList,RoleLieutenant, TeamPkInfo
  , AlienInfo, MagicBookState,SignInfo,BattleBossRewardInfo,MainGerTypeID,HronHistory,MainTask,VTrainerProf} 
		= role_data:request_role_data(RoleID),
	%% 设置mac地址和IP
	role_data:set_macAddr(MacAddr),
	role_data:set_ip(Ip),
	
	%% 初始化玩家各功能数据
	role_data:set_roleID(RoleID),
	role_data:set_roleInfo(RoleInfo),
	%% 读取离线扣取的元宝
	
%% 	[RolePatch]=db_sql:get_treasuer_patch(RoleID),
	OfflineDeductGold = db_sql:get_offlineDeductGold(RoleID),
	db_sql:set_offlineDeductGold(RoleID, 0),
	EncounterInfo = db_sql:get_role_encounterList(RoleID),
	
	hist_server:role_login(RoleID),
	{Gold, GoldBonus, GoldUsed} = cacl_deduct_gold(RoleInfo, OfflineDeductGold),
	LastLogoutTime = RoleInfo#role.lastLogoutTime,
%% 	plunder_server:load_role_patch(RolePatch),
	
	role_data:set_roleEncounterInfo(EncounterInfo, RoleInfo), 
	
	role_data:set_gatewayClientPid(GatewayClientPid),
	role_data:set_socket(Socket),
	role_data:set_roleExtra(RoleExtra, RoleInfo, AlienInfo),
	%% 先初始化装备，再初始化武将(因为要计算武将属性)
	%% 先初始化武将，再初始化副将，然后再计算副将对主将的影响(天命与加成)
	role_data:init_LieutenantInfo(RoleLieutenant),
	%% v315版本在初始化玩家装备的套装属性时，需要对应精灵的属性，所以此处加了PosList参数
	role_data:init_all_item(ListOfGerEquipList, BagEquip, BagItem,PosList),
	role_data:set_gerBag(GerList),
	
	{PosListT, LPosListT} = role_data:posList_filter(PosList, LPosList),
	%?ERR("RoleInfo: ~w~n",[role_data:get_roleInfo()]),
    role_data:init_magicBook_add_attr(MagicBookState),
	role_data:init_posList(PosListT),
	%%背包精灵的初始化一定要在小伙伴初始化前，初始化小伙伴的过程中可能会出现多余的小伙伴精灵不能放入伙伴营地的情况
	role_data:init_lieuList(LPosListT,RoleInfo#role.level),
	role_data:init_signinfo(SignInfo),

	role_data:init_maingertypeid(MainGerTypeID),
	role_data:init_battlebossrewardinfo(BattleBossRewardInfo),
	role_data:init_hronhistory(HronHistory),
	role_data:init_gag_list(RoleID),
	role_data:init_sign_emperor_info(RoleID),
	role_data:init_treaHouseInfo(RoleID),
	role_data:set_roleTeamPkInfo(TeamPkInfo),
	IsSelectGer = PosListT =/= [] orelse GerList =/= [],
	{Rank, Title} = 
		case IsSelectGer of
			true->
				pvp_server:call_get_title_rank(RoleID);
			false->
				{9999,0}
		end,
	RoleInfo2 = RoleInfo#role{title=Title,goldBonus=GoldBonus,gold=Gold,goldUsed=GoldUsed,deviceID=DeviceID},
	case IsSelectGer of
		true->
			role_lib:insert_rolePublic(RoleInfo,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data());
		false->
			ignore
	end,
	role_data:set_roleInfo(RoleInfo2),
	role_task:init_task_data(RoleID,LastLogoutTime),
	role_data:set_pvp_rank(Rank),
    role_plunder:request_plunder_info(),
    role_discount:init_discount_info(RoleID),
    role_panicbuy:init_panicbuy_info(RoleID),
	
	role_data:init_montVIP_info(RoleID,LastLogoutTime),
	role_data:init_lucky_roll_info(RoleID,LastLogoutTime),
	role_data:init_homeBoss_times(RoleID,LastLogoutTime),
	role_data:init_free_box_info(RoleID,LastLogoutTime),
	role_data:init_exBoss_data(RoleID,RoleInfo#role.level,LastLogoutTime),
    role_home:init_home_task(),
    role_skin:init_skin_info(RoleID),
    role_maintask:set_maintask_persist(MainTask),
    role_trainerProf:init_trainerProf(VTrainerProf),
    role_trainerRear:init_trainerrear_info(RoleID),
    erlang:send(doublematch_server, {check_rank_reward,RoleID}),
    
%% 	MonthVIPPay = db_sql:get_offlinePayMonthVIPLog(RoleID),
%% 	if MonthVIPPay =/= [] ->
%% 		   db_sql:clear_offlinePayMonthVIPLog(RoleID);
%% 	   true ->
%% 		   ignore
%% 	end,
%% 	lists:foreach(fun([AmountM,ReceiptM, Md5M, SrcTypeM]) -> role_lib:do_pay_monthVIP(AmountM,ReceiptM,Md5M,SrcTypeM) end, MonthVIPPay),
	
	role_ger:unload_ex_ger(),
	
    %% 自由額度處理儲值
    ChargeListAmount = db_sql:get_offlinePayAmountLog(RoleID),
    if ChargeListAmount =/= [] ->
           db_sql:clear_offlinePayAmountLog(RoleID);
       true ->
           ignore
    end,
    lists:foreach(fun([Amount, Receipt2, Md52, SrcType2]) -> role_lib:do_pay_amount(Amount, Receipt2, Md52, SrcType2) end, ChargeListAmount),
    
	%% 处理充值
	ChargeList = db_sql:get_offlinePayLog(RoleID),
	if ChargeList =/= [] ->
		   db_sql:clear_offlinePayLog(RoleID);
	   true ->
		   ignore
	end,
	lists:foreach(fun([PayItemID, Receipt, Md5, SrcType]) -> role_lib:do_pay(PayItemID, Receipt, Md5, SrcType) end, ChargeList),
	{RoleInfo2,LastLogoutTime}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% @doc gen_server:init/1
handle_call({func, F, Args}, _From, State) ->
	Result = ?CATCH(apply(F,Args)),
	{reply, Result, State};
handle_call(get_fighter_list, _From, State)->
	PosList = role_data:get_posList(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
	TrSpecial = role_data:get_trSpecial(),
	Result = {PosList, RoleLieuAdd,TalentList,TrSpecial},
	{reply, Result, State};
handle_call(get_role_info, _From, State) ->
    Reply = role_data:get_roleInfo(),
    {reply, Reply, State};
%实时返回玩家装备信息，格式和db_sql:get_equipedList(RoleID)一样，调用时注意加超时判断，超时后读取数据库数据
%Fix:160版本增加返回装备附魔特性
handle_call(get_role_equiped, _From, State) ->
    {GerEquipList, _BagEquip, _BagItem} = role_data:get_all_item(),
    EquipList = [ [ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,item_lib:itemDecay(ItemDecay),ItemExp,ItemEnchantType,ItemEnchantLevel,ItemLegendRank]
            || {ItemGerID, ItemList} <- GerEquipList,
               #item{itemUID=ItemUID
                     ,itemTypeID=ItemTypeID
                     ,itemDecay=ItemDecay
                     ,itemLevel=ItemLevel
                     ,itemPos=ItemPos
                     ,itemRank=ItemRank
                     ,itemExp = ItemExp
                     ,itemenchantType=ItemEnchantType
                     ,itemenchantLevel=ItemEnchantLevel
                     ,itemLegendRank=ItemLegendRank} <- ItemList],
    {reply, EquipList, State};
handle_call({sync_add_item,NewItem,AddLogType}, _From, State) ->
    item_lib:add_item_f(NewItem, AddLogType, 0, ""),
    {reply, ok, State};
handle_call({test_get_pd,PdName}, _From, State) ->
    {reply, get(PdName), State};

handle_call(get_skin_info,_From,State)->
	SkinInfo = role_skin:get_skin_info(),
	{reply,SkinInfo,State};

handle_call({deduct_gold,CostType,NeedNum,MoneyDecType,Arg},_From,State)->
    RoleInfo = role_data:get_roleInfo(),
    Res = 
        case role_lib:check_money(RoleInfo,CostType,NeedNum) of
            false ->
                fail;
            true ->
                role_lib:deduct_money_f(RoleInfo, CostType, NeedNum, MoneyDecType, Arg, ""),
                ok
        end,
    {reply,Res,State};
	
handle_call(get_posListT,_From,State)->
	PosListT = role_data:get_posListT(),
	Reply=PosListT,
	{reply,Reply,State};
    
handle_call({test_apply,Module,Method,ArgList},_From,State)-> {reply,erlang:apply(Module,Method,ArgList),State};
    
handle_call(get_tasklink_sign_data,_From,State)->
    RoleTaskLink = role_tasklink:get_role_tasklink(),
    {reply,{role_lib:calculate_force_value()
           ,RoleTaskLink#role_tasklink.free_time + RoleTaskLink#role_tasklink.buy_left_time},State};

handle_call(get_gag_list,_From,State)->
	GagList = role_data:get_gag_list(),
	{reply,GagList,State};
	
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_cast/2
handle_cast(stop, State) ->
	role_generalteam:leave_team(),
	{stop, normal, State};
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_info/2
handle_info({load_data,Params}, loading) ->
	case do_load_data(Params) of
		{ok,RoleID}->
			erlang:send(self(),finish_loading_data),
			{noreply, RoleID};
		Err ->
			?ERR("loading error=~w, params=~w",[Err,Params]),
			{stop, normal,  loading}
	end;
handle_info(finish_loading_data,State)->
	NewState = after_loading_data_hook(State),
	{noreply,State};

handle_info({timer_wheel_tick, _}, loading) ->
	NowSec = util:now(),
	timer_wheel:tick(NowSec),
	{noreply, loading};
handle_info(_,loading) ->
	{noreply, loading};
handle_info(check_loading,loading) ->
	{stop, loading_too_long, loading};
handle_info(check_loading, State)->
	{noreply,State};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info(do_hibernate, State) ->
	{noreply, State, hibernate};
handle_info({client_msg, Module, Info}, State) ->
	?DEBUG("recv msg=~w",[{client_msg, Module, Info}]),
	FuncName = element(1, Info),
	?CATCH(Module:FuncName(Info)),
    {noreply, State};
handle_info({?route, Module, Info}, State) ->
	?DEBUG("recv msg=~w",[{?route, Module, Info}]),
	FuncName = element(1, Info),
	?CATCH(Module:FuncName(Info)),
    {noreply, State};
handle_info({func, F, Args}, State) ->
	?CATCH(apply(F,Args)),
	{noreply, State};
handle_info({func, M,F, Args}, State) ->
    ?CATCH(apply(M,F,Args)),
    {noreply, State};
handle_info({timer_wheel_tick, LastTick}, State) ->
	timer_wheel:work(LastTick),
	{noreply, State};
handle_info({'EXIT',From,Reason},State) ->
    if From =/= self() andalso Reason == login_again ->
           {noreply, State};
       true ->
           case role_data:get_gatewayClientPid() of
               From ->
                   {stop, {gateway_stop,From,Reason},State};
               _ ->
                   erlang:unlink(From),
                   {noreply, State}
           end
    end;
handle_info({timeout, TimerRef, Info}, State) ->
    do_handle_info({timeout, TimerRef, Info}, State),
	{noreply, State};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info, State)),
	{noreply, State}.

-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
%% 初始化失败时，直接退出
terminate(load_data_error, State) ->
	RoleID = State,
	role_state:logoff_without_flush(RoleID),
	ok;
terminate(Reason, State) ->
	% 如果下线时间是0点整,则主动完成一次数据刷新,然后完成下线动作
	case erlang:localtime() of
		{_,{0,0,0}} -> hook_zero_clock();
		{_,{0,0,1}} -> hook_zero_clock();
		_ -> ignore
	end,	        
	
	RoleID = State,
	#role{deviceID=DeviceID} = role_data:get_roleInfo(),
	role_state:logoff_with_flush(RoleID,DeviceID),
	if Reason == forced_stop orelse Reason == normal orelse Reason == shutdown orelse element(1,Reason) == gateway_stop->
		   ignore;	   
	   true ->
		   ?ERR("~w terminate for \nReason=~300p\nState=~300p\n",[{role_server,RoleID}, Reason,  State])
	end,
	ok.

do_load_data([RoleID, GatewayClientPid, Socket, MacAddr, Ip, DeviceID]) ->
	%% 判断加载数据是否成功
	case ?CATCH(init_role_data(RoleID, GatewayClientPid, Socket, MacAddr, Ip,DeviceID)) of
		{'EXIT',_} ->
			role_state:logoff_without_flush(RoleID),
			{stop, load_data_error2};
		{RoleInfo,LastLogoutTime} ->
			
			
			%% 当前秒数
			NowSec = timer_wheel:nowsec(),
			%% 秒循环
			timer_wheel:add_plan(NowSec+1,fun sec_loop/1),
			
			%% 凌晨hook
			%% 					NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
			%% 					timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
			{Hour,_,_} = time(),
			NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{Hour,0,1}}) + ?ONE_HOUR_SECONDS,
			timer_wheel:add_plan(NextZeroClockSec, fun hook_hour/0),
			
			%% link
			erlang:link(GatewayClientPid),
			
			%% 定时持久化
			interval_dump_tick(RoleID),
			notice_family_online(true),
			role_homestead:init_role_homestead(RoleInfo,LastLogoutTime),
			role_talent:init_recacl_talent_timer(),
			enargy_server:enargy_role_online(RoleID),
			%%在此处初始化玩家七日活动信息，防止任务判断时，其他的数据不存在
			role_data:init_headSeven(RoleInfo#role.roleID),
    		%%由于七日活动在发奖的时候会触发对活动奖励的判断统计，故将挂机离线奖励放到此处。
    		role_data:init_xbattle_data(LastLogoutTime,RoleID),
			role_trainingRoom:checkLoad(),
			catch etc_server:role_online(RoleID),
            role_task_trigger:do_src_levelup_record(RoleID,RoleInfo#role.level),
			{ok, RoleID}
	end.

%% 严格的处理所有收到的消息
flush_msg(State) ->
	receive
		{'$gen_call',{From,_},Msg} ->
			handle_call(Msg, From, State),
			flush_msg(State);
		{'$gen_cast',Msg} ->
			handle_cast(Msg, State),
			flush_msg(State);
		Msg ->
			handle_info(Msg, State),
			flush_msg(State)
	after 0 ->
			ok
	end.


-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% 秒循环
sec_loop(NowSec) ->
	timer_wheel:add_plan(NowSec+1, fun sec_loop/1),
	do_sec_loop(NowSec),
	if (NowSec rem 10) == 0 ->
		   erlang:send(role_lib:gw(role_data:get_roleID()), role_beat);
	   true ->
		   ignore
	end,
	if (NowSec rem 60) =:= 0 ->
		   do_minute_loop(NowSec);
	   true ->
		   ignore
	end,
	if (NowSec rem 300) =:= 0 ->
		   erlang:send(self(), do_hibernate);
	   true ->
		   ignore
	 end.

do_sec_loop(_NowSec) ->
	ok.

do_minute_loop(_NowSec) ->
	%RoleID = role_data:get_roleID(),
	%% 更新ETS_ROLE_PUBLIC
	role_generalteam:delet_outdata_remain_time(),
	role_lib:update_rolePublic(role_data:get_roleInfo(),role_data:get_trSpecial()
                              ,role_data:get_plane_use_info(),role_data:get_xbattle_data()),
    role_carlos:update_plane_info(),
	ok.

%%每个小时执行
hook_hour()->
	{Hour,_,_} = time(),
	NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{Hour,0,1}}) + ?ONE_HOUR_SECONDS,
	timer_wheel:add_plan(NextZeroClockSec, fun hook_hour/0),
	case Hour of
		0->
			hook_zero_clock(),
            erlang:send(erlang:self(), {route, role_role, #cs_role_info{}});
		_->
			ignore
	end,
    role_team:erase_replay_record(),
	role_homestead:hook_hour(Hour).

%% 凌晨0点hook
hook_zero_clock() ->
	%% 每日领取奖励通知
	role_daily:hook_zero_clock(),
	%%任务每日刷新
	role_task:hook_zero_clock(),
	%%刷新玩家的签到信息
	role_sign:hook_zero_clock().
		  
sort([]) ->
	[];
sort([H|L]) ->
	sort([E||E<-L,E =< H]) ++ [H] ++ sort([E||E<-L, E>H]).
		   
		   
do_handle_info({dispach_task,Msg},_)->
	?CATCH(role_task_trigger:handle(Msg));
%% 处理特殊消息
do_handle_info({pvp_attack,Attacker,LieuAddA,AtkTalentList,TrSpecialA,SkinInfoA,LegendAddListA,From,Ref}, _RoleID) ->
	Defender = role_data:get_fighter_list(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
	TrSpecialB = role_data:get_trSpecial(),
	SkinInfoB = role_skin:get_skin_info(),
	LegendAddListD = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-Defender],
	role_fight:new(0,false,Attacker,Defender,LieuAddA,RoleLieuAdd,AtkTalentList,TalentList,TrSpecialA,TrSpecialB,From,Ref,SkinInfoA,SkinInfoB,LegendAddListA,LegendAddListD);
%% 新华丽大赛
do_handle_info({race2_attack,Attacker,LieuAddA,AtkTalentList,TrSpecialA,SkinInfoA,LegendAddListA,From,Ref,IsSwitch}, _RoleID) ->
    Defender = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
    TrSpecialB = role_data:get_trSpecial(),
    SkinInfoB = role_skin:get_skin_info(),
	LegendAddListD = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-Defender],
    case IsSwitch of
        false ->
            role_fight:new(0,false,Attacker,Defender,LieuAddA,RoleLieuAdd,AtkTalentList,TalentList
                          ,TrSpecialA,TrSpecialB,From,Ref,SkinInfoA,SkinInfoB,LegendAddListA,LegendAddListD);
        true ->
            role_fight:new(0,false,Defender,Attacker,RoleLieuAdd,LieuAddA,TalentList,AtkTalentList
                          ,TrSpecialB,TrSpecialA,From,Ref,SkinInfoB,SkinInfoA,LegendAddListD,LegendAddListA)
    end;
do_handle_info({pvp_rank, Rank}, _RoleID) ->
	role_data:set_pvp_rank(Rank);
do_handle_info({pvp_rank_reward, GoldNum, NewMaxRank}, _RoleID) ->
    RoleInfo = role_data:get_roleInfo(),
    role_reward:handle_sys_reward(RoleInfo, [{?REWARD_GOLD,GoldNum}], ?MONEY_ADD_TYPE_PVP_RANK_REWARD, NewMaxRank, "");
do_handle_info({title, Title}, _RoleID) ->
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{title=Title},
	role_data:set_roleInfo(RoleInfo2),
	role_generalteam:update_roleinfo(RoleInfo2),
	?notify_update(?ra_title(Title));
do_handle_info({plunder_product,NewItem}, _RoleID) ->
    ?CATCH(role_task_trigger:handle({dispach_task, compose_stone})),
	item_lib:add_item_f(NewItem, ?MONEY_ADD_TYPE_PLUNDER_COMPOSE, 0, "");
do_handle_info({plunder_multi_product, NewItem,Num},_RoleID) ->
	?CATCH(role_task_trigger:handle({dispach_task, compose_stone})),
	[item_lib:add_item_f(NewItem, ?MONEY_ADD_TYPE_PLUNDER_COMPOSE, 0, "")||_<-lists:duplicate(Num, 1)];
do_handle_info({do_pay,AppItemID,Receipt,Md5,SrcType},_RoleID) ->
	Daily = role_data:get_dailyInfo(),
	CurrentTimeStamp = util:now(),
	NewDaily = Daily#daily{lastPayTime=CurrentTimeStamp},
	role_data:set_dailyInfo(NewDaily),
	role_lib:do_pay(AppItemID,Receipt,Md5,SrcType);
%% do_handle_info({do_pay_monthVIP,AppItemID,Receipt,Md5,SrcType},_RoleID) ->
%% 	role_lib:do_pay_monthVIP(AppItemID,Receipt,Md5,SrcType);
do_handle_info({do_pay_amount,Amount,Receipt,Md5,SrcType},_RoleID) ->
    role_lib:do_pay_amount(Amount,Receipt,Md5,SrcType);
do_handle_info({do_ger_view, SrcRoleID, ServerID}, RoleID) ->
	RolePublic=role_lib:get_rolePublic(RoleID),
	FighterList=role_data:get_fighter_list(),
	Reply=role_ger:ger_view_info(RolePublic, FighterList),
    case ServerID =:= 0 of
        true ->
	       ?unicast(SrcRoleID,Reply);
        false ->
            cross_server:send_msg_to_slave_sever_id(ServerID, {cross_ger_view_return, SrcRoleID, RoleID, Reply})
    end;
do_handle_info({do_ger_view_dtl, SrcRoleID, ServerID}, RoleID) ->
	Role=role_data:get_roleInfo(),
	FighterList=role_data:get_posList(),
	EquipList=role_data:get_equipts_on_ger(),
	% {AtkAdd, HpAdd} = role_data:get_lieu_add_attr(),
	{AtkAdd,HpAdd} = role_buddy:get_buddy_normal_buff_for_total(),
	Special = role_data:get_trSpecial(),
	LieuInfoList = role_data:get_lieuInfoList(),
	SkinInfo = role_skin:get_skin_info(),
	Reply=role_ger:ger_view_info_dtl(Role, FighterList,EquipList, AtkAdd, HpAdd, LieuInfoList,Special,SkinInfo),
    case ServerID =:= 0 of
        true ->
	       ?unicast(SrcRoleID,Reply);
        false ->
            cross_server:send_msg_to_slave_sever_id(ServerID, {cross_ger_view_dtl_return, SrcRoleID, RoleID, Reply})
    end;
do_handle_info({draw_mail_reward,Reward,MailTemplateID}, _RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sys_reward(Role, Reward, ?MONEY_ADD_TYPE_MAIL_REWARD, MailTemplateID, ""),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_MAIL);
do_handle_info({draw_activity_reward,ActivityID,Reward},_RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_ACTIVITY_DRAW, ActivityID, "");
do_handle_info({do_alien_record_reward,Reward,Type}, _RoleID) ->
    Role = role_data:get_roleInfo(),
    role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_ALIEN_RECORD, Type, "");
do_handle_info({activity_exchange, ActivityID, DrawID, Condition, Reward}, RoleID) ->
	role_exchange:handle_exchange(RoleID, ActivityID, DrawID, Condition, Reward),
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_EXCHANGE);
do_handle_info({draw_activity_rank_reward, Reward, ActivityID, Rank}, _RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_TREAHOUSE_RANKREWARD, ActivityID, integer_to_list(Rank));
do_handle_info({role_energy_activity, Eng},_RoleID) ->
	#roleTimes{energy=Energy,lastEnergyTime=LastTick} = RoleTimes = role_data:get_roleTimes(),
	Energy2 = Energy + Eng,
	%?ERR("Energy:~w,~w",[Energy, Energy2]),
	NowSec = util:now(),
	RoleTimes2 = RoleTimes#roleTimes{energy=Energy2},
 	IntervalSeconds2 = role_lib:get_current_tick(?currentEnergyIntercal),
	NextTick = LastTick - NowSec + IntervalSeconds2,
	?notify_update(?ra_energy(Energy2, NextTick)),
	role_data:set_roleTimes(RoleTimes2),
	?CATCH(role_task_trigger:handle({dispach_task,role_get_energy,1}));
do_handle_info({role_get_family_energy,Eng},_RoleID) ->
	#roleTimes{energy=Energy,lastEnergyTime=LastTick} = RoleTimes = role_data:get_roleTimes(),
	Energy2 = Energy + Eng,
	%?ERR("Energy:~w,~w",[Energy, Energy2]),
	NowSec = util:now(),
	RoleTimes2 = RoleTimes#roleTimes{energy=Energy2},
	IntervalSeconds2 = role_lib:get_current_tick(?currentEnergyIntercal),
	NextTick = LastTick - NowSec + IntervalSeconds2,
	role_data:set_roleTimes(RoleTimes2),
	?sendself(#sc_family_get_role_energy{result=1}),
	?notify_update(?ra_energy(Energy2, NextTick));
do_handle_info({role_send_family_energy,TarRoleID},_RoleID) ->
	SendEnergyList = role_data:get_role_send_energy_list(),
	role_data:set_role_send_energy_list([TarRoleID|SendEnergyList]);
do_handle_info({get_family_daily_reward, Reward,FamilyID}, _)->
	Role = role_data:get_roleInfo(),
	role_reward:handle_daily_reward_f(Role, Reward, ?MONEY_ADD_TYPE_FAMILY_DAILY, FamilyID, "");
do_handle_info({bet_emperor, Money}, _RoleID)->
	Role = role_data:get_roleInfo(),
	role_lib:deduct_coin_f(Role, Money, ?MONEY_DEC_TYPE_BET_EMPEROR, 0, "");
do_handle_info({hula_harm_reward,AddCoin,AddRepu},_RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, #sell_reward{coin=AddCoin,reputation=AddRepu}, ?MONEY_ADD_TYPE_HULA_CHALLENGE, 0, "");
do_handle_info({nanm_harm_reward,AddCoin,AddRepu},_RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, #sell_reward{coin=AddCoin,reputation=AddRepu}, ?MONEY_ADD_TYPE_NANM_CHALLENGE, 0, "");
do_handle_info({add_weibo_share_mark, Type}, _RoleID) ->
	role_invite:do_add_weibo_share_mark(Type);
do_handle_info({nanm_boss_box_reward,BossID,BossQuality},_RoleID)->
	% RewardList = data_nanm:get({boss_reward,BossID}),
	RewardList0 = nanm_server:get_box_reward(BossID,BossQuality),
    RewardList = case activity_server:is_activity(?world_boss_box_double) of
                      true ->
                          RewardList0++RewardList0;
                      false ->
                          RewardList0
                  end,
	Role = role_data:get_roleInfo(),
	role_reward:handle_sys_reward(Role, RewardList, ?MONEY_ADD_TYPE_WORLD_BOSS, 0, ""),
	R = role_reward:transform2p_reward_view(RewardList,[]),
	?sendself(#sc_nanm_reward{result=1,rewardlist=R});
%% 皇权激励
do_handle_info({buy_king_buff, Type}, _RoleID) ->
	role_king:buy_king_buff(Type);
do_handle_info({support_fail, SupportRoleID, ReasonCode, SupportPrice}, _RoleID) ->
    RoleInfo = role_data:get_roleInfo(),
    role_lib:add_coin_f(RoleInfo, SupportPrice, ?MONEY_ADD_TYPE_CROSS_SUPPORT_FAIL, 0, ""),
    Record = #sc_cross_support{reason_code=ReasonCode, support_role_id=SupportRoleID},
%%     ?ERR("Record:~w", [Record]),
    ?sendself(Record);
do_handle_info({dojangrank_fight_res,ResultMsg,ReplayInfo,Reward,PayType},_RoleID) ->
    Dojangrank = role_data:get_dojangrank_data(),
    Role = role_data:get_roleInfo(),
    if
        ReplayInfo /= ?undefined andalso ResultMsg#sc_dojangrank_fight.result =:= 1 ->
            if
                Reward /= ?undefined ->
                    role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_DOJANGRANK, 0, "");
                true -> ignore
            end,
            FightRecList1 = lists:sort(fun(A,B)->
                                               A#p_dojang_replay_info.time > B#p_dojang_replay_info.time
                                               end,[ReplayInfo|Dojangrank#role_dojangrank.local_fight_rec_list]),
            FightRecList2 = lists:sublist(FightRecList1, 20),
            role_data:set_dojangrank_data(Dojangrank#role_dojangrank{local_fight_rec_list  = FightRecList2});
        true -> 
            %% 补回未消耗的次数
            case PayType of
                1 ->
                    role_data:set_dojangrank_data(Dojangrank#role_dojangrank{free_time = Dojangrank#role_dojangrank.free_time + 1});
                2 ->
                    role_data:set_dojangrank_data(Dojangrank#role_dojangrank{paid_time = Dojangrank#role_dojangrank.paid_time + 1})
            end 
    end,
    ?sendself(ResultMsg);
do_handle_info({dojangrank_world_fight_res,Result,FightInfo,ReplayInfo,AttackerInfo,Reward,PayType,RankType},RoleID) ->
    Dojangrank = role_data:get_dojangrank_data(),
    Role = role_data:get_roleInfo(),
    ?INFO("dojangrank_world_fight_res ~w",[{dojangrank_world_fight_res,Result,FightInfo,ReplayInfo,AttackerInfo,Reward,PayType,RankType}]),
    if
        Result =:= 1 ->
            if
                Reward /= ?undefined ->
                    role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_DOJANGRANK, 0, "");
                true -> ignore
            end,
            NewFightRecList = 
                lists:map(fun(RankIndex)-> 
                        List = lists:nth(RankIndex, Dojangrank#role_dojangrank.world_fight_rec_list),
                        if
                            RankIndex =:= ReplayInfo#p_dr_dojang_replay_info.rank_type ->
                                FightRecList1 = 
                                    lists:sort(fun(A,B)->
                                            A#p_dojang_replay_info.time > B#p_dojang_replay_info.time
                                        end,[ReplayInfo|List]),
                                FightRecList2 = lists:sublist(FightRecList1, 10),
                                ?INFO("dojangrank_world_fight_res ~w ~w",[erlang:length(List),erlang:length(FightRecList2)]),
                                FightRecList2;
                            true ->
                                List
                        end
                    end, lists:seq(1, 8)),
            %% 下面函数同时也做了保存数据
            [FightRecord] = FightInfo,
            NewDojangrank = Dojangrank#role_dojangrank{world_fight_rec_list = NewFightRecList},
            role_data:set_dojangrank_data(NewDojangrank),
            {EnemyList,_} = role_dojangrank:get_enemy_info_list(RankType
                                               ,AttackerInfo#p_dr_world_rank.rank
                                               ,NewDojangrank
                                               ,true =:=FightRecord#sc_fight_request.result,true);
        true -> 
            %% 补回未消耗的次数
            case PayType of
                1 -> role_data:set_dojangrank_data(Dojangrank#role_dojangrank{world_free_time = 
                                                                                  util:nth_replace(RankType, Dojangrank#role_dojangrank.world_free_time
                                                                                                  , lists:nth(RankType, Dojangrank#role_dojangrank.world_free_time) + 1)});
                2 -> role_data:set_dojangrank_data(Dojangrank#role_dojangrank{world_paid_time = 
                                                                                  util:nth_replace(RankType, Dojangrank#role_dojangrank.world_paid_time
                                                                                                  , lists:nth(RankType, Dojangrank#role_dojangrank.world_paid_time) + 1)});
                0 -> ignore
            end,
            {EnemyList,_} = role_dojangrank:get_enemy_info_list(RankType
                                               ,AttackerInfo#p_dr_world_rank.rank
                                               ,Dojangrank,false,false)
    end,
    DebugDojangrankRankSelf = ets:match_object(dojangrank_world_server:get_ets_name(RankType), #p_dr_world_rank{roleID=RoleID, _='_'}),
    if
        [AttackerInfo] /= DebugDojangrankRankSelf ->
            ?ERR("dojangrank_world_fight_res AttackerInfo:~w <<<<>>>> DojangrankRankSelf:~w",[AttackerInfo,DebugDojangrankRankSelf]);
        true -> ignore
    end,    
    if
        AttackerInfo#p_dr_world_rank.roleID =:= RoleID ->
            if
                Result =:= 1 ->
                    ?sendself(#sc_dojangrank_world_fight{result = Result
                                                        ,fightInfo = FightInfo
                                                        ,p_dojangrank_rank_self = role_dojangrank:check_rank_out(AttackerInfo)
                                                        ,p_dojangrank_rank_list = role_dojangrank:check_rank_out(EnemyList)
                                                        ,reward = #p_reward_info{coin = 0,roleExp = 0,gerExp = 0,gold = 0 
                                                                                ,reputation = 0,itemList = [],gerList = []}});
                true ->
                    %% 防止玩家快速连续点击战斗，异常时，延迟返回结果
                    erlang:send_after(1000, self(), {relic_to_client
                                                    ,#sc_dojangrank_world_fight{result = Result
                                                                               ,fightInfo = FightInfo
                                                                               ,p_dojangrank_rank_self = role_dojangrank:check_rank_out(AttackerInfo)
                                                                               ,p_dojangrank_rank_list = role_dojangrank:check_rank_out(EnemyList)
                                                                               ,reward = #p_reward_info{coin = 0,roleExp = 0,gerExp = 0,gold = 0 
                                                                               ,reputation = 0,itemList = [],gerList = []}}})
            end;
        true -> ignore
    end;

do_handle_info({fix_pay_time,AddTimeList}, RoleID) ->
    Dojangrank = role_data:get_dojangrank_data(),
    NewWorldPaidTimeList = 
        lists:map(fun(RankType)-> 
                        AddValue = lists:nth(RankType, AddTimeList),
                        NewTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_paid_time) + AddValue  
                  end, lists:seq(1, 8)),
    ?ERR("fix_pay_time2 (~w)~w",[RoleID,AddTimeList]),
    role_data:set_dojangrank_data(Dojangrank#role_dojangrank{world_paid_time = NewWorldPaidTimeList});

do_handle_info({race_guess_succ, GuessCoin}, _) ->
    #role{coin=Coin} = RoleInfo = role_data:get_roleInfo(),
    case GuessCoin > 0 andalso Coin > 0 of
        true ->
            NeedCoin =
                case Coin >= GuessCoin of
                    true ->
                        GuessCoin;
                    false ->
                        Coin
                end,
            role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_RACE_GUESS, 0, "");
        false ->
            next
    end;

do_handle_info({do_alien_reset_return_succ, ResetTime}, _) ->
    #role{gold=Gold,goldBonus=GoldBonus} = RoleInfo = role_data:get_roleInfo(),
    NeedGold = data_alien:get(reset_gold),
    AllGold = Gold + GoldBonus,
    case NeedGold > 0 andalso AllGold > 0 of
        true ->
            NeedGold2 =
                case AllGold >= NeedGold of
                    true ->
                        NeedGold;
                    false ->
                        AllGold
                end,
            role_lib:deduct_money_f(RoleInfo, gold, NeedGold2, ?MONEY_DEC_TYPE_ALIEN_RESET, 0, "");
        false ->
            next
    end,
    AlienInfo = role_data:get_roleAlienInfo(),
    role_data:set_roleAlienInfo(AlienInfo#alien_info{resetTime=ResetTime});
do_handle_info({do_alien_fight_succ_return, NeedTimes, NeedGold, AddCoin}, _) ->
    #role{gold=Gold,goldBonus=GoldBonus} = RoleInfo = role_data:get_roleInfo(),
    role_lib:add_coin_f(RoleInfo, AddCoin, ?MONEY_ADD_TYPE_ALIEN_FIGHT, 0, ""),
    AllGold = Gold + GoldBonus,
    case NeedGold > 0 andalso AllGold > 0 of
        true ->
            NeedGold2 =
                case AllGold >= NeedGold of
                    true ->
                        NeedGold;
                    false ->
                        AllGold
                end,
            role_lib:deduct_money_f(role_data:get_roleInfo(), gold, NeedGold2, ?MONEY_DEC_TYPE_ALIEN_FIGHT, 0, "");
        false ->
            next
    end,
    case NeedTimes =:= 1 of
        true ->
            #alien_info{times=Times, lastRecoverTime=Timestamp} = AlienInfo = role_data:get_roleAlienInfo(),
            NewTimes =
                case Times > 0 of
                    true ->
                        Times - 1;
                    false ->
                        0
                end,
            role_data:set_roleAlienInfo(AlienInfo#alien_info{times=NewTimes}),
            ?sendself(#sc_alien_update_times{leftTimes=NewTimes,timestamp=Timestamp+role_lib:to_sec(data_common:get(alien_recover_interval))});
        false ->
            next
    end;
do_handle_info({do_alien_guess_succ_return, GuessCoin}, _) ->
    #role{coin=Coin} = RoleInfo = role_data:get_roleInfo(),
    case GuessCoin > 0 andalso Coin > 0 of
        true ->
            NeedCoin =
                case Coin >= GuessCoin of
                    true ->
                        GuessCoin;
                    false ->
                        Coin
                end,
            role_lib:deduct_money_f(RoleInfo, coin, NeedCoin, ?MONEY_DEC_TYPE_ALIEN_GUESS, 0, "");
        false ->
            next
    end;

%% 联盟相关处理
do_handle_info({create_family_succ, FamilyInfo, CostType, CostNum}, RoleID) ->
    PFamilyInfo = family_misc:to_p_family_info(FamilyInfo),
    RoleInfo = role_data:get_roleInfo(),
    FamilyID = PFamilyInfo#p_family_info.family_id,
    db_sql:update_role_family_id(RoleID, FamilyID),
?INFO("cs_family_create 现在这里回扣除钻石"),
    %% deduct_money_f中会间接调用role_data:set_roleInfo(RoleInfo2)
    RoleInfo2 = role_lib:deduct_money_f(RoleInfo, CostType, CostNum, ?MONEY_DEC_TYPE_CREATE_FAMILY, 0, ""),
    role_data:set_roleInfo(RoleInfo2#role{familyID=FamilyID}),
    role_data:set_task_list(?TASK_TYPE_FAMILY_TODAY, FamilyInfo#family_info.family_task),
    role_task:send_notify_change(?TASK_TYPE_FAMILY_TODAY,FamilyInfo#family_info.family_task,[]),
    family_misc:unlock_family_protect(RoleID),
    % ?ERR("begin to recacl_gers reason: upLevel the familyTek Level~n"),
	ger_attr:recacl_gers(),
%%     ?ERR("PFamilyInfo:~w", [PFamilyInfo]),
    ?sendself(#sc_family_create{result=0, family_info=PFamilyInfo, timestamp=0});
do_handle_info({familyTek_donate_back, DonateType,_BackNum,DonateNum,ItemTypeID},RoleID) ->
	?INFO("cs_familyTek_donate 在此处会返回未捐献成功的物品，并且添加公会货币"),
	RoleInfo = role_data:get_roleInfo(),
	% //1=> 金币
	% //2=> 钻石
	% //3=> 徽章
	% //4=> 装备
	% //5=> 精灵
	% BagGer = role_data:get_gerBag(),
	% BagEquip = role_data:get_bagEquip(),
	case DonateType of
		1 ->
			role_lib:deduct_coin_f(RoleInfo, DonateNum, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
			AddUnioncoin = role_lib:calculate_unioncoin_back(1,0,DonateNum),
			RoleInfo2 = role_data:get_roleInfo(),
			role_lib:add_unioncoin_f(RoleInfo2, AddUnioncoin, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "");
		2 ->
			role_lib:deduct_gold_2_f(RoleInfo, DonateNum, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
			AddUnioncoin = role_lib:calculate_unioncoin_back(2,0,DonateNum),
			RoleInfo2 = role_data:get_roleInfo(),
			role_lib:add_unioncoin_f(RoleInfo2, AddUnioncoin, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "");
		3 ->
			role_lib:deduct_reputation_f(RoleInfo, DonateNum, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
			AddUnioncoin = role_lib:calculate_unioncoin_back(3,0,DonateNum),
			RoleInfo2 = role_data:get_roleInfo(),
			role_lib:add_unioncoin_f(RoleInfo2, AddUnioncoin, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "");
		4 ->
			BagEquip = role_data:get_bagEquip(),
			{NewBagEquip,DeleteList} = delete_item(DonateNum,ItemTypeID,RoleID,BagEquip),
			role_data:set_bagEquip(NewBagEquip),
			LogItemList = role_item:itemList2logItemList(DeleteList, []),
			DeletItemUIDList = [ItemUID||#item{itemUID=ItemUID}<-DeleteList],
			?sendself(#sc_item_delete_notify{itemUIDList=DeletItemUIDList}),
	 		{Date, _} = Time = erlang:localtime(),
	 		behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
	 		AddUnioncoin = role_lib:calculate_unioncoin_back(4,ItemTypeID,DonateNum),
	 		role_lib:add_unioncoin_f(RoleInfo, AddUnioncoin, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "");
		5 ->
			BagGer = role_data:get_gerBag(),
			{NewBagGer,DeleteList} = delete_ger(DonateNum,ItemTypeID,RoleID,BagGer),
			role_data:set_gerBag(NewBagGer),
			LogItemList = role_ger:gerList2logGerList(DeleteList),
			DeleteGerIDList = [GerID||#gerSimple{gerID = GerID}<-DeleteList],
			?sendself(#sc_ger_del{gerIDList=DeleteGerIDList}),
	 		{Date, _} = Time = erlang:localtime(),
            [erlang:send(self(), {ger_del_cancel_cirrus, GID})||GID<-DeleteGerIDList],
	 		behavior_ger_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, ""),
	 		AddUnioncoin = role_lib:calculate_unioncoin_back(5,ItemTypeID,DonateNum),
	 		role_lib:add_unioncoin_f(RoleInfo, AddUnioncoin, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, 0, "")
	end;
do_handle_info({update_family_tekAdd},_RoleID) ->
	% ?ERR("begin to recacl_gers reason: upLevel the familyTek Level~n"),
	ger_attr:recacl_gers(),
	RoleInfo2 = role_data:get_roleInfo(),
    role_lib:update_rolePublic(RoleInfo2,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data());
do_handle_info({create_family_fail,  _CostType, _CostNum}, RoleID) ->
%%     ?ERR("CostType:~w, CostNum:~w", [CostType, CostNum]),
%%    RoleInfo = role_data:get_roleInfo(),
%%?INFO("cs_family_create 以前这里会返还钻石"),
%% 之前没有扣除费用，所以此处的返还处理不需要了。
%%     case CostType of
%%         coin ->
%%             role_lib:add_coin_f(RoleInfo, CostNum, ?MONEY_ADD_TYPE_CREATE_FAMILY_FAIL, 0, "");
%%         gold ->
%%             role_lib:add_gold_f(RoleInfo, CostNum, ?MONEY_ADD_TYPE_CREATE_FAMILY_FAIL, 0, "")
%%     end,
    family_misc:unlock_family_protect(RoleID),
    ?sendself(#sc_family_create{result=8, family_info=family_misc:gen_p_family_info(), timestamp=0});

do_handle_info({update_family_id, FamilyID}, RoleID) ->
    RoleInfo = role_data:get_roleInfo(),
    case FamilyID =:= 0 of
        false ->
            db_sql:update_role_family_id(RoleID, FamilyID),
            role_data:set_roleInfo(RoleInfo#role{familyID=FamilyID}),
            ger_attr:recacl_gers(),
            RoleInfo2 = role_data:get_roleInfo(),
            role_lib:update_rolePublic(RoleInfo2,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data());
        true ->
            Now = util:now(),
            db_sql:update_role_family_id(RoleID, FamilyID, Now),
            NewUnioncoin = trunc(RoleInfo#role.unioncoin * data_family:get(reduce_unioncoin_rate)),
            role_data:set_roleInfo(RoleInfo#role{familyID=FamilyID, lastJoinFamily=Now, unioncoin=NewUnioncoin}),
            ger_attr:recacl_gers(),
            RoleInfo2 = role_data:get_roleInfo(),
            role_lib:update_rolePublic(RoleInfo2,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data())
    end,
    family_misc:unlock_family_protect(RoleID);
do_handle_info(get_family_recent_talk, RoleID) ->
    #role{familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {get_family_recent_talk, RoleID});
        false ->
            ?sendself(#sc_talk_recent_list{list=[],channel=?CHAT_CHANNEL_FAMILY})
    end;
do_handle_info({role_family_do_contribute,ConsumeType, ConsumeValue,TypeID,Reward}, _RoleID)->
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = role_lib:deduct_money_f(RoleInfo,ConsumeType, ConsumeValue, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, TypeID, ""),
	role_reward:handle_sell_reward_f(RoleInfo2, Reward, ?MONEY_ADD_TYPE_FAMILY_CONTRIBUTIOIN, TypeID, "");

%% 修改玩家的vip信息
do_handle_info({change_role_vip_level, VipLevel,ensure_action}, _RoleID) ->
	case is_integer(VipLevel) andalso VipLevel =< 16 of
		true ->
			Role = role_data:get_roleInfo(),
			Role2 = Role#role{vipLevel=VipLevel},
			role_data:set_roleInfo(Role2),
			RoleTimes=role_data:get_roleTimes(),
			VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(VipLevel),
			VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(VipLevel),
			NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(Role#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
			RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
											 challengeGodBuyTimes=VipChallengeGodBuyTimes
											 },
			role_data:set_roleTimes(RoleTimes2),
			?notify_update(?ra_vipLevel(VipLevel, NewChallengeGodEnergy,VipChallengeGodBuyTimes));
		_ ->
			ignore
	end;

%% 重复登录时的逻辑
do_handle_info({login_again, GatewayPid, Socket, MacAddr, Ip,DeviceID}, RoleID) ->
	role_data:set_gatewayClientPid(GatewayPid),
    erlang:link(GatewayPid),
	role_data:set_socket(Socket),
	role_data:set_macAddr(MacAddr),
	role_data:set_ip(Ip),
	Role = role_data:get_roleInfo(),
	NewRole = Role#role{deviceID=DeviceID},
	role_data:set_roleInfo(NewRole),
	%role_persist:persist_battle(RoleID),
    role_persist:persist_xbattle(RoleID),
	role_lib:join_online_table(RoleID, GatewayPid, Socket);

%%添加公会膜拜任务
do_handle_info({add_family_worship_task,Type,NeedCost,TaskID}, _RoleID) ->
    role_task:add_family_worship_task(Type,NeedCost,TaskID);

do_handle_info({role_family_worship_fight_succ, Reward},RoleID) ->
    %% 奖励并不直接发给玩家,等提交任务时发放,记录在任务的trigger_notes字段里面
    role_task:send_dispach(RoleID, {dispach_task, role_family_worship_fight, Reward});

do_handle_info({role_family_changename_check_succ, Type, LeftChgNameTimes, Name},_RoleID) ->
    role_family:family_changename_check_succ(Type, LeftChgNameTimes, Name);

do_handle_info({role_family_changename_fail, Type, Result, List},_RoleID) ->
    role_family:family_changename_fail(Type, Result, List);

do_handle_info({sync_family_task,NewTaskList},_RoleID) ->
    ?INFO("sync_family_task:~w",[NewTaskList]),
    OldTaskList = role_data:get_task_list(?TASK_TYPE_FAMILY_TODAY),
    NewTaskList2 = lists:foldr(fun(NewFamilyTask,ACC)->
                case lists:keyfind(NewFamilyTask#r_task.task_id, #r_task.task_id, OldTaskList) of
                    OldTask when OldTask#r_task.status =:= ?TASK_STATUS_WAS_ACCEPT->
                        [NewFamilyTask|ACC];
                    OldTask when OldTask#r_task.status /= ?TASK_STATUS_WAS_ACCEPT->
                        [OldTask|ACC];
                    _ ->
                        ACC
                end
            end, [], NewTaskList),
    role_data:set_task_list(?TASK_TYPE_FAMILY_TODAY, NewTaskList2),
    role_task:send_notify_change(?TASK_TYPE_FAMILY_TODAY,[E||E<-NewTaskList2,E#r_task.status=/=?TASK_STATUS_COMMIT],[]);
    
do_handle_info({sync_family_task_init,NewTaskList},_RoleID) ->
    role_data:set_task_list(?TASK_TYPE_FAMILY_TODAY, NewTaskList),
    role_task:send_notify_change(?TASK_TYPE_FAMILY_TODAY,NewTaskList,[]);

do_handle_info({alien_finals_guess_failed, Result, Rank}, _RoleID) ->
    role_alien:finals_guess_failed(Result, Rank);

%% Result :: [fail|{ok,NowBuyTimes,NowRestAttackTimes}]
do_handle_info({sync_plunder_info, BuyTimes}, _RoleID) ->
    role_plunder:set_plunder_info(BuyTimes);

%% 神殿祈福（爆竹）异步处理消息接收
do_handle_info({fire_open_reuslt, Total,Count,Rank,CanReward,ReturnGold,Name,Description,Icon}, _RoleID) ->
    role_firecracker:fire_open_reuslt(Total,Count,Rank,CanReward,ReturnGold,Name,Description,Icon);

do_handle_info({fire_calc_need_gold_reuslt, Cost, Times}, _RoleID) ->
    role_firecracker:fire_calc_need_gold_reuslt(Cost, Times);

do_handle_info({fire_setoff_reuslt, RewardList, NewCount, ReturnGold, IsCanGetReward, DeductGold}, _RoleID) ->
    role_firecracker:fire_setoff_reuslt(RewardList, NewCount, ReturnGold, IsCanGetReward, DeductGold);

do_handle_info({fire_get_reward_reuslt, Result}, _RoleID) ->
    role_firecracker:fire_get_reward_reuslt(Result);
% 取消魔藤挂机
do_handle_info({ger_del_cancel_cirrus, GerId}, _RoleID) ->
    CirrusNodeList = role_home:get_cirrus_list(),
    ?INFO("ger_del_cancel_cirrus ~w ~w",[GerId,CirrusNodeList]),
    case lists:keytake(GerId, #home_cirrus.doing_ger_id, CirrusNodeList) of
        {value, Cirrus, OtherCirrusNodeList} when 
          Cirrus#home_cirrus.cost_type > 0
          andalso Cirrus#home_cirrus.doing_timestamp > 0->
            #role{roleID=RoleID} = _RoleInfo = role_data:get_roleInfo(),
            HomeInfo = home_server:get_home(RoleID),
            % 邮件形式发放奖励
            mail_server:send_sys_mail(RoleID, ?MAIL_CIRRUS_REWARD, [Cirrus#home_cirrus.index], "", Cirrus#home_cirrus.cirrus_reward),
            % role_reward:handle_sell_reward_f(RoleInfo, Cirrus#home_cirrus.cirrus_reward, ?MONEY_ADD_TYPE_CURRIS, Cirrus#home_cirrus.index, ""),
            NewCirrusInfo = Cirrus#home_cirrus{doing_ger_id = 0
                                              ,doing_timestamp = 0
                                              ,cost_type=0
                                              ,cirrus_reward = #sell_reward{} 
                                              ,reward_already = 0 
                                              ,is_get_big = false 
                                              ,doing_ger_type = 0
                                              ,doing_ger_quality = 0},
            NewCirrusNodeList = [NewCirrusInfo|OtherCirrusNodeList],
            NewHomeInfo = HomeInfo#home_info{cirrus_list=NewCirrusNodeList},
            home_server:set_home(RoleID, NewHomeInfo),
            ?sendself(#sc_home_cirrus_operate{result= 1});
        _ ->
            ignore
    end;

do_handle_info({do_mystery_box, Type}, _RoleID) ->
    role_box:do_mystery_box(Type);
%% 针对连续战斗，批量增加神秘宝箱奖励
do_handle_info({do_mystery_box_rewards,MysteryRewardList}, _RoleID) ->
    RoleInfo = role_data:get_roleInfo(),
    lists:foldr(fun(Reward,RoleInfo2) ->
                        {RoleInfo3,_GerExp} = role_reward:handle_sys_reward(RoleInfo2, Reward, ?MONEY_ADD_TYPE_BAG_BOX, 1, ""),
                        RoleInfo3
                end, RoleInfo, MysteryRewardList);

do_handle_info({cs_talk_send_whisper, SendRoleID,TalkMessage}, _RoleID) ->
    P_whisper_record = #p_whisper_record{roleID = SendRoleID,talkMessage = TalkMessage,timeStamp=util:now()},
    case get(?whisper_list) of
        WhisperList when is_list(WhisperList)->
            put(?whisper_list,[P_whisper_record|WhisperList]);
        _ ->
            put(?whisper_list,[P_whisper_record])
    end,
    ?sendself(#sc_talk_whisper_notice{});
%%     ?sendself(#sc_talk_get_whisper{record_list=[P_whisper_record]});

do_handle_info(deduct_tasklink_time,_RoleID)->
    RoleTaskLink = role_tasklink:get_role_tasklink(),
    NewRoleTaskLink = 
        if
            RoleTaskLink#role_tasklink.free_time > 0->
                RoleTaskLink#role_tasklink{free_time = RoleTaskLink#role_tasklink.free_time-1};
            RoleTaskLink#role_tasklink.buy_left_time > 0->
                RoleTaskLink#role_tasklink{buy_left_time = RoleTaskLink#role_tasklink.buy_left_time-1};
            true ->
                ?ERR("deduct_tasklink_time fail ~w",[RoleTaskLink]),
                RoleTaskLink
        end,
    role_tasklink:set_role_tasklink(NewRoleTaskLink);

%% 领取红包成功
do_handle_info({get_bonus_success, Type, BonusID, Amount, BonusInfo}, _RoleID) ->
    role_trumpet:get_bonus_success(Type, BonusID, Amount, BonusInfo);

do_handle_info(update_discount_info,_RoleID)->
	role_discount:update_discount_info();

do_handle_info(recacl_talent,_RoleID)->
    role_talent:recacl_talent();

do_handle_info({panic_buy_response,Msg},_RoleID)->
	role_panicbuy:deal_panic_buy_response(Msg);

do_handle_info({update_panic_buy_info,Msg},_RoleID)->
	role_panicbuy:update_panic_buy_info(Msg);

do_handle_info({do_family_invite, FamilyID},_RoleID)->
    case catch role_family:check_can_request_join(FamilyID) of
        {ok, RoleInfo, OwnerRoleID} ->
            erlang:send(family_manager_server, {add_role_family_request, RoleInfo, FamilyID, OwnerRoleID, false}),
            ?sendself(#sc_mail_invite_operate{result=1});
        {false, _Reason} ->
            ?sendself(#sc_mail_invite_operate{result=3})
    end;

do_handle_info({update_teamid, TeamID},_RoleID)->
	Role = role_data:get_roleInfo(),
	role_data:set_roleInfo(Role#role{teamId=TeamID});

do_handle_info(cs_trumpet_redpacket_status,RoleID)->
    erlang:send(redpacket_server, {cs_trumpet_redpacket_status
                                  ,RoleID,item_lib:get_material_num(?ITEM_REDPACKET_ID)});
do_handle_info({update_team_invite_remain_time,Msg},_RoleID)->
	role_generalteam:set_invite_remain_time(Msg);

do_handle_info({add_attack_time_done,NewTimes,AddTimes},_RoleID)->
    {NeedGold,_BuyTimes}=data_family:get(family_instance_buy_times_price),
    role_lib:deduct_gold_f(role_data:get_roleInfo(), NeedGold*AddTimes, ?MONEY_DEC_TYPE_FAMILY_INST_ATTACK_TIME, 0, ""),
    ?sendself(#sc_familyfight_bug_attack_time{result=1,new_times=NewTimes});

do_handle_info({family_instance_boss_reward,TotalReward,Type},_RoleID)->
    role_reward:handle_sell_reward_f(role_data:get_roleInfo(),TotalReward,Type,0,"");

do_handle_info({relic_reward,Reward,Type,WarId},_RoleID)->
    role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,Type,WarId,"");

do_handle_info({tasklink_reward,RewardList,Type,Index},_RoleID)->
    role_reward:handle_sys_reward(role_data:get_roleInfo(), RewardList, Type, 0,erlang:integer_to_list(Index));

%% do_handle_info(test_del_shop_num_by_shopID, _RoleID) ->
%%     role_shop:del_shop_num_by_shopID(?SHOP_ID_HONOR),
%%     role_shop:do_get_shopNumList();
%% do_handle_info(test_refresh_family_instance, _RoleID) ->
%%     #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
%%     if
%%         FamilyID =:= 0 ->
%%             ignore;
%%         true ->
%%             family_misc:router_to_family_process(FamilyID, test_refresh_family_instance)
%%     end;

%% do_handle_info(test_auto_sign,_RoleID)->
%%     relic_server:carlos_relic_sign();


do_handle_info(test_clean_dojang, _RoleID) ->
    erase(?dojang_info);
do_handle_info(test_hook_zero_clock, _RoleID) ->
    hook_zero_clock();

do_handle_info({update_role_carlos_info,UpdateMsg}=Msg,_RoleID)->
	?INFO("玩家进程收到更新卡洛斯数据消息~w~n",[Msg]),
	update_role_carlos_info(UpdateMsg);

do_handle_info({cs_carlos_relic_buy,CurBuyTime,NeedGlod},RoleID)->
    RoleInfo = role_data:get_roleInfo(),
    case role_lib:check_money(RoleInfo, gold, NeedGlod) of
        true ->
            role_lib:deduct_gold_f(RoleInfo, NeedGlod, ?MONEY_DEC_TYPE_RELIC_BUT_TIME, CurBuyTime, ""),
            relic_server:send_to_me({relic_buy_pay_success, RoleID,CurBuyTime});
        _ ->
            ?sendself(#sc_carlos_relic_buy{result=2,cur_times=0,times=0,next_need=0})
    end;
do_handle_info({relic_to_client,Msg},_RoleID)->
    ?sendself(Msg);

do_handle_info({deduct_carlostime,{Time,Type,SeasonType}},_RoleID)->
	do_deduct_carlostime(Time,Type,SeasonType);

do_handle_info({update_awake_info,GerID,Step,SkillID,RecastTime},_RoleID)->
	role_awake:do_fix_role_ger_awakeinfo(GerID,Step,SkillID,RecastTime);

do_handle_info({update_crystal_info,GerID,CrystalInfo},_RoleID)->
	role_crystal:do_fix_role_ger_crystal(GerID,CrystalInfo);

do_handle_info({update_crystal_info,GerID,Type,Level,Quality},_RoleID)->
	role_crystal:do_fix_role_ger_crystal2(GerID,Type,Level,Quality);

do_handle_info({test_set_talent,List},_RoleID)->
	role_talent:test_set_talent({test_set_talent, List});

do_handle_info({cancel_del_ger,GerIdList},_) ->
    role_home:cancel_del_ger(GerIdList);

% 其他人领取了家园悬赏任务，如果任务主人在线的话，需要设定主人下一次生成新任务的timer
do_handle_info({set_new_home_task_timer,NewTaskWait},RoleID) ->
    HomeInfo = home_server:get_home(RoleID),
    Now = util:now(),
    role_home:home_start_timer(NewTaskWait*1000,{timer_add_new_home_task,NewTaskWait+Now}),
    NewTaskTimer = [NewTaskWait+Now|HomeInfo#home_info.new_task_timer],
    home_server:set_home(RoleID,HomeInfo#home_info{new_task_timer=NewTaskTimer});

do_handle_info({dounty_task_timeout,TaskId},_) ->
    role_home:do_home_timer_msg({refresh_home_task,TaskId,?ht_dounty_unaccept});

do_handle_info({timeout, _TimerRef, {home,Msg}},_) ->
    role_home:do_home_timer_msg(Msg);

%报名双排时，设定匹配超时计时器
do_handle_info({timeout, TimerRef, {dm_sign_timeout}},RoleID) ->
    case erlang:get(?sign_timeout_ref) of
        ?undefined ->
            ignore;
        TimerRef ->
            erlang:erase(?sign_timeout_ref),
            doublematch_server:doublematch_cancel(RoleID);
        _ ->
            % old timer ref
            ignore
    end;

%主动取消匹配或匹配成功时，取消匹配超时计时器
do_handle_info({clear_dm_timer,TimerRef},_) ->
    case erlang:get(?sign_timeout_ref) of
        ?undefined ->
            ignore;
        TimerRef ->
            erlang:erase(?sign_timeout_ref),
            erlang:cancel_timer(TimerRef);
        _ ->
            % old timer ref
            ignore
    end;

do_handle_info(reset_teamPkTimes,_) ->
    role_data:reset_teamPkTimes();

do_handle_info({set_signinfo,SignInfo},_RoleID)->
	role_data:set_signinfo(SignInfo),
	role_sign:send_sign_reward_info();

do_handle_info({twins_reward,Reward},_RoleID) ->
	Role = role_data:get_roleInfo(),
	role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_TWINS_FIGHT, 0, ""),
	ok;

do_handle_info({set_role_vip,VipLevel},_RoleID)->
	case is_integer(VipLevel) andalso VipLevel =< 16 of
		true ->
			Role = role_data:get_roleInfo(),
			Role2 = Role#role{vipLevel=VipLevel},
			role_data:set_roleInfo(Role2),
			RoleTimes=role_data:get_roleTimes(),
			VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(VipLevel),
			VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(VipLevel),
			NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(Role#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
			RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
											 challengeGodBuyTimes=VipChallengeGodBuyTimes
											 },
			role_data:set_roleTimes(RoleTimes2),
			?notify_update(?ra_vipLevel(VipLevel, NewChallengeGodEnergy,VipChallengeGodBuyTimes));
		_ ->
			ignore
	end;
do_handle_info({test_set_skininfo,SkinInfo},_RoleID)->
	role_skin:test_set_skininfo(SkinInfo);

do_handle_info({test_dm_sign,TeamMember},_RoleID)->
    doublematch_server:doublematch_sign(_RoleID, -1, TeamMember);

do_handle_info({test_set_chapter,Chapter,Current},_RoleID)->
    role_battle:set_progress(?BATTLE_DUNGEON_TYPE_NORMAL,Current),
    role_battle:set_chapter(Chapter);
do_handle_info(reset_transmigration,_RoleID)->
    OldRoleInfo = role_data:get_roleInfo(),
    NewRoleInfo = OldRoleInfo#role{transmigration = 0},
    role_data:set_roleInfo(NewRoleInfo);

do_handle_info({fixup_online_role_dungeon, RoleID, DungeonList},_RoleID) ->
    user_default:fixup_online_role_dungeon(RoleID, DungeonList);

do_handle_info({gold2goldbonus,{OldType,NewType,Type,AccountID,LogTimeStamp}},_RoleID)->
	role_lib:transform_gold2goldbonus(OldType,NewType,Type,AccountID,LogTimeStamp);

do_handle_info({update_main_gerTypeid,GerTypeID},_RoleID)->
	role_data:set_main_gerTypeID(GerTypeID);

do_handle_info({refresh_hroninfo,Msg},_RoleID)->
	role_hron:do_refresh_info(Msg);

do_handle_info({test_set_role_hronhistory,Type,Score},_RoleID)->
	role_hron:do_test_set_role_hronhistory(Type,Score);

do_handle_info({test_change_role_anubisinfo,SeasonID,AddKillNum,AddResourcePoint},RoleID)->
	role_familycross:do_test_change_role_anubisinfo(RoleID,SeasonID,AddKillNum,AddResourcePoint);

do_handle_info({test_change_role_battle_boss_status_for_payGuide,DungeonType,DungeonIDList,Status},_RoleID)->
	role_battle:do_change_online_role_battle_boss_box_status(DungeonType,DungeonIDList,Status);

do_handle_info(test_clear_ai_flag,_RoleID)->
    role_data:set_plane_ai_flag_all(0);

do_handle_info({test_sc_msg,Msg},_RoleID)->
    ?sendself(Msg);

do_handle_info({change_holygrail,Msg},_RoleID)->
	role_diamond:do_test_change_holygrail(Msg);

do_handle_info(State, Info) ->
    ?ERR("do_handle_info function clause:request=~100p,state=~100p",[Info, State]).
  
notice_family_online(IsOnline) ->
    #role{roleID=RoleID,familyID=FamilyID} = RoleInfo = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            %%FIXME Version 1.2.5 Bug修复,后续版本需要删除掉
            case is_in_family(RoleID) of
                true ->
                    family_misc:router_to_family_process(FamilyID, {update_role_online, RoleID, IsOnline});
                _ ->
                    ?ERR("清理玩家:~p的异常公会ID:~p.~n", [RoleID, FamilyID]),
                    db_sql:update_role_family_id(RoleID, 0),
                    role_data:set_roleInfo(RoleInfo#role{familyID=0, lastJoinFamily=util:now()}),
                    family_misc:unlock_family_protect(RoleID)
            end;
        false ->
            ignore
    end.

write_term(Term) ->
	Bin = io_lib:format("~w",[Term]),
	file:write_file("d:/log",Bin,[append]).

%%FIXME Version 1.2.5 Bug修复,后续版本需要删除掉
is_in_family(RoleID) ->
    Sql = io_lib:format("select `roleID` from `gFamilyMember` where `roleID`=~w", [RoleID]),
    db_sql:get_rows(Sql) =/= [].

%% 删除装备

delete_item(DonateNum,ItemTypeID,_RoleID,BagEquip) ->
	SameItemTypeIDList = lists:filter(fun(Item) ->
			#item{itemTypeID = ItemTypeID2,itemType = ItemType2,itemRank = ItemRank2,itemPos = ItemPos2} = Item,
			ItemTypeID2 == ItemTypeID andalso ItemType2 =/= stone andalso ItemRank2 == 0 andalso ItemPos2 == 0
		end,BagEquip),
	%?ERR("SameItemTypeIDList is ~w ~n",[SameItemTypeIDList]),
			
	OtherItemList = lists:filter(fun(Item) ->
			#item{itemTypeID = ItemTypeID3,itemType = ItemType3,itemRank = ItemRank3,itemPos = ItemPos3} = Item,
			ItemTypeID3 =/= ItemTypeID orelse ItemType3 == stone orelse ItemRank3 =/= 0 orelse ItemPos3 =/= 0
		end,BagEquip),
	%?ERR("OtherItemList is ~w ~n",[OtherItemList]),

	{DeletAfterList,LastDeleteList,_AccLast} =lists:foldl(fun(ItemInfo,{ResultList,DeleteList,Acc}) ->
		#item{itemNum = ItemNum4} = ItemInfo,
			case Acc > 0 of
				true ->
					if 	Acc >= ItemNum4 ->
							{ResultList,[ItemInfo|DeleteList],Acc-ItemNum4};
						Acc < ItemNum4 ->
							{[ItemInfo#item{itemNum=ItemNum4-Acc}|ResultList],[ItemInfo#item{itemNum=Acc}|DeleteList],0}
					end;
				false ->
					{[ItemInfo|ResultList],DeleteList,0}
			end
		end,{[],[],DonateNum},SameItemTypeIDList),
	FinialList = lists:merge(DeletAfterList,OtherItemList),
	{FinialList,LastDeleteList}.

%%删除精灵
delete_ger(DonateNum,ItemTypeID,RoleID,BagGer) ->
	HomeSteadGerID = homestead_server:get_homestead_ger(RoleID),
	{LastResultList,LastDeleteList,_LastAcc} = lists:foldl(fun(GerInfo,{ResultList,DeleteList,Acc}) ->
		#gerSimple{gerTypeID = GerTypeID,gerQuality = GerQuality,gerPos = GerPos,gerID=GerID} = GerInfo,
		case Acc > 0 of
			true ->
				case GerTypeID == ItemTypeID andalso GerQuality == 0 andalso GerPos == 0 andalso GerID =/= HomeSteadGerID of
					true ->
						{ResultList,[GerInfo|DeleteList],Acc -1};
					false ->
						{[GerInfo|ResultList],DeleteList,Acc}
				end;
			false ->
				{[GerInfo|ResultList],DeleteList,0}
		end
		end,{[],[],DonateNum},BagGer),
	{LastResultList,LastDeleteList}.

update_role_carlos_info({WinTime,EqualTime,LoseTime,SeasonID,_RankScore,LastWinTime,LastEqualTime,LastLoseTime,LastSeasonID})->
	RoleInfo = role_data:get_roleInfo(),
	{_RankInfoList,CurrentSeasonID}=gen_server:call(carlos_server, {get_seasoninfo,?CURRENTSEASON}),
	%%由于添加了修改玩家卡洛斯信息的接口，所以此处收到的消息可能是玩家上个赛季的数据修改信息，所以要进行特殊处理
	case RoleInfo#role.carloslastseasonid=:=SeasonID andalso CurrentSeasonID=/=SeasonID of
		true->
			NewRoleInfo=RoleInfo#role{carloslastwintime=WinTime,carloslastequaltime=EqualTime,carloslastlosetime=LoseTime};
		false->
			NewRoleInfo=RoleInfo#role{carloswintime=WinTime,carlosequaltime=EqualTime,carloslosetime=LoseTime,carlosseasonid=SeasonID,carloslastwintime=LastWinTime,carloslastequaltime=LastEqualTime,carloslastlosetime=LastLoseTime,carloslastseasonid=LastSeasonID}
	end,
	role_data:set_roleInfo(NewRoleInfo).

do_deduct_carlostime(Time,Type,SeasonType)->
	RoleInfo = role_data:get_roleInfo(),
    NewRoleInfo = case SeasonType of
        ?CURRENTSEASON->
            case Type of
                win->
                    RoleInfo#role{carloswintime=erlang:max(0,RoleInfo#role.carloswintime-Time)};
                equal->
                    RoleInfo#role{carlosequaltime=erlang:max(0,RoleInfo#role.carlosequaltime-Time)};
                lose->
                    RoleInfo#role{carloslosetime=erlang:max(0,RoleInfo#role.carloslosetime-Time)};
                _ ->
                    ?INFO("undefined Type:~w ~n",[Type]),
                    RoleInfo
            end;
        ?PRESEASON->
            case Type of
                win->
                    RoleInfo#role{carloslastwintime=erlang:max(0,RoleInfo#role.carloslastwintime-Time)};
                equal->
                    RoleInfo#role{carloslastequaltime=erlang:max(0,RoleInfo#role.carloslastequaltime-Time)};
                lose->
                    RoleInfo#role{carloslastlosetime=erlang:max(0,RoleInfo#role.carloslastlosetime-Time)};
                _ ->
                    ?INFO("undefined Type:~w ~n",[Type]),
                    RoleInfo
            end; 
        _ ->
            ?INFO("undefined SeasonType:~w ~n",[SeasonType]),
            RoleInfo
    end,
    role_data:set_roleInfo(NewRoleInfo).

%%所有数据初始化完成之后将会调用这个函数，应该在这个函数里面来处理各种玩家任务状态的判断工作，防止出现判断状态是有的数据不存在
after_loading_data_hook(State)->
	role_maintask:check_maintask_and_trigger(),
	role_payGuide:check_headSeven_and_trigger(),
	State.
