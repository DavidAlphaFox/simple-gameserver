%% @author caohongyang
%% @doc 战南蛮活动进程
%% Created 2013-5-6

%% 本进程在玩家进程的上层（可以call玩家进程，但玩家进程不能call本进程）

%% RankList 顺序为: 高分数在低位.

-module(nanm_server).
-compile(export_all).

-behaviour(gen_server).
-include("def_role.hrl").
-include("def_mail.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add_buff/2,fight/7,reborn/7,offline/1,set_offlinePlayFlag/2]).

-export([i/0,k/0,data_nanm_transform_list/1]).

-define(STATE_END,1).
-define(STATE_PREPARE,2).
-define(STATE_FIGHT,3).
-define(STATE_REWARD,4).
-define(state_end,5).
-define(state_begin,8).
%% -define(LUCK_REWARD_NUM, 3).改用配置文件配置百分比
-define(RANK_REWARD_NUM, 10).
-define(BC_INTERVAL, (30 * 1000)).
-define(BC_BEFORE_BEGIN_SEC, 300).
-define(PERSIST_INTERVAL,300).

-define(REWARD_HAS_GET,1).
-define(REWARD_NO_GET,2).
-define(REWARD_NO_RIGHT,3).
-define(REWARD_BOSS_NOT_KILL,4).

-define(KILL_BOSS_SUCCESS,1).
-define(KILL_BOSS_FAIL,2).
%% ===================Dict Key Begin =========================
-define(joinFlag, joinFlag).% 玩家加入标志
-define(bcList, bcList). % 广播列表
-define(nanm, nanm). % 玩家状态
-define(lastInfo, lastInfo). %前回战报
-define(rankList, rankList). %排行榜信息
-define(begin_time, begin_time).%本次活动开始时间
-define(offlinePlayFlag, offlinePlayFlag).%离线参与的标识
-define(harm, harm).% 等待广播的伤害列表
-define(first10,first10).% 前10名
-define(rebronPlayer,rebronPlayer).% 使用了复活的玩家列表（会增加下一回合的攻击百分比加成）
-define(TimerRef,timerRef).
-define(AlreadyRewardList,alreadyrewardlist).
%% ===================Dict Key End   =========================

%% 虎牢关持久化信息
-record(d_nanm,{
				bossQuality	%% boss当前等级
				,lastInfo	%% 上次战斗信息
				,failTimesAcc %% 连续未杀死boss的次数
				,lastBeginTime%% 上次活动开始时间
			   }).

-record(nanm, {
			   harm % 累计伤害
			   ,coinAcc%累计获得银两
			   ,repuAcc%累计获得声望
			   ,rank%当前排名
			   ,rebornTime%复活时间
			   ,name%名字
			  }).

%% 世界boss全局信息
-record(bossBase,{bossQuality=0,bossMaxHp=0,bossGerTypeID=0}).
-record(st, {
			 state = ?STATE_END %活动状态
			 ,bossBase ::#bossBase{}% BOSS基础信息
			 ,boss	:: #ger{}% BOSS实例
			 ,buffNum :: ?int16 %% 擂鼓层数
			 ,bossTotalHp :: ?int32 %% BOSS 现在的总血量
			 ,nextStateTime :: ?int32
			 ,lastresult = ?KILL_BOSS_FAIL
			 ,starttime :: ?int32
			 ,lastBossGerTypeID :: ?int32
			 ,get_reward_list=[]
			 ,lastBossGerQuality :: ?int32 
			}).

%% 排行榜信息
-record(rank, {
			   roleID 
			   ,name
			   ,harm
			   ,is_get_reward=2
			  }).

-define(changeBossHp(Ger, NewHp),(Ger#ger{gerHp=NewHp})).


i() ->
	gen_server:call(?MODULE, i).

k() ->
	db_sql:set_etc(?DB_ETC_KEY_NANM, []),
	user_default:kill(?MODULE).

set_offlinePlayFlag(RoleID, OpenFlag) ->
	erlang:send(?MODULE, {set_offlinePlayFlag, RoleID, OpenFlag}).

add_buff(RoleID, BuffNum) ->
	erlang:send(?MODULE, {add_buff, RoleID, BuffNum}).

fight(RoleID, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList) ->
	erlang:send(?MODULE, {fight, RoleID, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList}).

offline(RoleID) ->
	erlang:send(?MODULE, {offline, RoleID}).

is_state_begin() ->
    case gen_server:call(?MODULE, get_state) of
        #st{state= ?STATE_FIGHT} ->
            true;
        _ ->
            false
    end.

reborn(RoleID, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList) ->
	erlang:send(?MODULE, {reborn, RoleID, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList}).

highlight_push(RoleID) ->
	erlang:send(?MODULE, {push_info, RoleID}).

start() ->
	{ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	init_offlinePlayFlag(),
	%% 获取持久化信息
	D_Nanm = get_d_nanm(),
	?INFO("Nanm:~w ~n",[D_Nanm]),
	set_last_info(D_Nanm#d_nanm.lastBeginTime, D_Nanm#d_nanm.lastInfo),
	St = init_state(D_Nanm),
	case St#st.state =:= ?STATE_FIGHT of
		true->
			sync_tick(0,St#st.bossBase#bossBase.bossMaxHp);
		false->
			ignore
	end,
	ets:new(?ETS_NANM_BUFF_FLAG, [set,public, {keypos,1},named_table]),
	%%定时持久化数据
	erlang:send_after(?PERSIST_INTERVAL*1000,?MODULE,do_persit),
    {ok, St}.

init_boss(BossQuality1,BandGerTypeID) ->
    %%此处生成boss，需要修改，从配置文件中获取的boss信息
    case BossQuality1 >390 of
    	true->
    		BossQuality = 390;
    	false->
    		BossQuality = BossQuality1
    end,
    GerTypeID = generate_boss_typeID(BandGerTypeID),
    ?INFO("LastGerTypeID:~w NewGerTypeID:~w ~n",[BandGerTypeID,GerTypeID]),
    % GerTypeID = 10001,
	BossInfo = data_nanm:get(boss_info),
	AwakeInfo = generate_boss_awakeinfo(GerTypeID,BossQuality),
	{BossInfo2, BossTotalHp} = lists:foldl(fun({_GerTypeID, GerLevel, GerPos},{BossAcc,HpAcc})->
						BossGer = ger_attr:new_ger_with_awake(GerTypeID, GerLevel, BossQuality, [], [],AwakeInfo),
						BossGer2 = ?change_pos(BossGer, GerPos),
						{[BossGer2|BossAcc],BossGer2#ger.gerHp + HpAcc}
				end, {[],0}, BossInfo),
	{BossInfo2, #bossBase{bossQuality=BossQuality,bossMaxHp=BossTotalHp,bossGerTypeID=GerTypeID}}.

init_state(D_Nanm) ->
	case db_sql:get_etc(?DB_ETC_KEY_WORLDBOSS) of
		[]->
			init_state2(D_Nanm);
		State->
			update_state(State,D_Nanm)
	end.
%%实际初始化进程状态
init_state2(D_Nanm)->
	StartTimeList = data_nanm:get(fight_start_time),
	StartTimeStamp = get_early_starttime(StartTimeList),
	PrepareInterval = data_nanm:get(prepare_time_interval),
	PrepareTime = StartTimeStamp-PrepareInterval,
	FightInterval = data_nanm:get(fight_time_interval),
	RewardTime = StartTimeStamp+FightInterval,
	RewardInterval = data_nanm:get(reward_time_interval),
	EndTime = RewardTime + RewardInterval,
	NowSec = util:now(),
	{State,NextStateTimeStamp} = if
		NowSec < PrepareTime ->
			TimerRef = erlang:send_after((PrepareTime-NowSec)*1000,?MODULE,{change_status,?STATE_END}),
			put(?begin_time, StartTimeStamp),
			{?STATE_END,PrepareTime};
		NowSec >= PrepareTime andalso NowSec < StartTimeStamp ->
			TimerRef = erlang:send_after((StartTimeStamp-NowSec)*1000,?MODULE,{change_status,?STATE_PREPARE}),
			put(?begin_time, StartTimeStamp),
			{?STATE_PREPARE,StartTimeStamp};
		NowSec >= StartTimeStamp andalso NowSec < RewardTime ->
			TimerRef = erlang:send_after((RewardTime-NowSec)*1000,?MODULE,{change_status,?STATE_FIGHT}),
			put(?begin_time, util:now()),
			{?STATE_FIGHT,RewardTime};
		NowSec >= RewardTime andalso NowSec <EndTime ->
			TimerRef = erlang:send_after((EndTime-NowSec)*1000,?MODULE,{change_status,?STATE_REWARD}),
			put(?begin_time, StartTimeStamp),
			{?STATE_REWARD,EndTime};
		true ->
			TimerRef = erlang:send_after((PrepareTime+?ONE_DAY_SECONDS-NowSec)*1000,?MODULE,{change_status,?STATE_END}),
			put(?begin_time, StartTimeStamp),
			{?STATE_END,PrepareTime+?ONE_DAY_SECONDS}
	end,
	BossQuality = D_Nanm#d_nanm.bossQuality,
	{Boss, BossBaseInfo} = init_boss(BossQuality,0),
	StartTime = get(?begin_time),
	A = #st{boss=Boss,bossBase=BossBaseInfo,state=State, buffNum = get_buff() ,bossTotalHp = BossBaseInfo#bossBase.bossMaxHp,nextStateTime=NextStateTimeStamp,lastBossGerTypeID=BossBaseInfo#bossBase.bossGerTypeID,get_reward_list=[],lastBossGerQuality=BossQuality,starttime=StartTime},
	?INFO("A:~w ~n",[A]),
	put(?TimerRef,TimerRef),
	A.

%%从数据库中读出持久化的状态信息之后，对状态信息进行更新,如果不能再一个状态内重启，则放弃状态修复，直接根据上次的战斗数据生成新的状态
update_state(State,D_Nanm) when is_record(State,st)->
	NowSec = util:now(),
	%?ERR("NextTime:~w NowSec:~w ~n",[State#st.nextStateTime,NowSec]),
	case NowSec < State#st.nextStateTime of
		true->
			TimerRef = erlang:send_after((State#st.nextStateTime-NowSec)*1000,?MODULE,{change_status,State#st.state}),
			case get(?TimerRef) of
				?undefined->
					ignore;
				OldTimerRef->
					erlang:cancel_timer(OldTimerRef)
			end,
			put(?TimerRef,TimerRef),
			put(?begin_time,State#st.starttime),
			State;
		false->
			%?ERR("test~n"),
			init_state2(D_Nanm)
	end;
update_state(_State,D_Nanm) ->
	init_state2(D_Nanm).


sync_tick(Tick, BossHp) ->
	erlang:send_after(1000, self(), {sync_tick,Tick, BossHp}).

%% sec_till_begin(NowTime, StartTime) when NowTime <StartTime ->
%% 	time_diff(NowTime, StartTime);
%% sec_till_begin(NowTime, StartTime) ->
%% 	time_diff(NowTime, StartTime)+?ONE_DAY_SECONDS.
										   
time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).
	

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
handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(get_state, _From, State) ->
    {reply, State, State};
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
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(Info, State) ->
	case ?CATCH(do_handle_info(State#st.state, State, Info)) of
		#st{} = NewState->
			{noreply, NewState};
		_ ->
			{noreply, State}
 end.


-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	do_persit(State),
	persist_offlinePlayList(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.



-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

set_bcList(List) ->
	erlang:put(?bcList, List).
get_bcList() ->
	case erlang:get(?bcList) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

set_joinFlag(RoleID) ->
	erlang:put({?joinFlag,RoleID},1).
clear_joinFlag(RoleID) ->
	erlang:erase({?joinFlag,RoleID}).
is_join(RoleID) ->
	erlang:get({?joinFlag,RoleID}) =:= 1.

join_nanm(RoleID) -> 
	case is_join(RoleID) of
		true ->
			ignore;
		false ->
			
			set_joinFlag(RoleID),
			GW = role_lib:gw(RoleID),
			set_bcList([GW|get_bcList()])
	end.
quit_nanm(RoleID) ->
	case is_join(RoleID) of
		true ->
			clear_joinFlag(RoleID),
			set_bcList(lists:delete(role_lib:gw(RoleID), get_bcList()));
		false ->
			ignore
	end.

get_buff_flag(RoleID) ->
	case ets:lookup(?ETS_NANM_BUFF_FLAG, RoleID) of
		[] ->
			false;
		[_] ->
			true
	end.



%% 从数据库中初始化离线参与的玩家列表
init_offlinePlayFlag() ->
	RoleIDList = get_db_offlinePlayList(),
	lists:foreach(fun(RoleID) -> set_offlinePlayFlag(RoleID) end, RoleIDList).

%% 离线参与虎牢关的标识
get_offlinePlayFlag(RoleID) ->
	case erlang:get({?offlinePlayFlag,RoleID}) of
		1 ->
			true;
		_ ->
			false
	end.

set_offlinePlayFlag(RoleID) ->
	erlang:put({?offlinePlayFlag, RoleID}, 1).

%% 获取离线参与虎牢关的玩家ID列表
get_offlinePlayList() ->
	[RoleID||{{?offlinePlayFlag,RoleID},1} <-erlang:get()].

get_db_offlinePlayList() ->
	case db_sql:get_etc(?DB_ETC_KEY_NANM_OFFLINE_PLAY) of	
		List when is_list(List)->
			List;
		_ ->
			[]
	end.
	
%% 持久化离线参与虎牢关的玩家ID
persist_offlinePlayList() ->
	db_sql:set_etc(?DB_ETC_KEY_NANM_OFFLINE_PLAY, get_offlinePlayList()).

sc_nanm_open(RoleID, St) ->
	RewardState = check_reward(RoleID,St), 
	#st{bossBase=BossBaseInfo,buffNum=BuffNum,state=State} = St,
	Record = 
	#sc_nanm_open{
				  gerTypeID=BossBaseInfo#bossBase.bossGerTypeID,
				  bossQuality=BossBaseInfo#bossBase.bossQuality,
				  buffNum=BuffNum,
				  isBuffed=get_buff_flag(RoleID),
				  isOpen=State,
				  maxHp=BossBaseInfo#bossBase.bossMaxHp,
				  isOfflinePlay=get_offlinePlayFlag(RoleID),
				  beginTime=St#st.nextStateTime,
				  is_get_reward=RewardState
				 },
	?INFO("sc_nanm_open:~w,~w\n",[RoleID,Record]),
	?unicast(RoleID, Record).
			

get_nanm(RoleID) ->
	case erlang:get({?nanm,RoleID}) of
		#nanm{} =H ->
			H;
		_ ->
			RoleName = role_lib:get_name(RoleID),
			#nanm{coinAcc=0,harm=0,name=RoleName,rank=0,rebornTime=0,repuAcc=0}
	end.

set_nanm(RoleID, Nanm) when is_record(Nanm, nanm)->
	erlang:put({?nanm,RoleID}, Nanm).
			
sc_nanm_init_state(RoleID, St) ->
	#st{bossTotalHp=BossHp}=St,
	#nanm{harm=Harm,rank=Rank,rebornTime=RebornTime} = get_nanm(RoleID),
	Record = #sc_nanm_init_state{curHarm=Harm,curHp=BossHp,curRank=Rank,rebornTime=RebornTime},
	?unicast(RoleID, Record).

get_last_info() ->
	case erlang:get(?lastInfo) of
		{_,_}=I ->
			I;
		?undefined ->
			LastInfo = #sc_nanm_last_info_fail{bossQuality=data_nanm:get(boss_init_quality),intervalSec=-1,gerTypeID=10001},
			InfoBin = proto:encode(LastInfo),
			Info = {0, InfoBin},
			put(?lastInfo,Info),
			Info
	end.

set_last_info(SaveID, Info) when is_tuple(Info)->
	?INFO("set_last_Info:~w ~n",[Info]),
	put(?lastInfo, {SaveID,proto:encode(Info)}).

sc_last_info(RoleID, CurSaveID) ->
	case get_last_info() of
		{CurSaveID,_} ->
			?unicast(RoleID, #sc_nanm_last_info_ignore{});
		{_, InfoBin} ->
			?unicast(RoleID, InfoBin);
		_ ->
			LastInfo = #sc_nanm_last_info_fail{bossQuality=data_nanm:get(boss_init_quality),intervalSec=-1,gerTypeID=10001},
			InfoBin = proto:encode(LastInfo),
			put(?lastInfo,InfoBin)
	end.

%% 请求复活时，修改数据
reborn(RoleID) ->
	%% 战斗力增强
	erlang:put({?rebronPlayer,RoleID}, data_nanm:get(add_attackratio)),
	
	RoleNanm = get_nanm(RoleID),
	RoleNanm2 = RoleNanm#nanm{rebornTime=0},
	set_nanm(RoleID, RoleNanm2). 
	
%% 当前战报
sc_nanm_cur_info(RoleID) ->	
	?unicast(RoleID, #sc_nanm_cur_info{nanmInfoList=get_first10()}).

%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info(_, _St, {inet_reply,_S,_Status}) ->
    ignore;
do_handle_info(_, _St, {Ref,_Res}) when is_reference(Ref) ->
    ignore;

do_handle_info(_,St,{change_status,State})->
	?INFO("change_status State:~w St:~w ~ndictionary:~w ~n",[State,St,user_default:get_process_dictionary(?MODULE)]),
	{NextState,NextTime} = get_next_state_and_time(State),
	?INFO("NextState:~w NextTime:~w ~n",[NextState,util:seconds_to_datetime(NextTime)]),
	case NextState of
		?STATE_FIGHT->
			BossBase = St#st.bossBase,
			BuffNum = St#st.buffNum,
			LastResult = St#st.lastresult,
			NewSt = St,
			?INFO("开始TICK~n"),
			put(?begin_time,util:now()),
			%推送世界BOSS开始打的信息
			erlang:send(?MODULE,set_state_begin),
			sync_tick(0,BossBase#bossBase.bossMaxHp);
		?STATE_REWARD ->
			%%　擂鼓标识
			ets:delete_all_objects(?ETS_NANM_BUFF_FLAG),
			case St#st.lastresult =/= ?KILL_BOSS_SUCCESS of
				true->
					NewSt = do_fail(St);
				false->
					final_bc(1),
					NewSt = St
			end,
			LastResult = St#st.lastresult,
			BuffNum = 0;
		?STATE_PREPARE ->
			clear_info(),
			clear_harmList(),
			set_rankList([]),
			NewSt = St#st{get_reward_list=[]},
			LastResult = ?KILL_BOSS_FAIL,
			BuffNum = St#st.buffNum;
		_ ->
			NewSt = St,
			LastResult = St#st.lastresult,
			BuffNum = St#st.buffNum
	end,
	NowSec = util:now(),
	TimeRef = erlang:send_after((NextTime-NowSec)*1000,?MODULE,{change_status,NextState}),
	put(?TimerRef,TimeRef),
	StartTime = get(?begin_time),
	NewSt#st{state=NextState,nextStateTime=NextTime,buffNum=BuffNum,lastresult=LastResult,starttime=StartTime};

do_handle_info(?STATE_FIGHT, St, {client_msg, RoleID, #cs_nanm_open{}}) ->
	case St#st.nextStateTime < util:now() of
		true->
			?ERR("下个状态时间为过去时，出现BUG~n"),
			ignore;
		false->
			join_nanm(RoleID),
			sc_nanm_open(RoleID, St),
			sc_nanm_init_state(RoleID, St)
	end;
do_handle_info(?STATE_FIGHT, _St,{client_msg, RoleID, #cs_nanm_close{}}) ->
	quit_nanm(RoleID);
do_handle_info(?STATE_FIGHT, St, {fight, RoleID, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList}) ->
	%%防止不在战斗状态进入界面不能接收到关闭等消息
	join_nanm(RoleID),
	do_fight(RoleID, St, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList);
%% 活动结束时间到了
do_handle_info(?STATE_FIGHT,St,set_state_end) ->
	do_fail(St);
do_handle_info(?STATE_FIGHT, _St, set_state_begin) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=1}),
	ignore;
do_handle_info(?STATE_FIGHT, St, {reborn, RoleID, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList}) ->
	reborn(RoleID),
	?unicast(RoleID,#sc_nanm_reborn{result=1}),
	do_fight(RoleID, St, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList);
do_handle_info(?STATE_FIGHT, _St, {client_msg,RoleID, #cs_nanm_cur_info{}}) ->
	sc_nanm_cur_info(RoleID);
do_handle_info(?STATE_FIGHT, St,{sync_tick,Tick, BossOldHp}) ->
	do_sync_tick(St, Tick, BossOldHp);
do_handle_info(?STATE_FIGHT, _St, {client_msg,RoleID, #cs_nanm_rank_sync{}}) ->
	sc_nanm_cur_info(RoleID);
do_handle_info(?STATE_FIGHT, _St, {client_msg,RoleID, #cs_nanm_open_time{}}) ->
	join_nanm(RoleID),
	?unicast(RoleID, #sc_nanm_open_time{beginTime=0});
do_handle_info(_,_St,{sync_tick,_Tick, _BossOldHp})->
	ignore;
	% sync_tick(Tick,-1);
do_handle_info(_State, St, {client_msg, RoleID, #cs_nanm_open{}}) ->
	case St#st.nextStateTime < util:now() of
		true->
			?ERR("下一状态时间为过去时，出现BUG~n"),
			ignore;
		false->
			join_nanm(RoleID),
			sc_nanm_open(RoleID,St)
	end;
do_handle_info(_State, _St, {client_msg, RoleID, #cs_nanm_close{}}) -> 
	quit_nanm(RoleID);
do_handle_info(_State, _St, {client_msg, RoleID, #cs_nanm_open_time{}}) ->
	?unicast(RoleID, #sc_nanm_open_time{beginTime=get(?begin_time)});
do_handle_info(_State, _St, {fight, RoleID, _FighterList, _LieuAdd,_TalentList,_,_SkinInfo}) ->
	?unicast(RoleID, #sc_nanm_fight{fightInfo=[],rebornTime=0,result=4,rewardCoin=0,rewardReputation=0});
%% 活动开始
% do_handle_info(?state_end, St, set_state_begin) ->
% 	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=1}),
% 	NowSec = util:now(),
% 	EndTime = util:datetime_to_seconds({erlang:date(),data_nanm:get(stop_time)}),
% 	Interval = EndTime - NowSec,
% 	erlang:send_after(Interval*1000, self(), set_state_end),
% 	put(?begin_time, NowSec),
% 	St#st{state=?state_begin};
do_handle_info(?state_end, _St, set_state_end) ->
	ignore;
do_handle_info(_State, _St, {reborn, RoleID,_}) ->
	?unicast(RoleID,#sc_nanm_reborn{result=3});
do_handle_info(_State, _St, {reborn, RoleID}) ->
	?unicast(RoleID,#sc_nanm_reborn{result=3});
do_handle_info(_State, _St, {client_msg,RoleID, #cs_nanm_cur_info{}}) ->
	?unicast(RoleID,#sc_nanm_cur_info_ignore{});
% do_handle_info(State, _St,{sync_tick,Tick, _BossOldHp}) ->
% 	sync_tick(Tick,-1);


do_handle_info(_, _St, {set_offlinePlayFlag,RoleID, OpenFlag}) ->
	if OpenFlag =:= true ->
		   set_offlinePlayFlag(RoleID)
	end,
	?unicast(RoleID,#sc_nanm_offline_play{result=1,newOpenFlag=OpenFlag});

do_handle_info(_, _St, {client_msg, RoleID, #cs_nanm_last_info{curSavedInfoID=CurSaveID}}) ->
	sc_last_info(RoleID, CurSaveID);

%% 擂鼓处理，已经扣了钱了。设计时已考虑到扣了钱，但是无法擂鼓。此bug由前端控制。
do_handle_info(_, St, {add_buff, _RoleID, BuffNum}) ->
	BuffNum2 = St#st.buffNum+BuffNum,
	set_buff2etc(BuffNum2),
	St2 = St#st{buffNum=BuffNum2},
	%% 每个玩家只能点一次擂鼓，故可以即时同步
	?INFO("BuffNum2:~w ~n",[BuffNum2]),
	bc(#sc_nanm_buff_sync{buffNum=BuffNum2}),
	St2;
%% 玩家离线，推出广播列表
do_handle_info(_, _St, {offline, RoleID}) ->
	quit_nanm(RoleID);

do_handle_info(?STATE_FIGHT, _St, {push_info, RoleID}) ->
	?unicast(RoleID, #sc_push_highlight_Info{value=2, type=1});
do_handle_info(_State, _St, {push_info, _RoleID}) ->
	ignore;
do_handle_info(?STATE_FIGHT, _St, bc_state) ->
    erlang:send_after(?BC_INTERVAL, self(), bc_state),
    broadcast_server:bc_msgID(10036);
do_handle_info(_State, _St, bc_state) ->
    erlang:send_after(?BC_INTERVAL, self(), bc_state),
    case erlang:get(?begin_time) of
        Timestamp when erlang:is_integer(Timestamp) ->
            Now = util:datetime_to_seconds(erlang:localtime()),
            case Now < Timestamp andalso Timestamp - Now =< ?BC_BEFORE_BEGIN_SEC of
                true ->
                    broadcast_server:bc_msgID(10035);
                false ->
                    next
            end;
        _ ->
            next
    end;

do_handle_info(_,_St,{update_role_name, RoleID, OldName, Name}) ->
    %% 缓存的nanm数据
    case erlang:get({?nanm,RoleID}) of
        #nanm{} = H ->
            %% 新的nanm缓存数据
            NewH = H#nanm{name=Name},
            set_nanm(RoleID, NewH),

            %% 排行榜数据
		    RankList = get_rankList( ),
		    case lists:keyfind(RoleID, #rank.roleID, RankList) of
		        false ->
		            ignore;
		        Info ->
		            NewRankList = lists:keyreplace(RoleID, #rank.roleID, RankList, Info#rank{name=Name}),
		            set_rankList(NewRankList)
		    end,

		    %% 当前战报
		    First10 = get_first10(),
		    case lists:keyfind(OldName,#p_nanm_info.roleName,First10) of
		        false ->
		            ignore;
		        First10Info ->
		            NewFirst10 = lists:keyreplace(OldName,#p_nanm_info.roleName,First10,First10Info#p_nanm_info{roleName=Name}),
		            set_first10(NewFirst10)
		    end,

		    %% 伤害列表
		    case get_harmList() of
		        List when is_list(List) ->
		            case lists:keyfind(OldName,#p_nanm_harm.name,List) of
		                false ->
		                    ignore;
		                HarmInfo ->
		                   NewHarmList = lists:keyreplace(OldName,#p_nanm_harm.name,List,HarmInfo#p_nanm_harm{name=Name}), 
		                   put(?harm,NewHarmList)
		                end;
		        _ ->
		            ignore
		    end;
        _ ->
            %% 当前玩家没有参加nanm
            ignore
    end;
do_handle_info(State,_St,test_change_to_next_state)->
	case get(?TimerRef) of
		?undefined->
			ignore;
		TimeRef ->
			erlang:cancel_timer(TimeRef)
	end,
	erlang:send(?MODULE,{change_status,State});

do_handle_info(_State,St,do_persit)->
	do_persit(St);

do_handle_info(_State,St,{get_reward,RoleID})->
	case catch reward_check(RoleID,St) of
		{false,Reason}->
			?unicast(RoleID,#sc_nanm_reward{result=Reason,rewardlist=[]});
		{true,_Result}->
			%%奖励协议返回交由玩家进程执行
			Msg = {nanm_boss_box_reward,St#st.lastBossGerTypeID,St#st.lastBossGerQuality},
			case catch role_lib:send_server(RoleID, Msg) of
				{'EXIT',_}->	
					ignore;
				_ ->
					update_role_rankinfo(RoleID,St)
			end
	end;

do_handle_info(_, St, Info) ->
	?ERR("do_handle_info function clause:request=~100p,state=~100p",[Info,St]).
    

%%定时持久化
do_persit(State)->
	db_sql:set_etc(?DB_ETC_KEY_WORLDBOSS,State).

%% 每秒同步
do_sync_tick(St, Tick, BossOldHp) ->
	#st{bossTotalHp=BossHp} = St,
	case St#st.state =:= ?STATE_FIGHT of
		true->
			sync_tick(Tick+1, BossHp);
		false->
			ignore
	end,
	%% 如果boss血量修改了，则广播
	if BossOldHp =/= BossHp ->
		do_hp_sync(BossHp);
	   true ->
		   ignore
	end,
	%% 广播伤害列表
	HarmList = clear_harmList(),
	if HarmList =:= [] ->
		   ignore;
	   true ->
		   do_refresh_bcList(),
		   bc(#sc_nanm_harm_broadcast{harmList=lists:sublist(HarmList, data_nanm:get(max_sync_harmCnt))})
		   %bc(#sc_nanm_harm_broadcast{harmList=HarmList})
	end,
	%% 3秒同步一次排行
	if (Tick rem 3) =:= 2 ->
		   do_rank_sync();
	   true ->
		   ignore
	end,
	if (Tick rem 5) =:= 4 ->
		   do_refresh_bcList();
	   true ->
		   ignore
	end.

%% 刷新已挂掉的网关进程
do_refresh_bcList() ->
	BcList = get_bcList(),
	BcList2 = [Pid||Pid<-BcList, is_process_alive(Pid)],
	set_bcList(BcList2).

do_hp_sync(BossHp) ->
	HpSyncRecord = #sc_nanm_hp_sync{bossHp=BossHp},
	bc(HpSyncRecord).

do_rank_sync() ->
	RankList = get_rankList(),
	do_rank_sync(RankList, 1).

do_rank_sync([#rank{roleID=RoleID}|RankList], Rank) ->
	case is_join(RoleID) of
		true ->
	?unicast_async(RoleID,#sc_nanm_rank_sync{curRank=Rank});
		false ->
			ignore
	end,
	do_rank_sync(RankList,Rank+1);
do_rank_sync([], _) ->
	ok.


%% 消息广播接口
bc(Record) ->
	RecordBin = proto:encode(Record),
	?INFO("bclist:~w ~n",[get_bcList()]),
	%lists:foreach(fun(GWPid) -> role_lib:send_client_force(GWPid, RecordBin) end, get_bcList()).
	%lists:foreach(fun(GWPid) -> role_lib:send_client2(GWPid, RecordBin) end, get_bcList()).
	lists:foreach(fun(GWPid) -> role_lib:send_client_async(GWPid, RecordBin) end, get_bcList()).

do_fight(RoleID, St, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList) ->
	#nanm{rebornTime=RebornTime} = RoleNanm = get_nanm(RoleID),
	NowSec = util:now(),
	%% 给予一秒的允许误差
	if NowSec+1 >= RebornTime ->
		   %% 援军助阵时也允许挑战BOSS
		   do_fight2(RoleID, St,FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList,RoleNanm, NowSec);
%% 		   case get_offlinePlayFlag(RoleID) of
%% 			   true ->
%% 				   ?unicast(RoleID, #sc_nanm_fight{fightInfo=[],rebornTime=0,result=5,rewardCoin=0,rewardReputation=0});
%% 			   _ ->
%% 				   do_fight2(RoleID, St,FighterList,LieuAdd, RoleNanm, NowSec)
%% 		   end;
	   true ->
		   ?unicast(RoleID, #sc_nanm_fight{fightInfo=[],rebornTime=0,result=2,rewardCoin=0,rewardReputation=0})
		   end.

do_fight2(RoleID, St, FighterList,LieuAdd,TalentList,TrSpecial,SkinInfo,LegendAddList,RoleNanm, NowSec) ->
	#st{boss=Boss, buffNum=BuffNum, bossBase=BossBaseInfo,bossTotalHp=BossTotalHp} = St,
	?INFO("boss:~w\n",[St]),
	
	%% 计算复活带来的buff加成
	BuffNum2 =
		case erlang:get({?rebronPlayer, RoleID}) of
			?undefined ->
				BuffNum;
			N ->
				erlang:erase({?rebronPlayer, RoleID}),
				BuffNum + N
		end,
	
	%% 计算援军助阵带来的buff加成
	BuffNum3 =
		case get_offlinePlayFlag(RoleID) of
			true ->
				BuffNum2 + data_nanm:get(add_reinforcements);
			_ ->
				BuffNum2
		end,

	%% 加擂鼓buff
	FightBoss = lists:foldl(fun(BossGer,BossAcc)->
									BossGer2 = ?changeBossHp(BossGer, erlang:trunc(BossTotalHp/length(Boss))+1), 
									[BossGer2|BossAcc]
							end, [], Boss),
	FighterList2 = cacl_buff(FighterList, BuffNum3),
	%% 发生战斗
	%?ERR("FightBoss:~w ~n",[FightBoss]),
    {_Result, FightRecord1, {_, NewBossState,_,_}} = role_fight:new(RoleID,FighterList2,FightBoss,LieuAdd,#add_attr{},TalentList,[],TrSpecial,#trSpecial{},false,SkinInfo,#skin_info{},LegendAddList,[]),
	%% 防止加生命的登场技能导致生命超出
	NewBossHp = lists:foldl(fun({_,Hp,_}, BossHpAcc)->
									 Hp + BossHpAcc
							 end,0, NewBossState),
	BossNewHp2 = erlang:min(NewBossHp,BossBaseInfo#bossBase.bossMaxHp),
	PActionList = FightRecord1#sc_fight_request.actionList,
	NewFightList = fix_boss_quality(FightRecord1#sc_fight_request.fighterList),
	FightRecord = FightRecord1#sc_fight_request{fighterList=NewFightList},
	Harm1 = -lists:foldl(fun(Action,Acc)->case Action#p_action.gerPos<0 of true-> Action#p_action.addHp+Acc;false->Acc end end,0,PActionList),
	Harm = erlang:min(Harm1,BossTotalHp),
	BossNewHp3 = erlang:min(BossTotalHp-Harm,BossBaseInfo#bossBase.bossMaxHp),
	%?ERR("NewBossState:~w ~n",[NewBossState]),
	%Boss2 = Boss#ger{gerHp=BossNewHp2},
	%?DEBUG("Harm:~w BossNewHp3:~w ~n",[Harm,BossNewHp3]),
	#nanm{name=Name} = RoleNanm,
	%?ERR("RoleNanm:~w harm:~w ~n",[RoleNanm,Harm]),
	if Harm > 0 ->
		   %% 广播伤害
		   log_harm(Name, Harm),
		   %% 广播同步boss血量
			%?INFO("boss Newhp:~w OldHp:~w boss hp_max:~w \n ",[BossNewHp2,BossTotalHp,BossBaseInfo#bossBase.bossMaxHp]),
		   % bc_hp(BossNewHp2, BossMaxHp),
		   %% 加声望、加银两、加累计伤害、重排名
		   {RoleNanm2, AddCoin, AddRepu} = add_nanm(RoleID, RoleNanm, Harm);
	   true ->
	   	   ?ERR("no harm BossTotalHp:~w BossNewHp2:~w NewBossHp:~w ~n",[BossTotalHp,BossNewHp2,NewBossHp]),
		   RoleNanm2 = RoleNanm,
		   AddCoin=0,
		   AddRepu=0
	end,
	%% 判断是否胜利
	if BossNewHp3 =< 0 ->
		   RebornTime = 0;
	   true ->
		   RebornTime = NowSec + data_nanm:get(recover_cooldown_time)
	end,
	RoleNanm3 = RoleNanm2#nanm{rebornTime=RebornTime},
%% 	?ERR("RoleNanm3:~w ~n",[RoleNanm3]),
	set_nanm(RoleID, RoleNanm3),
	Record = #sc_nanm_fight{fightInfo=[FightRecord],rebornTime=RebornTime,result=1,rewardCoin=AddCoin,rewardReputation=AddRepu},
	?unicast(RoleID, Record),
	{DialyTask,_RankTask} = get_current_activity_trigger_type(),
	?CATCH(role_task:send_dispach(RoleID,{dispach_task,DialyTask})),
	behavior_world_boss:log(RoleID, 1,Harm),
	%% 战斗结束的处理
	if BossNewHp3 =< 0 ->
		   do_win(RoleID, St, Name, RoleNanm3#nanm.harm);
	   true ->
		   St#st{bossTotalHp = BossNewHp3}
	end.

test(I,B)->
	calc_next_boss_state(I,B,win),
	?ERR("get info:~w,~w",[get_nanm(4000002),[<<"，品阶降低：10，">>]]),
	broadcast_server:bc_msgID(10017,["，品阶提升：30，"]).

calc_next_boss_state(Interval, BossQuality,Result)->
	Prompt = <<"，品阶提升：">>,
	Down  = <<"，品阶降低：">>,
	End = <<",">>,
	case Result of
		fail ->
			broadcast_server:bc_msgID(10015);
		_ ->
			ignore
	end,
	IncreaseQuality = get_boss_quality_increase(BossQuality,Interval,Result),
	if
	 	IncreaseQuality < 0 ->
	 		broadcast_server:bc_msgID(10015),
	 		Msg = binary_to_list(Down) ++  integer_to_list(-IncreaseQuality) ++ binary_to_list(End);
	 	IncreaseQuality =:= 0->
	 		Msg = binary_to_list(End);
	 	true ->
	 		Msg = binary_to_list(Prompt) ++ integer_to_list(IncreaseQuality) ++ binary_to_list(End)
	end,
	Msg1 = list_to_binary(Msg),
	broadcast_server:bc_msgID(10016,[Msg1]),
	BossQuality + IncreaseQuality.

final_bc(Type)->
	if Type =:= 1 ->
		   lists:foreach(fun(E)->
								 case E of
									 {{?joinFlag,Role},_}->
									 	?INFO("STOP :Role:~w ~n",[Role]),
										 #nanm{coinAcc=Coin,harm=Harm,repuAcc=Repu}=get_nanm(Role),
										 ?unicast(Role, #sc_nanm_stop{type=Type, roleSta=#p_role_stastic{harm=Harm, coin=Coin, repu=Repu}});
									 _ ->
										 ignore
								 end
						 end, erlang:get()),
		   	lists:foreach(fun({Key,Value}) ->
					case Key of
						?nanm->
							do_clear(Key, Value);
						_ ->
						  ignore
					end
				  end, erlang:get());
	   true ->
		   ignore
	end.
		   

do_win(RoleID, St, Name, KillerHarm) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=2}),
	%% 杀死boss的世界广播
	broadcast_server:bc_msgID(10017, [Name]),
	%% 广播参与活动的玩家活动结束
	%% 给参与玩家通告本局统计(伤害，银两，声望    的统计)

	% final_bc(1),%% 1 : win

	#st{bossBase=BossBaseInfo} = St,
        BossGerTypeID = BossBaseInfo#bossBase.bossGerTypeID,
	send_killer_reward(RoleID,BossGerTypeID),

	EndTime = util:now(),
	BeginTime = get(?begin_time),
	?INFO("Interval:~w ~w   ~n",[EndTime,BeginTime]),
	Interval = EndTime - BeginTime,
	%% 计算下次boss的等级
	NextBossQuality = calc_next_boss_state(Interval, BossBaseInfo#bossBase.bossQuality,win),
	%% 重新读boss的配置，并计算属性
	{Boss, NewBossBaseInfo} = init_boss(NextBossQuality,BossGerTypeID ),
	% {Boss, NewBossBaseInfo} = init_boss(BossBaseInfo#bossBase.bossQuality,BossBaseInfo#bossBase.bossGerTypeID),
	%BossMaxHp = Boss#ger.gerHp,
	
	%% 发送幸运奖励
	{LuckRoleList, LuckRoleIDList} = do_send_lucky_reward(BossGerTypeID),
	LuckRoleNameList = [E||#p_nanm_info{roleName=E}<-LuckRoleList],
	LastNanmInfoList = [#p_nanm_info{harmValue=KillerHarm,roleName=Name}|lists:sublist(get_first10(), ?RANK_REWARD_NUM)],
	LastInfo = #sc_nanm_last_info_win{bossQuality=BossBaseInfo#bossBase.bossQuality,
									  bossMaxHp=BossBaseInfo#bossBase.bossMaxHp,
									  nanmInfolist=LastNanmInfoList,
									  intervalSec=Interval,
									  luckyRoleList=LuckRoleNameList,
									  gerTypeID=BossBaseInfo#bossBase.bossGerTypeID
									  },
	set_last_info(BeginTime, LastInfo),
	DNanm = #d_nanm{bossQuality=NextBossQuality,failTimesAcc=0,lastBeginTime=BeginTime,lastInfo=LastInfo},
	set_d_nanm(DNanm),
	
	%% 给排名前10的玩家发奖励
	First10 = lists:sublist(get_rankList(),?RANK_REWARD_NUM),
    First10RoleIDList = [E||#rank{roleID=E}<-First10],
	lists:foldl(fun(E, Acc) ->
						#sell_reward{} = Reward = get_rank_reward(Acc,BossGerTypeID),
						mail_server:send_sys_mail(E, ?MAIL_NANM_RANK_FIRST10, [Acc], "", Reward),
						Acc+1
				end	, 1, First10RoleIDList),
    
    %% 发送参与奖
    JoinReward = data_nanm:get(reward_join),
    RewardedRoleIDList = [RoleID] ++ First10RoleIDList ++ LuckRoleIDList,
    lists:foreach(fun(#rank{roleID=E}) ->
                          case lists:member(E, RewardedRoleIDList) of
                              false ->
                                  mail_server:send_sys_mail(E, ?MAIL_NANM_RANK_JOIN, [], "", JoinReward);
                              true ->
                                  next
                          end
                  end, get_rankList()),
	%% 活动结束的相关处理
	do_finish_nanm(),
	%% 新的虎牢关状态
	set_buff2etc(0),
	%% 此处不更新boss等级
	St2 = St#st{boss=Boss,bossBase=NewBossBaseInfo,buffNum=0,bossTotalHp = NewBossBaseInfo#bossBase.bossMaxHp,lastresult=?KILL_BOSS_SUCCESS,lastBossGerTypeID=BossBaseInfo#bossBase.bossGerTypeID,lastBossGerQuality=BossBaseInfo#bossBase.bossQuality},
	%%此处由于提前结束了战斗阶段，所以应该把旧的Timer给取消掉，防止出现多个Timer
	erlang:cancel_timer(get(?TimerRef)),
	erlang:send(?MODULE,{change_status,St#st.state}),
	% St2 = St#st{buffNum=0,state=?STATE_REWARD,bossTotalHp=BossBaseInfo#bossBase.bossMaxHp},
	?INFO("do_win  St:~w ~n",[St2]),
	St2.

do_fail(St) ->
	broadcast_server:bc(#sc_push_highlight_Info{value=2, type=2}),
	%% 世界广播吕布苟幸活下来了
	broadcast_server:bc_msgID(10018),
	%% 广播参与活动的玩家活动结束
	bc(#sc_nanm_stop{type=2,roleSta=#p_role_stastic{harm=0, coin=0, repu=0}}),
	
	#d_nanm{failTimesAcc=FailTimesAcc} = get_d_nanm(),
	#st{bossBase=BossBaseInfo} = St,
	% LevelDownTimes = data_nanm:get(boss_leveldown_times),
	FailTimesAcc2 = FailTimesAcc+1,

	EndTime = util:now(),
	BeginTime = get(?begin_time),
	Interval = EndTime - BeginTime,
	%% 计算下次boss的等级
	NextBossQuality = calc_next_boss_state(Interval, BossBaseInfo#bossBase.bossQuality,fail),
	% case FailTimesAcc2 >= LevelDownTimes of
	% 	true ->
	% 		%% 广播boss降级了
	% 		broadcast_server:bc_msgID(10015),
	% 		if BossBaseInfo#bossBase.bossQuality > 10 ->
	% 				NextBossQuality=BossBaseInfo#bossBase.bossQuality-10;
	% 			true ->
	% 				NextBossQuality=BossBaseInfo#bossBase.bossQuality
	% 		end;
	% 	false ->
	% 		NextBossQuality=BossBaseInfo#bossBase.bossQuality
	% end,
	%% 发送幸运奖励
	%%do_send_lucky_reward(),
	
	{Boss, NewBossBaseInfo} = init_boss(NextBossQuality,BossBaseInfo#bossBase.bossGerTypeID),
	%BossMaxHp = Boss#ger.gerHp,
	BeginTime = get(?begin_time),
	LastInfo = #sc_nanm_last_info_fail{bossQuality=BossBaseInfo#bossBase.bossQuality,intervalSec=data_nanm:get(fight_time_interval),gerTypeID=St#st.bossBase#bossBase.bossGerTypeID},
	set_last_info(BeginTime, LastInfo),
	%% 持久化下次虎牢关的用到的信息
	DNanm2 = #d_nanm{bossQuality=NextBossQuality,failTimesAcc=FailTimesAcc2,lastBeginTime=BeginTime,lastInfo=LastInfo},
	set_d_nanm(DNanm2),
	%% 活动结束的相关处理
	do_finish_nanm(),
	%% 新的虎牢关状态
	set_buff2etc(0),
	St2 = St#st{bossBase=NewBossBaseInfo,boss=Boss,state=?STATE_REWARD,buffNum=0,bossTotalHp = NewBossBaseInfo#bossBase.bossMaxHp,lastresult=?KILL_BOSS_FAIL,lastBossGerTypeID=BossBaseInfo#bossBase.bossGerTypeID,lastBossGerQuality=BossBaseInfo#bossBase.bossQuality},
	?INFO("do_fail:  St:~w ~n",[St2]),
	St2.



%% 活动结束的处理
do_finish_nanm() ->
	%% 进行一次排名同步
	do_rank_sync(),

	%% 给已付过钱的离线参与玩家发奖励
	PayedList = get_offlinePlayList(),
	#sell_reward{} = OfflinePayReward = data_nanm:get(offline_play_reward),
	lists:foreach(fun(RoleID) -> mail_server:send_sys_mail(RoleID, ?MAIL_NANM_OFFLINE, [], "", OfflinePayReward) end, PayedList),
    {_DialyTask,RankTask} = get_current_activity_trigger_type(),
    %%触发同步任务
    lists:foldl(fun(#rank{roleID=RoleID},Rank) ->
                    case role_lib:is_online(RoleID) of
                        true ->
                            ?CATCH(role_task:send_dispach(RoleID,{dispach_task,RankTask,Rank}));
                        _ ->
                            role_task_trigger:offline_nanm_rank(RoleID, Rank)
                        end,
                    Rank + 1
                end, 1, get_rankList( )),
    ?INFO("RankList:~w ~n",[get_rankList()]),
	% clear_info(),
	persist_offlinePlayList().


%% 小概率重复的情况
random_seq(Scale, Num) when Scale >= 3*Num ->
	random_seq(Scale, Num, []);
random_seq(Scale, Num) ->
	util:random_list(lists:seq(1,Scale), Num).

random_seq(_, 0, List) ->
	List;
random_seq(Scale, Num, List) ->
	R = random:uniform(Scale),
	case lists:member(R, List) of
		true ->
			random_seq(Scale, Num, List);
		false ->
			random_seq(Scale, Num-1, [R|List])
	end.

%% 活动结束时的幸运奖励
do_send_lucky_reward(BossGerTypeID) ->
	RankList = get_rankList(),
	LUCK_REWARD_NUM = trunc( length(RankList) * data_nanm:get(reward_luck) / 100 ),
	Num = length(RankList) - ?RANK_REWARD_NUM,
	if Num =< 0 ->
		   RL=[];
	   Num =< LUCK_REWARD_NUM ->
		   RL = lists:sublist(RankList, ?RANK_REWARD_NUM + 1, ?RANK_REWARD_NUM + LUCK_REWARD_NUM);
	   true ->
		   SL = random_seq(Num, LUCK_REWARD_NUM),
		   RL = [lists:nth(?RANK_REWARD_NUM+E, RankList) || E<- SL]
	end,
	#sell_reward{} = LuckReward = data_nanm:get(reward_luckers),
    LuckRoleIDList = [E||#rank{roleID=E}<-RL],
	lists:foreach(fun(E) -> mail_server:send_sys_mail(E, ?MAIL_NANM_LUCKY, [], "", LuckReward) end, LuckRoleIDList),
	{[#p_nanm_info{harmValue=HV,roleName=RN} || #rank{harm=HV,name=RN} <- RL],
     LuckRoleIDList}.
				 
	
get_rank_reward(X,BossGerTypeID) ->
    #sell_reward{item=I}=SellReward = data_nanm:get(erlang:list_to_atom(lists:concat([reward_rank,X]))),
    AddItemTypeID = data_nanm:get({boss_reward,BossGerTypeID}),
    SellReward#sell_reward{item=[#new_item{itemTypeID=AddItemTypeID,itemNum=1,itemLevel=1,itemRank=0}|I]}.

%% 获取虎牢关持久化信息
get_d_nanm() ->
	case db_sql:get_etc(?DB_ETC_KEY_NANM) of
		#d_nanm{}=D_Nanm ->
			%%由于在sc_nanm_last_info_fail中添加了上回的精灵ID，所以此处需要修改
			fix_old_Nanm(D_Nanm);
		[] ->
			BossQuality = data_nanm:get(boss_init_quality),
			#d_nanm{bossQuality=BossQuality,
					failTimesAcc=0,
					lastInfo=#sc_nanm_last_info_fail{bossQuality=BossQuality,intervalSec=-1,gerTypeID=10001},
					lastBeginTime=0}
	end.

set_d_nanm(D_Nanm) ->
	db_sql:set_etc(?DB_ETC_KEY_NANM, D_Nanm).


%% 擂鼓buff
get_buff() ->
	case db_sql:get_etc(?DB_ETC_KEY_NANM_BUFF) of
		BuffNum when is_integer(BuffNum) ->
			BuffNum;
		_ ->
			0
	end.
 
set_buff2etc(BuffNum) ->
	db_sql:set_etc(?DB_ETC_KEY_NANM_BUFF, BuffNum).

%% 玩家本次活动信息,此处去掉对伤害累积的清楚，由于stop协议发送延后，需要在发了stop协议之后再清楚
do_clear({?nanm,_}=Key, _) ->
	erlang:erase(Key);
do_clear({?offlinePlayFlag,_}=Key,_) ->
	erlang:erase(Key);
do_clear({?rebronPlayer,_}=Key,_) ->
	erlang:erase(Key);
do_clear(?rankList=Key,_) ->
	erlang:erase(Key);
do_clear(?first10=Key,_) ->
	erlang:erase(Key);
do_clear(_,_) ->
	ignore.

%% 活动结束后，清理活动信息
clear_info() ->
	% 清理进程字典
	lists:foreach(fun({Key,Value}) ->
					case Key of
						?rankList->
							ignore;
						_ ->
						  do_clear(Key, Value)
					end
				  end, erlang:get()).
							  


send_killer_reward(RoleID,BossGerTypeID) ->
	#sell_reward{item=I} = Reward = data_nanm:get(reward_killer),
        AddItemTypeID = data_nanm:get({boss_reward,BossGerTypeID}),
    Reward2 = Reward#sell_reward{item=[#new_item{itemTypeID=AddItemTypeID,itemNum=1,itemLevel=1,itemRank=0}|I]},
	mail_server:send_sys_mail(RoleID, ?MAIL_NANM_KILL, [], "", Reward2).

add_nanm(RoleID, RoleNanm, Harm) ->
	#nanm{coinAcc=CoinAcc,harm=HarmAcc,repuAcc=RepuAcc,name=Name} = RoleNanm,
	{CoinRatio, CoinMax, RepuRatio, RepuMax, RepuMaxAcc} = data_nanm:get(fight_reward_arg),
	AddCoin = erlang:min(CoinMax, trunc(Harm*CoinRatio)),
	if RepuAcc >= RepuMaxAcc ->
		   AddRepu = 0;
	   true ->
	AddRepu0 = erlang:min(RepuMax, trunc(Harm*RepuRatio)),
	AddRepu = erlang:min(RepuMaxAcc-RepuAcc, AddRepu0)
	end,
	catch role_lib:send_server(RoleID, {nanm_harm_reward,AddCoin,AddRepu}),
	CoinAcc2 = CoinAcc+AddCoin,
	RepuAcc2 = RepuAcc+AddRepu,
	HarmAcc2 = HarmAcc+Harm,
	Rank2 = insert_rank(RoleID, HarmAcc2, Name, HarmAcc),
	RoleNanm2 = RoleNanm#nanm{coinAcc=CoinAcc2,harm=HarmAcc2,rank=Rank2,repuAcc=RepuAcc2},
	{RoleNanm2, AddCoin, AddRepu}.
				  

get_rankList() ->
	case erlang:get(?rankList) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

set_rankList(List) ->
	erlang:put(?rankList, List).

get_first10() ->
	case erlang:get(?first10) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.


set_first10(First10) ->
	erlang:put(?first10,First10).
			
insert_rank(RoleID, Harm, Name, HarmAcc) ->
	?INFO("test~n"),
	Ranker = #rank{roleID=RoleID,harm=Harm,name=Name},
	RankList = get_rankList(),
	?INFO("oldRankList:~w ~n",[RankList]),
	if HarmAcc =:= 0 ->
		   % 新的记录
		   {NewRank, NewRankList} = insert_rank2(Harm, Ranker,RankList,[],0);
	   true ->
		   % 排行榜中已有该玩家的信息
		   {NewRank, NewRankList} = insert_rank3(Harm, Ranker,RankList,[],0)
	end,
	?INFO("NewRankList:~w ~n",[NewRankList]),
	set_rankList(NewRankList),
	if NewRank =< 10 ->
		   set_first10(
			 [#p_nanm_info{harmValue=Harm1,roleName=Name1}
						  ||#rank{harm=Harm1,name=Name1}<-lists:sublist(NewRankList,1,10)]
					  );
	   true ->
		   ignore
	end,
	?INFO("NewRankList:~w ~n",[NewRank]),
	NewRank.

%% 新的记录的插入
insert_rank2(Harm, Ranker, [#rank{harm=Harm2}=H|RankList]=E,Tail,N) ->
	if Harm2 >= Harm ->
	insert_rank2(Harm, Ranker, RankList, [H|Tail],N+1);
	   Harm2 < Harm ->
		   NewRankList = lists:reverse(Tail, [Ranker|E]),
		   {N + 1, NewRankList}
	end;	   
insert_rank2(_Harm, Ranker, [],Tail, N) ->
	NewRankList = lists:reverse([Ranker|Tail]),
	{N+1,NewRankList}.

%% 已存在旧的信息在排行榜
insert_rank3(Harm, Ranker, [#rank{harm=Harm2}=H|RankList]=E,Tail,N) ->
	if Harm2 >= Harm ->
	insert_rank3(Harm, Ranker, RankList, [H|Tail],N+1);
	   Harm2 < Harm ->
		   %% 删除旧的信息
		   NewRankList = lists:reverse(Tail,[Ranker|lists:keydelete(Ranker#rank.roleID, #rank.roleID, E)]),		   
		   {N + 1,NewRankList}
	end;	   
insert_rank3(_Harm, Ranker, [],Tail, N) ->
	NewRankList = lists:reverse([Ranker|Tail]),	
	{N+1,NewRankList}.

	
	
cacl_buff(FighterList, 0) ->
	FighterList;
cacl_buff(FighterList, BuffNum) ->	
	[begin 
		 GerAttack2 = trunc((GerAttr#gerAttr.gerAttack)*(1+BuffNum/100)),
		 Ger#ger{gerAttr=GerAttr#gerAttr{gerAttack=GerAttack2}}
	 end || 
	 #ger{gerAttr=GerAttr}=Ger<-FighterList].

log_harm(Name, Harm) ->
	put(?harm, [#p_nanm_harm{harm=Harm,name=Name}|get_harmList()]).

clear_harmList() ->
	case erlang:erase(?harm) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

get_harmList() ->
	case get(?harm) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.


%% 仅当血量小于某个值时，才进行即时同步，大于这个值时，走每秒的定时同步
bc_hp(BossNewHp, BossMaxHp) ->
	if (10*BossNewHp) < BossMaxHp ->
		   bc(#sc_nanm_hp_sync{bossHp=BossNewHp});
	   true ->
		   ignore
	end.


						
%% 预处理配置
%% 增加配置项，interval_sec
data_nanm_transform_list(List) ->
	{start_time,ST} = lists:keyfind(start_time,1,List),
	{stop_time, PT} = lists:keyfind(stop_time, 1,List),
	IntervalSec = time_diff(ST, PT),
	[{interval_sec, IntervalSec}|List].

%%产生随机bossTypeID
generate_boss_typeID(BandGerTypeID)->
	BossList = data_nanm:get(bosslist),
	NewBossList = [E||E={_,GerTypeID}<-BossList,GerTypeID=/=BandGerTypeID],
	case NewBossList of
		[]->
			util:random_one_from_weigh_list(BossList);
		_->
			util:random_one_from_weigh_list(NewBossList)
	end.

fix_old_Nanm(D_Nanm) when is_record(D_Nanm,d_nanm)->
	LastInfo = D_Nanm#d_nanm.lastInfo,
	case is_record(LastInfo,sc_nanm_last_info_fail) orelse is_record(LastInfo,sc_nanm_last_info_win) of
		true->
			D_Nanm;
		false ->
			OldList = tuple_to_list(LastInfo),
			NewList = OldList ++ [generate_boss_typeID(0)],
			NewInfo = list_to_tuple(NewList),
			D_Nanm#d_nanm{lastInfo=NewInfo}
	end;
fix_old_Nanm(_D_Nanm)->
	#d_nanm{}.
% {fight_start_time,[{12,30,0},{20,0,0},{17,55,0},{18,5,0},{18,15,0},{18,25,0}]}.
%%此处返回的时间是按照配置的时间间隔计算的下一次状态切换时间，除了跨天的准备时间是绝对时间
get_next_state_and_time(State)->
	StartTime = data_nanm:get(fight_start_time),
	StartTimeStamp = get_early_starttime(StartTime),
	PrepareInterval = data_nanm:get(prepare_time_interval),
	PrepareTime1 = StartTimeStamp-PrepareInterval,
	%%此处有可能出现PrepareTime已经是过去时间的情况
	% ?ERR("StartTimeStamp:~w PrepareTime1:~w ~n",[util:seconds_to_datetime(StartTimeStamp),util:seconds_to_datetime(PrepareTime1)]),
	PrepareTime = case PrepareTime1 > util:now() of
		true->
			PrepareTime1;
		false->
			util:now()+5
	end,
	FightInterval = data_nanm:get(fight_time_interval),
	RewardInterval = data_nanm:get(reward_time_interval),
	NowSec = util:now(),
	case State of
		?STATE_PREPARE->
			{?STATE_FIGHT,NowSec+FightInterval};
		?STATE_FIGHT->
			{?STATE_REWARD,NowSec+RewardInterval};
		?STATE_REWARD->
			{?STATE_END,PrepareTime};
		?STATE_END->
			{?STATE_PREPARE,StartTimeStamp};
		_ ->
			{?STATE_END,PrepareTime}
	end.

test_change_to_next_state()->
	erlang:send(?MODULE,test_change_to_next_state).

%%返回距离下个BOSS开启最近的时间点
get_early_starttime(StartTime)->
	NowSec = util:now(),
	TimeStampList = lists:foldl(fun(Time,Acc)->
		Temp = util:datetime_to_seconds({erlang:date(),Time}),
		case Temp > NowSec of
			true->
				[Temp|Acc];
			false->
				[Temp+?ONE_DAY_SECONDS|Acc]
		end
	end,[],StartTime),
	lists:min(TimeStampList).

get_reward(RoleID)->
	erlang:send(?MODULE,{get_reward,RoleID}).

reward_check(RoleID,State)->
	case State#st.state =:= ?STATE_REWARD of
		true->
			next;
		false->
			erlang:throw({false,2})
	end,
	case lists:member(RoleID,State#st.get_reward_list) of
		true->
			erlang:throw({false,5});
		false->
			next
	end,
	case State#st.lastresult =:= ?KILL_BOSS_SUCCESS of
		false->
			erlang:throw({false,4});
		true->
			next
	end,
	{true,1}.

check_reward(RoleID,St)->
	case lists:member(RoleID,St#st.get_reward_list) of
		true->
			?REWARD_HAS_GET;
		false->
			case St#st.lastresult =/= ?KILL_BOSS_SUCCESS of
				true->
					?REWARD_BOSS_NOT_KILL;
				false->
					?REWARD_NO_GET
			end
	end.

update_role_rankinfo(RoleID,St)->
	OldList = St#st.get_reward_list,
	case lists:member(RoleID,OldList) of
		true->
			?ERR("更新领奖列表，出现玩家已经加入领奖列表的情况~n"),
			ignore;
		false->
			St#st{get_reward_list=[RoleID|OldList]}
	end.
% {fight_start_time,[{12,30,0},{20,0,0},{11,56,0}]}.
%%判断当前的世界BOSS活动是要发送超梦还是风王的任务完成消息，根据接近时间判断
get_current_activity_trigger_type()->
	StartTImeList = data_nanm:get(fight_start_time),
	{Time1,Time2}=case length(StartTImeList) >=2 of
		true->
			[TempTime1,TempTime2|_Other]=StartTImeList,
			{TempTime1,TempTime2};
		false->
			{{12,30,0},{20,0,0}}
	end,
	TimeStamp1 = util:datetime_to_seconds({erlang:date(),Time1}),
	TimeStamp2 = util:datetime_to_seconds({erlang:date(),Time2}),
	Interval1 = erlang:abs(util:now()-TimeStamp1),
	Interval2 = erlang:abs(util:now()-TimeStamp2),
	case Interval1 < Interval2 of
		true->
			{role_join_nanm,role_nanm_rank};
		false->
			{role_join_hula,role_hula_rank}
	end.

%%根据BOSS gerTypeID以及Quality生成对应的奖励列表
get_box_reward(BossID,BossQuality)->
	case data_nanm:get({data_reward,get_reward_field(BossQuality)}) of
		?undefined->
			[];
		{MegaNum,BoxID}->
			BoxReward = util:random_one_from_weigh_list(data_nanm:get({data_reward_box,BoxID})),
			MegaReward = case data_nanm:get({boss_reward,BossID}) of
				?undefined->
					case MegaNum =:= 0 of
						true->
							[];
						false->
							[{6,7805,MegaNum}]
					end;
				MegaClipID->
					case MegaNum=:=0 of
						true->
							[];
						false->
							[{6,MegaClipID,MegaNum}]
					end
			end,
			CoinReward = 
			case BossQuality > 0 of
				true->
					[{1,BossQuality*data_nanm:get(boss_reward_coin)}];
				false->
					[]
			end,
			BoxReward++MegaReward++CoinReward
	end.

get_reward_field(BossQuality)->
	case data_nanm:get(data_reward_field) of
		?undefined->
			1;
		FieldList->
			get_reward_field2(FieldList,BossQuality)
	end.

get_reward_field2([],_BossQuality)->
	1;
get_reward_field2([{{Begin,End},Field}|T],BossQuality)->
	case BossQuality>=Begin andalso BossQuality=<End of
		true->
			Field;
		false->
			get_reward_field2(T,BossQuality)
	end.


get_boss_quality_increase(OldQuality,Interval,Result)->
	case Result of
		win->
			case data_nanm:get(data_boss_quality_up_rule) of
				?undefined->
					?ERR("can not find data_boss_quality_up_rule~n"),
					10;
				RuleList->
					FindList = lists:foldl(fun({Begin,End,_Increase} = E,Acc)->
						case Interval >= Begin andalso Interval < End of
							true->
								[E|Acc];
							false->
								Acc
						end
					end,[],RuleList),
					case FindList of
						[]->
							?ERR("未发现符合规则的增长项:~w Interval:~w ~n",[FindList,Interval]),
							OldQuality + 10;
						_ ->
							{_Begin,_End,Increase} = hd(FindList),
							NewQuality = OldQuality + Increase,
							case NewQuality < 0 of
								true->
									0;
								false->
									Increase
							end
					end
			end;
		fail->
			case OldQuality >= 3 of
				true->
					DeductQuality = data_nanm:get(data_deduct_quality),
					NewQuality = OldQuality + DeductQuality,
					case NewQuality >= 3 of
						true->
							DeductQuality;
						false->
							3-OldQuality
					end;
				false->
					0
			end
	end.

get_boss_awake_skill(GerTypeID,GerQuality)->
	AwakeBossList = data_nanm:get(awake_boss_list),
	case lists:member(GerTypeID,AwakeBossList) of
		false->
			?undefined;
		true->
			generate_boss_skillID(GerTypeID,GerQuality)
	end.

generate_boss_skillID(GerTypeID,GerQuality)->
	case generate_boss_skillIDList(GerTypeID,GerQuality) of
		?undefined->
			?undefined;
		SkillIDList1->
			SkillIDList = lists:foldl(fun(SkillID,Acc)->
				case data_awake:get({data_awake_skill,SkillID}) of
					?undefined->
						?ERR("undefined skillID:~w ~n",[SkillID]),
						Acc;
					_ ->
						[SkillID|Acc]
				end
			end,[],SkillIDList1),
			case SkillIDList =/=[] of
				true->
					lists:nth(random:uniform(length(SkillIDList)),SkillIDList);
				false->
					?undefined
			end
	end.

generate_boss_skillIDList(_GerTypeID,GerQuality)->
	Quality_Skill_Map = data_nanm:get(data_quality_awakeskill_map),
	SkillIDList1 = lists:foldl(fun({{Begin,End},SkillIDListElem},Acc)->
		case GerQuality >=Begin andalso GerQuality =<End of
			true->
				[SkillIDListElem|Acc];
			false->
				Acc
		end
	end,[],Quality_Skill_Map),
	if
	 	length(SkillIDList1) >1 ->
			?ERR("Quality：~w have more than one suit config:~w ~n",[GerQuality,SkillIDList1]);
			hd(SkillIDList1);
		length(SkillIDList1) =:= 1 ->
			hd(SkillIDList1);
		true->
			?undefined
	end.

generate_boss_awakeinfo(GerTypeID,GerQuality)->
	case get_boss_awake_skill(GerTypeID,GerQuality) of
		?undefined->
			[];
		SkillID->
			{Step,ID,Quality} = data_awake:get({data_awake_skill,SkillID}),
			[#awake{step=Step,skillID=ID,skill_quality=Quality}]
	end.

fix_boss_quality(FighterList)->
	BossList = data_nanm:get(bosslist),
	lists:foldl(fun(Fighter,Acc)->
		case lists:keyfind(Fighter#p_fighter.gerTypeID,2,BossList) of
			false->
				[Fighter|Acc];
			_->
				NewQuality = boss_quality_map(Fighter#p_fighter.gerQuality),
				[Fighter#p_fighter{gerQuality=NewQuality}|Acc]
		end
	end,[],FighterList).

boss_quality_map(GerQuality)->
	case data_nanm:get(boss_quality_map) of
		?undefined->
			GerQuality;
		List->
			NewQualityList = lists:foldl(fun({{BeginQuality,EndQuality},MapQuality},Acc)->
				case BeginQuality =<GerQuality andalso GerQuality =<EndQuality of
					true->
						[MapQuality|Acc];
					false->
						Acc
				end
			end,[],List),
			case NewQualityList =:= [] of
				true->
					GerQuality;
				false->
					hd(NewQualityList)
			end
	end.
