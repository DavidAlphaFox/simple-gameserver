%% @author lixinglong
%% @doc @todo 联盟跨服战模块

-module(family_fight_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(sync_signup_tick,300).
-define(msg_timeout_interval, 120).
-define(firstN_list,firstN_list).
-define(state_sign, state_sign).
-define(state_fight, state_fight).
-define(state_reward, state_reward).
-define(ets_cache_fight_state,ets_cache_fight_state). % 缓存工会战阶段状态，family_fight_server唯一写，其他进程读，减少进程间消息，避免call调用

% bc_new_period_begin、get_family_fight_dtl_back、change_state_info这三个消息会更新prepareEndTime等时间界限
-record(state, {period=0,signList=[],startTime=0,prepareEndTime=0,randomEndTime=0,fightEndTime=0,periodEndTime=0}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


i() ->
	gen_server:call(?MODULE, i).

request_master_status()->
	erlang:send(?MODULE, request_master_status).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	process_flag(trap_exit,true),
	case data_setting:get(server_type) == familyfight of
		true ->
			?ERR("family_fight_server not started,Reason:family_fight_maser_node"),
			ignore;
		_ ->
			connect_master(),
			sync_sign_tick(),
			timer_wheel:init(),
			{ok,#state{}}
	end.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
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
%% ====================================================================
handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
	{reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(Info, State) when is_binary(Info)->
%	?ERR("~w",[erlang:binary_to_term(Info)]),
    case catch do_handle_info(erlang:binary_to_term(Info), State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end;
handle_info(Info, State) ->
%	?ERR("~w",[Info]),
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, #state{signList=SignList,prepareEndTime=PT})->
	case util:now() < PT of
		true ->
			ServerID = data_setting:get(server_id),
			send_msg_to_master_server({update_sign_info, ServerID,SignList});
		_ ->
			ignore
	end,
	ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

do_handle_info(request_master_status, State)->
	connect_master(),
	{noreply,State};

do_handle_info({client_msg, RoleID, #cs_familyfight_rankerList{}},State) ->
	send_role_rankerList(RoleID),
	{noreply,State};

do_handle_info({cs_familyfight_info,RoleID,FamilyID,TotalScore,WarPeriod
			   ,WinStar, MatchWinStar,FightResult,AttackTimes,DefendTimes,WorldRank,FamilyFighterGroupLength}, State) ->
	#state{
		   period=Period
		  ,signList=SignList
		  ,startTime=StartTime
		  ,prepareEndTime=PrepareEndTime
		  ,randomEndTime=RandomEndTime
		  ,fightEndTime=FightEndTime
		  ,periodEndTime=PeriodEndTime}=State,
    ?INFO("L-family_fight_server ====  ~w ===",[{cs_familyfight_info,RoleID,FamilyID,TotalScore,WarPeriod
               ,WinStar, MatchWinStar,FightResult,AttackTimes,DefendTimes,WorldRank}]),
	IsSign = case lists:keyfind(FamilyID,#pf_fighter.familyID, SignList) of
				 false ->
					 2;
				 _ ->
					 1
			 end,
    %% 加入 WarPeriod =:= 0 是为了防止公会在报名期间创建，跳过了工会战全服初始化步骤，未同步Period数据，导致此处逻辑错误。
	{WinStar2,AttackTimes2,DefendTimes2} = 
	case WarPeriod =:= 0 orelse WarPeriod =:= Period of
		true ->
			{WinStar-MatchWinStar,AttackTimes, DefendTimes};
		_ ->
			{0,0,0}
	end,
	{SignNeedNumMin,_SignNeedNumMax} = get_sign_num_limit(),
	Now=util:now(),
	StateInfo = if Now > FightEndTime ->
					   3;
				   Now > PrepareEndTime ->
					   2;
				   true ->
					   1
				end,
	Result = 
		case util:now() < StartTime of
			true ->
				3;
			false ->
				case Period of
					0 ->
						4;
					_ ->
						1
				end 
		end,
	WinStar3 = 
		if WinStar2 > 0 ->
			   data_family_fight:get(winScore)*WinStar2;
		   WinStar2 == 0 ->
			   data_family_fight:get(eqScore)*WinStar2;
		   WinStar2 < 0 ->
			   data_family_fight:get(loseScore)*WinStar2
		end,
    {SignNeedNumMin,_SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
    %% TODO 确认signNeedNum、attackTimes、defendTimes在客户端是什么显示意义的
	?unicast(RoleID,#sc_familyfight_info{result=Result,info=[#p_familyfight_info_dtl{state=StateInfo,isSign=IsSign,signNeedNum=SignNeedNumMin
										 ,period=Period,startTime=StartTime,prepareEndTime=PrepareEndTime - 2
										 ,fightEndTime=FightEndTime - 2,periodEndTime=PeriodEndTime - 2,randomEndTime=RandomEndTime - 2
										 ,scoreCount=TotalScore,getStars=WinStar2,attackTimes=get_max_attackTimes() * FamilyFighterGroupLength
										 ,attackTimesUsed=AttackTimes2,defendTimes=get_max_defendTimes() * FamilyFighterGroupLength
										 ,defendTimesUsed=DefendTimes2,worldRank=WorldRank,winScore=WinStar3  
										 ,eqScore=0,loseScore=0}]}),
    {noreply, State};

do_handle_info({cs_familyfight_sign, RoleID,FamilyID,Score,FamilyName,TotalFightPower,OwnerName,Level}, #state{signList=SignList,startTime=StartTime,prepareEndTime=PrepareEndTime}=State) ->
	Now=util:now(),
	case Now > StartTime andalso Now < PrepareEndTime of
		true ->
			case lists:keyfind(FamilyID,#pf_fighter.familyID, SignList) of
				false ->
					%SignList2=[FamilyID|SignList],
					SignList2 = [#pf_fighter{familyID=FamilyID,serverID=util:calc_server_id(familyID,FamilyID),score=Score
											,familyName=FamilyName,totalFightPower=TotalFightPower,ownerName=OwnerName,level=Level}|SignList],
                    ?INFO("所有当前报名的公会 ~w",[SignList2]),
					family_misc:router_to_family_process(FamilyID, {familyfight_sign_succ,RoleID}),
					?unicast(RoleID, #sc_familyfight_sign{result=1});
				_ ->
					SignList2=SignList,
					?unicast(RoleID,#sc_familyfight_sign{result=4})
			end;
		_ ->
			SignList2=SignList,
			?unicast(RoleID,#sc_familyfight_sign{result=7})
	end,
	{noreply, State#state{signList=SignList2}};

do_handle_info(sync_signup_info, #state{signList=SignList,prepareEndTime=PT}=State)->
	case util:now() < (PT + ?sync_signup_tick) of
		true ->
			ServerID = data_setting:get(server_id),
			send_msg_to_master_server({update_sign_info, ServerID,SignList});
		_ ->
			ignore
	end,
	sync_sign_tick(),
	{noreply, State};

%% 更新state并通知family进程
do_handle_info({sync_period_result, #pf_fighter{familyID=FamilyID}=Fighter}, #state{signList=SignList}=State)->
	SignList2 = 
	case lists:keytake(FamilyID,#pf_fighter.familyID, SignList) of
		false ->
			?ERR("sync error fighter:~w",[Fighter]),
			[Fighter|SignList];
		{value, _, OtherFighter}->
			[Fighter|OtherFighter]
	end,
	case db_sql:if_family_exist(FamilyID) of
		false ->
			?ERR("get err result info:~w",[Fighter]);
		true ->
			family_misc:router_to_family_process(FamilyID, {sync_period_result, Fighter})			
	end,
	{noreply, State#state{signList=SignList2}};

do_handle_info({cs_familyfight_fighter_info, MatchServerID,MatchFamilyID, RoleID,FamilyID}, #state{randomEndTime=RandomEndTime} = State)->
	case util:now() > RandomEndTime of
		true ->
            case MatchServerID =:= 0 of
                true ->
			        ?unicast(RoleID,#sc_familyfight_fighter_info{result=5, selfFamily=[],otherFamily=[]});
                _ ->
			        ServerID = data_setting:get(server_id),
			        send_msg_to_server(MatchServerID,{family_fighter_info_get, MatchServerID,MatchFamilyID,RoleID,FamilyID,ServerID}) 
            end;
		_ ->
			?unicast(RoleID,#sc_familyfight_fighter_info{result=4, selfFamily=[],otherFamily=[]})
	end,
	{noreply, State};

do_handle_info({family_fighter_info_get, ServerID,FamilyID, GetRoleID,GetFamilyID,GetServerID},State)->
	case db_sql:if_family_exist(FamilyID) of
		false ->
			send_msg_to_server(GetServerID,{family_fighter_info_get_back, false, ignore,ServerID,FamilyID,GetRoleID, GetFamilyID});
		true ->
			family_misc:router_to_family_process(FamilyID, {family_fighter_info_get, ServerID,FamilyID,GetRoleID,GetFamilyID,GetServerID})
	end,
	{noreply, State};

do_handle_info({family_fighter_info_get_back, Res, Info, MatchServerID, MatchFamilyID,RoleID,FamilyID},State)->
	if Res == true andalso FamilyID > 0 ->
			family_misc:router_to_family_process(FamilyID, {family_fighter_info_get_back, RoleID,Info,MatchServerID,MatchFamilyID});
		true ->
			?unicast(RoleID,#sc_familyfight_fighter_info{result=3, selfFamily=[],otherFamily=[]})
		end,
	{noreply, State};

do_handle_info({family_fighter_info_get_back_info, Res, Info,ServerID,FamilyID, GetRoleID, GetFamilyID, GetServerID}, State)->
	send_msg_to_server(GetServerID, {family_fighter_info_get_back, Res, Info,ServerID,FamilyID, GetRoleID, GetFamilyID}),
	{noreply, State};

%% 战斗结束后，推送消息给对手，通知自己队伍信息的变化
do_handle_info({family_fighter_info_sync, SelfFamilyID,ListInfo,TarServerID,TarFamilyID},State)->
    case db_sql:if_family_exist(TarFamilyID) of
        false ->
            send_msg_to_server(TarServerID,{family_fighter_info_sync_back, data_setting:get(server_id),SelfFamilyID,ListInfo,TarFamilyID});
        true ->
            family_misc:router_to_family_process(TarFamilyID, {family_fighter_info_sync_back, data_setting:get(server_id),SelfFamilyID,ListInfo})
    end,
    {noreply, State};

do_handle_info({family_fighter_info_sync_back, SelfServerID,SelfFamilyID,ListInfo,TarFamilyID},State)->
    catch family_misc:router_to_family_process(TarFamilyID, {family_fighter_info_sync_back, SelfServerID,SelfFamilyID,ListInfo}),
    {noreply, State};

do_handle_info({cs_familyfight_attack, RoleID, FamilyID,SelfGerInfoList, RoleName,FamilyName,TarRoleID, TarFamilyID, TarServerID, ExtInfo}
			   ,#state{randomEndTime=RDTime,fightEndTime=FDTime}=State)->
	Now =util:now(),
	case Now > RDTime andalso Now < FDTime of
		true ->
			ServerID = data_setting:get(server_id),
			TarSec = util:now() + ?msg_timeout_interval,
			Ref = timer_wheel:add_plan(TarSec, fun()->
											        family_misc:router_to_family_process(FamilyID, {familyfight_attack_undo, RoleID, TarRoleID}) end),
			send_msg_to_server(TarServerID, {family_fight_attack, RoleID, FamilyID,ServerID,SelfGerInfoList,RoleName,FamilyName,TarRoleID,TarFamilyID,TarServerID,{ExtInfo,Ref}});
		_ ->
			family_misc:router_to_family_process(FamilyID, {familyfight_attack_undo, RoleID, TarRoleID}),
			?unicast(RoleID,#sc_familyfight_attack{result=10})
	end,
	{noreply, State};

do_handle_info({family_fight_attack, AttackRoleID,AttackFamilyID,AttackServerID,AttackGerInfoList,AttackRoleName,AttackFamilyName,TarRoleID,TarFamilyID,TarServerID,ExtInfo}, State)->
	case db_sql:if_family_exist(TarFamilyID) of
		false ->
			send_msg_to_server(AttackServerID, {family_fight_attack_back,not_exist_family,AttackRoleID,AttackFamilyID});
		true ->
			family_misc:router_to_family_process(TarFamilyID, {family_fight_attack,AttackRoleID,AttackFamilyID,AttackServerID,AttackGerInfoList
															  ,AttackRoleName,AttackFamilyName,TarRoleID,TarFamilyID,TarServerID,ExtInfo})
	end,
	{noreply, State};

do_handle_info({family_fight_attack_back_info, Res, AttackRoleID,AttackFamilyID,AttackServerID,WinFamilyName,GetStar,TarRoleID,TarFamilyID,TarServerID,TarRoleName,RecordInfo,FightersInfoList,ExtInfo},State)->
	send_msg_to_server(AttackServerID,{family_fight_attack_back,Res, AttackRoleID,AttackFamilyID, WinFamilyName,GetStar,TarRoleID,TarFamilyID,TarServerID,TarRoleName,RecordInfo,FightersInfoList,ExtInfo}),
	{noreply, State};

do_handle_info({family_fight_attack_back,Res, AttackRoleID,AttackFamilyID, _,GetStar,_,_,_,_,{IsWin, _, _, _},_,{_,Ref}}=Info,#state{period=Period, signList=MatchInfo}=State)->
	timer_wheel:cancel_plan(Ref),
	family_misc:router_to_family_process(AttackFamilyID,Info),
	MatchInfo2 = 
	case Res of
		not_exist_family ->
			?unicast(AttackRoleID,#sc_familyfight_attack{result=6,fight_dtl=[]}),
			MatchInfo;
		not_exist_role ->
			?unicast(AttackRoleID, #sc_familyfight_attack{result=7, fight_dtl=[]}),
			MatchInfo;
		role_can_not_attack->
			?unicast(AttackRoleID, #sc_familyfight_attack{result=4,fight_dtl=[]}),
			MatchInfo;
		true ->
            role_task:send_dispach(AttackRoleID, {dispach_task, family_fight_result, GetStar, IsWin, Period}),
			case lists:keytake(AttackFamilyID, #pf_fighter.familyID, MatchInfo) of
				false ->
					MatchInfo;
				{value,#pf_fighter{star=WinStar}=Fighter, OtherFighters} ->
					[Fighter#pf_fighter{star=WinStar+GetStar}|OtherFighters]
			end
	end,
	{noreply, State#state{signList=MatchInfo2}};

do_handle_info({cs_familyfight_result, RoleID,FamilyID, WinStar,Rank,LastWorldRank,FightResult,MatchStar,MatchServerID,MatchFamilyName},State)->
	#state{signList=SignList
		   ,fightEndTime=FightEndTime
		   ,periodEndTime=PeriodEndTime}=State,
	case lists:keyfind(FamilyID,#pf_fighter.familyID,SignList) of
		false ->
			?unicast(RoleID,#sc_familyfight_result{result=3,infoList=[]});
		_ ->
			Now=util:now(),
			case Now > FightEndTime andalso Now < PeriodEndTime of
				false ->
					?unicast(RoleID, #sc_familyfight_result{result=4,infoList=[]});
				_ ->
					case FightResult of 
						0 ->
							?unicast(RoleID, #sc_familyfight_result{result=6, infoList=[]});
						_ ->
							Info = #p_familyfight_result_dtl{fight_result=FightResult,win_star=WinStar,now_rank=Rank
															 ,old_rank=LastWorldRank,matcher_win_star=MatchStar
															,matcher_server_id=MatchServerID,matcher_family_name=MatchFamilyName},
							?unicast(RoleID, #sc_familyfight_result{result=1,infoList=[Info]})
					end
			end
	end,
	{noreply, State};

do_handle_info({family_server_get_family_fight_info, FamilyID}, #state{signList=MatchInfo,startTime=StartTime,prepareEndTime=PrepareEndTime
                      ,randomEndTime=RandomEndTime,fightEndTime=FightEndTime,periodEndTime=PeriodEndTime}=State)->
	case lists:keyfind(FamilyID, #pf_fighter.familyID, MatchInfo) of
		false ->
			Info = {family_fight_update_match_info, 0, 0,"",0,0, State#state.period,0
                    ,StartTime,PrepareEndTime,RandomEndTime,FightEndTime,PeriodEndTime},
			?INFO("get family_fight_info : ~w",[Info]),
			family_misc:router_to_family_process(FamilyID, Info);
		#pf_fighter{matchServerID=MatchServerID,matchFamilyID=MatchFamilyID,matchFamilyName=MatchFamilyName,matcherRank=MatchFamilyRank,matchStar=MatchFamilyStar} ->
			Info = {family_fight_update_match_info,MatchServerID,MatchFamilyID,MatchFamilyName,MatchFamilyRank,MatchFamilyStar,State#state.period,1
                   ,StartTime,PrepareEndTime,RandomEndTime,FightEndTime,PeriodEndTime},
			?INFO("get family_fight_info : ~w",[Info]),
			family_misc:router_to_family_process(FamilyID, Info)
	end,
	{noreply,State};

do_handle_info({timer_wheel_tick, LastTick}, State) ->
	timer_wheel:work(LastTick),
	{noreply, State};

do_handle_info({bc_new_period_begin,Period,StartTime,PrepareEndTime,RandomEndTime,FightEndTime,PeriodEndTime},State)->
	State2=State#state{period=Period,signList=[],startTime=StartTime,prepareEndTime=PrepareEndTime
					  ,randomEndTime=RandomEndTime,fightEndTime=FightEndTime,periodEndTime=PeriodEndTime},
    ?INFO("get bc_new_period_begin msg from family fight master server"),
	spawn(fun()->
				  FamilyIDs = db_sql:get_all_family_id(),
                  Msg = {clean_up_old_fight_info, Period,StartTime,PrepareEndTime, RandomEndTime,FightEndTime,PeriodEndTime},
				  lists:foreach(fun(FamilyID)-> family_misc:router_to_family_process(FamilyID, Msg)end, FamilyIDs)
				  end),
    db_sql:del_replay_with_type(?REPLAY_TYPE_FAMILY_FIGHT),
	{noreply, State2};

do_handle_info({random_match_fighters,F},#state{signList=List}=State)->
	%#pf_fighter{familyID=FamilyID, matchFamilyID=MatchFamilyID,matchServerID=MatchServerID} = F,
	FamilyID = F#pf_fighter.familyID,
	List2 = 
		case lists:keytake(FamilyID, #pf_fighter.familyID,List) of
			{value,_,Other}->
				[F|Other];
			_ ->
				[F|List]
		end,
	family_misc:router_to_family_process(FamilyID, {random_match_fighters,F}),
	{noreply, State#state{signList=List2}};

do_handle_info({get_family_fight_dtl_back,{Period,ST,PT,RT,FT,ET},NodeFighterInfo}, State)->
	case State#state.period == Period of
		true ->
			State2=State;
		_ ->
			erlang:send(self(),check_update_signer_info),
			State2 = #state{period=Period,signList=NodeFighterInfo,startTime=ST,prepareEndTime=PT,randomEndTime=RT,fightEndTime=FT,periodEndTime=ET}
	end,
	{noreply,State2};

do_handle_info({update_family_win_star, AttackFamilyID,FamilyWinStar}, State)->
	send_msg_to_master_server({update_family_win_star, AttackFamilyID,FamilyWinStar}),
	{noreply, State};

do_handle_info({change_state_info,{_,Period,StartTime,PrepareEndTime,RandomEndTime,FightEndTime,PeriodEndTime}}, State)->
    lists:foreach(fun(#pf_fighter{familyID=FamilyID
                                 ,matchServerID=MatchServerID
                                 ,matchFamilyID=MatchFamilyID
                                 ,matchFamilyName=MatchFamilyName
                                 ,matcherRank=MatchFamilyRank
                                 ,matchStar=MatchFamilyStar})->
                          Info = {family_fight_update_match_info,MatchServerID,MatchFamilyID,MatchFamilyName,MatchFamilyRank,MatchFamilyStar,State#state.period,1
                                 ,StartTime,PrepareEndTime,RandomEndTime,FightEndTime,PeriodEndTime},
                          family_misc:router_to_family_process(FamilyID, Info)
        end, State#state.signList),
	State2 = State#state{fightEndTime=FightEndTime,period=Period,startTime=StartTime,prepareEndTime=PrepareEndTime,randomEndTime=RandomEndTime,periodEndTime=PeriodEndTime},
	{noreply, State2};

do_handle_info({disband, FamilyID}, #state{signList=SignList}=State)->
	SignList2 = lists:keydelete(FamilyID, #pf_fighter.familyID, SignList),
	send_msg_to_master_server({disband, FamilyID}),
	{noreply, State#state{signList=SignList2}};

do_handle_info({update_firstN_list,List},State) ->
	update_firstN_list(List),
	{noreply,State};

do_handle_info(check_update_signer_info,#state{signList=SignList}=State)->
	SignList2 = lists:foldl(fun(#pf_fighter{}=E,Acc)->
									[E|Acc];
							   ({pf_fighter,FamilyID,ServerID,Score,PareID,Result,Star,MatchFamilyID,MatchServerID,MatchStar,Rank
								 ,LastRank,FamilyName,MatchFamilyName,MatcherRank,MatcherTotalFightPower,TotalFightPower},Acc)->
									{FamilyLevel,OwnerName} = db_sql:get_family_owner_name_level(FamilyID),
									[#pf_fighter{familyID=FamilyID,serverID=ServerID,score=Score,pareID=PareID,result=Result,star=Star,matchFamilyID=MatchFamilyID,matchServerID=MatchServerID,matchStar=MatchStar
												 ,rank=Rank,lastRank=LastRank,familyName=FamilyName,matchFamilyName=MatchFamilyName,matcherRank=MatcherRank,matcherTotalFightPower=MatcherTotalFightPower,totalFightPower=TotalFightPower
												 ,ownerName=OwnerName,level=FamilyLevel}|Acc]
							end,[], SignList),
	{noreply,State#state{signList=SignList2}};

do_handle_info({family_changed,FamilyID,OwnerName,FamilyLevel},State) ->
	send_msg_to_master_server({family_changed,FamilyID,OwnerName,FamilyLevel}),
	{noreply,State};

do_handle_info({update_family_name,FamilyID,NewName},#state{signList=SignList}=State) ->
    case lists:keytake(FamilyID, #pf_fighter.familyID, SignList) of
        false ->
            NewState = State;
        {value, Info, OtherList} ->
            NewList = [Info#pf_fighter{familyName=NewName}|OtherList],
            NewState = State#state{signList=NewList},
            ServerID = data_setting:get(server_id),
            send_msg_to_master_server({update_family_name, ServerID, FamilyID, NewName})
    end,  
    {noreply,NewState};

%% 找个仅用于公会战中，获得其他玩家的阵营信息
do_handle_info({cs_fighter_ger_view_other,RoleID,SrcServerID,TarRoleID,TarServerID},State) ->
    send_msg_to_server(TarServerID,{cs_fighter_ger_view_other_external, RoleID, SrcServerID,TarRoleID}),
    {noreply,State};

do_handle_info({cs_fighter_ger_view_other_external,RoleID,SrcServerID,TarRoleID},State) ->
	spawn(fun()->
				  case db_sql:get_roleInfo(TarRoleID) of
					  #role{}=Role ->
						  {FighterList,_,_,_} = role_data:get_otherRoleFighter(TarRoleID),
						  Reply = role_ger:ger_view_info(Role, FighterList);
					  _ ->
						  Reply = #sc_ger_view_other{tarRoleID=TarRoleID,roleName="",roleLevel=0,fightPower=0,gerList=[]}
				  end,
				  erlang:send(family_fight_server, {send_common_node_info, SrcServerID, RoleID,Reply})
		  end),
    {noreply,State};

do_handle_info({cs_fighter_ger_view_other_return, RoleID,Reply},State) ->
    ?unicast(RoleID,Reply),
    {noreply,State};

%% 找个仅用于公会战中，获得其他玩家的阵营信息
do_handle_info({cs_fighter_ger_view_other_dtl,RoleID,SrcServerID,TarRoleID,TarServerID},State) ->
    send_msg_to_server(TarServerID,{cs_fighter_ger_view_other_dtl_external, RoleID, SrcServerID,TarRoleID}),
    {noreply,State};

do_handle_info({cs_fighter_ger_view_other_dtl_external, RoleID,SrcServerID,TarRoleID}, State) ->
	spawn(fun()-> 
				  case db_sql:get_roleInfo(TarRoleID) of
					  #role{}=Role ->
						  %{FighterList, LieuInfoList} = db_sql:get_fighterList_and_lieuInfo(TarRoleID),
						  {FighterList, AtkAdd, HpAdd,LieuInfoList,Special,_LieuAdd} = db_sql:get_fighterList_and_lieu_add_info(TarRoleID),
						  ItemList = role_data:get_otherRoleItemEquips(TarRoleID),
						  SkinInfo = role_data:get_otherRoleSkinInfo(TarRoleID),
						  Reply = role_ger:ger_view_info_dtl(Role, FighterList, ItemList, AtkAdd, HpAdd,LieuInfoList,Special,SkinInfo);             
					  _ ->
						  % Reply = #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName="",roleLevel=0,isMale=false,fightPower=0,gerList=[],equipList=[], atkAdd=0, hpAdd=0,lieuViewList=[],skinInfo=#p_skin_info{equip=0},gerCrystalList=[]}
						  Reply = #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName="",roleLevel=0,isMale=false,fightPower=0,gerList=[],equipList=[],lieuViewList=[],skinInfo=#p_skin_info{equip=0},gerCrystalList=[]}
				  end,
				  erlang:send(family_fight_server, {send_common_node_info, SrcServerID, RoleID,Reply})
		  end),
	{noreply,State};

do_handle_info({send_common_node_info, SrcServerID, RoleID,Reply}, State)->
	send_msg_to_server(SrcServerID,{cs_fighter_ger_view_other_dtl_return, RoleID,Reply}),
    {noreply,State};

do_handle_info({update_match_family_name, OtherFamilyID, SelfFamilyID, NewName}, State) ->
    family_misc:router_to_family_process(OtherFamilyID, {update_match_family_name, SelfFamilyID, NewName}),
    {noreply,State};

do_handle_info({cs_fighter_ger_view_other_dtl_return, RoleID,Reply},State) ->
    ?unicast(RoleID,Reply),
    {noreply,State};

do_handle_info(Info,State)->
	?ERR("can't handle message:~w State:~w",[Info,State]),
	{noreply, State}.

%%%%%%%%%    send msg function   %%%%%%%%%%%%%%
sync_sign_tick()->
	erlang:send_after(?sync_signup_tick*1000, self(), sync_signup_info).

connect_master()->
	ServerID = data_setting:get(server_id),
	send_msg_to_master_server({get_familyfight_dtl,[ServerID|data_setting:get(merge_server_id_list)],data_setting:get(server_id)}),
	send_msg_to_master_server({get_firstN_list,ServerID}).

%% 这个方法是把消息发给另一个gameserver的family_fight_server
send_msg_to_server(ServerID,Msg) when is_binary(Msg)->
	send_msg:direct(ServerID,family_fight_server,Msg);

send_msg_to_server(ServerID,Msg) ->
	send_msg_to_server(ServerID,term_to_binary(Msg)).

send_msg_to_master_server(Msg) when erlang:is_binary(Msg)->
	send_msg:direct_by_name(get_master_server_name(),family_fight_master_server, Msg);

send_msg_to_master_server(Msg) ->
	send_msg_to_master_server(term_to_binary(Msg)).

%% 公会战服务器名称固定为family_fight_master
get_master_server_name()->
    family_fight_master.

%%%%%%% other function %%%%%%%%%%%%
update_firstN_list(List) when is_list(List)->
	put(?firstN_list,List).

get_firstN_list()->
	case get(?firstN_list) of
		List when is_list(List)->
			List;
		_ ->
			[]
	end.

send_role_rankerList(RoleID)->
	?unicast(RoleID,#sc_familyfight_rankerList{list=get_firstN_list()}).

get_sign_num_limit()->
	data_family_fight:get(sign_num_limit).

get_max_attackTimes()->
	data_family_fight:get(max_attack_times).

get_max_defendTimes()->	
	data_family_fight:get(max_defend_times).

get_eq_score()->
	data_family_fight:get(eqScore).
get_lose_score()->
	data_family_fight:get(loseScore).

%%%%%%% test function %%%%%%%%%%%
test_sign_in_familyfight()->
	%GetAllFamilyIDSql = io_lib:format(),
	FamilyIDList = db_sql:get_all("select familyID,familyScore,FamilyName  from gFamily;"),
	?ERR("test sign FamilyFight Info:~w",[FamilyIDList]),
	lists:foreach(fun([FamilyID,Score,FamilyName])->
						  erlang:send(family_fight_server, {cs_familyfight_sign, 0,FamilyID,Score,FamilyName,0,"OwnerName",0})
				  end, FamilyIDList),
	erlang:send_after(1000, family_fight_server, sync_signup_info).

test_sign_in_familyfight(FamilyIDinput)->
    %GetAllFamilyIDSql = io_lib:format(),
    FamilyIDList = db_sql:get_all("select familyID,familyScore,FamilyName  from gFamily where familyID = '~w';",[FamilyIDinput]),
    ?ERR("test sign FamilyFight Info:~w",[FamilyIDList]),
    lists:foreach(fun([FamilyID,Score,FamilyName])->
                          erlang:send(family_fight_server, {cs_familyfight_sign, 0,FamilyID,Score,FamilyName,0,"OwnerName",0})
                  end, FamilyIDList),
    erlang:send_after(1000, family_fight_server, sync_signup_info).

check_update_signer_info()->
	erlang:send(family_fight_server,check_update_signer_info).
