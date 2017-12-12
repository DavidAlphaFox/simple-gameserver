%% @author lixinglong
%% @doc @todo Add description to family_boss_server.


-module(family_boss_server).
-behaviour(gen_server).

-include("def_mail.hrl").
-include("def_role.hrl").

-define(DUMP_INTERVAL, (1000 * 60 * 5)).
-define(TICK_STATE_INTERVAL, 3600).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,start_link/1]).
-export([]).
%% ====================================================================
%% API functions
%% ====================================================================

start_link(FamilyID) ->
    gen_server:start_link(?MODULE, [FamilyID], []).



call_boss_server(FamilyID,Msg) ->
	gen_server:call(family_misc:make_family_boss_process_name(FamilyID), Msg).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-define(egg,0).
-define(born,1).
-define(sleep,2).
-define(boss,3).
-define(dead,4).
-define(familyID,familyID).
-define(rank_info,rank_info).
-record(boss,{pos=0, bossFighters=[],hp=0, maxHp=0,level=0,baseID=0,activeTime=0,attackList=[],lock=0,status=?egg,deadTime=0,dropID=0,isKilled=0}).
-record(state, {bossList = [],familyLevel=0,members=[],active_info=[],refreshDate={1970,1,1}}).%active_info[{date,N}]
%-record(p_family_boss_ranker,{roleID, roleName,rank,harm}).

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

init([FamilyID]) ->
    ?DEBUG("~ts:~w", ["初始化联盟副本进程", FamilyID]),
    erlang:process_flag(trap_exit, true),
    erlang:register(family_misc:make_family_boss_process_name(FamilyID), self()),
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), loop),
	State = init_state(FamilyID),
	check_refresh(),
    {ok, State}.

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
handle_call(Request, _From, State) ->
    {Reply2,State2} = 
		case catch do_handle_call(Request, State) of
			{'EXIT',Msg}->
				?ERR("exception:~w~nreq:~w~nstate:~w~n",[Msg, Request, State]),
				{ok,State};
        {Reply, NewState} ->
            {Reply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n req:~w~n State:~w", [Exeption, Request, State]),
            {ok, State}
    end,
	%?ERR("info:~w~n~w~n~w",[Request,State,State2]),
    {reply, Reply2, State2}.


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
handle_info({'EXIT', PID, Reason}, State) ->
    ?ERR("~ts: ~w, ~w", ["联盟进程收到exit消息", PID, Reason]),
    {noreply, State};
handle_info(Info, State) ->
	%?DEBUG("INFO:~w,~n,~w",[Info,State]),
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
		{noreply, State, hibernate}->
			{noreply, State, hibernate};
        {stop, normal, NewState} ->
            {stop, normal, NewState};
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
terminate(Reason, #state{bossList=BossList}=State) ->
	case Reason of
        shutdown ->
            ?DEBUG("Reason:~w, State:~w", [Reason, State]);
        _ ->
            ?ERR("Reason:~w, State:~w", [Reason, State])
    end,
	RankerList = get_rank_info(),
	FamilyID = get(?familyID),
	db_sql:set_family_boss_info(FamilyID,{BossList,RankerList}),
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

do_handle_call({cs_familyBoss_hatch_egg,Pos,_RoleID}, #state{bossList=BossList} = State) ->
	do_hatch_egg(Pos,BossList,State);
do_handle_call({cs_familyBoss_feed_boss, Pos,_RoleID}, #state{bossList=BossList,familyLevel=FamilyLevel}=State) ->
	do_feed_boss(Pos,BossList,BossList,FamilyLevel,State);
do_handle_call({cs_familyBoss_set_boss_time,Pos,Time,RoleID},#state{bossList=BossList,members=Members} = State)->
	do_set_boss_time(Pos,Time,BossList,Members,State,RoleID);
do_handle_call({family_levelup, Level}, State)->
	erlang:send(self(),{update_family_level, Level}),
	{ok,State};
do_handle_call({get_boss_level,Pos},#state{bossList=BossList}=State)->
	#boss{level=Level} = lists:keyfind(Pos,#boss.pos,BossList),
	{Level,State};
do_handle_call(Request, State)->
	?ERR("can't handle call :~w,state:~w",[Request,State]),
	{ok,State}.

do_handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
do_handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
do_handle_info(start_boss,State)->
	{noreply,State};
do_handle_info(check_refresh,#state{bossList=BossList,refreshDate=RefreshDate}=State)->
	check_refresh(),
	Today = erlang:date(),
	State2 = 
		case RefreshDate of
			Today ->
				State;
			_ ->
				BossList2 = lists:foldl(fun(Boss,Acc)->[Boss#boss{isKilled=0}|Acc] end, [], BossList),
				State#state{refreshDate=Today,bossList=BossList2}
		end,
	{noreply,State2};
do_handle_info(loop, #state{bossList=BossList}=State) ->
	erlang:send_after(?DUMP_INTERVAL, erlang:self(), loop),
	RankerList = get_rank_info(),
	FamilyID = get(?familyID),
	db_sql:set_family_boss_info(FamilyID,{BossList,RankerList}),
	erlang:garbage_collect(),
	{noreply, State, hibernate};
%	{noreply, State};
do_handle_info({update_family_level, Level}, #state{bossList=BossList,members=Members}=State)->
	case lists:keyfind(Level, 2, data_family_boss:get(family_boss_active_limit)) of
		false ->
			{noreply,State};
		{Pos,_,_} ->
			BaseLevel = data_family_boss:get(family_boss_base_level),
			{#ger{gerHp=Hp,gerBase=Base},_}={Boss,DropID}=init_boss_by_pos(Pos,Level,BaseLevel),
			BossInfo = #boss{pos=Pos,bossFighters=[Boss],hp=Hp,maxHp=Hp,level=data_family_boss:get(family_boss_base_level)
							 ,baseID = Base#gerBase.gerTypeID,activeTime=0,attackList=[],lock=1,status=?egg,deadTime=0,dropID=DropID},
			BossList2 = lists:keyreplace(Pos, #boss.pos, BossList, BossInfo),
			bc_msg(#sc_familyBoss_boss_unlock{pos=Pos}, Members),
			{noreply,State#state{bossList=BossList2}}
	end;
do_handle_info({be_boss,Pos},#state{bossList=BossList,members=Members} = State)->
	#boss{status=Status,activeTime=ActiveTime} =Boss= lists:keyfind(Pos, #boss.pos, BossList),
	case Status of
		?sleep ->
			BossList2 = lists:keyreplace(Pos,#boss.pos,BossList, Boss#boss{status=?boss}),
			bc_msg(#sc_familyBoss_boss_be_boss{pos=Pos},Members),
			TimeOutTime = data_family_boss:get(family_boss_attacked_duration),
			erlang:send_after((ActiveTime+TimeOutTime-util:now())*1000, self(), {boss_time_out,Pos}),
			clean_rank_history(Pos),
			{noreply,State#state{bossList=BossList2}};
		_ ->
			?ERR("err msg be_boss get when state:~w",[State]),
			{noreply,State}
	end;
do_handle_info({boss_time_out, Pos}, #state{bossList=BossList, members=Members}=State)->
	#boss{level=Level,status=Status} = lists:keyfind(Pos, #boss.pos, BossList),
	case Status of
		?boss ->
			BossInfo2 = do_reborn_boss(Pos,Level,timeout),
			BossList2 = lists:keyreplace(Pos, #boss.pos, BossList, BossInfo2),
			bc_msg(#sc_familyBoss_boss_dead{pos=Pos,reason=2},Members);
		_ ->
			BossList2 = BossList
	end,
	{noreply, State#state{bossList=BossList2}};
do_handle_info({cs_role_family_boss_base_info,RoleID}, #state{bossList=BossList,members=Members}=State)->
	case lists:member(RoleID,Members) of
		true ->
			MaxAttackTimes = data_family_boss:get(family_boss_max_attack_times),
			InfoList = lists:foldl(fun(#boss{pos=Pos, hp=Hp,maxHp=MaxHp, lock=Lock, attackList=AttackList, activeTime=ActiveTime,baseID=BaseID,status=Status,level=Level,deadTime=DeadTime,isKilled=IsKilled},Acc) ->
										   {_,AttackTimes} = case lists:keyfind(RoleID,1,AttackList) of false ->{0,0}; V -> V end, 
										   ProtectTime = 
											   case DeadTime of
												   0 ->
													   0;
												   _ ->
													   {Date,_} = util:seconds_to_datetime(DeadTime),
													   util:datetime_to_seconds({Date,{0,0,1}}) + ?ONE_DAY_SECONDS
											   end,
										   [#p_family_boss_base_info{pos=Pos,lock=Lock, hp=Hp,maxHp=MaxHp,activeTime=ActiveTime,baseID=BaseID,attackTimes=AttackTimes,status=Status,isKilled=IsKilled
																	,bossDuration=data_family_boss:get(family_boss_attacked_duration),level=Level,maxAttackTimes=MaxAttackTimes,bossDeadTime=ProtectTime}|Acc]
								   end,[], BossList),
			?unicast(RoleID,#sc_familyBoss_base_info{result = 1,infoList = InfoList});
		false ->
			?unicast(RoleID, #sc_familyBoss_base_info{result=2,infoList=[]})
	end,
	{noreply,State};
do_handle_info({boss_born, Pos,OldBoss=#boss{level=Level}}, #state{bossList=BossList,members=Members} = State) ->
	{Boss,DropID} = init_boss_by_pos(Pos, do_born, Level),
	BossList2 = lists:keyreplace(Pos, #boss.pos,BossList,OldBoss#boss{bossFighters=[Boss], hp=Boss#ger.gerHp,maxHp=Boss#ger.gerHp,status=?born,dropID=DropID}),
	bc_msg(#sc_familyBoss_boss_born{pos=Pos, hp=Boss#ger.gerHp,maxHp=Boss#ger.gerHp},Members),
	{noreply, State#state{bossList=BossList2}};
do_handle_info({member_join, JoinRoleID}, #state{members=Members}=State)->
	{noreply,State#state{members=[JoinRoleID|Members]}};
do_handle_info({member_leave,LeaveRoleID},#state{members=Members}=State)->
	{noreply,State#state{members=lists:delete(LeaveRoleID,Members)}};
do_handle_info(family_disbanded,State)->
	{stop, normal, State};
do_handle_info({cs_role_family_boss_attack_boss,Pos,RoleID,RoleFighterInfo,RoleName}, #state{bossList=BossList,members=Members}=State) ->
	case check_role_can_attack(Pos,RoleID, BossList, Members) of
		{true,Boss,DefendTimes,DefendList} ->
			BossList2 = do_attack_boss(Pos, RoleID,BossList,Members,Boss,RoleFighterInfo,RoleName,DefendTimes,DefendList),
			{noreply,State#state{bossList=BossList2}};
		{false,Reason} ->
			?unicast(RoleID,#sc_familyBoss_attack{result=Reason,fightInfo=[]}),
			{noreply, State}
	end;
do_handle_info({cs_familyBoss_get_rank, Pos,RoleID}, State)->
	do_role_get_rank(Pos, RoleID),
	{noreply,State};	


do_handle_info(Msg,State)->
	?ERR("can't handle msg:~w when state :~w",[Msg,State]),
	{noreply, State}.

do_role_get_rank(Pos,RoleID) ->
	case lists:member(Pos,data_family_boss:get(boss_pos_list)) of
		false ->
			?unicast(RoleID,#sc_familyBoss_get_rank{result=3,pos=Pos,rankList=[]});
		true ->
			{_,RankList} = lists:keyfind(Pos,1,get_rank_info()),
			Len = data_family_boss:get(family_boss_rank_len),
			?unicast(RoleID, #sc_familyBoss_get_rank{result=1, pos=Pos,rankList=lists:sublist(RankList,Len)})
	end.

do_attack_boss(Pos, RoleID,BossList,Members,BossInfo,{FighterList,LieuAdd},RoleName,DefendTimes,DefendList) ->
	#boss{dropID=DropID,maxHp=BossMaxHp,bossFighters=[Boss],level=Level}=BossInfo,
	SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
	EquipList = role_data:get_otherRoleItemEquips(RoleID),
    GerEquipList = role_item:assort_ger_equiplist(EquipList),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
	{_Result, FightRecord, {_,[{_,BossNewHp,_}],_}} = role_fight:new(FighterList, [Boss],LieuAdd,#add_attr{},SkinInfo,#skin_info{},LegendAddList,[]),
	BossNewHp2 = erlang:min(BossNewHp,BossMaxHp),
	Harm = Boss#ger.gerHp - BossNewHp2,
	Boss2 = Boss#ger{gerHp=BossNewHp2},
	bc_msg(#sc_familyBoss_bc_attack{roleName=RoleName,harm=Harm,pos=Pos},Members),
	update_rank_info(Pos,RoleID,RoleName,Harm),
	?unicast(RoleID,#sc_familyBoss_attack{result=1,fightInfo=[FightRecord]}),
	BossInfo2 = 
		if BossNewHp2 =< 0 ->
			   RewardList = data_family_boss:get({family_boss_drop,DropID}),
			   DropTimes = data_family_boss:get(family_boss_drop_num),
			   Rewards = util:random_list(RewardList,DropTimes),
			   %Reward = util:random_one_from_weigh_list(RewardList),
			   family_misc:router_to_family_process(get(?familyID),{family_extra,{family_boss_reward,Pos,Level,Rewards}}),
			   bc_msg(#sc_familyBoss_boss_dead{pos=Pos,reason=1}, Members),
			   do_reborn_boss(Pos,Level,killed);
		   true ->
			   BossInfo#boss{bossFighters=[Boss2], hp=BossNewHp2,attackList=[{RoleID, DefendTimes+1}|DefendList]}
		end,
	lists:keyreplace(Pos, #boss.pos,BossList,BossInfo2).

do_reborn_boss(Pos,Level,Type)->
	case init_boss_by_pos(Pos,ignore, Level) of
		?undefined ->
			#boss{pos=Pos,lock=0};
		{#ger{gerHp=Hp,gerBase=Base},_} = {Boss,DropID} ->
			IsKilled = 
				case Type of
					killed ->
						1;
					_ ->
						0
				end,
			#boss{pos=Pos,bossFighters=[Boss],hp=Hp,maxHp=Hp,level=Level,isKilled=IsKilled
				  ,baseID = Base#gerBase.gerTypeID,activeTime=0,attackList=[],lock=1,status=?born,deadTime=util:now(),dropID=DropID}
	end.

update_rank_info(Pos,RoleID,RoleName,Harm)->
	{value,{_,RankList},OtherRankInfo} = lists:keytake(Pos, 1, get_rank_info()),
	RankerList2 = update_rank_history(RoleID,RoleName,Harm, RankList),
	set_rank_info([{Pos,RankerList2}|OtherRankInfo]).

get_rank_info() ->
	get(?rank_info).

set_rank_info(List) when is_list(List) ->
	put(?rank_info,List).

clean_rank_history(Pos)->
	set_rank_info(lists:keyreplace(Pos, 1, get_rank_info(), {Pos,[]})).

update_rank_history(RoleID, RoleName, Harm, RankList)->
	RankList3 = 
		case lists:keytake(RoleID, #p_family_boss_ranker.roleID, RankList) of
			false ->
				lists:keysort(#p_family_boss_ranker.harm, [#p_family_boss_ranker{roleID=RoleID,roleName=RoleName,harm=Harm}|RankList]);
			{value,#p_family_boss_ranker{harm=Harm0}=Ranker, RankList2} ->
				lists:keysort(#p_family_boss_ranker.harm, [Ranker#p_family_boss_ranker{harm=Harm0+Harm}|RankList2])
		end,
	{RankList4,_}=lists:foldl(fun(Ranker,{RankerListAcc,Rank})->{[Ranker#p_family_boss_ranker{rank=Rank}|RankerListAcc],Rank+1} end, {[],1},lists:reverse(RankList3)),
	lists:reverse(RankList4).

check_role_can_attack(Pos,RoleID,BossList,Members) ->
	case lists:member(RoleID,Members) of
		false ->
			{false, 2};
		true ->
			case lists:keyfind(Pos,#boss.pos,BossList) of
				false ->
					{false,3};
				#boss{status=Status,attackList=DefendList}=Boss ->
					case Status of
						?boss ->
							case lists:keytake(RoleID,1,DefendList) of
								false ->
									{true,Boss,0,DefendList};
								{value,{_,Times},DefendList2}->
									MaxAttackTimes = data_family_boss:get(family_boss_max_attack_times),
									if Times < MaxAttackTimes ->
										   {true,Boss,Times,DefendList2};
									   true ->
										   {false,5}
									end
							end;
						_ ->
							{false, 4}
					end
			end
	end.

do_set_boss_time(Pos,Time,BossList,Members,State,RoleID)->
	case lists:member(Pos,data_family_boss:get(boss_pos_list)) of
		true ->
			#boss{lock=Lock,status=Status,deadTime=DeadTime}=Boss = lists:keyfind(Pos,#boss.pos,BossList),
			case Lock of
				0 ->
					{4, State};
				1 ->
					case Status of
						?born ->
							#state{active_info=ActiveInfo}=State,
							MaxSetN = data_family_boss:get(family_boss_max_change),
							{SetDate,_} = util:seconds_to_datetime(Time),
							Today = erlang:date(),
							SetN = 
								case lists:keyfind(SetDate,1,ActiveInfo) of
									false ->
										0;
									{_,N}->
										N
								end,
							if SetN >= MaxSetN ->
								   {9,State};
							   true ->
								   ActiveInfo2 = lists:keysort(1, ActiveInfo),
								   ActiveInfo3 =
									   lists:foldl(fun({Date1,_}=Value,Acc)->
														   case Date1 < Today of
															   true ->
																   Acc;
															   _ ->
																   [Value|Acc]
														   end
												   end, [], ActiveInfo2),
								   ActiveInfo4 = 
									   case lists:keytake(SetDate, 1, ActiveInfo3) of
										   false ->
											   [{SetDate,1}|ActiveInfo3];
										   {value,{_,N2},OtherActiveInfo} ->
											   [{SetDate,N2+1}|OtherActiveInfo]
									   end,
								   %ActiveDuration = data_family_boss:get(family_boss_active_duration),
								   Now=util:now(),
								   case DeadTime of
									   0 ->
										   DeadDate = {1970,1,1};
									   _ ->
										   {DeadDate,_} = util:seconds_to_datetime(DeadTime)
								   end,
								   case  erlang:date() =/= DeadDate of %DeadTime + ActiveDuration >= Now orelse
									   true ->
										   if Time - Now + 5 > 0 -> %% 5秒时差
												  erlang:send_after((Time - Now)*1000, self(), {be_boss,Pos}),
												  BossList2 = lists:keyreplace(Pos,#boss.pos,BossList,Boss#boss{status=?sleep,activeTime=Time}),
												  bc_msg(#sc_familyBoss_bc_set_boss_time{pos=Pos,activeTime=Time},lists:delete(RoleID,Members)),
												  {1, State#state{bossList=BossList2,active_info=ActiveInfo4}};
											  true ->
												  {8,State}
										   end;  
									   false ->
										   {6,State}
								   end
							end;
						_ ->
							{5,State}
					end
			end;
		_ ->
			{3,State}
	end.
do_hatch_egg(Pos,BossList,State)->
	case lists:member(Pos,data_family_boss:get(boss_pos_list)) of
		true ->
			#boss{lock=Lock,status = Status,level=Level} =Boss= lists:keyfind(Pos,#boss.pos, BossList),
			case Lock of
				0 ->
					{4,State};%{family_level_not_enough,State};
				1 ->
					case Status of
						?egg ->
							erlang:send(self(), {boss_born, Pos,Boss#boss{level=Level+1}}),
							{1, State};%{success_active, State}
						_ ->
							{5, State}%% already actived
					end
			end;
		_ ->
			{3,State}%{no_such_boss,State}
	end.
do_feed_boss(Pos,BossList,BossList,FamilyLevel,State)->
	case lists:member(Pos,data_family_boss:get(boss_pos_list)) of
		true ->
			#boss{lock=Lock,level=Level,status=Status}=Boss = lists:keyfind(Pos,#boss.pos,BossList),
			case Lock of
				0 ->
					{4, State};
				1 ->
					case Status of
						?born ->
							{_,MaxLevel} = lists:keyfind(Pos,1,data_family_boss:get(family_boss_max_level_limit)),
							case Level >= MaxLevel of
								true ->
									{6,State};
								_ ->
									{NewBoss,DropID} = init_boss_by_pos(Pos,FamilyLevel,Level+1),
									BossList2 = lists:keyreplace(Pos,#boss.pos,BossList,Boss#boss{bossFighters=[NewBoss],level=Level + 1, hp=NewBoss#ger.gerHp,maxHp=NewBoss#ger.gerHp,dropID=DropID}),
									%Cost = get_upLevel_cost(Pos,Level),
									{1,State#state{bossList=BossList2}}
							end;
						_ ->
							{5,State}
					end
			end;
		false ->
			{3,State}%% no such boss
	end.

get_upLevel_cost(Pos,Level)->
	CostList = data_family_boss:get({family_boss_uplevel_cost,Pos}),
	{_,Cost} = lists:keyfind(Level, 1, CostList),
	Cost.

bc_msg(Msg, Members)->
	lists:foreach(fun(RoleID)-> ?unicast_async(RoleID,Msg) end,Members).


init_boss_by_pos(Pos,FamilyLevel,?undefined) ->
	{_,LimitLevel,_} = lists:keyfind(Pos, 1, data_family_boss:get(family_boss_active_limit)),
	if LimitLevel > FamilyLevel ->
		   ?undefined;
	   true ->
		   BaseLevel = data_family_boss:get(family_boss_base_level),
		   init_boss_by_pos(Pos,FamilyLevel,BaseLevel)
	end;
init_boss_by_pos(Pos,_,NowBossLevel)->
	{_,GerTypeID, DropID, GerPos,GerLevel,GerQuality} = lists:keyfind(NowBossLevel,1,data_family_boss:get({family_boss_base_id, Pos})),
	Boss = ger_attr:new_ger(GerTypeID, GerLevel, GerQuality, [], []),
	{?change_pos(Boss, GerPos),DropID}.
	
init_state(FamilyID)->
	{FamilyLevel,Members} = family_server:boss_request_family_info(FamilyID),
	{FamilyBossInfo,RankerInfo} = db_sql:get_family_boss_info(FamilyID),
	put(?familyID,FamilyID),
	Now=util:now(),
	BossList = lists:foldl(fun(Pos,Acc)-> 
								   case lists:keyfind(Pos, #boss.pos, FamilyBossInfo) of
									   false ->
										   case init_boss_by_pos(Pos,FamilyLevel, ?undefined) of
											   ?undefined ->
												   [#boss{pos=Pos,lock=0}|Acc];
											   {#ger{gerHp=Hp,gerBase=Base},DropID} = {Boss,_} ->
												   [#boss{pos=Pos,bossFighters=[Boss],hp=Hp,maxHp=Hp,level=data_family_boss:get(family_boss_base_level)
														  ,baseID = Base#gerBase.gerTypeID,activeTime=0,attackList=[],lock=1,status=?egg,deadTime=0,dropID=DropID}|Acc]
										   end;
									   #boss{lock=0}->
										   case init_boss_by_pos(Pos,FamilyLevel, ?undefined) of
											   ?undefined ->
												   [#boss{pos=Pos,lock=0}|Acc];
											   {#ger{gerHp=Hp,gerBase=Base},DropID} = {Boss,_} ->
												   [#boss{pos=Pos,bossFighters=[Boss],hp=Hp,maxHp=Hp,level=data_family_boss:get(family_boss_base_level)
														  ,baseID = Base#gerBase.gerTypeID,activeTime=0,attackList=[],lock=1,status=?egg,deadTime=0,dropID=DropID}|Acc]
										   end;
									   #boss{activeTime=ActiveTime,pos=Pos,status=Status}=Boss ->
										   ActiveTime2 = 
											   case ActiveTime > Now of
												   true ->
													   erlang:send_after((ActiveTime-Now)*1000,self(), {be_boss,Pos}),
													   ActiveTime;
												   false ->
													   case Status of
														   ?sleep ->
															   erlang:send(erlang:self(),{be_boss,Pos}),
															   Now;
														   ?boss ->
															   erlang:send_after(data_family_boss:get(family_boss_attacked_duration) * 1000, erlang:self(), {boss_time_out,Pos}),
															   Now;
														   _ ->
															   ActiveTime
													   end
											   end,
										   [Boss#boss{activeTime=ActiveTime2}|Acc]
								   end
						   end,[], data_family_boss:get(boss_pos_list)),
	init_ranker_info(RankerInfo),
	#state{bossList = BossList,familyLevel=FamilyLevel,members=Members}.

init_ranker_info(RankerInfo) ->
	RankList = lists:foldl(fun(Pos,Acc)->
								   case lists:keyfind(Pos, 1, RankerInfo) of
									   false ->
										   [{Pos,[]}|Acc];
									   Info ->
										   [Info|Acc]
								   end
						   
						   end, [], data_family_boss:get(boss_pos_list)),
	set_rank_info(RankList).


check_refresh()->
	Sec = ?TICK_STATE_INTERVAL - (util:now() rem ?TICK_STATE_INTERVAL), 
	erlang:send_after(Sec*1000,self(),check_refresh).

get_boss_typeID(Pos,Level)->
	{_,GerTypeID, _, _,_,_} = lists:keyfind(Level,1,data_family_boss:get({family_boss_base_id, Pos})),
	GerTypeID.
