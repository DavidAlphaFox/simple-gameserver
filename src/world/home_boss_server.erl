%% @author crimoon-17
%% @doc @todo Add description to home_boss_server.


-module(home_boss_server).
-behaviour(gen_server).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  
-include("def_mail.hrl").  
-include("def_fight.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start/0
         ]).


start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {lastDate=0}).
-record(boss_dtl,{chapter=0,display=0,boss=[],activeTime=0,fullBlood=0,nowBlood=0,reward=0,isGetReward=0,state=0,from=0,own=0,ownName="",isRead=0,dead_list=[]}).
-record(home_boss,{roleID=0,self=#boss_dtl{},own=#boss_dtl{},last_boss=0,level=0,reward_level=0,machineBuff=0}).% level自己的boss等级

-define(BOSS_STATE_1,1).

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
	load_data(),
	tick(),
	timer_wheel:init(),
	Date = erlang:date(),
    NextZeroClockSec = util:datetime_to_seconds({Date,{0,0,1}}) + ?ONE_DAY_SECONDS,
    timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
    {ok, #state{lastDate=Date}}.

load_data()->
	case db_sql:get_etc(?DB_ETC_KEY_HOMEBOSS_DATA) of
		[] -> ignore;
		{Date,Data} -> 
			case erlang:date() of Date ->[begin
											  #home_boss{self=Self,own=Own} = D,
											  D2 = case is_record(Self,boss_dtl) of true -> D;
													   _ -> update_record(D,Self,Own)
												   end,
											  ets:insert(?ETS_HOME_BOSS_TABLE, D2)
										  end||D<-Data];
				_ -> ignore
			end
	end.

update_record(D,Self,Own) ->
	Self2 = update_record(Self),
	Own2 = update_record(Own),
	D#home_boss{self=Self2,own=Own2}.
update_record(0) -> 0;
update_record({boss_dtl,Chapter,Display,Boss,ActiveTime,FullBlood,NowBlood,Reward,IsGetReward,State,From,Own,OwnName,IsRead}) ->
	{boss_dtl,Chapter,Display,Boss,ActiveTime,FullBlood,NowBlood,Reward,IsGetReward,State,From,Own,OwnName,IsRead,[]}.
	

%%update_record(D,Self,Own) ->
%%	{boss_dtl,Chapter,Display,Boss,ActiveTime,FullBlood,NowBlood,Reward,IsGetReward,State,From,Own,OwnName,IsRead} = Self,
%%	{boss_dtl,Chapter2,Disppay2,Boss2,ActiveTime2,FillBlood2,NowBlood2,Reward2,IsGetReward2,State2,Frome2,Own2,OwnName2,IsRead2} = Own,
%%	Self2 = {boss_dtl,Chapter,Display,Boss,ActiveTime,FullBlood,NowBlood,Reward,IsGetReward,State,From,Own,OwnName,IsRead,[]},
%%	Own2 = {boss_dtl,Chapter2,Disppay2,Boss2,ActiveTime2,FillBlood2,NowBlood2,Reward2,IsGetReward2,State2,Frome2,Own2,OwnName2,IsRead2,[]},
%%	D#home_boss{self=Self2,own=Own2}.

tick()->
	erlang:send_after(30000,self(),do_persist).

do_persist(#state{lastDate=LastDate})->
	db_sql:set_etc(?DB_ETC_KEY_HOMEBOSS_DATA,{LastDate,ets:tab2list(?ETS_HOME_BOSS_TABLE)}).

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
handle_info(do_persist, State)->
	tick(),
	do_persist(State),
	{noreply,State,hibernate};
handle_info({set_date,Date},State) ->
	{noreply,State#state{lastDate=Date}};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
	{noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
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

do_handle_info({attack_homeBoss,SelfRoleID,RoleID,Type,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo}) ->
	attack_boss(SelfRoleID,RoleID,Type,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo),
	ok;
do_handle_info({get_reward,RoleID}) ->
	get_reward(RoleID),
	ok;
do_handle_info({read,RoleID})->
	#home_boss{self=Self}=RoleHomeBoss=get_role_home_boss(RoleID),
	case Self#boss_dtl.own of 
		0 ->
			NewSelf = Self#boss_dtl{isRead=1},
			ets:insert(?ETS_HOME_BOSS_TABLE,RoleHomeBoss#home_boss{self=NewSelf});
		TarRoleID ->
			#home_boss{own=Own}=TarRoleHomeBoss=get_role_home_boss(TarRoleID),
			NewOwn = Own#boss_dtl{isRead=1},
			ets:insert(?ETS_HOME_BOSS_TABLE,TarRoleHomeBoss#home_boss{own=NewOwn})
	end;
do_handle_info({timer_wheel_tick, LastTick}) ->
	timer_wheel:work(LastTick),
	ok;
do_handle_info({inet_reply,_S,_Status}) ->
    ok;
do_handle_info({Ref,_Res}) when is_reference(Ref) ->
    ok;
do_handle_info(Info) ->
	?ERR("get message :~w",[Info]),
	ok.

get_homestead_buff(RoleID) ->
    HomeBoss = get_role_home_boss(RoleID),
	HomeBoss#home_boss.machineBuff.

hook_zero_clock() ->
	Date= erlang:date(),
    NextZeroClockSec = util:datetime_to_seconds({Date,{0,0,1}}) + ?ONE_DAY_SECONDS,
    timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
    %% 刷新次数
    ets:delete_all_objects(?ETS_HOME_BOSS_TABLE),
	erlang:send(?MODULE,{set_date,Date}),
	role_lib:send_every_server_msg({route, role_homeBoss, #cs_homeBoss_info{roleID=0}}).

get_role_home_boss(RoleID) ->
	case ets:lookup(?ETS_HOME_BOSS_TABLE,RoleID) of
		[] ->
			RoleHomeBoss = gen_role_home_boss(RoleID,util:random_one_from_list(data_home_boss:get(boss_list)),1),
			ets:insert(?ETS_HOME_BOSS_TABLE,RoleHomeBoss),
			RoleHomeBoss;
		[RoleHomeBoss] ->
			RoleHomeBoss
	end.
		

gen_role_home_boss(RoleID,ID,Level) ->
	{Team1,Team2} = data_home_boss:get({boss,ID,Level}),
	{Boss,Mons} = gen_boss(Team1,Team2),
	Today = erlang:date(),
	FullBlood = calc_blood(Mons),
%	Reward = data_home_boss:get({boss_from_reward,ID,Level}),
		#rolePublic{isMale=IsMale} = role_lib:get_rolePublic(RoleID),
	{RewardBase,{Num,T1,T2}} = data_home_boss:get({boss_from_reward,ID,Level}),
	Reward = if IsMale -> role_homeBoss:add_reward(RewardBase,Num,T1);true -> role_homeBoss:add_reward(RewardBase,Num,T2) end,
	
	ActiveTime = util:datetime_to_seconds({Today,data_home_boss:get(init_active_time)}),
	SelfBoss = #boss_dtl{chapter=Level,display=ID,boss=Boss,activeTime=ActiveTime,fullBlood=FullBlood
						,nowBlood=FullBlood,reward=Reward,isGetReward=0,state=?BOSS_STATE_1,from=RoleID
						,own=0,isRead=0,dead_list=[]},
	#home_boss{roleID=RoleID,self=SelfBoss,own=0,last_boss=ID,level=Level}.

gen_boss(Team1,Team2) ->
	{T1,Mons1} = gen_boss2(Team1,0),
	{T2,Mons2} = gen_boss2(Team2,10),
	{{T1,T2},Mons1++Mons2}.

gen_boss2({Trainer,Skin,V1,V2,V3,V4,V5,V6},Base) ->
		case Trainer of
		{TrainerID,Level,Special} ->
			TrSpe = #trSpecial{trID = TrainerID,specialID = Special,roleLevel = Level,sp = 0,state = 0};
		_ ->
			TrSpe = #trSpecial{}
	end,
	Mons = gen_mon_list(V1,V2,V3,V4,V5,V6,Base),
	{#double_team{skin_info=Skin,talent=[],trainer=TrSpe,fighters=Mons,lieu=#add_attr{}},Mons}.

gen_mon_list(D1,D2,D3,D4,D5,D6,Base) -> %% Base 用于计算精灵是哪个训练师的
	List = [{1,D1},{2,D2},{3,D3},{4,D4},{5,D5},{6,D6}],
	IDList = [ID||{_Pos, #mon{gerTypeID=ID}} <- List],
	F = fun(E,P) ->
				GerTypeID = E#mon.gerTypeID,
				Level = E#mon.gerLevel,
				Quality = E#mon.gerQuality,
				ger_attr:recacl2(P+Base, GerTypeID, 0, Level, data_ger_level:get(Level), Quality, [], lists:delete(GerTypeID, IDList), 0, 0, true,#add_attr{},no_talent,#add_attr{},[],[],[],[],[],0,#holyGrail{})

%				ger_attr:new_mon(P+Base, E#mon.gerLevel, E#mon.gerQuality, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- List, is_record(Mon, mon), Ger<-[F(Mon,Pos)], is_record(Ger, ger)].

	
calc_blood(Mons) ->
	lists:foldl(fun(#ger{gerHp=Hp},Acc) -> Acc+Hp end,0,Mons).



get_homeBoss_info(RoleID) ->
	#home_boss{self=Self,own=Own} = get_role_home_boss(RoleID),
	{homeBoss2p_boss(Self),homeBoss2p_boss(Own)}.

homeBoss2p_boss(0) -> 
		#p_homeBoss{activeTime = 0
		   ,from=0
		   ,owner=0
		   ,isRead=0
		   ,rewardState=0
		   ,reward=activity_server:sell_reward2p_reward_info(#sell_reward{})
		   ,type=0
		   ,totalBlood=0
		   ,nowBlood=0
		   ,chapter=0
		   ,ownName=""
		   };
homeBoss2p_boss(HomeBoss) ->
	#p_homeBoss{activeTime = HomeBoss#boss_dtl.activeTime
		   ,from=HomeBoss#boss_dtl.from
		   ,owner=HomeBoss#boss_dtl.own
		   ,isRead=HomeBoss#boss_dtl.isRead
		   ,rewardState=HomeBoss#boss_dtl.isGetReward
		   ,reward=activity_server:sell_reward2p_reward_info(HomeBoss#boss_dtl.reward)
		   ,type=HomeBoss#boss_dtl.display
		   ,totalBlood=HomeBoss#boss_dtl.fullBlood
		   ,nowBlood=HomeBoss#boss_dtl.nowBlood
		   ,chapter=HomeBoss#boss_dtl.chapter
		   ,ownName=HomeBoss#boss_dtl.ownName
		   }.

read(RoleID) ->
	erlang:send(?MODULE,{read,RoleID}).

get_self_reward(RoleID) ->
	erlang:send(?MODULE,{get_reward,RoleID}).

get_reward(RoleID) ->
	#home_boss{self=Own,last_boss=Level,reward_level=RewardLevel,level=Round} = Boss = get_role_home_boss(RoleID),
	case Round of 
		RewardLevel ->
			case Own#boss_dtl.isGetReward of
				1 ->
					case ?CATCH(role_lib:send_server(RoleID,{route,role_homeBoss,{get_reward,true,{Level,RewardLevel}}})) of
						{'EXIT',_} -> ignore;
						_ ->
							case Round of 5 -> do_end_self(Boss);
								_ ->
									case get_tarRoleBoss(RoleID,Own#home_boss.own) of
										ignore -> ignore;
										TarRoleBoss -> 
											gen_next_boss(RoleID,TarRoleBoss)
									end
							end
					end;
				0 ->
					role_lib:send_server(RoleID,{route,role_homeBoss,{get_reward,false,2}});
				2 ->
					role_lib:send_server(RoleID,{route,role_homeBoss,{get_reward,false,3}})
			end;
		_ ->
			?ERR("not match:~w",[[Round,RewardLevel,RoleID]]),
			?unicast(RoleID,#sc_homeBoss_get_reward{result=3})
	end.

do_end_self(#home_boss{self=Self,roleID=RoleID}=Boss)->
	NewBoss = Boss#home_boss{self=Self#boss_dtl{activeTime=0}},
	ets:insert(?ETS_HOME_BOSS_TABLE,NewBoss),
		?unicast(RoleID,#sc_homeBoss_info{myBoss=homeBoss2p_boss(NewBoss#home_boss.self),ownBoss=homeBoss2p_boss(NewBoss#home_boss.own)}).
	

get_tarID(RoleID,OID) ->
	if OID /= 0 ->
		   L = get_role_home_boss(OID),
		   ets:insert(?ETS_HOME_BOSS_TABLE,L#home_boss{own=0});
	   true ->
		   ignore
	end,
	H = get_tarRoleBoss(RoleID,OID),
	#home_boss{self=Self}=Y = get_role_home_boss(RoleID),
	Y2 = Y#home_boss{self=Self#boss_dtl{own=H#home_boss.roleID}},
	ets:insert(?ETS_HOME_BOSS_TABLE,Y2),
	ets:insert(?ETS_HOME_BOSS_TABLE,H#home_boss{own=Y2#home_boss.self}),
	(H)#home_boss.roleID.

get_tarRoleBoss(RoleID,OID)->
	[{_,MaxID}] = ets:lookup(?ETS_ID, roleID),
	RobotId = tk_id:robot_roleID_max(),
	FMax0 =MaxID div 5,
	U =MaxID-100-FMax0,
	FMaxID = if U =< 5 -> 0;
				true -> random:uniform(U) + FMax0 
			 end,
	Ms = ets:fun2ms(fun(#home_boss{own=Own,roleID=RoleID2}=X) when Own == 0 andalso RoleID2 > FMaxID -> X end),
	case ets:select(?ETS_HOME_BOSS_TABLE, Ms, 3) of
		'$end_of_table' ->
			case random_new(MaxID,RobotId) of ignore -> get_homeBoss_info(RoleID); X -> X end;
		{[H],_} ->
			case H#home_boss.roleID of 
				RoleID -> HL = random_new(MaxID,RobotId) ,case HL  of ignore -> H; _ -> HL end; 
				_ -> H
			end;
		{List,_} -> 
			List2 = [L||#home_boss{roleID=HRoleID}=L<-List,HRoleID /=RoleID,HRoleID /= OID],
			case List2 of 
				[] -> 
					HB = random_new(MaxID,RobotId),
										%?ERR("va:~w",[[RoleID,OID,MaxID,RobotId,HB]]),
					case HB of ignore -> hd(List);
						_ -> util:random_one_from_list(List)
					end;
				_ -> hd(List2)
			end
%		{List,_} -> hd([L||#home_boss{roleID=HRoleID}=L<-List,HRoleID /=RoleID,HRoleID /= OID])
	end.
%			case H1#home_boss.roleID of
%				RoleID -> H2;
%				_ -> case H2#home_boss.roleID of
%						 OID -> HL = random_new(MaxID,RobotId), case HL of ignore -> H2; _ -> HL end;
%						 _ -> H2
%					 end													 
%			end;
%		{[H1,H2,H3],_} ->
%			case H1#home_boss.roleID of
%				RoleID -> H2;
%				_ -> case H2#home_boss.roleID of
%						 OID -> H3;
%						 _ -> H2
%					 end
%			end;
%	end.

random_new(MaxID,RobotId)->
	case get_tar_roleID(db_sql:random_get_roles(5,MaxID,RobotId)) of
		ignore ->ignore;
		TarRoleID -> get_role_home_boss(TarRoleID)
	end.

get_tar_roleID([]) -> ignore;
get_tar_roleID([H|T]) ->
	case ets:lookup(?ETS_HOME_BOSS_TABLE, H) of
		[] -> H;
		_ -> get_tar_roleID(T)
	end.

mark_self_reward(RoleID) ->
	#home_boss{self=Self,last_boss=ID,level=Level}=RoleHomeBoss = get_role_home_boss(RoleID),
	{RewardBase,{Num,T1,T2}} = data_home_boss:get({boss_from_reward,ID,Level}),
	#rolePublic{isMale=IsMale} = role_lib:get_rolePublic(RoleID),
	Reward = if IsMale -> role_homeBoss:add_reward(RewardBase,Num,T1);true -> role_homeBoss:add_reward(RewardBase,Num,T2) end,
	Now = RoleHomeBoss#home_boss{self=Self#boss_dtl{isGetReward=1,reward=Reward},reward_level=Level},
	ets:insert(?ETS_HOME_BOSS_TABLE,Now),
	Now.

gen_next_boss(RoleID,TarRoleHomeBoss) ->
	#home_boss{roleID=TarRoleID} = TarRoleHomeBoss,
	#rolePublic{roleName=Name} = role_lib:get_rolePublic(TarRoleID),
	#home_boss{self=Self,last_boss=ID,level=Level0}=RoleHomeBoss = get_role_home_boss(RoleID),
	Level = Level0+1,
	ActiveTime = util:now()+data_home_boss:get(active_interval),
	New=RoleHomeBoss#home_boss{self=Self#boss_dtl{isGetReward=2,own=TarRoleID,activeTime=ActiveTime,ownName=Name},level=Level},
	{Team1,Team2} = data_home_boss:get({boss,ID,Level}),
	{Boss,Mons} = gen_boss(Team1,Team2),
	FullBlood = calc_blood(Mons),
			#rolePublic{isMale=IsMale} = role_lib:get_rolePublic(RoleID),
	{RewardBase,{Num,T1,T2}} = data_home_boss:get({boss_from_reward,ID,Level}),
	Reward = if IsMale -> role_homeBoss:add_reward(RewardBase,Num,T1);true -> role_homeBoss:add_reward(RewardBase,Num,T2) end,
	OtherBoss = #boss_dtl{display=ID,boss=Boss,activeTime=ActiveTime,fullBlood=FullBlood,chapter=Level
						,nowBlood=FullBlood,reward=Reward,isGetReward=0,state=?BOSS_STATE_1,from=RoleID
						,own=TarRoleID,isRead=0,ownName=Name},
	%TarRoleHomeBoss = get_role_home_boss(TarRoleID),
	TarRoleHomeBoss2 =
		case RoleID of
			TarRoleID -> 
				?unicast(RoleID,#sc_homeBoss_info{myBoss=homeBoss2p_boss(New#home_boss.self),ownBoss=homeBoss2p_boss(OtherBoss)}),
				New#home_boss{own=OtherBoss}; 
			_ ->
				?unicast(RoleID,#sc_homeBoss_info{myBoss=homeBoss2p_boss(New#home_boss.self),ownBoss=homeBoss2p_boss(New#home_boss.own)}),
				?unicast(TarRoleID,#sc_homeBoss_info{myBoss=homeBoss2p_boss(TarRoleHomeBoss#home_boss.self),ownBoss=homeBoss2p_boss(OtherBoss)}),
				ets:insert(?ETS_HOME_BOSS_TABLE,New),
				TarRoleHomeBoss
		end,
	
	NewTarRoleHomeBoss = TarRoleHomeBoss2#home_boss{own=OtherBoss},
	ets:insert(?ETS_HOME_BOSS_TABLE,NewTarRoleHomeBoss).


check_attack(_SelfRoleID,RoleID,Type) ->
	#home_boss{self=Self,own=Own}=get_role_home_boss(RoleID),
	Now = util:now(),
	case Type of
		1 ->
			case Self#boss_dtl.activeTime < Now of
				true ->
					case Self#boss_dtl.nowBlood =< 0 of true -> {false,2};
						_ -> true
					end;
				false ->
					{false,3}
			end;
		2 ->
			case Own of
				0 -> {false,2};
				_ ->
					case Own#boss_dtl.activeTime < Now of
						true ->
							case Own#boss_dtl.nowBlood =< 0 of true -> false;
								_ -> true
							end;
						false ->
							{false,2}
					end
			end
	end.

attack_homeBoss(SelfRoleID,RoleID,Type,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo) ->
	erlang:send(?MODULE,{attack_homeBoss,SelfRoleID,RoleID,Type,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo}).

attack_boss(SelfRoleID,RoleID,1,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo)->
	#home_boss{self=Boss,reward_level=RewardLevel0,level=Level} = RoleHomeBoss = get_role_home_boss(RoleID),
	{BLeader,BFellow}=Boss#boss_dtl.boss,
	LastBlood = Boss#boss_dtl.nowBlood,
	case LastBlood =< 0 of
		true -> ?unicast(SelfRoleID,#sc_homeBoss_attack{result=2,replay=[],nowBoss=[]});
		_ ->	SelfTeam = #double_team{skin_info=SkinInfo,talent=TalentList,trainer=TrSpe,fighters=RoleFighterList,lieu=RoleLieuAdd},
				{IsWin,FightRecord,BLeader2,BFellow2,NowBlood,DD} = attack_boss(BLeader,BFellow,SelfTeam,LastBlood,Boss#boss_dtl.dead_list),
				if IsWin ->
					   TrueReward=data_home_boss:get(kill_reward),
					   ?CATCH(role_lib:send_server(SelfRoleID,{route,role_homeBoss,{kill_reward,TrueReward}})),
					   Boss2=Boss#boss_dtl{isGetReward=1,boss={BLeader2,BFellow2},nowBlood=0,isRead=1,dead_list=[]},
					   RewardLevel=Level;
				   true ->
					   TrueReward = data_home_boss:get(attack_reward),
					   ?CATCH(role_lib:send_server(SelfRoleID,{route,role_homeBoss,{attack_reward,TrueReward}})),
					   Boss2=Boss#boss_dtl{boss={BLeader2,BFellow2},nowBlood=NowBlood,isRead=1,dead_list=DD},
					   RewardLevel=RewardLevel0
				end,
				RoleHomeBoss2 = RoleHomeBoss#home_boss{self=Boss2,reward_level=RewardLevel},
				ets:insert(?ETS_HOME_BOSS_TABLE,RoleHomeBoss2),
				?unicast(SelfRoleID,#sc_homeBoss_attack{result=1,replay=[FightRecord],nowBoss=[homeBoss2p_boss(Boss2)],reward=[activity_server:sell_reward2p_reward_info(TrueReward)]})
	end;
attack_boss(SelfRoleID,RoleID,2,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo)->
	#home_boss{own=Boss,machineBuff=_MachineBuff} = RoleHomeBoss=get_role_home_boss(RoleID),
	{BLeader,BFellow}=Boss#boss_dtl.boss,
	LastBlood = Boss#boss_dtl.nowBlood,
	case LastBlood =< 0 of
		true -> ?unicast(SelfRoleID,#sc_homeBoss_attack{result=2,replay=[],nowBoss=[]});
		_ ->	SelfTeam = #double_team{skin_info=SkinInfo,talent=TalentList,trainer=TrSpe,fighters=RoleFighterList,lieu=RoleLieuAdd},
				{IsWin,FightRecord,BLeader2,BFellow2,NowBlood,DD} = attack_boss(BLeader,BFellow,SelfTeam,LastBlood,Boss#boss_dtl.dead_list),
				if IsWin -> 
					   FromRoleID = Boss#boss_dtl.from,
					   #home_boss{last_boss=BossID} = get_role_home_boss(FromRoleID),
					   send_mail_reward(SelfRoleID,FromRoleID,RoleID,BossID),N2 = mark_self_reward(FromRoleID),
					   Boss2=0,
					   Boss3 = Boss#boss_dtl{nowBlood=0},
					   TrueReward=data_home_boss:get(kill_reward),
					   ?CATCH(role_lib:send_server(SelfRoleID,{route,role_homeBoss,{kill_reward,TrueReward}})),
					   %NewMachineBuff0=MachineBuff+data_home_boss:get(reward_homestead),
					   MaxBuff = data_home_boss:get(max_add),
					   case FromRoleID of RoleID -> 
											  ?ERR("id same:~w,~w~n~w~n~w",[FromRoleID,RoleID,N2,RoleHomeBoss]),
											  NewSelf =  N2#home_boss.self,NewRewardLevel=N2#home_boss.reward_level;
						   _ -> NewSelf = RoleHomeBoss#home_boss.self,NewRewardLevel=RoleHomeBoss#home_boss.reward_level end;
				   %NewMachineBuff = if NewMachineBuff0> MaxBuff -> MaxBuff; true-> NewMachineBuff0 end;%Boss#boss_dtl{isGetReward=1,boss={BLeader2,BFellow2},nowBlood=0};
				   true ->
					   TrueReward = data_home_boss:get(attack_reward),
					   ?CATCH(role_lib:send_server(SelfRoleID,{route,role_homeBoss,{attack_reward,TrueReward}})),
					   NewSelf=RoleHomeBoss#home_boss.self,
					   NewRewardLevel=RoleHomeBoss#home_boss.reward_level,
					   Boss2=Boss#boss_dtl{boss={BLeader2,BFellow2},nowBlood=NowBlood,isRead=1,dead_list=DD},
					   %NewMachineBuff=MachineBuff,
					   Boss3=Boss2
				end,
				RoleHomeBoss2 = RoleHomeBoss#home_boss{self=NewSelf,own=Boss2,machineBuff=0,reward_level=NewRewardLevel},
				ets:insert(?ETS_HOME_BOSS_TABLE,RoleHomeBoss2),
				?unicast(SelfRoleID,#sc_homeBoss_attack{result=1,replay=[FightRecord],nowBoss=[homeBoss2p_boss(Boss3)],reward=[activity_server:sell_reward2p_reward_info(TrueReward)]})
	end. 

attack_boss(BLeader=#double_team{fighters=LeaderFighters},BFellow=#double_team{fighters=FellowFighters},SelfTeam,Blood,DeadList) -> 
	Ref = erlang:make_ref(),
	double_fight:new(SelfTeam,#double_team{},BLeader,BFellow,DeadList,self(),Ref,false),
	receive
		{fight_result,Ref,FightResult} ->
			{IsWin,FightRecord,{_SrcGerStateList,DGerStateList,_SrcGerList,_DGerList,_DA,DD}} = FightResult,
			LeaderFighters2 = lists:foldl(fun(#ger{gerID=GerID,gerAttr=#gerAttr{gerHpMax=HMX}}=Ger,Acc)-> case lists:keyfind(GerID,1,DGerStateList) of {_,Hp,_} when Hp>0 ->[Ger#ger{gerHp=min_hp(Hp,HMX)}|Acc];_ -> Acc end end,[],LeaderFighters),
			FellowFighters2 = lists:foldl(fun(#ger{gerID=GerID,gerAttr=#gerAttr{gerHpMax=HMX}}=Ger,Acc)-> case lists:keyfind(GerID,1,DGerStateList) of {_,Hp,_} when Hp>0 ->[Ger#ger{gerHp=min_hp(Hp,HMX)}|Acc];_ -> Acc end end,[],FellowFighters),
			NowBlood = lists:foldl(fun(#ger{gerHp=H},Acc) -> H+Acc end, 0, LeaderFighters2 ++ FellowFighters2),
			%DDL = lists:foldl(fun(#ger{gerID=GerID},Acc) -> case lists:keyfind(GerID,2,DD) of {Pos,GerID}=X ->[X|Acc]; _ -> Acc end end,[],LeaderFighters),
			%DDF = lists:foldl(fun(#ger{gerID=GerID},Acc) -> case lists:keyfind(GerID,2,DD) of {Pos,GerID}=X ->[X|Acc]; _ -> Acc end end,[],FellowFighters),
			%?ERR("blood:~w,~w,~n~w~n~w~n~w~n~w",[Blood,NowBlood,LeaderFighters,FellowFighters,LeaderFighters2,FellowFighters2]),
			{IsWin,FightRecord,BLeader#double_team{fighters=LeaderFighters2},BFellow#double_team{fighters=FellowFighters2},NowBlood,DD}
	    after 10000 ->
			{false,[],BLeader,BFellow,Blood,DeadList}
	end.

min_hp(Hp,MaxHp) when Hp>MaxHp -> MaxHp;
min_hp(Hp,_) -> Hp.

%send_mail_reward(SelfRoleID,SelfRoleID) -> ignore;
send_mail_reward(SelfRoleID,FromRoleID,RoleID,BossID) ->
	OwnReward1 = util:random_one_from_weigh_list(data_home_boss:get({boss_owner_reward,1})),
	OwnReward2 = util:random_one_from_weigh_list(data_home_boss:get({boss_owner_reward,2})),
	OwnReward = merge_reward(OwnReward1,OwnReward2),
	#rolePublic{roleName=Name1} = role_lib:get_rolePublic(FromRoleID),
	if FromRoleID == SelfRoleID -> Name2 = Name1;true -> #rolePublic{roleName=Name2} = role_lib:get_rolePublic(SelfRoleID) end,
	mail_server:send_sys_mail(RoleID, BossID + ?MAIL_HOME_BOSS_FROM_REWARD, [Name1,Name2], "", OwnReward).

merge_reward(R1,R2) ->
	#sell_reward{coin=R1#sell_reward.coin + R2#sell_reward.coin
				,roleExp=R1#sell_reward.roleExp + R2#sell_reward.roleExp
				,gerExp=R1#sell_reward.gerExp + R2#sell_reward.gerExp
				,gold=R1#sell_reward.gold + R2#sell_reward.gold
				,item=R1#sell_reward.item ++ R2#sell_reward.item
				,reputation = R1#sell_reward.reputation + R2#sell_reward.reputation
				,newGer = R1#sell_reward.newGer ++ R2#sell_reward.newGer
				 }.

			