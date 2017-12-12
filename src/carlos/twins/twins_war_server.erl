%% @author crimoon-17
%% @doc Add description to war_server.
-module(twins_war_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_carlos.hrl").
-include("def_mail.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).

-record(state, {warID=0,warStartTime=0,type=0}).
-record(tw_mine, {id=0,pos={0,0},players=[],startSec=0,bossType=0,attr=0,movTarget=0,speed=1
				  ,boss=[],maxHp=0,nowHp=0,blood=0,tr=#trSpecial{},state=0,hpLimit=-1}).
-record(tw_home, {id=0, pos={0,0},type=0,mineIDs=[]}).

-record(plan, {sec=0,key=0,value=0}).

-define(player, player).
-define(mine, mine).
-define(home,home).
-define(plan,plan).
-define(bcList,bcList).
-define(endInfo, endInfo).
-define(bc_mark,bc_mark).
-define(mark_bc,mark_bc).
-define(talk_data,talk_data).
-define(rank_data, rank_data).
-define(mov_plan,mov_plan).
-define(boss,boss).
-define(war_state,war_state).
-define(twins_type,twins_type).
-define(exchange, exchange).
-define(mark_dead,mark_dead).
-define(killed_boss,killed_boss).
-define(boss2_exchange,boss2_exchange).
-define(pd_operate_ts, pd_operate_ts).

-define(STATE_DEFAULT,0).

-define(STATE_READY,1).
-define(STATE_DEAD,2).
-define(STATE_INV, 3).%无敌
-define(STATE_PRE, 4).%隐藏

-define(STATE_BOSS1,1).%小boss状态
-define(STATE_BOSS2,2).% 大boss状态
-define(STATE_WAIT,3).% 无敌等待切换

-define(MAX_BOSS1_ID,5).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).
start_link(WarID,Players,Type,Level) ->
	gen_server:start_link(?MODULE,[WarID,Players,Type,Level],[]).
start(Args) ->
		supervisor:start_child(twins_war_sup,Args).

init([WarID,Players,Type,Level]) ->
	random:seed(util:gen_random_seed()),
	add_ets_war_info(WarID,self()),
	init_players(Players),
	init_bcList(Players),
	init_ground(Type,Level),
	tick_plan(),
	bc_open(),
	set_end_info(WarID),
	check_plan(),
	set_type({Type,Level}),
	{ok,#state{warID=WarID,warStartTime=util:now(),type=Type}}.

set_type(Type)->
	put(?twins_type,Type).
get_type()->
	get(?twins_type).

init_bcList(Players) ->
	RL=[{RoleID,ServerID}||#tw_player{roleID=RoleID,serverID=ServerID}<-Players],
	put(?bcList, RL).

get_bcList()->
	case get(?bcList) of
		?undefined ->
			[];
		X ->
			X
	end.

set_end_info(WarID) ->
	Interval = data_twins:get(war_interval),
	put(?endInfo,{WarID,util:now()+Interval}),
	erlang:send_after(Interval * 1000, self(), do_end_war),
	ok.

tick_plan()->
	erlang:send_after(1000, self(), tick_plan).

bc_open()->
	erlang:send(self(), {bc_info, open}).

add_ets_war_info(WarID,PID) -> 
	ets:insert(?ETS_TWINS_INFO,#ets_twins_info{warID=WarID,pid=PID}).

init_players(Players) ->
	Pos = data_twins:get(born_pos),
	[set_player(RoleID,ServerID,Info#tw_player{startPos=Pos})
	||#tw_player{roleID=RoleID,serverID=ServerID}=Info<-Players].

init_ground(Type,Level) ->
	Now = util:now(),
	Speed = data_twins:get({mine_speed,Level}),
	{BossType1,BossType2} = get_boss_type_list(Type),
	init_boss(BossType1,?STATE_READY,1,1,Level,Now,Speed),
	init_boss(BossType2,?STATE_PRE,5,2,Level,Now,0),
	init_homeinfo(),
	put(?boss2_exchange, 1),
	set_war_state(?STATE_BOSS1).

init_homeinfo()->
	HomeList = data_twins:get(home_list),
	VA = [begin {Pos,_} = data_twins:get({home,ID}),#p_twins_home{homeID=ID,pos=pos2twinsPos(Pos)} end||ID<-HomeList],
	put(?home,VA).

init_boss(BossType,STATE,BaseID,T,Level,Now,Speed)->
	EID = 
		lists:foldl(fun(BossX,HIDACC) ->
							{SpecialID0,BossID,Trainer} =
								case BossX of {DsID,SpID,Ty}->{SpID,Ty,#trSpecial{trID=DsID,specialID=SpID}};
									_ -> {0,BossX,#trSpecial{}} 
								end,
							case T of
									   1 -> {H1,_} = data_twins:get({bossList,BossID}),
											{BossTypeID,M1,M2,M3,M4,M5,M6} = util:random_one_from_list(H1);
									   2 -> {_,H2} = data_twins:get({bossList,BossID}),
											{_,M1,M2,M3,M4,M5,M6} = util:random_one_from_list(H2),
											BossTypeID = SpecialID0
								   end,
							{Pos,NextHID} = data_twins:get({home,HIDACC}) ,
							{MaxHp,Boss,TrS} = gen_boss([M1,M2,M3,M4,M5,M6],Trainer,Level),
							Mine = #tw_mine{id=HIDACC,pos=Pos,bossType=BossTypeID,attr=BossID,movTarget=NextHID,blood=100
										   ,speed=Speed,state=STATE,boss=Boss,maxHp=MaxHp,nowHp=MaxHp,tr=TrS,startSec=Now},
							put({?mine,HIDACC},Mine),
							HIDACC+1
					end, BaseID, BossType),
	put({?boss,T},lists:seq(BaseID,EID-1)).

gen_boss(Mons,Trainer,FLevel) ->
	{_,_,{Level,Rank,TrainerLevel}} = data_twins:get({fight_level,FLevel}),
	{_,MaxHp,GerList} = 
	lists:foldl(fun(0,Acc) ->  	Acc;
				   (GerTypeID,{Pos,AccHp,AccList}) ->
						Boss0 = ger_attr:new_ger(GerTypeID, Level, Rank, [], []), 
						Boss  = Boss0#ger{gerBase=((Boss0#ger.gerBase)#gerBase{gerPos=Pos})},
						{Pos+1,AccHp + Boss#ger.gerHp,[Boss|AccList]}
				end,{1,0,[]},Mons),
	{MaxHp,GerList,Trainer#trSpecial{roleLevel=TrainerLevel}}.

get_boss_type_list(Type) ->
	{{D1,L1,S1},{D2,L2,S2}} = data_twins:get({attr_type,Type}),
	BL = [{D1,S1,util:random_one_from_list(L1)},{D2,S2,util:random_one_from_list(L2)}],
	{util:random_list2(L1++L2),BL}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(stop,State) ->
	{stop,normal, State};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_handle_info({bc_info,Type}) ->
	bc_info(Type),
	ok;
do_handle_info(tick_plan) ->
	tick_plan(),
	plan(),
	ok;
do_handle_info(do_end_war) ->
	end_war(),
	ok;
do_handle_info(change_state) ->
	change_state(get_war_state()),
	ok;
do_handle_info({cs_twins_war_base_info, {_,RoleID,ServerID}}) ->
	Msg = get_war_base_info(),
	twins_router:send_client(ServerID,RoleID,Msg),
	ok;
do_handle_info({cs_twins_self, {_,RoleID,ServerID}}) ->
	#tw_player{trSpecial=Special}=Player = get_player(RoleID,ServerID),
	#trSpecial{specialID=MySpecial} = Special,
	Msg = #sc_twins_self{self=player2twinsPlayerS(Player),special=MySpecial},
	twins_router:send_client(ServerID,RoleID, Msg),
	ok;

					
do_handle_info({{cs_twins_mov, MineID}, {_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	case get_war_state() of
		?STATE_WAIT ->
			ok;
		_ ->
			#tw_player{tarMineID=TarMineID,startPos=Pos} = Player = get_player(RoleID,ServerID),
			IsMov = case TarMineID of
						MineID-> false;
						0 -> true;
						_ -> case check_mine_state(MineID) of true -> reset;	_ -> false end
					end,
			Msg = %% 如果原来有目标mine，则更新目标mine信息并开始移动，否则直接开始移动
				case IsMov of
					false ->
						#sc_twins_mov{result=2};
					reset ->
						#tw_mine{pos=StartPos}=TarMine= get_mine(TarMineID),
						TarMineInfo3 = delete_mine_player({RoleID,ServerID},TarMine),
						Player2 = reset_player(Player),
						set_mine(TarMineID,TarMineInfo3),
						Player3 = case Pos of %% 如果在mine上，则取得mine的pos为startPos，否则为reset得到的pos为startPos
									  {0,0} ->
										  mark_bc(TarMineID,{RoleID,ServerID}),
										  PlayerT = update_player(Player2,StartPos,MineID),
										  PlayerT#tw_player{tarMineID=MineID};
									  _ ->
										  mark_bc([],{RoleID,ServerID}),
										  add_mov_plan(RoleID,ServerID),
										  Player2#tw_player{tarMineID=MineID}
								  end,
						set_player(RoleID,ServerID,Player3),
						#sc_twins_mov{result=1};
					_ ->
						Player2 = update_player(Player,Pos,MineID),
						set_player(RoleID,ServerID,Player2),
						mark_bc([],{RoleID,ServerID}),
						#sc_twins_mov{result=1}
				end,
			twins_router:send_client(ServerID,RoleID,Msg)
	end,
	ok;

do_handle_info({{cs_twins_attack,MineID},{_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	case get_war_state() of
		?STATE_WAIT ->
			ok;
		_ ->
			#tw_player{fighters=Fighters,talent=Talent,trSpecial=TrSpecial,skin_info=FSkin
					   ,itemList=ItemList,baseFighters=BaseFighters,addBuff=LieuAdd}=Player=get_player(RoleID,ServerID),
			#tw_mine{boss=Boss,state=State,attr=Attr,nowHp=CurHp,tr=BossTr,maxHp=MaxHp,hpLimit=HpLimit} = Mine = get_mine(MineID),
			BSkin = #skin_info{},
			{IsSucc,Result,FightInfo,RewardX} = 
				case check_mine_player({RoleID,ServerID},Mine) of
					true ->
						case State of
							?STATE_INV ->
								{false,4,[],[]};
							?STATE_DEAD ->
								{false,5,[],[]};
							?STATE_PRE ->
								{false,8,[],[]};
							_ ->
								#trSpecial{specialID=SpecialID} = TrSpecial,
								DamageReduce = 
									case is_same_attr(Attr,SpecialID) of
										true ->
											0;
										_ ->
											data_twins:get(damage_reduce)
									end,
										case catch(do_fight(Boss,CurHp,filter_out_zero_hp(Fighters),LieuAdd,Talent,TrSpecial,BossTr,FSkin,BSkin,HpLimit,DamageReduce)) of
											{_Result1,BossNewCurHp,NewBossList,FightRecord0,NewFighterList0} ->
												FighterList2 = role_data:get_FighterList_with_effect(ItemList,[],FightRecord0#sc_fight_request.fighterList),
												FightRecord = FightRecord0#sc_fight_request{fighterList=FighterList2},
												NewFighterList = recalc_fighter_list(Fighters,NewFighterList0),
												Blood = calc_hp_percent(NewFighterList,BaseFighters),
												{Mine2,Player2} =
													if Blood == 0 ->
														   {delete_mine_player({RoleID,ServerID},Mine),reborn_player_now(Player)};
													   true ->
														   {Mine,update_player_fighters(Player,Blood,NewFighterList)}
													end,
												NewScore = Player#tw_player.score + max(0, Mine#tw_mine.nowHp - BossNewCurHp),
												set_player(RoleID,ServerID,Player2#tw_player{score=NewScore}),
												BossBlood = calc_hp_percent2(BossNewCurHp,MaxHp),
												NewMine = Mine2#tw_mine{nowHp=BossNewCurHp,blood=BossBlood,boss=NewBossList},
												set_mine(MineID,NewMine),
												Reward = data_twins:get(normal_fight_reward),
												send_msg:direct(ServerID,twins_server,{fight_reward, RoleID, Reward}),
												do_trigger_next(MineID,BossNewCurHp,HpLimit),
												mark_bc(MineID,{RoleID,ServerID}),
												sync_role(RoleID,ServerID),
												{true,1,[FightRecord],[activity_server:sell_reward2p_reward_info(Reward)]};
%% 											_ ->
%% 												{false,5,[],[]}
%% 										end;
									_ ->
										{false,7,[],[]}
								end
						end;
					_ ->
						{false,3,[],[]}
				end,
			if IsSucc ->
				   bc_info({boss,MineID});
			   true ->
				   ignore
			end,
			twins_router:send_client(ServerID,RoleID,#sc_twins_attack{result=Result,fightInfo=FightInfo,reward=RewardX})
	end,
	ok;

do_handle_info({{cs_twins_role_dtl, TarRoleID,TarServerID},{_,RoleID,ServerID}})->
	#tw_player{fighters=F,grade=Grade} = get_player(TarRoleID,TarServerID),
	FL = fairy2twinsFairy(F),
	twins_router:send_client(ServerID,RoleID,#sc_twins_role_dtl{target=FL,grade=Grade}),
	ok;
do_handle_info({cs_twins_mov_stop,{_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	#tw_player{tarMineID=TarMineID,startPos=StartPos}=Player = get_player(RoleID,ServerID),
	case TarMineID of
		0 ->
			ignore;
		_ ->
			cancel_mov_plan(RoleID,ServerID),
			case StartPos of
				{0,0} ->
					ignore;
				_ ->
					Player2 = reset_player(Player),
					set_player(RoleID,ServerID,Player2#tw_player{tarMineID=0})
			end,
			mark_bc([],{RoleID,ServerID})
	end,
	ok;
do_handle_info({{cs_twins_talk,Data}, {_,RoleID,ServerID}}) ->
	get_player(RoleID, ServerID),
	add_talk_data(Data),
	do_bc(talk,#sc_twins_talk{data=Data},twins_bc_info),
	ok;
do_handle_info({cs_twins_get_talk, {_,RoleID,ServerID}}) ->
	TalkData = get_talk_data(),
	twins_router:send_client(ServerID,RoleID,#sc_twins_get_talk{data=TalkData}),
	ok;
do_handle_info({cs_twins_get_rank, {_,RoleID,ServerID}}) ->
	RankList = get_rank_data(),
	twins_router:send_client(ServerID,RoleID, #sc_twins_get_rank{rank=RankList}),
	ok;
do_handle_info(check_tick) ->
	case get_war_state() of
		?STATE_BOSS1 ->	[update_mine_pos(MineID)||MineID<-get({?boss,1})];
		_ -> ignore
	end,
	update_players_pos(),
	check_plan(),
	ok;

do_handle_info(do_exchange_boss2) ->
	do_exchange_boss2();

do_handle_info(Info) ->
	?ERR("error get info:~w",[Info]).

bool2int(true ) ->
	1 ;
bool2int(_) ->
	0.

end_war()->
	case check_mine_all_dead() of
		true ->	end_reward();
		_ -> mail_fail()
	end,
	stop().

end_reward()->
	{Type,Level} = get_type(),
	Players = lists:foldl(fun({{?player,_},Player},Acc) ->[Player|Acc];
							 (_,Acc) ->Acc
						  end,[],erlang:get()),
	Players2 = lists:reverse(lists:keysort(#tw_player.score,Players)),
	WinReward = role_twins:merge_reward(data_twins:get({win_reward,Level}),get_boss_drop(Type,Level)),
	RankData = get_rank_data(),
    AfkLimit = util:now() - data_twins:get(afk_check_time),
	lists:foldl(fun(#tw_player{roleID=RoleID,serverID=ServerID,movDis=MovDis},Count)->
                          AfkTime = get({?pd_operate_ts,RoleID}),
                          if 
                              AfkTime /= ?undefined andalso AfkTime >= AfkLimit  ->
							   send_msg:direct(ServerID,twins_server,{twins_box_reward,RoleID,get_reward_rank(Count),Type,Level,RankData}),
							   send_msg:direct(ServerID,twins_server,{twins_reward,RoleID,?MAIL_TEMPLATE_TWINS_WIN,WinReward,Level,{1,RankData}});
						   true ->
							   send_msg:direct(ServerID,twins_server,{twins_reward,RoleID,?MAIL_TEMPLATE_TWINS_KICK,[],Level,{2,RankData}})
						end,
						Count+1
				end,1,Players2).

get_reward_rank(X) when X < 3 -> 1;
get_reward_rank(X) when X < 5 -> 2;
get_reward_rank(X) when X < 7 -> 3;
get_reward_rank(X) when X < 9 -> 4;
get_reward_rank(_) -> 5.

get_boss_drop(Type,Level) ->
	R1 = data_twins:get({boss_drop,Type}),
	N = data_twins:get({drop_num, Level}),
	[I#new_item{itemNum=N}||I<-R1].

mail_fail() ->
	{_Type,Level} = get_type(),
    AfkLimit = util:now() - data_twins:get(afk_check_time),
    RankData = get_rank_data(),
	lists:foreach(fun({{?player,_},#tw_player{roleID=RoleID,serverID=ServerID}}) ->
                          AfkTime = get({?pd_operate_ts,RoleID}),
                          if 
                              AfkTime /= ?undefined andalso AfkTime >= AfkLimit  ->
						          send_msg:direct(ServerID,twins_server,{twins_reward,RoleID,?MAIL_TEMPLATE_TWINS_FAIL,[],Level,{2,[]}});
                              true ->
                                  send_msg:direct(ServerID,twins_server,{twins_reward,RoleID,?MAIL_TEMPLATE_TWINS_KICK,[],Level,{2,RankData}})
                          end;
					 (_) ->
						  ignore
				  end,erlang:get()).

stop_mine(MineID)->
	#tw_mine{players=Players} = Mine = get_mine(MineID),
	#tw_mine{pos=MinePos}=Mine2 = update_mine_pos2(Mine),
	reset_player5(Players,MinePos),
	reset_other_players(MineID,MinePos),
	%set_mine(MineID,Mine2#tw_mine{startSec=0,speed=0,movTarget=0,players=[],state=?STATE_DEAD}).
	set_mine(MineID,Mine2#tw_mine{state=?STATE_DEAD,players=[]}).

do_trigger_next(MineID,Blood,HpLimit) ->
	case get_war_state() of
		0 ->
			ignore;
		?STATE_BOSS1 ->
			case Blood of
				0 ->
					stop_mine(MineID),
					do_trigger_next2(MineID);
				_ ->
					ignore
			end;
		?STATE_BOSS2 ->
			case Blood of
				0 ->
					mark_mine_dead(MineID),
					case check_mine_all_dead() of
						true -> end_war();
						_ -> ignore
					end;
				HpLimit ->
					mark_mine_inv(MineID),
					mark_exchange(MineID),
					case check_mark_exchange() of
						true ->	exchange_boss2();
						false -> ignore
					end;
				_ ->
					ignore
			end
	end.

mark_mine_inv(MineID) ->
	Mine = get_mine(MineID),
	set_mine(MineID,Mine#tw_mine{state=?STATE_INV}),
	mark_bc(MineID,[]).

mark_mine_dead(MineID)->
	put({?mark_dead, MineID}, true),
	#tw_mine{players=Players} = Mine = get_mine(MineID),
	#tw_mine{pos=MinePos}=Mine2 = update_mine_pos2(Mine),
	reset_player5(Players,MinePos),
	reset_other_players(MineID,MinePos),
	set_mine(MineID,Mine2#tw_mine{startSec=0,speed=0,movTarget=0,players=[],state=?STATE_DEAD}),
	mark_bc(MineID,[]).

check_mine_all_dead()->
	[Boss1,Boss2] = get({?boss,2}),
	MB1 = get({?mark_dead,Boss1}),
	MB2 = get({?mark_dead,Boss2}),
	MB1 == true andalso MB2 == true.

mark_exchange(MineID) ->
	put({?exchange,MineID},true).

check_mark_exchange()->
	[Boss1,Boss2] = get({?boss,2}),
	MB1 = get({?exchange,Boss1}),
	MB2 = get({?exchange,Boss2}),
	MB1 == true andalso MB2 == true.

calc_hp_limit(_,HpR) when HpR == 0 -> 0;
calc_hp_limit(Hp,HpR) -> trunc(Hp * HpR / 100)+1.

exchange_boss2()->
	Diff = data_twins:get(inv_exchange),
	[Boss1,Boss2] = get({?boss,2}),
	#tw_mine{pos=Pos1,players=Players1} = Mine1 = get_mine(Boss1),
	#tw_mine{pos=Pos2,players=Players2} = Mine2 = get_mine(Boss2),	
	[begin
		 P = get_player(RoleID,ServerID),
		 set_player(RoleID,ServerID,P#tw_player{tarMineID=0,startPos=Pos1})
	 end||{RoleID,ServerID}<-Players1],
	[begin
		 P = get_player(RoleID,ServerID),
		 set_player(RoleID,ServerID,P#tw_player{tarMineID=0,startPos=Pos2})
	 end||{RoleID,ServerID}<-Players2],
	sync_role2(Players1++Players2),
	Mine1_2 = Mine1#tw_mine{players=[]},
	Mine2_2 = Mine2#tw_mine{players=[]},
	set_mine(Boss1,Mine1_2),
	set_mine(Boss2,Mine2_2),
	erlang:send_after(Diff*1000, self(), do_exchange_boss2).

do_exchange_boss2()->
	[Boss1,Boss2] = get({?boss,2}),
	erase({?exchange,Boss1}),
	erase({?exchange,Boss2}),
	HpR = get_hp_limit(),
	#tw_mine{pos=Pos1,maxHp=MaxHp1,movTarget=MTG1} = Mine1 = get_mine(Boss1),
	#tw_mine{pos=Pos2,maxHp=MaxHp2,movTarget=MTG2} = Mine2 = get_mine(Boss2),	
	Mine1_2 = Mine1#tw_mine{pos=Pos2,players=[],hpLimit =calc_hp_limit(MaxHp1,HpR),state=?STATE_READY,movTarget=MTG2},
	Mine2_2 = Mine2#tw_mine{pos=Pos1,players=[],hpLimit =calc_hp_limit(MaxHp2,HpR),state=?STATE_READY,movTarget=MTG1},
	set_mine(Boss1,Mine1_2),
	set_mine(Boss2,Mine2_2),
	Now = util:now(),
	sync_mines2([Mine1_2#tw_mine{startSec=Now},Mine2_2#tw_mine{startSec=Now}]),
	update_player_mov_ex(Boss1,Boss2).

get_killed_boss1()->
	case get({?killed_boss,1}) of
		X when is_list(X) -> X;
		_ -> []
	end.

do_trigger_next2(MineID)->
	Boss1List = get({?boss,1}),
	KB = lists:sort([MineID|get_killed_boss1()]),
	case KB of
		Boss1List ->
			erlang:send_after(3000,self(),change_state);
			%change_state(get_war_state());
		_ ->
			put({?killed_boss,1},KB)
	end.

change_state(?STATE_BOSS1)->
	set_war_state(?STATE_WAIT),
	BornPos = data_twins:get(born_pos),
	SynRoleL = lists:foldl(fun({{?player,{RoleID,ServerID}},Player},Acc)->
						  Player2 = Player#tw_player{startPos=BornPos,tarMineID=0,startTime=0},
						  set_player(RoleID,ServerID,Player2),
						[{RoleID,ServerID}|Acc];
					 (_,Acc) ->
						  Acc
				  end,[],erlang:get()),
	sync_role2(SynRoleL),
	set_mov_plan([]),
	Diff = data_twins:get(boss_change_time),
	erlang:send_after(Diff*100, self(), change_state),
	Boss1 = get({?boss,1}),
	Boss2 = get({?boss,2}),
	HpR = get_hp_limit(),
	lists:foreach(fun(BossID0)-> 
						  Mine0 = get_mine(BossID0),
						  set_mine(BossID0,Mine0#tw_mine{state=?STATE_PRE,startSec=0,speed=0,movTarget=0,players=[]})
						  end, Boss1),
	lists:foreach(fun(BossID)-> 
						  #tw_mine{maxHp=MaxHp} = Mine = get_mine(BossID),
						  set_mine(BossID,Mine#tw_mine{state=?STATE_READY,speed=0,hpLimit=calc_hp_limit(MaxHp,HpR),startSec=0})
						  end, Boss2),
	sync_mines(Boss1),
	sync_mines(Boss2);
change_state(?STATE_WAIT) ->
	set_war_state(?STATE_BOSS2).

get_hp_limit()->
	L = data_twins:get(exchange_blood),
	N = get(?boss2_exchange),
	put(?boss2_exchange, N + 1),
	lists:nth(N,L).

get_talk_data() ->
	case get(?talk_data) of
		?undefined ->
			[];
		X ->
			X
	end.
set_talk_data(Data) ->
	put(?talk_data , Data).
add_talk_data(Data) ->
	Len = data_twins:get(talk_len),
	OL = get_talk_data(),
	L2 = lists:sublist([Data|OL], Len),
	set_talk_data(L2).

get_rank_data() ->
	case get(?rank_data) of
		?undefined ->
			[];
		X ->
			X
	end.
set_rank_data(Data) ->
	put(?rank_data, Data).
update_player_rank_data(#tw_player{roleID=RoleID,serverID=ServerID}=Player)->
	Data = get_rank_data(), 
	Data2 = [E||#p_twins_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
	Data3 = [player2p_twins_rank_dtl(Player)|Data2],
	set_rank_data(Data3).
update_player_rank_data2(#tw_player{roleID=RoleID,serverID=ServerID}=Player)->
	Data = get_rank_data(), 
	Data2 = [E||#p_twins_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
	Data3 = [player2p_twins_rank_dtl2(Player)|Data2],
	set_rank_data(Data3).

stop() ->
	{WarID,_} = get(?endInfo),
	do_bc(WarID,update_twins_war),
	twins_war_manager_server:war_stoped(WarID),
	erlang:send(self(), stop).

is_same_attr(_,0) -> false;
is_same_attr(Attr,SpecialID) ->
	ID2 = get_attr(SpecialID),
	lists:member(Attr,ID2).


calc_hp_percent(FighterList,BaseFighter) when is_list(FighterList) andalso is_list(BaseFighter)->
    SumHP = lists:sum([HP1||#ger{gerHp=HP1}<-FighterList]),
    SumHPMax = lists:sum([HP2||#ger{gerHp=HP2}<-BaseFighter]),
	B = erlang:trunc((SumHP * 100) / SumHPMax ),
	if SumHP =< 0 ->   0;
	   true ->  if B>0->B;
				   true -> 1
		        end
	end.
calc_hp_percent2(B1,B2) ->
	P = erlang:trunc((B1 * 100) / B2) ,
	if B1 =< 0 -> 0;
	   true -> if P > 0 -> P;
				  true -> 1
			   end
	end.


get_hp_dtl(FighterList)->
	lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
						{AccHP + HP, AccHPMax + HPMax}
				end, {0, 0}, FighterList).

filter_out_zero_hp(List) ->
    lists:filter(fun(#ger{gerHp=GerHP}) -> GerHP > 0 end, List).

recalc_fighter_list(FighterList, FighterList2) ->
    lists:foldr(fun(#ger{gerID=GerID}=Ger, Acc) ->
                        case lists:keyfind(GerID, #ger.gerID, FighterList2) of
                            false ->
                                [Ger|Acc];
                            #ger{gerHp=GerHp,gerProHp=GerProHp} ->
                                NewHp = min(GerHp,Ger#ger.gerAttr#gerAttr.gerHpMax),
                                [Ger#ger{gerHp=NewHp,gerProHp=GerProHp}|Acc]
                        end
                end, [], FighterList).



check_mine_state(MineID)->
	#tw_mine{state=State} = get_mine(MineID),
	case State == ?STATE_READY orelse State == ?STATE_INV of
		true ->
			case get_war_state() of
				0 ->  false;
				1 -> if MineID < ?MAX_BOSS1_ID -> true;
						true ->	false
					 end;
				2->	if MineID < ?MAX_BOSS1_ID -> false;
					   true ->	true
					end
			end;
		_ ->
			false
	end.


delete_mine_player(Value,#tw_mine{players=Players} = Mine) ->
	Mine#tw_mine{players=lists:delete(Value,Players)}.

add_mine_player(Value,#tw_mine{players=Players}=Mine) ->
	Mine#tw_mine{players=[Value|lists:delete(Value,Players)]}.

check_mine_player(Value,#tw_mine{players=Players}) ->
	lists:member(Value,Players).

get_min(X,Y) when X > Y ->	Y;
get_min(X,_Y) -> X.

update_player(#tw_player{roleID=RoleID,serverID=ServerID}=Player,Pos,MineID)->
	Player2 = Player#tw_player{tarMineID=MineID,startPos=Pos,startTime=util:now()},
	add_mov_plan(RoleID,ServerID),
	Player2.

update_player_fighters(Player,Blood,NewFightersA2)->
	Player#tw_player{blood=Blood,fighters=NewFightersA2}.

reborn_player_now(#tw_player{baseFighters=BF}=Player) ->
	Pos = data_twins:get(born_pos),
	Player#tw_player{startPos=Pos,startTime=0,tarMineID=0,fighters=BF,blood=100}.

%  更新player的起止位置到当前移动位置
reset_player(#tw_player{startPos=Pos0,tarMineID=TarMineID,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	case TarMineID of
		0 ->
			Player;
		_ ->
			Now=util:now(),
			TimeDiff = Now-ST,
			MovDis = Speed * TimeDiff,
			Mine=get_mine(TarMineID),
			Pos1 = Mine#tw_mine.pos,
			NowPos = calc_new_pos(Pos0,Pos1,MovDis),
			Player#tw_player{startPos=NowPos,startTime=Now,movDis=MovDis0+MovDis}
	end.

%% %  更新player的起始位置到当前移动位置
%% reset_player2(#tw_player{startPos=Pos0,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
%% 	Now=util:now(),
%% 	TimeDiff = Now-ST,
%% 	MovDis = Speed * TimeDiff,
%% 	NowPos = calc_new_pos(Pos0,Pos1,MovDis),
%% 	Player#tw_player{startPos=NowPos,startTime=Now,movDis=MovDis0+MovDis}.

% 更新player到目标终点
reset_player3(#tw_player{startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	Now=util:now(),
	MovDis=Speed*(Now-ST),
	Player#tw_player{startTime=0,movDis=MovDis0+MovDis,startPos={0,0}}.

% 更新player到目标终点
reset_player4(#tw_player{tarMineID=TarMineID,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	Now=util:now(),
	MovDis=Speed*(Now-ST),
	Mine = get_mine(TarMineID),
	NowPos = Mine#tw_mine.pos,%calc_new_pos(Pos0,Pos1,MovDis),
	Player#tw_player{startPos=NowPos,startTime=0,movDis=MovDis0+MovDis,tarMineID=0}.

reset_player5([],_) ->
	ignore;
reset_player5(L,Pos) ->
	lists:foreach(fun({RoleID,ServerID}) ->
						  Player = get_player(RoleID,ServerID),
						  NewPlayer = Player#tw_player{startPos=Pos,tarMineID=0,startTime=0},
						  	mark_bc([],{RoleID,ServerID}),
						  set_player(RoleID,ServerID,NewPlayer)
				  end, L).

reset_other_players(MineID,MinePos) ->
	List = [begin
				Player = #tw_player{tarMineID=TarMineID,startPos=Pos,speed=Speed,startTime=SC}=get_player(RoleID,ServerID),
				case TarMineID of
					MineID ->
						MVDis = Speed*(util:now()-SC),
						NewPos = calc_new_pos(Pos,MinePos,MVDis),
						set_player(RoleID,ServerID,Player#tw_player{startPos=NewPos,tarMineID=0,startTime=0}),
						{RoleID,ServerID};
					_ ->
						ignore
				end
			end||{RoleID,ServerID}<-get_mov_plan()],
	sync_role2(List).

player_arrive(RoleID,ServerID) ->
	player_arrive(get_player(RoleID,ServerID)).
player_arrive(#tw_player{tarMineID=TarMineID,roleID=RoleID,serverID=ServerID} = Player)->
	case TarMineID of
		0 ->
			ignore;
		MineID ->
			MineInfo = get_mine(MineID),
			cancel_mov_plan(RoleID,ServerID),
			Player2 = reset_player3(Player),
			set_player(RoleID,ServerID,Player2),
			NewMine2 = add_mine_player({RoleID,ServerID},MineInfo),
			mark_bc(MineID,{RoleID,ServerID}),
			set_mine(MineID,NewMine2)
	end.

update_player_mov_ex(Boss1,Boss2)->
	R = [begin
			 Player = #tw_player{tarMineID=TarMineID}=get_player(RoleID,ServerID),
			 case TarMineID of
				 Boss1 -> set_player(RoleID,ServerID,Player#tw_player{tarMineID=Boss2});
				 Boss2 -> set_player(RoleID,ServerID,Player#tw_player{tarMineID=Boss1});
				 _ -> ignore
			 end,
			 {RoleID,ServerID}
		 end||{RoleID,ServerID}<-get_mov_plan()],
	sync_role2(R).

update_players_pos()->
	CheckInterval = data_twins:get(check_interval),
	Near = data_twins:get(near_dis),
	Now = util:now(),
	[begin
		 Player = #tw_player{tarMineID=TarMineID,startPos=Pos,speed=Speed}=get_player(RoleID,ServerID),
		 case TarMineID of
			 0 ->
				 ignore;
			 _ ->
				 case Pos of
					 {0,0} ->
						 ignore;
					 _ ->
						 #tw_mine{pos=Pos1,state=_State} = get_mine(TarMineID),
%% 						 if State == ?STATE_DEAD ->
%% 								set_player(RoleID,ServerID,Player#tw_player{startTime=0,tarMineID=0});
%% 							true ->
								MVDis = Speed * CheckInterval,
								NewPos = calc_new_pos(Pos,Pos1,MVDis),
								case check_dis_near(NewPos,Pos1,MVDis + Near) of
									true ->	player_arrive(Player);
									_ -> set_player(RoleID,ServerID,Player#tw_player{startPos=NewPos,startTime=Now})
								end
%% 						 end
				 end
		 end
	 end||{RoleID,ServerID}<-get_mov_plan()].

update_mine_pos2(Mine)->
	#tw_mine{movTarget=HID,speed=Speed,pos=Pos0,startSec=ST}=Mine,
	{Pos1,_} = data_twins:get({home,HID}),
	Now = util:now(),
	TimeDiff=Now-ST,
	MovDis=Speed*TimeDiff,
	NowPos = calc_new_pos(Pos0,Pos1,MovDis),
	Mine#tw_mine{pos=NowPos}.

update_mine_pos(MineID) ->
	#tw_mine{movTarget=HID,speed=Speed,pos=Pos0,startSec=ST}=Mine = get_mine(MineID),
	if HID == 0 ->
		   ignore;
	   true ->
		   {Pos1,NextHID} = data_twins:get({home,HID}),
		   Now=util:now(),
		   TimeDiff = Now-ST,
		   MovDis = Speed * TimeDiff,
		   NowPos = calc_new_pos(Pos0,Pos1,MovDis),
		   NewMine = 
			   case check_mine_home(Pos0,NowPos,Pos1) of
				   true ->
					   Mine#tw_mine{pos=NowPos,startSec=Now,movTarget=NextHID};
				   false ->
					   Sync_tick = case data_twins:get(sync_tick) of
									   T when is_integer(T) -> if T > 3 -> T;true -> 3 end;
									   _ -> 3 
								   end,
					   case Now rem Sync_tick == 0 of
						   true-> mark_bc(MineID,[]);
						   _ -> ignore
					   end,
					   Mine#tw_mine{pos=NowPos,startSec=Now}
			   end,
		   set_mine(MineID,NewMine)
	end.


calc_player_gas(ScoreP,Score)->
	Now = ScoreP + Score,
	{Min,Max} = data_twins:get(score_limit),
	if Now < Min -> Min;
	   Now > Max -> Max;
	   true -> Now
	end.
 
check_mine_home(LastMinePos,NowMinePos,HomePos) ->
	{X1,Y1} = LastMinePos,
	{X2,Y2} = NowMinePos,
	{X3,Y3} = HomePos,
	L1 = (X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2),
	L2 = (X1-X3)*(X1-X3) + (Y1-Y3)*(Y1-Y3),
	Near = trunc(data_twins:get(near_dis)),
	if L1 + Near >= L2 ->   true;
	   true ->  false
	end.
	
calc_new_pos(Pos0,Pos0,_) ->
	Pos0;
calc_new_pos({A,B},{C,D}, Dis) ->
	Dx = A-C,
	Dy = B-D,
	DL = math:sqrt(Dx*Dx+Dy*Dy),
	Tx = trunc((Dx*Dis)/DL)-1,
	Ty = trunc((Dy*Dis)/DL)-1,
	{A-Tx,B-Ty}.

check_dis_near({A,B},{C,D},Charge)->
	L = (A-C)*(A-C) + (B-D)*(B-D),
	L < Charge*Charge.

get_player(RoleID,ServerID) ->				  
	get({?player, {RoleID,ServerID}}).			

set_player(RoleID,ServerID,Info) ->
	update_player_rank_data(Info),
	put({?player, {RoleID,ServerID}}, Info).
set_player2(RoleID,ServerID,Info) ->
	update_player_rank_data2(Info),
	put({?player, {RoleID,ServerID}}, Info).

get_mine(MineID) ->
	get({?mine, MineID}).

set_mine(MineID,Mine) ->
	put({?mine, MineID}, Mine).

plan(Value) ->
	?LOOSE_CATCH(Value()).


do_bc(talk,Data,twins_bc_info) ->
	BcList = get_bcList(),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,twins_server,{twins_bc_info,RoleID,Data})
				  end,BcList).

do_bc(Info,Type) ->
	BcList = get_bcList(),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,twins_server,{Type,RoleID,Info})
						  end,BcList).
bc_info(open) ->
	BaseInfo = get_war_base_info(),
	do_bc(BaseInfo,twins_bc_info);
bc_info({boss,MineID}) ->
	Mine = [mine2twinsMineS(get_mine(MineID))],
	Data = #sc_twins_update{mine=Mine},
	do_bc(Data, twins_bc_info);
bc_info(mark) ->
	put(?bc_mark,0),
	{ML,RL} = get_mark_bc(),
	MI = [mine2twinsMineS(get_mine(E))||E<-ML],
	RI = [player2twinsPlayerS(get_player(A,B))||{A,B}<-RL],
	Msg = #sc_twins_update{player=RI,mine=MI},
	set_mark_bc({[],[]}),
	do_bc(Msg,twins_bc_info).


get_war_base_info()->
	Homes = get(?home),
	{Mines,Players} = 
		lists:foldl(fun({{?mine,_},Mine},{MineAcc,PlayerAcc}) -> 
							{[mine2twinsMine(Mine)|MineAcc],PlayerAcc};
					   ({{?player,_},Player},{MineAcc,PlayerAcc})->
							{MineAcc,[player2twinsPlayer(Player)|PlayerAcc]};
					   (_,Acc) -> Acc
					end, {[],[]}, get()),
	{_,EndTime} = get(?endInfo),
	RebornPos = data_twins:get(born_pos),
	State=get_war_state(),
	#sc_twins_war_base_info{result=1,endTimeStamp=EndTime,rebornPos=pos2twinsPos(RebornPos)
								,homesPos=Homes,mines=Mines,players=Players,state=State}.


get_war_state() ->
	case get(?war_state) of
		S when is_integer(S) -> S;
		_ -> ?STATE_DEFAULT
	end.

set_war_state(State)->
	put(?war_state,State).

pos2twinsPos({X,Y}) ->
	#p_twins_pos{x=X,y=Y}.
home2twinsHome(Home) ->
	#p_twins_home{homeID=Home#tw_home.id
					 ,pos=pos2twinsPos(Home#tw_home.pos)}.
player2twinsPlayer(Player) ->
	#p_twins_player{roleID=Player#tw_player.roleID
					,serverID=Player#tw_player.serverID
					,startPos=pos2twinsPos(Player#tw_player.startPos)
					,fly=Player#tw_player.fly
					,mineID=Player#tw_player.tarMineID
					,blood=Player#tw_player.blood
					,name=Player#tw_player.roleName
					,startSec=Player#tw_player.startTime
					,attr=(Player#tw_player.trSpecial)#trSpecial.specialID
					,score=Player#tw_player.score
					,fightPower=Player#tw_player.fight_power
                    ,level=Player#tw_player.level
                    ,speed=Player#tw_player.speed}.
player2twinsPlayerS(Player) ->
	#p_twins_player_s{roleID=Player#tw_player.roleID
						 ,serverID=Player#tw_player.serverID
						 ,mineID=Player#tw_player.tarMineID
					  ,blood=Player#tw_player.blood
					  ,score=Player#tw_player.score
						 ,startPos=pos2twinsPos(Player#tw_player.startPos)
						 ,startSec=Player#tw_player.startTime}.

get_attr(SpecialID) ->
	data_twins:get({trtype, SpecialID}).

p2p_s(L) ->
	[p2p_s(R,S)||{R,S}<-L].
p2p_s(R,S) ->
	#p_twins_p_s{roleID=R,serverID=S}.
mine2twinsMine(Mine) ->
	#p_twins_mine{mineID=Mine#tw_mine.id
					  ,pos=pos2twinsPos(Mine#tw_mine.pos)
					  ,attr=Mine#tw_mine.attr
				  ,movTarget = Mine#tw_mine.movTarget
				  ,state = Mine#tw_mine.state
				  ,display = Mine#tw_mine.bossType
				  ,speed=Mine#tw_mine.speed
				  ,fullblood=Mine#tw_mine.maxHp
				  ,nowblood=Mine#tw_mine.nowHp
				 ,blood=Mine#tw_mine.blood
				  ,players=p2p_s(Mine#tw_mine.players)
					  ,startSec = Mine#tw_mine.startSec}.
mine2twinsMineS(Mine)->
	#p_twins_mine_s{mineID=Mine#tw_mine.id
						,startPos = pos2twinsPos(Mine#tw_mine.pos)
					,movTarget=Mine#tw_mine.movTarget
					,blood=Mine#tw_mine.blood
					,state=Mine#tw_mine.state
				   ,nowBlood = Mine#tw_mine.nowHp
				   ,players=p2p_s(Mine#tw_mine.players)
						,startSec=Mine#tw_mine.startSec}.
fairy2twinsFairy(FL) when is_list(FL) ->
	[fairy2twinsFairy(F)||F<-FL];
fairy2twinsFairy(#ger{gerBase=#gerBase{gerTypeID=TypeID,gerQuality=Rank}
					  ,gerAttr=#gerAttr{gerHpMax=HpMax},gerHp=Hp})->
	#p_twins_fairy{typeID=TypeID
					   ,maxHp=HpMax
					   ,nowHp=Hp
					   ,rank=Rank}.
player2p_twins_rank_dtl(Player)->
	#p_twins_rank_dtl{roleID=Player#tw_player.roleID
						  ,serverID=Player#tw_player.serverID
						  ,level=Player#tw_player.level
					  ,name=Player#tw_player.roleName
					  ,score = Player#tw_player.score
						 }.
player2p_twins_rank_dtl2(Player)->
    AfkTime = get({?pd_operate_ts,Player#ga_player.roleID}),
    AfkLimit = util:now() - data_galactica:get(afk_check_time),
	#p_twins_rank_dtl{roleID=Player#tw_player.roleID
						  ,serverID=Player#tw_player.serverID
						  ,level=Player#tw_player.level
					  ,name=Player#tw_player.roleName
					  ,score=Player#tw_player.score
						  ,type = if AfkTime =:= ?undefined orelse AfkTime < AfkLimit -> 1; true -> 0 end
					  }.

sync_role(RoleID,ServerID)->
	P1 = player2twinsPlayerS(get_player(RoleID,ServerID)),
	Msg = #sc_twins_update{player=[P1],mine=[]},
	twins_router:send_client(ServerID,RoleID,Msg),
	ok.
sync_mines(BossList) ->
	M = [mine2twinsMineS(get_mine(MineID))||MineID<-BossList],
	Msg= #sc_twins_update{player=[],mine=M},
	do_bc(Msg,twins_bc_info).
sync_mines2(BossList) ->
	M = [mine2twinsMineS(Mine)||Mine<-BossList],
	Msg= #sc_twins_update{player=[],mine=M},
	do_bc(Msg,twins_bc_info).
	
sync_role2(List) ->
	Msg = #sc_twins_update{player=[player2twinsPlayerS(get_player(RoleID,ServerID))||{RoleID,ServerID}<-List],mine=[]},
	do_bc(Msg,twins_bc_info),
	ok.

%% 0.3s sync info...
mark_bc(Mine,Role) ->
	{ML,RL} = get_mark_bc(),
	Info = {add_bc_list(Mine,ML),add_bc_list(Role,RL)},
	set_mark_bc(Info),
	case get(?bc_mark) of
		1 ->
			ignore;
		_ ->
			put(?bc_mark,1),
			erlang:send_after(500,self(), {bc_info, mark})
	end.

add_bc_list([],List) ->
	List;
add_bc_list(Value,List) ->
	case lists:member(Value, List) of
		true ->	List;
		_ -> [Value|List]
	end.
get_mark_bc()->
	case get(?mark_bc) of
		?undefined -> {[],[]};
		X -> X
	end.
set_mark_bc(Info) ->
	put(?mark_bc, Info).

add_mov_plan(RoleID,ServerID) ->
	set_mov_plan([{RoleID,ServerID}|lists :delete({RoleID,ServerID}, get_mov_plan())]).
cancel_mov_plan(RoleID,ServerID) ->
	set_mov_plan(lists :delete({RoleID,ServerID}, get_mov_plan())).

get_mov_plan()->
	case get(?mov_plan) of
		X when is_list(X) -> X;
		_ -> []
	end.
set_mov_plan(List)->
	put(?mov_plan,List).

check_plan()->
	F = fun()->
				erlang:send(self(), check_tick)
		end,
	Interval = data_twins:get(check_interval),
	add_plan(#plan{sec=util:now()+Interval, key=check_plan, value=F}).

calc_time({X1,Y1},{X2,Y2},Speed) ->
	Dx = X1 - X2,
	Dy = Y1 - Y2,
	Len = math:sqrt(Dx * Dx + Dy * Dy),
	calc_time(Len,Speed).
calc_time(Len,0)->
	Len;
calc_time(Len,Speed)->
	trunc(Len/Speed).

add_plan(#plan{key=K}=Plan)->
	Plans = get_plan(),
	Plans2 = lists:keydelete(K,#plan.key,Plans),
	set_plan([Plan|Plans2]).
get_plan()->
	case get(?plan) of
		?undefined ->
			[];
		X ->X
	end.
set_plan(Plan) ->
	put(?plan, Plan).

delete_plan(Key) ->
	Plans = get_plan(),
	Plans2 = lists:keydelete(Key, #plan.key, Plans),
	set_plan(Plans2).


plan()->
	Plans = get_plan(),
	Now = util:now(),
	Plans2 = lists:foldl(fun(Plan=#plan{sec=Sec,value=Value},Acc)-> 
							if Sec =< Now ->  plan(Value), Acc;
							   true -> [Plan|Acc]
							end
					end,[],Plans),
	set_plan(Plans2).

% boss全死了,直接返回到HpLimit的状态,否则正常战斗
do_fight(BossList,BossCurHp,FighterList,LieuAdd,TalentList,TrSpecial,BossTr,FSkin,BSkin,HpLimit,DamageReduce)->
	if BossCurHp =< HpLimit orelse BossList == [] -> 
		   {true,HpLimit,BossList,[],FighterList};
	   true ->
		   CurLimit = data_twins:get(cur_reduce),
		   {Result,FightRecord,{_,NewBossStateList,NewFightList,_}} =
			   role_fight:inv_new(FighterList, BossList, LieuAdd,{-DamageReduce,0},TalentList,[],TrSpecial,BossTr,FSkin,BSkin,HpLimit,CurLimit),
		   {NewCurHp,NewBossList}
			   = lists:foldl(fun(B,{AccHp,AccList})->
									 BHP = B#ger.gerHp,
									 case lists:keytake(B#ger.gerID,1,NewBossStateList) of
										 {value,{_,CHP,_},_} when CHP > BHP -> {AccHp+CHP,[B#ger{gerHp = BHP}|AccList]};
										 {value,{_,CHP,_},_} when CHP > 0 -> {AccHp+CHP,[B#ger{gerHp = CHP}|AccList]};
										 _ -> {AccHp,AccList}
									 end
							 end,{0,[]},BossList),
		   {Result, erlang:max(HpLimit,NewCurHp),NewBossList,FightRecord,NewFightList}
	end.

set_pd_operate_ts(RoleID)->
    put({?pd_operate_ts,RoleID},util:now()).

