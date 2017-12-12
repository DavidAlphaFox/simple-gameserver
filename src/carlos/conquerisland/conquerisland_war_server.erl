%% @author crimoon-17
%% @doc @todo Add description to war_server.
%% 按照出生点,出生在下方的是attacker:0,出生在三方的是defender:1  
%% 


-module(conquerisland_war_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_carlos.hrl").
-include("def_mail.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).

-define(player, player).
-define(centre, centre).
-define(boss,boss).
-define(plan,plan).
-define(bcList,bcList).
-define(endInfo, endInfo).
-define(bc_mark,bc_mark).
-define(mark_bc,mark_bc).
-define(full_speed, full_speed).
-define(centre_occupy,centre_occupy).
-define(Dump_interval, 60000).
-define(reborn_pos,reborn_pos).
-define(bossid,bossid).
-define(war_change,war_change).
-define(fight_rank,fight_rank).
-define(talk_cache,talk_cache).
-define(pd_operate_ts, pd_operate_ts).
-define(close_war_flage,close_war_flage).

-define(WAR_PLAYER_CHANGE_TYPE,1).
-define(WAR_BOSS_CHANGE_TYPE,2).
-define(WAR_CENTRE_CHANGE_TYPE,3).

-define(CENTRE_STATE_UNOCCUPY,0).
-define(CENTRE_STATE_OCCUPING,1).
-define(CENTRE_STATE_OCCUPIED,2).
-define(CENTRE_STATE_OCCUPIED_BY_ATTACKER,3).
-define(CENTRE_STATE_OCCUPIED_BY_DEFENDER,4).
-define(ATTACKER_BOSS_TYPE,1).
-define(DEFENDER_BOSS_TYPE,2).

-define(PLAYER_TYPE_ATTACKER,1).
-define(PLAYER_TYPE_DEFENDER,2).

%%========================================
-define(CENTRE_BUFF_BOSS_DEMAGE_PLUS,1).
-define(CENTRE_BUFF_FLY_SPEED_PLUS,2).
-define(CENTRE_BUFF_BOSS_HP_PLUS,3).
-define(CENTRE_BUFF_BOSS_CRIT_PLUS,4).
-define(CENTRE_BUFF_BOSS_DEMAGE_DEC,5).
-define(CENTRE_BUFF_PLAYER_REBORN,6).

-define(FIGHT_RESULT_NO_FINISH,0).
-define(FIGHT_RESULT_WIN,1).
-define(FIGHT_RESULT_EQUAL,2).
-define(FIGHT_RESULT_LOSE,3).
-define(FIGHT_RESULT_KICK,4).
-define(ETS_CLEAN_DELAY,300).
%%========================================
-record(state, {warID=0,warStartTime=0}).
-record(plan, {sec=0,key=0,value=0}).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).
start_link(WarID,Attackers,Defenders) ->
	gen_server:start_link(?MODULE,[WarID,Attackers, Defenders],[]).
start(Args) ->
		supervisor:start_child(conquerisland_war_sup,Args).

% init([WarID]) ->
% 	Data = db_sql:get_carlos_war_info(WarID),
% 	add_ets_war_info(WarID,self()),
% 	State2 = init_state(Data),
%     {ok, State2};
init([WarID,Attacker,Defender]) ->
	put(?close_war_flage,false),
	erlang:send(self(),{load_data,WarID,Attacker,Defender}),
	process_flag(trap_exit,true),
	{ok,#state{warID=WarID,warStartTime=util:now()}}.

add_ets_war_info(WarID,PID) ->
	% ?INFO("Insert WarID Map WarID:~w PID:~w ~n",[WarID,PID]), 
	Interval = data_conquerisland:get(war_interval),
	NowSec = util:now(),
	ets:insert(?ETS_CONQUERISLAND_WAR_PID_MAP,#ets_carlos_info{warID=WarID,pid=PID,createTime=NowSec,endTime=NowSec+Interval+?ETS_CLEAN_DELAY}).

% init_state({#state{warStartTime=WST}=State,Dict}) ->
%     lists:foreach(fun
%             ({K,V}) -> 
%                 put(K,V)
%         end, Dict),
% 	Now=util:now(),
% 	Interval = data_conquerisland:get(war_interval),
% 	End = WST + Interval,
% 	if End >= Now ->
% 		   erlang:send(self(), do_end_war);
% 	   true ->
% 		   erlang:send_after((End - Now) * 1000, self(), do_end_war)
% 	end,
% 	State.

init_players(Type,Players) ->
	Pos = data_conquerisland:get({born_pos, Type}),
	lists:foreach(fun(BasePlayer)->
		ConquerIslandPlayer= base_player2conquerisland_player(BasePlayer),
		%%加入到排行榜中
		add_fight_rank(ConquerIslandPlayer,?FIGHT_RESULT_NO_FINISH),
		set_player(ConquerIslandPlayer#conquerisland_player{startPos=Pos,endPos=Pos,type=Type})
	end,Players),
	set_player_reborn_pos(Type,Pos).

init_ground(PlayerList) ->
	init_centre(),
	init_boss(PlayerList).

init_centre()->
	%%初始化所有的据点
	CentreList = data_conquerisland:get(centre_list),
	lists:foreach(fun(CentreID)->
		{Pos,_BuffID,_Value} = data_conquerisland:get({centre,CentreID}),
		put({?centre,CentreID},#centre{id=CentreID,pos=Pos})
	end,CentreList).

set_centre(#centre{id=ID}=Centre)->
	put({?centre,ID},Centre).
get_centre(ID)->
	get({?centre,ID}).

init_boss(PlayerList)->
	BossHp = calculate_boss_init_hp(PlayerList),
	[{AttackerBossID,AttackerBossPos},{DefenderBossID,DefenderBossPos}|_T] = data_conquerisland:get(bosslist),
	BossA = #bossinfo{bossId=AttackerBossID,bossHp=BossHp,bossMaxHp=BossHp,bossPos=AttackerBossPos,type=?ATTACKER_BOSS_TYPE},
	BossD = #bossinfo{bossId=DefenderBossID,bossHp=BossHp,bossMaxHp=BossHp,bossPos=DefenderBossPos,type=?DEFENDER_BOSS_TYPE},
	set_player_bossid(?PLAYER_TYPE_ATTACKER,AttackerBossID),
	set_player_bossid(?PLAYER_TYPE_DEFENDER,DefenderBossID),
	set_boss(BossA),
	set_boss(BossD).
	
get_boss(ID)->
	get({?boss,ID}).
set_boss(#bossinfo{bossId=ID}=Boss)->
	put({?boss,ID},Boss).

set_player_bossid(Type,BossID)->
	put({?bossid,Type},BossID).
get_player_bossid(Type)->
	get({?bossid,Type}).

init_bcList(Attacker,Defender) ->
	RL=[{ServerID,RoleID}||#base_player{roleID=RoleID,serverID=ServerID}<-Attacker++Defender],
	put(?bcList, RL),
	RLA = [{ServerID,RoleID}||#base_player{roleID=RoleID,serverID=ServerID}<-Attacker],
	put({?bcList,?PLAYER_TYPE_ATTACKER},RLA),
	RLD = [{ServerID,RoleID}||#base_player{roleID=RoleID,serverID=ServerID}<-Defender],
	put({?bcList,?PLAYER_TYPE_DEFENDER},RLD).

get_bcList()->
	case get(?bcList) of
		?undefined ->
			[];
		X ->
			X
	end.

get_bcList(Type) ->
	case get({?bcList,Type}) of
		?undefined ->
			[];
		X ->
			X
	end.

set_end_info(WarID) ->
	Interval = data_conquerisland:get(war_interval),
	TimerRef = erlang:send_after(Interval * 1000, self(), do_end_war),
	put(?endInfo,{WarID,util:now()+Interval,TimerRef}).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(dump_interval_tick, State) ->
	add_dump(),
	?CATCH(do_interval_dump(State)),
	{noreply, State, hibernate};

handle_info(stop,State) ->
	{stop,normal, State};
handle_info({load_data,WarID,Attacker,Defender},State)->
	%%战斗进程写入映射中
	add_ets_war_info(WarID,self()),
	%%初始化各种广播列表
	init_bcList(Attacker,Defender),
	%%初始化地图
	init_ground(Attacker++Defender),
	%%初始化玩家
	init_players(?PLAYER_TYPE_ATTACKER, Attacker),
	init_players(?PLAYER_TYPE_DEFENDER, Defender),
	%%启动秒循环
	sec_tick(),
	%%添加持久化,感觉持久化没啥用，取消算了
	% add_dump(),
	%%增加战斗结束定时器
	set_end_info(WarID),
	%%广播战斗开始消息
	broadcast_war_begin(),
    conquerisland_ai:init(Attacker++Defender),
	{noreply,State};

handle_info(Info, State) ->
	case get(?close_war_flage) of
		false->
			?CATCH(do_handle_info(Info,State));
		true->
			?ERR("war is close Msg:~w ~n",[Info])
	end,
	{noreply, State}.

terminate(Reason, State) ->
	?CATCH(do_interval_dump(State)),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p"
		  ,[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_info(sec_tick,_State)->
	sec_tick(),
	%%检查时间段内是否有新的玩家操作时间完成
	plan(),
    conquerisland_ai:check_robot_act(),
	%%广播时间段内战场内情况的变化
	broadcast_war_change();

%%战斗结束
do_handle_info(do_end_war,#state{warID=WarID})->
	end_war(WarID);

do_handle_info({role,war,{Msg, {_WarID,RoleID,ServerID}}},State)->
	% ?INFO("receive role msg:~w ~n",[M]),
	case catch do_handle_role_msg(Msg,RoleID,ServerID,State) of
		noreply->
			ignore;
		{reply,[],[]}->
			ignore;
		{reply,ClientMsg,[]}->
			conquerisland_router:send_msg_to_client(ServerID,RoleID,ClientMsg);
		{reply,[],RoleMsg}->
			conquerisland_router:send_msg_to_role(ServerID,RoleID,RoleMsg);
		{reply,ClientMsg,RoleMsg}->
			conquerisland_router:send_msg_to_client(ServerID,RoleID,ClientMsg),
			conquerisland_router:send_msg_to_role(ServerID,RoleID,RoleMsg);
		Other->
			?ERR("undefined msg:~w ~n",[Other])
	end;

%%处理到达boss，攻击Boss动作
do_handle_info({arrive_boss,BossID,ServerID,RoleID},_State)->
	do_attack_boss(BossID,ServerID,RoleID);
do_handle_info({arrive_centre,CentreID,ServerID,RoleID},_State)->
	do_arrive_centre(CentreID,ServerID,RoleID);
do_handle_info({occupy_finish,CentreID,ServerID,RoleID},_State)->
	do_occupy_finish(CentreID,ServerID,RoleID);
do_handle_info({reborn,ServerID,RoleID},_State)->
	reborn_player(ServerID,RoleID);
do_handle_info(Msg,State)->
	?ERR("undefined Msg:~w State:~w ~n",[Msg,State]).

%%战场结束
end_war(WarID)->
	%%通知游戏服战斗结束，并且附带上所有玩家的奖励
	notice_end_war(WarID),
	%%删除战斗服务器上面的战场映射
	conquerisland_war_manager_server:deal_end_war(WarID),
	%%向自身发送结束消息
	put(?close_war_flage,true),
	erlang:send(self(),stop).

notice_end_war(WarID)->
	%%计算战斗结果保存在字典中
	calcu_fight_result(),
    AfkLimit = util:now() - data_conquerisland:get(afk_check_time),
	{PlayerClassifyList,BossList}= 
		lists:foldl(
		  fun(Elem,{PlayerAcc,BossAcc}) ->
				  case Elem of
					  {{?player,ServerID,RoleID},Player=#conquerisland_player{type=Type,movDis=_MovDis}}->
					  	#p_fight_rank{killNum=KillNum,centreNum=CentreNum,bossDemage=BossDemage,honor=Honor} = add_fight_rank(Player,get({fight_result,Type})),
                        FightResult = 
                                case get({?pd_operate_ts,RoleID}) of
                                    ?undefined ->            %% 挂机,就从来没动过
                                        ?FIGHT_RESULT_KICK;
                                    Ts when Ts < AfkLimit -> %% 挂机
                                        ?FIGHT_RESULT_KICK;
                                    _ ->
                                        get({fight_result,Type})
                                end,
%% 					  	FightResult = case MovDis of 0->?FIGHT_RESULT_KICK;_-> get({fight_result,Type}) end,
					  	case lists:keytake(ServerID,1,PlayerAcc) of
					  		false->
					  			{[{ServerID,[{RoleID,{KillNum,CentreNum,BossDemage,Honor,FightResult}}]}|PlayerAcc],BossAcc};
					  		{_Value,{ServerID,ExistList},Other}->
					  			{[{ServerID,[{RoleID,{KillNum,CentreNum,BossDemage,Honor,FightResult}}|ExistList]}|Other],BossAcc}
					  	end;
					  {{?boss,_ID},Boss} -> 
						  {PlayerAcc,[Boss|BossAcc]};
					   _->
					   	{PlayerAcc,BossAcc}
				  end
		  end,{[],[]},get()),
	RankList = get_fight_rank(),
	Msg = #sc_conquerisland_end_war{data=RankList},
	NeedBroadcastRoleList = get(?bcList),

	broadcast_war_change_to_role([],[],BossList,NeedBroadcastRoleList),
	broadcast_msg(Msg,NeedBroadcastRoleList),
	lists:foreach(fun({DesServerID,RoleList})->
		send_msg:direct(DesServerID,conquerisland_server,{end_war,WarID,RoleList})
	end,PlayerClassifyList).

calcu_fight_result()->
	AttackerBossID = get_player_bossid(?PLAYER_TYPE_ATTACKER),
	DefenderBossID = get_player_bossid(?PLAYER_TYPE_DEFENDER),
	#bossinfo{bossHp=BossHpA} = get_boss(AttackerBossID),
	#bossinfo{bossHp=BossHpD} = get_boss(DefenderBossID),
	{FightAttacker,FightDefender} = if 
		BossHpA > BossHpD ->
			{?FIGHT_RESULT_WIN,?FIGHT_RESULT_LOSE};
		BossHpA =:= BossHpD->
			{?FIGHT_RESULT_EQUAL,?FIGHT_RESULT_EQUAL};
		true->
			{?FIGHT_RESULT_LOSE,?FIGHT_RESULT_WIN}
	end,
	put({fight_result,?PLAYER_TYPE_ATTACKER},FightAttacker),
	put({fight_result,?PLAYER_TYPE_DEFENDER},FightDefender).

%%================================处理玩家进程发送过来的协议信息==================================
do_handle_role_msg(#cs_conquerisland_war_base_info{},_RoleID,_ServerID,_State)->
	Reply = generate_war_base_info(),
	{reply,Reply,[]};

do_handle_role_msg(#cs_conquerisland_role_dtl{serverId=TarServerID,roleId=TarRoleID},_RoleID,_ServerID,_State)->
	Reply = case get_player(TarServerID,TarRoleID) of
		?undefined->
			#sc_conquerisland_role_dtl{result=0,grade=0};
		Player->
			generate_player_dtl(Player)
	end,
	{reply,Reply,[]};
do_handle_role_msg(#cs_conquerisland_mov{centreId=TarCentreID},RoleID,ServerID,_State)->
    set_pd_operate_ts(RoleID),
	Reply = case get_player(ServerID,RoleID) of
		?undefined->
			#sc_conquerisland_mov{result=4};
		Player->
			case is_player_alive(Player) of
				true->
					do_player_mov(Player,TarCentreID);
				false->
					#sc_conquerisland_mov{result=5}
			end
	end,
	{reply,Reply,[]};
do_handle_role_msg(#cs_conquerisland_stop{},RoleID,ServerID,_State)->
    set_pd_operate_ts(RoleID),
	Reply = case get_player(ServerID,RoleID) of
		?undefined->
			#sc_conquerisland_stop{result=2};
		Player ->
			case is_player_alive(Player) of
				true->
					do_player_stop(Player);
				false->
					#sc_conquerisland_stop{result=3}
			end
	end,
	{reply,Reply,[]};
do_handle_role_msg(#cs_conquerisland_centre_dtl{centreId=CentreID},_RoleID,_ServerID,_State)->
	Reply = case get_centre(CentreID) of
		?undefined->
			#sc_conquerisland_centre_dtl{result=2,centre=#p_centre{centreId=0,owner=0,attackerNum=0,defenderNum=0,buffId=0,centrePos=#p_pos{},ownSec=0}};
		Centre->
			generate_centre_dtl(Centre)
	end,
	{reply,Reply,[]};
do_handle_role_msg(#cs_conquerisland_attack{serverId=TarServerID,roleId=TarRoleID,centreId=TarCentreID},RoleID,ServerID,_State)->
    set_pd_operate_ts(RoleID),
	Reply = case do_attack(ServerID,RoleID,TarServerID,TarRoleID,TarCentreID) of
		{false,Reason}->
			#sc_conquerisland_attack{result=Reason,fightInfo=[]};
		{true,Result,FightInfo}->
			#sc_conquerisland_attack{result=Result,fightInfo=FightInfo}
	end,
	{reply,Reply,[]};

do_handle_role_msg(#cs_conquerisland_centre_occupy{centreId=CentreID},RoleID,ServerID,_State)->
    set_pd_operate_ts(RoleID),
	Reply = case do_centre_occupy(ServerID,RoleID,CentreID) of
		{false,Reason,BeOwnSec}->
			#sc_conquerisland_centre_occupy{result=Reason,sec=BeOwnSec};
		{true,Sec}->
			#sc_conquerisland_centre_occupy{result=1,sec=Sec}
	end,
	{reply,Reply,[]};

%%战场重生，直接游戏服扣除材料，然后返回重生成功值，战斗服不做任何处理
do_handle_role_msg(#cs_conquerisland_reborn{},RoleID,ServerID,_State)->
    set_pd_operate_ts(RoleID),
	case do_player_reborn(ServerID,RoleID) of
		{false,Reason}->
			?INFO("reborn player with error:~w ~n",[Reason]);
		true->
			ignore
	end,
	noreply;

do_handle_role_msg(#cs_conquerisland_self{},RoleID,ServerID,_State)->
	Reply = case get_player(ServerID,RoleID) of
		?undefined->
			#sc_conquerisland_self{self=#p_player{}};
		Player->
			[PPlayer] = conquerisland_player2pPlayer([Player],[]),
			#sc_conquerisland_self{self=PPlayer}
	end,
	{reply,Reply,[]};

do_handle_role_msg(#cs_conquerisland_rank{},_RoleID,_ServerID,_State)->
	Rank = get_fight_rank(),
	Reply = #sc_conquerisland_rank{ranklist=Rank},
	{reply,Reply,[]};

do_handle_role_msg(#cs_conquerisland_talk{data=Content},RoleID,ServerID,_State)->
	case get_player(ServerID,RoleID) of
		?undefined->
			ignore;
		#conquerisland_player{type=Type,roleName=RoleName}->
			TalkUnit = #p_carlos_talk{roleID=RoleID,roleName=RoleName,data=Content},
			add_talk(Type,TalkUnit),
			NeedBroadcastRoleList = get({?bcList,Type}),
			Msg = #sc_conquerisland_talk{data=TalkUnit},
			broadcast_msg(Msg,NeedBroadcastRoleList)
	end,
	noreply;

do_handle_role_msg(#cs_conquerisland_get_talk{},RoleID,ServerID,_State)->
	case get_player(ServerID,RoleID) of
		?undefined->
			ignore;
		#conquerisland_player{type=Type}->
			TalkUnitList = get_talk_by_type(Type),
			Msg = #sc_conquerisland_get_talk{data=TalkUnitList},
			broadcast_msg(Msg,[{ServerID,RoleID}])
	end,
	noreply;

do_handle_role_msg(Msg,RoleID,ServerID,_State)->
	?ERR("undefined Msg:~w RoleID:~w ServerID:~w ~n",[Msg,RoleID,ServerID]),
	ignore.
%%================================================================================================
do_interval_dump(_State) ->
	% db_sql:set_carlos_data(WarID, {State,get()}).
	ok.

%%生成当前战场基本信息
generate_war_base_info()->
	{PlayerList,CentreList,BossList}=lists:foldl(fun(Elem,{PlayerAcc,CentreAcc,BossAcc}=Acc)->
		case Elem of
			{{?player,_ServerID,_RoleID},Player}->
				{[Player|PlayerAcc],CentreAcc,BossAcc};
			{{?boss,_Type},Boss}->
				{PlayerAcc,CentreAcc,[Boss|BossAcc]};
			{{?centre,_CentreID},Centre}->
				{PlayerAcc,[Centre|CentreAcc],BossAcc};
			_->
				Acc
		end
	end,{[],[],[]},get()),
	PPlayerList = conquerisland_player2pPlayer(PlayerList,[]),
	PCenterList = conquerisland_centre2pCentre(CentreList,[]),
	PBossList = conquerisland_boss2pBoss(BossList,[]),
	AttackerRebornPos = pos2ppos(get_player_reborn_pos(?PLAYER_TYPE_ATTACKER)),
	DefenderRebornPos = pos2ppos(get_player_reborn_pos(?PLAYER_TYPE_DEFENDER)),
	{_WarID,EndTime,_TimeRef} = get(?endInfo),
	#sc_conquerisland_war_base_info{result=1,endTimeStamp=EndTime,attackerPos=AttackerRebornPos,defenderPos=DefenderRebornPos,centre=PCenterList,players=PPlayerList,bosses=PBossList}.

do_player_mov(Player,TarCentreID)->
	#conquerisland_player{tarCentre=OldTarCentreID,type=PlayerType,serverID=ServerID,roleID=RoleID} = Player,
	case OldTarCentreID=:=TarCentreID of
		true->
			%%正在向对应的矿区移动
			#sc_conquerisland_mov{result=2};
		false->
			case delete_player_from_centre(PlayerType,{ServerID,RoleID},OldTarCentreID) of
				ignore->
					ignore;
				OldCentre->
					set_centre(OldCentre)
			end,
			case get_centre(TarCentreID) of
				?undefined->
					case get_boss(TarCentreID) of
						?undefined->
							%%不存在的矿区
							#sc_conquerisland_mov{result=1};
						Boss=#bossinfo{type=BossType}->
							case BossType=/=PlayerType of
								false->
									%%不能像本方的BOSS移动攻击
									#sc_conquerisland_mov{result=1};
								true->
									do_player_mov_to_boss(Player,Boss),
									#sc_conquerisland_mov{result=3}
							end
					end;
				Centre->
					do_player_mov_to_centre(Player,Centre),
					#sc_conquerisland_mov{result=3}
			end
	end.

do_player_mov_to_boss(Player,#bossinfo{bossId=BossID,bossPos=BossPos})->
	%%更新player的当前位置以及设置目标地点
	NowSec = util:now(),
	Player1 = #conquerisland_player{startPos=StartPos,baseSpeed=BaseSpeed,speedPlus=SpeedPlus,serverID=ServerID,roleID=RoleID} = reset_player(Player),
	Player2 = Player1#conquerisland_player{endPos=BossPos,tarCentre=BossID,startTime=NowSec},
	set_player(Player2),
	%%将player状态变化加入到同步列表
	add_player_update_list({ServerID,RoleID}),
	%%添加到达地点计划
	ArriveTimeInterval = calc_time(StartPos,BossPos,BaseSpeed*(1+SpeedPlus/10000)),
	F = fun()->erlang:send(self(),{arrive_boss,BossID,ServerID,RoleID}) end,
	add_plan(#plan{sec=util:now()+ArriveTimeInterval,key={ServerID,RoleID},value=F}).


do_player_mov_to_centre(Player=#conquerisland_player{roleID=RoleID},#centre{id=CentreID,pos=Pos})->
	NowSec = util:now(),
	Player1 = #conquerisland_player{startPos=StartPos,baseSpeed=BaseSpeed,speedPlus=SpeedPlus,serverID=ServerID,roleID=RoleID} = reset_player(Player),
	Player2 = Player1#conquerisland_player{endPos=Pos,tarCentre=CentreID,startTime=NowSec},
	% ?ERR("Player2:~w ~n",[Player2]),
	set_player(Player2),
	add_player_update_list({ServerID,RoleID}),
	ArriveTimeInterval = calc_time(StartPos,Pos,BaseSpeed*(1+SpeedPlus/10000)),
	% ?ERR("RoleID:~w TarCentreID:~w StartPos:~w endPos:~w startTime:~w arriveinterval:~w ArriveTime:~w ~n",[RoleID,CentreID,StartPos,Pos,NowSec,ArriveTimeInterval,NowSec+ArriveTimeInterval]),
	F = fun()->erlang:send(self(),{arrive_centre,CentreID,ServerID,RoleID}) end,
	add_plan(#plan{sec=NowSec+ArriveTimeInterval,key={ServerID,RoleID},value=F}).

%%修改了startpos和endpos，并且将tarcentre设置成0
do_player_stop(Player)->
	Player1 = #conquerisland_player{startPos=_StartPos,serverID=ServerID,roleID=RoleID} = reset_player(Player),
	Player2 = Player1#conquerisland_player{tarCentre=0},
	set_player(Player2),
	%%添加到同步列表中等待下次同步
	add_player_update_list({ServerID,RoleID}),
	%%取消到达计划
	delete_plan({ServerID,RoleID}),
	#sc_conquerisland_stop{result=1}.

%%生成据点当前具体状态
generate_centre_dtl(#centre{attackerList=AttackerKeyList,defenderList=DefenderKeyList}=Centre)->
	[PCentre] = conquerisland_centre2pCentre([Centre],[]),
	F = fun(List)->lists:foldl(fun({ServerID,RoleID},Acc)->case get_player(ServerID,RoleID) of ?undefined->Acc;Player->[Player|Acc] end end,[],List) end,
	AttackerList = F(AttackerKeyList),
	DefenderList = F(DefenderKeyList),
	Attackers = conquerisland_player2pPlayer(AttackerList,[]),
	Defenders = conquerisland_player2pPlayer(DefenderList,[]),
	#sc_conquerisland_centre_dtl{result=2,centre=PCentre,attackers=Attackers,defenders=Defenders}.

%%player之间互相发生攻击
do_attack(ServerID,RoleID,TarServerID,TarRoleID,TarCentreID)->
	case check_attack(ServerID,RoleID,TarServerID,TarRoleID,TarCentreID) of
		{false,Reason}->
			{false,Reason};
		true->
			do_attack2(ServerID,RoleID,TarServerID,TarRoleID,TarCentreID)
	end.

do_centre_occupy(ServerID,RoleID,CentreID)->
	case check_centre_occupy(ServerID,RoleID,CentreID) of
		{false,Reason,BeOwnSec}->
			{false,Reason,BeOwnSec};
		true->
			do_centre_occupy2(ServerID,RoleID,CentreID)
	end.

do_centre_occupy2(ServerID,RoleID,CentreID)->
	#centre{state=State,ownerType=OldOwnerType}=Centre = get_centre(CentreID),
	{_Pos,BuffType,Value} = data_conquerisland:get({centre,CentreID}),
	#conquerisland_player{type=Type,roleName=RoleName} = Player = get_player(ServerID,RoleID),
	OccupyNeedTimeInterval = data_conquerisland:get(occupy_need_time),
	NowSec = util:now(),
	case State of
		?CENTRE_STATE_OCCUPIED->
			%%占领阶段发生抢夺事件，触发据点所属权的变更
			OccupyFinishTime = NowSec+OccupyNeedTimeInterval,
			NewCentre = case Type of
				?PLAYER_TYPE_DEFENDER->
					Centre#centre{snatcherType=?PLAYER_TYPE_DEFENDER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPING,beOwnSec=OccupyFinishTime};
				?PLAYER_TYPE_ATTACKER->
					Centre#centre{snatcherType=?PLAYER_TYPE_ATTACKER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPING,beOwnSec=OccupyFinishTime};
				_->
					Centre
			end,
			%%发生抢夺，取消对应buff
			delete_centre_buff(BuffType,OldOwnerType,Value),
			%%添加据点所属权变更事件
			F = fun()->erlang:send(self(),{occupy_finish,CentreID,ServerID,RoleID}) end,
			Plan = #plan{sec=OccupyFinishTime,key={centre,CentreID},value=F},
			add_plan(Plan);
		?CENTRE_STATE_OCCUPING->
			case OldOwnerType =:= Type of
				true-> 
					OccupyFinishTime = NowSec,
					%%占领中发生抢夺事件，旧所属权属于抢夺者，只是更改据点状态(据点争夺回来)
					NewCentre = case Type of
						?PLAYER_TYPE_DEFENDER->
							Centre#centre{snatcherType=?PLAYER_TYPE_DEFENDER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPIED,ownerType=?PLAYER_TYPE_DEFENDER,beOwnSec=OccupyFinishTime,ownerRole={ServerID,RoleID}};
						?PLAYER_TYPE_ATTACKER->
							Centre#centre{snatcherType=?PLAYER_TYPE_ATTACKER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPIED,ownerType=?PLAYER_TYPE_ATTACKER,beOwnSec=OccupyFinishTime,ownerRole={ServerID,RoleID}};
						_->
							Centre
					end,
					%%取消被抢夺者抢夺倒计时
					delete_plan({centre,CentreID}),
					%%夺回据点，增加次数
					NewPlayer = add_player_occupytimes(Player,1),
					set_player(NewPlayer),
					%%夺回据点，增加buff
					%%加上buff的过程中，已经更新了player，故此处不能再重新设置
					add_centre_buff(BuffType,Type,Value),
					add_sys_msg(zhanling,RoleName,CentreID);
				false->
					%%占领中发生抢夺事件，旧所属权不属于抢夺者，触发抢夺倒计时
					OccupyFinishTime = NowSec+OccupyNeedTimeInterval,
					NewCentre = case Type of
						?PLAYER_TYPE_DEFENDER->
							Centre#centre{snatcherType=?PLAYER_TYPE_DEFENDER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPING,beOwnSec=OccupyFinishTime};
						?PLAYER_TYPE_ATTACKER->
							Centre#centre{snatcherType=?PLAYER_TYPE_ATTACKER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPING,beOwnSec=OccupyFinishTime};
						_->
							Centre
					end,
					%%发生抢夺，取消对应buff
					% delete_centre_buff(BuffType,OldOwnerType,Value),
					%%添加据点所属权变更事件
					F = fun()->erlang:send(self(),{occupy_finish,CentreID,ServerID,RoleID}) end,
					Plan = #plan{sec=OccupyFinishTime,key={centre,CentreID},value=F},
					add_plan(Plan)
			end;
		?CENTRE_STATE_UNOCCUPY->
			%%未占领阶段发生抢夺事件，触发据点所属权的变更
			%% 此处只是修改抢夺者的ID,类型,据点状态以及倒计时
			OccupyFinishTime = NowSec+OccupyNeedTimeInterval,
			NewCentre = case Type of
				?PLAYER_TYPE_DEFENDER->
					Centre#centre{snatcherType=?PLAYER_TYPE_DEFENDER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPING,beOwnSec=OccupyFinishTime};
				?PLAYER_TYPE_ATTACKER->
					Centre#centre{snatcherType=?PLAYER_TYPE_ATTACKER,snatcherRoleID={ServerID,RoleID},state=?CENTRE_STATE_OCCUPING,beOwnSec=OccupyFinishTime};
				_->
					Centre
			end,
			%%发生抢夺，取消对应buff
			% delete_centre_buff(BuffType,OldOwnerType,Value),
			%%添加据点所属权变更事件
			F = fun()->erlang:send(self(),{occupy_finish,CentreID,ServerID,RoleID}) end,
			Plan = #plan{sec=OccupyFinishTime,key={centre,CentreID},value=F},
			add_plan(Plan)
	end,
	set_centre(NewCentre),
	%%添加到更新据点列表中
	add_centre_update_list(CentreID),
	%%将占领信息发送给占领者
	broadcast_war_change_to_role([],[NewCentre],[],[{ServerID,RoleID}]),
	{true,OccupyFinishTime}.


check_centre_occupy(ServerID,RoleID,CentreID)->
	case get_centre(CentreID) of
		?undefined->
			{false,2,0};
		#centre{ownerType=OwnerType,snatcherType=SnatcherType,state=CentreState,beOwnSec=BeOwnSec}=Centre->
			case is_player_alive(ServerID,RoleID) of
				false->
					{false,7,BeOwnSec};
				true->
					case is_in_centre(ServerID,RoleID,Centre) of
						false->
							{false,3,BeOwnSec};
						{true,_Type}->
							#conquerisland_player{type=Type} = get_player(ServerID,RoleID),
							case (CentreState=:=?CENTRE_STATE_OCCUPIED andalso OwnerType=:=Type) of
								true->
									%%我方已经占领
									{false,4,BeOwnSec};
								false->
									case SnatcherType=:=Type andalso CentreState=:=?CENTRE_STATE_OCCUPING of
										true->
											%%我方正在占领
											{false,6,BeOwnSec};
										false->
											EnemyType = case Type of ?PLAYER_TYPE_DEFENDER->?PLAYER_TYPE_ATTACKER;_->?PLAYER_TYPE_DEFENDER end,
											case get_centre_player_by_type(CentreID,EnemyType) of
												0->
													true;
												_->
													{false,5,BeOwnSec}
											end
									end
							end
					end
			end
	end.

check_attack(ServerID,RoleID,TarServerID,TarRoleID,TarCentreID)->
	case {is_in_centre(ServerID,RoleID,TarCentreID),is_player_alive(ServerID,RoleID)} of
		{false,_}->
			{false,3};
		{_,false}->
			{false,6};
		{{true,AttackerType},true}->
			case is_in_centre(TarServerID,TarRoleID,TarCentreID) of
				false->
					{false,5};
				{true,DefenderType}->
					case AttackerType=/=DefenderType of
						false->
							{false,4};
						true->
							true
					end
			end
	end.

do_attack2(ServerID,RoleID,TarServerID,TarRoleID,TarCentreID)->
	#conquerisland_player{type=TypeA,fighters=FightersA,addBuff=BuffA,talent=TalentA
		   ,itemList=ItemListA,replayList=RLA,roleName=RoleNameA,trSpecial=TrSpecialA,skin_info=SkinInfoA} = PlayerA = get_player(ServerID,RoleID),
	#conquerisland_player{type=TypeD,fighters=FightersD,addBuff=BuffD,talent=TalentD
		   ,itemList=ItemListD,replayList=RLD,roleName=RoleNameD,trSpecial=TrSpecialD,skin_info=SkinInfoD} = PlayerD = get_player(TarServerID,TarRoleID),
    GerEquipList1 = role_item:assort_ger_equiplist(ItemListA),
    LegendAddList1 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList1],
    GerEquipList2 = role_item:assort_ger_equiplist(ItemListD),
    LegendAddList2 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList2],
	case ?CATCH(role_fight:new(filter_out_zero_hp(FightersA),filter_out_zero_hp(FightersD)
					,BuffA,BuffD,TalentA,TalentD,TrSpecialA,TrSpecialD,SkinInfoA,SkinInfoD,LegendAddList1,LegendAddList2)) of
		{IsWin, FightRecord0, {_,_,NewFightersA,NewFightersD}} ->
			FighterList2 = role_data:get_FighterList_with_effect(ItemListA,ItemListD,FightRecord0#sc_fight_request.fighterList),
			FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
			ReplayUID = tk_id:gen_replayUID(),
			%%此处调用replay_server保存战斗记录
			conquerisland_replay_server:record_replay(ReplayUID,FightRecord),
			ReplayDtl = #p_carlos_replay_dtl{replayUID=ReplayUID,isRole1Win=bool2int(IsWin),role1Name=RoleNameA,role2Name=RoleNameD},
			NewRLA = lists:sublist([ReplayDtl|RLA],data_conquerisland:get(max_replay_num)),
			NewRLD = lists:sublist([ReplayDtl|RLD],data_conquerisland:get(max_replay_num)),
			%%调整出战精灵列表的血量
			NewFightersA1 = recalc_fighter_list(FightersA, NewFightersA),
			NewFightersD1 = recalc_fighter_list(FightersD, NewFightersD),
			BloodA = calc_hp_percent(NewFightersA1),
			BloodD = calc_hp_percent(NewFightersD1),
			if 
				BloodA == 0 andalso BloodD == 0 ->
					%%更新player的血量，日志记录，以及当前出战精灵
					% NewPlayerA = update_player_fighters(PlayerA,BloodA,ReplayDtl,NewFightersA1,1,0,0),
					% NewPlayerD = update_player_fighters(PlayerD,BloodD,ReplayDtl,NewFightersD1,1,0,0),
					NewPlayerA = update_player_fighters(PlayerA,BloodA,NewRLA,NewFightersA1),
					NewPlayerD = update_player_fighters(PlayerD,BloodD,NewRLD,NewFightersD1),
					NewPlayerA2 = add_player_killNum(NewPlayerA,1),
					NewPlayerD2 = add_player_killNum(NewPlayerD,1),
					%%设置player重生时间，用以确定死亡状态，重生地点在重生时候确定
					NewPlayerA1 = prepare_reborn_player(NewPlayerA2),
					NewPlayerD1 = prepare_reborn_player(NewPlayerD2),
					set_player(NewPlayerA1),
					set_player(NewPlayerD1),
					%%修改据点信息
					NewCentre = delete_player_from_centre(TypeD,{TarServerID,TarRoleID},TarCentreID),
					NewCentre1 = delete_player_from_centre(TypeA,{ServerID,RoleID},NewCentre),
					set_centre(NewCentre1),
					%%将状态的更改信息同步到战斗的双方，其他的玩家等待秒同步
					broadcast_war_change_to_role([NewPlayerA1,NewPlayerD1],[],[],[{TarServerID,TarRoleID},{ServerID,RoleID}]),
					add_player_update_list({TarServerID,TarRoleID}),
					add_player_update_list({ServerID,RoleID}),
					add_centre_update_list(TarCentreID),
					{true,2,[FightRecord]};
				true ->
					if 
						IsWin ->
							NewPlayerA = update_player_fighters(PlayerA,BloodA,NewRLA,NewFightersA1),
							NewPlayerD = update_player_fighters(PlayerD,BloodD,NewRLD,NewFightersD1),
							NewPlayerA1 = add_player_killNum(NewPlayerA,1),
							NewPlayerD1 = prepare_reborn_player(NewPlayerD),
							set_player(NewPlayerA1),
							set_player(NewPlayerD1),
							NewCentre = delete_player_from_centre(TypeD,{TarServerID,TarRoleID},TarCentreID),
							set_centre(NewCentre),
							%%将状态的更改信息同步到战斗的双方，其他的玩家等待秒同步
							broadcast_war_change_to_role([NewPlayerA1,NewPlayerD1],[],[],[{TarServerID,TarRoleID},{ServerID,RoleID}]),
							add_player_update_list({TarServerID,TarRoleID}),
							add_player_update_list({ServerID,RoleID}),
							add_centre_update_list(TarCentreID),
							{true, 1,[FightRecord]};
						true ->
							NewPlayerA = update_player_fighters(PlayerA,BloodA,NewRLA,NewFightersA1),
							NewPlayerD = update_player_fighters(PlayerD,BloodD,NewRLD,NewFightersD1),
							NewPlayerA1 = prepare_reborn_player(NewPlayerA),
							set_player(NewPlayerA1),
							NewPlayerD1 = add_player_killNum(NewPlayerD,1),
							set_player(NewPlayerD1),
							NewCentre = delete_player_from_centre(TypeA,{ServerID,RoleID},TarCentreID),
							set_centre(NewCentre),
							broadcast_war_change_to_role([NewPlayerA1,NewPlayerD1],[],[],[{TarServerID,TarRoleID},{ServerID,RoleID}]),
							add_player_update_list({TarServerID,TarRoleID}),
							add_player_update_list({ServerID,RoleID}),
							add_centre_update_list(TarCentreID),
							{true,2,[FightRecord]}
					end
			end;
		{'EXIT', _} ->
			{false,5}
	end.

%%完成player复活功能
do_player_reborn(ServerID,RoleID)->
	case get_player(ServerID,RoleID) of
		?undefined->
			{false,no_player};
		#conquerisland_player{rebornSec=RebornTime}->
			NowSec = util:now(),
			case RebornTime < NowSec of
				true->
					{false,3};
				false->
					reborn_player(ServerID,RoleID),
					%%删除玩家的重生事件
					NewPlayer = get_player(ServerID,RoleID),
					delete_plan({reborn,ServerID,RoleID}),
					broadcast_war_change_to_role([NewPlayer],[],[],[{ServerID,RoleID}]),
					true
			end
	end.

sec_tick()->
	erlang:send_after(1000, self(), sec_tick).

plan()->
	Plans = get_plan(),
	Now = util:now(),
	PlansAfterExecute = plan2(Plans,Now),
	set_plan(PlansAfterExecute).

plan2(Plans,NowSec)->
	lists:foldl(fun(#plan{sec=Sec,value=Value}=H,Acc)->
		if
			NowSec >= Sec ->
				execute_plan(Value),
				Acc;
			true->
				[H|Acc]
		end
	end,[],Plans).

set_plan(Plans)->
	put(?plan,Plans).
get_plan()->
	case get(?plan) of
		?undefined->
			[];
		L ->
			L
	end.
add_plan(Plan)->
	Plans = get_plan(),
	add_plan(Plan,Plans).

add_plan(#plan{key=K}=Plan,Plans)->
	% Plans2 = lists:keydelete(K,#plan.key,Plans),
	Plans2 = [P||#plan{key=Key}=P<-Plans,Key=/=K],
	set_plan([Plan|Plans2]).
delete_plan(Key)->
	Plans = get_plan(),
	Plans2 = lists:keydelete(Key,#plan.key,Plans),
	set_plan(Plans2).

execute_plan(Value) ->
	?LOOSE_CATCH(Value()).

broadcast_war_change()->
	{PlayerChange,CentreChange,BossChange} = get_war_change(),
	clear_all_change(),
	NeedBroadcastRoleList = get(?bcList),
	broadcast_war_change_to_role(PlayerChange,CentreChange,BossChange,NeedBroadcastRoleList).
broadcast_war_change_to_role([],[],[],_NeedBroadcastRoleList)->
	%%没有任何变化，不进行广播
	ignore;
broadcast_war_change_to_role(PlayerChange,CentreChange,BossChange,NeedBroadcastRoleList)->
	%%此处广播协议到所有的战场玩家，将三种变化进行广播
	PPlayerList = conquerisland_player2pPlayer(PlayerChange,[]),
	PCenterList = conquerisland_centre2pCentre(CentreChange,[]),
	PBossList = conquerisland_boss2pBoss(BossChange,[]),
	Msg = #sc_conquerisland_update_war{updatePlayerList=PPlayerList,updateCentreList=PCenterList,updateBossList=PBossList},
	broadcast_msg(Msg,NeedBroadcastRoleList).

broadcast_msg(Msg,NeedBroadcastRoleList)->
	lists:foreach(fun({ServerID,RoleID})->conquerisland_router:send_msg_to_client(ServerID,RoleID,Msg) end,NeedBroadcastRoleList).

%%将数据持久化任务加入任务列表
add_dump()->
	F = fun()->erlang:send(?MODULE,dump_interval_tick) end,
	add_plan(#plan{sec=util:now()+?Dump_interval,key=dump_interval,value=F}).

%%广播战斗开始消息
broadcast_war_begin()->
	Msg = generate_war_base_info(),
	NeedBroadcastRoleList = get(?bcList),
	spawn(fun()->lists:foreach(fun({ServerID,RoleID})->conquerisland_router:send_msg_to_client(ServerID,RoleID,Msg) end,NeedBroadcastRoleList) end).

%%发送消息到对应的玩家
send_msg_to_client(ServerID,RoleID,Msg)->
	conquerisland_router:send_msg_to_client(ServerID,RoleID,Msg).

%%发送消息到某一方阵营
send_msg_to_client_by_type(Type,Msg)->
	PlayerList = get({?bcList,Type}),
	[send_msg_to_client(ServerID,RoleID,Msg)||{ServerID,RoleID}<-PlayerList].

set_player(ServerID,RoleID,Player)->
	put({?player,ServerID,RoleID},Player).
set_player(#conquerisland_player{serverID=ServerID,roleID=RoleID}=Player)->
	% ?ERR("RoleID:~w SpeedPlus:~w ~n",[RoleID,SpeedPlus]),
	set_player(ServerID,RoleID,Player).
get_player(ServerID,RoleID)->
	get({?player,ServerID,RoleID}).


%  更新conquerisland_player的起止位置到当前移动位置
reset_player(#conquerisland_player{startPos=Pos,endPos=Pos}=Player) ->
	Player;
reset_player(#conquerisland_player{startPos=Pos0,endPos=Pos1,startTime=ST,baseSpeed=BaseSpeed,movDis=MovDis0,speedPlus=SpeedPlus}=Player) ->
	Now=util:now(),
	TimeDiff = Now-ST,
	MovDis = trunc((BaseSpeed*(1+SpeedPlus/10000)) * TimeDiff),
	NowPos = calc_new_pos(Pos0,Pos1,MovDis),
	% ?INFO("reset_player: StartPos:~w endPos:~w TimeDiff:~w MovDis:~w NewPos:~w ~n",[Pos0,Pos1,TimeDiff,MovDis,NowPos]),
	Player#conquerisland_player{startPos=NowPos,endPos=NowPos,startTime=Now,movDis=MovDis0+MovDis}.

calc_new_pos(Pos0,Pos0,_) ->
	Pos0;
calc_new_pos({A,B},{C,D}, Dis) ->
	Dx = A-C,
	Dy = B-D,
	DL = math:sqrt(Dx*Dx+Dy*Dy),
	Tx = trunc((Dx*Dis)/DL)-1,
	Ty = trunc((Dy*Dis)/DL)-1,
	{A-Tx,B-Ty}.

calc_time({X1,Y1},{X2,Y2},Speed) ->
	Dx = X1 - X2,
	Dy = Y1 - Y2,
	Len = math:sqrt(Dx * Dx + Dy * Dy),
	calc_time(Len,Speed).
calc_time(Len,0)->
	Len;
calc_time(Len,Speed)->
	trunc(Len/Speed).

%%判断玩家是否在某个据点
is_in_centre(ServerID,RoleID,CentreID) when is_number(CentreID)->
	case get_centre(CentreID) of
		?undefined->
			false;
		Centre->
			is_in_centre(ServerID,RoleID,Centre)
	end;
is_in_centre(ServerID,RoleID,Centre)->
	#centre{attackerList=AttackerList,defenderList=DefenderList} = Centre,
	case lists:member({ServerID,RoleID},AttackerList) of
		true->
			{true,attacker};
		false->
			case lists:member({ServerID,RoleID},DefenderList) of
				true->
					{true,defender};
				false->
					false
			end
	end.

is_player_alive(ServerID,RoleID)->
	case get_player(ServerID,RoleID) of
		?undefined->
			false;
		Player->
			is_player_alive(Player)
	end.
is_player_alive(#conquerisland_player{rebornSec=RebornSec,blood=Blood})->
	RebornSec =< util:now() andalso Blood > 0.

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

calc_hp_percent(FighterList) ->
	{SumHP, SumHPMax} = lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
											if HP < 0->
												   {AccHP,AccHPMax+HPMax};
											   true ->
												   
												   {AccHP + HP, AccHPMax + HPMax}
											end
									end, {0, 0}, FighterList),
	B = erlang:trunc(SumHP / SumHPMax * 100),
	if SumHP =< 0 ->
		   0;
	   true ->
		   if SumHP == SumHPMax->
				  100;
			  true ->
				  B
		   end
	end.

calc_hp_percent(SumHP,SumHPMax) ->
    erlang:trunc(SumHP / SumHPMax * 100).

update_player_fighters(Player,Blood,Replay,Fighters)->
	Player#conquerisland_player{blood=Blood,replayList=Replay,fighters=Fighters}.

add_player_killNum(Player,AddKillNum)->
	#conquerisland_player{killNum=OldKillNum} = Player,
	NewPlayer = Player#conquerisland_player{killNum=erlang:max(0,OldKillNum+AddKillNum)},
	add_fight_rank(NewPlayer,?FIGHT_RESULT_NO_FINISH),
	NewPlayer.

add_player_occupytimes(Player,AddOccupyTimes)->
	#conquerisland_player{occupyCentreTimes=OldOccupyTimes} = Player,
	NewPlayer = Player#conquerisland_player{occupyCentreTimes=erlang:max(0,OldOccupyTimes+AddOccupyTimes)},
	add_fight_rank(NewPlayer,?FIGHT_RESULT_NO_FINISH),
	NewPlayer.

add_player_bossdemage(Player,AddBossDemage)->
	#conquerisland_player{bossDemage=OldBossDemage,bossfighttimes=OldBossFightTimes} = Player,
	NewPlayer = Player#conquerisland_player{bossDemage=erlang:max(0,OldBossDemage+AddBossDemage),bossfighttimes=OldBossFightTimes+1},
	add_fight_rank(NewPlayer,?FIGHT_RESULT_NO_FINISH),
	NewPlayer.

%%由于需要在重生的时间点确定玩家重生的位置，所以此处需要加入一个重生事件
prepare_reborn_player(Player)->
	RebornInterval = data_conquerisland:get(reborn_interval),
	prepare_reborn_player(Player,RebornInterval).

prepare_reborn_player(#conquerisland_player{serverID=ServerID,roleID=RoleID,type=Type}=Player,RebornInterval) ->
	RebornTime = util:now()+RebornInterval,
	F = fun()->erlang:send(self(),{reborn,ServerID,RoleID}) end,
	Plan = #plan{sec=RebornTime,key={reborn,ServerID,RoleID},value=F},
	add_plan(Plan),
	Pos = get_player_reborn_pos(Type),
	Player#conquerisland_player{rebornSec=RebornTime,blood=0,startPos=Pos,endPos=Pos}.


%%将player重生，主要是修改player的开始和结束位置
reborn_player(ServerID,RoleID)->
	case get_player(ServerID,RoleID) of
		?undefined->
			ignore;
		Player=#conquerisland_player{type=Type,baseFighters=BF}->
			Pos = get_player_reborn_pos(Type),
			NewPlayer = Player#conquerisland_player{startPos=Pos,endPos=Pos,tarCentre=0,fighters=BF,blood=100,startTime=0,rebornSec=util:now()},
			set_player(NewPlayer),
			add_player_update_list({ServerID,RoleID}),
			broadcast_war_change_to_role([NewPlayer],[],[],[{ServerID,RoleID}])
	end.

delete_player_from_centre(Type,{ServerID,RoleID},CentreID) when is_number(CentreID)->
	% ?ERR("Type:~w ServerID:~w RoleID:~w CentreID:~w ~n",[Type,ServerID,RoleID,CentreID]),
	case get_centre(CentreID) of
		?undefined->
			ignore;
		Centre->
			delete_player_from_centre(Type,{ServerID,RoleID},Centre)
	end;
delete_player_from_centre(?PLAYER_TYPE_ATTACKER,{ServerID,RoleID},Centre=#centre{attackerList=AttackerList,attackerNum=_OldAttackerNum})->
	case lists:member({ServerID,RoleID},AttackerList) of
		true->
			% NewAttackerList = lists:delete({ServerID,RoleID},AttackerList),
			NewAttackerList = [P||P<-AttackerList,P=/={ServerID,RoleID}],
			Num = length(NewAttackerList),
			% case Num=/=OldAttackerNum-1 of true->?ERR("delete player:~w in centre:~w exist more than one~n",[{ServerID,RoleID},Centre]);_->ignore end,
			Centre#centre{attackerList=NewAttackerList,attackerNum=Num};
		false->
			% ?ERR("leavel centre：~w and not find player:~w type:attacker ~n",[Centre,{ServerID,RoleID}]),
			Centre
	end;
delete_player_from_centre(?PLAYER_TYPE_DEFENDER,{ServerID,RoleID},Centre=#centre{defenderList=DefenderList,defenderNum=_OldDefenderNum})->
	case lists:member({ServerID,RoleID},DefenderList) of
		true->
			% NewDefenderList = lists:delete({ServerID,RoleID},DefenderList),
			NewDefenderList = [P||P<-DefenderList,P=/={ServerID,RoleID}],
			Num = length(NewDefenderList),
			% case Num=/=OldDefenderNum-1 of true->?ERR("delete player:~w in centre:~w exist more than one~n",[{ServerID,RoleID},Centre]);_->ignore end,
			Centre#centre{defenderList=NewDefenderList,defenderNum=Num};
		false->
			% ?ERR("leavel centre：~w and not find player:~w type:defender ~n",[Centre,{ServerID,RoleID}]),
			Centre
	end.

%%计算boss初始血量
calculate_boss_init_hp(PlayerList)->
	calculate_boss_init_hp(PlayerList,0).
calculate_boss_init_hp([],Acc)->
	trunc(Acc/2);
calculate_boss_init_hp([H|T],Acc)->
	#base_player{roleLevel=RoleLevel} = H,
	ConfigList = data_conquerisland:get(role_level_boss_hp_map),
	F = fun({{Begin,End},_Value})-> Begin =< RoleLevel andalso End >= RoleLevel end,
	{_Field,Value} = util:fun_find(F,ConfigList),
	calculate_boss_init_hp(T,Acc+Value).

base_player2conquerisland_player(BasePlayer) when is_record(BasePlayer,base_player)->
#base_player{serverID=ServerID,roleID=RoleID,roleName=RoleName,roleLevel=RoleLevel,head=Head,title=Title,isMale=IsMale
				,fighters=Fighters,addBuff=AddBuff,talent=Talent,itemList=ItemList,fly=Fly,trSpecial=TrSpecial,grade=Grade
                ,fight_power=FightPower,skin_info=SkinInfo,type=Type,speed=Speed} = BasePlayer,
#conquerisland_player{serverID=ServerID,roleID=RoleID,roleName=RoleName,roleLevel=RoleLevel,head=Head,title=Title,isMale=IsMale
					,talent=Talent,itemList=ItemList,grade=Grade,fly=Fly,trSpecial=TrSpecial,fight_power=FightPower,skin_info=SkinInfo
					,type=Type,baseFighters=Fighters,blood=100,startPos={p_pos,0,0},tarCentre=0,startTime=0,baseSpeed=Speed
					,fightPowerForBoss=init_player_fightpower_for_boss(BasePlayer),fighters=Fighters,addBuff=AddBuff}. 

get_fly_base_speed(Type) ->
    Speed = data_carlos:get({speed, Type}),
    if is_integer(Speed) ->
           Speed;
        true ->
            1
    end. 

filter_out_zero_hp(List) ->
    lists:filter(fun(#ger{gerHp=GerHP}) ->
                         GerHP > 0
                 end, List).

bool2int(true ) ->
	1 ;
bool2int(_) ->
	0.

set_player_reborn_pos(Type,Pos) ->
	put({?reborn_pos,Type},Pos).
get_player_reborn_pos(Type)->
	get({?reborn_pos,Type}).

conquerisland_player2pPlayer([],Acc)->
	Acc;
conquerisland_player2pPlayer([H|T],Acc)->
	#conquerisland_player{serverID=ServerID,roleID=RoleID,roleName=RoleName,roleLevel=RoleLevel,fly=Fly,
			fight_power=FightPower,type=Type,blood=Blood,rebornSec=RebornSec,startPos=StartPos,endPos=EndPos,
			tarCentre=TarCentre,startTime=StartSec,speedPlus=SpeedPlus,fightPowerForBoss=BossFightPower,baseSpeed=Speed} = H,
	PPlayer = #p_player{serverId=ServerID,roleId=RoleID,roleName=RoleName,roleLevel=RoleLevel,fly=Fly,
			fightPower=FightPower,type=Type,blood=Blood,rebornSec=RebornSec,startPos=pos2ppos(StartPos),
			endPos=pos2ppos(EndPos),centreId=TarCentre,startSec=StartSec,speedadd=SpeedPlus
			,bossfightpower=BossFightPower,speed=Speed},
	conquerisland_player2pPlayer(T,[PPlayer|Acc]).

conquerisland_boss2pBoss([],Acc)->
	Acc;
conquerisland_boss2pBoss([H|T],Acc)->
	#bossinfo{bossId=BossID,bossHp=BossHp,bossMaxHp=BossMaxHp,bossPos=BossPos,bossLevel=BossLevel,type=Type,hpPlus=HpPlus,critPlus=CritPlus,demagePlus=DemagePlus,demageDec=DemageDec}=H,
	PBoss = #p_conquerisland_boss{bossId=BossID,bossPos=pos2ppos(BossPos),bossDemagePlus=DemagePlus,bossHpAdd=HpPlus,bossCritPlus=CritPlus,bossDemageDec=DemageDec,bossHpMax=BossMaxHp,bossHp=BossHp,bossLevel=BossLevel,bossType=Type},
	conquerisland_boss2pBoss(T,[PBoss|Acc]).

conquerisland_centre2pCentre([],Acc)->
	Acc;
conquerisland_centre2pCentre([H|T],Acc)->
	#centre{id=ID,pos=Pos,snatcherType=SnatcherType,attackerNum=AttackerNum,defenderNum=DefenderNum,beOwnSec=BeOwnSec}=H,
	{_Pos,BuffType,_Value} = data_conquerisland:get({centre,ID}),
	PCentre = #p_centre{centreId=ID,owner=SnatcherType,attackerNum=AttackerNum,defenderNum=DefenderNum,buffId=BuffType,centrePos=pos2ppos(Pos),ownSec=BeOwnSec},
	conquerisland_centre2pCentre(T,[PCentre|Acc]).

generate_player_dtl(Player)->
	#conquerisland_player{fighters=Fighters,replayList=ReplayList,grade=Grade} = Player,
	GerList = ger2pConquereidlandGer(Fighters),
	#sc_conquerisland_role_dtl{result=1,gerList=GerList,replayList=ReplayList,grade=Grade}.

ger2pConquereidlandGer(FL) when is_list(FL) ->
	[ger2pConquereidlandGer(F)||F<-FL];
ger2pConquereidlandGer(#ger{gerBase=#gerBase{gerTypeID=TypeID,gerQuality=Rank,gerLevel=Level}
					  ,gerAttr=#gerAttr{gerHpMax=HpMax},gerHp=Hp})->
	#p_conquerisland_ger{gerTypeID=TypeID,gerRank=Rank,gerLevel=Level,gerMaxHp=HpMax,gerHp=Hp}.

pos2ppos(Pos) when is_record(Pos,p_pos)->
	Pos;
pos2ppos({X,Y})->
	#p_pos{x=X,y=Y}.

get_centre_player_by_type(CentreID,Type) when is_number(CentreID)->
	case get_centre(CentreID) of
		?undefined->
			0;
		Centre->
			get_centre_player_by_type(Centre,Type)
	end;

get_centre_player_by_type(Centre,Type)->
	case Type of
		?PLAYER_TYPE_DEFENDER->
			Centre#centre.defenderNum;
		?PLAYER_TYPE_ATTACKER->
			Centre#centre.attackerNum;
		_->
			0
	end.


%%攻击BOSS
do_attack_boss(BossID,ServerID,RoleID)->
	#bossinfo{bossHp=BossHp,demagePlus=DemagePlus,critPlus=CritPlus,demageDec=DemageDec,bossPos=BossPos}=Boss = get_boss(BossID),
	#conquerisland_player{fightPowerForBoss=FightPowerForBoss}=Player1 = get_player(ServerID,RoleID),
	%%主要是更新玩家的移动距离
	Player = reset_player(Player1),
	BaseDemage = FightPowerForBoss*(1+DemagePlus/10000)*(erlang:max(0,1-DemageDec/10000)),
	Demage1 = case random:uniform(10000) < CritPlus of
		true->
		%%发生暴击
			BaseDemage * data_conquerisland:get(crit_mult);
		false->
			BaseDemage
	end,
	Demage = trunc(Demage1),
	NewHP = BossHp-Demage,
	case NewHP > 0 of
		true->
			NewBoss = Boss#bossinfo{bossHp=NewHP},
			set_boss(NewBoss),
			%%将Boss变化加入下次更新中
			add_boss_update_list(BossID),
			%%将对boss的伤害加入到player中并更新排行榜
			NewPlayer= add_player_bossdemage(Player,Demage),
			AttackerBossRebornInterval = data_conquerisland:get(attack_boss_reborn_interval),
			NewPlayer1 = prepare_reborn_player(NewPlayer,AttackerBossRebornInterval),
			NowSec = util:now(),
			%%此处特殊处理 将重生倒计时去掉
			set_player(NewPlayer1#conquerisland_player{startPos=BossPos,endPos=BossPos,tarCentre=0,startTime=NowSec,blood=0,rebornSec=NowSec}),
			add_player_update_list({ServerID,RoleID});
		false->
			NewBoss = Boss#bossinfo{bossHp=0},
			set_boss(NewBoss),
			add_boss_update_list(BossID),
			% ?INFO("Boss is killed~n"),
			NewPlayer = add_player_bossdemage(Player,BossHp),
			set_player(NewPlayer),
			erlang:send(self(),do_end_war)
	end.

do_arrive_centre(CentreID,ServerID,RoleID)->
	Centre=#centre{attackerList=AttackerList,defenderList=DefenderList,defenderNum=_DefenderNum,pos=Pos} = get_centre(CentreID),
	#conquerisland_player{type=Type} =Player1 = get_player(ServerID,RoleID),
	check_player_centre_exist(ServerID,RoleID,Type,CentreID),
	Player = reset_player(Player1),
	case Type of
		?PLAYER_TYPE_DEFENDER->
			NewDefenderList = case lists:member({ServerID,RoleID},DefenderList) of 
				true->
					% ?ERR("arrive centre:~w Player:~w is exist already~n",[Centre,{ServerID,RoleID}]),
					DefenderList;
				false->
					[{ServerID,RoleID}|DefenderList]
			end,
			Num = length(NewDefenderList),
			NewCentre = Centre#centre{defenderList=NewDefenderList,defenderNum=Num};
		?PLAYER_TYPE_ATTACKER->
			NewAttackerList = case lists:member({ServerID,RoleID},AttackerList) of 
				true->
					% ?ERR("arrive centre:~w Player:~w is exist already~n",[Centre,{ServerID,RoleID}]),
					AttackerList;
				false->
					[{ServerID,RoleID}|AttackerList]
			end,
			Num = length(NewAttackerList),
			NewCentre = Centre#centre{attackerList=NewAttackerList,attackerNum=Num}
	end,
	set_centre(NewCentre),
	%%修改Player的startpos,endpos，以及starttime
	set_player(Player#conquerisland_player{startPos=Pos,endPos=Pos,startTime=util:now()}),
	add_centre_update_list(CentreID),
	add_player_update_list({ServerID,RoleID}).

%%据点抢夺倒计时完成
do_occupy_finish(CentreID,ServerID,RoleID)->
	#conquerisland_player{type=Type,roleName=RoleName}= Player =get_player(ServerID,RoleID),
	Centre = get_centre(CentreID),
	case Type of
		?PLAYER_TYPE_ATTACKER->
			NewCentre = Centre#centre{ownerRole={ServerID,RoleID},state=?CENTRE_STATE_OCCUPIED,ownerType=?PLAYER_TYPE_ATTACKER,snatcherType=?PLAYER_TYPE_ATTACKER};
		?PLAYER_TYPE_DEFENDER->
			NewCentre = Centre#centre{ownerRole={ServerID,RoleID},state=?CENTRE_STATE_OCCUPIED,ownerType=?PLAYER_TYPE_DEFENDER,snatcherType=?PLAYER_TYPE_DEFENDER}
	end,
	set_centre(NewCentre),
	add_centre_update_list(CentreID),
	NewPlayer = add_player_occupytimes(Player,1),
	set_player(NewPlayer),
	{_Pos,BuffType,Value} = data_conquerisland:get({centre,CentreID}),
	%%更新所有据点的占据情况，修改对应的buff效果
	add_sys_msg(zhanling,RoleName,CentreID),
	% update_centre_buff(BuffType,OldOwnerType,Type,Value).
	%%抢占完成，增加buff
	add_centre_buff(BuffType,Type,Value).

% update_centre_buff(BuffType,OldOwner,NewOwner,Value,CentreState)->
% 	?ERR("update_centre_buff：BuffType:~w OldOwner:~w NewOwner:~w Value:~w ~n",[BuffType,OldOwner,NewOwner,Value]),
% 	case OldOwner=:= NewOwner of
% 		true->
% 			ignore;
% 		false->
% 			case OldOwner =/= 0 of
% 				true->
% 					delete_centre_buff(BuffType,OldOwner,Value),
% 					add_centre_buff(BuffType,NewOwner,Value);
% 				false->
% 					add_centre_buff(BuffType,NewOwner,Value)
% 			end
% 	end.

delete_centre_buff(_BuffType,0,_Value)->
	ignore;
%%减少本方BOSS伤害
delete_centre_buff(?CENTRE_BUFF_BOSS_DEMAGE_DEC,OldOwnerType,Value)->
	BossID = get_player_bossid(OldOwnerType),
	OldBoss = #bossinfo{demageDec=OldDemageDec} = get_boss(BossID),
	NewBoss = OldBoss#bossinfo{demageDec=erlang:max(0,OldDemageDec-Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);

%%增加对方BOSS伤害
delete_centre_buff(?CENTRE_BUFF_BOSS_DEMAGE_PLUS,OldOwnerType,Value)->
	EnemyType = case OldOwnerType of ?PLAYER_TYPE_DEFENDER->?PLAYER_TYPE_ATTACKER;?PLAYER_TYPE_ATTACKER->?PLAYER_TYPE_DEFENDER end,
	BossID = get_player_bossid(EnemyType),
	OldBoss = #bossinfo{demagePlus=OldDemagePlus} = get_boss(BossID),
	NewBoss = OldBoss#bossinfo{demagePlus=erlang:max(0,OldDemagePlus-Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);	

delete_centre_buff(?CENTRE_BUFF_BOSS_CRIT_PLUS,OldOwnerType,Value)->
	EnemyType = case OldOwnerType of ?PLAYER_TYPE_DEFENDER->?PLAYER_TYPE_ATTACKER;?PLAYER_TYPE_ATTACKER->?PLAYER_TYPE_DEFENDER end,
	BossID = get_player_bossid(EnemyType),
	OldBoss = #bossinfo{critPlus=OldCritPlus} = get_boss(BossID),
	NewBoss = OldBoss#bossinfo{critPlus=erlang:max(0,OldCritPlus-Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);

delete_centre_buff(?CENTRE_BUFF_BOSS_HP_PLUS,OldOwnerType,Value)->
	BossID = get_player_bossid(OldOwnerType),
	OldBoss = #bossinfo{bossHp=BossHp,bossMaxHp=BossMaxHp,hpPlus=HpPlus} = get_boss(BossID),
	DecHp = trunc(BossMaxHp * Value/10000),
	NewBossHp = case BossHp > DecHp of true-> BossHp-DecHp;false->1 end,
	NewBoss = OldBoss#bossinfo{bossHp=NewBossHp,hpPlus=erlang:max(0,HpPlus-Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);

delete_centre_buff(?CENTRE_BUFF_FLY_SPEED_PLUS,OldOwnerType,Value)->
	% ?ERR("delete buff ~n"),
	PlayerIDList = get({?bcList,OldOwnerType}),
	ChangeSpeedList = lists:foldl(fun({ServerID,RoleID},Acc)->
		case get_player(ServerID,RoleID) of
			?undefined->
				Acc;
			#conquerisland_player{speedPlus=OldSpeedPlus,endPos=OldEndPos,tarCentre=OldTarCentreID}=Player->
				%%更改player当前位置
				NewPlayer=#conquerisland_player{startPos=StartPos1,type=Type,serverID=ServerID,roleID=RoleID} = reset_player(Player),
				%%首先将player的位置设定成当前位置，此处修改player的tarcentre主要是为了在执行do_player_mov的时候，能重新计算
				NewPlayer1= NewPlayer#conquerisland_player{speedPlus=erlang:max(0,OldSpeedPlus-Value),endPos=OldEndPos},
				set_player(NewPlayer1),
				case StartPos1=/=OldEndPos of
					true->
						%%正在向据点移动的过程中,重新启动新的mov过程
						%%此处修改player的tarcentre主要是为了在执行do_player_mov的时候，能重新计算
						% ?ERR("delete buff"),
						delete_player_from_centre(Type,{ServerID,RoleID},OldTarCentreID),
						do_player_mov(NewPlayer1#conquerisland_player{tarCentre=0},OldTarCentreID),
						[NewPlayer1|Acc];
					false->
						%%没有移动中
						[NewPlayer1|Acc]
				end
		end
	end,[],PlayerIDList),
	broadcast_war_change_to_role(ChangeSpeedList,[],[],get(?bcList));

delete_centre_buff(?CENTRE_BUFF_PLAYER_REBORN,OldOwnerType,_Value)->
	InitRebornPos = data_conquerisland:get({born_pos, OldOwnerType}),
	set_player_reborn_pos(OldOwnerType,InitRebornPos).

add_centre_buff(_BuffType,0,_Value)->
	ignore;
%%减少本方BOSS伤害
add_centre_buff(?CENTRE_BUFF_BOSS_DEMAGE_DEC,OwnerType,Value)->
	BossID = get_player_bossid(OwnerType),
	OldBoss = #bossinfo{demageDec=OldDemageDec} = get_boss(BossID),
	NewBoss = OldBoss#bossinfo{demageDec=erlang:max(0,OldDemageDec+Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);

%%增加对方BOSS伤害
add_centre_buff(?CENTRE_BUFF_BOSS_DEMAGE_PLUS,OwnerType,Value)->
	EnemyType = case OwnerType of ?PLAYER_TYPE_DEFENDER->?PLAYER_TYPE_ATTACKER;?PLAYER_TYPE_ATTACKER->?PLAYER_TYPE_DEFENDER end,
	BossID = get_player_bossid(EnemyType),
	OldBoss = #bossinfo{demagePlus=OldDemagePlus} = get_boss(BossID),
	NewBoss = OldBoss#bossinfo{demagePlus=erlang:max(0,OldDemagePlus+Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);	

add_centre_buff(?CENTRE_BUFF_BOSS_CRIT_PLUS,OwnerType,Value)->
	EnemyType = case OwnerType of ?PLAYER_TYPE_DEFENDER->?PLAYER_TYPE_ATTACKER;?PLAYER_TYPE_ATTACKER->?PLAYER_TYPE_DEFENDER end,
	BossID = get_player_bossid(EnemyType),
	OldBoss = #bossinfo{critPlus=OldCritPlus} = get_boss(BossID),
	NewBoss = OldBoss#bossinfo{critPlus=erlang:max(0,OldCritPlus+Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);

add_centre_buff(?CENTRE_BUFF_BOSS_HP_PLUS,OwnerType,Value)->
	BossID = get_player_bossid(OwnerType),
	OldBoss = #bossinfo{bossHp=BossHp,bossMaxHp=BossMaxHp,hpPlus=HpPlus} = get_boss(BossID),
	AddHp = trunc(BossMaxHp * Value/10000),
	NewBossHp = BossHp+AddHp,
	NewBoss = OldBoss#bossinfo{bossHp=NewBossHp,hpPlus=erlang:max(0,HpPlus+Value)},
	set_boss(NewBoss),
	add_boss_update_list(BossID);

add_centre_buff(?CENTRE_BUFF_FLY_SPEED_PLUS,OwnerType,Value)->
	% ?ERR("add speed buff Value:~w ~n",[Value]),
	PlayerIDList = get({?bcList,OwnerType}),
	ChangeSpeedList = lists:foldl(fun({ServerID,RoleID},Acc)->
		case get_player(ServerID,RoleID) of
			?undefined->
				% ?ERR("ServerID:~w RoleID:~w ~n",[ServerID,RoleID]),
				Acc;
			#conquerisland_player{speedPlus=OldSpeedPlus,endPos=OldEndPos,tarCentre=OldTarCentreID}=Player->
				NewPlayer=#conquerisland_player{startPos=StartPos1,type=Type,serverID=ServerID,roleID=RoleID} = reset_player(Player), 
				%%更改速度加成，以及设置目标位置为旧的目标位置
				NewPlayer1= NewPlayer#conquerisland_player{speedPlus=erlang:max(0,OldSpeedPlus+Value),endPos=OldEndPos},
				% ?ERR("NewPlayer1:~w ~n",[NewPlayer1]),
				set_player(NewPlayer1),
				case StartPos1=/=OldEndPos of
					true->
						%%正在向据点移动的过程中,重新启动新的mov过程
						% ?ERR("buff add ~n"),
						delete_player_from_centre(Type,{ServerID,RoleID},OldTarCentreID),
						do_player_mov(NewPlayer1#conquerisland_player{tarCentre=0},OldTarCentreID),
						[NewPlayer1|Acc];
					false->
						%%没有移动中
						[NewPlayer1|Acc]
				end
		end
	end,[],PlayerIDList),
	broadcast_war_change_to_role(ChangeSpeedList,[],[],get(?bcList));

add_centre_buff(?CENTRE_BUFF_PLAYER_REBORN,OwnerType,_Value)->
	CentreRebornPos = data_conquerisland:get(centre_reborn_pos),
	set_player_reborn_pos(OwnerType,CentreRebornPos).

get_war_change()->
	PlayerIDList = get_player_change(),
	BossIDList = get_boss_change(),
	CentreIDList = get_centre_change(),
	{[get_player(ServerID,RoleID)||{ServerID,RoleID}<-PlayerIDList],[get_centre(CentreID)||CentreID<-CentreIDList],[get_boss(BossID)||BossID<-BossIDList]}.

get_player_change()->
	case get({?war_change,?WAR_PLAYER_CHANGE_TYPE}) of
		X when is_list(X)->
			X;
		_ ->
			[]
	end.
get_boss_change()->
	case get({?war_change,?WAR_BOSS_CHANGE_TYPE}) of
		X when is_list(X)->
			X;
		_->
			[]
	end.
get_centre_change()->
	case get({?war_change,?WAR_CENTRE_CHANGE_TYPE}) of
		X when is_list(X)->
			X;
		_->
			[]
	end.
add_player_update_list(Key)->
	L = get_player_change(),
	LAD = lists:delete(Key,L),
	put({?war_change,?WAR_PLAYER_CHANGE_TYPE},[Key|LAD]).

add_boss_update_list(Key)->
	L = get_boss_change(),
	LAD = lists:delete(Key,L),
	put({?war_change,?WAR_BOSS_CHANGE_TYPE},[Key|LAD]).

add_centre_update_list(Key)->
	L = get_centre_change(),
	LAD = lists:delete(Key,L),
	put({?war_change,?WAR_CENTRE_CHANGE_TYPE},[Key|LAD]).
clear_change(Type)->
	case lists:member(Type,[?WAR_CENTRE_CHANGE_TYPE,?WAR_BOSS_CHANGE_TYPE,?WAR_PLAYER_CHANGE_TYPE]) of
		true->
			put({?war_change,Type},[]);
		false->
			ignore
	end.
clear_all_change()->
	clear_change(?WAR_PLAYER_CHANGE_TYPE),
	clear_change(?WAR_BOSS_CHANGE_TYPE),
	clear_change(?WAR_CENTRE_CHANGE_TYPE).

get_fight_rank()->
	case get(?fight_rank) of
		?undefined->
			[];
		X ->
			X
	end.
set_fight_rank(Rank)->
	put(?fight_rank,Rank).

%%winrank表示战斗结果
add_fight_rank(Player,WinRank)->
	PFightRank = player2pfightrank(Player,WinRank),
	OldRank = get_fight_rank(),
	NewRank = case lists:keytake(PFightRank#p_fight_rank.roleID,#p_fight_rank.roleID,OldRank) of
		false->
			[PFightRank|OldRank];
		{_Value,_Find,Other}->
			[PFightRank|Other]
	end,
	SortRank = lists:sort(fun(A,B)->compare_fight_rank(A,B) end,NewRank),
	set_fight_rank(SortRank),
	PFightRank.

player2pfightrank(#conquerisland_player{serverID=ServerID,roleID=RoleID,roleName=RoleName,roleLevel=RoleLevel,killNum=KillNum,occupyCentreTimes=OccupyFinishTime,bossDemage=BossDemage,bossfighttimes=BossFightTimes,type=Type,movDis=MovDis},WinRank)->
	EnemyType = case Type of ?PLAYER_TYPE_DEFENDER->?PLAYER_TYPE_ATTACKER;_->?PLAYER_TYPE_DEFENDER end,
	BossID = get_player_bossid(EnemyType),
	#bossinfo{bossMaxHp=BossMaxHp} = get_boss(BossID),
	HonorValue =
		case MovDis of
			0->
				0;
			_-> 
				calculate_honor(WinRank,OccupyFinishTime,KillNum,BossDemage,BossFightTimes,BossMaxHp)
		end,
	#p_fight_rank{serverID=ServerID,roleID=RoleID,roleName=RoleName,roleLevel=RoleLevel,killNum=KillNum,centreNum=OccupyFinishTime,bossDemage=BossDemage,honor=HonorValue}.

%%未决定出胜负
calculate_honor(OccupyFinishTime,KillNum,BossDemage,BossFightTimes,BossMaxHp)->
	OccupyHonor = OccupyFinishTime * data_conquerisland:get(occupy_rate),
	KillHonor = KillNum * data_conquerisland:get(kill_rate),
	BossAttackHonor = BossFightTimes * data_conquerisland:get(boss_fight_time_rate),
	BossDemageHonor = erlang:min(BossDemage / (BossMaxHp*data_conquerisland:get(boss_demage_rate)),BossFightTimes*data_conquerisland:get(boss_demage_time_rate)),
	trunc(OccupyHonor+KillHonor+BossAttackHonor+BossDemageHonor).
calculate_honor(Rank,OccupyFinishTime,KillNum,BossDemage,BossFightTimes,BossMaxHp)->
	Value = case data_conquerisland:get({fight_result,Rank}) of
		?undefined->
			calculate_honor(OccupyFinishTime,KillNum,BossDemage,BossFightTimes,BossMaxHp);
		BaseValue->
			BaseValue+calculate_honor(OccupyFinishTime,KillNum,BossDemage,BossFightTimes,BossMaxHp)
	end,
	erlang:min(Value,data_conquerisland:get(fight_max_honor)).


compare_fight_rank(A,B)->
	#p_fight_rank{honor=HonorA,killNum=KillNumA,centreNum=CentreNumA,bossDemage=BossDemageA} = A,
	#p_fight_rank{honor=HonorB,killNum=KillNumB,centreNum=CentreNumB,bossDemage=BossDemageB} = B,
	(HonorA > HonorB) orelse 
	(HonorA =:= HonorB andalso KillNumA > KillNumB) orelse
	(HonorA =:= HonorB andalso KillNumA =:= KillNumB andalso CentreNumA > CentreNumB) orelse
	(HonorA =:= HonorB andalso KillNumA =:= KillNumA andalso CentreNumA =:= CentreNumB andalso BossDemageA > BossDemageB).

add_talk(Type,TalkUnit)->
	OldTalkUnitList = case get({?talk_cache,Type}) of
		?undefined->
			[];
		X ->
			X
	end,
	NewTalkUnitList = lists:sublist([TalkUnit|OldTalkUnitList],data_conquerisland:get(talk_len)),
	put({?talk_cache,Type},NewTalkUnitList).

get_talk_by_type(Type)->
	case get({?talk_cache,Type}) of
		?undefined->
			[];
		X ->
			X
	end.

%%计算player对boss战斗力
init_player_fightpower_for_boss(#base_player{roleLevel=RoleLevel,fighters=Fighters,talent=Talent,itemList=ItemList})->
	GerFightPower = calculate_ger_fightpower_for_boss(Fighters),
	EquipFightPower = calculate_equip_fightpower_for_boss(ItemList),
	RoleFightPower = calculate_role_fightpower_for_boss(RoleLevel),
	TalentFightPower = calculate_talent_fightpower_for_boss(Talent),
	% ?ERR("GerFightPower:~w EquipFightPower:~w RoleFightPower:~w TalentFightPower:~w ~n",[GerFightPower,EquipFightPower,RoleFightPower,TalentFightPower]),
	trunc(GerFightPower+EquipFightPower+RoleFightPower+TalentFightPower).

calculate_ger_fightpower_for_boss(Fighters)->
	calculate_ger_fightpower_for_boss(Fighters,0).
calculate_ger_fightpower_for_boss([],Acc)->
	Acc;
calculate_ger_fightpower_for_boss([H|T],Acc)->
	SingleGerPower = calculate_single_ger_fightpower_for_boss(H),
	calculate_ger_fightpower_for_boss(T,Acc+SingleGerPower).

calculate_single_ger_fightpower_for_boss(#ger{gerBase=GerBase})->
	#gerBase{gerQuality=GerQuality,gerLevel=GerLevel,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerTypeID=GerTypeID}=GerBase,
	GerAwakePower = calculate_single_ger_awake_fightpower_for_boss(GerAwakeInfo),
	GerCrystalPower = calculate_single_ger_crystal_fightpower_for_boss(GerCrystalInfo),
	GerLevelPower = calculate_single_ger_level_fightpower_for_boss(GerLevel),
	GerQualityPower = calculate_single_ger_quality_fightpower_for_boss(GerQuality),
	#data_ger{gerStar=Star} = data_ger:get(GerTypeID),
	data_conquerisland:get({data_ger_star,Star})*(GerLevelPower+GerQualityPower+GerAwakePower)+GerCrystalPower.

calculate_single_ger_awake_fightpower_for_boss(GerAwakeInfo)->
	calculate_single_ger_awake_fightpower_for_boss(GerAwakeInfo,0).
calculate_single_ger_awake_fightpower_for_boss([],Acc)->
	Acc*data_conquerisland:get(data_awake_rate);
calculate_single_ger_awake_fightpower_for_boss([H|T],Acc)->
	#awake{skill_quality=SkillQuality} = H,
	calculate_single_ger_awake_fightpower_for_boss(T,Acc+SkillQuality).

calculate_single_ger_crystal_fightpower_for_boss(GerCrystalInfo)->
	calculate_single_ger_crystal_fightpower_for_boss(GerCrystalInfo,0).
calculate_single_ger_crystal_fightpower_for_boss(-1,Acc)->
	Acc;
calculate_single_ger_crystal_fightpower_for_boss([],Acc)->
	Acc;
calculate_single_ger_crystal_fightpower_for_boss([H|T],Acc)->
	#crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel}=H,
	CrystalLevelAcc = role_crystal:calculate_crystal_level_acc(CrystalType,CrystalQuality,CrystalLevel),
	% ?ERR("CrystalType:~w CrystalQuality:~w CrystalLevel:~w CrystalLevelAcc:~w ~n",[CrystalType,CrystalQuality,CrystalLevel,CrystalLevelAcc]),
	FightPower = CrystalQuality*data_conquerisland:get(crystal_quality_rate) + CrystalLevelAcc * data_conquerisland:get({crystal_level_base,CrystalType}),
	calculate_single_ger_crystal_fightpower_for_boss(T,FightPower+Acc).

calculate_single_ger_level_fightpower_for_boss(GerLevel)->
	GerLevel*data_conquerisland:get(data_level_rate).

calculate_single_ger_quality_fightpower_for_boss(GerQuality)->
	GerQuality*data_conquerisland:get(data_rank_rate).

calculate_equip_fightpower_for_boss(ItemList)->
	calculate_equip_fightpower_for_boss(ItemList,0).
calculate_equip_fightpower_for_boss([],Acc)->
	Acc;
calculate_equip_fightpower_for_boss([H|T],Acc)->
	SingleEquipPower = calculate_single_equip_fightpower_for_boss(H),
	calculate_equip_fightpower_for_boss(T,Acc+SingleEquipPower).

calculate_single_equip_fightpower_for_boss([_ItemUID,ItemTypeID,_ItemPos,_ItemLevel,_ItemRank,_ItemGerID,_Decay,_ItemExp,_ItemEnchantType,_ItemEnchantLevel,_ItemLegendRank]=Item)->
	#data_item{itemType=ItemType} = data_item:get(ItemTypeID),
	case item_lib:is_itemType_stone(ItemType) orelse item_lib:is_itemType_normal_legend(ItemType) of
		true->
			calculate_normal_stone_fightpower_for_boss(Item);
		false->
			case item_lib:is_itemType_essence_stone(ItemType) orelse item_lib:is_itemType_essence_legend(ItemType) of
				true->
					calculate_essence_stone_fightpower_for_boss(Item);
				false->
					case item_lib:is_itemType_trainer_stone(ItemType) orelse item_lib:is_itemType_trainer_equip(ItemType) of
						true->
							calculate_trainer_equip_fightpower_for_boss(Item);
						false->
							case item_lib:is_itemType_equip(ItemType) orelse item_lib:is_accelerate_equip(ItemType) of
								true->
									calculate_normal_ger_equip_fightpower_for_boss(Item);
								false->
									?INFO("undefined Type:~w ~n",[ItemType]),
									0
							end
					end
			end
	end.

calculate_normal_stone_fightpower_for_boss([_ItemUID,ItemTypeID,_ItemPos,ItemLevel,ItemRank,_ItemGerID,_Decay,_ItemExp,_ItemEnchantType,_ItemEnchantLevel,_ItemLegendRank])->
	#data_item{itemStar=Star,itemType=ItemType} = data_item:get(ItemTypeID),
	LegendRate = case item_lib:is_itemType_normal_legend(ItemType) of
		false->
			data_conquerisland:get(ger_normal_equip_rate);
		true->
			data_conquerisland:get(ger_legend_equip_rate)
	end,
	data_conquerisland:get({normal_stone_star,Star})*(data_conquerisland:get(normal_base_value)
		+ItemLevel*data_conquerisland:get(normal_stone_level_rate)+ItemRank*data_conquerisland:get(normal_stone_rank_rate))*LegendRate.

calculate_essence_stone_fightpower_for_boss([_ItemUID,ItemTypeID,_ItemPos,ItemLevel,ItemRank,_ItemGerID,_Decay,_ItemExp,_ItemEnchantType,_ItemEnchantLevel,_ItemLegendRank])->
	#data_item{itemStar=Star,itemType=ItemType} = data_item:get(ItemTypeID),
	LegendRate = case item_lib:is_itemType_essence_legend(ItemType) of
		false->
			data_conquerisland:get(ger_normal_equip_rate);
		true->
			data_conquerisland:get(ger_legend_equip_rate)
	end,
	data_conquerisland:get({essence_stone_star,Star})*(data_conquerisland:get(essence_base_value)
		+ItemLevel*data_conquerisland:get(essence_stone_level_rate)
		+ItemRank*data_conquerisland:get(essence_stone_rank_rate))*LegendRate.

calculate_trainer_equip_fightpower_for_boss([_ItemUID,ItemTypeID,_ItemPos,ItemLevel,ItemRank,_ItemGerID,_Decay,_ItemExp,_ItemEnchantType,_ItemEnchantLevel,_ItemLegendRank])->
	#data_item{itemStar=Star} = data_item:get(ItemTypeID),
	data_conquerisland:get({trainer_equip_star,Star})*(ItemLevel*data_conquerisland:get(trainer_equip_level_rate)+ItemRank*data_conquerisland:get(trainer_equip_rank_rate)).

calculate_normal_ger_equip_fightpower_for_boss([_ItemUID,ItemTypeID,_ItemPos,ItemLevel,ItemRank,_ItemGerID,_Decay,_ItemExp,_ItemEnchantType,ItemEnchantLevel,_ItemLegendRank])->
	LegendRate = case item_lib:is_legend_equip(ItemTypeID) of
		false->
			data_conquerisland:get(ger_normal_equip_rate);
		true->
			data_conquerisland:get(ger_legend_equip_rate)
	end,
	#data_item{itemStar=Star}=data_item:get(ItemTypeID),
	NormalRank = case ItemRank >10 of true->10;_->ItemRank end,
	data_conquerisland:get({ger_equip_star,Star})*(ItemLevel*data_conquerisland:get(ger_equip_level_rate)
		+NormalRank*data_conquerisland:get(ger_equip_rank_rate)
		+(erlang:max(0,ItemRank-10))*data_conquerisland:get(ger_equip_rank_rate_perf)
		+ItemEnchantLevel*data_conquerisland:get(ger_equip_enchant_rate))*LegendRate.

calculate_role_fightpower_for_boss(RoleLevel)->
	RoleLevel* data_conquerisland:get(role_level_rate).

calculate_talent_fightpower_for_boss(Talent)->
	calculate_talent_fightpower_for_boss(Talent,0).
calculate_talent_fightpower_for_boss([],Acc)->
	Acc;
calculate_talent_fightpower_for_boss([H|T],Acc)->
	SingleTalent = calculate_single_talent_fightpower_for_boss(H),
	calculate_talent_fightpower_for_boss(T,Acc+SingleTalent).
calculate_single_talent_fightpower_for_boss({Type,Level})->
	case  data_talent:get(Type+Level) of
		?undefined->
			0;
		#data_talent{max_level=MaxLevel}->
			data_conquerisland:get(talent_rate)*Level/MaxLevel
	end;
calculate_single_talent_fightpower_for_boss(_H)->
	0.

add_sys_msg(Type,RoleName, CentreID) ->
	ID = data_conquerisland:get(Type),
	TalkUnit = #p_carlos_talk{roleID=0,roleName=RoleName,data=ID,ext=CentreID},
	add_talk(?PLAYER_TYPE_ATTACKER, TalkUnit),
	add_talk(?PLAYER_TYPE_DEFENDER,TalkUnit),
	NeedBroadcastRoleList = get(?bcList),
	Msg = #sc_conquerisland_talk{data=TalkUnit},
	broadcast_msg(Msg,NeedBroadcastRoleList).

set_pd_operate_ts(RoleID)->
    put({?pd_operate_ts,RoleID},util:now()).

check_player_centre_exist(ServerID,RoleID,PlayerType,DesCentreID)->
	CentreIDList = data_conquerisland:get(centre_list),
	[check_and_delete_unexpect_player_exist(ServerID,RoleID,PlayerType,CentreID,DesCentreID)||CentreID<-CentreIDList,CentreID=/=DesCentreID].
check_and_delete_unexpect_player_exist(ServerID,RoleID,PlayerType,CentreID,DesCentreID) when is_number(CentreID)->
	Centre = get_centre(CentreID),
	check_and_delete_unexpect_player_exist(ServerID,RoleID,PlayerType,Centre,DesCentreID);
check_and_delete_unexpect_player_exist(ServerID,RoleID,?PLAYER_TYPE_DEFENDER,Centre=#centre{defenderList=DefenderList,id=CentreID},DesCentreID)->
	case lists:member({ServerID,RoleID},DefenderList) of
		true->
			#conquerisland_player{startPos=StartPos,endPos=EndPos,tarCentre=TarCentreID} = get_player(ServerID,RoleID),
			?ERR("arrive centre:~w  player exist other centre：~w StartPos:~w EndPos:~w TarCentreID:~w ~n",[DesCentreID,CentreID,StartPos,EndPos,TarCentreID]),
			NewList = [P||P<-DefenderList,P=/={ServerID,RoleID}],
			Num = length(NewList),
			NewCentre = Centre#centre{defenderList=NewList,defenderNum=Num},
			set_centre(NewCentre);
		false->
			ignore
	end;
check_and_delete_unexpect_player_exist(ServerID,RoleID,?PLAYER_TYPE_ATTACKER,Centre=#centre{attackerList=AttackerList,id=CentreID},DesCentreID)->
	case lists:member({ServerID,RoleID},AttackerList) of
		true->
			#conquerisland_player{startPos=StartPos,endPos=EndPos,tarCentre=TarCentreID} = get_player(ServerID,RoleID),
			?ERR("arrive centre:~w  player exist other centre：~w StartPos:~w EndPos:~w TarCentreID:~w ~n",[DesCentreID,CentreID,StartPos,EndPos,TarCentreID]),
			NewList = [P||P<-AttackerList,P=/={ServerID,RoleID}],
			Num = length(NewList),
			NewCentre = Centre#centre{attackerList=NewList,attackerNum=Num},
			set_centre(NewCentre);
		false->
			ignore
	end.

