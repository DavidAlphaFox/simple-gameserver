%% @author crimoon-17
%% @doc @todo Add description to war_server.
%% 按照出生点,出生在下方的是attacker:0,出生在三方的是defender:1  
%% 


-module(war_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_carlos.hrl").
-include("def_mail.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).

-record(state, {warID=0,warStartTime=0}).

-record(plan, {sec=0,key=0,value=0}).

-define(player, player).
-define(mine, mine).
-define(plan,plan).
-define(bcList,bcList).
-define(endInfo, endInfo).
-define(bc_mark,bc_mark).
-define(mark_bc,mark_bc).
-define(talk_data,talk_data).
-define(rank_data, rank_data).
-define(full_speed, full_speed).
-define(pd_operate_ts, pd_operate_ts).

-define(Dump_interval, 60000).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).
start_link(WarID,Attackers,Defenders) ->
	gen_server:start_link(?MODULE,[WarID,Attackers, Defenders],[]).
start(Args) ->
		supervisor:start_child(war_sup,Args).

init([WarID]) ->
	Data = db_sql:get_carlos_war_info(WarID),
	add_ets_war_info(WarID,self()),
	State2 = init_state(Data),
	init_player_mov(),
	init_mine_get(),
	check_winner_plan(),
    Players = lists:foldl(fun({{?player,{_,_}},Player},AccList)-> [Player|AccList];
                             (_,AccList)-> AccList 
                          end, [], get()),
    carlos_ai:init(Players),
    {ok, State2};
init([WarID,Attacker,Defender]) ->
	add_ets_war_info(WarID,self()),
	init_players(attacker, Attacker),
	init_players(defender, Defender),
	init_bcList(Attacker++Defender),
	init_ground(),
	tick(),
	tick_plan(),
	bc_open(),
	set_end_info(WarID),
	check_winner_plan(),
    carlos_ai:init(Attacker++Defender),
	{ok,#state{warID=WarID,warStartTime=util:now()}}.

init_bcList(Players) ->
	RL=[{RoleID,ServerID}||#player{roleID=RoleID,serverID=ServerID}<-Players],
	put(?bcList, RL),
	RLA = [{RoleID,ServerID}||#player{roleID=RoleID,serverID=ServerID,type=Type}<-Players,Type==attacker],
	put({?bcList,attacker},RLA),
	RLD = [{RoleID,ServerID}||#player{roleID=RoleID,serverID=ServerID,type=Type}<-Players,Type==defender],
	put({?bcList,defender},RLD).

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
	Interval = data_carlos:get(war_interval),
	put(?endInfo,{WarID,util:now()+Interval}),
	erlang:send_after(Interval * 1000, self(), do_end_war),
	ok.

tick_plan()->
	erlang:send_after(1000, self(), tick_plan).

bc_open()->
	erlang:send(self(), {bc_info, open}).

add_ets_war_info(WarID,PID) -> 
	ets:insert(?ETS_CARLOS_INFO,#ets_carlos_info{warID=WarID,pid=PID}).

init_state({#state{warStartTime=WST}=State,Dict}) ->
    lists:foreach(fun
            ({{?player,{_RoleID,_ServerID}} = K,V}) when erlang:size(V) =:= 35-> %旧的数据格式 
                ?INFO("war_server old value~w",[V]),
                NewV = erlang:tuple_to_list(V),
                NewV2 = [0|lists:reverse(NewV)],
                NewV3 = erlang:list_to_tuple(lists:reverse(NewV2)),
                put(K,  NewV3);
            ({K,V}) -> 
                put(K,V)
        end, Dict),
	Now=util:now(),
	Interval = data_carlos:get(war_interval),
	End = WST + Interval,
	if End >= Now ->
		   erlang:send(self(), do_end_war);
	   true ->
		   erlang:send_after((End - Now) * 1000, self(), do_end_war)
	end,
	State.

init_players(Type,Players) ->
	Pos = data_carlos:get({born_pos, Type}),
	%[put({?player, {RoleID,ServerID}}, Info#player{startPos=Pos,endPos=Pos})
	[set_player(RoleID,ServerID,Info#player{startPos=Pos,endPos=Pos})
	||#player{roleID=RoleID,serverID=ServerID}=Info<-Players].

init_ground() ->
	MineList = data_carlos:get(mine_list),
	[put({?mine, ID},#mine{id=ID,pos=Pos,gas=Gas,totalGas=Gas})||{ID,Pos,Gas}<-MineList].
	
handle_call(test_show_battle_player, _From, State) ->
    Reply = carlos_ai:show_battle_info(),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(dump_interval_tick, State) ->
	?CATCH(do_interval_dump(State)),
	tick(),
	{noreply, State, hibernate};
handle_info(stop,State) ->
	{stop,normal, State};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
	{noreply, State}.

terminate(Reason, State) ->
	[reset_mine(MineID)||{MineID,_,_}<-data_carlos:get(mine_list)],
	?CATCH(do_interval_dump(State)),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p"
		  ,[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_interval_dump(#state{warID=WarID}=State) ->
	db_sql:set_carlos_data(WarID, {State,get()}).

tick() ->
	erlang:send_after(?Dump_interval, self(), dump_interval_tick).


do_handle_info({bc_info,Type}) ->
	bc_info(Type),
	ok;
do_handle_info(tick_plan) ->
	tick_plan(),
	plan(),
    carlos_ai:check_robot_act(),
	ok;
do_handle_info(do_end_war) ->
	end_war(),
	ok;
do_handle_info({cs_carlos_war_base_info, {_,RoleID,ServerID}}) ->
	Msg = get_war_base_info(),
	carlos_router:send_client(ServerID,RoleID,Msg),
	ok;
do_handle_info({{cs_carlos_mine_detail, MineID}, {_,RoleID,ServerID}}) ->
	Mine = mine2carlosMineDtl(get_mine(MineID)),
	carlos_router:send_client(ServerID,RoleID, Mine),
	ok;
do_handle_info({cs_carlos_self, {_,RoleID,ServerID}}) ->
	Player = get_player(RoleID,ServerID),
	Msg = #sc_carlos_self{self=player2carlosPlayer(Player)},
	carlos_router:send_client(ServerID,RoleID, Msg),
	ok;
do_handle_info({{cs_carlos_mov, MineID, IsAi}, {_,RoleID,ServerID}}) ->
    if
        IsAi =:= false ->
            set_pd_operate_ts(RoleID);
        true ->
            ignore
    end,
	#player{tarMineID=TarMineID,rebornSec=RebornSec,type=Type} = Player = get_player(RoleID,ServerID),
	Msg = 
		case TarMineID of
			MineID ->
				#sc_carlos_mov{result=2};
			_ ->
				Now = util:now(),
				if Now > RebornSec ->
					   #mine{pos=Pos}= _MineInfo = get_mine(MineID),
					   if TarMineID == 0 ->
							  mark_bc([], {RoleID,ServerID});
						  true ->
							  MineInfo2 = delete_mine_player(Type,{RoleID,ServerID},get_mine(TarMineID)),
							  set_mine(TarMineID,MineInfo2),
							  mark_bc(TarMineID, {RoleID,ServerID})
					   end,
					   Player2 = reset_player(Player),
					   Player3 = update_player(Player2,Pos,MineID),
					   set_player(RoleID,ServerID,Player3),
					   add_mov_plan(RoleID,Player3),
					   #sc_carlos_mov{result=1};
				   true ->
					   #sc_carlos_mov{result=2}
				end
		end,
	carlos_router:send_client(ServerID,RoleID,Msg),
	ok;
do_handle_info({mine_gas,MineID,RoleID,ServerID}) ->
	#mine{owner=Owner,lastOwner=LastOwner} = MineInfo = get_mine(MineID),
	case Owner of
		LastOwner ->
			?ERR("something wrong, gas plan works, bug mine start get gas"),
			ignore;
		_ ->
			MineInfo2 = update_get_gas(Owner, MineInfo),
			send_mine_reward(ServerID,RoleID),
			set_mine(MineID, MineInfo2#mine{lastOwner=Owner}),
			update_gas_speed(Owner),
			case LastOwner of
				0 ->
					#player{roleName=RoleName} = get_player(RoleID,ServerID),
					add_sys_msg(zhanling, RoleName,MineID);
				_ ->
					#player{roleName=RoleName} = get_player(RoleID,ServerID),
					add_sys_msg(tuxi,RoleName,MineID)
			end
	end,																			  
	ok;
%% 点击占领后,更新owner为自己,但是lastOwner不变,直到倒计时结束,真正可以占领,lastOwner才修改为自己
%% 这样,当矿被别人占领后,如果自己立即杀掉对方,矿仍然是自己的.具体资源计算...
do_handle_info({{cs_carlos_ownMine, MineID}, {_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	{A,B} = cs_carlos_ownMine(MineID,ServerID,RoleID),
	carlos_router:send_client(ServerID,RoleID,#sc_carlos_ownMine{result=A,sec=B}),
	ok;	
do_handle_info({player_arrive, RoleID,ServerID,MineID}) ->
	#player{tarMineID=TarMineID,type=Type} = Player = get_player(RoleID,ServerID),
	case TarMineID of
		MineID ->
			mark_bc(MineID,{RoleID,ServerID}),
			MineInfo = get_mine(MineID),
			Player2 = reset_player3(Player),
			set_player(RoleID,ServerID,Player2),
			NewMine = add_mine_player(MineInfo,RoleID,ServerID,Type),
			set_mine(MineID,NewMine);
		_ ->
			ignore
	end;
do_handle_info({{cs_carlos_attack, TarRoleID,TarServerID,MineID,IsAi}, {_,RoleID,ServerID}}) ->
    if
        IsAi =:= false -> set_pd_operate_ts(RoleID);
        true -> ignore
    end,
	#player{tarMineID=TarMineID,type=Type,fighters=Fighters1,addBuff=Buff1,talent=Talent1
		   ,itemList=ItemList1,replayList=RL1,roleName=RoleName1,trSpecial=TrSpecial1,skin_info=SkinInfo1} = Player = get_player(RoleID,ServerID),
	#player{tarMineID=TarMineID2,type=Type2,fighters=Fighters2,addBuff=Buff2,talent=Talent2
		   ,itemList=ItemList2,replayList=RL2,roleName=RoleName2,trSpecial=TrSpecial2,skin_info=SkinInfo2} = Player2 = get_player(TarRoleID,TarServerID),
	{_,Result,FightInfo} = 
	if TarMineID == MineID andalso TarMineID2 == MineID ->
		   if Type /= Type2 ->
				  MineInfo = get_mine(MineID),
				  case check_mine_player(Type,{RoleID,ServerID},MineInfo) andalso check_mine_player(Type2,{TarRoleID,TarServerID},MineInfo) of
					  true ->
					  	  GerEquipList1 = role_item:assort_ger_equiplist(ItemList1),
    					  LegendAddList1 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList1],
    					  GerEquipList2 = role_item:assort_ger_equiplist(ItemList2),
    					  LegendAddList2 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList2],
						  case ?CATCH(role_fight:new(filter_out_zero_hp(Fighters1),filter_out_zero_hp(Fighters2)
													,Buff1,Buff2,Talent1,Talent2,TrSpecial1,TrSpecial2,SkinInfo1,SkinInfo2,LegendAddList1,LegendAddList2)) of
							  {IsWin, FightRecord0, {_,_,NewFighters1,NewFighters2}} ->
								  FighterList2 = role_data:get_FighterList_with_effect(ItemList1,ItemList2
																					  ,FightRecord0#sc_fight_request.fighterList),
								  FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
								  ReplayUID = tk_id:gen_replayUID(),
								  carlos_replay_server:record_replay(ReplayUID,FightRecord),
								  ReplayDtl = #p_carlos_replay_dtl{replayUID=ReplayUID,isRole1Win=bool2int(IsWin),role1Name=RoleName1,role2Name=RoleName2},
								  NewRL1 = add_replay(RL1,ReplayDtl),
								  NewRL2 = add_replay(RL2,ReplayDtl),
								  NewFightersA1 = recalc_fighter_list(Fighters1, NewFighters1),
								  NewFightersA2 = recalc_fighter_list(Fighters2, NewFighters2),
%% 								  {SumHp1,SumHpMax1}=get_hp_dtl(NewFightersA1),
%% 								  {SumHp2,SumHpMax2}=get_hp_dtl(NewFightersA2),
								  Blood1 = calc_hp_percent(NewFightersA1),
								  Blood2 = calc_hp_percent(NewFightersA2),
								  if Blood1 == 0 andalso Blood2 == 0 ->
										 NewPlayer = update_player_fighters(Player,Blood1,NewRL1,NewFightersA1),
										 NewPlayer2 = update_player_fighters(Player2,Blood2,NewRL2,NewFightersA2),
										 NewPlayer_1 = update_player_killNum(NewPlayer),
										 NewPlayer2_1 = update_player_killNum(NewPlayer2),
										 NewPlayer_2 = reborn_player(NewPlayer_1),
										 NewPlayer2_2 = reborn_player(NewPlayer2_1),
										 set_player(RoleID,ServerID,NewPlayer_2),
										 set_player(TarRoleID,TarServerID,NewPlayer2_2),
										 MineInfo2 = delete_mine_player(Type2,{TarRoleID,TarServerID},MineInfo),
										 MineInfo3 = delete_mine_player(Type,{RoleID,ServerID},MineInfo2),
										 set_mine(MineID,MineInfo3),
										 sync_role({RoleID,ServerID}, {TarRoleID,TarServerID}),
										 mark_bc(MineID, {TarRoleID,TarServerID}),
										 mark_bc(MineID,{RoleID,ServerID}),
										 {true,2,[FightRecord]};
									 true ->
%% 										 Blood2 = calc_hp_percent(SumHp1,SumHpMax1),
%% 										 Blood1 = calc_hp_percent(SumHp2,SumHpMax2),
										 if IsWin ->
												NewPlayer = update_player_fighters(Player,Blood1,NewRL1,NewFightersA1),
												NewPlayer2 = update_player_fighters(Player2,Blood2,NewRL2,NewFightersA2),
												NewPlayer_1 = update_player_killNum(NewPlayer),
												NewPlayer2_1 = reborn_player(NewPlayer2),
												set_player(RoleID,ServerID,NewPlayer_1),
												set_player(TarRoleID,TarServerID,NewPlayer2_1),
												MineInfo2 = delete_mine_player(Type2,{TarRoleID,TarServerID},MineInfo),
%												MineInfo3 = mine_owner_change(Type2,MineInfo2),
												set_mine(MineID,MineInfo2),
												sync_role({RoleID,ServerID}, {TarRoleID,TarServerID}),
												mark_bc(MineID, {TarRoleID,TarServerID}),
												{true, 1,[FightRecord]};
											true ->
												NewPlayer = update_player_fighters(Player,Blood1,NewRL1,NewFightersA1),
												NewPlayer2 = update_player_fighters(Player2,Blood2,NewRL2,NewFightersA2),
												NewPlayer_1 = reborn_player(NewPlayer),
												NewPlayer2_1 = update_player_killNum(NewPlayer2),
												set_player(RoleID,ServerID,NewPlayer_1),
												set_player(TarRoleID,TarServerID,NewPlayer2_1),
												MineInfo2 = delete_mine_player(Type,{RoleID,ServerID},MineInfo),
												set_mine(MineID, MineInfo2),
												sync_role({RoleID,ServerID}, {TarRoleID,TarServerID}),
												mark_bc(MineID, {RoleID,ServerID}),
												{true,2,[FightRecord]}
										 end
								  end;
							   {'EXIT', _} ->
								   {false,5,[]}
						  end;
					  false ->
						  {false, 3,[]}
				  end;
			  true ->
				  {false, 4,[]}
		   end;			  
	   true ->
		   {false, 3,[]}
	end,
	carlos_router:send_client(ServerID,RoleID,#sc_carlos_attack{result=Result,fightInfo=FightInfo}),
	ok;
do_handle_info({{cs_carlos_role_dtl, TarRoleID,TarServerID},{_,RoleID,ServerID}})->
	#player{fighters=F,replayList=RepList,grade=Grade} = get_player(TarRoleID,TarServerID),
	FL = fairy2carlosFairy(F),
	carlos_router:send_client(ServerID,RoleID,#sc_carlos_role_dtl{target=FL,replayDtl=RepList,grade=Grade}),
	ok;
do_handle_info({cs_carlos_mov_stop,{_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	Player = get_player(RoleID,ServerID),
	#player{startPos=StartPos,tarMineID=TarMineID}=Player2 = reset_player(Player),
	case TarMineID of
		0 ->
			ignore;
		_ ->
			#mine{pos=Pos} = get_mine(TarMineID),
			case StartPos of
				Pos ->
					set_player(RoleID,ServerID,Player2);
				_ ->
					set_player(RoleID,ServerID,Player2#player{tarMineID=0})
			end,
			cancel_mov_plan(RoleID,ServerID),
			mark_bc([],{RoleID,ServerID})
	end,
	ok;
do_handle_info({{cs_carlos_talk,Data}, {_,RoleID,ServerID}}) ->
	#player{type=Type} = get_player(RoleID, ServerID),
	add_talk_data(Type, Data),
	do_bc(talk,Type,#sc_carlos_talk{data=Data},carlos_bc_info),
	ok;
do_handle_info({cs_carlos_get_talk, {_,RoleID,ServerID}}) ->
	#player{type=Type} = get_player(RoleID,ServerID),
	TalkData = get_talk_data(Type),
	carlos_router:send_client(ServerID,RoleID,#sc_carlos_get_talk{data=TalkData}),
	ok;
do_handle_info({cs_carlos_get_rank, {_,RoleID,ServerID}}) ->
%	#player{type=Type} = get_player(RoleID,ServerID),
	RankList = get_rank_data(),
	carlos_router:send_client(ServerID,RoleID, #sc_carlos_get_rank{rank=RankList}),
	ok;
do_handle_info(check_winner) ->
	MineList = 
		[begin
			 MineInfo2 = update_mine_gas(get_mine(MineID)),
			 set_mine(MineID,MineInfo2)
		 end||{MineID,_,_}<-data_carlos:get(mine_list)],
	{Winner,AttackerGas,DefenderGas} = get_war_winner2(MineList),
	case Winner of
		ignore ->
			check_winner_plan();
		_ ->
			end_war(Winner,AttackerGas,DefenderGas)
	end,
	ok;
do_handle_info({cs_carlos_reborn,{_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	Player = reborn_player_now(get_player(RoleID,ServerID)),
	set_player(RoleID,ServerID, Player),
	mark_bc([],{RoleID,ServerID}),
	ok;

do_handle_info(Info) ->
	?ERR("error get info:~w",[Info]).

bool2int(true ) ->
	1 ;
bool2int(_) ->
	0.

%% add_sys_msg(tuxi,RoleName,MineID) ->
%% 	I = data_carlos:get(tuxi),
%% 	Msg0 = io_lib:format(I,[RoleName]),
%% 	Msg = #p_carlos_talk{roleID=0,roleName="",data=Msg0,ext=MineID},
%% 	add_sys_msg(Msg,Msg);
%% add_sys_msg(fangshou, RoleName,MineID) ->
%% 	I = data_carlos:get(fangshou),
%% 	Msg0 = io_lib:format(I,[RoleName]),
%% 	Msg = #p_carlos_talk{roleID=0,roleName="",data=Msg0,ext=MineID},
%% 	add_sys_msg(Msg,Msg);
%% add_sys_msg(zhanling, Type,MineID) ->
%% 	I1 = data_carlos:get(zhanling1),
%% 	I2 = data_carlos:get(zhanling2),
%% 	Msg1 = #p_carlos_talk{roleID=0,roleName="",data=I1,ext=MineID},
%% 	Msg2 = #p_carlos_talk{roleID=0,roleName="",data=I2,ext=MineID},
%% 	case Type of
%% 		attacker ->
%% 			add_sys_msg(Msg1,Msg2);
%% 		_ ->
%% 			add_sys_msg(Msg2,Msg1)
%% 	end.
add_sys_msg(Type,RoleName, MineID) ->
	ID = data_carlos:get(Type),
	Msg = #p_carlos_talk{roleID=0,roleName=RoleName,data=ID,ext=MineID},
	add_talk_data(attacker, Msg),
	add_talk_data(defender,Msg),
	do_bc(#sc_carlos_talk{data=Msg},carlos_bc_info),
	ok.
%% add_sys_msg(Msg1,Msg2) ->
%% 	add_talk_data(attacker,Msg1),
%% 	add_talk_data(defender,Msg2).

get_talk_data(Type) ->
	case get({?talk_data, Type}) of
		?undefined ->
			[];
		X ->
			X
	end.
set_talk_data(Type,Data) ->
	put({?talk_data, Type}, Data).
add_talk_data(Type,Data) ->
	Len = data_carlos:get(talk_len),
	OL = get_talk_data(Type),
	L2 = lists:sublist([Data|OL], Len),
	set_talk_data(Type,L2).

get_rank_data() ->
	case get(?rank_data) of
		?undefined ->
			[];
		X ->
			X
	end.
set_rank_data(Data) ->
	put(?rank_data, Data).
update_player_rank_data(#player{roleID=RoleID,serverID=ServerID}=Player)->
	Data = get_rank_data(), 
	Data2 = [E||#p_carlos_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
	Data3 = [player2p_carlos_rank_dtl(Player)|Data2],
	set_rank_data(Data3).
update_player_rank_data2(#player{roleID=RoleID,serverID=ServerID}=Player)->
	Data = get_rank_data(), 
	Data2 = [E||#p_carlos_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
	Data3 = [player2p_carlos_rank_dtl2(Player)|Data2],
	set_rank_data(Data3).


stop() ->
	{WarID,_} = get(?endInfo),
	do_bc(WarID,update_carlos_war),
	war_manager_server:war_stoped(WarID),
	erlang:send(self(), stop).

end_war()->
	{Winner,AttackerGas,DefenderGas} = get_war_winner(),
	end_war(Winner,AttackerGas,DefenderGas).
end_war(Winner,AttackerGas,DefenderGas) ->
	send_winner_reward(Winner,AttackerGas,DefenderGas),
%	send_killer_reward(AttackerGas,DefenderGas),
	bc_info(end_war,2,Winner),
	stop().

get_war_winner()->
	MineList = data_carlos:get(mine_list),
	Mines = [get_mine(ID)||{ID,_,_}<-MineList],
	{AttackerGas,DefenderGas} = 
		lists:foldl(fun(#mine{attackerGas=AttackerGas,defenderGas=DefenderGas},{AGAcc,DGAcc}) ->
							{AGAcc+AttackerGas#getGas.gas,DGAcc+DefenderGas#getGas.gas}	
					end,{0,0},Mines),
	if AttackerGas > DefenderGas ->
		   {attacker, AttackerGas,DefenderGas};
	   AttackerGas == DefenderGas ->
		   {equal,AttackerGas,DefenderGas};
	   true ->
		   {defender,AttackerGas,DefenderGas}
	end.

get_war_winner2(Mines) ->
	{AttackerGas,DefenderGas} = 
		lists:foldl(fun(#mine{attackerGas=AttackerGas,defenderGas=DefenderGas},{AGAcc,DGAcc}) ->
							{AGAcc+AttackerGas#getGas.gas,DGAcc+DefenderGas#getGas.gas}	
					end,{0,0},Mines),
	WinGas = data_carlos:get(winner_gas),
	if AttackerGas > DefenderGas ->
		   if AttackerGas >= WinGas ->
				  {attacker,AttackerGas,DefenderGas};
			  true ->
				  {ignore,0,0}
		   end;
	   AttackerGas == DefenderGas ->
		   if AttackerGas >= WinGas ->
				  {equal,AttackerGas,DefenderGas};
			  true ->
				  {ignore,0,0}
		   end;
	   AttackerGas < DefenderGas ->
		   if DefenderGas >= WinGas ->
				  {defender,AttackerGas,DefenderGas};
			  true ->
				  {ignore,0,0}
		   end
	end.

%% send_killer_reward(AttackerGas,DefenderGas) ->
%% 	lists:foreach(fun({{?player,{RoleID,ServerID}},#player{gas=Gas,type=Type,killNum=Kill}=_Player}) ->
%% 						  TotalGas = case Type of attacker -> AttackerGas;defender -> DefenderGas end,
%% 						  AR = get_reward(TotalGas,Gas,Kill),
%% 							  send_msg:direct(ServerID, carlos_server, {mail_reward, RoleID, ?MAIL_TEMPLATE_CARLOS_KILL, AR});
%% 					 (_) -> ignore
%% 				  end, get()).

get_reward(Gas,Mine,Kill) ->
	MC = data_carlos:get(mine_score),
	KC = data_carlos:get(kill_score),
	S = Gas div 10 + Mine * MC + Kill * KC,
	{Min,Max} = data_carlos:get(score_limit),
	S2 = if S < Min ->
				Min;
			true ->
				if S > Max ->
					   Max;
				   true ->
					   S
				end
		 end,
%	#sell_reward{item = [#new_item{itemTypeID=20040,itemNum=S2}]}.
	#new_item{itemTypeID=20040,itemNum=S2,itemLevel=1,itemRank=0}.

merge_reward(#sell_reward{item=[#new_item{itemNum=K}|T]},New = #new_item{itemNum=S}) ->
	#sell_reward{item=[New#new_item{itemNum=S+K}|T]}.

send_winner_reward(equal,AttackerGas,_) ->
	MailEqualReward = data_carlos:get(equal),
    AfkLimit = util:now() - data_carlos:get(afk_check_time),
	P_PlayerInfo_list = 
		lists:foldl(
		  fun(Elem,Acc) ->
				  case Elem of
					  {{?player,{RoleID,ServerID}},#player{gas=Gas,killNum=Kill,movDis=_MovDis}=Player}->
                          AfkTime = get({?pd_operate_ts,RoleID}),
						  if 
                              AfkTime =:= ?undefined orelse AfkTime < AfkLimit  ->
								 Player2 = Player#player{score=0},
								 set_player2(RoleID,ServerID,Player2),
								 send_msg:direct(ServerID, carlos_server,{mail_reward,RoleID,?MAIL_TEMPLATE_CARLOS_BAN});
							 true ->
								 R = get_reward(AttackerGas,Gas,Kill),
								 #sell_reward{item=[R2|_T]} = Reward = merge_reward(MailEqualReward, R),
								 Total = R2#new_item.itemNum,
								 set_player(RoleID,ServerID,Player#player{score=Total}),
								 send_msg:direct(ServerID, carlos_server, {mail_reward, RoleID, ?MAIL_TEMPLATE_CARLOS_EQUAL, Reward,Total}),
								 Player2 = Player
						  end,
						  [carlos_rank:transform_player2p_player_info(Player2)|Acc];
					  (_) -> 
						  Acc
				  end
		  end,[],get()),
	carlos_rank:send_war_result_to_rank(equal,P_PlayerInfo_list);
send_winner_reward(attacker,AttackerGas,DefenderGas) ->
	send_winner_reward_1(attacker, AttackerGas,DefenderGas);
send_winner_reward(defender,AttackerGas,DefenderGas) ->
	send_winner_reward_1(defender, DefenderGas,AttackerGas).
send_winner_reward_1(Type,WinnerGas,FailGas) ->
	MailWinReward = data_carlos:get(win),
	MailFailReward = data_carlos:get(fail),
    AfkLimit = util:now() - data_carlos:get(afk_check_time),
	P_PlayerInfo_list = 
		lists:foldl(
		  fun(Elem,Acc) ->
				  case Elem of
					  {{?player,{RoleID,ServerID}},#player{type=TypePlayer,gas=Gas,killNum=KillNum,movDis=_MovDis}=Player}->
                          AfkTime = get({?pd_operate_ts,RoleID}),
                          if 
                              AfkTime =:= ?undefined orelse AfkTime < AfkLimit  ->
								 Player2 = Player#player{score=0},
								 set_player2(RoleID,ServerID,Player2),
								 send_msg:direct(ServerID, carlos_server,{mail_reward,RoleID,?MAIL_TEMPLATE_CARLOS_BAN});
							 true ->
								 case Type of
									 TypePlayer ->
										 R = get_reward(WinnerGas,Gas,KillNum),
										 #sell_reward{item=[R2|_T]} = Reward=merge_reward(MailWinReward,R),
										 Total = R2#new_item.itemNum,
										 set_player(RoleID,ServerID,Player#player{score=Total}),
										 send_msg:direct(ServerID, carlos_server, {mail_reward, RoleID, ?MAIL_TEMPLATE_CARLOS_Win, Reward,Total});
									 _ ->
										 R = get_reward(FailGas,Gas,KillNum),
										 #sell_reward{item=[R2|_T]} = Reward=merge_reward(MailFailReward,R),
										 Total = R2#new_item.itemNum,
										 set_player(RoleID,ServerID,Player#player{score=Total}),
										 send_msg:direct(ServerID, carlos_server,{mail_reward,RoleID, ?MAIL_TEMPLATE_CARLOS_FAIL, Reward,Total})
								 end,
								 Player2 = Player
						  end,
						  [carlos_rank:transform_player2p_player_info(Player2)|Acc];
					  (_) -> 
						  Acc
				  end
		  end,[],get()),
	carlos_rank:send_war_result_to_rank(Type,P_PlayerInfo_list).

send_mine_reward(ServerID,RoleID) ->
	#player{score=Score,gas=Gas} = Player = get_player(RoleID,ServerID),
	Add = data_carlos:get(mine_score),
	set_player(RoleID,ServerID,Player#player{score=Score+Add, gas=Gas+1}).
%	MineReward = data_carlos:get(mine),
%	send_msg:direct(ServerID, carlos_server, {mail_reward, RoleID, ?MAIL_TEMPLATE_CARLOS_MINE, MineReward}).

update_gas_speed(Type) ->
	case lists:all(fun({ID,_,_})-> 
						   #mine{owner=Owner,lastOwner=LastOwner}=get_mine(ID),
						   Owner == Type andalso LastOwner == Type
				   end,data_carlos:get(mine_list)) of
		true ->
			set_full_speed(Type),
			clear_full_speed(enermy_type(Type));
		false ->
			ignore
	end.

enermy_type(attacker) ->
	defender;
enermy_type(defender) ->
	attacker;
enermy_type(0) ->
	0.

set_full_speed(Type) ->
	
	put({?full_speed,Type},data_carlos:get(full_speed)).

get_full_speed(Type) ->
	case get({?full_speed,Type}) of
		Speed when is_integer(Speed) ->
			Speed;
		_ ->
			1
	end.

clear_full_speed(Type) ->
	put({?full_speed,Type},1).

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

get_hp_dtl(FighterList)->
	% {SumHP, SumHPMax} = 
	lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
						{AccHP + HP, AccHPMax + HPMax}
				end, {0, 0}, FighterList).

filter_out_zero_hp(List) ->
    lists:filter(fun(#ger{gerHp=GerHP}) ->
                         GerHP > 0
                 end, List).

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

add_replay(RL,RUID)->
	lists:sublist([RUID|RL], data_carlos:get(max_replay_num)).

update_player_killNum(#player{killNum=K,score=Score}=Player) ->
	Add = data_carlos:get(kill_score),
	Player#player{killNum=K+1,score=Score+Add}.

type2Owner(attacker) ->
	1;
type2Owner(defender) ->
	2;
type2Owner(_) ->
	0.

get_enermys(Type,#mine{attackerList=AttackerList,defenderList=DefenderList}) ->
	case Type of
		attacker ->
			DefenderList;
		defender ->
			AttackerList
	end.

delete_mine_player(Type,Value,#mine{attackerList=AttackerList,defenderList=DefenderList} = MineInfo) ->
	case Type of
		attacker ->
			NewList = lists:delete(Value, AttackerList),
			MineInfo#mine{attackerList=NewList};
		defender ->
			NewList = lists:delete(Value, DefenderList),
			MineInfo#mine{defenderList=NewList}
	end.
add_mine_player(#mine{attackerList=AttackerList,defenderList=DefenderList}=Mine,RoleID,ServerID,Type) ->
	case Type of
		attacker ->
			NewList = add2list({RoleID,ServerID},AttackerList),
			Mine#mine{attackerList=NewList};
		defender ->
			NewList = add2list({RoleID,ServerID},DefenderList),
			Mine#mine{defenderList=NewList}
	end.
check_mine_player(Type,Value,#mine{attackerList=AttackerList,defenderList=DefenderList}=_MinInfo) ->
	case Type of
		attacker->
			lists:member(Value,AttackerList);
		defender ->
			lists:member(Value,DefenderList)
	end.
mine_owner_change(Type,#mine{attackerList=AttackerList,defenderList=DefenderList}=MineInfo) ->
	case Type of
		attacker ->
			case AttackerList of
				[] ->
					update_target_gas(Type,MineInfo);
				_ ->
					MineInfo
			end;
		defender ->
			case DefenderList of
				[] ->
					update_target_gas(Type, MineInfo);
				_ ->
					MineInfo
			end
	end.

update_get_gas(Type,#mine{attackerGas=AttackerGas,defenderGas=DefenderGas,id=MineID}=MineInfo) ->
	Now=util:now(),
	mark_bc(MineID, []),
	case Type of
		attacker ->
			MineInfo#mine{attackerGas=AttackerGas#getGas{startGetSec=Now}};
		defender ->
			MineInfo#mine{defenderGas=DefenderGas#getGas{startGetSec=Now}};
		_ ->
			MineInfo
	end.

update_target_gas(Type,#mine{attackerGas=AttackerGas,defenderGas=DefenderGas,gas=Gas}=MineInfo) ->
	case Type of
		attacker ->
			#getGas{startGetSec=StartGetSec,gas=AlreadyGetGas,getspeed=Speed}=AttackerGas,
			{NewGetGas,Gas2,_} = calc_gas(StartGetSec,AlreadyGetGas,Speed * get_full_speed(Type),Gas),
			MineInfo#mine{attackerGas=AttackerGas#getGas{startGetSec=0,gas=NewGetGas},gas=Gas2};
		defender ->
			#getGas{startGetSec=StartGetSec,gas=AlreadyGetGas,getspeed=Speed}=DefenderGas,
			{NewGetGas,Gas2,_} = calc_gas(StartGetSec,AlreadyGetGas,Speed * get_full_speed(Type),Gas),
			MineInfo#mine{defenderGas=DefenderGas#getGas{startGetSec=0,gas=NewGetGas},gas=Gas2};
		_ ->
			MineInfo
	end.

update_mine_gas(#mine{owner=Type,lastOwner=Type,attackerGas=AttackerGas,defenderGas=DefenderGas,gas=Gas,id=MineID}=MineInfo) ->
	Now=util:now(),
	case Type of
		attacker ->
			#getGas{startGetSec=StartGetSec,gas=AlreadyGetGas,getspeed=Speed}=AttackerGas,
			{NewGetGas,Gas2,_} = calc_gas(StartGetSec,AlreadyGetGas,Speed * get_full_speed(Type),Gas),
			mark_bc(MineID,[]),
			MineInfo#mine{attackerGas=AttackerGas#getGas{startGetSec=Now,gas=NewGetGas},gas=Gas2};
		defender ->
			#getGas{startGetSec=StartGetSec,gas=AlreadyGetGas,getspeed=Speed}=DefenderGas,
			{NewGetGas,Gas2,_} = calc_gas(StartGetSec,AlreadyGetGas,Speed * get_full_speed(Type),Gas),
			mark_bc(MineID,[]),
			MineInfo#mine{defenderGas=DefenderGas#getGas{startGetSec=Now,gas=NewGetGas},gas=Gas2};
		_ ->
			MineInfo
	end;
update_mine_gas(MineInfo) ->
	MineInfo.

reset_mine(MineID) ->
	#mine{owner=Owner,lastOwner=LastOwner}= MineInfo = get_mine(MineID),
	case Owner of
		LastOwner ->
			MineInfo2 = update_get_gas(Owner, MineInfo),
			set_mine(MineID, MineInfo2);
		_ ->
			ignore
	end.


init_mine_get() ->
	[init_mine_get(MineID)||{MineID,_,_}<-data_carlos:get(mine_list)].
init_mine_get(MineID) ->
	#mine{owner=Owner,lastOwner=LastOwner}= MineInfo = get_mine(MineID),
	case Owner of
		LastOwner ->
			MineInfo2 = update_get_gas(Owner, MineInfo),
			set_mine(MineID, MineInfo2);
		_ ->
			ignore
	end.

calc_gas(0,_,_,Gas) ->
	{0,Gas,0};
calc_gas(_,AlreadyGetGas,_,0) ->
	{AlreadyGetGas,0,0};
calc_gas(StartGetSec,AlreadyGetGas,Speed,Gas) ->
	Now = util:now(),
	MaxGet = Speed * (Now-StartGetSec),
	ReallyGet = get_min(MaxGet,Gas),
	{ReallyGet + AlreadyGetGas , Gas-ReallyGet,ReallyGet}.
	

get_min(X,Y) when X > Y ->
	Y;
get_min(X,_Y) ->
	X.

add2list(Value,List) ->
	case lists:member(Value, List) of
		true ->
			List;
		_ ->
			[Value|List]
	end.


cs_carlos_ownMine(MineID,ServerID,RoleID) ->	
	#player{tarMineID=_TarMineID,type=Type,roleName=RoleName} = _Player = get_player(RoleID,ServerID),
	#mine{owner=Owner,lastOwner=LastOwner,beOwnSec=BOS}= MineInfo = get_mine(MineID),
	{_,A,B} = 
		case check_mine_player(Type,{RoleID,ServerID},MineInfo) of
			true ->
				Now = util:now(),
				if LastOwner == Type ->
					   if Owner ==Type ->
							  {false, 3, 0};
						  true ->
							  case get_enermys(Type,MineInfo) of
								  [] ->
									  MineInfo2 = update_get_gas(Type, MineInfo),
									  set_mine(MineID,MineInfo2#mine{owner=Type,beOwnSec=0}),
									  cacel_mine_gas_plan(MineID),
									  send_mine_reward(ServerID,RoleID),
									  update_gas_speed(Type),
									  add_sys_msg(fangshou, RoleName,MineID),
									  {true, 4,0};
								  _ ->
									  {false, 5,0}
							  end
					   end;
				   true ->
					   if Owner == Type ->
							  if BOS > Now ->
									 {true, 6, BOS};
								 true ->
									 MineInfo2 = update_get_gas(Type,MineInfo),
									 set_mine(MineID,MineInfo2#mine{owner=Type,beOwnSec=0}),
									 send_mine_reward(ServerID,RoleID),
									 update_gas_speed(Type),
									 case LastOwner of
										 0 ->
											 add_sys_msg(zhanling, RoleName,MineID);
										 _ ->
											 ok
									 end,
									 ?ERR("something error, mine gas plan lost"),
									 {true, 4, 0}
							  end;
						  true ->
							  case get_enermys(Type, MineInfo) of
								  [] ->
									  NBOS = data_carlos:get(own_mine_interval) + Now,
									  set_mine(MineID,MineInfo#mine{owner=Type,beOwnSec=NBOS}),
									  add_mine_gas_plan(MineID,NBOS,RoleID,ServerID),
									  {true, 6, NBOS};
								  _ ->
									  {false, 5, 0}
							  end
					   end
				end;
			_ ->
				{false, 2, 0}
		end,
	{A,B}.

update_player(#player{roleID=RoleID}=Player,Pos,MineID)->
	Player2 = Player#player{tarMineID=MineID,endPos=Pos,startTime=util:now()},
	add_mov_plan(RoleID,Player2),
	Player2.

update_player_fighters(Player,Blood,NewRL2,NewFightersA2)->
	Player#player{blood=Blood,fighters=NewFightersA2,replayList=NewRL2}.

reborn_player(#player{type=Type,baseFighters=BF}=Player) ->
	Pos = data_carlos:get({born_pos, Type}),
	RebornInterval = data_carlos:get(reborn_interval),
	Player#player{startPos=Pos,endPos=Pos,startTime=0,tarMineID=0,fighters=BF,blood=100,rebornSec=util:now()+RebornInterval}.

reborn_player_now(#player{type=Type,baseFighters=BF}=Player) ->
	Pos = data_carlos:get({born_pos, Type}),
	Player#player{startPos=Pos,endPos=Pos,startTime=0,tarMineID=0,fighters=BF,blood=100,rebornSec=0}.

%  更新player的起止位置到当前移动位置
reset_player(#player{startPos=Pos,endPos=Pos}=Player) ->
	Player;
reset_player(#player{startPos=Pos0,endPos=Pos1,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	Now=util:now(),
	TimeDiff = Now-ST,
	MovDis = Speed * TimeDiff,
	NowPos = calc_new_pos(Pos0,Pos1,MovDis),
	Player#player{startPos=NowPos,endPos=NowPos,startTime=Now,movDis=MovDis0+MovDis}.

%  更细player的起始位置到当前移动位置
reset_player2(#player{startPos=Pos,endPos=Pos}=Player) ->
	Player;
reset_player2(#player{startPos=Pos0,endPos=Pos1,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	Now=util:now(),
	TimeDiff = Now-ST,
	MovDis = Speed * TimeDiff,
	NowPos = calc_new_pos(Pos0,Pos1,MovDis),
	Player#player{startPos=NowPos,startTime=Now,movDis=MovDis0+MovDis}.

% 更新player到目标终点
reset_player3(#player{startPos=Pos,endPos=Pos}=Player) ->
	Player;
reset_player3(#player{endPos=Pos1,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	Now=util:now(),
	MovDis=Speed*(Now-ST),
	Player#player{startPos=Pos1,endPos=Pos1,startTime=Now,movDis=MovDis0+MovDis}.


calc_new_pos(Pos0,Pos0,_) ->
	Pos0;
calc_new_pos({A,B},{C,D}, Dis) ->
	Dx = A-C,
	Dy = B-D,
	DL = math:sqrt(Dx*Dx+Dy*Dy),
	Tx = trunc((Dx*Dis)/DL)-1,
	Ty = trunc((Dy*Dis)/DL)-1,
	{A-Tx,B-Ty}.

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

init_player_mov()->
	lists:foreach(fun({{?player,{RoleID,_}},Player}) ->
						  Player2 = reset_player2(Player),
						  add_mov_plan(RoleID,Player2);
					 (_) -> ignore
				    end, get()).

plan()->
	Plans = get_plan(),
	Now = util:now(),
	Plans2 = 
		lists:foldl(fun(Plan=#plan{sec=Sec,value=Value},Acc)-> 
							if Sec =< Now ->
								   %?ERR("do_plan:~w",[Plan]),
								   plan(Value),
								   Acc;
							   true ->
								   [Plan|Acc]
							end
					end,[],Plans),
	set_plan(Plans2).

plan(Value) ->
	?LOOSE_CATCH(Value()).


do_bc(talk,Type,Data,carlos_bc_info) ->
	BcList = get_bcList(Type),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,carlos_server,{carlos_bc_info,RoleID,Data})
						  end,BcList).

do_bc(Info,Type) ->
	BcList = get_bcList(),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,carlos_server,{Type,RoleID,Info})
						  end,BcList).
bc_info(open) ->
	BaseInfo = get_war_base_info(),
	do_bc(BaseInfo,carlos_bc_info);
bc_info(mark) ->
	put(?bc_mark,0),
	{ML,RL} = get_mark_bc(),
	MI = [mine2carlosMine(get_mine(E))||E<-ML],
	RI = [player2carlosPlayer(get_player(A,B))||{A,B}<-RL],
	Msg = #sc_carlos_update{player=RI,mine=MI},
	set_mark_bc({[],[]}),
	do_bc(Msg,carlos_bc_info).
bc_info(end_war,Type,Winner) ->
	Msg = #sc_carlos_end_war{type=Type, winner=type2Owner(Winner),data=get_rank_data()},
	do_bc(Msg,carlos_bc_info).

get_war_base_info()->
	{Mines,Players} = 
		lists:foldl(fun({{?mine,_},Mine},{MineAcc,PlayerAcc}) -> {[mine2carlosMine(Mine)|MineAcc],PlayerAcc};
					   ({{?player,_},Player},{MineAcc,PlayerAcc})-> {MineAcc,[player2carlosPlayer(Player)|PlayerAcc]};
					   (_,Acc) -> Acc
					end, {[],[]}, get()),
	{_,EndTime} = get(?endInfo),
	AttackerPos = data_carlos:get({born_pos, attacker}),
	DefenderPos = data_carlos:get({born_pos, defender}),
	#sc_carlos_war_base_info{result=1,endTimeStamp=EndTime,attackerPos=pos2carlosPos(AttackerPos)
							,defenderPos=pos2carlosPos(DefenderPos),mines=Mines,players=Players}.
		

pos2carlosPos({X,Y}) ->
	#p_carlos_pos{x=X,y=Y}.
player2carlosPlayer(Player) ->
	?INFO("Player:~w ~n",[Player]),
	#p_carlos_player{roleID=Player#player.roleID
					,serverID=Player#player.serverID
					,startPos=pos2carlosPos(Player#player.startPos)
					,endPos=pos2carlosPos(Player#player.endPos)
					,fly=Player#player.fly
					,type=type2Owner(Player#player.type)
					,mineID=Player#player.tarMineID
					,blood=Player#player.blood
					%,replayList=Player#player.replayList
					,rebornSec=Player#player.rebornSec
					,name=Player#player.roleName
					,startSec=Player#player.startTime
					,fightPower=Player#player.fight_power
					,roleLevel=Player#player.level
                    ,speed=Player#player.speed}.
mine2carlosMine(#mine{attackerGas=AttackerGas,defenderGas=DefenderGas}=Mine) ->
	#p_carlos_mine{mineID=Mine#mine.id
				  ,owner=type2Owner(Mine#mine.owner)
				  ,pos=pos2carlosPos(Mine#mine.pos)
				  ,gas=Mine#mine.totalGas
				  ,nowGas=Mine#mine.gas
				  ,p1Gas=AttackerGas#getGas.gas
				  ,p2Gas=DefenderGas#getGas.gas
				  ,ownSec=Mine#mine.beOwnSec
				  ,attackerNum=length(Mine#mine.attackerList)
				  ,defenderNum=length(Mine#mine.defenderList)}.
mine2carlosMineDtl(#mine{attackerList=Attackers,defenderList=Defenders}=Mine) ->
	#sc_carlos_mine_detail{mine=mine2carlosMine(Mine)
						  ,lastOwner=type2Owner(Mine#mine.lastOwner)
						  ,attackers=[player2carlosPlayer(get_player(R,S))||{R,S}<-Attackers]
						  ,defenders=[player2carlosPlayer(get_player(R,S))||{R,S}<-Defenders]}.
fairy2carlosFairy(FL) when is_list(FL) ->
	[fairy2carlosFairy(F)||F<-FL];
fairy2carlosFairy(#ger{gerBase=#gerBase{gerTypeID=TypeID,gerQuality=Rank}
					  ,gerAttr=#gerAttr{gerHpMax=HpMax},gerHp=Hp})->
	#p_carlos_fairy{typeID=TypeID
				   ,maxHp=HpMax
				   ,nowHp=Hp
				   ,rank=Rank}.
player2p_carlos_rank_dtl(Player)->
	#p_carlos_rank_dtl{roleID=Player#player.roleID
					  ,serverID=Player#player.serverID
					  ,level=Player#player.level
					  ,get=Player#player.gas
					  ,score=Player#player.score
					  ,kill=Player#player.killNum
					  ,name=Player#player.roleName
					  }.
player2p_carlos_rank_dtl2(Player)->
    AfkTime = get({?pd_operate_ts,Player#ga_player.roleID}),
    AfkLimit = util:now() - data_carlos:get(afk_check_time),
	#p_carlos_rank_dtl{roleID=Player#player.roleID
					  ,serverID=Player#player.serverID
					  ,level=Player#player.level
					  ,get=Player#player.gas
					  ,score=Player#player.score
					  ,kill=Player#player.killNum
					   ,name=Player#player.roleName
					   ,type = if
								   AfkTime =:= ?undefined orelse AfkTime < AfkLimit ->
									   1;
								   true ->
									   0
							   end
					  }.

sync_role({RoleID,ServerID},{TarRoleID,TarServerID})->
	P1 = player2carlosPlayer(get_player(RoleID,ServerID)),
	P2 = player2carlosPlayer(get_player(TarRoleID,TarServerID)),
	Msg = #sc_carlos_war_update{newInfo=[P1,P2]},
	carlos_router:send_client(ServerID,RoleID,Msg),
	carlos_router:send_client(TarServerID,TarRoleID,Msg),
	ok.

mark_bc(Mine,Role) ->
	{ML,RL} = get_mark_bc(),
	Info = {add_bc_list(Mine,ML),add_bc_list(Role,RL)},
	set_mark_bc(Info),
	case get(?bc_mark) of
		1 ->
			ignore;
		_ ->
			put(?bc_mark,1),
			erlang:send_after(1000,self(), {bc_info, mark})
	end.

add_bc_list([],List) ->
	List;
add_bc_list(Value,List) ->
	case lists:member(Value, List) of
		true ->
			List;
		_ ->
			[Value|List]
	end.
get_mark_bc()->
	case get(?mark_bc) of
		?undefined ->
			{[],[]};
		X ->
			X
	end.
set_mark_bc(Info) ->
	put(?mark_bc, Info).

add_mine_gas_plan(MineID,Sec,RoleID,ServerID) ->
	F = fun() -> erlang:send(self(), {mine_gas,MineID,RoleID,ServerID}) end,
	add_plan(#plan{sec=Sec,key=MineID, value=F}).

cacel_mine_gas_plan(MineID) ->
	delete_plan(MineID).

add_mov_plan(RoleID,#player{serverID=ServerID,tarMineID=MineID,startPos=Start,endPos=End,speed=Speed}=_P)->
	TimeDiff = calc_time(Start,End,Speed),
	F = fun()->erlang:send(self(), {player_arrive, RoleID,ServerID,MineID}) end,
	add_plan(#plan{sec=util:now()+TimeDiff, key={RoleID,ServerID},value=F}).

cancel_mov_plan(RoleID,ServerID) ->
	delete_plan({RoleID,ServerID}).

check_winner_plan()->
	F = fun()->
				erlang:send(self(), check_winner)
		end,
	Interval = data_carlos:get(check_interval),
	add_plan(#plan{sec=util:now()+Interval, key=check_winner_plan, value=F}).

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

set_pd_operate_ts(RoleID)->
    put({?pd_operate_ts,RoleID},util:now()).

