%% @author crimoon-17
%% @doc Add description to war_server.
%% 按照出生点,出生在下方的是attacker:0,出生在上方的是defender:1  
%% 


-module(galactica_war_server).
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
-define(home,home).
-define(gas,gas).
-define(plan,plan).
-define(bcList,bcList).
-define(endInfo, endInfo).
-define(bc_mark,bc_mark).
-define(mark_bc,mark_bc).
-define(talk_data,talk_data).
-define(rank_data, rank_data).
-define(mov_plan,mov_plan).
-define(pd_operate_ts, pd_operate_ts).

-define(Dump_interval, 60000).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).
start_link(WarID,Attackers,Defenders) ->
	gen_server:start_link(?MODULE,[WarID,Attackers, Defenders],[]).
start(Args) ->
		supervisor:start_child(galactica_war_sup,Args).

init([WarID,Attacker,Defender]) ->
	add_ets_war_info(WarID,self()),
	init_players(attacker, Attacker),
	init_players(defender, Defender),
	init_bcList(Attacker++Defender),
	init_ground(),
	init_gas(),
	tick_plan(),
	bc_open(),
	set_end_info(WarID),
    galactica_ai:init(Attacker++Defender),
	check_winner_plan(),
	{ok,#state{warID=WarID,warStartTime=util:now()}}.

init_bcList(Players) ->
	RL=[{RoleID,ServerID}||#ga_player{roleID=RoleID,serverID=ServerID}<-Players],
	put(?bcList, RL),
	RLA = [{RoleID,ServerID}||#ga_player{roleID=RoleID,serverID=ServerID,type=Type}<-Players,Type==attacker],
	put({?bcList,attacker},RLA),
	RLD = [{RoleID,ServerID}||#ga_player{roleID=RoleID,serverID=ServerID,type=Type}<-Players,Type==defender],
	put({?bcList,defender},RLD).

init_gas() ->
	put({?gas,1},0),
	put({?gas,2},0).

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
	Interval = data_galactica:get(war_interval),
	put(?endInfo,{WarID,util:now()+Interval}),
	erlang:send_after(Interval * 1000, self(), do_end_war),
	ok.

tick_plan()->
	erlang:send_after(1000, self(), tick_plan).

bc_open()->
	erlang:send(self(), {bc_info, open}).

add_ets_war_info(WarID,PID) -> 
	ets:insert(?ETS_GALACTICA_INFO,#ets_galactica_info{warID=WarID,pid=PID}).

init_players(Type,Players) ->
	Pos = data_galactica:get({born_pos, Type}),
	%[put({?player, {RoleID,ServerID}}, Info#ga_player{startPos=Pos,endPos=Pos})
	[set_player(RoleID,ServerID,Info#ga_player{startPos=Pos})
	||#ga_player{roleID=RoleID,serverID=ServerID}=Info<-Players].

init_ground() ->
	MineList = data_galactica:get(mine_list),
	[put({?mine, ID},#ga_mine{id=ID,pos=Pos,gas=Gas,home=Home,maxNum=Max,speed=Speed,score=Score})
	||{ID,Pos,Gas,Home,Max,Speed,Score}<-MineList],
	AttackerHomeList = data_galactica:get({home_list,attacker}),
	[put({?home, ID}, #ga_home{id=ID, pos=Pos,type=1,mineIDs=[]})||{ID,Pos}<-AttackerHomeList],
	DefenderHomeList = data_galactica:get({home_list,defender}),
	[put({?home, ID}, #ga_home{id=ID, pos=Pos, type=2,mineIDs=[]})||{ID, Pos}<-DefenderHomeList].	


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
%% 	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p"
%% 		  ,[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_handle_info({bc_info,Type}) ->
	bc_info(Type),
	ok;
do_handle_info(tick_plan) ->
	tick_plan(),
	plan(),
    galactica_ai:check_robot_act(),
	ok;
do_handle_info(do_end_war) ->
	end_war(),
	ok;
do_handle_info({cs_galactica_war_base_info, {_,RoleID,ServerID}}) ->
	Msg = get_war_base_info(),
	galactica_router:send_client(ServerID,RoleID,Msg),
	ok;
do_handle_info({cs_galactica_self, {_,RoleID,ServerID}}) ->
	Player = get_player(RoleID,ServerID),
	Msg = #sc_galactica_self{self=player2galaticaPlayerS(Player)},
	galactica_router:send_client(ServerID,RoleID, Msg),
	ok;
do_handle_info({{cs_galactica_mov, MineID, IsAi}, {_,RoleID,ServerID}}) ->
    if
        IsAi =:= false -> set_pd_operate_ts(RoleID);
        true -> ignore
    end,
	#ga_player{tarMineID=TarMineID,rebornSec=RebornSec,type=Type,startPos=Pos} = Player = get_player(RoleID,ServerID),
	Now =util:now(),
	IsMov = case TarMineID of
				MineID->
					false;
				0 ->
					if Now > RebornSec -> true; true -> false end;
				_ ->
					if Now > RebornSec -> reset; true -> false end
			end,
	Msg = %% 如果原来有目标mine，则更新目标mine信息并开始移动，否则直接开始移动
		case IsMov of
			false ->
				#sc_galactica_mov{result=2};
			reset ->
				#ga_mine{home=OHome,pos=StartPos}=TarMine= get_mine(TarMineID),
				if is_integer(OHome) ->
						#sc_galactica_mov{result=3};
				   true ->
						TarMineInfo2 =
							case check_mine_player(Type,{RoleID,ServerID},TarMine) of
								true -> delete_mine_player(Type,{RoleID,ServerID},TarMine);
								_ -> delete_mine_player_waiting(Type,{RoleID,ServerID},TarMine)
							end,
						TarMineInfo3 = update_mine_mov(TarMineInfo2),
						Player2 = reset_player(Player),
						set_mine(TarMineID,TarMineInfo3),
						Player3 = case Pos of %% 如果在mine上，则取得mine的pos为startPos，否则为reset得到的pos为startPos
									  {0,0} ->
										  mark_bc(TarMineID,{RoleID,ServerID}),
										  update_player(Player2,StartPos,MineID);
									  _ ->
										  mark_bc([],{RoleID,ServerID}),
										  Player2#ga_player{tarMineID=MineID}
								  end,
						set_player(RoleID,ServerID,Player3),
						#sc_galactica_mov{result=1}
				end;
			_ ->
				Player2 = update_player(Player,Pos,MineID),
				set_player(RoleID,ServerID,Player2),
				mark_bc([],{RoleID,ServerID}),
				#sc_galactica_mov{result=1}
		end,
	galactica_router:send_client(ServerID,RoleID,Msg),
	ok;

do_handle_info({{cs_galactica_attack, TarRoleID,TarServerID,MineID,IsAi}, {_,RoleID,ServerID}}) ->
    if
        IsAi =:= false -> set_pd_operate_ts(RoleID);
        true -> ignore
    end,
	#ga_player{tarMineID=TarMineID,type=Type,fighters=Fighters1,addBuff=Buff1,talent=Talent1
		   ,itemList=ItemList1,replayList=RL1,roleName=RoleName1,trSpecial=TrSpecial1,skin_info=SkinInfo1} = Player = get_player(RoleID,ServerID),
	#ga_player{tarMineID=TarMineID2,type=Type2,fighters=Fighters2,addBuff=Buff2,talent=Talent2
		   ,itemList=ItemList2,replayList=RL2,roleName=RoleName2,trSpecial=TrSpecial2,skin_info=SkinInfo2} = Player2 = get_player(TarRoleID,TarServerID),
	{IsSucc,Result,FightInfo} = 
	if TarMineID == MineID andalso TarMineID2 == MineID ->
		   if Type /= Type2 ->
				  MineInfo = get_mine(MineID),
				  case check_mine_player(Type,{RoleID,ServerID},MineInfo) andalso check_mine_player(Type2,{TarRoleID,TarServerID},MineInfo) of
					  true ->
					  	  GerEquipList1 = role_item:assort_ger_equiplist(ItemList1),
    					  LegendAddList1 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList1],
                          GerEquipList2 = role_item:assort_ger_equiplist(ItemList2),
                          LegendAddList2 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList2],
						  case ?CATCH(role_fight:new(filter_out_zero_hp(Fighters1),filter_out_zero_hp(Fighters2),Buff1,Buff2,Talent1,Talent2,TrSpecial1,TrSpecial2,SkinInfo1,SkinInfo2,LegendAddList1,LegendAddList2)) of
							  {IsWin, FightRecord0, {_,_,NewFighters1,NewFighters2}} ->
								  FighterList2 = role_data:get_FighterList_with_effect(ItemList1,ItemList2,FightRecord0#sc_fight_request.fighterList),
								  FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
								  ReplayUID = tk_id:gen_replayUID(),
								  galactica_replay_server:record_replay(ReplayUID,FightRecord),
								  ReplayDtl = #p_galactica_replay_dtl{replayUID=ReplayUID,isRole1Win=bool2int(IsWin),role1Name=RoleName1,role2Name=RoleName2},
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
										  MineInfo4 = update_mine_mov(MineInfo3),
										 set_mine(MineID,MineInfo4),
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
												MineInfo3 = update_mine_mov(MineInfo2),
%												MineInfo3 = mine_owner_change(Type2,MineInfo2),
												set_mine(MineID,MineInfo3),
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
												 MineInfo3 = update_mine_mov(MineInfo2),
												set_mine(MineID, MineInfo3),
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
	if IsSucc ->
			bc_info(gas);
	   true ->
			ignore
	end,
	galactica_router:send_client(ServerID,RoleID,#sc_galactica_attack{result=Result,fightInfo=FightInfo}),
	ok;
do_handle_info({{cs_galactica_role_dtl, TarRoleID,TarServerID},{_,RoleID,ServerID}})->
	#ga_player{fighters=F,replayList=RepList,grade=Grade} = get_player(TarRoleID,TarServerID),
	FL = fairy2galacticaFairy(F),
	galactica_router:send_client(ServerID,RoleID,#sc_galactica_role_dtl{target=FL,replayDtl=RepList,grade=Grade}),
	ok;
do_handle_info({cs_galactica_mov_stop,{_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	#ga_player{tarMineID=TarMineID,startPos=StartPos}=Player = get_player(RoleID,ServerID),
	case TarMineID of
		0 ->
			ignore;
		_ ->
			cancel_mov_plan(RoleID,ServerID),
			case StartPos of
				{0,0} ->
					%%set_player(RoleID,ServerID,Player2);
					ignore;
				_ ->
					Player2 = reset_player(Player),
					set_player(RoleID,ServerID,Player2#ga_player{tarMineID=0})
			end,
			mark_bc([],{RoleID,ServerID})
	end,
	ok;
do_handle_info({{cs_galactica_mov_in,MineID},{_,RoleID,ServerID}})->
    set_pd_operate_ts(RoleID),
	Result = case player_join(RoleID,ServerID,MineID) of true -> 1; false ->2 end,
	galactica_router:send_client(ServerID,RoleID,#sc_galactica_mov_in{result=Result}),
	ok;

do_handle_info({{cs_galactica_talk,Data}, {_,RoleID,ServerID}}) ->
	#ga_player{type=Type} = get_player(RoleID, ServerID),
	add_talk_data(Type, Data),
	do_bc(talk,Type,#sc_galactica_talk{data=Data},galactica_bc_info),
	ok;
do_handle_info({cs_galactica_get_talk, {_,RoleID,ServerID}}) ->
	#ga_player{type=Type} = get_player(RoleID,ServerID),
	TalkData = get_talk_data(Type),
	galactica_router:send_client(ServerID,RoleID,#sc_galactica_get_talk{data=TalkData}),
	ok;
do_handle_info({cs_galactica_get_rank, {_,RoleID,ServerID}}) ->
%	#ga_player{type=Type} = get_player(RoleID,ServerID),
	RankList = get_rank_data(),
	galactica_router:send_client(ServerID,RoleID, #sc_galactica_get_rank{rank=RankList}),
	ok;
do_handle_info(check_winner) ->
	MineList = 
		[begin
			 update_mine_pos(MineID),
			 #ga_mine{home=OHome} = get_mine(MineID),
			 OHome
%% 			 MineInfo2 = update_mine_gas(get_mine(MineID)),
%% 			 set_mine(MineID,MineInfo2)
		 end||{MineID,_,_,_,_,_,_}<-data_galactica:get(mine_list)],
		update_players_pos(),
	{Winner,AttackerGas,DefenderGas} = get_war_winner2(),
	Winner2 = is_mine_all_homed(MineList,Winner,AttackerGas,DefenderGas),
	case Winner2 of
		ignore ->
			check_winner_plan();
		_ ->
			end_war(Winner2,AttackerGas,DefenderGas)
	end,
	ok;
do_handle_info({cs_galactica_reborn,{_,RoleID,ServerID}}) ->
    set_pd_operate_ts(RoleID),
	Player = reborn_player_now(get_player(RoleID,ServerID)),
	set_player(RoleID,ServerID, Player),
	mark_bc([],{RoleID,ServerID}),
	ok;

do_handle_info(Info) ->
	?ERR("error get info:~w",[Info]).

is_mine_all_homed(MineList,ignore,AttackerGas,DefenderGas) ->
	MineIDs = [OHome||OHome<-MineList, not(is_integer(OHome))],
	case MineIDs of
		[] ->
			if AttackerGas > DefenderGas ->
				   attacker;
			   AttackerGas == DefenderGas ->
				   equal;
			   AttackerGas < DefenderGas ->
				   defender
			end;
		_ ->
			ignore
	end;
is_mine_all_homed(_,Winner,_,_) ->
	Winner.

bool2int(true ) ->
	1 ;
bool2int(_) ->
	0.


add_sys_msg(Type,RoleName, MineID) ->
	ID = data_galactica:get(Type),
	Msg = #p_galactica_talk{roleID=0,roleName=RoleName,data=ID,ext=MineID},
	add_talk_data(attacker, Msg),
	add_talk_data(defender,Msg),
	do_bc(#sc_galactica_talk{data=Msg},galactica_bc_info),
	ok.

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
	Len = data_galactica:get(talk_len),
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
update_player_rank_data(#ga_player{roleID=RoleID,serverID=ServerID}=Player)->
	Data = get_rank_data(), 
	Data2 = [E||#p_galactica_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
	Data3 = [player2p_galactica_rank_dtl(Player)|Data2],
	set_rank_data(Data3).
update_player_rank_data2(#ga_player{roleID=RoleID,serverID=ServerID}=Player)->
	Data = get_rank_data(), 
	Data2 = [E||#p_galactica_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
	Data3 = [player2p_galactica_rank_dtl2(Player)|Data2],
	set_rank_data(Data3).


stop() ->
	{WarID,_} = get(?endInfo),
	do_bc(WarID,update_galactica_war),
	galactica_war_manager_server:war_stoped(WarID),
	erlang:send(self(), stop).

end_war()->
	{Winner,AttackerGas,DefenderGas} = get_war_winner(),
	end_war(Winner,AttackerGas,DefenderGas).
end_war(Winner,AttackerGas,DefenderGas) ->
	send_winner_reward(Winner,AttackerGas,DefenderGas),
	bc_info(end_war,2,Winner),
	stop().

get_war_winner()->
	AttackerGas =get_gas(1),
	DefenderGas = get_gas(2),
	if AttackerGas > DefenderGas ->
		   {attacker, AttackerGas,DefenderGas};
	   AttackerGas == DefenderGas ->
		   {equal,AttackerGas,DefenderGas};
	   true ->
		   {defender,AttackerGas,DefenderGas}
	end.

get_war_winner2() ->
	AttackerGas=get_gas(1),
	DefenderGas=get_gas(2),
	WinGas = data_galactica:get(winner_gas),
	if AttackerGas > DefenderGas ->
		   if AttackerGas >= WinGas ->
				  {attacker,AttackerGas,DefenderGas};
			  true ->
				  {ignore,AttackerGas,DefenderGas}
		   end;
	   AttackerGas == DefenderGas ->
		   if AttackerGas >= WinGas ->
				  {equal,AttackerGas,DefenderGas};
			  true ->
				  {ignore,AttackerGas,DefenderGas}
		   end;
	   AttackerGas < DefenderGas ->
		   if DefenderGas >= WinGas ->
				  {defender,AttackerGas,DefenderGas};
			  true ->
				  {ignore,AttackerGas,DefenderGas}
		   end
	end.


get_reward(Score) ->
	#new_item{itemTypeID=20040,itemNum=Score,itemLevel=1,itemRank=0}.

update_kill_gas(attacker) ->
	add_kill_score(1);
update_kill_gas(defender)->
	add_kill_score(2).
add_kill_score(Type)->
	KC = data_galactica:get(kill_gas),
	update_win_gas(Type,KC,0).

merge_reward(#sell_reward{item=[#new_item{itemNum=K}|T]},New = #new_item{itemNum=S}) ->
	Total = S+K,
	{Min,Max} = data_galactica:get(score_limit),
	Get = if Total > Max ->  Max;
			 Total < Min ->  Min;
			 true        ->  Total
		  end,
	#sell_reward{item=[New#new_item{itemNum=Get}|T]}.

send_winner_reward(equal,_AttackerGas,_) ->
	MailEqualReward = data_galactica:get(equal),
    AfkLimit = util:now() - data_galactica:get(afk_check_time),
	lists:foreach(
	  fun(Elem) ->
			  case Elem of
				  {{?player,{RoleID,ServerID}},#ga_player{score=Score,movDis=MovDis}=Player}->
                    AfkTime = get({?pd_operate_ts,RoleID}),
                    if 
                         AfkTime =:= ?undefined orelse AfkTime < AfkLimit  ->
							 Player2 = Player#ga_player{score=0},
							 set_player2(RoleID,ServerID,Player2),
							 send_msg:direct(ServerID, galactica_server,{mail_reward,RoleID,?MAIL_TEMPLATE_GALACTICA_BAN});
						 true ->
							 R = get_reward(Score),
							 #sell_reward{item=[R2|_T]} = Reward = merge_reward(MailEqualReward, R),
							 Total = R2#new_item.itemNum,
							 set_player(RoleID,ServerID,Player#ga_player{score=Total}),
							 send_msg:direct(ServerID, galactica_server, {mail_reward, RoleID, ?MAIL_TEMPLATE_GALACTICA_EQUAL, Reward,Total})
					  end;
				  _ ->
					  ignore
			  end
	  end,get());
send_winner_reward(attacker,AttackerGas,DefenderGas) ->
	send_winner_reward_1(attacker, AttackerGas,DefenderGas);
send_winner_reward(defender,AttackerGas,DefenderGas) ->
	send_winner_reward_1(defender, DefenderGas,AttackerGas).
send_winner_reward_1(Type,_WinnerGas,_FailGas) ->
	MailWinReward = data_galactica:get(win),
	MailFailReward = data_galactica:get(fail),
    AfkLimit = util:now() - data_galactica:get(afk_check_time),
	lists:foreach(
	  fun(Elem) ->
			  case Elem of
				  {{?player,{RoleID,ServerID}},#ga_player{score=Score,type=TypePlayer,movDis=MovDis}=Player}->
                    AfkTime = get({?pd_operate_ts,RoleID}),
                    if 
                         AfkTime =:= ?undefined orelse AfkTime < AfkLimit  ->
							 Player2 = Player#ga_player{score=0},
							 set_player2(RoleID,ServerID,Player2),
							 send_msg:direct(ServerID, galactica_server,{mail_reward,RoleID,?MAIL_TEMPLATE_GALACTICA_BAN});
						 true ->
							 case Type of
								 TypePlayer ->
									 R = get_reward(Score),
									 #sell_reward{item=[R2|_T]} = Reward=merge_reward(MailWinReward,R),
									 Total = R2#new_item.itemNum,
									 set_player(RoleID,ServerID,Player#ga_player{score=Total}),
									 send_msg:direct(ServerID, galactica_server, {mail_reward, RoleID, ?MAIL_TEMPLATE_GALACTICA_WIN, Reward,Total});
								 _ ->
									 R = get_reward(Score),
									 #sell_reward{item=[R2|_T]} = Reward=merge_reward(MailFailReward,R),
									 Total = R2#new_item.itemNum,
									 set_player(RoleID,ServerID,Player#ga_player{score=Total}),
									 send_msg:direct(ServerID, galactica_server,{mail_reward,RoleID, ?MAIL_TEMPLATE_GALACTICA_FAIL, Reward,Total})
							 end
					  end;
				  _ ->
					  ignore
			  end
	  end,get()).


enermy_type(attacker) ->
	defender;
enermy_type(defender) ->
	attacker;
enermy_type(0) ->
	0.


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
	lists:sublist([RUID|RL], data_galactica:get(max_replay_num)).

update_player_killNum(#ga_player{type=Type,killNum=K,score=Score}=Player) ->
	Add = data_galactica:get(kill_score),
	update_kill_gas(Type),
	Player#ga_player{killNum=K+1,score=calc_player_gas(Score,Add)}.

type2Owner(attacker) ->
	1;
type2Owner(defender) ->
	2;
type2Owner(_) ->
	0.

get_enermys(Type,#ga_mine{attackerList=AttackerList,defenderList=DefenderList}) ->
	case Type of
		attacker ->
			DefenderList;
		defender ->
			AttackerList
	end.
get_friends(Type,#ga_mine{attackerList=AttackerList,defenderList=DefenderList}) ->
	case Type of
		attacker ->
			AttackerList;
		defender ->
			DefenderList
	end.

update_mine_mov(Mine=#ga_mine{attackerList=AttackerList,speed=Speed,defenderList=DefenderList
							 ,startSec=SC,movTarget=OldTarget,pos=Pos,id=MineID}) ->
	LA = length(AttackerList),
	LD = length(DefenderList),
	Now = util:now(),
	Dis = (Now -SC) * Speed,
	if LA > LD ->
		   NewTarget =get_mine_target(Mine,attacker),
		   {NewPos,NewSc} = case NewTarget of
					   OldTarget ->
						   {Pos,SC};
					   _ ->
						   {calc_new_mine_pos(OldTarget,Dis,MineID,Pos),Now}
							end,
		   Mine#ga_mine{pos=NewPos,movTarget=NewTarget,startSec=NewSc};
	   LA == LD ->
			Mine#ga_mine{movTarget=0,startSec=0,pos=calc_new_mine_pos(OldTarget,Dis,MineID,Pos)};
	   true ->
		   NewTarget =get_mine_target(Mine,defender),
		   {NewPos,NewSc} = case NewTarget of
					   OldTarget ->
						   {Pos,SC};
					   _ ->
						   {calc_new_mine_pos(OldTarget,Dis,MineID,Pos),Now}
				   end,
			Mine#ga_mine{pos=NewPos,movTarget=NewTarget,startSec=NewSc}
	end.

calc_new_mine_pos(0,_,_MineID,Pos0) ->
	Pos0;
calc_new_mine_pos(HID,MovDis,MineID,Pos0)->
	#ga_home{pos=Pos1,mineIDs=MineIDs} = get_home(HID),
		   case lists:member(MineID,MineIDs) of
			   false ->
				   calc_new_pos(Pos0,Pos1,MovDis);
			   true ->
				   Pos1
		   end.

get_mine_target(Mine,attacker)->
	{H,_} = Mine#ga_mine.home,
	H;
get_mine_target(Mine,defender) ->
	{_,H} = Mine#ga_mine.home,
	H.

delete_mine_player(Type,Value,#ga_mine{attackerList=AttackerList,defenderList=DefenderList} = MineInfo) ->
	case Type of
		attacker ->	MineInfo#ga_mine{attackerList=lists:delete(Value,AttackerList)};
		defender ->	MineInfo#ga_mine{defenderList=lists:delete(Value,DefenderList)}
	end.
delete_mine_player_waiting(Type,Value,#ga_mine{attackerListW=AttackerList,defenderListW=DefenderList}=MineInfo)->
	case Type of
		attacker -> MineInfo#ga_mine{attackerListW=lists:delete(Value,AttackerList)};
		defender -> MineInfo#ga_mine{defenderListW=lists:delete(Value,DefenderList)}
	end.
add_mine_player_waiting(#ga_mine{attackerListW=AttackerList,defenderListW=DefenderList}=Mine,RoleID,ServerID,Type) ->
	case Type of
		attacker -> Mine#ga_mine{attackerListW=add2list({RoleID,ServerID},AttackerList)};
		defender ->	Mine#ga_mine{defenderListW=add2list({RoleID,ServerID},DefenderList)}
	end.
check_mine_player_waiting(Type,Value,#ga_mine{attackerListW=AttackerList,defenderListW=DefenderList}=_MineInfo) ->
	case Type of
		attacker ->lists:member(Value,AttackerList);
		defender -> lists:member(Value,DefenderList)
	end.
add_mine_player(#ga_mine{attackerList=AttackerList,defenderList=DefenderList}=Mine,RoleID,ServerID,Type) ->
	case Type of
		attacker ->	Mine#ga_mine{attackerList=add2list({RoleID,ServerID},AttackerList)};
		defender ->	Mine#ga_mine{defenderList=add2list({RoleID,ServerID},DefenderList)}
	end.
check_mine_player(Type,Value,#ga_mine{attackerList=AttackerList,defenderList=DefenderList}=_MinInfo) ->
	case Type of
		attacker -> lists:member(Value,AttackerList);
		defender ->	lists:member(Value,DefenderList)
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

update_player(#ga_player{roleID=RoleID,serverID=ServerID}=Player,Pos,MineID)->
	Player2 = Player#ga_player{tarMineID=MineID,startPos=Pos,startTime=util:now()},
	add_mov_plan(RoleID,ServerID),
%	add_mov_plan(RoleID,Player2),
	Player2.

update_player_fighters(Player,Blood,NewRL2,NewFightersA2)->
	Player#ga_player{blood=Blood,fighters=NewFightersA2,replayList=NewRL2}.

reborn_player(#ga_player{type=Type,baseFighters=BF}=Player) ->
	Pos = data_galactica:get({born_pos, Type}),
	RebornInterval = data_galactica:get(reborn_interval),
	Player#ga_player{startPos=Pos,startTime=0,tarMineID=0,fighters=BF,blood=100,rebornSec=util:now()+RebornInterval}.

reborn_player_now(#ga_player{type=Type,baseFighters=BF}=Player) ->
	Pos = data_galactica:get({born_pos, Type}),
	Player#ga_player{startPos=Pos,startTime=0,tarMineID=0,fighters=BF,blood=100,rebornSec=0}.

%  更新player的起止位置到当前移动位置
reset_player(#ga_player{startPos=Pos0,tarMineID=TarMineID,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	case TarMineID of
		0 ->
			Player;
		_ ->
			Now=util:now(),
			TimeDiff = Now-ST,
			MovDis = Speed * TimeDiff,
			Mine=get_mine(TarMineID),
			Pos1 = Mine#ga_mine.pos,
			NowPos = calc_new_pos(Pos0,Pos1,MovDis),
			Player#ga_player{startPos=NowPos,startTime=Now,movDis=MovDis0+MovDis}
	end.

%% %  更新player的起始位置到当前移动位置
%% reset_player2(#ga_player{startPos=Pos0,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
%% 	Now=util:now(),
%% 	TimeDiff = Now-ST,
%% 	MovDis = Speed * TimeDiff,
%% 	NowPos = calc_new_pos(Pos0,Pos1,MovDis),
%% 	Player#ga_player{startPos=NowPos,startTime=Now,movDis=MovDis0+MovDis}.

% 更新player到目标终点
reset_player3(#ga_player{startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	Now=util:now(),
	MovDis=Speed*(Now-ST),
	Player#ga_player{startTime=0,movDis=MovDis0+MovDis,startPos={0,0}}.

% 更新player到目标终点
reset_player4(#ga_player{tarMineID=TarMineID,startTime=ST,speed=Speed,movDis=MovDis0}=Player) ->
	Now=util:now(),
	MovDis=Speed*(Now-ST),
	Mine = get_mine(TarMineID),
	NowPos = Mine#ga_mine.pos,%calc_new_pos(Pos0,Pos1,MovDis),
	Player#ga_player{startPos=NowPos,startTime=0,movDis=MovDis0+MovDis,tarMineID=0}.

reset_player5([],_) ->
	ignore;
reset_player5(L,Pos) ->
	lists:foreach(fun({RoleID,ServerID}) ->
						  Player = get_player(RoleID,ServerID),
						  NewPlayer = Player#ga_player{startPos=Pos,tarMineID=0,startTime=0},
						  set_player(RoleID,ServerID,NewPlayer)
				  end, L).

reset_other_players(MineID,MinePos) ->
		List = [begin
			 Player = #ga_player{tarMineID=TarMineID,startPos=Pos,speed=Speed,startTime=SC}=get_player(RoleID,ServerID),
		 case TarMineID of
			 MineID ->
				 MVDis = Speed*(util:now()-SC),
				 NewPos = calc_new_pos(Pos,MinePos,MVDis),
				 set_player(RoleID,ServerID,Player#ga_player{startPos=NewPos,tarMineID=0,startTime=0}),
				 %%sync_role2(RoleID,ServerID);
				 {RoleID,ServerID};
			 _ ->
				 ignore
		 end
				end||{RoleID,ServerID}<-get_mov_plan()],
	sync_role2(List).

player_arrive(RoleID,ServerID) ->
	player_arrive(get_player(RoleID,ServerID)).
player_arrive(#ga_player{tarMineID=TarMineID,type=Type,roleID=RoleID,serverID=ServerID} = Player)->
	case TarMineID of
		0 ->
			ignore;
		MineID ->
			MineInfo = get_mine(MineID),
			cancel_mov_plan(RoleID,ServerID),
			case check_mine_can_arrive(MineInfo,Type) of
				true ->
					Player2 = reset_player3(Player),
					set_player(RoleID,ServerID,Player2),
					NewMine = add_mine_player(MineInfo,RoleID,ServerID,Type),
					NewMine2 = update_mine_mov(NewMine),
					mark_bc(MineID,{RoleID,ServerID}),
					set_mine(MineID,NewMine2);
				_ ->
					mark_bc([],{RoleID,ServerID}),
					Player2 = reset_player3(Player),%reset_player4(Player),
					NewMine = add_mine_player_waiting(MineInfo,RoleID,ServerID,Type),
					mark_bc(MineID,{RoleID,ServerID}),
					set_mine(MineID,NewMine),
					set_player(RoleID,ServerID,Player2)
			end
	end.

player_join(RoleID,ServerID,MineID)->
	MineInfo = get_mine(MineID),
	#ga_player{type=Type} = get_player(RoleID,ServerID),
	case check_mine_player_waiting(Type,{RoleID,ServerID},MineInfo) of
		true ->
			case check_mine_can_arrive(MineInfo,Type) of
				true ->
					MineInfo2 = delete_mine_player_waiting(Type,{RoleID,ServerID},MineInfo),
					NewMine = add_mine_player(MineInfo2,RoleID,ServerID,Type),
					NewMine2 = update_mine_mov(NewMine),
					mark_bc(MineID,{RoleID,ServerID}),
					set_mine(MineID,NewMine2),
					true;
				_->
					false
			end;
		_ ->
			false
	end.

update_players_pos()->
	CheckInterval = data_galactica:get(check_interval),
	Near = data_galactica:get(near_dis),
	[begin
		 Player = #ga_player{tarMineID=TarMineID,startPos=Pos,speed=Speed}=get_player(RoleID,ServerID),
		 case TarMineID of
			 0 ->
				 ignore;
			 _ ->
				 case Pos of
					 {0,0} ->
						 ignore;
					 _ ->
						 #ga_mine{pos=Pos1,home=OHome} = get_mine(TarMineID),
						 if is_integer(OHome) ->
								set_player(RoleID,ServerID,Player#ga_player{startTime=0,tarMineID=0});
							true ->
								MVDis = Speed * CheckInterval,
								NewPos = calc_new_pos(Pos,Pos1,MVDis),
								case check_dis_near(NewPos,Pos1,MVDis + Near) of
									true ->
										player_arrive(Player);
									_ ->
										%% mark_bc([],{RoleID,ServerID}),
										set_player(RoleID,ServerID,Player#ga_player{startPos=NewPos,startTime=util:now()})
								end
						 end
				 end
		 end
	 end||{RoleID,ServerID}<-get_mov_plan()].

check_mine_can_arrive(Mine=#ga_mine{maxNum=Max},Type)->
	Friends = get_friends(Type,Mine),
	case length(Friends) >= Max of
		true ->
			false;
		false ->
			true
	end.

update_mine_pos(MineID) ->
	#ga_mine{movTarget=HID,speed=Speed,pos=Pos0,startSec=ST}=Mine = get_mine(MineID),
	if ST == 0 ->
		   ignore;
	   true ->
		   #ga_home{pos=Pos1,mineIDs=MineIDs} =Home= get_home(HID),
		   case lists:member(MineID,MineIDs) of
			   false ->
				   Now=util:now(),
				   TimeDiff = Now-ST,
				   MovDis = Speed * TimeDiff,
				   NowPos = calc_new_pos(Pos0,Pos1,MovDis),
				   case check_mine_home(Pos0,NowPos,Pos1) of
					   true ->
						   NewMine=Mine#ga_mine{pos=NowPos,startSec=0,home=HID},
						   %set_mine(MineID,NewMine),
						   do_mine_home(NewMine,Home);
					   false ->
						   Sync_tick = case data_galactica:get(sync_tick) of
										 T when is_integer(T) -> if T > 3 -> T;true -> 3 end;
										 _ -> 3 
									 end,
						   case Now rem Sync_tick == 0 of
							   true-> mark_bc(MineID,[]);
							   _ -> ignore
						   end,
						   NewMine=Mine#ga_mine{pos=NowPos,startSec=Now},
						   set_mine(MineID,NewMine)
				   end;
			   true ->
				   ignore
		   end
	end.

do_mine_home(Mine,Home)->
	#ga_mine{id=MineID,attackerList=Attackers,defenderList=Defenders,attackerListW=AttackerListW
			 ,defenderListW=DefenderListW,gas=Gas,score=Score} = Mine,
	#ga_home{pos=Pos,mineIDs=MineIDs,type=Type} = Home,
	NewHome=Home#ga_home{mineIDs=[MineID|MineIDs]},
	NewMine=Mine#ga_mine{pos=Pos,attackerList=[],defenderList=[],attackerListW=[],defenderListW=[]},
	reset_player5(Attackers,Pos),
	reset_player5(Defenders,Pos),
	reset_player5(AttackerListW,Pos),
	reset_player5(DefenderListW,Pos),
	reset_other_players(MineID,Pos),
	set_mine(MineID,NewMine),
	set_home(NewHome),
	update_win_gas(Type,Gas,Score),
	bc_info(gas),
	bc_info(home).

update_win_gas(Type,Gas,Score) ->
	NowGas = get_gas(Type),
	if Score =< 0 ->
			ignore;
	   true ->
			lists:foreach(fun({_,#ga_player{roleID=RoleID,serverID=ServerID,type=TypeP,score=ScoreP}=GaP})->
								  if TypeP == defender andalso Type == 2 ->
										  set_player(RoleID,ServerID,GaP#ga_player{score=calc_player_gas(ScoreP,Score)});
									 TypeP == attacker andalso Type == 1 ->
										  set_player(RoleID,ServerID,GaP#ga_player{score=calc_player_gas(ScoreP,Score)});
									 true ->
										  ignore
								  end;
							 (_) ->
								  ignore
						  end, get())
	end,
	put({?gas,Type},Gas+NowGas).

calc_player_gas(ScoreP,Score)->
	Now = ScoreP + Score,
	{Min,Max} = data_galactica:get(score_limit),
	if Now < Min -> Min;
	   Now > Max -> Max;
	   true -> Now
	end.

get_gas(Type) ->
	case get({?gas,Type}) of
		X when is_integer(X) ->
			X;
		_ ->
			0
	end.	
 
check_mine_home(LastMinePos,NowMinePos,HomePos) ->
	{X1,Y1} = LastMinePos,
	{X2,Y2} = NowMinePos,
	{X3,Y3} = HomePos,
	L1 = (X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2),
	L2 = (X1-X3)*(X1-X3) + (Y1-Y3)*(Y1-Y3),
	Near = trunc(data_galactica:get(near_dis)),
	if L1 + Near >= L2 ->
		   true;
	   true ->
		   false
	end.
	

get_home(HID) ->
	get({?home,HID}).
	
set_home(Home=#ga_home{id=ID}) ->
	put({?home,ID},Home).

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


do_bc(talk,Type,Data,galactica_bc_info) ->
	BcList = get_bcList(Type),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,galactica_server,{galactica_bc_info,RoleID,Data})
				  end,BcList).

do_bc(Info,Type) ->
	BcList = get_bcList(),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,galactica_server,{Type,RoleID,Info})
						  end,BcList).
bc_info(open) ->
	BaseInfo = get_war_base_info(),
	do_bc(BaseInfo,galactica_bc_info);
bc_info(gas) ->
	Msg = #sc_galactica_gas{attackerGas=get_gas(1),defenderGas=get_gas(2)},
	do_bc(Msg,galactica_bc_info);
bc_info(home) ->
	Msg = #sc_galactica_home_s{homeInfo=get_home_info_s()},
	do_bc(Msg,galactica_bc_info);
bc_info(mark) ->
	put(?bc_mark,0),
	{ML,RL} = get_mark_bc(),
	MI = [mine2galacticaMineS(get_mine(E))||E<-ML],
	RI = [player2galaticaPlayerS(get_player(A,B))||{A,B}<-RL],
	Msg = #sc_galactica_update{player=RI,mine=MI},
	set_mark_bc({[],[]}),
	do_bc(Msg,galactica_bc_info).
bc_info(end_war,Type,Winner) ->
	Msg = #sc_galactica_end_war{type=Type, winner=type2Owner(Winner),data=get_rank_data()},
	do_bc(Msg,galactica_bc_info).

get_home_info_s()->
	IDA = [ID||{ID,_}<-data_galactica:get({home_list,attacker})],
	IDD = [ID||{ID,_}<-data_galactica:get({home_list,defender})],
	[begin
		 #ga_home{mineIDs=MineIDs}=get_home(ID),
		 #p_galactica_home_s{homeID=ID,mineIDs=MineIDs}
	 end||ID<-IDA++IDD].

get_war_base_info()->
	{Mines,Players,Homes} = 
		lists:foldl(fun({{?mine,_},Mine},{MineAcc,PlayerAcc,HomeAcc}) -> 
							{[mine2galacticaMine(Mine)|MineAcc],PlayerAcc,HomeAcc};
					   ({{?player,_},Player},{MineAcc,PlayerAcc,HomeAcc})->
							{MineAcc,[player2galacticaPlayer(Player)|PlayerAcc],HomeAcc};
					   ({{?home,_},Home},{MineAcc,PlayerAcc,HomeAcc})->
							{MineAcc,PlayerAcc,[home2galacticaHome(Home)|HomeAcc]};
					   (_,Acc) -> Acc
					end, {[],[],[]}, get()),
	{_,EndTime} = get(?endInfo),
	AttackerPos = data_galactica:get({born_pos, attacker}),
	DefenderPos = data_galactica:get({born_pos, defender}),
	#sc_galactica_war_base_info{result=1,endTimeStamp=EndTime,attackerPos=pos2galacticaPos(AttackerPos)
								,homes=Homes,defenderPos=pos2galacticaPos(DefenderPos),mines=Mines,players=Players
								,attackerGas=get_gas(1),defenderGas=get_gas(2)}.
		

pos2galacticaPos({X,Y}) ->
	#p_galactica_pos{x=X,y=Y}.
home2galacticaHome(Home) ->
	#p_galactica_home{homeID=Home#ga_home.id
					 ,type=Home#ga_home.type
					 ,mineIDs=Home#ga_home.mineIDs
					 ,pos=pos2galacticaPos(Home#ga_home.pos)}.
player2galacticaPlayer(Player) ->
	#p_galactica_player{roleID=Player#ga_player.roleID
					,serverID=Player#ga_player.serverID
					,startPos=pos2galacticaPos(Player#ga_player.startPos)
					,fly=Player#ga_player.fly
					,type=type2Owner(Player#ga_player.type)
					,mineID=Player#ga_player.tarMineID
					,blood=Player#ga_player.blood
					,rebornSec=Player#ga_player.rebornSec
					,name=Player#ga_player.roleName
					,startSec=Player#ga_player.startTime
					,fightPower=Player#ga_player.fight_power
					,roleLevel=Player#ga_player.level
                    ,speed=Player#ga_player.speed}.
player2galaticaPlayerS(Player) ->
	#p_galactica_player_s{roleID=Player#ga_player.roleID
						 ,serverID=Player#ga_player.serverID
						 ,mineID=Player#ga_player.tarMineID
						 ,blood=Player#ga_player.blood
						 ,startPos=pos2galacticaPos(Player#ga_player.startPos)
						 ,rebornSec = Player#ga_player.rebornSec
						 ,startSec=Player#ga_player.startTime}.

p2p_s(R,S) ->
	#p_galactica_p_s{roleID=R,serverID=S}.
mine2galacticaMine(Mine) ->
	#p_galactica_mine{mineID=Mine#ga_mine.id
					  ,pos=pos2galacticaPos(Mine#ga_mine.pos)
					  ,value=Mine#ga_mine.gas
					  ,maxNum = Mine#ga_mine.maxNum
					  ,movTarget = Mine#ga_mine.movTarget
					  ,attackers = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.attackerList]
					  ,defenders = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.defenderList]
					  ,attackersW = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.attackerListW]
					  ,defendersW = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.defenderListW]
					  ,startSec = Mine#ga_mine.startSec}.
mine2galacticaMineS(Mine)->
	#p_galactica_mine_s{mineID=Mine#ga_mine.id
						,pos = pos2galacticaPos(Mine#ga_mine.pos)
						,movTarget=Mine#ga_mine.movTarget
						,attackers = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.attackerList]
						,defenders = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.defenderList]
						,attackersW = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.attackerListW]
						,defendersW = [p2p_s(R,S)||{R,S}<-Mine#ga_mine.defenderListW]
						,startSec=Mine#ga_mine.startSec}.
fairy2galacticaFairy(FL) when is_list(FL) ->
	[fairy2galacticaFairy(F)||F<-FL];
fairy2galacticaFairy(#ger{gerBase=#gerBase{gerTypeID=TypeID,gerQuality=Rank}
					  ,gerAttr=#gerAttr{gerHpMax=HpMax},gerHp=Hp})->
	#p_galactica_fairy{typeID=TypeID
					   ,maxHp=HpMax
					   ,nowHp=Hp
					   ,rank=Rank}.
player2p_galactica_rank_dtl(Player)->
	#p_galactica_rank_dtl{roleID=Player#ga_player.roleID
						  ,serverID=Player#ga_player.serverID
						  ,level=Player#ga_player.level
						  ,score=Player#ga_player.score
						  ,kill=Player#ga_player.killNum
						  ,name=Player#ga_player.roleName
						 }.
player2p_galactica_rank_dtl2(Player)->
    AfkTime = get({?pd_operate_ts,Player#ga_player.roleID}),
    AfkLimit = util:now() - data_galactica:get(afk_check_time),
	#p_galactica_rank_dtl{roleID=Player#ga_player.roleID
						  ,serverID=Player#ga_player.serverID
						  ,level=Player#ga_player.level
						  ,score=Player#ga_player.score
						  ,kill=Player#ga_player.killNum
						  ,name=Player#ga_player.roleName
						  ,type = if AfkTime =:= ?undefined orelse AfkTime < AfkLimit -> 1; true -> 0 end
					  }.

sync_role({RoleID,ServerID},{TarRoleID,TarServerID})->
	P1 = player2galaticaPlayerS(get_player(RoleID,ServerID)),
	P2 = player2galaticaPlayerS(get_player(TarRoleID,TarServerID)),
	Msg = #sc_galactica_update{player=[P1,P2],mine=[]},
	galactica_router:send_client(ServerID,RoleID,Msg),
	galactica_router:send_client(TarServerID,TarRoleID,Msg),
	ok.

sync_role2(List) ->
	Msg = #sc_galactica_update{player=[player2galaticaPlayerS(get_player(RoleID,ServerID))||{RoleID,ServerID}<-List],mine=[]},
	do_bc(Msg,galactica_bc_info),
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

%% add_mine_gas_plan(MineID,Sec,RoleID,ServerID) ->
%% 	F = fun() -> erlang:send(self(), {mine_gas,MineID,RoleID,ServerID}) end,
%% 	add_plan(#plan{sec=Sec,key=MineID, value=F}).
%% 
%% cacel_mine_gas_plan(MineID) ->
%% 	delete_plan(MineID).

%% add_mov_plan(RoleID,#ga_player{serverID=ServerID,tarMineID=MineID,startPos=Start,endPos=End,speed=Speed}=_P)->
%% 	TimeDiff = calc_time(Start,End,Speed),
%% 	F = fun()->erlang:send(self(), {player_arrive, RoleID,ServerID,MineID}) end,
%% 	add_plan(#plan{sec=util:now()+TimeDiff, key={RoleID,ServerID},value=F}).
%% cancel_mov_plan(RoleID,ServerID) ->
%% 	delete_plan({RoleID,ServerID}).
add_mov_plan(RoleID,ServerID) ->
	set_mov_plan([{RoleID,ServerID}|lists :delete({RoleID,ServerID}, get_mov_plan())]).
cancel_mov_plan(RoleID,ServerID) ->
	set_mov_plan(lists :delete({RoleID,ServerID}, get_mov_plan())).

get_mov_plan()->
	case get(?mov_plan) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end.
set_mov_plan(List)->
	put(?mov_plan,List).


check_winner_plan()->
	F = fun()->
				erlang:send(self(), check_winner)
		end,
	Interval = data_galactica:get(check_interval),
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

set_pd_operate_ts(RoleID)->
    put({?pd_operate_ts,RoleID},util:now()).

