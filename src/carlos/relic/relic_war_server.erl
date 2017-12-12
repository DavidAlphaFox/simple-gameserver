%% @author crimoon-17
%% @doc @todo Add description to war_server.
%% 按照出生点,出生在下方的是attacker:0,出生在三方的是defender:1  
%% 

-module(relic_war_server).
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
%% -record(getGas,{startGetSec=0,gas=0,getspeed=1}).%% 开始时间,采集量
%% -record(mine, {id=0,pos={0,0},owner=0,attackerList=[],defenderList=[],beOwnSec=0,lastOwner=0
%% 			  ,gas=0,attackerGas=#getGas{},defenderGas=#getGas{},totalGas=0,ownRole=0}).%% owner用0和1表示占领者是attacker还是defender
-record(island_info, {id=0
                     ,island_state=0
                     ,nature_type=0
                     ,max_hp=0
                     ,cur_hp=0
                     ,pos={0,0}
                     ,attackerList=[]
                     ,boss_list=[]
                     ,player_id = {0,0}}).

-record(plan, {sec=0,key=0,value=0}).

-define(player, player).        %存放玩家数据的字典标志
-define(pd_island, pd_island).  %存放岛屿数据的字典标志
-define(plan,plan).             %存放定时器数据的字典标志
-define(bcList,bcList).         %存放广播对象id、server的字典标志
-define(endInfo, endInfo).      %存放战场计时结束的标志
-define(bc_mark,bc_mark).       %下一次计时器是否启动的字典标志
-define(mark_bc,mark_bc).       %记录下一次广播需要广播的内容
-define(talk_data,talk_data).
-define(rank_data, rank_data).
-define(boss_active_timeout,boss_active_timeout).   %boss激活时间的字典标志
-define(final_boss_type, final_boss_type).
-define(level_rank, level_rank).
-define(active_flag, active_flag). %如果有过移动或攻击行为则记录，其值为undefined的话，表示未移动，未攻击 v3.1.0版本不用了

-define(pd_operate_ts, pd_operate_ts).

-define(FinalIslandID, 6).
-define(Dump_interval, 60000).

%恢复战场
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).
%新建战场
start_link(WarID,Attackers,LevelRank) ->
	gen_server:start_link(?MODULE,[WarID,Attackers,LevelRank],[]).
start(Args) ->
    supervisor:start_child(relic_war_sup,Args).

%恢复战场
init([WarID]) ->
    ?INFO("relic_war_server init recover id:~w", [WarID]),
	Data = db_sql:get_relic_data(WarID),
	add_ets_war_info(WarID,self()),
	State2 = init_state(Data),
	init_player_mov(),
    {ok, State2};
%新建战场
init([WarID,Attacker,LevelRank]) ->
    ?INFO("relic_war_server init new id:~w~nattacker:~w~nlevel_rank:~w", [WarID,Attacker,LevelRank]),
    put(?level_rank,LevelRank),
	add_ets_war_info(WarID,self()),
    random:seed(util:gen_random_seed()), 
    NatureList = init_nature_type_list(),  %随机出岛屿属性
    RoleNatureList = init_players(Attacker,NatureList),
	{_,_,{GerLevel,GerQuality,_}}= data_relic:get({fight_level,LevelRank}),
    init_ground(GerLevel,GerQuality,RoleNatureList),
	init_bcList(Attacker),
	tick(),
	tick_plan(),
	bc_open(), %设置广播开关
	set_end_info(WarID),
	{ok,#state{warID=WarID,warStartTime=util:now()}}.

%新建战场，初始化广播ID字典
init_bcList(Players) ->
	RL=[{RoleID,ServerID}||#player{roleID=RoleID,serverID=ServerID}<-Players],
	put(?bcList, RL).

get_bcList()->
	case get(?bcList) of
		?undefined ->
			[];
		X ->
			X
	end.

%设置战斗结束时间
set_end_info(WarID) ->
	Interval = data_relic:get(war_interval)+10,
	put(?endInfo,{WarID,util:now()+Interval}),
	erlang:send_after(Interval * 1000, self(), do_end_war),
	ok.

%设置周期函数
tick_plan()->
	erlang:send_after(1000, self(), tick_plan).

%设定广播
bc_open()->
	erlang:send(self(), {bc_info, open}).

%保存记录进程id和pid对应关系的ets
add_ets_war_info(WarID,PID) -> 
	true = ets:insert_new(?ETS_RELIC_PROCESS_INFO,#ets_carlos_info{warID=WarID,pid=PID}).

%初始化进程state
init_state({#state{warStartTime=WST}=State,Dict}) ->
    lists:foreach(fun({K,V}) -> put(K,V) end, Dict),
	Now=util:now(),
	Interval = data_relic:get(war_interval),
	End = WST + Interval,
	if End >= Now ->
		   erlang:send(self(), do_end_war);
	   true ->
		   erlang:send_after((End - Now) * 1000, self(), do_end_war)
	end,
	State.

%初始化角色坐标，附带计算出boss的等级和品阶
init_players(Players,NatureList) ->
	{value,{_,Pos}} = lists:keysearch(?FinalIslandID,1, data_relic:get(island_pos_list)),
    {_,LvlTotal,_PFTotal,RoleNatureList} = 
        lists:foldr(fun(#player{roleID=RoleID,serverID=ServerID,level=L,fight_power=FP}=Info,{[H|AccNatureList],AccLvl,AccPower,AccRoleNature})->
                set_player(RoleID,ServerID,Info#player{startPos=Pos,endPos=Pos,type=H}),
                {AccNatureList,AccLvl+L,AccPower+FP,[{H,{RoleID,ServerID}}|AccRoleNature]}
            end, {util:random_list(NatureList),0,0,[]}, Players),
    RoleNatureList.

%初始化岛屿以及上面的boss
init_ground(GerLevel,GerQuality,RoleNatureList) ->
	IslandList = data_relic:get(island_pos_list),
    NatureList2 =  lists:reverse([{9,{0,0}}|RoleNatureList]), %第一个岛属性为0，无属性
    lists:foreach(fun({ID,Pos})->
            {NatureID0,{RoleID,ServerID}} = lists:nth(ID,NatureList2),
            {NatureID,BossTeamConfig} = if
                                 NatureID0 =:= 9 ->
                                     BigBossGerNum = random:uniform(data_relic:get(big_boss_all_num)),
                                     BigBossGerTypeID = data_relic:get({big_boss_type,BigBossGerNum}),
                                     put(?final_boss_type,BigBossGerTypeID),
                                     BigBossGerList = lists:filter(fun(BGerList)->
                                            lists:keymember(BigBossGerTypeID, 1, BGerList)
                                        end, data_relic:get({relic_boss,NatureID0})),
                                     {BigBossGerNum,lists:nth(1,BigBossGerList)};
                                 true->
                                     Player = get_player(RoleID,ServerID),
                                     set_player(RoleID,ServerID,Player#player{startPos=Pos,endPos=Pos,type=NatureID0}),
                                     {NatureID0,data_relic:get({relic_boss,NatureID0})}
                             end,    
            {MaxHp,GerList} = lists:foldl(fun({GerTypeID,_,_,GerPos},{AccHp,AccList2})-> 
                                            ?INFO("init_ground ger_attr-new_ger-----------~w ~w ~w",[GerTypeID, GerLevel, GerQuality]),
                                            Boss0 = ger_attr:new_ger(GerTypeID, GerLevel, GerQuality, [], []),
                                            Boss = Boss0#ger{gerBase=((Boss0#ger.gerBase)#gerBase{gerPos=GerPos})},
                                            {AccHp+Boss#ger.gerHp,[Boss|AccList2]}
                                        end , {0,[]}, BossTeamConfig),
            S = if
                    1 =< ID andalso ID =< 5 ->
                        ?RELIC_ISLAND_STATE_BOSS;
                    ID =:= ?FinalIslandID ->
                        ?RELIC_FINAL_ISLAND_STATE_REBORN
                end,
            put({?pd_island, ID},#island_info{id=ID,pos=Pos,nature_type=NatureID,island_state=S,max_hp=MaxHp,cur_hp=MaxHp,boss_list=GerList
                                             ,player_id = {RoleID,ServerID}})
        end, IslandList).

init_nature_type_list()->
    case random:uniform(2) of
         1 ->
             util:random_list([random:uniform(4)|lists:seq(1, 4)]);
         2 ->
             util:random_list([random:uniform(4)+4|lists:seq(5, 8)])
     end.

handle_call(test_get_info, _From, #state{warID=WarID}=State) -> 
    {reply, WarID, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(dump_interval_tick, State) ->
	?CATCH(do_interval_dump(State)),
	tick(),
	{noreply, State, hibernate};
handle_info(stop,State) ->
    erlang:send(relic_war_manager, {clear_in_battle,[RID||{player,{RID,_}}<-get_all_player_ids()]}),
	{stop,normal, State};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
	{noreply, State}.

terminate(Reason, State) ->
%% 	[reset_mine(IslandID)||{IslandID,_,_}<-data_relic:get(mine_list)],
	?CATCH(do_interval_dump(State)),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p"
		  ,[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_interval_dump(#state{warID=WarID}=State) ->
	db_sql:set_relic_data(WarID, {State,get()}).

tick() ->
	erlang:send_after(?Dump_interval, self(), dump_interval_tick).


do_handle_info({bc_info,Type}) ->
	bc_info(Type),
	ok;
do_handle_info(tick_plan) ->
	tick_plan(),
	plan(),
	ok;
do_handle_info(do_end_war) ->
	end_war(?RELIC_LOSE),
	ok;

do_handle_info(boss_active) ->
    ?INFO("boss_active--boss_active--boss_active-- 大boss激活",[]),
    IslandInfo = get_island(?FinalIslandID),
    set_island(?FinalIslandID,IslandInfo#island_info{island_state=?RELIC_FINAL_ISLAND_STATE_BOSS_ACTIVED}),
    mark_bc(?FinalIslandID,[]),
    ok;

%获得战场基本信息
do_handle_info({{cs_carlos_relic_war_base_info}, {_RoleID,_ServerID,RolePid}}) ->
    Msg = get_war_base_info(),
    send_client(RolePid,Msg),
    ok;
%获得岛屿细节界面信息
do_handle_info({{cs_carlos_relic_island_detail, IslandID}, {_RoleID,_ServerID,RolePid}}) ->
    Msg = to_msg_island_detail(get_island(IslandID)),
    send_client(RolePid,Msg),
    ok;
%获得自身信息
do_handle_info({{cs_carlos_relic_self}, {RoleID,ServerID,RolePid}}) ->
    case get_player(RoleID,ServerID) of
        ?undefined ->
            ?ERR("error, lose player, RolePid:~p, ServerID:~p~nAll player id list:~p~n", [RoleID, ServerID, get_all_player_ids()]),
            ignore;
        Player ->
            Msg = #sc_carlos_relic_self{self=player2carlosPlayer(Player)},
            send_client(RolePid,Msg) 
    end,
    ok;

do_handle_info({{cs_carlos_relic_mov, IslandID}, {RoleID,ServerID,RolePid}}) ->
    set_pd_operate_ts(RoleID),
    put({?active_flag,RoleID,ServerID},1),
    #player{tarMineID=TarIslandID,rebornSec=_RebornSec} = Player = get_player(RoleID,ServerID),
    #island_info{pos=Pos}= _MineInfo = get_island(IslandID),
    if 
        TarIslandID == 0 ->
            mark_bc([], {RoleID,ServerID});
        true ->
            MineInfo2 = delete_mine_player({RoleID,ServerID},get_island(TarIslandID)),
            set_island(TarIslandID,MineInfo2),
            mark_bc(TarIslandID, {RoleID,ServerID})
    end,
    Player2 = reset_player(Player),
    Player3 = update_player(Player2,Pos,IslandID),
    set_player(RoleID,ServerID,Player3),
    add_mov_plan(RoleID,Player3),
    Msg = #sc_carlos_relic_mov{result=1},
    send_client(RolePid,Msg),
    ok;
%到达boss处
do_handle_info({player_arrive, RoleID,ServerID,IslandID}) ->
    #player{tarMineID=TarMineID} = Player = get_player(RoleID,ServerID),
    ?INFO("player_arrive ~w ~w ~w",[RoleID,ServerID,IslandID]),
    case TarMineID of
        IslandID ->
            mark_bc(IslandID,{RoleID,ServerID}),
            MineInfo = get_island(IslandID),
            Player2 = reset_player3(Player),
            set_player(RoleID,ServerID,Player2),
            NewMine = add_island_player(MineInfo,RoleID,ServerID),
            set_island(IslandID,NewMine);
        _ ->
            ignore
    end;
do_handle_info({{cs_carlos_relic_attack, IslandID}, {RoleID,ServerID,RolePid}}) ->
    set_pd_operate_ts(RoleID),
	#player{fighters=FighterList,addBuff=LieuAdd,talent=TalentList,itemList=ItemList,trSpecial=TrSpecial
           ,type=AtkNatureType,baseFighters=BaseFighter,skin_info=SkinInfo} = Player = get_player(RoleID,ServerID),
    IslandInfo = get_island(IslandID),
    case check_island_player({RoleID,ServerID},IslandInfo) of
        true ->
            BossList = IslandInfo#island_info.boss_list,
            CurHp = IslandInfo#island_info.cur_hp,
            {AtkUp,DamageReduce}=
                if
                    IslandID =:= ?FinalIslandID ->
                        calc_actived_buff();
                    true ->
                        calc_nature_buff(AtkNatureType,IslandInfo#island_info.nature_type)
                end,
            ?INFO("Relic_buff ~w ~w",[AtkUp,DamageReduce]),
            GerEquipList = role_item:assort_ger_equiplist(ItemList),
            LegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
            case catch (do_fight(RoleID,BossList,CurHp,filter_out_zero_hp(FighterList),LieuAdd,TalentList,TrSpecial,AtkUp,DamageReduce,SkinInfo,LegendAddList)) of
                {_Result,BossNewCurHp,NewBossList,FightRecord0,NewFightList0} ->
                    put({?active_flag,RoleID,ServerID},1),
                    FighterList2 = role_data:get_FighterList_with_effect(ItemList,[],FightRecord0#sc_fight_request.fighterList),
                    FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
                    NewFighterList = recalc_fighter_list(FighterList, NewFightList0),
        %%             ?INFO("Player Ger List~nB:~w~nF:~w~nFightRecord:~w",[FighterList,NewFighterList,FightRecord]),
                    BloodPect = calc_hp_percent(NewFighterList,BaseFighter),
                    ?INFO("Player last BloodPect:~w Boss Hp:~w",[BloodPect,BossNewCurHp]),
                    %更新岛屿信息的残余血量和boss阵型
                    NewIslandInfo0 = IslandInfo#island_info{cur_hp=BossNewCurHp,boss_list=NewBossList},
                    %计算出玩家新的累积伤害
                    NewDamageScore = Player#player.score + max(0,IslandInfo#island_info.cur_hp-BossNewCurHp),
                    %如果小boss死了，需要变更状态
                    NewIslandInfo1 = if
                        BossNewCurHp =:= 0 andalso IslandID =/= ?FinalIslandID -> 
                            NewIslandInfo0#island_info{island_state = ?RELIC_ISLAND_STATE_UNACTIVE};
                        true ->
                            NewIslandInfo0
                    end,
                    %如果玩家死亡，需要重置玩家位置至复活点，并且从岛屿数据中，删除已经离开的该玩家
                    {NewIslandInfo2,NewPlayer} = if
                        BossNewCurHp > 0 orelse BloodPect =< 0 ->
                            %判定玩家死亡，玩家回到复活点
                            {delete_mine_player({RoleID,ServerID},NewIslandInfo1),relic_reborn_player_now(Player)};
                        true ->
                            %进攻者保持原地，只更新血量
                            {NewIslandInfo1,update_player_fighters(Player,BloodPect,NewFighterList)} 
                    end,
                    %保存岛屿和玩家信息
                    set_island(IslandID,NewIslandInfo2),
                    set_player(RoleID,ServerID,NewPlayer#player{score=NewDamageScore}),
                    sync_role({RoleID,ServerID}),
                    mark_bc(IslandID,{RoleID,ServerID}),
                    FightReward = data_relic:get(fight_reward),
                    send_msg:direct(ServerID,relic_server,{relic_reward,RoleID,?RELIC_REWARD_TYPE_FIGHT,FightReward}),
                    %检查发放何种奖励
                    if
                        NewIslandInfo2#island_info.cur_hp =< 0->
                            KillReward = data_relic:get(kill_reward),
                            send_msg:direct(ServerID,relic_server,{relic_reward,RoleID,?RELIC_REWARD_TYPE_KILL,KillReward}),
                            TotalReward = role_reward:reward_plus_reward(FightReward, KillReward),
                            P_RewardInfo = activity_server:sell_reward2p_reward_info(TotalReward),
                            IsEnd = if
                                        IslandID =:= ?FinalIslandID -> %大boss死亡
                                            1;
                                        true ->
                                            0
                                    end,
                            send_client(RolePid,#sc_carlos_relic_attack{result=1
                                                                       ,fightInfo=[FightRecord]
                                                                       ,new_islang_info=to_msg_relic_island(NewIslandInfo2)
                                                                       ,fight_reward=P_RewardInfo
                                                                       ,is_end=IsEnd});
                        true ->
                            P_RewardInfo = activity_server:sell_reward2p_reward_info(FightReward),
                            send_client(RolePid,#sc_carlos_relic_attack{result=2
                                                                       ,fightInfo=[FightRecord]
                                                                       ,new_islang_info=to_msg_relic_island(NewIslandInfo2)
                                                                       ,fight_reward=P_RewardInfo
                                                                       ,is_end=0})
                    end,   
                    %检查战斗是否结束
                    if
                        NewIslandInfo2#island_info.cur_hp =< 0 ->
                            if
                                IslandID =:= ?FinalIslandID -> %大boss死亡
                                    end_war(?RELIC_WIN);
                                true -> %大boss死亡
                                    IsAllDied = 
                                        lists:all(fun(Id)->
                                            Info = get_island(Id),
                                            Info#island_info.island_state =/= ?RELIC_ISLAND_STATE_BOSS
                                        end, lists:seq(1, 5)),
                                    if
                                        IsAllDied =:= true ->
                                            FinalIslandInfo = get_island(?FinalIslandID),
                                            set_island(?FinalIslandID,FinalIslandInfo#island_info{island_state=?RELIC_FINAL_ISLAND_STATE_BOSS_UNACTIVE}),
                                            mark_bc(?FinalIslandID,[]);
                                        true ->
                                            ignore
                                    end     
                            end;
                        true ->
                            ignore
                    end;
                Err ->
                    ?ERR("relic fight error ~w",[Err]),
                    set_island(IslandID,delete_mine_player({RoleID,ServerID},IslandInfo)),
                    set_player(RoleID,ServerID,relic_reborn_player_now(Player)),
                    sync_role({RoleID,ServerID}),
                    mark_bc(IslandID,{RoleID,ServerID}),
                    BlankReward = #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[],reputation=0,roleExp=0},
                    send_client(RolePid,#sc_carlos_relic_attack{result=3
                                                               ,fightInfo=[]
                                                               ,new_islang_info=to_msg_relic_island(IslandInfo)
                                                               ,fight_reward=BlankReward
                                                               ,is_end=0})
            end;
	   {fail,Reason} ->
            BlankReward = #p_reward_info{coin=0,gerExp=0,gerList=[],gold=0,itemList=[],reputation=0,roleExp=0},
            send_client(RolePid,#sc_carlos_relic_attack{result=Reason
                                                       ,fightInfo=[]
                                                       ,new_islang_info=to_msg_relic_island(IslandInfo)
                                                       ,fight_reward=BlankReward
                                                       ,is_end=0})
    end,
	ok;

do_handle_info({{cs_carlos_relic_active, IslandID},{RoleID,ServerID,RolePid}})->
    set_pd_operate_ts(RoleID),
    #player{type=RoleType} = get_player(RoleID,ServerID),
    IslandInfo = get_island(IslandID),
    IsAllDied = lists:all(fun(Id)->Info = get_island(Id),Info#island_info.island_state =/= ?RELIC_ISLAND_STATE_BOSS end, lists:seq(1, ?FinalIslandID-1)),
    #island_info{island_state=IslandState,nature_type=IslandType} = IslandInfo,

    if
        IsAllDied =:= false ->
            send_client(RolePid,#sc_carlos_relic_active{result=5,new_islang_info=to_msg_relic_island(IslandInfo)});
        IslandID =:= ?FinalIslandID ->
            send_client(RolePid,#sc_carlos_relic_active{result=2,new_islang_info=to_msg_relic_island(IslandInfo)});
        IslandState =:= ?RELIC_ISLAND_STATE_BOSS ->
            send_client(RolePid,#sc_carlos_relic_active{result=2,new_islang_info=to_msg_relic_island(IslandInfo)});
        IslandState =:= ?RELIC_ISLAND_STATE_ACTIVED ->
            send_client(RolePid,#sc_carlos_relic_active{result=3,new_islang_info=to_msg_relic_island(IslandInfo)});
        IslandType =/= RoleType ->
            send_client(RolePid,#sc_carlos_relic_active{result=6,new_islang_info=to_msg_relic_island(IslandInfo)});
        true ->
            case set_one_boss_active_timeout() of
                {ok,Timeout}->
                    reset_boss_active_plan(Timeout),
                    NewIslandState = IslandInfo#island_info{island_state=?RELIC_ISLAND_STATE_ACTIVED},
                    set_island(IslandID,NewIslandState),
                    mark_bc(IslandID,[]),
                    ?INFO("cs_carlos_relic_active --sss-is actived- IslandInfo ~w ~w",[IslandState, IslandType]),
                    send_client(RolePid,#sc_carlos_relic_active{result=1,new_islang_info=to_msg_relic_island(NewIslandState)});
                {fail,_}->
                    send_client(RolePid,#sc_carlos_relic_active{result=4,new_islang_info=to_msg_relic_island(IslandInfo)})
            end
    end,
    ok;
  
do_handle_info({{cs_carlos_relic_role_dtl, TarRoleID,TarServerID},{_RoleID,_ServerID,RolePid}})->
    #player{fighters=F,type=NatureID,grade=Grade} = get_player(TarRoleID,TarServerID),
    FL = fairy2carlosFairy(F),
    send_client(RolePid,#sc_carlos_relic_role_dtl{target=FL,naturetype=NatureID,grade=Grade}),
    ok;
do_handle_info({{cs_carlos_relic_mov_stop},{RoleID,ServerID,_RolePid}}) ->
    set_pd_operate_ts(RoleID),
    Player = get_player(RoleID,ServerID),
    #player{startPos=StartPos,tarMineID=TarMineID}=Player2 = reset_player(Player),
    case TarMineID of
        0 ->
            ignore;
        _ ->
            #island_info{pos=Pos} = get_island(TarMineID),
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
do_handle_info({{cs_carlos_relic_talk,Data}, {_RoleID,_ServerID,_RolePid}}) ->
    add_talk_data(attacker, Data),
    do_bc(talk,attacker,#sc_carlos_relic_talk{data=Data},carlos_relic_bc_info),
    ok;
do_handle_info({{cs_carlos_relic_get_talk}, {_RoleID,_ServerID,RolePid}}) ->
    TalkData = get_talk_data(attacker),
    send_client(RolePid,#sc_carlos_relic_get_talk{data=TalkData}),
    ok;

do_handle_info(test_win_war) ->
    end_war(?RELIC_WIN);
do_handle_info(test_show_info) ->
    io:format("----pd-start------~n~p",[get()]),
    io:format("~n--------pd-end------");
do_handle_info({test_show_info,pos}) ->
    io:format("----pd-start------~n",[]),
    lists:foreach(fun
            ({{?pd_island,IsLandID},E})->
                 GerIDList = [X#ger.gerBase#gerBase.gerTypeID||X<-E#island_info.boss_list],
                io:format("pd_island---->~w-~w ......~w~n",[IsLandID,E#island_info.id,{E#island_info.island_state
                                                                                      ,E#island_info.nature_type
                                                                                      ,E#island_info.max_hp
                                                                                      ,E#island_info.cur_hp
                                                                                      ,E#island_info.attackerList,GerIDList}]);
            (_)->
                 ignore
        end, get()),
    lists:foreach(fun
            ({{?player,{RoleID,_S}},E})->
                io:format("pd_island---~w----~w----~w--to--~w~n",[RoleID,{E#player.blood
                                                                         ,E#player.score
                                                                         ,E#player.tarMineID
                                                                         ,E#player.type
                                                                         ,E#player.level},E#player.startPos,E#player.endPos]);
            (_)->
                 ignore
        end, get()),
    io:format("--------pd-end------");
do_handle_info(Info) ->
	?ERR("error get info:~w",[Info]).

add_sys_msg(Type,RoleName, IslandID) ->
	ID = data_relic:get(Type),
	Msg = #p_carlos_talk{roleID=0,roleName=RoleName,data=ID,ext=IslandID},
	add_talk_data(attacker, Msg),
	add_talk_data(defender,Msg),
	do_bc(#sc_carlos_relic_talk{data=Msg},carlos_relic_bc_info),
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
	Len = data_relic:get(talk_len),
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
%% update_player_rank_data2(#player{roleID=RoleID,serverID=ServerID}=Player)->
%% 	Data = get_rank_data(), 
%% 	Data2 = [E||#p_carlos_rank_dtl{roleID=ERoleID,serverID=EServerID}=E<-Data,ERoleID/=RoleID orelse EServerID/=ServerID],
%% 	Data3 = [player2p_carlos_rank_dtl2(Player)|Data2],
%% 	set_rank_data(Data3).


stop() ->
	{WarID,_} = get(?endInfo),
	do_bc(WarID,update_carlos_war),
    ets:delete(?ETS_RELIC_PROCESS_INFO,WarID),
	erlang:send(self(), stop).

%战斗结束的处理逻辑
end_war(EndType)->
    {WarID,_EndTime} = get(?endInfo),
    ?INFO("end_war  WarID:~w",[WarID]),
    ets:delete(?ETS_RELIC_PROCESS_INFO,WarID),
    bc_info(end_war,EndType),
    erlang:send(self(), stop).

set_pd_operate_ts(RoleID)->
    put({?pd_operate_ts,RoleID},util:now()).

match_rank(SpendTime)->
    RankList = data_relic:get(fight_rank),
    match_rank(RankList,SpendTime).
match_rank([],_SpendTime)->
    erlang:length(data_relic:get(fight_rank));
match_rank([{{Min,Max},Rank}|OtherList],SpendTime)->
    if
        Max >= SpendTime andalso SpendTime >= Min ->
            Rank;
        true ->
            match_rank(OtherList,SpendTime)
    end.

%计算当前血量百分比
calc_hp_percent(FighterList,BaseFighter) when is_list(FighterList) andalso is_list(BaseFighter)->
    SumHP = lists:sum([HP1||#ger{gerHp=HP1}<-FighterList]),
    SumHPMax = lists:sum([HP2||#ger{gerHp=HP2}<-BaseFighter]),
%% 	{SumHP, SumHPMax} = lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
%% 											if HP < 0->
%% 												   {AccHP,AccHPMax+HPMax};
%% 											   true ->
%% 												   
%% 												   {AccHP + HP, AccHPMax + HPMax}
%% 											end
%% 									end, {0, 0}, FighterList),
	B = erlang:trunc((SumHP * 100) / SumHPMax ),
	if SumHP =< 0 ->
		   0;
	   true ->
		   if SumHP >= SumHPMax->
				  100;
			  true ->
				  B
		   end
	end;
calc_hp_percent(SumHP,SumHPMax) when is_number(SumHP) andalso is_number(SumHPMax)->
    erlang:trunc((SumHP * 100) / SumHPMax ).

calc_actived_buff()->
    lists:foldr(fun(IslandID,{AccAtk,AccDmgR})->
                        IslandInfo = get_island(IslandID),
                        if
                            IslandInfo#island_info.island_state =:= ?RELIC_ISLAND_STATE_ACTIVED ->
                                {AccAtk+40000,AccDmgR-1500}; %万分比
                            true ->
                                {AccAtk,AccDmgR}
                        end     
                end, {0,0}, lists:seq(1, ?FinalIslandID-1)). %不包括最后一个岛屿

%1）水->火->草->雷->水……
%2）飞行->格斗->超能->岩石->飞行……
calc_nature_buff(AtkNatureType0,BossNatureType0)->
    AtkNatureType1 = AtkNatureType0 rem 4,
    BossNatureType1 = BossNatureType0 rem 4,
    AtkNatureType2 = (AtkNatureType0+1) rem 4,
    BossNatureType2 = (BossNatureType0+1) rem 4,
    if
        AtkNatureType2 =:= BossNatureType1-> %攻击者优势
            {90000,-9000};
        AtkNatureType1 =:= BossNatureType2-> %Boss优势
            {-9000,90000};
        true ->
            {-2000,20000}
    end.

get_hp_dtl(FighterList)->
	% {SumHP, SumHPMax} = 
	lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
						{AccHP + HP, AccHPMax + HPMax}
				end, {0, 0}, FighterList).

filter_out_zero_hp(List) ->
    lists:filter(fun(#ger{gerHp=GerHP}) ->
                         GerHP > 0
                 end, List).

%更新宠物血量
recalc_fighter_list(FighterList, FighterList2) ->
    lists:foldr(fun(#ger{gerID=GerID}=Ger, Acc) ->
                        case lists:keyfind(GerID, #ger.gerID, FighterList2) of
                            false -> %返回列表中没有，说明死了
                                [Ger#ger{gerHp=0}|Acc];
                            #ger{gerHp=GerHp,gerProHp=GerProHP} ->
                                NewHp1 = min(GerHp,Ger#ger.gerAttr#gerAttr.gerHpMax),
                                NewHp2 = max(0,NewHp1),
                                [Ger#ger{gerHp=NewHp2,gerProHp=GerProHP}|Acc]
                        end
                end, [], FighterList).

add_replay(RL,RUID)->
	lists:sublist([RUID|RL], data_relic:get(max_replay_num)).

delete_mine_player(Value,#island_info{attackerList=AttackerList} = MineInfo) ->
        NewList = lists:delete(Value, AttackerList),
        MineInfo#island_info{attackerList=NewList}.
add_island_player(#island_info{attackerList=AttackerList}=IslandInfo,RoleID,ServerID) ->
	NewList = add2list({RoleID,ServerID},AttackerList),
    IslandInfo#island_info{attackerList=NewList}.
check_island_player(Value,#island_info{attackerList=AttackerList,island_state=IslandState}=_MinInfo) ->
	IsArrived = lists:member(Value,AttackerList),
    if
        IsArrived =:= false ->
            {fail,3};
        IslandState =/= ?RELIC_ISLAND_STATE_BOSS andalso IslandState =/= ?RELIC_FINAL_ISLAND_STATE_BOSS_ACTIVED ->
            {fail,4};
        true ->
            true
    end.

add2list(Value,List) ->
	case lists:member(Value, List) of
		true ->
			List;
		_ ->
			[Value|List]
	end.

update_player(#player{roleID=RoleID}=Player,Pos,IslandID)->
	Player2 = Player#player{tarMineID=IslandID,endPos=Pos,startTime=util:now()},
	add_mov_plan(RoleID,Player2),
	Player2.

%更新总血量及宠物
update_player_fighters(Player,Blood,NewFightersA2)->
	Player#player{blood=Blood,fighters=NewFightersA2}.

relic_reborn_player_now(#player{type=Type,baseFighters=BF}=Player) ->
    [Pos] = lists:foldl(fun(ID,AccRes)->
            IslandIndo = get({?pd_island, ID}),
            #island_info{player_id = {RoleID,ServerID}} = IslandIndo,
            if
                IslandIndo#island_info.nature_type =:= Type 
                  andalso Player#player.roleID =:= RoleID
                  andalso Player#player.serverID =:= ServerID ->
                    [IslandIndo#island_info.pos|AccRes];
                true ->
                    AccRes
            end
        end, [], lists:seq(1, ?FinalIslandID-1)), %不包括最后岛屿
    ?INFO("relic_reborn_player_now new pos~w",[Pos]),
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

get_all_player_ids() ->
    lists:foldl(fun({Key, _}, Acc) ->
                    case Key of
                        {?player, _} ->
                            [Key|Acc];
                        _ ->
                            Acc
                    end
                end, [], erlang:get()).

%更新排名信息，更新player数据，下面是两种不同的排名规则
set_player(RoleID,ServerID,Info) ->
    ?INFO("set_player ~w",[{RoleID,ServerID,Info}]),
	update_player_rank_data(Info),
	put({?player, {RoleID,ServerID}}, Info).
%% set_player2(RoleID,ServerID,Info) ->
%% 	update_player_rank_data2(Info),
%% 	put({?player, {RoleID,ServerID}}, Info).

get_island(IslandID) ->
	get({?pd_island, IslandID}).

set_island(IslandID,IslandInfo) ->
	put({?pd_island, IslandID}, IslandInfo).

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

%% do_bc(relic_reward,Type,RewardInfo)->
%%     BcList = get_bcList(),
%%     lists:foreach(fun({RoleID,ServerID})->
%%                           send_msg:direct(ServerID,relic_server,{relic_reward,RoleID,Type,RewardInfo})
%%                           end,BcList).

do_bc(talk,_Type,Data,carlos_relic_bc_info) ->
	BcList = get_bcList(),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,relic_server,{carlos_relic_bc_info,RoleID,Data})
						  end,BcList).

do_bc(Info,Type) ->
	BcList = get_bcList(),
	lists:foreach(fun({RoleID,ServerID})->
						  send_msg:direct(ServerID,relic_server,{Type,RoleID,Info})
						  end,BcList).
bc_info(open) ->
	BaseInfo = get_war_base_info(),
	do_bc(BaseInfo,carlos_relic_bc_info);
bc_info(mark) ->
	put(?bc_mark,0),
	{ML,RL} = get_mark_bc(),
	MI = [to_msg_relic_island(get_island(E))||E<-ML],
	RI = [player2carlosPlayer(get_player(A,B))||{A,B}<-RL],
    {AtkUp,DamageReduce}= calc_actived_buff(),
	Msg = #sc_carlos_relic_update{players=RI,islands=MI,boss_active_timeout=get_boss_active_timeout()
                                 ,atk_reinforce=erlang:abs(AtkUp) div 100,damage_reduce=erlang:abs(DamageReduce) div 100},
	set_mark_bc({[],[]}),
	do_bc(Msg,carlos_relic_bc_info).
bc_info(end_war,?RELIC_WIN) ->
    Now = util:now(),
    {TotalDamage,PlayerList} = 
        lists:foldl(fun
                       ({{?player,_},Player},{TotalDamageAcc,PlayerListAcc})-> 
                            {TotalDamageAcc+Player#player.score,[Player|PlayerListAcc]};
                       (_,Acc) -> Acc
                    end, {0,[]}, get()),
    DamageLimit = TotalDamage div 10, %10%
    ?INFO("end_war ----win---- ~w ~w",[TotalDamage,[{Player#player.roleID,Player#player.score}||Player<-PlayerList]]),
    WeakerList = [Player#player.roleID||Player<-PlayerList,Player#player.score < DamageLimit], %弱者，奖励减少
    OuterList = [{Player#player.roleID,Player#player.serverID}||Player<-PlayerList,Player#player.score =< 0], %挂机者，无奖励
    FinalGerTypeID = get(?final_boss_type),
    LevelRank = get(?level_rank),
    {_,_,{_GerLevel,_GerQuality,KeyStoneNum}}= data_relic:get({fight_level,LevelRank}),
    {_WarID,EndTime} = get(?endInfo),
    SpendTime0 = Now - (EndTime - data_relic:get(war_interval)),
    SpendTime = erlang:min(data_relic:get(war_interval), SpendTime0), %防止
    RewardRank = match_rank(SpendTime),
    BoxCostList = data_relic:get({relic_box_cost,LevelRank,RewardRank}),
    BoxNameList = data_relic:get({relic_box_name,RewardRank}),
    Others = 
        lists:foldl(fun({{?player,_},Player},OthersAcc)-> 
                            OtherInfo = #relic_role_other{roleID=Player#player.roleID
                                                         ,serverID=Player#player.serverID
                                                         ,fight_power=Player#player.fight_power
                                                         ,level=Player#player.level
                                                         ,isMale=Player#player.isMale
                                                         ,title=Player#player.title
                                                         ,head=Player#player.head
                                                         ,damage_score=Player#player.score},
                            [OtherInfo|OthersAcc];
                       (_,Acc) -> Acc
                    end, [], get()),
    Msg = #sc_carlos_relic_end_war{result=?RELIC_WIN,box_rank=RewardRank,box_name=BoxNameList,box_cost=BoxCostList,other_info=Others},
    AfkLimit = Now - data_relic:get(afk_check_time),
    AfkList = lists:foldl(fun(P,AccList)->
                                case get({?pd_operate_ts,P#player.roleID}) of
                                    ?undefined ->
                                        [{P#player.roleID,P#player.serverID}|AccList];
                                    Ts when Ts < AfkLimit -> %% 挂机
                                        [{P#player.roleID,P#player.serverID}|AccList];
                                    _ ->
                                        AccList
                                end 
                            end, [], PlayerList),
    do_bc({notice_relic_war_end,?RELIC_WIN,Msg,Others,[player2carlosPlayer(P)||P<-PlayerList],AfkList},carlos_relic_bc_info),
    KeyStoneTypeList = lists:usort([erlang:element(#island_info.nature_type, get_island(E))||E<-lists:seq(1, 5)]),
    ?INFO("KeyStoneTypeList:~w",[KeyStoneTypeList]),
    ?INFO("WeakerList:~w",[WeakerList]),
%%     do_bc(relic_reward,?RELIC_REWARD_TYPE_WIN,{SpendTime,RewardRank,{KeyStoneTypeList,KeyStoneNum},LevelRank,FinalGerTypeID,WeakerList});
    RewardInfo = {SpendTime,RewardRank,{KeyStoneTypeList,KeyStoneNum},LevelRank,FinalGerTypeID,WeakerList},
    BcList = get_bcList(),
    lists:foreach(fun({RoleID,ServerID})->
            case lists:member({RoleID,ServerID}, AfkList) of
                true ->
                    send_msg:direct(ServerID,relic_server,{relic_reward,RoleID,?RELIC_REWARD_TYPE_WIN_OUTER,[]});
                false ->
                    send_msg:direct(ServerID,relic_server,{relic_reward,RoleID,?RELIC_REWARD_TYPE_WIN,RewardInfo})
            end
        end,BcList);
bc_info(end_war,?RELIC_LOSE) ->
    Now = util:now(),
    {_Islands,Players,Others} = 
        lists:foldl(fun({{?pd_island,_},IslandInfo},{IslandAcc,PlayerAcc,OthersAcc}) -> 
                            {[island_info_to_msg_format(IslandInfo)|IslandAcc],PlayerAcc,OthersAcc};
                       ({{?player,_},Player},{IslandAcc,PlayerAcc,OthersAcc})-> 
                            OtherInfo = #relic_role_other{roleID=Player#player.roleID
                                                         ,serverID=Player#player.serverID
                                                         ,fight_power=Player#player.fight_power
                                                         ,level=Player#player.level
                                                         ,isMale=Player#player.isMale
                                                         ,title=Player#player.title
                                                         ,head=Player#player.head
                                                         ,damage_score=Player#player.score},
                            {IslandAcc,[player2carlosPlayer(Player)|PlayerAcc],[OtherInfo|OthersAcc]};
                       (_,Acc) -> Acc
                    end, {[],[],[]}, get()),
	Msg = #sc_carlos_relic_end_war{result=?RELIC_LOSE,box_rank=0,box_name=[],box_cost=[],other_info=Others},
    AfkLimit = Now - data_relic:get(afk_check_time),
    AfkList = lists:foldl(fun(P,AccList)->
                                case get({?pd_operate_ts,P#p_carlos_player.roleID}) of
                                    ?undefined ->
                                        [{P#p_carlos_player.roleID,P#p_carlos_player.serverID}|AccList];
                                    Ts when Ts < AfkLimit -> %% 挂机
                                        [{P#p_carlos_player.roleID,P#p_carlos_player.serverID}|AccList];
                                    _ ->
                                        AccList
                                end 
                            end, [], Players),
	do_bc({notice_relic_war_end,?RELIC_LOSE,Msg,Others,Players,AfkList},carlos_relic_bc_info).

send_client(RolePid,Msg)->
    erlang:send(RolePid, {relic_to_client,Msg}).

get_war_base_info()->
	{Islands,Players,Others} = 
		lists:foldl(fun({{?pd_island,_},IslandInfo},{IslandAcc,PlayerAcc,OthersAcc}) -> 
                            {[island_info_to_msg_format(IslandInfo)|IslandAcc],PlayerAcc,OthersAcc};
					   ({{?player,_},Player},{IslandAcc,PlayerAcc,OthersAcc})-> 
                            OtherInfo = #relic_role_other{roleID=Player#player.roleID
                                                         ,serverID=Player#player.serverID
                                                         ,fight_power=Player#player.fight_power
                                                         ,level=Player#player.level
                                                         ,isMale=Player#player.isMale
                                                         ,title=Player#player.title
                                                         ,head=Player#player.head
                                                         ,damage_score=Player#player.score},
                            {IslandAcc,[player2carlosPlayer(Player)|PlayerAcc],[OtherInfo|OthersAcc]};
					   (_,Acc) -> Acc
					end, {[],[],[]}, get()),
	{_,EndTime} = get(?endInfo),
    {AtkUp,DamageReduce}= calc_actived_buff(),
	#sc_carlos_relic_war_base_info{result=1,endTimeStamp=EndTime
                                  ,islandes=Islands,players=Players
                                  ,other_info=Others
                                  ,boss_active_timeout_max=data_relic:get(boss_active_timeout_init)
                                  ,atk_reinforce= erlang:abs(AtkUp) div 100
                                  ,damage_reduce= erlang:abs(DamageReduce) div 100}.

pos2carlosPos({X,Y}) ->
	#p_carlos_pos{x=X,y=Y}.
player2carlosPlayer(Player) ->
	?INFO("Player:~w ~n",[Player]),
	#p_carlos_player{roleID=Player#player.roleID
					,serverID=Player#player.serverID
					,startPos=pos2carlosPos(Player#player.startPos)
					,endPos=pos2carlosPos(Player#player.endPos)
					,fly=Player#player.fly
					,type=Player#player.type
					,mineID=Player#player.tarMineID
					,blood=Player#player.blood
					%,replayList=Player#player.replayList
					 ,rebornSec=Player#player.rebornSec
					 ,name=Player#player.roleName
					 ,startSec=Player#player.startTime
					 ,fightPower=Player#player.fight_power
					 ,roleLevel=Player#player.level
                    ,speed=Player#player.speed}.
island_info_to_msg_format(IslandInfo) when erlang:is_record(IslandInfo, island_info) ->
    {island_info,Id,State,Type,MaxHp,CurHp,Pos,_,_,_}=IslandInfo,
    {relic_island,Id,State,Type,MaxHp,CurHp,pos2carlosPos(Pos)}.

to_msg_relic_island(IslandInfo) ->
	#relic_island{island_id=IslandInfo#island_info.id
				  ,island_state=IslandInfo#island_info.island_state
                  ,nature_type=IslandInfo#island_info.nature_type
                  ,max_hp=IslandInfo#island_info.max_hp
				  ,cur_hp=IslandInfo#island_info.cur_hp
                  ,boss_pos=pos2carlosPos(IslandInfo#island_info.pos)}.
to_msg_island_detail(#island_info{attackerList=Attackers}=IslandInfo) ->
	#sc_carlos_relic_island_detail{island=to_msg_relic_island(IslandInfo)
						        ,attackers=[player2carlosPlayer(get_player(R,S))||{R,S}<-Attackers]}.
fairy2carlosFairy(FL) when is_list(FL) ->
	FL2 = lists:keysort(1, [{Pos,G}||#ger{gerBase=#gerBase{gerPos=Pos}}=G<-FL]),
	[fairy2carlosFairy(F)||{_,F}<-FL2];
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

sync_role({RoleID,ServerID})->
	P1 = player2carlosPlayer(get_player(RoleID,ServerID)),
    {AtkUp,DamageReduce}= calc_actived_buff(),
	Msg = #sc_carlos_relic_war_update{players=[P1]
                                     ,atk_reinforce= erlang:abs(AtkUp) div 100
                                     ,damage_reduce= erlang:abs(DamageReduce) div 100},
	carlos_router:send_client(ServerID,RoleID,Msg),
	ok.

mark_bc(IslandInfo,Role) ->
	{ML,RL} = get_mark_bc(),
	Info = {add_bc_list(IslandInfo,ML),add_bc_list(Role,RL)},
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

add_mov_plan(RoleID,#player{serverID=ServerID,tarMineID=IslandID,startPos=Start,endPos=End,speed=Speed}=_P)->
	TimeDiff = calc_time(Start,End,Speed),
	F = fun()->erlang:send(self(), {player_arrive, RoleID,ServerID,IslandID}) end,
	add_plan(#plan{sec=util:now()+TimeDiff, key={RoleID,ServerID},value=F}).

cancel_mov_plan(RoleID,ServerID) ->
	delete_plan({RoleID,ServerID}).

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

reset_boss_active_plan(TimePoint)->
    ?INFO("reset_boss_active_plan ~w",[TimePoint]),
    delete_plan(boss_active_plan),
    F = fun()->erlang:send(self(), boss_active) end,
    add_plan(#plan{sec=TimePoint, key=boss_active_plan,value=F}).

get_boss_active_timeout()->
    case get(?boss_active_timeout) of
        ?undefined ->-1;
        X ->X
    end.

%有boss死亡的时候调用
set_one_boss_active_timeout()->
    case get(?boss_active_timeout) of
        ?undefined -> %之前一个都没有激活
            TimeoutInit = util:now()+data_relic:get(boss_active_timeout_init),
            put(?boss_active_timeout,TimeoutInit),
            {ok,TimeoutInit};
        X ->
            Now = util:now(),
            if
                Now < X -> %还没到达激活截止时间
                    NewTimeout = X-data_relic:get(boss_active_time_reduce), %NewTimeout此刻可能小于now
                    put(?boss_active_timeout,NewTimeout),
                    {ok,NewTimeout};
                true -> %已经错过了激活时间，不能激活了
                    ?INFO("set_one_boss_active_timeout fail ~w ~w",[Now , X]),
                    {fail,X}
            end
    end.

%注意DamageRedeuce是负，-1000表示伤害减少10%
do_fight(RoleID, BossList, BossCurHp, FighterList,LieuAdd0,TalentList,TrSpecial,AtkUp,DamageRedeuce,SkinInfo,LegendAddList) ->
    % {AtkAdd, HpAdd} = LieuAdd0,
    %%v4.1版本传入的lieuAdd是add_attr结构，
    #add_attr{gerAttackAddtion=AtkAdd} = LieuAdd0,
    % LieuAdd = {((10000+AtkAdd)*(10000+AtkUp) div 10000)-10000, HpAdd},
    NewAttackAdd = ((10000+AtkAdd)*(10000+AtkUp) div 10000)-10000,
    LieuAdd = LieuAdd0#add_attr{gerAttackAddtion=NewAttackAdd},
    %% 发生战斗
    {Result, FightRecord, {_, NewBossStateList,NewFightList,_}} = role_fight:new(RoleID,FighterList, BossList,LieuAdd,{DamageRedeuce, 0},TalentList,[],TrSpecial,#trSpecial{},false,SkinInfo,#skin_info{},LegendAddList,[]),
    ?INFO("do_fight finish NewBossStateList:(~w)~w",[Result,NewBossStateList]),
    NewBossList = lists:foldl(fun(B,Acclist)-> 
                                      case lists:keysearch(B#ger.gerID, 1, NewBossStateList) of
                                          {value,{_,NewGerHp,_}} when NewGerHp > 0 ->
                                              [B#ger{gerHp = NewGerHp}|Acclist];
                                          _ ->
                                              Acclist
                                      end
                              end, [], BossList),
    %防止NewBossStateList中的Hp小于0
    NewCurHp =  erlang:min(lists:sum([erlang:max(0,Hp)||{_,Hp,_}<-NewBossStateList])
                          ,BossCurHp), 
    {Result,erlang:max(0,NewCurHp),NewBossList,FightRecord,NewFightList}.  
