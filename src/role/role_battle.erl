%% @author caohongyang
%% @doc 战役功能
%% Created 2013-3-5


-module(role_battle).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  
-include("def_mail.hrl").  
-include("def_fight.hrl").

-define(BATTLE_DUNGEON_TYPE_LIST,[]).
%-define(BATTLE_DUNGEON_TYPE_LIST,[?BATTLE_DUNGEON_TYPE_NORMAL
%                                 ,?BATTLE_DUNGEON_TYPE_HARD
%                                 ,?BATTLE_DUNGEON_TYPE_FAST_HARD
%                                 ,?BATTLE_DUNGEON_TYPE_TRANSMIGRATION]).
-define(DUNGEON_BOSS_REWARD_UNACCEPT_TYPE,1).
-define(DUNGEON_BOSS_REWARD_ACCEPTED_TYPE,2).
-define(DUNGEON_BOSS_REWARD_UNPASSED_TYPE,3).
-define(DUNGEON_BOSS_REWARD_NOBOSSBOX_TYPE,4).

-define(INVALID_BATTLE_DUNGEON_TYPE,5).
-define(INVALID_CHAPTER_TYPE,6).
-define(INVALID_DUNGEON_TYPE,7).
-define(CHAPTER_BOSS_REWARD_UNACCEPT,8).
-define(CHAPTER_BOSS_REWARD_ACCEPTED,9).
-define(BOSS_DUNGEON_NO_IN_FINISH_DUNGEON,10).
%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

cs_battle_world_level(_) ->
	MaxLevel = tk_id:exlv_get_maxLevel(),
	{N,Diff} = data_common:get(world_lv),
	?sendself(#sc_battle_world_level{maxLevel=MaxLevel,n=N,diff=Diff}).

bc_world_level(Level) ->
	{N,Diff} = data_common:get(world_lv),
	broadcast_server:bc(#sc_battle_world_level{maxLevel=Level,n=N,diff=Diff}).

%% @doc 处理请求：获取战役进度
cs_battle_progress(_) ->
	Fun = fun(Type)->
				  % CurDungeonID = get_progress(Type).
				  %%此处由于修改了最后关卡的下一个关卡ID，导致通过之后进度会大于所有的关卡，从而前端查找不到对应关卡而崩溃
				  CurDungeonID = case get_progress(Type) of
				  	31001->
				  		31000;
				  	X->
				  		X
				  end,
				  % #data_dungeon{chapterID=CurChapterID} = data_dungeon:get(CurDungeonID),
				  CurChapterID= caculate_chapterID_by_dungeonID(CurDungeonID),
				  #data_chapter{dungeonIDList=DungeonIDList} = data_chapter:get(CurChapterID),
				  Len = length(DungeonIDList),
				  #p_battle_progress{type=Type,dungeonID=CurDungeonID,chapterID=CurChapterID,dungeonCount=Len}
		  end,
	BPList = lists:map(Fun, ?BATTLE_DUNGEON_TYPE_LIST),
	BestPassChapterIDList = db_sql:get_bestPassChapterID(role_data:get_roleID()),
	StarRewardInfoList = [#p_battle_chapter_star_reward{chapterID=ChapterID,totalScore=Num
                                                       ,rewardStatus=StarRewarded,hasbossreward=case get_chapter_status(ChapterID,chapterid2battletype(ChapterID)) of ?CHAPTER_BOSS_REWARD_UNACCEPT->4;?BOSS_DUNGEON_NO_IN_FINISH_DUNGEON->6;_->5 end}
						 ||[_RoleID,ChapterID,StarRewarded,Num]<-db_sql:get_chapter_star_reward_info(role_data:get_roleID())],
	?sendself(#sc_battle_progress{bpList=BPList,bestPassChapterID=BestPassChapterIDList,chapterRewardList=StarRewardInfoList}).

%% @doc 处理请求：获取章节信息
cs_battle_info(#cs_battle_info{chapterID=ChapterID0,type=Type}) ->
	case lists:member(Type, ?BATTLE_DUNGEON_TYPE_LIST) of
		true->
			ChapterID = 
				if
					ChapterID0 =:= 0 ->
						(data_dungeon:get(get_progress(Type)))#data_dungeon.chapterID;
					true ->
						ChapterID0
				end,
			RoleID = role_data:get_roleID(),
			case data_chapter:get(ChapterID) of
				?undefined ->
					ignore;
				_ ->			
					Chapter = get_chapter(RoleID, ChapterID),
					#chapter{dungeonList=DungeonInfo, perfectRewarded= PerfectRewarded,starRewarded=StarRewarded}= Chapter,
					#data_chapter{dungeonIDList=DungeonIDList} = data_chapter:get(ChapterID),
				 	 Len = length(DungeonIDList),
					?sendself(#sc_battle_info{type=Type,chapterID=ChapterID,perfectRewarded=PerfectRewarded
											 ,dungeonInfo=DungeonInfo,dungeonCount=Len,starRewarded=StarRewarded})
			end;
		false->
			ignore
	end.

cs_battle_star_reward(#cs_battle_star_reward{chapterID=ChapterID,rewardID=RewardID}) ->
	case check_get_star_reward(ChapterID,RewardID) of
		{true,Chapter,Reward,Role} ->
			do_get_star_reward(Role,Chapter,Reward,ChapterID,RewardID);
		{false,Reason} ->
			?sendself(#sc_battle_star_reward{result=Reason,starRewarded=0,reward=[]})
	end.

cs_battle_reset_dungeon(#cs_battle_reset_dungeon{type=Type,dungeonID=DungeonID})->
	case check_can_reset_dungeon(Type,DungeonID) of
		{true, Role,NeedGold,Chapter,Dungeon} ->
			do_reset_dungeon(Role,NeedGold,Chapter,Dungeon,Type,DungeonID);
		{false,Reason} ->
			?sendself(#sc_battle_reset_dungeon{result=Reason,restTimes=0})
	end.

%% @doc 处理请求：挑战关卡
%% cs_battle_challenge_hard(#cs_battle_challenge_hard{dungeonID=DungeonID})->
%% 	case check_challenge_hard(DungeonID) of
%% 		{true, Progress, DataDungeon, RoleTimes, Chapter, Dungeon, DataChapter} ->
%% 			case do_fight(DataDungeon) of
%% 				{true, FightRecord, Score, RoleInfo} ->
%% 					do_fight_win(Progress, DataDungeon, RoleTimes, Chapter, Dungeon, Score, FightRecord, DataChapter, RoleInfo);
%% 				{false, FightRecord, _Score, _RoleInfo} ->
%% 					do_fight_fail(DataDungeon, RoleTimes, Chapter, Dungeon, FightRecord)
%% 			end;
%% 		{false, Reason} ->
%% 			?sendself(#sc_battle_challenge{result=Reason,score=0})
%% 	end.
cs_battle_challenge(#cs_battle_challenge{dungeonID=DungeonID,type=Type})->
	case check_challenge(DungeonID,Type) of
		{true, Progress, DataDungeon, RoleTimes, Chapter, Dungeon, DataChapter,NeedAssist} ->
			case do_fight(DataDungeon,NeedAssist) of
				{true, FightRecord, Score, RoleInfo} ->
					do_fight_win(Progress, Type,DataDungeon, RoleTimes, Chapter, Dungeon, Score, FightRecord, DataChapter, RoleInfo);
				{false, FightRecord, _Score, RoleInfo} ->
					do_fight_fail(DataDungeon, RoleTimes, Chapter, Dungeon, FightRecord, RoleInfo)
			end;
		{false, Reason} ->
			?sendself(#sc_battle_challenge{result=Reason,score=0});
		{false, Reason, Cost,RestResetTimes} ->
			?sendself(#sc_battle_battle_fail{result=Reason,cost=Cost,restResetTimes=RestResetTimes})
	end.

cs_battle_dungeon_raids(#cs_battle_dungeon_raids{dungeonID=DungeonID})->
	case check_raids(DungeonID) of
		{true, Energy, RestTimes,RoleTimes,Dungeon,Chapter,RoleInfo,DataDungeon}->
			do_raids(Energy, RestTimes, RoleTimes, Dungeon,Chapter,RoleInfo,DataDungeon);
		{false, Reason}->
			?sendself(#sc_battle_dungeon_raids{result=Reason, raidsTimes=0, reward=[]});
		{false, Reason, Cost,RestResetTimes} ->
			?sendself(#sc_battle_battle_fail{result=Reason,cost=Cost,restResetTimes=RestResetTimes})
	end.

cs_battle_obtain_boss_reward(#cs_battle_obtain_boss_reward{type=Type,chapterID=ChapterID,dungeonID=DungeonID})->
	case check_obtain_boss_reward(Type,ChapterID,DungeonID) of
		{false,Reason}->
			?sendself(#sc_battle_obtain_boss_reward{result=Reason,type=Type,chapterID=ChapterID,dungeonID=DungeonID,reward=[]});
		{true,Reward}->
			PRewardInfoList = do_obtain_boss_reward(Type,DungeonID,Reward),
			?sendself(#sc_battle_obtain_boss_reward{result=1,type=Type,chapterID=ChapterID,dungeonID=DungeonID,reward=PRewardInfoList})
	end.

cs_battle_get_boss_reward_info(#cs_battle_get_boss_reward_info{type=Type,chapterid=ChapterID,dungeonid=DungeonID})->
	{_Result,Reason} = case get_dungeon_status2(ChapterID,DungeonID,Type) of
		?INVALID_BATTLE_DUNGEON_TYPE->
			{false,1};
		?INVALID_CHAPTER_TYPE->
			{false,2};
		?INVALID_DUNGEON_TYPE->
			{false,3};
		?DUNGEON_BOSS_REWARD_UNACCEPT_TYPE->
			{true,4};
		?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE->
			{true,5};
		?DUNGEON_BOSS_REWARD_UNPASSED_TYPE->
			{true,6};
		?DUNGEON_BOSS_REWARD_NOBOSSBOX_TYPE->
			{true,7}
	end,
	?sendself(#sc_battle_get_boss_reward_info{result=Reason,type=Type,chapterid=ChapterID,dungeonid=DungeonID}).

cs_battle_dojang_info(_)->
    DojangIdList = data_dojang:get(dojang_list),
    DojangInfo = role_data:get_dojang_data(),
    DojangList = 
        lists:foldr(fun(DojangId,AccList)->
                        case check_dojang_unlock(DojangId) of
                            false ->
                                [#p_dojang_info{index = DojangId
                                              ,state = 1}|AccList];
                            true ->
                                case lists:member(DojangId, DojangInfo#dojang_info.pass_id_list) of
                                    true ->
                                        [#p_dojang_info{index = DojangId
                                                      ,state = 3}|AccList];
                                    false ->
                                        [#p_dojang_info{index = DojangId
                                                      ,state = 2}|AccList]
                                end     
                        end
            end, [], DojangIdList),
    ?sendself(#sc_battle_dojang_info{fight_time = DojangInfo#dojang_info.harvest_free_time + DojangInfo#dojang_info.harvest_pay_time
                                    ,already_buy_time = erlang:length(data_dojang:get(buy_cost)) - DojangInfo#dojang_info.buy_time
                                    ,dojang_list = DojangList
									,can_buy_time = DojangInfo#dojang_info.buy_time}).

cs_battle_dojang_fight(#cs_battle_dojang_fight{index=DojangId})->
    case check_dojang_unlock(DojangId) of
        false ->
            ?sendself(#sc_battle_dojang_fight{result = 2,fightInfo1= []
											 ,dojang_info = #p_dojang_info{index = DojangId
                                                                          ,state = 1}
										     ,rewards = []
										     ,fightInfo2 = []});
        true ->
            DojangInfo = role_data:get_dojang_data(),
            #role{level=RoleLevel,roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
       		case lists:member(DojangId, DojangInfo#dojang_info.pass_id_list) of
            	false ->
		            {MonTrSpe,MonSkinInfo,MonTalentList,MonTeamConfig} = data_dojang:get({dojang_enemy,DojangId}),
					MonTeam = get_dojang_enemy(MonTeamConfig),
		            RoleFighterList = role_data:get_fighter_list(),
		            RoleLieuAdd = role_data:get_lieu_add_attr(),
		            TalentList = role_talent:get_active_talent_list(),
		            SkinInfo = role_skin:get_skin_info(),
		            LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
                    TrSpe = role_data:get_trSpecial(),
		            {Result, FightRecord, {FinalState,_,_,_}} = role_fight:new(RoleID,RoleFighterList, MonTeam
																			  ,RoleLieuAdd,#add_attr{}
																			  ,TalentList,MonTalentList
																			  ,TrSpe,MonTrSpe,false
																			  ,SkinInfo,MonSkinInfo
																			  ,LegendAddList,[]),
		            if Result =:= false ->     
				            ?sendself(#sc_battle_dojang_fight{result = 1
				            								 %%此处由于配置的训练师等级没有超过转生阶段，但是又要显示转生的皮肤，故需要对训练师的p_fighter的gerQuality进行修改以便前端显示转生的皮肤
				                                             ,fightInfo1= [change_fighter_transmigration(FightRecord,MonSkinInfo)]
															 ,dojang_info = #p_dojang_info{index = DojangId
				                                                      ,state = 2}
															 ,rewards = []});
		                true ->
		                    Rewards0 = get_dojang_reward(DojangId),
		                    role_reward:handle_sell_reward_f(role_data:get_roleInfo(), Rewards0, ?MONEY_ADD_TYPE_DOJANG, DojangId, ""),
		                    case lists:member(DojangId, DojangInfo#dojang_info.pass_id_list) of
		                        true ->
		                            ignore;
		                        false ->
		                            NewPassIdList = [DojangId|DojangInfo#dojang_info.pass_id_list],
		                            role_data:set_dojang_data(DojangInfo#dojang_info{pass_id_list=NewPassIdList})
		                    end, 
				            ?sendself(#sc_battle_dojang_fight{result = 1
				            								%%此处由于配置的训练师等级没有超过转生阶段，但是又要显示转生的皮肤，故需要对训练师的p_fighter的gerQuality进行修改以便前端显示转生的皮肤
				                                             ,fightInfo1= [change_fighter_transmigration(FightRecord,MonSkinInfo)]
															 ,dojang_info = #p_dojang_info{index = DojangId
				                                                      ,state = 3}
															 ,rewards = [activity_server:sell_reward2p_reward_info(Rewards0)]})
		            end;
				true ->
		            ?sendself(#sc_battle_dojang_fight{result = 3,fightInfo1= []
													 ,dojang_info = #p_dojang_info{index = DojangId
		                                                                          ,state = 3}
												     ,rewards = []
													 ,fightInfo2 = []})
			end
    end.

cs_battle_dojang_harvest(#cs_battle_dojang_harvest{index=DojangId})->
    DojangInfo = role_data:get_dojang_data(),
    case lists:member(DojangId, DojangInfo#dojang_info.pass_id_list) of
        true ->
            if
                0 < DojangInfo#dojang_info.harvest_free_time + DojangInfo#dojang_info.harvest_pay_time ->
                    Rewards0 = get_dojang_reward(DojangId),
                    role_reward:handle_sell_reward_f(role_data:get_roleInfo(), Rewards0, ?MONEY_ADD_TYPE_DOJANG, DojangId, ""),
					if
						DojangInfo#dojang_info.harvest_free_time > 0 ->
                    		role_data:set_dojang_data(DojangInfo#dojang_info{harvest_free_time= DojangInfo#dojang_info.harvest_free_time - 1});
						true ->
                    		role_data:set_dojang_data(DojangInfo#dojang_info{harvest_pay_time= DojangInfo#dojang_info.harvest_pay_time - 1})
					end,
                    ?sendself(#sc_battle_dojang_harvest{result = 1 ,rewards= [activity_server:sell_reward2p_reward_info(Rewards0)]});
                true ->
                    ?sendself(#sc_battle_dojang_harvest{result = 3 ,rewards= []})
            end; 
        false ->
            ?sendself(#sc_battle_dojang_harvest{result = 2 ,rewards= []})
    end.

cs_battle_dojang_buy(#cs_battle_dojang_buy{buy_time = WantBuyTime}) ->
    Role = role_data:get_roleInfo(),
    DojangInfo = role_data:get_dojang_data(),
	case check_buy_time(DojangInfo,WantBuyTime) of
		true ->
		    NeedGold = get_dojang_cost(DojangInfo#dojang_info.buy_time,WantBuyTime),
		    case role_lib:check_money(Role, gold, NeedGold) of
		        true ->
                    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_BUY_DOJANG, 0, ""),
					NewDojangInfo = 
						DojangInfo#dojang_info{harvest_pay_time= DojangInfo#dojang_info.harvest_pay_time + WantBuyTime
                                       		  ,buy_time= DojangInfo#dojang_info.buy_time - WantBuyTime},
                    role_data:set_dojang_data(NewDojangInfo),
                    ?sendself(#sc_battle_dojang_buy{result = 1
                                           ,new_fight_time= NewDojangInfo#dojang_info.harvest_pay_time + NewDojangInfo#dojang_info.harvest_free_time
										   ,already_buy_time = erlang:length(data_dojang:get(buy_cost)) - NewDojangInfo#dojang_info.buy_time
									       ,can_buy_time = NewDojangInfo#dojang_info.buy_time});
		        false ->
		            ?sendself(#sc_battle_dojang_buy{result = 2
		                                           ,new_fight_time= DojangInfo#dojang_info.harvest_pay_time + DojangInfo#dojang_info.harvest_free_time
												   ,already_buy_time = erlang:length(data_dojang:get(buy_cost)) - DojangInfo#dojang_info.buy_time
											       ,can_buy_time = DojangInfo#dojang_info.buy_time})
		    end;
		false ->
            ?sendself(#sc_battle_dojang_buy{result = 3
                                           ,new_fight_time= DojangInfo#dojang_info.harvest_pay_time + DojangInfo#dojang_info.harvest_free_time
										   ,already_buy_time = erlang:length(data_dojang:get(buy_cost)) - DojangInfo#dojang_info.buy_time
									       ,can_buy_time = DojangInfo#dojang_info.buy_time})
	end.

%% double_fight(SkinInfo,TalentList,TrSpe,RoleFighterList,RoleLieuAdd)->
%% 	SelfTeam = #double_team{skin_info=SkinInfo,talent=TalentList,trainer=TrSpe,fighters=RoleFighterList,lieu=RoleLieuAdd},
%% 	{IsWin,FightRecord,BLeader2,BFellow2,NowBlood,DD} = double_fight(BLeader=#double_team{fighters=LeaderFighters},BFellow=#double_team{fighters=FellowFighters},SelfTeam,Blood,DeadList)
%% 
%% double_fight2(BLeader,BFellow,SelfTeam) -> 
%% 	Ref = erlang:make_ref(),
%% 	double_fight:new(SelfTeam,#double_team{},BLeader,BFellow,self(),Ref,false),
%% 	receive
%% 		{fight_result,Ref,FightResult} ->
%% 			{IsWin,FightRecord,{_SrcGerStateList,_DGerStateList,_SrcGerList,_DGerList,_DA,_DD}} = FightResult,
%% 			{IsWin,FightRecord}
%% 	    after 10000 ->
%% 			fail
%% 	end.

check_buy_time(DojangInfo,WantBuyTime) when WantBuyTime > 0->
	WantBuyTime =< DojangInfo#dojang_info.buy_time;
check_buy_time(DojangInfo,WantBuyTime)->
	false.
  
%% 算出多次购买需要的价格
get_dojang_cost(BuyTime,WantBuyTime)->
	get_dojang_cost(BuyTime,WantBuyTime,0).
get_dojang_cost(_BuyTime,0,Acc)->
	Acc;
get_dojang_cost(BuyTime,WantBuyTime,Acc)->
	get_dojang_cost(BuyTime-1,WantBuyTime-1,Acc + get_dojang_cost2(BuyTime)).
get_dojang_cost2(BuyTime)->
	if
	   BuyTime > 0 ->
		   lists:nth(BuyTime, lists:reverse(data_dojang:get(buy_cost)));
	   true ->
		   0
	end.

check_dojang_unlock(DojangId) ->
	#role{level=RoleLevel} = role_data:get_roleInfo(),
	Unlock = data_dojang:get(unlock_level),
	if
		RoleLevel >= Unlock ->
		    NeedChapterID = data_dojang:get({dojang_limit,DojangId}),
            #role_xbattle{chapterID=NowChapterID} = role_data:get_xbattle_data(),
            if NeedChapterID > NowChapterID -> false;
               true -> true
            end;
%% 		            #data_chapter{dungeonIDList=DungeonIDList} = data_chapter:get(NeedChapterID),
%% 					FinalDungeonID = lists:max(DungeonIDList),
%% 					case get_dungeon_status2(NeedChapterID,FinalDungeonID,?BATTLE_DUNGEON_TYPE_NORMAL) of
%% 						?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE ->
%% 							true;
%% 						?DUNGEON_BOSS_REWARD_UNACCEPT_TYPE ->
%% 							true;
%% 						_ ->
%% 							false
%% 					end;
		true ->
			false
	end.

get_dojang_enemy(ConfigList) ->
    lists:foldl(fun({GerTypeID,GerLevel, GerQuality,GerPos},AccList2)-> 
                Boss0 = ger_attr:new_mon(GerTypeID, GerLevel, GerQuality, [], []),
                Boss = Boss0#ger{gerBase=((Boss0#ger.gerBase)#gerBase{gerPos=GerPos})},
                [Boss|AccList2]
            end , [], ConfigList).

get_dojang_reward(DojangId)->
    get_dojang_reward(data_dojang:get({dojang_reward,DojangId}),{sell_reward,0,0,0,0,[],0,[]}).

get_dojang_reward([],AccList)->
    AccList;
get_dojang_reward([{Rate,Reward}|T],AccList)->
    Rand = util:random_int(1, 10000),
    if
        Rand =< Rate ->
            NewAccList = role_reward:reward_plus_reward(Reward,AccList),
            get_dojang_reward(T,NewAccList);
        true ->
            get_dojang_reward(T,AccList)
    end.

check_obtain_boss_reward(Type,ChapterID,DungeonID)->
	case lists:member(Type,[?BATTLE_DUNGEON_TYPE_NORMAL,?BATTLE_DUNGEON_TYPE_TRANSMIGRATION]) of
		false->
			{false,2};
		true->
			case data_battle_boss_reward:get({data_battle,ChapterID}) of
				?undefined->
					{false,3};
				DungeonList->
					case lists:keyfind(DungeonID,1,DungeonList) of
						false->
							{false,4};
						{DungeonID,RewardData}->
							case get_dungeon_status2(ChapterID,DungeonID,Type) of
								?DUNGEON_BOSS_REWARD_UNACCEPT_TYPE->
									{true,get_battle_boss_reward(RewardData)};
								_ ->
									{false,5}
							end
					end
			end
	end.

get_battle_boss_reward(RewardData) when is_record(RewardData,sell_reward)->
	RewardData;
get_battle_boss_reward(RewardData) when is_list(RewardData)->
	MainGerTypeID = role_data:get_main_gerTypeID(),
	case lists:keyfind(MainGerTypeID,1,RewardData) of
		false->
			?ERR("Can not find reward with main_gerTypeID:~w in RewardData:~w ~n",[MainGerTypeID,RewardData]),
			#sell_reward{};
		{_GerTypeID,Reward}->
			Reward
	end.
do_obtain_boss_reward(Type,DungeonID,Reward)->
	Role=role_data:get_roleInfo(),
	role_reward:handle_sys_reward_with_return(Role,Reward,?MONEY_ADD_TYPE_BATTLE_BOSS_REWARD,DungeonID,""),
    PRewardInfo = activity_server:sell_reward2p_reward_info(Reward),
    PRewardInfoList = role_recycle:transformPRewardInfo2PRewardInfoList(PRewardInfo,[]),
    update_battle_boss_reward_info2(Type,DungeonID,?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE,false),
    PRewardInfoList.

do_raids(Energy, RestTimes, RoleTimes, Dungeon,Chapter,RoleInfo,DataDungeon)->
	RaidsTimes = erlang:min(erlang:min(Energy, RestTimes),30),
	{_RoleTimes2,RoleInfo2} = role_lib:deduct_energy_f(RoleInfo,RoleTimes, RaidsTimes),
	Dungeon2 = Dungeon#p_dungeon{restTimes=Dungeon#p_dungeon.restTimes-RaidsTimes},
	#data_dungeon{dungeon_level=DungeonLevel,reward=Reward,dungeonID=DungeonID,chapterID=ChapterID,sufDungeonID=SufDungeonID} = DataDungeon,
	{RoleInfo3,GerAddExpList,RewardItemList,RewardGerList,LevelExp} = role_reward:handle_dungeon_reward_f2(RoleInfo2, RaidsTimes,Reward, ?MONEY_ADD_TYPE_BATTLE, DungeonID, ""),
	RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
	RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
	AddCoin = lists:max([RoleInfo3#role.coin - RoleInfo2#role.coin,0]),
	AddExp = lists:max([RoleInfo3#role.exp - RoleInfo2#role.exp,0]),
	Reward2 = role_lib:reward2p_reward(Reward#reward{coin = AddCoin, roleExp = AddExp}),
	Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView,levelExp=LevelExp},
	set_dungeon(Chapter, Dungeon2),
	#role{level=RoleLevel,roleID=RoleID} = RoleInfo,
	MonTeam = get_mon_team(DataDungeon, RoleLevel, DungeonLevel),
	
	DDeadList1 = lists:foldl(fun(G,Acc)->
									 role_fight:add_born_list(?b(G,gerTypeID),Acc)
							 end, [], MonTeam),
	DDeadList = [{T,N*RaidsTimes}||{T,N}<-DDeadList1],
	case DDeadList of
		[]->
			ignore;
		_->
			?CATCH_1(role_task_trigger:handle({dispach_task,kill_monster,DDeadList}),E1)
	end,
	case data_dungeon:get(SufDungeonID) of
		?undefined -> ignore;
		#data_dungeon{chapterID=SufChapterID} ->
			if ChapterID == SufChapterID -> ignore;
			   true -> catch(role_task_trigger:handle({dispach_task,chapter_pass,RoleID,ChapterID,1}))
			end
	end,
	
	?CATCH_1(role_task_trigger:handle({dispach_task,dungeon_pass,RoleID,DungeonID,1,RaidsTimes}),E2),
	behavior_dungen_fight:log( RoleID, DungeonID,1,2,RaidsTimes),
	?sendself(#sc_battle_dungeon_raids{result=1, raidsTimes=RaidsTimes, reward=[Reward3]}).

check_raids(DungeonID)->
	#role{roleID=RoleID,level=Level,vipLevel=VipLevel} =RoleInfo= role_data:get_roleInfo(),
	#data_dungeon{chapterID=ChapterID} = DataDungeon = data_dungeon:get(DungeonID),
	Chapter = get_chapter(RoleID, ChapterID),
	DungeonList = Chapter#chapter.dungeonList,
	
	case Level < data_dungeon_raids:get(need_level) of
		true ->
			{false, 6};
		_ ->
			case VipLevel < data_dungeon_raids:get(need_vipLevel) of
				true ->
					{false, 7};
				_ ->
					case lists:keyfind(DungeonID, #p_dungeon.dungeonID, DungeonList) of
						false ->
							{false, 5};
						#p_dungeon{bestScore=BestScore, restTimes=RestTimes,resetTimes=ResetTimes}=Dungeon->
							case BestScore =:= ?MAX_DUNGEON_SCORE of
								false ->
									{false, 4};
								true ->
									case RestTimes =:= 0 of
										true ->
											#data_vip{dungeonResetTimes=MaxDungeonResetTimes}=data_vip:get(VipLevel),
											if ResetTimes >= MaxDungeonResetTimes ->
											   		{false,3,0,0};
											   true ->
												   NewTimes = ResetTimes + 1,
												   NeedGold = lists:nth(NewTimes,data_common:get(buy_dungeon_reset_gold)),
												   {false, 2,NeedGold,MaxDungeonResetTimes-ResetTimes}
											end;
										_ ->
	
											#roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
											case Energy =:= 0 of
												true ->
													{false,2};
												_ ->
%% 													Progress = 
%% 														case DungeonID > data_battle_setting:get(hard_battle_start_dungeonID) of
%% 															true ->
%% 																get_progress();
%% 															_ ->
%% 																get_progress_hard()
%% 														end,
%% 													if Progress < DungeonID->
%% 														   {false, 5};
%% 													   true ->
														   {true, Energy, RestTimes,RoleTimes,Dungeon,Chapter,RoleInfo,DataDungeon}
%%													end
											end
									end
							end
					end
			end
	end.



-define(test_reward_view, ([#p_item_view{itemLevel=1,itemNum=2,itemRank=1,itemTypeID=11001},
							#p_item_view{itemLevel=1,itemNum=2,itemRank=1,itemTypeID=11002}])).
%% 赢了发奖励、扣体力、次数，打分。
%% Score是星星数
do_fight_win(Progress,Type, DataDungeon, RoleTimes, Chapter, Dungeon, Score, FightRecord, DataChapter, RoleInfo0) ->
    ?INFO("do_fight_win ~w",[{Progress,Type, DataDungeon, RoleTimes, Chapter, Dungeon, Score, FightRecord, DataChapter, RoleInfo0}]),
	{_,RoleInfo} = role_lib:deduct_energy_f(RoleInfo0,RoleTimes, DataDungeon#data_dungeon.costEnergy),
	Dungeon2 = Dungeon#p_dungeon{restTimes=Dungeon#p_dungeon.restTimes-1,bestScore=erlang:max(Score,Dungeon#p_dungeon.bestScore)},
	#data_dungeon{reward=Reward,chapterID=ChapterID,sufDungeonID=SufDungeonID}=DataDungeon,
	#role{roleID=RoleID} = RoleInfo,
	DungeonID = DataDungeon#data_dungeon.dungeonID,
    %% handle_dungeon_reward_f会根据?MONEY_ADD_TYPE_BATTLE，在后面判断是否需要检查奖励翻倍活动
	{NewRoleInfo, GerAddExpList, RewardItemList, RewardGerList,LevelExp} 
		= role_reward:handle_dungeon_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_BATTLE, DungeonID, ""),
	RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
	RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
	%RewardView = ?test_reward_view,
	update_battle_boss_reward_info(Type,Progress,DataDungeon#data_dungeon.dungeonID),
	Chapter2 = set_dungeon(Chapter, Dungeon2),
	
	%% 如果本关卡第一次达到3星，则判断是否完美通关，并加入到完美通关列表中
	if Dungeon#p_dungeon.bestScore =/= 3 andalso Score =:= 3 ->
		   case check_perfect(Chapter2, DataChapter) of
			   true ->
				   %ChapterID = DataChapter#data_chapter.chapterID,
				   List =  db_sql:get_bestPassChapterID(RoleID),
				   case lists:member(ChapterID, List) of
					   true->
						   ignore;
					   false->
				   			db_sql:add_bestPassChapterID(RoleID, ChapterID)
				   end;
			   {false,_} ->
				   ignore
		   end;
	   true ->
		   ignore
	end,
	Progress2 = 
		case SufDungeonID of
			0 ->
				#data_chapter{sufChapterID=SufChapterID}=DataChapter,
				if SufChapterID =:= 0 ->
					   	case Progress=:=31000 of
					   		true->
					   			catch(role_task_trigger:handle({dispach_task,chapter_pass,RoleID,1100,Type}));
					   		false->
					   			ignore
					   	end,
					   	Progress;
				   true ->
					   	case data_chapter:get(SufChapterID) of
						   #data_chapter{dungeonIDList=[NextDungeonID]} ->
							   NextDungeonID;
						   _ ->
							   Progress
					   	end
				end;
			_ ->
				#data_dungeon{chapterID=SufChapterID} = data_dungeon:get(SufDungeonID),
				if ChapterID == SufChapterID -> ignore;
				   true -> catch(role_task_trigger:handle({dispach_task,chapter_pass,RoleID,ChapterID,Type}))
				end,
				erlang:max(Progress, SufDungeonID)
		end,
	set_progress(Type,Progress2),
	Reward2 = role_lib:reward2p_reward(Reward),
    %% 由于可能奖励倍率增加，所以不能直接把关卡配置信息中的金币和经验返回，需要算一下实际差值。
    AddCoin = lists:max([NewRoleInfo#role.coin - RoleInfo#role.coin,0]),
    AddExp = lists:max([NewRoleInfo#role.exp - RoleInfo#role.exp,0]),
	Reward3=Reward2#p_reward{coin=AddCoin,roleExp=AddExp,gerExpList=GerAddExpList,itemList=RewardItemView,gerList=RewardGerView,levelExp=LevelExp},
	if RewardItemView =/= [] ->
		   RoleName = role_lib:get_name(role_data:get_roleID()),
		   lists:foreach(fun(X)->#p_item_view{itemTypeID=ItemTypeID, itemNum=ItemNum} = X,
								 #data_item{itemStar=ItemStar,itemType=ItemType} = data_item:get(ItemTypeID),
								 IsEquip = item_lib:is_itemType_equip(ItemType),
								 if	ItemStar >= 4 andalso IsEquip -> 
										broadcast_server:bc(#sc_battle_broadcast_get_item{roleName=RoleName,itemTypeID=ItemTypeID
																						 ,dungeonID=Dungeon#dungeon.dungeonID
																						 ,chapterID=Chapter#chapter.id,num=ItemNum});
									true	->
										ignore
								 end
						 end, RewardItemView);
	   true ->
		   ignore
	end,
    Type2 = case Type of
                1 ->
                    1;
                4 ->
                    1;
                _ ->
                    Type
            end,
	?CATCH(role_task_trigger:handle({dispach_task,dungeon_pass,RoleID,DungeonID,Type2,1})),
	behavior_dungen_fight:log( RoleID, DungeonID,1,1,0),
	?sendself(#sc_battle_challenge{result=1,fightInfo=[FightRecord],reward=[Reward3],score=Score}).

%% 输了，什么都不影响
do_fight_fail(DataDungeon, _Role, _Chapter, _Dungeon, FightRecord, RoleInfo) ->
	%Role2 = Role#role{energy=Role#role.energy-DataDungeon#data_dungeon.costEnergy},
	%Dungeon2 = Dungeon#p_dungeon{restTimes=Dungeon#p_dungeon.restTimes-1},
	%role_data:set_roleInfo(Role2),
	%set_dungeon(Chapter, Dungeon2),
	%Reward = DataDungeon#data_dungeon.reward,
    role_data:set_roleInfo(RoleInfo#role{isFailed=true}),
	DungeonID = DataDungeon#data_dungeon.dungeonID,
	#role{roleID=RoleID} = RoleInfo,
	behavior_dungen_fight:log( RoleID, DungeonID,0,1,0),
	?sendself(#sc_battle_challenge{result=1,fightInfo=[FightRecord],reward=[], score=0}).

get_mon_team(DataDungeon, RoleLevel, Dungeonlevel) when Dungeonlevel==0 orelse RoleLevel >= Dungeonlevel ->
	DataDungeon#data_dungeon.gerID1;
get_mon_team(DataDungeon, RoleLevel, Dungeonlevel) ->
	#data_dungeon{dungeonID=DungeonID} = DataDungeon,
	case ets:lookup(?ETS_DUNGEON_MON_CACHE, {DungeonID,RoleLevel}) of
		[] ->
			Team = get_mon_team2(DataDungeon, RoleLevel, Dungeonlevel),
			%% 在ets中缓存起来
			ets:insert(?ETS_DUNGEON_MON_CACHE, {{DungeonID,RoleLevel}, Team}),
			Team;
		[{_,Team}] ->
			Team			
	end.

get_mon_team2(DataDungeon, RoleLevel, Dungeonlevel) ->
	AddRank = 2*(Dungeonlevel - RoleLevel),
	#data_dungeon{gerID2=MonList,gerID3=IDList} = DataDungeon,
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, E#mon.gerQuality+AddRank, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- MonList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].



do_fight(#data_dungeon{dungeon_level=DungeonLevel,trainer=TrSpecialDungeon,dungeonID=DungeonID} = DataDungeon,NeedAssist) ->
	case NeedAssist of
		false->
			#role{level=RoleLevel,roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
			MonTeam = get_mon_team(DataDungeon, RoleLevel, DungeonLevel),
			RoleFighterList = role_data:get_fighter_list(),
			% RoleFighterList = reorganize_fighterlist_with_assist(RoleFighterList1,NeedAssist,DungeonID),
			RoleLieuAdd = role_data:get_lieu_add_attr(),
			%% ?DEBUG("L-do_fight RoleLieuAdd:~w",[RoleLieuAdd]),
    		TalentList = role_talent:get_active_talent_list(),
    		SkinInfo = role_skin:get_skin_info(),
    		LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
    		{Result, FightRecord, {FinalState,_,_,_}} = role_fight:new(RoleID,RoleFighterList, MonTeam, RoleLieuAdd,#add_attr{},TalentList,[],role_data:get_trSpecial(),TrSpecialDungeon,false,SkinInfo,#skin_info{},LegendAddList,[]),
			if Result =:= false ->
		   			Score = 0;
	   			true ->
		   			DeadNum = 
			   			lists:foldl(fun({_,Hp,_},Acc) ->
								   if Hp =< 0 ->
										  Acc+1;
									  true ->
										  Acc
								   end
						   end, 0, FinalState),
		   			if DeadNum >= 2 ->
				  			Score = 1;
			  			DeadNum =:= 1 ->
				  			Score = 2;
			  			true ->
				  			Score = 3
		   			end
			end,
			{Result, FightRecord, Score, RoleInfo};
		true->
			RoleInfo = role_data:get_roleInfo(),
			MainGerTypeID = role_data:get_main_gerTypeID(),
			Special = role_data:get_trSpecial(),
			FightRecord = get_special_battle_result(MainGerTypeID,DungeonID,Special),
			{true,FightRecord,3,RoleInfo}
	end.

is_pass(DungeonID)->
	RoleID = role_data:get_roleID(),
	Chapter = get_chapter_by_dungeonID(RoleID, DungeonID),
	#p_dungeon{bestScore=BestScore} = get_dungeon_by_chapter(Chapter, DungeonID),
	case BestScore of
		0->
			false;
		_->
			true 
	end.
	

check_challenge(DungeonID,Type) ->
	case lists:member(Type, ?BATTLE_DUNGEON_TYPE_LIST) of
		true->
			Progress = get_progress(Type),
			if Progress >= DungeonID->
				   #data_dungeon{chapterID=ChapterID} = DataDungeon = data_dungeon:get(DungeonID),
				   #role{roleID=RoleID, level=Level,vipLevel=VipLevel} = role_data:get_roleInfo(),
				   #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
				   #data_chapter{activeNeedLevel=ActiveNeedLevel} = DataChapter = data_chapter:get(ChapterID),
				   if 	Level < ActiveNeedLevel ->
						  	{false, 5};
					  	true ->
					  		case get_chapter_by_dungeonID(RoleID,DungeonID) of
					  			?undefined->
					  				{false,5};
					  			Chapter->
						  			if 
						  				Energy >= DataDungeon#data_dungeon.costEnergy ->
								 			#p_dungeon{resetTimes=ResetTimes,restTimes=RestTimes}= Dungeon = get_dungeon_by_chapter(Chapter, DungeonID),
								 			if 
								 				RestTimes >= 1 ->
                                        			%% DataDungeon包含奖励信息
													NeedAssist1 =case data_drama:get(is_special_always) of
														true->
															true;
														_ ->
															Progress=:=DungeonID
													end,
													NeedAssist = case NeedAssist1 of
																true->
																	MainGerTypeID = role_data:get_main_gerTypeID(),
																	#trSpecial{specialID=SpecialID} = role_data:get_trSpecial(),
																	case data_drama:get({special_dungeon,DungeonID,MainGerTypeID,SpecialID}) of
																		?undefined->
																			case data_drama:get({special_dungeon,DungeonID,0,SpecialID}) of
																				?undefined->
																					false;
																				_ ->
																					true
																			end;
																		_ ->
																			true
																	end;
																false->
																	false
																end,
													{true, Progress, DataDungeon, RoleTimes, Chapter, Dungeon, DataChapter,NeedAssist};
												true ->
													#data_vip{dungeonResetTimes=MaxDungeonResetTimes}=data_vip:get(VipLevel),
													if 
														ResetTimes >= MaxDungeonResetTimes ->
											   				{false,3,0,0};
										   				true ->
											   				NewTimes = ResetTimes + 1,
											   				NeedGold = lists:nth(NewTimes,data_common:get(buy_dungeon_reset_gold)),
											   				{false, 2,NeedGold,MaxDungeonResetTimes-ResetTimes}
													end
								 			end;
							 			true ->
								 			{false, 3}
						  			end
						  	end
				   end;
			   true ->
				   {false, 4}
			end;
		false->
			{false,5}
	end.

%% check_challenge_hard(DungeonID) ->
%% 	Progress = get_progress_hard(),
%% 	if Progress >= DungeonID->
%% 		   #data_dungeon{chapterID=ChapterID} = DataDungeon = data_dungeon:get(DungeonID),
%% 		   #role{roleID=RoleID, level=Level} = role_data:get_roleInfo(),
%% 		   #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
%% 		   #data_chapter{activeNeedLevel=ActiveNeedLevel} = DataChapter = data_chapter:get(ChapterID),
%% 		   if Level < ActiveNeedLevel ->
%% 				  {false, 5};
%% 			  true ->
%% 				  if Energy >= DataDungeon#data_dungeon.costEnergy ->
%% 						 Chapter = get_chapter_by_dungeonID(RoleID, DungeonID),
%% 						 Dungeon = get_dungeon_by_chapter(Chapter, DungeonID),
%% 						 if Dungeon#p_dungeon.restTimes >= 1 ->
%% 								{true, Progress, DataDungeon, RoleTimes, Chapter, Dungeon, DataChapter};
%% 							true ->
%% 								{false, 2}
%% 						 end;
%% 					 true ->
%% 						 {false, 3}
%% 				  end
%% 		   end;
%% 	   true ->
%% 		   {false, 4}
%% 	end.

%% @doc 领取完美通关奖励
cs_battle_perfect_reward(#cs_battle_perfect_reward{chapterID=_ChapterID}) ->
	?sendself(#sc_battle_perfect_reward{result=2}).
%% 	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
%% 	Chapter = get_chapter(RoleID, ChapterID),
%% 	DataChapter = data_chapter:get(ChapterID),
%% 	case check_perfect(Chapter, DataChapter) of
%% 		true ->
%% 			NewChapter = Chapter#chapter{perfectRewarded=true},
%% 			set_chapter(NewChapter),
%% 			?sendself(#sc_battle_perfect_reward{result=1}),
%%             role_reward:handle_sell_reward_f(Role, DataChapter#data_chapter.perfectReward, ?MONEY_ADD_TYPE_BATTLE_PERFECT, ChapterID, "");
%% %% 			role_reward:handle_dungeon_reward_f(Role, DataChapter#data_chapter.perfectReward, ?MONEY_ADD_TYPE_BATTLE_PERFECT, ChapterID, "");
%% 		{false,Reason} ->
%% 			
%% 			?sendself(#sc_battle_perfect_reward{result=Reason})
%% 	end.

check_perfect(Chapter, DataChapter) ->
	if Chapter#chapter.perfectRewarded =:= false ->
		   DungeonList = Chapter#chapter.dungeonList,
		   if length(DungeonList) =:= length(DataChapter#data_chapter.dungeonIDList) ->
				  case lists:all(fun(E) -> E#p_dungeon.bestScore =:= ?MAX_DUNGEON_SCORE end, DungeonList) of
					  true ->
						  true;
					  false ->
						  {false,3}
				  end;
			  true ->
				  {false, 3}
		   end;
	   true ->
		   {false,2}
	end.
do_reset_dungeon(Role,NeedGold,Chapter,Dungeon,_Type,DungeonID)->
	ResetTimes = Dungeon#p_dungeon.resetTimes,
	role_lib:deduct_gold_f(Role,NeedGold,?MONEY_DEC_TYPE_RESET_DUNGEON,DungeonID,integer_to_list(ResetTimes)),
	set_dungeon(Chapter,Dungeon),
	?sendself(#sc_battle_reset_dungeon{result=1,restTimes=Dungeon#p_dungeon.restTimes}).
	
check_can_reset_dungeon(Type,DungeonID)->
	case lists:member(Type, ?BATTLE_DUNGEON_TYPE_LIST) of
		false->
			{false,5};
		true ->
			Progress = get_progress(Type),
			if Progress < DungeonID->
				   {false,6};
			   true ->
				    #role{roleID=RoleID, vipLevel=VipLevel} = Role = role_data:get_roleInfo(),
				   	Chapter = get_chapter_by_dungeonID(RoleID, DungeonID),
					#p_dungeon{resetTimes=ResetTimes,restTimes=RestTimes}=Dungeon = get_dungeon_by_chapter(Chapter, DungeonID),
					if RestTimes >0 ->
						   {false,4};
					   true ->
						   #data_dungeon{maxTimes=MaxTimes} = data_dungeon:get(DungeonID),
						   #data_vip{dungeonResetTimes=MaxDungeonResetTimes}=data_vip:get(VipLevel),
						   if ResetTimes >= MaxDungeonResetTimes ->
								  {false,2};
							  true ->
								  NewTimes = ResetTimes + 1,
								  NeedGold = lists:nth(NewTimes,data_common:get(buy_dungeon_reset_gold)),
                            	  case role_lib:check_money(Role,gold,NeedGold) of
                                		true ->
											Dungeon2 = Dungeon#p_dungeon{resetTimes=ResetTimes+1,restTimes=MaxTimes},
											{true, Role,NeedGold,Chapter,Dungeon2};
									  _ ->
										  {false,3}
								  end
							end
					end
			end
	end.

check_get_star_reward(ChapterID,0) ->
	case data_chapter:get(ChapterID) of
		?undefined ->
			{false,4};
		#data_chapter{starRewardID=StarRewardID} ->
			#role{roleID=RoleID}=Role=role_data:get_roleInfo(),
			#chapter{dungeonList=DungeonList,starRewarded=StarRewarded,perfectRewarded=PerfectReward}=Chapter=get_chapter(RoleID,ChapterID),
			if  StarRewarded >= 7 ->
					{false,2};
				true ->
					#data_chapter_star_reward{rewardList=RewardList}=data_chapter_star_reward:get(StarRewardID),
					TotalScore = lists:foldl(fun(#p_dungeon{bestScore=DScore},Acc) -> Acc+DScore end, 0, DungeonList),
					StarRewarded2 = 
						case PerfectReward of
							1 ->
								StarRewarded bor 4;
							_ ->
								StarRewarded
						end,
					GetRewardList = [STReward||{ID,Star,STReward}<-RewardList,TotalScore >= Star,ID band StarRewarded2 == 0], 
					{true,Chapter#chapter{starRewarded=7},GetRewardList,Role}
			end
	end;	
check_get_star_reward(ChapterID,RewardID) ->
	if RewardID > 7 ->
		   {false,5};
	   true ->
		   case data_chapter:get(ChapterID) of
			   ?undefined ->
				   {false,4};
			   #data_chapter{starRewardID=StarRewardID} ->
				   #role{roleID=RoleID} = Role = role_data:get_roleInfo(),
				   #chapter{dungeonList=DungeonList,starRewarded=StarRewarded,perfectRewarded=PerfectRewarded}=Chapter = get_chapter(RoleID,ChapterID),
				   StarRewarded2 = 
					   case PerfectRewarded of
						   true ->
							   StarRewarded bor 4;
						   _ ->
							   StarRewarded
					   end,
				   State =  RewardID band StarRewarded2,
				   if State > 0 ->
						  {false,2};
					  true ->
						  #data_chapter_star_reward{rewardList=RewardList} = data_chapter_star_reward:get(StarRewardID),
						  TotalScore = lists:foldl(fun(#p_dungeon{bestScore=DScore},Acc)->Acc+DScore end, 0, DungeonList),
						  {_,NeedScore,SellReward} = lists:keyfind(RewardID,1,RewardList),
						  if TotalScore < NeedScore ->
								 {false,3};
							 true ->
								 {true,Chapter#chapter{starRewarded=StarRewarded2 bor RewardID},[SellReward],Role}
						  end
				   end
		   end
	end.

do_get_star_reward(Role,Chapter,RewardList,ChapterID,RewardID) ->
	set_chapter(Chapter),
	?sendself(#sc_battle_star_reward{result=1,starRewarded=Chapter#chapter.starRewarded
									 ,reward=[activity_server:sell_reward2p_reward_info(Reward)||Reward<-RewardList]}),
	[role_reward:handle_sell_reward_f(Role,Reward,?MONEY_ADD_TYPE_CHAPTER_STAR_REWARD,ChapterID,integer_to_list(RewardID))
	||Reward <- RewardList].
	



%% ====================================================================
%% Internal functions
%% ====================================================================

%% 保存战役进度，type是分别保存三种不同难度的战役进度
set_progress(Type,Progress) ->
	put({?batProg,Type}, Progress).
%% 读取战役进度，type是分别保存三种不同难度的战役进度
get_progress(Type) ->
	case get({?batProg,Type}) of
		Progress when is_integer(Progress) ->
            ?INFO("get_progress:~w",[Progress]),
			Progress;
		?undefined ->
			hd(data_dungeon:get_list())
	end.


get_dungeon_by_chapter(Chapter, DungeonID) ->
	case lists:keyfind(DungeonID, #p_dungeon.dungeonID, Chapter#chapter.dungeonList) of
		false ->
			new_dungeon(DungeonID);
		DungeonInfo ->
			DungeonInfo
	end.

get_chapter_by_dungeonID(RoleID, DungeonID) ->
	#data_dungeon{chapterID=ChapterID} = data_dungeon:get(DungeonID),
	#data_chapter{dungeonIDList=DungeonIDList} = data_chapter:get(ChapterID),
	case lists:member(DungeonID,DungeonIDList) of
		true->
			get_chapter(RoleID, ChapterID);
		false->
			?ERR("DungeonID：~w is not in ChapterID:~w ~n",[DungeonID,ChapterID]),
			?undefined
	end.

get_chapter(RoleID, ChapterID) ->	
	case get({?chapter, ChapterID}) of
		Info when is_record(Info, chapter) ->
			%% 刷新一次
			case regular(Info) of
				Info ->
					Info;
				Info2 ->
					set_chapter(Info2),
					Info2
			end;
		?undefined ->
			case db_sql:get_chapter(RoleID, ChapterID) of
				?undefined->
					%% 新创建的不需要regular
					NewChapter = new_chapter(RoleID,ChapterID),
					set_chapter(NewChapter),
					NewChapter;
				#chapter{}=Chapter ->
					RegChapter = regular(Chapter),
					set_chapter(RegChapter),
					RegChapter					
			end
	end.

new_chapter(_RoleID,ChapterID) ->
	#chapter{
			 perfectRewarded=false,
			 curDate=erlang:date(),
			 dungeonList=[],
			 id=ChapterID,
			 starRewarded=0}.

new_dungeon(DungeonID) ->
	#p_dungeon{
			   dungeonID=DungeonID,
			   bestScore=0,
			   restTimes=(data_dungeon:get(DungeonID))#data_dungeon.maxTimes,
			   resetTimes=0
			  }.

regular(Chapter) ->
	NowDate = erlang:date(),
	case NowDate =:= Chapter#chapter.curDate of
		false ->
			DungeonList = [Dungeon#p_dungeon{restTimes=((data_dungeon:get(Dungeon#p_dungeon.dungeonID))#data_dungeon.maxTimes)
											,resetTimes=0}
											|| Dungeon <- Chapter#chapter.dungeonList],
			Chapter#chapter{curDate=NowDate,dungeonList=DungeonList};
		true ->
			Chapter
	end.

%%为玩家初始化所有关卡的关卡boss奖励信息
init_battlebossrewardinfo()->
	lists:foldl(fun(BattleType,Acc)->
		ChapterBossRewardInfo = init_chapterbossrewardinfo(BattleType),
		[#battle_boss_reward_info{battletype=BattleType,chapterlist=ChapterBossRewardInfo}|Acc]
	end,[],[?BATTLE_DUNGEON_TYPE_NORMAL]).

%%{{data_battle,chapterID},[{dungenID1,Reward1},{dungenID2,Reward2}]}.
init_chapterbossrewardinfo(BattleType)->
	CurrentBattleProgress = role_battle:get_progress(BattleType),
	ChapterID1 = caculate_chapterID_by_dungeonID(CurrentBattleProgress),
	%%最后一关需要特殊处理，最后一关是否通关，进度都会是30800，所以需要特殊处理
	ChapterID = case CurrentBattleProgress=:=30800 andalso ChapterID1=:=1080 of
		true->
			#role{roleID=RoleID} = role_data:get_roleInfo(),
			Chapter = get_chapter(RoleID, ChapterID1),
			#chapter{dungeonList=DungeonList}= Chapter,
			case lists:keyfind(CurrentBattleProgress, #p_dungeon.dungeonID, DungeonList) of
				false->
					ChapterID1;
				_ ->
					ChapterID1+1
			end;
		false->
			ChapterID1
	end,
	%%ChapterID表示的是将要挑战的关卡，在最后关卡的位置，设置了最大关卡+1的关卡，并且做了特殊处理，故最后一个关卡攻打完之后，此处也能统一
	init_chapterbossrewardinfo2(ChapterID-1,[]).

init_chapterbossrewardinfo2(1000,Acc)->
	Acc;
init_chapterbossrewardinfo2(ChapterID,Acc)->
	NewAcc = case data_battle_boss_reward:get({data_battle,ChapterID}) of
		?undefined->
			Acc;
		BossDungeonList1->
			DungeonRewardList = lists:foldl(fun(BossDungeon,DungeonAcc)->
				{BossDungeonID,_Reward} = BossDungeon,
			[#dungeon_boss_reward_info{dungeonID=BossDungeonID,rewardstatus=?DUNGEON_BOSS_REWARD_UNACCEPT_TYPE}|DungeonAcc]
			end,[],BossDungeonList1),
			ChapterInfo = #chapter_boss_reward_info{chapterID=ChapterID,dungeonlist=DungeonRewardList},
			[ChapterInfo|Acc]
	end,
	init_chapterbossrewardinfo2(ChapterID-1,NewAcc).

update_battle_boss_reward_info(Type,OldProgress,DungeonID)->
    ?INFO("update_battle_boss_reward_info Type:~w OldProgress:~w DungeonID:~w",[Type,OldProgress,DungeonID]),
	if 
		DungeonID =:= OldProgress -> 
			#role{roleID=RoleID} = role_data:get_roleInfo(),
			#data_dungeon{chapterID=ChapterID} = data_dungeon:get(DungeonID),
			Chapter = get_chapter(RoleID,ChapterID),
			#chapter{dungeonList=DungeonList}= Chapter,
			case lists:keyfind(DungeonID, #p_dungeon.dungeonID, DungeonList) of
				false->
					update_battle_boss_reward_info2(Type,DungeonID,?DUNGEON_BOSS_REWARD_UNACCEPT_TYPE,true);
				_->
					ignore
			end;
		DungeonID >=OldProgress ->
			%此处的Progress表示达到了对应的关卡，而不是通过了对应关卡，故需要减少一个关卡
			update_battle_boss_reward_info2(Type,DungeonID,?DUNGEON_BOSS_REWARD_UNACCEPT_TYPE,true);
		true->
			ignore
	end.

%%更新玩家对应关卡boss宝箱信息,如果是ACCEPT状态，则删除
update_battle_boss_reward_info2(Type,DungeonID,Status,NeedAdd)->
	BattleBossRewardInfo = role_data:get_battlebossrewardinfo(),
    ?INFO("update_battle_boss_reward_info2 -1- ~w>~w",[[Type,DungeonID,Status,NeedAdd],BattleBossRewardInfo]),
	case lists:keytake(Type,#battle_boss_reward_info.battletype,BattleBossRewardInfo) of
		false->
			% ?ERR("can not find battle type:~w in BattleBossRewardInfo:~w ~n",[Type,BattleBossRewardInfo]),
			error;
		{_Value,#battle_boss_reward_info{chapterlist=ChapterList}=TypeBattleBossRewardInfo,Other}->
			ChapterID = caculate_chapterID_by_dungeonID(DungeonID),
			case create_battle_boss_reward_dungeon_info(DungeonID,Status) of
				?undefined->
					% ?ERR("can not create dungeon reward info DungeonID:~w Status:~w ~n",[DungeonID,Status]),
					error;
				DungeonRewardUnit->
                    ?INFO("update_battle_boss_reward_info2 -2- DungeonRewardUnit:~w",[DungeonRewardUnit]),
					{Result,NewTypeBattleBossRewardInfo} = case lists:keytake(ChapterID,#chapter_boss_reward_info.chapterID,ChapterList) of
						false->
							case NeedAdd of
								true->
									AddChapter = #chapter_boss_reward_info{chapterID=ChapterID,dungeonlist=[DungeonRewardUnit]},
									{ok,TypeBattleBossRewardInfo#battle_boss_reward_info{chapterlist=[AddChapter|ChapterList]}};
								false->
									{error,TypeBattleBossRewardInfo}
							end;
						{_Value,#chapter_boss_reward_info{dungeonlist=ChapterDungeonList}= FindChapter,OtherChapterList}->
							{ChapterResult,NewFindChapter} = case lists:keytake(DungeonRewardUnit#dungeon_boss_reward_info.dungeonID,#dungeon_boss_reward_info.dungeonID,ChapterDungeonList) of
								false->
									case NeedAdd of
										true->
											{ok,FindChapter#chapter_boss_reward_info{dungeonlist=[DungeonRewardUnit|ChapterDungeonList]}};
										false->
											{error,FindChapter}
									end;
								{_Value,FindDungeon,OtherDungeonList}->
									case NeedAdd of
										true->
											% ?ERR("find dungeonID:~w insert into BattleBossReward:~w too many times ~n",[DungeonID,FindChapter]),
											{error,FindChapter};
										false->
											if 
												Status=:=?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE andalso OtherDungeonList=/=[]->
													{ok,FindChapter#chapter_boss_reward_info{dungeonlist=OtherDungeonList}};
												Status=:=?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE andalso OtherDungeonList=:=[]->
													{ok,[]};
												true ->
													NewFindDungeon = FindDungeon#dungeon_boss_reward_info{rewardstatus=Status},
													{ok,FindChapter#chapter_boss_reward_info{dungeonlist=[NewFindDungeon|OtherDungeonList]}}
											end
									end
							end,
							case NewFindChapter of
								[]->
									{ChapterResult,TypeBattleBossRewardInfo#battle_boss_reward_info{chapterlist=OtherChapterList}};
								_ ->
									{ChapterResult,TypeBattleBossRewardInfo#battle_boss_reward_info{chapterlist=[NewFindChapter|OtherChapterList]}}
							end
					end,
					case Result of
						ok->
							role_data:set_battlebossrewardinfo([NewTypeBattleBossRewardInfo|Other]);
						error->
							error
					end
			end
	end.
get_dungeon_status(DungeonID,Type)->
	ChapterID = caculate_chapterID_by_dungeonID(DungeonID),
	get_dungeon_status2(ChapterID,DungeonID,Type).
get_dungeon_status2(ChapterID,DungeonID,Type)->
    ?INFO("get_dungeon_status2 ChapterID:~w DungeonID:~w Type:~w ",[ChapterID,DungeonID,Type]),
	case lists:member(Type,[?BATTLE_DUNGEON_TYPE_NORMAL,?BATTLE_DUNGEON_TYPE_TRANSMIGRATION]) of
		false->
			?INVALID_BATTLE_DUNGEON_TYPE;
		true->
			case data_battle_boss_reward:get({data_battle,ChapterID}) of
				?undefined->
					?INVALID_CHAPTER_TYPE;
				DungeonList->
					case lists:keyfind(DungeonID,1,DungeonList) of
						false->
							?DUNGEON_BOSS_REWARD_NOBOSSBOX_TYPE;
						_ ->
							R = get_dungeon_status_in_battlebossrewardinfo(ChapterID,DungeonID,Type),
                            ?INFO("get_dungeon_status_in_battlebossrewardinfo:~w",[R]),
                            R
					end
			end
	end.
get_dungeon_status_in_battlebossrewardinfo(ChapterID,DungeonID,Type)->
	BattleBossRewardInfo = role_data:get_battlebossrewardinfo(),
    ?INFO("BattleBossRewardInfo:~w",[BattleBossRewardInfo]),
	case lists:keytake(Type,#battle_boss_reward_info.battletype,BattleBossRewardInfo) of
		false->
			?ERR("can not find battle type:~w in BattleBossRewardInfo:~w ~n",[Type,BattleBossRewardInfo]),
			?INVALID_BATTLE_DUNGEON_TYPE;	
		{_Value,#battle_boss_reward_info{chapterlist=ChapterList},_Other}->
			BattleProgress = role_battle:get_progress(Type),
			if 
				DungeonID > BattleProgress ->
					?DUNGEON_BOSS_REWARD_UNPASSED_TYPE;
				true->
					#role{roleID=RoleID} = role_data:get_roleInfo(),
					Chapter = get_chapter(RoleID, ChapterID),
                    ?INFO("get_chapter ~w ~w>~w",[RoleID, ChapterID,Chapter]),
					#chapter{dungeonList=DungeonList}= Chapter,
					case lists:keyfind(DungeonID, #p_dungeon.dungeonID, DungeonList) of
						false->
							?DUNGEON_BOSS_REWARD_UNPASSED_TYPE;
						_->
							case lists:keytake(ChapterID,#chapter_boss_reward_info.chapterID,ChapterList) of
								false->
									?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE;
								{_Value,#chapter_boss_reward_info{dungeonlist=ChapterDungeonList},_OtherChapterList}->
									case lists:keytake(DungeonID,#dungeon_boss_reward_info.dungeonID,ChapterDungeonList) of
										false->
											?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE;
										{_Value,FindDungeon,_OtherDungeonList}->
											FindDungeon#dungeon_boss_reward_info.rewardstatus
									end
							end
					end
			end
	end.
%获得对应章节boss宝箱情况
get_chapter_status(ChapterID,Type)->
	case lists:member(Type,[?BATTLE_DUNGEON_TYPE_NORMAL,?BATTLE_DUNGEON_TYPE_TRANSMIGRATION]) of
		false->
			?INVALID_BATTLE_DUNGEON_TYPE;
		true->
			case data_battle_boss_reward:get({data_battle,ChapterID}) of
				?undefined->
					?INVALID_CHAPTER_TYPE;
				RewardDungeonList ->
					%%依据完成的章节内的关卡是否包含有boss关卡来确定能够领取
					case is_boss_dungeon_in_chapter_finish_dungeon(ChapterID,RewardDungeonList) of
						false->
							?BOSS_DUNGEON_NO_IN_FINISH_DUNGEON;
						true->
							BattleBossRewardInfo = role_data:get_battlebossrewardinfo(),
							case lists:keytake(Type,#battle_boss_reward_info.battletype,BattleBossRewardInfo) of
								false->
									?CHAPTER_BOSS_REWARD_ACCEPTED;
								{_Value,#battle_boss_reward_info{chapterlist=ChapterList},_Other}->
									case lists:keytake(ChapterID,#chapter_boss_reward_info.chapterID,ChapterList) of
										false->
											?CHAPTER_BOSS_REWARD_ACCEPTED;
										{_Value,#chapter_boss_reward_info{dungeonlist=ChapterDungeonList},_OtherChapterList}->
											case ChapterDungeonList=:=[] of
												true->
													?CHAPTER_BOSS_REWARD_ACCEPTED;
												false->
													?CHAPTER_BOSS_REWARD_UNACCEPT
											end
									end
							end
					end
			end
	end.

is_boss_dungeon_in_chapter_finish_dungeon(ChapterID,BossDungeonList)->
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	Chapter = get_chapter(RoleID, ChapterID),
	#chapter{dungeonList=DungeonList}= Chapter,
	lists:any(fun({DungeonID,_Reward})->case lists:keyfind(DungeonID,#p_dungeon.dungeonID,DungeonList) of false->false;_->true end end,BossDungeonList).
%%根据关卡ID以及战役类型创建boss宝箱记录，会检查是否有对应的配置
create_battle_boss_reward_dungeon_info(DungeonID,Status)->
	ChapterID = caculate_chapterID_by_dungeonID(DungeonID),
	case data_battle_boss_reward:get({data_battle,ChapterID}) of
		?undefined->
			?undefined;
		DungeonList->
			case lists:keyfind(DungeonID,1,DungeonList) of
				false->
					?undefined;
				_FindDungeon->
					#dungeon_boss_reward_info{dungeonID=DungeonID,rewardstatus=Status}
			end
	end.

set_dungeon(Chapter, Dungeon) ->
    ?INFO("set_dungeon Chapter:~w Dungeon:~w ",[Chapter, Dungeon]),
	DungeonList2 = lists:keystore(Dungeon#p_dungeon.dungeonID, #p_dungeon.dungeonID, Chapter#chapter.dungeonList, Dungeon),
	Chapter2 = Chapter#chapter{dungeonList=DungeonList2},
	set_chapter(Chapter2),
	Chapter2.

set_chapter(#chapter{id=ChapterID}=Chapter) ->
    ?INFO("set_chapter Chapter:~w",[Chapter]),
	put({?chapter,ChapterID}, Chapter).


%%直接读取配置能够获得ChapterID,此处如果打通关之后，Progress的值为30801,直接查询30801将出错，故对其特殊处理
caculate_chapterID_by_dungeonID(DungeonID) when DungeonID >= 33001 andalso DungeonID < 34000 ->
    ((DungeonID-1) div 10) - 300 + 1;
caculate_chapterID_by_dungeonID(DungeonID) when DungeonID>31000->
	1100;
caculate_chapterID_by_dungeonID(DungeonID)->
	Rem = DungeonID rem 10000,
	SubChapterDiv = Rem div 10,
	SubChapterRem1 = Rem rem 10,
	SubChapterRem = case SubChapterRem1=:=0 of
		true->
			SubChapterDiv;
		false->
			SubChapterDiv+1
	end,
	Div = DungeonID div 10000,
	case Div of
		1->
			2000+SubChapterRem;
		3->
			1000+SubChapterRem
	end.

			%%困难关卡

	% case data_dungeon:get(DungeonID) of
	% 	?undefined->
	% 		?undefined;
	% 	#data_dungeon{chapterID=ChapterID}->
	% 		ChapterID
	% end.





%%根据关卡ID,用辅助精灵重组玩家的战斗列表
reorganize_fighterlist_with_assist(FighterList,NeedAssist,DungeonID)->
	case NeedAssist of
		false->
			FighterList;
		true->
			case data_drama:get(DungeonID) of
				?undefined->
					FighterList;
				DramaData->
					reorganize_fighterlist_with_assist2(DramaData,FighterList)
			end
	end.

reorganize_fighterlist_with_assist2({_SpecialActionList,MonDataList,UserPosList},FighterList)->
	RoleFighterList = get_maxfightpower_list(FighterList,UserPosList),
	UserFighterGerTypeIDList = [GerBase#gerBase.gerTypeID||#ger{gerBase=GerBase}<-RoleFighterList],
	MonsterList = create_monster_list(MonDataList,UserFighterGerTypeIDList),
	RoleFighterList++MonsterList.
get_maxfightpower_list([],_UserPosList)->
	?ERR("error: fighterList is null~n"),
	[];
get_maxfightpower_list(FighterList,UserPosList)->
	SortFighterList = lists:sort(fun(A,B)->
		#ger{gerAttr=GerAttrA} = A,
		#ger{gerAttr=GerAttrB} = B,
		GerAttrA#gerAttr.gerFightPower > GerAttrB#gerAttr.gerFightPower
	end,FighterList),
	get_maxfightpower_list2(SortFighterList,UserPosList,[]).

get_maxfightpower_list2([],_Pos,Acc)->
	Acc;
get_maxfightpower_list2(_FighterList,[],Acc)->
	Acc;
get_maxfightpower_list2([H|T],[Pos|PosT],Acc)->
	#ger{gerBase=GerBase} = H,
	NewGerBase = GerBase#gerBase{gerPos=Pos},
	NH = H#ger{gerBase=NewGerBase},
	get_maxfightpower_list2(T,PosT,[NH|Acc]).

create_monster_list(MonsterList,UserFighterGerTypeIDList)->
	MonGerTypeIDList = [ID||{#mon{gerTypeID=ID},_Pos} <- MonsterList],
	AllGerTypeIDList = MonGerTypeIDList ++ UserFighterGerTypeIDList,
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, E#mon.gerQuality, [], lists:delete(GerTypeID, AllGerTypeIDList))
		end,
	[?change_pos(Ger, Pos)||{Mon,Pos} <- MonsterList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].

chapterid2battletype(ChapterID)->
	case ChapterID div 1000 of
		1->
			?BATTLE_DUNGEON_TYPE_NORMAL;
		2->
			?BATTLE_DUNGEON_TYPE_HARD;
        3->
            ?BATTLE_DUNGEON_TYPE_TRANSMIGRATION;
		_ ->
			?undefined
	end.

test()->
	user_default:l(),
	timer:sleep(2000),
	ID=user_default:id(),
	user_default:emu(ID,#cs_battle_info{chapterID=1}),
	user_default:emu(ID,#cs_battle_progress{}),
	user_default:emu(ID,#cs_battle_challenge{dungeonID=1}),
	user_default:emu(ID,#cs_battle_perfect_reward{chapterID=1}).


format_config(List) ->
	Result=lists:map(fun format_config2/1, List),
	io:format("finish cacl data_dungeon\n"),
	Result.

format_config2(DataDungeon) ->
	#data_dungeon{gerID1=D1,gerID2=D2,gerID3=D3,gerID4=D4,gerID5=D5,gerID6=D6,trainer=Trainer}=DataDungeon,		
	MonList = gen_mon_list(D1,D2,D3,D4,D5,D6),
	case Trainer of
	{TrainerID,Level,Special} ->
	TrSpe = #trSpecial{trID = TrainerID,specialID = Special,roleLevel = Level,sp = 0,state = 0};
		_ ->
			TrSpe = #trSpecial{}
	end,
	List = [{1,D1},{2,D2},{3,D3},{4,D4},{5,D5},{6,D6}],
	IDList = [ID||{_Pos, #mon{gerTypeID=ID}} <- List],
	DataDungeon#data_dungeon{gerID1=MonList,gerID2=List,gerID3=IDList,gerID4=0,gerID5=0,gerID6=0,trainer=TrSpe}.

gen_mon_list(D1,D2,D3,D4,D5,D6) ->
	List = [{1,D1},{2,D2},{3,D3},{4,D4},{5,D5},{6,D6}],
	IDList = [ID||{_Pos, #mon{gerTypeID=ID}} <- List],
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, E#mon.gerQuality, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- List, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].


get_first_dungeonID() ->
	data_battle_setting:get(common_battle_start_dungeonID).
%hd((data_chapter:get(hd(data_chapter:get_list())))#data_chapter.dungeonIDList).

%% 如何获取第一个章节的id
get_first_hard_dungeonID() ->
	data_battle_setting:get(hard_battle_start_dungeonID).

get_first_fast_hard_dungeonID()->
	data_battle_setting:get(fast_hard_battle_start_dungeonID).

get_first_transmigration_dungeonID()->
    data_battle_setting:get(transmigration_battle_start_dungeonID).

load_star_reward_config(List)->
	[{data_chapter_star_reward,ID,[begin
									   NID = 1 bsl (ID1 - 1), 
									   {NID,Star,Reward}
								   end
								   ||{ID1,Star,Reward}<-RList]}
	 ||{data_chapter_star_reward,ID,RList}<-List].

demo_fight()->
	MonTeam = [{ger,5013,{gerBase,5013,1,1,1,0,[],[],0},{gerAttr,0,0,0,0,25347,140000,0,100,0,0,0,0,1500,0,0,0,0,0,0,0,105,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},140000,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5043,{gerBase,5043,1,1,2,0,[],[],0},{gerAttr,0,0,0,0,13563,140000,0,100,0,0,21,0,0,0,0,0,0,0,0,0,113,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},140000,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5053,{gerBase,5053,1,1,3,0,[],[],0},{gerAttr,0,0,0,0,27896,140000,0,100,15,0,0,0,0,0,0,0,0,0,0,0,112,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},140000,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5113,{gerBase,5113,1,1,4,0,[],[],0},{gerAttr,0,0,0,0,10990,80000,0,100,0,0,0,0,0,0,0,0,21,0,0,0,122,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},80000,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5063,{gerBase,5063,1,1,5,0,[],[],0},{gerAttr,0,0,0,0,27880,130000,0,100,0,0,0,0,0,0,0,0,0,0,0,30,125,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},240000,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5033,{gerBase,5033,1,1,6,0,[],[],0},{gerAttr,0,0,0,0,20442,80000,0,100,0,0,0,0,0,0,0,0,0,20,0,0,114,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},80000,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],

RoleTeam = [{ger,5123,{gerBase,5123,20,1,1,0,[],[],0},{gerAttr,0,0,0,0,34563,700000,100,100,15,0,0,0,0,0,0,0,0,0,0,0,136,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},700000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5133,{gerBase,5133,20,1,3,0,[],[],0},{gerAttr,0,0,0,0,50475,700000,100,100,15,0,0,0,0,0,0,0,0,0,0,0,123,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},700000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5023,{gerBase,5023,20,1,4,0,[],[],0},{gerAttr,0,0,0,0,64486,700000,100,100,0,0,0,0,0,0,15,0,0,0,0,0,105,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},700000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,5103,{gerBase,5103,20,1,6,0,[],[],0},{gerAttr,0,0,0,0,98767,700000,100,100,0,0,0,0,0,0,0,0,0,30,0,0,128,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},700000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
%{_, FightRecord, _} =
						 role_fight:new(0,RoleTeam, MonTeam, #add_attr{},#add_attr{},[],[],#trSpecial{},#trSpecial{},false,#skin_info{},#skin_info{}).
	%?ERR(":~w",[FightRecord]).

demo_fight2()->
	MonTeam = [{ger,7243,{gerBase,7243,1,1,1,0,[],[],0},{gerAttr,0,0,0,0,141182,793800,0,100,0,0,0,0,1500,0,0,0,0,0,0,0,105,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},793800,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,6103,{gerBase,6103,1,1,2,0,[],[],0},{gerAttr,0,0,0,0,75546,793800,0,100,0,0,21,0,0,0,0,0,0,0,0,0,113,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},793800,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,7253,{gerBase,7253,1,1,3,0,[],[],0},{gerAttr,0,0,0,0,155384,793800,0,100,15,0,0,0,0,0,0,0,0,0,0,0,112,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},793800,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,6053,{gerBase,6053,1,1,4,0,[],[],0},{gerAttr,0,0,0,0,611214,453600,0,100,0,0,0,0,0,0,0,0,21,0,0,0,122,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},453600,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,7093,{gerBase,7093,1,1,5,0,[],[],0},{gerAttr,0,0,0,0,155291,1360800,0,100,0,0,0,0,0,0,0,0,0,0,0,30,125,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},1360800,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,6033,{gerBase,6033,1,1,6,0,[],[],0},{gerAttr,0,0,0,0,113862,453600,0,100,0,0,0,0,0,0,0,0,0,20,0,0,114,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},453600,0,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],

RoleTeam = [{ger,7204,{gerBase,7204,20,1,1,0,[],[],0},{gerAttr,0,0,0,0,171835,1500000,100,100,15,0,0,0,0,0,0,0,0,0,0,0,136,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},1500000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,7134,{gerBase,7134,20,1,3,0,[],[],0},{gerAttr,0,0,0,0,462754,1500000,100,100,15,0,0,0,0,0,0,0,0,0,0,0,123,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},1500000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,7194,{gerBase,7194,20,1,4,0,[],[],0},{gerAttr,0,0,0,0,342388,1500000,100,100,0,0,0,0,0,0,15,0,0,0,0,0,105,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},1500000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
,{ger,7184,{gerBase,7184,20,1,6,0,[],[],0},{gerAttr,0,0,0,0,451788,1500000,100,100,0,0,0,0,0,0,0,0,0,30,0,0,128,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},1500000,100,0,{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
%{_, FightRecord, _} =
						 role_fight:new(0,RoleTeam, MonTeam, #add_attr{},#add_attr{},[],[],#trSpecial{},#trSpecial{},false,#skin_info{},#skin_info{}).



% %%小火龙特殊场景
% get_special_battle_result(5120,_DungeonID)->
% 	% {sc_battle_challenge,1,[{sc_fight_request,[{p_fighter,2000000012124,5120,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,77,[],0,0,0,0},{p_action,5,76,[],0,0,0,0},{p_action,1,75,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,56,[],25,-803860,0,0},{p_action,-2,26,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,56,[],25,-1930,-400000,64},{p_action,5,26,[-2],50,0,0,0},{p_action,5,53,[],25,-77,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true}],[],1};
% 	{sc_fight_request,[{p_fighter,2000000012124,5120,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,77,[],0,0,0,0},{p_action,5,76,[],0,0,0,0},{p_action,1,75,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,56,[],25,-803860,0,0},{p_action,-2,26,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,56,[],25,-1930,-400000,64},{p_action,5,26,[-2],50,0,0,0},{p_action,5,53,[],25,-77,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true};
% %%皮卡丘特殊场景
% get_special_battle_result(5130,_DungeonID)->
% 	% {sc_battle_challenge,1,[{sc_fight_request,[{p_fighter,2000000012124,5130,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,85,[],0,0,0,0},{p_action,5,84,[],0,0,0,0},{p_action,1,83,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,57,[],25,-432718,0,0},{p_action,-2,27,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,57,[],25,0,-216359,0},{p_action,5,27,[-2],50,0,0,0},{p_action,5,53,[],25,-77,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true}],[],1};
% 	{sc_fight_request,[{p_fighter,2000000012124,5130,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,85,[],0,0,0,0},{p_action,5,84,[],0,0,0,0},{p_action,1,83,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,57,[],25,-432718,0,0},{p_action,-2,27,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,57,[],25,0,-216359,0},{p_action,5,27,[-2],50,0,0,0},{p_action,5,53,[],25,-77,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true};
% %%妙蛙种子特殊场景
% get_special_battle_result(5020,_DungeonID)->
% 	% {sc_battle_challenge,1,[{sc_fight_request,[{p_fighter,2000000012124,5020,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,81,[],0,0,0,0},{p_action,5,80,[],0,0,0,0},{p_action,1,79,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,58,[],25,-392044,0,0},{p_action,-2,28,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,58,[],25,0,-196022,0},{p_action,5,28,[-2],50,0,0,0},{p_action,5,53,[],25,-72,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true}],[],1};
% 	{sc_fight_request,[{p_fighter,2000000012124,5020,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,81,[],0,0,0,0},{p_action,5,80,[],0,0,0,0},{p_action,1,79,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,58,[],25,-392044,0,0},{p_action,-2,28,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,58,[],25,0,-196022,0},{p_action,5,28,[-2],50,0,0,0},{p_action,5,53,[],25,-72,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true};
% %%杰尼龟特殊场景
% get_special_battle_result(5100,_DungeonID)->
% 	% {sc_battle_challenge,1,[{sc_fight_request,[{p_fighter,2000000012124,5100,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,101,[],0,36566,0,2},{p_action,5,113,[],0,100,0,2},{p_action,1,74,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,55,[],25,-394278,0,0},{p_action,-2,25,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,55,[],25,0,-197139,0},{p_action,5,25,[-2],50,0,0,0},{p_action,5,53,[],25,-73,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true}],[],1}.
% 	{sc_fight_request,[{p_fighter,2000000012124,5100,5,1662131,1662131,0,0,0,100,0,0,0,0},{p_fighter,2000000006970,7250,12,179822,179822,400000,400000,0,100,20,0,0,0},{p_fighter,2000000012219,7250,-2,34919,34919,400000,400000,0,100,23,0,0,0},{p_fighter,8589935594,0,1,0,0,0,0,0,0,0,0,0,0}],[{p_action,-2,4,[],0,0,0,0},{p_action,-2,53,[],0,-1940480,0,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,9,[],0,179822,0,0},{p_action,2,53,[],0,-892390,-77850,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,9,[],0,34919,0,0},{p_action,-2,53,[],0,-246195,-238925,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-242560,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-121280,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],0,0,-60640,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-30320,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-15160,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-7580,0},{p_action,2,23,[-2],0,0,0,0},{p_action,2,115,[],0,0,0,0},{p_action,2,53,[],25,0,-3790,0},{p_action,-2,23,[2],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,53,[],25,0,-1895,64},{p_action,2,23,[-2],50,0,0,0},{p_action,12,16,[],0,0,0,0},{p_action,5,201,[],0,0,0,0},{p_action,5,101,[],0,36566,0,2},{p_action,5,113,[],0,100,0,2},{p_action,1,74,[5],2,0,0,128},{p_action,0,10,[],0,0,0,0},{p_action,5,55,[],25,-394278,0,0},{p_action,-2,25,[5],0,0,0,0},{p_action,-2,115,[],0,0,0,0},{p_action,-2,55,[],25,0,-197139,0},{p_action,5,25,[-2],50,0,0,0},{p_action,5,53,[],25,-73,0,0},{p_action,-2,23,[5],50,0,0,0},{p_action,-2,72,[],0,0,0,0},{p_action,-2,71,[],0,0,0,0},{p_action,1,70,[-2],2,0,0,128},{p_action,0,10,[],0,0,0,0}],true}.
get_special_battle_result(MainGerTypeID)->
	get_special_battle_result(MainGerTypeID,30001,1001).
get_special_battle_result(MainGerTypeID,DungeonID,#trSpecial{specialID=TrSpecialID}=TrSpecial)->
	SkinInfo = role_skin:get_skin_info(),
	case data_drama:get({special_dungeon,DungeonID,MainGerTypeID,TrSpecialID}) of
		?undefined->
			case data_drama:get({special_dungeon,DungeonID,0,TrSpecialID}) of
				?undefined->
					?ERR("undefined special dungeonID:~w MainGerTypeID:~w TrSpecialID:~w ~n",[DungeonID,MainGerTypeID,TrSpecialID]),
					[];
				Temp->
					change_fighter(Temp,MainGerTypeID,TrSpecial,SkinInfo)
			end;	
		X ->
			change_fighter(X,MainGerTypeID,TrSpecial,SkinInfo)
	end.

test_change_main_gertypeid(GerTypeID,RoleID)->
	case lists:member(GerTypeID,[5120,5130,5100,5020]) of
		true->
			case catch role_lib:send_server(RoleID,{update_main_gerTypeid,GerTypeID}) of
    			{'EXIT',_}->
      				?INFO(" 修改主精灵：~w 晶体信息失败~n",[GerTypeID]);
    			_->
      				ok
  			end;
  		false->
  			ignore
  	end.

% %战役BOSS宝箱领取信息(增加这一步，用于扩展其他难度的战役能开同样的宝箱)
% -record(battle_boss_reward_info,{battletype=0,chapterlist=[]}).

% %每个章节的boss宝箱领取信息
% -record(chapter_boss_reward_info,{chapterID=0,dungeonlist=[]}).

% %关卡的boss宝箱领取信息
% -record(dungeon_boss_reward_info,{dungeonID=0,rewardstatus=0}).
transform_persistform2termform(-1,_Acc)->
	-1;
transform_persistform2termform([],Acc)->
	Acc;
transform_persistform2termform([H|T],Acc)->
	BattleTerm = transform_persistform2battleform(H),
	transform_persistform2termform(T,[BattleTerm|Acc]).

transform_persistform2battleform(H)->
	{BattleType,ChapterList} = H,
	#battle_boss_reward_info{battletype=BattleType,chapterlist=transform_persistform2chapterform(ChapterList,[])}.

transform_persistform2chapterform([],Acc)->
	Acc;
transform_persistform2chapterform([H|T],Acc)->
	{ChapterID,DungeonList}=H,
	case DungeonList of
		[]->
			transform_persistform2chapterform(T,Acc);
		_->
			Chapter = #chapter_boss_reward_info{chapterID=ChapterID,dungeonlist=transform_persistform2dungeonform(DungeonList,[])},
			transform_persistform2chapterform(T,[Chapter|Acc])
	end.
transform_persistform2dungeonform([],Acc)->
	Acc;
transform_persistform2dungeonform([H|T],Acc)->
	{DungeonID,RewardStatus}=H,
	Dungeon=#dungeon_boss_reward_info{dungeonID=DungeonID,rewardstatus=RewardStatus},
	transform_persistform2dungeonform(T,[Dungeon|Acc]).

transform_termform2persistform(-1,_Acc)->
	-1;
transform_termform2persistform([],Acc)->
	Acc;
transform_termform2persistform([H|T],Acc)->
	Battle = transform_termform2persistform2(H),
	transform_termform2persistform(T,[Battle|Acc]).
transform_termform2persistform2(H)->
	#battle_boss_reward_info{battletype=BattleType,chapterlist=ChapterList}=H,
	{BattleType,transform_chapterform2persistform(ChapterList,[])}.

transform_chapterform2persistform([],Acc)->
	Acc;
transform_chapterform2persistform([H|T],Acc)->
	#chapter_boss_reward_info{chapterID=ChapterID,dungeonlist=DungeonList}=H,
	case DungeonList of
		[]->
			transform_chapterform2persistform(T,Acc);
		_ ->
			Chapter = {ChapterID,transform_dungeonform2persistform(DungeonList,[])},
			transform_chapterform2persistform(T,[Chapter|Acc])
	end.

transform_dungeonform2persistform([],Acc)->
	Acc;
transform_dungeonform2persistform([H|T],Acc)->
	#dungeon_boss_reward_info{dungeonID=DungeonID,rewardstatus=RewardStatus}=H,
	Dungeon={DungeonID,RewardStatus},
	transform_dungeonform2persistform(T,[Dungeon|Acc]).

%%替换战斗队列中的首宠ID,首宠固定在5号位
change_fighter(Temp,MainGerTypeID,TrSpecial,SkinInfo)->
	#sc_fight_request{fighterList=FighterList} = Temp,
	NewFightList = lists:foldl(fun(#p_fighter{gerPos=GerPos}=PFighter,Acc)->
									   case GerPos of
										   1->
											   %%更换训练师专精
											   [change_trainer(PFighter,TrSpecial,SkinInfo)|Acc];
										   5->
											   %%更换主宠
											   [change_main_ger(PFighter,MainGerTypeID)|Acc];
										   _->
											   [PFighter|Acc]
									   end;
								  ({p_fighter,A,B,C,D,E,F,G,H,I,J,K,L,M},Acc) ->
									   PFighter = {p_fighter,A,B,C,D,E,F,G,H,I,J,K,L,M,0},
									   case PFighter#p_fighter.gerPos of
										   1->
											   %%更换训练师专精
											   [change_trainer(PFighter,TrSpecial,SkinInfo)|Acc];
										   5->
											   %%更换主宠
											   [change_main_ger(PFighter,MainGerTypeID)|Acc];
										   _->
											   [PFighter|Acc]
									   end
							   end,[],FighterList),
	Temp#sc_fight_request{fighterList=NewFightList}.

change_trainer(Fighter,#trSpecial{specialID=TrSpecialID,trID=TrID}=_TrSpecial,#skin_info{equip=Equip})->
	Fighter#p_fighter{gerHpMax=Equip,gerID=(TrID bsl 32) bor TrSpecialID}.
change_main_ger(Fighter,MainGerTypeID)->
	Fighter#p_fighter{gerTypeID=MainGerTypeID}.
	

%%修改玩家BOSS宝箱状态
test_change_role_battle_boss_status_for_payGuide(RoleID,DungeonType,DungeonIDList,Status)->
	case role_lib:is_online(RoleID) of
		true->
			case catch role_lib:send_server(RoleID, {test_change_role_battle_boss_status_for_payGuide,DungeonType,DungeonIDList,Status}) of
        		{'EXIT',_}->
            		?INFO("修改玩家关卡boss宝箱状态失败RoleID:~w DungeonType:~w DungeonIDList:~w Status:~w ~n",[RoleID,DungeonType,DungeonIDList,Status]),
            		false;
        		_ ->
            		do_change_offline_role_battle_boss_box_status_for_payGuide(RoleID,DungeonType,DungeonIDList,Status)
            end;
        false->
        	do_change_offline_role_battle_boss_box_status_for_payGuide(RoleID,DungeonType,DungeonIDList,Status)               
  	end.

do_change_online_role_battle_boss_box_status(DungeonType,DungeonIDList,Status)->
	lists:foreach(fun(DungeonID)->
		case get_dungeon_status(DungeonID,DungeonType) of
			?DUNGEON_BOSS_REWARD_ACCEPTED_TYPE->
				update_battle_boss_reward_info2(DungeonType,DungeonID,Status,true);
			_->
				ignore
		end
	end,DungeonIDList).

do_change_offline_role_battle_boss_box_status_for_payGuide(RoleID,DungeonType,DungeonIDList,Status)->
	Sql = io_lib:format("select unit from gPayGuide where roleID = ~w",[RoleID]),
	case db_sql:get_row(Sql) of
		[Unit] when is_binary(Unit)->
			case db_sql:to_term(Unit) of
				X when is_record(X,payGuideInfo)->
					%%成长计划数据已经被重置，需要更新对应宝箱状态
					do_change_offline_role_battle_boss_box_status(RoleID,DungeonType,DungeonIDList,Status);
				OldUnit->
					%%成长计划数据未被重置，直接修改对应的成长计划数据
					#p_payGuide_unit{task1ID=Task1ID} = OldUnit,
					AcceptedIDList = generate_acceptedIDlist(Task1ID),
					NewPayGuideInfo = #payGuideInfo{currenttaskid=Task1ID,unaccepttasklist=[OldUnit],acceptedtaskIDList=AcceptedIDList,showtaskID=Task1ID},
					db_sql:set_payGuide(RoleID,NewPayGuideInfo)
			end;
		_ ->
			ignore
	end.

do_change_offline_role_battle_boss_box_status(RoleID,DungeonType,DungeonIDList,Status)->
	Sql = io_lib:format("select BattleBossReward from gRoleExtra where roleID=~w;",[RoleID]),
	case db_sql:get_row(Sql) of
		[<<"-1">>]->
			ignore;
		[BattleBossRewardBin]->
			BossRewardInfoPersist = db_sql:to_term(BattleBossRewardBin),
			NewBossRewardInfoPersist = lists:foldl(fun(DungeonID,Acc)->
				change_boss_box_dungeon_type_status(Acc,DungeonType,DungeonID,Status)
			end,BossRewardInfoPersist,DungeonIDList),
			Sql2 = io_lib:format("update gRoleExtra set battlebossreward=~s where roleID=~w;",[db_sql:to_bin(NewBossRewardInfoPersist),RoleID]),
			db_sql:sql_execute_with_log(Sql2)
	end.

change_boss_box_dungeon_type_status(BossRewardInfoPersist,DungeonType,DungeonID,Status)->
	% ?ERR("BossRewardInfoPersist:~w ~n",[BossRewardInfoPersist]),
	case lists:keytake(DungeonType,1,BossRewardInfoPersist) of
		false->
			BossRewardInfoPersist;
		{_Value,{DungeonType,ChapterInfoPersist},Other}->
			NewChapterInfoPersist = change_boss_box_dungeon_chapter_status(ChapterInfoPersist,DungeonID,Status),
			[{DungeonType,NewChapterInfoPersist}|Other]
	end.
change_boss_box_dungeon_chapter_status(ChapterInfoPersist,DungeonID,Status)->
	case data_dungeon:get(DungeonID) of
		false->
			ChapterInfoPersist;
		#data_dungeon{chapterID=ChapterID}->
			case lists:keytake(ChapterID,1,ChapterInfoPersist) of
				false->
					NewDungeonList = change_boss_box_dungeon_dungeon_status([],DungeonID,Status),
					[{ChapterID,NewDungeonList}|ChapterInfoPersist];
				{_Value,{ChapterID,DungeonList},Other}->
					NewDungeonList = change_boss_box_dungeon_dungeon_status(DungeonList,DungeonID,Status),
					[{ChapterID,NewDungeonList}|Other]
			end
	end.
change_boss_box_dungeon_dungeon_status(DungeonList,DungeonID,Status)->
	case lists:keytake(DungeonID,1,DungeonList) of
		false->
			[{DungeonID,Status}|DungeonList];
		{_Value,_Find,Other}->
			[{DungeonID,Status}|Other]
	end.

generate_acceptedIDlist(Task1ID)->
	FirstID = hd(data_payGuide:get_list()),
	generate_acceptedIDlist(FirstID,Task1ID,[]).

generate_acceptedIDlist(Task1ID,Task1ID,Acc)->
	Acc;
generate_acceptedIDlist(FirstID,Task1ID,Acc)->
	#data_payGuide{task2ID=Task2ID,nextID=NextID} = data_payGuide:get(FirstID),
	generate_acceptedIDlist(NextID,Task1ID,[FirstID,Task2ID|Acc]).

test_change_all_role_battle_boss_box_status(DungeonType,DungeonIDList,Status)->
	ServerType = data_setting:get(server_type),
	case ServerType of
		normal->
			Sql = io_lib:format("select roleID from gRole;",[]),
			case db_sql:sql_execute_with_log(Sql) of
				{ok,RoleIDList}->
					[test_change_role_battle_boss_status_for_payGuide(RoleID,DungeonType,DungeonIDList,Status)||[RoleID]<-RoleIDList],
					?ERR("finish update battle boss status");
				Msg ->
					?ERR("update battle boss status error:~w ~n",[Msg])
			end;
		_ ->
			ignore
	end.

test_change_all_role_battle_boss_box_status_with_rpc(DungeonType,DungeonList,Status)->
	Nodes = [erlang:node()|erlang:nodes()],
	[rpc:call(Node,role_battle,test_change_all_role_battle_boss_box_status,[DungeonType,DungeonList,Status])||Node<-Nodes].

%%根据Skin中穿戴的皮肤ID确定训练师是一转还是二转,如果是一转皮肤则不处理
change_fighter_transmigration(FightRecord,#skin_info{equip=Equip})->
	case data_skin:get({data_skin_shape,Equip}) of
		?undefined->
			change_fighter_transmigration2(FightRecord,1);
		_->
			FightRecord
	end.
change_fighter_transmigration2(#sc_fight_request{fighterList=FighterList}=R,Transmigration)->
	NewFightList = [change_single_fighter_transmigration2(F,Transmigration)||F<-FighterList],
	R#sc_fight_request{fighterList=NewFightList};
change_fighter_transmigration2(R,_Transmigration)->
	R.

change_single_fighter_transmigration2(#p_fighter{gerTypeID=0,gerPos=Pos}=PF,Transmigration)->
	case Pos < 0 of
		true->
			PF#p_fighter{gerQuality=Transmigration};
		false->
			PF
	end;
change_single_fighter_transmigration2(PF,_Transmigration)->
	PF.
