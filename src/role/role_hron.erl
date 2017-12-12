%% @author caohongyang
%% @doc 华容道 玩家处理模块
%% Created 2013-5-9


-module(role_hron).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_reward.hrl").

%% -define(SUCC_REWARD_GER, 1).
%% -define(SUCC_REWARD_ITEM, 2).
%% -define(SUCC_REWARD_GOLD, 3).
%% -define(SUCC_REWARD_SILVER, 4).
%% -define(SUCC_REWARD_REPU, 5).

-define(WIN, 1).
-define(FAIL, 2).

%% API functions
-export([]).

%% Internal functions 
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_hron_info(#cs_hron_info{}) ->
    case hron_server:is_open() of
        true ->
            #hronInfo{curDungeonNum=CurDungeonNum,isSelect=IsSelect,star=Star} = HronInfo = role_data:get_hronInfo(),
            BestHistoryScore = get_hronhistory_bestscore_by_difficultytype(Star),
            if CurDungeonNum =:= 0 ->
                   Record = #sc_hron_info_wait{bestScore=HronInfo#hronInfo.bestScore,
                                               star = HronInfo#hronInfo.star,
											   isSelect=IsSelect,
                                               challengeTimes = HronInfo#hronInfo.challengeTimes,maxBuffAdd=data_hron:get(max_moral_add)},
                   ?sendself(Record);
               true ->
                   #hronInfo{dungeonIDList=[ID1,ID2,ID3], challengeTimes=ChallengeTimes} = HronInfo,
                   Record = #sc_hron_info_on{attackAdd=HronInfo#hronInfo.attackAdd,
                                             curDungeonNum=CurDungeonNum,
                                             dungeonID1=ID1,
                                             dungeonID2=ID2,
                                             dungeonID3=ID3,
                                             hpAdd=HronInfo#hronInfo.hpAdd,
                                             morale=HronInfo#hronInfo.morale,
                                             isHaveSuccReward=HronInfo#hronInfo.isHaveSuccReward,
                                             challengeTimes=ChallengeTimes,
                                             lastFightResult=HronInfo#hronInfo.lastFightResult,
                                             historybestscore=BestHistoryScore,
                                             maxBuffAdd=data_hron:get(max_moral_add)},
                   ?sendself(Record),
                   case HronInfo#hronInfo.lastFightResult of
                       ?FAIL ->
                           Record2 = #sc_hron_info_wait{bestScore=HronInfo#hronInfo.bestScore,
                                                        star = HronInfo#hronInfo.star,
														isSelect=IsSelect,
                                                        challengeTimes = HronInfo#hronInfo.challengeTimes,maxBuffAdd=data_hron:get(max_moral_add)},
                           ?sendself(Record2);
                       _ ->
                           next
                   end
            end;
        {false, BeginTime} ->
            ?sendself(#sc_hron_info_stop{beginTime=BeginTime})
    end.

cs_hron_pass(_) ->
	{VipLevel,Level} = data_common:get(hron_pass),
	?sendself(#sc_hron_pass{level=Level,vipLevel=VipLevel}).

cs_hron_reward_view(#cs_hron_reward_view{star=Star,dungeonID=DungeonID}) ->
	case data_hron:get({succ_reward,Star,DungeonID}) of
		?undefined ->
			?sendself(#sc_hron_reward_view{result=2,reward=[]});
		Reward ->
            case activity_server:is_activity(?hron_reward_double) of
                true ->
                    Reward2 = role_reward:reward_reward_double(Reward);
                false ->
                    Reward2 = Reward
            end,
			?sendself(#sc_hron_reward_view{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward2)]})
	end.


cs_hron_select(#cs_hron_select{star=Star}) ->
	case hron_server:is_open() of
		false ->
			?sendself(#sc_hron_select{result=4});
		_ ->
			#hronInfo{isSelect=IsSelect}=Hron=role_data:get_hronInfo(),
			case IsSelect of
				1 ->
					?sendself(#sc_hron_select{result=3});
				_->
					case lists:member(Star,[1,2,3,4]) of
						false ->
							?sendself(#sc_hron_select{result=2});
						true ->
							role_data:set_hronInfo(Hron#hronInfo{star=Star,isSelect=1}),
							?sendself(#sc_hron_select{result=1})
					
					end
			end
	end.
			
			

cs_hron_open_time(_) ->
	case hron_server:is_open() of
		true ->
			#hronInfo{star=Star,isSelect=IsSelect} = role_data:get_hronInfo(),
			?sendself(#sc_hron_open_time{beginTime=0,star=Star,isSelect=IsSelect});
		{false, BeginTime} ->
%%			#role{title=Title} = role_data:get_roleInfo(),
%%			Star = cacl_star(Title),
			#hronInfo{star=Star,isSelect=IsSelect} = role_data:get_hronInfo(),
			?sendself(#sc_hron_open_time{beginTime=BeginTime,star=Star,isSelect=IsSelect})
	end.

cs_hron_info_on(#cs_hron_info_on{}) ->
	case check_hron_on() of
		{true, HronInfo} ->
			do_hron_on(HronInfo);
		{false, Reason} ->
			?sendself(#sc_hron_info_on_fail{result=Reason})
			end.

cs_hron_buy(#cs_hron_buy{type=Type,moraleNum=MoraleNum}) ->
	case check_buy(Type,MoraleNum) of
		{true, AddAttack, AddHp, HronInfo, DeductMorale} ->
			#hronInfo{attackAdd=Attack, hpAdd=Hp, morale=Morale} = HronInfo,
			NewHpAdd = Hp+AddHp,
			NewAttackAdd = Attack+AddAttack,
			NewMorale = Morale-DeductMorale,
			HronInfo2 = HronInfo#hronInfo{attackAdd=NewAttackAdd,hpAdd=NewHpAdd,morale=NewMorale},
			role_data:set_hronInfo(HronInfo2),
			?sendself(#sc_hron_buy{result=1,type=Type,morale=NewMorale,attackAdd=NewAttackAdd,hpAdd=NewHpAdd});
		{false, Reason} ->
			?sendself(#sc_hron_buy{result=Reason,type=Type,morale=0,attackAdd=0,hpAdd=0})
			end.

cs_hron_fight(#cs_hron_fight{type=Type}) ->
	case check_fight(Type) of
		{true,DataDungeon, HronInfo} ->
			do_fight(DataDungeon,HronInfo);
		{false, Reason} ->
			?sendself(#sc_hron_fight{fightInfo=[],result=Reason,rewardInfo=[]})
			end.

cs_hron_succ_reward(#cs_hron_succ_reward{}) ->
    case check_hron_succ_reward() of
        {true, RoleHronInfo, TheNth, AllRewardList} ->
            do_hron_succ_reward(RoleHronInfo, TheNth, AllRewardList);
        {false, Reason} when erlang:is_integer(Reason) ->
            ?sendself(#sc_hron_succ_reward{result=Reason,reward_list=activity_server:sell_reward2p_reward_info(#sell_reward{})})
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================
check_hron_succ_reward() ->
    #hronInfo{isHaveSuccReward=IsHaveSuccReward,star=Star} = RoleHronInfo = role_data:get_hronInfo(),
    case IsHaveSuccReward > 0 of
        true ->
            case data_hron:get({succ_reward, Star,IsHaveSuccReward}) of
				SuccReward when is_record(SuccReward,sell_reward) ->
                    case activity_server:is_activity(?hron_reward_double) of
                        true ->
                            {true, RoleHronInfo,1,role_reward:reward_plus_reward(SuccReward, SuccReward)};
                        false ->
                            {true, RoleHronInfo,1,SuccReward}
                    end;
                _ ->
                    {false, 2}
            end;
        false ->
            {false, 1}
    end.


get_hron_succ_reward(SuccQualityList) ->
    WeightSum =
        lists:foldl(fun({_, TmpWeight}, Acc) ->
                            TmpWeight + Acc
                    end, 0, SuccQualityList),
    Random = random:uniform(WeightSum),
    case
        util:foldl(
          fun({_, CurWeight}, {AccWeight, Nth}) ->
                  NewAccWeight = AccWeight + CurWeight,
                  case Random =< NewAccWeight of
                      true ->
                          {return, {ok, Nth}};
                      false ->
                          {NewAccWeight, Nth + 1}
                  end 
          end, {0, 1}, SuccQualityList) of
        {ok, TheNth} ->
            AllRewardList =
                lists:foldr(
                  fun({CurQuality, _}, Acc) ->
                          CurQualityRewardList = data_hron:get({quality,CurQuality}),
                          CurQualityReward = lists:nth(random:uniform(erlang:length(CurQualityRewardList)), CurQualityRewardList),
                          [{CurQualityReward, CurQuality}|Acc]
                  end, [], SuccQualityList),
            {ok, TheNth, AllRewardList};
        _ ->
            false
    end.

do_hron_succ_reward(RoleHronInfo, _TheNth, Reward) ->
    RoleHronInfo2 = RoleHronInfo#hronInfo{isHaveSuccReward=0},
    %GiveReward = lists:nth(TheNth, AllRewardList),
    RoleInfo = role_data:get_roleInfo(),
    %Reward = get_reward_data(GiveReward),
    role_reward:handle_sell_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_HRON_SUCC, 0, ""),
    role_data:set_hronInfo(RoleHronInfo2),
    %PRewardList = get_reward_list_to_client(TheNth, AllRewardList),
    ?sendself(#sc_hron_succ_reward{result=0, reward_list=activity_server:sell_reward2p_reward_info(Reward)}).

get_reward_list_to_client(TheNth, AllRewardList) ->
    {_, AllRewardList2} = 
        lists:foldl(
          fun({{RewardType, RewardID, RewardNum}, _}, {Nth, Acc}) ->
                  case Nth =:= TheNth of
                      true ->
                          {Nth + 1, [#p_hron_succ_reward{reward_type=RewardType, reward_id=RewardID, reward_num=RewardNum, is_get=true}|Acc]};
                      false ->
                          {Nth + 1, [#p_hron_succ_reward{reward_type=RewardType, reward_id=RewardID, reward_num=RewardNum, is_get=false}|Acc]}
                  end
          end, {1, []}, AllRewardList),
    AllRewardList2.

%% get_reward_data({{RewardType, RewardID, RewardNum}, _}) ->
%%     case RewardType of
%%         ?SUCC_REWARD_GER ->
%%             #sell_reward{coin=0,gerExp=0,gold=0,item=[],reputation=0,roleExp=0,newGer=lists:duplicate(RewardNum, #new_ger{gerTypeID=RewardID,gerLevel=1,gerQuality=0})};
%%         ?SUCC_REWARD_ITEM ->
%%             #sell_reward{coin=0,gerExp=0,gold=0,item=[#new_item{itemTypeID=RewardID,itemNum=RewardNum,itemLevel=1,itemRank=0}],reputation=0,roleExp=0,newGer=[]};
%%         ?SUCC_REWARD_GOLD ->
%%             #sell_reward{coin=0,gerExp=0,gold=RewardNum,item=[],reputation=0,roleExp=0,newGer=[]};
%%         ?SUCC_REWARD_SILVER ->
%%             #sell_reward{coin=RewardNum,gerExp=0,gold=0,item=[],reputation=0,roleExp=0,newGer=[]};
%%         ?SUCC_REWARD_REPU ->
%%             #sell_reward{coin=0,gerExp=0,gold=0,item=[],reputation=RewardNum,roleExp=0,newGer=[]}
%%     end.

do_fight(DataDungeon, HronInfo) ->
	#hronInfo{bestScore=BestScore0, curDungeonNum=CurDungeonNum, star=Star, attackAdd=AttackAdd, hpAdd=HpAdd} = HronInfo,
	DefenderList = get_mon_list(DataDungeon, CurDungeonNum),
	FighterList = role_data:get_fighter_list(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
	AttackerList = add_buff(FighterList, AttackAdd,HpAdd),
	RoleID = role_data:get_roleID(),
  ASkinInfo = role_skin:get_skin_info(),
  TalentList = role_talent:get_active_talent_list(),
  LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-AttackerList],
    AttackerList2 = role_xbattle:get_add_buff(role_data:get_xbattle_data(),AttackerList),
  {Result, FightRecord, _} = role_fight:new(RoleID,AttackerList2, DefenderList,RoleLieuAdd, #add_attr{},TalentList,[],role_data:get_trSpecial(),#trSpecial{}, false,ASkinInfo,#skin_info{},LegendAddList,[]),
	if Result =:= true ->
		   % 胜利了
			
		   % 普通关卡的发奖励
		   #data_dungeon{reward=Reward,dungeonID=DungeonID} = DataDungeon,
		   #role{roleID=RoleID,roleName=Name} = Role = role_data:get_roleInfo(),
		   {_, GerAddExpList, RewardItemList, RewardGerList,_} = role_reward:handle_dungeon_reward_f(Role, Reward, ?MONEY_ADD_TYPE_HRON_CHALLENGE, DungeonID, ""),
		   RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
		   RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
		   Reward2 = role_lib:reward2p_reward(Reward),
		   Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView,levelExp=0},
		   
		   % 华容道结算
		   NewDungeonNum = CurDungeonNum+1,
		   [ID1,ID2,ID3] = DungeonList = random_dungeon(Star),
		   
		   BestScore = erlang:max(CurDungeonNum,BestScore0),
		   Morale2 = HronInfo#hronInfo.morale + DataDungeon#data_dungeon.addMorale,
           IsHaveSuccReward =
               case data_hron:get({succ_reward, Star,CurDungeonNum}) of
                   SuccReward when is_record(SuccReward,sell_reward) ->
                       CurDungeonNum;
                   _ ->
                       0
               end,
		   HronInfo2 = HronInfo#hronInfo{curDungeonNum=NewDungeonNum,dungeonIDList=DungeonList,bestScore=BestScore,morale=Morale2,
                                         isHaveSuccReward=IsHaveSuccReward,lastFightResult=?WIN},
		   role_data:set_hronInfo(HronInfo2),
		   %% 在排行榜上更新排名
		   if CurDungeonNum > BestScore0 ->
				  hron_server:cast_update_score(RoleID, Name, Star, BestScore, false),
          update_hron_history(Star,BestScore);
			  true ->
				  ignore
		   end,
		   ?sendself(#sc_hron_fight{fightInfo=[FightRecord],rewardInfo=[Reward3],result=1}),
        BestHistoryScore = get_hronhistory_bestscore_by_difficultytype(Star),
		   ?sendself(#sc_hron_info_on{attackAdd=HronInfo#hronInfo.attackAdd,
									  curDungeonNum=NewDungeonNum,
									  dungeonID1=ID1,
									  dungeonID2=ID2,
									  dungeonID3=ID3,
									  hpAdd=HronInfo#hronInfo.hpAdd,
									  morale=Morale2,
                                      isHaveSuccReward=IsHaveSuccReward,
                                      challengeTimes=HronInfo#hronInfo.challengeTimes,
                                      lastFightResult=?WIN,
                                      historybestscore=BestHistoryScore,
                                      maxBuffAdd=data_hron:get(max_moral_add)}),
		   ?CATCH_1(role_task_trigger:handle({dispach_task,role_pass_hron,CurDungeonNum}),E3);
	   true ->
           NewChallengeTimes = HronInfo#hronInfo.challengeTimes - 1,
           NewMorale =
               case NewChallengeTimes =< 0 of
                   true ->
                       HronInfo#hronInfo.morale;
                   false ->
                       FailAddMorale = data_hron:get(fail_add_morale),
                       HronInfo#hronInfo.morale + FailAddMorale
               end,
		    HronInfo2 = HronInfo#hronInfo{challengeTimes=NewChallengeTimes,morale=NewMorale,lastFightResult=?FAIL},
		    role_data:set_hronInfo(HronInfo2),
        BestHistoryScore = get_hronhistory_bestscore_by_difficultytype(Star),
		   ?sendself(#sc_hron_fight{fightInfo=[FightRecord],rewardInfo=[],result=1}),
           [ID1, ID2, ID3] = HronInfo2#hronInfo.dungeonIDList,
           ?sendself(#sc_hron_info_on{attackAdd=HronInfo2#hronInfo.attackAdd,
                                      curDungeonNum=HronInfo2#hronInfo.curDungeonNum,
                                      dungeonID1=ID1,
                                      dungeonID2=ID2,
                                      dungeonID3=ID3,
                                      hpAdd=HronInfo2#hronInfo.hpAdd,
                                      morale=HronInfo2#hronInfo.morale,
                                      isHaveSuccReward=HronInfo2#hronInfo.isHaveSuccReward,
                                      challengeTimes=HronInfo2#hronInfo.challengeTimes,
                                      lastFightResult=?FAIL,
                                      historybestscore=BestHistoryScore,
                                      maxBuffAdd=data_hron:get(max_moral_add)}),
		   RoleID = role_data:get_roleID(),
		   hron_server:cast_rank_sync(RoleID,Star)
	end,
	?CATCH(role_task_trigger:handle({dispach_task,role_join_hron})).


%% 动态计算华容道的怪物
get_mon_list(DataDungeon, CurDungeonNum) ->
	Rank = get_mon_rank(CurDungeonNum),
	#data_dungeon{gerID2=MonList,gerID3=IDList} = DataDungeon,
	F = fun(E) ->
				GerTypeID = E#mon.gerTypeID,
				ger_attr:new_mon(GerTypeID, E#mon.gerLevel, Rank, [], lists:delete(GerTypeID, IDList))
		end,
	[?change_pos(Ger, Pos)||{Pos, Mon} <- MonList, is_record(Mon, mon), Ger<-[F(Mon)], is_record(Ger, ger)].
	
%% 计算怪物的品阶
get_mon_rank(CurDungeonNum) when CurDungeonNum < 10 -> (1.1+0.05*(CurDungeonNum-1))*CurDungeonNum;
get_mon_rank(CurDungeonNum) -> 13.5+1.85*(CurDungeonNum-9).
	
	

%% 加buff
add_buff(FighterList, 0,0) ->
	FighterList;
add_buff(FighterList, AttackAdd,HpAdd) ->
	[add_buff2(Ger, AttackAdd,HpAdd) || Ger<- FighterList].

add_buff2(Ger, AttackAdd, HpAdd) ->
	#ger{gerAttr=GerAttr}=Ger,
	#gerAttr{gerAttack=GerAttack, gerHpMax=GerHpMax} = GerAttr,
	if AttackAdd > 0 ->
		   Attack2 = trunc(GerAttack*(1+AttackAdd/100));
	   true ->
		   Attack2 = GerAttack
	end,
	if HpAdd > 0 ->
		   GerHpMax2 = trunc(GerHpMax*(1+HpAdd/100));
	   true ->
		   GerHpMax2 = GerHpMax
	end,
	GerAttr2 = GerAttr#gerAttr{gerAttack=Attack2,gerHpMax=GerHpMax2},
	Ger#ger{gerAttr=GerAttr2,gerHp=GerHpMax2}.

	

check_fight(Type) ->
    case hron_server:is_open() of
        true ->
            #hronInfo{curDungeonNum=CurDungeonNum,dungeonIDList=DungeonIDList, challengeTimes=ChallengeTimes, isHaveSuccReward=IsHaveSuccReward} = HronInfo = role_data:get_hronInfo(),
            case IsHaveSuccReward > 0 of
                true ->
                    {false, 7};
                false ->
                    case ChallengeTimes =< 0 of
                        true ->
                            {false, 6};
                        false ->
                            case CurDungeonNum >= 1 andalso is_list(DungeonIDList) andalso Type=<3 andalso Type >=1 of
                                true ->
                                    DungeonID = lists:nth(Type, DungeonIDList),
                                    case data_dungeon:get(DungeonID) of
                                        #data_dungeon{}=DataDungeon ->
                                            {true,DataDungeon, HronInfo};
                                        _ ->
                                            {false, 5}
                                    end;
                                false ->
                                    {false, 3} 
                            
                            end
                    end
            end;
        _ ->
            {false, 2}
    end.

check_buy(Type,MoraleNum) ->
	case hron_server:is_open() of
		true ->
			#hronInfo{morale=Morale, curDungeonNum=CurDungeonNum,hpAdd=_HpAdd,attackAdd=_AttackAdd} = HronInfo = role_data:get_hronInfo(),
			if CurDungeonNum >= 1 ->
				   case data_hron:get({morale, Type}) of
				   {DeductMorale, AddAttack, AddHp} ->
					   if Morale >= DeductMorale andalso Morale >= MoraleNum->
                AddTimes = MoraleNum div DeductMorale,
                % TotalAddAttack = AddAttack*AddTimes,
                % TotalAddHp     = AddHp * AddTimes,
            %     case TotalAddHp > data_hron:get(max_moral_add) orelse TotalAddAttack > data_hron:get(max_moral_add) of
            %       false->
							     % {true, AddAttack*AddTimes, AddHp*AddTimes, HronInfo, DeductMorale*AddTimes};
            %       true->
            %         {false,6}
            %     end;
                {true, AddAttack*AddTimes, AddHp*AddTimes, HronInfo, DeductMorale*AddTimes};
						  true ->
							  {false, 2}
					   end;
					   _ ->
						   {false, 5}
				   end;
			   true ->
				   {false, 4}
			end;			
		_ ->
			{false, 3}
	end.

do_hron_on(HronInfo) ->
	#hronInfo{star=Star} = HronInfo,
	[ID1,ID2,ID3] = DungeonList = random_dungeon(Star),
    MaxTimes = data_hron:get(max_challenge_times),
	HronInfo2 = HronInfo#hronInfo{challengeTimes=MaxTimes,attackAdd=0,curDungeonNum=1,dungeonIDList=DungeonList,hpAdd=0,morale=0},
	role_data:set_hronInfo(HronInfo2),
  BestHistoryScore = get_hronhistory_bestscore_by_difficultytype(Star),
	?sendself(#sc_hron_info_on{attackAdd=0,curDungeonNum=1,dungeonID1=ID1,dungeonID2=ID2,dungeonID3=ID3,hpAdd=0,morale=0,isHaveSuccReward=0,
                               challengeTimes=HronInfo2#hronInfo.challengeTimes,lastFightResult=0,historybestscore=BestHistoryScore,maxBuffAdd=data_hron:get(max_moral_add)}).

random_dungeon(Star) ->
	{Config1, Config2, Config3} = data_hron:get({dungeon,Star}),
	[util:random_one_from_list(Config1),
	 util:random_one_from_list(Config2),
	 util:random_one_from_list(Config3)].
	
check_hron_on() ->
    case hron_server:is_open() of
        true ->
            #hronInfo{challengeTimes=ChallengeTimes, curDungeonNum=CurDungeonNum,isHaveSuccReward=IsHaveSuccReward} = HronInfo = role_data:get_hronInfo(),
            case CurDungeonNum =:= 0 of
                true ->
                    case ChallengeTimes > 0 of
                        true ->
                            case IsHaveSuccReward =:= 0 of
                                true ->
                                    {true, HronInfo};
                                false ->
                                    {false, 4}
                            end;
                        false ->
                            {false, 2}
                    end;
                false ->
                    {false, 3}
            end;
        _ ->
            {false, 1}
    end.


%% 计算难度星级
cacl_star(Title) ->
	[Star4Title, Star3Title, Star2Title, _] = data_hron:get(title_cond),
	if Title >= Star4Title ->
		   4;
	   Title >= Star3Title ->
		   3;
	   Title >= Star2Title ->
		   2;
	   true ->
		   1
	end.

update_hron_history(DifficultyType,NewScore)->
  HronHistory = role_data:get_hronhistory(),
  case lists:keytake(DifficultyType,#hron_history_info.difficultytype,HronHistory) of
    false->
      NewHronHistory = [#hron_history_info{difficultytype=DifficultyType,bestscore=NewScore}|HronHistory],
      role_data:set_hronhistory(NewHronHistory);
    {_Value,#hron_history_info{bestscore=BestScore,difficultytype=DifficultyType}=Find,Other}->
      case BestScore>=NewScore of
        true->
          ignore;
        false->
          NewHronHistory = [Find#hron_history_info{bestscore=NewScore}|Other],
          role_data:set_hronhistory(NewHronHistory),
          ?sendself(#sc_hron_update_history{star=DifficultyType,besthistoryscore=NewScore}),
          %%触发对玩家无尽深渊关卡任务的判定
          role_payGuide:trigger_task_change(?HRON_REACH_N,{0})
      end
  end.

get_hronhistory_bestscore_by_difficultytype(DifficultyType)->
  case role_data:get_hronhistory() of
    []->
      0;
    HronHistory->
      case lists:keyfind(DifficultyType,#hron_history_info.difficultytype,HronHistory) of
        false->
          0;
        #hron_history_info{bestscore=BestScore}->
          BestScore
      end
  end.

cs_hron_raids(#cs_hron_raids{})->
  HronInfo = role_data:get_hronInfo(),
  HronHistory = role_data:get_hronhistory(),
  case check_hron_raids(HronInfo,HronHistory) of
    {false,Reason}->
      ?sendself(#sc_hron_raids{result=Reason,nextdungeonID=0,newmorale=0,rewardInfo=[]});
    {true,NextDungeonID,NewMorale,Reward}->
      RewardList = do_hron_raids(NextDungeonID,NewMorale,Reward),
      ?sendself(#sc_hron_raids{result=1,nextdungeonID=NextDungeonID,newmorale=NewMorale,rewardInfo=RewardList})
  end.

check_hron_raids(HronInfo,HronHistory)->
  #hronInfo{star=Star,curDungeonNum=CurDungeonNum,morale=Morale,isHaveSuccReward=IsHaveSuccReward}=HronInfo,
  case IsHaveSuccReward =:= 0 of
    false->
      {false,3};
    true->
      case lists:keyfind(Star,#hron_history_info.difficultytype,HronHistory) of
        false->
          {false,2};
        #hron_history_info{bestscore=BestScore}->
          case BestScore>data_hron:get(radis_min_limit) of
            true->
              case check_hron_raids2(CurDungeonNum,BestScore) of
                {false,Reason}->
                  {false,Reason};
                {true,RaidsDungeonDest}->
                  {RewardList,AddMorale} = generate_radis_reward(CurDungeonNum,RaidsDungeonDest),
                  % ?ERR("CurDungeonNum:~w RaidsDungeonDest:~w RewardList:~w AddMorale:~w ~n",[CurDungeonNum,RaidsDungeonDest,RewardList,AddMorale]),
                  {true,RaidsDungeonDest,Morale+AddMorale,RewardList}
              end;
            false->
              {false,4}
          end
      end
  end.
check_hron_raids2(CurDungeonNum,MaxDungenNum)->
  case CurDungeonNum >=MaxDungenNum of
    true->
      {false,5};
    false->
      List = data_hron:get(radis_field_list),
      MaxDungenNum2 = erlang:max(MaxDungenNum-data_hron:get(unradis_limit),0),
      Result = util:fun_find(fun({Begin,End})->
        MaxDungenNum2>=Begin andalso MaxDungenNum2 =< End
      end,List),
      case Result of
        {_LBegin,LEnd}->
          case CurDungeonNum >= LEnd of
            true->
              {false,5};
            false->
              {true,LEnd+1}
          end;
        _->
          Rem = MaxDungenNum2 rem 10,
          Exe = case Rem =:= 0 of true->0;false->1 end,
          DesDungeonID = (MaxDungenNum2 div 10 +Exe)*10+1,
          case DesDungeonID > CurDungeonNum of
            true->
              {true,DesDungeonID};
            false->
              {false,5}
          end
      end
  end.
generate_radis_reward(CurDungeonNum,RaidsDungeonDest)->
  #hronInfo{star=Star} = role_data:get_hronInfo(),
  #role{familyID = FamilyID} = role_data:get_roleInfo(),
  FamilyTek_Battleout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,9,1),
  generate_radis_reward(CurDungeonNum,RaidsDungeonDest,{#sell_reward{},0},Star,FamilyTek_Battleout_Add).

generate_radis_reward(_RaidsDungeonDest,_RaidsDungeonDest,Acc,_Star,_FamilyTek_Battleout_Add)->
  Acc;
generate_radis_reward(CurDungeonNum,RaidsDungeonDest,{RewardAcc,MoraleAcc}=Acc,Star,FamilyTek_Battleout_Add)->
  %%按照困难关卡生成通关的关卡，从而确定掉落和勇气
  DungeonID = lists:nth(3,random_dungeon(Star)),
  case data_dungeon:get(DungeonID) of
    ?undefined->
      ?ERR("can not find DungeonID:~w~n",DungeonID),
      generate_radis_reward(CurDungeonNum+1,RaidsDungeonDest,Acc,Star,FamilyTek_Battleout_Add);
    DungeonData->
      {Reward,AddMorale} = generate_radis_single_reward(DungeonData,FamilyTek_Battleout_Add,CurDungeonNum,Star),
      NewAcc = role_reward:reward_plus_reward(Reward,RewardAcc),
      generate_radis_reward(CurDungeonNum+1,RaidsDungeonDest,{NewAcc,MoraleAcc+AddMorale},Star,FamilyTek_Battleout_Add)
  end.

do_hron_raids(NextDungeonID,NewMorale,RewardList)->
  %%更新无尽深渊数据
  HronInfo=#hronInfo{star=Star,bestScore=BestScore0}= role_data:get_hronInfo(),
  DungeonList = random_dungeon(Star),
  BestScore = NextDungeonID-1,
  NewHronInfo = HronInfo#hronInfo{curDungeonNum=NextDungeonID,morale=NewMorale,bestScore=BestScore,dungeonIDList=DungeonList},
  role_data:set_hronInfo(NewHronInfo),
  %%更新无尽深渊历史数据
  update_hron_history(Star,NextDungeonID),
  %%更新无尽深渊排行榜
  if 
    BestScore > BestScore0 ->
      #role{roleID=RoleID,roleName=RoleName}=role_data:get_roleInfo(),
      hron_server:cast_update_score(RoleID, RoleName, Star, BestScore, false);
    true ->
      ignore
  end,
  %%发放无尽深渊扫荡奖励
  Role = role_data:get_roleInfo(),
  % ?ERR("Reward:~w~n",[RewardList]),
  role_reward:handle_sys_reward_with_return(Role,RewardList,?MONEY_ADD_TYPE_HRON_RAIDS,NextDungeonID,""),
  PRewardInfo = activity_server:sell_reward2p_reward_info(RewardList),
  ?CATCH(role_task_trigger:handle({dispach_task,role_join_hron})),
  role_recycle:transformPRewardInfo2PRewardInfoList(PRewardInfo,[]).

generate_radis_single_reward(#data_dungeon{dungeonID=DungeonID,reward=Reward,addMorale=AddMorale},FamilyTekBattleAdd,CurDungeonNum,Star)->
  #reward{coin=AddCoin0,gerExp=AddGerExp,roleExp=AddRoleExp0,gold=AddGold,dropList=DropList,reputation=AddRepu}=Reward,
  % %% 计算公会科技对产出金币与经验的影响
  AddCoin2 = role_lib:calculate_familyTekeffectGenerate(AddCoin0,FamilyTekBattleAdd),
  AddRoleExp2 = role_lib:calculate_familyTekeffectGenerate(AddRoleExp0,FamilyTekBattleAdd),
  %% DropList是data_drop ID的列表
  %% ArgID 是 DungeonID 
  RandomSelect = role_reward:random_drop(DropList),
  RandomSelectBonus = role_reward:getDropBonus(DungeonID),
  {RewardItemList, RewardGerList} = role_reward:partition_drop(RandomSelectBonus++RandomSelect),
  RandomDropReward = #sell_reward{coin=AddCoin2,roleExp=AddRoleExp2,gerExp=AddGerExp,gold=AddGold,item=RewardItemList,reputation=AddRepu,newGer=RewardGerList},
  case data_hron:get({succ_reward,Star,CurDungeonNum}) of
    ?undefined->
      {RandomDropReward,AddMorale};
    SuccessReward->
      {role_reward:reward_plus_reward(RandomDropReward,SuccessReward),AddMorale}
  end.

test_refresh_hronInfo(RoleID,Msg)->
    case catch role_lib:send_server(RoleID, {refresh_hroninfo,Msg}) of
                {'EXIT',_}->
                    ?INFO("修改玩家:~w 无尽深渊信息失败~n",[RoleID]);
                _ ->
                    ignore                            
    end.

do_refresh_info({refresh_hroninfo,Msg})->
  role_data:set_hronInfo(Msg).

test_set_role_hronhistory(RoleID,Type,Score)->
  case catch role_lib:send_server(RoleID, {test_set_role_hronhistory,Type,Score}) of
    {'EXIT',_}->
      ?INFO("修改玩家:~w 无尽深渊历史数据失败~n",[RoleID]);
    _ ->
      ignore                            
  end.

do_test_set_role_hronhistory(Star,Score)->
  HronHistory = role_data:get_hronhistory(),
  NewHronHistory = case lists:keytake(Star,#hron_history_info.difficultytype,HronHistory) of
    false->
      [#hron_history_info{difficultytype=Star,bestscore=Score}|HronHistory];
    {_Value,_Find,Other}->
      [#hron_history_info{difficultytype=Star,bestscore=Score}|Other]
  end,
  role_data:set_hronhistory(NewHronHistory).