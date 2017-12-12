%% @author 李兴龙
%% @doc 聊天处理
%% Created 2013-11-13


-module(role_challengeGod).
-compile(export_all).
-include("def_role.hrl").

%% API functions
-export([]).

%% Internal functions
-export([]).

-define(last_talk_time,last_talk_time).

%% ====================================================================
%% API functions
%% ====================================================================

cs_challengeGod_info(_)->
	#roleTimes{challengeGodEnergy=ChallengeGodEnergy, challengeGodBuyTimes=ChallengeGodBuyTimes}=role_data:get_roleTimes(),
	ChallengePos = role_data:get_challengeGod_pos(),
	Price = data_common:get(buy_challengeGod_gold),
	#role{vipLevel=VIPLevel}=Role=role_data:get_roleInfo(),
	DataVIP=data_vip:get(VIPLevel),
	ChallengeGodCanBuyTimes =DataVIP#data_vip.challengeGodTimes - ChallengeGodBuyTimes,
	?sendself(#sc_challengeGod_info{freeTimes=ChallengeGodEnergy,buyTimes=ChallengeGodCanBuyTimes,gerPos=ChallengePos, price=Price}).


%% 选武将，如果选武将和挑战时，武将位置发生变化，该位置没有武将，则从位置1开始读取一个武将,客户端如果不设置出战,则默认出战位置为2,否则根据客户端设置武将位置.
cs_challengeGod_select_ger(#cs_challengeGod_select_ger{pos=Pos})->
	PosList=role_data:get_posList(),
	Result = 
		lists:foldl(fun(#ger{gerBase=Base},Acc)->
							if Base#gerBase.gerPos =:= Pos ->
								   role_data:set_challengeGod_pos(Pos),
								   Acc+1 ;
							   true ->
								   Acc
							end
					end,1, PosList),
	?sendself(#sc_challengeGod_select_ger{result=Result}).

cs_challengeGod_challenge_dungeon_one(#cs_challengeGod_challenge_dungeon_one{dungeonID=DungeonID})->
	case check_challenge_one(DungeonID) of
		true ->
			{_Result, FightRecord, Reward3} = do_challenge_one(DungeonID),
			?sendself(#sc_challengeGod_challenge_dungeon_one{result=1,fightInfo=[FightRecord],reward=[Reward3]});
		{false, Reason} ->
			?sendself(#sc_challengeGod_challenge_dungeon_one{result=Reason, reward=[]})
	end.

cs_challengeGod_challenge_dungeon_ten(#cs_challengeGod_challenge_dungeon_ten{dungeonID=DungeonID})->
	case check_challenge_ten(DungeonID) of
		{true,Result} ->
			?sendself(#sc_challengeGod_challenge_ten{result=1,info=Result});
		{false, Reason} ->
			?sendself(#sc_challengeGod_challenge_ten{result=Reason, info=[]})
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%　challengeGodBuyTimes中的数值为购买单次挑战的次数，即挑战十次，则消耗该次数10次
check_challenge_ten(DungeonID) ->
	case lists:member(DungeonID, data_common:get(challengeGodDungeonIDList)) of
		true ->
			#roleTimes{challengeGodEnergy=ChallengeGodEnergy,challengeGodBuyTimes=ChallengeGodBuyTimes} = RoleTimes=role_data:get_roleTimes(),
			#role{vipLevel=VIPLevel}=Role=role_data:get_roleInfo(),
			DataVIP=data_vip:get(VIPLevel),
			ChallengeGodCanBuyTimes =DataVIP#data_vip.challengeGodTimes - ChallengeGodBuyTimes,
			ChallengeTime = erlang:min(ChallengeGodEnergy+ChallengeGodCanBuyTimes, 10),
			if ChallengeTime > 0 ->
				   NewChallengeGodBuyTimes = ChallengeTime - ChallengeGodEnergy,
				   NeedGold = data_common:get(buy_challengeGod_gold)*NewChallengeGodBuyTimes,
				   case role_lib:check_money(Role, gold, NeedGold) of
					   true ->
						   role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_CHALLENGEGOD, 1, ""),
						   RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy = 0,challengeGodBuyTimes = ChallengeGodBuyTimes+NewChallengeGodBuyTimes},
						   role_data:set_roleTimes(RoleTimes2),
						   Result =
							   lists:foldl(fun(_,Acc)->
												   {Result, _, Reward}=do_challenge_one(DungeonID),
												   [#p_challengeGod_result{result=Result, reward=Reward}|Acc]
										   end, [], lists:seq(1,ChallengeTime)),
						   {true, Result};
					   false ->
						   {false, 2}
				   end;
			   true ->
				   {false, 4}
			end;
		_ ->
			{false, 3}
	end.

check_challenge_one(DungeonID)->
	case lists:member(DungeonID, data_common:get(challengeGodDungeonIDList)) of
		true ->
			#roleTimes{challengeGodEnergy=ChallengeGodEnergy,challengeGodBuyTimes=ChallengeGodBuyTimes} =RoleTimes= role_data:get_roleTimes(),
			#role{vipLevel=VIPLevel}=Role = role_data:get_roleInfo(),
			if ChallengeGodEnergy > 0 ->
				   RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=ChallengeGodEnergy-1},
				   role_data:set_roleTimes(RoleTimes2),
				   true;
			   true ->
				   DataVIP=data_vip:get(VIPLevel),
				   if ChallengeGodBuyTimes < DataVIP#data_vip.challengeGodTimes ->
						  NeedGold = data_common:get(buy_challengeGod_gold),
						  case role_lib:check_money(Role, gold, NeedGold) of
							  true ->
								  role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_CHALLENGEGOD, 0, ""),
								  RoleTimes3 = RoleTimes#roleTimes{challengeGodEnergy=0, challengeGodBuyTimes=ChallengeGodBuyTimes+1},
								  role_data:set_roleTimes(RoleTimes3),
								  true;
							  false ->
								  {false, 2}
						  end;
					  true ->
						  {false, 4}
				   end
			end;
		_ ->
			{false, 3}
	end.

do_challenge_one(DungeonID) ->
	#data_dungeon{reward=Reward}=DataDungeon=data_dungeon:get(DungeonID),
	RoleFighter = get_challenge_info(),
	RoleFighter2 = 
		case RoleFighter of
			[] ->
				%lists:nth(1, role_data:get_posList());
				get_first_fighter();
			_  ->
				RoleFighter
		end,
	#role{level=RoleLevel,roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
	MonTeam = role_battle:get_mon_team(DataDungeon, RoleLevel, DataDungeon#data_dungeon.dungeon_level),
	RoleLieuAdd = role_data:get_lieu_add_attr(),

    TalentList = role_talent:get_active_talent_list(),
    ASkinInfo = role_skin:get_skininfo(RoleID),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-[RoleFighter2]],
    {Result, FightRecord, {_FinalState,_,_,_}} = role_fight:new(RoleID,[RoleFighter2], MonTeam, RoleLieuAdd, #add_attr{},TalentList,[],role_data:get_trSpecial(),#trSpecial{},false,ASkinInfo,#skin_info{},LegendAddList,[]),
	Result2 = 
		case Result of
			true ->
				{_, GerAddExpList, RewardItemList, RewardGerList,_} = role_reward:handle_dungeon_reward_f(RoleInfo, Reward, ?MONEY_ADD_TYPE_CHALLENGEGOD, DungeonID, ""),
				RewardItemView = [item_lib:new_item2p_item_view(E) || E<- RewardItemList],
				RewardGerView = [ger_lib:new_ger2p_ger_view(E) || E<- RewardGerList],
				Reward2 = role_lib:reward2p_reward(Reward),
				Reward3 = Reward2#p_reward{gerExpList=GerAddExpList, itemList=RewardItemView, gerList=RewardGerView,levelExp=0},
				1;
			false ->
				Reward3 = #p_reward{coin=0,gerExpList=[],gerList=[],gold=0,itemList=[],roleExp=0,levelExp=0},
				2
		end,
	{Result2, FightRecord, Reward3}.

get_challenge_info()->
	PosList = role_data:get_posList(),
	Pos = role_data:get_challengeGod_pos(),
	lists:foldl(fun(#ger{gerBase=Base}=Ger,Acc)->
						if Base#gerBase.gerPos =:= Pos ->
							   Ger;
						   true ->
							   Acc
						end end, [], PosList).

get_first_fighter()->
	Fighters=role_data:get_posList(),
	lists:foldl(fun(#ger{gerBase=Base}=Ger, #ger{gerBase=BaseAcc}=Acc)->
						case BaseAcc#gerBase.gerPos > Base#gerBase.gerPos of
							true ->
								Ger;
							_->
								Acc
						end
				end, hd(Fighters),Fighters).
