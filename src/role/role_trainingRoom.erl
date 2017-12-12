%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 十一月 2017 18:46
%%%-------------------------------------------------------------------
-module(role_trainingRoom).
-author("chenlong").

-include("def_role.hrl").

%% API
-export([cs_trainingRoom_info/1, cs_trainingRoom_start_training/1]).

-export([checkLoad/0, getPlayerTrainingRoomData/0]).

-define(PlayerTrainingRoom, playerTrainingRoom).

%%获取训练室界面信息
cs_trainingRoom_info(#cs_trainingRoom_info{}) ->
	#playerTrainingRoom{chapterID = ChapterID, leftHP = LeftHP} = getPlayerTrainingRoomData(),
	Msg = #sc_trainingRoom_info{chapterID = ChapterID, leftHP = LeftHP},
	?sendself(Msg).

%%开始训练
cs_trainingRoom_start_training(#cs_trainingRoom_start_training{energy = CostEnergy}) ->
	try
		case CostEnergy > 0 of
			?TRUE -> ok;
			_ -> throw(2)
		end,
		%%检测条件
		#roleTimes{energy = Energy} = RoleTimes = role_data:get_roleTimes(),
		case Energy >= CostEnergy of
			?TRUE -> ok;
			_ -> throw(2)
		end,
		%%扣除消耗,记录消耗体力原因码
		role_lib:deduct_energy_f(role_data:get_roleInfo(), RoleTimes, CostEnergy, ?MONEY_DEC_TYPE_TRAINING_ROOM, 0, ""),
		%%执行训练
		AwardList = doTraining(CostEnergy),
		MsgFunc = fun({Index, ChapterID, AddCoin, AddExp, BoxAwardList, BossBoxAward, CurCostEnergy}) ->
			#p_trainingRoom_reward{index = Index, chapterID = ChapterID, coin = AddCoin, roleExp = AddExp,
				rateReward = [activity_server:sell_reward2p_reward_info(BoxAward) || BoxAward <- BoxAwardList],
				bossReward = [activity_server:sell_reward2p_reward_info(BossBoxAward)],
				energy = CurCostEnergy}
		          end,
		MsgAwardList = lists:map(MsgFunc, AwardList),
		Msg = #sc_trainingRoom_start_training{result = 1, rewardList = MsgAwardList},
		?sendself(Msg)
	catch
		ErrorCode ->
			?sendself(#sc_trainingRoom_start_training{result = ErrorCode, rewardList = []});
		_:Why ->
			?ERR("start_training CostEnergy=~w,Why=~w,stack=~p~n", [CostEnergy, Why, erlang:get_stacktrace()]),
			?sendself(#sc_trainingRoom_start_training{result = 2, rewardList = []})
	end.

checkLoad() ->
	case getPlayerTrainingRoomData() of
		#playerTrainingRoom{} -> ok;
		_ ->
			RoleID = role_data:get_roleID(),
			Data = case db_sql:getTrainingRoom(RoleID) of
				       [_RoleID, ChapterID, LeftHP] ->
					       #playerTrainingRoom{chapterID = ChapterID, leftHP = LeftHP};
				       _ -> initPlayerTrainingRoom()
			       end,
			put(?PlayerTrainingRoom, Data)
	end.

getPlayerTrainingRoomData() ->
	get(?PlayerTrainingRoom).

%%=======================LOCAL FUNC==================================
initPlayerTrainingRoom() ->
	refreshPlayerTrainingRoom().

refreshPlayerTrainingRoom() ->
	CurChapterID = role_data:get_xbattle_chapterID(),
	ConfigData = data_train_room:get({train_boss, CurChapterID}),
	BossHP = element(1, ConfigData),
	InitData = #playerTrainingRoom{chapterID = CurChapterID, leftHP = BossHP},
	put(?PlayerTrainingRoom, InitData),
	InitData.

%%执行训练，返回奖励列表
doTraining(CostEnergy) ->
	CurChapterID = role_data:get_xbattle_chapterID(),
	doTraining_1(CostEnergy, 1, CurChapterID).
doTraining_1(CostEnergy, _Index, _CurMaxChapterID) when CostEnergy =< 0 ->
	[];
doTraining_1(CostEnergy, Index, CurMaxChapterID) ->
	#playerTrainingRoom{chapterID = ChapterID, leftHP = LeftHP} = getPlayerTrainingRoomData(),
	{CoinRate, ExpRate, _} = data_train_room:get({train_reward, CurMaxChapterID}),
	Role = role_data:get_roleInfo(),
	MergeFunc = fun(SellRewardB,TSellReward) -> role_reward:reward_plus_reward(SellRewardB,TSellReward) end,
	case CostEnergy >= LeftHP of
		?TRUE ->%%可以击杀稻草人
			%%计算出当前稻草人的奖励
			AddCoin = CoinRate * LeftHP,
			AddExp = ExpRate * LeftHP,
			BoxAwardList = calcBoxAwardList(CurMaxChapterID, LeftHP),
			LeftCostEnergy = CostEnergy - LeftHP,
			BossConfigData = data_train_room:get({train_boss, ChapterID}),
			BossBoxAward = element(2, BossConfigData),
			%%刷新下一个稻草人
			refreshPlayerTrainingRoom(),
			%%发放奖励
			SellReward1 = #sell_reward{roleExp = AddExp,coin = AddCoin},
			FinalAward = lists:foldl(MergeFunc, ?NULL_AWARD, [SellReward1, BossBoxAward|BoxAwardList]),
			role_reward:handle_sell_reward_f(Role,FinalAward,?MONEY_ADD_TYPE_TRAININGROOM,LeftHP,""),
			[{Index, ChapterID, AddCoin, AddExp, BoxAwardList, BossBoxAward, LeftHP} | doTraining_1(LeftCostEnergy, Index + 1, CurMaxChapterID)];
		_ ->
			AddCoin = CoinRate * CostEnergy,
			AddExp = ExpRate * CostEnergy,
			BoxAwardList = calcBoxAwardList(CurMaxChapterID, CostEnergy),
			util:updateProcDictDataProperty(?PlayerTrainingRoom,#playerTrainingRoom.leftHP,LeftHP-CostEnergy),
			%%发放奖励
			SellReward1 = #sell_reward{roleExp = AddExp,coin = AddCoin},
			FinalAward = lists:foldl(MergeFunc, ?NULL_AWARD, [SellReward1|BoxAwardList]),
			role_reward:handle_sell_reward_f(Role,FinalAward,?MONEY_ADD_TYPE_TRAININGROOM,CostEnergy,""),
			[{Index, ChapterID, AddCoin, AddExp, BoxAwardList, ?NULL_AWARD, CostEnergy}]
	end.

calcBoxAwardList(CurMaxChapterID, CostEnergy) ->
	ConfigData = data_train_room:get({train_reward, CurMaxChapterID}),
	{BoxRate, BoxID} = element(3, ConfigData),
	calcBoxAwardList1(1, CostEnergy, BoxRate, BoxID).
calcBoxAwardList1(Max, Max, BoxRate, BoxID) ->
	BoxAward = calcBoxAwardSingle(BoxRate, BoxID),
	util:getTernaryValue(BoxAward == ok, [], [BoxAward]);
calcBoxAwardList1(I, Max, BoxRate, BoxID) ->
	BoxAward = calcBoxAwardSingle(BoxRate, BoxID),
	case BoxAward of
		ok -> calcBoxAwardList1(I + 1, Max, BoxRate, BoxID);
		_ -> [BoxAward | calcBoxAwardList1(I + 1, Max, BoxRate, BoxID)]
	end.
calcBoxAwardSingle(BoxRate, BoxID) ->
	RandInt = util:random_int(1, 10000),
	case BoxRate >= RandInt of
		?TRUE ->
			RandAwardList = data_train_room:get({train_box, BoxID}),
			element(2, hd(util:random_weigh_list(RandAwardList, 1)));
		_ -> ok
	end.

%%-----------------------