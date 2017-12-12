%% @author lixinglong
%% @doc 汉帝宝库功能
%% Created 2013-4-23
-module(role_treaHouse).
-compile(export_all).
-include("def_role.hrl").
-include("def_weibo_share.hrl").
-include("def_activity_rank.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

-define(bomob_clo, 11).
-define(double_clo, 12).
-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
%% ====================================================================
%% API functions
%% ====================================================================

%% 免费抽取次数的恢复与神将录的恢复走相同流程
%% 如果id发生改变,即
cs_treaHouse_get_list(#cs_treaHouse_get_list{}) ->
    {EndTime, StopTime} = get_end_time(),

    {_,OneTimeNeedGold} = data_treasure_box:get(oneTimeCost),
    RefreshNeedCoin = data_treasure_box:get(refresh_cost),
    #treaHouseInfo{value_info=ValueInfo, card_list=CardList} = TreaHouseInfo = role_data:get_treaHouseInfo(),
    case check_enter_treaHouse() of
        true ->
            ActivityID = activityRank_server:get_treaHouse_activityID(),
            ActivityID2 = TreaHouseInfo#treaHouseInfo.activityID,
            if CardList =:= [] orelse ActivityID =/= ActivityID2->
                    CardList2 = random_treaHouse_list(),
                    TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{activityID=ActivityID,card_list=CardList2},
                    role_data:set_treaHouseInfo(TreaHouseInfo2),
                    ?sendself(#sc_treaHouse_get_list{isOpen=1,endTime=EndTime,
                            cardList=treaHouseCard2p_treaHouse_card(CardList2),
                            freeTimes = TreaHouseInfo#treaHouseInfo.free_times,
                            boxProcess = 0,boxOpenProcess=[],stopTime=StopTime,
                            oneTimeNeedGold=OneTimeNeedGold, refreshNeedCoin=RefreshNeedCoin, valueinfo=1});
                true ->
                    BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
                    ?sendself(#sc_treaHouse_get_list{isOpen=1,endTime=EndTime,
                            cardList=treaHouseCard2p_treaHouse_card(CardList),
                            freeTimes = TreaHouseInfo#treaHouseInfo.free_times,
                            boxProcess = BoxProcess,stopTime=StopTime,
                            boxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
                            oneTimeNeedGold=OneTimeNeedGold, refreshNeedCoin=RefreshNeedCoin, valueinfo=ValueInfo})
            end;
        _ ->
            IsOpen = 
            if EndTime == 0 ->
                    CardList2=[],
                    2;
                true ->
                    if CardList == [] ->
                            CardList2 = random_treaHouse_list();
                        true ->
                            CardList2=CardList
                    end,
                    3
            end,
            BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
            ?sendself(#sc_treaHouse_get_list{isOpen=IsOpen,endTime=EndTime,cardList=treaHouseCard2p_treaHouse_card(CardList2),
                    freeTimes = 0,boxProcess = BoxProcess,stopTime=StopTime,
                    boxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
                    oneTimeNeedGold=0, refreshNeedCoin=0, valueinfo=1})
    end.

cs_treaHouse_is_open(#cs_treaHouse_is_open{}) ->
    case check_enter_treaHouse2() of
        true ->
            {Type,Icon} = data_treasure_box:get(activity_info),
            ?sendself(#sc_treaHouse_is_open{type = 1,cost=Type,icon=Icon});
        _ ->
            ?sendself(#sc_treaHouse_is_open{type = 2,cost=0,icon=0})
    end.

cs_treaHouse_explore_one(#cs_treaHouse_explore_one{})->
    case check_enter_treaHouse() of
        true ->
            case check_treaHouse_explore_one() of
                {true, #treaHouseInfo{mark=OldMark}=TreaHouseInfo, SpecialID,NeedType,NeedGold}->
                    {Info,TreaHouseInfo2,Reward} = do_treaHouse_explore_one(TreaHouseInfo, SpecialID, 0),
%					role_lib:deduct_gold_2_f(role_data:get_roleInfo(), NeedGold, ?MONEY_DEC_TYPE_TREAHOUSE_EXPLORE
%											, 0, "",role_reward:typed_reward_transform_normal([X||{_,X}<-Reward])),
                    role_lib:deduct_money_f(role_data:get_roleInfo(), NeedType,NeedGold, ?MONEY_DEC_TYPE_TREAHOUSE_EXPLORE
                                            , 0, ""),%,role_reward:typed_reward_transform_normal([X||{_,X}<-Reward])),
                    role_data:set_treaHouseInfo(TreaHouseInfo2),
                    do_reward(Reward,1),
                    #role{roleID=RoleID,roleName=RoleName} = role_data:get_roleInfo(),
                    Mark = TreaHouseInfo2#treaHouseInfo.mark,
                    %%由于抽到双倍的当次不增加探险值,就没有必要去刷新排行榜
                    case OldMark =:= Mark of
                        true ->
                            ignore;
                        _ ->
                            activityRank_server:update_treaHouse_roleRank(RoleID, RoleName, Mark)
                    end,
                    BoxProcess = data_treasure_box_baseReward:get(Mark),
                    ?sendself(#sc_treaHouse_explore_one{type=1,mark=Mark,boxProcess=BoxProcess,info=[Info]});
                {false, Reason} ->
                    ?sendself(#sc_treaHouse_explore_one{type=Reason,mark=0,boxProcess=0,info=[]})
            end;
        _ ->
            ?sendself(#sc_treaHouse_explore_one{type=2,mark=0,boxProcess=0,info=[]})
    end.

cs_treaHouse_explore_ten(#cs_treaHouse_explore_ten{})->
    case check_enter_treaHouse() of
        true ->
            case check_treaHouse_explore_ten() of
				{true, OpenTimes,NeedType,NeedGold} ->
					#treaHouseInfo{mark=OldMark} = role_data:get_treaHouseInfo(),
					{InfoList, TreaHouseInfo2,RewardList} = do_treaHouse_explore_ten(OpenTimes),
%					role_lib:deduct_gold_2_f(role_data:get_roleInfo(), NeedGold , ?MONEY_DEC_TYPE_TREAHOUSE_EXPLORE
%										   , OpenTimes, "",role_reward:typed_reward_transform_normal([X||{_,X}<-RewardList])),
                    role_lib:deduct_money_f(role_data:get_roleInfo(),NeedType, NeedGold , ?MONEY_DEC_TYPE_TREAHOUSE_EXPLORE
                                           , OpenTimes, ""),%,role_reward:typed_reward_transform_normal([X||{_,X}<-RewardList])),
					do_reward(RewardList,2),
					Mark = TreaHouseInfo2#treaHouseInfo.mark,
                    BoxProcess = data_treasure_box_baseReward:get(Mark),
                    role_data:set_treaHouseInfo(TreaHouseInfo2),
                    #role{roleID=RoleID,roleName=RoleName} = role_data:get_roleInfo(),
                    case OldMark =:= Mark of
                        true ->
                            ignore;
                        _ ->
                            activityRank_server:update_treaHouse_roleRank(RoleID, RoleName, Mark)
                    end,
                    ?sendself(#sc_treaHouse_explore_ten{type=1, mark=Mark, openTimes=OpenTimes, boxProcess=BoxProcess,infoList=lists:reverse(InfoList)});
                {false, Reason} ->
                    ?sendself(#sc_treaHouse_explore_ten{type=Reason, mark=0, openTimes=0,boxProcess=0,infoList=[]})
            end;
        _ ->
            ?sendself(#sc_treaHouse_explore_ten{type=2, mark=0, openTimes=0, boxProcess=0, infoList=[]})
    end.

cs_treaHouse_refresh(#cs_treaHouse_refresh{})->
    case check_enter_treaHouse() of
        true ->
            case check_treaHouse_refresh() of
                true ->
                    CardList = do_treaHouse_refresh(),
                    ?sendself(#sc_treaHouse_refresh{type=1, cardList=treaHouseCard2p_treaHouse_card(CardList)});
                {false, Reason} ->
                    ?sendself(#sc_treaHouse_refresh{type=Reason, cardList=[]})
            end;
        _ ->
            ?sendself(#sc_treaHouse_refresh{type=2, cardList=[]})
    end.

cs_treaHouse_open_base_box(#cs_treaHouse_open_base_box{pos=Pos})->
    case check_enter_treaHouse2() of
        true ->
            case check_open_treaHouse_baseBox(Pos) of
                {true, TreaHouseInfo,_BoxOpenProcess} ->
                    BoxOpenProcess2=do_open_treaHouse_baseBox(Pos, TreaHouseInfo),
                    ?sendself(#sc_treaHouse_open_base_box{type=1, boxOpenProcess=BoxOpenProcess2});
                {false, Reason,BoxOpenProcess} ->
                    ?sendself(#sc_treaHouse_open_base_box{type=Reason, boxOpenProcess=BoxOpenProcess})
            end;
        _ ->
            ?sendself(#sc_treaHouse_open_base_box{type=2, boxOpenProcess=0})
    end.

cs_treaHouse_get_baseBoxRewardInfo(#cs_treaHouse_get_baseBoxRewardInfo{})->
    RewardList = data_treasure_box:get(base_reward),
    InfoList=lists:foldr(fun(0,Acc)->
                Acc;
            (E,Acc)->
                Pos = data_treasure_box_baseReward:get(E),
                {_,Reward} = lists:keyfind(Pos, 1, RewardList),
                [#p_treaHouse_BaseReward_Info{pos=Pos,needMark=E, rewardInfo=activity_server:sell_reward2p_reward_info(Reward)}
                    |Acc]
        end, [], data_treasure_box_baseReward:get_list()),
    %InfoList = [activity_server:sell_reward2p_reward_info(E)||{_,E}<-RewardList],
    ?sendself(#sc_treaHouse_get_baseBoxRewardInfo{baseReaward_boxInfoList=InfoList}).

cs_treaHouse_get_rankInfo(#cs_treaHouse_get_rankInfo{})->
    #treaHouseInfo{mark=Mark} = role_data:get_treaHouseInfo(),
    {Type, IsGetRankReward, SelfInfo, RankInfo} = activityRank_server:get_treaHouse_rankInfo(role_data:get_roleID(), Mark),
    ?sendself(#sc_treaHouse_get_rankInfo{type=Type, isGetRankReward=IsGetRankReward, selfInfo=SelfInfo, rankInfoList=RankInfo}).

%% ====================================================================
%% Internal functions
%% ====================================================================

do_open_treaHouse_baseBox(BoxPos, TreaHouseInfo) ->
    BaseBoxGetList = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
    BoxList = data_treasure_box:get(base_reward),
    {BoxPos, Reward} = lists:keyfind(BoxPos, 1, BoxList),
    Role = role_data:get_roleInfo(),
    role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_TREAHOUSE_BASEBOX, BoxPos, ""),
    TreaHouseInfo2=TreaHouseInfo#treaHouseInfo{baseBoxGetList=[#p_baseBoxOpenInfo{pos=BoxPos, isOpen=2}|BaseBoxGetList]},
    role_data:set_treaHouseInfo(TreaHouseInfo2),
    TreaHouseInfo2#treaHouseInfo.baseBoxGetList.

check_open_treaHouse_baseBox(Pos) ->
    TreaHouseInfo = role_data:get_treaHouseInfo(),
    BoxProcess = data_treasure_box_baseReward:get(TreaHouseInfo#treaHouseInfo.mark),
    BoxOpenProcess = TreaHouseInfo#treaHouseInfo.baseBoxGetList,
    if Pos > BoxProcess orelse Pos == 0 ->
            {false, 3, BoxOpenProcess};
        true ->
            case lists:keyfind(Pos, #p_baseBoxOpenInfo.pos, BoxOpenProcess) of
                false ->
                    {true, TreaHouseInfo,BoxOpenProcess};
                _ ->
                    {false, 5, BoxOpenProcess}
            end
    end.

do_treaHouse_refresh()->
    #treaHouseInfo{card_list=CardList} = TreaHouseInfo = role_data:get_treaHouseInfo(),
    CardList2 = random_treaHouse_list(CardList),
    TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{card_list=CardList2},
    role_data:set_treaHouseInfo(TreaHouseInfo2),
    CardList2.

check_treaHouse_refresh()->
    Role = role_data:get_roleInfo(),
    RefreshNeedCoin = data_treasure_box:get(refresh_cost),
    case role_lib:check_money(Role, coin, RefreshNeedCoin) of
        false ->
            {false, 3};
        true ->
            role_lib:deduct_coin_f(Role, RefreshNeedCoin, ?MONEY_DEC_TYPE_TREAHOUSE_REFRESH, 0, ""),
            true
    end.

do_treaHouse_explore_ten(OpenTimes)->
	TreaHouseInfo = role_data:get_treaHouseInfo(),
	lists:foldl(fun(_,{InfoAcc, TreaHouseInfoAcc,RewardAcc})->
						#treaHouseInfo{buy_count=BuyCount} = TreaHouseInfoAcc,
						TreaHouseInfoAcc2 = TreaHouseInfoAcc#treaHouseInfo{buy_count=BuyCount+1},
						SpecialID = data_fixed_treasure_gold:get(BuyCount + 1),
						{InfoT, TreaHouseInfoT,Reward} = do_treaHouse_explore_one(TreaHouseInfoAcc2,SpecialID,0),
%% 						role_lib:deduct_gold_f(role_data:get_roleInfo(), OneTimeNeedGold , ?MONEY_DEC_TYPE_TREAHOUSE_EXPLORE
%% 												, OpenTimes, "",role_reward:typed_reward_transform_normal(Reward)),
%% 						do_reward(Reward,2),
						{[InfoT|InfoAcc], TreaHouseInfoT,Reward++RewardAcc}
				end, {[], TreaHouseInfo,[]}, lists:seq(1, OpenTimes)).


check_treaHouse_explore_ten() ->
    Role = role_data:get_roleInfo(),
    {NeedType,OneTimeNeedGold} = data_treasure_box:get(oneTimeCost), 
    OpenTimes = calc_open_times(NeedType,Role, OneTimeNeedGold),
    case OpenTimes > 1 of
        false ->
            {false, 3};
        true ->
            {true, OpenTimes,NeedType,OpenTimes*OneTimeNeedGold}
    end.

calc_open_times(gold,#role{gold=GoldA, goldBonus=GoldB}, OneTimeNeedGold)->
    Gold = GoldA + GoldB,
    if Gold >= OneTimeNeedGold * 10 ->
            10;
        true ->
            trunc(Gold/OneTimeNeedGold)
    end;
calc_open_times(coin, #role{coin=Coin},OneTimeNeedGold) ->
    erlang:min(Coin div OneTimeNeedGold, 10);
calc_open_times(reputation, #role{reputation=Coin},OneTimeNeedGold) ->
    erlang:min(Coin div OneTimeNeedGold, 10);
calc_open_times(ticket, #role{ticket=Coin},OneTimeNeedGold) ->
    erlang:min(Coin div OneTimeNeedGold, 10).

check_treaHouse_explore_one()->
    #treaHouseInfo{free_times=FreeTimes, free_count=FreeCount, buy_count=BuyCount} = TreaHouseInfo = role_data:get_treaHouseInfo(),
    if FreeTimes > 0 ->
            SpecialID = data_fixed_treasure_free:get(FreeCount + 1),
            {true, TreaHouseInfo#treaHouseInfo{free_times=FreeTimes-1, free_count=FreeCount+1}, SpecialID,gold,0};
        true ->
            Role = role_data:get_roleInfo(),
            {NeedType,NeedGold} = data_treasure_box:get(oneTimeCost),
            case role_lib:check_money(Role, NeedType, NeedGold) of
                false ->
                    {false,3};
                true ->
                    SpecialID = data_fixed_treasure_gold:get(BuyCount + 1),
                    {true, TreaHouseInfo#treaHouseInfo{buy_count=BuyCount+1}, SpecialID,NeedType,NeedGold}
            end
    end.

do_treaHouse_explore_one(TreaHouseInfo,SpecialID,Count )->
    if Count < 3 -> %% 计数防止死循环
            PosList = data_treasure_box:get(pos_random),
            Pos = 
            case SpecialID of
                X when is_integer(X) ->
                    X;
                _ ->

                    %util:random_one_from_list(PosList)
                    util:random_one_from_weigh_list(PosList)
            end,
            #treaHouseInfo{value_info=ValueInfo,card_list=CardList, mark=Mark} = TreaHouseInfo,
            {value, {_, {Type, _Count, _}}=Val, CardList2} = lists:keytake(Pos, 1, CardList),
            BaseMark = data_treasure_box:get(add_mark),
	        NewValueInfo = calc_value_info(ValueInfo, Type),
            %% 双倍和爆灯 一直保留,不刷新
            case Type of
                ?bomob_clo ->
                    NewCloset = Val,
                    %% 爆灯和双倍只有一个有效,所以如果抽取到的不是爆灯,则直接计算,如果抽取到的是爆灯,则判断是否是双倍状态,是双倍状态,重新抽取,如果不是双倍状态,则按概率随机抽取非双倍的pos
                    if ValueInfo > 1 ->
                            do_treaHouse_explore_one(TreaHouseInfo, ?undefined, Count+1);
                        true ->
                            %% 随机取出四个,然后选出不是双倍的三个
                            %% 						 OpenPosListT=util:random_list2(lists:delete(Pos, data_treasure_box:get(pos_list)),4),
                            %% 						 OpenPosList = lists:sublist(lists:keydelete(?double_clo, 1, OpenPosListT), 3),
                            OpenPosList = random_bomb_pos(Pos, 3, PosList, CardList),
                            {OpenList, NewValueInfo, ReFillList, NewCardList} = calc_bomb_info(CardList, OpenPosList, ValueInfo),
                            Reward = calc_reward(ValueInfo, Type, OpenList),
                            %% 抽到连续转动的当次, 经验值加3
                            TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{value_info=NewValueInfo, card_list=NewCardList, mark=Mark + BaseMark * 3},
                            Info = cacl_explore_one_return([Val|OpenList], [NewCloset|ReFillList]),
                            {Info, TreaHouseInfo2, Reward}
                    end;
                _ ->
                    OpenList = [Val],
                    Reward = calc_reward(ValueInfo, Type, OpenList),
                    %% 抽到双倍奖励的当此不增加探险值, 直到下次才增加双倍
                    case Type =:= ?double_clo of
                        true ->
                            AddMark = 0,
                            NewCloset = Val;
                        _ ->
                            AddMark = BaseMark * ValueInfo,
                            NewCloset = get_one_box(Pos)
                    end,
                    ReFillList = [NewCloset],
                    TreaHouseInfo2 = TreaHouseInfo#treaHouseInfo{value_info=NewValueInfo, card_list=[NewCloset|CardList2], mark=Mark + AddMark},
                    Info = cacl_explore_one_return(OpenList, ReFillList),
                    {Info, TreaHouseInfo2,Reward}
            end;
        true ->
            ?ERR("bad config in treaHouse, get bomb and double at the same time for too many times"),
            Info = cacl_explore_one_return([],[]),
            {Info, TreaHouseInfo, []}
    end.

random_bomb_pos(Pos, N, RandomList, CardList)->
    random_bomb_pos(Pos, N, RandomList, CardList, [],0).

random_bomb_pos(Pos, N, RandomList, CardList, List,Count)->
    case length(List) >= N orelse Count >= 20 of
        true ->
            List;
        _ ->
            TPos = util:random_one_from_weigh_list(RandomList),
            case lists:member(TPos, List) of
                true ->
                    random_bomb_pos(Pos, N, RandomList, CardList, List,Count+1);
                _ ->
                    {_,{Type2, _,_}}=lists:keyfind(TPos, 1, CardList),
                    if Type2 =/= ?bomob_clo andalso Type2 =/= ?double_clo ->
                            random_bomb_pos(Pos, N, RandomList, CardList, [TPos|List],Count+1);
                        true ->
                            random_bomb_pos(Pos, N, RandomList, CardList, List,Count+1)
                    end
            end
    end.

cacl_explore_one_return(OpenList, RefillList)->
    OpenCardList = treaHouseCard2p_treaHouse_card(OpenList),
    ReFillList = treaHouseCard2p_treaHouse_card(RefillList),
    #p_treaHouse_card_oneTime{openCardList=OpenCardList, newCardList=ReFillList}.

do_reward(RewardList,Type)->
    %% 对于高品质的精灵和装备需要显示全屏公告,并且带上奖励数量,
    %% 所以这里需要对RewardList进行预处理,将相同奖励合并
    RewardList2 = 
        lists:foldl(fun({_, {R, C, V}=Val},Acc) ->
                        case lists:member(Val, Acc) of
                            false ->
                                [Val|Acc];
                            _ ->
                                Acc2 = lists:delete(Val, Acc),
                                [{R, C + C, V}|Acc2]
                        end
                end, [], RewardList),
    lists:foreach(fun({RewardType, Count, Value}) ->
                    %%?ERR("奖品是:~p.~n", [{RewardType, Count, Value}]),
                    role_reward:handle_treaHouse_reward_f(RewardType, Value, Type, Count)
                  end, RewardList2).

calc_bomb_info(CardList, PosList,ValueInfo)->
    %	{OpenList, ValueInfo2, ReFillList, NewCardList} = 
    lists:foldl(fun(Pos,{Acc1,Acc2, Acc3,CardListAcc})->
                {value, {_, {Type,_Count, _}}=Val, CardList2} = lists:keytake(Pos, 1, CardListAcc),
                NewCloset = get_one_box(Pos),
                if Type == ?bomob_clo ->
                        ?ERR("ERR config in treaHouse , too many bombs"),
                        {Acc1,Acc2,[NewCloset|Acc3],[NewCloset|CardList2]};
                    Type == ?double_clo ->
                        %?ERR("?ERR config in treaHouse, find double in bomb"),
                        {Acc1, calc_value_info(ValueInfo, Type), [NewCloset|Acc3],[NewCloset|CardList2]};
                    true ->
                        {[Val|Acc1],ValueInfo, [NewCloset|Acc3],[NewCloset|CardList2]}
                end
        end, {[],ValueInfo,[],CardList}, PosList).

calc_reward(ValueInfo, Type, ValList)->
    if Type == ?double_clo ->
            [];
        true ->
            lists:foldl(fun(E,Acc)->
                            lists:duplicate(ValueInfo,E) ++ Acc
                        end, [], ValList)
    end.

calc_value_info(ValueInfo, Type) ->
    if Type =:= ?double_clo ->
            case ValueInfo of
                2 ->
                    4;
                4 ->
                    8;
                8 ->
                    8;
                _ ->
                    2
            end;
        true ->
            1
    end.


check_enter_treaHouse()->
    #role{level=Level} = role_data:get_roleInfo(),
    case Level >= data_treasure_box:get(level_limit) of
        true ->
            check_open();
        _ ->
            false
    end.

check_open()->
    activityRank_server:check_treaHouse_open_state().

check_enter_treaHouse2()->
    #role{level=Level} = role_data:get_roleInfo(),
    case Level >= data_treasure_box:get(level_limit) of
        true ->
            case check_open2() of
                true ->
                    true ;
                _ ->
                    false
            end;
        _ ->
            false
    end.

check_open2()->
    activityRank_server:check_treaHouse_open_state2().

get_end_time()->
    EndTime = activityRank_server:get_treaHouse_activity_end_time(),
    case is_tuple(EndTime) of
        true ->
            EndTime;
        _ ->
            {0,0}
    end.

random_treaHouse_list()->
    PosList = data_treasure_box:get(pos_list),
    [get_one_box(P)||P<-PosList].

%% 刷新时保留原来的双倍和连续转动
%% 且其他类型要求不能和之前的重复
random_treaHouse_list(CardList) ->
    NewCardList = 
        lists:foldl(fun({Pos, {Type, _, _}=CardInfo}=Info, AccList) ->
                        case Type =:= ?bomob_clo orelse Type =:= ?double_clo of
                            true ->
                                [Info|AccList];
                            _ ->
                                [get_one_box(Pos, [CardInfo])|AccList]
                        end
                    end, [], CardList),
    lists:reverse(NewCardList).

get_one_box(Pos)->
    get_one_box(Pos, []).

get_one_box(Pos, MaskList) ->
    RandomList = data_treasure_box_setting:get(Pos),
    BoxList = util:random_one_from_weigh_list(RandomList),
    BoxList2 = BoxList -- MaskList,
    BoxList3 =
        case BoxList2 of
            [] ->
                BoxList;
            _ ->
                BoxList2
        end,
    Box = util:random_one_from_list(BoxList3),
    {Pos, Box}.

treaHouseCard2p_treaHouse_card(CardList)->
    PosBaseID = data_treasure_box:get(pos_baseID),
    [
        begin
                %%双倍和爆灯客户端需要显示两个物品的图标,CardValue为对应的ID
                if Type =:= 11 ->
                        PosType = 2, CardType = 0, Count=1, CardValue = 32013;
                    Type =:= 12 ->
                        PosType = 3, CardType = 0, Count=1, CardValue = 32012;
                    true ->
                        PosType = 1, CardType = Type , Count=Cnt, CardValue = Value
                end,
                #p_treaHouse_card{pos=Pos-PosBaseID, posType = PosType,cardType=CardType, count=Count,value=CardValue}
        end
        ||{Pos, {Type, Cnt, Value}}<-CardList].
