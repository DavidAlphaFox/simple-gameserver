-module(role_goldenegg).
-compile(export_all).
-include("def_role.hrl").
%%==========================================================
%%(1)判断玩家的积分有效期只在玩家进程获取数据时判断并且修改。
%%(2)当前设计中，未设置定时器用于清除玩家的积分
%%==========================================================
-define(RATE_INCREASE,1).
-define(REMAIN_SCORE,2).
-define(MONEY_SCORE_TYPE,12).
-define(MONEY_GOLD_TYPE,1).

-define(STATE_SMASH,1).
-define(STATE_EXCHANGE,2).
-define(STATE_CLOSE,3).

-define(SMASH_SCORE,1).
-define(SMASH_ITEM,2).
-define(SMASH_NOTHING,3).

%%============================================协议处理函数======================================================
cs_goldenegg_use_item(#cs_goldenegg_use_item{itemTypeID=ItemTypeID})->
    case catch do_goldenegg_use_item(ItemTypeID) of
        {false,Reason}->
            ?sendself(#sc_goldenegg_use_item{result=Reason,itemTypeID=ItemTypeID,useItemTypeIDList=[]});
        UseItemTypeIDList->
            ?sendself(#sc_goldenegg_use_item{result=1,itemTypeID=ItemTypeID,useItemTypeIDList=UseItemTypeIDList})
    end.

cs_goldenegg_smash(#cs_goldenegg_smash{itemTypeID=ItemTypeID,extraitemTypeIDList=ExtraItemUseList})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    case catch do_goldenegg_smash(ItemTypeID,ExtraItemUseList) of
        {false,Reason}->
            send_goldenegg_roleinfo(RoleID),
            ?sendself(#sc_goldenegg_smash{result=Reason,score=0,rewardlist=[]});
        {true,Score,RewardList,SmashResult}->
            case SmashResult of
                ?SMASH_SCORE->
                    ?sendself(#sc_goldenegg_smash{result=1,score=Score,rewardlist=RewardList});
                ?SMASH_NOTHING->
                    ?sendself(#sc_goldenegg_smash{result=4,score=Score,rewardlist=RewardList});
                ?SMASH_ITEM->
                    ?sendself(#sc_goldenegg_smash{result=6,score=Score,rewardlist=RewardList});
                _->
                    ?ERR("undefined SmashResult:~w ~n",[SmashResult]),
                    ?sendself(#sc_goldenegg_smash{result=6,score=0,rewardlist=[]})
            end,
            send_goldenegg_roleinfo(RoleID)
    end.

cs_goldenegg_roleinfo(#cs_goldenegg_roleinfo{})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    send_goldenegg_roleinfo(RoleID).

cs_goldenegg_shop(#cs_goldenegg_shop{})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    send_shopinfo(RoleID).

cs_goldenegg_open(#cs_goldenegg_open{})->
    {NextChangeStatusTime,State} = goldenegg_server:get_status(),
    ?sendself(#sc_goldenegg_open{status=State,endtimestamp=NextChangeStatusTime}).
%%==============================================================================================================

%%============================================实际处理函数======================================================
do_goldenegg_use_item(ItemTypeID)->
    case goldenegg_server:get_status() of
        {_NextChangeStatusTimes,?STATE_SMASH}->
            next;
        _ ->
            erlang:throw({false,4})
    end,
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    RoleEggInfo = goldenegg_server:get_role_egginfo(RoleID),
    case lists:member(ItemTypeID,RoleEggInfo#role_egg_info.item_use_list) of
        false->
            next;
        true->
            erlang:throw({false,3})
    end,
    case is_buff_item(ItemTypeID) of
        false->
            erlang:throw({false,6});
        true->
            next
    end,
    %%准备使用删除sell_reward的现成接口，所以此处需要将删除的道具转换成sell_reward结构
    case role_item:delete_sell_reward(#sell_reward{item=[#new_item{itemTypeID=ItemTypeID,itemLevel=1,itemRank=0,itemNum=1}]},[],[]) of
        {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
            role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_GOLDENEGG),
            ItemUseList = [ItemTypeID|RoleEggInfo#role_egg_info.item_use_list],
            goldenegg_server:set_role_egginfo(RoleEggInfo#role_egg_info{item_use_list=ItemUseList}),
            ItemUseList;
        false->
            erlang:throw({false,2})
    end.

do_goldenegg_smash(ItemTypeID,UseItemTypeIDList)->
    UseItemResult = lists:all(fun(UseItemTypeID)->
        case catch do_goldenegg_use_item(UseItemTypeID) of
            {false,_Reason}->
                false;
            _ ->
                true
        end
    end,UseItemTypeIDList),
    case UseItemResult of
        false->
            erlang:throw({false,7});
        true->
            next
    end,
    case goldenegg_server:get_status() of
        {_NextChangeStatusTimes,?STATE_SMASH}->
            next;
        _->
            erlang:throw({false,3})
    end,
    case is_smash_tool(ItemTypeID) of
        false->
            erlang:throw({false,5});
        true->
            next
    end,
    case role_item:delete_sell_reward(#sell_reward{item=[#new_item{itemTypeID=ItemTypeID,itemLevel=1,itemRank=0,itemNum=1}]},[],[]) of
        {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
            role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_GOLDENEGG),
            #role{roleID=RoleID} = role_data:get_roleInfo(),
            RoleEggInfo = goldenegg_server:get_role_egginfo(RoleID),
            {SmashResult,Score,RewardList,NewRoleEggInfo} = smash_egg(RoleEggInfo,ItemTypeID),
            goldenegg_server:set_role_egginfo(NewRoleEggInfo),
            Role=role_data:get_roleInfo(),
            role_reward:handle_sys_reward_with_return(Role, RewardList, ?MONEY_ADD_TYPE_GOLDENEGG, 0, "", true),
            {true,Score,role_reward:transform2p_reward_view(RewardList,[]),SmashResult};
        false->
            erlang:throw({false,2})
    end.

send_goldenegg_roleinfo(RoleID)->
    RoleEggInfo = goldenegg_server:get_role_egginfo(RoleID),
    ?sendself(#sc_goldenegg_roleinfo{totalscore=RoleEggInfo#role_egg_info.score,validtimestamp=RoleEggInfo#role_egg_info.validtime,useItemTypeIDList=RoleEggInfo#role_egg_info.item_use_list}).

send_shopinfo(_RoleID)->
    {ValidTimeStamp,GoodsIDList} = goldenegg_server:get_current_goodslist(),
    if
        is_list(GoodsIDList) ->
            {DynamicShopID,SolidShopID} = data_goldenegg:get(data_shopID),
            SolidShopIDList = case data_shop:get(SolidShopID) of
                ?undefined->
                    [];
                #data_shop{sellList=SellList}->
                    SellList
            end,
            DynamicGoodsList = get_goodslist(GoodsIDList,DynamicShopID),
            SolidGoodsList = get_goodslist(SolidShopIDList,SolidShopID),
            ?sendself(#sc_goldenegg_shop{result=1,goodslist=DynamicGoodsList++SolidGoodsList,validtimestamp=ValidTimeStamp});
        true->
            ?sendself(#sc_goldenegg_shop{result=2,goodslist=[],validtimestamp=0}) 
    end.

%%==============================================================================================================
%%传入玩家的砸蛋信息，以及此次使用的砸蛋道具，返回玩家新的砸蛋信息以及结果
%%砸金蛋数据
% -record(role_egg_info,{roleID=0,score=0,validtime=0,times=0,item_use_list=[]}).
smash_egg(RoleEggInfo,ItemTypeID)->
    ExtraList = get_extra_effect([ItemTypeID|RoleEggInfo#role_egg_info.item_use_list]),
    {ExtraRate,LastRemain}=lists:foldl(fun(Extra,{AccRate,RemainAcc})->
        case Extra of
            {?RATE_INCREASE,Value}->
                {(Value+AccRate),RemainAcc};
            {?REMAIN_SCORE,Value}->
                {AccRate,{Value+RemainAcc}};
            _ ->
                ?ERR("undefined Extra type:~w ~n",[Extra]),
                {AccRate,RemainAcc}
        end
    end,{0,0},ExtraList),

    #role_egg_info{times=OldTimes} = RoleEggInfo,
    {SmashResult,Score,Reward} = get_smash_result(RoleEggInfo#role_egg_info{times=OldTimes+1},ExtraRate),
    NowSec=util:now(),
    ScoreValidedInterval = data_goldenegg:get(score_valided_interval),
    case SmashResult of
        ?SMASH_SCORE->
            NewRoleEggInfo = RoleEggInfo#role_egg_info{score=Score,validtime=NowSec+ScoreValidedInterval,times=OldTimes+1,item_use_list=[]},
            goldenegg_server:set_role_egginfo(NewRoleEggInfo),
            {SmashResult,Score,Reward,NewRoleEggInfo};
        ?SMASH_ITEM->
            NewRoleEggInfo = RoleEggInfo#role_egg_info{score=Score,validtime=NowSec+ScoreValidedInterval,item_use_list=[]},
            goldenegg_server:set_role_egginfo(NewRoleEggInfo),
            {SmashResult,Score,Reward,NewRoleEggInfo};
        ?SMASH_NOTHING->
            case LastRemain > 0 of
                false->
                    NewRoleEggInfo = RoleEggInfo#role_egg_info{score=0,validtime=NowSec+ScoreValidedInterval,times=0,item_use_list=[]},
                    goldenegg_server:set_role_egginfo(NewRoleEggInfo),
                    {SmashResult,0,[],NewRoleEggInfo};
                true->
                    NewRoleEggInfo = RoleEggInfo#role_egg_info{validtime=NowSec+ScoreValidedInterval,item_use_list=[]},
                    goldenegg_server:set_role_egginfo(NewRoleEggInfo),
                    {SmashResult,0,[],NewRoleEggInfo}
            end;
        _ ->
            ?ERR("undefined SmashResult:~w ~n",[SmashResult]),
            {?SMASH_NOTHING,0,[],RoleEggInfo}
    end.
%%返回道具带来的附加属性    
get_extra_effect(ItemList)->
    lists:foldl(fun(ItemID,Acc)->
        case data_goldenegg:get({item_effect,ItemID}) of
            ?undefined->
                Acc;
            {_ID,Type,Value}->
                case lists:keytake(Type,1,Acc) of
                    false->
                        [{Type,Value}|Acc];
                    {_,{OldType,OldValue},Other}->
                        [{OldType,OldValue+Value}|Other]
                end
        end
    end,[],ItemList).

%%仅仅根据次数计算出本次的概率
get_base_rate(RoleEggInfo)->
    case data_goldenegg:get({smash_time,RoleEggInfo#role_egg_info.times}) of
        ?undefined->
            0;
        {_Time,Rate,_Score}->
            Rate
    end.

%%根据玩家砸蛋数据生成砸中奖励
get_egg_reward(RoleEggInfo)->
    Score = get_egg_score_reward(RoleEggInfo),
    ItemReward = get_egg_item_reward(RoleEggInfo),
    {Score,ItemReward}.

get_egg_score_reward(RoleEggInfo)->
    #role_egg_info{score=Score} = RoleEggInfo,
    case Score =:= 0 of
        true->
            data_goldenegg:get(data_basescore);
        false->
            data_goldenegg:get(data_increase_mul)*Score
    end.

get_egg_item_reward(RoleEggInfo)->
    %%此处没有使用砸蛋次数来获得不同的开启箱子
    case data_goldenegg:get({goldenegg_box,1}) of
        ?undefined->
            ?ERR("can not find config goldenegg_box in data_goldenegg where RoleEggInfo:~w ~n",[RoleEggInfo]),
            [];
        Config->
            R = util:random_one_from_weigh_list(Config),
            [R]
    end.

%%检查是否是buff道具
is_buff_item(ItemTypeID)->
    case data_goldenegg:get({item_effect,ItemTypeID}) of
        ?undefined->
            false;
        _ ->
            true
    end.

is_smash_tool(ItemTypeID)->
    case data_goldenegg:get(smash_toollist) of
        ?undefined->
            false;
        List->
            lists:member(ItemTypeID,List)
    end.

get_goodslist(GoodsIDList,ShopID)->
    lists:foldl(fun(GoodsID,Acc)->
        case data_sell:get(GoodsID) of
            ?undefined->
                ?ERR("can not find goods config in data_sell where goodsID:~w ~n",[GoodsID]),
                Acc;
            #data_sell{maxBuyNum=MaxBuyNum,sellReward=Goods,costType=CostType,costNum=Price}->
                BuyNum = role_shop:get_buy_num(ShopID,GoodsID),
                [#p_goods_unit{goodsID=GoodsID,goods=activity_server:sell_reward2p_reward_info(Goods),price=Price,moneytype=get_moneytype(CostType),maxbuytimes=MaxBuyNum,buytime=BuyNum,shopid=ShopID}|Acc]
        end
    end,[],GoodsIDList).

check_money_enough(Price,MoneyType,Role)->
    case MoneyType of
        ?MONEY_SCORE_TYPE->
            goldenegg_server:is_role_eggscore_enough(Price,Role#role.roleID);
        ?MONEY_GOLD_TYPE->
            Role#role.gold+Role#role.goldBonus >= Price;
        _ ->
            ?ERR("undefined MoneyType:~w ~n",[MoneyType]),
            false
    end.

deduct_money(Price,MoneyType,RoleID,Type,ArgID,Desc)->
    case MoneyType of
        ?MONEY_SCORE_TYPE->
            goldenegg_server:deduct_eggscore(RoleID,Price,Type,ArgID,Desc),
            #role_egg_info{score=Score,validtime=ValidTimeStamp} = goldenegg_server:get_role_egginfo(RoleID),
            ?sendself(#sc_goldenegg_update_score{totalscore=Score,validtimestamp=ValidTimeStamp});
        ?MONEY_GOLD_TYPE->
            RoleInfo=role_data:get_roleInfo(),
            role_lib:deduct_gold_f(RoleInfo,Price,Type,ArgID,Desc);
        _ ->
            ?ERR("undefined MoneyType:~w ~n",[MoneyType]),
            erlang:throw({false,5})
    end.

deduct_eggscore_money(Role, EggScore, Type, ArgID, Desc)->
    deduct_money(EggScore,?MONEY_SCORE_TYPE,Role#role.roleID,Type,ArgID,Desc).


send_reward(Goods)->
    Role = role_data:get_roleInfo(),
    role_reward:handle_sell_reward_f(Role, Goods, ?MONEY_ADD_TYPE_GOLDENEGG_EXCHANGE, 0, ""),
    {true,role_reward:transform2p_reward_view(Goods,[])}.

get_moneytype(Type)->
    case Type of
        eggscore->
            ?MONEY_SCORE_TYPE;
        _ ->
            ?MONEY_GOLD_TYPE
    end.
get_smash_result(RoleEggInfo,ExtraRate)->
    #role_egg_info{score=OldScore} = RoleEggInfo,
    Times = get_smash_time(OldScore),
    % ?ERR("Times:~w ~n",[Times]),
    case data_goldenegg:get({smash_time,Times}) of
        ?undefined->
            {?SMASH_NOTHING,0,[]};
        {MissRate,HitRate,BoxRate}->
            RandValue = random:uniform(MissRate+HitRate+BoxRate),
            % ?ERR("RandValue~w MissRate:~w HitRate:~w BoxRate:~w ExtraRate:~w RoleEggInfo:~w ~n",[RandValue,MissRate,HitRate,BoxRate,ExtraRate,RoleEggInfo]),
            if
                RandValue =< HitRate*ExtraRate ->
                    %%积分命中
                    % ?ERR("Score hit~n"),
                    NewScore = get_egg_score_reward(RoleEggInfo),
                    {?SMASH_SCORE,NewScore,[]}; 
                RandValue > HitRate+ExtraRate andalso RandValue =< HitRate*ExtraRate+BoxRate->
                    %%道具命中
                    % ?ERR("Item hit ~n"),
                    ItemReward = get_egg_item_reward(RoleEggInfo),
                    {?SMASH_ITEM,OldScore,ItemReward};
                true->
                    %%未命中
                    % ?ERR("nothing hit ~n"),
                    {?SMASH_NOTHING,0,[]}
            end
    end.

% {data_score_time_map,[{0,10000,1}]}.
%根据玩家当前剩余的积分返回砸蛋应该选择第几次砸蛋
get_smash_time(Score)->
    case data_goldenegg:get(data_score_time_map) of
        ?undefined->
            20;
        List->
            get_smash_time2(List,Score)
    end.
get_smash_time2(List,Score)->
    FindUnitList = lists:foldl(fun(E,Acc)->
        {Begin,End,Times} = E,
        case Score > Begin andalso Score =<End of
            true->
                [Times|Acc];
            false->
                Acc
        end
    end,[],List),
    Length = length(FindUnitList),
    if
        Length =:= 1->
            hd(FindUnitList);
        Length =:= 0->
            ?ERR("find no suit config in data_goldenegg Score:~w List:~w~n ",[Score,List]),
            20;
        true->
            ?ERR("find more than one config score:~w List:~w ~n",[Score,List]),
            hd(FindUnitList)
    end.
%test==========================================================================================
test_fix_egginfo(RoleEggInfo)->
    goldenegg_server:set_role_egginfo(RoleEggInfo).

test(ItemTypeID,UseItemList,Times)->
    RoleEggInfo = #role_egg_info{item_use_list=UseItemList},
    {NewRoleEggInfo,ScoreSuccess,ItemSuccess,Failed}=test_smash(RoleEggInfo,0,0,0,Times,ItemTypeID),
    ?ERR("NewRoleEggInfo:~w ScoreSuccess:~w ItemSuccess:~w Failed:~w ~n",[NewRoleEggInfo,ScoreSuccess,ItemSuccess,Failed]),
    {NewRoleEggInfo,ScoreSuccess,ItemSuccess,Failed}.

test_smash(RoleEggInfo,ScoreSuccess,ItemSuccess,Failed,0,_ItemTypeID)->
    {RoleEggInfo,ScoreSuccess,ItemSuccess,Failed};
test_smash(RoleEggInfo,ScoreSuccess,ItemSuccess,Failed,Times,ItemTypeID)->
    #role_egg_info{item_use_list=ItemUseList} = RoleEggInfo,
    {SmashResult,_Score,_RewardList,NewRoleEggInfo} = smash_egg(RoleEggInfo,ItemTypeID),
    case SmashResult of
        ?SMASH_SCORE->
            test_smash(NewRoleEggInfo#role_egg_info{item_use_list=ItemUseList},ScoreSuccess+1,ItemSuccess,Failed,Times-1,ItemTypeID);
        ?SMASH_ITEM->
            test_smash(NewRoleEggInfo#role_egg_info{item_use_list=ItemUseList},ScoreSuccess,ItemSuccess+1,Failed,Times-1,ItemTypeID);
        ?SMASH_NOTHING->
            test_smash(NewRoleEggInfo#role_egg_info{item_use_list=ItemUseList},ScoreSuccess,ItemSuccess,Failed+1,Times-1,ItemTypeID)
    end.