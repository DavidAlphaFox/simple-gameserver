%% @author lijunfeng
%% @doc 背包、装备、道具功能
%% Created 2015-3-12

-module(role_talent).
-compile(export_all).
-include("def_role.hrl").
-export([]).

%说明：
%1、升级天赋，由于有冷却时间。升级成功时，增加等级值；在需要天赋生效时判断是不是在冷却中，冷却中则降级使用或不使用。

-define(pd_last_recacl_talent,pd_last_recacl_talent).

cs_talent_get_info(_) ->
    TalentList = role_data:get_trainer_info(),
    Record = #sc_talent_get_info{talent_list=to_p_talent_list(TalentList)
                                ,remains_point=get_remains_point(TalentList)
                                ,unlock_level=data_trainer:get(talent_unlock_level)},
    ?sendself(Record).

cs_talent_study(#cs_talent_study{talent_type = TalentType}) ->
    case check_can_study(TalentType) of
        {true,NewTalent,OtherNewTalentList,NeedConsume,NeedTime} ->
            %消耗品是否足够
            case checkConsume(NeedConsume,TalentType) of
                true->
                    role_data:set_trainer_info([NewTalent|OtherNewTalentList]),
                    {Type,Level,Cd} = NewTalent,
                    ?notify_update(?ra_remains_point(get_remains_point())),
                    ?INFO("set timer to recacl_talent ~w",[NeedTime]),
                    role_payGuide:trigger_task_change(?ACTIVATE_TALENT,{0}),
%% 取消了天赋升级冷却
%%                     erlang:send_after((NeedTime+1)*1000, self(), recacl_talent),
                    erlang:send(self(), recacl_talent),
                    ?sendself(#sc_talent_study{result=1
                                    ,new_talent=#p_talent{talent_id=Type+Level,cool_down_time=Cd}
                                    ,need_gold=0
                                    ,can_buy_times=0});
                false->
                    ?sendself(#sc_talent_study{result=6
                            ,new_talent = #p_talent{talent_id=0,cool_down_time=0}
                            ,need_gold=0
                            ,can_buy_times=0})
            end;
        {false,4,NeedGold,VipBuyTime} ->
            ?sendself(#sc_talent_study{result=4
                            ,new_talent = #p_talent{talent_id=0,cool_down_time=0}
                            ,need_gold=NeedGold
                            ,can_buy_times=VipBuyTime});
        {false,Reason} ->
            ?sendself(#sc_talent_study{result=Reason
                            ,new_talent = #p_talent{talent_id=0,cool_down_time=0}
                            ,need_gold=0 
                            ,can_buy_times=0})
    end.

cs_talent_undo(#cs_talent_undo{talent_type=TalentType}) ->
    NowSecond = util:now(),
    TalentList = role_data:get_trainer_info(),
    ?INFO("cs_talent_undo TalentType:~w TalentList:~w",[TalentType,TalentList]),
    case lists:keytake(TalentType, 1, TalentList) of
        {value, {_,Level,CoolDown}, OtherTalentList} when (TalentType rem 100) =:= 0 ->
            case check_undo(TalentType,Level,OtherTalentList,TalentList,NowSecond) of
                %升级1级学习中，或者1级学习完毕
                true when Level == 1 ->
                    role_data:set_trainer_info(OtherTalentList),
                    ?notify_update(?ra_remains_point(get_remains_point())),
                    recacl_talent(),
                    ?sendself(#sc_talent_undo{result=1});
                %非1级
                true when Level >= 1 ->
                    NewList = [{TalentType,Level-1,0}|OtherTalentList],
                    role_data:set_trainer_info(NewList),
                    ?notify_update(?ra_remains_point(get_remains_point())),
                    recacl_talent(),
                    ?sendself(#sc_talent_undo{result=1});
                false->
                    ?sendself(#sc_talent_undo{result=3})
            end;
        false ->
            ?sendself(#sc_talent_undo{result=2})
    end.

cs_talent_cooldown(#cs_talent_cooldown{talent_type=TalentType})->
    NowSecond = util:now(),
    TalentList = role_data:get_trainer_info(),    
    case role_role:check_buy_energy(7,1) of
        _  when (TalentType rem 100) =/= 0 ->
            ?sendself(#sc_talent_cooldown{result=2});
        {false,_} ->
            ?sendself(#sc_talent_cooldown{result=5}); %购买次数上线了
        {true ,_RoleTimes, RoleInfo, _NeedGold, _Today, NewBuyTimes, RoleTimes2, NewValue,Add} ->
            case lists:keytake(TalentType, 1, TalentList) of
                {value, {_,_,Cooldown}, TalentListOther} when NowSecond >= Cooldown->
                    ?sendself(#sc_talent_cooldown{result=3}); %不必冷却
                {value, {_,Level,Cooldown}, TalentListOther} ->
                    NeedGold = ((Cooldown - NowSecond +59) div 60) * data_trainer:get(gold_per_min),
                    ?INFO("cs_talent_cooldown NeedGold:~w ,i have ~w+~w",[NeedGold,RoleInfo#role.gold,RoleInfo#role.goldBonus]),
                    if
                        NeedGold > (RoleInfo#role.gold+RoleInfo#role.goldBonus) ->
                            ?sendself(#sc_talent_cooldown{result=4}); %没钱了
                        true ->
                            #role{roleID=RoleID,vipLevel=VipLevel} = RoleInfo,
                            role_data:set_roleTimes(RoleTimes2), %% RoleTimes2是一个元组
                            behavior_log_times:log(RoleID, VipLevel, NewBuyTimes, NewValue, Add,7),
                            RoleInfo2 = role_lib:deduct_gold_f(RoleInfo,NeedGold,?MONEY_DEC_TYPE_TALENT_COOLDOWN_COST,TalentType,""),
                            NewList = [{TalentType,Level,0}|TalentListOther],
                            role_data:set_trainer_info(NewList),
                            recacl_talent(),
                            ?sendself(#sc_talent_cooldown{result=1})
                    end;
                false ->
                    ?sendself(#sc_talent_cooldown{result=2}) %没这个天赋
            end
    end.

%% 检查是否符合学习条件，会调用checkTalentList
check_can_study(TalentType)->
    NowSecond = util:now(),
    TalentList = role_data:get_trainer_info(),
    
    {OldLevel,OldCoolDown,TalentListOther} = case lists:keytake(TalentType, 1, TalentList) of
        {value, {_,Level0,CoolDownTime0}, TalentListOther0} ->
            {Level0,CoolDownTime0,TalentListOther0};
        false ->
            {0,0,TalentList}
    end,
    
    #role{level = Level,vipLevel=VipLevel} = role_data:get_roleInfo(),
    TotalRolePoint = lists:max([0,(Level-29)]) * data_trainer:get(point_per_level),
    PointWithStudying = lists:sum([Level0||{_,Level0,_}<-TalentList]),
    
    ActiveTalentList = to_active_talent_list(TalentList,NowSecond),
    ActivePoint = lists:sum([Level00||{_,Level00}<-ActiveTalentList]),
    
    ?INFO("TalentType:~w OldLevel:~w cd:~w",[TalentType,OldLevel,OldCoolDown]),
    MaxLevel = if
                    OldLevel > 0 ->
                        CurTalentConfig = data_talent:get(TalentType+OldLevel),
                        CurTalentConfig#data_talent.max_level;
                    true -> %即将等级为1级，所以不必判断是否到达上限
                        99999
               end,
    
    if
        (TalentType rem 100) =/= 0 ->
            {false,2}; %已经学习天赋总值不符合条件
        OldCoolDown > NowSecond ->            
            case role_role:check_buy_energy(7,1) of
                {false,_} ->
                    {false,5}; %不能买了，已经达到购买上限了
                {true ,_RoleTimes, _RoleInfo, _NeedGold, _Today, _NewBuyTimes, _RoleTimes2, _NewValue,_Add} ->
                    DataVIP = data_vip:get(VipLevel),
                    {false,4,((OldCoolDown - NowSecond +59) div 60) * data_trainer:get(gold_per_min),DataVIP#data_vip.talentstudyBuyTimes} %学习冷却中,包括OldCoolDownTime2=0的情况
            end;
        OldLevel =:= MaxLevel ->
            {false,3}; %这个天赋已经满级
        true->
            StudyTalentId = TalentType+OldLevel+1,
            NextTalentConfig = data_talent:get(StudyTalentId),
            ?INFO("ActiveTalentList:~w ActivePoint:~w",[ActiveTalentList,ActivePoint]),
            case NextTalentConfig of
                ?undefined ->
                    ?ERR("can find this talent config(~w)",[StudyTalentId]),
                    {false,2}; %已经学习天赋总值不符合条件
                _ ->
                    NeedPoint = NextTalentConfig#data_talent.need_point,
                    NeedTalentList = NextTalentConfig#data_talent.need_talent,
                    if
                        TotalRolePoint =< PointWithStudying ->
                            {false,5}; %天赋点用光了，不能再学了
                        ActivePoint < NeedPoint ->
                            ?INFO("check_can_study ~w ~w ~n ~w",[ActivePoint,NeedPoint,ActiveTalentList]),
                            {false,2}; %已经学习天赋总值不符合条件
                        true ->
                            %是否符合前置条件
                            case checkTalentList(NeedTalentList,ActiveTalentList) of
                                true ->
                                    {true,{TalentType,OldLevel+1,NextTalentConfig#data_talent.cooldown+NowSecond},TalentListOther,NextTalentConfig#data_talent.consume,0};
                                false ->
                                    {false,2} %前置天赋不足
                            end
                    end
            end
    end.
        
    
    

%% 检查天赋前置条件是否符合
checkTalentList(NeedTalentList,TalentListOther)->
    lists:foldl(fun({Type,NeedLevel},Acc)-> 
                    case Acc of
                        true ->
                            case lists:keytake(Type, 1, TalentListOther) of
                                {value, {_,ActiveLevel}, _} ->
                                    if
                                        ActiveLevel >= NeedLevel ->
                                            true;
                                        true->
                                            false
                                    end;
                                false->
                                    false
                            end;
                        false ->
                            false
                    end
                end, true, NeedTalentList).

%% 检查材料是否符合要求，需要考虑组合消费，当返回true时，已经完成消费逻辑
checkConsume(NeedConsume,TalentType)->
    RoleInfo = role_data:get_roleInfo(),
    #sell_reward{coin=NeedCoin,gold=NeedGold,reputation=NeedRep,item=NeedItemList}=NeedConsume,
    if
        NeedCoin > RoleInfo#role.coin 
          orelse  NeedGold > RoleInfo#role.gold
          orelse NeedRep > RoleInfo#role.reputation ->
            false;
        NeedItemList =:= [] orelse NeedItemList == 0 ->
            RoleInfo2 = role_lib:deduct_coin_f(RoleInfo,NeedCoin,?MONEY_DEC_TYPE_TALENT_STUDY_COST,TalentType,""),
            RoleInfo3 = role_lib:deduct_reputation_f(RoleInfo2,NeedRep,?MONEY_DEC_TYPE_TALENT_STUDY_COST,TalentType,""),
            role_lib:deduct_gold_f(RoleInfo3,NeedGold,?MONEY_DEC_TYPE_TALENT_STUDY_COST,TalentType,""),
            true;
        true ->
            Items = role_data:get_bagItem(),
            {ItemList2,Result,DelItemList2,UpdateItemList2,UpdateItemLogList2} = 
                lists:foldl(fun({new_item,ItemTypeID,Num,_,_},{AccList,AccResult,DelItemAcc,UpdataItemAcc,UpdateItemLogAcc})->
                                 case AccResult of
                                     true ->
                                        case item_lib:check_material2(AccList, ItemTypeID, Num) of
                                            {BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateItemLogList} ->
                                                {BagItemAcc2, true, DelItemList++DelItemAcc, UpdateItemList++UpdataItemAcc, UpdateItemLogAcc++UpdateItemLogList};
                                            _ ->
                                                {AccList, false, [],[],[]}
                                        end;
                                     false ->
                                         {AccList,false, [],[],[]}
                                 end
                         end, {Items,true, [],[],[]}, NeedItemList),
            case Result of
                false ->
                    false;
                true ->
                    ?INFO("rank before:~w~nrank after:~w ~nUpdateItemList2:~w DelItemList2:~w",[Items,ItemList2,UpdateItemList2,DelItemList2]),
                    role_data:set_bagItem(ItemList2),
                    
                    if %通知神奇果实消耗量
                        UpdateItemList2 =:= [] ->
                            ignore;
                        true ->
                            UpdateItemList3 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItemList2],
                            ?sendself(#sc_item_update{updateList=UpdateItemList3})
                    end,
                    if %通知神奇果实用光了
                        DelItemList2 =:= [] ->
                            ignore;
                        true ->
                            DelItemList3 = [E||#item{itemUID=E}<-DelItemList2],
                            ?sendself(#sc_item_delete_notify{itemUIDList=DelItemList3})
                    end,                    
                    
                    RoleID = role_data:get_roleID(),
                    %% 写道具日志
                    LogItemList = role_item:itemList2logItemList(DelItemList2, UpdateItemLogList2),
                    {Date, _} = Time = erlang:localtime(),
                    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_TALENT_STUDY_COST, TalentType, ""),
                    
                    RoleInfo2 = role_lib:deduct_coin_f(RoleInfo,NeedCoin,?MONEY_DEC_TYPE_TALENT_STUDY_COST,TalentType,""),
                    RoleInfo3 = role_lib:deduct_reputation_f(RoleInfo2,NeedRep,?MONEY_DEC_TYPE_TALENT_STUDY_COST,TalentType,""),
                    role_lib:deduct_gold_f(RoleInfo3,NeedGold,?MONEY_DEC_TYPE_TALENT_STUDY_COST,TalentType,""),
                    true
            end
    end.

%% 检查
check_undo(TalentType,Level,OtherTalentList,TalentList,NowSecond)->
    ?INFO("check_undo ~w ~w ~w",[TalentType,Level,OtherTalentList]),
    %% 得到一个列表，包含所有当前技能所依赖的前置技能
    {FrontNeedList,NeedTotal} = lists:foldr(fun({Type1,Level1,_},{AccList,AccTotal})-> 
                                        Config = data_talent:get(Type1+Level1),
                                        NewTotal = 
                                            if
                                                AccTotal < Config#data_talent.need_point ->
                                                    Config#data_talent.need_point;
                                                true ->
                                                    AccTotal
                                            end,
                                        {Config#data_talent.need_talent++AccList,NewTotal}
                                        end, {[],0}, OtherTalentList),
    ActiveTalentList = to_active_talent_list(TalentList,NowSecond),
    ActivePoint = lists:sum([Level00||{_,Level00}<-ActiveTalentList]),    
    ?INFO("check_undo FrontNeedList~w NeedTotal:~w ActivePoint:~w",[FrontNeedList,NeedTotal,ActivePoint]),
    if
        ActivePoint-1 =<  NeedTotal andalso NeedTotal > 0->
            false;
        true ->
            case lists:keyfind(TalentType, 1, FrontNeedList) of
                {Type2,Level2} when Level-1 < Level2->
                    false;
                {Type2,Level2} ->
                    true;
                false ->
                    true
            end
    end.
    
%% 转换数据格式，根据冷却时间返回包含有效等级信息的天赋列表
to_active_talent_list(TalentList,NowSecond)->
    lists:foldl(fun({Type,L,Cd},AccList)->
                    if
                        Cd > NowSecond andalso L == 1 ->
                            AccList;
                        Cd > NowSecond andalso L > 1 ->
                            [{Type,L-1}|AccList];
                        Cd =< NowSecond ->
                            [{Type,L}|AccList]
                    end
                end, [], TalentList).

get_active_talent_list() ->
    TalentList = role_data:get_trainer_info(),
    ActiveTalentList = to_active_talent_list(TalentList, util:now()),
    TrainerBattleRank = role_trainerProf:get_trainer_battle_rank(),
    TrainerBattleSkills = [{TagID,UnLevel,TrainerBattleRank}||#p_trainerProf_battle_unit{level=UnLevel,tagID=TagID}<-role_data:get_trainerProf_battle_data()],
    TrainerBattleSkills ++ ActiveTalentList.

get_remains_point()->
    TalentList = role_data:get_trainer_info(),
    get_remains_point(TalentList).
get_remains_point(TalentList)->
    #role{level = Level} = role_data:get_roleInfo(),
    TotalRolePoint = lists:max([0,(Level-29)]) * data_trainer:get(point_per_level),
    TotalRolePoint - lists:sum([Level0||{_,Level0,_}<-TalentList]).

%转换数据格式，返回用于传给客户端格式的列表
to_p_talent_list(DetailList)->
    lists:foldl(fun({Type,Level,CoolDownTime},Acc)->
                        [#p_talent{talent_id=Type+Level,cool_down_time=CoolDownTime}|Acc]
        end, [], DetailList).

init_recacl_talent_timer()->
    Now = util:now(),
    lists:foreach(fun({_,_,Timestamp})->
                          NeedTime = Timestamp - Now,
                          if
                              NeedTime > 0 ->
                                  ?INFO("set timer to recacl_talent ~w",[NeedTime]),
                                  erlang:send_after((NeedTime+1)*1000, self(), recacl_talent);
                              true ->
                                  ignore
                          end      
                    end, role_data:get_trainer_info()).

%天赋变化，重新计算宠物属性
%为了提高效率，判断和上次计算比较，天赋是否变化，变化的话，才重新计算。
recacl_talent()->
    CurTalentList = to_active_talent_list(role_data:get_trainer_info(), util:now()),
    LastTalentList = get(?pd_last_recacl_talent),
    if
        CurTalentList =:= LastTalentList ->
            ?INFO("recacl_gers --> ignore, beacuse talent changed"),
            ignore;
        true ->
            ?INFO("recacl_gers --> do it, beacuse talent changed"),
            ger_attr:recacl_gers(),
            put(?pd_last_recacl_talent,CurTalentList)
    end.

%-----------测试用，版本发布时可以把这个注释掉-----------------
test_set_talent({test_set_talent, TalentList}) when is_list(TalentList)->
    role_data:set_trainer_info(TalentList).


