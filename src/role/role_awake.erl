-module(role_awake).

-compile(export_all).

-include("def_role.hrl").

-define(FIVE_STEP_AWAKE,5).

-define(LOG_AWAKE_FIRST_OPERATIONCODE,1).
-define(LOG_AWAKE_RECAST_OPERATIONCODE,2).
-define(LOG_AWAKE_EXCHANGE_OPERATIONCODE,3).
-define(LOG_AWAKE_CANCEL_OPERATIONCODE,4).
-define(LOG_AWAKE_DOWN_GER_OPERATIONCODE,5).
-define(LOG_AWAKE_BLINK_INHERIT_OPERATIONCODE,6).

%%===============================协议处理接口=========================================%%
cs_awake_ger(#cs_awake_ger{gerID=GerID,step=Step})->
    case catch do_cs_awake_ger(GerID,Step) of   
        {ok,SkillID}->
            ?sendself(#sc_awake_ger{result=1,skillID=SkillID,gerID=GerID,step=Step});
        {false,Reason}->
            ?sendself(#sc_awake_ger{result=Reason,skillID=0,gerID=GerID,step=Step})
    end.

cs_awake_recast_ger(#cs_awake_recast_ger{gerID=GerID,step=Step,recasttimes=RecastTime})->
    case catch do_cs_awake_recast_ger(GerID,Step,RecastTime) of
        {ok,SkillList}->
            SkillIDList = [SkillID||{SkillID,_SkillQuality}<-SkillList,SkillID=/=0],
            ?sendself(#sc_awake_recast_ger{result=1,skillID=SkillIDList,gerID=GerID,step=Step});
        {false,Reason}->
            ?sendself(#sc_awake_recast_ger{result=Reason,skillID=[],gerID=GerID,step=Step})
    end.

cs_awake_exchange_skill(#cs_awake_exchange_skill{gerID=GerID,step=Step,skillID=ExchangeSkillID})->
    case catch do_cs_awake_exchange_skill(GerID,Step,ExchangeSkillID) of
        {ok,SkillID}->
            ?sendself(#sc_awake_exchange_skill{result=1,gerID=GerID,step=Step,skillID=SkillID});
        {false,Reason}->
            ?sendself(#sc_awake_exchange_skill{result=Reason,gerID=GerID,step=Step,skillID=0})
    end.

cs_awake_cancel_skill(#cs_awake_cancel_skill{gerID=GerID,step=Step})->
    case catch do_cs_awake_cancel_skill(GerID,Step) of
        true->
            ?sendself(#sc_awake_cancel_skill{result=1,gerID=GerID,step=Step});
        {false,Reason}->
            ?sendself(#sc_awake_cancel_skill{result=Reason,gerID=GerID,step=Step})
    end.
%%===========================协议内部处理函数=========================================%%
do_cs_awake_ger(GerID,Step)->
    {Ger, PosList, LPosList, GerBag, Type} =case find_ger_by_gerID(GerID) of
        false->
            erlang:throw({false,2});
        Result ->
            Result
    end,
    case catch check_awake_already(Ger,Step) of
        true->
            erlang:throw({false,3});
        _->
            next
    end,
    case catch check_awake_illegal(Ger,Step) of
        false->
            erlang:throw({false,4});
        true->
            next
    end,
    case catch check_awake_condition(Ger,Step) of
        false->
            erlang:throw({false,5});
        true->
            next
    end,
    case catch deduct_awake_material(Ger,Step) of
        false->
            erlang:throw({false,6});
        {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
            NewAwakeInfo = generate_awake_skill(Step,Ger,1),
            #role{roleID=RoleID} = role_data:get_roleInfo(),
            {_NewGer,SkillID} = update_ger_info(Ger, PosList, LPosList, GerBag, Step,Type,NewAwakeInfo,false,true),
            role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_AWAKE,Step,integer_to_list(GerID)),
            add_ger_awake_log(RoleID,Ger,Step,[],NewAwakeInfo,?LOG_AWAKE_FIRST_OPERATIONCODE),
            {ok,SkillID}
    end.

%%处理精灵觉醒技能洗练请求
do_cs_awake_recast_ger(GerID,Step,RecastTime)->
    {Ger, PosList, LPosList, GerBag, Type} =case find_ger_by_gerID(GerID) of
        false->
            erlang:throw({false,2});
        Result ->
            Result
    end,
    case catch check_awake_already(Ger,Step) of
        true->
            next;
        _->
            erlang:throw({false,3})
    end,
    case catch deduct_awake_recast_material(Ger,Step,RecastTime) of
        false->
            erlang:throw({false,4});
        {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
            NewAwakeInfo = generate_awake_skill(Step,Ger,RecastTime),
            #role{roleID=RoleID} = role_data:get_roleInfo(),
            {_NewGer,SkillID} = update_ger_info(Ger, PosList, LPosList, GerBag, Step,Type,NewAwakeInfo,true,false),
            OldAwakeInfo = case is_record(Ger,gerSimple) of true->  Ger#gerSimple.gerAwakeInfo;false->Ger#ger.gerBase#gerBase.gerAwakeInfo end,
            role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_RECAST,Step,integer_to_list(GerID)),
            add_ger_awake_log(RoleID,Ger,Step,OldAwakeInfo,NewAwakeInfo,?LOG_AWAKE_RECAST_OPERATIONCODE),
            {ok,SkillID}
    end.

%处理精灵觉醒技能置换请求
do_cs_awake_exchange_skill(GerID,Step,ExchangeSkillID)->
    {Ger, PosList, LPosList, GerBag, Type} =case find_ger_by_gerID(GerID) of
        false->
            erlang:throw({false,2});
        Result ->
            Result
    end,
    case check_awake_already(Ger,Step) of
        true->
            next;
        _ ->
            erlang:throw({false,3})
    end,
    case catch change_skill(Ger,Step,ExchangeSkillID) of
        {false,Reason} ->
            erlang:throw({false,Reason});
        {_SkillID,NewAwakeInfo}->
            #role{roleID=RoleID} = role_data:get_roleInfo(),
            {_NewGer,NewSkillID} = update_ger_info(Ger, PosList, LPosList, GerBag, Step,Type,NewAwakeInfo,false,false),
            OldAwakeInfo = case is_record(Ger,gerSimple) of true->  Ger#gerSimple.gerAwakeInfo;false->Ger#ger.gerBase#gerBase.gerAwakeInfo end,
            add_ger_awake_log(RoleID,Ger,Step,OldAwakeInfo,NewAwakeInfo,?LOG_AWAKE_EXCHANGE_OPERATIONCODE),
            {ok,NewSkillID}
    end.

%%处理精灵觉醒技能放弃请求
do_cs_awake_cancel_skill(GerID,Step)->
    {Ger, PosList, LPosList, GerBag, Type} =case find_ger_by_gerID(GerID) of
        false->
            erlang:throw({false,2});
        Result ->
            Result
    end,
    case check_awake_already(Ger,Step) of
        true->
            next;
        _ ->
            erlang:throw({false,3})
    end,
    case catch cancle_skill(Ger,Step) of
        {false,Reason} ->
            erlang:throw({false,Reason});
        {true,NewAwakeInfo}->
            #role{roleID=RoleID} = role_data:get_roleInfo(),
            update_ger_info(Ger, PosList, LPosList, GerBag, Step,Type,NewAwakeInfo,false,false),
            OldAwakeInfo = case is_record(Ger,gerSimple) of true->  Ger#gerSimple.gerAwakeInfo;false->Ger#ger.gerBase#gerBase.gerAwakeInfo end,
            add_ger_awake_log(RoleID,Ger,Step,OldAwakeInfo,NewAwakeInfo,?LOG_AWAKE_CANCEL_OPERATIONCODE),
            true
    end.

%=======================================================================================%
find_ger_by_gerID(GerID)->
    case role_data:get_ger(GerID) of
        false ->
            false;
        {value, Ger,PosList2,LPosList,GerBag,Type}->
            {Ger, PosList2, LPosList, GerBag, Type}
    end.

%%检查精灵对应的阶段是否已经觉醒
check_awake_already(Ger,Step)->
    if
        is_record(Ger,ger) ->
            check_awake_already2(Step,Ger#ger.gerBase#gerBase.gerAwakeInfo);
        is_record(Ger,gerSimple)->
            check_awake_already2(Step,Ger#gerSimple.gerAwakeInfo);
        true->
            ?undefined
    end.

check_awake_already2(Step,GerAwakeInfo) when is_list(GerAwakeInfo)->
    case lists:keyfind(Step,#step.stepid,GerAwakeInfo) of
        false->
            false;
        _ ->
            true
    end;
check_awake_already2(_Step,_GerAwakeInfo)->
    ?undefined.

%%判断精灵能够觉醒对应的阶段
check_awake_illegal(Ger,Step)->
    if
        is_record(Ger,ger) ->
            check_awake_illegal2(Step,Ger#ger.gerBase#gerBase.gerTypeID);
        is_record(Ger,gerSimple)->
            check_awake_illegal2(Step,Ger#gerSimple.gerTypeID);
        true->
            false
    end.
%%判断精灵觉醒的条件是否满足
check_awake_condition(Ger,Step)->
    if
        is_record(Ger,ger) ->
            Mega = check_ger_mega(Ger#ger.gerBase#gerBase.gerTypeID),
            GerAwakeInfo = Ger#ger.gerBase#gerBase.gerAwakeInfo,
            Rank = Ger#ger.gerBase#gerBase.gerQuality,
            check_awake_condition2(GerAwakeInfo,Step,Rank,Mega);
        is_record(Ger,gerSimple)->
            Mega = check_ger_mega(Ger#gerSimple.gerTypeID),
            GerAwakeInfo = Ger#gerSimple.gerAwakeInfo,
            Rank = Ger#gerSimple.gerQuality,
            check_awake_condition2(GerAwakeInfo,Step,Rank,Mega);
        true->
            false
    end.

%%判断玩家觉醒材料是否足够
deduct_awake_material(Ger,Step)->
    {Cost,GerID}= if
        is_record(Ger,ger) ->
            {get_awake_cost(Ger#ger.gerBase#gerBase.gerTypeID,Step),Ger#ger.gerID};
        is_record(Ger,gerSimple)->
            {get_awake_cost(Ger#gerSimple.gerTypeID,Step),Ger#gerSimple.gerID};
        true->
            erlang:throw(false)
    end,
    case is_record(Cost,sell_reward) of
        true->
            role_item:delete_sell_reward(Cost,[],[GerID]);
        false->
            ?INFO("生成的消耗不是sell_reward结构"),
            erlang:throw(false)
    end.

    % role_data:set_gerBag(GerBag3),
    % ger_lib:notify_update(SrcGer3),
    % %% 提醒客户端更新武将
    % role_data:set_lieuposList(LPosList3),
    % PosList4 = ger_attr:refresh_fightPower(PosList3),
    % role_data:set_posList(PosList4),
%%更新对应精灵的觉醒信息，并且返回产生的技能
update_ger_info(Ger, PosList, LPosList, GerBag, Step,Type,NewAwakeInfo,New,FirstAwake)->
    % NewAwakeInfo = generate_awake_skill(Step,Ger),
    ?INFO("NewAwakeInfo:~w ~n",[NewAwakeInfo]),
    NewGer3 = case Type of
        bag->
            NewGer = Ger#gerSimple{gerAwakeInfo=NewAwakeInfo},
            role_data:set_gerbag([NewGer|GerBag]),
            NewGer;
        lieu->
            GerBase = Ger#ger.gerBase,
            NewGerBase = GerBase#gerBase{gerAwakeInfo=NewAwakeInfo},
            NewGer = Ger#ger{gerBase=NewGerBase},
            role_data:set_lieuposList([NewGer|LPosList]),
            NewGer;
        ger->
            GerBase = Ger#ger.gerBase,
            NewGerBase = GerBase#gerBase{gerAwakeInfo=NewAwakeInfo},
            NewGer = Ger#ger{gerBase=NewGerBase},
            ?INFO("NewGer:~w ,Other:~w ~n",[NewGer,PosList]),
            NewGer1 = case FirstAwake of
                true->
                    NewGer4 = role_crystal:refresh_gercrystal(NewGer),
                    role_crystal:notice_ger_crystal(NewGer4),
                    NewGer4;
                false->
                    NewGer
            end,
            NewGer2 = ger_attr:recacl(NewGer1,PosList),        
            PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList]),
            role_data:set_posList(PosList4),
            NewGer2;
        _ ->
            ?INFO("undefined Type:~w ~n",[Type]),
            ?undefined
    end,
    % ger_attr:recacl_f(Ger#ger.gerID),
    case lists:keyfind(Step,#step.stepid,NewAwakeInfo) of
        false->
            ?INFO("更新了觉醒信息之后，新的觉醒列表中没有对应的觉醒阶段数据：Step：~w NewAwakeInfo:~w ~n",[Step,NewAwakeInfo]),
            ignore;
        FindOne->
            case New of
                true->
                    {NewGer3,FindOne#awake.new_skilllist};
                false->
                    {NewGer3,FindOne#awake.skillID}
            end
    end.

%%判断精灵觉醒的条件是否满足
check_awake_condition2(GerAwakeInfo,Step,Rank,Mega)->
    Condition = case Mega of
        false->
            data_awake:get({awake_condition_unmega,Step});
        true->
            data_awake:get({awake_condition_mega,Step});
        _ ->
            erlang:throw(false)
    end,
    case Condition of
        ?undefined->
            false;
        {PreList,RankLimit}->
            case Rank >= RankLimit of
                false->
                    false;
                true->
                    lists:all(fun(StepID)->
                        case lists:keyfind(StepID,#awake.step,GerAwakeInfo) of
                            false->
                                false;
                            _ ->
                                true
                        end
                    end,PreList)
            end
    end.

%%判断精灵是否为mega精灵
check_ger_mega(GerTypeID)->
    case lists:member(GerTypeID,data_awake:get(mega_gerTypeIDList)) of
        true->
            true;
        false->
            case lists:member(GerTypeID,data_awake:get(unmega_gerTypeIDList)) of
                true->
                    false;
                false->
                    ?INFO("精灵ID同时不属于mega与非mega列表ID：~w~n",[GerTypeID]),
                    ?undefined
            end
    end.

%%根据配置以及GerTypeID生成消耗结构,会添加上消耗同等GerTypeID精灵的配置，结构为sell_reward
get_awake_cost(GerTypeID,Step)->
    case ger_lib:is_blink_ger(GerTypeID) of
        false->
            get_awake_cost2(GerTypeID,Step);
        true->
            case data_ger:get(GerTypeID) of
                #data_ger{sameType=SameType}->
                    get_awake_cost2(SameType,Step);
                _->
                    ?ERR("undefined gerTypeID:~w ~n",[GerTypeID]),
                    false
            end
    end.
get_awake_cost2(GerTypeID,Step)->
    Cost1 = case check_ger_mega(GerTypeID) of
        false->
            data_awake:get({awake_step_unmega,Step});
        true->
            data_awake:get({awake_step_mega,Step})
    end,
    case Cost1 of
        {#sell_reward{}=SellReward,Num}->
            OldGerList = SellReward#sell_reward.newGer,
            NeedGer = #new_ger{gerTypeID=GerTypeID,gerLevel=-1,gerQuality=0},
            NeedGerList=lists:duplicate(Num,NeedGer),
            SellReward#sell_reward{newGer=NeedGerList++OldGerList};
        _->
            false
    end.

%%判断精灵是否能够进行对应step的觉醒
check_awake_illegal2(Step,GerTypeID)->
    StepList = case check_ger_mega(GerTypeID) of
        false->
            data_awake:get(unmega_step_list);
        true->
            data_awake:get(mega_step_list);
        _->
            erlang:throw(false)
    end,
    lists:member(Step,StepList).

%%生成觉醒技能,返回精灵新的awakeinfo|undefined
generate_awake_skill(Step,Ger,Times)->
    if
        is_record(Ger,ger) ->
            generate_awake_skill2_with_times(Step,Ger#ger.gerBase#gerBase.gerAwakeInfo,Ger#ger.gerBase#gerBase.gerTypeID,Times);
        is_record(Ger,gerSimple)->
            generate_awake_skill2_with_times(Step,Ger#gerSimple.gerAwakeInfo,Ger#gerSimple.gerTypeID,Times);
        true->
            ?undefined
    end.

%%将原来的recastskillIDList清除掉，重新生成技能
generate_awake_skill2_with_times(Step,GerAwakeInfo,GerTypeID,Times)->
    case lists:keytake(Step,#awake.step,GerAwakeInfo) of
        false->
            generate_awake_skill2_with_times2(Step,GerAwakeInfo,GerTypeID,Times);
        {_Value,FindAwakeUnit,OtherAwakeUnit}->
            NewGerAwakeInfo = [FindAwakeUnit#awake{new_skilllist=[]}|OtherAwakeUnit],
            generate_awake_skill2_with_times2(Step,NewGerAwakeInfo,GerTypeID,Times)
    end.

%%重复生成Times次技能
generate_awake_skill2_with_times2(_Step,GerAwakeInfo,_GerTypeID,0)->
    GerAwakeInfo;
generate_awake_skill2_with_times2(Step,GerAwakeInfo,GerTypeID,Times)->
    NewGerAwakeInfo = generate_awake_skill2(Step,GerAwakeInfo,GerTypeID),
    generate_awake_skill2_with_times2(Step,NewGerAwakeInfo,GerTypeID,Times-1).

%%根据精灵的觉醒信息生成觉醒技能ID
generate_awake_skill2(Step,GerAwakeInfo,GerTypeID)->
    case check_ger_mega(GerTypeID) of
        true->
            generate_awake_skill2_mega(Step,GerAwakeInfo,GerTypeID);
        false->
            generate_awake_skill2_unmega(Step,GerAwakeInfo,GerTypeID);
        _ ->
            ?undefined
    end.

%%生成非mega精灵觉醒技能,返回undefined|新的awakeinfoList
generate_awake_skill2_unmega(Step,GerAwakeInfo,GerTypeID)->
    case check_first_awake(Step,GerAwakeInfo) of
        true->
            generate_first_awake_skill_unmega(Step,GerAwakeInfo,GerTypeID);
        {false,OldAwakeInfo,Other}->
            generate_recast_awake_skill_unmega(Step,Other,OldAwakeInfo,GerTypeID);
        _ ->
            ?undefined
    end.

%%生成mega精灵觉醒技能
generate_awake_skill2_mega(Step,GerAwakeInfo,GerTypeID)->
    case check_first_awake(Step,GerAwakeInfo) of
        true->
            generate_first_awake_skill_mega(Step,GerAwakeInfo,GerTypeID);
        {false,OldAwakeInfo,Other}->
            generate_recast_awake_skill_mega(Step,Other,OldAwakeInfo,GerTypeID);
        _->
            ?undefined
    end.

%%判断精灵是否为初次觉醒，返回true|undefined|{false,#awake,Other}
check_first_awake(Step,GerAwakeInfo) when is_list(GerAwakeInfo)->
    case lists:keytake(Step,#awake.step,GerAwakeInfo) of
        false->
            true;
        {_,FindOne,Other} ->
            {false,FindOne,Other}
    end;
check_first_awake(_Step,_GerAwakeInfo)->
    ?undefined.

%%生成非mega精灵初次觉醒技能，返回新的awakeinfo
generate_first_awake_skill_unmega(Step,GerAwakeInfo,GerTypeID)-> 
    case data_ger:get(GerTypeID) of
        ?undefined->
            GerAwakeInfo;
        #data_ger{gerProperty=GerProperty}->
            StepList = case Step =:= ?FIVE_STEP_AWAKE of
                true->
                    %%按照逻辑，非mega精灵是不能够进行第5次觉醒的，如果到了这里说明前面的判断阶段出现问题
                    ?ERR("不应该运行到这里，非mega精灵进行了第5次觉醒：GerTypeID：~w ~n",[GerTypeID]),
                    data_awake:get({data_mega_first_awake_five,GerProperty});
                false->
                    data_awake:get(data_unmega_first_awake)
            end,
            StepInfo = case lists:keyfind(Step,#step.stepid,StepList) of
                false->
                    ?undefined;
                FindOne->
                    FindOne
            end,
            [get_random_awake_skillID(StepInfo,#awake{},false,GerTypeID)|GerAwakeInfo]
    end.

%%根据step结构、awake结构生成新的觉醒技能
get_random_awake_skillID(StepInfo,AwakeInfo,IsRecast,GerTypeID)->
    case IsRecast of
        false->
            generate_first_awake_skillID(StepInfo,GerTypeID);
        true->
            generate_recast_awake_skillID(StepInfo,AwakeInfo,GerTypeID)
    end.

%%生成初次觉醒技能
generate_first_awake_skillID(StepInfo,GerTypeID)->
    SkillIDList1 = StepInfo#step.skillIDList,
    SkillIDList = case length(SkillIDList1) =:= 2 of
        true->
            [PhySkillIDList,MagicSkillIDList]=SkillIDList1,
            case data_ger:get(GerTypeID) of
                ?undefined->
                    PhySkillIDList;
                #data_ger{gerProperty=GerProperty}->
                    case is_magic(GerProperty) of
                        true->
                            MagicSkillIDList;
                        false->
                            PhySkillIDList
                    end
            end;
        false->
            SkillIDList1
    end,
    {SkillID,Quality} = util:random_one_from_weigh_list(SkillIDList),
    #awake{step=StepInfo#step.stepid,skillID=SkillID,skill_quality=Quality}.

%%参考洗练次数生成觉醒技能
generate_recast_awake_skillID(StepInfo,AwakeInfo=#awake{new_skilllist=OldSkillIDList},GerTypeID)->
    SkillIDList1=StepInfo#step.skillIDList,
    SkillIDList = case length(SkillIDList1) =:= 2 of
        true->
            [PhySkillIDList,MagicSkillIDList]=SkillIDList1,
            case data_ger:get(GerTypeID) of
                ?undefined->
                    PhySkillIDList;
                #data_ger{gerProperty=GerProperty}->
                    case is_magic(GerProperty) of
                        true->
                            MagicSkillIDList;
                        false->
                            PhySkillIDList
                    end
            end;
        false->
            SkillIDList1
    end,
    StepInfo1 = StepInfo#step{skillIDList=SkillIDList},
    SkillIDList2 = case check_must_generate(AwakeInfo) of
        false->
            delete_must_not_generate(AwakeInfo,StepInfo1);
        {true,Quality}->
            [E||{_,{_SkillID,Quality1}}=E<-StepInfo1#step.skillIDList,Quality=:=Quality1]
    end,
    {SkillID,FindQuality} = generate_awake_skill_with_oldid(SkillIDList2,AwakeInfo#awake.skillID),
    AwakeInfo#awake{recast_time=AwakeInfo#awake.recast_time+1,new_skilllist=[{SkillID,FindQuality}|OldSkillIDList]}.

%%判断此次洗练觉醒技能是否有必出的品质,返回false|{true,Quality}
check_must_generate(AwakeInfo)->
    MustGenerateList= data_awake:get(data_awake_must_generate_list),
    case lists:keyfind(AwakeInfo#awake.recast_time+1,1,MustGenerateList) of
        false->
            false;
        {_Num,Quality}->
            ?INFO("出现必须产生品质为：~w Time:~w 情况~n",[Quality,AwakeInfo#awake.recast_time+1]),
            {true,Quality}
    end.

%%删除掉必定不会出现的品质
%%return:[{priority,{skillID,quality}}]|[]
delete_must_not_generate(AwakeInfo,StepInfo)->
    ?INFO("AwakeInfo:~w StepInfo:~w ~n",[AwakeInfo,StepInfo]),
    NotGenerateConfigList = data_awake:get(data_awake_must_not_generate_list),
    NotGenerateList = [Quality||{Quality,Num}<-NotGenerateConfigList,Num>AwakeInfo#awake.recast_time],
    ResultList=lists:foldl(fun(E,Acc)->
        {_Priority,{_SkillID,Quality}}=E,
        case lists:member(Quality,NotGenerateList) of
            false->
                [E|Acc];
            true->
                Acc
        end
    end,[],StepInfo#step.skillIDList),
    ?INFO("去除不能出现的品质后的技能列表：~w ~n",[ResultList]),
    ResultList.


%%生成非mega精灵觉醒洗练技能,返回undefined|新的awakeinfoList
generate_recast_awake_skill_unmega(Step,OtherAwakeInfo,OldAwakeInfo,GerTypeID)->
    case data_ger:get(GerTypeID) of
        ?undefined->
            [OldAwakeInfo|OtherAwakeInfo];
        #data_ger{gerProperty=GerProperty}->
            StepList = case Step =:= ?FIVE_STEP_AWAKE of
                true->
                    %%按照逻辑，非mega精灵是不能够进行第5次觉醒的，如果到了这里说明前面的判断阶段出现问题
                    ?ERR("不应该运行到这里，非mega精灵进行了第5次觉醒：GerTypeID：~w ~n",[GerTypeID]),
                    data_awake:get({data_mega_recast_awake_five,GerProperty});
                false->
                    data_awake:get(data_unmega_recast_awake)
            end,
            StepInfo = case lists:keyfind(Step,#step.stepid,StepList) of
                false->
                    ?undefined;
                FindOne->
                    FindOne
            end,
            [get_random_awake_skillID(StepInfo,OldAwakeInfo,true,GerTypeID)|OtherAwakeInfo]
    end.

%%生成mega精灵初次觉醒技能,返回undefined|新的awakeinfoList
generate_first_awake_skill_mega(Step,GerAwakeInfo,GerTypeID)->
    case data_ger:get(GerTypeID) of
        ?undefined->
            GerAwakeInfo;
        #data_ger{gerProperty=GerProperty}->
            StepList = case Step =:= ?FIVE_STEP_AWAKE of
                true->
                    data_awake:get({data_mega_first_awake_five,GerProperty});
                false->
                    data_awake:get(data_mega_first_awake)
            end,
            StepInfo = case lists:keyfind(Step,#step.stepid,StepList) of
                false->
                    ?undefined;
                FindOne->
                    FindOne
            end,
            [get_random_awake_skillID(StepInfo,#awake{},false,GerTypeID)|GerAwakeInfo]
    end.

%%生成mega精灵觉醒洗练技能,返回undefined|新的awakeinfoList
generate_recast_awake_skill_mega(Step,OtherAwakeInfo,OldAwakeInfo,GerTypeID)->
    case data_ger:get(GerTypeID) of
        ?undefined->
            [OldAwakeInfo|OtherAwakeInfo];
        #data_ger{gerProperty=GerProperty}->
            StepList = case Step =:= ?FIVE_STEP_AWAKE of
                true->
                    data_awake:get({data_mega_recast_awake_five,GerProperty});
                false->
                    data_awake:get(data_mega_recast_awake)
            end,
            StepInfo = case lists:keyfind(Step,#step.stepid,StepList) of
                false->
                    ?undefined;
                FindOne->
                    FindOne
            end,
            [get_random_awake_skillID(StepInfo,OldAwakeInfo,true,GerTypeID)|OtherAwakeInfo]
    end.

%%判断洗练材料是否满足
%%return： 消耗后的各种物品剩余|erlang:throw(false)    
deduct_awake_recast_material(Ger,Step,RecastTime)->
    {Cost,GerID}= if
        is_record(Ger,ger) ->
            {get_awake_recast_cost_with_recasttime(Ger#ger.gerBase#gerBase.gerTypeID,Step,RecastTime),Ger#ger.gerID};
        is_record(Ger,gerSimple)->
            {get_awake_recast_cost_with_recasttime(Ger#gerSimple.gerTypeID,Step,RecastTime),Ger#gerSimple.gerID};
        true->
            erlang:throw(false)
    end,
    case is_record(Cost,sell_reward) of
        true->
            role_item:delete_sell_reward(Cost,[],[GerID]);
        false->
            ?INFO("生成的消耗不是sell_reward结构:~w~n",[Cost]),
            erlang:throw(false)
    end.

get_awake_recast_cost_with_recasttime(GerTypeID,Step,RecastTime)->
    get_awake_recast_cost_with_recasttime2(GerTypeID,Step,RecastTime,#sell_reward{}).
get_awake_recast_cost_with_recasttime2(_GerTypeID,_Step,0,Acc)->
    Acc;
get_awake_recast_cost_with_recasttime2(GerTypeID,Step,RecastTime,Acc)->
    SingleRecastCost = get_awake_recast_cost(GerTypeID,Step),
    NewAcc = role_reward:reward_plus_reward(SingleRecastCost,Acc),
    get_awake_recast_cost_with_recasttime2(GerTypeID,Step,RecastTime-1,NewAcc).

%%获取单次觉醒洗练消耗的材料
get_awake_recast_cost(GerTypeID,Step)->
    Cost1 = case check_ger_mega(GerTypeID) of
        false->
            data_awake:get({data_recast_cost_unmega,Step});
        true->
            data_awake:get({data_recast_cost_mega,Step})
    end,
    case Cost1 of
        {#sell_reward{}=SellReward,MegaNum,StoneNum}->
            case MegaNum=:=0 andalso StoneNum=:=0 of
                false->
                    MegaClipTypeID = get_mega_clip_id(GerTypeID),
                    KeyClipTypeID = get_key_clip_id(GerTypeID),
                    OldItemList = SellReward#sell_reward.item,
                    MegaClip = case MegaNum =/= 0 of
                        true->
                            [#new_item{itemTypeID=MegaClipTypeID,itemNum=MegaNum,itemLevel=1,itemRank=0}];
                        false->
                            []
                    end,
                    KeyClip = case StoneNum =/= 0 of
                        true->
                            [#new_item{itemTypeID=KeyClipTypeID,itemNum=StoneNum,itemLevel=1,itemRank=0}];
                        false->
                            []
                    end,
                    SellReward#sell_reward{item=MegaClip++KeyClip++OldItemList};
                true->
                    SellReward
            end;
        _->
            false
    end.

%%根据精灵系判断精灵属于物理还是法系
is_magic(GerProperty)->
    lists:member(GerProperty,data_awake:get(data_magic_property)).


test_set_ger_awake_info(Step,SkillID,RecastTime,RoleID,GerID)->
    case catch role_lib:send_server(RoleID, {update_awake_info,GerID,Step,SkillID,RecastTime}) of
                {'EXIT',_}->
                    ?INFO("修改玩家觉醒信息失败~n");
                _ ->
                    ignore                            
    end.

do_fix_role_ger_awakeinfo(GerID,Step,SkillID,RecastTime)->
     {Ger, PosList, LPosList, GerBag, Type} =case find_ger_by_gerID(GerID) of
        false->
            ignore;
        Result ->
            Result
    end, 
    AwakeInfo = if
        is_record(Ger,gerSimple)->
            Ger#gerSimple.gerAwakeInfo;
        is_record(Ger,ger)->
            Ger#ger.gerBase#gerBase.gerAwakeInfo
    end,
    NewAwakeInfo = case lists:keytake(Step,#awake.step,AwakeInfo) of
        false->
            [#awake{step=Step,skillID=SkillID,recast_time=RecastTime}|AwakeInfo];
        {_,Find,Other}->
            [Find#awake{skillID=SkillID,recast_time=RecastTime}|Other]
    end,
    case Type of
        bag->
            NewGer = Ger#gerSimple{gerAwakeInfo=NewAwakeInfo},
            role_data:set_gerbag([NewGer|GerBag]);
        lieu->
            GerBase = Ger#ger.gerBase,
            NewGerBase = GerBase#gerBase{gerAwakeInfo=NewAwakeInfo},
            NewGer = Ger#ger{gerBase=NewGerBase},
            role_data:set_lieuposList([NewGer|LPosList]);
        ger->
            GerBase = Ger#ger.gerBase,
            NewGerBase = GerBase#gerBase{gerAwakeInfo=NewAwakeInfo},
            NewGer = Ger#ger{gerBase=NewGerBase},
            NewGer2 = ger_attr:recacl(NewGer,PosList),        
            PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList]),
            role_data:set_posList(PosList4);
        _ ->
            ?INFO("undefined Type:~w ~n",[Type])
    end.

%%置换精灵的觉醒技能
change_skill(Ger,Step,ExchangeSkillID)->
    GerAwakeInfo = if
        is_record(Ger,ger) ->
            Ger#ger.gerBase#gerBase.gerAwakeInfo;
        is_record(Ger,gerSimple)->
            Ger#gerSimple.gerAwakeInfo;
        true->
            ?ERR("出现异常Ger：~w ~n",[Ger]),
            erlang:throw({false,5})
    end,
    case lists:keytake(Step,#awake.step,GerAwakeInfo) of
        false->
            ?ERR("出现不能找到的觉醒阶段：step:~w GerAwakeInfo:~w ~n",[Step,GerAwakeInfo]),
            erlang:throw({false,5});
        {_,#awake{new_skilllist=NewSkillIDList}=TheOne,Other}->
            NewAwake = case lists:keyfind(ExchangeSkillID,1,NewSkillIDList) of
                false->
                    erlang:throw({false,4});
                {ExchangeSkillID,ExchangeSkillQuality}->
                    TheOne#awake{skillID=ExchangeSkillID,skill_quality=ExchangeSkillQuality,new_skilllist=[]}
            end,
            {ExchangeSkillID,[NewAwake|Other]}
    end.

%%放弃精灵洗练出来的技能
cancle_skill(Ger,Step)->
    GerAwakeInfo = if
        is_record(Ger,ger) ->
            Ger#ger.gerBase#gerBase.gerAwakeInfo;
        is_record(Ger,gerSimple)->
            Ger#gerSimple.gerAwakeInfo;
        true->
            ?ERR("出现异常Ger：~w ~n",[Ger]),
            erlang:throw({false,5})
    end,
    case lists:keytake(Step,#awake.step,GerAwakeInfo) of
        false->
            ?ERR("出现不能找到的觉醒阶段：step:~w GerAwakeInfo:~w ~n",[Step,GerAwakeInfo]),
            erlang:throw({false,5});
        {_,#awake{}=TheOne,Other}->
            NewAwake = TheOne#awake{new_skilllist=[]},
            {true,[NewAwake|Other]}
    end.

get_mega_clip_id(GerTypeID)->
    case data_awake:get({data_ger_mega_map,GerTypeID}) of
        ?undefined->
            ?ERR("查找不到对应精灵ID:~w 需要消耗的mega碎片~n",[GerTypeID]),
            erlang:throw(false);
        ID ->
            ID
    end.

get_key_clip_id(GerTypeID)->
    #data_ger{gerProperty=GerProperty} = data_ger:get(GerTypeID),
    case data_awake:get({data_property_stone_map,GerProperty}) of
        ?undefined->
            ?ERR("查找不到对应精灵ID：~w 需要消耗的键石~n",[GerTypeID]),
            erlang:throw(false);
        ID->
            ID
    end.

%%更新精灵觉醒数据，对低于觉醒品阶条件的觉醒数据予以删除
%%return 新的觉醒列表
calculate_awakeinfo_down_rank([],_NewQuality,_GerTypeID)->
    [];
calculate_awakeinfo_down_rank(OldAwakeInfoList,NewQuality,GerTypeID)->
    Name= case check_ger_mega(GerTypeID) of
            false->
                awake_condition_unmega;
            true->
                awake_condition_mega
        end,
    lists:foldl(fun(Awake,Acc)->
        {_PreList,Rank} = data_awake:get({Name,Awake#awake.step}),
        case NewQuality >= Rank of
            true->
                [Awake|Acc];
            false->
                Acc
        end
    end,[],OldAwakeInfoList).

trans_awakeInfo2p_awake_info(GerAwakeInfo) when is_list(GerAwakeInfo)->
    lists:foldl(fun(#awake{step=Step,skillID=CurrentSkillID,skill_quality=CurrentQuality,new_skilllist=RecastSkillList},Acc)->
        RecastSkillIDList = [SkillID||{SkillID,_SkillQuality}<-RecastSkillList],
        [#p_awake_info{step=Step,current_skillID=CurrentSkillID,current_quality=CurrentQuality,recastskillIDlist=RecastSkillIDList}|Acc]
    end,[],GerAwakeInfo);
trans_awakeInfo2p_awake_info(_GerAwakeInfo)->
    [].             

generate_awake_skill_with_oldid(SkillIDList,OldSkillID)->
    {SkillID,Quality}=util:random_one_from_weigh_list(SkillIDList),
    case OldSkillID=:= SkillID andalso length(SkillIDList)=/=1 of
        true->
            generate_awake_skill_with_oldid(SkillIDList,OldSkillID);
        false->
            {SkillID,Quality}
    end.

get_max_awake_step(GerAwakeInfo) when erlang:is_integer(GerAwakeInfo) ->
    GerAwakeInfo;
get_max_awake_step(GerAwakeInfo)->
    get_max_awake_step(GerAwakeInfo,0).
get_max_awake_step([],MaxStep)->
    MaxStep;
get_max_awake_step([H|T],MaxStep) when is_record(H,awake)->
    case H#awake.step>MaxStep of
        true->
            get_max_awake_step(T,H#awake.step);
        false->
            get_max_awake_step(T,MaxStep)
    end;
get_max_awake_step([H|T],MaxStep) when is_record(H,p_awake_info)->
    case H#p_awake_info.step>MaxStep of
        true->
            get_max_awake_step(T,H#p_awake_info.step);
        false->
            get_max_awake_step(T,H#p_awake_info.step)
    end;
get_max_awake_step([_H|T],MaxStep)->
    get_max_awake_step(T,MaxStep).

transform_oldawake2newawake(AwakeUnit) when is_record(AwakeUnit,awake)->
    AwakeUnit;
transform_oldawake2newawake(AwakeUnit)->
    {awake,Step,SkillID,RecastTime,SkillQuality,NewSkillID,NewSkillQuality} = AwakeUnit,
    NewSkillList = case NewSkillID=:=0 andalso NewSkillQuality =:= 0 of true-> [];false->[{NewSkillID,NewSkillQuality}] end,
    #awake{step=Step,skillID=SkillID,recast_time=RecastTime,skill_quality=SkillQuality,new_skilllist=NewSkillList}.

%%直接根据将NewAwakeInfo与OldAwakeInfo中互相不存在的写入记录,该函数只用于因精灵品阶降低造成的觉醒删除的情况
add_ger_awake_log(RoleID,Ger,0,OldAwakeInfo,NewAwakeInfo,OperationCode)->
    OldStepList= [OldStep||#awake{step=OldStep}<-OldAwakeInfo],
    NewStepList= [NewStep||#awake{step=NewStep}<-NewAwakeInfo],
    OldWithoutNew = [S||S<-OldStepList,not lists:member(S,NewStepList)],
    NewWithoutOld = [S||S<-NewStepList,not lists:member(S,OldStepList)],
    [add_ger_awake_log(RoleID,Ger,S,OldAwakeInfo,NewAwakeInfo,OperationCode)||S<-OldWithoutNew++NewWithoutOld];
add_ger_awake_log(RoleID,Ger,AwakeStep,OldAwakeInfo,NewAwakeInfo,OperationCode)->
    {Date,_} = Time = erlang:localtime(),
    case is_record(Ger,gerSimple) of
        true->
            #gerSimple{gerID=GerUID,gerTypeID=GerTypeID} = Ger;
        false ->
            #ger{gerBase=GerBase,gerID=GerUID} = Ger,
            #gerBase{gerTypeID=GerTypeID} = GerBase
    end,
    {OldSkillID,OldOptionalSkillID,OldRecastTimes} = get_step_awke_log(AwakeStep,OldAwakeInfo),
    {NewSkillID,NewOptionalSkillID,NewRecastTimes} = get_step_awke_log(AwakeStep,NewAwakeInfo),
    behavior_ger_awake:log(RoleID,GerUID,GerTypeID,AwakeStep,OldSkillID,NewSkillID,OldOptionalSkillID,NewOptionalSkillID,OldRecastTimes,NewRecastTimes,Date,Time,OperationCode).

get_step_awake(Step,AwakeInfo)->
    case lists:keyfind(Step,#awake.step,AwakeInfo) of
        false->
            [];
        Find->
            Find
    end.
get_step_awke_log(Step,AwakeInfo)->
    case get_step_awake(Step,AwakeInfo) of
        []->
            {0,"",0};
        #awake{skillID=SkillID,recast_time=RecastTime,new_skilllist=NewSkillList}->
            {SkillID,transform_skillid2log(NewSkillList),RecastTime}
    end.

transform_skillid2log(SkillList)->
   StringList = [integer_to_list(E)||{E,_Q}<-SkillList],
   string:join(StringList,","). 