-module(role_skin).
-compile(export_all).
-include("def_role.hrl").
-define(SKIN, skin).
-define(FETTER_GER_DEMAGE_PLUS_TYPE,1).
-define(FETTER_GER_DEMAGE_DEC_TYPE,2).
-define(TRAINER_MARK_PLUS_TYPE,3).
-define(TRAINER_EVEN_DEMAGE_PLUS,4).
%% 客户端请求
cs_skin_compose(#cs_skin_compose{itemTypeID=ItemTypeID}) ->
    RoleInfo = role_data:get_roleInfo(),
    case catch check_compose(ItemTypeID,RoleInfo) of
        {false, Code} ->
            ?sendself(#sc_skin_compose{result=Code,is_first=false});
        {true, Cost,EffectId} ->
            do_compose(ItemTypeID, Cost,EffectId),
            send_skin_info()
    end.

cs_skin_activate(#cs_skin_activate{itemTypeID=ItemTypeID, pos = Pos}) ->
    case catch check_activate(ItemTypeID, Pos) of
        {false, Code} ->
            ?sendself(#sc_skin_activate{result=Code});
        {true, Cost, EffectId, Has, Skin} ->
            do_activate(Cost, EffectId, Has, Skin,ItemTypeID,Pos),
            send_skin_info()
    end.

cs_skin_equip(#cs_skin_equip{itemTypeID=ItemTypeID}) ->
    case catch check_equip(ItemTypeID) of
        {false, Code} ->
            ?sendself(#sc_skin_activate{result=Code});
        {true, SkinInfo} ->
            do_equip(ItemTypeID, SkinInfo)
    end.

cs_skin_info(_) ->
    send_skin_info().

cs_skin_demount(_)->
    case catch do_cs_skin_demount() of
        {false,Reason}->
            ?sendself(#sc_skin_demount{result=Reason});
        true->
            ?sendself(#sc_skin_demount{result=0})
    end.

%%%%%============================================================================
%%%功能函数
check_compose(ItemTypeID,RoleInfo) ->
    case data_skin:get(ItemTypeID) of
        undefined ->
            {false, 3};
        #data_skin{activate = AList,isMale=IsMale} ->
            case check_male(IsMale,RoleInfo) of
                false->
                    {false,4};
                true->
                    case has_skin(ItemTypeID) of
                        false->
                            {Material,EffectId} = get_activte_config(AList,0),
                            Role = role_data:get_roleInfo(),
	                        BagItem = role_data:get_bagItem(),
                            case item_lib:check_need_list(Role, Material, BagItem, []) of
                                false ->
                                    {false, 1};
                                {true, Cost} ->
                                    {true, Cost,EffectId}
                            end;
                        true->
                            {false,2}
                    end
            end
    end.

do_compose(ItemTypeID, {BagItem, #role{roleID=RoleID}, _BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu},EffectId) ->
    %% 扣除消耗
    lists:foreach(fun({MType, Money}) ->
                    case Money > 0 of
                        true ->
                            Role = role_data:get_roleInfo(),
                            role_lib:deduct_money_f(Role, MType, Money, ?MONEY_DEC_TYPE_COMPOSE_SKIN, ItemTypeID, "");
                        _ ->
                            ignore
                    end
                end, [{coin, NeedCoin}, {gold, NeedGold}, {reputation, NeedRepu}]),
    %% 扣除物品消耗
	{Date, _} = Time = erlang:localtime(),
	LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_COMPOSE_SKIN, ItemTypeID, ""),
	role_data:set_bagItem(BagItem),
	DelItemIDList = [E||#item{itemUID=E}<-DelList],
	UpdateList2 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateList],
	%% 提醒客户端更新物品
	?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
	?sendself(#sc_item_update{updateList=UpdateList2}),

    IsFirst = 
        case has_skin(ItemTypeID) of
            true ->
                %% 发送皮肤物品,并激活皮肤属性
                NewItem = #new_item{itemTypeID=ItemTypeID, itemNum=1, itemLevel=1, itemRank=1},
                role_reward:handle_item_f(role_data:get_roleInfo(), [NewItem], ?MONEY_ADD_TYPE_COMPOSE_SKIN, ItemTypeID, ""),
                false;
            _ ->
                add_skin(ItemTypeID,EffectId),
                true
        end,

    role_data:set_interval_persist_flag(),
    ?sendself(#sc_skin_compose{result=0, is_first=IsFirst}).
            
%% 添加皮肤
add_skin(ItemTypeID,EffectId) ->
    SkinInfo = #skin_info{has=CurHas} = get_skin_info(),
    case data_skin:get(ItemTypeID) of
        ?undefined->
            ?ERR("undefined skinID:~w ~n",[ItemTypeID]),
            ignore;
        _ ->
            NewSkin = #skin{id=ItemTypeID,activateIDList=[EffectId]},
            SkinInfo2 = SkinInfo#skin_info{has=[NewSkin|CurHas]},
            set_skin_info(SkinInfo2)
    end.

check_activate(ItemTypeID, Pos) ->
    #skin_info{has=Has} = get_skin_info(),
    case lists:keytake(ItemTypeID, #skin.id, Has) of
        false ->
            {false, 3};
        {value, #skin{arank = Arank} = Skin, Has2} ->
            if 
                Pos =< Arank ->
                    {false, 5};
                Pos =/= Arank + 1 ->
                    {false, 2};
                true ->
                    #data_skin{activate=AList} = data_skin:get(ItemTypeID),
                    Len = erlang:length(AList),
                    case Pos > Len of
                        true ->
                            {false, 6};
                        _ ->
                            {Need,EffectId} = get_activte_config(AList,Pos),
                            % ?ERR("Need:~w EffectId:~w Pos:~w AList:~w ~n",[Need,EffectId,Pos,AList]),
                            Role = role_data:get_roleInfo(),
                            BagItem = role_data:get_bagItem(),
                            case item_lib:check_need_list(Role, Need, BagItem, []) of
                                false ->
                                    {false, 1};
                                {true, Cost} ->
                                    {true, Cost, EffectId,Has2, Skin#skin{arank=Pos,activateIDList=[EffectId|Skin#skin.activateIDList]}}
                            end
                    end
            end 
    end.

%% 激活属性
do_activate({BagItem, #role{roleID=RoleID}, _BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu}, EffectId, Has, Skin,_ItemTypeID,_Pos) ->
    %% 扣除消耗
    lists:foreach(fun({MType, Money}) ->
                    case Money > 0 of
                        true ->
                            Role = role_data:get_roleInfo(),
                            role_lib:deduct_money_f(Role, MType, Money, ?MONEY_DEC_TYPE_ACTIVATE_SKIN, EffectId, "");
                        _ ->
                            ignore
                    end
                end, [{coin, NeedCoin}, {gold, NeedGold}, {reputation, NeedRepu}]),
    %% 扣除物品消耗
	{Date, _} = Time = erlang:localtime(),
	LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ACTIVATE_SKIN, EffectId, ""),
	role_data:set_bagItem(BagItem),
	DelItemIDList = [E||#item{itemUID=E}<-DelList],
	UpdateList2 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateList],
	%% 提醒客户端更新物品
	?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
	?sendself(#sc_item_update{updateList=UpdateList2}),
    SkinInfo = get_skin_info(),
    set_skin_info(SkinInfo#skin_info{has=[Skin|Has]}),
    role_data:set_interval_persist_flag(),
    ?sendself(#sc_skin_activate{result=0}).

check_equip(ItemTypeID) ->
    SkinInfo = #skin_info{has=Has} = get_skin_info(),
    case lists:keyfind(ItemTypeID, #skin.id, Has) of
        false ->
            {false, 1};
        _ ->
            {true, SkinInfo}
    end.

check_male(IsMale,RoleInfo)->
    IsMale=:=RoleInfo#role.isMale.
       
%% 装备皮肤
do_equip(ItemTypeID,SkinInfo) ->
    SkinInfo2 = SkinInfo#skin_info{equip=ItemTypeID},
    set_skin_info(SkinInfo2),
    role_data:set_interval_persist_flag(),
    ?sendself(#sc_skin_equip{result=0}).
    
%% 发送皮肤信息
send_skin_info() ->
    #role{transmigration=Transmigration} = role_data:get_roleInfo(),
    #skin_info{has=Has, equip=Equip} = get_skin_info(),
    {Skins,TotalAdd} = 
        lists:foldl(fun(#skin{id=ItemTypeID, arank=Arank}=Skin, {Acc,#p_skin_buff{ger_attack_plus=OldGerAttackPlus,ger_demage_sub=OldGerDemagePlus,
            trainer_mark_plus=OldTrainerMarkPlus,trainer_demage_plus=OldTrainerDemagePlus}}) ->
                            #skin_buff{ger_attack_plus=GerAttackPlus,ger_demage_sub=GerDemageSub,
                                trainer_mark_plus=TrainerMarkPlus,even_trainer_demage_plus=TrainerDemagePlus} = calculate_single_skin_buff(Skin,Equip,Transmigration),
                            NewPSkinBuff = #p_skin_buff{ger_attack_plus=OldGerAttackPlus+GerAttackPlus,ger_demage_sub=OldGerDemagePlus+GerDemageSub,
                                trainer_demage_plus=OldTrainerDemagePlus+TrainerDemagePlus,trainer_mark_plus=OldTrainerMarkPlus+TrainerMarkPlus},
                            {[#p_skin{itemTypeID=ItemTypeID, pos=Arank}|Acc],NewPSkinBuff}
                    end, {[],#p_skin_buff{}}, Has),
    ?sendself(#sc_skin_info{skins=Skins, equip=Equip,totalAdd=TotalAdd}).

%%完成皮肤的卸下功能能
do_cs_skin_demount()->
    #skin_info{equip=Equip,has=HasSkin} =SkinInfo = get_skin_info(),
    case Equip =/= 0 of
        true->
            set_skin_info(SkinInfo#skin_info{equip=0}),
            true;
        false->
            case HasSkin=:=[] of
                true->
                    {false,2};
                false->
                    {false,1}
            end
    end.

%% 读取存盘数据
init_skin_info(RoleID) ->
    #skin_info{has=Has} = SkinInfo = db_sql:get_skin_info(RoleID),
    NewHas = [update_skin_activatelist(Skin)||Skin<-Has],
    set_skin_info(SkinInfo#skin_info{has=NewHas}).
%% 设置皮肤数据
set_skin_info(SkinInfo) ->
    % ?ERR("set_skin_info:~w ~n",[SkinInfo]),
    erlang:put(?SKIN, SkinInfo).

%% 获得皮肤数据
get_skin_info() ->
    case erlang:get(?SKIN) of
        undefined ->
            #skin_info{};
        Any ->
            Any
    end.

has_skin(SkinID) ->
    #skin_info{has=Has} = get_skin_info(),
    lists:keyfind(SkinID, #skin.id, Has) =/= false.

has_skinshape(ShapeID)->
    #skin_info{has=Has} = get_skin_info(),
    ShapeList = lists:foldl(fun(#skin{id=SkinID},Acc)->
        case data_skin:get({data_skin_shape,SkinID}) of
            ?undefined->
                ?ERR("undefined skinID:~w ~n",[SkinID]),
                Acc;
            {_,_,List}->
                Acc++List
        end
    end,[],Has),
    lists:member(ShapeID,ShapeList).

get_activte_config(AList,Pos) when is_list(AList)->
    case lists:keyfind(Pos,#skin_activate_unit.pos,AList) of
        false->
            ?ERR("undefined pos:~w AList:~w ~n",[Pos,AList]),
            erlang:throw({false,4});
        Find->
            {Find#skin_activate_unit.need,Find#skin_activate_unit.effectID}
    end. 

%%计算训练师皮肤为玩家带来的buff |return #skin_buff
calculate_skin_buff(#skin_info{has=[],equip=0},_Transmigrate) ->
    [];
calculate_skin_buff(SkinInfo,Transmigration) when is_record(SkinInfo,skin_info)->
    case is_list(SkinInfo#skin_info.has) of
        true->
            lists:foldl(fun(Skin,Acc)->
                case data_skin:get(Skin#skin.id) of
                    ?undefined->
                        ?ERR("undefined skinID:~w ~n",[Skin#skin.id]),
                        Acc;
                    #data_skin{fetter=FetterList}->
                        SingleBuff = calculate_single_skin_buff(Skin,SkinInfo#skin_info.equip,Transmigration),
                        [#skin_fetter_buff{buff=SingleBuff,fetterGerList=FetterList}|Acc]
                end
            end
            ,[],SkinInfo#skin_info.has);
        false->
            []
    end;
calculate_skin_buff(SkinInfo,_Transmigrate)->
    ?ERR("not skin_info:~w~n",[SkinInfo]),
    [].    

%%v3.3.0版本修改了转生对皮肤buff的影响
calculate_single_skin_buff(Skin,Equip,Transmigration) when is_record(Skin,skin)->
    ActivateIDList = Skin#skin.activateIDList,
    case is_list(ActivateIDList) of
        true->
            case ActivateIDList of
                []->
                    #skin_buff{};
                _ ->
                    lists:foldl(fun(ActivateID,Acc)->
                        case data_skin_effect:get({ActivateID,Transmigration}) of
                            ?undefined->
                                ?ERR("undefined ActivateID:~w ~n",[ActivateID]),
                                Acc;
                            #data_skin_effect{type=Type,args=Args,is_need_equip=NeedEquip}->
                                EffectBuff = get_effect_buff(Type,Args,NeedEquip,Equip=:=Skin#skin.id),
                                skin_buff_plus(Acc,EffectBuff)
                        end
                    end,#skin_buff{},ActivateIDList)
            end; 
        false->
            #skin_buff{}
    end.

%%返回激活的技能ID对应的属性加成
get_effect_buff(Type,Args,NeedEquip,IsEquip)->
    case NeedEquip =:= false orelse NeedEquip =:= IsEquip of
        true->
            case Type of
                ?FETTER_GER_DEMAGE_PLUS_TYPE->
                    #skin_buff{ger_attack_plus=hd(Args)};
                ?FETTER_GER_DEMAGE_DEC_TYPE->
                    #skin_buff{ger_demage_sub=hd(Args)};
                ?TRAINER_MARK_PLUS_TYPE->
                    #skin_buff{trainer_mark_plus=hd(Args)};
                ?TRAINER_EVEN_DEMAGE_PLUS->
                    #skin_buff{even_trainer_demage_plus=hd(Args)}
            end;
        false->
            #skin_buff{}
    end.

skin_buff_plus(BuffA,BuffB) when is_record(BuffA,skin_buff) andalso is_record(BuffB,skin_buff)->
    #skin_buff{ger_attack_plus=GAPA,ger_demage_sub=GDSA,trainer_mark_plus=TMPA,even_trainer_demage_plus=ETDPA}=BuffA,
    #skin_buff{ger_attack_plus=GAPB,ger_demage_sub=GDSB,trainer_mark_plus=TMPB,even_trainer_demage_plus=ETDPB}=BuffB,
    #skin_buff{ger_attack_plus=GAPA+GAPB,ger_demage_sub=GDSA+GDSB,trainer_mark_plus=TMPA+TMPB,even_trainer_demage_plus=ETDPA+ETDPB};
skin_buff_plus(BuffA,BuffB)->
    ?ERR("undefiend skin_buff BuffA:~w BuffB:~w ~n",[BuffA,BuffB]),
    #skin_buff{}.

get_skinbuff_for_ger(SkinFetterBuffList,GerID) when is_list(SkinFetterBuffList)->
    lists:foldl(fun(SkinFetterBuff,Acc)->
        %%此处增加了[-1]这项，直接针对所有的精灵
        case lists:member(GerID,SkinFetterBuff#skin_fetter_buff.fetterGerList) orelse SkinFetterBuff#skin_fetter_buff.fetterGerList=:=[-1] of
            true->
                skin_buff_plus(SkinFetterBuff#skin_fetter_buff.buff,Acc);
            false->
                Acc
        end
    end,#skin_buff{},SkinFetterBuffList);
get_skinbuff_for_ger(SkinFetterBuffList,_GerID)->
    ?ERR("illegal SkinFetterBuffList:~w ~n",[SkinFetterBuffList]),
    #skin_buff{}.

get_skinbuff_for_trainer(SkinFetterBuffList) when is_list(SkinFetterBuffList)->
    lists:foldl(fun(SkinFetterBuff,Acc)->
        skin_buff_plus(SkinFetterBuff#skin_fetter_buff.buff,Acc)
    end,#skin_buff{},SkinFetterBuffList);
get_skinbuff_for_trainer(SkinFetterBuffList)->
    ?ERR("illegal SkinFetterBuffList:~w ~n",[SkinFetterBuffList]),
    #skin_buff{}.

get_skinbuff_for_trainer_even(SkinBuff,0)->
    SkinBuff;
get_skinbuff_for_trainer_even(_SkinBuff,1)->
    #skin_buff{}.

transforSkinInfo2PSkinInfo(SkinInfo) when is_record(SkinInfo,skin_info)->
    #p_skin_info{equip=SkinInfo#skin_info.equip};
transforSkinInfo2PSkinInfo(SkinInfo)->
    ?ERR("illegal record SkinInfo:~w ~n",[SkinInfo]),
    #p_skin_info{equip=0}.

get_skin_activateIDList(SkinID,Rank) ->
    case data_skin:get(SkinID) of
        ?undefined->
            [];
        #data_skin{activate=AList}->
            [ID||#skin_activate_unit{pos=Pos,effectID=ID}<-AList,Pos =< Rank]
    end.

update_skin_activatelist(Skin) when is_record(Skin,skin)->
    ActivateIDList = get_skin_activateIDList(Skin#skin.id,Skin#skin.arank),
    Skin#skin{activateIDList=ActivateIDList};
update_skin_activatelist(Skin)->
    ?ERR("illegal skin :~w ~n",[Skin]),
    #skin{}.

test_set_skin_info(RoleID,SkinInfo) when is_record(SkinInfo,skin_info)->
    role_lib:send_server(RoleID,{test_set_skininfo,SkinInfo});
test_set_skin_info(_RoleID,SkinInfo)->
    ?ERR("illegal skin_info:~w ~n",[SkinInfo]).

%%实际在玩家进程中修改皮肤数据
test_set_skininfo(SkinInfo)->
    #skin_info{has=Has}=SkinInfo,
    NewHas = [update_skin_activatelist(Skin)||Skin<-Has],
    set_skin_info(SkinInfo#skin_info{has=NewHas}).
