%% Author dongquan
-module(role_race).

-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_race.hrl").


%% ====================================================================
%% API functions
%% ====================================================================

cs_race_sign(#cs_race_sign{}) ->
    RoleInfo = role_data:get_roleInfo(),
    RoleID = role_data:get_roleID(),
    case erlang:whereis(race_server) of
        ?undefined ->
            ?sendself(#sc_race_sign{reason_code=?REASON_CODE_LOCAL_RACE_SERVER_IS_DOWN});
        _Pid ->
            ?sendself(#sc_race_sign{reason_code=?REASON_CODE_LOCAL_RACE_SERVER_IS_DOWN})
    end.

cs_race_auto_sign(#cs_race_auto_sign{}) ->
    RoleInfo = role_data:get_roleInfo(),
    RoleID = role_data:get_roleID(),
    case erlang:whereis(race_server) of
        ?undefined ->
            ?sendself(#sc_race_auto_sign{reason_code=?REASON_CODE_LOCAL_RACE_SERVER_IS_DOWN});
        _Pid ->
            ?sendself(#sc_race_auto_sign{reason_code=?REASON_CODE_LOCAL_RACE_SERVER_IS_DOWN})
    end.

cs_race_guess(#cs_race_guess{guessCoin=GuessCoin,roleID=GuessRoleID}) ->
%%     CoinValList = data_race:get(coin_val_list),
%%     case lists:member(GuessCoin, CoinValList) of
%%         true ->
%%             #role{roleID=RoleID,coin=RoleCoin} = role_data:get_roleInfo(),
%%             case RoleCoin >= GuessCoin of
%%                 true ->
%%                     erlang:send(race_server, {do_race_guess, RoleID, GuessCoin, GuessRoleID});
%%                 false ->
%%                     ?sendself(#sc_race_guess{result=1})
%%             end;
%%         false ->
            ?sendself(#sc_race_guess{result=5}).
%%     end.

do_race2_fight({do_race2_fight, TarRoleID, AtkRoleID,ExtraInfo})->
%%     {DiceNumA,DiceNumB} = doublematch_match:random_first_atk(),
    {DiceNumA,DiceNumB} = {6,1},
    IsSwitch = DiceNumB>DiceNumA,
    {Result, FightRecord0, _State} = race2_pvp(TarRoleID,IsSwitch),    
    FighterList2 = role_data:get_FighterList_with_effect(role_data:get_equipts_on_ger(),TarRoleID,FightRecord0#sc_fight_request.fighterList),
    FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
    FightResult = #sc_race2_fight{result = 1
                                 ,dice_numA = DiceNumA
                                 ,dice_numB = DiceNumB
                                 ,fight_rec = [FightRecord]},
    erlang:send(race2_server, {do_race2_fight_res,FightResult,AtkRoleID,TarRoleID,ExtraInfo}).

race2_pvp(TarRoleID,IsSwitch) ->
    MyFighterList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentListA = role_talent:get_active_talent_list(),
    TrSpecialA = role_data:get_trSpecial(),
    ASkinInfo = role_skin:get_skin_info(),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-MyFighterList],
%%     ?ERR("race2_pvp ~w ___ ~w ___ ~w",[TarRoleID,IsSwitch,role_lib:regName(TarRoleID)]),
    case erlang:whereis(role_lib:regName(TarRoleID)) of
        ?undefined ->
            DSkinInfo = role_data:get_otherRoleSkinInfo(TarRoleID),
            {TarFighterList,TarLieuAdd,TalentListB,TrSpecialB} = role_data:get_otherRoleFighter(TarRoleID),
%%             ?ERR("race2_pvp ~w",[{MyFighterList, TarFighterList, RoleLieuAdd, TarLieuAdd
%%                                   , TalentListA, TalentListB,TrSpecialA,TrSpecialB
%%                                   ,ASkinInfo,DSkinInfo}])
            TarEquipList = role_data:get_otherRoleItemEquips(TarRoleID),
            %%将数据库中获得的精灵装备列表按照精灵分类
            GerEquipList = role_item:assort_ger_equiplist(TarEquipList),
            TarLegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
            case IsSwitch of
                false ->
                    role_fight:new(MyFighterList, TarFighterList, RoleLieuAdd, TarLieuAdd
                                  , TalentListA, TalentListB,TrSpecialA,TrSpecialB
                                  ,ASkinInfo,DSkinInfo,LegendAddList,TarLegendAddList);
                true ->
                    role_fight:new(TarFighterList,MyFighterList,TarLieuAdd,RoleLieuAdd
                                  ,TalentListB,TalentListA,TrSpecialB,TrSpecialA
                                  ,DSkinInfo,ASkinInfo,TarLegendAddList,LegendAddList)
            end;
        _ ->
            Ref = erlang:make_ref(),
            Info = {race2_attack, MyFighterList,RoleLieuAdd, TalentListA,TrSpecialA,ASkinInfo,LegendAddList,self(), Ref, IsSwitch},
            role_lib:send_server(TarRoleID, Info),
            role_fight:get_result(Ref)
    end.

