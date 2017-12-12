-module(role_dojangrank).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  
-include("def_mail.hrl").  

-export([]).

%% -define(pd_, pd_). 

%% ====================================================================
%% external functions
%% ====================================================================

cs_dojangrank_info(_)->
    Role = role_data:get_roleInfo(),
    Dojangrank = role_data:get_dojangrank_data(),
    VipLevel  = Role#role.vipLevel,
    Max = if
              0 < VipLevel ->
                  lists:nth(VipLevel, data_dojangrank:get(vip_buy_time));
              true ->
                  lists:nth(1, data_dojangrank:get(vip_buy_time))
          end,
    NextCost = if
                    Max > Dojangrank#role_dojangrank.buy_time ->
                        lists:nth(Dojangrank#role_dojangrank.buy_time+1, data_dojangrank:get(buy_cost));
                    true ->
                        0
               end,
    ?sendself(#sc_dojangrank_info{challenge_time = Dojangrank#role_dojangrank.free_time + Dojangrank#role_dojangrank.paid_time
                                 ,buy_cost = NextCost
                                 ,already_buy_time = Dojangrank#role_dojangrank.buy_time
                                 ,can_buy_time = Max - Dojangrank#role_dojangrank.buy_time }).

cs_dojangrank_rank(#cs_dojangrank_rank{index = RankType})->
    Dojangrank = role_data:get_dojangrank_data(),
    #role{roleID =RoleID} = Role = role_data:get_roleInfo(),
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    RankInfo = 
        case ets:match_object(EtsName, #dojangrank{role_id=RoleID, _='_'}) of
            [] ->
                dojangrank_server:to_p_dojangrank_rank(Role,RankType,Dojangrank#role_dojangrank.selected_ger_type_list);
            [Info1] ->
                dojangrank_server:to_p_dojangrank_rank(Info1)
        end,
    ?sendself(#sc_dojangrank_rank{index = RankType
                                 ,p_dojangrank_rank_self = RankInfo
                                 ,p_dojangrank_rank_list = dojangrank_server:get_ranklist_sub(RankType,RoleID)}).

cs_dojangrank_buy(#cs_dojangrank_buy{buy_time=BuyTime})->
    Role = role_data:get_roleInfo(),
    Dojangrank = role_data:get_dojangrank_data(),
    VipLevel  = Role#role.vipLevel,
    Max = if
              0 < VipLevel ->
                  lists:nth(VipLevel, data_dojangrank:get(vip_buy_time));
              true ->
                  lists:nth(1, data_dojangrank:get(vip_buy_time))
          end,
    if			
        BuyTime > 0 andalso
        Max >= (Dojangrank#role_dojangrank.buy_time + BuyTime) ->
            Cost = lists:foldl(fun(T,AccSum)-> 
                                    AccSum + lists:nth(T, data_dojangrank:get(buy_cost))
                               end, 0, lists:seq(Dojangrank#role_dojangrank.buy_time + 1
                                                ,Dojangrank#role_dojangrank.buy_time + BuyTime)),
            NextCost = if
                            Max > (Dojangrank#role_dojangrank.buy_time + BuyTime) ->
                                lists:nth(Dojangrank#role_dojangrank.buy_time + BuyTime + 1, data_dojangrank:get(buy_cost));
                            true ->
                                0
                       end,
            case role_lib:check_money(Role, gold, Cost) of
                false ->
                    ?sendself(#sc_dojangrank_buy{result = 3
                                                ,new_buy_cost = NextCost
                                                ,already_buy_time = Dojangrank#role_dojangrank.buy_time
                                                ,can_buy_time = Max - Dojangrank#role_dojangrank.buy_time });
                true ->
                    role_lib:deduct_gold_f(Role, Cost, ?MONEY_DEC_TYPE_DOJANGRANK_BUY, Dojangrank#role_dojangrank.buy_time, ""),
                    role_data:set_dojangrank_data(Dojangrank#role_dojangrank{paid_time = Dojangrank#role_dojangrank.paid_time + BuyTime
                                                                            ,buy_time= Dojangrank#role_dojangrank.buy_time + BuyTime}),
                    ?sendself(#sc_dojangrank_buy{result = 1
                                                ,new_buy_cost = NextCost
                                                ,already_buy_time = Dojangrank#role_dojangrank.buy_time + BuyTime
                                                ,can_buy_time = Max - Dojangrank#role_dojangrank.buy_time - BuyTime})
            end;
        true ->
            ?sendself(#sc_dojangrank_buy{result = 2
                                        ,new_buy_cost = 0
                                        ,already_buy_time = Dojangrank#role_dojangrank.buy_time
                                        ,can_buy_time = 0})
    end.

cs_dojangrank_select_ger_type(#cs_dojangrank_select_ger_type{index = RankType
                                              ,selected_ger_type = SelectedGerType})->
    Dojangrank = role_data:get_dojangrank_data(),
    #role{roleID =RoleID} = Role = role_data:get_roleInfo(),
    L1 = [E#gerSimple.gerTypeID||E<-role_data:get_gerBag()],
    L2 = [E#ger.gerBase#gerBase.gerTypeID||E<-role_data:get_posListT()],
    L3 = [E#ger.gerBase#gerBase.gerTypeID||E<-role_data:get_lieuposList()],
    ?INFO("cs_dojangrank_select_ger_type ~w",[L1++L2++L3]),
    case lists:member(SelectedGerType div 256, L1++L2++L3) of
        true ->
            NewSelectedGerType = util:nth_replace(RankType,Dojangrank#role_dojangrank.selected_ger_type_list,SelectedGerType),
            role_data:set_dojangrank_data(Dojangrank#role_dojangrank{selected_ger_type_list=NewSelectedGerType}),
            dojangrank_server:set_selected_gerType(RoleID,RankType,SelectedGerType),
            ?sendself(#sc_dojangrank_select_ger_type{index = RankType
                                                    ,selected_ger_type = SelectedGerType
                                                    ,result = 1});
        false ->
            ?sendself(#sc_dojangrank_select_ger_type{index = RankType
                                                    ,selected_ger_type = SelectedGerType
                                                    ,result = 2})
    end.

cs_dojangrank_fight(#cs_dojangrank_fight{index = RankType
                                        ,enemy_role_id = EnemyRoleId
                                        ,rank = EnemyRank})->
    Dojangrank = role_data:get_dojangrank_data(),
    #role{roleID =RoleID,roleName=RoleName} = Role = role_data:get_roleInfo(),
    FighterList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    Talent = role_talent:get_active_talent_list(),
    TrSpecial = role_data:get_trSpecial(),
    case check_fight(Dojangrank,RankType,FighterList,EnemyRoleId,EnemyRank) of
        {true,NewDojangrank,PayType} ->
            role_data:set_dojangrank_data(NewDojangrank),
            dojangrank_server:dojangrank_fight(RoleID,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,Role#role.fightPower
                                              ,{FighterList,RoleLieuAdd,Talent,TrSpecial});
        {false,R} ->
            ?INFO("cs_dojangrank_fight ~w",[[data_ger:get(E#ger.gerBase#gerBase.gerTypeID)||E<-FighterList]]),
            EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
            RankInfo = 
                case ets:match_object(EtsName, #dojangrank{role_id=RoleID, _='_'}) of
                    [] ->
                        dojangrank_server:to_p_dojangrank_rank(Role,RankType,Dojangrank#role_dojangrank.selected_ger_type_list);
                    [Info1] ->
                        dojangrank_server:to_p_dojangrank_rank(Info1)
                end,
            ?sendself(#sc_dojangrank_fight{result = R
                                          ,fightInfo = []
                                          ,p_dojangrank_rank_self = RankInfo
                                          ,p_dojangrank_rank_list = dojangrank_server:get_ranklist_sub(RankType,RoleID)
                                          ,reward = #p_reward_info{coin       = 0
                                                                  ,roleExp     = 0
                                                                  ,gerExp      = 0
                                                                  ,gold        = 0
                                                                  ,reputation  = 0
                                                                  ,itemList    = []
                                                                  ,gerList     = []}})
    end.

cs_dojangrank_replay_list(_) ->
    Dojangrank = role_data:get_dojangrank_data(),  %% TODO 改回来
%%     List = lists:nth(1,Dojangrank#role_dojangrank.local_fight_rec_list),
    List = Dojangrank#role_dojangrank.local_fight_rec_list,
    ?sendself(#sc_dojangrank_replay_list{infoList = List}).

cs_dojangrank_replay_detail(#cs_dojangrank_replay_detail{replayUID = ReplayUID})->
    Dojangrank = role_data:get_dojangrank_data(),
%%     case lists:member(true, [lists:keymember(ReplayUID, #p_pvp_replay_info.replayUID, L)||L<- Dojangrank#role_dojangrank.local_fight_rec_list]) of
    case lists:keymember(ReplayUID, #p_pvp_replay_info.replayUID, Dojangrank#role_dojangrank.local_fight_rec_list) of
        true ->
            case db_sql:get_dojangrank_fightrec(ReplayUID) of
                []->
                    ?sendself(#sc_dojangrank_replay_detail{result = 2
                                                          ,fightInfo = []});
                Rec ->
                    ?sendself(#sc_dojangrank_replay_detail{result = 1
                                                          ,fightInfo = [Rec]})
            end;    
        false ->
            ?sendself(#sc_dojangrank_replay_detail{result = 2
                                                  ,fightInfo = []})
    end.

cs_dojangrank_ger_view_other(#cs_dojangrank_ger_view_other{tarRoleID = TarRoleID
                                                          ,rank = RankType})->
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    case ets:match_object(EtsName, #dojangrank{role_id=TarRoleID, _='_'}) of
        [] ->
            ignore;
        [DefRankInfo] ->
            RolePublic=role_lib:get_rolePublic(TarRoleID),
            Reply = role_ger:ger_view_info(RolePublic, DefRankInfo#dojangrank.fighter_array_cfg),
            ?sendself(#sc_dojangrank_ger_view_other{tarRoleID       =Reply#sc_ger_view_other.tarRoleID
                                                   ,roleName        =Reply#sc_ger_view_other.roleName
                                                   ,roleLevel       =Reply#sc_ger_view_other.roleLevel
                                                   ,fightPower      =DefRankInfo#dojangrank.fighter_power
                                                   ,gerList         =Reply#sc_ger_view_other.gerList})
    end.
cs_dojangrank_self_rank(_)->
    #role{roleID =RoleID} = Role = role_data:get_roleInfo(),
    RankNumList = lists:map(fun(T)-> 
                    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',T])),
                    case ets:match_object(EtsName, #dojangrank{role_id=RoleID, _='_'}) of
                        [] ->
                            1001;
                        [Info1] ->
                            Info1#dojangrank.rank
                    end
        end, lists:seq(1, 8)),
    ?sendself(#sc_dojangrank_self_rank{rank_num_list=RankNumList}).


cs_dojangrank_world_info(_)->
    #role{roleID = RoleID} = Role = role_data:get_roleInfo(),
    Dojangrank = role_data:get_dojangrank_data(),
    VipLevel  = Role#role.vipLevel,
    Max = if
              0 < VipLevel ->
                  lists:nth(VipLevel, data_dojangrank:get(world_vip_buy_time));
              true ->
                  lists:nth(1, data_dojangrank:get(world_vip_buy_time))
          end,
    NextCost = if
                    Max > Dojangrank#role_dojangrank.buy_time ->
                        lists:nth(Dojangrank#role_dojangrank.buy_time+1, data_dojangrank:get(world_buy_cost));
                    true ->
                        0
               end,
    OpenState = get_open_state(),
    AttendFlag = 
        lists:foldl(fun(RankType,Acc)-> 
                        case is_attend(RankType,RoleID) of
                            false -> Acc;
                            true -> Acc + erlang:trunc(math:pow(2,RankType - 1))
                        end
            end, 0, lists:seq(1, 8)),
    {YY,MM,DD} = erlang:date(),
    {{StartYear,StartMouth,_},_} = data_dojangrank:get(start_date),
    SessionNum = 
        if
            YY =:= StartYear ->
                MM - StartMouth + 1;
            true ->
                MM
        end,
    ChallengeTime = 
        lists:map(fun(RankType)-> 
                lists:nth(RankType, Dojangrank#role_dojangrank.world_free_time) + lists:nth(RankType, Dojangrank#role_dojangrank.world_paid_time)
            end, lists:seq(1, 8)),
    AlreadyBuyTime = 
        lists:map(fun(RankType)-> 
                lists:nth(RankType, Dojangrank#role_dojangrank.world_buy_time)
            end, lists:seq(1, 8)),
    CanBuyTime = 
        lists:map(fun(RankType)-> 
                Max - lists:nth(RankType, Dojangrank#role_dojangrank.world_buy_time)
            end, lists:seq(1, 8)),
    ?sendself(#sc_dojangrank_world_info{challenge_time = ChallengeTime
                                       ,buy_cost = NextCost
                                       ,already_buy_time = AlreadyBuyTime
                                       ,can_buy_time = CanBuyTime
                                       ,open_state = OpenState
                                       ,next_timestamp = get_season_endtime()
                                       ,attend_flag = AttendFlag
                                       ,session_num = SessionNum}).

cs_dojangrank_world_rank(#cs_dojangrank_world_rank{index = RankType
                                                  ,start = Start
                                                  ,length = Length
                                                  ,type = Type})->
    #role{roleID = RoleID} = role_data:get_roleInfo(),
    erlang:send(dojangrank_server, {req_dojangrank_world_rank,RoleID,RankType,Start,Length,Type}).

cs_dojangrank_world_buy(#cs_dojangrank_world_buy{rank_type = RankType
                                                ,buy_time=BuyTime})->
    Role = role_data:get_roleInfo(),
    Dojangrank = role_data:get_dojangrank_data(),
    VipLevel  = Role#role.vipLevel,
    Max = if
              0 < VipLevel ->
                  lists:nth(VipLevel, data_dojangrank:get(world_vip_buy_time));
              true ->
                  lists:nth(1, data_dojangrank:get(world_vip_buy_time))
          end,
    WorldPaidTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_paid_time),
    WorldBuyTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_buy_time),
    if          
        BuyTime > 0 andalso
        Max >= (WorldBuyTime + BuyTime) ->
            Cost = lists:foldl(fun(T,AccSum)-> 
                                    AccSum + lists:nth(T, data_dojangrank:get(world_buy_cost))
                               end, 0, lists:seq(WorldBuyTime + 1
                                                ,WorldBuyTime + BuyTime)),
            NextCost = if
                            Max > (WorldBuyTime + BuyTime) ->
                                lists:nth(WorldBuyTime + BuyTime + 1, data_dojangrank:get(world_buy_cost));
                            true ->
                                0
                       end,
            case role_lib:check_money(Role, gold, Cost) of
                false ->
                    ?sendself(#sc_dojangrank_world_buy{rank_type = RankType
                                                ,result = 3
                                                ,new_buy_cost = NextCost
                                                ,already_buy_time = WorldBuyTime
                                                ,can_buy_time = Max - WorldBuyTime });
                true ->
                    role_lib:deduct_gold_f(Role, Cost, ?MONEY_DEC_TYPE_DOJANGRANK_WORLD_BUY, WorldBuyTime, ""),
                    NewWorldPaidTimeList = util:nth_replace(RankType, Dojangrank#role_dojangrank.world_paid_time, WorldPaidTime + BuyTime),
                    NewWorldBuyTime = util:nth_replace(RankType, Dojangrank#role_dojangrank.world_buy_time, WorldBuyTime + BuyTime),
                    role_data:set_dojangrank_data(Dojangrank#role_dojangrank{world_paid_time = NewWorldPaidTimeList
                                                                            ,world_buy_time = NewWorldBuyTime}),
                    ?sendself(#sc_dojangrank_world_buy{rank_type = RankType
                                                      ,result = 1
                                                      ,new_buy_cost = NextCost
                                                      ,already_buy_time = WorldBuyTime + BuyTime
                                                      ,can_buy_time = Max - WorldBuyTime - BuyTime})
            end;
        true ->
            ?sendself(#sc_dojangrank_world_buy{rank_type = RankType
                                              ,result = 2
                                              ,new_buy_cost = 0
                                              ,already_buy_time = WorldBuyTime
                                              ,can_buy_time = 0})
    end.

cs_dojangrank_world_refresh_enemy(#cs_dojangrank_world_refresh_enemy{rank_type = RankType, refresh_type = 0})->
    #role{roleID = RoleID} = role_data:get_roleInfo(),
    Dojangrank = role_data:get_dojangrank_data(),
    DojangrankRankSelf = dojangrank_server:get_world_dojangrank(RoleID,RankType),
    {EnemyList,_} = get_enemy_info_list(RankType,DojangrankRankSelf#p_dr_world_rank.rank,Dojangrank,false,false),
    WorldRefreshTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_refresh_time),
    Cost = 
        if
            WorldRefreshTime > 0 ->
                data_dojangrank:get(refresh_cost) * erlang:trunc(math:pow(2, max(0,((WorldRefreshTime + 4) div 5) - 1)));
            true ->
                0
        end,
    ?sendself(#sc_dojangrank_world_refresh_enemy{enemy_list = check_rank_out(EnemyList)
                                                ,refresh_cost =  Cost
                                                ,p_dojangrank_rank_self = check_rank_out(DojangrankRankSelf)
                                                ,result = 1});
cs_dojangrank_world_refresh_enemy(#cs_dojangrank_world_refresh_enemy{rank_type = RankType, refresh_type = 1})->
    #role{roleID = RoleID} = Role = role_data:get_roleInfo(),
    Dojangrank = role_data:get_dojangrank_data(),
    WorldRefreshTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_refresh_time),
    Cost = 
        if
            WorldRefreshTime > 0 ->
                data_dojangrank:get(refresh_cost) * erlang:trunc(math:pow(2, max(0,((WorldRefreshTime + 4) div 5) - 1)));
            true ->
                0
        end,
    NextCost = data_dojangrank:get(refresh_cost) * erlang:trunc(math:pow(2, max(0,((WorldRefreshTime + 5) div 5) - 1))),
    DojangrankRankSelf = dojangrank_server:get_world_dojangrank(RoleID,RankType),
    case role_lib:check_money(Role, gold, Cost) of
        false ->
            {EnemyList,_} = get_enemy_info_list(RankType,DojangrankRankSelf#p_dr_world_rank.rank,Dojangrank,false,false),
            ?sendself(#sc_dojangrank_world_refresh_enemy{enemy_list = check_rank_out(EnemyList)
                                                        ,refresh_cost =  Cost
                                                        ,p_dojangrank_rank_self = check_rank_out(DojangrankRankSelf)
                                                        ,result = 2});
        true ->
            role_lib:deduct_gold_f(Role, Cost, ?MONEY_DEC_TYPE_DOJANGRANK_WORLD_REFRESH, 1, ""),
            {EnemyList,_} = get_enemy_info_list(RankType,DojangrankRankSelf#p_dr_world_rank.rank
                                               ,Dojangrank,true,false),
            ?sendself(#sc_dojangrank_world_refresh_enemy{enemy_list = check_rank_out(EnemyList)
                                                        ,refresh_cost =  NextCost
                                                        ,p_dojangrank_rank_self = check_rank_out(DojangrankRankSelf)
                                                        ,result = 1})
    end.

cs_dojangrank_world_select_ger_type(#cs_dojangrank_world_select_ger_type{index = RankType
                                                                        ,selected_ger_type = SelectedGerType})->
    Dojangrank = role_data:get_dojangrank_data(),
    #role{roleID =RoleID} = Role = role_data:get_roleInfo(),
    L1 = [E#gerSimple.gerTypeID||E<-role_data:get_gerBag()],
    L2 = [E#ger.gerBase#gerBase.gerTypeID||E<-role_data:get_posListT()],
    L3 = [E#ger.gerBase#gerBase.gerTypeID||E<-role_data:get_lieuposList()],
    ?INFO("cs_dojangrank_select_ger_type ~w",[L1++L2++L3]),
    case is_attend(RankType,RoleID) of
        false ->
            ?sendself(#sc_dojangrank_world_select_ger_type{index = RankType
                                                    ,selected_ger_type = SelectedGerType
                                                    ,result = 3});
        true ->
            DojangrankRankSelf = dojangrank_server:get_world_dojangrank(RoleID,RankType),
            if
                SelectedGerType =:= DojangrankRankSelf#p_dr_world_rank.selected_ger_type ->
                    ?sendself(#sc_dojangrank_world_select_ger_type{index = RankType
                                                            ,selected_ger_type = SelectedGerType
                                                            ,result = 1});
                true ->
                    case lists:member(SelectedGerType div 256, L1++L2++L3) of
                        true ->
                            dojangrank_world_server:set_world_select_ger_type(data_setting:get(server_id),RoleID,RankType,SelectedGerType);
                        false ->
                            ?sendself(#sc_dojangrank_world_select_ger_type{index = RankType
                                                                    ,selected_ger_type = SelectedGerType
                                                                    ,result = 2})
                    end
            end
    end.

cs_dojangrank_world_fight(#cs_dojangrank_world_fight{index = RankType
                                                    ,enemy_role_id = EnemyRoleId
                                                    ,rank = EnemyRank})->
    Dojangrank = role_data:get_dojangrank_data(),
    #role{roleID =RoleID,roleName=RoleName} = Role = role_data:get_roleInfo(),
    FighterList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    Talent = role_talent:get_active_talent_list(),
    TrSpecial = role_data:get_trSpecial(),
    #skin_info{equip=Equip} = SkinInfo = role_skin:get_skin_info(),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-FighterList],
    case check_world_fight(RoleID,Dojangrank,RankType,FighterList,EnemyRoleId,EnemyRank) of
        {true,NewDojangrank,PayType} ->
            ServerID = data_setting:get(server_id),
            role_data:set_dojangrank_data(NewDojangrank),
            NewPDR = 
                #p_dr_world_rank{roleID = RoleID
                                ,fightPower = Role#role.fightPower 
                                ,isMale = Role#role.isMale 
                                ,title = Role#role.title 
                                ,head = Role#role.head 
                                ,level = Role#role.level 
                                ,roleName = RoleName
                                ,vip = Role#role.vipLevel
                                ,rank = 0
                                ,selected_ger_type = 0
                                ,skinID = Equip
                                ,serverID = ServerID
                                ,ger_info_list = dojangrank_server:to_dr_ger_info(FighterList)
                                ,win_num = 0
                                ,lost_num = 0
                                ,total_num = 0
                                ,rank_score = 0},
            %%{FighterListA,RoleLieuAddA,TalentA,TrSpecialA,SkinInfoA,LegendAddListA}
            dojangrank_world_server:dojangrank_world_fight(RoleID,ServerID,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,Role#role.fightPower,NewPDR
                                                          ,{FighterList,RoleLieuAdd,Talent,TrSpecial,SkinInfo,LegendAddList});
        {false,R} ->
%%             ?INFO("cs_dojangrank_fight ~w",[[data_ger:get(E#ger.gerBase#gerBase.gerTypeID)||E<-FighterList]]),
            DojangrankRankSelf = dojangrank_server:get_world_dojangrank(RoleID,RankType),
            {EnemyList,_} = get_enemy_info_list(RankType,DojangrankRankSelf#p_dr_world_rank.rank,Dojangrank,false,false),
            ?sendself(#sc_dojangrank_world_fight{result = R
                                                ,fightInfo = []
                                                ,p_dojangrank_rank_self = check_rank_out(DojangrankRankSelf)
                                                ,p_dojangrank_rank_list = check_rank_out(EnemyList)
                                                ,reward = #p_reward_info{coin       = 0
                                                                        ,roleExp     = 0
                                                                        ,gerExp      = 0
                                                                        ,gold        = 0
                                                                        ,reputation  = 0
                                                                        ,itemList    = []
                                                                        ,gerList     = []}})
    end.

cs_dojangrank_world_replay_list(#cs_dojangrank_world_replay_list{index = RankIndex}) when  1 =< RankIndex andalso RankIndex =< 8 ->
    Dojangrank = role_data:get_dojangrank_data(),
    List = lists:nth(RankIndex,Dojangrank#role_dojangrank.world_fight_rec_list),
    ?sendself(#sc_dojangrank_world_replay_list{infoList = List}).

cs_dojangrank_world_replay_detail(#cs_dojangrank_world_replay_detail{replayUID = ReplayUID
                                                                    ,replay_type = 1 })->
    Dojangrank = role_data:get_dojangrank_data(),
    case lists:member(true, [lists:keymember(ReplayUID, #p_dr_dojang_replay_info.replayUID, L)||L<- Dojangrank#role_dojangrank.world_fight_rec_list]) of
        true ->
            case db_sql:get_dojangrank_world_fightrec(ReplayUID) of
                []->
                    ?sendself(#sc_dojangrank_world_replay_detail{result = 2
                                                          ,fightInfo = []});
                Rec ->
                    ?sendself(#sc_dojangrank_world_replay_detail{result = 1
                                                          ,fightInfo = [Rec]})
            end;    
        false ->
            ?sendself(#sc_dojangrank_world_replay_detail{result = 2
                                                  ,fightInfo = []})
    end;
cs_dojangrank_world_replay_detail(#cs_dojangrank_world_replay_detail{replayUID = ReplayUID
                                                                    ,replay_type = 2 })->
    #role{roleID =RoleID} = role_data:get_roleInfo(),
    dojangrank_server:get_dojangrank_world_top_replay_detail(RoleID,ReplayUID).

cs_dojangrank_world_ger_view_other(#cs_dojangrank_world_ger_view_other{tarRoleID = TarRoleID
                                                                      ,rank_index = RankType
                                                                      ,rank_data_type = 0})->
    DojangrankRank = dojangrank_server:get_world_dojangrank(TarRoleID,RankType),
    ?sendself(#sc_dojangrank_world_ger_view_other{role_info = DojangrankRank
                                                 ,ger_info_list = DojangrankRank#p_dr_world_rank.ger_info_list});
cs_dojangrank_world_ger_view_other(#cs_dojangrank_world_ger_view_other{tarRoleID = TarRoleID
                                                                      ,rank_index = RankType
                                                                      ,rank_data_type = 2})->
    #role{roleID =RoleID} = role_data:get_roleInfo(),
    dojangrank_server:get_history_other_detail(RoleID,RankType,TarRoleID).

cs_dojangrank_world_top_replay_list(#cs_dojangrank_world_top_replay_list{index = RankType})->
    #role{roleID =RoleID} = role_data:get_roleInfo(),
    dojangrank_server:get_dojangrank_world_top_replay_list(RoleID,RankType).

%% ====================================================================
%% Internal functions
%% ====================================================================

is_attend(RankType,RoleID)->
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    case ets:match_object(EtsName, #p_dr_world_rank{roleID=RoleID, _='_'}) of
        [] -> false;
        _ -> true
    end.

%% check_property(_,_)->       %% TODO TEMP
%%     true;
check_property(RankType,_) when 0 >= RankType andalso RankType > 8 ->
    false;
check_property(RankType,[])->
    true;
check_property(RankType,[H|T])->
    GerConfig = data_ger:get(H#ger.gerBase#gerBase.gerTypeID),
    if
        GerConfig#data_ger.gerProperty /= RankType ->
            false;
        true ->
            check_property(RankType,T)
    end.

check_time(Dojangrank)->
    if
        Dojangrank#role_dojangrank.free_time > 0 ->
            {ok,Dojangrank#role_dojangrank{free_time = Dojangrank#role_dojangrank.free_time - 1},1};
        Dojangrank#role_dojangrank.paid_time > 0 ->
            {ok,Dojangrank#role_dojangrank{paid_time = Dojangrank#role_dojangrank.paid_time - 1},2};
        true ->
            false
    end.

check_world_time(RankType,Dojangrank)->
    WorldFreeTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_free_time),
    WorldPaidTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_paid_time),
    if
        WorldFreeTime > 0 ->
            NewWorldFreeTimeList = util:nth_replace(RankType, Dojangrank#role_dojangrank.world_free_time, WorldFreeTime-1),
            {ok,Dojangrank#role_dojangrank{world_free_time = NewWorldFreeTimeList},1};
        WorldPaidTime > 0 ->
            NewWorldPaidTimeList = util:nth_replace(RankType, Dojangrank#role_dojangrank.world_paid_time, WorldPaidTime-1),
            {ok,Dojangrank#role_dojangrank{world_paid_time = NewWorldPaidTimeList},2};
        true ->
            false
    end.

check_fight(Dojangrank,RankType,FighterList,EnemyRoleId,EnemyRank)->
    EtsName = list_to_atom(lists:concat(['ets_dojangrank_',RankType])),
    DefRankInfo = case ets:match_object(EtsName, #dojangrank{role_id=EnemyRoleId, _='_'}) of
                      [] ->
                          ?undefined;
                      [Info] ->
                          Info
                  end,
    if
        ?undefined =:= DefRankInfo ->
            {false,3}; %%  
        EnemyRank /= DefRankInfo#dojangrank.rank ->
            {false,3}; %%  
        true ->
            case check_time(Dojangrank) of
                {ok,NewDojangrank,PayType} ->
                    case check_property(RankType,FighterList) of
                        true ->
                            {true,NewDojangrank,PayType};
                        false ->
                            {false,2}
                    end;
                false ->
                    {false,5}
            end
    end.

check_world_fight(RoleID,Dojangrank,RankType,FighterList,EnemyRoleId,EnemyRank)->
    case get_open_state() of
        2 ->
            EtsName = dojangrank_world_server:get_ets_name(RankType),
            DefRankInfo = case ets:lookup(EtsName, EnemyRank) of
                              [] ->
                                  ?undefined;
                              [Info] ->
                                  Info
                          end,
            IsAttend = [] /= ets:match_object(EtsName, #p_dr_world_rank{roleID=RoleID, _='_'}),
            IsEnemy = lists:member(EnemyRank, lists:nth(RankType, Dojangrank#role_dojangrank.world_enemy_list)),
            if
                false =:= IsEnemy ->
                    ?ERR("check_world_fight fail 对手数据不在刷新数据中"),
                    {false,3}; %%  
                ?undefined =:= DefRankInfo ->
                    ?ERR("check_world_fight fail 对手数据不存在"),
                    {false,3}; %%  
                IsAttend =:= false ->
                    ?ERR("check_world_fight fail 没有参赛资格"),
                    {false,3}; %%  没有参赛资格
                EnemyRoleId /= DefRankInfo#p_dr_world_rank.roleID ->
                    ?ERR("check_world_fight fail 对手状态变化  ~w  ------>>>>> ~w",[{EnemyRoleId,EnemyRank},DefRankInfo]),
                    {false,3}; %%  
                true ->
                    case check_world_time(RankType,Dojangrank) of
                        {ok,NewDojangrank,PayType} ->
                            case check_property(RankType,FighterList) of
                                true ->
                                    {true,NewDojangrank,PayType};
                                false ->
                                    {false,2}
                            end;
                        false ->
                            {false,5}
                    end
            end;
        _ ->
            {false,6}
    end.

%% 0数据同步中1休战期2开启
get_open_state()->
	{{_,_,DD},NowTime} = util:seconds_to_datetime(util:now()),
    {SessionDay,SessionTime} = data_dojangrank:get(session_swap_point),
    if
        DD == SessionDay andalso NowTime < SessionTime ->
            1;
        true ->
            IsInited = 
                lists:all(fun(RankType)->
                                EtsName = dojangrank_world_server:get_ets_name(RankType),
                                0 /= ets:info(EtsName,size)
                          end, lists:seq(1, 8)),
            DateTime = {date(),time()},
            StartTime = data_dojangrank:get(start_date),
            if
                DateTime >= StartTime andalso IsInited ->
                    2;
                true ->
                    0
            end
    end.

get_season_endtime()->
    {Year,Month,NowDay} = erlang:date(),
    {Hour,_,_} = erlang:time(),
    if
        NowDay =:= 1 andalso Hour < 8 ->
            util:datetime_to_seconds({{Year,Month,NowDay},{8,0,0}});
        Month < 12 ->
            util:datetime_to_seconds({{Year,Month+1,1},{0,0,0}});
        Month =:= 12 ->
            util:datetime_to_seconds({{Year+1,1,1},{0,0,0}})
    end.

get_enemy_info_list(RankType,Rank,Dojangrank,IsReset,IsClear) when erlang:is_integer(Rank)->
    EnemyRankList0 = lists:nth(RankType, Dojangrank#role_dojangrank.world_enemy_list),
    {EnemyRankList,NewDojangrank} = 
        if
            [] =:= EnemyRankList0 orelse IsReset =:= true ->
                ?INFO("get_enemy_info_list refresh data"),
                EnemyList0 = get_enemy_rank_list(Rank),
                NewWorldEnemyList = 
                    util:nth_replace(RankType,Dojangrank#role_dojangrank.world_enemy_list,EnemyList0),
                NewWorldRefreshTime = 
                    if
                        IsClear orelse [] =:= EnemyRankList0 ->
                            util:nth_replace(RankType, Dojangrank#role_dojangrank.world_refresh_time, 0);
                        true ->
                            WorldRefreshTime = lists:nth(RankType, Dojangrank#role_dojangrank.world_refresh_time),
                            util:nth_replace(RankType, Dojangrank#role_dojangrank.world_refresh_time, WorldRefreshTime + 1)
                    end,    
                NewDojangrank0 = Dojangrank#role_dojangrank{world_enemy_list = NewWorldEnemyList
                                                           ,world_refresh_time = NewWorldRefreshTime},
                role_data:set_dojangrank_data(NewDojangrank0),
                {EnemyList0,NewDojangrank0};
            true ->
                {EnemyRankList0,Dojangrank}
        end,
    EtsName = dojangrank_world_server:get_ets_name(RankType),
    EnemyList = lists:map(fun(RankIndex)-> 
                                  [F] = ets:lookup(EtsName,RankIndex),
                                  F
                          end, EnemyRankList),
    {EnemyList,NewDojangrank}.
    
%% get_enemy_info_list2(RankType,IdList) when erlang:is_list(IdList)->
%%     EtsName = dojangrank_world_server:get_ets_name(RankType),
%%     EnemyList = lists:map(fun(RankIndex)-> 
%%                                   [F] = ets:lookup(EtsName,RankIndex),
%%                                   F
%%                           end, get_enemy_rank_list(Rank)).

get_enemy_rank_list(0)->
    get_enemy_rank_list(data_dojangrank:get(init_robot_num_max));
get_enemy_rank_list(1)->
    [2,3,4];
get_enemy_rank_list(2)->
    [1,3,4];
get_enemy_rank_list(3)->
    [1,2,4];
get_enemy_rank_list(SelfRank) when SelfRank =< 10 ->
    [SelfRank-3,SelfRank-2,SelfRank-1];
get_enemy_rank_list(SelfRank)->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    if
        SelfRank > InitRobotNum ->
            get_enemy_rank_list(InitRobotNum);
        true ->
            P0 = SelfRank - 1,                              %% 11:10   100:99
            P1 = SelfRank - (SelfRank div 10),              %% 11:10   100:90
            P2 = SelfRank - ((SelfRank*2) div 10),          %% 11:9    100:80
            P3 = SelfRank - ((SelfRank*3) div 10),          %% 11:8    100:70
            [util:random_int(P3,P2-1)                
            ,util:random_int(P2,P1-1)
            ,util:random_int(P1,P0)]
    end.

check_rank_out(PDrWorldRankList) when erlang:is_list(PDrWorldRankList)->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    [check_rank_out(E,InitRobotNum)||E<-PDrWorldRankList];
check_rank_out(PDrWorldRank) when erlang:is_record(PDrWorldRank, p_dr_world_rank)->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    check_rank_out(PDrWorldRank,InitRobotNum).

check_rank_out(PDrWorldRank,InitRobotNum)->
    if
        PDrWorldRank#p_dr_world_rank.rank > InitRobotNum ->
            PDrWorldRank#p_dr_world_rank{rank = 0};
        true ->
            PDrWorldRank
    end.












