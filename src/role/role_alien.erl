-module(role_alien).

-compile(export_all).

-include("def_role.hrl").

cs_alien_info(_) ->
    case role_data:get_roleAlienInfo() of
        ?undefined ->
            #role{level=RoleLevel, vipLevel=VipLevel} = role_data:get_roleInfo(),
            NeedRoleLevel = data_alien:get(need_role_level),
            NeedVipLevel = data_alien:get(need_vip_level),
            case RoleLevel >= NeedRoleLevel andalso VipLevel >= NeedVipLevel of
                true ->
                    AlienInfo = #alien_info{times=data_common:get(max_alien_times), lastRecoverTime=util:now()},
                    role_data:set_roleAlienInfo(AlienInfo),
                    erlang:send(alien_server, {get_alien_info, role_data:get_roleID(), data_common:get(max_alien_times), 0});
                false ->
                    erlang:send(alien_server, {get_alien_info, role_data:get_roleID(), 0, 0})
            end;
        #alien_info{times=AlienTimes,resetTime=ResetTime} ->
            erlang:send(alien_server, {get_alien_info, role_data:get_roleID(), AlienTimes, ResetTime})
    end.

cs_alien_sign(_) ->
    #role{level=RoleLevel, vipLevel=VipLevel,svipLevel=SVipLevel} = RoleInfo = role_data:get_roleInfo(),
    FighterList = role_data:get_posList(),
    ItemList = role_data:get_equipts_on_ger(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    RoleLieuAddList = role_data:get_lieuInfoList(),
    NeedRoleLevel = data_alien:get(need_role_level),
    NeedVipLevel = data_alien:get(need_vip_level),
	TrSpecial= role_data:get_trSpecial(),
    SkinInfo = role_skin:get_skin_info(),
    Vip = role_lib:cacl_vip_info(VipLevel,SVipLevel),

    case RoleLevel >= NeedRoleLevel of
        true ->
            case VipLevel >= NeedVipLevel of
                true ->
                    TalentList = role_talent:get_active_talent_list(),
                    erlang:send(alien_server, {alien_sign, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip});
                false ->
                    ?sendself(#sc_alien_sign{result=2})
            end;
        false ->
            ?sendself(#sc_alien_sign{result=1})
    end.

cs_alien_fight(#cs_alien_fight{tarRoleID=TarRoleID, tarRank=TarTank}) ->
    case catch check_can_alien_fight(TarRoleID) of
        {ok, RoleID, NeedTimes, NeedGold} ->
            erlang:send(alien_server, {alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold});
        {false, Reason} ->
            ?sendself(#sc_alien_fight{result=Reason,fightInfo=[],newRank=0,addCoin=0,fighterList=[]})
    end.

check_can_alien_fight(TarRoleID) ->
    #role{roleID=RoleID,level=RoleLevel,vipLevel=VipLevel,gold=Gold,goldBonus=GoldBonus} = role_data:get_roleInfo(),
    NeedRoleLevel = data_alien:get(need_role_level),
    NeedVipLevel = data_alien:get(need_vip_level),
    case TarRoleID of
        RoleID ->
            erlang:throw({false, 6});
        _ ->
            next
    end,
    case RoleLevel >= NeedRoleLevel of
        true ->
            next;
        false ->
            erlang:throw({false, 9})
    end,
    case VipLevel >= NeedVipLevel of
        true ->
            next;
        false ->
            erlang:throw({false, 10})
    end,
    case role_data:get_roleAlienInfo() of
        ?undefined ->
            role_data:set_roleAlienInfo(#alien_info{times=data_common:get(max_alien_times), lastRecoverTime=util:now()}),
            Times = data_common:get(max_alien_times);
        #alien_info{times=Times} ->
            next
    end,
    case Times >= 1 of
        true ->
            {ok, RoleID, 1, 0};
        false ->
            NeedGold = data_alien:get(fight_need_gold),
            case Gold + GoldBonus >= NeedGold of
                true ->
                    {ok, RoleID, 0, NeedGold};
                false ->
                    {false, 7}
            end
    end.

cs_alien_reset(_) ->
    #role{gold=Gold,goldBonus=GoldBonus,vipLevel=VipLevel,svipLevel=SVipLevel} = RoleInfo = role_data:get_roleInfo(),
    FighterList = role_data:get_posList(),
    ItemList = role_data:get_equipts_on_ger(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TrSpecial= role_data:get_trSpecial(),
    RoleLieuAddList = role_data:get_lieuInfoList(),
    SkinInfo = role_skin:get_skin_info(),   
    NeedGold = data_alien:get(reset_gold),
    Vip = role_lib:cacl_vip_info(VipLevel,SVipLevel),
    case Gold + GoldBonus >= NeedGold of
        true ->
            case role_data:get_roleAlienInfo() of
                #alien_info{resetTime=ResetTime} ->
                    case ResetTime =< util:now() of
                        true ->
                            TalentList = role_talent:get_active_talent_list(),
                            erlang:send(alien_server, {alien_reset, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip});
                        false ->
                            ?sendself(#sc_alien_reset{result=4,timestamp=ResetTime})
                    end
            end;
        false ->
            ?sendself(#sc_alien_reset{result=1,timestamp=0})
    end.

cs_alien_guess(#cs_alien_guess{guessCoin=GuessCoin,guessType=GuessType}) ->
    case GuessType of
        false ->
            CoinValList = data_alien:get(coin_val_list_odd);
        true ->
            CoinValList = data_alien:get(coin_val_list_even)
    end,
    #role{roleID=RoleID,coin=RoleCoin} = RoleInfo = role_data:get_roleInfo(),
    case lists:member(GuessCoin, CoinValList) of
        true ->
            case RoleCoin >= GuessCoin of
                true ->
                    erlang:send(alien_server, {alien_guess, RoleID, RoleInfo, GuessCoin, GuessType});
                false ->
                    ?sendself(#sc_alien_guess{result=2})
            end;
        false ->
            ?sendself(#sc_alien_guess{result=1})
    end.

cs_alien_buy_times(#cs_alien_buy_times{buyTimes=BuyTimes}) when BuyTimes > 0 ->
    case role_data:get_roleAlienInfo() of
        #alien_info{times=255} ->
            ?sendself(#sc_alien_buy_times{result=3,newTimes=0});
        #alien_info{times=Times} = AlienInfo ->
            NewBuyTimes =
                case BuyTimes > 255 - Times of
                    true ->
                        255 - Times;
                    false ->
                        BuyTimes
                end,
            Price = data_alien:get(price),
            NeedGold = Price * NewBuyTimes,
            #role{gold=Gold,goldBonus=GoldBonus} = RoleInfo = role_data:get_roleInfo(),
            case Gold + GoldBonus >= NeedGold of
                true ->
                    role_lib:deduct_money_f(RoleInfo, gold, NeedGold, ?MONEY_DEC_TYPE_ALIEN_BUY_TIMES, 0, ""),
                    role_data:set_roleAlienInfo(AlienInfo#alien_info{times=Times+NewBuyTimes}),
                    ?sendself(#sc_alien_buy_times{result=0,newTimes=Times+NewBuyTimes});
                false ->
                    ?sendself(#sc_alien_buy_times{result=1,newTimes=0})
            end;
        ?undefined ->
            ?sendself(#sc_alien_buy_times{result=2,newTimes=0})
    end.

cs_alien_finals_guess(#cs_alien_finals_guess{guessID=GuessID, rank=Rank}) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    case check_can_guess(RoleInfo, Rank) of
        {false, Reason} ->
            ?sendself(#sc_alien_finals_guess{result=Reason});
        _ ->
            erlang:send(alien_server, {client_msg, RoleID, #cs_alien_finals_guess{guessID=GuessID, rank=Rank}})
    end.

cs_alien_self_rank(_) ->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    erlang:send(alien_server, {get_self_alien_rank, RoleID}).

check_can_guess(Role, Rank) ->
    case data_alien_finals:get(match_stakes) of
        ?undefined ->
            {false, 4};
        List ->
            Length = erlang:length(List),
            case Rank =< 0 orelse Rank > Length of
                true ->
                    {false, 5};
                _ ->
                    {Rank, Value} = lists:nth(Rank, List),
                    case role_lib:check_money(Role, reputation, Value) of
                        false ->
                            {false, 1};
                        true ->
                            NewRoleInfo = role_lib:deduct_money_f(Role, reputation, Value, ?MONEY_DEC_TYEP_ALIEN_FINALS_GUESS_COST, Rank, ""),
                            role_data:set_roleInfo(NewRoleInfo),
                            true
                    end
            end
    end.

%% 异星总决赛竞猜错误的处理
finals_guess_failed(Result, Rank) ->
    StakesList = data_alien_finals:get(match_stakes),
    {Rank, Value} = lists:nth(Rank, StakesList),
    RoleInfo = role_data:get_roleInfo(),
    NewRoleInfo = role_lib:add_reputation_f(RoleInfo, Value, ?MONEY_ADD_TYPE_FINALS_GUESS_FAIL, Rank, "", false),
    role_data:set_roleInfo(NewRoleInfo),
    ?sendself(#sc_alien_finals_guess{result=Result}).

cs_alien_finals_stake_list(#cs_alien_finals_stake_list{}) ->
    StakesList = data_alien_finals:get(match_stakes),
    ?sendself(#sc_alien_finals_stake_list{list=lists:foldr(fun({_,E},Acc) -> [E|Acc] end, [], StakesList)}).

