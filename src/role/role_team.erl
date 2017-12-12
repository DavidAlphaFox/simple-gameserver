-module(role_team).

-compile(export_all).

-include("def_role.hrl").
-include("def_team_pk.hrl").

-define(REFRESH_SELF, 1).
-define(REFRESH_OTHER, 2).

-define(self3v3ReplayRecord, self3v3ReplayRecord).

cs_team_pk_info(_) ->
    #role{level=RoleLevel,roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    Session = get_team_pk_session(),
    case role_data:get_roleTeamPkInfo() of
        ?undefined ->
            case RoleLevel >= data_team_pk:get(open_level) of
                true ->
                    {SelfList, OtherList} = gen_team_data(RoleInfo),
                    NewTeamPkInfo = #team_pk_info{session=Session,refreshSelf=1,refreshOther=1,selfTeam=SelfList,otherTeam=OtherList},
                    role_data:set_roleTeamPkInfo(NewTeamPkInfo),
                    cs_team_pk_info2(RoleID, RoleInfo, NewTeamPkInfo);
                false ->
                    ?sendself(#sc_team_pk_not_open{needLevel=data_team_pk:get(open_level)})
            end;
        #team_pk_info{session=OldSession} = OldTeamPkInfo ->
            case OldSession of
                Session ->
                    TeamPkInfo = OldTeamPkInfo;
                _ ->
                    erase_replay_record(),
                    TeamPkInfo = #team_pk_info{session=Session,refreshSelf=1,refreshOther=1,selfTeam=[],otherTeam=[],selfRecordList=[]}
            end,
            #team_pk_info{selfTeam=SelfList,otherTeam=OtherList} = TeamPkInfo,
            case SelfList =:= [] orelse OtherList =:= [] of
                true ->
                    {NewSelfList, NewOtherList} = gen_team_data(RoleInfo),
                    NewTeamPkInfo = TeamPkInfo#team_pk_info{selfTeam=NewSelfList,otherTeam=NewOtherList},
                    role_data:set_roleTeamPkInfo(NewTeamPkInfo),
                    cs_team_pk_info2(RoleID, RoleInfo, NewTeamPkInfo);
                false ->
                    {ok, NewTeamPkInfo} = update_role_team_pk_info(TeamPkInfo, RoleInfo),
                    cs_team_pk_info2(RoleID, RoleInfo, NewTeamPkInfo)
            end
    end.

update_role_team_pk_info(TeamPkInfo, RoleInfo) ->
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName,isMale=IsMale,title=Title,head=Head,fightPower=FightPower} = RoleInfo,
    #team_pk_info{selfTeam=SelfTeam} = TeamPkInfo,
    TeamRole = lists:keyfind(RoleID, #team_member.roleID, SelfTeam),
    NewTeamRole = TeamRole#team_member{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName},
    NewSelfTeam = lists:keyreplace(RoleID, #team_member.roleID, SelfTeam, NewTeamRole),
    NewTeamPkInfo = TeamPkInfo#team_pk_info{selfTeam=NewSelfTeam},
    role_data:set_roleTeamPkInfo(NewTeamPkInfo),
    {ok, NewTeamPkInfo}.

cs_team_pk_info2(RoleID, #role{fightPower=FightPower}, TeamPkInfo) ->
    #team_pk_info{refreshSelf=RefreshSelf,refreshOther=RefreshOther,selfTeam=SelfTeam,otherTeam=OtherTeam} = TeamPkInfo,
    case catch gen_server:call(team_pk_server, {cs_team_pk_info, RoleID}) of
        {ok, NextCloseTime, Rank, Score} ->
            ?sendself(#sc_team_pk_open{fightPower=FightPower,rank=Rank,score=Score,refreshSelf=RefreshSelf,refreshOther=RefreshOther,
                                       selfTeam=transTeamList(SelfTeam),otherTeam=transTeamList(OtherTeam),
                                       closeTimestamp=NextCloseTime});
        {ok, NextTimestamp, RankList, Rank, Score} ->
            ?sendself(#sc_team_pk_close{fightPower=FightPower,score=Score,rank=Rank,nextTimestamp=NextTimestamp,rankList=transRankList(RankList)});
        Err ->
            ?ERR("Err:~w", [Err]),
            ?sendself(#sc_team_pk_close{fightPower=FightPower,score=0,rank=0,nextTimestamp=0,rankList=[]})
    end.

transTeamList(TeamList) ->
    lists:map(fun(#team_member{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName,vip=Vip}) ->
                      #p_team_member{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName,vip=Vip}
              end, TeamList).

transRankList(RankList) ->
    lists:map(fun(#team_pk_rank{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName,vip=Vip}) ->
                      #p_team_member{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName,vip=Vip}
              end, RankList).

gen_team_data(RoleInfo) ->
    erlang:send(team_pk_server, {gen_team_data, RoleInfo#role.roleID}),
    receive
        {ok, {[FID1,FID2], [EID1, EID2, EID3]}} ->
            TeamRole = gen_team_data2(RoleInfo),
            TeamF1 = gen_team_data2(FID1),
            TeamF2 = gen_team_data2(FID2),
            TeamE1 = gen_team_data2(EID1),
            TeamE2 = gen_team_data2(EID2),
            TeamE3 = gen_team_data2(EID3),
            {[TeamRole,TeamF1,TeamF2], [TeamE1,TeamE2,TeamE3]};
        {ok,{[],[]}} ->
            {[], []}
    after
            5000 ->
            {[], []}
    end.

gen_team_data2(RoleInfo) when erlang:is_record(RoleInfo, role) ->
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName,isMale=IsMale,title=Title
          ,vipLevel=VipLevel,svipLevel=SVipLevel,head=Head,fightPower=FightPower} = RoleInfo,
    CurRoleID = role_data:get_roleID(),
    {ItemList,SkinInfo,{FighterList,LieuAddAttr, TalentList,TrSpecial}} = 
        case RoleID =/= CurRoleID of
            true->  %% 其实并不太可能走这个分支
                {role_data:get_otherRoleItemEquips(RoleID)
                ,role_data:get_otherRoleSkinInfo(RoleID)
                ,role_data:get_otherRoleFighter(RoleID)};
            false->
                {GerEquipList, _BagEquip, _BagItem} = role_data:get_all_item(),
                ItemList0 = [ [ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,item_lib:itemDecay(ItemDecay),ItemExp,ItemEnchantType,ItemEnchantLevel]
                        || {ItemGerID, ItemList} <- GerEquipList,
                        #item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemDecay=ItemDecay,itemLevel=ItemLevel,itemPos=ItemPos,itemRank=ItemRank,itemExp = ItemExp,itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel} <- ItemList],
                FighterList0 = role_data:get_posList(),
                LieuAddAttr1 = role_data:get_lieu_add_attr(),
                TalentList0 = role_talent:get_active_talent_list(),
                TrSpecial0 = role_data:get_trSpecial(),
                SkinInfo0 = role_skin:get_skin_info(),
                {ItemList0,SkinInfo0,{FighterList0, LieuAddAttr1, TalentList0,TrSpecial0}}
        end,
    ?INFO("gen_team_data2 r:~w",[RoleID]),
    #team_member{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName
                ,fighterData={FighterList, LieuAddAttr,TalentList,TrSpecial,SkinInfo},itemList=ItemList,vip=role_lib:cacl_vip_info(VipLevel,SVipLevel)};

gen_team_data2(RoleID) when erlang:is_integer(RoleID) ->
    {FighterList,LieuAdd, TalentList,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
    SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
    ItemList = role_data:get_otherRoleItemEquips(RoleID),
    #role{roleID=RoleID,level=RoleLevel,roleName=RoleName,isMale=IsMale,title=Title,head=Head
          ,vipLevel=VipLevel,svipLevel=SVipLevel,fightPower=FightPower} = role_data:get_otherRoleInfo(RoleID),
    #team_member{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,
                 roleName=RoleName,fighterData={FighterList, LieuAdd,TalentList,TrSpecial,SkinInfo}
                 ,vip=role_lib:cacl_vip_info(VipLevel,SVipLevel),itemList=ItemList}.

get_team_pk_status() ->
    case catch gen_server:call(team_pk_server, get_status) of
        {ok, Status} ->
            Status;
        Err ->
            ?ERR("Err:~w", [Err]),
            0
    end.

get_team_pk_session() ->
    case catch gen_server:call(team_pk_server, get_session) of
        {ok, Session} ->
            Session;
        Err ->
            ?ERR("Err:~w", [Err]),
            0
    end.


cs_team_refresh(#cs_team_refresh{type=Type}) ->
    RoleInfo = role_data:get_roleInfo(),
    TeamPkInfo = role_data:get_roleTeamPkInfo(),
    case Type of
        ?REFRESH_SELF ->
            case check_refresh_self(RoleInfo, TeamPkInfo) of
                {ok, NeedGold, NewRefreshSelf} ->
                    do_refresh_self(RoleInfo, TeamPkInfo, NewRefreshSelf, NeedGold);
                {false, Reason} ->
                    ?sendself(#sc_team_refresh{type=Type,result=Reason,list=[]})
            end;
        ?REFRESH_OTHER ->
            case check_refresh_other(RoleInfo, TeamPkInfo) of
                {ok, NeedGold, NewRefreshOther} ->
                    do_refresh_other(RoleInfo, TeamPkInfo, NewRefreshOther, NeedGold);
                {false, Reason} ->
                    ?sendself(#sc_team_refresh{type=Type,result=Reason,list=[]})
            end
    end.

check_refresh_self(RoleInfo, TeamPkInfo) ->
    Status = get_team_pk_status(),
    ?DEBUG("Status:~w", [Status]),
    case Status of
        1 ->
            case TeamPkInfo of
                ?undefined ->
                    {false, 3};
                #team_pk_info{refreshSelf=RefreshSelf} ->
                    case RefreshSelf >= 1 of
                        true ->
                            {ok, 0, RefreshSelf - 1};
                        false ->
                            NeedGold = data_team_pk:get(refresh_self_gold),
                            #role{gold=Gold,goldBonus=GoldBonus} = RoleInfo,
                            case Gold + GoldBonus >= NeedGold of
                                true ->
                                    {ok, NeedGold, 0};
                                false ->
                                    {false, 1}
                            end
                    end
            end;
        0 ->
            {false, 2}    
    end.

do_refresh_self(RoleInfo, TeamPkInfo, NewRefreshSelf, NeedGold) ->
    role_lib:deduct_money_f(RoleInfo, gold, NeedGold, ?MONEY_DEC_TYPE_TEAM_PK_REFRESH, ?REFRESH_SELF, ""),
    {NewSelfList, _NewOtherList} = gen_team_data(RoleInfo),
    role_data:set_roleTeamPkInfo(TeamPkInfo#team_pk_info{refreshSelf=NewRefreshSelf,selfTeam=NewSelfList}),
    ?sendself(#sc_team_refresh{type=?REFRESH_SELF,result=0,list=transTeamList(NewSelfList)}).

check_refresh_other(RoleInfo, TeamPkInfo) ->
    Status = get_team_pk_status(),
    case Status of
        1 ->
            case TeamPkInfo of
                ?undefined ->
                    {false, 3};
                #team_pk_info{refreshOther=RefreshOther} ->
                    case RefreshOther >= 1 of
                        true ->
                            {ok, 0, RefreshOther - 1};
                        false ->
                            NeedGold = data_team_pk:get(refresh_other_gold),
                            #role{gold=Gold,goldBonus=GoldBonus} = RoleInfo,
                            case Gold + GoldBonus >= NeedGold of
                                true ->
                                    {ok, NeedGold, 0};
                                false ->
                                    {false, 1}
                            end
                    end
            end;
        0 ->
            {false, 2}    
    end.

do_refresh_other(RoleInfo, TeamPkInfo, NewRefreshOther, NeedGold) ->
    role_lib:deduct_money_f(RoleInfo, gold, NeedGold, ?MONEY_DEC_TYPE_TEAM_PK_REFRESH, ?REFRESH_OTHER, ""),
    {_NewSelfList, NewOtherList} = gen_team_data(RoleInfo),
    role_data:set_roleTeamPkInfo(TeamPkInfo#team_pk_info{refreshOther=NewRefreshOther,otherTeam=NewOtherList}),
    ?sendself(#sc_team_refresh{type=?REFRESH_OTHER,result=0,list=transTeamList(NewOtherList)}).

cs_team_move(#cs_team_move{fromPos=FromPos,toPos=ToPos}) ->
    case catch check_team_move(FromPos, ToPos) of
        {ok, NewTeamPkInfo} ->
            role_data:set_roleTeamPkInfo(NewTeamPkInfo),
            ?sendself(#sc_team_move{result=0});
        {false, Reason} ->
            ?sendself(#sc_team_move{result=Reason})
    end.

check_team_move(FromPos, ToPos) ->
    case FromPos =:= ToPos of
        true ->
            erlang:throw({false, 1});
        false ->
            next
    end,
    case lists:member(FromPos, [1,2,3]) andalso lists:member(ToPos, [1,2,3]) of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    TeamPkInfo = role_data:get_roleTeamPkInfo(),
    case TeamPkInfo of
        ?undefined ->
            SelfTeam = [],
            erlang:throw({false, 2});
        #team_pk_info{selfTeam=SelfTeam} ->
            case erlang:is_list(SelfTeam) andalso erlang:length(SelfTeam) =:= 3 of
                true ->
                    next;
                false ->
                    erlang:throw({false, 2})
            end
    end,
    {ok, NewSelfTeam} = do_move(SelfTeam, FromPos, ToPos),
    {ok, TeamPkInfo#team_pk_info{selfTeam=NewSelfTeam}}.

do_move(SelfTeam, FromPos, ToPos) ->
    SrcMember = lists:nth(FromPos, SelfTeam),
    DestMember = lists:nth(ToPos, SelfTeam),
    NewSelfTeam =
        lists:foldr(
          fun(Member, Acc) ->
                  case Member of
                      SrcMember ->
                          [DestMember|Acc];
                      DestMember ->
                          [SrcMember|Acc];
                      _ ->
                          [Member|Acc]
                  end
          end, [], SelfTeam),
    {ok, NewSelfTeam}.


cs_team_fight(#cs_team_fight{}) ->
    case catch check_team_fight() of
        {ok, TeamPkInfo, RoleTimes, RoleInfo} ->
            do_team_fight(TeamPkInfo, RoleTimes, RoleInfo);
        {false, Reason} ->
            ?sendself(#sc_team_fight_error{result=Reason})
    end.

do_team_fight(TeamPkInfo, RoleTimes, #role{roleID=RoleID,roleName=RoleName,level=RoleLevel}=RoleInfo) ->
    role_lib:deduct_teamPkTimes_f(RoleTimes),
    #team_pk_info{refreshSelf=RefreshSelf,refreshOther=RefreshOther,
                  selfTeam=SelfTeam,otherTeam=OtherTeam,selfRecordList=SelfRecordList} = TeamPkInfo,
    {ok, NewSelfTeam, NewOtherTeam, IsWin, SelfDeadCount, OtherDeadCount, ReplayUIDList, FightRecordList} =
        do_team_fight_loop(SelfTeam, OtherTeam, 1, 1, true, 0, 0, [], []),
    AddCoin = get_add_coin(RoleLevel, OtherDeadCount),
    %%计算公会科技对3v3金币产出影响
    #role{familyID = FamilyID} = role_data:get_roleInfo(),
    FamilyTek_3v3Coinout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,8,1),
    % ?ERR("FamilyTek_3v3Coinout_Add ~w~n",[FamilyTek_3v3Coinout_Add]),
    % ?ERR("oldCOin ~w ~n ",[AddCoin]),
    AddCoin2 = role_lib:calculate_familyTekeffectGenerate(AddCoin,FamilyTek_3v3Coinout_Add),
    % ?ERR("FamilyTek_3v3Coinout_Add AddCoin ~w ~n",[AddCoin2]),
    AddExp = get_add_exp(RoleLevel, OtherDeadCount),
    POtherTeam = transTeamMember2(NewOtherTeam),
    PSelfTeam = transTeamMember2(NewSelfTeam),
    role_reward:handle_sell_reward_f(RoleInfo, #sell_reward{coin=AddCoin2,roleExp=AddExp}, ?MONEY_ADD_TYPE_3V3, 0, ""),
    NewSelfRecord =
        #team_self_record{timestamp=util:now(),isWin=IsWin,addExp=AddExp,addCoin=AddCoin2,addScore=OtherDeadCount,
                          selfNameList=lists:map(fun(#team_member{roleName=Name}) -> Name end, SelfTeam),
                          otherNameList=lists:map(fun(#team_member{roleName=Name}) -> Name end, OtherTeam),
                          replayUIDList=ReplayUIDList,
                          selfList=PSelfTeam,otherList=POtherTeam},
    NewSelfRecordList =
        case erlang:length(SelfRecordList) =:= 10 of
            true ->
                DelSelfRecord = lists:nth(10, SelfRecordList),
                [NewSelfRecord|lists:delete(DelSelfRecord, SelfRecordList)];
            false ->
                DelSelfRecord = ?undefined,
                [NewSelfRecord|SelfRecordList]
        end,
    {GenSelfList, GenOtherList} = gen_team_data(RoleInfo),
    NewRefreshSelf = erlang:min(RefreshSelf+1, 1),
    NewRefreshOther = erlang:min(RefreshOther+1, 1),
    role_data:set_roleTeamPkInfo(
      TeamPkInfo#team_pk_info{selfRecordList=NewSelfRecordList,
                              refreshSelf=NewRefreshSelf,
                              refreshOther=NewRefreshOther,
                              selfTeam=GenSelfList,
                              otherTeam=GenOtherList}),
    {ok, GodName} = get_god_name(NewSelfTeam,NewOtherTeam,SelfDeadCount,OtherDeadCount),
    {ok, OldRank, NewRank} = update_rank_and_record(RoleName,GodName,IsWin,SelfDeadCount,OtherDeadCount,
                                                    ReplayUIDList,DelSelfRecord,PSelfTeam,POtherTeam),
    
    role_task:send_dispach(RoleID, {dispach_task,role_finished_3v3,1}),

    FightResult =
        #sc_team_fight_result{
                              isWin=IsWin
                              ,addCoin=AddCoin2
                              ,addExp=AddExp
                              ,addScore=OtherDeadCount
                              ,oldRank=OldRank
                              ,newRank=NewRank
                              ,refreshSelf=NewRefreshSelf
                              ,refreshOther=NewRefreshOther
                              ,otherList=POtherTeam
                              ,selfList=PSelfTeam
                              ,fightInfoList=FightRecordList
                              ,otherTeam=transTeamList(GenOtherList)
                              ,selfTeam=transTeamList(GenSelfList)
                             },
    role_box:do_mystery_box(team_pk),
    ?sendself(FightResult).

get_god_name(NewSelfTeam,NewOtherTeam,SelfDeadCount,OtherDeadCount) ->
    if
        SelfDeadCount =:= 0 ->
            [#team_member{roleName=GodName}|_] = NewSelfTeam;
        OtherDeadCount =:= 0 ->
            [#team_member{roleName=GodName}|_] = NewOtherTeam;
        true ->
            GodName = <<"">>
    end,
    {ok, GodName}.

update_rank_and_record(RoleName,GodName,IsWin,SelfDeadCount,OtherDeadCount,ReplayUIDList,DelSelfRecord,PSelfTeam,POtherTeam) ->
    Now = util:now(),
    if
        SelfDeadCount =:= 0 ->
            NewTeamRecord = #team_record{isWin=IsWin,timestamp=Now,roleName=RoleName,godName=GodName,
                                         replayUIDList=ReplayUIDList,selfList=PSelfTeam ,otherList=POtherTeam};
        OtherDeadCount =:= 0 ->
            NewTeamRecord = #team_record{isWin=IsWin,timestamp=Now,roleName=RoleName,godName=GodName,
                                         replayUIDList=ReplayUIDList,selfList=PSelfTeam ,otherList=POtherTeam};
        true ->
            NewTeamRecord = ?undefined
    end,
    case DelSelfRecord of
        ?undefined ->
            DelReplayUIDList = [];
        #team_self_record{replayUIDList=DelReplayUIDList} ->
            next
    end,
    case catch gen_server:call(team_pk_server, {update_rank_and_record, role_data:get_roleInfo(), OtherDeadCount, NewTeamRecord, DelReplayUIDList}) of
        {ok, OldRank, NewRank} ->
            next;
        Err ->
            ?ERR("Err:~w", [Err]),
            OldRank = 0,
            NewRank = 0
    end,
    {ok, OldRank, NewRank}.

transTeamMember2(List) ->
    lists:map(fun(#team_member{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName,fighterData={FighterList,_,_,_,_},vip=Vip}) ->
                      IsDead = check_is_dead(FighterList),
                      #p_team_member2{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=RoleLevel,roleName=RoleName,isDead=IsDead,vip=Vip}
              end, List).

check_is_dead(FighterList) ->
    List = 
        lists:filter(fun(#ger{gerHp=GerHP}) ->
                             GerHP > 0
                     end, FighterList),
    erlang:length(List) =:= 0.

get_add_coin(RoleLevel, OtherDeadCount) ->
    BaseCoin = data_team_pk:get({coin, RoleLevel}),
    Mul = data_team_pk:get({coin_mul, OtherDeadCount}),
    erlang:trunc(BaseCoin * Mul).

get_add_exp(RoleLevel, OtherDeadCount) ->
    ExpBase = data_team_pk:get({exp, RoleLevel}),
    Mul = data_team_pk:get({exp_mul, OtherDeadCount}),
    erlang:trunc(ExpBase * Mul).

do_team_fight_loop(SelfTeam, OtherTeam, SelfCount, OtherCount, IsAtk, SelfDeadCount, OtherDeadCount, ReplayUIDList,FightRecordList) ->
    SelfN = lists:nth(SelfCount, SelfTeam),
    OtherN = lists:nth(OtherCount, OtherTeam),
    case IsAtk of
        true ->
            {ok, NewSelfN, NewOtherN, IsWin, ReplayUID, FightRecord, IsBothDead} = do_fight(SelfN, OtherN, IsAtk),
            IsSelfWin = IsWin;
        false ->
            {ok, NewSelfN, NewOtherN, IsWin, ReplayUID, FightRecord, IsBothDead} = do_fight(SelfN, OtherN, IsAtk),
            IsSelfWin = not IsWin
    end,
    NewReplayUIDList = [ReplayUID|ReplayUIDList],
    NewFightRecordList = [FightRecord|FightRecordList],
    NewSelfTeam = lists:keyreplace(SelfN#team_member.roleID, #team_member.roleID, SelfTeam, NewSelfN),
    NewOtherTeam = lists:keyreplace(OtherN#team_member.roleID, #team_member.roleID, OtherTeam, NewOtherN),
    %% 两边都死光了,则都换到下一组去
    case IsBothDead of
        true ->
            NewSelfCount = SelfCount + 1,
            NewOtherCount = OtherCount + 1,
            NewSelfDeadCount = SelfDeadCount + 1,
            NewOtherDeadCount = OtherDeadCount + 1;
        _ ->
            case IsSelfWin of
                true ->
                    NewSelfCount = SelfCount,
                    NewOtherCount = OtherCount + 1,
                    NewSelfDeadCount = SelfDeadCount,
                    NewOtherDeadCount = OtherDeadCount + 1;
                false ->
                    NewSelfCount = SelfCount + 1,
                    NewOtherCount = OtherCount,
                    NewSelfDeadCount = SelfDeadCount + 1,
                    NewOtherDeadCount = OtherDeadCount
            end
    end,
    NewIsAtk = not IsSelfWin,
    if
        NewSelfCount =:= 4 ->
            {ok, NewSelfTeam, NewOtherTeam, false, NewSelfDeadCount, NewOtherDeadCount,lists:reverse(NewReplayUIDList),lists:reverse(NewFightRecordList)};
        NewOtherCount =:= 4 ->
            {ok, NewSelfTeam, NewOtherTeam, true, NewSelfDeadCount, NewOtherDeadCount,lists:reverse(NewReplayUIDList),lists:reverse(NewFightRecordList)};
        true ->
            do_team_fight_loop(NewSelfTeam, NewOtherTeam, NewSelfCount, NewOtherCount,NewIsAtk,
                               NewSelfDeadCount, NewOtherDeadCount,NewReplayUIDList,NewFightRecordList)
    end.

reparse_fighterData({A,B,C}) ->
	reparse_fighterData({A,B,C,#trSpecial{}});
reparse_fighterData({A,B,C,D})->
    {A,B,C,D,#skin_info{}};
reparse_fighterData(D) ->
	D.

do_fight(Self, Other, IsAtk) ->
    #team_member{roleID=SelfRoleID,fighterData=SelfFighterData,itemList=ItemListSelf} = Self,
    #team_member{roleID=OtherRoleID,fighterData=OtherFighterData,itemList=ItemListOther} = Other,
	{SelfFighterList, LieuAddSelf, SelfTalentList,SelfTr,SelfSkinInfo} = reparse_fighterData(SelfFighterData),
	{OtherFighterList, LieuAddOther, OtherTalentList,OtherTr,OtherSkinInfo} = reparse_fighterData(OtherFighterData),
    %?ERR("Self:~w ~nOther:~w ~n IsAtk:~w ~n",[Self,Other,IsAtk]),
    %%将数据库中获得的精灵装备列表按照精灵分类
    SelfGerEquipList = role_item:assort_ger_equiplist(ItemListSelf),
    SelfLegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-SelfGerEquipList],
    OtherGerEquipList = role_item:assort_ger_equiplist(ItemListOther),
    OtherLegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-OtherGerEquipList],
	case IsAtk of
        true ->
            case catch role_fight:new(SelfFighterList, OtherFighterList, LieuAddSelf, LieuAddOther,SelfTalentList,OtherTalentList,SelfTr,OtherTr,SelfSkinInfo,OtherSkinInfo,SelfLegendAddList,OtherLegendAddList) of
                {IsWin, FightRecord0, {_,_,NewSelfFighterListT,NewOtherFighterListT}} ->
                    FightRecord = check_stone_effect(SelfRoleID,OtherRoleID,FightRecord0),
                    NewOtherFighterListT2 = lists:map(fun(#ger{gerBase=GerBase}=Ger) ->
                                                          NewGerBase = GerBase#gerBase{gerPos=erlang:abs(GerBase#gerBase.gerPos)},
                                                          Ger#ger{gerBase=NewGerBase}
                                                  end, NewOtherFighterListT),
                    NewSelfFighterList = filter_for_hp_not_zero(NewSelfFighterListT, SelfFighterList),
                    NewOtherFighterList = filter_for_hp_not_zero(NewOtherFighterListT2, OtherFighterList),
                    ReplayUID = tk_id:gen_replayUID(),
                    catch db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_3V3);
                _ ->
                    ?ERR("fight error"),
                    IsWin = true,
                    ReplayUID = 0,
                    FightRecord = #sc_fight_request{actionList=[],fighterList=[],result=true},
                    NewSelfFighterList = SelfFighterList,
                    NewOtherFighterList = lists:map(fun(Ger) ->
                                                          Ger#ger{gerHp=0}
                                                  end, OtherFighterList)
            end;
        false ->
            case catch role_fight:new2(SelfFighterList, OtherFighterList, LieuAddSelf, LieuAddOther, SelfTalentList, OtherTalentList,SelfTr,OtherTr,SelfSkinInfo,OtherSkinInfo,SelfLegendAddList,OtherLegendAddList) of
                {IsWin, FightRecord0, {_,_,NewOtherFighterListT,NewSelfFighterListT}} ->
                    FightRecord1 = check_stone_effect(SelfRoleID,OtherRoleID,FightRecord0),
                    NewOtherFighterListT2 = lists:map(fun(#ger{gerBase=GerBase}=Ger) ->
                                                          NewGerBase = GerBase#gerBase{gerPos=erlang:abs(GerBase#gerBase.gerPos)},
                                                          Ger#ger{gerBase=NewGerBase}
                                                  end, NewOtherFighterListT),
                    NewSelfFighterList = filter_for_hp_not_zero(NewSelfFighterListT, SelfFighterList),
                    NewOtherFighterList = filter_for_hp_not_zero(NewOtherFighterListT2, OtherFighterList),
                    ReplayUID = tk_id:gen_replayUID(),
					            % 为了适应客户端的显示，如果是被打的一方，需要反转胜利结果。
					FightRecord = FightRecord1#sc_fight_request{result=not FightRecord1#sc_fight_request.result},
                    catch db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_3V3);
                _ ->
                    ?ERR("fight error"),
                    IsWin = true,
                    ReplayUID = 0,
                    FightRecord = #sc_fight_request{actionList=[],fighterList=[],result=true},
                    NewOtherFighterList = OtherFighterList,
                    NewSelfFighterList = lists:map(fun(Ger) ->
                                                          Ger#ger{gerHp=0}
                                                  end, SelfFighterList)
            end
    end,
    {ok, Self#team_member{fighterData={NewSelfFighterList,LieuAddSelf,SelfTalentList,SelfTr,SelfSkinInfo}},
     Other#team_member{fighterData={NewOtherFighterList,LieuAddOther,OtherTalentList,OtherTr,OtherSkinInfo}},
     IsWin,ReplayUID,FightRecord, NewSelfFighterList =:= NewOtherFighterList}.

check_stone_effect(AtkRoleID,TarRoleID,FightRecord)->
    SelfID = role_data:get_roleID(),
    FighterList2 = case SelfID of
        % 如果这个ID是自己的的话，就不必读取数据库
        AtkRoleID ->
            {GerEquipList, _BagEquip, _BagItem} = role_data:get_all_item(),
            SelfEquipList = [ [ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,item_lib:itemDecay(ItemDecay),ItemExp,ItemEnchantType,ItemEnchantLevel]
                        || {ItemGerID, ItemList} <- GerEquipList,
                           #item{itemUID=ItemUID
                                 ,itemTypeID=ItemTypeID
                                 ,itemDecay=ItemDecay
                                 ,itemLevel=ItemLevel
                                 ,itemPos=ItemPos
                                 ,itemRank=ItemRank
                                 ,itemExp = ItemExp
                                 ,itemenchantType=ItemEnchantType
                                 ,itemenchantLevel=ItemEnchantLevel} <- ItemList],
            role_data:get_FighterList_with_effect(SelfEquipList,TarRoleID,FightRecord#sc_fight_request.fighterList);
        _ ->
            role_data:get_FighterList_with_effect(AtkRoleID,TarRoleID,FightRecord#sc_fight_request.fighterList)
    end,
    FightRecord#sc_fight_request{fighterList=FighterList2}.    

filter_for_hp_not_zero(List, OldList) ->
    % ?ERR("List :~w OldList :~w ~n",[List,OldList]),
    lists:foldr(fun(#ger{gerID=GerID, gerHp=GerHP, gerProHp=GerProHp}, Acc) ->
                        case GerHP > 0 of
                            true ->
                                OldGer = lists:keyfind(GerID, #ger.gerID, OldList),
                                [OldGer#ger{gerHp=GerHP, gerProHp=GerProHp}|Acc];
                            false ->
                                Acc
                        end
                end, [], List).

check_team_fight() ->
    TeamPkInfo = role_data:get_roleTeamPkInfo(),
    case erlang:is_record(TeamPkInfo, team_pk_info) of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    #team_pk_info{selfTeam=SelfTeam,otherTeam=OtherTeam} = TeamPkInfo,
    case erlang:length(SelfTeam) =:= 3 andalso erlang:length(OtherTeam) =:= 3 of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    #roleTimes{teamPkTimes=TeamPkTimes} = RoleTimes = role_data:get_roleTimes(),
    case TeamPkTimes > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,
    case get_team_pk_status() of
        1 ->
            next;
        0 ->
            erlang:throw({false, 4})
    end,
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    RoleFighterList = role_data:get_posList(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
	TrSpecial = role_data:get_trSpecial(),
    SkinInfo = role_skin:get_skin_info(),
    MemberRole = lists:keyfind(RoleID, #team_member.roleID, SelfTeam),
    NewMemberRole = MemberRole#team_member{fighterData={RoleFighterList,RoleLieuAdd,TalentList,TrSpecial,SkinInfo}},
    NewSelfTeam = lists:keyreplace(RoleID, #team_member.roleID, SelfTeam, NewMemberRole),
    NewTeamPkInfo = TeamPkInfo#team_pk_info{selfTeam=NewSelfTeam},
    {ok, NewTeamPkInfo, RoleTimes, RoleInfo}.
    
cs_team_self_record(#cs_team_self_record{}) ->
    case role_data:get_roleTeamPkInfo() of
        ?undefined ->
            ?sendself(#sc_team_self_record{recordList=[]});
        #team_pk_info{selfRecordList=SelfRecordList} ->
            ?DEBUG("SelfRecordList:~w", [SelfRecordList]),
            ?sendself(#sc_team_self_record{recordList=trans2pselfrecord(SelfRecordList)})
    end.


cs_team_rank(#cs_team_rank{}) ->
    ?DEBUG("cs_team_rank"),
    erlang:send(team_pk_server, {cs_team_rank, role_data:get_roleInfo()}).

%%战报查询缓存
get_replay_record(ReplayUID)->
    case erlang:get({?self3v3ReplayRecord, ReplayUID}) of
        undefined ->
            case db_sql:get_fightReplay(ReplayUID) of
                []->
                    case ReplayUID of
                        0 ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 1};
                        _ ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 2}
                    end;
                Rec->
                    erlang:put({?self3v3ReplayRecord, ReplayUID}, Rec),
                    {Rec, 1}
            end;
        Cached ->
            {Cached, 1}
    end.

%% 清除所有的战报缓存
erase_replay_record() ->
    lists:foreach(fun({{?self3v3ReplayRecord, ReplayUID},_}) ->
                          erlang:erase({?self3v3ReplayRecord, ReplayUID});
                     (_) ->
                          next
                  end, erlang:get()).

%% 清除指定战报缓存
erase_replay_record(ReplayUIDList) ->
    lists:foreach(fun(ReplayUID) ->
                          erlang:erase({?self3v3ReplayRecord, ReplayUID})
                  end, ReplayUIDList).

cs_team_self_fight_replay(#cs_team_self_fight_replay{replayUIDList=ReplayUIDList}) ->
    case role_data:get_roleTeamPkInfo() of
        ?undefined ->
            ?sendself(#sc_team_self_fight_replay{result=2,fightInfoList=[]});
        #team_pk_info{selfRecordList=SelfRecordList} ->
            case lists:keyfind(ReplayUIDList, #team_self_record.replayUIDList, SelfRecordList) of
                false ->
                    ?sendself(#sc_team_self_fight_replay{result=2,fightInfoList=[]});
                #team_self_record{selfList=PSelfList,otherList=POtherList} ->
                    {Result, FightInfoList} = get_team_self_fight_replay(ReplayUIDList, []),
                    ?sendself(#sc_team_self_fight_replay{result=Result,fightInfoList=FightInfoList,selfList=PSelfList,otherList=POtherList})
            end
    end.

get_team_self_fight_replay([], FightInfoList) ->
    {1, lists:reverse(FightInfoList)};
get_team_self_fight_replay([ReplayUID|ReplayUIDList], FightInfoList) ->
    case get_replay_record(ReplayUID) of
        {FightInfo, 1} ->
            get_team_self_fight_replay(ReplayUIDList, [FightInfo|FightInfoList]);
        {_FightInfo, 2} ->
            {2, []}
    end.

trans2pselfrecord(List) ->
    lists:map(fun(#team_self_record{timestamp=Timestamp,isWin=IsWin,addExp=AddExp,addCoin=AddCoin,
                                    addScore=AddScore,selfNameList=SelfNameList,otherNameList=OtherNameList ,
                                    replayUIDList=ReplayUIDList}) ->
                      #p_team_self_record{timestamp=Timestamp,isWin=IsWin,addExp=AddExp,addCoin=AddCoin,
                                          addScore=AddScore,selfNameList=SelfNameList,otherNameList=OtherNameList ,
                                          replayUIDList=ReplayUIDList}
              end, List).


cs_team_view_other(#cs_team_view_other{tarRoleID=TarRoleID}) ->
    case role_data:get_roleTeamPkInfo() of
        ?undefined ->
            ?sendself(#sc_team_view_other{tarRoleID=TarRoleID,roleName="",roleLevel=0,fightPower=0,gerList=[]});
        #team_pk_info{selfTeam=SelfTeam,otherTeam=OtherTeam} ->
            case lists:keyfind(TarRoleID, #team_member.roleID, SelfTeam ++ OtherTeam) of
                false ->
                    ?sendself(#sc_team_view_other{tarRoleID=TarRoleID,roleName="",roleLevel=0,fightPower=0,gerList=[]});
                #team_member{fightPower=FightPower,level=Level,roleName=RoleName,fighterData={FighterList,_,_,_,_}} ->
                    GerViewList = [ger_lib:ger2p_ger_view(E)||E<-FighterList],
                    ?sendself(#sc_team_view_other{tarRoleID=TarRoleID,roleName=RoleName,roleLevel=Level,fightPower=FightPower,gerList=GerViewList})
            end
    end.

cs_team_view_other_dtl(#cs_team_view_other_dtl{tarRoleID=TarRoleID}) ->
    case role_data:get_roleTeamPkInfo() of
        ?undefined ->
            ?sendself(#sc_team_view_other_dtl{tarRoleID=TarRoleID,roleName="",isMale=false,roleLevel=0,fightPower=0,gerList=[],equipList=[]
                                             , atkAdd=0, hpAdd=0,lieuViewList=[],head=0,title=0});
        #team_pk_info{selfTeam=SelfTeam,otherTeam=OtherTeam} ->
            case lists:keyfind(TarRoleID, #team_member.roleID, SelfTeam ++ OtherTeam) of
                false ->
                    ?sendself(#sc_team_view_other_dtl{tarRoleID=TarRoleID,roleName="",isMale=false,roleLevel=0,fightPower=0,gerList=[],equipList=[]
                                                     , atkAdd=0, hpAdd=0,lieuViewList=[],head=0,title=0});
                #team_member{fightPower=FightPower,isMale=IsMale,level=Level,roleName=RoleName,fighterData={FighterList,{AtkAdd, HpAdd},_,_,_},itemList=ItemList,head=Head,title=Title} ->
                    cs_team_view_other_dtl2(TarRoleID, RoleName, IsMale, FightPower, Level, FighterList, ItemList, AtkAdd, HpAdd,Head,Title)
            end
    end.


cs_team_view_other_dtl2(TarRoleID, RoleName, IsMale, FightPower, Level, FighterListT, ItemList, AtkAdd, HpAdd,Head,Title) ->
    FighterList = ger_attr:refresh_other_fightPower(FighterListT, AtkAdd, HpAdd),
    GerViewList = [ger_lib:ger2p_ger_view_dtl(E)||E<-FighterList],
    GerPosList = [ger_lib:ger2p_ger_pos(E)||E<-FighterList],
    EquipViewList = [item_lib:item2p_item_view_dtl(E)||E<-ItemList],
    Record = 
        #sc_team_view_other_dtl{tarRoleID=TarRoleID,roleName=RoleName,isMale=IsMale,roleLevel=Level,fightPower=FightPower, gerList=GerViewList
                               ,equipList=EquipViewList,gerPosList=GerPosList,atkAdd=AtkAdd, hpAdd=HpAdd,lieuViewList=[],head=Head,title=Title},
    ?sendself(Record).
