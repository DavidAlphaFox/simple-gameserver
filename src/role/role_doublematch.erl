-module(role_doublematch).
-compile(export_all).
-include("def_role.hrl").
-include("def_fight.hrl").
-include("def_doublematch.hrl").

-define(LEADER_AUTHORITY_RANK,1).
-define(VICE_LEADER_AUTHORITY_RANK,2).
-define(NORMAL_MEMBER_AUTHORITY_RANK,3).
-define(TURNS_BASE,100).
-define(SIGN_DOUBLEMATH_AUTHORITY,1).
-define(UPDATE_LINEUP_AUTHORITY,2).
%%========================来自doublematch_server中的定义,需要同步修改==========================
-define(match_state_standby,1).
-define(match_state_sign,2).
-define(match_state_fight,3).

%%%===================================从role_generalteam中定义，需要同步更新==================
-define(NORMAL_TEAM_TYPE,1).
-define(DOUBLEMATCH_TEAM_TYPE,2). 
%%============================================================================================

%%=============================================================================================
%% 客户端请求
% cs_doublematch_change_teamtype(#cs_doublematch_change_teamtype{newteamtype=NewTeamType})->
%     RoleInfo = role_data:get_roleInfo(),
%     case catch do_cs_doublematch_change_teamtype(NewTeamType,RoleInfo) of
%         {false,Reason}->
%             ?sendself(#sc_doublematch_change_teamtype{result=Reason});
%         true->
%             ?sendself(#sc_doublematch_change_teamtype{result=1})
%     end.
cs_doublematch_lineup(#cs_doublematch_lineup{})->
    #role{roleID=RoleID,teamId=TeamID} = role_data:get_roleInfo(),
    send_doublematch_lineup_info(RoleID,TeamID).

cs_doublematch_update_lineup(#cs_doublematch_update_lineup{gerposlist=GerPosList})->
    #role{roleID=RoleID,teamId=TeamID} = role_data:get_roleInfo(),
    case catch check_update_lineup(RoleID,TeamID) of
        {false,Reason}->
            ?sendself(#sc_doublematch_update_lineup{result=Reason});
        true->
            case catch do_update_lineup(GerPosList,TeamID) of
                {false,Reason}->
                    ?sendself(#sc_doublematch_update_lineup{result=Reason});
                true->
                    brocast_update_lineup(TeamID)
            end
    end.
cs_doublematch_sign(#cs_doublematch_sign{})->
    #role{roleID=RoleID,teamId=TeamID} = role_data:get_roleInfo(),
    case catch check_sign(RoleID,TeamID,2) of
        {false,Reason}->
            ?sendself(#sc_doublematch_sign{result=Reason});
        {true,_SignInfo}->
            do_sign(RoleID,TeamID)
            %?sendself(#sc_doublematch_sign{result=1})
    end.

cs_doublematch_sign_cancel(#cs_doublematch_sign_cancel{})->
    #role{roleID=RoleID,teamId=TeamID} = role_data:get_roleInfo(),
    case catch check_sign_cancel(RoleID,TeamID) of
        {false, Reason}->
            ?sendself(#sc_doublematch_sign_cancel{result=Reason});
        {true,CancelSignInfo}->
            do_sign_cancle(CancelSignInfo),
            ?sendself(#sc_doublematch_sign_cancel{result=1})
    end.

cs_doublematch_times_buy(#cs_doublematch_times_buy{})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    case check_times_buy(RoleID) of
        {false,Reason}->
            ?sendself(#sc_doublematch_times_buy{result=Reason,cur_times=0,next_buy_cost=0});
        {true,Cost}->
            RoleInfo=role_data:get_roleInfo(),
            role_lib:deduct_gold_f(RoleInfo, Cost, ?MONEY_DEC_TYPE_BUY_DOUBLEMATCH_TIMES, 0, ""),
            doublematch_server:doublematch_add_times(RoleID)
    end.
cs_doublematch_roleinfo(#cs_doublematch_roleinfo{})->
    doublematch_server:get_dm_info_msg().
%%     #role{roleID=RoleID} = role_data:get_roleInfo(),
%%     send_doublematch_roleinfo(RoleID).

cs_doublematch_rankinfo(#cs_doublematch_rankinfo{type=Type,position=BeginPosition,length=Length})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    case Type of
        ?FRIEND_RANK ->
            FriendList = friend_server:get_role_friend_roleID(RoleID),
            doublematch_server:get_dm_friend_rank(RoleID,FriendList);
        ?All_REGION_RANK ->
            doublematch_server:get_dm_rank_info(BeginPosition,Length);
        ?LOCAL_REGION_RANK ->
            doublematch_server:get_dm_role_rank(BeginPosition,Length);
        ?OLD_ALL_RANK ->
            doublematch_server:get_dm_old_rank(BeginPosition,Length);
        _ ->
            ?ERR("undefined Type:~w ~n",[Type]),
            ?sendself(#sc_doublematch_rankinfo{result=2,type=Type,position=BeginPosition,ranklist=[],length=Length})
    end.

cs_doublematch_record_list(#cs_doublematch_record_list{beginindex=BeginIndex,type=Type,length=Length})->
    doublematch_server:get_dm_rec_list(BeginIndex, Type, Length).

cs_doublematch_replay(#cs_doublematch_replay{recordID=RecID})->
    doublematch_server:get_dm_fight_record(RecID).

%==============================================================================================
check_sign(_RoleID,_TeamID,0)->
    {false,6};
check_sign(RoleID,TeamID,Times)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            case TeamID=:=-1 of
                true->
                    {true,RoleID};
                false->
                    #team_info{teamtype=OldTeamType} = GerneralTeamInfo =  team_manage_server:get_team_info_by_teamid(TeamID),
                    RoleInfo = role_data:get_roleInfo(),
                    case catch role_generalteam:do_change_teamtype(OldTeamType,?DOUBLEMATCH_TEAM_TYPE,RoleInfo) of
                        {false,5}->
                            {false,3};
                        {false,6}->
                            {false,8};
                        {false,_}->
                            {false,6};
                        true->
                            check_sign(RoleID,TeamID,Times-1)
                    end
            end;
        DoubleMatchTeamInfo->
            case check_authority_by_doubleteaminfo(RoleID,DoubleMatchTeamInfo,?SIGN_DOUBLEMATH_AUTHORITY) of
                false->
                    erlang:throw({false,3});
                true->
                    {true,DoubleMatchTeamInfo}
            end
    end.

check_sign_cancel(RoleID,TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            {true,RoleID};
        DoubleMatchTeamInfo->
            case check_authority_by_doubleteaminfo(RoleID,DoubleMatchTeamInfo,?SIGN_DOUBLEMATH_AUTHORITY) of
                false->
                    erlang:throw({false,3});
                true->
                    {true,DoubleMatchTeamInfo}
            end
    end.

check_times_buy(RoleID)->
    AlreadyBuy = doublematch_server:get_buy_time(RoleID),
    Cost = get_buy_cost(AlreadyBuy+1),
    RoleInfo = role_data:get_roleInfo(),
    case check_times_buy_sec() of
        true ->
            case role_lib:check_money(RoleInfo, gold, Cost) of
                true ->
                    {true,Cost};
                false ->
                    {false,2}   % 钱不够
            end;
        false ->
            {false,3}   % 不能购买太快
    end.    

check_times_buy_sec()->
    Now = util:now(),
    case erlang:get(check_times_buy_sec) of
        X when is_integer(X) ->
            if 
                X < Now ->
                    put(check_times_buy_sec,Now+1),
                    true;
                true ->
                    false
            end;
        _ ->
            put(check_times_buy_sec, Now+1),
            true
    end.

get_buy_cost(NewAlreadyBuy)->
    case data_doublematch:get({buy_cost,NewAlreadyBuy}) of
        ?undefined->
            data_doublematch:get(buy_cost_max);  % 已经达到上限
        Cost ->
            Cost
    end.

%% %%此处需要从doublematch_server获取战报的概要列表
%% send_doublematch_recordinfo(BeginIndex)->
%%     RecordList = [],
%%     Length = data_doublematch:get(data_record_length),
%%     SubRecordList = case length(RecordList) >= BeginIndex of
%%         true->
%%             lists:sublist(RecordList,BeginIndex,Length);
%%         false->
%%             []
%%     end,
%%     PRecordUnitList = [transformDMFightRec2PRecordUnit(DMFightRec)||DMFightRec<-SubRecordList],
%%     todo,
%%     ?sendself(#sc_doublematch_record_list{result=1,recordlist=PRecordUnitList,beginindex=0,type=0,length=0}).
%%=============================================================================================
%%根据组队信息创建
create_doublematch_teaminfo(TeamInfo,RoleID)->
    #team_info{teamid=TeamID,teamleader_roleid=TeamLeaderRoleID,vice_teamleader_rolelist=ViceRoleList,team_member=TeamMemberList} = TeamInfo,
    DoubleMatchTeamMemberList = generate_doublematch_teammemberlist(TeamLeaderRoleID,ViceRoleList,TeamMemberList,RoleID),
    #doublematch_teaminfo{teamid=TeamID,team_memberlist=DoubleMatchTeamMemberList}.


generate_doublematch_teammemberlist(TeamLeaderRoleID,ViceRoleIDList,TeamMemberList,RoleID)->
    generate_doublematch_teammemberlist2(TeamLeaderRoleID,ViceRoleIDList,TeamMemberList,[],RoleID).

generate_doublematch_teammemberlist2(_TeamLeaderRoleID,_ViceRoleIDList,[],DoubleMatchTeamMemberList,_RoleID)->
    DoubleMatchTeamMemberList;
generate_doublematch_teammemberlist2(TeamLeaderRoleID,ViceRoleIDList,[H|T],DoubleMatchTeamMemberList,RoleID)->
    DoubleMatchTeamMember = generate_doublematch_teammember(TeamLeaderRoleID,ViceRoleIDList,H,DoubleMatchTeamMemberList,RoleID),
    generate_doublematch_teammemberlist2(TeamLeaderRoleID,ViceRoleIDList,T,[DoubleMatchTeamMember|DoubleMatchTeamMemberList],RoleID).

% -record(doublematch_teammemberinfo,{roleID=0,skin_info=[],talent=[],trainer=#trSpecial{},fighters=[],lieu=#add_attr{},authority_rank=0,fight_turns=0}).
%% 双排规定队长首发，所以此处传递进来的ExistDoubleMatchTeamMemberList意义不大，但是为了扩展功能准备
generate_doublematch_teammember(TeamLeaderRoleID,ViceRoleIDList,TeamMember,ExistDoubleMatchTeamMemberList,RoleID)->
    {FighterList1,RoleLieuAdd,Talent,TrSpecial,SkinInfo} = case RoleID =:=TeamMember#p_team_member_info.roleID of
        false->
            {FighterList2,RoleLieuAdd2,Talent2,TrSpecial2} = role_data:get_otherRoleFighter(TeamMember#p_team_member_info.roleID),
            SkinInfo2 = role_data:get_otherRoleSkinInfo(TeamMember#p_team_member_info.roleID),
            {FighterList2,RoleLieuAdd2,Talent2,TrSpecial2,SkinInfo2};
        true->
            PosList2 = role_data:get_posList(),
            RoleLieuAdd2 = role_data:get_lieu_add_attr(),
            TalentList2 = role_talent:get_active_talent_list(),
            TrSpecial2 = role_data:get_trSpecial(),
            SkinInfo2 = role_skin:get_skin_info(),
            {PosList2, RoleLieuAdd2,TalentList2,TrSpecial2,SkinInfo2}
    end,
    {AuthorityRank,FightTurns} = get_authority_and_fightturns(TeamLeaderRoleID,ViceRoleIDList,TeamMember#p_team_member_info.roleID,ExistDoubleMatchTeamMemberList),
    FighterList = [#doublematch_lineup_gerinfo{flage=AuthorityRank,ger=Fighter}||Fighter<-FighterList1],
    #doublematch_teammemberinfo{roleID=TeamMember#p_team_member_info.roleID,skin_info=SkinInfo,talent=Talent,trainer=TrSpecial,fighters=FighterList,lieu=RoleLieuAdd,authority_rank=AuthorityRank,fight_turns=FightTurns}.

%%双排队伍中，只有两个玩家，队长首发
get_authority_and_fightturns(TeamLeaderRoleID,ViceRoleIDList,RoleID,_ExistDoubleMatchTeamMemberList)->
    case TeamLeaderRoleID =:= RoleID of
        true->
            {?LEADER_AUTHORITY_RANK,1};
        false->
            case lists:member(RoleID,ViceRoleIDList) of
                true->
                    {?VICE_LEADER_AUTHORITY_RANK,2};
                false->
                    {?NORMAL_MEMBER_AUTHORITY_RANK,2}
            end
    end.

set_doublematch_teaminfo(DoubleMatchTeamInfo)->
    ets:insert(?ETS_DOUBLEMATCH_LINEUP_INFO,DoubleMatchTeamInfo).
get_doublematch_teaminfo(TeamID)->
    case ets:lookup(?ETS_DOUBLEMATCH_LINEUP_INFO,TeamID) of
        []->
            ?undefined;
        [X]->
            X
    end.

send_doublematch_lineup_info(RoleID,TeamID)->
    CurrentRoleID = case role_data:get_roleInfo() of
        #role{roleID=FindRoleID}->
            FindRoleID;
        _ ->
            0
    end,
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            ?ERR("can not find doublematchteaminfo by teamid:~w ~n",[TeamID]),
            ?unicast(RoleID,#sc_doublematch_lineup{result=2,memberlist=[]});
        DoubleMatchTeamInfo->
            ALLEquipList = get_all_equip(DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist),
            AllPosListT = get_all_posListT(DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist),
            MemberList = generate_p_doublematch_team_member_by_doublematch_teaminfo(DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist,[],CurrentRoleID,AllPosListT,ALLEquipList),
            ?unicast(RoleID,#sc_doublematch_lineup{result=1,memberlist=MemberList})
    end.

generate_p_doublematch_team_member_by_doublematch_teaminfo([],PDoubleMatchTeamMemberList,CurrentRoleID,_AllPosListT,_ALLEquipList)->
    PDoubleMatchTeamMemberList;
generate_p_doublematch_team_member_by_doublematch_teaminfo([H|T],PDoubleMatchTeamMemberList,CurrentRoleID,AllPosListT,ALLEquipList)->
    #doublematch_teammemberinfo{roleID=TarRoleID,fighters=FighterList,fight_turns=Turns} = H,
    RolePublic = role_lib:get_rolePublic(TarRoleID),
    PDoubleMatchTeamTrainerInfo = transformRolePublic2PDoubleMatchTeamTrainerInfo(RolePublic,CurrentRoleID),
    PDoubleMatchTeamGerInfoList = get_p_doublematch_teamgerinfo_by_fighter(ALLEquipList,FighterList,Turns,[],AllPosListT),
    PDoubleMatchTeamMember = #p_doublematch_team_member{trainer=PDoubleMatchTeamTrainerInfo,gerlist=PDoubleMatchTeamGerInfoList},
    generate_p_doublematch_team_member_by_doublematch_teaminfo(T,[PDoubleMatchTeamMember|PDoubleMatchTeamMemberList],CurrentRoleID,AllPosListT,ALLEquipList).

transformRolePublic2PDoubleMatchTeamTrainerInfo(RolePublic,CurrentRoleID)->
    #rolePublic{roleID=TarRoleID,roleName=RoleName,isMale=IsMale,title=Title,head=Head,trSpecial=TrSpecial,level=RoleLevel} = RolePublic,
    case TarRoleID =:= CurrentRoleID of
        false->
            #skin_info{equip=Equip} = role_data:get_otherRoleSkinInfo(TarRoleID);
        true->
            #skin_info{equip=Equip} = role_skin:get_skin_info()
    end,
    #p_doublematch_team_trainer_info{roleID=TarRoleID,roleName=RoleName,isMale=IsMale,title=Title,head=Head
                                    ,skinID=Equip,trSpecialID=TrSpecial,level=RoleLevel}.

get_p_doublematch_teamgerinfo_by_fighter(_ItemList,[],_Turns,GerInfoList,PosListT)->
    GerInfoList;
get_p_doublematch_teamgerinfo_by_fighter(ItemList,[H|T],Turns,GerInfoList,PosListT)->
    #doublematch_lineup_gerinfo{flage=Flage,ger=Fighter1} = H,
    {Fighter,Remain} = case lists:keytake(Fighter1#ger.gerID,#ger.gerID,PosListT) of
        {_,Find,Other}->
            GerBase = Find#ger.gerBase,
            NewGerBase = GerBase#gerBase{gerPos=Fighter1#ger.gerBase#gerBase.gerPos},
            {Find#ger{gerBase=NewGerBase},Other};
        false->
            ?ERR("can not find fighter:~w in postListT:~w ~n",[Fighter1,PosListT]),
            {Fighter1,PosListT}
    end,
    PGer = ger_lib:ger2p_ger(Fighter),
    PEquip = get_ger_equip_by_gerID(Fighter#ger.gerID,ItemList),
    DoubleMatchPos = generate_doublematch_pos(Fighter#ger.gerBase#gerBase.gerPos,Turns),
    PDoubleMatchTeamGerInfo = #p_doublematch_team_ger_info{pos=DoubleMatchPos,ger=PGer,equip=PEquip,flage=Flage},
    get_p_doublematch_teamgerinfo_by_fighter(ItemList,T,Turns,[PDoubleMatchTeamGerInfo|GerInfoList],Remain).

get_ger_equip_by_gerID(GerID,ItemList)->
    lists:foldl(fun(Item,Acc)->
        [ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,ItemDecay,ItemExp,ItemEnchantType,ItemEnchantLevel,ItemLegendRank] = Item,
        case ItemGerID=:=GerID of
            true->
                NewItem = #p_equip{itemUID=ItemUID,itemTypeID=ItemTypeID,itemLevel=ItemLevel,itemRank=ItemRank,itemGerID=GerID,itemPos=ItemPos,itemDecay=ItemDecay,itemExp=ItemExp,itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel,itemLegendRank=ItemLegendRank},
                [NewItem|Acc];
            false->
                Acc
        end
    end,[],ItemList).

generate_doublematch_pos(GerPos,Turns)->
    Turns * ?TURNS_BASE + GerPos.

%%此处需要重新确定那个精灵数量
generate_ger_pos_by_doublematchpos(DoubleMatchGerPos)->
    Turns = DoubleMatchGerPos div ?TURNS_BASE,
    GerPos = DoubleMatchGerPos rem ?TURNS_BASE,
    case GerPos > 6 of
        true->
            erlang:throw(false);
        false->
            {Turns,GerPos}
    end.

check_update_lineup(RoleID,TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            erlang:throw({false,2});
        DoubleMatchTeamInfo ->
            case check_authority_by_doubleteaminfo(RoleID,DoubleMatchTeamInfo,?UPDATE_LINEUP_AUTHORITY) of
                false->
                    erlang:throw({false,3});
                true->
                    true
            end
    end.

check_authority_by_teamid(RoleID,TeamID,Right)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            false;
        DoubleMatchTeamInfo ->
            check_authority_by_doubleteaminfo(RoleID,DoubleMatchTeamInfo,Right)
    end.

check_authority_by_doubleteaminfo(RoleID,DoubleMatchTeamInfo,Right)->
    case lists:keyfind(RoleID,#doublematch_teammemberinfo.roleID,DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist) of
        false->
            false;
        Find->
            check_authority2(Find#doublematch_teammemberinfo.authority_rank,Right)
    end.

check_authority2(AuthorityRank,Right)->
    case data_doublematch:get({data_doublematch_right,AuthorityRank}) of
        ?undefined->
            false;
        AuthorityList->
            lists:member(Right,AuthorityList)
    end.

do_update_lineup([],_TeamID)->
    true;
do_update_lineup(GerPosList,TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            {false,2};
        DoubleMatchTeamInfo->
            {GerList,GerEmptyTeamMemberList} = lists:foldl(fun(TeamMember,{GerAcc,TeamMemberAcc})->
                #doublematch_teammemberinfo{fighters=Fighters}=TeamMember,
                {GerAcc++Fighters,[TeamMember#doublematch_teammemberinfo{fighters=[]}|TeamMemberAcc]}
            end,{[],[]},DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist),
            case catch exchange_gerpos(DoubleMatchTeamInfo#doublematch_teaminfo{team_memberlist=GerEmptyTeamMemberList},GerList,GerPosList) of
                false->
                    erlang:throw({false,4});
                NewDoubleMatchTeamInfo->
                    % ?ERR("DoubleMatchTeamInfo:~w ~n",[NewDoubleMatchTeamInfo]),
                    set_doublematch_teaminfo(NewDoubleMatchTeamInfo),
                    true
            end
    end.

brocast_update_lineup(TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            ignore;
        DoubleMatchTeamInfo -> 
            CurrentRoleID = case role_data:get_roleInfo() of
                #role{roleID=FindRoleID}->
                    FindRoleID;
                _ ->
                    0
            end,
            ALLEquipList = get_all_equip(DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist),
            AllPosListT = get_all_posListT(DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist),
            MemberList = generate_p_doublematch_team_member_by_doublematch_teaminfo(DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist,[],CurrentRoleID,AllPosListT,ALLEquipList),
            lists:foreach(fun(TeamMember)->
                #doublematch_teammemberinfo{roleID=RoleID} = TeamMember,
                ?unicast(RoleID,#sc_doublematch_update_lineup{result=1,memberlist=MemberList})
                % ?unicast(RoleID,#sc_doublematch_lineup{result=5,memberlist=MemberList})
                % ?unicast(RoleID,#sc_doublematch_update_lineup{result=5,memberlist=[]})
            end,DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist)
    end.

%%DoubleMatchTeamInfo中的team_memberlist的fighters需要设置成空，最后根据GerPosMap以及GerList向对应的doublematch_teammemberinfo中添加对应的精灵
exchange_gerpos(DoubleMatchTeamInfo,GerList,GerPosMap)->
    {ResultDoubleMatchTeamInfo,ResultRemainGerList} = lists:foldl(fun(#p_ger_pos_unit{pos=Pos,gerID=GerID},{DoubleMatchTeamInfoAcc,RemainGerList})->
        case get_doublematch_lineup_gerinfo_by_gerID(GerID,RemainGerList) of
            false->
                erlang:throw(false);
            {Find,Other}->
                {Turns,GerPos} = generate_ger_pos_by_doublematchpos(Pos),
                GerBase = Find#doublematch_lineup_gerinfo.ger#ger.gerBase,
                NewGerBase = GerBase#gerBase{gerPos=GerPos},
                NewGer = Find#doublematch_lineup_gerinfo.ger#ger{gerBase=NewGerBase},
                NewDoubleGer = Find#doublematch_lineup_gerinfo{ger=NewGer},
                case lists:keytake(Turns,#doublematch_teammemberinfo.fight_turns,DoubleMatchTeamInfoAcc#doublematch_teaminfo.team_memberlist) of
                    false->
                        erlang:throw(false);
                    {_,FindDoubleMatchTeamMember,OtherMemeber} ->
                        OldFighterList = FindDoubleMatchTeamMember#doublematch_teammemberinfo.fighters,
                        NewFindDoubleMatchTeamMember = FindDoubleMatchTeamMember#doublematch_teammemberinfo{fighters=[NewDoubleGer|OldFighterList]},
                        NewDoubleMatchTeamInfo = DoubleMatchTeamInfoAcc#doublematch_teaminfo{team_memberlist=[NewFindDoubleMatchTeamMember|OtherMemeber]},
                        {NewDoubleMatchTeamInfo,Other}
                end
        end
    end,{DoubleMatchTeamInfo,GerList},GerPosMap),
    case length(ResultRemainGerList)=:=0 of
        true->
            ResultDoubleMatchTeamInfo;
        false->
            ?ERR("exist remain ger:~w ~n",[ResultRemainGerList]),
            false
    end.
get_doublematch_lineup_gerinfo_by_gerID(GerID,List)->
    get_doublematch_lineup_gerinfo_by_gerID(GerID,List,[]).
get_doublematch_lineup_gerinfo_by_gerID(_GerID,[],_Other)->
    false;
get_doublematch_lineup_gerinfo_by_gerID(GerID,[H|T],Other)->
    #doublematch_lineup_gerinfo{ger=Ger} = H,
    case Ger#ger.gerID =:= GerID of
        true->
            {H,T++Other};
        false->
            get_doublematch_lineup_gerinfo_by_gerID(GerID,T,[H|Other])
    end.

do_sign(RoleID,-1)->
    do_signle_sign(RoleID);
do_sign(RoleID,TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            do_signle_sign(RoleID);
        DoubleMatchTeamInfo->
            do_team_sign(RoleID,TeamID,DoubleMatchTeamInfo)
    end.

do_signle_sign(RoleID) ->
    #role{roleID=TarRoleID} = role_data:get_roleInfo(),
    case RoleID =/= TarRoleID of
        true->
            {FighterList1,RoleLieuAdd,Talent,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
            SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
            ItemList = role_data:get_otherRoleItemEquips(RoleID);
        false->
            FighterList1 = role_data:get_posList(),
            RoleLieuAdd = role_data:get_lieu_add_attr(),
            Talent = role_talent:get_active_talent_list(),
            TrSpecial = role_data:get_trSpecial(),
            SkinInfo = role_skin:get_skin_info(),
            ItemList = role_data:get_equipts_on_ger()
    end,
    {AuthorityRank,FightTurns} = get_authority_and_fightturns(RoleID,[],RoleID,[]),
    FighterList = [#doublematch_lineup_gerinfo{flage=AuthorityRank,ger=Fighter}||Fighter<-FighterList1],
    DoubleMatch_TeamMember = #doublematch_teammemberinfo{roleID=RoleID,skin_info=SkinInfo,talent=Talent,trainer=TrSpecial,fighters=FighterList,lieu=RoleLieuAdd,authority_rank=AuthorityRank,fight_turns=FightTurns,itemList=ItemList},
    do_send_sign_info_to_doublematch_server(RoleID,-1,[DoubleMatch_TeamMember]).

do_team_sign(RoleID,TeamID,DoubleMatchTeamInfo)->
    do_send_sign_info_to_doublematch_server(RoleID,TeamID,DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist).

%%此处需要向doublematch_server发送报名信息,布阵中使用的TeamMember与战斗需要的list不一样，需要进行转换成{RoleId,#double_team{}}
do_send_sign_info_to_doublematch_server(RoleID,TeamID,TeamMemberList)->
    TeamMemberList1 = lists:keysort(#doublematch_teammemberinfo.fight_turns,TeamMemberList),
    NewTeamMemberList = [transformDMTeamMember2DMTeam(TeamMember)||TeamMember<-TeamMemberList1],
    % ?ERR("RoleID:~w TeamID:~w ~n",[RoleID,TeamID]),
    doublematch_server:doublematch_sign(RoleID,TeamID,NewTeamMemberList).

%%双排队员信息,需要注意的地方是由于布阵的过程中，需要精灵有队长和非队长的区别，所以fighters列表中为#doublematch_lineup_gerinfo{}方式保存
% {RoleId,#double_team{}}
transformDMTeamMember2DMTeam(TeamMember)->
    #doublematch_teammemberinfo{roleID=RoleID,skin_info=SkinInfo,talent=Talent,trainer=TrSpecial,fighters=FighterList1,lieu=RoleLieuAdd,itemList=ItemList}=TeamMember,
    FighterList = [Ger||#doublematch_lineup_gerinfo{ger=Ger}<-FighterList1],
    {RoleID,#double_team{skin_info=SkinInfo,talent=Talent,trainer=TrSpecial,fighters=FighterList,lieu=RoleLieuAdd,itemList=ItemList}}.

is_sign(RoleID)->
    case doublematch_server:doublematch_state(RoleID) of
        ?match_state_standby->
            false;
        ?match_state_sign ->
            true
    end.
is_sign_by_teamID(TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            false;
        DoubleMatchTeamInfo->
            case lists:keyfind(?LEADER_AUTHORITY_RANK,#doublematch_teammemberinfo.authority_rank,DoubleMatchTeamInfo#doublematch_teaminfo.team_memberlist) of
                false->
                    false;
                #doublematch_teammemberinfo{roleID=LeaderRoleID}->
                    is_sign(LeaderRoleID)
            end
    end.

do_sign_cancle(RoleID) when is_number(RoleID)->
    % ?ERR("RoleID:~w sign cancle~n",[RoleID]),
    doublematch_server:doublematch_cancel(RoleID);

do_sign_cancle(DoubleMatchTeamInfo) when is_record(DoubleMatchTeamInfo,doublematch_teaminfo)->
    #doublematch_teaminfo{team_memberlist=TeamMemberList} = DoubleMatchTeamInfo,
    case lists:keyfind(1,#doublematch_teammemberinfo.authority_rank,TeamMemberList) of
        false->
            ?ERR("can not find leader in doublematch team:~w ~n",[DoubleMatchTeamInfo]),
            ignore;
        Find->
            doublematch_server:doublematch_cancel(Find#doublematch_teammemberinfo.roleID)
    end.

check_role_level_limit(Level)->
    case data_doublematch:get(data_open_level) of
        ?undefined->
            false;
        NeedLevel->
            Level >= NeedLevel
    end.

%% get_buy_times_cost(AlreadyBuyTimes)->
%%     case data_doublematch:get({buy_cost,AlreadyBuyTimes}) of
%%         ?undefined->
%%             case data_doublematch:get(max_cost) of
%%                 ?undefined->
%%                     1000;
%%                 MaxCost->
%%                     MaxCost
%%             end;
%%         Cost->
%%             Cost
%%     end.

do_set_role_times(RoleID,NewTimes)->
    ok.

%% do_doublematch_friend_rankinfo(RoleID,BeginPosition)->
%%     FriendList = friend_server:get_role_friend_roleID(RoleID),
%%     FriendDMRank1 = lists:foldl(fun(RoleID,Acc)->
%%         case doublematch_server:get_friend_dm(RoleID) of
%%             ?undefined->
%%                 Acc;
%%             DMInfo->
%%                 [DMInfo|Acc]
%%         end
%%     end,[],FriendList),
%%     FriendDMRank = sort_dm_rank(FriendDMRank1),
%%     Length = data_doublematch:get(data_rank_length),
%%     ?INFO("do_doublematch_friend_rankinfo ~w ~w",[FriendDMRank, BeginPosition]),
%%     case length(FriendDMRank) >= BeginPosition of
%%         true->
%%             lists:sublist(FriendDMRank,BeginPosition,Length);
%%         false->
%%             []
%%     end.
%% do_doublematch_allregion_rankinfo(RoleID,BeginPosition)->
%%     AllRegionRankList = doublematch_server:get_dm_rank_info(),
%%     Length = data_doublematch:get(data_rank_length),
%%     case length(AllRegionRankList) >= BeginPosition of
%%         true->
%%             lists:sublist(AllRegionRankList,BeginPosition,Length);
%%         false->
%%             []
%%     end.

%%FIX此处需要调用doublematch_server的排序接口
sort_dm_rank(DMRankList)->
    lists:sort(fun(A,B)->
                  if
                      A#dm_rank.rank > B#dm_rank.rank ->
                          true;
                      A#dm_rank.rank < B#dm_rank.rank ->
                          false;
                      A#dm_rank.score  > B#dm_rank.score  ->
                          true;
                      A#dm_rank.score  < B#dm_rank.score  ->
                          false;
                      A#dm_rank.role_id < B#dm_rank.role_id ->
                          true;
                      A#dm_rank.role_id > B#dm_rank.role_id ->
                          false
                  end
        end,DMRankList).

transformDMRank2RankUnit(DMRankInfo) when is_record(DMRankInfo,dm_rank)->
    #dm_rank{role_id=RoleID,server_id=ServerID,name=Name,index=Index,remain_time=RemainTime
            ,already_buy=AlreadyBuyTimes,level=Level,rank=Rank,score=Score,isMale=IsMale,title=Title,head=Head
            ,score_change=ScoreChange,vip=Vip} = DMRankInfo,
    {_,GRank,GLevel,_,_} = data_doublematch:get({rank,Rank}),
    RankTop = data_doublematch:get(rank_top),
    RankScore = if
                    Rank >= RankTop ->
                        {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
                        Score - TopScore;
                    true ->
                        Score rem 100
                end,
    #p_doublematch_rank_unit{roleID=RoleID
                            ,roleName=Name
                            ,isMale=IsMale
                            ,title=Title
                            ,head=Head
                            ,level=Level
                            ,serverID=ServerID
                            ,grade=GRank
                            ,gradeLevel=GLevel
                            ,score=RankScore
                            ,rank=Index
                            ,score_change=ScoreChange
                            ,vip=Vip};
transformDMRank2RankUnit(DMRankInfo)->
    DMInfoList = erlang:tuple_to_list(DMRankInfo),
    case hd(DMInfoList) == dm_rank andalso length(DMInfoList) < 20 of
        true -> NDMRankInfo = erlang:list_to_tuple(DMInfoList++[1]),
                transformDMRank2RankUnit(NDMRankInfo);
        _ ->
            ?ERR("undefined DMRankInfo:~w ~n",[DMRankInfo]),
            #p_doublematch_rank_unit{}
    end.

create_teaminfo_by_normal_team(TeamInfo,RoleID)->
    DoubleMatchTeamInfo = create_doublematch_teaminfo(TeamInfo,RoleID),
    set_doublematch_teaminfo(DoubleMatchTeamInfo).

%%此处传递过来的TeamInfo是在组队中的队伍信息，需要获取到doublematch的组队信息予以删除
delete_teaminfo_by_normal_team(TeamInfo,RoleID)->
    #team_info{teamid=TeamID} = TeamInfo,
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            ignore;
        DoubleMatchTeamInfo->
            delete_doublematch_teaminfo(DoubleMatchTeamInfo)
    end.

%%此处还需要向双排所有玩家广播退出双排消息
delete_doublematch_teaminfo(TeamInfo)->
    #doublematch_teaminfo{teamid=TeamID,team_memberlist=TeamMemberList} = TeamInfo,
    ets:delete(?ETS_DOUBLEMATCH_LINEUP_INFO,TeamID),
    Msg = #sc_doublematch_quit{result=1},
    lists:foreach(fun(TeamMember)->
        #doublematch_teammemberinfo{roleID=RoleID} = TeamMember,
        ?unicast(RoleID,Msg)
    end,TeamMemberList).

send_deleteteaminfo_from_team_manage(TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            ignore;
        DoubleMatchTeamInfo->
            delete_doublematch_teaminfo(DoubleMatchTeamInfo)
    end.

%% transformDMFightRec2PRecordUnit(DMFightRec)->
%%     #dm_fight_rec{rec_id=RecordID,leaderA=LeaderA,fellowA=FellowA,leaderB=LeaderB,fellowB=FellowB,timestamp=TimeStamp,result=Result}=DMFightRec,
%%     RecordTeamMember = [transformDMFighter2PDMRankUnit(LeaderA),transformDMFighter2PDMRankUnit(FellowA),transformDMFighter2PDMRankUnit(LeaderB),transformDMFighter2PDMRankUnit(FellowB)],
%%     #p_record_unit{recordID=RecordID,timestamp=TimeStamp,recordteammember=RecordTeamMember}.
%% transformDMFighter2PDMRankUnit(DMFighter)->
%%     #dm_fighter{role_id=RoleID,server_id=ServerID,name=Name,level=Level,index=Index,rank=Rank,score=Score,isMale=IsMale,title=Title,head=Head}=DMFighter,
%%     {_,GRank,GLevel,_,_} = data_doublematch:get({rank,Rank}),
%%     RankTop = data_doublematch:get(rank_top),
%%     RankScore = if
%%                     Rank >= RankTop ->
%%                         {TopScore,_GRank,_GLevel,_,_} = data_doublematch:get({rank,RankTop}),
%%                         Score - TopScore;
%%                     true ->
%%                         Score rem 100
%%                 end,
%%     #p_doublematch_rank_unit{roleID=RoleID,roleName=Name,isMale=IsMale,title=Title,head=Head,level=Level,serverID=ServerID
%%                             ,grade=GRank,gradeLevel=GLevel,score=RankScore,rank=Index}.

set_sign_timeout()->
    Timeout = data_doublematch:get(sign_timeout) * 1000,
    SignTimerRef = 
        erlang:start_timer(Timeout, self(), {dm_sign_timeout}),
    put(?sign_timeout_ref,SignTimerRef),
    SignTimerRef.

change_leader(TarRoleID,TeamID)->
    case get_doublematch_teaminfo(TeamID) of
        ?undefined->
            ignore;
        DoubleMatchTeamInfo->
            #doublematch_teaminfo{team_memberlist=TeamMemberList} = DoubleMatchTeamInfo,
            TeamMemberList1 = lists:foldl(fun(TeamMember,Acc)->
                #doublematch_teammemberinfo{authority_rank=AuthorityRank} = TeamMember,
                case AuthorityRank of
                    ?LEADER_AUTHORITY_RANK->
                        OldFighterList = TeamMember#doublematch_teammemberinfo.fighters,
                        FighterList = [Fighter#doublematch_lineup_gerinfo{flage=?NORMAL_MEMBER_AUTHORITY_RANK}||Fighter<-OldFighterList],
                        [TeamMember#doublematch_teammemberinfo{authority_rank=?NORMAL_MEMBER_AUTHORITY_RANK,fight_turns=2,fighters=FighterList}|Acc];
                    _ ->
                        [TeamMember|Acc]
                end
            end,[],TeamMemberList),
            case lists:keytake(TarRoleID,#doublematch_teammemberinfo.roleID,TeamMemberList1) of
                {_,Find,Other}->
                    OldFighterList = Find#doublematch_teammemberinfo.fighters,
                    FighterList = [Fighter#doublematch_lineup_gerinfo{flage=?NORMAL_MEMBER_AUTHORITY_RANK}||Fighter<-OldFighterList],
                    NewTeamMemberList = [Find#doublematch_teammemberinfo{authority_rank=?LEADER_AUTHORITY_RANK,fight_turns=1,fighters=FighterList}|Other],
                    NewDoubleMatchTeamInfo= DoubleMatchTeamInfo#doublematch_teaminfo{team_memberlist=NewTeamMemberList},
                    set_doublematch_teaminfo(NewDoubleMatchTeamInfo),
                    brocast_update_lineup(TeamID);
                false->
                    ?ERR("can not find TarRoleID:~w ~n in team:~w ~n",[TarRoleID,TeamMemberList1]),
                    ignore
            end
    end.

get_all_posListT(TeamMemberList)->
    get_all_posListT(TeamMemberList,[]).
get_all_posListT([],PosListTList)->
    PosListTList;
get_all_posListT([H|T],PosListTList)->
    #doublematch_teammemberinfo{roleID=TarRoleID} = H,
    RoleID = case role_data:get_roleInfo() of
        #role{roleID=CurrentRoleID}->
            CurrentRoleID;
        _->
            0
    end,
    PosListT = case TarRoleID =:= RoleID of
        true->
            role_data:get_posListT();
        false->
            role_data:get_otherRolePosListT(TarRoleID)
    end,
    get_all_posListT(T,PosListT++PosListTList).

get_all_equip(TeamMemberList)->
    get_all_equip(TeamMemberList,[]).
get_all_equip([],EquipList)->
    EquipList;
get_all_equip([H|T],EquipList)->
    #doublematch_teammemberinfo{roleID=TarRoleID} = H,
    RoleID = case role_data:get_roleInfo() of
        #role{roleID=CurrentRoleID}->
            CurrentRoleID;
        _->
            0
    end,
    ItemList = case TarRoleID =:= RoleID of
        true->
            {GerEquipList, _BagEquip, _BagItem} = role_data:get_all_item(),
            ItemList1 = [ [ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,item_lib:itemDecay(ItemDecay),ItemExp,ItemEnchantType,ItemEnchantLevel,ItemLegendRank]
            || {ItemGerID, ItemList} <- GerEquipList,
            #item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemDecay=ItemDecay,itemLevel=ItemLevel,itemPos=ItemPos,itemRank=ItemRank,itemExp = ItemExp,itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel,itemLegendRank=ItemLegendRank} <- ItemList];
        false->
            role_data:get_otherRoleItemEquips(TarRoleID)
    end,
    get_all_equip(T,ItemList++EquipList).

check_doublematch_changeauthority_limit(Type,TarRoleID,TeamID)->
    case is_sign_by_teamID(TeamID) of
        true->
            false;
        false->
            true
    end.
