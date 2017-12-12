-module(role_carlos).

-compile(export_all).
-include("def_role.hrl").
-include("def_carlos.hrl").
-include("def_reward.hrl").
-define(CURRENTSEASON,1).
-define(PRESEASON,2).
%%报名
cs_carlos_sign(_) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    IsFirst = role_data:check_plane_ai_flag_type(?ai_flag_carlos),
    #times{has_buy=BuyTimes, free_left=FreeLefts} = Time = carlos_server:get_times(RoleID),
    Left = BuyTimes + FreeLefts,
    ?INFO("carlos time is ~w ~n",[Time]),
    case check_sign(RoleInfo, RoleID, Left) of
        {ok, TeamId, TeamMember, Level} when IsFirst =:= true andalso TeamId =:= -1 ->
            %% 此分之说明是该玩家第一次进入卡洛斯
            RobotMemberIDList = util:random_list(db_sql:get_roleIDList_Robot(), (2 * data_carlos:get(match_need))-1),
            RobotTeamMember = [#p_team_member_info{roleID=RID}||RID<-[RoleID|RobotMemberIDList]],
            role_data:save_plane_ai_flag_type(?ai_flag_carlos),
            carlos_server:send_carlos_war_msg({sign, RoleID, 0, RobotTeamMember, Level});
        {ok, TeamId, TeamMember, Level} ->
            if
                IsFirst =:= true -> %% 如果玩家第一次是组队的，那么就丧失了第一次对战AI的机会
                    role_data:save_plane_ai_flag_type(?ai_flag_carlos);
                true ->
                    ignore
            end,
            carlos_server:send_carlos_war_msg({sign, RoleID, TeamId, TeamMember, Level});
        {false, Result} ->
            ?sendself(#sc_carlos_sign{result=Result, times=Left}) 
    end.

%%购买
cs_carlos_buy(_) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    CarlosTimes = carlos_server:get_times(RoleID),
    case check_buy(RoleInfo, CarlosTimes) of
        {Result, NewCarlosTimes, Cost, CurTime, Left} ->
            role_lib:deduct_gold_f(RoleInfo, Cost, ?MONEY_DEC_TYPE_CARLOS_BUY, 0, ""),
            carlos_server:set_times(RoleID, NewCarlosTimes);
        {Result, CurTime, Left} ->
            ignore
    end,
    %%?ERR("购买结果:~p,当前购买次数:~p,剩余购买次数:~p", [Result, CurTime, Left]),
    ?sendself(#sc_carlos_buy{result=Result, cur_times=CurTime, times=Left}).

%%获取数据
cs_carlos_info(_) ->
    #role{roleID=RoleID,plane_level=PlaneLevel} = role_data:get_roleInfo(),
    #times{has_buy=BuyTimes, buy_left=BuyLeft, free_left=FreeLefts} = carlos_server:get_times(RoleID),
    {BuyTimeLMT, TimeCost} = data_carlos:get(buy_limit),
	WinGas = data_carlos:get(winner_gas),
	{AP,PT}=afk_record_server:get_afk_punish(RoleID,?AFK_TYPE_CARLOS),
    ?sendself(#sc_carlos_info{times=BuyTimes + FreeLefts, cur_times=BuyTimeLMT - BuyLeft
							 ,buy_left=BuyLeft, golds=TimeCost,plane_level=PlaneLevel,winGas=WinGas
                             ,afk_punish = AP, punish_timestamp = PT}),
	#plane_use_info{planes=PlaneInfoList,use=Use}=role_data:get_plane_use_info(),
	?sendself(#sc_carlos_plane_use_info{planeInfo=plane_info2p_plane_info(PlaneInfoList),display=plane(Use)}).

%%取消匹配
cs_carlos_unrequest(_) ->
    #role{roleID=RoleID, teamId=TeamId} = role_data:get_roleInfo(),
    case TeamId =:= -1 of
        true ->
            case carlos_server:is_role_sign(RoleID) of
                false ->
                    ?sendself(#sc_carlos_unrequest{result=1});
                {true, ID} ->
                    erlang:send(carlos_server, {unrequest, ID})
            end;
        _ ->
            case ets:lookup(ets_team_list, TeamId) of
                [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
                    case carlos_server:is_team_sign(TeamId, erlang:length(TeamMember)) of
                        false ->
                            ?sendself(#sc_carlos_unrequest{result=1});
                        {true, ID} ->
                            erlang:send(carlos_server, {unrequest, ID})
                    end;
                _ ->
                    ?sendself(#sc_carlos_unrequest{result=2}) 
            end
    end.

cs_carlos_war_base_info(_)->
    update_role_carlos_time(),
	send_carlos_war_msg(cs_carlos_war_base_info).

cs_carlos_mine_detail(#cs_carlos_mine_detail{mineID=MineID}) ->
	send_carlos_war_msg({cs_carlos_mine_detail, MineID}).

cs_carlos_self(_) ->
	send_carlos_war_msg(cs_carlos_self).

cs_carlos_mov(#cs_carlos_mov{mineID=MineID}) ->
	send_carlos_war_msg({cs_carlos_mov, MineID, false}).

cs_carlos_ownMine(#cs_carlos_ownMine{mineID=MineID}) ->
	send_carlos_war_msg({cs_carlos_ownMine, MineID}).

cs_carlos_role_dtl(#cs_carlos_role_dtl{roleID=RoleID,serverID=ServerID}) ->
	send_carlos_war_msg({cs_carlos_role_dtl, RoleID,ServerID}).

cs_carlos_attack(#cs_carlos_attack{tarRoleID=TarRoleID,tarServerID=TarServerID,mineID=MineID}) ->
	send_carlos_war_msg({cs_carlos_attack, TarRoleID,TarServerID,MineID,false}).

cs_carlos_mov_stop(_) ->
	send_carlos_war_msg(cs_carlos_mov_stop).

cs_carlos_talk(#cs_carlos_talk{data=Data}) ->
	#role{roleName=Name,roleID=RoleID} = role_data:get_roleInfo(),
	Data2 = #p_carlos_talk{roleID=RoleID,roleName=Name,data=util:words_filter(Data)},
	send_carlos_war_msg({cs_carlos_talk, Data2}).

cs_carlos_get_talk(_) ->
	send_carlos_war_msg(cs_carlos_get_talk).

cs_carlos_get_rank(_) ->
	send_carlos_war_msg(cs_carlos_get_rank).

cs_carlos_replay(#cs_carlos_replay{replayUID=ReplayUID}) ->
		RoleID = role_data:get_roleID(),
	case ets:lookup(?ETS_CARLOS_ROLE_WAR,{match_info,RoleID}) of
		[] ->
			?sendself(#sc_carlos_war_base_info{result=2,attackerPos=#p_carlos_pos{x=0,y=0},defenderPos=#p_carlos_pos{x=0,y=0}});
		[{_, {_WarID, MatchServerID,_Sec}}] ->
			carlos_replay_server:get_replay(ReplayUID, MatchServerID,RoleID)
	end.

cs_carlos_reborn(_) ->
	NeedMoney = data_carlos:get(reborn_money),
	Role = role_data:get_roleInfo(),
	case role_lib:check_money(Role,gold,  NeedMoney) of
		true ->
			role_lib:deduct_gold_f(Role, NeedMoney, ?MONEY_DEC_TYPE_CARLOS_REBORN, 0, ""),
			send_carlos_war_msg(cs_carlos_reborn),
			?sendself(#sc_carlos_reborn{result=1});
		_ ->
			?sendself(#sc_carlos_reborn{result=2})
	end.
	
%%飞机升级
cs_carlos_plane_uplevel(_) ->
    RoleInfo = role_data:get_roleInfo(),
    case check_plane_uplevel(RoleInfo) of
        {Result, Cost, NewLevel} ->
			RoleInfo2 = role_data:get_roleInfo(),
            RoleInfo3 = deduct_cost(Cost, RoleInfo2, ?MONEY_DEC_TYPE_CARLOS_PLANE_UPLEVEL),
            NewRoleInfo = RoleInfo3#role{plane_level=NewLevel},
			role_data:set_roleInfo(NewRoleInfo),
            role_lib:update_rolePublic(NewRoleInfo,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data()),
            %%触发玩家飞机等级的任务变化
            role_payGuide:trigger_task_change(?PLANE_LEVEL_N,{0});
        Result ->
			NewLevel = RoleInfo#role.plane_level,
            Result
    end,
    ?sendself(#sc_carlos_plane_uplevel{result=Result,plane_level=NewLevel}).

cs_carlos_rank_list(#cs_carlos_rank_list{type=Type})->

    case gen_server:call(carlos_server, {get_seasoninfo,Type}) of
        {[],0}->
            RoleInfo = role_data:get_roleInfo(),
            PlayerRankInfo = transformrole2player_rank_info(RoleInfo,0,Type),
            ?sendself(#sc_carlos_rank_list{result=2,type=Type,year=0,season=0,ranklist=[],rolerankinfo=PlayerRankInfo});
        {RankInfoList,SeasonID}-> 
            ShowRankInfoList = lists:sublist(RankInfoList,data_carlos:get(max_show_rank_num)),
            PlayerRankList = [transformp_player_info2player_rank_info(Elem)||Elem<-ShowRankInfoList],
            RoleInfo = role_data:get_roleInfo(),
            PlayerRankInfo = transformrole2player_rank_info(RoleInfo,SeasonID,Type),
            Rank = case lists:keyfind(PlayerRankInfo#player_rank_info.roleID,#p_player_info.roleID,RankInfoList) of
                false->
                    0;
                FindOne->
                    FindOne#p_player_info.rank
            end,
            ?sendself(#sc_carlos_rank_list{result=1,type=Type,year=?get_season_year(SeasonID),season=?get_season_seasonid(SeasonID),ranklist=PlayerRankList,rolerankinfo=PlayerRankInfo#player_rank_info{rank=Rank}})
    end.

cs_carlos_season_info(#cs_carlos_season_info{})->
    {Year,Mon,Day} = carlos_rank:get_next_season_begin(erlang:date()),
    ?sendself(#sc_carlos_season_info{result=1,timestamp=util:datetime_to_seconds({{Year,Mon,Day},{0,0,0}})}).



cs_carlos_change_plane(#cs_carlos_change_plane{itemUID=ItemUID}) ->
	case check_change_plane(ItemUID) of
		{true,  OtherItem, Item,PlaneUseInfo} ->
			do_change_plane( OtherItem, Item,PlaneUseInfo);
		{false,Reason} ->
			?sendself(#sc_carlos_change_plane{result=Reason})
			end.

check_change_plane(ItemUID) ->
	BagItem = role_data:get_bagItem(),
	case lists:keytake(ItemUID, #item.itemUID, BagItem) of
		false ->
			{false,2};
		{value, Item=#item{itemTypeID=ItemTypeID,itemNum=ItemNum}, OtherItem} ->
			{CostNum,PlaneV,V1}=data_carlos:get({plane_item,ItemTypeID}),
			if ItemNum < CostNum ->
				   {false,5};
			   true ->
				   #role{plane_level=PlaneLevel} = role_data:get_roleInfo(),
				   case PlaneLevel == data_carlos:get(max_plane_level) of
					   true ->
						   PlaneUseInfo= role_data:get_plane_use_info(),
                           check_change_plane(PlaneV,PlaneUseInfo,Item,OtherItem,V1);
					   _ ->
						   {false,3}
				   end
			end
	end.

check_change_plane(PlaneV,#plane_use_info{planes=Planes}=PlaneUseInfo,Item,OtherItem,V1) ->
    case lists:keyfind(PlaneV, #plane_info.type, Planes) of
        false -> {false,4};
        #plane_info{valid=Valid} ->
            case util:now() > Valid orelse V1 == -1 of 
                true -> {true,OtherItem,Item,PlaneUseInfo};
                _->{false,4}
            end
    end.

do_change_plane2(#plane_use_info{planes=Planes}=PlaneUseInfo,Valid,Type) ->
    case lists:keytake(Type, #plane_info.type, Planes) of
        false -> PlaneUseInfo#plane_use_info{planes=[#plane_info{type=Type,state=1,valid=get_valid_time(Valid)}|Planes]};
        {value,_,OtherPlanes} ->PlaneUseInfo#plane_use_info{planes=[#plane_info{type=Type,state=1,valid=get_valid_time(Valid)}|OtherPlanes]}
    end.

get_valid_time(-1) -> -1;
get_valid_time(Valid) -> util:now()+ Valid.

do_change_plane( OtherItem, Item=#item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemNum=Num},PlaneUseInfo) ->
	{CostNum,Plane,Valid} = data_carlos:get({plane_item,ItemTypeID}), 
	if Num > CostNum ->
		   ?sendself(#sc_item_update{updateList=[#p_item_num_update{itemUID=ItemUID,itemNum=Num-1}]}),
		   role_data:set_bagItem([Item#item{itemNum=Num-1}|OtherItem]);
	   true ->
		   ?sendself(#sc_item_delete_notify{itemUIDList=[ItemUID]}),
		   role_data:set_bagItem(OtherItem)
	end,
	LogItemList = [[ItemUID,ItemTypeID,Num,Num-CostNum]],
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_CARLOS_CHANGE_PLANE, 0, ""),
	#plane_use_info{use=Use,planes=Planes} =PlaneUseInfo2 = do_change_plane2(PlaneUseInfo,Valid,Plane),
	role_data:set_plane_use_info(PlaneUseInfo2),
	?sendself(#sc_carlos_change_plane{result=1}),
	?sendself(#sc_carlos_plane_use_info{planeInfo=plane_info2p_plane_info(Planes),display=plane(Use)}).

cs_carlos_plane_select(#cs_carlos_plane_select{type=Type}) ->
	case check_plane_select(Type) of
		{true,#plane_use_info{planes=PlaneInfoList}=PlaneUseInfo} ->
			role_data:set_plane_use_info(PlaneUseInfo#plane_use_info{use=Type}),
			RolePublic = role_lib:get_rolePublic(role_data:get_roleID()),
			ets:insert(?ETS_ROLE_PUBLIC,RolePublic#rolePublic{plane_level=Type}),
			?sendself(#sc_carlos_plane_select{result=1}),
            ?sendself(#sc_carlos_plane_use_info{planeInfo=plane_info2p_plane_info(PlaneInfoList),display=plane(Type)});
		{false,Reason} ->
			?sendself(#sc_carlos_plane_select{result=Reason})
			end.

check_plane_select(Type) ->
	MaxPlaneLevel=data_carlos:get(max_plane_level),
	#plane_use_info{planes=Planes,use=Use}  =PlaneUseInfo = role_data:get_plane_use_info(),
    if Type == MaxPlaneLevel -> {true,PlaneUseInfo};
       true -> case lists:keyfind(Type, #plane_info.type, Planes) of
                   false -> {false,3};
                   #plane_info{state=State} ->
                       case State of 0 -> {false,3};
                           _-> case Use of Type -> {false,2};
                                   _ -> {true,PlaneUseInfo}
                               end
                       end
               end
    end.

plane_info2p_plane_info(L) ->
    [#p_carlos_plane_dtl{type=Type,validTime=Valid}||#plane_info{type=Type,valid=Valid}<-L].

update_plane_info(Info,Now,Use) ->
    #plane_info{state=_State,valid=Valid,type=Type} = Info,
    if Now > Valid andalso Valid > 0 -> {true,#plane_info{type=Type,state=0,valid=0}, off_plane(Use,Type)};
       true -> {false,Info,Use}
    end.
 
update_plane_info()->
	#plane_use_info{planes=PlaneInfoList,use=Use} = role_data:get_plane_use_info(),
	Now = util:now(),
    {IsChange,PlaneInfoList2,Use2} = 
	lists:foldl(fun(P,{CAcc,PAcc,UAcc}) ->
                        {IC,PD,UD} = update_plane_info(P,Now,UAcc),
                        {IC orelse CAcc,[PD|PAcc],UD}
                end, {false,[],Use}, PlaneInfoList),
    if IsChange ->
           role_data:set_plane_use_info(#plane_use_info{planes=PlaneInfoList2,use=Use2}),
           ?sendself(#sc_carlos_plane_use_info{planeInfo=plane_info2p_plane_info(PlaneInfoList),display=plane(Use2)}),
		   case Use of
			   Use2 -> ignore;
			   _ ->
				   RoleInfo = role_data:get_roleInfo(),
				   RolePublic = role_lib:get_rolePublic(RoleInfo#role.roleID),
				   ets:insert(?ETS_ROLE_PUBLIC,RolePublic#rolePublic{plane_level=RoleInfo#role.plane_level})
		   end;
	   true ->
		   ignore
	end.

off_plane(A,A) ->
	0;
off_plane(A,_) ->
	A.

%下面是巨龙遗迹
cs_carlos_relic_info(#cs_carlos_relic_info{})->
    #role{roleID=RoleID,plane_level=PlaneLevel} = role_data:get_roleInfo(),
    ?sendself(relic_server:get_sc_carlos_relic_info(RoleID,PlaneLevel)).

cs_carlos_relic_sign(#cs_carlos_relic_sign{level_rank=LevelRank})->
    carlos_relic_sign(LevelRank).

%为了便于测试，写成接口
carlos_relic_sign(LevelRank)->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    case check_relic_sign(RoleInfo,RoleID,LevelRank) of
        {ok,MemberList} ->
            % MemberList :: [#p_team_member_info{}]
            relic_server:send_to_me({cs_carlos_relic_sign,RoleID,MemberList,RoleInfo#role.teamId,LevelRank});
        {fail,Result} ->
            ?sendself(#sc_carlos_relic_sign{result=Result})
    end.

cs_carlos_relic_sign_cancel(_) ->
    #role{roleID=RoleID, teamId=TeamId} = role_data:get_roleInfo(),
    ServerID = data_setting:get(server_id),
    case TeamId =:= -1 of
        true ->
            case relic_server:is_role_sign(RoleID) of
                false ->
                    ?sendself(#sc_carlos_relic_sign_cancel{result=1});
                {true, SignID} ->
                    lists:foreach(fun(LevelRank)->
                                          send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {unrequest, ServerID, SignID})
                                  end, lists:seq(1, ?RELIC_LEVEL_NUM))
            end;
        _ ->
            case ets:lookup(ets_team_list, TeamId) of
                [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
                    case relic_server:is_team_sign(TeamId, erlang:length(TeamMember)) of
                        false ->
                            ?sendself(#sc_carlos_relic_sign_cancel{result=1});
                        {true, SignID} ->
                            lists:foreach(fun(LevelRank)->
                                                  send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {unrequest, ServerID, SignID})
                                          end, lists:seq(1, ?RELIC_LEVEL_NUM))
                    end;
                _ ->
                    ?sendself(#sc_carlos_relic_sign_cancel{result=2}) 
            end
    end.

cs_carlos_relic_buy(#cs_carlos_relic_buy{}) ->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    relic_server:send_to_me({cs_carlos_relic_buy,RoleID}).

cs_carlos_relic_open_reward_box(#cs_carlos_relic_open_reward_box{index=Index})->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    RelicRoleData = relic_server:get_relic_role_data(RoleID),
    if
        Index >=3 orelse Index >= 1 ->
            case lists:nth(Index, RelicRoleData#relic_role_data.box_status) of
                0 ->
                    ?sendself(#sc_carlos_relic_open_reward_box{result=2,reward=[]});
                1 ->
                    {BossLevelRank,RewardRank,_FinalGerTypeID} = RelicRoleData#relic_role_data.box_rank,
                    NeedGold = lists:nth(Index, data_relic:get({relic_box_cost,BossLevelRank,RewardRank})),
                    IsSleeper = RelicRoleData#relic_role_data.is_sleeper,
                    Reward0 = get_relix_box_reward(RelicRoleData,Index,IsSleeper),
                    Reward = case activity_server:is_activity(?relic_box_double) of
                                      true ->
                                          role_reward:reward_plus_reward(Reward0, Reward0);
                                      false ->
                                          Reward0
                                  end,
                    if
                        NeedGold =< 0 ->
                            do_relix_open_box(RelicRoleData,Index,Reward);
                        true ->
                            case role_lib:check_money(RoleInfo, gold, NeedGold) of
                                true ->
                                    role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_RELIC_OPEN_BOX, Index, ""
														  ,role_reward:transform2normal_reward(Reward)),
                                    do_relix_open_box(RelicRoleData,Index,Reward);
                                _ ->
                                    ?sendself(#sc_carlos_relic_open_reward_box{result=3,reward=[]})
                            end
                    end
            end;
        true ->
            ?sendself(#sc_carlos_relic_open_reward_box{result=2,reward=[]})
    end.

cs_carlos_relic_war_base_info(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_island_detail(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_self(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_mov(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_attack(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_active(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_role_dtl(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_mov_stop(Msg)->
    send_relic_war(Msg).

cs_carlos_relic_talk(#cs_carlos_relic_talk{data=Data})->
    #role{roleName=Name,roleID=RoleID} = role_data:get_roleInfo(),
    Data2 = #p_carlos_talk{roleID=RoleID,roleName=Name,data=util:words_filter(Data)},
    send_relic_war({cs_carlos_relic_talk, Data2}).

cs_carlos_relic_get_talk(Msg)->
    send_relic_war(Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 队伍报名判断
check_team(RoleID, TeamId) ->
    case ets:lookup(ets_team_list, TeamId) of
        [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
            Len = erlang:length(TeamMember),
            case carlos_server:is_team_sign(TeamId, Len) of
                false ->
                    case carlos_server:is_team_in_war(TeamMember) of
                        true ->
                            {false, 6};
                        _ ->
                            LvLimit = data_carlos:get(lv_limit),
                            {Pass, TLv} =
                                lists:foldl(fun(#p_team_member_info{roleID=MID}, {PassAcc, TLvAcc}) ->
                                              #rolePublic{level=MLv} = role_lib:get_rolePublic(MID),
                                              {PassAcc andalso MLv >= LvLimit, TLvAcc + MLv} 
                                            end, {true, 0}, TeamMember),
                            case Pass of 
                                true ->
                                    case lists:all(fun(#p_team_member_info{roleID=MID}) ->
                                                        #times{has_buy=MHasBuy, free_left=MFreeLefts} = carlos_server:get_times(MID),
                                                        MHasBuy + MFreeLefts > 0
                                                    end, TeamMember) of
                                        false ->
                                            {false, 5};
                                        _ ->
                                            IsAfkPunish = lists:any(fun(M)-> 
                                                                        {S,_Timestamp} = afk_record_server:get_afk_punish(M#p_team_member_info.roleID,?AFK_TYPE_CARLOS),
                                                                        2 =:= S
                                                            end, TeamMember),
                                            if
                                                IsAfkPunish =:= true ->
                                                    {false, 22};
                                                true ->
                                                    {ok, TeamId, TeamMember, erlang:trunc(TLv / Len)}
                                            end
                                    end;
                                _ ->
                                    {false, 7} 
                            end
                    end;
                _ ->
                    {false, 4} 
            end;
        _ ->
            {false, 1} 
    end.

%% 判断能否报名
check_sign(RoleInfo, RoleID, Left) ->
    case Left > 0 of
        false ->
            %fix 这里以前返回的4，是否应该返回5
            {false, 5};
        _ ->
            TeamId = RoleInfo#role.teamId,
            case TeamId =/= -1 of
                true ->
                    check_team(RoleID, TeamId);
                _ ->
                    case carlos_server:is_role_sign(RoleID) of
                        false ->
                            case carlos_server:is_role_in_war(RoleID) of
                                true ->
                                    {false, 6};
                                _ ->
                                    Level = RoleInfo#role.level,
                                    {S,_Timestamp} = afk_record_server:get_afk_punish(RoleID,?AFK_TYPE_CARLOS),
                                    case Level < data_carlos:get(lv_limit) of
                                        true ->
                                            {false, 7};
                                        _ when S =:= 2 ->
                                            {false, 21};
                                        _ ->
                                            {ok, TeamId, [#p_team_member_info{roleID=RoleID}], Level} 
                                    end
                            end;
                        _ ->
                            {false, 4} 
                    end 
            end
    end.

check_relic_sign(RoleInfo,RoleID,LevelRank)->
    TeamId = RoleInfo#role.teamId,
    {NeedLevel,NeedVip}=data_relic:get(need_lvl_vip),
    {RankMinLevel,NeedFightPower,_} = data_relic:get({fight_level,LevelRank}),
    FightPower = RoleInfo#role.fightPower,
    {IsTeamMember,MemberList} = if
                       TeamId =:= -1 ->
                           {false,[team_manage_server:transform_role2p_team_member_info(RoleInfo)]};
                       true ->
                           case ets:lookup(ets_team_list, TeamId) of
                               [#team_info{teamleader_roleid=LeaderRoleID,team_member=TeamMember}] ->
                                    if
                                        RoleID =:= LeaderRoleID ->
                                            {false,TeamMember};
                                        true ->
                                            {true,[]}
                                    end;
                               _ ->
                                   ?ERR("team id is wrong. not found team info ~w ~w",[RoleID,TeamId]),
                                   {true,[]}
                           end
                   end,
    IsAfkPunish = lists:any(fun(M)-> 
                                {S,_Timestamp} = afk_record_server:get_afk_punish(M#p_team_member_info.roleID,?AFK_TYPE_RELIC),
                                2 =:= S
                    end, MemberList),
    ?INFO("check_relic_sign ~w ~w ~w",[IsAfkPunish,IsTeamMember,MemberList]),
    if
        IsAfkPunish =:= true andalso TeamId =:= -1 ->
            {fail,21}; % 挂机惩罚中，未组队
        IsAfkPunish =:= true andalso TeamId /= -1 ->
            {fail,22}; % 挂机惩罚中，组队
        IsTeamMember =:= true ->
            {fail,2}; %组队了且没权限
        RoleInfo#role.level < NeedLevel ->
            {fail,3}; %等级不足
        RoleInfo#role.vipLevel < NeedVip ->
            {fail,4}; %Vip不足
        FightPower < NeedFightPower andalso IsTeamMember /= true ->
            {fail,10}; %战斗力不足。不是队员的话，需要检查战斗力
        RoleInfo#role.level < RankMinLevel ->
            {fail,3}; %等级不足
        true -> %ok不在队伍中
            {ok,MemberList}
    end.    

%% 判断能否购买
check_buy(RoleInfo, #times{buy_left=BuyLeft,has_buy=HasBuy} = CarlosTimes) ->
    {BuyTimeLMT, TimeCost} = data_carlos:get(buy_limit),
    HasBuyTimes = BuyTimeLMT - BuyLeft,
    case BuyLeft > 0 of
        false ->
            {2, HasBuyTimes, BuyLeft};
        _ ->
            NewHasBuyTimes = HasBuyTimes + 1,
            Cost = lists:nth(NewHasBuyTimes, TimeCost),
            %% fix 添加对购买钻石判断，添加上奖励钻石
            case RoleInfo#role.gold + RoleInfo#role.goldBonus < Cost of
                true ->
                    {1, HasBuyTimes, BuyLeft};
                _ ->
                    %% fix 是否购买的时候需要将hasbuy变量加1？
                    NewCarlosTimes = CarlosTimes#times{buy_left = BuyLeft - 1,has_buy = HasBuy+1},
                    {0, NewCarlosTimes, Cost, NewHasBuyTimes, BuyLeft - 1}
            end
    end.

send_client({send_client, {route,2}}) ->
	Msg = #sc_carlos_war_base_info{result=2,attackerPos=#p_carlos_pos{x=0,y=0},defenderPos=#p_carlos_pos{x=0,y=0}},
	?sendself(Msg);
send_client({send_client, Msg}) ->
	?sendself(Msg).

send_carlos_war_msg(Msg) ->
	RoleID = role_data:get_roleID(),
	case ets:lookup(?ETS_CARLOS_ROLE_WAR,{match_info,RoleID}) of
		[] ->
			?sendself(#sc_carlos_war_base_info{result=2,attackerPos=#p_carlos_pos{x=0,y=0},defenderPos=#p_carlos_pos{x=0,y=0}});
		[{_, {WarID, MatchServerID,_Sec}}] ->
			carlos_router:send_carlos({WarID,MatchServerID},Msg,RoleID)
	end.


%% 飞机升级判断
check_plane_uplevel(RoleInfo) ->
    #role{plane_level=PlaneLevel} = RoleInfo,
    NewLevel = PlaneLevel + 1,
    case data_carlos:get({plane, NewLevel}) of
         undefined ->
			 1;
		_ ->
            #data_carlos_plane{upcost=Cost} = data_carlos:get({plane, PlaneLevel}),
            case check_up_cost(Cost, RoleInfo) of
                0 ->
                   {0, Cost, NewLevel};
                Result ->
                    Result
            end
    end.

%% 判断升级消耗
check_up_cost([], _) ->
    0;
check_up_cost([{Type, Num}|T], RoleInfo) ->
    case role_lib:check_money(RoleInfo, Type, Num) of
        true ->
            check_up_cost(T, RoleInfo);
        _ ->
            case Type of
                gold ->
                    2;
                coin ->
                    3;
                reputation ->
                    4
            end
    end.

%% 扣除消耗
deduct_cost([], RoleInfo, _) ->
    RoleInfo;
deduct_cost([{Type, Num}|T], RoleInfo, MoneyType) ->
    NewRoleInfo = 
        case Type of
            gold ->
                role_lib:deduct_gold_f(RoleInfo, Num, MoneyType, 0, "");
            coin ->
                role_lib:deduct_coin_f(RoleInfo, Num, MoneyType, 0, "");
            reputation ->
                role_lib:deduct_reputation_f(RoleInfo, Num, MoneyType, 0, "") 
        end,
    deduct_cost(T, NewRoleInfo, MoneyType).

transformp_player_info2player_rank_info(P_Player_Info) when is_record(P_Player_Info,p_player_info)->
    #p_player_info{serverID=ServerID,roleID=RoleID,roleName=RoleName,fly=Fly,level=Level,score=_Score,head=Head,title=Title,
                   isMale=IsMale,type=_Type,wintime=WinTime,losetime=LoseTime,equaltime=EqualTime,rankscore=RankScore,rank=Rank,vip=Vip} = P_Player_Info,
    #player_rank_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,score=RankScore,wintime=WinTime,
                      losetime=LoseTime,equaltime=EqualTime,rank=Rank,serverID=ServerID,plantype=Fly,vip=Vip};

transformp_player_info2player_rank_info(P_Player_Info)->
    ?INFO("p_player_info转换到player_rank_info出现非p_player_info结构 P_Player_Info:~w ~n",[P_Player_Info]),
    #player_rank_info{}.   

%%由于飞机的等级只更新到了rolepublic中，所以此处需要先从rolepublic中取得一部分数据，如果rolepublic不存在数据再直接使用roleinfo
transformrole2player_rank_info(RoleInfo,SeasonID,Type) when is_record(RoleInfo,role)->
    #role{roleID=RoleID,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,carlosseasonid=CarlosSeason,
          carloslastwintime=PreWinTime,carloslastequaltime=PreEqualTime,carloslastlosetime=PreLoseTime,carloslastseasonid=PreSeasonID}=RoleInfo,
    RolePublic = role_lib:get_rolePublic(RoleID),
    #rolePublic{isMale=IsMale,level=Level,title=Title,roleName=RoleName,head=Head,plane_level=Fly,viplevel=VipLevel,svipLevel=SVipLevel} = RolePublic,
    Vip = role_lib:cacl_vip_info(VipLevel,SVipLevel),
    case Type of
        ?CURRENTSEASON->
            case CarlosSeason =:= SeasonID of
                true->
                    #player_rank_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,wintime=CarlosWinTime,
                                      score=calculate_rankscore(CarlosWinTime,CarlosLoseTime,CarlosEqualTime),losetime=CarlosLoseTime,
                                      equaltime=CarlosEqualTime,rank=0,serverID=util:calc_server_id(roleID,RoleID),plantype=Fly,vip=Vip};
                false->
                    #player_rank_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,score=0,wintime=0,
                                      losetime=0,equaltime=0,rank=0,serverID=util:calc_server_id(roleID,RoleID),plantype=Fly,vip=Vip}
            end;
        ?PRESEASON->
            case PreSeasonID =:= SeasonID of
                true->
                    #player_rank_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,wintime=PreWinTime, 
                                      score=calculate_rankscore(PreWinTime,PreLoseTime,PreEqualTime),losetime=PreLoseTime,vip=Vip,
                                      equaltime=PreEqualTime,rank=0,serverID=util:calc_server_id(roleID,RoleID),plantype=Fly};
                false->
                    case CarlosSeason =:= SeasonID of %%在赛季切换的时候，玩家如果一直在线，玩家对应的赛季信息不会被主动切换，所以此处存在玩家的当前赛季信息其实已经是上届赛季信息的情况
                        true->
                            #player_rank_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,wintime=CarlosWinTime, 
                                              score=calculate_rankscore(CarlosWinTime,CarlosLoseTime,CarlosEqualTime),losetime=CarlosLoseTime
                                              ,equaltime=CarlosEqualTime,rank=0,serverID=util:calc_server_id(roleID,RoleID),plantype=Fly,vip=Vip};
                        false->
                            #player_rank_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,score=0,wintime=0,
                                              losetime=0,equaltime=0,rank=0,serverID=util:calc_server_id(roleID,RoleID),plantype=Fly,vip=Vip}
                    end
            end
    end;
transformrole2player_rank_info(RoleInfo,_SeasonID,_Type)->
    ?INFO("将role转换成player_rank_info出现非role结构RoleInfo:~w ~n",[RoleInfo]),
    #player_rank_info{roleID=0,isMale=0,head=0,title=0,level=0,score=0,wintime=0,losetime=0,equaltime=0,rank=0,serverID=0,plantype=0,vip=1}.

calculate_rankscore(WinTime,LoseTime,EqualTime)->
    data_carlos:get(win_score)*WinTime + data_carlos:get(equal_score)*EqualTime + data_carlos:get(lose_score)*LoseTime.

update_role_carlos_time()->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    #times{has_buy=HasBuy, free_left=FreeLefts} = carlos_server:get_times(RoleID),
    ?sendself(#sc_carlos_times_update{times=HasBuy+FreeLefts}).

send_relic_war(Msg)->
    #role{roleID=RoleID,teamId=TeamID} = role_data:get_roleInfo(),
    case relic_server:get_warid(RoleID,TeamID) of
        {ok,WarID} ->
            ?INFO("send_relic_war ~w",[Msg]),
            send_msg:direct_by_name(carlos_match, relic_war_manager, {to_war_chlid,WarID,data_setting:get(server_id),RoleID,self(),Msg});
        _ ->
            RelicRoleData = relic_server:get_relic_role_data(RoleID),
            ?sendself(#sc_carlos_relic_war_base_info{result=2
                                                    ,endTimeStamp=0
                                                    ,islandes=[]
                                                    ,players=RelicRoleData#relic_role_data.last_player
                                                    ,other_info=RelicRoleData#relic_role_data.other_list
                                                    ,boss_active_timeout_max=0
                                                    ,atk_reinforce=0
                                                    ,damage_reduce=0})
    end.
    
get_relix_box_reward(RelicRoleData,_Index,IsSleeper) ->
    {BossLevelRank,Rank,FinalGerTypeID} = RelicRoleData#relic_role_data.box_rank,
	RewardRandomList1 = data_relic:get({finish_time_box_reward,BossLevelRank,Rank}),
    RewardRandomList2 = if
                            IsSleeper ->
                                [E||E<-RewardRandomList1,erlang:length(E) =:= 1];
                            true ->
                                RewardRandomList1
                        end,
    ?INFO("get_relix_box_reward  ---  RewardRandomList2 ~w",[RewardRandomList2]),
	MegaNum = data_relic:get({mega_reward,BossLevelRank,Rank}),
	{value,{_,MegaTypeID}} = lists:keysearch(FinalGerTypeID, 1, data_relic:get(boss_extra_reward)),
	lists:foldl(fun(RandomList,AccReward)->
						role_reward:reward_plus_reward(AccReward, util:random_one_from_weigh_list(RandomList))
				end, {sell_reward,0,0,0,0,[{new_item,MegaTypeID, MegaNum, 1, 0}],0,[]}, RewardRandomList2).

do_relix_open_box(RelicRoleData,Index,Reward)->
    RoleID = RelicRoleData#relic_role_data.role_id,
	OldBoxStatus = RelicRoleData#relic_role_data.box_status,
    role_reward:handle_sys_reward(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_RELIC_BOX, Index, ""),
    IsOpen = get_sc_carlos_relic_box_info(RelicRoleData),
    ?sendself(#sc_carlos_relic_open_reward_box{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward)]}),
    {_,NewBoxStatus} = list_replace(OldBoxStatus,Index,0),
    ets:update_element(?ETS_RELIC_ROLE_INFO, RoleID, {#relic_role_data.box_status,NewBoxStatus}).

get_sc_carlos_relic_box_info(RelicRoleData)->
    {_,BoxRank,_FinalGerTypeID} = RelicRoleData#relic_role_data.box_rank,
    BoxName = if BoxRank >= 1 andalso BoxRank =< 6 ->
                     data_relic:get({relic_box_name,BoxRank});
                 true ->[]
              end,
    if BoxName =:= [] -> [] ; true -> RelicRoleData#relic_role_data.box_status end.

%用E替换List中第Index个元素，返回{替换的元素,新list}
list_replace(List,Index,E) when Index >= 1 andalso is_list(List)->
    list_replace(List,Index,E,[]).
list_replace([],_,S,NewList)->
    {S,lists:reverse(NewList)};
list_replace([S|List],0,E,NewList)->
    list_replace(List,0,E,[S|NewList]);
list_replace([S|List],1,E,NewList)->
    list_replace(List,0,S,[E|NewList]);
list_replace([S|List],Index,E,NewList)->
    list_replace(List,Index-1,E,[S|NewList]).

plane(Tag)->
	case data_carlos:get({planeTag,Tag}) of
		X when is_integer(X) -> X;
		_ -> 0
	end.
