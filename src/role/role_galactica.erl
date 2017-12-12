-module(role_galactica).

-compile(export_all).
-include("def_role.hrl").
-include("def_carlos.hrl").


%%报名
cs_galactica_sign(_) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    IsFirst = role_data:check_plane_ai_flag_type(?ai_flag_galactica),
    #ga_times{has_buy=BuyTimes, free_left=FreeLefts} = Time = galactica_server:get_times(RoleID),
    Left = BuyTimes + FreeLefts,
    ?INFO("galactica time is ~w ~n",[Time]),
    case check_sign(RoleInfo, RoleID, Left) of
        {ok, TeamId, TeamMember, Level} when IsFirst =:= true andalso TeamId =:= -1 ->
            %% 此分之说明是该玩家第一次进入卡洛斯
            RobotMemberIDList = util:random_list(db_sql:get_roleIDList_Robot(), (2 * data_galactica:get(match_need))-1),
            RobotTeamMember = [#p_team_member_info{roleID=RID}||RID<-[RoleID|RobotMemberIDList]],
            role_data:save_plane_ai_flag_type(?ai_flag_galactica),
            galactica_server:send_galactica_war_msg({sign, RoleID, 0, RobotTeamMember, Level});
        {ok, TeamId, TeamMember, Level} ->
            if
                IsFirst =:= true -> %% 如果玩家第一次是组队的，那么就丧失了第一次对战AI的机会
                    role_data:save_plane_ai_flag_type(?ai_flag_galactica);
                true ->
                    ignore
            end,
            galactica_server:send_galactica_war_msg({sign, RoleID, TeamId, TeamMember, Level});
        {false, Result} ->
            ?sendself(#sc_galactica_sign{result=Result, times=Left}) 
    end.

%%购买
cs_galactica_buy(_) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    CarlosTimes = galactica_server:get_times(RoleID),
    case check_buy(RoleInfo, CarlosTimes) of
        {Result, NewCarlosTimes, Cost, CurTime, Left} ->
            role_lib:deduct_gold_f(RoleInfo, Cost, ?MONEY_DEC_TYPE_GALACTICA_BUY, 0, ""),
            galactica_server:set_times(RoleID, NewCarlosTimes);
        {Result, CurTime, Left} ->
            ignore
    end,
    %%?ERR("购买结果:~p,当前购买次数:~p,剩余购买次数:~p", [Result, CurTime, Left]),
    ?sendself(#sc_galactica_buy{result=Result, cur_times=CurTime, times=Left}).

%%获取数据
cs_galactica_info(_) ->
    #role{roleID=RoleID,plane_level=PlaneLevel} = role_data:get_roleInfo(),
    #ga_times{has_buy=BuyTimes, buy_left=BuyLeft, free_left=FreeLefts} = galactica_server:get_times(RoleID),
    {BuyTimeLMT, TimeCost} = data_galactica:get(buy_limit),
	WinGas = data_galactica:get(winner_gas),
	{AP,PT}=afk_record_server:get_afk_punish(RoleID,?AFK_TYPE_GALACTICA),
    ?sendself(#sc_galactica_info{times=BuyTimes + FreeLefts, cur_times=BuyTimeLMT - BuyLeft
							    ,buy_left=BuyLeft, golds=TimeCost,plane_level=PlaneLevel,winGas=WinGas
                                ,afk_punish = AP, punish_timestamp = PT}).

%%取消匹配
cs_galactica_unrequest(_) ->
    #role{roleID=RoleID, teamId=TeamId} = role_data:get_roleInfo(),
    case TeamId =:= -1 of
        true ->
            case galactica_server:is_role_sign(RoleID) of
                false ->
                    ?sendself(#sc_galactica_unrequest{result=1});
                {true, ID} ->
                    erlang:send(galactica_server, {unrequest, ID})
            end;
        _ ->
            case ets:lookup(ets_team_list, TeamId) of
                [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
                    case galactica_server:is_team_sign(TeamId, erlang:length(TeamMember)) of
                        false ->
                            ?sendself(#sc_galactica_unrequest{result=1});
                        {true, ID} ->
                            erlang:send(galactica_server, {unrequest, ID})
                    end;
                _ ->
                    ?sendself(#sc_galactica_unrequest{result=2}) 
            end
    end.

cs_galactica_war_base_info(_)->
    update_role_galactica_time(),
	send_galactica_war_msg(cs_galactica_war_base_info).

cs_galactica_self(_) ->
	send_galactica_war_msg(cs_galactica_self).

cs_galactica_mov(#cs_galactica_mov{mineID=MineID}) ->
	send_galactica_war_msg({cs_galactica_mov, MineID, false}).

cs_galactica_mov_in(#cs_galactica_mov_in{mineID=MineID}) ->
	send_galactica_war_msg({cs_galactica_mov_in,MineID}).

cs_galactica_role_dtl(#cs_galactica_role_dtl{roleID=RoleID,serverID=ServerID}) ->
	send_galactica_war_msg({cs_galactica_role_dtl, RoleID,ServerID}).

cs_galactica_attack(#cs_galactica_attack{tarRoleID=TarRoleID,tarServerID=TarServerID,mineID=MineID}) ->
	send_galactica_war_msg({cs_galactica_attack, TarRoleID,TarServerID,MineID,false}).

cs_galactica_mov_stop(_) ->
	send_galactica_war_msg(cs_galactica_mov_stop).

cs_galactica_talk(#cs_galactica_talk{data=Data}) ->
	#role{roleName=Name,roleID=RoleID} = role_data:get_roleInfo(),
	Data2 = #p_galactica_talk{roleID=RoleID,roleName=Name,data=util:words_filter(Data)},
	send_galactica_war_msg({cs_galactica_talk, Data2}).

cs_galactica_get_talk(_) ->
	send_galactica_war_msg(cs_galactica_get_talk).

cs_galactica_get_rank(_) ->
	send_galactica_war_msg(cs_galactica_get_rank).

cs_galactica_replay(#cs_galactica_replay{replayUID=ReplayUID}) ->
		RoleID = role_data:get_roleID(),
	case ets:lookup(?ETS_GALACTICA_ROLE_WAR,{match_info,RoleID}) of
		[] ->
			?sendself(#sc_galactica_war_base_info{result=2,attackerPos=#p_galactica_pos{x=0,y=0},defenderPos=#p_galactica_pos{x=0,y=0}
												 ,attackerGas=0,defenderGas=0});
		[{_, {_WarID, MatchServerID,_Sec}}] ->
			galactica_replay_server:get_replay(ReplayUID, MatchServerID,RoleID)
	end.

cs_galactica_reborn(_) ->
	NeedMoney = data_galactica:get(reborn_money),
	Role = role_data:get_roleInfo(),
	case role_lib:check_money(Role,gold,  NeedMoney) of
		true ->
			role_lib:deduct_gold_f(Role, NeedMoney, ?MONEY_DEC_TYPE_GALACTICA_REBORN, 0, ""),
			send_galactica_war_msg(cs_galactica_reborn),
			?sendself(#sc_galactica_reborn{result=1});
		_ ->
			?sendself(#sc_galactica_reborn{result=2})
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 队伍报名判断
check_team(RoleID, TeamId) ->
    case ets:lookup(ets_team_list, TeamId) of
        [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
            Len = erlang:length(TeamMember),
            case galactica_server:is_team_sign(TeamId, Len) of
                false ->
                    case galactica_server:is_team_in_war(TeamMember) of
                        true ->
                            {false, 6};
                        _ ->
                            LvLimit = data_galactica:get(lv_limit),
                            {Pass, TLv} =
                                lists:foldl(fun(#p_team_member_info{roleID=MID}, {PassAcc, TLvAcc}) ->
                                              #rolePublic{level=MLv} = role_lib:get_rolePublic(MID),
                                              {PassAcc andalso MLv >= LvLimit, TLvAcc + MLv} 
                                            end, {true, 0}, TeamMember),
                            case Pass of 
                                true ->
                                    case lists:all(fun(#p_team_member_info{roleID=MID}) ->
                                                        #ga_times{has_buy=MHasBuy, free_left=MFreeLefts} = galactica_server:get_times(MID),
                                                        MHasBuy + MFreeLefts > 0
                                                    end, TeamMember) of
                                        false ->
                                            {false, 5};
                                        _ ->
                                            {ok, TeamId, TeamMember, erlang:trunc(TLv / Len)} 
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
                    case galactica_server:is_role_sign(RoleID) of
                        false ->
                            case galactica_server:is_role_in_war(RoleID) of
                                true ->
                                    {false, 6};
                                _ ->
                                    Level = RoleInfo#role.level,
                                    case Level < data_galactica:get(lv_limit) of
                                        true ->
                                            {false, 7};
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
    {RankMinLevel,_} = data_relic:get({fight_level,LevelRank}),
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
    ?INFO("check_relic_sign ~w ~w",[IsTeamMember,MemberList]),
    if
        IsTeamMember =:= true ->
            {fail,2}; %组队了且没权限
        RoleInfo#role.level < NeedLevel ->
            {fail,3}; %等级不足
        RoleInfo#role.vipLevel < NeedVip ->
            {fail,4}; %Vip不足
        RoleInfo#role.level < RankMinLevel ->
            {fail,3}; %等级不足
        true -> %ok不在队伍中
            {ok,MemberList}
    end.    

%% 判断能否购买
check_buy(RoleInfo, #ga_times{buy_left=BuyLeft,has_buy=HasBuy} = CarlosTimes) ->
    {BuyTimeLMT, TimeCost} = data_galactica:get(buy_limit),
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
                    NewCarlosTimes = CarlosTimes#ga_times{buy_left = BuyLeft - 1,has_buy = HasBuy+1},
                    {0, NewCarlosTimes, Cost, NewHasBuyTimes, BuyLeft - 1}
            end
    end.

send_client({send_client, {route,2}}) ->
	Msg = #sc_galactica_war_base_info{result=2,attackerPos=#p_galactica_pos{x=0,y=0}
									 ,defenderPos=#p_galactica_pos{x=0,y=0},attackerGas=0,defenderGas=0},
	?sendself(Msg);
send_client({send_client, Msg}) ->
	?sendself(Msg).

send_galactica_war_msg(Msg) ->
	RoleID = role_data:get_roleID(),
	case ets:lookup(?ETS_GALACTICA_ROLE_WAR,{match_info,RoleID}) of
		[] ->
			?sendself(#sc_galactica_war_base_info{result=2,attackerPos=#p_galactica_pos{x=0,y=0}
												 ,defenderPos=#p_galactica_pos{x=0,y=0}
												 ,attackerGas=0,defenderGas=0});
		[{_, {WarID, MatchServerID,_Sec}}] ->
			galactica_router:send_galactica({WarID,MatchServerID},Msg,RoleID)
	end.

update_role_galactica_time()->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    #ga_times{has_buy=HasBuy, free_left=FreeLefts} = galactica_server:get_times(RoleID),
    ?sendself(#sc_galactica_times_update{times=HasBuy+FreeLefts}).


