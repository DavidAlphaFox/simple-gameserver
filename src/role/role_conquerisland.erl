-module(role_conquerisland).

-compile(export_all).
-include("def_role.hrl").
-include("def_carlos.hrl").
-include("def_reward.hrl").

%报名
cs_conquerisland_sign(_) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    IsFirst = role_data:check_plane_ai_flag_type(?ai_flag_conquerisland),
    #times{has_buy=BuyTimes, free_left=FreeLefts} = Time = conquerisland_server:get_times(RoleID),
    Left = BuyTimes + FreeLefts,
    ?INFO("conquerisland time is ~w ~n",[Time]),
    case check_sign(RoleInfo, RoleID, Left) of
        {ok, TeamId, TeamMember, Level} when IsFirst =:= true andalso TeamId =:= -1 ->
            %% 此分之说明是该玩家第一次进入卡洛斯
            RobotMemberIDList = util:random_list(db_sql:get_roleIDList_Robot(), (2 * data_conquerisland:get(match_need))-1),
            RobotTeamMember = [#p_team_member_info{roleID=RID}||RID<-[RoleID|RobotMemberIDList]],
            role_data:save_plane_ai_flag_type(?ai_flag_conquerisland),
            conquerisland_server:send_to_me({sign, RoleID, 0, RobotTeamMember, Level});
        {ok, TeamId, TeamMember, Level} ->
            if
                IsFirst =:= true -> %% 如果玩家第一次是组队的，那么就丧失了第一次对战AI的机会
                    role_data:save_plane_ai_flag_type(?ai_flag_conquerisland);
                true ->
                    ignore
            end,
            conquerisland_server:send_to_me({sign, RoleID, TeamId, TeamMember, Level});
        {false, Result} ->
            ?sendself(#sc_conquerisland_sign{result=Result, times=Left}) 
    end.

%%购买
cs_conquerisland_buy(_) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    Times = conquerisland_server:get_times(RoleID),
    case check_buy(RoleInfo, Times) of
        {Result, NewTimes, Cost, CurTime, Left} ->
            role_lib:deduct_gold_f(RoleInfo, Cost, ?MONEY_DEC_TYPE_CONQUERISLAND_BUY, 0, ""),
            conquerisland_server:set_times(RoleID, NewTimes),
            behavior_carlos:log([RoleID], ?carlos_type_conquerisland, ?carlos_op_time_buy,conquerisland_server:times2logextra(NewTimes)),
            update_role_time();
        {Result, CurTime, Left} ->
            ignore
    end,
    %%?ERR("购买结果:~p,当前购买次数:~p,剩余购买次数:~p", [Result, CurTime, Left]),
    ?sendself(#sc_conquerisland_buy{result=Result, cur_times=CurTime, times=Left}).

%%获取数据
cs_conquerisland_info(_) ->
    #role{roleID=RoleID,plane_level=PlaneLevel} = role_data:get_roleInfo(),
    #times{has_buy=BuyTimes, buy_left=BuyLeft, free_left=FreeLefts} = conquerisland_server:get_times(RoleID),
    {BuyTimeLMT, TimeCost} = data_conquerisland:get(buy_limit),
	{AP,PT}=afk_record_server:get_afk_punish(RoleID,?AFK_TYPE_CONQUERISLAND),
    ?sendself(#sc_conquerisland_info{times=BuyTimes + FreeLefts, cur_times=BuyTimeLMT - BuyLeft
							 ,buy_left=BuyLeft, golds=TimeCost,plane_level=PlaneLevel,winGas=0
                             ,afk_punish = AP, punish_timestamp = PT}),
    #plane_use_info{planes=PlaneInfoList,use=Use}=role_data:get_plane_use_info(),
    ?sendself(#sc_carlos_plane_use_info{planeInfo=role_carlos:plane_info2p_plane_info(PlaneInfoList),display=plane(Use)}).

%%取消匹配
cs_conquerisland_unrequest(_) ->
    #role{roleID=RoleID, teamId=TeamId} = role_data:get_roleInfo(),
    case TeamId =:= -1 of
        true ->
            case conquerisland_server:is_role_sign(RoleID) of
                false ->
                    ?sendself(#sc_conquerisland_unrequest{result=1});
                {true, ID} ->
                    erlang:send(conquerisland_server, {unrequest, ID})
            end;
        _ ->
            case ets:lookup(ets_team_list, TeamId) of
                [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
                    case conquerisland_server:is_team_sign(TeamId, erlang:length(TeamMember)) of
                        false ->
                            ?sendself(#sc_conquerisland_unrequest{result=1});
                        {true, ID} ->
                            erlang:send(conquerisland_server, {unrequest, ID})
                    end;
                _ ->
                    ?sendself(#sc_conquerisland_unrequest{result=2}) 
            end
    end.

update_role_time()->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    #times{has_buy=HasBuy, free_left=FreeLefts} = conquerisland_server:get_times(RoleID),
    ?sendself(#sc_conquerisland_times_update{times=HasBuy+FreeLefts}).

%%处理战斗过程中相关协议
cs_conquerisland_war_base_info(Msg)->
    send_war_msg(Msg).
cs_conquerisland_role_dtl(Msg)->
    send_war_msg(Msg).
cs_conquerisland_mov(Msg)->
    send_war_msg(Msg).
cs_conquerisland_stop(Msg)->
    send_war_msg(Msg).
cs_conquerisland_centre_dtl(Msg)->
    send_war_msg(Msg).
cs_conquerisland_attack(Msg)->
    send_war_msg(Msg).
cs_conquerisland_centre_occupy(Msg)->
    send_war_msg(Msg).
cs_conquerisland_reborn(Msg)->
    NeedMoney = data_conquerisland:get(reborn_money),
    Role = role_data:get_roleInfo(),
    case role_lib:check_money(Role,gold,  NeedMoney) of
        true ->
            role_lib:deduct_gold_f(Role, NeedMoney, ?MONEY_DEC_TYPE_CONQUERISLAND_REBORN, 0, ""),
            send_war_msg(Msg),
            ?sendself(#sc_conquerisland_reborn{result=1});
        _ ->
            ?sendself(#sc_conquerisland_reborn{result=2})
    end.
cs_conquerisland_replay(#cs_conquerisland_replay{replayUID=ReplayUID})->
    RoleID = role_data:get_roleID(),
    case ets:lookup(?ETS_CONQUERISLAND_WAR_INFO,{match_info,RoleID}) of
        [] ->
            ?sendself(#sc_carlos_war_base_info{result=2,attackerPos=#p_carlos_pos{x=0,y=0},defenderPos=#p_carlos_pos{x=0,y=0}});
        [{_, {_WarID, MatchServerID,_Sec}}] ->
            conquerisland_replay_server:get_replay(ReplayUID, MatchServerID,RoleID)
    end.
cs_conquerisland_self(Msg)->
    send_war_msg(Msg).

cs_conquerisland_rank(Msg)->
    send_war_msg(Msg).

cs_conquerisland_talk(Msg)->
    send_war_msg(Msg).
cs_conquerisland_get_talk(Msg)->
    send_war_msg(Msg).
%%接收来自conquerisland_router路由过来的信息
send_client({send_client, {route,2}}) ->
    Msg = #sc_conquerisland_war_base_info{result=2,attackerPos=#p_pos{x=0,y=0},defenderPos=#p_pos{x=0,y=0}},
    ?sendself(Msg);
send_client({send_client, Msg}) ->
    ?sendself(Msg).
%%====================================================================================================%%
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
                    case conquerisland_server:is_role_sign(RoleID) of
                        false ->
                            case conquerisland_server:is_role_in_war(RoleID) of
                                true ->
                                    {false, 6};
                                _ ->
                                    Level = RoleInfo#role.level,
                                    case Level < data_conquerisland:get(lv_limit) of
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
%% 队伍报名判断
check_team(RoleID, TeamId) ->
    case ets:lookup(ets_team_list, TeamId) of
        [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
            Len = erlang:length(TeamMember),
            case conquerisland_server:is_team_sign(TeamId, Len) of
                false ->
                    case conquerisland_server:is_team_in_war(TeamMember) of
                        true ->
                            {false, 6};
                        _ ->
                            LvLimit = data_conquerisland:get(lv_limit),
                            {Pass, TLv} =
                                lists:foldl(fun(#p_team_member_info{roleID=MID}, {PassAcc, TLvAcc}) ->
                                              #rolePublic{level=MLv} = role_lib:get_rolePublic(MID),
                                              {PassAcc andalso MLv >= LvLimit, TLvAcc + MLv} 
                                            end, {true, 0}, TeamMember),
                            case Pass of 
                                true ->
                                    case lists:all(fun(#p_team_member_info{roleID=MID}) ->
                                                        #times{has_buy=MHasBuy, free_left=MFreeLefts} = conquerisland_server:get_times(MID),
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

%% 判断能否购买
check_buy(RoleInfo, #times{buy_left=BuyLeft,has_buy=HasBuy} = Times) ->
    {BuyTimeLMT, TimeCost} = data_conquerisland:get(buy_limit),
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
                    NewTimes = Times#times{buy_left = BuyLeft - 1,has_buy = HasBuy+1},
                    {0, NewTimes, Cost, NewHasBuyTimes, BuyLeft - 1}
            end
    end.

plane(Tag)->
	case data_carlos:get({planeTag,Tag}) of
		X when is_integer(X) -> X;
		_ -> 0
	end.

send_war_msg(Msg) ->
    RoleID = role_data:get_roleID(),
    case ets:lookup(?ETS_CONQUERISLAND_WAR_INFO,{match_info,RoleID}) of
        [] ->
            ?sendself(#sc_conquerisland_war_base_info{result=2,attackerPos=#p_pos{x=0,y=0},defenderPos=#p_pos{x=0,y=0}});
        [{_, {WarID, MatchServerID,_Sec}}] ->
            conquerisland_router:send_msg_to_war({WarID,MatchServerID},Msg,RoleID)
    end.