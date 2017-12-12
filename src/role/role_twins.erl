-module(role_twins).

-compile(export_all).
-include("def_role.hrl").
-include("def_carlos.hrl").


%%报名
cs_twins_sign(#cs_twins_sign{level_rank=LevelRank}) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    #tw_times{has_buy=BuyTimes, free_left=FreeLefts} = twins_server:get_times(RoleID),
    Left = BuyTimes + FreeLefts,
    case check_sign(RoleInfo, RoleID, Left,LevelRank) of
        {ok, TeamId, TeamMember, _Level} ->
            twins_server:send_twins_war_msg({sign, RoleID, TeamId, TeamMember, LevelRank});
        {false, Result} ->
            ?sendself(#sc_twins_sign{result=Result, times=Left}) 
    end.

%%购买
cs_twins_buy(_) ->
    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
    CarlosTimes = twins_server:get_times(RoleID),
    case check_buy(RoleInfo, CarlosTimes) of
        {Result, NewCarlosTimes, Cost, CurTime, Left} ->
            role_lib:deduct_gold_f(RoleInfo, Cost, ?MONEY_DEC_TYPE_TWINS_BUY, 0, ""),
            twins_server:set_times(RoleID, NewCarlosTimes);
        {Result, CurTime, Left} ->
            ignore
    end,
    %%?ERR("购买结果:~p,当前购买次数:~p,剩余购买次数:~p", [Result, CurTime, Left]),
    ?sendself(#sc_twins_buy{result=Result, cur_times=CurTime, times=Left}).

%%获取数据
cs_twins_info(_) ->
    #role{roleID=RoleID,plane_level=PlaneLevel} = role_data:get_roleInfo(),
    #tw_times{has_buy=BuyTimes, buy_left=BuyLeft, free_left=FreeLefts,last_war_data=WD
			 ,twins_box_data_list=BoxList,rank_data=RankData} = twins_server:get_times(RoleID),
	{BoxRank,_,_}=WD,
	Display = twins_match:calc_type(),
    {BuyTimeLMT, TimeCost} = data_twins:get(buy_limit),
	{AP,PT}=afk_record_server:get_afk_punish(RoleID,?AFK_TYPE_TWINS),
    ?sendself(#sc_twins_info{times=BuyTimes + FreeLefts, cur_times=BuyTimeLMT - BuyLeft,box_rank=BoxRank,rank=RankData
						    ,buy_left=BuyLeft, golds=TimeCost,plane_level=PlaneLevel,display=Display,box=BoxList
                            ,afk_punish = AP, punish_timestamp = PT}).

%%取消匹配
cs_twins_unrequest(_) ->
    #role{roleID=RoleID, teamId=TeamId} = role_data:get_roleInfo(),
    case TeamId =:= -1 of
        true ->
            case twins_server:is_role_sign(RoleID) of
                false ->
                    ?sendself(#sc_twins_unrequest{result=1});
                {true, ID} ->
                    erlang:send(twins_server, {unrequest, ID})
            end;
        _ ->
            case ets:lookup(ets_team_list, TeamId) of
                [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
                    case twins_server:is_team_sign(TeamId, erlang:length(TeamMember)) of
                        false ->
                            ?sendself(#sc_twins_unrequest{result=1});
                        {true, ID} ->
                            erlang:send(twins_server, {unrequest, ID})
                    end;
                _ ->
                    ?sendself(#sc_twins_unrequest{result=2}) 
            end
    end.

cs_twins_war_base_info(_)->
    update_role_twins_time(),
	send_twins_war_msg(cs_twins_war_base_info).

cs_twins_self(_) ->
	send_twins_war_msg(cs_twins_self).

cs_twins_mov(#cs_twins_mov{mineID=MineID}) ->
	send_twins_war_msg({cs_twins_mov, MineID}).

cs_twins_role_dtl(#cs_twins_role_dtl{roleID=RoleID,serverID=ServerID}) ->
	send_twins_war_msg({cs_twins_role_dtl, RoleID,ServerID}).

cs_twins_attack(#cs_twins_attack{mineID=MineID}) ->
	case check_attack() of
		true ->
			send_twins_war_msg({cs_twins_attack, MineID});
		_ ->
			ignore
	end.

check_attack()->
	Now = util:now(),
	case get(last_twins_attack) of
		Now ->
			false;
		_ ->
			put(last_twins_attack, Now),
			true
	end.

cs_twins_mov_stop(_) ->
	send_twins_war_msg(cs_twins_mov_stop).

cs_twins_talk(#cs_twins_talk{data=Data}) ->
	#role{roleName=Name,roleID=RoleID} = role_data:get_roleInfo(),
	Data2 = #p_twins_talk{roleID=RoleID,roleName=Name,data=util:words_filter(Data)},
	send_twins_war_msg({cs_twins_talk, Data2}).

cs_twins_get_talk(_) ->
	send_twins_war_msg(cs_twins_get_talk).

cs_twins_get_rank(_) ->
	send_twins_war_msg(cs_twins_get_rank).

cs_twins_open_reward_box(#cs_twins_open_reward_box{id=Index})->
	#role{roleID=RoleID,isMale=IsMale} = RoleInfo = role_data:get_roleInfo(),
	#tw_times{twins_box_data_list=BL,last_war_data={LevelRank,RewardRank,Type}}
				 =TwinsRoleData = twins_server:get_times(RoleID),
	case lists:keytake(Index,#p_twins_box_info.id,BL) of
		false ->
			?sendself(#sc_twins_open_reward_box{result=4,reward=[]});
		{value,Box,_} ->
			#p_twins_box_info{status=Status}=Box,
			case Status of
				1 ->
					?sendself(#sc_twins_open_reward_box{result=2,reward=[]});
				0 ->
					N = data_twins:get({box_reward,LevelRank,RewardRank}),
					Reward = merge_reward(#sell_reward{},get_trainer_drop(Type,LevelRank,IsMale,N)),
					RoleInfo = role_data:get_roleInfo(),
					do_twins_open_box(RoleInfo,Index,Reward,Box,BL,TwinsRoleData)
			end
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_trainer_drop(Type,_LevelRank,IsMale,{N1,N2,N3,N4}) ->
	%R2 = data_twins:get({box_drop,Type}),
	R2 = util:random_one_from_list(data_twins:get(box_drop)),
	R1 = data_twins:get({box_drop,Type,IsMale}),
	StReward = [#new_item{itemTypeID=20043,itemNum=N3,itemLevel=1,itemRank=0}
			   ,#new_item{itemTypeID=20044,itemNum=N4,itemLevel=1,itemRank=0}],
	StReward++[R2#new_item{itemNum=N2}]++[I1#new_item{itemNum=N1}||I1<-R1].
merge_reward(Reward=#sell_reward{item=Item},Reward2) ->
	Reward3 = [I||I<-Reward2,I#new_item.itemNum > 0 ],
	Reward#sell_reward{item=Item++Reward3}.

%% 队伍报名判断
check_team(RoleID, TeamId,LevelLimit,FightPowerLimit) ->
    case ets:lookup(ets_team_list, TeamId) of
        [#team_info{teamleader_roleid=RoleID,team_member=TeamMember}] ->
            Len = erlang:length(TeamMember),
            case twins_server:is_team_sign(TeamId, Len) of
                false ->
                    case twins_server:is_team_in_war(TeamMember) of
                        true ->
                            {false, 6};
                        _ ->
                            {Pass, TLv} =
                                lists:foldl(fun(#p_team_member_info{roleID=MID}, {PassAcc, TLvAcc}) ->
                                              #rolePublic{level=MLv,fightPower=FightPower} = role_lib:get_rolePublic(MID),
                                              {PassAcc andalso MLv >= LevelLimit andalso FightPower >= FightPowerLimit, TLvAcc + MLv} 
                                            end, {true, 0}, TeamMember),
                            case Pass of 
                                true ->
                                    case lists:all(fun(#p_team_member_info{roleID=MID}) ->
                                                        #tw_times{has_buy=MHasBuy, free_left=MFreeLefts} = twins_server:get_times(MID),
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

check_sign(RoleInfo, RoleID, Left,LevelRank) ->
    case Left > 0 of
        false ->
            %fix 这里以前返回的4，是否应该返回5
            {false, 5};
        _ ->
            TeamId = RoleInfo#role.teamId,
			{LevelLimit,FightPowerLimit,_} = data_twins:get({fight_level,LevelRank}),
            case TeamId =/= -1 of
                true ->
                    check_team(RoleID, TeamId,LevelLimit,FightPowerLimit);
                _ ->
                    case twins_server:is_role_sign(RoleID) of
                        false ->
                            case twins_server:is_role_in_war(RoleID) of
                                true ->
                                    {false, 6};
                                _ ->
									#role{level=Level,fightPower=FightPower} = RoleInfo,
                                    case Level < LevelLimit orelse FightPower < FightPowerLimit  of
                                        true ->
                                            {false, 7};
                                        _ ->
											{ok, TeamId, [#p_team_member_info{roleID=RoleID}],Level}
                                    end
                            end;
                        _ ->
                            {false, 4} 
                    end 
            end
    end. 

%% 判断能否购买
check_buy(RoleInfo, #tw_times{buy_left=BuyLeft,has_buy=HasBuy} = CarlosTimes) ->
    {BuyTimeLMT, TimeCost} = data_twins:get(buy_limit),
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
                    NewCarlosTimes = CarlosTimes#tw_times{buy_left = BuyLeft - 1,has_buy = HasBuy+1},
                    {0, NewCarlosTimes, Cost, NewHasBuyTimes, BuyLeft - 1}
            end
    end.

send_client({send_client, {route,2}}) ->
	Msg = #sc_twins_war_base_info{result=2,rebornPos=#p_twins_pos{x=0,y=0}},
	?sendself(Msg);
send_client({send_client, Msg}) ->
	?sendself(Msg).

send_twins_war_msg(Msg) ->
	RoleID = role_data:get_roleID(),
	case ets:lookup(?ETS_TWINS_ROLE_WAR,{match_info,RoleID}) of
		[] ->
			?sendself(#sc_twins_war_base_info{result=2,rebornPos=#p_twins_pos{x=0,y=0}});
		[{_, {WarID, MatchServerID,_Sec}}] ->
			twins_router:send_twins({WarID,MatchServerID},Msg,RoleID)
	end.

update_role_twins_time()->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    #tw_times{has_buy=HasBuy, free_left=FreeLefts} = twins_server:get_times(RoleID),
    ?sendself(#sc_twins_times_update{times=HasBuy+FreeLefts}).

do_twins_open_box(RoleInfo,Index,Reward,Box,Bl,TwinsRoleData) ->
%% 	RoleInfo2 = 
%% 	if Cost > 0 ->
%% 		   role_lib:deduct_gold_f(RoleInfo, Cost, ?MONEY_DEC_TYPE_TWINS_OPEN_BOX, Index, ""
%% 								  ,role_reward:transform2normal_reward(Reward));
%% 	   true ->
%% 		   RoleInfo
%% 	end,
	role_reward:handle_sell_reward_f(RoleInfo,Reward,?MONEY_ADD_TYPE_TWINS_OPEN_BOX,Index,""),
	Bl2 = lists:keystore(Index,#p_twins_box_info.id,Bl,Box#p_twins_box_info{status=1}),
	TwinsRoleData2 = TwinsRoleData#tw_times{twins_box_data_list=Bl2},
	twins_server:set_times(RoleInfo#role.roleID,TwinsRoleData2),
	?sendself(#sc_twins_open_reward_box{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward)]}).

