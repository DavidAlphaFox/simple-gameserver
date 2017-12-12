-module(role_familycross).

-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").

%%=================================================阿努比斯赛季排行榜相关================================================================
cs_familycross_seasoninfo(#cs_familycross_seasoninfo{})->
    case anubis_server:get_anubis_seasonid() of
        {0,[0]}->
            ?sendself(#sc_familycross_seasoninfo{result=0,currentseasonid=0});
        {CurrentSeasonID,ExistSeasonIDList}->
            ?sendself(#sc_familycross_seasoninfo{result=1,currentseasonid=CurrentSeasonID,existseasonid=ExistSeasonIDList})
    end.

cs_familycross_season_rank(#cs_familycross_season_rank{seasonid=RequestSeasonID,beginpos=Begin})->
        OurFamily = [#p_anubis_family{familyid=0,serverid=0,score=0,rank=0}],
        case check_get_anubis_season_rank(RequestSeasonID,Begin) of
            {false,Reason}->
                ?sendself(#sc_familycross_season_rank{result=Reason,seasonid=RequestSeasonID,beginpos=Begin,isend=0,ourfamily=OurFamily});
            {true,FamilyList,IsEnd}->
            	case Begin=:=1 of
            		true->
                		#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
                		case family_misc:router_to_family_process(FamilyID, {cs_familycross_season_rank,RoleID,transform_familyplayer2panubis_family(FamilyList,[]),IsEnd,RequestSeasonID,Begin}) of
                    		ok->
                        		ignore;
                    		_ ->
                        		?ERR("send msg to family:~W failed~n",[FamilyID]),
                        		?sendself(#sc_familycross_season_rank{result=2,seasonid=RequestSeasonID,beginpos=Begin,isend=IsEnd,familylist=transform_familyplayer2panubis_family(FamilyList,[]),ourfamily=OurFamily})
                       	end;
                    false->
                    	?sendself(#sc_familycross_season_rank{result=1,seasonid=RequestSeasonID,beginpos=Begin,isend=IsEnd,familylist=transform_familyplayer2panubis_family(FamilyList,[]),ourfamily=[]})
                end
        end.

cs_familycross_family_rank(#cs_familycross_family_rank{seasonid=SeasonID})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID >0 of
		true->
			case family_misc:router_to_family_process(FamilyID,{cs_familycross_family_rank,RoleID,SeasonID}) of
				ok->
					ignore;
				_->
                    ?ERR("send msg to family:~W failed~n",[FamilyID]),
                    ?sendself(#sc_familycross_family_rank{result=3,memberlist=[]})
            end;
        false->
        	?sendself(#sc_familycross_family_rank{result=2,memberlist=[]})
    end.
cs_familycross_info(_) ->
	case check_familycross_open() of
		true ->
			#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
			case FamilyID>0 of
				true -> family_misc:router_to_family_process(FamilyID,{cs_familycross_info,RoleID});
				_ -> ?sendself(#sc_familycross_info{result=2,info=[]})
			end;
		_ -> ?sendself(#sc_familycross_info{result=3,info=[]})
	end.
	
cs_familycross_sign(_) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	if FamilyID > 0 -> family_misc:router_to_family_process(FamilyID,{cs_familycross_sign,RoleID});
	   true -> ?sendself(#sc_familycross_sign{result=2})
	end.

cs_familycross_displayer(#cs_familycross_displayer{cars=Cars}) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	if FamilyID > 0 -> family_misc:router_to_family_process(FamilyID,{cs_familycross_displayer,RoleID,Cars});
	   true -> ?sendself(#sc_familycross_displayer{result=3})
	end.

cs_familycross_displayer_info(_)->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	if FamilyID > 0 -> family_misc:router_to_family_process(FamilyID,{cs_familycross_displayer_info, RoleID});
	   true -> ?sendself(#sc_familycross_displayer_info{cars=[]})
	end.

cs_familycross_player_fly(_)->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	if FamilyID > 0 -> family_misc:router_to_family_process(FamilyID,{cs_familycorss_player_fly, RoleID});
	   true -> ?sendself(#sc_familycross_player_fly{fly=[]})
	end.

cs_familycross_enermy_info(_)->
	#role{roleID=RoleID,familyID=FamilyID}=role_data:get_roleInfo(),
	if FamilyID > 0 -> family_misc:router_to_family_process(FamilyID,{cs_familycross_enermy_info,RoleID});
	   true -> ?sendself(#sc_familycross_enermy_info{serverID1=0,serverID2=0})
	end.


cs_familycross_war_info(Msg) -> 
	case send_familycross_war_msg(Msg) of
		ignore -> ?sendself(#sc_familycross_war_info{startTime=0,interval=0,needGas=0});
		_ -> ignore
	end.
 
cs_familycross_drive_car(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_mov(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_attack(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_city_dtl(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_site_dtl(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_fly_dtl(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_self_info(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_be_driver(Msg) -> send_familycross_war_msg(Msg).
cs_familycross_be_driver2(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_drive_car_stop(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_mov_stop(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_mov_back(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_reborn(#cs_familycross_reborn{}=Msg) ->
	Cost= data_family_cross:get(reborn_cost),
	case role_lib:check_money(role_data:get_roleInfo(),gold,Cost) of
		true ->	send_familycross_war_msg(Msg);
		_ -> ?sendself(#sc_familycross_reborn{result=3})
	end;
cs_familycross_reborn({cs_familycross_reborn, _RoleID}) ->
	Cost = data_family_cross:get(reborn_cost),
	role_lib:deduct_money_f(role_data:get_roleInfo(), gold, Cost, ?MONEY_DEC_TYPE_FAMILYCROSS_REBORN, 0, "").

cs_familycross_own_site(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_battle_get_rank(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_self_car(Msg) -> send_familycross_war_msg(Msg).

cs_familycross_replay(#cs_familycross_replay{replayUID=ReplayUID}) -> 
		RoleID = role_data:get_roleID(),
	case ets:lookup(?ETS_FAMILYCROSS_ROLE_WAR,{match_info,RoleID}) of
		[] ->
			?sendself(#sc_carlos_war_base_info{result=2,attackerPos=#p_carlos_pos{x=0,y=0},defenderPos=#p_carlos_pos{x=0,y=0}});
		[{_, {_WarID, MatchServerID,_Sec}}] ->
			family_cross_replay_server:get_replay(ReplayUID, MatchServerID,RoleID)
	end.



	


%% ====================================================================
%% Internal functions
%% ====================================================================

check_familycross_open()->
	case data_family_cross:get(family_cross_open) of
		"1" ->
			true;
		_ ->
			false
	end.

send_familycross_war_msg(Msg) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	if FamilyID > 0 ->
		   case ets:lookup(?ETS_FAMILYCROSS_ROLE_WAR,{match_info,RoleID}) of
			   [] -> ignore;
			   [{_, {WarID, MatchServerID,_Sec}}] -> family_cross_fight_router:send_war({WarID,MatchServerID},Msg,RoleID)
		   end;
	   true -> ignore
	end.

check_get_anubis_season_rank(RequestSeasonID,Begin)->
    case anubis_server:get_anubis_rank(RequestSeasonID) of
        []->
            {false,2};
        List ->
            Length = length(List),
            case Length < Begin of
                true->
                    {false,3};
                false->
                    MaxLength = data_anubis:get(rank_max_send_length),
                    NeedSendList = lists:sublist(List,Begin,MaxLength),
                    _IsEnd = case length(NeedSendList) < MaxLength of
                        true->
                            {true,NeedSendList,0};
                        false->
                            case MaxLength =:= length(List) of
                                true->
                                    {true,NeedSendList,1};
                                false->
                                    {true,NeedSendList,0}
                            end
                    end
            end
    end.

transform_familyplayer2panubis_family([],Acc)->
    lists:reverse(Acc);
transform_familyplayer2panubis_family([H|T],Acc)->
    #family_player{familyid=FamilyID,familyname=FamilyName,familyserver=FamilyServer,familyleader=FamilyLeader,familyrank=FamilyRank,familyscore=FamilyScore} = H,
    NewAcc = [#p_anubis_family{familyid=FamilyID,familyname=FamilyName,serverid=FamilyServer,leadername=FamilyLeader,score=FamilyScore,rank=FamilyRank}|Acc],
    transform_familyplayer2panubis_family(T,NewAcc).

%%判断玩家是否有公会，如果有公会则更新对应赛季的数据
do_test_change_role_anubisinfo(RoleID,SeasonID,AddKillNum,AddResourcePoint)->
	#role{familyID=FamilyID} = role_data:get_roleInfo(),
	if
		FamilyID > 0 ->
			case family_misc:router_to_family_process(FamilyID,{test_change_role_anubisinfo,RoleID,SeasonID,AddKillNum,AddResourcePoint}) of
				ok->
					ok;
				_->
					?ERR("send test_change_role_anubisinfo to family_server:~w failed ~n",[FamilyID])
			end;
		true->
			ok
	end.