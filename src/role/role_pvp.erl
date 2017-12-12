%% @author caohongyang
%% @doc 玩家pvp
%% Created 2013-4-8


-module(role_pvp).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_pvp_fight(Record) ->
	PVPTimes = role_data:get_pvp_times(),
	RoleID = role_data:get_roleID(),
	case PVPTimes >= 1 of
		true ->
			erlang:send(pvp_server, {fight,RoleID, Record});
		false ->
			?sendself(#sc_pvp_fight{fightInfo=[],newRank=0,result=3,pvppoint=0,reputation=0,gold=0})
	end.

cs_pvp_free_fight(#cs_pvp_free_fight{roleID = TarRoleID})->
    Msg = case check_can_free_fight() of
        true ->
            {_, FightRecord0, _State} = role_lib:pvp(TarRoleID),    
            FighterList2 = role_data:get_FighterList_with_effect(role_data:get_equipts_on_ger(),TarRoleID,FightRecord0#sc_fight_request.fighterList),
            FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2}, 
            #sc_pvp_free_fight{result = 1,
                               fightInfo = FightRecord,
                               reward = []};
        false ->
            #sc_pvp_free_fight{result = 2,
                               fightInfo = #sc_fight_request{result=false,fighterList=[],actionList=[]},
                               reward = []}
    end,
    ?sendself(Msg).

cs_pvp_get_free_fight_level(_)->
    Msg = #sc_pvp_get_free_fight_level{need_level=data_common:get(free_fight_level)},
    ?sendself(Msg). 

get_pvp(RoleInfo, RoleFighterPower) ->
	#p_pvp{fightPower=RoleFighterPower,
		   isMale=(RoleInfo#role.isMale),
		   roleID=RoleInfo#role.roleID,
		   level=RoleInfo#role.level,
		   title=RoleInfo#role.title,
		   roleName=RoleInfo#role.roleName,
		   head=RoleInfo#role.head,
           vip=role_lib:cacl_vip_info(RoleInfo#role.vipLevel, RoleInfo#role.svipLevel)}.
		   

do_pvp_fight({do_pvp_fight, TarRoleID}) ->
	Now = util:now(),
	case role_lib:get_rolePublic(TarRoleID) of
		#rolePublic{lastLogoutTime=LLT} = RolePublic ->
			case LLT of
				0 ->
					TarRoleID2= TarRoleID,
					ignore;
				_ ->
					case Now - LLT > data_common:get(pvp_replace_duration) of
						true ->
							TarRoleID2=0,
							db_sql:update_role_fighters(RolePublic);
						_ ->
							TarRoleID2=TarRoleID,
							ignore
					end
			end,
			TarPVP = #p_pvp{
			fightPower=(role_lib:get_rolePublic(TarRoleID))#rolePublic.fightPower,
		   	isMale=RolePublic#rolePublic.isMale,
		   	roleID=RolePublic#rolePublic.roleID,
		   	level=RolePublic#rolePublic.level,
		   	title=RolePublic#rolePublic.title,
		   	roleName=RolePublic#rolePublic.roleName,
			head=RolePublic#rolePublic.head,
            vip=role_lib:cacl_vip_info(RolePublic#rolePublic.viplevel, RolePublic#rolePublic.svipLevel)};
		_ ->
			TarRoleID2=TarRoleID,
			TarPVP = []
	end,
	{Result, FightRecord0, _State} = role_lib:pvp(TarRoleID),    
    FighterList2 = role_data:get_FighterList_with_effect(role_data:get_equipts_on_ger(),TarRoleID,FightRecord0#sc_fight_request.fighterList),
    FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},    
	RoleInfo = role_data:get_roleInfo(),
	role_data:dec_pvp_times(),
	RoleFightPower = role_data:get_roleFightPower(),
	PVP = get_pvp(RoleInfo, RoleFightPower),
	%%刷新目标TarRole的信息

	#role{roleID=RoleID,roleName=RoleName,vipLevel=VipLevel,level=Level} = RoleInfo,
    role_box:do_mystery_box(pvp),
    DoubleValue = case activity_server:is_activity(?pvp_reward_double) of
                      true ->
                          2;
                      false ->
                          1
                  end,
	case Result of
		true ->
            AddPvpPoint = data_pvp_mystery:get(win_pvppoint) * DoubleValue ,
			AddRep = data_common:get(pvp_fight_win_add_rep) * DoubleValue,
			erlang:send(pvp_server, {fight_win, FightRecord, RoleID,TarRoleID, PVP, TarPVP,AddPvpPoint,AddRep,TarRoleID2});
		false ->
            AddPvpPoint = data_pvp_mystery:get(lose_pvppoint) * DoubleValue,
%% 			RewardView = pvp_reward_box(RoleInfo,VipLevel,Level,0),
			AddRep = data_common:get(pvp_fight_fail_add_rep) * DoubleValue,
			erlang:send(pvp_server, {fight_fail, FightRecord, RoleID,
									 RoleName, TarRoleID, PVP, TarPVP,AddPvpPoint,AddRep,TarRoleID2})
	end,
    ?INFO("AddPvpPoint ~w DoubleValue:~w",[AddPvpPoint,DoubleValue]),
    RoleInfo2 = role_lib:add_pvppoint_f(RoleInfo, AddPvpPoint,?MONEY_ADD_TYPE_PVP_FIGHT,0,""),
	role_lib:add_reputation_f(RoleInfo2, AddRep,?MONEY_ADD_TYPE_PVP_FIGHT,0,"").
	
pvp_reward_box(Role,VipLevel,Level,IsWin)->
	BoxID =
		case data_pvp:get({VipLevel,Level}) of
			{W,F}->
				case IsWin of
					1->
						W;
					_->
						F
				end;
			_->
				0
		end,
	case BoxID of
		0->
			[];
		_->
			MainGerTypeID = role_data:get_mainGerTypeID(),
			[R1|_] = data_box:get({BoxID, MainGerTypeID}),
			LogType = ?MONEY_ADD_TYPE_PVP_FIGHT,
			RewardList = [util:random_one_from_weigh_list(R1)],
			RewardView = role_reward:transform2p_reward_view(RewardList, []),
			role_reward:handle_sys_reward(Role, RewardList, LogType, IsWin, ""),
			RewardView
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================

%未来有必要的话，在这里加入针对对方是否是好友的检查
check_can_free_fight()->
    true.

