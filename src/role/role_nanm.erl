%% @author caohongyang
%% @doc 战南蛮 玩家处理模块
%% Created 2013-5-6


-module(role_nanm).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
%% API functions
-export([]).

%% Internal functions 
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc 请求擂鼓
cs_nanm_buff(#cs_nanm_buff{type = Type}) ->
	case check_add_buff(Type) of
		{true, Role, NeedType, NeedNum, BuffNum, AddReputation} ->
			do_add_buff(Type, Role, NeedType, NeedNum, BuffNum, AddReputation);
		{false, Reason} ->
			?sendself(#sc_nanm_buff{result=Reason,type=Type})
			end.

%% 请求战斗
cs_nanm_fight(#cs_nanm_fight{}) ->
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
	TrSpecial = role_data:get_trSpecial(),
	ASkinInfo = role_skin:get_skin_info(),
	FighterList = role_data:get_fighter_list(),
	LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-FighterList],
    FighterList2 = role_xbattle:get_add_buff(role_data:get_xbattle_data(),FighterList),
	nanm_server:fight(role_data:get_roleID(),FighterList2,RoleLieuAdd,TalentList,TrSpecial,ASkinInfo,LegendAddList).


%% 请求复活
cs_nanm_reborn(_) ->
    #role{roleID=RoleID} = Role = role_data:get_roleInfo(),
    NeedGold = data_nanm:get(recover_gold),
    TalentList = role_talent:get_active_talent_list(),
    case role_lib:check_money(Role, gold, NeedGold) of
        true ->
            case nanm_server:is_state_begin() of
                true ->
                    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_NANM_REBORN, 0, ""),
                    #role{familyID = FamilyID} = role_data:get_roleInfo(),
                    RoleLieuAdd = role_data:get_lieu_add_attr(),
					TrSpecial=role_data:get_trSpecial(),
					ASkinInfo = role_skin:get_skin_info(),
					FighterList = role_data:get_fighter_list(),
					LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-FighterList],
                        FighterList2 = role_xbattle:get_add_buff(role_data:get_xbattle_data(),FighterList),
                    nanm_server:reborn(RoleID, FighterList2, RoleLieuAdd,TalentList,TrSpecial,ASkinInfo,LegendAddList);
                false ->
                    ?sendself(#sc_nanm_reborn{result=3})
            end;
        false ->
            ?sendself(#sc_nanm_reborn{result=2})
    end.
    
%%请求世界BOSS奖励
cs_nanm_reward(_)->
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	nanm_server:get_reward(RoleID).

%% 请求设置离线参与战南蛮
cs_nanm_offline_play(#cs_nanm_offline_play{openFlag=OpenFlag}) ->
	#role{roleID=RoleID, vipLevel=VipLevel} = role_data:get_roleInfo(),
	if OpenFlag =:= true ->
		   NeedVipLevel = data_nanm:get(offline_play_need_vipLevel),
		   if VipLevel >= NeedVipLevel ->
				  DeductGold = data_nanm:get(offline_play_cost_gold),
				  Role = role_data:get_roleInfo(),
				  case role_lib:check_money(Role, gold, DeductGold) of
					  true ->
						  role_lib:deduct_gold_f(Role, DeductGold, ?MONEY_DEC_TYPE_NANM_OFFLINE, 0, ""),
						  nanm_server:set_offlinePlayFlag(RoleID,OpenFlag);
					  false ->
						  ?sendself(#sc_nanm_offline_play{result=4, newOpenFlag=false})
				  end;
			  true ->
				  ?sendself(#sc_nanm_offline_play{result=3, newOpenFlag=false})
		   end;
	   true ->
		   ?sendself(#sc_nanm_offline_play{result=2, newOpenFlag=true})
	end.

do_add_buff(Type, Role, NeedType, NeedNum, BuffNum, AddReputation) ->
	#role{roleID=RoleID} = Role,
	ets:insert(?ETS_NANM_BUFF_FLAG, {RoleID, true}),
	Role2 = role_lib:deduct_money_f(Role, NeedType, NeedNum, ?MONEY_DEC_TYPE_NANM_ADD_BUFF, 0, ""),
	_Role3 = role_lib:add_reputation_f(Role2, AddReputation, ?MONEY_ADD_TYPE_NANM_ADD_BUFF, 0, ""),
	nanm_server:add_buff(RoleID, BuffNum),
	?sendself(#sc_nanm_buff{result=1,type=Type}).
	
check_add_buff(Type) ->
	{BuffNum, NeedType, NeedNum, AddReputation} = data_nanm:get({buff,Type}),
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	case role_lib:check_money(Role, NeedType, NeedNum) of
		true ->
			case ets:lookup(?ETS_NANM_BUFF_FLAG, RoleID) of
				[] ->
					{true, Role, NeedType, NeedNum, BuffNum, AddReputation};
				_ ->
					{false, 3}
			end;				
		false ->
			{false, 2}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


