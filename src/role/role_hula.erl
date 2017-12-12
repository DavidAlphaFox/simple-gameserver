%% @author caohongyang
%% @doc 虎牢关 玩家处理模块
%% Created 2013-5-6


-module(role_hula).
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
cs_hula_buff(#cs_hula_buff{type = Type}) ->
	case check_add_buff(Type) of
		{true, Role, NeedType, NeedNum, BuffNum, AddReputation} ->
			do_add_buff(Type, Role, NeedType, NeedNum, BuffNum, AddReputation);
		{false, Reason} ->
			?sendself(#sc_hula_buff{result=Reason,type=Type})
			end.

%% 请求战斗
cs_hula_fight(#cs_hula_fight{}) ->
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
	TrSpecial= role_data:get_trSpecial(),
	hula_server:fight(role_data:get_roleID(),role_data:get_fighter_list(), RoleLieuAdd, TalentList,TrSpecial).


%% 请求复活
cs_hula_reborn(_) ->
    #role{roleID=RoleID} = Role = role_data:get_roleInfo(),
    NeedGold = data_hula:get(recover_gold),
    case role_lib:check_money(Role, gold, NeedGold) of
        true ->
            case hula_server:is_state_begin() of
                true ->
                    RoleLieuAdd = role_data:get_lieu_add_attr(),
                    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_HULA_REBORN, 0, ""),
                    TalentList = role_talent:get_active_talent_list(),
                    hula_server:reborn(RoleID, role_data:get_fighter_list(),RoleLieuAdd,TalentList);
                false ->
                    ?sendself(#sc_hula_reborn{result=3})
            end;
        false ->
            ?sendself(#sc_hula_reborn{result=2})
    end.

%% 请求设置离线参与虎牢关
cs_hula_offline_play(#cs_hula_offline_play{openFlag=OpenFlag}) ->
	#role{roleID=RoleID, vipLevel=VipLevel} = role_data:get_roleInfo(),
	if OpenFlag =:= true ->
		   NeedVipLevel = data_hula:get(offline_play_need_vipLevel),
		   if VipLevel >= NeedVipLevel ->
				  DeductGold = data_hula:get(offline_play_cost_gold),
				  Role = role_data:get_roleInfo(),
				  case role_lib:check_money(Role, gold, DeductGold) of
					  true ->
						  role_lib:deduct_gold_f(Role, DeductGold, ?MONEY_DEC_TYPE_HULA_OFFLINE, 0, ""),
						  hula_server:set_offlinePlayFlag(RoleID,OpenFlag);
					  false ->
						  ?sendself(#sc_hula_offline_play{result=4, newOpenFlag=false})
				  end;
			  true ->
				  ?sendself(#sc_hula_offline_play{result=3, newOpenFlag=false})
		   end;
	   true ->
		   ?sendself(#sc_hula_offline_play{result=2, newOpenFlag=true})
	end.

do_add_buff(Type, Role, NeedType, NeedNum, BuffNum, AddReputation) ->
	#role{roleID=RoleID} = Role,
	ets:insert(?ETS_HULA_BUFF_FLAG, {RoleID, true}),
	Role2 = role_lib:deduct_money_f(Role, NeedType, NeedNum, ?MONEY_DEC_TYPE_HULA_ADD_BUFF, 0, ""),
	_Role3 = role_lib:add_reputation_f(Role2, AddReputation, ?MONEY_ADD_TYPE_HULA_ADD_BUFF, 0, ""),
	hula_server:add_buff(RoleID, BuffNum),
	?sendself(#sc_hula_buff{result=1,type=Type}).
	
check_add_buff(Type) ->
	{BuffNum, NeedType, NeedNum, AddReputation} = data_hula:get({buff,Type}),
	#role{roleID=RoleID} = Role = role_data:get_roleInfo(),
	case role_lib:check_money(Role, NeedType, NeedNum) of
		true ->
			case ets:lookup(?ETS_HULA_BUFF_FLAG, RoleID) of
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


