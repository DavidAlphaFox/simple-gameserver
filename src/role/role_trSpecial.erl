%% @author lxl
%% @doc @todo Add description to trSpecial.


-module(role_trSpecial).
-compile(export_all).
-include("def_role.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================

cs_trSpecial_info(_) ->
	#trSpecial{specialID=ID,trID=TrainerID} = role_data:get_trSpecial(),
	Cost = data_trSpecial:get(clear_ID_cost),
	?sendself(#sc_trSpecial_info{specialID=ID,trainerID=TrainerID
								,cost=activity_server:sell_reward2p_reward_info(Cost)}).

cs_trSpecial_select(#cs_trSpecial_select{specialID=ID}) ->
	#trSpecial{specialID=NowID} =TrSpecial= role_data:get_trSpecial(),
	case data_trSpecial:get({specialID,ID}) of
		?undefined ->
			?sendself(#sc_trSpecial_select{result=3});
		_ ->
			case NowID of
				0 ->
					NewTrSpecial = TrSpecial#trSpecial{specialID=ID},
					role_data:set_trSpecial(NewTrSpecial),
                    %% FIXME 专精现在影响到部分天赋的加成,所以这儿需要重新刷新属性,但感觉效率上有影响
                    %% 而且玩家不一定有受到影响的天赋
                    ger_attr:recacl_gers(), 
					Role = role_data:get_roleInfo(),
					FightPower = role_data:cacl_roleFightPower(),
					RoleInfo = Role#role{fightPower=FightPower},
					role_data:set_roleInfo(RoleInfo),
					role_lib:update_rolePublic(RoleInfo,NewTrSpecial,role_data:get_plane_use_info(),role_data:get_xbattle_data()),
					?sendself(#sc_trSpecial_select{result=1});
				_ ->
					?sendself(#sc_trSpecial_select{result=2})
			end
	end.

cs_trSpecial_clear(_) ->
	#trSpecial{specialID=NowID} = TrSpecial = role_data:get_trSpecial(),
	case NowID of
		0 ->
			?sendself(#sc_trSpecial_clear{result=3});
		_ ->
			Cost = data_trSpecial:get(clear_ID_cost),
			Role= role_data:get_roleInfo(),
			case role_tvcard:check_cost(Role,Cost) of
				{true,{BagItem2,#role{roleID=RoleID},_BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu}} ->
					if  NeedGold > 0 ->
							role_lib:deduct_gold_2_f(Role,NeedGold,?MONEY_DEC_TYPE_TRSPECIAL_CLEAR,NowID,"");
						true ->
							ignore
					end,
					if NeedCoin > 0 ->
						   role_lib:deduct_coin_f(Role,NeedCoin,?MONEY_DEC_TYPE_TRSPECIAL_CLEAR,NowID,"");
					   true ->
						   ignore
					end,
					if NeedRepu > 0 ->
						   role_lib:deduct_reputation_f(Role,NeedRepu,?MONEY_DEC_TYPE_TRSPECIAL_CLEAR,NowID,"");
					   true ->
						   ignore
					end,
					{Date, _} = Time = erlang:localtime(),
					LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
					behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_TRSPECIAL_CLEAR, NowID, ""),
					role_data:set_bagItem(BagItem2),
					DelItemIDList = [E||#item{itemUID=E}<-DelList],
					UpdateList2 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateList],
					%% 提醒客户端更新物品
					?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
					?sendself(#sc_item_update{updateList=UpdateList2}),
					NewTrSpecial = TrSpecial#trSpecial{specialID=0},
					role_data:set_trSpecial(NewTrSpecial),
                    ger_attr:recacl_gers(), 
					RoleLL = role_data:get_roleInfo(),
					FightPower = role_data:cacl_roleFightPower(),
					NewRoleInfo = RoleLL#role{fightPower=FightPower},
					role_data:set_roleInfo(NewRoleInfo),
					role_lib:update_rolePublic(NewRoleInfo,NewTrSpecial,role_data:get_plane_use_info(),role_data:get_xbattle_data()),
					?sendself(#sc_trSpecial_clear{result=1});
				_ ->
					?sendself(#sc_trSpecial_clear{result=2})
			end
	end.
