%% @author lixinglong
%% @doc @todo Add description to role_homeBoss.


-module(role_homeBoss).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  
-include("def_mail.hrl").  

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================

cs_homeBoss_info(#cs_homeBoss_info{roleID=RoleID}) ->
	RoleID2 = case RoleID of 0 -> role_data:get_roleID();  _ -> RoleID end,
	{MyBoss,OwnBoss} = home_boss_server:get_homeBoss_info(RoleID2),
	?sendself(#sc_homeBoss_info{myBoss=MyBoss,ownBoss=OwnBoss}).

cs_homeBoss_attack(#cs_homeBoss_attack{roleID=RoleID,type=Type}) ->
	case check_attack(RoleID,Type) of
		{true,SelfRoleID,NewTimes,HomeBossTimes} ->
			RoleFighterList = role_data:get_fighter_list(),
			RoleLieuAdd = role_data:get_lieu_add_attr(),
			TalentList = role_talent:get_active_talent_list(),
			SkinInfo = role_skin:get_skin_info(),
			TrSpe = role_data:get_trSpecial(),
			role_data:set_homeBoss_times(HomeBossTimes#homeBoss_times{total=NewTimes}),
			home_boss_server:attack_homeBoss(SelfRoleID,RoleID,Type,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo);
%% 			case home_boss_server:attack_boss(SelfRoleID,RoleID,Type,RoleFighterList,RoleLieuAdd,TalentList,TrSpe,SkinInfo) of
%% 				{FightRecord,NowBoss}->
%% 					?sendself(#sc_homeBoss_attack{result=1,replay=FightRecord,nowBoss=NowBoss});
%% 				{false,Reason} ->
%% 					?sendself(#sc_homeBoss_attack{reason=Reason,replay=[],nowBoss=[]})
%% 			end;
		{false,Reason} ->
			?sendself(#sc_homeBoss_attack{result=Reason,replay=[],nowBoss=[]})
	end.

cs_homeBoss_buy_times(#cs_homeBoss_buy_times{n=B})->
	case check_homeBoss_buy_times(B) of
		{true,RoleInfo,NewTimes,HomeBossTimes,Total,NeedGold} ->
			do_homeBoss_buy_times(RoleInfo,NewTimes,HomeBossTimes,Total,NeedGold);
		{false,Reason} ->
			?sendself(#sc_homeBoss_buy_times{result=Reason})
	end.

cs_homeBoss_self(_)->
		#homeBoss_times{total=Total,goldTimes=GoldTimes} = role_data:get_homeBoss_times(),
		{_,GoldList} = data_home_boss:get(buy_times_gold),
		?sendself(#sc_homeBoss_self{times=Total,goldList=GoldList,n=GoldTimes}).

cs_homeBoss_read(#cs_homeBoss_read{roleID=RoleID}) ->
	home_boss_server:read(RoleID),
	?sendself(#sc_homeBoss_read{}).

cs_homeBoss_get_reward(_) ->
	home_boss_server:get_self_reward(role_data:get_roleID()).
get_reward({_,true,{Level,RewardLevel}}) ->
	#role{isMale=IsMale}=RoleInfo = role_data:get_roleInfo(),
	{RewardBase,{Num,T1,T2}} = data_home_boss:get({boss_from_reward,Level,RewardLevel}),
	Reward = if IsMale -> add_reward(RewardBase,Num,T1);true -> add_reward(RewardBase,Num,T2) end,
	role_reward:handle_sell_reward_f(RoleInfo,Reward,?MONEY_ADD_TYPE_HOMEBOSS_REWARD_1,0,""),
	?sendself(#sc_homeBoss_get_reward{result=1});
get_reward({_,false,Reason}) ->
	?sendself(#sc_homeBoss_get_reward{result=Reason}).

add_reward(R=#sell_reward{item=Items},Num,TypeID) ->
	R#sell_reward{item=[#new_item{itemTypeID=TypeID,itemNum=Num,itemLevel=1,itemRank=0}|Items]}.

kill_reward({kill_reward,Reward}) ->
	role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_HOMEBOSS_REWARD_KILL,0,""),
	?CATCH(role_task_trigger:handle({dispach_task,home_boss,1})).

attack_reward({attack_reward,Reward}) ->
	role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_HOMEBOSS_REWARD_ATTACK,0,""),
	?CATCH(role_task_trigger:handle({dispach_task,home_boss,1})).

check_attack(RoleID,Type) ->
	#homeBoss_times{total=Total} = HomeBossTimes = role_data:get_homeBoss_times(),
	if Total =<0 ->
		   {false,4};
	   true ->
		   SelfRoleID = role_data:get_roleID(),
		   case home_boss_server:check_attack(SelfRoleID,RoleID,Type) of
			   true ->
				   {true,SelfRoleID,Total-1,HomeBossTimes};
			   {false,R} ->
				   {false,R}
		   end
	end.
	

check_homeBoss_buy_times(B)->
	#homeBoss_times{goldTimes=GoldTimes,total=Total} = HomeBossTimes = role_data:get_homeBoss_times(),
	{MaxTimes,GoldList} = data_home_boss:get(buy_times_gold),
	case GoldTimes+B > MaxTimes of
		true ->
			{false,3};
		_ ->
			NewTimes = GoldTimes+B,
			NeedGold = calc_need_gold(GoldList,B,GoldTimes),
			Role = role_data:get_roleInfo(),
			case role_lib:check_money(Role, gold, NeedGold) of
				true ->
					{true,Role,NewTimes,HomeBossTimes,Total+B,NeedGold};
				_ ->
					{false,3}
			end
	end.
calc_need_gold(GoldList,N,GoldTimes)->
	lists:foldl(fun(X,Acc)->lists:nth(X,GoldList) + Acc end,0,lists:seq(GoldTimes+1,GoldTimes+N)).

do_homeBoss_buy_times(RoleInfo,NewTimes,HomeBossTimes,Total,NeedGold) ->
	role_lib:deduct_gold_f(RoleInfo,NeedGold,?MONEY_DEC_TYPE_HOME_BOSS_TIMES,NewTimes,""),
	role_data:set_homeBoss_times(HomeBossTimes#homeBoss_times{total=Total,goldTimes=NewTimes}),
	?sendself(#sc_homeBoss_buy_times{result=1,n=NewTimes}).
	
	

init_times()->
	FreeTimes = data_home_boss:get(free_times),
	#homeBoss_times{total=FreeTimes,goldTimes=0}.
