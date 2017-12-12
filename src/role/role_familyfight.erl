%% @author lixinglong
%% @doc @todo Add description to role_familyfight.


-module(role_familyfight).

-include("def_role.hrl").
-include("def_family.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

cs_familyfight_info(#cs_familyfight_info{}) ->
	case check_familyfight_open() of
		true ->
			#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
			case FamilyID > 0 of
				true ->
					family_misc:router_to_family_process(FamilyID, {cs_familyfight_info, RoleID});
				false ->
					?sendself(#sc_familyfight_info{result=2,info=[]})
			end;
		_ ->
			?sendself(#sc_familyfight_info{result=3,info=[]})
	end.

%% 报名工会战，消息转发给family_server
cs_familyfight_sign(#cs_familyfight_sign{roleID_list=RoleIDList})->
	#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    {SignNeedNumMin,SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
    RoleIDListLen =  erlang:length(RoleIDList),
    if
        SignNeedNumMin =< RoleIDListLen andalso RoleIDListLen =< SignNeedNumMax ->
        	case FamilyID > 0 of
        		true ->
        			family_misc:router_to_family_process(FamilyID, {cs_familyfight_sign, RoleID, RoleIDList});
        		false ->
        			?sendself(#sc_familyfight_sign{result=2})
        	end;
        true ->
            ?sendself(#sc_familyfight_sign{result=5})
    end.

cs_familyfight_fighter_info(#cs_familyfight_fighter_info{}) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_familyfight_fighter_info, RoleID});
		false ->
			?sendself(#sc_familyfight_fighter_info{result=2})
	end.

cs_familyfight_attack(#cs_familyfight_attack{serverid=TarServerID,familyid=TarFamilyID, roleid=TarRoleID})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_familyfight_attack, RoleID, TarRoleID, TarFamilyID, TarServerID});
		false ->
			?sendself(#sc_familyfight_attack{result=2})
	end.

cs_familyfight_get_fighter_history(#cs_familyfight_get_fighter_history{tarRoleID=TarRoleID}) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID,{cs_familyfight_get_fighter_history,RoleID,TarRoleID});
		_ ->
			?sendself(#sc_familyfight_get_fighter_history{historyList=[]})
	end.

cs_familyfight_replay(#cs_familyfight_replay{recordUID=ReplayUID})->
	Replay = db_sql:get_familyfight_record(ReplayUID),
	?sendself(#sc_familyfight_replay{fightInfo=Replay}).
	
cs_familyfight_get_fight_record_list(#cs_familyfight_get_fight_record_list{})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID,{cs_familyfight_get_fight_record_list,RoleID});
		_ ->
			?sendself(#sc_familyfight_get_fight_record_list{result=2,infoList=[]})
	end.

cs_familyfight_result(#cs_familyfight_result{})->
	#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID,{cs_familyfight_result, RoleID});
		false ->
			?sendself(#sc_familyfight_result{result=2,infoList=[]})
	end.
			
cs_familyfight_instance_open_state(#cs_familyfight_instance_open_state{}) ->
    #role{roleID=RoleID,familyID=FamilyID,vipLevel=VipLevel} = role_data:get_roleInfo( ),
    if
        FamilyID =:= 0 ->
            ?INFO("not in family"),
            ?sendself(#sc_familyfight_instance_open_state{state_list=[],attack_times=0,buy_price=0,buy_price_times=0,next_instance_id=0,is_have_reward=0});
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_instance_open_state,RoleID,VipLevel})
    end.

cs_familyfight_attack_boss(#cs_familyfight_attack_boss{boss_index=BossIndex}) ->
    #role{roleID=RoleID,familyID=FamilyID,vipLevel=VipLevel,roleName=Name} = role_data:get_roleInfo(),
    if
        FamilyID =:= 0 ->
            BlankReward = #p_reward_info{coin=0
                                       ,roleExp=0
                                       ,gerExp=0
                                       ,gold=0
                                       ,reputation=0
                                       ,itemList=[]
                                       ,gerList=[]},
            ?sendself(#sc_familyfight_attack_boss{result=2,fightInfo=[],isKillBoss=0
                                                 ,fight_reward=BlankReward});
        true ->
            FighterList = role_data:get_fighter_list(),
            RoleLieuAdd = role_data:get_lieu_add_attr(),
            TalentList = role_talent:get_active_talent_list(),
			TrSpecial = role_data:get_trSpecial(),
            FighterList2 = role_xbattle:get_add_buff(role_data:get_xbattle_data(),FighterList),
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_attack_boss,RoleID, BossIndex
                                                           ,FighterList2, RoleLieuAdd, TalentList,TrSpecial,VipLevel,Name})
    end.

cs_familyfight_instance_boss_info(#cs_familyfight_instance_boss_info{})->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    if
        FamilyID =:= 0 ->
            ?sendself(#sc_familyfight_instance_boss_info{instance_id=0,boss_list=[]});
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_instance_boss_info,RoleID})
    end.

cs_familyfight_select_instance(#cs_familyfight_select_instance{instance_id=SeleteId})->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    if
        FamilyID =:= 0 ->
            ?sendself(#sc_familyfight_select_instance{result=3});
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_select_instance,RoleID,SeleteId})
    end.

cs_familyfight_bug_attack_time(#cs_familyfight_bug_attack_time{times=Times})->
    #role{roleID=RoleID,familyID=FamilyID,vipLevel=VipLevel} = Role = role_data:get_roleInfo(),
    {NeedGold,_BuyTimes}=data_family:get(family_instance_buy_times_price),
	NeedGold2 = NeedGold * Times,
    CanBuy = role_lib:check_money(Role, gold, NeedGold2),
    if
        FamilyID =:= 0 ->
            ?sendself(#sc_familyfight_bug_attack_time{result=2,new_times=0});
        CanBuy =:= false ->
            ?sendself(#sc_familyfight_bug_attack_time{result=3,new_times=0});
        true ->
            family_misc:router_to_family_process(FamilyID, {add_attack_time_request,RoleID,VipLevel,Times})
    end.

cs_familyfight_instance_reward_info(#cs_familyfight_instance_reward_info{})->
     #role{roleID=RoleID,familyID=FamilyID} = Role = role_data:get_roleInfo(),
    if
        FamilyID =:= 0 ->
            BlankReward = #p_reward_info{coin=0
                                       ,roleExp=0
                                       ,gerExp=0
                                       ,gold=0
                                       ,reputation=0
                                       ,itemList=[]
                                       ,gerList=[]},
            ?sendself(#sc_familyfight_instance_reward_info{is_get_reward=0,win_reward_list=[]
                                                          ,extra_reward_info=BlankReward,damage_list=[]});
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_instance_reward_info,RoleID})
    end.
       
cs_familyfight_instance_get_reward(#cs_familyfight_instance_get_reward{select_index=SelectIndex})->
    #role{roleID=RoleID,familyID=FamilyID,roleName=Name} = Role = role_data:get_roleInfo(),
    if
        FamilyID =:= 0 ->
            BlankReward = #p_reward_info{coin=0
                                       ,roleExp=0
                                       ,gerExp=0
                                       ,gold=0
                                       ,reputation=0
                                       ,itemList=[]
                                       ,gerList=[]},
            BlankInstReward = #instance_reward{roleName="",reward_info=BlankReward,reward_index=0,is_open=0},
            ?sendself(#sc_familyfight_instance_get_reward{result=2,reward_detail=BlankInstReward});
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_instance_get_reward,RoleID,SelectIndex,Name})
    end.

cs_familyfight_get_fighter(#cs_familyfight_get_fighter{})->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    if
        FamilyID =:= 0 ->
            ?sendself(#sc_familyfight_select_fighter{result=2});
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_get_fighter,RoleID})
    end.

cs_familyfight_select_fighter(#cs_familyfight_select_fighter{roleID_list=RoleIdList0})->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    RoleIdList = lists:usort(RoleIdList0), %容错处理，剔除一下重复的roleid
    {SignNeedNumMin,SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
    L = erlang:length(RoleIdList),
    if
        FamilyID =:= 0 ->
            ?sendself(#sc_familyfight_select_fighter{result=2});
        L < SignNeedNumMin orelse SignNeedNumMax < L ->
            ?sendself(#sc_familyfight_select_fighter{result=3});
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_familyfight_select_fighter,RoleID,RoleIdList})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_familyfight_open()->
	case data_family_fight:get(family_fight_open) of
		"1" ->
			true;
		_ ->
			false
	end.
