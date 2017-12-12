%% @author lixinglong
%% @doc @todo Add description to role_familyfight.


-module(role_familyBoss).

-include("def_role.hrl").
-include("def_family.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

cs_familyBoss_base_info(_) ->
	#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_boss_process(FamilyID, {cs_role_family_boss_base_info,RoleID});
		false ->
			?sendself(#sc_familyBoss_base_info{result=2,infoList=[]})
	end.

cs_familyBoss_attack(#cs_familyBoss_attack{pos=Pos})->
	#role{roleID=RoleID,roleName=RoleName,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			RoleFighterList = role_data:get_fighter_list(),
			RoleLieuAdd = role_data:get_base_add(),
			family_misc:router_to_family_process(FamilyID, {cs_role_family_boss_attack_boss,Pos,RoleID,{RoleFighterList,RoleLieuAdd},RoleName});
			%family_misc:router_to_family_boss_process(FamilyID,{cs_role_family_boss_attack_boss,Pos,RoleID,{RoleFighterList,RoleLieuAdd},RoleName});
		false ->
			?sendself(#sc_familyBoss_attack{result=2,fightInfo=[]})
	end.

cs_familyBoss_hatch_egg(#cs_familyBoss_hatch_egg{pos=Pos})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_familyBoss_hatch_egg,Pos,RoleID});
		false ->
		?sendself(#sc_familyBoss_hatch_egg{result=2})
	end.

cs_familyBoss_feed_boss(#cs_familyBoss_feed_boss{pos=Pos})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_familyBoss_feed_boss, Pos,RoleID});
		false ->
			?sendself(#sc_familyBoss_feed_boss{result=2})
	end.

cs_familyBoss_set_boss_time(#cs_familyBoss_set_boss_time{pos=Pos,time=Time}) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_familyBoss_set_boss_time,Pos,Time,RoleID});
		false ->
			?sendself(#sc_familyBoss_set_boss_time{result=2})
	end.

cs_familyBoss_get_rank(#cs_familyBoss_get_rank{pos=Pos}) ->
	#role{roleID=RoleID,familyID = FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_boss_process(FamilyID, {cs_familyBoss_get_rank, Pos,RoleID});
		false ->
			?sendself(#sc_familyBoss_get_rank{result=2,pos=0,rankList=[]})
		end.

			
			
			
			
			

%% ====================================================================
%% Internal functions
%% ====================================================================



