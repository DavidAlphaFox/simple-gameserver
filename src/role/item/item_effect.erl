%% @author caohongyang
%% @doc 功能道具
%% Created 2013-4-10


-module(item_effect).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% %% 夺宝的免战功能
%% free_plunder(Minutes, _Num) ->
%% 	RoleID = role_data:get_roleID(),
%% 	plunder_server:free_plunder(RoleID, Minutes),
%% 	{ok, 1}.

add_times(ItemTypeID, Num) ->
    case catch add_times2(ItemTypeID, Num) of
        ok ->
            {ok, Num};
        {false, Reason} ->
            {false, Reason}
    end.

add_times2(ItemTypeID, Num) ->
    #data_item_use{addType=AddType,addValue=AddValue,maxTimes=MaxTimes} = data_item_use:get(ItemTypeID),
    ItemUseList = role_data:get_itemUseList(),
    #item_use_info{useTimes=UseTimes} = ItemUseInfo = lists:keyfind(ItemTypeID, #item_use_info.itemTypeID, ItemUseList),
    NewUseTimes = UseTimes + Num,
    case NewUseTimes > MaxTimes of
        true ->
            erlang:throw({false, 4});
        false ->
            next
    end,
    NowValue = get_spec_type_value(AddType),
    MaxValue = get_spec_type_max_value(AddType),
    case NowValue =< MaxValue of
        true ->
            next;
        false ->
            erlang:throw({false, 5})
    end,
    %% 现在允许体力类超过上限
    UsedNum = erlang:min(Num, erlang:trunc((MaxValue - NowValue) / AddValue) + 1),
    NewValue = UsedNum * AddValue + NowValue,
    update_spec_type_value(AddType, NewValue),
	NewUseTimes2 = UseTimes + UsedNum,
    NewItemUseInfo = ItemUseInfo#item_use_info{useTimes=NewUseTimes2},
    NewItemUseList = lists:keyreplace(ItemTypeID, #item_use_info.itemTypeID, ItemUseList, NewItemUseInfo),
    role_data:set_itemUseList(NewItemUseList),
    ?sendself(#sc_item_use_info{use_info_list=[#p_item_use_info{type_id=ItemTypeID,left_times=MaxTimes-NewUseTimes2}]}),
    ok.

get_spec_type_value(Type) ->
    RoleTimes = role_data:get_roleTimes(),
    case Type of
        energy ->
            RoleTimes#roleTimes.energy;
        dscv ->
            RoleTimes#roleTimes.discoveryTimes;
        pvp ->
            RoleTimes#roleTimes.pvpTimes
%%         plunder ->
%%             RoleTimes#roleTimes.plunderTimes
    end.

update_spec_type_value(Type, NewValue) ->
    RoleTimes = role_data:get_roleTimes(),
    #role{familyID = FamilyID} = role_data:get_roleInfo(),
    case Type of
        energy ->
            role_data:set_roleTimes(RoleTimes#roleTimes{energy=NewValue}),
            IntervalSeconds2 = role_lib:get_current_tick(?currentEnergyIntercal),
            ?notify_update(?ra_energy(NewValue, IntervalSeconds2 + RoleTimes#roleTimes.lastEnergyTime));
        dscv ->
            role_data:set_roleTimes(RoleTimes#roleTimes{discoveryTimes=NewValue}),
            IntervalSeconds2 = role_lib:get_current_tick(?currentdiscoveryInterval),
            ?notify_update(?ra_dscv(NewValue, IntervalSeconds2 + RoleTimes#roleTimes.lastDscvTime));
        pvp ->
            role_data:set_roleTimes(RoleTimes#roleTimes{pvpTimes=NewValue}),
            IntervalSeconds2 = role_lib:get_current_tick(?currentPvpInterval),
            ?notify_update(?ra_pvpTimes(NewValue, IntervalSeconds2 + RoleTimes#roleTimes.lastPvpTime));
        _ -> ignore
%%         plunder ->
%%             role_data:set_roleTimes(RoleTimes#roleTimes{plunderTimes=NewValue}),
%%             IntervalSeconds2 = role_lib:get_current_tick(?currentPlunderInterval),
%%             ?notify_update(?ra_plunderTimes(NewValue, IntervalSeconds2 + RoleTimes#roleTimes.lastPlunderTime))
    end.

get_spec_type_max_value(Type) ->
    case Type of
        energy ->
            #role{vipLevel=VipLevel} = role_data:get_roleInfo(),
			10000;
            %role_lib:get_max_energy(VipLevel);
        dscv -> 1;
            %#role{vipLevel=VipLevel} = role_data:get_roleInfo(),
			%10000;
            %role_lib:get_max_dscv_times(VipLevel);
        pvp ->
            data_common:get(max_pvp_times);
        plunder ->
            data_common:get(max_plunder_times)
    end.






%% ====================================================================
%% Internal functions
%% ====================================================================

