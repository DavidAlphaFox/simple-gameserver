%% @author lihuachao
%% @doc 套装功能


-module(role_allequipment).


-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-define(ROLE_ALL_EQUIPMENT_INFO_LIST,role_all_equipment_info_list).%%玩家的套装信息
-define(ROLE_ALL_EQUIPMENT_ADD_ATTR_LIST,role_all_equipment_add_attr_list).%%玩家套装增加的属性
-define(ALLEQUIPMENT_EFFECTIVE_ALWAWYS_TYPE,0).
-define(ALLEQUIPMENT_EFFECTIVE_PROPERTY_TYPE,1).
-define(TYPE_BASE_VALUE,1000).
-define(get_type_type(Type),(Type div ?TYPE_BASE_VALUE)).
-define(get_type_value(Type),(Type rem ?TYPE_BASE_VALUE)).
%% ====================================================================
%% Internal functions
%% ====================================================================
init_all_equipment(GerID,0,RoleEquipList)->
    set_ger_all_equipment_info_list(GerID,RoleEquipList),
    set_ger_all_equipment_add_attr_list(GerID,RoleEquipList);

init_all_equipment(GerID,GerTypeID,RoleEquipList)->
	%%?DEBUG("======>>>>>~w",[RoleEquipList]),
	case RoleEquipList of
		?undefined->
			set_ger_all_equipment_info_list(GerID,[]),
			set_ger_all_equipment_add_attr_list(GerID,[]);
		_->
			case length(RoleEquipList) >= 2 of
				true->
					AllEquipmentIDList = data_all_equipment:get_list(),
                    %% IsReplace 表示传奇装备是否在计算套装效果时，成为过替代品
					{_GerAllEquipmentInfoList,GerAllEquipmentAddAttrList} = 
                        lists:foldl(fun(AllEquipmentID,{InfoAcc,AttrAcc}=Acc)->
    							   #data_all_equipment{all_equipment_part_list=AllEquipmentPartList,all_equipment_stat_list=AllEquimentStatList,all_equipment_type=Type} = data_all_equipment:get(AllEquipmentID),
    							   		AllEquipmentEquipList = 
                                       	lists:filter(fun(AllEquipmentPart)->
											case lists:keyfind(AllEquipmentPart,#item.itemTypeID,RoleEquipList) of
												false->
													false;
												_->
													true
											end
										end,AllEquipmentPartList),
                               			LegendAllEquipmentEquipList = 
                                      	lists:filter(fun(AllEquipmentPart)->
                                            case lists:keyfind(AllEquipmentPart + ?LEGENDIDSHIFT,#item.itemTypeID,RoleEquipList) of
                                                false->
                                                    false;
                                                _->
                                                    true
                                            end
                                       	end,AllEquipmentPartList),
                                    	Num1 = length(AllEquipmentEquipList),
                                    	Num2 = length(LegendAllEquipmentEquipList),
                                		% ?INFO("init_all_equipment raw:~w legend:~w",[length(AllEquipmentEquipList),length(LegendAllEquipmentEquipList)]),
    							   		case calc_legend_all_equipment_type(Num1,Num2) of
    								   		{StartPos,Num} ->
    								   			case ?get_type_type(Type) of
    								   				?ALLEQUIPMENT_EFFECTIVE_ALWAWYS_TYPE->
                                           				% ?INFO("init_all_equipment type:~w s:~w n:~w",[AllEquipmentID,StartPos,Num]),
    									   				?CATCH(role_task_trigger:handle({dispach_task,role_allequipment,Num+1})),
    									   				AllEquipmentAddAttrList = lists:sublist(AllEquimentStatList, StartPos, Num),
    									   				{[#p_all_equipment{all_equipment_id=AllEquipmentID,all_equipment_list=AllEquipmentEquipList}|InfoAcc],
    													lists:append(AllEquipmentAddAttrList, AttrAcc)};
    												?ALLEQUIPMENT_EFFECTIVE_PROPERTY_TYPE->
    													%%此处需要判断精灵的Property和激活套装的属性一致性
                                                        ?ERR("GerTypeID:~w ~n",[GerTypeID]),
    													#data_ger{gerProperty=GerProperty} = data_ger:get(GerTypeID),
    													case GerProperty =:= ?get_type_value(Type) orelse ?get_type_value(Type)=:=0 of
    														true->
    															AllEquipmentAddAttrList = lists:sublist(AllEquimentStatList, StartPos, Num),
    									   						{[#p_all_equipment{all_equipment_id=AllEquipmentID,all_equipment_list=AllEquipmentEquipList}|InfoAcc],
    															lists:append(AllEquipmentAddAttrList, AttrAcc)};
    														false->
    															Acc
    													end
    											end;
    								   		ignore ->
    								   			Acc
    							   		end
    					   			end, {[],[]}, AllEquipmentIDList),
%% 					set_ger_all_equipment_info_list(GerID,GerAllEquipmentInfoList),
					set_ger_all_equipment_add_attr_list(GerID,GerAllEquipmentAddAttrList);
				%% 			?sendself(#sc_item_all_equipment{gerID=GerID,all_equipment_info_list=GerAllEquipmentInfoList});
				false->
					set_ger_all_equipment_info_list(GerID,[]),
					set_ger_all_equipment_add_attr_list(GerID,[])
			%% 			?sendself(#sc_item_all_equipment{gerID=GerID,all_equipment_info_list=[]})
			end
	end.

calc_legend_all_equipment_type(NumNormal0,NumLegend0) when (NumNormal0+NumLegend0) >= 2->
    NumLegend = max(0,NumLegend0 -1),
    Num = (NumNormal0+NumLegend0) - 1 - NumLegend,
    StartPos = max(1,NumLegend + 1),
    {StartPos,Num};
calc_legend_all_equipment_type(_,_) ->
    ignore.

set_ger_all_equipment_info_list(GerID,RoleAllEquipmentInfoList) when is_list(RoleAllEquipmentInfoList)->
			put({?ROLE_ALL_EQUIPMENT_INFO_LIST,GerID},RoleAllEquipmentInfoList).

get_ger_all_equipment_info_list(GerID)->
			case get({?ROLE_ALL_EQUIPMENT_INFO_LIST,GerID}) of
					?undefined->
							[];
					InfoL->
							InfoL
			end.

set_ger_all_equipment_add_attr_list(GerID,RoleAllEquipmentAddAttrList) when is_list(RoleAllEquipmentAddAttrList)->
			put({?ROLE_ALL_EQUIPMENT_ADD_ATTR_LIST,GerID},RoleAllEquipmentAddAttrList).

get_ger_all_equipment_add_attr_list(GerID)->
			case get({?ROLE_ALL_EQUIPMENT_ADD_ATTR_LIST,GerID}) of
					?undefined->
							[];
					InfoL->
							InfoL
			end.

%%判断装备列表是否能够组成某个套装
check_equip_satify_suit(EquipList,SuitID)->
    #data_all_equipment{all_equipment_part_list=AllEquipmentPartList} = data_all_equipment:get(SuitID),
    AllEquipmentEquipList = lists:filter(fun(AllEquipmentPart)->
                                            case lists:keyfind(AllEquipmentPart,#item.itemTypeID,EquipList) of
                                                false->
                                                    false;
                                                _->
                                                    true
                                            end
                                        end,AllEquipmentPartList),
    LegendAllEquipmentEquipList = lists:filter(fun(AllEquipmentPart)->
                                            case lists:keyfind(AllEquipmentPart + ?LEGENDIDSHIFT,#item.itemTypeID,EquipList) of
                                                false->
                                                    false;
                                                _->
                                                    true
                                            end
                                        end,AllEquipmentPartList), 
    length(AllEquipmentPartList) =:=  length(AllEquipmentEquipList)+length(LegendAllEquipmentEquipList).  






