-module(role_family).

-include("def_role.hrl").
-include("def_family.hrl").
-include("def_task.hrl").

-export([]).

cs_family_create(#cs_family_create{family_name=FamilyName, is_gold_create=IsGoldCreate}) ->
    case catch check_family_create(FamilyName, IsGoldCreate) of
        {true, RoleInfo, CostType, CostNum} ->
%% 待公会创建成功后再扣除费用
%%             RoleInfo2 = role_lib:deduct_money_f(RoleInfo, CostType, CostNum, ?MONEY_DEC_TYPE_CREATE_FAMILY, 0, ""),
?INFO("L-cs_family_create 以前这里回扣除钻石"),
            erlang:send(family_manager_server, {do_create, RoleInfo, CostType, CostNum, FamilyName});
        {false, Reason} ->
            case Reason of
                10 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(role_data:get_roleID())
            end,
            %%?ERR("Reason:~w", [Reason]),
            #role{lastJoinFamily=LastJoinFamily} = role_data:get_roleInfo(),
            ?sendself(#sc_family_create{result=Reason, family_info=family_misc:gen_p_family_info(), timestamp=LastJoinFamily + data_family:get(join_protect_seconds)})
    end.

check_family_create(FamilyName, IsGoldCreate) ->
    #role{roleID=RoleID, level=RoleLevel, familyID=FamilyID, lastJoinFamily=LastJoinFamily} = RoleInfo = role_data:get_roleInfo(),
    case family_misc:lock_family_protect(RoleID, ?PROTECT_FAMILY_CREATE) of
        true ->
            next;
        false ->
             erlang:throw({false, 10})
    end,
    check_family_name(FamilyName),
    case RoleLevel >= data_family:get(family_open_level) of
        true ->
            next;
        false ->
            erlang:throw({false, 4})
    end,
    case FamilyID =:= 0 of
        true ->
            next;
        false ->
            erlang:throw({false,5})
    end,
    case LastJoinFamily + data_family:get(join_protect_seconds) >= util:now() of
        true ->
            erlang:throw({false, 9});
        false ->
            next
    end,
    case IsGoldCreate of
        true ->
            NeedGold = data_family:get(gold_create_need),
            case role_lib:check_money(RoleInfo, gold, NeedGold) of
                true ->
                    {true, RoleInfo, gold, NeedGold};
                false ->
                    erlang:throw({false, 6})
            end;
        false ->
%%             NeedCoin = data_family:get(coin_create_need),
%%             case role_lib:check_money(RoleInfo, coin, NeedCoin) of
%%                 true ->
%%                     {true, RoleInfo, coin, NeedCoin};
%%                 false ->
                    erlang:throw({false, 7})
%%             end
    end.

cs_family_invite_request(#cs_family_invite_request{roleID = TarRoleID})->
    #role{roleID=SendRoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID of
        0 ->
            ?sendself(#sc_family_invite_request{result=5}); %自己不在公会中
        _ ->
            family_misc:router_to_family_process(FamilyID, {cs_family_invite_request, SendRoleID, TarRoleID,FamilyID})
    end.

check_family_name(FamilyName) ->
%%     ?ERR("FamilyName:~w", [FamilyName]),
    DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(FamilyName), []),
    case util:check_blankName(DecodeList) of
        true ->
            Length = util:calc_name_length(DecodeList),
            {MinLength, MaxLength} = data_family:get(name_length_limit),
            if Length > MaxLength orelse Length < MinLength ->
                   erlang:throw({false, 2});
               true ->
                   case db_sql:search_familyName(util:latin1(FamilyName),false) of
                       FamilyID when is_integer(FamilyID) ->
                           erlang:throw({false, 3});
                       ?undefined ->
                           ok
                   end
            end;
        _ ->
            erlang:throw({false, 1})
    end.

cs_family_request_join(#cs_family_request_join{family_id=FamilyID}) ->
    case catch check_can_request_join(FamilyID) of
        {ok, RoleInfo, OwnerRoleID} ->
            erlang:send(family_manager_server, {add_role_family_request, RoleInfo, FamilyID, OwnerRoleID, true});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            #role{lastJoinFamily=LastJoinFamily} = role_data:get_roleInfo(),
            ?sendself(#sc_family_request_join{result=Reason, is_self=true, timestamp=LastJoinFamily + data_family:get(join_protect_seconds),family_id=FamilyID})
    end.

check_can_request_join(FamilyID) ->
    #role{roleID=RoleID,familyID=RoleFamilyID, level=RoleLevel, lastJoinFamily=LastJoinFamily} = RoleInfo = role_data:get_roleInfo(),
    case RoleFamilyID > 0 of
        false ->
            next;
        true ->
            erlang:throw({false, 1})
    end,
    case RoleLevel >= data_family:get(family_open_level) of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    case ets:lookup(?ETS_FAMILY_SUMMARY, FamilyID) of
        [] ->
            OwnerRoleID = 0,
            erlang:throw({false, 3});
        [#p_family_summary{cur_members=CurMembers, level=FamilyLevel, owner_role_id=OwnerRoleID}] ->
            case data_family:get({max_role_num, FamilyLevel}) > CurMembers of
                false ->
                    erlang:throw({false, 4});
                true ->
                    next
            end
    end,
    case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,family_id=FamilyID,_='_'}) of
        [] ->
            next;
        _ ->
            erlang:throw({false, 5})
    end,
    case erlang:length(ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,_='_'})) >= data_family:get(role_request_max_num) of
        false ->
            next;
        true ->
            erlang:throw({false, 6})
    end,
%%     case erlang:length(ets:match_object(?ETS_FAMILY_REQUEST, #family_request{family_id=FamilyID,_='_'})) >= data_family:get(family_request_max_num) of
%%         false ->
%%             next;
%%         true ->
%%             erlang:throw({false, 7})
%%     end,
    case LastJoinFamily + data_family:get(join_protect_seconds) >= util:now() of
        true ->
            erlang:throw({false, 8});
        false ->
            next
    end,
    {ok, RoleInfo, OwnerRoleID}.

cs_family_cancel_join(#cs_family_cancel_join{family_id=FamilyID}) ->
    case catch check_can_cancel_join(FamilyID) of
        {ok, RoleID, FamilyRequest} ->
            erlang:send(family_manager_server, {del_role_family_request, RoleID, FamilyID, FamilyRequest});
        {false, Reason} ->
%%             ?ERR("FamilyID:~w, Reason:~w", [FamilyID, Reason]),
            ?sendself(#sc_family_cancel_join{result=Reason, is_self=true,family_id=FamilyID})
    end.

check_can_cancel_join(FamilyID) ->
    RoleID = role_data:get_roleID(),
    case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,family_id=FamilyID,_='_'}) of
        [] ->
            FamilyRequest = ?undefined,
            erlang:throw({false, 1});
        [FamilyRequest] ->
            next
    end,
    {ok, RoleID, FamilyRequest}.

cs_family_agree_join(#cs_family_agree_join{role_id=JoinRoleID}) ->
    case catch check_can_agree_join(JoinRoleID) of
        {ok, RoleID, FamilyID, FamilyRequest} ->
            family_misc:router_to_family_process(FamilyID, {agree_join, RoleID, FamilyID, JoinRoleID, FamilyRequest});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            case Reason of
                9 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(JoinRoleID)
            end,
            ?sendself(#sc_family_agree_join{result=Reason, is_self=true, family_info=family_misc:gen_p_family_info()})
    end.

check_can_agree_join(JoinRoleID) ->
    #role{roleID=RoleID, familyID=FamilyID} = _RoleInfo = role_data:get_roleInfo(),
    case family_misc:lock_family_protect(JoinRoleID, ?PROTECT_FAMILY_JOIN) of
        true ->
            next;
        false ->
            erlang:throw({false, 9})
    end,
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
%%     ?ERR("FamilyID:~w, JoinRoleID:~w", [FamilyID, JoinRoleID]),
    case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=JoinRoleID,family_id=FamilyID,_='_'}) of
        [] ->
            FamilyRequest = ?undefined,
            erlang:throw({false, 2});
        [FamilyRequest] ->
            next
    end,
    {ok, RoleID, FamilyID, FamilyRequest}.

cs_family_refuse_join(#cs_family_refuse_join{role_id=JoinRoleID}) ->
    case catch check_can_refuse_join(JoinRoleID) of
        {ok, RoleID, FamilyID} ->
            family_misc:router_to_family_process(FamilyID, {refuse_join, RoleID, JoinRoleID});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            ?sendself(#sc_family_refuse_join{result=Reason, is_self=true})
    end.

check_can_refuse_join(_JoinRoleID) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    {ok, RoleID, FamilyID}.

cs_family_create_consume(#cs_family_create_consume{}) ->
    NeedCoin = data_family:get(coin_create_need),
    NeedGold = data_family:get(gold_create_need),
    ?sendself(#sc_family_create_consume{need_coin=NeedCoin, need_gold=NeedGold}).

cs_family_get_info(#cs_family_get_info{}) ->
    #role{roleID=RoleID, familyID=FamilyID,lastJoinFamily=LastJoinFamily} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            case family_misc:router_to_family_process(FamilyID, {get_family_info, RoleID}) of
                ok ->
                    ignore;
                fail ->
                    ?sendself(#sc_family_get_info{result=1, family_info=family_misc:gen_p_family_info(),timestamp=LastJoinFamily + data_family:get(join_protect_seconds)})
            end;
        false ->
            ?sendself(#sc_family_get_info{result=1, family_info=family_misc:gen_p_family_info(),timestamp=LastJoinFamily + data_family:get(join_protect_seconds)})
    end.

cs_family_kick(#cs_family_kick{kick_role_id=KickRoleID}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {kick, RoleID, KickRoleID});
        false ->
            ?sendself(#sc_family_kick{result=1, is_self=true, family_info=family_misc:gen_p_family_info()})     
    end.

cs_family_leave(#cs_family_leave{}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {leave, RoleID});
        false ->
%%             ?ERR("no family can not leave", []),
            ?sendself(#sc_family_leave{result=1, is_self=true, family_info=family_misc:gen_p_family_info()})     
    end.

cs_family_change_notice(#cs_family_change_notice{notice=Notice}) ->
    case catch check_can_change_notice(Notice) of
        {ok, RoleID, FamilyID} ->
            family_misc:router_to_family_process(FamilyID, {change_notice, RoleID, Notice});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            ?sendself(#sc_family_change_notice{result=Reason, is_self=true, notice= <<"">>})
    end.

check_can_change_notice(Notice) ->
    DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(Notice), []),
    Length = util:calc_name_length(DecodeList),
    MaxLength = data_family:get(notice_max_length),
    case Length =< MaxLength of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    {ok, RoleID, FamilyID}.


cs_family_change_slogan(#cs_family_change_slogan{slogan=Slogan}) ->
    case catch check_can_change_slogan(Slogan) of
        {ok, RoleID, FamilyID} ->
            family_misc:router_to_family_process(FamilyID, {change_slogan, RoleID, Slogan});
        {false, Reason} ->
%%             ?ERR("Reason:~w", [Reason]),
            ?sendself(#sc_family_change_slogan{result=Reason, is_self=true, slogan= <<"">>})
    end.

check_can_change_slogan(Slogan) ->
    DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(Slogan), []),
    Length = util:calc_name_length(DecodeList),
    MaxLength = data_family:get(slogan_max_length),
    case Length =< MaxLength of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    {ok, RoleID, FamilyID}.

cs_family_request_list(#cs_family_request_list{}) ->
    case catch check_can_get_request_list() of
        {ok, RequestList} ->
            %%?ERR("RequestList:~w", [RequestList]),
            ?sendself(#sc_family_request_list{request_list=RequestList, result=0});
        {false, Reason} ->
            ?sendself(#sc_family_request_list{request_list=[], result=Reason})     
    end.

check_can_get_request_list() ->
    #role{familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 1})
    end,
%%     ?ERR("FamilyID:~w", [FamilyID]),
    RequestList = ets:match_object(?ETS_FAMILY_REQUEST, #family_request{family_id=FamilyID, _='_'}),
    {ok, lists:map(fun(Request) -> family_misc:to_p_family_request(Request) end, RequestList)}.


cs_family_get_log_list(#cs_family_get_log_list{}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {get_family_log_list, RoleID});
        false ->
            ?sendself(#sc_family_get_log_list{result=1, logDataList=[]})
    end.



cs_family_get_contribute_info(#cs_family_get_contribute_info{}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_family_get_contribute_info, RoleID});
        false ->
            ?sendself(#sc_family_get_contribute_info{result=1,lastCTDate=0, ctTypeInfoList=[],ctLogList=[]})
    end.

cs_family_do_contribute(#cs_family_do_contribute{typeID=_TypeID}) ->
	?sendself(#sc_family_do_contribute{result=1}). %% 此处屏蔽主动升级公会的消息处理
%%     #role{roleID=RoleID, familyID=FamilyID,roleName=RoleName} = RoleInfo = role_data:get_roleInfo(),
%%     case FamilyID > 0 of
%%         true ->
%% 		case check_do_family_contribute(RoleInfo,TypeID) of
%% 			{true,ConsumeType, ConsumeValue, Reward,Contribute,Rice} ->
%% 				%RoleInfo2 = role_lib:deduct_money_f(RoleInfo,ConsumeType, ConsumeValue, ?MONEY_DEC_TYPE_FAMILY_CONTRUBUTION, TypeID, ""),
%% 				%role_reward:handle_sell_reward_f(RoleInfo2, Reward, ?MONEY_ADD_TYPE_FAMILY_CONTRIBUTIOIN, TypeID, ""),
%% 				%%update_role_contribution(),
%%             	family_misc:router_to_family_process(FamilyID, {cs_family_do_contribute, RoleID, Contribute,RoleName,TypeID,ConsumeType,ConsumeValue,Reward,Rice});
%% 			{false,Reason} ->
%% 				?sendself(#sc_family_do_contribute{result=Reason})
%% 		end;
%%         false ->
%%             ?sendself(#sc_family_do_contribute{result=1})
%%     end.

check_do_family_contribute(RoleInfo,TypeID)->
	case lists:keyfind(TypeID, 1, data_family:get(contributeTypeList)) of
		false ->
			{false, 5};
		{_,ConsumeType,ConsumeValue,Reward, Contribute,Rice} ->
			ConsumeType2 = 
				case ConsumeType of
					1 -> coin;
					2 -> gold
				end,
			case role_lib:check_money(RoleInfo, ConsumeType2, ConsumeValue) of
				false ->
					{false,3};
				_ ->
					{true,ConsumeType2,ConsumeValue,Reward, Contribute,Rice}
			end
	end.

cs_family_search_by_family_name(#cs_family_search_by_family_name{searchName=SearchName}) ->
    DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(SearchName), []),
    case util:check_blankName(DecodeList) of
        true ->
            Length = util:calc_name_length(DecodeList),
            {MinLength, MaxLength} = data_family:get(name_length_limit),
            if Length > MaxLength orelse Length < MinLength ->
                   ?sendself(#sc_family_search_by_family_name{result=2,infoList=[]});
               true ->
                   case db_sql:search_familyName(util:latin1(SearchName),true) of
                       FamilyIDList when is_list(FamilyIDList) ->
						   erlang:send(family_manager_server, {cs_family_search_by_family_name, role_data:get_roleID(), FamilyIDList});
                       ?undefined ->
                           ?sendself(#sc_family_search_by_family_name{result=3,infoList=[]})
                   end
            end;
        _ ->
           ?sendself(#sc_family_search_by_family_name{result=2,infoList=[]})
    end.

cs_family_change_member_power(#cs_family_change_member_power{changeRoleID=ChangeRoleID,typeID=TypeID}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
			if RoleID =:= ChangeRoleID ->
				   ?sendself(#sc_family_change_member_power{result=4});
			   true ->
				   family_misc:router_to_family_process(FamilyID, {cs_family_change_member_power, RoleID, ChangeRoleID,TypeID})
			end;
        false ->
            ?sendself(#sc_family_change_member_power{result=2})     
    end.

cs_family_send_role_energy(#cs_family_send_role_energy{tarRoleID=TarRoleID}) ->
    #role{roleID=RoleID, familyID=FamilyID,roleName=RoleName} = role_data:get_roleInfo(),
	SendEnergyList = role_data:get_role_send_energy_list(),
    case FamilyID > 0 of
        true ->
			case lists:member(TarRoleID,SendEnergyList) of
				true ->
				   ?sendself(#sc_family_send_role_energy{result=2});
				false->
				   family_misc:router_to_family_process(FamilyID, {cs_family_send_role_energy, RoleID,RoleName, TarRoleID})
			end;
        false ->
            ?sendself(#sc_family_send_role_energy{result=3})     
    end.

cs_family_get_role_energy(#cs_family_get_role_energy{roleName=TarRoleName}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
			family_misc:router_to_family_process(FamilyID, {cs_family_get_role_energy, RoleID,TarRoleName});
        false ->
            ?sendself(#sc_family_send_role_energy{result=2})     
    end.

cs_family_get_send_role_energy_list(#cs_family_get_send_role_energy_list{}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
			family_misc:router_to_family_process(FamilyID, {cs_family_get_send_role_energy_list, RoleID});
        false ->
            ?sendself(#sc_family_get_send_role_energy_list{roleNameList=[]})     
    end.

cs_family_get_member_power(#cs_family_get_member_power{})->
	#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
			family_misc:router_to_family_process(FamilyID, {cs_family_get_member_power, RoleID});
        false ->
            ?sendself(#sc_family_get_member_power{memberPowerList=[]})     
    end.

cs_family_get_role_send_energy_list(#cs_family_get_role_send_energy_list{})->
	?sendself(#sc_family_get_role_send_energy_list{roleIDList = role_data:get_role_send_energy_list()}).

cs_family_storage_info(_) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {family_extra,{cs_family_storage_info,RoleID}});
		false ->
			?sendself(#sc_family_storage_info{result=2,level=0,maxLen=0,itemLen=0,itemList=[]})
	end.

cs_family_storage_req(#cs_family_storage_req{itemUID=ItemUID})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_family_storage_req, ItemUID,RoleID});
		false ->
			?sendself(#sc_family_storage_req{result=2})
	end.

cs_family_storage_assign(#cs_family_storage_assign{itemUID=ItemUID,roleID=TarRoleID})->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_family_storage_assign, ItemUID,TarRoleID,RoleID});
		false ->
			?sendself(#sc_family_storage_assign{result=2})
		end.

cs_family_owner_impeach(_) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_family_owner_impeach,RoleID});
		false ->
			?sendself(#sc_family_owner_impeach{result=2})
	end.

cs_family_impeach_list(_) ->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_family_impeach_list,RoleID});
        false ->
            ?sendself(#sc_family_impeach_list{result=2,impeachmemberlist=[]})
    end.

cs_family_donate_contribution_summary(_)->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID,{cs_family_donate_contribution_summary,RoleID});
        false ->
            ?sendself(#sc_family_donate_contribution_summary{result=2,donate_summary=[]})
    end.

cs_family_wallet(_)->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {family_extra,{cs_family_wallet,RoleID}}) ;
		false ->
			?sendself(#sc_family_wallet{rice=0})
	end.

%% type 1 请求获得新任务 2 请求刷新任务
cs_family_worship(#cs_family_worship{type=Type}) ->
    %% 根据客户端的显示规则,在一个膜拜任务没有提交前,是无法接下一个任务的,
    %% 因此这里需要加上判断,防止无限接任务。公会膜拜任务完成后就会被删除掉
	#role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo( ),
    case FamilyID > 0 of
        false ->
		    ?sendself(#sc_family_worship{type=Type,result=2,task_id=0});
        true ->
		    case role_data:get_task_list(?TASK_TYPE_FAMILY_WORSHIP) of
		        [] ->
		            case Type of
		                1 ->
						    family_misc:router_to_family_process(FamilyID, {cs_family_worship, Type, RoleID, 0});
		                2 ->
		                    ?sendself(#sc_family_worship{type=Type,result=5,task_id=0})
		            end;
		        _ ->
		            case Type of 
		                1 ->
		                    ?sendself(#sc_family_worship{type=Type,result=5,task_id=0});
		                2 ->
		                    case data_family_daily_reward:get(worship_task_refresh_cost) of
		                        undefined ->
		                            ?sendself(#sc_family_worship{type=Type,result=6,task_id=0});
		                        UnionCoin ->
		                            case role_lib:check_money(role_data:get_roleInfo(), unioncoin, UnionCoin) of
		                                false ->
		                                    ?sendself(#sc_family_worship{type=Type,result=6,task_id=0});
		                                true ->
						                    family_misc:router_to_family_process(FamilyID, {cs_family_worship, Type, RoleID, UnionCoin})
		                            end
		                    end
		            end
		    end
    end.

cs_family_worship_fight(#cs_family_worship_fight{}) ->
    #role{roleID=RoleID, familyID=FamilyID} = role_data:get_roleInfo( ),
    case FamilyID > 0 of
        true ->
            RoleLieuAdd = role_data:get_lieu_add_attr(),
            TalentList = role_talent:get_active_talent_list(),
			TrSpecial= role_data:get_trSpecial(),
            family_misc:router_to_family_process(FamilyID, {cs_family_worship_fight, RoleID, role_data:get_fighter_list(), RoleLieuAdd,TalentList,TrSpecial});
        false ->
            ?sendself(#sc_family_worship_fight{result=3,reward=0})
    end.

cs_family_worship_info(#cs_family_worship_info{}) ->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo( ),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_family_worship_info, RoleID});
        false ->
            ?sendself(#sc_family_worship_info{self=0,family=0,reward=0})
    end.

cs_family_worship_limit(#cs_family_worship_limit{}) ->
	#role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
	case FamilyID > 0 of
		true ->
			family_misc:router_to_family_process(FamilyID, {cs_family_worshop_limit,RoleID});
		false ->
			?sendself(#sc_family_worship_limit{member=0,family=0,cost=0})
			end.
    %?sendself(#sc_family_worship_limit{member=data_family_daily_reward:get(worship_times),family=data_family_daily_reward:get(total_worship_times),cost=data_family_daily_reward:get(worship_task_refresh_cost)}).

cs_changename_freetimes(Type) ->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo( ),
    case FamilyID > 0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {cs_changename_freetimes, RoleID,Type});
        false ->
            ?sendself(#sc_changename_freetimes{type=Type,times=-1})
    end.

cs_changename(Type, Name) ->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo( ),
    case FamilyID > 0 of
        true ->
            case catch check_family_name(Name) of
                {false, Result} ->
                    %% 错误码转换
                    NewResult = case Result of
                                    1 ->
                                        4;
                                    2 -> 
                                        5;
                                    3 -> 
                                        3
                                end,
                    ?sendself(#sc_changename{type=Type, result=NewResult});
                ok ->
                    family_misc:router_to_family_process(FamilyID, {cs_changename_check, RoleID, Type, Name})
            end;
        false ->
            #sc_changename{type=Type,result=2} 
   end.

%%请求玩家捐献数据
cs_family_get_donate_contribute_list(#cs_family_get_donate_contribute_list{roleID=TargetRoleID}) ->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo(),
    case FamilyID >0 of
        true ->
            family_misc:router_to_family_process(FamilyID, {do_get_family_donate_contribute_list,RoleID,TargetRoleID});
        false ->
            ?sendself(#sc_family_get_donate_contribute_list{result=2,roleID=TargetRoleID,donateInfo=family_server:family_contribution_to_donateInfo(#family_contribution{})})
    end.
    

family_changename_check_succ(Type, LeftChgNameTimes, Name) ->
    #role{roleID=RoleID,familyID=FamilyID} = role_data:get_roleInfo( ),
    case FamilyID > 0 of
        false ->
            ?sendself(#sc_changename{type=Type,result=2});
        true ->
            case check_cost(Type, LeftChgNameTimes) of 
                {false, Result} ->
                    ?sendself(#sc_changename{type=Type,result=Result});
                {true, List} ->
                    case List of 
                       [] ->
                            next;
                       [BagOther, DelAcc, UpdateAcc, UpdateLogList] ->
                            role_data:set_bagItem(BagOther),
		                    LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
		                    {Date,_} = Time = erlang:localtime(),
		                    behavior_item_consume:log(RoleID, LogItemList, Date, Time,?MONEY_DEC_TYPE_CHANGENAME,Type,""),
		                    %% 通知更新道具数量
		                    if UpdateAcc =/= [] ->
		                        UpdateInfoList = 
		                            lists:map(fun(Update) ->
		                                        #p_item_num_update{itemNum=Update#item.itemNum, itemUID=Update#item.itemUID}
		                                     end, UpdateAcc),
		                            ?sendself(#sc_item_update{updateList=UpdateInfoList});
		                        true ->
		                            next
		                    end,
		                    DelItemIDList = [E||#item{itemUID=E} <- DelAcc],
		                    ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList})
                    end,
                    family_misc:router_to_family_process(FamilyID, {do_change_family_name, RoleID, Type, Name, List})
            end
    end.

                
check_cost(Type, LeftChgNameTimes) ->
    case LeftChgNameTimes > 0 of 
        true ->
            {true, []};
        false ->
             case data_change_name:get({change_cost, Type}) of
                undefined ->
                    {false,2};
                {_,TypeID, NeedNum} ->
                    case item_lib:check_material(TypeID,NeedNum) of
                        false ->
                            {false,7};
                        {true, BagOther, DelAcc, UpdateAcc, UpdateLogList} ->
                            {true, [BagOther, DelAcc, UpdateAcc, UpdateLogList]}
                    end
            end
    end.

%% 操作失败,加回玩家扣除的道具
family_changename_fail(Type, Result, List) ->
    ?sendself(#sc_changename{type=Type,result=Result}),
    %% 将物品加回去
    case List of 
        [_BagOther, DelAcc, _UpdateAcc, UpdateLogList] ->
            lists:foreach(fun(#item{itemTypeID=TypeID,itemNum=Num,itemLevel=Level,itemRank=Rank}) ->
                            item_lib:add_item_f([#new_item{itemLevel=Level,itemNum=Num,itemRank=Rank,itemTypeID=TypeID}], ?MONEY_ADD_TYPE_CHANGENAME_FAIL, 0, "")
                          end, DelAcc),
            %% 道具的Rank和Level不会变,取配置里面的值
            lists:foreach(fun([_, TypeID, NeedAdd, _]) ->
                            #data_item{itemLevel=Level, itemRank=Rank} = data_item:get(TypeID),
                            item_lib:add_item_f([#new_item{itemLevel=Level,itemNum=NeedAdd,itemRank=Rank,itemTypeID=TypeID}], ?MONEY_ADD_TYPE_CHANGENAME_FAIL, 0, "")
                          end, UpdateLogList);
        _ ->
            next
    end.

test_clear_family_talk(RoleID)->
    #rolePublic{familyID=FamilyID} = role_lib:get_rolePublic(RoleID),
    case FamilyID>0 of
        true->
            family_misc:router_to_family_process(FamilyID, {do_test_clear_family_talkdata,RoleID});
        false->
            ignore
    end.

