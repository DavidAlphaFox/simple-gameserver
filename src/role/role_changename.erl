-module(role_changename).

-include("def_role.hrl"). 
-compile(export_all). 

cs_changename_freetimes(#cs_changename_freetimes{type=Type}) ->
    case Type of 
       ?CHANGE_SELF_NAME ->
            #roleTimes{leftChgNameTimes=LeftChgNameTimes} = role_data:get_roleTimes(),
            case LeftChgNameTimes > 0 of
                true ->
                    ?sendself(#sc_changename_freetimes{type=?CHANGE_SELF_NAME,times=LeftChgNameTimes}); 
                _ ->
                    {LeftTimes, Extra} = case data_change_name:get({change_cost, ?CHANGE_SELF_NAME}) of
		                                     undefined ->
		                                        {-1,[]};
		                                    {_,TypeID,NeedNum} ->
		                                        {0, [TypeID,NeedNum]}
		                                end,  
                    ?sendself(#sc_changename_freetimes{type=?CHANGE_SELF_NAME,times=LeftTimes,extra=Extra}) 
            end;
       ?CHANGE_FAMILY_NAME ->
            role_family:cs_changename_freetimes(Type);
        _ ->
           ?sendself(#sc_changename_freetimes{type=Type,times=-1})
    end. 

cs_changename(#cs_changename{type=Type,name=Name}) ->
    %%?ERR("�����޸�����,����:~p, ����:~p.~n", [Type, util:latin1(Name)]),
    case Type of
        ?CHANGE_SELF_NAME ->
            case check_role_change_name(Name) of
                {true, RoleTimes, List} ->
                    do_change_role_name(Name, RoleTimes, List);
                {false, Result} ->
                    ?sendself(#sc_changename{type=?CHANGE_SELF_NAME, result=Result})
            end;
        ?CHANGE_FAMILY_NAME ->
            role_family:cs_changename(Type, Name);
        _ ->
            ?sendself(#sc_changename{type=Type, result=2})
    end.

%% �ж��ܷ����
check_role_change_name(Name) ->
    #roleTimes{leftChgNameTimes=LeftChgNameTimes} = RoleTimes = role_data:get_roleTimes( ),
    case data_change_name:get({change_cost, ?CHANGE_SELF_NAME}) of
        undefined ->
            {false,2};
        {_,TypeID,NeedNum} ->
		    case LeftChgNameTimes > 0 of
		        true ->
		            {true, RoleTimes, []};
		        false ->
		            case item_lib:check_material(TypeID,NeedNum) of
		                false ->
		                    {false,7};
		                {true, BagOther, DelAcc, UpdateAcc, UpdateLogList} ->
		                    case gw:check_roleName(Name) of
		                        ok ->
		                            {true, RoleTimes, [BagOther, DelAcc, UpdateAcc, UpdateLogList]};
		                        {false, Result} ->
		                            {false, Result};
		                        _ ->
		                            {false,2}
		                    end
                    end
            end
    end.

%% �������
do_change_role_name(Name, #roleTimes{leftChgNameTimes=LeftChgNameTimes}=RoleTimes, List) ->
    #role{roleID=RoleID,familyID=FamilyID,roleName=OldName} = RoleInfo = role_data:get_roleInfo( ),
    %% �ȸ�����ݿ��е�����,��ֹ����
    case db_sql:update_role_name(RoleID,Name) of
        false ->
            ?sendself(#sc_changename{type=?CHANGE_SELF_NAME,result=3});
        true ->
            %% �޸��ڴ��е����
            NewRoleInfo = RoleInfo#role{roleName=Name},
            role_data:set_roleInfo(NewRoleInfo),
            role_lib:update_rolePublic(NewRoleInfo,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data()),
            role_generalteam:update_roleinfo(NewRoleInfo),
            case List of 
                [] ->
                    next;
                [BagOther, DelAcc, UpdateAcc, UpdateLogList] ->
                    role_data:set_bagItem(BagOther),
                    LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
                    {Date,_} = Time = erlang:localtime(),
                    behavior_item_consume:log(RoleID, LogItemList, Date, Time,?MONEY_DEC_TYPE_CHANGENAME,?CHANGE_SELF_NAME,""),
                    %% ֪ͨ���µ�������
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
            role_data:set_roleTimes(RoleTimes#roleTimes{leftChgNameTimes=max(0,LeftChgNameTimes-1)}),
            ?sendself(#sc_changename{type=?CHANGE_SELF_NAME,result=0}),
            
            %% ��ݿ���ڴ��е�role����޸����,��֪ͨ�ͻ����޸ĳɹ�
            %% Ȼ�������½�����޸�����ͽ�ɫ����صĵط�,��ֹ��Ҫ��
            %% �ĵĵط�̫��ʱ,���¿���
            spawn(fun( )-> do_change_role_name2(RoleID,FamilyID,OldName,Name) end )
    end.

do_change_role_name2(RoleID,FamilyID,OldName,Name) ->
    %% hron_server��levelRank_server��plunder_server��û�н��и���
    %% ����ҪID��
    lists:foreach(fun(E) ->
                    ?CATCH(erlang:send(E, {update_role_name, RoleID, Name}))
                  end, [homestead_server,hron_server,race_server,team_pk_server,mail_server,talk_server,alien_server]),

    %% ��ҪID�;����
    lists:foreach(fun(E) ->
                    ?CATCH(erlang:send(E, {update_role_name, RoleID, OldName, Name}))
                  end, [hula_server,nanm_server]),

    %% �������
    case FamilyID > 0 of
        false ->
            %% û�й���,��Ҫȥ���?���������
            ?CATCH(erlang:send(family_manager_server, {update_role_name, RoleID, Name}));
        true ->
            %% �й���,��Ҫȡ���ᴦ�����
            ?CATCH(family_misc:router_to_family_process(FamilyID, {update_role_name, RoleID, Name})) 
    end.
