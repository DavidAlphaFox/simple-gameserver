%% @author:lixinglong
%% @doc:send reward to anyone by mail

-module(role_mail).
-include("def_role.hrl").
-include("def_mail.hrl").

-compile(export_all).
-export([cs_mail_new/1]).

cs_mail_new(#cs_mail_new{content=Content, targetRoleID=TargetRoleID, targetRoleName=TargetRoleName}=Msg) ->
	case get(?last_mail_ts) of
		X when is_integer(X) ->
			Now = util:now(),
			case Now - X > data_common:get(mail_ts_diff) of
				true ->
					TargetRoleID2 = get_targetRoleID(TargetRoleID, TargetRoleName),
					GagList = role_data:get_otherRoleGagList(TargetRoleID2),
					#role{roleName=RoleName} = role_data:get_roleInfo(),
    				case role_talk:is_gag_role(GagList,binary_to_list(RoleName)) of
    					false->
							if 	TargetRoleID2 =:= 0 ->
									?sendself(#sc_mail_new{result=4});
								length(Content) >= 1000 ->
									?sendself( #sc_mail_new{result=2});
								true->
									RoleInfo = role_data:get_roleInfo(),
									erlang:put(?last_mail_ts,Now),
									catch( erlang:send(mail_server,{role_mail, role_data:get_roleID()
														   ,Msg#cs_mail_new{targetRoleID=TargetRoleID2
																		   ,content=util:words_filter(Content)},RoleInfo#role.head,RoleInfo#role.isMale}))							  
							end;
						true->
							?sendself(#sc_mail_new{result=6})
					end;
				_ ->
					?sendself( #sc_mail_new{result=3})
			end;
		_ ->
			?sendself( #sc_mail_new{result=3})
	end.

cs_mail_canvass_info(_) ->
    case data_canvass:get(open_time) of
        {Start,End} ->
            Now = calendar:local_time(),
            if
                Start =< Now andalso Now =< End ->
                    CfgCanvassList = data_canvass:get(canvass_list),
                    {_CanvassId,CanvassInfo} = role_data:get_canvass_info(),
                    Curlen = erlang:length(CanvassInfo),
                    AllNum = erlang:length(CfgCanvassList),
                    if
                        Curlen >= AllNum -> State = 3;
                        true -> State = 2 end,
                    ?sendself(#sc_mail_canvass_info{state = State
                                                   ,end_time = util:datetime_to_seconds(End)
                                                   ,all_num = AllNum
                                                   ,current_index = Curlen + 1
                                                   ,reward_num = data_canvass:get(canvass_reward)});        
                true ->
                    ?sendself(#sc_mail_canvass_info{state = 1
                                                   ,end_time = 0
                                                   ,all_num = 0
                                                   ,current_index = 0
                                                   ,reward_num = 0})
            end;
        _ ->
            ?sendself(#sc_mail_canvass_info{state = 1
                                           ,end_time = 0
                                           ,all_num = 0
                                           ,current_index = 0
                                           ,reward_num = 0})
    end.

cs_mail_get_question(#cs_mail_get_question{index = Index})->
    case data_canvass:get(open_time) of
        {Start,End} ->
            Now = calendar:local_time(),
            CfgCanvassList = data_canvass:get(canvass_list),
            AllNum = erlang:length(CfgCanvassList),
            if
                Start =< Now andalso Now =< End andalso Index =< AllNum andalso Index > 0->
                    {Describe,OptionList} = lists:nth(Index,CfgCanvassList),
                    ?sendself(#sc_mail_get_question{index = Index
                                                   ,describe = Describe
                                                   ,option_list = OptionList});    
                true ->
                    ?sendself(#sc_mail_get_question{index = Index
                                                   ,describe = ""
                                                   ,option_list = []})
            end;
        _ ->
            ?sendself(#sc_mail_get_question{index = Index
                                           ,describe = ""
                                           ,option_list = []})
    end.
    
cs_mail_do_select(#cs_mail_do_select{index = Index,selected_num=SelectedNum})->
    case data_canvass:get(open_time) of
        {Start,End} ->
            Now = calendar:local_time(),
            CfgCanvassList = data_canvass:get(canvass_list),
            AllNum = erlang:length(CfgCanvassList),
            {OldCanvassId,OldCanvassInfo} = role_data:get_canvass_info(),
            OldCanvassInfoLen = erlang:length(OldCanvassInfo),
            if
                Start > Now orelse Now > End ->
                    ?INFO("cs_mail_do_select no open ~w",[{Index,SelectedNum,OldCanvassInfo}]),
                    ?sendself(#sc_mail_do_select{result = 2
                                                ,state = 1});
                Index =< AllNum andalso Index == (OldCanvassInfoLen + 1)->
                    {_,OptionList} = lists:nth(Index,CfgCanvassList),
                    OptionLen = erlang:length(OptionList),
                    if
                        SelectedNum > OptionLen orelse SelectedNum =< 0 ->
                            ?sendself(#sc_mail_do_select{result = 2
                                                        ,state = 2});
                        true ->
                            NewCanvassInfo = lists:reverse([SelectedNum|lists:reverse(OldCanvassInfo)]),
                            role_data:set_canvass_info({OldCanvassId,NewCanvassInfo}),
                            CfgCanvassList = data_canvass:get(canvass_list),
                            Curlen = erlang:length(NewCanvassInfo),
                            AllNum = erlang:length(CfgCanvassList),
                            #role{roleID=RoleID,roleName=RoleName} = role_data:get_roleInfo(),
                            if
                                Curlen >= AllNum ->
                                    mail_server:send_sys_mail(RoleID, ?MAIL_CANVASS_REWARD, [RoleName], ""
                                                             ,{sell_reward,0,0,0,data_canvass:get(canvass_reward),[],0,[]}),
                                    State = 3;
                                true -> State = 2 end,
                            ?sendself(#sc_mail_do_select{result = 1
                                                        ,state = State})
                    end;    
                true ->
                    ?INFO("cs_mail_do_select error ~w",[{Index,SelectedNum,OldCanvassInfo}]),
                    ?sendself(#sc_mail_do_select{result = 2
                                                ,state = 1})
            end;
        _ ->
            ?sendself(#sc_mail_do_select{result = 2
                                        ,state = 1})
    end.

get_targetRoleID(0, TargetRoleName) ->
	case db_sql:search_roleName(TargetRoleName) of
		?undefined ->
			0;
		RoleID ->
			RoleID
	end;
get_targetRoleID(TargetRoleID,_TargetRoleName) ->
	TargetRoleID.



	
