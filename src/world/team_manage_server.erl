%% @author zcl
%% @doc 组队管理
%% Created 2014-01-08
-module(team_manage_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-include("def_carlos.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================
% 1->解散队伍权限
% 2->离开队伍权限
% 3->任命队长权限
% 4->任命副队长权限
% 5->撤销副队长权限
% 6->邀请成员权限
% 7->踢出成员权限
% 8->更改队伍类型
-define(dump_interval, 300*1000).			  %保存数据间隔
-define(check_tick,300*1000).
-define(ETS_TEAM_LIST,ets_team_list).     %保存当前所有的组队信息
-define(TEAM_CREATE,1).
-define(UNTEAM_CREATE,2).
-define(RELEASE_TEAM_AUTHORITY,1).                    %%解散队伍权限
-define(LEAVE_TEAM_AUTHORITY,2).                      %%离开队伍权限
-define(APPOINT_TEAM_LEADER_AUTHORITY,3).             %%任命队长权限
-define(APPOINT_VICE_TEAM_LEADER_AUTHORITY,4).        %%任命副队长权限
-define(CANCEL_VICE_TEAM_LEADER_AUTHORITY,5).         %%撤销副队长权限
-define(INVITE_MEMBER_AUTHORITY,6).                   %%邀请成员权限
-define(KICK_MEMBER_AUTHORITY,7).                     %%踢出成员权限
-define(CHANGE_TEAM_TYPE_AUTHORITY,8).                %%更改队伍类型权限     

-define(VICE_TEAM_LEADER_LIMIT,1).                    %%副队长上限类型
-define(TEAM_MEMBER_LIMIT,2).                         %%成员上限类型
-define(INVITE_ACCEPT,1).                             %%接受组队邀请
-define(INVITE_REFUSE,2).                             %%拒绝组队邀请
-define(IS_IN_TEAM,3).                                %%已经在组队中

%%队伍状态
-define(RELEASE_BY_LEADER,1).                         %%队长解散组队
-define(RELEASE_BY_NUMBER,2).                         %%人数解散队伍
-define(NEW_CREATE,3).                                %%新建组队
-define(EXIST,4).                                     %%组队存在
-define(UPDATE_TEAM,5).                               %%队伍更新
-define(DELETE_AFTER_PERSIST,6).                      %%持久化之后需要删除ets项

-define(invite_timeout_ref,invite_timeout_ref).       %%保存所有邀请超时的计时器引用

%%========================来自doublematch_server中的定义,需要同步修改==========================
-define(match_state_standby,1).
-define(match_state_sign,2).
-define(match_state_fight,3).
%% ===================Dict Key End   =========================


%% =========================================================================================

%%组队信息
% -record(team_info,{teamid=0,teamleader_roleid=0,vice_teamleader_rolelist=[],team_member=[],teamtype=0,teamstatus=0}).
%% 初始化组队管理进程，创建ets表
init_team_manager_server() ->
	case db_sql:get_generalteam_all_info() of 
		TeamInfoList when is_list(TeamInfoList)->
			NewTeamInfoList = deal_teaminfo_list(TeamInfoList),
			spawn(fun()->lists:foreach(fun(E)->update_teamid_by_teaminfo(E) end,NewTeamInfoList) end),
			ets:insert(?ETS_TEAM_LIST,NewTeamInfoList);
		Other ->
			?INFO("从数据库中读取的组队信息不是列表：TeamInfo：~w ~n",[Other])
	end.

%% ===================================================================


start() ->
	{ok,_}=supervisor:start_child(world_sup, 
								  {?MODULE,
								   {?MODULE, start_link, []},
								   permanent, 600000, worker, [?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	ets:new(?ETS_TEAM_LIST, [{keypos, 2}, set, public, named_table]),

	init_team_manager_server(),
	erlang:send_after(?dump_interval,self(),dump_data),
	erlang:send_after(?check_tick,self(),check_data),
	erlang:send_after(?dump_interval, self(), do_hibernate),
	{ok, []}.

handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info(do_hibernate,State)->
	erlang:send_after(?dump_interval, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(dump_data,State)->
	erlang:send_after(?dump_interval,self(),dump_data),
	do_persist(),
	{noreply,State};

handle_info(check_data,State)->
	erlang:send_after(?dump_interval,self(),check_data),
	do_check(),
	{noreply,State};

handle_info(Info, State) ->
	?CATCH(do_handle_info(State, Info)),
	%do_handle_info(State, Info),
	{noreply, State}.

terminate(Reason, State) ->
	do_persist(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% %% ====================================================================
do_persist()->
	case ets:tab2list(?ETS_TEAM_LIST) of
		[]->
			db_sql:del_all_generalteam_info();
		TeamInfoList->
			{UpdateList,DeleteList} = lists:foldl(fun(TeamInfo,{UpdateAcc,DeleteAcc})->
				case TeamInfo#team_info.teamstatus of
					?RELEASE_BY_NUMBER->
						{UpdateAcc,[{TeamInfo#team_info.teamid,TeamInfo}|DeleteAcc]};
					?RELEASE_BY_LEADER->
						{UpdateAcc,[{TeamInfo#team_info.teamid,TeamInfo}|DeleteAcc]};
					?NEW_CREATE->
						{[{TeamInfo#team_info.teamid,TeamInfo}|UpdateAcc],DeleteAcc};
					?UPDATE_TEAM->
						{[{TeamInfo#team_info.teamid,TeamInfo}|UpdateAcc],DeleteAcc};
					_ ->
						{UpdateAcc,DeleteAcc}
				end
			end,{[],[]},TeamInfoList),
			?INFO("UpdateList:~w DeleteList:~w ~n",[UpdateList,DeleteList]),
			case UpdateList of
				[]->
					ignore;
				_ ->
					db_sql:set_generalteam_info(UpdateList)
			end,
			case DeleteList of
				[]->
					ignore;
				_ ->
					TeamIDList = [TeamID||{TeamID,_TeamInfo}<-DeleteList],
					db_sql:del_generalteam_info(TeamIDList)
			end,
			lists:foreach(fun(TeamInfo)->
				case TeamInfo#team_info.teamstatus of
					?RELEASE_BY_LEADER->
						store_teaminfo2ets(TeamInfo#team_info{teamstatus=?DELETE_AFTER_PERSIST});
					?RELEASE_BY_NUMBER->
						store_teaminfo2ets(TeamInfo#team_info{teamstatus=?DELETE_AFTER_PERSIST});
					_ ->
						store_teaminfo2ets(TeamInfo#team_info{teamstatus=?EXIST})
				end
			end,TeamInfoList)
	end.

do_check()->
	ok.

%%创建组队时，实际上没有创建组队并且向ets表中保存组队信息，只是向被组队者发送组队信息
do_handle_info(_State,{createteam,CreateRole,TarRoleID,TeamType})->
	TeamId = generate_team_id(),
	case role_lib:is_online(TarRoleID) of
		true ->
			RoleInfo = role_lib:call_server(TarRoleID, get_role_info,1000),
			case is_in_team(RoleInfo) of
				true ->
					?unicast(CreateRole#role.roleID,#sc_generalteam_create{result=3});
				false ->
					EndTime = util:now()+data_team:get(data_team_invite_interval),
					TimeOutRef = erlang:send_after(data_team:get(data_team_invite_interval)*1000,self(),{invite_timeout,RoleInfo,CreateRole,TeamId}),
					case add_invite_timeout_ref({CreateRole#role.roleID,TarRoleID,TimeOutRef}) of
						true->
							?unicast(TarRoleID,#sc_generalteam_invite_request{memberinfo=transform_role2p_team_member_info(CreateRole),teamType=TeamType,teamStatu=?UNTEAM_CREATE,teamID=TeamId,endtime=EndTime}),
							update_invite_remian_time(TarRoleID,CreateRole#role.roleID,TeamId,TeamType);
						false->
							ignore
					end,
					?unicast(CreateRole#role.roleID,#sc_generalteam_create{result=1})
			end;
		false ->
			?unicast(CreateRole#role.roleID,#sc_generalteam_create{result=2})
	end;

do_handle_info(_State,{teaminvite,Role,TarRoleID})->
	case catch check_invite(Role,TarRoleID) of
		{error,Reason}->
			?unicast(Role#role.roleID,#sc_generalteam_invite{result=Reason});
		{ok,Reason,TeamType,TeamId,TarRoleInfo} ->
			EndTime = util:now()+data_team:get(data_team_invite_interval),
			TimeOutRef=erlang:send_after(data_team:get(data_team_invite_interval)*1000,self(),{invite_timeout,TarRoleInfo,Role,TeamId}),
			case add_invite_timeout_ref({Role#role.roleID,TarRoleID,TimeOutRef}) of
				true->
					?unicast(TarRoleID,#sc_generalteam_invite_request{memberinfo=transform_role2p_team_member_info(Role),teamType=TeamType,teamStatu=?TEAM_CREATE,teamID=TeamId,endtime=EndTime}),
					update_invite_remian_time(TarRoleID,Role#role.roleID,TeamId,TeamType);
				false->
					ignore
			end,
			?unicast(Role#role.roleID,#sc_generalteam_invite{result=Reason})

	end;

do_handle_info(_State,{inviteresponse,InviteAccept,TeamID,InviteRoleID,TeamType,TeamStatu,TarRole})->
	#role{roleID=TarRoleID} = TarRole,
	del_invite_timeout_ref({InviteRoleID,TarRole#role.roleID}),
	case role_lib:is_online(InviteRoleID) of
		true->
			case InviteAccept of
				?INVITE_REFUSE->
					InviteRoleInfo = role_lib:call_server(InviteRoleID,get_role_info,1000),
					case is_in_team(TarRole) andalso TarRole#role.teamId=:= InviteRoleInfo#role.teamId andalso TarRole#role.teamId =/= -1 of
						true->
							ignore;
						false->
							?unicast(InviteRoleID,#sc_generalteam_invite_response{inviteAccept=InviteAccept,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=transform_role2p_team_member_info(TarRole)})
					end;
				?INVITE_ACCEPT->
					case TeamStatu of
						?TEAM_CREATE->
							case catch add_team_member(TeamID,TarRoleID,InviteRoleID,true) of
								Info ->
									?INFO("add_team_member result:~w~n",[Info])
							end;
						?UNTEAM_CREATE->
							InviteRoleInfo = role_lib:call_server(InviteRoleID,get_role_info,1000),
							case is_in_team(InviteRoleInfo) of
								true->
									case catch add_team_member(InviteRoleInfo#role.teamId,TarRoleID,InviteRoleID,false) of
										Info-> ?INFO("add_team_member result:~w ~n",[Info])
									end;
								false->
									add_team_info(InviteRoleID,TeamType,TarRoleID)
							end;
						Other ->
							?INFO("undefined TeamStatu :~w ~n",[Other])
					end;
				?IS_IN_TEAM->
					?unicast(InviteRoleID,#sc_generalteam_invite_response{inviteAccept=4,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=transform_role2p_team_member_info(TarRole)})

			end;
		false ->
			?unicast(TarRole#role.roleID,#sc_generalteam_invite_response{inviteAccept=4,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=transform_role2p_team_member_info(TarRole)})
	end;	

do_handle_info(_State,{leaveteam,RoleInfo})->
	case get_team_info_by_teamid(RoleInfo#role.teamId) of
		?undefined->
			?unicast(RoleInfo#role.roleID,#sc_generalteam_leaveteam{result=2,roleID=RoleInfo#role.roleID});
		TeamInfo ->
			case check_role_right(RoleInfo#role.roleID,TeamInfo,?LEAVE_TEAM_AUTHORITY) of
				false->
					?unicast(RoleInfo#role.roleID,#sc_generalteam_leaveteam{result=3,roleID=RoleInfo#role.roleID});
				true->
					{_Result,NewTeamInfo} = do_leave_team(RoleInfo#role.roleID,TeamInfo),
					broadcast_team_info(TeamInfo,#sc_generalteam_leaveteam{result=1,roleID=RoleInfo#role.roleID}),
					update_role_teamid(RoleInfo#role.roleID,-1),
                    %% FIXME WORKAROUND 
                    send_teamupdate_message_to_other_server(TeamInfo),
                    ?INFO("TeamInfo update to other server TeamInfo:~w ~n",[NewTeamInfo]),
					send_teamupdate_message_to_special_team(NewTeamInfo),
					store_teaminfo2ets(NewTeamInfo),
					update_generalteam_info(NewTeamInfo)
			end
	end;

do_handle_info(_State,{disbandteam,RoleInfo})->
	case get_team_info_by_teamid(RoleInfo#role.teamId) of
		?undefined->
			?unicast(RoleInfo#role.roleID,#sc_generalteam_disbandteam{result=2});
		TeamInfo->
			case check_role_right(RoleInfo#role.roleID,TeamInfo,?RELEASE_TEAM_AUTHORITY) of
				false->
					?unicast(RoleInfo#role.roleID,#sc_generalteam_disbandteam{result=3});
				true->
					NewTeamInfo = TeamInfo#team_info{teamstatus=?RELEASE_BY_LEADER},
					lists:foreach(fun(Member)->update_role_teamid(Member#p_team_member_info.roleID,-1) end,NewTeamInfo#team_info.team_member),
					update_generalteam_info(NewTeamInfo),
                    %% FIXME WORKAROUND 
                    send_teamupdate_message_to_other_server(TeamInfo),
                    ?INFO("TeamInfo update to other server TeamInfo:~w ~n",[NewTeamInfo]),
					send_teamupdate_message_to_special_team(NewTeamInfo),
					store_teaminfo2ets(NewTeamInfo)
			end
	end;

do_handle_info(_State,{kickmember,KickRoleID,RoleInfo})->
	case get_team_info_by_teamid(RoleInfo#role.teamId) of
		?undefined->
			?unicast(RoleInfo#role.roleID,#sc_generalteam_kick{result=2,kickRoleID=KickRoleID,roleID=RoleInfo#role.roleID});
		TeamInfo->
			case check_role_right(RoleInfo#role.roleID,TeamInfo,?KICK_MEMBER_AUTHORITY) of
				false->
					?unicast(RoleInfo#role.roleID,#sc_generalteam_kick{result=3,kickRoleID=KickRoleID,roleID=RoleInfo#role.roleID});
				true->
					case do_leave_team(KickRoleID,TeamInfo) of
						{ok,NewTeamInfo}->
							?unicast(KickRoleID,#sc_generalteam_kick{result=1,kickRoleID=KickRoleID,roleID=RoleInfo#role.roleID}),
							% ?unicast(RoleInfo#role.roleID,#sc_generalteam_kick{result=1,kickRoleID=KickRoleID}),
							broadcast_team_info(NewTeamInfo,#sc_generalteam_kick{result=1,kickRoleID=KickRoleID,roleID=RoleInfo#role.roleID}),
							update_generalteam_info(NewTeamInfo),
							update_role_teamid(KickRoleID,-1),
                            %% FIXME WORKAROUND 
                            send_teamupdate_message_to_other_server(TeamInfo),
                           	?INFO("TeamInfo update to other server TeamInfo:~w ~n",[NewTeamInfo]),
							send_teamupdate_message_to_special_team(NewTeamInfo),
							store_teaminfo2ets(NewTeamInfo);
						{error,_NewTeamInfo}->
							?unicast(RoleInfo#role.roleID,#sc_generalteam_kick{result=4,kickRoleID=KickRoleID,roleID=RoleInfo#role.roleID})
					end
			end
	end;

do_handle_info(_State,{changeauthority,Type,RoleInfo,TarRoleID})->
	case get_team_info_by_teamid(RoleInfo#role.teamId) of
		?undefined->
			?unicast(RoleInfo#role.roleID,#sc_generalteam_change_authority{result=2,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID});
		TeamInfo->
			case check_role_right(RoleInfo#role.roleID,TeamInfo,Type) of
				false->
					?unicast(RoleInfo#role.roleID,#sc_generalteam_change_authority{result=3,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID});
				true->
					case is_in_team(TarRoleID,TeamInfo) of
						false->
							?unicast(RoleInfo#role.roleID,#sc_generalteam_change_authority{result=4,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID});
						true->
							case change_authority(Type,TarRoleID,TeamInfo) of
								{ok,NewTeamInfo}->
									case check_special_changeauthority_limit(Type,TarRoleID,TeamInfo) of
										true->
											change_special_team_authority(Type,TarRoleID,TeamInfo),
											broadcast_team_info(NewTeamInfo,#sc_generalteam_change_authority{result=1,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID}),
											update_generalteam_info(NewTeamInfo),
											store_teaminfo2ets(NewTeamInfo);
										false->
											?unicast(RoleInfo#role.roleID,#sc_generalteam_change_authority{result=8,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID})
									end;
								{error,_NewTeamInfo}->
									?unicast(RoleInfo#role.roleID,#sc_generalteam_change_authority{result=6,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID})
							end
					end
			end
	end;

do_handle_info(_State,{teamtalk,Message,RoleInfo})->
	case get_team_info_by_teamid(RoleInfo#role.teamId) of
		?undefined->
			?unicast(RoleInfo#role.roleID,#sc_generalteam_talk{result=2});
		TeamInfo->
			spawn(fun()-> broadcast_team_meesage(Message,RoleInfo,TeamInfo) end),
			?unicast(RoleInfo#role.roleID,#sc_generalteam_talk{result=1})
	end;

do_handle_info(_State,{invite_timeout,TarRoleInfo,Role,TeamID})->
	?INFO("invite_timeout: TarRoleInfoID:~w RoleID:~w TeamID:~w ~n",[TarRoleInfo#role.roleID,Role#role.roleID,TeamID]),
	del_invite_timeout_ref({Role#role.roleID,TarRoleInfo#role.roleID}),
	?unicast(Role#role.roleID,#sc_generalteam_invite_response{inviteAccept=2,teamID=TeamID,inviteRoleID=Role#role.roleID,invitedRole=transform_role2p_team_member_info(TarRoleInfo)});

do_handle_info(_State,Info)->
	?ERR("undefined Info:~w ~n",[Info]).

%%internal function--------------------------------------------------------------

%%向组队中添加成员
add_team_member(TeamID,TarRoleID,InviteRoleID,IsCheckTeamID)->
	TeamInfo = get_team_info_by_teamid(TeamID),
	case TeamInfo of
		?undefined->
			erlang:throw({error,1});
		_->
			next
	end,
	case IsCheckTeamID of
		true->
			case TeamInfo#team_info.teamid =:= TeamID of
				false->
					erlang:throw({error,2});
				true->
					next
			end;
		false->	
			next
	end, 
	case TeamInfo#team_info.teamstatus =/= ?RELEASE_BY_LEADER andalso TeamInfo#team_info.teamstatus =/= ?RELEASE_BY_NUMBER of
		false->
			?unicast(TarRoleID,#sc_generalteam_invite_response{inviteAccept=6,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=get_p_team_member_info(TarRoleID)}),
			erlang:throw({error,3});
		true->
			next
	end,
	case is_in_team(TarRoleID,TeamInfo) of
		false->
			next;
		true->
			erlang:throw({error,4})
	end,
	case check_role_right(InviteRoleID,TeamInfo,?INVITE_MEMBER_AUTHORITY) of
		false->
			?unicast(TarRoleID,#sc_generalteam_invite_response{inviteAccept=7,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=get_p_team_member_info(TarRoleID)}),
			erlang:throw({error,5});
		true->
			next
	end,
	case check_team_limit(TeamInfo,?TEAM_MEMBER_LIMIT) of
		true->
			Position = get_team_next_position(TeamInfo),
			P_team_member_info = get_p_team_member_info(TarRoleID),
			New_p_team_member_info = P_team_member_info#p_team_member_info{position=Position},
			NewTeamMemberList = [New_p_team_member_info|TeamInfo#team_info.team_member],
			NewTeamMemberList2 = lists:keysort(#p_team_member_info.position,NewTeamMemberList),
			NewTeamInfo = TeamInfo#team_info{team_member=NewTeamMemberList2,teamstatus=?UPDATE_TEAM},
			store_teaminfo2ets(NewTeamInfo),
			broadcast_team_info(NewTeamInfo,#sc_generalteam_invite_response{inviteAccept=1,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=New_p_team_member_info}),
			update_role_teamid(TarRoleID,TeamInfo#team_info.teamid),
            %%FIXME WORKAROUND
            send_teamupdate_message_to_other_server(TeamInfo),
            ?INFO("TeamInfo update to other server TeamInfo:~w ~n",[NewTeamInfo]),
			send_teamupdate_message_to_special_team(NewTeamInfo),
			send_roleupdate_message_to_other_server(TarRoleID),
			update_generalteam_info_by_teamID(TeamID);
		false->
			?unicast(TarRoleID,#sc_generalteam_invite_response{inviteAccept=5,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=get_p_team_member_info(TarRoleID)})
	end.

%%新建组队信息
add_team_info(InviteRoleID,TeamType,TarRoleID)->
	TeamID = generate_team_id(),
	P_team_member_info1 = get_p_team_member_info(InviteRoleID),
	P_team_member_info2 = get_p_team_member_info(TarRoleID),
	New_p_team_member_info1 = P_team_member_info1#p_team_member_info{position=1},
	New_p_team_member_info2 = P_team_member_info2#p_team_member_info{position=2},
	TeamInfo = #team_info{teamid=TeamID,teamleader_roleid=InviteRoleID,team_member=[New_p_team_member_info1,New_p_team_member_info2],teamtype=TeamType,teamstatus=?NEW_CREATE},
	store_teaminfo2ets(TeamInfo),
	update_role_teamid(InviteRoleID,TeamInfo#team_info.teamid),
	update_role_teamid(TarRoleID,TeamInfo#team_info.teamid),
	send_roleupdate_message_to_other_server(TarRoleID),
	send_roleupdate_message_to_other_server(InviteRoleID),
	broadcast_team_info(TeamInfo,#sc_generalteam_invite_response{inviteAccept=1,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=New_p_team_member_info2}),
	update_generalteam_info_by_teamID(TeamID).

%%处理玩家离开组队
do_leave_team(RoleID,TeamInfo)->
	case RoleID =:= TeamInfo#team_info.teamleader_roleid of
		true->
			do_leader_leave_team(RoleID,TeamInfo);
		false ->
			case lists:member(RoleID,TeamInfo#team_info.vice_teamleader_rolelist) of
				true->
					do_viceleader_leave_team(RoleID,TeamInfo);
				false->
					do_member_leave_team(RoleID,TeamInfo)
			end
	end.
	
do_leader_leave_team(RoleID,TeamInfo)->
	{_Result,NewTeamInfo} = do_member_leave_team(RoleID,TeamInfo),
	case length(NewTeamInfo#team_info.vice_teamleader_rolelist) =:= 0 of
		true->
			transfer_leader2normalmember(NewTeamInfo);
		false ->
			case length(NewTeamInfo#team_info.team_member) >0 of
				true->
					transfer_leader2viceleader(NewTeamInfo);
				false->
					?INFO("do_leader_leave_team failed :RoleID:~w TeamInfo:~w~n",[RoleID,TeamInfo]),
					{error,TeamInfo}
			end
	end.

do_viceleader_leave_team(RoleID,TeamInfo)->
	ViceLeaderList = TeamInfo#team_info.vice_teamleader_rolelist,
	case lists:member(RoleID,ViceLeaderList) of
		true->
			NewViceLeaderList = ViceLeaderList--[RoleID],
			NewTeamInfo = TeamInfo#team_info{vice_teamleader_rolelist=NewViceLeaderList},
			do_member_leave_team(RoleID,NewTeamInfo);
		false->
			?INFO("副队长离开队伍未在副队长列表：~w 中发现RoleID：~w~n",[ViceLeaderList,RoleID]),
			{error,TeamInfo}
	end.

do_member_leave_team(RoleID,TeamInfo)->
	{Result,NewTeamInfo} = delete_team_member(RoleID,TeamInfo),
	case length(NewTeamInfo#team_info.team_member) >1 of
		true->
			{Result,NewTeamInfo#team_info{teamstatus=?UPDATE_TEAM}};
		false->
			spawn(fun()->lists:foreach(fun(Member)->update_role_teamid(Member#p_team_member_info.roleID,-1) end,TeamInfo#team_info.team_member) end),
			{Result,NewTeamInfo#team_info{teamstatus=?RELEASE_BY_NUMBER}}
	end.

delete_team_member(RoleID,TeamInfo)->
	MemberList = TeamInfo#team_info.team_member,
	{FindOneList,Other} = lists:foldl(fun(Member,{FindAcc,OtherAcc})->
		case Member#p_team_member_info.roleID =/= RoleID of
			true->
				{FindAcc,[Member|OtherAcc]};
			false ->
				{[Member|FindAcc],OtherAcc}
		end
	end,{[],[]},MemberList),
	case length(FindOneList) =/= 0 of
		true->
			Result = ok,
			spawn(fun()->
			lists:foreach(fun(Member)->
				update_role_teamid(Member#p_team_member_info.roleID,-1)
			    end,FindOneList)
		    end),
			NewMemberList = adjust_member_list(Other);
		false->
			Result = error,
			NewMemberList = Other,
			?INFO("删除组队成员没有找到对应的成员 roleID：~w TeamInfo:~w ~n",[RoleID,TeamInfo])
	end,	
	{Result,TeamInfo#team_info{team_member=NewMemberList}}.

adjust_member_list(MemberList)->
	NewMemberList = lists:keysort(#p_team_member_info.position,MemberList),
	{NewMemberList1,_Index} = lists:foldl(fun(Member,{ListAcc,Acc})->
		{[Member#p_team_member_info{position = Acc}|ListAcc],Acc+1}
		end,{[],1},NewMemberList),
	NewMemberList1.

transfer_leader2normalmember(TeamInfo)->
	case get_position_one_member(TeamInfo) of
		?undefined->
			?INFO("未找到position位置为1的成员~n"),
			{error,TeamInfo};
		FindOne->
			{ok,TeamInfo#team_info{teamleader_roleid=FindOne#p_team_member_info.roleID}}
	end.

transfer_leader2viceleader(TeamInfo)->
	case TeamInfo#team_info.vice_teamleader_rolelist of
		[]->
			?INFO("转移队长给副队长，副队长列表为空 TeamInfo:~w ~n",[TeamInfo]),
			{error,TeamInfo};
		ViceLeaderList ->
			MemberList = TeamInfo#team_info.team_member,
			ViceLeaderMember = lists:foldl(fun(RoleID,Acc)->
				case lists:keyfind(RoleID,#p_team_member_info.roleID,MemberList) of
					false ->
						Acc;
					FindOne ->
						[FindOne|Acc]
				end
			end,[],ViceLeaderList),
			NewViceLeaderMember = lists:keysort(#p_team_member_info.position,ViceLeaderMember),
			FindOne = hd(NewViceLeaderMember),
			FindRoleID = FindOne#p_team_member_info.roleID,
			{ok,TeamInfo#team_info{teamleader_roleid=FindRoleID,vice_teamleader_rolelist=TeamInfo#team_info.vice_teamleader_rolelist--[FindRoleID]}}
	end.

get_position_one_member(TeamInfo)->
	case TeamInfo#team_info.team_member of
		[]->
			?INFO("出现成员列表为空~n"),
			?undefined;
		MemberList-> 
			case lists:keyfind(1,#p_team_member_info.position,MemberList) of
				false ->
					?INFO("找不到postion位置为1的成员~n"),
					MemberList1 = lists:keysort(#p_team_member_info.position,MemberList),
					hd(MemberList1);
				FindOne->
					FindOne
			end
	end.

%%产生一个随机的组队号
generate_team_id()->
	tk_id:gen_teamID().

%%向组队中的所有玩家发送组队更新消息
update_generalteam_info_by_teamID(TeamID)->
	case get_team_info_by_teamid(TeamID) of
		?undefined->
			ignore;
		TeamInfo->
			update_generalteam_info(TeamInfo)
	end.

update_generalteam_info(TeamInfo)->
	TeamMemberList = TeamInfo#team_info.team_member,
	Status= case TeamInfo#team_info.teamstatus of
		?RELEASE_BY_LEADER->
			2;
		?RELEASE_BY_NUMBER->
			3;
		_ ->
			1
	end,
	spawn(fun()->
		lists:foreach(fun(Member)->
					RoleID = Member#p_team_member_info.roleID,
					?unicast(RoleID,#update_generalteam_info{team_statu=Status,team_info=transform_team_info2p_team_info(TeamInfo)})
				end,TeamMemberList)
			end).
%%返回组队的下一个position
get_team_next_position(TeamInfo) when is_record(TeamInfo,team_info)->
	TeamMemberList = TeamInfo#team_info.team_member,
	NewTeamMemberList = lists:reverse(lists:keysort(#p_team_member_info.position,TeamMemberList)),
	TeamMember = hd(NewTeamMemberList),
	TeamMember#p_team_member_info.position+1.

%%判断玩家组队邀请有效性
check_invite(Role,TarRoleID)->
	#role{roleID=RoleID} = Role,
	#team_info{teamid=TeamID,teamtype=TeamType} = TeamInfo = case ets:lookup(?ETS_TEAM_LIST,Role#role.teamId) of
		[]->
			erlang:throw({error,1});
		[FindOne]->
			FindOne
	end,
	case check_role_right(RoleID,TeamInfo,?INVITE_MEMBER_AUTHORITY) of
		false ->
			erlang:throw({error,2});
		true->
			next
	end,
	case check_team_limit(TeamInfo,?TEAM_MEMBER_LIMIT) of
		false ->
			erlang:throw({error,3});
		true->
			next
	end,
	case role_lib:is_online(TarRoleID) of
		true->
			TarRoleInfo = role_lib:call_server(TarRoleID,get_role_info,1000),
			case is_in_team(TarRoleInfo) of
				false->
					{ok,6,TeamType,TeamID,TarRoleInfo};
				true ->
					erlang:throw({error,5})
			end;
		false ->
			erlang:throw({error,4})
	end.

get_team_info_by_teamid(-1)->
	?undefined;
get_team_info_by_teamid(ID)->
	case ets:lookup(?ETS_TEAM_LIST,ID) of
		[]->
			?INFO("未能找到组队信息 ID:~w ~n",[ID]),
			?undefined;
		[FindOne]->
			FindOne
	end.

check_role_right(RoleID,TeamInfo,Right)->
	case RoleID =:= TeamInfo#team_info.teamleader_roleid of
		true->
			lists:member(Right,data_team:get({data_team_right,1}));
		false->
			case lists:member(RoleID,TeamInfo#team_info.vice_teamleader_rolelist) of
				false ->
					lists:member(Right,data_team:get({data_team_right,3}));
				_FindOne ->
					lists:member(Right,data_team:get({data_team_right,2}))
			end
	end.

check_team_limit(TeamInfo,LimitType)->
	{_LeaderLimit,MaxVicLeaderLimit,MinVicLeaderNum,MaxMemberLimit,MinMemberLimit} = data_team:get({data_team_type,TeamInfo#team_info.teamtype}),
	case LimitType of
		?TEAM_MEMBER_LIMIT ->
			TeamMemberNum = length(TeamInfo#team_info.team_member),
			MaxMemberLimit > TeamMemberNum andalso TeamMemberNum >= MinMemberLimit;
		?VICE_TEAM_LEADER_LIMIT ->
			VicLeaderNum =length(TeamInfo#team_info.vice_teamleader_rolelist),
			MaxVicLeaderLimit > VicLeaderNum andalso VicLeaderNum>= MinVicLeaderNum;
		_->
			?INFO("出现没有处理的人数上线检查,查询类型：~w~n",[LimitType]),
			false
	end.

%%通过RoleID获得RoleInfo进而转换成p_team_member_info
get_p_team_member_info(RoleID)->
    IsRoleOnline = role_lib:is_online(RoleID),
    RoleInfo = 
        case IsRoleOnline of
            true ->
                catch role_lib:call_server(RoleID, get_role_info,1000);
            false ->
                db_sql:get_roleInfo(RoleID)
        end,
    transform_role2p_team_member_info(RoleInfo).

is_in_team(RoleInfo) when is_record(RoleInfo,role)->
	case get_team_info_by_teamid(RoleInfo#role.teamId) of
		?undefined->
			?INFO("RoleInfo:~w~n",[RoleInfo]),
			false;
		FindOne ->
			case FindOne#team_info.teamstatus =/= ?RELEASE_BY_NUMBER andalso FindOne#team_info.teamstatus =/= ?RELEASE_BY_LEADER of
				true->
					case lists:keyfind(RoleInfo#role.roleID,#p_team_member_info.roleID,FindOne#team_info.team_member) of
						false->
							false;
						_ ->
							true
					end;
				false->
					false
			end
	end;
is_in_team(RoleInfo)->
	?INFO("判断玩家是否有组队出现非role数据：~w ~n",[RoleInfo]),
	false.

is_in_team(RoleID,TeamInfo) when is_record(TeamInfo,team_info)->
	case TeamInfo#team_info.teamstatus =/= ?RELEASE_BY_NUMBER andalso TeamInfo#team_info.teamstatus =/= ?RELEASE_BY_LEADER of
		true->
			MemberList = TeamInfo#team_info.team_member,
			case lists:keyfind(RoleID,#p_team_member_info.roleID,MemberList) of
				false->
					false;
				_FindOne->
					true
			end;
		false->
			false
	end;
is_in_team(RoleID,TeamInfo)->
	?INFO("判断玩家是否有组队出现非team_info数据：RoleID:~w TeamInfo:~w ~n",[RoleID,TeamInfo]),
	false.

is_in_team_by_roleID_and_teamID(RoleID,TeamID)->
	case get_team_info_by_teamid(TeamID) of
		?undefined->
			false;
		TeamInfo->
			is_in_team(RoleID,TeamInfo)
	end.

update_role_teamid(RoleID,TeamID)->
	?INFO("更新roleID:~w TeamID:~w ~n",[RoleID,TeamID]),
	case catch role_lib:send_server(RoleID, {update_teamid, TeamID}) of
				{'EXIT',_}->
                    %% 玩家不在线则执行sql操作
                    db_sql:update_role_teamid(RoleID, TeamID);
				_ ->
					ignore
	end.

update_invite_remian_time(RoleID,InviteRoleID,TeamID,TeamType)->
	RemainTimeInterval = data_team:get(data_team_invite_interval),
	case catch role_lib:send_server(RoleID, {update_team_invite_remain_time, {util:now()+RemainTimeInterval,InviteRoleID,TeamID,TeamType}}) of
				{'EXIT',_}->
					?INFO("向玩家RoleID：~w 更新邀请有效时间出现异常~n",[RoleID]);
				_ ->
					ignore
	end.

%%根据组队信息的状态，判断是更新TeamInfo还是删除TeamInfo,如果队伍解散了，向卡洛斯进程发送解散消息
store_teaminfo2ets(TeamInfo) when is_record(TeamInfo,team_info)->
	case TeamInfo#team_info.teamstatus of
		?DELETE_AFTER_PERSIST->
			ets:delete(?ETS_TEAM_LIST,TeamInfo#team_info.teamid);
		?EXIST->
			?INFO("出现存储exist状态组队信息：oldTeamInfo:~w NewTeamInfo:~w ~n",[ets:lookup(?ETS_TEAM_LIST,TeamInfo#team_info.teamid),TeamInfo]),
			ets:insert(?ETS_TEAM_LIST,TeamInfo);
		_ ->
			% send_teamupdate_message_to_other_server(TeamInfo),                  
			ets:insert(?ETS_TEAM_LIST,TeamInfo)
	end;
store_teaminfo2ets(TeamInfo)->
	?INFO("保存TeamInfo出现非team_info结构 ~w ~n",[TeamInfo]),
	ignore.

get_invite_timeout_ref({InviteRoleID,TarRoleID})->
	case get(?invite_timeout_ref) of
		?undefined->
			{?undefined,[]};
		TimeRefList->
			FindOne = [Elem||{EInviteRoleID,ETarRoleID,_Ref} = Elem <-TimeRefList,EInviteRoleID=:=InviteRoleID,ETarRoleID=:=TarRoleID],
			case FindOne of
				[]->
					{?undefined,TimeRefList};
				_->
					[H|_]=FindOne,
					{H,TimeRefList--FindOne}
			end
	end.

%%添加新的超时,如果查找到旧的超时记录，则不添加新的  返回false，否则true
add_invite_timeout_ref({InviteRoleID,TarRoleID,TimeRef})->
	case get_invite_timeout_ref({InviteRoleID,TarRoleID}) of
		{?undefined,TimeRefList}->
			put(?invite_timeout_ref,[{InviteRoleID,TarRoleID,TimeRef}|TimeRefList]),
			true;
		{_FindOne,_OtherTimeRefList}->
			?INFO("发现InviteRoleID:~w TarRoleID:~w TimeRef:~w ~n",[InviteRoleID,TarRoleID,TimeRef]),
			erlang:cancel_timer(TimeRef),
			false
			% {_InviteRoleID,_TarRoleID,OldTimeRef} = FindOne,
			% erlang:cancel_timer(OldTimeRef),
			% put(?invite_timeout_ref,[{InviteRoleID,TarRoleID,TimeRef}|OtherTimeRefList])
	end.

del_invite_timeout_ref({InviteRoleID,TarRoleID})->
	case get_invite_timeout_ref({InviteRoleID,TarRoleID}) of
		{?undefined,TimeOutRef}->
			?INFO("未找到InviteRoleID：~w TarRoleID:~w 的TimeOutRef~n",[InviteRoleID,TarRoleID]),
			put(?invite_timeout_ref,TimeOutRef);
		{FindOne,OtherTimeRefList}->
			{_InviteRoleID,_TarRoleID,TimeOutRef} = FindOne,
			erlang:cancel_timer(TimeOutRef),
			put(?invite_timeout_ref,OtherTimeRefList)
	end.

change_authority(Type,TarRoleID,TeamInfo)->
	case Type of
		?APPOINT_TEAM_LEADER_AUTHORITY->
			#team_info{vice_teamleader_rolelist=OldViceTeamLeaderRoleList}=TeamInfo,
			{ok,TeamInfo#team_info{teamleader_roleid=TarRoleID,vice_teamleader_rolelist=OldViceTeamLeaderRoleList--[TarRoleID]}};
		?APPOINT_VICE_TEAM_LEADER_AUTHORITY->
			case check_team_limit(TeamInfo,?VICE_TEAM_LEADER_LIMIT) of
				true->
					OldViceTeamLeaderRoleList = TeamInfo#team_info.vice_teamleader_rolelist,
					case lists:member(TarRoleID,OldViceTeamLeaderRoleList) of
						false->
							{ok,TeamInfo#team_info{vice_teamleader_rolelist=[TarRoleID|OldViceTeamLeaderRoleList]}};
						true->
							?INFO("转移副队长，被转移玩家已经在副队长队列中TarRoleID:~w TeamInfo:~w ~n",[TarRoleID,TeamInfo]),
							{ok,TeamInfo}
					end;
				false->
					{error,TeamInfo}
			end;
		?CANCEL_VICE_TEAM_LEADER_AUTHORITY->
			OldViceTeamLeaderRoleList = TeamInfo#team_info.vice_teamleader_rolelist,
			case lists:member(TarRoleID,OldViceTeamLeaderRoleList) of
				true->
					{ok,TeamInfo#team_info{vice_teamleader_rolelist=OldViceTeamLeaderRoleList--[TarRoleID]}};
				false->
					?INFO("撤销副队长权限出现TarRoleID：~w 不在副队长列表中：TeamInfo:~w ~n",[TarRoleID,TeamInfo]),
					{error,TeamInfo}
			end;
		_ ->
			?INFO("出现未定义的权限转移类型：Type：~w TarRoleID:~w TeamInfo: ~w ~n",[Type,TarRoleID,TeamInfo]),
			{error,TeamInfo}
	end.

broadcast_team_meesage(Message,RoleInfo,TeamInfo)->
	case lists:keyfind(RoleInfo#role.roleID,#p_team_member_info.roleID,TeamInfo#team_info.team_member) of
		false->
			?INFO("广播组队聊天时，在team_member中没有找到发送消息玩家信息 RoleID:~w team_info:~w ~n",[RoleInfo#role.roleID,TeamInfo]),
			ignore;
		FindOne->
			lists:foreach(fun(Member)->?unicast(Member#p_team_member_info.roleID,#update_generalteam_talk{messageRole=FindOne,message=Message}) end,TeamInfo#team_info.team_member)
	end.

broadcast_team_info(TeamInfo,Message)->
	lists:foreach(fun(Member)->?unicast(Member#p_team_member_info.roleID,Message) end,TeamInfo#team_info.team_member).

%%对数据库中读取的组队信息进行处理，队长不在的情况下，默认组队不存在
deal_teaminfo_list(TeamInfoList)->
	lists:foldl(fun(TeamInfo,Acc)->
		case role_lib:is_online(TeamInfo#team_info.teamleader_roleid) of
			false->
				Acc;
			true->
				NewTeamInfo = delete_vice_role(TeamInfo),
				NewTeamInfo2 = delete_member(NewTeamInfo),
				case length(NewTeamInfo2#team_info.team_member) >1 of
					true->
						[NewTeamInfo2|Acc];
					false->
						Acc
				end
		end
	end,[],TeamInfoList).

%%删除不在线的副队长
delete_vice_role(TeamInfo)->
	NewViceRoleList = lists:foldl(fun(ViceRoleID,Acc)->
		case role_lib:is_online(ViceRoleID) of
			true->
				[ViceRoleID|Acc];
			false->
				Acc
		end
	end,[],TeamInfo#team_info.vice_teamleader_rolelist),
	TeamInfo#team_info{vice_teamleader_rolelist=NewViceRoleList}.

%%删除不在线的成员
delete_member(TeamInfo)->
	NewMemberList = lists:foldl(fun(Member,Acc)->
		case role_lib:is_online(Member#p_team_member_info.roleID) of
			true->
				[Member|Acc];
			false->
				Acc
		end
	end,[],TeamInfo#team_info.team_member),
	TeamInfo#team_info{team_member=NewMemberList}.

update_teamid_by_teaminfo(TeamInfo)->
	lists:foreach(fun(Member)->
		update_role_teamid(Member#p_team_member_info.roleID,TeamInfo#team_info.teamid)
	end,TeamInfo#team_info.team_member).

update_level(Role,NewLevel) when is_record(Role,role)->
	case get_team_info_by_teamid(Role#role.teamId) of
		?undefined->
			ignore;
		TeamInfo->
			case lists:keytake(Role#role.roleID,#p_team_member_info.roleID,TeamInfo#team_info.team_member) of
				{_,FindOne,Other}->
					NewFindone = FindOne#p_team_member_info{level=NewLevel},
					NewTeamInfo = TeamInfo#team_info{team_member=[NewFindone|Other]},
					store_teaminfo2ets(NewTeamInfo);
				false ->
					?INFO("因为等级变化更新组队信息时,没有在组队信息中找到玩家信息：teaminfo:~w Role:~w ~n",[TeamInfo,Role])
			end
	end.

update_roleinfo(RoleInfo)->
	case get_team_info_by_teamid(RoleInfo#role.teamId) of
		?undefined->
			ignore;
		TeamInfo->
			case lists:keytake(RoleInfo#role.roleID,#p_team_member_info.roleID,TeamInfo#team_info.team_member) of
				{_,FindOne,Other}->
					#role{level=Level,fightPower=FightPower,roleName=RoleName,head=Head,isMale=IsMale,title=Title} = RoleInfo,
					NewFindone = FindOne#p_team_member_info{roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,fightPower=FightPower},
					NewTeamInfo = TeamInfo#team_info{team_member=[NewFindone|Other]},
					store_teaminfo2ets(NewTeamInfo);
				false ->
					?INFO("因为等级变化更新组队信息时,没有在组队信息中找到玩家信息：teaminfo:~w Role:~w ~n",[TeamInfo,RoleInfo])
			end
	end.

%%队伍解散之后向其他服务器进程发送解散消息
send_teamupdate_message_to_other_server(#team_info{teamid=TeamID, team_member=Members,teamleader_roleid=TeamLeaderRoleID}=TeamInfo)->
	case carlos_server:is_team_sign(TeamID, erlang:length(Members)) of
		false ->
			?INFO("向卡洛斯服务器更新信息，判断组队没有报名卡洛斯：TeamInfo:~w ~n",[TeamInfo]),
			ignore;
		{true,ID}->
            catch erlang:send(carlos_server,{unrequest,ID})
	end,
    case relic_server:is_team_sign(TeamID, erlang:length(Members)) of
        false ->
            ?INFO("向巨龙遗迹服务器更新信息，判断组队没有报名卡洛斯：~w TeamInfo:~w ~n",[TeamID,TeamInfo]),
            ignore;
        {true,RID}->
            catch erlang:send(relic_server, {unrequest, RID})
    end,
	case galactica_server:is_team_sign(TeamID, erlang:length(Members)) of 
		false ->
			ignore;
		{true,IDG} ->
			catch erlang:send(galactica_server,{unrequest,IDG})
	end,
	case twins_server:is_team_sign(TeamID,erlang:length(Members)) of
		false ->
			ignore;
		{true,IDTT} ->
			catch erlang:send(twins_server, {unrequest, IDTT})
	end,
	case doublematch_server:doublematch_state(TeamLeaderRoleID) of
        ?match_state_standby->
            ignore;
        ?match_state_sign ->
            catch role_doublematch:do_sign_cancle(TeamLeaderRoleID)
    end,
    case conquerisland_server:is_team_sign(TeamID,erlang:length(Members)) of
    	false->
    		ignore;
    	{true,CID}->
    		catch erlang:send(conquerisland_server,{unrequest,CID})
    end,
    erlang:send(tasklink_server,{cs_tasklink_ready_opt,TeamLeaderRoleID,2}).

send_roleupdate_message_to_other_server(RoleID)->
	?INFO("RoleInfo update to other server RoleID:~w ~n",[RoleID]),
	case carlos_server:is_role_sign(RoleID) of
		false ->
			?INFO("向卡洛斯服务器更新信息，判断组队没有报名卡洛斯：RoleID:~w ~n",[RoleID]),
			ignore;
		{true,ID}->
			catch erlang:send(carlos_server,{unrequest,ID})
	end,
    case relic_server:is_role_sign(RoleID) of
        false ->
            ?INFO("向巨龙遗迹服务器更新信息，判断组队没有报名卡洛斯：RoleID:~w ~n",[RoleID]),
            ignore;
        {true,RID}->
            ?INFO("team unrequest ~w ~w ~n",[RoleID,RID]),
            ServerID = data_setting:get(server_id),
            lists:foreach(fun(LevelRank)->
                                  send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {unrequest, ServerID, RID})
                          end, lists:seq(1, 5))
    end,
	case galactica_server:is_role_sign(RoleID) of
		false ->
			ignore;
		{true,IDG} ->
			catch erlang:send(galactica_server, {unrequest, IDG})
	end,
	case twins_server:is_role_sign(RoleID) of
		false ->
			ignore;
		{true, IDTT} ->
			catch erlang:send(twins_server, {unrequest, IDTT})
	end,
	case doublematch_server:doublematch_state(RoleID) of
        ?match_state_standby->
            ignore;
        ?match_state_sign ->
            catch role_doublematch:do_sign_cancle(RoleID)
    end,
    case conquerisland_server:is_role_sign(RoleID) of
    	false->
    		ignore;
    	{true,CID}->
    		catch erlang:send(conquerisland_server,{unrequest,CID})
    end,
    erlang:send(tasklink_server,{cs_tasklink_ready_opt,RoleID,2}).

%%将RoleInfo转换成p_team_member_info
transform_role2p_team_member_info(RoleInfo) when is_record(RoleInfo,role)->
	#role{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,fightPower=FightPower} = RoleInfo,
    #p_team_member_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,head=Head,title=Title,level=Level,fightPower=FightPower,position=-1};
transform_role2p_team_member_info(_RoleInfo)->
	?INFO("出现非role结构转换成p_team_member_info~n"),
	#p_team_member_info{}.	

%%将team_info转换成p_team_info, 同时由于team_info的memberlist中p_team_member_info保存的position可能是初始值，所以需要使用position修改
transform_team_info2p_team_info(TeamInfo) when is_record(TeamInfo,team_info)->
	#team_info{teamid=TeamID,teamleader_roleid=LeaderID,vice_teamleader_rolelist=ViceLeaderList,team_member=TeamMemberList,teamtype=TeamType} = TeamInfo,
	#p_team_info{teamType=TeamType,teamID=TeamID,leaderID=LeaderID,viceleaderIDList=ViceLeaderList,memberList=TeamMemberList};
transform_team_info2p_team_info(_TeamInfo)->
	#p_team_info{}.

%%将队伍的队伍类型转换成其他的类型，其中除去队长外，其他的权限全部去除，只保留为队员权限,检查人数
change_team_type(TeamInfo,NewTeamType)->
	case data_team:get({data_team_type,NewTeamType}) of
		?undefined->
			false;
		{_LeaderNum,_MaxVicLeaderNum,_MinVicLeaderNum,MaxMemberNum,MinMemberNum}->
			#team_info{teamleader_roleid=LeaderRoleID,team_member=TeamMemberList} = TeamInfo,
			% VicLeaderNum = length(VicLeaderRoleIDList),
			TeamMemberNum = length(TeamMemberList),
			case LeaderRoleID > 0 andalso TeamMemberNum>=MinMemberNum andalso TeamMemberNum =<MaxMemberNum of
				false->	
					false;
				true-> 
					NewTeamInfo = TeamInfo#team_info{vice_teamleader_rolelist=[],teamstatus=?UPDATE_TEAM,teamtype=NewTeamType},
					store_teaminfo2ets(NewTeamInfo),
					update_generalteam_info(NewTeamInfo),
					NewTeamInfo
			end
	end.

check_role_change_teamtype_authority(RoleID,TeamInfo)->
	check_role_right(RoleID,TeamInfo,?CHANGE_TEAM_TYPE_AUTHORITY).

send_teamupdate_message_to_special_team(TeamInfo)->
	#team_info{teamid=TeamID, team_member=_Members,teamstatus=TeamStatu}=TeamInfo,
	case TeamStatu of
		?RELEASE_BY_NUMBER->
			role_doublematch:send_deleteteaminfo_from_team_manage(TeamID);
		?RELEASE_BY_LEADER->
			role_doublematch:send_deleteteaminfo_from_team_manage(TeamID);
		_ ->
			ignore
	end.

change_special_team_authority(Type,TarRoleID,TeamInfo)->
	#team_info{teamid=TeamID} = TeamInfo,
	case Type of
		?APPOINT_TEAM_LEADER_AUTHORITY->
			role_doublematch:change_leader(TarRoleID,TeamID);
		?APPOINT_VICE_TEAM_LEADER_AUTHORITY->
			ignore;
		?CANCEL_VICE_TEAM_LEADER_AUTHORITY->
			ignore;
		_ ->
			ignore
	end.

%%由于判断当前阶段是否有以其他的特殊组队模块不允许队伍权限更改
check_special_changeauthority_limit(Type,TarRoleID,TeamInfo)->
	#team_info{teamid=TeamID} = TeamInfo,
	role_doublematch:check_doublematch_changeauthority_limit(Type,TarRoleID,TeamID).
%%test---------------------------------------------------------------------------	


