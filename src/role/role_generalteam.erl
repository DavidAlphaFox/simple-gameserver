%% @author zcl
%% @doc 通用组队系统
%% Created 2015-03-28
-module(role_generalteam).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").

-define(INVITEACCEPT,1).                                       %%接受组队请求
-define(INVITEREFUSE,2).                                       %%拒绝组队请求
-define(IS_IN_TEAM,3).                                         %%已经组队中
-define(last_team_talk_time,last_team_talk_time).              %%保存上次发送聊天信息的时间戳
-define(invite_remain_time,invite_remain_time).                %%邀请有效时间戳
-define(last_message,last_message).                            %%上次的消息
-define(last_message_time,last_message_time).     
-define(last_update_info_time,last_update_info_time).

-define(NORMAL_TEAM_TYPE,1).
-define(DOUBLEMATCH_TEAM_TYPE,2).                       
%% ====================================================================
%% API functions
%% ====================================================================

%%处理组队请求
cs_generalteam_create(#cs_generalteam_create{tarRoleID=TarRoleID,teamType=TeamType}=Msg)->
  case check_last_message_interval(Msg) of
    true->
      ?INFO("check true~n"),
      case role_lib:is_online(TarRoleID) of  
        false ->
          ?sendself(#sc_generalteam_create{result=2});
        true ->
          RoleInfo = role_data:get_roleInfo(),
          case team_manage_server:is_in_team(RoleInfo) of
            true->
              ?sendself(#sc_generalteam_create{result=4});
            false->
              TarRoleInfo = role_lib:call_server(TarRoleID, get_role_info,1000),
              case team_manage_server:is_in_team(TarRoleInfo) of
                true->
                  ?sendself(#sc_generalteam_create{result=3});
                false->
                  CreateRole = role_data:get_roleInfo(),
                  erlang:send(team_manage_server,{createteam,CreateRole,TarRoleID,TeamType})
              end
          end
      end;
    false->
      ?INFO("check false~n"),
      ignore
  end.

cs_generalteam_invite(#cs_generalteam_invite{tarRoleID=TarRoleID}=Msg)->
  case check_last_message_interval(Msg) of
    true->
      RoleInfo = role_data:get_roleInfo(),
      case team_manage_server:is_in_team(RoleInfo) of
        false ->
          ?sendself(#sc_generalteam_invite{result=1});
        true->
          case role_lib:is_online(TarRoleID) of
            false ->
              ?sendself(#sc_generalteam_invite{result=4});
            true->
              Role = role_data:get_roleInfo(),
              erlang:send(team_manage_server,{teaminvite,Role,TarRoleID})
          end
      end;
    false->
      ignore
  end.

cs_generalteam_invite_response(#cs_generalteam_invite_response{inviteAccept=InviteAccept,teamID=TeamID,inviteRoleID=InviteRoleID,teamType=TeamType,teamStatu=TeamStatu})->
  RoleInfo= role_data:get_roleInfo(),
  case check_invite_time(InviteRoleID,TeamID,TeamType) of
    true->
      del_invite_remian_time(InviteRoleID),
      case team_manage_server:is_in_team(RoleInfo) of
        false->
          case role_lib:is_online(InviteRoleID) of
            true->
              erlang:send(team_manage_server,{inviteresponse,InviteAccept,TeamID,InviteRoleID,TeamType,TeamStatu,RoleInfo});
            false->
              ?sendself(#sc_generalteam_invite_response{inviteAccept=4,teamID=TeamID,inviteRoleID=InviteRoleID,invitedRole=team_manage_server:transform_role2p_team_member_info(RoleInfo)})
          end;
        true->
          erlang:send(team_manage_server,{inviteresponse,?IS_IN_TEAM,TeamID,InviteRoleID,TeamType,TeamStatu,RoleInfo})
      end; 
    false->
      ignore
  end.

cs_generalteam_leaveteam(#cs_generalteam_leaveteam{})->
  RoleInfo = role_data:get_roleInfo(),
  case team_manage_server:is_in_team(RoleInfo) of
    false->
      ?sendself(#sc_generalteam_leaveteam{result=2,roleID=RoleInfo#role.roleID});
    true->
      erlang:send(team_manage_server,{leaveteam,RoleInfo})
  end.

cs_generalteam_disbandteam(#cs_generalteam_disbandteam{})->
  RoleInfo = role_data:get_roleInfo(),
  case team_manage_server:is_in_team(RoleInfo) of
    false->
      ?sendself(#sc_generalteam_disbandteam{result=2});
    true->
      erlang:send(team_manage_server,{disbandteam,RoleInfo})
  end.

cs_generalteam_kick(#cs_generalteam_kick{kickRoleID=KickRoleID})->
  RoleInfo = role_data:get_roleInfo(),
  case team_manage_server:is_in_team(RoleInfo) of
    false ->
      ?sendself(#sc_generalteam_kick{result=2,kickRoleID=KickRoleID,roleID=0});
    true->
      case KickRoleID =:= RoleInfo#role.roleID of
        true->
          ?sendself(#sc_generalteam_kick{result=5,kickRoleID=KickRoleID});
        false->
           erlang:send(team_manage_server,{kickmember,KickRoleID,RoleInfo})
      end
  end.

cs_generalteam_change_authority(#cs_generalteam_change_authority{type=Type,tarRoleID=TarRoleID})->
  RoleInfo = role_data:get_roleInfo(),
  case team_manage_server:is_in_team(RoleInfo) of
    false ->
      ?sendself(#sc_generalteam_change_authority{result=2,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID});
    true->
      case TarRoleID =:= RoleInfo#role.roleID of
        true->
          ?sendself(#sc_generalteam_change_authority{result=5,tarRoleID=TarRoleID,type=Type,roleID=RoleInfo#role.roleID});
        false->
           erlang:send(team_manage_server,{changeauthority,Type,RoleInfo,TarRoleID})
      end
  end.

cs_generalteam_talk(#cs_generalteam_talk{message=Message0})->
	Message = util:words_filter(Message0),
  case check_message_length(Message) of
    {error,Reason}->
      ?sendself(#sc_generalteam_talk{result=Reason});
    ok->
      case check_talk_time() of
        false->
          ?sendself(#sc_generalteam_talk{result=5});
        true->
          RoleInfo = role_data:get_roleInfo(),
          case team_manage_server:is_in_team(RoleInfo) of
            false->
              ?sendself(#sc_generalteam_talk{result=2});
            true->
              erlang:send(team_manage_server,{teamtalk,Message,RoleInfo})
          end
      end
  end.

cs_generalteam_info(#cs_generalteam_info{})->
  RoleInfo = role_data:get_roleInfo(),
  case RoleInfo#role.teamId =/= -1 of
    true ->
      case team_manage_server:is_in_team(RoleInfo) of
        true->
          TeamInfo = team_manage_server:get_team_info_by_teamid(RoleInfo#role.teamId),
          ?sendself(#sc_generalteam_info{result=2,teamInfo=team_manage_server:transform_team_info2p_team_info(TeamInfo)});
        false->
          ?sendself(#sc_generalteam_info{result=1,teamInfo=team_manage_server:transform_team_info2p_team_info(#team_info{})})
      end;
    false->
      ?sendself(#sc_generalteam_info{result=1,teamInfo=team_manage_server:transform_team_info2p_team_info(#team_info{})})
  end.

cs_generalteam_change_teamtype(#cs_generalteam_change_teamtype{oldTeamType=OldTeamType,newTeamType=NewTeamType})->
    RoleInfo = role_data:get_roleInfo(),
    case catch do_change_teamtype(OldTeamType,NewTeamType,RoleInfo) of
        {false,Reason}->
            ?sendself(#sc_generalteam_change_teamtype{result=Reason,newTeamType=NewTeamType});
        true->
            ?sendself(#sc_generalteam_change_teamtype{result=1,newTeamType=NewTeamType})
    end.
%%---------------------------------------internal functions---------------------------------------

do_change_teamtype(OldTeamType,NewTeamType,RoleInfo)->
    case OldTeamType =:= NewTeamType of
      true->
        erlang:throw({false,7});
      false->
        next
    end,
    case team_manage_server:is_in_team(RoleInfo) of
        false->
            erlang:throw({false,2});
        true->
            next
    end,
    TeamInfo = team_manage_server:get_team_info_by_teamid(RoleInfo#role.teamId),
    case TeamInfo#team_info.teamtype=:=OldTeamType of
      true->
        next;
      false->
        erlang:throw({false,3})
    end,
    case data_team:get({data_team_type,NewTeamType}) of
      ?undefined->
        erlang:throw({false,4});
      _ ->
        next
    end,
    case team_manage_server:check_role_change_teamtype_authority(RoleInfo#role.roleID,TeamInfo) of
        false->
          erlang:throw({false,5});
        true->
          next
    end,
    case team_manage_server:change_team_type(TeamInfo,NewTeamType) of
        false->
          erlang:throw({false,6});
        _->
          #role{roleID=RoleID}=RoleInfo,
          create_and_delete_special_teaminfo(OldTeamType,NewTeamType,TeamInfo,RoleID),
          true
    end.

check_message_length(Message)->
  case erlang:length(Message) >0 of
    true->
      case erlang:length(Message) =< data_team:get(data_talk_length_limit) of
        true->
          ok;
        false->
          {error,4}
      end;
    false->
      {error,3}
  end.

check_talk_time()->
  LastTime = get_last_team_talk_time(),
  Now = util:now(),
  case Now >= LastTime + data_team:get(data_talk_interval) of
    true->
      set_last_team_talk_time(Now),
      true;
    false->
      false
  end.

check_invite_time(InviteRoleID,TeamID,TeamType)->
  % {TimeStamp,StoreInviteRoleID,StoreTeamID,StoreTeamType} = get_invite_remain_time(),
  case get_invite_remain_time() of
    []->
      false;
    TimeList->
      case lists:keyfind(InviteRoleID,2,TimeList) of
        false->
          false;
        {TimeStamp,StoreInviteRoleID,StoreTeamID,StoreTeamType}->
          TimeStamp >= util:now() andalso StoreInviteRoleID=:=InviteRoleID andalso StoreTeamID=:=TeamID andalso StoreTeamType=:=TeamType
      end
  end.

leave_team()->
  RoleInfo = role_data:get_roleInfo(),
  ?INFO("leave RoleInfo:~w ~n",[RoleInfo]),
  case team_manage_server:is_in_team(RoleInfo) of
    false->
      role_data:set_roleInfo(RoleInfo#role{teamId=-1}),
      team_manage_server:send_roleupdate_message_to_other_server(RoleInfo#role.roleID);
    true->
      erlang:send(team_manage_server,{leaveteam,RoleInfo})
  end.

update_level(Role,NewLevel)->
  case check_update_info_time() of
    true->
      ?INFO("update_level~n"),
      team_manage_server:update_level(Role,NewLevel);
    false->
      ?INFO("check_update_info_time failed~n"),
      ignore
  end.

update_roleinfo(RoleInfo) when is_record(RoleInfo,role)->
  case check_update_info_time() of
    true->
      ?INFO("update roleinfo RoleInfo:~w ~n",[RoleInfo]),
      team_manage_server:update_roleinfo(RoleInfo);
    false->
      ?INFO("check_update_info_time failed~n")
  end;
update_roleinfo(RoleInfo)->
  ?INFO("update_roleinfo 出现非role record RoleInfo:~w ~n",[RoleInfo]).

check_update_info_time()->
  util:now() >= get_last_update_info_time()+data_team:get(data_team_update_info_interval).

check_last_message_interval(Msg)->
  case get_last_message() of
    ?undefined->
      set_last_message(Msg),
      set_last_message_time(util:now()),
      true;
    OldMsg ->
      case Msg=:=OldMsg of
        true->
          Now = util:now(),
          case Now >= get_last_message_time() + data_team:get(data_team_last_message_interval) of
            true->
              set_last_message_time(Now),
              set_last_message(Msg),
              true;
            false->
              false
          end;
        false->
          set_last_message(Msg),
          set_last_message_time(util:now()),
          true
      end
  end.



%%==================================================================================================

get_last_message()->
  get(?last_message).

set_last_message(Msg)->
  put(?last_message,Msg).

get_last_message_time()->
  case get(?last_message_time) of
    ?undefined->
      0;
    X ->
      X
  end.
set_last_message_time(Time)->
  put(?last_message_time,Time).

get_last_update_info_time()->
  case get(?last_update_info_time) of
    ?undefined->
      0;
    X ->
      X
  end.

set_last_update_info_time(Time)->
  put(?last_update_info_time,Time).

get_last_team_talk_time()->
  case erlang:get(?last_team_talk_time) of
    ?undefined ->
      0;
    X ->
      X
  end.

set_last_team_talk_time(TimeStamp)->
  erlang:put(?last_team_talk_time,TimeStamp).

set_invite_remain_time(Msg)->
  {_TimeStamp,InviteRoleID,_TeamID,_TeamType} = Msg,
  NewReamainTime = case get_invite_remain_time() of
    []->
      [Msg];
    TimeList ->
      case lists:keytake(InviteRoleID,2,TimeList) of
        {_,_FindOne,Other}->
          [Msg|Other];
        false ->
          [Msg|TimeList]
      end
  end,
  erlang:put(?invite_remain_time,NewReamainTime).

get_invite_remain_time()->
  case erlang:get(?invite_remain_time) of
    ?undefined ->
      [];
    X ->
      X
  end.

del_invite_remian_time(InviteRoleID)->
  TimeList = get_invite_remain_time(),
  NewTimeList = [FindOne||{_TimeStamp,FindInviteRoleID,_TeamID,_TeamType}=FindOne<-TimeList,FindInviteRoleID=/=InviteRoleID],
  erlang:put(?invite_remain_time,NewTimeList). 

delet_outdata_remain_time()->
  TimeList = get_invite_remain_time(),
  Now = util:now(),
  NewTimeList = [FindOne||{TimeStamp,_FindInviteRoleID,_TeamID,_TeamType}=FindOne<-TimeList,TimeStamp >=Now],
  erlang:put(?invite_remain_time,NewTimeList).

%%此处调用其他模块接口创建对应的特殊队伍信息
create_and_delete_special_teaminfo(OldTeamType,NewTeamType,TeamInfo,RoleID)->
  create_special_teaminfo(NewTeamType,TeamInfo,RoleID),
  delete_special_teaminfo(OldTeamType,TeamInfo,RoleID).

create_special_teaminfo(NewTeamType,TeamInfo,RoleID)->
  case NewTeamType of
    ?NORMAL_TEAM_TYPE->
      ignore;
    ?DOUBLEMATCH_TEAM_TYPE->
      role_doublematch:create_teaminfo_by_normal_team(TeamInfo,RoleID);
    _ ->
      ?ERR("undefined TeamType:~w ~n",[NewTeamType]),
      ignore
  end.
delete_special_teaminfo(OldTeamType,TeamInfo,RoleID)->
  case OldTeamType of
    ?NORMAL_TEAM_TYPE->
      ignore;
    ?DOUBLEMATCH_TEAM_TYPE->
      role_doublematch:delete_teaminfo_by_normal_team(TeamInfo,RoleID);
    _ ->
      ?ERR("undefined TeamType:~w ~n",[OldTeamType]),
      ignore
  end.