-module(family_misc).


-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").

-export([
         make_family_process_name/1,
         register_family/1,
         router_to_family_process/2,
		 router_to_family_boss_process/2,
         to_p_family_info/1,
         to_p_family_member_info/1,
         to_p_family_summary/1,
         to_p_family_request/1,
         gen_p_family_info/0,
         lock_family_protect/2,
         unlock_family_protect/1
         ]).

%% 获取联盟进程名称
make_family_process_name(FamilyID) ->
    erlang:list_to_atom(lists:concat(["family_", FamilyID])).

make_family_boss_process_name(FamilyID) ->
	erlang:list_to_atom(lists:concat(["family_boss_",FamilyID])).

%%启动联盟进程
register_family(FamilyID) ->
    case FamilyID > 0 of
        true ->
            case supervisor:start_child(family_sup,
                                        {make_family_process_name(FamilyID) , 
                                         {family_server, start_link, [FamilyID]},
                                         transient, 300000, worker, [family_server]}) of
                {ok, _PID} ->
                    ?DEBUG("~ts:~w", ["联盟进程启动成功", FamilyID]),
                    ok;
                {error,{already_started,_}} ->
                    ?DEBUG("~ts:~w", ["联盟进程已经启动", FamilyID]),
                    ok;
                {error, Reason} ->
                    ?ERR("~ts:~w", ["启动联盟进程失败", Reason]),
                    {error, Reason}
            end;
        false ->
            ignore
    end.

%%将消息路由到联盟进程
router_to_family_process(FamilyID, Info) when is_integer(FamilyID) ->
    ProcessName = make_family_process_name(FamilyID),
    case erlang:whereis(ProcessName) of
        undefined ->
            register_family(FamilyID),
            do_send_to_family_process(ProcessName, Info, FamilyID);
        PID ->
            erlang:send(PID, Info),
            ok
    end.
do_send_to_family_process(ProcessName, Info, FamilyID) when is_atom(ProcessName) ->
    case erlang:whereis(ProcessName) of
        undefined ->
            if
                FamilyID > 0 ->
                    ?ERR("~ts:~w ~w", ["没有找到联盟进程", ProcessName, Info]);
                true ->
                    ignore
            end,
            fail;
        _ ->
            erlang:send(ProcessName, Info),
            ok
    end.

%% 启动联盟boss进程
register_family_boss(FamilyID) ->
    case FamilyID > 0 of
        true ->
            case supervisor:start_child(family_boss_sup,
                                        {make_family_boss_process_name(FamilyID) , 
                                         {family_boss_server, start_link, [FamilyID]},
                                         transient, 300000, worker, [family_boss_server]}) of
                {ok, _PID} ->
                    ?DEBUG("~ts:~w", ["联盟boss进程启动成功", FamilyID]),
                    ok;
                {error,{already_started,_}} ->
                    ?DEBUG("~ts:~w", ["联盟boss进程已经启动", FamilyID]),
                    ok;
                {error, Reason} ->
                    ?ERR("~ts:~w", ["启动boss联盟进程失败", Reason]),
                    {error, Reason}
            end;
        false ->
            ignore
    end.

%%将消息路由到联盟进程
router_to_family_boss_process(FamilyID, Info) when is_integer(FamilyID) ->
    ok.
%%     ProcessName = make_family_boss_process_name(FamilyID),
%%     case erlang:whereis(ProcessName) of
%%         undefined ->
%%             register_family_boss(FamilyID),
%%             do_send_to_family_boss_process(ProcessName, Info, FamilyID);
%%         PID ->
%%             erlang:send(PID, Info),
%%             ok
%%     end.
do_send_to_family_boss_process(ProcessName, Info, FamilyID) when is_atom(ProcessName) ->
    case erlang:whereis(ProcessName) of
        undefined ->
            if
                FamilyID > 0 ->
                    ?ERR("~ts:~w ~w", ["没有找到联盟boss进程", ProcessName, Info]);
                true ->
                    ignore
            end;
        _ ->
            erlang:send(ProcessName, Info),
            ok
    end.

to_p_family_info(FamilyInfo) when erlang:is_record(FamilyInfo, family_info) ->
    #family_info{
                 family_id=FamilyID
                 ,family_name=FamilyName
                 ,level=FamilyLevel
                 ,create_role_id=CreateRoleID
                 ,create_role_name=CreateRoleName
                 ,owner_role_id=OwnerRoleID
                 ,owner_role_name=OwnerRoleName
                 ,cur_members=CurMembers
                 ,active_points=ActivePoints
				 ,family_score=FamilyScore
                 ,notice=Notice
                 ,slogan=Slogan
                 ,members=Members
                 ,rank=Rank
				 ,world_rank=WorldRank
                 ,create_time=CreateTime
                 ,cross_rank=CrossRank
				 ,talkRoomID=TalkRoomID
                } = FamilyInfo,
    NewMembers = lists:map(fun(Member) -> to_p_family_member_info(Member) end, Members),
    #p_family_info{
                   family_id=FamilyID
                   ,family_name=FamilyName
                   ,level=FamilyLevel
                   ,create_role_id=CreateRoleID
                   ,create_role_name=CreateRoleName
                   ,owner_role_id=OwnerRoleID
                   ,owner_role_name=OwnerRoleName
                   ,cur_members=CurMembers
                   ,active_points=ActivePoints
				   ,family_score=FamilyScore
                   ,notice=Notice
                   ,members=NewMembers
                   ,rank=Rank
				   ,world_rank=WorldRank
                   ,create_time=CreateTime
                   ,slogan=Slogan
                   ,total_fight_power = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, Members)
                   ,cross_rank=CrossRank
				   ,talkRoomID=TalkRoomID}.

to_p_family_member_info(MemberInfo) when erlang:is_record(MemberInfo, family_member_info) ->
    #family_member_info{
                        role_id=RoleID
                        ,role_name=RoleName
                       %%                             ,family_id=FamilyID
                        ,family_contribution=FamilyCon
                        ,left_family_contribution=LeftFamilyCon
                        ,use_gold_time=UseGoldTime
                        ,title=Title
                        ,is_male=IsMale
                        ,online=IsOnline
                        ,role_level=RoleLevel
                        ,fight_power=FightPower
                        ,family_title=FamilyTitle
			,join_time=JoinTime
			,weeklyContributes=WeeklyContributes
                        ,reward_level=RewardLevel 
                        ,head=Head
                        ,offline_time=OfflineTime
                       } = MemberInfo,
    #p_family_member_info{
                          role_id=RoleID
                          ,role_name=RoleName
                          ,family_contribution=FamilyCon
                          ,left_family_contribution=LeftFamilyCon
                          ,use_gold_time=UseGoldTime
                          ,title=Title
                          ,is_male=IsMale
                          ,online=IsOnline
                          ,role_level=RoleLevel
                          ,fight_power=FightPower
                          ,family_title=FamilyTitle
			  ,join_time=JoinTime
                          ,worship_times=RewardLevel
			  ,weekly_contributes = WeeklyContributes
                          ,head=Head 
                          ,offline_time=OfflineTime}.

to_p_family_summary(FamilyInfo) when erlang:is_record(FamilyInfo, family_info) ->
    #family_info{family_id=FamilyID, family_name=FamilyName,members=Members,notice=Notice,slogan=Slogan,rank=Rank,owner_role_id=OwnerRoleID,
                   owner_role_name=OwnerRoleName, cur_members=CurMembers,level=Level,world_rank=WorldRank, family_score=Score,cross_rank=CrossRank} = FamilyInfo,
    #p_family_summary{family_id=FamilyID, family_name=FamilyName, notice=Notice,rank=Rank,owner_role_id=OwnerRoleID,
                      owner_role_name=OwnerRoleName, cur_members=CurMembers, level=Level,world_rank=WorldRank,slogan=Slogan,
                      total_fight_power = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, Members), 
                      score=Score, cross_rank=CrossRank, serverID=util:calc_server_id(familyID, FamilyID)}.

to_p_family_request(FamilyRequest) when erlang:is_record(FamilyRequest, family_request) ->
    #family_request{
                    role_id=RoleID
                    ,role_name=RoleName
                    ,level=RoleLevel
                    ,fight_power=FightPower
                    ,timestamp=Timestamp
                    ,family_id=FamilyID
                    ,head=_Head
                    ,title=Title
                    ,is_male=IsMale} = FamilyRequest,
    #rolePublic{lastLogoutTime=LastLogoutTime,head=RoleHead} = role_lib:get_rolePublic(RoleID),
    #p_family_request{
                      role_id=RoleID
                      ,role_name=RoleName
                      ,level=RoleLevel
                      ,fight_power=FightPower
                      ,timestamp=Timestamp
                      ,family_id=FamilyID
                      ,online=role_lib:is_online(RoleID)
                      ,offline_time=LastLogoutTime
                      ,head=RoleHead
                    ,title=Title
                    ,is_male=IsMale}.

gen_p_family_info() ->
    #p_family_info{
                   family_id=0
                   ,family_name= <<"">>
                   ,level=0
                   ,create_role_id=0
                   ,create_role_name= <<"">>
                   ,owner_role_id=0
                   ,owner_role_name= <<"">>
                   ,cur_members=0
                   ,active_points=0
				   ,family_score=0
                   ,notice= <<"">>
                   ,members= []
                   ,rank=0
				   ,world_rank=0
                   ,create_time=0
				   ,slogan = <<"">>
                   ,total_fight_power=0
                   ,cross_rank=0
				   ,talkRoomID=""}.

lock_family_protect(RoleID, Status) ->
    case ets:lookup(?ETS_FAMILY_PROTECT, RoleID) of
        [] ->
%%             ?ERR("lock RoleID:~w, Status:~w", [RoleID, Status]),
            ets:insert(?ETS_FAMILY_PROTECT, {RoleID, Status});
        [{RoleID, _OldStatus}] ->
%%             ?ERR("lock fail RoleID:~w, Status:~w, OldStatus:~w", [RoleID, Status, OldStatus]),
            false
    end.

unlock_family_protect(RoleID) ->
%%     ?ERR("unlock:~w", [ets:lookup(?ETS_FAMILY_PROTECT, RoleID)]),
    ets:delete(?ETS_FAMILY_PROTECT, RoleID).



