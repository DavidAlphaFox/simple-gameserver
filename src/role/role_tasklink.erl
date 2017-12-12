-module(role_tasklink).
-compile(export_all).
-include("def_role.hrl").
-include("def_team_pk.hrl").
-export([]).

%% 只允许role本进程调用
get_role_tasklink()->
    NowDate = erlang:date(),
    Tasklink1 = 
        case get(?role_tasklink) of
            ?undefined ->
                RoleID = role_data:get_roleID(),
                Tasklink0 = db_sql:get_role_tasklink(RoleID),
                put(?role_tasklink,Tasklink0),
                Tasklink0;
            Tasklink0 ->
                Tasklink0
        end,
    if
        NowDate =:= Tasklink1#role_tasklink.last_date ->
            Tasklink1;
        true ->
            NewTasklink = Tasklink1#role_tasklink{last_date = NowDate
                                                 ,free_time = 2
                                                 ,pay_count = 0},
            put(?role_tasklink,NewTasklink),
            NewTasklink
    end.

set_role_tasklink(NewTasklink)->
    put(?role_tasklink,NewTasklink).

cs_tasklink_get_info(_) ->
    RoleID = role_data:get_roleID(),
    RoleTasklink = get_role_tasklink(),
    TasklinkInfo = ets:lookup(?ETS_TASKLINK_LIST, RoleID),
    SignStatus = if
                     TasklinkInfo /= [] ->
                         1;
                     true ->
                         0
                 end,
    LevelInfoList = lists:map(fun(Level)-> 
                    {FV,R} = data_tasklink:get({diffcult_level,Level}),
                    #p_level_info{diffcult_level = Level
                                 ,need_force_value = FV
                                 ,reward_view = role_reward:transform2p_reward_view(R, [])}
            end, data_tasklink:get(diffcult_level_list)),
    ?sendself(#sc_tasklink_get_info{sign_status = SignStatus
                                   ,diffcult_level = 0
                                   ,sign_time = RoleTasklink#role_tasklink.free_time + RoleTasklink#role_tasklink.buy_left_time
                                   ,pay_count = RoleTasklink#role_tasklink.pay_count
                                   ,level_info_list = LevelInfoList
                                   ,self_force_value = role_lib:calculate_force_value()
                                   ,buy_need_gold = data_tasklink:get(but_need_gold)
                                   ,sign_open_level = data_tasklink:get(sign_open_level)}).

cs_tasklink_get_progress(_)->
    RoleID = role_data:get_roleID(),
    Now = util:now(),
    TasklinkInfo = tasklink_server:get_tasklink_progress(RoleID),
    case TasklinkInfo of
        ?undefined ->
            ?sendself(#sc_tasklink_get_progress{current_timestamp=0
                                               ,next_point = 0
                                               ,remaining_time = 0
                                               ,remaining_percent = 0
                                               ,point_info_list = []
                                               ,end_timestamp = 0
                                               ,members = []
                                               ,all_need_time=0});
        _ ->
            Msg = get_progress_msg(TasklinkInfo,Now),
            ?sendself(Msg)
    end.

cs_tasklink_get_reward_log(_)->
    RoleID = role_data:get_roleID(),
    TasklinkInfo = tasklink_server:get_tasklink_progress(RoleID),
    if
        ?undefined /= TasklinkInfo ->
            ?sendself(#sc_tasklink_get_reward_log{reward_log_list = lists:sort(get_all_log(TasklinkInfo))});
        true ->
            ?sendself(#sc_tasklink_get_reward_log{reward_log_list = []})
    end.

cs_tasklink_sign(#cs_tasklink_sign{level = DiffcultLevel})->
    case check_tasklink_sign(DiffcultLevel) of
        {ok,MemberList} ->
            LeaderRoleInfo = role_data:get_roleInfo(),
            erlang:send(tasklink_server,{cs_tasklink_sign,LeaderRoleInfo,DiffcultLevel,MemberList});
        {fail,Reason,L} ->
            ErrorName = 
                lists:foldr(fun(R,StrAcc)->
                        #rolePublic{roleName=RoleName} = role_lib:get_rolePublic(R),
                        if
                            StrAcc =:= "" ->
                                RoleName;
                            true ->
                                 <<StrAcc/binary,"、",RoleName/binary>>
                        end
                    end, "", L),
            ?sendself(#sc_tasklink_sign{result = Reason, error_name = ErrorName});
        {fail,Reason} ->
            ?sendself(#sc_tasklink_sign{result = Reason, error_name = ""})
    end.

cs_tasklink_buy_time(_)->
    RoleID = role_data:get_roleID(),
    RoleTasklink = get_role_tasklink(),
    CurPayCount = RoleTasklink#role_tasklink.pay_count,
    Max = data_tasklink:get(but_count_max),
    if
        Max > CurPayCount ->
            RoleInfo = role_data:get_roleInfo(),
            NeedGold = data_tasklink:get(but_need_gold),
            case role_lib:check_money(RoleInfo, gold, NeedGold) of
                true ->
                    role_lib:deduct_gold_f(RoleInfo, NeedGold, ?MONEY_DEC_TYPE_TASKLINK_BUYTIME, 0, ""),
                    NewTasklink = RoleTasklink#role_tasklink{buy_left_time = RoleTasklink#role_tasklink.buy_left_time + 1
                                                            ,pay_count = RoleTasklink#role_tasklink.pay_count  +1},
                    put(?role_tasklink,NewTasklink),
                    ?sendself(#sc_tasklink_buy_time{result = 1
                                                   ,new_time = NewTasklink#role_tasklink.free_time + NewTasklink#role_tasklink.buy_left_time});
                false ->
                    ?sendself(#sc_tasklink_buy_time{result = 3
                                                   ,new_time = RoleTasklink#role_tasklink.free_time + RoleTasklink#role_tasklink.buy_left_time})
            end;
        true ->
            ?sendself(#sc_tasklink_buy_time{result = 2
                                           ,new_time = RoleTasklink#role_tasklink.free_time + RoleTasklink#role_tasklink.buy_left_time})
    end.

cs_tasklink_ready_opt(#cs_tasklink_ready_opt{opt_type = OptType})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    TasklinkInfo = ets:lookup(?ETS_TASKLINK_LIST, RoleID),
    if
        [] =:= TasklinkInfo ->
            erlang:send(tasklink_server,{cs_tasklink_ready_opt,RoleID,OptType});
        true ->
            ?sendself(#sc_tasklink_ready_opt{result = 4})
    end.
            
cs_tasklink_get_reward(#cs_tasklink_get_reward{index=Index})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    TasklinkInfo = ets:lookup(?ETS_TASKLINK_LIST, RoleID),
    if
        [] /= TasklinkInfo ->
            erlang:send(tasklink_server,{cs_tasklink_get_reward,RoleID,Index});
        true ->
            ?sendself(#sc_tasklink_get_reward{result = 4,reward_view=[]})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_progress_msg(TasklinkInfo,Now)->
    RemainingTime = max(0,TasklinkInfo#tasklink_info.next_timestamp - Now),
    NextPoint = 
        case lists:sort([E||E<-TasklinkInfo#tasklink_info.points,E#p_point_info.point_status =:= 0]) of
            [] -> 0;
            [Next|_] -> Next#p_point_info.point_index
        end,
    EndTimestamp = TasklinkInfo#tasklink_info.start_timestamp
                + (TasklinkInfo#tasklink_info.point_need_time * erlang:length(TasklinkInfo#tasklink_info.points)),
    #sc_tasklink_get_progress{current_timestamp=Now
                             ,next_point = NextPoint
                             ,remaining_time = TasklinkInfo#tasklink_info.next_timestamp
                             ,remaining_percent = min(99,((100 * RemainingTime) div TasklinkInfo#tasklink_info.point_need_time) )
                             ,point_info_list= lists:sort([E#p_point_info{point_args=[]}||E<-TasklinkInfo#tasklink_info.points])
                             ,end_timestamp = EndTimestamp
                             ,members = TasklinkInfo#tasklink_info.member
                             ,all_need_time = EndTimestamp - TasklinkInfo#tasklink_info.start_timestamp}.

check_tasklink_sign(DiffcultLevel)->
    #role{roleID=RoleID, level=Level} = RoleInfo = role_data:get_roleInfo(),
    TeamId = RoleInfo#role.teamId,
    RoleTaskLink = get_role_tasklink(),
    if
        TeamId =:= -1 ->
            case check_member_sign(RoleID,[{RoleID
                                           ,role_lib:calculate_force_value()
                                           ,tasklink_server:tasklink_state(RoleID)
                                           ,RoleTaskLink#role_tasklink.free_time + RoleTaskLink#role_tasklink.buy_left_time
                                           ,Level}],DiffcultLevel) of
                ok ->
                    {ok,[#p_member_info{roleID=RoleID
                                       ,head=RoleInfo#role.head, camp=0, status=0
                                       ,isMale=RoleInfo#role.isMale, title=RoleInfo#role.title
                                       ,level=Level}]};
                {fail,Reason,L} ->
                    {fail,Reason,L}
            end;
        true ->
            case ets:lookup(ets_team_list, TeamId) of
                [#team_info{teamleader_roleid=LeaderRoleID,team_member=TeamMember}] ->
                    if
                        RoleID /= LeaderRoleID ->
                            {fail,3};  %% 不是队长
                        true ->
                            case collect_team_data(TeamMember,[]) of
                                fail ->
                                    %% 其实是队员数据有变化
                                    ?ERR("check_tasklink_sign someone member is offline or dead ~w ",[TeamMember]),
                                    {fail,3};  %% 不是队长
                                TeamDataList ->
                                    case check_member_sign(LeaderRoleID,TeamDataList,DiffcultLevel) of
                                        ok ->
                                            {ok,[#p_member_info{roleID=E#p_team_member_info.roleID
                                                               ,head=E#p_team_member_info.head, camp=0, status=0
                                                               ,isMale=E#p_team_member_info.isMale, title=E#p_team_member_info.title
                                                               ,level=E#p_team_member_info.level}||E<-TeamMember]};
                                        {fail,Reason,L} ->
                                            {fail,Reason,L}
                                    end
                            end
                    end;
                _ ->
                    ?ERR("team id is wrong. not found team info ~w ~w",[RoleID,TeamId]),
                    {fail,3}   %% 不是队长
            end
    end.

collect_team_data([],AccList)->
    AccList;
collect_team_data([HeadMember|Other],AccList)->
    RId = HeadMember#p_team_member_info.roleID,
    RLevel = HeadMember#p_team_member_info.level,
    #role{roleID=RoleID,level=Level} = role_data:get_roleInfo(),
    FightState = tasklink_server:tasklink_state(RId),
    if
        RId =:= RoleID ->
            RoleTaskLink = get_role_tasklink(),
            collect_team_data(Other,[{RoleID,role_lib:calculate_force_value()
                                    ,FightState
                                    ,RoleTaskLink#role_tasklink.free_time + RoleTaskLink#role_tasklink.buy_left_time
                                    ,Level}|AccList]);
        true ->
            case catch role_lib:call_server(RId,get_tasklink_sign_data,1000) of
                {V,Time} when erlang:is_integer(V)->
                    collect_team_data(Other,[{RId,V,FightState,Time,RLevel}|AccList]);
                _ ->
                    fail
            end
    end.

check_member_sign(LeaderRoleID,TeamDataList,DiffcultLevel)->
    {NeedFv,_}=data_tasklink:get({diffcult_level,DiffcultLevel}),
    {L1,L2,L3,L4,L5} = check_member_sign2(TeamDataList,NeedFv,{[],[],[],[],[]}),
    ?INFO("check_member_sign ~w",[{L1,L2,L3,L4,L5}]),
%%     IsLeaderWeak = lists:member(LeaderRoleID, L2),
    IsLeaderNoTime = lists:member(LeaderRoleID, L3),
    if
        L1 /= [] ->
           {fail,2,L1};  %% 有人还在战斗中
        L4 /= [] ->
           {fail,5,L4};  %% 有人奖励未领取
        L2 =:= true ->
           {fail,4,[]};  %% 队长原力值不足
        IsLeaderNoTime =:= true ->
           {fail,8,[]};
        L3 /= [] ->
           {fail,7,L3};  %% 有人次数不够了
        L5 /= [] ->
           {fail,9,L5};  %% 有人次数不够了
       true ->
           ok
    end.
check_member_sign2([],_NeedFv,{Acc1,Acc2,Acc3,Acc4,Acc5})->
    {Acc1,Acc2,Acc3,Acc4,Acc5};
check_member_sign2([{R,FV,FightState,Time,Level}|T],NeedFv,{Acc1,Acc2,Acc3,Acc4,Acc5})->
    NeedLevel = data_tasklink:get(sign_open_level),
    if
        FV < NeedFv ->
            check_member_sign2(T,NeedFv,{Acc1,[R|Acc2],Acc3,Acc4,Acc5});
        FightState =:= 1 ->
            check_member_sign2(T,NeedFv,{[R|Acc1],Acc2,Acc3,Acc4,Acc5});
        FightState =:= 2 ->
            check_member_sign2(T,NeedFv,{Acc1,Acc2,Acc3,[R|Acc4],Acc5});
        Time =< 0 ->
            check_member_sign2(T,NeedFv,{Acc1,Acc2,[R|Acc3],Acc4,Acc5});
        Level < NeedLevel ->
            check_member_sign2(T,NeedFv,{Acc1,Acc2,Acc3,Acc4,[R|Acc5]});
        true ->
            check_member_sign2(T,NeedFv,{Acc1,Acc2,Acc3,Acc4,Acc5})
    end.
    
get_all_log(TasklinkInfo)->
    get_all_log([E#p_member_info.roleID||E<-TasklinkInfo#tasklink_info.member],[]).

get_all_log([],AccLog)->
    AccLog;
get_all_log([RoleID|Other],AccLog)->
    case tasklink_server:get_tasklink_progress(RoleID) of
        ?undefined ->
            get_all_log(Other,AccLog);
        TasklinkInfo ->
            get_all_log(Other,get_all_log2(TasklinkInfo#tasklink_info.reward_log,AccLog))
    end.

get_all_log2([],AccLog)->
    AccLog;
get_all_log2([H|T],AccLog)->
    get_all_log2(T,[H|AccLog]).
