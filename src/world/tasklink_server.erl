-module(tasklink_server).
-compile(export_all).
-behaviour(gen_server).
-include("def_mail.hrl").
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_item.hrl").
-include("def_tasklink_log.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(POINT_STATUS_NOARRIVED, 0). %% 没到达
-define(POINT_STATUS_ARRIVED_REWARD, 1).   %% 到达了,有奖励可以领取，未领取
-define(POINT_STATUS_ARRIVED_NOREWARD, 2).   %% 到达了,无奖励可以领取
-define(POINT_STATUS_REWARD_ALREADY_GET, 3).   %% 到达了,有奖励可以领取，已经领取

-define(REWARD_LOG_TYPE_RANDOM_BASE, 1900).   %% 随机任务起始值

-define(persist_interval_second, 600).   %% 3m
-define(persist_point_time, 180).

-record(state,{}).
-record(ready_info,{member_list = []
                   ,leader_info = #role{}
                   ,level_diffcult = 0
                   ,end_timestamp = 0}).

%%%%%===========需要固化保存的数据
%%%%%===========不需要固化保存的数据
-define(ready_list, ready_list).%所有玩家列表
-define(ready_timer_ref, ready_timer_ref).%所有玩家列表
-define(point_timer_ref, point_timer_ref).%所有玩家列表

%% ------------------------------------------------------------------
%% API Function Definitions gen_server
%% ------------------------------------------------------------------

start() ->
    {ok,_}=
        supervisor:start_child(world_sup, 
                               {?MODULE,
                                {?MODULE, start_link, []},
                                permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_tasklink_progress(RoleID)->
    case ets:lookup(?ETS_TASKLINK_LIST, RoleID) of
        [] ->
            ?undefined;
        [TasklinkInfo] ->
            TasklinkInfo
    end.

tasklink_state(RoleID)->
    case ets:lookup(?ETS_TASKLINK_LIST, RoleID) of
        [] ->
            0;
        [TasklinkInfo] ->
            IsNoFinish = lists:any(fun(P) -> P#p_point_info.point_status =:= 0 end, TasklinkInfo#tasklink_info.points),
            if
                IsNoFinish =:= true ->
                    1;
                true ->
                    2
            end
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    random:seed(util:gen_random_seed()),
    process_flag(trap_exit,true),
    put(?ready_list,[]),
    ets:delete_all_objects(?ETS_TASKLINK_LIST),
    case db_sql:get_etc(?DB_ETC_KEY_TASKLINK) of
        TasklinkList0 when erlang:is_list(TasklinkList0) ->
            Now = util:now(),
            {NewTasklinkUpdateList,NewNextTs} = refresh_tasklink_info_list(TasklinkList0,Now,[],?persist_point_time + Now),
            Timer = NewNextTs - Now,
            ets:insert(?ETS_TASKLINK_LIST,NewTasklinkUpdateList);
        ERR ->
            Timer = ?persist_point_time,
            ?ERR("tasklink_server ~w",[ERR])
    end,
    reset_timer(?ready_timer_ref,data_tasklink:get(ready_time),ready_timeout),
    reset_timer(?point_timer_ref,Timer,point_timeout),
    reset_tick_interval(),
    {ok, #state{}}.

handle_call(get_current_ready_list, _From, State) ->
    {reply, get(?ready_list), State};
handle_call({get_pd,Pd}, _From, State) ->
    {reply, get(Pd), State};
handle_call({put_pd,Pd,Value}, _From, State) ->
    {reply, put(Pd,Value), State};
handle_call(Request, _From, State) ->
    ?ERR("handle_call function clause:request=~w",[Request]),
    {reply, ok, State}.

handle_cast({test_enter_war,Num}, State) ->
    lists:foreach(fun(RID)-> 
                          enter_war(1,[{p_member_info,RID,10000011,0,0,true,0,301}],RID,true) 
                  end, lists:seq(6000001, 6000001+Num)),
    {noreply, State};
handle_cast(Msg, State) ->
    ?ERR("handle_cast function clause:request=~w",[Msg]),
    {noreply, State}.

handle_info({cs_tasklink_sign,LeaderRoleInfo,DiffcultLevel,MemberList},State) ->
    IsFight = lists:any(fun(M)-> 
                          TI = ets:lookup(?ETS_TASKLINK_LIST, M#p_member_info.roleID),
                          TI /= [] %% 没有战斗数据
            end, MemberList),
    ReadyList = get(?ready_list),
    IsReady = lists:any(fun(M)-> 
                case lists:filter(fun(ReadyInfo)-> 
                                          lists:member(M#p_member_info.roleID, [E#p_member_info.roleID||E<-ReadyInfo#ready_info.member_list]) 
                                  end, ReadyList) of
                    [] -> false;   %% 没有在准备中
                    _ -> true
                end
            end, MemberList),
    Len = erlang:length(MemberList),
    Now = util:now(),
    CloseTimestamp = Now + data_tasklink:get(ready_time),
    NewReadyInfo = #ready_info{member_list = MemberList
                              ,leader_info = LeaderRoleInfo
                              ,level_diffcult = DiffcultLevel
                              ,end_timestamp = CloseTimestamp},
    if
        IsFight =:= true ->
            ?unicast(LeaderRoleInfo#role.roleID,#sc_tasklink_sign{result=2,error_name=""});
        IsReady =:= true ->
            ?unicast(LeaderRoleInfo#role.roleID,#sc_tasklink_sign{result=6,error_name=""});
        -1 =:= LeaderRoleInfo#role.teamId ->
            ?unicast(LeaderRoleInfo#role.roleID,#sc_tasklink_sign{result=1,error_name=""}),
            enter_war(DiffcultLevel
                     ,MemberList
                     ,LeaderRoleInfo#role.roleID);
        true ->
            put(?ready_list,[NewReadyInfo|get(?ready_list)]),
            lists:foreach(fun(M)->
                            ?unicast(M#p_member_info.roleID,send_ready_notice(NewReadyInfo))
                end, MemberList),
            ?unicast(LeaderRoleInfo#role.roleID,#sc_tasklink_sign{result=1,error_name=""})
    end,
    {noreply, State};
handle_info({cs_tasklink_ready_opt,RoleID,OptType},State) ->
    ReadyList = get(?ready_list),
    case lists:splitwith(fun(RI)-> 
                              lists:member(RoleID, [E#p_member_info.roleID||E<-RI#ready_info.member_list]) 
                      end, ReadyList) of
        {[],_} ->
            ?unicast(RoleID,#sc_tasklink_ready_opt{result = 4}); %% 没有在准备中
        {[ReadyInfo|Repeated],OtherReadyList} ->
            if
                OptType =:= 2 ->
                    put(?ready_list,OtherReadyList),
                    {value,Self,OtherMemberList} = lists:keytake(RoleID,#p_member_info.roleID,ReadyInfo#ready_info.member_list),
                    NewReadyInfo = ReadyInfo#ready_info{member_list = [Self#p_member_info{status = 2}|OtherMemberList]},
                    lists:foreach(fun(M)->
                            ?unicast(M#p_member_info.roleID,send_ready_notice(NewReadyInfo#ready_info{end_timestamp = 0})) %% 有人取消了准备
                        end, ReadyInfo#ready_info.member_list);
                OptType =:= 1 ->
                    ?INFO("cs_tasklink_ready_opt ~w ~w ~w",[ReadyInfo,ReadyInfo#ready_info.member_list,RoleID]),
                    {value,Self,OtherMemberList} = lists:keytake(RoleID,#p_member_info.roleID,ReadyInfo#ready_info.member_list),
                    NewReadyInfo = ReadyInfo#ready_info{member_list = [Self#p_member_info{status = 1}|OtherMemberList]},
                    case [E||E<-NewReadyInfo#ready_info.member_list,E#p_member_info.status =:= 0] of
                        [] -> %% 都准备了
                            lists:foreach(fun(M)->
                                    ?unicast(M#p_member_info.roleID,send_ready_notice(NewReadyInfo#ready_info{end_timestamp = 0}))
                                end, ReadyInfo#ready_info.member_list),
                            put(?ready_list,OtherReadyList),
                            enter_war(ReadyInfo#ready_info.level_diffcult
                                     ,NewReadyInfo#ready_info.member_list
                                     ,NewReadyInfo#ready_info.leader_info#role.roleID);
%%                             L = erlang:length(ReadyInfo#ready_info.member_list),
%%                             if
%%                                 L > 1 ->
%%                                     erlang:send(team_manage_server,{disbandteam,NewReadyInfo#ready_info.leader_info});
%%                                 true ->
%%                                     ignore
%%                             end;
                        _ -> %% 还有人没准备
                            lists:foreach(fun(M)->
                                    ?unicast(M#p_member_info.roleID,send_ready_notice(NewReadyInfo))
                                end, ReadyInfo#ready_info.member_list),
                            put(?ready_list,[NewReadyInfo|OtherReadyList])
                    end
            end,
            if
                Repeated /= [] ->
                    ?ERR("cs_tasklink_ready_opt Error RoleID Repeated ~w",[ReadyList]);
                true ->
                    ignore
            end,
            ?unicast(RoleID,#sc_tasklink_ready_opt{result = 1}) %% 没有在准备中
    end,
    {noreply,State};
handle_info({cs_tasklink_get_reward,RoleID,Index},State) when Index =:= 0 ->
    case tasklink_server:get_tasklink_progress(RoleID) of
        ?undefined ->
            ?unicast(RoleID,#sc_tasklink_get_reward{result = 4,reward_view=[]});
        TasklinkProgress ->
%%             MemberNum = erlang:length(TasklinkProgress#tasklink_info.member),
%%             LeaderID = TasklinkProgress#tasklink_info.leader_id,
            {NewPoints,RewardData} = lists:foldl(fun(PointInfo,{AccPoints,AccRewardData})->
                        %% 队长奖励翻倍
%%                         if
%%                             RoleID =:= LeaderID andalso MemberNum > 1 ->
%%                                 RewardData2 = leader_double_reward(PointInfo#p_point_info.point_args),
%%                                 ?INFO("leader double(~w):~w->~w",[RoleID,PointInfo#p_point_info.point_args,RewardData2]);
%%                             true ->
%%                                 RewardData2 = PointInfo#p_point_info.point_args,
%%                         end,
                        if
                            PointInfo#p_point_info.point_status =:= ?POINT_STATUS_ARRIVED_REWARD->
                                RewardList = case PointInfo#p_point_info.point_args of
                                                 {RewardList0} ->
                                                     RewardList0;
                                                 {RewardList1,RewardList2}->
                                                     RewardList1 ++ RewardList2
                                             end,
                                NewPoints = [PointInfo#p_point_info{point_status=?POINT_STATUS_REWARD_ALREADY_GET}|AccPoints],
                                {NewPoints,RewardList ++ AccRewardData};
                            true  ->
                                {AccPoints,AccRewardData}
                        end
                end, {[],[]}, TasklinkProgress#tasklink_info.points),
%%             ?INFO("cs_tasklink_get_reward all ~w",[NewPoints]),
            RewardView = role_reward:transform2p_reward_view(RewardData, []),
            IsDelete = lists:all(fun(P)-> 
                                         P#p_point_info.point_status =:= ?POINT_STATUS_ARRIVED_NOREWARD 
                                            orelse P#p_point_info.point_status =:= ?POINT_STATUS_REWARD_ALREADY_GET
                                 end, NewPoints),
            ?INFO("IsDelete ~w:~w",[IsDelete,NewPoints]),
            if
                IsDelete =:= true ->
                    ets:delete(?ETS_TASKLINK_LIST,RoleID);
                true ->
                    ets:insert(?ETS_TASKLINK_LIST, TasklinkProgress#tasklink_info{points = NewPoints})    %% 该进程可以操作?ETS_TASKLINK_LIST
            end,
            ?unicast(RoleID,#sc_tasklink_get_reward{result = 1,reward_view=RewardView}),
            role_lib:send_server(RoleID, {tasklink_reward,RewardData,?MONEY_ADD_TYPE_TASKLINK,Index})
    end,
    {noreply,State};
handle_info({cs_tasklink_get_reward,RoleID,Index},State) when Index /= 0 ->
    case tasklink_server:get_tasklink_progress(RoleID) of
        ?undefined ->
            ?unicast(RoleID,#sc_tasklink_get_reward{result = 4,reward_view=[]});
        TasklinkProgress ->
            case lists:keytake(Index, #p_point_info.point_index, TasklinkProgress#tasklink_info.points) of
                {value,PointInfo,OtherPoints} when PointInfo#p_point_info.point_status =:= ?POINT_STATUS_ARRIVED_REWARD ->
                    ?INFO("cs_tasklink_get_reward -1- PointInfo:~w",[PointInfo]),
                    %% 队长奖励翻倍
%%                     MemberNum = erlang:length(TasklinkProgress#tasklink_info.member),
%%                     LeaderID = TasklinkProgress#tasklink_info.leader_id,
%%                     if
%%                         RoleID =:= LeaderID andalso MemberNum > 1 ->
%%                             RewardListDouble = leader_double_reward(PointInfo#p_point_info.point_args),
%%                             ?INFO("leader double(~w):~w->~w",[RoleID,PointInfo#p_point_info.point_args,RewardListDouble]);
%%                         true ->
%%                             RewardListDouble = PointInfo#p_point_info.point_args,
%%                     end,
                    RewardList = case PointInfo#p_point_info.point_args of
                                     {RewardList0} ->
                                         RewardList0;
                                     {RewardList1,RewardList2}->
                                         RewardList1 ++ RewardList2
                                 end,
                    if
                        [] /= RewardList ->
                            NewPoints = [PointInfo#p_point_info{point_status=?POINT_STATUS_REWARD_ALREADY_GET}|OtherPoints],
                            role_lib:send_server(RoleID, {tasklink_reward,RewardList,?MONEY_ADD_TYPE_TASKLINK,Index}),
                            RewardView = role_reward:transform2p_reward_view(RewardList, []),
                            ?unicast(RoleID,#sc_tasklink_get_reward{result = 1,reward_view=RewardView}),
                            ?unicast(RoleID,role_tasklink:get_progress_msg(TasklinkProgress#tasklink_info{points = NewPoints},util:now()));
                        true ->
                            NewPoints = [PointInfo#p_point_info{point_status=?POINT_STATUS_ARRIVED_NOREWARD}|OtherPoints],
                            ?unicast(RoleID,#sc_tasklink_get_reward{result = 2,reward_view=[]})
                    end,    
                    IsDelete = lists:all(fun(P)-> 
                                                 P#p_point_info.point_status =:= ?POINT_STATUS_ARRIVED_NOREWARD 
                                                    orelse P#p_point_info.point_status =:= ?POINT_STATUS_REWARD_ALREADY_GET
                                         end, NewPoints),
                    ?INFO("IsDelete ~w:~w",[IsDelete,NewPoints]),
                    if
                        IsDelete =:= true ->
                            ets:delete(?ETS_TASKLINK_LIST,RoleID);
                        true ->
                            ets:insert(?ETS_TASKLINK_LIST, TasklinkProgress#tasklink_info{points = NewPoints})    %% 该进程可以操作?ETS_TASKLINK_LIST
                    end;
               {value,PointInfo,OtherPoints} when PointInfo#p_point_info.point_status =:= ?POINT_STATUS_NOARRIVED ->
                    ?unicast(RoleID,#sc_tasklink_get_reward{result = 2,reward_view=[]});
               {value,PointInfo,OtherPoints} when PointInfo#p_point_info.point_status =:= ?POINT_STATUS_ARRIVED_NOREWARD ->
                    ?unicast(RoleID,#sc_tasklink_get_reward{result = 5,reward_view=[]});
               {value,PointInfo,OtherPoints} when PointInfo#p_point_info.point_status =:= ?POINT_STATUS_REWARD_ALREADY_GET ->
                    ?unicast(RoleID,#sc_tasklink_get_reward{result = 3,reward_view=[]});
               _ ->
                    ?unicast(RoleID,#sc_tasklink_get_reward{result = 4,reward_view=[]})
            end
    end,
    {noreply,State};
%% 最初触发timer的准备队列可能已经取消或全员通过了
handle_info({timeout, TimerRef, ready_timeout},State) ->
    ?INFO("ready_timeout"),
    OldRef = get(?ready_timer_ref),
    if
        TimerRef /= OldRef -> ?ERR("ready_timeout timer ref error ~w ~w",[TimerRef,OldRef]);
        true -> ignore
    end,
    ReadyList = get(?ready_list),
    Now = util:now(),
    {MinTimer,NewReadyList} = 
        lists:foldl(fun(RI,{AccMinTimer,AccReadyList})->
                        if
                            RI#ready_info.end_timestamp =< Now ->
                                lists:foreach(fun(M)->
                                        ?unicast(M#p_member_info.roleID,send_ready_notice(RI#ready_info{end_timestamp = 0})) %% 有人取消了准备
                                    end, RI#ready_info.member_list),
                                {AccMinTimer,AccReadyList}; %%取消报名
                            true ->
                                {erlang:min(RI#ready_info.end_timestamp,AccMinTimer)  
                                ,[RI|AccReadyList]}
                        end
            end, {Now + data_tasklink:get(ready_time),[]}, ReadyList),
    put(?ready_list,NewReadyList),
    reset_timer(?ready_timer_ref,erlang:max((MinTimer - Now),1),ready_timeout),
    {noreply,State};

handle_info({timeout, TimerRef, point_timeout},State) ->
    OldRef = get(?point_timer_ref),
    if
        TimerRef /= OldRef -> ?ERR("point_timeout timer ref error ~w ~w",[TimerRef,OldRef]);
        true -> ignore
    end,
    Now = util:now(),
    TimeFilter = Now + ?persist_point_time,  %% 是考虑最多一个周期内，可能触发的事件
%%     Filter = ets:fun2ms(fun(RI) when RI#tasklink_info.next_timestamp > 0 andalso RI#tasklink_info.next_timestamp =< (Now + ?persist_point_time) -> RI end),
    Filter = [{#tasklink_info{role_id = '_',leader_id = '_',
                 diffcult = '_',start_timestamp = '_',points = '_',
                 reward_log = '_',next_timestamp = '_',point_need_time = '_',
                 member = '_',log_args = '_'},
              [{'andalso',{'and',{'orelse',true,fail},
                                 {'>',{element,8,'$_'},0}},
                          {'=<',{element,8,'$_'},TimeFilter}}],
              ['$_']}],
    BeforeMem = erlang:process_info(self(),memory),
    RefreshList = ets:select(?ETS_TASKLINK_LIST, Filter),
    BeforeMem2 = erlang:process_info(self(),memory),
    {NewTasklinkUpdateList,NewNextTs} = refresh_tasklink_info_list(RefreshList,Now,[],?persist_point_time + Now),
    ets:insert(?ETS_TASKLINK_LIST, NewTasklinkUpdateList),
    AfterMem = erlang:process_info(self(),memory),
    erlang:garbage_collect(self()),
    AfterMem2 = erlang:process_info(self(),memory),
    ?INFO("point_timeout(~w) ~w ~w, ~w>~w>~w>~w",[erlang:length(RefreshList),NewNextTs,erlang:max((NewNextTs - Now),1),BeforeMem,BeforeMem2,AfterMem,AfterMem2]),
    reset_timer(?point_timer_ref,erlang:max((NewNextTs - Now),1),point_timeout),
    {noreply,State};

%% 定周期处理
handle_info(interval, State) ->
    do_persist(),
    reset_tick_interval(),
    {noreply, State};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref)->
    {noreply,State};
handle_info(Info, State) ->
    ?ERR("can't handle_info message:~w State:~w",[Info,State]),
    {noreply, State}.

terminate(Reason, State) ->
    do_persist(),
    ?ERR("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 设置下一次定周期触发
reset_tick_interval()->
    erlang:send_after(?persist_interval_second*1000, self(), interval).

do_persist()->
    db_sql:set_etc(?DB_ETC_KEY_TASKLINK, ets:tab2list(?ETS_TASKLINK_LIST)),
    erlang:garbage_collect(self()).

is_persist(_) ->
    false.

check_tasklink_sign([])->
    ok;
check_tasklink_sign([H|T])->
    TasklinkInfo = ets:lookup(?ETS_TASKLINK_LIST, H).

creat_points(DiffcultLevel,MemberNum)->
    GroupT = lists:foldl(fun(Type,AccGroup)->
            NowPoint = #p_point_info{point_index = erlang:length(lists:nth(1,AccGroup)) + 1
                                    ,point_type = Type
                                    ,point_status = ?POINT_STATUS_NOARRIVED
                                    ,point_args = []},
            RewardGroup = creat_reward(Type,MemberNum),
            [[NowPoint#p_point_info{point_args=lists:nth(I,RewardGroup)}|lists:nth(I,AccGroup)]||I<-lists:seq(1, MemberNum)]
        end, lists:duplicate(MemberNum,[]), data_tasklink:get({level_point,DiffcultLevel})),
    [lists:reverse(E)||E<-GroupT].

creat_reward(Type,MemberNum) when Type >= 11 andalso Type =< 30 ->
    BoxId = util:random_one_from_list(data_tasklink:get({point_type,Type})),
    [{[util:random_one_from_weigh_list(data_box_more:get(BoxId))]}||I<-lists:seq(1, MemberNum)];
creat_reward(Type,MemberNum) when Type >= 31 andalso Type =< 99 ->
    {EventTypeRandomList,BaseRewardList} = data_tasklink:get({point_type,Type}),
    {Num0,EventTypeList} = util:random_one_from_weigh_list(EventTypeRandomList),
    Num = erlang:min(Num0, erlang:length(EventTypeList)),
    EventList = util:random_list2(EventTypeList,Num),
    MemberList = util:random_list2(lists:seq(1, MemberNum),Num),
    %% 因为是随机的，顺序没关系
    ?INFO("creat_reward -1- ~w ++ ~w",[EventList,MemberList]),
    {RewardList2,_} = 
        lists:foldl(fun(I,{AccReward,AccEventList})->
                        case lists:member(I, MemberList) of
                            true ->
                                [H|T] = AccEventList,
                                {BoxIdList} = data_tasklink:get({event_type,H}),
                                BoxId = util:random_one_from_list(BoxIdList),
                                RewardList = {[util:random_one_from_weigh_list(data_box_more:get(BoxId))],BaseRewardList},
                                {[RewardList|AccReward],T};
                            false ->
                                {[{BaseRewardList}|AccReward],AccEventList}
                        end
            end, {[],EventList}, lists:seq(1, MemberNum)),
    RewardList2;
creat_reward(_Type,MemberNum) ->
    lists:duplicate(MemberNum,[]).

enter_war(LevelDiffcult,MemberList0,LeaderID)->
    enter_war(LevelDiffcult,MemberList0,LeaderID,false).
enter_war(LevelDiffcult,MemberList0,LeaderID,IsDebug)->
    {value,LM,OtherMemberList} = lists:keytake(LeaderID, #p_member_info.roleID, MemberList0),
    MemberList = [LM|lists:sort(OtherMemberList)],
    ?INFO("enter_war ~w----~w----~w",[LevelDiffcult,MemberList,LeaderID]),
    MemberNum = erlang:length(MemberList),
    PointInterval = get_point_interval(MemberNum,LevelDiffcult),
    PointsGroup = creat_points(LevelDiffcult,MemberNum),
    Now = util:now(),
    IsAllOnline = case IsDebug of
                      true ->
                          true;
                      _ ->
                          lists:all(fun(M)-> role_lib:is_online(M#p_member_info.roleID) end, MemberList)
                  end,
    SamplePoint = lists:nth(1, PointsGroup),
    LogArgs0 = util:random_list2(lists:seq(1,erlang:length([P||P<-SamplePoint,P#p_point_info.point_type > 30]))),
    {LogArgsT,_} = lists:foldl(fun(I,{AccRes,AccLogArgs})->
                        SampleP = lists:nth(I, SamplePoint),
                        if
                            SampleP#p_point_info.point_type > 30 ->
                                [H|T] = AccLogArgs,
                                {[H|AccRes],T};
                            true ->
                                {[0|AccRes],AccLogArgs}
                        end     
                end, {[],LogArgs0}, lists:seq(1,erlang:length(SamplePoint))),
    LogArgs = lists:reverse(LogArgsT),
    ?INFO("enter_war ~w",[LogArgs]),
    if
        IsAllOnline =:= true ->
            
            lists:foreach(fun(I)->
                    M = lists:nth(I, MemberList),
                    RID = M#p_member_info.roleID,
                    catch role_lib:send_server(RID, deduct_tasklink_time),
                    Points = lists:nth(I, PointsGroup),
%%                     %% 队长奖励翻倍
%%                     if
%%                         RID =:= LeaderID andalso MemberNum > 1 ->
%%                             Points = lists:map(fun(E)->
%%                                         NewDoubleReward = leader_double_reward(E#p_point_info.point_args),
%%                                         ?INFO("leader double:~w->~w",[E#p_point_info.point_args,NewDoubleReward]),
%%                                         E#p_point_info{point_args = NewDoubleReward}
%%                                 end, Points0);
%%                         true ->
%%                             Points = Points0
%%                     end,
                    P = role_lib:get_rolePublic(RID),
                    Name = P#rolePublic.roleName,
                    TasklinkInfo = #tasklink_info{role_id=RID
                                                 ,leader_id=LeaderID
                                                 ,diffcult=LevelDiffcult
                                                 ,start_timestamp=Now,points=Points
                                                 ,reward_log=[#p_reward_log{timestamp=Now
                                                                           ,args_name= [Name]
                                                                           ,log_type = ?TLLOG_TYPE_START
                                                                           ,reward_view = []}]
                                                 ,next_timestamp=PointInterval + Now
                                                 ,point_need_time=PointInterval
                                                 ,member = MemberList
                                                 ,log_args = [LogArgs]},
                    ?INFO("---------enter_war-----~w------~n~w",[TasklinkInfo#tasklink_info.role_id,TasklinkInfo]),
                    ets:insert(?ETS_TASKLINK_LIST, TasklinkInfo#tasklink_info{points = Points}),
                    EndTimestamp = TasklinkInfo#tasklink_info.start_timestamp 
                          + (TasklinkInfo#tasklink_info.point_need_time * erlang:length(TasklinkInfo#tasklink_info.points)),
                    ?INFO("EndTimestamp:~w  ~w ---- ~w ---- ~w",[EndTimestamp,TasklinkInfo#tasklink_info.start_timestamp,TasklinkInfo#tasklink_info.point_need_time
                                            ,(TasklinkInfo#tasklink_info.point_need_time * erlang:length(TasklinkInfo#tasklink_info.points))]),
                    ?unicast(RID,#sc_tasklink_get_progress{current_timestamp=Now
                                                          ,next_point = 1
                                                          ,remaining_time = Now + data_tasklink:get(point_need_time) div data_tasklink:get(debug_speed_up)
                                                          ,remaining_percent = 0
                                                          ,point_info_list= lists:sort([E#p_point_info{point_args=[]}||E<-Points])
                                                          ,end_timestamp = EndTimestamp
                                                          ,members=TasklinkInfo#tasklink_info.member
                                                          ,all_need_time = EndTimestamp - TasklinkInfo#tasklink_info.start_timestamp})
                end, lists:seq(1, MemberNum));
        true ->
            ?ERR("enter_war fail. someone is offline")
    end.

get_next_point(Points)->
    case [E||E<-Points,E#p_point_info.point_status =:= ?POINT_STATUS_NOARRIVED] of
        [] ->
            [];
        UntouchPoints ->
            [H|_] = lists:sort(UntouchPoints),
            H
    end.

get_point_interval(MemberNum,LevelDiffcult)->
    RawTime=data_tasklink:get(point_need_time),
    AllPointNum = erlang:length(data_tasklink:get({level_point,LevelDiffcult})),
    AllTime = RawTime*AllPointNum,
    NewAllTime = AllTime - (3600*(MemberNum-1)),
    (NewAllTime div AllPointNum) div data_tasklink:get(debug_speed_up).   %% 临时测试，10倍速

reset_timer(RefType,Time,Msg)->
%%     ?INFO("set time ~w",[util:seconds_to_datetime(Time + util:now())]),
    NewRef = erlang:start_timer(Time*1000, self(), Msg),
    put(RefType,NewRef).


refresh_tasklink_info_list([],_Now,AccUpdateList,AccNextTs)->
    {AccUpdateList,AccNextTs};
refresh_tasklink_info_list([TasklinkInfo|OtherList],Now,AccUpdateList,AccNextTs) when TasklinkInfo#tasklink_info.next_timestamp =:= 0 ->
    refresh_tasklink_info_list(OtherList,Now,AccUpdateList,AccNextTs);  %% 不加入更新列表
refresh_tasklink_info_list([TasklinkInfo|OtherList],Now,AccUpdateList,AccNextTs)->
    {RawList,NewAccList,NewAccNextTs} = 
        if
            TasklinkInfo#tasklink_info.next_timestamp =< Now ->
                ArrivedNum = (Now - TasklinkInfo#tasklink_info.start_timestamp) div TasklinkInfo#tasklink_info.point_need_time,
                [OldLogArgs] = TasklinkInfo#tasklink_info.log_args,
                RID = TasklinkInfo#tasklink_info.role_id,
                {NewPointsT,NewLog} = lists:foldl(fun(P,{AccPoints,AccLog})->
                        Type = P#p_point_info.point_type,
                        NewP = 
                            if
                                P#p_point_info.point_index =< ArrivedNum andalso ?POINT_STATUS_NOARRIVED =:= P#p_point_info.point_status ->
                                    Timestamp = TasklinkInfo#tasklink_info.start_timestamp + P#p_point_info.point_index*TasklinkInfo#tasklink_info.point_need_time,
    %%                                 ?INFO("refresh_tasklink_info_list ~w 往前走了一步",[{TasklinkInfo#tasklink_info.role_id
    %%                                                                                  ,TasklinkInfo#tasklink_info.leader_id,P#p_point_info.point_index
    %%                                                                                  ,OldLogArgs}]),
                                    #rolePublic{roleName =RoleName} = role_lib:get_rolePublic(RID),
                                    H = lists:nth(P#p_point_info.point_index, OldLogArgs),
    %%                                 ?INFO("add log Type:~w(~w) leader_id:~w RID:~w point_index:~w",[Type,H,TasklinkInfo#tasklink_info.leader_id
    %%                                                                                            ,RID,P#p_point_info.point_index]),
                                    AddLogReward1 = case P#p_point_info.point_args of
                                                    {RewardList1,RewardList2}->
                                                        BaseReward = RewardList2,
                                                        #p_reward_log{timestamp=Timestamp,args_name= [RoleName,get_reward_name(RewardList1)],log_type = ?TLLOG_TYPE_ROLE_REWARD,reward_view = []};
                                                    {RewardList0} when Type >= 31 andalso Type =< 99 ->
                                                        BaseReward = RewardList0,
                                                        #p_reward_log{timestamp=Timestamp,args_name= [RoleName],log_type = ?TLLOG_TYPE_NOREWARD,reward_view = []};
                                                    {RewardList0} when Type >= 11 andalso Type =< 30 ->
                                                        BaseReward = RewardList0,
                                                        #p_reward_log{}
                                                 end,
                                    NewAccLogAll = if
                                                     RID =:= TasklinkInfo#tasklink_info.leader_id andalso Type >= 11 andalso Type =< 30 andalso ArrivedNum >= 20  ->
                                                         %% 目前约定最后一个点一定是宝箱
                                                         [#p_reward_log{timestamp=Timestamp,args_name= [get_reward_name(BaseReward)],log_type = ?TLLOG_TYPE_END,reward_view = []}];
                                                     RID =:= TasklinkInfo#tasklink_info.leader_id andalso Type >= 11 andalso Type =< 30 ->
                                                         %% 目前约定最后一个点一定是宝箱
                                                         [#p_reward_log{timestamp=Timestamp,args_name= [get_reward_name(BaseReward)],log_type = ?TLLOG_TYPE_BOX_ALLREWARS,reward_view = []}];
                                                     RID =:= TasklinkInfo#tasklink_info.leader_id andalso Type >= 31 andalso Type =< 99 ->
                                                         [#p_reward_log{timestamp=Timestamp-2,args_name= [],log_type = H*100+1,reward_view = []}
                                                         ,#p_reward_log{timestamp=Timestamp-1,args_name= [],log_type = H*100+2,reward_view = []}
                                                         ,AddLogReward1
                                                         ,#p_reward_log{timestamp=Timestamp+1,args_name= [get_reward_name(BaseReward)],log_type = ?TLLOG_TYPE_EVENT_ALLREWARS,reward_view = []}];
                                                     RID /= TasklinkInfo#tasklink_info.leader_id andalso Type >= 31 andalso Type =< 99 ->
                                                         [AddLogReward1];
                                                     true ->
                                                         []
                                                 end,
    %%                                 lists:foreach(fun(L)->
    %%                                     ?INFO("refresh_tasklink_info_list add ~w:~w",[L,L#p_reward_log.args_name])
    %%                                 end, lists:sort(NewAccLogAll)),
                                    NewAccLog = NewAccLogAll ++ AccLog,
                                    P#p_point_info{point_status = ?POINT_STATUS_ARRIVED_REWARD};
                                true ->
                                    NewAccLog = AccLog,
                                    P
                            end,
                        {[NewP|AccPoints],NewAccLog}
                    end,{[],TasklinkInfo#tasklink_info.reward_log},TasklinkInfo#tasklink_info.points),
                if
                    ArrivedNum < 20 ->
                        NewTasklinkInfo = 
                            TasklinkInfo#tasklink_info{next_timestamp = TasklinkInfo#tasklink_info.start_timestamp + ((ArrivedNum + 1)*TasklinkInfo#tasklink_info.point_need_time)
                                                      ,points = lists:reverse(NewPointsT)
                                                      ,reward_log = lists:sort(NewLog)},
%%                         refresh_tasklink_info_list(OtherList,Now,[NewTasklinkInfo|AccUpdateList],AccNextTs),
                        {OtherList,[NewTasklinkInfo|AccUpdateList],AccNextTs};
                    true ->
                        NewTasklinkInfo = 
                            TasklinkInfo#tasklink_info{next_timestamp = 0
                                                      ,points = lists:reverse(NewPointsT)
                                                      ,reward_log = lists:sort(NewLog)},
%%                         refresh_tasklink_info_list(OtherList,Now,[NewTasklinkInfo|AccUpdateList],AccNextTs),
                        {OtherList,[NewTasklinkInfo|AccUpdateList],AccNextTs}
                end;
            true ->
                NewNextTs = if 
                                TasklinkInfo#tasklink_info.next_timestamp > Now ->
                                    erlang:min(TasklinkInfo#tasklink_info.next_timestamp, AccNextTs);
                                true ->
                                    AccNextTs
                            end,
    %%             NewLog = add_random_log(TasklinkInfo,Now),
    %%             ?INFO("add_random_log(~w) ~w -> ~w   TasklinkInfo:~w",[TasklinkInfo#tasklink_info.role_id
    %%                                                                   ,erlang:length(TasklinkInfo#tasklink_info.reward_log),0
    %%                                                                   ,TasklinkInfo]),
%%                 refresh_tasklink_info_list(OtherList,Now,[TasklinkInfo|AccUpdateList],NewNextTs)
                {OtherList,[TasklinkInfo|AccUpdateList],NewNextTs}
    %%                 refresh_tasklink_info_list(OtherList,Now,[TasklinkInfo|AccUpdateList],NewNextTs)
        end,
    refresh_tasklink_info_list(RawList,Now,NewAccList,NewAccNextTs).

leader_double_reward({L1,L2})->
    {[leader_double_reward2(E)||E<-L1],[leader_double_reward2(E)||E<-L2]};
leader_double_reward({L1})->
    {[leader_double_reward2(E)||E<-L1]}.

leader_double_reward2({A,Num})->
    {A,Num*2};
leader_double_reward2({A,B,Num})->
    {A,B,Num*2};
leader_double_reward2(Unknown)->
    Unknown.

get_reward_name(L) when erlang:is_list(L)  ->
    [H|T] = L,
    case T of
        [] ->
            get_reward_name(H);
        _ ->
            get_reward_name(H) ++ "、" ++ get_reward_name(T)
    end;    
get_reward_name(L)->
    get_reward_name2(L).

get_reward_name2({?REWARD_GOLD,Value})->
    io_lib:format("~w钻石",[Value]);
get_reward_name2({?REWARD_COIN,Value})->
    io_lib:format("~w金币",[Value]);
get_reward_name2({?REWARD_REPU,Value})->
    io_lib:format("~w徽章",[Value]);
get_reward_name2({?REWARD_ROLE_EXP,Value})->
    io_lib:format("~w经验",[Value]);
get_reward_name2({?REWARD_ITEM,TypeID,_})->
    "道具";
get_reward_name2({?REWARD_GER,TypeID,_})->
    "精灵";
get_reward_name2(ERR)->
    ?INFO("get_reward_name ~w",[ERR]),
    "".

add_random_log(TasklinkInfo,Now)->
    case lists:reverse(lists:sort([P||P<-TasklinkInfo#tasklink_info.reward_log,P#p_reward_log.log_type > ?REWARD_LOG_TYPE_RANDOM_BASE])) of
        [] ->
            Num = if
                      Now > TasklinkInfo#tasklink_info.start_timestamp ->
                            (Now - TasklinkInfo#tasklink_info.start_timestamp) div ?persist_point_time;
                      true ->
                          0
                  end,
            ?INFO("add_random_log add Num:~w",[Num]),
            add_random_log2(max(Num,0),Now,TasklinkInfo#tasklink_info.reward_log,TasklinkInfo#tasklink_info.start_timestamp);
        [F|_] ->
            Num = if
                      Now > F#p_reward_log.timestamp ->
                            (Now - F#p_reward_log.timestamp) div ?persist_point_time;
                      true ->
                          0
                  end,
            ?INFO("add_random_log add Num:~w",[Num]),
            add_random_log2(max(Num,0),Now,TasklinkInfo#tasklink_info.reward_log,F#p_reward_log.timestamp)
    end.

add_random_log2(Large,_Now,_Acc,_StartTime) when Large > 600 ->
    ?ERR("add_random_log2 Large:~w",[Large]),
    [];
add_random_log2(0,_Now,Acc,_StartTime)->
    Acc;
add_random_log2(Num,Now,Acc,StartTime)->
    Timestamp = StartTime + Num*?persist_point_time,
    if
        Now > Timestamp ->
            L = #p_reward_log{timestamp= Timestamp
                         ,args_name= []
                         ,log_type = ?REWARD_LOG_TYPE_RANDOM_BASE + util:random_int(1, 10)
                         ,reward_view = []},
            add_random_log2(Num-1,Now,[L|Acc],Timestamp);
        true ->
            Acc
    end.

send_ready_notice(ReadyInfo)->
    #ready_info{member_list = MemberList
               ,leader_info = LeaderRoleInfo
               ,level_diffcult = DiffcultLevel
               ,end_timestamp = CloseTimestamp} = ReadyInfo,
    {value,Leader,OtherMemberList} = lists:keytake(LeaderRoleInfo#p_member_info.roleID, #p_member_info.roleID, MemberList),
    #sc_tasklink_ready_notice{members=[Leader|lists:sort(OtherMemberList)],close_timestamp=CloseTimestamp
                                ,diffcult_level=DiffcultLevel}.

%% ====================================================================
%% Debug functions
%% ====================================================================

debug_get_current_ready_list()->
    gen_server:call(tasklink_server, get_current_ready_list).

debug_get_log_type(RoleID)->
    case ets:lookup(?ETS_TASKLINK_LIST, RoleID) of
        [] ->
            [];
        [TasklinkInfo] ->
            io:format("~n--------debug_get_log_type-------~n", []),
            lists:map(fun(RL) -> 
                              io:format("tasklink_log:~w~n", [{RL#p_reward_log.log_type,RL#p_reward_log.args_name}]),
                              {RL#p_reward_log.log_type,RL#p_reward_log.args_name}
                      end, lists:sort(TasklinkInfo#tasklink_info.reward_log))
    end.