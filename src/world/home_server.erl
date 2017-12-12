-module(home_server).
-behaviour(gen_server).
-export([start/0,start_link/0
        ,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_home_data/0
        ,can_cirrus_big_reward/0
        ,add_cirrus_big_reward_count/0
        ,get_home/1
        ,set_home/2
        ,get_home_task/1
        ,get_homestead_buff/1
        ,get_park_level/1
        ,filter_task_type/1
		,get_dounty_ac_num/1
        ,add_dounty_ac_num/1]).
-export([show_ets/0,show_task/2,clean_task_info/1]).
%% -compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_homestead.hrl").			
-include("def_mail.hrl").  

-define(once_max, 500). %% 一次最多处理多少条
-define(read_timeout, 900). %% 15m没有人读取数据的话，则
-define(write_timeout, 300).    %% 数据被修改，5m之后，则将数据写入数据库。注：十分钟内如果数据再次修改不刷新时间戳
-define(persist_interval, 20000). %% 20s检查一次数据是否有需要写入的

%debug
%% -define(read_timeout, 60). %% 15m没有人读取数据的话，则
%% -define(write_timeout, 30).    %% 数据被修改，5m之后，则将数据写入数据库。注：十分钟内如果数据再次修改不刷新时间戳
%% -define(persist_interval, 10000). %% 20s检查一次数据是否有需要写入的

-define(constr_default_level, 1). %默认建筑物初始1级

-define(finish_task_timer, finish_task_timer).
-define(dounty_task_timer, dounty_task_timer).
-define(dounty_role_id, dounty_role_id). %home_server初始化和设定悬赏任务时会添加数据。读取全部悬赏数据时，如果发现悬赏已经没有了，则清除对应id。（这样写简单一点）

-record(home_db_record, {role_id=0
                        ,home_info=#home_info{}
                        ,write_ts=0             %最后一次写数据是什么时间，0表示已经写入数据库了，已同步无需再写入
                        ,read_ts=0}).           %最后一次读数据是什么时间       

-record(home_task_db_record, {role_id=0
                        ,home_task_list=[#home_task{}]
                        ,write_ts=0             %最后一次写数据是什么时间，0表示已经写入数据库了，已同步无需再写入
                        ,read_ts=0}).           %最后一次读数据是什么时间       

-record(home_server_data, {cirrus_big_reward_date = {0,0,0}
                          ,cirrus_big_reward_count = 0}).

-record(state, {}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    HomeData = case db_sql:get_etc(?DB_ETC_KEY_HOME) of
        #home_server_data{} = X ->
            X;
        _ ->
            #home_server_data{}
    end,
    ets:delete_all_objects(?ETS_HOME_DATA),
    ets:insert_new(?ETS_HOME_DATA, HomeData),
	process_flag(trap_exit,true),
    random:seed(util:gen_random_seed()),
    init_etc_cache(),
    set_persist_interval(),
    init_task_timer(),
    {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info(Info, State) ->
	NewState = do_handle_info(Info, State),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    persist_immediately(false),
    db_sql:set_etc(?DB_ETC_KEY_HOME, get_home_data()),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Api functions 
%% ====================================================================

get_home_data()->
    [HomeData] = ets:lookup(?ETS_HOME_DATA, home_server_data),
    Data = erlang:date(),
    if
        HomeData#home_server_data.cirrus_big_reward_date =:= Data ->
            HomeData;
        true ->
            NewHomeData = HomeData#home_server_data{cirrus_big_reward_date = Data
                                                   ,cirrus_big_reward_count = 0},
            ets:insert(?ETS_HOME_DATA, NewHomeData),
            NewHomeData
    end.

can_cirrus_big_reward()->
    HomeData = get_home_data(),
    MaxTime = data_home:get(cirrus_big_reward_count),
    if
        HomeData#home_server_data.cirrus_big_reward_count >= MaxTime ->
            false;
        true ->
            true
    end.

% 本来这里应该同步操作，防止多线程出问题，但是获得大奖几率很低，同时发的可能更小，所以就不必同步操作或加锁了
add_cirrus_big_reward_count()->
    HomeData = get_home_data(),
    NewCount = HomeData#home_server_data.cirrus_big_reward_count + 1,
    NewHomeData = HomeData#home_server_data{cirrus_big_reward_count = NewCount},
    ets:insert(?ETS_HOME_DATA, NewHomeData),
    NewHomeData.

get_home(RoleID)->
    case ets:lookup(?ETS_HOME_LIST, RoleID) of
        []->
            HomeInfo0 = db_sql:get_home_info(RoleID),
            ConstrList0 = HomeInfo0#home_info.constr_list,
            ConstrList = lists:foldl(fun(Type,AccList)->
                                             case lists:keymember(Type, 1, ConstrList0) of
                                                 true ->
                                                     AccList;
                                                 false -> 
                                                     [{Type,?constr_default_level}|AccList]
                                             end
                                     end, ConstrList0, data_home:get(c_info_type)),
            HomeInfo = HomeInfo0#home_info{constr_list=ConstrList},
            HomeRecord = #home_db_record{role_id=RoleID
                                        ,home_info=HomeInfo
                                        ,write_ts=0
                                        ,read_ts=util:now()},   %最后一次读数据是什么时间
            ets:insert_new(?ETS_HOME_LIST, HomeRecord),
            HomeInfo;
        [HomeRecord] when erlang:is_record(HomeRecord, home_db_record) ->
            ets:update_element(?ETS_HOME_LIST, RoleID,{#home_db_record.read_ts,util:now()}),
            HomeRecord#home_db_record.home_info
    end.

% 逻辑正常的情况下，不可能未读取而写
set_home(RoleID,NewHomeInfo)->
    case ets:lookup(?ETS_HOME_LIST, RoleID) of
        [OldHomeRecord] when erlang:is_record(OldHomeRecord, home_db_record) ->
            ets:update_element(?ETS_HOME_LIST, RoleID,[{#home_db_record.home_info,NewHomeInfo}
                                                      ,{#home_db_record.write_ts,util:now()}])
    end.

get_home_task(RoleID) ->
    case ets:lookup(?ETS_HOME_TASK_LIST, RoleID) of
        []->
            TaskList = db_sql:get_home_task_info(RoleID),
            TaskRecord = #home_task_db_record{role_id=RoleID
                                             ,home_task_list=TaskList
                                             ,write_ts=0
                                             ,read_ts=util:now()},   %最后一次读数据是什么时间
            ets:insert_new(?ETS_HOME_TASK_LIST, TaskRecord),
            TaskList;
        [TaskRecord] when erlang:is_record(TaskRecord, home_task_db_record) ->
            ets:update_element(?ETS_HOME_TASK_LIST, RoleID,{#home_task_db_record.read_ts,util:now()}),
            TaskRecord#home_task_db_record.home_task_list
    end.

% 逻辑正常的情况下，不可能未读取而写
set_home_task(RoleID,TaskList)->
    case ets:lookup(?ETS_HOME_TASK_LIST, RoleID) of
        [OldTaskRecord] when erlang:is_record(OldTaskRecord, home_task_db_record) ->
            ets:update_element(?ETS_HOME_TASK_LIST, RoleID,[{#home_task_db_record.home_task_list,TaskList}
                                                      ,{#home_task_db_record.write_ts,util:now()}])
    end.
    
get_homestead_buff(RoleID)->
    HomeInfo = get_home(RoleID),
    HomeLevel = HomeInfo#home_info.stage,
    {value,{?constr_type_homestead,Level}} = lists:keysearch(?constr_type_homestead, 1, HomeInfo#home_info.constr_list),
    HomesteadBuff = data_home:get(homestead_buff),
    util:floor(Level*HomeLevel*HomesteadBuff).

get_park_level(RoleID)->
    HomeInfo = get_home(RoleID),
    {value,{?constr_type_park,Level}} = lists:keysearch(?constr_type_park, 1, HomeInfo#home_info.constr_list),
    Level.

get_dounty_ac_num(RoleID)->
    HomeInfo = get_home(RoleID),
    CurDate = erlang:date(),
    if
        HomeInfo#home_info.dounty_ac_date =:= CurDate ->
            HomeInfo#home_info.dounty_ac_num;
        true ->
            0
    end.

add_dounty_ac_num(RoleID)->
    HomeInfo = get_home(RoleID),
    CurDate = erlang:date(),
    NewHomeInfo = if
        HomeInfo#home_info.dounty_ac_date =:= CurDate ->
            OldNum = HomeInfo#home_info.dounty_ac_num,
            HomeInfo#home_info{dounty_ac_num=OldNum+1};
        true ->
            HomeInfo#home_info{dounty_ac_num=1,dounty_ac_date=CurDate}
    end,
    set_home(RoleID,NewHomeInfo).

%% ====================================================================
%% Internal functions
%% ====================================================================

do_handle_info({add_new_home_task,TaskOwner,AddTaskList}, State) ->
    ?INFO("add_new_home_task ~w ~w",[TaskOwner,AddTaskList]),
    OldTaskList = get_home_task(TaskOwner),
    NewTaskList = AddTaskList++OldTaskList,
    PkLv = get_park_level(TaskOwner),
    TaskNum = 5 + (PkLv div 5),
    TaskListLen = erlang:length([E||E<-NewTaskList,E#home_task.onwer_role_id =:= TaskOwner 
                                andalso (E#home_task.status =:= ?ht_normal_unaccept 
                                         orelse E#home_task.status =:= ?ht_normal_accepted
                                         orelse E#home_task.status =:= ?ht_dounty_unaccept)]),
%%     TaskListLen = erlang:length(NewTaskList),
    if
        TaskNum >= TaskListLen->
            set_home_task(TaskOwner,NewTaskList),
            case role_lib:is_online(TaskOwner) of
                true ->
                    ?unicast(TaskOwner,#sc_home_task_info{task_list= filter_task_type(NewTaskList)});
                false ->
                    ignore
            end;
        true ->
%%             ?ERR("add_new_home_task -> num error ~w ~w ~w",[TaskNum>=TaskListLen,TaskNum,TaskListLen]),
            [db_sql:del_home_task(T#home_task.id)||T<-AddTaskList]
    end,
    State;
do_handle_info({refersh_task_type,TaskOwner,RefreshL,TaskStatus}, State) ->
    ?INFO("refersh_task_type ~w ~w",[TaskOwner,RefreshL]),
    OldTaskL = get_home_task(TaskOwner),
    NewTaskList = lists:foldr(fun(NewTaskInfo,Acc)->
                                      case lists:keytake(NewTaskInfo#home_task.id, #home_task.id, Acc) of
                                          {value,Task,OtherL} 
                                            when Task#home_task.status =:= TaskStatus ->
                                              %新的下一个Timer是在role_server中设定的
                                              [NewTaskInfo|OtherL];
                                          _ ->
                                              Acc
                                      end
                              end, OldTaskL, RefreshL),
    set_home_task(TaskOwner,NewTaskList),
    case role_lib:is_online(TaskOwner) of
        true ->
            ?unicast(TaskOwner,#sc_home_task_info{task_list= filter_task_type(NewTaskList)});
        false ->
            ignore
    end,
    State;
%% 注意，接受的任务可能是自己的，也可能是别人的悬赏任务
do_handle_info({accept_task,AptRoleID,RoleLevel,NewTask,TaskOwner}, State) ->
    TaskId = NewTask#home_task.id,
    NextNeedTime = NewTask#home_task.timtout_ts,
    Now = util:now(),
    ?INFO("accept_task ~w ~w ~ws",[AptRoleID,TaskId,NextNeedTime - Now]),
    OldTaskL = get_home_task(TaskOwner),
    case lists:keytake(TaskId, #home_task.id, OldTaskL) of
        {value,OldTask,OtherL} when (OldTask#home_task.status =:= ?ht_normal_unaccept orelse OldTask#home_task.status =:= ?ht_dounty_unaccept) 
          andalso OldTask#home_task.onwer_role_id =:= TaskOwner ->
            NewTaskList = [NewTask|OtherL],
            set_home_task(TaskOwner,NewTaskList),
            ?INFO("accept_task set finish timer after ~w task:~w",[NextNeedTime - Now,NewTask]),
            FinishTimerRef = 
                erlang:start_timer((NextNeedTime - Now)*1000, self(), {finish_task,AptRoleID,TaskId}),
            put({?finish_task_timer,TaskId},FinishTimerRef),
            if
                %% 需要撤掉定时器
                OldTask#home_task.status =:= ?ht_dounty_unaccept ->
                    case get({?dounty_task_timer,TaskId}) of
                        ?undefined ->
                            ?ERR("lost timer ~w ~w",[AptRoleID,TaskId]),
                            ignore;
                        Ref ->
                            erlang:cancel_timer(Ref),
                            erlang:erase({?dounty_task_timer,TaskId})
                    end,
                    if
                        %别人的悬赏
                        NewTask#home_task.onwer_role_id /= AptRoleID ->
                            AptNewTaskList = [NewTask|get_home_task(AptRoleID)],
                            ?INFO("Acpter Task ~w<<<>>>~w",[NewTask,AptNewTaskList]),
                            set_home_task(AptRoleID,AptNewTaskList),
                            add_dounty_ac_num(AptRoleID),
                            case role_lib:is_online(AptRoleID) of
                                true -> ?unicast(AptRoleID,#sc_home_task_info{task_list= filter_task_type(AptNewTaskList)});
                                false -> ignore
                            end;
                        true -> ignore
                    end;
                true -> ignore
            end,
            ?INFO("start_timer ~w ~w ",[(NextNeedTime - util:now())*1000, {finish_task,AptRoleID,TaskId}]),
            case role_lib:is_online(TaskOwner) of
                true ->
                    ?unicast(TaskOwner,#sc_home_task_info{task_list= filter_task_type(NewTaskList)});
                false ->
                    ignore
            end;
        {value,OldTask,_OtherL} ->
            ?ERR("accept_task error a:~w o:~w t:~w s:~w to:~w",[AptRoleID,TaskOwner,TaskId,OldTask#home_task.status,OldTask#home_task.onwer_role_id]),
            ignore;
        _ ->
            ?ERR("accept_task not found a:~w o:~w t:~w",[AptRoleID,TaskOwner,TaskId]),
            ignore
    end,
    State;
do_handle_info({set_bounty,TaskOwner,TaskId},State) ->
    ?INFO("set_bounty ~w ~w",[TaskOwner,TaskId]),
    OldTaskL = get_home_task(TaskOwner),
    case lists:keytake(TaskId, #home_task.id, OldTaskL) of
        {value,Task,OtherL} when Task#home_task.status =:= ?ht_normal_unaccept ->
            %新的下一个Timer是在role_server中设定的
            Now = util:now(),
            Timeout = data_home:get(dounty_task_timeout),
            NewTaskList = [Task#home_task{status=?ht_dounty_unaccept,timtout_ts=Now+Timeout}|OtherL],
            set_home_task(TaskOwner,NewTaskList),
            Ref = erlang:start_timer(Timeout*1000, self(), {dounty_task_timeout,TaskOwner,TaskId}),
            put({?dounty_task_timer,TaskId},Ref),
            DountyRoleIdList = get(?dounty_role_id),
            case lists:member(TaskOwner, DountyRoleIdList) of
                false ->
                    put(?dounty_role_id,[TaskOwner|DountyRoleIdList]);
                true ->
                    ignore
            end,
            case role_lib:is_online(TaskOwner) of
                true ->
                    ?unicast(TaskOwner,#sc_home_task_operate{result= 1}),
                    ?unicast(TaskOwner,#sc_home_task_info{task_list= filter_task_type(NewTaskList)});
                false ->
                    ignore
            end;
        _ ->
            ?unicast(TaskOwner,#sc_home_task_operate{result= 2})
    end,
    State;
do_handle_info({set_cancel,TaskOwner,TaskId},State) ->
    ?INFO("set_cancel ~w ~w",[TaskOwner,TaskId]),
    OldTaskL = get_home_task(TaskOwner),
    case lists:keytake(TaskId, #home_task.id, OldTaskL) of
        {value,Task,OtherL} when Task#home_task.status =:= ?ht_normal_accepted ->
            set_home_task(TaskOwner,OtherL),
            db_sql:del_home_task(TaskId),
            case get({?finish_task_timer,TaskId}) of
                ?undefined ->
                    ?ERR("c set_cancel !! BUT erase fail ~w",[TaskId]);
                TimerRef ->
                    erlang:cancel_timer(TimerRef),
                    erlang:erase({?finish_task_timer,TaskId})
            end,
            ?unicast(TaskOwner,#sc_home_task_operate{result= 1}),
            case role_lib:is_online(TaskOwner) of
                true ->
                    ?unicast(TaskOwner,#sc_home_task_info{task_list= filter_task_type(OtherL)}),
                    catch role_lib:send_server(TaskOwner, {cancel_del_ger,Task#home_task.ger_id});
                false ->
                    ignore
            end;
        _ ->
            ?unicast(TaskOwner,#sc_home_task_operate{result= 2})
    end,
    State;
do_handle_info({cs_home_bounty_task_info,RoleID},State) ->
    DountyRoleIdList = get(?dounty_role_id),
    {BountyTaskList,NewDountyRoleIdList} = lists:foldr(fun(FriendId,{BountyTaskAccList,DountyRoleIdAccList})-> 
                                        FTaskList0 = get_home_task(FriendId),
                                        FTaskList = lists:filter(fun(Task) ->
                                                Task#home_task.status =:= ?ht_dounty_unaccept
                                            end, FTaskList0),
                                        case FTaskList of
                                            [] ->
                                                {BountyTaskAccList,DountyRoleIdAccList};
                                            _ ->
                                                {FTaskList ++ BountyTaskAccList,[FriendId|DountyRoleIdAccList]}
                                        end
                                 end, {[],[]}, DountyRoleIdList), % 加入自己，剔除重复
    put(?dounty_role_id,NewDountyRoleIdList),
    SelfBountyTask =[T||T<-get_home_task(RoleID),T#home_task.status =:= ?ht_dounty_unaccept orelse T#home_task.status =:= ?ht_dounty_accepted],
    AllTaskList = [T#home_task{base_reward_num = T#home_task.base_reward_num div ?home_exchange_rate} || T<-(SelfBountyTask ++ BountyTaskList)],
    ?unicast(RoleID,#sc_home_bounty_task_info{task_list= AllTaskList
                                             ,accept_num=home_server:get_dounty_ac_num(RoleID)
                                             ,accept_max=data_home:get(dounty_ac_max)}),
    State;
do_handle_info({timeout, TimerRef, {dounty_task_timeout,TaskOwner,TaskId}},State) ->
    dounty_task_timeout(TaskOwner,TaskId),
    State;
do_handle_info({timeout, TimerRef, {finish_task,AptRoleID,TaskId}},State) ->
    case get({?finish_task_timer,TaskId}) of
        TimerRef ->
            erlang:erase({?finish_task_timer,TaskId});
        R ->
            ?INFO("f timeout active !! BUT erase fail ~w ~w ~w",[R,AptRoleID,TaskId])
    end,
    finish_task(AptRoleID,TaskId),
    State;

do_handle_info(fix_check_task1, State) ->
    do_fix_check_task1(),
    State;

do_handle_info({persist_immediately,IsAll}, State) ->
    persist_immediately(IsAll),
    State;
do_handle_info(persist_tick, State) ->
    persist_check(),
    set_persist_interval(),
    State;
do_handle_info(_Info, State) ->
    ?ERR("unkown do_handle_info ~w",[_Info]),
    State.

%设定下次timer
set_persist_interval()->
    erlang:send_after(?persist_interval, self(), persist_tick).

% {_,A,B,C,D} = #home_db_record{}
%ets:fun2ms(fun({_,A,B,W,R}) when R > 0 andalso R < 999 -> {A,B} end).
persist_check()->
%%     ?INFO("persist_check ",[]),
    Now = util:now(),
    WriteLimit = Now - ?write_timeout,
    %检查Home写入
    HomeResW = 
        ets:select(?ETS_HOME_LIST, [{{'_','$1','$2','$3','_'},
                                      [{'andalso',{'>','$3',0},{'<','$3',WriteLimit}}],
                                      [{{'$1','$2'}}]}]
                                ,?once_max),
    case HomeResW of
        {HomeNeedWriteList,_} ->
            ?INFO("HomeNeedWriteList is (~w) ~w",[erlang:length(HomeNeedWriteList),HomeNeedWriteList]),
            lists:foreach(fun({RoleID,HomeInfo})-> 
                                  db_sql:persist_home_info(RoleID,HomeInfo),
                                  ets:update_element(?ETS_HOME_LIST, RoleID,{#home_db_record.write_ts,0})
                          end, HomeNeedWriteList);
        _ ->
            ignore
    end,
    %检查HomeTask写入
    TaskResW = 
        ets:select(?ETS_HOME_TASK_LIST, [{{'_','$1','$2','$3','_'},
                                      [{'andalso',{'>','$3',0},{'<','$3',WriteLimit}}],
                                      [{{'$1','$2'}}]}]
                                ,?once_max),
    case TaskResW of
        {TaskHomeNeedWriteList,_} ->
            ?INFO("TaskHomeNeedWriteList is (~w) ~w",[erlang:length(TaskHomeNeedWriteList),TaskHomeNeedWriteList]),
            lists:foreach(fun({RoleID,[]})-> 
                                  ets:update_element(?ETS_HOME_TASK_LIST, RoleID,{#home_task_db_record.write_ts,0});
                             ({RoleID,TaskInfoList})-> 
                                  db_sql:persist_home_task_info(TaskInfoList),
                                  ets:update_element(?ETS_HOME_TASK_LIST, RoleID,{#home_task_db_record.write_ts,0})
                          end, TaskHomeNeedWriteList);
        _ ->
            ignore
    end,
    %检查Home数据是否需要从缓存中删除
    ReadLimit = Now - ?read_timeout,
    HomeResR = 
        ets:select(?ETS_HOME_LIST, [{{'_','$1','_','$2','$3'},
                                      [{'andalso',{'=:=','$2',0},{'<','$3',ReadLimit}}],
                                      [{{'$1'}}]}]
                                ,?once_max),
    case HomeResR of
        {NeedDelList1,_} ->
            ?INFO("NeedDelList1 is ~w",[erlang:length(NeedDelList1)]),
            lists:foreach(fun({RoleID})-> 
                                  ets:delete(?ETS_HOME_LIST, RoleID)
                          end, NeedDelList1);
        _ ->
            ignore
    end,
    %检查Home数据是否需要从缓存中删除
    TaskResR = 
        ets:select(?ETS_HOME_TASK_LIST, [{{'_','$1','_','$2','$3'},
                                      [{'andalso',{'=:=','$2',0},{'<','$3',ReadLimit}}],
                                      [{{'$1'}}]}]
                                ,?once_max),
    case TaskResR of
        {NeedDelList2,_} ->
            ?INFO("NeedDelList2 is ~w",[erlang:length(NeedDelList2)]),
            lists:foreach(fun({RoleID})-> 
                                  ets:delete(?ETS_HOME_TASK_LIST, RoleID)
                          end, NeedDelList2);
        _ ->
            ignore
    end.

%立即把所有数据写入数据库
persist_immediately(IsAll)->
    HomeResW = if
               IsAll ->
                    ets:select(?ETS_HOME_LIST
                              ,[{{'_','$1','$2','$3','_'},[],[{{'$1','$2'}}]}]
                              ,?once_max);
               true ->
                    ets:select(?ETS_HOME_LIST
                              ,[{{'_','$1','$2','$3','_'},[{'>','$3',0}],[{{'$1','$2'}}]}]
                              ,?once_max)
           end,
    case HomeResW of
        {HomeNeedWriteList,_} ->
            lists:foreach(fun({RoleID,HomeInfo})-> 
                                  db_sql:persist_home_info(RoleID,HomeInfo),
                                  ets:update_element(?ETS_HOME_LIST, RoleID,{#home_db_record.write_ts,0})
                          end, HomeNeedWriteList);
        _ ->
            ignore
    end,
    TaskResW = if
               IsAll ->
                    ets:select(?ETS_HOME_TASK_LIST
                              ,[{{'_','$1','$2','$3','_'},[],[{{'$1','$2'}}]}]
                              ,?once_max);
               true ->
                    ets:select(?ETS_HOME_TASK_LIST
                              ,[{{'_','$1','$2','$3','_'},[{'>','$3',0}],[{{'$1','$2'}}]}]
                              ,?once_max)
           end,
    case TaskResW of
        {TaskNeedWriteList,_} ->
            lists:foreach(fun({RoleID,[]})-> 
                                  ets:update_element(?ETS_HOME_TASK_LIST, RoleID,{#home_task_db_record.write_ts,0});
                             ({RoleID,TaskInfo})-> 
                                  db_sql:persist_home_task_info(TaskInfo),
                                  ets:update_element(?ETS_HOME_TASK_LIST, RoleID,{#home_task_db_record.write_ts,0})
                          end, TaskNeedWriteList);
        _ ->
            ignore
    end.

%注意，这里是依照accept_role_id作为索引的
init_task_timer()->
    List = db_sql:get_task_need_timer(),
    ?INFO("init_task_timer ~w",[erlang:length(List)]),
    Now = util:now(),
    put(?dounty_role_id,[]),
    lists:foreach(fun([TId,AptRoleID,OwnerRoleID,Status,Ts])->
        if
            Now < Ts andalso (Status =:= ?ht_normal_accepted orelse Status =:= ?ht_dounty_accepted)->
                erlang:start_timer((Ts - Now)*1000, self(), {finish_task,AptRoleID,TId}),
                ?INFO("init start_timer ~w ~w ",[(Ts - Now), {finish_task,AptRoleID,TId}]);
            Now >= Ts andalso (Status =:= ?ht_normal_accepted orelse Status =:= ?ht_dounty_accepted) ->
                finish_task(AptRoleID,TId);
            Now < Ts andalso Status =:= ?ht_dounty_unaccept ->
                Ref = erlang:start_timer((Ts - Now)*1000, self(), {dounty_task_timeout,OwnerRoleID,TId}),
                put({?dounty_task_timer,TId},Ref),
                OldDountyRoleIdList = get(?dounty_role_id),
                case lists:member(OwnerRoleID, OldDountyRoleIdList) of
                    false ->
                        put(?dounty_role_id,[OwnerRoleID|OldDountyRoleIdList]);
                    true ->
                        ignore
                end;
            Now >= Ts andalso Status =:= ?ht_dounty_unaccept  ->
                dounty_task_timeout(OwnerRoleID,TId)
        end
    end, List).
    
dounty_task_timeout(TaskOwner,TaskId)->
    ?INFO("dounty_task_timeout ~w ~w",[TaskOwner,TaskId]),
    OldTaskL = get_home_task(TaskOwner),
    case lists:keytake(TaskId, #home_task.id, OldTaskL) of
        {value,Task,OtherL} when Task#home_task.status =:= ?ht_dounty_unaccept ->
            case role_lib:is_online(TaskOwner) of
                true ->
                    catch role_lib:send_server(TaskOwner, {dounty_task_timeout,Task#home_task.id});
                false ->
                    % 玩家再上线的时候，会刷新任务内容
                    NewTaskL = [Task#home_task{status=?ht_normal_unaccept,timtout_ts=util:now()-1}|OtherL],
                    set_home_task(TaskOwner,NewTaskL)
            end,
            {TaskName} = data_home:get({home_task,Task#home_task.quality,Task#home_task.task_type}),
            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_TIMEOUT, [TaskName], "", []);
        _ ->
            ?ERR("dounty_task_timeout error task not found ~w ~w",[TaskOwner,TaskId]),
            ignore
    end,
    erlang:erase({?dounty_task_timer,TaskId}).
%%     case get({?dounty_task_timer,TaskId}) of
%%         TimerRef ->
%%             erlang:erase({?dounty_task_timer,TaskId});
%%         R ->
%%             ?ERR("timeout active !! BUT erase fail ~w",[R])
%%     end.
    
finish_task(AptRoleID,TaskId)->
    ?INFO("timer finish_task ~w ~w",[AptRoleID,TaskId]),
    OldTaskL = get_home_task(AptRoleID), %%接收方和拥有方，都记录了任务
    case lists:keytake(TaskId, #home_task.id, OldTaskL) of
        {value,Task,OtherTaskList} when Task#home_task.role_id =:= AptRoleID andalso 
          Task#home_task.status =:= ?ht_normal_accepted orelse Task#home_task.status =:= ?ht_dounty_accepted->
            set_home_task(AptRoleID,OtherTaskList),
            db_sql:del_home_task(TaskId),
            case role_lib:is_online(AptRoleID) of
                true ->
                    catch role_lib:send_server(AptRoleID, {cancel_del_ger,Task#home_task.ger_id}),
                    ?unicast(AptRoleID,#sc_home_task_info{task_list= filter_task_type(OldTaskL)});
                false ->
                    ignore
            end,
            TaskOwner = Task#home_task.onwer_role_id,
            if
                Task#home_task.status =:= ?ht_dounty_accepted andalso TaskOwner /= AptRoleID ->
                    NewOwnerTaskList = lists:keydelete(TaskId, #home_task.id, get_home_task(TaskOwner)),
                    set_home_task(TaskOwner,NewOwnerTaskList),
                    case role_lib:is_online(TaskOwner) of
                        true ->
                            ?unicast(TaskOwner,#sc_home_task_info{task_list= filter_task_type(NewOwnerTaskList)});
                        false ->
                            ignore
                    end;
                true -> ignore
            end,
            Rand1 = random:uniform(1000),
            Rand2 = random:uniform(1000),
            WinRate = role_home:cacl_win_rate(Task),
            IsWin = WinRate > Rand1,
            IsBox = (WinRate div 2) > Rand2,
            RawNum = Task#home_task.base_reward_num div ?home_exchange_rate,
            HomeItem  = case TaskId rem 2 of
                            0 -> ?home_stone;
                            1 -> ?home_wood end,
            BaseReward = {sell_reward,0,0,0,0,[{new_item,HomeItem,RawNum,1,0}],0,[]},
%%             Num = erlang:length(Task#home_task.ger_type),
            HalfReward = {sell_reward,0,0,0,0,[{new_item,HomeItem,RawNum div 2,1,0}],0,[]},
            TQuality = Task#home_task.quality,
            BoxReward = activity_server:p_reward_info2sell_reward(Task#home_task.box_reward),
%%             {_LevelAmend,_RandNumList,RandRTList} = data_home:get({task_quality, TQuality}),
%%             RewardType = util:random_one_from_list(RandRTList),
%%             BoxReward = role_home:cacl_reward(RewardType,TQuality,Task#home_task.level),
            ?INFO("new task reward(~w,~w,~w) ~w",[TQuality,Task#home_task.level,Task#home_task.base_reward_num,BoxReward]),
%%             BoxReward = util:random_one_from_weigh_list(data_home:get({box,100+Task#home_task.quality})),
            ExReward = role_reward:reward_plus_reward(BaseReward,BoxReward),
            BoxHalfReward = role_reward:reward_plus_reward(HalfReward,BoxReward),
%%             ExReward = role_reward:reward_plus_reward()
%%                                                      , BaseReward),
            {TaskName} = data_home:get({home_task,TQuality,Task#home_task.task_type}),
            #rolePublic{roleName=OwnerName} = role_lib:get_rolePublic(TaskOwner),
            #rolePublic{roleName=AptName} = role_lib:get_rolePublic(AptRoleID),
            if 
                IsWin andalso IsBox->
                    if
                        Task#home_task.status =:= ?ht_dounty_accepted andalso TaskOwner /= AptRoleID ->
                            Rand3 = random:uniform(1000),
                            if
                                500 > Rand3 ->
                                    mail_server:send_sys_mail(AptRoleID, ?MAIL_HOMETASK_BOUNTY_ACP_REWARD1, [OwnerName,TaskName], "", HalfReward),
                                    mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_OWN_REWARD2, [AptName,TaskName], "", BoxHalfReward);
                                true ->
                                    mail_server:send_sys_mail(AptRoleID, ?MAIL_HOMETASK_BOUNTY_ACP_REWARD2, [OwnerName,TaskName], "", BoxHalfReward),
                                    mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_OWN_REWARD1, [AptName,TaskName], "", HalfReward)
                            end;
                        Task#home_task.status =:= ?ht_dounty_accepted -> 
                            ?ERR("dounty task error 不会领取自己的悬赏"),
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_OWN_REWARD2, [AptName,TaskName], "", ExReward);
                        true -> 
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_NORMAL_REWARD2, [TaskName], "", ExReward)
                    end,
                    ?INFO("Home Task Is Win +++++ Apt：~w Owner:~w ~w ~w",[AptRoleID,Task#home_task.onwer_role_id,WinRate,Rand1]);
                IsWin ->
                    if
                        Task#home_task.status =:= ?ht_dounty_accepted andalso TaskOwner /= AptRoleID ->
                            mail_server:send_sys_mail(AptRoleID, ?MAIL_HOMETASK_BOUNTY_ACP_REWARD1, [OwnerName,TaskName], "", HalfReward),
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_OWN_REWARD1, [AptName,TaskName], "", HalfReward);
                        Task#home_task.status =:= ?ht_dounty_accepted -> 
                            ?ERR("dounty task error 不会领取自己的悬赏"),
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_OWN_REWARD1, [AptName,TaskName], "", BaseReward);
                        true -> 
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_NORMAL_REWARD1, [TaskName], "", BaseReward)
                    end,
                    ?INFO("Home Task Is Win +++++ Apt：~w Owner:~w ~w ~w",[AptRoleID,Task#home_task.onwer_role_id,WinRate,Rand1]);
                true ->
                    if
                        Task#home_task.status =:= ?ht_dounty_accepted andalso TaskOwner /= AptRoleID ->
                            mail_server:send_sys_mail(AptRoleID, ?MAIL_HOMETASK_BOUNTY_ACP_FAIL, [OwnerName,TaskName], "", []),
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_OWN_FAIL, [AptName,TaskName], "", []);
                        Task#home_task.status =:= ?ht_dounty_accepted ->
                            ?ERR("dounty task error 不会领取自己的悬赏"),
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_BOUNTY_OWN_FAIL, [AptName,TaskName], "", []);
                        true -> 
                            mail_server:send_sys_mail(TaskOwner, ?MAIL_HOMETASK_NORMAL_FAIL, [TaskName], "", [])
                    end,    
                    ?INFO("Home Task Is Lose ----- Apt：~w Owner:~w ~w ~w",[AptRoleID,Task#home_task.onwer_role_id,WinRate,Rand1])
            end;
        _ ->
            ignore
    end.

init_etc_cache()->
    {GS2,GS3,GS4,GS5,GS6,GS7} = 
        lists:foldl(fun(Id,{AccGS2,AccGS3,AccGS4,AccGS5,AccGS6,AccGS7})->
                case data_ger:get(Id) of
                    ?undefined ->
                        {AccGS2,AccGS3,AccGS4,AccGS5,AccGS6,AccGS7};
                    GerData ->
                        GerTypeID = GerData#data_ger.gerTypeID,
                        if
                            (GerTypeID rem 10) =:= 0 ->
                                if
                                    2000 < GerTypeID andalso GerTypeID < 3000 ->
                                        {[GerTypeID|AccGS2],AccGS3,AccGS4,AccGS5,AccGS6,AccGS7};
                                    3000 < GerTypeID andalso GerTypeID < 4000->
                                        {AccGS2,[GerTypeID|AccGS3],AccGS4,AccGS5,AccGS6,AccGS7};
                                    4000 < GerTypeID andalso GerTypeID < 5000->
                                        {AccGS2,AccGS3,[GerTypeID|AccGS4],AccGS5,AccGS6,AccGS7};
                                    5000 < GerTypeID andalso GerTypeID < 6000->
                                        {AccGS2,AccGS3,AccGS4,[GerTypeID|AccGS5],AccGS6,AccGS7};
                                    6000 < GerTypeID andalso GerTypeID < 7000->
                                        {AccGS2,AccGS3,AccGS4,AccGS5,[GerTypeID|AccGS6],AccGS7};
                                    7000 < GerTypeID andalso GerTypeID =< 7120->
                                        {AccGS2,AccGS3,AccGS4,AccGS5,AccGS6,[GerTypeID|AccGS7]};
                                    true ->
                                        {AccGS2,AccGS3,AccGS4,AccGS5,AccGS6,AccGS7}
                                end;
                            true ->
                                {AccGS2,AccGS3,AccGS4,AccGS5,AccGS6,AccGS7}
                        end
                end
        end, {[],[],[],[],[],[]}, lists:seq(1, 9999)),
    {ES1,ES2,ES3,ES4,ES5,ES6,ES7} = 
        lists:foldl(fun(Id,{AccES1,AccES2,AccES3,AccES4,AccES5,AccES6,AccES7})->
                case data_item:get(Id) of
                    ?undefined ->
                        {AccES1,AccES2,AccES3,AccES4,AccES5,AccES6,AccES7};
                    Ger ->
                        ItemrTypeID = Ger#data_item.itemTypeID,
                        if
                            10100 =< ItemrTypeID andalso ItemrTypeID < 10200 ->
                                {[ItemrTypeID|AccES1],AccES2,AccES3,AccES4,AccES5,AccES6,AccES7};
                            10200 =< ItemrTypeID andalso ItemrTypeID < 10300 ->
                                {AccES1,[ItemrTypeID|AccES2],AccES3,AccES4,AccES5,AccES6,AccES7};
                            10300 =< ItemrTypeID andalso ItemrTypeID < 10400 ->
                                {AccES1,AccES2,[ItemrTypeID|AccES3],AccES4,AccES5,AccES6,AccES7};
                            10400 =< ItemrTypeID andalso ItemrTypeID < 10500 ->
                                {AccES1,AccES2,AccES3,[ItemrTypeID|AccES4],AccES5,AccES6,AccES7};
                            10500 =< ItemrTypeID andalso ItemrTypeID < 10600 ->
                                {AccES1,AccES2,AccES3,AccES4,[ItemrTypeID|AccES5],AccES6,AccES7};
                            10600 =< ItemrTypeID andalso ItemrTypeID < 10700 ->
                                {AccES1,AccES2,AccES3,AccES4,AccES5,[ItemrTypeID|AccES6],AccES7};
                            10700 =< ItemrTypeID andalso ItemrTypeID =< 10718 ->
                                {AccES1,AccES2,AccES3,AccES4,AccES5,AccES6,[ItemrTypeID|AccES7]};
                            true ->
                                {AccES1,AccES2,AccES3,AccES4,AccES5,AccES6,AccES7}
                        end
                end
        end, {[],[],[],[],[],[],[]}, lists:seq(1, 49999)),
    ets:insert(?ETS_CACHE_CONFIG_ID, {equip_1,ES1}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {equip_2,ES2}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {equip_3,ES3}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {equip_4,ES4}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {equip_5,ES5}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {equip_6,ES6}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {equip_7,ES7}),
    
    ets:insert(?ETS_CACHE_CONFIG_ID, {ger_2,GS2}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {ger_3,GS3}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {ger_4,GS4}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {ger_5,GS5}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {ger_6,GS6}),
    ets:insert(?ETS_CACHE_CONFIG_ID, {ger_7,GS7}),

    ok.

%% ====================================================================
%%  test functions
%% ====================================================================

show_ets()->
    {ets:tab2list(?ETS_HOME_LIST),ets:tab2list(?ETS_HOME_TASK_LIST)}.

show_task(RoleID,TaskID)->
    lists:keysearch(TaskID, #home_task.id, get_home_task(RoleID)).

clean_task_info(TaskOwner)->
    io:format("debug: clean task for ~w~n", [TaskOwner]),
    OldTaskL = get_home_task(TaskOwner),
    lists:foreach(fun(TaskInfo)-> 
            TaskId = TaskInfo#home_task.id,
            erlang:erase({?finish_task_timer,TaskId}),
            erlang:erase({?dounty_task_timer,TaskId}),
            db_sql:del_home_task(TaskId)
        end, OldTaskL),
    set_home_task(TaskOwner,[]),
    HomeInfo = home_server:get_home(TaskOwner),
    home_server:set_home(TaskOwner,HomeInfo#home_info{new_task_timer=[]}).

showt(RoleID)->
    io:format("-----------showt start-------------~n"),
    lists:foreach(fun(T)->
            {TaskName} = data_home:get({home_task,T#home_task.quality,T#home_task.task_type}),
            Rate = if
                       T#home_task.status =:= ?ht_normal_accepted orelse T#home_task.status =:= ?ht_dounty_accepted ->
                           role_home:cacl_win_rate(T);
                       true ->
                           0
                   end,
            io:format("~w (~w)~s q:~w s:~w(~w) enemy:~w need_time:~w ~w breward:~w~n", [T#home_task.id
                                       ,T#home_task.task_type,TaskName
                                       ,T#home_task.quality
                                       ,T#home_task.status,Rate
                                       ,T#home_task.tgt_ger_type
                                       ,T#home_task.finish_time / 3600
                                       ,util:seconds_to_datetime(T#home_task.timtout_ts)
                                       ,T#home_task.base_reward_num])
        end, home_server:get_home_task(RoleID)),
    io:format("-----------new task wait-----------~n"),
    Home = home_server:get_home(RoleID),
    lists:foreach(fun(T)->
            io:format("~w~n",[util:seconds_to_datetime(T)])
        end, Home#home_info.new_task_timer),
    io:format("-----------showt  end--------------~n").

filter_task_type(TaskList)->
    [T#home_task{base_reward_num = T#home_task.base_reward_num div ?home_exchange_rate}||T<-TaskList,T#home_task.status /= ?ht_dounty_unaccept].

fix_check_task1()->
    erlang:send(home_server, fix_check_task1).

%注意，这里是依照accept_role_id作为索引的
do_fix_check_task1()->
    List = db_sql:get_task_need_timer(),
    Now = util:now(),
    lists:foreach(fun([TId,AptRoleID,OwnerRoleID,Status,Ts])->
        if
            Status =:= ?ht_dounty_accepted orelse
              Status =:= ?ht_normal_accepted->
                OldAptTaskList = get_home_task(AptRoleID),
                DBAptTaskList = db_sql:get_home_task_info(AptRoleID),
                NewAptTaskList = lists:foldl(fun(ST,AccTaskList)-> 
                        case lists:keymember(ST#home_task.id, #home_task.id, AccTaskList) of
                            true ->
                                AccTaskList;
                            false ->
                                [ST|AccTaskList]
                        end
                    end,[],OldAptTaskList++DBAptTaskList),
                set_home_task(AptRoleID,NewAptTaskList),                        
                if
                    Now < Ts ->
                        ignore;
                    Now >= Ts ->
                        finish_task(AptRoleID,TId)
                end;
            true ->
                ignore
        end
    end, List).


