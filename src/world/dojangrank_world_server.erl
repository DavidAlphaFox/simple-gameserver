-module(dojangrank_world_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/0]).
-inline([get_ets_name/1]).

-define(master_server,dojangrank_world_master). 

-define(pd_slow_update_date1,pd_slow_update_date1).         %% 不保存数据库，该字典里的数据永远和ets里的保持一致，属于其子集
-define(pd_slow_update_date2,pd_slow_update_date2).         %% 不保存数据库，该字典里的数据永远和ets里的保持一致，属于其子集
-define(pd_slow_update_date3,pd_slow_update_date3).         %% 不保存数据库，该字典里的数据永远和ets里的保持一致，属于其子集
-define(pd_robot_temp,pd_robot_temp).           %% 记录机器人同步状态，该字典不保存数据库，该字典被定义时，说明正处于强制初始化中

-define(pd_history_rank,pd_history_rank).       %% 历史名次，保存数据库
-define(pd_top_fightrec,pd_top_fightrec).       %%

-define(slow_update_interval1,600).             %% TODO TEMP        default 600
-define(slow_update_interval2,7200).            %% TODO TEMP        default 7200
-define(slow_update_interval3,57600).           %% TODO TEMP        default 16小时

-define(eliminate_interval,240).                %% 2分钟 TODO TEMP    default 240
-define(state_check_max_interval,600).           %% TODO TEMP default 20分钟，必须小于init_step_1,init_step_2的间隔
-define(update_server_list_interval,30).        %% 激活列表的时间淘汰间隔,一定要和dojangrank_server中的同名宏保持一致
-define(notice_delay,1).                        %% 排行榜通知的最小时间间隔,0即关闭延迟发送

-define(collect_top10_finish,collect_top10_finish).
-define(wait_collect_top10,wait_collect_top10).       %% 标志而已没什么意义

-define(top_rec_rank,20).
-define(top_rec_num,20).

-record(state,{current_date = {0,0,0}                %% 记录奖金池积累时间
              ,dojangrank_world_session = 0         %% 暂时没用
              ,quick_need_update_data = []          %% 存放的是，快速同步中需要更新的数据
              ,update_server_list = {[],[]}         %% 每间隔update_server_list_interval，则淘汰掉末尾的成员，
              ,send_timestamp = 0                   %% 存放的是，快速同步中最后一次实际广播的时间
              ,data_version = 1}).                %% 上次发送update的时间

%% 保存于pd_slow_update_date1、pd_slow_update_date2、pd_slow_update_date3中
-record(slow_update_date, {point_timestamp_a = 1
                          ,point_timestamp_b = 1
                          ,point_version_a = 1
                          ,point_version_b = 1
                          ,slow_need_update_data_a = []
                          ,slow_need_update_data_b = []}).
                       
%% 切换时间在凌晨0:40开始

%% 双策略同步数据。部分游戏服务器采用快速同步，其他游戏服务器采用慢速同步
%% 游戏服务器如果发送了请求列表，或者战斗请求，则将服务器置于激活列表1分钟(update_server_list_interval)
%% 游戏服务器如果处于激活状态，即在激活列表中，采用快速同步，中央服务器主动推送排名变化，最频繁1秒一次，(notice_delay)
%% 快速同步，每次只同步，与之前数据相比变更的部分。
%% 非激活状态的服务器，需要主动请求列表更新。
%% 游戏服务器并不维护激活与否的状态

%% 慢速同步，维护两个变更列表（也可以是三个四个五个），5分钟列表，1小时列表。目的是减少需要同步的数据量
%% 5分钟列表，保存最近五分钟（至少包含最近五分钟的数据），如果游戏服之前请求数据的的时间戳在5分钟之内，就只发送5分钟列表内的数据。
%% 1小时类似

%% check 检查drw_rank_data.rank 和 dr_info中的p_dr_world_rank.rank 是否都保持一致了
%% 杜绝使用ets:tab2list、lists:foreach、lists:map、[E||E<-List]
%% 检查赛季关闭期间所有发往跨服的消息是否正确处理

%% TODO
%% 1、全同步排名，改为异步
%% 2、战斗要考虑崩溃
%% 

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
    {ok,_}=
        supervisor:start_child(world_sup,
                               {?MODULE,
                                {?MODULE,start_link,[]},
                                permanent,600000,worker,[?MODULE]}).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% FighterData = {FighterListA,RoleLieuAddA,TalentA,TrSpecialA,SkinInfoA,LegendAddListA}
dojangrank_world_fight(RoleID,ServerId,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,FightPower,NewPDR,NewAtkFighterData)->
    ?INFO("dojangrank_world_fight   ------Send-----      ~w",[{RoleID,ServerId,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,FightPower,NewPDR,NewAtkFighterData}]),
    send_msg:direct_by_name(?master_server, ?MODULE, {dojangrank_world_fight,RoleID,ServerId,RoleName,RankType,PayType,EnemyRoleId,EnemyRank,FightPower,NewPDR,NewAtkFighterData}).

req_rank_slow({RemotePointTimestamp,RemotePointVersion},ServerId,FromPid)->
    ?INFO("req_rank_slow  from ~w.   verison:~w",[ServerId,{RemotePointTimestamp,RemotePointVersion}]),
    send_msg:direct_by_name(?master_server, ?MODULE, {req_rank_slow,{RemotePointTimestamp,RemotePointVersion},ServerId,FromPid}).

set_world_select_ger_type(ServerId,RoleID,RankType,SelectedGerType)->
    send_msg:direct_by_name(?master_server, ?MODULE, {set_world_select_ger_type,ServerId,RoleID,RankType,SelectedGerType}).

get_dojangrank_world_top_replay_list(FromPid)->
    send_msg:direct_by_name(?master_server, ?MODULE, {get_dojangrank_world_top_replay_list,FromPid}).

get_dojangrank_world_history_list(FromPid)->
    send_msg:direct_by_name(?master_server, ?MODULE, {get_dojangrank_world_history_list,FromPid}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
    random:seed(util:gen_random_seed()),
    process_flag(trap_exit,true),
    State = init_process_data(),
    set_persist_interval(State),
    erlang:send_after(?eliminate_interval*1000,?MODULE,eliminate_tick),
    set_update_server_list_interval(),
    {ok,State}.

terminate(Reason,State) ->
    ?ERR("~w terminate,reason:~w",[?MODULE,Reason]),
    do_persist(State),
    ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.

handle_call({debug_get_pd,PdName},_From,State) ->
    ?ERR("debug_get_pd ~w:~w",[PdName,get(PdName)]),
    {reply,get(PdName),State};
handle_call({debug_put_pd,PdName,Value},_From,State) ->
    OldValue = get(PdName),
    ?ERR("debug_get_pd ~w:~w",[PdName,put(PdName,Value)]),
    {reply,{OldValue,get(PdName)},State};

handle_call(already_server_num,_From,State) ->
    case get(?pd_robot_temp) of
        {DebugFlag,AlreadyServerList,OldRobotList,_}->
            Res = case erlang:is_list(OldRobotList) of
                      true ->
                          erlang:length(OldRobotList);
                      false ->
                          OldRobotList
                  end,
            {reply,{DebugFlag,erlang:length(AlreadyServerList),Res},State};
        _ ->
            {reply,fail,State}
    end;

handle_call(Request,_From,State) ->
    ?ERR("未知的Request:~w,From:~w",[Request,_From]),
    {reply,ok,State}.

handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info({force_notice_all_server,AllServer},State) ->
    Len = erlang:length(AllServer),
    {RecvServerList,OtherList} = 
        if
            Len > 10 ->
                lists:split(10,AllServer);
            true ->
                {AllServer,[]}
        end,
    lists:foreach(fun(Node)-> 
                          erlang:send({dojangrank_server,Node}, refresh_world_rank_start) 
                  end, RecvServerList),
    ?INFO("force_notice_all_server notice left:~w",[Len-10]),
    case OtherList of
        [] -> ignore;
        _ -> erlang:send_after(2000, self(), {force_notice_all_server,OtherList})
    end,
    {noreply,State};
handle_info(debug_force_init_session,State) ->
    All = ets:tab2list(?ETS_NODE_INFO_TABLE2),
    GameServerList = [E||{N,E}<-All,is_game_server(N)],
    ?INFO("debug_force_init_session ~w",[GameServerList]),
    lists:foreach(fun({Node,_})-> 
                          erlang:send({dojangrank_server,Node}, {debug_force_init_session,self()}) 
                  end, All),
    erlang:garbage_collect(),
    {noreply,State#state{dojangrank_world_session = get_now_session()
                        ,data_version = State#state.data_version + 1}};

%% 启动赛季切换，设定好Filter和切换时间，先进行本赛季数据保护
handle_info({swap_session_start,DebugFlag},State) ->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    %% ets:fun2ms(fun(R) when R#drw_rank_data.rank =< InitRobotNum -> element(5,R) end).
    Filter = [{#drw_rank_data{rank = '_',role_id = '_',server_id = '_',
                 dr_info = '_',fighter_data = '_'},
                  [{'and',{'orelse',true,fail},{'=<',{element,2,'$_'},InitRobotNum}}],
                  [{element,5,'$_'}]}],
    Now = util:now(),
    erlang:send(self(), {swap_session_step1,lists:seq(1, 8),Now,Filter,DebugFlag}),
    {noreply,State};

%% 第一步，本赛季数据保存至文本文件
handle_info({swap_session_step1,[],Now,Filter,DebugFlag},State) ->
    erlang:send(self(), {swap_session_step2,Now,Filter,DebugFlag}),
    {noreply,State};
handle_info({swap_session_step1,[RankType|OtherRankTypes],Now,Filter,DebugFlag},State) ->
    swap_session_step1(RankType,Now,Filter),
    erlang:send(self()
               ,{swap_session_step1,OtherRankTypes,Now,Filter,DebugFlag}),
    {noreply,State};

%% 第二部，保存历史数据，切换历史数据
handle_info({swap_session_step2,Now,Filter,DebugFlag},State) ->
    TempGroup = lists:map(fun(RankType)-> 
                            ets:select(get_ets_name(RankType), [{'$1',[],[{element,5,'$1'}]}])
                    end, lists:seq(1, 8)),
    %% 设置初始化数据
    put(?pd_robot_temp,{DebugFlag,[],[],TempGroup}),
    erlang:garbage_collect(),
    
    put(?pd_slow_update_date1,#slow_update_date{point_version_b = State#state.data_version, point_timestamp_b=Now}),
    put(?pd_slow_update_date2,#slow_update_date{point_version_b = State#state.data_version, point_timestamp_b=Now}),
    put(?pd_slow_update_date3,#slow_update_date{point_version_b = State#state.data_version, point_timestamp_b=Now}),
    %% 清除巅峰录像榜单
    db_sql:clear_dojangrank_world_fightrec_group(),
    put(?pd_top_fightrec,{0,lists:duplicate(8, [])}),

    erlang:send(self(),{swap_session_step3,lists:seq(1, 8),Now,Filter,DebugFlag}),
    ?INFO("swap_session_step2",[]),
    {noreply,State};

%% 第三部，保存历史数据，切换历史数据
handle_info({swap_session_step3,[],Now,Filter,DebugFlag},State) ->
    erlang:send(self(), {swap_session_step4,Now,Filter,DebugFlag}),
    {noreply,State};
handle_info({swap_session_step3,[RankType|OtherRankTypes],Now,Filter,DebugFlag},State) ->
    swap_session_step3(RankType,Now,Filter),
    erlang:send(self(),{swap_session_step3,OtherRankTypes,Now,Filter,DebugFlag}),
    {noreply,State};

%% 第四部，清理录像数据，慢同步数据
handle_info({swap_session_step4,_Now,_Filter,DebugFlag},State) -> 
    do_persist(State),
    ?INFO("swap_session_step4 date swap finish",[]),
    if
        999 =:= DebugFlag ->
            erlang:send(self(), debug_force_init_session);
        true -> ignore
    end,    
    {noreply,State};

handle_info({force_slow_update,ServerID},State) ->
    case get(?pd_slow_update_date3) of
        SlowDate when erlang:is_record(SlowDate, slow_update_date) ->
            UpdateGroup = SlowDate#slow_update_date.slow_need_update_data_b,
            send_msg:direct(ServerID,dojangrank_server,{fix_world_rank,UpdateGroup}),
            ?ERR("force_slow_update success ~w",[ServerID]);
        true ->
            ?ERR("force_slow_update fail ~w",[ServerID])
    end,
    {noreply,State};

handle_info({set_slow_update_time,PdName,NewTime,NewVersion},State) ->
    case get(PdName) of 
        OldData when erlang:is_record(OldData, slow_update_date)->
            put(PdName,OldData#slow_update_date{point_timestamp_b = NewTime
                                               ,point_version_b = NewVersion}),
            ?ERR("set_slow_update_time->  ~w",[{PdName
                                              ,OldData#slow_update_date.point_timestamp_b
                                              ,NewTime
                                              ,OldData#slow_update_date.point_version_b
                                              ,NewVersion}]);
        _ -> ignore
    end,
    {noreply,State};

%% 最近请求过
%% 字典中未记录
%% 字典中记录了，但是太久了
handle_info({req_rank_slow,{RemotePointTimestamp,RemotePointVersion},ServerId,FromPid},State) ->
    ?INFO("--------req_rank_slow---------:~w",[{{RemotePointTimestamp,RemotePointVersion},ServerId,FromPid}]),
    case get_need_update_date({RemotePointTimestamp,RemotePointVersion}
                             ,[get(?pd_slow_update_date1),get(?pd_slow_update_date2),get(?pd_slow_update_date3)]) of
        {true,SlowDate} ->  %% 可以用慢更新
            UpdateGroup = SlowDate#slow_update_date.slow_need_update_data_b,
            TopRecVersion = get_newest_version(),
            erlang:send(FromPid,{notice_update_rank
                                ,{util:now(),State#state.data_version},false
                                ,UpdateGroup,TopRecVersion}),
            %% 将刚刚战斗的服务器也加入激活列表
            NewState = add_server_to_list(ServerId,State);
        false ->    %% 需要全更新
            case process_info(whereis(?MODULE),message_queue_len) of
                {message_queue_len,MsgLen} when MsgLen  =< 100 ->
                    notice_all_rank(FromPid,ServerId,State#state.data_version,{RemotePointTimestamp,RemotePointVersion}),
                    %% 将刚刚战斗的服务器也加入激活列表
                    NewState = add_server_to_list(ServerId,State);
                _ ->
                    NewState = State
            end
    end,
    {noreply,NewState};

handle_info({dojangrank_world_fight,RoleID,ServerId,_RoleName,RankType,PayType,EnemyRoleId,EnemyRank,_FightPower,NewPDR,NewAtkFighterData},State) ->
    EtsName = get_ets_name(RankType),
    Reward = ?undefined,
    case check_dojangrank_world_fight(EtsName,RoleID,EnemyRoleId,EnemyRank) of
        {false,Result} ->
            IsTopRec = false,
            TopFightRecUid = 0,
            ReplayInfo0 = ?undefined,
            FightInfo = [],
            NewAtkRankData = ?undefined,
            NewDefRankData = ?undefined,
            DefRankInfo = ?undefined,
            
            %% 挑战者所在服务器的敌人信息可能有误，此处返回当前正确的敌人信息
            FixDefRankData = case ets:lookup(EtsName, EnemyRank) of
                              [] ->
                                  ets:match_object(EtsName, #drw_rank_data{role_id=EnemyRoleId, _='_'});
                              [Info2] ->
                                  if
                                      Info2#drw_rank_data.role_id /= EnemyRoleId ->
                                          [Info2|ets:match_object(EtsName, #drw_rank_data{role_id=EnemyRoleId, _='_'})];
                                      true ->
                                          [Info2]
                                  end
                          end,
            FixDefRankInfo = lists:map(fun(E)-> E#drw_rank_data.dr_info end, FixDefRankData),
            
            UpdateListAll = [#p_dr_world_rank{roleID = RoleID
                                             ,serverID = ServerId}|FixDefRankInfo];
        {true,AtkRankData,DefRankData} ->
            AtkRankInfo0 = AtkRankData#drw_rank_data.dr_info ,
            AtkRankInfo = AtkRankInfo0#p_dr_world_rank{fightPower = NewPDR#p_dr_world_rank.fightPower
                                                      ,isMale = NewPDR#p_dr_world_rank.isMale
                                                      ,title = NewPDR#p_dr_world_rank.title
                                                      ,head = NewPDR#p_dr_world_rank.head
                                                      ,level = NewPDR#p_dr_world_rank.level
                                                      ,roleName = NewPDR#p_dr_world_rank.roleName
                                                      ,vip = NewPDR#p_dr_world_rank.vip
                                                      ,skinID = NewPDR#p_dr_world_rank.skinID
                                                      ,ger_info_list = NewPDR#p_dr_world_rank.ger_info_list},
            ?INFO("dojangrank_world_fight ready ~w   >>>>  ~w",[AtkRankInfo0,AtkRankInfo]),
            DefRankInfo = DefRankData#drw_rank_data.dr_info,
            FightRecord = 
                do_dojangrank_fight(NewAtkFighterData,DefRankData#drw_rank_data.fighter_data),
%%             FightRecord = FightRecord0#sc_fight_request{result = true},     %% TODO TEMP
            case FightRecord#sc_fight_request.result of
                true when AtkRankInfo#p_dr_world_rank.rank > DefRankInfo#p_dr_world_rank.rank ->
                    NewAtkRankInfo = AtkRankInfo#p_dr_world_rank{rank = DefRankData#drw_rank_data.rank
                                                                ,win_num = AtkRankInfo#p_dr_world_rank.win_num + 1
                                                                ,total_num = AtkRankInfo#p_dr_world_rank.total_num + 1},
                    NewDefRankInfo = DefRankInfo#p_dr_world_rank{rank = AtkRankData#drw_rank_data.rank
                                                                ,lost_num = DefRankInfo#p_dr_world_rank.lost_num + 1
                                                                ,total_num = DefRankInfo#p_dr_world_rank.total_num + 1},
                    
                    NewAtkRankData = AtkRankData#drw_rank_data{rank = NewAtkRankInfo#p_dr_world_rank.rank
                                                              ,dr_info = NewAtkRankInfo
                                                              ,fighter_data = NewAtkFighterData},
                    NewDefRankData = DefRankData#drw_rank_data{rank = NewDefRankInfo#p_dr_world_rank.rank
                                                              ,dr_info = NewDefRankInfo},
                    
                    IsWinNum = 1;
                true when AtkRankInfo#p_dr_world_rank.rank =< DefRankInfo#p_dr_world_rank.rank->
                    NewAtkRankInfo = AtkRankInfo#p_dr_world_rank{win_num = AtkRankInfo#p_dr_world_rank.win_num + 1
                                                                ,total_num = AtkRankInfo#p_dr_world_rank.total_num + 1},
                    NewDefRankInfo = DefRankInfo#p_dr_world_rank{lost_num = DefRankInfo#p_dr_world_rank.lost_num + 1
                                                                ,total_num = DefRankInfo#p_dr_world_rank.total_num + 1},
                    
                    NewAtkRankData = AtkRankData#drw_rank_data{dr_info = NewAtkRankInfo
                                                              ,fighter_data = NewAtkFighterData},
                    NewDefRankData = DefRankData#drw_rank_data{dr_info = NewDefRankInfo},
                    
                    IsWinNum = 2;
                false ->
                    NewAtkRankInfo = AtkRankInfo#p_dr_world_rank{lost_num = AtkRankInfo#p_dr_world_rank.lost_num + 1
                                                                ,total_num = AtkRankInfo#p_dr_world_rank.total_num + 1},
                    NewDefRankInfo = DefRankInfo#p_dr_world_rank{win_num = DefRankInfo#p_dr_world_rank.win_num + 1
                                                                ,total_num = DefRankInfo#p_dr_world_rank.total_num + 1},
                    
                    NewAtkRankData = AtkRankData#drw_rank_data{dr_info = NewAtkRankInfo
                                                              ,fighter_data = NewAtkFighterData},
                    NewDefRankData = DefRankData#drw_rank_data{dr_info = NewDefRankInfo},
                    
                    IsWinNum = 0
            end,
            UpdateListAll = [NewAtkRankInfo,NewDefRankInfo],
            ReplayInfo0 = 
                #p_dr_dojang_replay_info{attacker_info = role_dojangrank:check_rank_out(NewAtkRankData#drw_rank_data.dr_info)
                                        ,defender_info = role_dojangrank:check_rank_out(NewDefRankData#drw_rank_data.dr_info)
                                        ,replayUID = 0      %% 待游戏服保存后再填入
                                        ,time = util:now()
                                        ,rank_type = RankType
                                        ,is_win = IsWinNum},
            FightInfo = [FightRecord],
            IsTopRec = AtkRankData#drw_rank_data.rank =< ?top_rec_rank orelse DefRankData#drw_rank_data.rank =< ?top_rec_rank,
            TopFightRecUid = 
                if
                    IsTopRec -> save_top_rec(RankType,ReplayInfo0,FightRecord);
                    true -> 0
                end,
            Result = 1
    end,
    case catch send_msg:direct(ServerId,dojangrank_server
                              ,{dojangrank_world_fight_res,Result,FightInfo,ReplayInfo0,Reward,PayType,RankType,UpdateListAll,{IsTopRec,TopFightRecUid}}) of
        {'EXIT',Error1}-> ?ERR("dojangrank_fight error ~w",[Error1]);
        _ -> ignore
    end,
    if
        DefRankInfo /= ?undefined 
        andalso DefRankInfo#p_dr_world_rank.serverID /= ServerId ->
            case catch send_msg:direct(DefRankInfo#p_dr_world_rank.serverID,dojangrank_server
                                      ,{dojangrank_world_fight_res,Result,FightInfo,ReplayInfo0,Reward,PayType,RankType,UpdateListAll,{IsTopRec,TopFightRecUid}}) of
                {'EXIT',Error2}-> ?ERR("dojangrank_fight error ~w",[Error2]);
                _ -> ignore
            end;
        true -> ignore
    end,
    StateFinal = 
        if
            %% 如果修改此处条件，千万注意，战斗失败的情况，不要把UpdateListAll写入ets数据
            FightInfo /= [] ->  
                %% 保存战斗排序变化
                ets:insert(EtsName,[NewAtkRankData,NewDefRankData]),
                %% 升级数据版本号,同时也刷新慢更新的更新列表
                State2= version_add(State,{RankType,UpdateListAll}),  %%  :: p_dr_world_rank
                %% 尝试通知激活列表中的服务器，此处可能立即通知，也可能延迟通知
                State3 = quick_notice_rank(State2,{RankType,UpdateListAll}),
                State3;
            true ->
                State
        end,
    {noreply,StateFinal};

handle_info(delay_notice_rank,State) ->
    NewState = delay_notice_rank(State),
    {noreply,NewState};

handle_info({get_dojangrank_world_history_list,FromPid},State) ->
    ?INFO("----------------get_dojangrank_world_history_list--------------"),
    HistoryGroup = 
        lists:map(fun(RankType)->
                    get({?pd_history_rank,RankType})
            end, lists:seq(1, 8)),
    erlang:send(FromPid, {get_dojangrank_world_history_list_req,State#state.dojangrank_world_session
                         ,HistoryGroup}),
    {noreply,State};

handle_info({get_dojangrank_world_top_replay_list,FromPid},State) ->
    {Version,FightRecGroup} = get(?pd_top_fightrec),
    erlang:send(FromPid, {get_dojangrank_world_top_replay_list_res,Version,FightRecGroup}),
    {noreply,State};

handle_info({set_world_select_ger_type,ServerId,RoleID,RankType,SelectedGerType},State) ->
    EtsName = get_ets_name(RankType),
    case ets:match_object(EtsName, #drw_rank_data{role_id=RoleID, _='_'}) of
        [] ->
            catch send_msg:direct(ServerId,dojangrank_server,{set_world_select_ger_type_res,2,RankType,SelectedGerType,RoleID});
        [Drw] ->
            Old = Drw#drw_rank_data.dr_info,
            NewRankInfo = Old#p_dr_world_rank{selected_ger_type = SelectedGerType},
            ets:insert(EtsName, Drw#drw_rank_data{dr_info = NewRankInfo}),
            catch send_msg:direct(ServerId,dojangrank_server,{set_world_select_ger_type_res,1,RankType,SelectedGerType,RoleID}),
            %% 升级数据版本号,同时也刷新慢更新的更新列表
            State2= version_add(State,{RankType,[NewRankInfo]}),
            %% 尝试通知激活列表中的服务器，此处可能立即通知，也可能延迟通知
            State3 = quick_notice_rank(State2,{RankType,[NewRankInfo]}),
            State3
    end,
    {noreply,State};

handle_info({update_top10,RoleListGroup},State) ->
    update_top10(RoleListGroup),
    {noreply,State};
%% 游戏服更新参赛者数据
%% 除了时间，pd_robot_temp标记，表示正在处于赛季交替,pd_robot_temp有值的时间范围，小于交替的实际时间
handle_info({update_top10,ServerID,RobotInfoList,RoleListGroup,FromPid},State) ->
    OpenState = role_dojangrank:get_open_state(),
    case get(?pd_robot_temp) of
        {DebugFlag,AlreadyServerList,OldRobotList,LastGroup} when DebugFlag =:= 999 orelse OpenState /= 2  ->
            ?ERR("update_top10 -1- ServerID:~w RobotInfoListLen:~w OpenState:~w RoleListGroup:~w  --DebugFlag--~w"
                 ,[ServerID,length(RobotInfoList),OpenState
                  ,[length(element(2, lists:nth(E,RoleListGroup)))||E<-lists:seq(1, 8)]
                  ,DebugFlag]),
            case lists:member(ServerID, AlreadyServerList) of
                false when OldRobotList =:= ?wait_collect_top10 orelse erlang:is_list(OldRobotList) ->
                    update_top10(RoleListGroup),
                    NewAlreadyServerList = [ServerID|AlreadyServerList],
                    case erlang:is_list(OldRobotList) of
                        true ->
                            NeedRobot = data_dojangrank:get(init_robot_num_max),
                            NewRobotList0 = RobotInfoList ++ OldRobotList,
                            RobotLen = erlang:length(NewRobotList0),
                            ?INFO("RobotLen:~w",[RobotLen]),
                            if
                                NeedRobot =< RobotLen ->
                                    spawn(fun()-> 
                                            init_robot(lists:sublist(NewRobotList0,RobotLen)),
                                            lists:foreach(fun(RankType)-> 
                                                            EtsName = get_ets_name(RankType),
                                                            ?ERR("RobotLen init finish ~w size:~w memory:~w",[EtsName,ets:info(EtsName,size),ets:info(EtsName,memory)])
                                                      end, lists:seq(1, 8)),
                                            erlang:send(?MODULE, force_init_robot_finish)
                                        end),
                                    NewRobotList = ?wait_collect_top10;
                                true -> 
                                    NewRobotList = NewRobotList0
                            end;
                        _ ->
                            NewRobotList = OldRobotList
                    end,
                    put(?pd_robot_temp,{DebugFlag,NewAlreadyServerList,NewRobotList,LastGroup}),
                    DateTime = {date(),time()},
                    StartTime = data_dojangrank:get(start_date),
                    if
                        DateTime < StartTime ->
                            ?ERR("TopHero -init- :(~w)",[ServerID]),
                            erlang:send(FromPid, {world_init_req,State#state.dojangrank_world_session,[]});
                        1 =:= OpenState orelse DebugFlag =:= 999 ->
                            TopHero = get_top_hero(ServerID,LastGroup),
                            ?ERR("TopHero -normal- :(~w)~w",[ServerID,[erlang:length(E)||{_,E}<-TopHero]]),
                            erlang:send(FromPid, {world_init_req,State#state.dojangrank_world_session,TopHero});
                        true ->
                            ?ERR("update_top10 奇怪的实际和场合收到了这个消息 ~w",[OpenState])
                    end,
                    ?INFO("update_top10-2- ServerID:~w(other:~w) RoleListGroup:~w"
                         ,[ServerID,NewAlreadyServerList
                          ,[length(element(2, lists:nth(E,RoleListGroup)))||E<-lists:seq(1, 8)]]);
                ERR ->
                    ?ERR("update_top10 -1- ServerID:~w  ----  ServerList:~w   ----  ERR:~w",[ServerID,AlreadyServerList,ERR])
            end;
        ?undefined ->
            ?ERR("update_top10 -2- ServerID:~w CenterOpenState:~w",[ServerID,OpenState])    
    end,
    {noreply,State};

%% 只有调试模式才需要这样处理
handle_info(force_init_robot_finish,State) ->
    case get(?pd_robot_temp) of
        {_DebugFlag,AlreadyServerList,?wait_collect_top10,_LastGroup} ->
            ?ERR("force_init_robot_finish sucess ~w",[erlang:length(AlreadyServerList)]);
        _ -> ?ERR("force_init_robot_finish Fail ",[])
    end,
    {noreply,State};

handle_info(persist_dojangrank_world_rank_start,State) ->
    case get(?pd_robot_temp) of
        {DebugFlag,AlreadyServerList,?wait_collect_top10,LastGroup} ->
            erlang:send(?MODULE,{persist_dojangrank_world_rank,?undefined,lists:seq(1, 8)}),
            put(?pd_robot_temp,{DebugFlag,AlreadyServerList,?collect_top10_finish,LastGroup});
        _ -> ?ERR("persist_dojangrank_world_rank_start Fail ",[])
    end,
    {noreply,State};

handle_info({persist_dojangrank_world_rank,?undefined,[]},State) ->
    erlang:send_after(100, ?MODULE, persist_dojangrank_world_rank_finish),
    {noreply,State};
handle_info({persist_dojangrank_world_rank,Key,[RankType|OtherList]},State) ->
    EtsName = get_ets_name(RankType),
    Res = 
        case Key of
            ?undefined ->
                ets:match(EtsName,'$1',200);
            _ ->
                ets:match(Key)
        end,
    case Res of
        '$end_of_table' ->
            ?INFO("persist_dojangrank_world_rank -1-~w",[RankType]),
            erlang:send_after(100, ?MODULE, {persist_dojangrank_world_rank,?undefined,OtherList});
        {ElementList,'$end_of_table'} ->
            ?INFO("persist_dojangrank_world_rank -2-~w",[RankType]),
            db_sql:persist_dojangrank_world_rank_single(RankType,ElementList),
            erlang:send_after(100, ?MODULE, {persist_dojangrank_world_rank,?undefined,OtherList});
        {ElementList,Key2} ->
            db_sql:persist_dojangrank_world_rank_single(RankType,ElementList),
            erlang:send_after(100, ?MODULE,{persist_dojangrank_world_rank,Key2,[RankType|OtherList]})
    end,    
    {noreply,State};
handle_info(persist_dojangrank_world_rank_finish,State) ->
    case get(?pd_robot_temp) of
        {_DebugFlag,AlreadyServerList,?collect_top10_finish,_} ->
            NodeList = 
                lists:foldl(fun(ServerID,AccList)-> 
                        case ets:lookup(?ETS_NODE_INFO_TABLE, ServerID) of
                            [{_,NodeName}] ->
                                [NodeName|AccList];
                            _ ->
                                AccList
                        end
                    end, [], AlreadyServerList),
            ?INFO("----------------swap_session_end---AlreadyServerList:~w-----",[AlreadyServerList]),
            erlang:send(?MODULE,{force_notice_all_server,NodeList}),
            erlang:erase(?pd_robot_temp);
        _ ->
            ?ERR("persist_dojangrank_world_rank_finish error. 不该走到这个分支")
    end,
    ?ERR("----------------swap_session_end-------------------"),
    {noreply,State};

%% 更新慢同步列表
handle_info(eliminate_tick,State) ->
    eliminate_slow_data(State),
    erlang:send_after(?eliminate_interval*1000,self(),eliminate_tick),
    {noreply,State};
%% 相当于删除超过时间阈值的
handle_info(update_server_list_tick,State) ->
    set_update_server_list_interval(),
    {A,_B} = State#state.update_server_list,
    {noreply,State#state{update_server_list = {[],A}}};
handle_info(state_check_tick,State) ->
    set_persist_interval(State),
    StateNew = check_state(State),
    {noreply,StateNew};
%% debug function
handle_info(debug_show_quick_list_len,State) ->
    {A,B} = State#state.update_server_list,
    ?ERR("debug_show_quick_list_len ~w - ~w",[erlang:length(A)
                                             ,erlang:length(B)]),
    {noreply,State};
handle_info({test_status,FromPid},State) ->
    SU1 = get(?pd_slow_update_date1),
    SU2 = get(?pd_slow_update_date2),
    SU3 = get(?pd_slow_update_date3),
    StatusString = io_lib:format("start time:~w~nversion:~w~nrank_data:~w~ncurrent_session_state:~w~nquick_server:~w~nquick_need_update_data:~w~nslow_update_dict1:~w~n2:~w~n3:~w~n"
                                , [data_dojangrank:get(start_date)
                                  ,State#state.data_version 
                                  ,dojangrank_world_server:debug_show_rank_num()
                                  ,role_dojangrank:get_open_state()
                                  ,State#state.update_server_list
                                  ,[{T,erlang:length(R)}||{T,R}<-State#state.quick_need_update_data]
                                  ,{SU1#slow_update_date.point_timestamp_a,SU1#slow_update_date.point_version_a
                                   ,SU1#slow_update_date.point_timestamp_b,SU1#slow_update_date.point_version_b
                                   ,length(SU1#slow_update_date.slow_need_update_data_a),length(SU1#slow_update_date.slow_need_update_data_b)}
                                  ,{SU2#slow_update_date.point_timestamp_a,SU2#slow_update_date.point_version_a
                                   ,SU2#slow_update_date.point_timestamp_b,SU2#slow_update_date.point_version_b
                                   ,length(SU2#slow_update_date.slow_need_update_data_a),length(SU2#slow_update_date.slow_need_update_data_b)}
                                  ,{SU3#slow_update_date.point_timestamp_a,SU3#slow_update_date.point_version_a
                                   ,SU3#slow_update_date.point_timestamp_b,SU3#slow_update_date.point_version_b
                                   ,length(SU3#slow_update_date.slow_need_update_data_a),length(SU3#slow_update_date.slow_need_update_data_b)}]),
    ?ERR("----test_status--~w--~s",[FromPid,StatusString]),io:format("~s",[StatusString]),
    erlang:send(FromPid, {test_status_res,StatusString}),
    {noreply,State};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info(test_crash,State) ->
    A = 1,B = 2,
    A = B,
    {noreply,State};
handle_info(Info,State) ->
    ?ERR("未知的Info:~w",[Info]),
    {noreply,State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_now_session()->
    {YY,MM,_} = erlang:date(),
    YY*100 + MM.

init_robot(RobotList)->
    AllLen = erlang:length(RobotList),
    ?ERR("init_robot start AllLen:~w",[AllLen]),
    RobotConfig = data_dojangrank:get(world_robot_init),
    AllGer = [data_ger:get(ID) || ID<-data_ger:get_list(), ID < 20000],
    lists:foreach(fun(RankType)->  
            RobotListAcc = util:random_list(RobotList),
            RankTypeGerList = lists:filter(fun(GerCfg)-> 
                                                   GerCfg#data_ger.gerProperty =:= RankType 
                                           end, AllGer),
            init_robot2(RobotListAcc,RankType,RobotConfig,RankTypeGerList)
        end, lists:seq(1, 8)).

%% 机器人数据用完了
init_robot2([],_,_,_)->
    [];
%% 配置中需要创建的机器人创建完了
init_robot2(OtherRobotList,_,[],_)->
    OtherRobotList;
init_robot2(RobotList,RankType,[{{Rank1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality}}|Other],AllGer)->
    ItemDict = {[],[],[],[],[],[]},    %% no used
    EquipLevel = 1,                    %% no used
    NewRobotList = init_robot3(RobotList,RankType,{Rank1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality},AllGer,EquipLevel,ItemDict),
    init_robot2(NewRobotList,RankType,Other,AllGer).

init_robot3([],_,_,_,_,_,_)->
    [];
init_robot3(RobotList,_,{Rank1,Rank2},_,_,_,_) when Rank1 - 1 >= Rank2->
    RobotList;
init_robot3([H|T],RankType,{Rank1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality},AllGer,EquipLevel,ItemDict)->
%%     ?INFO("init_robot3 ~w  >>> ~w (~w)",[{Rank1,Rank2},H,erlang:length(T)]),
    RoleID = H#p_dr_world_rank.roleID,
    GerDict = [E||#data_ger{gerStar=GerStar, gerTypeID=GerTypeID}=E<-AllGer, lists:member(GerStar, GerStarList), GerTypeID > 1000, GerTypeID < 10000, GerTypeID rem 10=:=0],
%%     ?INFO("init_robot3(~w)",[{[{E#data_ger.gerTypeID,E#data_ger.gerProperty,E#data_ger.gerStar}||E<-GerDict],GerNum,GerLevel,EquipLevel,ItemDict,GerQuality}]),
    FighterList = gen_account:random_ger_list(GerDict,min(erlang:length(GerDict),GerNum),GerLevel,EquipLevel,ItemDict,GerQuality),
    FightPower = lists:sum([E#ger.gerAttr#gerAttr.gerFightPower||E<-FighterList]),
    SelectedGerType = 
        lists:nth(1,[E#ger.gerBase#gerBase.gerTypeID*256 + E#ger.gerBase#gerBase.gerQuality||E<-FighterList]),
    PDrWorldRank = H#p_dr_world_rank{fightPower = FightPower
                                    ,rank = Rank1
                                    ,selected_ger_type = SelectedGerType
                                    ,ger_info_list = dojangrank_server:to_dr_ger_info(FighterList)
                                    ,rank_score = 10110 - (Rank1*10)},
    TrID = if H#p_dr_world_rank.isMale =:= false -> 1; true -> 2 end,
    %%{FighterListA,RoleLieuAddA,TalentA,TrSpecialA,SkinInfoA,LegendAddListA}
    FighterData = {FighterList,{0,0},[],#trSpecial{trID = TrID,specialID = 0,roleLevel = H#p_dr_world_rank.level,sp = 0,state = 0}
                  ,#skin_info{has = [],equip = 0},[]},
    ets:insert(get_ets_name(RankType), #drw_rank_data{rank = Rank1
                                           ,role_id = RoleID
                                           ,server_id = H#p_dr_world_rank.serverID
                                           ,dr_info = PDrWorldRank          %% #p_dr_world_rank{}
                                           ,fighter_data = FighterData}),
    init_robot3(T,RankType,{Rank1+1,Rank2},{GerNum,GerStarList,GerLevel,GerQuality},AllGer,EquipLevel,ItemDict).

add_server_to_list(ServerID,State)->
    {A,B} = State#state.update_server_list,
    case lists:member(ServerID, A) of
        false -> State#state{update_server_list = {[ServerID|A],B}};
        true -> State
    end.

%% 升级数据版本号,同时也刷新慢更新的更新列表
version_add(State,{RankType,UpdateListAll})->
    NewVersion = State#state.data_version + 1,
    version_add2(?pd_slow_update_date1,{RankType,UpdateListAll},NewVersion),
    version_add2(?pd_slow_update_date2,{RankType,UpdateListAll},NewVersion),
    version_add2(?pd_slow_update_date3,{RankType,UpdateListAll},NewVersion),
    State#state{data_version = NewVersion}.
%% 数据加入慢更新列表，如果当前列表为空，则额外还要记录好当前的时间和版本
version_add2(PdName,{RankType,UpdateListAll},NewVersion)->
    SU = get(PdName),
    NewSU1 = 
        if
            [] /= SU#slow_update_date.slow_need_update_data_a ->
                NewA = merge_update_list({RankType,UpdateListAll},SU#slow_update_date.slow_need_update_data_a),
                SU#slow_update_date{slow_need_update_data_a = NewA};
            [] =:= SU#slow_update_date.slow_need_update_data_a ->
                SU#slow_update_date{point_timestamp_a = util:now()
                                   ,point_version_a = NewVersion
                                   ,slow_need_update_data_a = [{RankType,UpdateListAll}]}
        end,
    NewSU2 = 
        if
            [] /= NewSU1#slow_update_date.slow_need_update_data_b ->
                NewA2 = merge_update_list({RankType,UpdateListAll},NewSU1#slow_update_date.slow_need_update_data_b),
                NewSU1#slow_update_date{slow_need_update_data_b = NewA2};
            [] =:= NewSU1#slow_update_date.slow_need_update_data_b ->
                NewSU1#slow_update_date{point_timestamp_b = util:now()
                                       ,point_version_b = NewVersion
                                       ,slow_need_update_data_b = [{RankType,UpdateListAll}]}
        end,
    put(PdName,NewSU2).

eliminate_slow_data(State)->
    case get(?pd_robot_temp) of
        ?undefined ->
            Now = util:now(),
            SU3 = get(?pd_slow_update_date3),
            if
                SU3#slow_update_date.point_timestamp_a + ?slow_update_interval3 =< Now ->
                    put(?pd_slow_update_date3
                       ,#slow_update_date{point_timestamp_a = Now
                                         ,point_timestamp_b = SU3#slow_update_date.point_timestamp_a
                                         ,point_version_a = State#state.data_version
                                         ,point_version_b = SU3#slow_update_date.point_version_a
                                         ,slow_need_update_data_a = []
                                         ,slow_need_update_data_b = SU3#slow_update_date.slow_need_update_data_a});
                true -> ignore
            end,
            SU2 = get(?pd_slow_update_date2),
            if
                SU2#slow_update_date.point_timestamp_a + ?slow_update_interval2 =< Now ->
                    put(?pd_slow_update_date2
                       ,#slow_update_date{point_timestamp_a = Now
                                         ,point_timestamp_b = SU2#slow_update_date.point_timestamp_a
                                         ,point_version_a = State#state.data_version
                                         ,point_version_b = SU2#slow_update_date.point_version_a
                                         ,slow_need_update_data_a = []
                                         ,slow_need_update_data_b = SU2#slow_update_date.slow_need_update_data_a});
                true -> ignore
            end,
            SU1 = get(?pd_slow_update_date1),
            if
                SU1#slow_update_date.point_timestamp_a + ?slow_update_interval1 =< Now ->
                    put(?pd_slow_update_date1
                       ,#slow_update_date{point_timestamp_a = Now
                                         ,point_timestamp_b = SU1#slow_update_date.point_timestamp_a
                                         ,point_version_a = State#state.data_version
                                         ,point_version_b = SU1#slow_update_date.point_version_a
                                         ,slow_need_update_data_a = []
                                         ,slow_need_update_data_b = SU1#slow_update_date.slow_need_update_data_a}),
                    do_persist(State);
                true -> ignore
            end;
        _ -> ignore
    end.

notice_all_rank(FromPid,ServerId,Version,{RemotePointTimestamp,RemotePointVersion})->
    ?ERR("----notice_all_rank-----start-00-ServerId:~w-- gameserver:~w",[ServerId
                                                                        ,{RemotePointTimestamp,RemotePointVersion}]),
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
%%     Filter = ets:fun2ms(fun(R) when R#drw_rank_data.server_id =:= 9 ->element(5,R) ; (R) when R#drw_rank_data.rank =< 99 -> element(5,R) end),
    Filter = [{#drw_rank_data{rank = '_',role_id = '_',server_id = '_',
                                 dr_info = '_',fighter_data = '_'},
                  [{'and',{'orelse',true,fail},{'=:=',{element,4,'$_'},ServerId}}],
                  [{element,5,'$_'}]},
                 {#drw_rank_data{rank = '_',role_id = '_',server_id = '_',
                                 dr_info = '_',fighter_data = '_'},
                  [{'and',{'orelse',true,fail},{'=<',{element,2,'$_'},InitRobotNum}}],
                  [{element,5,'$_'}]}],
    notice_all_rank2(lists:seq(1, 8),FromPid,ServerId,Version,Filter),
    ?ERR("----notice_all_rank-----end----").

notice_all_rank2([],_FromPid,_ServerId,_Version,_Filter)->
    ok;
notice_all_rank2([RankType|T],FromPid,ServerId,Version,Filter)->
    ?INFO("----notice_all_rank-----start---2----~w--",[RankType]),
    EtsName = get_ets_name(RankType),
    RankData = ets:select(EtsName, Filter),
    if
        RankData /= [] ->
            ?INFO("----notice_all_rank-----start---2--first--~w--",[lists:nth(1, RankData)]);
        true -> ignore
    end,
    RankAllGroup = [{RankType,RankData}],
    TopRecVersion = get_newest_version(),
    ?INFO("----notice_all_rank-----start----2---end---"),
    erlang:send(FromPid, {notice_update_rank,{util:now(),Version},true,RankAllGroup,TopRecVersion}),
    notice_all_rank2(T,FromPid,ServerId,Version,Filter).

quick_notice_rank(State,NeedUpdateData)->
    {A,B} = State#state.update_server_list,
    Now = util:now(),
    if
        A =:= [] andalso B =:= [] ->
            ?INFO("quick_notice_rank quick list(A,B) is blank ~w",[State]),
            State;
        State#state.send_timestamp + ?notice_delay > Now 
          orelse State#state.quick_need_update_data /= [] ->
            %% 需要等待一段时间再通知
            if
                State#state.quick_need_update_data =:= [] -> %% 说明此前还没出现需要延迟通知的数据
                    erlang:send_after(?notice_delay*1000, ?MODULE, delay_notice_rank),
                    State#state{quick_need_update_data = [NeedUpdateData]};
                true ->
                    %% 防止重复
                    NewNeedUpdateList = merge_update_list(NeedUpdateData,State#state.quick_need_update_data),
                    State#state{quick_need_update_data = NewNeedUpdateList}
            end;    
        true ->
            TopRecVersion = get_newest_version(),
            quick_notice({A,B},{quick_notice_rank,State#state.data_version,Now,[NeedUpdateData],TopRecVersion}),
            State#state{send_timestamp = Now}
    end.

get_top_hero(ServerID,LastGroup)->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    %% TODO HERE 优化
    lists:map(fun(RankType)-> 
                    {RankType,[E||E<-lists:nth(RankType,LastGroup),E#p_dr_world_rank.rank =< InitRobotNum andalso E#p_dr_world_rank.serverID =:= ServerID]}
              end, lists:seq(1, 8)).

merge_update_list([],OldQuickNeedUpdateData) ->
    OldQuickNeedUpdateData;
merge_update_list([H|T],OldQuickNeedUpdateData) ->
    New = merge_update_list(H,OldQuickNeedUpdateData),
    merge_update_list(T,New);
merge_update_list({T,NeedUpdateList},OldQuickNeedUpdateData)->
    NeedUpdateData = {T,NeedUpdateList},
    case lists:keytake(T, 1, OldQuickNeedUpdateData) of
        false ->
            [NeedUpdateData|OldQuickNeedUpdateData];
        {value,{T,OldList},OtherList} ->
            NewOldList = 
                lists:foldr(fun(R,AccList)-> 
                                    case lists:keytake(R#p_dojangrank_rank.rank, #p_dojangrank_rank.rank, AccList) of
                                        {value,_,Other} ->
                                            [R|Other];
                                        false ->
                                            [R|AccList]
                                    end
                            end, OldList, NeedUpdateList),
            [{T,NewOldList}|OtherList]
    end.

delay_notice_rank(State)->
    {A,B} = State#state.update_server_list,
    if
        A =:= [] andalso B =:= [] ->
            ?INFO("quick_notice_rank error1 ~w",[State]);
        State#state.quick_need_update_data =:= [] ->
            ?ERR("quick_notice_rank error2 ~w",[State]);
        true ->
            TopRecVersion = get_newest_version(),
            quick_notice({A,B},{quick_notice_rank,State#state.data_version,util:now(),State#state.quick_need_update_data,TopRecVersion})
    end,
    State#state{quick_need_update_data = []
               ,send_timestamp = util:now()}.

quick_notice({A,B},Msg)->
    %% 节约时间，没有对AB进行合并
    lists:foreach(fun(ServerId)-> 
                    catch send_msg:direct(ServerId,dojangrank_server,Msg)
                  end, A),
    lists:foreach(fun(ServerId)-> 
                    catch send_msg:direct(ServerId,dojangrank_server,Msg)
                  end, B).

%% [{type,RoleInfoList}]
%% 休战和非休战时，都可以加入比赛圈
update_top10(RoleInfoListGroup)->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    lists:foreach(fun({Type,RoleList})->
            EtsName = get_ets_name(Type),
            lists:foreach(fun(FighterInfo)-> 
                        {PDrWorldRank,FighterData} = FighterInfo,
                        RoleID = PDrWorldRank#p_dr_world_rank.roleID,
                        case ets:lookup(EtsName, InitRobotNum + RoleID) of
                            [] -> ignore;
                            [DuplicateFighterInfo1] -> 
                                ?ERR("update_top10 rank(~w)   <<<>>>     duplicate:~w   <<<>>>    FighterInfo:~w",[InitRobotNum + RoleID
                                                                                                                  ,DuplicateFighterInfo1#drw_rank_data.dr_info
                                                                                                                  ,FighterInfo])
                        end,
                        case ets:match_object(EtsName, #drw_rank_data{role_id=RoleID, _='_'}) of
                            [] -> ignore;
                            [DuplicateFighterInfo2] -> 
                                ets:delete(EtsName, DuplicateFighterInfo2#drw_rank_data.rank),
                                ?ERR("update_top10 roleID(~w)   <<<>>>     duplicate:~w   <<<>>>    FighterInfo:~w",[RoleID
                                                                                                                    ,DuplicateFighterInfo2#drw_rank_data.dr_info
                                                                                                                    ,FighterInfo])
                        end,
                        ets:insert(EtsName, #drw_rank_data{rank = InitRobotNum + RoleID
                                   ,role_id = RoleID
                                   ,server_id = PDrWorldRank#p_dr_world_rank.serverID
                                   ,dr_info = PDrWorldRank#p_dr_world_rank{rank = InitRobotNum + RoleID}
                                   ,fighter_data = FighterData})    %%{FighterListA,RoleLieuAddA,TalentA,TrSpecialA,SkinInfoA,LegendAddListA}
                end, RoleList)
        end, RoleInfoListGroup).

set_update_server_list_interval()->
    erlang:send_after(?update_server_list_interval*1000,self(),update_server_list_tick).

set_persist_interval(_State)->
    Now = util:now(),
    {_,_,Day} = TodayDate = erlang:date(),
    InitStep1 = util:datetime_to_seconds({TodayDate,{0,0,0}}) + data_dojangrank:get(init_step_1) - 60, % 要给初始化预留一分钟时间，以便游戏服发来消息的时候
    InitStep2 = util:datetime_to_seconds({TodayDate,{0,0,0}}) + data_dojangrank:get(init_step_2),
    if
        1 =:= Day ->
            if
                InitStep1 > Now andalso (Now + ?state_check_max_interval) > InitStep1 ->
                    erlang:send_after(max(1,InitStep1 - Now)*1000,self(),state_check_tick);
                InitStep2 > Now andalso (Now + ?state_check_max_interval) > InitStep2 ->
                    erlang:send_after(max(1,InitStep2 - Now)*1000,self(),state_check_tick);
                true ->
                    erlang:send_after(?state_check_max_interval*1000,self(),state_check_tick)
            end;    
        true ->
            erlang:send_after(?state_check_max_interval*1000,self(),state_check_tick)
    end.

%%  状态检查切换时间在凌晨1点
check_state(State)->
    {_,_,Day} = TodayDate = erlang:date(),
    InitStep1 = util:datetime_to_seconds({TodayDate,{0,0,0}}) + data_dojangrank:get(init_step_1) - 60, % 要给初始化预留一分钟时间，以便游戏服发来消息的时候
    InitStep2 = util:datetime_to_seconds({TodayDate,{0,0,0}}) + data_dojangrank:get(init_step_2),
    DateTime = {date(),time()},
    StartTime = data_dojangrank:get(start_date),
    if
        DateTime < StartTime ->
            State;
        State#state.current_date /= TodayDate ->
            case TodayDate of
                %% 每个1号会执行的逻辑，并且此时应该是关闭状态
                {_,_,1} ->
                    NowSession = get_now_session(),
                    Now = util:now(),
                    if
                        State#state.dojangrank_world_session /= NowSession 
                          andalso InitStep1 =< Now ->
                            swap_session_start(0),
                            State#state{current_date = TodayDate
                                       ,dojangrank_world_session = NowSession
                                       ,data_version = State#state.data_version + 1};
                        true ->
                            State
                    end;
                _ ->
                    State#state{current_date = TodayDate}
            end;
        true ->
            if
                Day =:= 1 ->
                    Now = util:now(),
                    if
                        InitStep2 =< Now ->
                            swap_session_end();
                        true -> ignore
                    end;
                true -> ignore
            end,
            State
    end.

%% 调试命令中会立即调用
%% 实际上过了0点过data_dojangrank:get(init_step_1)时调用，

swap_session_start(DebugFlag)->
    ?ERR("----------------swap_session_start-------------------",[]),
    erlang:send(?MODULE, {swap_session_start,DebugFlag}).

swap_session_step1(RankType,Now,Filter)->
    EtsName = get_ets_name(RankType),
    RankListAll = ets:select(EtsName, Filter),
    FileName = io_lib:format("log/dojangrank_world_log_~w_~w.txt", [Now,RankType]),
    StartTs = util:now_mili(),
    {ok, LogFile} = file:open(FileName, write),
    io:format(LogFile, "~w", [RankListAll]),
    EndTs = util:now_mili(),
    ?ERR("swap_session_step1 save file ~w",[{RankType
                                            ,EndTs - StartTs}]),
    file:close(LogFile).

%% 数据保存为历史数据
swap_session_step3(RankType,_Now,Filter)->
    StartTs = util:now_mili(),
    EtsName = get_ets_name(RankType),
    RankListAll00 = ets:select(EtsName, Filter),
    
    %% 保存当前排行榜作为历史排行
    RankListAll0 = lists:sort(fun(A,B)-> A#p_dr_world_rank.rank < B#p_dr_world_rank.rank
                             end,RankListAll00),
    RankListAllBin = db_sql:compress_encode(RankListAll0),
    put({?pd_history_rank,RankType},RankListAllBin),
    db_sql:set_dojangrank_world_history(RankType,RankListAllBin),
    
    %% 清除现在的榜单
    ets:delete_all_objects(EtsName),
    db_sql:clear_dojangrank_world_rank(RankType),
    EndTs = util:now_mili(),
    ?INFO("swap_session_step3 date swap finish ~w",[{EndTs - StartTs}]).

%% 初始化话结束必须满足三个条件
%% 1、时间达到切换时机，且大于第一次开启时间        %% TODO 妥善的决定何时关闭服务器同步
%% 2、收集的机器人数量够多
%% 3、收集的区服数量够多
swap_session_end()->
    case get(?pd_robot_temp) of
        {_,_,?wait_collect_top10,_} ->
            erlang:send(?MODULE,persist_dojangrank_world_rank_start);
        _ -> ignore
    end.

do_persist(State)->
    SU= get(?pd_slow_update_date1),
    UpdateGroup = SU#slow_update_date.slow_need_update_data_b,
    if
        [] /= UpdateGroup ->
            ?INFO("do_persist ~w",[{T,erlang:length(RL)}||{T,RL}<-UpdateGroup]);
        true -> ignore 
    end,
    persist_dojangrank_world_rank(UpdateGroup),
    db_sql:set_etc(?DB_ETC_KEY_WORLD_DOJANGRANK,[State]),
    db_sql:set_etc(?DB_ETC_KEY_DJRW_CACHE_1,get(?pd_slow_update_date1)),
    db_sql:set_etc(?DB_ETC_KEY_DJRW_CACHE_2,get(?pd_slow_update_date2)),
    erlang:garbage_collect().

get_need_update_date({0,0},_)->
    false;
get_need_update_date(_,[])->
    false;
get_need_update_date({RemotePointTimestamp,RemotePointVersion},[SlowDate|OtherSlowDate])->
    if
        SlowDate#slow_update_date.point_timestamp_b > RemotePointTimestamp
          orelse SlowDate#slow_update_date.point_version_b > RemotePointVersion ->
            %% fail 不能使用这个阶段你的数据
            get_need_update_date({RemotePointTimestamp,RemotePointVersion},OtherSlowDate);
        true ->
            {true,SlowDate}
    end.

get_ets_name(Index) when 0 =< Index andalso Index =< 8 ->
    list_to_atom(lists:concat(['ets_dr_world_',Index])).

init_process_data()->
    Now = util:now(),
    All = ets:all(),
    %% 初始化ets
    lists:foreach(fun(Index)->
                    EtsName = get_ets_name(Index),
                    case lists:member(EtsName, All) of
                        true -> 
                            ets:delete_all_objects(EtsName);
                        false -> 
                            %% {排名,展示数据,阵容数据}
                            ets:new(EtsName, [{keypos, #drw_rank_data.rank}, set, public, named_table])
                    end
        end, lists:seq(1, 8)),
    %% 初始化State
    State0 = 
        case db_sql:get_etc(?DB_ETC_KEY_WORLD_DOJANGRANK) of
            [State00] when erlang:is_record(State00, state)->
                lists:foreach(fun(T)->
                                EtsName = get_ets_name(T),
                                ets:insert(EtsName, db_sql:get_dojangrank_world_rank(T))
                        end,lists:seq(1,8)),
                State00;
            X ->
                ?ERR("init_process_data ~w",[X]),
                swap_session_start(0),
                #state{dojangrank_world_session = get_now_session()}
        end,
    State = State0#state{quick_need_update_data = []
                        ,update_server_list = {[],[]}
                        ,send_timestamp = 0},

    %% 初始化历史排名
    lists:foreach(fun(RankType)->
            put({?pd_history_rank,RankType},db_sql:get_dojangrank_world_history(RankType))
        end, lists:seq(1,8)),
    %% 初始化巅峰录像
    put(?pd_top_fightrec,db_sql:get_dojangrank_world_fightrec_group()),
    
    %% 初始化慢更新缓存数据
    %% 取出缓存的慢更新数据，避免游戏服因为跨服重启，而大量同步数据
    SlowUpdateDate1 = 
        case db_sql:get_etc(?DB_ETC_KEY_DJRW_CACHE_1) of
            DJRW1 when erlang:is_record(DJRW1, slow_update_date) ->
                DJRW1;
            _ ->
                #slow_update_date{point_version_b = State#state.data_version, point_timestamp_b=Now}
        end,
    SlowUpdateDate2 = 
        case db_sql:get_etc(?DB_ETC_KEY_DJRW_CACHE_2) of
            DJRW2 when erlang:is_record(DJRW2, slow_update_date) ->
                DJRW2;
            _ ->
                #slow_update_date{point_version_b = State#state.data_version, point_timestamp_b=Now}
        end,
    put(?pd_slow_update_date1,SlowUpdateDate1),
    put(?pd_slow_update_date2,SlowUpdateDate2),
    %% pd_slow_update_date3 数据量过大，暂不保存至ets
    put(?pd_slow_update_date3,#slow_update_date{point_version_b = State#state.data_version, point_timestamp_b=Now}),
    State.

check_dojangrank_world_fight(EtsName,RoleID,EnemyRoleId,EnemyRank)->
    AtkRankData = case ets:match_object(EtsName, #drw_rank_data{role_id=RoleID, _='_'}) of
                      [] ->
                          ?undefined;
                      [Info1] ->
                          Info1
                  end,
    DefRankData = case ets:lookup(EtsName, EnemyRank) of
                      [] ->
                          ?undefined;
                      [Info2] ->
                          Info2
                  end,
    OpenState = role_dojangrank:get_open_state(),
    PdRobotTemp = get(?pd_robot_temp),
    if
        PdRobotTemp /= ?undefined ->
            ?ERR("check_dojangrank_world_fight error 1 ~w",[PdRobotTemp]),
            {false,6};
        OpenState /= 2 ->
            ?ERR("check_dojangrank_world_fight error 2 ~w",[OpenState]),
            {false,6};
        DefRankData =:= ?undefined ->
            ?ERR("check_dojangrank_world_fight DefRankData: ~w",[DefRankData]),
            {false,6};
        AtkRankData =:= ?undefined ->
            ?ERR("check_dojangrank_world_fight AtkRankData: ~w",[AtkRankData]),
            {false,6};
        EnemyRoleId /= DefRankData#drw_rank_data.role_id ->
            ?ERR("check_dojangrank_world_fight:~w   <<-------->>   ~w  <<--------->> ~w",[[EtsName,RoleID,EnemyRoleId,EnemyRank]
                                                                                         ,AtkRankData#drw_rank_data.dr_info
                                                                                         ,DefRankData#drw_rank_data.dr_info]),
            {false,3};
        true ->
            {true,AtkRankData,DefRankData}
    end.

do_dojangrank_fight(AtkRankInfo,DefRankInfo)->
    ?INFO("do_dojangrank_fight start ~w",[[AtkRankInfo,DefRankInfo]]),
    {FighterListAtk,RoleLieuAddAtk,TalentAtk,TrSpecialAtk,SkinInfoAtk,LegendAddListAtk} = AtkRankInfo,
    {FighterListDef,RoleLieuAddDef,TalentDef,TrSpecialDef,SkinInfoDef,LegendAddListDef} = DefRankInfo,
    FightRes = ?CATCH(role_fight:new(FighterListAtk,FighterListDef,RoleLieuAddAtk,RoleLieuAddDef
                                    ,TalentAtk,TalentDef,TrSpecialAtk,TrSpecialDef
                                    ,SkinInfoAtk,SkinInfoDef,LegendAddListAtk,LegendAddListDef)),
    case FightRes of
        {_Result, FightRecord, _State} ->
            FightRecord;
        ERR ->
            ?ERR("do_dojangrank_fight error ERR:  ~w",[ERR]),
            #sc_fight_request{actionList=[],fighterList=[],result=false}
    end.

is_game_server(NodeName)->
    Platform = data_setting:get(platform),
    is_game_server(erlang:atom_to_list(Platform)
                  ,erlang:atom_to_list(NodeName)).
is_game_server([],_)->
    true;
is_game_server([H1|T1],[H2|T2])->
    if
        H1 =:= H2 ->
            is_game_server(T1,T2);
        true ->
            false
    end.

%% ListGroup :: p_dr_world_rank
persist_dojangrank_world_rank(ListGroup)->
    persist_dojangrank_world_rank2(ListGroup).

persist_dojangrank_world_rank2([])->
    ok;
persist_dojangrank_world_rank2([{Type,Rank}|T])->
    EtsName = get_ets_name(Type),
    AllList = 
        lists:foldl(fun
                       (RankInfo,AccList) when erlang:is_record(RankInfo, p_dr_world_rank)->
                            [DrwData] = ets:lookup(EtsName, RankInfo#p_dr_world_rank.rank),
%%                             ?ERR("--------------persist_dojangrank_world_rank2a------------(~w)>>>>>>>>> ~w",[RankInfo#p_dr_world_rank.rank,DrwData]),
                            [[DrwData#drw_rank_data.rank
                            ,DrwData#drw_rank_data.role_id
                            ,DrwData#drw_rank_data.server_id 
                            ,db_sql:to_bin(DrwData#drw_rank_data.dr_info)
                            ,db_sql:to_bin(DrwData#drw_rank_data.fighter_data)]|AccList];
                       (DrwData,AccList) when erlang:is_record(DrwData, drw_rank_data)->
%%                             ?ERR("--------------persist_dojangrank_world_rank2b------------(~w)>>>>>>>>> ~w",[DrwData#drw_rank_data.rank,DrwData]),
                            [[DrwData#drw_rank_data.rank
                            ,DrwData#drw_rank_data.role_id
                            ,DrwData#drw_rank_data.server_id 
                            ,db_sql:to_bin(DrwData#drw_rank_data.dr_info)
                            ,db_sql:to_bin(DrwData#drw_rank_data.fighter_data)]|AccList]
            end, [], Rank),
    db_sql:persist_dojangrank_world_rank(Type,erlang:length(AllList),200,AllList),
    persist_dojangrank_world_rank2(T).

save_top_rec(RankType,ReplayInfo0,FightRecord)->
    TopFightRecUid = 
        db_sql:insert_new_dojangrank_world_fightrec(ReplayInfo0#p_dr_dojang_replay_info.attacker_info#p_dr_world_rank.roleID
                                                   ,ReplayInfo0#p_dr_dojang_replay_info.defender_info#p_dr_world_rank.roleID
                                                   ,ReplayInfo0#p_dr_dojang_replay_info.attacker_info#p_dr_world_rank.serverID
                                                   ,ReplayInfo0#p_dr_dojang_replay_info.defender_info#p_dr_world_rank.serverID
                                                   ,ReplayInfo0#p_dr_dojang_replay_info.is_win
                                                   ,FightRecord
                                                   ,ReplayInfo0#p_dr_dojang_replay_info.attacker_info
                                                   ,ReplayInfo0#p_dr_dojang_replay_info.defender_info
                                                   ,RankType),
    {_,OldGroup} = get(?pd_top_fightrec),
    NewGroup = 
        lists:map(fun(RT)-> 
                OldList = lists:nth(RT, OldGroup),
                if
                    RT =:= RankType ->
                        lists:reverse(lists:sort(lists:sublist([{TopFightRecUid,RankType,ReplayInfo0#p_dr_dojang_replay_info{replayUID = TopFightRecUid},FightRecord}|OldList]
                                                              ,?top_rec_num)));
                    true ->
                        OldList
                end
            end, lists:seq(1, 8)),
    put(?pd_top_fightrec,{TopFightRecUid,NewGroup}),
    TopFightRecUid.
  
get_newest_version()->
    {TopFightRecUid,_} = get(?pd_top_fightrec),
    TopFightRecUid.

get_new_score_by_rank(_Rank)->
    0.

%% ====================================================================
%% Debug functions
%% ====================================================================

%% force_init_session()->
%%     erlang:send(?MODULE,debug_force_init_session).

%% force_notice_all_server()->
%%     AllServer = [E||{E,_}<-ets:tab2list(?ETS_NODE_INFO_TABLE2),is_game_server(E)],
%%     erlang:send(?MODULE,{force_notice_all_server,AllServer}).

fix_player_time([])->
    ok;
fix_player_time([{ServerID,FixDataList}|Ohter])->
    send_msg:direct(ServerID
                   ,dojangrank_server
                   ,{fix_pay_time,FixDataList}),
    fix_player_time(Ohter).

force_slow_update(ServerID)->
    erlang:send(?MODULE,{force_slow_update,ServerID}).

debug_show_rank_num()->
    lists:map(fun(RankType)->
            {ets:info(get_ets_name(RankType),size)
            ,ets:info(get_ets_name(RankType),memory)}
        end, lists:seq(1, 8)).

check_world_server()->
    Res1 = 
        lists:map(fun(RankType)-> 
                EtsName = dojangrank_world_server:get_ets_name(RankType),
                [E||E<-ets:tab2list(EtsName),false =:= erlang:is_record(E, drw_rank_data)]
            end, lists:seq(1, 8)),
    Res2 = 
        lists:map(fun(RankType)-> 
                EtsName = dojangrank_world_server:get_ets_name(RankType),
                [E||E<-ets:tab2list(EtsName),false =:= erlang:is_record(E#drw_rank_data.dr_info, p_dr_world_rank)]
            end, lists:seq(1, 8)),
    Res3 = 
        lists:map(fun(RankType)-> 
                EtsName = dojangrank_world_server:get_ets_name(RankType),
                lists:foldr(fun(RoleID,AccList)->
                        R =  ets:match_object(EtsName, #drw_rank_data{role_id=RoleID, _='_'}),
                        L = erlang:length(R),
                        if
                            L >= 2 ->
                                [R|AccList];
                            true ->
                                AccList
                        end
                    end, [], [E#drw_rank_data.role_id ||E<-ets:tab2list(EtsName)])
            end, lists:seq(1, 8)),
    erlang:garbage_collect(),
    {Res1,Res2,Res3}.

force_notice_single(ServerID,RankType)->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    Filter = [{#drw_rank_data{rank = '_',role_id = '_',server_id = '_',
                             dr_info = '_',fighter_data = '_'},
              [{'and',{'orelse',true,fail},{'=:=',{element,4,'$_'},ServerID}}],
              [{element,5,'$_'}]},
             {#drw_rank_data{rank = '_',role_id = '_',server_id = '_',
                             dr_info = '_',fighter_data = '_'},
              [{'and',{'orelse',true,fail},{'=<',{element,2,'$_'},InitRobotNum}}],
              [{element,5,'$_'}]}],
    EtsName = get_ets_name(RankType),
    RankData = ets:select(EtsName, Filter),
    if
        RankData /= [] ->
            ?INFO("force_notice_single",[erlang:length(RankData)]);
        true -> ignore
    end,
    send_msg:direct(ServerID,dojangrank_server,{fix_world_rank,[{RankType,RankData}]}).

debug_notice_all_rank(RankType,ServerId,Version,TopRecVersion)->
    InitRobotNum = data_dojangrank:get(init_robot_num_max),
    Filter = [{#drw_rank_data{rank = '_',role_id = '_',server_id = '_',
                                 dr_info = '_',fighter_data = '_'},
                  [{'and',{'orelse',true,fail},{'=:=',{element,4,'$_'},ServerId}}],
                  [{element,5,'$_'}]},
                 {#drw_rank_data{rank = '_',role_id = '_',server_id = '_',
                                 dr_info = '_',fighter_data = '_'},
                  [{'and',{'orelse',true,fail},{'=<',{element,2,'$_'},InitRobotNum}}],
                  [{element,5,'$_'}]}],
    EtsName = get_ets_name(RankType),
    RankData = ets:select(EtsName, Filter),
    RankAllGroup = [{RankType,RankData}],
    send_msg:direct(ServerId,dojangrank_server,{notice_update_rank,{util:now(),Version},true,RankAllGroup,TopRecVersion}).



