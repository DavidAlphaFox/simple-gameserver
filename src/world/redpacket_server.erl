-module(redpacket_server).
-compile(export_all).
-behaviour(gen_server).
-include("def_mail.hrl").
-include("def_role.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(persist_interval_second, 180).   %% 180
-define(world_rank_interval, 3600).
-define(pd_world_rank_list, pd_world_rank_list).
-define(pd_world_rank_version, pd_world_rank_version).
-define(pd_world_rank_min, pd_world_rank_min).
-define(pd_world_rank_reward, pd_world_rank_reward).

-record(state,{is_open=0
              ,open_time=[0,0,0]}).
-record(redbox_info,{rp_type = 0
                       ,rp_num = 0
                       ,rp_residual = []    %% [5,12]
                       ,rp_recv_list = []}).  %% [{roleid,get_num}]

%%%%%===========需要固化保存的数据
-define(golder_list, golder_list).  %% 金主列表
%%%%%===========不需要固化保存的数据

%% ===================Spec Begin =========================
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().

-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ===================Spec End   =========================





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

creat_redbox(RoleID,GoldNum)->
    erlang:send(?MODULE,{creat_redbox,RoleID,GoldNum}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    random:seed(util:gen_random_seed()),
    process_flag(trap_exit,true),
    put(?pd_world_rank_version,0),
    put(?pd_world_rank_list,[]),
    put(?pd_world_rank_min,0),
    put(?golder_list,[]),
    lists:foreach(fun({K,V})->
                          put(K,V)
                  end, db_sql:get_etc(?DB_ETC_KEY_REDPACKET)),
    reset_tick_interval(),
    fix_redpacket_list(),
    erlang:send_after(180*1000, self(), world_rank_interval),
    {ok, check_state()}.

handle_call({get_pd,Pd}, _From, State) ->
    {reply, get(Pd), State};
handle_call({put_pd,Pd,Value}, _From, State) ->
    {reply, put(Pd,Value), State};
handle_call(Request, _From, State) ->
    ?ERR("handle_call function clause:request=~w",[Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?ERR("handle_cast function clause:request=~w",[Msg]),
    {noreply, State}.

handle_info({creat_redbox,RoleID,GoldNum},State)->
    Pub = role_lib:get_rolePublic(RoleID),
    case data_redpacket:get({redpacket_type,GoldNum}) of
        {RedType,RedTotal,RedPacketNum,[RedMin,RedMax]} when Pub /= [] andalso State#state.is_open =:= 1 ->
            {NewGolderInfo,UpdateType,_NewRank} = add_redbox(RoleID,GoldNum,RedType,RedTotal,RedPacketNum,RedMin,RedMax),
            RedpacketList = to_p_publisher_info_list(RoleID,NewGolderInfo),
            NewPublisher = 
                #p_publisher_info{roleID = RoleID
                                 ,roleName = Pub#rolePublic.roleName
                                 ,head = Pub#rolePublic.head
                                 ,roleTitle = Pub#rolePublic.title 
                                 ,is_male = Pub#rolePublic.isMale
                                 ,level = Pub#rolePublic.level 
                                 ,skinInfo = #p_skin_info{equip=0}
                                 ,redpacket_num = NewGolderInfo#golder_info.redpacket_num  %% 本届的获得的红包
                                 ,redpacket_list = RedpacketList
                                 ,serverID = data_setting:get(server_id)
                                 ,pay_timestamp = NewGolderInfo#golder_info.last_pay_time},
            ?unicast(RoleID, #sc_trumpet_notice_publisher{update_type = UpdateType
                                                         ,publisher = NewPublisher}),
            catch role_lib:send_server(RoleID,cs_trumpet_redpacket_status),
            RolePub = role_lib:get_rolePublic(RoleID),
            lists:foreach(fun({R,_})->
                    ?unicast(R, #sc_trumpet_new_redpacket_status{role_name = RolePub#rolePublic.roleName
                                                                     ,rmb_num = RedType}),
                    if
                        RoleID /= R ->
                            Rp2 = to_p_publisher_info_list(R,NewGolderInfo),
                            ?unicast(R, #sc_trumpet_notice_publisher{update_type = UpdateType
                                                                    ,publisher = NewPublisher#p_publisher_info{redpacket_list = Rp2}});
                        true ->
                            ignore
                    end     
                end, ets:tab2list(?ETS_ROLE_ONLINE));
        _ ->
            ignore
    end,
    {noreply, State};
handle_info({cs_trumpet_redpacket_status,RoleID,GetAllNum},State)->
    [_,EndTime,Fianl] = State#state.open_time,
    ThisNum = case lists:keysearch(RoleID, #golder_info.golder_roleid, get(?golder_list)) of
                  {value, V} ->
                      V#golder_info.redpacket_num;
                  false ->
                      0
              end,
    MsgEndTime = if
                     State#state.is_open == 0 ->
                         0;
                     State#state.is_open == 1 ->
                         EndTime;
                     State#state.is_open == 2 ->
                         Fianl;
                     true ->
                         0
                 end,
    ?unicast(RoleID, #sc_trumpet_redpacket_status{is_open = State#state.is_open
                                                 ,end_time = MsgEndTime
                                                 ,get_all_num = GetAllNum
                                                 ,pos_index = 1     %% 位置客户端判断了
                                                 ,this_num = ThisNum}),
    {noreply, State};
handle_info({client_msg, RoleID, #cs_trumpet_redpacket_get_reward{roleID = Golder
                                                                 ,redpacket_type = RedType}}, State) ->
    GolderList = get(?golder_list),
    Now = util:now(),
    SelfPub = role_lib:get_rolePublic(RoleID),
    case lists:keytake(Golder, #golder_info.golder_roleid, GolderList) of
        {value,GolderInfo,OtherList} when State#state.is_open =:= 1 ->
            RedpacketList = GolderInfo#golder_info.redbox_list,
            case check_redpacket_list(RoleID,RedType,RedpacketList) of
                false ->
                    ?unicast(RoleID, #sc_trumpet_redpacket_get_reward{result = 2,get_add_num=0});
                {true,GetNum,NewRedpacketList} ->
                    NewGolderInfo = GolderInfo#golder_info{redbox_list=NewRedpacketList},
                    NewGolderList = [NewGolderInfo|OtherList],
                    NewGolderList2 = 
                        case lists:keytake(RoleID, #golder_info.golder_roleid , NewGolderList) of
                            false ->
                                AddGolderInfo0 = #golder_info{golder_roleid = RoleID
                                                            ,redpacket_num = GetNum
                                                            ,redpacket_day = {0,{0,0,0}}
                                                            ,redbox_list = []
                                                            ,last_pay_time = Now},
                                AddGolderInfo = AddGolderInfo0#golder_info{redpacket_day = add_today_num(AddGolderInfo0,GetNum)},
                                SelfNewPublisher = 
                                    #p_publisher_info{roleID = RoleID
                                                     ,roleName = SelfPub#rolePublic.roleName
                                                     ,head = SelfPub#rolePublic.head
                                                     ,roleTitle = SelfPub#rolePublic.title 
                                                     ,is_male = SelfPub#rolePublic.isMale
                                                     ,level = SelfPub#rolePublic.level 
                                                     ,skinInfo = #p_skin_info{equip=0}
                                                     ,redpacket_num = AddGolderInfo#golder_info.redpacket_num  %% 本届的获得的红包
                                                     ,redpacket_list = []
                                                     ,serverID = data_setting:get(server_id)
                                                     ,pay_timestamp = NewGolderInfo#golder_info.last_pay_time},
                                SelfNotice = ?undefined,
                                [AddGolderInfo|NewGolderList];
                            {value,OldGolderInfo,OtherList2} ->
                                RedpacketGetMax = data_redpacket:get(redpacket_get_max),
                                CurRedpacketGet = get_today_num(OldGolderInfo),
                                ?INFO("cs_trumpet_redpacket_get_reward ~w(~w)  >>>~w",[CurRedpacketGet,RedpacketGetMax,GolderInfo]),
                                if
                                    %% 可能出现最后一次抢夺后超过上限的情况，以GolderInfo为判断标准
                                    RedpacketGetMax =< CurRedpacketGet ->
                                        AddGolderInfo = ?undefined,
                                        SelfNewPublisher = ?undefined,
                                        SelfNotice = ?undefined,
                                        false;
                                    true ->
                                        NewValue = OldGolderInfo#golder_info.redpacket_num + GetNum,
                                        AddGolderInfo = OldGolderInfo#golder_info{redpacket_num = NewValue
                                                                                 ,redpacket_day = add_today_num(OldGolderInfo,GetNum)
                                                                                 ,last_pay_time = Now},
                                        SelfRL = to_p_publisher_info_list(RoleID,AddGolderInfo),
                                        SelfNewPublisher = 
                                            #p_publisher_info{roleID = RoleID
                                                             ,roleName = SelfPub#rolePublic.roleName
                                                             ,head = SelfPub#rolePublic.head
                                                             ,roleTitle = SelfPub#rolePublic.title 
                                                             ,is_male = SelfPub#rolePublic.isMale
                                                             ,level = SelfPub#rolePublic.level 
                                                             ,skinInfo = #p_skin_info{equip=0}
                                                             ,redpacket_num = AddGolderInfo#golder_info.redpacket_num  %% 本届的获得的红包
                                                             ,redpacket_list = SelfRL
                                                             ,serverID = data_setting:get(server_id)
                                                             ,pay_timestamp = NewGolderInfo#golder_info.last_pay_time},
                                        SelfNotice = #sc_trumpet_notice_publisher{update_type = 2
                                                                                 ,publisher = SelfNewPublisher},
                                        [AddGolderInfo|OtherList2]
                                end
                        end,
                    if
                        NewGolderList2 =:= false ->
                            ?unicast(RoleID, #sc_trumpet_redpacket_get_reward{result = 3,get_add_num=0});
                        true ->
                            NewItem = #new_item{itemTypeID=?ITEM_REDPACKET_ID
                                               ,itemNum=GetNum
                                               ,itemLevel=1
                                               ,itemRank=0},
                            case catch role_lib:call_server(RoleID,{sync_add_item,NewItem,?MONEY_ADD_TYPE_OPEN_REDPACKET},1000) of
                                ok ->
                                    put(?golder_list,NewGolderList2),
                                    ?unicast(RoleID, #sc_trumpet_redpacket_get_reward{result = 1,get_add_num=GetNum}),
                                    if
                                        Golder /= RoleID ->
                                            Pub = role_lib:get_rolePublic(Golder),
                                            RL = to_p_publisher_info_list(RoleID,NewGolderInfo),
                                            NewPublisher = 
                                                #p_publisher_info{roleID = Golder
                                                                 ,roleName = Pub#rolePublic.roleName
                                                                 ,head = Pub#rolePublic.head
                                                                 ,roleTitle = Pub#rolePublic.title 
                                                                 ,is_male = Pub#rolePublic.isMale
                                                                 ,level = Pub#rolePublic.level 
                                                                 ,skinInfo = #p_skin_info{equip=0}
                                                                 ,redpacket_num = NewGolderInfo#golder_info.redpacket_num  %% 本届的获得的红包
                                                                 ,redpacket_list = RL
                                                                 ,serverID = data_setting:get(server_id)
                                                                 ,pay_timestamp = NewGolderInfo#golder_info.last_pay_time},
                                            notice_world_rank([SelfNewPublisher,NewPublisher]),
                                            ?unicast(RoleID, #sc_trumpet_notice_publisher{update_type = 2
                                                                                         ,publisher = NewPublisher});
                                        true ->
                                            notice_world_rank([SelfNewPublisher]),
                                            ignore
                                    end,
                                    if
                                        SelfNotice /= ?undefined andalso SelfNotice#sc_trumpet_notice_publisher.publisher#p_publisher_info.redpacket_list /= [] ->
                                            ?unicast(RoleID,SelfNotice);
                                        true ->
                                            ignore
                                    end;
                                _ ->
                                    ?unicast(RoleID, #sc_trumpet_redpacket_get_reward{result = 2,get_add_num=0})
                            end
                    end
            end;
        _ when State#state.is_open =:= 2 ->
            ?unicast(RoleID, #sc_trumpet_redpacket_get_reward{result = 4,get_add_num=0});
        _ ->
            ?unicast(RoleID, #sc_trumpet_redpacket_get_reward{result = 2,get_add_num=0})
    end,
    {noreply, State};
handle_info({client_msg, RoleID, #cs_trumpet_get_all_publishers{}}, State) ->
    GolderList = get(?golder_list),
    GolderList2 = sort_gold_info_list(GolderList),
    PublisherList = lists:map(fun(GolderInfo)-> 
                        Pub = role_lib:get_rolePublic(GolderInfo#golder_info.golder_roleid),
                        ?INFO("cs_trumpet_get_all_publishers redbox_list:~w",[GolderInfo#golder_info.redbox_list]),
                        RedpacketList = to_p_publisher_info_list(RoleID,GolderInfo),
                        #p_publisher_info{roleID = GolderInfo#golder_info.golder_roleid
                                         ,roleName = Pub#rolePublic.roleName
                                         ,head = Pub#rolePublic.head
                                         ,roleTitle = Pub#rolePublic.title 
                                         ,is_male = Pub#rolePublic.isMale
                                         ,level = Pub#rolePublic.level 
                                         ,skinInfo = #p_skin_info{equip=0}
                                         ,redpacket_num = GolderInfo#golder_info.redpacket_num  %% 本届的获得的红包
                                         ,redpacket_list = RedpacketList
                                         ,serverID = data_setting:get(server_id)
                                         ,pay_timestamp = GolderInfo#golder_info.last_pay_time}
            end, GolderList2),
    ?unicast(RoleID, #sc_trumpet_get_all_publishers{publisher_list = PublisherList}),
    {noreply, State};
handle_info({client_msg, RoleID, #cs_trumpet_redpacket_openclose{}}, State) ->
    ?unicast(RoleID, #sc_trumpet_redpacket_openclose{result = 1}),
    {noreply, State};

handle_info({client_msg, RoleID, #cs_trumpet_get_world_publishers{}}, State) ->
    ServerID = data_setting:get(server_id),
    GroupID = get_server_group_id(),
    Min = GroupID div 10000,
    Max = GroupID rem 10000,
    PublisherList0 = case get(?pd_world_rank_list) of
                     ?undefined ->
                         [];
                     PublisherList00 ->
                         PublisherList00
                 end,
    PublisherList = lists:map(fun(P)-> 
                        L0 = [E#redpacket_info{can_get_num=0}||E<-P#p_publisher_info.redpacket_list],
                        P2 = P#p_publisher_info{redpacket_list = L0},
                        if
                            ServerID =:= P#p_publisher_info.serverID ->
                                Pub = role_lib:get_rolePublic(P#p_publisher_info.roleID),
                                P2#p_publisher_info{roleName = Pub#rolePublic.roleName
                                                   ,head = Pub#rolePublic.head
                                                   ,roleTitle = Pub#rolePublic.title 
                                                   ,is_male = Pub#rolePublic.isMale
                                                   ,level = Pub#rolePublic.level};
                            true ->
                                P2
                        end     
            end, PublisherList0),
    PublisherList2 = sort_gold_info_list(PublisherList),
    ?unicast(RoleID, #sc_trumpet_get_world_publishers{server_id_start = Min
                                                     ,server_id_end = Max
                                                     ,publisher_list = PublisherList2}),
    {noreply, State};

handle_info({sync_world_rank,LastVersion,WorldRank,MinMun,IsGetReward}, State) ->
    put(?pd_world_rank_version,LastVersion),
    put(?pd_world_rank_list,WorldRank),
    put(?pd_world_rank_min,MinMun),
    ?INFO("sync_world_rank get new rank, len:~w v:~w (~w)",[erlang:length(WorldRank),LastVersion,IsGetReward]),
    IsReward = get(?pd_world_rank_reward),
    if
        IsGetReward =:= true andalso IsReward =:= true ->
            ?ERR("发送跨服奖励  pd_world_rank_version true,,, ~w",[sort_gold_info_list(WorldRank)]),
            GroupID = get_server_group_id(),
            ServerIDMin = GroupID div 10000,
            ServerIDMax = GroupID rem 10000,
            put(?pd_world_rank_reward,false),
            check_rank_reward2_world(1,sort_gold_info_list(WorldRank),data_redpacket:get(world_reward),{ServerIDMin,ServerIDMax});
        true ->
            ignore
    end,    
    {noreply, State};
%% 定周期处理
handle_info(world_rank_interval, State) ->
    if
        State#state.is_open == 1 ->
            fix_redpacket_list();
        true -> ignore
    end,
    reset_world_rank_interval(),
    {noreply, State};
%% 定周期处理
handle_info(interval, OldState) ->
    do_persist(),
    reset_tick_interval(OldState),
    NewState = check_state(),
    if
        OldState#state.is_open == 0 andalso NewState#state.is_open == 1 ->
            %% 推送活动开启
            lists:foreach(fun({R,_})->
                    catch role_lib:send_server(R,cs_trumpet_redpacket_status)
                end, ets:tab2list(?ETS_ROLE_ONLINE)),
            put(?pd_world_rank_version,0),
            put(?pd_world_rank_list,[]),
            put(?pd_world_rank_min,0),
            put(?golder_list,[]);
        true ->
            ignore
    end,
    check_rank_reward(OldState,NewState),
    {noreply, NewState};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({debug_del_role,RoleID},State) ->
    GolderList = get(?golder_list),
    NewGolderList = lists:filter(fun(G)-> G#golder_info.golder_roleid /= RoleID end, GolderList),
    put(?golder_list,NewGolderList),
    {noreply,State};
handle_info(i,State) ->
    GolderList = get(?golder_list),
    ?INFO("i ~w",[GolderList]),
    {noreply,State};
handle_info(debug_redpacket_world_status,State) ->
    send_msg:direct_by_name(carlos_match, redpacketworld_server, {debug_redpacket_world_status, data_setting:get(server_id)}),
    {noreply,State};
handle_info(fix_redpacket_list,State) ->
    if
        State#state.is_open == 1 ->
            NewPublisherList = 
                lists:map(fun(GolderInfo)-> 
                                GolderInfo,
                                Pub = role_lib:get_rolePublic(GolderInfo#golder_info.golder_roleid),
                                RedpacketList = to_p_publisher_info_list(GolderInfo#golder_info.golder_roleid,GolderInfo),
                                #p_publisher_info{roleID = GolderInfo#golder_info.golder_roleid
                                                 ,roleName = Pub#rolePublic.roleName
                                                 ,head = Pub#rolePublic.head
                                                 ,roleTitle = Pub#rolePublic.title 
                                                 ,is_male = Pub#rolePublic.isMale
                                                 ,level = Pub#rolePublic.level 
                                                 ,skinInfo = #p_skin_info{equip=0}
                                                 ,redpacket_num = GolderInfo#golder_info.redpacket_num  %% 本届的获得的红包
                                                 ,redpacket_list = RedpacketList
                                                 ,serverID = data_setting:get(server_id)
                                                 ,pay_timestamp = GolderInfo#golder_info.last_pay_time}
                    end, get(?golder_list)),
            notice_world_rank(NewPublisherList);
        true -> ignore
    end,
    {noreply,State};
handle_info({debug_redpacket_world_status_response, GroupInfo},State) ->
    io:format("debug_redpacket_world_status_response\n~w",[GroupInfo]),
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
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
reset_world_rank_interval()->
    erlang:send_after((?world_rank_interval+util:random_int(1,100))*1000, self(), world_rank_interval).

%% 设置下一次定周期触发
reset_tick_interval()->
    erlang:send_after(?persist_interval_second*1000, self(), interval).
    
reset_tick_interval(State)->
    Now = util:now(),
    [_,EndTime,Fianl] = State#state.open_time,
    Time = if
                 State#state.is_open == 0 ->
                     ?persist_interval_second;
                 State#state.is_open == 1 andalso (EndTime - Now) =< ?persist_interval_second ->
                     ?persist_interval_second - (EndTime - Now);
                 State#state.is_open == 2 andalso (Fianl - Now) =< ?persist_interval_second ->
                     ?persist_interval_second - (Fianl - Now);
                 true ->
                     ?persist_interval_second
             end,
    erlang:send_after(erlang:max(1, Time)*1000, self(), interval).

do_persist()->
    ProcDictList = [E||E<-erlang:get(),is_persist(E)],
    db_sql:set_etc(?DB_ETC_KEY_REDPACKET,ProcDictList).

is_persist({?golder_list,_})->
    true;
is_persist(_) ->
    false.

check_state()->
    TimeList = lists:sort(data_redpacket:get(open_time)),
    NowTime = util:seconds_to_datetime(util:now()),
    {IsOpen,OpenTime} = check_state2(NowTime,TimeList),
    #state{is_open=IsOpen
          ,open_time=OpenTime}.
check_state2(_NowTime,[]) ->
    {0,[0,0,0]};
check_state2(NowTime,[[Start,End,Final]|TimeList])->
    if
        Start =< NowTime andalso NowTime =< End ->
            {1,[util:datetime_to_seconds(Start)
               ,util:datetime_to_seconds(End)
               ,util:datetime_to_seconds(Final)]};
        End < NowTime andalso NowTime =< Final ->
            {2,[util:datetime_to_seconds(Start)
               ,util:datetime_to_seconds(End)
               ,util:datetime_to_seconds(Final)]};
        true ->
            check_state2(NowTime,TimeList)
    end.

sort_gold_info_list(GolderList)->
    lists:sort(fun(A,B) when erlang:is_record(A, golder_info) andalso erlang:is_record(B, golder_info) -> 
                    if
                        A#golder_info.redpacket_num > B#golder_info.redpacket_num -> true;
                        A#golder_info.redpacket_num < B#golder_info.redpacket_num -> false;
                        A#golder_info.last_pay_time < B#golder_info.last_pay_time -> true;
                        A#golder_info.last_pay_time > B#golder_info.last_pay_time -> false;
                        A#golder_info.golder_roleid < B#golder_info.golder_roleid -> true;
                        true -> false
                    end;
                (A,B) when erlang:is_record(A, p_publisher_info) andalso erlang:is_record(B, p_publisher_info) -> 
                    if
                        A#p_publisher_info.redpacket_num > B#p_publisher_info.redpacket_num -> true;
                        A#p_publisher_info.redpacket_num < B#p_publisher_info.redpacket_num -> false;
                        A#p_publisher_info.pay_timestamp < B#p_publisher_info.pay_timestamp -> true;
                        A#p_publisher_info.pay_timestamp > B#p_publisher_info.pay_timestamp -> false;
                        A#p_publisher_info.roleID < B#p_publisher_info.roleID -> true;
                        true -> false
                    end
                end, GolderList).

add_redbox(RoleID,GoldNum,RedType,RedTotal,RedPacketNum,RedMin,RedMax)->
    Now = util:now(),
    GolderList = get(?golder_list),
    RpResidual = segment_redbox(RedTotal,RedPacketNum,RedMin,RedMax,[]),
    NewRedpacketInfo = #redbox_info{rp_type = RedType
                                      ,rp_num = RedPacketNum
                                      ,rp_residual = RpResidual
                                      ,rp_recv_list = []},
    case lists:keytake(RoleID, #golder_info.golder_roleid, GolderList) of
        {value,GolderInfo,OtherList} ->
            NewGolderInfo = GolderInfo#golder_info{redbox_list=[NewRedpacketInfo|GolderInfo#golder_info.redbox_list]
                                                  ,redpacket_num = GolderInfo#golder_info.redpacket_num + GoldNum
                                                  ,last_pay_time = Now},
            NewGolderInfoList = [NewGolderInfo|OtherList],
            if
                GolderInfo#golder_info.redbox_list /= [] ->
                    UpdateType = 2;
                true ->
                    UpdateType = 1
            end;
        false ->
            NewGolderInfo = #golder_info{golder_roleid = RoleID
                                        ,redpacket_num = GoldNum
                                        ,redpacket_day = {0,{0,0,0}}
                                        ,redbox_list = [NewRedpacketInfo]
                                        ,last_pay_time = Now},
            NewGolderInfoList = [NewGolderInfo|GolderList],
            UpdateType = 1
    end,
    Pub = role_lib:get_rolePublic(RoleID),
    RL = to_p_publisher_info_list(RoleID,NewGolderInfo),
    NewPublisher = 
        #p_publisher_info{roleID = RoleID
                         ,roleName = Pub#rolePublic.roleName
                         ,head = Pub#rolePublic.head
                         ,roleTitle = Pub#rolePublic.title 
                         ,is_male = Pub#rolePublic.isMale
                         ,level = Pub#rolePublic.level 
                         ,skinInfo = #p_skin_info{equip=0}
                         ,redpacket_num = NewGolderInfo#golder_info.redpacket_num  %% 本届的获得的红包
                         ,redpacket_list = RL
                         ,serverID = data_setting:get(server_id)
                         ,pay_timestamp = NewGolderInfo#golder_info.last_pay_time},
    notice_world_rank(NewPublisher),
    NewItem = #new_item{itemTypeID=?ITEM_REDPACKET_ID
                       ,itemNum=GoldNum
                       ,itemLevel=1
                       ,itemRank=0},
    case catch role_lib:call_server(RoleID,{sync_add_item,NewItem,?MONEY_ADD_TYPE_SELF_REDPACKET},1000) of
        ok ->
            ignore;
        _ ->
            ?ERR("add_redbox add redpacket fail ~w(~w)",[RoleID,GoldNum])
    end,
    put(?golder_list,NewGolderInfoList),
    NewRank = erlang:length(lists:filter(fun(E)-> E#golder_info.redpacket_num > NewGolderInfo#golder_info.redpacket_num end, NewGolderInfoList))+1,
    {NewGolderInfo,UpdateType,NewRank}.

segment_redbox(RedTotal,1,_RedMin,_RedMax,AccList)->
    [RedTotal|AccList];
segment_redbox(RedTotal,RedPacketNum,RedMin,RedMax,AccList) when RedPacketNum > 1 ->
    PacketNum = util:random_int(RedMin, RedMax),
    RedTotal2 = RedTotal-PacketNum,
    NewRedPacketNum = RedPacketNum-1,
    if
        RedTotal2 < (NewRedPacketNum*RedMin) ->
            AccList2 = [RedTotal - (RedMin*(RedPacketNum-1))|AccList],
            lists:duplicate(RedPacketNum-1,RedMin) ++ AccList2;
        RedTotal2 > (NewRedPacketNum*RedMax) ->
            AccList2 = [RedTotal - (RedMax*(RedPacketNum-1))|AccList],
            lists:duplicate(RedPacketNum-1,RedMax) ++ AccList2;
        true ->
            segment_redbox(RedTotal2,NewRedPacketNum,RedMin,RedMax,[PacketNum|AccList])
    end.

check_redpacket_list(RoleID,RedType,RedpacketList)->
    check_redpacket_list(RoleID,RedType,RedpacketList,[]).
check_redpacket_list(_RoleID,_RedType,[],_)->
    false;
check_redpacket_list(RoleID,RedType,[Rp|OtherList],AccList)->
    case lists:keymember(RoleID, 1, Rp#redbox_info.rp_recv_list) of
        false when Rp#redbox_info.rp_type =:= RedType andalso Rp#redbox_info.rp_residual /= [] ->
            RedNum = util:random_one_from_list(Rp#redbox_info.rp_residual),
            NewRpResidual = Rp#redbox_info.rp_residual -- [RedNum],
            NewRpRecvList  = [{RoleID,RedNum}|Rp#redbox_info.rp_recv_list],
            NewRp = Rp#redbox_info{rp_residual = NewRpResidual
                                     ,rp_recv_list = NewRpRecvList},
            {true, RedNum, [NewRp|AccList] ++ OtherList};
        _ ->
            check_redpacket_list(RoleID,RedType,OtherList,[Rp|AccList])
%%         true ->
%%             check_redpacket_list(RoleID,RedType,OtherList,[Rp|AccList]);
%%         false when Rp#redbox_info.rp_residual =:= [] ->
%%             check_redpacket_list(RoleID,RedType,OtherList,[Rp|AccList])
    end.

to_p_publisher_info_list(GolderInfo)->
    lists:foldl(fun(RedboxInfo,AccList)->
                        case lists:keytake(RedboxInfo#redbox_info.rp_type, #redpacket_info.redpacket_type, AccList) of
                            {value,Value,Other} ->
                                New = Value#redpacket_info{total_num = Value#redpacket_info.total_num + 1},
                                [New|Other];
                            false ->
                                New = #redpacket_info{redpacket_type = RedboxInfo#redbox_info.rp_type
                                                     ,can_get_num = 0
                                                     ,total_num = 1},
                                [New|AccList]
                        end 
            end,[],GolderInfo#golder_info.redbox_list).
    
to_p_publisher_info_list(RoleID,GolderInfo)->
    lists:foldl(fun(RedboxInfo,AccList)->
                        CanGetValue = case lists:keymember(RoleID, 1, RedboxInfo#redbox_info.rp_recv_list) of
                                          false ->
                                              case erlang:length(RedboxInfo#redbox_info.rp_residual) of
                                                  0 -> 0;
                                                  _ -> 1
                                              end;
                                          true -> 0
                                      end,
                        case lists:keytake(RedboxInfo#redbox_info.rp_type, #redpacket_info.redpacket_type, AccList) of
                            {value,Value,Other} ->
                                New = Value#redpacket_info{can_get_num = Value#redpacket_info.can_get_num + CanGetValue
                                                          ,total_num = Value#redpacket_info.total_num + 1},
                                [New|Other];
                            false ->
                                New = #redpacket_info{redpacket_type = RedboxInfo#redbox_info.rp_type
                                                     ,can_get_num = CanGetValue
                                                     ,total_num = 1},
                                [New|AccList]
                        end 
            end,[],GolderInfo#golder_info.redbox_list).
    
check_rank_reward(OldState,NewState)->
    if
        OldState#state.is_open == 1 andalso NewState#state.is_open == 2 ->
            GolderList2 = get(?golder_list),
            GolderListSorted = sort_gold_info_list(GolderList2),
            ?ERR("redpacket next turn ~w",[GolderListSorted]),
            check_rank_reward2_local(1,GolderListSorted,data_redpacket:get(rank_reward)),
            put(?pd_world_rank_reward,true),
            send_msg:direct_by_name(carlos_match, redpacketworld_server, {sync_world_rank, data_setting:get(server_id), 1, true}),
            %% 推送活动关闭
            lists:foreach(fun({R,_})->
                    catch role_lib:send_server(R,cs_trumpet_redpacket_status)
                end, ets:tab2list(?ETS_ROLE_ONLINE));
        OldState#state.is_open == 2 andalso NewState#state.is_open == 0 ->
            put(?pd_world_rank_version,0),
            put(?pd_world_rank_list,[]),
            put(?pd_world_rank_min,0),
            put(?golder_list,[]),
            %% 推送活动关闭
            lists:foreach(fun({R,_})->
                    catch role_lib:send_server(R,cs_trumpet_redpacket_status)
                end, ets:tab2list(?ETS_ROLE_ONLINE));
        true ->
            ignore
    end.

check_rank_reward2_local(_,[],_CfgRankReward)->
    ignore;
check_rank_reward2_local(_,_,[])->
    ignore;
check_rank_reward2_local(Index,[H|OtherGolderListSorted],[C|CfgRankReward])->
    {{Min,Max},Reward} = C,
    if
        Max >= Index andalso Min =< Index ->
            case role_lib:get_rolePublic(H#golder_info.golder_roleid) of
                [] ->
                    ignore;
                Pub ->
                    mail_server:send_sys_mail(H#golder_info.golder_roleid, ?MAIL_REDPACKET_RANK
                                             ,[Pub#rolePublic.roleName,Index,H#golder_info.redpacket_num,Index], ""
                                             ,Reward)
            end;
        true->
            ?ERR("check_rank_reward2_local r:~w (~w,~w):~w",[H#golder_info.golder_roleid,Min,Max,Index])
    end,
    if
        Max =< Index ->
            check_rank_reward2_local(Index+1,OtherGolderListSorted,CfgRankReward);
        true->
            check_rank_reward2_local(Index+1,OtherGolderListSorted,[C|CfgRankReward])
    end.

check_rank_reward2_world(_,[],_CfgRankReward,{_Min,_Max})->
    ignore;
check_rank_reward2_world(_,_,[],{_Min,_Max})->
    ignore;
check_rank_reward2_world(Index,[H|OtherGolderListSorted],[C|CfgRankReward],{ServerIDMin,ServerIDMax})->
    {{Min,Max},Reward} = C,
    if
        Max >= Index andalso Min =< Index ->
            case role_lib:get_rolePublic(H#p_publisher_info.roleID) of
                [] ->
                    ignore;
                Pub ->
                    mail_server:send_sys_mail(H#p_publisher_info.roleID, ?MAIL_REDPACKET_WORLD
                                             ,[Pub#rolePublic.roleName,ServerIDMin,ServerIDMax,Index,H#p_publisher_info.redpacket_num], ""
                                             ,Reward)
            end;
        true->
            ?ERR("check_rank_reward2_world r:~w (~w,~w):~w",[H#p_publisher_info.roleID,Min,Max,Index])
    end,
    if
        Max =< Index ->
            check_rank_reward2_world(Index+1,OtherGolderListSorted,CfgRankReward,{ServerIDMin,ServerIDMax});
        true->
            check_rank_reward2_world(Index+1,OtherGolderListSorted,[C|CfgRankReward],{ServerIDMin,ServerIDMax})
    end.

get_rank_p_reward_info(Rank,Cfg)->
    case get_rank_reward(Rank,Cfg) of
        ?undefined -> [];
        SellReward -> [activity_server:sell_reward2p_reward_info(SellReward)]
    end.

get_rank_reward(_Rank,[])->
    ?undefined;
get_rank_reward(Rank,[{{Min,Max},Reward}|OtherCfgRankReward])->
    if
        Min =< Rank andalso Rank =< Max ->
            Reward;
        true ->
            get_rank_reward(Rank,OtherCfgRankReward)
    end.
    
get_total_value(RedboxList)->
    get_total_value(0,RedboxList).
get_total_value(Acc,[])->
    Acc;
get_total_value(Acc,[H|OtherRedboxList]) when H#redbox_info.rp_type == 1 ->
    get_total_value(Acc + 6,OtherRedboxList);
get_total_value(Acc,[H|OtherRedboxList]) when H#redbox_info.rp_type == 2 ->
    get_total_value(Acc + 30,OtherRedboxList);
get_total_value(Acc,[H|OtherRedboxList]) when H#redbox_info.rp_type == 3 ->
    get_total_value(Acc + 50,OtherRedboxList);
get_total_value(Acc,[H|OtherRedboxList]) when H#redbox_info.rp_type == 4 ->
    get_total_value(Acc + 100,OtherRedboxList);
get_total_value(Acc,[H|OtherRedboxList]) when H#redbox_info.rp_type == 5 ->
    get_total_value(Acc + 200,OtherRedboxList);
get_total_value(Acc,[H|OtherRedboxList]) when H#redbox_info.rp_type == 6 ->
    get_total_value(Acc + 500,OtherRedboxList);
get_total_value(Acc,[H|OtherRedboxList]) when H#redbox_info.rp_type == 7 ->
    get_total_value(Acc + 1000,OtherRedboxList).

get_today_num(GolderInfo)->
    Today = erlang:date(),
    case GolderInfo#golder_info.redpacket_day of
        {NowNum,Today} ->
            NowNum;
        _ ->
            0
    end.

add_today_num(GolderInfo,AddNum)->
    Today = erlang:date(),
    case GolderInfo#golder_info.redpacket_day of
        {NowNum,Today} ->
            ?INFO("add_today_num1 ~w ~w+~w ~w",[GolderInfo,NowNum,AddNum,Today]),
            {NowNum+AddNum,Today};
        _ ->
            ?INFO("add_today_num2 ~w ~w ~w",[GolderInfo,AddNum,Today]),
            {AddNum,Today}
    end.

get_world()->
    case get(?pd_world_rank_list) of
        ?undefined -> % 理论上来说出来活动开启时刻，其他时间不会走此分之。活动执行期间，服务器开始时，就取得过列表了
            Version = case get(?pd_world_rank_version) of
                          ?undefined -> 0;
                          Version0 -> Version0
                      end,
            send_msg:direct_by_name(carlos_match, redpacketworld_server, {sync_world_rank, data_setting:get(server_id), Version, false}),
            [];
        WorldRank -> WorldRank
    end.

notice_world_rank([])->
    ignore;
notice_world_rank(NewPublisherList) when erlang:is_list(NewPublisherList)->
    Min = case get(?pd_world_rank_min) of
              ?undefined -> 0;
              Min0 -> Min0
          end,
    Max = lists:max([E#p_publisher_info.redpacket_num||E<-NewPublisherList]),
    if
        Max > Min ->
            send_msg:direct_by_name(carlos_match, redpacketworld_server, {notice_golder_info, data_setting:get(server_id), NewPublisherList});
        true ->
            ignore
    end;
notice_world_rank(NewPublisher) when erlang:is_record(NewPublisher, p_publisher_info)->
    notice_world_rank([NewPublisher]).

%% 获取跨服红包榜，的区域ID
get_server_group_id()->
    get_server_group_id(data_setting:get(server_id)).
get_server_group_id(ServerID)->
    get_server_group_id(ServerID,data_redpacket:get(server_group)).
get_server_group_id(ServerID,[])->
    ?ERR("get_server_group_id ~w",[ServerID]),
    0;
get_server_group_id(ServerID,[{Min,Max}|T])->
    if
       Min =< ServerID andalso ServerID =< Max ->
           Min*10000+Max;
       true ->
           get_server_group_id(ServerID,T)
    end.

%%-------------------------Fix-------------------------------------
fix_reward_20170508()->
    Res = db_sql:sql_execute_with_log("select temp.roleID as role_id ,(select roleName from gRole where gRole.roleID = temp.roleID) as rname ,sum(addNum) as addNum_sum ,max(time) as max_time from (select * from t_item_add_2017_5 where itemTypeID = 20052 and time >= '2017-4-29 0:0:0' and time <= '2017-5-5 23:59:59' union select * from t_item_add_2017_4 where itemTypeID = 20052 and time >= '2017-4-29 0:0:0' and time <= '2017-5-5 23:59:59') temp group by role_id order by addNum_sum desc, max_time limit 50;"),
    {ok,PayList0} = db_sql:sql_execute_with_log("select distinct roleID from gPay where time >= '2017-4-29 0:0:0' and time <= '2017-5-5 23:59:59';"),
    PayList = [E||[E]<-PayList0],
    case Res of
        {ok,List} ->
            AllRank = lists:foldl(fun([RID,Name,Num,_],AccList)->
                                Index = 1 + erlang:length(AccList),
                                [{RID,Index,Name,Num}|AccList] 
                        end, [], List),
            FixList = lists:foldl(fun({RID,Index,Name,Num},AccList)-> 
                                case lists:member(RID, PayList) of
                                    false -> [{RID,Index,Name,Num}|AccList];
                                    true -> AccList
                                end
                        end, [], AllRank),
            CfgRankReward = data_redpacket:get(rank_reward),
            lists:foreach(fun({RID,Index,Name,Num})->
                    case check_rank_reward_fix(Index,CfgRankReward) of
                        ?undefined->
                            ?ERR("check_rank_reward_fix error Index:~w",[Index]);
                        Reward ->
                            ?ERR("check_rank_reward_fix ok role:~w Index:~w  ,,, reward:~w",[RID,Index,Reward]),
                            mail_server:send_sys_mail(RID, ?MAIL_REDPACKET_RANK
                                                     ,[Name,Index,Num,Index], ""
                                                     ,Reward)
                    end           
                end, FixList),
            ?ERR("fix_reward_20170508 sql ok:~w --->> ~w  List:~w",[erlang:length(AllRank)
                                                          ,erlang:length(FixList),FixList]),
            ok;
        _ ->
            ?ERR("fix_reward_20170508 sql error:~w",[Res]),
            0
    end.

fix_reward_20170518()->
    Res = db_sql:sql_execute_with_log("select temp.roleID as role_id ,(select roleName from gRole where gRole.roleID = temp.roleID) as rname ,sum(addNum) as addNum_sum ,max(time) as max_time from (select * from t_item_add_2017_5 where itemTypeID = 20052 and time >= '2017-4-29 0:0:0' and time <= '2017-5-5 23:59:59' union select * from t_item_add_2017_5 where itemTypeID = 20052 and time >= '2017-4-29 0:0:0' and time <= '2017-5-5 23:59:59') temp group by role_id order by addNum_sum desc, max_time limit 50;"),
    {ok,PayList0} = db_sql:sql_execute_with_log("select distinct roleID from gPay where time >= '2017-4-29 0:0:0' and time <= '2017-5-5 23:59:59';"),
    PayList = [E||[E]<-PayList0],
    case Res of
        {ok,List} ->
            AllRank = lists:foldl(fun([RID,Name,Num,_],AccList)->
                                Index = 1 + erlang:length(AccList),
                                [{RID,Index,Name,Num}|AccList] 
                        end, [], List),
            FixList = lists:foldl(fun({RID,Index,Name,Num},AccList)-> 
                                case lists:member(RID, PayList) of
                                    false -> [{RID,Index,Name,Num}|AccList];
                                    true -> AccList
                                end
                        end, [], AllRank),
            CfgRankReward = data_redpacket:get(rank_reward),
            lists:foreach(fun({RID,Index,Name,Num})->
                    case check_rank_reward_fix(Index,CfgRankReward) of
                        ?undefined->
                            ?ERR("check_rank_reward_fix error Index:~w",[Index]);
                        Reward ->
                            ?ERR("check_rank_reward_fix ok role:~w Index:~w  ,,, reward:~w",[RID,Index,Reward]),
                            mail_server:send_sys_mail(RID, ?MAIL_REDPACKET_RANK
                                                     ,[Name,Index,Num,Index], ""
                                                     ,Reward)
                    end           
                end, FixList),
            ?ERR("fix_reward_20170508 sql ok:~w --->> ~w  List:~w",[erlang:length(AllRank)
                                                          ,erlang:length(FixList),FixList]),
            ok;
        _ ->
            ?ERR("fix_reward_20170508 sql error:~w",[Res]),
            0
    end.

check_rank_reward_fix(_,[])->
    ?undefined;
check_rank_reward_fix(Index,[C|CfgRankReward])->
    {{Min,Max},Reward} = C,
    if
        Max >= Index andalso Min =< Index ->
            Reward;
        true->
            check_rank_reward_fix(Index,CfgRankReward)
    end.

%%-------------------------For Test-------------------------------------

debug_del_role(RoleID)->
    erlang:send(?MODULE,{debug_del_role,RoleID}).
    
debug_redpacket_world_status()->
    erlang:send(?MODULE,debug_redpacket_world_status).

fix_redpacket_list()->
    erlang:send(?MODULE,fix_redpacket_list).

