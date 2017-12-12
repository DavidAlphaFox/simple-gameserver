-module(family_manager_server).

-behaviour(gen_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").
-include("def_mail.hrl").

-define(LOOP_INTERVAL, (1000 * 60 * 10)).
-define(REQUEST_JOIN_TIMEOUT_SECONDS, (3600 * 24 * 3)).
-define(one_day_second,3600*24).

%% API
-export([
         start/0,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([test_get_family_list/3]).

-record(state, {}).
 
-define(all_family_info_list, all_family_info_list).
-define(create_family_name_cache, create_family_name_cache).

-define(SYNC_SUMMARY_INTERVAL, (1000 * 60 * 5)). %% 向排名服提交数据的时间间隔
-define(SYNC_UPDATE_LIST, sync_update_list).     %% 需要更新的公会摘要列表
-define(SYNC_DEL_LIST, sync_del_list).        %% 需要删除的ID列表
-define(SYNC_RANK_LIST, sync_rank_list).         %% 当前全服排名列表
-define(CROSS_FAMILY_MEMBER_LIST, cross_family_member_list). %%跨服公会成员排名

%% 创建城邦信件

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	{ok, _} =
		supervisor:start_child(family_world_sup,
							   {family_sup,
								{family_sup, start_link, []},
								permanent, infinity, supervisor, [family_sup]}),
	{ok, _} =
		supervisor:start_child(family_world_sup,
							   {family_boss_sup,
								{family_boss_sup, start_link, []},
								permanent, infinity, supervisor, [family_boss_sup]}),
	{ok, _} =
		supervisor:start_child(family_world_sup,
							   {family_cross_fight_sup,
								{family_cross_fight_sup, start_link, []},
								permanent, infinity, supervisor, [family_cross_fight_sup]}),
	{ok, _} =
		supervisor:start_child(family_world_sup, 
							   {?MODULE,
								{?MODULE, start_link, []},
								permanent, 600000, worker, [?MODULE]}).

%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------

init([]) ->
	process_flag(trap_exit,true),
	ets:new(?ETS_FAMILY_SUMMARY, [named_table, set, public,{keypos,2}]),
	ets:new(?ETS_FAMILY_REQUEST, [named_table, bag, public,{keypos,2}]),
	ets:new(?ETS_FAMILY_PROTECT, [named_table, set, public,{keypos,1}]),
	ets:new(?ETS_FAMILY_TECHNOLOGY,[named_table, set, public,{keypos,1}]),
	ets:new(?ETS_FAMILY_INVITE, [named_table, set, public, {keypos, 1}]),
	erlang:put(?create_family_name_cache, []),
	case data_setting:get(server_type) of
		normal ->
			
			%% 初始化全服排名相关数据, 简化使用时的判断
			case db_sql:get_etc(?DB_ETC_KEY_CROSS_FAMILY_RANK) of
				[] ->
					erlang:put(?SYNC_UPDATE_LIST, []),
					erlang:put(?SYNC_DEL_LIST, []);
				%%erlang:put(?SYNC_RANK_LIST, []);
				%%为?undefined代表没有请求过,[]代表请求过,但为空
				Any ->
					lists:foreach(fun({K,V}) -> erlang:put(K,V) end, Any)
			end,
			
			InviteList = db_sql:get_etc(?DB_ETC_KEY_FAMILY_INVITE),
			case InviteList of
				[] ->
					ignore;
				_ ->
					?INFO("读出邀请列表~w",[InviteList]),
					ets:insert(?ETS_FAMILY_INVITE, InviteList)
			end,
			
			erlang:put(?CROSS_FAMILY_MEMBER_LIST, []),
			init_family_list(),
			init_request_list(),
			erlang:send(self(), loop),
			erlang:send_after(?SYNC_SUMMARY_INTERVAL, self(), sync_summary),
			{ok, #state{}};
		_ ->
			{ok,#state{}}
	end.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->  
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    ?INFO("handle_cast ~w",[_Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
handle_info({'EXIT', PID, Reason}, State) ->
    ?ERR("~ts: ~w, ~w", ["联盟管理进程收到exit消息", PID, Reason]),
    {noreply, State};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};

handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?INFO("----------do_persist1-------------"),
    do_persist(),
    ok.


%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sort_family_list(FamilyInfoList) ->
    FamilyInfoList2 = lists:sort(fun(A,B)->
                                        #family_info{family_id=FamilyIDA, level=LevelA, family_score=ScoreA, members=MembersA} = A,
                                        #family_info{family_id=FamilyIDB, level=LevelB, family_score=ScoreB, members=MembersB} = B,
                                        case ScoreA =:= ScoreB of
                                            false ->
                                                ScoreA > ScoreB;
                                            _ ->
                                                case LevelA =:= LevelB of
                                                    false ->
                                                        LevelA > LevelB;
                                                    _ ->
                                                        PowerA = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, MembersA),
                                                        PowerB = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, MembersB),
                                                        case PowerA =:= PowerB of
                                                            false ->
                                                                PowerA > PowerB;
                                                            _ ->
                                                                %% 按照创建时间确定顺序(和跨服排名保持一致)
                                                                FamilyIDA < FamilyIDB
                                                        end
                                                end
                                        end
                            end, FamilyInfoList),
    
    {FamilyInfoList3, _} = lists:foldr(fun(FamilyInfo, {AccFamilyInfoList, Rank}) ->
                                               {[FamilyInfo#family_info{rank=Rank}|AccFamilyInfoList], Rank - 1}
                                       end, {[], erlang:length(FamilyInfoList2)}, FamilyInfoList2),
    
    FamilySummaryList = lists:foldr(fun(FamilyInfo, AccFamilySummaryList) ->
                                                 FamilySummary = family_misc:to_p_family_summary(FamilyInfo),
                                                 [FamilySummary|AccFamilySummaryList]
                                         end, [], FamilyInfoList3),
    erlang:put(?all_family_info_list, FamilyInfoList3),
    ets:insert(?ETS_FAMILY_SUMMARY, FamilySummaryList),
    erlang:spawn(fun() -> update_rank_to_each_family(FamilyInfoList3) end).

init_family_list() ->
    FamilyInfoList = db_sql:get_all_family_info(),
    FamilySummaryList = lists:foldr(fun(#family_info{family_id=FamilyID}=FamilyInfo, AccFamilySummaryList) ->
                                            add_sync_update_id(FamilyID),
                                            FamilySummary = family_misc:to_p_family_summary(FamilyInfo),
                                            [FamilySummary|AccFamilySummaryList]
                                    end, [], FamilyInfoList),
    erlang:put(?all_family_info_list, FamilyInfoList),
    ets:insert(?ETS_FAMILY_SUMMARY, FamilySummaryList).

init_request_list() ->
    RequestList = db_sql:get_all_family_request(),
    ets:insert(?ETS_FAMILY_REQUEST, RequestList).

add_family(FamilyInfo) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    sort_family_list([FamilyInfo|FamilyInfoList]).

%% 获取联盟列表
do_handle_info({client_msg, RoleID, #cs_family_get_list{type=Type, start=Start, num=Num}}, State) ->
    %% 0 本服, 1 全服
    case Type of
        0 ->
            do_get_list(RoleID, Type, Start, Num);
        1 ->
            do_get_cross_list(RoleID, Type, Start, Num)
    end,
    {noreply, State};

%% 获得公会成员列表
do_handle_info({client_msg, RoleID, #cs_family_get_member_list{family_id=FamilyID}}, State) ->
    do_get_member_list(RoleID, FamilyID),
    {noreply, State};

do_handle_info({cs_family_search_by_family_name, RoleID,FamilyIDList}, State) ->
	do_get_search_list(RoleID, FamilyIDList),
    {noreply, State};

%% 添加申请加入联盟的请求，会广播至全员
do_handle_info({add_role_family_request, RoleInfo, FamilyID, OwnerRoleID, IsNeedReply}, State) ->
    #role{roleID=RoleID, roleName=RoleName, level=RoleLevel, fightPower=FightPower,head=Head,title=Title,isMale=IsMale} = RoleInfo,
    FamilyRequest = #family_request{role_id=RoleID,role_name=RoleName,level=RoleLevel,fight_power=FightPower,timestamp=util:now(),family_id=FamilyID,head=Head,title=Title,is_male=IsMale},
    OldFamilyRequestList = ets:match_object(?ETS_FAMILY_REQUEST, #family_request{family_id=FamilyID,_='_'}),
    case erlang:length(OldFamilyRequestList) >= data_family:get(family_request_max_num) of
        false ->
            next;
        true ->
            [TheFirst|_] = lists:keysort(#family_request.timestamp, OldFamilyRequestList),
            ?INFO("add_role_family_request max:~w length:~w will_delete:~w",
                  [data_family:get(family_request_max_num)
                  ,erlang:length(OldFamilyRequestList)
                  ,TheFirst]),
            db_sql:del_spec_family_request(TheFirst#family_request.role_id, TheFirst#family_request.family_id),            
            ets:delete(?ETS_FAMILY_REQUEST,TheFirst#family_request.role_id)
    end,
    db_sql:add_role_family_request(FamilyRequest),
    ets:insert(?ETS_FAMILY_REQUEST, FamilyRequest),
    Request = #sc_family_request_join{result=0, is_self=false, timestamp=0,family_id=FamilyID},
    case IsNeedReply of
        true ->
            ?unicast(RoleID, #sc_family_request_join{result=0, is_self=true, timestamp=0,family_id=FamilyID}),
            ?unicast(OwnerRoleID, Request);
        false ->
            ignore
    end,
	family_misc:router_to_family_process(FamilyID, {bc_join_family_request, Request}),
    {noreply, State};

do_handle_info({del_role_family_request, RoleID, FamilyID, FamilyRequest}, State) ->
    ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest),
    db_sql:del_spec_family_request(RoleID, FamilyID),
%%     ?ERR("RoleID:~w, FamilyID:~w", [RoleID, FamilyID]),
    ?unicast(RoleID, #sc_family_cancel_join{result=0, is_self=true,family_id=FamilyID}),
    family_misc:router_to_family_process(FamilyID, {send_msg_to_owner2, #sc_family_cancel_join{result=0, is_self=false,family_id=FamilyID}}),
    {noreply, State};

do_handle_info({refuse_join, RoleID, JoinRoleID, FamilyID, BeRefusedRoleIDList, FamilyRequestList}, State) ->
    lists:foreach(fun(FamilyRequest) -> ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest) end, FamilyRequestList),
    case JoinRoleID of
        0 ->
            db_sql:del_family_request(FamilyID);
        _ ->
            db_sql:del_spec_family_request(JoinRoleID, FamilyID)
    end,
    ?unicast(RoleID, #sc_family_refuse_join{result=0, is_self=true}),
    lists:foreach(fun(BeRefusedRoleID) ->
                          ?unicast(BeRefusedRoleID, #sc_family_refuse_join{result=0, is_self=false})
                  end, BeRefusedRoleIDList),
    {noreply, State};

do_handle_info({role_join_family, FamilyID, JoinRoleID}, State) ->
    del_role_request(JoinRoleID),
    update_role_family_id(JoinRoleID, FamilyID),
    {noreply, State};

do_handle_info({kick, KickRoleID}, State) ->
    update_role_family_id(KickRoleID, 0),
    {noreply, State};

do_handle_info({update_family_info, NewFamilyInfo, NewFamilySummary}, State) ->
    update_family_info(NewFamilyInfo, NewFamilySummary),
    {noreply, State};

do_handle_info(update_family_name_cache, State) ->
    NameList = db_sql:get_family_name_list(), 
    NewCacheList = lists:foldl(fun(E, Acc) ->
                                    case E of
                                        [FamilyName, FamilyID] ->
                                            [{FamilyName, FamilyID}|Acc];
                                        _ ->
                                            ?ERR("更新名字缓存异常~n.",[])
                                    end
                            end, [], NameList),
    put(?create_family_name_cache, NewCacheList),
    {noreply, State};

do_handle_info({leave, RoleID}, State) ->
    update_role_family_id(RoleID, 0),
    {noreply, State};

do_handle_info({disband, FamilyID, RoleID}, State) ->
    update_role_family_id(RoleID, 0),
    del_family_info(FamilyID),
    del_create_family_name(FamilyID),
    add_sync_del_id(FamilyID),
    {noreply, State};

%% 创建联盟
do_handle_info({do_create, RoleInfo, CostType, CostNum, FamilyName}, State) ->
    do_create(RoleInfo, CostType, CostNum, FamilyName),
    {noreply, State};

%%公会改名
do_handle_info({update_family_name, FamilyID, NewName}, State) ->
    case get(?all_family_info_list) of
        List when is_list(List) ->
            case lists:keytake(FamilyID, #family_info.family_id, List) of
                false ->
                    ignore;
                {value, Info, _OtherList} ->
                    NewList = lists:keyreplace(FamilyID, #family_info.family_id, List, Info#family_info{family_name=NewName}),
                    put(?all_family_info_list, NewList)
            end;
        _ ->
            ignore
    end,
    case get(?create_family_name_cache) of
        CacheList when is_list(CacheList) ->
            case lists:keytake(FamilyID, 2,  CacheList) of
                false ->
                    ignore;
                {value, _Info, OtherList} ->
                    NewCacheList = [{NewName, FamilyID}|OtherList],
                    erlang:put(?create_family_name_cache, NewCacheList)
            end;
        _ ->
            ignore
    end,
    {noreply, State};

%%角色改名(未加入公会的人)
do_handle_info({update_role_name, RoleID, Name}, State) ->
    %% 先修改内存数据 
    case ets:lookup(?ETS_FAMILY_REQUEST, RoleID) of
        List when is_list(List) ->
            case lists:keytake(RoleID, #family_request.role_id, List) of
                false ->
                    next;
                {value, Info, _} ->
                    NewInfo = Info#family_request{role_name=Name},
                    ets:delete_object(?ETS_FAMILY_REQUEST, Info),
                    ets:insert(?ETS_FAMILY_REQUEST, NewInfo),
                    
                    %% 数据库改名
                    db_sql:update_family_request_role_name(RoleID, Name)
            end;
        _ ->
            next
    end,
    {noreply, State};

%%公会会员改名(公会会长或则创始人)
do_handle_info({update_family_role_name,FamilyID,RoleID,Name}, State) ->
    case get(?all_family_info_list) of
        List when is_list(List) ->
            case lists:keytake(FamilyID, #family_info.family_id, List) of
                false ->
                    ignore;
                {value, #family_info{create_role_id=CreateRoleID,owner_role_id=OwnerRoleID} = Info, _} ->
                    Info1 = case CreateRoleID =:= RoleID of
                                true ->
                                    Info#family_info{create_role_name=Name};
                                _ ->
                                    Info
                            end,
                    Info2 = case OwnerRoleID =:= RoleID of
                                true ->
                                    Info1#family_info{owner_role_name=Name};
                                _ ->
                                    Info1 
                            end,
                    case Info2 == Info of
                        true ->
                            ignore;
                        _ ->
                            NewList = lists:keyreplace(FamilyID, #family_info.family_id, List, Info2),
                            put(?all_family_info_list, NewList)
                    end
            end;
        _ ->
            ignore
    end,
    {noreply, State};

do_handle_info(loop, State) ->
    erlang:send_after(?LOOP_INTERVAL, self(), loop),
    loop(),
    {noreply, State};

do_handle_info(sync_summary, State) ->
	erlang:send_after(?SYNC_SUMMARY_INTERVAL, self(), sync_summary),
    do_sync_summary(),
    {noreply,State};

do_handle_info({afrs_rank_list, RoleID, Start, Num, RankList}, State) ->
    erlang:put(?SYNC_RANK_LIST, RankList),
    do_get_cross_list(RoleID, 1, Start, Num),
    {noreply, State};

do_handle_info({afrs_update_firstN, RankList}, State) ->
    erlang:put(?SYNC_RANK_LIST, RankList),
    {noreply, State};

do_handle_info({afrs_update_rank, RankList}, State) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    NewFamilyInfoList =  
        lists:foldl(fun({FamilyID, Rank}, Acc) ->
                        case lists:keyfind(FamilyID, #family_info.family_id, Acc) of
                            false ->
                                Acc;
                            Info ->
                                NewFamilyInfo = Info#family_info{cross_rank=Rank},
                                NewFamilySummary = family_misc:to_p_family_summary(NewFamilyInfo),
                                db_sql:update_family_cross_rank(FamilyID, Rank),
                                ets:insert(?ETS_FAMILY_SUMMARY, NewFamilySummary),
                                family_misc:router_to_family_process(FamilyID, {update_cross_rank, Rank}),
                                lists:keyreplace(FamilyID, #family_info.family_id, Acc, NewFamilyInfo)
                        end 
                    end, FamilyInfoList, RankList),
    erlang:put(?all_family_info_list, NewFamilyInfoList),
    {noreply, State};

do_handle_info({afrs_get_cross_member_list, ServerID, RoleID, ViewFamilyID}, State) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    case lists:keyfind(ViewFamilyID, #family_info.family_id, FamilyInfoList) of
        false ->
            send_msg:direct_by_name(family_fight_server:get_master_server_name(), all_family_rank_server, {get_cross_member_list_return, ServerID, RoleID, ViewFamilyID, []});
        _ ->
            family_misc:router_to_family_process(ViewFamilyID, {get_cross_member_list, ServerID, RoleID, ViewFamilyID})
    end,
    {noreply, State};

do_handle_info({get_cross_member_list_return, RoleID, ViewFamilyID, List}, State) ->
    CacheList = erlang:get(?CROSS_FAMILY_MEMBER_LIST),
    case lists:keyfind(ViewFamilyID, 1, CacheList) of
        false ->
            erlang:put(?CROSS_FAMILY_MEMBER_LIST, [{ViewFamilyID, List}|CacheList]);
        _ ->
            ignore
    end,
    ?unicast(RoleID, #sc_family_get_member_list{family_id=ViewFamilyID, members=List}),  
    {noreply, State};

do_handle_info(Info, State) ->
    ?ERR("~ts:~w", ["未知的消息", Info]),
    {noreply, State}.

%% 定周期检查是否有超时的公会请求
loop() ->
    Now = util:now(),
    lists:foreach(fun(#family_request{role_id=RoleID,family_id=FamilyID,timestamp=Timestamp}=FamilyRequest) ->
                          case Timestamp + ?REQUEST_JOIN_TIMEOUT_SECONDS < Now of
                              true ->
                                  ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest),
                                  db_sql:del_spec_family_request(RoleID, FamilyID);
                              false ->
                                  next
                          end
                  end, ets:match_object(?ETS_FAMILY_REQUEST, #family_request{_='_'})),    
    lists:foreach(fun(#family_info{family_id=FamilyID, cur_members = MemberNum,owner_role_id = OwnerRoleId}=FamilyInfo) ->
                        %% 检查是否有公会复合解散条件
                        case MemberNum =< 1 of
                            true ->
                                #rolePublic{roleID=RoleID,lastLogoutTime = LastLogoutTime} = role_lib:get_rolePublic(OwnerRoleId),
                                IsOnline = role_lib:is_online(OwnerRoleId),
                                case IsOnline =:= false andalso LastLogoutTime /= 0 andalso (LastLogoutTime + ?one_day_second*data_family:get(disband_owner_offline_day)) =< Now of
                                    true ->
                                        ?DEBUG("公会:~p只有会长:~p一人,且会长超过一定时间没有上线,公会将被解散.~n", [FamilyInfo,RoleID]),
                                        family_server:do_disband(FamilyInfo,RoleID),
                                        add_sync_del_id(FamilyID),
                                        mail_server:send_sys_mail(FamilyInfo#family_info.create_role_id, ?MAIL_FAMILY_BE_AUTO_DISBAND, [], "", []);
                                    _ ->
                                        next
                                end;
                            _ ->
                                next
                        end
            end, erlang:get(?all_family_info_list)),
    sort_family_list(erlang:get(?all_family_info_list)),
    do_persist( ).

%%创建联盟
do_create(RoleInfo, CostType, CostNum, FamilyName) ->
    case catch do_create2(RoleInfo, FamilyName) of
        {ok, #family_info{family_id=FamilyID}=FamilyInfo} ->
            RoleID = RoleInfo#role.roleID,
            role_lib:send_server(RoleID, {create_family_succ, FamilyInfo, CostType, CostNum}),
            add_family(FamilyInfo),
            del_role_request(RoleID),
            add_sync_update_id(FamilyID);
        ErrorInfo ->
            ?ERR("创建失败:ErrorInfo=~w", [ErrorInfo]),
            role_lib:send_server(RoleInfo#role.roleID, {create_family_fail,  CostType, CostNum})
    end.

do_create2(RoleInfo, FamilyName) ->
    FamilyID = tk_id:gen_familyID(),
    add_create_family_name(FamilyName, FamilyID),
    #role{roleID=RoleID, roleName=RoleName, title=Title, isMale=IsMale, level=RoleLevel, fightPower=FightPower, head=Head, lastLogoutTime=OfflineTime} = RoleInfo,
    NewMember =
		#family_member_info{
							role_id=RoleID
							,role_name=RoleName
							,family_id=FamilyID
							,family_contribution=0
							,left_family_contribution=0
							,use_gold_time=0
							,title=Title
							,is_male=IsMale
							,online=true
							,role_level=RoleLevel
							,fight_power=FightPower
							,family_title=?FAMILY_TITLE_OWNER
							,is_join_war=0
							,attack_times=0
							,defend_times=016
							,win_star=0
							,join_time=util:now()
							,weeklyContributes=0
							,lastContributeDate={1970,1,1}
						    ,recvEnergyList=[]
							,storageReqData={[],{{1970,1,1},0}}
                            ,head=Head
                            ,offline_time = OfflineTime
						    ,limit_shop=family_server:init_family_limit_shop()
                            ,anubisinfo=[]
						   },
    Members = [NewMember],

	FamilyInfo =
		#family_info{
					 family_id=FamilyID
					 ,family_name=FamilyName
					 ,level=1
					 ,create_role_id=RoleID
					 ,create_role_name=RoleName
					 ,owner_role_id=RoleID
					 ,owner_role_name=RoleName
					 ,cur_members=1
					 ,family_score=data_family_fight:get(initScore)
					 ,active_points=0
					 ,notice= data_family:get(default_notice)
					 ,slogan= data_family:get(default_slogan)
					 ,members=Members
					 ,rank= erlang:length(erlang:get(?all_family_info_list)) + 1
					 ,world_rank=0
					 ,is_zero_refreshed=erlang:date()
					 ,family_fight_other_info=#family_fight_other_info{}
					 ,create_time=util:now()
					 ,talkRoomID="0"
                     ,family_task=role_task:add_all_family_today_task()
                     ,family_instance_state=family_data:init_instance_boss(Members)
                     ,family_anubisinfo=#anubis_family_info{}}, %% 此处add_all_family_today_task返回值是r_task类型的list
    {ok, _Pid} = supervisor:start_child(family_sup,
                                        {family_misc:make_family_process_name(FamilyID) , 
                                         {family_server, start_link, [FamilyID, FamilyInfo]},
                                         transient, 300000, worker, [family_server]}),
	spawn(fun()-> db_sql:set_family_info(FamilyInfo) end),
    {ok, FamilyInfo}.

do_get_list(RoleID, Type, Start, Num) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    Length = erlang:length(FamilyInfoList),

    case Start + 1 > Length of
        false ->
            FamilyInfoList2 = lists:sublist(FamilyInfoList, Start + 1, Num),
            FamilySummaryList = lists:map(fun(FamilyInfo) -> family_misc:to_p_family_summary(FamilyInfo) end, FamilyInfoList2),
            FamilySummaryList2 = update_family_summary_for_role_request(FamilySummaryList, RoleID),
%%             ?ERR("FamilySummaryList2:~w", [FamilySummaryList2]),
            ?unicast(RoleID, #sc_family_get_list{type=Type, result=0, family_list=FamilySummaryList2});
        true ->
            case Length =:= 0 of
                true ->
                    ?unicast(RoleID, #sc_family_get_list{type=Type, result=0, family_list=[]});
                false ->
                    ?unicast(RoleID, #sc_family_get_list{type=Type, result=1, family_list=[]})
            end
    end.

do_get_cross_list(RoleID, Type, Start, Num) ->
    RankList = erlang:get(?SYNC_RANK_LIST),
    case RankList of
        %% 没有值 则请求一次
        ?undefined ->
            send_msg:direct_by_name(family_fight_server:get_master_server_name(), all_family_rank_server, {get_rank_list, data_setting:get(server_id), RoleID, Start, Num});
        List ->
            Length= erlang:length(List),
            case Start + 1 > Length of
                false ->
                    SummaryList = lists:sublist(List, Start + 1, Num),
                    ?unicast(RoleID, #sc_family_get_list{type=Type, result=0, family_list=SummaryList});
                _ ->
                    case Length =:= 0 of
                        true ->
                            ?unicast(RoleID, #sc_family_get_list{type=Type, result=0, family_list=[]});
                        _ ->
                            ?unicast(RoleID, #sc_family_get_list{type=Type, result=1, family_list=[]})
                    end
            end
    end.

do_get_member_list(RoleID, FamilyID) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    case lists:keyfind(FamilyID, #family_info.family_id, FamilyInfoList) of
        false ->
            %% 先查看缓存
            CacheList = erlang:get(?CROSS_FAMILY_MEMBER_LIST),
            case proplists:get_value(FamilyID, CacheList) of
                ?undefined ->
                    send_msg:direct_by_name(family_fight_server:get_master_server_name(), all_family_rank_server, {get_cross_member_list, data_setting:get(server_id), RoleID, FamilyID});
                List ->
                    ?unicast(RoleID, #sc_family_get_member_list{family_id=FamilyID, members=List})
            end;
        _ ->
            %%?unicast(RoleID, #sc_family_get_member_list{family_id=FamilyID, members=[]});
            family_misc:router_to_family_process(FamilyID, {get_member_list, RoleID})
    end.

do_get_search_list(RoleID, FamilyIDList)->
	FamilyInfoList = erlang:get(?all_family_info_list),
	FamilyInfoList2 = lists:foldl(fun(FamilyID,Acc) ->
										  case lists:keyfind(FamilyID, #family_info.family_id, FamilyInfoList) of
											  false ->
												  Acc;
											  X ->
												  [X|Acc]
										  end
										  end,[],FamilyIDList),
    
%%     SortFamilyInfoList2 = lists:sort(fun(A,B)->
%%                                         if
%%                                             A#family_info.level > B#family_info.level ->
%%                                                 true;
%%                                             A#family_info.level < B#family_info.level ->
%%                                                 false;
%%                                             true ->
%%                                                 APower = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, A#family_info.members),
%%                                                 BPower = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, B#family_info.members),
%%                                                 if
%%                                                     APower >= BPower ->
%%                                                         true;
%%                                                     true ->
%%                                                         false
%%                                                 end
%%                                         end
%%                             end, FamilyInfoList2),
    
	FamilySummaryList = lists:map(fun(FamilyInfo) -> family_misc:to_p_family_summary(FamilyInfo) end, FamilyInfoList2),
	FamilySummaryList2 = update_family_summary_for_role_request(FamilySummaryList, RoleID),
	?unicast(RoleID, #sc_family_search_by_family_name{result=1,infoList=FamilySummaryList2}).

update_family_summary_for_role_request(FamilySummaryList, RoleID) ->
    RoleRequestList = ets:lookup(?ETS_FAMILY_REQUEST, RoleID),
%%     ?ERR("RoleRequestList:~w, RoleID:~w", [RoleRequestList, RoleID]),
    lists:map(fun(#p_family_summary{family_id=FamilyID}=FamilySummary) ->
                      case lists:keyfind(FamilyID, #family_request.family_id, RoleRequestList) of
                          false ->
                              FamilySummary#p_family_summary{is_request=false};
                          _ ->
                              FamilySummary#p_family_summary{is_request=true}
                      end
              end, FamilySummaryList).

del_family_info(FamilyID) ->
    ets:delete(?ETS_FAMILY_SUMMARY, FamilyID),
    sort_family_list(lists:keydelete(FamilyID, #family_info.family_id, erlang:get(?all_family_info_list))).

update_family_info(#family_info{family_id=FamilyID}=NewFamilyInfo, NewFamilySummary) ->
    FamilyInfoList = erlang:get(?all_family_info_list),
    NewFamilyInfoList =
        case lists:keyfind(FamilyID, #family_info.family_id, FamilyInfoList) of
            false ->
                FamilyInfoList ++ [NewFamilyInfo];
            _ ->
                lists:keyreplace(FamilyID, #family_info.family_id, FamilyInfoList, NewFamilyInfo)
        end,
    erlang:put(?all_family_info_list, NewFamilyInfoList),
    ets:insert(?ETS_FAMILY_SUMMARY, NewFamilySummary),
    add_sync_update_id(FamilyID).

update_role_family_id(JoinRoleID, FamilyID) ->
    case role_lib:is_online(JoinRoleID) of
        true ->
            role_lib:send_server(JoinRoleID, {update_family_id, FamilyID});
        false ->
            case FamilyID of
                0 ->
                    db_sql:reduce_role_unioncoin(JoinRoleID, data_family:get(reduce_unioncoin_rate)),
                    db_sql:update_role_family_id(JoinRoleID, FamilyID, util:now());
                _ ->
                    db_sql:update_role_family_id(JoinRoleID, FamilyID)
            end,
            %%role_lib:update_rolePublic(Role, TrSpecial)
            role_lib:update_rolePublic(db_sql:get_roleInfo(JoinRoleID),db_sql:get_trSpecial_info(JoinRoleID)
                                       ,db_sql:get_carlos_plane_info(JoinRoleID),db_sql:get_xbattle_data(JoinRoleID)),
            family_misc:unlock_family_protect(JoinRoleID)
    end.

update_rank_to_each_family(FamilyInfoList) ->
    lists:foreach(fun(#family_info{rank=Rank, family_id=FamilyID}) ->
                          FamilyPName = family_misc:make_family_process_name(FamilyID),
                          case erlang:whereis(FamilyPName) of
                              ?undefined ->
                                  db_sql:update_family_rank(FamilyID, Rank);
                              _ ->
                                  erlang:send(FamilyPName, {update_family_rank, Rank})
                          end
                  end, FamilyInfoList).

del_role_request(RoleID) ->
    lists:foreach(
      fun(#family_request{family_id=FamilyID} = FamilyRequest) ->
              ets:delete_object(?ETS_FAMILY_REQUEST, FamilyRequest),
              db_sql:del_spec_family_request(RoleID, FamilyID),
              FamilyPName = family_misc:make_family_process_name(FamilyID),
              case erlang:whereis(FamilyPName) of
                  ?undefined ->
                      next;
                  _ ->
                      erlang:send(FamilyPName, {send_msg_to_owner, #sc_family_del_request{role_id=RoleID}})
              end
      end, ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=RoleID,_='_'})).

add_create_family_name(FamilyName, FamilyID) ->
    List = erlang:get(?create_family_name_cache),
    case lists:keyfind(FamilyName, 1, List) of
        false ->
            erlang:put(?create_family_name_cache, [{FamilyName, FamilyID}|List]);
        _ ->
            erlang:throw(family_name_used)
    end.

del_create_family_name(FamilyID) ->
    List = erlang:get(?create_family_name_cache),
    List2 = lists:keydelete(FamilyID, 2, List),
    erlang:put(?create_family_name_cache, List2).

add_sync_update_id(FamilyID) ->
    UpdateList = erlang:get(?SYNC_UPDATE_LIST),
    case lists:member(FamilyID, UpdateList) of
        true ->
            ignore;
        _ ->
            erlang:put(?SYNC_UPDATE_LIST, [FamilyID|UpdateList])
    end.

add_sync_del_id(FamilyID) ->
    DelList = erlang:get(?SYNC_DEL_LIST),
    case lists:member(FamilyID, DelList) of
        true ->
            ignore;
        _ ->
            erlang:put(?SYNC_DEL_LIST, [FamilyID|DelList])
    end.

do_sync_summary() ->
    UpdateList = erlang:get(?SYNC_UPDATE_LIST),
    case UpdateList =/= [] of
        false ->
            ignore;
        _ ->
            ServerID = data_setting:get(server_id), 
            SummaryList = lists:foldl(fun(E,Acc) -> 
                                        case ets:lookup(?ETS_FAMILY_SUMMARY, E) of
                                            [] ->
                                                %% 当公会数据有更新之后,公会解散了,就会出现找不到摘要的情况.
                                                %% 在这里处理可以避免每次删除时都去更新列表里面查找 
                                                Acc;
                                            [Info] ->
                                                [Info|Acc]
                                        end
                                      end, [], UpdateList),
            send_msg:direct_by_name(family_fight_server:get_master_server_name(), all_family_rank_server, {submit_summary_info, ServerID, SummaryList}),
            erlang:put(?SYNC_UPDATE_LIST, [])
    end,

    DelList = erlang:get(?SYNC_DEL_LIST),
    case DelList =/= [] of
        false ->
            ignore;
        _ ->
            send_msg:direct_by_name(family_fight_server:get_master_server_name(), all_family_rank_server, {delete_family_info, DelList}),
            erlang:put(?SYNC_DEL_LIST, [])
    end,
    erlang:put(?CROSS_FAMILY_MEMBER_LIST, []).

do_persist( ) ->
    ?INFO("----------do_persist2-------------"),
    PersistList = [E || E <- erlang:get(), is_persist(E)],
    db_sql:set_etc(?DB_ETC_KEY_CROSS_FAMILY_RANK, PersistList),
    ?INFO("保存邀请列表do_persist:~w",[ets:tab2list(?ETS_FAMILY_INVITE)]),
    db_sql:set_etc(?DB_ETC_KEY_FAMILY_INVITE, ets:tab2list(?ETS_FAMILY_INVITE)).

is_persist({?SYNC_RANK_LIST,_}) -> true;
is_persist({?SYNC_UPDATE_LIST,_}) -> true;
is_persist({?SYNC_DEL_LIST,_}) -> true;
is_persist(_) -> false.

%%使用ets_family_summary中保存的公会信息，向所有在线公会广播消息
broadcast_to_all_family(Msg)->
    FamilySummaryList = ets:tab2list(?ETS_FAMILY_SUMMARY),
    lists:foreach(fun(#p_family_summary{family_id=FamilyID})->
        ProcessName = family_misc:make_family_process_name(FamilyID),
        case erlang:whereis(ProcessName) of
            ?undefined->
                ignore;
            Pid ->
                erlang:send(Pid, Msg)
        end
    end,FamilySummaryList).

%%----------test以下方法仅用于调试----------------

test_get_family_list(RoleID,Start,Num)->
	erlang:send(?MODULE,{client_msg, RoleID, #cs_family_get_list{start=Start, num=Num}}).

update_family_name_cache() ->
    erlang:send(?MODULE, update_family_name_cache).

clean_invite()->
    ets:delete_all_objects(?ETS_FAMILY_INVITE).