-module(family_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").
-include("def_mail.hrl").
-include("def_task.hrl").
-include("def_carlos.hrl").
-include("def_homestead.hrl").  

-define(MAMILY_LOG_TYPE_JOIN,           1001).% 加入公会   
-define(MAMILY_LOG_TYPE_JOBUP,          1002).% 职位提升
-define(MAMILY_LOG_TYPE_JOBDOWN,        1003).% 职位下降
-define(MAMILY_LOG_TYPE_CHANGE_LEADER,  1004).% 转让馆长
-define(MAMILY_LOG_TYPE_KICK,           1005).% 踢出会员
-define(MAMILY_LOG_TYPE_LEAVE,          1006).% 离开公会
-define(MAMILY_LOG_TYPE_SIGN_UP,        1007).% 报名盟战
-define(MAMILY_LOG_TYPE_CONTRIBUTE_10,  1008).% 当天累积获得10点活跃度
-define(MAMILY_LOG_TYPE_CONTRIBUTE_50,  1009).% 当天累积获得50点活跃度
-define(MAMILY_LOG_TYPE_CONTRIBUTE_100, 1010).% 当天累积获得100点活跃度
-define(MAMILY_LOG_TYPE_LEVEL_UP,       1011).% 公会升级
-define(MAMILY_LOG_TYPE_JOIN_BE_INVITE, 1012).% 接受邀请加入公会
-define(MAMILY_LOG_TYPE_CHANGE_ANUBIS, 	1013).% anubis修改驾驶员 

-define(DUMP_INTERVAL, (1000 * 60 * 70)). %改为70分钟，避免与其他定周期处理重叠
-define(POWER_UPDATE_INTERVAL, (1000 * 295)). %295s更新一次成员战斗力
-define(online_role_id_list, online_role_id_list).
-define(cache_fighter_list_self, cache_fighter_list_self). %缓存工会战战斗结果成员列表，自己
-define(cache_fighter_list_enemy, cache_fighter_list_enemy). %缓存工会战战斗结果成员列表，敌人
-define(CONTRIBUTION_TOP_N,contribution_top_n).  %缓存公会历史活跃度前N成员id列表
-define(RECENT_TALK_DATA_NUM, 10).
-define(match_info_valid_interval, (60 * 5)).
-define(wait_info_interval, 30).
-define(tick_interval,60*1000).
-define(family_fight_reward,100).
-define(family_fight_reward_person,1000).
-define(change_pos(Ger, NewPos), (Ger#ger{gerBase=((Ger#ger.gerBase)#gerBase{gerPos=NewPos})})).  

-define(max_get_star,9).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start_link/1,
         start_link/2,
		boss_request_family_info/1
        ]).

start_link(FamilyID) ->
    gen_server:start_link(?MODULE, [FamilyID], []).

start_link(FamilyID, FamilyInfo) ->
    gen_server:start_link(?MODULE, [FamilyID, FamilyInfo], []).

boss_request_family_info(FamilyID)->
	gen_server:call(family_misc:make_family_process_name(FamilyID), boss_request_family_info).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {family_info,family_match,family_fight_hist,startTime=0,prepareEndTime=0,randomEndTime=0,fightEndTime=0,periodEndTime=0}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([FamilyID, FamilyInfo]) ->
    ?INFO("~ts:~w", ["初始化联盟进程", FamilyID]),
    erlang:process_flag(trap_exit, true),
	erlang:register(family_misc:make_family_process_name(FamilyID), self()),
	%db_sql:set_family_info(FamilyInfo),
	FamilyInfo3 = 
		case db_sql:get_family_info(FamilyID) of
			?undefined ->
				FamilyInfo;
			FamilyInfo2 ->
				FamilyInfo2
		end,
	init2(FamilyInfo3);

init([FamilyID]) ->
	random:seed(util:gen_random_seed()), 
    ?INFO("~ts:~w", ["初始化联盟进程1", FamilyID]),
    erlang:process_flag(trap_exit, true),
    erlang:register(family_misc:make_family_process_name(FamilyID), self()),
    FamilyInfo = db_sql:get_family_info(FamilyID),
    case FamilyInfo of %公会已经被删除
        undefined ->
            ?ERR("公会已经不存在于数据库 可能解散了~w",[FamilyID]),
            {stop, normal};
        _ ->
        	erlang:send_after(1000 * ?wait_info_interval ,family_fight_server,{family_server_get_family_fight_info, FamilyID}),
            init2(FamilyInfo)
    end.

init2(FamilyInfo) ->
    #family_info{members=Members,family_id=FamilyID} = FamilyInfo,
    NewMembers = init_online(Members),
    init_online_role_id_list(NewMembers),
    State = #state{family_info=FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers)},family_match=#family_match{},family_fight_hist=[]},
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), loop),
    erlang:send_after(?POWER_UPDATE_INTERVAL, erlang:self(), member_power_fight),
	check_and_do_zero_clock(),
	family_data:init_family_extra_data(FamilyID),
	erlang:send_after(1000 * ?wait_info_interval, self(), start_boss),
	erlang:send(self(), update_family_talk_data),
    %%触发更新公会的阿努比斯所有存在赛季的排名(此处更新排行赛排行榜，主要是防止排行榜的某次更新，公会进程因为崩溃而没有接收到，导致排行出现问题)
    erlang:send(self(),update_family_anubis_rank),
    {ok, State}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
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
%% ====================================================================

handle_call(boss_request_family_info, _, #state{family_info=FamilyInfo}=State)->
	#family_info{members=Members, level=Level}=FamilyInfo,
	MembersID = lists:foldl(fun(#family_member_info{role_id=RoleID},Acc)->[RoleID|Acc] end,[], Members),
	{reply, {Level, MembersID},State};
handle_call(get_state,_,State)->
    Reply=State,
    {reply,Reply,State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({'EXIT', PID, Reason}, State) ->
    ?ERR("~ts: ~w, ~w", ["联盟进程收到exit消息", PID, Reason]),
    {noreply, State};
handle_info(Info, State) ->
	%?DEBUG("INFO:~w,~n,~w",[Info,State]),
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
		{noreply,NewState,hibernate} ->
			{noreply,NewState,hibernate};
        {stop, normal, NewState} ->
            {stop, normal, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(Reason, State) ->
    case erlang:is_record(State#state.family_info, family_info) of
        true ->
			FamilyInfo = State#state.family_info,
			family_data:dump_family_extra_data(FamilyInfo#family_info.family_id),
            db_sql:update_family_info(FamilyInfo);
        false ->
            next
    end,
    case Reason of
        shutdown ->
            ?DEBUG("Reason:~w, State:~w", [Reason, State]);
        _ ->
            ?ERR("Reason:~w, State:~w", [Reason, State])
    end,
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% hook_zero_clock()->
%% 	NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS ,
%% 	timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
%% 	erlang:send(self(), do_zero_clock_setting).

check_and_do_zero_clock()->
	NextTick = ?tick_interval - (util:now() rem ?tick_interval),
	erlang:send_after(NextTick, self(), check_and_do_zero_clock).

init_online(Members) ->
    lists:map(fun(#family_member_info{role_id=RoleID}=Member) ->
                case role_lib:get_rolePublic(RoleID) of
                    [] ->
                        ?ERR("公会成员:~p的数据无法找到.~n", [RoleID]),
                        Member;
                    #rolePublic{lastLogoutTime=LastLogoutTime,viplevel=VipLevel,svipLevel=SVipLevel} ->
                        Vip = role_lib:cacl_vip_info(VipLevel,SVipLevel),
                        Member#family_member_info{online=role_lib:is_online(RoleID), offline_time = LastLogoutTime,vip=Vip}
                end
            end, Members).

init_online_role_id_list(Members) ->
    RoleIDList =
        lists:foldr(fun(#family_member_info{role_id=RoleID, online=IsOnline}, Acc) ->
                            case IsOnline of
                                true ->
                                    [RoleID|Acc];
                                false ->
                                    Acc
                            end     
                    end, [], Members),
    erlang:put(?online_role_id_list, RoleIDList).

update_online(RoleID, IsOnline) when erlang:is_boolean(IsOnline) ->
    OnlineRoleIDList = erlang:get(?online_role_id_list),
    case IsOnline of
        true ->
            erlang:put(?online_role_id_list, [RoleID|lists:delete(RoleID, OnlineRoleIDList)]);
        false ->
            erlang:put(?online_role_id_list, lists:delete(RoleID, OnlineRoleIDList))
    end.


update_online(RoleID, IsOnline, Members) when erlang:is_boolean(IsOnline) ->
    OnlineRoleIDList = erlang:get(?online_role_id_list),
    case IsOnline of
        true ->
            erlang:put(?online_role_id_list, [RoleID|lists:delete(RoleID, OnlineRoleIDList)]);
        false ->
            erlang:put(?online_role_id_list, lists:delete(RoleID, OnlineRoleIDList))
    end,
    case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        {value, Member, Members2} ->
            #rolePublic{lastLogoutTime=LastLogoutTime} = role_lib:get_rolePublic(RoleID),
            [Member#family_member_info{online=IsOnline, offline_time = LastLogoutTime}|Members2];
        false ->
            Members
    end.

do_handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
do_handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};


%% do_handle_info(create_talk_room, #state{family_info=FamilyInfo}=State) ->
%% 	RoomID = 
%% 		case get_room_id() of
%% 			"0" ->
%% 				erlang:send(self(),create_talk_room),
%% 				"0";
%% 			ID when is_list(ID)->
%% 				db_sql:save_family_roomID(FamilyInfo#family_info.family_id,ID),
%% 				ID
%% 		end,
%% 	{noreply,State#state{family_info=FamilyInfo#family_info{talkRoomID=RoomID}}};

do_handle_info(update_family_talk_data, #state{family_info=FamilyInfo}=State) ->
	#family_info{talk_data=TalkData} = FamilyInfo,
	NewTalkData = 
		lists:foldl(fun({sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1},Acc) ->
                            #rolePublic{level = Level} = role_lib:get_rolePublic(RoleID),
                            Grade = doublematch_server:dirty_get_role_dm_grade(RoleID),
						[{sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,1,Grade,Level}|Acc];
                       ({sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip},Acc) ->
                            #rolePublic{level = Level} = role_lib:get_rolePublic(RoleID),
                            Grade = doublematch_server:dirty_get_role_dm_grade(RoleID),
                        [{sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip,Grade,Level}|Acc];
				   (Msg,Acc) ->
						[Msg|Acc]
				end,[],TalkData),
	FamilyInfo2 = FamilyInfo#family_info{talk_data=NewTalkData},
	{noreply,State#state{family_info=FamilyInfo2}};


do_handle_info({update_role_online, RoleID, IsOnline}, #state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	NewMembers = update_online(RoleID, IsOnline, Members),
	{noreply, State#state{family_info=FamilyInfo#family_info{members=NewMembers}}};

do_handle_info({bc, Data}, #state{family_info=FamilyInfo}=State) ->
	{ok, NewFamilyInfo} = do_bc(Data, FamilyInfo),
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({get_family_recent_talk, RoleID}, #state{family_info=#family_info{talk_data=TalkData}}=State) ->
	?unicast(RoleID, #sc_talk_recent_list{list=lists:reverse(TalkData),channel=?CHAT_CHANNEL_FAMILY}),
	{noreply, State};

do_handle_info({bc_join_family_request, Request}, #state{family_info=FamilyInfo}=State)->
	#family_info{members=Members}=FamilyInfo,
	case lists:keyfind(?FAMILY_TITLE_DEPUTY, #family_member_info.family_title, Members) of
		false ->
			ignore;
		#family_member_info{role_id=RoleID} ->
			case check_role_can_agree_or_kick(?FAMILY_TITLE_DEPUTY) of
				true ->
					?unicast(RoleID,Request);
				false ->
					ignore
			end
	end,
	{noreply, State};

do_handle_info({get_member_list, RoleID}, #state{family_info=FamilyInfo}=State) ->
    #family_info{family_id=FamilyID, members=Members} = FamilyInfo,
    ?unicast(RoleID, #sc_family_get_member_list{family_id=FamilyID, members=[family_misc:to_p_family_member_info(E) || E <- Members]}),
    {noreply, State};

do_handle_info({get_cross_member_list, ServerID, RoleID, ViewFamilyID}, #state{family_info=FamilyInfo}=State) ->
    MasterName = family_fight_server:get_master_server_name(),
    #family_info{members=Members} = FamilyInfo,
    send_msg:direct_by_name(MasterName, all_family_rank_server, 
                            {get_cross_member_list_return, ServerID, RoleID, ViewFamilyID, [family_misc:to_p_family_member_info(E) || E <- Members]}),
    {noreply, State};

do_handle_info({agree_join, RoleID, FamilyID, JoinRoleID, FamilyRequest}, #state{family_info=FamilyInfo,fightEndTime=FightEndTime}=State) ->
	{ok, NewFamilyInfo} = do_agree_join(FamilyInfo, RoleID, FamilyID, JoinRoleID, FamilyRequest,FightEndTime),
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({agree_invite_join, SenderID ,RoleID, FamilyID}, #state{family_info=FamilyInfo,fightEndTime=FightEndTime}=State) ->
    {ok, NewFamilyInfo} = do_invite_join(FamilyInfo, SenderID, RoleID, FamilyID, FightEndTime),
    {noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({kick, RoleID, KickRoleID}, #state{family_info=FamilyInfo,fightEndTime=FightEndTime}=State) ->
	{ok, NewFamilyInfo} = do_kick(FamilyInfo, RoleID, KickRoleID,FightEndTime),
	{noreply, State#state{family_info=NewFamilyInfo}};

%% 离开公会
do_handle_info({leave, RoleID}, #state{family_info=FamilyInfo,fightEndTime=FightEndTime}=State) ->
	{ok, NewFamilyInfo} = do_leave(FamilyInfo, RoleID,FightEndTime),
	case NewFamilyInfo of
		?undefined ->
			{stop, normal, State#state{family_info=NewFamilyInfo}};
		_ ->
			{noreply, State#state{family_info=NewFamilyInfo}}
	end;

do_handle_info({refuse_join, RoleID, JoinRoleID}, #state{family_info=FamilyInfo}=State) ->
	do_refuse_join(FamilyInfo, RoleID, JoinRoleID),
	{noreply, State};

do_handle_info({get_family_info, RoleID}, #state{family_info=FamilyInfo}=State) ->
	?unicast(RoleID, #sc_family_get_info{result=0, family_info=family_misc:to_p_family_info(FamilyInfo),timestamp=0}),
	{noreply, State};

do_handle_info({change_notice, RoleID, Notice}, #state{family_info=FamilyInfo}=State) ->
	{ok, NewFamilyInfo} = do_change_notice(RoleID, Notice, FamilyInfo),
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({change_slogan, RoleID, Slogan}, #state{family_info=FamilyInfo}=State) ->
	{ok, NewFamilyInfo} = do_change_slogan(RoleID, Slogan, FamilyInfo),
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({send_msg_to_owner, Msg}, #state{family_info=#family_info{owner_role_id=OwnerRoleID}}=State) ->
    ?unicast(OwnerRoleID, Msg),
    {noreply, State};
% send_msg_to_owner2 广播给会长副会长
do_handle_info({send_msg_to_owner2, Msg}, #state{family_info=#family_info{members=Members}}=State) ->
    lists:foreach(fun(M)-> 
            if
                M#family_member_info.family_title =:= 1 
                  orelse M#family_member_info.family_title =:= 2 ->
                    ?unicast(M#family_member_info.role_id, Msg);
                true ->
                    ignore
            end     
        end, Members),
	{noreply, State};

do_handle_info({update_family_rank, Rank}, #state{family_info=FamilyInfo}=State) ->
	NewFamilyInfo = FamilyInfo#family_info{rank=Rank},
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info(loop, State) ->
    erlang:send_after(?DUMP_INTERVAL + random:uniform(300)*1000, erlang:self(), loop),
    FamilyInfo = State#state.family_info,
    case erlang:is_record(FamilyInfo, family_info) of
        true ->
            ?INFO("family loop dump"),
            family_data:dump_family_extra_data(FamilyInfo#family_info.family_id),
            NewFamilyInfo = update_family_member_info(FamilyInfo),
            db_sql:update_family_info(NewFamilyInfo),
            check_summary(NewFamilyInfo),
            {noreply, State#state{family_info=NewFamilyInfo},hibernate};
        false ->
            {stop, normal, State}
    end;

%%成员战斗力定周期更新
do_handle_info(member_power_fight, State) ->
	erlang:send_after(?POWER_UPDATE_INTERVAL + random:uniform(60)*1000, erlang:self(), member_power_fight),
	FamilyInfo = State#state.family_info,
	case erlang:is_record(FamilyInfo, family_info) of
		true ->
            ?INFO("family member power update"),
			NewFamilyInfo = update_family_member_info(FamilyInfo),
            if
                NewFamilyInfo =:= FamilyInfo ->
                    ignore;
                true ->
                    do_bc_msg_except(#sc_family_update_family_info{info=family_misc:to_p_family_info(NewFamilyInfo)}, [])
            end,
            check_summary(NewFamilyInfo),
			{noreply, State#state{family_info=NewFamilyInfo}};
		false ->
			{stop, normal, State}
	end;

do_handle_info({get_family_log_list, RoleID}, #state{family_info=FamilyInfo}=State) ->
	?unicast(RoleID, #sc_family_get_log_list{result=0, logDataList=FamilyInfo#family_info.log_data}),
	{noreply, State};

do_handle_info({cs_family_get_contribute_info, RoleID}, #state{family_info=FamilyInfo}=State) ->
	#family_info{contribute_log=ContributeLog,members=Members}=FamilyInfo,
	#family_member_info{lastContributeDate=LastContributeDate}= lists:keyfind(RoleID,#family_member_info.role_id,Members),
	ContributeRewardList = get_contribute_reward_list(),
	?unicast(RoleID, #sc_family_get_contribute_info{result=0, ctTypeInfoList=ContributeRewardList,ctLogList=ContributeLog
													,lastCTDate=erlang:trunc(util:datetime_to_seconds({LastContributeDate,{8,0,0}}) / (3600 * 24))}),
	{noreply, State};

do_handle_info({cs_family_do_contribute, _RoleID, _Contribution,_RoleName,_TypeID,_ConsumeType,_ConsumeValue,_Reward,_Rice} = Info, #state{family_info=FamilyInfo}=State) ->
	FamilyInfo2 = do_family_contribute(Info, FamilyInfo),
	{noreply, State#state{family_info=FamilyInfo2}};

do_handle_info(clean_weekly_contributes,#state{family_info=FamilyInfo}=State)->
	NewMembers = clean_weekly_contributes(FamilyInfo#family_info.members),
	FamilyInfo2 = FamilyInfo#family_info{members=NewMembers},
	do_bc_msg_except(#sc_family_update_family_info{info=family_misc:to_p_family_info(FamilyInfo2)}, []),
	State2 = State#state{family_info=FamilyInfo2},
	{noreply,State2};

do_handle_info({cs_family_change_member_power, RoleID, ChangeRoleID,TypeID}, State=#state{family_info=FamilyInfo}) ->
	NewFamilyInfo = do_change_member_power(RoleID,ChangeRoleID,TypeID, FamilyInfo),
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({cs_family_send_role_energy, RoleID,RoleName, TarRoleID}, #state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	case lists:keytake(TarRoleID, #family_member_info.role_id, Members) of
		false ->
			NewFamilyInfo = FamilyInfo,
			?unicast(RoleID,#sc_family_send_role_energy{result=4});
		{value,TarRoleInfo=#family_member_info{recvEnergyList=RecvEnergyList},OtherMembers} ->
			TarRoleInfo2 = TarRoleInfo#family_member_info{recvEnergyList=[{RoleID,RoleName}|RecvEnergyList]},
			NewFamilyInfo = FamilyInfo#family_info{members=[TarRoleInfo2|OtherMembers]},
			catch role_lib:send_server(RoleID,{role_send_family_energy,TarRoleID}),
			RecvEnergyList2 = lists:foldl(fun({_,Name},Acc)->[Name|Acc] end,[], RecvEnergyList),
			?unicast(TarRoleID, #sc_family_get_send_role_energy_list{roleNameList=[RoleName|RecvEnergyList2]}),
			?unicast(RoleID, #sc_family_send_role_energy{result=1})
	end,
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({cs_family_get_role_energy, RoleID,TarRoleName}, #state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	case lists:keytake(RoleID, #family_member_info.role_id, Members) of
		false ->
			NewFamilyInfo=FamilyInfo,
			?unicast(RoleID, #sc_family_get_role_energy{result=2});
		{value, MemberInfo=#family_member_info{recvEnergyList=RecvEnergyList}, OtherMembers} ->
			case lists:keytake(erlang:list_to_binary(TarRoleName), 2, RecvEnergyList) of
				false ->
					NewFamilyInfo=FamilyInfo,
					?unicast(RoleID,#sc_family_get_role_energy{result=3});
				{value, _, OtherRecvList} ->
					Energy = data_family:get(familySendEnergyNum),
					case catch role_lib:send_server(RoleID, {role_get_family_energy, Energy}) of
						{'EXIT', {badarg, _}} ->
							NewFamilyInfo = FamilyInfo;
						_ ->
							NewMemberInfo = MemberInfo#family_member_info{recvEnergyList=OtherRecvList},
							NewFamilyInfo = FamilyInfo#family_info{members = [NewMemberInfo|OtherMembers]}
					end
			end
	end,			
	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({cs_family_get_send_role_energy_list, RoleID}, #state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	case lists:keytake(RoleID, #family_member_info.role_id, Members) of
		false ->
			?unicast(RoleID,#sc_family_get_send_role_energy_list{roleNameList=[]});
		{value,#family_member_info{recvEnergyList=RecvEnergyList},_} ->
			RecvEnergyList2 = lists:foldl(fun({_,A},Acc) -> [A|Acc] end, [], RecvEnergyList),
			?unicast(RoleID,#sc_family_get_send_role_energy_list{roleNameList=RecvEnergyList2})
	end,
	{noreply, State};

%% do_handle_info(do_zero_clock_setting, #state{family_info=FamilyInfo}=State)->
%% 	FamilyInfo2 = do_zero_clock_setting(FamilyInfo),
%% 	{noreply, State#state{family_info=FamilyInfo2}};

do_handle_info(check_and_do_zero_clock, #state{family_info=FamilyInfo}=State) ->
	check_and_do_zero_clock(),
	FamilyInfo2 = do_zero_clock_setting(FamilyInfo),
	{noreply,State#state{family_info=FamilyInfo2}};

do_handle_info({cs_family_get_member_power, RoleID}, #state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	case lists:keytake(RoleID, #family_member_info.role_id, Members) of
		false ->
			?unicast(RoleID,#sc_family_get_member_power{memberPowerList=[]});
		{value,#family_member_info{family_title=Title},_} ->
			PowerList = get_role_power_list(Title),
			?unicast(RoleID,#sc_family_get_member_power{memberPowerList=PowerList})
	end,
	{noreply, State};

do_handle_info({cs_familyfight_info, RoleID}, #state{family_info=FamilyInfo}=State) ->
	#family_info{family_score=TotalScore,family_id=FamilyID, family_fight_other_info=FamilyFightInfo,world_rank=WorldRank,family_fighter_group=FamilyFighterGroup} = FamilyInfo,
	#family_fight_other_info{war_period=WarPeriod,win_star=WinStar,matcher_win_star=MatchWinStar,fight_result=FightResult,attack_times=AttackTimes,defend_times=DefendTimes}=FamilyFightInfo,
	erlang:send(family_fight_server
				, {cs_familyfight_info,RoleID,FamilyID,TotalScore,WarPeriod, WinStar, MatchWinStar,FightResult,AttackTimes,DefendTimes,WorldRank,erlang:length(FamilyFighterGroup)}),
	{noreply, State};

do_handle_info({cs_familyfight_sign, RoleID, RoleIDList}, #state{family_info=FamilyInfo}=State) ->
	#family_info{level=FamilyLevel,family_id=FamilyID,cur_members=Num,members=Members,family_score=Score,family_name=FamilyName,owner_role_name=OwnerName} = FamilyInfo,
	NewFamilyInfo = case FamilyLevel < data_family_fight:get(signNeedLevel) of
		true ->
			?unicast(RoleID,#sc_familyfight_sign{result=3}),
            FamilyInfo;
		_ ->
            {SignNeedNumMin,_SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
			case Num < SignNeedNumMin of
				true ->
					?unicast(RoleID, #sc_familyfight_sign{result=5}),
                    FamilyInfo;
				_ ->
					case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
						#family_member_info{family_title=Title} ->
							case check_role_can_sign(Title) of
								true ->
                                    case lists:all(fun(RID)-> lists:keymember(RID, #family_member_info.role_id, Members) end,RoleIDList) of
                                        true ->
                                            TotalFightPower = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, Members),
                                            ?INFO("公会(~w)报名工会战，战斗力~w",[FamilyID,TotalFightPower]),
                                            erlang:send(family_fight_server, {cs_familyfight_sign, RoleID,FamilyID,Score,FamilyName,TotalFightPower,OwnerName,FamilyLevel}),
                                            FamilyInfo#family_info{family_fighter_group=RoleIDList};
                                        false ->
                                            ?unicast(RoleID, #sc_familyfight_sign{result=5}),
                                            FamilyInfo
                                    end;
								_ ->
									?unicast(RoleID, #sc_familyfight_sign{result=6}),
                                    FamilyInfo
							end;
						_ ->
							?unicast(RoleID, #sc_familyfight_sign{result=2}),
                            FamilyInfo
					end
			end
	end,			
	{noreply, State#state{family_info=NewFamilyInfo}};

%% 先由fight_server取对方数据,然后把对方信息route回来,拼接完本地信息后发送到客户端
%% fight_server 取到的数据可以在X 秒内视为有效数据,超时后需重新获取
%% family_match_info的serverID和familyID在初始化时从family_fight_server获取.重新分配对手后由family_fight_server主动推送更新
do_handle_info({cs_familyfight_fighter_info, RoleID}, #state{family_info=FamilyInfo, family_match=FamilyMatch,fightEndTime=FT,periodEndTime=ET}=State) ->
	#family_info{members=Members,family_id=FamilyID,family_fight_other_info=FamilyFightOtherInfo,family_name=FamilyName,world_rank=Rank,family_fighter_group=GroupList}=FamilyInfo,
	#family_fight_other_info{win_star=WinStar,is_sign=IsSign}=FamilyFightOtherInfo,
	%#family_match{server_id=MatchServerID,family_id=MatchFamilyID,time_stamp=TimeStamp, fighter_info_list=MatchFighterInfoList}=FamilyMatch,
	#family_match{time_stamp=TimeStamp, fighter_info_list=MatchFighterInfoList}=FamilyMatch,
    %% 防止短时间重复取数据
	case is_match_info_valid(TimeStamp) of
		true -> %Mark
            ?INFO("使用缓存数据 time:~w",[TimeStamp]),
            GroupListLength = erlang:length(GroupList),
            {SignNeedNumMin,_SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
            if
                GroupListLength < SignNeedNumMin ->
                    GroupListNew = fill_grouplist(GroupList,Members),
                    SelfInfoList = generate_self_info(get_self_info_list_for_fight_cache(Members,GroupListNew,?cache_fighter_list_self,IsSign,FT,ET),FamilyID,WinStar,Rank,0,FamilyName),
                    send_client_member_info(RoleID,MatchFighterInfoList,SelfInfoList,FamilyFightOtherInfo,IsSign,FT,ET),
                    {noreply, State#state{family_info=FamilyInfo#family_info{family_fighter_group=GroupListNew}}};
                true ->
        			SelfInfoList = generate_self_info(get_self_info_list_for_fight_cache(Members,GroupList,?cache_fighter_list_self,IsSign,FT,ET),FamilyID,WinStar,Rank,0,FamilyName),
        			send_client_member_info(RoleID,MatchFighterInfoList,SelfInfoList,FamilyFightOtherInfo,IsSign,FT,ET),
                    {noreply, State}
            end;
		false ->
            ?INFO("请求新数据 time:~w",[TimeStamp]),
			#family_fight_other_info{matcher_server_id=MatchServerID,matcher_family_id=MatchFamilyID}=FamilyFightOtherInfo,
			erlang:send(family_fight_server, {cs_familyfight_fighter_info, MatchServerID,MatchFamilyID, RoleID,FamilyID}),
            {noreply, State}
	end;

%% 联盟信息做缓存,出战信息不缓存,每次重新获取
do_handle_info({family_fighter_info_get_back, RoleID,Info,MatchServerID,MatchFamilyID}, #state{family_info=FamilyInfo,family_match=FamilyMatch,fightEndTime=FT,periodEndTime=ET}=State) ->
	State2 = State#state{family_match=FamilyMatch#family_match{server_id=MatchServerID,family_id=MatchFamilyID
													,time_stamp=util:now()+?match_info_valid_interval,fighter_info_list=Info}},
    ?INFO("获得新数据~w",[State2#state.family_match]),
	#family_info{members=Members,family_fight_other_info=FamilyFightOtherInfo,family_id=FamilyID,family_name=FamilyName,world_rank=Rank,family_fighter_group=GroupList}=FamilyInfo,
	#family_fight_other_info{win_star=WinStar,is_sign=IsSign}=FamilyFightOtherInfo,
    GroupListLength = erlang:length(GroupList),
    {SignNeedNumMin,_SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
    if
        GroupListLength < SignNeedNumMin ->
            GroupListNew = fill_grouplist(GroupList,Members);
        true ->
            GroupListNew = GroupList
    end,
	SelfInfoList = generate_self_info(get_self_info_list_for_fight_cache(Members,GroupListNew,?cache_fighter_list_self,IsSign,FT,ET),FamilyID,WinStar,Rank,0,FamilyName),
	send_client_member_info(RoleID,Info,SelfInfoList,FamilyFightOtherInfo,IsSign,FT,ET),
	{noreply, State2#state{family_info=FamilyInfo#family_info{family_fighter_group=GroupListNew}}};

%% 接受SelfServerID、SelfFamilyID对手服务器发送来的同步信息
do_handle_info({family_fighter_info_sync_back, SelfServerID,SelfFamilyID,ListInfo}, #state{family_match=FamilyMatch}=State) ->
    #family_match{server_id=TarServerID,family_id=TarFamilyID} = FamilyMatch,
    if
        %更新数据必须familyID一致，或者是旧的TarFamilyID为0
        SelfFamilyID =:= TarFamilyID orelse TarFamilyID =:= 0 ->
            ?INFO("同步了数据 ~w",[ListInfo]),
            State2 = State#state{family_match=FamilyMatch#family_match{time_stamp=util:now()+?match_info_valid_interval,fighter_info_list=ListInfo}};
        true ->
            ?ERR("family_fighter_info_sync_back fail, server or id is wrong,~w ~w ~w ~w",
                    [SelfServerID,SelfFamilyID,TarServerID,TarFamilyID]),
            State2 = State
    end,
    {noreply, State2};

do_handle_info({family_fighter_info_get,ServerID,FamilyID ,GetRoleID,GetFamilyID,GetServerID}
			   , #state{family_info=FamilyInfo} = State)->
	#family_info{members=Members,family_fighter_group=GroupList0}=FamilyInfo,
    GroupListLength = erlang:length(GroupList0),
    {SignNeedNumMin,_SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
    if
        GroupListLength < SignNeedNumMin ->
            GroupListNew = fill_grouplist(GroupList0,Members);
        true ->
            GroupListNew = GroupList0
    end,
	SelfInfoList = get_self_info_list_for_fight(Members,GroupListNew),
	send_msg:direct(GetServerID,family_fight_server,{family_fighter_info_get_back_info,true,SelfInfoList, ServerID,FamilyID,GetRoleID, GetFamilyID,GetServerID}),	
	{noreply,State#state{family_info=FamilyInfo#family_info{family_fighter_group=GroupListNew}}};

%% 此处是攻击方发送给被攻击方进程的消息
do_handle_info({cs_familyfight_attack, RoleID, TarRoleID, TarFamilyID, TarServerID}
			   , #state{family_info=FamilyInfo,family_match=FamilyMatch}=State)->
	%%%#family_match{fighter_info_list=MatchFighterInfoList}=FamilyMatch,
	#family_match{locked_list=LockedList}=FamilyMatch,
	#family_info{members=Members0,family_fighter_group=GroupList} = FamilyInfo,
    Members = lists:filter(fun(R)-> lists:member(R#family_member_info.role_id, GroupList) end, Members0),
	case lists:keyfind(TarRoleID, #family_member_info.role_id, Members) of
		false ->  %% 目标玩家不存在
			#family_member_info{is_join_war=IsJoinWar,attack_times=AttackTimes} = lists:keyfind(RoleID,#family_member_info.role_id,Members),
			if IsJoinWar > 0 ->
				   %% 				   case dirty_check_can_be_challenged(MatchFighterInfoList, TarRoleID) of
				   %% 					   %		{true, 0} ->
				   %% 					   %			erlang:send(family_fight_server,{cs_familyfight_attack, RoleID, SelfGerInfoList,TarRoleID, TarFamilyID, TarServerID});
				   %% 					   % 本地没有数据,也要进行挑战信息更新,如果远程服务器连接失败,则恢复本地数据,通过timeout恢复
				   %% 					   {true,TarFighterInfo} ->
				   %% 						   State2 = do_familyfight(RoleID,State,TarRoleID,TarFighterInfo,TarFamilyID,TarServerID);
				   case check_can_challenge(AttackTimes,LockedList, TarRoleID) of
					   {true,LockedList2} ->
						   State2 = do_familyfight(RoleID,State,TarRoleID,LockedList2,TarFamilyID, TarServerID);
					   {false,1} ->
						   State2=State,
						   ?unicast(RoleID, #sc_familyfight_attack{result=3,fight_dtl=[]});
					   {false,2} ->
						   State2=State,
						   ?unicast(RoleID, #sc_familyfight_attack{result=4,fight_dtl=[]})
				   end;
			   true ->
				   State2=State,
				   ?unicast(RoleID,#sc_familyfight_attack{result=9,fight_dtl=[]})
			end;
		_ ->
			State2=State,
			?unicast(RoleID, #sc_familyfight_attack{result=8, fight_dtl=[]})
	end,
	{noreply, State2};

do_handle_info({familyfight_attack_undo, RoleID, TarRoleID},#state{family_info=FamilyInfo,family_match=FamilyMatch}=State)->
%%	#family_match{fighter_info_list=MatchFighterInfoList}=FamilyMatch,
	#family_match{locked_list=LockedList}=FamilyMatch,
	#family_info{members=Members} = FamilyInfo,
	{value,#family_member_info{attack_times=AttackTimes}=FamilyRole,OtherMembers}  = lists:keytake(RoleID, #family_member_info.role_id, Members),
	FamilyInfo2=FamilyInfo#family_info{members=[FamilyRole#family_member_info{attack_times=AttackTimes-1}|OtherMembers]},
	FamilyMatch2 = FamilyMatch#family_match{locked_list=lists:delete(TarRoleID, LockedList)},
%% 	FamilyMatch2 = 
%% 		case lists:take(TarRoleID, #p_familyfighter_member_info.roleID, MatchFighterInfoList) of
%% 			false ->
%% 				FamilyMatch;
%% 			{value,#p_familyfighter_member_info{defendTimes=DefendTimes}=FighterInfo ,OtherMatchers}->
%% 				if DefendTimes > 0 ->
%% 					   FamilyMatch#family_match{fighter_info_list=[FighterInfo#p_familyfighter_member_info{defendTimes=DefendTimes+1}|OtherMatchers]};
%% 				   true ->
%% 					   FamilyMatch
%% 				end
%% 		end,
	{noreply,State#state{family_info=FamilyInfo2,family_match=FamilyMatch2}};

do_handle_info({family_fight_attack,AttackRoleID,AttackFamilyID,AttackServerID,AttackGerInfoList,AttackRoleName,AttackFamilyName,TarRoleID,TarFamilyID,TarServerID,ExtInfo}
			   ,#state{family_info=FamilyInfo,family_match=Match}=State)->
	#family_info{members=Members,family_name=FamilyName,family_fight_other_info=FightOtherInfo,family_fighter_group=GroupList} = FamilyInfo,
	#family_match{fighter_info_list=FighterInfoList} = Match,
	%%%case lists:keytake(TarRoleID, #family_member_info.role_id, Members) of
	case lists:keyfind(TarRoleID, #family_member_info.role_id, Members) of
		false ->
			State2=State,
			erlang:send(family_fight_server,{family_fight_attack_back_info, not_exist_role, AttackRoleID,AttackFamilyID,AttackServerID,"",0
											 ,TarRoleID,TarFamilyID,TarServerID,"",{false, 0, 0, 0},get_self_info_list(Members),ExtInfo});
		%%%{value, #family_member_info{defend_times=DefendTimes,role_name=TarRoleName}=FamilyMemberInfo,OtherMembers}->
		#family_member_info{role_name=TarRoleName,defend_times=DefendTimes}->
			%%%%%%%% defendTimes 表示被夺取的星星数,DefendTimes = MaxDefendTimes 可战斗可夺取星星,DefendTimes >= MaxdefendTime,可战斗不可夺取星星
			%% 			case DefendTimes >= MaxDefendTimes of
			%% 				true ->
			%% 					State2=State,
			%% 					erlang:send(family_fight_server,{family_fight_attack_back_info, role_can_not_attack, AttackRoleID,AttackFamilyID,AttackServerID,"",0
			%% 													,TarRoleID,TarFamilyID,TarServerID,"",0,get_self_info_list(Members),ExtInfo});
			%% 				_ ->
		%%%	Members2 = [FamilyMemberInfo#family_member_info{defend_times=DefendTimes+1}|OtherMembers],
			FighterInfoList2 = 
				case lists:keytake(AttackRoleID,#p_familyfighter_member_info.roleID,FighterInfoList) of
					false ->
						FighterInfoList;
					{value,#p_familyfighter_member_info{attackTimes=AttackTimes}=FighterInfo, OtherFigherInfoList} ->
						[FighterInfo#p_familyfighter_member_info{attackTimes=AttackTimes-1}|OtherFigherInfoList]
				end,
			Match2 = Match#family_match{fighter_info_list=FighterInfoList2},
		%%%	FamilyInfo2 = FamilyInfo#family_info{members=Members2},
		%%%	State2 = State#state{family_info=FamilyInfo2,family_match=Match2},
			State2 = State#state{family_match=Match2},
			DefendGersInfo = role_data:get_otherRoleFighter(TarRoleID),
			WarPeriod = FightOtherInfo#family_fight_other_info.war_period,
            TarEquipList = role_data:get_otherRoleItemEquips(TarRoleID),
            %%将数据库中获得的精灵装备列表按照精灵分类
            GerEquipList = role_item:assort_ger_equiplist(TarEquipList),
            TarLegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
			Pid=self(),
			spawn(%fun(AttackRoleID,AttackFamilyID,AttackServerID,AttackGerInfoList,TarRoleID,TarFamilyID,TarServerID,DefendGersInfo,Pid,ExtInfo)->
			  fun()->
					  {AttackerList,LieuAddAttacker, TalentListA,TrSpecialA,SkinInfoA,LegendAddListA} = AttackGerInfoList,
					  {DefenderList,LieuAddDefender, TalentListD,TrSpecialD} = DefendGersInfo,
                      DSkinInfo = role_data:get_otherRoleSkinInfo(TarRoleID),

					  {IsWin, FightRecord0, {FinalState,_,_,_}} = role_fight:new(AttackerList, DefenderList,LieuAddAttacker, LieuAddDefender, TalentListA, TalentListD,TrSpecialA,TrSpecialD,SkinInfoA,DSkinInfo,LegendAddListA,TarLegendAddList),
                      FighterList2 = role_data:get_FighterList_with_effect(AttackRoleID,TarRoleID,FightRecord0#sc_fight_request.fighterList),
                      FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
					  GetStar = calc_get_star(IsWin,FinalState,DefendTimes),
					  ReplayUID = tk_id:gen_replayUID(),%tk_id:gen_replayUID_family(),
					  {IsWin2,WinFamilyName} = case IsWin of
												   false ->
													   {1,FamilyName};
												   _ ->
													   {2,AttackFamilyName}
											   end,
					  TimeStamp=util:now(),
					  catch db_sql:set_familyfight_record(ReplayUID, FightRecord, ?REPLAY_TYPE_FAMILY_FIGHT,TarRoleID,TarFamilyID,TarRoleName,AttackRoleID,AttackFamilyID,AttackRoleName,WinFamilyName,TimeStamp,2,IsWin2,GetStar,WarPeriod),
					  erlang:send(Pid,{familyfight_update_star_defend,IsWin2,GetStar,ReplayUID,TarRoleID,TarRoleName,AttackRoleID,AttackRoleName,WinFamilyName}),
					  erlang:send(family_fight_server,{family_fight_attack_back_info,true,AttackRoleID,AttackFamilyID,
                                                        AttackServerID,WinFamilyName,GetStar,TarRoleID,TarFamilyID,
                                                        TarServerID,TarRoleName,{IsWin2,FightRecord,ReplayUID,TimeStamp},
                                                        get_self_info_list_for_fight(Members,GroupList),ExtInfo})
			  end)
		%%%end
	end,	
	{noreply, State2};

do_handle_info({family_fight_attack_back,Res, AttackRoleID,AttackFamilyID, WinFamilyName,GetStar,TarRoleID,TarFamilyID,TarServerID,TarRoleName,RecordInfo,FightersInfoList,_}
			   ,#state{family_info=FamilyInfo,family_match=Match,family_fight_hist=HistList}=State)->
	#family_info{members=Members,family_fight_other_info=FamilyFightInfo,family_fighter_group=GroupList}=FamilyInfo,
	#family_fight_other_info{win_star=FamilyStar,attack_times=FamilyAttackTimes,war_period=WarPeriod}=FamilyFightInfo,
	%% 	#family_match{fighter_info_list=Fighters}=Match,
	#family_match{locked_list=LockedList}=Match,
	case Res of
		true ->
			{IsWin,FightRecord,_ReplayUID,TimeStamp}=RecordInfo,
			{value,TarFighter=#p_familyfighter_member_info{defendTimes=DefendTimes},OtherFighters} = lists:keytake(TarRoleID, #p_familyfighter_member_info.roleID, FightersInfoList),
			TarFighter2 = TarFighter#p_familyfighter_member_info{defendTimes=DefendTimes-GetStar},
			FighterInfoList2 = [TarFighter2|OtherFighters],
			Match2 = Match#family_match{fighter_info_list=FighterInfoList2,time_stamp=util:now() + ?match_info_valid_interval,locked_list=lists:delete(TarRoleID, LockedList)},
			{value,#family_member_info{win_star=MemberStar,role_name=AttackRoleName} = MemberInfo,OtherMembers} = lists:keytake(AttackRoleID, #family_member_info.role_id, Members),
			
%防止因为bug导致获得星星数超过理论值上限
            GetStar2 = if
                            MemberStar+GetStar > ?max_get_star ->
                                ?ERR("FamilyFight ~w get ~w+~w star, is overflow",[AttackRoleID,MemberStar,GetStar]),
                                0;
                            true ->
                                GetStar
                       end,

            MemberInfo2 = MemberInfo#family_member_info{win_star=MemberStar+GetStar2},
			FamilyWinStar = FamilyStar+GetStar2,
			FamilyInfo2 = FamilyInfo#family_info{members=[MemberInfo2|OtherMembers]
												,family_fight_other_info=FamilyFightInfo#family_fight_other_info{win_star=FamilyWinStar,attack_times=FamilyAttackTimes+1}},
			ReplayUID = tk_id:gen_replayUID(),
			catch db_sql:set_familyfight_record(ReplayUID,FightRecord,?REPLAY_TYPE_FAMILY_FIGHT,TarRoleID,TarFamilyID,TarRoleName,AttackRoleID,AttackFamilyID,AttackRoleName,WinFamilyName,TimeStamp,1,IsWin,GetStar2,WarPeriod),
			%% update_family_win_star消息由family_fight_server转发给family_fight_master_server
            erlang:send(family_fight_server, {update_family_win_star, AttackFamilyID,FamilyWinStar}),
			FightRecordDtl = #p_familyfight_record_info{attackerRoleID=AttackRoleID,defenderRoleID=TarRoleID,win_star=GetStar2,result=IsWin,attackerName=AttackRoleName,defenderName=TarRoleName,recordUID=ReplayUID,winFamilyName=WinFamilyName},
			HistList2 = [FightRecordDtl|HistList],
			State2=State#state{family_fight_hist=HistList2,family_info=FamilyInfo2,family_match=Match2},
			do_bc_msg_except(#sc_familyfight_update_star_info{attackRoleID=AttackRoleID,defendRoleID=TarRoleID,fighterType=1,starNum=GetStar2,fightRecord=FightRecordDtl}, []),
			?unicast(AttackRoleID,#sc_familyfight_attack{result=1,fight_dtl=#p_familyfight_record_dtl{recordUID=ReplayUID,winScore=GetStar2,fightInfo=FightRecord,attackerName=AttackRoleName,defenderName=TarRoleName}});
		_ ->
			{value,#family_member_info{attack_times=AttackTimes}=MemberInfo,OtherMembers} = lists:keytake(AttackRoleID,#family_member_info.role_id,Members),
			Members2 = [MemberInfo#family_member_info{attack_times=AttackTimes-1}|OtherMembers],
			FamilyInfo2 = FamilyInfo#family_info{members=Members2},
%% 			{value,#fighter_info{defendTimes=DefendTimes}=Fighter,OtherFighters} = lists:keytake(TarRoleID,#fighter_info.roleID,Fighters),
%% 			Fighters2 = [Fighter#fighter_info{defendTimes=DefendTimes - 1}|OtherFighters],
			Match2 = Match#family_match{fighter_info_list=FightersInfoList,time_stamp=util:now() + ?match_info_valid_interval,locked_list=lists:delete(TarRoleID, LockedList)},
			State2 = State#state{family_info=FamilyInfo2,family_match=Match2}
	end,
    
    #family_info{members=Members3}=State2#state.family_info,
    SelfInfoList = get_self_info_list_for_fight(Members3,GroupList),
    ?INFO("向~w ~w同步自己的战斗数据",[TarServerID,TarFamilyID]),
    erlang:send(family_fight_server,{family_fighter_info_sync, AttackFamilyID,SelfInfoList,TarServerID,TarFamilyID}),   
    {noreply,State2};

do_handle_info({familyfight_update_star_defend,IsWin,GetStar,ReplayUID,TarRoleID,TarRoleName,AttackRoleID,AttackRoleName,WinFamilyName}
			   ,#state{family_fight_hist=HistList,family_match=Matcher,family_info=FamilyInfo}=State)->
	FightRecordDtl = #p_familyfight_record_info{attackerRoleID=AttackRoleID,defenderRoleID=TarRoleID,win_star=GetStar,result=IsWin
											,attackerName=AttackRoleName,defenderName=TarRoleName,recordUID=ReplayUID,winFamilyName=WinFamilyName},
	HistList2 = [FightRecordDtl|HistList],
	#family_match{fighter_info_list=Fighters}=Matcher,
	#family_info{members=Members}=FamilyInfo,
	Fighters2 =
		case lists:keytake(AttackRoleID,#p_familyfighter_member_info.roleID,Fighters) of
			false ->
				Fighters;
			{value,#p_familyfighter_member_info{winStar=GetStars,attackTimes=AttackTimes}=Fighter,OtherFighters}->
				[Fighter#p_familyfighter_member_info{winStar=GetStars+GetStar,attackTimes=AttackTimes+1}|OtherFighters]
		end,
	Matcher2 = Matcher#family_match{fighter_info_list=Fighters2},
	#family_info{family_fight_other_info=FightOtherInfo} = FamilyInfo,
	#family_fight_other_info{matcher_win_star=MatcherFamilyStar} = FightOtherInfo,
	FightOtherInfo2 = FightOtherInfo#family_fight_other_info{matcher_win_star=MatcherFamilyStar+GetStar},
	Members2 = 
		case lists:keytake(TarRoleID, #family_member_info.role_id, Members) of
			false ->
				Members;
			{value, #family_member_info{defend_times=DefendTimes}=Member, OtherMembers} ->
				[Member#family_member_info{defend_times=DefendTimes+GetStar}|OtherMembers]
		end,
	FamilyInfo2 = FamilyInfo#family_info{family_fight_other_info=FightOtherInfo2,members=Members2},
	State2 = State#state{family_fight_hist=HistList2,family_match=Matcher2,family_info=FamilyInfo2},
	do_bc_msg_except(#sc_familyfight_update_star_info{attackRoleID=AttackRoleID,defendRoleID=TarRoleID,fighterType=2,starNum=GetStar,fightRecord=FightRecordDtl}, []),
	{noreply, State2};

do_handle_info({cs_familyfight_get_fighter_history,RoleID,TarRoleID},#state{family_fight_hist=HistList}=State)->
	History = lists:filter(fun(#p_familyfight_record_info{attackerRoleID=RoleID1, defenderRoleID=RoleID2})->
								   if RoleID1 =:= TarRoleID orelse RoleID2 =:= TarRoleID ->
								   %if RoleID2 =:= TarRoleID ->
										  true ;
									  true ->
										  false
								   end
						   end,HistList),
	?unicast(RoleID,#sc_familyfight_get_fighter_history{historyList=lists:keysort(#p_familyfight_record_info.recordUID, History)}),
	{noreply, State};

do_handle_info({cs_familyfight_get_fight_record_list,RoleID},#state{family_fight_hist=HistList}=State)->
	?unicast(RoleID,#sc_familyfight_get_fight_record_list{result=1,infoList=HistList}),
	{noreply, State};

do_handle_info({cs_familyfight_result, RoleID}, #state{family_info=FamilyInfo}=State)->
	#family_info{family_fight_other_info=OtherInfo,family_id=FamilyID,world_rank=Rank}=FamilyInfo,
	erlang:send(family_fight_server, {cs_familyfight_result, RoleID, FamilyID,OtherInfo#family_fight_other_info.win_star
									 ,Rank,OtherInfo#family_fight_other_info.last_world_rank
									 ,OtherInfo#family_fight_other_info.fight_result,OtherInfo#family_fight_other_info.matcher_win_star
									 ,OtherInfo#family_fight_other_info.matcher_server_id,OtherInfo#family_fight_other_info.matcher_family_name}),
	{noreply, State};


do_handle_info({family_fight_update_match_info,MatchServerID,MatchFamilyID,MatchFamilyName,MatchFamilyRank,MatchFamilyStar,MatchPeriod,IsSign
               ,StartTime,PrepareEndTime,RandomEndTime,FightEndTime,PeriodEndTime}
			   , #state{family_info=FamilyInfo}=State)->
	#family_info{family_id=FamilyID,members=Members,family_fight_other_info=FamilyFightInfo}=FamilyInfo,
	Period=FamilyFightInfo#family_fight_other_info.war_period,
	%% family进程启动,发现period不匹配或者当前没有进行出战列表匹配,则重新生成出战列表
	case MatchPeriod == 0 andalso Period == 0 of
		true ->
			%% family记录Period和获取的period都是0,则让初始化该字段;否则当MatchPeriodID和PeriodID相同,数据不变,取出战报即可;当ID不同或者没有生成出战列表,则初始化出战人员等信息
			FamilyInfo2 = FamilyInfo#family_info{family_fight_other_info=#family_fight_other_info{}},
			Rep=[];
		false ->
			case (MatchPeriod =/= Period orelse false == lists:keyfind(1, #family_member_info.is_join_war, Members)) of
				true ->
					MembersT = lists:keysort(#family_member_info.fight_power, Members),
					MaxFighterNum = data_family_fight:get(signMaxNum),
%%                    NewMembersT = lists:sort(fun(A,B)->
%%                                if
%%                                    A#family_member_info.family_title > B#family_member_info.family_title ->
%%                                        true;
%%                                    A#family_member_info.family_title < B#family_member_info.family_title ->
%%                                        false;
%%                                    A#family_member_info.fight_power > B#family_member_info.fight_power ->
%%                                        true;
%%                                    A#family_member_info.fight_power < B#family_member_info.fight_power ->
%%                                        false;
%%                                    true ->
%%                                        true
%%                                end
%%                    end, MembersT),
					{Members2,_} = lists:foldl(fun(Member, {Acc,Count})->
													   case Count of
														   MaxFighterNum->
															   {[Member#family_member_info{attack_times=0,defend_times=0,win_star=0,is_join_war=0}|Acc],MaxFighterNum};
														   _ ->
															   {[Member#family_member_info{attack_times=0,defend_times=0,win_star=0,is_join_war=Count+1}|Acc],Count+1}
													   end
											   end, {[],0}, MembersT),
					FamilyFightInfo2 = FamilyFightInfo#family_fight_other_info{war_period=MatchPeriod, is_sign=IsSign,attack_times=0,defend_times=0,win_star=0,matcher_family_rank=MatchFamilyRank
																			   ,matcher_server_id=MatchServerID,matcher_family_id=MatchFamilyID,matcher_family_name=MatchFamilyName,matcher_win_star=MatchFamilyStar},
					FamilyInfo2 = FamilyInfo#family_info{members=Members2,family_fight_other_info=FamilyFightInfo2},
					Rep=[];
				false ->
					FamilyInfo2=FamilyInfo,
					Rep = db_sql:get_family_fight_record_summary(FamilyID,MatchPeriod)
			%Rep = State#state.family_fight_hist
			end
	end,
	State2 = State#state{family_info=FamilyInfo2,family_fight_hist=Rep,startTime=StartTime,prepareEndTime=PrepareEndTime
                      ,randomEndTime=RandomEndTime,fightEndTime=FightEndTime,periodEndTime=PeriodEndTime},
	{noreply, State2};

%% 工会战结算后，最后通知family进程，比赛结果 
do_handle_info({sync_period_result, F}, #state{family_info=FamilyInfo,family_match=FamilyMatch}=State)->
?INFO("family~w RECV sync_period_result ~w",[FamilyInfo#family_info.family_id,F]),
	#family_info{family_fight_other_info=Info,members=Members,world_rank=LastWorldRank}=FamilyInfo,
	#pf_fighter{rank=NowRank,result=FightResult,matchStar=MatchStar,star=MyStar}=F,
	#family_fight_other_info{win_star=WinStar,matcher_server_id=MatchServerID,matcher_family_name=MatchFamilyName}=Info,
	Info2 = Info#family_fight_other_info{last_world_rank=LastWorldRank,matcher_win_star=MatchStar,fight_result=FightResult},
	FamilyInfo2 = FamilyInfo#family_info{world_rank=NowRank, family_score=F#pf_fighter.score,family_fight_other_info=Info2},
    %% 发送奖励邮件
	if 
        F#pf_fighter.matchFamilyID /= 0 andalso F#pf_fighter.result == 1 ->
            MarginStar = MyStar - MatchStar,
                    lists:foreach(fun(#family_member_info{role_id=RoleID,win_star=StarNum})->
                                        if
                                            StarNum /= 0 ->
                                                mail_server:send_sys_mail(RoleID, ?MAIL_FAMILYFIGHT_REWARD_MEMBER, [StarNum,StarNum*?family_fight_reward_person], "", {sell_reward,0,0,0,0,[],StarNum*?family_fight_reward_person,[]});
                                            true ->
                                                next
                                        end,
                                        if
                                            MarginStar /= 0 ->
                                                mail_server:send_sys_mail(RoleID, ?MAIL_FAMILYFIGHT_REWARD_WIN, [MarginStar,MarginStar*?family_fight_reward], "", {sell_reward,0,0,0,0,[],MarginStar*?family_fight_reward,[]});
                                            true ->
                                                next
                                        end
                                  end,Members);
        F#pf_fighter.matchFamilyID /= 0 andalso (F#pf_fighter.result == 2 orelse F#pf_fighter.result == 3)->
            lists:foreach(fun(#family_member_info{role_id=RoleID,win_star=StarNum})->
                                        if
                                            StarNum /= 0 ->
                                                mail_server:send_sys_mail(RoleID, ?MAIL_FAMILYFIGHT_REWARD_MEMBER, [StarNum,StarNum*?family_fight_reward_person], "", {sell_reward,0,0,0,0,[],StarNum*?family_fight_reward_person,[]});
                                            true ->
                                                next
                                        end
                          end,Members);
		true ->
			ignore
	end,
    %% 通知各个客户端结算结果
	do_bc_msg_except(#sc_familyfight_result{result=1,infoList=[#p_familyfight_result_dtl{fight_result=FightResult,win_star=WinStar,now_rank=NowRank,old_rank=LastWorldRank
											  ,matcher_win_star=MatchStar,matcher_server_id=MatchServerID,matcher_family_name=MatchFamilyName}]},[]),
    %缓存下此刻的数据，防止有人退会，导致战报信息缺失
    put(?cache_fighter_list_self,Members),
    put(?cache_fighter_list_enemy,FamilyMatch#family_match.fighter_info_list),
?INFO("L-Debug ~w \n ~w",[Members,FamilyMatch#family_match.fighter_info_list]),
	{noreply, State#state{family_info=FamilyInfo2}};

%% last_world_rank 在sync_period_result的时候已经更新了,这里直接记录下来就可以了
%% 该消息同步比赛各个阶段时间至family_server
do_handle_info({clean_up_old_fight_info, Period,StartTime,PrepareEndTime, RandomEndTime,FightEndTime,PeriodEndTime}, #state{family_info=FamilyInfo}=State)->
	#family_info{members=Members,family_fight_other_info=FamilyFightInfo}=FamilyInfo,
	Members2 = lists:foldl(fun(E,Acc)->
								   E2 = E#family_member_info{is_join_war=0,attack_times=0,defend_times=0,win_star=0},
								   [E2|Acc]
						   end,[],Members),
	FamilyInfo2 = FamilyInfo#family_info{members=Members2,family_fight_other_info=#family_fight_other_info{war_period=Period
															,last_world_rank=FamilyFightInfo#family_fight_other_info.last_world_rank}},
	State2=State#state{family_fight_hist=[],family_match=#family_match{},family_info=FamilyInfo2
                      ,startTime=StartTime,prepareEndTime=PrepareEndTime
                      ,randomEndTime=RandomEndTime,fightEndTime=FightEndTime,periodEndTime=PeriodEndTime},
	{noreply, State2};

do_handle_info({random_match_fighters,PF_fighter},#state{family_info=FamilyInfo}=State)->
%% ?ERR("L-random_match_fighters ~w",[PF_fighter]),
	#family_info{family_fight_other_info=FamilyFightOtherInfo,members=Members}=FamilyInfo,
	FamilyMatch=#family_match{server_id=PF_fighter#pf_fighter.matchServerID,family_id=PF_fighter#pf_fighter.matchFamilyID,family_name=[]},
    ?INFO("得到匹配信息~w",[FamilyMatch]),
	#pf_fighter{result=Result,matchFamilyID=MatchFamilyID, matchServerID=MatchServerID,matchFamilyName=MatchFamilyName,matcherRank=MatchRank}=PF_fighter,
	FamilyFightOtherInfo2 =
		FamilyFightOtherInfo#family_fight_other_info{fight_result=Result,matcher_win_star=0,matcher_server_id=MatchServerID
								 ,matcher_family_id=MatchFamilyID,matcher_family_name=MatchFamilyName,matcher_family_rank=MatchRank},
    MembersT = util:random_list(Members),
	MaxFighterNum = data_family_fight:get(signMaxNum),
%% 战斗力越大的排号值越大,不参加的值为0,发送到客户端的时候,直接按is_join_war排序后逆序
	{Members2,_} = lists:foldl(fun(Member, {Acc,Count})->
									   case Count of
										   MaxFighterNum->
											   {[Member#family_member_info{attack_times=0,defend_times=0,win_star=0,is_join_war=0}|Acc],MaxFighterNum};
										   _ ->
											   {[Member#family_member_info{attack_times=0,defend_times=0,win_star=0,is_join_war=Count+1}|Acc],Count+1}
									   end
							   end, {[],0}, MembersT),
	FamilyInfo2=FamilyInfo#family_info{family_fight_other_info=FamilyFightOtherInfo2,members=Members2},
    case Result of
        1 ->
            %% 发送轮空奖励
            Reward = data_family:get(family_fight_bye_reward),
            lists:foreach(fun(#family_member_info{role_id=RoleID}) ->
                            mail_server:send_sys_mail(RoleID, ?MAIL_FAMILYFIGHT_BYE_REWARD, [], "", Reward)
                          end, Members);
        _ ->
            ignore
    end,

%% ?ERR("L-random_match_fighters family_match:~w",[FamilyMatch]),
	{noreply,State#state{family_match=FamilyMatch,family_info=FamilyInfo2}};

do_handle_info({familyfight_sign_succ,RoleID},#state{family_info=FamilyInfo}=State)->
	#family_info{family_fight_other_info=FamilyFightInfo}=FamilyInfo,
	FamilyFightInfo2 = FamilyFightInfo#family_fight_other_info{is_sign=1},
        case lists:keyfind(RoleID, #family_member_info.role_id, FamilyInfo#family_info.members) of
            #family_member_info{role_name=RoleName,role_level=RoleLevel,head=Head,title=Title,is_male = IsMale} ->
                NewLog = rebuild_family_log(RoleID,RoleName,Head,RoleLevel,?MAMILY_LOG_TYPE_SIGN_UP,"", FamilyInfo#family_info.log_data,Title,IsMale);
            false ->
                NewLog = FamilyInfo#family_info.log_data
        end,
	FamilyInfo2 = FamilyInfo#family_info{family_fight_other_info=FamilyFightInfo2, log_data = NewLog},
	do_bc_msg_except(#sc_familyfight_update_state_info{type=1},[]),
	{noreply, State#state{family_info=FamilyInfo2}};

do_handle_info({update_match_family_name, FamilyID, NewName}, #state{family_info=FamilyInfo}=State) ->
    #family_info{family_fight_other_info=FamilyFightOtherInfo}=FamilyInfo,
    #family_fight_other_info{matcher_family_id=FamilyIDT}=FamilyFightOtherInfo,
    case FamilyID =:= FamilyIDT of
        false ->
            NewState = State;
        true ->
            NewFightOtherInfo = FamilyFightOtherInfo#family_fight_other_info{matcher_family_name=NewName},
            NewFamilyInfo = FamilyInfo#family_info{family_fight_other_info=NewFightOtherInfo},
            NewState = State#state{family_info=NewFamilyInfo}
    end,
    {noreply, NewState};

do_handle_info({cs_familyBoss_hatch_egg,Pos,RoleID}=Msg,#state{family_info=FamilyInfo}=State)->
	do_hatch_family_boss_egg(RoleID,FamilyInfo,Msg,Pos),
	{noreply,State};
do_handle_info({cs_familyBoss_feed_boss, _Pos,_RoleID}=Msg, #state{family_info=FamilyInfo}=State)->
	do_feed_family_boss(Msg,FamilyInfo),
	{noreply,State};
do_handle_info({cs_familyBoss_set_boss_time,_Pos,_Time,RoleID}=Msg,#state{family_info=FamilyInfo}=State)->
	do_set_family_boss_time(RoleID,FamilyInfo,Msg),
	{noreply,State};
do_handle_info({cs_role_family_boss_attack_boss,_Pos,RoleID,_FighterInfo,_RoleName}=Msg,#state{family_info=FamilyInfo}=State) ->
	do_family_boss_attack(RoleID,FamilyInfo,Msg),
	{noreply,State};
	

do_handle_info({cs_family_owner_impeach,RoleID},#state{family_info=FamilyInfo}=State) ->
	case check_family_owner_impeach(RoleID,FamilyInfo) of
		{true,OwnerRoleID} ->
				FamilyInfo2 = do_family_owner_impeach(OwnerRoleID,RoleID,FamilyInfo),
				State2=State#state{family_info=FamilyInfo2},
				?unicast(RoleID,#sc_family_owner_impeach{result=1});
		{false,Reason} ->
			State2=State,
			?unicast(RoleID,#sc_family_owner_impeach{result=Reason})
	end,
	{noreply,State2};

do_handle_info({cs_family_impeach_list,RoleID},#state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members}=FamilyInfo,
    case lists:keyfind(RoleID,#family_member_info.role_id,Members) of
        false ->
            ?unicast(RoleID,#sc_family_impeach_list{result=2,impeachmemberlist=[]});
        _ ->
            case check_family_owner_can_impeach(FamilyInfo) of 
                true ->
                    ?unicast(RoleID,#sc_family_impeach_list{result=1,impeachmemberlist=get_contribution_top_n(FamilyInfo)});
                false ->
                    ?unicast(RoleID,#sc_family_impeach_list{result=1,impeachmemberlist=[]})
            end
    end,
    {noreply,State};


%% do_handle_info({timer_wheel_tick, LastTick}, State) ->
%% 	timer_wheel:work(LastTick),
%% 	{noreply, State};

do_handle_info({cs_family_storage_req, ItemUID,RoleID},State=#state{family_info=FamilyInfo})->
	#family_info{members=Members}=FamilyInfo,
	FamilyInfo2=
		case check_can_req_storage(RoleID,ItemUID,Members) of 
			{true,Item,Other,ReqInfo,Member,OtherMembers}->
				Members2 = do_req_storage(RoleID,Item,Other,ReqInfo,Member,OtherMembers),
				FamilyInfo#family_info{members=Members2};
			{false,Reason}->
				?unicast(RoleID,#sc_family_storage_req{result=Reason}),
				FamilyInfo
		end,
	{noreply,State#state{family_info=FamilyInfo2}};

do_handle_info({cs_family_storage_assign, ItemUID,TarRoleID,RoleID},State=#state{family_info=FamilyInfo})->
	#family_info{owner_role_id=Owner,members=Members}=FamilyInfo,
	FamilyInfo2 = 
	case check_can_assign_storage(ItemUID,TarRoleID,RoleID,Owner,Members) of
		{true,Storage,ReqInfo,Member,OtherMembers} ->
			Members2 = do_assign_storage(ItemUID,TarRoleID,RoleID,	Storage,ReqInfo,Member,OtherMembers),
			FamilyInfo#family_info{members=Members2};
		{false, Reason}->
			?unicast(RoleID,#sc_family_storage_assign{result=Reason}),
			FamilyInfo
	end,
	{noreply,State#state{family_info=FamilyInfo2}};

do_handle_info({family_extra,Req}, #state{family_info=FamilyInfo} = State)->
	family_extra_handler:handle_info(Req,FamilyInfo),
	{noreply,State};

do_handle_info(start_boss,#state{family_info=_FamilyInfo}=State)->
	%spawn(fun()->family_misc:router_to_family_boss_process(FamilyInfo#family_info.family_id, start_boss) end),
	{noreply,State};

%% do_handle_info({add_family_log,AddLog},#state{family_info=FamilyInfo}=State)->
%% %% 	{noreply,State#state{family_info=FamilyInfo#family_info{log_data=rebuild_family_log(AddLog,FamilyInfo#family_info.log_data)}}};
%%     {noreply,State};

do_handle_info({add_family_exp,RoleID,ExpAdd},#state{family_info=FamilyInfo}=State)->
	%% 给公会成员加活跃度
	Members = FamilyInfo#family_info.members,
	NewMembers = case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        {value, Member, Members2} ->
            OldFamilyLastContribution = Member#family_member_info.left_family_contribution,
			NewFamilyContribution = Member#family_member_info.family_contribution + ExpAdd,
			NewFamilyLastContribution = OldFamilyLastContribution + ExpAdd,

            LogPrama1008 = data_family:get(log_prama_1008_1),
            LogPrama1009 = data_family:get(log_prama_1009_1),
            LogPrama1010 = data_family:get(log_prama_1010_1),
            
            if
                NewFamilyLastContribution >= LogPrama1008
                  andalso OldFamilyLastContribution < LogPrama1008 ->
                    NewLog = rebuild_family_log(Member#family_member_info.role_id,
                                    Member#family_member_info.role_name,
                                    Member#family_member_info.head,
                                    Member#family_member_info.role_level,?MAMILY_LOG_TYPE_CONTRIBUTE_10,
                                    integer_to_list(NewFamilyLastContribution), FamilyInfo#family_info.log_data,
                                    Member#family_member_info.title,
                                    Member#family_member_info.is_male);
                NewFamilyLastContribution >= LogPrama1009
                  andalso OldFamilyLastContribution < LogPrama1009  ->
                    NewLog = rebuild_family_log(Member#family_member_info.role_id,
                                    Member#family_member_info.role_name,
                                    Member#family_member_info.head,
                                    Member#family_member_info.role_level,?MAMILY_LOG_TYPE_CONTRIBUTE_50,
                                    integer_to_list(NewFamilyLastContribution), FamilyInfo#family_info.log_data,
                                    Member#family_member_info.title,
                                    Member#family_member_info.is_male);
                NewFamilyLastContribution >= LogPrama1010
                  andalso OldFamilyLastContribution < LogPrama1010  ->
                    NewLog = rebuild_family_log(Member#family_member_info.role_id,
                                    Member#family_member_info.role_name,
                                    Member#family_member_info.head,
                                    Member#family_member_info.role_level,?MAMILY_LOG_TYPE_CONTRIBUTE_100,
                                    integer_to_list(NewFamilyLastContribution), FamilyInfo#family_info.log_data,
                                    Member#family_member_info.title,
                                    Member#family_member_info.is_male);
                true ->
                    NewLog = FamilyInfo#family_info.log_data
            end,
            [Member#family_member_info{family_contribution=NewFamilyContribution,left_family_contribution = NewFamilyLastContribution}|Members2];
        false ->
            NewLog = FamilyInfo#family_info.log_data,
            Members
	end,
	%% 给公会加经验
	NewFamilyInfo = check_exp_to_up_level(ExpAdd,FamilyInfo#family_info{members = NewMembers, log_data=NewLog}),
	{noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info({cs_family_worship,Type,RoleID,NeedCost},#state{family_info=FamilyInfo}=State) ->
   #family_info{members=Members,level=FamilyLevel} = FamilyInfo,
   case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
            ?unicast(RoleID, #sc_family_worship{type=Type,result=2,task_id=0});
        #family_member_info{reward_level=RewardValue} ->
            case RewardValue >= data_family_daily_reward:get({worship_times,FamilyLevel}) of
                true ->
                    ?unicast(RoleID, #sc_family_worship{type=Type,result=3,task_id=0});
                false ->
                    case random_worship_task(FamilyLevel) of 
                        {true, TaskID} ->
                            catch role_lib:send_server(RoleID, {add_family_worship_task,Type, NeedCost, TaskID}),
                            ?unicast(RoleID, #sc_family_worship{type=Type,result=1,task_id=TaskID});
                        _ ->
                            ?unicast(RoleID, #sc_family_worship{type=Type,result=4,task_id=0})
                    end
            end
   end,
   {noreply, State};

do_handle_info({cs_family_worshop_limit,RoleID},#state{family_info=FamilyInfo}=State)->
	#family_info{level=FamilyLevel} = FamilyInfo,
	?unicast(RoleID,#sc_family_worship_limit{member=data_family_daily_reward:get({worship_times,FamilyLevel})
											,family=data_family_daily_reward:get(total_worship_times)
											,cost=data_family_daily_reward:get(worship_task_refresh_cost)}),
	{noreply,State};

do_handle_info({commit_family_worship_task, RoleID}, #state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members,level=FamilyLevel} =FamilyInfo,
    case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        false ->
            NewState = State,
            ?ERR("玩家~p修改膜拜次数时出错,该公会没有该玩家!,公会信息:~p.~n", [RoleID,FamilyInfo]);
        {value, #family_member_info{reward_level=Times}=RoleInfo, List} ->
            NewMembers = [RoleInfo#family_member_info{reward_level=Times+1}|List],
            NewFamilyInfo = FamilyInfo#family_info{members=NewMembers},
            NewState = State#state{family_info=NewFamilyInfo},
            ?unicast(RoleID, #sc_family_worship_self_push{times=Times+1}),
             {TotalTimes,RewardValue,_RewardMembers} = get_worship_reward_info(NewMembers,FamilyLevel), 
            do_bc_msg_except(#sc_family_worship_family_push{times=TotalTimes,reward=RewardValue},[])
    end,
    {noreply, NewState};

do_handle_info({cs_family_worship_fight,RoleID,PosList,LieuAddAttr,TalentList,TrSpecial}, #state{family_info=FamilyInfo}=State) ->
   #family_info{members=Members,level=FamilyLevel} = FamilyInfo,
    case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
            ?unicast(RoleID, #sc_family_worship_fight{result=3,reward=0});
        _ ->
            case get_fight_dungeonid(FamilyLevel) of
                false ->
                    ?ERR("公会挑战钻石兽难度配置出错!"),
                    ?unicast(RoleID, #sc_family_worship_fight{result=4,reward=0});
                DungeonId ->
                    do_family_worship_fight(RoleID,PosList,LieuAddAttr,TalentList,TrSpecial,DungeonId) 
            end
    end,
    {noreply, State};

do_handle_info({family_worship_fight_succ, RoleID, Result, FightRecord, NewMonList, TotalHp}, State) ->
    CurHp = lists:foldl(fun(#ger{gerHp=HP},Acc) ->
                            Acc + HP 
                        end, 0, NewMonList),
    % 这个是伤害百分比
    Percent = max(0,(1 - CurHp / TotalHp)) * 100,
    %%?ERR("挑战钻石兽,当前血量:~p,总血量:~p,伤害百分比:~p.~n",[CurHp,TotalHp,Percent]),
    case data_family_daily_reward:get(worship_fight_reward) of
        undefined ->
            %%?ERR("钻石兽伤害奖励配置出错");
            ignore;
        [] ->
            %%?ERR("钻石兽伤害奖励配置出错");
            ignore;
        List ->
            Rewards = lists:reverse(List),
            case util:fun_take(fun({Damage,_}) ->
                                Percent >= Damage end, Rewards) of
                {value, {_,Reward}, _} ->
                    next;
                false ->
                    {_,Reward} = hd(List)
            end,
             
            %%?ERR("挑战钻石兽奖励:~p.~n",[Reward]),
            case Result of 
                true ->
                    ?unicast(RoleID, #sc_family_worship_fight{result=1,fightInfo=[FightRecord],reward=Reward});
                false ->
                    ?unicast(RoleID, #sc_family_worship_fight{result=2,fightInfo=[FightRecord],reward=Reward})
            end,

            % 通知role发送奖励、触发任务
            role_lib:send_server(RoleID, {role_family_worship_fight_succ, Reward})
    end,
    {noreply, State};

do_handle_info({cs_changename_freetimes,RoleID,Type}, #state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members, leftChgNameTimes=LeftChgNameTimes} = FamilyInfo,
    case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
            ?unicast(RoleID, #sc_changename_freetimes{type=Type,times=-1});
        _ ->
            case LeftChgNameTimes > 0 of
                true -> 
                    ?unicast(RoleID, #sc_changename_freetimes{type=Type,times=LeftChgNameTimes});
                _ ->
                    {LeftTimes, Extra} = case data_change_name:get({change_cost, Type}) of
                                            undefined ->
                                                {-1, []};
                                            {_, TypeID, NeedNum} ->
                                                {0, [TypeID,NeedNum]}
                                        end,
                    ?unicast(RoleID, #sc_changename_freetimes{type=Type,times=LeftTimes,extra=Extra})
            end
    end,
    {noreply, State};

do_handle_info({cs_changename_check,RoleID,Type,Name}, #state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members, leftChgNameTimes=LeftChgNameTimes} = FamilyInfo,
    case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
            ?unicast(RoleID, #sc_changename{type=Type,result=2});
        #family_member_info{family_title=Title} ->
            HavePower = case Title of 
			                ?FAMILY_TITLE_OWNER ->
			                    true;
			                ?FAMILY_TITLE_DEPUTY ->
			                    true;
			                _ ->
			                    false
                        end,   
            case HavePower of
                false ->
                    ?unicast(RoleID, #sc_changename{type=Type,result=6});
                true ->
                    %%?ERR("角色;~p修改公会名,权限验证成功,新名:~p.~n", [RoleID, Name]),
                    role_lib:send_server(RoleID, {role_family_changename_check_succ, Type, LeftChgNameTimes, Name})
            end
    end,
    {noreply, State};

do_handle_info({do_change_family_name, RoleID, Type, Name, List}, #state{family_info=FamilyInfo}=State) ->
    #family_info{leftChgNameTimes=LeftChgNameTimes,family_id=FamilyID} = FamilyInfo,
    case db_sql:update_family_name(FamilyID,Name) of
        false ->
            %% 操作失败, 将玩家丢失的道具加回来
            role_lib:send_server(RoleID, {role_family_changename_fail, Type, 3, List}),
            ?ERR("玩家:~p,修改公会:~p名称失败,消耗:~p.~n",[RoleID,FamilyID,List]),
            NewState = State;
        true ->
            %%?ERR("公会改名成功.~n",[]),
            ?unicast(RoleID, #sc_changename{type=Type, result=0}),
            NewInfo = FamilyInfo#family_info{leftChgNameTimes=max(0,LeftChgNameTimes-1),family_name=Name},
            lists:foreach(fun(E) ->
                            erlang:send(E, {update_family_name, FamilyID, Name})
                          end, [family_fight_server, family_manager_server]),
            NewState=State#state{family_info=NewInfo}
        end,
    {noreply, NewState};
        
do_handle_info({cs_family_worship_info,RoleID}, #state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members,level=FamilyLevel} =FamilyInfo,
    case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
             ?unicast(RoleID, #sc_family_worship_info{self=0,family=0,reward=0});
        #family_member_info{reward_level=RewardLevel} ->
             {TotalTimes,RewardValue,_RewardMembers} = get_worship_reward_info(Members,FamilyLevel), 
             ?unicast(RoleID, #sc_family_worship_info{self=RewardLevel,family=TotalTimes,reward=RewardValue})
    end,
    {noreply, State};

do_handle_info({update_role_name, RoleID, Name}, #state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members,family_id=FamilyID, create_role_id=CreateRoleID,owner_role_id=OwnerRoleID}=FamilyInfo,
    case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        false ->
            NewState = State;
        {value,Info,OtherMembers} ->
            case db_sql:update_family_member_name(FamilyID,RoleID,Name) of
                false ->
                    NewState = State;
                true ->
                    NewMembers = [Info#family_member_info{role_name=Name}|OtherMembers],
                    NewFamilyInfo3 = FamilyInfo#family_info{members=NewMembers},
                    case CreateRoleID =:= RoleID of
                        false ->
                            NeedToManager1 = false,
                            NewFamilyInfo2 = NewFamilyInfo3;
                        true ->
                            NeedToManager1 = true,
                            NewFamilyInfo2 = NewFamilyInfo3#family_info{create_role_name=Name}
                    end,
                    case OwnerRoleID =:= RoleID of
                        false ->
                            NeedToManager2 = false,
                            NewFamilyInfo = NewFamilyInfo2;
                        true ->
                            NeedToManager2 = true,
                            NewFamilyInfo = NewFamilyInfo2#family_info{owner_role_name=Name}
                    end,
                    NewState = State#state{family_info=NewFamilyInfo},  
                    %%创始人或则会长改名,需要通知family_manager_server
                    case NeedToManager1 orelse NeedToManager2 of
                        true ->
                            erlang:send(family_manager_server,{update_family_role_name, FamilyID, RoleID, Name});
                        _ ->
                            ignore
                    end
                end
    end,
    {noreply, NewState};

%% 游戏进程触发任务状态变化，发送最新任务状态给role_server
do_handle_info({family_dispach_task, TriggerID,TriggerData}, #state{family_info=FamilyInfo}=State) ->
    TaskList = FamilyInfo#family_info.family_task, %% TaskList 类型是[#r_task{}]
    case family_disapach_task(TriggerID,TriggerData,TaskList) of
        ignore ->
            NewFamilyInfo = FamilyInfo;
        NewTaskList ->
            NewFamilyInfo = FamilyInfo#family_info{family_task=NewTaskList}
    end,
    if 
        NewFamilyInfo#family_info.family_task /= TaskList->
            sync_family_task(NewFamilyInfo#family_info.family_task);
        true ->
            ignore
    end,
    {noreply, State#state{family_info=NewFamilyInfo}};

%% 客户端请求任务信息
do_handle_info({request_task, RoleID}, #state{family_info=FamilyInfo}=State) ->
    case role_lib:is_online(RoleID) of
        true ->
            FamilyTask = FamilyInfo#family_info.family_task,
            FamilyTask2 = [E||E<-FamilyTask,?undefined =/= data_task:get(E#r_task.task_id)],
            catch role_lib:send_server(RoleID, {sync_family_task, FamilyTask2});
        _ ->
            next
    end,
    {noreply, State};

do_handle_info({role_change_head, RoleID, Head}, #state{family_info=FamilyInfo}=State)->
	#family_info{members=Members,log_data=LogData}=FamilyInfo,
	State2 = 
	case lists:keytake(RoleID, #family_member_info.role_id, Members) of
		false ->
			State;
		{value,Member,OtherMembers} ->
			NewMembers = [Member#family_member_info{head=Head}|OtherMembers],
			NewLogData = lists:foldr(fun(#p_family_log_dtl{roleid=RoleID2}=D,Acc)-> case RoleID2 of RoleID -> [D#p_family_log_dtl{rolehead=Head}|Acc]; _ ->[D|Acc] end end, [], LogData),
			NewFamilyInfo = FamilyInfo#family_info{members=NewMembers,log_data=NewLogData},
			State#state{family_info=NewFamilyInfo}
	end,
	{noreply, State2};

do_handle_info({update_cross_rank, Rank}, #state{family_info=FamilyInfo}=State) ->
    NewFamilyInfo = FamilyInfo#family_info{cross_rank=Rank},
    {noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info(family_disbanded, #state{family_info=FamilyInfo}=State) ->
	do_disband(FamilyInfo,FamilyInfo#family_info.owner_role_id),
	{stop, normal, State};
do_handle_info({fix_familyTek_level,FamilyID,TekID,Level},#state{family_info=FamilyInfo}=State)->
    #family_info{members=Members}=FamilyInfo,
    FamilyTekInfoList = family_data:get_family_tek(),
    case lists:keytake(TekID,#p_familyTekDtl.tekID,FamilyTekInfoList) of
        false ->
            ?ERR("can not find the TekID :~w ~n",[TekID]);
        {_,FindTekInfo,Other} ->
            NewTekInfo = FindTekInfo#p_familyTekDtl{tekID = (TekID div 1000)*1000 + Level,tekLevel = Level,tekWalletinfo = #p_reward_info2{coin=0,roleExp=0,gerExp=0,gold=0,reputation=0,itemList=[],gerList=[]}},
            NewTekInfoList =[NewTekInfo|Other],
            family_data:set_family_tek(NewTekInfoList), 
            family_server:do_bc_msg_except(#sc_familyTek_upLevel{result=1, tekID=(TekID div 1000)*1000 + Level,level=Level}, []),
            TekList =family_data:get_family_tek(),
            %%将新的公会科技等级发送到family_manager_server的共享ETS表中，更新其中的结果。
            TekLevelList = lists:map(fun(TekDetail) ->
            #p_familyTekDtl{tekID = NewTekID ,tekLevel = NewTekLevel} = TekDetail,
            {NewTekID,NewTekLevel}
            end,TekList),
            ets:insert(?ETS_FAMILY_TECHNOLOGY,{FamilyID,TekLevelList}),
            spawn(fun()->family_extra_handler:update_member_tekAttr(Members)end)
    end,
    {noreply,State};

%%处理获取玩家捐献数据
do_handle_info({do_get_family_donate_contribute_list,RoleID,TargetRoleID},#state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members} = FamilyInfo,
    case lists:keyfind(TargetRoleID,#family_member_info.role_id,Members) of
        false ->
            ?unicast(RoleID, #sc_family_get_donate_contribute_list{result=3,roleID=TargetRoleID,donateInfo=family_contribution_to_donateInfo(#family_contribution{})});
        _ ->
            case family_data:get_family_contribute_list() of
                [] ->
                   ?unicast(RoleID, #sc_family_get_donate_contribute_list{result=1,roleID=TargetRoleID,donateInfo=family_contribution_to_donateInfo(#family_contribution{})}); 
                Family_donate_contribution_list->
                    case lists:keyfind(TargetRoleID,#family_contribution.role_id,Family_donate_contribution_list) of
                        false ->
                            ?unicast(RoleID, #sc_family_get_donate_contribute_list{result=1,roleID=TargetRoleID,donateInfo=family_contribution_to_donateInfo(#family_contribution{})});
                        Family_donate_contribution_info ->
                            case is_record(Family_donate_contribution_info,family_contribution) of
                                true ->
                                    ?unicast(RoleID, #sc_family_get_donate_contribute_list{result=1,roleID=TargetRoleID,donateInfo=family_contribution_to_donateInfo(Family_donate_contribution_info)}); 
                                false ->
                                    ?INFO("发现Family_donate_contribution_info：~w 与family_contribution不符~n",[Family_donate_contribution_info]),
                                    ?unicast(RoleID, #sc_family_get_donate_contribute_list{result=4,roleID=TargetRoleID,donateInfo=family_contribution_to_donateInfo(#family_contribution{})})
                            end
                    end
            end
    end,
    {noreply,State};

%%处理获取玩家捐献概要数据
do_handle_info({cs_family_donate_contribution_summary,RoleID},#state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members} = FamilyInfo,
    case lists:keyfind(RoleID,#family_member_info.role_id,Members) of
        false ->
            ?unicast(RoleID,#sc_family_donate_contribution_summary{result=2,donate_summary=[]});
        _ ->
            DonateSummary = get_donate_summary(FamilyInfo),
            ?unicast(RoleID,#sc_family_donate_contribution_summary{result=1,donate_summary=DonateSummary})
    end,
    {noreply,State};

do_handle_info({add_contribute_record, RoleID,_,_,_}=Msg,#state{family_info=FamilyInfo}=State)->
    #family_info{members=Members} = FamilyInfo,
    case lists:keyfind(RoleID,#family_member_info.role_id,Members) of
        false ->
            ?INFO("添加捐献记录时，出现公会成员：~w 不在公会：~w ~n",[RoleID,FamilyInfo]);
        _ ->
            Family_contribution_info =  family_data:get_family_contribute_list(),
            family_data:set_family_contribute_list(update_contribute_list(Family_contribution_info,Msg))
    end,
    {noreply,State};

do_handle_info({test_sync_info,ST,PT,RT,FT,ET}, State) ->
    ?INFO("test_sync_info ~w ~w ~w ~w ~w",[ST,PT,RT,FT,ET]),
    {noreply, State#state{startTime=ST,prepareEndTime=PT,randomEndTime=RT,fightEndTime=FT,periodEndTime=ET}};

do_handle_info({cs_family_invite_request, SendRoleID, TarRoleID, FamilyID}, #state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members,family_name=FamilyName} = FamilyInfo,
    #rolePublic{roleName=RoleName}=role_lib:get_rolePublic(SendRoleID),
    case check_invite_request(SendRoleID,TarRoleID,Members,FamilyID) of
        ok ->
            ets:insert(?ETS_FAMILY_INVITE, {{TarRoleID,FamilyID}}),
            mail_server:send_family_invite_mail(SendRoleID, RoleName, TarRoleID,FamilyID,FamilyName),
            ?unicast(SendRoleID,#sc_family_invite_request{result=1});
        {fail,Reason}->
            ?unicast(SendRoleID,#sc_family_invite_request{result=Reason})
    end,
    {noreply, State};
do_handle_info({cs_familyfight_instance_open_state,RoleID,VipLevel}, #state{family_info=FamilyInfo}=State) ->
    NeedFamilyLevel = data_family:get(family_instance_family_level),
    #data_vip{familyInstanceTimes =FamilyInstanceTimes}=data_vip:get(VipLevel),
    if
        FamilyInfo#family_info.level < NeedFamilyLevel ->
            ?INFO("family level is too low ~w",[FamilyInfo#family_info.level]),
            ?unicast(RoleID,#sc_familyfight_instance_open_state{state_list=[],attack_times=0,buy_price=0,buy_price_times=0,next_instance_id=0,is_have_reward=0});
        true ->
            Msg = get_family_inst_msg(RoleID,FamilyInstanceTimes,FamilyInfo#family_info.family_instance_state),
            ?unicast(RoleID,Msg)
    end,
    {noreply, State};
do_handle_info({cs_familyfight_attack_boss,RoleID, BossIndex, FighterList, LieuAdd, TalentList,TrSpecial,VipLevel,RoleName}, #state{family_info=FamilyInfo}=State) ->
    FamilyInstanceInfo = FamilyInfo#family_info.family_instance_state,
    InstBossList = FamilyInstanceInfo#family_instance_state.cur_inst_boss, 
    NeedFamilyLevel = data_family:get(family_instance_family_level),
    BossListLen = length(InstBossList),
    {IsCanFight,NewTimesList} = check_family_instance_fight_times(RoleID,FamilyInstanceInfo#family_instance_state.fight_member_times,VipLevel),
    BlankReward = #p_reward_info{coin=0
                               ,roleExp=0
                               ,gerExp=0
                               ,gold=0
                               ,reputation=0
                               ,itemList=[]
                               ,gerList=[]},
    R = lists:keytake(?F_INST_FIGHT, 2, FamilyInstanceInfo#family_instance_state.inst_list),
    NewState = if
        FamilyInfo#family_info.level < NeedFamilyLevel ->
            ?unicast(RoleID,#sc_familyfight_attack_boss{result=6,fightInfo=[],isKillBoss=0
                                                 ,fight_reward=BlankReward}),
            State;
        R =:= false ->
            ?unicast(RoleID,#sc_familyfight_attack_boss{result=6,fightInfo=[],isKillBoss=0
                                                 ,fight_reward=BlankReward}),
            State;
        BossListLen < BossIndex -> %boss序号错误
            ?unicast(RoleID,#sc_familyfight_attack_boss{result=4,fightInfo=[],isKillBoss=0
                                                 ,fight_reward=BlankReward}),
            State;
        IsCanFight =:= false -> %战斗次数不足
            ?unicast(RoleID,#sc_familyfight_attack_boss{result=5,fightInfo=[],isKillBoss=0
                                                 ,fight_reward=BlankReward}),
            State;
        true ->
            {value,InstBossInfo,OtherInstBossList} = lists:keytake(BossIndex, #inst_boss_info.index, InstBossList),
            CurHp = InstBossInfo#inst_boss_info.cur_hp,
            if
                CurHp =< 0 -> %boss已经死了不能打
                    ?unicast(RoleID,#sc_familyfight_attack_boss{result=3,fightInfo=[],isKillBoss=0
                                                 ,fight_reward=BlankReward}),
                    State;
                true -> %boss活着，可以打
                    ?INFO("cs_familyfight_attack_boss~nRoleID:~w~nCurHp:~w~nBossList:~w~nFighterList:~w~nLieuAdd:~w~nTalentList:~w~n"
                                ,[RoleID, CurHp, InstBossInfo#inst_boss_info.boss_list,FighterList,LieuAdd,TalentList]),
                    BossList = InstBossInfo#inst_boss_info.boss_list,
                    {_Result,NewCurHp,NewBossList,FightRecord} = do_fight(RoleID,BossList,CurHp, FighterList,LieuAdd,TalentList,TrSpecial),
                    AddDamage = max(0,(CurHp-NewCurHp)),
                    NewInstBossInfoList = [InstBossInfo#inst_boss_info{cur_hp=NewCurHp,boss_list=NewBossList}|OtherInstBossList],
                    {value,{CurInstId,_},OtherInstList} = R,
                    MsgInstBossInfoList = lists:foldl(fun({_,Index,MaxHp,Hp,_BossList,_},AccList)-> 
                                                           [{instance_boss_info,Index,MaxHp,Hp}|AccList] 
                                                   end, [], NewInstBossInfoList),
                    {NewInstBossInfoList2,IsKillBoss,IsWin} = if
                        NewCurHp =< 0 -> % boss is death
                            L = [InstBossInfo#inst_boss_info{cur_hp=NewCurHp,killer_role_id=RoleID}|OtherInstBossList],
                            AliveBossLen = length([E||E<-L,E#inst_boss_info.cur_hp > 0]),
                            if
                                AliveBossLen =:= 0 -> {L,1,true}; %boss死亡，副本通关
                                true -> {L,1,false} %boss死亡，副本未通关
                            end;
                        true ->
                            {NewInstBossInfoList,0,false} %boss未死亡
                    end,
                    NewRoleDamage = case lists:keysearch(RoleID, #instance_damage_info.roleID, FamilyInstanceInfo#family_instance_state.damage_list) of
                                    {value,RoleDamage} when IsKillBoss =:= 1 ->
                                        RoleDamage#instance_damage_info{kill_num=RoleDamage#instance_damage_info.kill_num+1
                                                                       ,damage=RoleDamage#instance_damage_info.damage+AddDamage};
                                    {value,RoleDamage} when IsKillBoss =:= 0 ->
                                        RoleDamage#instance_damage_info{damage=RoleDamage#instance_damage_info.damage+AddDamage};
                                    false when IsKillBoss =:= 1 ->
                                        {instance_damage_info,RoleID,RoleName,1,AddDamage};
                                    false when IsKillBoss =:= 0 ->
                                        {instance_damage_info,RoleID,RoleName,0,AddDamage}
                                end,
                    NewDamageList = case lists:keytake(RoleID, #instance_damage_info.roleID, FamilyInstanceInfo#family_instance_state.damage_list) of
                                        {value, _, OtherDamageList} ->
                                            [NewRoleDamage|OtherDamageList];
                                        false ->
                                            [NewRoleDamage|FamilyInstanceInfo#family_instance_state.damage_list]
                                    end,
                    ?INFO(">>>>>>>>>>>>>>>>>>>>>>>>NewDamageList ~w",[NewDamageList]),
%%                     NewDamageList = lists:keyreplace(RoleID, #instance_damage_info.roleID, FamilyInstanceInfo#family_instance_state.damage_list,NewRoleDamage),
                    ?unicast(RoleID,#sc_familyfight_attack_boss{result=1,fightInfo=[FightRecord],isKillBoss=IsKillBoss
                                                               ,fight_reward=activity_server:sell_reward2p_reward_info(#sell_reward{})}),
                    NewInstList = if
                        IsWin =:= true ->
                            case lists:keymember(CurInstId+1, 1, OtherInstList) of
                                false ->
                                    [{CurInstId+1,?F_INST_OPEN}|[{CurInstId,?F_INST_WIN}|OtherInstList]];
                                true->
                                    [{CurInstId,?F_INST_WIN}|OtherInstList]
                            end;
                        true ->
                            FamilyInstanceInfo#family_instance_state.inst_list
                    end,
                    if
                        IsWin =:= true -> %副本通关后向
                            erlang:send(self(), {cs_familyfight_instance_open_state,RoleID,VipLevel}),
                            erlang:send(self(), {cs_familyfight_instance_reward_info,RoleID}),
							erlang:send(self(), {send_pass_chapter_reward, CurInstId});
                        true ->
                            ignore
                    end,
                    NewFamilyBossInfo =  FamilyInstanceInfo#family_instance_state{inst_list=NewInstList,cur_inst_boss = NewInstBossInfoList2,fight_member_times=NewTimesList,is_win=IsWin,damage_list=NewDamageList},
                    NewFamilyInfo = FamilyInfo#family_info{family_instance_state = NewFamilyBossInfo},
                    MsgInstBossInfoListSorted = lists:sort(fun({_,IndexA,_,_},{_,IndexB,_,_})-> IndexA =< IndexB end, MsgInstBossInfoList),
                    do_bc_msg_except(#sc_familyfight_instance_boss_info{instance_id=CurInstId,boss_list=MsgInstBossInfoListSorted}, []),
                    State#state{family_info=NewFamilyInfo}
            end
    end,
    {noreply, NewState};
do_handle_info({send_pass_chapter_reward, CurInstId},#state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	Reward = data_family:get({send_pass_chapter_reward, CurInstId}),
	#data_temp_mail{mailInfoList=[#mail_template{content=Content}]} = data_temp_mail:get(?MAIL_TYPE_FAMILY_PASS_CHAPTER),
	Content2 = re:replace(Content,"@\\(0\\)@",integer_to_list(CurInstId),[global,{return, binary}]),
	lists:foreach(fun(#family_member_info{role_id=RoleID})->
						  mail_server:send_mail(0,"",RoleID,?MAIL_TYPE_REWARD,?MAIL_TYPE_FAMILY_PASS_CHAPTER,[CurInstId],Content2,Reward)
						  end,Members),
	{noreply,State};
do_handle_info({cs_familyfight_instance_boss_info,RoleID}, #state{family_info=FamilyInfo}=State) ->
    FamilyInstanceInfo = FamilyInfo#family_info.family_instance_state,
    [CurInstId] = [InstId||{InstId,InstState}<-FamilyInstanceInfo#family_instance_state.inst_list,InstState /= ?F_INST_OPEN],
    InstBossInfoList = lists:foldl(fun({inst_boss_info,Index,MaxHp,CurHp,_BossList,_},AccList)-> 
                                           [{instance_boss_info,Index,MaxHp,CurHp}|AccList] 
                                   end, [], FamilyInstanceInfo#family_instance_state.cur_inst_boss),
    InstBossInfoListSorted = lists:sort(fun({_,IndexA,_,_},{_,IndexB,_,_})-> IndexA =< IndexB end, InstBossInfoList),
    ?unicast(RoleID,#sc_familyfight_instance_boss_info{instance_id=CurInstId,boss_list=InstBossInfoListSorted}),
    {noreply, State};
do_handle_info({cs_familyfight_select_instance,RoleID,SeleteId}, #state{family_info=FamilyInfo}=State) ->
    FamilyInstanceInfo = FamilyInfo#family_info.family_instance_state,
    IsHaveAuth = check_role_can_select_instance(get_member_title(RoleID,FamilyInfo#family_info.members)),
    Res1 = lists:keymember(SeleteId, 1, FamilyInstanceInfo#family_instance_state.inst_list),
    NewState = if
        IsHaveAuth =:= false ->
            ?unicast(RoleID,#sc_familyfight_select_instance{result=2}),
            State;
        Res1 =:= false ->
            ?unicast(RoleID,#sc_familyfight_select_instance{result=3}),
            State;
        true ->
            ?unicast(RoleID,#sc_familyfight_select_instance{result=1}),
            State#state{family_info=
                FamilyInfo#family_info{family_instance_state=
                    FamilyInstanceInfo#family_instance_state{next_instance=SeleteId}}}
    end,
    {noreply, NewState};
do_handle_info({add_attack_time_request,RoleID,VipLevel,AddTimes}, #state{family_info=FamilyInfo}=State) ->
    FamilyInstanceInfo = FamilyInfo#family_info.family_instance_state,
    {_NeedGold,_BuyTimes}=data_family:get(family_instance_buy_times_price),
    InitTime = data_family:get(family_instance_fight_times),
    TimesList = FamilyInstanceInfo#family_instance_state.fight_member_times,
    R = case lists:keytake(RoleID, 1, TimesList) of
        {value , {RoleID,AtkTimes,BugTimes} ,OtherTimesList} ->
            #data_vip{familyInstanceTimes =FamilyInstanceTimes}=data_vip:get(VipLevel),
            if
                BugTimes+AddTimes > FamilyInstanceTimes ->
                    false;
                true ->
                    {AtkTimes+AddTimes,[{RoleID,AtkTimes+AddTimes,BugTimes+AddTimes}|OtherTimesList]}
            end;
        false ->
            {InitTime+AddTimes,[{RoleID,InitTime+AddTimes,AddTimes}|TimesList]} %这个玩家今天还没攻击过，就买次数了
    end,
    case R of
        false ->
            ?unicast(RoleID,#sc_familyfight_bug_attack_time{result=4,new_times=0}),
            {noreply, State};
        {NewTimes,NewTimesList} ->
            NewFamilyInfo = FamilyInfo#family_info{family_instance_state=
                                FamilyInstanceInfo#family_instance_state{fight_member_times=NewTimesList}},
            role_lib:send_server(RoleID, {add_attack_time_done,NewTimes,AddTimes}),
            {noreply, State#state{family_info=NewFamilyInfo}}
    end;
do_handle_info({cs_familyfight_instance_get_reward,RoleID,SelectIndex,Name}, #state{family_info=FamilyInfo}=State) ->
    FamilyInstanceState = FamilyInfo#family_info.family_instance_state,
    RewardGetStatus = FamilyInstanceState#family_instance_state.reward_get_status,
    IsGet = lists:keymember(RoleID, 2, RewardGetStatus),
    BoxNum = data_family:get(family_instance_box_num),
    BlankReward = #p_reward_info{coin=0
                               ,roleExp=0
                               ,gerExp=0
                               ,gold=0
                               ,reputation=0
                               ,itemList=[]
                               ,gerList=[]},
            BlankInstReward = #instance_reward{roleName="",reward_info=BlankReward,reward_index=0,is_open=0},
    Res = if
        FamilyInstanceState#family_instance_state.is_win =:= false ->
            ?unicast(RoleID,#sc_familyfight_instance_get_reward{result=3,reward_detail=BlankInstReward}),
            fail;
        IsGet =:= true  andalso SelectIndex /= 1000  ->
            ?unicast(RoleID,#sc_familyfight_instance_get_reward{result=5,reward_detail=BlankInstReward}),
            fail;
        BoxNum < SelectIndex andalso SelectIndex /= 1000 ->
            ?unicast(RoleID,#sc_familyfight_instance_get_reward{result=4,reward_detail=BlankInstReward}),
            fail;
        true andalso SelectIndex =:= 1000 ->
            case lists:member(RoleID, FamilyInstanceState#family_instance_state.extra_reward_is_get) of
                false ->
                    [CurInstId] = [InstId||{InstId,InstState}<-FamilyInstanceState#family_instance_state.inst_list,InstState /= ?F_INST_OPEN],
                    FamilyExtraReward = data_family:get({family_extra_reward,CurInstId}),
                    InstRewardDetail = #instance_reward{roleName="",reward_info=activity_server:sell_reward2p_reward_info(FamilyExtraReward),reward_index=1000,is_open=0},
                    ?unicast(RoleID,#sc_familyfight_instance_get_reward{result=1,reward_detail=InstRewardDetail}),
                    role_lib:send_server(RoleID, {family_instance_boss_reward,FamilyExtraReward,?MONEY_ADD_TYPE_FAMILY_INSTANCE_EXTRA}),
                    ok;
                true ->
                    ?unicast(RoleID,#sc_familyfight_instance_get_reward{result=6,reward_detail=BlankInstReward}),
                    fail
            end;
        true ->
             case lists:keytake(SelectIndex, 1, RewardGetStatus) of
                 {value,{SelectIndex,0,Reward,_Name},OtherRewardGetStatus} ->
                    InstRewardDetail = #instance_reward{roleName=Name,reward_info=activity_server:sell_reward2p_reward_info(Reward),reward_index=SelectIndex,is_open=1},
                    ?unicast(RoleID,#sc_familyfight_instance_get_reward{result=1,reward_detail=InstRewardDetail}),
                    role_lib:send_server(RoleID, {family_instance_boss_reward,Reward,?MONEY_ADD_TYPE_FAMILY_INSTANCE_WIN}),
                    {ok,[{SelectIndex,RoleID,Reward,Name}|OtherRewardGetStatus]};
                 {value,_,_OtherRewardGetStatus} ->
                    ?unicast(RoleID,#sc_familyfight_instance_get_reward{result=4,reward_detail=BlankInstReward}),
                    fail
             end
    end,
    case Res of
        ok ->
            {noreply, State#state{family_info=
                        FamilyInfo#family_info{family_instance_state=
                            FamilyInstanceState#family_instance_state{extra_reward_is_get=[RoleID|FamilyInstanceState#family_instance_state.extra_reward_is_get]}}}};
        {ok,NewRewardGetStatus} ->
            lists:foreach(fun(R_ID) ->
                        erlang:send(self(), {cs_familyfight_instance_reward_info,R_ID})
                end, erlang:get(?online_role_id_list)),
            {noreply, State#state{family_info=
                        FamilyInfo#family_info{family_instance_state=
                            FamilyInstanceState#family_instance_state{reward_get_status=NewRewardGetStatus}}}};
        fail ->
            {noreply, State}
    end;
do_handle_info({cs_familyfight_instance_reward_info,RoleID}, #state{family_info=FamilyInfo}=State) ->
    FamilyInstanceState = FamilyInfo#family_info.family_instance_state,
    ?INFO("cs_familyfight_instance_reward_info FamilyInstanceState:~w",[FamilyInstanceState]),
    RewardGetStatus = FamilyInstanceState#family_instance_state.reward_get_status,
    IsWin = FamilyInstanceState#family_instance_state.is_win,
    IsGetExtra = lists:member(RoleID, FamilyInstanceState#family_instance_state.extra_reward_is_get),
    IsGet = case lists:keymember(RoleID, 2, RewardGetStatus) of
                true when IsGetExtra =:= true->
                    4; %4通关，奖励都领取了。
                true ->
                    2; %2通关，宝箱已领取，
                false when IsWin =:= true andalso IsGetExtra =:= true->
                    3; %3，通关，公会货币已领取，
                false when IsWin =:= true ->
                    1; %1通关，奖励都未领取，
                false when IsWin =:= false ->
                    0 %0没通关，
            end,
    [CurInstId] = [InstId||{InstId,InstState}<-FamilyInstanceState#family_instance_state.inst_list,InstState /= ?F_INST_OPEN],
    FamilyExtraReward = data_family:get({family_extra_reward,CurInstId}),
    WinRewardList = [{instance_reward,Name,Index,activity_server:sell_reward2p_reward_info(Reward),bool_to_byte(ID)}||{Index,ID,Reward,Name}<-RewardGetStatus],
    WinRewardListSorted = lists:sort(fun({_,_,IndexA,_,_},{_,_,IndexB,_,_})-> IndexA =< IndexB end, WinRewardList),
    DamageList = lists:sort(fun(DA,DB)->
                                    DA#instance_damage_info.damage > DB#instance_damage_info.damage
                            end, FamilyInstanceState#family_instance_state.damage_list),
    DamageListAll = lists:reverse(
                      lists:foldl(fun(Member,DamageListAcc)-> 
                                          case lists:keymember(Member#family_member_info.role_id, #instance_damage_info.roleID, DamageListAcc) of
                                              true ->
                                                  DamageListAcc;
                                              false ->
                                                  [{instance_damage_info
                                                   ,Member#family_member_info.role_id
                                                   ,Member#family_member_info.role_name,0,0}|DamageListAcc]
                                          end
                                  end, lists:reverse(DamageList), FamilyInfo#family_info.members)),
    Msg = #sc_familyfight_instance_reward_info{is_get_reward=IsGet
                                              ,win_reward_list=WinRewardListSorted
                                              ,extra_reward_info=activity_server:sell_reward2p_reward_info(FamilyExtraReward)
                                              ,damage_list=DamageListAll},
    ?unicast(RoleID,Msg),
    {noreply, State};

do_handle_info({do_shop_family_limit_buy,RoleID,ShopID,false},#state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	NewMembers = 
		case lists:keytake(RoleID, #family_member_info.role_id,Members) of
			false ->
				Members;
			{value,Member=#family_member_info{limit_shop=Shop},OtherMembers} ->
				case lists:keytake(ShopID,#p_shop_family_limit.shopID,Shop) of
					false ->
						?unicast(RoleID,#sc_shop_family_limit_buy{result=3}),
						Members;
					{value,SP=#p_shop_family_limit{usedTimes=UsedTimes},OSP} ->
						SP2 = SP#p_shop_family_limit{usedTimes=UsedTimes-1},
						[Member#family_member_info{limit_shop=[SP2|OSP]}|OtherMembers]
				end
		end,
	NewFamilyInfo=FamilyInfo#family_info{members=NewMembers},
	{noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info({cs_shop_family_limit_info,RoleID},#state{family_info=FamilyInfo}=State) ->
	#family_info{members=Member} = FamilyInfo,
	case lists:keyfind(RoleID,#family_member_info.role_id, Member) of
		false ->
			?unicast(RoleID,#sc_shop_family_limit_info{result=2,shop=[]});
		#family_member_info{limit_shop=LimitShop} ->
			?unicast(RoleID,#sc_shop_family_limit_info{result=1,shop=[S#p_shop_family_limit{reward = activity_server:sell_reward2p_reward_info(Reward)}
																	 ||#p_shop_family_limit{reward=Reward}=S<-LimitShop]})
	end,
	{noreply,State};

do_handle_info({cs_shop_family_limit_buy,RoleID,ShopID},#state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members,level=FamilyLevel} = FamilyInfo,
	NewMembers = 
	case lists:keytake(RoleID, #family_member_info.role_id,Members) of
		false ->
			Members;
		{value,Member=#family_member_info{limit_shop=Shop},OtherMembers} ->
			case lists:keytake(ShopID,#p_shop_family_limit.shopID,Shop) of
				false ->
					?unicast(RoleID,#sc_shop_family_limit_buy{result=3}),
					Members;
				{value,SP=#p_shop_family_limit{levelLimit=LevelLimit,usedTimes=UsedTimes
											  ,buyTimesLimit=Limit,costUnionCoin=Cost,reward=Reward},OSP} ->
					if FamilyLevel < LevelLimit ->
						   ?unicast(RoleID,#sc_shop_family_limit_buy{result=4}),
						   Members;
					   true ->
							if UsedTimes >= Limit ->
								   ?unicast(RoleID,#sc_shop_family_limit_buy{result=2}),
								   Members;
							   true ->
								   case ?CATCH(role_lib:send_server(RoleID,{route,role_shop,
																			{do_shop_family_limit_buy,ShopID,Cost,Reward}}
																   )
										) of
									   {'EXIT',_} ->
										   Members;
									   _ ->
										   SP2 = SP#p_shop_family_limit{usedTimes=UsedTimes+1},
										   [Member#family_member_info{limit_shop=[SP2|OSP]}|OtherMembers]
								   end
							end
					end
			end
			end,
	NewFamilyInfo=FamilyInfo#family_info{members=NewMembers},
	{noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info(refresh_family_limit_shop,#state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo,
	NewMembers = 
		[Member#family_member_info{limit_shop=[S#p_shop_family_limit{usedTimes=0}||S<-Shop]}
								  ||#family_member_info{limit_shop=Shop}=Member<-Members],
	{noreply,State#state{family_info=FamilyInfo#family_info{members=NewMembers}}};

do_handle_info({cs_familyfight_get_fighter,RoleID},#state{family_info=FamilyInfo}=State) ->
    #family_info{members=Members} = FamilyInfo,
    FamilyFighterGroup = lists:filter(fun(FID)-> lists:keymember(FID, #family_member_info.role_id, Members) end, FamilyInfo#family_info.family_fighter_group),
    ?unicast(RoleID,#sc_familyfight_get_fighter{roleID_list=FamilyFighterGroup}),
    {noreply,State#state{family_info=FamilyInfo#family_info{family_fighter_group=FamilyFighterGroup}}};
do_handle_info({cs_familyfight_select_fighter,RoleID,RoleIdList},#state{family_info=FamilyInfo,randomEndTime=RT}=State) ->
    #family_info{members=Members} = FamilyInfo,
    IsSign = FamilyInfo#family_info.family_fight_other_info#family_fight_other_info.is_sign,
    Now = util:now(),
    if
        IsSign /= 1 ->
            ?unicast(RoleID,#sc_familyfight_select_fighter{result=4}),
            {noreply,State};
        Now >= RT ->
            ?unicast(RoleID,#sc_familyfight_select_fighter{result=4}),
            {noreply,State};
        true ->
            case lists:all(fun(RID)-> lists:keymember(RID, #family_member_info.role_id, Members) end,RoleIdList) of
                true ->
                    NewFamilyInfo = FamilyInfo#family_info{family_fighter_group=RoleIdList},
                    ?unicast(RoleID,#sc_familyfight_select_fighter{result=1}),
                    {noreply,State#state{family_info=NewFamilyInfo}};
                false ->
                    ?unicast(RoleID,#sc_familyfight_select_fighter{result=4}),
                    {noreply,State}
            end
    end;
do_handle_info(test_refresh_family_instance, #state{family_info=FamilyInfo}=State) ->
    NewFamilyInfo = do_zero_clock_setting(FamilyInfo#family_info{is_zero_refreshed=?undefined}),
%%     NewFamilyInstanceState = family_data:refresh_instance_boss(FamilyInfo#family_info.family_instance_state),
    {noreply, State#state{family_info=NewFamilyInfo}};
do_handle_info(stop, State) ->
	{stop, normal, State};

%% do_handle_info({reset_family_level, StartSec,StopSec,Lv}, #state{family_info=FamilyInfo}=State) ->
%% 	NewFamilyInfo = reset_family_level(StartSec,StopSec,Lv,FamilyInfo),
%% 	{noreply, State#state{family_info=NewFamilyInfo}};

do_handle_info({reset_family_level,FamilyID,OldLevel},#state{family_info=FamilyInfo}=State) ->
	NewFamilyInfo = reset_family_level(FamilyID,OldLevel,FamilyInfo,2),
	{noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info(extract_log,
               #state{family_info = FamilyInfo} = State) ->
    #family_info{family_id = FamilyID, level = Level,
                 log_data = Data} =
        FamilyInfo,
    if Level >= 5 ->
           logger:error_msg(family_server, 1120,
                            "record_family_log:~w,~w,~w",
                            [FamilyID, Level, Data]);
       true -> ignore
    end,
    {noreply, State};
    
do_handle_info({do_test_clear_family_talkdata,_RoleID},#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo = FamilyInfo#family_info{talk_data=[]},
    {noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info({cs_familycross_info,RoleID},#state{family_info=#family_info{family_id=FamilyID,familycross_fight_info=FamilyCross}}=State) ->
	#familycross_fight{period=Period,isSign=IsSign}=FamilyCross,
	erlang:send(family_cross_fight_server, {cs_familycross_info, RoleID,Period,IsSign,FamilyID}),
	{noreply,State};

do_handle_info({cs_familycross_sign,RoleID},#state{family_info=#family_info{family_id=FamilyID,level=Level,family_name=FamilyName,owner_role_name=OwnerName
																		   ,members=Members,familycross_fight_info=FamilyCross}=FamilyInfo}=State) ->
	#familycross_fight{isSign=IsSign} = FamilyCross,
	case IsSign of 
		1 -> ?unicast(RoleID,#sc_familycross_sign{result=4});
		_ ->
			case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
				#family_member_info{family_title=Title} ->
					case check_role_can_sign(Title) of
						true -> 
							case Level >= data_family_cross:get(need_level) of
								true -> 
									{#family_player{familyscore=Score},_} = get_family_player(FamilyInfo),
									TotalFightPower = lists:foldl(fun(#family_member_info{fight_power=FightPower},Acc)-> Acc+FightPower end, 0, Members),
									erlang:send(family_cross_fight_server,{cs_familycross_sign,RoleID,FamilyID,FamilyName
																			  ,TotalFightPower,OwnerName,Score});
								_ -> ?unicast(RoleID,#sc_familycross_sign{result=3})
							end;
						_ -> ?unicast(RoleID,#sc_familycross_sign{result=5})
					end;
				_ -> ?unicast(RoleID,#sc_familycross_sign{result=2})
			end
	end,
	{noreply,State};

do_handle_info({cs_familycorss_player_fly, RoleID},#state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members} = FamilyInfo, 
	Data = [begin
		 #rolePublic{plane_level=Fly} = role_lib:get_rolePublic(FRoleID),
		 #p_familycross_player_fly{roleID=FRoleID,fly=Fly}
	 end||#family_member_info{role_id=FRoleID}<-Members],
	?unicast(RoleID,#sc_familycross_player_fly{fly=Data}),
	{noreply,State};

do_handle_info({cs_familycross_displayer,RoleID,Cars},#state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members,familycross_fight_info=FamilyCross,level=FamilyLevel} = FamilyInfo,
	#familycross_fight{isSign=IsSign,cars=OldCars,disTime=DisTime} = FamilyCross,
	NewFamilyInfo = case IsSign of 
						0 -> ?unicast(RoleID,#sc_familycross_displayer{result=4}),FamilyInfo;
						_ -> 
							case util:now() > DisTime of 
								true ->	?unicast(RoleID,#sc_familycross_displayer{result=5}),FamilyInfo;
								_ -> 
									case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
										#family_member_info{family_title=Title,role_name=RoleName,head=Head,role_level=RoleLevel,is_male=IsMale} ->
											case check_role_can_sign(Title) of
												true ->   ?unicast(RoleID,#sc_familycross_displayer{result=1}),
														  NewCars = valid_cars(Cars, Members,FamilyLevel,OldCars),
														  NewLog = rebuild_family_log(RoleID,RoleName,Head,RoleLevel,?MAMILY_LOG_TYPE_CHANGE_ANUBIS,RoleName, FamilyInfo#family_info.log_data,Title,IsMale),
														  NewFamilyCross = FamilyCross#familycross_fight{cars=NewCars},
														  FamilyInfo#family_info{log_data=NewLog,familycross_fight_info=NewFamilyCross};
												_ ->  ?unicast(RoleID,#sc_familycross_displayer{result=6}),FamilyInfo %% 没权限
											end;
										_ ->
											?unicast(RoleID,#sc_familycross_displayer{result=3}),FamilyInfo
									end
							end
					end,
	{noreply,State#state{family_info=NewFamilyInfo}};
		

do_handle_info({random_match_cross_fighters,FcFighter},#state{family_info=FamilyInfo}=State) ->
	#family_info{familycross_fight_info=FamilyCross} = FamilyInfo,
	FamilyCross2 = FamilyCross#familycross_fight{enermy=FcFighter#fc_fighter.enermyInfo},
	{noreply,State#state{family_info=FamilyInfo#family_info{familycross_fight_info=FamilyCross2}}};

do_handle_info({cs_familycross_enermy_info,RoleID},#state{family_info=FamilyInfo}=State) ->
	#family_info{familycross_fight_info=#familycross_fight{enermy=Enermy}}=FamilyInfo,
	[{ServerID1,Name1},{ServerID2,Name2}]=Enermy,
	?unicast(RoleID,#sc_familycross_enermy_info{serverID1=ServerID1,familyName1=Name1
											   ,serverID2=ServerID2,familyName2=Name2}),
	{noreply,State};

do_handle_info({get_player,_,WarID,ManagerServerID,Process,PlayerType},#state{family_info=FamilyInfo}=State)->
	#family_info{family_id=FamilyID,members=Members,familycross_fight_info=FamilyCross} = FamilyInfo,
        %%传递的数据中多增加了一个当前赛季的统计数据，方便阿努比斯排行榜更新
        CSeasonFamilyPlayerMsg = get_family_player(FamilyInfo),
		#familycross_fight{cars=CarsTemp} = FamilyCross,
		F = fun()->
					case CarsTemp of
						[] ->
							?ERR("no dis player, but signed:~w",[FamilyID]),
							erlang:send(family_cross_fight_server, {family_unsign,FamilyID}),
							Cars2 = [],
							MemberData=[];
						_->
					Cars = re_generate_car_team(CarsTemp,Members),
					ServerID = data_setting:get(server_id),
					Cars2 = [begin
									#car{players=[{C1,ServerID},{C2,ServerID},{C3,ServerID},{Driver,ServerID}],driver=Driver
										,type=PlayerType,carType=CType,id=ID,speed=data_family_cross:get(car_speed)}
								 end||#p_familycross_car{driver=Driver,c1=C1,c2=C2,c3=C3,type=CType,id=ID}<-Cars,Driver/= 0],
					PlayerCarMatch = lists:foldl(fun(#p_familycross_car{driver=Driver,c1=C1,c2=C2,c3=C3,id=ID},Acc) ->
														[{Driver,ID},{C1,ID},{C2,ID},{C3,ID}]++Acc
												 end,[],Cars),
					MemberData=
						lists:foldl(fun(#family_member_info{role_id=RoleID},Acc) ->
											case lists:keyfind(RoleID, 1, PlayerCarMatch) of
												false ->
													Acc;
												CarPlayerData ->
													{FighterList,RoleLieuAdd,Talent,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
													ItemList = role_data:get_otherRoleItemEquips(RoleID),
													RP = role_lib:get_rolePublic(RoleID),
													Fly = RP#rolePublic.plane_level,
													SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
													ets:insert(?ETS_FAMILYCROSS_ROLE_WAR,{{match_info,RoleID},{WarID, ManagerServerID,util:now()}}),
                                                    SpeedAdd = data_home:get({constr_type_plane_base,role_home:get_build_type_level(RoleID,?constr_type_plane_base)}),
													[#familycross_player{serverID=ServerID
																		 ,roleID=RoleID
																		 ,familyID=FamilyID
																		 ,roleName=RP#rolePublic.roleName
																		 ,fighters=FighterList
																		 ,baseFighters=FighterList
																		 ,addBuff = RoleLieuAdd
																		 ,talent=Talent
																		 ,trSpecial=TrSpecial
																		 ,itemList=ItemList
																		 ,type=PlayerType
																		 ,flyType=Fly
																		 ,speed=(carlos_server:get_fly_speed(Fly) * (10000 + SpeedAdd)) div 10000
																		 ,level=RP#rolePublic.level
																		 ,blood=100
																		 ,head = RP#rolePublic.head
																		 ,title = RP#rolePublic.title
																		 ,isMale = RP#rolePublic.isMale
																		 ,fightPower = RP#rolePublic.fightPower
																		 ,skin_info = SkinInfo
																		,grade=doublematch_server:dirty_get_role_dm_grade(RoleID)
																		 ,car = element(2,CarPlayerData)
																		} |Acc]
											end end,[], Members)
						end,
					% 附带上公会赛季信息到阿努比斯战斗进程中
					Msg = {PlayerType,MemberData,Cars2,CSeasonFamilyPlayerMsg},
					send_msg:direct(ManagerServerID,Process,Msg)
					%behavior_family_cross:log(RoleID, 1, 1, WarID)
			end,
	spawn(F),
{noreply,State};

do_handle_info({cs_familycross_displayer_info, RoleID},#state{family_info=#family_info{familycross_fight_info=FamilyCross}}=State) ->
	#familycross_fight{cars=Cars} = FamilyCross,
	?unicast(RoleID,#sc_familycross_displayer_info{cars=Cars}),
	{noreply,State};

do_handle_info({update_family_cross_status,Period,DisTime},#state{family_info=FamilyInfo}=State)->
	#family_info{familycross_fight_info=FamilyCross} = FamilyInfo,
	FamilyInfo2 = FamilyInfo#family_info{familycross_fight_info=FamilyCross#familycross_fight{period=Period,isSign=0,disTime=DisTime}},
	{noreply,State#state{family_info=FamilyInfo2}};

do_handle_info({update_family_cross_disTime, DisTime},#state{family_info=FamilyInfo}=State) ->
	#family_info{familycross_fight_info=FamilyCross}=FamilyInfo,
	FamilyInfo2 = FamilyInfo#family_info{familycross_fight_info=FamilyCross#familycross_fight{disTime=DisTime}},
	{noreply, State#state{family_info=FamilyInfo2}};

%% do_handle_info({cs_familycorss_player_fly, RoleID}, #state{family_info=FamilyInfo}=State) ->
%% 	#family_info{members=Members} = FamilyInfo,
%% 	F = fun()->
%% 				Msg = [begin RP = role_lib:get_rolePublic(RoleID),
%% 							 #p_familycross_player_fly{roleID=RoleID
%% 													  ,fly = RP#rolePublic.plane_level}
%% 					   end||#family_member_info{role_id=RoleID}<-Members],
%% 				?unicast(RoleID,Msg)
%% 				end,
%% 	spawn(F),
%% 	{noreply,State};

do_handle_info(familycross_sign_succ, #state{family_info=FamilyInfo}=State) ->
	#family_info{members=Members,familycross_fight_info=FamilyCross,level=_FamilyLevel}=FamilyInfo,
	#familycross_fight{cars=CarsTemp} = FamilyCross,
	%Cars = [],%generate_car_team(Members,FamilyLevel),
	Cars = re_generate_car_team(CarsTemp,Members),
	FamilyInfo2=FamilyInfo#family_info{familycross_fight_info=FamilyCross#familycross_fight{isSign=1,cars=Cars}},
	{noreply, State#state{family_info=FamilyInfo2}};

%%处理在赛季排名更新之后，anubis_server广播过来的更新驱动，用于公会主动更新自身消息
do_handle_info({update_anubis_rank,SeasonID},#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo = do_update_family_anubis_rank(FamilyInfo,SeasonID),
    {noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info({update_anubis_rank2,SeasonID,FamilyPlayer},#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo = new_do_update_family_anubis_rank(SeasonID,FamilyPlayer,FamilyInfo),
    {noreply,State#state{family_info=NewFamilyInfo}};

%%公会进程初始根据排行榜存在的排行信息，更新公会的排名信息（自己向自己发送的信息）
do_handle_info(update_family_anubis_rank,#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo = update_family_anubis_rank(FamilyInfo),
    {noreply,State#state{family_info=NewFamilyInfo}};

%%处理转发来的战斗结束信息，用于更新公会信息以及发送奖励
do_handle_info({update_family_anubis_info_after_fight,CSeasonID,CSeasonFamilyPlayer,FightMsg},#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo = update_family_anubis_info(CSeasonID,CSeasonFamilyPlayer,FightMsg,FamilyInfo),
    db_sql:update_family_info(NewFamilyInfo),
    {noreply,State#state{family_info=NewFamilyInfo}};

%%处理玩家进程的请求，获取排行榜的信息
do_handle_info({cs_familycross_season_rank,RoleID,FamilyList,IsEnd,SeasonID,Begin},#state{family_info=FamilyInfo}=State)->
    OurFamily = get_family_season_rank_from_familyinfo(FamilyInfo,SeasonID),
    ?unicast(RoleID,#sc_familycross_season_rank{result=1,seasonid=SeasonID,beginpos=Begin,isend=IsEnd,familylist=FamilyList,ourfamily=[OurFamily]}),
    {noreply,State};

%%处理玩家进程获取公会内排名信息
do_handle_info({cs_familycross_family_rank,RoleID,SeasonID},#state{family_info=FamilyInfo}=State)->
    FamilyMemberRank = lists:reverse(get_family_member_anubis_rank(FamilyInfo,SeasonID)),
    ?unicast(RoleID,#sc_familycross_family_rank{result=1,memberlist=FamilyMemberRank}),
    {noreply,State};

%%处理广播来的删除对应赛季信息的命令
do_handle_info({delete_season_rank,DeleteSeasonIDList},#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo = delete_season_representation(DeleteSeasonIDList,FamilyInfo),
    {noreply,State#state{family_info=NewFamilyInfo}};

%%处理赛季切换信息
do_handle_info({update_current_seasonid,SeasonID},#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo=update_current_seasonid(SeasonID,FamilyInfo),
    {noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info({send_season_reward,Rank,SeasonID},#state{family_info=FamilyInfo}=State)->
    send_anubis_season_reward(SeasonID,Rank,FamilyInfo),
    {noreply,State};

do_handle_info({test_change_family_anubis_info,FamilyKillNum,FamilyScore},#state{family_info=FamilyInfo}=State)->
    do_test_change_family_anubis_info(FamilyInfo,FamilyKillNum,FamilyScore),
    {noreply,State};
do_handle_info({test_change_role_anubisinfo,RoleID,SeasonID,AddKillNum,AddResourcePoint},#state{family_info=FamilyInfo}=State)->
    NewFamilyInfo =  do_test_change_role_anubisinfo(RoleID,SeasonID,AddKillNum,AddResourcePoint,FamilyInfo),
    {noreply,State#state{family_info=NewFamilyInfo}};

do_handle_info({test_send_family_reward,Reward},#state{family_info=FamilyInfo}=State)->
    do_test_send_family_reward(Reward,FamilyInfo),
    {noreply,State};

do_handle_info(Info, State) ->
	?ERR("Info:~w, State:~w", [Info, State]),
    ?ERR("s:~w, e:~w", [get(?cache_fighter_list_self), get(?cache_fighter_list_enemy)]),
	{noreply, State}.

family_storage_reqList_update(RoleID) ->
	{_,Storage} = family_data:get_family_storage(),
	Storage2 = 
		lists:foldl(fun(#p_family_storage{reqRoleIDList=RIL,itemUID=ItemUID2}=Item,Acc)->
							do_bc_msg_except(#sc_family_storage_update{itemUID=ItemUID2,type=4,reqRoleIDList=[RoleID]}, []),
							[Item#p_family_storage{reqRoleIDList=lists:delete(RoleID,RIL)}|Acc]
					end, [], Storage),
	family_data:set_family_storage(Storage2).
	

do_assign_storage(ItemUID,TarRoleID,RoleID,Storage,_ReqInfo,Member,OtherMembers)->
	{value,Item=#p_family_storage{itemTypeID=_TypeID,type=_Type},Storage2} = lists:keytake(ItemUID, #p_family_storage.itemUID, Storage),
	FamilyID = family_data:get_familyID(),
	family_data:set_family_storage(Storage2),
	?unicast(RoleID,#sc_family_storage_assign{result=1}),
	mail_server:family_assign_item(TarRoleID,Item#p_family_storage.itemTypeID,Item#p_family_storage.type),
	do_bc_msg_except(#sc_family_storage_update{itemUID=ItemUID,type=2,reqRoleIDList=[]},[]),
	{Date,_}=Time=erlang:localtime(),
	behavior_storage_consume:log(FamilyID,family_extra_handler:storage_item2logstorage([Item]), Date,Time,?MONEY_DEC_TYPE_FAMILY_STORAGE_ASSIGN,TarRoleID,""),
	OtherMembers2 = lists:foldl(fun(#family_member_info{storageReqData={ItemUIDR,LRI}}=Member2,Acc)->
										case ItemUIDR of
											[] ->
												[Member2|Acc];
											[HDItemUID|_] ->
												case HDItemUID of
													ItemUID ->
														[Member2#family_member_info{storageReqData={[],LRI}}|Acc];
													_ ->
														[Member2|Acc]
												end
										end
								end, [], OtherMembers),
	[Member#family_member_info{storageReqData={[],{Date,1}}}|OtherMembers2].

check_can_assign_storage(ItemUID,TarRoleID,RoleID,Owner,Members)->
	case RoleID of
		Owner->
			{_,Storage} = family_data:get_family_storage(),
			case lists:keyfind(ItemUID, #p_family_storage.itemUID, Storage) of
				#p_family_storage{reqRoleIDList=ReqList} ->
					case lists:member(TarRoleID,ReqList) of
						true->
							case lists:keytake(TarRoleID, #family_member_info.role_id, Members) of
								false ->
									{false,2};
								{value,#family_member_info{storageReqData=ReqInfo}=Member,OtherMembers} ->
									{_,{Date,_}}=ReqInfo,
									case erlang:date() of
										Date ->
											{false,6};
										_ ->
											
											{true,Storage,ReqInfo,Member,OtherMembers}
									end
							end;
						false ->
							{false,4}
					end;
				false ->
					{false,3}
			end;
		_ ->
			{false,5}
	end.

do_req_storage(RoleID,#p_family_storage{reqRoleIDList=ReqList}=Item,Other,{RoleReqList,LastAsnInfo},Member,OtherMembers)->
	Storage = [Item#p_family_storage{reqRoleIDList=[RoleID|ReqList]} |Other],
	Storage2 = 
		case RoleReqList of
			[] ->
				Storage;
			[Req|_] ->
				case lists:keytake(Req, #p_family_storage.itemUID, Storage) of
					false ->
						Storage;
					{value,Item2=#p_family_storage{reqRoleIDList=ReqList2,itemUID=ItemUID2},OtherItem2} ->
						do_bc_msg_except(#sc_family_storage_update{itemUID=ItemUID2,type=4,reqRoleIDList=[RoleID]}, []),
						[Item2#p_family_storage{reqRoleIDList=lists:delete(RoleID, ReqList2)}|OtherItem2]
				end
		end,
	family_data:set_family_storage(Storage2),
	?unicast(RoleID,#sc_family_storage_req{result=1}),
	do_bc_msg_except(#sc_family_storage_update{itemUID=Item#p_family_storage.itemUID,type=3,reqRoleIDList=[RoleID]}, []),
	%lists:keyreplace(RoleID, #family_member_info.role_id, Members, Member2).
	[Member#family_member_info{storageReqData={[Item#p_family_storage.itemUID],LastAsnInfo}}|OtherMembers].


check_can_req_storage(RoleID,ItemUID,Members)->
	case lists:keytake(RoleID, #family_member_info.role_id, Members) of
		false ->
			{false,2};
		{value,Member=#family_member_info{join_time=JoinTime,storageReqData=ReqInfo},OtherMembers} ->
			{_,{LastAssignDate,_}}=ReqInfo,
			{Date,_} = util:seconds_to_datetime(JoinTime),
			Today = erlang:date(),
			case Today of
				Date ->
					{false,6};
				_ ->
					case Today of
						LastAssignDate ->
							{false,7};
						_ ->
							{_,Storage} = family_data:get_family_storage(),
							case lists:keytake(ItemUID, #p_family_storage.itemUID,Storage) of
								false ->
									{false,3};
								{value,#p_family_storage{reqRoleIDList=ReqList}=Item,Other}->
									case lists:member(RoleID,ReqList) of
										true ->
											{false,5};
										false ->
											MaxReqNum = data_family:get(max_storagy_req_num),
											case length(ReqList) >= MaxReqNum of
												true ->
													{false,4};
												false ->
													{true,Item,Other,ReqInfo,Member,OtherMembers}
											end
									end
							end
					end
			end
	end.


do_family_owner_impeach(OldOwnerRoleID,NewOwnerID,#family_info{members=Members,family_id=FamilyID,level=FamilyLevel} = FamilyInfo)->
	{value,OldOwner,OtherMembers} = lists:keytake(OldOwnerRoleID, #family_member_info.role_id, Members),
	{value,NewOwner,OtherMembers2} = lists:keytake(NewOwnerID,#family_member_info.role_id,OtherMembers),
	NewOwner2 = NewOwner#family_member_info{family_title=?FAMILY_TITLE_OWNER},
	OldOwner2 = OldOwner#family_member_info{family_title=?FAMILY_TITLE_MEMBER},
	FamilyInfo2 = FamilyInfo#family_info{owner_role_id=NewOwnerID,owner_role_name=NewOwner2#family_member_info.role_name,members=[NewOwner2,OldOwner2|OtherMembers2]},
	notify_family_changed(FamilyID,NewOwner2#family_member_info.role_name,FamilyLevel),
	do_bc_msg_except(#sc_family_update_family_info{info=family_misc:to_p_family_info(FamilyInfo2)}, []),
	% erlang:send(family_manager_server, {update_family_info, FamilyInfo2}),
	FamilyInfo2.
	
	
%%检查玩家是否能够弹劾会长
check_family_owner_impeach(RoleID,#family_info{members=Members,owner_role_id=OwnerID}=FamilyInfo)->
    case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
            {false,2};
        _ ->
            case role_lib:is_online(OwnerID) of
                true ->
                    {false,5};
                false ->
                    case lists:keyfind(OwnerID,#family_member_info.role_id,Members) of
                        false ->
                            ?INFO("未在公会：~w 中查找到公会成员：~w ~n",[FamilyInfo,OwnerID]),
                            {false,4};
                        OwnerInfo ->
                            LastOnLineTime = OwnerInfo#family_member_info.offline_time,
                            case check_offline_second_outband(LastOnLineTime) of
                                false ->
                                    {false,5};
                                true ->
                                    case check_role_can_impeach_owner(RoleID,FamilyInfo) of
                                        {false,Reason} ->
                                            {false,Reason};
                                        true ->
                                            {true,OwnerID}
                                    end                               
                            end
                    end
            end
    end.

check_family_owner_can_impeach(#family_info{members=Members,owner_role_id=OwnerID}=FamilyInfo)->
    case role_lib:is_online(OwnerID) of
        true ->
            false;
        false ->
            case lists:keyfind(OwnerID,#family_member_info.role_id,Members) of
                false ->
                    ?INFO("未在公会：~w 中查找到公会成员：~w ~n",[FamilyInfo,OwnerID]),
                    false;
                OwnerInfo ->
                    LastOnLineTime = OwnerInfo#family_member_info.offline_time,
                    check_offline_second_outband(LastOnLineTime)
            end
    end.

check_offline_second_outband(LastOnLineTime)->
	MaxDate = data_family:get(owner_outban_days),
	LastOnLineTime + MaxDate =< util:now().

do_hatch_family_boss_egg(RoleID,#family_info{family_id=FamilyID,members=Members},Msg,Pos)->
	case check_role_familyBoss_power(lists:keyfind(RoleID, #family_member_info.role_id, Members),hatch) of
		true ->
			{_,_,NeedRice} = lists:keyfind(Pos, 1, data_family_boss:get(family_boss_active_limit)),
			case family_lib:check_family_cost(rice,NeedRice) of
				{true,Wallet} ->
					case catch family_boss_server:call_boss_server(FamilyID,Msg) of
						{'EXIT',_} ->
							?unicast(RoleID, #sc_familyBoss_hatch_egg{result=7});
						Result ->
							family_lib:deduct_family_wallet(rice,NeedRice,Wallet,FamilyID,?MONEY_DEC_TYPE_HATCH_EGG,Pos,""),
							?unicast(RoleID, #sc_familyBoss_hatch_egg{result=Result})
					end;
				false ->
					?unicast(RoleID,#sc_familyBoss_hatch_egg{result=8})
			end;
		_ ->
			?unicast(RoleID, #sc_familyBoss_hatch_egg{result=6})
	end.

do_set_family_boss_time(RoleID,#family_info{family_id=FamilyID,members=Members},Msg)->
	case check_role_familyBoss_power(lists:keyfind(RoleID, #family_member_info.role_id, Members),set_time) of
		true ->
			case catch family_boss_server:call_boss_server(FamilyID,Msg) of
				{'EXIT',_} ->
					?unicast(RoleID,#sc_familyBoss_set_boss_time{result=8});
				Result ->
					?unicast(RoleID,#sc_familyBoss_set_boss_time{result=Result})
			end;
		_ ->
			?unicast(RoleID,#sc_familyBoss_set_boss_time{result=7})
	end.

do_family_boss_attack(RoleID,#family_info{members=Members,family_id=FamilyID},Msg)->
	case lists:keyfind(RoleID,#family_member_info.role_id,Members) of
		false ->
			?unicast(RoleID,#sc_familyBoss_attack{result=2,fightInfo=[]});
		#family_member_info{join_time=JoinTime}->
			{Date,_} = util:seconds_to_datetime(JoinTime),
			case erlang:date() of
				Date ->
					?unicast(RoleID,#sc_familyBoss_attack{result=6,fightInfo=[]});
				_ ->
					family_misc:router_to_family_boss_process(FamilyID,Msg)
			end
	end.

check_role_familyBoss_power(#family_member_info{family_title=FamilyTitle,join_time=JoinTime},Type)->
	{Date,_} = util:seconds_to_datetime(JoinTime),
	case erlang:date() of
		Date ->
			false;
		_ ->
			check_role_familyBoss_power2(FamilyTitle,Type)
	end;
check_role_familyBoss_power(_,_) ->
	false.

get_reward_templateID(#pf_fighter{pareID=PareID,result=Result})->
	GroupID = PareID div ?familyfight_group_base,
	{_,RewardID} = data_family_fight_split:get(GroupID),
	{_,RewardList} = lists:keyfind(RewardID,1,data_family_fight:get(rewardList)),
	case Result of
		0 ->
			ignore;
		_ ->
			{Type,{_,Reward}} = 
				case Result of
					1 ->
						{?MAIL_FAMILYFIGHT_REWARD_WIN,lists:keyfind(1, 1, RewardList)};
					3 ->
						{?MAIL_FAMILYFIGHT_REWARD_EQ, lists:keyfind(2, 1, RewardList)};
					2 ->
						{?MAIL_FAMILYFIGHT_REWARD_LOSE, lists:keyfind(3, 1, RewardList)}
				end,
			{Type,Reward}
	end.
			
%% svn 15431
calc_get_star(IsWin,FinalState,DefendTimes)->
	MaxDefendTimes = data_family_fight:get(max_defend_times),
	if IsWin =:= false ->
		   Score = 0;
	   true ->
		   DeadNum = 
			   lists:foldl(fun({_,Hp,_},Acc) ->
								   if Hp =< 0 ->
										  Acc+1;
									  true ->
										  Acc
								   end
						   end, 0, FinalState),
		   if DeadNum >= 2 ->
				  Score = 1;
			  DeadNum =:= 1 ->
				  Score = 2;
			  true ->
				  Score = 3
		   end
	end,
	Score2 = MaxDefendTimes - DefendTimes,
	if Score2 =< 0 ->
		   0;
	   Score =< Score2 ->
		   Score;
	   true ->
		   Score2
	end.

do_familyfight(RoleID, #state{family_info=FamilyInfo,family_match=MatchInfo}=State,TarRoleID,LockedList2,TarFamilyID, TarServerID)->
	#family_info{members=Members,family_id=FamilyID,family_name=FamilyName}=FamilyInfo,
	case lists:keytake(RoleID, #family_member_info.role_id, Members) of
		false ->
			?unicast(RoleID, #sc_familyfight_attack{result=2,fight_dtl=[]}),
			State;
		{value,#family_member_info{attack_times=AttackTimes,role_name=RoleName}=FamilyRole,OtherMembers} ->
			FamilyInfo2=FamilyInfo#family_info{members=[FamilyRole#family_member_info{attack_times=AttackTimes+1}|OtherMembers]},
            {FighterList,RoleLieuAdd,Talent,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
            SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
            EquipList = role_data:get_otherRoleItemEquips(RoleID),
            GerEquipList = role_item:assort_ger_equiplist(EquipList),
            LegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
            %%此处需要传递上皮肤信息
			% SelfGerInfoList = role_data:get_otherRoleFighter(RoleID),
            SelfGerInfoList = {FighterList,RoleLieuAdd,Talent,TrSpecial,SkinInfo,LegendAddList},
			MatchInfo2 = MatchInfo#family_match{locked_list=LockedList2},
			erlang:send(family_fight_server,{cs_familyfight_attack, RoleID, FamilyID, SelfGerInfoList,RoleName,FamilyName,TarRoleID, TarFamilyID, TarServerID,needInfo}),
			State#state{family_info=FamilyInfo2,family_match=MatchInfo2}
	end.
	
check_can_challenge(AttackTimes,LockedList, TarRoleID)->
	case AttackTimes >= data_family_fight:get(max_attack_times) of
		true ->
			{false, 1};
		_ -> 
			case lists:member(TarRoleID, LockedList) of
				true ->
					{false,2};
				_ ->
					{true,[TarRoleID|LockedList]}
			end
	end.

send_client_member_info(RoleID,FighterInfoList,SelfInfo,FamilyFightOtherInfo,IsSign,FT,ET)->
	#family_fight_other_info{matcher_server_id=MatcherServerID
								,matcher_family_id=MatcherFamilyID
								,matcher_family_name=MatcherName
								,matcher_family_rank=MatcherRank
								,matcher_win_star=MatcherStar}=FamilyFightOtherInfo,
    NewFighterInfoList = lists:sort(fun(A,B) ->
                    #p_familyfighter_member_info{winStar=WinStarA,fight_power=FightPowerA, rolefamilyTitle=TitleA}=A,
                    #p_familyfighter_member_info{winStar=WinStarB,fight_power=FightPowerB, rolefamilyTitle=TitleB}=B,
                    case WinStarA =:= WinStarB of
                        false ->
                            WinStarA > WinStarB;
                        _ ->
                            case FightPowerA =:= FightPowerB of
                                false ->
                                    FightPowerA > FightPowerB;
                                _ ->
                                    TitleA > TitleB
                            end
                    end
            end, get_self_info_list_for_fight_cache(FighterInfoList,?cache_fighter_list_enemy,IsSign,FT,ET)),
	OtherInfo = generate_self_info(NewFighterInfoList,MatcherFamilyID,MatcherStar,MatcherRank,MatcherServerID,MatcherName),
	?unicast(RoleID,#sc_familyfight_fighter_info{result=1,selfFamily=[SelfInfo],otherFamily=[OtherInfo]}).
		
generate_self_info(MemberInfoList,FamilyID,WinStar,Rank,ServerID,FamilyName)->
	#p_familyfighter_info_dtl{family_id=FamilyID,server_id=ServerID,family_name=FamilyName,winStar=WinStar,worldRank=Rank,memberList=MemberInfoList}.

%% 公会战返回列表，无排序，isJoinWar是随机出来的
get_self_info_list(Members) ->
    InfoList = get_no_sort_self_info_list(Members),
	lists:reverse(lists:keysort(#p_familyfighter_member_info.isJoinWar, InfoList)).

%% 公会战返回的已经列表有特殊的排序要求
get_self_info_list_for_fight(Members0,GroupList) ->
    Members = lists:filter(fun(R)-> lists:member(R#family_member_info.role_id, GroupList) end, Members0),
    InfoList = get_no_sort_self_info_list(Members),
    lists:sort(fun(A,B) ->
                    #p_familyfighter_member_info{winStar=WinStarA,fight_power=FightPowerA, rolefamilyTitle=TitleA}=A,
                    #p_familyfighter_member_info{winStar=WinStarB,fight_power=FightPowerB, rolefamilyTitle=TitleB}=B,
                    case WinStarA =:= WinStarB of
                        false ->
                            WinStarA > WinStarB;
                        _ ->
                            case FightPowerA =:= FightPowerB of
                                false ->
                                    FightPowerA > FightPowerB;
                                _ ->
                                    TitleA > TitleB
                            end
                    end
            end, InfoList).

%% 公会战返回的已经列表有特殊的排序要求 带有缓存检测，用缓存数据补充缺失的内容
%% 需要防止其他场景回复残留退会人员信息 %Mark
get_self_info_list_for_fight_cache(Members,GroupList,CacheName,IsSign,FT,_ET) when CacheName =:= ?cache_fighter_list_self->
    CacheList = get(CacheName),
    Now = util:now(),
    IsShowResult = IsSign =:= 1 andalso Now > FT,
    ?INFO("get_self_info_list_for_fight_cache ~w ~w ~w ~w",[CacheList,IsSign,Now,FT]),
    case IsShowResult of
        true when is_list(CacheList)->
            ?INFO("使用缓存数据匹配排行信息"),
            {NewMembers,_} = lists:foldr(fun(#family_member_info{role_id=RoleID}=OldMemberInfo,{AccList,MemberInfoList})->
                                case lists:keytake(RoleID, 2, MemberInfoList) of
                                    {value, NewMemberInfo, MemberInfoList2}->
                                        {[NewMemberInfo|AccList],MemberInfoList2};
                                    false ->
                                        {[OldMemberInfo|AccList],MemberInfoList}
                                end  
                            end, {[],Members}, CacheList),
            put(CacheName,NewMembers),
            get_self_info_list_for_fight(NewMembers,GroupList);
        true when CacheList =:= undefined ->
            ?INFO("无缓存信息，初始化缓存"),
            put(CacheName,Members),
            get_self_info_list_for_fight(Members,GroupList);
        _ ->
            ?INFO("不在战果展示阶段"),
            get_self_info_list_for_fight(Members,GroupList)
    end.
get_self_info_list_for_fight_cache(Members,CacheName,IsSign,FT,_ET) when CacheName =:= ?cache_fighter_list_enemy ->
    CacheList = get(CacheName),
    Now = util:now(),
    IsShowResult = IsSign =:= 1 andalso Now > FT,
    case IsShowResult of
        true when is_list(CacheList)->
            {NewMembers,_} = lists:foldr(fun(#p_familyfighter_member_info{roleID=RoleID}=OldMemberInfo,{AccList,MemberInfoList})->
                                case lists:keytake(RoleID, 2, MemberInfoList) of
                                    {value, NewMemberInfo, MemberInfoList2}->
                                        {[NewMemberInfo|AccList],MemberInfoList2};
                                    false ->
                                        {[OldMemberInfo|AccList],MemberInfoList}
                                end  
                            end, {[],Members}, CacheList),
            put(CacheName,NewMembers),
            NewMembers;
        true when CacheList =:= undefined ->
            put(CacheName,Members),
            Members;
        _ ->
            Members
    end.

get_no_sort_self_info_list(Members) ->
	[begin 
		 #family_member_info{role_id=RoleID
							 ,role_name=RoleName
							 ,family_title=RoleFamilyTitle
							 ,win_star=WinStar
							 ,is_join_war=IsJoinWar
							 ,attack_times=AttackTimes
							 ,defend_times=DefendTimes
                            ,head=Head
                            ,is_male=IsMale
                            ,title=Title
                            ,role_level=RoleLevel
							,fight_power=FightPower}=Member,
		 #p_familyfighter_member_info{roleID=RoleID,roleName=RoleName, rolefamilyTitle=RoleFamilyTitle,isJoinWar=IsJoinWar
									  ,winStar=WinStar,attackTimes=get_max_attackTimes()-AttackTimes,defendTimes=get_max_defendTimes()-DefendTimes
                                      ,head=Head,title=Title,is_male=IsMale,roleLevel=RoleLevel, fight_power = FightPower}
	 end || Member <- Members, Member#family_member_info.is_join_war > 0].

is_match_info_valid(Stamp)->
	case Stamp > util:now() of
		true ->
			true;
		false ->
			false
	end.

do_invite_join(FamilyInfo, SenderID, RoleID, FamilyID, FightEndTime) ->
    case catch check_can_agree_join(FamilyInfo, SenderID, RoleID, FightEndTime) of
        {ok, JoinRoleInfo} ->
            do_invite_join2(FamilyInfo, FamilyID, JoinRoleInfo, SenderID, RoleID);
        {false, Reason} ->
            ?unicast(RoleID, #sc_mail_invite_operate{result=Reason}),
            {ok, FamilyInfo}
    end.

do_invite_join2(FamilyInfo, FamilyID, JoinRoleInfo, _RoleID, JoinRoleID) ->
    #family_info{members=Members} = FamilyInfo,
    #role{roleID=JoinRoleID, roleName=JoinRoleName, title=JoinTitle, isMale=JoinIsMale, level=JoinRoleLevel, fightPower=JoinFightPower, head=Head, lastLogoutTime=OfflineTime} = JoinRoleInfo,
    NewMember =
        #family_member_info{
                            role_id=JoinRoleID
                            ,role_name=JoinRoleName
                            ,family_id=FamilyID
                            ,family_contribution=0
                            ,left_family_contribution=0
                            ,use_gold_time=0
                            ,title=JoinTitle
                            ,is_male=JoinIsMale
                            ,online=role_lib:is_online(JoinRoleID)
                            ,role_level=JoinRoleLevel
                            ,fight_power=JoinFightPower
                            ,family_title=?FAMILY_TITLE_MEMBER
                            ,is_join_war=0
                            ,attack_times=0
                            ,defend_times=0
                            ,win_star=0
                            ,join_time=util:now()
                            ,weeklyContributes=0
                            ,lastContributeDate={1970,1,1}
                            ,recvEnergyList=[]
                            ,head=Head
                            ,offline_time = OfflineTime
						    ,limit_shop=family_server:init_family_limit_shop()
                           },  
    ?INFO("新加入玩家(~w)战斗力为 ~w",[JoinRoleID,JoinFightPower]),
    NewMembers = [NewMember|Members],
%    NewLog = rebuild_family_log(JoinRoleID,JoinRoleName,Head,JoinRoleLevel,?MAMILY_LOG_TYPE_JOIN_BE_INVITE,"", FamilyInfo#family_info.log_data,JoinTitle,JoinIsMale),
    NewDamageList = [{instance_damage_info,JoinRoleID,JoinRoleName
                    ,0,0}|FamilyInfo#family_info.family_instance_state#family_instance_state.damage_list],
    NewFamilyInstanceState = FamilyInfo#family_info.family_instance_state#family_instance_state{damage_list=NewDamageList},
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers),family_instance_state=NewFamilyInstanceState},%,log_data=NewLog},
    db_sql:update_family_info(NewFamilyInfo),
    %PFamilyInfo = family_misc:to_p_family_info(NewFamilyInfo),
    update_online(JoinRoleID, role_lib:is_online(JoinRoleID)),
    case role_lib:is_online(JoinRoleID) of
        true ->
            role_lib:send_server(JoinRoleID, {sync_family_task, FamilyInfo#family_info.family_task});
        _ ->
            next
    end,
    erlang:send(family_manager_server, {role_join_family, FamilyID, JoinRoleID}),
    ?unicast(JoinRoleID, #sc_mail_invite_operate{result=1}),
    {ok, NewFamilyInfo}.

do_agree_join(FamilyInfo, RoleID, FamilyID, JoinRoleID, _FamilyRequest,FightEndTime) ->
    case catch check_can_agree_join(FamilyInfo, RoleID, JoinRoleID,FightEndTime) of
        {ok, JoinRoleInfo} ->
            do_agree_join2(FamilyInfo, FamilyID, JoinRoleInfo, RoleID, JoinRoleID);
        {false, Reason} ->
            family_misc:unlock_family_protect(JoinRoleID),
            ?unicast(RoleID, #sc_family_agree_join{result=Reason,is_self=true,family_info=family_misc:gen_p_family_info()}),
            {ok, FamilyInfo}
    end.

do_agree_join2(FamilyInfo, FamilyID, JoinRoleInfo, RoleID, JoinRoleID) ->
    #family_info{family_name=FamilyName, members=Members} = FamilyInfo,
    #role{roleID=JoinRoleID, roleName=JoinRoleName, title=JoinTitle, isMale=JoinIsMale, level=JoinRoleLevel, fightPower=JoinFightPower, head=Head
         , lastLogoutTime=OfflineTime,vipLevel=VipLevel,svipLevel=SVipLevel} = JoinRoleInfo,
    Vip = role_lib:cacl_vip_info(VipLevel,SVipLevel),
    NewMember =
		#family_member_info{
							role_id=JoinRoleID
							,role_name=JoinRoleName
							,family_id=FamilyID
							,family_contribution=0
							,left_family_contribution=0
							,use_gold_time=0
							,title=JoinTitle
							,is_male=JoinIsMale
							,online=role_lib:is_online(JoinRoleID)
							,role_level=JoinRoleLevel
							,fight_power=JoinFightPower
							,family_title=?FAMILY_TITLE_MEMBER
							,is_join_war=0
							,attack_times=0
							,defend_times=0
							,win_star=0
							,join_time=util:now()
							,weeklyContributes=0
							,lastContributeDate={1970,1,1}
						    ,recvEnergyList=[]
                            ,head=Head
                            ,offline_time = OfflineTime
						    ,limit_shop=family_server:init_family_limit_shop()
                            ,vip=Vip
						   },
    ?INFO("新加入玩家(~w)战斗力为 ~w",[JoinRoleID,JoinFightPower]),
    NewMembers = [NewMember|Members],
    NewLog = rebuild_family_log(JoinRoleID,JoinRoleName,Head,JoinRoleLevel,?MAMILY_LOG_TYPE_JOIN,"", FamilyInfo#family_info.log_data,JoinTitle,JoinIsMale),
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers),log_data=NewLog},
					%  ,log_data=lists:sublist([#p_family_log_dtl{timeStamp=util:now(), logMsg=list_to_binary(binary_to_list(JoinRoleName) ++ data_family:get(join_log_msg))}|FamilyInfo#family_info.log_data],20)},
    db_sql:update_family_info(NewFamilyInfo),
    PFamilyInfo = family_misc:to_p_family_info(NewFamilyInfo),
    ?unicast(RoleID, #sc_family_agree_join{result=0, is_self=true, family_info=PFamilyInfo}),
    update_online(JoinRoleID, role_lib:is_online(JoinRoleID)),
    case role_lib:is_online(JoinRoleID) of
        true ->
            role_lib:send_server(JoinRoleID, {sync_family_task, FamilyInfo#family_info.family_task});
        _ ->
            next
    end,
    do_bc_msg_except(#sc_family_agree_join{result=0, is_self=false, family_info=PFamilyInfo}, [RoleID]),
    erlang:send(family_manager_server, {role_join_family, FamilyID, JoinRoleID}),
%% 	notify_role_join_family(FamilyID, JoinRoleID),
    catch mail_server:send_sys_mail(JoinRoleID, ?MAIL_FAMILY_BE_AGREE, [FamilyName], "", []),
%% 下面代码是关于联盟商店的
%%     case role_lib:is_online(JoinRoleID) of
%%         true ->
%%             role_lib:send_server(JoinRoleID, {join_family_succ,FamilyID});
%%         false ->
%%             db_sql:refresh_familyShopInfo(JoinRoleID)
%%     end,
	%family_extra_handler:update_role_tekInfo(JoinRoleID,family_data:get_family_tek()),
    %family_extra_handler:update_role_tekInfo(JoinRoleID),
    {ok, NewFamilyInfo}.

check_can_agree_join(FamilyInfo, RoleID, JoinRoleID,FightEndTime) ->
    #family_info{level=FamilyLevel, owner_role_id=_OwnerRoleID, cur_members=CurMembers, members=Members,family_fight_other_info = FightInfo} = FamilyInfo,
    
    %% 是否在联盟战期间，这期间禁止成员加入
    NowSecond = util:now(),
    ?INFO("Check it's on the war? now:~w end_time:~w ~w",[NowSecond,FightEndTime,NowSecond =< FightEndTime]),
    case FightInfo#family_fight_other_info.is_sign of
             1 when NowSecond =< FightEndTime ->
                 ?DEBUG("L-联盟战期间禁止加入~w",[JoinRoleID]),
                 erlang:throw({false, 10});
             _ ->
                 next
    end,

	case check_role_can_agree_or_kick(get_member_title(RoleID,Members)) of
		false ->
			erlang:throw({false, 3});
		true ->
			next
	end,
    case CurMembers < data_family:get({max_role_num, FamilyLevel}) of
        true ->
            next;
        false ->
            erlang:throw({false, 4})
    end,
    case lists:keyfind(JoinRoleID, #family_member_info.role_id, Members) of
        false ->
            next;
        _ ->
            erlang:throw({false, 5})
    end,
    IsJoinRoleOnline = role_lib:is_online(JoinRoleID),
    JoinRoleInfo = 
        case IsJoinRoleOnline of
            true ->
                catch role_lib:call_server(JoinRoleID, get_role_info,1000);
            false ->
                db_sql:get_roleInfo(JoinRoleID)
        end,
    case erlang:is_record(JoinRoleInfo, role) of
        true ->
            next;
        false ->
            erlang:throw({false, 6})
    end,
    #role{familyID=JoinRoleFamilyID, lastJoinFamily=LastJoinFamily} = JoinRoleInfo,
    case JoinRoleFamilyID =:= 0 of
        true ->
            next;
        false ->
            erlang:throw({false, 7})
    end,
    case LastJoinFamily + data_family:get(join_protect_seconds) < util:now() of
        true ->
            next;
        false ->
            erlang:throw({false, 8})
    end,
    {ok, JoinRoleInfo}.

do_refuse_join(FamilyInfo, RoleID, JoinRoleID) ->
    case catch check_can_refuse_join(FamilyInfo, RoleID, JoinRoleID) of
        {ok, FamilyID, BeRefusedRoleIDList, FamilyRequestList} ->
            erlang:send(family_manager_server, {refuse_join, RoleID, JoinRoleID, FamilyID, BeRefusedRoleIDList, FamilyRequestList}),
            lists:foreach(fun(BeRefusedRoleID) ->
                              catch mail_server:send_sys_mail(BeRefusedRoleID, ?MAIL_FAMILY_BE_REFUSE, [FamilyInfo#family_info.family_name], "", [])    
                          end, BeRefusedRoleIDList);
        {false, Reason} ->
            ?unicast(RoleID, #sc_family_refuse_join{result=Reason, is_self=true})
    end.

check_can_refuse_join(FamilyInfo, RoleID, JoinRoleID) ->
    #family_info{owner_role_id=_OwnerRoleID, family_id=FamilyID,members=CurMembers} = FamilyInfo,
	case check_role_can_agree_or_kick(get_member_title(RoleID,CurMembers)) of
%    case RoleID =:= OwnerRoleID of
        true ->
            next;
        false ->
            erlang:throw({false, 2})
    end,
    case JoinRoleID =:= 0 of
        false ->
            case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=JoinRoleID, family_id=FamilyID, _='_'}) of
                [] ->
                    FamilyRequestList = [],
                    BeRefusedRoleIDList = [],
                    erlang:throw({false, 3});
                FamilyRequestList ->
                    BeRefusedRoleIDList = lists:map(fun(#family_request{role_id=BeRefusedRoleID}) -> BeRefusedRoleID end, FamilyRequestList)
            end;
        true ->
            case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{family_id=FamilyID, _='_'}) of
                [] ->
                    FamilyRequestList = [],
                    BeRefusedRoleIDList = [],
                    erlang:throw({false, 3});
                FamilyRequestList ->
                    BeRefusedRoleIDList = lists:map(fun(#family_request{role_id=BeRefusedRoleID}) -> BeRefusedRoleID end, FamilyRequestList)
            end
    end,
    {ok, FamilyID, BeRefusedRoleIDList, FamilyRequestList}.

do_leave(FamilyInfo, RoleID,FightEndTime) ->
    case catch check_can_leave(FamilyInfo, RoleID,FightEndTime) of
		{ok, KickMember, NewMembers, IsFamilyOwner} ->
			NewMembers2 = clear_energy_sended_by_role(RoleID, NewMembers),  %% 让其他成员不再看到离开的人给予的能量
            NewLog = rebuild_family_log(KickMember#family_member_info.role_id,
                                            KickMember#family_member_info.role_name,
                                            KickMember#family_member_info.head,
                                            KickMember#family_member_info.role_level,?MAMILY_LOG_TYPE_LEAVE,"", FamilyInfo#family_info.log_data,
                                            KickMember#family_member_info.title,
                                            KickMember#family_member_info.is_male),
            delete_contribute_record(RoleID),
			case IsFamilyOwner of
				false ->
					do_leave2(FamilyInfo#family_info{log_data=NewLog}, NewMembers2, RoleID);
				true ->
					do_leave_owner(FamilyInfo#family_info{log_data=NewLog}, NewMembers2, RoleID)
			end;
		{false, Reason} ->
            case Reason of
                5 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(RoleID)
            end,
            ?unicast(RoleID, #sc_family_leave{result=Reason, is_self=true, family_info=family_misc:gen_p_family_info()}),
            {ok, FamilyInfo}
    end.

%% 盟主离开，如果联盟只有盟主一人则解散，否则自动移交盟主给其他玩家
do_leave_owner(FamilyInfo, NewMembers, RoleID) ->
    case NewMembers of
        [] ->
            ?ERR("公会:~p只有会长:~p一人,且会长退出了公会,公会将被解散.~n", [FamilyInfo,RoleID]),
            ?unicast(RoleID, #sc_family_leave{result=2, is_self=true, family_info=family_misc:gen_p_family_info()});
%%             do_disband(FamilyInfo, RoleID);
        _ ->
            do_hand_over(FamilyInfo, NewMembers, RoleID)
    end.

do_disband(FamilyInfo, RoleID) ->
    ?ERR("公会:~p将被解散,会长~p.~n",[FamilyInfo,RoleID]),
    FamilyID = FamilyInfo#family_info.family_id,
    db_sql:del_family_member(FamilyID, RoleID),
    db_sql:del_family_info(FamilyID),
    ?unicast(RoleID, #sc_family_leave{result=0, is_self=true, family_info=family_misc:gen_p_family_info()}),
    erlang:send(family_manager_server, {disband, FamilyID, RoleID}),
	family_misc:router_to_family_process(FamilyID, family_disbanded),
	erlang:send(family_fight_server, {disband, FamilyID}),
	family_extra_handler:update_role_tekInfo(RoleID),
	%delete_room_id(FamilyInfo#family_info.talkRoomID),
    {ok, ?undefined}.

gen_new_family_owner(NewMembers) ->
    [Member|LeftMembers] =
        lists:sort(fun(#family_member_info{role_level=RoleLevel1, fight_power=FightPower1, join_time=JoinTime1},
                       #family_member_info{role_level=RoleLevel2, fight_power=FightPower2, join_time=JoinTime2}) ->
                           if
                               RoleLevel1 > RoleLevel2 ->
                                   true;
                               RoleLevel1 < RoleLevel2 ->
                                   false;
                               FightPower1 > FightPower2 ->
                                   true;
                               FightPower1 < FightPower2 ->
                                   false;
                               JoinTime1 < JoinTime2 ->
                                   true;
                               true ->
                                   false
                           end
                   end, NewMembers),
    #family_member_info{role_id=RoleID, role_name=RoleName} = Member,
    {RoleID, RoleName, [Member#family_member_info{family_title=?FAMILY_TITLE_OWNER}|LeftMembers]}.

do_hand_over(FamilyInfo, NewMembers, RoleID) ->
    {NewOwnerRoleID, NewOwnerRoleName, NewMembers2} = gen_new_family_owner(NewMembers),
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers2, cur_members=erlang:length(NewMembers2), owner_role_id=NewOwnerRoleID, owner_role_name=NewOwnerRoleName},
    db_sql:del_family_member(NewFamilyInfo#family_info.family_id, RoleID),
    db_sql:update_family_info(NewFamilyInfo),
    ?unicast(RoleID, #sc_family_leave{result=0, is_self=true, family_info=family_misc:gen_p_family_info()}),
    update_online(RoleID, false),
    do_bc_msg_except(#sc_family_leave{result=0, is_self=false, family_info=family_misc:to_p_family_info(NewFamilyInfo)}, [RoleID]),
	notify_family_changed(FamilyInfo#family_info.family_id,NewOwnerRoleName,FamilyInfo#family_info.level),
    erlang:send(family_manager_server, {leave, RoleID}),
	notify_family_member_leave_family(FamilyInfo#family_info.family_id, RoleID),
	family_extra_handler:update_role_tekInfo(RoleID),
	family_storage_reqList_update(RoleID),
    {ok, NewFamilyInfo}.
    
do_leave2(FamilyInfo, NewMembers, RoleID) ->
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers)},
    db_sql:del_family_member(NewFamilyInfo#family_info.family_id, RoleID),
    db_sql:update_family_info(NewFamilyInfo),
    update_online(RoleID, false), %% 更新在线状态
    do_bc_msg_except(#sc_family_leave{result=0, is_self=false, family_info=family_misc:to_p_family_info(NewFamilyInfo)}, [RoleID]),
    erlang:send(family_manager_server, {leave, RoleID}),  %% family_manager_server收到消息后，会执行update_role_family_id(RoleID, 0),
	?unicast(RoleID, #sc_family_leave{result=0, is_self=true, family_info=family_misc:gen_p_family_info()}),
    notify_family_member_leave_family(FamilyInfo#family_info.family_id, RoleID),
	family_storage_reqList_update(RoleID),
    {ok, NewFamilyInfo}.

%%  屏蔽工会战boss消息广播
notify_family_member_leave_family(_FamilyID,_RoleID)->
%% 	family_misc:router_to_family_boss_process(FamilyID, {member_leave,RoleID}),
%% 	erlang:send(pvp_server, {role_leave_family,RoleID}),
	%erlang:send(plunder_server, {role_leave_family,RoleID}),
	ok.

notify_role_join_family(_FamilyID,_RoleID) ->
    ok.
%	family_misc:router_to_family_boss_process(FamilyID, {member_join, RoleID}).
%	erlang:send(pvp_server,{role_join_family,RoleID,FamilyID}).
%	erlang:send(plunder_server,{role_join_family,RoleID,FamilyID}).

check_can_leave(FamilyInfo, RoleID,FightEndTime) ->
    #family_info{owner_role_id=OwnerRoleID, members=Members,family_fight_other_info=FamilyFightInfo} = FamilyInfo,
    NowSecond = util:now(),
    ?INFO("Check it's on the war? now:~w end_time:~w ~w",[NowSecond,FightEndTime,NowSecond =< FightEndTime]),
	case FamilyFightInfo#family_fight_other_info.is_sign of
		1 when NowSecond =< FightEndTime ->
			erlang:throw({false,6});
		_ ->
			next
	end,
    case family_misc:lock_family_protect(RoleID, ?PROTECT_FAMILY_LEAVE) of
        true ->
            next;
        false ->
            erlang:throw({false, 5})
    end,
    case lists:keytake(RoleID, #family_member_info.role_id, Members) of
        false ->
            erlang:throw({false, 4});
        {value, KickMember, NewMembers} ->  
            {ok, KickMember, NewMembers, RoleID =:= OwnerRoleID}
    end.

do_kick(FamilyInfo, RoleID, KickRoleID,FightEndTime) ->
	case catch check_can_kick(FamilyInfo, RoleID, KickRoleID,FightEndTime) of
		{ok, KickMember, NewMembers} ->
			NewMembers2 = clear_energy_sended_by_role(KickRoleID, NewMembers),
			do_kick2(KickMember, NewMembers2, FamilyInfo, RoleID, KickRoleID);
		{false, Reason} ->
			case Reason of
                5 ->
                    next;
                _ ->
                    family_misc:unlock_family_protect(KickRoleID)
            end,
            ?unicast(RoleID, #sc_family_kick{result=Reason, is_self=true, family_info=family_misc:gen_p_family_info()}),
            {ok, FamilyInfo}
    end.

do_kick2(KickMember, NewMembers, FamilyInfo, RoleID, KickRoleID) ->
	RoleMember = lists:keyfind(RoleID, #family_member_info.role_id, NewMembers),
    if
        is_binary(KickMember#family_member_info.role_name)->
            NewLog = rebuild_family_log(RoleMember#family_member_info.role_id,
                                            RoleMember#family_member_info.role_name,
                                            RoleMember#family_member_info.head,
                                            RoleMember#family_member_info.role_level,?MAMILY_LOG_TYPE_KICK,
                                            binary_to_list(KickMember#family_member_info.role_name), FamilyInfo#family_info.log_data,
                                            RoleMember#family_member_info.title,
                                            RoleMember#family_member_info.is_male);
        is_list(KickMember#family_member_info.role_name)->
            NewLog = rebuild_family_log(RoleMember#family_member_info.role_id,
                                            RoleMember#family_member_info.role_name,
                                            RoleMember#family_member_info.head,
                                            RoleMember#family_member_info.role_level,?MAMILY_LOG_TYPE_KICK,
                                            KickMember#family_member_info.role_name, FamilyInfo#family_info.log_data,
                                            RoleMember#family_member_info.title,
                                            RoleMember#family_member_info.is_male);
        true ->
            ?ERR("KickMember#family_member_info.role_name error. Data type is unknown ~w",[KickMember#family_member_info.role_name]),
            NewLog = rebuild_family_log(RoleMember#family_member_info.role_id,
                                            RoleMember#family_member_info.role_name,
                                            RoleMember#family_member_info.head,
                                            RoleMember#family_member_info.role_level,?MAMILY_LOG_TYPE_KICK,
                                            "", FamilyInfo#family_info.log_data,
                                            RoleMember#family_member_info.title,
                                            RoleMember#family_member_info.is_male)
    end,
    NewFamilyInfo = FamilyInfo#family_info{members=NewMembers, cur_members=erlang:length(NewMembers),log_data=NewLog},
    db_sql:del_family_member(NewFamilyInfo#family_info.family_id, KickRoleID),
    db_sql:update_family_info(NewFamilyInfo),
    PFamilyInfo = family_misc:to_p_family_info(NewFamilyInfo),
    ?unicast(RoleID, #sc_family_kick{result=0, is_self=true, family_info=PFamilyInfo}),
    ?unicast(KickRoleID, #sc_family_kick{result=0, is_self=false, family_info=family_misc:gen_p_family_info()}),
    update_online(KickRoleID, false),
    do_bc_msg_except(#sc_family_kick{result=0, is_self=false, family_info=PFamilyInfo}, [RoleID, KickRoleID]),
    erlang:send(family_manager_server, {kick, KickRoleID}),
	notify_family_member_leave_family(FamilyInfo#family_info.family_id, KickRoleID),
	family_extra_handler:update_role_tekInfo(KickRoleID),
    delete_contribute_record(KickRoleID),
	family_storage_reqList_update(KickRoleID),
    catch mail_server:send_sys_mail(KickRoleID, ?MAIL_FAMILY_BE_KICK, [], "", []),

    {ok, NewFamilyInfo}.

check_can_kick(FamilyInfo, RoleID, KickRoleID,FightEndTime) ->
    #family_info{members=Members,family_fight_other_info=FamilyFightInfo} = FamilyInfo,
    NowSecond = util:now(),
    ?INFO("Check it's on the war? now:~w end_time:~w ~w",[NowSecond,FightEndTime,NowSecond =< FightEndTime]),
	case FamilyFightInfo#family_fight_other_info.is_sign of
		1 when NowSecond =< FightEndTime ->
			erlang:throw({false,6});
		_ ->
			next
	end,
    case family_misc:lock_family_protect(KickRoleID, ?PROTECT_FAMILY_KICK) of
        true ->
            next;
        false ->
            erlang:throw({false, 5})
    end,
    case RoleID =:= KickRoleID of
        true ->
            erlang:throw({false, 2});
        false ->
            next
    end,
	case check_role_can_agree_or_kick(get_member_title(RoleID,Members)) of
%%     case RoleID =:= OwnerRoleID of
        true ->
            next;
        false ->
            erlang:throw({false, 3})
    end,	
    case lists:keytake(KickRoleID, #family_member_info.role_id, Members) of
        false ->
            erlang:throw({false, 4});
        {value, KickMember, NewMembers} ->
            {ok, KickMember, NewMembers}
    end.

do_change_notice(RoleID, Notice, FamilyInfo) ->
    case catch check_can_change_notice(RoleID, Notice, FamilyInfo) of
        {ok, NewFamilyInfo} ->
            ?unicast(RoleID, #sc_family_change_notice{result=0,is_self=true, notice=Notice}),
            do_bc_msg_except(#sc_family_change_notice{result=0,is_self=false, notice=Notice}, [RoleID]),
            {ok, NewFamilyInfo};
        {false, Reason} ->
            ?unicast(RoleID, #sc_family_change_notice{result=Reason, is_self=true, notice= <<"">>}),
            {ok, FamilyInfo}
    end.

check_can_change_notice(RoleID, Notice, FamilyInfo) ->
    #family_info{members=Members} = FamilyInfo,
	case check_role_can_change_notice(get_member_title(RoleID,Members)) of
		false ->
			erlang:throw({false, 3});
		true ->
			next
	end,
	Notice2 = util:words_filter(Notice),
    {ok, FamilyInfo#family_info{notice=Notice2}}.

do_change_slogan(RoleID, Slogan, FamilyInfo) ->
    case catch check_can_change_slogan(RoleID, Slogan, FamilyInfo) of
        {ok, NewFamilyInfo} ->
            ?unicast(RoleID, #sc_family_change_slogan{result=0,is_self=true, slogan=Slogan}),
            do_bc_msg_except(#sc_family_change_slogan{result=0,is_self=false, slogan=Slogan}, [RoleID]),
            {ok, NewFamilyInfo};
        {false, Reason} ->
            ?unicast(RoleID, #sc_family_change_slogan{result=Reason, is_self=true, slogan= <<"">>}),
            {ok, FamilyInfo}
    end.

check_can_change_slogan(RoleID, Slogan, FamilyInfo) ->
    #family_info{members=Members} = FamilyInfo,
	case check_role_can_change_slogan(get_member_title(RoleID,Members)) of
		false ->
			erlang:throw({false, 3});
		true ->
			next
	end,
    {ok, FamilyInfo#family_info{slogan=Slogan}}.

do_bc(Data, FamilyInfo) ->
    IsFamilyTalkMessage = erlang:is_record(Data, sc_talk_world_message),
    Data2 =
        case IsFamilyTalkMessage of
            false ->
                Data;
            true ->
                #sc_talk_world_message{roleID=RoleID} = Data,
                Data#sc_talk_world_message{familyTitle=get_role_family_title(RoleID, FamilyInfo)}
        end,
    lists:foreach(fun(RoleID) ->
                          ?unicast_async(RoleID, Data2)
                  end, erlang:get(?online_role_id_list)),
    case IsFamilyTalkMessage of
        false ->
            {ok, FamilyInfo};
        true ->
            save_recent_talk_data(Data2, FamilyInfo)
    end.

do_bc_msg_except(Msg, ExceptRoleIDList) ->
    lists:foreach(fun(RoleID) ->
                          case lists:member(RoleID, ExceptRoleIDList) of
                              false ->
                                  ?unicast_async(RoleID, Msg);
                              true ->
                                  next
                          end
                  end, erlang:get(?online_role_id_list)).

save_recent_talk_data(Data, #family_info{talk_data=DataList}=FamilyInfo) ->
    NewDataList = lists:sublist([Data|DataList], ?RECENT_TALK_DATA_NUM),
    {ok, FamilyInfo#family_info{talk_data=NewDataList}}.
    
get_role_family_title(RoleID, FamilyInfo) ->
    #family_info{members=Members} = FamilyInfo,
    case lists:keyfind(RoleID, #family_member_info.role_id, Members) of
        false ->
            0;
        #family_member_info{family_title=FamilyTitle} ->
            FamilyTitle
    end.

update_family_member_info(FamilyInfo) ->
    #family_info{members=Members} = FamilyInfo,
    NewMembers =
        lists:foldr(fun(#family_member_info{role_id=RoleID}=Member, Acc) ->
                            NewMember = update_family_member_info(RoleID, Member),
                            [NewMember|Acc]
                    end, [], Members),
    FamilyInfo#family_info{members=NewMembers}.

update_family_member_info(RoleID, Member) ->
    case role_lib:get_rolePublic(RoleID) of
        #rolePublic{level=Level,title=Title,fightPower=FightPower,viplevel=VipLevel,svipLevel=SVipLevel} ->
                        Vip = role_lib:cacl_vip_info(VipLevel,SVipLevel),
            Member#family_member_info{role_level=Level, title=Title, fight_power=FightPower,vip=Vip};
        _ ->
            Member
    end.

get_contribute_reward_list()->
	lists:foldl(fun({ID,CType,CValue, RRepu,RCT,Rice},Acc)->
							[#p_familyContributeType{typeID=ID
												 ,needType=CType
												 ,needValue=CValue
												 ,getReward=activity_server:sell_reward2p_reward_info(RRepu)
												 ,getContribute=RCT
												 ,getRice=Rice}|Acc]
						end, [], data_family:get(contributeTypeList)).

do_family_contribute({cs_family_do_contribute, RoleID, Contribution,RoleName,TypeID,ConsumeType,ConsumeValue,Reward,Rice}, FamilyInfo)->
	#family_info{members=Members,level=OldLevel,family_id=FamilyID,owner_role_name=OwnerName}=FamilyInfo,
	{value,#family_member_info{lastContributeDate=LastContributeDate,family_contribution=OldContribution,weeklyContributes=WeeklyContribution,join_time=JoinTime} = MemberInfo,OtherMembers}
		=lists:keytake(RoleID,#family_member_info.role_id,Members),
	case is_today(JoinTime) of
		true ->
			?unicast(RoleID, #sc_family_do_contribute{result=7}),
			FamilyInfo;
		false ->
			Today = erlang:date(),
			case LastContributeDate >= Today of
				true ->
					Level = OldLevel,
					FamilyInfo2=FamilyInfo,
					?unicast(RoleID, #sc_family_do_contribute{result=6});
				false ->
					Wallet = family_data:get_family_wallet(),
					family_lib:add_family_wallet(rice, Rice,Wallet,FamilyID,?MONEY_ADD_TYPE_FAMILY_CONTRIBUTIOIN,TypeID,""),
					ActivePoints = FamilyInfo#family_info.active_points,
					ExpT = Contribution + ActivePoints,
					MaxExp = data_family:get(familyMaxLevelExp),
					WeeklyContributionT = WeeklyContribution,
					{Exp, Result,FamilyContribution,WeeklyContributin2} = 
						case ExpT >= MaxExp of
							true ->
								{MaxExp,4,OldContribution+Contribution,WeeklyContributionT+Contribution};
							_ ->
								{ExpT,0,OldContribution+Contribution,WeeklyContributionT+Contribution}
						end,
					Level = data_family_exp:get(Exp), %% 根据当前经验推算出等级
					
					case catch role_lib:send_server(RoleID,{role_family_do_contribute,ConsumeType, ConsumeValue,TypeID,Reward}) of
						{'EXIT',{badarg,_}} ->
							FamilyInfo2 = FamilyInfo;
						_ ->
							%%					do_bc_msg_except(#sc_family_update_exp{expValue=Exp,level=Level}, []),
							
							%% 					CTLog = lists:reverse(lists:sublist(lists:reverse([#p_familyContributeLog{contributerName=RoleName,getTypeID=TypeID}|FamilyInfo#family_info.contribute_log])
							%% 										  , 1,data_family:get(contributeNoticeNum))),
							CTLog = lists:sublist([#p_familyContributeLog{contributerName=RoleName,getTypeID=TypeID}|FamilyInfo#family_info.contribute_log], data_family:get(contributeNoticeNum)),
							Members2 = [MemberInfo#family_member_info{lastContributeDate=Today,family_contribution=FamilyContribution,weeklyContributes=WeeklyContributin2}|OtherMembers],
							FamilyInfo2 = FamilyInfo#family_info{active_points=Exp
																 ,level=Level
																 ,members=Members2
																 ,contribute_log=CTLog},
							do_bc_msg_except(#sc_family_update_family_info{info=family_misc:to_p_family_info(FamilyInfo2)}, []),
							do_bc_msg_except(#sc_family_update_contribute_log{ctLogList=CTLog},[]),
							%erlang:send(family_manager_server, {update_family_info, FamilyInfo2}),
							?unicast(RoleID, #sc_family_do_contribute{result=Result})
					end
			end,
			FamilyInfo3 = 
				if Level > OldLevel ->
					   CTLog2 = lists:sublist([#p_familyContributeLog{contributerName=integer_to_list(Level),getTypeID=0}|FamilyInfo2#family_info.contribute_log], data_family:get(contributeNoticeNum)),
					   FamilyInfo2T = FamilyInfo2#family_info{contribute_log=CTLog2},
					   do_bc_msg_except(#sc_family_update_contribute_log{ctLogList=CTLog2},[]),
					   notify_family_changed(FamilyID,OwnerName,Level),
					   %?CATCH(family_boss_server:call_boss_server(FamilyID,{family_levelup, Level})),
					   family_misc:router_to_family_boss_process(FamilyID, {update_family_level, Level}),
                       FamilyInfo2T;
				   true ->
					   FamilyInfo2
				end,
			FamilyInfo3
	end.

do_change_member_power(RoleID,ChangeRoleID,TypeID, FamilyInfo)->
	#family_info{members=Members,level=FamilyLevel,family_id=FamilyID} = FamilyInfo,
    %% 检查执行者是否在公会中
	case lists:keyfind(RoleID,#family_member_info.role_id, Members) of
		false ->
			NewFamilyInfo = FamilyInfo,
			?unicast(RoleID,#sc_family_change_member_power{result=5});
		#family_member_info{family_title=RoleTitle,role_name=RoleName,head=Head,role_level=Level,title=Title,is_male=IsMale}=OwnerInfo ->
            %% 检查被执行者是否在公会中
			case lists:keytake(ChangeRoleID, #family_member_info.role_id, Members) of
				false ->
					NewFamilyInfo = FamilyInfo,
					?unicast(RoleID, #sc_family_change_member_power{result=6});
                %% 找到被执行者，取出详细信息
				{value, ChangeRoleInfo=#family_member_info{family_title=ChangeRoleTitle,role_name=ChangeRoleName,join_time=JoinTime}, OtherMembers} ->
					case check_role_can_change_other_power(RoleTitle, TypeID,ChangeRoleTitle,JoinTime) of
						{true, NewTitle} ->
							if NewTitle =:= 1 ->
								   NewMembers = [OwnerInfo#family_member_info{family_title = ?FAMILY_TITLE_MEMBER}
																			 , ChangeRoleInfo#family_member_info{family_title = ?FAMILY_TITLE_OWNER}
																													|lists:keydelete(RoleID, #family_member_info.role_id,OtherMembers)],
								   NewLog = rebuild_family_log(RoleID,RoleName,Head,Level,?MAMILY_LOG_TYPE_CHANGE_LEADER,ChangeRoleName, FamilyInfo#family_info.log_data,Title,IsMale),
                                   NewFamilyInfo = FamilyInfo#family_info{members=NewMembers,owner_role_id=ChangeRoleID,owner_role_name=ChangeRoleName,log_data=NewLog},
								   do_bc_msg_except(#sc_family_update_family_info{info=family_misc:to_p_family_info(NewFamilyInfo)}, []),
								   %erlang:send(family_manager_server, {update_family_info, NewFamilyInfo}),
								   notify_family_changed(FamilyID,ChangeRoleName,FamilyLevel),
								   ?unicast(ChangeRoleID,#sc_family_get_member_power{memberPowerList=get_role_power_list(NewTitle)}),
								   ?unicast(RoleID,#sc_family_get_member_power{memberPowerList=get_role_power_list(?FAMILY_TITLE_MEMBER)}),
                                   ?unicast(RoleID, #sc_family_change_member_power{result=1});
							   true ->
								   TitleCount = get_title_count(OtherMembers,NewTitle),
								   MaxTitleCount = get_max_title_count(NewTitle, FamilyLevel),
								   case TitleCount+1 =< MaxTitleCount of
									   true ->
										   NewMemberInfo=ChangeRoleInfo#family_member_info{family_title = NewTitle},
                                           if
                                               NewTitle > ChangeRoleTitle ->
                                                   NewLog = rebuild_family_log(RoleID,RoleName,Head,Level,?MAMILY_LOG_TYPE_JOBUP,ChangeRoleName, FamilyInfo#family_info.log_data,Title,IsMale);
                                               true ->
                                                   NewLog = rebuild_family_log(RoleID,RoleName,Head,Level,?MAMILY_LOG_TYPE_JOBDOWN,ChangeRoleName, FamilyInfo#family_info.log_data,Title,IsMale)
                                           end,
										   NewFamilyInfo = FamilyInfo#family_info{members=[NewMemberInfo|OtherMembers],log_data=NewLog},
                                           do_bc_msg_except(#sc_family_update_family_info{info=family_misc:to_p_family_info(NewFamilyInfo)}, []),
										   %erlang:send(family_manager_server, {update_family_info, NewFamilyInfo}),
										   ?unicast(ChangeRoleID, #sc_family_get_member_power{memberPowerList=get_role_power_list(NewTitle)}),

                                           ?unicast(RoleID, #sc_family_change_member_power{result=1});
									   _ ->
										   NewFamilyInfo = FamilyInfo,
										   ?unicast(RoleID, #sc_family_change_member_power{result=7})
								   end
							end;
						{false,2}->
							NewFamilyInfo=FamilyInfo,
							?unicast(RoleID,#sc_family_change_member_power{result=8});
						_ ->
							NewFamilyInfo = FamilyInfo,
							?unicast(RoleID, #sc_family_change_member_power{result=3})
					end
			end
	end,
	NewFamilyInfo.


notify_family_changed(FamilyID,OwnerName,FamilyLevel)->
	catch erlang:send(family_fight_server,{family_changed,FamilyID,OwnerName,FamilyLevel}).

%% rebuild_family_log([], OldLog)->
%% 	OldLog.
%% rebuild_family_log(NewMsg, OldLog)->
%% 	%lists:reverse(lists:sublist(lists:reverse([#p_family_log_dtl{timeStamp=util:now(), logMsg=NewMsg}|OldLog]), 0, data_family:get(max_family_log_num))).
%% 	LogList = lists:sublist([#p_family_log_dtl{timeStamp=util:now(), logMsg=NewMsg}|OldLog], data_family:get(max_family_log_num)),
%% 	do_bc_msg_except(#sc_family_get_log_list{result=0, logDataList=LogList}, []),
%% 	LogList.

%% RoleName,RoleHead,RoleLevel是用于头像的显示，是固定传给客户端的参数
rebuild_family_log(RoleID,RoleName,RoleHead,RoleLevel,Type,Arg1, OldLog,Title,IsMale)->
    %lists:reverse(lists:sublist(lists:reverse([#p_family_log_dtl{timeStamp=util:now(), logMsg=NewMsg}|OldLog]), 0, data_family:get(max_family_log_num))).
    LogList = lists:sublist([#p_family_log_dtl{
                                               roleid = RoleID,
                                               rolename = RoleName,
                                               rolehead = RoleHead,
                                               rolelevel = RoleLevel,
                                               timeStamp = util:now(),
                                               type = Type,
                                               arg_1 = Arg1,
                                               title = Title,
                                               is_male = IsMale}|OldLog], data_family:get(max_family_log_num)),
    do_bc_msg_except(#sc_family_get_log_list{result=0, logDataList=LogList}, []),
    LogList.

do_zero_clock_setting(FamilyInfo) ->
	#family_info{members=Members,is_zero_refreshed=IsRefresh,family_instance_state=FamilyInstanceState} = FamilyInfo,
	Today = erlang:date(),
	case IsRefresh of
		Today ->
			FamilyInfo;
		_ -> 
            %% 这里的逻辑是确保此处的代码每天只执行一次
			case calendar:day_of_the_week(Today) of
				1 ->
					erlang:send(self(),clean_weekly_contributes),
					erlang:send(self(),refresh_family_limit_shop);
				_ ->
					ignore
			end,
            NewFamilyInstanceState = family_data:refresh_instance_boss(FamilyInstanceState,Members),
            %?INFO("do_zero_clock_setting NewFamilyInstanceState:~w",[NewFamilyInstanceState]),
            lists:foreach(fun(RoleID) ->
                      #rolePublic{viplevel=VipLevel}=role_lib:get_rolePublic(RoleID),
                      #data_vip{familyInstanceTimes =FamilyInstanceTimes}=data_vip:get(VipLevel),
                      Msg = get_family_inst_msg(RoleID,FamilyInstanceTimes,NewFamilyInstanceState),
                      ?INFO("get_family_inst_msg msg:~w",[Msg]),
                      ?unicast_async(RoleID, Msg)
              end, erlang:get(?online_role_id_list)),
            
            %% 发送每日膜拜奖励
            send_worship_reward(FamilyInfo),
            %% 清除recvEnergyList、每天膜拜次数、贡献度
			Members2 = lists:foldl(fun(FamilyMember, Acc)->
										   ?unicast(FamilyMember#family_member_info.role_id,#sc_family_get_send_role_energy_list{roleNameList=[]}),
										   ?unicast(FamilyMember#family_member_info.role_id,#sc_family_worship_self_push{times=0}),
										   [FamilyMember#family_member_info{recvEnergyList=[],reward_level=0,left_family_contribution = 0}|Acc] 
								   end, [],Members),
            do_bc_msg_except(#sc_family_worship_family_push{times=0,reward=0},[]),
			FamilyInfo#family_info{members=Members2
                                  ,family_task=role_task:add_all_family_today_task()
                                  ,family_instance_state=NewFamilyInstanceState
                                  ,is_zero_refreshed=erlang:date()}
	end.

check_role_can_change_other_power(Title,Type,Title2,_JoinTime)->
%% 	case is_today(JoinTime) of
%% 		true ->
%% 			{false,2};
%% 		false ->
			check_role_can_change_other_power(Title,Type,Title2).
%% 	end.
check_role_can_change_other_power(1, Type,_)-> %% 盟主
	{true,change_role_power(Type)};
check_role_can_change_other_power(0, _,_)-> %% 成员
	false;
check_role_can_change_other_power(2, Type,0) -> %% 副盟主
	RolePowerList = data_family:get(familyDeputySetting),
	Type2 = calc_change_power_type(Type),
	case lists:member(Type2, RolePowerList)of
		true ->
			{true,change_role_power(Type)};
		_ ->
			false
	end;
check_role_can_change_other_power(_,_,_) ->
	false.

is_today(Sec)->
	{Date,_} = util:seconds_to_datetime(Sec),
	erlang:date() == Date.

check_role_can_sign(1)->
	true;
check_role_can_sign(0)->
	false;
check_role_can_sign(2) ->
	lists:member(5, get_role_power_list(2));
check_role_can_sign(_) ->
	false.

get_role_power_list(0)->
	{_,X} = data_family:get(familyMemberSetting),
	X;
get_role_power_list(1)->
	{_,X} = data_family:get(familyOwnerSetting),
	X;
get_role_power_list(2)->
	{_,X} = data_family:get(familyDeputySetting),
	X;
get_role_power_list(_)->
	[].

change_role_power(1)->
	1;
change_role_power(2) ->
	2;
change_role_power(3)->
	0;
change_role_power(_) ->
	0.

calc_change_power_type(1) ->
	3;
calc_change_power_type(2) ->
	4;
calc_change_power_type(3) ->
	4;
calc_change_power_type(_) ->
	0.

get_title_count(Members,NewTitle)->
	lists:foldl(fun(#family_member_info{family_title=Title},Acc) ->
						if Title =:= NewTitle ->
							   Acc + 1;
						   true ->
							   Acc
						end
				end, 0, Members).

get_max_title_count(1,_) ->
	1;
get_max_title_count(2,_) ->
	{Num,_} = data_family:get(familyDeputySetting),
	Num;
get_max_title_count(_,FamilyLevel) ->
	 data_family:get({max_role_num, FamilyLevel}).

check_role_can_agree_or_kick(1) ->
	true;
check_role_can_agree_or_kick(0) ->
	false;
check_role_can_agree_or_kick(2) ->
	lists:member(4, get_role_power_list(2));
check_role_can_agree_or_kick(_) ->
	false.

check_role_can_change_notice(1)->
	true;
check_role_can_change_notice(0)->
	false;
check_role_can_change_notice(2) ->
	lists:member(7, get_role_power_list(2));
check_role_can_change_notice(_) ->
	false.

check_role_can_change_slogan(1)->
	true;
check_role_can_change_slogan(0)->
	false;
check_role_can_change_slogan(2) ->
	lists:member(7, get_role_power_list(2));
check_role_can_change_slogan(_) ->
	false.

check_role_can_change_invite(1)->
    true;
check_role_can_change_invite(0)->
    false;
check_role_can_change_invite(2)->
    true; %这里没有要求权限配置化，所以此处固定，副会长有邀请的权利    
check_role_can_change_invite(_)->
    false.

check_role_can_select_instance(1)->
    true;
check_role_can_select_instance(0)->
    false;
check_role_can_select_instance(2)->
    true; %这里没有要求权限配置化，所以此处固定，副会长有邀请的权利    
check_role_can_select_instance(_)->
    false.

check_invite_request(SendRoleID,TarRoleID,Members,FamilyID)->
    %检查是否有权限
    case check_role_can_change_invite(get_member_title(SendRoleID,Members)) of
        false ->
            {fail,6}; %没有权限
        true ->
            %看一下被邀请者，是不是已经在公会中了
            IsJoinRoleOnline = role_lib:is_online(TarRoleID),
            JoinRoleInfo = 
                case IsJoinRoleOnline of
                    true ->
                        catch role_lib:call_server(TarRoleID, get_role_info,1000);
                    false ->
                        db_sql:get_roleInfo(TarRoleID)
                end,
            #role{level=Level,familyID=TarFamilyID,lastJoinFamily=LastJoinFamily}=JoinRoleInfo,
            ?INFO("check_invite_request ~w ~w ~w",[Level,TarFamilyID,LastJoinFamily]),
            OpenLevel = data_family:get(family_open_level),
            if
                OpenLevel > Level ->
                    {fail,9}; %已经申请了这个公会
                TarFamilyID =:= 0 ->
                    case LastJoinFamily + data_family:get(join_protect_seconds) >= util:now() of
                        true ->
                            {fail,4}; %已经申请了这个公会
                        false ->
                            %看一下，被邀请者，是不是已经申请了我们公会
                            case ets:match_object(?ETS_FAMILY_REQUEST, #family_request{role_id=TarRoleID, family_id=TarFamilyID, _='_'}) of
                                [] ->
                                    case ets:lookup(?ETS_FAMILY_INVITE, {TarRoleID,FamilyID}) of
                                        []->
                                            ok;
                                        [_]->
                                            {fail,7}
                                    end;
                                _FamilyRequestList ->
                                    {fail,3} %已经申请了这个公会
                            end
                    end;
                true ->
                    case FamilyID =/= TarFamilyID of
                        true->
                            {fail,8};
                        false->
                            {fail,2} %对方已经在公会中
                    end
            end
    end.

check_role_can_impeach_owner(RoleID,FamilyInfo) ->
    case get_contribution_top_n(FamilyInfo) of
        [] ->
            ?ERR("公会的历史活跃度前N为空 ~n"),
            {false,4};
        TopNList ->
            case is_list(TopNList) of 
                true ->
                    ?INFO("当前的排序：~w~n",[TopNList]),
                    case lists:member(RoleID,TopNList) of
                        true ->
                            true;
                        false ->
                            {false,3} 
                    end;
                false ->
                    {false,4}
            end
    end.



check_role_familyBoss_power2(1,_) ->
	true;
check_role_familyBoss_power2(0,_) ->
	false;
check_role_familyBoss_power2(2,hatch)->
	{_,PowerList} = data_family:get(familyDeputySetting),
	lists:member(8, PowerList);
check_role_familyBoss_power2(2,set_time)->
	{_,PowerList} = data_family:get(familyDeputySetting),
	lists:member(8, PowerList);
check_role_familyBoss_power2(_,_)->
	false.

get_member_title(RoleID,Members) ->
	#family_member_info{family_title=Title} = lists:keyfind(RoleID, #family_member_info.role_id, Members),
	Title.

clean_weekly_contributes(Members)->
	Monday = get_week_monday(),
	lists:foldl(fun(#family_member_info{lastContributeDate=LCD}=Info, Acc)-> 
						case LCD < Monday of
							true ->
								[Info#family_member_info{weeklyContributes=0}|Acc];
							_ ->
								[Info|Acc]
						end
				end, [], Members).

get_week_monday()->
	{Date,_} = util:seconds_to_datetime(util:now() - ?ONE_DAY_SECONDS * (calendar:day_of_the_week(date()) - 1)),
	Date.
	
clear_energy_sended_by_role(RoleID, Members)->
	lists:foldl(fun(#family_member_info{recvEnergyList=List}=Member,Acc)->
						List2 = lists:keydelete(RoleID, 1, List),
						[Member#family_member_info{recvEnergyList=List2}|Acc]
				end, [], Members).

get_new_title_msg(?FAMILY_TITLE_DEPUTY,RoleName,ChangeRoleName)->
	{Msg1,Msg2,Msg3} = data_family:get(rise_log_msg),
	binary_to_list(RoleName) ++ Msg1 ++ binary_to_list(ChangeRoleName) ++ Msg2 ++ data_family:get(deputy_title) ++ Msg3 ;
get_new_title_msg(?FAMILY_TITLE_MEMBER, RoleName, ChangeRoleName)->
	{Msg1,Msg2,Msg3} = data_family:get(down_log_msg),
	binary_to_list(RoleName) ++ Msg1 ++ binary_to_list(ChangeRoleName) ++ Msg2 ++ data_family:get(member_title) ++ Msg3 ;
get_new_title_msg(_, _, _)->
	[].
	
get_max_attackTimes()->
	data_family_fight:get(max_attack_times).

get_max_defendTimes()->	
	data_family_fight:get(max_defend_times).

do_feed_family_boss({cs_familyBoss_feed_boss, Pos,RoleID}=Msg,FamilyInfo)->
	#family_info{family_id=FamilyID,members=Members} = FamilyInfo,
	BossLevel = family_boss_server:call_boss_server(FamilyID,{get_boss_level,Pos}),
	CostNum = family_boss_server:get_upLevel_cost(Pos,BossLevel),
	case family_lib:check_family_cost(rice,CostNum) of
		{true,Wallet} ->
			case check_role_familyBoss_power(lists:keyfind(RoleID, #family_member_info.role_id, Members),hatch) of
				true ->
					case catch family_boss_server:call_boss_server(FamilyID,Msg) of
						{'EXIT', _} ->
							?unicast(RoleID, #sc_familyBoss_feed_boss{result=8});
						1 ->
							%Wallet = family_data:get_family_wallet(),
							family_lib:deduct_family_wallet(rice,CostNum,Wallet,FamilyID,?MONEY_DEC_TYPE_FEED_BOSS,Pos,""),
							?unicast(RoleID,#sc_familyBoss_feed_boss{result=1});
						Result ->
							?unicast(RoleID,#sc_familyBoss_feed_boss{result=Result})
					end;
				_ ->
					?unicast(RoleID,#sc_familyBoss_feed_boss{result=7})
			end;
		false ->
			?unicast(RoleID,#sc_familyBoss_feed_boss{result=9})
	end.

check_exp_to_up_level(AddExp,FamilyInfo=#family_info{family_id=FamilyID,owner_role_name=OwnerRoleName,level=OldLevel,active_points=OldExp})->
	NewExp = OldExp + AddExp,
	NewLevel = data_family_exp:get(NewExp), %% 根据当前经验推算出等级
	case NewLevel > OldLevel of
		false ->
			FamilyInfo#family_info{level=NewLevel,active_points=NewExp};
		true ->	
            case lists:keyfind(FamilyInfo#family_info.owner_role_id, #family_member_info.role_id, FamilyInfo#family_info.members) of
                false ->
                    NewLog = FamilyInfo#family_info.log_data;
                Member ->
                    NewLog = rebuild_family_log(Member#family_member_info.role_id,
                        Member#family_member_info.role_name,
                        Member#family_member_info.head,
                        Member#family_member_info.role_level,?MAMILY_LOG_TYPE_LEVEL_UP,
                        integer_to_list(NewLevel), FamilyInfo#family_info.log_data,
                        Member#family_member_info.title,
                        Member#family_member_info.is_male)
            end,
			notify_family_changed(FamilyID,OwnerRoleName,NewLevel),
            do_bc_msg_except(#sc_family_levelup{familyLevel=NewLevel},[]),
			family_misc:router_to_family_boss_process(FamilyID, {update_family_level, NewLevel}),
			FamilyInfo#family_info{level=NewLevel,active_points=NewExp,
									 log_data=NewLog}
	end.

% 获得公会膜拜相关奖励
get_worship_reward_info(Members, FamilyLevel) ->
    {TotalTimes, RewardMembers} = lists:foldl(fun(#family_member_info{role_id=RoleId, reward_level=RewardLevel},{Times,IdList}=Acc) ->
                                                case RewardLevel of 
                                                    0 ->
                                                        Acc;
                                                    _ ->
                                                        {Times+RewardLevel, [RoleId|IdList]}
                                                end
                                               end, {0, []}, Members),
    %% 当公会膜拜次数达到上限后,玩家依然可以进行膜拜,因此会出现总次数大于上限的情况,这里需要取min.
    TotalTimesT = min(TotalTimes, data_family_daily_reward:get(total_worship_times)),
    RewardValue = data_family_daily_reward:get(worship_reward_base1) * TotalTimesT + data_family_daily_reward:get(worship_reward_base2) * (FamilyLevel - 1),
    {TotalTimesT, RewardValue, RewardMembers}. 

%% 发送公会膜拜奖励
send_worship_reward(#family_info{members=Members, level=FamilyLevel}) ->
    {TotalTimes, RewardValue, RewardMembers} = get_worship_reward_info(Members, FamilyLevel),
    lists:foreach(fun(RoleID) ->
                    mail_server:send_sys_mail(RoleID, ?MAIL_FAMILY_WORSHIP_REWARD, [TotalTimes, RewardValue], "", #sell_reward{gold=RewardValue})
                    end, RewardMembers).

% 获得公会总膜拜次数, 当这个值达到上限后, 成员依然可以膜拜。
% 所以会出现总值大于配置上限,因此最后需要取min返回。
get_worship_total_times(#family_info{members=Members}) ->
    Total = lists:foldl(fun(#family_member_info{reward_level=RewardLevel}, Acc) ->
		                    case RewardLevel of
		                        0 ->
		                            Acc;
		                        _ ->
		                            Acc + RewardLevel
		                    end
		                end, 0, Members),
    min(Total,data_family_daily_reward:get(total_worship_times)).

% 随机一个膜拜任务出来
random_worship_task(Level) ->
    case data_family_daily_reward:get(worship_type_probability) of
        undefined ->
            false;
        [] ->
            false;
        List0 ->
			case lists:keyfind(Level,1,List0) of
				false ->
					false;
				{_,List} ->
					TypeTotalWeight = util:get_total_weight(List),
					Value = random:uniform(TypeTotalWeight),
					case chose_from_weight_list(List, Value, 0) of
						false ->
							false;
						{_, Type} ->  
							random_worship_task_id(Type,Level)
					end
			end
    end.

chose_from_weight_list([], _, _ )  ->
    ?ERR("公会膜拜的任务类型配置异常"),
    false;
chose_from_weight_list([H|T], Value, Acc) ->
    NewAcc = Acc + erlang:element(1, H),
    case NewAcc >= Value of
        true ->
            H;
        false ->
            chose_from_weight_list(T, Value, NewAcc)
    end.
    
% 从一类任务中随机一个任务ID出来
random_worship_task_id(Type,Level) ->
    case data_family_daily_reward:get({worship_task_probability,Type}) of
        undefined ->
			false;
		[] ->
			false;
		List0 ->
			case lists:keyfind(Level,1,List0) of
				false ->
					false;
				{_,List} ->
					IdTotalWeight = util:get_total_weight(List),
					Value = random:uniform(IdTotalWeight),
					case chose_from_weight_list(List, Value, 0) of
						false ->
							false;
						{_,TaskId} ->
							{true, TaskId}
					end
			end
    end.

% 获得公会当前等级对应的挑战关卡
get_fight_dungeonid(FamilyLevel) ->
    case data_family_daily_reward:get(worship_fight_level) of
        undefined ->
            false;
        [] ->
            false;
        List ->
            LevelList = lists:reverse(List),
            case util:fun_take(fun({Level,_}) ->
                                FamilyLevel >= Level end, LevelList) of
                {value, {_,DungeonId}, _} ->
                    DungeonId;
                false ->
                    %比第一个还小,取第一个
                    {_,DungeonId} = hd(List),
                    DungeonId
            end
    end.

% 进行挑战
do_family_worship_fight(RoleID,PosList,LieuAddAttr,TalentList,TrSpecial,DungeonId) ->
    case data_dungeon:get(DungeonId) of
        undefined ->
            ?ERR("公会挑战钻石兽关卡配置出错!"),
            ?unicast(RoleID, #sc_family_worship_fight{result=4,reward=0});
        [] -> 
            ?ERR("公会挑战钻石兽关卡配置出错!"),
            ?unicast(RoleID, #sc_family_worship_fight{result=4,reward=0});
        #data_dungeon{dungeonID=DungeonId} = Info ->
            case get({dungeonID, DungeonId}) of
                {MonTeam,TotalHp} ->
                    next;
                undefined ->
                    MonTeam = get_mon_list(Info),
                    TotalHp = calc_mon_totalhp(MonTeam),
                    put({dungeonID,DungeonId},{MonTeam,TotalHp})
            end,
            ASkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
            EquipList = role_data:get_otherRoleItemEquips(RoleID),
            %%将数据库中获得的精灵装备列表按照精灵分类
            GerEquipList = role_item:assort_ger_equiplist(EquipList),
            LegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
            Pid = self( ),
            spawn( fun( ) ->
                        {Result, FightRecord, {_,_,_,NewMonList}} = role_fight:new(RoleID, PosList, MonTeam, LieuAddAttr, #add_attr{}, TalentList, [],TrSpecial,#trSpecial{}, false,ASkinInfo,#skin_info{},LegendAddList,[]),
                        erlang:send(Pid, {family_worship_fight_succ, RoleID, Result, FightRecord, NewMonList, TotalHp})
                    end )
    end.

% 获得对战列表
get_mon_list(DungeonInfo) ->
    #data_dungeon{gerID2=MonList,gerID3=IDList} = DungeonInfo,
    F = fun(E) ->
                GerTypeID = E#mon.gerTypeID,
                GerQuality= E#mon.gerQuality,
                ger_attr:new_mon(GerTypeID, E#mon.gerLevel, GerQuality, [], lists:delete(GerTypeID, IDList))
        end,
    [?change_pos(Ger,Pos)||{Pos,Mon} <- MonList, is_record(Mon, mon), Ger <-[F(Mon)], is_record(Ger,ger)].

% 获得钻石兽总血量
calc_mon_totalhp(MonTeam) ->
    lists:foldl(fun(#ger{gerAttr=GerAttr}, HpAcc) ->
                        HpAcc + GerAttr#gerAttr.gerHpMax
                    end, 0, MonTeam).


family_disapach_task(TriggerID,TriggerData,TailTaskList) ->
    family_disapach_task(TriggerID,TriggerData,TailTaskList,[]).
family_disapach_task(_TriggerID,_TriggerData,[],ResTailTaskList)->
    ResTailTaskList;

family_disapach_task(TriggerID,TriggerData,[Task|TailTaskList],ResTailTaskList)->
    TaskConfig = data_task:get(Task#r_task.task_id),
    case TaskConfig of
        ?undefined ->
            family_disapach_task(TriggerID,TriggerData,TailTaskList,ResTailTaskList);
        #data_task{} ->
            #data_task{trigger_id=ConfigTriggerID,trigger_num=ConfigNum,trigger_int_list=ConfigIntList,task_type=ConfigTaskType} = TaskConfig,
            if
                TriggerID =:= ConfigTriggerID andalso ConfigTaskType =:= ?TASK_TYPE_FAMILY_TODAY andalso Task#r_task.status =:= ?TASK_STATUS_WAS_ACCEPT ->
                    NewTask = family_trigger_task(TriggerID,Task,TriggerData,ConfigNum,ConfigIntList),
                    family_disapach_task(TriggerID,TriggerData,TailTaskList,[NewTask|ResTailTaskList]);
                true ->
                    family_disapach_task(TriggerID,TriggerData,TailTaskList,[Task|ResTailTaskList])
            end
    end.

family_trigger_task(TriggerID,Task,TriggerData,ConfigNum,ConfigIntList)->
    %% 取得配置信息
    #r_task{trigger_num=CurrNum,trigger_notes=TriggerNotes} = Task,  
    
    case role_task_trigger:check_data(TriggerID,TriggerData,CurrNum,TriggerNotes,ConfigNum,ConfigIntList) of
        true->
            Task#r_task{status=?TASK_STATUS_FINISH};
        {true,NewNum}->
            Task#r_task{status=?TASK_STATUS_FINISH,trigger_num=NewNum};
        {true,NewNum,NewTriggerNotes} ->
            Task#r_task{status=?TASK_STATUS_FINISH,trigger_notes=NewTriggerNotes,trigger_num=NewNum};
        {false,NewNum}->
            Task#r_task{trigger_num=NewNum};
        {false,NewNum,NewTriggerNotes}->
            Task#r_task{trigger_notes=NewTriggerNotes,trigger_num=NewNum};
        false->
            Task
    end.

sync_family_task(NewTaskList)->
    lists:foreach(fun(RoleID) ->
                        catch role_lib:send_server(RoleID, {sync_family_task, NewTaskList})
                  end, erlang:get(?online_role_id_list)).

check_summary(#family_info{family_id=FamilyID}=NewFamilyInfo) ->
    NewSummary = family_misc:to_p_family_summary(NewFamilyInfo),
    case ets:lookup(?ETS_FAMILY_SUMMARY, FamilyID) of
        [] ->
	        erlang:send(family_manager_server, {update_family_info, NewFamilyInfo, NewSummary});
        [OldSummary] ->
            %%剔除不需要校验的字段
            [C1,C2] = 
                lists:map(fun(E) ->
                            E1 = erlang:setelement(#p_family_summary.rank, E, 0),
                            E2 = erlang:setelement(#p_family_summary.is_request, E1, false),
                            E3 = erlang:setelement(#p_family_summary.cross_rank, E2, 0),
                            erlang:setelement(#p_family_summary.serverID, E3, 0)
                          end, [NewSummary, OldSummary]),
            case C1 =:= C2 of
                true ->
                    ignore;
                _ ->
	                erlang:send(family_manager_server, {update_family_info, NewFamilyInfo, NewSummary})
            end
    end.

%获取公会活跃度前N的成员ID列表
get_contribution_top_n(FamilyInfo) ->
    case is_record(FamilyInfo, family_info) of
        true ->
            #family_info{members=Members,owner_role_id=OwnerID} = FamilyInfo,
            SortMembers = lists:sort(fun(A,B)->
                                     A#family_member_info.family_contribution> B#family_member_info.family_contribution
                             orelse (A#family_member_info.family_contribution =:= B#family_member_info.family_contribution andalso A#family_member_info.fight_power > B#family_member_info.fight_power)
                             orelse (A#family_member_info.family_contribution =:= B#family_member_info.family_contribution andalso A#family_member_info.fight_power =:= B#family_member_info.fight_power andalso A#family_member_info.role_level > B#family_member_info.role_level) 
                             orelse (A#family_member_info.family_contribution =:= B#family_member_info.family_contribution andalso A#family_member_info.fight_power =:= B#family_member_info.fight_power andalso A#family_member_info.role_level =:= B#family_member_info.role_level andalso A#family_member_info.join_time<B#family_member_info.join_time)
                            end,Members),
            TopNList = lists:sublist(SortMembers,data_family:get(family_contribution_top_n)+1),
            TopNList2 = case lists:keytake(OwnerID,#family_member_info.role_id,TopNList) of
                {_,_,Others} ->
                    Others;
                false ->
                    lists:sublist(TopNList,data_family:get(family_contribution_top_n))
                end,
            [RoleID||#family_member_info{role_id=RoleID}<-TopNList2];
        false ->
            []
    end.

%%将family_contribution转换成发送协议需要的列表
family_contribution_to_donateInfo(Family_contribution_info)->
    case is_record(Family_contribution_info,family_contribution) of
        true ->
            GerDonateList = [#p_ger_donate_record_unit{star=Star,donateNum=DonateNum}||#donateunit{star=Star,donateNum=DonateNum}<-Family_contribution_info#family_contribution.gerdonateinfo],
            ItemDonateList = [#p_item_donate_record_unit{star=Star,donateNum=DonateNum}||#donateunit{star=Star,donateNum=DonateNum}<-Family_contribution_info#family_contribution.itemdonateinfo],
            #p_donate_info{diamondNum=Family_contribution_info#family_contribution.diamondNum,coinNum=Family_contribution_info#family_contribution.coinNum,reputationNum=Family_contribution_info#family_contribution.reputationNum,gerdonateinfo=GerDonateList,itemdonateinfo=ItemDonateList};
        false ->
            #p_donate_info{diamondNum=0,coinNum=0,reputationNum=0,gerdonateinfo=[],itemdonateinfo=[]}
    end.
 
 %%修改捐献排行列表
update_contribute_record(RoleID,DonateType,DonateNum,ItemTypeID)->
     erlang:send(self(), {add_contribute_record, RoleID,DonateType,DonateNum,ItemTypeID}).

update_contribute_list(Family_contribution_info,Msg)->
    case Msg of 
        {add_contribute_record, RoleID,DonateType,DonateNum,ItemTypeID}->
            add_contribute_record(Family_contribution_info,RoleID,DonateType,DonateNum,ItemTypeID);
        _ ->
            ?INFO("undefined msg :~p ~n ",[Msg])
    end.

%%删除捐献贡献列表中某个玩家的捐献信息
delete_contribute_record(RoleID) ->
    case family_data:get_family_contribute_list() of 
        [] ->
            ignore;
        Family_contribution_info ->
            NewFamily_contribution_info = lists:dropwhile(fun(#family_contribution{role_id=FindRoleID})->
                FindRoleID == RoleID
            end,Family_contribution_info),
            family_data:set_family_contribute_list(NewFamily_contribution_info)
    end.

% %%玩家公会捐献信息
% -record(family_contribution,{role_id=0,diamondNum=0,coinNum=0,reputationNum=0,gerdonateinfo=[],itemdonateinfo=[],donateContribution=0}).

% %%精灵或者道具捐献
% -record(donateunit,{star=0,donateNum=0}).

%%根据捐献类型向contribute_list中添加新的记录
add_contribute_record(Family_contribution_info,RoleID,DonateType,DonateNum,ItemTypeID)->
    Temp_Contribution = case DonateType of
        ?COIN_DONATE -> 
            #family_contribution{role_id=RoleID,coinNum=DonateNum,donateContribution=calculate_contribution(DonateType,0,DonateNum)};
        ?DIAMOND_DONATE ->
            #family_contribution{role_id=RoleID,diamondNum=DonateNum,donateContribution=calculate_contribution(DonateType,0,DonateNum)};
        ?REPUTATION_DONATE ->
            #family_contribution{role_id=RoleID,reputationNum=DonateNum,donateContribution=calculate_contribution(DonateType,0,DonateNum)};
        ?GER_DONATE ->
            #data_ger{gerStar=GerStar}=data_ger:get(ItemTypeID),
            #family_contribution{role_id=RoleID,gerdonateinfo=[#donateunit{star=GerStar,donateNum=DonateNum}],donateContribution=calculate_contribution(DonateType,GerStar,DonateNum)};
        ?ITEM_DONATE ->
            #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
            #family_contribution{role_id=RoleID,itemdonateinfo=[#donateunit{star=ItemStar,donateNum=DonateNum}],donateContribution=calculate_contribution(DonateType,ItemStar,DonateNum)};
        _ ->
            ?INFO("undefined donate type : ~w ~n",[DonateType]),
            #family_contribution{role_id=RoleID}
    end,
    case Family_contribution_info of
        [] ->
            [Temp_Contribution];
        _ ->
            case lists:keytake(RoleID,#family_contribution.role_id,Family_contribution_info) of
                {_,Find_Contribution_Info,Others} ->
                    [merg_contribution(Find_Contribution_Info,Temp_Contribution)|Others];
                false ->
                    [Temp_Contribution|Family_contribution_info]
            end
    end.                     

%%合并family_contribution
merg_contribution(OldContribution,NewContribution) ->
    case OldContribution#family_contribution.role_id == NewContribution#family_contribution.role_id of
        false ->
            ?INFO("出现合并Contribution具有不同的RoleID contribution1:~w contribution2: ~w ~n",[OldContribution,NewContribution]);
        true ->
            #family_contribution{role_id =RoleID,diamondNum=DiamondNum1,coinNum=CoinNum1,reputationNum=ReputationNum1,donateContribution=DonateContribution1,gerdonateinfo=GerDonateInfo1,itemdonateinfo=ItemDonateInfo1} = OldContribution,
            #family_contribution{diamondNum=DiamondNum2,coinNum=CoinNum2,reputationNum=ReputationNum2,donateContribution=DonateContribution2,gerdonateinfo=GerDonateInfo2,itemdonateinfo=ItemDonateInfo2} = NewContribution,
            ResultGerDonateinfo = case GerDonateInfo1 of 
                []->
                    GerDonateInfo2;
                _ ->
                    case GerDonateInfo2 of 
                        []->
                            GerDonateInfo1;
                        _ ->
                            lists:foldl(fun(DonateUnit,Acc)->
                                #donateunit{star=Star,donateNum=DonateNum}=DonateUnit,
                                case lists:keytake(Star,#donateunit.star,Acc) of
                                    false ->
                                        [DonateUnit|Acc];
                                    {_,FindUnit,Others}->
                                        [#donateunit{star=Star,donateNum=FindUnit#donateunit.donateNum+DonateNum}|Others]
                                end
                            end,GerDonateInfo1,GerDonateInfo2)
                    end
            end,
            ResultItemDonateinfo = case ItemDonateInfo1 of 
                []->
                    ItemDonateInfo2;
                _ ->
                    case ItemDonateInfo2 of 
                        []->
                            ItemDonateInfo1;
                        _ ->
                            lists:foldl(fun(DonateUnit,Acc)->
                                #donateunit{star=Star,donateNum=DonateNum}=DonateUnit,
                                case lists:keytake(Star,#donateunit.star,Acc) of
                                    false ->
                                        [DonateUnit|Acc];
                                    {_,FindUnit,Others}->
                                        [#donateunit{star=Star,donateNum=FindUnit#donateunit.donateNum+DonateNum}|Others]
                                end
                            end,ItemDonateInfo1,ItemDonateInfo2)
                    end
            end,
            #family_contribution{role_id=RoleID,diamondNum=DiamondNum1+DiamondNum2,coinNum=CoinNum2+CoinNum1,reputationNum=ReputationNum2+ReputationNum1,gerdonateinfo=ResultGerDonateinfo,itemdonateinfo=ResultItemDonateinfo,donateContribution=DonateContribution1+DonateContribution2}
    end.

%% 根据捐献类型计算公会贡献度
calculate_contribution(DonateType,Star,DonateNum)->
    case DonateType of
        ?COIN_DONATE ->
            {A,B} = data_family:get(coin_contribution),
            trunc(B/A*DonateNum);
        ?DIAMOND_DONATE ->
            {A,B} = data_family:get(diamond_contribution),
            trunc(B/A*DonateNum);
        ?REPUTATION_DONATE ->
            {A,B} = data_family:get(reputation_contribution),
            trunc(B/A*DonateNum);
        ?GER_DONATE ->
            A = data_family:get({ger_contribution,Star}),
            A * DonateNum;
        ?ITEM_DONATE ->
            A = data_family:get({item_contribution,Star}),
            A * DonateNum;
        _ ->
            ?INFO("undefined donatetype: ~w ~n",[DonateType])
    end.


%%根据捐献列表构造公会的捐献简要列表            
get_donate_summary(FamilyInfo) ->

    #family_info{members = Members} = FamilyInfo,
     SortMembers = lists:sort(fun(A,B)->
                                     A#family_member_info.fight_power> B#family_member_info.fight_power
                             orelse (A#family_member_info.fight_power =:= B#family_member_info.fight_power andalso A#family_member_info.role_level > B#family_member_info.role_level)
                            end,Members),

    case family_data:get_family_contribute_list() of
        [] ->
            lists:reverse(lists:foldl(fun(Member,Acc) ->
                #family_member_info{role_id=RoleID,role_name=RoleName,family_title=Family_Title,is_male=Is_Male,role_level=Level,head=Head,title=Title,vip=Vip} = Member,
                [#p_family_memeber_donate_info_summary{roleID=RoleID,family_title=Family_Title,role_name=RoleName,is_male=Is_Male,level=Level,donate_contribution=0,head=Head,title=Title,vip=Vip}|Acc]
            end,[],SortMembers));
        Family_donate_contribution_list ->
            ResultList = lists:foldl(fun(Member,Acc)->
                #family_member_info{role_id=RoleID,role_name=RoleName,family_title=Family_Title,is_male=Is_Male,role_level=Level,head=Head,title=Title,vip=Vip} = Member,
                DonateContribution = case lists:keyfind(RoleID,#family_contribution.role_id,Family_donate_contribution_list) of
                    false ->
                        0;
                    FindMember ->
                        FindMember#family_contribution.donateContribution
                    end,
                Temp_Contribution_Info = #p_family_memeber_donate_info_summary{roleID=RoleID,family_title=Family_Title,role_name=RoleName,is_male=Is_Male,level=Level,donate_contribution=DonateContribution,head=Head,title=Title,vip=Vip},
                [#temp_contribution_unit{memberinfo=Member,contributioninfo=Temp_Contribution_Info}|Acc]
            end,[],SortMembers),
            ResultList2 = lists:sort(fun(A,B) ->
                #temp_contribution_unit{contributioninfo=ContributionInfoA,memberinfo=MemberInfoA} = A,
                #temp_contribution_unit{contributioninfo=ContributionInfoB,memberinfo=MemberInfoB} = B,
                ContributionInfoA#p_family_memeber_donate_info_summary.donate_contribution > ContributionInfoB#p_family_memeber_donate_info_summary.donate_contribution
                orelse (ContributionInfoA#p_family_memeber_donate_info_summary.donate_contribution =:= ContributionInfoB#p_family_memeber_donate_info_summary.donate_contribution andalso MemberInfoA#family_member_info.fight_power > MemberInfoB#family_member_info.fight_power)
                orelse (ContributionInfoA#p_family_memeber_donate_info_summary.donate_contribution =:= ContributionInfoB#p_family_memeber_donate_info_summary.donate_contribution andalso MemberInfoA#family_member_info.fight_power =:= MemberInfoB#family_member_info.fight_power andalso MemberInfoA#family_member_info.role_level > MemberInfoB#family_member_info.role_level)
                end,ResultList),
            [ContributionInfo||#temp_contribution_unit{contributioninfo=ContributionInfo}<-ResultList2]
    end.


%% get_room_id()->
%% 	Url = "http://service.ctalk.cn/socialservice/room/create.do?",
%% 	{GameID,GameAreaNo} = data_setting:get(ctalk_config),
%% 	Args = io_lib:format("grpNums=1&roomType=2&gameId=~s&gameAreaNo=~s&gameServerNo=~w&ver=2.1",[GameID,GameAreaNo,data_setting:get(server_id)]),
%% 	Res = httpc:request(get,{Url++Args,[]},[{timeout,3000},{connect_timeout,3000}],[]),
%% 	case Res of
%% 		{ok, {_,_,Content}} ->
%% 			{Content2} = ejson:decode(Content),
%% 			case get_value(Content2, <<"code">>) of
%% 				1 ->
%% 					[VA] = get_value(Content2,<<"data">>),
%% 					?ERR("create_room_succ,roomID = ~p",[VA]),
%% 					binary_to_list(VA);
%% 				_ ->
%% 					"0"
%% 			end;		
%% 		_ ->
%% 			"0"
%% 	end.
%% delete_room_id(ID) when is_binary(ID) ->
%% 	delete_room_id(erlang:binary_to_list(ID));
%% delete_room_id(ID) when is_list(ID)->
%% 	Url = "http://service.ctalk.cn/socialservice/room/remove.do?roomIds="++ID,
%% 	httpc:request(get,{Url,[]},[{timeout,3000},{connect_timeout,3000}],[]).

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

% 返回的是新的TimesList
check_family_instance_fight_times(RoleID,TimesList,_VipLevel)->
    case lists:keytake(RoleID, 1, TimesList) of
        {value, {RoleID,AtkTimes,BugTimes},OtherTimesList} ->
            if
                0 < AtkTimes ->
                    {true, [{RoleID,AtkTimes-1,BugTimes}|OtherTimesList]};
                true ->
                    {false, TimesList}
            end;
        false ->
            {true, [{RoleID,data_family:get(family_instance_fight_times)-1,0}|TimesList]}
    end.

do_fight(RoleID, BossList, BossCurHp, FighterList,LieuAdd,TalentList,TrSpecial) ->
    ASkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
    TarEquipList = role_data:get_otherRoleItemEquips(RoleID),
    %%将数据库中获得的精灵装备列表按照精灵分类
    GerEquipList = role_item:assort_ger_equiplist(TarEquipList),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
    %% 发生战斗
    {Result, FightRecord, {_, NewBossStateList,_,_}} = role_fight:new(RoleID,FighterList, BossList,LieuAdd,#add_attr{},TalentList,[],TrSpecial,#trSpecial{},false,ASkinInfo,#skin_info{},LegendAddList,[]),
    ?INFO("do_fight finish NewBossStateList:~w",[NewBossStateList]),
    NewBossList = lists:foldl(fun(TempB,Acclist)->
                                    B = ger_attr:transOldGer2NewGer(TempB), 
                                      case lists:keysearch(B#ger.gerID, 1, NewBossStateList) of
                                          {value,{_,NewGerHp,_}} when NewGerHp > 0 ->
                                              [B#ger{gerHp = NewGerHp}|Acclist];
                                          _ ->
                                              Acclist
                                      end
                              end, [], BossList),
    NewCurHp =  erlang:min(lists:sum([Hp||{_,Hp,_}<-NewBossStateList])
                          ,BossCurHp), 
    {Result,erlang:max(0,NewCurHp),NewBossList,FightRecord}.  

merge_reward(R1,R2) ->
	#sell_reward{coin=R1#sell_reward.coin + R2#sell_reward.coin
				,roleExp=R1#sell_reward.roleExp + R2#sell_reward.roleExp
				,gerExp=R1#sell_reward.gerExp + R2#sell_reward.gerExp
				,gold=R1#sell_reward.gold + R2#sell_reward.gold
				,item=R1#sell_reward.item ++ R2#sell_reward.item
				,reputation = R1#sell_reward.reputation + R2#sell_reward.reputation
				,newGer = R1#sell_reward.newGer ++ R2#sell_reward.newGer
				 }.

reset_family_level(FamilyID,OldLevel,FamilyInfo,2) ->
	#family_info{family_id=FamilyID0,level=Level,active_points=AP}=FamilyInfo,
	case FamilyID of
		FamilyID0 ->
			if OldLevel > Level ->
				   FamilyInfo;
			   true ->
				   NewExp = get_level_exp(OldLevel,AP),
				   ?ERR("{reset_family_level, StartSec,StopSec,Lv}:~w,~w,~w",[FamilyID,OldLevel,NewExp]),
				   FamilyInfo#family_info{level=OldLevel,active_points=NewExp}
			end;
		_ ->
			FamilyInfo
	end.
	
get_family_inst_msg(RoleID,FamilyInstanceTimes,FamilyInstanceState)->
    InstList = FamilyInstanceState#family_instance_state.inst_list,
    InstList2 = lists:foldr(fun(InstId,AccList)->
            case data_family:get({family_instance,InstId}) of
                ?undefined ->
                    AccList;
                _ ->
                    case lists:keyfind(InstId, 1, InstList) of
                        {InstId,InstState} ->
                            [{family_instance_open_state,InstId,InstState}|AccList];
                        false ->
                            [{family_instance_open_state,InstId,?F_INST_CLOSE}|AccList] %boss未开启
                    end
            end     
        end, [], data_family:get(family_instance_id_list)),
    {AtkTime,BuyTime} = case lists:keysearch(RoleID, 1, FamilyInstanceState#family_instance_state.fight_member_times) of
                  false ->
                      {data_family:get(family_instance_fight_times),0};
                  {value,{RoleID,Atk,Buy}} ->
                      {Atk,Buy}
                end,
    {Price,BuyOneTimes}=data_family:get(family_instance_buy_times_price),
    RewardGetStatus = FamilyInstanceState#family_instance_state.reward_get_status,
    IsWin = FamilyInstanceState#family_instance_state.is_win,
    BoxIdGet = lists:keymember(RoleID, 2, RewardGetStatus),
    ExtraBoxIsGet = lists:member(RoleID, FamilyInstanceState#family_instance_state.extra_reward_is_get),
    CanGet = case IsWin of
                 true when BoxIdGet =:= false orelse ExtraBoxIsGet =:= false  ->
                     1;
                 _ ->
                     0
             end,
    #sc_familyfight_instance_open_state{state_list=lists:sort(fun({family_instance_open_state,InstIdA,_},{family_instance_open_state,InstIdB,_})-> InstIdA =< InstIdB end , InstList2),attack_times=AtkTime
                                       ,buy_price=Price,buy_one_times=BuyOneTimes,buy_price_times=FamilyInstanceTimes-BuyTime
                                       ,next_instance_id=FamilyInstanceState#family_instance_state.next_instance
                                       ,is_have_reward=CanGet}.

%% reset_family_level(StartSec,StopSec,Lv,FamilyInfo) ->
%% 	#family_info{family_id=FamilyID,log_data=Log,active_points=AP}=FamilyInfo,
%% 	Lv2 = integer_to_list(Lv),
%% 	List = [ArgID||#p_family_log_dtl{timeStamp=TS,type=Type,arg_1=ArgID}<-Log, ArgID >= Lv2, TS > StartSec andalso TS < StopSec andalso Type== 1011],
%% 	case lists:sort(List) of
%% 		[] ->
%% 			FamilyInfo;
%% 		[A|_] ->
%% 			NewLevel = list_to_integer(A),
%% 			if NewLevel > Lv ->
%% 				   NewExp = get_level_exp(NewLevel,AP),
%% 				   ?ERR("{reset_family_level, StartSec,StopSec,Lv}:~w,~w,~w",[FamilyID,NewLevel,NewExp]),
%% 				   FamilyInfo#family_info{level=NewLevel,active_points=NewExp};
%% 			   true ->
%% 				   FamilyInfo
%% 			end
%% 	end.
%% reset_family_level2(StartSec,StopSec,Lv1, FamilyInfo) ->
%% 	#family_info{family_id=FamilyID,log_data=Log,active_points=AP,level=Level} = FamilyInfo,
%% 	{NewLv,_} = 
%% 		lists:foldl(fun(#p_family_log_dtl{timeStamp=TS,type=Type,arg_1=ArgID},{Lv,0}) ->
%% 							case Type of
%% 								1011 ->
%% 									if TS > StartSec andalso TS < StopSec ->
%% 										   Lv0 = list_to_integer(ArgID),
%% 										   if Lv0 >= Lv1 andalso Lv0 < Lv ->
%% 												  {Lv0,0};
%% 											  true ->
%% 												  {Lv,0}
%% 										   end;
%% 									   true ->
%% 										   {Lv,0}
%% 									end;
%% 								_ ->
%% 									{Lv,0}
%% 							end;
%% 					   (_,{Lv,1}) ->
%% 							{Lv,1}
%% 					end, {Level,0}, Log),
%% 	if NewLv == Level ->
%% 		   FamilyInfo;
%% 	   true ->
%% 		   NewExp = get_level_exp(NewLv,AP),
%% 		   ?ERR("{reset_family_level, StartSec,StopSec,Lv}:~w,~w,~w",[FamilyID,NewLv,NewExp]),
%% 		   FamilyInfo#family_info{level=NewLv,active_points=NewExp}
%% 	end.

get_level_exp(Level,AP) ->
	{Exp,_} = 
	lists:foldl(fun(Exp,{Ap,0})->
						case data_family_exp:get(Exp) of
							Level ->
								{Exp+1,1};
							_ ->
								{Ap,0}
						end;
				   (_,{Ap,1}) ->
						{Ap,1}
				end,{AP,0},data_family_exp:get_list()),
	Exp.
	

%L1和L2必须是相同的record
list_plus_list(L1,L2)->
    lists:reverse(list_plus_list(L1,L2,[])).

list_plus_list([H1|T1],[H2|T2],T) when is_integer(H1) andalso is_integer(H2) ->
    list_plus_list(T1,T2,[H1+H2|T]);
list_plus_list([H1|T1],[H2|T2],T) when is_list(H1) andalso is_list(H2) ->
    list_plus_list(T1,T2,[H1++H2|T]);
list_plus_list([H1|T1],[_H2|T2],T) ->
    list_plus_list(T1,T2,[H1|T]);
list_plus_list([],[],R) ->
    R.

bool_to_byte(false)->
    0;
bool_to_byte(0)->
    0;
bool_to_byte(_)->
    1.

init_family_limit_shop() ->
	ShopList = data_family:get(limit_shop),
	ShopList2 = [{p_shop_family_limit,ShopID,LevelLimit,BuyTimesLimit,0,Cost,Reward}
				 ||{ShopID,LevelLimit,BuyTimesLimit,Cost,Reward}<-ShopList],
	ShopList2.


fill_grouplist(GroupList0,Members)->
    GroupList = lists:usort(GroupList0),
    Members2 = lists:filter(fun(E)-> not lists:member(E#family_member_info.role_id, GroupList) end, Members),
    Members3 = lists:sort(fun(A,B)-> A#family_member_info.fight_power > B#family_member_info.fight_power end, Members2),
    {SignNeedNumMin,SignNeedNumMax} = family_fight_server:get_sign_num_limit(),
    fill_grouplist(GroupList,Members3,erlang:length(GroupList),SignNeedNumMin,SignNeedNumMax).
    
fill_grouplist(GroupList,_Members,L,Min,Max)  when L >= Min ->
    {R,_}=lists:split(Max,GroupList),
    R;
fill_grouplist(GroupList,[H|Members],L,Min,Max) ->
    fill_grouplist([H#family_member_info.role_id|GroupList],Members,L+1,Min,Max);
fill_grouplist(GroupList,[],L,Min,_Max) ->
    if
        Min > L ->
            ?INFO("fill_grouplist fail, too less ~w",[GroupList]);
        true ->
            ignore
    end,
    GroupList.

% //1 => 盟主      2 => 副盟主     0 => 成员
%%比较公会成员之间的职位
compare_member_family_title(1,1)->
    equal;
compare_member_family_title(1,_TitleB)->
    true;
compare_member_family_title(2,1)->
    false;
compare_member_family_title(2,2)->
    equal;
compare_member_family_title(2,0)->
    true;
compare_member_family_title(0,0)->
    equal;
compare_member_family_title(0,_TitleB)->
    false.

%%公会进程在阿努比斯之门(v3.0.0)中将会处理以下四个消息
%%(1) 战斗结束后更新公会以及公会成员的阿努比斯数据
%%(2) 发放阿努比斯战斗的奖励（一场战斗的奖励）
%%(3) 发放阿努比斯赛季的奖励
%%(4) 接收anubis_server推送过来的更新公会排名的信息    



%%战斗结果，其中memberresult代表公会成员战斗结果列表
% -record(anubis_fight_result,{fightresult=0,killnum=0,resource=0,memberresult=[]}).

%%公会成员战斗结果
% -record(anubis_family_member_result,{roleid=0,killnum=0,resource=0}).

%%赛季表现， 用于记录公会以及公会成员在对应赛季的表现(公会成员的rank可能不需要)
% -record(anubis_representation,{seasonid=0,killnum=0,rank=0,score=0}).
%%排行榜上榜数据
% -record(family_player,{familyid=0,familyname=0,familyserver=0,familyleader=0,familyrank=0,familyscore=0,familykillnum=0,familyfightpower=0,ischange=true,updatetime=util:now()}).
%%公会阿努比斯信息
% -record(family_anubis_info,{currentseasonid=0,anubisreputationlist=[]}).
%%公会成员战斗结果
%%公会的阿努比斯赛季信息
% -record(anubis_family_info,{current_season_id=0,representationlist=[]}).
%%战斗结束后更新公会以及公会成员的阿努比斯数据
update_family_anubis_info(CSeasonID,CSeasonFamilyPlayer,#anubis_fight_result{fightresult=FightResult}=FightMsg,#family_info{family_anubisinfo=FamilyAnubisInfo,family_id=FamilyID}=FamilyInfo)->
    #anubis_family_info{representationlist=FamilyAnubisRepresentList} = FamilyAnubisInfo,
    #family_player{familyscore=FamilyScore,familykillnum=FamilyKillNum}=CSeasonFamilyPlayer,
    FamilyInfo1 = case lists:keytake(CSeasonID,#anubis_family_representation.seasonid,FamilyAnubisRepresentList) of
        false->
            % ?ERR("Season transform CSeasonID:~w~n",[CSeasonID]),
            %%出现了跨越赛季，需要增加一个新的赛季信息
            OldFamilyKillNum = 0,
            OldFamilyScore = 0,
            AddSeasonRepresention = #anubis_family_representation{seasonid=CSeasonID,killnum=FamilyKillNum,score=FamilyScore},
            NewFamilyAnubisRepresentList = [AddSeasonRepresention|FamilyAnubisRepresentList],
            NewFamilyAnubisInfo = FamilyAnubisInfo#anubis_family_info{representationlist=NewFamilyAnubisRepresentList,current_season_id=CSeasonID},
            FamilyInfo#family_info{family_anubisinfo=NewFamilyAnubisInfo};
        {_Value,FindRepresention=#anubis_family_representation{killnum=OldFamilyKillNum,score=OldFamilyScore},Other}->
            % ?ERR("CSeasonID:~w ~n",[CSeasonID]),
            NewFindRepresention = FindRepresention#anubis_family_representation{killnum=FamilyKillNum,score=FamilyScore},
            NewFamilyAnubisRepresentList = [NewFindRepresention|Other],
            NewFamilyAnubisInfo = FamilyAnubisInfo#anubis_family_info{representationlist=NewFamilyAnubisRepresentList,current_season_id=CSeasonID},
            FamilyInfo#family_info{family_anubisinfo=NewFamilyAnubisInfo}
    end,
    OperateType = case FightResult of test->0;_->1 end,
    Time = erlang:localtime(),
    behavior_anubis_score_add:log(FamilyID,OldFamilyKillNum,OldFamilyScore,FamilyKillNum,FamilyScore,Time,OperateType),
    FamilyInfo2 = update_family_member_anubis_info(CSeasonID,FightMsg,FamilyInfo1),
    %%发放一次战斗的奖励
    send_anubis_fight_member_reward(FightMsg,FamilyInfo2),
    FamilyInfo2.

%%战斗结果，其中memberresult代表公会成员战斗结果列表
% -record(anubis_fight_result,{fightresult=0,killnum=0,resource=0,memberresult=[]}).

%%公会成员战斗结果
% -record(anubis_family_member_result,{roleid=0,killnum=0,resource=0}).

%%根据战斗结果更新每个公会成员的阿努比斯赛季信息
update_family_member_anubis_info(CSeasonID,#anubis_fight_result{memberresult=MemberFightResultList},#family_info{members=MemberList}=FamilyInfo)->
    % ?ERR("MemberList:~w ~n",[MemberList]),
    NewMemberList = lists:foldl(fun(#anubis_family_member_result{roleid=RoleID,killnum=KillNum,resourcepoint=ResourcePoint},MemberListAcc)->
            % ?ERR("RoleID:~w MemberListAcc:~w ~n",[RoleID,MemberListAcc]),
            case lists:keytake(RoleID,#family_member_info.role_id,MemberListAcc) of
                false->
                    % ?INFO("update family_member:~w not in familyMemberList:~w ~n",[RoleID,MemberListAcc]),
                    MemberListAcc;
                {_Value,#family_member_info{anubisinfo=MemberAnubisRepresentationList}=Member,Other}->
                    case lists:keytake(CSeasonID,#anubis_member_representation.seasonid,MemberAnubisRepresentationList) of
                        false->
                            AddSeasonRepresention = #anubis_member_representation{seasonid=CSeasonID,killnum=KillNum,resourcepoint=ResourcePoint},
                            NewMember = Member#family_member_info{anubisinfo=[AddSeasonRepresention|MemberAnubisRepresentationList]},
                            [NewMember|Other];
                        {_Value,#anubis_member_representation{killnum=OldKillNum,resourcepoint=OldResourcePoint}=FindRepresention,OtherRepresention}->
                            NewFindRepresention = FindRepresention#anubis_member_representation{killnum=OldKillNum+KillNum,resourcepoint=OldResourcePoint+ResourcePoint},
                            NewMember = Member#family_member_info{anubisinfo=[NewFindRepresention|OtherRepresention]},
                            [NewMember|Other]
                    end
            end
        end,MemberList,MemberFightResultList),
    FamilyInfo#family_info{members=NewMemberList}.

%%发放阿努比斯战斗的奖励（一场战斗的奖励(向公会当前所有的成员发送奖励))
send_anubis_fight_member_reward(#anubis_fight_result{memberresult=MemberFightResultList,fightresult=FightResult,resource=Resource,killnum=KillNum},#family_info{members=MemberList})->
    lists:foreach(fun(#family_member_info{role_id=RoleID})->
        MemberKillNum = case lists:keyfind(RoleID,#anubis_family_member_result.roleid,MemberFightResultList) of false -> 0;#anubis_family_member_result{killnum=MemberKillNum1}-> MemberKillNum1 end,
        %%anubis_family_member_result中的resource
        % #anubis_family_member_result{roleid=RoleID} = MemberFightResult,
        send_anubis_fight_single_member_reward(RoleID,FightResult,MemberKillNum,Resource,KillNum)
    end,MemberList).

send_anubis_fight_single_member_reward(RoleID,FightResult,MemberKillNum,TotalResource,TotalKillNum)->
    {FightScore,Honor,UnionCoin,Reward} = anubis_rank_server:calculate_family_member_score(FightResult,MemberKillNum,TotalResource,TotalKillNum),
    ?INFO("FightResult:~w ~n",[FightResult]),
    MailTemplateID = case FightResult of
        win->
            ?MAIL_TEMPLATE_ANUBIS_WIN;
        equal->
            ?MAIL_TEMPLATE_ANUBIS_EQUAL;
        _->
            ?MAIL_TEMPLATE_ANUBIS_LOSE
    end,
    ArgsList = [Honor,UnionCoin,FightScore],
    mail_server:send_sys_mail(RoleID, MailTemplateID, ArgsList , "", Reward).

%%发放阿努比斯赛季的奖励
send_anubis_season_reward(SeasonID,SeasonRank,FamilyInfo)->
    Config = data_anubis:get(season_reward),
    case get_anubis_season_rank_rewardConifg(SeasonRank,Config) of
        false->
            ?ERR("not find SeasonRank:~w  in data_anubis~n",[SeasonRank]);
        {LeaderReward,VicLeaderReward,MemberReward}->
            %%向公会当前所有玩家发送奖励
            send_anubis_season_reward_for_all_member(SeasonID,SeasonRank,MemberReward,FamilyInfo),
            %%向公会会长发送奖励
            send_anubis_season_reward_for_leader(SeasonID,SeasonRank,LeaderReward,FamilyInfo),
            %%向公会副会长发送奖励
            send_anubis_season_reward_for_vicleader(SeasonID,SeasonRank,VicLeaderReward,FamilyInfo)
    end.

send_anubis_season_reward_for_all_member(SeasonID,SeasonRank,RewardConfig,#family_info{members=MemberList})->
    AnubisMemberSort = anubis_rank_server:sort_family_member(MemberList,SeasonID),
    lists:foreach(fun({RoleRank,#family_member_info{role_id=RoleID,role_name=RoleName}})->
        case get_anubis_season_rank_rewardConifg(RoleRank,RewardConfig) of
            false ->
                ?ERR("not find RoleRank:~w RoleID:~w  in data_anubis~n",[RoleRank,RoleID]);
            Reward-> 
                ArgsList = [RoleName,SeasonID,SeasonRank,RoleRank],
                mail_server:send_sys_mail(RoleID,?MAIL_TEMPLATE_ANUBIS_SEASON_MEMBER, ArgsList , "", Reward)
        end
    end,AnubisMemberSort).

send_anubis_season_reward_for_leader(SeasonID,SeasonRank,Reward,#family_info{owner_role_id=LeaderRoleID,family_name=FamilyName})->
    ArgsList=[FamilyName,SeasonID,SeasonRank],
    mail_server:send_sys_mail(LeaderRoleID, ?MAIL_TEMPLATE_ANUBIS_SEASON_LEADER, ArgsList , "", Reward).
    %%此处由于前端配置的的会长奖励邮件ID错误，故不能使用对应的邮件ID，需要后端构造奖励邮件信息
    % Content1 = io_lib:format("{prop ffffffff-24}尊敬的@(0)@公会会长，您率领您的公会在第@(1)@赛季中，排名第@(2)@，以下是给您的单独奖励，请查收",[]),
    % Content = util:latin1(Content1),
    % mail_server:send_sys_mail(LeaderRoleID,0,ArgsList,Content,Reward).

send_anubis_season_reward_for_vicleader(SeasonID,SeasonRank,Reward,#family_info{members=MemberList,family_name=FamilyName})->
    ArgsList=[FamilyName,SeasonID,SeasonRank],
    lists:foreach(fun(#family_member_info{role_id=RoleID,family_title=Title})->
        case Title=:=2 of
            true->
                mail_server:send_sys_mail(RoleID, ?MAIL_TEMPLATE_ANUBIS_SEASON_VICLEADER, ArgsList , "", Reward);
            false->
                ignore
        end
    end,MemberList).

%%% 受anubis_server驱动，主动更新自身的赛季排名，赛季统计数据在战斗结束之后就已经更新了，排名可能随着其他公会的战斗而被动改变，所以需要更新
deal_update_family_anubis_rank(_CSeasonID,Rank,#family_info{family_anubisinfo=FamilyAnubisInfo}=FamilyInfo,UpdateSeasonID)->
    #anubis_family_info{representationlist=AnubisRepresentationList} = FamilyAnubisInfo,
    case lists:keytake(UpdateSeasonID,#anubis_family_representation.seasonid,AnubisRepresentationList) of
        false->
            % ?ERR("update family anubis_representation:~w  SeasonID:~w~n",[AnubisRepresentationList,UpdateSeasonID]),
            AddRepresentation = #anubis_family_representation{seasonid=UpdateSeasonID,rank=-1,updatetime=util:now()},
            NewAnubisRepresentationList = [AddRepresentation|AnubisRepresentationList];
        {_Value,#anubis_family_representation{rank=_OldRank}=FindRepresention,Other}->
            % ?ERR("update history:~w  cseason:~w updateseason:~w rank:~w newrank:~w ~n",[UpdateSeasonID=/=CSeasonID,CSeasonID,UpdateSeasonID,OldRank,Rank]),
            NewAnubisRepresentationList = [FindRepresention#anubis_family_representation{rank=Rank}|Other]
    end,
    NewFamilyAnubisInfo = FamilyAnubisInfo#anubis_family_info{representationlist=NewAnubisRepresentationList},
    FamilyInfo#family_info{family_anubisinfo=NewFamilyAnubisInfo}.


%%公会进程更新自己的阿努比斯排名信息
do_update_family_anubis_rank(#family_info{family_anubisinfo=FamilyAnubisInfo,family_id=FamilyID} = FamilyInfo,SeasonID)->
    #anubis_family_info{current_season_id=CurrentSeasonID} = FamilyAnubisInfo,
    NewRank = case anubis_server:get_family_anubis_rank_for_season(SeasonID,FamilyID) of
        0->
            % ?ERR("request anubis_rank seasonID:~w familyID:~w SeasonRanklist not exist~n",[CurrentSeasonID,FamilyID]),
            0;
        Rank->
            Rank
    end,
    deal_update_family_anubis_rank(CurrentSeasonID,NewRank,FamilyInfo,SeasonID).

new_do_update_family_anubis_rank(SeasonID,FamilyPlayer,#family_info{family_anubisinfo=FamilyAnubisInfo} = FamilyInfo)->
    % ?ERR("receive update rank:SeasonID:~w FamilyPlayer:~w ~n",[SeasonID,FamilyPlayer]),
    #anubis_family_info{current_season_id=CurrentSeasonID} = FamilyAnubisInfo,
    deal_update_family_anubis_rank(CurrentSeasonID,FamilyPlayer#family_player.familyrank,FamilyInfo,SeasonID).


%%%%赛季表现， 用于记录公会对应赛季表现
% -record(anubis_family_representation,{seasonid=0,killnum=0,rank=0,score=0,updatetime=0}).
%%获取当前存在的赛季信息，更新公会的对应排名
update_family_anubis_rank(#family_info{family_anubisinfo=FamilyAnubisInfo,family_id=FamilyID}=FamilyInfo)->
    #anubis_family_info{representationlist=AnubisRepresentationList}= FamilyAnubisInfo,
    case anubis_server:get_anubis_seasonid() of
        {0,[]}->
            FamilyInfo;
        {CurrentSeasonID,ExistSeaonIDList}-> 
            NowSec = util:now(),
            NewAnubisRepresentationList = lists:foldl(fun(SeasonID,Acc)->
            case SeasonID=:=CurrentSeasonID of
                false->
                    case lists:keyfind(SeasonID,#anubis_family_representation.seasonid,AnubisRepresentationList) of
                        false->
                            case anubis_server:get_family_player_from_rank(SeasonID,FamilyID) of
                                ?undefined->
                                    [#anubis_family_representation{seasonid=SeasonID,updatetime=NowSec,rank=-1}|Acc];
                                #family_player{familyrank=Rank,familyscore=FamilyScore,familykillnum=FamilyKillNum}->
                                    [#anubis_family_representation{seasonid=SeasonID,killnum=FamilyKillNum,rank=Rank,score=FamilyScore,updatetime=NowSec}]
                            end;
                        OldRepresentation->
                            case anubis_server:get_family_player_from_rank(SeasonID,FamilyID) of
                                ?undefined->
                                    [OldRepresentation|Acc];
                                #family_player{familyrank=Rank,familyscore=FamilyScore,familykillnum=FamilyKillNum}->
                                    [#anubis_family_representation{seasonid=SeasonID,killnum=FamilyKillNum,rank=Rank,score=FamilyScore,updatetime=NowSec}]
                            end
                    end;
                true->
                    case anubis_server:get_family_player_from_rank(SeasonID,FamilyID) of
                        ?undefined->
                            [#anubis_family_representation{seasonid=SeasonID,updatetime=NowSec,rank=-1}|Acc];
                        #family_player{familyrank=Rank,familyscore=FamilyScore,familykillnum=FamilyKillNum}->
                            [#anubis_family_representation{seasonid=SeasonID,killnum=FamilyKillNum,rank=Rank,score=FamilyScore,updatetime=NowSec}]
                    end
            end
            end,[],ExistSeaonIDList),
            NewFamilyAnubisInfo = FamilyAnubisInfo#anubis_family_info{current_season_id=CurrentSeasonID,representationlist=NewAnubisRepresentationList},
            FamilyInfo#family_info{family_anubisinfo=NewFamilyAnubisInfo}
    end.

%%处理赛季的切换
update_current_seasonid(CSeasonID,#family_info{family_anubisinfo=FamilyAnubisInfo}=FamilyInfo)->
    NewFamilyAnubisInfo = FamilyAnubisInfo#anubis_family_info{current_season_id=CSeasonID},
    FamilyInfo#family_info{family_anubisinfo=NewFamilyAnubisInfo}.

%%删除对应的赛季排名数据
delete_season_representation(DeleteSeasonIDList,#family_info{family_anubisinfo=FamilyAnubisInfo,members=MemberList}=FamilyInfo)->
    #anubis_family_info{representationlist=AnubisRepresentationList}=FamilyAnubisInfo,
    NewAnubisRepresentationList = lists:filter(fun(#anubis_family_representation{seasonid=SeasonID})->not lists:member(SeasonID,DeleteSeasonIDList) end,AnubisRepresentationList),
    NewFamilyAnubisInfo=FamilyAnubisInfo#anubis_family_info{representationlist=NewAnubisRepresentationList},
    NewMemberList = delete_family_member_season_representation(DeleteSeasonIDList,MemberList),
    FamilyInfo#family_info{family_anubisinfo=NewFamilyAnubisInfo,members=NewMemberList}.

%%删除公会成员对应赛季的记录
delete_family_member_season_representation(DeleteSeasonIDList,MemberList)->
    delete_family_member_season_representation(DeleteSeasonIDList,MemberList,[]).
delete_family_member_season_representation(_DeleteSeasonIDList,[],Acc)->
    Acc;
delete_family_member_season_representation(DeleteSeasonIDList,[H|T],Acc)->
    #family_member_info{anubisinfo=MemberRepresentationList} = H,
    NewMemberRepresentationList = lists:filter(fun(#anubis_member_representation{seasonid=SeasonID})-> not lists:member(SeasonID,DeleteSeasonIDList) end,MemberRepresentationList),
    NewMember = H#family_member_info{anubisinfo=NewMemberRepresentationList},
    delete_family_member_season_representation(DeleteSeasonIDList,T,[NewMember|Acc]).
%%=============================================================================================================================
get_anubis_season_rank_rewardConifg(Rank,Config)->
case util:fun_take(fun({{Begin,End},_Reward})-> Rank >= Begin andalso Rank=<End end,Config) of
    false->
        ?ERR("not find Rank:~w  in config:~w~n",[Rank,Config]),
        false;
    {_Value,{_,Reward},_Other}->
        Reward
end.

get_family_season_rank_from_familyinfo(#family_info{family_anubisinfo=FamilyAnubisInfo,family_id=FamilyID,family_name=FamilyName,owner_role_name=FamilyLeader},SeasonID)->
    #anubis_family_info{representationlist=RepresentationList} = FamilyAnubisInfo,
    {FamilyRank,FamilyScore} = case lists:keyfind(SeasonID,#anubis_family_representation.seasonid,RepresentationList) of
        false->
            {0,0};
        #anubis_family_representation{rank=FRank,score=FScore}->
            {FRank,FScore}
    end,
    FamilyServer = data_setting:get(server_id),
    #p_anubis_family{familyid=FamilyID,familyname=FamilyName,serverid=FamilyServer,leadername=FamilyLeader,score=FamilyScore,rank=FamilyRank}.
%%从赛季表现列表中找到对应赛季的表现，不存在返回？undefined
find_anubis_member_season(RepresentationList,SeasonID)->
    case lists:keyfind(SeasonID,#anubis_member_representation.seasonid,RepresentationList) of   
        false->
            ?undefined;
        X ->
            X
    end.

get_family_player(#family_info{family_anubisinfo=FamilyAnubisInfo,level=Level} = FamilyInfo)->
    ServerID = data_setting:get(server_id),
    #anubis_family_info{representationlist=RepresentationList,current_season_id=CSeasonID} = FamilyAnubisInfo,
    {OldRank,OldKillNum,OldScore} = case lists:keyfind(CSeasonID,#anubis_family_representation.seasonid,RepresentationList) of
        false->
            {-1,0,0};
        #anubis_family_representation{killnum=KillNum,rank=Rank,score=Score}->
            {Rank,KillNum,Score}
    end,
    #p_family_info{family_id=FamilyID,family_name=FamilyName,owner_role_name=OwnerRoleName,total_fight_power =FamilyFightPower} = family_misc:to_p_family_info(FamilyInfo),
    {#family_player{familyid=FamilyID,familyname=FamilyName,familyserver=ServerID,familyleader=OwnerRoleName,familyrank=OldRank,familyscore=OldScore,familykillnum=OldKillNum,familyfightpower=FamilyFightPower,ischange=1,familylevel=Level},CSeasonID}.

%%anubis_member_representation转换成存储结构
transform_persist2representation(L)->
    transform_persist2representation(L,[]).
transform_persist2representation([],Acc)->
    lists:reverse(Acc);
transform_persist2representation([{SeasonID,KillNum,ResourcePoint}|T],Acc)->
    AR = #anubis_member_representation{seasonid=SeasonID,killnum=KillNum,resourcepoint=ResourcePoint},
    transform_persist2representation(T,[AR|Acc]);
transform_persist2representation(_,_)->
    [].

transform_representation2persist(L)->
    transform_representation2persist(L,[]).
transform_representation2persist([],Acc)->
    lists:reverse(Acc);
transform_representation2persist([H|T],Acc)->
    #anubis_member_representation{seasonid=SeasonID,killnum=KillNum,resourcepoint=ResourcePoint}=H,
    transform_representation2persist(T,[{SeasonID,KillNum,ResourcePoint}|Acc]).

get_family_member_anubis_rank(#family_info{members=MemberList},SeasonID)->
    SortList = anubis_rank_server:sort_family_member(MemberList,SeasonID),
    lists:foldl(fun({Rank,#family_member_info{role_id=RoleID,role_name=RoleName,anubisinfo=MemberAnubis}},Acc)->
        case find_anubis_member_season(MemberAnubis,SeasonID) of
            ?undefined->
                [#p_anubis_family_member{roleid=RoleID,rolename=RoleName,killnum=0,resource=0,rank=Rank}|Acc];
            #anubis_member_representation{killnum=KillNum,resourcepoint=ResourcePoint}->
                [#p_anubis_family_member{roleid=RoleID,rolename=RoleName,killnum=KillNum,resource=ResourcePoint,rank=Rank}|Acc]
        end
    end,[],SortList).

generate_car_team(Members,FamilyLevel)->
	Members2 = lists:sort(fun(#family_member_info{fight_power=FP1,title=T1}
											,#family_member_info{fight_power=FP2,title=T2}) ->
												if T1 == 1 -> true;
												   T2 == 1 -> false;
												   T1 > T2 -> true;
												   true -> 
													   if FP1 > FP2 -> true;
														  true -> false
													   end
												end end,Members),
	Dis = fill_car(Members2,FamilyLevel),
	{Dis2,_}=lists:foldl(fun({D,C1,C2,C3},{DisAcc,C}) ->
								 {[#p_familycross_car{driver=D,c1=C1,c2=C2,c3=C3,id=C,type=0}|DisAcc],C+1}
						 end,{[],1}, Dis),
	Dis2.

fill_car(Members,FamilyLevel) ->
	Sits = data_family_cross:get(car_sits),
	Max = data_family_cross:get({car_family_level, FamilyLevel}), 
	fill_car(Members, lists:duplicate(Max,erlang:list_to_tuple(lists:duplicate(Sits, 0))),[],1,1,Max).
fill_car([], C1,Car, _,_,_) -> lists:reverse(Car)++C1;
fill_car(T, [],Car,Round,_,Max) -> fill_car(T,lists:reverse(Car),[],Round+1,1,Max);
fill_car([#family_member_info{role_id=RoleID}|T]=L,[Car|OCar],CC,Round,Count,Max) ->
	case element(Round,Car) of
		0-> fill_car(T, OCar,[erlang:setelement(Round,Car,RoleID)|CC], Round,Count+1,Max);
		_-> fill_car(L, OCar,[Car|CC], Round,Count+1,Max)
	end.

valid_cars(Cars, Members, FamilyLevel,OldCars) ->
	IDList = lists:seq(1, data_family_cross:get({car_family_level, FamilyLevel})),
	case valid_cars(IDList,Cars, Members) of
		true -> Cars;
		false -> OldCars
  end.

valid_cars([],[],_) -> true;
valid_cars([H|T], Cars, Members) ->
	case lists:keytake(H, #p_familycross_car.id, Cars) of
		false -> valid_cars(T, Cars, Members);
		{value, #p_familycross_car{driver=D,c1=C1,c2=C2,c3=C3},OtherCar} ->
			case valid_member([D,C1,C2,C3],Members) of
				true -> valid_cars(T,OtherCar, Members);
				false -> false
			end
	end.

valid_member([],_) -> true;
valid_member([0|T], Member) -> valid_member(T,Member);
valid_member([H|T],Member) -> 
	case lists:keyfind(H,#family_member_info.role_id, Member) of
		false -> false;
		_ -> valid_member(T, Member)
	end.

re_generate_car_team(CarsTemp,Members) ->
	FamilyRoles = [RoleID||#family_member_info{role_id=RoleID}<-Members],
	{CarFlys,RR} = lists:foldl(fun(#p_familycross_car{driver=D,c1=C1,c2=C2,c3=C3}=F,{CarAcc,RAcc}) ->
				{F1,RAcc1} = case lists:member(D,RAcc) of
					true -> {F, lists:delete(D,RAcc)};
					_ -> {F#p_familycross_car{driver=0},RAcc}
					end,
				{F2,RAcc2} = case lists:member(C1,RAcc1) of
					true -> {F1,lists:delete(C1,RAcc1)};
					_ -> {F1#p_familycross_car{c1=0},RAcc1}
					end,
				{F3,RAcc3} = case lists:member(C2,RAcc2) of
					true -> {F2,lists:delete(C2,RAcc2)};
					_ -> {F2#p_familycross_car{c2=0},RAcc2}
					end,
				{F4,RAcc4} = case lists:member(C3, RAcc3) of
					true -> {F3, lists:delete(C3,RAcc3)};
					_ -> {F3#p_familycross_car{c3=0}, RAcc3}
					end,
				{[F4|CarAcc],RAcc4}
				end,{[],FamilyRoles}, CarsTemp),
	%NewCars  = re_fill_car(RR,CarFlys,[]),
	lists:foldl(fun(#p_familycross_car{driver=0,c1=0,c2=0,c3=0}=LCar,Acc) -> [LCar|Acc];
				   (#p_familycross_car{driver=D,c1=C1,c2=C2,c3=C3}=LCar,Acc) ->
						case D of
							0 -> if C1 /= 0 -> [LCar#p_familycross_car{driver=C1,c1=0}|Acc];
									C2 /= 0 -> [LCar#p_familycross_car{driver=C2,c2=0}|Acc];
									C3 /= 0 -> [LCar#p_familycross_car{driver=C3,c3=0}|Acc]
								 end;
							_ ->[LCar|Acc]
						end end,[], CarFlys).
						%end end,[],	NewCars).
re_fill_car(_,[],CB)-> CB;
re_fill_car([],CA,CB) -> lists:reverse(CB)++CA;
re_fill_car([RoleID|OtherRole]=RL,[#p_familycross_car{driver=D,c1=C1,c2=C2,c3=C3}=F|T],CB) ->
	if D == 0 ->
		   re_fill_car(OtherRole,[F#p_familycross_car{driver=RoleID}|T],CB);
	   C1 == 0 ->
		   re_fill_car(OtherRole,[F#p_familycross_car{c1=RoleID}|T],CB);
	   C2 == 0 ->
		   re_fill_car(OtherRole,[F#p_familycross_car{c2=RoleID}|T],CB);
	   C3 == 0 ->
		   re_fill_car(OtherRole,[F#p_familycross_car{c3=RoleID}|T],CB);
	   true -> re_fill_car(RL,T,[F|CB])
	end.

%%处理对公会阿努比斯数据的修改，按照上榜的流程进行，直接获取并修改数据之后进行上榜,后面的更新交由同步流程处理
do_test_change_family_anubis_info(FamilyInfo,FamilyKillNum,FamilyScore)->
    {#family_player{familykillnum=OldFamilyKillNum,familyscore=OldFamilyScore}=CSeasonFamilyPlayer,CSeasonID} = get_family_player(FamilyInfo),
    NewCseasonFamilyPlayer = CSeasonFamilyPlayer#family_player{familykillnum=erlang:max(OldFamilyKillNum+FamilyKillNum,0),familyscore=erlang:max(OldFamilyScore+FamilyScore,0)},
    anubis_rank_server:up_anubis_rank(NewCseasonFamilyPlayer,#anubis_fight_result{fightresult=test},CSeasonID).

%%实际修改玩家在对应赛季的公会阿努比斯数据
do_test_change_role_anubisinfo(RoleID,SeasonID,AddKillNum,AddResourcePoint,FamilyInfo)->
    #family_info{members=MemberList} = FamilyInfo,
    NewMemberList = case lists:keytake(RoleID,#family_member_info.role_id,MemberList) of
        false->
            ?ERR("修改玩家阿努比斯赛季数据未找到玩家：~W ~n",[RoleID]),
            MemberList;
        {_Value1,#family_member_info{anubisinfo=MemberAnubis}=Member,OtherMembers}->
            NewMemberAnubis = case lists:keytake(SeasonID,#anubis_member_representation.seasonid,MemberAnubis) of
                false->
                    [#anubis_member_representation{seasonid=SeasonID,killnum=erlang:max(AddKillNum,0),resourcepoint=erlang:max(AddResourcePoint,0)}|MemberAnubis];
                {_Value,#anubis_member_representation{killnum=OldKillNum,resourcepoint=OldResourcePoint}=OldMemberRepresentation,Other}->
                    [OldMemberRepresentation#anubis_member_representation{killnum=erlang:max(0,OldKillNum+AddKillNum),resourcepoint=erlang:max(0,OldResourcePoint+AddResourcePoint)}|Other]
            end,
            NewMember = Member#family_member_info{anubisinfo=NewMemberAnubis},
            [NewMember|OtherMembers]
    end,
    FamilyInfo#family_info{members=NewMemberList}.

test_send_family_reward(FamilyID,Reward) when is_record(Reward,sell_reward)->
    family_misc:router_to_family_process(FamilyID,{test_send_family_reward,Reward});
test_send_family_reward(FamilyID,Reward)->
    ?ERR("send_family_reward failed familyID:~w Reward:~w ~n",[FamilyID,Reward]).

do_test_send_family_reward(Reward,#family_info{members=MemberList})->
    lists:foreach(fun(#family_member_info{role_id=RoleID})->
        mail_server:send_sys_mail(RoleID,?MONEY_ADD_TYPE_FAMILY_BATCH_SEND, [],"", Reward)
    end,MemberList).    

test_send_family_reward_batch(FileName)->
    case read_config_file(FileName) of
        {false,Reason}->
            ?ERR("test_change_family_anubis_info_batch fail: ~w ~n",[Reason]);
        FamilyList ->
        lists:foreach(fun({FamilyID,Reward})->
            ServerID = (FamilyID div 1000000)-1,
            case node_info_server:get_node_info(ServerID) of
                ignore->
                    ?ERR("can not find Node for serverID:~w ~n",[ServerID]),
                    ?ERR("send reward FamilyID:~w reward:~w failed~n",[FamilyID,Reward]);
                Node ->
                    rpc:cast(Node,family_server,test_send_family_reward,[FamilyID,Reward])
            end
        end,FamilyList)
    end.

read_config_file(FileName)->
    case file:consult(FileName) of
        {ok,Content}->
            case proplists:get_value(default_reward,Content) of
                ?undefined->
                    ?ERR("not find default_reward~n"),
                    [];
                DefaultReward->
                    case proplists:get_value(familylist,Content) of
                        ?undefined->
                            ?ERR("not find familylist~n"),
                            [];
                        FamilyList->
                            lists:foldl(fun({FamilyID,Reward},Acc)->
                                if 
                                    is_record(Reward,sell_reward) ->
                                        [{FamilyID,Reward}|Acc];
                                    true->
                                        [{FamilyID,DefaultReward}|Acc]
                                end
                            end,[],FamilyList)
                    end
            end;
        _ ->
            ?ERR("read file:~w FileName failed~n",[FileName]),
            []
    end.

