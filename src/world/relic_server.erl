%% 消息发送
-module(relic_server).
-behaviour(gen_server).
-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  

%% API
-export([start_link/0
        ,get_sc_carlos_relic_info/2
        ,get_warid/2
        ,check_server_status/1
        ,test_clean_status/0
        ,test_clean_status/1
        ,clean_relic_times/0
        ,is_team_sign/2
        ,is_role_sign/1]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
%% state record
-record(state, {}).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).

%% ------------------------------------------------------------------
%% API Function Definitions gen_server
%% ------------------------------------------------------------------
start( ) ->
    {ok,_}=
        supervisor:start_child(world_sup, 
                               {?MODULE,
                                {?MODULE, start_link, []},
                                permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% API Function Definitions relic
%% ------------------------------------------------------------------

send_to_me(Msg)->
    erlang:send(?MODULE, Msg).
    
%外部调用不改写ets，ets只有本进程可以修改
get_sc_carlos_relic_info(RoleID,PlaneLevel)->
    NeedList = data_relic:get(need_gold),
    MaxBuyTimes = erlang:length(NeedList),
    MaxFightTimes = data_relic:get(init_times),
	{AP,PT}=afk_record_server:get_afk_punish(RoleID,?AFK_TYPE_RELIC),
    case ets:lookup(?ETS_RELIC_ROLE_INFO, RoleID) of
        [] ->
            #sc_carlos_relic_info{remain_times=MaxFightTimes
                                 ,next_need=lists:nth(1, NeedList)
                                 ,remain_buy = MaxBuyTimes
                                 ,sign_state=?RELIC_SIGN_STATE_NO_SIGN
                                 ,plane_level=PlaneLevel
                                 ,box_rank=0
                                 ,box_name=[]
                                 ,box_cost=[]
                                 ,is_open=[]
                                 ,afk_punish = AP, punish_timestamp = PT};
        [RelicRoleData] ->
            RemainFightTimes = RelicRoleData#relic_role_data.remain_fight_times,
            RemainBuyTime = RelicRoleData#relic_role_data.remain_buy_time,
            SignState = RelicRoleData#relic_role_data.sign_state,
            IsOpenList = RelicRoleData#relic_role_data.box_status,
            {BossLevelRank,BoxRank,_FinalGerTypeID} = RelicRoleData#relic_role_data.box_rank,
            NextNeed = if 
                           RemainBuyTime > 0 ->
                               lists:nth(MaxBuyTimes-RemainBuyTime+1, NeedList);
                           true ->
                               0
                       end,
            {BoxName,BoxCost} = if
                                    BoxRank >= 1 andalso BoxRank =< 6 ->
                                        {data_relic:get({relic_box_name,BoxRank})
                                        ,data_relic:get({relic_box_cost,BossLevelRank,BoxRank})};
                                    true ->
                                        {[],[]}
                                end,
            #sc_carlos_relic_info{remain_times=RemainFightTimes
                                 ,next_need=NextNeed
                                 ,remain_buy = RemainBuyTime
                                 ,sign_state=SignState
                                 ,plane_level=PlaneLevel
                                 ,box_rank=BoxRank
                                 ,box_name=BoxName
                                 ,box_cost=BoxCost
                                 ,is_open= if BoxName =:= [] -> [] ; true -> IsOpenList end
                                 ,afk_punish = AP, punish_timestamp = PT}
    end.

get_warid(RoleID,_TeamID)->
    case ets:lookup(?ETS_RELIC_ROLE_INFO, RoleID) of
        [#relic_role_data{sign_state=SignState,war_id=WarID}]when SignState =:= ?RELIC_SIGN_STATE_FIGHT andalso WarID >= 0->
            {ok,WarID};
        _ ->
            fail
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    ets:new(?ETS_RELIC_ROLE_INFO,[{keypos,2},set, public, named_table]),
    %缓冲数据无需保存，保存正在匹配的队伍ID与RoleID的对应关系
    ets:new(?ETS_RELIC_SIGN,[{keypos,1},set, public, named_table]),
    case db_sql:get_etc(?DB_ETC_KEY_RELIC_ROLE_INFO) of
        {Date,RelicInfoList}  ->
            lists:foreach(fun(RIL)->
                                case RIL of
                                    {relic_role_data,E1,E2,E3,E4,E5,E6,E7,E8} ->
                                        ets:insert(?ETS_RELIC_ROLE_INFO, {relic_role_data,E1,E2,E3,E4,E5,E6,E7,E8,false,[],[]});
                                    {relic_role_data,E1,E2,E3,E4,E5,E6,E7,E8,E9} ->
                                        ets:insert(?ETS_RELIC_ROLE_INFO, {relic_role_data,E1,E2,E3,E4,E5,E6,E7,E8,E9,[],[]});
                                    #relic_role_data{}->
                                        ets:insert(?ETS_RELIC_ROLE_INFO, RIL)
                                end
                          end, RelicInfoList),
            CurDate = erlang:date(),
            if
                Date =/= CurDate ->
                    clean_relic_times();
                true ->
                    ignore
            end;
        _ ->
            ignore %无数据即第一次执行
    end,
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
    
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State} 
    end.

terminate(_Reason, _State) ->
    do_persist(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - do_handle_info
%%%===================================================================

do_handle_info({cs_carlos_relic_sign,RoleID,MemberList,TeamID,LevelRank}, State) ->
    ?INFO("cs_carlos_relic_sign ~w ~w ~w",[RoleID,MemberList,TeamID]),
    case check_relic_sign(RoleID,MemberList,TeamID,[],LevelRank) of
        {ok,NewRelicDataList} ->
            SignID = 
                case TeamID of
                    -1 ->
                        ?make_id(-RoleID, 1);
                    _ ->
                        ?make_id(TeamID, erlang:length(MemberList))
                end,
            RoleIDList = [E#relic_role_data.role_id||E<-NewRelicDataList],
            case carlos_aux:check_team_sign_time(RoleIDList) of
                false ->
                    ?unicast(RoleID, #sc_carlos_relic_sign{result=11});
                _ ->
                    SignID = 
                        case TeamID of
                            -1 ->
                                ?make_id(-RoleID, 1);
                            _ ->
                                ?make_id(TeamID, erlang:length(MemberList))
                        end,
                    ets:insert(?ETS_RELIC_SIGN, {{sign, SignID}, RoleIDList}),
                    ets:insert(?ETS_RELIC_ROLE_INFO, NewRelicDataList),
                    ServerID = data_setting:get(server_id),
                    ReqMemberInfo = [#member_base_info{serverID=ServerID, roleID=ERoleID, head=EHead, title=Title, isMale=IsMale, level=Level, fight_power=FightPower}||
                                        #p_team_member_info{roleID=ERoleID, head=EHead,isMale=IsMale,title=Title, level=Level, fightPower=FightPower}<-MemberList],
                    Request = #request{serverID=ServerID, id = SignID, members=ReqMemberInfo, level = 0}, %巨龙遗迹不用level字段
                    send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {request, Request}),
                    lists:foreach(fun(RID)-> 
                                          ?unicast(RID, #sc_carlos_relic_sign{result=1})
                                  end, RoleIDList), 
                    behavior_carlos:log(RoleIDList, ?carlos_type_relic, ?carlos_op_sign) 
            end;
        {fail,WrongRoleID,Reason} ->
            ?INFO("cs_carlos_relic_sign fail RoleId:~w R:~w",[WrongRoleID,Reason]),
            ?unicast(RoleID, #sc_carlos_relic_sign{result=Reason}) 
    end,
    {noreply, State};

%% 成功取消匹配
do_handle_info({reply_unrequest, SignID}, State) ->
    case ets:lookup(?ETS_RELIC_SIGN, {sign, SignID}) of
        [] ->
            ignore;
        [{_, IDList}] ->
            ets:delete(?ETS_RELIC_SIGN, {sign, SignID}),
            carlos_aux:set_team_unrequest_time(IDList),
            lists:foreach(fun(E) -> 
                            RelicRoleData = get_relic_role_data(E),
                            ets:insert(?ETS_RELIC_ROLE_INFO, RelicRoleData#relic_role_data{sign_state=?RELIC_SIGN_STATE_NO_SIGN
                                                                                          ,is_sleeper=false}),
                            case role_lib:is_online(E) of
                                true ->
                                    %% 通知客户端取消匹配
                                    ?unicast(E, #sc_carlos_relic_sign_cancel{result=0});
                                _ ->
                                    ignore
                            end
                        end, IDList)
    end,
    {noreply, State};

%% 匹配成功
do_handle_info({relic_match_success, ID, WarID}, State) ->
    case ets:lookup(?ETS_RELIC_SIGN, {sign, ID}) of
        [] ->
            ignore;
        [{_, IDList}] ->
            lists:foreach(fun(MID) ->
                    RelicRoleData = get_relic_role_data(MID),
                    #relic_role_data{remain_fight_times=RemainFightTimes} = RelicRoleData,
                    NewRemainFightTimes = max(0,RemainFightTimes-1),
                    NewRelicRoleData = 
                        RelicRoleData#relic_role_data{remain_fight_times=NewRemainFightTimes
                                                     ,sign_state=?RELIC_SIGN_STATE_FIGHT
                                                     ,war_id=WarID
                                                     ,fight_end_ts=util:now()+data_relic:get(war_interval)
                                                     ,box_status=[0,0,0]
                                                     ,box_rank={0,0,0}
                                                     ,other_list=[]},
                    ets:insert(?ETS_RELIC_ROLE_INFO, NewRelicRoleData),
                    case role_lib:is_online(MID) of
                        true ->
                            ?unicast(MID, #sc_carlos_relic_times_update{times=NewRemainFightTimes});
                        _ ->
                            ignore
                    end 
                end, IDList),
            ets:delete(?ETS_RELIC_SIGN, {sign, ID}),
            behavior_carlos:log(IDList, ?carlos_type_relic, ?carlos_op_match_success)
    end,
    {noreply, State};

do_handle_info({cs_carlos_relic_buy,RoleID}, State) ->
    case check_relic_buy(RoleID) of
        {ok,CurBuyTime,NeedGlod} ->
            %CurBuyTime用于日志记录
            ?INFO("cs_carlos_relic_buy role_server ~w ~w ~w",[RoleID,CurBuyTime,NeedGlod]),
            send_msg_to_role(RoleID,{cs_carlos_relic_buy,CurBuyTime,NeedGlod});
        {fail,Reason} ->
            ?unicast(RoleID, #sc_carlos_relic_buy{result=Reason,cur_times=0,times=0,next_need=0})
    end,
    {noreply, State};
do_handle_info({relic_buy_pay_success, RoleID,CurBuyTime}, State) ->
    ?INFO("relic_buy_pay_success ~w ~w",[RoleID,CurBuyTime]),
    NeedList = data_relic:get(need_gold),
    MaxBuyTimes = length(NeedList),
    #relic_role_data{remain_fight_times=RemainFightTimes,remain_buy_time=RemainBuyTime} = RelicRoleData = get_relic_role_data(RoleID),
    if 
        CurBuyTime /= (MaxBuyTimes - RemainBuyTime +1) ->
            ?ERR("relic_buy_pay_success logic wrong -1- ~w ~w",[CurBuyTime , MaxBuyTimes - RemainBuyTime +1]);
        CurBuyTime > MaxBuyTimes ->
            ?ERR("relic_buy_pay_success logic wrong -2- ~w ~w",[CurBuyTime , MaxBuyTimes]);
        true ->
            ets:update_element(?ETS_RELIC_ROLE_INFO, RoleID, {#relic_role_data.remain_fight_times, RemainFightTimes+1})
    end,
    NextGold = if
                    MaxBuyTimes =:= CurBuyTime ->
                        0;
                    true ->
                        lists:nth(CurBuyTime+1, NeedList)
               end,
    ets:insert(?ETS_RELIC_ROLE_INFO, RelicRoleData#relic_role_data{remain_fight_times=RemainFightTimes+1,remain_buy_time=RemainBuyTime-1}),
    ?unicast(RoleID, #sc_carlos_relic_buy{result=0,cur_times=CurBuyTime,times=MaxBuyTimes - CurBuyTime,next_need=NextGold}),
    {noreply, State};

%% 报名结果
do_handle_info({sign_result, _ID, _Result}, State) ->
    ignore, %如果能发送报名请求给匹配服，即认定匹配是成功的
    {noreply, State};

do_handle_info({get_player, RoleID, WarID,SID,P,Type},State) ->
    F = fun()->
                {FighterList,RoleLieuAdd,Talent,TrSpecial} = role_data:get_otherRoleFighter(RoleID), %战斗信息
                ItemList = role_data:get_otherRoleItemEquips(RoleID),                      %装备信息
                RP = role_lib:get_rolePublic(RoleID),
                Fly = RP#rolePublic.plane_level,
				SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
                SpeedAdd = data_home:get({constr_type_plane_base,role_home:get_build_type_level(RoleID,?constr_type_plane_base)}),
				Msg = {Type,#player{serverID=data_setting:get(server_id)
									,roleID=RoleID
									,roleName=RP#rolePublic.roleName
									,fighters=FighterList
									,baseFighters=FighterList
									,addBuff = RoleLieuAdd
									,talent=Talent
									,trSpecial=TrSpecial
									,itemList=ItemList
									,type=Type
									,fly=Fly
									,speed=(carlos_server:get_fly_speed(Fly) * (10000 + SpeedAdd)) div 10000
									,level=RP#rolePublic.level
									,blood=100
									,head = RP#rolePublic.head
									,title = RP#rolePublic.title
									,isMale = RP#rolePublic.isMale
									,lastcarloswintime= 0
									,lastcarlosequaltime= 0
									,lastcarloslosetime = 0
									,lastseasonid = 0
									,carloswintime = 0
									,carlosequaltime = 0
									,carloslosetime = 0
									,seasonid = 0
									,fight_power = RP#rolePublic.fightPower
									,skin_info = SkinInfo
								   ,grade=doublematch_server:dirty_get_role_dm_grade(RoleID)
								   }},
				%?ERR("msg:~w~n~w",[Msg,P]),
				send_msg:direct(SID,P,Msg),
				%%                 ets:insert(?ETS_CARLOS_ROLE_WAR,{{match_info,RoleID},{WarID, SID,util:now()}}),
                behavior_carlos:log(RoleID, ?carlos_type_relic, ?carlos_op_enter_war, WarID),
				?CATCH(role_task:send_dispach(RoleID,{dispach_task,relic_fight,1})),
				ok
		end,
	spawn(F),
    {noreply,State};
do_handle_info({carlos_relic_bc_info,RoleID,{notice_relic_war_end,EndType,Msg,Others,LastPlayers,AfkList}},State) ->
    lists:foreach(fun({AfkRoleID,_AfkServerID})-> 
                    if 
                        RoleID =:= AfkRoleID->
                            afk_record_server:add_afk_record(RoleID,?AFK_TYPE_RELIC);
                        true ->
                            ignore
                    end
            end, AfkList),
    RelicRoleData = get_relic_role_data(RoleID),
    case EndType of
        ?RELIC_LOSE ->
            mail_server:send_sys_mail(RoleID, ?MAIL_RELIC_REWARD_LOSE, [], "", []);
        _ ->
            ignore
    end,
    ets:insert(?ETS_RELIC_ROLE_INFO
               ,RelicRoleData#relic_role_data{sign_state=?RELIC_SIGN_STATE_NO_SIGN
                                             ,is_sleeper=false,other_list=Others,last_player=LastPlayers}),
    ?unicast(RoleID, Msg),
    behavior_carlos:log(RoleID, ?carlos_type_relic, ?carlos_op_close_war),
    {noreply,State};
do_handle_info({carlos_relic_bc_info,RoleID,Msg},State) ->
    ?unicast(RoleID, Msg),
    {noreply,State};
do_handle_info({relic_reward,RoleID,Type,RewardInfo},State)->
    IsDouble = activity_server:is_activity(?relic_box_double),
    case Type of
        ?RELIC_REWARD_TYPE_FIGHT ->
            if
                IsDouble ->
                    role_lib:send_server(RoleID, {relic_reward,role_reward:reward_plus_reward(RewardInfo, RewardInfo),?MONEY_ADD_TYPE_RELIC_FIGHT,0});
                true ->
                    role_lib:send_server(RoleID, {relic_reward,RewardInfo,?MONEY_ADD_TYPE_RELIC_FIGHT,0})
            end;
        ?RELIC_REWARD_TYPE_KILL ->
            if
                IsDouble ->
                    role_lib:send_server(RoleID, {relic_reward,role_reward:reward_plus_reward(RewardInfo, RewardInfo),?MONEY_ADD_TYPE_RELIC_KILL,0});
                true ->
                    role_lib:send_server(RoleID, {relic_reward,RewardInfo,?MONEY_ADD_TYPE_RELIC_KILL,0})
            end;
        ?RELIC_REWARD_TYPE_WIN_OUTER ->
            ?INFO("relic win, but this's a outer(~w)",[RoleID]),
            mail_server:send_sys_mail(RoleID, ?MAIL_RELIC_REWARD_WIN_OUTER, [], "", []);
        ?RELIC_REWARD_TYPE_WIN ->
            {SpendTime,RewardRank,{KeyStoneTypeList,KeyStoneNum},BossLevelRank,FinalGerTypeID,WeakerList} = RewardInfo,
%%             BoxNameList = data_relic:get(relic_box_name),
            KeyStoneItemList = lists:foldl(fun(_Id,AccList)->
                    KeyStoneTypeId = lists:nth(random:uniform(4), KeyStoneTypeList),
                    ?INFO("relic_reward ~w ~w",[KeyStoneTypeList,KeyStoneTypeId]),
                    [{new_item,get_keystone_item_id(KeyStoneTypeId),1,1,0}|AccList]
                end, [], lists:seq(1, KeyStoneNum)),
%%             KeyStoneItemList = lists:usort([{new_item,get_keystone_item_id(KeyStoneTypeList),1,1,0}||E <- KeyStoneList]),
            KeyStoneReward = #sell_reward{coin=0,gerExp=0,gold=0,item=KeyStoneItemList,reputation=0,roleExp=0,newGer=[]},
            MailReward00 = data_relic:get({finish_time_mail_reward,BossLevelRank,RewardRank}),
            MailReward0 = role_reward:reward_plus_reward(MailReward00, KeyStoneReward),
            MailReward = if
                            IsDouble ->
                                role_reward:reward_reward_double(MailReward0);
                            true ->
                                MailReward0
                        end,
            ?INFO("Double ~w ~w ~w",[IsDouble,MailReward,MailReward0]),
            RelicRoleData = get_relic_role_data(RoleID),
            ets:insert(?ETS_RELIC_ROLE_INFO, RelicRoleData#relic_role_data{box_status=[1,1,1]
                                                                          ,box_rank={BossLevelRank,RewardRank,FinalGerTypeID}
                                                                          ,is_sleeper=lists:member(RoleID, WeakerList)}),
            mail_server:send_sys_mail(RoleID, ?MAIL_RELIC_REWARD_WIN, [SpendTime,data_relic:get({mail_rank_text,BossLevelRank,RewardRank})], "", MailReward),
            %%触发玩家巨龙遗迹任务变化
            role_payGuide:asyn_trigger_task_change(RoleID,?RELIC_DIFFICULTY_N_WIN_N,{BossLevelRank,RewardRank,1})
    end,
    {noreply, State};

do_handle_info({unrequest, SignID}, State) ->
    case ets:lookup(?ETS_RELIC_SIGN, {sign, SignID}) of
        [] ->
            ignore;
        [{_, IDList}] ->
            ServerID = data_setting:get(server_id),
            lists:foreach(fun(LevelRank)->
                            send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {unrequest, ServerID, SignID})
                        end, lists:seq(1, ?RELIC_LEVEL_NUM)),
            carlos_aux:set_team_unrequest_time(IDList),
            behavior_carlos:log(IDList, ?carlos_type_relic, ?carlos_op_unsign) 
    end,
    {noreply, State};

do_handle_info({role_offline,RoleID}, State) ->
    RelicRoleData = get_relic_role_data(RoleID),
    if
        RelicRoleData#relic_role_data.sign_state =:= ?RELIC_SIGN_STATE_MATCH ->
            SignID = ?make_id(-RoleID, 1),
            case ets:lookup(?ETS_RELIC_SIGN, {sign, SignID}) of
                [] ->
                    ignore;
                [{_,_IDList}] ->
                    ServerID = data_setting:get(server_id),
                    lists:foreach(fun(LevelRank)->
                                          send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {unrequest, ServerID, SignID})
                                  end, lists:seq(1, ?RELIC_LEVEL_NUM))
            end;
        true ->
            ignore
    end,
    {noreply, State};

do_handle_info({clear_war_info,RoleID,WarID,IsReturnTime}, State) ->
    RelicRoleData = get_relic_role_data(RoleID),
    if
        RelicRoleData#relic_role_data.sign_state /= ?RELIC_SIGN_STATE_NO_SIGN 
            andalso RelicRoleData#relic_role_data.war_id =:= WarID ->
            if
                IsReturnTime =:= true 
                  andalso RelicRoleData#relic_role_data.sign_state =:= ?RELIC_SIGN_STATE_FIGHT ->
                    #relic_role_data{remain_fight_times=OldRemainFightTimes} = RelicRoleData,
                    ets:insert(?ETS_RELIC_ROLE_INFO, RelicRoleData#relic_role_data{sign_state=?RELIC_SIGN_STATE_NO_SIGN
                                                                                  ,is_sleeper=false
                                                                                  ,remain_fight_times=OldRemainFightTimes+1});
                true ->
                    ets:insert(?ETS_RELIC_ROLE_INFO, RelicRoleData#relic_role_data{sign_state=?RELIC_SIGN_STATE_NO_SIGN
                                                                                  ,is_sleeper=false})
            end;
        true ->
            ?ERR("clear_war_info RoleID:~w",[RoleID]),
            ignore
    end,
    {noreply, State};
%% 存盘
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(),
    {noreply, State};

do_handle_info({check_server_status, EtsTab}, State) ->
    ?ERR("check_server_status ok ~n~p~n", [EtsTab]),
    io:format("check_server_status ok ~n~p~n", [EtsTab]),
    {noreply, State};

do_handle_info({test_war,MID,WarID},State) ->
	RelicRoleData = get_relic_role_data(MID),
	#relic_role_data{remain_fight_times=RemainFightTimes} = RelicRoleData,
	NewRemainFightTimes = max(0,RemainFightTimes-1),
	NewRelicRoleData = 
		RelicRoleData#relic_role_data{remain_fight_times=NewRemainFightTimes
									  ,sign_state=?RELIC_SIGN_STATE_FIGHT
									  ,war_id=WarID
									  ,fight_end_ts=util:now()+data_relic:get(war_interval)},
	ets:insert(?ETS_RELIC_ROLE_INFO, NewRelicRoleData),
	{noreply,State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~w.~n", [Info]),
    {noreply, State}.

%%%===================================================================
%%% Internal functions - other
%%%===================================================================

check_relic_sign(SelfRoleID,[OneMember|MemberList],_TeamID,AccList,LevelRank) ->
    RoleId = OneMember#p_team_member_info.roleID,
    RoleLevel = OneMember#p_team_member_info.level,
    {RankMinLevel,_,_} = data_relic:get({fight_level,LevelRank}),
    RelicRoleData = get_relic_role_data(RoleId),
    #relic_role_data{remain_fight_times=RemainFightTimes,sign_state=SignState} = RelicRoleData,
    if
        RoleLevel < RankMinLevel ->
            {fail,RoleId,9}; %队友等级不足
        SignState =:= ?RELIC_SIGN_STATE_FIGHT andalso SelfRoleID /= RoleId ->
            {fail,RoleId,5}; %队友在战斗中
        SignState =:= ?RELIC_SIGN_STATE_FIGHT andalso SelfRoleID =:= RoleId ->
            {fail,RoleId,8}; %自己在战斗中
        SignState =:= ?RELIC_SIGN_STATE_MATCH ->
            {fail,RoleId,6}; %匹配中
        RemainFightTimes =:= 0 ->
            {fail,RoleId,7}; %次数没了
        true ->
            check_relic_sign(SelfRoleID,MemberList,_TeamID
                            ,[RelicRoleData#relic_role_data{sign_state=?RELIC_SIGN_STATE_MATCH
                                                           ,is_sleeper=false}|AccList]
                            ,LevelRank)
    end;
check_relic_sign(_SelfRoleID,[],_TeamID,NewRelicDataList,_LevelRank) ->
    {ok,NewRelicDataList}.

check_relic_buy(RoleID)->
    NeedList = data_relic:get(need_gold),
    MaxBuyTimes = length(NeedList),
    #relic_role_data{remain_buy_time=RemainBuyTime} = get_relic_role_data(RoleID),
    if
        RemainBuyTime =< 0 ->
            {fail,2}; %购买次数没了
        true ->
            CurBuyTime = MaxBuyTimes - RemainBuyTime +1,
            {ok,CurBuyTime,lists:nth(CurBuyTime, NeedList)}
    end.
    
%取得某个玩家的巨龙遗迹信息
get_relic_role_data(RoleID)->
    case ets:lookup(?ETS_RELIC_ROLE_INFO, RoleID) of
        [] ->
            ets_insert_init_role_data(RoleID);
        [RelicData] ->
            Now = util:now(),
            if
                RelicData#relic_role_data.sign_state =:= ?RELIC_SIGN_STATE_FIGHT
                        andalso RelicData#relic_role_data.fight_end_ts < Now -> %战斗应该已经结束了
                    NewRelicData = RelicData#relic_role_data{sign_state=?RELIC_SIGN_STATE_NO_SIGN
                                                            ,is_sleeper=false},
                    ets:insert(?ETS_RELIC_ROLE_INFO, NewRelicData),
                    NewRelicData;
                true ->
                    RelicData
            end
    end.

%初始化某个玩家的巨龙遗迹信息
ets_insert_init_role_data(RoleID)->
    NewData = #relic_role_data{role_id=RoleID
                        ,remain_fight_times=data_relic:get(init_times)
                        ,remain_buy_time=length(data_relic:get(need_gold))
                        ,sign_state=?RELIC_SIGN_STATE_NO_SIGN
                        ,war_id=-1
                        ,box_status=[0,0,0]
                        ,box_rank={0,0,0}
                        ,fight_end_ts=0
                        ,is_sleeper=false},
%%     NewData = {relic_role_data,RoleID,data_relic:get(init_times),length(data_relic:get(need_gold)),?RELIC_SIGN_STATE_NO_SIGN
%%               ,-1,[0,0,0],{0,0,0},0,false},
    case ets:insert_new(?ETS_RELIC_ROLE_INFO, NewData) of
        true ->
            NewData;
        false ->
            ?ERR("ets_insert_init_role_data logic wrong. not should call ets_insert_init_role_data ~w",[ets:tab2list(?ETS_RELIC_ROLE_INFO)]),
            lists:nth(1, ets:lookup(?ETS_RELIC_ROLE_INFO, RoleID))
    end.

%发数据给玩家进程
send_msg_to_role(RoleID,Msg)->
    case catch role_lib:send_server(RoleID, Msg) of
                {'EXIT',Reason}->
                    ?INFO("send_msg_to_role fail ~w",[Reason]);
                _ ->
                    ok
    end.

%保存数据
do_persist() ->
    db_sql:set_etc(?DB_ETC_KEY_RELIC_ROLE_INFO, {erlang:date(),ets:tab2list(?ETS_RELIC_ROLE_INFO)}).

check_server_status(LevelRank)->
    ServerID = data_setting:get(server_id),
    send_msg:direct_by_name(carlos_match, ?RELIC_MATCH_SERVER_NAME(LevelRank), {check_server_status,ServerID}).
    
test_clean_status()->
    clean_relic_times().

test_clean_status(RoleId)->
    ets:delete(?ETS_RELIC_ROLE_INFO,RoleId).

%所有在线玩家可报名的都报名巨龙遗迹
%% all_in_fight()->
%%     lists:foreach(fun({RoleID,_})->
%%             relic_server:send_to_me(test_auto_sign)
%%         end, ets:tab2list(?ETS_ROLE_ONLINE)).

%% 判断队伍是否报名了
is_team_sign(TeamID, TeamLen) ->
    ID = ?make_id(TeamID, TeamLen),
    case ets:lookup(?ETS_RELIC_SIGN, {sign, ID}) of
        [_] ->
            {true, ID};
        _ ->
            false
    end.

%% 判断玩家个人是否保报了名
is_role_sign(RoleID) ->
    ID = ?make_id(-RoleID, 1),
    case ets:lookup(?ETS_RELIC_SIGN, {sign, ID}) of 
        [_] ->
            {true, ID};
        _ ->
            false
    end.

clean_relic_times()->
    OldList = ets:tab2list(?ETS_RELIC_ROLE_INFO),
    KeepList = filter_keep_data(OldList,[]),
    ets:delete_all_objects(?ETS_RELIC_ROLE_INFO),
    ets:insert(?ETS_RELIC_ROLE_INFO, KeepList).

%处于节约内存的考虑，仅保留匹配中或战斗中的数据。
%其他玩家如果次日战斗，重新初始化数据
filter_keep_data([],AccList)->
    AccList;
filter_keep_data([H|OldList],AccList)->
    InitTimes = data_relic:get(init_times),
    if
        H#relic_role_data.remain_fight_times > InitTimes->
            New = H#relic_role_data{remain_buy_time=length(data_relic:get(need_gold))},
            filter_keep_data(OldList,[New|AccList]);
        H#relic_role_data.sign_state =/= ?RELIC_SIGN_STATE_NO_SIGN andalso H#relic_role_data.remain_fight_times =< InitTimes->
            New = H#relic_role_data{remain_fight_times=InitTimes
                                   ,remain_buy_time=length(data_relic:get(need_gold))},
            filter_keep_data(OldList,[New|AccList]);
        true ->
            filter_keep_data(OldList,AccList)
    end.

%水
get_keystone_item_id(1)->
    7210;
%草
get_keystone_item_id(3)->
    7220;
%炎
get_keystone_item_id(2)->
    7230;
%飞
get_keystone_item_id(5)->
    7240;
%岩
get_keystone_item_id(8)->
    7250;
%超
get_keystone_item_id(7)->
    7260;
%电
get_keystone_item_id(4)->
    7270;
%格
get_keystone_item_id(_)->
    7280.

%% ====================================================================
%% Debug functions
%% ====================================================================

fix_relic_state(RoleID)->
    case ets:lookup(?ETS_RELIC_ROLE_INFO, RoleID) of
        [] ->
            ?undefined;
        [RelicRoleData] ->
            if
                ?RELIC_SIGN_STATE_MATCH =:= RelicRoleData#relic_role_data.sign_state ->
                    ets:update_element(?ETS_RELIC_ROLE_INFO, RoleID,[{#relic_role_data.sign_state,2}]),
                    ok;
                true ->
                    ignore
            end
    end.
                












