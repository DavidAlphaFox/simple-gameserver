-module(alien_master_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_alien.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% -define(lieuGerPos,100). % 参军的base位置,主要用在数据库中区分上阵武将，非上阵武将与参军
%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
%%state2和state的区别是status字段对应的枚举值发生了变化,需要将state里面的枚举进行转化,升级到对应的枚举
-record(state2, {count=0,session_id=0,status=0, next_status_left_seconds,master_node, master_server,
                slave_node_list=[], slave_server_list=[],
                is_all_connect=false, noticed_server_list=[],
                sign_list=[], guessOddNum=0, guessEvenNum=0,
                killNumRankList=[],killConRankList=[],fightRecordList=[],group_num_list=[]}).

-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(SYNC_STATUS_INTERVAL, 2000).

-define(role_alien_info, role_alien_info).
-define(rank_to_role_id, rank_to_role_id).
-define(fight_lock, fight_lock).
-define(replayRecord,replayRecord).
-define(MASTER_FLAG, master_flag).

-define(FIRST_FIVE_RANK_LIST, [1,2,3,4,5]).
-define(FIRST_SIX_RANK_LIST, [1,2,3,4,5,6]).

-define(KILL_NUM_RANK_MAX, data_alien:get(kill_num_rank_max)).
-define(KILL_CON_RANK_MAX, data_alien:get(kill_con_rank_max)).

-define(KILL_RANK_NEED_NUM, data_alien:get(kill_rank_need_num)).
-define(KILL_CON_RANK_NEED_NUM, data_alien:get(kill_con_rank_need_num)).
-define(KILL_CON_STOP_NUM, data_alien:get(kill_con_stop_num)).

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

init([]) ->
    process_flag(trap_exit,true),
    erlang:set_cookie(erlang:node(), data_setting:get(cookie)),
    case db_sql:get_etc(?DB_ETC_KEY_ALIEN) of
        [{state2, DBState2}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo),
            DBState = DBState2; 
        _ ->
            DBState = #state2{}
    end,
    MasterNode = erlang:node(),
    MasterServerName = get_cross_server_name(get_server_id(), true, data_setting:get(platform)),
    State = DBState#state2{count=0,master_node=MasterNode, master_server=MasterServerName,noticed_server_list=[]},
    %% 设置主服标识
    erlang:put(?MASTER_FLAG, true),
    erlang:send(erlang:self(), sync_status),
    %% 何时dump数据由主服务器控制
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
	erlang:send_after(?DUMP_INTERVAL, self(), do_hibernate),
    {ok, State}.

calc_left_seconds( ) ->
    case erlang:get(alien_master_next_time_stamp) of  
        Any when is_number(Any) ->
            max(0, Any - util:now( ));
        _ ->
            0
    end.

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
%% 测试用指令
handle_call({get_killcon_rank_list, GroupID}, _From, #state2{killConRankList=KillConRankListT}=State) ->
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            {reply, [], State};
        {_, List} ->
            {reply, List, State}
    end;

%% 测试用指令
handle_call({get_killnum_rank_list, GroupID}, _From, #state2{killNumRankList=KillNumRankListT}=State) ->
    case lists:keyfind(GroupID, 1, KillNumRankListT) of
        false ->
            {reply, [], State};
        {_, List} ->
            {reply, List, State}
    end;

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

handle_info(do_hibernate,State)->
	erlang:send_after(?DUMP_INTERVAL, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
    
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
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
terminate(_Reason, State) ->
    do_persist(State),
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
%% 断线重连的处理
do_handle_info(notice_request_status,State)->
	send_msg:direct_by_name(alien_distribute,{request_distribute_status, data_setting:get(server_id)}),
	{noreply,State};

do_handle_info({get_finals_match_list,Ref,FinalsServer},  #state2{killNumRankList=KillNumRankListT, killConRankList=KillConRankListT, group_num_list=GroupNumList}=State) ->
    PromoteRank = data_alien_finals:get(promote_rank),
    
    %% 普通排行榜
    List1 = lists:foldl(fun({GroundID, _}, Acc) ->
                            RankList = lists:seq(1, PromoteRank),
                            lists:foldl(fun(Rank, Acc2) ->
                                            case get_role_alien_by_rank(Rank, GroundID) of
                                                #role_alien{roleID=RoleID, fightPower=FightPower, roleName=RoleName, isMale=IsMale, title=Title, head=Head, 
                                                                level=Level,serverID=ServerID, fighterList=FighterList, atkAdd=AtkAdd, hpAdd=HpAdd,vip=Vip
                                                                ,itemList=EquipedList, talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo} ->

                                                    [#role_alien_final{roleID=RoleID, roleName=RoleName, isMale=IsMale, title=Title, fightPower=FightPower,vip=Vip, 
                                                                    head=Head, level=Level, serverID=ServerID, fighterList=FighterList, lieuAdd=AtkAdd
                                                                    ,equiped_list=EquipedList, talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo} | Acc2];
                                            _ ->
                                                Acc2
                                            end
                                        end, Acc, RankList)
                            end, [], GroupNumList),
    %% 连胜排行榜
    List2 = lists:foldl(fun({_GroupID, KillConRankList}, AccT) ->
                            lists:foldl(fun(#alien_fighter3{roleID=RoleID2, fightPower=FightPower2, roleName=RoleName2, isMale=IsMale2, title=Title2, head=Head2, level=Level2}, AccT2) ->
                                                case lists:keyfind(RoleID2, #role_alien_final.roleID, AccT2) of
                                                    false ->
                                                        #role_alien{serverID=ServerID2, fighterList=FighterList2, atkAdd=AtkAdd2, hpAdd=HpAdd2,vip=Vip
                                                                ,itemList=EquipedList2,talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo} = get_role_alien(RoleID2),
                                                        [#role_alien_final{roleID=RoleID2, roleName=RoleName2, isMale=IsMale2, title=Title2, fightPower=FightPower2, vip=Vip,
                                                                            head=Head2, level=Level2,serverID=ServerID2, fighterList=FighterList2, lieuAdd=AtkAdd2
                                                                            ,equiped_list=EquipedList2,talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo} | AccT2];
                                                    _ ->
                                                       AccT2 
                                                end
                                            end, AccT, lists:sublist(KillConRankList, PromoteRank))
                        end, List1, KillConRankListT),
    %% 杀人数排行榜
    List3 = lists:foldl(fun({_GroundID, KillNumRankList}, AccN) ->
                            lists:foldl(fun(#alien_fighter2{roleID=RoleID3, fightPower=FightPower3, roleName=RoleName3, isMale=IsMale3, title=Title3, head=Head3, level=Level3}, AccN2) ->
                                                case lists:keyfind(RoleID3, #role_alien_final.roleID, AccN2) of
                                                    false ->
                                                        #role_alien{serverID=ServerID3, fighterList=FighterList3, atkAdd=AtkAdd3, hpAdd=HpAdd3,itemList=EquipedList3
                                                                    ,talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo,vip=Vip} = get_role_alien(RoleID3),
                                                        [#role_alien_final{roleID=RoleID3, roleName=RoleName3, isMale=IsMale3, title=Title3, fightPower=FightPower3,vip=Vip, 
                                                                            head=Head3, level=Level3,serverID=ServerID3, fighterList=FighterList3, lieuAdd=AtkAdd3
                                                                            ,equiped_list=EquipedList3,talentList=TalentList,trSpecial=TrSpecial,skin_info=SkinInfo} | AccN2];
                                                    _ ->
                                                        AccN2
                                                end
                                        end, AccN, lists:sublist(KillNumRankList, PromoteRank))
                            end, List2, KillNumRankListT),
	send_msg:direct_by_name(FinalsServer,alien_finals, {get_finals_match_result,Ref, List3}),
	{noreply,State};

do_handle_info({do_alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, ServerID}, State) ->
    do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, ServerID, State),
    {noreply, State};
do_handle_info({fight_over, RoleID, ServerID, TarRoleID, IsWin, FightRecord, NewAtkFighterList,NewDefFighterList,NeedTimes, NeedGold}, State) ->
    {ok, NewState} = do_fight_over_succ(RoleID, ServerID, TarRoleID, IsWin, FightRecord, NewAtkFighterList,NewDefFighterList, NeedTimes, NeedGold, State),
    {noreply, NewState};
do_handle_info({fight_over, RoleID, ServerID, TarRoleID}, State) ->
    do_fight_over_error(RoleID, ServerID, TarRoleID),
    {noreply, State};
do_handle_info({get_alien_fight_replay, RoleID, ReplayUID, ServerID}, State) ->
    {FightRecord, _} = get_replay_record(ReplayUID),
    send_msg_to_slave_sever_id(ServerID, {get_alien_fight_replay_return, RoleID, FightRecord}),
    {noreply, State};
do_handle_info({get_alien_info, RoleID, AlienTimes, ResetTime, ServerID}, State) ->
    get_alien_info(RoleID, AlienTimes, ResetTime, ServerID, State),
    {noreply, State};
do_handle_info({get_alien_first_five, RoleID, GroupID, ServerID}, State) ->
    get_alien_first_five(RoleID, GroupID, ServerID),
    {noreply, State};
do_handle_info({alien_view_other, RoleID, TarRoleID, ServerID}, State) ->
    do_alien_view_other(RoleID, TarRoleID, ServerID),
    {noreply, State};
do_handle_info({alien_view_other_dtl, RoleID, TarRoleID, ServerID}, State) ->
    do_alien_view_other_dtl(RoleID, TarRoleID, ServerID),
    {noreply, State};
do_handle_info({get_alien_kill_con_rank, RoleID, Start, Num, ServerID}, #state2{killConRankList=KillConRankListT}=State) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            KillConRankList = [];
        {_, KillConRankList} ->
            next
    end,
    get_alien_kill_con_rank(RoleID, Start + 1, Num, ServerID, KillConRankList),
    {noreply, State};
do_handle_info({get_alien_kill_num_rank, RoleID, Start, Num, ServerID}, #state2{killNumRankList=KillNumRankListT}=State) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    case lists:keyfind(GroupID, 1, KillNumRankListT) of
        false ->
            KillNumRankList = [];
        {_, KillNumRankList} ->
            next
    end,
    get_alien_kill_num_rank(RoleID, Start + 1, Num, ServerID, KillNumRankList),
    {noreply, State};
do_handle_info({get_alien_record, RoleID, Start, Num, ServerID}, #state2{fightRecordList=FightRecordListT}=State) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    case lists:keyfind(GroupID, 1, FightRecordListT) of
        false ->
            FightRecordList = [];
        {_, FightRecordList} ->
            next
    end,
    get_alien_record(RoleID, Start + 1, Num, ServerID, FightRecordList),
    {noreply, State};
do_handle_info({get_alien_guess_info, RoleID, ServerID}, State) ->
    get_alien_guess_info(RoleID, ServerID, State),
    {noreply, State};
do_handle_info({do_alien_guess, RoleID, RoleInfo, GuessCoin, GuessType, ServerID}, State) ->
    {ok, NewState} = do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, ServerID, State),
    {noreply, NewState};
do_handle_info({do_alien_sign, RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip, ServerID}, State) ->
    do_alien_sign(RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList, TalentList,TrSpecial,SkinInfo,Vip, ServerID, State);
do_handle_info({do_alien_reset, RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip,ServerID}, State) ->
    do_alien_reset(RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList, TalentList,TrSpecial,SkinInfo,Vip,ServerID, State),
    {noreply, State};
do_handle_info(do_sign, #state2{session_id=SessionID}=State) ->
    ?ERR("开始报名", []),
    erlang:spawn(fun() -> db_sql:del_replay_with_type(?REPLAY_TYPE_ALIEN) end),
    NewState = State#state2{session_id=SessionID+1,sign_list=[], guessOddNum=0, guessEvenNum=0,
                           killNumRankList=[],killConRankList=[],fightRecordList=[],group_num_list=[]},
    send_msg_to_slave(NewState#state2.slave_server_list, do_sign),
    {noreply, NewState};
do_handle_info(do_fight, State) ->
    ?ERR("开始比赛~~~~~~~~~~", []),
    {ok, NewState} = do_fight(State),
    send_msg_to_slave(NewState#state2.slave_server_list, do_fight),
    {noreply, NewState};
do_handle_info(do_final, State) ->
    ?ERR("开始总决赛~~~~~~~~",[]),
    %%战场结束后,即发送奖励
    erlang:send_after(data_alien:get(delay_seconds) * 1000, erlang:self(), send_reward),
    send_msg_to_slave(State#state2.slave_server_list, do_final),
    {noreply, State}; 
do_handle_info(do_close, State) ->
    ?ERR("关闭跨服战", []),
    send_msg_to_slave(State#state2.slave_server_list, do_close),
    {noreply, State};
do_handle_info(send_reward, State) ->
    send_reward(State),
    {noreply, State};
do_handle_info(sync_status, State) ->
    erlang:send_after(?SYNC_STATUS_INTERVAL, erlang:self(), sync_status),
    case data_setting:get(is_need_connect) of
        true ->
            NewState = sync_status(State, false);
        _ ->
            NewState = State#state2{is_all_connect=false}
    end,
    {noreply, NewState};
do_handle_info(force_sync_status, State) ->
    NewState = sync_status(State, true),
    {noreply, NewState};
do_handle_info(dump_data, #state2{count=Count}=State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    send_msg_to_slave(State#state2.slave_server_list, dump_data),
    case Count rem 6 of
        0 ->
            erase_replay_record();
        _ ->
            next
    end,
    {noreply, State#state2{count=Count+1}};
do_handle_info(show_current_status, #state2{status=Status, next_status_left_seconds=LeftSeconds}=State) ->
    ?ERR("Current Status:~w, NextTimeStamp:~w, LeftSeconds:~w.~n", [Status, erlang:get(alien_master_next_time_stamp), LeftSeconds]),
    {noreply, State};
do_handle_info({update_server_list,SlaveList}, State) ->
    %% 获得分配的子服务器列表后,即清除旧数据,但时间戳不能被清除掉
    NextTimestamp = erlang:get(alien_master_next_time_stamp),
    erlang:erase(),
    erlang:put(alien_master_next_time_stamp,NextTimestamp),
    do_update_server_list(SlaveList,State);
%% 断线时的数据更新
do_handle_info({update_server_list,reconnect,SlaveList}, State) ->
    {SlaveNodeList,SlaveServerList} = lists:foldl(fun({SlaveNode,SlaveServer}, {AccNode,AccServer}) ->
                                                    {[SlaveNode|AccNode], [SlaveServer|AccServer]}  
                                                  end, {[],[]}, SlaveList),
    NewState = State#state2{slave_node_list=SlaveNodeList,slave_server_list=SlaveServerList,noticed_server_list=[]}, 
    ?ERR("重新更新server_list成功~n, master_node:~w,master_server:~w,slave_node_list:~w,slave_server_list:~w",
            [NewState#state2.master_node, NewState#state2.master_server, SlaveNodeList, SlaveServerList]),
    {noreply, NewState};
do_handle_info({update_role_name,RoleID, Name}, State) ->
    case get_role_alien(RoleID) of
        #role_alien{roleID=RoleID} = Info ->
            set_role_alien(Info#role_alien{roleName=Name});
        _ ->
            ignore
    end,
    {noreply, State};
do_handle_info({get_self_alien_rank, RoleID, ServerID}, #state2{killNumRankList=KillNumRankListT, killConRankList=KillConRankListT}=State) ->
    case get_role_alien(RoleID) of
        undefined ->
            send_msg_to_slave_sever_id(ServerID, {get_self_alien_rank_return, RoleID, false, 0, 0});
        #role_alien{group_id=GroupID, is_sign=IsSign} ->
            case GroupID =:= 0 orelse IsSign =:= false of
                true ->
                    send_msg_to_slave_sever_id(ServerID, {get_self_alien_rank_return, RoleID, false, 0, 0});
                _ ->
                    KillNumRank = 
                        case lists:keyfind(GroupID, 1, KillNumRankListT) of
                            false ->
                                0;
                            {_, KillNumRankList} ->
                                case lists:keyfind(RoleID, #alien_fighter2.roleID, KillNumRankList) of
                                    false ->
                                        0;
                                    #alien_fighter2{rank=NumRank} ->
                                        NumRank 
                                end
                        end,
                    ConNumRank = 
                        case lists:keyfind(GroupID, 1, KillConRankListT) of
                            false ->
                                0;
                            {_, KillConRankList} ->
                                case lists:keyfind(RoleID, #alien_fighter3.roleID, KillConRankList) of
                                    false ->
                                        0;
                                    #alien_fighter3{rank=ConRank} ->
                                        ConRank 
                                end
                        end,
                    send_msg_to_slave_sever_id(ServerID, {get_self_alien_rank_return, RoleID, true, KillNumRank, ConNumRank})
            end
    end,
    {noreply, State};
do_handle_info({update_status, NewStatus, NewNextTimestamp},#state2{status=Status}=State) ->
    %% 分配服刷新状态数据
    ?ERR("收到分配服更新状态消息:~p,~p.~n",[NewStatus, NewNextTimestamp]),
    erlang:put(alien_master_next_time_stamp, NewNextTimestamp),
    NewState = State#state2{status=NewStatus, next_status_left_seconds=NewNextTimestamp - util:now()},
    %% 有可能只是刷新时间,状态不改变
    case Status =:= NewStatus of
        true ->
            ignore;
        _ ->
            case get_next_status_msg(Status) of
                ignore ->
                    ignore;
                Msg ->
                    erlang:send(self(), Msg)
            end
    end,
    {noreply, NewState};
do_handle_info(test_do_fight, #state2{status=Status,sign_list=SignList}=State) ->
    case Status =/= ?STATUS_FIGHT of
        true ->
            ignore;
        _ ->
            lists:map(fun(RoleID) ->
                        TarRoleID = util:random_one_from_list(SignList),
                        case RoleID =:= TarRoleID of
                           true -> 
                                ignore;
                           _ ->
                                #role_alien{serverID=ServerID} = RoleAlien = get_role_alien(RoleID),
                                TarRoleAlien = get_role_alien(TarRoleID),
                                do_alien_fight2(RoleID, ServerID, TarRoleID, RoleAlien, TarRoleAlien, 0, 0)
                        end
                     end, SignList)
    end,
    {noreply, State};

do_handle_info(fix_alien_master_figherlist,#state2{sign_list=SignList}=State) ->
    lists:map(fun(RoleID) ->
              RoleAlienInfo = erlang:get({?role_alien_info, RoleID}),
              #role_alien{fighterList = FighterList} = RoleAlienInfo,
              NewFighterList = lists:foldl(fun(Fighter,Acc) ->
                                            [ger_attr:transOldGer2NewGer(Fighter)|Acc]
                                        end,[],FighterList),
              erlang:put({?role_alien_info,RoleID},RoleAlienInfo#role_alien{fighterList = NewFighterList})
            end,SignList),
    {noreply,State};

do_handle_info({check_server_alive,Ref,DistributeServer},State) ->
	send_msg:direct_by_name(DistributeServer,alien_distribute,{server_alive, Ref}),
	{noreply,State};

do_handle_info(void,State) ->
    {noreply, State};
do_handle_info({alien_server_get_alien_master_status,ServerID}
			  , #state2{session_id=SessionID, status=Status, next_status_left_seconds=LeftSeconds,
                              slave_server_list=SlaveServerList, master_server=MasterServerName}=State)->
	case lists:member(alien_master_server:get_cross_server_name(ServerID, false, data_setting:get(platform)), SlaveServerList) of
		true ->
	send_msg:direct(ServerID,alien_server,{update_status, SessionID, Status, LeftSeconds,MasterServerName});
		_ ->
			ignore
	end,
	{noreply,State};

do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.

do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state2,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_ALIEN,Info2).

is_persist({{?role_alien_info, _}, _}) ->
    true;
is_persist({{?rank_to_role_id, _, _}, _}) ->
    true;
is_persist({alien_master_next_time_stamp,_}) ->
    true;
is_persist(_) ->
    false.

%%--------------------------------------------------------------------------------------------------------------------------
recalc_fighter_list(FighterList, FighterList2) ->
    lists:foldr(fun(#ger{gerID=GerID}=Ger, Acc) ->
                        case lists:keyfind(GerID, #ger.gerID, FighterList2) of
                            false ->
                                [Ger|Acc];
                            #ger{gerHp=GerHp,gerProHp=GerProHp} ->
                                NewHp = min(GerHp,Ger#ger.gerAttr#gerAttr.gerHpMax),
                                [Ger#ger{gerHp=NewHp,gerProHp=GerProHp}|Acc]
                        end
                end, [], FighterList).

do_fight_over_succ(RoleID, ServerID, TarRoleID, IsWin, FightRecord, AtkFighterListT,DefFighterListT, NeedTimes, NeedGold, State) ->
    #role_alien{roleName=AtkName,rank=RoleRank,killNum=RoleKillNum,
                killContinuousNum=RoleKillContinuousNum,
                maxKillContinuousNum=RoleMaxKillContinuousNum,
                group_id=GroupID,fighterList=RoleFighterList} = get_role_alien(RoleID),
    #role_alien{roleName=DefName,rank=TarRoleRank,killNum=TarRoleKillNum,
                killContinuousNum=TarRoleKillContinuousNum,
                maxKillContinuousNum=TarRoleMaxKillContinuousNum,
                group_id=GroupID, serverID=TarServerID,fighterList=TarRoleFighterList} = get_role_alien(TarRoleID),
    AtkFighterList = recalc_fighter_list(RoleFighterList, AtkFighterListT),
    DefFighterList = recalc_fighter_list(TarRoleFighterList, DefFighterListT),
    #state2{group_num_list=GroupNumList} = State,
    {_, GroupNum} = lists:keyfind(GroupID, 1, GroupNumList),
    case IsWin of
        true ->
            NewRoleFighterList = winner_zero_hp_fixup(AtkFighterList),
            NewRoleHPPercent = calc_hp_percent(AtkFighterList),
            NewRoleKillNum = RoleKillNum + 1,
            NewRoleKillContinuousNum = RoleKillContinuousNum + 1,
            NewRoleMaxKillContinuousNum =
                case NewRoleKillContinuousNum > RoleMaxKillContinuousNum of
                    true ->
                        NewRoleKillContinuousNum;
                    false ->
                        RoleMaxKillContinuousNum
                end,
            NewRoleIsInContinuous =
                case NewRoleKillContinuousNum >= 2 andalso NewRoleKillContinuousNum > RoleMaxKillContinuousNum of
                    true ->
                        true;
                    false ->
                        false
                end,
            AddCoin = data_alien:get(win_coin),
            
            NewTarRoleFighterList = full_hp(DefFighterList),
            NewTarRoleHPPercent = 100,
            NewTarRoleKillNum = TarRoleKillNum,
            NewTarRoleKillContinuousNum = 0,
            NewTarRoleMaxKillContinuousNum = TarRoleMaxKillContinuousNum,
            NewTarRoleIsInContinuous = false,
            
            
            case RoleRank > TarRoleRank of
                true ->
                    refresh_rank(RoleRank, TarRoleRank, RoleRank, GroupID, GroupNum);
                false ->
                    refresh_rank(0, 0, TarRoleRank, GroupID, GroupNum)
            end;
        false ->
            NewRoleFighterList = full_hp(AtkFighterList),
            NewRoleHPPercent = 100,
            NewRoleKillNum = RoleKillNum,
            NewRoleKillContinuousNum = 0,
            NewRoleMaxKillContinuousNum = RoleMaxKillContinuousNum,
            NewRoleIsInContinuous = false,
            AddCoin = data_alien:get(lose_coin),
            
            NewTarRoleFighterList = winner_zero_hp_fixup(DefFighterList),
            NewTarRoleHPPercent = calc_hp_percent(DefFighterList),
            NewTarRoleKillNum = TarRoleKillNum + 1,
            NewTarRoleKillContinuousNum = TarRoleKillContinuousNum + 1,
            NewTarRoleMaxKillContinuousNum =
                case NewTarRoleKillContinuousNum > TarRoleMaxKillContinuousNum of
                    true ->
                        NewTarRoleKillContinuousNum;
                    false ->
                        TarRoleMaxKillContinuousNum
                end,
            NewTarRoleIsInContinuous =
                case NewTarRoleKillContinuousNum >= 2 andalso NewTarRoleKillContinuousNum > TarRoleMaxKillContinuousNum of
                    true ->
                        true;
                    false ->
                        false
                end,
            
            
            refresh_rank(0, 0, RoleRank, GroupID, GroupNum)
    end,
    update_role_alien(RoleID, NewRoleFighterList, NewRoleHPPercent, NewRoleKillNum,
                NewRoleKillContinuousNum, NewRoleMaxKillContinuousNum, NewRoleIsInContinuous),
    update_role_alien(TarRoleID, NewTarRoleFighterList, NewTarRoleHPPercent, NewTarRoleKillNum,
                NewTarRoleKillContinuousNum, NewTarRoleMaxKillContinuousNum, NewTarRoleIsInContinuous),
    
    send_msg_to_slave_sever_id(ServerID, {do_alien_fight_succ_return, RoleID, IsWin, DefName, FightRecord, NeedTimes,
                                          NeedGold, AddCoin,
                                          get_role_rank(RoleID), get_fighter_list(RoleID, true), NewRoleKillContinuousNum, TarRoleKillContinuousNum}),
    send_msg_to_slave_sever_id(TarServerID, {be_fighted, TarRoleID, not IsWin, AtkName, get_role_rank(TarRoleID), FightRecord, NewTarRoleKillContinuousNum}),
    
    unlock(RoleID),
    unlock(TarRoleID),
    
    {ok, NewState} = update_state(RoleID, NewRoleKillNum,
                                  NewRoleKillContinuousNum,
                                  TarRoleID, NewTarRoleKillNum,
                                  NewTarRoleKillContinuousNum,
                                  RoleKillContinuousNum,
                                  RoleMaxKillContinuousNum,
                                  TarRoleKillContinuousNum,
                                  TarRoleMaxKillContinuousNum,
                                  IsWin, State, FightRecord,GroupID),
    
    {ok, NewState}.

update_state(RoleID, NewRoleKillNum,
             NewRoleKillContinuousNum,
             TarRoleID, NewTarRoleKillNum,
             NewTarRoleKillContinuousNum,
             RoleKillContinuousNum,
             RoleMaxKillContinuousNum,
             TarRoleKillContinuousNum,
             TarRoleMaxKillContinuousNum,
             IsWin, #state2{killNumRankList=KillNumRankList,killConRankList=KillConRankList,fightRecordList=RecordList}=State, FightRecord,GroupID) ->
    case IsWin of
        true ->
            %%?DEBUG("NewRoleKillContinuousNum:~w,RoleMaxKillContinuousNum:~w", [NewRoleKillContinuousNum,RoleMaxKillContinuousNum]),
            case NewRoleKillContinuousNum >= ?KILL_CON_RANK_NEED_NUM andalso NewRoleKillContinuousNum > RoleMaxKillContinuousNum of
                true ->
                    NewKillConRankList = new_kill_con_rank_list(RoleID, NewRoleKillContinuousNum, KillConRankList,GroupID);
                false ->
                    NewKillConRankList = refresh_kill_con_rank_list(KillConRankList,GroupID)
            end,
            %%?DEBUG("NewRoleKillNum:~w", [NewRoleKillNum]),
            case NewRoleKillNum >= ?KILL_RANK_NEED_NUM of
                true ->
                    NewKillNumRankList = new_kill_num_rank_list(RoleID, NewRoleKillNum, KillNumRankList,GroupID);
                false ->
                    NewKillNumRankList = KillNumRankList
            end,
            Bool1 = lists:member(NewRoleKillContinuousNum, data_alien:get(kill_con_num_list)),
            Bool2 = TarRoleKillContinuousNum >= ?KILL_CON_STOP_NUM andalso TarRoleKillContinuousNum =:= TarRoleMaxKillContinuousNum,
            case Bool1 orelse Bool2 of
                true ->
                    NewRecordList = new_record_list(RoleID, NewRoleKillNum, NewRoleKillContinuousNum,
                                                    TarRoleID, TarRoleMaxKillContinuousNum,
                                                    RecordList, Bool1, Bool2, FightRecord,GroupID,true);
                false ->
                    NewRecordList = RecordList
            end;
        false ->
            %%?DEBUG("NewTarRoleKillContinuousNum:~w,TarRoleMaxKillContinuousNum:~w", [NewTarRoleKillContinuousNum,TarRoleMaxKillContinuousNum]),
            case NewTarRoleKillContinuousNum >= ?KILL_CON_RANK_NEED_NUM andalso NewTarRoleKillContinuousNum > TarRoleMaxKillContinuousNum of
                true ->
                    NewKillConRankList = new_kill_con_rank_list(TarRoleID, NewTarRoleKillContinuousNum, KillConRankList,GroupID);
                false ->
                    NewKillConRankList = refresh_kill_con_rank_list(KillConRankList,GroupID)
            end,
            %%?DEBUG("NewTarRoleKillNum:~w", [NewTarRoleKillNum]),
            case NewTarRoleKillNum >= ?KILL_RANK_NEED_NUM of
                true ->
                    NewKillNumRankList = new_kill_num_rank_list(TarRoleID, NewTarRoleKillNum, KillNumRankList,GroupID);
                false ->
                    NewKillNumRankList = KillNumRankList
            end,
            Bool1 = lists:member(NewTarRoleKillContinuousNum, data_alien:get(kill_con_num_list)),
            Bool2 = RoleKillContinuousNum >= ?KILL_CON_STOP_NUM andalso RoleKillContinuousNum =:= RoleMaxKillContinuousNum,
            case Bool1 orelse Bool2 of
                true ->
                    NewRecordList = new_record_list(TarRoleID, NewTarRoleKillNum, NewTarRoleKillContinuousNum,
                                                    RoleID, RoleMaxKillContinuousNum,
                                                    RecordList, Bool1, Bool2, FightRecord,GroupID,false);
                false ->
                    NewRecordList = RecordList
            end
    end,
    {ok, State#state2{killNumRankList=NewKillNumRankList,killConRankList=NewKillConRankList,fightRecordList=NewRecordList}}.

new_record_list(RoleID, NewRoleKillNum, NewRoleKillContinuousNum,
                TarRoleID, TarRoleMaxKillContinuousNum,
                RecordListT, Bool1, Bool2, FightRecord,GroupID, WinIsAtk) ->
    case lists:keyfind(GroupID, 1, RecordListT) of
        false ->
            RecordList = [];
        {_, RecordList} ->
            next
    end,
    ReplayUID = tk_id:gen_replayUID(),
    erlang:spawn(fun() -> db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_ALIEN) end),
    #role_alien{roleName=AtkName, serverID=ServerID} = get_role_alien(RoleID),
    #role_alien{roleName=DefName} = get_role_alien(TarRoleID),
    NewRecord1 = 
        case Bool1 of
            true ->
                %%连胜
                MailReward = do_reward(ServerID,RoleID,0,NewRoleKillContinuousNum), 
                [#p_alien_record3{type=0, winIsAtk=WinIsAtk,winName=AtkName,loseName=DefName,continuousCount=NewRoleKillContinuousNum,
                        killCount=NewRoleKillNum,replayUID=ReplayUID,timestamp=util:now(),reward=MailReward}];
            false ->
                []
        end,

    NewRecord2 = 
        case Bool2 of
            true ->
                %%终结
                MailReward2 = do_reward(ServerID,RoleID,1,TarRoleMaxKillContinuousNum),
                [#p_alien_record3{type=1,winIsAtk=WinIsAtk,winName=AtkName,loseName=DefName,continuousCount=TarRoleMaxKillContinuousNum,
                    killCount=NewRoleKillNum,replayUID=ReplayUID,timestamp=util:now(),reward=MailReward2}];
            false ->
                []
        end,
    NewRecordList = NewRecord2 ++ NewRecord1 ++ RecordList,
    case erlang:length(NewRecordList) =<  data_alien:get(record_max_num) of
        true ->
            NewRecordList2 = NewRecordList;
        false ->
            #p_alien_record3{replayUID=LastReplayUID} = LastRecord = lists:last(NewRecordList),
            erlang:spawn(fun() -> db_sql:del_fightReplay(LastReplayUID) end),
            NewRecordList2 = lists:delete(LastRecord, NewRecordList)
    end,
    case lists:keyfind(GroupID, 1, RecordListT) of
        false ->
            [{GroupID, NewRecordList2}|RecordListT];
        _ ->
            lists:keyreplace(GroupID, 1, RecordListT, {GroupID, NewRecordList2})
    end.

new_kill_num_rank_list(RoleID, NewRoleKillNum, KillNumRankListT,GroupID) ->
    case lists:keyfind(GroupID, 1, KillNumRankListT) of
        false ->
            KillNumRankList = [];
        {_, KillNumRankList} ->
            next
    end,
    #role_alien{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID} = get_role_alien(RoleID),
    NewKillNum = #alien_fighter2{roleID=RoleID ,
                                 fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                 killNum=NewRoleKillNum ,timestamp=util:now()},
    %%?DEBUG("NewKillNum:~w", [NewKillNum]),
    case lists:keyfind(RoleID, #alien_fighter2.roleID, KillNumRankList) of
        false ->
            KillNumRankList2 = [NewKillNum|KillNumRankList];
        _ ->
            KillNumRankList2 = lists:keyreplace(RoleID, #alien_fighter2.roleID, KillNumRankList, NewKillNum)
    end,
    KillNumRankList3 = lists:sort(fun(#alien_fighter2{killNum=K1,timestamp=T1},
                                      #alien_fighter2{killNum=K2,timestamp=T2}) ->
                                          if
                                              K1 > K2 ->
                                                  true;
                                              K1 =:= K2 ->
                                                  T1 < T2; 
                                              true ->
                                                  false
                                          end
                                  end, KillNumRankList2),
    {KillNumRankList4, _} = 
        lists:foldr(fun(Elem, {Acc, AccRank}) ->
                            {[Elem#alien_fighter2{rank=AccRank}|Acc], AccRank - 1}
                    end, {[], erlang:length(KillNumRankList3)}, KillNumRankList3),
    case lists:keyfind(GroupID, 1, KillNumRankListT) of
        false ->
            [{GroupID, KillNumRankList4}|KillNumRankListT];
        _ ->
            lists:keyreplace(GroupID, 1, KillNumRankListT, {GroupID, KillNumRankList4})
    end.

refresh_kill_con_rank_list(KillConRankListT, GroupID) ->
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            KillConRankList = [];
        {_, KillConRankList} ->
            next
    end,
    
    KillConRankList2 =
        lists:map(
          fun(#alien_fighter3{roleID=ID}=Elem) ->
                  #role_alien{isInContinuous=Bool} = get_role_alien(ID),
                  Elem#alien_fighter3{isInContinuous=Bool}
          end, KillConRankList),

    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            [{GroupID,KillConRankList2}|KillConRankListT];
        _ ->
            lists:keyreplace(GroupID, 1, KillConRankListT, {GroupID, KillConRankList2})
    end.

new_kill_con_rank_list(RoleID, NewRoleKillContinuousNum, KillConRankListT,GroupID) ->
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            KillConRankList = [];
        {_, KillConRankList} ->
            next
    end,
    #role_alien{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID} = get_role_alien(RoleID),
    NewKillCon = #alien_fighter3{roleID=RoleID,
                                 fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=ServerID,
                                 killContinuousNum=NewRoleKillContinuousNum, timestamp=util:now()},
    %%?DEBUG("NewKillCon:~w", [NewKillCon]),
    case lists:keyfind(RoleID, #alien_fighter3.roleID, KillConRankList) of
        false ->
            KillConRankList2 = [NewKillCon|KillConRankList];
        #alien_fighter3{killContinuousNum=OldRoleKillContinuousNum} ->
            case NewRoleKillContinuousNum > OldRoleKillContinuousNum of
                true ->
                    KillConRankList2 = lists:keyreplace(RoleID, #alien_fighter3.roleID, KillConRankList, NewKillCon);
                false ->
                    KillConRankList2 = KillConRankList
            end
    end,
    KillConRankList3 = lists:sort(fun(#alien_fighter3{killContinuousNum=K1,timestamp=T1},
                                      #alien_fighter3{killContinuousNum=K2,timestamp=T2}) ->
                                          if
                                              K1 > K2 ->
                                                  true;
                                              K1 =:= K2 ->
                                                  T1 < T2; 
                                              true ->
                                                  false
                                          end
                                  end, KillConRankList2),
    {KillConRankList4, _} =
        lists:foldr(fun(#alien_fighter3{roleID=ID}=Elem, {Acc, AccRank}) ->
                            #role_alien{isInContinuous=Bool} = get_role_alien(ID),
                            {[Elem#alien_fighter3{rank=AccRank, isInContinuous=Bool}|Acc], AccRank - 1}       
                    end, {[], erlang:length(KillConRankList3)}, KillConRankList3),
    case lists:keyfind(GroupID, 1, KillConRankListT) of
        false ->
            [{GroupID,KillConRankList4}|KillConRankListT];
        _ ->
            lists:keyreplace(GroupID, 1, KillConRankListT, {GroupID,KillConRankList4})
    end.

update_role_alien(RoleID, NewRoleFighterList, NewRoleHPPercent, NewRoleKillNum,
                NewRoleKillContinuousNum, NewRoleMaxKillContinuousNum, NewRoleIsInContinuous) ->
    RoleAlien = get_role_alien(RoleID),
    NewRoleFighterList2 =
        lists:map(
          fun(#ger{gerBase=GerBase}=Ger) ->
                  NewGerBase = GerBase#gerBase{gerPos=erlang:abs(GerBase#gerBase.gerPos)},
                  Ger#ger{gerBase=NewGerBase}
          end, NewRoleFighterList),
    set_role_alien(RoleAlien#role_alien{fighterList=NewRoleFighterList2,hpPercent=NewRoleHPPercent, killNum=NewRoleKillNum,
                                        killContinuousNum=NewRoleKillContinuousNum,
                                        maxKillContinuousNum=NewRoleMaxKillContinuousNum,
                                        isInContinuous=NewRoleIsInContinuous,
                                        canBeAtkTime=util:now() + data_alien:get(cool_down)}).

calc_hp_percent(FighterList) ->
    {SumHP, SumHPMax} = lists:foldr(fun(#ger{gerHp=HP, gerAttr=#gerAttr{gerHpMax=HPMax}}, {AccHP, AccHPMax}) ->
                             {AccHP + HP, AccHPMax + HPMax}
                     end, {0, 0}, FighterList),
    erlang:trunc(SumHP / SumHPMax * 100).

full_hp(FighterList) ->
    lists:map(fun(#ger{gerAttr=#gerAttr{gerHpMax=HPMax}}=Ger) ->
                      Ger#ger{gerHp=HPMax}
              end, FighterList).

refresh_rank(OldRoleRank, NewRoleRank, DropRank, GroupID, GroupNum) ->
    %%?DEBUG("OldRoleRank:~w, NewRoleRank:~w, DropRank:~w, GroupID:~w, GroupNum:~w", [OldRoleRank, NewRoleRank, DropRank, GroupID, GroupNum]),
    case OldRoleRank > NewRoleRank of
        true ->
            change_rank(OldRoleRank, NewRoleRank,GroupID);
        false ->
            next
    end,
    drop_rank(DropRank, GroupID, GroupNum).

change_rank(LowRank, HighRank,GroupID) ->
    %%?DEBUG("LowRank:~w, HighRank:~w,GroupID:~w", [LowRank, HighRank,GroupID]),
    LowAlien = get_role_alien_by_rank(LowRank, GroupID),
    HighAlien = get_role_alien_by_rank(HighRank, GroupID),
    %%?DEBUG("LowAlien:~w, HighAlien:~w", [LowAlien#role_alien.roleID, HighAlien#role_alien.roleID]),
    set_role_alien(LowAlien#role_alien{rank=HighRank}),
    set_role_rank_to_role_id(HighRank, GroupID, LowAlien#role_alien.roleID),
    set_role_alien(HighAlien#role_alien{rank=LowRank}),
    set_role_rank_to_role_id(LowRank, GroupID, HighAlien#role_alien.roleID).

drop_rank(DropRank, GroupID, GroupNum) ->
    DropAlien = get_role_alien_by_rank(DropRank, GroupID),
    %%?DEBUG("DropRank:~w, GroupID:~w, DropAlien:~w", [DropRank, GroupID, DropAlien#role_alien.roleID]),
    case DropRank < GroupNum of
        true ->
            UpAlienList = lists:map(fun(UpRank) ->
                                            get_role_alien_by_rank(UpRank, GroupID)
                                    end, lists:seq(DropRank + 1, GroupNum));
        false ->
            UpAlienList = []
    end,
    set_role_alien(DropAlien#role_alien{rank=GroupNum}),
    set_role_rank_to_role_id(GroupNum, GroupID, DropAlien#role_alien.roleID),
    lists:foreach(fun(#role_alien{rank=UpRank}=UpAlien) ->
                          set_role_alien(UpAlien#role_alien{rank=UpRank-1}),
                          set_role_rank_to_role_id(UpRank-1,GroupID,UpAlien#role_alien.roleID)
                  end, UpAlienList).

do_fight_over_error(RoleID, ServerID, TarRoleID) ->
    unlock(RoleID),
    unlock(TarRoleID),
    send_msg_to_slave_sever_id(ServerID, {do_alien_fight_error_return, RoleID, 12, []}).

do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, ServerID, State) ->
    case catch check_alien_fight(RoleID, TarRoleID, TarTank, State) of
        {ok, RoleAlien, TarRoleAlien} ->
            do_alien_fight2(RoleID, ServerID, TarRoleID, RoleAlien, TarRoleAlien, NeedTimes, NeedGold);
        {false, Reason} ->
            case Reason of
                2 ->
                    NewFighterList = get_fighter_list(RoleID, true);
                _ ->
                    NewFighterList = []
            end,
            send_msg_to_slave_sever_id(ServerID, {do_alien_fight_error_return, RoleID, Reason, NewFighterList})
    end.

do_alien_fight2(RoleID, ServerID, TarRoleID, RoleAlien, TarRoleAlien, NeedTimes, NeedGold) ->
    AlienMasterServer = erlang:self(),
    #role_alien{itemList=AttackerItemList,fighterList=AttackerList,atkAdd=AtkAddA, hpAdd=HpAddA, talentList=TalentListA,trSpecial=TrSpecialA,skin_info=SkinInfoA} = RoleAlien,
    #role_alien{itemList=DefenderItemList,fighterList=DefenderList,atkAdd=AtkAddD, hpAdd=HpAddD, talentList=TalentListB,trSpecial=TrSpecialB,skin_info=SkinInfoB} = TarRoleAlien,
    ?INFO("do_alien_fight2 RoleAlien:~w ~n TarRoleAlien:~w",[RoleAlien,TarRoleAlien]),
    lock(RoleID),
    lock(TarRoleID),
    LieuAddA = role_fight:transformOldLieuAdd2NewLieuAdd({AtkAddA,HpAddA}),
    LieuAddD = role_fight:transformOldLieuAdd2NewLieuAdd({AtkAddD,HpAddD}),
    GerEquipList1 = role_item:assort_ger_equiplist(AttackerItemList),
    LegendAddList1 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList1],
    GerEquipList2 = role_item:assort_ger_equiplist(DefenderItemList),
    LegendAddList2 = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList2],
    erlang:spawn(fun() ->
                        case catch role_fight:new(filter_out_zero_hp(AttackerList), filter_out_zero_hp(DefenderList), LieuAddA, LieuAddD, TalentListA, TalentListB,TrSpecialA,TrSpecialB,SkinInfoA,SkinInfoB,LegendAddList1,LegendAddList2) of
                            {IsWin, FightRecord0, {_,_,NewAtkFighterList,NewDefFighterList}} ->
                                FighterList2 = role_data:get_FighterList_with_effect(AttackerItemList,DefenderItemList,FightRecord0#sc_fight_request.fighterList),
                                FightRecord = FightRecord0#sc_fight_request{fighterList = FighterList2},
                                 erlang:send(AlienMasterServer, {fight_over, RoleID, ServerID, TarRoleID, IsWin, FightRecord, NewAtkFighterList,NewDefFighterList, NeedTimes, NeedGold});
                             {'EXIT', Err} ->
                                 ?ERR("Err:~w", [Err]),
                                 erlang:send(AlienMasterServer, {fight_over, RoleID, ServerID, TarRoleID})
                         end
                 end).

filter_out_zero_hp(List) ->
    lists:filter(fun(#ger{gerHp=GerHP}) ->
                         GerHP > 0
                 end, List).

%% 修正获胜者为空血的情况(反弹或则奥义同归于尽)
winner_zero_hp_fixup(List) ->
    case lists:all(fun(#ger{gerHp=GerHp}) -> GerHp =< 0 end, List) of
        true ->
            [Ger#ger{gerHp=1} || Ger <- List];
        _ ->
            List
    end.

check_alien_fight(RoleID, TarRoleID, TarTank, #state2{status=Status, sign_list=SignList}) ->
    case Status of
        ?STATUS_FIGHT ->
            next;
        _ ->
            erlang:throw({false, 11})
    end,
    case lists:member(RoleID, SignList) of
        true ->
            next;
        false ->
            erlang:throw({false, 13})
    end,
    #role_alien{group_id=RoleGroupID,rank=RoleRank,canBeAtkTime=RoleCanBeAtkTime} = RoleAlien = get_role_alien(RoleID),
    TarRoleAlien = get_role_alien(TarRoleID),
    case TarRoleAlien of
        ?undefined ->
            erlang:throw({false, 8});
        #role_alien{group_id=TarRoleGroupID,rank=TarRank2} ->
            case RoleGroupID =:= TarRoleGroupID of
                true ->
                    case TarRank2 =:= TarTank of
                        true ->
                            case erlang:abs(RoleRank - TarRank2) =< 5 of
                                true ->
                                    next;
                                false ->
                                    erlang:throw({false, 15})
                            end;
                        false ->
                            erlang:throw({false, 2})
                    end;
                false ->
                    erlang:throw({false, 14})
            end
    end,
    Now = util:now(),
    case RoleCanBeAtkTime =< Now of
        true ->
            case TarRoleAlien#role_alien.canBeAtkTime =< Now of
                true ->
                    next;
                false ->
                    erlang:throw({false, 17})
            end;
        false ->
            erlang:throw({false, 16})
    end,
    case is_lock(RoleID) of
        false ->
            next;
        true ->
%%             next
            erlang:throw({false, 4})
    end,
    case is_lock(TarRoleID) of
        false ->
            next;
        true ->
%%             next
            erlang:throw({false, 5})
    end,
    {ok, RoleAlien, TarRoleAlien}.

get_alien_info(RoleID, AlienTimes, ResetTime, ServerID, State) ->
    #state2{status=Status,sign_list=SignList, next_status_left_seconds=LeftSeconds,group_num_list=GroupNumList} = State,
    case erlang:is_integer(LeftSeconds) of
        true ->
            EndTimestamp = util:now() + LeftSeconds;
        false ->
            EndTimestamp = util:now()
    end,
    IsSign = lists:member(RoleID, SignList),
    if 
        GroupNumList =:= [] ->
            GroupNum = 0;
        true ->
            GroupNum = case lists:last(GroupNumList) of
                            {GroupID,_} ->
                                GroupID;
                            _ ->
                                0
                            end
    end,
    Msg =
        case Status of
            ?STATUS_SIGN ->
                {?STATUS_SIGN, IsSign, RoleID, EndTimestamp};
            ?STATUS_FIGHT ->
                {?STATUS_FIGHT, IsSign, get_role_group_id(RoleID), get_fighter_list(RoleID, IsSign), RoleID, EndTimestamp, AlienTimes, ResetTime,GroupNum};
            ?STATUS_CLOSE ->
                {?STATUS_CLOSE, get_role_group_id(RoleID), get_fighter_list(RoleID, IsSign), RoleID, EndTimestamp, AlienTimes,GroupNum};
            ?STATUS_FINAL ->
                %% 总决赛时, 主服不在做实际操作, 玩家看到的数据和关闭时一样
                {?STATUS_FINAL, get_role_group_id(RoleID), get_fighter_list(RoleID, IsSign), RoleID, 0, AlienTimes,GroupNum}
        end,
    send_msg_to_slave_sever_id(ServerID, {get_alien_info_return, Msg}).

get_alien_first_five(RoleID, GroupID, ServerID) ->
    send_msg_to_slave_sever_id(ServerID, {get_alien_info_return, RoleID, get_fighter_list2(GroupID)}).

do_alien_view_other(RoleID, TarRoleID, ServerID) ->
    case get_role_alien(TarRoleID) of
        #role_alien{is_sign=true} = TarAlien ->
            case get_role_alien(RoleID) of
                #role_alien{is_sign=true} = RoleAlien ->
                    do_alien_view_other(RoleID, TarRoleID, ServerID ,RoleAlien, TarAlien);
                _ ->
                    do_alien_view_other(RoleID, TarRoleID, ServerID, ?undefined, TarAlien)
            end;
        _ ->
            next
    end.

do_alien_view_other_dtl(RoleID, TarRoleID, ServerID) ->
    case get_role_alien(TarRoleID) of
        #role_alien{is_sign=true} = TarAlien ->
            do_alien_view_other_dtl(RoleID, TarRoleID, ServerID, ?undefined, TarAlien);
        _ ->
            next
    end.

do_alien_view_other(RoleID, TarRoleID, ServerID, ?undefined, TarAlien) ->
    #role_alien{fightPower=FightPower,level=Level,roleName=RoleName,fighterList=FighterList} = TarAlien,
    GerViewList = [ger_lib:ger2p_ger(E)||E<-FighterList],
    Record = #sc_alien_view_other{tarRoleID1=0,roleName1= <<"">>,roleLevel1=0,fightPower1=0,gerList1=[],
                                  tarRoleID2=TarRoleID,roleName2=RoleName,roleLevel2=Level,fightPower2=FightPower,gerList2=GerViewList},
    send_msg_to_slave_sever_id(ServerID, {do_alien_view_other_return, RoleID, Record});
do_alien_view_other(RoleID, TarRoleID, ServerID ,RoleAlien, TarAlien) ->
    
    #role_alien{fightPower=FightPower1,level=Level1,roleName=RoleName1,fighterList=FighterList1} = RoleAlien,
    GerViewList1 = [ger_lib:ger2p_ger(E)||E<-FighterList1],
    
    #role_alien{fightPower=FightPower2,level=Level2,roleName=RoleName2,fighterList=FighterList2} = TarAlien,
    GerViewList2 = [ger_lib:ger2p_ger(E)||E<-FighterList2],
    
    Record = #sc_alien_view_other{tarRoleID1=RoleID,roleName1=RoleName1,roleLevel1=Level1,fightPower1=FightPower1,gerList1=GerViewList1,
                                  tarRoleID2=TarRoleID,roleName2=RoleName2,roleLevel2=Level2,fightPower2=FightPower2,gerList2=GerViewList2},
    
    send_msg_to_slave_sever_id(ServerID, {do_alien_view_other_return, RoleID, Record}).

%%此处的LieuViewList在2.6版本以及以前都是旧的p_liue_view结构,缺少gerQuality数据，如果异星中有旧的数据会照成这个结构不一致的情况
do_alien_view_other_dtl(RoleID, TarRoleID, ServerID, ?undefined, TarAlien) ->
    #role_alien{fightPower=FightPower,level=Level,isMale=IsMale,
				roleName=RoleName,fighterList=FighterListT,itemList=ItemList,
				atkAdd=AtkAdd, hpAdd=HpAdd,lieuViewList=LieuViewList,head=Head,title=Title,trSpecial=TrSpecial} = TarAlien,
    FighterList = ger_attr:refresh_other_fightPower(FighterListT, 0, 0),
    GerViewList = [ger_lib:ger2p_ger_view_dtl(E)||E<-FighterList],
    GerCrystalList =[ger_lib:ger2ger_crystalinfo_brief(E)||E<-FighterList],
    GerPosList = [ger_lib:ger2p_ger_pos(E)||E<-FighterList],
    EquipViewList = [item_lib:item2p_item_view_dtl(E)||E<-ItemList],
	SkinInfo = role_skin:get_skin_info(),
    PSkinInfo = role_skin:transforSkinInfo2PSkinInfo(SkinInfo),
    Record = 
        % #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName=RoleName,isMale=IsMale,roleLevel=Level,fightPower=FightPower, gerList=GerViewList
        %                        ,equipList=EquipViewList,gerPosList=GerPosList,atkAdd=AtkAdd, hpAdd=HpAdd,lieuViewList=transformOldLiueView2NewLiueView(LieuViewList)
        %                     ,head=Head,title=Title,trID=TrSpecial#trSpecial.trID,specialID=TrSpecial#trSpecial.specialID,skinInfo=PSkinInfo,gerCrystalList=GerCrystalList},
        #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName=RoleName,isMale=IsMale,roleLevel=Level,fightPower=FightPower, gerList=GerViewList
                               ,equipList=EquipViewList,gerPosList=GerPosList,lieuViewList=transformOldLiueView2NewLiueView(LieuViewList)
                            ,head=Head,title=Title,trID=TrSpecial#trSpecial.trID,specialID=TrSpecial#trSpecial.specialID,skinInfo=PSkinInfo,gerCrystalList=GerCrystalList},
    send_msg_to_slave_sever_id(ServerID, {do_alien_view_other_dtl_return, RoleID, Record}).

get_alien_kill_con_rank(RoleID, Start, Num, ServerID, KillConRankList) ->
    case Start =< is_list_length_in(KillConRankList, ?KILL_CON_RANK_MAX) of
        true ->
            List = lists:sublist(KillConRankList, Start, Num),
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_con_rank_return, RoleID, List});
        false ->
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_con_rank_return, RoleID, []})
    end.

get_alien_kill_num_rank(RoleID, Start, Num, ServerID, KillNumRankList) ->
    case Start =< is_list_length_in(KillNumRankList, ?KILL_NUM_RANK_MAX) of
        true ->
            List = lists:sublist(KillNumRankList, Start, Num),
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_num_rank_return, RoleID, List});
        false ->
            send_msg_to_slave_sever_id(ServerID, {get_alien_kill_num_rank_return, RoleID, []})
    end.

get_alien_record(RoleID, Start, Num, ServerID, FightRecordList) ->
    case Start =< erlang:length(FightRecordList) of
        true ->
            List = lists:sublist(FightRecordList, Start, Num),
            send_msg_to_slave_sever_id(ServerID, {get_alien_record_return, RoleID, List});
        false ->
            send_msg_to_slave_sever_id(ServerID, {get_alien_record_return, RoleID, []})
    end.

get_alien_guess_info(RoleID, ServerID, #state2{guessOddNum=GuessOddNum,guessEvenNum=GuessEvenNum}) ->
    {Type, Coin} = get_role_guess_type_and_coin(RoleID),
    send_msg_to_slave_sever_id(ServerID, {get_alien_guess_info_return, RoleID, Type, Coin, GuessOddNum, GuessEvenNum}).

do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, ServerID,
               #state2{status=Status,next_status_left_seconds=LeftSeconds,
                      guessOddNum=GuessOddNum,guessEvenNum=GuessEvenNum}=State) ->
    case Status of
        ?STATUS_FIGHT ->
            case LeftSeconds >= ?TEN_MINUTES_SECONDS of
                true ->
                    case get_role_alien(RoleID) of
                        ?undefined ->
                            OldGuessCoin = 0,
                            #role{isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
                            RoleAlien = #role_alien{roleID=RoleID,is_sign=true, group_id=0, fighterList=[],
                                                    fightPower=0,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,
                                                    rank = 0, serverID = ServerID, hpPercent=0, killNum=0, killContinuousNum=0,
                                                    isInContinuous=false, guessCoin=0, guessType=false};
                        #role_alien{guessCoin=OldGuessCoin} = RoleAlien ->
                            next
                    end,
                    case OldGuessCoin =:= 0  of
                        true ->
                            set_role_alien(RoleAlien#role_alien{guessCoin=GuessCoin, guessType=GuessType}),
                            send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 0, GuessCoin}),
                            case GuessType of
                                false ->
                                    NewGuessOddNum = GuessOddNum + 1,
                                    NewGuessEvenNum = GuessEvenNum;
                                true ->
                                    NewGuessOddNum = GuessOddNum,
                                    NewGuessEvenNum = GuessEvenNum + 1
                            end,
                            {ok, State#state2{guessOddNum=NewGuessOddNum,guessEvenNum=NewGuessEvenNum}};
                        false ->
                            send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 5}),
                            {ok, State}
                    end;
                false ->
                    send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 4}),
                    {ok, State}
            end;
        _ ->
            send_msg_to_slave_sever_id(ServerID, {do_alien_guess_return, RoleID, 4}),
            {ok, State}
    end.

do_alien_sign(RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo, Vip, ServerID,#state2{status=Status, sign_list=SignList}=State) ->
    case Status of
        ?STATUS_SIGN -> 
            case lists:member(RoleID, SignList) of
                false ->
                    ?ERR("RoleID:~w sign succ", [RoleID]),
                    #role{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
                    %%v4.1.0小伙伴传递过来的属性包含了特殊属性，将会使用#add_attr结构，此处为了避免修改role_alien结构，直接将原来的atkAdd和hpAdd都设置成传递过来的add_attr结构
                    set_role_alien(#role_alien{roleID=RoleID,is_sign=true, group_id=0, fighterList=FighterList,itemList=ItemList,
                                               fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,
                                               rank = 0, serverID = ServerID, hpPercent=100,
											   atkAdd=RoleLieuAdd, hpAdd=RoleLieuAdd, lieuViewList=RoleLieuAddList, 
											   killNum=0, killContinuousNum=0,trSpecial=TrSpecial,vip=Vip,
                                               isInContinuous=false, guessCoin=0, guessType=false,talentList=TalentList,skin_info=SkinInfo}),
                    send_msg_to_slave_sever_id(ServerID, {do_alien_sign_return, RoleID, 0}),
                    {noreply, State#state2{sign_list=[RoleID|SignList]}};
                true ->
                    send_msg_to_slave_sever_id(ServerID, {do_alien_sign_return, RoleID, 4}),
                    {noreply, State}
            end;
        _ ->
            send_msg_to_slave_sever_id(ServerID, {do_alien_sign_return, RoleID, 3}),
            {noreply, State}
    end.

do_alien_reset(RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip, ServerID, #state2{status=Status,sign_list=SignList}) ->
    case Status of
        ?STATUS_FIGHT ->
            case lists:member(RoleID, SignList) of
                true ->
                    #role{fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName} = RoleInfo,
                    RoleAlien = get_role_alien(RoleID),
                    %%v4.1.0小伙伴传递过来的属性包含了特殊属性，将会使用#add_attr结构，此处为了避免修改role_alien结构，直接将原来的atkAdd和hpAdd都设置成传递过来的add_attr结构
                    NewRoleAlien = RoleAlien#role_alien{fighterList=FighterList,itemList=ItemList,fightPower=FightPower,isMale=IsMale,
                                                        title=Title,head=Head,level=Level,roleName=RoleName,hpPercent=100,vip=Vip,
														atkAdd=RoleLieuAdd, hpAdd=RoleLieuAdd,lieuViewList=RoleLieuAddList,talentList=TalentList,skin_info=SkinInfo,trSpecial=TrSpecial},
                    set_role_alien(NewRoleAlien),
                    NewFighterList = get_fighter_list(RoleID, true),
                    send_msg_to_slave_sever_id(ServerID, {do_alien_reset_return, RoleID, 0, NewFighterList});
                false ->
                    send_msg_to_slave_sever_id(ServerID, {do_alien_reset_return, RoleID, 3})
            end;
        _ ->
            send_msg_to_slave_sever_id(ServerID, {do_alien_reset_return, RoleID, 2})
    end.

do_fight(State) ->
    #state2{sign_list=SignList} = State,
    Num = erlang:length(SignList),
    ?ERR("Num:~w, SignList:~w", [Num, SignList]),
    GroupNum = erlang:trunc(Num / data_alien:get(calc_arg)) + 1,
    EveryNum = Num div GroupNum,
    {GroupList, GroupNumList} = gen_group_list(GroupNum, util:random_list(SignList), [], [], EveryNum),
    ?ERR("GroupList:~w, GroupNumList:~w", [GroupList, GroupNumList]),
    lists:foreach(fun({GroupID, RoleIDList}) ->
                          init_group(GroupID, RoleIDList)
                  end, GroupList),
    {ok, State#state2{group_num_list=GroupNumList}}.

init_group(GroupID, RoleIDList) ->
    RoleAlienList = lists:map(fun(RoleID) ->
                                      get_role_alien(RoleID)
                              end, RoleIDList),
    lists:foldr(fun(RoleAlien, Rank) ->
                        set_role_alien(RoleAlien#role_alien{rank=Rank,group_id=GroupID}),
                        set_role_rank_to_role_id(Rank, GroupID, RoleAlien#role_alien.roleID),
                        Rank - 1
                end, erlang:length(RoleAlienList), util:random_list(RoleAlienList)).

gen_group_list(1, SignList, GroupList, GroupNumList, _EveryNum) ->
    {[{1, SignList}|GroupList], [{1,erlang:length(SignList)}|GroupNumList]};
gen_group_list(GroupNum, SignList, GroupList, GroupNumList, EveryNum) ->
    NewList = lists:sublist(SignList, EveryNum),
    gen_group_list(GroupNum - 1, SignList -- NewList, [{GroupNum,NewList}|GroupList], [{GroupNum,EveryNum}|GroupNumList], EveryNum).

send_reward(#state2{killNumRankList=KillNumRankListT,killConRankList=KillConRankListT,group_num_list=GroupNumList}) ->
    %?ERR("KillNumRankListT:~w", [KillNumRankListT]),
    %?ERR("KillConRankListT:~w", [KillConRankListT]),
    lists:foreach(fun({GroupID, KillNumRankList}) ->
                        send_reward_kill_num(GroupID, KillNumRankList)  
                  end, KillNumRankListT),
    lists:foreach(fun({GroupID, KillConRankList}) ->
                          send_reward_kill_con(GroupID, KillConRankList)
                  end, KillConRankListT),
    lists:foreach(fun({GroupID, NormalRankList}) ->
                          send_reward_normal_rank(GroupID, NormalRankList)
                  end, get_all_rank_data()),
    {Low,High} = data_alien:get(champion_reward_lvl),
    case data_alien:get(champion_reward) of 
        undefined ->
            ignore;
        Reward ->
            lists:foreach(fun({GroupID,_}) ->
                            case get_role_alien_by_rank(1,GroupID) of
                                #role_alien{serverID=ServerID,roleID=RoleID, roleName=RoleName} ->
                                    send_msg_to_slave_sever_id(ServerID,{send_champion_reward,RoleID,RoleName,GroupID,Reward,"",Low,High});
                                _ ->
                                    ignore
                            end
                          end, GroupNumList)
    end,
    lists:foreach(fun({GroupIDT, GuessList}) ->
                          case GroupIDT of
                              0 ->
                                  GroupID = 1;
                              GroupID ->
                                  next
                          end,
                          case lists:keyfind(GroupID, 1, KillConRankListT) of
                              false ->
                                  Bool = false;
                              {_, [#alien_fighter3{killContinuousNum=Val}|_]} ->
                                  case Val rem 2 of
                                      1 ->
                                          Bool = false;
                                      0 ->
                                          Bool = true
                                  end
                          end,
                          send_reward_guess(GuessList, Bool)
                  end, get_all_guess_data()).

send_reward_guess(GuessList, Bool) ->
    lists:foreach(fun(#role_alien{roleID=RoleID,guessCoin=GuessCoin,guessType=GuessType,serverID=ServerID}) ->
                          case GuessType =:= Bool of
                              true ->
                                  send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID,
                                                                       #sell_reward{coin=erlang:trunc(GuessCoin*data_alien:get(guess_right)),
                                                                                    roleExp=0,gerExp=0,gold=0,item=[],reputation=0,newGer=[]}, 1062, []});
                              false ->
                                  send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID,
                                                                       #sell_reward{coin=erlang:trunc(GuessCoin*data_alien:get(guess_error)),
                                                                                    roleExp=0,gerExp=0,gold=0,item=[],reputation=0,newGer=[]}, 1063, []})
                          end
                  end, GuessList).

send_reward_kill_num(_GroupID, KillNumRankList) ->
    lists:foldl(fun({Min, Max, SellReward}, List) ->
                    lists:foldl(fun(#alien_fighter2{roleID=RoleID,rank=Rank,serverID=ServerID}=Info, Acc) ->
                                    case Rank >= Min andalso Rank =< Max of
                                        true ->
                                            send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID, SellReward, 1060, [Rank]}),
                                            Acc;
                                        _ ->
                                            [Info|Acc]
                                    end
                                end, [], List)
                end, KillNumRankList, data_alien:get(kill_num_rank_reward_list)).

send_reward_kill_con(_GroupID, KillConRankList) ->
    lists:foldl(fun({Min, Max, SellReward}, List) ->
                    lists:foldl(fun(#alien_fighter3{roleID=RoleID,rank=Rank,serverID=ServerID}=Info, Acc) ->
                                    case Rank >= Min andalso Rank =< Max of
                                        true ->
                                            send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID, SellReward, 1061, [Rank]}),
                                            Acc;
                                        _ ->
                                            [Info|Acc]
                                    end
                                end, [], List)
                end, KillConRankList, data_alien:get(kill_con_rank_reward_list)).

send_reward_normal_rank(_GroupID, NormalRankList) ->
    lists:foldl(fun({Min, Max, SellReward}, List) ->
                lists:foldl(fun(#role_alien{roleID=RoleID,rank=Rank,serverID=ServerID}=Info, Acc) ->
                                case Rank >= Min andalso Rank =< Max of
                                    true ->
                                        send_msg_to_slave_sever_id(ServerID,{send_mail_reward, RoleID, SellReward, 1059, [Rank]}),
                                        Acc;
                                    _ ->
                                        [Info|Acc]
                                end
                            end, [], List)
                end, NormalRankList, data_alien:get(normal_rank_reward_list)).
%% -------------------------------------------------------------------------------------------------------------------------
is_lock(RoleID) ->
    case erlang:get({?fight_lock, RoleID}) of
        ?undefined ->
            false;
        _ ->
            true
    end.

lock(RoleID) ->
    erlang:put({?fight_lock, RoleID}, ?fight_lock).

unlock(RoleID) ->
    erlang:erase({?fight_lock, RoleID}).

get_fighter_list(RoleID, false) ->
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    lists:foldr(fun(RoleRank, Acc) ->
                      case get_role_alien_by_rank(RoleRank, GroupID) of
                          ?undefined ->
                              Acc;
                          RoleAlien ->
                              [RoleAlien|Acc]
                      end
              end, [], ?FIRST_FIVE_RANK_LIST);
get_fighter_list(RoleID, true) ->
    Rank = get_role_rank(RoleID),
    case get_role_group_id(RoleID) of
        0 ->
            GroupID = 1;
        GroupID ->
            next
    end,
    RankList = get_rank_list(Rank),
    lists:foldr(fun(RoleRank, Acc) ->
                      case get_role_alien_by_rank(RoleRank,GroupID) of
                          ?undefined ->
                              Acc;
                          RoleAlien ->
                              [RoleAlien|Acc]
                      end
              end, [], RankList).

get_rank_list(Rank) when Rank > 5 ->
    lists:seq(Rank - 5, Rank);
get_rank_list(Rank) when Rank =< 5 ->
    ?FIRST_SIX_RANK_LIST.

get_role_guess_type_and_coin(RoleID) ->
    case get_role_alien(RoleID) of
        ?undefined ->
            {false, 0};
        #role_alien{guessCoin=Coin, guessType=Type} ->
            {Type, Coin}
    end.

get_role_rank(RoleID) ->
    case get_role_alien(RoleID) of
        ?undefined ->
            0;
        #role_alien{rank=Rank} ->
            Rank
    end. 

get_role_group_id(RoleID) ->
    case get_role_alien(RoleID) of
        ?undefined ->
            0;
        #role_alien{group_id=GroupID} ->
            GroupID
    end.

get_role_alien(RoleID) ->
    Res = erlang:get({?role_alien_info, RoleID}),
    case Res of
        #role_alien{} ->
            Res;
        %% 走到这个case说明是读取了旧格式的数据，需要给天赋技能填上值。该情况会发生在1.3.0升级至1.4.0时
        {role_alien,RoleID,Is_sign, Group_id, FighterList, ItemList, FightPower, IsMale, Title, Head,
         Level, RoleName, Rank, ServerID, HpPercent, KillNum,
         KillContinuousNum, MaxKillContinuousNum, IsInContinuous,
         GuessCoin, GuessType, Timestamp,CanBeAtkTime, AtkAdd, HpAdd, LieuViewList} ->
            #role_alien{roleID=RoleID,is_sign=Is_sign, group_id=Group_id, fighterList=FighterList, itemList=ItemList, fightPower=FightPower, isMale=IsMale, title=Title, head=Head,
                        level = Level, roleName = RoleName, rank = Rank, serverID = ServerID, hpPercent=HpPercent, killNum=KillNum,
                        killContinuousNum=KillContinuousNum, maxKillContinuousNum=MaxKillContinuousNum, isInContinuous=IsInContinuous,
                        guessCoin=GuessCoin, guessType=GuessType, timestamp=Timestamp,canBeAtkTime=CanBeAtkTime,atkAdd=AtkAdd,hpAdd=HpAdd,
                        lieuViewList=LieuViewList,talentList=[],trSpecial=#trSpecial{},skin_info=#skin_info{}};
        %% 走到这个case说明是读取了旧格式的数据，需要给天赋技能填上值。该情况会发生在1.3.0升级至1.4.0时
        {role_alien,RoleID,Is_sign, Group_id, FighterList, ItemList, FightPower, IsMale, Title, Head,
         Level, RoleName, Rank, ServerID, HpPercent, KillNum,
         KillContinuousNum, MaxKillContinuousNum, IsInContinuous,
         GuessCoin, GuessType, Timestamp,CanBeAtkTime, AtkAdd, HpAdd,TalentList,LieuViewList} ->
            #role_alien{roleID=RoleID,is_sign=Is_sign, group_id=Group_id, fighterList=FighterList, itemList=ItemList, fightPower=FightPower, isMale=IsMale, title=Title, head=Head,
                        level = Level, roleName = RoleName, rank = Rank, serverID = ServerID, hpPercent=HpPercent, killNum=KillNum,
                        killContinuousNum=KillContinuousNum, maxKillContinuousNum=MaxKillContinuousNum, isInContinuous=IsInContinuous,
                        guessCoin=GuessCoin, guessType=GuessType, timestamp=Timestamp,canBeAtkTime=CanBeAtkTime,atkAdd=AtkAdd,hpAdd=HpAdd,
                        lieuViewList=LieuViewList,talentList=TalentList,trSpecial=#trSpecial{},skin_info=#skin_info{}};
        {role_alien,RoleID,Is_sign, Group_id, FighterList, ItemList, FightPower, IsMale, Title, Head,
         Level, RoleName, Rank, ServerID, HpPercent, KillNum,
         KillContinuousNum, MaxKillContinuousNum, IsInContinuous,
         GuessCoin, GuessType, Timestamp,CanBeAtkTime, AtkAdd, HpAdd,TalentList, LieuViewList,TrSpecial} ->
            #role_alien{roleID=RoleID,is_sign=Is_sign, group_id=Group_id, fighterList=FighterList, itemList=ItemList, fightPower=FightPower, isMale=IsMale, title=Title, head=Head,
                        level = Level, roleName = RoleName, rank = Rank, serverID = ServerID, hpPercent=HpPercent, killNum=KillNum,
                        killContinuousNum=KillContinuousNum, maxKillContinuousNum=MaxKillContinuousNum, isInContinuous=IsInContinuous,
                        guessCoin=GuessCoin, guessType=GuessType, timestamp=Timestamp,canBeAtkTime=CanBeAtkTime,atkAdd=AtkAdd,hpAdd=HpAdd,
                        lieuViewList=LieuViewList,talentList=TalentList,trSpecial=TrSpecial,skin_info=#skin_info{}};
        {role_alien,RoleID,Is_sign, Group_id, FighterList, ItemList, FightPower, IsMale, Title, Head,
         Level, RoleName, Rank, ServerID, HpPercent, KillNum,KillContinuousNum, MaxKillContinuousNum, IsInContinuous,
         GuessCoin, GuessType, Timestamp,CanBeAtkTime, AtkAdd, HpAdd,TalentList, LieuViewList,TrSpecial,SkinInfo} ->
            #role_alien{roleID=RoleID,is_sign=Is_sign, group_id=Group_id, fighterList=FighterList, itemList=ItemList, fightPower=FightPower, isMale=IsMale, title=Title, head=Head,
                        level = Level, roleName = RoleName, rank = Rank, serverID = ServerID, hpPercent=HpPercent, killNum=KillNum,
                        killContinuousNum=KillContinuousNum, maxKillContinuousNum=MaxKillContinuousNum, isInContinuous=IsInContinuous,
                        guessCoin=GuessCoin, guessType=GuessType, timestamp=Timestamp,canBeAtkTime=CanBeAtkTime,atkAdd=AtkAdd,hpAdd=HpAdd,
                        lieuViewList=LieuViewList,talentList=TalentList,trSpecial=TrSpecial,skin_info=#skin_info{},vip=1};
        ?undefined ->
            %%			?ERR("L-get_role_alien OTHER!!! ~w",[Res]),  %% 走到这个分支，说明从服务器认为已经建立了异星战场信息，而主服务器未建立异星战场信息。
            Res
    end.

set_role_alien(#role_alien{roleID=RoleID}=RoleAlien) ->
    erlang:put({?role_alien_info, RoleID}, RoleAlien).

set_role_rank_to_role_id(RoleRank, GroupID, RoleID) ->
    erlang:put({?rank_to_role_id, GroupID, RoleRank}, RoleID).

get_role_alien_by_rank(RoleRank, GroupID) ->
    case erlang:get({?rank_to_role_id, GroupID, RoleRank}) of
        ?undefined ->
            ?undefined;
        RoleID ->
            get_role_alien(RoleID)
    end.

get_all_rank_data() ->
    lists:foldr(fun(Key, Acc) ->
                    case Key of
                        {{?role_alien_info, _}, #role_alien{is_sign=true,group_id=GroupID}=Info} ->
                            case lists:keyfind(GroupID, 1, Acc) of
                                false ->
                                    [{GroupID,[Info]}|Acc];
                                {GroupID, List} ->
                                    lists:keyreplace(GroupID, 1, Acc, {GroupID, [Info|List]})
                            end;
						_ ->
							Acc
                    end
                end, [], erlang:get()).

get_all_guess_data() ->
    lists:foldr(fun({{?role_alien_info, _}, V2}, Acc) ->
                        case V2 of
                            #role_alien{guessCoin=GuessCoin,group_id=GroupID} when GuessCoin > 0 ->
                                case lists:keyfind(GroupID, 1, Acc) of
                                    false ->
                                        [{GroupID,[V2]}|Acc];
                                    {GroupID, List} ->
                                        lists:keyreplace(GroupID, 1, Acc, {GroupID, [V2|List]})
                                end;
                            _ ->
                                Acc
                        end;
                   (_, Acc) ->
                        Acc
                end, [], erlang:get()).

get_role_alien_dict_data() ->
    lists:foldr(fun({{?rank_to_role_id, _, _}, _}=D, Acc) ->
                        [D|Acc];
                   ({{?role_alien_info, V1}, V2}, Acc) ->
                        [{{?role_alien_info, V1}, V2#role_alien{fighterList=[], itemList=[]}}|Acc];
                   (_, Acc) ->
                        Acc
                end, [], erlang:get()).

update_fighter_list(_SlaveServerList) ->
    DictDataList = get_role_alien_dict_data(),
	send_msg:broadcast(alien_server,{update_fighter_list,DictDataList}).

get_replay_record(ReplayUID) ->
    case erlang:get({?replayRecord, ReplayUID}) of
        undefined ->
            case db_sql:get_fightReplay(ReplayUID) of
                []->
                    case ReplayUID of
                        0 ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 1};
                        _ ->
                            {#sc_fight_request{actionList=[],fighterList=[],result=true}, 2}
                    end;
                Rec->
                    erlang:put({?replayRecord, ReplayUID}, Rec),
                    {Rec, 1}
            end;
        Cached ->
            {Cached, 1}
    end.

%% 清除所有的战报缓存
erase_replay_record() ->
    lists:foreach(fun({{?replayRecord, ReplayUID},_}) ->
                          erlang:erase({?replayRecord, ReplayUID});
                     (_) ->
                          next
                  end, erlang:get()).

%% 清除指定战报缓存
erase_replay_record(ReplayUIDList) ->
    lists:foreach(fun(ReplayUID) ->
                          erlang:erase({?replayRecord, ReplayUID})
                  end, ReplayUIDList).
%% -------------------------------------------------------------------------------------------------------------------------
%% it's broadcast! 
send_msg_to_slave(SlaveServerList, Msg) ->
    lists:foreach(fun(E) -> send_msg:direct(get_server_id(E),alien_server,Msg) end, SlaveServerList).

send_msg_to_slave_sever_id(ServerIDT, Msg) ->
	send_msg:direct(ServerIDT,alien_server, Msg).

get_server_id() ->
    data_setting:get(server_id).

%%给子服发消息不能使用广播的交换机,那样的话A主服发送的消息也会
%%被B主服的子服收到.可以在每个子服上多创建一个queue，route_key
%%就设置为子服的alien_name，不过这样做性价比不高,这里的做法是
%%从alien_name里面提取出server_id
get_server_id(AtomName) ->
    NameList = erlang:atom_to_list(AtomName),
    get_server_id(lists:reverse(NameList), []).

get_server_id([], Acc) ->
    erlang:list_to_integer(Acc);

get_server_id([H|T], Acc) ->
    case H >= $0 andalso H =< $9 of
        true ->
            get_server_id(T, [H|Acc]);
        _ ->
            get_server_id([], Acc)
    end.

is_master() ->
    case erlang:get(?MASTER_FLAG) of
        undefined ->
            false;
        _ ->
            true
    end.

get_cross_server_name() ->
    ServerID = get_server_id(),
    IsCrossMaster = is_master(), 
    Platform = data_setting:get(platform),
    get_cross_server_name(ServerID, IsCrossMaster, Platform).

get_cross_server_name(ServerID, IsCrossMaster, Platform) ->
    case IsCrossMaster of
        true ->
            erlang:list_to_atom(lists:concat(['cross_master_', Platform, ServerID]));
        false ->
            erlang:list_to_atom(lists:concat(['cross_slave_', Platform, ServerID]))
    end.

%% 这个函数给alien_distribute使用
get_master_node(MasterIP, MasterID, Platform) ->
    NodeNameList = lists:concat([Platform, '_master_', MasterID, '@', MasterIP]),
    erlang:list_to_atom(NodeNameList).

%% 主服务器更新数据到子服务器
sync_status(State, IsForce) ->
    #state2{master_node=MasterNode, master_server=MasterServer,slave_node_list=_SlaveNodeList, slave_server_list=SlaveServerList,
           next_status_left_seconds=NextStatusLeftSeconds, status=Status,session_id=SessionID,
           noticed_server_list=NoticedServerList} = State,
	case NoticedServerList =:= [] orelse IsForce of	
        true ->
			%send_msg:broadcast(alien_server,{update_server_status,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID});
			send_msg_to_slave(SlaveServerList, {update_server_status,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID});
		_ ->
			ignore
	end,
    NewNextStatusLeftSeconds = calc_left_seconds( ),
    State#state2{is_all_connect=true, next_status_left_seconds=NewNextStatusLeftSeconds, noticed_server_list=SlaveServerList}.

get_next_status_msg(Status) ->
    case Status of
        ?STATUS_CLOSE ->
            do_sign;
        ?STATUS_SIGN ->
            do_fight;
        ?STATUS_FIGHT ->
            do_final;
        ?STATUS_FINAL ->
            do_close
    end.

%% 分配后, 即清除旧数据
do_update_server_list([],State) ->
    NewState = State#state2{sign_list=[], guessOddNum=0, guessEvenNum=0, killNumRankList=[],killConRankList=[],fightRecordList=[],group_num_list=[], slave_node_list=[], slave_server_list=[], noticed_server_list=[]},
    {noreply, NewState};

do_update_server_list(SlaveList,State) ->
    {SlaveNodeList,SlaveServerList} = lists:foldl(fun({SlaveNode,SlaveServer}, {AccNode,AccServer}) ->
                                                    {[SlaveNode|AccNode], [SlaveServer|AccServer]}  
                                                  end, {[],[]}, SlaveList),
    NewState = State#state2{sign_list=[], guessOddNum=0, guessEvenNum=0, killNumRankList=[],killConRankList=[],fightRecordList=[],group_num_list=[], slave_node_list=SlaveNodeList, slave_server_list=SlaveServerList, noticed_server_list=[]},
    ?ERR("更新server_list成功~n, master_node:~w,master_server:~w,slave_node_list:~w,slave_server_list:~w",
            [NewState#state2.master_node, NewState#state2.master_server, SlaveNodeList, SlaveServerList]),
    {noreply, NewState}.

do_reward(ServerID,RoleID,0,Num) ->
    case lists:keyfind(Num,1,data_alien:get(kill_con_num_reward)) of
        {Num, Reward} ->
            send_msg_to_slave_sever_id(ServerID,{send_record_reward,RoleID,Reward,0}),       
            role_reward:transform2p_mail_reward(Reward);
        false ->
            #p_mail_reward{}
end;  

do_reward(ServerID,RoleID,1,Num) ->
    case util:fun_take(fun({Require,_}) ->
                            Num >= Require end, lists:reverse(data_alien:get(kill_con_stop_reward))) of               
        {value,{_,Reward},_} ->
            send_msg_to_slave_sever_id(ServerID,{send_record_reward,RoleID,Reward,1}),       
            role_reward:transform2p_mail_reward(Reward);
        false ->
            #p_mail_reward{}
end.

get_fighter_list2(GroupID) ->
    GID = 
        case GroupID of 
            0 ->
                1;
            _ ->
                GroupID 
        end,
    lists:foldr(fun(RoleRank, Acc) ->
                      case get_role_alien_by_rank(RoleRank, GID) of
                          ?undefined ->
                              Acc;
                          RoleAlien ->
                              [RoleAlien|Acc]
                      end
              end, [], ?FIRST_FIVE_RANK_LIST).

%% 比较一个列表长度是否超过了某个值,如果没有返回这个列表的长度,否则返回这个值 
is_list_length_in(List, Value) ->
    is_list_length_in(List, Value, 0).

is_list_length_in([], _, Acc) ->
    Acc;
is_list_length_in(_, Value, Value) ->
    Value;
is_list_length_in([_H|T], Value, Acc) ->
    is_list_length_in(T, Value, Acc + 1).

transformOldLiueView2NewLiueView(LieuViewList)->
    transformOldLiueView2NewLiueView(LieuViewList,1,[]).
    
transformOldLiueView2NewLiueView([],Pos,Result)->
    Result;
transformOldLiueView2NewLiueView([H|T],Pos,Result)->
    transformOldLiueView2NewLiueView(T,Pos+1,[transformOldLiueView2NewLiueView2(H,Pos)|Result]).

transformOldLiueView2NewLiueView2(LieuView,Pos) when is_record(LieuView,p_lieu_view)->
    LieuView;
transformOldLiueView2NewLiueView2({p_lieu_view,GerTypeID},Pos)->
    #p_lieu_view{lieuGerTypeID=GerTypeID,lieuGerQuality=0,lieuPos=Pos,lieuLevel=0}.
%% ----------------------------------------------------------------------------------------------------------------------
%% 管理命令
show_current_status() ->
    erlang:send(?MODULE, show_current_status).

%% 所有报名的人随机打一个对手(仅测试用)
%% 这个操作有可能会和不在一个战场的玩家打 所以会有许多log
test_do_fight() ->
    erlang:send(?MODULE, test_do_fight).

%%为主服中保存的玩家精灵列表的属性添加上科技属性，用于统一玩家战斗计算
add_familyAttr_to_figherList() ->
    erlang:send(?MODULE,fix_alien_master_figherlist).

%%获得连杀排行榜
get_killcon_rank_list(GroupID) ->
    gen_server:call(?MODULE, {get_killcon_rank_list, GroupID}).

%%获得杀人数排行榜
get_killnum_rank_list(GroupID) ->
    gen_server:call(?MODULE, {get_killnum_rank_list, GroupID}).

%% 状态同步
force_sync_status( ) ->
    erlang:send(?MODULE, force_sync_status).
