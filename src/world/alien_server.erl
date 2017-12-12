-module(alien_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_alien.hrl").
-include("def_mail.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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

request_master_status()->
	erlang:send(?MODULE, request_master_status).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state2, {count=0,session_id=0,status=0,next_status_left_seconds,master_node, master_server, finals_node=?undefined, finals_server=?undefined}).

-define(CHECK_NODE_CONNECTION_INTERVAL, 20000).
-define(update_timestamp, update_timestamp).
-define(bc_list, bc_list).
-define(self_record_list, self_record_list).
-define(selfReplayRecord,selfReplayRecord).

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
        [{state2,State}|AllInfo] ->
            lists:foreach(fun({Key,Value}) -> erlang:put(Key, Value) end, AllInfo);
        _ ->
            State = #state2{}
    end,
    %% 合服后清除个人战报
    fix_after_merge(), 
	get_master_status_info(),
	erlang:send(self(),do_hibernate),
    {ok, State#state2{count=0}}.

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
	erlang:send_after(600000, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

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
do_handle_info(request_master_status, State)->
	get_master_status_info(),
	{noreply,State};

do_handle_info({get_roleFightInfo,Ref,RoleID},  State) ->
    #rolePublic{fightPower=FightPower,viplevel=VipLevel,svipLevel=SvipLevel} = role_lib:get_rolePublic(RoleID),
    {FighterList, LieuAdd, TalentList,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
    EquipedList = role_data:get_otherRoleItemEquips(RoleID),
    SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
    Vip = role_lib:cacl_vip_info(VipLevel,SvipLevel),
    send_msg:direct_by_name(State#state2.finals_server,alien_finals,{get_roleFightInfoBack,Ref,{FightPower, FighterList, LieuAdd, TalentList, EquipedList,TrSpecial,SkinInfo,Vip}}),
    {noreply,State};
do_handle_info({send_mail_reward, RoleID, SellReward, MailTemplateID, ArgList}, State) ->
    mail_server:send_sys_mail(RoleID, MailTemplateID, ArgList, "", SellReward),
    {noreply, State};
do_handle_info({send_champion_reward, RoleID, RoleName, GroupID, Reward, Text, Low, High}, State) ->
    %% 给战场第一名发邮件
    mail_server:send_sys_mail(RoleID, ?MAIL_ALIEN_FIELD_CHAMPION_REWARD, [], "", data_alien:get(champion_self_reward)),
    OfflineTimeMax = 4070880000, % util:datetime_to_seconds({{2099,1,1},{0,0,0}})
    OfflineTimeMin = util:now() - 388800, % 24*360*45
%%     erlang:spawn(fun() -> role_mail_gift:send_level_reward(Reward,Text,Low,High,?MAIL_ALIEN_CHAMPION_REWARD,[RoleName,GroupID]) end),
    erlang:spawn(fun() -> 
                    role_mail_gift:send_level_offtime_reward(Reward, Text, Low, High,OfflineTimeMin, OfflineTimeMax ,?MAIL_ALIEN_CHAMPION_REWARD,[RoleName,GroupID])
                 end),
    {noreply, State};
do_handle_info({send_record_reward, RoleID, Reward, Type}, State) ->
    case role_lib:is_online(RoleID) of
        true ->
            role_lib:send_server(RoleID, {do_alien_record_reward, Reward, Type});    
        _ ->
            %% TODO 需要加封邮件,给掉线的玩家领取奖励
            todo
    end,
    {noreply, State};
do_handle_info({alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold}, State) ->
    do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, State),
    {noreply, State};
do_handle_info({do_alien_fight_succ_return, RoleID, IsWin, EnemyName, FightRecord, NeedTimes, NeedGold, AddCoin,NewRank,NewFighterList,RoleKillCon,TarKillCon}, State) ->
    ?unicast(RoleID, #sc_alien_fight{result=1,fightInfo=[FightRecord],newRank=NewRank,addCoin=AddCoin,
                                     fighterList=trans2p_alien_fighterList(NewFighterList), roleconnum=RoleKillCon, tarconnum=TarKillCon}),
    catch role_lib:send_server(RoleID, {do_alien_fight_succ_return, NeedTimes, NeedGold, AddCoin}),
    save_fight_record(RoleID,IsWin, EnemyName, FightRecord,NewRank,true,State#state2.session_id),
    trigger_kill_continuous(RoleID, RoleKillCon),
    {noreply, State};
do_handle_info({be_fighted, RoleID, IsWin, EnemyName, NewRank, FightRecord, RoleKillCon}, State) ->
    save_fight_record(RoleID,IsWin, EnemyName, FightRecord,NewRank,false,State#state2.session_id),
    trigger_kill_continuous(RoleID, RoleKillCon),
    {noreply, State};
do_handle_info({do_alien_fight_error_return, RoleID, Reason, NewFighterList}, State) ->
    ?unicast(RoleID, #sc_alien_fight{result=Reason,fightInfo=[],newRank=0,addCoin=0,fighterList=[],roleconnum=0,tarconnum=0}),
    case erlang:is_list(NewFighterList) andalso NewFighterList =/= [] of
        true ->
            ?unicast(RoleID, #sc_alien_new_fighter_list{fighterList=trans2p_alien_fighterList(NewFighterList)});
        false ->
            next
    end,
    {noreply, State};
do_handle_info({get_alien_info, RoleID, AlienTimes, ResetTime}, State) ->
    do_get_alien_info(RoleID, AlienTimes, ResetTime, State),
    {noreply, State};
do_handle_info({get_alien_info_return, Msg}, State) ->
    do_get_alien_info_return(Msg),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_view_other{tarRoleID=TarRoleID}}, #state2{master_server=MasterServer}=State) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{alien_view_other, RoleID, TarRoleID, data_setting:get(server_id)}) of
        ?undefined ->
            next;
        _ ->
            ok
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_view_other_dtl{tarRoleID=TarRoleID}}, #state2{master_server=MasterServer}=State) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{alien_view_other_dtl, RoleID, TarRoleID, data_setting:get(server_id)}) of
        ?undefined ->
            next;
        _ ->
            ok
    end, 
    {noreply, State};
do_handle_info({do_alien_view_other_return, RoleID, Record}, State) ->
    ?unicast(RoleID, Record),
    {noreply, State};
do_handle_info({do_alien_view_other_dtl_return, RoleID, Record}, State) ->
    ?unicast(RoleID, Record),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_first_five{groupID=GroupID}}, State) ->
    do_alien_first_five(RoleID, GroupID, State),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_leave{}}, State) ->
    set_bc_list(lists:delete(RoleID,get_bc_list())),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_kill_continuous_rank{start=Start,num=Num}}, #state2{master_server=MasterServer}=State) ->
    case Num > 0 of
        true ->
			case send_msg:direct_by_name(MasterServer,alien_master_server,{get_alien_kill_con_rank, RoleID, Start, Num, data_setting:get(server_id)}) of
                ?undefined ->
                    next;
                _ ->
                    ok
            end;
        false ->
            next
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_kill_num_rank{start=Start,num=Num}}, #state2{master_server=MasterServer}=State) ->
    case Num > 0 of
        true ->
			case send_msg:direct_by_name(MasterServer,alien_master_server,{get_alien_kill_num_rank, RoleID, Start, Num, data_setting:get(server_id)}) of
                ?undefined ->
                    next;
                _ ->
                    ok
            end;
        false ->
            next
    end,
    {noreply, State};
do_handle_info({get_alien_kill_num_rank_return, RoleID, List}, State) ->
    case List of
        [] ->
            ?unicast(RoleID, #sc_alien_buy_times{result=20,newTimes=0});
        _ ->
            ?unicast(RoleID, #sc_alien_kill_num_rank{fighterList=transkillnumranklist(List)})
    end,
    {noreply, State};
do_handle_info({get_alien_kill_con_rank_return, RoleID, List}, State) ->
    case List of
        [] ->
            ?unicast(RoleID, #sc_alien_buy_times{result=20,newTimes=0});
        _ ->
            ?unicast(RoleID, #sc_alien_kill_continuous_rank{fighterList=transkillconranklist(List)})
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_self_fight_replay{replayUID=ReplayUID}}, State) ->
    {FightRecord, _} = get_replay_record(ReplayUID),
    ?unicast(RoleID, #sc_alien_self_fight_replay{fightInfoList=FightRecord}),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_fight_replay{replayUID=ReplayUID}}, #state2{master_server=MasterServer}=State) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{get_alien_fight_replay, RoleID, ReplayUID, data_setting:get(server_id)}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_fight_repaly{fightInfoList=#sc_fight_request{actionList=[],fighterList=[],result=true}});
        _ ->
            ok
    end,
    {noreply, State};
do_handle_info({get_alien_fight_replay_return, RoleID, FightRecord}, State) ->
    ?unicast(RoleID, #sc_alien_fight_repaly{fightInfoList=FightRecord}),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_self_record{start=StartT,num=Num}}, State) ->
    Start = StartT + 1,
    case Num > 0 of
        true ->
            List = get_self_record(RoleID),
            case Start =< erlang:length(List) of
                true ->
                    ?unicast(RoleID, #sc_alien_self_record{recordList=lists:sublist(List, Start, Num)});
                false ->
                    ?unicast(RoleID, #sc_alien_self_record{recordList=[]})
            end;
        false ->
            ?unicast(RoleID, #sc_alien_self_record{recordList=[]})
    end,
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_record{start=Start,num=Num}}, #state2{master_server=MasterServer}=State) ->
    case Num > 0 of
        true ->
			case send_msg:direct_by_name(MasterServer,alien_master_server,{get_alien_record, RoleID, Start, Num, data_setting:get(server_id)}) of
                ?undefined ->
                    ?unicast(RoleID, #sc_alien_record{recordList=[]});
                _ ->
                    ok
            end;
        false ->
            ?unicast(RoleID, #sc_alien_record{recordList=[]})
    end,
    {noreply, State};
do_handle_info({get_alien_record_return, RoleID, List}, State) ->
    ?unicast(RoleID, #sc_alien_record{recordList=List}),
    {noreply, State};
do_handle_info({role_offline, RoleID}, State) ->
    set_bc_list(lists:delete(RoleID,get_bc_list())),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_active{}}, #state2{status=Status}=State) ->
    ?unicast(RoleID, #sc_alien_active{status=Status}),
    {noreply, State};
do_handle_info({get_alien_info_return, RoleID, FighterList}, State) ->
    do_get_alien_info_return(RoleID, FighterList),
    {noreply, State};
do_handle_info({client_msg, RoleID, #cs_alien_guess_info{}}, State) ->
    do_alien_guess_info(RoleID, State),
    {noreply, State};
do_handle_info({alien_guess, RoleID, RoleInfo, GuessCoin, GuessType}, State) ->
    do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, State),
    {noreply, State};
do_handle_info({do_alien_guess_return, RoleID, 0, GuessCoin}, State) ->
    %%压注成功,触发一次成就
    role_task:send_dispach(RoleID, {dispach_task,role_bet_alien,1}),
    ?unicast(RoleID, #sc_alien_guess{result=0}),
    role_lib:send_server(RoleID, {do_alien_guess_succ_return, GuessCoin}),
    {noreply, State};
do_handle_info({do_alien_guess_return, RoleID, Reason}, State) ->
    ?unicast(RoleID, #sc_alien_guess{result=Reason}),
    {noreply, State};
do_handle_info({get_alien_guess_info_return, RoleID, Type, Coin, GuessOddNum, GuessEvenNum}, State) ->
    ?unicast(RoleID, #sc_alien_guess_info{guessCoin=Coin,guessType=Type,guessOddNum=GuessOddNum,guessEvenNum=GuessEvenNum,
                                          coinValList=data_alien:get(coin_val_list_odd)++data_alien:get(coin_val_list_even)}),
    {noreply, State};
do_handle_info({alien_sign, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip}, State) ->
    do_alien_sign(RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip, State),
    {noreply, State};
do_handle_info({do_alien_sign_return, RoleID, Result}, State) ->
	case Result of 
		0 ->
            %%报名成功,触发一次报名成就
			 role_task:send_dispach(RoleID, {dispach_task,role_sign_alien,1});
		_ ->
			void
    end,	
    ?unicast(RoleID, #sc_alien_sign{result=Result}),
    {noreply, State};
do_handle_info({alien_reset, RoleInfo, FighterList,ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip}, State) ->
    do_alien_reset(RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip,State),
    {noreply, State};
do_handle_info({do_alien_reset_return, RoleID, Reason}, State) ->
    ?unicast(RoleID, #sc_alien_reset{result=Reason,timestamp=0}),
    {noreply, State};
do_handle_info({do_alien_reset_return, RoleID, 0, NewFighterList}, State) ->
    ResetTime = util:now()+data_alien:get(reset_seconds),
    ?unicast(RoleID, #sc_alien_reset{result=0,fighterList=trans2p_alien_fighterList(NewFighterList),
                                     timestamp=ResetTime}),
    role_lib:send_server(RoleID, {do_alien_reset_return_succ, ResetTime}),
    {noreply, State};
do_handle_info(do_sign, State) ->
    ?ERR("开始报名", []),
    broadcast_server:bc(#sc_alien_active{status=?STATUS_SIGN}),
    BcList = get_bc_list(),
    erlang:erase(),
    do_delete_alien_replay(erlang:localtime()),
    set_bc_list(BcList),
    send_bc_list_msg({route, role_alien, #cs_alien_info{}}),
    {noreply, State};
do_handle_info(do_fight, State) ->
    ?ERR("开始比赛", []),
    broadcast_server:bc(#sc_alien_active{status=?STATUS_FIGHT}),
    send_bc_list_msg({route, role_alien, #cs_alien_info{}}),
    {noreply, State};
do_handle_info(do_final, State) ->
    ?ERR("进行总决赛", []),
    broadcast_server:bc(#sc_alien_active{status=?STATUS_FINAL}),
    send_bc_list_msg({route, role_alien, #cs_alien_info{}}),
    {noreply, State};
do_handle_info(do_close, State) ->
    ?ERR("关闭跨服战", []),
    broadcast_server:bc(#sc_alien_active{status=?STATUS_CLOSE}),
    send_bc_list_msg({route, role_alien, #cs_alien_info{}}),
    {noreply, State};
do_handle_info({update_server_status,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID}, State) ->
    do_update_server_status(State,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID);
do_handle_info(dump_data, #state2{count=Count}=State) ->
    do_persist(State),
    case Count rem 6 of
        0 ->
            erase_replay_record();
        _ ->
            next
    end,
    {noreply, State#state2{count=Count+1}};
do_handle_info({update_role_name, RoleID, Name}, #state2{master_server=MasterServer}=State) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{update_role_name, RoleID, Name}) of
        undefined ->
            ignore;
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info({get_self_alien_rank, RoleID}, #state2{master_server=MasterServer}=State) ->
    case send_msg:direct_by_name(MasterServer, alien_master_server, {get_self_alien_rank, RoleID, data_setting:get(server_id)}) of 
        undefined ->
            ?unicast(RoleID, #sc_alien_self_rank{isSign=false, continuousRank=0, killNumRank=0});
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info({get_self_alien_rank_return, RoleID, IsSign, KillNumRank, ConNumRank}, State) ->
    ?unicast(RoleID, #sc_alien_self_rank{isSign=IsSign,continuousRank=ConNumRank, killNumRank=KillNumRank}),
    {noreply, State};
    
do_handle_info({do_delete_alien_replay,Time},State)->
	do_delete_alien_replay(Time),
	{noreply,State};

%% ----------------------------------------------------------------------------------------------------------------------
%% 这里时异星总决赛相关代码ANCHOR:FINALS
%% 客户端的数据
do_handle_info({client_msg, RoleID, #cs_alien_finals_info{}} = Msg, #state2{finals_server=FinalsServer}=State) ->
	case send_msg:direct_by_name(FinalsServer,alien_finals,{data_setting:get(server_id), Msg}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_finals_info{status=0,round=0,endTimestamp=0});
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info({client_msg, RoleID, #cs_alien_finals_fight_replay{}} = Msg, #state2{finals_server=FinalsServer}=State) ->
	case send_msg:direct_by_name(FinalsServer,alien_finals,{data_setting:get(server_id), Msg}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_finals_fight_replay{fightInfoList=#sc_fight_request{actionList=[],fighterList=[],result=true}});
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info({client_msg, RoleID, #cs_alien_finals_guess{}} = Msg, #state2{finals_server=FinalsServer}=State) ->
	case send_msg:direct_by_name(FinalsServer,alien_finals,{data_setting:get(server_id), Msg}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_finals_guess{result=2});
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info({client_msg, RoleID, #cs_alien_finals_list{type=Type, groupID=GroupID}} = Msg, #state2{finals_server=FinalsServer}=State) ->
	case send_msg:direct_by_name(FinalsServer,alien_finals,{data_setting:get(server_id), Msg}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_finals_list{type=Type,groupID=GroupID,list=[]});
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info({client_msg, RoleID, #cs_alien_finals_records{type=Type, groupID=GroupID}} = Msg, #state2{finals_server=FinalsServer}=State) ->
	case send_msg:direct_by_name(FinalsServer,alien_finals,{data_setting:get(server_id), Msg}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_finals_records{type=Type,groupID=GroupID,rounds=[]});
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info({client_msg, RoleID, #cs_alien_finals_self_info{}} = Msg, #state2{finals_server=FinalsServer}=State) ->
	case send_msg:direct_by_name(FinalsServer,alien_finals,{data_setting:get(server_id), Msg}) of
        ?undefined ->
            %% 在断开连接时,返回数据时默认显示玩家已经押注了,防止此时玩家押注导致声望丢失 
            ?unicast(RoleID, #sc_alien_finals_self_info{guessRank=1,guessId=0,groupId=0});
        _ ->
            ok
    end,
    {noreply, State};

%% 决赛服的数据
do_handle_info({finals_update_status, Status, CurGame, Timestamp}, State) ->
    ?ERR("更新决赛服状态:~p,~p,~p", [Status, CurGame, Timestamp]),
    broadcast_server:bc(#sc_alien_finals_info{status=Status, round=CurGame, endTimestamp=Timestamp}),
    {noreply, State};

do_handle_info({finals_update_node_info, FinalsNode, FinalsServer}, State) ->
    ?ERR("更新决赛服节点:~p,Server:~p.~n", [FinalsNode, FinalsServer]),
    {noreply, State#state2{finals_node=FinalsNode, finals_server=FinalsServer}};

%% 决赛服返回的客户端请求
do_handle_info({finals_finals_info, RoleID, Status, LeftGame, NextTimeStamp}, State) ->
    ?unicast(RoleID, #sc_alien_finals_info{status=Status,round=LeftGame,endTimestamp=NextTimeStamp}),
    {noreply, State};

do_handle_info({finals_fight_replay, RoleID, Record}, State) ->
    ?unicast(RoleID, #sc_alien_finals_fight_replay{fightInfoList=Record}),
    {noreply, State};

do_handle_info({finals_guess_return, false, RoleID, Result, Rank}, State) ->
    %% 发送失败消息给玩家, 然后添加回扣除的徽章
    role_lib:send_server(RoleID, {alien_finals_guess_failed, Result, Rank}),
    {noreply, State};

do_handle_info({finals_guess_return, true, RoleID}, State) ->
    ?unicast(RoleID, #sc_alien_finals_guess{result=0}),
    {noreply, State};

do_handle_info({finals_finals_list, RoleID, Type, GroupID, List}, State) ->
    ?unicast(RoleID, #sc_alien_finals_list{type=Type,groupID=GroupID,list=List}),
    {noreply, State};

do_handle_info({finals_finals_records, RoleID, Type, GroupID, List}, State) ->
    ?unicast(RoleID, #sc_alien_finals_records{type=Type,groupID=GroupID,rounds=List}),
    {noreply, State};

do_handle_info({finals_match_reward, RoleID, Status}, State) ->
    Reward = data_alien_finals:get({match_reward, Status}),
    MailTemplateID = case Status of
                        ?STATUS_FINALS_GROUP_MATCH ->
                            ?MAIL_ALIEN_FINALS_GROUP_REWARD; 
                        ?STATUS_FINALS_QUARTERFINAL ->
                            ?MAIL_ALIEN_FINALS_QUARTER_REWARD;
                        ?STATUS_FINALS_SEMIFINALS ->
                            ?MAIL_ALIEN_FINALS_SEMIFINAL_REWARD;
                        ?STATUS_FINALS_FINAL ->
                            %% 冠军有两封邮件
                            mail_server:send_sys_mail(RoleID, ?MAIL_ALIEN_FINALS_CHAMPION_REWARD2, [], "", data_alien_finals:get(champion_reward)),
                            ?MAIL_ALIEN_FINALS_CHAMPION_REWARD
                        end,
    mail_server:send_sys_mail(RoleID, MailTemplateID, [], "", Reward),
    {noreply, State};    

do_handle_info({finals_champion_welfare, ServerID, ChampionName, ChampionId}, State) ->
    Reward = data_alien_finals:get(champion_welfare),
    erlang:spawn(fun() -> role_mail_gift:send_everyone_reward_except(Reward,"",?MAIL_ALIEN_FINALS_WELFARE_REWARD,[ServerID,ChampionName],[ChampionId]) end),
    {noreply, State};

%% 发送奖励, 新开线程
do_handle_info({finals_guess_reward, {HitList, MissList}}, State) ->
    send_guess_reward(HitList, true),
    send_guess_reward(MissList, false),
    {noreply, State};

do_handle_info({finals_self_info, RoleID, GuessID, Rank, GroupID}, State) ->
    ?unicast(RoleID, #sc_alien_finals_self_info{guessRank=Rank,guessId=GuessID,groupId=GroupID}),
    {noreply, State};

do_handle_info({alien_server_master_server_list,List,Final},State) ->
	lists:foreach(fun(E)-> send_msg:direct_by_name(E,alien_master_server,{alien_server_get_alien_master_status,data_setting:get(server_id)}) end,List),
	{noreply,State#state2{finals_server=Final}};

do_handle_info({update_status, SessionID, Status, LeftSeconds,MasterServerName}, State) ->
    NewState = State#state2{session_id=SessionID, status=Status, next_status_left_seconds=LeftSeconds,
                            master_server=MasterServerName},
    {noreply, NewState};

do_handle_info(Info, State) ->
    ?ERR("unkown info:~w", [Info]),
    {noreply, State}.

%% ----------------------------------------------------------------------------------------------------------------------
do_persist(State) ->
    Info = [E || E<-erlang:get(), is_persist(E) ],
    Info2 = [{state2,State} | Info],
    db_sql:set_etc(?DB_ETC_KEY_ALIEN,Info2).

is_persist({{?self_record_list, _}, _}) ->
    true;
is_persist(_) ->
    false.

%% ------------------------------------------------------------------------------------------------------------------------
do_alien_fight(RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, #state2{master_server=MasterServer}) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{do_alien_fight, RoleID, TarRoleID, TarTank, NeedTimes, NeedGold, data_setting:get(server_id)}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_fight{result=11,fightInfo=[],newRank=0,addCoin=0,fighterList=[],roleconnum=0,tarconnum=0});
        _ ->
            ok
    end.

do_alien_sign(#role{roleID=RoleID}=RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip, #state2{master_server=MasterServer}) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{do_alien_sign, RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip, data_setting:get(server_id)}) of
        ?undefined ->
           ?unicast(RoleID, #sc_alien_sign{result=3});
        _ ->
            ok
    end.

do_alien_reset(#role{roleID=RoleID}=RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList, TalentList,TrSpecial,SkinInfo,Vip, #state2{master_server=MasterServer}) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{do_alien_reset, RoleID, RoleInfo, FighterList, ItemList,RoleLieuAdd,RoleLieuAddList,TalentList,TrSpecial,SkinInfo,Vip, data_setting:get(server_id)}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_reset{result=2,timestamp=0});
        _ ->
            ok
    end.

do_alien_guess(RoleID, RoleInfo, GuessCoin, GuessType, #state2{master_server=MasterServer}) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{do_alien_guess, RoleID, RoleInfo, GuessCoin, GuessType, data_setting:get(server_id)}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_guess{result=3});
        _ ->
            ok
    end.

do_alien_guess_info(RoleID, #state2{master_server=MasterServer}) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{get_alien_guess_info, RoleID, data_setting:get(server_id)}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_guess_info{guessCoin=0,guessType=false,guessOddNum=0,guessEvenNum=0,
                                                  coinValList=[]});
        _ ->
            ok
    end.   

do_alien_first_five(RoleID, GroupID, #state2{master_server=MasterServer}) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{get_alien_first_five, RoleID, GroupID, data_setting:get(server_id)}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_first_five{fighterList=[]});
        _ ->
            ok
    end.

do_get_alien_info_return(RoleID, FighterList) ->
    ?unicast(RoleID, #sc_alien_first_five{fighterList=trans2p_alien_fighterList(FighterList)}).

do_get_alien_info(RoleID, AlienTimes,ResetTime,
                  #state2{master_server=MasterServer}) ->
	case send_msg:direct_by_name(MasterServer,alien_master_server,{get_alien_info, RoleID, AlienTimes, ResetTime, data_setting:get(server_id)}) of
        ?undefined ->
            ?unicast(RoleID, #sc_alien_info{status=?STATUS_CLOSE,endTimestamp=0,groupID=0,isSign=false,leftTimes=0,fighterList=[],
                                            resetTime=0,resetNeedGold=data_alien:get(reset_gold),
                                            maxTimes=data_common:get(max_alien_times),
                                            price=data_alien:get(price),groupNum=0});
        _ ->
            ok
    end.

do_get_alien_info_return(Msg) ->
    case Msg of
        {?STATUS_FIGHT, IsSign, GroupID, FighterList, RoleID, EndTimestamp, AlienTimes, ResetTime,GroupNum} ->
            ?unicast(RoleID, #sc_alien_info{status=?STATUS_FIGHT,endTimestamp=EndTimestamp,groupID=GroupID,isSign=IsSign,
                                            leftTimes=AlienTimes,fighterList=trans2p_alien_fighterList(FighterList),
                                            resetTime=ResetTime,resetNeedGold=data_alien:get(reset_gold),
                                            maxTimes=data_common:get(max_alien_times),
                                            price=data_alien:get(price),groupNum=GroupNum});
        {?STATUS_CLOSE, GroupID, FighterList, RoleID, EndTimestamp, AlienTimes,GroupNum} ->
            ?unicast(RoleID, #sc_alien_info{status=?STATUS_CLOSE,endTimestamp=EndTimestamp,groupID=GroupID,isSign=false,
                                            leftTimes=AlienTimes,fighterList=trans2p_alien_fighterList(FighterList),
                                            resetTime=0,resetNeedGold=data_alien:get(reset_gold),
                                            maxTimes=data_common:get(max_alien_times),
                                            price=data_alien:get(price),groupNum=GroupNum});
        {?STATUS_SIGN, IsSign, RoleID, EndTimestamp} ->
            NeedVipLevel = data_alien:get(need_vip_level),
            NeedRoleLevel = data_alien:get(need_role_level),
            ?unicast(RoleID, #sc_alien_sign_info{needVipLevel=NeedVipLevel,needLevel=NeedRoleLevel,
                                                 isSign=IsSign,signEndTimestamp=EndTimestamp});
        {?STATUS_FINAL, GroupID, FighterList, RoleID, EndTimestamp, AlienTimes,GroupNum} ->
            ?unicast(RoleID, #sc_alien_info{status=?STATUS_FINAL,endTimestamp=EndTimestamp,groupID=GroupID,isSign=false,
                                            leftTimes=AlienTimes,fighterList=trans2p_alien_fighterList(FighterList),
                                            resetTime=0,resetNeedGold=data_alien:get(reset_gold),
                                            maxTimes=data_common:get(max_alien_times),
                                            price=data_alien:get(price),groupNum=GroupNum})
    end,
    BcList = get_bc_list(),
    set_bc_list([RoleID|lists:delete(RoleID,BcList)]).

trans2p_alien_fighterList(List) ->
    lists:map(fun(#role_alien{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,
                              level=Level,roleName=RoleName,rank=Rank,hpPercent=HPPercent,vip=Vip,
                              canBeAtkTime=CanBeAtkTime,killContinuousNum=KillContinuousNum}) ->
               #p_alien_fighter{roleID=RoleID,fightPower=FightPower,isMale=IsMale,title=Title,head=Head,
                                level=Level,roleName=RoleName,rank=Rank,serverID=util:calc_server_id(roleID, RoleID),hpPercent=HPPercent,
                                canBeAtkTime=CanBeAtkTime,killContinuousNum=KillContinuousNum,vip=Vip}       
              end, List).

do_update_server_status(State,MasterNode,MasterServer,NextStatusLeftSeconds,Status,SessionID) ->
    NewState = State#state2{session_id=SessionID,status=Status, next_status_left_seconds=NextStatusLeftSeconds,
                           master_node=MasterNode, master_server=MasterServer},
    ?ERR("更新server_status成功~n master_node:~w,master_server:~w",
         [NewState#state2.master_node, NewState#state2.master_server]),
    {noreply, NewState}.

save_fight_record(RoleID,IsWin, EnemyName, FightRecord,NewRank,IsAtk,_Session_id) ->
    ReplayUID = tk_id:gen_replayUID(),
    erlang:spawn(fun() -> db_sql:set_fightReplay(ReplayUID, FightRecord, ?REPLAY_TYPE_ALIEN) end),
    NewSelfRecord =
        #p_alien_self_record{
                             isAtk=IsAtk
                             ,isWin=IsWin
                             ,enemyName=EnemyName
                             ,newRank=NewRank
                             ,replayUID=ReplayUID
                             ,timestamp=util:now()},
    SelfRecordList = get_self_record(RoleID),
    NewSelfRecordList = [NewSelfRecord|SelfRecordList],
    
    case erlang:length(NewSelfRecordList) =< data_alien:get(record_self_max_num) of
        true ->
            NewSelfRecordList2 = NewSelfRecordList;
        false ->
            #p_alien_self_record{replayUID=LastReplayUID} = LastSelfRecord = lists:last(NewSelfRecordList),
            erlang:spawn(fun() -> db_sql:del_fightReplay(LastReplayUID) end),
            NewSelfRecordList2 = lists:delete(LastSelfRecord, NewSelfRecordList)
    end,
    set_self_record(RoleID, NewSelfRecordList2),
    ?unicast(RoleID, #sc_alien_new_self_record{}).

get_self_record(RoleID) ->
    case erlang:get({?self_record_list, RoleID}) of
        List when erlang:is_list(List) ->
            List;
        _ ->
            []
    end.

set_self_record(RoleID, NewList) when erlang:is_list(NewList) ->
    erlang:put({?self_record_list, RoleID}, NewList).

get_bc_list() ->
    case erlang:get(?bc_list) of
        ?undefined ->
            [];
        List ->
            List
    end.

set_bc_list(List) when erlang:is_list(List) ->
    erlang:put(?bc_list, List).

send_bc_list_msg(Msg) ->
    BcList = get_bc_list(),
    erlang:spawn(
      fun() ->
              lists:foreach(
                fun(RoleID) ->
                        catch role_lib:send_server(RoleID, Msg)
                end, BcList)
      end).

transkillnumranklist(List) ->
    lists:map(fun(#alien_fighter2{roleID=RoleID ,rank=Rank,vip=Vip,
                                  fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,
                                  killNum=NewRoleKillNum}) ->
                      #p_alien_fighter2{roleID=RoleID ,rank=Rank,vip=Vip,
                                        fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=util:calc_server_id(roleID, RoleID),
                                        killNum=NewRoleKillNum} 
              end, List).

transkillconranklist(List) ->
    lists:map(fun(#alien_fighter3{roleID=RoleID,rank=Rank,isInContinuous=IsInContinuous,vip=Vip,
                                  fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,
                                  killContinuousNum=NewRoleKillContinuousNum}) ->
                      #p_alien_fighter3{roleID=RoleID,rank=Rank,isInContinuous=IsInContinuous,vip=Vip,
                                        fightPower=FightPower,isMale=IsMale,title=Title,head=Head,level=Level,roleName=RoleName,serverID=util:calc_server_id(roleID, RoleID),
                                        killContinuousNum=NewRoleKillContinuousNum}
              end, List).

get_replay_record(ReplayUID)->
    case erlang:get({?selfReplayRecord, ReplayUID}) of
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
                    erlang:put({?selfReplayRecord, ReplayUID}, Rec),
                    {Rec, 1}
            end;
        Cached ->
            {Cached, 1}
    end.

%% 清除所有的战报缓存
erase_replay_record() ->
    lists:foreach(fun({{?selfReplayRecord, ReplayUID},_}) ->
                          erlang:erase({?selfReplayRecord, ReplayUID});
                     (_) ->
                          next
                  end, erlang:get()).

%% 清除指定战报缓存
erase_replay_record(ReplayUIDList) ->
    lists:foreach(fun(ReplayUID) ->
                          erlang:erase({?selfReplayRecord, ReplayUID})
                  end, ReplayUIDList).

send_guess_reward(List, IsHit) ->
    spawn(fun() -> send_guess_reward2(List, IsHit) end).

send_guess_reward2(List, IsHit) ->
    HitRatio = data_alien_finals:get(stakes_hit_ratio),
    MissRatio = data_alien_finals:get(stakes_miss_ratio),
    StakesList = data_alien_finals:get(match_stakes),
    lists:map(fun({RoleID, SrcServerID, GuessName, Rank}) ->
                {_, BaseValue} = lists:nth(Rank, StakesList),
                case IsHit of
                    true ->
                        FinalValue = BaseValue * HitRatio,
                        MailTemplateID = ?MAIL_ALIEN_FINALS_GAMBLE_HIT_REWARD,
                        ArgList = [SrcServerID, GuessName, FinalValue];
                    false ->
                        FinalValue = erlang:trunc(BaseValue*MissRatio),
                        MailTemplateID = ?MAIL_ALIEN_FINALS_GAMBLE_MISS_REWARD,
                        ArgList = [SrcServerID, GuessName, BaseValue, FinalValue]
                end,
                mail_server:send_sys_mail(RoleID, MailTemplateID, ArgList, "", #sell_reward{coin=0,roleExp=0,gerExp=0,gold=0,item=[],reputation=FinalValue,newGer=[]}) 
        end, List).

trigger_kill_continuous(RoleID, RoleKillContinuousNum) ->
    %%触发完成异星战场成就
    case role_lib:is_online(RoleID) of
        true->
            role_task:send_dispach(RoleID, {dispach_task,role_alien_figth_res,RoleKillContinuousNum});
        false->
            role_task_trigger:offline_alien_fight_res(RoleID, RoleKillContinuousNum)
    end.
%% ----------------------------------------------------------------------------------------------------------------------
test_sign_niubi(N) when erlang:is_integer(N) andalso N > 0 ->
    lists:foreach(fun(RoleID) ->
                          test_sign(RoleID)
                  end, get_niubi(N)).

get_niubi(N) ->
    Sql = io_lib:format("select roleID from gRole order by fightPower desc limit ~w;", [N]),
    case db_sql:get_all(Sql) of
        [_|_]=List ->
            [E||[E]<-List];
        _ ->
            []
    end.

test_sign(RoleID) ->
    RoleInfo = role_data:get_otherRoleInfo(RoleID),
    {FighterList, LieuAdd, TalentList,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
    ItemList = role_data:get_otherRoleItemEquips(RoleID),
    SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
    erlang:send(?MODULE, {alien_sign, RoleInfo, FighterList, ItemList,LieuAdd,[],TalentList,TrSpecial,SkinInfo,1}).

test_sign_others(Max) when Max >= 1, Max =< 9999 ->
    lists:foreach(fun(RoleID) ->
                          test_sign(RoleID)   
                  end, lists:seq(?ROLE_ID_BASE + 1, ?ROLE_ID_BASE + Max)).

do_delete_alien_replay(Time)->
	{_,{H,_,_}} = erlang:localtime(),
	case H >= 2 andalso H =< 9 of
		true ->
			?ERR("do_delete_alien_replay"),
			erlang:spawn(fun() -> db_sql:del_spec_type_replay_before_time(?REPLAY_TYPE_ALIEN,Time) end);
		_ ->
			erlang:send_after(6 * 3600 * 1000, self(), {do_delete_alien_replay,Time})
	end.

fix_after_merge( ) ->
    case file:read_file_info("./merge.touch") of
        {ok, _} ->
            lists:foreach(fun(Data) ->
                            case Data of
                                {{?self_record_list, _} = Key, _} ->
                                    erlang:erase(Key);
                                _ ->
                                    ignore
                            end
                        end, erlang:get());
        _ ->
            ignore
    end.

get_master_status_info()->
	send_msg:direct_by_name(alien_distribute,alien_distribute,{alien_server_get_master_server_list,data_setting:get(server_id)}).
