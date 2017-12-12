%% 消息发送
-module(anubis_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-compile(export_all).
-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-record(state, {current_season_id=0}).

-define(BELOW_RANK,-1).  %%公会未上榜
-define(NORESPONSE_SEASON,0).%%没有对应赛季信息
-define(DELAY_REQUEST_SEASONINFO,30).

%%进程启动之后，请求同步赛季数据成功标志
-define(syn_flage,syn_flage).
-define(SYN_SUCCESS,1).
-define(SYN_FAILED,0).
-define(MAX_SYN_TIMES,50).
%%50次以前，5秒一次同步，50次以后5分钟一次
-define(SynInterval1,5).
-define(SynInterval2,300).
-define(update_family_rank_pid,update_family_rank_pid).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start( ) ->
    case data_setting:get(server_type) of
        normal->
            {ok,_} = 
            supervisor:start_child(anubis_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]});
        _ ->
            ignore
    end.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    case data_setting:get(server_type) of
        carlos_match ->
            ?ERR("not carlos match server"),
            ignore;
        _->
            process_flag(trap_exit,true),
            %%由于在请求赛季信息之后，会直接向在线的公会广播信息，而广播的过程使用了family_manager_server初始化的数据，故延迟一段时间，以便family_manager_server初始化完成
            erlang:send_after(?DELAY_REQUEST_SEASONINFO*1000,self(),request_season_info),
            {ok, #state{}}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    do_persist(State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_info({get_exist_season_id_return,SeasonIDList,CSeasonID},State)->
    set_syn_flage({?SYN_SUCCESS,0}),
    % ?ERR("SeasonIDList:~w CSeasonID:~w ~n",[SeasonIDList,CSeasonID]),
    {OldSeasonID,OldExistSeasonIDList} = get_anubis_seasonid(),
    case OldSeasonID=:= CSeasonID andalso OldExistSeasonIDList=:=SeasonIDList of
        true->
            ignore;
        false-> 
            ServerID = data_setting:get(server_id),
            set_anubis_seasonid(SeasonIDList,CSeasonID),
            %%删除多余的赛季数据
            DeleteSeasonIDList = lists:filter(fun(SeasonID)-> not lists:member(SeasonID,SeasonIDList) end,OldExistSeasonIDList),
            do_delete_seasoninfo(DeleteSeasonIDList),
            %%请求不存在的赛季数据
            NeedSynSeasonIDList = lists:filter(fun(SeasonID)-> not lists:member(SeasonID,OldExistSeasonIDList) end,SeasonIDList),
            [anubis_rank_server:request_syn_seasoninfo(SeasonID,ServerID)||SeasonID<-NeedSynSeasonIDList]
    end,
    {noreply,State#state{current_season_id=CSeasonID}};

%%处理anubis_rank_server传递过来的赛季更新信息
do_handle_info({syn_anubis_rank,SeasonID,_FamilyList,_Force,IsOK}=Msg,#state{current_season_id=CSeasonID}=State)->
    case IsOK of
        false->
            ?ERR("request not exist seasonID:~w ~n",[SeasonID]);
        true->
            deal_syn_anubis_rank_msg(Msg,CSeasonID)
    end,
    {noreply,State};

%%转发战斗结果数据到公会服
do_handle_info({update_family_anubis_info_after_fight,_CSeasonID,CSeasonFamilyPlayer,_FightMsg}=Msg,State)->
    #family_player{familyid=FamilyID} = CSeasonFamilyPlayer,
    family_misc:router_to_family_process(FamilyID, Msg),
    {noreply,State};

%%向anubis_rank_server请求赛季信息（自己向自己发送的信息）
do_handle_info(request_season_info,State)->
    case get_syn_flage() of
        {?SYN_SUCCESS,_}->
            ignore;
        {?SYN_FAILED,MaxTime}->
            init_anubis_rank(),
            set_syn_flage({?SYN_FAILED,MaxTime-1}),
            Interval = case MaxTime > 0 of true->?SynInterval1;false->?SynInterval2 end,
            erlang:send_after(Interval*1000,self(),request_season_info)
    end,
    {noreply,State};

do_handle_info({delete_season_rank,DeleteSeasonIDList},State)->
    do_delete_seasoninfo(DeleteSeasonIDList),
    {noreply,State};

%%处理anubis_rank_server发送来的赛季切换消息
do_handle_info({update_current_seasonid,SeasonID},State)->
    NewState = do_update_current_seasonid(SeasonID,State),
    {noreply,NewState};

%%处理赛季奖励的消息
do_handle_info({send_season_reward,FamilyRewardList,SeasonID},State)->
    do_send_season_reward(FamilyRewardList,SeasonID),
    {noreply,State};

do_handle_info(check_state,State)->
    % ?ERR("receive broadcast_check_state~n"),
    ServerID = data_setting:get(server_id),
    anubis_rank_server:get_exist_season_id_list(ServerID),
    {noreply,State};

do_handle_info({subseason_info,SeasonID},State)->
    % ?ERR("receive subseason_info seasonID:~w ~n",[SeasonID]),
    do_subseason_info(SeasonID),
    {noreply,State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
    {noreply, State}.

set_anubis_seasonid(SeasonIDList,CurrentSeasonID)->
    ets:insert(?ETS_ANUBIS_RANK,{exist_seaonid_list,CurrentSeasonID,lists:sort(SeasonIDList)}).
get_anubis_seasonid()->
    case ets:lookup(?ETS_ANUBIS_RANK,exist_seaonid_list) of
        [{_,CurrentSeasonID,ExistSeasonIDList}]->
            {CurrentSeasonID,ExistSeasonIDList};
        _->
            {0,[]}
    end.
%%获取对应赛季的排行榜
get_anubis_rank(SeasonID)  ->
    case ets:lookup(?ETS_ANUBIS_RANK,{anubis_season_rank,SeasonID}) of
        [{_,_UpdateTime,List}]->
            List;
        _ ->
            []
    end.

%%返回对应赛季排行榜更新时间
get_anubis_season_updatetime(SeasonID)->
    case ets:lookup(?ETS_ANUBIS_RANK,{anubis_season_rank,SeasonID}) of
        [{_,UpdateTime,_List}]->
            UpdateTime;
        _ ->
            ?undefined
    end.

%%从某个赛季数据列表中获取对应公会的信息
get_family_player_from_rank(SeasonID,FamilyID)->
    case get_anubis_rank(SeasonID) of
        []->
            ?undefined;
        L->
            case lists:keyfind(FamilyID,#family_player.familyid,L) of
                false->
                    ?undefined;
                X->
                    X
            end
    end.

%%设置对应赛季的排行榜
set_anubis_rank(SeasonID,AnubisRank)->
    UpdateTime = util:now(),
    ets:insert(?ETS_ANUBIS_RANK,{{anubis_season_rank,SeasonID},UpdateTime,AnubisRank}).


%%首先请求存在的的赛季编号，然后依次请求赛季数据
init_anubis_rank()->
    % ?INFO("request anubis_rank_server exist season~n"),
    ServerID = data_setting:get(server_id),
    anubis_rank_server:get_exist_season_id_list(ServerID).

%%处理阿努比斯排行榜传递过来的同步数据
deal_syn_anubis_rank_msg({syn_anubis_rank,SeasonID,FamilyList,Force,_IsOk},CurrentSeasonID)->
    Length = rank_length(SeasonID,CurrentSeasonID),
    case Force of
        true->
            % NeedPersitFamilyList = filter_need_persist_family(FamilyList),
            FamilyList1 = add_family_player_rank(FamilyList),
            update_family_rank(SeasonID,FamilyList1),
            PersistRankList = subfamilylist(FamilyList1,Length);
        false->
            % NeedPersitFamilyList = filter_need_persist_family(FamilyList),
            OldFamilyList = get_anubis_rank(SeasonID),
            FamilyList1 = merge_family_list(OldFamilyList,FamilyList),
            update_family_rank(SeasonID,FamilyList1),
            PersistRankList = subfamilylist(FamilyList1,Length)
    end,
    set_anubis_rank(SeasonID,PersistRankList).
    % %%广播更新赛季排名的信息到所有在线的公会服，驱动公会服更新对应的赛季排名
    % Msg = {update_anubis_rank,SeasonID},
    % family_manager_server:broadcast_to_all_family(Msg).
    % % [set_family_anubis_rank(FamilyID,FamilyAnubisRank)||#family_player{familyid=FamilyID}=FamilyAnubisRank<-NeedPersitFamilyList].

filter_need_persist_family(FamilyList)->
    lists:filter(fun(E)->
        #family_player{familyid=FamilyID}=E,
            is_familyid_exist_on_server(FamilyID)
    end,FamilyList).

subfamilylist(FamilyList,Length)->
    lists:sublist(FamilyList,Length).

rank_length(SeasonID,CSeasonID)->
    case SeasonID=:=CSeasonID of
        true->
            data_anubis:get(max_store_rank_num);
        false->
            data_anubis:get(max_history_store_rank_num)
    end.

is_familyid_exist_on_server(FamilyID)->
    AllServerID = [data_setting:get(server_id)|data_setting:get(merge_server_id_list)],
    ServerID = (FamilyID div 1000000)-1,
    lists:member(ServerID,AllServerID).

merge_family_list(OldFamilyList,AddFamilyList)->
    MergeList = lists:foldl(fun(AF,Acc)->
        case lists:keytake(AF#family_player.familyid,#family_player.familyid,Acc) of
            false->
                [AF|Acc];
            {_Value,_Find,Other}->
                [AF|Other]
        end
    end,OldFamilyList,AddFamilyList),
    SortMergList = lists:sort(fun(A,B)->anubis_rank_server:compare_family_player(A,B) end,MergeList),
    add_family_player_rank(SortMergList).

add_family_player_rank(SortMergList)->
    {SortMergListWithRank,_Index}=lists:foldl(fun(E,{Acc,IndexAcc})->{[E#family_player{familyrank=IndexAcc}|Acc],IndexAcc+1} end,{[],1},SortMergList),
    lists:reverse(SortMergListWithRank).

get_family_anubis_rank_for_season(SeasonID,FamilyID)->
    case get_anubis_rank(SeasonID) of
        []->
            ?NORESPONSE_SEASON;
        X ->
            case lists:keyfind(FamilyID,#family_player.familyid,X) of
                false->
                    ?BELOW_RANK;
                #family_player{familyrank=Rank}->
                    Rank
            end
    end.

do_persist(_State)->
    ok.

%%删除对应的赛季信息
do_delete_seasoninfo(DeleteSeasonIDList)->
    % ?ERR("delete season :~w ~n",[DeleteSeasonIDList]),
    %%删除对应的赛季排名
    lists:foreach(fun(SeasonID)->
        ets:delete(?ETS_ANUBIS_RANK,{anubis_season_rank,SeasonID}),
        %%更新存在赛季列表,此处没有更新当前赛季信息，是否会出现当前赛季没有切换的情况下，赛季被清除了
        {CurrentSeasonID,ExistSeasonIDList} = get_anubis_seasonid(),
        NewExistSeasonIDList = lists:filter(fun(ExistSeasonID)->not lists:member(ExistSeasonID,DeleteSeasonIDList) end,ExistSeasonIDList),
        set_anubis_seasonid(NewExistSeasonIDList,CurrentSeasonID)
    end,DeleteSeasonIDList),
    %%广播赛季删除信息到所有在线的公会服
    family_manager_server:broadcast_to_all_family({delete_season_rank,DeleteSeasonIDList}).

%%更新当前赛季编号,创建对应当前赛季排行
do_update_current_seasonid(SeasonID,State)->
    %%更新本地的当前赛季编号
    {_CurrentSeasonID,ExistSeasonIDList}=get_anubis_seasonid(),
    NewExistSeasonIDList = case lists:member(SeasonID,ExistSeasonIDList) of
        false->
            [SeasonID|ExistSeasonIDList];
        true->
            ExistSeasonIDList
    end,
    set_anubis_seasonid(NewExistSeasonIDList,SeasonID),
    %%此处没有增加对应的赛季信息，空列表不用创建
    %%广播赛季更新信息到公会服
    Msg = {update_current_seasonid,SeasonID},
    family_manager_server:broadcast_to_all_family(Msg),
    State#state{current_season_id=SeasonID}.

%%处理赛季奖励信息
do_send_season_reward(FamilyRewardList,SeasonID)->
    %?ERR("Send Season Reward FamilyList:~w SeasonID:~w ~n",[FamilyRewardList,SeasonID]),
    lists:foreach(fun({FamilyID,Rank})->
        case family_misc:router_to_family_process(FamilyID,{send_season_reward,Rank,SeasonID}) of
            ok->
                ignore;
            R ->
                ?ERR("send anubis season:~w rank:~w familyID:~w reward failed:~w ~n",[SeasonID,Rank,FamilyID,R])
        end
    end,FamilyRewardList).

get_syn_flage()->
    case get(?syn_flage) of
        ?undefined->
            {?SYN_FAILED,?MAX_SYN_TIMES};
        X ->
            X
    end.

set_syn_flage(X)->
    put(?syn_flage,X).

%%处理截断赛季排行榜
do_subseason_info(SeasonID)->
    SeasonRank = get_anubis_rank(SeasonID),
    SubSeasonRank = lists:sublist(SeasonRank,data_anubis:get(max_history_store_rank_num)),
    set_anubis_rank(SeasonID,SubSeasonRank).

%%根据游戏服存在的Family获取对应公会的Rank并且将新的Rank发送到对应公会，驱动公会进程更新自己的信息
update_family_rank(SeasonID,PersistRankList)->
    case get(?update_family_rank_pid) of
        ?undefined->
            Pid = spawn(fun()->update_anubis_rank2(SeasonID,PersistRankList) end),
            put(?update_family_rank_pid,Pid);
        OldPid->
            erlang:exit(OldPid,kill),
            NewPid = spawn(fun()->update_anubis_rank2(SeasonID,PersistRankList) end),
            put(?update_family_rank_pid,NewPid)
    end.
update_anubis_rank2(SeasonID,PersistRankList)->
    % ?ERR("begin to update family rank :SeasonID:~w ~n",[SeasonID]),
    FamilySummaryList = ets:tab2list(?ETS_FAMILY_SUMMARY),
    ServerExistFamilyIDList = [FamilyID||#p_family_summary{family_id=FamilyID}<-FamilySummaryList],
    NeedUpdateFamilyPlayerList = lists:filter(fun(#family_player{familyid=FamilyID})-> lists:member(FamilyID,ServerExistFamilyIDList) end, PersistRankList),
    lists:foreach(fun(#family_player{familyid=FamilyID}=FamilyPlayer)->
        ProcessName = family_misc:make_family_process_name(FamilyID),
        case erlang:whereis(ProcessName) of
            ?undefined->
                ignore;
            Pid ->
                Msg = {update_anubis_rank2,SeasonID,FamilyPlayer},
                erlang:send(Pid, Msg)
        end
    end,NeedUpdateFamilyPlayerList).

%%修改当前赛季公会阿努比斯赛季信息
test_change_family_anubis_info(FamilyID,FamilyKillNum,FamilyScore)->
    case family_misc:router_to_family_process(FamilyID,{test_change_family_anubis_info,FamilyKillNum,FamilyScore}) of
        ok->
            ok;
        R->
            ?ERR("test_change_family_anubis_info failed FamilyID:~w FamilyKillNum:~w FamilyScore:~w Result:~w ~n",[FamilyID,FamilyKillNum,FamilyScore,R])
    end.

%%修改玩家对应赛季的数据
test_change_role_anubis_info(RoleID,SeasonID,AddKillNum,AddResourcePoint)->
    case catch role_lib:send_server(RoleID, {test_change_role_anubisinfo,SeasonID,AddKillNum,AddResourcePoint}) of
        {'EXIT',_}->
            ?INFO("修改玩家:~w 阿努比斯赛季数据SeasonID:~w KillNum:~w ResourcePoint:~w 失败~n",[RoleID,SeasonID,AddKillNum,AddResourcePoint]);
        _ ->
            ok                          
  end.

test_change_family_anubis_info_batch(FileName)->
    case read_fix_content_from_file(FileName) of
        {false,Reason}->
            ?ERR("test_change_family_anubis_info_batch fail: ~w ~n",[Reason]);
        FamilyList ->
            test_change_family_anubis_info_batch2(FamilyList)
    end.

read_fix_content_from_file(FileName)->
    case file:consult(FileName) of
        {ok,[L]}->
            [{FamilyID,0,-AddScore}||{FamilyID,_UnionCoin,AddScore,_Times,_ServerID}<-L];
        _ ->
            ?ERR("read file:~w FileName failed~n",[FileName]),
            []
    end.
test_change_family_anubis_info_batch2(FamilyList)->
    lists:foreach(fun({FamilyID,AddKillNum,AddScore})->
            ServerID = (FamilyID div 1000000)-1,
            case node_info_server:get_node_info(ServerID) of
                ignore->
                    ?ERR("can not find Node for serverID:~w ~n",[ServerID]),
                    ?ERR("change FamilyID:~w AddKillNum:~w AddScore:~w failed~n",[FamilyID,AddKillNum,AddScore]);
                Node ->
                    rpc:cast(Node,anubis_server,test_change_family_anubis_info,[FamilyID,AddKillNum,AddScore])
            end
    end,FamilyList).