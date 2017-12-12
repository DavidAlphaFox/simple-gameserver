-module(all_family_rank_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-define(SYNC_INTERVAL, (1000 * 60 * 60)).       %% 刷新排行的时间
-define(DUMP_INTERVAL, (1000 * 60 * 10)).       %% 数据存盘的时间
-define(FMCACHE_INTERVAL, (1000 * 60 * 10)).    %% 刷新跨服公会成员列表缓存的时间
-define(SCORE_LOCK_INTERVAL, (1000 * 60 * 10)). %% 公会分数修改锁定时间

-define(FMCACHE_LIST, fm_cache_list).           %% 缓存查看的跨服公会成员列表
-define(SCORE_LOCK, score_lock).                %% 公会分数修改锁
-record(state, {rank_list=[]}).
%% rank_list 为前x名缓存

%%%===================================================================
%%% API
%%%===================================================================
-compile(export_all).
start( ) ->
    {ok,_} = 
    supervisor:start_child(world_sup,
        {?MODULE,
            {?MODULE, start_link, []},
            permanent, 600000, worker, [?MODULE]}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
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
    ?FAMILY_FIGHT_MASTER_ETC = ets:new(?FAMILY_FIGHT_MASTER_ETC, [set, public, named_table, {keypos,1}, {heir,none}, {write_concurrency,false}, {read_concurrency,true}]),
    NewState =  
        case db_sql:get_etc(?DB_ETC_KEY_CROSS_FAMILY_RANK) of
            [{state, State}|EtsList] ->
               lists:foreach(fun(Object) -> ets:insert(?FAMILY_FIGHT_MASTER_ETC, Object) end, EtsList),
                State;
            _ ->
                #state{rank_list=[]}
        end,
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    erlang:send_after(?SYNC_INTERVAL, erlang:self(), sync_data),
    erlang:send_after(?FMCACHE_INTERVAL, erlang:self(), refresh_fmcache),
	erlang:send_after(?DUMP_INTERVAL, self(), do_hibernate),
    {ok, NewState}.

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

handle_info(do_hibernate,State)->
	erlang:send_after(?DUMP_INTERVAL, self(),do_hibernate),
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
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
    {noreply, State};

do_handle_info(sync_data, State) ->
    Limit = data_family:get(sync_amount_limit),
    NewRankList = db_sql:get_cross_family_rank_head(Limit), 
    send_msg_to_all_node({afrs_update_firstN, NewRankList}),
    OrderList = db_sql:get_cross_family_rank_order(),
    {_, ServerOrderList} =
        lists:foldl(fun([FamilyID],{Rank, AccList}) ->
                        case ets:lookup(?FAMILY_FIGHT_MASTER_ETC, {?FAMILY2ServerID, FamilyID}) of
                            [] ->
                                {Rank+1, AccList};
                            [{_,ServerID}] ->
                                case lists:keytake(ServerID, 1, AccList) of
                                    false ->
                                        NewAccList = [{ServerID, [{FamilyID, Rank}]}|AccList],
                                        {Rank+1, NewAccList};
                                    {value, {ServerID, IDRankList}, RestList} ->
                                        NewIDRankList = [{FamilyID, Rank}|IDRankList],
                                        NewAccList = [{ServerID, NewIDRankList}|RestList],
                                        {Rank+1, NewAccList}
                                end
                        end
                    end, {1, []}, OrderList),
    lists:foreach(fun({ServerID, SendList}) -> send_msg_to_server(ServerID, {afrs_update_rank, SendList}) end, ServerOrderList),
    erlang:send_after(?SYNC_INTERVAL, erlang:self(), sync_data),
    {noreply, State#state{rank_list=NewRankList}};

%% 子服提交新数据(分数锁定时,不更新公会的积分)
do_handle_info({submit_summary_info, ServerID, NewList}, State) ->
    IsLocked = erlang:get(?SCORE_LOCK),
    lists:foreach(fun(E) -> update_family_info(E, ServerID, IsLocked) end, NewList),  
    {noreply, State};

%% 子服公会解散
do_handle_info({delete_family_info, FamilyIDList}, State) ->
    lists:foreach(fun(FamilyID) -> delete_family_info(FamilyID) end, FamilyIDList),
    {noreply, State};

%% 子服请求排名(主要对新服还没同步数据时)
do_handle_info({get_rank_list, ServerID, RoleID, Start, Num}, #state{rank_list=RankList}=State) ->
    send_msg_to_server(ServerID, {afrs_rank_list, RoleID, Start, Num, RankList}),
    {noreply, State};

%% 跨服取公会成员列表
do_handle_info({get_cross_member_list, SrcServerID, RoleID, ViewFamilyID}, State) ->
    %% 先查看缓存
    case erlang:get({?FMCACHE_LIST, ViewFamilyID}) of
        List when is_list(List) ->
            send_msg_to_server(SrcServerID, {get_cross_member_list_return, RoleID, ViewFamilyID, List});
        _ ->
            case ets:lookup(?FAMILY_FIGHT_MASTER_ETC, {?FAMILY2ServerID, ViewFamilyID}) of
		        [] ->
		            send_msg_to_server(SrcServerID, {get_cross_member_list_return, RoleID, ViewFamilyID, []});
		        [{_,ServerID}] ->
                    send_msg_to_server(ServerID, {afrs_get_cross_member_list, SrcServerID, RoleID, ViewFamilyID})
		    end
    end,
    {noreply, State};

%% 返回的公会成员列表
do_handle_info({get_cross_member_list_return, ServerID, RoleID, ViewFamilyID, List}, State) ->
    erlang:put({?FMCACHE_LIST, ViewFamilyID}, List),
    send_msg_to_server(ServerID, {get_cross_member_list_return, RoleID, ViewFamilyID, List}),
    {noreply, State};

%% 清空缓存的跨服公会成员列表
do_handle_info(refresh_fmcache, State) ->
    erlang:send_after(?FMCACHE_INTERVAL, erlang:self(), refresh_fmcache),
    lists:foreach(fun(E) ->
                    case E of
                        {?FMCACHE_LIST, _} = Key ->
                            erlang:erase(Key);
                        _ ->
                            ignore
                    end
                end, erlang:get()),
    {noreply, State};

%% 公会战服更新积分时直接将最新的积分发送过来,这里更新积分后立马
%% 进行一次刷新(130策划需求),为了防止子服发送过来的更新数据覆盖掉
%% 当前的最新积分,这里加一个修改锁.在积分锁定期间,将不会更新数据库
%% 中的积分字段,直到锁被解除后.
do_handle_info({family_fight_updete_score, ScoreList}, State) ->
    erlang:put(?SCORE_LOCK, true),
    lists:foreach(fun({FamilyID, Score}) ->
                    db_sql:update_family_rank_score(FamilyID, Score)
                  end, ScoreList),
    %% 同步新排名
    erlang:send(?MODULE, sync_data),
    erlang:send_after(?SCORE_LOCK_INTERVAL, erlang:self(), unlock_score),
    {noreply, State};

do_handle_info(unlock_score, State) ->
    erlang:put(?SCORE_LOCK, false),
    {noreply, State};

do_handle_info(Info, State) ->
    ?ERR("can't handle this message:~w, state:~w", [Info, State]),
    {noreply, State}.

send_msg_to_server(ServerID, Msg) ->
    send_msg:direct(ServerID,family_manager_server, Msg).

send_msg_to_all_node(Msg) ->
    send_msg:broadcast(family_manager_server, Msg).

do_persist(State) ->
    Info = {state, State},
    EtcList = ets:tab2list(?FAMILY_FIGHT_MASTER_ETC),
    db_sql:set_etc(?DB_ETC_KEY_CROSS_FAMILY_RANK, [Info|EtcList]).

delete_family_info(FamilyID) ->
    ets:delete(?FAMILY_FIGHT_MASTER_ETC, {?FAMILY2ServerID, FamilyID}),
    db_sql:delete_family_rank_info(FamilyID). 

update_family_info(#p_family_summary{family_id=FamilyID}=Info, ServerID, IsLocked) ->
    ets:insert(?FAMILY_FIGHT_MASTER_ETC, {{?FAMILY2ServerID, FamilyID}, ServerID}),
    db_sql:update_family_rank_info(Info, IsLocked).

%%------------------------------------------------------------------------------ 
%%测试指令
force_sync_data() ->
    erlang:send(?MODULE, sync_data).
