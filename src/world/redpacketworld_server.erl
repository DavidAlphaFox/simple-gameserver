-module(redpacketworld_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-compile(export_all).
-include("def_role.hrl").
-include("def_carlos.hrl").

-define(persist_interval, 120000).
-define(notice_interval, 30).
-define(pd_group_info,pd_group_info).
-define(pd_last_notice_version, pd_last_notice_version).
-record(group_info, {version = 0
                    ,rank = []
                    ,min_mun = 0
                    ,notice_timestamp = 0}).
-record(state, {is_open = 0
               ,open_time=[0,0,0]}).

%%%===================================================================
%%% API
%%%===================================================================

start( ) ->
    {ok,_} = 
    supervisor:start_child(plane_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	case data_setting:get(server_type) of
		carlos_match ->
			process_flag(trap_exit,true),
            {State,PdList} = case db_sql:get_etc(?DB_ETC_KEY_WORLD_REDPACKET) of
                                 {A,B}->
                                     {A,B};
                                 _ ->
                                     {#state{},[]}
                             end,
            lists:foreach(fun({PdName,PdValue})->
                                  erlang:put(PdName, PdValue)
                          end, PdList),
            erlang:send(self(), check_tick),
            set_persist_interval(),
			{ok, State};
		_->
			?ERR("not carlos match server"),
			ignore
	end.

handle_call({debug_get_pd,PdName}, _From, State) ->
    {reply, get(PdName), State};
handle_call(debug_get_state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?ERR("未知的cast消息:~p.~n", [Msg]),
    {noreply, State}.

handle_info({sync_world_rank,ServerID,SlaveVersion,IsGetReward},State) ->
    ?INFO("sync_world_rank start, ~w v:~w",[ServerID,SlaveVersion]),
    GroupID = redpacket_server:get_server_group_id(ServerID),
    GroupInfo = get({?pd_group_info,GroupID}),
    if
        GroupInfo =:= ?undefined ->
            ?ERR("sync_world_rank undefined, ~w v:~w",[ServerID,SlaveVersion]),
            ignore;
        SlaveVersion =:= GroupInfo#group_info.version andalso IsGetReward =:= false ->
            ?ERR("sync_world_rank ignore, ~w v:~w",[ServerID,SlaveVersion]),
            ignore;
        true ->
            MaxLength = data_redpacket:get(world_rank_num),
            MinMun = 
                case erlang:length(GroupInfo#group_info.rank) of
                    MaxLength -> GroupInfo#group_info.min_mun;
                    _ -> 0 end,
            send_msg:direct(ServerID, redpacket_server, {sync_world_rank
                                                        ,GroupInfo#group_info.version
                                                        ,GroupInfo#group_info.rank
                                                        ,MinMun,IsGetReward})
    end,
    {noreply,State};
handle_info({notice_golder_info, ServerID, NewPublisherList},OldState) ->
    NewState = do_check(OldState),
    if
        NewState#state.is_open =:= 1 orelse NewState#state.is_open =:= 2->
            ?INFO("sync_world_rank, NewPublisher:~w ",[NewPublisherList]),
            GroupID = redpacket_server:get_server_group_id(ServerID),
            GroupInfo = get({?pd_group_info,GroupID}),
            Now = util:now(),
            NewGroupInfo = 
                case GroupInfo of
                    ?undefined ->
                        #group_info{version = 1
                                   ,rank = NewPublisherList
                                   ,min_mun = 0
                                   ,notice_timestamp = Now};
                    _ ->
                        NewWorldRank1 = 
                            lists:foldl(fun(NewPublisher,AccNewWorldRank) ->
                                                case lists:keytake(NewPublisher#p_publisher_info.roleID, #p_publisher_info.roleID, AccNewWorldRank) of
                                                    {value,OldGolderInfo,OtherWorldRank} ->
                                                        [NewPublisher|OtherWorldRank];
                                                    false ->
                                                        [NewPublisher|AccNewWorldRank]
                                                end
                                       end, GroupInfo#group_info.rank, NewPublisherList),
                        NewWorldRank2 = redpacket_server:sort_gold_info_list(NewWorldRank1),
                        NewWorldRank3 = lists:sublist(NewWorldRank2, data_redpacket:get(world_rank_num)),
                        LastGolderInfo = lists:last(NewWorldRank3),
                        if
                            GroupInfo#group_info.notice_timestamp + ?notice_interval < Now ->
                                #group_info{version = GroupInfo#group_info.version+1
                                           ,rank = NewWorldRank3
                                           ,min_mun = LastGolderInfo#p_publisher_info.redpacket_num
                                           ,notice_timestamp = Now};
                            true ->
                                #group_info{version = GroupInfo#group_info.version+1
                                           ,rank = NewWorldRank3
                                           ,min_mun = LastGolderInfo#p_publisher_info.redpacket_num
                                           ,notice_timestamp = GroupInfo#group_info.notice_timestamp}
                        end     
                end,
            put({?pd_group_info,GroupID},NewGroupInfo),
            ?INFO("notice_golder_info (now:~w)~w",[Now,NewGroupInfo]),
            if
                Now == NewGroupInfo#group_info.notice_timestamp ->
                    notice_to_server(GroupID,NewGroupInfo);
                true ->
                    MaxLength = data_redpacket:get(world_rank_num),
                    MinMun = 
                        case erlang:length(NewGroupInfo#group_info.rank) of
                            MaxLength -> NewGroupInfo#group_info.min_mun;
                            _ -> 0 end,
                    send_msg:direct(ServerID, redpacket_server, {sync_world_rank
                                                                ,NewGroupInfo#group_info.version
                                                                ,NewGroupInfo#group_info.rank
                                                                ,MinMun,false})
            end;
        true ->
            ignore
    end,
    {noreply,NewState};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({debug_redpacket_world_status,ServerID},State) ->
    GroupID = redpacket_server:get_server_group_id(ServerID),
    send_msg:direct(ServerID, redpacket_server, {debug_redpacket_world_status_response,get({?pd_group_info,GroupID})}),
    {noreply,State};
handle_info(persist_tick,State) ->
    do_persist(State),
    set_persist_interval(),
    {noreply,State};
handle_info(check_tick,State) ->
    NewState = do_check(State),
    set_check_interval(NewState),
    {noreply,NewState};
handle_info(save_files,State) ->
    save_files(),
    {noreply,State};
handle_info(Info,State) ->
    ?ERR("未知的info消息:~p.~n", [Info]),
    {noreply,State}.

terminate(_Reason, State) ->
    do_persist(State),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notice_to_server(GroupID,GroupInfo)->
    Min = GroupID div 10000,
    Max = GroupID rem 10000,
    MaxLength = data_redpacket:get(world_rank_num),
    MinMun = 
        case erlang:length(GroupInfo#group_info.rank) of
            MaxLength -> GroupInfo#group_info.min_mun;
            _ -> 0 end,
    lists:foreach(fun(ServerID)->
            catch send_msg:direct(ServerID, redpacket_server, {sync_world_rank
                                                              ,GroupInfo#group_info.version
                                                              ,GroupInfo#group_info.rank
                                                              ,MinMun,false})
        end, lists:seq(Min, Max)),
    put({?pd_last_notice_version,GroupID},GroupInfo#group_info.version).

do_check(OldState) ->
    {state,IsOpen,OpenTime} = redpacket_server:check_state(),
    if
        OldState#state.is_open =:= 0 andalso IsOpen =:= 1 ->
            lists:foreach(fun({PdName,_})-> 
                    case PdName of
                        {?pd_group_info,_} ->
                            erlang:erase(PdName);
                        _ ->
                            ignore
                    end
                end,erlang:get());
        true ->
            Now = util:now(),
            lists:foreach(fun({PdName,GroupInfo})-> 
                    case PdName of
                        {?pd_group_info,GroupID} ->
                            CurVersion = GroupInfo#group_info.version,
                            case get({?pd_last_notice_version,GroupID}) of
                                CurVersion ->
                                    ignore;
                                _ ->
                                    if
                                        GroupInfo#group_info.notice_timestamp + ?notice_interval < Now ->
                                            put(PdName,GroupInfo#group_info{notice_timestamp = Now}),
                                            notice_to_server(GroupID,GroupInfo);
                                        true ->
                                            ignore
                                    end
                            end;
                        _ ->
                            ignore
                    end
                end,erlang:get())
    end,
    {state,IsOpen,OpenTime}.

%% 存盘
do_persist(State) ->
    db_sql:set_etc(?DB_ETC_KEY_WORLD_REDPACKET, {State,erlang:get()}).

%设定下次timer
set_persist_interval()->
    erlang:send_after(?persist_interval, self(), persist_tick).

set_check_interval(State)->
%%     erlang:send_after(1000*?notice_interval, self(), check_tick).
    Now = util:now(),
    [_,EndTime,Fianl] = State#state.open_time,
    Time = if
                 State#state.is_open == 0 ->
                     ?notice_interval;
                 State#state.is_open == 1 andalso (EndTime - Now) =< ?notice_interval ->
                     ?notice_interval - (EndTime - Now);
                 State#state.is_open == 2 andalso (Fianl - Now) =< ?notice_interval ->
                     ?notice_interval - (Fianl - Now);
                 true ->
                     ?notice_interval
             end,
    NextTime = max(1, Time)*1000,
    %% ?INFO("next set_check_interval ~w s:~w",[NextTime,State]),
    erlang:send_after(NextTime, self(), check_tick).


save_files()->
    erlang:garbage_collect(),
    {ok, F} = file:open("test2.txt", write),
    lists:foreach(fun({PdName,GroupInfo})->
                    case PdName of
                        {?pd_group_info,GroupID} ->
                            ?INFO("GroupInfo:~w",[GroupInfo]),
                            GroupInfoRank = redpacket_server:sort_gold_info_list(GroupInfo#group_info.rank),
                            save_files2(GroupInfoRank,1,F);
                        _ -> ignore
                    end
                end, get()),
    file:close(F),
    erlang:garbage_collect().

save_files2([],_,F)->
    ignore;
save_files2(GroupInfoSorted,Index,F)->
    [H|T] = GroupInfoSorted,
    io:format(F, "~w,~w,~w,~w~n", [H#p_publisher_info.roleID
                       ,H#p_publisher_info.serverID
                       ,redpacket_server:get_server_group_id(H#p_publisher_info.serverID)
                       ,Index]),
    save_files2(T,Index+1,F).

    
