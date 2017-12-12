-module(afk_record_server).
-behaviour(gen_server).
-export([start/0,start_link/0
        ,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-record(state, {}).

-define(persist_interval, 180000).
-define(punish_time, 86400). %% 24H

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit,true),
    L = case db_sql:get_etc(?DB_ETC_KEY_AFK_RECORD) of
            L0 when erlang:is_list(L0) ->
                L0;
            _ ->
                []
        end,
    ets:insert(?ETS_AFK_RECORD, L),
    {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
handle_info(persist_tick, State) ->
    persist_afk_record(),
    set_persist_interval(),
    {noreply,State};
handle_info(Info, State) ->
    ?ERR("未知的消息:~w.~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    persist_afk_record(),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

set_persist_interval()->
    erlang:send_after(?persist_interval, self(), persist_tick).

persist_afk_record()->
    db_sql:set_etc(?DB_ETC_KEY_AFK_RECORD, ets:tab2list(?ETS_AFK_RECORD)).

%% 该方法被执行时，战斗未彻底结束，无法报名，所以不需要做互斥处理，可以异步调用。
add_afk_record(RoleID,WarType)->
    case ets:lookup(?ETS_AFK_RECORD, {RoleID,WarType}) of
        []->
            NewRec = #afk_record{afk_key = {RoleID,WarType}
                                ,afk_time = 1
                                ,punish_timestamp = 0},
            ets:insert(?ETS_AFK_RECORD, NewRec);
        [OldRec] when OldRec#afk_record.afk_time >= 1 ->
            Now = util:now(),
            NewRec = OldRec#afk_record{afk_time = 2
                                      ,punish_timestamp = Now + ?punish_time},
            ets:insert(?ETS_AFK_RECORD, NewRec)
    end.

get_afk_punish(RoleID,WarType) ->
    case ets:lookup(?ETS_AFK_RECORD, {RoleID,WarType}) of
        []->
            {0,0};
        [OldRec] when OldRec#afk_record.afk_time =:= 1 ->
            {1,0};
        [OldRec] when OldRec#afk_record.afk_time >= 2 ->
            Now = util:now(),
            if
                OldRec#afk_record.punish_timestamp =< Now ->
                    ets:delete_object(?ETS_AFK_RECORD,OldRec),
                    {0,0};
                true ->
                    {2,OldRec#afk_record.punish_timestamp}
            end
    end.
    


