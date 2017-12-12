-module(deamon_server).
-behaviour(gen_server).
-include("common.hrl").

-export([start/0,start_link/0
        ,check_process_immediately/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {timer_ref=?undefined
               ,mass_msg_process_list=[]}).

-record(process_monitor_info,{role_id=0,num_list=[],last_msg=?undefined}).

-define(INTERVAL_TIME,180000). % check per 3min 
-define(MSG_QUEUE_LEN_MAX,50). % 消息上限

%% ====================================================================
%% API functions
%% ====================================================================
start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%立即执行一次检查
check_process_immediately() ->
    erlang:start_timer(1, ?MODULE, check_processs).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

    
init([]) ->
    TimerRef = erlang:start_timer(?INTERVAL_TIME, self(), check_processs),
    {ok, #state{timer_ref=TimerRef,mass_msg_process_list=[]}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
  
handle_info({timeout, TimerRef, check_processs}, State) ->
    if
        TimerRef /= State#state.timer_ref ->
            ?WARNING("timer is wrong. ~w ~w",[TimerRef, State#state.timer_ref]),
            erlang:cancel_timer(State#state.timer_ref);
        true ->
            ignore
    end,
    NewList = check_mass_msg_processes(State#state.mass_msg_process_list),
    ?INFO("period check mass_msg_process_list:~w",[NewList]),
    NewTimerRef = erlang:start_timer(?INTERVAL_TIME, self(), check_processs),
    {noreply, State#state{timer_ref=NewTimerRef,mass_msg_process_list=NewList}};
handle_info(Info, State) ->
    ?ERR("unkown info ~w state:~w",[Info,State]),
    {noreply, State}.

terminate(Reason, State) ->
    ?INFO("~w terminate for \nReason=~300p\nState=~300p\n",[deamon_server, Reason,  State]), 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_mass_msg_processes(OldProcessList)->
    lists:foldr(fun({RoleId,_},AccProcessList)->
                        ProcName = list_to_atom(lists:concat(["role", RoleId])),
                        case erlang:whereis(ProcName) of
                            RolePid when erlang:is_pid(RolePid) ->
                                check_processe_msg(RoleId,RolePid,OldProcessList,AccProcessList);
                            _ ->
                                AccProcessList
                        end
                end, [], ets:tab2list(?ETS_ROLE_ONLINE)).

check_processe_msg(RoleId,RolePid,OldProcessList,AccProcessList)->
    {message_queue_len,QL} =erlang:process_info(RolePid,message_queue_len),
    {messages,CurMsgList} =erlang:process_info(RolePid,messages),
    if
        QL > ?MSG_QUEUE_LEN_MAX -> 
            %发现队列大于阀值的role process
            [FirstMsg|_] = CurMsgList,
            case lists:keysearch(RoleId, #process_monitor_info.role_id, OldProcessList) of
                {value,ProcessMonitorInfo} ->
                    [LastNum|_] = OldQL = ProcessMonitorInfo#process_monitor_info.num_list,
                    if
                        QL >= LastNum  %队列持续增长，且队列头的消息是同一个，即消息极可能是停滞不前的
                          andalso ProcessMonitorInfo#process_monitor_info.last_msg =:= FirstMsg ->
                            Times = erlang:length(OldQL),
                            if
                                Times >= 2 ->
                                    ?ERR("role ~w process is killed. ~w",[RoleId,OldQL]),
                                    kill_role_process(RoleId,RolePid),
                                    AccProcessList;     % 1、认定为永久堵塞的进程，执行杀死
                                true ->
                                    [ProcessMonitorInfo#process_monitor_info{num_list=[QL|OldQL]}|AccProcessList] % 2、进程消息队列依然很长
                            end;
                        true ->
                            [#process_monitor_info{role_id=RoleId
                                                  ,num_list=[QL]
                                                  ,last_msg=FirstMsg}|AccProcessList] % 3、消息队列的头部消息不同，或消息有减少，重新计数
                    end;
                false ->
                    [#process_monitor_info{role_id=RoleId
                                          ,num_list=[QL]
                                          ,last_msg=FirstMsg}|AccProcessList] % 4、新发现的大队列进程
            end;
        true ->
            AccProcessList  % 5、正常，队列消息不算长
    end.

kill_role_process(RoleId,RolePid)->
    user_default:kill(RolePid),
    ets:delete(role_state,RoleId).

    
