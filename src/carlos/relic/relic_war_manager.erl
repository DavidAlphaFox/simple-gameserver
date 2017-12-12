-module(relic_war_manager).
-behaviour(gen_server).
-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").
-export([start_link/0,send_to_me/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(pd_in_battle, pd_in_battle).  %缓存当前在战斗的玩家，防止重复报名

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
    case data_setting:get(server_type) of
        carlos_match ->
            {ok, _} =
                supervisor:start_child(carlos_sup,
                                       {relic_war_sup,
                                        {relic_war_sup, start_link, []},
                                        permanent, infinity, supervisor, [relic_war_sup]}),
            {ok, _} =
                supervisor:start_child(carlos_sup, 
                                       {?MODULE,
                                        {?MODULE, start_link, []},
                                        permanent, 600000, worker, [?MODULE]});
        _ ->
            ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

send_to_me({new_relic_war, WarID, Team, LevelRank}) ->
    IDList = data_relic:get(war_relic_fight_servers),
    ID = lists:nth(random:uniform(length(IDList)), IDList),
    send_msg:direct(ID,relic_war_manager,{new_relic_war_base, WarID, Team, LevelRank}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    process_flag(trap_exit,true),
    put(?pd_in_battle,[]),
    ets:new(?ETS_RELIC_PROCESS_INFO, [named_table, set, public,{keypos,2}]),
    %读取保存在数据库中的进程数据，恢复战场
    WarIDList = db_sql:get_relic_id_list(), 
    ?INFO("db_sql:get_relic_id_list ~w",[WarIDList]),
    lists:foreach(fun([WarID]) -> relic_war_server:start(WarID) end,WarIDList),
    {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("--END TERMINATE-processor_manager- ~w ~w",[_Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_info({new_relic_war, WarID, Attackers,Team,LevelRank}, State) ->
    case check_in_battle([RID||#member_base_info{roleID=RID}<-Team]) of
        true ->
            case relic_war_server:start([WarID, Attackers,LevelRank]) of
                {error, Reason} ->
                    ?ERR("new_relic_war fail ~w",[Reason]),
                    [send_msg:direct(ServerID,relic_server, {clear_war_info, RoleID, WarID, true})||#member_base_info{serverID=ServerID,roleID=RoleID}<-Team];
                R ->
                    add_in_battle([RID||#member_base_info{roleID=RID}<-Team]),
                    ?INFO("new_relic_war ~w",[R])
            end;
        false ->
            ?ERR("new_relic_war fail check_in_battle ~w",[[RID||#member_base_info{roleID=RID}<-Team]]),
            [send_msg:direct(ServerID,relic_server, {clear_war_info, RoleID, WarID, true})||#member_base_info{serverID=ServerID,roleID=RoleID}<-Team]
    end,
    {noreply, State};
handle_info({new_relic_war_base, WarID, Team,LevelRank},State) ->
    %新建进程收集战斗数据
    F = fun()-> 
                P = self(),
                [send_msg:direct(ServerID,relic_server, {get_player, RoleID, WarID,data_setting:get(server_id),P,attacker})||#member_base_info{serverID=ServerID,roleID=RoleID}<-Team],
                TeamLength = length(lists:usort([RID||#member_base_info{roleID=RID}<-Team])),
                case wait_msg([],TeamLength) of
                    {Attackers} ->
                        AttackersLength = length(Attackers),
                        if
                            TeamLength < 5 orelse AttackersLength < 5 ->
                                ?ERR("new_relic_war_base -- ~nAttackers:~w ~n team:~w",[Attackers,Team]),
                                [send_msg:direct(ServerID2,relic_server, {clear_war_info, RoleID2, WarID, true})||#member_base_info{serverID=ServerID2,roleID=RoleID2}<-Team];
                            true ->
                                ?INFO("new_relic_war_base -- ~nAttackers:~w ~n",[Attackers]),
                                erlang:send(?MODULE, {new_relic_war,WarID,Attackers,Team,LevelRank})
                        end;
                    fail ->
                        [send_msg:direct(ServerID2,relic_server, {clear_war_info, RoleID2, WarID, true})||#member_base_info{serverID=ServerID2,roleID=RoleID2}<-Team]
                end
        end,
    spawn(F),
    {noreply,State};

handle_info({clear_in_battle, RoleIdList}, State) ->
    clear_in_battle(RoleIdList),
    {noreply,State};
handle_info({to_war_chlid,WarID,ServerID,RoleID,RolePid,Msg}, State) ->
    case ets:lookup(?ETS_RELIC_PROCESS_INFO, WarID) of
        [#ets_carlos_info{warID=WarID,pid=PID}] ->
            %?WARNING("to_war_chlid ~w",[{to_war_chlid,Msg,WarID,ServerID,RoleID,RolePid}]),
            erlang:send(PID,{Msg,{RoleID,ServerID,RolePid}});
        _ ->
            send_msg:direct(ServerID,relic_server, {clear_war_info, RoleID, WarID, false}),
            erlang:send(RolePid,{relic_to_client,#sc_carlos_relic_war_base_info{result=2
                                                    ,endTimeStamp=0
                                                    ,islandes=[]
                                                    ,players=[]
                                                    ,other_info=[]
                                                    ,boss_active_timeout_max=0
                                                    ,atk_reinforce=0
                                                    ,damage_reduce=0}}),
            ?ERR("lost war(~w,~w) child process pid",[WarID,RoleID])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    ?WARNING("Unknown info ~w",[_Info]),
    {noreply, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

check_in_battle(RoleIdList) ->
    TsLimit = util:now() - data_relic:get(war_interval),
    lists:all(fun(Id)-> 
                      case lists:keytake(Id, 1, get(?pd_in_battle)) of
                          false ->
                              true;
                          {value,{Id,LastTs},OtherInList} ->
                              if
                                  TsLimit > (LastTs + 10) -> % 多出10秒，防止数据延迟
                                    ?ERR("check_in_battle ~w",[{Id,LastTs}]),
                                    put(?pd_in_battle,OtherInList),
                                    true;
                                  true ->
                                      false
                              end
                      end
              end, RoleIdList).

add_in_battle(RoleIdList) ->
    Now = util:now(),
    NewInList = 
        lists:foldl(fun(Id,AccList) -> 
                         [{Id,Now}|AccList]
                    end, get(?pd_in_battle), RoleIdList),
    put(?pd_in_battle,NewInList).

clear_in_battle(RoleIdList) ->
    lists:foreach(fun(Id)-> 
                      case lists:keytake(Id, 1, get(?pd_in_battle)) of
                          false ->
                              ?ERR("clear_in_battle(~w)~w",[Id,get(?pd_in_battle)]),
                              ignore;
                          {value,{Id,LastTs},OtherInList} ->
                              put(?pd_in_battle,OtherInList)
                      end
              end, RoleIdList).

wait_msg(A,0) ->
    {A};
wait_msg(A,LA) ->
    receive 
        {attacker,Player} ->
            wait_msg([Player|A],LA-1)
    after 60000 ->
            ?ERR("wait msg failed..."),
            fail
    end.

a()->
    io:format("----show relic_war_sup child-----~n", []),
    lists:foldr(fun({ProcId,Child,Type,Modules},AccList)->
                        WarID = gen_server:call(Child, test_get_info),
                        io:format("(~w)~w ~w ~w ~w~n", [WarID,ProcId,Child,Type,Modules])                
                end, [], supervisor:which_children(relic_war_sup)).
    
test_sample1()->
        erlang:send(?MODULE,{new_relic_war_base, 9999
                        , [{1,5,6011646},{1,5,6011648}]
                        }).

nt(RL) ->
	WarID=random:uniform(100000),
	RL2 = [begin
			   send_msg:direct(S, relic_server, {test_war, R, WarID}),
			   {1,S,R}
		   end||{S,R}<-RL],
	
	erlang:send(?MODULE,{new_relic_war_base, WarID, RL2 }).

test_start() ->
    {ok,_} = 
    supervisor:start_child(carlos_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]}).


test_show_info(WarID)->
    test_send_msg(WarID,test_show_info).

test_send_msg(WarID,Msg)->
    case ets:lookup(?ETS_RELIC_PROCESS_INFO, WarID) of
        [#ets_carlos_info{warID=WarID,pid=PID}] ->
            erlang:send(PID,Msg);
        _ ->
            not_found
    end.

test_pos(WarID)->
    case ets:lookup(?ETS_RELIC_PROCESS_INFO, WarID) of
        [#ets_carlos_info{warID=WarID,pid=PID}] ->
            erlang:send(PID,{test_show_info,pos});
        _ ->
            not_found
    end.

test_do_end_war(WarID)->
    case ets:lookup(?ETS_RELIC_PROCESS_INFO, WarID) of
        [#ets_carlos_info{warID=WarID,pid=PID}] ->
            erlang:send(PID,do_end_war);
        _ ->
            not_found
    end.

test_win_war(WarID)->
    case ets:lookup(?ETS_RELIC_PROCESS_INFO, WarID) of
        [#ets_carlos_info{warID=WarID,pid=PID}] ->
            erlang:send(PID,test_win_war);
        _ ->
            not_found
    end.
