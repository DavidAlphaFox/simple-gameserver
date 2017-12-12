-module(family_cross_fight_manager).
-bahaviour(gen_server).
-include("common.hrl").
-include("def_carlos.hrl").
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

new_war(WarID, Team1, Team2, Team3,Team1_1,Team2_1,Team3_1) ->
	erlang:send(?MODULE, {new_war, WarID, Team1,Team2,Team3,Team1_1,Team2_1,Team3_1}).

send_to_me({new_war, WarID,Team1,Team2,Team3}) ->
	{N, IDList} = data_family_cross_fight:get(war_servers),
	ID = lists:nth(random:uniform(N), IDList),
	send_msg:direct(ID, ?MODULE, {new_war_base, WarID,Team1,Team2,Team3}).

send_to_me(C,WarID,Team1,Team2,Team3) ->
	case get_war_node(C) of
		NodeID when is_integer(NodeID) ->
			send_msg:direct(NodeID,?MODULE,{new_war_base,WarID,Team1,Team2,Team3});
		_ ->
			?ERR("not find node,info:~w",[[C,WarID,Team1,Team2,Team3]])
			%db_sql:save_family_cross_dis_data([C,WarID,Team1,Team2,Team3])
	end.

get_war_node(C) ->
	data_family_cross_fight_nodes:get(C).

war_stopped(WarID) ->
	erlang:send(?MODULE, {war_stopped, WarID}).


-record(state,{}).

start()->
	case data_setting:get(server_type) of
		family_cross ->
			{ok,_} = supervisor:start_child(family_cross_fight_sup,
								   {family_cross_fight_battle_sup,
									{family_cross_fight_battle_sup, start_link, []},
									permanent, infinity, supervisor, [family_cross_fight_battle_sup]}),
			{ok, _}=supervisor:start_child( family_cross_fight_sup,
											{?MODULE, {?MODULE, start_link, []},
											 permanent, 600000,worker, [?MODULE]});
		_ ->
			ok
	end.

start_link() -> gen_server:start_link({local, ?MODULE},?MODULE, [],[]).

init([])->
	process_flag(trap_exit, true),
	ets:new(?ETS_FAMILY_CROSS_INFO, [named_table, set,public, {keypos,2}]),
	{ok, #state{}}.

handle_call(_,_,State)-> {reply, ok,State}.
handle_cast(_,State) -> {noreply, State}.

handle_info({new_war,WarID,Team1,Team2,Team3,Team1_1,Team2_1,Team3_1},State) ->
	family_cross_fight_battle:start([WarID,Team1,Team2,Team3,Team1_1,Team2_1,Team3_1]),
	{noreply, State};
handle_info({war_stopped,WarID}, State) ->
	ets:delete(?ETS_FAMILY_CROSS_INFO, WarID),
	{noreply, State};
handle_info({new_war_base,WarID,Team1,Team2,Team3},State)->
	ManagerServerID = data_setting:get(server_id),
	F = fun()->
				P=self(),
				send_msg:direct(Team1#fc_fighter.serverID,family_cross_fight_server,{get_player,Team1#fc_fighter.familyID,WarID,ManagerServerID,P,1}),
				send_msg:direct(Team2#fc_fighter.serverID,family_cross_fight_server,{get_player,Team2#fc_fighter.familyID,WarID,ManagerServerID,P,2}),
				send_msg:direct(Team3#fc_fighter.serverID,family_cross_fight_server,{get_player,Team3#fc_fighter.familyID,WarID,ManagerServerID,P,3}),
				{Team1_1,Team2_1,Team3_1} = wait_msg([]),
				family_cross_fight_battle:start([WarID,Team1_1,Team2_1,Team3_1,Team1#fc_fighter{type=1},Team2#fc_fighter{type=2},Team3#fc_fighter{type=3}])
				%new_war(WarID,Team1_1,Team2_1,Team3_1,Team1,Team2,Team3)
		end,
	spawn(F),
	{noreply,State};
handle_info(_,State)-> {noreply,State}.

terminate(_,_)-> ok.

code_change(_,State,_) -> {ok,State}.

wait_msg([A,B,C])->{A,B,C};
wait_msg(L)->
	receive
		X -> wait_msg([X|L])
	after 60000 ->
			case L of
				[] -> {[],[],[]};
				[A] -> {A,[],[]};
				[A,B] -> {A,B,[]};
				[A,B,C] ->{A,B,C}
			end
	end.
		  
								 
