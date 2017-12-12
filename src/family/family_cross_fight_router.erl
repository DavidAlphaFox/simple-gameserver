-module(family_cross_fight_router).
-behaviour(gen_server).
-include("common.hrl").
-include("def_carlos.hrl").
-export([init/1,handle_call/3,handle_cast/2,handle_info/2, terminate/2,code_change/3]).

-record(state, {}).
-define(route,route).

send_war({WarID,ServerID},Msg,RoleID)->
	send_msg:direct(ServerID, ?MODULE, {route_family_cross_fight, WarID,Msg, data_setting:get(server_id), RoleID}).

send_role(ServerID, RoleID,Msg)->
	send_msg:direct(ServerID,?MODULE, {route_role, Msg,RoleID}).

send_client(ServerID,RoleID,Msg) ->
	send_msg:direct(ServerID, ?MODULE, {unicast_client, Msg, RoleID}).

unicast_client(ServerID,RoleID,Msg)->
	send_msg:direct(ServerID,?MODULE, {unicast_client,Msg,RoleID}).


start()->
	{ok,_} = supervisor:start_child(family_cross_fight_sup, {?MODULE,{?MODULE, start_link, []},
												 permanent, 600000, worker, [?MODULE]}).

start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

init([]) -> {ok, #state{}}.

handle_call(_,_,State) -> {reply, ok,State}.
handle_cast(_,State) -> {noreply,State}.

handle_info({route_family_cross_fight, WarID, Msg,ServerID,RoleID},State) ->
	case ets:lookup(?ETS_FAMILY_CROSS_INFO, WarID) of
		[#ets_family_cross_info{pid=Pid}] ->
			catch(erlang:send(Pid, {Msg, { RoleID,ServerID}}));
		_ ->
			send_msg:direct(ServerID, ?MODULE, {route_role, {?route, role_family_cross_fight, {send_client, {route, 2}}},RoleID})
	end,
	{noreply,State};
handle_info({route_role,Msg,RoleID},State) ->
	catch(role_lib:send_server(RoleID, Msg)),
    {noreply,State};
handle_info({unicast_client, Msg,RoleID},State) ->
	?unicast(RoleID, Msg),
	{noreply,State};
handle_info({inet_reply, _S,_Status}, State)->
	{noreply,State};
handle_info({Ref, _Res},State) when is_reference(Ref) ->
	{noreply, State};
handle_info(_,State) -> {noreply,State}.

terminate(_,_) -> ok.

code_change(_,State,_) -> {ok,State}.
