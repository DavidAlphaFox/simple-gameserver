%% @author caohongyang
%% @doc application of world node
%% Created 2013-3-7


-module(behavior_app).
-behaviour(application).
-include("common.hrl").
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-define(ALL_BEHAVIOR_MOD,
		[
		 behavior_gold_consume
		,behavior_gold_pay_add
		,behavior_gold_bonus_add
		,behavior_coin_consume
		,behavior_coin_add
		,behavior_repu_consume
		,behavior_repu_add
        ,behavior_unioncoin_consume
        ,behavior_unioncoin_add
		,behavior_score_consume
		,behavior_item_add
		,behavior_item_consume
		,behavior_item_uprank
		,behavior_item_uplevel
		,behavior_ticket_add
		,behavior_ticket_consume
		,behavior_ger_add
		,behavior_ger_consume
		,behavior_ger_uplevel
		,behavior_ger_uprank
        ,behavior_ger_downrank
		,behavior_item_downrank
		,behavior_login_log
		,behavior_create_role
		,behavior_homestead_mating
		,behavior_friend_enargy
		,behavior_world_boss
		,behavior_pvp_fight
		,behavior_dungen_fight
		,behavior_race_sign
		 ,behavior_log_times
		 ,behavior_familytek_source_consume
		 ,behavior_profoundCrystal_add
		 ,behavior_profoundCrystal_consume
         ,behavior_honor_add
         ,behavior_honor_consume
         ,behavior_home_resource_add
         ,behavior_home_resource_consume
         ,behavior_pvppoint_add
         ,behavior_pvppoint_consume
         ,behavior_carlos
         ,behavior_anubis_score_add
         ,behavior_laputastone_add
         ,behavior_laputastone_consume
         ,behavior_item_enchant
         ,behavior_ger_awake
         ,behavior_ger_crystal
		 ,behavior_magicBook_update
         ,behavior_dojankrank_world_fight
		 ,behavior_energy_consume
		 ]).
%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
	Return = 'behavior_sup':start_link(),
	lists:foreach(fun start_mod/1, ?ALL_BEHAVIOR_MOD),
	start_(behavior_online_num),
	Return.

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_mod(ServerName) ->
	?CATCH(behavior_mod:start(ServerName)).

start_(ServerName) ->
	?CATCH(ServerName:start()).
