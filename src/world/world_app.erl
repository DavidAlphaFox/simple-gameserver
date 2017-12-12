%% @author caohongyang
%% @doc application of world node
%% Created 2013-3-7


-module(world_app).
-behaviour(application).
-include("common.hrl").
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



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
    Return = 'world_sup':start_link(),
    random:seed(now()),
    case data_setting:get(server_type) of
        normal ->
			ets:new(?ETS_ETC,[{keypos,1},named_table,set,public]),
			ets:new(?ETS_HRON,[{keypos,1},named_table,set,public]),	
			ets:new(?ETS_BOUNTY,[{keypos,1},named_table,set,public]),
			start_(broadcast_server),
			start_(state_sync_server),
            start_(etc_server),
			start_(pay_server),
			start_(push_server),
			start_(pvp_server),
			start_(hula_server),
			start_(nanm_server),
			start_(hron_server),
			start_(mail_server),
			start_(hist_server),
			start_(homestead_server),
			start_(enargy_server),
            start_(friend_server),
            start_(invite_server),
            start_(activity_server),
            start_(talk_server),
            start_(fire_server),
            start_(activityRank_server),
            start_(rebate_server),
%%             start_(race_server),
            start_(race2_server),
            start_(team_pk_server),
            start_(alien_server),
            start_(levelRank_server),
            start_(plunder_server),
            start_(home_server),
            start_(trumpet_server),
            start_(discount_server),
            start_(family_fight_server),         %% 联盟战从服务器
            start_(panic_buy_server),            %% 全区抢购从服务器
            start_(team_manage_server),          %% 组队管理服务器
			start_(activityVipServer),
			start_(lucky_roll_server),
            start_(relic_server),
            start_(match_room_server),
            start_(sign_server),
			start_(bounty_server),
            start_(goldenegg_server),
            start_(doublematch_server),
			start_(home_boss_server),
            start_(afk_record_server),
            start_(consume_back_server),
            start_(redpacket_server),
            start_(tasklink_server),
            start_(treasurebowl_server),
            start_(dojangrank_server),
            start_(consumerank_server),
%暂时不用            start_(deamon_server),               %% 监控玩家进程的守护进程
            deal_merge_touch();
        familyfight ->
            start_(all_family_rank_server),
            start_(family_fight_master_server),  %% 联盟战主服务器
            start_(all_panic_buy_server),        %% 全区抢购
            start_(cross_talk_server);           %% 跨服喇叭
		master ->
            start_(alien_master_server);
        distribute ->
            start_(alien_distribute);
        finals ->
            start_(alien_finals);
        dojangrank_world_master ->
            start_(dojangrank_world_server);
		Type ->
			?ERR("server type = ~w, world start nothing",[Type])
	end,
    start_(db_manage_server),
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
start_(ServerName) ->
	?CATCH(ServerName:start()).

deal_merge_touch() ->
    catch file:delete("./merge.touch").
