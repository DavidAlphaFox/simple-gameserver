%% @author caohongyang
%% @doc database application 
%% Created 2013-2-19


-module(db_app).
-include("ets_name.hrl").
-behaviour(application).
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
	%db:start(),
	db:connect_mysql(),
	ets:new(?ETS_ROLE_ONLINE, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_ROLE_DUMP, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_STOP_FLAG, [{keypos, 1}, set, public, named_table]),
	%% 缓存玩家的role信息,keypos=2,如何清理缓存?
	ets:new(?ETS_ROLE_PUBLIC, [{keypos, 2}, set, public, named_table]),
    ets:new(?ETS_ROLE_LEVEL, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_HOMESTEAD_DATA_BASE_TABLE, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_HOMESTEAD_DATA_MACHINE_TABLE, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_HOMESTEAD_DATA_LOG_TABLE, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_ENARGY_FRIEND_DATA, [{keypos, 2}, set, public, named_table]),
	
	
	ets:new(?ETS_DUNGEON_MON_CACHE,[{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_ID, [{keypos, 1}, set, public, named_table]),
	tk_id:init(),
	tk_id:exLv_init(),
	ets:new(?ETS_RANDOM_SEED,[{keypos,1},named_table,set,public]),
	ets:insert(?ETS_RANDOM_SEED,{seed,random:uniform(100)}),
    ets:new(?ETS_CACHE_STONECHIP_LIST, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_HOME_LIST, [{keypos, 2}, set, public, named_table]),
    ets:new(?ETS_HOME_TASK_LIST, [{keypos, 2}, set, public, named_table]),
    ets:new(?ETS_CACHE_ROLE_PLUNDER, [{keypos, 2}, set, public, named_table]),
    ets:new(?ETS_WHISPER_NUM, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_ENERGY_ROLELIST, [{keypos, 1}, set, public, named_table]),
	ets:new(?ETS_CARLOS_ROLE_WAR,[{keypos,1},set, public, named_table]),
	ets:new(?ETS_GALACTICA_ROLE_WAR,[{keypos,1},set, public, named_table]),
	ets:new(?ETS_TWINS_ROLE_WAR,[{keypos,1},set,public,named_table]),
	ets:new(?ETS_LUCKY_ROLL, [set, public, named_table, {keypos, 1}, {heir, none}, {write_concurrency,false}, {read_concurrency,true}]),
	ets:new(?ETS_VIP_ACTIVITY, [set, public, named_table, {keypos, 1}, {heir, none}, {write_concurrency,false}, {read_concurrency,true}]),
    ets:new(?ETS_CACHE_CONFIG_ID, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_HOME_DATA, [{keypos, 1}, set, public, named_table]),
    
    ets:new(?ETS_DOUBLEMATCH_LINEUP_INFO,[{keypos, 2}, set, public, named_table]),
    
    ets:new(?ETS_DM_FIGHT_REC, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_DM_SIGN_LIST, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_DM_REC_LIST, [{keypos, 2}, set, public, named_table]),
    ets:new(?ETS_DM_RANK_DATA, [{keypos, 2}, set, public, named_table]),
    ets:new(?ETS_DM_INFO_LIST, [{keypos, 2}, set, public, named_table]),
    ets:new(?ETS_DM_OLD_RANK, [{keypos, 2}, set, public, named_table]),
	
	ets:new(?ETS_CARLOS_SIGN,[{keypos,1},set, public, named_table]),
	ets:new(?ETS_CARLOS_ROLETIMES,[{keypos,1},set, public, named_table]),
	
	ets:new(?ETS_GALACTICA_SIGN,[{keypos,1},set, public, named_table]),
	ets:new(?ETS_GALACTICA_ROLETIMES,[{keypos,1},set, public, named_table]),
	ets:new(?ETS_TWINS_SIGN,[{keypos,1},set, public, named_table]),
	ets:new(?ETS_TWINS_ROLETIMES,[{keypos,1},set, public, named_table]),
	ets:new(?ETS_HOME_BOSS_TABLE, [{keypos,2},set,public,named_table]),
	
	ets:new(?ETS_FAMILYCROSS_ROLE_WAR,[{keypos,1},set,public,named_table]),
	ets:new(?ETS_ANUBIS_RANK,[{keypos,1},set, public, named_table]),

    ets:new(?ETS_AFK_RECORD, [{keypos, 2}, set, public, named_table]),

    ets:new(?ETS_CONSUME_REABCK,[{keypos,1},set,public,named_table]),
    
    ets:new(?ETS_TASKLINK_LIST, [{keypos, 2}, set, public, named_table]),
    
    ets:new(?ETS_TREASURE_BOWL,[{keypos,1},set,public,named_table]),

    ets:new(?ETS_TRAINERREAR,[{keypos,2},set,public,named_table]),
    
    ets:new(?ETS_DOJANGRANK_1,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_DOJANGRANK_2,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_DOJANGRANK_3,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_DOJANGRANK_4,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_DOJANGRANK_5,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_DOJANGRANK_6,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_DOJANGRANK_7,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_DOJANGRANK_8,[{keypos,2},set,public,named_table]),
    ets:new(?ETS_CONSUMERANK,[{keypos,1},set,public,named_table]),
    db_sql:init_ets_whisper_num(),
    case 'db_sup':start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
    end.

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


