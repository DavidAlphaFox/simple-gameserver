-include("db_name.hrl").
-include("ets_name.hrl").
-include("def_money_log.hrl").
-define(TCP_OPTIONS, [
					  binary
					 ,{packet, 0}
					 ,{active, false}
					 ,{reuseaddr, true}
					 ,{nodelay, false}
					 ,{delay_send, true}
					 ,{send_timeout, 15000}
					 ,{keepalive, true}
					 ,{backlog,8192}
					 ,{exit_on_close, true}]).

-define(TICKET, "this_is_crimoon_server").

-define(undefined, undefined).
-define(is_doing_merge, is_doing_merge).
-define(AccidBase, 10000000000).
-define(ROLE_ID_BASE, ((data_setting:get(server_id)+1) * 1000000)).
-define(FAMILY_ID_BASE, ((data_setting:get(server_id)+1) * 1000000)).
-define(GER_ID_BASE,   ((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(ITEM_ID_BASE,  ((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(MAIL_ID_BASE,  ((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(REPLAY_ID_BASE,((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(TEAM_ID_BASE,((data_setting:get(server_id)+1) * (10000 * 10000 * 10000))).
-define(BONUS_ID_BASE, 0).

-define(REPLAY_TYPE_UNKNOWN, 0).
-define(REPLAY_TYPE_PVP, 1).
-define(REPLAY_TYPE_PLUNDER, 2).
-define(REPLAY_TYPE_KING, 3).
-define(REPLAY_TYPE_EMPEROR, 4).
-define(REPLAY_TYPE_CROSS, 5).
-define(REPLAY_TYPE_RACE, 6).
-define(REPLAY_TYPE_3V3, 7).
-define(REPLAY_TYPE_ALIEN, 8).
-define(REPLAY_TYPE_CARLOS, 10).
-define(REPLAY_TYPE_FAMILY_CROSS,11).

-define(CHAT_CHANNEL_WORLD, 1).     %世界频道
-define(CHAT_CHANNEL_FAMILY, 3).    %公会频道

-define(LOCAL_TRUMPET, 1).          %%小喇叭
-define(CROSS_TRUMPET, 2).          %%大喇叭

-define(CATCH(Expression), (
							try Expression
							catch 
								ErrType:Reason ->
									?ERR("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, Reason, erlang:get_stacktrace(), ??Expression]),
									{'EXIT',{ErrType, Reason}}
							end
							)).

-define(LOOSE_CATCH(Expression), (
							try Expression
							catch 
								throw:SomeThing ->
									SomeThing;
								ErrType:Reason ->
									?ERR("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, Reason, erlang:get_stacktrace(), ??Expression]),
									{'EXIT',{ErrType, Reason}}
							end
							)).
-define(CATCH_1(Expression,Reason), (
							try Expression
							catch 
								_:Reason ->
									?ERR("ErrReason:~1000p, Stack=~100000p, Expression=~s",[Reason, erlang:get_stacktrace(), ??Expression]),
									{'EXIT',Reason}
							end
							)).

%% 致命错误
-define(CRITICAL(Format), logger:critical_msg(?MODULE,?LINE, Format, [])).
-define(CRITICAL(Format, Args), logger:critical_msg(?MODULE,?LINE, Format, Args)).

%% 信息
-define(INFO(Format), logger:info_msg(?MODULE,?LINE, Format, [])).
-define(INFO(Format, Args), logger:info_msg(?MODULE,?LINE, Format, Args)).

%% 警告
-define(WARNING(Format), logger:warning_msg(?MODULE,?LINE, Format, [])).
-define(WARNING(Format, Args), logger:warning_msg(?MODULE,?LINE, Format, Args)).

%% 开发信息
-define(DEV(Format), logger:dev_msg(?MODULE,?LINE, Format, [])).
-define(DEV(Format, Args), logger:dev_msg(?MODULE,?LINE, Format, Args)).

%% 调试信息
-define(DEBUG(Format), logger:debug_msg(?MODULE,?LINE, Format, [], [{module, ?MODULE}])).
-define(DEBUG(Format, Args), logger:debug_msg(?MODULE,?LINE, Format, Args, [{module, ?MODULE}])).

%% 错误信息
-define(ERR(Format), logger:error_msg(?MODULE,?LINE, Format, [])).
-define(ERR(Format, Args), logger:error_msg(?MODULE,?LINE, Format, Args)).

%% 不应该出现的日志
-define(UNEXPECTED, ?ERR("unexpected~~n")).

%% 类型定义
-define(int8, type:int8()).
-define(int16, type:int16()).
-define(int32, type:int32()).
-define(int64, type:int64()).
-define(string, type:str()).

%% 这个只在战斗中使用,然后除法和取绝对值都比较费时间
%%-define(sign(Integer), (Integer div abs(Integer))).
-define(sign(Integer), (Integer > 0)).

-define(male, 1).%男
-define(female,2).%女

% -define(unicast(RoleID, Info), role_lib:send_client(RoleID,Info)).
-define(unicast(RoleID, Info), role_lib:send_client3(RoleID,Info)).
-define(unicast2(RoleID, Info), role_lib:send_client_no_block(RoleID,Info)).
-define(unicast_async(RoleID, Info), role_lib:send_client_async(RoleID, Info)).

-define(ONE_DAY_SECONDS, 86400).
-define(ONE_HOUR_SECONDS,3600).
-define(TEN_MINUTES_SECONDS,600).

-define(AFK_TYPE_CARLOS, 1).
-define(AFK_TYPE_RELIC,2).
-define(AFK_TYPE_TWINS,3).
-define(AFK_TYPE_GALACTICA,4).
-define(AFK_TYPE_CONQUERISLAND,5).

-type timerRef() :: {EndSec :: ?int32, reference()}.

-define(DB, tk).

%%任务结构数据
-record(r_task,{
	task_id	,									%%任务id
	status,										%%任务状态  1:未接  2：已接  3：已完成
	trigger_id,								%%触发类型
	trigger_num,							%%触发了的个数（比如当前杀了多少个怪）
	trigger_notes	=[]						%%触发记录(根据触发类型来确定是否用这个字段)
}).


%%HeadSeven任务的类型
-define(LOGIN_REWARD,1).
-define(N_GOLD_PURCHASE,2).
-define(RECHARGE_ONCE_N,3).
-define(RECHARGE_ACC_TN,4).
-define(ROLE_LEVEL_N,5).
-define(FORMATION_GER_STAR_N_QUALITY_N_ISMEGA_TN,6).
-define(FORMATION_GER_LEVEL_TN,7).
-define(BATTLE_FINISH,8).
-define(FORMATION_GER_EQUIP_SUIT_X_TN,9).
-define(FORMATION_GER_EQUIP_LEVEL_TN,10).
-define(FORMATION_GER_DESTINY_TN,11).
-define(BUIDING_X_LEVEL_N,12).
-define(FORMATION_GER_EQUIP_STAR_TN,13).
-define(FORMATION_GER_STAR_TN,14).
-define(ACTIVATE_TALENT,15).
-define(FORMATION_GER_EQUIP_RANK_TN,16).
-define(GAIN_STONE_STAR_N_TN,17).
-define(FORMATION_GER_STONE_TN,18).
-define(FIGHTPOWER,19).
-define(PVP_RANK_N,20).
-define(CHALLENGE_KILL_BOSS_N,21).
-define(CARLOS_WIN_N,22).
-define(RELIC_DIFFICULTY_N_WIN_N,23).
-define(HRON_REACH_N,24).
-define(PLANTATION_HAS_N,25).
-define(CONQUERISLAND_BOSS_DEMAGE_TN,26).
-define(TWINS_DIFFICULTY_N_WIN_N,27).
-define(DOUBLEMATCH_RANK_N,28).
-define(PLANE_LEVEL_N,29).

%%主线任务新增的任务类型
-define(FORMATION_GER_LEVEL_N_TN,30).
-define(FORMATION_GER_EQUIP_RANK_N_TN_TN,31).
-define(FORMATION_GER_EQUIP_LEVEL_N_TN_TN,32).
-define(GER_OWN_STAR_N_QUALITY_N_ISMEGA_TN,33).

%%训练师职业增加的职业类型
-define(FIGHT_TRAINER_TYPE,1).      %% 战斗训练师
-define(REAR_TRAINER_TYPE,2).       %% 饲育训练师
-define(COORDINATE_TRAINER_TYPE,3). %% 调整者

-define(TRUE, true).
-define(FALSE, false).