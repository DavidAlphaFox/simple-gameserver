
-define(ETS_HULA_BUFF_FLAG, ets_hula_buff_flag).% 虎牢关 擂鼓标识
-define(ETS_ETC, ets_etc).% 通用存储ets表
-define(ETS_HRON, ets_hron).% 华容道ets表
-define(ETS_RANDOM_SEED, ets_random_seed).% 用来存公共种子的ets
-define(ETS_ROLE_ONLINE, ets_role_online).%在线玩家列表
-define(ETS_STOP_FLAG, ets_stop_flag).     %停服标志
-define(ETS_ROLE_DUMP, ets_role_dump).%停服dump数据的玩家列表
-define(ETS_ROLE_PUBLIC, ets_role_public).%玩家共享的属性
-define(ETS_ROLE_LEVEL, ets_role_level).%检索好友列表用的共享数据，{RoleID，Level}
-define(ETS_DUNGEON_MON_CACHE, ets_dungeon_mon_cache). %关卡怪物的缓存
-define(ETS_ID,ets_id).% 生成ID的ets表
-define(ETS_NANM_BUFF_FLAG, ets_nanm_buff_flag).% 战南蛮 擂鼓标识
-define(ETS_FAMILY_REQUEST, ets_family_request).    %联盟请求加入的信息
-define(ETS_FAMILY_SUMMARY, ets_family_summary).    %联盟简略信息列表
-define(ETS_FAMILY_PROTECT, ets_family_protect).    %联盟处理的保护操作表，防止同时操作加入两个联盟的情况
-define(ETS_FAMILY_TECHNOLOGY,ets_family_technology).  %公会科技信息表

-define(ETS_HOMESTEAD_DATA_BASE_TABLE,ets_homestead_data_base_table).										%%家园基础数据缓存表
-define(ETS_HOMESTEAD_DATA_MACHINE_TABLE,ets_homestead_data_machine_table).						%%家园机器数据缓存表
-define(ETS_HOMESTEAD_DATA_LOG_TABLE,ets_homestead_data_log_table).											%%家园日志数据缓存表

-define(ETS_ENARGY_FRIEND_DATA,ets_enargy_friend_data).					%%好友赠送体力
-define(ETS_CACHE_STONECHIP_LIST, ets_stonechip_list).                 %% 缓存符文碎片信息
-define(ETS_CACHE_ROLE_PLUNDER, ets_role_plunder).                 %% 缓存玩家符文碎片争夺战信息

-define(ETS_WHISPER_NUM, ets_whisper_num).%保存未在线玩家积累的私聊数量，保存ets，减少查询数据库的次数,临时数据无需写入数据库

-define(ETS_ENERGY_ROLELIST, ets_energy_roleList).
-define(ETS_NODE_INFO_TABLE, ets_node_info_table). %% serverid - node
-define(ETS_NODE_INFO_TABLE2,ets_node_info_table2).%% all nodes

-define(ETS_FAMILY_INVITE, ets_family_invite).    %联盟邀请的信息
-define(ETS_CARLOS_INFO, ets_carlos_info). %% 卡洛斯进程信息
-define(ETS_CARLOS_ROLE_WAR, ets_carlos_role_war). %% 卡洛斯玩家战场信息
-define(ETS_CARLOS_SIGN, ets_carlos_sign).% 卡洛斯报名信息
-define(ETS_CARLOS_ROLETIMES,ets_carlos_roletimes).% 卡洛斯玩家才次数信息
-define(ETS_LUCKY_ROLL, ets_lucky_roll).%幸运大转盘信息
-define(ETS_VIP_ACTIVITY, ets_vip_activity).% vip打折活动信息
-define(ETS_GALACTICA_ROLE_WAR,ets_galactica_role_war).%卡拉狄加玩家战场信息
-define(ETS_GALACTICA_INFO,ets_galactica_info).%卡拉狄加进程信息
-define(ETS_GALACTICA_SIGN,ets_galactica_sign).%%卡拉狄加报名信息
-define(ETS_GALACTICA_ROLETIMES,ets_galactica_roletimes).%卡拉狄加玩家次数信息
-define(ETS_TWINS_ROLE_WAR, ets_twins_role_war).%双子玩家战场信息
-define(ETS_TWINS_INFO,ets_twins_info).%双子进程信息
-define(ETS_TWINS_SIGN,ets_twins_sign).%双子报名信息
-define(ETS_TWINS_ROLETIMES,ets_twins_roletimes).%双子玩家次数信息
-define(ETS_HOME_LIST, ets_home_list).                 %% 家园数据
-define(ETS_HOME_TASK_LIST, ets_home_task_list).                 %% 家园数据
-define(ETS_CACHE_CONFIG_ID,ets_cache_config_id). % 无需保存数据库，根据配置，按不同条件分类
-define(ETS_BOUNTY,ets_bounty).% 赏金副本信息

-define(ETS_HOME_DATA, ets_home_data).                  %% 家园全局数据，需要保存数据库

-define(ETS_DOUBLEMATCH_LINEUP_INFO,ets_doublematch_lineup_info). %%保存双排布阵信息
-define(ETS_CARLOS_REQUEST_TIME, ets_carlos_request_time). %% 保存玩家飞机两次的报名时间

% ----双排竞技场doublematch相关----
% 下面三个ets仅doublematch_server可写，其他进程只读
% 录像信息，仅以ID作为key,游戏服不保存数据，都在匹配服的ets写入数据库
-define(ETS_DM_FIGHT_REC,ets_dm_fight_rec).
% 排名数据，保存报名信息,数据不保存，重启时取消所有匹配，启动时默认都未匹配
-define(ETS_DM_SIGN_LIST,ets_dm_sign_list).
% 录像简报列表，不保存启动时从匹配服获取
-define(ETS_DM_REC_LIST,ets_dm_rec_list).
% 排名数据，不保存启动时从匹配服获取
-define(ETS_DM_RANK_DATA,ets_dm_rank_data).
% 往届排名数据，不保存启动时从匹配服获取
-define(ETS_DM_OLD_RANK,ets_dm_old_rank).

% 本服玩家的双排数据缓存，需要保存数据库
-define(ETS_DM_INFO_LIST,ets_dm_info_list).

-define(ETS_HOME_BOSS_TABLE,ets_home_boss_table).%% 家园boss数据
-define(ETS_FAMILY_CROSS_INFO,ets_family_cross_info).%% 联盟跨服系你想
-define(ETS_FAMILYCROSS_ROLE_WAR, ets_family_cross_role_war).%% 联盟跨服战场

-define(ETS_ANUBIS_RANK,ets_anubis_rank).%%阿努比斯赛季排名

-define(ETS_CONQUERISLAND_SIGN_INFO,ets_conquerisland_sign_info). %%征服之岛玩家报名信息
-define(ETS_CONQUERISLAND_WAR_INFO,ets_conquerisland_war_info). %%征服之岛战场信息
-define(ETS_CONQUERISLAND_ROLETIMES_INFO,ets_conquerisland_roletime_info). %%征服之岛玩家次数信息
-define(ETS_CONQUERISLAND_WAR_PID_MAP,ets_conquerisland_war_pid_map).

% 本服玩家的双排数据缓存，需要保存数据库
-define(ETS_AFK_RECORD,ets_afk_record).

%%保存玩家消费返利活动数据
-define(ETS_CONSUME_REABCK,ets_consume_reback).

%% 任务环数据
-define(ETS_TASKLINK_LIST,ets_tasklink_list).

%% 聚宝盆数据
-define(ETS_TREASURE_BOWL,ets_treasure_bowl).


%% 培育室数据
-define(ETS_TRAINERREAR,ets_trainerRear).

%% 道馆竞技
-define(ETS_DOJANGRANK_1,ets_dojangrank_1).
-define(ETS_DOJANGRANK_2,ets_dojangrank_2).
-define(ETS_DOJANGRANK_3,ets_dojangrank_3).
-define(ETS_DOJANGRANK_4,ets_dojangrank_4).
-define(ETS_DOJANGRANK_5,ets_dojangrank_5).
-define(ETS_DOJANGRANK_6,ets_dojangrank_6).
-define(ETS_DOJANGRANK_7,ets_dojangrank_7).
-define(ETS_DOJANGRANK_8,ets_dojangrank_8).

%% 跨服PVP
-define(ETS_DR_WORLD_1,ets_dr_world_1).
-define(ETS_DR_WORLD_2,ets_dr_world_2).
-define(ETS_DR_WORLD_3,ets_dr_world_3).
-define(ETS_DR_WORLD_4,ets_dr_world_4).
-define(ETS_DR_WORLD_5,ets_dr_world_5).
-define(ETS_DR_WORLD_6,ets_dr_world_6).
-define(ETS_DR_WORLD_7,ets_dr_world_7).
-define(ETS_DR_WORLD_8,ets_dr_world_8).

-define(ETS_CONSUMERANK,ets_consumerank).