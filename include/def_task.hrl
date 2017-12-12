
-define(TASK_STATUS_NOT_ACCEPT,1).				%%未接
-define(TASK_STATUS_WAS_ACCEPT,2).				%%已接
-define(TASK_STATUS_FINISH,3).					%%已完成
-define(TASK_STATUS_COMMIT,4).					%%已提交(日常和成就)

-define(TASK_TYPE_MAIN,1).					%% 主线任务
-define(TASK_TYPE_TODAY,2).					%% 日常任务
-define(TASK_TYPE_ACH,3).					%% 成就
-define(TASK_TYPE_FAMILY_TODAY,4).          %% 公会日常任务
-define(TASK_TYPE_FAMILY_ACH,5).            %% 公会成就
-define(TASK_TYPE_FAMILY_WORSHIP, 6).       %公会膜拜任务

-define(TRIGGER_TASK_ID_LIST,trigger_task_id_list).
-define(TASK_CURR_ACH_TASKID,task_curr_ach_taskid).
-define(TASK_ACH_NEXT_TASKID,task_ach_next_taskid).

%%关卡类
-define(TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS,100).							%%普通关卡  (num=次数 int_list=[关卡id])																									ok
-define(TASK_TRIGGER_ID_DUNGEON_HARD_PASS,101).							%%困难关卡  (num=次数 int_list=[关卡id])																									ok
-define(TASK_TRIGGER_ID_DUNGEON_FAST_HARD_PASS,102).							%%最困难关卡  (num=次数 int_list=[关卡id])																									ok
-define(TASK_TRIGGER_ID_DUNGEON_PASS,103).							%%任意关卡  (num=次数 int_list=[关卡类型])  1：普通关卡  2：困难关卡  3：最困难关卡	
-define(TASK_TRIGGER_ID_CHAPTER_NORMAL_PASS,104).				%% 通过一个关卡			 (num=次数 int_list=[关卡id])																	ok	
-define(TASK_TRIGGER_ID_PLUNDER_FIGHT,115).							%%参加N次符文争夺战 (num=次数) 																								ok

%%喇叭
-define(TASK_TRIGGER_ID_LOCAL_TRUMPET,118).                         %%使用小喇叭
-define(TASK_TRIGGER_ID_CROSS_TRUMPET,119).                         %%使用大喇叭

% 精灵王
-define(TASK_TRIGGER_ID_EMPEROR,125).                         %%膜拜精灵王

% 体力
-define(TASK_TRIGGER_ID_GET_ENERGY,130).                         %%领取体力
-define(TASK_TRIGGER_ID_BUY_ENERGY,131).						%% 购买体力

% 充值
-define(TASK_TRIGGER_ID_PAY_GOLD, 140).							%% 充值

%消耗钻石
-define(TASK_TRIGGER_ID_USE_GOLD, 145).							%% 消耗钻石 (num=消耗量 )


%%怪物相关
-define(TASK_TRIGGER_ID_KILL_MONSTER,200).					%%击杀怪物 (num=击杀数量 int_list=[怪物id])																									ok
-define(TASK_TRIGGER_ID_GER_GAIN_GER,201).					%%获取武将 (num=数量 int_list=[武将id])																											ok
-define(TASK_TRIGGER_ID_GER_GAIN_STAR_GER,202).				%%获得星级武将 (num=数量 int_list=[星级])
-define(TASK_TRIGGER_ID_GER_UP_QUALITY_TIMES,203).		%%宠物升品次数 (num=次数 int_list=[怪物id])																								
-define(TASK_TRIGGER_ID_GER_UP_QUALITY,204).				%%将X只宠物升品到Y级别(num=数量 int_list=[等级])																						ok
-define(TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES,205).					%%升级X次宠物(num=次数 int_list=[怪物id])			

%%等级相关
-define(TASK_TRIGGER_ID_ROLE_UP_LEVLE,300).				%%角色升级到几级 (num=角色等级 int_list=[])																									ok
-define(TASK_TRIGGER_ID_GER_UP_LEVEL,301).					%%某只宠物升级到几级(num=宠物等级 int_list=[])																							ok




%%装备相关
-define(TASK_TRIGGER_ID_EQUIP_STRONG,400).					%%某个装备强化 (num=强化等级 int_list=[装备id] 如果int_list=[]则任意装备)												ok
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY,401).			%%某个装备升品 (num=品质 int_list=[装备id] 如果int_list=[]则任意装备)														ok
-define(TASK_TRIGGER_ID_EQUIP_UP_EQUIP,402).				%%穿上一定数量装备 (num=数量 int_list=[装备id] 如果int_list=[]则任意装备)													ok

%%合成时装备减少	
-define(TASK_TRIGGER_ID_EQUIP_TOTAL_EQUIP,403).		%%总计获得多少件装备 (num=数量 int_list=[装备id] 如果int_list=[]则任意装备)												ok
-define(TASK_TRIGGER_ID_EQUIP_GAIN_STAR_EQUIP,404).			%%获得X件Y星级的装备(num=数量 int_list=[星级])	

-define(TASK_TRIGGER_ID_EQUIP_STRONG_TIMES,405).	%%装备强化次数 (num=次数 int_list=[装备id] 如果int_list=[]则任意装备)														ok
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY_TIMES,406).	%%装备升品次数 (num=次数 int_list=[装备id] 如果int_list=[]则任意装备)													ok

-define(TASK_TRIGGER_ID_EQUIP_STRONG_1,407).				%%将X件装备强化到Y级别(num=数量 int_list=[等级])																						ok
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY_1,408).			%%将X件装备升阶到Y级别(num=数量 int_list=[等级])
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY_2,410).			%%将X件N星装备完美精炼到(num=数量 int_list=[星级,Rank])								ok
-define(TASK_TRIGGER_ID_EQUIP_UP_QUALITY_3,409).			%%将x件N星装备精炼到完美(num=数量 int_list=[星级])	


-define(TASK_TRIGGER_ID_EXPLORE_TIMES,500).					%%探索次数   (num=次数 int_list=[])

-define(TASK_TRIGGER_ID_COMBINE_TIMES,700).					%%合成次数  (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_COMBINE_NORMAL_TIMES,701).					%%配方合成次数  (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_COMBINE_RANDOM_TIMES,702).					%%随机合成次数  (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_COMBINE_GER_RANDOM_TIMES,703).			%%随机合成宠物次数 (num=次数 int_list=[])

%% 公会膜拜 精灵收集任务需要屏蔽掉的精灵ID列表
% -define(COLLECT_GER_MASK_TYPE_IDS, [0,7019,7029,7039,7049,7059,7069,7079,7089,7099,7109,10002,10000,10001,2,3,4,5,6,7,8,9]).
-define(COLLECT_GER_MASK_TYPE_IDS,data_family:get(collect_ger_mask_type_id)).
-define(TASK_TRIGGER_ID_FAMILY_WORSHIP_AUTOFINISH, 800).        %%公会膜拜自动完成任务
-define(TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_GER, 801).       %%公会膜拜收集精灵任务
-define(TASK_TRIGGER_ID_FAMILY_WORSHIP_COLLECT_EQUIP, 802).     %%公会膜拜收集装备任务
-define(TASK_TRIGGER_ID_FAMILY_WORSHIP_FIGHT, 803).             %%公会膜拜挑战钻石兽任务

-define(TASK_TRIGGER_ID_ADD_REWARD,1000).					%%获得东西     (num=数量 int_list=[类型]) 1: 银两  2: 元宝 3:声望    类型必须有
-define(TASK_TRIGGER_ID_ADD_REWARD_GOLD,1001).                   %%获得东西     (num=数量 int_list=[类型]) 1: 银两  2: 元宝 3:声望    类型必须有
-define(TASK_TRIGGER_ID_ADD_REWARD_REPU,1002).                   %%获得东西     (num=数量 int_list=[类型]) 1: 银两  2: 元宝 3:声望    类型必须有
-define(TASK_TRIGGER_ID_ADD_FRIEND_NUM,1004).			%%添加X个好友  (num=数量 int_list=[]) 


-define(TASK_TRIGGER_ID_EXTRACT_CARD_TIMES,3001).					%%抽卡N次 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_JOIN_HRON_TIMES,3002).							%%参加N次无尽深渊战斗 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_JOIN_HULA_TIMES,3003).							%%参加N次玲玲塔战斗 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_JOIN_NANM_TIMES,3004).						%%参加N次研究所战斗 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES,3005).				%%充能次数 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES,3006).						%%交配次数 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_GER_TIMES_BOTH,3007).                   %%分别抽精灵抽道具各一次 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_EXTRACT_CARD_TIMES_GOLD,3008).                   %%钻石抽卡N次 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_ENARGY_GIVE_REWARD,3009).                   %%赠送体力 (num=次数 int_list=[])


-define(TASK_TRIGGER_ID_ROLE_FIGHT_POWER,2001).						%%战斗力达到多少 (num=战斗力 int_list=[])
-define(TASK_TRIGGER_ID_ROLE_PVP_RANK,2002).							%%竞技场排名达到多少 (num=排名 int_list=[])
-define(TASK_TRIGGER_ID_PASS_HRON_DUNGEON,2003).				%%无尽深渊通过第N关卡 (num=关数 int_list=[])
-define(TASK_TRIGGER_ID_ALLEQUIPMENT,2004).								%%穿戴N件套 (num=件数 int_list=[])
-define(TASK_TRIGGER_ID_UP_GER_NUM,2005).									%%上阵武将个数 (num=个数 int_list=[])
-define(TASK_TRIGGER_ID_REPLACE_UP_GER_TIMES,2006).				%%进行N次换阵 (num=次数 int_list=[])
-define(TASK_TRIGGER_ID_ADD_DIFF_SEX_FRIEND,2007).					%%添加N名异性好友(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_CHAT_TIMES,2008).									%%聊天N次(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_SIGN_RACE_TIMES,2009).							%%华丽大赛报名次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_CHANGE_HEAD_TIMES,2010).					%%更换头像次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_PVP_FIGHT_TIMES,2011).							%%竞技场战斗次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_BUY_COIN_TIMES,2012).							%%招财次数(num=数量 int_list=[]) 
-define(TASK_TRIGGER_ID_ACTIVE_DESTINY_NUM,2013).					%%激活的天命条数(num=条数 int_list=[]) 

%%3v3相关
-define(TASK_TRIGGER_ID_FINISHED_3V3,2101).                  %%完成3v3战斗(num=需要完成的次数,int_list=[])
-define(TASK_TRIGGER_ID_3V3_RANK_CHANGE,2102).					%%3V3排名发生改变(num=最低奖励名次,int_list=[])

%%异星战场
-define(TASK_TRIGGER_ID_SIGN_ALIEN,2103).					%%报名参加异星战场(num=参加次数,int_list=[])
-define(TASK_TRIGGER_ID_BET_ALIEN,2104).					%%进行异星战场压注(num=压注次数,int_list=[])
-define(TASK_TRIGGER_ID_KILLED_ALIEN,2105).					%%异星战场连杀(num=连杀次数,int_list=[])

-define(TASK_TRIGGER_ID_AMIGO_BATTLE,2201).                  %%小伙伴上阵
-define(TASK_TRIGGER_ID_DMG_RANK_HULA,2202).                 %%挑战凤王中伤害排名
-define(TASK_TRIGGER_ID_DMG_RANK_NANM,2203).                 %%挑战超梦中伤害排名
-define(TASK_TRIGGER_ID_GEN_MATCH,2204).                     %%累计完成N次基因融合
-define(TASK_TRIGGER_ID_REFIE_EQUIP,2205).                   %%累计精炼N次X星装备(init_list=[星级])
-define(TASK_TRIGGER_ID_EVOLUTION_PET,2206).                 %%累计进化N次X星精灵(init_list=[星级])
-define(TASK_TRIGGER_ID_TRANSFORM_LVL1,2207).                %%将N只X星精灵一转(init_list=[星级])
-define(TASK_TRIGGER_ID_TRANSFORM_LVL2,2208).                %%将N只X星精灵二转(init_list=[星级])
-define(TASK_TRIGGER_ID_BOX_GER_10,2209).                    %%完成N次精灵10连抽
-define(TASK_TRIGGER_ID_BOX_EQUIP_10,2210).                  %%完成N次装备10连抽


-define(TASK_TRIGGER_ID_PLUNDER_FIGHT_ACH, 2211).            %%参加符文争夺成就
-define(TASK_TRIGGER_ID_GET_STONE, 2212).                    %%获得符文
-define(TASK_TRIGGER_ID_EAT_STONE, 2213).                    %%符文升级
-define(TASK_TRIGGER_ID_COMPOSE_STONE, 2214).                %%符文合成
-define(TASK_TRIGGER_ID_ALL_STONE_IN_POS, 2215).             %%上阵精灵装备N个符文
-define(TASK_TRIGGER_ID_STONE_TOTAL_STAR, 2216).             %%单个上阵精灵符文星级和
-define(TASK_TRIGGER_ID_STONE_MAX_LEVEL, 2217).              %%符文达到最大等级
-define(TASK_TRIGGER_ID_JOIN_WORSHIP_TASK, 2218).            %%参加公会占卜成就
-define(TASK_TRIGGER_ID_JOIN_FAMILY_FIGHT, 2219).            %%参加公会战
-define(TASK_TRIGGER_ID_GET_STAR, 2220).                     %%公会战中获得星星
-define(TASK_TRIGGER_ID_WIN_FAMILY_FIGHT, 2221).             %%公会战中获胜一次(主动)
-define(TASK_TRIGGER_ID_GET_ACTIVITY, 2222).                 %%获得活跃值
-define(TASK_TRIGGER_ID_BOX_TRAINER_10,2223).                %%完成N次训练师装备10连抽

-define(TASK_TRIGGER_ID_VIP_LEVEL, 2224).                    %%vip等级成就 
-define(TASK_TRIGGER_ID_TRANSFORM_LVL3, 2225).		         %%N只X星精灵三转(init_list=[星级])
-define(TASK_TRIGGER_ID_TRANSFORM_LVL4, 2226).		         %%N只X星精灵四转(init_list=[星级])


%%--------------公会任务----------------
-define(FAMILY_TASK_TRIGGER_ID_DUNGEON_NORMAL_PASS, 10001).     %%-公会-普通关卡  (num=次数)    
-define(FAMILY_TASK_TRIGGER_ID_GER_UP_LEVEL_TIMES,  10002).     %%-公会-升级X次宠物(num=次数)
-define(FAMILY_TASK_TRIGGER_ID_EVOLUTION_PET,       10003).     %%-公会-累计进化N次精灵
-define(FAMILY_TASK_TRIGGER_ID_EQUIP_STRONG_TIMES,  10004).     %%-公会-装备强化次数 (num=次数)                                                        ok
-define(FAMILY_TASK_TRIGGER_ID_REFIE_EQUIP,         10005).     %%-公会-累计精炼N次装备
-define(FAMILY_TASK_TRIGGER_ID_EXPLORE_TIMES,       10006).     %%-公会-探索次数   (num=次数)
-define(FAMILY_TASK_TRIGGER_ID_COMBINE_TIMES,       10007).     %%-公会-合成次数  (num=次数)
-define(FAMILY_TASK_TRIGGER_ID_EXTRACT_CARD_TIMES,  10008).     %%-公会-抽卡N次 (num=次数)
-define(FAMILY_TASK_TRIGGER_ID_JOIN_HRON_TIMES,     10009).     %%-公会-无尽深渊通过次数 (num=次数)
-define(FAMILY_TASK_TRIGGER_ID_JOIN_HULA_TIMES,     10010).     %%-公会-参加N次玲玲塔战斗 (num=次数)
-define(FAMILY_TASK_TRIGGER_ID_JOIN_NANM_TIMES,     10011).     %%-公会-参加N次研究所战斗 (num=次数 int_list=[])
-define(FAMILY_TASK_TRIGGER_ID_ADD_ENARGY_TO_FRIEND_TIMES,  10012). %%-公会-充能次数 (num=次数 int_list=[])
-define(FAMILY_TASK_TRIGGER_ID_MATING_TO_FRIEND_TIMES,      10013). %%-公会-交配次数 (num=次数 int_list=[])
-define(FAMILY_TASK_TRIGGER_ID_PVP_FIGHT_TIMES,     10014).     %%-公会-竞技场战斗次数(num=数量 int_list=[]) 
-define(FAMILY_TASK_TRIGGER_ID_PLUNDER_FIGHT, 10015).           %%公会符文争夺
-define(FAMILY_TASK_TRIGGER_ID_BUY_IN_SHOP, 10020).             %%在公会商店交易N次
-define(FAMILY_TASK_TRIGGER_ID_REFRESH_SHOP, 10021).            %%刷新N次商店

-define(FAMILY_TASK_TRIGGER_ID_FAMILY_HOME_SEED, 10022).				%% 全民种植
-define(FAMILY_TASK_TRIGGER_ID_FAMILY_HOME_BOSS, 10023).				%% 全民家园boss
-define(FAMILY_TASK_TRIGGER_ID_FAMILY_BUY_COIN, 10024).					%% 全民招财
-define(FAMILY_TASK_TRIGGER_ID_FAMILY_EMPEROR, 10025).					%% 全民膜拜
-define(FAMILY_TASK_TRIGGER_ID_FAMILY_SEND_ENERGY, 10026).				%% 全民赠送体力
-define(FAMILY_TASK_TRIGGER_ID_FAMILY_GET_ENERGY, 10027).				%% 全民领体力


%% 家园相关
-define(TASK_TRIGGER_ID_HOME_SEED,600).					%%家园种植
-define(TASK_TRIGGER_ID_HOME_BOSS,601).					%%家园Boss

%% 卡洛斯系统
-define(TASK_TRIGGER_ID_CARLOS, 900).					%% 卡洛斯
-define(TASK_TRIGGER_ID_GALACTICA,901).					%% 卡拉狄加
-define(TASK_TRIGGER_ID_RELIC, 902).					%% 巨龙
-define(TASK_TRIGGER_ID_TWINS, 903).					%% 双子

%% 双排
-define(TASK_TRIGGER_ID_DOUBLE_MATCH, 950).				%% 排位赛


