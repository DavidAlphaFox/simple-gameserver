-define(PRELOAD_CONFIG, [ 
%% 服务器通用配置
{"config/moduleconfig/data_common.config",data_common, key_value, original},

%% 运维配置
{"setting/setting.config", data_setting, key_value, original},

%% 玩家经验配置
{"config/moduleconfig/data_role_exp.config",data_role_exp, if_clause, original},
fun tk_config:load_data_role_level/0,

%% 武将经验配置
{"config/moduleconfig/data_ger_exp.config",data_ger_exp, if_clause, original},
fun tk_config:load_data_ger_level/0,
{"config/moduleconfig/data_ger_exp_light.config",data_ger_exp_light, if_clause, original},
fun tk_config:load_data_ger_light_level/0,

%% 武将突破配置
{"config/moduleconfig/data_ger_breakthrough.config", data_ger_breakthrough, key_value, original},

%%装备精炼加成
{"config/moduleconfig/data_rankaddation.config",data_rankaddation,key_value,original},

%% 招财配置
{"config/moduleconfig/data_coin_buy.config", data_coin_buy, key_value, original},

%% 武将技能配置
{"config/moduleconfig/data_skill.config", data_skill, key_value, original},

%% 武将配置
{"config/moduleconfig/data_ger.config",data_ger, key_value, original},

%% 武将天命配置
{"config/moduleconfig/data_destiny.config",data_destiny,key_value,original},
fun tk_config:load_data_destiny_rela/0,

%% 体力类道具使用信息配置
{"config/moduleconfig/data_item_use.config",data_item_use, key_value, original},

%% 战役和探索功能的章节配置
{"config/moduleconfig/data_chapter.config",data_chapter, key_value, original},

%% 战役基础配置
{"config/moduleconfig/data_battle_setting.config",data_battle_setting, key_value, original},

%% 冒险额外掉落活动
{"config/moduleconfig/data_trainer.config",data_trainer, key_value, original},

%% 等级段属性养成的配置
{"config/moduleconfig/data_lvl_attr_role.config", data_lvl_attr_role, key_value, original},
{"config/moduleconfig/data_lvl_attr_ger.config", data_lvl_attr_ger, key_value, original},
{"config/moduleconfig/data_lvl_attr_equip.config", data_lvl_attr_equip, key_value, original},

%% 精灵觉醒
{"config/moduleconfig/data_awake.config",data_awake,key_value,original},

%%技能宝石
{"config/moduleconfig/data_diamond.config",data_diamond,key_value,original},

%% 任务跑环
{"config/moduleconfig/data_tasklink.config",data_tasklink,key_value,original},

%% 天赋技能配置
{"config/moduleconfig/data_unique_append.config", data_unique_append, key_value, original},
{"config/moduleconfig/data_unique_effect.config", data_unique_effect, key_value, original},

%% 遭遇战和战役的关卡配置
{"config/moduleconfig/data_dungeon.config",data_dungeon, key_value, fun role_battle:format_config/1},

%% 怪物配置（未使用）
{"config/moduleconfig/data_mon.config",data_mon, key_value, fun ger_lib:data_mon_list2ger_list/1},

%% 武将可出战数量配置
{"config/moduleconfig/data_ger_num.config",data_ger_num, if_clause, original},

%% 武将变身后
{"config/moduleconfig/data_ger_name2.config",data_ger_name2, key_value, original},

%% VIP特权配置
{"config/moduleconfig/data_vip.config",data_vip, key_value, original},

%% 联盟配置
{"config/moduleconfig/data_family.config",data_family, key_value, original},

%% 道具配置
{"config/moduleconfig/data_item.config",data_item,key_value, original},

%% 道具配置
{"config/moduleconfig/data_homestead_seed.config",data_homestead_seed,key_value, original},

%% 合成配置
{"config/moduleconfig/data_combine.config",data_combine,key_value, original},
{"config/activityconfig/data_combine_crt.config",data_combine_crt,key_value, original},
{"config/moduleconfig/data_combine_random.config",data_combine_random,key_value, original},
{"config/moduleconfig/data_mirror.config", data_mirror, key_value, original},
{"config/moduleconfig/data_combine_mage.config",data_combine_mage, key_value, original},

%% 宝物的属性成长配置 
{"config/moduleconfig/data_treasure_value.config",data_treasure_value,key_value,fun item_lib:data_treasure_value_list2key_value_list/1},

%% 宝物经验配置
{"config/moduleconfig/data_treasure_exp.config",data_treasure_exp, if_clause, original},
fun tk_config:load_data_treasure_rank/0,
{"config/moduleconfig/data_treasure_exp2.config",data_treasure_exp2, if_clause, original},
fun tk_config:load_data_treasure_rank2/0,
{"config/moduleconfig/data_trea_equip_rule.config",data_trea_equip_rule, key_value, original},
%% 探索的随机配置
{"config/moduleconfig/data_encounter.config",data_encounter,if_clause, original},
{"config/moduleconfig/data_fixed_encounter.config",data_fixed_encounter,key_value, fun role_explore:fixed_encounter_config_va/1},

%% 商店配置
{"config/moduleconfig/data_shop.config",data_shop,key_value,original},
{"config/moduleconfig/data_sell.config",data_sell,key_value,original},
%% 奇遇、奇才商店配置
{"config/moduleconfig/data_shop_etc.config",data_shop_etc,key_value,original},


%% 官爵配置
{"config/moduleconfig/data_title.config",data_title,key_value,original},

%% 争霸排行榜配置
{"config/moduleconfig/data_pvp.config",data_pvp,if_clause,original},
%% 争霸排行榜的神秘宝箱配置
{"config/moduleconfig/data_pvp_mystery.config",data_pvp_mystery,key_value,original},

%% 关卡掉落配置
{"config/moduleconfig/data_drop.config",data_drop,key_value,original},

{"config/moduleconfig/data_dojang.config",data_dojang, key_value, original},

{"config/moduleconfig/data_dojangrank.config",data_dojangrank, key_value, original},

%% 宝物合成配置
{"config/moduleconfig/data_compose.config",data_compose,key_value,original},
fun tk_config:load_data_patch/0,

%% 随机名称配置(data_name.config)
fun tk_config:load_data_name/0,

%% 官爵奖励配置
{"config/moduleconfig/data_title_reward.config",data_title_reward,key_value,original},

%% 连续登录奖励配置
{"config/moduleconfig/data_login_reward.config",data_login_reward,key_value,fun role_daily:transform_list/1},

%% 装备强化配置
{"config/moduleconfig/data_reinforce.config",data_reinforce,key_value,original},

%% 装备强化训练师配置
{"config/moduleconfig/data_reinforce_trainer.config",data_reinforce_trainer,key_value,original},

%% 服务器公告配置
{"config/activityconfig/data_notice.config", data_notice, key_value, fun role_message:data_notice_transform_list/1},

%% 点将配置
{"config/activityconfig/data_card.config", data_card, key_value, original},
{"config/moduleconfig/data_fixed_card.config", data_fixed_card, key_value, original},

%% 武将升品配置
{"config/moduleconfig/data_ger_up_rank.config", data_ger_up_rank, key_value, original},
{"config/moduleconfig/data_ger_up_rank_light.config", data_ger_up_rank_light, key_value, original},

%% 道具品阶衰减配置
{"config/moduleconfig/data_item_decay.config", data_item_decay, key_value, original},

%% 道具升品配置
{"config/moduleconfig/data_item_up_rank.config", data_item_up_rank, key_value, original},

%% 传奇装备零散配置
{"config/moduleconfig/data_legendary.config", data_legendary, key_value, original},

%% 训练师装备升品配置
{"config/moduleconfig/data_item_trainer_up_rank.config", data_item_trainer_up_rank, key_value, original},

%% 符文配置
{"config/moduleconfig/data_item_stone.config", data_item_stone, key_value, original},
%% 符文升级配置
{"config/moduleconfig/data_stone_exp.config",data_stone_exp, key_value, original},
%% 符文经验转换配置
{"config/moduleconfig/data_stone_applyexp.config",data_stone_applyexp, key_value, original},
%% 符文争夺战配置
{"config/moduleconfig/data_stonechip_fight.config",data_stonechip_fight, key_value, original},
%% 符文的属性成长配置 
{"config/moduleconfig/data_stone_rank.config",data_stone_rank,key_value,fun item_lib:data_stone_rank_value_list2key_value_list/1},
%% 符文碎片掉落配置
{"config/moduleconfig/data_drop_stonechip.config",data_drop_stonechip,key_value,original},

%% 虎牢关配置
{"config/moduleconfig/data_hula.config", data_hula, key_value, fun hula_server:data_hula_transform_list/1},

%% 战南蛮配置
% {"config/data_nanm.config", data_nanm, key_value, fun nanm_server:data_nanm_transform_list/1},
{"config/moduleconfig/data_nanm.config", data_nanm, key_value, original},

%% 演示战斗（创角第一场战斗）
{"config/moduleconfig/data_demo_fight.config", data_demo_fight, key_value, original},%fun role_role:data_demo_fight_transform_list/1},
						
%% 华容道配置
{"config/moduleconfig/data_hron.config", data_hron, key_value, original},

%% 充值商店配置
{"config/moduleconfig/data_pay.config", data_pay, key_value, original},

%% 首充奖励配置
{"config/activityconfig/data_pay_reward.config", data_pay_reward, key_value, original},

%% 道具、武魂碎片合成
{"config/moduleconfig/data_compound.config", data_compound, key_value, original},

%% 微博绑定、微博分享、邀请码绑定、邀请者首充奖励
{"config/moduleconfig/data_invite.config", data_invite, key_value, original}, 

%% 探索常规奖励界面
{"config/moduleconfig/data_explore_reward.config", data_explore_reward, key_value, original},

%% 探索常规奖励倍数活动
{"config/activityconfig/data_explore_reward_crt.config",data_explore_reward_crt, key_value, original},

%% 冒险常规奖励倍数活动
{"config/activityconfig/data_dungeon_reward_crt.config",data_dungeon_reward_crt, key_value, original},

%% 冒险额外掉落活动
{"config/activityconfig/data_drop_bonus.config",data_drop_bonus, key_value, original},

%% 冒险额外掉落活动
{"config/activityconfig/data_canvass.config",data_canvass, key_value, original},

%% 升级礼包配置
{"config/moduleconfig/data_levelup_reward.config", data_levelup_reward, key_value, original},

%% 活动配置
{"config/activityconfig/data_activity.config", data_activity, key_value, original},

%% 宝箱配置
{"config/moduleconfig/data_box.config", data_box, key_value, fun role_box:config_format_va/1},
{"config/activityconfig/data_box_price.config", data_box_price, key_value, fun activity_server:load_box_price/1},
%% 付费元宝固定次数单抽宝箱
{"config/moduleconfig/data_fixed_gold_box.config", data_fixed_gold_box, key_value, original},
%% 赠送元宝固定次数单抽宝箱
{"config/moduleconfig/data_fixed_goldbonus_box.config", data_fixed_goldbonus_box, key_value, original},

%% 无模版邮件配置						
{"config/moduleconfig/data_temp_mail.config",data_temp_mail, key_value, original},

%% 卡牌分解规则配置
{"config/moduleconfig/data_card_split.config",data_card_split, key_value, original},

%% 聊天配置
{"config/moduleconfig/data_talk.config",data_talk, key_value, original},

%% 关键词配置
{"config/moduleconfig/data_words.config",data_words, key_value, fun role_talk:load_words/1},

%% 体力恢复活动配置					
{"config/moduleconfig/data_get_energy.config",data_get_energy, key_value, original},

%% 副将配置
{"config/moduleconfig/data_lieu_add.config",data_lieu_add,if_clause, original},
{"config/moduleconfig/data_lieu_clo_setting.config",data_lieu_clo_setting,key_value, original},
{"config/moduleconfig/data_lieu_open_charge.config",data_lieu_open_charge,if_clause, original},
{"config/moduleconfig/data_fixed_lieu_refresh.config",data_fixed_lieu_refresh,key_value, original},

%% 扫荡配置
{"config/moduleconfig/data_dungeon_raids.config",data_dungeon_raids,key_value, original},

%% 关卡配置信息
{"config/moduleconfig/data_battle_setting.config", data_battle_setting, key_value, original},
%% 华丽大赛配置信息
{"config/moduleconfig/data_race.config", data_race, key_value, original},

%% 华丽大赛配置信息
{"config/moduleconfig/data_race2.config", data_race2, key_value, original},

%% 异星战场配置信息
{"config/moduleconfig/data_alien.config", data_alien, key_value, original},

%% 3v3配置信息
{"config/moduleconfig/data_team_pk.config", data_team_pk, key_value, original},

%% 放鞭炮配置信息
{"config/activityconfig/data_fire.config", data_fire, key_value, original},

%% 汉帝宝库配置信息
{"config/activityconfig/data_fixed_treasure_free.config", data_fixed_treasure_free, key_value, original},
{"config/activityconfig/data_fixed_treasure_gold.config", data_fixed_treasure_gold, key_value, original},
{"config/activityconfig/data_treasure_box.config", data_treasure_box, key_value, original},
{"config/activityconfig/data_treasure_box_setting.config", data_treasure_box_setting, key_value, original},
{"config/activityconfig/data_treasure_box_baseReward.config", data_treasure_box_baseReward, if_clause, original},

%% 排行榜类活动配置
{"config/activityconfig/data_activityRank.config", data_activityRank, key_value, original},

%% 消费返利配置信息
{"config/activityconfig/data_rebate.config", data_rebate, key_value, original},

%%套装配置信息
{"config/moduleconfig/data_all_equipment.config",data_all_equipment,key_value, original},
%%召唤精灵和祈祷装备配置
{"config/moduleconfig/data_fixed_box_withitem.config",data_fixed_box_withitem,key_value,original},
%% 冲级活动配置
{"config/moduleconfig/data_levelRank.config", data_levelRank, key_value, original},
%% 抽卡暴击配置
{"config/activityconfig/data_box_crt.config", data_box_crt, key_value, original},

%% 联盟经验配置
{"config/moduleconfig/data_family_exp.config",data_family_exp, if_clause, original},

%% 联盟战争
{"config/baseconfig/data_family_fight.config", data_family_fight, key_value, original},
{"config/moduleconfig/data_family_fight_split.config", data_family_fight_split, if_clause, original},

%% 联盟福利
{"config/moduleconfig/data_family_daily_reward.config", data_family_daily_reward, key_value, original},

%% 联盟boss配置
{"config/moduleconfig/data_family_boss.config",data_family_boss, key_value, original},		

%%任务
{"config/moduleconfig/data_task.config",data_task,key_value,original},
{"config/moduleconfig/data_homestead.config",data_homestead,key_value,original},
{"config/moduleconfig/data_push.config",data_push,key_value,original},

%% 新家园
{"config/moduleconfig/data_home.config",data_home,key_value,original},

%% 新家园
{"config/moduleconfig/data_doublematch.config",data_doublematch,key_value,original},

%% 小伙伴配置
{"config/moduleconfig/data_partner.config", data_partner, key_value, original},
					
%% 合服配置
{"config/moduleconfig/do_merge.config", do_merge, key_value, original},

%%异星战场配置
{"config/baseconfig/data_alien_distribute.config", data_alien_distribute, key_value, original},

%%异星总决赛配置
{"config/moduleconfig/data_alien_finals.config", data_alien_finals, key_value, original},

%%合服后的ID映射
{"config/baseconfig/data_serverID_map.config", data_serverID_map , key_value, original},

%%改名的配置
{"config/moduleconfig/data_change_name.config", data_change_name, key_value, original},

%%公会科技的配置
{"config/moduleconfig/data_family_technology.config", data_family_technology, key_value, original},

%%喇叭的配置
{"config/moduleconfig/data_trumpet.config", data_trumpet, key_value, original},

%%喇叭的配置
{"config/activityconfig/data_redpacket.config", data_redpacket, key_value, original},

%% 天赋
{"config/moduleconfig/data_talent.config",data_talent, key_value, original},

%%战斗顺序的配置
{"config/moduleconfig/data_fight_order.config", data_fight_order, key_value, original},

%%分解系统的配置
{"config/moduleconfig/data_equip_decompose.config", data_equip_decompose, key_value, original},

%%战斗相关配置
{"config/moduleconfig/data_fight.config", data_fight, key_value, original},

%% 限时打折活动配置
{"config/activityconfig/data_discount.config",data_discount,key_value,original},

%% 全区抢购活动配置
{"config/activityconfig/data_panic_buy.config",data_panic_buy,key_value,original},

%% 服务器名称对应的id配置
{"config/baseconfig/data_svrname_map.config", data_svrname_map, key_value, original},
        
%% 绝技附加效果的配置



{"config/moduleconfig/data_talent_skill.config",data_talent_skill, key_value, original},

%% 冒险额外掉落活动
{"config/moduleconfig/data_trainer.config",data_trainer, key_value, original},
						
{"config/activityconfig/data_monthVIP.config", data_monthVIP, key_value, original},
{"config/moduleconfig/data_talent_skill.config",data_talent_skill, key_value, original},
{"config/moduleconfig/data_team.config",data_team,key_value,original},

% 关卡得星兑换奖励
%{"config/moduleconfig/data_chapter_star_reward.config",data_chapter_star_reward,key_value, fun role_battle:load_star_reward_config/1 },

% 卡洛斯的配置
{"config/baseconfig/data_carlos.config",data_carlos,key_value,original},
% AI参数配置
{"config/baseconfig/data_plane_ai.config",data_plane_ai,key_value,original},
% 卡拉狄加的配置
{"config/baseconfig/data_galactica.config",data_galactica,key_value,original},
% 双子配置
{"config/baseconfig/data_twins.config",data_twins,key_value,original},
% 巨龙遗迹的配置
{"config/baseconfig/data_relic.config",data_relic,key_value,original},

% 幸运转盘
{"config/moduleconfig/data_fixed_lucky_roll_gold.config", data_fixed_lucky_roll_gold,key_value, original},
{"config/moduleconfig/data_fixed_lucky_roll_free.config", data_fixed_lucky_roll_free,key_value, original},
{"config/moduleconfig/data_fixed_lucky_roll_inner_gold.config", data_fixed_lucky_roll_inner_gold, key_value, original},
{"config/moduleconfig/data_fixed_lucky_roll_inner_free.config", data_fixed_lucky_roll_inner_free, key_value, original},
{"config/activityconfig/data_lucky_roll.config", data_lucky_roll, key_value, origonal},
{"config/activityconfig/data_lucky_roll_setting.config", data_lucky_roll_setting, key_value, original},
{"config/activityconfig/data_activity_lucky.config", data_activity_lucky, key_value,original},

% vip商店
{"config/activityconfig/data_activity_vip.config",data_activity_vip,key_value,original},

% 魔典配置
{"config/moduleconfig/data_magic_book.config",data_magic_book,key_value,original},
{"config/moduleconfig/data_magic_picture.config",data_magic_picture,key_value,original},
{"config/moduleconfig/data_item_enchant.config",data_item_enchant,key_value,original},

%% 神兵天降
{"config/activityconfig/data_tvcard.config",data_tvcard,key_value,original},

%% 训练师专精
{"config/moduleconfig/data_trSpecial.config",data_trSpecial, key_value, original} ,

%% 精灵觉醒技能
{"config/moduleconfig/data_awake_skill.config",data_awake_skill,key_value,original},   
%% 数据库分区配置
{"config/moduleconfig/data_db_partition.config",data_db_partition,key_value,original},
%% quick渠道映射配置
{"config/baseconfig/data_quick.config",data_quick,key_value,original},
%% 签到配置
{"config/moduleconfig/data_sign.config",data_sign,key_value,original},

{"config/moduleconfig/data_box_more.config",data_box_more,key_value,original},
%%精华符文配置
{"config/moduleconfig/data_essence_stone_exp.config",data_essence_stone_exp,key_value,original},

% 定向获得装备配置
{"config/moduleconfig/data_item_x_get.config",data_item_x_get, key_value, original},
%% 道具完美升品
{"config/moduleconfig/data_item_pft_uprank.config",data_item_pft_uprank, key_value,original},

%% 皮肤配置
{"config/moduleconfig/data_skin.config",data_skin, key_value,original},

%% 皮肤效果配置
{"config/moduleconfig/data_skin_effect.config",data_skin_effect, key_value,original},

%% 赏金副本配置
{"config/moduleconfig/data_bounty.config",data_bounty, key_value,original},

%% 砸金蛋配置
{"config/activityconfig/data_goldenegg.config",data_goldenegg, key_value,original},

%% 双排配置
{"config/moduleconfig/data_doublematch.config",data_doublematch, key_value,original},

%% 首冲活动
{"config/moduleconfig/data_first_pay.config", data_first_pay, key_value, original},

%% 家园boss
{"config/moduleconfig/data_home_boss.config",data_home_boss, key_value,original},

%% 晶体配置
{"config/moduleconfig/data_crystal.config",data_crystal, key_value,original},

%% 符文精炼
{"config/moduleconfig/data_stone_level.config",data_stone_level, key_value,original},

%% 资源返还
{"config/moduleconfig/data_recycle.config",data_recycle, key_value,original},

%% 剧情信息
{"config/moduleconfig/data_drama.config",data_drama, key_value,original},

%% 等级上阵配置
{"config/moduleconfig/data_stand_fighters.config",data_stand_fighters, if_clause, original},

%% 支付引导
{"config/moduleconfig/data_payGuide.config",data_payGuide,key_value,original},

%% 二次进化
{"config/moduleconfig/data_second_evolution.config",data_second_evolution, key_value,original},

%% 关卡BOSS宝箱配置
{"config/moduleconfig/data_battle_boss_reward.config",data_battle_boss_reward, key_value,original},

%% 阿努比斯配置
{"config/moduleconfig/data_anubis.config",data_anubis, key_value,original},
{"config/baseconfig/data_family_cross.config",data_family_cross, key_value,original},
{"config/baseconfig/data_family_cross_fight_nodes.config",data_family_cross_fight_nodes, key_value,original},
{"config/moduleconfig/data_family_cross_fight_split.config",data_family_cross_fight_split, if_clause,original},

%% 关卡征服之岛配置
{"config/baseconfig/data_conquerisland.config",data_conquerisland, key_value,original},
%%加速装备配置
{"config/moduleconfig/data_accelerate_equip.config",data_accelerate_equip,key_value,original},
%点券抽卡
{"config/moduleconfig/data_fixed_ticket_box.config",data_fixed_ticket_box, key_value,original},
{"config/moduleconfig/data_ticket_box.config",data_ticket_box, key_value,original},
%%消费返利
{"config/activityconfig/data_consume_back.config",data_consume_back,key_value,original},

%节日活动
{"config/activityconfig/data_activityFestival.config",data_activityFestival, key_value,original},

%%技能宝石技能
{"config/moduleconfig/data_diamond_skill.config",data_diamond_skill,key_value,original},
%% exBoss
{"config/moduleconfig/data_exBoss.config",data_exBoss,key_value,original},
%% svip
{"config/moduleconfig/data_svip.config",data_svip,key_value,original},
%%经验果实配置
{"config/moduleconfig/data_fruit.config",data_fruit,key_value,original},
%% xbattle 
{"config/moduleconfig/data_xbattle_chapter.config",data_xbattle_chapter, key_value, original},
{"config/moduleconfig/data_xbattle_dungeon.config",data_xbattle_dungeon, key_value, fun role_xbattle:load_data_xbattle_dungeon/1},
{"config/moduleconfig/data_xbattle_drop.config", data_xbattle_drop, key_value, original},
{"config/moduleconfig/data_xbattle.config", data_xbattle, key_value, original},

%%HeadSeven活动配置
{"config/moduleconfig/data_headSeven.config",data_headSeven,key_value,fun role_payGuide:deal_headSeven_config/1},

%%新图鉴配置
{"config/moduleconfig/data_manual.config",data_manual,key_value,fun role_payGuide:deal_headSeven_config/1},

%%
{"config/moduleconfig/data_maintask.config",data_maintask,key_value,fun role_payGuide:deal_headSeven_config/1},

%%聚宝盆配置
{"config/activityconfig/data_treasurebowl.config",data_treasurebowl,key_value,fun role_payGuide:deal_headSeven_config/1},
%%训练师职业配置
{"config/moduleconfig/data_trainerProf.config",data_trainerProf,key_value,fun role_payGuide:deal_headSeven_config/1},
%%训练师培育室配置
{"config/moduleconfig/data_trainerRear.config",data_trainerRear,key_value,fun role_payGuide:deal_headSeven_config/1},
%战斗训练师
{"config/moduleconfig/data_trainer_battle.config", data_trainer_battle, key_value, original},

% 签到
{"config/moduleconfig/data_dSign.config", data_dSign, key_value, original},
%%训练室
{"config/moduleconfig/data_train_room.config", data_train_room, key_value, original},
%%训练室
{"config/activityconfig/data_consumerank.config", data_consumerank, key_value, original}
]).

