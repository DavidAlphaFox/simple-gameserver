-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

%% =================================
%% dictionary key in role server
-define(roleInfo, roleInfo).
-define(roleID, roleID).
-define(macAddr,macAddr).%% 玩家客户端设备的物理地址
-define(ip, ip).
-define(ger, ger).
-define(gw, gw).
-define(socket,socket).
-define(batProg, batProg).	%% 战役进度
-define(chapter, chapter).	%% 战役章节信息
-define(posList, posList).	%% 站位信息
-define(fighterList, fighterList).%% 出战列表
-define(roleTimes, roleTimes).%% 体力相关
-define(encounterList, encounterList). %% 遭遇站列表
-define(encounterInfo, encounterInfo). %% 遭遇战详细信息
-define(bagEquip, bagEquip).%% 背包中的所有装备
-define(bagItem, bagItem).%% 背包中的非装备
-define(gerEquip, gerEquip).%% 获取武将的装备
-define(gerTypeID, gerTypeID). %% 武将模版ID
-define(shopNumList, shopNumList).%% 商店已购买次数
-define(itemUseList, itemUseList).  %% 道具使用信息列表
-define(posTypeList, posTypeList).%% 出站武将的模版ID列表
-define(roleFightPower, roleFightPower).%% 主公总战斗力
-define(pvp_rank, pvp_rank). % pvp排名
-define(dailyInfo, dailyInfo). % 每日奖励信息
-define(interval_persist_flag, interval_persist_flag).%定时持久化标识
-define(interval_persist_flag2, interval_persist_flag2).%定时持久化标识
-define(cardInfo, cardInfo). % 点将信息
-define(hronInfo, hronInfo). % 华容道信息
-define(limitInfo,limitInfo).% 限制开启信息
-define(gatherInfo,gatherInfo).% 图鉴信息
-define(mainGerTypeID, mainGerTypeID).% 核心武将的模版ID
-define(gerBag, gerBag). % 武将背包
-define(weiboShareList, weiboShareList).% 可分享微博的事件列表
-define(randomShopList, randomShopList).% 奇遇商店列表
-define(unioncoinShopList, unioncoinShopList).% 奇遇商店列表
-define(rolePatch, rolePatch). % 宝物碎片列表
%%4.1.0版本修改成以200作为Base值，并且小伙伴的pos位置只表示所属营地,使用200方便与旧的相区别，减少计算
-define(newlieuGerPos,200).
-define(lieuGerPos,100).
-define(lieutenantInfo,lieutenantInfo). % 参军的格子配置信息
-define(lieuPosList,lieuPosList). % 副将武将列表
-define(lieu_add_attr,lieu_add_attr). % 副将攻击血量加成信息
-define(posListT,posListT). % 带有副将格子加成信息的主将信息
-define(gag_list,gag_list). %屏蔽发言列表
-define(challengeGodPos,challengeGodPos). %神将录出战武将位置
-define(sign_emperor_info, sign_emperor_info). %拜帝王数据记录
-define(treahouseInfo, treahouseInfo). %汉帝宝库数据信息
-define(fireGold, fireGold). %玩家放鞭炮总共消费的元宝
-define(rebateInfo, rebateInfo). %玩家返利信息
-define(guideVal, guideVal). %玩家新手引导具体步骤的值
-define(teamPkInfo, teamPkInfo). % 3v3 数据
-define(alienInfo, alienInfo).      %异星战场数据
-define(whisper_list, whisper_list).    %私聊缓存数据
-define(roleMonthVIP, roleMonthVIP).    %% 月卡数据
-define(roleGerMirror, roleGerMirror).    %% 宠物魔镜数据
-define(magicBook_add_attr, magicBook_add_attr). %%魔典加成buffer
-define(magicBook_state, magicBook_state).  %% 魔典状态
-define(dojang_info, dojang_info).          %% 道馆

-define(ROLE_TASK_LIST,role_task_list).										%%玩家任务列表
-define(freeBoxInfo,freeBoxInfo). %% 免费抽卡相关数据
-define(currentPvpInterval,currentPvpInterval).%%当前竞技场次数恢复间隔
-define(currentEnergyIntercal,currentEnergyIntercal). %%当前体力次数恢复时间间隔
-define(currentPlunderInterval,currentPlunderInterval). %%当前符文争夺次数恢复时间间隔
-define(currentdiscoveryInterval,currentdiscoveryInterval). %%当前探索次数恢复时间间隔
-define(currentTeamPkIntercal,currentTeamPkIntercal). %%当前3v3次数恢复间隔

-define(talent_list, talent_list). % 天赋talent列表
-define(lucky_roll,lucky_roll).% 幸运转盘
-define(vip_shop, vip_shop). % vip 商店
-define(last_mail_ts,last_mail_ts).%% 最后发送邮件时间
-define(tvcard,tvcard).%%神兵天降数据
-define(trSpecial,trSpecial).%训练师
-define(sign_info,sign_info).%%签到信息
-define(growth_fund_record,growth_fund_record).%% 最后发送邮件时间
-define(bountyInfo,bountyInfo).
-define(bounty_fighters,bounty_fighters).
-define(mark_role_lvl_up_refresh, mark_role_lvl_up_refresh). %角色升级后刷新属性脏位
-define(plane_use_info,plane_use_info).%% 飞机使用信息
-define(last_request_fight_sec, last_request_fight_sec).% 挑战数据记录

-define(homeBossTimes,homeBoss_times).%% homeboss挑战次数记录
-define(shopBoxData,shopBoxData).% 商店抽卡信息

-define(battle_boss_reward_info,battle_boss_reward_info).%关卡boss奖励
-define(main_ger_typeid,main_ger_typeid).%主精灵ID
-define(payGuide,payGuide).% payGuide数据
-define(hronhistory,hronhistory).%%无尽深渊历史数据
-define(activityFestival,activityFestival).%节日活动
-define(role_tasklink,role_tasklink).
-define(exBoss_data, exBoss_data). % 
-define(xbattle_data, xbattle_data). % xbattle
-define(chapter_x, chapter_x).% xbattle_chapter
-define(xbattle_display, xbattle_display).%% xbattle ger view display
-define(plane_ai_flag, plane_ai_flag). % 
-define(tag_data,tag_data). % 队形信息
-define(headSeven,headSeven). %%头七信息
-define(familyTecDec,familyTecDec).%% xbattle familyTek dec
-define(maintask,maintask).%% main task current id
-define(role_equip_bag_full,role_equip_bag_full).
-define(canvass_info,canvass_info).%%民意调查填写结果
-define(dojangrank_data, dojangrank_data). % 
-define(trainerProf_battle, trainerProf_battle).%战斗训练师
-define(dSignData,dSignData).%签到
-define(signSec,signSec).
%% =================================

-define(sendself(Info), role_lib:send_self(Info)).

-define(route, route).

%% 获取#ger.gerBase的子属性的简写
-define(b(Ger, Field), (Ger#ger.gerBase#gerBase.Field)).

%% 获取#ger.gerAttr的子属性的简写
-define(a(Ger, Field), (Ger#ger.gerAttr#gerAttr.Field)).

-define(e(Ger, Field), (Ger#ger.gerExtra#data_ger.Field)).

-define(dict, process_dictionary).

-define(MAXSP, 100).

-define(SKILL_NEED_QUALITY_LIST,[-9999,-9999,2,5,10,15,20,25,30]).
-define(UNIQUE2_REPLACE_UNIQUE_NEED_QUALITY,12).
-define(UNIQUE3_REPLACE_UNIQUE2_NEED_QUALITY,25).

%% 特殊的ActionID
-define(ACTION_MISS, 1).		% 未命中
-define(ACTION_ABSORB, 2).		% 吸血
-define(ACTION_DAMAGE_BACK, 3).	% 反弹
-define(ACTION_NORMAL_HURT, 3). % 普通受伤动作
-define(ACTION_DEAD, 4).		% 死亡
-define(ACTION_UNREEL, 5).		% 从眩晕状态中清醒(沉睡解除也用这个,但状态传天赋的状态)
-define(ACTION_REBORN, 9).		% 复活
-define(ACTION_LOOP_START, 10). % 回合开始
-define(ACTION_DAMAGE_PLUS_RMV, 11). %%坚韧效果去除
-define(ACTION_INVINCIBLEP_RMV, 12). %%无敌效果去除
-define(ACTION_DOT_RMV,13).     % 流血效果去除(天赋状态)
-define(ACTION_LINK_RMV,14).    % 取消伤害链接(天赋状态)
-define(ACTION_UNSLEEP,15).     % 取消沉睡(天赋状态)
-define(ACTION_APPLY_TALENT, 127). %被天赋技能打中
-define(ACTION_UNFROZEN,41).    % 消除冰冻
-define(ACTION_UNPARALYSIS,43). % 消除麻痹 
-define(ACTION_RAGE_RMV, 139).  % 消除暴怒
-define(ACTION_WEARY_RMV, 140). % 消除疲惫
-define(ACTION_WEAK_RMV, 141).  % 消除脆弱
-define(ACTION_DS_RMV, 142).    % 消除伤害增益
-define(ACTION_ADP_RMV, 144).   % 消除伤害减益

-define(ACTION_TR_RIP,			70).% 破绽
-define(ACTION_TR_RIP2,			71).% 破绽受击
-define(ACTION_TR_RIP3,			72).% 破绽持续
-define(ACTION_TR_RIP4,			73).% 破绽解除
-define(ACTION_TR_COOL,			74).% 冷静
-define(ACTION_TR_THIRTY,		75).% 嗜血
-define(ACTION_TR_THIRTY2,		76).% 嗜血受击
-define(ACTION_TR_THIRTY3,		77).% 嗜血持续
-define(ACTION_TR_THIRTY4,		78).% 嗜血解除
-define(ACTION_TR_STONE,		79).% 石化
-define(ACTION_TR_STONE2,		80).% 石化受击
-define(ACTION_TR_STONE3,		81).% 石化持续
-define(ACTION_TR_STONE4,		82).% 石化解除
-define(ACTION_TR_KNOW,			83).% 会心
-define(ACTION_TR_KNOW2,		84).% 会心受击
-define(ACTION_TR_KNOW3,		85).% 会心持续
-define(ACTION_TR_KNOW4,		86).% 会心解除
-define(ACTION_TR_SNOW,			87).% 暴风雪
-define(ACTION_TR_SNOW2,		88).% 暴风雪释放
-define(ACTION_TR_SNOW3,		89).% 暴风雪受击
-define(ACTION_TR_AEROLITE,		90).% 陨石
-define(ACTION_TR_AEROLITE2,	91).% 陨石释放
-define(ACTION_TR_AREOLITE3,	92).% 陨石受击
-define(ACTION_TR_KILL,			93).% 斩杀
-define(ACTION_TR_KILL2,		94).% 斩杀释放
-define(ACTION_TR_KILL3,		95).% 斩杀受击
-define(ACTION_TR_HURT,			96).% 重伤
-define(ACTION_TR_HURT2,		97).% 重伤释放
-define(ACTION_TR_HURT3,		98).% 重伤受击

-define(ACTION_TR_COOL_ALL,		121).% 群体冷静
-define(ACTION_TR_THIRTY_ALL,	122).% 群体嗜血
-define(ACTION_TR_STONE_ALL,	123).% 群体石化
-define(ACTION_TR_KNOW_ALL,		124).% 群体会心



%% #p_action.state中的位值
-define(STATE_DEFAULT, 		2#00000000000000).% 无状态
-define(STATE_CRIT, 		2#00000001000000).% 暴击
-define(STATE_REEL, 		2#00000000100000).% 击晕
-define(STATE_DEAD, 		2#00000000010000).% 死亡
-define(STATE_ABSORB, 		2#00000000001000).% 吸血
-define(STATE_DAMAGE_BACK, 	2#00000000000100).% 反弹
-define(STATE_TALENT,       2#00000000000010).%天赋技能
-define(STATE_TRAINER,      2#00000010000000).%训练师技能
-define(STATE_TRAINER_CRIT, 2#00000100000000).%训练师技能暴击
-define(KINGDOM_QUN,1).
-define(KINGDOM_SHU, 2).
-define(KINGDOM_WEI, 3).
-define(KINGDOM_WU, 4).

-define(BEAT_ADD_SP, 50).%% 命中时攻击方与被攻击方增加的怒气
-define(HURT_ADD_SP, 25).%% 未命中时攻击方与被攻击方增加的怒气

-define(action(ActionID, Pos, TargetPos, AddHp, AddSp, State), #p_action{actionID=ActionID,
																		 gerPos=Pos,
																		 targetPos=TargetPos,
																		 addHp=AddHp,
                                                                         addProHp=0,
																		 addSp=AddSp,
																		 state=State}).

-define(pro_action(ActionID, Pos, TargetPos, AddHp, AddProHp, AddSp, State), #p_action{actionID=ActionID,
																		 gerPos=Pos,
																		 targetPos=TargetPos,
																		 addHp=AddHp,
                                                                         addProHp=AddProHp,
																		 addSp=AddSp,
																		 state=State}).
																		 
-define(miss_action(Ger, AddSp), #p_action{actionID=?ACTION_MISS,
									gerPos=?b(Ger,gerPos),
									state=?STATE_DEFAULT,
									addHp=0,
                                    addProHp=0,
									targetPos=[],
									addSp=AddSp}).


%% 用来测试的攻击者队伍
-define(test_attacker_list, [
#ger{gerID=1,gerBase=#gerBase{gerTypeID=1,gerPos=1},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=2,gerBase=#gerBase{gerTypeID=2,gerPos=2},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=3,gerBase=#gerBase{gerTypeID=3,gerPos=3},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=4,gerBase=#gerBase{gerTypeID=4,gerPos=4},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=5,gerBase=#gerBase{gerTypeID=5,gerPos=5},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40},
#ger{gerID=6,gerBase=#gerBase{gerTypeID=6,gerPos=6},
	  gerAttr=#gerAttr{gerAttack=4000,gerHpMax=20000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=20000,gerSp=40}]).

%% 用来测试的防守者队伍
-define(test_defender_list, [
#ger{gerID=7,gerBase=#gerBase{gerTypeID=7,gerPos=1 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=8,gerBase=#gerBase{gerTypeID=8,gerPos=2 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=9,gerBase=#gerBase{gerTypeID=9,gerPos=3 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=10,gerBase=#gerBase{gerTypeID=10,gerPos=4 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=11,gerBase=#gerBase{gerTypeID=11,gerPos=5 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40},
#ger{gerID=12,gerBase=#gerBase{gerTypeID=12,gerPos=6 },
	  gerAttr=#gerAttr{gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
	  gerHp=2000,gerSp=40}]).							

			
%% ra = role_attribute
-define(ra_level(V), #sc_role_update_level{level=V}).
-define(ra_exp(V),	#sc_role_update_exp{exp=V}).
-define(ra_coin(V),#sc_role_update_coin{coin=V}).
-define(ra_reputation(V),#sc_role_update_reputation{reputation=V}).
-define(ra_gold(V),#sc_role_update_gold{gold=V}).
-define(ra_goldBonus(V),#sc_role_update_goldBonus{goldBonus=V}).
-define(ra_goldUsed(V),#sc_role_update_goldUsed{goldUsed=V}).
-define(ra_vipLevel(V, V1, V2),#sc_role_update_vipLevel{vipLevel=V, challengeGodFree=V1, challengeGodBuy=V2}).
-define(ra_energy(V,V2), #sc_role_update_energy{energy=V,nextEnergyTime=V2}).
-define(ra_dscv(V,V2), #sc_role_update_discoveryTimes{discoveryTimes=V,nextDscvTime=V2}).
-define(ra_alien(V,V2), #sc_alien_update_times{leftTimes=V,timestamp=V2}).
-define(ra_title(V), #sc_role_update_title{title=V}).
-define(ra_pvpTimes(V,V2), #sc_role_update_pvpTimes{pvpTimes=V, nextPvpTime=V2}).
-define(ra_plunderTimes(V,V2), #sc_role_update_plunderTimes{plunderTimes=V, nextPlunderTime=V2}).
-define(ra_encounterFreeNum(V), #sc_role_update_encounterFreeNum{encounterFreeNum=V}).
-define(ra_weiboCount(V), #sc_role_update_weiboCount{weiboCount=V}).
-define(ra_unioncoin(V),#sc_role_update_unioncoin{unioncoin=V}).
-define(ra_remains_point(V), #sc_role_talent_remains_point{remains_point=V}).
-define(ra_profoundCrystal(V),#sc_role_update_profoundCrystal{profoundCrystal=V}).
-define(ra_honor(V),#sc_role_update_honor{honor=V}).
-define(ra_pvppoint(V),#sc_role_update_pvppoint{pvppoint=V}).
-define(ra_teamPkTimes(V1,V2), #sc_role_update_teamPkTimes{teamPkTimes=V1,nextTeamPkTime=V2}).
-define(ra_home_resource(V),#sc_role_update_home_resource{home_resource=V}).
-define(ra_ticket(V),#sc_role_update_ticket{ticket=V}).
-define(ra_laputastone(V),#sc_role_update_laputastone{laputastone=V}).
-define(notify_update(Record), ?sendself(Record)).


-define(change_pos(Ger, NewPos), (Ger#ger{gerBase=((Ger#ger.gerBase)#gerBase{gerPos=NewPos})})).

-define(MAX_GER_RANK, 20).
-define(MAX_GER_SECOND_RANK,30).
-define(MIN_GER_SECOND_RANK,20).

-define(GATHER_TYPE_GER,    1).    %% 武将图鉴
-define(GATHER_TYPE_ITEM,   2).    %% 道具图鉴
-define(GATHER_TYPE_EQUIP,  3).    %% 装备图鉴

-define(SHAPE_BASE, 10000000).


-define(BATTLE_DUNGEON_TYPE_NORMAL,1).			%%普通关卡
-define(BATTLE_DUNGEON_TYPE_HARD,2).					%%困难关卡
-define(BATTLE_DUNGEON_TYPE_FAST_HARD,3).		%%最困难关卡
-define(BATTLE_DUNGEON_TYPE_TRANSMIGRATION,4).       %%转生后的关卡

-define(CHANGE_SELF_NAME,1).       %% 改自己名字
-define(CHANGE_FAMILY_NAME,2).     %% 改公会名字

-define(SHOP_ID_HONOR, 50001).      %荣誉点商店
-define(SHOP_ID_PVPPOINT, 20000).   %竞技场点数商店
-define(SHOP_ID_FAMILY_FIXED, 21000).   %公会固定商店

-define(MAX_MAGIC_BOOK_NUM, 7).          %% 魔典个数
-define(MAX_MAGIC_PICTURE_POS, 3).               %% 图鉴位置个数

-define(ITEM_REDPACKET_ID, 20052).      %% 红包代币


