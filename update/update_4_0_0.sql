
DROP TABLE IF EXISTS `gBoxInfo`;
CREATE  TABLE IF NOT EXISTS `gBoxInfo` (
  `roleID` INT UNSIGNED NOT NULL,
  `gerTime1` TINYINT UNSIGNED NOT NULL comment '已抽取免费道具抽取精灵次数',
  `gerTime2` TINYINT UNSIGNED NOT NULL comment '已抽取免费钻石抽取精灵次数',
  `itemTime1` TINYINT UNSIGNED NOT NULL comment '已抽取免费道具抽取装备次数',
  `itemTime2` TINYINT UNSIGNED NOT NULL comment '已抽取免费钻石抽取装备次数',
  `trainerTime1` TINYINT UNSIGNED NOT NULL comment '已抽取免费道具抽取训练师次数',
  `trainerTime2` TINYINT UNSIGNED NOT NULL comment '已抽取免费钻石抽取训练师次数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='免费抽卡信息';

ALTER TABLE `gRole` ADD COLUMN `sGoldTotalPaid` INT UNSIGNED NOT NULL comment 'svip充值记录' AFTER `transmigration`;
ALTER TABLE `gRole` ADD COLUMN `svipLevel` tinyINT UNSIGNED NOT NULL comment 'svip level' AFTER `sGoldTotalPaid`;

ALTER TABLE `gExBoss` ADD COLUMN `cdduration` smallint UNSIGNED default 600 NOT NULL COMMENT 'cdduration' after `reward`;

ALTER TABLE `gFamilyMember` ADD COLUMN `vip` smallint UNSIGNED NOT NULL COMMENT 'vip info' after `anubisinfo`;


DROP TABLE IF EXISTS `gXBattle`;
CREATE  TABLE IF NOT EXISTS `gXBattle` (
  `roleID` INT UNSIGNED NOT NULL,
  `nowChapterID` INT UNSIGNED NOT NULL COMMENT '当前关卡ID',
  `quickFightSec` INT UNSIGNED NOT NULL COMMENT '快速战斗开始时间',
  `buyQuickCount` INT UNSIGNED NOT NULL COMMENT '购买的快速战斗次数',
  `buyQuickToday` SMALLINT UNSIGNED NOT NULL COMMENT '今天购买的快速战斗次数',
  `lastTriggerSec` INT UNSIGNED NOT NULL COMMENT '上次触发事件时间点',
  `challengeCount` INT UNSIGNED NOT NULL COMMENT '挑战了多少次',
  `raidCount` INT UNSIGNED NOT NULL COMMENT '快速探索了多少次',
  `passData` varbinary(500) not null comment '关卡通过序列',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='战役';

DROP TABLE IF EXISTS `gXbattleChapter`;
CREATE  TABLE IF NOT EXISTS `gXbattleChapter` (
  `roleID` INT UNSIGNED NOT NULL,
  `chapterID` INT UNSIGNED NOT NULL COMMENT '当前关卡ID',
  `passDungeons` VARBINARY(200) NOT NULL COMMENT '当前关卡通过信息',
  `isGetReward` tinyINT UNSIGNED NOT NULL COMMENT '是否领取通关奖励',
  `challengeCount` INT UNSIGNED NOT NULL COMMENT '关卡挑战次数' ,
  PRIMARY KEY (`roleID`,`chapterID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='战役chapter';

DROP TABLE IF EXISTS `gXBattleGerDisplay`;
CREATE  TABLE IF NOT EXISTS `gXBattleGerDisplay` (
  `roleID` INT UNSIGNED NOT NULL,
  `displayData` VARBINARY(1000) NOT NULL COMMENT '解锁精灵信息',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='战役图鉴';

DROP TABLE IF EXISTS `gHeadSeven`;
CREATE  TABLE IF NOT EXISTS `gHeadSeven` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `doingTask` blob NOT NULL COMMENT '正在进行的任务',
  `finishTask` blob NOT NULL COMMENT '已经完成的任务未领取奖励',
  `finishRewardTask` VARBINARY(100) NOT NULL COMMENT '已经完成奖励领取',
  `period` SMALLINT NOT NULL COMMENT '玩家当前所属的阶段',
  `beginTimeStamp` INT UNSIGNED NOT NULL COMMENT '任务开始时间',
  `isFirst` TINYINT NOT NULL DEFAULT 1 COMMENT '玩家是否首次登陆',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='七日计划';

 delete from gTask where taskID >= 20085 and taskID <= 20164;
 delete from gTask where taskID >= 20195 and taskID <= 20214;
 
 delete from gBounty;
 alter table gBounty modify column type int(10) unsigned not null comment '上次更新时间';

 delete from gGuide;
 
 DROP TABLE IF EXISTS `gManual`;
 CREATE  TABLE IF NOT EXISTS `gManual` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `manual` blob NOT NULL COMMENT '玩家新图鉴信息',
  PRIMARY KEY (`roleID`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='新图鉴';

 ALTER TABLE `gRoleExtra` ADD COLUMN `maintask` varbinary(200)  NOT NULL COMMENT '玩家当前主线任务信息' after `plane_ai_flag`;
 