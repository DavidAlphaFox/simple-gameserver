
alter table gRoleExtra ADD COLUMN `energyPac` INT UNSIGNED NOT NULL COMMENT '体力包' after `maintask`;
alter table gXbattleChapter add column `passCount` smallint unsigned not null comment '通过关卡统计' after `challengecount`;
ALTER TABLE `gBounty` ADD COLUMN `fighterData` LONGBLOB NULL AFTER `bountyData`;

DROP TABLE IF EXISTS `gDailySign`;
CREATE  TABLE IF NOT EXISTS `gDailySign` (
	`roleID` INT UNSIGNED NOT NULL,
	`birthDay` INT UNSIGNED NOT NULL,
	`sevenContinue` INT UNSIGNED NOT NULL COMMENT '连续7天签到',
	`totalSigned` INT UNSIGNED NOT NULL COMMENT '历史签到总计',
	`lastMonthSignDate` INT UNSIGNED NOT NULL COMMENT '最后月累计签到时间',
	`lmReward` varbinary(500) not null ,
	`monthData` blob NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='签到';



DROP TABLE IF EXISTS `gtrainingroom`;
CREATE TABLE `gtrainingroom` (
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `chapterID` smallint(5) unsigned NOT NULL DEFAULT '0' COMMENT '对应章节ID',
  `leftHP` BIGINT UNSIGNED NOT NULL DEFAULT '0' COMMENT '稻草人当前剩余血量',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

alter table logWorldBoss_2017_11 add column `harm` bigint(20) unsigned not null comment '伤害' after `type`;
alter table logWorldBoss_2017_12 add column `harm` bigint(20) unsigned not null comment '伤害' after `type`;
alter table logWorldBoss_2018_1 add column `harm` bigint(20) unsigned not null comment '伤害' after `type`;
alter table logWorldBoss_2018_2 add column `harm` bigint(20) unsigned not null comment '伤害' after `type`;

alter table gFighterList add column `lieuAddAttr` varbinary(500) not null comment '小伙伴营地所有加成' after `trSpecial`;
alter table gFighterList modify column lieuInfoList varbinary(4000) not null comment '小伙伴营地数据';

alter table gBounty add column `last_login` int not null after alreadyBuyTimes;

delete from ghron;


alter table gCarlosPlaneInfo modify column validTime11 int(10) not null;
alter table gCarlosPlaneInfo modify column validTime12 int(10) not null;
alter table gCarlosPlaneInfo modify column validTime13 int(10) not null;
alter table gCarlosPlaneInfo modify column validTime14 int(10) not null;
alter table gCarlosPlaneInfo modify column validTime15 int(10) not null;
alter table gCarlosPlaneInfo modify column validTime16 int(10) not null;
alter table gCarlosPlaneInfo modify column validTime17 int(10) not null;
alter table gCarlosPlaneInfo modify column validTime18 int(10) not null;