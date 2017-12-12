-- 保存玩家参加全区抢购活动信息
-- 限时打折活动信息
DROP TABLE IF EXISTS `gPanicBuyActivityInfo`;
CREATE TABLE `gPanicBuyActivityInfo` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `PanicBuyActivityInfo` varbinary(1000) NOT NULL COMMENT '玩家参加全区抢购活动信息',
  PRIMARY KEY (`roleID`),
  UNIQUE INDEX `roleID_UNIQUE` (`roleID` ASC)
)ENGINE = InnoDB DEFAULT CHARSET=utf8;

-- 增加字段，记录奥义结晶
ALTER TABLE gRole ADD COLUMN profoundCrystal INT NOT NULL DEFAULT '0'  COMMENT '奥义结晶' AFTER `unioncoin`;

DROP TABLE IF EXISTS `gMonthVIP`;
CREATE TABLE `gMonthVIP` (
  `roleID` int(11) UNSIGNED NOT NULL COMMENT '玩家ID',
  `buyBigSec` int(11) unsigned NOT NULL comment '购买大月卡时间',
  `buyLittleSec` int(11) unsigned not null comment '购买小月卡时间',
  `lastGetBigSec` int(11) unsigned not null comment '最后领取大月卡时间',
  `lastGetLittleSec` int(11) unsigned not null comment '最后领取小月卡时间',
  `restBigDays` int(11) unsigned not null comment '剩余大月卡领取时间',
  `restLittleDays` int(11) unsigned not null comment '剩余小月卡领取时间',
  `todayPayBig` tinyint(2) unsigned not null comment '今天是否购充值了大月卡额度',
  `todayPaylittle` tinyint(2) unsigned not null comment '今天是否充值小月卡额度',
  PRIMARY KEY (`roleID`)
)ENGINE = InnoDB DEFAULT CHARSET=utf8 comment '月卡信息';

-- DROP TABLE IF EXISTS `gOfflinePayMonthVIPLog`;
-- CREATE TABLE `gOfflinePayMonthVIPLog`(
-- 	`roleID` int(11) unsigned not null,
-- 	`payItemID` int(11) unsigned not null,
-- 	`srcType` tinyint(4) unsigned not null,
-- 	`receipt` varchar(3000) not null,
-- 	`receiptMd5` varchar(40) not null,
-- 	UNIQUE KEY `receiptMd5` (`receiptMd5`),
-- 	KEY `roleID` (`roleID`)
-- ) ENGINE=InnoDB DEFAULT CHARSET=utf8 comment '月卡充值离线到账';

-- alter table gRole add column `goldMonthVIPTotalPaid` int(11) unsigned not null comment '购买月卡总共消费钻石数' after `srcType`;
alter table gChapter add column `starRewarded` smallint(5) unsigned not null default 0 comment '星星领取奖励' after `curDate`;
alter table gDungeon add column `resetTimes` smallint(4) unsigned not null default 0 comment '关卡重置次数' after `bestScore`;
alter table gRole add column `lastLoginTime` int(11) unsigned not null comment '上次登录时间' after `srcType`;
ALTER TABLE `gRole` ADD COLUMN `tasklevel` SMALLINT UNSIGNED NOT NULL DEFAULT 0  AFTER `lastLoginTime` ;

ALTER TABLE `gLimit` ADD COLUMN `trainerGoldBonusBoxCount` INT UNSIGNED NOT NULL  AFTER `equipItemBonusBoxCount` , ADD COLUMN `trainerGoldBoxCount` INT UNSIGNED NOT NULL  AFTER `trainerGoldBonusBoxCount` , ADD COLUMN `trainerItemBoxCount` INT UNSIGNED NOT NULL  AFTER `trainerGoldBoxCount` ;
ALTER TABLE `gBoxInfo` ADD COLUMN `lastTrainerTime` INT UNSIGNED NOT NULL  AFTER `lastItemTime` ;

DROP TABLE IF EXISTS `gStarRewardChapter`;
CREATE TABLE `gStarRewardChapter` (
  `roleID` int(11) unsigned not null comment 'roleID',
  `chapterID` int(11) unsigned not null comment 'chapterID',
  `starRewardStatus` smallint(5) unsigned not null comment '星星领取状态',
  `starCount` smallint(5) unsigned not null comment '星星总数',
  primary key (`roleID`,`chapterID`),
  key `roleID` (`roleID`) 
)ENGINE = InnoDB DEFAULT CHARSET=utf8 comment '关卡星星奖励信息';
