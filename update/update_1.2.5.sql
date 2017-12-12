DROP TABLE IF EXISTS `gBoxInfo`;
CREATE TABLE `gBoxInfo` (
  `roleID` int(11) unsigned NOT NULL,
  `lastGerTime` int(11) unsigned not null comment '上次免费抽取武将时间',
  `lastItemTime` int(11) unsigned not null comment '上次免费抽取装备时间',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gStoneChip`;
CREATE TABLE `gStoneChip` (
  `roleID` int(11) unsigned NOT NULL,
  `stoneType` int(11) unsigned NOT NULL comment '碎片合成对应的符文TypeID',
  `firstNum` int(11) unsigned NOT NULL comment '第一类碎片数量',
  `secondNum` int(11) unsigned NOT NULL comment '第二类碎片数量',
  `thirdNum` int(11) unsigned NOT NULL comment '第三类碎片数量',
  `fourthNum` int(11) unsigned NOT NULL comment '第四类碎片数量',
  UNIQUE KEY `roleID_stoneType` (`roleID`,`stoneType`),
          KEY `roleID` (`roleID`),
          KEY `stoneType` (`stoneType`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gPlunder`;
CREATE TABLE `gPlunder` (
  `roleID` int(11) unsigned NOT NULL,
  `protectEndTime` int(11) unsigned NOT NULL comment '保护结束的时间',
  `restAttackTimes` int(11) unsigned NOT NULL comment '剩余攻击次数',
  `buyTimes` int(11) unsigned NOT NULL comment '剩余购买次数',
  `lastTimestamp`   VARBINARY(1000) NOT NULL comment '上次更新数据的时间，主要是用于隔天初始化数据用'  ,
    `recoverTimestamp`     int(11) unsigned NOT NULL comment '夺取回复时间戳',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gFamilySortInfo`;
CREATE TABLE `gFamilySortInfo` ( 
    `familyID`            int(11) unsigned NOT NULL       COMMENT '联盟ID',
    `score`               int(11) unsigned NOT NULL       COMMENT '公会站积分',
    `familyLevel`         smallint(6) unsigned NOT NULL   COMMENT '联盟等级',
    `fightPower`          bigint(20) unsigned NOT NULL    COMMENT '公会总战力',
    `familyName`          varchar(20) NOT NULL            COMMENT '联盟名称',
    `ownerRoleName`       varchar(20) NOT NULL            COMMENT '盟主名称',
    `curMembers`          smallint(4) unsigned NOT NULL   COMMENT '当前成员数量',
    `rank`                int(11) unsigned NOT NULL       COMMENT '联盟排名',
    `notice`              varchar(200) NOT NULL           COMMENT '公告',
    `ownerRoleID`         int(11) unsigned NOT NULL       COMMENT '盟主roleID',
    `worldRank`           int(11) unsigned NOT NULL       COMMENT '联盟战世界排名',
    `slogan`              varchar(200) NOT NULL           COMMENT '口号',
    `serverID`            int(11) unsigned NOT NULL       COMMENT '服务器ID',
    PRIMARY KEY(`familyID`),
    INDEX `order` (`score`, `familyLevel`, `fightPower`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

ALTER TABLE `gFamilyMasterRecord` ADD COLUMN `totalPower` INT UNSIGNED NOT NULL comment '上次报名时记录的战斗力' DEFAULT 0  AFTER `lastWorldRank`;
ALTER TABLE `gFamily` ADD COLUMN `cross_rank` INT UNSIGNED NOT NULL DEFAULT 0 COMMENT '全服排名' AFTER `familyTask`;
