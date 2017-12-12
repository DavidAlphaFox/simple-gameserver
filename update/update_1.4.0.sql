DROP TABLE IF EXISTS `gFamilyDonate`;
CREATE  TABLE `gFamilyDonate` (
  `familyID` INT UNSIGNED NOT NULL COMMENT '公会ID',
  `roleID` INT UNSIGNED NOT NULL COMMENT "玩家ID",
  `diamondNum` INT UNSIGNED NOT NULL COMMENT "钻石捐献数量",
  `coinNum` INT UNSIGNED NOT NULL COMMENT "金币捐献数量",
  `reputationNum` INT UNSIGNED NOT NULL COMMENT "徽章捐献数量",
  `gerInfo` varbinary(300) NOT NULL COMMENT "精灵捐献数量",
  `itemInfo` varbinary(300) NOT NULL COMMENT "装备捐献数量",
  `donateContribution` INT UNSIGNED NOT NULL COMMENT "玩家贡献值",
  UNIQUE INDEX `DonateID` (`familyID` ASC, `roleID` ASC)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8; 

-- gRoleExtra增加三个字段用于记录天赋,
ALTER TABLE gRoleExtra ADD COLUMN `talentStudyBuyTimes` smallint(5) unsigned NOT NULL DEFAULT '0'  COMMENT '增加天赋的次数的购买时间' AFTER `leftChgNameTimes`;

-- gRoleExtra增加一个字段用于记录上次充值的时间
ALTER TABLE gRoleExtra ADD COLUMN `lastPayTime` INT NOT NULL  COMMENT '增加上次充值的时间' AFTER `talentStudyBuyTimes` ;

DROP TABLE IF EXISTS `gTrainer`;
CREATE TABLE `gTrainer` (
    `roleID`    int(11) unsigned NOT NULL   COMMENT '角色ID',
    `talent`    blob    NOT NULL            COMMENT '已经学习的天赋id列表',
    PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- 喇叭红包信息
DROP TABLE IF EXISTS `gBonus`;
CREATE TABLE `gBonus` (
    `bonusID` INT UNSIGNED NOT NULL COMMENT '红包的唯一ID',
    `info` varbinary(3000) NOT NULL COMMENT '红包的具体内容',
    PRIMARY KEY (`bonusID`)
) ENGINE = InnoDB DEFAULT CHARSET=utf8;

-- 限时打折活动信息
DROP TABLE IF EXISTS `gDiscountInfo`;
CREATE TABLE `gDiscountInfo` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `DiscountActivityInfo` varbinary(1000) NOT NULL COMMENT '限时打折活动信息',
  PRIMARY KEY (`roleID`),
  UNIQUE INDEX `roleID_UNIQUE` (`roleID` ASC)
)ENGINE = InnoDB DEFAULT CHARSET=utf8;

-- 保存上次玩家能够参加的所有限时打折活动信息
-- 限时打折活动信息
DROP TABLE IF EXISTS `gDiscountActivityInfo`;
CREATE TABLE `gDiscountActivityInfo` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `DiscountActivityInfo` varbinary(1000) NOT NULL COMMENT '玩家上次能够参加的限时打折活动',
  PRIMARY KEY (`roleID`),
  UNIQUE INDEX `roleID_UNIQUE` (`roleID` ASC)
)ENGINE = InnoDB DEFAULT CHARSET=utf8;

ALTER TABLE gRole modify column fightPower bigint(14) unsigned not null comment '战斗力';
