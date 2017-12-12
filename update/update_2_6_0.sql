alter table ggift modify column type char(3) comment 'type';
alter table gRole add column firtPayStatus tinyint(2) unsigned not null comment '首冲活动' after home_resource;
alter table gCarlosPlaneInfo add column planeType2 tinyint(2) unsigned not null comment '飞机8的信息' after planeType;
alter table gCarlosPlaneInfo add column validTime2 int(10) unsigned not null comment '飞机8的有效时间' after validTime;

CREATE  TABLE IF NOT EXISTS `gGerCrystal` (
  `roleID` INT UNSIGNED NOT NULL,
  `gerID` BIGINT UNSIGNED NOT NULL,
  `gerCrystalInfo` VARBINARY(1000) NOT NULL DEFAULT -1 ,
  PRIMARY KEY (`gerID`),
  INDEX `roleID` (`roleID` ASC)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='精灵晶体信息';

DROP TABLE IF EXISTS `gHomeBossTimes`;
CREATE TABLE `gHomeBossTimes` (
    `roleID` int(11) unsigned NOT NULL,
    `total` tinyint(3) unsigned  NOT NULL COMMENT '当前剩余免费次数',
    `goldTimes` tinyint(3) unsigned NOT NULL COMMENT '花钱购买的次数',
    PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='家园boss攻打次数';

ALTER TABLE `gdoublematch` ADD COLUMN `remain_time_buy` INT UNSIGNED NOT NULL DEFAULT 0 AFTER `session` ;
ALTER TABLE `gger` ADD COLUMN `gerBody` MEDIUMINT UNSIGNED NOT NULL DEFAULT 0  AFTER `gerAwakeInfo` ;
alter table `gFighterList` modify column fighterList varbinary(6000) not null;

delete from gShopNum where shopID = 10001;


