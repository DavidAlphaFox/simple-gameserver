
DROP TABLE IF EXISTS `gExBoss`;
CREATE  TABLE IF NOT EXISTS `gExBoss` (
  `roleID` INT UNSIGNED NOT NULL,
  `bossID` INT UNSIGNED NOT NULL comment 'bossID',
  `curHp` BIGINT UNSIGNED NOT NULL comment 'boss 当前剩余血量',
  `maxHp` BIGINT UNSIGNED NOT NULL comment 'boss 的最高血量',
  `useTimes` SMALLINT UNSIGNED NOT NULL comment '攻击boss剩余次数',
  `buyTimes` SMALLINT UNSIGNED NOT NULL comment '已经购买次数',
  `lastRefreshSec` INT UNSIGNED NOT NULL comment '上次恢复次数的时间',
  `isGetReward` TINYINT UNSIGNED NOT NULL comment '是否领取了宝箱',
  `bossLevel` TINYINT UNSIGNED NOT NULL comment 'boss等级',
  `oneHit` INT UNSIGNED NOT NULL comment '每次打boss产生的伤害',
  `nowHit` SMALLINT UNSIGNED NOT NULL comment '当前打到第几次',
  `maxHit` SMALLINT UNSIGNED NOT NULL comment '最多可以打boss多少次',
  `freeTimes` TINYINT UNSIGNED NOT NULL comment 'used free times',
  `hitList` VARBINARY(2000) NOT NULL COMMENT '打boss的暴击信息列表',       
  `reward` VARBINARY(200) NOT NULL comment 'boss reward',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='探险boss';

DROP TABLE IF EXISTS `gEquipExtra`;
CREATE  TABLE IF NOT EXISTS `gEquipExtra` (
  `roleID` INT(11) UNSIGNED NOT NULL COMMENT '玩家ID',
  `itemUID` BIGINT UNSIGNED NOT NULL COMMENT '装备ID',
  `legendRank` SMALLINT UNSIGNED NOT NULL DEFAULT 0 COMMENT '装备传奇锻造等级',
  PRIMARY KEY (`itemUID`),
  INDEX `roleID` (`roleID` ASC)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT = '精灵装备额外信息';

ALTER TABLE `gRoleExtra` ADD COLUMN `plane_ai_flag` int(11) NOT NULL DEFAULT 0 COMMENT '用于飞行基地AI' AFTER `maingerTypeid`;
UPDATE `gRoleExtra` SET `plane_ai_flag`='7' WHERE `roleID`>'0';

delete from gTask where taskID in (20077,20078,20079,20080,20081,20082,20083,20084,40017,40029,40043,40061,45014) ;
