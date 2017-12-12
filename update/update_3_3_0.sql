
DROP TABLE IF EXISTS `gGerHolyGrail`;
CREATE  TABLE IF NOT EXISTS `gGerHolyGrail` (
  `roleID` INT UNSIGNED NOT NULL,
  `gerID` BIGINT UNSIGNED NOT NULL,
  `gerDiamondInfo` VARBINARY(100) NOT NULL COMMENT '宝石信息',       
  `gerHolyGrailLevel` SMALLINT(6) NOT NULL DEFAULT 0 COMMENT '神器等级',
  `isFinishSacrifice` tinyint(1) NOT NULL DEFAULT 0 COMMENT '是否完成对应等级神器献祭',
  PRIMARY KEY (`gerID`),
  INDEX `roleID` (`roleID` ASC)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='精灵神器信息';

DROP TABLE IF EXISTS `gtasklink`;
CREATE  TABLE IF NOT EXISTS `gtasklink` (
  `roleID` INT UNSIGNED NOT NULL,
  `last_date` DATE NOT NULL COMMENT '数据处理的最后日期，判断是否跨天',
  `free_time` INT UNSIGNED NOT NULL COMMENT '剩余免费次数',
  `buy_left_time` INT UNSIGNED NOT NULL COMMENT '剩余购买的次数',
  `pay_count` INT UNSIGNED NOT NULL COMMENT '当天买了多少次',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='跑环';

ALTER TABLE `grole` ADD COLUMN `transmigration` INT UNSIGNED NOT NULL DEFAULT 0  AFTER `laputastone` ;
ALTER TABLE `groleextra` ADD COLUMN `battleProgressTransmigration` SMALLINT UNSIGNED NOT NULL DEFAULT 0  AFTER `battleProgressFastHard`;

ALTER TABLE `t_item_consume_2017_3` modify column `desc` varchar(50);