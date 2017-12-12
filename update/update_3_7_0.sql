
DROP TABLE IF EXISTS `gTagData`;
CREATE  TABLE IF NOT EXISTS `gTagData` (
  `roleID` INT UNSIGNED NOT NULL,
  `tagID` TINYINT UNSIGNED NOT NULL,
  `tagData` varbinary(60000) NOT NULL,
  `iconData` varbinary(200) NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='装备穿戴信息';

alter table gRoleExtra ADD COLUMN `trainerProf` bigint UNSIGNED NOT NULL DEFAULT 0 COMMENT '训练师职业' after `energyPac`;

DROP TABLE IF EXISTS `gTrainerRear`;
CREATE  TABLE IF NOT EXISTS `gTrainerRear` (
  `roleID` INT UNSIGNED NOT NULL,
  `rearInfo` VARBINARY(1000) NOT NULL DEFAULT -1,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='培育室信息';

DROP TABLE IF EXISTS `gdojangrank`;
CREATE  TABLE IF NOT EXISTS `gdojangrank` (
  `roleID` INT(11) UNSIGNED NOT NULL COMMENT '玩家ID',
  `day` date NOT NULL COMMENT '上次修改数据的时间',
  `free_time` INT(11) UNSIGNED NOT NULL COMMENT '剩余免费挑战次数',
  `paid_time` INT(11) UNSIGNED NOT NULL COMMENT '剩余收费挑战次数',
  `buy_time` INT(11) UNSIGNED NOT NULL COMMENT '当日购买次数',
  `selected_ger_type` blob NOT NULL COMMENT '精灵展示',
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT = '道馆竞技';

DROP TABLE IF EXISTS `gDojangrankFightRecord`;
CREATE TABLE `gDojangrankFightRecord`(
  `recordUid` bigint(20) unsigned not null AUTO_INCREMENT comment '记录uid',
  `roleID` int(11) unsigned not null comment '角色ID',
  `tarRoleID` int(11) unsigned not null comment '对手角色ID',
  `isWin` tinyint(3) unsigned not null comment '是否胜利',
  `replay` blob not null comment '具体战斗信息',
  `fTime` datetime not null comment '时间戳',
  `roleName` varchar(20) not null comment '角色名字',
  `tarRoleName` varchar(20) not null comment '对手角色名字',
  PRIMARY KEY (`recordUid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
AUTO_INCREMENT = 100001;

DROP TABLE IF EXISTS `gdojangrankfightrecord`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gdojangrankfightrecord` (
  `recordUid` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '记录uid',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ID',
  `tarRoleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `attackerNewRank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '',
  `defenderNewRank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '',
  `isWin` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '是否胜利',
  `replay` blob NOT NULL COMMENT '具体战斗信息',
  `fTime` datetime NOT NULL COMMENT '时间戳',
  `roleName` varchar(20) NOT NULL COMMENT '角色名字',
  `tarRoleName` varchar(20) NOT NULL COMMENT '对手角色名字',
  `rank_type` INT(11) UNSIGNED NOT NULL COMMENT '排行榜类型',
  PRIMARY KEY (`recordUid`),
  KEY `roleID` (`roleID`,`tarRoleID`,`fTime`)
) ENGINE=InnoDB AUTO_INCREMENT=100010 DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gTrainerProfBattle`;
CREATE  TABLE IF NOT EXISTS `gTrainerProfBattle` (
  `roleID` INT UNSIGNED NOT NULL,
  `data` VARBINARY(1000) NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='战斗训练师';

DROP TABLE IF EXISTS `gDojangrankRank`;
CREATE  TABLE IF NOT EXISTS `gDojangrankRank` (
  `rank_index` INT UNSIGNED NOT NULL,
  `value` longblob NOT NULL,
  PRIMARY KEY (`rank_index`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='道馆竞技的排名数据';

