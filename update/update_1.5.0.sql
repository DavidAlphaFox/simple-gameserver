alter table gfamily add column `talkRoomID` varchar(25) NOT NULL Default "0" comment '语聊房间id' after `cross_rank`;
DROP TABLE IF EXISTS `gGeneralTeamInfo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGeneralTeamInfo` (
  `teamid` bigint(20) unsigned NOT NULL,
  `teaminfo` longblob NOT NULL,
  PRIMARY KEY (`teamid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gGerMirror`;
CREATE TABLE `gGerMirror` (
  `roleID` int(11) unsigned NOT NULL,
  `gerMirrorInfo` blob NOT NULL COMMENT '保存镜像转化信息',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

ALTER TABLE `gRole` ADD COLUMN `plane_level` SMALLINT UNSIGNED NOT NULL DEFAULT 1 COMMENT '卡洛斯飞机等级' AFTER `tasklevel`; 
ALTER TABLE gRole ADD COLUMN `teamid` bigint(20) NOT NULL DEFAULT -1 COMMENT '组队ID' AFTER `plane_level`; 
-- 增加字段，记录荣誉
ALTER TABLE gRole ADD COLUMN honor INT NOT NULL DEFAULT '0'  COMMENT '荣誉' AFTER `teamid`;

DROP TABLE IF EXISTS `gCarlosWar`;
CREATE TABLE `gCarlosWar` (
	`warID` bigint(20) unsigned not null comment '战场id',
	`war_data` blob comment '战场数据',
	PRIMARY key (`warID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;
