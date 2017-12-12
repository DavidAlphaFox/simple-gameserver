
alter table gdojangrank ADD COLUMN `world_free_time` blob NOT NULL COMMENT '跨服剩余免费挑战次数' after `selected_ger_type`;
alter table gdojangrank ADD COLUMN `world_paid_time` blob NOT NULL COMMENT '跨服剩余收费挑战次数' after `world_free_time`;
alter table gdojangrank ADD COLUMN `world_buy_time` blob NOT NULL COMMENT '跨服当日购买次数' after `world_paid_time`;
alter table gdojangrank ADD COLUMN `world_enemy_list` blob NOT NULL COMMENT '敌人对手数据' after `world_buy_time`;
alter table gdojangrank ADD COLUMN `world_refresh_time` blob NULL COMMENT '刷新次数' after `world_enemy_list`;

alter table gcarlosplaneinfo add column `planeType11` tinyint(2) unsigned not null default 0 after `validTime4`;
alter table gcarlosplaneinfo add column `validTime11` int(10) unsigned not null default 0 after `planeType11`;
alter table gcarlosplaneinfo add column `planeType12` tinyint(2) unsigned not null default 0 after `validTime11`;
alter table gcarlosplaneinfo add column `validTime12` int(10) unsigned not null default 0 after `planeType12`;
alter table gcarlosplaneinfo add column `planeType13` tinyint(2) unsigned not null default 0 after `validTime12`;
alter table gcarlosplaneinfo add column `validTime13` int(10) unsigned not null default 0 after `planeType13`;
alter table gcarlosplaneinfo add column `planeType14` tinyint(2) unsigned not null default 0 after `validTime13`;
alter table gcarlosplaneinfo add column `validTime14` int(10) unsigned not null default 0 after `planeType14`;
alter table gcarlosplaneinfo add column `planeType15` tinyint(2) unsigned not null default 0 after `validTime14`;
alter table gcarlosplaneinfo add column `validTime15` int(10) unsigned not null default 0 after `planeType15`;
alter table gcarlosplaneinfo add column `planeType16` tinyint(2) unsigned not null default 0 after `validTime15`;
alter table gcarlosplaneinfo add column `validTime16` int(10) unsigned not null default 0 after `planeType16`;
alter table gcarlosplaneinfo add column `planeType17` tinyint(2) unsigned not null default 0 after `validTime16`;
alter table gcarlosplaneinfo add column `validTime17` int(10) unsigned not null default 0 after `planeType17`;
alter table gcarlosplaneinfo add column `planeType18` tinyint(2) unsigned not null default 0 after `validTime17`;
alter table gcarlosplaneinfo add column `validTime18` int(10) unsigned not null default 0 after `planeType18`;


------ gRoleExtra是最后一列,在370升级380,和400升级410,after 的列不同,所以没强制after,升级脚本需要注意
alter table gRoleExtra add column `signSec` int(10) unsigned not null comment '注册时间'; 

DROP TABLE IF EXISTS `gdojangrank_world_fightrecord`;
CREATE TABLE `gdojangrank_world_fightrecord` (
  `recordUid` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '记录uid',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ID',
  `tarRoleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `tarServerID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ServerID',
  `attackerNewRank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '',
  `defenderNewRank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '',
  `isWin` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '是否胜利',
  `replay` blob NOT NULL COMMENT '具体战斗信息',
  `fTime` datetime NOT NULL COMMENT '时间戳',
  `roleInfo` blob NOT NULL COMMENT '角色名字',
  `tarRoleInfo` blob NOT NULL COMMENT '对手角色名字',
  `rank_type` INT(11) UNSIGNED NOT NULL COMMENT '排行榜类型',
  PRIMARY KEY (`recordUid`),
  KEY `roleID` (`roleID`,`tarRoleID`,`fTime`)
) ENGINE=InnoDB AUTO_INCREMENT=100010 DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_1`;
CREATE TABLE `gdojangrank_world_rank_1` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_2`;
CREATE TABLE `gdojangrank_world_rank_2` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_3`;
CREATE TABLE `gdojangrank_world_rank_3` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_4`;
CREATE TABLE `gdojangrank_world_rank_4` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_5`;
CREATE TABLE `gdojangrank_world_rank_5` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_6`;
CREATE TABLE `gdojangrank_world_rank_6` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_7`;
CREATE TABLE `gdojangrank_world_rank_7` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_8`;
CREATE TABLE `gdojangrank_world_rank_8` (
  `rank` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜名次',
  `roleID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '对手角色ID',
  `serverID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '角色ServerID',
  `dr_info_bin` blob NOT NULL COMMENT '角色信息',
  `fighter_data_bin` blob NOT NULL COMMENT '阵容数据，包括小伙伴加成和天赋',
  PRIMARY KEY (`rank`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_rank_history`;
CREATE TABLE `gdojangrank_world_rank_history` (
  `rank_type` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜类型',
  `rank_data` LONGBLOB NOT NULL COMMENT '排行榜数据',
  PRIMARY KEY (`rank_type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gdojangrank_world_top_fightrec`;
CREATE TABLE `gdojangrank_world_top_fightrec` (
  `replay_uid` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排行榜类型',
  `attacker_info` blob NOT NULL,
  `defender_info` blob NOT NULL,
  `attackerNewRank` int(11) unsigned NOT NULL DEFAULT '0',
  `defenderNewRank` int(11) unsigned NOT NULL DEFAULT '0',
  `time` int(11) unsigned NOT NULL DEFAULT '0',
  `rank_type` int(11) unsigned NOT NULL DEFAULT '0',
  `is_win` int(11) unsigned NOT NULL DEFAULT '0',
  `fight_rec` LONGBLOB NOT NULL,
  PRIMARY KEY (`replay_uid`)
) ENGINE=InnoDB AUTO_INCREMENT=100010 DEFAULT CHARSET=utf8;

