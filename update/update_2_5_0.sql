ALTER TABLE gHome ADD COLUMN `cirrus_info` blob NOT NULL  AFTER `dounty_ac_num` ;

DROP TABLE IF EXISTS `gDoublematch`;
CREATE TABLE `gDoublematch` (
  `roleID` int(10) unsigned NOT NULL,
  `remain_time` int(10) unsigned not null comment '', 
  `already_buy` int(10) unsigned not null comment '', 
  `rf_date` varchar(32) DEFAULT NULL,
  `rank` int(10) unsigned not null comment '', 
  `score` int(10) unsigned not null comment '', 
  `fight_rec` blob not null comment '包含双方的骰子',
  `session` int(10) unsigned not null comment '记录玩家数据属于哪个赛季',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='双排竞技场数据';


DROP TABLE IF EXISTS `gCarlosPlaneInfo`;
CREATE TABLE `gCarlosPlaneInfo` (
	`roleID` int(10) unsigned not null comment 'roleID',
	`planeType`	tinyint(2) unsigned not null default 0 comment '飞机类型,0=原始飞机',
	`inuse` tinyint(2) unsigned not null default 0 comment '实际使用的飞机',
	`validTime`	int(10) unsigned not null default 0 comment '有效期',
	PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='飞机道具使用信息';

DROP TABLE IF EXISTS `gDoubleFightRec`;
CREATE TABLE `gDoubleFightRec` (
  `rec_id` int(10) unsigned NOT NULL,
  `f1` int(10) unsigned NOT NULL,
  `f2` int(10) unsigned NOT NULL,
  `f3` int(10) unsigned NOT NULL,
  `f4` int(10) unsigned NOT NULL,
  `cscore` tinyint(2) NOT NULL,
  `ts` int(10) unsigned NOT NULL,
  `fight_rec` blob not null comment '包含双方的骰子',
  PRIMARY KEY (`rec_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='双排竞技场数据';

DROP TABLE IF EXISTS `gDmData`;
CREATE TABLE `gDmData` (
  `data_id` bigint unsigned NOT NULL,
  `state_data` blob not null comment 'state数据',
  PRIMARY KEY (`data_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='双排竞技场数据';

CREATE TABLE IF NOT EXISTS `logCarlos_2016_4` (
    `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
    `roleID` int(11) unsigned NOT NULL,
    `carlostype` int(11) unsigned NOT NULL COMMENT '飞机类型',
    `optype` int(11) unsigned NOT NULL COMMENT '操作类型',
    `time` datetime NOT NULL COMMENT '时间',
    `extra` int(11) NOT NULL DEFAULT 0 COMMENT '额外数据',
    PRIMARY KEY (`id`),
    KEY `roleID` (`roleID`),
    KEy `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='飞机操作记录表';

delete from gStoneChip where stoneType in (5051,5052,5053,5054,5055,5056,5057,5058,5033,5034,5035,5036,5037,5038,5039,5040,5041,5042,5043,5044,5045,5046);

  


