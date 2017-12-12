CREATE TABLE IF NOT EXISTS `t_item_downrank_2016_1` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `ItemTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `ItemLevel` tinyint(4) unsigned NOT NULL COMMENT '降品品前武将等级',
  `ItemRank` tinyint(4) unsigned NOT NULL COMMENT '降品品前的武将品阶',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '新的武将品阶',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='装备降品记录';

DROP TABLE IF EXISTS `gHome`;
CREATE TABLE `ghome` (
  `roleID` int(10) unsigned NOT NULL,
  `stage` smallint(5) unsigned NOT NULL,
  `constr_list` varchar(4096) DEFAULT NULL,
  `new_task_timer` varchar(4096) DEFAULT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='新家园数据';


DROP TABLE IF EXISTS `gSkin`;
CREATE TABLE `gSkin` (
    `roleID` int(11) unsigned NOT NULL,
    `has` varbinary(1000) NOT NULL COMMENT '当前拥有的皮肤的数据',
    `equip` int(11) unsigned NOT NULL COMMENT '当前装备的皮肤ID',
    PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='皮肤数据';

DROP TABLE IF EXISTS `ghometask`;
CREATE TABLE `ghometask` (
  `idghometask` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `owner_role_id` int(10) unsigned NOT NULL,
  `quality` smallint(5) unsigned NOT NULL,
  `level` smallint(5) unsigned NOT NULL,
  `tgt_ger_type` varchar(128) NOT NULL,
  `timtout_ts` int(10) unsigned NOT NULL,
  `status` smallint(5) unsigned NOT NULL,
  `accept_role_id` int(10) unsigned NOT NULL,
  `ger_id` varchar(1024) NOT NULL,
  `ger_type` varchar(512) NOT NULL,
  `ger_quality` varchar(512) NOT NULL,
  `ger_level` varchar(512) NOT NULL,
  `task_type` int(10) unsigned NOT NULL,
  `reward_num` int(10) unsigned NOT NULL,
  `role_level` int(10) unsigned NOT NULL,
  `box_reward_bin` varbinary(1000) NOT NULL,
  PRIMARY KEY (`idghometask`),
  KEY `owner_role_id_index` (`owner_role_id`),
  KEY `status_index` (`status`),
  KEY `accept_role_id` (`accept_role_id`)
) ENGINE=InnoDB AUTO_INCREMENT=0 DEFAULT CHARSET=utf8;

alter table gRole add `home_resource` int(13) NOT NULL DEFAULT '0'  COMMENT '家园资源' after `carlospreseason`;

