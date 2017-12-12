-define(CREATE_SQL_FORMAT_LIST,[

"CREATE TABLE IF NOT EXISTS `logBuyTimes_~w_~w` (
  `roleID` int(11) unsigned NOT NULL,
  `time` datetime NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT '购买时候的VIP等级',
  `todayBuyTimes` int(11) unsigned NOT NULL COMMENT '本日的第几次购买',
  `newValue` int(11) unsigned NOT NULL COMMENT '购买后的值',
  `add` int(11) unsigned NOT NULL COMMENT '本次增加值',
  `type` int(11) unsigned NOT NULL COMMENT '类型 1:体力次数 2:探索次数 3:pvp次数 4:PLUNDER次数 5：兑换银两次数',
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='购买探索次数的LOG';",

"CREATE TABLE IF NOT EXISTS `logCreateRole_~w_~w` (
  `accid` bigint(11) unsigned NOT NULL COMMENT '用户ID',
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `devid` varchar(100) DEFAULT NULL COMMENT '设备id',
  `ip` varchar(20) NOT NULL COMMENT 'IP地址',
  `result` tinyint(1) unsigned NOT NULL COMMENT '创角结果：1=成功，2=重名，3=其他',
  `time` datetime NOT NULL,
  `sex` tinyint(1) unsigned NOT NULL COMMENT '性别',
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家创角日志';",

"CREATE TABLE IF NOT EXISTS `logSelectGer_~w_~w` (
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `time` datetime DEFAULT NULL,
  `gerID` smallint(6) unsigned NOT NULL COMMENT '武将的模版ID',
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='选择第一个武将的日志记录';",

"CREATE TABLE IF NOT EXISTS `t_coin_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` bigint unsigned NOT NULL COMMENT '获得的银两',
  `curCoin` bigint NOT NULL COMMENT '获得前的银两数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='银两获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_coin_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` bigint unsigned NOT NULL COMMENT '消耗的银两',
  `curCoin` bigint NOT NULL COMMENT '消费前的银两数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='银两消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_unioncoin_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `unioncoin` int(11) unsigned NOT NULL COMMENT '获得的公会货币',
  `curCoin` int(11) NOT NULL COMMENT '获得前的公会货币数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='公会货币获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_unioncoin_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `unioncoin` int(11) unsigned NOT NULL COMMENT '消耗的公会货币',
  `curCoin` int(11) NOT NULL COMMENT '消费前的公会货币数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='公会货币消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_ger_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '新获得的武将UID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` int(11) unsigned NOT NULL COMMENT '武将等级',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '武将品阶',
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '获得的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(50) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将获得记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` int(11) unsigned NOT NULL COMMENT '武将等级',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '武将品阶',
  `time` datetime NOT NULL,
  `type` tinyint(4) unsigned NOT NULL COMMENT '武将消耗类型',
  `argID` int(11) unsigned NOT NULL COMMENT '附带参数',
  `desc` varchar(50) NOT NULL COMMENT '附带描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将消耗记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_uplevel_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` int(11) unsigned NOT NULL COMMENT '升级前武将等级',
  `gerExp` bigint(20) unsigned NOT NULL COMMENT '升级前的武将经验',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '武将品阶',
  `newLevel` int(11) unsigned NOT NULL COMMENT '新的武将等级',
  `newExp` bigint(20) unsigned NOT NULL COMMENT '新的武将等级',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将升级记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_uprank_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` int(11) unsigned NOT NULL COMMENT '升品前武将等级',
  `gerExp` bigint(20) unsigned NOT NULL COMMENT '升品前的武将经验',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '升品前的武将品阶',
  `newLevel` int(11) unsigned NOT NULL COMMENT '新的武将等级',
  `newExp` bigint(20) unsigned NOT NULL COMMENT '新的武将经验',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '新的武将品阶',
  `foodGerUID` bigint(20) unsigned NOT NULL COMMENT '作为升品材料的武将唯一ID',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将升品记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_downrank_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `gerLevel` int(11) unsigned NOT NULL COMMENT '降品品前武将等级',
  `gerRank` tinyint(4) unsigned NOT NULL COMMENT '降品品前的武将品阶',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '新的武将品阶',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='武将降品记录';",

"CREATE TABLE IF NOT EXISTS `t_item_downrank_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '武将唯一ID',
  `ItemTypeID` smallint(6) unsigned NOT NULL COMMENT '武将模版ID',
  `ItemLevel` int(11) unsigned NOT NULL COMMENT '降品品前武将等级',
  `ItemRank` tinyint(4) unsigned NOT NULL COMMENT '降品品前的武将品阶',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '新的武将品阶',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='装备降品记录';",

"CREATE TABLE IF NOT EXISTS `t_gold_bonus_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `goldBonus` int(11) unsigned NOT NULL COMMENT '获得的赠送元宝',
  `curGoldBonus` int(11) NOT NULL COMMENT '获得前的赠送元宝数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='赠送获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_gold_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `gold` int(11) unsigned NOT NULL COMMENT '消耗的元宝',
  `goldBonus` int(11) unsigned NOT NULL COMMENT '消耗的赠送元宝',
  `curGold` int(11) unsigned NOT NULL COMMENT '消费前的元宝数量',
  `curGoldBonus` int(11) unsigned NOT NULL COMMENT '消费前的赠送元宝数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  `vlog` varchar(4000) not null comment '附加日志',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='元宝消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_item_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '新创建的道具UID、或者更新了数量的旧道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `addNum` smallint(6) unsigned NOT NULL COMMENT '获得的道具数量',
  `curItemNum` int(11) unsigned NOT NULL COMMENT '获得道具前的该道具数量',
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '获得的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(50) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='道具获得记录';",

"CREATE TABLE IF NOT EXISTS `t_item_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '消耗的道具UID、或者更新了数量的道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `decNum` smallint(6) unsigned NOT NULL COMMENT '消耗的道具数量',
  `curItemNum` int(11) unsigned NOT NULL COMMENT '消耗道具前的该道具数量',
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '消耗的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(50) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='道具消耗记录';",

"CREATE TABLE IF NOT EXISTS `t_item_uplevel_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '升级的道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `addLevel` int(11) unsigned NOT NULL COMMENT '提升的装备等级',
  `newLevel` int(11) unsigned NOT NULL COMMENT '升级后的装备等级',
  `addTimes` tinyint(4) unsigned NOT NULL COMMENT '连续升级的次数',
  `coin` int(11) DEFAULT NULL COMMENT '消耗的银两',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='装备升级记录';",

"CREATE TABLE IF NOT EXISTS `t_item_uprank_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '升品的道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `curLevel` int(11) unsigned NOT NULL COMMENT '升品前的装备等级',
  `newLevel` int(11) unsigned NOT NULL COMMENT '升品后的装备等级',
  `curRank` tinyint(4) unsigned NOT NULL COMMENT '升品前的装备品阶',
  `newRank` tinyint(4) unsigned NOT NULL COMMENT '升品后的装备品阶',
  `foodItemUID` bigint(20) unsigned NOT NULL COMMENT '消耗的装备UID',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `time` (`time`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='道具升品记录';",

"CREATE TABLE IF NOT EXISTS `t_repu_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '获得的声望',
  `curCoin` int(11) NOT NULL COMMENT '获得前的声望数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='声望获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_repu_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的声望',
  `curCoin` int(11) NOT NULL COMMENT '消费前的声望数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='声望消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_score_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的积分',
  `curCoin` int(11) NOT NULL COMMENT '消费前的积分数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='积分消耗记录表';",

"CREATE TABLE IF NOT EXISTS `logLogin_~w_~w` (
  `accid` bigint(11) unsigned NOT NULL COMMENT '用户ID',
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `devid` varchar(100) DEFAULT NULL COMMENT '设备号',
  `ip` varchar(20) NOT NULL COMMENT 'IP地址',
  `datetime` datetime NOT NULL,
  `duration` int(8) unsigned NOT NULL COMMENT '在线时间',
  KEY `roleID` (`roleID`),
  KEY `datetime` (`datetime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家登录日志';",

"CREATE TABLE IF NOT EXISTS `logSuggest_~w_~w` (
  `accid` bigint(11) unsigned NOT NULL COMMENT '用户ID',
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `roleName` varchar(20) NOT NULL COMMENT '主公名字',
  `title` varchar(50) NOT NULL COMMENT '物理地址',
  `content` varchar(500) NOT NULL COMMENT 'IP地址',
  `datetime` datetime NOT NULL,
  KEY `accid` (`accid`),
  KEY `roleID` (`roleID`),
  KEY `datetime` (`datetime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家反馈日志';",

"CREATE TABLE IF NOT EXISTS `logHomestead_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='家园交配日志';",

"CREATE TABLE IF NOT EXISTS `logFriendEnargy_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
   `type` tinyint(1) unsigned NOT NULL COMMENT '1=领取，2=赠送',
  `DRoleID` int(11) unsigned NOT NULL COMMENT '主公ID',
 `value` int(11) unsigned NOT NULL COMMENT '体力值',
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='好友送和领取体力日志';",

"CREATE TABLE IF NOT EXISTS `logDungenFight_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
   `dungenID` int(11) unsigned NOT NULL COMMENT '关卡ID',
   `result` tinyint(1) unsigned NOT NULL COMMENT '结果：1=成功，0=失败',
   `type` tinyint(1) unsigned NOT NULL COMMENT '1=挑战，2=扫荡',
   `times` tinyint(4) unsigned NOT NULL COMMENT '扫荡的循环次数',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='关卡挑战日志';",

"CREATE TABLE IF NOT EXISTS `logWorldBoss_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
	`type` tinyint(1) unsigned NOT NULL COMMENT '1=南蛮，2=虎牢关',
    `harm` bigint(20) unsigned not null comment '伤害',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='世界boss日志';",

"CREATE TABLE IF NOT EXISTS `logRaceSign_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='华丽大赛报名日志';",

"CREATE TABLE IF NOT EXISTS `logFamilyTekDonateBack_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
  `donateType` tinyint(1) unsigned NOT NULL COMMENT '捐献类型：1=金币，2=钻石，3=徽章，4=装备，5=精灵',
  `donateNum` int(11) unsigned NOT NULL COMMENT '捐献数量',
  `donateUID` bigint(11) unsigned NOT NULL COMMENT '物品UID',
  `donateTypeID` int(11) unsigned NOT NULL COMMENT '物品TypeID',
  `donateQuality` int(11) unsigned NOT NULL COMMENT '物品品质',
  `donateLevel` int(11) unsigned NOT NULL COMMENT '物品等级',
  `donateExp` int(11) unsigned NOT NULL COMMENT '物品经验',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='公会科技捐献反馈日志';",

"CREATE TABLE IF NOT EXISTS `logPvpFight_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL COMMENT '主公ID',
  `datetime` datetime NOT NULL,
	`result` tinyint(1) unsigned NOT NULL COMMENT '结果：1=成功，0=失败',
	`rank` int(11) unsigned NOT NULL COMMENT '排名',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='好友送体力日志';",

"CREATE TABLE IF NOT EXISTS  `t_familytek_source_consume_~w_~w`
(
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `familyID` INT(11) NULL ,
  `familytekuplevelCost` TEXT NOT NULL COMMENT '升级公会科技消耗',
  `familytekdate` VARCHAR(128) NOT NULL COMMENT '升级公会科技时间',
  `familytekdatedetail` DATETIME NOT NULL COMMENT '升级公会科技日志时间',
  `consumetype` INT NOT NULL COMMENT '消耗的类型',
  `familytekID` INT NOT NULL COMMENT '升级前公会科技ID',
  `familytekLevelresult` VARCHAR(10) NOT NULL COMMENT '升级科技后的公会科技等级',
  PRIMARY KEY (`id`),
  KEY `familyID` (`familyID`)
  )ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='公会科技升级日志';",

"CREATE TABLE IF NOT EXISTS `t_profoundCrystal_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '获得的奥义结晶',
  `curCoin` int(11) NOT NULL COMMENT '获得前的奥义结晶数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='奥义结晶获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_profoundCrystal_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的奥义结晶',
  `curCoin` int(11) NOT NULL COMMENT '消费前的奥义结晶数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='奥义结晶消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_honor_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '获得的荣誉',
  `curCoin` int(11) NOT NULL COMMENT '获得前的荣誉数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='荣誉获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_honor_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的荣誉',
  `curCoin` int(11) NOT NULL COMMENT '消费前的荣誉数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='荣誉消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_home_resource_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '获得的家园物资',
  `curCoin` int(11) NOT NULL COMMENT '获得前的家园物资数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='家园物资获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_home_resource_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的家园物资',
  `curCoin` int(11) NOT NULL COMMENT '消费前的家园物资数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='家园物资消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_pvppoint_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '获得的荣誉',
  `curCoin` int(11) NOT NULL COMMENT '获得前的荣誉数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='荣誉获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_pvppoint_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `coin` int(11) unsigned NOT NULL COMMENT '消耗的荣誉',
  `curCoin` int(11) NOT NULL COMMENT '消费前的荣誉数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='荣誉消耗记录表';",

"CREATE TABLE IF NOT EXISTS `logCarlos_~w_~w` (
    `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
    `roleID` int(11) unsigned NOT NULL,
    `carlostype` int(11) unsigned NOT NULL COMMENT '飞机类型',
    `optype` int(11) unsigned NOT NULL COMMENT '操作类型',
    `time` datetime NOT NULL COMMENT '时间',
    `extra` int(11) NOT NULL DEFAULT 0 COMMENT '额外数据',
    PRIMARY KEY (`id`),
    KEY `roleID` (`roleID`),
    KEy `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='飞机操作记录表';",

"CREATE TABLE IF NOT EXISTS `t_anubis_score_add_~w_~w` (
    `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
    `familyID` int(11) unsigned NOT NULL,
    `oldfamilykillnum` int(11) unsigned NOT NULL COMMENT '旧的公会杀人数',
    `oldfamilyscore` int(11) unsigned NOT NULL COMMENT '旧的公会积分',
    `familykillnum` int(11) unsigned NOT NULL COMMENT '新的公会杀人数',
    `familyscore` int(11) unsigned NOT NULL COMMENT '新的公会积分',
    `time` datetime NOT NULL COMMENT '时间',
    `operatetype` int(11) NOT NULL  COMMENT '操作类型',
    PRIMARY KEY (`id`),
    KEY `familyID` (`familyID`),
    KEy `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='公会阿努比斯积分变化记录表';",

"CREATE TABLE IF NOT EXISTS `t_ticket_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `ticket` int(11) unsigned NOT NULL COMMENT '获得的点券',
  `curTicket` int(11) NOT NULL COMMENT '获得前的点券数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='点券获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_ticket_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `ticket` int(11) unsigned NOT NULL COMMENT '消耗的点券',
  `curTicket` int(11) NOT NULL COMMENT '消费前的点券数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='点券消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_laputastone_add_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `laputastone` int(11) unsigned NOT NULL COMMENT '获得的飞行币',
  `curLaputastone` int(11) NOT NULL COMMENT '获得前的飞行币数量',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='飞行币获得记录表';",

"CREATE TABLE IF NOT EXISTS `t_laputastone_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `laputastone` int(11) unsigned NOT NULL COMMENT '消耗的飞行币',
  `curLaputastone` int(11) NOT NULL COMMENT '消费前的飞行币数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='飞行币消耗记录表';",

"CREATE TABLE IF NOT EXISTS `t_item_enchant_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '升级的道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `oldItemEnchantType` smallint(6) unsigned NOT NULL COMMENT '道具旧的附魔类型', 
  `oldItemEnchantLevel` int(11) unsigned NOT NULL COMMENT '道具旧的附魔等级', 
  `newItemEnchantType` smallint(6) unsigned NOT NULL COMMENT '道具新的附魔类型', 
  `newItemEnchantLevel` int(11) unsigned NOT NULL COMMENT '道具新的附魔等级', 
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `itemUID`(`itemUID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='装备附魔记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_awake_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '精灵唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '精灵模版ID',
  `awakeStep` smallint(6) unsigned NOT NULL COMMENT '精灵觉醒阶段',
  `oldSkillID` int(11) unsigned NOT NULL COMMENT '精灵旧的觉醒技能ID',
  `newskillID` int(11) unsigned NOT NULL COMMENT '精灵新的觉醒技能ID',
  `oldOptinSkillID` varchar(150) DEFAULT NULL COMMENT '旧的备选技能ID',
  `newOptinSkillID` varchar(150) DEFAULT NULL COMMENT '新的备选技能ID',
  `oldRecastTimes` int(11) unsigned NOT NULL COMMENT '精灵旧的洗练次数',
  `newRecastTimes` int(11) unsigned NOT NULL COMMENT '精灵新的洗练次数',
  `operateType`    smallint(6) unsigned NOT NULL COMMENT '操作ID',  
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `gerUID` (`gerUID`),
  KEY `awakeStep` (`awakeStep`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='精灵觉醒记录';",

"CREATE TABLE IF NOT EXISTS `t_ger_crystal_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `gerUID` bigint(20) unsigned NOT NULL COMMENT '精灵唯一ID',
  `gerTypeID` smallint(6) unsigned NOT NULL COMMENT '精灵模版ID',
  `crystalType` smallint(6) unsigned NOT NULL COMMENT '精灵晶体类型',
  `oldQuality` smallint(6) unsigned NOT NULL COMMENT '精灵晶体旧品阶',
  `newQuality` smallint(6) unsigned NOT NULL COMMENT '精灵晶体新品阶',
  `oldLevel` int(11) unsigned NOT NULL COMMENT '精灵晶体旧的等级',
  `newLevel` int(11) unsigned NOT NULL COMMENT '精灵晶体新的等级',
  `oldExp` bigint(20) unsigned NOT NULL COMMENT '精灵晶体旧经验',
  `newExp` bigint(20) unsigned NOT NULL COMMENT '精灵晶体新经验',
  `oldRankExp` int(11) unsigned NOT NULL COMMENT '精灵晶体旧的晶簇经验',
  `newRankExp` int(11) unsigned NOT NULL COMMENT '精灵晶体新的晶簇经验',
  `operateType`    smallint(6) unsigned NOT NULL COMMENT '操作ID',  
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `gerUID` (`gerUID`),
  KEY `crystalType` (`crystalType`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='精灵晶体记录';",

"CREATE TABLE IF NOT EXISTS `t_family_anubis_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `familyID` int(11) unsigned NOT NULL COMMENT '工会id',
  `score` int(11) NOT NULL COMMENT '旧分数',
  `newScore` int(11) NOT NULL COMMENT '新分数',
  `playerNum` smallint(3) not null comment '修改布阵后的人数',
  `time` datetime NOT NULL COMMENT '消费时间',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='阿努比斯相关';",

"CREATE TABLE IF NOT EXISTS `t_magicBook_update_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `bookID` int(11) unsigned not null comment '当前升级的魔典id',
  `stateStr` varchar(40) comment '当前魔典内容',
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='阿努比斯相关';",







"CREATE TABLE IF NOT EXISTS `t_plunder_fight_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `roleName` varchar(20) NOT NULL COMMENT '主公名字',
  `level` int(11) unsigned not null,
  `fight_power` int(11) unsigned not null,
  `vip` int(11) unsigned not null,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='阿努比斯相关';",







"CREATE TABLE IF NOT EXISTS `t_dojankrank_world_record_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `time` int(11) unsigned NOT NULL,
  `atk_serverid` int(11) unsigned not null comment '当前升级的魔典id',
  `atk_devid` varchar(100) NOT NULL,
  `atk_roleID` int(11) unsigned NOT NULL COMMENT '玩家ID',
  `atk_level` smallint(6) unsigned NOT NULL DEFAULT '1' COMMENT '等级',
  `atk_fightPower` bigint(14) unsigned NOT NULL DEFAULT '0' COMMENT '总战斗力',
  `atk_vipLevel` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'VIP等级',
  `ranktype` int(11) unsigned NOT NULL,
  `wintype` int(11) unsigned NOT NULL,
  `before_rank` int(11) unsigned NOT NULL,
  `after_rank` int(11) unsigned NOT NULL,
  `def_serverid` int(11) unsigned not null comment '当前升级的魔典id',
  `def_devid` varchar(100) NOT NULL,
  `def_roleID` int(11) unsigned NOT NULL COMMENT '玩家ID',
  `def_level` smallint(6) unsigned NOT NULL DEFAULT '1' COMMENT '等级',
  `def_fightPower` bigint(14) unsigned NOT NULL DEFAULT '0' COMMENT '总战斗力',
  `def_vipLevel` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'VIP等级',
  PRIMARY KEY (`id`),
  KEY `atk_roleID` (`atk_roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='跨服pvp战斗记录';",

"CREATE TABLE IF NOT EXISTS `t_energy_consume_~w_~w` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `vipLevel` int(11) unsigned NOT NULL COMMENT 'VIP等级',
  `energy` bigint unsigned NOT NULL COMMENT '消耗的体力',
  `curEnergy` bigint NOT NULL COMMENT '消费前的体力数量',
  `time` datetime NOT NULL COMMENT '消费时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '消费类型',
  `argID` int(11) NOT NULL COMMENT '与消费类型相关的：商品ID、武将ID或者道具ID',
  `desc` varchar(50) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleID`),
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='体力消耗记录表';"

]).

