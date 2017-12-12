DROP TABLE IF EXISTS `gFamily`;
CREATE TABLE `gFamily` (
    `familyID`            int(11) unsigned NOT NULL       COMMENT '联盟ID',
    `familyName`          varchar(20) NOT NULL            COMMENT '联盟名称',
    `familyLevel`         smallint(6) unsigned NOT NULL   COMMENT '联盟等级',
    `createRoleID`        int(11) unsigned NOT NULL       COMMENT '创建者roleID',
    `createRoleName`      varchar(20) NOT NULL            COMMENT '创建者名称',
    `ownerRoleID`         int(11) unsigned NOT NULL       COMMENT '盟主roleID',
    `ownerRoleName`       varchar(20) NOT NULL            COMMENT '盟主名称',
    `curMembers`          smallint(4) unsigned NOT NULL   COMMENT '当前成员数量',
    `activePoints`        int(11) unsigned NOT NULL       COMMENT '联盟建设值',
    `familyScore`         int(11) unsigned NOT NULL       COMMENT '联盟战争得分',
    `notice`              varchar(200) NOT NULL           COMMENT '公告',
    `slogan`              varchar(200) NOT NULL           COMMENT '口号',
    `rank`                int(11) unsigned NOT NULL       COMMENT '联盟排名',
    `worldRank`           int(11) unsigned NOT NULL       COMMENT '联盟战世界排名',
    `isZeroRefreshed`     date NOT NULL    COMMENT '当日是否进行了0点刷新',
    `createTime`          int(11) unsigned NOT NULL       COMMENT '创建联盟的时间戳',
    `talkData`            blob    NOT NULL                COMMENT '联盟成员最近的聊天记录',
    `changeLog`           varbinary(5000) NOT NULL        COMMENT '联盟动态日志',
    `contributeLog`       varbinary(3000) NOT NULL        COMMENT '联盟贡献公告',
    `leftChgNameTimes`    SMALLINT(5) SIGNED NOT NULL DEFAULT -1 COMMENT '剩余改名次数',
    `familyTask`       varbinary(3000) NOT NULL        COMMENT '联盟任务信息',
    PRIMARY KEY (`familyID`),
    UNIQUE KEY `familyName` (`familyName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
    
DROP TABLE IF EXISTS `gFamilyMember`;
CREATE TABLE `gFamilyMember` (
    `roleID`            int(11) unsigned NOT NULL           COMMENT '角色ID',
    `roleName`          varchar(20) NOT NULL                COMMENT '角色名称',
    `familyID`          int(11) unsigned NOT NULL           COMMENT '联盟ID',
    `familyCon`         int(11) unsigned NOT NULL           COMMENT '联盟贡献',
    `leftFamilyCon`     int(11) unsigned NOT NULL           COMMENT '剩余贡献',
    `useGoldTime`       int(11) unsigned NOT NULL           COMMENT '元宝贡献时间戳',
    `title`             tinyint(2) unsigned NOT NULL        COMMENT '玩家称号',
    `isMale`            tinyint(1) unsigned NOT NULL        COMMENT '是否为男性',
    `roleLevel`         smallint(6) unsigned NOT NULL       COMMENT '角色等级',
    `fightPower`        bigint(20) unsigned NOT NULL        COMMENT '角色战斗力',
    `familyTitle`       tinyint(2) unsigned NOT NULL        COMMENT '联盟官职称号',
    `isJoinWar`         tinyint(3) unsigned NOT NULL        COMMENT '本次是否在联盟战出战列表，如果参战表示在队列中的位置',
    `attackTimes`       smallint(5) unsigned NOT NULL       COMMENT '本轮攻击次数',
    `defendTimes`       smallint(5) unsigned NOT NULL       COMMENT '本轮受击次数',
    `winStar`           smallint(5) unsigned NOT NULL       COMMENT '本轮获得星星数',
    `rewardLevel`       tinyint(3) unsigned NOT NULL        COMMENT '膜拜次数',
    `JoinTime`          int(11) unsigned NOT NULL           COMMENT '加入联盟时间',
    `weeklyContributes` int(11) unsigned NOT NULL           COMMENT '本周贡献度',
    `lastContributeDate` date NOT NULL                      COMMENT '最后贡献的日期',
    `lastRecvEnergyList` varbinary(2000) NOT NULL           COMMENT '上次获得的体力来源玩家列表',
    `storageReqData` varbinary(1000) NOT NULL COMMENT '仓库申请信息',
    `head`              int(11) unsigned NOT NULL           COMMENT '自定义头像',
    `offlineTime`       int(11) unsigned NOT NULL           COMMENT '成员最后下线时间',
    PRIMARY KEY (`roleID`),
    KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gFamilyRequest`;
CREATE TABLE `gFamilyRequest` (
    `roleID`        int(11) unsigned NOT NULL           COMMENT '角色ID',
    `roleName`      varchar(20) NOT NULL                COMMENT '角色名字',
    `roleLevel`     smallint(6) unsigned NOT NULL       COMMENT '角色等级',
    `fightPower`    bigint(20) unsigned NOT NULL        COMMENT '战斗力',
    `timestamp`     int(11) unsigned NOT NULL           COMMENT '申请时间戳',
    `familyID`      int(11) unsigned NOT NULL           COMMENT '申请联盟ID',
    `head`          int(11) unsigned NOT NULL           COMMENT '申请者头像',
    `title`         tinyint(2) unsigned NOT NULL        COMMENT '玩家称号',
    `isMale`            tinyint(1) unsigned NOT NULL        COMMENT '是否为男性',
    UNIQUE KEY `roleID_2` (`roleID`,`familyID`),
    KEY (`roleID`),
    KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gFamilyFight`;
CREATE TABLE `gFamilyFight` (
  `familyID` int(11) unsigned not null comment '联盟id',
  `warPeriod` int(11) unsigned not null comment '联盟战为第几届',
  `isSign` tinyint(2) unsigned not null comment '是否报名',
  `attackTimes` smallint(5) unsigned not null comment '出手次数',
  `defendTimes` smallint(5) unsigned not null comment '被取得的星星数',
  `winStar` smallint(5) unsigned not null comment '本局赢得的星星数',
  `matcherFamilyID` int(11) unsigned not null comment '对手的联盟ID',
  `matcherServerID` int(11) unsigned not null comment '对手的serverID',
  `matcherWinStar` int(11) unsigned not null comment '对手赢得的星星数',
  `matcherWorldRank` int(11) unsigned not null comment '对手的排行',
  `matcherFamilyName` varchar(40) not null comment '对手的联盟名字',
  `result` tinyint(3) unsigned not null comment '战斗结果',
  `lastWorldRank` int(11) unsigned not null comment '上届世界排名',
  PRIMARY KEY (`familyID`) 
)ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gSendEnergy`;
CREATE TABLE `gSendEnergy` (
  `roleID` int(11) unsigned NOT NULL comment '角色ID',
  `lastSendEnergyDate` date not null comment '上次赠送体力的日期',
  `lastSendEnergyList` varbinary(2000) not null comment '上次赠送体力的玩家列表',
  PRIMARY KEY (`RoleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gFamilyFightRecord`;
CREATE TABLE `gFamilyFightRecord`(
  `recordUid` bigint(20) unsigned not null comment '记录uid',
  `familyID` int(11) unsigned not null comment '联盟id',
  `tarFamilyID` int(11) unsigned not null comment '对手联盟id',
  `roleID` int(11) unsigned not null comment '角色ID',
  `tarRoleID` int(11) unsigned not null comment '对手角色ID',
  `warPeriod` int(11) unsigned not null comment '联盟战第几轮',
  `isChallenger` tinyint(3) unsigned not null comment '是否是挑战者',
  `isWin` tinyint(3) unsigned not null comment '是否胜利',
  `winStar` smallint(5) unsigned not null comment '获得星星数量',
  `replay` blob not null comment '具体战斗信息',
  `fTime` datetime not null comment '时间戳',
  `winfamily` varchar(40) not null comment '胜利的家族名称',
  `roleName` varchar(20) not null comment '角色名字',
  `tarRoleName` varchar(20) not null comment '对手角色名字',
  PRIMARY KEY (`recordUid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


DROP TABLE IF EXISTS `gFamilyMasterRecord`;
CREATE TABLE `gFamilyMasterRecord` (
  `familyID` int(11) unsigned not null comment '联盟id',
  `serverID` int(11) unsigned not null comment '联盟的服务器id',
  `score`  int(11) unsigned not null comment '联盟得分',
  `rank` int(11) unsigned not null comment '世界排名',
  `lastWorldRank` int(11) unsigned not null comment '上次世界排名',
  PRIMARY KEY (`familyID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gFamilyTek`;
CREATE TABLE `gFamilyTek`(
  `familyID` int(11) unsigned NOT NULL COMMENT '联盟ID',
  `level1`  smallint(5) unsigned NOT NULL,
  `level2`  smallint(5) unsigned NOT NULL,
  `level3`  smallint(5) unsigned NOT NULL,
  `level4`  smallint(5) unsigned NOT NULL,
  `level5`  smallint(5) unsigned NOT NULL,
  `level6`  smallint(5) unsigned NOT NULL,
  `level7`  smallint(5) unsigned NOT NULL,
  `level8`  smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='联盟科技';

DROP TABLE IF EXISTS `gFamilyWallet`;
CREATE TABLE `gFamilyWallet`(
  `familyID` int(11) unsigned NOT NULL COMMENT '联盟ID',
  `rice` int(11) unsigned NOT NULL COMMENT '军粮',
  PRIMARY KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='联盟直接资源';

DROP TABLE IF EXISTS `gFamilyStorage`;
CREATE TABLE `gFamilyStorage` (
  `itemUID` bigint(11) unsigned NOT NULL comment '道具ID',
  `familyID` int(11) unsigned NOT NULL comment '联盟ID',
  `itemTypeID` smallint(5) unsigned not null comment '道具类型',
  `type` tinyint(3) unsigned not null comment '类型,1 item,2 ger',
  `itemReqBin` varbinary(2000) not null comment '申请列表',
  PRIMARY KEY (`itemUID`) ,
  KEY `familyID` (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='联盟可分配资源仓库';


DROP TABLE IF EXISTS `t_rice_add`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `t_rice_add` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `familyID` int(11) unsigned NOT NULL,
  `rice` int(11) unsigned NOT NULL COMMENT '获得的银两',
  `curRice` int(11) NOT NULL COMMENT '获得前的银两数量',
  `date` date NOT NULL COMMENT '获得日期',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`familyID`),
  KEY `date` (`date`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='军粮获得记录表';
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `t_rice_consume`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `t_rice_consume` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `familyID` int(11) unsigned NOT NULL,
  `rice` int(11) unsigned NOT NULL COMMENT '获得的银两',
  `curRice` int(11) NOT NULL COMMENT '获得前的银两数量',
  `date` date NOT NULL COMMENT '获得日期',
  `time` datetime NOT NULL COMMENT '获得时间',
  `type` smallint(6) unsigned NOT NULL COMMENT '获得来源',
  `argID` int(11) NOT NULL COMMENT '与获得来源相关的：商品ID、武将ID、道具ID、活动ID',
  `desc` varchar(20) CHARACTER SET latin1 NOT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `roleID` (`familyID`),
  KEY `date` (`date`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='军粮消耗记录表';
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `t_storage_add`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `t_storage_add` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `familyID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '新创建的道具UID、或者更新了数量的旧道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `date` date NOT NULL,
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '获得的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(20) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `date` (`date`),
  KEY `roleID` (`familyID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='仓库获得记录';
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `t_storage_consume`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `t_storage_consume` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `familyID` int(11) unsigned NOT NULL,
  `itemUID` bigint(20) unsigned NOT NULL COMMENT '新创建的道具UID、或者更新了数量的旧道具UID',
  `itemTypeID` smallint(6) unsigned NOT NULL COMMENT '道具模版ID',
  `date` date NOT NULL,
  `time` datetime NOT NULL,
  `type` smallint(6) unsigned NOT NULL COMMENT '获得的类型',
  `argID` int(11) DEFAULT NULL COMMENT '参数',
  `desc` varchar(20) DEFAULT NULL COMMENT '附加描述',
  PRIMARY KEY (`id`),
  KEY `date` (`date`),
  KEY `roleID` (`familyID`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='仓库消耗记录';
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gFamilyBoss`;
CREATE TABLE `gFamilyBoss` (
  `familyID` int(11) unsigned NOT NULL comment '联盟ID',
  `value` varbinary(2000) not null comment 'boss信息',
  PRIMARY KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='联盟boss信息';

-- delete from gTeamPk where length(teamPkData) < 5;
ALTER TABLE gTeamPk modify column `teamPkData` varbinary(35000) not null comment '3v3数据';
ALTER TABLE gRole add `extRdActTime` int(11) unsigned not null comment '本地首冲活动时间' after `payExtReward`;
ALTER TABLE `gRoleExtra` ADD `leftChgNameTimes` SMALLINT(5) SIGNED NOT NULL DEFAULT -1 COMMENT '剩余改名次数' AFTER `randomShopList`;
ALTER TABLE gCard add `activityID` smallint(5) unsigned not null comment '活动id' after `drawCount`;
UPDATE `gRoleExtra` SET `battleProgressHard`=0 WHERE `battleProgressHard`>=30001 and roleID >1; -- 加“roleID >1” 是防止某些设置了safe update模式的数据库无法执行这一条命令