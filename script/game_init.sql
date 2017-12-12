-- MySQL dump 10.13  Distrib 5.1.67, for redhat-linux-gnu (x86_64)
--
-- Host: localhost    Database: game
-- ------------------------------------------------------
-- Server version	5.1.67-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `gActivity`
--

DROP TABLE IF EXISTS `gActivity`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gActivity` (
  `roleID` int(11) unsigned NOT NULL,
  `actID` int(5) unsigned NOT NULL,
  `value` varbinary(200) NOT NULL,
  `list` varbinary(1000) NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`actID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gAlien`;
CREATE TABLE `gAlien` (
  `roleID` int(11) unsigned NOT NULL,
  `alienTimes` int unsigned NOT NULL COMMENT '异星战场相关数据',
  `lastRecoverTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
  `resetTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `gBagItem`
--

DROP TABLE IF EXISTS `gBagItem`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gBagItem` (
  `itemUID` bigint(20) unsigned NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `itemTypeID` smallint(5) unsigned NOT NULL,
  `itemNum` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`itemUID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gBanAccount`
--

DROP TABLE IF EXISTS `gBanAccount`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gBanAccount` (
  `accountID` bigint(11) unsigned NOT NULL COMMENT '玩家帐号ID',
  PRIMARY KEY (`accountID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='封禁帐号表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gBestPassChapter`
--

DROP TABLE IF EXISTS `gBestPassChapter`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gBestPassChapter` (
  `roleID` int(11) unsigned NOT NULL,
  `chapterID` smallint(6) unsigned NOT NULL,
  UNIQUE KEY `roleID` (`roleID`,`chapterID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gCard`
--

DROP TABLE IF EXISTS `gCard`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gCard` (
  `roleID` int(11) unsigned NOT NULL,
  `openedCardList` varbinary(1000) NOT NULL COMMENT '已抽取的卡牌,term_to_binary存储',
  `cardList` varbinary(1000) NOT NULL COMMENT '等待抽取的列表,term_to_binary存储',
  `drawCount` int(11) NOT NULL COMMENT '当前的总点击次数',
  `activityID` smallint(5) unsigned not null comment '活动id',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gChapter`
--

DROP TABLE IF EXISTS `gChapter`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gChapter` (
  `roleID` int(11) unsigned NOT NULL,
  `chapterID` int(11) unsigned NOT NULL COMMENT '章节ID',
  `bestRewarded` tinyint(1) NOT NULL DEFAULT '0' COMMENT '是否领取过完美通关奖励',
  `curDate` date NOT NULL,
  `starRewarded` smallint(5) unsigned not null default 0 comment '星星领取奖励',
  PRIMARY KEY (`roleID`,`chapterID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gDungeon`
--

DROP TABLE IF EXISTS `gDungeon`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gDungeon` (
  `roleID` int(11) unsigned NOT NULL,
  `chapterID` smallint(5) unsigned NOT NULL,
  `dungeonID` smallint(5) unsigned NOT NULL,
  `restTimes` smallint(6) unsigned NOT NULL,
  `bestScore` smallint(2) unsigned NOT NULL,
  `resetTimes` smallint(4) unsigned not null default 0 comment '关卡重置次数',
  UNIQUE KEY `roleID_2` (`roleID`,`dungeonID`),
  KEY `roleID` (`roleID`,`chapterID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gETC`
--

DROP TABLE IF EXISTS `gETC`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gETC` (
  `key` tinyint(4) unsigned NOT NULL,
  `value` longblob NOT NULL,
  PRIMARY KEY (`key`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gEquip`
--

DROP TABLE IF EXISTS `gEquip`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gEquip` (
  `itemUID` bigint(20) unsigned NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `itemTypeID` smallint(5) unsigned NOT NULL,
  `itemPos` tinyint(2) unsigned NOT NULL,
  `itemLevel` smallint(4) unsigned NOT NULL,
  `itemRank` tinyint(4) unsigned NOT NULL,
  `itemGerID` bigint(20) unsigned NOT NULL,
  `itemDecay` int(11) unsigned NOT NULL COMMENT '下次的道具品阶衰减时间',
  `itemExp` int(11) DEFAULT '0',
  `itemenchantType` smallint(6) NOT NULL DEFAULT 0 COMMENT '装备附魔类型',
  `itemenchantLevel` smallint(6) NOT NULL DEFAULT 0 COMMENT '装备附魔等级',
  PRIMARY KEY (`itemUID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

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
    `familyTask`          varbinary(3000) NOT NULL        COMMENT '联盟任务信息',
    `cross_rank`          int(11) unsigned NOT NULL DEFAULT 0 COMMENT '全服排名',   
    `talkRoomID`          varchar(25) NOT NULL Default "0" comment '语聊房间id',
    `boss_info`           blob    NOT NULL                COMMENT '公会boss信息',
    `fighter_group`       BLOB    NOT NULL                COMMENT '工会战出战阵容',		
    `familycrossInfo` BLOB NOT NULL comment '阿努比斯之门',
    `familyanubisinfo` BLOB NOT NULL COMMENT '公会阿努比斯赛季记录',	
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
    `limitShop`         varbinary(1000) not null            comment '工会限制商店',
    `anubisinfo`        varbinary(500) NOT NULL             comment '公会成员阿努比斯之门记录',
    `vip`               smallint UNSIGNED NOT NULL          COMMENT 'vip info',
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
    KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Table structure for table `gFighterList`
--

DROP TABLE IF EXISTS `gFighterList`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFighterList` (
  `roleID` int(11) unsigned NOT NULL,
  `fighterList` varbinary(9999) NOT NULL COMMENT '上阵武将列表',
  `lieuInfoList` varbinary(1000) NOT NULL COMMENT '副将数据信息,查看他人出战时使用',
  `lieuAtkAdd` smallint(6) unsigned NOT NULL COMMENT '副将的攻击加成',
  `lieuHpAdd` smallint(6) unsigned NOT NULL COMMENT '副将的血量加成',
  `trSpecial` varbinary(100) not null comment '训练师专精信息',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gFriend`
--

DROP TABLE IF EXISTS `gFriend`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFriend` (
  `roleID` int(11) unsigned NOT NULL,
  `type` tinyint(1) unsigned NOT NULL,
  `friendID` int(11) unsigned NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`type`,`friendID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGather`
--

DROP TABLE IF EXISTS `gGather`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGather` (
  `roleID` int(11) unsigned NOT NULL,
  `type` tinyint(1) unsigned NOT NULL,
  `typeID` int(11) unsigned NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`type`,`typeID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGer`
--

DROP TABLE IF EXISTS `gGer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGer` (
  `gerID` bigint(20) unsigned NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `gerTypeID` smallint(5) unsigned NOT NULL,
  `gerLevel` int(3) unsigned NOT NULL,
  `gerExp` bigint(16) unsigned NOT NULL,
  `gerRank` tinyint(3) unsigned NOT NULL,
  `gerPos` tinyint(2) unsigned NOT NULL,
  `gerAwakeInfo` varbinary(1000) not null comment '精灵觉醒信息',
  `gerBody` MEDIUMINT UNSIGNED NOT NULL DEFAULT 0,
  PRIMARY KEY (`gerID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGift`
--

DROP TABLE IF EXISTS `gGift`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGift` (
  `roleID` int(11) unsigned NOT NULL,
  `type` char(3) NOT NULL COMMENT 'ç¤¼å“ç±»åž‹',
  PRIMARY KEY (`roleID`,`type`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gGuide`
--

DROP TABLE IF EXISTS `gGuide`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gGuide` (
  `roleID` int(11) unsigned NOT NULL,
  `guideState` smallint(6) unsigned NOT NULL COMMENT '新手引导的状态',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gHist`
--

DROP TABLE IF EXISTS `gHist`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gHist` (
  `histUID` bigint(20) unsigned NOT NULL,
  `histType` tinyint(4) unsigned NOT NULL COMMENT '?±???????4=pvp?????￥???5=?¤o????????￥',
  `name` varchar(15) NOT NULL COMMENT 'å¯¹æ‰‹åå­—',
  `enemyID` int(11) NOT NULL COMMENT 'å¯¹æ‰‹çš„çŽ©å®¶ID',
  `time` int(11) NOT NULL COMMENT 'Unixæ—¶é—´æˆ³',
  `arg` mediumint(9) unsigned NOT NULL COMMENT 'é™„å¸¦å‚æ•°',
  `isRead` tinyint(1) unsigned NOT NULL COMMENT 'æ˜¯å¦è¯»è¿‡è¿™å°æˆ˜æŠ¥',
  `fightInfo` varbinary(2000) NOT NULL COMMENT '????–—??????',
  `roleID` int(11) unsigned NOT NULL,
  `type` tinyint(1) unsigned NOT NULL COMMENT '战报大类型：4=争霸，5=夺宝',
  PRIMARY KEY (`histUID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='çŽ©å®¶æˆ˜æŠ¥';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gHron`
--

DROP TABLE IF EXISTS `gHron`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gHron` (
  `roleID` int(11) unsigned NOT NULL,
  `date` date NOT NULL COMMENT '数据日期，活动每天开一次，每天的数据对应一个日期,验证使用',
  `star` tinyint(2) unsigned NOT NULL COMMENT '4个星级',
  `curDungeonNum` smallint(6) unsigned NOT NULL COMMENT '当前第几关，0=当前未开始挑战',
  `attackAdd` smallint(6) unsigned NOT NULL COMMENT '当前攻击增加百分比',
  `hpAdd` smallint(6) unsigned NOT NULL COMMENT '当前血量增加百分比',
  `morale` smallint(6) unsigned NOT NULL COMMENT '士气',
  `dungeonIDList` varbinary(100) NOT NULL COMMENT '随机出来的关卡列表',
  `bestScore` tinyint(4) unsigned NOT NULL COMMENT '最高关数',
  `challengeTimes` tinyint(4) unsigned NOT NULL COMMENT '已挑战次数',
  `isHaveSuccReward` smallint(6) unsigned NOT NULL COMMENT '记录通过有通关奖励的华容道关卡',
  `lastFightResult` smallint(6) unsigned NOT NULL COMMENT '记录上一次挑战关卡的输赢',
  `isSelect` tinyint(2) unsigned not null comment '是否选择了星级',
  `history` varbinary(100) NOT NULL comment '无尽深渊历史数据',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gInvite`
--

DROP TABLE IF EXISTS `gInvite`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gInvite` (
  `roleID` int(11) unsigned NOT NULL,
  `rewardNum` smallint(6) unsigned NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gInviteRoleList`
--

DROP TABLE IF EXISTS `gInviteRoleList`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gInviteRoleList` (
  `roleID` int(11) unsigned NOT NULL,
  `inviteRoleID` int(11) unsigned NOT NULL,
  UNIQUE KEY `inviteRoleID` (`inviteRoleID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gKing`
--

DROP TABLE IF EXISTS `gKing`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gKing` (
  `key` tinyint(4) unsigned NOT NULL,
  `value` longblob NOT NULL,
  PRIMARY KEY (`key`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gLimit`
--

DROP TABLE IF EXISTS `gLimit`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gLimit` (
  `roleID` int(11) unsigned NOT NULL,
  `encounterNum` tinyint(2) unsigned NOT NULL COMMENT '奇遇解锁数量',
  `isBindWeibo` tinyint(1) NOT NULL COMMENT '是否绑定了微博',
  `inviteRoleID` int(11) unsigned NOT NULL COMMENT '邀请你的玩家ID',
  `inviteRoleName` varchar(20) CHARACTER SET utf8 DEFAULT NULL COMMENT '要请你的角色名称',
  `lastShareLevel` smallint(4) unsigned NOT NULL COMMENT '上次微博分享的主公等级',
  `spiritGoldBoxCount` int(11) unsigned NOT NULL COMMENT '付费元宝召唤精灵单抽宝箱计数',
  `spiritGoldBonusBoxCount` int(11) unsigned NOT NULL COMMENT '赠送元宝召唤精灵单抽宝箱计数',
  `spiritItemBonusBoxCount` int(11) unsigned NOT NULL COMMENT '道具召唤精灵抽取宝箱计数',
  `equipGoldBoxCount` int(11) unsigned NOT NULL COMMENT '付费元宝祈祷装备单抽宝箱计数',
  `equipGoldBonusBoxCount` int(11) unsigned NOT NULL COMMENT '赠送元宝宝祈祷装备单抽宝箱计数',
  `equipItemBonusBoxCount` int(11) unsigned NOT NULL COMMENT '道具祈祷装备抽宝箱计数',
  `trainerGoldBonusBoxCount` INT UNSIGNED NOT NULL,
  `trainerGoldBoxCount` INT UNSIGNED NOT NULL,
  `trainerItemBoxCount` INT UNSIGNED NOT NULL,
  `spiritRefresh` tinyint(2) NOT NULL DEFAULT 0,
  `equipRefresh` tinyint(2) NOT NULL DEFAULT 0,
  `trainerRefresh` tinyint(2) NOT NULL DEFAULT 0,
  `combine2Value` tinyint(2) NOT NULL DEFAULT 0,
  `ticketBoxCount` int(8) unsigned  NOT NULL comment '点券次数记录',
  `ticketBoxCount2` int(8) unsigned  NOT NULL comment '十连抽点券次数记录',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gMail`
--

DROP TABLE IF EXISTS `gMail`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gMail` (
  `mailUID` bigint(20) unsigned NOT NULL COMMENT 'é‚®ä»¶å”¯ä¸€ID',
  `recvID` int(11) unsigned NOT NULL COMMENT 'é‚®ä»¶æŽ¥å—è€…çš„çŽ©å®¶ID',
  `mailType` tinyint(1) unsigned NOT NULL COMMENT 'é‚®ä»¶ç±»åž‹',
  `senderID` int(11) unsigned NOT NULL COMMENT 'é‚®ä»¶å‘é€è€…çš„UIDï¼Œ0=ç³»ç»Ÿå‘é€çš„',
  `senderName` varchar(20) NOT NULL COMMENT 'å‘é€è€…åå­—,å¦‚æžœæœ¬é‚®ä»¶æ˜¯ç³»ç»Ÿé‚®ä»¶ï¼Œåˆ™æ­¤å­—æ®µä¸ºç©º',
  `content` varchar(300) NOT NULL COMMENT 'å†…å®¹',
  `time` int(11) unsigned NOT NULL COMMENT 'å‘é€æ—¶é—´(Unixæ—¶é—´æˆ³)',
  `mailTemplateID` smallint(6) unsigned NOT NULL COMMENT 'é‚®ä»¶æ¨¡ç‰ˆID',
  `paramList` varbinary(300) NOT NULL COMMENT '??¨?€??±?????????°??—è?¨',
  `mailReward` varbinary(300) NOT NULL COMMENT '??ˉé￠???–?￥–??±',
  `isRead` tinyint(1) unsigned NOT NULL COMMENT 'æ˜¯å¦è¢«é˜…è¯»è¿‡',
  `head` int(11) unsigned NOT NULL DEFAULT '0'  COMMENT '邮件发件人头像',
  `isMale` tinyint(1) unsigned NOT NULL DEFAULT '0'  COMMENT '邮件发件人性别',
  PRIMARY KEY (`mailUID`),
  KEY `recvID` (`recvID`),
  KEY `senderID` (`senderID`),
  KEY `mailType` (`mailType`),
  KEY `isRead` (`isRead`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='é‚®ä»¶ä¿¡æ¯';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gOfflineDeductGold`
--

DROP TABLE IF EXISTS `gOfflineDeductGold`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gOfflineDeductGold` (
  `roleID` int(11) unsigned NOT NULL,
  `deductGold` int(11) unsigned NOT NULL COMMENT '离线时已扣取的元宝',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gOfflinePayLog`
--

DROP TABLE IF EXISTS `gOfflinePayLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gOfflinePayLog` (
  `roleID` int(11) unsigned NOT NULL,
  `payItemID` int(11) unsigned NOT NULL COMMENT 'å……å€¼å•†å“ID',
  `receipt` varchar(3000) NOT NULL,
  `receiptMd5` varchar(40) NOT NULL,
  `SrcType` tinyint(4) NOT NULL,
  UNIQUE KEY `receiptMd5` (`receiptMd5`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gPatch`
--

DROP TABLE IF EXISTS `gPatch`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gPatch` (
  `roleID` int(11) unsigned NOT NULL,
  `patchID` smallint(5) unsigned NOT NULL COMMENT '宝物碎片的道具类型ID',
  `num` smallint(5) unsigned NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='玩家拥有的宝物碎片信息';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gPay`
--

DROP TABLE IF EXISTS `gPay`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gPay` (
  `receiptMd5` varchar(40) NOT NULL,
  `roleID` int(11) unsigned NOT NULL,
  `receipt` varchar(3000) NOT NULL,
  `srcType` tinyint(4) NOT NULL,
  `time` datetime NOT NULL,
  `payGold` int(11) unsigned NOT NULL,
  PRIMARY KEY (`receiptMd5`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gPush`
--

DROP TABLE IF EXISTS `gPush`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gPush` (
  `roleID` int(11) unsigned NOT NULL,
  `token` varchar(1000) NOT NULL,
  `isPVPPushOpen` tinyint(1) NOT NULL,
  `isPushNightMute` tinyint(1) NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gReplay`
--

DROP TABLE IF EXISTS `gReplay`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gReplay` (
  `replayUID` bigint(20) unsigned NOT NULL,
  `replay` blob NOT NULL COMMENT '战斗录像具体内容。term_to_binary序列化。',
  `time` datetime NOT NULL COMMENT '战斗发生的时间，用来做定期的维护，过久的删除',
  `type` tinyint unsigned NOT NULL DEFAULT '0' COMMENT '战报的所属的功能模块',
  `isCompress` tinyint unsigned NOT NULL DEFAULT '0' COMMENT '是否压缩',
  PRIMARY KEY (`replayUID`),
  KEY `time` (`time`),
  KEY `type` (`type`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gRole`
--

DROP TABLE IF EXISTS `gRole`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gRole` (
  `roleID` int(11) unsigned NOT NULL COMMENT '玩家ID',
  `accid` bigint(11) unsigned NOT NULL COMMENT '帐号ID',
  `roleName` varchar(20) NOT NULL COMMENT '主公名字',
  `isMale` tinyint(1) NOT NULL COMMENT '性别',
  `level` smallint(6) unsigned NOT NULL DEFAULT '1' COMMENT '主公等级',
  `exp` bigint(16) unsigned NOT NULL DEFAULT '0' COMMENT '主公经验',
  `coin` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '银两',
  `reputation` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '声望',
  `gold` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '元宝',
  `goldBonus` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '官方赠送元宝',
  `goldUsed` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '花费元宝获得的积分',  
  `unioncoin` int(13) NOT NULL DEFAULT '0' COMMENT '公会货币',
  `profoundCrystal` int(13) NOT NULL DEFAULT '0'  COMMENT '奥义结晶',
  `vipLevel` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'VIP等级',
  `goldTotalPaid` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '总充值元宝',
  `title` tinyint(2) unsigned NOT NULL DEFAULT '0' COMMENT '官爵',
  `fightPower` bigint(14) unsigned NOT NULL DEFAULT '0' COMMENT '总战斗力',
  `lastLogoutTime` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '上次下线时间',
  `familyID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '玩家的联盟ID',
  `lastJoinFamily` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '最近一次加入联盟的时间',
  `head` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '选取的头像，0：默认头像',
  `payExtReward` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '充值额外奖励记录',
  `extRdActTime` int(11) unsigned not null comment '本地首冲活动时间',
  `location` varchar(30) NOT NULL DEFAULT '' COMMENT '地理位置',
  `isFailed` tinyint(1) NOT NULL DEFAULT '0' COMMENT '是否关卡战败过',
  `devid` varchar(100) NOT NULL COMMENT 'push token',
  `srcType` smallint unsigned NOT NULL DEFAULT '0' COMMENT '渠道ID',
  `lastLoginTime` int(11) unsigned not null comment '上次登录时间',
  `tasklevel` SMALLINT UNSIGNED NOT NULL DEFAULT 0,
  `plane_level` SMALLINT UNSIGNED NOT NULL DEFAULT 1 COMMENT '卡洛斯飞机等级',
  `teamid` bigint(20) NOT NULL DEFAULT -1 COMMENT '组队ID',
  `honor` int(13) NOT NULL DEFAULT '0'  COMMENT '荣誉',
  `pvppoint` int(13) NOT NULL DEFAULT '0'  COMMENT '竞技场点数',
  carloswintime INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯胜利次数',
  carlosequaltime INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯平局次数',
  carloslosetime INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯失败次数',
  carlosseason INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯赛季编号',
  carlosprewintime INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯胜利次数',
  carlospreequaltime INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯平局次数',
  carlosprelosetime INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯失败次数',
  carlospreseason INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯赛季编号',
  `home_resource` int(13) NOT NULL DEFAULT '0'  COMMENT '家园资源',
  firtPayStatus tinyint(2) unsigned not null comment '首冲活动',
  `ticket` int(10) unsigned not null comment '点券',
  `laputastone` int(11) unsigned not null comment '飞行币',
  `transmigration` INT UNSIGNED NOT NULL DEFAULT 0 ,
  `sGoldTotalPaid` INT UNSIGNED NOT NULL comment 'svip充值记录',
  `svipLevel` tinyINT UNSIGNED NOT NULL comment 'svip level',
  PRIMARY KEY (`roleID`),
  UNIQUE KEY `accid` (`accid`),
  UNIQUE KEY `roleName` (`roleName`),
  KEY `vipLevel` (`vipLevel`),
  KEY `level` (`level`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gRoleExtra`
--

DROP TABLE IF EXISTS `gRoleExtra`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gRoleExtra` (
  `roleID` int(11) unsigned NOT NULL,
  `battleProgress` smallint(5) unsigned NOT NULL,
  `battleProgressHard` smallint(5) unsigned NOT NULL COMMENT '炼狱难度的战役推图',
  `battleProgressFastHard` smallint(5) unsigned NOT NULL COMMENT '最难难度的战役推图',
  `battleProgressTransmigration` SMALLINT UNSIGNED NOT NULL DEFAULT 0,
  `energy` smallint(6) unsigned NOT NULL,
  `energyBuyTimes` smallint(5) unsigned NOT NULL COMMENT '体力已购买次数',
  `challengeGodEnergy` smallint(6) unsigned NOT NULL COMMENT '神将录挑战次数',
  `challengeGodBuyTimes` smallint(5) unsigned NOT NULL COMMENT '购买神将录挑战的次数',
  `lastChallengeGodDate` date NOT NULL COMMENT '上次获得挑战神将录次数的日期',
  `refreshLieuTimes` smallint(5) unsigned NOT NULL COMMENT '副将免费刷新次数',
  `alreadyPayRefreshLieuTimes` mediumint(9) unsigned NOT NULL DEFAULT 0  COMMENT '元宝刷新副将次数累计',
  `dscvBuyTimes` smallint(5) unsigned NOT NULL COMMENT '探索已购买次数',
  `pvpBuyTimes` smallint(5) unsigned NOT NULL COMMENT '争霸已购买次数',
  `plunderBuyTimes` smallint(5) unsigned NOT NULL COMMENT '夺宝力已购买次数',
  `coinBuyTimes` smallint(6) unsigned NOT NULL COMMENT '银两购买次数',
  `fireTimes` INT UNSIGNED NOT NULL COMMENT '放鞭炮次数',
  `lastBuyTimesRefreshDate` date NOT NULL,
  `lastEnergyTime` int(11) unsigned NOT NULL,
  `discoveryTimes` smallint(5) unsigned NOT NULL,
  `lastDscvTime` int(11) unsigned NOT NULL,
  `dscvCount` int(11) unsigned NOT NULL,
  `pvpTimes` smallint(5) unsigned NOT NULL,
  `lastPvpTime` int(11) unsigned NOT NULL,
  `plunderTimes` smallint(5) unsigned NOT NULL,
  `lastPlunderTime` int(11) unsigned NOT NULL,
  `weiboCount` smallint(5) unsigned NOT NULL,
  `nextWeiboCountRefreshSec` int(11) unsigned NOT NULL,
  `lastWeiXinShareSec` int(11) unsigned not null COMMENT '上次微信分享的时间',
  `encounterList` varbinary(3000) NOT NULL COMMENT '遭遇战列表,term_to_binary存储',
  `lastTitleRewardDate` date NOT NULL COMMENT '上次领取官爵奖励的时间',
  `lastDrawTitle` tinyint(4) unsigned NOT NULL COMMENT '上次领取官爵奖励时的官爵',
  `lastLoggedLoginDate` date NOT NULL COMMENT '上次记录下来的登录日期',
  `lastDrawLoginRewardDays` int(11) unsigned NOT NULL COMMENT '上次领取连续登录奖励的连续登录日数',
  `loginDays` int(11) unsigned NOT NULL COMMENT '当前的连续登录日数',
  `lastDrawLevelUpLevel` smallint(6) unsigned NOT NULL COMMENT '上次领取的升级礼包的等级',
  `randomShopList` varbinary(1000) NOT NULL COMMENT '奇遇商店列表',
  `leftChgNameTimes` SMALLINT(5) SIGNED NOT NULL DEFAULT -1 COMMENT '剩余改名次数',
  `talentStudyBuyTimes` smallint(5) unsigned NOT NULL COMMENT '当天购买天赋冷却的次数',
  `lastPayTime` INT NOT NULL  COMMENT '增加上次充值的时间',
  `magicBookState` varchar(200) NOT NULL DEFAULT '' COMMENT '魔典状态',
  `teamPkTimes` smallint(6) unsigned NOT NULL COMMENT '3v3次数',
  `teampPkBuyTimes` smallint(5) unsigned NOT NULL COMMENT '3v3已购买次数',
  `lastTeamPkTime` int(11) unsigned NOT NULL COMMENT '3v3上次恢复时间',
  `sign_day_count` smallint(6) NOT NULL DEFAULT 0 COMMENT '玩家签到天数',
  `is_get_sign_reward` smallint(6) NOT NULL DEFAULT 0 COMMENT '玩家是否领取签到奖励',
  `last_sign_time` int(11) NOT NULL DEFAULT 0 COMMENT '玩家上次签到时间',
  `is_get_acc_sign_reward` int(11) NOT NULL DEFAULT 0 COMMENT '玩家已经领取累积签到奖励天数',
  `battleBossReward` varbinary(2000) NOT NULL DEFAULT -1 COMMENT '玩家关卡BOSS宝箱领取信息',
  `maingerTypeid` smallint(5) NOT NULL DEFAULT 0 COMMENT '玩家主宠',
  `plane_ai_flag` int(11) NOT NULL DEFAULT 0 COMMENT '用于飞行基地AI',
  `maintask` varbinary(200)  NOT NULL COMMENT '玩家当前主线任务信息',
  `signSec` int(10) unsigned not null comment '注册时间',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gItemUse`;
CREATE TABLE `gItemUse` (
    `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
    `itemTypeID`  smallint(5) unsigned NOT NULL  COMMENT '道具模版ID',
    `useDate`     date NOT NULL COMMENT '最近一次使用日期',
    `useTimes`    tinyint unsigned NOT NULL  COMMENT '当日累积使用次数',
    UNIQUE KEY `roleID_2` (`roleID`,`itemTypeID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `gShopNum`
--

DROP TABLE IF EXISTS `gShopNum`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gShopNum` (
  `roleID` int(11) unsigned NOT NULL,
  `shopID` smallint(6) unsigned NOT NULL,
  `sellID` smallint(6) unsigned NOT NULL,
  `buyNum` smallint(6) unsigned NOT NULL,
  UNIQUE KEY `roleID_2` (`roleID`,`shopID`,`sellID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

DROP TABLE IF EXISTS `gTeamPk`;
CREATE TABLE `gTeamPk` (
  `roleID` int(11) unsigned NOT NULL,
  `teamPkData` varbinary(60000) NOT NULL COMMENT '3v3相关数据',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `test`
--

DROP TABLE IF EXISTS `test`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `test` (
  `id` int(11) NOT NULL,
  `value` varbinary(2000) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `user_online`
--

DROP TABLE IF EXISTS `user_online`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_online` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `time` char(10) CHARACTER SET latin1 NOT NULL,
  `num` mediumint(9) NOT NULL,
  `online` date NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 CHECKSUM=1 DELAY_KEY_WRITE=1;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

DROP TABLE IF EXISTS `gtreasurepatch`;
CREATE TABLE `gtreasurepatch` (
  `roleID` int(11) unsigned NOT NULL,
  `21201` tinyint(8) unsigned NOT NULL,
  `21202` tinyint(8) unsigned NOT NULL,
  `21203` tinyint(8) unsigned NOT NULL,
  `21204` tinyint(8) unsigned NOT NULL,
  `21205` tinyint(8) unsigned NOT NULL,
  `21211` tinyint(8) unsigned NOT NULL,
  `21212` tinyint(8) unsigned NOT NULL,
  `21213` tinyint(8) unsigned NOT NULL,
  `21214` tinyint(8) unsigned NOT NULL,
  `21215` tinyint(8) unsigned NOT NULL,
  `21216` tinyint(8) unsigned NOT NULL,
  `21217` tinyint(8) unsigned NOT NULL,
  `21218` tinyint(8) unsigned NOT NULL,
  `21219` tinyint(8) unsigned NOT NULL,
  `21220` tinyint(8) unsigned NOT NULL,
  `21231` tinyint(8) unsigned NOT NULL,
  `21232` tinyint(8) unsigned NOT NULL,
  `21233` tinyint(8) unsigned NOT NULL,
  `21234` tinyint(8) unsigned NOT NULL,
  `21235` tinyint(8) unsigned NOT NULL,
  `21241` tinyint(8) unsigned NOT NULL,
  `21242` tinyint(8) unsigned NOT NULL,
  `21243` tinyint(8) unsigned NOT NULL,
  `21244` tinyint(8) unsigned NOT NULL,
  `21245` tinyint(8) unsigned NOT NULL,
  `21251` tinyint(8) unsigned NOT NULL,
  `21252` tinyint(8) unsigned NOT NULL,
  `21253` tinyint(8) unsigned NOT NULL,
  `21254` tinyint(8) unsigned NOT NULL,
  `21255` tinyint(8) unsigned NOT NULL,
  `21261` tinyint(8) unsigned NOT NULL,
  `21262` tinyint(8) unsigned NOT NULL,
  `21263` tinyint(8) unsigned NOT NULL,
  `21264` tinyint(8) unsigned NOT NULL,
  `21265` tinyint(8) unsigned NOT NULL,
  `21266` tinyint(8) unsigned NOT NULL,
  `21267` tinyint(8) unsigned NOT NULL,
  `21268` tinyint(8) unsigned NOT NULL,
  `21269` tinyint(8) unsigned NOT NULL,
  `21270` tinyint(8) unsigned NOT NULL,
  `21206` tinyint(8) unsigned NOT NULL,
  `21207` tinyint(8) unsigned NOT NULL,
  `21208` tinyint(8) unsigned NOT NULL,
  `21209` tinyint(8) unsigned NOT NULL,
  `21210` tinyint(8) unsigned NOT NULL,
  `21221` tinyint(8) unsigned NOT NULL,
  `21222` tinyint(8) unsigned NOT NULL,
  `21223` tinyint(8) unsigned NOT NULL,
  `21224` tinyint(8) unsigned NOT NULL,
  `21225` tinyint(8) unsigned NOT NULL,
  `21226` tinyint(8) unsigned NOT NULL,
  `21227` tinyint(8) unsigned NOT NULL,
  `21228` tinyint(8) unsigned NOT NULL,
  `21229` tinyint(8) unsigned NOT NULL,
  `21230` tinyint(8) unsigned NOT NULL,
  `21236` tinyint(8) unsigned NOT NULL,
  `21237` tinyint(8) unsigned NOT NULL,
  `21238` tinyint(8) unsigned NOT NULL,
  `21239` tinyint(8) unsigned NOT NULL,
  `21240` tinyint(8) unsigned NOT NULL,
  `21246` tinyint(8) unsigned NOT NULL,
  `21247` tinyint(8) unsigned NOT NULL,
  `21248` tinyint(8) unsigned NOT NULL,
  `21249` tinyint(8) unsigned NOT NULL,
  `21250` tinyint(8) unsigned NOT NULL,
  `21256` tinyint(8) unsigned NOT NULL,
  `21257` tinyint(8) unsigned NOT NULL,
  `21258` tinyint(8) unsigned NOT NULL,
  `21259` tinyint(8) unsigned NOT NULL,
  `21260` tinyint(8) unsigned NOT NULL,
  PRIMARY KEY(`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


DROP TABLE IF EXISTS `gEncounter`;
CREATE TABLE `gEncounter` (
	`roleID` int(11) unsigned NOT NULL,
	`monsterRank` tinyint(8) unsigned NOT NULL,
	`chapterInfoList` longblob NOT NULL,
	PRIMARY KEY(`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


DROP TABLE IF EXISTS `gLieuInfo`;
CREATE TABLE `gLieuInfo`(
  `roleID` int(11) unsigned NOT NULL ,
  `lieuInfo` blob NOT NULL COMMENT '玩家的参军信息',
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gTalk`;
CREATE TABLE `gTalk`(
	`roleID` int(11) unsigned NOT NULL,
	`gag_list` blob NOT NULL COMMENT '玩家屏蔽的世界聊天发言人',
	PRIMARY KEY(`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gOtherRecord`;
CREATE TABLE `gOtherRecord`(
	`roleID` int(11) unsigned NOT NULL,
	`lastSignDate` date NOT NULL COMMENT '上次签到的日期',
	`signedDays` tinyint(4) unsigned NOT NULL COMMENT '连续签到的天数',
	`isEmperor` tinyint(4) unsigned NOT NULL COMMENT '是否是帝王',
	`isGetBox` tinyint(4) unsigned NOT NULL COMMENT '是否已经领取连续签到的宝箱',
	PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gFailedPayLog`;
CREATE TABLE `gFailedPayLog` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `roleID` int(11) unsigned NOT NULL,
  `receipt` varchar(3000) NOT NULL,
  `srcType` tinyint(4) NOT NULL,
  `time` datetime NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='充值失败单号记录表';


DROP TABLE IF EXISTS `gTreasureHouse`;
CREATE TABLE `gTreasureHouse`(
	`roleID` int(11) unsigned NOT NULL COMMENT '玩家的角色ID',
	`value_info` tinyint(4) unsigned NOT NULL COMMENT '玩家当前的倍率值',
	`card_list` varbinary(3000)  NOT NULL COMMENT '玩家获当前的显示道具列表',
	`free_count` int(9) unsigned NOT NULL COMMENT '玩家免费探索的次数',
	`buy_count` int(9) unsigned NOT NULL COMMENT '玩家付费探索的次数',
	`free_times` tinyint(5) unsigned NOT NULL COMMENT '玩家当日免费探索次数',
	`mark` int(11) unsigned NOT NULL COMMENT '玩家探索得到的分数',
	`baseBoxGetProcess` varbinary(1000) NOT NULL COMMENT '玩家获取的保底宝箱列表',
	`isGetRankReward` tinyint(4) unsigned NOT NULL COMMENT '玩家是否领取了排行榜奖励',
	`lastExploreDate` date COMMENT '玩家上次探索的日期',
	`activityID` mediumint(8) unsigned	NOT NULL COMMENT '活动ID',
	PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gFire`;
CREATE TABLE `gFire` (
	`roleID` int(11) unsigned NOT NULL COMMENT '角色ID',
	`gold` int(11) unsigned NOT NULL COMMENT '消费的元宝',
	PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='燃放鞭炮消费的元宝';


DROP TABLE IF EXISTS `gRebate`;
CREATE TABLE `gRebate` (
  `roleID` int(10) unsigned NOT NULL COMMENT '角色ID',
  `value` longblob NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='返利记录';


DROP TABLE IF EXISTS `gTask`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gTask` (
  `roleID` int(11) unsigned NOT NULL  COMMENT '角色ID',
  `taskID` int(11) unsigned NOT NULL COMMENT '任务ID',
  `status`  int(3) unsigned NOT NULL COMMENT '任务状态',
  `triggerNum`  int(11) unsigned NOT NULL COMMENT '触发进度',
  `triggerNotes`  blob NOT NULL COMMENT '任务触发条件记录',
  UNIQUE KEY `roleID_2` (`roleID`,`taskID`),
  KEY `taskID` (`taskID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gHomestead`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gHomestead` (
  `roleID` 		int(11) unsigned NOT NULL  COMMENT '角色ID',
  `roleName`   varchar(20) NOT NULL    COMMENT '角色名称',
  `addEnergyTimes`  int(3) unsigned NOT NULL COMMENT '充能剩余次数',
  `matingTimes`  int(3) unsigned NOT NULL COMMENT '交配剩余次数',
  `matingCoolSecond`  int(11) unsigned NOT NULL COMMENT '交配冷却时间',
  `add4mating`  int(3) unsigned NOT NULL COMMENT '交配加成',
  `gerID`  bigint(20) unsigned NOT NULL COMMENT '守护宠物UID',
  `gerTypeID` smallint(5) unsigned NOT NULL COMMENT '守护宠物类型',
  `quality` tinyint(3) unsigned NOT NULL COMMENT '守护宠物品阶',
  `level` tinyint(3) unsigned NOT NULL COMMENT '守护宠物等级',
  `refreshMatingSecond` int(11) unsigned NOT NULL COMMENT '上一次刷新交配次数时间',
  `machineList`  blob NOT NULL COMMENT '机器列表',
  `logList`  blob NOT NULL COMMENT '日志列表',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gFriendEnargy`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFriendEnargy` (
  `roleID` 		int(11) unsigned NOT NULL  COMMENT '角色ID',
  `refreshDate`  date NOT NULL COMMENT '上次次数刷新日期',
  `giveTimes`  int(11) NOT NULL COMMENT '剩余领取体力次数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gToFriend`;
CREATE TABLE `gToFriend` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  `isGive`      tinyint(1) unsigned NOT NULL  COMMENT '是否赠送',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gToMe`;
CREATE TABLE `gToMe` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gAddFriend`;
CREATE TABLE `gAddFriend` (
  `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
  `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
  `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
  UNIQUE KEY `roleID_2` (`roleID`,`friendID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `t_gold_pay_add`;
CREATE TABLE IF NOT EXISTS `t_gold_pay_add` (
    `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
    `roleID` int(11) unsigned NOT NULL,
    `vipLevel` tinyint(4) unsigned  NOT NULL,
    `gold` int(11) unsigned NOT NULL,
    `curGold` int(11) unsigned NOT NULL,
    `time` datetime NOT NULL,
    `appItemID` int(8) unsigned NOT NULL,
    `desc` varchar(20) CHARACTER SET latin1 NOT NULL,
    `receiptMd5` varchar(40) NOT NULL,
     `accid` bigint(11) unsigned NOT NULL COMMENT '帐号ID',
     `devid` varchar(100) NOT NULL COMMENT '设备号',
     `srcType` smallint NOT NULL COMMENT '渠道id',
    PRIMARY KEY(`id`),
    KEY `roleID` (`roleID`),
    KEY `time` (`time`),
    KEY `appItemID` (`appItemID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `logGuide`;
CREATE TABLE `logGuide` (
  `roleID` int(11) unsigned NOT NULL,
  `guideState` smallint(6) unsigned NOT NULL COMMENT '新手引导的状态',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


DROP TABLE IF EXISTS `gOfflinePayAmountLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gOfflinePayAmountLog` (
  `roleID` int(11) unsigned NOT NULL,
  `amount` int(11) unsigned NOT NULL COMMENT '充值额度',
  `receipt` varchar(3000) NOT NULL,
  `receiptMd5` varchar(40) NOT NULL,
  `SrcType` tinyint(4) NOT NULL,
  UNIQUE KEY `receiptMd5` (`receiptMd5`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

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
  `totalPower` int(11) unsigned not null comment '上次报名时记录的战斗力',
  PRIMARY KEY (`familyID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

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

DROP TABLE IF EXISTS `gBoxInfo`;
CREATE  TABLE IF NOT EXISTS `gBoxInfo` (
  `roleID` INT UNSIGNED NOT NULL,
  `gerTime1` TINYINT UNSIGNED NOT NULL comment '已抽取免费道具抽取精灵次数',
  `gerTime2` TINYINT UNSIGNED NOT NULL comment '已抽取免费钻石抽取精灵次数',
  `itemTime1` TINYINT UNSIGNED NOT NULL comment '已抽取免费道具抽取装备次数',
  `itemTime2` TINYINT UNSIGNED NOT NULL comment '已抽取免费钻石抽取装备次数',
  `trainerTime1` TINYINT UNSIGNED NOT NULL comment '已抽取免费道具抽取训练师次数',
  `trainerTime2` TINYINT UNSIGNED NOT NULL comment '已抽取免费钻石抽取训练师次数',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='免费抽卡信息';

DROP TABLE IF EXISTS `gStoneChip`;
CREATE TABLE `gStoneChip` (
  `roleID` int(11) unsigned NOT NULL,
  `stoneType` int(11) unsigned NOT NULL,
  `firstNum` int(11) unsigned NOT NULL,
  `secondNum` int(11) unsigned NOT NULL,
  `thirdNum` int(11) unsigned NOT NULL,
  `fourthNum` int(11) unsigned NOT NULL,
  UNIQUE KEY `roleID_stoneType` (`roleID`,`stoneType`),
  KEY `stoneType` (`stoneType`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gPlunder`;
CREATE TABLE `gPlunder` (
  `roleID` int(11) unsigned NOT NULL,
  `protectEndTime` int(11) unsigned NOT NULL comment '保护结束的时间',
  `restAttackTimes` int(11) unsigned NOT NULL comment '剩余攻击次数',
  `buyTimes` int(11) unsigned NOT NULL comment '剩余购买次数',
  `lastTimestamp`   VARBINARY(1000) NOT NULL comment '上次更新数据的时间，主要是用于隔天初始化数据用'  ,
  `recoverTimestamp`     int(11) unsigned NOT NULL comment '夺取回复时间戳',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gFamilySortInfo`;
CREATE TABLE `gFamilySortInfo` ( 
    `familyID`            int(11) unsigned NOT NULL       COMMENT '联盟ID',
    `score`               int(11) unsigned NOT NULL       COMMENT '公会站积分',
    `familyLevel`         smallint(6) unsigned NOT NULL   COMMENT '联盟等级',
    `fightPower`          bigint(20) unsigned NOT NULL    COMMENT '公会总战力',
    `familyName`          varchar(20) NOT NULL            COMMENT '联盟名称',
    `ownerRoleName`       varchar(20) NOT NULL            COMMENT '盟主名称',
    `curMembers`          smallint(4) unsigned NOT NULL   COMMENT '当前成员数量',
    `rank`                int(11) unsigned NOT NULL       COMMENT '联盟排名',
    `notice`              varchar(200) NOT NULL           COMMENT '公告',
    `ownerRoleID`         int(11) unsigned NOT NULL       COMMENT '盟主roleID',
    `worldRank`           int(11) unsigned NOT NULL       COMMENT '联盟战世界排名',
    `slogan`              varchar(200) NOT NULL           COMMENT '口号',
    `serverID`            int(11) unsigned NOT NULL       COMMENT '服务器ID',
    PRIMARY KEY(`familyID`),
    INDEX `order` (`score`, `familyLevel`, `fightPower`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
DROP TABLE IF EXISTS `gFamilyTek`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFamilyTek` (
  `familyID` int(11) unsigned NOT NULL COMMENT "公会ID",
  `level1` smallint(5) unsigned NOT NULL,
  `level2` smallint(5) unsigned NOT NULL,
  `level3` smallint(5) unsigned NOT NULL,
  `level4` smallint(5) unsigned NOT NULL,
  `level5` smallint(5) unsigned NOT NULL,
  `level6` smallint(5) unsigned NOT NULL,
  `level7` smallint(5) unsigned NOT NULL,
  `level8` smallint(5) unsigned NOT NULL,
  `level9` smallint(5) unsigned NOT NULL,
  `level10` smallint(5) unsigned NOT NULL,
  `level11` smallint(5) unsigned NOT NULL,
  `level12` smallint(5) unsigned NOT NULL,
  `level13` smallint(5) unsigned NOT NULL,
  `level14` smallint(5) unsigned NOT NULL,
  `level15` smallint(5) unsigned NOT NULL,
  `level16` smallint(5) unsigned NOT NULL,
  `level17` smallint(5) unsigned NOT NULL,
  `level18` smallint(5) unsigned NOT NULL,
  `level19` smallint(5) unsigned NOT NULL,
  `level20` smallint(5) unsigned NOT NULL,
  `level21` smallint(5) unsigned NOT NULL,
  `level22` smallint(5) unsigned NOT NULL,
  `level23` smallint(5) unsigned NOT NULL,
  `level24` smallint(5) unsigned NOT NULL,
  `level25` smallint(5) unsigned NOT NULL,
  `level26` smallint(5) unsigned NOT NULL,
  `level27` smallint(5) unsigned NOT NULL,
  `level28` smallint(5) unsigned NOT NULL,
  `level29` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='公会科技';

DROP TABLE IF EXISTS `gFamilyWallet`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFamilyWallet` (
  `familyID` int(11) unsigned NOT NULL COMMENT '公会ID',
  `familySource` blob NOT NULL COMMENT '公会资源',
  `familyTekID` int(11) NOT NULL,
  UNIQUE KEY `walletID` (`familyID`,`familyTekID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- 保存离线私聊信息
DROP TABLE IF EXISTS `gWhisper`;
CREATE  TABLE `gWhisper` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '接收者ID' ,
  `sendID` INT UNSIGNED NOT NULL COMMENT '发送者ID' ,
  `content` VARCHAR(80) NOT NULL COMMENT '私聊内容' ,
  `sendtime` INT UNSIGNED NOT NULL COMMENT '发送时间' )
ENGINE=InnoDB DEFAULT CHARSET=utf8 
COMMENT = '保存离线私聊信息' ;

-- 喇叭红包信息
DROP TABLE IF EXISTS `gBonus`;
CREATE TABLE `gBonus` (
    `bonusID` INT UNSIGNED NOT NULL COMMENT '红包的唯一ID',
    `info` varbinary(3000) NOT NULL COMMENT '红包的具体内容',
    PRIMARY KEY (`bonusID`)
) ENGINE = InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gTrainer`;
CREATE TABLE `gTrainer` (
    `roleID`    int(11) unsigned NOT NULL   COMMENT '角色ID',
    `talent`    blob    NOT NULL            COMMENT '已经学习的天赋id列表',
    PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gFamilyDonate`;
CREATE  TABLE `gFamilyDonate` (
  `familyID` INT UNSIGNED NOT NULL COMMENT '公会ID',
  `roleID` INT UNSIGNED NOT NULL COMMENT "玩家ID",
  `diamondNum` INT UNSIGNED NOT NULL COMMENT "钻石捐献数量",
  `coinNum` INT UNSIGNED NOT NULL COMMENT "金币捐献数量",
  `reputationNum` INT UNSIGNED NOT NULL COMMENT "徽章捐献数量",
  `gerInfo` varbinary(300) NOT NULL COMMENT "精灵捐献数量",
  `itemInfo` varbinary(300) NOT NULL COMMENT "装备捐献数量",
  `donateContribution` INT UNSIGNED NOT NULL COMMENT "玩家贡献值",
  UNIQUE INDEX `DonateID` (`familyID` ASC, `roleID` ASC)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8; 
 
-- 限时打折活动信息
DROP TABLE IF EXISTS `gDiscountInfo`;
CREATE TABLE `gDiscountInfo` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `DiscountActivityInfo` varbinary(1000) NOT NULL COMMENT '限时打折活动信息',
  PRIMARY KEY (`roleID`)
)ENGINE = InnoDB DEFAULT CHARSET=utf8;
 
-- 保存上次玩家能够参加的所有限时打折活动信息
-- 限时打折活动信息
DROP TABLE IF EXISTS `gDiscountActivityInfo`;
CREATE TABLE `gDiscountActivityInfo` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `DiscountActivityInfo` varbinary(1000) NOT NULL COMMENT '玩家上次能够参加的限时打折活动',
  PRIMARY KEY (`roleID`)
)ENGINE = InnoDB DEFAULT CHARSET=utf8;
 
 DROP TABLE IF EXISTS `t_pay_stastics`;
CREATE  TABLE `t_pay_stastics` (
  `id` int(11) unsigned not null AUTO_INCREMENT comment 'logid',
  `roleID` int(13) unsigned not null comment '角色id',
  `accId` int(13) unsigned not null comment '平台id',
  `payTime` datetime comment '支付时间',
  `payDate` date comment '支付日期',
  `roleGold` int(11) unsigned not null comment '充值前剩余元宝',
  `roleGoldBonus` int(11) unsigned not null comment '充值前剩余赠送元宝',
  `roleGoldNew` int(11) unsigned not null comment '充值后剩余元宝',
  `roleGoldBonusNew` int(11) unsigned not null comment '充值后剩余赠送元宝',
  `roleVip` tinyint(3) unsigned not null comment '充值前vip等级',
  `roleVipNew` tinyint(3) unsigned not null comment '充值后vip等级',
  `roleName` varchar(40) not null comment '玩家名字',
  `srcType` tinyint(3) unsigned not null comment '渠道类型',
  `devID` varchar(40) comment '充值设备号',
  `clienIp` varchar(40) not null comment '充值ip',
  `payGold` smallint(5) unsigned not null comment '充值金额',
  `payItemID` int(11) unsigned not null comment '购买道具ID',
  `receipt` varchar(3000),
  PRIMARY KEY (`id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8  COMMENT = '充值详细信息' ;

-- 保存玩家参加全区抢购活动信息
-- 限时打折活动信息
DROP TABLE IF EXISTS `gPanicBuyActivityInfo`;
CREATE TABLE `gPanicBuyActivityInfo` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `PanicBuyActivityInfo` varbinary(1000) NOT NULL COMMENT '玩家参加全区抢购活动信息',
  PRIMARY KEY (`roleID`)
)ENGINE = InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `gMonthVIP`;
CREATE TABLE `gMonthVIP` (
  `roleID` int(11) UNSIGNED NOT NULL COMMENT '玩家ID',
  `buyBigSec` int(11) unsigned NOT NULL comment '购买大月卡时间',
  `buyLittleSec` int(11) unsigned not null comment '购买小月卡时间',
  `lastGetBigSec` int(11) unsigned not null comment '最后领取大月卡时间',
  `lastGetLittleSec` int(11) unsigned not null comment '最后领取小月卡时间',
  `restBigDays` int(11) unsigned not null comment '剩余大月卡领取时间',
  `restLittleDays` int(11) unsigned not null comment '剩余小月卡领取时间',
  `todayPayBig` tinyint(2) unsigned not null comment '今天是否购充值了大月卡额度',
  `todayPaylittle` tinyint(2) unsigned not null comment '今天是否充值小月卡额度',
  PRIMARY KEY (`roleID`)
)ENGINE = InnoDB DEFAULT CHARSET=utf8 comment '月卡信息';

DROP TABLE IF EXISTS `gStarRewardChapter`;
CREATE TABLE `gStarRewardChapter` (
  `roleID` int(11) unsigned not null comment 'roleID',
  `chapterID` int(11) unsigned not null comment 'chapterID',
  `starRewardStatus` smallint(5) unsigned not null comment '星星领取状态',
  `starCount` smallint(5) unsigned not null comment '星星总数',
  primary key (`roleID`,`chapterID`)
)ENGINE = InnoDB DEFAULT CHARSET=utf8 comment '关卡星星奖励信息';

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

DROP TABLE IF EXISTS `gCarlosWar`;
CREATE TABLE `gCarlosWar` (
    `warID` bigint(20) unsigned not null comment '战场id',
    `war_data` blob comment '战场数据',
    PRIMARY key (`warID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gLuckyRoll`;
CREATE TABLE `gLuckyRoll` (
    `roleID` int(11) unsigned not null comment 'roleID',
    `free_count` smallint(5) unsigned not null comment '总使用免费次数',
    `buy_count` int(11) unsigned not null comment '总使用购买的次数',
    `free_times` smallint(5) unsigned not null comment '当天使用的免费次数',
    `mark` int(11) unsigned not null comment '总积分',
    `isGetRankReward` tinyint(2) unsigned not null comment '是否领取了保底宝箱,1=>已领取', 
    `baseBoxInfo` varbinary(1000) not null comment '保底宝箱信息',
    `outer` varbinary(1000) not null comment 'outercards',
    `inner` varbinary(500) not null comment 'inner',
    `pos` varbinary(500) not null comment 'pos',
    `activityID` int(11) unsigned not null comment '活动ID',
    PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gVipActivity`;
CREATE TABLE `gVipActivity` (
    `roleID` int(11) unsigned not null comment 'roleID',
    `activityID` int(11) unsigned not null comment 'activityID',
    `items` varbinary(8000) not null comment '活动数据',
    `timestamp`  int(11) unsigned NOT NULL DEFAULT 0 COMMENT '刷新时间戳',
    primary key (`roleID`)
) ENGINE =InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gRelicWar`;
CREATE TABLE `gRelicWar` (
    `warID` bigint(20) unsigned not null comment '战场id',
    `war_data` blob comment '战场数据',
    PRIMARY key (`warID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gTvCard`;
CREATE TABLE `gTvCard` (
    `roleID` int(11) unsigned not null comment 'roleID',
    `tvcard` varbinary(1000) not null comment 'data',
    PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gTrSpecial`;
CREATE TABLE `gTrSpecial` (
    `roleID` int(11) unsigned not null comment 'roleID',
    `trID` smallint(6) unsigned not null comment '展示ID',
    `specialID` smallint(6) unsigned not null comment '专精ID',
    PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gGrowthFund`;
CREATE TABLE `gGrowthFund` (
    `roleID` int(11) unsigned NOT NULL comment 'roleID',
    `is_buy` smallint(2) NOT NULL DEFAULT 0 comment '是否购买成长计划',
    `reward_record` blob NOT NULL comment '领取成长计划的记录',
    PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `log_waiting_pids`;
CREATE TABLE `log_waiting_pids` (
    `id` int(11) unsigned not null auto_increment,
    `info` text not null comment 'data',
    `roles` text  not null comment 'roles',
    `time` timestamp default now(),
    PRIMARY key (`id`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gSkin`;
CREATE TABLE `gSkin` (
    `roleID` int(11) unsigned NOT NULL,
    `has` varbinary(1000) NOT NULL COMMENT '当前拥有的皮肤的数据',
    `equip` int(11) unsigned NOT NULL COMMENT '当前装备的皮肤ID',
    PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='皮肤数据';

DROP TABLE IF EXISTS `gHome`;
CREATE TABLE `gHome` (
  `roleID` int(10) unsigned NOT NULL,
  `stage` smallint(5) unsigned NOT NULL,
  `constr_list` varchar(4096) DEFAULT NULL,
  `new_task_timer` varchar(4096) DEFAULT NULL,
  `dounty_ac_num` VARCHAR(128) NOT NULL,
  `cirrus_info` blob NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='新家园数据';

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

DROP TABLE IF EXISTS `gBounty`;
CREATE TABLE `gBounty` (
  `roleID` int(10) unsigned NOT NULL,
  `saveTime` int(10) unsigned not null comment '保存数据的时间戳', 
  type int(10) unsigned not null comment '上次更新时间',
  `restTimes` tinyint(3) unsigned not null comment '剩余次数',
  `alreadyBuyTimes` tinyint(3) unsigned not null comment '已经购买次数',
  `bountyData` varbinary(4096) DEFAULT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='赏金副本';

DROP TABLE IF EXISTS `gDoublematch`;
CREATE TABLE `gDoublematch` (
  `roleID` int(10) unsigned NOT NULL,
  `remain_time` int(10) unsigned not null comment '', 
  `already_buy` int(10) unsigned not null comment '', 
  `rf_date` varchar(32) DEFAULT NULL,
  `rank` int(10) unsigned not null comment '', 
  `score` int(10) unsigned not null comment '', 
  `fight_rec` MEDIUMBLOB not null comment '包含双方的骰子',
  `session` int(10) unsigned not null comment '记录玩家数据属于哪个赛季',
  `remain_time_buy` INT UNSIGNED NOT NULL DEFAULT 0,
  `before_score` INT UNSIGNED NOT NULL DEFAULT 0,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='双排竞技场数据';

DROP TABLE IF EXISTS `gCarlosPlaneInfo`;
CREATE TABLE `gCarlosPlaneInfo` (
	`roleID` int(10) unsigned not null comment 'roleID',
	`planeType`	tinyint(2) unsigned not null default 0 comment '飞机类型,0=原始飞机',
	planeType2 tinyint(2) unsigned not null comment '飞机8的信息',
	`inuse` tinyint(2) unsigned not null default 0 comment '实际使用的飞机',
	`validTime`	int(10) unsigned not null default 0 comment '有效期',
    validTime2 int(10) unsigned not null comment '飞机8的有效时间',
    `planeType3` tinyint(2) unsigned not null comment '飞机3',
    `validTime3` int(10)  not null comment '有效期',
    `planeType4` tinyint(2) unsigned not null comment '飞机4',
    `validTime4` int(10) unsigned not null comment '有效期',
    `planeType11` tinyint(2) unsigned not null default 0,
    `validTime11` int(10) unsigned not null default 0,
    `planeType12` tinyint(2) unsigned not null default 0,
    `validTime12` int(10) unsigned not null default 0,
    `planeType13` tinyint(2) unsigned not null default 0,
    `validTime13` int(10) unsigned not null default 0,
    `planeType14` tinyint(2) unsigned not null default 0,
    `validTime14` int(10) unsigned not null default 0,
    `planeType15` tinyint(2) unsigned not null default 0,
    `validTime15` int(10) unsigned not null default 0,
    `planeType16` tinyint(2) unsigned not null default 0,
    `validTime16` int(10) unsigned not null default 0,
    `planeType17` tinyint(2) unsigned not null default 0,
    `validTime17` int(10) unsigned not null default 0,
    `planeType18` tinyint(2) unsigned not null default 0,
    `validTime18` int(10) unsigned not null default 0,
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

DROP TABLE IF EXISTS `gGerCrystal`;
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

DROP TABLE IF EXISTS `gold2goldbonus_log`;
CREATE TABLE `gold2goldbonus_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `roleid` int(10) unsigned NOT NULL,
  `accountID` int(10) unsigned NOT NULL,
  `oldgold` bigint(20) unsigned NOT NULL,
  `oldgoldbonus` bigint(20) unsigned NOT NULL,
  `oldsrctype` smallint(5) unsigned NOT NULL,
  `newgold` bigint(20) unsigned NOT NULL,
  `newgoldbonus` bigint(20) unsigned NOT NULL,
  `newsrctype` smallint(5) unsigned NOT NULL,
  `transform_time` datetime NOT NULL,
  `type` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleid`),
  index `accountID` (`accountID`),
  index `transform_time` (`transform_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE  TABLE IF NOT EXISTS `gShopBoxCard` (
  `roleID` INT UNSIGNED NOT NULL,
  `cdInfo` VARBINARY(1000) NOT NULL,
  `openedCardInfo` VARBINARY(5000) NOT NULL ,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='商店抽卡信息';

DROP TABLE IF EXISTS `gold2goldbonus_log`;
CREATE TABLE `gold2goldbonus_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `roleid` int(10) unsigned NOT NULL,
  `accountID` int(10) unsigned NOT NULL,
  `oldgold` bigint(20) unsigned NOT NULL,
  `oldgoldbonus` bigint(20) unsigned NOT NULL,
  `oldsrctype` smallint(5) unsigned NOT NULL,
  `newgold` bigint(20) unsigned NOT NULL,
  `newgoldbonus` bigint(20) unsigned NOT NULL,
  `newsrctype` smallint(5) unsigned NOT NULL,
  `transform_time` datetime NOT NULL,
  `type` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `roleID` (`roleid`),
  index `accountID` (`accountID`),
  index `transform_time` (`transform_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE  TABLE IF NOT EXISTS `gPayGuide` (
  `roleID` INT UNSIGNED NOT NULL,
  `unit` VARBINARY(1000) NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='充值引导';

DROP TABLE IF EXISTS `gFamilyCrossMasterRecord`;
CREATE TABLE `gFamilyCrossMasterRecord` (
  `familyID` int(11) unsigned not null comment '联盟id',
  `serverID` int(11) unsigned not null comment '联盟的服务器id',
  `score`  int(11) unsigned not null comment '联盟得分',
  `rank` int(11) unsigned not null comment '世界排名',
  `lastWorldRank` int(11) unsigned not null comment '上次世界排名',
  `totalPower` int(11) unsigned not null comment '上次报名时记录的战斗力',
  PRIMARY KEY (`familyID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gactivityfestival`;
CREATE TABLE `gactivityfestival` (
  `roleID` int(11) unsigned not null comment 'roleID',
  `data` varbinary(1000) not null ,
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- v3.3.0
--
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
  `cdduration` smallint UNSIGNED default 600 NOT NULL COMMENT 'cdduration',
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

DROP TABLE IF EXISTS `gXBattle`;
CREATE  TABLE IF NOT EXISTS `gXBattle` (
  `roleID` INT UNSIGNED NOT NULL,
  `nowChapterID` INT UNSIGNED NOT NULL COMMENT '当前关卡ID',
  `quickFightSec` INT UNSIGNED NOT NULL COMMENT '快速战斗开始时间',
  `buyQuickCount` INT UNSIGNED NOT NULL COMMENT '购买的快速战斗次数',
  `buyQuickToday` SMALLINT UNSIGNED NOT NULL COMMENT '今天购买的快速战斗次数',
  `lastTriggerSec` INT UNSIGNED NOT NULL COMMENT '上次触发事件时间点',
  `challengeCount` INT UNSIGNED NOT NULL COMMENT '挑战了多少次',
  `raidCount` INT UNSIGNED NOT NULL COMMENT '快速探索了多少次',
  `passData` varbinary(500) not null comment '关卡通过序列',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='战役';

DROP TABLE IF EXISTS `gXBattleChapter`;
CREATE  TABLE IF NOT EXISTS `gXBattleChapter` (
  `roleID` INT UNSIGNED NOT NULL,
  `chapterID` INT UNSIGNED NOT NULL COMMENT '当前关卡ID',
  `passDungeons` VARBINARY(200) NOT NULL COMMENT '当前关卡通过信息',
  `isGetReward` tinyINT UNSIGNED NOT NULL COMMENT '是否领取通关奖励',
  `challengeCount` INT UNSIGNED NOT NULL COMMENT '关卡挑战次数' ,
  PRIMARY KEY (`roleID`,`chapterID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='战役chapter';

DROP TABLE IF EXISTS `gXBattleGerDisplay`;
CREATE  TABLE IF NOT EXISTS `gXBattleGerDisplay` (
  `roleID` INT UNSIGNED NOT NULL,
  `displayData` VARBINARY(1000) NOT NULL COMMENT '解锁精灵信息',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='战役图鉴';

DROP TABLE IF EXISTS `gHeadSeven`;
CREATE  TABLE IF NOT EXISTS `gHeadSeven` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `doingTask` blob NOT NULL COMMENT '正在进行的任务',
  `finishTask` blob NOT NULL COMMENT '已经完成的任务未领取奖励',
  `finishRewardTask` VARBINARY(100) NOT NULL COMMENT '已经完成奖励领取',
  `period` SMALLINT NOT NULL COMMENT '玩家当前所属的阶段',
  `beginTimeStamp` INT UNSIGNED NOT NULL COMMENT '任务开始时间',
  `isFirst` TINYINT NOT NULL DEFAULT 1 COMMENT '玩家是否首次登陆',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='七日计划';

 DROP TABLE IF EXISTS `gManual`;
 CREATE  TABLE IF NOT EXISTS `gManual` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '玩家ID',
  `manual` blob NOT NULL COMMENT '玩家新图鉴信息',
  PRIMARY KEY (`roleID`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='新图鉴';
 
 DROP TABLE IF EXISTS `gCanvassinfo`;
 CREATE TABLE IF NOT EXISTS `gCanvassinfo` (
  `roleID` INT UNSIGNED NOT NULL,
  `canvass_id` INT UNSIGNED NOT NULL,
  `select_list` VARCHAR(256) NOT NULL,
  PRIMARY KEY (`roleID`));

DROP TABLE IF EXISTS `gDojang`;
CREATE  TABLE IF NOT EXISTS `gDojang` (
  `roleID` INT(11) UNSIGNED NOT NULL COMMENT '玩家ID',
  `dojang_data` blob comment '道馆数据',
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT = '道馆';

DROP TABLE IF EXISTS `gDojangRank`;
CREATE  TABLE IF NOT EXISTS `gDojangRank` (
  `roleID` INT(11) UNSIGNED NOT NULL COMMENT '玩家ID',
  `day` date NOT NULL COMMENT '上次修改数据的时间',
  `free_time` INT(11) UNSIGNED NOT NULL COMMENT '剩余免费挑战次数',
  `paid_time` INT(11) UNSIGNED NOT NULL COMMENT '剩余收费挑战次数',
  `buy_time` INT(11) UNSIGNED NOT NULL COMMENT '当日购买次数',
  `fight_rec_list` blob comment '战斗记录id',
  `world_free_time` blob NOT NULL COMMENT '跨服剩余免费挑战次数',
  `world_paid_time` blob NOT NULL COMMENT '跨服剩余收费挑战次数',
  `world_buy_time` blob NOT NULL COMMENT '跨服当日购买次数',
  `world_enemy_list` blob NOT NULL COMMENT '敌人对手数据',
  `world_refresh_time` blob NULL COMMENT '刷新次数',
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
