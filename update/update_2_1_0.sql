--2.1.0版本先不做--delete from gStarRewardChapter where chapterID > 2000 and chapterID < 3000 ;
--2.1.0版本先不做--update gChapter set starRewarded = 0 where chapterID > 2000 and chapterID < 3000;
alter table gHron add column isSelect tinyint(2) unsigned not null comment '是否选择了星级' after  `lastFightResult`;
alter table gFamilyMember add column limitShop varbinary(1000) not null comment '工会限制商店' after `offlineTime`;

DROP TABLE IF EXISTS `gTvCard`;
CREATE TABLE `gTvCard` (
    `roleID` int(11) unsigned not null comment 'roleID',
    `tvcard` varbinary(1000) not null comment 'data',
    PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

alter table gRole add `pvppoint` int(13) NOT NULL DEFAULT '0'  COMMENT '竞技场点数' after `honor`;
alter table gGer add column gerAwakeInfo varbinary(1000) not null comment '精灵觉醒信息' after `gerPos`;
alter table gFighterList add column trSpecial varbinary(100) not null comment '训练师专精信息' after `lieuHpAdd`;

DROP TABLE IF EXISTS `gtrSpecial`;
CREATE TABLE `gTrSpecial` (
    `roleID` int(11) unsigned not null comment 'roleID',
    `trID` smallint(6) unsigned not null comment '展示ID',
    `specialID` smallint(6) unsigned not null comment '专精ID',
    PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

delete from gShopNum where shopID = 10001;


--alter table gcarloswar partition by range (warID) (partition p_1 values less than 1000000,partition p_2 values less than 2000000);

-- 删除旧的日志
--truncate table gReplay;
-- 修改旧的日指标为备份表,同时创建新表并插入旧表的最大数据值
alter table gReplay rename gReplay_bak_210;
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
insert into gReplay (replayUID,replay,time,type,isCompress) select replayUID,replay,time,type,isCompress from gReplay_bak_210 where replayUID = (select max(replayUID) from gReplay_bak_210);


alter table gFamilyFightRecord rename gFamilyFightRecord_bak_210;
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
insert into gFamilyFightRecord (recordUID,familyID,tarFamilyID,roleID,TarRoleID,warPeriod,isChallenger,isWin,winStar,replay,ftime,winfamily,roleName,tarRoleName)
	select recordUID,familyID,tarFamilyID,roleID,TarRoleID,warPeriod,isChallenger,isWin,winStar,replay,ftime,winfamily,roleName,tarRoleName from gFamilyFightRecord_bak_210 where
	recordUID = (select max(recordUID) from gFamilyFightRecord_bak_210);

alter table gHist rename ghist_bak_210;
CREATE TABLE `gHist` (
  `histUID` bigint(20) unsigned NOT NULL,
  `histType` tinyint(4) unsigned NOT NULL COMMENT '',
  `name` varchar(15) NOT NULL COMMENT '',
  `enemyID` int(11) NOT NULL COMMENT '',
  `time` int(11) NOT NULL COMMENT '',
  `arg` mediumint(9) unsigned NOT NULL COMMENT '',
  `isRead` tinyint(1) unsigned NOT NULL COMMENT '',
  `fightInfo` varbinary(2000) NOT NULL COMMENT '',
  `roleID` int(11) unsigned NOT NULL,
  `type` tinyint(1) unsigned NOT NULL COMMENT '战报大类型：4=争霸，5=夺宝',
  PRIMARY KEY (`histUID`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='';

insert into gHist (histUID,histType,name,enemyID,time,arg,isRead,fightInfo,roleID,type)
	select histUID,histType,name,enemyID,time,arg,isRead,fightInfo,roleID,type from ghist_bak_210 where
	histUID = (select max(histUID) from ghist_bak_210);
