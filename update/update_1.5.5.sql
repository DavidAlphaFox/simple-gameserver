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
--	`lastExploreDate` date default NULL comment '上次探索的日期',
	`activityID` int(11) unsigned not null comment '活动ID',
	PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;
ALTER TABLE gRole ADD COLUMN carloswintime INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯胜利次数' AFTER `honor`;
ALTER TABLE gRole ADD COLUMN carlosequaltime INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯平局次数' AFTER `carloswintime`;
ALTER TABLE gRole ADD COLUMN carloslosetime INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯失败次数' AFTER `carlosequaltime`;
ALTER TABLE gRole ADD COLUMN carlosseason INT NOT NULL DEFAULT '0'  COMMENT '卡洛斯赛季编号' AFTER `carloslosetime`;
ALTER TABLE gRole ADD COLUMN carlosprewintime INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯胜利次数' AFTER `carlosseason`;
ALTER TABLE gRole ADD COLUMN carlospreequaltime INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯平局次数' AFTER `carlosprewintime`;
ALTER TABLE gRole ADD COLUMN carlosprelosetime INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯失败次数' AFTER `carlospreequaltime`;
ALTER TABLE gRole ADD COLUMN carlospreseason INT NOT NULL DEFAULT '0'  COMMENT '上届卡洛斯赛季编号' AFTER `carlosprelosetime`;

DROP TABLE IF EXISTS `gVipActivity`;
CREATE TABLE `gVipActivity` (
	`roleID` int(11) unsigned not null comment 'roleID',
	`activityID` int(11) unsigned not null comment 'activityID',
	`items` varbinary(8000) not null comment '活动数据',
	primary key (`roleID`)
) ENGINE =InnoDB DEFAULT CHARSET=latin1;

ALTER TABLE gRoleExtra ADD COLUMN `magicBookState` varchar(200) NOT NULL DEFAULT '' COMMENT '魔典状态' AFTER `lastPayTime`;
ALTER TABLE gFamily ADD COLUMN `boss_info` blob NOT NULL COMMENT '公会boss信息' AFTER `talkRoomID`;
