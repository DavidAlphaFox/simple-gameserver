ALTER TABLE `gFamily` ADD COLUMN `familycrossInfo` BLOB NOT NULL comment '阿努比斯之门'  AFTER `fighter_group` ;
ALTER TABLE `ghron` ADD COLUMN `history` varbinary(100) NOT NULL comment '无尽深渊历史数据' AFTER `isSelect`;
ALTER TABLE `gFamilyMember` ADD COLUMN `anubisinfo` varbinary(500) NOT NULL comment '公会成员阿努比斯之门记录' AFTER `limitShop`;
ALTER TABLE `gFamily` ADD COLUMN `familyanubisinfo` BLOB NOT NULL COMMENT '公会阿努比斯赛季记录' AFTER `familycrossInfo`;

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

-- 删除旧的pvp记录
delete from gHist where type= 4;   

-- 增加双排战斗记录长度
ALTER TABLE `gdoublefightrec` CHANGE COLUMN `fight_rec` `fight_rec` MEDIUMBLOB NOT NULL COMMENT '包含双方的骰子'  ;
