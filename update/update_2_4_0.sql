alter table gMail add COLUMN `head` int(13) NOT NULL DEFAULT '0'  COMMENT '邮件发件人头像' after `isRead`,add COLUMN `isMale` tinyint(1) NOT NULL DEFAULT '0'  COMMENT '邮件发件人性别' after `head`;
ALTER TABLE gHome ADD COLUMN `dounty_ac_num` VARCHAR(128) NOT NULL  AFTER `new_task_timer` ;

DROP TABLE IF EXISTS `gBounty`;
CREATE TABLE `gBounty` (
  `roleID` int(10) unsigned NOT NULL,
  `saveTime` int(10) unsigned not null comment '保存数据的时间戳', 
  `type` tinyint(3) unsigned not null comment '当前类型',
  `restTimes` tinyint(3) unsigned not null comment '剩余次数',
  `alreadyBuyTimes` tinyint(3) unsigned not null comment '已经购买次数',
  `bountyData` varbinary(4096) DEFAULT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='赏金副本';


alter table gRole modify column coin bigint(20) unsigned not null comment '金币';