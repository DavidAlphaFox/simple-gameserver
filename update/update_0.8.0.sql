ALTER TABLE `gFriendEnargy` ADD `refreshDate` date NOT NULL DEFAULT '1970-01-01' COMMENT '上次次数刷新日期' AFTER `addFriendList`;
ALTER TABLE `gFriendEnargy` ADD `giveTimes` int(11) unsigned NOT NULL DEFAULT 30 COMMENT '剩余领取体力次数' AFTER `refreshDate`;
ALTER TABLE `gRole` ADD `isFailed` tinyint(1) NOT NULL DEFAULT '0' COMMENT '是否关卡战败过' AFTER `location`;
ALTER TABLE `gRole` ADD `devid` varchar(100) NOT NULL DEFAULT '' COMMENT 'push token' AFTER `isFailed`;
