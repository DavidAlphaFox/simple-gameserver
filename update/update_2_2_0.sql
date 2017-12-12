ALTER TABLE `gRoleExtra` ADD COLUMN `teamPkTimes` smallint(6) unsigned NOT NULL COMMENT '3v3次数' AFTER `magicBookState`;
ALTER TABLE `gRoleExtra` ADD COLUMN `teampPkBuyTimes` smallint(5) unsigned NOT NULL COMMENT '3v3已购买次数' AFTER `teamPkTimes`;
ALTER TABLE `gRoleExtra` ADD COLUMN `lastTeamPkTime` int(11) unsigned NOT NULL COMMENT '3v3上次恢复时间' AFTER `teampPkBuyTimes`;

ALTER TABLE `gRoleExtra` ADD COLUMN `sign_day_count` smallint(6) NOT NULL DEFAULT 0 COMMENT '玩家签到天数' AFTER `lastTeamPkTime`; 
ALTER TABLE `gRoleExtra` ADD COLUMN `is_get_sign_reward` smallint(6) NOT NULL DEFAULT 0 COMMENT '玩家是否领取签到奖励' AFTER `sign_day_count`; 
ALTER TABLE `gRoleExtra` ADD COLUMN `last_sign_time` int(11) NOT NULL DEFAULT 0 COMMENT '玩家上次签到时间' AFTER `is_get_sign_reward`;
ALTER TABLE `gRoleExtra` ADD COLUMN `is_get_acc_sign_reward` int(11) NOT NULL DEFAULT 0 COMMENT '玩家已经领取累积签到奖励天数' AFTER `last_sign_time`;

DROP TABLE IF EXISTS `gGrowthFund`;
CREATE TABLE `gGrowthFund` (
    `roleID` int(11) unsigned NOT NULL comment 'roleID',
    `is_buy` smallint(2) NOT NULL DEFAULT 0 comment '是否购买成长计划',
    `reward_record` blob NOT NULL comment '领取成长计划的记录',
    PRIMARY key (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

ALTER TABLE `gFamily` ADD COLUMN `fighter_group` BLOB NOT NULL  AFTER `boss_info` ;

DROP TABLE IF EXISTS `log_waiting_pids`;
CREATE TABLE `log_waiting_pids` (
    `id` int(11) unsigned not null auto_increment,
    `info` text not null comment 'data',
    `roles` text  not null comment 'roles',
    `time` timestamp default now(),
    PRIMARY key (`id`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;
