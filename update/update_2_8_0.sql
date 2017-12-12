
ALTER TABLE `gRoleExtra` ADD COLUMN `battleBossReward` varbinary(2000) NOT NULL DEFAULT -1 AFTER `is_get_acc_sign_reward`;
ALTER TABLE `gRoleExtra` ADD COLUMN `maingerTypeid` smallint(5) NOT NULL DEFAULT 0 AFTER `battleBossReward`;

ALTER TABLE `gLimit` ADD COLUMN `spiritRefresh` tinyint(2) NOT NULL DEFAULT 0 AFTER `trainerItemBoxCount`;
ALTER TABLE `gLimit` ADD COLUMN `equipRefresh` tinyint(2) NOT NULL DEFAULT 0 AFTER `spiritRefresh`;
ALTER TABLE `gLimit` ADD COLUMN `trainerRefresh` tinyint(2) NOT NULL DEFAULT 0 AFTER `equipRefresh`;
ALTER TABLE `gLimit` ADD COLUMN `combine2Value` tinyint(2) NOT NULL DEFAULT 0 AFTER `trainerRefresh`;


CREATE  TABLE IF NOT EXISTS `gPayGuide` (
  `roleID` INT UNSIGNED NOT NULL,
  `unit` VARBINARY(1000) NOT NULL,
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='充值引导';

update gGuide set guideState = 31;

delete from gTask where taskID < 30000;
	