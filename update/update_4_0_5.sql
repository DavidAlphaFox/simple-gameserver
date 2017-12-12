alter table `t_gold_pay_add` ADD COLUMN `nowChapterID` int(5)  NOT NULL COMMENT '充值时候的chapterID' after `srcType`;
alter table `t_gold_pay_add` ADD COLUMN `roleLevel` smallint(3) NOT NULL COMMENT '充值时候的等级' after `nowChapterID`;

alter table gXbattleChapter ADD COLUMN `challengeCount` INT UNSIGNED NOT NULL COMMENT '关卡挑战次数' after `isGetReward`;

delete from gXbattleChapter;
--delete from gGuide;
update gGuide set guideState = 104;

 DROP TABLE IF EXISTS `gCanvassinfo`;
 CREATE TABLE IF NOT EXISTS `gCanvassinfo` (
  `roleID` INT UNSIGNED NOT NULL,
  `canvass_id` INT UNSIGNED NOT NULL,
  `select_list` VARCHAR(256) NOT NULL,
  PRIMARY KEY (`roleID`));
