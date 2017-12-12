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

ALTER TABLE `t_coin_add_2016_7` CHANGE COLUMN `coin` `coin` BIGINT UNSIGNED NOT NULL COMMENT '获得的银两'  , CHANGE COLUMN `curCoin` `curCoin` BIGINT NOT NULL COMMENT '获得前的银两数量';
ALTER TABLE `t_coin_consume_2016_7` CHANGE COLUMN `coin` `coin` BIGINT UNSIGNED NOT NULL COMMENT '获得的银两'  , CHANGE COLUMN `curCoin` `curCoin` BIGINT NOT NULL COMMENT '获得前的银两数量';
ALTER TABLE `gdoublematch` ADD COLUMN `before_score` INT UNSIGNED NOT NULL DEFAULT 0  AFTER `remain_time_buy` ;

alter table `gTeamPk` modify column teamPkData varbinary(60000) not null;
alter table `gFighterList` modify column fighterList varbinary(9000) not null;
alter table `gequip` modify column itemExp int(11) not null;


