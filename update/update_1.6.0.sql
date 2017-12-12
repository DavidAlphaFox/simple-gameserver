ALTER TABLE `gVipActivity` ADD COLUMN `timestamp` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '刷新时间戳' AFTER `items`;  
ALTER TABLE `gEquip` ADD COLUMN `itemenchantType` smallint(6) NOT NULL DEFAULT 0 COMMENT '装备附魔类型' AFTER `itemExp`; 
ALTER TABLE `gEquip` ADD COLUMN `itemenchantLevel` smallint(6) NOT NULL DEFAULT 0 COMMENT '装备附魔等级' AFTER `itemenchantType`; 

DROP TABLE IF EXISTS `gRelicWar`;
CREATE TABLE `gRelicWar` (
    `warID` bigint(20) unsigned not null comment '战场id',
    `war_data` blob comment '战场数据',
    PRIMARY key (`warID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

ALTER TABLE `gFighterList` modify column `fighterList` varbinary(4000);

ALTER TABLE `t_gold_consume_2015_9` add column vlog varchar(4000);
