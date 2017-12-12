ALTER TABLE `gLimit` ADD COLUMN `ticketBoxCount` int(8) unsigned  NOT NULL comment '点券次数记录'  AFTER `combine2Value` ;
ALTER TABLE `gLimit` ADD COLUMN `ticketBoxCount2` int(8) unsigned  NOT NULL comment '十连抽点券次数记录'  AFTER `ticketBoxCount` ;

ALTER TABLE `gRole` ADD COLUMN `ticket` int(10) unsigned not null comment '点券' after `firtPayStatus`;
ALTER TABLE `gRole` ADD COLUMN `laputastone` int(11) unsigned not null comment '飞行币' after `ticket`;  

ALTER TABLE gFighterList modify column fighterList varbinary(9999) not null;