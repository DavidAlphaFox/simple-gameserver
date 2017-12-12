DROP TABLE IF EXISTS `gactivityfestival`;
CREATE TABLE `gactivityfestival` (
  `roleID` int(11) unsigned not null comment 'roleID',
  `data` varbinary(1000) not null ,
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1;

ALTER TABLE `gCarlosPlaneInfo` ADD COLUMN `planeType3` tinyint(2) unsigned not null comment '飞机3' after `validTime2`;
ALTER TABLE `gCarlosPlaneInfo` ADD COLUMN `validTime3` int(10)  not null comment '有效期' after `planeType3`;
ALTER TABLE `gCarlosPlaneInfo` ADD COLUMN `planeType4` tinyint(2) unsigned not null comment '飞机4' after `validTime3`;
ALTER TABLE `gCarlosPlaneInfo` ADD COLUMN `validTime4` int(10) unsigned not null comment '有效期' after `planeType4`;





