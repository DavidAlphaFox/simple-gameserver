DROP TABLE IF EXISTS `gTeamPk`;
CREATE TABLE `gTeamPk` (
  `roleID` int(11) unsigned NOT NULL,
  `teamPkData` varbinary(20000) NOT NULL COMMENT '3v3相关数据',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `gAlien`;
CREATE TABLE `gAlien` (
  `roleID` int(11) unsigned NOT NULL,
  `alienTimes` int unsigned NOT NULL COMMENT '异星战场相关数据',
  `lastRecoverTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
  `resetTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;