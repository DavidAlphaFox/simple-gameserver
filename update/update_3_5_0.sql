
DROP TABLE IF EXISTS `gDojang`;
CREATE  TABLE IF NOT EXISTS `gDojang` (
  `roleID` INT(11) UNSIGNED NOT NULL COMMENT '玩家ID',
  `dojang_data` blob comment '道馆数据',
  PRIMARY KEY (`roleID`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT = '道馆';
