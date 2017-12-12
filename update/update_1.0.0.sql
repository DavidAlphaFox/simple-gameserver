DROP TABLE IF EXISTS `logGuide`;
CREATE TABLE `logGuide` (
  `roleID` int(11) unsigned NOT NULL,
  `guideState` smallint(6) unsigned NOT NULL COMMENT '新手引导的状态',
  PRIMARY KEY (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

ALTER TABLE `gRole` ADD `srcType` smallint unsigned NOT NULL DEFAULT '0' COMMENT '渠道ID' AFTER `devid`;

ALTER TABLE `t_gold_pay_add` ADD `accid` bigint(11) unsigned NOT NULL DEFAULT '0' COMMENT '帐号ID'  AFTER `receiptMd5`;
ALTER TABLE `t_gold_pay_add` ADD `devid` varchar(100) NOT NULL DEFAULT '' COMMENT '设备号'  AFTER `accid`;
ALTER TABLE `t_gold_pay_add` ADD `srcType` smallint unsigned NOT NULL DEFAULT '0' COMMENT '渠道ID' AFTER `devid`;

DROP TABLE IF EXISTS `gOfflinePayAmountLog`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gOfflinePayAmountLog` (
  `roleID` int(11) unsigned NOT NULL,
  `amount` int(11) unsigned NOT NULL COMMENT '充值额度',
  `receipt` varchar(3000) NOT NULL,
  `receiptMd5` varchar(40) NOT NULL,
  `SrcType` tinyint(4) NOT NULL,
  UNIQUE KEY `receiptMd5` (`receiptMd5`),
  KEY `roleID` (`roleID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;