-- 保存离线私聊信息
DROP TABLE IF EXISTS `gWhisper`;
CREATE  TABLE `gWhisper` (
  `roleID` INT UNSIGNED NOT NULL COMMENT '接收者ID' ,
  `sendID` INT UNSIGNED NOT NULL COMMENT '发送者ID' ,
  `content` VARCHAR(80) NOT NULL COMMENT '私聊内容' ,
  `sendtime` INT UNSIGNED NOT NULL COMMENT '发送时间' )
ENGINE=InnoDB DEFAULT CHARSET=utf8 
COMMENT = '保存离线私聊信息' ;

-- 增加字段，记录公会币
-- 初次增加公会币字段后，
ALTER TABLE gRole ADD COLUMN unioncoin INT NOT NULL DEFAULT '-1'  COMMENT '公会货币' AFTER `goldUsed`;

--
-- Table structure for table `gFamilytek`
--

DROP TABLE IF EXISTS `gFamilyTek`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFamilyTek` (
  `familyID` int(11) unsigned NOT NULL COMMENT "公会ID",
  `level1` smallint(5) unsigned NOT NULL,
  `level2` smallint(5) unsigned NOT NULL,
  `level3` smallint(5) unsigned NOT NULL,
  `level4` smallint(5) unsigned NOT NULL,
  `level5` smallint(5) unsigned NOT NULL,
  `level6` smallint(5) unsigned NOT NULL,
  `level7` smallint(5) unsigned NOT NULL,
  `level8` smallint(5) unsigned NOT NULL,
  `level9` smallint(5) unsigned NOT NULL,
  `level10` smallint(5) unsigned NOT NULL,
  `level11` smallint(5) unsigned NOT NULL,
  `level12` smallint(5) unsigned NOT NULL,
  `level13` smallint(5) unsigned NOT NULL,
  `level14` smallint(5) unsigned NOT NULL,
  `level15` smallint(5) unsigned NOT NULL,
  `level16` smallint(5) unsigned NOT NULL,
  `level17` smallint(5) unsigned NOT NULL,
  `level18` smallint(5) unsigned NOT NULL,
  `level19` smallint(5) unsigned NOT NULL,
  `level20` smallint(5) unsigned NOT NULL,
  `level21` smallint(5) unsigned NOT NULL,
  `level22` smallint(5) unsigned NOT NULL,
  `level23` smallint(5) unsigned NOT NULL,
  `level24` smallint(5) unsigned NOT NULL,
  `level25` smallint(5) unsigned NOT NULL,
  `level26` smallint(5) unsigned NOT NULL,
  `level27` smallint(5) unsigned NOT NULL,
  `level28` smallint(5) unsigned NOT NULL,
  `level29` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`familyID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='公会科技';

DROP TABLE IF EXISTS `gFamilyWallet`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gFamilyWallet` (
  `familyID` int(11) unsigned NOT NULL COMMENT '公会ID',
  `familySource` blob NOT NULL COMMENT '公会资源',
  `familyTekID` int(11) NOT NULL,
  UNIQUE KEY `walletID` (`familyID`,`familyTekID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

