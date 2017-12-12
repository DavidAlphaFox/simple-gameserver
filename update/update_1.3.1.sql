
DROP TABLE IF EXISTS `t_pay_stastics`;
CREATE  TABLE `t_pay_stastics` (
  `id` int(11) unsigned not null AUTO_INCREMENT comment 'logid',
  `roleID` int(13) unsigned not null comment '角色id',
  `accId` int(13) unsigned not null comment '平台id',
  `payTime` datetime comment '支付时间',
  `payDate` date comment '支付日期',
  `roleGold` int(11) unsigned not null comment '充值前剩余元宝',
  `roleGoldBonus` int(11) unsigned not null comment '充值前剩余赠送元宝',
  `roleGoldNew` int(11) unsigned not null comment '充值后剩余元宝',
  `roleGoldBonusNew` int(11) unsigned not null comment '充值后剩余赠送元宝',
  `roleVip` tinyint(3) unsigned not null comment '充值前vip等级',
  `roleVipNew` tinyint(3) unsigned not null comment '充值后vip等级',
  `roleName` varchar(40) not null comment '玩家名字',
  `srcType` tinyint(3) unsigned not null comment '渠道类型',
  `devID` varchar(40) comment '充值设备号',
  `clienIp` varchar(40) not null comment '充值ip',
  `payGold` smallint(5) unsigned not null comment '充值金额',
  `payItemID` int(11) unsigned not null comment '购买道具ID',
  `receipt` varchar(3000),
  PRIMARY KEY (`id`)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8  COMMENT = '充值详细信息' ;
