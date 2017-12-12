ALTER TABLE `logBuyTimes_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT '购买时候的VIP等级' ;

ALTER TABLE `t_coin_add_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_coin_consume_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_unioncoin_add_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_unioncoin_consume_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ger_add_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '武将等级';

ALTER TABLE `t_ger_consume_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '武将等级';

ALTER TABLE `t_ger_uplevel_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '升级前武将等级';

ALTER TABLE `t_ger_uplevel_2017_8` CHANGE COLUMN `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '新的武将等级';

ALTER TABLE `t_ger_uprank_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '升品前武将等级';

ALTER TABLE `t_ger_uprank_2017_8` CHANGE COLUMN `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '新的武将等级';

ALTER TABLE `t_ger_downrank_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '降品品前武将等级';

ALTER TABLE `t_item_downrank_2017_8` CHANGE COLUMN `ItemLevel` `ItemLevel` INT UNSIGNED NOT NULL COMMENT '降品品前武将等级';

ALTER TABLE `t_gold_bonus_add_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_gold_consume_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_item_uplevel_2017_8` CHANGE `addLevel` `addLevel` INT UNSIGNED NOT NULL COMMENT '提升的装备等级';

ALTER TABLE `t_item_uplevel_2017_8` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '升级后的装备等级';

ALTER TABLE `t_item_uprank_2017_8` CHANGE `curLevel` `curLevel` INT UNSIGNED NOT NULL COMMENT '升品前的装备等级';

ALTER TABLE `t_item_uprank_2017_8` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '升品后的装备等级';

ALTER TABLE `t_repu_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_repu_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_score_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_profoundCrystal_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_profoundCrystal_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_honor_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_honor_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_home_resource_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_home_resource_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_pvppoint_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_pvppoint_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ticket_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ticket_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_item_enchant_2017_8` CHANGE `oldItemEnchantLevel` `oldItemEnchantLevel` INT UNSIGNED NOT NULL COMMENT '道具旧的附魔等级';

ALTER TABLE `t_item_enchant_2017_8` CHANGE `newItemEnchantLevel` `newItemEnchantLevel` INT UNSIGNED NOT NULL COMMENT '道具新的附魔等级';

ALTER TABLE `t_ger_crystal_2017_8` CHANGE `oldLevel` `oldLevel` INT UNSIGNED NOT NULL COMMENT '精灵晶体旧的等级';

ALTER TABLE `t_ger_crystal_2017_8` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '精灵晶体新的等级';


-- 8月

ALTER TABLE `logBuyTimes_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT '购买时候的VIP等级' ;

ALTER TABLE `t_coin_add_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_coin_consume_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_unioncoin_add_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_unioncoin_consume_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ger_add_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '武将等级';

ALTER TABLE `t_ger_consume_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '武将等级';

ALTER TABLE `t_ger_uplevel_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '升级前武将等级';

ALTER TABLE `t_ger_uplevel_2017_8` CHANGE COLUMN `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '新的武将等级';

ALTER TABLE `t_ger_uprank_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '升品前武将等级';

ALTER TABLE `t_ger_uprank_2017_8` CHANGE COLUMN `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '新的武将等级';

ALTER TABLE `t_ger_downrank_2017_8` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '降品品前武将等级';

ALTER TABLE `t_item_downrank_2017_8` CHANGE COLUMN `ItemLevel` `ItemLevel` INT UNSIGNED NOT NULL COMMENT '降品品前武将等级';

ALTER TABLE `t_gold_bonus_add_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_gold_consume_2017_8` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_item_uplevel_2017_8` CHANGE `addLevel` `addLevel` INT UNSIGNED NOT NULL COMMENT '提升的装备等级';

ALTER TABLE `t_item_uplevel_2017_8` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '升级后的装备等级';

ALTER TABLE `t_item_uprank_2017_8` CHANGE `curLevel` `curLevel` INT UNSIGNED NOT NULL COMMENT '升品前的装备等级';

ALTER TABLE `t_item_uprank_2017_8` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '升品后的装备等级';

ALTER TABLE `t_repu_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_repu_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_score_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_profoundCrystal_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_profoundCrystal_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_honor_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_honor_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_home_resource_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_home_resource_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_pvppoint_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_pvppoint_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ticket_add_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ticket_consume_2017_8` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_item_enchant_2017_8` CHANGE `oldItemEnchantLevel` `oldItemEnchantLevel` INT UNSIGNED NOT NULL COMMENT '道具旧的附魔等级';

ALTER TABLE `t_item_enchant_2017_8` CHANGE `newItemEnchantLevel` `newItemEnchantLevel` INT UNSIGNED NOT NULL COMMENT '道具新的附魔等级';

ALTER TABLE `t_ger_crystal_2017_8` CHANGE `oldLevel` `oldLevel` INT UNSIGNED NOT NULL COMMENT '精灵晶体旧的等级';

ALTER TABLE `t_ger_crystal_2017_8` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '精灵晶体新的等级';


--- 9 月

ALTER TABLE `logBuyTimes_2017_9` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT '购买时候的VIP等级' ;

ALTER TABLE `t_coin_add_2017_9` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_coin_consume_2017_9` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_unioncoin_add_2017_9` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_unioncoin_consume_2017_9` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ger_add_2017_9` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '武将等级';

ALTER TABLE `t_ger_consume_2017_9` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '武将等级';

ALTER TABLE `t_ger_uplevel_2017_9` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '升级前武将等级';

ALTER TABLE `t_ger_uplevel_2017_9` CHANGE COLUMN `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '新的武将等级';

ALTER TABLE `t_ger_uprank_2017_9` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '升品前武将等级';

ALTER TABLE `t_ger_uprank_2017_9` CHANGE COLUMN `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '新的武将等级';

ALTER TABLE `t_ger_downrank_2017_9` CHANGE COLUMN `gerLevel` `gerLevel` INT UNSIGNED NOT NULL COMMENT '降品品前武将等级';

ALTER TABLE `t_item_downrank_2017_9` CHANGE COLUMN `ItemLevel` `ItemLevel` INT UNSIGNED NOT NULL COMMENT '降品品前武将等级';

ALTER TABLE `t_gold_bonus_add_2017_9` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_gold_consume_2017_9` CHANGE COLUMN `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_item_uplevel_2017_9` CHANGE `addLevel` `addLevel` INT UNSIGNED NOT NULL COMMENT '提升的装备等级';

ALTER TABLE `t_item_uplevel_2017_9` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '升级后的装备等级';

ALTER TABLE `t_item_uprank_2017_9` CHANGE `curLevel` `curLevel` INT UNSIGNED NOT NULL COMMENT '升品前的装备等级';

ALTER TABLE `t_item_uprank_2017_9` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '升品后的装备等级';

ALTER TABLE `t_repu_add_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_repu_consume_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_score_consume_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_profoundCrystal_add_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_profoundCrystal_consume_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_honor_add_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_honor_consume_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_home_resource_add_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_home_resource_consume_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_pvppoint_add_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_pvppoint_consume_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ticket_add_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_ticket_consume_2017_9` CHANGE `vipLevel` `vipLevel` INT UNSIGNED NOT NULL COMMENT 'VIP等级';

ALTER TABLE `t_item_enchant_2017_9` CHANGE `oldItemEnchantLevel` `oldItemEnchantLevel` INT UNSIGNED NOT NULL COMMENT '道具旧的附魔等级';

ALTER TABLE `t_item_enchant_2017_9` CHANGE `newItemEnchantLevel` `newItemEnchantLevel` INT UNSIGNED NOT NULL COMMENT '道具新的附魔等级';

ALTER TABLE `t_ger_crystal_2017_9` CHANGE `oldLevel` `oldLevel` INT UNSIGNED NOT NULL COMMENT '精灵晶体旧的等级';

ALTER TABLE `t_ger_crystal_2017_9` CHANGE `newLevel` `newLevel` INT UNSIGNED NOT NULL COMMENT '精灵晶体新的等级';






