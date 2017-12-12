%% 服务器可识别的奖励格式
% [{?REWARD_GOLD, 100},
%  {?REWARD_COIN,100},
%  {?REWARD_REPU, 100},
%  {?REWARD_ITEM, 10010, Num},
%  {?REWARD_GER, 11010, Num},
%  #new_item{},
%  #new_ger{},
%  [#new_item{}],
%  [#new_ger{}]]
%
%
%
%

-define(REWARD_COIN, 1).% 银两
-define(REWARD_GOLD, 2).% 元宝
-define(REWARD_REPU, 3).% 声望
-define(REWARD_ROLE_EXP,4).% 主公经验
-define(REWARD_GER_EXP,5).% 武将经验
-define(REWARD_ITEM, 6).% 道具
-define(REWARD_GER, 7).% 武将
-define(REWARD_FAMILY_CONTRIBUTION, 8).% 公会活跃度
-define(REWARD_PROFOUNDCRYSTAL,9).     % 奥义结晶
-define(REWARD_HONOR,10).     % 荣誉
-define(REWARD_PVPPOINT,11).     % 竞技场点数
-define(REWARD_HOME_RESOURCE,12).     % 家园物资

-define(ITEM_TYPE_REPUTATION,20006).          %% 徽章
-define(ITEM_TYPE_COIN,20007).                %% 金币
-define(ITEM_TYPE_GOLD,20008).                %% 钻石
-define(ITEM_TYPE_UNION_COIN,20027).          %% 公会货币
-define(ITEM_TYPE_PROFOUNDCRYSTAL,20037).     %% 奥义结晶的ITEM_ID
-define(ITEM_TYPE_HONOR,20040).               %% 荣誉的ITEM_ID
-define(ITEM_TYPE_PVPPOINT,20041).            %% 竞技场点数的ITEM_ID
-define(ITEM_TYPE_HOME_RESOURCE,20045).       %% 家园物质的ITEM_ID
-define(ITEM_TYPE_TICKET,20050).              %% 点劵
-define(ITEM_TYPE_LAPUTASTONE,20051).         %% 飞行币

-define(carlos_honor_double,carlos_honor_double).
-define(pvp_reward_double,pvp_reward_double).
-define(hron_reward_double,hron_reward_double).
-define(relic_box_double,relic_box_double).
-define(homestead_reward_double,homestead_reward_double).
-define(world_boss_box_double,world_boss_box_double).

-define(SPECIAL_CURRENCY_LIST,[?ITEM_TYPE_REPUTATION,?ITEM_TYPE_COIN,?ITEM_TYPE_GOLD,?ITEM_TYPE_UNION_COIN,?ITEM_TYPE_PROFOUNDCRYSTAL,?ITEM_TYPE_HONOR,?ITEM_TYPE_PVPPOINT,?ITEM_TYPE_HOME_RESOURCE,?ITEM_TYPE_TICKET,?ITEM_TYPE_LAPUTASTONE]).
