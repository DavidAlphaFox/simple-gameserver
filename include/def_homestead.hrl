-define(HOMESTEAD_ERROR_NOT_OPEN,2).								%%家园没有开启
-define(HOMESTEAD_ERROR_ONLY_REQUEST_FRIEND,3).			%%只能请求好友家园	
-define(HOMESTEAD_ERROR_FRIEND_NOT_OPEN,4).					%%好友家园没有开启
-define(HOMESTEAD_ERROR_MACHINE_WAS_UNLOCK,5).		%%机器已经开启
-define(HOMESTEAD_ERROR_NOT_UNLOCK_MACHINE,6).		%%该机器前面还有未开始的机器，不能开启
-define(HOMESTEAD_ERROR_NOT_MACHINE,7).						%%没有该机器
-define(HOMESTEAD_ERROR_GOLD_NOT_ENOUGH,8).				%%钻石不足
-define(HOMESTEAD_ERROR_MACHINE_NOT_UNLOCK,9).		%%机器没有解锁
-define(HOMESTEAD_ERROR_MACHINE_NOT_SEED,10).	%%没有播种
-define(HOMESTEAD_ERROR_UPROOT_SEED_HAS_HARVEST,11).	%%还没有收成,不能铲除
-define(HOMESTEAD_ERROR_MACHINE_WAS_SEEDING,12).			%%机器已经播种
-define(HOMESTEAD_ERROR_NO_SEED,13).								%%种子不存在
-define(HOMESTEAD_ERROR_SEED_NOT_RIPE,14).						%%种子还没有成熟
-define(HOMESTEAD_ERROR_NO_MORE_HARVEST,15).				%%没有收成
-define(HOMESTEAD_ERROR_CHANGE_GER_NOT_GER,16).		%%没有该武将
-define(HOMESTEAD_ERROR_FRIEND_MATING_BUSY,17).			%%好友神兽忙碌
-define(HOMESTEAD_ERROR_FRIEND_MACHINE_BUSY,18).		%%好友机器忙碌
-define(HOMESTEAD_ERROR_FRIEND_MACHIE_UNLOCK,19).			%%好友这个机器没有解锁
-define(HOMESTEAD_ERROR_MATING_TIMES_NOT_ENOUGH,20).	%%交配次数不足
-define(HOMESTEAD_ERROR_ADDENERGY_TIMES_NOT_ENOUGH,21).	%%充能次数不足
-define(HOMESTEAD_ERROR_NOT_ADDENERGY_NO_SEED,22).				%%不能给没有种子的机器充能
-define(HOMESTEAD_ERROR_FRIEND_NOT_GER,23).									%%好友没有守护神兽
-define(HOMESTEAD_ERROR_NOT_GER,24).									%%没有守护神兽
-define(HOMESTEAD_ERROR_GER_ALREADY_ON, 25).                %%已经放入了此守护神兽
-define(HOMESTEAD_ERROR_USER_NOT_EXIST,26).					%%请求玩家不存在
-define(HOMESTEAD_ERROR_MACHINE_HAS_HARVEST,27).			%%机器还没有收成、不能播种
-define(HOMESTEAD_ERROR_MIRROR,28).     %%没有该武将

-define(HOMESTEAD_GET_HARVEST_COUNT,1).				%%领取产出最大次数

-define(HOMESTEAD_DATA_CHANGE_ROLEID_LIST,homestead_data_change_roleid_list).

-define(HOMESTEAD_REFRESH_MATING_TIMS_HOUR,[10,16,22]).%%每日10、16、22时重置交配次数

-define(constr_type_park, 1). %任务大厅
-define(constr_type_homestead, 2). %种植园
-define(constr_type_cirrus, 3). %魔腾
-define(constr_type_office, 4). %行政大厅
-define(constr_type_card, 5). %扭蛋
-define(constr_type_shop, 6). %商店
-define(constr_type_combine, 7). %合成炉
-define(constr_type_plane_base, 8). %飞行基地

-define(ht_normal_unaccept, 1). 
-define(ht_normal_accepted, 2). 
-define(ht_dounty_unaccept, 3). 
-define(ht_dounty_accepted, 4). 

-define(home_wood, 20072).
-define(home_stone, 20073).

-define(home_exchange_rate, 100). % 旧家园物资和新石头木材的兑换比例，为了兼容旧数据

-record(homeated_back_data,{
			matingCoolTime,						%%交配冷却时间
			matingTimes,								%%交配次数
			addEnergyTimes,						%%充能次数
			machineList}).								%%机器数据[{num,addenergyCoolTime}]