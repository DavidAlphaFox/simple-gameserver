%%道具类型定义
-define(weapon																,weapon                          			  ).
-define(armor																					,armor												).
-define(horse																,horse									).
-define(treasure_physical_damage_addtion			,		treasure_physical_damage_addtion).
-define(treasure_physical_damage_reduction			,treasure_physical_damage_reduction).
-define(treasure_magic_damage_addtion				,treasure_magic_damage_addtion).
-define(treasure_magic_damage_reduction				,treasure_magic_damage_reduction).
-define(treasure_critic							,treasure_critic).
-define(treasure_critic_reduce						,treasure_critic_reduce).
-define(treasure_doom							,treasure_doom).
-define(treasure_miss							,treasure_miss).
-define(treasure_sp_init						,treasure_sp_init).
-define(treasure_tianqian						,treasure_tianqian).
-define(treasure_dikun							,treasure_dikun).
-define(treasure_leizhen						,treasure_leizhen).
-define(treasure_fengxun						,treasure_fengxun).
-define(treasure_shuikan						,treasure_shuikan).
-define(treasure_huoli							,treasure_huoli).
-define(treasure_shangeng						,treasure_shangeng).
-define(treasure_zedui							,treasure_zedui).
-define(treasure_sp_left							,treasure_sp_left).
-define(treasure_absorb							,treasure_absorb).
-define(treasure_damage_back						,treasure_damage_back).
-define(treasure_reel							,treasure_reel).
-define(treasure_reel_reduce						,treasure_reel_reduce).
-define(material								,material).
-define(patch_treasure								,patch_treasure).
-define(other									,other).
-define(soul_general	                        ,soul_general	).
-define(debris_weapon	                        ,debris_weapon	).
-define(debris_armor	                        ,debris_armor	).
-define(debris_horse	                        ,debris_horse	).
-define(box										,box).
-define(formula, formula).
-define(add_times, add_times).
-define(wing,wing).					%%翅膀
-define(headwear,headwear).	%%头饰
-define(totem,totem).				%%图腾
-define(runestone,runestone).	%%符石
-define(acceleratestone,acceleratestone).   %% 加速宝石
-define(accelerateslate,accelerateslate).   %% 加速石板
-define(stonephydefbite,stonephydefbite).   %% 物理攻击符文，注意这不是普通装备！
-define(stonephydef,stonephydef).           %% 物理防御符文，注意这不是普通装备！
-define(stonemagdefbite,stonemagdefbite).   %% 魔法攻击符文，注意这不是普通装备！
-define(stonemagdef,stonemagdef).           %% 魔法防御符文，注意这不是普通装备！
-define(stoneessencehp,stoneessencehp).     %% 生命精华符文
-define(stoneessenceab,stoneessenceab).     %% 破甲精华符文
-define(stoneessencedr,stoneessencedr).     %% 双抗精华符文
-define(stoneessencemb,stoneessencemb).     %% 法穿精华符文

-define(stoneessencehpgod,stoneessencehpgod).
-define(stoneessencedrgod,stoneessencedrgod).
-define(stoneessenceabgod,stoneessenceabgod).
-define(stoneessencembgod,stoneessencembgod).
-define(stonephydefbitegod,stonephydefbitegod).
-define(stonemagdefbitegod,stonemagdefbitegod).
-define(stonephydefgod,stonephydefgod).
-define(stonemagdefgod,stonemagdefgod).
 

-define(stone,stone).                       %% 经验符文，注意这不是普通装备！
-define(stonechip,stonechip).               %% 符文碎片，注意这不是普通装备！也不是符文！
-define(trainer_equip,trainer_equip).       %% 训练师装备

-define(trainer_weapon,trainer_weapon).         %% 训练师武器装备
-define(trainer_gloves,trainer_gloves).         %% 训练师手套装备，位置相当于宠物的头饰
-define(trainer_armor,trainer_armor).           %% 训练师衣服装备
-define(trainer_glasses,trainer_glasses).       %% 训练师眼镜装备，位置相当于宠物的翅膀
-define(trainer_watch,trainer_watch).           %% 训练师手表装备，位置相当于宠物的符石
-define(trainer_boost,trainer_boost).           %% 训练师鞋子装备，位置相当于宠物的图腾

-define(trainer_stone_ruby,trainer_stone_ruby).      %% 训练师红宝石装备
-define(trainer_stone_topaz,trainer_stone_topaz).     %% 训练黄宝石师装备
-define(trainer_stone_sapphire,trainer_stone_sapphire).  %% 训练师蓝宝石装备

-define(EQUIP_TYPE_LIST_NO_STONE, [?weapon,?headwear,?armor,?wing,?runestone,?totem]).
-define(STONE_TYPE_LIST, [?stonephydefbite,?stonephydef,?stonemagdefbite,?stonemagdef]).
-define(TRAINER_EQUIP_TYPE, [?trainer_weapon,?trainer_gloves,?trainer_armor,?trainer_glasses,?trainer_watch,?trainer_boost
                             ,?trainer_stone_ruby,?trainer_stone_topaz,?trainer_stone_sapphire]).
-define(TRAINER_EQUIP_TYPE_NO_STONE, [?trainer_weapon,?trainer_gloves,?trainer_armor,?trainer_glasses,?trainer_watch,?trainer_boost]).
-define(TRAINER_STONE_TYPE_LIST, [?trainer_stone_ruby,?trainer_stone_topaz,?trainer_stone_sapphire]).

-define(STONE_TYPE,[?stonephydefbite,?stonephydef,?stonemagdefbite,?stonemagdef,?stone]).
-define(ESSENCE_STONE_TYPE,[?stoneessencehp,?stoneessencedr,?stoneessencemb,?stoneessenceab]).
-define(GER_ACCELERATE_EQUIP_TYPE,[?accelerateslate,?acceleratestone]).    %%精灵加速装备列表
%% 装备位置定义
-define(ITEM_POS_WEAPON, 1).
-define(ITEM_POS_HEADWEAR,2).
-define(ITEM_POS_ARMOR, 3).
-define(ITEM_POS_WING,4).
-define(ITEM_POS_RUNESTONE,5).
-define(ITEM_POS_TOTEM,6).

%% 符文位置定义
-define(ITEM_POS_STONE_FIRST,11).
-define(ITEM_POS_STONE_SECOND,12).
-define(ITEM_POS_STONE_THREE,13).
%%听说前端14被占用了，就换了15
-define(ITEM_POS_STONE_FOURTH,15).
-define(ITEM_POS_STONE_FIFTH,16).
-define(ITEM_POS_STONE_SIXTH,17).

%%加速装备位置
-define(ITEM_POS_ACCELERATE_STONE,18).
-define(ITEM_POS_ACCELERATE_SLATE,19).

%% 训练师装备位置定义
-define(ITEMPOS_TRAINER_WEAPON, 121).
-define(ITEMPOS_TRAINER_HEADWEAR,122).
-define(ITEMPOS_TRAINER_ARMOR, 123).
-define(ITEMPOS_TRAINER_WING,124).
-define(ITEMPOS_TRAINER_RUNESTONE,125).
-define(ITEMPOS_TRAINER_TOTEM,126).
-define(ITEMPOS_TRAINER_STONE_FIRST,131).
-define(ITEMPOS_TRAINER_STONE_SECOND,132).
-define(ITEMPOS_TRAINER_STONE_THREE,133).

-define(MAX_RANK_OF_MAIN_EQUIP, 19).

%% 分解系统的分解类型
-define(GER_DECOMPOSE,1).                    %%精灵分解 
-define(GER_EQUIPMENT_DECOMPOSE,2).          %%精灵装备分解
-define(STONE_DECOMPOSE,3).                  %%符文分解
-define(TRAINER_EQUIPMENT_DECOMPOSE,4).      %%训练师装备分解
-define(MAGE_DECOMPOSE,5).                   %%mage石和键石分解
-define(ACCELERATE_EQUIPMENT_DECOMPOSE,6).   %%加速装备分解
-define(MAX_RANK_OF_TREASURE, 9).
-define(BagItemType,[?other,?material,?soul_general,?debris_weapon,?debris_armor,?debris_horse,?box,?formula,?add_times]).

%活跃度物品ID
-define(ITEM_ID_ACTIVITY, 20038). %表示活跃度的道具ID

%%附魔类型
-define(ENCHANT_TYPE_WATER,1).
-define(ENCHANT_TYPE_FIRE,2).
-define(ENCHANT_TYPE_GRASS,3).
-define(ENCHANT_TYPE_LIGHTNING,4).
-define(ENCHANT_TYPE_FLY,5).
-define(ENCHANT_TYPE_COMBAT,6).
-define(ENCHANT_TYPE_SUPER,7).
-define(ENCHANT_TYPE_ROCK,8).
-define(ENCHANT_TYPE,[?ENCHANT_TYPE_WATER,?ENCHANT_TYPE_FIRE,?ENCHANT_TYPE_GRASS,?ENCHANT_TYPE_LIGHTNING,?ENCHANT_TYPE_FLY,?ENCHANT_TYPE_COMBAT,?ENCHANT_TYPE_SUPER,?ENCHANT_TYPE_ROCK]).

-define(KEYSTONELIST,[7010,7020,7030,7040,7050,7050,7060,7070,7080]).

-define(LEGENDIDSHIFT, 50).
-define(LEGENDLIST,[10769,10770,10771,10772,10773,10774
                   ,10775,10776,10777,10778,10779,10780]).

-define(STONE_LEGEND_LIST, [?stoneessencehpgod,?stoneessencedrgod,?stoneessenceabgod,?stoneessencembgod,?stonephydefbitegod,?stonemagdefbitegod,?stonephydefgod,?stonemagdefgod]).
-define(STONE_ESSENCE_LEGEND_LIST, [?stoneessencehpgod,?stoneessencedrgod,?stoneessenceabgod,?stoneessencembgod]).
-define(STONE_NORMAL_LEGEND_LIST, [?stonephydefbitegod,?stonemagdefbitegod,?stonephydefgod,?stonemagdefgod]).
-define(STONE_CAN_LEGEND_LIST,[5035,5036,5037,5038,5039,5040,5041,5042,5043,5044,5045,5046,5055,5056,5057,5058]).
%%技能宝石类型
-define(killdiamond,killdiamond).                               %%秒杀技能宝石
-define(invinciblediamond,invinciblediamond).                   %%无敌技能宝石
-define(controldiamond,controldiamond).                         %%控制技能宝石
-define(controlrebackdiamond,controlrebackdiamond).             %%受到攻击反向控制
-define(avoidhurtdiamond,avoidhurtdiamond).                     %%免伤
-define(curediamond,curediamond).                               %%治疗
-define(groupcurediamond,groupcurediamond).                     %%群体治疗
-define(uniquetargetadddiamond,uniquetargetadddiamond).         %%必杀目标增加
-define(normaltargetadddiamond,normaltargetadddiamond).         %%普攻目标增加
-define(specialdiamond,specialdiamond).                         %%专属

-define(DIAMOND_TYPE_LIST,[?killdiamond,?invinciblediamond,?controldiamond,?controlrebackdiamond,?avoidhurtdiamond,?curediamond,?groupcurediamond,?uniquetargetadddiamond,?normaltargetadddiamond,?specialdiamond]).