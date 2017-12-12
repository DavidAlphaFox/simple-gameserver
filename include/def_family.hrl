-define(FAMILY_TITLE_MEMBER, 0).
-define(FAMILY_TITLE_OWNER, 1).
-define(FAMILY_TITLE_DEPUTY, 2).

%% 请求保护状态编号
-define(PROTECT_FAMILY_CREATE, 1).
-define(PROTECT_FAMILY_JOIN, 2).
-define(PROTECT_FAMILY_LEAVE, 3).
-define(PROTECT_FAMILY_KICK, 4).
-define(REPLAY_TYPE_FAMILY_FIGHT,0).
-define(familyfight_group_base,100000).
-define(family_tek,family_tek).
-define(familyID,familyID).
-define(family_storage,family_storage).
-define(family_wallet,family_wallet).
-define(FAMILY_CONTRIBUTE_LIST,family_contribute_list).

%%公会捐献类型
-define(COIN_DONATE,1).                % //1=> 金币
-define(DIAMOND_DONATE,2).             % //2=> 钻石
-define(REPUTATION_DONATE,3).          % //3=> 徽章
-define(ITEM_DONATE,4).                % //4=> 装备               
-define(GER_DONATE,5).                 % //5=> 精灵

%% 公会战、全服排名共用数据的etc表
-define(FAMILY_FIGHT_MASTER_ETC, family_fight_master_etc).
%% 保存服务器ID->节点的映射
-define(SLAVE_INFO, slave_info).
%% 保存公会ID->实际所在的服务器ID的映射
%% 这是个冗余表,实际可以通过family_id计算出对应的服务器ID，
%% 而slave_info里面会记录合服和未合服的所有ID->节点的映射关系
-define(FAMILY2ServerID, family_to_serverid).

%%公会副本状态
-define(F_INST_CLOSE,1).
-define(F_INST_OPEN,2).
-define(F_INST_FIGHT,3).
-define(F_INST_WIN,4).
