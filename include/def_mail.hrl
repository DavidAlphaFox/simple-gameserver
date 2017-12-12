

%% =================================
%% dictionary key in role server




%% =================================



-define(MAIL_TYPE_SYS,			1).% 系统消息     
-define(MAIL_TYPE_REWARD,		2).% 系统奖励     
-define(MAIL_TYPE_ADD_FRIEND,	3).% 加好友申请      
-define(MAIL_TYPE_JOIN_UNION,	4).% 加联盟申请      
-define(MAIL_TYPE_PRIVATE,		5).% 私人邮件       
-define(MAIL_TYPE_UNION,		6).% 联盟邮件       
-define(MAIL_TYPE_INVITE,       9).% 邀请邮件,7似乎被什么战报占用了

-define(MAIL_TEMPLATE_ADD_FRIEND_REQUEST, 1001).%% 好友申请
-define(MAIL_TEMPLATE_DEL_FRIEND, 		1002).%% 删除好友通知
-define(MAIL_TEMPLATE_ADD_FRIEND_REQUEST_TRANSMIGRATION, 1109).%% 好友申请
-define(MAIL_TEMPLATE_DEL_FRIEND_TRANSMIGRATION,       1110).%% 删除好友通知
-define(MAIL_BIND_WEIBO_SUCC,			1003).%% 绑定微博成功奖励
-define(MAIL_INVITE_GUY_FIRST_PAY_REWARD,1004).%% 邀请好友充值奖励
-define(MAIL_HULA_KILL, 1005).%% 虎牢关击杀奖励
-define(MAIL_HULA_LUCKY, 1006).%% 虎牢关幸运奖励
-define(MAIL_HULA_RANK_THREE, 1007).%% 虎牢关排名前3奖励
-define(MAIL_HULA_OFFLINE, 1008).%% 虎牢关离线参与奖励
-define(MAIL_HRON_FOUR_STAR, 1009).%% 华容道4星   
-define(MAIL_HRON_THREE_STAR, 1010).%% 华容道3星   
-define(MAIL_HRON_TWO_STAR, 1011).%% 华容道2星   
-define(MAIL_HRON_ONE_STAR, 1012).%% 华容道1星   
-define(MAIL_KING_FIRST,1013).%%皇权冠军
-define(MAIL_KING_SECOND,1014).%%皇权亚军
-define(MAIL_KING_OTHER,1015).%%皇权其他排名奖励
-define(MAIL_OTHER_REWARD, 0).%%其他奖励
-define(MAIL_HULA_RANK_FIRST10, 1017).%% 虎牢关排名前10奖励
-define(MAIL_HULA_RANK_JOIN, 1018).%% 虎牢关参与奖励
-define(MAIL_NANM_KILL, 1020).%% 战南蛮击杀奖励
-define(MAIL_NANM_LUCKY, 1021).%% 战南蛮幸运奖励
-define(MAIL_NANM_RANK_FIRST10, 1022).%% 战南蛮排名前10奖励
-define(MAIL_NANM_RANK_JOIN, 1023).%% 战南蛮参与奖励
-define(MAIL_NANM_OFFLINE, 1025).%% 战南蛮离线参与奖励
-define(MAIL_BET_EMPEROR_WIN, 1028). % 帝王争霸战下注成功奖励
-define(MAIL_BET_EMPEROR_LOSE, 1029). % 帝王争霸战下注失败返还
-define(MAIL_EMPEROR_WIN,1026). %帝王争霸战皇帝获得
-define(MAIL_JOIN_EMPEROR,1027). %帝王争霸战参与获得
-define(MAIL_FIRECRACKER_RETURN_GOLD, 1030). % 燃放爆竹返还元宝
-define(MAIL_FIRECRACKER_RANK_REWARD, 1031). % 燃放爆竹排名奖励
-define(MAIL_TREAHOUSE_RANK_REWARD, 1050). % 汉帝宝库排行榜获得
-define(MAIL_CROSS_SIGN, 1032).
-define(MAIL_CROSS_SUPPORT_FAIL, 1033).
-define(MAIL_CROSS_SUPPORT_SUCC, 1034).
-define(MAIL_CROSS_SKY_FIRST, 1035).
-define(MAIL_CROSS_SKY_SECOND, 1036).
-define(MAIL_CROSS_SKY_EIGHT, 1037).
-define(MAIL_CROSS_SKY_SIGN, 1038).
-define(MAIL_CROSS_GROUND_FIRST, 1039).
-define(MAIL_CROSS_GROUND_SECOND, 1040).
-define(MAIL_CROSS_GROUND_EIGHT, 1041).
-define(MAIL_CROSS_GROUND_SIGN, 1042).
-define(MAIL_CROSS_REWARD_ALL_SKY_FIRST, 1043).
-define(MAIL_CROSS_REWARD_ALL_SKY_SECOND, 1044).
-define(MAIL_CROSS_REWARD_ALL_GROUND_FIRST, 1045).
-define(MAIL_CROSS_REWARD_ALL_GROUND_SECOND, 1046).
-define(MAIL_CROSS_ENTER_SKY_LIST, 1047).
-define(MAIL_CROSS_ENTER_GROUND_LIST, 1048).
-define(MAIL_REBATE_REWARD, 1049). % 压岁钱未领取奖励

-define(MAIL_RACE_GROUP_SIGN_REWARD, 1051). %华丽大赛参与奖
-define(MAIL_RACE_GROUP_FIRST_FOUR_REWARD, 1052). %华丽大赛小组赛奖励
-define(MAIL_RACE_FIRST_EIGHT_REWARD, 1053). %华丽大赛八强奖励
-define(MAIL_RACE_FIRST_FOUR_REWARD, 1054). %华丽大赛第四名奖
-define(MAIL_RACE_THIRD_REWARD, 1137). %华丽大赛季军奖 
-define(MAIL_RACE_SECOND_REWARD, 1055). %华丽大赛亚军奖
-define(MAIL_RACE_FIRST_REWARD, 1056). %华丽大赛冠军精灵王奖
-define(MAIL_RACE_ALIEN_NORMAL_RANK, 1059). %异星战场排名奖励
-define(MAIL_RACE_ALIEN_KILL_NUM, 1060). %异星战场胜利场次排名	
-define(MAIL_RACE_ALIEN_KILL_CON, 1061). %异星战场连胜排名奖励
-define(MAIL_RACE_ALIEN_GUESS_RIGHT, 1062). %异星战场竞猜正确
-define(MAIL_RACE_ALIEN_GUESS_WRONG, 1063). %异星战场竞猜错误
-define(MAIL_ALIEN_CHAMPION_REWARD, 1064). %异星战场冠军全服奖励邮件
-define(MAIL_FAMILY_ASSIGN_STORAGE,1065).%联盟物资分喷
%% -define(MAIL_FAMILYFIGHT_REWARD_WIN, 1066). %% 联盟战胜利
-define(MAIL_FAMILYFIGHT_REWARD_EQ, 1067). %%联盟战平局
-define(MAIL_FAMILYFIGHT_REWARD_LOSE, 1068). %% 联盟战失败

-define(MAIL_3V3_REWARD, 1071).        %3v3排行榜奖励 
-define(MAIL_LEVEL_RANK_REWARD, 1072). % 冲级排行奖励邮件
%% -define(MAIL_FAMILY_RETURN, 1082). % 公会解散返还钻石

-define(MAIL_FAMILY_WORSHIP_REWARD, 1065).  %公会膜拜奖励邮件

-define(MAIL_NONE_TEMPLATE_MAIL, 0). %% 由配置文件配置的邮件
   
%% 重复了0 0,重新定id
-define(MAIL_WEIXIN_SHARE_REWARD, 1054). % 微信分享奖励邮件
-define(MAIL_QUARTER_EMPEROR,1052). % 帝王战4强邮件奖励
-define(MAIL_SECOND_EMPEROR, 1051). % 帝王战第二名邮件奖励
-define(MAIL_FAMILY_BE_KICK, 1066). %被踢出联盟
-define(MAIL_FAMILY_BE_AGREE, 1067). %加联盟申请被同意
-define(MAIL_FAMILY_BE_REFUSE, 1068). %加联盟申请被拒绝
-define(MAIL_FAMILY_BE_AUTO_DISBAND, 1069). %公会自动解散通知

-define(MAIL_FAMILYFIGHT_REWARD_MEMBER, 1073). %% 联盟战星星奖励
-define(MAIL_FAMILYFIGHT_REWARD_WIN, 1074). %% 联盟战胜利奖励
-define(MAIL_FAMILYFIGHT_BYE_REWARD, 1075). %% 公会战轮空奖励

%% 异星总决赛的
-define(MAIL_ALIEN_FINALS_GROUP_REWARD, 1080). %%异星八强奖励
-define(MAIL_ALIEN_FINALS_QUARTER_REWARD, 1081). %%异星四强奖励
-define(MAIL_ALIEN_FINALS_SEMIFINAL_REWARD, 1082). %%异星半决赛奖励
-define(MAIL_ALIEN_FINALS_CHAMPION_REWARD, 1083). %%异星冠军奖励
-define(MAIL_ALIEN_FINALS_WELFARE_REWARD, 1084). %%异星冠军福利
-define(MAIL_ALIEN_FINALS_GAMBLE_HIT_REWARD, 1085). %%押中
-define(MAIL_ALIEN_FINALS_GAMBLE_MISS_REWARD, 1086). %%没有押中
-define(MAIL_ALIEN_FIELD_CHAMPION_REWARD, 1087).%%战场第一名的奖励
-define(MAIL_ALIEN_FINALS_CHAMPION_REWARD2, 1088).%%战场第一名的奖励

-define(MAIL_PLUNDER_STONECHIP_RECOVER, 1090). %% 符文碎片满了以后，发送邮件包含此符文碎片
-define(MAIL_DISCOUNT_PAY_ACTIVITY_REWARD,1091). %%限时打折活动中的充值兑换活动未领取奖励是发送

-define(MAIL_TEMPLATE_ADD_FRIEND_INVITE, 1093).%% 公会邀请
-define(MAIL_TEMPLATE_CARLOS_EQUAL, 1094).% 卡洛斯平局奖励邮件
-define(MAIL_TEMPLATE_CARLOS_Win, 1095).% 卡洛斯胜利奖励邮件
-define(MAIL_TEMPLATE_CARLOS_FAIL,1096).% 卡洛斯失败奖励邮件
% -define(MAIL_TEMPLATE_CARLOS_EQUAL_1, 1104).% 卡洛斯平局奖励邮件
% -define(MAIL_TEMPLATE_CARLOS_Win_1, 1105).% 卡洛斯胜利奖励邮件
% -define(MAIL_TEMPLATE_CARLOS_FAIL_1,1106).% 卡洛斯失败奖励邮件
%% -define(MAIL_TEMPLATE_CARLOS_MINE,1097).% 卡洛斯占冷矿坑邮件
%% -define(MAIL_TEMPLATE_CARLOS_KILL,1098).% 卡洛斯杀十个人邮件
-define(MAIL_CARLOS_SEASON_RANK_REWARD,1097). %卡洛斯赛季排名奖励
-define(MAIL_TEMPLATE_CARLOS_BAN,1098). % 卡洛斯挂机邮件
-define(MAIL_LUCKY_ROLL_RANK_REWARD,1099).% 幸运大转盘奖励

-define(MAIL_RELIC_REWARD_WIN, 1100). %% 巨龙遗迹获胜
-define(MAIL_RELIC_REWARD_LOSE, 1101). %% 巨龙遗迹失败
-define(MAIL_TYPE_FAMILY_PASS_CHAPTER,1102).%%公会boss奖励钻石
-define(MAIL_PVP_RANK_REWARD,1103).
-define(MAIL_RELIC_REWARD_WIN_OUTER, 1104). %% 巨龙遗迹获胜-挂机者

-define(MAIL_TEMPLATE_GALACTICA_WIN,1105).%% 卡拉狄加胜利
-define(MAIL_TEMPLATE_GALACTICA_EQUAL,1106).%% 卡拉狄加平局
-define(MAIL_TEMPLATE_GALACTICA_FAIL,1107).%% 卡拉狄加失败
-define(MAIL_TEMPLATE_GALACTICA_BAN, 1108).%% 卡拉狄加挂机

-define(MAIL_HOMETASK_NORMAL_REWARD1,1111). %% 1）非悬赏任务成功，但没有获得宝箱奖励（无参数）
-define(MAIL_HOMETASK_NORMAL_REWARD2,1112). %% 2）非悬赏任务成功，获得了宝箱奖励（无参数）
-define(MAIL_HOMETASK_NORMAL_FAIL,1113). %% 3）非悬赏任务失败（无参数）
-define(MAIL_HOMETASK_BOUNTY_TIMEOUT,1114). %% 4）悬赏任务过期失效（无参数）

-define(MAIL_HOMETASK_BOUNTY_OWN_REWARD1,1115). %% 8）完成了XXX发布的悬赏任务，自己没有获得宝箱奖励（参数：领取人名字）
-define(MAIL_HOMETASK_BOUNTY_OWN_REWARD2,1116). %% 9）完成了XXX发布的悬赏任务，自己获得了宝箱奖励（参数：领取人名字）
-define(MAIL_HOMETASK_BOUNTY_OWN_FAIL,1117). %% 10）执行XXX发布的悬赏任务，不幸失败了（参数：领取人名字）

-define(MAIL_TEMPLATE_TWINS_WIN,1118).% 双子胜利
-define(MAIL_TEMPLATE_TWINS_FAIL,1119).% 双子失败
-define(MAIL_TEMPLATE_TWINS_KICK,1120).% 双子挂机

-define(MAIL_HOMETASK_BOUNTY_ACP_REWARD1,1121). %% 5）悬赏任务被某人领取并成功完成，自己没有获得宝箱奖励（参数：领取人名字）
-define(MAIL_HOMETASK_BOUNTY_ACP_REWARD2,1122). %% 6）悬赏任务被某人领取并成功完成，自己获得了宝箱奖励（参数：领取人名字）
-define(MAIL_HOMETASK_BOUNTY_ACP_FAIL,1123). %% 7）悬赏任务被某人领取但失败了（参数：领取人名字）

-define(MAIL_DOUBLEMATCH_RANK_REWARD,1124). %% 双排赛季奖励
-define(MAIL_CIRRUS_REWARD,1125).
-define(MAIL_HOME_BOSS_FROM_REWARD,1126).% 家园boss来源者奖励
-define(MAIL_HOME_BOSS_OWN_REWARD1,1127).% 家园boss放置者奖励type=1
-define(MAIL_HOME_BOSS_OWN_REWARD2,1128).% 家园boss放置者奖励type=2
-define(MAIL_HOME_BOSS_OWN_REWARD3,1129).% 家园boss放置者奖励type=3
-define(MAIL_TEMPLATE_ANUBIS_EQUAL,1130).% 阿努比斯平局奖励邮件
-define(MAIL_TEMPLATE_ANUBIS_WIN,1131).%%阿努比斯胜利奖励邮件
-define(MAIL_TEMPLATE_ANUBIS_LOSE,1132).%%阿努比斯失败奖励邮件
-define(MAIL_TEMPLATE_ANUBIS_SEASON_MEMBER,1136).%%阿努比斯赛季所有成员奖励邮件
-define(MAIL_TEMPLATE_ANUBIS_SEASON_LEADER,1134).%%阿努比斯赛季会长奖励邮件
-define(MAIL_TEMPLATE_ANUBIS_SEASON_VICLEADER,1135).%%阿努比斯赛季副会长奖励邮件
-define(MAIL_TEMPLATE_CONQUERISLAND_REWARD_WIN,1138).%%征服之岛胜利奖励
-define(MAIL_TEMPLATE_CONQUERISLAND_REWARD_EQUAL,1140).%%征服之岛平局奖励
-define(MAIL_TEMPLATE_CONQUERISLAND_REWARD_LOSE,1139).%%征服之岛失败奖励
-define(MAIL_TEMPLATE_CONQUERISLAND_REWARD_KICK,1141).%%挂机

-define(MAIL_RACE2_GAMBLE_GUANJUN_RIGHT, 1057).
-define(MAIL_RACE2_GAMBLE_GUANJUN_WRONG, 1058).
-define(MAIL_RACE2_GAMBLE_JIJUN_RIGHT, 1142).
-define(MAIL_RACE2_GAMBLE_JIJUN_WRONG, 1143).
-define(MAIL_RACE2_GAMBLE_BANJUESAI_RIGHT, 1144).
-define(MAIL_RACE2_GAMBLE_BANJUESAI_WRONG, 1145).
-define(MAIL_RACE2_GAMBLE_BAQIANG_RIGHT, 1146).
-define(MAIL_RACE2_GAMBLE_BAQIANG_WRONG, 1147).
-define(MAIL_RACE2_GAMBLE_XIAOZUSAI_RIGHT, 1148).
-define(MAIL_RACE2_GAMBLE_XIAOZUSAI_WRONG, 1149). % 压岁钱未领取奖励
-define(MAIL_CONSUME_BACK,1150).

-define(MAIL_REDPACKET_RANK, 1151).       %% 红包排名
-define(MAIL_CANVASS_REWARD,1152).        %% 调查奖励
-define(MAIL_REDPACKET_WORLD, 1153).       %% 跨服红包排名
-define(MAIL_DOJANGRANK_REWARD,1154).       %% 道馆竞技

-define(MAIL_CONSUMERANK_RANK,1171).      %%消费充值上榜
-define(MAIL_CONSUMERANK_NO_RANK,1172).   %%消费充值未上榜