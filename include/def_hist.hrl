

%% =================================
%% dictionary key in role server




%% =================================

%% 战报类型
-define(HIST_YOU_FIGHT, 2#00000000).% 你发起战斗
-define(HIST_YOU_BE_FIGHT,2#10000000).% 别人发起战斗
-define(HIST_WIN, 2#01000000).% 发起者战斗胜利
-define(HIST_FAIL,2#00000000).% 发起者战斗失败
-define(HIST_SUCC_PATCH, 2#00100000).% 成功夺宝
-define(HIST_FAIL_PATCH, 2#00000000).% 未成功夺宝
-define(HIST_PVP, 2#00010000).% PVP战报
-define(HIST_PLUNDER,2#00000000).% 夺宝战报
   
-define(TYPE_PVP, 4).
-define(TYPE_PLUNDER, 5).
   
%% 战报类型，注释见,"115hist.proto"
-record(hist, {
			   histUID
			   ,histType
			   ,name
			   ,enemyID
			   ,time
			   ,arg
  			   ,isRead=false	:: boolean()				%% 是否读过这封战报
			  }).

   
%% 战报数据库结构
-record(d_hist, {
				 %id	:: {RoleID::?int32,Type::?int8} %key
				histList :: [#hist{}]		%战报列表
				,unreadNum :: ?int16		%未读战报数量
				}).   