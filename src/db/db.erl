%% @author caohongyang
%% @doc	database operate interface
%% 抽象数据库层接口

-module(db).
-include("common.hrl").
-compile(export_all).
-export([]).

%% start() ->
%% 	mnesia:create_schema([]),
%% 	mnesia:start(),
%% 	init(),
%% 	mnesia:wait_for_tables(all_table(), infinity).
%% 
%% stop() ->
%% 	mnesia:stop().
%% 
%% init() ->
%% 	create_table().
%% 	%load_table().
%% 
%% %% @doc 创建表
%% create_table() ->
%% 	create_table(table_struct()).

%% @doc 读出表结构配置
table_struct() ->
	case file:consult(filename:join([tk_config:root_dir(), "config/app/table_struct.config"])) of
		{ok, [TableStruct]} ->
			TableStruct;
		{error, Reason} ->
			Error = io_lib:format("read config/table_struct.config error, Reason=~100p",[Reason]),
			?ERR(Error),
			[]
	end.

%% @doc 获取所有表
all_table() ->
	[TabName || {TabName, _TabType, _RecordName, _FieldsAndTypes} <- table_struct()].

%% %% @doc 初始化所有表
%% %% disk(磁盘表) ==>>  mnesia::disc_only_copies()
%% %% ram(内存表) ==>> 	mnesia::disc_copies()
%% create_table(TableStruct) ->
%% 	[begin
%% 		 Fields = [Field || {Field, _Type} <- FieldsAndTypes],
%% 		 %% 磁盘表
%% 		 case TabType of
%% 			 disk ->
%% 				 case mnesia:create_table(TabName, [{disc_only_copies, [node()]}, {attributes,Fields},{type,set},{record_name,RecordName}]) of
%% 					 {atomic, ok} ->
%% 						 ?INFO("创建表~-10w成功....",[TabName]);
%% 					 {aborted, {already_exists, _}} ->
%% 						 ignore;
%% 					 Error ->
%% 						 ?ERR("创建表~-10w失败...\n~200p",[TabName,Error])
%% 				 end;
%% 			 %% 内存表
%% 			 ram ->
%% 				 case mnesia:create_table(TabName, [{disc_copies, [node()]}, {attributes,Fields},{type,set},{record_name,RecordName}]) of
%% 					 {atomic, ok} ->
%% 						 ?INFO("创建表~-10w成功....",[TabName]);
%% 					 {aborted, {already_exists, _}} ->
%% 						 ignore;
%% 					 Error ->
%% 						 ?ERR("创建表~-10w失败...\n~200p",[TabName,Error])
%% 				 end
%% 		 end
%% 		
%% 	 end || {TabName, TabType, RecordName, FieldsAndTypes} <- TableStruct].
		 
connect_mysql() ->
	{IP,Port,Name,Pass,DBName,Num} = data_setting:get(database),
	emysql:add_pool(?DB, Num,Name,Pass, IP, Port,DBName, utf8),
	%% 预定义一些常用的sql语句
	emysql:prepare(get_role_info,<<"SELECT accid,roleName,isMale,level,exp,coin,reputation,gold,goldBonus,goldUsed,unioncoin,profoundCrystal,vipLevel,goldTotalPaid,title,fightPower,lastLogoutTime,familyID,lastJoinFamily,head,payExtReward,extRdActTime,location,isFailed,devid,srcType,tasklevel,plane_level,teamid,honor,pvppoint,carloswintime,carlosequaltime,carloslosetime,carlosseason,carlosprewintime,carlospreequaltime,carlosprelosetime,carlospreseason,home_resource,firtPayStatus,ticket,laputastone,transmigration,sGoldTotalPaid,svipLevel FROM gRole WHERE roleID= ?">>),
	emysql:prepare(check_role_created,<<"select roleID from gRole where accid= ?">>),
	emysql:prepare(get_role_extra,<<"select battleProgress,battleProgressHard,battleProgressFastHard,battleProgressTransmigration,energy,energyBuyTimes,challengeGodEnergy,challengeGodBuyTimes,lastChallengeGodDate,refreshLieuTimes,alreadyPayRefreshLieuTimes,dscvBuyTimes,pvpBuyTimes,plunderBuyTimes,coinBuyTimes,fireTimes,lastBuyTimesRefreshDate,lastEnergyTime,discoveryTimes,lastDscvTime,dscvCount,pvpTimes,plunderTimes,weiboCount,nextWeiboCountRefreshSec,lastWeiXinShareSec,lastPvpTime,lastPlunderTime,encounterList,lastTitleRewardDate,lastDrawTitle,lastLoggedLoginDate,lastDrawLoginRewardDays,loginDays,lastDrawLevelUpLevel,randomShopList,leftChgNameTimes,talentStudyBuyTimes,lastPayTime,magicBookState,teamPkTimes,teampPkBuyTimes,lastTeamPkTime,sign_day_count,is_get_sign_reward,last_sign_time,is_get_acc_sign_reward,battleBossReward,maingerTypeid,plane_ai_flag,maintask,energyPac,trainerProf,signSec from gRoleExtra where roleID= ?">>).

