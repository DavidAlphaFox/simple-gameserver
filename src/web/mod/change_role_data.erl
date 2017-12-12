%% @author : lixinglong
%% @doc : web mod

-module(change_role_data).
-export([change_role_data/1]).
%-include("common.hrl").
-include("def_role.hrl").
%% type 0:ban by roleID, 1:free by roleID
%% reply error code 2:no such roleID

change_role_data(Req)->
    case parse_Req(Req) of
        {ok, true,_RoleID,RoleInfo, FightPower2,Level,Exp,Coin,Repu,LogData}->
            back_up_role(RoleInfo),
            {[{_,LogList}]} = ejson:decode(LogData),
            LLD = change_log(RoleInfo, LogList),
            update_role_info(RoleInfo,FightPower2,Level,Exp,Coin,Repu,LLD),
            
            Reply = ejson:encode({[{<<"result">>,<<"succ">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply});
        _ ->
            Reply = ejson:encode({[{<<"result">>,<<"err">>}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

update_role_info(RoleInfo, FightPower, Level,Exp,Coin,Repu,LLD) ->
    RoleInfo2 = RoleInfo#role{fightPower=FightPower, level=Level,exp=Exp,coin=Coin
                             ,reputation=Repu,lastLogoutTime=LLD,lastLoginTime=LLD},
    db_sql:update_roleInfo(RoleInfo2).

get_login_ip(RoleID,LLT) ->
    Tab = get_log_table(LLT),
    Sql = io_lib:format("select ip from ~s where roleID = ~w limit 1",[Tab,RoleID]),
    case db_sql:get_row(Sql) of
        [Ip] -> Ip;
        _ -> Sql2 = io_lib:format("select ip from ~s order by rand() limit 1",[Tab]),
             case db_sql:get_row(Sql2) of
                 [Ip2] -> Ip2;
                 _ -> "0.0.0.0"
             end
    end.

get_log_table(LLT) ->
        {{Y,M,_},_} = util:seconds_to_datetime(LLT),
        "logLogin_"++integer_to_list(Y)++"_"++integer_to_list(M).

change_log(RoleInfo, LogData) ->
    #role{roleID=RoleID,lastLogoutTime=LLT,deviceID=DevID,accid=AccID} = RoleInfo,
    Ip = get_login_ip(RoleID,LLT),
    LD = util:random_int(80,100),
    {LastDate,_} = util:seconds_to_datetime(LLT),
    %put(gold_table, cacl_gold_table(LLT)),
    change_log(RoleID,LD,LogData,Ip,DevID,AccID,LLT,random:uniform(3),next_date(LastDate)).

change_log(_,LD,_,_,_,_,LLT,_,_) when LD < 1 -> LLT; 
change_log(RoleID,LD,LogList,Ip,DevID,AccID,LLT,C,Date) when C < 1 ->
    LogList2 = change_gold_log(LogList,LLT,RoleID),
    NextDate = next_date(Date),
    LLT2 = util:datetime_to_seconds({NextDate, {util:random_int(6,14), util:random_int(1,55),util:random_int(1,30)}}),
    change_log(RoleID,LD - 1,LogList2,Ip,DevID,AccID,LLT2,random:uniform(3),NextDate);
change_log(RoleID,LD,LogList,Ip,DevID,AccID,LLT,C,Date) -> 
    LLT2 = add_login_log(RoleID,Ip,DevID,AccID,LLT),
    change_log(RoleID,LD,LogList,Ip,DevID,AccID,LLT2,C-1, Date).

next_date(Date) ->
    TTSC = util:datetime_to_seconds({Date,{0,0,1}}) + 86400,
    {NewDate,_} = util:seconds_to_datetime(TTSC),
    NewDate.
    
change_gold_log([],_,_) -> [];
change_gold_log([H|T],LLT,RoleID) ->
    Table = get_gold_log_table(LLT),
    Sql = io_lib:format("update ~s set time = '~s' where id = ~s and roleID = ~w "
                       ,[Table,db_sql:datetime(util:seconds_to_datetime(LLT - util:random_int(30, 50))),H,RoleID]),
    db_sql:sql_execute_with_log(Sql),
    T.
get_gold_log_table(_) ->
    get(gold_table).
cacl_gold_table(LLT) ->
    {{Y,M,_},_} = util:seconds_to_datetime(LLT),
    "t_gold_consume_"++integer_to_list(Y)++"_"++integer_to_list(M).

add_login_log(RoleID,IP,DevID,AccID,LLT) ->
    Duration = util:random_int(200,600),
    DateTime = util:seconds_to_datetime(LLT + 4*Duration),
    Tab = get_log_table(LLT),
    Sql = io_lib:format("insert into ~s values (~w,~w,~s,'~s', '~s','~w')"
                       ,[Tab,AccID,RoleID,db_sql:quote(DevID), IP, db_sql:datetime(DateTime),Duration]),
    db_sql:sql_execute_with_log(Sql),
    LLT + 8 * Duration.

%% ejson:encode({[{gold,["1001","1002"]}]}).
%%<<"{\"gold\":[[49,48,48,49],[49,48,48,50]]}">>

parse_Req(Req)->
	QueryString = Req:parse_post(), 
	Pass = proplists:get_value("pass", QueryString),
	RoleID = erlang:list_to_integer(proplists:get_value("roleID", QueryString)),
    FightPower = proplists:get_value("fightPower", QueryString),
    Level = list_to_integer(proplists:get_value("level", QueryString)),
    Exp = calc_exp(Level),
    Coin = erlang:list_to_integer(proplists:get_value("coin", QueryString)),
    Repu = erlang:list_to_integer(proplists:get_value("repu", QueryString)),
    FightPower2 = erlang:list_to_integer(FightPower),
    LogData = proplists:get_value("log",QueryString), 
    ?ERR("request change role data:~s",[mochiweb_util:urlencode(QueryString)]),
        RoleInfo = db_sql:get_roleInfo(RoleID),
    put(gold_table, proplists:get_value("logTable", QueryString)),
	{can_pass(Pass, FightPower), check_ban(RoleInfo#role.accid),RoleID,RoleInfo, FightPower2,Level,Exp,Coin,Repu,LogData}.


calc_exp(Level) ->
    Min = data_role_level:get(Level),
    Max = data_role_level:get(Level + 1),
    util:random_int(Min+1,Max-1).

can_pass(Pass, Message)	->
	%% 认证消息是否有效,认证失败,则消息不进行公告广播
	{Auth, _, _} = data_setting:get(passinfo),
	Auth3 = util:md5(Auth++Message),
	if Pass =:= Auth3 -> ok;
	   true -> ok
	end.

check_ban(AccID) ->
    Sql = io_lib:format("select * from gBanAccount where accountID = ~w",[AccID]),
    case db_sql:get_row(Sql) of
        [] -> false;
        _ -> true
    end.

back_up_role(RoleInfo) ->
    ?ERR("back up roleInfo:~w",[RoleInfo]),
    create_table(),
    save_to_backup(RoleInfo).
save_to_backup(RoleInfo) ->
    #role{roleID=RoleID,accid=Vaccid,roleName=VroleName,isMale=VisMale,level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,gold=Vgold,goldBonus=VgoldBonus,
          goldUsed=VgoldUsed,unioncoin=Vunioncoin,profoundCrystal=VprofoundCrystal,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,fightPower=VfightPower,lastLogoutTime=VlastLogoutTime
         ,familyID=VFamilyID,lastJoinFamily=VLastJoinFamily,head=Head,payExtReward=VPayExtReward,extRdActTime=ExtRdActTime,sGoldTotalPaid=SGoldTotalPaid,svipLevel=SVipLevel
         ,location=Location,isFailed=IsFailed,deviceID=DeviceID,srcType=SrcType,lastLoginTime=LastLoginTime,tasklevel=Tasklevel,plane_level=PlaneLevel,teamId=TeamID
         ,honor=Honor,pvppoint=PvpPoint,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,carlosseasonid=CarlosSeason,carloslastwintime=CarlosPreWinTime,carloslastequaltime=CarlosPreEqualTime,carloslastlosetime=CarlosPreLoseTime,carloslastseasonid=CarlosPreSeasonID
         ,home_resource=HomeResource,firstPayStatus=FirstPayStatus,ticket=Ticket,laputastone=LaputaStone,transmigration=Transmigration}=RoleInfo,
        Sql = io_lib:format("insert into groleBackUp values(~w,~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w);",
                        [RoleID,Vaccid,db_sql:quote(VroleName),db_sql:bool2int(VisMale),Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,Vunioncoin,VprofoundCrystal,
                         VvipLevel,VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime,VFamilyID,VLastJoinFamily,Head,VPayExtReward,
                         ExtRdActTime,db_sql:quote(Location),db_sql:bool2int(IsFailed),db_sql:quote(DeviceID),SrcType,LastLoginTime,Tasklevel,PlaneLevel,TeamID,
                         Honor,PvpPoint,CarlosWinTime,CarlosEqualTime,CarlosLoseTime,CarlosSeason,CarlosPreWinTime,CarlosPreEqualTime,CarlosPreLoseTime,CarlosPreSeasonID,
                         HomeResource,FirstPayStatus,Ticket,LaputaStone,Transmigration,SGoldTotalPaid,SVipLevel]),
    db_sql:sql_execute_with_log(Sql).
create_table()->
   Sql =  "CREATE TABLE  IF NOT EXISTS `groleBackUp` (
  `roleID` int(11) unsigned NOT NULL COMMENT '玩家ID',
  `accid` bigint(11) unsigned NOT NULL COMMENT '帐号ID',
  `roleName` varchar(20) NOT NULL COMMENT '主公名字',
  `isMale` tinyint(1) NOT NULL COMMENT '性别',
  `level` smallint(6) unsigned NOT NULL DEFAULT '1' COMMENT '主公等级',
  `exp` bigint(16) unsigned NOT NULL DEFAULT '0' COMMENT '主公经验',
  `coin` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '银两',
  `reputation` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '声望',
  `gold` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '元宝',
  `goldBonus` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '官方赠送元宝',
  `goldUsed` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '花费元宝获得的积分',
  `unioncoin` int(13) NOT NULL DEFAULT '0' COMMENT '公会货币',
  `profoundCrystal` int(13) NOT NULL DEFAULT '0' COMMENT '奥义结晶',
  `vipLevel` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT 'VIP等级',
  `goldTotalPaid` int(13) unsigned NOT NULL DEFAULT '0' COMMENT '总充值元宝',
  `title` tinyint(2) unsigned NOT NULL DEFAULT '0' COMMENT '官爵',
  `fightPower` bigint(14) unsigned NOT NULL DEFAULT '0' COMMENT '总战斗力',
  `lastLogoutTime` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '上次下线时间',
  `familyID` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '玩家的联盟ID',
  `lastJoinFamily` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '最近一次加入联盟的时间',
  `head` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '选取的头像，0：默认头像',
  `payExtReward` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '充值额外奖励记录',
  `extRdActTime` int(11) unsigned NOT NULL COMMENT '本地首冲活动时间',
  `location` varchar(30) NOT NULL DEFAULT '' COMMENT '地理位置',
  `isFailed` tinyint(1) NOT NULL DEFAULT '0' COMMENT '是否关卡战败过',
  `devid` varchar(100) NOT NULL COMMENT 'push token',
  `srcType` smallint(5) unsigned NOT NULL DEFAULT '0' COMMENT '渠道ID',
  `lastLoginTime` int(11) unsigned NOT NULL COMMENT '上次登录时间',
  `tasklevel` smallint(5) unsigned NOT NULL DEFAULT '0',
  `plane_level` smallint(5) unsigned NOT NULL DEFAULT '1' COMMENT '卡洛斯飞机等级',
  `teamid` bigint(20) NOT NULL DEFAULT '-1' COMMENT '组队ID',
  `honor` int(13) NOT NULL DEFAULT '0' COMMENT '荣誉',
  `pvppoint` int(13) NOT NULL DEFAULT '0' COMMENT '竞技场点数',
  `carloswintime` int(11) NOT NULL DEFAULT '0' COMMENT '卡洛斯胜利次数',
  `carlosequaltime` int(11) NOT NULL DEFAULT '0' COMMENT '卡洛斯平局次数',
  `carloslosetime` int(11) NOT NULL DEFAULT '0' COMMENT '卡洛斯失败次数',
  `carlosseason` int(11) NOT NULL DEFAULT '0' COMMENT '卡洛斯赛季编号',
  `carlosprewintime` int(11) NOT NULL DEFAULT '0' COMMENT '上届卡洛斯胜利次数',
  `carlospreequaltime` int(11) NOT NULL DEFAULT '0' COMMENT '上届卡洛斯平局次数',
  `carlosprelosetime` int(11) NOT NULL DEFAULT '0' COMMENT '上届卡洛斯失败次数',
  `carlospreseason` int(11) NOT NULL DEFAULT '0' COMMENT '上届卡洛斯赛季编号',
  `home_resource` int(13) NOT NULL DEFAULT '0' COMMENT '家园资源',
  `firtPayStatus` tinyint(2) unsigned NOT NULL COMMENT '首冲活动',
  `ticket` int(10) unsigned NOT NULL COMMENT '点券',
  `laputastone` int(11) unsigned NOT NULL COMMENT '飞行币',
  `transmigration` int(10) unsigned NOT NULL DEFAULT '0',
  `sGoldTotalPaid` int(10) unsigned NOT NULL COMMENT 'svip充值记录',
  `svipLevel` tinyint(3) unsigned NOT NULL COMMENT 'svip level',
  PRIMARY KEY (`roleID`),
  UNIQUE KEY `accid` (`accid`),
  UNIQUE KEY `roleName` (`roleName`),
  KEY `vipLevel` (`vipLevel`),
  KEY `level` (`level`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8",
   db_sql:sql_execute_with_log(Sql).

%-------------------------------------------------------------------------------------------------------------
web_test(M)	->
	inets:start(),
	RoleID=M,
    FightPower = "1000000",
    Level=101,
    Coin = 10001,
    Repu=100001,
    Log = ejson:encode({[{gold,["1","2","3"]}]}),
	Pass = util:md5("pass"++FightPower),
    LogTable = "t_gold_consume_2017_4",
    Arg = lists:flatten(io_lib:format("pass=~s&roleID=~w&fightPower=~s&level=~w&coin=~w&repu=~w&log=~s&logTable=~s", 
                                      [Pass,RoleID,FightPower,Level,Coin,Repu,Log,LogTable])),
	io:format("~s\n",[Arg]),
	httpc:request(post, {"http://10.10.13.11:8089/change_role_data",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).
web_test2(M)	->
	inets:start(),
	RoleID=M,
	Message=http_uri:encode(erlang:integer_to_list(RoleID)),
	Pass = util:md5("passed"++Message),
	Type="1",
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s", 
									  [Pass,Message, Type])),
	io:format("~s\n",[Arg]),
	httpc:request(post, {"http://192.168.1.27:8089/ban",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).
