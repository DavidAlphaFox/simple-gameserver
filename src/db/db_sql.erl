%% @author crimoon11
%% @doc @todo Add description to db_sql.


-module(db_sql).
-include("def_role.hrl").
-include("def_battle.hrl").
-include("def_mail.hrl").
-compile(export_all).
-include("def_hist.hrl").
-include("def_task.hrl").
-include("def_team_pk.hrl").
-include("def_fight.hrl").
-include("def_doublematch.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-define(GET_SQL_MAIL_NUM, 70).% the max num of mails get from sql once

%% 获取日志表的表名,日志表按月来分表
get_log_table_name(t_gold_pay_add) ->
    t_gold_pay_add;
get_log_table_name(TableName) when erlang:is_atom(TableName) ->
	{{Y, M, _}, _} = erlang:localtime(),
	lists:flatten(io_lib:format("~w_~w_~w", [TableName,Y,M])).

%% ====================================================================
%% 玩家数据 持久化
%% ====================================================================
sql_execute_with_log(Sql)	->
	case emysql:execute(?DB,Sql) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~s*****execute with err:~p,~s",[Sql,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~s*****execute with err:~p~n",[Sql,Exception])
	end.

%% 传入的Sql变量为多条sql语句的集合,最好加入事务的保护,保证整个数据操作的原子性
sql_execute_with_log2(Sql)   ->
    case emysql:execute(?DB,Sql) of
        [{ok_packet, _,_,_RetId,_,_,_},{ok_packet, _,_,RetId,_,_,_}]    ->
            {ok,RetId};
        {result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
            {ok,Rows};
        {error_packet, _, _,ErrCode,Reason2}    ->
            ?ERR("sql ****~s*****execute with err:~p,~s",[Sql,ErrCode,Reason2]),
            {error,{ErrCode,Reason2}};
        Exception ->
            ?ERR("sql ****~s*****execute with err:~p~n",[Sql,Exception])
    end.

sql_execute_sqls(Sql) ->
	Result = emysql:execute(?DB, Sql),
	lists:foldl(fun(E, CntAcc)->
						case E of
							{ok_packet, _,_,_RetId,_,_,_}	->
								CntAcc+1;
							{result_packet, _SeqNum,_FieldList,_Rows,_Extra} ->
								CntAcc+1;
							{error_packet, _, _,ErrCode,Reason2} ->
								?ERR("sql ****~s*****execute ~wth sqls with err:~p,~s",[Sql,CntAcc,ErrCode,Reason2]),
								CntAcc+1;
							Exception ->
								?ERR("sql ****~s*****execute ~wth sqls with err:~p~n",[Sql,CntAcc,Exception])
						end
					end,0, Result),
	ok.

sql_execute_with_log(Statement, Args)	->
	case emysql:execute(?DB,Statement,Args) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~p,~p*****execute with err:~p,~s",[Statement,Args,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~p,~p*****execute with err:~p~n",[Statement,Args,Exception])
	end.

get_all(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,List} ->
			List;
		_ ->
			[]
	end.

get_all(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			List;
		_ ->
			[]
	end.

get_row(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.

get_row(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.

get_rows(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,Lists} ->
			Lists;	 
		_ ->
			[]
	end.

%%删除所有机器人数据
delete_all_android()->
	sql_execute_with_log("delete from gRole where accid<10000").

search_all_roleName()->
	case get_all("select roleName from gRole;") of
		[]->
			[];
		List->
			[quote2(binary_to_list(RN))||RN<-lists:append(List)]
	end.

search_roleName(Name) ->
	Sql = io_lib:format("select roleID from gRole where roleName=~s;",[quote(Name)]),
	case get_row(Sql) of
		[RoleID] ->
			RoleID;
		_ ->
			?undefined
	end.
search_fuzzy_roleName(Name) ->
	Sql = io_lib:format("select roleID from gRole where roleName like '%~s%' and accid>10000 limit 30;",[quote2(Name)]),
	case get_all(Sql) of
		[]->
			?undefined;
		List->
			lists:append(List)
	end.

search_roleName2(Name) ->
	Sql = io_lib:format("select accid,roleID from gRole where roleName=~s;",[quote(Name)]),
	case get_row(Sql) of
		[AccountID, RoleID] ->
			[AccountID, RoleID];
		_ ->
			?undefined
	end.

get_all_roles()	->
	case get_row("select max(roleID) from gRole;") of
		[MaxId] when is_integer(MaxId) ->
			get_roles([], MaxId div 2000 + 2, MaxId div 2000 + 2);
		_ ->
			[]
	end.

get_roles(L, N, M) ->
	if N > 0 ->
			NextRL=get_roles(M - N),
			get_roles([NextRL|L], N - 1, M);
		true ->
			lists:flatten(L)
	end.

get_roles(M)	->
	RobotId = tk_id:robot_roleID_max(),
	DiffDays = case data_common:get(all_mail_days) of
				   X when is_integer(X) -> X;
				   _ -> 30
			   end,
	LastLogoutTime = util:now() - DiffDays * 86400,
	Sql = io_lib:format("select roleID from gRole where roleID >= ~w and roleId < ~w and lastLogoutTime > ~w ;", [RobotId + 2000 * M, RobotId + 2000*(M + 1),LastLogoutTime]),
	case get_all(Sql) of
		[_|_]=List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

random_get_roles(N,MaxID,RobotId) ->
%% 	RobotId = tk_id:robot_roleID_max(),
%% 	[{_,MaxID}] = ets:lookup(?ETS_ID, roleID),
	DiffDays = case data_common:get(all_mail_days) of
				   X when is_integer(X) -> X;
				   _ -> 30
			   end,
	U = MaxID-20-RobotId,
	if U =< 5-> [MaxID];
	   true -> 
		   MaxID2 = random:uniform(U) + RobotId + random:uniform(5),
		   Sql = io_lib:format("select roleID from gRole where roleID >= ~w and roleID <= ~w and lastLogoutTime > ~w limit ~w;",[MaxID2, MaxID,util:now() - DiffDays * 86400,N]),
		   case get_all(Sql) of
			   [_|_] = List -> [E||[E]<-List];
			   _ -> []
		   end
	end.

get_level_role_id_list(L, H) ->
    case data_setting:get(is_release) of
        false ->
            Sql = io_lib:format("select roleID from gRole where level >= ~w and level <= ~w;", [L, H]),
            case get_all(Sql) of
                [_|_] = List ->
                    [E||[E]<-List];
                _ ->
                    []
            end;
        true ->
            Sql = io_lib:format("select roleID from gRole where level >= ~w and level <= ~w and roleID >= ~w", [L, H, tk_id:robot_roleID_max()]),
            case get_all(Sql) of
                [_|_] = List ->
                    [E||[E]<-List];
                _ ->
                    []
            end
    end.

%% get_type_roles(Type) ->
%% 	case get_row("select max(roleID) from gRole;") of
%% 		[MaxId] when is_integer(MaxId) ->
%% 			get_type_roles(Type,[], MaxId div 2000 + 2, MaxId div 2000 + 2);
%% 		_ ->
%% 			[]
%% 	end.
%% 
%% get_type_roles(Type,L, N, M) ->
%% 	if N > 0 ->
%% 			NextRL=get_type_roles(Type,M - N),
%% 			get_type_roles(Type,[NextRL|L], N - 1, M);
%% 		true ->
%% 			lists:flatten(L)
%% 	end.
%% 
%% get_type_roles(Type,M)	->
%% 	RobotId = tk_id:robot_roleID_max(),
%% 	Sql = io_lib:format("select roleID from gRole where srcType = ~w and roleID >= ~w and roleId < ~w;", [Type,RobotId + 2000 * M, RobotId + 2000*(M + 1)]),
%% 	case get_all(Sql) of
%% 		[_|_]=List ->
%% 			[E||[E]<-List];
%% 		_ ->
%% 			[]
%% 	end.

get_type_level_and_vip_roles(LevelLow,LevelHigh,VipLow,VipHigh,Type,OffTime) ->
    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and level < ~w and vipLevel >= ~w and vipLevel < ~w and lastLogoutTime <= ~w and lastLogoutTime > 0 and srcType = ~w;",
                           [tk_id:robot_roleID_max(),LevelLow,LevelHigh,VipLow,VipHigh,OffTime,Type]),
    case get_all(Sql) of
        [_|_] = List ->
            [E||[E]<-List];
        _ ->
            []
    end.

%% _OffL 暂时没用上
get_level_offtime_roles(L,H,OffL,OffH) ->
	Sql = io_lib:format("select max(roleID) from gRole where level >= ~w and level < ~w and lastLogoutTime <= ~w and lastLogoutTime > ~w ;", [L,H,OffH,OffL]),
	case get_row(Sql) of
		[MaxID] when is_integer(MaxID) ->
			get_level_offtime_roles([],MaxID div 2000 + 2, MaxID div 2000 + 2, L, H,OffL,OffH);
		_ ->
			[]
	end.
get_level_offtime_roles(L,N,M,LL,LH,OffL,OffH) ->
	if N > 0 ->
		   NextRL = get_level_offtime_roles(M-N,LL,LH,OffL,OffH),
		   get_level_offtime_roles([NextRL|L],N-1,M,LL,LH,OffL,OffH);
	   true ->
		   lists:flatten(L)
	end.
get_level_offtime_roles(M,LL,LH,OffL,OffH)->
	RobotID = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID >=~w and roleID < ~w and level >= ~w and level < ~w and lastLogoutTime <= ~w and lastLogoutTime > ~w ;", [RobotID + 2000 * M, RobotID + 2000 * (M+1), LL, LH,OffH,OffL]),
	case get_all(Sql) of
		[_|_] = List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_level_roles(L,H) ->
	Sql = io_lib:format("select max(roleID) from gRole where level >= ~w and level < ~w;", [L,H]),
	case get_row(Sql) of
		[MaxID] when is_integer(MaxID) ->
			get_level_roles([],MaxID div 2000 + 2, MaxID div 2000 + 2, L, H);
		_ ->
			[]
	end.

get_level_roles(L,N,M,LL,LH) ->
	if N > 0 ->
		   NextRL = get_level_roles(M-N,LL,LH),
		   get_level_roles([NextRL|L],N-1,M,LL,LH);
	   true ->
		   lists:flatten(L)
	end.

get_level_roles(M,LL,LH)->
	RobotID = tk_id:robot_roleID_max(),
			DiffDays = case data_common:get(all_mail_days) of
				   X when is_integer(X) -> X;
				   _ -> 30
			   end,
	LastLogoutTime = util:now() - DiffDays * 86400,
	Sql = io_lib:format("select roleID from gRole where roleID >=~w and roleID < ~w and level >= ~w and level < ~w and lastLogoutTime > ~w;", [RobotID + 2000 * M, RobotID + 2000 * (M+1), LL, LH,LastLogoutTime]),
	case get_all(Sql) of
		[_|_] = List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_level_and_vip_roles(LevelLow,LevelHigh,VipLow,VipHigh) ->
			DiffDays = case data_common:get(all_mail_days) of
				   X when is_integer(X) -> X;
				   _ -> 30
			   end,
	LastLogoutTime = util:now() - DiffDays * 86400,
    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and level < ~w and vipLevel >= ~w and vipLevel < ~w and lastLogoutTime > ~w;",
                           [tk_id:robot_roleID_max(),LevelLow,LevelHigh,VipLow,VipHigh,LastLogoutTime]),
    case get_all(Sql) of
        [_|_] = List ->
            [E||[E]<-List];
        _ ->
            []
    end.

%% _OffL 暂时没用上
get_vip_offtime_roles(L,H,_OffL,OffH) ->
	Sql = io_lib:format("select max(roleID) from gRole where vipLevel >= ~w and vipLevel < ~w and lastLogoutTime <= ~w  and lastLogoutTime > 0;", [L,H,OffH]),
	case get_row(Sql) of
		[MaxID] when is_integer(MaxID) ->
			get_vip_offtime_roles([],MaxID div 2000 + 2, MaxID div 2000 + 2, L, H,_OffL,OffH);
		_ ->
			[]
	end.
get_vip_offtime_roles(L,N,M,LL,LH,_OffL,OffH) ->
	if N > 0 ->
		   NextRL = get_vip_offtime_roles(M-N,LL,LH,_OffL,OffH),
		   get_vip_offtime_roles([NextRL|L],N-1,M,LL,LH,_OffL,OffH);
	   true ->
		   lists:flatten(L)
	end.
get_vip_offtime_roles(M,LL,LH,_OffL,OffH)->
	RobotID = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID >=~w and roleID < ~w and vipLevel >= ~w and vipLevel < ~w and  lastLogoutTime <= ~w  and lastLogoutTime > 0;", [RobotID + 2000 * M, RobotID + 2000 * (M+1), LL, LH, OffH]),
	case get_all(Sql) of
		[_|_] = List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_vip_roles(L,H) ->
	Sql = io_lib:format("select max(roleID) from gRole where vipLevel >= ~w and vipLevel < ~w;", [L,H]),
	case get_row(Sql) of
		[MaxID] when is_integer(MaxID) ->
			get_vip_roles([],MaxID div 2000 + 2, MaxID div 2000 + 2, L, H);
		_ ->
			[]
	end.

get_vip_roles(L,N,M,LL,LH) ->
	if N > 0 ->
		   NextRL = get_vip_roles(M-N,LL,LH),
		   get_vip_roles([NextRL|L],N-1,M,LL,LH);
	   true ->
		   lists:flatten(L)
	end.

get_vip_roles(M,LL,LH)->
			DiffDays = case data_common:get(all_mail_days) of
				   X when is_integer(X) -> X;
				   _ -> 30
			   end,
	LastLogoutTime = util:now() - DiffDays * 86400,
	RobotID = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID >=~w and roleID < ~w and vipLevel >= ~w and vipLevel < ~w and lastLogoutTime > ~w;", [RobotID + 2000 * M, RobotID + 2000 * (M+1), LL, LH,LastLogoutTime]),
	case get_all(Sql) of
		[_|_] = List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

%% _OfftimeLow 暂时没用上
get_level_and_vip_and_offtime_roles(LevelLow,LevelHigh,VipLow,VipHigh,_OfftimeLow,OfftimeHigh) ->
	case {LevelLow,LevelHigh,VipLow,VipHigh,_OfftimeLow,OfftimeHigh} of
		{LevelLow,LevelHigh,VipLow,VipHigh,_,OfftimeHigh}->
			    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and level < ~w and vipLevel >= ~w and vipLevel < ~w and lastLogoutTime <= ~w and lastLogoutTime > 0;",
                           [tk_id:robot_roleID_max(),LevelLow,LevelHigh,VipLow,VipHigh,OfftimeHigh]);
		{LevelLow,LevelHigh,VipLow,0,_,OfftimeHigh}->
			    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and level < ~w and vipLevel >= ~w and lastLogoutTime <= ~w and lastLogoutTime > 0;",
                           [tk_id:robot_roleID_max(),LevelLow,LevelHigh,VipLow,OfftimeHigh]);
		{LevelLow,0,VipLow,VipHigh,_,OfftimeHigh}->
			    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and vipLevel >= ~w and vipLevel < ~w and lastLogoutTime <= ~w and lastLogoutTime > 0;",
                           [tk_id:robot_roleID_max(),LevelLow,VipLow,VipHigh,OfftimeHigh]);
		{LevelLow,0,VipLow,0,_,OfftimeHigh}->
			    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and vipLevel >= ~w and lastLogoutTime <= ~w;",
                           [tk_id:robot_roleID_max(),LevelLow,VipLow,OfftimeHigh])
	end,
    case get_all(Sql) of
        [_|_] = List ->
            [E||[E]<-List];
        _ ->
            []
    end.

get_roleIDList() ->
	RobotId = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select roleID from gRole where roleID > ~w limit 2000;", [RobotId]),
	case get_all(Sql) of
		[_|_]=List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

% 获得一个列表用于添加好友的全集，其中不包括离线超过15天的人
get_roleIDList_noRobot(Day) ->
	RobotId = tk_id:robot_roleID_max(),
    TimeLine = util:now() - (Day * 3600 * 24),
    ?INFO("get_roleIDList_noRobot get_roleIDList_noRobot:~w",[TimeLine]),
	Sql = io_lib:format("select roleID,level from gRole where roleID >= ~w and (lastLogoutTime > ~w or lastLogoutTime = 0) ", [RobotId,TimeLine]),
	case get_rows(Sql) of
		List when erlang:is_list(List) ->
			[{E1,E2}||[E1,E2]<-List];
		_ ->
			[]
	end.

get_role_max_level()->
	RobotID = tk_id:robot_roleID_max(),
	Sql = io_lib:format("select max(level) from gRole where roleID > ~w",[RobotID]),
	case get_row(Sql) of
		[X] when is_integer(X) ->
			X;
		_ ->
			10
	end.

get_roleIDList_Robot() ->
    RobotId = tk_id:robot_roleID_max(),
    Sql = io_lib:format("select roleID from gRole where roleID <= ~w", [RobotId]),
    case get_rows(Sql) of
        List when erlang:is_list(List) ->
            [E||[E]<-List];
        _ ->
            []
    end.

get_gold(RoleID) ->
	Sql = io_lib:format("select gold,goldBonus from gRole where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[Gold, GoldBonus] ->
			{Gold, GoldBonus};
		_ ->
			{0,0}
	end.

get_offlineDeductGold(RoleID) ->
	Sql = io_lib:format("select deductGold from gOfflineDeductGold where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[DGold] ->
			DGold;
		_ ->
			0
	end.

set_offlineDeductGold(RoleID, 0) ->
	Sql = io_lib:format("delete from gOfflineDeductGold where roleID=~w;",[RoleID]),
	sql_execute_with_log(Sql);	
set_offlineDeductGold(RoleID, DeductGold)->
	Sql = io_lib:format("replace into gOfflineDeductGold values(~w,~w);",[RoleID, DeductGold]),
	sql_execute_with_log(Sql).

%% 离线充值记录
get_offlinePayLog(RoleID) ->
	Sql = io_lib:format("select payItemID,receipt,receiptMd5,SrcType from gOfflinePayLog where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] =List ->
			List;
		_ ->
			[]
	end.

%% APP充值失败的订单记录
set_failedPayLog(RoleID,Receipt) ->
	Sql = io_lib:format("insert into gFailedPayLog values(null,~w,'~s',~w,'~s');",[RoleID,Receipt,1,datetime(erlang:localtime())]),
	sql_execute_with_log(Sql).

clear_offlinePayLog(RoleID) ->
	Sql = io_lib:format("delete from gOfflinePayLog where roleID=~w;",[RoleID]),
	sql_execute_with_log(Sql).

%% 充值重复验证表
check_pay_receipt_duplicate(Md5)->
	Sql = io_lib:format("select count(*) from gPay where receiptMd5='~s';",[Md5]),
	case get_row(Sql) of
		[0] ->
			true;
		_ ->
			false
	end.

add_pay_receipt(Md5,RoleID,Receipt,SrcType, PayGold) ->
	Sql = io_lib:format("insert into gPay values('~s',~w,'~s',~w,'~s',~w);",[Md5,RoleID,Receipt,SrcType,datetime(erlang:localtime()), PayGold]),
	sql_execute_with_log(Sql).
	
get_role_accid(RoleID) ->
	Sql = io_lib:format("select accid from gRole where roleID = ~w;", [RoleID]),
	case get_row(Sql) of
		[X] ->
			X rem ?AccidBase;
		_ ->
			0
	end.

get_role_accid_and_type(RoleID) ->
	Sql = io_lib:format("select accid,srcType from gRole where roleID = ~w;", [RoleID]),
	case get_row(Sql) of
		[X,SrcType] ->
			{X rem ?AccidBase,SrcType};
		_ ->
			{0,0}
	end.

get_role_name(RoleID) ->
    Sql = io_lib:format("select roleName from gRole where roleID = ~w;", [RoleID]),
    case get_row(Sql) of
        [Name] ->
            Name;
        _ ->
            "" 
    end.
	
add_offlinePayLog(RoleID, PayItemID, Receipt, Md5, SrcType) ->
	Sql = io_lib:format("insert into gOfflinePayLog values(~w,~w,~s,'~s',~w);",[RoleID,PayItemID,quote(Receipt), Md5,SrcType]),
	sql_execute_with_log(Sql).

add_offlinePayAmountLog(RoleID, Amount, Receipt, Md5, SrcType) ->
    Sql = io_lib:format("insert into gOfflinePayAmountLog values(~w,~w,~s,'~s',~w);",[RoleID,Amount,quote(Receipt), Md5,SrcType]),
    sql_execute_with_log(Sql).

get_offlinePayAmountLog(RoleID) ->
    Sql = io_lib:format("select amount,receipt,receiptMd5,SrcType from gOfflinePayAmountLog where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_] =List ->
            List;
        _ ->
            []
    end.

clear_offlinePayAmountLog(RoleID) ->
    Sql = io_lib:format("delete from gOfflinePayAmountLog where roleID=~w;",[RoleID]),
    sql_execute_with_log(Sql).


%% add_offlinePayMonthVIPLog(RoleID, PayItemID, Receipt, Md5, SrcType) ->
%% 	Sql = io_lib:format("insert into gOfflinePayMonthVIPLog values(~w,~w,~w,~s,'~s');",[RoleID,PayItemID,SrcType,quote(Receipt), Md5]),
%% 	sql_execute_with_log(Sql).
%% 
%% get_offlinePayMonthVIPLog(RoleID) ->
%% 	Sql = io_lib:format("select payItemID,receipt,receiptMd5,SrcType from gOfflinePayMonthVIPLog where roleID=~w;",[RoleID]),
%% 	case get_all(Sql) of
%% 		[_|_] = List ->
%% 			List;
%% 		_ ->
%% 			[]
%% 	end.
%% 
%% clear_offlinePayMonthVIPLog(RoleID) ->
%% 	Sql = io_lib:format("delete from gOfflinePayMonthVIPLog where roleID = ~w",[RoleID]),
%% 	sql_execute_with_log(Sql).


get_role_lastLogoutTime(RoleID)->
	Sql = io_lib:format("select lastLogoutTime,lastLoginTime from gRole where roleID = ~w", [RoleID]),
	case get_row(Sql) of
		[LastLogoutTime,0] ->
			{LastLogoutTime,LastLogoutTime};
		[LastLogoutTime,LastLoginTime] ->
			{LastLogoutTime,LastLoginTime};
		_ ->
			{util:now(),util:now()}
	end.

%% 日志表按月分开存储，只取本月的记录
get_role_lastLoginTime(RoleID)->
	TableName = get_log_table_name(logLogin),
	Sql = io_lib:format("select datetime from ~s where RoleID = ~w order by datetime desc limit 1", [TableName, RoleID]),
	case get_row(Sql) of
		[LoginTime] ->
			util:toUnixTime(LoginTime);
		_ ->
			util:now()
	end.
	
get_roleInfo(RoleID) ->
	case get_row(get_role_info, [RoleID]) of
		[Vaccid,VroleName,VisMale,Vlevel,Vexp0,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,Vunioncoin,VprofoundCrystal,VvipLevel,VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime
		 ,VFamilyID,VLastJoinFamily,Head,VPayExtReward,ExtRdActTime,Location,IsFailed,DeviceID,SrcType,Tasklevel,PlaneLevel,TeamID,Honor,PvpPoint,CarlosWinTime,CarlosEqualTime,CarlosLoseTime
         ,CarlosSeason,CarlosPreWinTime,CarlosPreEqualTime,CarlosPreLoseTime,CarlosPreSeason ,HomeResource,FirstPayStatus,Ticket,LaputaStone,Transmigration,SGoldTotalPaid,SVipLevel]->
			Vexp = repaire_exp(Vlevel,Vexp0),
            NewUnioncoin = check_unioncoin_for_update(RoleID,Vunioncoin,Vlevel),
            NewTasklevel = check_tasklevel_for_update(Tasklevel,Vlevel),
			#role{roleID=RoleID,description="",familyID=VFamilyID,lastJoinFamily=VLastJoinFamily,accid=Vaccid,roleName=VroleName,
				  isMale=int2bool(VisMale),level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,gold=Vgold,goldBonus=VgoldBonus,
				  goldUsed=VgoldUsed,unioncoin=NewUnioncoin,profoundCrystal=VprofoundCrystal,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,fightPower=VfightPower,
				  lastLogoutTime=VlastLogoutTime,head=Head,payExtReward=role_lib:update_pay_ext_reward(VPayExtReward,ExtRdActTime),sGoldTotalPaid=SGoldTotalPaid,svipLevel=SVipLevel,
				  extRdActTime=ExtRdActTime,location=Location,isFailed=int2bool(IsFailed),deviceID=DeviceID,srcType=SrcType,tasklevel=NewTasklevel,plane_level=PlaneLevel,teamId=TeamID
                 ,honor=Honor,pvppoint=PvpPoint,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,carlosseasonid=CarlosSeason
                 ,carloslastwintime=CarlosPreWinTime,carloslastequaltime=CarlosPreEqualTime,carloslastlosetime=CarlosPreLoseTime,carloslastseasonid=CarlosPreSeason
                 ,home_resource=HomeResource,firstPayStatus=FirstPayStatus,ticket=Ticket,laputastone=LaputaStone,transmigration=Transmigration};
		_ ->
			undefined
	end.

repaire_exp(Vlevel,Vexp0) ->
	NowExp = data_role_level:get(Vlevel),
	if Vexp0 < NowExp -> data_role_level:get(Vlevel+1) -1;
	   true -> Vexp0
	end.

check_roleCreated(Accid) ->
	case get_row(check_role_created, [Accid]) of
		[] ->
			false;
		[RoleID] ->
			{true,RoleID}
	end.
	
create_roleInfo(RoleInfo) ->
	#role{roleID=RoleID,accid=Vaccid,roleName=VroleName,isMale=VisMale,level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,gold=Vgold,goldBonus=VgoldBonus,
          goldUsed=VgoldUsed,unioncoin=Vunioncoin,profoundCrystal=VprofoundCrystal,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,fightPower=VfightPower,lastLogoutTime=VlastLogoutTime
         ,familyID=VFamilyID,lastJoinFamily=VLastJoinFamily,head=Head,payExtReward=VPayExtReward,extRdActTime=ExtRdActTime,sGoldTotalPaid=SGoldTotalPaid,svipLevel=SVipLevel
		 ,location=Location,isFailed=IsFailed,deviceID=DeviceID,srcType=SrcType,lastLoginTime=LastLoginTime,tasklevel=Tasklevel,plane_level=PlaneLevel,teamId=TeamID
         ,honor=Honor,pvppoint=PvpPoint,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,carlosseasonid=CarlosSeason,carloslastwintime=CarlosPreWinTime,carloslastequaltime=CarlosPreEqualTime,carloslastlosetime=CarlosPreLoseTime,carloslastseasonid=CarlosPreSeasonID
         ,home_resource=HomeResource,firstPayStatus=FirstPayStatus,ticket=Ticket,laputastone=LaputaStone,transmigration=Transmigration}=RoleInfo,
	Sql = io_lib:format("insert into gRole values(~w,~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w);",
						[RoleID,Vaccid,quote(VroleName),bool2int(VisMale),Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,Vunioncoin,VprofoundCrystal,
                         VvipLevel,VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime,VFamilyID,VLastJoinFamily,Head,VPayExtReward,
                         ExtRdActTime,quote(Location),bool2int(IsFailed),quote(DeviceID),SrcType,LastLoginTime,Tasklevel,PlaneLevel,TeamID,
                         Honor,PvpPoint,CarlosWinTime,CarlosEqualTime,CarlosLoseTime,CarlosSeason,CarlosPreWinTime,CarlosPreEqualTime,CarlosPreLoseTime,CarlosPreSeasonID,
                         HomeResource,FirstPayStatus,Ticket,LaputaStone,Transmigration,SGoldTotalPaid,SVipLevel]),
	sql_execute_with_log(Sql).

update_roleInfo(RoleInfo) ->
	#role{roleID=RoleID,level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,gold=Vgold,goldBonus=VgoldBonus,goldUsed=VgoldUsed,unioncoin=Vunioncoin,sGoldTotalPaid=SGoldTotalPaid,svipLevel=SVipLevel,
          profoundCrystal=VprofoundCrystal,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,fightPower=VfightPower,lastLogoutTime=VlastLogoutTime,lastLoginTime=VLastLoginTime,
          familyID=VFamilyID,lastJoinFamily=VLastJoinFamily,head=Head,payExtReward=VPayExtReward,extRdActTime=ExtRdActTime,location=Location,isFailed=IsFailed,deviceID=DeviceID,srcType=SrcType,tasklevel=VTasklevel,plane_level=PlaneLevel,teamId=TeamID,
          honor=Honor,pvppoint=VPvpPoint,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,carlosseasonid=CarlosSeason,carloslastwintime=CarlosPreWinTime,carloslastequaltime=CarlosPreEqualTime,carloslastlosetime=CarlosPreLoseTime,carloslastseasonid=CarlosPreSeasonID
         ,home_resource=HomeResource,firstPayStatus=FirstPayStatus,ticket=Ticket,laputastone=LaputaStone,transmigration=Transmigration}=RoleInfo,
    Sql = io_lib:format("update gRole set level= ~w,exp= ~w,coin= ~w,reputation= ~w,gold= ~w,goldBonus= ~w,goldUsed= ~w,unioncoin= ~w,profoundCrystal=~w,vipLevel= ~w,goldTotalPaid= ~w,title= ~w,fightPower= ~w,lastLogoutTime= ~w,FamilyID = ~w,lastJoinFamily = ~w,head=~w,payExtReward = ~w,extRdActTime = ~w,location = ~s,isFailed = ~w,devid=~s,srcType=~w,lastLoginTime=~w,tasklevel=~w,plane_level=~w,teamid=~w
                        ,honor=~w,pvppoint=~w,carloswintime=~w,carlosequaltime=~w,carloslosetime=~w,carlosseason=~w,carlosprewintime=~w,carlospreequaltime=~w,carlosprelosetime=~w,carlospreseason=~w,home_resource=~w,firtPayStatus=~w,ticket=~w,laputastone=~w,transmigration=~w,sGoldTotalPaid=~w,svipLevel=~w where roleID= ~w",
                        [Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,Vunioncoin,VprofoundCrystal,VvipLevel,VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime,VFamilyID,VLastJoinFamily,Head,VPayExtReward,ExtRdActTime,quote(Location),bool2int(IsFailed),quote(DeviceID),SrcType,VLastLoginTime,VTasklevel,PlaneLevel,TeamID
                        ,Honor,VPvpPoint,CarlosWinTime,CarlosEqualTime,CarlosLoseTime,CarlosSeason,CarlosPreWinTime,CarlosPreEqualTime,CarlosPreLoseTime,CarlosPreSeasonID,HomeResource,FirstPayStatus,Ticket,LaputaStone,Transmigration,SGoldTotalPaid,SVipLevel,RoleID]),
	sql_execute_with_log(Sql).

update_role_family_id(RoleID, FamilyID) ->
    Sql = io_lib:format("update gRole set familyID = ~w where roleID = ~w", [FamilyID, RoleID]),
    sql_execute_with_log(Sql).

%% lastJoinFamily实际上记录的是最后主动离开工会的时间
update_role_family_id(RoleID, FamilyID, LastJoinFamily) ->
    Sql = io_lib:format("update gRole set familyID = ~w, lastJoinFamily = ~w where roleID = ~w", [FamilyID, LastJoinFamily, RoleID]),
    sql_execute_with_log(Sql).

update_role_carlos_info(RoleID,WinTime,EqualTime,LoseTime,SeasonID,LastWinTime,LastEqualTime,LastLoseTime,LastSeasonID)->
	Sql = io_lib:format("update gRole set carloswintime = ~w, carlosequaltime = ~w,carloslosetime = ~w,carlosseason= ~w,carlosprewintime=~w,carlospreequaltime=~w,carlosprelosetime=~w,carlospreseason=~w where roleID = ~w",[WinTime,EqualTime,LoseTime,SeasonID,LastWinTime,LastEqualTime,LastLoseTime,LastSeasonID,RoleID]),
	sql_execute_with_log(Sql).

update_role_vip(RoleID,Vip)->
	Sql = io_lib:format("update gRole set vipLevel = ~w where roleID=~w",[Vip,RoleID]),
	sql_execute_with_log(Sql).
		
reduce_role_unioncoin(RoleID, ReduceRate) ->
    Sql = io_lib:format("select unioncoin from gRole where roleID=~w;",[RoleID]),
    case get_row(Sql) of
        [OldUnioncoin] when is_number(OldUnioncoin)->
            NewUnioncoin = trunc(OldUnioncoin*ReduceRate),
            Sql2 = io_lib:format("update gRole set unioncoin = ~w where roleID = ~w", [NewUnioncoin, RoleID]),
            sql_execute_with_log(Sql2);
        _->
            ignore
    end.

test(RoleID, T) ->
	Sql = io_lib:format("update gRoleExtra set randomShopList=~s where roleID=~w;",[to_bin(T), RoleID]),
	sql_execute_with_log(Sql).

test2(RoleID) ->
	Sql = io_lib:format("select randomShopList from gRoleExtra where roleID=~w;",[RoleID]),
	get_row(Sql).

get_roleExtra(RoleID) ->
    case get_row(get_role_extra, [RoleID]) of
        [VbattleProgress,VbattleProgressHard,VbattleProgressFastHard,VbattleProgressTransmigration,Venergy,VenergyBuyTimes,VChallengeGodEnergy, VChallengeGodBuyTimes,{date,VlastChallengeGodDate},VRefreshLieuTimes
        ,VAlreadyPayRefreshLieuTimes,VdscvBuyTimes,VpvpBuyTimes,_VplunderBuyTimes,VcoinBuyTimes,VfireTimes,{date,VlastBuyTimesRefreshDate},VlastEnergyTime,VdiscoveryTimes
        ,VlastDscvTime,VdscvCount,VpvpTimes,_VplunderTimes,VweiboCount,VnextWeiboCountRefreshSec,VlastWeiXinShareSec,VlastPvpTime,_VlastPlunderTime,VencounterList0
        ,{date,VlastTitleRewardDate},VlastDrawTitle,{date,VlastLoggedLoginDate},VlastDrawLoginRewardDays,VloginDays,VlastDrawLevelUpLevel,VrandomShopList0,VleftChgNameTimes
        ,VtalentstudyBuyTimes,VlastPayTime,VMagicBookState,VteamPkTimes,VteamPkBuyTimes,VlastTeamPkTime,VSignDayCount,VIsSignRewardGet,VLastSignTime,VIsAccSignRewardGet,VBattleBossRewardInfo0
        ,VMainGerTypeID,PlaneAiFlag,VMainTask,VEnergyPac,VTrainerProf,SignSec] ->
			VencounterList = to_term(VencounterList0),
			VrandomShopList=to_term(VrandomShopList0),
			%%此处需要通过-1这个特殊的值来判断玩家是否已经初始过宝箱信息,故做了特殊判断
			VBattleBossRewardInfo1=case VBattleBossRewardInfo0 of <<"-1">>->-1;_->to_term(VBattleBossRewardInfo0)end,
			VBattleBossRewardInfo = role_battle:transform_persistform2termform(VBattleBossRewardInfo1,[]),
            VitemUseList = get_role_item_use_list(RoleID),
            MainTask = to_term(VMainTask);
		Err ->
			?DEBUG("get_role_extra:~p~n",[Err]),
			VbattleProgress=role_battle:get_first_dungeonID(),
			VbattleProgressHard=role_battle:get_first_hard_dungeonID(),
			VbattleProgressFastHard = role_battle:get_first_fast_hard_dungeonID(),
            VbattleProgressTransmigration = role_battle:get_first_transmigration_dungeonID(),
			Venergy=role_lib:get_max_energy(0),
			VenergyBuyTimes=0,
			VChallengeGodEnergy=data_common:get(challengeGodTimes),
			VChallengeGodBuyTimes=0,
			VlastChallengeGodDate={1989,7,17},
			VRefreshLieuTimes=data_lieu_clo_setting:get(daily_free_refresh),
			VAlreadyPayRefreshLieuTimes=0,
			VdscvBuyTimes=0,
			VpvpBuyTimes=0,
			VcoinBuyTimes=0,
			VfireTimes=0,
			VlastBuyTimesRefreshDate={1989,7,17},
			VlastEnergyTime=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VdiscoveryTimes=0,
			VlastDscvTime=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VdscvCount=0,
			VpvpTimes=data_common:get(max_pvp_times),
			VweiboCount=0,
			VnextWeiboCountRefreshSec=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VlastWeiXinShareSec=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VlastPvpTime=util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
			VencounterList=[],
			VlastTitleRewardDate={2013,1,1},
			VlastDrawTitle=0,
			VlastLoggedLoginDate={2013,1,1},
			VlastDrawLoginRewardDays=0,
			VloginDays=0,
			VlastDrawLevelUpLevel=0,
			VrandomShopList=[],
            VitemUseList=[],
            VleftChgNameTimes=-1,
            VtalentstudyBuyTimes = 0,
            VlastPayTime = 0,
            VSignDayCount = 0,
            VIsSignRewardGet = 0,
            VLastSignTime = 0,
            VIsAccSignRewardGet = 0, 
            VMagicBookState = [],
            VteamPkTimes = 0,
            VteamPkBuyTimes = 0,
            VBattleBossRewardInfo=-1,
            VlastTeamPkTime = util:datetime_to_seconds({{2013,1,1},{1,1,1}}),
            VMainGerTypeID=0,
            PlaneAiFlag = 0,
            MainTask = 0,
            VEnergyPac = 0,
            VTrainerProf = 0,
            SignSec = 0
	end,
    %% -1 为还没有赋值过的数据
    case VleftChgNameTimes of
        -1 ->
            case data_change_name:get({change_cost, ?CHANGE_SELF_NAME}) of
                undefined ->
                    VleftChgNameTimes2 = 0;
                {FreeTimes,_,_} ->
                    VleftChgNameTimes2 = FreeTimes
            end;
        _ ->
            VleftChgNameTimes2 = VleftChgNameTimes
    end,
    SignSec2 = if SignSec == 0 -> util:now(); true -> SignSec end,
	RoleTimes = #roleTimes{energy=Venergy,
						   challengeGodEnergy=VChallengeGodEnergy,
						   challengeGodBuyTimes=VChallengeGodBuyTimes,
						   lastChallengeGodDate=VlastChallengeGodDate,
						   refreshLieuTimes=VRefreshLieuTimes,
						   alreadyPayRefreshLieuTimes=VAlreadyPayRefreshLieuTimes,
						   energyBuyTimes=VenergyBuyTimes,
						   dscvBuyTimes=VdscvBuyTimes,
						   pvpBuyTimes=VpvpBuyTimes,
						   %plunderBuyTimes=VplunderBuyTimes,
						   coinBuyTimes=VcoinBuyTimes,
						   fireTimes=VfireTimes,
						   lastBuyTimesRefreshDate=VlastBuyTimesRefreshDate,
						   lastEnergyTime=VlastEnergyTime,
						   discoveryTimes=VdiscoveryTimes,
						   lastDscvTime=VlastDscvTime,
						   dscvCount=VdscvCount,
						   pvpTimes=VpvpTimes,
						   %plunderTimes=VplunderTimes,
						   weiboCount=VweiboCount,
						   nextWeiboCountRefreshSec=VnextWeiboCountRefreshSec,
						   lastWeiXinShareSec=VlastWeiXinShareSec,
						   lastPvpTime=VlastPvpTime,
						   %lastPlunderTime=VlastPlunderTime,
                           leftChgNameTimes=VleftChgNameTimes2,
                           talentstudyBuyTimes = VtalentstudyBuyTimes,
                           teamPkTimes = VteamPkTimes,
                           teamPkBuyTimes = VteamPkBuyTimes,
                           lastTeamPkTime = VlastTeamPkTime ,
                           energyPac = VEnergyPac
						  },
    ?INFO("天赋数据库输出  VtalentstudyBuyTimes:~w",[VtalentstudyBuyTimes]),
	DailyInfo = #daily{lastTitleRewardDate     = VlastTitleRewardDate     
					  ,lastDrawTitle           = VlastDrawTitle           
					  ,lastLoggedLoginDate     = VlastLoggedLoginDate     
					  ,lastDrawLoginRewardDays = VlastDrawLoginRewardDays 
					  ,loginDays               = VloginDays               
					  ,lastDrawLevelUpLevel    = VlastDrawLevelUpLevel
					  ,lastPayTime             = VlastPayTime    
					  },	
	VbattleProgressHard2 = 
		if VbattleProgressHard =:= 0 ->
			   role_battle:get_first_hard_dungeonID();
		   true ->
			   VbattleProgressHard
		end,
	VbattleProgressFastHard2 = 
		if VbattleProgressHard =:= 0 ->
			   role_battle:get_first_fast_hard_dungeonID();
		   true ->
			   VbattleProgressFastHard
		end,
    VbattleProgressTransmigration2 = 
        if VbattleProgressTransmigration =:= 0 ->
               role_battle:get_first_transmigration_dungeonID();
           true ->
               VbattleProgressTransmigration
        end,
	SignInfo = #sign_info{sign_time_count=VSignDayCount,is_get_sign_reward=VIsSignRewardGet,last_sign_time=VLastSignTime,is_get_acc_sign_reward=VIsAccSignRewardGet},
    role_data:set_plane_ai_flag_all(PlaneAiFlag),
    {VbattleProgress,VbattleProgressHard2, VbattleProgressFastHard2,VbattleProgressTransmigration2,RoleTimes, VencounterList, DailyInfo, VrandomShopList, VitemUseList, VMagicBookState,SignInfo,VBattleBossRewardInfo,VMainGerTypeID,MainTask,VTrainerProf,SignSec2}.

set_roleExtra(RoleID, VbattleProgress, VbattleProgressHard, VbattleProgressFastHard,BattleProgressTransmigration,RoleTimes, VencounterList, DailyInfo, VrandomShopList, VitemUseList, MagicState,SignInfo,BattleBossRewardInfo1,MainGerTypeID,VTrainerProf,SignSec) ->	
	#roleTimes{energy=Venergy,
			   challengeGodEnergy=VChallengeGodEnergy,
			   challengeGodBuyTimes=VChallengeGodBuyTimes,
			   lastChallengeGodDate=VlastChallengeGodDate,
			   refreshLieuTimes=VRefreshLieuTimes,
			   alreadyPayRefreshLieuTimes=VAlreadyPayRefreshLieuTimes,
			   energyBuyTimes=VenergyBuyTimes,
			   dscvBuyTimes=VdscvBuyTimes,
			   pvpBuyTimes=VpvpBuyTimes,
			   %plunderBuyTimes=VplunderBuyTimes,
			   coinBuyTimes=VcoinBuyTimes,
			   fireTimes=VfireTimes,
			   lastBuyTimesRefreshDate=VlastBuyTimesRefreshDate,
			   lastEnergyTime=VlastEnergyTime,
			   discoveryTimes=VdiscoveryTimes,
			   lastDscvTime=VlastDscvTime,
			   dscvCount=VdscvCount,
			   pvpTimes=VpvpTimes,
			   %plunderTimes=VplunderTimes,
			   weiboCount=VweiboCount,
			   nextWeiboCountRefreshSec=VnextWeiboCountRefreshSec,
			   lastWeiXinShareSec=VlastWeiXinShareSec,
			   lastPvpTime=VlastPvpTime,
			   %lastPlunderTime=VlastPlunderTime,
               leftChgNameTimes=VleftChgNameTimes,
               talentstudyBuyTimes = VtalentstudyBuyTimes,
               teamPkTimes = VteamPkTimes,
               teamPkBuyTimes = VteamPkBuyTimes,
               lastTeamPkTime = VlastTeamPkTime ,
               energyPac = VEnergyPac
			  } = RoleTimes,
	#daily{lastTitleRewardDate     = VlastTitleRewardDate     
		   ,lastDrawTitle           = VlastDrawTitle           
		   ,lastLoggedLoginDate     = VlastLoggedLoginDate     
		   ,lastDrawLoginRewardDays = VlastDrawLoginRewardDays 
		   ,loginDays               = VloginDays               
		   ,lastDrawLevelUpLevel    = VlastDrawLevelUpLevel
		   ,lastPayTime             = VlastPayTime    
		  } = DailyInfo,
	BattleBossRewardInfo = role_battle:transform_termform2persistform(BattleBossRewardInfo1,[]),
    #sign_info{sign_time_count=VSignDayCount,is_get_sign_reward=VIsSignRewardGet,last_sign_time=VLastSignTime,is_get_acc_sign_reward=VIsAccSignRewardGet}=SignInfo,
	PlaneAiFlag = role_data:get_plane_ai_flag(),
	MainTask = role_maintask:get_maintask_persist(),
	% ?ERR("MainTask:~w ~n",[MainTask]),
    Sql=io_lib:format("replace into gRoleExtra values(~w,~w,~w,~w,~w,~w,~w,~w,~w,'~s',~w,~w,~w,~w,~w,~w,~w,'~s',~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,'~s',~w,'~s',~w,~w,~w,~s,~w,~w,~w,'~s',~w,~w,~w,~w,~w,~w,~w,~s,~w,~w,~s,~w,~w,~w);",

					  [RoleID,
					   VbattleProgress,VbattleProgressHard,VbattleProgressFastHard,BattleProgressTransmigration,Venergy,VenergyBuyTimes,VChallengeGodEnergy, VChallengeGodBuyTimes,date(VlastChallengeGodDate),VRefreshLieuTimes,VAlreadyPayRefreshLieuTimes,
					   VdscvBuyTimes,VpvpBuyTimes,0,VcoinBuyTimes,VfireTimes,date(VlastBuyTimesRefreshDate),VlastEnergyTime,VdiscoveryTimes,VlastDscvTime,VdscvCount,VpvpTimes,VlastPvpTime,0,0,VweiboCount,VnextWeiboCountRefreshSec,
					   VlastWeiXinShareSec,to_bin(VencounterList),
					   date(VlastTitleRewardDate),VlastDrawTitle,date(VlastLoggedLoginDate),VlastDrawLoginRewardDays,VloginDays,VlastDrawLevelUpLevel,
					   to_bin(VrandomShopList),VleftChgNameTimes,VtalentstudyBuyTimes,VlastPayTime,MagicState,VteamPkTimes,VteamPkBuyTimes,VlastTeamPkTime,VSignDayCount,VIsSignRewardGet,VLastSignTime,VIsAccSignRewardGet,to_bin(BattleBossRewardInfo)
                      ,MainGerTypeID,PlaneAiFlag,to_bin(MainTask),VEnergyPac,VTrainerProf,SignSec]),
	sql_execute_with_log(Sql),
    set_role_item_use_list(RoleID, VitemUseList).

get_role_item_use_list(RoleID) ->
    Sql = io_lib:format("select itemTypeID,useDate,useTimes from gItemUse where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [#item_use_info{itemTypeID=ItemTypeID,useDate=UseDate,useTimes=UseTimes}||[ItemTypeID,{date, UseDate},UseTimes]<-List];
        _ ->
            []
    end.

set_role_item_use_list(RoleID, ItemUseList) ->
    ArgList = [[RoleID,ItemTypeID,date(UseDate),UseTimes]||#item_use_info{itemTypeID=ItemTypeID,useDate=UseDate,useTimes=UseTimes}<-ItemUseList],
    DeleteSql = io_lib:format("delete from gItemUse where roleID=~w;", [RoleID]),
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql);
       true ->
           InsertSql = make_sql_batch("insert into gItemUse values", "(~w,~w,'~s',~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.
	
get_role_encounterList(RoleID)->
	Sql = io_lib:format("select * from gEncounter where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[] ->
			init_role_encounterList(RoleID),
			Sql = io_lib:format("select * from gEncounter where roleID = ~w",[RoleID]),
			case get_row(Sql) of
				[_|_] ->
					get_role_encounterList(RoleID);
				_ ->
					?ERR("create role encounterList fail with roleID :~w",[RoleID])
			end;
		[RoleID, MonsterRank, EncounterInfo] ->
			{MonsterRank, uncompress_decode(EncounterInfo)};
		X ->
			?ERR("get role encounterList err:~w",[X]),
			{0,[]}
	end.

init_role_encounterList(RoleID)->
	Sql = io_lib:format("insert into gEncounter values (~w,~w,~s)",[RoleID, 0, quote(compress_encode([]))]),
	sql_execute_with_log(Sql).

set_role_encounterList(RoleID, EncounterInfo)->
	{MonsterRank, EncounterList} = EncounterInfo,
	Sql = io_lib:format("replace into gEncounter values (~w,~w,~s)",[RoleID, MonsterRank, quote(compress_encode(EncounterList))]),
	sql_execute_with_log(Sql).


get_shopNumList(RoleID) ->
	Sql = io_lib:format("select shopID,sellID,buyNum from gShopNum where roleID=~w",[RoleID]),
	case get_all(Sql) of
		[_|_] = ShopNumList0 ->
            ShopNumList = lists:foldr(fun([ShopID,SellID,BuyNum],AccList)-> 
                            DataSell = data_sell:get(SellID),
                            case DataSell of
                                #data_sell{} ->
                                    {RefreshType,MaxBuyNum} = case DataSell#data_sell.maxBuyNum of
                                                    -1 ->
                                                        {0,0};
                                                    Num when erlang:is_integer(Num) andalso Num >= 0 ->
                                                        {1,Num};
                                                    {week,WeekDay,Num} ->
                                                        {10+WeekDay,Num}
                                                  end,
                                    [#p_shop_num{shopID=ShopID,sellID=SellID,buyNum=BuyNum
                                              ,buyMax=MaxBuyNum,refresh_type=RefreshType}|AccList];
                                _ ->
                                    ?WARNING("SellID(~w) config is wrong. ~w",[SellID,DataSell]),
                                    AccList
                            end
                        end, [], ShopNumList0);
%% 			ShopNumList = [#p_shop_num{shopID=ShopID,sellID=SellID,buyNum=BuyNum}||[ShopID,SellID,BuyNum]<-ShopNumList0];
		_ ->
			ShopNumList=[]
	end,
	ShopNumList.

set_shopNumList(RoleID,ShopNumList) ->
	sql_execute_with_log(io_lib:format("delete from gShopNum where roleID=~w;",[RoleID])),
	ArgList = [[RoleID, ShopID,SellID,BuyNum] || #p_shop_num{shopID=ShopID,sellID=SellID,buyNum=BuyNum} <- ShopNumList],
	if ShopNumList == [] ->
		   {ok,0};
	   true ->
		   Sql = make_sql_batch("insert into gShopNum values", "(~w,~w,~w,~w)", ArgList),
		   sql_execute_with_log(Sql)
	end.



get_cardInfo(RoleID) ->
	Sql = io_lib:format("select openedCardList,cardList,drawCount,activityID from gCard where roleID=~w",[RoleID]),
	case get_row(Sql) of
		[OpenCardList0,CardList0,DrawCount, ActivityID] ->
			#cardInfo{cardList=to_term(CardList0),drawCount=DrawCount,openedCardList=to_term(OpenCardList0), activityID=ActivityID};
		_ ->
			#cardInfo{cardList=[],drawCount=0,openedCardList=[],activityID=0}
	end.

set_cardInfo(RoleID,CardInfo) ->
	Sql = io_lib:format("replace into gCard values(~w,~s,~s,~w,~w);",
						[RoleID,
						 to_bin(CardInfo#cardInfo.openedCardList),
						 to_bin(CardInfo#cardInfo.cardList),
						 CardInfo#cardInfo.drawCount,
						CardInfo#cardInfo.activityID
						]),
	sql_execute_with_log(Sql).


get_treasure_house_info(RoleID) ->
	Sql = io_lib:format("select value_info, card_list, free_count, buy_count, free_times, mark,baseBoxGetProcess, isGetRankReward,lastExploreDate,activityID from gTreasureHouse where roleID = ~w", [RoleID]),
	case get_row(Sql) of
		[ValueInfo, CardList0, FreeCount, BuyCount, FreeTimes, Mark, BaseBoxGetProcess, IsGetRankReward,{date,LastExploreDate},ActivityID] ->
			FreeTimes2 = 
				case LastExploreDate == erlang:date() of
					true ->
						FreeTimes;
					_ ->
						data_treasure_box:get(treasure_house_free_times)
				end,
			FreeTimes3 = 
				case FreeCount =< 5 andalso erlang:date() == {2014,1,27} of
					true ->
					   data_treasure_box:get(treasure_house_free_times);
				   _ ->
					   FreeTimes2
				end,
			#treaHouseInfo{value_info=ValueInfo, card_list=to_term(CardList0), free_count=FreeCount,
						   buy_count=BuyCount, free_times=FreeTimes3, mark=Mark, baseBoxGetList=to_term(BaseBoxGetProcess),
						   isGetRankReward=IsGetRankReward,activityID=ActivityID};
		_ ->
			FreeTimes = data_treasure_box:get(treasure_house_free_times),
			#treaHouseInfo{value_info=1, card_list=[],free_count=0,buy_count=0, free_times=FreeTimes,
						   mark=0, baseBoxGetList=[],isGetRankReward=0,activityID=0}
	end.

set_treasure_house_info(RoleID, TreaHouseInfo)->
	Sql = io_lib:format("replace into gTreasureHouse values (~w,~w, ~s,~w,~w,~w,~w,~s,~w,'~s',~w);",
						[RoleID, TreaHouseInfo#treaHouseInfo.value_info,to_bin(TreaHouseInfo#treaHouseInfo.card_list),
						 TreaHouseInfo#treaHouseInfo.free_count, TreaHouseInfo#treaHouseInfo.buy_count,
						 TreaHouseInfo#treaHouseInfo.free_times, TreaHouseInfo#treaHouseInfo.mark,
						 to_bin(TreaHouseInfo#treaHouseInfo.baseBoxGetList),TreaHouseInfo#treaHouseInfo.isGetRankReward,
						 date(erlang:date()),TreaHouseInfo#treaHouseInfo.activityID]),
	sql_execute_with_log(Sql).

erase_all_treaHouse_info()->
	ok.
%% 	Sql=io_lib:format("delete from gTreasureHouse;", []),
%% 	sql_execute_with_log(Sql).
get_hronInfo(RoleID) ->
	Sql = io_lib:format("select * from gHron where roleID=~w", [RoleID]),
	case get_row(Sql) of
		[_, {date,Vdate},Vstar,VcurDungeonNum,VattackAdd,VhpAdd,Vmorale,VdungeonIDList,VbestScore,VchallengeTimes,VisHaveSuccReward,VlastFightResult,IsSelect,BHistory] ->
			HronInfo = #hronInfo{
					  date          = Vdate          
					  ,star          = Vstar          
					  ,curDungeonNum = VcurDungeonNum 
					  ,attackAdd     = VattackAdd     
					  ,hpAdd         = VhpAdd         
					  ,morale        = Vmorale        
					  ,dungeonIDList = to_term(VdungeonIDList) 
					  ,bestScore	  = VbestScore	  
					  ,challengeTimes= VchallengeTimes
                      ,isHaveSuccReward=VisHaveSuccReward
                      ,lastFightResult=VlastFightResult
					  ,isSelect=IsSelect
					 },
			History = to_term(BHistory),
			HronHistory = [#hron_history_info{difficultytype=Type,bestscore=Score}||{Type,Score}<-History],
			{HronInfo,HronHistory};
		_ ->
			{[],[]}
	end.

set_hronInfo(RoleID, #hronInfo{}=HronInfo,HronHistory) ->
	#hronInfo{
			  date          = Vdate          
			  ,star          = Vstar          
			  ,curDungeonNum = VcurDungeonNum 
			  ,attackAdd     = VattackAdd     
			  ,hpAdd         = VhpAdd         
			  ,morale        = Vmorale        
			  ,dungeonIDList = VdungeonIDList 
			  ,bestScore	  = VbestScore	  
			  ,challengeTimes= VchallengeTimes
              ,isHaveSuccReward=VisHaveSuccReward
              ,lastFightResult=VlastFightResult
			  ,isSelect=IsSelect
			 } = HronInfo,
	THistory = [{Type,Score}||#hron_history_info{difficultytype=Type,bestscore=Score}<-HronHistory],
	Sql = io_lib:format("replace into gHron values(~w,'~s',~w,~w,~w,~w,~w,~s,~w,~w,~w,~w,~w,~s);",
						[RoleID, date(Vdate),Vstar,VcurDungeonNum,VattackAdd,VhpAdd,Vmorale
						,to_bin(VdungeonIDList),VbestScore,VchallengeTimes,VisHaveSuccReward,VlastFightResult,IsSelect,to_bin(THistory)]),
	sql_execute_with_log(Sql);
set_hronInfo(_,_,_) ->
	ignore.

get_gerMirrorInfo(RoleID) ->
    Sql = io_lib:format("select * from gGerMirror where roleID=~w;",[RoleID]),
    case get_row(Sql) of
        [_, GerMirrorInfoBin]->
            GerMirrorInfo = to_term(GerMirrorInfoBin),
            {MirrorSimpleGer,NewGerTypeId} = GerMirrorInfo#gerMirrorInfo.gerMirrorState,
            case MirrorSimpleGer of
                #gerSimple{gerAwakeInfo=GerAwakeInfo} ->
                	%%v3.1.0版本觉醒数据做了优化处理，此处需要兼容
                	NewGerAwakeInfo = [role_awake:transform_oldawake2newawake(GerAwakeUnit)||GerAwakeUnit<-GerAwakeInfo],
                    NewMirrorSimpleGer = MirrorSimpleGer#gerSimple{gerAwakeInfo=NewGerAwakeInfo},
                    GerMirrorInfo#gerMirrorInfo{gerMirrorState={NewMirrorSimpleGer,NewGerTypeId}};
                {gerSimple,GerID,GerTypeID,GerQuality,GerLevel,GerExp,GerPos} ->
                    NewMirrorSimpleGer = {gerSimple,GerID,GerTypeID,GerQuality,GerLevel,GerExp,GerPos,[],-1,0,#holyGrail{}},
                    GerMirrorInfo#gerMirrorInfo{gerMirrorState={NewMirrorSimpleGer,NewGerTypeId}};
                {gerSimple,GerID,GerTypeID,GerQuality,GerLevel,GerExp,GerPos,_} ->
                    NewMirrorSimpleGer = {gerSimple,GerID,GerTypeID,GerQuality,GerLevel,GerExp,GerPos,[],-1,0,#holyGrail{}},
                    GerMirrorInfo#gerMirrorInfo{gerMirrorState={NewMirrorSimpleGer,NewGerTypeId}};
                %%v3.3.0增加了精灵的神器信息，需要兼容
               	{gerSimple,GerID,GerTypeID,GerQuality,GerLevel,GerExp,GerPos,AwakeInfo,CrystalInfo,Body}->
               		NewAwakeInfo = [role_awake:transform_oldawake2newawake(GerAwakeUnit)||GerAwakeUnit<-AwakeInfo],
                    NewMirrorSimpleGer = {gerSimple,GerID,GerTypeID,GerQuality,GerLevel,GerExp,GerPos,NewAwakeInfo,CrystalInfo,Body,#holyGrail{}},
                    GerMirrorInfo#gerMirrorInfo{gerMirrorState={NewMirrorSimpleGer,NewGerTypeId}}
            end;
        _ ->
            ?INFO("get_gerMirrorInfo: no data, so init(~w)",[RoleID]),
            #gerMirrorInfo{gerMirrorState={#gerSimple{gerID=0},0},gerMirrorTimeList=[]}
    end.

set_gerMirrorInfo(RoleID, #gerMirrorInfo{}=GerMirrorInfo) ->
    Sql = io_lib:format("replace into gGerMirror values (~w,~s);",[RoleID,to_bin(GerMirrorInfo)]),
    sql_execute_with_log(Sql).

get_limitInfo(RoleID) ->
	Sql = io_lib:format("select * from gLimit where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[_, VencounterNum,VisBindWeibo,VinviteRoleID,VinviteRoleName,VlastShareLevel, VspiritGoldBoxCount,VspiritGoldBonusBoxCount
		,VspiritItemBoxCount, VequipGoldBoxCount,VequipGoldBonusBoxCount,VequipItemBoxCount,VtrainerGoldBonusBoxCount,VtrainerGoldBoxCount
		,VtrainerItemBoxCount,VspiritRefresh,VequipRefresh,VtrainerRefresh,VCombine2Value,VTicketValue,VTicketValue2]->
			#limitInfo{
					   encounterNum    = VencounterNum  
					   ,isBindWeibo     = int2bool(VisBindWeibo)   
					   ,inviteRoleID    = VinviteRoleID  
					   ,inviteRoleName  = VinviteRoleName
					   ,lastShareLevel  = VlastShareLevel
					   ,equipGoldBonusBoxCount=VequipGoldBonusBoxCount
					  ,equipGoldBoxCount=VequipGoldBoxCount
					  ,equipItemBoxCount=VequipItemBoxCount
					  ,spiritGoldBonusBoxCount=VspiritGoldBonusBoxCount
					  ,spiritGoldBoxCount=VspiritGoldBoxCount
					  ,spiritItemBoxCount=VspiritItemBoxCount
                       ,trainerGoldBonusBoxCount=VtrainerGoldBonusBoxCount
                       ,trainerGoldBoxCount=VtrainerGoldBoxCount
                       ,trainerItemBoxCount=VtrainerItemBoxCount
					  ,spiritRefresh = VspiritRefresh
					  ,equipRefresh = VequipRefresh
					  ,trainerRefresh=VtrainerRefresh
					  ,combine2StarCount=VCombine2Value
					  ,ticketBoxCount=VTicketValue
					  ,ticketBoxCount2=VTicketValue2
					  };
		_ ->
			undefined
	end.

set_limitInfo(RoleID, #limitInfo{}=LimitInfo) ->
	#limitInfo{
			   encounterNum    = VencounterNum  
			   ,isBindWeibo     = VisBindWeibo   
			   ,inviteRoleID    = VinviteRoleID  
			   ,inviteRoleName  = VinviteRoleName
			   ,lastShareLevel  = VlastShareLevel
			   ,equipGoldBonusBoxCount=VequipGoldBonusBoxCount
			   ,equipGoldBoxCount=VequipGoldBoxCount
			   ,equipItemBoxCount=VequipItemBoxCount
			   ,spiritGoldBonusBoxCount=VspiritGoldBonusBoxCount
			   ,spiritGoldBoxCount=VspiritGoldBoxCount
			   ,spiritItemBoxCount=VspiritItemBoxCount
               ,trainerGoldBonusBoxCount=VtrainerGoldBonusBoxCount
               ,trainerGoldBoxCount=VtrainerGoldBoxCount
               ,trainerItemBoxCount=VtrainerItemBoxCount	
			  ,spiritRefresh=VSpiritRefresh
			  ,equipRefresh=VEquipRefresh
			  ,trainerRefresh=VTrainerRefresh
			  ,combine2StarCount=VCombine2Value
			  ,ticketBoxCount=VTicketValue
			  ,ticketBoxCount2 = VTicketValue2
			  } = LimitInfo,
	Sql = io_lib:format("replace into gLimit values (~w,~w,~w,~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w);",
						[RoleID,VencounterNum,bool2int(VisBindWeibo),VinviteRoleID,quote(VinviteRoleName),VlastShareLevel,
						 VspiritGoldBoxCount,VspiritGoldBonusBoxCount,VspiritItemBoxCount
                        ,VequipGoldBoxCount,VequipGoldBonusBoxCount,VequipItemBoxCount
                        ,VtrainerGoldBonusBoxCount,VtrainerGoldBoxCount,VtrainerItemBoxCount
						,VSpiritRefresh,VEquipRefresh,VTrainerRefresh,VCombine2Value,VTicketValue,VTicketValue2]),
	sql_execute_with_log(Sql);
set_limitInfo(_,_)->
	ignore.

get_gatherInfo(RoleID) ->
	Sql = io_lib:format("select type,typeID from gGather where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = List ->
			lists:foldl(fun([1,E], [A,B,C]) ->
								[ [E|A], B,C];
						   ([2,E], [A,B,C]) ->
								[ A, [E|B],C];
                           ([3,E], [A,B,C]) ->
                                [ A, B,[E|C]]
						end, [[], [], []], List);
		_ ->
			[[],[],[]]
	end.

add_gatherInfo(RoleID, Type, TypeIDList) ->
	ArgList = [ [RoleID,Type,TypeID] || TypeID <- TypeIDList],
	if TypeIDList == [] ->
		   {ok,0};
	   true ->
	Sql = make_sql_batch("insert into gGather values","(~w,~w,~w)",ArgList),
	sql_execute_with_log(Sql)
	end.

get_manualInfo(RoleID)->
	Sql = io_lib:format("select * from gManual where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[_,ManualInfoBin]->
			{ManualUnitPersit,ManualCollectTaskPersist} = to_term(ManualInfoBin),
			#manual_info{manual_unit_list=role_gather:manual_unit_persit2term(ManualUnitPersit,[]),manual_collect_task_list=role_gather:manual_collect_task_persist2term(ManualCollectTaskPersist,[])};
		_->
			ManualCollectTask = role_gather:init_manual_collect_task(),
			#manual_info{manual_collect_task_list=ManualCollectTask}
	end.

set_manualInfo(RoleID,ManualInfo)->
	DeleteSql = io_lib:format("delete from gManual where roleID=~w;",[RoleID]),
	#manual_info{manual_unit_list=ManualUnitL,manual_collect_task_list=ManualCollectTaskL} = ManualInfo,
	if ManualUnitL=:=[] andalso ManualCollectTaskL=:=[]->
			sql_execute_with_log(DeleteSql),
			{ok,0};
		true->
			ManualInfoPersist = {role_gather:manual_unit_term2persit(ManualUnitL,[]),role_gather:manual_collect_task_term2persist(ManualCollectTaskL,[])},
			InsertSql = io_lib:format("insert into gManual values (~w,~s);",[RoleID,to_bin(ManualInfoPersist)]),
			sql_execute_with_log2(DeleteSql++InsertSql)
	end.

get_bestPassChapterID(RoleID) ->
	Sql = io_lib:format("select chapterID from gBestPassChapter where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] =List ->
			[E||[E]<-List];
		_ ->
			[]
	end.

get_chapter_star_reward_info(RoleID) ->
	Sql = io_lib:format("select roleID,chapterID,starRewardStatus,starCount from gStarRewardChapter where roleID = ~w",[RoleID]),
	case get_all(Sql) of
		[] ->
			recacl_chapter_reward_info(RoleID);
		[_|_] =List ->
			List;
		_ ->
			[]
	end.

update_chapter_star_reward_info(RoleID,ChapterID,StarRewardStatus,StarCount) ->
	Sql = io_lib:format("replace into gStarRewardChapter values (~w,~w,~w,~w)",[RoleID,ChapterID,StarRewardStatus,StarCount]),
	sql_execute_with_log(Sql).

recacl_chapter_reward_info(RoleID) ->
	Sql = io_lib:format("select chapterID,starRewarded,bestRewarded from gChapter where roleID = ~w",[RoleID]),
	case get_all(Sql) of
		[_|_] =List ->
			Result = 
				lists:foldl(fun([ChapterID,StarRewarded,BestRewarded],Acc) ->
								Sql0 = io_lib:format("select sum(bestScore) from gDungeon where roleID = ~w and chapterID = ~w",[RoleID,ChapterID]),
								case get_row(Sql0) of
									[Num] when is_integer(Num) ->
										case BestRewarded of
											1 ->
												StarRewarded2 = StarRewarded bor 4,
												update_gChapter_starRewarded(RoleID,ChapterID,StarRewarded2),
												[[RoleID,ChapterID, StarRewarded2,Num]|Acc];
											_ ->
												[[RoleID,ChapterID,StarRewarded,Num]|Acc]
										end;
									_ ->
										Acc
								end
						end,[],List),
			update_chapter_star_reward_info(Result),
			Result;
		_ ->
			[]
	end.

update_gChapter_starRewarded(RoleID,ChapterID,StarRewarded2) ->
	Sql = io_lib:format("update gChapter set starRewarded = ~w where roleID = ~w and ChapterID = ~w",[StarRewarded2,RoleID,ChapterID]),
	sql_execute_with_log(Sql).

update_chapter_star_reward_info([]) ->
	ignore;
update_chapter_star_reward_info(Result) ->
	Sql = make_sql_batch("replace into gStarRewardChapter values","(~w,~w,~w,~w)",Result),
	sql_execute_with_log(Sql).


add_bestPassChapterID(RoleID, ChapterID) ->
	Sql = io_lib:format("insert into gBestPassChapter values(~w,~w);",[RoleID,ChapterID]),
	sql_execute_with_log(Sql).

get_chapter(RoleID,ChapterID) ->
	Sql = io_lib:format("select bestRewarded,curDate,starRewarded from gChapter where roleID=~w and chapterID=~w;",[RoleID,ChapterID]),
	case get_row(Sql) of
		[BestRewarded0,{date,CurDate},StarRewarded]->
			BestRewarded = int2bool(BestRewarded0),
			Sql2 = io_lib:format("select dungeonID,restTimes,bestScore,resetTimes from gDungeon where roleID=~w and chapterID=~w;",[RoleID,ChapterID]),
			DungeonList = case get_all(Sql2) of
				[_|_]=List ->
					#data_chapter{dungeonIDList=DungeonIDList} = data_chapter:get(ChapterID),
					{ValidList,InvalidDungeonIDList}=lists:foldl(fun([DungeonID,RestTimes,BestScore,ResetTimes],{ValidAcc,InValidAcc})->
						case lists:member(DungeonID,DungeonIDList) of
							false->
								{ValidAcc,[DungeonID|InValidAcc]};
							true->
								PDungeon = #p_dungeon{bestScore=BestScore,dungeonID=DungeonID,restTimes=RestTimes,resetTimes=ResetTimes}, 
								{[PDungeon|ValidAcc],InValidAcc}
						end
					end,{[],[]},List),
					user_default:fixup_del_dungeonlist(RoleID,InvalidDungeonIDList),
					ValidList;
				_ ->
					[]
			end,
			#chapter{
				  id=ChapterID
				  ,perfectRewarded=BestRewarded
				  ,dungeonList=DungeonList
				  ,curDate=CurDate
				  ,starRewarded=StarRewarded
				  };
		_ ->
			?undefined
	end.

set_chapter(RoleID, ChapterList) ->
%% 	ArgList = [[RoleID,ChapterID,bool2int(BestRewarded),date(CurDate),StarRewarded] ||
%% 			   #chapter{
%% 						id=ChapterID
%% 						,perfectRewarded=BestRewarded
%% 						,curDate=CurDate
%% 					    ,starRewarded=StarRewarded
%% 					   } <- ChapterList],
%% 	ArgList2 = [[RoleID,ChapterID,DungeonID,RestTimes,BestScore,ResetTimes]||
%% 				#chapter{id=ChapterID,dungeonList=DungeonList} <- ChapterList,
%% 				#p_dungeon{bestScore=BestScore,dungeonID=DungeonID,restTimes=RestTimes,resetTimes=ResetTimes} <- DungeonList],
	{ArgList,ArgList2,ArgList3} = 
		lists:foldl(fun(Chapter,{Acc1,Acc2,Acc3})->
							#chapter{id=ChapterID,perfectRewarded=BestRewarded,curDate=CurDate,
									 dungeonList=DungeonList,starRewarded=StarRewarded}=Chapter,
							{DungeonAcc1,ScoreAcc1} = 
								lists:foldl(fun(Dungeon,{AccD1,AccD2})-> 
													#p_dungeon{bestScore=BestScore,dungeonID=DungeonID,restTimes=RestTimes,resetTimes=ResetTimes}=Dungeon,
													{[[RoleID,ChapterID,DungeonID,RestTimes,BestScore,ResetTimes]|AccD1],AccD2+BestScore}
											end,{[],0},DungeonList),
							{[[RoleID,ChapterID,bool2int(BestRewarded),date(CurDate),StarRewarded]|Acc1],DungeonAcc1++Acc2,[[RoleID,ChapterID,StarRewarded,ScoreAcc1]|Acc3]}
					end, {[],[],[]}, ChapterList),
	update_chapter_star_reward_info(ArgList3),
	if ArgList==[] ->
		   ignore;
	   true ->
		   Sql = make_sql_batch("replace into gChapter values","(~w,~w,~w,'~s',~w)",ArgList),
		   sql_execute_with_log(Sql)
	end,
%% 	if ArgList3 == [] ->
%% 		   ignore;
%% 	   true ->
%% 		   Sql3 = make_sql_batch("replace into gStarRewardChapter values","(~w,~w,~w,~w)",ArgList3),
%% 		   sql_execute_with_log(Sql3)
%% 	end,
	if ArgList2 == [] ->
		   {ok,0};
	   true ->
		   Sql2 = make_sql_batch("replace into gDungeon values", "(~w,~w,~w,~w,~w,~w)", ArgList2),
		   sql_execute_with_log(Sql2)
	end.

get_xbattle_chapter(RoleID,ChapterID)->
    Sql = io_lib:format("select passDungeons,isGetReward,challengeCount,passCount from gXbattleChapter where roleID = ~w and chapterID = ~w",[RoleID,ChapterID]),
    case db_sql:get_row(Sql) of
        [DungeonBin,IsGetReward,ChallengeCount,PassCount] ->
            #xbattle_chapter{chapterID=ChapterID,passDungeons=to_term(DungeonBin)
                             ,isGetReward=IsGetReward,challengeCount=ChallengeCount,passCount=PassCount};
        _ -> ?undefined 
    end.

set_xbattle_chapter(RoleID,Chapter)->
    #xbattle_chapter{chapterID=ChapterID,passDungeons=Dungeon,isGetReward=IsGetReward,challengeCount=ChallengeCount,passCount=PassCount}=Chapter,
    Sql = io_lib:format("replace into gXbattleChapter values (~w,~w,~s,~w,~w,~w)",[RoleID,ChapterID,to_bin(Dungeon),IsGetReward,ChallengeCount,PassCount]),
    sql_execute_with_log(Sql).

get_equipList(RoleID) ->
	Sql = io_lib:format("select itemUID,itemTypeID,itemPos,itemLevel,itemRank,itemGerID,itemDecay,itemExp,itemenchantType,itemenchantLevel from gEquip where roleID=~w;",[RoleID]),
	EquipExtraList = get_equipextralist(RoleID),
	case get_all(Sql) of
		[_|_]=EquipList ->
			lists:foldl(
			  fun([ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,ItemDecay,ItemExp,ItemEnchantType,ItemEnchantLevel],{Acc,BagEquip}) ->
					  #data_item{itemType=ItemType} = data_item:get(ItemTypeID),
					  %% 如果是在身上的装备，则计算属性
					  ItemLegendRank = case lists:keyfind(ItemUID,1,EquipExtraList) of false-> 0;{_,LegendRank}-> LegendRank end,
					  Item = role_item:refresh_item(#item{itemUID=ItemUID
													 ,itemTypeID=ItemTypeID
													 ,addAttr=0
													 ,itemDecay=ItemDecay
													 ,itemLevel=ItemLevel
													 ,itemNum=1
													 ,itemPos=ItemPos
													 ,itemRank=ItemRank
													 ,itemType=ItemType
													 ,itemExp=ItemExp
													 ,itemenchantType=ItemEnchantType
													 ,itemenchantLevel=ItemEnchantLevel
													 ,itemLegendRank = ItemLegendRank
													 }),
					  if ItemGerID == 0 ->
							 {Acc, [Item|BagEquip]};
						 true ->
							 case lists:keytake(ItemGerID, 1, Acc) of
								 false ->
									 {[{ItemGerID,[Item]}| Acc], BagEquip};
								 {value, {_, L}, Acc2} ->
									 {[{ItemGerID, [Item|L]} | Acc2], BagEquip}							
							 end
					  end
			  end, {[],[]}, EquipList);
		_ ->
			{[],[]}
	end.


%%该函数返回了对应装备的传奇锻造等级
get_equipedList(RoleID)->
	EquipExtraList = get_equipextralist(RoleID),
	Sql = io_lib:format("select itemUID,itemTypeID,itemPos,itemLevel,itemRank,itemGerID,itemDecay,itemExp,itemenchantType,itemenchantLevel from gEquip where roleID=~w AND itemPos > 0;",[RoleID]),
	case get_all(Sql) of
		[_|_]=EquipList ->
			lists:foldl(fun([ItemUID|T]=E,Acc)->
				LegendRank = case lists:keyfind(ItemUID,1,EquipExtraList) of false->0;{_,ItemLegendRank}->ItemLegendRank end,
				[lists:reverse([LegendRank|lists:reverse(E)])|Acc]
			end,[],EquipList);
		_ ->
			[]
	end.
	
set_equipList(RoleID, ListOfEquipList) ->
	DeleteSql = io_lib:format("delete from gEquip where roleID=~w",[RoleID]),
	% DeleteSql = io_lib:format("delete from gEquip where itemUID in (select itemUID from gEquip where roleID=~w);",[RoleID]),
	{ArgList,ExtraArgList} = lists:foldl(fun({ItemGerID,ItemList},{ArgListAcc,ExtraArgListAcc})-> 
		lists:foldl(fun(Item,{ArgListAcc1,ExtraArgListAcc1})->
 					#item{itemUID=ItemUID
						 ,itemTypeID=ItemTypeID
						 ,itemDecay=ItemDecay
						 ,itemLevel=ItemLevel
						 ,itemPos=ItemPos
						 ,itemRank=ItemRank
						 ,itemExp = ItemExp
						 ,itemenchantType=ItemEnchantType
						 ,itemenchantLevel=ItemEnchantLevel
						 ,itemLegendRank=ItemLegendRank} = Item,
					{[[ItemUID,RoleID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,item_lib:itemDecay(ItemDecay),ItemExp,ItemEnchantType,ItemEnchantLevel]|ArgListAcc1],
						[{ItemUID,ItemLegendRank}|ExtraArgListAcc1]}
			end,{ArgListAcc,ExtraArgListAcc},ItemList)
	end,{[],[]},ListOfEquipList),
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql),
           {ok,0};
       true ->
           %%持久化装备的extra信息
       	   set_equipextralist(RoleID,ExtraArgList),
           InsertSql = make_sql_batch("insert into gEquip values", "(~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ ";" ++ InsertSql)
    end.

get_equipextralist(RoleID)->
	Sql = io_lib:format("select itemUID,legendRank from gEquipExtra where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_]=EquipExtraList->
			[list_to_tuple(E)||E<-EquipExtraList];
		_ ->
			[]
	end.

% %%传奇锻造大于0的情况下才会持久化
% set_equipextralist(RoleID,ExtraList)->
% 	%%增加对数据是否存在做判断，防止delete删除数据时使用间隙锁，造成死锁
% 	CheckSql = io_lib:format("select itemUID from gEquipExtra where roleID=~w;",[RoleID]),
% 	case get_all(CheckSql) of
% 		[_|_]->
% 			Del = io_lib:format("delete from gEquipExtra where roleID=~w;",[RoleID]),
% 			ArgList = [[RoleID,ItemUID,LegendRank]||{ItemUID,LegendRank}<-ExtraList,LegendRank>0],
% 				case ArgList of
% 					[]->
% 						sql_execute_with_log(Del),
% 						{ok,0};
% 					_->
% 						InsertSql = make_sql_batch("insert into gEquipExtra values","(~w,~w,~w)",ArgList),
% 						sql_execute_with_log2(Del++InsertSql)
% 				end;
% 		_->
% 			ArgList = [[RoleID,ItemUID,LegendRank]||{ItemUID,LegendRank}<-ExtraList,LegendRank>0],
% 			case ArgList of
% 				[]->
% 					{ok,0};
% 				_->
% 					InsertSql = make_sql_batch("insert into gEquipExtra values","(~w,~w,~w)",ArgList),
% 					sql_execute_with_log(InsertSql)
% 			end
% 	end.
% set_equipextralist(RoleID,ExtraList)->
% 	%%增加对数据是否存在做判断，防止delete删除数据时使用间隙锁，造成死锁
% 	CheckSql = io_lib:format("select itemUID from gEquipExtra where roleID=~w;",[RoleID]),
% 	case get_all(CheckSql) of
% 		[]->
% 			ArgList = [[RoleID,ItemUID,LegendRank]||{ItemUID,LegendRank}<-ExtraList,LegendRank>0],
% 			case ArgList of
% 				[]->
% 					{ok,0};
% 				_->
% 					InsertSql = make_sql_batch("insert into gEquipExtra values","(~w,~w,~w)",ArgList),
% 					sql_execute_with_log(InsertSql)
% 			end;
% 		ItemUIDL->
% 			%%使用主键来作为索引删除，防止死锁
% 			ItemUIDStr1 = [integer_to_list(ItemUID)||[ItemUID]<-ItemUIDL],
% 			ItemUIDStr = string:join(ItemUIDStr1,","),
% 			Del = io_lib:format("delete from gEquipExtra where itemUID in (~s);",[ItemUIDStr]),
% 			ArgList = [[RoleID,ItemUID,LegendRank]||{ItemUID,LegendRank}<-ExtraList,LegendRank>0],
% 				case ArgList of
% 					[]->
% 						sql_execute_with_log(Del),
% 						{ok,0};
% 					_->
% 						InsertSql = make_sql_batch("insert into gEquipExtra values","(~w,~w,~w)",ArgList),
% 						sql_execute_with_log2(Del++InsertSql)
% 				end
% 	end.
set_equipextralist(RoleID,ExtraList)->
	%%增加对数据是否存在做判断，防止delete删除数据时使用间隙锁，造成死锁
	CheckSql = io_lib:format("select itemUID,legendRank from gEquipExtra where roleID=~w;",[RoleID]),
	case get_all(CheckSql) of
		[]->
			ArgList = [[RoleID,ItemUID,LegendRank]||{ItemUID,LegendRank}<-ExtraList,LegendRank>0],
			case ArgList of
				[]->
					{ok,0};
				_->
					InsertSql = make_sql_batch("insert into gEquipExtra values","(~w,~w,~w)",ArgList),
					sql_execute_with_log(InsertSql)
			end;
		ItemUIDL->
			ItemL = [list_to_tuple(E)||E<-ItemUIDL],
			ReplaceList = lists:foldl(fun({ItemUID,LegendRank},ReplaceAcc)->
					case lists:keyfind(ItemUID,1,ItemL) of
						false->
							[[RoleID,ItemUID,LegendRank]|ReplaceAcc];
						{ItemUID,LegendRank}->
							ReplaceAcc;
						{ItemUID,OldLegendRank}->
							[[RoleID,ItemUID,LegendRank]|ReplaceAcc]
					end						
				end,[],ExtraList),
			DeleteList = lists:foldl(fun({ItemUID,LegendRank},DelAcc)->
				case lists:keyfind(ItemUID,1,ExtraList) of
					false->
						[[RoleID,ItemUID,LegendRank]|DelAcc];
					_->
						DelAcc
				end
			end,[],ItemL),
			case DeleteList of
				[]->
					case ReplaceList of
						[]->
							{ok,0};
						_->
							InsertSql = make_sql_batch("replace into gEquipExtra values","(~w,~w,~w)",ReplaceList),
							sql_execute_with_log(InsertSql)
					end;
				_->
					ItemUIDStr1 = [integer_to_list(ItemUID)||[_,ItemUID,LegendRank]<-DeleteList],
					ItemUIDStr = string:join(ItemUIDStr1,","),
					DelSql = io_lib:format("delete from gEquipExtra where itemUID in (~s);",[ItemUIDStr]),
					case ReplaceList of
						[]->
							sql_execute_with_log(DelSql);
						_->
							InsertSql = make_sql_batch("replace into gEquipExtra values","(~w,~w,~w)",ReplaceList),
							sql_execute_with_log2(DelSql++InsertSql)
					end
			end
	end.
%% 统计出私聊离线记录中表中，每个roleid保存的私聊记录数量
init_ets_whisper_num()->
    Sql = io_lib:format("select roleID,count(1) from gWhisper group by roleID;",[]),
    case get_all(Sql) of
        [_|_] = ActList ->
            lists:foreach(fun([RoleID,Num])->
                                  ets:insert(?ETS_WHISPER_NUM, {RoleID,Num})
                            end, ActList);
        _ ->
            ok
    end.
get_whisper(RecvRoleID)->
    Sql = io_lib:format("select * from gWhisper where roleID=~w;",[RecvRoleID]),
    case get_all(Sql) of
        [_|_] = ActList ->
            ActList2 = [#p_whisper_record{roleID=SendRoleID
                                     ,talkMessage=Content
                                     ,timeStamp=SendTime} 
                            || [_RecvRoleID,SendRoleID,Content,SendTime] <-ActList],
            del_whisper(RecvRoleID),
            ?INFO("读取所有记录并删除  ~w",[ActList2]),
            ActList2;
        _ ->
            []
    end.
add_whisper(RecvRoleID,SendRoleID,Content)->
    SendTime = util:now(),
    add_whisper(RecvRoleID,SendRoleID,Content,SendTime).
add_whisper(RecvRoleID,SendRoleID,Content,SendTime)->    
    CurNum = case ets:lookup(?ETS_WHISPER_NUM, RecvRoleID) of
                 [{RecvRoleID,CurNum0}]->
                     CurNum0;
                 []->
                     0
             end,
    Max = data_talk:get(max_whisper),
    if
        CurNum < Max ->
            Sql = io_lib:format("insert into gWhisper values(~w,~w,~s,~w);",[RecvRoleID,SendRoleID,quote(Content),SendTime]),            
            sql_execute_with_log(Sql),
            ?INFO("写入一条私聊记录  ~w",[Sql]),
            ets:insert(?ETS_WHISPER_NUM, {RecvRoleID,CurNum+1}); %插入或覆盖
        true ->
            Sql1 = io_lib:format("delete from gwhisper where roleID = ~w order by sendtime limit 1;",[RecvRoleID]),          
            sql_execute_with_log(Sql1),
            Sql2 = io_lib:format("insert into gWhisper values(~w,~w,~s,~w);",[RecvRoleID,SendRoleID,quote(Content),SendTime]),            
            sql_execute_with_log(Sql2),
            ?INFO("先删除一条~w，写入一条私聊记录  ~w",[Sql1,Sql2])
    end.

del_whisper(RecvRoleID)->
    ets:delete(?ETS_WHISPER_NUM, RecvRoleID),
    Sql = io_lib:format("delete from gWhisper where roleID=~w;",[RecvRoleID]),
    sql_execute_with_log(Sql).

del_whisper(SendRoleID,RecvRoleID)->
    Sql = io_lib:format("delete from gWhisper where roleID=~w and sendID = ~w;",[RecvRoleID,SendRoleID]),
    ?INFO("del_whisper ~w",[Sql]),
    sql_execute_with_log(Sql).

get_bagItem(RoleID) ->
	Sql = io_lib:format("select itemUID,itemTypeID,itemNum from gBagItem where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_]=List ->
				[begin
					 #data_item{itemType=ItemType} = data_item:get(ItemTypeID),
					 #item{itemUID=ItemUID
													 ,itemTypeID=ItemTypeID
													 ,addAttr=0
													 ,itemDecay=0
													 ,itemLevel=1
													 ,itemNum=ItemNum
													 ,itemPos=0
													 ,itemRank=0
													 ,itemType=ItemType
													 ,itemExp=0
													 ,itemenchantLevel=0
													 ,itemenchantType=0}
				 end||[ItemUID,ItemTypeID,ItemNum]<-List];
		_ ->
			[]
	end.

set_bagItem(RoleID, BagItem) ->
    DeleteSql = io_lib:format("delete from gBagItem where roleID=~w;", [RoleID]),
    ArgList = [ [ItemUID,RoleID,ItemTypeID,ItemNum] || #item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemNum=ItemNum}<-BagItem],
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql),
           {ok,0};
       true ->
           InsertSql = make_sql_batch("insert into gBagItem values", "(~w,~w,~w,~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.

get_skin_info(RoleID) ->
    Sql = io_lib:format("select `has`, `equip`from gSkin where roleID = ~w", [RoleID]),
    case get_row(Sql) of
        [] ->
            #skin_info{};
        [Has, Equip] ->
            #skin_info{has=to_term(Has), equip=Equip}
    end.

set_skin_info(RoleID, SkinInfo) ->
    #skin_info{has=Has, equip=Equip} = SkinInfo,
    Sql = io_lib:format("replace into gSkin values(~w, ~s, ~w)", [RoleID, to_bin(Has), Equip]),
    {ok,_} = sql_execute_with_log(Sql).

check_ger_select(RoleID) ->
	Sql = io_lib:format("select count(*) from gGer where roleID=~w;",[RoleID]),
	[Num] = get_row(Sql),
	if Num > 0 ->
		   true;
	   true ->
		   false
	end.

get_ger_type(0) ->
    0;
get_ger_type(GerID) ->
    Sql = io_lib:format("select gerTypeID from gGer where gerID=~w;",[GerID]),
    case get_row(Sql) of
        [GerTypeID] when erlang:is_integer(GerTypeID) ->
            GerTypeID;
        _ ->
            0
    end.

get_ger_rank(0) ->
    0;
get_ger_rank(GerID) ->
    Sql = io_lib:format("select gerRank from gGer where gerID=~w;",[GerID]),
    case get_row(Sql) of
        [GerRank] when erlang:is_integer(GerRank) ->
            GerRank;
        _ ->
            0
    end.

get_gerList(RoleID) ->
	GerCrystalList = get_gerCrystal(RoleID),
	GerHolyGrailList = get_gerHolyGrail(RoleID),
	Sql = io_lib:format("select * from gGer where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = List ->
			lists:foldl(fun([GerID,_,GerTypeID,GerLevel,GerExp,GerRank,GerPos,GerAwakeInfo,GerBody], {PosListAcc, LPosListAcc, GerBagAcc}) ->
								GerCrystalInfo = case lists:keyfind(GerID,1,GerCrystalList) of
									false->
										[];
									{GerID,FindOne}->
										FindOne
								end,
								GerHolyGrailInfo = case lists:keyfind(GerID,1,GerHolyGrailList) of
									false->
										#holyGrail{};
									{GerID,FindGerHolyGrailInfo}->
										FindGerHolyGrailInfo
								end,
								%v3.1.0版本修改觉醒的刷新技能为一个列表，用以实现批量刷新觉醒技能，故此处进行数据兼容
								OldGerAwakeInfo = to_term(GerAwakeInfo),
								NewGerAwakeInfo = [role_awake:transform_oldawake2newawake(OldAwakeUnit)||OldAwakeUnit<-OldGerAwakeInfo],
								GerSimple = #gerSimple{gerExp=GerExp
															 ,gerID=GerID
															 ,gerLevel=GerLevel
															 ,gerPos=GerPos
															 ,gerQuality=GerRank
															 ,gerTypeID=GerTypeID
															 ,gerAwakeInfo=NewGerAwakeInfo
															 ,gerCrystalInfo=GerCrystalInfo
                                                             ,gerBody=GerBody
                                                             ,gerHolyGrailInfo=GerHolyGrailInfo
                                                             },
								if 
									GerPos == 0 ->
									   {PosListAcc, LPosListAcc, [GerSimple|GerBagAcc]};
								   	GerPos > ?lieuGerPos andalso GerPos < ?newlieuGerPos ->
								   		%%此处是4.1.0版本前的小伙伴的位置信息，需要根据gerTypeID计算出对应的伙伴营地
								   		#data_ger{gerProperty=GerProperty} = data_ger:get(GerTypeID),
										{PosListAcc, [GerSimple#gerSimple{gerPos=GerProperty}|LPosListAcc], GerBagAcc};
									GerPos > ?newlieuGerPos ->
										%%此处是4.1.0版本修改的小伙伴的位置信息，位置信息表示对应的伙伴营地
										{PosListAcc, [GerSimple#gerSimple{gerPos=GerPos-?newlieuGerPos}|LPosListAcc], GerBagAcc};
								   	true ->
									   {[GerSimple|PosListAcc],LPosListAcc, GerBagAcc}
								end
						end, {[],[],[]}, List);									   
		_ ->
			{[], [],[]}
	end.

set_gerList(RoleID, PosList, GerList, LPosList) ->
	ArgList1 = lists:foldl(fun(Ger, {GerAcc,CrystalAcc,HolyGrailAcc}) ->
								   #ger{gerID=GerID,gerBase=GerBase} = Ger,
								   #gerBase{gerExp=GerExp                   %% gerBody
											,gerLevel=GerLevel
											,gerPos=GerPos
											,gerQuality=GerRank
											,gerTypeID=GerTypeID
											,gerAwakeInfo=GerAwakeInfo
											,gerCrystalInfo=GerCrystalInfo
                                            ,gerBody=GerBody
                                            ,gerHolyGrailInfo=GerHolyGrailInfo
                                            }=GerBase,
								   {[[GerID,RoleID,GerTypeID,GerLevel,GerExp,GerRank,GerPos,to_bin(GerAwakeInfo),GerBody]|GerAcc],[[GerID,GerCrystalInfo]|CrystalAcc],[[GerID,GerHolyGrailInfo]|HolyGrailAcc]}
						   end, {[],[],[]}, PosList),
	ArgList2 = lists:foldl(fun(Ger, {GerAcc,CrystalAcc,HolyGrailAcc}) ->
								   #gerSimple{gerID=GerID                   %% gerBody
											  ,gerExp=GerExp
											  ,gerLevel=GerLevel
											  ,gerPos=GerPos
											  ,gerQuality=GerRank
											  ,gerTypeID=GerTypeID
											  ,gerAwakeInfo=GerAwakeInfo
											  ,gerCrystalInfo=GerCrystalInfo
                                              ,gerBody=GerBody
                                              ,gerHolyGrailInfo=GerHolyGrailInfo
                                              }=Ger,
								   {[[GerID,RoleID,GerTypeID,GerLevel,GerExp,GerRank,GerPos,to_bin(GerAwakeInfo),GerBody]|GerAcc],[[GerID,GerCrystalInfo]|CrystalAcc],[[GerID,GerHolyGrailInfo]|HolyGrailAcc]}
						   end, ArgList1, GerList),
	{ArgList3,CrystalArgList,HolyGrailArgList} = lists:foldl(fun(Ger,{GerAcc,CrystalAcc,HolyGrailAcc}) ->
								   #ger{gerID=GerID,gerBase=GerBase}=Ger,
								   #gerBase{gerExp=GerExp                   %% gerBody
											 ,gerLevel=GerLevel
											 ,gerPos=GerPos
											 ,gerQuality=GerRank
											 ,gerTypeID=GerTypeID
											 ,gerAwakeInfo=GerAwakeInfo
											 ,gerCrystalInfo=GerCrystalInfo
                                             ,gerBody=GerBody
                                             ,gerHolyGrailInfo=GerHolyGrailInfo}=GerBase,
								   {[[GerID,RoleID,GerTypeID, GerLevel,GerExp,GerRank,GerPos + ?newlieuGerPos,to_bin(GerAwakeInfo),GerBody]|GerAcc],[[GerID,GerCrystalInfo]|CrystalAcc],[[GerID,GerHolyGrailInfo]|HolyGrailAcc]}
						   end, ArgList2, LPosList),
    DeleteSql = io_lib:format("delete from gGer where roleID=~w;", [RoleID]),
    %?ERR("aaa:~w",[sql:execute(DeleteSql)]),
    %?ERR("ArgList3:~w CrystalArgList:~w ~n",[ArgList3,CrystalArgList]),
    set_gerCrystal(RoleID,CrystalArgList),
    set_gerHolyGrail(RoleID,HolyGrailArgList),
    if ArgList3 == [] ->
           sql_execute_with_log(DeleteSql),
           %?ERR("arglist=[]"),
           {ok, 0};
       true ->
           %?ERR("argList=~1000p",[ArgList2]),
           InsertSql = make_sql_batch("insert into gGer values","(~w,~w,~w,~w,~w,~w,~w,~s,~w)",ArgList3),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.

set_role_tag_data(RoleID,{TagID,TagData,IconData}) ->
    Sql = io_lib:format("replace into gTagData values (~w,~w,~s,~s)",[RoleID, TagID,to_bin(TagData),to_bin(IconData)]),
    sql_execute_with_log(Sql).

get_role_tag_data(RoleID) ->
    Sql = io_lib:format("select tagID,tagData,iconData from gTagData where roleID = ~w",[RoleID]),
    case get_row(Sql) of
        [TagID,TagBin,IconBin] -> {TagID,to_term(TagBin),to_term(IconBin)};
        _ -> {0,[],[]}
    end.

set_trainerProf_battle_data(RoleID,Data) ->
    Sql = io_lib:format("replace into gTrainerProfBattle values (~w,~s)",[RoleID,to_bin(Data)]),
    sql_execute_with_log(Sql).

get_trainerProf_battle_data(RoleID) ->
    Sql = io_lib:format("select data from gTrainerProfBattle where roleID = ~w",[RoleID]),
    case get_row(Sql) of
        [Data] -> to_term(Data);
        _ -> []
    end.

get_gerCrystal(RoleID)->
	Sql = io_lib:format("select gerID,gerCrystalInfo from gGerCrystal where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = List ->
			[{GerID,to_term(GerCrystal)}||[GerID,GerCrystal]<-List];								   
		_ ->
			[]
	end.

set_gerCrystal(RoleID,GerCrystalList)->
	DeleteSql = io_lib:format("delete from gGerCrystal where roleID=~w;",[RoleID]),
	if GerCrystalList =:= []->
			sql_execute_with_log(DeleteSql),
			{ok,0};
		true->
			Args = [[RoleID,GerID,to_bin(GerCrystalInfo)]||[GerID,GerCrystalInfo]<-GerCrystalList],
			InsertSql = make_sql_batch("insert into gGerCrystal values","(~w,~w,~s)",Args),
			sql_execute_with_log2(DeleteSql++InsertSql)
	end.

get_gerCrystal(RoleID,GerID)->
	Sql = io_lib:format("select gerCrystalInfo from gGerCrystal where roleID=~w and gerID=~w;",[RoleID,GerID]),
	case get_all(Sql) of
		[_|_] = List ->
			[{GerID,to_term(GerCrystal)}||[GerCrystal]<-List];								   
		_ ->
			[]
	end.

get_gerHolyGrail(RoleID)->
	Sql = io_lib:format("select gerID,gerDiamondInfo,gerHolyGrailLevel,isFinishSacrifice from gGerHolyGrail where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = List->
			[begin
				GerDiamondInfo = [#diamond_unit{pos=Pos,diamondID=DiamondID}||{Pos,DiamondID}<-to_term(GerDiamondInfoPersitForm)],
				{GerID,#holyGrail{holyGrailLevel=GerHolyGrailLevel,isFinishSacrifice=int2bool(IsFinishSacrifice),diamondInfo=GerDiamondInfo}}
			end||[GerID,GerDiamondInfoPersitForm,GerHolyGrailLevel,IsFinishSacrifice]<-List];
		_ ->
			[]
	end.

set_gerHolyGrail(RoleID,GerHolyGrailList)->
	DeleteSql = io_lib:format("delete from gGerHolyGrail where roleID=~w;",[RoleID]),
	if GerHolyGrailList =:= [] ->
			sql_execute_with_log(DeleteSql),
			{ok,0};
		true->
			Args = [begin
					GerDiamondInfoPersitForm = to_bin([{Pos,DiamondID}||#diamond_unit{pos=Pos,diamondID=DiamondID}<-GerDiamondInfo]), 
					[RoleID,GerID,GerDiamondInfoPersitForm,GerHolyGrailLevel,bool2int(IsFinishSacrifice)]
					end
			||[GerID,#holyGrail{holyGrailLevel=GerHolyGrailLevel,isFinishSacrifice=IsFinishSacrifice,diamondInfo=GerDiamondInfo}]<-GerHolyGrailList
				,GerHolyGrailLevel>1 orelse IsFinishSacrifice=:=true orelse GerDiamondInfo=/=[]],
			%?ERR("Args:~w ~n",[Args]),
			case Args of
				[]->
					sql_execute_with_log(DeleteSql),
					{ok,0};
				_ ->
					%?ERR("Args:~w ~n",[Args]),
					InsertSql = make_sql_batch("insert into gGerHolyGrail values","(~w,~w,~s,~w,~w)",Args),
					sql_execute_with_log2(DeleteSql++InsertSql)
			end
	end.


get_role_activityFestival(RoleID) ->
	Sql = io_lib:format("select data from gactivityfestival where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[Bin] -> uncompress_decode(Bin,#activity_festival{});
		_ -> #activity_festival{}
	end.

set_role_activityFestival(RoleID,V) ->
	Bin = compress_encode(V),
	Sql = io_lib:format("replace into gactivityfestival values (~w,~s)",[RoleID,quote(Bin)]),
	sql_execute_with_log(Sql).

get_role_lieutenant_info(RoleID)->
	Sql = io_lib:format("select lieuInfo from gLieuInfo where roleID = ~w;",[RoleID]),
	case get_row(Sql) of
		[Bin]->
			uncompress_decode(Bin,[]);
		_ ->
			[]
	end.

set_role_lieutenant_info(RoleID,Info)->
	Bin = compress_encode(Info),
	Sql = io_lib:format("replace into gLieuInfo values (~w,~s);",[RoleID, quote(Bin)]),
	{ok,_} = sql_execute_with_log(Sql).

update_role_fighters(#rolePublic{roleID=RoleID,fightPower=FightPower}=RolePublic) ->
	Sql = io_lib:format("select fightPower from gRole where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[FightPower] ->
			case get_row("select roleID,fightPower from gRole where accid < 10000 order by fightPower limit 1") of
				[RoRoleID,RoFightPower] ->
					ets:insert(?ETS_ROLE_PUBLIC, RolePublic#rolePublic{fightPower=RoFightPower}),
					{A,C,D,B,E,F} = get_fighterList_and_lieu_add_info(RoRoleID),
					B1 = lieuview2lieupersist(B),
					Sql2 = io_lib:format("replace into gFighterList values (~w,~s,~s,~w,~w,~s,'~w')"
										,[RoleID,to_bin(A), to_bin(B1), C, D,to_bin(E),F]),
					sql_execute_with_log(Sql2);
				_ ->
					ignore
			end;
		_ ->
			ignore
	end.

get_fighterList(RoleID) ->
	Sql = io_lib:format("select fighterList from gFighterList where roleID=~w",[RoleID]),
	case get_row(Sql) of
		[FighterListBin] ->
			FighterList1 = to_term(FighterListBin),
			lists:foldl(fun(Fighter,Acc) ->
                            [ger_attr:transOldGer2NewGer(Fighter)|Acc]
			            end,[],FighterList1);
		_ ->
            []
	end.

get_fighterList_and_lieu_add_info(RoleID)->
	Sql = io_lib:format("select fighterList, lieuInfoList,lieuAtkAdd, lieuHpAdd,trSpecial,lieuAddAttr from gFighterList where roleID = ~w;",[RoleID]),
	case get_row(Sql) of
		[FighterListBin, LieuInfoListBin, AtkAdd, HpAdd,TrBin,LieuAdd]->
    		FighterList = to_term(FighterListBin),
    		TrSpecial = case to_term(TrBin) of [] -> #trSpecial{};X -> X end,
    		NewFighterList = lists:foldl(fun(Fighter,Acc) ->
                                            [ger_attr:transOldGer2NewGer(Fighter)|Acc]
    			                        end,[],FighterList),
            if
                NewFighterList =:= [] ->
                    ?ERR("get_fighterList_and_lieu_add_info data error fightlist:~w",[NewFighterList]);
                true ->
                    ignore
            end,
            LieuAdd1 = case LieuAdd of ?undefined->#add_attr{};_->addattrpersist2addattr(LieuAdd) end,
			{NewFighterList,AtkAdd,HpAdd,lieuviewpersist2lieu(to_term(LieuInfoListBin)),TrSpecial,LieuAdd1};
		_ ->
			{[],0,0,[],#trSpecial{},#add_attr{}}
	end.

set_fighterList(RoleID,FighterList,LieuInfoList, AtkAdd, HpAdd,TrSpecial,LieuAddAttr) ->
	LieuInfoList1 = lieuview2lieupersist(LieuInfoList), 
	Sql = io_lib:format("replace into gFighterList values (~w,~s,~s,~w,~w,~s,'~w')"
					   ,[RoleID,to_bin(FighterList), to_bin(LieuInfoList1), AtkAdd, HpAdd,to_bin(TrSpecial),LieuAddAttr]),
	sql_execute_with_log(Sql).

get_fighterList_and_lieu_add(RoleID)->
	Sql = io_lib:format("select fighterList,trSpecial,lieuAddAttr from gFighterList where roleID=~w;", [RoleID]),
	case get_row(Sql) of
		[FighterListBin,TrBin,LieuAdd] ->
			FighterList = to_term(FighterListBin),
			TrSpecial = case to_term(TrBin) of [] -> #trSpecial{};X -> X end,
			NewFighterList = lists:foldl(fun(Fighter,Acc) ->
                                            [ger_attr:transOldGer2NewGer(Fighter)|Acc]
			                            end,[],FighterList),
            TalentListT = get_trainer_info(RoleID),
            TalentList = role_talent:to_active_talent_list(TalentListT, util:now()),
            LieuAdd1 = case LieuAdd of ?undefined->#add_attr{};_->addattrpersist2addattr(LieuAdd) end,
			{NewFighterList, LieuAdd1, TalentList,TrSpecial};
		_ ->
            {[],#add_attr{},[],#trSpecial{}}
	end.

set_role_gag_list(RoleID, GagList)->
	Sql = io_lib:format("replace into gTalk values (~w, ~s);",[RoleID, to_bin(GagList)]),
	sql_execute_with_log(Sql).

get_role_gag_list(RoleID) ->
	Sql = io_lib:format("select gag_list from gTalk where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[GagListBin]->
			to_term(GagListBin);
		_ ->
            []
	end.

set_canvass_info(RoleID, CanvassId, CanvassInfo) 
  when erlang:is_list(CanvassInfo) andalso CanvassInfo /= [] ->
    CanvassStr = 
        lists:foldr(fun(C,AccStr)-> 
                            [C+48|AccStr] 
                    end, "", CanvassInfo),
    Sql = io_lib:format("replace into gCanvassinfo values (~w, ~w, ~s);",[RoleID, CanvassId, CanvassStr]),
    sql_execute_with_log(Sql);
set_canvass_info(RoleID, CanvassId, CanvassInfo) ->
    ignore.

get_canvass_info(RoleID) ->
    Sql = io_lib:format("select canvass_id,select_list from gCanvassInfo where roleID=~w;",[RoleID]),
    case get_row(Sql) of
        [CanvassId,CanvassStr]->
            {CanvassId,lists:foldr(fun(S,AccList)-> [S-48|AccList] end, [], erlang:binary_to_list(CanvassStr))};
        _ ->
            {0,[]}
    end.

get_sign_emperor_info(RoleID) ->
	Sql = io_lib:format("select lastSignDate,signedDays,isEmperor, isGetBox from gOtherRecord where roleID = ~w;", [RoleID]),
	case get_row(Sql) of
		[{date,LastSignDate}, SignedDays, IsEmperor, IsGetBox] ->
			{LastSignDate, SignedDays, IsEmperor, IsGetBox};
		_ ->
			{{1989,7,17},0,0,0}
	end.

set_sign_emperor_info(RoleID, {LastSignedDate, SignedDays, IsEmperor, IsGetBox}) ->
	Sql = io_lib:format("replace into gOtherRecord values (~w, '~s', ~w, ~w, ~w);",[RoleID, date(LastSignedDate), SignedDays, IsEmperor, IsGetBox] ),
	sql_execute_with_log(Sql).

log_emperor_roleID(RoleID) ->
	Sql = io_lib:format("insert into log_emperor_history values (null, ~w);", [RoleID]),
	sql_execute_with_log(Sql).
	
get_activityInfo(RoleID) ->
	Sql = io_lib:format("select actID,value,list from gActivity where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_] = ActList ->
			ActList2 = 
			[#act{actID=ActID,list=to_term(List),value=to_term(Value,0)}
			|| [ActID,Value,List]<-ActList],
			 #dc{actList=ActList2,roleID=RoleID};
		_ ->
			#dc{actList=[],roleID=RoleID}
	end.

set_activityInfo(RoleID, DC) ->
    DeleteSql = io_lib:format("delete from gActivity where roleID=~w;",[RoleID]),
    ArgList = [[RoleID,ActID,to_bin(Value),to_bin(List)]
               || #act{actID=ActID,list=List,value=Value} <- DC#dc.actList],
    
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql),
           {ok,0};
       true ->
           InsertSql = make_sql_batch("insert into gActivity values","(~w,~w,~s,~s)",ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.


get_friendList(RoleID) ->
	Sql = io_lib:format("select type,friendID from gFriend where roleID=~w;",[RoleID]),
	case get_all(Sql) of
		[_|_]=List->
			Pal = [E||[1,E]<-List],
			Foe = [E||[2,E]<-List],
			#d_friend{foe=Foe,pal=Pal,roleID=RoleID};
		_ ->
			#d_friend{foe=[],pal=[],roleID=RoleID}
	end.

add_friend(RoleID, 1=Type, FriendID) ->
	Sql = io_lib:format("insert into gFriend values(~w,~w,~w),(~w,~w,~w);",[RoleID,Type,FriendID,FriendID,Type,RoleID]),
	sql_execute_with_log(Sql);
add_friend(RoleID, 2=Type, FriendID) ->
	Sql = io_lib:format("insert into gFriend values(~w,~w,~w);",[RoleID,Type,FriendID]),
	sql_execute_with_log(Sql).

del_friend(RoleID, 1=Type, FriendID) ->
	Sql = io_lib:format("delete from gFriend where roleID=~w and type=~w and friendID=~w;",[RoleID,Type,FriendID]),
	sql_execute_with_log(Sql),
	Sql2 = io_lib:format("delete from gFriend where roleID=~w and type=~w and friendID=~w;",[FriendID,Type,RoleID]),
	sql_execute_with_log(Sql2);	
del_friend(RoleID, 2=Type, FriendID) ->
	Sql = io_lib:format("delete from gFriend where roleID=~w and type=~w and friendID=~w;",[RoleID,Type,FriendID]),
	sql_execute_with_log(Sql).
					 
get_pushInfo(RoleID) ->
	Sql = io_lib:format("select * from gPush where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[_,Token,IsPVPPushOpen,IsPushNightMute]->
			#d_push{isPVPPushOpen=int2bool(IsPVPPushOpen),
					isPushNightMute=int2bool(IsPushNightMute),
					roleID=RoleID,
					token=Token};
		_ ->
			?undefined
			end.

set_pushInfo(RoleID,PushInfo) ->
	#d_push{isPVPPushOpen=IsPVPPushOpen,
			isPushNightMute=IsPushNightMute,
			token=Token} = PushInfo,		
	Sql = io_lib:format("replace into gPush values (~w,~s,~w,~w);",
						[RoleID,quote(Token),bool2int(IsPVPPushOpen),bool2int(IsPushNightMute)]),
	sql_execute_with_log(Sql).
	
				   
	
get_inviteInfo(RoleID) ->
	RewardNumSql = io_lib:format("select rewardNum from gInvite where roleID=~w;",[RoleID]),
	case get_row(RewardNumSql) of
		[RewardNum] ->
			next;
		_ ->
			RewardNum=0
	end,
	InviteListSql = io_lib:format("select inviteRoleID from gInviteRoleList where roleID=~w;",[RoleID]),
	case get_all(InviteListSql) of
		[_|_]=List ->
			InviteList = [E||[E]<-List];
		_ ->
			InviteList = []
	end,
	#d_invite{inviteRoleIDList=InviteList,rewardNum=RewardNum,roleID=RoleID}.

set_inviteRewardNum(RoleID,RewardNum) ->
	Sql = io_lib:format("replace into gInvite values(~w,~w);",[RoleID,RewardNum]),
	sql_execute_with_log(Sql).

get_inviteRewardNum(InviterRoleID)->
	Sql = io_lib:format("select rewardNum from gInvite where roleID = ~w;", [InviterRoleID]),
	case get_row(Sql) of
		[Num] ->
			Num;
		_ ->
			0
	end.

add_inviteRoleID(RoleID, InviteRoleID) ->
	Sql = io_lib:format("insert into gInviteRoleList values (~w,~w);",[RoleID, InviteRoleID]),
	sql_execute_with_log(Sql).
	
get_guideState(RoleID)->
	Sql = io_lib:format("select guideState from gGuide where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[GuideState] ->
			GuideState;
		_ ->
			0
	end.

set_guideState(RoleID,GuideState) ->
	Sql = io_lib:format("replace into gGuide values(~w,~w);",[RoleID,GuideState]),
	sql_execute_with_log(Sql).

get_etc(Key) ->
	Sql = io_lib:format("select value from gETC where `key`=~w;", [Key]),
	case get_row(Sql) of
		[Bin] ->
			uncompress_decode(Bin,[]);
		_ ->
			[]
	end.

get_generalteam_info(TeamID) ->
	Sql = io_lib:format("select teaminfo from gGeneralTeamInfo where `teamid`=~w;", [TeamID]),
	case get_row(Sql) of
		[Bin] ->
			uncompress_decode(Bin,[]);
		_ ->
			[]
	end.

set_generalteam_info(TeamID,TeamInfo)->
	Bin = compress_encode(TeamInfo),
	Sql = io_lib:format("replace into gGeneralTeamInfo values (~w,~s);",[TeamID,quote(Bin)]),
	{ok,_} = sql_execute_with_log(Sql).

set_generalteam_info(InfoList) when is_list(InfoList)->
	ArgList = [[TeamID,quote(compress_encode(TeamInfo))]||{TeamID,TeamInfo}<-InfoList],
	make_sql_batch_by_piece("replace into gGeneralTeamInfo values","(~w,~s)",ArgList, 100).

del_generalteam_info(TeamIDList) when is_list(TeamIDList)->
	Arg = string:join([integer_to_list(E)||E<-TeamIDList], ","),
	Sql = io_lib:format("delete from gGeneralTeamInfo where teamid in (~s);",[Arg]),
	sql_execute_with_log(Sql);

del_generalteam_info(TeamID)->
	Sql = io_lib:format("delete from gGeneralTeamInfo where `teamid` = ~w;",[TeamID]),
	sql_execute_with_log(Sql).

del_all_generalteam_info()->
	Sql = "TRUNCATE table gGeneralTeamInfo;",
	sql_execute_with_log(Sql).
	
get_generalteam_all_info()->
	Sql = io_lib:format("select teaminfo from gGeneralTeamInfo", []),
	case get_rows(Sql) of
		 TeamInfoBinList when is_list(TeamInfoBinList) ->
			[uncompress_decode(Bin,[])||[Bin]<-TeamInfoBinList];
		_ ->
			[]
	end.

set_etc(Key, Info) ->
	Bin = compress_encode(Info),
	%?DEBUG("set etc,size=~w,key=~w",[byte_size(Bin), Key]),
	Sql = io_lib:format("replace into gETC values (~w,~s);",[Key, quote(Bin)]),
	{ok,_} = sql_execute_with_log(Sql).

del_etc(Key) ->
	Sql = io_lib:format("delete from gETC where `key` = ~w;",[Key]),
	sql_execute_with_log(Sql).

%% 获得红包内容
get_bonusInfo(BonusUID) ->
    Sql = io_lib:format("select `info` from gBonus where `bonusID` = ~w", [BonusUID]),
    case get_row(Sql) of
        [Info] ->
            to_term(Info, {p_bonus_info, 0, 0, 0, 0, 0, []});
        _ ->
            {p_bonus_info, 0, 0, 0, 0, 0, []}
    end.

%% 设置红包内容
set_bonusInfo(BonusUID, Info) ->
    Sql = io_lib:format("replace into gBonus values (~w, ~s);", [BonusUID, to_bin(Info)]),
    sql_execute_with_log(Sql).
		
%% 返回的格式是#sc_fight_request,需要适应旧数据
get_fightReplay(ReplayUID) ->
    Sql = io_lib:format("select replay,isCompress from gReplay where replayUID=~w;", [ReplayUID]),
    ScFightRequest = case get_row(Sql) of
        [Bin, IsCompress] ->
            case IsCompress of
                0 ->
                    to_term(Bin,[]);
                _ ->
                    uncompress_decode(Bin)
            end;
        _ ->
            []
    end,
    case ScFightRequest of
        _ when ScFightRequest /= [] andalso is_list(ScFightRequest#sc_fight_request.fighterList)->
            NewFighterList = lists:foldr(fun(Pfighter,AccList)->
                                    Pfighter2 = case Pfighter of
                                        % 这是旧协议保存的数据，需要转化为新格式
                                        {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality} ->
                                            {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,0,[],0};
                                        {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone}->
                                        	{p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone,[],0};
                                        {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone,_}->
                                            {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone,[],0};
                                        _ ->
                                            Pfighter
                                    end,
                                    [Pfighter2|AccList]
            end, [], ScFightRequest#sc_fight_request.fighterList),
            ScFightRequest#sc_fight_request{fighterList = NewFighterList};
        _ ->
            ScFightRequest
    end.

set_fightReplay(ReplayUID, Info, RepType) ->
	Sql = io_lib:format("insert into gReplay values (~w,~s,'~s',~w,~w);",[ReplayUID, quote(compress_encode(Info)), datetime(erlang:localtime()), RepType,1]),
	sql_execute_with_log(Sql).

set_fightReplayList(List) ->
	Time = datetime(erlang:localtime()),
	ArgList = [ [ReplayUID, quote(compress_encode(Info)), Time, RepType,1] || [ReplayUID, Info, RepType] <- List],
	make_sql_batch_by_piece("insert into gReplay values","(~w,~s,'~s',~w,~w)",ArgList, 100).

del_fightReplay(ReplayUID) ->
    Sql = io_lib:format("delete from gReplay where replayUID = ~w;",[ReplayUID]),
    sql_execute_with_log(Sql).

del_fightReplayList([]) ->
	{ok,0};
del_fightReplayList(List) ->
	Sql = io_lib:format("delete from gReplay where replayUID in (~s);",[List]),
	sql_execute_with_log(Sql).

del_all_replay() ->
    sql_execute_with_log("truncate table gReplay").

del_replay_with_type(Type) ->
    Sql = io_lib:format("delete from gReplay where `type` = ~p;", [Type]),
    sql_execute_with_log(Sql).

del_spec_type_and_time_replay(Type, DayLimit) ->
	Time = util:seconds_to_datetime(util:now() - DayLimit * ?ONE_DAY_SECONDS),
	del_spec_type_replay_before_time(Type,Time).

del_spec_type_replay_before_time(Type,Time) ->
	Sql = io_lib:format("delete from `gReplay` where type = ~w and time < '~s';", [Type, datetime(Time)]),
    sql_execute_with_log(Sql).

add_role_gift_drawed_type(RoleID, Type) ->
	Sql = io_lib:format("insert into gGift values (~w,~s)",[RoleID, quote(Type)]),
	sql_execute_with_log(Sql).

check_role_gift_drawed_type(RoleID, Type) ->
	Sql = io_lib:format("select count(*) from gGift where roleID=~w and `type`=~s;",[RoleID, quote(Type)]),
	case get_row(Sql) of
		[A] when is_integer(A) ->
			A >= 1;
		_ ->
			false
	end.

sql_find_mail(MailUID)->
	Sql = io_lib:format("select * from gMail where mailUID = ~w;",[MailUID]),
	case get_row(Sql) of
		[MailUID,RecvID,MailType,SenderID,_SenderName,_Content,_Time,MailTemplateID,_ParamList,MailReward,_IsRead,_Head,_IsMale]->
			[RecvID,MailTemplateID,db_sql:to_term(MailReward),MailType,SenderID];
		_ ->
			[0,0,0,0,0]
	end.

sql_find_mail_dtl_more(MailUID)->
    Sql = io_lib:format("select * from gMail where mailUID = ~w;",[MailUID]),
    case get_row(Sql) of
        [MailUID,RecvID,MailType,SenderID,_SenderName,_Content,_Time,MailTemplateID,ParamList,MailReward,_IsRead,_Head,_IsMale]->
            [RecvID,MailTemplateID,db_sql:to_term(MailReward),MailType,SenderID,db_sql:to_term(ParamList)];
        _ ->
            [0,0,0,0,0,[]]
    end.

sql_find_spec_mailUID_list(RecvID, SenderID, MailType) ->
    Sql = io_lib:format("select mailUID from gMail where recvID = ~w and senderID = ~w and mailType = ~w", [RecvID, SenderID, MailType]),
    case get_all(Sql) of
        List when erlang:is_list(List) ->
            lists:flatten(List);
        _ ->
            []
    end.

get_unread_num(RoleID,Type)->
	case Type of
		0 ->
			Sql = io_lib:format("select count(*) from gMail where recvID = ~w and isRead = false;", [RoleID]);
		2 ->
			Sql = io_lib:format("select count(*) from gMail where recvID = ~w and mailType = 5 and isRead = false;", [RoleID]);
		3 ->
            Sql = io_lib:format("select count(*) from gMail where recvID = ~w and (mailType = 3 or mailType = 9) and isRead = false;", [RoleID]);
        _ ->
			Sql = io_lib:format("select count(*) from gMail where recvID = ~w and mailType <> 5 and mailType <> 3 and mailType <> 9 and isRead = false;", [RoleID])
	end,
	case get_row(Sql) of
		[A] when is_integer(A) ->
			A;
		{error, {_, Reason2}} ->
			?ERR("sql ****~s*****execute with err:~s",[Sql,Reason2]),
			0
	end.

get_last_mails_by_type(RoleID, Type, ClientTopMailUID) ->
    %% 初始化时ClientTopMailUID为0,通过判断这个,做一些邮件的初始化工作
    case ClientTopMailUID of
        0 ->
            case data_common:get(add_friend_mail_day) of
                Number when is_number(Number) ->
                    OverdueTime= util:now() - Number * 86400,
                    DelSql = io_lib:format("delete from gMail where `recvID` = ~w and `mailType` = ~w and `time` <= ~w;", [RoleID, ?MAIL_TYPE_ADD_FRIEND, OverdueTime]),
                    sql_execute_with_log(DelSql);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end,
	case Type of
		2 ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType = 5 and mailUID > ~w order by mailUID desc limit ~w;",[RoleID, ClientTopMailUID, ?GET_SQL_MAIL_NUM]);		
        3 ->
            Sql = io_lib:format("select * from gMail where recvID = ~w and (mailType = 3 or mailType = 9) and mailUID > ~w order by mailUID desc limit ~w;",[RoleID, ClientTopMailUID, ?GET_SQL_MAIL_NUM]);
        _ ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType <> 5 and mailType <> 3 and mailType <> 9 and mailUID > ~w order by mailUID desc limit ~w;",[RoleID, ClientTopMailUID, ?GET_SQL_MAIL_NUM])
	end,
	case get_all(Sql) of
		[_|_] = List ->
			A = 
				lists:foldr(fun([MailUID,_,MailType,SenderID,SenderName,Content,Time,MailTemplateID,ParamList,MailReward,IsRead,Head,IsMale],
							Acc) ->
								IsRead2 = db_sql:int2bool(IsRead),
								IsMale2 = db_sql:int2bool(IsMale),
								Mail = #mail{
									mailUID = MailUID,
									mailType = MailType,
									senderID = SenderID,
									senderName = SenderName,
									content = Content,
									time = Time,
									mailTemplateID = MailTemplateID,
									paramList = db_sql:to_term(ParamList),
									mailReward = db_sql:to_term(MailReward,[]),
									isRead = IsRead2,
									head=Head,
									isMale=IsMale2},
									[Mail|Acc]
							end, [],List),
			#d_mail{mail = [A], roleID = RoleID};
		_ ->
			#d_mail{mail = [[]], roleID = RoleID}
	end.

sql_get_more_mail(RoleID, Type, StartMailUID)->
	case Type of
		2 ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType = 5 and mailUID < ~w order by mailUID desc limit ~w;",[RoleID, StartMailUID, ?GET_SQL_MAIL_NUM]);
        3 ->
            Sql = io_lib:format("select * from gMail where recvID = ~w and (mailType = 3 or mailType = 9) and mailUID < ~w order by mailUID desc limit ~w;",[RoleID, StartMailUID, ?GET_SQL_MAIL_NUM]);
        _ ->
			Sql = io_lib:format("select * from gMail where recvID = ~w and mailType <> 5 and mailType <> 3 and mailType <> 9 and mailUID < ~w order by mailUID desc limit ~w;",[RoleID, StartMailUID, ?GET_SQL_MAIL_NUM])
	end,
	case get_all(Sql) of
		[_|_] = List ->
			A = 
				lists:foldl(fun([MailUID,_,MailType,SenderID,SenderName,Content,Time,MailTemplateID,ParamList,MailReward,IsRead,Head,IsMale],
							Acc) ->
								IsRead2 = db_sql:int2bool(IsRead),
								IsMale2 = db_sql:int2bool(IsMale),
								Mail = #mail{
									mailUID = MailUID,
									mailType = MailType,
									senderID = SenderID,
									senderName = SenderName,
									content = Content,
									time = Time,
									mailTemplateID = MailTemplateID,
									paramList = db_sql:to_term(ParamList),
									mailReward = db_sql:to_term(MailReward,[]),
									isRead = IsRead2,
									head=Head,
									isMale=IsMale},
									[Mail|Acc]
							end, [],List),
			lists:reverse(A);
		_ ->
			[]
	end.

%% get_mailList(RoleID) ->
%% 	Sql = io_lib:format("select * from gMail where recvID=~w order by mailUID;",[RoleID]),
%% 	case get_all(Sql) of
%% 		[_|_] = List ->
%% 			{A,B,C, UA,UB,UC} = 
%% 			lists:foldl(fun([MailUID,_,MailType,SenderID,SenderName,Content,Time,MailTemplateID,ParamList,MailReward,IsRead,Head,IsMale],
%% 							{SysAcc, PrivAcc, CollAcc, Un1, Un2, Un3}) ->
%% 								IsRead2 = int2bool(IsRead),
%% 								IsMale = int2bool(IsMale),
%% 								Mail = #mail{
%% 											mailUID=MailUID
%% 											,mailType=MailType
%% 											,senderID=SenderID
%% 											,senderName=SenderName
%% 											,content=Content
%% 											,time=Time
%% 											,mailTemplateID=MailTemplateID
%% 											,paramList=to_term(ParamList)
%% 											,mailReward=to_term(MailReward,[])
%% 											,isRead=IsRead2
%% 											,head=Head
%% 											,isMale=IsMale
%% 											 },
%% 								if MailType == ?MAIL_TYPE_PRIVATE ->
%% 									   if IsRead2 ->
%% 									   {SysAcc, [Mail|PrivAcc], CollAcc, Un1, Un2, Un3};
%% 										  true ->
%% 									   {SysAcc, [Mail|PrivAcc], CollAcc, Un1, Un2+1, Un3}
%% 									   end;
%% 								   MailType == ?MAIL_TYPE_UNION ->
%% 									   if IsRead2 ->
%% 									   {SysAcc, PrivAcc, [Mail|CollAcc], Un1, Un2, Un3};
%% 										  true ->
%% 									   {SysAcc, PrivAcc, [Mail|CollAcc], Un1, Un2, Un3+1}
%% 									   end;
%% 								   true ->
%% 									   if IsRead2 ->
%% 									   {[Mail|SysAcc], PrivAcc, CollAcc, Un1, Un2, Un3};
%% 										  true ->
%% 									   {[Mail|SysAcc], PrivAcc, CollAcc, Un1+1, Un2, Un3}
%% 									   end
%% 								end
%% 						end, {[], [], [], 0, 0, 0}, List), 
%% 			#d_mail{mail=[A,B,C],roleID=RoleID,unreadNum=[UA,UB,UC]};
%% 		_ ->
%% 			#d_mail{mail=[[],[],[]],roleID=RoleID,unreadNum=[0,0,0]}
%% 	end.
del_mail([]) ->
	{ok,0};
del_mail(MailUIDList) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("delete from gMail where mailUID in (~s);",[Arg]),
	sql_execute_with_log(Sql).

read_mail([]) ->
	{ok,0};
read_mail(MailUIDList) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("update gMail set isRead=1 where mailUID in (~s);",[Arg]),
	sql_execute_with_log(Sql).


add_mailList([]) ->
	{ok,0};
add_mailList(List) ->
	case length(List) > 1001 of
		true ->
			add_mailList2(List);
		_ ->
			add_mailList1(List)
	end.
add_mailList1(List) ->
	ArgList = 
		[begin
			 #mail{
				   mailUID=MailUID
				   ,mailType=MailType
				   ,senderID=SenderID
				   ,senderName=SenderName
				   ,content=Content
				   ,time=Time
				   ,mailTemplateID=MailTemplateID
				   ,paramList=ParamList
				   ,mailReward=MailReward
				   ,isRead=IsRead
				   ,head=Head
				   ,isMale=IsMale
				  }= Mail,
			 [MailUID,RoleID,MailType,SenderID,quote(SenderName),quote(Content),Time,MailTemplateID,to_bin(ParamList),to_bin(MailReward),bool2int(IsRead),Head,bool2int(IsMale)]
		 end || {RoleID, Mail} <-List],
    make_sql_batch_by_piece("insert into gMail values","(~w,~w,~w,~w,~s,~s,~w,~w,~s,~s,~w,~w,~w)",ArgList, 1000).

add_mailList2(List) ->
	add_mailList3(List,[],0).
add_mailList3([],L,_) ->
	add_mailList1(L);
add_mailList3(T,L,1001)->
	add_mailList1(L),
	add_mailList3(T,[],0);
add_mailList3([H|T],L,N)->
	add_mailList3(T,[H|L],N+1).

get_histList(RoleID,Type) ->
	Sql = io_lib:format("select histUID,histType,name,enemyID,time,arg,isRead from gHist where roleID=~w and type=~w order by histUID;",[RoleID, Type]),
	case get_all(Sql) of
		[_|_] = List ->
			{A,B} = 
				lists:foldl(fun([VhistUID,VhistType,Vname,VenemyID,Vtime,Varg,VisRead]
								,{Acc, UN})  ->
									IsRead = int2bool(VisRead),
									Hist = #hist{
												 histUID     = VhistUID    
												 ,histType   = VhistType  
												 ,name       = Vname      
												 ,enemyID    = VenemyID   
												 ,time       = Vtime      
												 ,arg        = Varg       
												 ,isRead     = IsRead
												},
									if IsRead ->
										   {[Hist|Acc], UN};
									   true ->
										   {[Hist|Acc], UN+1}
									end					
							end, {[],0}, List),
			#d_hist{histList=A,unreadNum=B};
		_ ->
			#d_hist{histList=[],unreadNum=0}
	end.

del_hist([]) ->
	{ok,0};
del_hist(MailUIDList) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("delete from gHist where histUID in (~s);delete from gReplay where replayUID in (~s);",[Arg,Arg]),
	sql_execute_sqls(Sql).

del_oldhist()->
	Time = util:now() - 9 * 24 * 3600,
	Sql = io_lib:format("delete from gHist where time < ~w; delete from gReplay where time < '~s';", [Time,datetime(util:seconds_to_datetime(Time))]),
	sql_execute_sqls(Sql).

read_hist([]) ->
	{ok,0};
read_hist(MailUIDList) ->
	Arg = string:join([integer_to_list(E)||E<-MailUIDList], ","),
	Sql = io_lib:format("update gHist set isRead=1 where histUID in (~s);",[Arg]),
	sql_execute_with_log(Sql).


add_histList([]) ->
	{ok,0};
add_histList(List) ->
	ArgList = 
		[begin
			 #hist{
									histUID     = VhistUID    
									,histType   = VhistType  
									,name       = Vname      
									,enemyID    = VenemyID   
									,time       = Vtime      
									,arg        = Varg       
									,isRead     = IsRead
				  }= Hist,
			 [VhistUID,VhistType,quote(Vname),VenemyID,Vtime,Varg,bool2int(IsRead), RoleID, Type]
		 end || {RoleID, Hist, _FightInfo, Type} <-List],
	%?ERR("list=~w",[List]),
	Sql = make_sql_batch("insert into gHist values", "(~w,~w,~s,~w,~w,~w,~w,'',~w,~w)",ArgList),
	sql_execute_with_log(Sql),
	ReplayList = [ [HistUID,FightInfo,trans2reptype(Type)] || {_RoleID, #hist{histUID=HistUID}, FightInfo, Type} <-List ],
	set_fightReplayList(ReplayList).

trans2reptype(Type) ->
    case Type of
        ?TYPE_PVP ->
            ?REPLAY_TYPE_PVP;
        ?TYPE_PLUNDER ->
            ?REPLAY_TYPE_PLUNDER;
        _ ->
            ?REPLAY_TYPE_UNKNOWN
    end.

get_hist_fightInfo(HistUID) ->
	get_fightReplay(HistUID).
	

check_account_ban(AccountID) ->
	Sql = io_lib:format("select count(*) from gBanAccount where accountID=~w;",[AccountID]),
	case get_row(Sql) of
		[N] when N > 0 ->
			true;
		_ ->
			false
	end.

add_account_ban(AccountID) ->
	Sql = io_lib:format("insert into gBanAccount values (~w);",[AccountID]),
	sql_execute_with_log(Sql).

del_account_ban(AccountIDList) ->
	Arg = string:join([integer_to_list(E)||E<-AccountIDList], ","),
	Sql = io_lib:format("delete from gBanAccount where accountID in (~s);",[Arg]),
	sql_execute_with_log(Sql).



get_team_pk_data(RoleID) ->
    Sql = io_lib:format("select teamPkData from gTeamPk where roleID=~w",[RoleID]),
    case get_row(Sql) of
        [Bin] ->
            TeamPkData = to_term(Bin),
            case TeamPkData of
                #team_pk_info{selfTeam=SelfTeam,otherTeam=OtherTeam} ->
                    TeamPkData#team_pk_info{selfTeam=addFamilyAttrToFighterTeam(SelfTeam),otherTeam=addFamilyAttrToFighterTeam(OtherTeam)};
                _ ->
                    {SelfList, OtherList} = role_team:gen_team_data(get_roleInfo(RoleID)),
                    #team_pk_info{session=role_team:get_team_pk_session(),refreshSelf=1,refreshOther=1,selfTeam=SelfList,otherTeam=OtherList}
            end;
        _ ->
            ?undefined
    end.


%% 从1.3.0升级到1.4.0 team_member的fighterData,新增加天赋列表一项,所以这里要进行数据升级
%% 从2.2.0升级到2.3.0 team_member的fighterData中添加皮肤数据
addFamilyAttrToFighterData(FighterData)->
    case FighterData of
        {FighterList, B} ->
            TalentList = [],
			TrSpecial=#trSpecial{},
			SkinInfo = #skin_info{};
        {FighterList, B, TalentList} ->
            TrSpecial=#trSpecial{},
            SkinInfo = #skin_info{};
		{FighterList,B,TalentList,TrSpecial} ->
			SkinInfo = #skin_info{};
		{FighterList,B,TalentList,TrSpecial,SkinInfo}->
			ignore
    end,
	case FighterList of
		[] ->
			{FighterList,B,TalentList};
		_ ->
    		NewFighterList = lists:foldl(fun(OldGer,Acc) ->
                                            #ger{gerAttr=GerAttr} = NewFighter = ger_attr:transOldGer2NewGer(OldGer),
						                    [NewFighter#ger{gerAttr=deduct_gerattr(GerAttr)}|Acc]
			                            end,[],FighterList),
			{lists:reverse(NewFighterList),B,TalentList,TrSpecial,SkinInfo}
	end.
%-record(team_member, {roleID=0,fightPower=0,isMale=true,title=0,head=0,level=0,roleName= <<"">>,fighterData={[],{0,0},[],[],[]},itemList=[],vip=0}).
addFamilyAttrToFighterTeam(TeamList) ->
    NewTeamList = lists:foldl(fun(#team_member{fighterData=FigherData}=Team,Acc) ->
                                      %#team_member{fighterData = FigherData} = Team,
                                      NewTeam = Team#team_member{fighterData = addFamilyAttrToFighterData(FigherData)},
                                      [NewTeam|Acc];
                                 ({A,B,C,D,E,F,G,H,I,J},Acc) ->
                                      NewTeam={A,B,C,D,E,F,G,H,addFamilyAttrToFighterData(I),J,1},
                                      [NewTeam|Acc]
                              end,[],TeamList),
    lists:reverse(NewTeamList).


set_team_pk_data(_RoleID,?undefined) ->
    ok;
set_team_pk_data(RoleID,TeamPkData) ->
    Sql = io_lib:format("replace into gTeamPk values (~w,~s)",[RoleID,to_bin(TeamPkData)]),
    sql_execute_with_log(Sql).

get_alien_data(RoleID) ->
    Sql = io_lib:format("select alienTimes,lastRecoverTime,resetTime from gAlien where roleID=~w",[RoleID]),
    case get_row(Sql) of
        [AlienTiems,LastRecoverTime,ResetTime] ->
            AlienData = #alien_info{times=AlienTiems, lastRecoverTime=LastRecoverTime,resetTime=ResetTime};
        _ ->
            AlienData = ?undefined
    end,
    AlienData.

set_alien_data(_RoleID,?undefined) ->
    ok;
set_alien_data(RoleID,#alien_info{times=AlienTiems, lastRecoverTime=LastRecoverTime, resetTime=ResetTime}) ->
    Sql = io_lib:format("replace into gAlien values (~w,~w,~w,~w)",[RoleID,AlienTiems,LastRecoverTime,ResetTime]),
    sql_execute_with_log(Sql).

create_treasure_patch(RoleID)->
	case tk_id:is_robot(RoleID) of
		true ->
			{ok,0};
		_ ->
			Sql = io_lib:format("insert into gtreasurepatch values (~w,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);", [RoleID]),
			sql_execute_with_log(Sql)
	end.

%% 在对原来的宝物碎片转换时，由于有些玩家是没有任何碎片的，也就无法对这些玩家创建碎片数据，所以其首次登录时，需要创建一下碎片数据
%% 新创角玩儿家在此处创建碎片数据集合
get_treasuer_patch(RoleID)->
	Sql = io_lib:format("select * from gtreasurepatch where roleID = ~w;", [RoleID]),
	case get_all(Sql) of
		[] ->
			create_treasure_patch(RoleID),
			Sql = io_lib:format("select * from gtreasurepatch where roleID = ~w;", [RoleID]),
			case get_all(Sql) of
				[] ->
					?ERR("can't create treasure_patch in sql,roleID=~w",[RoleID]);
				[_|_]->
					get_treasuer_patch(RoleID)
			end;
		[_|_]=L ->
			[#t_patch{roleID=R1
					  ,p_21201 = P1
					  ,p_21202 = P2
					  ,p_21203 = P3
					  ,p_21204 = P4
					  ,p_21205 = P5
					  ,p_21211 = P6
					  ,p_21212 = P7
					  ,p_21213 = P8
					  ,p_21214 = P9
					  ,p_21215 = P10
					  ,p_21216 = P11
					  ,p_21217 = P12
					  ,p_21218 = P13
					  ,p_21219 = P14
					  ,p_21220 = P15
					  ,p_21231 = P16
					  ,p_21232 = P17
					  ,p_21233 = P18
					  ,p_21234 = P19
					  ,p_21235 = P20
					  ,p_21241 = P21
					  ,p_21242 = P22
					  ,p_21243 = P23
					  ,p_21244 = P24
					  ,p_21245 = P25
					  ,p_21251 = P26
					  ,p_21252 = P27
					  ,p_21253 = P28
					  ,p_21254 = P29
					  ,p_21255 = P30
					  ,p_21261 = P31
					  ,p_21262 = P32
					  ,p_21263 = P33
					  ,p_21264 = P34
					  ,p_21265 = P35
					  ,p_21266 = P36
					  ,p_21267 = P37
					  ,p_21268 = P38
					  ,p_21269 = P39
					  ,p_21270 = P40}
			 ||[R1, P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23,P24,P25,P26,P27,P28,P29,P30,P31,P32,P33,P34,P35,P36,P37,P38,P39,P40|_]<-L];
		_ ->
			?ERR("get treasure patch error RoleID=~w",[RoleID]),
			[#t_patch{}]
	end.
update_treasure_patch(PatchInfo)->
	#t_patch{roleID=R1
			 ,p_21201 = P1
			 ,p_21202 = P2
			 ,p_21203 = P3
			 ,p_21204 = P4
			 ,p_21205 = P5
			 ,p_21211 = P6
			 ,p_21212 = P7
			 ,p_21213 = P8
			 ,p_21214 = P9
			 ,p_21215 = P10
			 ,p_21216 = P11
			 ,p_21217 = P12
			 ,p_21218 = P13
			 ,p_21219 = P14
			 ,p_21220 = P15
			 ,p_21231 = P16
			 ,p_21232 = P17
			 ,p_21233 = P18
			 ,p_21234 = P19
			 ,p_21235 = P20
			 ,p_21241 = P21
			 ,p_21242 = P22
			 ,p_21243 = P23
			 ,p_21244 = P24
			 ,p_21245 = P25
			 ,p_21251 = P26
			 ,p_21252 = P27
			 ,p_21253 = P28
			 ,p_21254 = P29
			 ,p_21255 = P30
			 ,p_21261 = P31
			 ,p_21262 = P32
			 ,p_21263 = P33
			 ,p_21264 = P34
			 ,p_21265 = P35
			 ,p_21266 = P36
			 ,p_21267 = P37
			 ,p_21268 = P38
			 ,p_21269 = P39
			 ,p_21270 = P40} = PatchInfo,
	Sql = io_lib:format(%"delete from gtreasurepatch where roleID = ~w;
						"replace into gtreasurepatch values(~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);"
						,[R1,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22,P23,P24,P25,P26,P27,P28,P29,P30,P31,P32,P33,P34,P35,P36,P37,P38,P39,P40]),
	%sql:call_proc(Sql).
	sql_execute_with_log(Sql).

get_some_patch(ItemTypeID, N)->
	Sql = io_lib:format("select RoleID from gtreasurepatch where gtreasurepatch.~w > 0 order by rand() limit 1;", [ItemTypeID]),
	case get_row(Sql) of
		[ID] ->
			Sql2 = io_lib:format("select RoleID, gtreasurepatch.~w from gtreasurepatch where gtreasurepatch.~w >0 and RoleID > ~w limit ~w;", [ItemTypeID, ItemTypeID, ID, N]),
			case get_all(Sql2) of
				[_|_]=L ->
					L;
				_ ->
					[]
			end;
		_ ->
			[]
	end.


get_task(RoleID)->
	TaskSql = io_lib:format("select * from gTask where roleID = ~w",[RoleID]),
	List = get_all(TaskSql),
	{RList1,L} = lists:foldl(fun([_,TaskID,Status,Num,ArgsBin],{Acc,IDS}=CC)->
					  case data_task:get(TaskID) of
						  ?undefined->
							  CC;
						  #data_task{trigger_id=TriggerID}->
					 		 {[#r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=to_term(ArgsBin),trigger_id=TriggerID}|Acc],[TaskID|IDS]}
					  end
			  end, {[],[]},List),
	ListAll = data_task:get_list(),
	AddList = ListAll -- L,
	lists:foldl(fun(ID,Acc)->
						#data_task{task_type=TaskType,trigger_id=TriggerID} = data_task:get(ID),
							case TaskType of
								?TASK_TYPE_MAIN ->
                                    Acc;
                                %% 公会膜拜任务不自动接取
                                ?TASK_TYPE_FAMILY_WORSHIP ->
									Acc;
                                ?TASK_TYPE_TODAY ->
                                    case role_task:check_level(ID) of
                                        true ->
                                            [#r_task{task_id=ID,status=?TASK_STATUS_WAS_ACCEPT,trigger_num=0,trigger_notes=[],trigger_id=TriggerID}|Acc];
                                        false ->
                                            Acc
                                    end;
								_->
									[#r_task{task_id=ID,status=?TASK_STATUS_WAS_ACCEPT,trigger_num=0,trigger_notes=[],trigger_id=TriggerID}|Acc]
							end
				end, RList1, AddList).
	

set_task(RoleID,RoleTaskList)->
%% 	DBRoleTaskList =
%% 		lists:map(fun(#r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=TriggerArgs})->
%% 							[RoleID,TaskID,Status,Num,to_bin(TriggerArgs)]
%% 					end, RoleTaskList),
	DBRoleTaskList = [[RoleID,TaskID,Status,Num,to_bin(TriggerArgs)]
					 ||#r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=TriggerArgs}<-RoleTaskList
					 	,not(Status == ?TASK_STATUS_WAS_ACCEPT andalso TriggerArgs == [] andalso Num == 0)],
	DeleteTaskSql = io_lib:format("delete from gTask where roleID=~w;", [RoleID]),
    if DBRoleTaskList == [] ->
           sql_execute_with_log(DeleteTaskSql);
       true ->
           InsertSql1 = make_sql_batch2("insert into gTask values","(~w,~w,~w,~w,~s)",DBRoleTaskList),
		   OnDuplicateUpDateSql = " ON DUPLICATE KEY UPDATE status=values(status),triggerNum=values(triggerNum), triggerNotes=values(triggerNotes); ",
           sql_execute_with_log2(DeleteTaskSql ++ InsertSql1 ++ OnDuplicateUpDateSql)
    end.

get_task_by_id(RoleID, TaskID) ->
    TaskSql = io_lib:format("select * from gTask where roleID = ~w and taskID = ~w",[RoleID, TaskID]),
    case get_row(TaskSql) of
        [RoleID,TaskID,Status,Num,ArgsBin] ->
            case data_task:get(TaskID) of
                ?undefined->
                    ?undefined;
                #data_task{trigger_id=TriggerID}->
                    #r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=to_term(ArgsBin),trigger_id=TriggerID}
            end;
        [] ->
            #data_task{task_type=TaskType,trigger_id=TriggerID} = data_task:get(TaskID),
            case TaskType of
                ?TASK_TYPE_MAIN->
                    ?undefined;
                _ ->
                    #r_task{task_id=TaskID,status=?TASK_STATUS_WAS_ACCEPT,trigger_num=0,trigger_notes=[],trigger_id=TriggerID}
            end;
        ErrData ->
            ?ERR("RoleID:~w, TaskID:~w, ErrData:~w", [RoleID, TaskID, ErrData]),
            ?undefined
    end.

set_task_by_id(RoleID, #r_task{task_id=TaskID,status=Status,trigger_num=Num,trigger_notes=TriggerArgs}) ->
    Sql = io_lib:format("replace into gTask values (~w,~w,~w,~w,~s)", [RoleID,TaskID,Status,Num,to_bin(TriggerArgs)]),
    sql_execute_with_log(Sql);

set_task_by_id(RoleID, Data) ->
    ?ERR("set_task_by_id error,RoleID:~w, Data:~w", [RoleID, Data]).
			
update_machine_info(InfoList)->
	[begin
		 case Ele of
			 {p_homestead_machine,A,B,C,D,E,F,G}->
				 if C > 0 ->
						{p_homestead_machine,A,B,C,D,E,F,G,C-3600};
					true ->
						{p_homestead_machine,A,B,C,D,E,F,G,0}
				 end;
			 _ ->
				 Ele
		 end
	 end
	 ||Ele<-InfoList].

get_homestead(RoleID)->
	Sql = io_lib:format("select * from gHomestead where roleID = ~w", [RoleID]),
	case get_all(Sql) of
		[[_,RoleName,EneayTimes,MatingTimes,MatingCoolSecond,Add4Mating,GerID,GerTypeID,Quality,Level,RefreshMatingSecond,MachineListBin,LogListBin]]->
			HomeSteadInfo = #p_homestead{roleName=RoleName,energyTimes=EneayTimes,matingTimes=MatingTimes,
						 matingCoolSecond=MatingCoolSecond,add4mating=Add4Mating,gerID=GerID,gerTypeID=GerTypeID,quality=Quality,level=Level,refreshMatingSecond=RefreshMatingSecond},
			MachineList = update_machine_info(to_term(MachineListBin)),
			LogList = to_term(LogListBin),
			{HomeSteadInfo,MachineList,LogList};
		_->
			?undefined
	end.
set_homestead(RoleID,{HomeSteadInfo,MachieList,LogList})->
	#p_homestead{roleName=RoleName,energyTimes=AddEneayTimes,matingTimes=MatingTimes,
						 matingCoolSecond=MatingCoolSecond,add4mating=Add4Mating,gerID=GerID,gerTypeID=GerTypeID,quality=Quality,level=Level,refreshMatingSecond=RefreshMatingSecond} = HomeSteadInfo,
	MachineListBin = to_bin(MachieList),
	LogListBin = to_bin(LogList),
	Sql = io_lib:format("replace into gHomestead values(~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~s) ", [RoleID,quote(RoleName),AddEneayTimes,MatingTimes,MatingCoolSecond,Add4Mating,GerID,GerTypeID,Quality,Level,RefreshMatingSecond,MachineListBin,LogListBin]),
	sql_execute_with_log(Sql);
set_homestead(_RoleID,?undefined)->
	ignore.

get_home_info(RoleID) ->
    Sql = io_lib:format("select * from gHome where roleID = ~w;", [RoleID]),
    case get_row(Sql) of
        [RoleID, Stage, ConstStr,NewTaskTimerStr,DountyAcNum,CirrusBlob]->
            ConstList = case catch to_constr_list(ConstStr) of
                List when erlang:is_list(List) ->
                    List;
                {'EXIT', Reason} ->
                    ?ERR("to_constr_list fail(~w) ~w >>> ~w",[RoleID,ConstStr,Reason]),
                    []
                end,        
            NewTaskTimer = str_to_int_list(NewTaskTimerStr),
            {Num,Y,M,D} = case str_to_int_list(DountyAcNum) of
                    [Num0,Y0,M0,D0] ->
                        {Num0,Y0,M0,D0};
                    _ ->
                        {0,0,0,0}
                end,
            OldCirrus = to_term(CirrusBlob),
            CirrusList = lists:foldr(fun
                                        (Old,AccList) when erlang:is_record(Old, home_cirrus) -> [Old|AccList];
                                        ({home_cirrus,Index,IsWin,DoingTimestamp,DoingGerId,CostType
                                         ,CirrusReward,RewardAlready,IsGetBig},AccList)->
                                             New = #home_cirrus{index = Index
                                                               ,is_win = IsWin
                                                               ,doing_timestamp = DoingTimestamp
                                                               ,doing_ger_id = DoingGerId
                                                               ,cost_type = CostType
                                                               ,cirrus_reward = CirrusReward
                                                               ,reward_already = RewardAlready
                                                               ,is_get_big = IsGetBig
                                                               ,doing_ger_type = get_ger_type(DoingGerId)
                                                               ,doing_ger_quality = get_ger_rank(DoingGerId)},
                                             [New|AccList] ;
                                        ({home_cirrus,Index,IsWin,DoingTimestamp,DoingGerId,CostType
                                         ,CirrusReward,RewardAlready,IsGetBig,DoingGerType},AccList)->
                                             New = #home_cirrus{index = Index
                                                               ,is_win = IsWin
                                                               ,doing_timestamp = DoingTimestamp
                                                               ,doing_ger_id = DoingGerId
                                                               ,cost_type = CostType
                                                               ,cirrus_reward = CirrusReward
                                                               ,reward_already = RewardAlready
                                                               ,is_get_big = IsGetBig
                                                               ,doing_ger_type = DoingGerType
                                                               ,doing_ger_quality = get_ger_rank(DoingGerId)},
                                             [New|AccList]
                                     end, [], OldCirrus),
            #home_info{stage=Stage
                      ,constr_list=ConstList
                      ,new_task_timer=NewTaskTimer
                      ,dounty_ac_num=Num
                      ,dounty_ac_date={Y,M,D}
                      ,cirrus_list=CirrusList};
        _ ->
            #home_info{stage=1
                      ,constr_list=[]
                      ,new_task_timer=[]
                      ,dounty_ac_num=0
                      ,dounty_ac_date={0,0,0}
                      ,cirrus_list=[]}
    end.
persist_home_info(RoleID,HomeInfo) ->
    {Y,M,D} = HomeInfo#home_info.dounty_ac_date,
    Sql = io_lib:format("replace into gHome values (~w,~w,\"~s\",\"~s\",\"~s\",~s);"
                       ,[RoleID
                        ,HomeInfo#home_info.stage
                        ,constr_list_to_str(HomeInfo#home_info.constr_list)
                        ,int_list_to_str(HomeInfo#home_info.new_task_timer)
                        ,int_list_to_str([HomeInfo#home_info.dounty_ac_num,Y,M,D])
                        ,to_bin(HomeInfo#home_info.cirrus_list)]),
    sql_execute_with_log(Sql).

add_home_task_info(TaskInfo) ->
    TGerTypeListStr0 = lists:foldr(fun(TgtGerType,AccStr)->
                                        [["_"|erlang:integer_to_list(TgtGerType)]|AccStr]
                                    end, "", TaskInfo#home_task.tgt_ger_type),
    [_|TGerTypeListStr] = lists:flatten(TGerTypeListStr0),
    AddSql = io_lib:format("insert into ghometask (owner_role_id,quality,level,tgt_ger_type,timtout_ts,status,accept_role_id,ger_id
                            ,ger_type,ger_quality,ger_level,task_type,reward_num,role_level,box_reward_bin) 
                            values (~w,~w,~w,\"~s\",~w,~w,~w,\"~s\",\"~s\",\"~s\",\"~s\",~w,~w,~w,~s);"
                         ,[TaskInfo#home_task.onwer_role_id
                          ,TaskInfo#home_task.quality
                          ,TaskInfo#home_task.level
                          ,int_list_to_str(TaskInfo#home_task.tgt_ger_type)
                          ,TaskInfo#home_task.timtout_ts
                          ,TaskInfo#home_task.status
                          ,TaskInfo#home_task.role_id
                          ,int_list_to_str(TaskInfo#home_task.ger_id)
                          ,int_list_to_str(TaskInfo#home_task.ger_type)
                          ,int_list_to_str(TaskInfo#home_task.ger_quality)
                          ,int_list_to_str(TaskInfo#home_task.ger_level)
                          ,TaskInfo#home_task.task_type
                          ,TaskInfo#home_task.base_reward_num
                          ,TaskInfo#home_task.role_level
                          ,to_bin(TaskInfo#home_task.box_reward)]),
    {ok,RetId} = sql_execute_with_log(AddSql),
    RetId.

%% [{TId,ARoleId,Ts}]
get_task_need_timer() ->
    Sql = io_lib:format("select idghometask,accept_role_id,owner_role_id,status,timtout_ts from ghometask where status = ~w or status = ~w or status = ~w;", [2,3,4]),
    lists:foldl(fun(DbData,AccList)->
                        [DbData|AccList]
                end, [], get_rows(Sql)).

get_home_task_info(RoleID) ->
    Sql = io_lib:format("select * from ghometask where owner_role_id = ~w or accept_role_id = ~w;", [RoleID,RoleID]),
    lists:foldl(fun(DbData,AccList)->
                        [to_pb_home_task_info(DbData)|AccList]
                end, [], get_rows(Sql)).

del_home_task(TaskId)->
    Sql = io_lib:format("delete from ghometask where idghometask = ~w;", [TaskId]),
    sql_execute_with_log(Sql).    

persist_home_task_info(TaskInfoList) ->
    Sqls = lists:foldl(fun(TaskInfo,AccStr)->
                        TGerTypeListStr = int_list_to_str(TaskInfo#home_task.tgt_ger_type),
                        io_lib:format("replace into ghometask values (~w,~w,~w,~w,\"~s\",~w,~w,~w,\"~s\",\"~s\",\"~s\",\"~s\",~w,~w,~w,~s);"
                                     ,[TaskInfo#home_task.id
                                      ,TaskInfo#home_task.onwer_role_id
                                      ,TaskInfo#home_task.quality
                                      ,TaskInfo#home_task.level
                                      ,TGerTypeListStr
                                      ,TaskInfo#home_task.timtout_ts
                                      ,TaskInfo#home_task.status
                                      ,TaskInfo#home_task.role_id
                                      ,int_list_to_str(TaskInfo#home_task.ger_id)
                                      ,int_list_to_str(TaskInfo#home_task.ger_type)
                                      ,int_list_to_str(TaskInfo#home_task.ger_quality)
                                      ,int_list_to_str(TaskInfo#home_task.ger_level)
                                      ,TaskInfo#home_task.task_type
                                      ,TaskInfo#home_task.base_reward_num
                                      ,TaskInfo#home_task.role_level
                                      ,to_bin(TaskInfo#home_task.box_reward)])
                        ++AccStr
        end, "", TaskInfoList),
    case erlang:length(TaskInfoList) of
        0 ->
            ignore;
        1 ->
            sql_execute_with_log(Sqls);
        _ ->
            sql_execute_sqls(Sqls)
    end.

to_pb_home_task_info(DbData)->
    [TaskId, OwnerRoleId, Quality,Level,TGerTypeListStr,TimtoutTs
    ,Status,AcceptRoleId,GerId,GerType,GerQuality,GerLevel,TaskType,RewardNum,RoleLevel,BoxRewardBin] = DbData,
    GerTypeList = str_to_int_list(TGerTypeListStr),
    TNum = erlang:length(GerTypeList),
    #rolePublic{roleName=OwnerName} = role_lib:get_rolePublic(OwnerRoleId),
    AtkName = if
                  AcceptRoleId =:= 0 ->
                      [];
                  true ->
                      case role_lib:get_rolePublic(AcceptRoleId) of
                          #rolePublic{roleName=AtkName0} ->
                              AtkName0;
                          _ ->
                              []
                      end
              end,    
    #home_task{id=TaskId
              ,quality=Quality
              ,level=Level
              ,tgt_ger_type=GerTypeList
              ,timtout_ts=TimtoutTs
              ,finish_time=(2+TNum*2+Quality*2) *3600
              ,base_reward_num=RewardNum
              ,box_reward=to_term(BoxRewardBin)
              ,status=Status
              ,role_id=AcceptRoleId
              ,ger_id=str_to_int_list(GerId)
              ,ger_type=str_to_int_list(GerType)
              ,ger_quality=str_to_int_list(GerQuality)
              ,ger_level=str_to_int_list(GerLevel)
              ,onwer_role_id=OwnerRoleId
              ,task_type=TaskType
              ,role_level=RoleLevel
              ,owner_name=OwnerName
              ,atk_name=AtkName}.

% 固定以'_'为分隔符
str_to_int_list(Str) when erlang:is_binary(Str)->
    str_to_int_list(erlang:binary_to_list(Str));
str_to_int_list([])->
    [];
str_to_int_list(Str) when erlang:is_list(Str)->
    List0 = [string:to_integer(E)||E<-string:tokens(Str,"_")],
    [E||{E,_}<-List0].

% 固定以'_'为分隔符
int_list_to_str([])->
    "";
int_list_to_str(IntList)->
    string:join([erlang:integer_to_list(E)||E<-IntList],"_").

% 字符串装为建筑物数据结构，仅家园用
to_constr_list(ConstStr) when erlang:is_binary(ConstStr)->
    to_constr_list(erlang:binary_to_list(ConstStr));
to_constr_list(ConstStr) when erlang:is_list(ConstStr)->
    List1 = [E||E<-string:tokens(ConstStr,"_")],
    lists:reverse(to_constr_list(List1,[])).
to_constr_list([],AccList)->
    AccList;
to_constr_list([A|[B|Other]],AccList)->
    {Type,_} = string:to_integer(A),
    {Level,_} = string:to_integer(B),
    to_constr_list(Other,[{Type,Level}|AccList]).
    
constr_list_to_str(ConstrList)->
    L1 = lists:foldr(fun({Type,Level},AccStr)->
                    [["_"|erlang:integer_to_list(Type)]|[["_"|erlang:integer_to_list(Level)]|AccStr]]
                end, "", ConstrList),
    [_|L2] = lists:flatten(L1),
    L2.
    
get_trainer_info(RoleID) ->
    Sql = io_lib:format("select * from gTrainer where roleID = ~w;", [RoleID]),
    case get_row(Sql) of
        [RoleID, BinTalentList]->
            to_term(BinTalentList);
        _ ->
            []
    end.
set_trainer_info(RoleID,TalentList) ->
    BinTalentList = to_bin(TalentList),
    Sql = io_lib:format("replace into gTrainer values (~w,~s);"
                       ,[RoleID,BinTalentList]),
    sql_execute_with_log(Sql).

get_enargy_friend(RoleID)->
	Sql = io_lib:format("select * from gFriendEnargy where roleID = ~w", [RoleID]),
	case get_all(Sql) of
		[[_,{date,Date},GiveTimes]]->
            ToFriendList = get_role_to_friend_list(RoleID),
            ToMeList = get_role_to_me_list(RoleID),
            AddFriendList = get_role_add_friend_list(RoleID),
			#friend_enargy{roleID=RoleID,toFriendList=ToFriendList,toMeList=ToMeList,addFriendList=AddFriendList,date=Date,giveTimes=GiveTimes};
		_->
			#friend_enargy{roleID=RoleID,toFriendList=[],toMeList=[],addFriendList=[],date=date(),giveTimes=30}
	end.
set_enargy_friend(#friend_enargy{roleID=RoleID,toFriendList=ToFriendList,toMeList=ToMeList,addFriendList=AddFriendList,date=Date,giveTimes=GiveTimes})->
	Sql = io_lib:format("replace into gFriendEnargy values(~w,'~s',~w)", [RoleID,date(Date),GiveTimes]),
	sql_execute_with_log(Sql),
    set_role_to_friend_list(RoleID, ToFriendList),
    set_role_to_me_list(RoleID, ToMeList),
    set_role_add_friend_list(RoleID, AddFriendList).

get_role_to_friend_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp,isGive from gToFriend where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp,IsGive}||[FriendID,Timestamp,IsGive]<-List];
        _ ->
            []
    end.

set_role_to_friend_list(RoleID, ToFriendList) ->
    ArgList = [[RoleID,FriendID,Timestamp,IsGive]||{FriendID,Timestamp,IsGive}<-ToFriendList],
    DeleteSql = io_lib:format("delete from gToFriend where roleID=~w;", [RoleID]),
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql);
       true ->
           InsertSql = make_sql_batch("insert ignore into gToFriend values", "(~w,~w,~w,~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.

get_role_to_me_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp from gToMe where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp}||[FriendID,Timestamp]<-List];
        _ ->
            []
    end.

set_role_to_me_list(RoleID, ToMeList) ->
    ArgList = [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-ToMeList],
    DeleteSql = io_lib:format("delete from gToMe where roleID=~w;", [RoleID]),
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql);
       true ->
           InsertSql = make_sql_batch("insert into gToMe values", "(~w,~w,~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.

get_role_add_friend_list(RoleID) ->
    Sql = io_lib:format("select friendID,timestamp from gAddFriend where roleID=~w;",[RoleID]),
    case get_all(Sql) of
        [_|_]=List ->
                [{FriendID,Timestamp}||[FriendID,Timestamp]<-List];
        _ ->
            []
    end.

set_role_add_friend_list(RoleID, AddFriendList) ->
    ArgList = [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-AddFriendList],
    DeleteSql = io_lib:format("delete from gAddFriend where roleID=~w;", [RoleID]),
    if ArgList == [] ->
           sql_execute_with_log(DeleteSql);
       true ->
           InsertSql = make_sql_batch("insert into gAddFriend values", "(~w,~w,~w)", ArgList),
           sql_execute_with_log2(DeleteSql ++ InsertSql)
    end.

search_familyName(Name,IsFuzzySearch) ->
    case IsFuzzySearch of
        true->
            Sql = io_lib:format("select familyID from gFamily where familyName like '%~s%';",[quote2(Name)]),
            case get_rows(Sql) of
                FamilyID when is_list(FamilyID) ->
                    lists:append(FamilyID);
                _ ->
                    ?undefined
            end;
        false->
            Sql = io_lib:format("select familyID from gFamily where familyName = ~s;",[quote(Name)]),
            case get_row(Sql) of
                [FamilyID] ->
                    FamilyID;
                _ ->
                    ?undefined
            end
    end.


if_family_exist(FamilyID) ->
	Sql = io_lib:format("select count(*) from gFamily where familyID = ~w;", [FamilyID]),
	case get_row(Sql) of
		[0] ->
			false;
		_ ->
			true
	end.

if_role_exist(RoleID) ->
    Sql = io_lib:format("select count(*) from gRole where roleID = ~w;", [RoleID]),
    case get_row(Sql) of
        [0] ->
            false;
        _ ->
            true
    end.

get_family_name(FamilyID) ->
	Sql = io_lib:format("select familyName from gFamily where familyID = ~w;", [FamilyID]),
	case get_row(Sql) of
		[] ->
			"";
		[FamilyName] ->
			FamilyName
	end.

save_family_roomID(FamilyID,RoomID)->
	Sql = io_lib:format("update gFamily set talkRoomID = ~s where familyID = ~w",[RoomID,FamilyID]),
	sql_execute_with_log(Sql).


update_family_rank(FamilyID, Rank) ->
    sql_execute_with_log(io_lib:format("update gFamily set rank = ~w where familyID = ~w", [Rank, FamilyID])).

update_family_cross_rank(FamilyID, Rank) ->
    sql_execute_with_log(io_lib:format("update gFamily set cross_rank = ~w where familyID = ~w", [Rank, FamilyID])).
 
del_family_info(FamilyID) ->
    sql_execute_with_log(io_lib:format("delete from gFamily where familyID = ~w", [FamilyID])).

get_family_info(FamilyID) ->
    Sql = io_lib:format("select * from gFamily where FamilyID = ~w", [FamilyID]),
    case get_row(Sql) of
        [FamilyID,FamilyName,FamilyLevel,CreateRoleID,CreateRoleName,OwnerRoleID,OwnerRoleName,CurMembers,ActivePoints,FamilyScore,Notice,Slogan,Rank,
		 WorldRank,{date,IsRefresh},CreateTime,TalkBin,LogBin,ContributeLog,LeftChgNameTimes,TaskBin,CrossRank,TalkRoomID,BossInfoBin,FigherGroupBin
		,FamilyCrossInfoBin,FamilyAnubisInfoBin] ->
			Members = get_family_member_info(FamilyID),
        	Family_fight_other_info=get_family_fight_info(FamilyID),
                %% -1表示还没有赋值过
                case LeftChgNameTimes of
                    -1 ->
                        case data_change_name:get({change_cost, ?CHANGE_FAMILY_NAME}) of
                            undefined ->
                                LeftChgNameTimes2 = 0;
                            {FreeTimes,_,_} ->
                                LeftChgNameTimes2 = FreeTimes
                        end;
                    _ ->
                        LeftChgNameTimes2 = LeftChgNameTimes
                end,
            FamilyInstState0 = if
                           BossInfoBin =:= <<>> ->
                               family_data:init_instance_boss(CurMembers);
                           true ->
                               to_term(BossInfoBin,[])
                       end,
            FigherGroup = if
                           FigherGroupBin =:= <<>> ->
                               [];
                           true ->
                               to_term(FigherGroupBin,[])
                       end,
            %v2.1.0版本兼容v2.0.0版本旧数据
            ?INFO("FamilyInstState0 ~w  FigherGroup:~w",[FamilyInstState0,FigherGroup]),
            FamilyInstState = case FamilyInstState0 of
                                  #family_instance_state{} ->
                                      FamilyInstState0;
                                  {family_instance_state,IsWin,NextInstance,InstList
                                  ,CurInstBoss,FightMemberTimes,RewardGetStatus,ExtraRewardIsGet} ->
                                       {family_instance_state,IsWin,NextInstance,InstList
                                       ,CurInstBoss,FightMemberTimes,RewardGetStatus,ExtraRewardIsGet,[]}
                              end,
            TalkData0 = uncompress_decode(TalkBin,[]),
            TalkData = lists:foldr(fun
                                        ({sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1},AccTalkList)->
                                            Vip = role_lib:get_vip_level(RoleID),
                                            Grade = doublematch_server:dirty_get_role_dm_grade(RoleID),
                                            #rolePublic{level = Level} = role_lib:get_rolePublic(RoleID),
                                            [{sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip,Grade,Level}|AccTalkList];
                                        ({sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip},AccTalkList)->
                                            #rolePublic{level = Level} = role_lib:get_rolePublic(RoleID),
                                            Grade = doublematch_server:dirty_get_role_dm_grade(RoleID),
                                            [{sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip,Grade,Level}|AccTalkList];
                                        (TalkOne,AccTalkList)->
                                            [TalkOne|AccTalkList]
                                   end, [], TalkData0),
            FamilyAnubisInfo1 = case to_term(FamilyAnubisInfoBin) of []->#anubis_family_info{};X->X end, 
			FamilyCrossInfo2 = case to_term(FamilyCrossInfoBin) of [] -> #familycross_fight{}; DF -> DF end,
            #family_info{
                         family_id=FamilyID
                         ,family_name=FamilyName
                         ,level=FamilyLevel
                         ,create_role_id=CreateRoleID
                         ,create_role_name=CreateRoleName
                         ,owner_role_id=OwnerRoleID
                         ,owner_role_name=OwnerRoleName
                         ,cur_members=CurMembers
                         ,active_points=ActivePoints
						 ,family_score=FamilyScore
                         ,notice=Notice
						 ,slogan=Slogan
                         ,members=Members
                         ,rank=Rank
						 ,world_rank=WorldRank
						 ,is_zero_refreshed=IsRefresh
						 ,family_fight_other_info=Family_fight_other_info
                         ,create_time=CreateTime
                         ,talk_data=uncompress_decode(TalkBin,[])
						 ,log_data=to_term(LogBin,[])
						 ,contribute_log=to_term(ContributeLog,[])
                         ,leftChgNameTimes=LeftChgNameTimes2
                         ,family_task=to_term(TaskBin,[])
                         ,cross_rank=CrossRank
						 ,talkRoomID=TalkRoomID
                        ,family_instance_state=FamilyInstState
                        ,family_fighter_group=FigherGroup
						,familycross_fight_info=FamilyCrossInfo2
                        %%此处需要修改
                        ,family_anubisinfo=FamilyAnubisInfo1};
        _ ->
            ?undefined
    end.

set_family_info(FamilyInfo) ->
    #family_info{
                 family_id=FamilyID
                 ,family_name=FamilyName
                 ,level=FamilyLevel
                 ,create_role_id=CreateRoleID
                 ,create_role_name=CreateRoleName
                 ,owner_role_id=OwnerRoleID
                 ,owner_role_name=OwnerRoleName
                 ,cur_members=CurMembers
                 ,active_points=ActivePoints
				 ,family_score=FamilyScore
                 ,notice=Notice
				 ,slogan=Slogan
				 ,members=Members
				 ,rank=Rank
				 ,world_rank=WorldRank
				 ,is_zero_refreshed=IsRefresh
				 ,family_fight_other_info=Family_fight_other_info
				 ,create_time=CreateTime
				 ,talk_data=TalkData
				 ,log_data=LogData
				 ,contribute_log=ContributeLog
                 ,leftChgNameTimes=LeftChgNameTimes
                 ,family_task=TaskData
                 ,cross_rank=CrossRank
				,talkRoomID=TalkRoomID
                ,family_instance_state=BossInfo
                ,family_fighter_group=FigherGroup
				,familycross_fight_info=FamilyCrossInfo
                ,family_anubisinfo=FamilyAnubisInfo
                }=FamilyInfo,
    % ?ERR("FamilyAnubisInfo:~w~n",[FamilyAnubisInfo]),
	set_family_fight_info(Family_fight_other_info,FamilyID),
    sql_execute_with_log(io_lib:format("replace into gFamily values(~w,~s,~w,~w,~s,~w,~s,~w,~w,~w,~s,~s,~w,~w,'~s',~w,~s,~s,~s,~w,~s,~w,~s,~s,~s,~s,~s)",
                                       [FamilyID,quote(FamilyName),FamilyLevel,CreateRoleID,quote(CreateRoleName),OwnerRoleID,quote(OwnerRoleName),
                                        CurMembers,ActivePoints,FamilyScore,quote(Notice),quote(Slogan),
										Rank,WorldRank,date(IsRefresh),CreateTime,quote(compress_encode(TalkData)),to_bin(LogData),
										to_bin(ContributeLog),LeftChgNameTimes,to_bin(TaskData), CrossRank,quote(TalkRoomID),to_bin(BossInfo),to_bin(FigherGroup),to_bin(FamilyCrossInfo),to_bin(FamilyAnubisInfo)])),
    lists:foreach(fun(Member) -> set_family_member_info(Member) end, Members).

update_family_info(FamilyInfo)->
	    #family_info{
                 family_id=FamilyID
                 ,level=FamilyLevel
                 ,owner_role_id=OwnerRoleID
                 ,owner_role_name=OwnerRoleName
                 ,cur_members=CurMembers
                 ,active_points=ActivePoints
				 ,family_score=FamilyScore
                 ,notice=Notice
				 ,slogan=Slogan
				 ,members=Members
				 ,rank=Rank
				 ,world_rank=WorldRank
				 ,is_zero_refreshed=IsRefresh
				 ,family_fight_other_info=Family_fight_other_info
				 ,talk_data=TalkData
				 ,log_data=LogData
				 ,contribute_log=ContributeLog
                 ,leftChgNameTimes=LeftChgNameTimes
                 ,family_task=TaskData
                 ,cross_rank=CrossRank
				 ,talkRoomID=TalkRoomID
                 ,family_instance_state=BossInfo
                 ,family_fighter_group=FigherGroup
				 ,familycross_fight_info=FamilyCrossInfo
                 ,family_anubisinfo=FamilyAnubisInfo}=FamilyInfo,
		set_family_fight_info(Family_fight_other_info,FamilyID),
        Sql = io_lib:format("update gFamily set familyLevel= ~w,ownerRoleID= ~w,ownerRoleName= ~s,curMembers= ~w,activePoints= ~w,familyScore= ~w, notice= ~s, slogan= ~s,rank= ~w,worldRank= ~w,isZeroRefreshed='~s',talkData= ~s,changeLog= ~s,contributeLog= ~s,leftChgNameTimes= ~w, familyTask = ~s, cross_rank = ~w,talkRoomID=~s,boss_info=~s,fighter_group=~s,familycrossInfo=~s,familyanubisinfo=~s where familyID = ~w"
						   , [FamilyLevel,OwnerRoleID,quote(OwnerRoleName),CurMembers,ActivePoints,FamilyScore,quote(Notice),quote(Slogan),Rank,WorldRank,date(IsRefresh),quote(compress_encode(TalkData)),to_bin(LogData),to_bin(ContributeLog),LeftChgNameTimes,to_bin(TaskData),CrossRank,quote(TalkRoomID),to_bin(BossInfo),to_bin(FigherGroup),to_bin(FamilyCrossInfo),to_bin(FamilyAnubisInfo),FamilyID]),
		sql_execute_with_log(Sql),
		lists:foreach(fun(Member) -> set_family_member_info(Member) end, Members).

		
		
get_family_fight_info(FamilyID) ->
	Sql = io_lib:format("select * from gFamilyFight where familyID = ~w;", [FamilyID]),
	case get_row(Sql) of
		[FamilyID, WarPeriod, IsSign, AttackTimes,DefendTimes,WinStar,MatcherFamilyID,MatcherServerID,MatcherWinStar
		 ,MatcherWorldRank,MatcherFamilyName,Result, LastWorldRank]->
			#family_fight_other_info{win_star=WinStar
									 ,war_period=WarPeriod
									 ,is_sign=IsSign
									 ,attack_times=AttackTimes
									 ,defend_times=DefendTimes
									 ,last_world_rank=LastWorldRank
									 ,fight_result=Result
									 ,matcher_win_star=MatcherWinStar
									 ,matcher_server_id=MatcherServerID
									 ,matcher_family_id=MatcherFamilyID
									 ,matcher_family_name=MatcherFamilyName
									 ,matcher_family_rank=MatcherWorldRank};
		_ ->
			#family_fight_other_info{}
	end.

set_family_fight_info(Family_fight_other_info,FamilyID) ->
	#family_fight_other_info{				 war_period=WarPeriod
											 ,is_sign=IsSign
											 ,attack_times=AttackTimes
											 ,defend_times=DefendTimes
											 ,win_star=WinStar
											 ,last_world_rank=LastWorldRank
											 ,fight_result=FightResult
											 ,matcher_win_star=MatcherWinStar
											 ,matcher_server_id=MatcherServerID
											 ,matcher_family_id=MatcherFamilyID
											 ,matcher_family_name=MatcherFamilyName
											 ,matcher_family_rank=MatcherFamilyRank}=Family_fight_other_info,
	Sql = io_lib:format("replace into gFamilyFight values (~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~w,~w);"
					   ,[FamilyID,WarPeriod,IsSign,AttackTimes,DefendTimes,WinStar,MatcherFamilyID,MatcherServerID,MatcherWinStar,MatcherFamilyRank,quote(MatcherFamilyName),FightResult,LastWorldRank]),
	sql_execute_with_log(Sql).

get_growth_fund_record(RoleID) ->
    Sql = io_lib:format("select * from gGrowthFund where roleID = ~w;", [RoleID]),
    case get_row(Sql) of
        [RoleID, IsBuy, RewardRecBin]->
            #growth_fund_record{is_buy=IsBuy,reward_get_list=to_term(RewardRecBin)};
        _ ->
            #growth_fund_record{}
    end.

set_growth_fund_record(RoleID,#growth_fund_record{is_buy=IsBuy,reward_get_list=RewardRec}) ->
    if
        IsBuy =:= 0->
            ignore;
        true ->
            Sql = io_lib:format("replace into gGrowthFund values (~w,~w,~s);",[RoleID, IsBuy, to_bin(RewardRec)]),
            sql_execute_with_log(Sql)
    end.

get_family_member_info(FamilyID) ->
    case get_all(io_lib:format("select * from gFamilyMember where familyID = ~w", [FamilyID])) of
        List when erlang:is_list(List) ->
            [begin
				 LastContributeDate2 = 
					 case LastContributeDate of 
						 {0,0,0} ->
							 {1970,1,1};
						 _ ->
							 LastContributeDate
					 end,
				 StorageReqData2 = 
					 case to_term(StorageReqData) of
						[] ->
							{[],{{1970,1,1},0}};
						Value ->
							 Value
					 end,
				 LimitShop2 = 
					 case to_term(LimitShop) of
						 [] ->
							 family_server:init_family_limit_shop();
						 Value2 ->
							 Value2
					 end,
				MemberAnubis= 
					case to_term(LMemberAnubis) of
						[]->
							[];
						L ->
							family_server:transform_persist2representation(L)
					end,
				 #family_member_info{
                                 role_id=RoleID,
                                 role_name=RoleName,
                                 family_id=FamilyID,
                                 family_contribution=FamilyCon,
                                 left_family_contribution=LeftFamilyCon,
                                 use_gold_time=UseGoldTime,
                                 title=Title,
                                 is_male=int2bool(IsMale),
                                 online=false,
                                 role_level=RoleLevel,
                                 fight_power=FightPower,
                                 family_title=FamilyTitle,
								 is_join_war=IsJoinWar,
								 attack_times=AttackTimes,
								 defend_times=DefendTimes,
								 win_star=WinStar,
								 reward_level=RewardLevel,
                                 join_time=JoinTime,
								 weeklyContributes = WeeklyContributes,
								 lastContributeDate = LastContributeDate2,
								 recvEnergyList = to_term(RecvEnergyList),
								 storageReqData = StorageReqData2,
                                 head = Head,
                                 offline_time = OfflineTime,
								 limit_shop=LimitShop2,
								 anubisinfo=MemberAnubis,
                                 vip=Vip
                                }
                            end  ||[RoleID,RoleName,_FamilyID,FamilyCon,LeftFamilyCon,UseGoldTime,Title,IsMale,RoleLevel,FightPower,FamilyTitle,IsJoinWar,
								   AttackTimes,DefendTimes,WinStar,RewardLevel,JoinTime,WeeklyContributes,{date,LastContributeDate},RecvEnergyList
								   ,StorageReqData,Head,OfflineTime,LimitShop,LMemberAnubis,Vip]<-List];
        _ ->
            []
    end.

set_family_member_info(FamilyMemberInfo) ->
    #family_member_info{
                        role_id=RoleID,
                        role_name=RoleName,
                        family_id=FamilyID,
                        family_contribution=FamilyCon,
                        left_family_contribution=LeftFamilyCon,
                        use_gold_time=UseGoldTime,
                        title=Title,
                        is_male=IsMale,
                        role_level=RoleLevel,
						fight_power=FightPower,
						family_title=FamilyTitle,
						is_join_war=IsJoinWar,
						attack_times=AttackTimes,
						defend_times=DefendTimes,
						win_star=WinStar,
						reward_level=RewardLevel,
						join_time=JoinTime,
						weeklyContributes = WeeklyContributes,
						lastContributeDate = LastContributeDate,
						recvEnergyList = RecvEnergyList,
						storageReqData=StorageReqData,
                        head = Head,
                        offline_time = OfflineTime,
						limit_shop=LimitShop,
						anubisinfo=MemberAnubis,
                        vip=Vip
                       }=FamilyMemberInfo,
    LMemberAnubis = family_server:transform_representation2persist(MemberAnubis),
    Sql = io_lib:format("replace into gFamilyMember values(~w,~s,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,'~s',~s,~s,~w,~w,~s,~s,~w)"
					   , [RoleID,quote(RoleName),FamilyID,FamilyCon,LeftFamilyCon,UseGoldTime,Title,bool2int(IsMale),RoleLevel,FightPower,FamilyTitle
						 ,IsJoinWar,AttackTimes,DefendTimes,WinStar,RewardLevel,JoinTime,WeeklyContributes,date(LastContributeDate),to_bin(RecvEnergyList)
						 ,to_bin(StorageReqData),Head,OfflineTime,to_bin(LimitShop),to_bin(LMemberAnubis),Vip]),
    sql_execute_with_log(Sql).

del_family_member(FamilyID, RoleID) ->
    Sql = io_lib:format("delete from gFamilyMember where familyID = ~w and roleID = ~w", [FamilyID, RoleID]),
    sql_execute_with_log(Sql).

get_all_family_id()->
	case get_all("select familyID from gFamily") of
		List when is_list(List)->
			lists:flatten(List);
		_ ->
			[]
	end.

get_all_family_info() ->
    case get_all("select * from gFamily") of
		List when erlang:is_list(List) ->
			[begin
				 FamilyCrossInfo2 = case to_term(FamilyCrossFight) of [] -> #familycross_fight{}; X -> X end,
				 FamilyAnubisInfo1 = case to_term(FamilyAnubisInfoBin) of []->#anubis_family_info{};FAX->FAX end, 
			 #family_info{
						  family_id=FamilyID
						  ,family_name=FamilyName
						  ,level=FamilyLevel
						  ,create_role_id=CreateRoleID
						  ,create_role_name=CreateRoleName
						  ,owner_role_id=OwnerRoleID
						  ,owner_role_name=OwnerRoleName
						  ,cur_members=CurMembers
						  ,active_points=ActivePoints
						  ,family_score=FamilyScore
						  ,notice=Notice
						  ,slogan=Slogan
						  ,members=get_family_member_info(FamilyID)
						  ,rank=Rank
						  ,world_rank=WorldRank
						  ,is_zero_refreshed=IsRefresh
						  ,family_fight_other_info=get_family_fight_info(FamilyID)
						  ,create_time=CreateTime
						  ,talk_data=[]
						  ,log_data=to_term(ChangeLog)
						  ,contribute_log=to_term(ContributeLog)
                          ,leftChgNameTimes=LeftChgNameTimes
                          ,family_task=to_term(TaskData)
                          ,cross_rank=CrossRank
						  ,talkRoomID=TalkRoomID
                          ,family_instance_state=to_term(BossInfo)
                          ,family_fighter_group=FigherGroup
						  ,familycross_fight_info=FamilyCrossInfo2
						  ,family_anubisinfo=FamilyAnubisInfo1
						 }
						end ||[FamilyID,FamilyName,FamilyLevel,CreateRoleID,CreateRoleName,OwnerRoleID,OwnerRoleName,CurMembers
							,ActivePoints,FamilyScore,Notice,Slogan,Rank,WorldRank,{date,IsRefresh},CreateTime,_TalkBin,ChangeLog
						   ,ContributeLog,LeftChgNameTimes,TaskData,CrossRank,TalkRoomID,BossInfo,FigherGroup,FamilyCrossFight,FamilyAnubisInfoBin]<-List];
		_ ->
			[]
    end.

add_role_family_request(FamilyRequest) ->
    #family_request{
                    role_id=RoleID
                    ,role_name=RoleName
                    ,level=RoleLevel
                    ,fight_power=FightPower
                    ,timestamp=Timestamp
                    ,family_id=FamilyID
                    ,head=Head
                    ,title=Title
                    ,is_male=IsMale} = FamilyRequest,
    Sql = io_lib:format("replace into gFamilyRequest values(~w,~s,~w,~w,~w,~w,~w,~w,~w)", [RoleID,quote(RoleName),RoleLevel,FightPower,Timestamp,FamilyID,Head,Title,bool2int(IsMale)]),
    sql_execute_with_log(Sql).

del_role_family_request(RoleID) ->
    Sql = io_lib:format("delete from gFamilyRequest where roleID = ~w", [RoleID]),
    sql_execute_with_log(Sql).

del_family_request(FamilyID) ->
    Sql = io_lib:format("delete from gFamilyRequest where FamilyID = ~w", [FamilyID]),
    sql_execute_with_log(Sql).

del_spec_family_request(RoleID, FamilyID) ->
    Sql = io_lib:format("delete from gFamilyRequest where roleID = ~w and familyID = ~w", [RoleID, FamilyID]),
    sql_execute_with_log(Sql).

get_all_family_request() ->
    case get_all("select * from gFamilyRequest") of
        List when erlang:is_list(List) ->
            [
             #family_request{
                             role_id=RoleID
                             ,role_name=RoleName
                             ,level=RoleLevel
                             ,fight_power=FightPower
                             ,timestamp=Timestamp
                             ,family_id=FamilyID
                             ,head=Head
                            ,title=Title
                            ,is_male= int2bool(IsMale)}
                            ||[RoleID,RoleName,RoleLevel,FightPower,Timestamp,FamilyID,Head,Title,IsMale]<-List];
        _ ->
            []
    end.

get_role_send_energy(RoleID) ->
	Sql = io_lib:format("select lastSendEnergyDate,lastSendEnergyList from gSendEnergy where roleID = ~w;", [RoleID]),
	case get_row(Sql) of
		[{date, LastSendEnergyDate},LastSendEnergyList] ->
			case LastSendEnergyDate =:= erlang:date() of
				true ->
					to_term(LastSendEnergyList);
				_ ->
					[]
			end;
		_ ->
			[]
	end.

set_role_send_energy(RoleID,Today, SendEnergyList) ->
	Sql = io_lib:format("replace into gSendEnergy values (~w, '~s', ~s);", [RoleID, date(Today),to_bin(SendEnergyList)]),
	sql_execute_with_log(Sql).

get_role_tasklink(RoleID) ->
    Sql = io_lib:format("select * from gTasklink where roleID = ~w;", [RoleID]),
    case get_row(Sql) of
        [_,{date, LastDate},FreeTime,BuyLeftTime,PayCount] ->
            #role_tasklink{last_date=LastDate,free_time=FreeTime
                          ,buy_left_time=BuyLeftTime,pay_count=PayCount};
        _ ->
            #role_tasklink{last_date=erlang:date(),free_time=2,buy_left_time=0,pay_count=0}
    end.

set_role_tasklink(RoleID,RoleTasklink) ->
    Sql = io_lib:format("replace into gTasklink values (~w,'~s',~w,~w,~w);"
                       ,[RoleID
                        ,date(RoleTasklink#role_tasklink.last_date)
                        ,RoleTasklink#role_tasklink.free_time
                        ,RoleTasklink#role_tasklink.buy_left_time
                        ,RoleTasklink#role_tasklink.pay_count]),
    sql_execute_with_log(Sql).

get_dojangrank_rank(RankType) ->
    Sql = io_lib:format("select * from gDojangrankRank where rank_index = ~w;", [RankType]),
    case get_row(Sql) of
        [RankType,RankListBin] ->
            lists:map(fun(DojangRank)->
                              if
                                  DojangRank#dojangrank.selected_ger_type =:= 0 ->
                                      DojangRank#dojangrank{selected_ger_type = lists:nth(RankType, data_dojangrank:get(default)) * 256 + 1};
                                  DojangRank#dojangrank.selected_ger_type < 256000 ->
                                      DojangRank#dojangrank{selected_ger_type = lists:nth(RankType, data_dojangrank:get(default)) * 256 + 1};
                                  true ->
                                      DojangRank
                              end
                      end, uncompress_decode(to_term(RankListBin)));
        _ ->
            []
    end.

set_dojangrank_rank(RankType,RankList) ->
    Sql = io_lib:format("replace into gDojangrankRank values (~w,~s);"
                       ,[RankType
                        ,to_bin(compress_encode(RankList))]),
    sql_execute_with_log(Sql).

insert_new_dojangrank_fightrec(RoleID,TarRoleID,AttackerNewRank,DefenderNewRank,IsWinNum,Replay,AtkName,DefName,RankType)->
    Now = util:now(),
    Sql = io_lib:format("insert into gdojangrankfightrecord (roleID,tarRoleID,attackerNewRank,defenderNewRank,isWin,replay,fTime,roleName,tarRoleName,rank_type) value (~w,~w,~w,~w,~w,~s,'~s','~s','~s',~w);"
                       ,[RoleID,TarRoleID,AttackerNewRank,DefenderNewRank,IsWinNum
                        ,to_bin(compress_encode(Replay))
                        ,datetime(util:seconds_to_datetime(Now))
                        ,AtkName,DefName,RankType]),
    case db_sql:sql_execute_with_log(Sql) of
        {ok,RecordUid} ->
            RecordUid;
        _ ->
            ?undefined
    end.

get_dojangrank_fightrec(RecordUid) ->
    Sql = io_lib:format("select replay from gdojangrankfightrecord where recordUID = ~w", [RecordUid]),
    case get_row(Sql) of
        [Bin]->
            uncompress_decode(to_term(Bin));
        _ ->
            []
    end.

get_dojangrank_fightrec_by_roleid(RoleID) ->
    Sql = io_lib:format("select * from gdojangrankfightrecord where roleID = ~w or tarRoleID = ~w order by 'fTime' desc limit ~w"
                       , [RoleID,RoleID,20]),
    lists:map(fun([RecordUid,AtkRoleID,DefRoleID,AttackerNewRank,DefenderNewRank,IsWinNum,_ReplayBin,FTime,RoleName,TarRoleName,RankType])->
                #p_dojang_replay_info{attackerName = RoleName
                                     ,defenderName = TarRoleName
                                     ,attackerNewRank = AttackerNewRank
                                     ,defenderNewRank = DefenderNewRank
                                     ,replayUID = RecordUid
                                     ,time = util:toUnixTime(FTime)
                                     ,rank_type = RankType
                                     ,is_win = IsWinNum}
            end, get_rows(Sql)).

insert_new_dojangrank_world_fightrec(RoleID,TarRoleID,ServerID,TarServerID,IsWinNum,Replay,AttackerInfo,DefenderInfo,RankType)->
    Now = util:now(),
    Sql = io_lib:format("insert into gdojangrank_world_fightrecord (roleID,tarRoleID,serverID,tarServerID,attackerNewRank,defenderNewRank,isWin,replay,fTime,roleInfo,tarRoleInfo,rank_type) value (~w,~w,~w,~w,~w,~w,~w,~s,'~s',~s,~s,~w);"
                       ,[RoleID,TarRoleID,ServerID,TarServerID,AttackerInfo#p_dr_world_rank.rank,DefenderInfo#p_dr_world_rank.rank,IsWinNum
                        ,to_bin(compress_encode(Replay))
                        ,datetime(util:seconds_to_datetime(Now))
                        ,to_bin(AttackerInfo),to_bin(DefenderInfo),RankType]),
    case db_sql:sql_execute_with_log(Sql) of
        {ok,RecordUid} ->
            RecordUid;
        _ ->
            ?undefined
    end.

get_dojangrank_world_fightrec(RecordUid) ->
    Sql = io_lib:format("select replay from gdojangrank_world_fightrecord where recordUID = ~w", [RecordUid]),
    case get_row(Sql) of
        [Bin]->
            uncompress_decode(to_term(Bin));
        _ ->
            []
    end.

get_dojangrank_world_fightrec_by_roleid(RoleID) ->
    {YY,MM,_} = date(),
    TimeLimit = util:datetime_to_seconds({{YY,MM,1},{0,0,0}}),
    lists:map(fun(RankIndex)->
            Sql = io_lib:format("select * from gdojangrank_world_fightrecord where 'fTime' > ~w and rank_type = ~w and (roleID = ~w or tarRoleID = ~w) order by 'fTime' desc limit ~w"
                               , [TimeLimit,RankIndex,RoleID,RoleID,10]),
            lists:map(fun([RecordUid,AtkRoleID,DefRoleID,_ServerID,_TarServerID,_AttackerNewRank,_DefenderNewRank,IsWinNum,_ReplayBin,FTime,AttackerInfoBin,DefenderInfoBin,RankType])->
                            AttackerInfo = fix_format_p_dr_world_rank(to_term(AttackerInfoBin)),
                            DefenderInfo = fix_format_p_dr_world_rank(to_term(DefenderInfoBin)),      
                            #p_dr_dojang_replay_info{attacker_info = AttackerInfo
                                                    ,defender_info = DefenderInfo
                                                    ,replayUID = RecordUid
                                                    ,time = util:toUnixTime(FTime)
                                                    ,rank_type = RankType
                                                    ,is_win = IsWinNum}
                    end, get_rows(Sql))
        end, lists:seq(1, 8)).


get_dojangrank_world_fightrec_group() ->
    {YY,MM,_} = date(),
    TimeLimit = util:datetime_to_seconds({{YY,MM,1},{0,0,0}}),
    SqlVersion = io_lib:format("select max(recordUid) from gdojangrank_world_fightrecord ", []),
    case get_row(SqlVersion) of
        [Version0] when erlang:is_integer(Version0)->
            Version = Version0;
        _ ->
            Version = 0
    end,
    {Version
    ,lists:map(fun(RankIndex)->
            Sql = io_lib:format("select * from gdojangrank_world_fightrecord where 'fTime' > ~w and rank_type = ~w order by 'recordUid' desc limit ~w"
                               , [TimeLimit,RankIndex,20]),
            lists:map(fun([RecordUid,AtkRoleID,DefRoleID,_ServerID,_TarServerID,_AttackerNewRank,_DefenderNewRank,IsWinNum,ReplayBin,FTime,AttackerInfoBin,DefenderInfoBin,RankType])->
                        {RecordUid,RankType
                        ,#p_dr_dojang_replay_info{attacker_info = fix_format_p_dr_world_rank(to_term(AttackerInfoBin))
                                                ,defender_info = fix_format_p_dr_world_rank(to_term(DefenderInfoBin))
                                                ,replayUID = RecordUid
                                                ,time = util:toUnixTime(FTime)
                                                ,rank_type = RankType
                                                ,is_win = IsWinNum}
                        ,uncompress_decode(to_term(ReplayBin))}
                    end, get_rows(Sql))
        end, lists:seq(1, 8))}.

fix_format_p_dr_world_rank(FighterInfo) when erlang:is_record(FighterInfo, p_dr_world_rank)->
    FighterInfo;
fix_format_p_dr_world_rank(ElementTuple) ->
    ElementList = erlang:tuple_to_list(ElementTuple),
    FighterInfo = 
        case erlang:length(ElementList) of
            17 -> erlang:list_to_tuple(lists:reverse([data_dojangrank:get(init_score)|lists:reverse(ElementList)]))
        end,
    NewScore = dojangrank_world_server:get_new_score_by_rank(FighterInfo#p_dr_world_rank.rank),
    FighterInfo#p_dr_world_rank{rank_score = NewScore}.

clear_dojangrank_world_fightrec_group()->
    Sql = io_lib:format("delete from gdojangrank_world_fightrecord where recordUid > 0",[]),
    sql_execute_with_log(Sql).

clear_dojangrank_world_rank(Type)->
    Sql = io_lib:format("delete from gdojangrank_world_rank_~w where rank > 0",[Type]),
    sql_execute_with_log(Sql).

persist_dojangrank_world_rank(_,_,_,[])->
    ok;
persist_dojangrank_world_rank(Type,Count,Max,AllList)->
    ?ERR("persist_dojangrank_world_rank ~w",[[Type,Count,Max]]),
    {SubList,OtherList} = 
        if
            Count =< Max ->
                {AllList,[]};
            true ->
                lists:split(Count, AllList)
        end,
    InsertData0 = 
        lists:foldr(fun(Drw,AccStr)->
                io_lib:format(",(~w,~w,~w,~s,~s)", Drw) ++ AccStr
            end, "", SubList),
    [_|InsertData] = InsertData0, %% 删除掉第一个逗号
    Sql = io_lib:format("replace into gdojangrank_world_rank_~w (rank,roleID,serverID,dr_info_bin,fighter_data_bin) values ",[Type]) ++ InsertData,
    StartTs = util:now_mili(),
    Res = sql_execute_with_log(Sql),
    EndTs = util:now_mili(),
    case Res of
        {ok,_} -> ?INFO("persist_dojangrank_world_rank finish(~w) ~w",[EndTs - StartTs,min(Count,Max)]);
        _ -> ?ERR("persist_dojangrank_world_rank fail")
    end,
    persist_dojangrank_world_rank(Type,Count - Max,Max,OtherList).

persist_dojangrank_world_rank_single(Type,SubList)->
    AllList = 
        lists:foldl(fun([DrwData],AccList) when erlang:is_record(DrwData, drw_rank_data)->
                            [[DrwData#drw_rank_data.rank
                            ,DrwData#drw_rank_data.role_id
                            ,DrwData#drw_rank_data.server_id 
                            ,to_bin(DrwData#drw_rank_data.dr_info)
                            ,to_bin(DrwData#drw_rank_data.fighter_data)]|AccList]
            end, [], SubList),    
    InsertData0 = 
        lists:foldr(fun(Drw,AccStr)->
                io_lib:format(",(~w,~w,~w,~s,~s)", Drw) ++ AccStr
            end, "", AllList),
    [_|InsertData] = InsertData0, %% 删除掉第一个逗号
    Sql = io_lib:format("replace into gdojangrank_world_rank_~w (rank,roleID,serverID,dr_info_bin,fighter_data_bin) values ",[Type]) ++ InsertData,
    StartTs = util:now_mili(),
    Res = sql_execute_with_log(Sql),
    EndTs = util:now_mili(),
    case Res of
        {ok,_} -> 
            ?INFO("persist_dojangrank_world_rank finish(~w)-~w--~w-",[EndTs - StartTs
                                                                 ,Type,erlang:length(SubList)]);
        _ -> 
            ?ERR("persist_dojangrank_world_rank fail")
    end.

get_dojangrank_world_rank(Type)->
    Sql = io_lib:format("select * from gdojangrank_world_rank_~w ", [Type]),
    RankList = get_rows(Sql),
    lists:map(fun([Rank,RoleId,ServerId,DrInfoBin,FighterDatabin])->
                    #drw_rank_data{rank = Rank
                                  ,role_id = RoleId
                                  ,server_id = ServerId
                                  ,dr_info = fix_format_p_dr_world_rank(to_term(DrInfoBin))
                                  ,fighter_data = to_term(FighterDatabin)}
                end, RankList).


set_dojangrank_world_history(RankType,RankListBin)->
    Sql = io_lib:format("replace into gdojangrank_world_rank_history values (~w, ~s)"
                       ,[RankType, to_bin(RankListBin)]),
    sql_execute_with_log(Sql).

get_dojangrank_world_history(RankType)->
    Sql = io_lib:format("select * from gdojangrank_world_rank_history where rank_type = ~w", [RankType]),
    case get_row(Sql) of
        [RankType,RankListBin] ->
            to_term(RankListBin);
        Err ->
            ?ERR("get_dojangrank_world_history read fail: ~w",[Err]),
            compress_encode([])
    end.

set_familyfight_record(ReplayUID,FightRecord,_RecordType,DefendRoleID,DefendFamilyID,DefendRoleName
						,AttackRoleID,AttackFamilyID,AttackRoleName,WinFamilyName,TimeStamp,IsChallenger,IsWin,WinStar,Period)->
	case  IsChallenger of
		1 ->
			FID1 = AttackFamilyID,
			FID2 = DefendFamilyID,
			RID1 = AttackRoleID,
			RID2 = DefendRoleID,
			RNM1 = AttackRoleName,
			RNM2 = DefendRoleName;
		_ ->
			FID1 = DefendFamilyID,
			FID2 = AttackFamilyID,
			RID1 = DefendRoleID,
			RID2 = AttackRoleID,
			RNM1 = DefendRoleName,
			RNM2 = AttackRoleName
	end,
			
	Sql = io_lib:format("insert into gFamilyFightRecord values (~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,'~s',~s,~s,~s);"
					   ,[ReplayUID, FID1,FID2,RID1,RID2,Period,IsChallenger,IsWin,WinStar,quote(compress_encode(FightRecord)),datetime(util:seconds_to_datetime(TimeStamp)),quote(WinFamilyName),quote(RNM1),quote(RNM2)]),
	sql_execute_with_log(Sql).

get_familyfight_record(UID) ->
	Sql = io_lib:format("select replay from gFamilyFightRecord where recordUID = ~w", [UID]),
	ScFightRequest = case db_sql:get_row(Sql) of
%%		[_,FamilyID, TarFamilyID, RoleID, TarRoleID, WarPeriod, IsChallenger,IsWin,WinStar,Replay, FTime,WinFamily,RoleName,TarRoleName] ->
%% 		[ReplayUID, FID1,FID2,RID1,RID2,Period,IsChallenger,IsWin,WinStar,FightRecord,{datetime,TimeStamp),WinFamilyName,RNM1,RNM2] ->
%% 			[ReplayUID, FID1,FID2,RID1,RID2,Period,IsChallenger,IsWin,WinStar,uncompress_decode(FightRecord),util:datetime_to_seconds(TimeStamp),WinFamilyName,RNM1,RNM2];
		[Bin]->
			uncompress_decode(Bin);
		_ ->
			[]
	end,
    NewFighterList = lists:foldr(fun(Pfighter,AccList)->
                            Pfighter2 = case Pfighter of
                                % 这是旧协议保存的数据，需要转化为新格式
                                {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality} ->
                                    {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,0,[],0};
                                {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone}->
                                	{p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone,[],0};
                                {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone,_}->
                                    {p_fighter,GerID,GerTypeID,GerPos,GerHp,GerHpMax,GerSp,GerQuality,Stone,[],0};
                                Pfighter ->
                                    Pfighter
                            end,
                            [Pfighter2|AccList]
                end, [], ScFightRequest#sc_fight_request.fighterList),
    ScFightRequest#sc_fight_request{fighterList = NewFighterList}.
			
get_family_fight_record_summary(FamilyID,Period)->
	Sql = io_lib:format("select recordUID,isWin,winStar,roleID,tarRoleID,isChallenger,winFamily,roleName,tarRoleName from gFamilyFightRecord where familyID=~w and warPeriod=~w;",[FamilyID,Period]),
	case get_all(Sql) of
		List when is_list(List) ->
			[begin 
				 if IsChallenger == 1 ->
						#p_familyfight_record_info{win_star=WinStar
												   ,result=IsWin
												   ,attackerName = RoleName
												   ,defenderName = TarRoleName
												   ,winFamilyName = WinFamilyName
												   ,recordUID = RecordUID
												   ,attackerRoleID = RoleID
												   ,defenderRoleID = TarRoleID};
					true ->
						#p_familyfight_record_info{win_star=WinStar
												   ,result=IsWin
												   ,attackerName = TarRoleName
												   ,defenderName = RoleName
												   ,winFamilyName = WinFamilyName
												   ,recordUID = RecordUID
												   ,attackerRoleID = TarRoleID
												   ,defenderRoleID = RoleID}
				 end
			 end ||[RecordUID,IsWin,WinStar,RoleID,TarRoleID,IsChallenger,WinFamilyName,RoleName,TarRoleName]<-List];
		_ ->
			[]
	end.

%% 返回[#pf_fighter{}]
get_family_fight_master_rank_info()->
	case get_all("select serverID from gFamilyMasterRecord group by serverID") of
		List when is_list(List) ->
			lists:foldl(fun([E],Acc)->
								get_family_fight_master_rank_info(E) ++ Acc
								end, [], List);
		_ ->
			[]
	end.

get_family_fight_master_rank_info(ServerID)->
	case get_all(io_lib:format("select * from gFamilyMasterRecord where serverID = ~w;",[ServerID])) of
		List when is_list(List) ->
			[#pf_fighter{familyID=FamilyID
						 ,serverID=ServerIDF
						 ,score=Score
						 ,pareID=0
						 ,result=0
						 ,star=0
						 ,matchFamilyID=0
						 ,matchServerID=0
						 ,matchStar=0
						 ,rank=0
						 ,lastRank=Rank
                         ,totalFightPower=TotalFightPower}
						||[FamilyID,ServerIDF,Score,Rank,_LastRank,TotalFightPower]<-List];
		_ ->
			[]
	end.

set_family_fight_master_rank_info(List) ->
	ArgList = [ [FamilyID,ServerID,Score,Rank,LastRank,TotalFightPower]
				||#pf_fighter{familyID=FamilyID
							  ,serverID=ServerID
							  ,score=Score
							  ,rank=Rank
							  ,lastRank=LastRank
                              ,totalFightPower = TotalFightPower}<-List],
	make_sql_batch_by_piece("replace into gFamilyMasterRecord values", "(~w,~w,~w,~w,~w,~w)", ArgList, 1000).
get_roleInfo_by_accid(AccID)->
	Sql = io_lib:format("select roleID,roleName from gRole where accid = ~w;",[AccID]),
	case get_row(Sql) of
		[_|_] = List ->
			List;
		_ ->
			["",""]
	end.

clean_family_fight_master_info(FamilyID)->
	Sql = io_lib:format("delete from gFamilyMasterRecord where familyID = ~w",[FamilyID]),
	sql_execute_with_log(Sql).

get_family_wallet(FamilyID,TekID)->
	Sql = io_lib:format("select familySource from gFamilyWallet where familyID = ~w and familyTekID = ~w", [FamilyID,TekID]),
	case db_sql:get_row(Sql) of
		[Bin]->
			uncompress_decode(Bin);
		_ ->
			#sc_familyTek_wallet{wallet= #p_reward_info2{coin =0,roleExp=0,gerExp=0,gold = 0,itemList=[],reputation=0,gerList=[]}}
	end.
set_family_wallet(FamilyID,TekID,Wallet)->
   	Bin=compress_encode(Wallet),
	Sql = io_lib:format("replace into gFamilyWallet values (~w,~s,~w)", [FamilyID,quote(Bin),TekID]),
    sql_execute_with_log(Sql).

get_family_storage(FamilyID)->
	Sql = io_lib:format("select itemUID,itemTypeID,type,itemReqBin from gFamilyStorage where familyID = ~w;",[FamilyID]),
	case get_all(Sql) of
		[_|_] = List ->
			[#p_family_storage{itemUID=ItemUID,itemTypeID=ItemTypeID,type=Type,reqRoleIDList=to_term(ItemReqBin)}||[ItemUID,ItemTypeID,Type,ItemReqBin]<-List];
		_ ->
			[]
		  end.
set_family_storage(FamilyID,{_,Storage})->
	ArgList = [[ItemUID,FamilyID,ItemTypeID,Type,to_bin(ReqList)]
			   ||#p_family_storage{itemUID=ItemUID,itemTypeID=ItemTypeID,type=Type,reqRoleIDList=ReqList} <-Storage],
	DeleteSql = io_lib:format("delete from gFamilyStorage where familyID = ~w;", [FamilyID]),
	case ArgList of
		[] ->
			sql_execute_with_log(DeleteSql);
		_ ->
			InsertSql = make_sql_batch("insert into gFamilyStorage values", "(~w,~w,~w,~w,~s)",ArgList),
			sql_execute_with_log2(DeleteSql++InsertSql)
	end.


get_family_tek(FamilyID)->
	Sql = io_lib:format("select level1,level2,level3,level4,level5,level6,level7,level8,level9,level10,level11,level12,level13,level14,level15,level16,level17,level18,level19,level20,level21,level22,level23,level24,level25,level26,level27,level28,level29 from gFamilyTek where familyID = ~w",[FamilyID]),
	case get_row(Sql) of
		[_|_] = LevelList ->
		    % ?ERR("Tek:~w",[LevelList]),
			IDList = data_family:get(tekIDList),
			[begin
					 TekID = lists:nth(N,IDList),
					 Level = lists:nth(TekID rem 100,LevelList),
					 %%科技Key以  科技ID*1000+Level的方式；
					 ID = TekID*1000+Level,
					 {Type,_,Level2,_,_,_} = get_family_tek_add(ID,Level),
					 %?ERR("ID :~w Type :~w",[ID,Type]),
					 #sc_familyTek_wallet{wallet= Wallet} = get_family_wallet(FamilyID,ID),

					 #p_familyTekDtl{tekID=ID,tekType=Type,tekLevel=Level2,tekWalletinfo = Wallet,tekFinish = 0}
			 end||N<- lists:seq(1,length(IDList))];
		_ ->
			family_data:default_family_tek()
	end.

get_family_contribute_list(FamilyID) ->
	Sql = io_lib:format("select roleID,diamondNum,coinNum,reputationNum,gerInfo,itemInfo,donateContribution from gFamilyDonate where familyID =~w",[FamilyID]),
	case get_all(Sql) of
		[_|_] = List ->
			[#family_contribution{role_id=RoleID,diamondNum=DiamondNum,coinNum=CoinNum,reputationNum=ReputationNum,gerdonateinfo=to_term(GerInfo),itemdonateinfo=to_term(ItemInfo),donateContribution=DonateContribution}||[RoleID,DiamondNum,CoinNum,ReputationNum,GerInfo,ItemInfo,DonateContribution]<-List];
		_ ->
			[]
	end.

set_family_contribute_list(FamilyID,Family_contribution_list)->
	ArgList = [[FamilyID,RoleID,DiamondNum,CoinNum,ReputationNum,to_bin(GerInfo),to_bin(ItemInfo),DonateContribution]||#family_contribution{role_id=RoleID,diamondNum=DiamondNum,coinNum=CoinNum,reputationNum=ReputationNum,gerdonateinfo=GerInfo,itemdonateinfo=ItemInfo,donateContribution=DonateContribution}<-Family_contribution_list],
	DeleteSql = io_lib:format("delete from gFamilyDonate where familyID = ~w;", [FamilyID]),
	case ArgList of
		[] ->
			sql_execute_with_log(DeleteSql);
		_ ->
			InsertSql = make_sql_batch("insert into gFamilyDonate values", "(~w,~w,~w,~w,~w,~s,~s,~w)",ArgList),
			sql_execute_with_log2(DeleteSql++InsertSql)
	end.

get_family_tek_add(ID,_Level)->
	data_family_technology:get({data_technology_rank,ID}).
%% 	{_,Add,_} = lists:keyfind(Level, 1, data_family:get({tekAttrInfo,ID})),
%% 	Add.

set_family_tek(FamilyID,Tek)->
	ArgList2 = [{TekID,Level}||#p_familyTekDtl{tekLevel=Level,tekID=TekID}<-lists:keysort(#p_familyTekDtl.tekID, Tek)],
	ArgList = [findLevel(N,ArgList2)||N<- lists:seq(1,29)],
	Sql = io_lib:format("replace into gFamilyTek values (~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w)", [FamilyID|ArgList]),
	sql_execute_with_log(Sql).

findLevel(N,ArgList) ->
	case lists:keyfind(N,1,[{(TekID div 1000) rem 100,Level}||{TekID,Level}<-ArgList]) of
		{_Pos,FindLevel}->
			FindLevel;
		false ->
			0
	end.

get_family_tekAdd(RoleID)->
	Sql = io_lib:format("select familyTekAtkAdd,familyTekHpAdd from gFighterList where roleID=~w;", [RoleID]),
	case get_row(Sql) of
		[FamilyTekAtkAdd,FamilyTekHpAdd] ->
			{FamilyTekAtkAdd,FamilyTekHpAdd};
		_ ->
			{0,0}
	end.

set_family_tekAdd(RoleID,Tek)->
%	Kingdom = get_kingdom(RoleID),
	Kingdom = 0,
	{_,AtkID,HpID} = lists:keyfind(Kingdom, 1, data_family:get(tekSetting)),
	{AtkAdd,HpAdd} = get_family_tekAdd(RoleID),
	Sql = io_lib:format("update gFighterList  set familyTekAtkAdd = ~w ,familyTekHpAdd = ~w where roleID = ~w;",[role_data:get_tekAdd(AtkID,Tek,AtkAdd),role_data:get_tekAdd(HpID,Tek,HpAdd),RoleID]),
	sql_execute_with_log(Sql).


set_family_boss_info(FamilyID,Info)->
	Bin=compress_encode(Info),
	Sql = io_lib:format("replace into gFamilyBoss values (~w,~s)", [FamilyID,quote(Bin)]),
	sql_execute_with_log(Sql).

get_family_boss_info(FamilyID)->
	Sql = io_lib:format("select value from gFamilyBoss where familyID = ~w;", [FamilyID]),
	case get_row(Sql) of
		[Bin] ->
			uncompress_decode(Bin,[]);
		_ ->
			{[],[]}
	end.
	
get_family_owner_name_level(FamilyID)->
	Sql = io_lib:format("select familyLevel,ownerRoleName from gFamily where familyID = ~w ;",[FamilyID]),
	case get_row(Sql) of
		[FamilyName,OwnerName] ->
			{FamilyName,OwnerName};
		_ ->
			{"",""}
	end.

%get_role_free_box_info(RoleID)->
%	Sql = io_lib:format("select lastGerTime,lastItemTime,lastTrainerTime from gBoxInfo where roleID = ~w",[RoleID]),
%	case get_row(Sql) of
%		[LastGerTime,LastItemTime,LastTrainerTime] when LastTrainerTime =:= 0 -> % 针对版本更新1.4.0->1.4.5
%            Now=util:now(),
%            {_, _,TrainerFree} = data_box:get(box_free_init),
%            Interval = data_box:get(box_free_interval),
%			#box_open_info{lastGerTime=LastGerTime, lastItemTime=LastItemTime,lastTrainerTime=Now-TrainerFree-Interval};
%        [LastGerTime,LastItemTime,LastTrainerTime] ->
%            #box_open_info{lastGerTime=LastGerTime, lastItemTime=LastItemTime,lastTrainerTime=LastTrainerTime};
%		_ ->
%			Now=util:now(),
%			{GerFree, ItemFree,TrainerFree} = data_box:get(box_free_init),
%			Interval = data_box:get(box_free_interval),
%			#box_open_info{lastGerTime=Now-GerFree-Interval, lastItemTime=Now-ItemFree,lastTrainerTime=Now-TrainerFree-Interval}
%	end.
%
%set_role_free_box_info(RoleID,#box_open_info{lastGerTime=LastGerTime,lastItemTime=LastItemTime,lastTrainerTime=LastTrainerTime}) ->
%	Sql = io_lib:format("replace into gBoxInfo values (~w,~w,~w,~w);",[RoleID, LastGerTime,LastItemTime,LastTrainerTime]),
%	sql_execute_with_log(Sql).
get_role_free_box_info(RoleID)->
    Sql = io_lib:format("select gerTime1,gerTime2,itemTime1,itemTime2,trainerTime1,trainerTime2 from gBoxInfo where roleID = ~w",[RoleID]),
	case get_row(Sql) of
        [GT1,GT2,IT1,IT2,TT1,TT2] ->
            #box_open_info{gerTime1=GT1,gerTime2=GT2,itemTime1=IT1,itemTime2=IT2,trainerTime1=TT1,trainerTime2=TT2};
		_ ->
            #box_open_info{}
	end.

set_role_free_box_info(RoleID,#box_open_info{gerTime1=GT1,gerTime2=GT2,itemTime1=IT1,itemTime2=IT2,trainerTime1=TT1,trainerTime2=TT2}) ->
    Sql = io_lib:format("replace into gBoxInfo values (~w,~w,~w,~w,~w,~w,~w)",
                       [RoleID,GT1,GT2,IT1,IT2,TT1,TT2]),
	sql_execute_with_log(Sql).

get_role_monthVIP_Info(RoleID,LastLogoutTime) ->
	Sql = io_lib:format("select buyBigSec,buyLittleSec,lastGetBigSec,lastGetLittleSec,restBigDays,restLittleDays,todayPayBig,todayPayLittle from gMonthVIP where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[BuyBigSec,BuyLittleSec,LastGetBigSec,LastGetLittleSec, RestBigDays,RestLittleDays,TodayPayBig,TodayPayLittle] ->
			Now = erlang:localtime(),
			LogoutTime = util:seconds_to_datetime(LastLogoutTime),
			PassDays = get_pass_days(LogoutTime,Now),
			RestBigDays1 = calc_biggest_num(RestBigDays,PassDays),
			RestLittleDays1 = calc_biggest_num(RestLittleDays, PassDays),
			{TodayPayBig2,TodayPayLittle2} = 
				if PassDays > 0 ->
					   {0,0};
				   true ->
					   {TodayPayBig,TodayPayLittle}
				end,
			#monthVIP_info{buyBigSec=BuyBigSec,buyLittleSec=BuyLittleSec,lastGetBigSec=LastGetBigSec
						  ,lastGetLittleSec=LastGetLittleSec,restBigDays=RestBigDays1,restLittleDays=RestLittleDays1
						  ,todayPayBig=TodayPayBig2,todayPayLittle=TodayPayLittle2};
		_ ->
			#monthVIP_info{}
	end.

get_pass_days({D1,_},{D1,_}) ->
	0;
get_pass_days({_,T1}=LogoutTime,{_,T2}=Now) ->
	{PassDays,_} = calendar:time_difference(LogoutTime,Now),
	case T1 > T2 of
		true ->
			PassDays + 1;
		_ ->
			PassDays
	end.

calc_biggest_num(Num1,Num2) ->
	Num3 = Num1-Num2,
	if Num3 > 0 ->
		   Num3;
	   true ->
		   0
	end.

set_role_monthVIP_info(RoleID,#monthVIP_info{buyBigSec=BuyBigSec,buyLittleSec=BuyLittleSec
											,lastGetBigSec=LastGetBigSec,lastGetLittleSec=LastGetLittleSec
											,restBigDays=RestBigDays, restLittleDays=RestLittleDays
											,todayPayBig = TodayPayBig,todayPayLittle=TodayPayLittle}) ->
	Sql = io_lib:format("replace into gMonthVIP values (~w,~w,~w,~w,~w,~w,~w,~w,~w)"
					   ,[RoleID,BuyBigSec,BuyLittleSec,LastGetBigSec,LastGetLittleSec,RestBigDays,RestLittleDays,TodayPayBig,TodayPayLittle]),
	sql_execute_with_log(Sql).

get_relic_data(WARID) ->
	Sql = io_lib:format("select war_data from gRelicWar where warID = ~w",[WARID]),
	case get_row(Sql) of
		[Bin] ->
			uncompress_decode(Bin,[]);
		_ ->
			[]
	end.

set_relic_data(WARID,Data) ->
	Bin = compress_encode(Data),
	Sql = io_lib:format("replace into gRelicWar values (~w,~s);",[WARID, quote(Bin)]),
	sql_execute_with_log(Sql).

get_dojang_data(RoleID) ->
    Sql = io_lib:format("select dojang_data from gDojang where roleID = ~w",[RoleID]),
    case get_row(Sql) of
        [Bin] ->
            case db_sql:to_term(Bin) of
				DojangData when erlang:is_record(DojangData, dojang_info) ->
					DojangData;
				_ ->
		            #dojang_info{pass_id_list = []
		                        ,date = erlang:date()
		                        ,harvest_free_time = data_dojang:get(init_time)
		                        ,harvest_pay_time = 0
		                        ,buy_time = erlang:length(data_dojang:get(buy_cost))}
			end;	
        _ ->
            #dojang_info{pass_id_list = []
                        ,date = erlang:date()
                        ,harvest_free_time = data_dojang:get(init_time)
                        ,harvest_pay_time = 0
                        ,buy_time = erlang:length(data_dojang:get(buy_cost))}
    end.

set_dojang_data(RoleID,DojangData) ->
    Sql = io_lib:format("replace into gDojang values (~w,~s);",[RoleID, to_bin(DojangData)]),
    sql_execute_with_log(Sql).

get_role_dojangrank(RoleID) ->
    Sql = io_lib:format("select * from gdojangrank where roleID = ~w",[RoleID]),
    case get_row(Sql) of
        [RoleID,{date,Day},FreeTime,PaidTime,BuyTime,SelectedGerTypeBin
        ,WorldFreeTimeBin,WorldPaidTimeBin,WorldBuyTimeBin,WorldEnemyListBin,WorldRefreshTimeBin] ->
            LocalFightRecListGroup = get_dojangrank_fightrec_by_roleid(RoleID),
            WorldFightRecList = get_dojangrank_world_fightrec_by_roleid(RoleID),
            WorldFreeTimeList  = case to_term(WorldFreeTimeBin) of
                                  [La1,La2,La3,La4,La5,La6,La7,La8] when erlang:is_integer(La1)->
                                      [La1,La2,La3,La4,La5,La6,La7,La8];
                                  _ ->
                                      lists:duplicate(8,data_dojangrank:get(world_init_time))
                              end,
            WorldPaidTimeList  = case to_term(WorldPaidTimeBin) of
                                  [Lb1,Lb2,Lb3,Lb4,Lb5,Lb6,Lb7,Lb8] when erlang:is_integer(Lb1)->
                                      [Lb1,Lb2,Lb3,Lb4,Lb5,Lb6,Lb7,Lb8];
                                  _ ->
                                      lists:duplicate(8, 0)
                              end,
            WorldBuyTimeList  = case to_term(WorldBuyTimeBin) of
                                  [Lc1,Lc2,Lc3,Lc4,Lc5,Lc6,Lc7,Lc8] when erlang:is_integer(Lc1)->
                                      [Lc1,Lc2,Lc3,Lc4,Lc5,Lc6,Lc7,Lc8];
                                  _ ->
                                      lists:duplicate(8, 0)
                              end,
            WorldEnemyList  = case to_term(WorldEnemyListBin) of
                                  [L1,L2,L3,L4,L5,L6,L7,L8] when erlang:is_list(L1)->
                                      [L1,L2,L3,L4,L5,L6,L7,L8];
                                  _ ->
                                      lists:duplicate(8, [])
                              end,
            WorldRefreshTimeList  = case to_term(WorldRefreshTimeBin) of
                                  [Ld1,Ld2,Ld3,Ld4,Ld5,Ld6,Ld7,Ld8] when erlang:is_integer(Ld1)->
                                      [Ld1,Ld2,Ld3,Ld4,Ld5,Ld6,Ld7,Ld8];
                                  _ ->
                                      lists:duplicate(8, 0)
                              end,
            SelectedGerType0 = to_term(SelectedGerTypeBin),
            #role_dojangrank{day=Day
                            ,free_time=FreeTime
                            ,paid_time=PaidTime
                            ,buy_time=BuyTime
                            ,local_fight_rec_list = LocalFightRecListGroup
                            ,world_fight_rec_list = WorldFightRecList
                            ,selected_ger_type_list = lists:map(fun(RankType)->
                                                                        GerType = lists:nth(RankType, SelectedGerType0),
                                                                        if
                                                                            0 =:= GerType ->
                                                                                lists:nth(RankType, data_dojangrank:get(default)) * 256 + 1;
                                                                            GerType < 256000 ->
                                                                                lists:nth(RankType, data_dojangrank:get(default)) * 256 + 1;
                                                                            true ->
                                                                                GerType
                                                                        end
                                                                end, lists:seq(1,8))
                            ,world_free_time = WorldFreeTimeList
                            ,world_paid_time = WorldPaidTimeList
                            ,world_buy_time = WorldBuyTimeList
                            ,world_enemy_list = WorldEnemyList
                            ,world_refresh_time = WorldRefreshTimeList};
        _ ->
            FighterList = get_fighterList(RoleID),
            SelectedGerTypeList = 
                lists:foldr(fun(T,Acc)-> 
                        List1 = 
                            lists:filter(fun(F) ->
                                        GerConfig = data_ger:get(F#ger.gerBase#gerBase.gerTypeID),
                                        GerConfig#data_ger.gerProperty =:= T
                                end, FighterList),
                        case List1 of
                            [] ->
                                [lists:nth(T, data_dojangrank:get(default)) * 256 + 1|Acc];
                            [H|_] ->
                                [H#ger.gerBase#gerBase.gerTypeID  * 256 + H#ger.gerBase#gerBase.gerLevel|Acc]
                        end     
                    end,[],lists:seq(1, 8)),
            #role_dojangrank{day={0,0,0}
                            ,free_time=0
                            ,paid_time=0
                            ,buy_time=0
                            ,local_fight_rec_list = []
                            ,world_fight_rec_list = [[],[],[],[],[],[],[],[]]
                            ,selected_ger_type_list = SelectedGerTypeList
                            ,world_free_time = lists:duplicate(8,data_dojangrank:get(world_init_time))
                            ,world_paid_time = lists:duplicate(8, 0)
                            ,world_buy_time = lists:duplicate(8, 0)
                            ,world_enemy_list = lists:duplicate(8, [])
                            ,world_refresh_time = lists:duplicate(8, 0)}
    end.

set_role_dojangrank(RoleID,RoleDojangrank) ->
    Sql = io_lib:format("replace into gdojangrank values (~w,'~s',~w,~w,~w,~s,~s,~s,~s,~s,~s);",[RoleID
                                                                   ,date(RoleDojangrank#role_dojangrank.day)
                                                                   ,RoleDojangrank#role_dojangrank.free_time
                                                                   ,RoleDojangrank#role_dojangrank.paid_time
                                                                   ,RoleDojangrank#role_dojangrank.buy_time
                                                                   ,to_bin(RoleDojangrank#role_dojangrank.selected_ger_type_list)
                                                                   ,to_bin(RoleDojangrank#role_dojangrank.world_free_time)
                                                                   ,to_bin(RoleDojangrank#role_dojangrank.world_paid_time)
                                                                   ,to_bin(RoleDojangrank#role_dojangrank.world_buy_time)
                                                                   ,to_bin(RoleDojangrank#role_dojangrank.world_enemy_list)
                                                                   ,to_bin(RoleDojangrank#role_dojangrank.world_refresh_time)]),
    sql_execute_with_log(Sql).

get_relic_id_list() ->
	case get_all("select warID from gRelicWar ") of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

get_carlos_data(WARID) ->
	Sql = io_lib:format("select war_data from gCarlosWar where warID = ~w",[WARID]),
	case get_row(Sql) of
		[Bin] ->
			uncompress_decode(Bin,[]);
		_ ->
			[]
	end.

set_carlos_data(WARID,Data) ->
	Bin = compress_encode(Data),
	Sql = io_lib:format("replace into gCarlosWar values (~w,~s);",[WARID, quote(Bin)]),
	sql_execute_with_log(Sql).

get_carlos_id_list() ->
	case get_all("select warID from gCarlosWar ") of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

get_doublematch_info(RoleID)->
    Sql = io_lib:format("select * from gDoublematch where roleID=~w;",[RoleID]),
    ServerId = data_setting:get(server_id),
    {RemainTime,AlreadyBuy,RfDate,Rank,Score,FightRec,Session,RemainTimeBuy,BeforeScore} = case get_row(Sql) of
        [] ->
            {data_doublematch:get(daily_time)
            ,0,erlang:date(),0
            ,data_doublematch:get(init_score),[],doublematch_server:get_session(),0,0};
        % 此处不做赛季判断
        [_,RemainTime0,AlreadyBuy0,RfDate0,Rank0,Score0,FightRecBin,Session0,RemainTimeBuy0,BeforeScore0]->
            RfDate1 = case str_to_int_list(RfDate0) of
                [Y,M,D] ->
                    {Y,M,D};
                _ ->
                    {0,0,0}
            end,
            FightRecList = case to_term(FightRecBin) of
                FightRec0 when erlang:is_list(FightRec0) ->
                    FightRec0;
                _ ->
                    []
            end,
            {RemainTime0,AlreadyBuy0,RfDate1,Rank0,Score0,FightRecList,Session0,RemainTimeBuy0,BeforeScore0}
    end,
    % dm_data是双排模块保存在内存中的数据格式，
    #dm_data{role_id = RoleID        %% 
            ,server_id = ServerId      %% role所在服务器id
            ,index = 0          %% 玩家排名，没有排名的话，返回0
            ,remain_time = RemainTime    %% 残余战斗次数
            ,already_buy = AlreadyBuy    %% 已经购买的次数
            ,rf_date = RfDate        %% 用于战斗次数的刷新
            ,rank = Rank           %% 竞技场段位
            ,score = Score          %% 总积分
            ,fight_rec=FightRec
            ,session=Session
            ,remain_time_buy=RemainTimeBuy
            ,before_score = BeforeScore       %% role所在服务器id
    }.

set_doublematch_info(RoleID,RoleDmInfo) when erlang:is_record(RoleDmInfo, dm_data)->
    RfDateStr = int_list_to_str(erlang:tuple_to_list(RoleDmInfo#dm_data.rf_date)),
    FightRecBin = to_bin(RoleDmInfo#dm_data.fight_rec),
    Sql = io_lib:format("replace into gDoublematch values (~w,~w,~w,\"~s\",~w,~w,~s,~w,~w,~w)"
                       , [RoleDmInfo#dm_data.role_id
                         ,RoleDmInfo#dm_data.remain_time
                         ,RoleDmInfo#dm_data.already_buy
                         ,RfDateStr
                         ,RoleDmInfo#dm_data.rank
                         ,RoleDmInfo#dm_data.score
                         ,FightRecBin
                         ,RoleDmInfo#dm_data.session
                         ,RoleDmInfo#dm_data.remain_time_buy
                         ,RoleDmInfo#dm_data.before_score]),
    sql_execute_with_log(Sql).

get_local_rank(Session)->
    Sql = io_lib:format("select * from gDoublematch where session = ~w and rank > 0 order by score desc limit 1000;",[Session]),
    case get_rows(Sql) of
        RankList when erlang:is_list(RankList) ->
            get_local_rank2([],RankList,1);
        _ ->
            []
    end.

get_local_rank2(ResList,[],_Index)->
    lists:reverse(ResList);
get_local_rank2(AccList,[[RoleID,RemainTime,AlreadyBuy,_RfDate,Rank,Score,_FightRecBin,_Session,_RemainTimeBuy,_]|OtherList],Index)->
    RolePub = role_lib:get_rolePublic(RoleID),
    ?INFO("get_local_rank2 ~w ~w",[RoleID,RolePub]),
    case RolePub of
        [] ->
            get_local_rank2(AccList,OtherList,Index);
        _ ->
            DmRank = #dm_rank{role_id = RoleID
                             ,server_id = data_setting:get(server_id)
                             ,name = RolePub#rolePublic.roleName
                             ,index = Index, remain_time = RemainTime, already_buy = AlreadyBuy 
                             ,level = RolePub#rolePublic.level, rank = Rank, score = Score
                             ,isMale= RolePub#rolePublic.isMale  
                             ,title = RolePub#rolePublic.title
                             ,head = RolePub#rolePublic.head
                             ,rec_id = 0 ,punit = [] ,ts = 0
                             ,score_change = 0
                             ,vip=role_lib:cacl_vip_info(RolePub#rolePublic.viplevel, RolePub#rolePublic.svipLevel)},
            get_local_rank2([DmRank|AccList],OtherList,Index+1)
    end.

get_dm_fight_rec(RecID)->
    Sql = io_lib:format("select * from gDoubleFightRec where rec_id=~w;",[RecID]),
    case get_row(Sql) of
        [] ->
            ?undefined;
        [_,_,_,_,_,_,_,FightRecBin]->
            case to_term(FightRecBin) of
                FightRec when erlang:is_record(FightRec, sc_doublematch_replay) ->
                    FightInfo0 = FightRec#sc_doublematch_replay.fight_info,
                    FighterList0 = FightInfo0#sc_fight_double_request.fighterList,
                    FighterList = lists:foldr(fun(Pfighter,AccList)->
                                                      [check_p_fight_format(Pfighter)|AccList]
                                              end, [], FighterList0),
                    FightRec#sc_doublematch_replay{fight_info = FightInfo0#sc_fight_double_request{fighterList=FighterList}};
                _ ->
                    ?undefined
            end
    end.

check_p_fight_format(Pfighter) when erlang:is_record(Pfighter, p_fighter)->
    Pfighter;
check_p_fight_format({p_fighter,GerID,GerTypeID,GerPos,GerHp,ProHp,GerSp,GerHpMax
                     ,GerProHpMax,GerQuality,StoneEffectType,SpMax,GerAwakeInfo}) ->
    #p_fighter{gerID=GerID
              ,gerTypeID=GerTypeID
              ,gerPos=GerPos
              ,gerHp=GerHp
              ,gerProHp=ProHp
              ,gerSp=GerSp
              ,gerHpMax=GerHpMax
              ,gerProHpMax=GerProHpMax
              ,gerQuality=GerQuality
              ,stoneEffectType = StoneEffectType
              ,spMax = SpMax
              ,maxAwakeStep=role_awake:get_max_awake_step(GerAwakeInfo)
              ,gerBody=0}.    

insert_dm_fight_rec(RecID,TS,FightRec,ChangeScore,PUnit) when erlang:is_record(FightRec, sc_doublematch_replay)->
    FightRecBin = to_bin(FightRec),
    [P1,P2,P3,P4] = PUnit,
    Sql = io_lib:format("replace into gDoubleFightRec values (~w,~w,~w,~w,~w,~w,~w,~s)"
                       , [RecID
                         ,P1#p_doublematch_rank_unit.roleID
                         ,P2#p_doublematch_rank_unit.roleID
                         ,P3#p_doublematch_rank_unit.roleID
                         ,P4#p_doublematch_rank_unit.roleID
                         ,ChangeScore
                         ,TS
                         ,FightRecBin]),
    sql_execute_with_log(Sql).

backup_dm_rank(State) ->
    {Y,Mon,D} = erlang:date(), {H,Min,_} = erlang:time(),
    DataID = Y *100000000 + Mon*1000000 + D*10000 + H * 100 + Min,
    StateBin = to_bin(State),
    Sql = io_lib:format("replace into gDmData values (~w,~s)"
                       , [DataID
                         ,StateBin]),
    sql_execute_with_log(Sql).

del_old_dm_fight_rec()->
    %% delete from 表名 where id=1;
    TimeLimit = util:now() - 70*24*3600,
    Sql = io_lib:format("delete from gDoubleFightRec where ts < ~w and rec_id > 0",[TimeLimit]),
    sql_execute_with_log(Sql).

get_dm_rank(DataID)->
    Sql = io_lib:format("select * from gDmData where data_id=~w;",[DataID]),
    case get_row(Sql) of
        [] ->
            ?undefined;
        [_,StateBin]->
            to_term(StateBin)
    end.

get_lucky_roll(RoleID,LastLogoutTime) ->
	Sql = io_lib:format("select free_count,buy_count,free_times,mark,isGetRankReward,baseBoxInfo,gLuckyRoll.outer,gLuckyRoll.inner,pos,activityID from gLuckyRoll where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[FreeCount,BuyCount,FreeTimes,Mark,IsGetRankReward,BaseBoxInfo,Outer,Inner,Pos,ActivityID] ->
			{NowDate,_}=_Now = erlang:localtime(),
			{LogoutDate,_}=_LogoutTime = util:seconds_to_datetime(LastLogoutTime),
			case NowDate of
				LogoutDate ->
					FreeTimes2 = FreeTimes;
				_ ->
					FreeTimes2 = 0
			end,
%%			PassDays = get_pass_days(LogoutTime,Now),
%%			if PassDays > 0 ->
%%				   FreeTimes2 = 0;
%%			   true ->
%%				   FreeTimes2 = FreeTimes 
%%			end,
			#lucky_roll{activityID=ActivityID,free_count=FreeCount,buy_count=BuyCount,free_times=FreeTimes2,mark=Mark,isGetRankReward=IsGetRankReward
					   ,baseBoxInfo=to_term(BaseBoxInfo),outer=to_term(Outer),inner=to_term(Inner),pos=to_term(Pos)};
		_ ->
			#lucky_roll{}
	end.

set_lucky_roll(RoleID,LuckyRoll) ->
	#lucky_roll{activityID=ActivityID,free_count=FreeCount,buy_count=BuyCount,free_times=FreeTimes
			   ,mark=Mark,isGetRankReward=IsGetRankReward,baseBoxInfo=BaseBoxInfo,outer=Outer,inner=Inner,pos=Pos}=LuckyRoll,
	Sql = io_lib:format("replace into gLuckyRoll values (~w,~w,~w,~w,~w,~w,~s,~s,~s,~s,~w)",
						[RoleID,FreeCount,BuyCount,FreeTimes,Mark,IsGetRankReward,to_bin(BaseBoxInfo),to_bin(Outer),to_bin(Inner),to_bin(Pos),ActivityID]),
	sql_execute_with_log(Sql).
			

get_vip_shop(RoleID) ->
	Sql = io_lib:format("select activityID,items,timestamp from gVipActivity where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[ActivityID,Items,Timestamp] ->
			#vip_shop{activityID=ActivityID,items=to_term(Items),timestamp=Timestamp};
		_ ->
			#vip_shop{activityID=0,items=[],timestamp=0}
	end.

set_vip_shop(RoleID,#vip_shop{activityID=ActivityID,items=Items,timestamp=Timestamp}) ->
	Sql = io_lib:format("replace into gVipActivity values (~w,~w,~s,~w)",[RoleID,ActivityID,to_bin(Items),Timestamp]),
	sql_execute_with_log(Sql).

set_tvcard(RoleID,TVCard) ->
	Sql = io_lib:format("replace into gTvCard values (~w,~s)",[RoleID,to_bin(TVCard)]),
	sql_execute_with_log(Sql).

get_tvcard(RoleID) ->
	Sql = io_lib:format("select tvcard from gTvCard where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[] ->
			#tvcard{};
		[X] when is_binary(X) ->
			to_term(X);
		_ ->
			#tvcard{}
	end.

		
get_trSpecial_info(RoleID) ->
	Sql = io_lib:format("select trID,specialID from gTrSpecial where roleID = ~w",[RoleID]),
	#role{isMale=IsMale,level=Level} = get_roleInfo(RoleID),
	case get_row(Sql ) of
		[TrID,SpecialID] ->
			#trSpecial{trID=TrID,specialID=SpecialID,roleLevel=Level};
		_ ->
			TrID = data_trSpecial:get({defaultTrID,IsMale}),
			#trSpecial{trID=TrID,specialID=0,roleLevel=Level}
	end.

set_trSpecial_info(RoleID,#trSpecial{trID=TrID,specialID=SpecialID}) ->
	Sql = io_lib:format("replace into gTrSpecial values (~w,~w,~w)",[RoleID,TrID,SpecialID]),
	sql_execute_with_log(Sql).

get_bounty_info(RoleID) ->
	Sql = io_lib:format("select type,bountyData,fighterData,last_login from gBounty where roleID = ~w",[RoleID]),
	TType = util:now(),%role_bounty:get_ets_bounty_info(),
	case get_row(Sql) of
		[Type,BountyData,FighterListBin,LastLogin] when FighterListBin /= ?undefined->
            Now = util:now(),
            Length = erlang:byte_size(FighterListBin),
            if TType < Type andalso Length > 20 -> 
                   lists:foreach(fun({Info,ChapterID})-> 
                                         role_data:set_bounty_fighters(Info,ChapterID) 
                                 end, to_term(FighterListBin)),
                   #bounty_info{time_limit=Type,bountyData=to_term(BountyData),last_login=LastLogin};
               true ->
                   ?INFO("get_bounty_info error ~w<~w:~w  Length:~w",[TType,Type,TType < Type,Length]),
                   role_bounty:init_bounty_info()
            end;
		_ ->
			role_bounty:init_bounty_info()
	end.

set_bounty_info(RoleID,BountyInfo,FighterList) ->
	#bounty_info{bountyData=BountyData,time_limit=Type,last_login=LastLogin}=BountyInfo,
	Sql = io_lib:format("replace into gBounty values (~w,~w,~w,~w,~w,~w,~s,~s)"
					   ,[RoleID,0,Type,0,0,LastLogin,to_bin(BountyData),to_bin(FighterList)]),
	sql_execute_with_log(Sql).
					
get_carlos_plane_info(RoleID) ->
	Now = util:now(),
    Sql = io_lib:format("select * from gCarlosPlaneInfo where roleID = ~w",[RoleID]),
    case get_row(Sql) of
        [_,P7,P8,Use,V7,V8,P9,V9,P10,V10,P11,V11,P12,V12,P13,V13,P14,V14,P15,V15,P16,V16,P17,V17,P18,V18] ->
            Now = util:now(),
            {PlaneInfoList,Use2} = lists:foldl(fun({T,P,V},{DAcc,UAcc}) -> 
                                                       Da = #plane_info{type=T,state=P,valid=V},
                                                       {_,Da2,U2} = role_carlos:update_plane_info(Da,Now,UAcc),
                                                       {[Da2|DAcc],U2}
                                               end, {[],Use}, [{7,P7,V7},{8,P8,V8},{9,P9,V9},{10,P10,V10},{11,P11,V11},{12,P12,V12},
                                                               {13,P13,V13},{14,P14,V14},{15,P15,V15},{16,P16,V16},{17,P17,V17},{18,P18,V18}]),
            #plane_use_info{planes=PlaneInfoList,use=Use2};
		_ ->
            PlaneInfoList = [#plane_info{type=T,state=0,valid=0}||T<-[7,8,9,10,11,12,13,14,15,16,17,18]],
			#plane_use_info{planes=PlaneInfoList,use=0}
	end.

set_carlos_plane_info(RoleID,#plane_use_info{planes=Planes,use=Use})->
    VL = lists:foldl(fun(Type,Acc)->
                             case lists:keyfind(Type, #plane_info.type, Planes) of
                                 false -> [0,0|Acc];
                                 #plane_info{state=S,valid=V} -> [V,S|Acc]
                             end
                     end, [], [7,8,9,10,11,12,13,14,15,16,17,18]),
    [P7,V7,P8,V8,P9,V9,P10,V10,P11,V11,P12,V12,P13,V13,P14,V14,P15,V15,P16,V16,P17,V17,P18,V18]=
        lists:reverse(VL),
    Sql = io_lib:format("replace into gCarlosPlaneInfo values (~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w)",
                        [RoleID,P7,P8,Use,V7,V8,P9,V9,P10,V10,P11,V11,P12,V12,P13,V13,P14,V14,P15,V15,P16,V16,P17,V17,P18,V18]),
    sql_execute_with_log(Sql).

%set_carlos_plane_info(RoleID,#plane_use_info{planeType=Type,validTime=ValidTime,use=Use,planeType2=Type2,validTime2=ValidTime2
%											,planeType3=Type3,validTime3=ValidTime3,planeType4=Type4,validTime4=ValidTime4}) ->
%	Sql = io_lib:format("replace into gCarlosPlaneInfo values (~w,~w,~w,~w,~w,~w,~w,~w,~w,~w)"
%					   ,[RoleID, Type,Type2, Use,ValidTime,ValidTime2,Type3,ValidTime3,Type4,ValidTime4]),
%	sql_execute_with_log(Sql).


get_role_homeBoss_times(RoleID,LastLogOutTime) ->
	{Date,_} = util:seconds_to_datetime(LastLogOutTime),
	case erlang:date()  of
		Date ->
			Sql = io_lib:format("select total,goldTimes from gHomeBossTimes where roleID = ~w",[RoleID]),
			case get_row(Sql) of
				[Total,GoldTimes] ->
					#homeBoss_times{total=Total,goldTimes=GoldTimes};
				_ ->
					role_homeBoss:init_times()
			end;
		_ ->
			role_homeBoss:init_times()
	end.

set_role_homeBoss_times(RoleID,#homeBoss_times{total=Total,goldTimes=GoldTimes}) ->
	Sql = io_lib:format("replace into gHomeBossTimes values (~w,~w,~w)",[RoleID,Total,GoldTimes]),
	sql_execute_with_log(Sql).

set_role_box_shop(RoleID,#shop_box_card{cdInfo=CDInfo,openedCardInfo=OpenCardInfo}) ->
	Sql = io_lib:format("replace into gShopBoxCard values (~w,~s,~s);",[RoleID,to_bin(CDInfo),to_bin(OpenCardInfo)]),
	sql_execute_with_log(Sql).

get_role_box_shop(RoleID) ->
	Sql = io_lib:format("select cdInfo,openedCardInfo from gShopBoxCard where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[CDInfo,OpenedCardInfo] when is_binary(OpenedCardInfo)->
			#shop_box_card{cdInfo=to_term(CDInfo),openedCardInfo=to_term(OpenedCardInfo)};
		_ ->
			#shop_box_card{}
	end.

get_payGuide(RoleID) ->
	Sql = io_lib:format("select unit from gPayGuide where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[Unit] when is_binary(Unit) ->
			% case to_term(Unit) of #p_payGuide_unit{}=X -> X ; _ -> role_payGuide:new_payGuideUnit() end;
			case to_term(Unit) of 
				X when is_record(X,payGuideInfo)->
					X;
				%%兼容旧数据
				X when is_record(X,p_payGuide_unit)->
					#p_payGuide_unit{task1ID=Task1ID} = X,
					AcceptedIDList = role_battle:generate_acceptedIDlist(Task1ID),
					#payGuideInfo{currenttaskid=Task1ID,unaccepttasklist=[X],acceptedtaskIDList=AcceptedIDList,showtaskID=Task1ID};
				_->
					AddPayGuideUnit=#p_payGuide_unit{task1ID=Task1ID} = role_payGuide:new_payGuideUnit(),
					#payGuideInfo{currenttaskid=Task1ID,unaccepttasklist=[AddPayGuideUnit],showtaskID=Task1ID} 
			end;
		_ ->
			% role_payGuide:new_payGuideUnit()
			AddPayGuideUnit=#p_payGuide_unit{task1ID=Task1ID} = role_payGuide:new_payGuideUnit(),
			#payGuideInfo{currenttaskid=Task1ID,unaccepttasklist=[AddPayGuideUnit],showtaskID=Task1ID}
	end.

set_payGuide(RoleID,Unit) ->
	Sql = io_lib:format("replace into gPayGuide values (~w,~s)", [RoleID,to_bin(Unit)]),
	sql_execute_with_log(Sql).

get_role_headSeven(RoleID)->
	Sql = io_lib:format("select doingTask,finishTask,finishRewardTask,period,beginTimeStamp,isFirst from gHeadSeven where roleID=~w",[RoleID]),
	case get_row(Sql) of
		[DoingTaskBin,FinishTaskBin,FinishRewardTaskBin,Period,BeginTimeStamp,IsFirst]->
			DoingTaskPersist = to_term(DoingTaskBin),
			FinishTaskPersist = to_term(FinishTaskBin),
			FinishRewardTaskPersist = to_term(FinishRewardTaskBin),
			DoingTask = role_payGuide:transform_persist2term(DoingTaskPersist),
			FinishTask = role_payGuide:transform_persist2term(FinishTaskPersist),
			FinishRewardTask = role_payGuide:transform_persist2term(FinishRewardTaskPersist),
			#head_seven{begintime=BeginTimeStamp,period=Period,doingtask=DoingTask,finishtask=FinishTask,finishrewardtask=FinishRewardTask,isfirst=IsFirst};
		_ ->
			%%不存在数据的情况下，直接设置一个新的活动,时间修改成自然天的0点
			{Day,_Time} = erlang:localtime(),
			BeginTimeStamp = util:datetime_to_seconds({Day,{0,0,1}}),
			#head_seven{begintime=BeginTimeStamp,period=0}
	end.
set_role_headSeven(RoleID,HeadSevenInfo)->
	#head_seven{period=Period,begintime=BeginTimeStamp,doingtask=DoingTask,finishtask=FinishTask,finishrewardtask=FinishRewardTask,isfirst=IsFirst} = HeadSevenInfo,
	DoingTaskPersist = role_payGuide:transform_term2persist(DoingTask),
	FinishTaskPersist = role_payGuide:transform_term2persist(FinishTask),
	FinishRewardTaskPersist = role_payGuide:transform_term2persist(FinishRewardTask),
	% ?ERR("DoingTaskPersist:~w FinishTaskPersist:~w FinishRewardTaskPersist:~w~n",[DoingTaskPersist,FinishTaskPersist,FinishRewardTaskPersist]),
	Sql = io_lib:format("replace into gHeadSeven values (~w,~s,~s,~s,~w,~w,~w)",[RoleID,to_bin(DoingTaskPersist),to_bin(FinishTaskPersist),to_bin(FinishRewardTaskPersist),Period,BeginTimeStamp,IsFirst]),
	sql_execute_with_log(Sql).


get_exBoss_data(RoleID,RoleLevel)->
	Sql = io_lib:format("select bossID,curHp,MaxHp,useTimes,buyTimes,lastRefreshSec,isGetReward,bossLevel,oneHit,nowHit,maxHit,freeTimes,hitList,reward,cdduration from gExBoss where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[BossID,CurHp,MaxHp,UseTimes,BuyTimes,LastRefreshSec,IsGetReward,BossLevel,OneHit,NowHit,MaxHit,FreeTimes,HitListBin,RewardBin,CDDuration] ->
			#role_exBoss{bossID=BossID
						 ,curHp=CurHp
						 ,maxHp=MaxHp
						 ,haveTimes=UseTimes
						 ,buyTimes=BuyTimes
						 ,lastRefreshSec=LastRefreshSec
						 ,isGetReward=IsGetReward
						 ,bossLevel=BossLevel
						 ,oneHit=OneHit
						 ,nowHit=NowHit
						 ,maxHit=MaxHit
                         ,freeRefresh=FreeTimes
						 ,hitList=to_term(HitListBin)
                         ,reward=to_term(RewardBin)
                         ,cdduration=CDDuration
						};
		_ ->role_exBoss:gen_exBoss_data(RoleLevel)
	end.

set_exBoss_data(RoleID,Data)->
	#role_exBoss{bossID=BossID
				 ,curHp=CurHp
				 ,maxHp=MaxHp
				 ,haveTimes=UseTimes
				 ,buyTimes=BuyTimes
				 ,lastRefreshSec=LastRefreshSec
				 ,isGetReward=IsGetReward
				 ,bossLevel=BossLevel
				 ,oneHit=OneHit
				 ,nowHit=NowHit
				 ,maxHit=MaxHit
                 ,freeRefresh=FreeTimes
				 ,hitList=HitList
                 ,reward=Reward
                 ,cdduration=CDDuration
				}= Data,
	Sql = io_lib:format("replace into gExBoss values (~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~s,~w);"
                       ,[RoleID,BossID,CurHp,MaxHp,UseTimes,BuyTimes,LastRefreshSec,IsGetReward,BossLevel
                         ,OneHit,NowHit,MaxHit,FreeTimes,to_bin(HitList),to_bin(Reward),CDDuration]),
	sql_execute_with_log(Sql).

get_xbattle_data(RoleID) ->
    Sql = io_lib:format("select nowChapterID,quickFightSec,buyQuickCount,buyQuickToday,lastTriggerSec,challengeCount,raidCount,passData from gXBattle where roleID = ~w",[RoleID]),
    case get_row(Sql) of 
        [ChapterID,QuickFightSec,BuyQuickCount,BuyQuickToday,LastTriggerSec,ChallengeCount,RaidCount,PassData]->
            #role_xbattle{chapterID=ChapterID 
                          ,buyQuickCount=BuyQuickCount
                          ,buyQuickToday=BuyQuickToday
                          ,quickFightSec=QuickFightSec
                          ,lastTriggerSec=LastTriggerSec
                          ,challengeCount=ChallengeCount
                          ,raidTimes=RaidCount
                         ,passData=to_term(PassData)};
        _ ->
            FirstChapter = case role_data:get_posList() of
                               [] -> 0;
                               _ -> data_common:get(xbattle_first_chapter)
                           end,
            #role_xbattle{chapterID = FirstChapter,challengeCount=1,raidTimes=0}
    end.
set_xbattle_data(RoleID,Xbattle)->
    #role_xbattle{chapterID=ChapterID 
                  ,buyQuickCount=BuyQuickCount
                  ,buyQuickToday=BuyQuickToday
                  ,quickFightSec=QuickFightSec
                  ,lastTriggerSec=LastTriggerSec
                 ,challengeCount=ChallengeCount
                 ,raidTimes=RaidCount
                 ,passData=PassData}=Xbattle,
    Sql = io_lib:format("replace into gXBattle values (~w,~w,~w,~w,~w,~w,~w,~w,~s)",
                        [RoleID,ChapterID,QuickFightSec,BuyQuickCount,BuyQuickToday,LastTriggerSec,ChallengeCount,RaidCount,to_bin(PassData)]),
    sql_execute_with_log(Sql).

set_xbattle_ger_display(RoleID,Data)->
    Sql = io_lib:format("replace into gXbattleGerDisplay values (~w,~s)",[RoleID,to_bin(Data)]),
    sql_execute_with_log(Sql).

get_xbattle_ger_display(RoleID)->
    Sql = io_lib:format("select displayData from gXbattleGerDisplay where roleID = ~w",[RoleID]),
    case get_row(Sql) of
        Data when is_binary(Data) -> to_term(Data);
        _ -> []
    end.

get_trainerRear(RoleID)->
	Sql = io_lib:format("select rearInfo from gTrainerRear where roleID=~w;",[RoleID]),
	case get_row(Sql) of
		[RearBin]->
			to_term(RearBin);
		_->
			-1
	end.

set_trainerRear(RoleID,RearInfo)->
	Sql = io_lib:format("replace into gTrainerRear values (~w,~s);",[RoleID,to_bin(RearInfo)]),
	sql_execute_with_log(Sql).

set_dSignData(RoleID,Data) ->
    #daySign{mData=MData,bsDay=BSDay,s7c=S7C,tto=TTO,lmSec=LMSEC,lmReward=LmReward}=Data,
    Sql = io_lib:format("replace into gDailySign values (~w,~w,~w,~w,~w,~s,~s)",[RoleID,BSDay,S7C,TTO,LMSEC,to_bin(LmReward),to_bin(MData)]),
    sql_execute_with_log(Sql).

get_dSignData(RoleID) ->
    Sql = io_lib:format("select birthDay,sevenContinue,totalSigned,lastMonthSignDate,lmReward,monthData from gDailySign where roleID=~w",[RoleID]),
    case get_row(Sql) of
        [BSDay,S7C,TTO,LMSEC,LmReward,MonthData] ->
            #daySign{mData=to_term(MonthData),bsDay=BSDay,tto=TTO,lmSec=LMSEC,lmReward=to_term(LmReward),s7c=S7C};
        _ ->
            #daySign{bsDay=util:now()}
    end.

%%-------------训练室------------------
getTrainingRoom(RoleID) ->
	Sql = io_lib:format("select * from gtrainingroom where roleID = ~w",[RoleID]),
	get_row(Sql).

setTrainingRoom(RoleID, #playerTrainingRoom{chapterID = ChapterID, leftHP = LeftHP}) ->
	Sql = io_lib:format("replace into gtrainingroom values (~w,~w,~w)",
		[RoleID, ChapterID,LeftHP]),
	sql_execute_with_log(Sql);
setTrainingRoom(_RoleID,_) ->
	ok.

%%-------------训练室 END------------------

%% ====================================================================
%% 日志记录
%% ====================================================================
log_homestead_mating(List)->
	TableName = get_log_table_name(logHomestead),
	ArgList =[ [RoleID, datetime({Date, Time})]
			 ||{RoleID, Date, Time} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s')", ArgList,1000).

log_friend_enargy(List)->
	TableName = get_log_table_name(logFriendEnargy),
	ArgList =[ [RoleID,Type,DRoleID,Value, datetime({Date, Time})]
			 ||{RoleID, Date, Time,DRoleID,Type,Value} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s')", ArgList,1000).
log_dungen_fight(List)->
	TableName = get_log_table_name(logDungenFight),
	ArgList =[ [RoleID, datetime({Date, Time}), DungenID,Result,Type,T]
			 ||{RoleID, DungenID,Result,Date, Time,Type,T} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s',~w,~w,~w,~w)", ArgList,1000).
log_world_boss(List)->
	TableName = get_log_table_name(logWorldBoss),
	ArgList =[ [RoleID, datetime({Date, Time}),Type,Harm]
			 ||{RoleID, Date, Time, Type,Harm} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s',~w,~w)", ArgList,1000).
log_race_sign(List)->
	TableName = get_log_table_name(logRaceSign),
	ArgList =[ [RoleID, datetime({Date, Time})]
			 ||{RoleID, Date, Time} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s')", ArgList,1000).
log_pvp_fight(List)->
	TableName = get_log_table_name(logPvpFight),
	ArgList =[ [RoleID, datetime({Date, Time}),Result,Rank]
			 ||{RoleID, Date, Time,Result,Rank} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~s',~w,~w)", ArgList,1000).

log_create_role(List)->
	TableName = get_log_table_name(logCreateRole),
	ArgList =[ [Accid,RoleID, quote(DevID), IP, Result, datetime({Date, Time}),Sex]
			 ||{Accid, RoleID, DevID, IP, Sex, Result, Date, Time} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(~w,~w,~s,'~w',~w,'~s', ~w)", ArgList,1000).

log_login(List) ->
	TableName = get_log_table_name(logLogin),
	ArgList =[ [Accid,RoleID, quote(DevID), IP, datetime(DateTime),LastDuration]
			 ||{Accid, RoleID, DevID, IP, DateTime,LastDuration} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(~w,~w,~s,'~w', '~s','~w')", ArgList,1000).

log_selectGer(RoleID, GerTypeID) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logSelectGer),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w);",
						[TableName, RoleID, datetime({Date,Time}), GerTypeID]),
	sql_execute_with_log(Str).

log_buyTimes(List) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyTimes),
	ArgList =[ [RoleID, datetime({Date,Time}), VipLevel, Seq, NewValue, Add, Type]
			 ||{RoleID, VipLevel, Seq, NewValue, Add,Type} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(~w,'~s',~w,~w,~w, ~w, ~w)", ArgList,1000).

log_buyEnergy(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyEnergy),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_buyDscv(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyDscv),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_buyPlunder(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyPlunder),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_buyPVP(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyPVP),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_buyCoin(RoleID, VipLevel, Seq, EnergyBefore) ->
	{Date,Time} = erlang:localtime(),
	TableName = get_log_table_name(logBuyCoin),
	Str = io_lib:format("insert into ~s values(~w,'~s',~w,~w,~w);",
						[TableName, RoleID, datetime({Date,Time}), VipLevel, Seq, EnergyBefore]),
	sql_execute_with_log(Str).

log_gold_consume([]) ->
	{ok, 0};
log_gold_consume(List) ->
	TableName = get_log_table_name(t_gold_consume),
	ArgList =[ [RoleID, VipLevel, Gold, GoldBonus, CurGold, CurGoldBonus, datetime(Time), Type, ArgID, quote(Desc),quote(VLog)]
			 ||{RoleID, VipLevel, Gold, GoldBonus, CurGold, CurGoldBonus, _Date, Time, Type, ArgID, Desc,VLog} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,'~s',~w,~w,~s,~s)", ArgList,1000).

log_familytek_source_consume([]) ->
	{ok, 0};
log_familytek_source_consume(List) ->
	TableName = get_log_table_name(t_familytek_source_consume),
	ArgList =[ [FamilyID, Cost, Date, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{FamilyID, Cost, _, Date, Time, Type, ArgID, Desc} <-List],
	?INFO("log_familytek_source_consume running ~w",[ArgList]),
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,'~w','~w','~s',~w,~w,~s)", ArgList,1000).

log_profoundCrystal_consume([]) ->
	{ok, 0};
log_profoundCrystal_consume(List) ->
	TableName = get_log_table_name(t_profoundCrystal_consume),
	ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_honor_consume([]) ->
    {ok, 0};
log_honor_consume(List) ->
    TableName = get_log_table_name(t_honor_consume),
    ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_home_resource_consume([]) ->
    {ok, 0};
log_home_resource_consume(List) ->
    TableName = get_log_table_name(t_home_resource_consume),
    ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_pvppoint_consume([]) ->
    {ok, 0};
log_pvppoint_consume(List) ->
    TableName = get_log_table_name(t_pvppoint_consume),
    ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_coin_consume([]) ->
	{ok,0};
log_coin_consume(List) ->
	TableName = get_log_table_name(t_coin_consume),
	ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_energy_consume([]) ->
	{ok,0};
log_energy_consume(List) ->
	TableName = get_log_table_name(t_energy_consume),
	ArgList =[ [RoleID, VipLevel, Energy, CurEnergy, datetime(Time), Type, ArgID, quote(Desc)]
		||{RoleID, VipLevel, Energy, CurEnergy, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_unioncoin_consume([]) ->
    {ok,0};
log_unioncoin_consume(List) ->
    TableName = get_log_table_name(t_unioncoin_consume),
    ArgList =[ [RoleID, VipLevel, Unioncoin, CurUnioncoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Unioncoin, CurUnioncoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
    
log_repu_consume([]) ->
	{ok,0};
log_repu_consume(List) ->
	TableName = get_log_table_name(t_repu_consume),
	ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_ticket_consume([]) ->
	{ok,0};
log_ticket_consume(List) ->
	TableName = get_log_table_name(t_ticket_consume),
	ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_ticket_add([]) ->
    {ok,0};
log_ticket_add(List) ->
    TableName = get_log_table_name(t_ticket_add),
    ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_laputastone_consume([]) ->
	{ok,0};
log_laputastone_consume(List) ->
	TableName = get_log_table_name(t_laputastone_consume),
	ArgList =[ [RoleID, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_score_consume([]) ->
	{ok,0};
log_score_consume(List) ->
	TableName = get_log_table_name(t_score_consume),
	ArgList =[ [RoleID, VipLevel, Score, CurScore, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Score, CurScore, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_coin_add([]) ->
	{ok,0};
log_coin_add(List) ->
	TableName = get_log_table_name(t_coin_add),
	ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_profoundCrystal_add([]) ->
	{ok,0};
log_profoundCrystal_add(List) ->
	TableName = get_log_table_name(t_profoundCrystal_add),
	ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_honor_add([]) ->
    {ok,0};
log_honor_add(List) ->
    TableName = get_log_table_name(t_honor_add),
    ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_home_resource_add([]) ->
    {ok,0};
log_home_resource_add(List) ->
    TableName = get_log_table_name(t_home_resource_add),
    ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_pvppoint_add([]) ->
    {ok,0};
log_pvppoint_add(List) ->
    TableName = get_log_table_name(t_pvppoint_add),
    ArgList =[ [RoleID, VipLevel, Coin, CurCoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Coin, CurCoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_repu_add([]) ->
	{ok,0};
log_repu_add(List) ->
	TableName = get_log_table_name(t_repu_add),
	ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_unioncoin_add([]) ->
    {ok,0};
log_unioncoin_add(List) ->
    TableName = get_log_table_name(t_unioncoin_add),
    ArgList =[ [RoleID, VipLevel, Unioncoin, CurUnioncoin, datetime(Time), Type, ArgID, quote(Desc)]
             ||{RoleID, VipLevel, Unioncoin, CurUnioncoin, _Date, Time, Type, ArgID, Desc} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_gold_bonus_add([]) ->
	{ok,0};
log_gold_bonus_add(List) ->
	TableName = get_log_table_name(t_gold_bonus_add),
	ArgList =[ [RoleID, VipLevel, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, VipLevel, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_gold_pay_add([])->
	{ok,0};
log_gold_pay_add(List)->
	TableName = get_log_table_name(t_gold_pay_add),
	TableName2 = get_log_table_name(t_gold_bonus_add),
	ArgList = [ [RoleID, VipLevel, Gold, CurGold, datetime(Time), AppItemID, quote(Desc), Md5,Accid,quote(DeviceID),SrcType,ChapterID,Level]
			 ||{RoleID, VipLevel, Gold, CurGold, _, _, _Date, Time, _, AppItemID, Desc, Md5,Accid,DeviceID,SrcType,ChapterID,Level} <-List],
	ArgList2 = [ [RoleID, VipLevel, GoldBonus, CurGoldBonus, datetime(Time), Type, AppItemID, quote(Desc)]
			 ||{RoleID, VipLevel, _, _, GoldBonus, CurGoldBonus, _Date, Time, Type, AppItemID, Desc, _Md5,_Accid,_DeviceID,_SrcType,_,_} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName2]), "(null, ~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList2, 1000),
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null, ~w,~w,~w,~w,'~s',~w,~s,'~s',~w,~s,~w,~w,~w)",ArgList, 1000).

log_item_add([]) ->
	{ok,0};
log_item_add(List) ->
	TableName = get_log_table_name(t_item_add),
	ArgList =[ [RoleID, ItemUID, ItemTypeID, AddNum, CurItemNum, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, ItemList, _Date, Time, Type, ArgID, Desc} <-List, [ItemUID, ItemTypeID, AddNum, CurItemNum] <- ItemList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	
log_item_consume([]) ->
	{ok,0};
log_item_consume(List) ->
	TableName = get_log_table_name(t_item_consume),
	ArgList =[ [RoleID, ItemUID, ItemTypeID, DecNum, CurItemNum, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, ItemList, _Date, Time, Type, ArgID, Desc} <-List, [ItemUID, ItemTypeID, DecNum, CurItemNum] <- ItemList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).
	

log_ger_add([]) ->
	{ok,0};
log_ger_add(List) ->
	TableName = get_log_table_name(t_ger_add),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerRank, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, GerList, _Date, Time, Type, ArgID, Desc} <-List, [GerUID, GerTypeID, GerLevel, GerRank] <- GerList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_ger_consume([]) ->
	{ok,0};
log_ger_consume(List) ->
	TableName = get_log_table_name(t_ger_consume),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerRank, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, GerList, _Date, Time, Type, ArgID, Desc} <-List, [GerUID, GerTypeID, GerLevel, GerRank] <- GerList],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_ger_uplevel([]) ->
	{ok,0};
log_ger_uplevel(List) ->
	TableName = get_log_table_name(t_ger_uplevel),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, datetime(Time)]
			 ||{RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, _Date, Time} <- List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).

log_ger_uprank([]) ->
	{ok,0};
log_ger_uprank(List) ->
	TableName = get_log_table_name(t_ger_uprank),
	ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, NewRank, FoodGerUID, datetime(Time)]
			 ||{RoleID, GerUID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, NewRank, FoodGerUID, _Date, Time} <- List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).

log_ger_downrank([]) ->
    {ok,0};
log_ger_downrank(List) ->
    TableName = get_log_table_name(t_ger_downrank),
    ArgList =[ [RoleID, GerUID, GerTypeID, GerLevel, GerRank, NewRank, datetime(Time)]
             ||{RoleID, GerUID, GerTypeID, GerLevel, GerRank, NewRank, Time} <- List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).

log_ger_awake([]) ->
    {ok,0};
log_ger_awake(List) ->
    TableName = get_log_table_name(t_ger_awake),
    ArgList =[[RoleID,GerUID,GerTypeID,AwakeStep,OldSkillID,NewSkillID,OldOptionalSkillID,NewOptionalSkillID,OldRecastTimes,NewRecastTimes,OperationCode,datetime(Time)]
             ||{RoleID,GerUID,GerTypeID,AwakeStep,OldSkillID,NewSkillID,OldOptionalSkillID,NewOptionalSkillID,OldRecastTimes,NewRecastTimes,Date,Time,OperationCode} <- List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,'~s','~s',~w,~w,~w,'~s')", ArgList,1000).

log_ger_crystal([]) ->
    {ok,0};
log_ger_crystal(List) ->
    TableName = get_log_table_name(t_ger_crystal),
    ArgList =[[RoleID,GerUID,GerTypeID,CrystalType,OldQuality,NewQuality,OldLevel,NewLevel,OldExp,NewExp,OldRankExp,NewRankExp,OperationCode,datetime(Time)]
             ||{RoleID,GerUID,GerTypeID,CrystalType,OldQuality,NewQuality,OldLevel,NewLevel,OldExp,NewExp,OldRankExp,NewRankExp,Date,Time,OperationCode} <- List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).

log_item_downrank([]) ->
    {ok,0};
log_item_downrank(List) ->
    TableName = get_log_table_name(t_item_downrank),
    ArgList =[ [RoleID, GerUID, ItemTypeID, ItemLevel, ItemRank, NewRank, datetime(Time)]
             ||{RoleID, GerUID, ItemTypeID, ItemLevel, ItemRank, NewRank, Time} <- List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,~w,~w,~w,'~s')", ArgList,1000).


log_item_uprank([]) ->
	{ok, 0};
log_item_uprank(List) ->
	TableName = get_log_table_name(t_item_uprank),
	ArgList = [ [RoleID,ItemUID,ItemTypeID,CurLevel,NewLevel,CurRank,NewRank,FoodItemUID,datetime(Time)]
			  ||{RoleID,ItemUID,ItemTypeID,CurLevel,NewLevel,CurRank,NewRank,FoodItemUID,_Date,Time}<-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values ", [TableName]),"(null,~w,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList, 1000).

log_item_uplevel([]) ->
	{ok, 0};
log_item_uplevel(List) ->
	TableName = get_log_table_name(t_item_uplevel),
	ArgList = [ [RoleID,ItemUID,ItemTypeID,AddLevel,NewLevel,AddTimes,Coin,datetime(Time)]
			  ||{RoleID,ItemUID,ItemTypeID,AddLevel,NewLevel,AddTimes,Coin,_Date,Time}<-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]),"(null,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList, 1000).

log_item_enchant([]) ->
	{ok, 0};
log_item_enchant(List) ->
	TableName = get_log_table_name(t_item_enchant),
	ArgList = [ [RoleID,ItemUID,ItemTypeID,OldEnchantType,OldEnchantLevel,NewEnchantType,NewEnchantLevel,datetime(Time)]
			  ||{RoleID,ItemUID,ItemTypeID,OldEnchantType,OldEnchantLevel,NewEnchantType,NewEnchantLevel,_Date,Time}<-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]),"(null,~w,~w,~w,~w,~w,~w,~w,'~s')", ArgList, 1000).

log_user_online(Num, Date, Time) ->
	Sql = io_lib:format("insert into user_online values (null,'~s',~w,'~s');",[minute(Time),Num,date(Date)]),
	sql_execute_with_log(Sql).

log_guide(RoleID, GuideVal) ->
    Sql = io_lib:format("replace into logGuide values(~w,~w)", [RoleID, GuideVal]),
    sql_execute_with_log(Sql).


log_suggest(RoleID, Accid, RoleName, Title, Content, DateTime) ->
    TableName = get_log_table_name(logSuggest),
    Sql = io_lib:format("insert into ~s values(~w,~w,~s,~s,~s,'~s')",
                        [TableName,Accid,RoleID,quote(RoleName),quote(Title),quote(Content), datetime(DateTime)]),
    sql_execute_with_log(Sql).

log_carlos([]) ->
    {ok, 0};
log_carlos(List) ->
    TableName = get_log_table_name(logCarlos),
    ArgList = 
        lists:foldl(fun({IDList, CarlosType, OpType, DateTime, Extra}, Acc) ->
                        case is_list(IDList) of
                            true ->
                                lists:foldl(fun(E, Acc2) ->
                                                EArg = [E, CarlosType, OpType, datetime(DateTime), Extra],
                                                [EArg|Acc2]
                                            end, Acc, IDList);
                            _ ->
                                EArg = [IDList, CarlosType, OpType, datetime(DateTime), Extra],
                                [EArg|Acc]
                        end
                    end, [], List),
    make_sql_batch_by_piece(io_lib:format("insert into ~s values ", [TableName]), "(null, ~w, ~w, ~w, '~s', ~w)", ArgList, 1000).

log_anubis_score_add([])->
	{ok,0};
log_anubis_score_add(List)->
    TableName = get_log_table_name(t_anubis_score_add),
	ArgList = [ [FamilyID,OldFamilyKillNum,OldFamilyScore,FamilyKillNum,FamilyScore,datetime(Time),OperateType]
			  ||{FamilyID,OldFamilyKillNum,OldFamilyScore,FamilyKillNum,FamilyScore,Time,OperateType}<-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]),"(null,~w,~w,~w,~w,~w,'~s',~w)", ArgList, 1000).

log_laputastone_add([])->
	{ok,0};
log_laputastone_add(List)->
	TableName = get_log_table_name(t_laputastone_add),
	ArgList =[ [RoleID, Repu, CurRepu, datetime(Time), Type, ArgID, quote(Desc)]
			 ||{RoleID, Repu, CurRepu, _Date, Time, Type, ArgID, Desc} <-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName]), "(null,~w,~w,~w,'~s',~w,~w,~s)", ArgList,1000).

log_magicBook_update({}) -> {ok,0};
log_magicBook_update(List) ->
	TableName = get_log_table_name(t_magicBook_update),
	ArgList = [[RoleID,BookID,quote(State),datetime(Time)]
			  ||{RoleID,BookID,State,Time}<-List],
	make_sql_batch_by_piece(io_lib:format("insert into ~s values",[TableName]),"(null, ~w,~w,~s,'~s')",ArgList,500).

log_djr_world_add([])->
    {ok,0};
log_djr_world_add(List) ->
    ?INFO("log_djr_world_add ~w",[List]),
    TableName = get_log_table_name(t_dojankrank_world_record),
    ArgList =[[Time,AtkServerID,quote(AtkDevID),AtkRoleID,AtkLevel,AtkFightPower,AtkVip
               ,RankType,WinType,BeforeRank,AfterRank
               ,DefServerID,quote(DefDevID),DefRoleID,DefLevel,DefFightPower,DefVip]
             ||{Time,AtkServerID,AtkDevID,AtkRoleID,AtkLevel,AtkFightPower,AtkVip
               ,RankType,WinType,BeforeRank,AfterRank
               ,DefServerID,DefDevID,DefRoleID,DefLevel,DefFightPower,DefVip} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName])
                           ,"(null,~w, ~w,~s,~w,~w,~w,~w, ~w,~w,~w,~w, ~w,~s,~w,~w,~w,~w)", ArgList,1000).

log_plunder_fight_add([])->
    {ok,0};
log_plunder_fight_add(List) ->
    TableName = get_log_table_name(t_plunder_fight),
    ArgList =[[Time,RoleID,quote(RoleName),Level,Power,Vip]
             ||{Time,RoleID,RoleName,Level,Power,Vip} <-List],
    make_sql_batch_by_piece(io_lib:format("insert into ~s values", [TableName])
                           ,"(null,~w, ~w,~s,~w,~w,~w)", ArgList,1000).

set_family_donate_back(RoleID,DonateType,BackNum,DonateNum,ItemTypeID) ->
	case DonateType of
		1 ->
			#role{coin = Coin,unioncoin = Unioncoin} = get_roleInfo(RoleID),
			case Coin >= DonateNum of
				true ->
					ResultCoin = Coin - DonateNum,
					ResultUnionCoin = Unioncoin + role_lib:calculate_unioncoin_back(1,0,DonateNum);
				false ->
					ResultCoin = 0,
					ResultUnionCoin = Unioncoin,
					?ERR("这里出现了BUG，玩家拥有的金币不足以完成公会科技升级捐献的钻石~n")
			end,
		    Sql = io_lib:format("update gRole set coin= ~w ,unioncoin = ~w where roleID= ~w;",[ResultCoin,ResultUnionCoin,RoleID]),
    		sql_execute_with_log(Sql);
		2 ->
			#role{gold=Gold,goldBonus=GoldBonus,unioncoin = Unioncoin} = get_roleInfo(RoleID),
			case Gold >= DonateNum of
				true ->
					ResultUnionCoin = Unioncoin + role_lib:calculate_unioncoin_back(2,0,DonateNum),
					Sql = io_lib:format("update gRole set gold= ~w ,unioncoin = ~w where roleID= ~w;",[Gold-DonateNum,ResultUnionCoin,RoleID]);
				false ->
					ResultGold = 0,
					NeedGold = DonateNum - Gold,
					case  GoldBonus >= NeedGold  of
						true ->
							ResultUnionCoin = Unioncoin + role_lib:calculate_unioncoin_back(2,0,DonateNum),
							Sql = io_lib:format("update gRole set gold= ~w,goldBonus = ~w,unioncoin = ~w where roleID= ~w;",[ResultGold,GoldBonus-NeedGold,ResultUnionCoin,RoleID]);
						false ->
							Sql = io_lib:format("update gRole set gold= ~w,goldBonus = ~w where roleID= ~w;",[0,0,RoleID]),
							?ERR("这里出现了BUG，玩家拥有的钻石不足以完成公会科技升级捐献的钻石~n")
					end
			end,
			sql_execute_with_log(Sql);
		3 ->
			#role{reputation = Reputation,unioncoin=Unioncoin} = get_roleInfo(RoleID),
			case Reputation >= DonateNum of
				true ->
					ResultUnionCoin = Unioncoin + role_lib:calculate_unioncoin_back(3,0,DonateNum),
					ResultReputation = Reputation - DonateNum;
				false ->
					ResultReputation = 0,
					ResultUnionCoin = 0,
					?ERR("这里出现了BUG，玩家拥有的徽章不足以完成公会科技升级捐献的钻石~n")
			end,
		    Sql = io_lib:format("update gRole set reputation= ~w,unioncoin= ~w where roleID= ~w;",[ResultReputation,ResultUnionCoin,RoleID]),
    		sql_execute_with_log(Sql);
		4 ->
			#role{unioncoin = Unioncoin} = get_roleInfo(RoleID),
			Sql = io_lib:format("select itemUID from gequip where roleID=~w and itemTypeID= ~w and itemPos=0 and itemRank=0 order by itemUID desc limit 0,~w;",[RoleID,ItemTypeID,DonateNum]),
    		case get_all(Sql) of
        		[_|_] = GerIDList ->
        			GerIDField = string:join([integer_to_list(E)||[E] <- GerIDList], ","),
            		Sql2 = io_lib:format("delete  from gequip where itemUID in (~s);",[GerIDField]),
            		ResultUnionCoin = Unioncoin + role_lib:calculate_unioncoin_back(4,ItemTypeID,DonateNum),
            		Sql3 = io_lib:format("update gRole set unioncoin= ~w where roleID= ~w;",[ResultUnionCoin,RoleID]),
            		sql_execute_with_log2(Sql2++Sql3);
        		_ ->
            	?undefined
    		end;
		5 ->
			#role{unioncoin = Unioncoin} = get_roleInfo(RoleID),
			Sql = io_lib:format("select gerID from gger where roleID=~w and gerTypeID= ~w and gerPos=0 and gerRank=0 order by gerID desc limit 0,~w;",[RoleID,ItemTypeID,DonateNum]),
    		case get_all(Sql) of
        		[_|_] = GerIDList->
        			GerIDField = string:join([integer_to_list(E)||[E] <- GerIDList], ","),
            		Sql2 = io_lib:format("delete  from gger where gerID in (~s);",[GerIDField]),
            		ResultUnionCoin = Unioncoin + role_lib:calculate_unioncoin_back(5,ItemTypeID,DonateNum),
            		Sql3 = io_lib:format("update gRole set unioncoin= ~w where roleID= ~w;",[ResultUnionCoin,RoleID]),
            		sql_execute_with_log2(Sql2++Sql3);
        		_ ->
            	?undefined
    		end
	end.
get_recent_suggest_datetime(RoleID) ->
    TableName = get_log_table_name(logSuggest),
    Sql = io_lib:format("select datetime from ~s where roleID = ~w order by datetime desc limit 1", [TableName, RoleID]),
    case get_row(Sql) of
        [LoginTime] ->
            util:toUnixTime(LoginTime);
        _ ->
            ?undefined
    end.

update_role_name(RoleID,Name) ->
    Sql = io_lib:format("update `gRole` set `roleName` = '~s' where `roleID` = ~w;", [util:latin1(Name),RoleID]),
    case sql_execute_with_log(Sql) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

update_family_name(FamilyID,Name) ->
    Sql = io_lib:format("update `gFamily` set `familyName` = '~s' where `familyID` = ~w;", [util:latin1(Name), FamilyID]),
    case sql_execute_with_log(Sql) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

update_role_name_in_mail(RoleID, Name) ->
    Sql = io_lib:format("update `gMail` set `senderName` = '~s' where `senderID` = ~w;", [util:latin1(Name), RoleID]),
    sql_execute_with_log(Sql).

update_family_request_role_name(RoleID, Name) ->
    Sql = io_lib:format("update `gFamilyRequest` set `roleName` = '~s' where `roleID` = ~w;", [util:latin1(Name), RoleID]),
    sql_execute_with_log(Sql).

update_family_member_name(FamilyID, RoleID, Name) ->
    Sql1 = io_lib:format("update `gFamilyMember` set `roleName` = '~s' where `familyID` = ~w and `roleID` = ~w;", [util:latin1(Name), FamilyID, RoleID]),
    Sql2 = io_lib:format("update `gFamily` set `createRoleName` = '~s' where `familyID` = ~w and `createRoleID` = ~w;", [util:latin1(Name), FamilyID,RoleID]),
    Sql3 = io_lib:format("update `gFamily` set `ownerRoleName` = '~s' where `familyID` = ~w and `ownerRoleID` = ~w;", [util:latin1(Name), FamilyID,RoleID]),
    {Suc2, Err2} = lists:foldl(fun(E,{Suc,Err}) ->
                                    case sql_execute_with_log(E) of
                                        {ok, _} ->
                                            {[E|Suc], Err};
                                        _ ->
                                            {Suc,[E|Err]}
                                    end
                                end,{[],[]},[Sql1,Sql2,Sql3]),
    case Err2 == [] of
        true ->
            true;
        _ ->
            case Suc2 == [] of
                true ->
                    false;
                _ ->
                    ?ERR("公会成员改名时,部分sql执行异常,成功:~p,失败:~p.~n", [Suc2,Err2]),
                    true
            end

    end.

get_family_name_list() ->
    Sql = "select `familyName`, 'familyID' from `gFamily`;",
    get_rows(Sql).

fixup_self_is_friend() ->
    Sql = "delete from `gFriend` where `roleID` = `friendID`;",
    sql_execute_with_log(Sql).

%% 跨服公会排序规则
%% 优先级1:公会站积分、公会等级、战力 降序
%% 优先级2:公会ID，升序, 公会ID越小则创立时间越早
get_cross_family_rank_head(Limit) ->
    Sql = io_lib:format("select * from `gFamilySortInfo` order by `score` desc, `familyLevel` desc, `fightPower` desc, `familyID` limit ~w;", [Limit]),
    case get_rows(Sql) of
        [] ->
            [];
        Lists ->
            {_, RankList} = 
                lists:foldl(fun([FamilyID, Score, Level, FightPower, FamilyName, OwnerRoleName, CurMembers, InRank, Notice, OwnerRoleID, WorldRank, Slogan, ServerID], {Rank, Acc}) ->
                                {Rank+1, [#p_family_summary{family_id=FamilyID, family_name=FamilyName, owner_role_name=OwnerRoleName, cur_members=CurMembers, 
                                                level=Level, rank=InRank, notice=Notice, is_request=false, owner_role_id=OwnerRoleID, world_rank=WorldRank, slogan=Slogan, 
                                                total_fight_power=FightPower, score=Score, cross_rank=Rank, serverID=ServerID}|Acc]}
                            end,{1, []}, Lists),
        lists:reverse(RankList)
    end.

get_cross_family_rank_order() ->
    Sql = "select `familyID` from `gFamilySortInfo` order by `score` desc, `familyLevel` desc, `fightPower` desc, `familyID`;", 
    get_rows(Sql).

update_family_rank_info(Info, IsLocked) ->
    #p_family_summary{family_id=FamilyID, family_name=FamilyName, owner_role_name=OwnerRoleName, cur_members=CurMembers, level=Level, rank=Rank,
                        notice=Notice, owner_role_id=OwnerRoleID, world_rank=WorldRank, slogan=Slogan, total_fight_power=TotalFightPower, score=Score, serverID=ServerID} = Info,
    case IsLocked of
        true ->
            Sql = io_lib:format("replace into `gFamilySortInfo` (`familyID`, `familyLevel`, `fightPower`, `familyName`, `ownerRoleName`, `curMembers`,
                                    `rank`, `notice`, `ownerRoleID`, `worldRank`, `slogan`, `serverID`) values (~w, ~w, ~w, ~s, ~s, ~w, ~w, ~s, ~w, ~w, ~s, ~w);", 
                                     [FamilyID, Level, TotalFightPower, quote(FamilyName), quote(OwnerRoleName), CurMembers, Rank, quote(Notice), OwnerRoleID, WorldRank, quote(Slogan), ServerID]); 
        _ ->
            Sql = io_lib:format("replace into `gFamilySortInfo` (`familyID`, `score`, `familyLevel`, `fightPower`, `familyName`, `ownerRoleName`, `curMembers`,
                                    `rank`, `notice`, `ownerRoleID`, `worldRank`, `slogan`, `serverID`) values (~w, ~w, ~w, ~w, ~s, ~s, ~w, ~w, ~s, ~w, ~w, ~s, ~w);", 
                                     [FamilyID, Score, Level, TotalFightPower, quote(FamilyName), quote(OwnerRoleName), CurMembers, Rank, quote(Notice), OwnerRoleID, WorldRank, quote(Slogan), ServerID]) 
    end,
    sql_execute_with_log(Sql). 

update_family_rank_score(FamilyID, Score) ->
    Sql = io_lib:format("update `gFamilySortInfo` set `score` = ~w where `familyID` = ~w;", [Score, FamilyID]),
    sql_execute_with_log(Sql). 

delete_family_rank_info(FamilyID) ->
    Sql = io_lib:format("delete from `gFamilySortInfo` where `familyID` = ~w;", [FamilyID]),
    sql_execute_with_log(Sql).

%% 获取全部符文碎片信息
get_stonechip_all()->
    Sql = io_lib:format("select roleID,stoneType,firstNum,secondNum,thirdNum,fourthNum from gStoneChip;",[]),
    StonechipList = get_all(Sql),
?INFO("get_stonechip_all- StonechipList:~w",[StonechipList]),
    case StonechipList of
        [_|_] ->
            StonechipList;
        _ ->
            []
    end.

%%获取玩家限时打折活动信息
get_discount_activity_info(RoleID)->
	Sql = io_lib:format("select * from gDiscountInfo where roleID=~w;",[RoleID]),
	Info = case get_row(Sql) of
		[] ->
			[];
		[RoleID,DiscountActivityInfo]->
			to_term(DiscountActivityInfo)
	end,
	Info.
set_discount_activity_info(RoleID,DiscountActivityInfo)->
	Sql = io_lib:format("replace into gDiscountInfo values (~w,~s);",[RoleID,to_bin(DiscountActivityInfo)]),
    sql_execute_with_log(Sql).

%%获取玩家全区抢购活动信息
get_panicbuy_activity_record(RoleID)->
	Sql = io_lib:format("select * from gPanicBuyActivityInfo where roleID=~w;",[RoleID]),
	Info = case get_row(Sql) of
		[] ->
			[];
		[RoleID,PanicBuyActivityInfo]->
			to_term(PanicBuyActivityInfo)
	end,
	Info.
set_panicbuy_activity_record(RoleID,PanicBuyActivityInfo)->
	Sql = io_lib:format("replace into gPanicBuyActivityInfo values (~w,~s);",[RoleID,to_bin(PanicBuyActivityInfo)]),
    sql_execute_with_log(Sql).

get_role_carlosrank(RoleID)->
	Sql = io_lib:format("select * from gCarlosRank where roleID=~w;",[RoleID]),
	Info = case get_row(Sql) of
		[] ->
			[];
		[RoleID,Player]->
			to_term(Player)
	end,
	Info.
set_role_carlosrank(RoleID,Player)->
	Sql = io_lib:format("replace into gCarlosRank values (~w,~s);",[RoleID,to_bin(Player)]),
    sql_execute_with_log(Sql).

%%获取玩家限时打折活动信息
get_last_discount_activity_info(RoleID)->
	Sql = io_lib:format("select * from gDiscountActivityInfo where roleID=~w;",[RoleID]),
	Info = case get_row(Sql) of
		[] ->
			[];
		[RoleID,LastDiscountActivityInfo]->
			to_term(LastDiscountActivityInfo)
	end,
	Info.
set_last_discount_activity_info(RoleID,LastDiscountActivityInfo)->
	Sql = io_lib:format("replace into gDiscountActivityInfo values (~w,~s);",[RoleID,to_bin(LastDiscountActivityInfo)]),
    sql_execute_with_log(Sql).

%% 如果没有记录则插入
replace_stonechip(RoleId,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum)->
    Sql = io_lib:format("replace into gStoneChip values(~w,~w,~w,~w,~w,~w);",[RoleId,StoneType,FirstNum,SecondNum,ThirdNum,FourthNum]),
    sql_execute_with_log(Sql).
%% 删除是因为他已经没有这一种碎片了
del_stonechip(RoleId,StoneType)->
    Sql = io_lib:format("delete from gStoneChip where roleID=~w and stoneType=~w ;",[RoleId,StoneType]),
    sql_execute_with_log(Sql).

%% 获得RoleID玩家符文碎片争夺战信息
get_role_plunder(RoleID)->
    Sql = io_lib:format("select * from gPlunder where roleID=~w;",[RoleID]),
    case get_row(Sql) of
        [RoleID,ProtectEndTime,RestAttackTimes,BuyTimes,LastTimestamp,RecoverTimestamp]->
            [{plunder_role_info,RoleID,ProtectEndTime,RestAttackTimes,BuyTimes,to_term(LastTimestamp),0,RecoverTimestamp}];
        _ ->
            []
    end.
%% 如果没有记录则插入
replace_role_plunder({plunder_role_info,RoleId,ProtectEndTime,RestAttackTimes,BuyTimes,LastPlunderTimestamp,_,RecoverTimestamp})->
    Sql = io_lib:format("replace into gPlunder values(~w,~w,~w,~w,~s,~w);",[RoleId,ProtectEndTime,RestAttackTimes,BuyTimes,to_bin(LastPlunderTimestamp),RecoverTimestamp]),
    sql_execute_with_log(Sql).

%% 为了保存日志，方便检查用户数据，需要用erlang函数来执行
check_unioncoin_for_update(RoleID,OldUnioncoin,VipLevel)->
    if
        OldUnioncoin < 0 ->
            {Date, _} = Time = erlang:localtime(),
            Sql1 = io_lib:format("select familyCon from gFamilyMember where roleID = '~w';",[RoleID]),
            NewUnioncoin = case get_row(Sql1) of
                [FamilyCon] when FamilyCon > 9 ->
                    FamilyCon div 10;
                _ ->
                    0
            end,  
            Sql2 = io_lib:format("update `gRole` set `unioncoin` = ~w where `roleID` = ~w;", [NewUnioncoin, RoleID]),
            sql_execute_with_log(Sql2),
            behavior_unioncoin_add:log(RoleID, VipLevel, NewUnioncoin, 0, Date, Time, ?MONEY_ADD_TYPE_INIT_FAMILY_UNIONCOIN, 0, "init"),
            NewUnioncoin;
        true ->
            OldUnioncoin
    end.

check_tasklevel_for_update(Tasklevel,Vlevel)->
    if
        Tasklevel =< 0 ->
            Vlevel;
        true ->
            Tasklevel
	end.

%% 更新玩家teamid
update_role_teamid(RoleID, TeamID) ->
    Sql = io_lib:format("update `gRole` set `teamId` = ~w where `roleID` = ~w;", [TeamID, RoleID]),
    sql_execute_with_log(Sql).

%% 添加gold转换成goldbonus日志
add_gold2goldbonus_log(RoleID,OldGold,OldGoldBonus,OldType,NewGold,NewGoldBonus,NewType,Type,AccountID)->
	add_gold2goldbonus_log(RoleID,OldGold,OldGoldBonus,OldType,NewGold,NewGoldBonus,NewType,Type,AccountID,datetime(erlang:localtime())).
add_gold2goldbonus_log(RoleID,OldGold,OldGoldBonus,OldType,NewGold,NewGoldBonus,NewType,Type,AccountID,LogTimeStr)->
	Sql = io_lib:format("insert into gold2goldbonus_log values(null,~w,~w,~w,~w,~w,~w,~w,~w,'~s',~w);",[RoleID,AccountID,OldGold,OldGoldBonus,OldType,NewGold,NewGoldBonus,NewType,LogTimeStr,Type]),
	sql_execute_with_log(Sql).

%%更改玩家当前的SrcType以及将Gold转换成goldBonus
gold2goldbonus(RoleID,OldType,NewType,Type,AccountID)->
	gold2goldbonus(RoleID,OldType,NewType,Type,AccountID,0).
gold2goldbonus(RoleID,OldType,NewType,Type,AccountID,LogTimeStamp)->
	case get_roleInfo(RoleID) of
		?undefined->
			false;
		#role{gold=OldGold,goldBonus=OldGoldBonus,srcType=OldSrcType,vipLevel=VipLevel}=RoleInfo->
			NewGold=0,
			NewGoldBonus=OldGold+OldGoldBonus,
			{Date,_}=Time =case LogTimeStamp of 0-> erlang:localtime();_->util:seconds_to_datetime(LogTimeStamp) end,
			case OldGold =:= 0 of
				false->
					%%添加gold消耗日志
					behavior_gold_consume:log(RoleID, VipLevel, OldGold, 0, NewGold, NewGoldBonus, Date, Time, Type, 0, "",[]),
					%%添加gold_bonus添加日志
					behavior_gold_bonus_add:log(RoleID, VipLevel, OldGold, NewGoldBonus, Date, Time, Type, 0, "");
				true->
					ignore
			end,
			NewRoleInfo=RoleInfo#role{gold=NewGold,goldBonus=NewGoldBonus,srcType=NewType},
			update_roleInfo(NewRoleInfo),
			add_gold2goldbonus_log(RoleID,OldGold,OldGoldBonus,OldSrcType,NewGold,NewGoldBonus,NewType,Type,AccountID,datetime(Time))
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================
%% 批量插入
make_sql_batch(Sql, Format, List) when List =/= []->
	Str = lists:foldl(fun(E,Acc) ->
							  ","++io_lib:format(Format,E)++Acc
					  end, ";", List),
	Sql++tl(Str).

make_sql_batch2(Sql, Format, List) when List =/= []->
	Str = lists:foldl(fun(E,Acc) ->
							  ","++io_lib:format(Format,E)++Acc
					  end, "", List),
	Sql++tl(Str).

%% 分段批量插入
make_sql_batch_by_piece(Sql, Format, List, PieceNum) ->
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "").

make_sql_batch_by_piece(Sql, _Format, [], _PieceNum, _AccNum, Acc) ->
	if Acc == "" ->
		   ignore;
	   true ->
		   Sql2 = Sql ++ tl(Acc),
		   sql_execute_with_log(Sql2)
	end;
make_sql_batch_by_piece(Sql, Format, List, PieceNum, PieceNum, Acc) ->
	Sql2 = Sql ++ tl(Acc),
	sql_execute_with_log(Sql2),
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "");
make_sql_batch_by_piece(Sql, Format, [E|List], PieceNum, AccNum, Acc) ->
	Acc2 = ","++io_lib:format(Format,E)++Acc,
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, AccNum+1, Acc2).
	
	
to_term(Bin)->
	to_term(Bin,[]).
to_term(Bin, Default) ->
	case catch binary_to_term(Bin) of
		{'EXIT',_} ->
			Default;
		Term ->
			Term
	end.

to_bin(Term) ->
	quote(term_to_binary(Term)).

compress_encode(Term) ->
	zlib:compress(term_to_binary(Term)).

uncompress_decode(Bin) ->
	uncompress_decode(Bin,[]).
uncompress_decode(Bin, Default) ->
	case catch binary_to_term(zlib:uncompress(Bin)) of
		{'EXIT',_} ->
			Default;
		Term ->
			Term
	end.

datetime({{A,B,C},{D,E,F}}) ->
	io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[A,B,C,D,E,F]);
datetime(Err) ->
	?ERR("datetime err:~p~n",[Err]).
date({A,B,C}) ->
	io_lib:format("~w-~.2.0w-~.2.0w",[A,B,C]);
date(Err) ->
	?ERR("date err:~p~n",[Err]).
time({A,B,C}) ->
	io_lib:format("~.2.0w:~.2.0w:~.2.0w",[A,B,C]).
minute({A,B,_C}) ->
	io_lib:format("~.2.0w:~.2.0w",[A,B]).
	

bool2int(true)->
	1;
bool2int(false)->
	0.

int2bool(1)->
	true;
int2bool(0)->
	false.

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];	%% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote2(String) when is_list(String) ->
    lists:reverse(quote(String, []));
quote2(Bin) when is_binary(Bin) ->
    list_to_binary(quote2(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

%%由于错误的使用了结构转换，有将ger_attr转换成add_attr的情况，所以还原下
deduct_gerattr(GerAttr) when is_record(GerAttr,gerAttr)->
	GerAttr;
deduct_gerattr(GerAttr)->
	List = tuple_to_list(GerAttr),
	Length = length(List),
	NewList = lists:sublist(List,Length-4),
	list_to_tuple(NewList).

addattrpersist2addattr(AddAttrBin) when is_record(AddAttrBin,add_attr)->
	AddAttrBin;
addattrpersist2addattr(AddAttrBin) when is_binary(AddAttrBin)->
	AddAttrStr = binary_to_list(AddAttrBin),
	case AddAttrStr of
		[]->
			#add_attr{};
		_->	
			[H|ValueStrL] = string:tokens(AddAttrStr,"{,}"),
			ValueL = [list_to_integer(E)||E<-ValueStrL],
			list_to_tuple([add_attr|ValueL])
	end;
addattrpersist2addattr(X)->
	?ERR("undefined X:~w~n",[X]),
	X.

lieuview2lieupersist(List)->
	[{GerTypeID,GerQuality,GerPos,GerLevel}||
		#p_lieu_view{lieuGerTypeID=GerTypeID,lieuGerQuality=GerQuality,lieuPos=GerPos,lieuLevel=GerLevel}<-List].
lieuviewpersist2lieu(List)->
	[#p_lieu_view{lieuGerTypeID=GerTypeID,lieuGerQuality=GerQuality,lieuPos=GerPos,lieuLevel=GerLevel}||{GerTypeID,GerQuality,GerPos,GerLevel}<-List].