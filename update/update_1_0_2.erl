-module(update_1_0_2).

-compile(export_all).

-export([
         update/0
         ]).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-define(APP, [crypto,emysql]).
-record(role_info, {roleID=0,roleName= <<"">>,isMale=true,title=0,fightPower=0,head=0,level=0}).

-define(GUIDE_STATE_CHANGE_LIST,
        [
         {5,6},
         {6,7},
         {7,8},
         {8,10},
         {9,11},
         {10,12},
         {11,14},
         {12,15},
         {13,21},
         {14,22},
         {15,23},
         {16,24},
         {17,25},
         {18,26},
         {19,27}
        ]).

-define(CREATE_FRIEND_EXT_TABLES,
        [
         "DROP TABLE IF EXISTS `gToFriend`;",
        "CREATE TABLE `gToFriend` (
        `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
        `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
        `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
        `isGive`      tinyint(1) unsigned NOT NULL  COMMENT '是否赠送',
        UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
        KEY (`roleID`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;",
         "DROP TABLE IF EXISTS `gToMe`;",
        "CREATE TABLE `gToMe` (
        `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
        `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
        `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
        UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
        KEY (`roleID`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;",
         "DROP TABLE IF EXISTS `gAddFriend`;",
        "CREATE TABLE `gAddFriend` (
        `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
        `friendID`    int(11) unsigned NOT NULL  COMMENT '好友ID',
        `timestamp`   int(11) unsigned NOT NULL  COMMENT '时间戳',
        UNIQUE KEY `roleID_2` (`roleID`,`friendID`),
        KEY (`roleID`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"
        ]).

-define(DELETE_FRIEND_COLUMNS,
        [
         "alter table `gFriendEnargy` drop column `toFriendList`;",
         "alter table `gFriendEnargy` drop column `toMeList`;",
         "alter table `gFriendEnargy` drop column `addFriendList`;"
        ]).

-define(CREATE_ITEM_USE_TABLE,
        [
         "DROP TABLE IF EXISTS `gItemUse`;",
         "CREATE TABLE `gItemUse` (
        `roleID`      int(11) unsigned NOT NULL  COMMENT '角色ID',
        `itemTypeID`  smallint(5) unsigned NOT NULL  COMMENT '道具模版ID',
        `useDate`     date NOT NULL COMMENT '最近一次使用日期',
        `useTimes`    tinyint unsigned NOT NULL  COMMENT '当日累积使用次数',
        UNIQUE KEY `roleID_2` (`roleID`,`itemTypeID`),
        KEY (`roleID`)
        ) ENGINE=InnoDB DEFAULT CHARSET=latin1;"
        ]).

-define(DELETE_ITEM_USE_COLUMN,
        "alter table `gRoleExtra` drop column `itemUseList`;").

update() ->
    tk_misc:start_applications(?APP),
    {IP,Port,Name,Pass,DBName,Num} = data_setting:get(database),
    ok = emysql:add_pool(?DB, Num,Name,Pass, IP, Port,DBName, utf8),
    reload_data_vip_config(),
    update_vipLevel_by_vip_config(),
    update_guide_state(),
    update_pay_ext_reward(),
	update_role_task(),
    update_race_server(),
    update_item_use(),
    update_friend(),
    tk_misc:stop_applications(?APP).

update_item_use() ->
    lists:foreach(fun(Sql) ->
                      {ok,_} = execute(Sql)
                  end, ?CREATE_ITEM_USE_TABLE),

    update_item_use2(),

    {ok,_} = execute(?DELETE_ITEM_USE_COLUMN),
    write_log("update_item_use ok").

update_item_use2() ->
    {ok, List} = execute("select roleID,itemUseList from gRoleExtra;"),
    lists:foreach(fun([RoleID, ItemUseListBin]) ->
                        ItemUseList = db_sql:to_term(ItemUseListBin),
                        ArgList = [[RoleID,ItemTypeID,db_sql:date(UseDate),UseTimes]||#item_use_info{itemTypeID=ItemTypeID,useDate=UseDate,useTimes=UseTimes}<-ItemUseList],
                        ok = make_sql_batch_by_piece("insert into gItemUse values", "(~w,~w,'~s',~w)", ArgList, 1000)
                  end, List).

update_friend() ->
    lists:foreach(fun(Sql) ->
                    {ok,_} = execute(Sql)
                  end, ?CREATE_FRIEND_EXT_TABLES),

    update_friend2(),

    lists:foreach(fun(Sql) ->
                    {ok,_} = execute(Sql)
                  end, ?DELETE_FRIEND_COLUMNS),
    write_log("update_friend ok").

update_friend2() ->
    update_to_friend(),
    update_to_me(),
    update_add_friend().

update_to_friend() ->
    {ok, List} = execute("select roleID,toFriendList from gFriendEnargy;"),
    lists:foreach(fun([RoleID, ToFriendListBin]) ->
                        ToFriendList = db_sql:to_term(ToFriendListBin),
                        ArgList = [[RoleID,FriendID,Timestamp,IsGive]||{FriendID,Timestamp,IsGive}<-ToFriendList],
                        ok = make_sql_batch_by_piece("replace into gToFriend values", "(~w,~w,~w,~w)", ArgList, 1000)
                  end, List).

update_to_me() ->
    {ok, List} = execute("select roleID,toMeList from gFriendEnargy;"),
    lists:foreach(fun([RoleID, ToMeListBin]) ->
                        ToMeList = db_sql:to_term(ToMeListBin),
                        ArgList = [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-ToMeList],
                        ok = make_sql_batch_by_piece("replace into gToMe values", "(~w,~w,~w)", ArgList, 1000)
                  end, List).

update_add_friend() ->
    {ok, List} = execute("select roleID,addFriendList from gFriendEnargy;"),
    lists:foreach(fun([RoleID, AddFriendListBin]) ->
                        AddFriendList = db_sql:to_term(AddFriendListBin),
                        ArgList = [[RoleID,FriendID,Timestamp]||{FriendID,Timestamp}<-AddFriendList],
                        ok = make_sql_batch_by_piece("replace into gAddFriend values", "(~w,~w,~w)", ArgList, 1000)
                  end, List).

update_race_server() ->
    Sql = io_lib:format("select value from gETC where `key`=~w;", [?DB_ETC_KEY_RACE]),
    {ok,[[Bin]]} = execute(Sql),
    List = db_sql:uncompress_decode(Bin,[]),
    NewList =
        lists:foldr(
          fun({Key, Value}, Acc) ->
                  case Key of
                      {role_race, _RoleID} ->
                          NewValue = trans_role_race(Value),
                          [{Key, NewValue}|Acc];
                      _ ->
                          [{Key, Value}|Acc]
                  end
          end, [], List),
    NewBin = db_sql:compress_encode(NewList),
    Sql2 = io_lib:format("replace into gETC values (~w,~s);",[?DB_ETC_KEY_RACE, db_sql:quote(NewBin)]),
    {ok,_} = execute(Sql2),
    write_log("update_race_server ok").

trans_role_race({role_race, RoleInfo, GroupID}) ->
    NewRoleInfo = trans_role_info(RoleInfo),
    {role_race, NewRoleInfo, GroupID}.

trans_role_info(#role{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level}) ->
    #role_info{roleID=RoleID,roleName=RoleName,isMale=IsMale,title=Title,fightPower=FightPower,head=Head,level=Level};
trans_role_info(#role_info{}=RoleInfo) ->
    RoleInfo.

update_role_task()->
	Sql = io_lib:format("update gTask set triggerNotes = ~s where status >= 3", [to_bin([])]),
	{ok, _} = execute(Sql),
    write_log("update_role_task ok~n").

update_pay_ext_reward() ->
    {ok, _} = execute("update gRole set payExtReward = 0"),
    write_log("update_pay_ext_reward ok").

update_guide_state() ->
    lists:foreach(fun({OldVal, NewVal}) ->
                          Sql = io_lib:format("update gGuide set guideState = ~w where guideState = ~w", [NewVal, OldVal]),
                          {ok, _} = execute(Sql)
                  end, ?GUIDE_STATE_CHANGE_LIST),
    write_log("update_guide_state ok").

update_vipLevel_by_vip_config()->
    SelectRoleIDListSql = io_lib:format("select roleID from gRole where roleID >= ~w;", [tk_id:robot_roleID_max()]),
    {ok, RoleIDDataList} = execute(SelectRoleIDListSql),
    RoleIDList = lists:flatten(RoleIDDataList),
    io:format("length:~w~n",[length(RoleIDList)]),
    ChangedRoleIDList=
    lists:foldr(fun(RoleID,Acc)->
                          {ok, [[VipLevel,GoldPaid]]} = execute(io_lib:format("select VipLevel,GoldTotalPaid from gRole where roleID = ~w;",[RoleID])),
                          VipLevel2 = config_data_vip_update:get(GoldPaid),
                          if VipLevel >= VipLevel2 ->
                                 Acc;
                             true ->
                                 %io:format("do RoleID:~w~n",[RoleID]),
                                 execute(io_lib:format("update gRole set VipLevel=~w where roleID=~w;", [VipLevel2, RoleID])),
                                 [{RoleID, VipLevel, VipLevel2}|Acc]
                          end
                  end,[], RoleIDList),
    write_log(io_lib:format("changednum : ~w changedList :~n~w~n", [erlang:length(ChangedRoleIDList), ChangedRoleIDList])).

reload_data_vip_config()->
    tk_config:reload_config(data_vip),
    io:format("load config_data_vip_update\n"),
    RootDir = tk_config:root_dir(),
    OutDir = filename:join([RootDir, "ebin"]),
    KeyValList = [begin
                      #data_vip{vipLevel=Level,needPayGold=Gold}=data_vip:get(E),
                      {Gold, Level}
                  end
                       || E<-data_vip:get_list(), E > 0],
    config2erl:beam2(KeyValList, config_data_vip_update, OutDir, if_clause, orignal).
    

%%-----------------------------------------------------------internal api---------------------------------------------------------------------------
-define(LOG_PATH, data_setting:get(logger_file_dir)).

execute(Sql) ->
    case catch emysql:execute(?DB, Sql) of
        {ok_packet, _,_,RetId,_,_,_}    ->
            {ok,RetId};
        {result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
            {ok,Rows};
        Exception ->
            M = io_lib:format("~w~n", [Exception]),
            write_log(M),
            timer:sleep(10000),
            execute(Sql)
    end.

write_log(M) ->
    FileName = filename:join([?LOG_PATH, erlang:atom_to_list(?MODULE)]),
    file:write_file(FileName, M, [append, delayed_write]).

%% 分段批量插入
make_sql_batch_by_piece(Sql, Format, List, PieceNum) ->
    make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "").

make_sql_batch_by_piece(Sql, _Format, [], _PieceNum, _AccNum, Acc) ->
    if Acc == "" ->
           ok;
       true ->
           Sql2 = Sql ++ tl(Acc),
           {ok, _} = execute(Sql2),
           ok
    end;
make_sql_batch_by_piece(Sql, Format, List, PieceNum, PieceNum, Acc) ->
    Sql2 = Sql ++ tl(Acc),
    {ok, _} = execute(Sql2),
    make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "");
make_sql_batch_by_piece(Sql, Format, [E|List], PieceNum, AccNum, Acc) ->
    Acc2 = ","++io_lib:format(Format,E)++Acc,
    make_sql_batch_by_piece(Sql, Format, List, PieceNum, AccNum+1, Acc2).

%%指定匹配值分段批量更新或删除
make_sql_batch_in_by_piece(Sql, List, PieceNum) ->
    make_sql_batch_in_by_piece(Sql, List, PieceNum, 0, "").


make_sql_batch_in_by_piece(Sql, [], _PieceNum, _AccNum, Acc) ->
    if Acc == "" ->
           ignore;
       true ->
           Sql2 = Sql ++ "(" ++ Acc,
           {ok, _} = execute(Sql2)
    end;
make_sql_batch_in_by_piece(Sql, List, PieceNum, PieceNum, Acc) ->
    Sql2 = Sql ++ "(" ++ Acc,
    {ok, _} = execute(Sql2),
    make_sql_batch_in_by_piece(Sql, List, PieceNum, 0, "");
make_sql_batch_in_by_piece(Sql, [E|List], PieceNum, AccNum, Acc) ->
    case AccNum of
        0 ->
            Acc2 = erlang:integer_to_list(E) ++ ");";
        _ ->
            Acc2 = erlang:integer_to_list(E) ++ "," ++ Acc
        end,
    make_sql_batch_in_by_piece(Sql, List, PieceNum, AccNum+1, Acc2).





to_bin(Term) ->
	quote(term_to_binary(Term)).
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



























