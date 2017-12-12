-module(update_1_0_5).

-compile(export_all).

-export([
         update/0
         ]).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-define(APP, [crypto,emysql]).

-define(GUIDE_STATE_CHANGE_LIST,
        [
         {25,26},
         {26,27}
        ]).

-define(SQLS,
        [
         "DROP TABLE IF EXISTS `gTeamPk`;",
         
         "CREATE TABLE `gTeamPk` (
        `roleID` int(11) unsigned NOT NULL,
        `teamPkData` varbinary(20000) NOT NULL COMMENT '3v3相关数据',
        PRIMARY KEY (`roleID`)
         ) ENGINE=InnoDB DEFAULT CHARSET=latin1;",
         
         "DROP TABLE IF EXISTS `gAlien`;",
         
         "CREATE TABLE `gAlien` (
        `roleID` int(11) unsigned NOT NULL,
        `alienTimes` int unsigned NOT NULL COMMENT '异星战场相关数据',
        `lastRecoverTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
        `resetTime` int unsigned NOT NULL COMMENT '异星战场相关数据',
        PRIMARY KEY (`roleID`)
        ) ENGINE=InnoDB DEFAULT CHARSET=latin1;"
        ]).

update() ->
    tk_misc:start_applications(?APP),
    {IP,Port,Name,Pass,DBName,Num} = data_setting:get(database),
    ok = emysql:add_pool(?DB, Num,Name,Pass, IP, Port,DBName, utf8),
    upadte_tables(),
    update_guide_state(),
    update_login_days_reward(),
    update_race_server(),
    update_world_boss(),
    tk_misc:stop_applications(?APP).

upadte_tables() ->
    lists:foreach(fun(Sql) ->
                    {ok,_} = execute(Sql)
              end, ?SQLS),
    write_log("upadte_tables ok").

update_world_boss() ->
    update_nanm(),
    update_hula().

%% 虎牢关持久化信息
-record(d_nanm,{
                bossQuality %% boss当前等级
                ,lastInfo   %% 上次战斗信息
                ,failTimesAcc %% 连续未杀死boss的次数
                ,lastBeginTime%% 上次活动开始时间
               }).

update_nanm() ->
    Sql = io_lib:format("select value from gETC where `key`=~w;", [?DB_ETC_KEY_NANM]),
    {ok,[[Bin]]} = execute(Sql),
    Data = db_sql:uncompress_decode(Bin,[]),
    case Data of
        #d_nanm{bossQuality=BossQuality} ->
            NewBossQuality =
                case BossQuality - 30 >= 0 of
                    true ->
                        BossQuality - 30;
                    false ->
                        0
                end,
            write_log(io_lib:format("BossQuality:~w, NewBossQuality:~w~n", [BossQuality, NewBossQuality])),
            NewData = Data#d_nanm{bossQuality=NewBossQuality},
            NewBin = db_sql:compress_encode(NewData),
            Sql2 = io_lib:format("replace into gETC values (~w,~s);",[?DB_ETC_KEY_NANM, db_sql:quote(NewBin)]),
            {ok,_} = execute(Sql2);
        _ ->
            next
    end,
    write_log("update_nanm ok").

%% 虎牢关持久化信息
-record(d_hula,{
                bossQuality %% boss当前等级
                ,lastInfo   %% 上次战斗信息
                ,failTimesAcc %% 连续未杀死boss的次数
                ,lastBeginTime%% 上次活动开始时间
               }).

update_hula() ->
    Sql = io_lib:format("select value from gETC where `key`=~w;", [?DB_ETC_KEY_HULA]),
    {ok,[[Bin]]} = execute(Sql),
    Data = db_sql:uncompress_decode(Bin,[]),
    case Data of
        #d_hula{bossQuality=BossQuality} ->
            NewBossQuality =
                case BossQuality - 30 >= 0 of
                    true ->
                        BossQuality - 30;
                    false ->
                        0
                end,
            write_log(io_lib:format("BossQuality:~w, NewBossQuality:~w~n", [BossQuality, NewBossQuality])),
            NewData = Data#d_hula{bossQuality=NewBossQuality},
            NewBin = db_sql:compress_encode(NewData),
            Sql2 = io_lib:format("replace into gETC values (~w,~s);",[?DB_ETC_KEY_HULA, db_sql:quote(NewBin)]),
            {ok,_} = execute(Sql2);
        _ ->
            next
    end,
    write_log("update_hula ok").

update_race_server() ->
    {ok, _} = execute("delete from gETC where `key` = 21"),
    write_log("update_race_server ok").

update_login_days_reward() ->
    {ok, _} = execute("update gRoleExtra set lastDrawLoginRewardDays = 0,loginDays = 0"),
    write_log("update_login_days_reward ok").

update_guide_state() ->
    lists:foreach(fun({OldVal, NewVal}) ->
                          Sql = io_lib:format("update gGuide set guideState = ~w where guideState = ~w", [NewVal, OldVal]),
                          {ok, _} = execute(Sql)
                  end, ?GUIDE_STATE_CHANGE_LIST),
    write_log("update_guide_state ok").

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
    [39 | lists:reverse([39 | quote(String, [])])]; %% 39 is $'
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
quote([39 | Rest], Acc) ->      %% 39 is $'
    quote(Rest, [39, $\\ | Acc]);   %% 39 is $'
quote([34 | Rest], Acc) ->      %% 34 is $"
    quote(Rest, [34, $\\ | Acc]);   %% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).



























