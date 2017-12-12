-module(update_1_0_0).

-compile(export_all).

-export([
         update/0
         ]).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-define(APP, [crypto,emysql]).


%%--------------------------------------------------------dong quan 2014-3-12 start----------------------------------------------------------------------

update() ->
    tk_misc:start_applications(?APP),
    {IP,Port,Name,Pass,DBName,Num} = data_setting:get(database),
    ok = emysql:add_pool(?DB, Num,Name,Pass, IP, Port,DBName, utf8),
    do_update(),
    tk_misc:stop_applications(?APP).

do_update() ->
    update_race_server().


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
    {ok,_} = execute(Sql2).

trans_role_race({role_race, RoleInfo, GroupID}) ->
    NewRoleInfo = trans_role_info(RoleInfo),
    {role_race, NewRoleInfo, GroupID}.

trans_role_info({
                 role
                 ,RoleID
                 ,Accid
                 ,RoleName
                 ,IsMale
                 ,Description
                 ,FamilyID
                 ,LastJoinFamily
                 ,Level
                 ,Exp
                 ,Coin
                 ,Reputation
                 ,Gold
                 ,GoldBonus
                 ,GoldUsed
                 ,VipLevel
                 ,GoldTotalPaid
                 ,Title
                 ,FightPower
                 ,LastLogoutTime
                 ,Head
                 ,PayExtReward
                 ,Location
                 ,IsFailed
                 ,DeviceID
                }) ->
    {
     role
     ,RoleID
     ,Accid
     ,RoleName
     ,IsMale
     ,Description
     ,FamilyID
     ,LastJoinFamily
     ,Level
     ,Exp
     ,Coin
     ,Reputation
     ,Gold
     ,GoldBonus
     ,GoldUsed
     ,VipLevel
     ,GoldTotalPaid
     ,Title
     ,FightPower
     ,LastLogoutTime
     ,Head
     ,PayExtReward
     ,Location
     ,IsFailed
     ,DeviceID
     ,1
    }.

%%--------------------------------------------------------dong quan 2014-3-12 end--------------------------------------------------------------------



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


































