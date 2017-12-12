-module(trans_db_table).
-compile(export_all).

-define(MYSQL_TYPE_LIST, lists:map(fun erlang:atom_to_list/1, 
                                    [tinyint, smallint, mediumint, int, integer, bigint, float, double, decimal, date, time,
                                     year, datetime, timestamp, char, varchar, tinyblob, tinytext, blob, text,
                                     mediumblob, mediumtext, longblob, longtext, varbinary])).
-define(SEPARATOR_List, [9, 11, 32, $;, $(, $)]).

start() ->
    AllRoleIDTables = do_merge:get(all_role_id_tables),
    NeedMergeTables = do_merge:get(need_merge_tables),
    FilterMergeTables = do_merge:get(filter_merge_tables),
    AllList = AllRoleIDTables ++ NeedMergeTables ++ FilterMergeTables,
    erlang:put(table_list, lists:map(fun erlang:atom_to_list/1, AllList)),
    {ok, In} = file:open("script/game_init.sql", read),
    table_trans_writer:start(),
    fetch_line(In).

fetch_line(In) ->
    fetch_line(In, file:read_line(In)).

fetch_line(In, {ok, Line}) ->
    check_line(In, Line),
    fetch_line(In, file:read_line(In));

fetch_line(In, eof) ->
    file:close(In),
    table_trans_writer:stop(),
    eof;

fetch_line(In, Error) ->
    file:close(In),
    table_trans_writer:stop(),
    Error.

check_line(In, Line) ->
    Tokens = tokens(Line),
    case erlang:length(Tokens) > 2 of 
        true ->
            case has_trans_table(Tokens) of
                false ->
                    ignore;
                Table ->
                    trans_table(In, Table) 
            end;
        _ ->
            ignore
    end.

has_trans_table(Tokens) ->
    case is_create_table(Tokens) of
        false ->
            false;
        _ ->
            Pos = get_table_pos(Tokens),
            CurTable = lists:nth(Pos, Tokens),
            CurTable2 = string:strip(CurTable, both, $`),
            TableList = erlang:get(table_list),
            case lists:member(CurTable2, TableList) of
                false ->
                    false;
                _ ->
                    erlang:put(table_list, lists:delete(CurTable2, TableList)),
                    CurTable2
            end
    end.

is_create_table(Tokens) ->
    lists:any(fun(E) -> string:to_upper(E) =:= "CREATE" end, Tokens).

get_table_pos(Tokens) ->
    case is_has_if(Tokens) of
        true ->
            6; 
        _ ->
            3
    end.

is_has_if(Tokens) ->
    lists:any(fun(E) -> string:to_upper(E) =:= "IF" end, Tokens).

trans_table(In, Table) ->
    get_table_proto(Table, In, []).

get_table_proto(Table, In, Acc) ->
    case file:read_line(In) of
        {ok, Line} ->
            case trans_line(Line) of
                terminal ->
                    get_table_proto_end(Table, Acc);
                Info ->
                    get_table_proto(Table, In, [Info|Acc])
            end;
        eof ->
            get_table_proto_end(Table, Acc);
        _ ->
            get_table_proto_end(Table, Acc)
    end.

trans_line(Line) ->
    Tokens = tokens(Line),
    case erlang:length(Tokens) > 2 of
        true ->
		    Type = lists:nth(2, Tokens),
		    Type2 = string:to_lower(Type), 
		    case is_valid_type(Type2) of
		        terminal ->
		            terminal; 
		        _ ->
		            Name = lists:nth(1, Tokens),
		            Name2 = string:strip(Name, both, $`),
		            {Name2, Type2}
		    end;
        _ ->
            terminal

    end.

is_valid_type(Type) ->
    case lists:member(Type, ?MYSQL_TYPE_LIST) of
        true ->
            Type;
        _ ->
            terminal
    end.

get_table_proto_end(Table, Acc) ->
    TypeList = lists:reverse(Acc),
    table_trans_writer:send({write, Table, TypeList}).

tokens(Line) ->
    {Tokens, _ } = 
        lists:foldl(fun(E, {TAcc, CAcc}=Acc) ->
                        case lists:member(E, ?SEPARATOR_List) of
                            true ->
                                case CAcc of
                                    [] ->
                                        Acc;
                                    _ ->
                                        {[lists:reverse(CAcc)|TAcc],[]}
                                end;
                            _ ->
                                {TAcc, [E|CAcc]}
                        end
                    end, {[],[]}, Line),
    lists:reverse(Tokens).
