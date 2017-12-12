-module(table_trans_writer).
-compile(export_all).

-define(MYSQL_NUMBER_TYPE, lists:map(fun erlang:atom_to_list/1, [tinyint, smallint, mediumint, int, integer, bigint, float, double])).

start() ->
    {ok, Out} = file:open("src/db/trans/db_trans.erl", write),
    file:write(Out, "-module(db_trans).\n"),
    file:write(Out, "-compile(export_all).\n\n"),
    erlang:put(writer, spawn_link(fun() -> loop(Out) end)).

send(Msg) ->
    Pid = erlang:get(writer),
    erlang:send(Pid, Msg).

stop( ) ->
    send(stop).

loop(Out) ->
    receive 
        {write, Table, InfoList} ->
            write(Out, Table, InfoList),
            loop(Out);
        stop ->
            file:write(Out, "trans(Name, _) -> erlang:error(io_lib:format(\"unknown db table:~p.~n\", [Name]))."),
            file:close(Out),
            stop
    end.

write(Out, Table, InfoList) ->
    Head = io_lib:format("trans(~s, DataList) ->\n", [Table]),
    TableAtom = erlang:list_to_atom(Table),
    SpecialList = do_merge:get(special),
    case is_all_number(InfoList) of
        true ->
            Tail = get_return_list(Table, InfoList,  "DataList");
        _ ->
            MapVarList = get_map_var_list(InfoList),
            case lists:keyfind(TableAtom, 1, SpecialList) of
                false ->
                    SpecialFieldList = [];
                 {_, SpecialFieldList} ->
                    next
            end,
            MapRetList = get_map_return_list(InfoList, SpecialFieldList),
            MapHeadList = io_lib:format("\t\tlists:map(fun([~s]) ->\n", [MapVarList]),
            MapEndList = io_lib:format("\t\t\t\t\t[~s] end, DataList),\n", [MapRetList]),
            MapList = "\tNewDataList = \n" ++ MapHeadList ++ MapEndList,
            ReturnList = get_return_list(Table, InfoList, "NewDataList"),
            Tail = MapList ++ ReturnList
    end,
    file:write(Out, Head ++ Tail).

is_all_number(InfoList) ->
    lists:all(fun({_,E}) -> lists:member(E, ?MYSQL_NUMBER_TYPE) end, InfoList).

get_type_ctl_list(InfoList) ->
    get_type_ctl_list(InfoList, "").

get_type_ctl_list([], Acc) ->
    Acc;

get_type_ctl_list([{_,Type}], Acc) ->
    Acc ++ get_type_ctl(Type);

get_type_ctl_list([{_,Type}|T], Acc) ->
    get_type_ctl_list(T, Acc ++ get_type_ctl(Type) ++ ", " ).

get_type_ctl(Type) ->
    case lists:member(Type, ?MYSQL_NUMBER_TYPE) of
        true ->
            "~w";
        _ ->
            case Type of 
                "date" ->
                    "'~s'";
                "datetime" ->
                    "'~s'";
                _ ->
                    "~s"

            end
    end.

get_return_list(Table, InfoList, DataList) ->
    InsertList = io_lib:format("insert into ~s values", [Table]),
    TypeCtlList = get_type_ctl_list(InfoList),
    io_lib:format("\t{\"~s\", \"(~s)\", ~s};\n\n", [InsertList, TypeCtlList, DataList]).

get_map_var_list(InfoList) ->
    get_map_var_list(InfoList, "").

get_map_var_list([], Acc) ->
    Acc;

get_map_var_list([E], Acc) ->
    Acc ++ gen_map_var_str(E); 

get_map_var_list([E|T], Acc) ->
    NewAcc = Acc ++ gen_map_var_str(E) ++ ", ",
    get_map_var_list(T, NewAcc).

gen_map_var_str({Field, Type}) ->
    Name = gen_name(Field), 
    case Type of
        "date" ->
            "{date, "  ++ Name ++ "}"; 
        "datetime" ->
            "{datetime, " ++ Name ++ "}";
        _ ->
            Name
    end.

get_map_return_list(InfoList, SpecialFieldList) ->
    get_map_return_list(InfoList, SpecialFieldList, "").

get_map_return_list([], _, Acc) ->
    Acc;

get_map_return_list([E], SpecialFieldList, Acc) ->
    Acc ++ gen_map_return_var_str(SpecialFieldList, E);

get_map_return_list([E|T], SpecialFieldList, Acc) ->
    NewAcc = Acc ++ gen_map_return_var_str(SpecialFieldList, E) ++ ", ",
    get_map_return_list(T, SpecialFieldList, NewAcc).

gen_map_return_var_str(SpecialFieldList, {Field, Type}) ->
    Name = gen_name(Field),
    FieldAtom = erlang:list_to_atom(Field),
    case lists:keyfind(FieldAtom, 1, SpecialFieldList) of
        false ->
            RName = Name;
        {_, RName} ->
            next
    end,
    case lists:member(Type, ?MYSQL_NUMBER_TYPE) of
        true ->
            RName;
        _ ->
            case Type of
                "date" ->
                    "db_sql:date(" ++ RName ++ ")";
                "datetime" ->
                    "db_sql:datetime(" ++ RName ++ ")";
                _ ->
                    "db_sql:quote(" ++ RName ++ ")"
            end
    end.

upper_first(String) ->
    {H,T} = lists:split(1, String),
    string:to_upper(H) ++ T.

gen_name(Field) ->
    case is_number_list(Field) of
        true ->
            "V" ++ Field;
        _ ->
            upper_first(Field)
    end.

is_number_list(Field) ->
    lists:all(fun(E) -> E >= $0 andalso E =< $9 end, Field).
