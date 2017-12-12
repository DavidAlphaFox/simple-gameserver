%% @author liuqiang


-module(pay_mod_37wan).
-include("common.hrl").

-export([pay_gold/1]).

-define(ACCOUNT_TYPE_37WAN, 45).
-define(APPKEY, "cf207e9ba5993ab24d07ba8c98560eda").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    Username = proplists:get_value("username", QueryString),
    ChangeID = proplists:get_value("change_id", QueryString),
    Money = proplists:get_value("money", QueryString),
    Hash = proplists:get_value("hash", QueryString),
    Object = proplists:get_value("object", QueryString),
    case lists:member(?undefined, [Username, ChangeID, Money, Hash, Object]) of
        false ->
            case check_sign(Username, ChangeID, Money, Hash) of
                true ->
                    Amount = trunc(erlang:list_to_integer(Money) * 10),
                    RoleID = erlang:list_to_integer(Object),
                    {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
                    case SrcType of
                        ?ACCOUNT_TYPE_37WAN ->
                            QS = mochiweb_util:urlencode(QueryString),
                            pay_gold2(RoleID, Amount, QS, Hash, ?ACCOUNT_TYPE_37WAN),
                            Reply = ejson:encode({[{<<"result">>, 1}, {<<"accid">>, AccID}]}),
                            Req:ok({"text/html; charset=utf-8", Reply});
                        _ ->
                            ?ERR("type error ~w",[SrcType]),
                            Reply = ejson:encode({[{<<"result">>, 0}]}),
                            Req:ok({"text/html; charset=utf-8", Reply})
                    end;
                _ ->
                    Reply = ejson:encode({[{<<"result">>, 0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;
        _ ->
            ?ERR("the pay message is not complete~n", []),
            Reply = ejson:encode({[{<<"result">>, 0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_37wan(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Username, ChangeID, Money, Hash) ->
    sign(Username, ChangeID, Money) =:= Hash.

sign(Username, ChangeID, Money) ->
    md5(Username ++ "|" ++ ChangeID ++ "|" ++ Money ++ "|" ++ ?APPKEY).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


test() ->
    X = ejson:encode({[{<<"result">>, 0}, {<<"hehe">>, <<"1">>}]}),
    io:format("~s~n", [X]).