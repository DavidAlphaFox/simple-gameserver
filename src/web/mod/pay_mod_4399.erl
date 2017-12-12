%% @author dongquan
%% @doc 4399 sdk充值处理

-module(pay_mod_4399).
-export([pay_gold/1]).
-include("common.hrl").

-define(ACCOUNT_TYPE_4399, 46).
-define(SECRET, "pAaZfynHgjE0mXtKMAIYGi6jUHKS5S1B").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,RoleID,Money,QueryList,Sign} ->
            Amount = erlang:trunc(Money * 10),
			QueryList2 = proplists:delete("time", QueryList),
            QS = mochiweb_util:urlencode(QueryList2),
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_4399->
                    pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_4399),
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_4399(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
%%     ?ERR("debug 4399 pay, QueryList = ~p~n", [QueryList]),
    UserID = proplists:get_value("uid", QueryList),
    OrderID = proplists:get_value("orderid", QueryList),
    Money = proplists:get_value("money", QueryList),
    GameMoney = proplists:get_value("gamemoney", QueryList),
    Time = proplists:get_value("time", QueryList),
    SID = proplists:get_value("serverid", QueryList),
    Mark = proplists:get_value("mark", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [UserID,OrderID,Money,GameMoney,Time,Mark,Sign])) of
        false ->
            ?ERR("4399 pay, query list error, QueryList = ~p~n", [QueryList]),
            false;
        true ->
            Mask2 = lists:sublist(Mark, 11, 22),
            RoleID = lists:sublist(Mask2, 5, 18),
            case sign(OrderID,UserID,Money,GameMoney,SID,Mark,Time) =:= Sign of
                false ->
                    ?ERR("4399 pay, check sign error, QueryList = ~p~n", [QueryList]),
                    false;
                true ->
                    {true,erlang:list_to_integer(RoleID),erlang:list_to_integer(Money),
                     QueryList, Sign}
            end
    end.

sign(OrderID,UserID,Money,GameMoney,?undefined,Mark,Time) ->
    md5(OrderID ++ UserID ++ Money ++ GameMoney ++ ?SECRET ++ Mark ++ Time);
sign(OrderID,UserID,Money,GameMoney,SID,Mark,Time) ->
    md5(OrderID ++ UserID ++ Money ++ GameMoney ++ SID ++ ?SECRET ++ Mark ++ Time).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% -------------------------------------- test code ---------------------------------------------
