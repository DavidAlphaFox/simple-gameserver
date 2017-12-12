%% @author liuqiang
%% @doc 奇天乐地ARD充值处理

-module(pay_mod_qtld_chk_ard).
-export([pay_gold/1]).
-include("common.hrl").

-define(ACCOUNT_TYPE_QTLD_ARD, 51).             % 奇天乐地ARD
-define(GID, "2025").
-define(APPKEY, "7b5989a2e3f2140388709e30b71361bc").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        true ->
            Req:ok({"text/html; charset=utf-8","1"});
        _ ->
            Req:ok({"text/html; charset=utf-8","0"})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
    Sid = proplists:get_value("sid", QueryList),
    Accid = proplists:get_value("accid", QueryList),
    case (not lists:member(?undefined, [Sid,Accid])) of
        false ->
            ?ERR("qtld_ard_chk pay, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            ServerAccid = ?AccidBase*(erlang:list_to_integer(Sid)+1) + erlang:list_to_integer(Accid),
            Sql = io_lib:format("select * from gRole where accid=~w;", [ServerAccid]),
            case db_sql:get_row(Sql) of
                [] -> false;
                _ -> true
            end
    end.