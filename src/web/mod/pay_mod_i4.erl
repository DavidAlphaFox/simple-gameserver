%% @author liuqiang
%% @doc 爱思助手sdk充值处理

-module(pay_mod_i4).
-export([pay_gold/1]).
-include("common.hrl").

-define(ACCOUNT_TYPE_I4, 39).
-define(APPID, "67").
-define(APPKEY, "48dc5a825f584eaf801d13732be0a228").
-define(PUBKEY, "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCtIPPt/Cqkub+kgh78Jt8HVJ+30YDsJqTqIBMtah+uHvdoe/NqGSmvXIo8gTBetolp5eKnU8NT3zp+yrPAn3CAxHKwuwEZUhnGoEFvJJdI6PM4NfCSq+67U97QSTIXO6JyrLKe88Z9O/ZLGE15TZCzW8T2XZa74pFPlV1ao21hTQIDAQAB").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,_,RoleID,_,_,_,"1"} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_I4 ->
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                _ ->
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        {true,_,RoleID,Money,QueryList,Sign,"0"} ->
            Amount = Money * 10,
            QS = mochiweb_util:urlencode(QueryList),
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_I4 ->
                    pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_I4),
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
    pay_server:do_pay_from_i4(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
%%     ?ERR("debug i4 pay, QueryList = ~p~n", [QueryList]),
    Account = proplists:get_value("account", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    AppID = proplists:get_value("app_id", QueryList),
    Billno = proplists:get_value("billno", QueryList),
    OrderID = proplists:get_value("order_id", QueryList),
    Status = proplists:get_value("status", QueryList),
    Role = proplists:get_value("role", QueryList),
    Zone = proplists:get_value("zone", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [Account,Amount,AppID,Billno,OrderID,Status,Role,Zone,Sign])) of
        false ->
            ?ERR("i4 pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "fail",
            Req:ok({"text/html; charset=utf-8",Reply}), false;
        true ->
            try verify_sign(QueryList, Sign) of
                true ->
                    [ServerID, RoleID, _] = string:tokens(Billno, "."),
                    {true,list_to_integer(ServerID),list_to_integer(RoleID),list_to_integer(Amount),QueryList,Sign,Status};
                false ->
                    ?ERR("i4 pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8",Reply}), false
            catch _:_ ->
                    ?ERR("i4 pay, check sign error, QueryList = ~p~n", [QueryList]),
                    Reply = "fail",
                    Req:ok({"text/html; charset=utf-8",Reply}), false
            end
    end.

verify_sign(QueryList, Sign) ->
    QueryStr = mochiweb_util:urlencode(lists:keydelete("sign", 1, QueryList)),
    DecodeStr = rsa_decrypt(base64:decode(Sign)),
    QsList1 = lists:keysort(1, mochiweb_util:parse_qs(QueryStr)),
    QsList2 = lists:keysort(1, mochiweb_util:parse_qs(DecodeStr)),
    QsList1 =:= QsList2.

get_rsaPubKey()->
    PubKey = erlang:list_to_binary("-----BEGIN PUBLIC KEY-----\n"++?PUBKEY++"\n-----END PUBLIC KEY-----"),
    PemEntries = public_key:pem_decode(PubKey),
    public_key:pem_entry_decode(erlang:hd(PemEntries)).

rsa_decrypt(Data) ->
    RSAPubKey = get_rsaPubKey(),
    lists:append(lists:map(
        fun(X) ->
            erlang:binary_to_list(
                public_key:decrypt_public(X, RSAPubKey, ['rsa_pkcs1_padding']))
        end, binary_split(Data))).

binary_split(Data) ->
    binary_split(Data, []).
binary_split(<<>>, Acc) ->
    lists:reverse(Acc);
binary_split(<<HD:128/binary, Rest/binary>>, Acc) ->
    binary_split(Rest, [HD|Acc]).
