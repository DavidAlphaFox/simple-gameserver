%% @author lxl
%% @doc x7

-module(pay_mod_x7).
-export([pay_gold/1]).
-include("common.hrl").

-define(ACCOUNT_TYPE_X7, 120).

-define(APPID, "11651").
-define(APPKEY, "6cf60f9d636e8d2b26c65567f1672b1d").
-define(PUBKEY, "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCeB097VmK0zUDu/JKYQN4o+8+R4D5AoOgyriwL75AgK4EaZk5uYwaNqELNkqj5g1mjbgpkhMaTImMFsao4bv9g62WRWatxG9j+dFbjhadDh50oziPStFLNbRxb/ay5Siu7sIfJLUa2j6+XY6GMqRSXV6a2XwiUervlIY3bq4vXUwIDAQAB").


%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,_,RoleID,Amount,QueryList,"1"} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_X7 ->
                    QS = mochiweb_util:urlencode(QueryList),
                    Sign = util:md5(QS),
                    pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_X7),
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        {true,_,RoleID,_,_,_} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_X7 ->
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                _ ->
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8",Reply})
            end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_x7(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
    X = proplists:delete("sign_data", QueryList),
     Data = proplists:get_value("encryp_data",QueryList),
     Game_orderid = proplists:get_value("game_orderid", QueryList),
    Sign = proplists:get_value("sign_data",QueryList),
    Sstr = mochiweb_util:urlencode(X),
    try verify_sign(Sstr, Sign) of
        true ->
            Data2 = rsa_decrypt(base64:decode(Data)),
            Status = proplists:get_value("payflag", Data2),
            Amount0 = proplists:get_value("pay", Data2),
            Amount = trunc(list_to_float(Amount0) * 10),
            [ServerID, RoleID, _] = string:tokens(Game_orderid, "."),
            {true,list_to_integer(ServerID),list_to_integer(RoleID),Amount,QueryList,Status};
        false ->
            ?ERR("x7 pay, check sign error, QueryList = ~p~n", [QueryList]),
            Reply = "fail",
            Req:ok({"text/html; charset=utf-8",Reply}), false
    catch _:_ ->
              ?ERR("x7 pay, check sign error, QueryList = ~p~n", [QueryList]),
              Reply = "fail",
              Req:ok({"text/html; charset=utf-8",Reply}), false
    end.

verify_sign(Sstr, Sign) ->
    RSAPubKey = get_pubKey(),
    public_key:verify(list_to_binary(Sstr), 'sha', base64:decode(Sign), RSAPubKey).

get_pubKey()->
    PubKey = erlang:list_to_binary("-----BEGIN PUBLIC KEY-----\n"++?PUBKEY++"\n-----END PUBLIC KEY-----"),
    PemEntries = public_key:pem_decode(PubKey),
    public_key:pem_entry_decode(erlang:hd(PemEntries)).

rsa_decrypt(Data) ->
    RSAPubKey = get_pubKey(),
    Data2 = public_key:decrypt_public(Data, RSAPubKey,['rsa_pkcs1_padding']),
    mochiweb_util:parse_qs(binary:bin_to_list(Data2)).

binary_split(Data) ->
    binary_split(Data, []).
binary_split(<<>>, Acc) ->
    lists:reverse(Acc);
binary_split(<<HD:128/binary, Rest/binary>>, Acc) ->
    binary_split(Rest, [HD|Acc]).
