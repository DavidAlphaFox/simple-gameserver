%% @author zcl
%% @doc 49游sdk支付处理
%% Created 2014/6/11


-module(pay_mod_49you).
-include("common.hrl").

-export([pay_gold/1]).

-define(ACCOUNT_TYPE_49YOU, 81).                   % 49游

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    % ?ERR("debug 49you pay, the query_string is: ~p~n", [QueryString]),
    case check_auth(QueryString) of
        {true,Amount,OrderID,RoleID,Sign} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_49YOU->
                    pay_gold2(RoleID, Amount, OrderID, Sign, ?ACCOUNT_TYPE_49YOU),
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply});
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

pay_gold2(RoleID, Amount, Receipt, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_49you(RoleID, Amount, Receipt, Sign, SrcType);
pay_gold2(RoleID, _Amount, Receipt, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID, Receipt]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(QueryString) ->
    AppKey = "cf45493548164e7f81d9a91d317528b6",
    OrderID = proplists:get_value("orderId", QueryString),
    Uid = proplists:get_value("uid", QueryString),
    Amount = proplists:get_value("amount", QueryString),
    ExtraInfo = proplists:get_value("extraInfo", QueryString),
    Sign = proplists:get_value("sign", QueryString),
    ServerID = proplists:get_value("serverId", QueryString),
    case not lists:member(?undefined, [OrderID,Uid,Amount,ExtraInfo,Sign]) of
        false ->
            ?ERR("has null data: OrderID: ~p Uid: ~p Amount: ~p ExtraInfo:~p Sign: ~p ~n",[OrderID,Uid,Amount,ExtraInfo,Sign]),
            false;
        true ->
            [_,RoleID,_] = string:tokens(ExtraInfo,"A"),
            Message = OrderID ++ Uid ++ ServerID ++ Amount ++ ExtraInfo ++ AppKey,
            Sign2 = string:to_lower(md5(Message)),
            case Sign2 =:= Sign of
                true ->
                    {true,list_to_integer(Amount)*10,OrderID,list_to_integer(RoleID),Sign};
                false ->
                    ?ERR("Check failed OrderID:~p Uid:~p ServerID:~p Amount:~p ExtraInfo:~p AppKey:~p ~n",[OrderID,Uid,ServerID,Amount,ExtraInfo,AppKey]),
                    false
            end
    end.



md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).



%% ----------------------------- DES 相关 -----------------------------------

 -define(DESKEY, "G3jFuJRt"). % 正式用
%% -define(DESKEY, "2SoXIhFB"). % 测试用
-define(BLOCKSIZE, 8). % DES加解密块大小

%% DES cbc 加密
des_cbc_encrypt(Text0) ->
    Key = list_to_binary(?DESKEY),
    IVec = list_to_binary(?DESKEY),
    Text = list_to_binary(pkcs5Pad(Text0)),
    Cipher = crypto:des_cbc_encrypt(Key, IVec, Text),
    string:to_upper(bin_to_hexstr(Cipher)).

pkcs5Pad(Str) ->
    Pad = ?BLOCKSIZE - (erlang:length(Str) rem ?BLOCKSIZE),
    lists:append(Str, lists:duplicate(Pad, Pad)).

%% DES cbc 解密
des_cbc_decrypt(Cipher0) ->
    Key = list_to_binary(?DESKEY),
    IVec = list_to_binary(?DESKEY),
    Cipher = hexstr_to_bin(string:to_lower(Cipher0)),
    Text = crypto:des_cbc_decrypt(Key, IVec, Cipher),
    pkcs5Unpad(binary_to_list(Text)).

pkcs5Unpad(Str) ->
    Pad = lists:last(Str),
    lists:filter(fun(X) -> X =/= Pad end, Str).


bin_to_hexstr(Bin) ->
   lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
 
hexstr_to_bin(S) ->
   hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
   list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
   {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
   hexstr_to_bin(T, [V | Acc]).

%% ----------------------------- DES 相关 -----------------------------------