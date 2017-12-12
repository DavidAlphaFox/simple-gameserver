%% @author liuqiang
%% @doc 悠悠村sdk支付处理
%% Created 2014/6/11


-module(pay_mod_uu).
-include("common.hrl").

-export([pay_gold/1]).

-define(ACCOUNT_TYPE_UU, 50).                   % 悠悠村
-define(UUKEY, "3UYUQr").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
%%    ?ERR("debug uu pay, the query_string is: ~p~n", [QueryString]),
    case check_auth(QueryString) of
		{true,RoleID,Amount,Receipt,Sign} ->
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType of
				?ACCOUNT_TYPE_UU ->
					pay_gold2(RoleID, Amount, Receipt, Sign, ?ACCOUNT_TYPE_UU),
					Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply});
				_ ->
					QSL = mochiweb_util:urlencode(QueryString),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QSL]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID, Amount, Receipt, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_uu(RoleID, Amount, Receipt, Sign, SrcType);
pay_gold2(RoleID, _Amount, Receipt, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID, Receipt]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

check_auth(QueryString) ->
    ECallbackRsp = proplists:get_value("callback_rsp", QueryString),
    ECallbackUser = proplists:get_value("callback_user", QueryString),
    ECallbackAppkey = proplists:get_value("callback_appkey", QueryString),
    case not lists:member(?undefined, [ECallbackRsp,ECallbackUser,ECallbackAppkey]) of
        false ->
            {query_string_error,QueryString};
        true ->
            CallbackRsp = des_cbc_decrypt(ECallbackRsp),
%%            ?ERR("debug uu pay, the callback_rsp is: ~p~n", [CallbackRsp]),
            CRList = split_callback_rsp(CallbackRsp),
%%            ?ERR("debug uu pay, the cr_list is: ~p~n", [CRList]),
            AppKey = proplists:get_value("app_key", CRList),
            TxnSeq = proplists:get_value("txn_seq", CRList),
            OrderID = proplists:get_value("order_id", CRList),
            RspCode = proplists:get_value("rsp_code", CRList),
            TxnTime = proplists:get_value("txn_time", CRList),
            ActualTxnAmt = proplists:get_value("actual_txn_amt", CRList),
            TimeStamp = proplists:get_value("time_stamp", CRList),
            SignMsg = proplists:get_value("signMsg", CRList),
            case not lists:member(?undefined, [AppKey,TxnSeq,OrderID,RspCode,TxnTime,ActualTxnAmt,TimeStamp,SignMsg]) of
                false ->
                    {callback_rsp_error,CallbackRsp};
                true ->
                    case RspCode of
                        "000000" ->
                            CallbackUser = des_cbc_decrypt(ECallbackUser),
                            CUList = split_callback_user(CallbackUser),
                            RoleID = proplists:get_value("roleid", CUList),
                            case not lists:member(?undefined, [RoleID]) of
                                false ->
                                    {callback_user_error,CallbackUser};
                                true ->
                                    Key = ?UUKEY,
                                    Sign = sign(AppKey,TxnSeq,OrderID,RspCode,TxnTime,ActualTxnAmt,TimeStamp,Key),
                                    case SignMsg =:= Sign of
                                        false ->
                                            {sign_check_error, Sign};
                                        true ->
                                            {true,list_to_integer(RoleID),(list_to_integer(ActualTxnAmt) div 10),mochiweb_util:urlencode(QueryString),Sign}
                                    end
                            end;
                        _ ->
                            {rsp_code_error,RspCode}
                    end
            end
    end.

split_callback_rsp(Str) ->
    WordList = string:tokens(Str, "&"),
    WordToTuple = fun(Word) -> list_to_tuple(string:tokens(Word, "=")) end,
    lists:map(WordToTuple, WordList).

split_callback_user(Str) ->
    WordList = string:tokens(Str, "#"),
    WordToTuple = fun(Word) -> list_to_tuple(string:tokens(Word, "=")) end,
    lists:map(WordToTuple, WordList).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0B",[N]) || N <- binary_to_list(erlang:md5(S))]).


sign(AppKey,TxnSeq,OrderID,RspCode,TxnTime,ActualTxnAmt,TimeStamp,Key) ->
    Text = "app_key="++AppKey++"&txn_seq="++TxnSeq++"&order_id="++OrderID++"&rsp_code="++RspCode
            ++"&txn_time="++TxnTime++"&actual_txn_amt="++ActualTxnAmt++"&time_stamp="++TimeStamp++"&key="++Key,
    md5(Text).


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