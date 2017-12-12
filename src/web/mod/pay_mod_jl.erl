%% @author lxl
%% @doc @todo Add description to pay_mod_jl


-module(pay_mod_jl).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = Req:parse_post(),
	
	case verify_sign(QueryString) of
		true ->
			GameRole = proplists:get_value("out_order_no", QueryString),
			[_ServerID,RoleID|_] = string:tokens(GameRole,"A"),
			RoleID2 = list_to_integer(RoleID),
			{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
			case SrcType of
				34->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					Amount2 = trunc(erlang:list_to_float(proplists:get_value("deal_price", QueryString)) * 10),
					Body = Req:recv_body(),
					MSign = md5(Body),
					QS = mochiweb_util:urlencode(QueryString),
					pay_gold2(RoleID2,Amount2,QS,MSign,34);
				_->
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("jl pay with other SrcType:~w ~n",[SrcType])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("jl check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_jl(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDI7sMDH99+OM5Iu9yOuPzVaWoVmi5c5TrIstis3NyR0AuLDqlcCdSqGlT4tVyI5EatqIIQ2Q60P6qIM1DgJe3qlqLO4da0h/BWRBBPxLSHg/ntk/RMka4TmpIlXKnhiOlgyY2y6MCQrwqJBo9JYmBKE0T0qlLMNcvZfRWttUNbywIDAQAB\n-----END PUBLIC KEY-----">>,
	PemEntries = public_key:pem_decode(PubKey),
	public_key:pem_entry_decode(hd(PemEntries)).

decode_data(Base64Data) ->
	Data = base64:decode(Base64Data),
	RSAPubKey = get_pubKey(),
	Data2 = public_key:decrypt_public(Data, RSAPubKey,['rsa_pkcs1_padding']),
	mochiweb_util:parse_qs(binary:bin_to_list(Data2)).

verify_sign(QueryString)->
	RSAPubKey = get_pubKey(),
	Base64Sign = proplists:get_value("sign",QueryString),
	Sign = base64:decode(Base64Sign),
	Data = get_original_encode_list(QueryString),
	public_key:verify(list_to_binary(Data), 'sha', Sign, RSAPubKey).


get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[H|L] = 
		lists:foldl(fun({A,B},Acc) ->
							case A of
								"sign" ->
									Acc;
								_ ->
									Acc++"&"++A++"="++B
							end
					end, "", QS2),
	L.

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

test()->
	QS=[{"result","0"},
		{"userName","900086000020110429"},
		{"productName",[48,46,49,48,229,133,131,229,174,157]},
		{"payType","4"},
		{"amount","0.10"},
		{"orderId","A201312310227147731F5317"},
		{"notifyTime","1388472000177"},
		{"requestId","{1},{2000049}"},
		{"sign",
		 "YsjewBoY8HetFGWEq4GVqqvxNYEnILxmUqCqWeFsCo9ZkC++ldCZOrSNguH8+BtGbupt8MOwRIOurxjOcdlLGg=="}],
	%verify_sign(QS),
	QS.
