%% @author lxl
%% @doc @todo Add description to pay_mod_hw


-module(pay_mod_hw).
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
			Result = proplists:get_value("result", QueryString),
			case Result of
				"0" ->
					GameRole = proplists:get_value("requestId", QueryString),
					[_ServerID,RoleID|_] = string:tokens(GameRole,"{},."),
					RoleID2 = list_to_integer(RoleID),
                    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                    case SrcType of
                        16 ->
        					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        					Req:ok({"text/html; charset=utf-8", Reply}),
        					Amount2 = get_amount(QueryString),
        					Body = Req:recv_body(),
        					MSign = md5(Body),
        					QS = mochiweb_util:urlencode(QueryString),
        					pay_gold2(RoleID2,Amount2,QS,MSign,16);
                        _ ->
                            ?ERR("type error ~w",[SrcType]),
                            Reply = ejson:encode({[{<<"result">>,0}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("hw check_order failed. reason:sign wrong,order:~w",[QueryString])
                    end;    
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("hw check_order failed. reason:payresult not 0,order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("hw check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_hw(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("amount", Notify_Data),
	trunc(erlang:list_to_float(Value) * 10).

check_pay(Notify_Data) ->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("result", Notify_Data),
	if Value =:= "0" ->
		   true;
	   true ->
		   false
	end.


get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAKFbddw/wxwthJhOkeiom2FQK0GjnaYQEuOjfSf4+UotURQqwxnfUoXxdCTWkMFOC3sezULY5Zi8ZeZXothOVcECAwEAAQ==\n-----END PUBLIC KEY-----">>,
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
