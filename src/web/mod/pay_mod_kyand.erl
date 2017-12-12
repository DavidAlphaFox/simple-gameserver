-module(pay_mod_kyand).
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
			NotifyData = proplists:get_value("notify_data", QueryString),
			case check_pay(NotifyData) of
				true ->
					GameRole = proplists:get_value("dealseq", QueryString),
					[_ServerID,RoleID|_] = string:tokens(GameRole,"_"),
					RoleID2 = list_to_integer(RoleID),
					{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
					case SrcType of
						98->
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							Amount2 = get_amount(NotifyData),
							QS = mochiweb_util:urlencode(QueryString),
							NewSign = util:md5("ky"++proplists:get_value("dealseq", QueryString)),
							pay_gold2(RoleID2,Amount2,QS,NewSign,98);
						_->
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("kyand pay for other SrcType:~w ~n",[SrcType])
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("kyand check_order failed. reason:payresult not 0,order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("kyand check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_kyand(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("fee", Notify_Data2),
	trunc(erlang:list_to_float(Value) * 10).

check_pay(Notify_Data) ->
	Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("payresult", Notify_Data2),
	if Value =:= "0" ->
		   true;
	   true ->
		   false
	end.

get_pubKey()->
    PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCaKCyINNHBbhXHN3365SIUCBS/5GaGOd7QcfJPsD1b5WAg5jfDdwbVETA2tC56dgoBlbKDbVi+kJHdnOMUEu8Lsz0lBXAn51z5Ei9neWfdHp3Bi5Bs/G3Owz56pKotZAoVu7xX0+ZxJLdm1waLIv1tOlvoiYUmgWrEOW6JTHh6nwIDAQAB\n-----END PUBLIC KEY-----">>,
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

get_original_encode_list(QueryString)->
	DealSeq = "dealseq="++proplists:get_value("dealseq", QueryString),
	NotifyData = "notify_data="++proplists:get_value("notify_data", QueryString),
	OrderID = "orderid="++proplists:get_value("orderid", QueryString),
	Uid = "uid="++proplists:get_value("uid", QueryString),
	Subject = "subject="++proplists:get_value("subject", QueryString),
	Version = "v="++proplists:get_value("v", QueryString),
	DealSeq ++ "&" ++ NotifyData ++ "&" ++ OrderID ++ "&" ++ Subject ++ "&" ++ Uid ++ "&" ++ Version.

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
	QS=[{"sign","BmqxCC7C1umX6FKBwBrPsg4i5OoQAMrqmin7dKSJhzceEIDVDCU/PQq1XMZi3EeDB28x14BLY0ZlpJ3/ESCfQ66pcEKPD07Jm7UyE9mLa60V3V+P1KaDG5bHY6t9UQOWE1cNjaSWDchK3SNZawhKDmvVnqaQcGlHqDsHPlZHO5Y="},
        {"uid","{1},{2000013}"},
        {"v", "1.0"},
        {"notify_data","Ylj5cSyh/QqFWGsr/6WcJ81T/mFndRH1xmpuJHULPhToiX1v8Dh2koiVUq10Z854dSXYcr9XJ1OP\nLSvF0Uf9Jh2z7t3wnhyy0wsQsho+33s93hsOueKPt9GySCvReWsyci9aDOC1c8WGBZK7E02QyqSt\nIIBqBfklIRZU1nI+LQs="},
        {"dealseq","2000013-2031002060"},
        {"subject",[54,48,229,133,131,229,174,157]},
        {"orderid","131225260619544586338"}],
	verify_sign(QS),
	QS.
