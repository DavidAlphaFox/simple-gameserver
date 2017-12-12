%% @author lxl
%% @doc @todo Add description to pay_mod_oppo


-module(pay_mod_oppo).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = Req:parse_post(),
	%?ERR("QS:~w",[QueryString]),
	case verify_sign(QueryString) of
		true ->
			GameRole = proplists:get_value("attach", QueryString),
			[_ServerID,RoleID|_] = string:tokens(GameRole,"{},"),
			RoleID2 = list_to_integer(RoleID),
			{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
			case SrcType of
				22->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					Amount2 = list_to_integer(proplists:get_value("price", QueryString)) div 10,
					Body = Req:recv_body(),
					MSign = md5(Body),
					QS = mochiweb_util:urlencode(QueryString),
					pay_gold2(RoleID2,Amount2,QS,MSign,22);
				_->
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("oppo pay for other SrcType:~w ~n",[SrcType])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("oppo check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_oppo(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCmreYIkPwVovKR8rLHWlFVw7YDfm9uQOJKL89Smt6ypXGVdrAKKl0wNYc3/jecAoPi2ylChfa2iRu5gunJyNmpWZzlCNRIau55fxGW0XEu553IiprOZcaw5OuYGlf60ga8QT6qToP0/dpiL/ZbmNUO9kUhosIjEu22uFgR+5cYyQIDAQAB\n-----END PUBLIC KEY-----">>,
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
	NotifyID = "notifyId="++proplists:get_value("notifyId", QueryString),
	PartnerOrder = "partnerOrder="++proplists:get_value("partnerOrder", QueryString),
	ProductName = "productName="++proplists:get_value("productName", QueryString),
	ProductDesc = "productDesc="++proplists:get_value("productDesc", QueryString),
	Price = "price="++proplists:get_value("price", QueryString),
	Count = "count="++proplists:get_value("count", QueryString),
	Attach = "attach="++proplists:get_value("attach",QueryString),
	NotifyID ++ "&" ++ PartnerOrder ++ "&" ++ ProductName ++ "&" ++ 
		ProductDesc ++ "&" ++ Price ++ "&" ++ Count ++ "&" ++ Attach.

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
