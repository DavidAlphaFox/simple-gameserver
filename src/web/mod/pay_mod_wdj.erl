%% @author zxystar
%% @doc @todo Add description to pay_mod_wdj.


-module(pay_mod_wdj).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_post(),
	%% ?ERR("wdj SDK:~w",[QueryString]),
	case verify_sign(QueryString) of
		true ->
			Content = proplists:get_value("content", QueryString),
			Content2 = str_to_term(Content),
			%% ?ERR("QS:~p",[Content2]),
			GameRole = proplists:get_value("out_trade_no", Content2),
			GameRole2 = mochiweb_util:parse_qs(GameRole),
			RoleID = proplists:get_value("RoleID",GameRole2),
			RoleID2 = list_to_integer(RoleID),
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
			case RRType of
				7 ->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					Amount = proplists:get_value("money",Content2),
					Amount2 = Amount div 10,
					Body = Req:recv_body(),
					Sign = md5(Body),
					pay_gold2(RoleID2,Amount2,Content,Sign,7);
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
	
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_wdj(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

verify_sign(QueryString) ->
     %% 读取公钥和签名，
     %% 读取的PubKey和Base64Sign都是binary类型
     %% {ok, PubKey} = file:read_file("wdj_pubkey.pem"),
	 PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCd95FnJFhPinpNiE/h4VA6bU1rzRa5+a25BxsnFX8TzquWxqDCoe4xG6QKXMXuKvV57tTRpzRo2jeto40eHKClzEgjx9lTYVb2RFHHFWio/YGTfnqIPTVpi7d7uHY+0FZ0lYL5LlW4E2+CQMxFOPRwfqGzMjs1SDlH7lVrLEVy6QIDAQAB\n-----END PUBLIC KEY-----">>,
     %% {ok, Base64Sign} = file:read_file("sig.txt"),
	 Data = proplists:get_value("content", QueryString),
	 Base64Sign = proplists:get_value("sign",QueryString),
	 %% ?ERR("Data:~w,Base64sign:~w",[Data,Base64Sign]),
     %% 将签名的base64编码解码
     Sign = base64:decode(Base64Sign),
     %% 获取RSA public key
     PemEntries = public_key:pem_decode(PubKey),
     RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),
     %% 验证签名
     public_key:verify(list_to_binary(Data), 'sha', Sign, RSAPubKey).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

str_to_term(Str)->
	M = lists:foldl(fun(E,Acc)->
							case E of
								$,->
									[${,$,,$}|Acc];
								$:->
									[$,|Acc];
								${->
									[${,$[|Acc];
								$}->
									[$],$}|Acc];
								P ->
									[P|Acc]
							end end,[], Str),
	M1 = lists:flatten(lists:reverse(["."|M])),
	{ok,Tokens,_} = erl_scan:string(M1),
	{ok,M3} = erl_parse:parse_term(Tokens),
	M3.