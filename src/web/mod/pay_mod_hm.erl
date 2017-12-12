%% @author lxl
%% @doc @todo Add description to pay_mod_hm.


-module(pay_mod_hm).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("QS in hm:~w",[QueryString]),
	Type = proplists:get_value("signtype", QueryString),
	case Type of
		"RSA" ->
			do_new_pay(QueryString,Req);
		_ ->
			Sign = proplists:get_value("sign", QueryString),
			TransData = proplists:get_value("transdata", QueryString),
			case check_sign(TransData,Sign) of
				true ->
					{DataList} = ejson:decode(TransData),
					case get_value(DataList,<<"result">>) of
						0 ->
							Amount = trunc(proplists:get_value(<<"money">>,DataList) div 10),
							Pt = erlang:binary_to_list(proplists:get_value(<<"cpprivate">>,DataList)),
							[_ServerIDT,RoleID|_] = string:tokens(Pt,"-"),
							RoleID2 = erlang:list_to_integer(RoleID),
                            {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                            case SrcType of
                                60 ->
        							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        							Req:ok({"text/html; charset=utf-8", Reply}),
        							QS = mochiweb_util:urlencode(QueryString),
        							Sign2 = util:md5(proplists:get_value("sign", QueryString)),
        							pay_gold2(RoleID2,Amount,QS,Sign2	,60);
                                _ ->
                                    ?ERR("type error ~w",[SrcType]),
                                    Reply = ejson:encode({[{<<"result">>,0}]}),
                                    Req:ok({"text/html; charset=utf-8", Reply})
                            end;    
						_ ->
							Reply = ejson:encode({[{<<"result">>,0}]}),
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_hm(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================


check_sign(TransData,Sign)->
	Key = "NTk2MEQ4OUQwQzQyRDEzMDU2NDlGQ0FBRDFFNEI3MkJBMDYxQkJGMU1URXpOekl4TXpNMU5qRTJOakV5TXpVM01Ua3JNalF5TWpnek5qQXpOekV6TmpRMU5UQXdOek00TVRreU1UYzBPREV5T0RVMU1qazFOamsz",
	Key1 = erlang:binary_to_list(base64:decode(Key)),
	Key2 = lists:sublist(Key1, 41, length(Key1)),
	Key3 = erlang:binary_to_list(base64:decode(Key2)),
	[P_keyT,M_keyT] = string:tokens(Key3, "+"),
	P_key = erlang:list_to_integer(P_keyT),
	M_key = erlang:list_to_integer(M_keyT),
	Sign2 = string:tokens(Sign, " "),
	Md5Sign = lists:foldl(fun(H,Acc) ->
								  I = crypto:mod_exp(erlang:list_to_integer(H,16),P_key, M_key),
								  Acc++get_value_byte(I)
						  end,[],Sign2),
	Md5Sign2 = http_uri:decode(string:strip(Md5Sign)),
	Md5Data = util:md5(TransData),
	Md5Data == Md5Sign2.



get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

get_value_byte(Num)->
	get_value_byte(Num,"").
get_value_byte(0,S)->
	lists:flatten(S);
get_value_byte(In, S)->
	P = In rem 256,
	P1 = io_lib:format("~c", [P]),
	get_value_byte(erlang:trunc(In div 256), P1++S).

test()->
	[{[116,114,97,110,115,100,97,116,97],[123,34,101,120,111,114,100,101,114,110,111,34,58,34,49,97,50,48,48,48,48,48,51,97,49,51,57,49,56,53,52,54,53,53,34,44,34,116,114,97,110,115,105,100,34,58,34,48,50,53,49,52,48,50,48,56,49,56,49,55,51,54,50,49,57,51,50,34,44,34,119,97,114,101,115,105,100,34,58,55,44,34,97,112,112,105,100,34,58,34,50,48,48,49,56,55,48,48,48,48,48,48,48,50,50,48,48,49,56,55,34,44,34,102,101,101,116,121,112,101,34,58,50,44,34,109,111,110,101,121,34,58,49,48,44,34,99,111,117,110,116,34,58,49,44,34,114,101,115,117,108,116,34,58,48,44,34,116,114,97,110,115,116,121,112,101,34,58,48,44,34,116,114,97,110,115,116,105,109,101,34,58,34,50,48,49,52,45,48,50,45,48,56,32,49,56,58,49,55,58,53,51,34,44,34,99,112,112,114,105,118,97,116,101,34,58,34,123,49,125,44,123,50,48,48,48,48,48,51,125,34,44,34,112,97,121,116,121,112,101,34,58,52,48,49,125]},{[115,105,103,110],[101,53,97,57,56,101,100,55,56,54,50,98,54,57,56,48,54,49,48,51,98,50,48,54,97,99,51,51,97,52,52,32,99,57,101,55,102,48,101,52,99,54,97,99,52,51,54,51,49,53,57,97,100,49,99,101,48,98,97,49,97,51,100,97,32,57,102,55,56,53,102,57,98,97,101,49,54,51,101,101,102,49,49,53,52,102,56,49,56,102,101,100,50,57,97,48,48,32]}].

do_new_pay(QueryString,Req)->
	TransData = proplists:get_value("transdata", QueryString),
	case new_check_sign(QueryString) of
		true ->
			{DataList} = ejson:decode(TransData),
			case get_value(DataList,<<"result">>) of
				0 ->
					Amount = trunc(proplists:get_value(<<"money">>,DataList) * 10),
					Pt = erlang:binary_to_list(proplists:get_value(<<"cpprivate">>,DataList)),
					[_ServerIDT,RoleID|_] = string:tokens(Pt,"-"),
					RoleID2 = erlang:list_to_integer(RoleID),
					RoleAccID = db_sql:get_role_accid(RoleID2),
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					QS = mochiweb_util:urlencode(QueryString),
					Sign2 = util:md5(proplists:get_value("sign", QueryString)),
					pay_gold2(RoleID2,Amount,QS,Sign2	,60);
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

get_pubKey()->                                                   
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC8Olhdg/ZZ5Xjt7v00wLcAW33H5bnjY+aHsNAmYtgB46MfI1JuhGs5wNbxTybeb+88o0WVEliCts2+QqGO4jHEdPpcuLvyEK3U8IIH3t1ulDGVMsiLJxA3RoNdpqGjNkFH89J/aDTQgBAvOlPueXEedFLrVkD2Vy0Q7rikR6ZdSQIDAQAB\n-----END PUBLIC KEY-----">>,
	PemEntries = public_key:pem_decode(PubKey),
	public_key:pem_entry_decode(hd(PemEntries)).

new_check_sign(QueryString)->
	%%?ERR("QueryString~w",[QueryString]),
	RSAPubKey = get_pubKey(),
	PostSign = proplists:get_value("sign",QueryString),
	Base64Sign = re:replace(PostSign," ","+",[global,{return,list}]),
	Sign = base64:decode(Base64Sign),
	Data = proplists:get_value("transdata",QueryString),
	public_key:verify(list_to_binary(Data), 'md5', Sign, RSAPubKey).


