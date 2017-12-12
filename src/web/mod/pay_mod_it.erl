%% @author lxl
%% @doc @todo Add description to pay_mod_it


-module(pay_mod_it).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = Req:parse_post(),
	%?ERR("it QS:~w",[QueryString]),
	{NotifyData, SignData} = decode_data(proplists:get_value("notify_data", QueryString)),
	case verify_sign(QueryString, SignData) of
		true ->
			
			case check_pay(NotifyData) of
				true ->
					GameRole = binary:bin_to_list(proplists:get_value(<<"order_id_com">>, NotifyData)),
					%GameRole = base64:decode(GameRoleT),
					[_ServerID,RoleID|_] = string:tokens(GameRole,"."),
					RoleID2 = list_to_integer(RoleID),
                      {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                      case SrcType == 14 orelse SrcType == 58 of
                        true ->
        					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        					Req:ok({"text/html; charset=utf-8", Reply}),
        
        					Amount2 = get_amount(NotifyData),
        					Body = Req:recv_body(),
        					MSign = md5(Body),
        					QS = mochiweb_util:urlencode(NotifyData),
        					
        					pay_gold2(RoleID2,Amount2,QS,MSign,14);
                        _ ->
                            ?ERR("type error ~w,~w",[RoleID2,SrcType]),
                            Reply = ejson:encode({[{<<"result">>,0}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("it check_order failed. reason:sign wrong,order:~w",[QueryString])
                    end;    
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("it check_order failed. reason:result = fail,order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("it check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_it(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = binary:bin_to_list(proplists:get_value(<<"amount">>, Notify_Data)),
	trunc(erlang:list_to_float(Value) * 10).

check_pay(Notify_Data) ->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value(<<"result">>, Notify_Data),
	if Value =:= <<"success">> ->
		   true;
	   true ->
		   false
	end.


get_pubKey()->
	PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC2kcrRvxURhFijDoPpqZ/IgPlAgppkKrek6wSrua1zBiGTwHI2f+YCa5vC1JEiIi9uw4srS0OSCB6kY3bP2DGJagBoEgj/rYAGjtYJxJrEiTxVs5/GfPuQBYmU0XAtPXFzciZy446VPJLHMPnmTALmIOR5Dddd1Zklod9IQBMjjwIDAQAB\n-----END PUBLIC KEY-----">>,
	PemEntries = public_key:pem_decode(PubKey),
	public_key:pem_entry_decode(hd(PemEntries)).

decode_data(Base64Data) ->
	Data = binary:bin_to_list(base64:decode(Base64Data)),
	RSAPubKey = get_pubKey(),
	Length = trunc(length(Data) / 128),
	{_, Data2} = lists:foldl(fun(_, {DataAcc, Data2Acc})->
									 {Data0,DataAcc2}=lists:split(128,DataAcc),
									 Data2Acc2 = public_key:decrypt_public(list_to_binary(Data0), RSAPubKey),
									 {DataAcc2, Data2Acc ++ binary:bin_to_list(Data2Acc2)}
							 end , {Data,""}, lists:seq(1, Length)),
	{struct, List} = mochijson2:decode(Data2),
	{List,Data2}.

verify_sign(QueryString, Data)->
	RSAPubKey = get_pubKey(),
	Base64Sign = proplists:get_value("sign",QueryString),
	Sign = base64:decode(Base64Sign),
	public_key:verify(erlang:list_to_binary(Data), 'sha', Sign, RSAPubKey).


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
	QS = [{[110,111,116,105,102,121,95,100,97,116,97],[103,48,98,56,97,97,110,67,57,76,104,109,56,74,76,57,120,87,117,54,121,57,119,102,89,70,111,66,43,79,98,113,118,111,118,81,89,110,70,118,57,76,108,74,51,98,49,90,109,117,80,112,57,72,97,74,70,68,111,57,98,116,77,122,70,82,81,79,47,49,110,119,101,81,90,97,71,99,72,75,98,48,75,121,84,120,52,57,48,106,119,50,90,86,116,69,51,83,48,70,74,116,82,57,69,52,48,82,79,76,43,74,110,69,100,71,108,88,112,82,120,108,47,109,89,88,72,86,111,89,54,117,73,115,109,53,84,113,115,112,90,115,70,77,51,53,100,76,43,109,77,112,90,68,103,76,84,117,99,97,49,122,48,65,111,52,69,115,48,115,108,84,78,72,57,75,75,97,105,78,101,105,99,119,69,113,68,65,86,121,56,104,99,117,80,53,65,80,66,116,65,48,72,75,112,112,89,66,84,113,113,73,77,122,67,87,111,86,49,67,67,100,70,68,85,72,51,66,74,104,55,76,65,57,54,113,69,83,112,47,109,73,119,72,114,113,87,77,47,73,98,105,105,74,47,52,48,117,113,83,70,118,102,88,54,103,119,47,114,48,102,85,79,101,70,85,83,43,117,81,108,120,112,71,72,118,71,83,102,103,120,80,109,90,117,120,121,87,73,78,108,109,78,100,98,114,54,55,115,108,80,71,105,51,118,113,115,49,98,75,48,117,101,104,86,55,47,76,110,109,122,98,76,49,108,122,81,53,72,81,112,103,61,61]},{[115,105,103,110],[108,83,120,102,66,75,116,84,47,106,82,113,82,50,112,117,121,52,119,85,82,56,82,78,81,79,52,75,109,106,111,109,70,120,75,82,81,107,122,79,87,43,115,99,121,86,79,57,83,79,115,122,74,101,102,120,73,47,86,89,110,81,107,55,109,115,113,54,119,84,113,67,55,82,110,76,49,105,107,50,88,98,106,68,120,73,72,48,82,108,90,78,57,50,101,109,51,108,86,76,111,71,73,102,102,52,120,68,119,102,122,109,107,55,48,75,70,79,70,114,87,102,57,101,70,86,83,122,48,119,43,110,107,67,66,111,51,82,43,83,109,105,67,99,121,111,117,108,90,74,83,84,80,117,51,114,115,83,106,71,100,84,86,99,117,89,57,90,122,57,119,61]}],
	decode_data(proplists:get_value("notify_data", QS)),
	QS.
