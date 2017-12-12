%% @author zxystar
%% @doc @todo Add description to pay_mod_az.


-module(pay_mod_az).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QS = Req:parse_post(),
	%% ?ERR("QS:~w",[QS]),
	QS2 = proplists:get_value("data", QS),
	%% ?ERR("QS2:~w",[QS2]),
	AppSec = "hO5y5HdRpg9i5v6nONvHv5QJ",
	QS3 = decrypt_des3_ecb(AppSec,QS2),
	{QueryString} = ejson:decode(QS3),
	%% ?ERR("QueryString:~w",[QueryString]),
	ReqCode = proplists:get_value(<<"code">>, QueryString),
	if ReqCode =:= 1 ->
			AmountBin = proplists:get_value(<<"orderAmount">>, QueryString),
			Amount0 = proplists:get_value(<<"redBagMoney">>, QueryString),
			Amount =
				case Amount0 of
					?undefined ->
						list_to_integer(binary_to_list(AmountBin));
					_ ->
						list_to_integer(binary_to_list(AmountBin)) + list_to_integer(binary_to_list(Amount0))
				end,
			
			GameRoleBin = proplists:get_value(<<"cpInfo">>, QueryString),
			GameRole = binary_to_list(GameRoleBin),
			[_ServerID,RoleID] = string:tokens(GameRole,"."),
			RoleID2 = list_to_integer(RoleID),
			Amount2 = Amount div 10,
			Sign = md5(mochiweb_util:urlencode(proplists:delete(<<"notifyTime">>, QueryString))),
            {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
            case SrcType of
                10 ->
                    pay_gold2(RoleID2,Amount2,QS3,Sign,10),
        			Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        			Req:ok({"text/html; charset=utf-8", Reply});
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;    
		true ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end. 
	
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_az(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.


decrypt_des3_ecb(Key,Cipher) ->
	<<K1:64,K2:64,K3:64>> = list_to_binary(Key), 
	Cipher1 = base64:decode_to_string(Cipher),
	{_,_,_,TextReal,_} = lists:foldl(fun(E,{Key1,Key2,Key3,CipherList,CipherByte})->
							CipherByte2 = [E|CipherByte],
							if length(CipherByte2) < 8 ->
									{Key1,Key2,Key3,CipherList,CipherByte2};
								true ->
									Bin = crypto:block_decrypt(des3_cbc,[Key1,Key2,Key3],<<0:64>>,list_to_binary(lists:reverse(CipherByte2))),
									{Key1,Key2,Key3,CipherList ++ binary_to_list(Bin),[]}
							end
						end,{<<K1:64>>,<<K2:64>>,<<K3:64>>,[],[]}, Cipher1),
	LastWord = lists:last(TextReal),
	if LastWord < 9 ->
			TextLen = length(TextReal) - LastWord,
			lists:sublist(TextReal,TextLen);
		true ->
			TextReal
	end.


