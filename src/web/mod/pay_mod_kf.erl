%% @author lxl
%% @doc @todo Add description to pay_mod_kf


-module(pay_mod_kf).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = Req:parse_post(),
	Sign = proplists:get_value("sign", QueryString),
	Result = proplists:get_value("result",QueryString),
	case verify_sign(QueryString, Sign) of
		true ->
			case check_pay(Result) of
				true ->
					GameRole = proplists:get_value("extend", QueryString),
					[_ServerID,RoleID|_] = string:tokens(GameRole,"."),
					RoleID2 = list_to_integer(RoleID),
					{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
					case SrcType of
						105->
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							Amount2 = erlang:trunc(erlang:list_to_float(proplists:get_value("amount", QueryString)) * 10),
							Body = Req:recv_body(),
							MSign = md5(Body),
							QS = mochiweb_util:urlencode(QueryString),
							pay_gold2(RoleID2,Amount2,QS,MSign,105);
						_->
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("kf pay for other SrcType:~w ~n",[SrcType])
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("kf check_order failed. reason:result = fail,order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("kf check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_kf(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================
check_pay("0") ->
	true;
check_pay(_) ->
	false.

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[_H|L] = 
	lists:foldl(fun({A,B},Acc) ->
						case A of
							"sign" ->
								Acc;
							_ ->
								Acc++"&"++A++"="++http_uri:encode(B)
						end
						end, "", QS2),
	L.

verify_sign(QueryString, Sign) ->
	Str = get_original_encode_list(QueryString),
	LSign = md5(md5(Str)++"e5Je5r0RnXwK7QyfSte8BCaJXN9cCYOw"),
	LSign == Sign.



md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

