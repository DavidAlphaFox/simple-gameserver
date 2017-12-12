%% @author lxl
%% @doc @todo Add description to pay_mod_ouw


-module(pay_mod_ouw).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = Req:parse_qs(),
	case verify_sign(QueryString) of
		true ->
			Result = proplists:get_value("orderStatus", QueryString),
			case Result of
				"1" ->
					GameRole = proplists:get_value("callbackInfo", QueryString),
					%%?ERR("payoder: ~s",[GameRole]),
					[_ServerID,RoleID|_] = string:tokens(GameRole,"."),
					RoleID2 = list_to_integer(RoleID),
					{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
					case SrcType of
						30->
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							Amount2 = get_amount(QueryString),
							QS = mochiweb_util:urlencode(QueryString),
							MSign = md5(QS),
							pay_gold2(RoleID2,Amount2,QS,MSign,30);
						_->
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("ouw pay for other SrcType:~w ~n",[SrcType])
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("ouw check_order failed. reason:payresult not 0,order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("ouw check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_ouw(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

get_amount(Notify_Data)->
	%Notify_Data2 = decode_data(Notify_Data),
	Value = proplists:get_value("amount", Notify_Data),
	trunc(erlang:list_to_integer(Value) * 10).


verify_sign(QueryString)->
	Sign = proplists:get_value("sign", QueryString),
	Str = get_original_encode_list(QueryString),
	Check = md5(Str ++ "b1261bc6ee002fac"),
	Sign == Check.


get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	lists:foldl(fun({A,B},Acc) ->
						case A of
							"sign" ->
								Acc;
							_ ->
								Acc++A++"="++B
						end
						end, "", QS2).

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

