%% @author lixinglong
%% @doc @todo Add description to pay_mod_pyw.


-module(pay_mod_pyw).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	Data =Req:recv_body(),
	{QueryString} = ejson:decode(Data),
	case verify_info(QueryString) of
		true ->
			CPParam = get_value(QueryString,<<"cp_param">>),
			{LocalParams} = ejson:decode(CPParam),
			GameRole2 =  erlang:binary_to_list(get_value(LocalParams,<<"order_id">>)),
			Amount =  erlang:binary_to_list(get_value(QueryString,<<"amount">>)),
			Amount2 = trunc(list_to_float(Amount) * 10),
			[_ServerID,RoleID|_] = string:tokens(GameRole2,"_"),
			RoleID2 = list_to_integer(RoleID),
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
			case RRType of 
				106 -> 
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					
					QS = mochiweb_util:urlencode(QueryString),
					pay_gold2(RoleID2,Amount2,QS,md5(QS),106);
				_ ->
					QS = mochiweb_util:urlencode(QueryString),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("pyw pay failed order:~w",[QueryString])
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_pyw(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(QueryString)->
	Secret = "c87765579747420f",
	CP_OrderID = erlang:binary_to_list( get_value(QueryString,<<"cp_orderid">>)),
	CH_OrderID =  erlang:binary_to_list(get_value(QueryString,<<"ch_orderid">>)),
	Amount =  erlang:binary_to_list(get_value(QueryString,<<"amount">>)),
	Signature =  erlang:binary_to_list(get_value(QueryString,<<"sign">>)),
	Sign = md5(Secret++CP_OrderID++CH_OrderID++Amount),
	Signature == Sign.

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

