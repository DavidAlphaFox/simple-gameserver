%% @author zxystar
%% @doc @todo Add description to pay_mod_mi.


-module(pay_mod_mi).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	AppId = proplists:get_value("appId", QueryString),
%%	CpOrderId = proplists:get_value("cpOrderId", QueryString),
	CpUserInfo = proplists:get_value("cpUserInfo", QueryString),
%%	Uid = proplists:get_value("uid", QueryString),
%%	OrderId = proplists:get_value("orderId", QueryString),
	OrderStatus = proplists:get_value("orderStatus", QueryString),
	PayFee = proplists:get_value("payFee", QueryString),
%%	ProductCode = proplists:get_value("productCode", QueryString),
%%	ProductName = proplists:get_value("productName", QueryString),
%%	ProductCount = proplists:get_value("productCount", QueryString),
%%	PayTime = proplists:get_value("payTime", QueryString),
	Signature = proplists:get_value("signature", QueryString),
	%%	OrderConsumeType = proplists:get_value("orderConsumeType", QueryString),
	%%	ParterGiftConsume = proplists:get_value("partnerGiftConsume", QueryString),
	AppIdLocal = "2882303761517227884",
	AppKey = "TN6Fj3XQd+oXEw26LTjilw==",
	
	<<SignTemp:160/integer>> = crypto:hmac('sha', AppKey, get_original_encode_list(QueryString)),
	SignLocal = lists:flatten(io_lib:format("~40.16.0b", [SignTemp])),
	%% ?ERR("md5:~w,md5local:~w",[ClientSec,ClientSecLocal]),
	if SignLocal =:= Signature andalso AppIdLocal =:= AppId ->
		   case OrderStatus of
			   "TRADE_SUCCESS" ->
				   [_ServerID,RoleID] = string:tokens(CpUserInfo,"."),
				   RoleID2 = list_to_integer(RoleID),
				   {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
				   case SrcType of
				    9->
				   		Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
				   		Req:ok({"text/html; charset=utf-8", Reply}),
				   		Amount2 = list_to_integer(PayFee) div 10,
				   		QS = mochiweb_util:urlencode(QueryString),
				   		pay_gold2(RoleID2,Amount2,QS,Signature,9);
				    _->
				   		Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
				   		Req:ok({"text/html; charset=utf-8", Reply}),
				   		?ERR("mi pay for other SrcType:~w ~n",[SrcType])
				   end;
			   true ->
				   Reply = ejson:encode({[{<<"result">>,0}]}),
				   Req:ok({"text/html; charset=utf-8", Reply}),
				   ?ERR("mi pay failed order:~w",[QueryString])
		   end;
	   true ->
		   Reply = ejson:encode({[{<<"result">>,0}]}),
		   Req:ok({"text/html; charset=utf-8", Reply}),
		   ?ERR("mi pay failed order:~w",[QueryString])
	end.


pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_mi(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================
get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[_H|L] = 
		lists:foldl(fun({A,B},Acc) ->
							case A of
								"signature" ->
									Acc;
								_ ->
									Acc++"&"++A++"="++B
							end
					end, "", QS2),
	L.

