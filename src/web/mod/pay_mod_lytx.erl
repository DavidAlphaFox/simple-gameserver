%% @author lixinglong
%% @doc @todo Add description to pay_mod_lytx.


-module(pay_mod_lytx).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	OpenID = proplists:get_value("openId", QueryString),
	ServerID = proplists:get_value("serverId", QueryString),
	ServerName = http_uri:decode(proplists:get_value("serverName", QueryString)),
	RoleID = proplists:get_value("roleId",QueryString),
	RoleName = http_uri:decode(proplists:get_value("roleName", QueryString)),
	OrderID = proplists:get_value("orderId", QueryString),
	OrderStatus = proplists:get_value("orderStatus", QueryString),
	PayType = http_uri:decode(proplists:get_value("payType", QueryString)),
	Amount = proplists:get_value("amount",QueryString),
	Remark= http_uri:decode(proplists:get_value("remark", QueryString)),
	CallBackInfo = proplists:get_value("callBackInfo", QueryString),
	PayTime = proplists:get_value("payTime", QueryString),
	PaySUTime = proplists:get_value("paySUTime",QueryString),
	Sign = proplists:get_value("sign", QueryString),
	
	ClientSecLocal = md5(io_lib:format("openId=~s&serverId=~s&serverName=~s&roleId=~s&roleName=~s&orderId=~s&orderStatus=~s&payType=~s&amount=~s&remark=~s&callBackInfo=~s&payTime=~s&paySUTime=~s&app_key=57049f2764e87abf625d70b2161c8ce2"
									  , [OpenID, ServerID,ServerName,RoleID,RoleName,OrderID,OrderStatus,PayType,Amount,Remark,CallBackInfo,PayTime,PaySUTime])),
	
	if OrderStatus =:= "1" ->
			if Sign =:= ClientSecLocal ->
				  [_ServerID,RoleID,_] = string:tokens(CallBackInfo,"."),
				  RoleID2 = list_to_integer(RoleID),
				  {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
				  case SrcType of
				  	63->
				  		Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
				  		Req:ok({"text/html; charset=utf-8", Reply}), 
				  		Amount2 = trunc(list_to_integer(Amount) div 10),
				  		QS = mochiweb_util:urlencode(QueryString),
				  		pay_gold2(RoleID2,Amount2,QS,Sign,63);
				  	_->
				  		Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
				  		Req:ok({"text/html; charset=utf-8", Reply}),
				  		?ERR("lytx pay for other SrcType:~w ~n",[SrcType])
				  end; 
			  true ->
				  Reply = ejson:encode({[{<<"result">>,0}]}),
				  Req:ok({"text/html; charset=utf-8", Reply}),
				  ?ERR("lytx pay failed order:~w",[QueryString])
			end;
		true ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_lytx(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================


md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
