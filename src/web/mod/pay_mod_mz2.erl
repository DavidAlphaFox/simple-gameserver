%% @author lixinglong


-module(pay_mod_mz2).
-include("common.hrl").

-export([pay_gold/1]).

-define(ACCOUNT_TYPE_MZ, 35).
%AppID：1878322
%AppKey：c06ff97942f448caaecd2f01a1de4ea8
%AppSecret：P0tarrGtlyI85AbNBMebIZSSKoxAtpHI

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
 	QS = Req:parse_qs(),
	NotifyTime = proplists:get_value("notify_time",QS),
	NotifyID = proplists:get_value("notify_id",QS),
	OrderID = proplists:get_value("order_id",QS),
	UID = proplists:get_value("uid",QS),
	PartnerID = proplists:get_value("partner_id",QS),
	CpOrderID=proplists:get_value("cp_order_id",QS),
	ProductID=proplists:get_value("product_id",QS),
	ProductUnit=proplists:get_value("product_unit",QS),
	BuyAmount=proplists:get_value("buy_amount",QS),
	ProductPerPrice=proplists:get_value("product_per_price",QS),
	TotalPrice = proplists:get_value("total_price",QS),
	TradeStatus=proplists:get_value("trade_status",QS),
	CreateTime=proplists:get_value("create_time",QS),
	PayTime=proplists:get_value("pay_time",QS),
	PayType=proplists:get_value("pay_type",QS),
	UserInfo=proplists:get_value("user_info",QS),
	Sign = proplists:get_value("sign",QS),
	
	Str = io_lib:format("app_id=1878322&buy_amount=~s&cp_order_id=~s&create_time=~s&notify_id=~s&notify_time=~s&order_id=~s&partner_id=~s&pay_time=~s&pay_type=~s&product_id=~s&product_per_price=~s&product_unit=~s&total_price=~s&trade_status=~s&uid=~s&user_info=~s:P0tarrGtlyI85AbNBMebIZSSKoxAtpHI",
						[BuyAmount,CpOrderID,CreateTime,NotifyID,NotifyTime,OrderID,PartnerID,PayTime,PayType,ProductID,ProductPerPrice,ProductUnit,TotalPrice,TradeStatus,UID,UserInfo]),
	
	case util:md5(Str) of
		Sign ->
			Amount = trunc(erlang:list_to_float(TotalPrice) * 10),
			[_,_ServerIDT,RoleIDT] = string:tokens(CpOrderID,"A"),
			RoleID = list_to_integer(RoleIDT),
			
			QS2 = mochiweb_util:urlencode(QS),
			Sign2 = util:md5("mz_1878322_"++NotifyID++CpOrderID++OrderID),
			{AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
			case SrcType of
				?ACCOUNT_TYPE_MZ->
					pay_gold2(RoleID, Amount, QS2, Sign2, ?ACCOUNT_TYPE_MZ),
					Reply = ejson:encode({[{<<"result">>, 1}, {<<"accid">>, AccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply});
				_->
					Reply = ejson:encode({[{<<"result">>, 5}, {<<"accid">>, AccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("mz2 pay for other SrcType:~w ~n",[SrcType])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>, 0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_mz(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================
