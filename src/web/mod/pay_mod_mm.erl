-module(pay_mod_mm).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-include("common.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-define(SRCTYPE, 67).
-define(APPKEY,"4272C6D23724A2FA").
-define(APPID,"300008522557").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = erlang:binary_to_list(Req:recv_body()),
	{XmlRoot, _RemainingText = ""} = xmerl_scan:string(QueryString),
	Md5Sign = get_xml_node_value("MD5Sign",XmlRoot),
	OrderType = get_xml_node_value("OrderType", XmlRoot),
	ExData = get_xml_node_value("ExData", XmlRoot),
	TotalPrice = get_xml_node_value("TotalPrice", XmlRoot),
	ChannelID = get_xml_node_value("ChannelID", XmlRoot),
	PayCode = get_xml_node_value("PayCode", XmlRoot),
	OrderID = get_xml_node_value("OrderID", XmlRoot),
	AppID = get_xml_node_value("AppID", XmlRoot),
	OrderPayMent = get_xml_node_value("OrderPayment", XmlRoot),
	SubsNum = get_xml_node_value("SubsNumb", XmlRoot),
	SubsSeq = get_xml_node_value("SubsSeq", XmlRoot),
	Amount = 
    %% OrderPayment 为4代表第三方支付(前3为三大运营商)
		case OrderPayMent of
			"4" ->
				trunc(list_to_integer(TotalPrice) div 10);
			_ ->
				case SubsSeq of
					SubsNum ->
						trunc(list_to_integer(TotalPrice) div 10);
					_ ->
						0
				end
		end, 
	case check_sign(PayCode,OrderID, ChannelID,Md5Sign,AppID,OrderType) of
		true ->
			[_ServerID,RoleID|_] = string:tokens(ExData, "A"),
			{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(list_to_integer(RoleID)),
			case SrcType of
				?SRCTYPE->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					pay_gold2(list_to_integer(RoleID),Amount,QueryString,Md5Sign,?SRCTYPE);
				_->
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("mm pay for other SrcType:~w ~n",[SrcType])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_mm(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

check_sign(PayCode,OrderID, ChannelID,Md5Sign,AppID,_OrderType)->
	Sign = md5(OrderID ++ "#" ++ ChannelID ++ "#" ++ PayCode ++ "#" ++ ?APPKEY),
	if Sign == Md5Sign ->
		   if 
               %% OrderType =:= "0" ->
				%%    false;
				AppID =:= ?APPID->
				  true ;
			  true ->
				  false
		   end;
	   true ->
		   false
	end.

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0B",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_xml_node_value(Node,Root)->
	Node1 = "//" ++ Node ++ "/text()",
	case catch xmerl_xpath:string(Node1, Root) of
		[{_,_,_,_,Value,_}] ->
			Value;
		_ ->
			""
	end.

