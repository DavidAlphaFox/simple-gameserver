%% @author zxystar
%% @doc @todo Add description to pay_mod_lt.


-module(pay_mod_lt).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = erlang:binary_to_list(Req:recv_body()),
	{XmlRoot, _RemainingText = ""} = xmerl_scan:string(QueryString),
	RallbackReq = get_xml_node_value("callbackReq",XmlRoot),
	HRet = get_xml_node_value("hRet", XmlRoot),
	Status = get_xml_node_value("status",XmlRoot),
	case HRet++Status of
		"000000" ->
			Order = get_xml_node_value("orderid",XmlRoot),
			OrderTime = get_xml_node_value("ordertime", XmlRoot),
			ConsumeCode = get_xml_node_value("consumeCode",XmlRoot),
			PayFee = get_xml_node_value("payfee", XmlRoot),
			PayType = get_xml_node_value("payType", XmlRoot),
			Cpid = get_xml_node_value("cpid",XmlRoot),
			Appid = get_xml_node_value("appid",XmlRoot),
			Fid = get_xml_node_value("fid",XmlRoot),

			Sign = get_xml_node_value("signMsg", XmlRoot),
			
			Str = io_lib:format("orderid=~s&ordertime=~s&cpid=~s&appid=~s&fid=~s&consumeCode=~s&payfee=~s&payType=~s&hRet=0&status=00000&Key=5db94234fe2921205f5e",
								[Order,OrderTime,Cpid,Appid,Fid,ConsumeCode,PayFee,PayType]),
			Md5Sign = util:md5(Str),
			Str2 = io_lib:format("orderid=~s&ordertime=~s&cpid=~s&appid=~s&fid=~s&consumeCode=~s&payfee=~s&payType=~s&hRet=0&status=00000&Key=c36ea31cc0e7f298de87",
								 [Order,OrderTime,Cpid,Appid,Fid,ConsumeCode,PayFee,PayType]),
			Md5Sign2 = util:md5(Str2),
			case Sign == Md5Sign2 orelse Sign == Md5Sign of
				true ->
						[Info|_] = string:tokens(Order,"A"),
						ServerID = string:substr(Info,3,4),
						RoleID = string:substr(Info,7),
						RoleID2 = list_to_integer(RoleID),
						ServerID2 = list_to_integer(ServerID), 
						Amount2 = list_to_integer(PayFee),
						Amount3 = Amount2 div 10,
						{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
						case SrcType of
							74->
								Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
								Req:ok({"text/html; charset=utf-8", Reply}),
					    		QueryString2 = mochiweb_util:parse_qs(QueryString),
								QS = mochiweb_util:urlencode(QueryString2),
								pay_gold2(RoleID2,Amount3,QS,md5(QS),74);
							_->
								Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
								Req:ok({"text/html; charset=utf-8", Reply}),
								?ERR("lt pay for other SrcType:~w ~n",[SrcType])
						end;
				_ ->
						Reply = ejson:encode({[{<<"result">>,0}]}),
						Req:ok({"text/html; charset=utf-8", Reply}),
						?ERR("vivo pay failed order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_lt(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================
get_xml_node_value(Node,Root)->
	Node1 = "//" ++ Node ++ "/text()",
	case catch xmerl_xpath:string(Node1, Root) of
		[{_,_,_,_,Value,_}] ->
			Value;
		_ ->
			""
	end.
	
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).




test() -> "<?xml version=\"1.0\" encoding=\"UTF-8\"?><callbackReq><orderid>00A5010010A11A1420017381</orderid><ordertime>20141231171626</ordertime><cpid>90166060312</cpid><appid>9016606031220140923121427205400</appid><fid>12243</fid><consumeCode>9016606031220140923121427205400008</consumeCode><payfee>10</payfee><payType>1</payType><hRet>0</hRet><status>00000</status><signMsg>18d6d7d0ed945103b1432c06e6a7f5e9</signMsg></callbackReq>".