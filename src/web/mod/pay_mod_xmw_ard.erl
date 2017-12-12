%% @author lxl
%% @doc @todo Add description to pay_mod_xmw.


-module(pay_mod_xmw_ard).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	Money = proplists:get_value("amount",QueryString),
	Serial = proplists:get_value("serial", QueryString),
	AppOrderID = proplists:get_value("app_order_id",QueryString),
	AppUserID = proplists:get_value("app_user_id",QueryString),
	Status = proplists:get_value("status",QueryString),
	Sign = proplists:get_value("sign",QueryString),
		Sub = proplists:get_value("app_subject",QueryString),
	SignString = "amount="++Money++"&app_order_id="++AppOrderID++"&app_subject="++Sub++"&app_user_id="++AppUserID++"&serial="++Serial++"&status="++Status++"&client_secret=39b46316eb67b226afa1fc3c0cb8dfe8",
	SignLocal = util:md5(SignString),
	[_ServerID,RoleID|_] = string:tokens(AppOrderID,"."),
	RoleID2 = list_to_integer(RoleID),
	case Status of 
		"success" ->
			case SignLocal of 
				Sign ->
					Amount = list_to_integer(Money) * 10,
					
					[_ServerID,RoleID|_] = string:tokens(AppOrderID,"."),
					RoleID2 = list_to_integer(RoleID),
					{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
					case RRType of
						117 ->
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							pay_gold2(RoleID2,Amount,SignString,Sign,117);
						_ ->
							QSL = mochiweb_util:urlencode(QueryString),
							?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QSL]),
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_xmw(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================

sign(StrList) ->
	md5(sign2(StrList)).

sign2([A]) ->
	A;
sign2([A,B]) ->
	A++B;
sign2([]) ->
	"";
sign2([A|T]) ->
	A++sign2(T).

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).



