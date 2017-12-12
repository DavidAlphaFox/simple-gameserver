%% @author lxl
%% @doc @todo Add description to pay_mod_sogou


-module(pay_mod_sogou).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = Req:parse_post(),
	
	case verify_sign(QueryString) of
		true ->
			case check_appid(QueryString) of
				true ->
					GameRole = proplists:get_value("appdata", QueryString),
					[_ServerID,RoleID|_] = string:tokens(http_uri:decode(http_uri:decode(GameRole)),"{},"),
					RoleID2 = list_to_integer(RoleID),
					{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
					QS = mochiweb_util:urlencode(QueryString),
					case RRType of
						23 ->
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							
							Amount2 = get_amount(QueryString),
							Body = Req:recv_body(),
							MSign = md5(Body),
							
							pay_gold2(RoleID2,Amount2,QS,MSign,23);
						_ ->
							?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("sogou check_order failed. reason:appid wrong,order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("sogou check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_sogou(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================
get_amount(QueryString)->
	Value = proplists:get_value("amount1", QueryString),
	trunc(erlang:list_to_integer(Value) * 10).

check_appid(QueryString) ->
	Gid = proplists:get_value("gid", QueryString),
	Gid == "268".

verify_sign(QueryString)->
	Amount1 = "amount1="++proplists:get_value("amount1",QueryString),
	Amount2 = "amount2="++proplists:get_value("amount2",QueryString),
	Appdata = "appdata="++http_uri:encode(http_uri:decode(proplists:get_value("appdata",QueryString))),
	%Appdata = "appdata="++proplists:get_value("appdata",QueryString),
	Date = "date="++proplists:get_value("date", QueryString),
	Gid = "gid="++proplists:get_value("gid", QueryString),
	Oid = "oid="++proplists:get_value("oid", QueryString),
	RealAmount = "realAmount="++proplists:get_value("realAmount", QueryString),
	Role = "role="++proplists:get_value("role", QueryString),
	Sid = "sid="++proplists:get_value("sid", QueryString),
	Time = "time="++proplists:get_value("time", QueryString),
	Uid = "uid="++proplists:get_value("uid", QueryString),
	Sign = proplists:get_value("auth", QueryString),
	PayKey = "{7D334D88-66D6-4B23-971D-18A641C9E606}",
	Str = Amount1 ++ "&" ++ Amount2 ++ "&" ++ Appdata ++ "&" ++ Date ++ "&" ++
			  Gid ++ "&" ++ Oid ++ "&" ++ RealAmount ++ "&" ++ Role ++ "&" ++
			  Sid ++ "&" ++ Time ++ "&" ++ Uid ++ "&" ++ PayKey,
	md5(Str) == Sign.

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

