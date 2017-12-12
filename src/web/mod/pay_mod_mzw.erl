%% @author lxl
%% @doc @todo Add description to pay_mod_mzw.


-module(pay_mod_mzw).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	case check_sign(QueryString) of
		true ->
			AmountT = proplists:get_value("money", QueryString),
			Amount = list_to_integer(AmountT) * 10,
			Pt = proplists:get_value("extern",QueryString),
			[_ServerIDT,RoleID|_] = string:tokens(Pt,"A"),
			RoleID2 = erlang:list_to_integer(RoleID),
			{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
			case SrcType of
				38->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					QS = mochiweb_util:urlencode(QueryString),
					Sign = proplists:get_value("sign", QueryString),
					pay_gold2(RoleID2,Amount,QS,Sign,38);
				_->
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("mzw pay for other SrcType:~w ~n",[SrcType])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_mzw(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QueryString)->
	OrderID = proplists:get_value("orderID",QueryString),
	ProductName = proplists:get_value("productName",QueryString),
	ProductDesc = proplists:get_value("productDesc",QueryString),
	ProductID = proplists:get_value("productID", QueryString),
	Amount = proplists:get_value("money", QueryString),
	Uid = proplists:get_value("uid", QueryString),
	Extern = proplists:get_value("extern", QueryString),
	Sign = proplists:get_value("sign",QueryString),
	util:md5("d6b385ac420c6b8052e21214ca3744bd"++OrderID++ProductName++ProductDesc++ProductID++Amount++Uid++Extern++"53a16682c9c31") == Sign.


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

test()->
	[{"order_id",
	  "52e0d474da4a65925"},
	 {"amount","1"},
	 {"order_uid",
	  "1541687951"},
	 {"source",
	  "1922657997"},
	 {"actual_amount","1"},
	 {"pt",
	  "{1},{2000003}"},
	 {"signature",
	  "3172c984ea58c46656cd682bd4f03b7d70efbb2f"}].
