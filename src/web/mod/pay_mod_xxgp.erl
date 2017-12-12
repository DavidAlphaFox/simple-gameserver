%% @author lxl
%% @doc @todo Add description to pay_mod_xxgp.


-module(pay_mod_xxgp).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:QS=~p\n",[QueryString]),
	Amount = proplists:get_value("money", QueryString),
	SerialNumber = proplists:get_value("serialNumber",QueryString),
	Status = proplists:get_value("status", QueryString),
	T = proplists:get_value("t", QueryString),
	
	Ext = proplists:get_value("reserved", QueryString),
	Sign = proplists:get_value("sign", QueryString),
	Key = "6SYWX69DRDVGRN8M1PKQDSPBBJ31UPTYNEMX6ETKF7QRMBOZIFXHRXM9ANNIB7EZ",
	MSign = md5(SerialNumber++Amount++Status++T ++ Key),

	
	if Sign =:= MSign ->
		   [_ServerID,RoleID|_] = string:tokens(Ext,"{}"),
		   RoleID2 = list_to_integer(RoleID),
		   {RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
		   case RRType of
			   96 ->
				   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
				   Req:ok({"text/html; charset=utf-8", Reply}),
				   
				   Amount2 = trunc(list_to_float(Amount)*10),
				   QueryString2 = proplists:delete("sign", proplists:delete("t", QueryString)),
				   QS = mochiweb_util:urlencode(QueryString2),
				   pay_gold2(RoleID2,Amount2,QS,md5(QS),96);
			   _ ->
				   QSL = mochiweb_util:urlencode(QueryString),
				   ?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QSL]),
				   Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
				   Req:ok({"text/html; charset=utf-8", Reply})
		   end;
	   true ->
		   Reply = ejson:encode({[{<<"result">>,0}]}),
		   Req:ok({"text/html; charset=utf-8", Reply}),
		   ?ERR("xxgp pay failed order:~w",[QueryString])
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_xxgp(RoleID,Amount,Req,Sign,SrcType);
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
