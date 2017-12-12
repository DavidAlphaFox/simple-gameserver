%% @author zxystar
%% @doc @todo Add description to pay_mod_pptv.


-module(pay_mod_pptv).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
	Amount = proplists:get_value("amount", QueryString),
	Sid = proplists:get_value("sid", QueryString),
	RID = proplists:get_value("roid", QueryString),
	Oid = proplists:get_value("oid", QueryString),
	UserName = proplists:get_value("username", QueryString),
	Extra = proplists:get_value("extra", QueryString),
	Time = proplists:get_value("time", QueryString),
	Sign = proplists:get_value("sign", QueryString),
	AppSec = "d1e28bd22c48e1415d2d1c59d42d292b",
	ClientSecLocal = sign([Sid,UserName,RID,Oid,Amount,Time,AppSec]),
	if Sign =:= ClientSecLocal ->
		   [_ServerID,RoleID,_] = string:tokens(Extra,"A"),
		   RoleID2 = list_to_integer(RoleID),
		   {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
		   case SrcType of
		   		61->
		   			Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
		   			Req:ok({"text/html; charset=utf-8", Reply}),
		   			Amount2 = erlang:trunc(erlang:list_to_float(Amount)*10),
		   			QS = mochiweb_util:urlencode(QueryString),
		   			pay_gold2(RoleID2,Amount2,QS,Sign,61);
		   		_->
		   			Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
		   			Req:ok({"text/html; charset=utf-8", Reply}),
		   			?ERR("pptv pay for other SrcType:~w ~n",[SrcType])
		   	end;
	   true ->
		   Reply = ejson:encode({[{<<"result">>,0}]}),
		   Req:ok({"text/html; charset=utf-8", Reply}),
		   ?ERR("pptv pay failed order:~w",[QueryString])
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_pptv(RoleID,Amount,Req,Sign,SrcType);
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
