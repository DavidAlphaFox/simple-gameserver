%% @author lxl
%% @doc @todo Add description to pay_mod_kw.


-module(pay_mod_kw).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),

	Amount = proplists:get_value("money", QueryString),
	UserID = proplists:get_value("user_id", QueryString),
	OrderNo = proplists:get_value("order_no", QueryString),
	Ext = proplists:get_value("ext_order_num", QueryString),
	Sign = proplists:get_value("token", QueryString),
	Key = "16d564e2bfd2e865523f774db8d25f32",
	MSign = md5(UserID ++ Amount ++ OrderNo ++ Key),
	
	if Sign =:= MSign ->
		   [_ServerID,RoleID|_] = string:tokens(Ext,"A"),
		   RoleID2 = list_to_integer(RoleID),
		   {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
		   case SrcType of
		   		36->
		   			Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
		   			Req:ok({"text/html; charset=utf-8", Reply}), 
		   			Amount2 = list_to_integer(Amount)*10,
		   			QS = mochiweb_util:urlencode(QueryString),
		   			pay_gold2(RoleID2,Amount2,QS,md5(QS),36);
		   		_->
		   			Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
		   			Req:ok({"text/html; charset=utf-8", Reply}),
		   			?ERR("kw pay for other SrcType:~w ~n",[SrcType])
		   	end; 	   			
	   true ->
		   Reply = ejson:encode({[{<<"result">>,0}]}),
		   Req:ok({"text/html; charset=utf-8", Reply}),
		   ?ERR("kw pay failed order:~w",[QueryString])
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_kw(RoleID,Amount,Req,Sign,SrcType);
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
