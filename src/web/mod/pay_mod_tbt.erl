%% @author lxl
%% @doc @todo Add description to pay_mod_tbt.


-module(pay_mod_tbt).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
	Key = "&key=I@Kh5E7Ro2qANkax@UJ5uE7od2AYNkxH",
	Source = proplists:get_value("source", QueryString),
	Trade_no = proplists:get_value("trade_no", QueryString),
	Amount = proplists:get_value("amount", QueryString),
	Partner = "&partner=" ++ proplists:get_value("partner", QueryString),
	Paydes = proplists:get_value("paydes", QueryString), 
	Debug = proplists:get_value("debug", QueryString), 
	Sign = proplists:get_value("sign", QueryString),
	TbOrder = proplists:get_value("tborder", QueryString),
	ParamString = "source=" ++ Source ++ "&trade_no=" ++ Trade_no ++ "&amount=" ++ Amount ++
					  Partner ++ "&paydes=" ++ Paydes ++ "&debug=" ++ Debug ++ "&tborder=" ++ TbOrder ++ Key,
	MSign = sign([ParamString]),
	if Sign =:= MSign ->
		   [_ServerID,RoleID] = string:tokens(Paydes,"."),
		   RoleID2 = list_to_integer(RoleID),
		   {RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
		   QS = mochiweb_util:urlencode(QueryString),
		   case RRType of
			   15 ->
				   Reptbt = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
				   Req:ok({"text/html; charset=utf-8", Reptbt}),
				   
				   Amount2 = trunc(erlang:list_to_integer(Amount) / 10),
				   
				   pay_gold2(RoleID2,Amount2,QS,Sign,15);
			   _ ->
				   ?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
				   Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
				   Req:ok({"text/html; charset=utf-8", Reply})
		   end;
	   true ->
		   Reptbt = ejson:encode({[{<<"result">>,0}]}),
		   Req:ok({"text/html; charset=utf-8", Reptbt}),
		   ?ERR("tbt pay failed ,Reason:sign wrong,order:~w",[QueryString])
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_tbt(RoleID,Amount,Req,Sign,SrcType);
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
