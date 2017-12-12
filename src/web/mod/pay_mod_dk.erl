%% @author zxystar
%% @doc @todo Add description to pay_mod_dk.


-module(pay_mod_dk).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
	Amount = proplists:get_value("amount", QueryString),
	CardType = proplists:get_value("cardtype", QueryString),
	OrderID = proplists:get_value("orderid", QueryString),
	NetResult = proplists:get_value("result", QueryString),
	Timetamp = proplists:get_value("timetamp", QueryString),
	Aid = proplists:get_value("aid", QueryString),
	ClientSec = proplists:get_value("client_secret", QueryString),
	AppSec = "2f4dc6204c8fb62d9728485a34bdc0d0",
	ClientSecLocal = sign([Amount,CardType,OrderID,NetResult,Timetamp,AppSec,Aid]),
	if NetResult =:= "1" ->
		   if ClientSec =:= ClientSecLocal ->
				  [_ServerID,RoleID] = string:tokens(Aid,"."),
				  RoleID2 = list_to_integer(RoleID),
                  {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                  case SrcType of
                      8 ->
        				  Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        				  Req:ok({"text/html; charset=utf-8", Reply}),
        				  
        				  Amount2 = list_to_integer(Amount)*10,
        				  QS = mochiweb_util:urlencode(QueryString),
        				  pay_gold2(RoleID2,Amount2,QS,ClientSec,8);
                      _ ->
                          ?ERR("type error ~w",[SrcType]),
                          Reply = ejson:encode({[{<<"result">>,0}]}),
                          Req:ok({"text/html; charset=utf-8", Reply}),
                          ?ERR("dk pay failed order:~w",[QueryString])
                  end;    
			  true ->
				  Reply = ejson:encode({[{<<"result">>,0}]}),
				  Req:ok({"text/html; charset=utf-8", Reply}),
				  ?ERR("dk pay failed order:~w",[QueryString])
			end;
		true ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_dk(RoleID,Amount,Req,Sign,SrcType);
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
