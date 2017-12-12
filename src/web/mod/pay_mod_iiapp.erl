%% @author lixinglong
%% @doc @todo Add description to pay_mod_iiapp.


-module(pay_mod_iiapp).
-include("common.hrl").
-define(SECKEY, "6e68b112d0e64d94d783bf75362f5154").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	Sign = proplists:get_value("_sign", QueryString),
	LSign = md5(md5(get_original_encode_list(QueryString))++?SECKEY),
	Aid = proplists:get_value("gameExtend", QueryString),
	Amount = proplists:get_value("amount", QueryString),
	NetResult = proplists:get_value("status", QueryString),
	if NetResult =:= "1" ->
			if Sign =:= LSign ->
				  [_ServerID,RoleID|_] = string:tokens(Aid,"."),
				  RoleID2 = list_to_integer(RoleID),
                  {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                  case SrcType of
                    77 ->
    				  Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
    				  Req:ok({"text/html; charset=utf-8", Reply}),
    				  
    				  Amount2 = trunc(list_to_float(Amount))*10,
    				  QS = mochiweb_util:urlencode(QueryString),
    				  pay_gold2(RoleID2,Amount2,QS,Sign,77);
                   _ ->
                      ?ERR("type error ~w",[SrcType]),
                      Reply = ejson:encode({[{<<"result">>,0}]}),
                      Req:ok({"text/html; charset=utf-8", Reply}),
                      ?ERR("iiapp pay failed order:~w",[QueryString])
                  end;   
			  true ->
				  Reply = ejson:encode({[{<<"result">>,0}]}),
				  Req:ok({"text/html; charset=utf-8", Reply}),
				  ?ERR("iiapp pay failed order:~w",[QueryString])
			end;
		true ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_iiapp(RoleID,Amount,Req,Sign,SrcType);
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

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[_H|L] = 
	lists:foldl(fun({A,B},Acc) ->
						case A of
							"_sign" ->
								Acc;
							_ ->
								Acc++"&"++A++"="++B
						end
						end, "", QS2),
	L.
