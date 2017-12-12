%% @author lixinglong
%% @doc @todo Add description to pay_mod_ck.


-module(pay_mod_ck).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	ClientSec = proplists:get_value("sign",QueryString),
	OriStr = get_original_encode_list(QueryString)++"&cb2456f528bfcdbba2fc53f04774f169",
	NetResult = proplists:get_value("status",QueryString),
	ClientSecLocal = md5(OriStr),
	
	Amount = proplists:get_value("total_fee", QueryString),
	Aid = proplists:get_value("misc",QueryString),

	if NetResult =:= "1" ->
			if ClientSec =:= ClientSecLocal ->
					[RoleID|_] = string:tokens(Aid,"A"),
				    RoleID2 = list_to_integer(RoleID),
                    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                    case SrcType of
                        86 ->
        				  Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        				  Req:ok({"text/html; charset=utf-8", Reply}),
        				  Amount2 = trunc(list_to_float(Amount)*10),
        				  QS = mochiweb_util:urlencode(QueryString),
        				  pay_gold2(RoleID2,Amount2,QS,ClientSec,86);
                        _ ->
                            ?ERR("type error ~w",[SrcType]),
                            Reply = ejson:encode({[{<<"result">>,1}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("ck pay failed order:~w",[QueryString])
                    end;    
			  true ->
				  Reply = ejson:encode({[{<<"result">>,0}]}),
				  Req:ok({"text/html; charset=utf-8", Reply}),
				  ?ERR("ck pay failed order:~w",[QueryString])
			end;
		true ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_ck(RoleID,Amount,Req,Sign,SrcType);
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
							"sign" ->
								Acc;
							"create_time" ->
								[B1,B2] = string:tokens(B," "),
								Acc++"&"++http_uri:encode(A)++"="++http_uri:encode(B1)++"+"++http_uri:encode(B2);
							"pay_time" ->
								[B1,B2] = string:tokens(B," "),
								Acc++"&"++http_uri:encode(A)++"="++http_uri:encode(B1)++"+"++http_uri:encode(B2);
							_ ->
								Acc++"&"++http_uri:encode(A)++"="++http_uri:encode(B)
						end
						end, "", QS2),
	L.

