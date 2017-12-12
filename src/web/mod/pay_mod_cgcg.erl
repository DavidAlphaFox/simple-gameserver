%% @author lxl
%% @doc @todo Add description to pay_mod_cgcg.


-module(pay_mod_cgcg).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
	PartnerTransactionNo = proplists:get_value("partnerTransactionNo",QueryString),
	StatusCode = proplists:get_value("statusCode",QueryString),
	OrderPrice = proplists:get_value("orderPrice",QueryString),
	Sign = proplists:get_value("sign",QueryString),

	AppSec = "&30dfd105a0194821b565e254513d7123",
	ClientSecLocal = md5(get_original_encode_list(QueryString)++AppSec),
	%% ?ERR("md5:~w,md5local:~w",[ClientSec,ClientSecLocal]),
	if StatusCode =:= "0000" ->
		   if Sign =:= ClientSecLocal ->
				  [_,_ServerID,RoleID] = string:tokens(PartnerTransactionNo,"A"),
				  RoleID2 = list_to_integer(RoleID),
                  {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                  case SrcType of
                      99 ->
        				  Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        				  Req:ok({"text/html; charset=utf-8", Reply}),
        				  
        				 Amount2 = erlang:trunc(list_to_float(OrderPrice)*10),
        				  QS = mochiweb_util:urlencode(QueryString),
        				  pay_gold2(RoleID2,Amount2,QS,Sign,99);
                      _ ->
                          ?ERR("type error ~w",[SrcType]),
                          Reply = ejson:encode({[{<<"result">>,0}]}),
                          Req:ok({"text/html; charset=utf-8", Reply}),
                          ?ERR("cgcg pay failed order:~w",[QueryString])
                  end;
			  true ->
				  Reply = ejson:encode({[{<<"result">>,0}]}),
				  Req:ok({"text/html; charset=utf-8", Reply}),
				  ?ERR("cgcg pay failed order:~w",[QueryString])
			end;
		true ->
			Reply = ejson:encode({[{<<"result">>,1}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_cgcg(RoleID,Amount,Req,Sign,SrcType);
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
    [H|L] = 
    lists:foldl(fun({A,B},Acc) ->
                        case A of
                            "sign" ->
                                Acc;
                            _ ->
                                case B of 
                                    "" -> Acc;
                                _ ->Acc++"&"++A++"="++B
                                end
                        end
                        end, "", QS2),
    L.
