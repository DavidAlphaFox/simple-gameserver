%% @author lxl
%% @doc @todo Add description to pay_mod_sina.


-module(pay_mod_sina).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("Req:~p,QS=~p\n",[Req,QueryString]),
	Status = proplists:get_value("status", QueryString),
	if Status =/= "F" ->
		   case check_sign(QueryString) of
			   true ->
				   AmountT = proplists:get_value("amount", QueryString),
				   %Actual_amount = proplists:get_value("actual_amount", QueryString),
				   Pt = proplists:get_value("pt",QueryString),
				   [_ServerIDT,RoleID] = string:tokens(Pt,"{},"),
				   RoleID2 = erlang:list_to_integer(RoleID),
				   QS = mochiweb_util:urlencode(QueryString),
				   {RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
				   case RRType of
					   20 ->
						   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
						   Req:ok({"text/html; charset=utf-8", Reply}),
						   
						   Amount = trunc(erlang:list_to_integer(AmountT) div 10),
						   Sign = proplists:get_value("signature", QueryString),
						   pay_gold2(RoleID2,Amount,QS,Sign	,20);
					   _ ->
						   ?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
						   Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
						   Req:ok({"text/html; charset=utf-8", Reply})
				   end;
			   _ ->
				   Reply = ejson:encode({[{<<"result">>,0}]}),
				   Req:ok({"text/html; charset=utf-8", Reply})
		   end;
	   true ->
		   Reply = ejson:encode({[{<<"result">>,0}]}),
		   Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_sina(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QS)->
	AppSecret = "|653775213f9562e759c187606542436b",
	Source = proplists:get_value("source", QS),
	AppKey = "1922657997",
	case Source of
		AppKey ->
			Signature = proplists:get_value("signature", QS),
			Sign = crypto:sha(get_original_encode_list(QS)++AppSecret),
			case hexstring(Sign) of
				Signature ->
					true;
				_ ->
					false
			end;
		_ ->
			false
	end.

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[_H|L] = 
		lists:foldl(fun({A,B},Acc) ->
							case A of
								"signature" ->
									Acc;
								_ ->
									Acc++"|"++A++"|"++B
							end
					end, "", QS2),
	L.

hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X])).

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
