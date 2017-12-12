%% @author lxl
%% @doc @todo Add description to pay_mod_lenovo.


-module(pay_mod_lenovo_push).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("QS in lenovo:~w",[QueryString]),
	Sign = proplists:get_value("sign", QueryString),
	TransData = proplists:get_value("transdata", QueryString),
	case check_sign(TransData,Sign) of
		true ->
			{DataList} = ejson:decode(TransData),
			case get_value(DataList,<<"result">>) of
				0 ->
					Amount = trunc(proplists:get_value(<<"money">>,DataList) div 10),
					Pt = erlang:binary_to_list(proplists:get_value(<<"cpprivate">>,DataList)),
					[_ServerIDT,RoleID] = string:tokens(Pt,"{},"),
					RoleID2 = erlang:list_to_integer(RoleID),
					{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
					 case SrcType == 21 orelse SrcType == 75 orelse SrcType == 95 orelse SrcType == 29 of
						true->
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							QS = mochiweb_util:urlencode(QueryString),
							Sign2 = util:md5(proplists:get_value("sign", QueryString)),
							pay_gold2(RoleID2,Amount,QS,Sign2	,29);
						_->
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply}),
							?ERR("lenovo_push pay for other SrcType:~w ~n",[SrcType])
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_lenovo(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================


check_sign(TransData,Sign)->
	Key = "MjVCOTAzRDJEQkZGMzY2MDdBMDBBRjIxRTUxN0REMDU4QTlEQjU1RE1UUXlNRFkyT0RreU16azJOell3TlRjME16RXJNalExT0Rrek9EazNNek0wTWpZME1qa3hNall3TmpRM01ESTRORFV4TmpjNE16QTVNREkz",
	Key1 = erlang:binary_to_list(base64:decode(Key)),
	Key2 = lists:sublist(Key1, 41, length(Key1)),
	Key3 = erlang:binary_to_list(base64:decode(Key2)),
	[P_keyT,M_keyT] = string:tokens(Key3, "+"),
	P_key = erlang:list_to_integer(P_keyT),
	M_key = erlang:list_to_integer(M_keyT),
	Sign2 = string:tokens(Sign, " "),
	Md5Sign = lists:foldl(fun(H,Acc) ->
								  I = crypto:mod_exp(erlang:list_to_integer(H,16),P_key, M_key),
								  Acc++get_value_byte(I)
								  end,[],Sign2),
	Md5Sign2 = http_uri:decode(string:strip(Md5Sign)),
	Md5Data = util:md5(TransData),
	Md5Data == Md5Sign2.



get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

get_value_byte(Num)->
	get_value_byte(Num,"").
get_value_byte(0,S)->
	lists:flatten(S);
get_value_byte(In, S)->
	P = In rem 256,
	P1 = io_lib:format("~c", [P]),
	get_value_byte(erlang:trunc(In div 256), P1++S).

test()->
	[{"transdata",
  "{\"exorderno\":\"1a2000003a1395391665\",\"transid\":\"04514032116475214886\",\"waresid\":8,\"appid\":\"20018700000008200187\",\"feetype\":2,\"money\":10,\"result\":0,\"transtype\":0,\"transtime\":\"2014-03-21 16:48:05\",\"count\":1,\"cpprivate\":\"{1},{2000003}\",\"paytype\":5}"},
 {"sign",
  "88159c7a284f4108b685913d9b5cac9f 5bcab0d1a9d3af7ea44b4e911a3eb61b a9c1e8da9bb910a8ef60e442bc0bee33 "}].
