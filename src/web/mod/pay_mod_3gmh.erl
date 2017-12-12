%% @author lxl
%% @doc @todo Add description to pay_mod_3gmh


-module(pay_mod_3gmh).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = mochiweb_util:parse_qs(erlang:binary_to_list(Req:recv_body())),
	%?ERR("info:~w",[QueryString]),
	Data = proplists:get_value("data", QueryString),
	Sign = proplists:get_value("key", QueryString),
	{ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(Data),
	[{_,_,_,_,OrderID,_}] = xmerl_xpath:string("//orderid/text()",ParsedDocumentRootElement),
	[{_,_,_,_,GameID,_}] = xmerl_xpath:string("//gameid/text()",ParsedDocumentRootElement),
	[{_,_,_,_,Token,_}] = xmerl_xpath:string("//token/text()",ParsedDocumentRootElement),
	case verify_info(Sign, OrderID,GameID,Token) of
		true ->
			[{_,_,_,_,Access,_}] = xmerl_xpath:string("//access/text()",ParsedDocumentRootElement),
			case Access of
				"1" ->
					[{_,_,_,_,Amount,_}] = xmerl_xpath:string("//paytotalfee/text()",ParsedDocumentRootElement),
					[{_,_,_,_,GameRole,_}] = xmerl_xpath:string("//cporderid/text()", ParsedDocumentRootElement),
					%%Amount2 = trunc(list_to_integer(Amount) *10),
					Amount2 = trunc(list_to_float(Amount) * 10),
					[_ServerIDT,RoleID|_] = string:tokens(GameRole,"."),
					RoleID2 = erlang:list_to_integer(RoleID),
                    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                    case SrcType of
                        65->
        					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        					Req:ok({"text/html; charset=utf-8", Reply}),
        					
        					Body = Req:recv_body(),
        					MSign = md5(Body),
        					QS = mochiweb_util:urlencode(QueryString),
        					pay_gold2(RoleID2,Amount2,QS,MSign,65);
                        _ ->
                            ?ERR("type error ~w",[SrcType]),
                            Reply = ejson:encode({[{<<"result">>,0}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("3gmh check_order failed. reason:status wrong,order:~w",[QueryString])
                    end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("3gmh check_order failed. reason:status wrong,order:~w",[QueryString])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("3gmh check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.
	
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_3gmh(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(Sign,OrderID,"2636"=GameID,Token)->
	Sign == md5(io_lib:format("~s_~s_~s_~s", ["2324qubapikaqiu",OrderID,GameID,Token])).

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.


