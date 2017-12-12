%% @author lxl
%% @doc @todo Add description to pay_mod_jf


-module(pay_mod_jf).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryStringPost = Req:parse_post(),
	QueryStringGet = Req:parse_qs(),
	Sign = proplists:get_value("sign", QueryStringGet),
	Time = proplists:get_value("time", QueryStringGet),
	case verify_info(Sign, Time) of
		true ->
			[Texts]=proplists:get_keys(QueryStringPost),
			{ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(Texts),
			[{_,_,_,_,Amount,_}] = xmerl_xpath:string("//cost/text()",ParsedDocumentRootElement),
			[{_,_,_,_,GameRole,_}] = xmerl_xpath:string("//order_id/text()", ParsedDocumentRootElement),
			Amount2 = list_to_integer(Amount),
			[_ServerIDT,RoleID|_] = string:tokens(GameRole,"."),
			RoleID2 = erlang:list_to_integer(RoleID),

			{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
			case SrcType of
				24->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					Body = Req:recv_body(),
					MSign = md5(Body),
					QS = mochiweb_util:urlencode(QueryStringGet++QueryStringPost),
					pay_gold2(RoleID2,Amount2,QS,MSign,24);
				_->
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("jf pay with other SrcType:~w ~n",[SrcType])
			end;	
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("jf check_order failed. reason:sign wrong,order:~w",[QueryStringGet++QueryStringPost])
	end.
	
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_jf(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(Sign,Time)->
	AppID = "26792675",
	Sign == md5( AppID ++ Time).

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.


