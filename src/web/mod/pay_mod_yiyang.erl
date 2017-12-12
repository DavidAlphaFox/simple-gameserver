%% @author lxl
%% @doc @todo Add description to pay_mod_yiyang.


-module(pay_mod_yiyang).
-include("common.hrl").

-define(KEY, "A2158EEFOendoemS3J").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_post(),
	case check_sign(QueryString) of
		true ->
					GameRole = proplists:get_value("extend", QueryString), 
					[RoleID,_ServerID|_] = string:tokens(GameRole,"."),
					RoleID2 = list_to_integer(RoleID),
					Amount = proplists:get_value("price", QueryString),			
					Amount2 = trunc(erlang:list_to_float(Amount) * 10),
					Sign = proplists:get_value("sign",QueryString),
					QSL = mochiweb_util:urlencode(QueryString),
					{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
					case RRType of
						115 ->
							pay_gold2(RoleID2,Amount2,QSL,Sign,115),
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply});
						_ ->
							?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QSL]),
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end. 
	
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_yiyang(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.



check_sign(QueryString) ->
	OTN = proplists:get_value("out_trade_no", QueryString),
	Price = proplists:get_value("price",QueryString),
	PStatus = proplists:get_value("pay_status", QueryString),
	Extend = proplists:get_value("extend",QueryString),
	Sign = proplists:get_value("sign",QueryString),
	LSign = util:md5(OTN++Price++PStatus++Extend++?KEY),
	LSign == Sign.

