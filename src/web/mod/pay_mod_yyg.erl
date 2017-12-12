%% @author lijunfeng

-module(pay_mod_yyg).
-include("common.hrl").

-define( APPKEY, "dcfa3cf30ebfa40a43b94bfbd1f3f046" ).

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
%%     ?ERR("info:~p.~n",[QueryString]),
    case check_verify( QueryString ) of
        {true,_Orderid,_Rmb,_Account,Cparam,Num} ->
			{Amount,_}= string:to_integer(Num),
            [RoleID] = string:tokens(Cparam, "-"),
			RoleID2 = erlang:list_to_integer(RoleID),
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
			case RRType of
				59 ->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					QS = mochiweb_util:urlencode(QueryString),
					Sign2 = util:md5(proplists:get_value("sign", QueryString)),
					pay_gold2(RoleID2,Amount,QS,Sign2,59);			
				_->
					QS = mochiweb_util:urlencode(QueryString),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		{false,Error} ->
            Reply = ejson:encode({[{<<"result">>,Error}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
             ?ERR("yyg pay failed. reason:sign wrong,order:~w",[QueryString])
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_yyg(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
%% 	?ERR("pay_gold2 yyg err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

check_verify( QueryString ) ->
    Sign = proplists:get_value("sign", QueryString),
	
    Account = proplists:get_value("account", QueryString),
    Orderid = proplists:get_value("orderid", QueryString),
    Rmb = proplists:get_value("rmb", QueryString),
    Num = proplists:get_value("num", QueryString),
    Type = proplists:get_value("type", QueryString),
    Time = proplists:get_value("time", QueryString),
    Game = proplists:get_value("game", QueryString),
    Server = proplists:get_value("server", QueryString),
    Role = proplists:get_value("role", QueryString),
    Itemid = proplists:get_value("itemid", QueryString),
    Price = proplists:get_value("price", QueryString),
    Itemname = proplists:get_value("itemname", QueryString),
    Cparam = proplists:get_value("cparam", QueryString),

	case (not lists:member(?undefined, [Account , Orderid , Rmb , Num , Type , Time 
				 , Game , Server , Role , Itemid , Price , Itemname , Cparam])) of
        true ->
			Md5Result = md5( Account ++ Orderid ++ Rmb ++ Num ++ Type ++ Time 
				 ++ Game ++ Server ++ Role ++ Itemid ++ Price ++ Itemname ++ Cparam ++ ?APPKEY),
			case Sign=:=Md5Result of
				true ->
					{true,Orderid,Rmb,Account,Cparam,Num};
				false->
					{false,-11}
			end;
		false->
			{false,-10}
	end.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
