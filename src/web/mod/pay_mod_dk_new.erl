%% @author lxl
%% @doc @todo Add description to pay_mod_dk.


-module(pay_mod_dk_new).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	AppID = proplists:get_value("AppID", QueryString),
	Sign = proplists:get_value("Sign", QueryString),
	OrderSerial = proplists:get_value("OrderSerial", QueryString),
	CooperatorOrderSerial = proplists:get_value("CooperatorOrderSerial", QueryString),
	Content = proplists:get_value("Content", QueryString),
	{ContentJson} = ejson:decode((base64:decode(Content))),
	Amount = binary_to_list(get_value(ContentJson,<<"OrderMoney">>)),
	NetResult = get_value(ContentJson,<<"OrderStatus">>),
	Aid = binary_to_list(get_value(ContentJson,<<"ExtInfo">>)),
	SecretKey = "Q4CQayneuVGzBqLjsz0DP17eqX9WCIRl",
	ClientSecLocal = sign([AppID,OrderSerial,CooperatorOrderSerial,Content,SecretKey]),
	if NetResult =:= 1 ->
			if ClientSecLocal =:= Sign ->
					[_ServerID,RoleID] = string:tokens(Aid,"."),
					RoleID2 = list_to_integer(RoleID),
					Amount2 = trunc(list_to_float(Amount)*10),
                    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                    case SrcType of
                        8 ->
        				  Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        				  Req:ok({"text/html; charset=utf-8", Reply}),
        				  QS = mochiweb_util:urlencode(QueryString),
        				  pay_gold2(RoleID2,Amount2,QS,ClientSecLocal,8);
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

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.
