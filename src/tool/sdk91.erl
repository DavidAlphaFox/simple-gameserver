%% @author caohongyang
%% @doc @todo 91平台sdk
%% Created 2013-6-17


-module(sdk91).
-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-define(ACT_CHECK_AUTH, 4).

check_auth(Uin,SessionID) ->
	true.
%% 	AppIDStr = "107061",
%% 	AppKey = "ddee2c0a5fdd0b7b7d363c01dbe153292667de1d2dc68488",
%% 	Sign = sign([AppIDStr, integer_to_list(?ACT_CHECK_AUTH), Uin,SessionID, AppKey]),
%% 	URL = "http://service.sj.91.com/usercenter/ap.aspx?AppId="++AppIDStr
%% 			  ++"&Act=4"++"&Uin="++Uin++"&SessionId="++SessionID++"&Sign="++Sign,
%% 	case httpc:request(get, {URL, []}, [], []) of
%% 		{ok,{_,_,R}} ->
%% 			RS = ejson:decode(R),
%% 			case get_value(RS, <<"ErrorCode">>) of
%% 				<<"1">> ->
%% 					true;
%% 				_ ->
%% 					false
%% 			end;
%% 		Err ->
%% 			false
%% 	end.

check_order_serial(AppID,AppKey,OrderSerial) ->
	Sign = md5(AppID++"1"++OrderSerial++AppKey),
	URL = "http://service.sj.91.com/usercenter/ap.aspx?AppId="++AppID
 			  ++"&Act=1&CooOrderSerial="++OrderSerial++"&Sign="++Sign,
	case httpc:request(get, {URL,[]},[], []) of
		{ok, {_,_,R}} ->
			{RS} = ejson:decode(R),
			%io:format("~100000p",[RS]),
			case (get_value(RS, <<"ErrorCode">>) =:= <<"1">>) of
				true ->
					{true, get_value(RS, <<"GoodsCount">>), get_value(RS,<<"GoodsId">>)};
				false ->
					{false, 2}
			end;
		_ ->
			{false, 4}
	end.
	

%% ====================================================================
%% Internal functions
%% ====================================================================


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

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

test() ->
	check_order_serial("107061","ddee2c0a5fdd0b7b7d363c01dbe153292667de1d2dc68488","33033FA8-F62F-4534-97AD-9C685B164F21").