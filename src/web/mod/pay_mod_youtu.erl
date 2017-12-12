%% @author zcl
%% @doc youtusdk支付处理
%% Created 2014/6/11


-module(pay_mod_youtu).
-include("common.hrl").



-export([pay_gold/1]).

-define(ACCOUnt_TYPE_YOUTU,111).
%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString) of
		{true,RoleID,Gold,OurChannel,Receipt,Sign} ->
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType of
				?ACCOUnt_TYPE_YOUTU ->
					pay_gold2(RoleID, Gold, Receipt, Sign, OurChannel),
					Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply});
				_ ->
					QS = mochiweb_util:urlencode(QueryString),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID, Amount, Receipt, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_quick(RoleID, Amount, Receipt, Sign, SrcType);
pay_gold2(RoleID, _Amount, Receipt, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID, Receipt]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

verify_sign(QueryString)->
    CPOrderID = proplists:get_value("cporderid",QueryString),
    OrderID = proplists:get_value("orderid",QueryString),
    AppID = proplists:get_value("appid",QueryString),
    Uid = proplists:get_value("uid",QueryString),
    Time= proplists:get_value("time",QueryString),
    ExTInfo =  proplists:get_value("extinfo",QueryString),
    Amount = proplists:get_value("amount",QueryString),
    ServerID = proplists:get_value("serverid",QueryString),
    CharID = proplists:get_value("charid",QueryString),
    Gold = proplists:get_value("gold",QueryString),
    Sign = proplists:get_value("sign",QueryString),
    Sign2 = get_message_sign(CPOrderID,OrderID,AppID,Uid,Time,ExTInfo,Amount,ServerID,CharID,Gold),
    case Sign=:=Sign2 of
        true->
            [_TimeStamp,_ServerIDE,RoleID|_T] = string:tokens(ExTInfo,"A"),
            {true,list_to_integer(RoleID),string_to_int(Gold),?ACCOUnt_TYPE_YOUTU,mochiweb_util:urlencode(QueryString),Sign};
        false->
            false
    end.

get_message_sign(CPOrderID,OrderID,AppID,Uid,Time,ExTInfo,Amount,ServerID,CharID,Gold)->
    Key = "91269b1e1b3dfc290e2a5c7771582b38",
    ArgList = [{"cporderid",CPOrderID},{"orderid",OrderID},{"appid",AppID},{"uid",Uid},{"time",Time},{"extinfo",ExTInfo},{"amount",Amount},{"serverid",ServerID},{"charid",CharID},{"gold",Gold}],
    ArgList2 = lists:sort(fun(A,B)->{AH,_AC}=A,{BH,_BC}=B,AH<BH end,ArgList),
    Str = mochiweb_util:urlencode(ArgList2)++Key,
    md5(Str).

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

string_to_int(Str)->
    [Int|_T] = string:tokens(Str,"."),
    list_to_integer(Int).