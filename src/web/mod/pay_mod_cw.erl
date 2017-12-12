%% @author dongquan
%% @doc 益玩 sdk充值处理

-module(pay_mod_cw).
-export([pay_gold/1]).
-include("common.hrl").

-define(ACCOUNT_TYPE_CW, 48).

-define(APP_ID,"10117").
-define(PACKET_ID,"25331").
-define(APP_KEY,"wAU0pCdyF2IcMYvs").
-define(SIGN_KEY,"rXTt9bJWlzN3inR6").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,_ServerID,RoleID,AmountT,QueryList,Sign} ->
            Amount = erlang:trunc(AmountT / 10),
            QS = mochiweb_util:urlencode(QueryList),
            {AccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType of
				?ACCOUNT_TYPE_CW ->
            pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_CW),
					Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
					Req:ok({"text/html; charset=utf-8",Reply});
				_ ->
					QS = mochiweb_util:urlencode(Req:parse_qs()),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,AccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_cw(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
    ServerID = proplists:get_value("serverid", QueryList),
    CustomInfo = proplists:get_value("custominfo", QueryList),
    UserID = proplists:get_value("openid", QueryList),
    OrderID = proplists:get_value("ordernum", QueryList),
    Status = proplists:get_value("status", QueryList),
    PayType = proplists:get_value("paytype", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    ErrDesc = proplists:get_value("errdesc", QueryList),
    PayTime = proplists:get_value("paytime", QueryList),    
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,PayTime,Sign])) of
        false ->
            ?ERR("cw pay, query list error, QueryList = ~p~n", [QueryList]),
            false;
        true ->
            case sign(ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,ErrDesc,PayTime) =:= Sign of
                false ->
                    ?ERR("cw pay, check sign error, QueryList = ~p~n", [QueryList]),
                    false;
                true ->
                    {CustomInfo2} = ejson:decode(CustomInfo),
                    RoleID = get_value(CustomInfo2, <<"roleid">>),
                    {true,erlang:list_to_integer(ServerID),RoleID,erlang:list_to_integer(Amount),
                     QueryList, Sign}
            end
    end.

sign(ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,?undefined,PayTime) ->
    md5(ServerID ++ "|" ++ CustomInfo ++ "|" ++ UserID ++ "|" ++ OrderID ++ "|" ++ Status ++ "|" ++ PayType ++ "|" ++ Amount ++ "|" ++ PayTime ++ "|" ++ ?APP_KEY);

sign(ServerID,CustomInfo,UserID,OrderID,Status,PayType,Amount,ErrDesc,PayTime) ->
    md5(ServerID ++ "|" ++ CustomInfo ++ "|" ++ UserID ++ "|" ++ OrderID ++ "|" ++ Status ++ "|" ++ PayType ++ "|" ++ Amount ++ "|" ++ ErrDesc ++ "|" ++ PayTime ++ "|" ++ ?APP_KEY).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

