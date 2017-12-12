%% @author dongquan
%% @doc @todo Add description to pay_mod_gg


-module(pay_mod_gg).
-include("common.hrl").

-define(ACCOUNT_TYPE_GG, 55).  

-define(GAME_ID,"15704").
-define(APP_KEY,"0c7da86a2f095eda21b7d76e6b4f8aa4").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
    QueryString = Req:parse_post(),
    case check_sign(QueryString) of
        {true,RoleID,AmountT,Sign} ->
            Amount = erlang:trunc(AmountT / 10),
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_GG ->
                    Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    QS = mochiweb_util:urlencode(QueryString),
                    pay_gold2(RoleID,Amount,QS,Sign,?ACCOUNT_TYPE_GG);
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("xy check_order failed. reason:sign wrong,order:~w",[QueryString])
            end;    
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("xy check_order failed. reason:sign wrong,order:~w",[QueryString])
    end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_gg(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].



%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QueryList) ->
    OpenIdT = proplists:get_value("openId", QueryList),
    ServerIdT = proplists:get_value("serverId", QueryList),
    ServerNameT = proplists:get_value("serverName", QueryList),
    RoleIdT = proplists:get_value("roleId", QueryList),
    RoleNameT = proplists:get_value("roleName", QueryList),
    OrderIdT = proplists:get_value("orderId", QueryList),
    OrderStatusT = proplists:get_value("orderStatus", QueryList),
    PayTypeT = proplists:get_value("payType", QueryList),
    AmountT = proplists:get_value("amount", QueryList), %%单位为分，float
    RemarkT = proplists:get_value("remark", QueryList),
    CallBackInfoT = proplists:get_value("callBackInfo", QueryList),
    PayTimeT = proplists:get_value("payTime", QueryList),
    PaySUTimeT = proplists:get_value("paySUTime", QueryList),
    SignT = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [OpenIdT,ServerIdT,RoleIdT,OrderIdT,OrderStatusT,AmountT,SignT])) of
        false ->
            ?ERR("gg pay, query list error, QueryList = ~p~n", [QueryList]),
            Reply = "error",
            {false, Reply};
        true ->
            ArgList =
                [OpenId, ServerId, ServerName, RoleId, RoleName, OrderId, OrderStatus,
                 PayType, Amount, Remark, CallBackInfo, PayTime, PaySUTime] =
                    lists:map(fun(String) ->
                                      case String of
                                          ?undefined ->
                                              String;
                                          _ ->
                                              http_uri:decode(String)
                                      end
                              end, [OpenIdT, ServerIdT, ServerNameT, RoleIdT, RoleNameT, OrderIdT, OrderStatusT,
                                    PayTypeT, AmountT, RemarkT, CallBackInfoT, PayTimeT, PaySUTimeT]),
            ArgNameList = ["openId", "serverId", "serverName", "roleId", "roleName", "orderId", "orderStatus",
                           "payType", "amount", "remark", "callBackInfo", "payTime", "paySUTime"],
            case OrderStatus of
                "1" ->
                    Sign = http_uri:decode(SignT),
                    case md5(gen_sign(ArgNameList, ArgList, [])) =:= Sign of
                        false ->
                            ?ERR("gg pay, check sign error, QueryList = ~p~n", [QueryList]),
                            Reply = "errorSign",
                            {false, Reply};
                        true ->
                            {true,erlang:list_to_integer(RoleId),
                             erlang:list_to_integer(Amount), Sign}
                    end;
                _ ->
                    ?ERR("gg pay, order status not succ, QueryList = ~p~n", [QueryList]),
                    Reply = "success",
                    {false, Reply}
            end
    end.

gen_sign([], [], List) ->
    [_|List2] = List ++ "&app_key=" ++ ?APP_KEY,
    List2;
gen_sign([ArgName|ArgNameList], [Arg|ArgList], List) ->
    case Arg of
        ?undefined ->
            NewList = List ++ "&" ++ ArgName ++ "=",
            gen_sign(ArgNameList, ArgList, NewList) ;
        _ ->
            NewList = List ++ "&" ++ ArgName ++ "=" ++ Arg,
            gen_sign(ArgNameList, ArgList, NewList)   
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.