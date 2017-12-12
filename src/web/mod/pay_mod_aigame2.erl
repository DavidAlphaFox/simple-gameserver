-module(pay_mod_aigame2).

%% ====================================================================
%% API functions
%% ====================================================================
-include("common.hrl").
-define(SRCTYPE, 68).
-define(APPKEY,"20cdacd087d089ac05bf40e1c5e49774").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_post(),
	%%?ERR("AIGame:~w",[QueryString]),
    CpOrderId = proplists:get_value("cp_order_id", QueryString),
    Correlator = proplists:get_value("correlator", QueryString),
    OrderTime = proplists:get_value("order_time", QueryString),
    Method = proplists:get_value("method", QueryString),
    Md5Sign = proplists:get_value("sign", QueryString),
    CheckResult = case Method of
				        "check" ->
				            Fee = proplists:get_value("fee", QueryString),
				            check_sign(CpOrderId, Correlator, OrderTime, Method,Md5Sign);
				        "callback" ->
				            ResultCode = proplists:get_value("result_code", QueryString),
				            PayType = proplists:get_value("pay_type", QueryString),
				            Fee = proplists:get_value("fee", QueryString),
				            callback_sign(CpOrderId, Correlator, ResultCode, Fee, PayType, Method, Md5Sign)
                    end,
    [_TimeStamp,RoleID|_] = string:tokens(CpOrderId, "*"),
    Amount = list_to_integer(Fee) * 10,
    case CheckResult of
        {false, _} ->
			Reply = ejson:encode({[{<<"result">>,0}]});
        true ->
            {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(list_to_integer(RoleID)),
            case SrcType of
                ?SRCTYPE ->
        			Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
                    QS = mochiweb_util:urlencode(QueryString),
        			pay_gold2(list_to_integer(RoleID),Amount,QS,Md5Sign,?SRCTYPE);
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]})
            end     
	end,
	Req:ok({"text/html; charset=utf-8", Reply}).

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_aigame(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

check_sign(CpOrderId, Correlator, OrderTime, Method, Md5Sign) ->
    Sign = md5(CpOrderId ++ Correlator ++ OrderTime ++ Method ++ ?APPKEY),
    if
        Sign =:= Md5Sign ->
            true;
        true ->
            ?ERR("TF1-Cp订单:~p, 订单:~p,校验错误~n", [CpOrderId, Correlator]),
            {false, 1}
    end.

callback_sign(CpOrderId, Correlator, ResultCode, Fee, PayType, Method, Md5Sign) ->
    case ResultCode of
        "00" ->
            Sign = md5(CpOrderId ++ Correlator ++ ResultCode ++ Fee ++ PayType ++ Method ++ ?APPKEY),
            if 
                Sign =:= Md5Sign ->
                    true;
                true ->
                    ?ERR("TF2-Cp订单:~p, 订单:~p,校验错误~n", [CpOrderId, Correlator]),
                    {false, 1}
            end;
        _ ->
            ?ERR("Cp订单:~p, 订单:~p,返回码错误:~p.~n", [CpOrderId, Correlator, ResultCode]),
            {false , 2}
    end.
