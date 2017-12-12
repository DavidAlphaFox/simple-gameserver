%% @author zcl
%% @doc caohua支付处理
%% Created 2014/6/11


-module(pay_mod_caohua).
-include("common.hrl").

-define(ACCOUNT_TYPE_CAOHUA,109).    %草花


-export([pay_gold/1]).


%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    % ?INFO("QS:~w ~n",[QueryString]),
    case verify_sign(QueryString) of
        {true,RoleID,Amount,OurChannel,Receipt,Sign} ->
            Amount2 = Amount*10,
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                OurChannel ->
                    pay_gold2(RoleID, Amount2, Receipt, Sign, OurChannel),
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply});
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]}),
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
    ServerKey = "C6A8F2F190D37D4BBF6EE60618311BB1",
    OrderNo = proplists:get_value("OrderNo", QueryString),
    OutPayNo = proplists:get_value("OutPayNo", QueryString),
    UserID = proplists:get_value("UserID", QueryString),
    ServerNo = proplists:get_value("ServerNo", QueryString),
    PayType = proplists:get_value("PayType", QueryString),
    Money = proplists:get_value("Money", QueryString),
    PMoney = proplists:get_value("PMoney", QueryString),
    PayTime = proplists:get_value("PayTime", QueryString),
    Sign = proplists:get_value("Sign", QueryString),
    Sign3 = md5(OrderNo++OutPayNo++UserID++ServerNo++PayType++Money++PMoney++PayTime++ServerKey),
    Sign2 = string:to_upper(Sign3),
    Receipt = mochiweb_util:urlencode(QueryString),
    case Sign =:= Sign2 of
        true->
            [_ServerID,RoleID,_] = string:tokens(OutPayNo,"A"),
            [Money2|_T] = string:tokens(Money,"."),
            {true,list_to_integer(RoleID),list_to_integer(Money2),?ACCOUNT_TYPE_CAOHUA,Receipt,Sign};
        false-> 
            false
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

