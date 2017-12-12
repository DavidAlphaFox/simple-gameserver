%% @author ljf
%% @doc leyousdk支付处理
%% Created 2014/6/11

-module(pay_mod_leyou).
-include("common.hrl").

-export([pay_gold/1]).

-define(ACCOUnt_TYPE_LEYOU,112).
%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    ?INFO("Req:~w,QS:~w ~n",[Req,QueryString]),
    case verify_sign(QueryString) of
        {true,RoleID,Amount,OurChannel,Receipt,Sign} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
               ?ACCOUnt_TYPE_LEYOU ->
                    Amount2 = Amount*10,
                    pay_gold2(RoleID, Amount2, Receipt, Sign, OurChannel),
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply});
                _->
                    Reply = ejson:encode({[{<<"result">>,5}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("leyou pay for other SrcType:~w ~n",[SrcType])
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
    RoleId = get_value(QueryString,"roleid"),
    ServerId= get_value(QueryString,"serverid"),
    Amount = get_value(QueryString,"amount"),
    Sign = string:substr(get_value(QueryString,"sign"), 1, 32),
    Sign2 = md5(mochiweb_util:urlencode(lists:keydelete("sign", 1,QueryString))++"&appkey=62f128b911f9db1387fa3e25accd44b4"),
    ?INFO("pay_mod_leyou verify_sign ~w~n~s<<<>>>~s",[QueryString,Sign,Sign2]),
    case Sign=:=Sign2 of
        true->
%%          [ServerID,RoleID|T] = string:tokens(ExTInfo,"A"),
            {true,list_to_integer(RoleId),list_to_integer(Amount),?ACCOUnt_TYPE_LEYOU,mochiweb_util:urlencode(QueryString),Sign};
        false->
            false
    end.

%% get_message_sign(CPOrderID,OrderID,AppID,Uid,Time,ExTInfo,Amount,ServerID,CharID,Gold)->
%%  Key = "",
%%  ArgList = [{"cporderid",CPOrderID},{"orderid",OrderID},{"appid",AppID},{"uid",Uid},{"time",Time},{"extinfo",ExTInfo},{"amount",Amount},{"serverid",ServerID},{"charid",CharID},{"gold",Gold}],
%%  md5(mochiweb_util:urlencode(ArgList)++Key).

get_value(Response, Key) when is_list(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).