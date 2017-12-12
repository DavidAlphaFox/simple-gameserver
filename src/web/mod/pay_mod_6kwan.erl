%% @author zcl
%% @doc 6kwansdk支付处理
%% Created 2014/6/11


-module(pay_mod_6kwan).
-include("common.hrl").



-export([pay_gold/1]).

-define(ACCOUnt_TYPE_6KWAN,113).
-define(APPID,"1000044").
%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString) of
        {true,RoleID,Gold,OurChannel,Receipt,Sign} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                OurChannel ->
                    pay_gold2(RoleID, Gold, Receipt, Sign, OurChannel),
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
    AppID = proplists:get_value("appid",QueryString),
    Data = proplists:get_value("data",QueryString),
    Sign = proplists:get_value("sign",QueryString),
    Sign2 = get_message_sign(Data),
    case Sign=:=Sign2 andalso AppID=:= ?APPID of
        true->
            analyse_data(Data,QueryString,Sign);
        false->
            false
    end.

get_message_sign(Data)->
    Key = "44d0d6f9c6f0b5c9de9669edf234ca20",
    {Content} = ejson:decode(Data),
    Content2 = [{binary_to_list(TempKey),binary_to_list(TempValue)}||{TempKey,TempValue}<-Content],
    Content3 = lists:sort(fun(A,B)->{HA,_TA}=A,{HB,_TB}=B,HA<HB end,Content2),
    ContentUrl = mochiweb_util:urlencode(Content3),
    SignMessage = ?APPID++ContentUrl++Key,
    md5(SignMessage).

analyse_data(Data,QueryString,Sign)->
    {Content} = ejson:decode(Data),
    CallBackInfo = binary_to_list(proplists:get_value(<<"callbackInfo">>,Content)),
    Money = binary_to_list(proplists:get_value(<<"money">>,Content)),
    [_ServerStr,RoleIDStr|_T] = string:tokens(CallBackInfo,"A"),
    {true,list_to_integer(RoleIDStr),trunc(list_to_float(Money)*10),?ACCOUnt_TYPE_6KWAN,mochiweb_util:urlencode(QueryString),Sign}.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
