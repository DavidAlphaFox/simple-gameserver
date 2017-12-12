%% @author zcl
%% @doc baofengsdk支付处理
%% Created 2014/6/11


-module(pay_mod_baofeng).
-include("common.hrl").

-define(ACCOUNT_TYPE_BAOFENG,104).    %乐盟互动
%%游戏定义的渠道结束


-export([pay_gold/1]).


%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString) of
        {true,RoleID,Amount,Receipt,Sign} ->
            Amount2 = erlang:trunc(Amount*10),
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_BAOFENG ->
                    pay_gold2(RoleID, Amount2, Receipt, Sign, ?ACCOUNT_TYPE_BAOFENG),
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
    pay_server:do_pay_from_quick(RoleID,Amount,Receipt,Sign,SrcType);
pay_gold2(RoleID, _Amount, Receipt, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID, Receipt]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

verify_sign(QueryString)->
    Key = "Aib@8maWBRwnC26arsCSNBFJEm7RGfpd",
    Amount = proplists:get_value("amount",QueryString),
    BillNo = proplists:get_value("bill_no", QueryString),
    Extra = proplists:get_value("extra",QueryString),
    GameID = proplists:get_value("game_id",QueryString),
    ServerID = proplists:get_value("server_id",QueryString),
    UserID = proplists:get_value("user_id",QueryString),
    Sign = proplists:get_value("sign",QueryString),
    Msg = Amount++BillNo++Extra++GameID++ServerID++UserID++Key,
    Sign2 = string:to_upper(md5(Msg)),
    case Sign2 =:= Sign of
        true->
            [_ServerID2,RoleID,_TimeStamp] = string:tokens(Extra,"A"),
            L = string:tokens(Amount,"."),
            QS = mochiweb_util:urlencode(QueryString),
            case length(L)=:=1 of
                true->

                    {true,list_to_integer(RoleID),list_to_integer(Amount),QS,Sign};
                false->
                    {true,list_to_integer(RoleID),list_to_float(Amount),QS,Sign}
            end;
        false->
            ?ERR("verify_sign failed Sign:~w Sign2:~w ~n",[Sign,Sign2]),
            false
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
