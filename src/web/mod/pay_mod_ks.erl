%% @author liuqiang
%% @doc 金山sdk充值处理


-module(pay_mod_ks).
-include("common.hrl").

-export([pay_gold/1]).

-define(ACCOUNT_TYPE_KS, 37).
-define(GID, "104").
-define(PFID, "104").
-define(PFKEY, "IhOwSg1K9vMU8VpgHfr6sTPXQs5ywaw6").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,RoleID,Money,Sign,QueryString} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_KS->
                    Amount = Money div 10,
                    QS = mochiweb_util:urlencode(QueryString),
                    Result = pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_KS),
                    Reply = ejson:encode({[{<<"result">>,Result}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8",Reply});
                _->
                    Reply = ejson:encode({[{<<"result">>,5}, {<<"accid">>,AccID}]}), 
                    Req:ok({"text/html; charset=utf-8",Reply}),
                    ?ERR("ks pay for other SrcType:~w ~n",[SrcType])
            end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_ks(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]), 0.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryString = Req:parse_qs(),
%%     ?ERR("debug ks pay, QueryString = ~p~n", [QueryString]),
    Pfid = proplists:get_value("pfid", QueryString),
    Gid = proplists:get_value("gid", QueryString),
    Sid = proplists:get_value("sid", QueryString),
    Uid = proplists:get_value("uid", QueryString),
    Oid = proplists:get_value("oid", QueryString),
    Payway = proplists:get_value("payway", QueryString),
    Gold = proplists:get_value("gold", QueryString),
    Money = proplists:get_value("money", QueryString),
    Paytime = proplists:get_value("paytime", QueryString),
    Cpparam = proplists:get_value("cpparam", QueryString),
    Time = proplists:get_value("time", QueryString),
    Sign = proplists:get_value("sign", QueryString),
    case (not lists:member(?undefined, [Pfid,Gid,Sid,Uid,Oid,Payway,Gold,Money,Paytime,Cpparam,Time,Sign])) of
        false ->
            ?ERR("ks pay, query string error, QueryString = ~p~n", [QueryString]), false;
        true ->
            {_,MySign} = sign(Pfid, Gid, Sid, Uid, Oid, Payway, Gold, Money, Paytime, Cpparam, Time),
            case MySign =:= Sign of
                false ->
                    ?ERR("ks pay, check sign error, MySign = ~p~n", [MySign]), false;
                true ->
                    [_ServerID, RoleID] = string:tokens(Cpparam, "{, }"),
                    {true,erlang:list_to_integer(RoleID),erlang:list_to_integer(Money),Sign,QueryString}
            end
    end.

sign(Pfid, Gid, Sid, Uid, Oid, Payway, Gold, Money, Paytime, Cpparam, Time) ->
    OriStr = mochiweb_util:urlencode([{cpparam,Cpparam},{gid,Gid},{gold,Gold},{money,Money},{oid,Oid}
                                     ,{paytime,Paytime},{payway,Payway},{pfid,Pfid},{sid,Sid},{time,Time},{uid,Uid}]),
    {OriStr,md5(OriStr ++ ?PFKEY)}.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- erlang:binary_to_list(erlang:md5(S))]).
