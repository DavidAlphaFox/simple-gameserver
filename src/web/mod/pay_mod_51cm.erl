%% @author wenshuai

-module(pay_mod_51cm).
-include("common.hrl").

-define(srcType, 49).
-define(SECRET, "2132735e02a70bdeceae06f4c248ed9e").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    %% ?ERR("QS=~p\n",[QueryString]),
    Success = proplists:get_value( "success", QueryString ),
    if Success =:= "0" ->
            case check_sign(QueryString, Success) of 
                {true, ExtraInfo, Amount, Sign} ->
                    [RoleID, _ServerID] = string:tokens(ExtraInfo, "."),		
                    RoleID2 = erlang:list_to_integer(RoleID),
                    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                    case SrcType of
                        ?srcType ->
                            Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            Amount2 = list_to_integer(Amount) * 10,
                            QS = mochiweb_util:urlencode(QueryString),
                            pay_gold2(RoleID2,Amount2,QS,Sign,?srcType);
                        _ -> 
                            ?ERR("type error ~w",[SrcType]),
                            Reply = ejson:encode({[{<<"result">>,0}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("51cm pay failed order:~w",[QueryString])
                    end;
                false ->
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("51cm check sign failed.Order:~w.",[QueryString])
            end;
        true -> 
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("51cm pay failed order:~w",[QueryString])
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QueryString, Success) ->
    Sign = proplists:get_value("sign", QueryString),
    OrderId = proplists:get_value("orderid", QueryString),
    Uid = proplists:get_value("uid", QueryString),
    Amount = proplists:get_value("amount", QueryString),
    ExtraInfo = proplists:get_value("extrainfo", QueryString),
    CheckStr = "orderid="++OrderId++"&uid="++Uid++"&amount="++Amount++"&actualamount="++Amount++
               "&extrainfo="++ExtraInfo++"&success="++Success++"&secret="++?SECRET,
    Sign2 = md5(CheckStr),
    if Sign =:= Sign2 ->
            {true, ExtraInfo, Amount, Sign };
        true -> 
            false
    end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_51cm(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
