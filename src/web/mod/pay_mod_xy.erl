%% @author dongquan
%% @doc @todo Add description to pay_mod_xy


-module(pay_mod_xy).
-include("common.hrl").

-define(ACCOUNT_TYPE_XY, 56).

-define(APPID, "100001069").
-define(APPKEY, "2cWx11uf6YjDOPtQEppePgrsz6Mg6PrB").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
    QueryString = Req:parse_post(),
    case check_sign(QueryString) of
        {true,_ServerID,RoleID,AmountT,Sign} ->
            Amount = erlang:trunc(AmountT * 10),
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType == ?ACCOUNT_TYPE_XY orelse RRType == 47 of
				true ->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					QS = mochiweb_util:urlencode(QueryString),
					pay_gold2(RoleID,Amount,QS,Sign,?ACCOUNT_TYPE_XY);
				_ ->
					QSL = mochiweb_util:urlencode(QueryString),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QSL]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("xy check_order failed. reason:sign wrong,order:~w",[QueryString])
    end.




pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_xy(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].



%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(QueryList) ->
    ?ERR("debug xy pay, QueryList = ~p~n", [QueryList]),
    OrderID = proplists:get_value("orderid", QueryList),
    UID = proplists:get_value("uid", QueryList),
    ServerID = proplists:get_value("serverid", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    Extra = proplists:get_value("extra", QueryList),
    Timestamp = proplists:get_value("ts", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [OrderID,UID,ServerID,Amount,Extra,Timestamp,Sign])) of
        false ->
            ?ERR("xy pay, query list error, QueryList = ~p~n", [QueryList]),
            false;
        true ->
            case sign(Amount, Extra, OrderID, ServerID, Timestamp, UID) =:= Sign of
                false ->
                    ?ERR("xy pay, check sign error, QueryList = ~p~n", [QueryList]),
                    false;
                true ->
                    %% 3.7.0以前的版本只有RoleID
                    %% 3.7.0版本将extra修改成AserveridAuidAtimestap
                    case string:tokens(Extra,"A") of
                        [_ServerID,RoleID1,_Timestamp]->
                            RoleID=RoleID1;
                        [RoleID1]->
                            RoleID=RoleID1
                    end,
                    {true,erlang:list_to_integer(ServerID),erlang:list_to_integer(RoleID),
                     erlang:list_to_float(Amount),Sign}
            end
    end.

sign(Amount, Extra, OrderID, ServerID, Timestamp, UID) ->
    md5(?APPKEY ++ "amount=" ++ Amount ++ "&extra=" ++ Extra ++ "&orderid=" ++ OrderID ++ "&serverid=" ++ ServerID
       ++ "&ts=" ++ Timestamp ++ "&uid=" ++ UID).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.