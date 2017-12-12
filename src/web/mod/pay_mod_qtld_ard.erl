%% @author liuqiang
%% @doc 奇天乐地ARD充值处理

-module(pay_mod_qtld_ard).
-export([pay_gold/1]).
-include("common.hrl").

-define(ACCOUNT_TYPE_QTLD_ARD, 51).             % 奇天乐地ARD
-define(GID, "2025").
-define(APPKEY, "7b5989a2e3f2140388709e30b71361bc").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {_,_,0,_,_,_} ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply});
        {true,_,RoleID,Amount,QueryList,Sign} ->
			QS = mochiweb_util:urlencode(QueryList),
			pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_QTLD_ARD),
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType of
				?ACCOUNT_TYPE_QTLD_ARD ->
					Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8",Reply});
				_ ->
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_qtld_ard(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_sign(Req) ->
    QueryList = Req:parse_qs(),
%%     ?ERR("debug qtld_ard pay, QueryList = ~p~n", [QueryList]),
    PAccid = proplists:get_value("accid", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    Cash = proplists:get_value("cash", QueryList),
    Exchange = proplists:get_value("exchange", QueryList),
    Extra = proplists:get_value("extra", QueryList),
    Money = proplists:get_value("money", QueryList),
    OrderID = proplists:get_value("orderid", QueryList),
    PayTime = proplists:get_value("pay_time", QueryList),
    Sid = proplists:get_value("sid", QueryList),
    Uid = proplists:get_value("uid", QueryList),
    WayName = proplists:get_value("way_name", QueryList),
    Sign = proplists:get_value("sign", QueryList),
    case (not lists:member(?undefined, [Amount,Cash,Exchange,Extra,Money,OrderID,PayTime,Sid,Uid,WayName,Sign])) of
        false ->
            ?ERR("qtld_ard pay, query list error, QueryList = ~p~n", [QueryList]), false;
        true ->
            case sign(Amount,Cash,Exchange,Extra,Money,OrderID,PayTime,Sid,Uid,WayName) =:= Sign of
                false ->
                    ?ERR("qtld_ard pay, check sign error, QueryList = ~p~n", [QueryList]), false;
                true ->
                    [_SServerID,_SRoleID] = string:tokens(Extra, "{},"),
                    ServerID = erlang:list_to_integer(Sid),
                    Accid = (ServerID+1)*?AccidBase + erlang:list_to_integer(PAccid),
                    RoleID = get_roleID(Accid),
                    Amount2 = erlang:round(erlang:list_to_float(Money) * 10),
                    {true,ServerID,RoleID,Amount2,QueryList,Sign}
            end
    end.

sign(Amount,Cash,Exchange,Extra,Money,OrderID,PayTime,Sid,Uid,WayName) ->
    OriStr = "amount="++Amount++"cash="++Cash++"exchange="++Exchange++"extra="++Extra++"money="++Money
            ++"orderid="++OrderID++"pay_time="++PayTime++"sid="++Sid++"uid="++Uid++"way_name="++WayName++?APPKEY,
    md5(OriStr).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_roleID(Accid) ->
    Sql = io_lib:format("select roleID from gRole where accid=~w;", [Accid]),
    case db_sql:get_row(Sql) of
        [] -> 0;
        [H|_] -> H
    end.