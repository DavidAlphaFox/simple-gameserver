-module(pay_mod_kaopu).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).
-define(SKEY, "C916827D-BA1A-48B3-BDBB-336EB780CF2A").
-define(SrcType, 101).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
    case check_auth(QueryString) of
        {true, RoleID, Amount, Sign} ->
		    RoleID2 = list_to_integer(RoleID),
            Amount2 = list_to_integer(Amount),
			{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
            case SrcType of
                ?SrcType->
			        Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
			        Req:ok({"text/html; charset=utf-8", Reply}),				  
			        Amount3 = erlang:trunc(Amount2/10),
			        QS = mochiweb_util:urlencode(QueryString),
			        pay_gold2(RoleID2,Amount3,QS,Sign,?SrcType);
                _->
                    Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("kaopu pay for other SrcType:~w~n",[SrcType])
            end;   
		true ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("kaopu pay failed order:~w",[QueryString])
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_kaopu(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("kaopu pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

check_auth(QueryString) ->
    Status = proplists:get_value("status", QueryString),
    case Status =:= "1" of
        true ->
            Username = proplists:get_value("username", QueryString),
            Kpordernum = proplists:get_value("kpordernum", QueryString),
            Ywordernum = proplists:get_value("ywordernum", QueryString),
            PayType = proplists:get_value("paytype", QueryString),
            Amount = proplists:get_value("amount", QueryString),
            GSvr = proplists:get_value("gameserver", QueryString),
            Errdesc = proplists:get_value("errdesc", QueryString),
            PayTime = proplists:get_value("paytime", QueryString),
            GName = proplists:get_value("gamename", QueryString),
            ToHash = Username ++ "|" ++ Kpordernum ++ "|" ++ Ywordernum  ++ "|" ++ 
                       Status ++ "|" ++ PayType ++ "|" ++ Amount ++ "|" ++ GSvr ++ "|"
                        ++ Errdesc ++ "|" ++ PayTime ++ "|" ++ GName ++ "|" ++ ?SKEY,
            Sign = proplists:get_value("sign", QueryString),
            Sign2 = md5(ToHash),
            case Sign2 =:= Sign of 
                true ->
                    [_, RoleID|_] = string:tokens(Ywordernum, "."),
                    {true, RoleID, Amount, Sign};
                _ ->
                    false 
            end;
        _ ->
            false
    end.
