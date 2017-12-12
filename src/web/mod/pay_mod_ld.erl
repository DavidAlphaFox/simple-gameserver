%% @author dongquan
%% @doc 乐逗 sdk充值处理

-module(pay_mod_ld).
-export([pay_gold/1]).
-include("common.hrl").

-define(ACCOUNT_TYPE_LD, 53).

-define(SECRET,"9c40d6e2250a0276b1ef").

%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    case check_sign(Req) of
        {true,RoleID,AmountT,QueryList,Sign} ->
        	{AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
            	?ACCOUNT_TYPE_LD->
            		Amount = erlang:trunc(AmountT * 10),
            		QS = mochiweb_util:urlencode(QueryList),
            		pay_gold2(RoleID, Amount, QS, Sign, ?ACCOUNT_TYPE_LD),
            		Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
            		Req:ok({"text/html; charset=utf-8",Reply});
            	_->
            		Reply = ejson:encode({[{<<"result">>,5}, {<<"accid">>,AccID}]}),
            		Req:ok({"text/html; charset=utf-8",Reply}),
            		?ERR("ld pay for other SrcType:~w ~n",[SrcType])
            end;
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8",Reply})
    end.

pay_gold2(RoleID, Amount, Req, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_ld(RoleID, Amount, Req, Sign, SrcType);
pay_gold2(RoleID, _Amount, Req, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]).

%% ====================================================================
%% Internal functions
%% ====================================================================
check_sign(Req) ->
    QueryList = Req:parse_qs(),
    ?ERR("debug ld pay, QueryList = ~p~n", [QueryList]),
    OrderId = proplists:get_value("orderId", QueryList),
    OpenId = proplists:get_value("openid", QueryList),
    Amount = proplists:get_value("amount", QueryList),
    ActualAmount = proplists:get_value("actualAmount", QueryList),
    ExtraInfo = proplists:get_value("extraInfo", QueryList),
    Success = proplists:get_value("success", QueryList),
    _Msg = proplists:get_value("msg", QueryList),
    _Created = proplists:get_value("created", QueryList),
    _P_ID = proplists:get_value("p_id", QueryList),
    _P_Identifier = proplists:get_value("p_identifier", QueryList),    
    Sign = proplists:get_value("sign", QueryList),
	case (not lists:member(?undefined, [OrderId,OpenId,Amount,ActualAmount,Success,ExtraInfo,Sign])) of
		false ->
			db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_LD, QueryList),
			?ERR("ld pay, query list error, QueryList = ~p~n", [QueryList]),
			Reply = "fail",
			Req:ok({"text/html; charset=utf-8",Reply}), false;
		true ->
			case Success of
				"0" ->
					ExtraInfo2 = http_uri:decode(ExtraInfo),
					case md5("orderId=" ++ OrderId ++ "&openid=" ++ OpenId ++ "&amount=" ++ Amount ++
								 "&actualAmount=" ++ ActualAmount ++ "&success=" ++ Success ++
								 "&extraInfo=" ++ ExtraInfo2 ++ "&secret=" ++ ?SECRET) =:= Sign of
						false ->
							db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_LD, QueryList),
							?ERR("ld pay, check sign error, QueryList = ~p~n", [QueryList]),
							Reply = "fail",
							Req:ok({"text/html; charset=utf-8",Reply}), false;
						true ->
							ExtraInfo3 = mochiweb_util:parse_qs(ExtraInfo2),
							RoleID = proplists:get_value("roleid", ExtraInfo3),
							{true,erlang:list_to_integer(RoleID),erlang:list_to_integer(Amount),
							 QueryList,Sign}
					end;
				"1" ->
					db_func:log_pay_info(2, 0, 0, 0, erlang:localtime(), 0, ?ACCOUNT_TYPE_LD, QueryList),
					?ERR("ld pay, status error, QueryList = ~p~n", [QueryList]),
					Reply = "fail",
					Req:ok({"text/html; charset=utf-8",Reply}), false
			end
	end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

