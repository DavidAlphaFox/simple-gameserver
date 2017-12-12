%% @author wenshuai

-module(pay_mod_mmy).
-include("common.hrl").

-define(srcType,54).
-define( APPKEY, "890aa7f45a5bcf32A4CNO88UtR6qYNrifKR3fkwsZ08FS3ts4oRXmPpDGCMrXKGF" ).

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    %%?ERR("QS=~p.~n",[QueryString]),
    case proplists:get_value( "tradeState", QueryString ) of
        "success" ->
            case check_verify( QueryString ) of
                {true, Amount,Sign} ->
                    ExtraInfo = proplists:get_value("productDesc", QueryString), 
                    [RoleID, _ServerID] = string:tokens(ExtraInfo, "."),		
                    RoleID2 = erlang:list_to_integer(RoleID),
                    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                    case SrcType of
                        ?srcType->
                            Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            QS = mochiweb_util:urlencode(QueryString),
                            pay_gold2(RoleID2,Amount,QS,Sign,?srcType);
                        _->
                            Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("mmy pay for other srcType:~w ~n",[SrcType])
                    end;
                false ->
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("mmy check sign failed.Order:~w.",[QueryString])
            end;
        _ -> 
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
            ?ERR("mmy pay failed order:~w",[QueryString])
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_verify( QueryString ) ->
    Sign = proplists:get_value("tradeSign", QueryString),
    OrderIdStr = proplists:get_value("orderID", QueryString),
    %%?ERR("Sign:~p.~nOrderIdStr:~p.~n", [Sign, OrderIdStr] ),
    if 
        length( Sign ) < 14 ->
            false;
        true ->
            VerifyStr = string:sub_string( Sign, 1, 8 ),
            SignStr = string:sub_string( Sign, 9 ),
            TempStr = md5( SignStr ),
            %%?ERR( "VerifyStr:~p.~nTempStr:~p.~n", [VerifyStr,TempStr] ),
            case VerifyStr =:= string:sub_string( TempStr, 1, 8 ) of
                true ->
                    KeyStr = string:sub_string( SignStr, 1, 6 ),
                    RandKey = md5( KeyStr ++ ?APPKEY ),
					Ba64Str = string:sub_string( SignStr, 7 ),
                    Base64KeyStr = 
						case catch base64:decode_to_string(Ba64Str) of
							{'EXIT',_} ->
								base64:decode_to_string(Ba64Str++"==");
							Value ->
								Value
						end,
                    %%?ERR("RandKey:~p.~nBase64KeyStr:~p.~n", [RandKey,Base64KeyStr] ),
                    case OrderIdStr =:= calcOrderId( Base64KeyStr, RandKey, 0, "" ) of 
                        true ->
                            Amount = proplists:get_value("productPrice", QueryString),
                            Amount2 = erlang:trunc( erlang:list_to_integer(Amount) * 10 ),
                            {true, Amount2, Sign};
                        _ -> false
                    end;
                _ -> false
            end
    end.

calcOrderId( [H|T], RandKey, I, OrderIdStr ) ->
    Pos = I rem 32 + 1,
    KeyRan = lists:nth( Pos, RandKey ),
    calcOrderId( T, RandKey, 1 + I, [(H bxor KeyRan) band 16#000000FF | OrderIdStr] );

calcOrderId( [], _, _, OrderIdStr ) -> 
	lists:reverse(OrderIdStr).

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_mmy(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
    ?ERR("mmy pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
    [].

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
