-module(pay_mod_alipay).

%% ====================================================================
%% API functions
%% ====================================================================
-include("common.hrl").
-define(SRCTYPE, 69).

-define(PUBLICKEY,<<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCnxj/9qwVfgoUh/y2W89L6BkRAFljhNhgPdyPuBV64bfQNN1PjbCzkIM6qRdKBoLPXmKKMiFYnkd6rAoprih3/PrQEB/VsW8OoM8fxn67UDYuyBTqA23MML9q1+ilIZwBC2AQ2UBVOrFXfFl75p6/B5KsiNG9zpgmLCUYuLkxpLQIDAQAB\n-----END PUBLIC KEY-----">>).
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_post(),
	case check_sign(QueryString) of
		true ->
            Amount2 = proplists:get_value("total_fee", QueryString),
			Amount = trunc(erlang:list_to_float(Amount2) * 10),
            Body = proplists:get_value("body", QueryString),
			[_ServerID,RoleID|_] = string:tokens(Body, "A"),
            Sign = proplists:get_value("sign",QueryString),
            {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(list_to_integer(RoleID)),
            case SrcType of
                ?SRCTYPE ->
        			Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        			Req:ok({"text/html; charset=utf-8", Reply}),
                    QS = mochiweb_util:urlencode(QueryString),
        			pay_gold2(list_to_integer(RoleID),Amount,QS,Sign,?SRCTYPE);
                _ ->
                    ?ERR("type error ~w",[SrcType]),
                    Reply = ejson:encode({[{<<"result">>,0}]}),
                    Req:ok({"text/html; charset=utf-8", Reply})
            end;    
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_alipay(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================
get_rsapubkey( ) ->
    [RSAEntry] = public_key:pem_decode(?PUBLICKEY),
    public_key:pem_entry_decode(RSAEntry).

check_sign(QueryString) ->
    Sign = proplists:get_value("sign", QueryString),
    Sign2 = base64:decode(Sign),
    SortStr = lists:keysort(1, QueryString),
    [_|ToCheckSort] = lists:foldl(fun({Key,Value},Acc) ->
                                    if 
                                        Key == "sign" orelse Key == "sign_type" ->
                                            Acc;
                                        true ->
                                            Acc ++ "&" ++ Key ++ "=" ++ Value 
                                    end
                                 end, "", SortStr),
    public_key:verify(list_to_binary(ToCheckSort), 'sha', Sign2, get_rsapubkey( )).
