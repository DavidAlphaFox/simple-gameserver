%% @author lxl
%% @doc @todo Add description to pay_mod_17173.


-module(pay_mod_17173).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	Money = proplists:get_value("amount",QueryString),
	GameRole = proplists:get_value("ext",QueryString),
	SignString = get_original_encode_list(QueryString) ++ "&appkey=63c756e837d474a728bcb112e78ed1af",
	SignLocal = md5(SignString),
	GameRole = proplists:get_value("ext",QueryString),
	[_ServerID,RoleID] = string:tokens(GameRole,"."),
	RoleID2 = list_to_integer(RoleID),
    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
    case SrcType of
        108 ->
        	Signature = proplists:get_value("sign",QueryString),
        	if SignLocal =:= Signature ->
        			Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        			Req:ok({"text/html; charset=utf-8", Reply}),
        			Amount = trunc(list_to_integer(Money)/10),
        			pay_gold2(RoleID2,Amount,SignString,Signature,108);
        	   true ->
        			Reply = ejson:encode({[{<<"result">>,0}]}),
        			Req:ok({"text/html; charset=utf-8", Reply})
        	end;
       _ ->
            ?ERR("type error ~w",[SrcType]),
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_17173(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================

sign(StrList) ->
	md5(sign2(StrList)).

sign2([A]) ->
	A;
sign2([A,B]) ->
	A++B;
sign2([]) ->
	"";
sign2([A|T]) ->
	A++sign2(T).
		
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_original_encode_list(QS)->
	QS2 = lists:keysort(1, QS),
	[_H|L] = 
	lists:foldl(fun({A,B},Acc) ->
						case A of
							"sign" ->
								Acc;
							"signType" ->
								Acc;
							_ ->
								Acc++"&"++string:to_lower(A)++"="++B
						end
						end, "", QS2),
	L.

