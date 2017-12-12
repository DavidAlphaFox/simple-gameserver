%% @author zxystar
%% @doc @todo Add description to pay_mod_dl.


-module(pay_mod_dl).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%?ERR("Req:~w\nQS=~w",[Req,QueryString]),
	%Uin = proplists:get_value("uin", QueryString),
	%SessionID = proplists:get_value("sessionID", QueryString),
	Result = proplists:get_value("result",QueryString),
	Money = proplists:get_value("money",QueryString),
	Order = proplists:get_value("order",QueryString),
	Mid = proplists:get_value("mid",QueryString),
	Time = proplists:get_value("time",QueryString),
	Signature = proplists:get_value("signature",QueryString),
	GameRole = proplists:get_value("ext",QueryString),
	[_ServerID,RoleID] = string:tokens(GameRole,"."),
	RoleID2 = list_to_integer(RoleID),
    {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
    case SrcType of
        4 ->
        	Key="T9QmHU2nuryX",
%        	SignString = "order="++Order++"&money="++Money++"&mid="++Mid++"&time="++Time++"&result="++Result++"&ext="++GameRole++"&key="++Key,
			CpOrder = proplists:get_value("cpOrder",QueryString), 
			SignString = 
				case CpOrder of
					?undefined ->
						"order="++Order++"&money="++Money++"&mid="++Mid++"&time="++Time++"&result="++Result++"&ext="++GameRole++"&key="++Key;
					_ ->
						"order="++Order++"&money="++Money++"&mid="++Mid++"&time="++Time++"&result="++Result++"&cpOrder="++CpOrder++"&ext="++GameRole++"&key="++Key
				end,
        	SignLocal = md5(SignString),
        	if SignLocal =:= Signature ->
        			Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        			Req:ok({"text/html; charset=utf-8", Reply}),
        			if Result =:= "1" ->
        							Amount2 = list_to_float(Money),
        							pay_gold2(RoleID2,trunc(Amount2*10),SignString,Signature,4);
        					   true->
        							 ?ERR("OrderStatus err:",[QueryString])  
        					end;
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
	pay_server:do_pay_from_dl(RoleID,Amount,Req,Sign,SrcType);
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



