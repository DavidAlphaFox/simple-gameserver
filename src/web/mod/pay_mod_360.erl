%% @author zxystar
%% @doc @todo Add description to pay_mod_360.


-module(pay_mod_360).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	%% ?ERR("QS=~p\n",[QueryString]),
	SessionID = proplists:get_value("gateway_flag", QueryString),
	Ucid = proplists:get_value("user_id",QueryString),
	if SessionID =:= "success" ->
		   case check_order(QueryString) of
			   {true,CheckStr} ->
				   GameRole = proplists:get_value("app_ext1", QueryString),
				   [_ServerID,RoleID] = string:tokens(GameRole,"{},"),
				   RoleID2 = list_to_integer(RoleID),
                   {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
                    case SrcType of
                        6->
        				   PayAccountID = call_get_360_accountID(Ucid),
        				   case RoleAccID of
        					   PayAccountID ->
        						   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
        						   Req:ok({"text/html; charset=utf-8", Reply}),
        						   Amount = proplists:get_value("amount", QueryString),
        						   Amount2 = list_to_integer(Amount) div 10,
        						   Sign = proplists:get_value("sign", QueryString),
        						   pay_gold2(RoleID2,Amount2,CheckStr,Sign,6);
        					   _->
        						   Reply = ejson:encode({[{<<"result">>,5}]}),
        						   Req:ok({"text/html; charset=utf-8", Reply})
        				   end;
                       _->
                            ?ERR("type error ~w",[SrcType]),
                            Reply = ejson:encode({[{<<"result">>,0}]}),
                            Req:ok({"text/html; charset=utf-8", Reply}),
                            ?ERR("360 pay failed order:~w",[QueryString])
                   end;
			   {false,Reason} ->
				   Reply = ejson:encode({[{<<"result">>,0}]}),
				   Req:ok({"text/html; charset=utf-8", Reply}),
				   ?ERR("360 check_order failed. reason:~w,order:~w",[Reason,QueryString])
		   end;
	   true ->
		   Reply = ejson:encode({[{<<"result">>,0}]}),
		   Req:ok({"text/html; charset=utf-8", Reply}),
		   ?ERR("360 pay failed order:~w",[QueryString])
	end.


pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_360(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================

check_order(QueryString) ->
	KeyList = proplists:get_keys(QueryString),
	KeyList2 = lists:sort(KeyList),
	{CheckStr,_} = lists:foldl(fun(E,{Acc,QS}) ->
										if E =:= "sign" orelse E =:= "sign_return" ->
												{Acc,QS};
											true ->
												Value = proplists:get_value(E,QS),
												{Acc++Value++"#",QS}
										end
									end,
					{[],QueryString},KeyList2),
	AppSec = "9a29bf83b3752321445b0bd3ccbbbafe",
	CheckStr2 = CheckStr ++ AppSec,
	Md5Local = md5(CheckStr2),
	Md5 = proplists:get_value("sign",QueryString),
	%%?ERR("Md5:~w,Md5Local:~w, CheckStr:~w",[Md5,Md5Local,CheckStr2]),
	if Md5 =:= Md5Local ->
			{true,CheckStr};
		true ->
			{false,2}
	end.

call_get_360_accountID(Ucid) ->
	AccountServerAddr = data_setting:get(account_server_url),
	URL = lists:flatten(io_lib:format("~s/get360ID?id=~s", [AccountServerAddr,Ucid])),
	case httpc:request(get, {URL, []}, [{timeout, 3000}], []) of
		{ok, {_,_,Content3}} ->
			erlang:list_to_integer(Content3);
		_ ->
			0
	end.


md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

fix_Receipt(QueryString) ->
	KeyList = proplists:get_keys(QueryString),
	{FixedReceipt,_} = lists:foldl(fun(E,{Acc,QS}) ->
										Value = proplists:get_value(E,QS),
										{[{list_to_atom(E),list_to_atom(Value)}|Acc],QS}
									end,
					{[],QueryString},KeyList),
	FixedReceipt.
