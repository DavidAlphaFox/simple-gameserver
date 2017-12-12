%% @author lxl
%% @doc @todo Add description to pay_mod_ttios.


-module(pay_mod_ttios).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QS = Req:recv_body(),
	Sign = Req:get_header_value("sign"),
	QS2 = http_uri:decode(erlang:binary_to_list(QS)),
	{QueryString} = ejson:decode(QS2),
	case check_sign(QS2,Sign) of
		true ->
			case get_value(QueryString,<<"payResult">>) of
				<<"1">>  ->
					%Amount = proplists:get_value(<<"payFee">>, QueryString),
					AmountBin = proplists:get_value(<<"payFee">>, QueryString),         
                    Amount = list_to_float(binary_to_list(AmountBin)),
					GameRoleBin = proplists:get_value(<<"exInfo">>, QueryString),
					GameRole = binary_to_list(GameRoleBin),
					[_ServerID,RoleID] = string:tokens(GameRole,"."),
					RoleID2 = list_to_integer(RoleID),
					Amount2 = trunc(Amount * 10),
					QSL = mochiweb_util:urlencode(QueryString),
					{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID2),
					case RRType of
						122 ->
							pay_gold2(RoleID2,Amount2,QSL,Sign,122),
							Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply});
						_ ->
							?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QSL]),
							Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
							Req:ok({"text/html; charset=utf-8", Reply})
					end;
				_ ->
					Reply = ejson:encode({[{<<"result">>,0}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end. 
	
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_tt(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.



check_sign(QS,Sign) ->
	base64:encode_to_string(erlang:md5(re:replace(QS,"\\+"," ",[global,caseless,{return,list}])++"31eb8a85cf10ce0c4948ab372fcb8aec"))==Sign.


