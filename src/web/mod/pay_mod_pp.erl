%% @author lxl
%% @doc @todo Add description to pay_mod_pp


-module(pay_mod_pp).
-include("common.hrl").

-include_lib("public_key/include/public_key.hrl"). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req)->
	QueryString = Req:parse_post(),
	Info = decode_data(QueryString),
	OrderID = proplists:get_value("order_id", QueryString),
	BillNo = proplists:get_value("billno", QueryString),
	Account = proplists:get_value("account", QueryString),
	Amount = proplists:get_value("amount", QueryString),
	Status = proplists:get_value("status", QueryString),
	AppID = proplists:get_value("app_id", QueryString),
	case verify_info(Info, OrderID, BillNo, Account, Amount, Status, AppID) of
		true ->
			GameRole = proplists:get_value("roleid", QueryString),
			[_ServerID,RoleID] = string:tokens(GameRole,"{},"),
			RoleID2 = list_to_integer(RoleID),
			{RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
			case SrcType of
				11->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					Amount2 = erlang:trunc(erlang:list_to_float(Amount)) * 10,
					Body = Req:recv_body(),
					MSign = md5(Body),
					QS = mochiweb_util:urlencode(QueryString),
					pay_gold2(RoleID2,Amount2,QS,MSign,11);
				_->
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					?ERR("pp pay for other SrcType:~w ~n",[SrcType])
			end;
		_ ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply}),
			?ERR("pp check_order failed. reason:sign wrong,order:~w",[QueryString])
	end.


decode_data(QueryString)->
    PubKey = <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtvLIffGO5aixqQF5phejh1TIw/hRE5hVu7KRDHi6FO6hTA8Qkz7F3/nxUHevErMcARHuOgkVXsHXO8/VT2WoZ/Hw2pLzIKSlt2UpIbOtIP9SPnhq4ekO9RPLFa2sf+prrXBpYLoX4ogoii15Gf7jN1BoUBG/AuCH3yMeCEViVa8bfNRviDvSObvOEnT8Ppmm2R5kL4IRATDS37KXXwygPGwaUT247RsSMl2Us+LhYR7eWqhY3/usV2eU5y+k9AdDaa66la4vn9ON2Gp3H79PeG7he/owVEYl+ECkqVt2CPhTtH9CErnF0XeakF5NUjykKqdwPyShBkaQx2gJyRk7UQIDAQAB\n-----END PUBLIC KEY-----">>,
	Base64Sign = proplists:get_value("sign",QueryString),
	Sign = base64:decode(Base64Sign),
	PemEntries = public_key:pem_decode(PubKey),
	RSAPubKey = public_key:pem_entry_decode(hd(PemEntries)),
	Data = public_key:decrypt_public(Sign, RSAPubKey,['rsa_pkcs1_padding']),
	{struct, List}=mochijson2:decode(Data),
	List.
	
	

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_pp(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].



%% ====================================================================
%% Internal functions
%% ====================================================================

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


verify_info(Info, VOrderID, VBillNo, VAccount, VAmount, VStatus, VAppID)->
	OrderID = binary:bin_to_list(proplists:get_value(<<"order_id">>, Info)),
	BillNO = binary:bin_to_list(proplists:get_value(<<"billno">>, Info)),
	Account = binary:bin_to_list(proplists:get_value(<<"account">>, Info)),
	Amount = binary:bin_to_list(proplists:get_value(<<"amount">>, Info)),
	Status = erlang:integer_to_list(proplists:get_value(<<"status">>, Info)),
	AppID = binary:bin_to_list(proplists:get_value(<<"app_id">>, Info)),
	
	if OrderID == VOrderID , BillNO == VBillNo, Account == VAccount , 
	   Status == VStatus, AppID == VAppID ,Amount == VAmount ->
		if Status =:= "0" ->
		   true;
		true ->
		  false
		end;
	   true ->
		   false
	end.

