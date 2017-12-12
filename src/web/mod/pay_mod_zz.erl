%% @author zxystar
%% @doc @todo Add description to pay_mod_zz.


-module(pay_mod_zz).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-include("common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
	QueryStringT = Req:parse_post(),
	?DEBUG("ZZ SDK:~w",[QueryStringT]), 
	Md5 = proplists:get_value("sign",QueryStringT),
	Md53 = zz_cal_string(Md5),
	NTData = proplists:get_value("nt_data",QueryStringT),
	Md52 = zz_cal_md5("nt_data="++NTData),
	?DEBUG("MD5 1:~w,2:~w,NTData:~w",[Md5,Md52,NTData]),
	case Md52 =:= Md53 of
		false ->
			Reply = ejson:encode({[{<<"result">>,0}]}),
			Req:ok({"text/html; charset=utf-8", Reply});
		true ->
			%platform_tool:return(Req, Reply),

			NTData2 = zz_cal_string(NTData),
			{ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(NTData2),
			[{_,_,_,_,GameRole,_}] = xmerl_xpath:string("//game_role/text()",ParsedDocumentRootElement),
			[{_,_,_,_,Amount,_}] = xmerl_xpath:string("//amount/text()",ParsedDocumentRootElement),
			[{_,_,_,_,Status,_}] = xmerl_xpath:string("//status/text()",ParsedDocumentRootElement),
			[_ServerID,RoleID] = string:tokens(GameRole,"{},"),

			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(list_to_integer(RoleID)),
			case RRType of
				5 ->
					
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					
					if Status =:= "0" ->
						   Amount2 = list_to_float(Amount),
						   pay_gold2(list_to_integer(RoleID),trunc(Amount2*10),NTData2,Md52,5);
					   true->
						   ?ERR("OrderStatus err:~w",[NTData2])
					end ;
				_->
					QS = mochiweb_util:urlencode(QueryStringT),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end
	end.


pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_zz(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

%% ====================================================================
%% Internal functions
%% ====================================================================

get_value(Response, Key) when is_list(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

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

%% zz接口相关函数
zz_cal_md5(QS) ->
	Md5 = md5(QS),
	P1 = string:substr(Md5,1,1),
	P2 = string:substr(Md5, 2,1),
	P3 = string:substr(Md5, 3, 3),
	P4 = string:substr(Md5,6,1),
	P5 = string:substr(Md5,7,1),
	P6 = string:substr(Md5,8,1),
	P7 = string:substr(Md5, 9, 5),
	P8 = string:substr(Md5, 14, 1),
	P9 = string:substr(Md5, 15, 3),
	P10 = string:substr(Md5, 18, 1),
	P11 = string:substr(Md5, 19, 5),
	P12 = string:substr(Md5, 24, 1),
	P13 = string:substr(Md5, 25),
	Md52 = P1++P8++P3++P10++P5++P12++P7++P2++P9++P4++P11++P6++P13,
	Md52.
		
zz_cal_string(NTData) ->
	ZZKey = data_setting:get(zz_key),
	KeyLength = length(ZZKey),
	NTData2 = string:tokens(NTData,"@"),
	NTData3 = [list_to_integer(E)|| E<- NTData2],
	
	{Data4,_,_} = lists:foldl(fun(E,{Acc,VZZKey,VLength}) ->
									Len = length(Acc),
									Len2 = (Len rem VLength) + 1,
									Word = lists:nth(Len2, VZZKey),
									E2 = E - Word,
									{[E2|Acc],VZZKey,VLength}
								end,
					{[],ZZKey,KeyLength},NTData3),
	Data5 = lists:reverse(Data4),
	io:format("DATA4:~w",[Data5]),
	Data5.


%% ================
%%     test
%% ================

test()->
	QueryStringT = [{[110,116,95,100,97,116,97],[64,49,55,51,64,49,55,54,64,50,49,53,64,50,48,56,64,50,49,55,64,49,51,53,64,50,49,57,64,49,57,54,64,50,50,49,64,50,49,54,64,50,50,54,64,50,48,54,64,50,50,50,64,49,55,51,64,49,52,55,64,49,54,50,64,49,52,49,64,49,52,55,64,49,52,51,64,49,51,53,64,50,48,50,64,50,48,53,64,50,48,54,64,50,49,50,64,50,50,49,64,50,48,48,64,50,50,50,64,50,49,53,64,49,55,52,64,49,52,55,64,49,56,48,64,49,56,51,64,49,55,57,64,49,52,56,64,49,53,55,64,49,50,57,64,49,51,57,64,50,49,54,64,50,51,55,64,49,57,50,64,50,50,50,64,50,49,50,64,50,49,48,64,50,50,49,64,50,48,54,64,50,48,57,64,50,49,48,64,49,54,52,64,49,51,53,64,50,48,53,64,50,49,56,64,49,51,53,64,49,56,52,64,49,53,55,64,49,55,50,64,50,49,49,64,50,50,50,64,50,49,54,64,49,57,54,64,49,57,52,64,50,49,56,64,50,48,52,64,50,49,54,64,50,49,48,64,50,48,52,64,50,48,52,64,50,50,50,64,49,53,55,64,49,55,50,64,50,50,49,64,50,49,52,64,50,50,56,64,50,49,48,64,49,57,54,64,50,49,50,64,50,48,52,64,49,54,51,64,49,53,53,64,50,49,53,64,50,49,50,64,50,50,52,64,50,48,48,64,50,50,50,64,50,48,55,64,50,50,51,64,50,49,48,64,50,48,52,64,50,48,48,64,49,55,49,64,49,56,56,64,49,53,48,64,49,52,51,64,49,54,48,64,49,53,52,64,49,55,53,64,49,52,52,64,49,54,55,64,49,54,51,64,49,55,51,64,49,54,48,64,50,48,51,64,50,49,48,64,50,49,50,64,50,48,56,64,50,49,49,64,49,57,48,64,50,49,55,64,49,57,56,64,50,51,48,64,49,57,54,64,49,55,52,64,49,55,50,64,50,49,54,64,50,49,48,64,50,48,52,64,50,48,48,64,50,48,52,64,50,49,55,64,50,49,50,64,50,48,51,64,50,48,56,64,49,54,51,64,49,55,49,64,49,52,51,64,49,54,48,64,49,54,48,64,49,54,49,64,49,54,50,64,49,52,53,64,49,53,57,64,49,53,54,64,50,48,54,64,49,57,56,64,50,48,52,64,50,48,56,64,49,57,54,64,50,51,53,64,50,48,54,64,50,50,48,64,50,49,51,64,49,55,53,64,49,55,51,64,50,48,54,64,50,49,51,64,50,48,57,64,50,48,52,64,50,49,53,64,49,57,48,64,50,49,50,64,50,48,49,64,49,56,51,64,49,52,56,64,49,54,52,64,49,54,51,64,49,54,55,64,49,54,57,64,49,54,48,64,49,55,56,64,49,53,56,64,49,53,49,64,49,52,57,64,49,52,51,64,49,53,53,64,49,52,57,64,49,54,57,64,49,53,50,64,49,54,56,64,49,54,51,64,49,54,55,64,49,55,51,64,49,52,50,64,50,49,48,64,50,50,51,64,50,48,51,64,50,48,50,64,50,48,57,64,50,48,50,64,50,48,54,64,50,50,49,64,49,53,55,64,49,55,50,64,50,50,52,64,50,49,48,64,50,51,52,64,49,57,48,64,50,49,53,64,50,49,52,64,50,49,50,64,50,48,50,64,49,53,55,64,49,53,55,64,49,52,57,64,49,55,48,64,49,52,54,64,49,53,55,64,49,54,48,64,49,55,48,64,49,53,56,64,49,52,53,64,49,53,50,64,49,52,49,64,49,53,51,64,49,53,50,64,49,53,51,64,49,53,54,64,49,53,49,64,49,55,57,64,49,52,52,64,49,54,56,64,49,55,50,64,49,54,48,64,50,50,53,64,49,57,50,64,50,50,48,64,50,48,52,64,50,49,57,64,50,48,54,64,50,48,52,64,50,48,56,64,49,54,51,64,49,56,49,64,49,57,50,64,50,50,49,64,50,50,51,64,50,51,48,64,50,50,51,64,50,49,49,64,49,54,49,64,49,53,55,64,49,52,57,64,49,53,48,64,49,52,51,64,49,54,55,64,49,52,56,64,50,49,56,64,50,48,52,64,50,50,51,64,50,50,57,64,50,50,51,64,50,50,57,64,49,53,55,64,49,53,57,64,50,50,52,64,50,49,57,64,49,57,56,64,50,49,49,64,50,50,52,64,50,49,54,64,49,56,51,64,49,52,51,64,49,55,50,64,49,53,57,64,50,50,56,64,50,50,57,64,49,57,50,64,50,49,53,64,50,50,54,64,50,49,56,64,49,54,51,64,49,53,53,64,49,53,52,64,50,49,48,64,50,50,50,64,50,49,48,64,50,50,55,64,50,48,57,64,50,49,54,64,50,49,52,64,49,53,55,64,49,53,57,64,49,53,54,64,50,48,50,64,50,49,48,64,49,57,56,64,50,48,56,64,49,57,54,64,50,51,48,64,49,57,54,64,50,50,55,64,50,50,55,64,50,49,48,64,50,49,54,64,49,57,54,64,49,54,49]},{[115,105,103,110],[64,50,49,48,64,49,54,49,64,49,52,52,64,49,57,57,64,49,54,54,64,50,48,49,64,49,53,52,64,49,52,51,64,50,48,57,64,49,57,56,64,49,55,51,64,49,52,51,64,50,49,50,64,50,49,51,64,50,49,53,64,50,49,53,64,49,53,50,64,49,52,55,64,50,49,48,64,49,53,52,64,49,57,56,64,49,52,53,64,49,54,48,64,49,53,53,64,49,55,54,64,49,53,48,64,49,54,51,64,50,49,51,64,50,49,51,64,49,54,56,64,49,57,54,64,49,52,56]}],
	Md5 = proplists:get_value("sign",QueryStringT),
	Md53 = zz_cal_string(Md5),
	NTData = proplists:get_value("nt_data",QueryStringT),
	Md52 = zz_cal_md5("nt_data="++NTData),
	io:format("MD5 1:~w,2:~w,NTData:~w",[Md53,Md52,NTData]),
	case Md52 =:= Md53 of
		false ->
			Reply = "failed";
			%Req:ok({"text/html; charset=utf-8", Reply});
		true ->
			NTData2 = zz_cal_string(NTData),
			{ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(NTData2),
			[{_,_,_,_,RoleID,_}] = xmerl_xpath:string("//game_role/text()",ParsedDocumentRootElement),
			[{_,_,_,_,Amount,_}] = xmerl_xpath:string("//amount/text()",ParsedDocumentRootElement),
			[{_,_,_,_,Status,_}] = xmerl_xpath:string("//status/text()",ParsedDocumentRootElement),
			io:format("parsed document:~w",[RoleID]),
			%%_++"<game_role>"++RoleID++"</game_role>"++_++"<amount>"++Amount++"</amount>"++_++"<status>"++Status++"</status>"++_ = NTData2,
			Reply = "success",
			%Req:ok({"text/html; charset=utf-8", Reply}),
			if Status =:= "0" ->
					Amount2 = list_to_float(Amount);
					%pay_gold2(RoleID,trunc(Amount2)*10,NTData2,Md52,5);
			   true->
					 ?ERR("OrderStatus err:~w",[NTData2])
			end 
	end.

