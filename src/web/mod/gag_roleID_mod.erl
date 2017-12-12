%% @author : lixinglong
%% @doc : web mod

-module(gag_roleID_mod).
-export([gag_roleID/1]).
-include("common.hrl").
%% type 0:gag one by roleID, 1:free one by roleID 2:get gag list,message=000


gag_roleID(Req)->
	case parse_Req(Req) of
		{ok, Type, RoleID}->
			Info = 
			case Type of
				"0" ->
					talk_server:gag_one(RoleID),
					ok;
				"1" ->
					talk_server:ungag_one(RoleID),
					ok;
				"2" ->
					talk_server:get_gag_list()
			end,
			Reply =ejson:encode({[{<<"result">>,Info}]}),
			Req:ok({"text/html; charset=utf-8", Reply});
		_ ->
			Reply = ejson:encode({[{<<"result">>,<<"err">>}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

parse_Req(Req)->
	QueryString = Req:parse_post(), 
	Pass = proplists:get_value("pass", QueryString),
	Message = proplists:get_value("message", QueryString),
	Type = proplists:get_value("type", QueryString),
	RoleID = erlang:list_to_integer(Message),
	{can_pass(Pass, Message),Type, RoleID}.

can_pass(Pass, Message)	->
	%% 认证消息是否有效,认证失败,则消息不进行公告广播
	{Auth, _, _} = data_setting:get(passinfo),
	Auth3 = util:md5(lists:merge(Auth, Message)),
	
	if Pass =:= Auth3 -> ok;
	   true -> ok
	end.

%-------------------------------------------------------------------------------------------------------------
web_test(M)	->
	inets:start(),
	RoleID=M,
	Message=http_uri:encode(erlang:integer_to_list(RoleID)),
	Pass = util:md5("passed"++Message),
	Type="0",
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s", 
									  [Pass,Message, Type])),
	io:format("~s\n",[Arg]),
	httpc:request(post, {"http://127.0.0.1:8089/gag",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).
web_test2(M)	->
	inets:start(),
	RoleID=M,
	Message=http_uri:encode(erlang:integer_to_list(RoleID)),
	Pass = util:md5("passed"++Message),
	Type="1",
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s", 
									  [Pass,Message, Type])),
	io:format("~s\n",[Arg]),
	httpc:request(post, {"http://127.0.0.1:8089/gag",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).

web_test3()	->
	inets:start(),
	RoleID=000,
	Message=http_uri:encode(erlang:integer_to_list(RoleID)),
	Pass = util:md5("passed"++Message),
	Type="2",
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s", 
									  [Pass,Message, Type])),
	io:format("~s\n",[Arg]),
	httpc:request(post, {"http://127.0.0.1:8089/gag",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).
