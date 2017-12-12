%% @author : lixinglong
%% @doc : web mod

-module(ban_roleID_mod).
-export([ban_roleID/1]).
-include("common.hrl").
%% type 0:ban by roleID, 1:free by roleID
%% reply error code 2:no such roleID

ban_roleID(Req)->
	case parse_Req(Req) of
		{ok, Type, RoleID}->
			Result = 
				case Type of
					"0" ->
						case gateway_ban:ban_by_roleID(RoleID) of
							ok ->
								case role_lib:is_online(RoleID) of
									true ->
										?CATCH(role_server:stop(RoleID));
									_ ->
										ignore
								end,
								ok;
							{false, M}->
								M
						end;
					"1" ->
						case gateway_ban:free_by_roleID(RoleID) of
							ok ->
								ok;
							{false, M}->
								M
						end;
					X ->
						?ERR("error type:~w",[X])
				end,
			case Result of
				ok ->
					Reply = ejson:encode({[{<<"result">>,<<"succ">>}]}),
					Req:ok({"text/html; charset=utf-8", Reply});
				N->
					Reply = ejson:encode({[{<<"result">>,N}]}),
					Req:ok({"text/html;charset=utf-8", Reply})
			end;
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
	httpc:request(post, {"http://192.168.1.27:8089/ban",
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
	httpc:request(post, {"http://192.168.1.27:8089/ban",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).
