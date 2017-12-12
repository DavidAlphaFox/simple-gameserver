%% @author : lixinglong
%% @doc : web mod

-module(ban_roleID_list_mod).
-export([ban_roleID/1]).
-include("common.hrl").
%% type 0:ban by roleID, 1:free by roleID
%% reply error code 2:no such roleID

ban_roleID(Req)->
	case parse_Req(Req) of
		{ok, true,ok, Message,DevInfo}->
			do_change(Message,DevInfo),
			
			Reply = ejson:encode({[{<<"result">>,<<"succ">>}]}),
			Req:ok({"text/html; charset=utf-8", Reply});
		{_,S,_,_,_} ->
			Code = case S of false -> <<"timeout">>;_ -> <<"err">> end, 
			Reply = ejson:encode({[{<<"result">>,Code}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

parse_Req(Req)->
	QueryString = Req:parse_post(), 
	Ip = Req:get(peer),
	Pass = proplists:get_value("pass", QueryString),
	Message = proplists:get_value("message", QueryString),
	Ticket = proplists:get_value("ticket", QueryString),
	DevInfo = proplists:get_value("dev", QueryString),
	?ERR("ban_roleID_list_operation:~p,~p",[Ip,Message]),
	{can_pass(Pass, Message),check_ticket(Ticket),check_ip(Ip), Message,DevInfo}.

can_pass(Pass, Message)	->
	%% 认证消息是否有效,认证失败,则消息不进行公告广播
	{Auth, _, _} = data_setting:get(passinfo),
	Auth3 = util:md5(lists:merge(Auth, Message)),
	
	if Pass =:= Auth3 -> ok;
	   true -> ok
	end.

check_ip("10.10.60.243") -> ok;
check_ip("10.6.12.72") -> ok;
check_ip("10.10.11.207") -> ok;
check_ip("10.10.11.11") -> ok;
check_ip(Ip) -> ?ERR("ip:~s",[Ip]),false.
	

check_ticket(Ticket) ->
	{_,GTicket} = gen_ticket(),
	case GTicket of Ticket -> true;
		_ -> false
	end.

gen_ticket()->
	{{_,M,D},{H,_,_}} = erlang:localtime(),
	H2 = (H div 4)*3 + H div 2,
	S = erlang:integer_to_list(M * 32 + D, 33)++erlang:integer_to_list(H2, 33),
	Ticket = base64:encode(S),
	{S,erlang:binary_to_list(Ticket)}.

do_change(Message,DevInfo) ->
	{MessageL} = ejson:decode(Message),
	{[{Dev0,LogTabNames}]} = ejson:decode(DevInfo),
	Dev = erlang:binary_to_list(Dev0),
	%LogTabName = erlang:binary_to_list(LogTabName0),
	[pro_role_data(erlang:binary_to_list(RoleID),erlang:binary_to_list(Level),Dev,LogTabNames)||{RoleID,Level}<-MessageL].

pro_role_data(RoleID,Level,DevInfo,LogTabNames) ->
	case db_sql:get_row("select roleID,Level from gRole where roleID = " ++ RoleID) of
		[RoleID_1,Level_1] ->
			Sql = io_lib:format("update gRole set level=~w, devID = '~s' where roleID = ~s", [list_to_integer(Level),DevInfo, RoleID]),
			db_sql:sql_execute_with_log(Sql),
			?ERR("change_role_level:~w,~w,~s,~s",[RoleID_1,Level_1,RoleID,Level]),
			gateway_ban:ban_by_roleID(RoleID_1),
			Sql2 = io_lib:format("update t_gold_pay_add set devID = '~s' where roleID = ~w",[DevInfo, RoleID_1]),
			db_sql:sql_execute_with_log(Sql2),
						Sql3 = io_lib:format("update t_pay_stastics set devID = '~s' where roleID = ~w",[DevInfo, RoleID_1]),
			db_sql:sql_execute_with_log(Sql3),
			[begin
				 Sql4 = io_lib:format("update ~s set devID = '~s' where roleID = '~w'",[LogTabName, DevInfo,RoleID_1]),
				 db_sql:sql_execute_with_log(Sql4)
			 end|| LogTabName <- LogTabNames];
		_ ->
			?ERR("not find role:~s",[RoleID])
	end.


%-------------------------------------------------------------------------------------------------------------
web_test()	->
	inets:start(),
	Message=mochijson2:encode({[{"2000001","8"},{"2000002","9"}]}),
	Dev = ejson:encode({[{<<"02000000">>,[<<"loglogin_2016_9">>,<<"loglogin_2016_8">>,<<"logcreaterole_2016_9">>]}]}),
	Pass = util:md5("passed"++Message),
	Type="0",
	{_,Ticket} = gen_ticket(),
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s&ticket=~s&dev=~s", 
									  [Pass,Message, Type,Ticket,Dev])),
	io:format("~s\n",[Arg]),
	httpc:request(post, {"http://10.10.11.11:1789/ban2",
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
	httpc:request(post, {"http://192.168.1.27:8089/ban2",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).




