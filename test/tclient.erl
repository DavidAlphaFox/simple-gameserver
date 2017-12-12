%% @author caohongyang
%% @doc 模拟前端
%% Created 2013-2-20


-module(tclient).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================
-define(TCP_TIMEOUT, -1). % 解析协议超时时间
-define(HEART_TIMEOUT, -1). % 心跳包超时时间
-define(HEART_TIMEOUT_TIME, 100). % 心跳包超时次数
-define(HEADER_LENGTH, 2). % 消息头长度
%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.
	
%% 连接服务器
connect() ->
	Port = data_setting:get(game_port),
	{ok, S} = gen_tcp:connect("localhost",Port, ?TCP_OPTIONS),
	put(socket,S),
	{ok, S}.

send_p(Record) ->
	Socket = get(socket),
	tk_misc:send_sock(Socket, Record).

%% 创建服务器消息处理程序
spawn_receiver() ->
	Socket = get(socket),
	Father = self(),
	ReceiverPid= spawn(fun() ->
							   put(father, Father),
				  receive {go,S} ->recv_loop(S)  end
		  end),
	ok = gen_tcp:controlling_process(Socket, ReceiverPid),
	ReceiverPid ! {go, Socket},
	put(receiver_pid, ReceiverPid).

%% 接收服务器消息
recv_loop(Socket) ->
	Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	receive
		{inet_async, Socket, Ref, {ok, <<BodyLen:16>>}} ->
			Ref1 = async_recv(Socket, BodyLen-2, ?TCP_TIMEOUT),
			receive
				{inet_async, Socket, Ref1, {ok, Binary}} ->
					Msg = proto:decode(Binary),
					erlang:send(get(father), Msg),
					recv_loop(Socket);
				Msg ->
					io:format("unexpected msg=~w",[Msg])
			end;
		Msg ->
			io:format("unexpected msg=~w",[Msg])
	end.
	
%% 模拟登录
login_role(RoleID) when is_integer(RoleID) ->
	case mnesia:dirty_read(?dbRole, RoleID) of
		[] ->
			no_such_role;
		[#role{accid=Accid}] ->
			login(Accid)
	end.
login(Accid) ->
	spawn(fun() ->login2(Accid) end).

login2(Accid) ->
	put(accountName, Accid),
	connect(),
	spawn_receiver(),
	auth(),
	request_roleInfo(),
	timer:sleep(1000000).

%% 登录认证
auth() ->	
	Accid= get(accountName),
	NowSec=util:now(),
	Ticket = util:md5(integer_to_list(Accid)++ integer_to_list(NowSec) ++ ?TICKET),
	Record = #cs_account_login{accountName="",
							   unixTime=NowSec,
							   userID=Accid,
							   ticket=Ticket,
							   macAddr="",
                               serverID=data_setting:get(server_id)},
	send_p(Record),
	receive 
		#sc_account_login{result=Result, isCreated = IsCreated} ->
			if Result =/= 1 ->
				   io:format("login auth fail, result=~w\n",[Result]),
				   exit(fail);
			   true ->
				   io:format("auth success\n"),
				   ensure_role_exist(IsCreated)
			end
	end.

%% 获取角色列表（并创建角色）
ensure_role_exist(true)->
	next;
ensure_role_exist(false)->
	create_role().
					

%% 创建角色
create_role() ->
	send_p(#cs_account_create{sex=1,roleName=get(accountName)}),
	receive
		#sc_account_create{result=Result} ->
			if Result =:= 1 ->
				   io:format("create sucess\n"),
				   next;
			   true ->
				   io:format("create_role fail, result=~w\n",[Result]),
				   exit(fail)
			end
	end.

%% 进入游戏
%% enter_game() ->
%% 	send_p(#cs_account_enter_game{}),
%% 	receive
%% 		#sc_account_enter_game{result=Result} ->
%% 			if Result =:= 1 ->
%% 				   next;
%% 			   true ->
%% 				   io:format("enter_game fail, result=~w\n",[Result]),
%% 				   exit(fail)
%% 			end
%% 	end.

%% 获取角色基本信息
request_roleInfo() ->
	send_p(#cs_role_info{}),
	receive
		#sc_role_info{
					  roleID=RoleID}=Msg ->
			io:format("msg=~10000p",[Msg]),
			put(roleID,RoleID),
			next
		
	end.
		