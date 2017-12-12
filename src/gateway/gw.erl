%% @author caohongyang
%% @doc 玩家网关进程
%% 负责：登录验证、创建角色
%% Created 2013-2-20

%% ~~~~~！！！！
%% 请保证此进程的进程字典只有一个socket值，因为role_lib:send_client的特殊实现。

-module(gw).
-compile(export_all).

-export([start_client/1, start_link/0, init/0]).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-define(TCP_TIMEOUT, -1). % 解析协议超时时间
-define(HEART_TIMEOUT, 300000). % 心跳包超时时间
-define(HEART_TIMEOUT_SEC, 300000). % 心跳包超时描述
-define(HEADER_LENGTH, 2). % 消息头长度
-define(TICK_INTERVAL, 5*1000).%TICK间隔
%% ======================================
%% dictionary key in this process
-define(roleInfo, roleInfo).
-define(socket, socket).

%% ======================================
-define(test_attacker_list, (ger_attr:test_ger_list())).
-define(test_all_ger, (ger_attr:test_all_ger())).
%% #ger{gerID=1,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=1,gerTypeID=12101,gerKingdom=1,gerSkill1=1,gerSkill2=2,gerPos=1,gerProMean=100,gerHpToProMean=100},
%% 	  gerAttr=#gerAttr{gerID=1,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=2,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=2,gerTypeID=12102,gerKingdom=1,gerSkill1=1,gerSkill2=3,gerPos=2,gerProMean=100,gerHpToProMean=100},
%% 	  gerAttr=#gerAttr{gerID=2,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=3,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=3,gerTypeID=12103,gerKingdom=1,gerSkill1=1,gerSkill2=4,gerPos=3,gerProMean=100,gerHpToProMean=100},
%% 	  gerAttr=#gerAttr{gerID=3,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=4,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=4,gerTypeID=12104,gerKingdom=1,gerSkill1=1,gerSkill2=5,gerPos=4,gerProMean=100,gerHpToProMean=100},
%% 	  gerAttr=#gerAttr{gerID=4,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=5,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=5,gerTypeID=12105,gerKingdom=1,gerSkill1=1,gerSkill2=6,gerPos=5,gerProMean=100,gerHpToProMean=100},
%% 	  gerAttr=#gerAttr{gerID=5,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40},
%% #ger{gerID=6,gerBase=#gerBase{gerLevel=1,gerQuality=0,gerID=6,gerTypeID=12106,gerKingdom=1,gerSkill1=1,gerSkill2=7,gerPos=6,gerProMean=100,gerHpToProMean=100},
%% 	  gerAttr=#gerAttr{gerID=6,gerAttack=400,gerHpMax=2000,gerSpInit=40,gerSpMax=30,gerCritic=300,gerCriticReduce=100,gerDoom=200,gerMiss=100,gerAbsorb=10,gerDamageBack=10,gerReel=30,gerReelReduce=15,gerPhyDef=50,gerPhyDefBite=20,gerMagDef=50,gerMagDefBite=20},
%% 	  gerHp=2000,gerSp=40}]).


%%记录客户端进程
-record(client, {
				 socket = undefined,
				 roleServer= none,
				 roleID=0,
				 login  = 0,
				 accid  = 0,
				 accname = none,
				 timeout = 0, % 上次心跳时间
				macAddr = 0,
				 ip=0,
				 deviceID="",
                 srcType=0,
				 role_beat=0
				}).

%% 开启客户端服务
start_client(Sock) ->
	{ok, Child} = supervisor:start_child(gateway_tcp_client_sup, []),
	ok = gen_tcp:controlling_process(Sock, Child),
	Child ! {go, Sock}.

start_link() ->
	{ok, spawn_link(?MODULE, init, [])}.

%%gen_server init
%%Host:主机IP
%%Port:端口
init() ->
	process_flag(trap_exit, true),
	receive
		{go, Socket} ->
	Client = #client{
					 roleServer = none,
					 login  = 0,
					 accid  = 0,
					 accname = none,
					 timeout = util:now(),
					 role_beat = util:now()
					},
			put(?socket, Socket),
			tick(),
			login_parse_packet(Socket, Client)
	end.

%%接收来自客户端的数据 - 先处理登陆
%%Socket：socket id
%%Client: client记录
login_parse_packet(Socket, Client) ->
    async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    login_parse_packet1(Socket, Client).

login_parse_packet1(Socket, Client) ->
    receive
        %%登陆处理
        {inet_async, Socket, _Ref, {ok, <<BodyLen:16>>}} ->
            case BodyLen > 0 of
                true ->
                    _Ref1 = async_recv(Socket, BodyLen-?HEADER_LENGTH, ?TCP_TIMEOUT),
                    login_parse_packet2(Socket, Client);
                false ->
                    login_lost(Socket, Client, 7, "WPE encounted！！")
            end;
        tick ->
            do_login_tick(Socket, Client);
		role_beat ->
			do_parse_packet(Socket,Client#client{role_beat=util:now()});
        %%用户断开连接或出错
        Other ->
            login_lost(Socket, Client, 8, Other)
    end.

login_parse_packet2(Socket, Client) ->
    GuiCreateRole = data_common:get(guiCreateRole),
    receive
        {inet_async, _, _, {error, timeout}} ->
            login_parse_packet(Socket, Client);
        {inet_async, Socket, _, {ok, Binary}} ->
            ?DEBUG("receive packet=~w",[Binary]),
            case proto:decode(Binary) of
                %% 验证版本号
                #cs_version{version=Version}->
                    check_client_version(Version,Socket,Client);
                %%先验证登陆
                #cs_account_login{
                                  userID=UserID,
                                  accountName=Accname,
                                  macAddr=MacAddr,
                                  serverID=ServerID,
                                  deviceID=DeviceID,
                                  srcType=SrcType
                                 }=Data ->
                    ?INFO("DEBUG_L cs_account_login ~w",[Data]),
                    Accid = (ServerID + 1) * ?AccidBase + UserID,
                    IsServerIDBad = not lists:member(ServerID, [data_setting:get(server_id) | data_setting:get(merge_server_id_list)]),
                    OnlineNum = ets:info(?ETS_ROLE_ONLINE,size),
                    MaxOnlineNum = data_setting:get(max_online_num),
                    if OnlineNum > MaxOnlineNum ->
                           Reply = #sc_account_login{result=5,isCreated=false,isGerSelected=false,guiCreateRole=GuiCreateRole},
                           tk_misc:send_sock(Socket, Reply),
                           login_lost(Socket, Client, 1, "login full");
                       IsServerIDBad ->
                           Reply = #sc_account_login{result=6,isCreated=false,isGerSelected=false},
                           tk_misc:send_sock(Socket, Reply),
                           login_lost(Socket, Client, 2, "login send error server id");
                       true ->
                           case check_auth(Data) of
                               true ->                                            
                                   Client1 = Client#client{
                                                           login = 1,
                                                           accid = Accid,
                                                           accname = Accname,
                                                           socket=Socket,
                                                           macAddr=MacAddr
                                                          },
                                   %% 判断该帐号是否已经创建了主公
                                   case check_created(Accid) of
                                       {true, RoleID, IsSelectedGer} ->
                                           Reply = #sc_account_login{result=1,isCreated=true,isGerSelected=IsSelectedGer,guiCreateRole=GuiCreateRole},
                                           tk_misc:send_sock(Socket, Reply),
                                           do_enter_game(Accid, RoleID, Client1#client{roleID=RoleID,deviceID=DeviceID,srcType=SrcType}, Socket);
                                       false ->
                                           Reply = #sc_account_login{result=1,isCreated=false,isGerSelected=false,guiCreateRole=GuiCreateRole},
                                           tk_misc:send_sock(Socket, Reply),
                                           login_parse_packet(Socket, Client1#client{deviceID=DeviceID,srcType=SrcType})
                                   end;
                               {false,Reason} ->
                                   Reply = #sc_account_login{result=Reason,isCreated=false,isGerSelected=false,guiCreateRole=GuiCreateRole},
                                   tk_misc:send_sock(Socket, Reply),
                                   login_lost(Socket, Client, 3, "login fail")
                           end
                    end;
                
                %%创建角色
                #cs_account_create{}=Data ->
                    case Client#client.login == 1 of
                        true ->
                            case do_create_role(Client, Data) of
                                {true, RoleID}->
                                    post_create_process_to_platform(Client#client.accid, RoleID, 0),
                                    mail_server:send_notemp_mail(RoleID, 1),
                                    do_enter_game(0, RoleID, Client#client{roleID=RoleID}, Socket);
                                false ->
                                    login_parse_packet(Socket, Client)
                            
                            end;
                        false ->
                            login_lost(Socket, Client, 4, "request cs_account_create before request cs_account_login")
                    end;
                #cs_account_check_rolename{roleName=RoleName}->
                    case check_roleName(RoleName) of
                        ok->
                            Reply = #sc_account_check_rolename{result=1},
                            tk_misc:send_sock(Socket, Reply),
                            login_parse_packet(Socket, Client);
                        {false,Reason} ->
                            Reply = #sc_account_check_rolename{result=Reason},
                            tk_misc:send_sock(Socket, Reply),
                            login_parse_packet(Socket, Client)
                    end;
				#cs_account_demo{type=Type,isMale=IsMale,specialID=SpecialID} ->
					Reply = get_demo_fight(Type,IsMale,SpecialID),
					tk_misc:send_sock(Socket,#sc_account_demo{fightInfo = Reply}),
					login_parse_packet(Socket,Client);
                #cs_account_heart{} ->
                    NowSec = util:now(),
                    tk_misc:send_sock(Socket, #sc_account_heart{unixTime=NowSec}),
                    login_parse_packet(Socket, Client#client{timeout=NowSec});
                
                Other ->
                    login_lost(Socket, Client, 5, Other)
            end;
        tick ->
            do_login_tick2(Socket, Client);
		role_beat ->
			do_parse_packet(Socket,Client#client{role_beat=util:now()});
        Other ->
            login_lost(Socket, Client, 6, Other)
    end.

check_client_version(Version,Socket,Client)->
	{M,S,L} = data_setting:get(client_version),
	[CMT,CST,CLT|_] = re:split(Version, "[.]",[{return,list}]),
	CM = erlang:list_to_integer(CMT),
	CS = erlang:list_to_integer(CST),
	CL = erlang:list_to_integer(CLT),
	%Rst = compare_two_digit(CM,M) andalso compare_two_digit(CS,S) andalso compare_two_digit(CL,L),
	Rst = if CM < M ->
				 false;
			 CM =:= M ->
				 if CS < S ->
						false;
					CS =:= S ->
						if CL < L ->
							   false;
						   true ->
							   true
						end;
					true ->
						true
				 end;
			 true ->
				 true
		  end,

	case Rst of
		true->
			Reply = #sc_version{result=1},
			tk_misc:send_sock(Socket, Reply),
			login_parse_packet(Socket, Client);
		false ->
			Reply = #sc_version{result=2},
			tk_misc:send_sock(Socket, Reply),
			login_lost(Socket, Client, 9, "version error")
	end.

do_login_tick(Socket, Client) ->
	case check_timeout(Client) of
		true ->
			login_lost(Socket, Client, 10, timeout);
		false->
			tick(),
			login_parse_packet1(Socket, Client)
	end.	

do_login_tick2(Socket, Client) ->
    case check_timeout(Client) of
        true ->
            login_lost(Socket, Client, 10, timeout);
        false->
            tick(),
            login_parse_packet2(Socket, Client)
    end.

check_timeout(#client{timeout=LastSec}) ->
	NowSec = util:now(),
	% ?DEBUG("check_timeout = ~p:~p",[NowSec - LastSec, ?HEART_TIMEOUT_SEC]),
	NowSec - LastSec > ?HEART_TIMEOUT_SEC.
	


%%接收来自客户端的数据 - 登陆后进入游戏逻辑
%%Socket：socket id
%%Client: client记录
do_parse_packet(Socket, Client) ->
	receive
		{inet_async, Socket, _Ref, {ok, <<BodyLen:16>>}} ->
			case BodyLen > 0 of
				true ->
					async_recv(Socket, BodyLen-?HEADER_LENGTH, ?TCP_TIMEOUT),
					receive
						{inet_async, Socket, _Ref1, {ok, Binary}} ->
							?DEBUG("receive packet=~w",[Binary]),
							case proto:decode(Binary) of
								%%这里是处理游戏逻辑
								#cs_account_heart{} ->
									NowSec = util:now(),
									tk_misc:send_sock(Socket, #sc_account_heart{unixTime=NowSec}),
									async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
									do_parse_packet(Socket, Client#client{timeout=NowSec});
								#cs_account_logout{}	->
									do_lost(Socket,Client, {'EXIT',client_exit,normal});
								#cs_account_demo{type=Type,isMale=IsMale,specialID=SpecialID} ->
									Reply = get_demo_fight(Type,IsMale,SpecialID),
									tk_misc:send_sock(Socket,#sc_account_demo{fightInfo = Reply}),
									async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
									do_parse_packet(Socket,Client);
								Data ->
									%% 路由消息
									?DEBUG("recv Socket(~w) msg=~100000p",[Socket,Data]),
									route(Client, Data),
									async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
									do_parse_packet(Socket, Client)
							end;
						{inet_async, Socket, _Ref1, {error,closed}} ->
							do_lost(Socket, Client, {recv_close, BodyLen})
						after 300000 ->
								do_lost(Socket,Client,{client_no_response,BodyLen})
					end;
				false ->
					do_lost(Socket, Client,  "WPE encounted！！")
			end;
		{send_client, Msg} ->
			tk_misc:send_sock(Socket, Msg),
			do_parse_packet(Socket, Client);
		{emu, Msg} ->
			case Msg of
				{cs_account_demo,_,_,_} ->
					#cs_account_demo{type=Type,isMale=IsMale,specialID=SpecialID} =Msg,
					Reply = get_demo_fight(Type,IsMale,SpecialID),
					tk_misc:send_sock(Socket,#sc_account_demo{fightInfo = Reply}),
					do_parse_packet(Socket,Client);
				_ ->
					
					route(Client, Msg),
					?DEBUG("emu route=~100p",[Msg]),
					do_parse_packet(Socket, Client)
			end;
		
		%% 帐号在别的地方登录
		login_again ->
			tk_misc:send_sock(Socket, #sc_account_kick{reason=1}),
			do_lost(Socket, Client, login_again);
		%% 延迟收到的进程退出错误
		{'DOWN', _, process, _, _} ->
			do_parse_packet(Socket, Client);
		tick ->
			check_role_beat(Client),
			case check_timeout(Client) of
				true ->
					do_lost(Socket, Client, timeout);
				false ->
					tick(),
					do_parse_packet(Socket, Client)
			end;
		role_beat ->
			do_parse_packet(Socket,Client#client{role_beat=util:now()});
		%%用户断开连接或出错
		Other ->
			do_lost(Socket, Client,  Other)
	end.

check_role_beat(#client{role_beat=Beat,roleServer=Pid,socket=S,roleID=RoleID}) ->
	NowSec= util:now(),
	case NowSec - Beat > 20  of
		true ->
			?ERR("do_clean:~w",[RoleID]),
			catch erlang:send(Pid, {inet_reply, S,ok});
		_ ->
			ignore
	end.

tick() ->
	erlang:send_after(?TICK_INTERVAL, self(), tick).
	
%%断开连接
login_lost(Socket, Client, Cmd, Reason) ->
	case Reason of
		{inet_async,_,_,{error,closed}} ->
			?DEBUG("socket closed...during login reason=~100000p Cmd:~w",[Reason,Cmd]);
		timeout ->
			?DEBUG("socket closed...during login ..timeout",[]);
		_ ->
			?ERR("login lost..reason=~100000p,Socket:~w, Client:~w, Cmd:~w Pid:~w",[Reason,Socket, Client, Cmd, self()])
	end,
	%% 等写socket
	timer:sleep(200),
	exit({unexpected_message, Reason}).

%%退出游戏
do_lost(_Socket, _Client=#client{socket=S,roleServer=Pid}, login_again) ->
	%% 等写socket,重新登录时,不关闭逻辑进程
	timer:sleep(200),
	catch erlang:send(Pid, {inet_reply, S,ok}),
	exit(login_again);
do_lost(_Socket, Client=#client{socket=S,roleServer=Pid},  Reason) ->
	catch erlang:send(Pid, {inet_reply, S,ok}),
	case Reason of
		{_,_,_,{error,closed}} ->
			?DEBUG("socket closed...",[]);
		{'EXIT',_,normal} ->
			?DEBUG("socket closed...role_server shutdown",[]);
		timeout ->
			?DEBUG("socket closed...timeout",[]);
		_ ->
			?ERR("do lost...~100000p,roleServer=~w",[Reason,Client#client.roleServer])
	end,
	role_server:stop(Client#client.roleServer),
	%% 等写socket
	timer:sleep(200).

%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
	prim_inet:async_recv(Sock, Length, Timeout).
%% 		{error, Reason} -> throw({Reason});
%% 		{ok, Res}       -> Res;
%% 		Res             -> Res
%% 	end.

%% 检查密钥是否正确
%% Accid=1, UnixTime=1361000000, accountName="crimoon", ticket="1e5eaff16f066b8b97cb0296cf46aeee"
%% tk_misc:send_sock(S, #cs_account_login{userID=1,unixTime=1361000000,accountName="crimoon",ticket="1e5eaff16f066b8b97cb0296cf46aeee"}).
check_auth(#cs_account_login{userID=Accid,
							 unixTime=UnixTime,
							 accountName=_Accname,
							 ticket=Ticket,
                             serverID=ServerID
							}) ->
	NowSec = util:now(),
	%% UnixTime 是帐号服务器发给客户端的过期时间，此处判断ticket是否过期
	if NowSec > UnixTime  ->
		   {false,3};
	   true ->
		   Md5 = integer_to_list(Accid) ++ integer_to_list(UnixTime) ++ ?TICKET,
		   Hex = util:md5(Md5),		   
		   if Hex =:= Ticket ->
				  case gateway_ban:check_ban((ServerID + 1) * ?AccidBase + Accid) of
					  true ->
						  {false, 4};
					  false ->
						  true
				  end;
			  true ->
				  {false, 2}
		   end
	end.

%% 判断是否已经注册了主公信息
check_created(Accid) ->
	case db_sql:check_roleCreated(Accid) of
		{true, RoleID} ->
			case db_sql:check_ger_select(RoleID) of
				false ->
					{true,RoleID, false};
				true ->			
					{true, RoleID, true}
			end;
		false ->
			false 
	end.


%% 判断名字是否合法
check_roleName(RoleName) ->
	DecodeList = gen_utf8_decode_list(erlang:list_to_binary(RoleName), []),
	case check_blankName(DecodeList) of
		true ->
			Length = util:calc_name_length(DecodeList),
			if Length > 24 orelse Length < 1 ->
				   ?DEBUG("RoleName length:~w,name:~w,DecodeList:~w",[Length,RoleName,DecodeList]),
				   {false,5};
			   true ->
				   case db_sql:search_roleName(util:latin1(RoleName)) of
					   RoleID when is_integer(RoleID) ->
						   {false, 3};
					   _ ->
%% 						   ?ERR("RoleName:~s, DecodeList:~w, Length:~w", [RoleName, DecodeList, Length]),
%% 						   {false, 3}
						   %% 玩家不存在
						   ok
				   end
			end;
		_ ->
			{false, 4}
	end.

gen_utf8_decode_list(<<>>, AccDecodeList) ->
	lists:reverse(AccDecodeList);
gen_utf8_decode_list(<<0:1,X1:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,X5:1,
					   1:1,0:1,X6:1,X7:1,X8:1,X9:1,X10:1,X11:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,
					   1:1,0:1,X5:1,X6:1,X7:1,X8:1,X9:1,X10:1,
					   1:1,0:1,X11:1,X12:1,X13:1,X14:1,X15:1,X16:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1, 
					   1:1,0:1,X4:1,X5:1,X6:1,X7:1,X8:1,X9:1, 
					   1:1,0:1,X10:1,X11:1,X12:1,X13:1,X14:1,X15:1, 
					   1:1,0:1,X16:1,X17:1,X18:1,X19:1,X20:1,X21:1, 
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,0:1,X1:1,X2:1, 
					   1:1,0:1,X3:1,X4:1,X5:1,X6:1,X7:1,X8:1, 
					   1:1,0:1,X9:1,X10:1,X11:1,X12:1,X13:1,X14:1, 
					   1:1,0:1,X15:1,X16:1,X17:1,X18:1,X19:1,X20:1, 
					   1:1,0:1,X21:1,X22:1,X23:1,X24:1,X25:1,X26:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,1:1,0:1,X1:1, 
					   1:1,0:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1, 
					   1:1,0:1,X8:1,X9:1,X10:1,X11:1,X12:1,X13:1, 
					   1:1,0:1,X14:1,X15:1,X16:1,X17:1,X18:1,X19:1, 
					   1:1,0:1,X20:1,X21:1,X22:1,X23:1,X24:1,X25:1, 
					   1:1,0:1,X26:1,X27:1,X28:1,X29:1,X30:1,X31:1,
					   Left/binary>>, AccDecodeList) ->
	Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31]),
	gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(Binary, AccDecodeList) ->
	?ERR("非法的utf8编码，Binary：~w, AccDecodeList：~w", [Binary, AccDecodeList]),
	[].

get_val(List) ->
	{_, Val} = 
	lists:foldr(fun(Elem, {Count, AccVal}) ->
						{Count + 1, AccVal + math:pow(2, Count) * Elem}
	            end, {0, 0}, List),
	erlang:trunc(Val).
	
check_blankName([])->
	true;
check_blankName([H|T])->
	case (H >=48 andalso H =< 57) orelse (H >=65 andalso H =< 90) orelse (H >=97 andalso H =< 122) orelse (H >= 16#4e00 andalso H =< 16#9fa5) of
		false ->
			false;
		true ->
			check_blankName(T)
	end.

%% 创建角色流程
do_create_role(Client, Data) ->
	#client{accid=Accid, socket=Socket, deviceID=DeviceID, srcType=SrcType} = Client,
	#cs_account_create{roleName=RoleName, sex=Sex} = Data,

	case check_roleName(RoleName) of
		ok ->
			do_create_role2(RoleName,Sex,Socket,Accid,DeviceID,SrcType);
		{false, Type} ->
			?ERR("Invalid RoleName, Can't cteate,roleName=~1000p,type=~w, accid=~w",[RoleName, Type,Accid]),
			Reply = #sc_account_create{result=Type},
			tk_misc:send_sock(Socket, Reply),
			false
	end.

sex_to_bool(1) ->
	true;
sex_to_bool(2) ->
	false.

gen_roleID() ->
	%% 存下来是为了减小 角色名重复等允许的创角错误带来的roleID不连续情况
	case get(genRoleID) of
		RoleID when is_integer(RoleID) ->
			RoleID;
		_ ->
			RoleID = tk_id:gen_roleID(),
			put(genRoleID, RoleID),
			RoleID
	end.

do_create_role2(RoleName,Sex,Socket,Accid,DeviceID,SrcType) ->			
	%GerList = ?test_attacker_list,
	%OtherGer = ?test_all_ger,
	Now = util:now(),
	RoleID = gen_roleID(),
	tk_id:gen_roleID(),
	RoleInfo = #role{roleID= RoleID,
					 accid=Accid,
					 roleName=RoleName,
					 isMale=sex_to_bool(Sex),
					 gold=0,
					 coin=data_common:get(account_init_coin),
					 fightPower=0,
					 title=0,
					 familyID=0,
                     lastJoinFamily=0,
					 description="",
					 exp=0,
					 goldBonus=0,
					 goldTotalPaid=0,
					 goldUsed=0,
                     unioncoin=0,
					 lastLogoutTime=Now,
					 level=1,
					 reputation=0,
					 vipLevel=data_common:get(account_init_vipLevel),
                     payExtReward=role_lib:update_pay_ext_reward(0,0),
					 deviceID=DeviceID,
                     srcType=SrcType,
					 lastLoginTime=Now,
                     tasklevel=1,
                     transmigration=0
					},
	case db_sql:create_roleInfo(RoleInfo) of
		{ok, _} ->
			%?ERR("atomic"),
			Reply = #sc_account_create{result=1},
			tk_misc:send_sock(Socket, Reply),
			erase(genRoleID),
			behavior_create_role:log(Accid, RoleID, DeviceID, get_ip(Socket), Sex, 1),
			{true, RoleID};
		{error, Error} ->
			?ERR("create role error=~1000p\n with roleInfo=~1000p",[Error, RoleInfo]),
			Reply = #sc_account_create{result=3},
			tk_misc:send_sock(Socket, Reply),
			behavior_create_role:log(Accid, RoleID, DeviceID, get_ip(Socket), Sex, 3),
			false
	end.

%% 进入游戏
do_enter_game(Accid, RoleID, Client, Socket) ->
	#client{macAddr=MacAddr,deviceID=DeviceID} = Client,
	behavior_login_log:log(Accid, RoleID, DeviceID, get_ip(Socket)),
	{ok, RoleServerPid} = do_enter_game2(RoleID, Socket, MacAddr, Accid, DeviceID),
	async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	?DEBUG("enter game ok"),
	do_parse_packet(Socket, Client#client{roleServer = RoleServerPid}).
%?CATCH(do_parse_packet(Socket, Client#client{roleServer = RoleServerPid})).

do_enter_game2(RoleID, Socket, MacAddr, Accid, DeviceID) ->
	Ip = get_ip(Socket),
	
	case check_login_again(RoleID) of
		true ->
			case role_lib:pid(RoleID) of
				?undefined ->
					{ok, Pid} =role_server:start([RoleID, self(), Socket, MacAddr, Ip, DeviceID]);
				Pid ->
					post_login_info_to_platform(Accid),
					erlang:send(Pid, {login_again, self(), Socket, MacAddr, Ip, DeviceID})
			end;
		false ->
            case role_lib:pid(RoleID) of
                ?undefined ->
                    {ok, Pid} =role_server:start([RoleID, self(), Socket, MacAddr, Ip, DeviceID]);
                OldPid ->
                    Ref = erlang:monitor(process, OldPid),
                    role_server:stop(RoleID),
                    receive
                        {'DOWN', Ref, process, OldPid, _} ->
                            {ok, Pid} =role_server:start([RoleID, self(), Socket, MacAddr, Ip ,DeviceID])
                    after 2000 ->
                            Pid = ?undefined,
                            ?ERR("stop_role_server_timeout:RoleID:~w,Pid:~w", [RoleID,OldPid]),
                            erlang:exit(stop_role_server_timeout)
                    end
            end
    end,
	{ok, Pid}.

post_login_info_to_platform(Accid) ->
    case data_setting:get(account_server_url) of
        AccountServerUrl when erlang:is_list(AccountServerUrl)->
			%?ERR("url:~w",[AccountServerUrl++"/login_again?accid="++erlang:integer_to_list(Accid rem ?AccidBase)]),
			httpc:request(get,{AccountServerUrl++"/login_again?accid="++erlang:integer_to_list(Accid rem ?AccidBase),[]},[{timeout,2}],[]);
        _ ->
            ?ERR("get account_server_addr from config error", [])
    end.
post_create_process_to_platform(Accid, RoleID, Process) ->
    case data_setting:get(account_server_url) of
        AccountServerUrl when erlang:is_list(AccountServerUrl)->
			httpc:request(get,{AccountServerUrl++"/process_create?accid="++erlang:integer_to_list(Accid rem ?AccidBase)
							  ++"&roleid="++erlang:integer_to_list(RoleID)++"&process="++erlang:integer_to_list(Process),[]},[{timeout,2}],[]);
        _ ->
            ?ERR("get account_server_addr from config error", [])
    end.
%% post_login_info_to_platform(Accid) ->
%%     ?INFO("post_login_info_to_platform ~w",[Accid]),
%%     case catch gen_server:call({global, util:get_platform_server()}, {login_again, Accid rem ?AccidBase}) of
%%         ok ->
%%            ok;
%%         Error ->
%%             ?ERR("Error:~w", [Error])
%%     end.
%% 
%% post_create_process_to_platform(Accid, RoleID, Process)
%%   when erlang:is_integer(Accid), erlang:is_integer(RoleID), erlang:is_integer(Process), Process >=0, Process =< 2 ->
%%     ?INFO("post_create_process_to_platform ~w ~w ~w",[Accid, RoleID, Process]),
%%     case catch gen_server:call({global, util:get_platform_server()}, {process_create, Accid rem ?AccidBase, RoleID, Process}) of
%%         ok ->
%%             ok;
%%         Error ->
%%             ?ERR("Error:~w", [Error])
%%     end.
%% 清理进程字典中的缓存
%clear_dict() ->
%	erlang:erase(?roleInfo).

%% 消息路由
route(_Client, #cs_account_login{}) ->
	ignore;
route(_Client, #cs_version{}) ->
	ignore;
route(_Client, #cs_account_create{}) ->
	ignore;
route(Client, Data) ->
	MsgTag = element(1, Data),
	case proto_route:route(MsgTag) of
		{role, HandleModule} ->
			?DEBUG("handle module=~w",[HandleModule]),
			erlang:send(Client#client.roleServer, {client_msg, HandleModule, Data});
		{Server, _HandleModule} ->
			catch erlang:send(Server, {client_msg, Client#client.roleID, Data})
	end.


%% 检查重复登录
check_login_again(RoleID) ->
    RegName = role_lib:gatewayRegName(RoleID),
    case whereis(RegName) of
        ?undefined ->
            true = erlang:register(RegName, self()),
            false;
        Pid ->
            Ref = erlang:monitor(process, Pid),
            erlang:send(Pid, login_again),
            receive
                {'DOWN', Ref, process, Pid, _} ->
                    erlang:register(RegName, self()),
                    true
            after 2000 ->
                    erlang:exit(Pid, kill),
                    receive
                        {'DOWN', Ref, process, Pid, _} ->
                            erlang:register(RegName, self()),
                            true
                    after 2000 ->
                            ?ERR("exit_gw_timeout:RoleID:~w,Pid:~w", [RoleID,Pid]),
                            erlang:exit(exit_gw_timeout)
                    end
            end
    end.


get_ip(Socket) ->
	case inet:peername(Socket) of
		{ok, {Ip,_Port}} ->
			Ip;
		_ ->
			""
	end.

get_demo_fight(1,_,_)-> data_demo_fight:get(1);
get_demo_fight(2,IsMale,SpecialID) ->
	ID = 
	case {IsMale,SpecialID} of
		{1,1001} -> 2;
		{1,1002} -> 3;
		{1,1003} -> 4;
		{1,1004} -> 5;
		{_,1001} -> 6;
		{_,1002} -> 7;
		{_,1003} -> 8;
		{_,1004} -> 9;
		{_,_} -> ?ERR("get info:~w",[{IsMale,SpecialID}]), 2
	end,
	case data_demo_fight:get(ID) of
		?undefined -> data_demo_fight:get(2);
		X -> X
	end;
%get_demo_fight(2) -> data_demo_fight:get(2);
%% {sc_fight_request
%% ,[{p_fighter,5123,5123,1,700000,700000,0,0,100,100,20,0,0,0},{p_fighter,5013,5013,-1,140000,140000,0,0,0,100,1,0,0,0},{p_fighter,5043,5043,-2,140000,140000,0,0,0,100,1,0,0,0},{p_fighter,5133,5133,3,700000,700000,0,0,100,100,20,0,0,0},{p_fighter,5053,5053,-3,140000,140000,0,0,0,100,1,0,0,0},{p_fighter,5023,5023,4,700000,700000,0,0,100,100,20,0,0,0},{p_fighter,5113,5113,-4,80000,80000,0,0,0,100,1,0,0,0},{p_fighter,5063,5063,-5,130000,130000,0,0,0,100,1,0,0,0},{p_fighter,5103,5103,6,700000,700000,0,0,100,100,20,0,0,0},{p_fighter,5033,5033,-6,80000,80000,0,0,0,100,1,0,0,0}]
%% ,[{p_action,-3,4,[],0,0,0,0},{p_action,6,2,[],0,25185,0,0},{p_action,-3,65,[],0,-167903,0,24},{p_action,6,35,[-3],-100,0,0,0}
%%  ,{p_action,-5,4,[],0,0,0,0},{p_action,-1,4,[],0,0,0,0},{p_action,-2,4,[],0,0,0,0},{p_action,-5,68,[],25,-70504,0,16},{p_action,-1,68,[],0,-77383,0,16},{p_action,-2,68,[],0,-77383,0,16},{p_action,4,38,[-2,-1,-5],-100,0,0,0}
%%  ,{p_action,3,51,[],25,-27896,0,0},{p_action,-3,21,[3],50,0,0,0}
%%  ,{p_action,-4,4,[],0,0,0,0},{p_action,-6,4,[],0,0,0,0},{p_action,-1,67,[],25,-104287,0,64},{p_action,-2,67,[],25,-52143,0,0},{p_action,-3,67,[],25,-52143,0,0},{p_action,-4,67,[],25,-104287,0,80},{p_action,-5,67,[],25,-47368,0,0},{p_action,-6,67,[],25,-52143,0,16},{p_action,3,37,[-6,-5,-4,-3,-2,-1],-100,0,0,0}
%%  ,{p_action,1,56,[],25,-13563,0,0},{p_action,3,56,[],0,-13563,0,0},{p_action,-2,26,[3,1],50,0,0,0}
%%  ,{p_action,-1,2,[],0,7604,0,0}
%%  ,{p_action,4,55,[],0,-25347,0,8},{p_action,6,55,[],0,-25347,0,8},{p_action,-1,25,[6,4],50,0,0,0},{p_action,-1,66,[],25,-34563,0,0},{p_action,-2,66,[],25,-69126,0,64},{p_action,-3,66,[],25,-34563,0,0},{p_action,-4,66,[],25,-69126,0,64},{p_action,-5,66,[],25,-31490,0,0},{p_action,-6,66,[],25,-34563,0,0},{p_action,1,36,[-6,-5,-4,-3,-2,-1],-100,0,0,0}
%%  ,{p_action,0,10,[],0,0,0,0},{p_action,0,201,[],0,0,0,0}]
%% ,true};
get_demo_fight(_,_,_) -> 
{sc_fight_double_request,[],[],false}.
