%% @author caohongyang
%% @doc 推送服务进程
%% Created 2013-5-6

%% 等待优化：看一次socket连接，能否发送两个推送请求

-module(push_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_push.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).

-export([update_token/2, push/2, get_db_push/1]).

update_token(RoleID, Token) ->
	erlang:send(?MODULE, {update_token, RoleID,Token}).

push(RoleID, MsgID) ->
	case role_lib:is_online(RoleID) of
		true ->
			ignore;
		false ->
			erlang:send(?MODULE, {push, RoleID, MsgID})
	end.

i() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	gen_server:call(?MODULE, i).


start() ->
	{ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([]) ->
	process_flag(trap_exit,true),
    {ok, []}.


-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% @doc gen_server:init/1
handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.


-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_cast/2
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_info/2

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
    
handle_info({update_token, RoleID, Token}, State) ->
	PushInfo = get_push(RoleID),
	case PushInfo#d_push.token =:= Token of
		false ->
			PushInfo2 = PushInfo#d_push{token=Token},
			set_push(RoleID, PushInfo2),
			persist_push(RoleID, PushInfo2);
		true ->
			ignore
	end,
	{noreply, State};
handle_info({client_msg,RoleID, #cs_role_push_setting{type=Type,value=Value}}, State) ->
	?INFO("push...~w...~w",[RoleID,{Type,Value}]),
	role_setting(RoleID, Type,Value),
	{noreply, State};
handle_info({push, RoleID, MsgID}, State) ->
	do_push(RoleID, MsgID),
	{noreply, State};
handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.


-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[broadcast_server, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.



-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
get_db_push(RoleID) ->
	case db_sql:get_pushInfo(RoleID) of
		#d_push{}=PushInfo ->
			PushInfo;
		_ ->
			#d_push{isPVPPushOpen=true,isPushNightMute=true,roleID=RoleID,token= <<>>}
			end.

persist_push(RoleID, PushInfo) ->
	db_sql:set_pushInfo(RoleID, PushInfo).

get_push(RoleID) ->
	case get(RoleID) of
		?undefined ->
			PushInfo=get_db_push(RoleID),
			set_push(RoleID, PushInfo),
			PushInfo;
		#d_push{}=PushInfo ->
			PushInfo
	end.
			
set_push(RoleID, PushInfo) ->
	put(RoleID, PushInfo).

do_push(RoleID,MsgID) ->
	case get_push(RoleID) of
		?undefined ->
			ignore;
		#d_push{isPVPPushOpen=IsPVPPushOpen,isPushNightMute=IsNightMute,token=DeviceToken }->
			%% 判断是否有DeviceToken，且是否开启了争霸推送
			if is_binary(DeviceToken) 
				 andalso DeviceToken =/= <<>> 
				 andalso (MsgID =/= ?PVP_PUSH_MSG orelse IsPVPPushOpen == true) ->
				 Time = erlang:time(),
				 %%　判断是否开启了晚上免打扰
				 if IsNightMute == false orelse (Time  =< {23,0,0} orelse Time >= {8,0,0}) ->
						do_push2(DeviceToken, msg(MsgID));
					true ->
						ignore
				 end;
			   true ->
				   ignore
			end
	end.

do_push2(DeviceToken, Msg) ->
	%% 测试地址: "ssl://gateway.sandbox.push.apple.com:2195"
	%% 正式地址: "ssl://gateway.push.apple.com:2195"	
	{ok, Socket} = ssl:connect("ssl://gateway.push.apple.com:2195", 
							   [{certfile,"config/app/PushChatCert.pem"},
								{password,"abcd"}]),
	PayLoad = ejson:encode({[{<<"alert">>,Msg},{<<"sound">>,<<"default">>}]}),
	Request = <<0,0,32, DeviceToken/binary, (byte_size(PayLoad)):16/integer, PayLoad/binary>>,
	ssl:send(Socket, Request).

%% 消息列表
msg(?PVP_PUSH_MSG) ->
	"oksdfwefsf";
msg(1002) ->
	"sdfwefdf".

-define(SETTING_PVP_PUSH, 1).
-define(SETTING_PUSH_NONIGHT,2).

to_bool(1) ->true;
to_bool(_) -> false.

role_setting(RoleID, ?SETTING_PVP_PUSH=Type, Value) ->
	Flag = to_bool(Value),
	#d_push{isPVPPushOpen=IsPVPPushOpen} = PushInfo = get_push(RoleID),
	if IsPVPPushOpen == Flag ->
		   ignore;
	   true ->
		   PushInfo2 = PushInfo#d_push{isPVPPushOpen=Flag},
		   set_push(RoleID, PushInfo2),
		   persist_push(RoleID, PushInfo2)
	end,
	?unicast(RoleID, #sc_role_push_setting{result=1,type=Type,value=Value});
role_setting(RoleID, ?SETTING_PUSH_NONIGHT=Type, Value) ->
	Flag = to_bool(Value),
	#d_push{isPushNightMute=IsPVPPushOpen} = PushInfo = get_push(RoleID),
	if IsPVPPushOpen == Flag ->
		   ignore;
	   true ->
		   PushInfo2 = PushInfo#d_push{isPushNightMute=Flag},
		   set_push(RoleID, PushInfo2),
		   persist_push(RoleID, PushInfo2)
	end,
	?unicast(RoleID, #sc_role_push_setting{result=1,type=Type,value=Value}).