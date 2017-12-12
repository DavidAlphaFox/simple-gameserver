%% @author crimoon26
%% @doc @todo Add description to etc_server.


-module(etc_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,start_link/0,role_offline/2,role_online/1]).


-record(etc_info,{roleID,devid,type}).

-define(ETC_DATA_CHANGE_FLAG,etc_data_change_flag).

-define(PUSH_HOUR_1,13).
-define(PUSH_HOUR_2,19).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).




start() ->
	{ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

role_online(RoleID)->
	erlang:send(?MODULE, {role_online,RoleID}).

role_offline(RoleID,DeviceID)->
	{Hour,_,_} = time(),
	case Hour>=?PUSH_HOUR_2 of
		true->
			ignore;
		false->
			case role_daily:can_draw_loginreward() of
				false->
					ignore;
				{true,LoginDay}->
					case LoginDay of
						2->
							erlang:send(?MODULE, {role_offline,RoleID,DeviceID,1});
						3->
							erlang:send(?MODULE, {role_offline,RoleID,DeviceID,2});
						_->
							ignore
					end
			end
	end.
	
init([]) ->
	init_data(),
	%% 开始时间轮
	timer_wheel:init(),
	{Hour,_,_}  = time(),
	NextPushSec = util:datetime_to_seconds({erlang:date(),{Hour,0,1}}) + ?ONE_HOUR_SECONDS,
	timer_wheel:add_plan(NextPushSec, fun hook_hour/0),
	NextDumpSec = util:now() + 120,
	timer_wheel:add_plan(NextDumpSec, fun persist/0),
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
    
handle_info(Info, State) ->
	do_handle_info(Info),
    {noreply, State}.

terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\n",[pvp_server, Reason,  State]), 
	do_persist(),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info({timer_wheel_tick, LastTick}) ->
	timer_wheel:work(LastTick);
do_handle_info({role_online,RoleID})->
	erlang:erase(RoleID),
	erlang:put(?ETC_DATA_CHANGE_FLAG,true);
do_handle_info({role_offline,RoleID,DeviceID,Type})->
	EtcInfo = #etc_info{roleID=RoleID,devid=DeviceID,type=Type},
	erlang:put(RoleID, EtcInfo),
	erlang:put(?ETC_DATA_CHANGE_FLAG,true);
do_handle_info(debug_push)->
	send_push(false);
do_handle_info(debug_push_1)->
	hook_hour();
do_handle_info(_Info)->
	?ERR("无效消息:~p",[_Info]).

init_data()->
	List = db_sql:get_etc(?DB_ETC_KEY_PUSH),
	lists:foreach(fun(EtcInfo)->
						  #etc_info{roleID=RoleID} = EtcInfo,
						  erlang:put(RoleID,EtcInfo)
					end,List).
persist()->
	NextDumpSec = util:now() + 120,
	timer_wheel:add_plan(NextDumpSec, fun persist/0),
	do_persist().

do_persist() ->
	case erlang:erase(?ETC_DATA_CHANGE_FLAG) of
		true->
			List = lists:foldl(fun({_,EtcInfo}, Acc) when is_record(EtcInfo,etc_info) ->
									   [EtcInfo|Acc];
								  (_,Acc) ->
									   Acc
							   end, [], erlang:get()),
			db_sql:set_etc(?DB_ETC_KEY_PUSH, List);
		_->
			ignore
	end.

send_push(Remove)->
	{DList1,DList2} = lists:foldl(fun({_,#etc_info{roleID=RoleID,devid=Devid,type=1}},{Acc1,Acc2}) ->
										  case Remove of
											  true->erlang:erase(RoleID);
											  _->ignore
										  end,
										  {[[Devid]|Acc1],Acc2};
									 ({_,#etc_info{roleID=RoleID,devid=Devid,type=2}},{Acc1,Acc2})->
										  case Remove of
											  true->erlang:erase(RoleID);
											  _->ignore
										  end,
										  {Acc1,[[Devid]|Acc2]};
									 (_,Acc) ->
										  Acc
								  end, {[],[]},erlang:get()),
	case DList1 of
		[]->
			ignore;
		_->
			spawn(fun()->send_push(1,DList1) end)
	end,
	case DList2 of
		[]->
			ignore;
		_->
			spawn(fun()->send_push(2,DList2) end)
	end.

send_push(_,_) ->
    ok.
%% send_push(Type,DevIDList)->
%% 	case data_push:get(Type) of
%% 		undefined->
%% 			ignore;
%% 		Msg->
%% 			Key = util:md5(data_setting:get(push_key)++Msg),
%% 			%%http://10.10.11.211:28082/recvpush?isall=~p&msg=~s&devidlist=~w&key=~s
%% 			Url = data_setting:get(push_url),
%% %% 			RequestData = ejson:encode([{[{<<"isall">>,<<"false">>}]},{[{<<"msg">>,base64:encode(Msg)}]},{[{<<"devidlist">>,ejson:encode(DevIDList)}]},{[{<<"key">>,list_to_binary(Key)}]}]),
%% 			RequestData = "isall=false&msg="++Msg++"&devidlist="++binary_to_list(ejson:encode(DevIDList))++"&key="++Key,
%% 			httpc:request(post, {Url,[], "application/x-www-form-urlencoded",RequestData}, [{timeout, 5000}], [])
%% 	end.


%%每个小时执行
hook_hour()->
	{Hour,_,_} = time(),
	NextPushSec = util:datetime_to_seconds({erlang:date(),{Hour,0,1}}) + ?ONE_HOUR_SECONDS,
	timer_wheel:add_plan(NextPushSec, fun hook_hour/0),
	case Hour of
		?PUSH_HOUR_1->
			send_push(false);
		?PUSH_HOUR_2->
			send_push(true);
		_->
			ignore
	end.
