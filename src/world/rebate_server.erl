%% @author shujunjie
%% @doc 压岁钱
%% Created 2014.01.14

-module(rebate_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================

-define(check_interval, 30).	%% 检查状态间隔，单位：秒
-define(dump_interval, 60*2).	%% 保存数据间隔，单位：秒

%% 服务器信息
-record(rebate_status,{lastActivity		%% 上次活动ID
					   ,lastRebate		%% 上次返利ID
					  }).

-define(status, status).		% 服务器状态 d_rebate_status
-define(roleInfo, roleInfo).	% 玩家信息 [ {roleid, [rebate_info,rebate_info]}, ]

%% ===================Dict Key End   =========================


get_rebate_list(RoleID)->
	gen_server:call(?MODULE, {get_rebate_list, RoleID}).
get_roleinfo(RoleID)->
	gen_server:call(?MODULE, {get_roleinfo, RoleID}).
set_roleinfo(RoleID, Info)->
	gen_server:call(?MODULE, {set_roleinfo, RoleID, Info}).
get_reward(RebateID, RebateInfo)->
	gen_server:call(?MODULE, {get_reward, RebateID, RebateInfo}).

%% =========================================================================================

init_status()->
	ActivityID = get_rebate_activity(),
	RebateID = get_enabled_rebate_id(),
%% 	case ActivityID of
%% 		?undefined->
%% 			set_rebate_roleinfo([]);
%% 		_->
%% 			ignore
%% 	end,
	set_status(#rebate_status{lastActivity=ActivityID,lastRebate=RebateID}).
get_status()->
	case erlang:get(?status) of
		Status when is_record(Status, rebate_status) ->
			Status;
		_ ->
			#rebate_status{lastActivity=?undefined,lastRebate=?undefined}
	end.
set_status(Status)->
	erlang:put(?status, Status).

%% 玩家信息  [ {roleid, [rebate_info,rebate_info]}, ]
init_rebate_roleinfo()->
	RebateRoleInfo = db_sql:get_etc(?DB_ETC_KEY_REBATE_ROLEINFO),
	set_rebate_roleinfo(RebateRoleInfo).
get_rebate_roleinfo()->
	case erlang:get(?roleInfo) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.
get_rebate_roleinfo(RoleID)->
	RebateRoleInfo = get_rebate_roleinfo(),
	case lists:keyfind(RoleID, 1, RebateRoleInfo) of
		false->
			[];
		{RoleID, X} ->
			X
	end.
set_rebate_roleinfo(RebateRoleInfo)->
	erlang:put(?roleInfo, RebateRoleInfo).
set_rebate_roleinfo(RoleID, RoleInfo)->
	RebateRoleInfo = get_rebate_roleinfo(),
	NewRebateRoleInfo = 
		case lists:keyfind(RoleID, 1, RebateRoleInfo) of
			false->
				[ {RoleID, RoleInfo} | RebateRoleInfo];
			_->
				RebateRoleInfo2 = lists:keydelete(RoleID, 1, RebateRoleInfo),
				[ {RoleID, RoleInfo} | RebateRoleInfo2]
		end,
	set_rebate_roleinfo(NewRebateRoleInfo).
persist_rebate_roleinfo()->
	RebateRoleInfo = get_rebate_roleinfo(),
	db_sql:set_etc(?DB_ETC_KEY_REBATE_ROLEINFO, RebateRoleInfo).

%% =========================================================================================

start() ->
	{ok,_}=supervisor:start_child(world_sup, 
								  {?MODULE,
								   {?MODULE, start_link, []},
								   permanent, 600000, worker, [?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	init_rebate_roleinfo(),
	init_status(),
	check_tick(),
	dump_tick(),
	{ok, ?undefined}.

handle_call({get_rebate_list, RoleID}, _From, State) ->
	RebateInfo = get_rebate_roleinfo(RoleID),
	#data_activity_rebate{drawList=DrawList, startTime=BeginTime_} = get_rebate_activity_info(),
	BeginTime = util:datetime_to_seconds(BeginTime_),
	RebateList = [ #p_rebate_list{rebateID=RebateID,name=Name,
								  status=get_rebate_status(RebateDraw, RebateInfo),
								  startTime=BeginTime+relative_time(StartTime),
								  closeTime=BeginTime+relative_time(CloseTime),
								  rewardTime=BeginTime+relative_time(RewardTime),
								  rebateInfo=get_rebate_info(RebateDraw, RebateInfo)
								 } 
								 || #data_rebate_draw{rebateID=RebateID,name=Name,startTime=StartTime,closeTime=CloseTime,rewardTime=RewardTime}=RebateDraw<-DrawList],
	{reply, RebateList, State};
handle_call({get_roleinfo, RoleID}, _From, State) ->
	Info = get_rebate_roleinfo(RoleID),
	{reply, Info, State};
handle_call({set_roleinfo, RoleID, Info}, _From, State) ->
	set_rebate_roleinfo(RoleID, Info),
	{reply, State, State};
handle_call({get_reward, RebateID, RebateInfo}, _From, State) ->
	RebateDraw = 
		case get_rebate_activity() of
			?undefined->
				?undefined;
			ActivityID->
				get_rebate_draw(ActivityID, RebateID)
		end,
	Result = 
		case RebateDraw of
			#data_rebate_draw{ratioList=RatioList}->
				case get_rebate_status(RebateDraw, RebateInfo) of
					3->
						%% 可以领取奖励
						case lists:keyfind(RebateID, #rebate_info.id, RebateInfo) of
							false->
								{2, []};
							#rebate_info{amount=Amount}->
								Coin = get_amount_reward(1, Amount, RatioList),
								Gold = get_amount_reward(2, Amount, RatioList),
								Reputation = get_amount_reward(3, Amount, RatioList),
								case Coin==0 andalso Gold==0 andalso Reputation==0 of
									true->
										{3, []};
									false->
										{1, #p_rebate_reward{coin=Coin, gold=Gold, reputation=Reputation}}
								end
						end;
					_->
						{2, []}
				end;
			?undefined->
				{2, []}
		end,
	{reply, Result, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
    
handle_info(Info, State) ->
	?CATCH(do_handle_info(State, Info)),
	{noreply, State}.

terminate(Reason, State) ->
	persist_rebate_roleinfo(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

do_handle_info(_State, check_tick) ->
	erlang:erase(),
	ok;

%% do_handle_info(_State, check_tick)->
%% 	check_tick(),
%% 	ActivityID = get_rebate_activity(),
%% 	RebateID = get_enabled_rebate_id(),
%% 	#rebate_status{lastActivity=LastActivityID,lastRebate=LastRebateID} = get_status(),
%% 	case LastActivityID=/=ActivityID of
%% 		true->
%% 			case LastActivityID of
%% 				?undefined->
%% 					%% 清除运行信息
%% 					set_rebate_roleinfo([]);
%% 				_->
%% 					rebate_close(LastActivityID)
%% 			end,
%% 			send_everyone_change_state();
%% 		false->
%% 			case LastRebateID=/=RebateID of
%% 				true->
%% 					send_everyone_change_state();
%% 				false->
%% 					ignore
%% 			end
%% 	end,
%% 	set_status(#rebate_status{lastActivity=ActivityID,lastRebate=RebateID});
do_handle_info(_State, dump_tick)->
	dump_tick(),
	persist_rebate_roleinfo();
do_handle_info(State, Info) ->
	?ERR("do_handle_info function clause:request=~100p,state=~100p",[Info, State]).

check_tick()->
	erlang:send_after(?check_interval * 1000, self(), check_tick).

dump_tick()->
	erlang:send_after(?dump_interval * 1000, self(), dump_tick).

send_everyone_change_state()->
	role_lib:send_every_server_msg({route, role_activity, #cs_activity_rebate_info{}}).

%% 相对时间转换
relative_time({Day,Hour,Minute,Second})->
	Day*24*60*60 + Hour*60*60 + Minute*60 + Second.

%% 获取当前开启的返利活动ID，没有活动返回?undefined
get_rebate_activity()->
	Now = util:now(),
	Activity = lists:filter(fun(ActivityID)->
									#data_activity_rebate{startTime=StartTime,closeTime=CloseTime} = data_rebate:get(ActivityID),
									BeginTime = util:datetime_to_seconds(StartTime),
									StopTime =BeginTime + relative_time(CloseTime),
									Now>=BeginTime andalso Now=<StopTime
							end, data_rebate:get_list()),
	case Activity of
		[ActivityID]->
			ActivityID;
		_->
			?undefined
	end.

%% 获取当前开启的返利活动信息，没有活动返回?undefined
get_rebate_activity_info()->
	case get_rebate_activity() of
		?undefined->
			?undefined;
		ActivityID->
			data_rebate:get(ActivityID)
	end.

%% 获取当前正在生效的返利ID，没有返利返回?undefined
get_enabled_rebate_id()->
	NowSec = util:now(),
	case get_rebate_activity_info() of
		?undefined->
			?undefined;
		#data_activity_rebate{startTime=ActivityStartTime, drawList=RebateList} ->
			BeginTime = util:datetime_to_seconds(ActivityStartTime),
			Rebate = lists:filter(fun(#data_rebate_draw{startTime=StartTime,closeTime=CloseTime})->
										  RebateStartTime = BeginTime + relative_time(StartTime),
										  RebateCloseTime = BeginTime + relative_time(CloseTime),
										  NowSec>=RebateStartTime andalso NowSec=<RebateCloseTime
								  end, RebateList),
			case Rebate of
				[#data_rebate_draw{rebateID=RebateID}]->
					RebateID;
				_->
					?undefined
			end
	end.

%%	RebateInfo = [ #rebate_info{id=1,amount=[1,2,3], get=true}, ]
get_reward_status(RebateID, RebateInfo)->
	case lists:keyfind(RebateID, #rebate_info.id, RebateInfo) of
		false->
			5;
		#rebate_info{amount=Amount, get=Get}->
			case Get of
				true->
					4; %%已经领取奖励
				false->
					case Amount of
						[0,0,0]->
							5;
						_->
							3 %%可以领取奖励
					end
			end
	end.

get_rebate_status(#data_rebate_draw{rebateID=RebateID, startTime=StartTime_, closeTime=CloseTime_, rewardTime=RewardTime_}, RebateInfo)->
	#data_activity_rebate{startTime=BeginTime_} = get_rebate_activity_info(),
	BeginTime = util:datetime_to_seconds(BeginTime_),
	NowSec = util:now(),
	StartTime = BeginTime + relative_time(StartTime_),
	CloseTime = BeginTime + relative_time(CloseTime_),
	RewardTime = BeginTime + relative_time(RewardTime_),
	case NowSec>StartTime of
		true->
			%已经开始
			case NowSec>CloseTime of
				true->
					%已经结束统计
					case NowSec>RewardTime of
						true->
							%开始领奖
							get_reward_status(RebateID, RebateInfo);
						false->
							2
					end;
				false->
					1
			end;
		false->
			%未开始
			0
	end.

%%	RebateInfo = [ #rebate_info{id=0,amount=[100,200,300], get=true}, #rebate_info{id=1,amount=[100,200,300], get=false}]
get_rebate_info(#data_rebate_draw{rebateID=RebateID,ratioList=RatioList}, RebateInfo)->
	case lists:keyfind(RebateID, #rebate_info.id, RebateInfo) of
		false->
			lists:foldr(fun(Type, ACC)->
								[ #p_rebate_info{type=Type,ratio=lists:nth(Type, RatioList),amount=0} | ACC]
						end, [], lists:seq(1, length(RatioList)));
		#rebate_info{amount=Amount}->
			lists:foldr(fun(Type, ACC)->
								[ #p_rebate_info{type=Type,ratio=lists:nth(Type, RatioList),amount=lists:nth(Type, Amount)} | ACC]
						end, [], lists:seq(1, length(RatioList)))
	end.

get_rebate_draw(ActivityID, RebateID)->
	#data_activity_rebate{drawList=DrawList} = data_rebate:get(ActivityID),
	case lists:keyfind(RebateID, #data_rebate_draw.rebateID, DrawList) of
		false->
			?undefined;
		X->
			X
	end.

get_amount_reward(Type, Amount, RatioList)->
	trunc( lists:nth(Type, Amount) * lists:nth(Type, RatioList) / 100 ).

rebate_close(ActivityID) ->
	%% 未领奖的用户，把奖励通过邮件发送出去
	RebateInfo = get_rebate_roleinfo(),
	lists:foreach(fun({RoleID, Info})->
						  {Coin, Gold, Reputation} = lists:foldl(fun(#rebate_info{id=RebateID,amount=Amount, get=Get},{Coin, Gold, Reputation}=Acc)->
																		 case Get of
																			 false->
																				 RebateDraw = get_rebate_draw(ActivityID, RebateID),
																				 RatioList = RebateDraw#data_rebate_draw.ratioList,
																				 CoinDelta = get_amount_reward(1, Amount, RatioList),
																				 GoldDelta = get_amount_reward(2, Amount, RatioList),
																				 ReputationDelta = get_amount_reward(3, Amount, RatioList),
																				 {Coin+CoinDelta, Gold+GoldDelta, Reputation+ReputationDelta};
																			 true->
																				 Acc
																		 end
																 end, {0,0,0}, Info),
						  if Coin>0 orelse Gold>0 orelse Reputation>0 ->
								 Reward = {sell_reward,Coin,0,0,Gold,[],Reputation,[]},
								 mail_server:send_sys_mail(RoleID, ?MAIL_REBATE_REWARD, [Coin, Gold], "", Reward);
							 true->
								 ignore
						  end
				  end, RebateInfo).
