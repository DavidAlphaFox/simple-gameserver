%% @author caohongyang
%% @doc 活动
%% Created 2013-6-8

%% buffer 说明：每次玩家读取活动数据时，将数据加载到内存，每天0晨hook时，将不在线玩家的缓存数据刷出内存。玩家数据更新，更新内存的同时，更新数据库数据，保证数据完整。

-module(activity_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).

-export([pay/2,role_energy_activity/2]).

-define(TICK_INTERVAL, 3600). % 检查进程buff大小的时间间隔，单位：秒
-define(TICK_STATE_INTERVAL, 10). % 检查活动开启状态的时间间隔，单位：秒
-define(TICK_CHECK_ENERTY_TIME, 10). % 检查回复体力活动的时间间隔，单位：秒
-define(get_energy_state, get_energy_state). % 是否可以获取体力标记
-define(energy_activity_info, energy_activity_info). % 领取体力时间状态
-define(energy_switch, energy_switch). % tag
-define(FOREVER, -1).% 活动永久有效
-define(box_price_info,box_price_info). % 点将价格信息

-define(REFRESH_CONFIG_PLAN_PLAG,refresh_config_plan_plag).
-define(last_activity_config,last_activity_config).

%%领取体力的四个时间点
-define(ENERGY_FIRST_TIME_PERIOD,1).
-define(ENERGY_SECOND_TIME_PERIOD,2).
-define(ENERGY_THIRD_TIME_PERIOD,3).
-define(ENERGY_FORTH_TIME_PERIOD,4).

% 某些功能，会受到活动的影响，需要获取当前活动状态，用ets来证明
-define(ets_cache_cur_activity,ets_cache_cur_activity). %缓存当前的活动内容，缓存数据，减少进程间通讯。无需保存数据持久化
% 需要记录的活动标志列表如下，会放入上面的缓存中
-define(cache_filter_list,[?carlos_honor_double
                          ,?pvp_reward_double
                          ,?hron_reward_double
                          ,?relic_box_double
                          ,?homestead_reward_double
                          ,?world_boss_box_double]).

%% ===================Dict Key Begin =========================
%% ===================Dict Key End   =========================

role_energy_activity(RoleID,EnergyStep)->
	erlang:send(?MODULE, {role_get_energy, RoleID,EnergyStep}).

finish_exchange_condition(RoleID, ActivityID, DrawID)->
	erlang:send(?MODULE, {finish_exchange_condition, RoleID, ActivityID, DrawID}).

refresh_daily_activity(RoleID)->
	erlang:send(?MODULE, {refresh_daily_activity, RoleID}).
  
pay(RoleID, Gold) ->
	erlang:send(?MODULE,{pay, RoleID, Gold}).

role_consume(Type, RoleID, ConsumeValue)->
	erlang:send(?MODULE, {role_consume, Type, RoleID, ConsumeValue}).

is_activity(Type)->
    ets:member(?ets_cache_cur_activity, Type).

i() ->
	gen_server:call(?MODULE, i).

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
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
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	tick(),
    
    %读取固化在数据库中的体力领取信息
    CurDate = erlang:date(),
    case db_sql:get_etc(?DB_ETC_KEY_ENERGY_ROLELIST) of
        {Date,EnergyRolelist} when CurDate =:= Date->
            lists:foreach(fun(EnergyRole)-> 
                            ets:insert(?ETS_ENERGY_ROLELIST, EnergyRole)
                        end, EnergyRolelist);
        _ ->
            ignore
    end,
    
    ets:new(?ets_cache_cur_activity, [set,public, {keypos,1},named_table]),
	State = init_state(),
	tick_state(),
	timer_wheel:init(),
	put(?REFRESH_CONFIG_PLAN_PLAG,true),
	NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
	timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
	tick_energy_time(),
    {ok, State}.


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
handle_info(tick, State) ->
	{memory,Memory }= erlang:process_info(self(),memory),
	MemoryByM = Memory div (1024*1024),
	MaxBuffSizeByM = data_setting:get(friend_process_max_buff_size),
	if MemoryByM >= MaxBuffSizeByM ->
		   clear_buff();
	   true ->
		   ignore
	end,
	erlang:garbage_collect(),
	tick(),
	{noreply, State, hibernate};
handle_info(tick_state, _State) ->
	NewState = init_state(),
	tick_state(),
	refresh_config(),
	{noreply,NewState};
handle_info(tick_energy_time, State)->

	tick_energy_time(),
	{noreply, State};
handle_info({timer_wheel_tick, LastTick}, State) ->
	timer_wheel:work(LastTick),
	{noreply, State};
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
    
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info, State)),
    {noreply, State}.



-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
    %保存当前时间，用于下次服务器启动读取时，判断是否隔日
    db_sql:set_etc(?DB_ETC_KEY_ENERGY_ROLELIST, {erlang:date(),ets:tab2list(?ETS_ENERGY_ROLELIST)}),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
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

%%此函数废弃 替换为get_list
%%do_handle_info({client_msg, RoleID, #cs_activity_get_list{}}, State) ->
%%	do_get_list(RoleID, State);
%%此函数废弃,替换为get_info
%%do_handle_info({client_msg, RoleID, #cs_activity_info{activityID=ID}}, _State) ->
%%	do_info(RoleID, ID);
%%此函数废弃,替换为activity_draw
%%do_handle_info({client_msg, RoleID, #cs_activity_draw{activityID=ActivityID,drawID=DrawID}}, _State) ->
%%	do_draw(RoleID, ActivityID, DrawID);

do_handle_info({get_list,RoleID,RoleLevel,VipLevel}, State) ->
	do_get_list(RoleID, RoleLevel, VipLevel, State);
do_handle_info({get_info,RoleID,ID}, _State) ->
	do_info(RoleID,ID);
do_handle_info({activity_draw,RoleID,ActivityID,DrawID,VipLevel,RoleLevel,Chose}, _State) ->
	do_draw(RoleID, ActivityID, DrawID,VipLevel,RoleLevel,Chose);

do_handle_info({pay, RoleID, Gold}, State) ->
    redpacket_server:creat_redbox(RoleID, Gold),
	do_pay(RoleID, Gold,  State);
do_handle_info({finish_exchange_condition, RoleID, ActivityID, DrawID}, _State) ->
	do_finish_exchange_condition(RoleID, ActivityID, DrawID);
do_handle_info({role_get_energy, RoleID,EnergyStep}, _State) ->
	role_get_energy(RoleID,EnergyStep);
do_handle_info({refresh_daily_activity, RoleID}, _State) ->
	daily_fresh(RoleID);
do_handle_info({client_msg, RoleID, #cs_activity_energy{}}, _State) ->
	activity_energy_info(RoleID);
do_handle_info({clear_energy_state}, _State)->
	hook_zero_clock();
do_handle_info({role_consume, Type, RoleID, ConsumeValue}, State)->
	do_role_consume(Type, RoleID, ConsumeValue, State);
do_handle_info(Info, State) ->
	throw({cannot_handle,Info, State}).

activity_energy_info(RoleID)->
	{#p_energy_activity{endTime=_End1}=P1, P2, P3, P4} = erlang:get(?energy_activity_info),
	IsGet1 = case is_role_get_energy(RoleID,1) of
				 true->
					 2;
				 false ->
					 1
			 end,
	IsGet2 = case is_role_get_energy(RoleID,2) of
				 true ->
					 2;
				 false ->
					 1
			 end,
    IsGet3 = case is_role_get_energy(RoleID,3) of
                 true ->
                     2;
                 false ->
                     1
             end,
    IsGet4 = case is_role_get_energy(RoleID,4) of
                true ->
                    2;
                _ ->
                    1
            end,
%% 	Now = util:now(),     
%% 	%?ERR("time:~w,~w",[Now, End1]),
%% 	if Now < End1 ->
%% 		   ?unicast(RoleID, #sc_activity_energy{activityList=[P1#p_energy_activity{isGet = IsGet1}, P2#p_energy_activity{isGet = IsGet2}]});
%% 	   true ->
%% 		   ?unicast(RoleID, #sc_activity_energy{activityList=[P1#p_energy_activity{isGet = IsGet2}, P2#p_energy_activity{isGet = IsGet1}]})
%% 	end.
    ?unicast(RoleID, #sc_activity_energy{activityList=[P1#p_energy_activity{isGet = IsGet1}
                                                      ,P2#p_energy_activity{isGet = IsGet2}
                                                      ,P3#p_energy_activity{isGet = IsGet3}
                                                      ,P4#p_energy_activity{isGet = IsGet4}]}).


%%v2.9.0修改为任何时间点都能够领取同一天内没有领取到的体力
role_get_energy(RoleID,EnergyStep)->	
	case check_enegy_step_illegal(EnergyStep) of
		{false,Reason}->
			?unicast(RoleID,#sc_role_get_energy{result=Reason});
		{Type, Eng} ->
			case is_role_get_energy(RoleID,Type) of
				true ->
					?unicast(RoleID,#sc_role_get_energy{result=3});
				false ->
					set_role_get_energy(RoleID,Type),
					role_lib:send_server(RoleID, {role_energy_activity, Eng}),
					?unicast(RoleID, #sc_role_get_energy{result=1})
			end
	end.

check_enegy_step_illegal(Step)->
	Now = util:now(),
	Result = case Step of
		?ENERGY_FIRST_TIME_PERIOD->
			data_get_energy:get(energy_time1);
		?ENERGY_SECOND_TIME_PERIOD->
			data_get_energy:get(energy_time2);
		?ENERGY_THIRD_TIME_PERIOD->
			data_get_energy:get(energy_time3);
		?ENERGY_FORTH_TIME_PERIOD->
			data_get_energy:get(energy_time4);
		_ ->
			false
	end,
	case Result of
		false->
			{false,2};
		{Beintime,_EndTime,Energy}->
			case Now>=util:datetime_to_seconds({erlang:date(),Beintime}) of
				true->
					{Step,Energy};
				false->
					{false,4}
			end
	end.

hook_zero_clock()->
	NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
	timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
	%% 重新加载活动配置和点将价钱配置
	user_default:lc(data_box_price),
    clean_role_get_energy(),
    relic_server:clean_relic_times(),
%% 	erlang:put(?get_energy_roleList,[]),
%% 	erlang:put(?get_energy_roleList2,[]),
	clear_buff().

clean_role_get_energy()->
    ets:delete_all_objects(?ETS_ENERGY_ROLELIST).

is_role_get_energy(RoleID,Type)->
    case ets:lookup(?ETS_ENERGY_ROLELIST, {RoleID,Type}) of
        [] ->
            false;
        _ ->
            true
    end.

set_role_get_energy(RoleID,Type)->
    ets:insert(?ETS_ENERGY_ROLELIST, {{RoleID,Type},1}).

do_finish_exchange_condition(RoleID, ActivityID, DrawID) ->

	#data_activity{drawList=ConfigDrawList,type=Type} =data_activity:get(ActivityID),
	#data_activity_draw{maxDrawNum=MaxDrawNum}=lists:keyfind(DrawID, #data_activity_draw.drawID, ConfigDrawList),
		
  	Info = get_info(RoleID),
	{#act{list=DrawList} = Act, ActList2} = take_act(Info, ActivityID, Type),
	
	case lists:keytake(DrawID, #draw.drawID, DrawList) of
		false ->
			DrawList2 = DrawList,
			AT2=1,
			if MaxDrawNum =:= ?FOREVER ->
				   CanDrawTimes2 = MaxDrawNum;
			   true->
				   CanDrawTimes2=MaxDrawNum-1
			end,
			Draw2=#draw{ableDrawTimes=CanDrawTimes2,drawID=DrawID,alreadyDrawTimes=AT2};
		{value, #draw{ableDrawTimes=CanDrawTimes,alreadyDrawTimes=AT}=Draw, DrawList2}->
			AT2=AT+1,
			if CanDrawTimes =:= ?FOREVER ->
				   CanDrawTimes2 = CanDrawTimes;
			   true->
				   CanDrawTimes2=CanDrawTimes-1
			end,
			Draw2=Draw#draw{ableDrawTimes=CanDrawTimes2,alreadyDrawTimes=AT2},
			next
	end,
   	DrawList3 = [Draw2|DrawList2],
   	Act2 = Act#act{list=DrawList3},
   	Info2 = Info#dc{actList=[Act2|ActList2]},
   	set_info(RoleID, Info2),
	?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=AT2,canDrawTimes=CanDrawTimes2,drawID=DrawID,result=1}).
		
		
%%　兑换活动领取
do_draw_exchange(RoleID, ActivityID, DrawID, _Info, _Act, _ActList2, DrawList, Condition, MaxDrawNum, Reward) ->
	case lists:keytake(DrawID, #draw.drawID, DrawList) of
		false ->
			_DrawList2 = DrawList,
			AT=0,
			CanDrawTimes=MaxDrawNum,
			_Draw=#draw{ableDrawTimes=CanDrawTimes,drawID=DrawID,alreadyDrawTimes=AT};
		{value, #draw{ableDrawTimes=CanDrawTimes,alreadyDrawTimes=AT}=_Draw, _DrawList2}->
			next
	end,
	if MaxDrawNum > AT orelse MaxDrawNum =:= ?FOREVER ->
		   role_lib:send_server(RoleID, {activity_exchange, ActivityID, DrawID, Condition, Reward});
	   true ->
		   ?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=AT,canDrawTimes=CanDrawTimes,drawID=DrawID,result=2})
	end.


%%　其他活动领取
do_draw(RoleID, ActivityID, DrawID,VipLevel,RoleLevel,Chose) ->
	case data_activity:get(ActivityID) of
		#data_activity{drawList=ConfigDrawList,type=Type,startTime=StartTimeConfig,stopTime=StopTimeConfig,vip = {_,NeedVipMin,NeedVipMax},level={_,NeedLevelMin,NeedLevelMax}} 
		when (VipLevel =< NeedVipMax) andalso (VipLevel >= NeedVipMin) andalso (RoleLevel =< NeedLevelMax) andalso (RoleLevel >= NeedLevelMin) ->
			case check_active_time(StartTimeConfig,StopTimeConfig) of
				true->
					case lists:keyfind(DrawID, #data_activity_draw.drawID, ConfigDrawList) of
						false ->
							?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2});
						#data_activity_draw{reward=RewardTemp,condition=Condition,maxDrawNum=MaxDrawNum,getNum=GetNum} ->
							
							case check_reward(RewardTemp, Chose,GetNum)of
								#sell_reward{}=Reward->
									Info = get_info(RoleID),
									{#act{list=DrawList} = Act, ActList2} = take_act(Info, ActivityID, Type),
									case Type of
										exchange ->
											do_draw_exchange(RoleID, ActivityID, DrawID, Info, Act, ActList2, DrawList, Condition, MaxDrawNum,Reward);
										exchange2 ->
											do_draw_exchange(RoleID, ActivityID, DrawID, Info, Act, ActList2, DrawList, Condition, MaxDrawNum,Reward);
										exchange3 ->
											do_draw_exchange(RoleID, ActivityID, DrawID, Info, Act, ActList2, DrawList, Condition, MaxDrawNum,Reward);
										_ ->
											case lists:keytake(DrawID, #draw.drawID, DrawList) of
												false ->
													?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2});
												{value, #draw{ableDrawTimes=ADT,alreadyDrawTimes=AT}=Draw, DrawList2}->
													if ADT >0 ->
														   ADT2 = ADT-1,
														   AT2 = AT + 1,
														   Draw2 = Draw#draw{ableDrawTimes=ADT2,alreadyDrawTimes=AT2},
														   DrawList3 = [Draw2|DrawList2],
														   Act2 = Act#act{list=DrawList3},
														   Info2 = Info#dc{actList=[Act2|ActList2]},
														   set_info(RoleID, Info2),
														   role_lib:send_server(RoleID, {draw_activity_reward,ActivityID,Reward}),
														   ?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=AT2,canDrawTimes=ADT2,drawID=DrawID,result=1});
													   true ->
														   ?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2})
													end
											end
									end;
								_ ->
									?unicast(RoleID,#sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2})
							end
					end;
				false ->
					?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2})
			end;
		_ ->
			?unicast(RoleID, #sc_activity_draw{activityID=ActivityID,alreadyDrawTimes=0,canDrawTimes=0,drawID=DrawID,result=2})
	end.
%%客户端上传奖励是用于实现在所有奖励中选择其中一个的功能
check_reward(RewardTemp,_,0) ->
	RewardTemp;
check_reward(RewardTemp, Chose,GetNum) ->
	#p_reward_info{coin=Coin,roleExp=RoleExp,gerExp=GerExp,gold=Gold,reputation=Repu,itemList=Items,gerList=Gers} = Chose,
	Chose2 = [{coin,Coin},{roleExp,RoleExp},{gerExp,GerExp},{gold,Gold},{reputation,Repu},{itemList,get_item_list(Items)},{gerList,get_ger_list(Gers)}],
	Reward = check_reward(RewardTemp,Chose2,GetNum,#sell_reward{}),
	Reward.

check_reward(_,[],0,Reward) ->
	Reward;
check_reward(RewardTemp,[{_,0}|T],GetNum,Reward)->
	check_reward(RewardTemp,T,GetNum,Reward);
check_reward(RewardTemp,[{_,[]}|T],GetNum,Reward)->
	check_reward(RewardTemp,T,GetNum,Reward);
check_reward(_,_,0,_) ->
	false;
check_reward(RewardTemp,[{coin,Coin}|T],GetNum,Reward) ->
	case RewardTemp#sell_reward.coin of
		Coin ->
			check_reward(RewardTemp,T,GetNum-1,Reward#sell_reward{coin=Coin});
		_ ->
			false
  end;
check_reward(RewardTemp,[{roleExp,RoleExp}|T],GetNum,Reward) ->
	case RewardTemp#sell_reward.roleExp of
		RoleExp ->
			check_reward(RewardTemp,T,GetNum-1,Reward#sell_reward{roleExp=RoleExp});
		_ ->
			false
	end;
check_reward(RewardTemp, [{gerExp,GerExp}|T],GetNum,Reward)->
	case RewardTemp#sell_reward.gerExp of
		GerExp ->
			check_reward(RewardTemp,T,GetNum-1,Reward#sell_reward{gerExp=GerExp});
		_ ->
			false
	end;
check_reward(RewardTemp,[{gold,Gold}|T],GetNum,Reward) ->
	case RewardTemp#sell_reward.gold of
		Gold ->
			check_reward(RewardTemp,T,GetNum-1,Reward#sell_reward{gold=Gold});
		_ ->
			false
	end;
check_reward(RewardTemp,[{reputation,Repu}|T],GetNum,Reward) ->
	case RewardTemp#sell_reward.reputation of
		Repu ->
			check_reward(RewardTemp,T,GetNum-1,Reward#sell_reward{reputation=Repu});
		_ ->
			false
	end;
check_reward(RewardTemp,[{itemList,Items}|T],GetNum,Reward) ->
	Result = 
	lists:foldl(fun(_,false) ->
						false;
				   (NewItem,RItem)-> 
						case lists:member(NewItem,RItem) of 
							true ->
								lists:delete(NewItem,RItem);
							_ ->
								false
						end
				end, RewardTemp#sell_reward.item, Items),
	case Result of
		false ->
			false;
		_ ->
			N = GetNum - length(Items),
			if N >= 0 ->
				   check_reward(RewardTemp,T,N,Reward#sell_reward{item=Items});
			   true ->
				   false
			end
	end;
check_reward(RewardTemp,[{gerList,Gers}|T],GetNum,Reward) ->
	Result = 
		lists:foldl(fun(_,false) ->
							false;
					   (NewGer,RGer) ->
							case lists:member(NewGer,RGer) of
								true ->
									lists:delete(NewGer,RGer);
								_ ->
									false
							end
					end,RewardTemp#sell_reward.newGer,Gers),
	case Result of
		false ->
			false;
		_->
			N = GetNum - length(Gers),
			if N >= 0 ->
				   check_reward(RewardTemp,T,N,Reward#sell_reward{newGer=Gers});
			   true ->
				   false
			end
	end;
check_reward(_,[],_,_) ->
	false.


get_ger_list(GerList) ->
	ger_lib:p_ger_view2new_gerList(GerList).
get_item_list(ItemList) ->
	item_lib:p_item_view2new_itemList(ItemList).


do_role_consume(Type, RoleID, ConsumeValue,{_, ConfigList})->
	Info = get_info(RoleID),
	Info2 = 
		lists:foldl(fun(DataActivity, InfoAcc) ->
							case DataActivity#data_activity.type of
								consume ->
									do_role_consume2(Type, RoleID, ConsumeValue, DataActivity, InfoAcc);
								_ ->
									InfoAcc
							end
					end, Info, ConfigList),
	set_info(RoleID, Info2).
do_role_consume2(Type, RoleID, ConsumeValue, DataActivity, Info)->
	#data_activity{activityID=ID, drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value=Value} = Act, ActList2} = take_act(Info, ID, consume),
	{Type, OldValue} = get_consume_value(Type, Value),
	NewValue = ConsumeValue + OldValue,
	GetList = lists:filter(fun(#data_activity_draw{condition=[{CType, Condition}]})->
								   CType2 = get_CType(CType),
								   CType2 == Type andalso Condition > OldValue andalso Condition =< NewValue;
							  (_)-> false
						    end, ConfigDrawList),
	%?ERR("list:,~w,~w,~w,~w\n~w",[GetList,Type,ConfigDrawList, DataActivity,DrawList]),
	DrawList2 = lists:foldl(fun(DataActivityDraw, Acc)->
									#data_activity_draw{drawID=DrawID} = DataActivityDraw,
									case lists:keyfind(DrawID, #draw.drawID, Acc) of
										false ->
											?unicast(RoleID, #sc_activity_update{activityID=ID, drawID=DrawID, canDrawTimes=1}),
											[#draw{drawID=DrawID, ableDrawTimes=1, alreadyDrawTimes=0}|Acc];
										#draw{}->
											Acc
									end
							end,DrawList, GetList),
	{Type, NowValue} = calc_new_consume_value(Type, Value, ConsumeValue),
	{_,GoldValue} = get_consume_value(gold, NowValue),
	?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=[GoldValue]}),
	Act2 = Act#act{list=DrawList2,value=NowValue},
	Info#dc{actList=[Act2|ActList2]}.
calc_new_consume_value(Type, {Gold, Coin, Repu}, ConsumeValue)->
	case Type of
		gold ->
			{Type, {Gold+ConsumeValue, Coin, Repu}};
		coin ->
			{Type, {Gold, Coin+ConsumeValue, Repu}};
		repu ->
			{Type, {Gold, Coin, Repu+ConsumeValue}};
		_ ->
			{Type, {Gold, Coin, Repu}}
	end.
get_consume_value(Type, {Gold, Coin, Repu})->
	case Type of
		gold ->
			{Type, Gold};
		coin ->
			{Type, Coin};
		repu ->
			{Type, Repu};
		_ ->
			?ERR("Error Type in activity : consume,Type = ~w",[Type]),
			{Type, 0}
	end.
get_CType(CType)->
	case CType of
		2 ->
			gold;
		_ ->
			repu
	end.
	
do_pay(RoleID, Gold, {_, ConfigList}) ->
	Info = get_info(RoleID),
	Info2 = 
		lists:foldl(fun(DataActivity, InfoAcc) -> 
							do_pay2(DataActivity#data_activity.type, DataActivity, InfoAcc, Gold, RoleID)
					end, Info, ConfigList),
	set_info(RoleID, Info2).

%% 定额充值奖励
do_pay2(pay_special_num=Type, DataActivity, Info, Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList} = Act, ActList2} = take_act(Info, ID, Type),
	%%　判断是否配置了这个元宝项
	case lists:keyfind(Gold, #data_activity_draw.condition, ConfigDrawList) of
		false ->
			Info;
		#data_activity_draw{drawID=DrawID,maxDrawNum=Max} ->
			%% 判断是否有历史数据
			case lists:keytake(DrawID, #draw.drawID, DrawList) of
				false ->
					DrawList3 = [#draw{ableDrawTimes=1,alreadyDrawTimes=0,drawID=DrawID}|DrawList],
					?unicast(RoleID, #sc_activity_update{activityID=ID,drawID=DrawID,canDrawTimes=1});
				{value, #draw{ableDrawTimes=ADT,alreadyDrawTimes=AT}=Draw, DrawList2} ->
					%% 判断是否超过了最大可领取次数
					if AT + ADT >= Max ->
						   DrawList3 = DrawList;
					   true ->
						   ADT2 = ADT+1,
						   NewDraw = Draw#draw{ableDrawTimes=ADT2},
						   DrawList3 = [NewDraw|DrawList2],
						   ?unicast(RoleID, #sc_activity_update{activityID=ID,drawID=DrawID,canDrawTimes=ADT2})
					end
			end,
			Act2 = Act#act{list=DrawList3},
			Info2 = Info#dc{actList=[Act2|ActList2]},
			Info2
	end;
%% 累计充值
do_pay2(pay_acc_num=Type, DataActivity, Info, Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value=OldValue} = Act, ActList2} = take_act(Info, ID, Type),
	NewValue = OldValue+Gold,
	%%　判断是否配置了这个元宝项
	GetList = lists:filter(fun(#data_activity_draw{condition=Condition}) ->
								   Condition > OldValue andalso Condition =< NewValue end, ConfigDrawList),
	DrawList2 = 
		lists:foldl(fun(DataActivityDraw, Acc) ->
							#data_activity_draw{drawID=DrawID} = DataActivityDraw,
							case lists:keyfind(DrawID,#draw.drawID, Acc) of
								false ->
									?unicast(RoleID, #sc_activity_update{activityID=ID,drawID=DrawID,canDrawTimes=1}),
									[#draw{drawID=DrawID,ableDrawTimes=1,alreadyDrawTimes=0}|Acc];
								#draw{} ->
									Acc
							end
					end, DrawList, GetList),
	?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=[NewValue]}),
	Act2 = Act#act{list=DrawList2,value=NewValue},
	Info2 = Info#dc{actList=[Act2|ActList2]},
	Info2;
%% 累计充值天数
do_pay2(pay_day_num=Type, DataActivity, Info, _Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value={OldValue,LastPayDate}} = Act, ActList2} = take_act(Info, ID, Type),
	NowDate = erlang:date(),
	%% 判断上次充值与这次充值的日期是否相同
	if LastPayDate =:= NowDate ->
		   Info;
	   true ->
		   NewValue = OldValue+1,
		   AddDrawList=
			   lists:foldl(fun(#data_activity_draw{drawID=DrawID, condition=Condition},Acc)->
								   if Condition =< NewValue ->
										  case lists:keyfind(DrawID, #draw.drawID, DrawList) of
											  false ->
												  ?unicast(RoleID, #sc_activity_update{activityID=ID, drawID=DrawID, canDrawTimes=1}),
												  [#draw{ableDrawTimes=1, alreadyDrawTimes=0, drawID=DrawID}|Acc];
											  _ ->
												  Acc
										  end;
									  true ->
										  Acc
								   end
						   end, [], ConfigDrawList),
		   DrawList2=AddDrawList ++ DrawList,
		   ?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=[NewValue]}),
		   Act2 = Act#act{list=DrawList2,value={NewValue,NowDate}},
		   Info2 = Info#dc{actList=[Act2|ActList2]},
		   Info2
	end;

%% 累计充值次数
do_pay2(pay_pay_num=Type, DataActivity, Info, _Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value=OldValue} = Act, ActList2} = take_act(Info, ID, Type),
	NewValue = OldValue+1,
	%%　判断奖励条件
	GetList = lists:filter(fun(#data_activity_draw{condition=Condition}) ->
								   Condition > OldValue andalso Condition =< NewValue end, ConfigDrawList),
	DrawList2 = 
		lists:foldl(fun(DataActivityDraw, Acc) ->
							#data_activity_draw{drawID=DrawID} = DataActivityDraw,
							case lists:keyfind(DrawID,#draw.drawID, Acc) of
								false ->
									?unicast(RoleID, #sc_activity_update{activityID=ID,drawID=DrawID,canDrawTimes=1}),
									[#draw{drawID=DrawID,ableDrawTimes=1,alreadyDrawTimes=0}|Acc];
								#draw{} ->
									Acc
							end
					end, DrawList, GetList),
	?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=[NewValue]}),
	Act2 = Act#act{list=DrawList2,value=NewValue},
	Info2 = Info#dc{actList=[Act2|ActList2]},
	Info2;

%% 累计充值天数
do_pay2(pay_con_day=Type, DataActivity, Info, Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value={LastPayDate,ConDays,NewCon}} = Act, ActList2} = take_act(Info, ID, Type),
	NowDate = erlang:date(),
	%{YesterDay,_} = util:seconds_to_datetime(util:datetime_to_seconds({NowDate,{0,0,1}})-86400),
	%% 判断上次充值与这次充值的日期是否相同
	if LastPayDate =:= NowDate ->
		   Info;
	   true ->
		   NewConDays = ConDays+1,NewCon2=NewCon,DrawListT = DrawList,
		   {AddDrawList,IsP,NewCon3}=
			   lists:foldl(fun(#data_activity_draw{drawID=DrawID, condition={Condition,Days}},{Acc,State,NewConAcc})->
								   if Days =< NewConDays andalso Gold == Condition ->
										  case lists:keyfind(DrawID, #draw.drawID, DrawListT) of
											  false ->
												  ?unicast(RoleID, #sc_activity_update{activityID=ID, drawID=DrawID, canDrawTimes=1}),
												  {[#draw{ableDrawTimes=1, alreadyDrawTimes=0, drawID=DrawID}|Acc],true,false};
											  _ ->
												  State2 = if NewCon2 -> true;true -> State end,
												  {Acc,State2,NewConAcc}
										  end;
									  true ->
										  {Acc,State,NewConAcc}
								   end
						   end,{ [],false,NewCon2}, ConfigDrawList),
		   DrawList2=AddDrawList ++ DrawListT,
		   {LPDate,NewConDays2} = if IsP -> {NowDate,NewConDays}; true -> {LastPayDate,NewConDays-1} end,
		   ?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=[NewConDays2]}),
		   Act2 = Act#act{list=DrawList2,value={LPDate,NewConDays2,NewCon3}},
		   Info#dc{actList=[Act2|ActList2]}
	end;

%% 累计充值累计天数
do_pay2(pay_acc_con_day=Type, DataActivity, Info, Gold, RoleID) ->
	#data_activity{activityID=ID,drawList=ConfigDrawList} = DataActivity,
	{#act{list=DrawList,value={OldValue,LastPayDate,ConDays,LastState,NewCon}} = Act, ActList2} = take_act(Info, ID, Type),
	NowDate = erlang:date(),
	%{YesterDay,_} = util:seconds_to_datetime(util:datetime_to_seconds({NowDate,{0,0,1}})-86400),
	
	if LastPayDate == NowDate -> OldValue2 = OldValue,NewValue=OldValue+Gold,LastState2 = LastState,NewCon2=NewCon,DrawListT = DrawList;
	   true -> OldValue2 = 0,NewValue = Gold,LastState2 = false,NewCon2=NewCon,DrawListT = DrawList
	end,
	NewConDays = if LastState2 -> ConDays; true -> ConDays +  1 end,
	{AddDrawList,LastState3,NewCon3}=
		lists:foldl(fun(#data_activity_draw{drawID=DrawID, condition={Condition,Days}},{Acc,LastStateAcc,NewConAcc})->
							if Days =< NewConDays andalso NewValue >=  Condition andalso OldValue2 < Condition ->
								   case lists:keyfind(DrawID, #draw.drawID, DrawListT) of
									   false ->
										   ?unicast(RoleID, #sc_activity_update{activityID=ID, drawID=DrawID, canDrawTimes=1}),
										   {[#draw{ableDrawTimes=1, alreadyDrawTimes=0, drawID=DrawID}|Acc],true,false};
									   _ ->
										   {Acc,LastStateAcc,NewConAcc}
								   end;
							   true ->
								   {Acc,LastStateAcc,NewConAcc}
							end
					end, {[],LastState2,NewCon2}, ConfigDrawList),
	DrawList2=AddDrawList ++ DrawListT,
	NewConDays2 = if LastState3 -> NewConDays ; true -> NewConDays -1 end,  
	?unicast(RoleID, #sc_activity_record_update{activityID=ID,typeValue=[NewValue,NewConDays2]}),
	Act2 = Act#act{list=DrawList2,value={NewValue,NowDate,NewConDays2,LastState3,NewCon3}},
	Info2 = Info#dc{actList=[Act2|ActList2]},
	Info2;

do_pay2(_, _, Info, _, _) ->
	Info.
		   
		   
		   
	

do_get_list(RoleID, RoleLevel, VipLevel, {IconList, DataList}) ->
	{IconList2, _} = lists:foldl(fun(Icon, {Acc1,Acc2}) ->
						[#data_activity{level=TLevel,vip=TVip}|Acc4] = Acc2,
						%% 检查等级限制
						case TLevel of
							{level,MinLevel,MaxLevel} when MinLevel =< RoleLevel andalso MaxLevel >= RoleLevel ->
								%% 检查vip限制
								case TVip of
									{vip,MinVip,MaxVip} when MinVip =< VipLevel andalso MaxVip >= VipLevel ->
										{[Icon|Acc1],Acc4};
									_ ->
										{Acc1,Acc4}
								end;
							_ ->
								{Acc1, Acc4}
						end		
					end, {[],DataList}, IconList),
	?unicast(RoleID, #sc_activity_get_list{iconList=IconList2}).

do_info(RoleID, ID) ->
	Info = get_info(RoleID),
	#data_activity{type=Type, isForever=IsForever, isDailyRefresh=IsDailyRefresh,startTime=StartTime} = Config = data_activity:get(ID),
	%% value结构不同 充值天数,{0,0};其他:{0}
	if Type =:= pay_day_num ->
			#act{value={TypeValue,_TypeDate}} = Act = act(Info, ID, Type),
			TypeValue2 = [TypeValue];
	   Type =:= consume ->
		   #act{value={TypeValue, _OtherType1, _OtherType2}} = Act = act(Info, ID, Type),
		   TypeValue2 = [TypeValue];
	   Type =:= pay_con_day ->
		   	#act{value={TypeDate,TypeValue,_NewCon}} = Act = act(Info, ID, Type),
			NowDate = erlang:date(),
			%{YesterDay,_} = util:seconds_to_datetime(util:datetime_to_seconds({NowDate,{0,0,1}})-86400),
				%if NewCon -> Act=ActT#act{list=[]}, TypeValue2 = [0]; 
				  case TypeDate of
								NowDate -> TypeValue2 =  [TypeValue];
								_ ->  TypeValue2 = [TypeValue]
							end;
	   Type =:= pay_acc_con_day ->
		   NowDate = erlang:date(),
		   %{YesterDay,_} = util:seconds_to_datetime(util:datetime_to_seconds({NowDate,{0,0,1}})-86400),
		   #act{value={OldValue,LastPayDate,ConDays,_LastState,_NewCon}} = Act = act(Info, ID, Type),
		   if LastPayDate == NowDate -> NewValue=OldValue;
			  true -> NewValue = 0
		   end,
		   %NewConDays = if LastState -> ConDays; true -> ConDays -1 end,
		   %TypeValue2 = if NewCon -> Act = ActT2#act{list=[]}, [NewValue,0];true ->
		   TypeValue2 = [NewValue,ConDays];% end;
	   true ->
		   #act{value=TypeValue} = Act = act(Info, ID, Type),
		   TypeValue2 = [TypeValue]
	end,
	if IsForever =:= true->
		   IsForever2 = 1;
	   true ->
		   IsForever2 = 2
	end,
	if IsDailyRefresh =:= true->
		   IsDailyRefresh2 = 1;
	   true ->
		   IsDailyRefresh2 = 2
	end,
	{StartDay,StartSec} = StartTime,
	if erlang:is_tuple(StartDay) ->
		   Record = #sc_activity_info{startTime=util:datetime_to_seconds(StartTime),
									  stopTime=util:datetime_to_seconds(Config#data_activity.stopTime),
									  type=type(Type),
									  drawList=proto_draw_list(Act,Config),
									  activityID=ID,
									  description=Config#data_activity.description,
									  isForever = IsForever2,
									  isDailyRefresh = IsDailyRefresh2,
									  typeValue=TypeValue2};
	   true ->
		   {ServerOpenDate,_} = data_setting:get(serverOpenTime),
		   Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay - 1) * ?ONE_DAY_SECONDS,
		   {StopDay, StopSec} = Config#data_activity.stopTime,
		   Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay -1) * ?ONE_DAY_SECONDS,
		   Record = #sc_activity_info{startTime=Start,
									  stopTime=Stop,
									  type=type(Type),
									  drawList=proto_draw_list(Act,Config),
									  activityID=ID,
									  description=Config#data_activity.description,
									  isForever = IsForever2,
									  isDailyRefresh = IsDailyRefresh2,
									  typeValue=TypeValue2}
	end,
	?unicast(RoleID, Record).

act(#dc{actList=List}, ID, Type) ->
	case lists:keyfind(ID, #act.actID, List) of
		false ->
			#act{actID=ID,list=[],value=default_value(Type)};
		#act{}=Act ->
			Act
	end.

take_act(#dc{actList=List}, ID, Type) ->
	case lists:keytake(ID, #act.actID, List) of
		false ->
			Act = #act{actID=ID,list=[],value=default_value(Type)},
			{Act, List};
		{value, #act{}=Act, List2} ->
			{Act, List2}
	end.

proto_draw_list(Act, DataActivity) ->
	#data_activity{drawList=DrawList} = DataActivity,
	#act{list=List} = Act,
	[begin
		 case lists:keyfind(ID, #draw.drawID, List) of
			 false ->
				 Already = 0,
				 CanDrawTimes = 0;
			 #draw{ableDrawTimes=CanDrawTimes,alreadyDrawTimes=Already} ->
				 next
		 end,
		 if is_list(Condition) ->
		 		NeedMaterial =role_reward:transform2p_reward_view(Condition,[]);
		 	is_record(Condition,sell_reward)->
		 		NeedMaterial = role_reward:transform2p_reward_view(Condition,[]);
			true ->
				NeedMaterial=[]
		 end,
		 #p_activity_draw{alreadyDrawTimes=Already,canDrawTimes=CanDrawTimes,description=Description,drawID=ID,maxDrawTimes=Max,
						  rewardInfo=sell_reward2p_reward_info(Reward),
						  needMaterial=NeedMaterial,getNum=GetNum,discountnum=DiscountNum}						 
	 end || #data_activity_draw{description=Description,drawID=ID,maxDrawNum=Max,reward=Reward,condition=Condition,getNum=GetNum,discountnum=DiscountNum} <- DrawList].
	
sell_reward2p_reward_info(SellReward) ->
	#sell_reward{coin=Coin,gerExp=GerExp,gold=Gold,item=Item,newGer=NewGer,reputation=Reputation,roleExp=RoleExp} = SellReward,
	NewGer2 = ger_lib:new_gerList2p_ger_view(NewGer),
	Item2 = item_lib:new_itemList2p_item_view(Item),
	#p_reward_info{coin=Coin,gerExp=GerExp,gerList=NewGer2,gold=Gold,itemList=Item2,reputation=Reputation,roleExp=RoleExp}.

p_reward_info2sell_reward(RewardInfo) ->
    #p_reward_info{coin=Coin,gerExp=GerExp,gerList=NewGer2,gold=Gold,itemList=Item2,reputation=Reputation,roleExp=RoleExp} = RewardInfo,
    NewGer = ger_lib:p_ger_view2new_gerList(NewGer2),
    Item = item_lib:p_item_view2new_itemList(Item2),
    #sell_reward{coin=Coin,gerExp=GerExp,gold=Gold,item=Item,newGer=NewGer,reputation=Reputation,roleExp=RoleExp}.

get_info(RoleID) ->
	case get(RoleID) of
		#dc{}=Info ->
			Info;
		_ ->
			Info = get_db_info(RoleID),
			Info
	end.

get_db_info(RoleID) ->
	db_sql:get_activityInfo(RoleID).

set_info(RoleID, Info) ->
	put(RoleID, Info),
	db_sql:set_activityInfo(RoleID, Info).

clear_buff() ->
	lists:foreach(fun({RoleID, #dc{}}) when is_integer(RoleID) ->
						  case role_lib:is_online(RoleID) of
							  false->
								  erlang:erase(RoleID);
							  _ ->
								  ignore
						  end;
					 (_) ->
						  ignore				  
				  end, erlang:get()).

daily_fresh(RoleID)->
	#dc{actList=Info} = Acty = get_info(RoleID),
	%%　清理记录，将需要每日刷新的和过期的记录从数据库列表中删除，并通知客户端刷新活动内容
	{NewInfo, ChangeList} = 
		lists:foldl(fun(#act{actID=ID}=Act, {InfoAcc, ChangeAcc})->
							case need_refresh(ID) of
								true ->
									{InfoAcc,[ID|ChangeAcc]};
								_ ->
									{[Act|InfoAcc], ChangeAcc}
							end
					end, {[],[]}, Info),
	% delete outTime act
	set_info(RoleID, Acty#dc{actList=NewInfo}),
	lists:foreach(fun(E)->do_info(RoleID, E) end, ChangeList).
	
need_refresh(ID)->
	case data_activity:get(ID) of
		#data_activity{stopTime  = StopTime, isDailyRefresh = IsDailyRefresh} -> 
			%% out of time
			NowDate = erlang:localtime(),
			%% 计算绝对时间
			{StopDay,StopSec} = StopTime,
			if erlang:is_tuple(StopDay) ->
				   StopTime2 = StopTime;
			   true ->
				   {ServerOpenDate,_} = data_setting:get(serverOpenTime),
				   {StopDay,StopSec} = StopTime,
				   Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay - 1) * ?ONE_DAY_SECONDS,
				   StopTime2 = util:seconds_to_datetime(Stop)
			end,
			
			if IsDailyRefresh =:= true orelse NowDate > StopTime2 ->
				   true;
			   true ->
				   false
			end;
		_ ->
			false
	end.

clear_timeout_buff(RoleID) ->
	Info = get_info(RoleID),
	List = Info#dc.actList,
	{NewList,_} = lists:foldl(fun(ID, {ListNew,ListOld}) ->
				case lists:keytake(ID, #act.actID, ListOld) of
					false ->
						{ListNew,ListOld};
					{value, #act{}=Act, List2} ->
						{[Act|ListNew],List2}
				end
			end , {[],List}, data_activity:get_list()),
	Info2 = Info#dc{actList=NewList},
	set_info(RoleID,Info2).

%% 检查本进程buff的tick
tick() ->
	erlang:send_after(?TICK_INTERVAL*1000, self(), tick).

%% 更新活动状态
tick_state() ->
	Sec = ?TICK_STATE_INTERVAL - (util:now() rem ?TICK_STATE_INTERVAL) - 1, 
	erlang:send_after(Sec*1000 + 700 + random:uniform(600),self(),tick_state).

tick_energy_time()->
	%?ERR("what:~w,~w,~w",[erlang:get(?get_energy_roleList), erlang:get(?get_energy_roleList2),erlang:get(?energy_switch)]),
	{_,NowTime} = erlang:localtime(),
	{Beg1, End1, Eng1} = data_get_energy:get(energy_time1),
	{Beg2, End2, Eng2} = data_get_energy:get(energy_time2),
    {Beg3, End3, Eng3} = data_get_energy:get(energy_time3),
    {Beg4, End4, Eng4} = data_get_energy:get(energy_time4),
	Dif1 = time_diff(Beg1, NowTime),
	Dif2 = time_diff(End1, NowTime),
	Dif3 = time_diff(Beg2, NowTime),
	Dif4 = time_diff(End2, NowTime),
    Dif5 = time_diff(Beg3, NowTime),
    Dif6 = time_diff(End3, NowTime),
    Dif7 = time_diff(Beg4, NowTime),
    Dif8 = time_diff(End4, NowTime),
	%?ERR("what diff:~w,~w,~w,~w",[Dif1, Dif2, Dif3, Dif4]),
	if Dif1 < 0 ->
		   ignore;
	   Dif1 > 0 andalso Dif2 < 0 ->
		   get_energy_open(1,Eng1);
	   Dif2 > 0 andalso Dif3 < 0 ->
		   get_energy_close(1);
	   Dif3 > 0 andalso Dif4 < 0 ->
		   get_energy_open(2,Eng2);
	   Dif4 > 0 andalso Dif5 < 0->
		   get_energy_close(2);       
       Dif5 > 0 andalso Dif6 < 0 ->
           get_energy_open(3,Eng3);
       Dif6 > 0 andalso Dif7 < 0 ->
           get_energy_close(3);       
       Dif7 > 0 andalso Dif8 < 0 ->
           get_energy_open(4,Eng4);
       Dif8 > 0 ->
           get_energy_close(4);       
	   true ->
		   ignore
	end,
	erlang:send_after(?TICK_CHECK_ENERTY_TIME * 1000, self(), tick_energy_time),
	Date = erlang:date(),
	%?ERR("what:~w,~w,~w",[erlang:get(?get_energy_roleList), erlang:get(?get_energy_roleList2),erlang:get(?energy_switch)]),
	P1 = #p_energy_activity{startTime=util:datetime_to_seconds({Date, Beg1}), endTime=util:datetime_to_seconds({Date, End1}),energy=Eng1},
	P2 = #p_energy_activity{startTime=util:datetime_to_seconds({Date, Beg2}), endTime=util:datetime_to_seconds({Date, End2}),energy=Eng2},
    P3 = #p_energy_activity{startTime=util:datetime_to_seconds({Date, Beg3}), endTime=util:datetime_to_seconds({Date, End3}),energy=Eng3},
    P4 = #p_energy_activity{startTime=util:datetime_to_seconds({Date, Beg4}), endTime=util:datetime_to_seconds({Date, End4}),energy=Eng4},
	erlang:put(?energy_activity_info, {P1,P2,P3,P4}).


get_energy_open(Type,Eng)->
	%?ERR("energy open"),
	erlang:put(?get_energy_state, {Type,Eng}).
get_energy_close(N)->
	%?ERR("energy close"),
	erlang:put(?get_energy_state, 0).

time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).

type(exchange) ->1;
type(exchange2) ->5;
type(pay_special_num) ->3;
type(pay_acc_num) ->2;
type(pay_day_num) ->4;
type(consume) ->6;
type(pay_pay_num)->7;
type(pay_con_day)->10;
type(pay_acc_con_day)->11;
type(exchange3) ->12;

type(?carlos_honor_double)->5;
type(?pvp_reward_double)->5;
type(?hron_reward_double)->5;
type(?relic_box_double)->5;
type(?homestead_reward_double)->5;
type(?world_boss_box_double)->5.

default_value(pay_special_num) ->0;
default_value(pay_acc_num) ->0;
default_value(pay_day_num) ->{0, 0};
default_value(consume) ->{0, 0, 0};
default_value(exchange) ->0;
default_value(exchange2) ->0;
default_value(pay_pay_num) -> 0;
default_value(pay_con_day) -> {0,0,true};
default_value(pay_acc_con_day) -> {0,0,0,0,true};
default_value(_) -> 0.

refresh_config()->
		
	% 同时更新下activity和box_price的配置
	tk_config:reload_config(data_box_price),
	ActivityLastModifyTime = filelib:last_modified("config/activityconfig/data_activity.config"),
	case get(?last_activity_config) of
		ActivityLastModifyTime ->
			ignore;
		_ ->
			put(?last_activity_config,ActivityLastModifyTime),
			?ERR("load data_activity.config"),
			tk_config:reload_config(data_activity)
	end,
		%% 判断是否通知客户端更新价格信息
	Last_Price = get_last_price(),
	New_Price = lists:foldl(fun(E,Acc)->[data_box_price:get(E)|Acc] end, [], lists:reverse(data_box_price:get_list())),
	case New_Price == Last_Price of
		true ->
			ignore;
		_ ->
			NextStopTime = lists:foldl(fun(#data_box_price2{isOpenActivity=IOA,endtime=ET},MinET)->
								case IOA of
									0->
										MinET;
									_->
										case MinET of
											0->
												ET;
											_->
												erlang:min(ET, MinET)
										end
								end
						end, 0, New_Price),
			?DEBUG("====NextStopTime====~w",[{util:now(),NextStopTime}]),
			timer_wheel:cancel_plan(get(?REFRESH_CONFIG_PLAN_PLAG)),
			case NextStopTime of
				0->
					ignore;
				_->
					{Sec,Ref} = timer_wheel:add_plan(NextStopTime+1, fun refresh_config/0),
					put(?REFRESH_CONFIG_PLAN_PLAG,{Sec,Ref})
			end,
			set_new_price(New_Price),
			%List = [data_box_price:get(E)||E<-data_box_price:get_list()],
			ShopInfo = [#p_shop_box_info{itemTypeID=ItemTypeID,valueOne=ValueOne,valueTen=ValueTen,discount=Discount,isOpenActivity=IsOpenActivity,endtime=EndTime} || #data_box_price2{itemTypeID=ItemTypeID,oncePrice=ValueOne,tenTimesPrice=ValueTen,discount=Discount,isOpenActivity=IsOpenActivity,endtime=EndTime}<-New_Price],
			?DEBUG("===广播===~p",[ShopInfo]),
			broadcast_server:bc(#sc_box_shop_info{info=ShopInfo})
	end.

init_state() ->
    ets:delete_all_objects(?ets_cache_cur_activity),
	Now = util:now(),
	lists:foldl(fun(ID, {Acc1,Acc2}) ->
						#data_activity{activityName=Name,iconSrc=IconSrc,startTime=StartTime,stopTime=StopTime, isForever=IsForever,sort_type=SortType} = DataActivity =  data_activity:get(ID),
						{StartDay,StartSec} = StartTime,
						if erlang:is_tuple(StartDay) ->
							   Start = util:datetime_to_seconds(StartTime),
							   Stop = util:datetime_to_seconds(StopTime);
						   true ->
							   {ServerOpenDate,_} = data_setting:get(serverOpenTime),
							   Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay - 1) * ?ONE_DAY_SECONDS,
							   {StopDay,StopSec} = StopTime,
							   Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay - 1) * ?ONE_DAY_SECONDS
						end,
						if Now > Start , Now < Stop orelse IsForever =:= true->
							   StartTime2 = util:seconds_to_datetime(Start),
							   StopTime2 = util:seconds_to_datetime(Stop),
                               case lists:member(DataActivity#data_activity.type,?cache_filter_list) of
                                   true ->
                                       ets:insert(?ets_cache_cur_activity, {DataActivity#data_activity.type});
                                   false ->
                                       ignore
                               end,
							   {[#p_activity_icon{activityID=ID,iconRrc=IconSrc,activityName=Name,activityType=SortType}|Acc1],[DataActivity#data_activity{startTime=StartTime2,stopTime=StopTime2}|Acc2]};
						   true ->
							   {Acc1,Acc2}
						end
				end , {[] ,[]}, data_activity:get_list()).

%时间条件不满足的话，就用默认值替代
load_box_price(List) ->
	Now = util:now(),
	%InitList = lists:foldl(fun(E,Acc)->[#data_box_price2{itemTypeID=E,oncePrice=280,tenTimesPrice=2800,isOpenActivity=0}|Acc] end, [], [24100,24002,24001]),
	%下面这个列表的顺序不能变化，客户端依赖这个顺序显示三种物品的抽取
    InitList = [#data_box_price2{itemTypeID=24001,oncePrice=280,tenTimesPrice=2800,isOpenActivity=0}
               ,#data_box_price2{itemTypeID=24002,oncePrice=280,tenTimesPrice=2800,isOpenActivity=0}
               ,#data_box_price2{itemTypeID=24100,oncePrice=600,tenTimesPrice=6000,isOpenActivity=0}],
    lists:foldl(fun(Info, Acc)->
						#data_box_price{type=Type,start_time=StartTime, stop_time=StopTime,itemTypeID=ItemTypeID,oncePrice=OncePrice,tentimesPrice=TenTimesPrice}=Info,
						case Type of
							1->
								Start = util:datetime_to_seconds(StartTime),
								Stop = util:datetime_to_seconds(StopTime);
							2 ->
								{ServerOpenDate,_} = data_setting:get(serverOpenTime),
								{StartDay,StartSec} = StartTime,
								Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay -1) * ?ONE_DAY_SECONDS,
								{StopDay, StopSec} = StopTime,
								Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay -1) * ?ONE_DAY_SECONDS
						end,
						if Now >= Start ,Now =< Stop ->
							   Discount = if
                                             ItemTypeID =:= 24100 ->
                                                trunc(OncePrice / 600 * 100); %训练师装备的默认价格是600
                                             true ->
                                                trunc(OncePrice / 280 * 100)
                                          end,
							   case ItemTypeID of
								   0 ->
									   Acc2 = lists:foldl(fun(E,AccL)->[#data_box_price2{itemTypeID=E,oncePrice=OncePrice,tenTimesPrice=TenTimesPrice,isOpenActivity=1,endtime=Stop,discount=Discount}|AccL] end, [], [24100,24002,24001]);
								   _ ->
									   if ItemTypeID >= 24001 ,ItemTypeID =< 24100 ->
											  Acc2 = lists:keyreplace(ItemTypeID, #data_box_price2.itemTypeID, Acc, #data_box_price2{itemTypeID=ItemTypeID,oncePrice=OncePrice,tenTimesPrice=TenTimesPrice,isOpenActivity=1,endtime=Stop,discount=Discount});
										  true ->
											  Acc2 = Acc
									   end
							   end;
						   true ->
							   Acc2 = Acc
						end,
						
						Acc2
				end, InitList, List).

set_new_price(Price)->
	erlang:put(?box_price_info,Price).
get_last_price()->
	case erlang:get(?box_price_info) of
		?undefined ->
			lists:foldl(fun(E,Acc)->[#data_box_price2{itemTypeID=E,oncePrice=280,tenTimesPrice=2800,isOpenActivity=0}|Acc] end, [], [24100,24002,24001]);
		X ->
			X
	end.

check_active_time(StartTimeConfig,StopTimeConfig)->
	{StartDay,StartTime} = StartTimeConfig,
	{StopDay,StopTime} = StopTimeConfig,
	{ServerOpenDate, _} = data_setting:get(serverOpenTime),
	NowSecond = util:now(),
	case is_number(StartDay) of
		true ->
			StartTimeSecond = util:datetime_to_seconds({ServerOpenDate,StartTime}) + (StartDay -1) * ?ONE_DAY_SECONDS;
		false ->
			StartTimeSecond = util:datetime_to_seconds({StartDay,StartTime})
	end,
	case is_number(StopDay) of
		true ->
			StopTimeSecond = util:datetime_to_seconds({ServerOpenDate,StopTime}) + (StopDay -1) * ?ONE_DAY_SECONDS;
		false ->
			StopTimeSecond = util:datetime_to_seconds({StopDay,StopTime})
	end,
	NowSecond >= StartTimeSecond andalso NowSecond =< StopTimeSecond.
