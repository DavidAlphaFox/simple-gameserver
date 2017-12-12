%% @author shujunjie
%% @doc 放爆竹
%% Created 2014-01-08
-module(fire_server).
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

%% ===================Dict Key Begin =========================

-define(dump_interval, 60).			% 保存数据间隔
-define(check_interval, 30).		% 检查状态改变间隔

-define(status, status).			% 服务器当前开启的活动 activity，保存当前执行的活动配置信息序号，该序号在配置文档中可以查到
-define(fireInfo, fireInfo).		% 服务器信息
-define(roleInfo, roleInfo).		% 玩家信息
-define(pushList, pushList).		% 推送列表，保存需要得到推送消息的玩家

%% ===================Dict Key End   =========================

open(RoleID,Name,Description,Icon)->
	erlang:send(?MODULE, {open, RoleID,Name,Description,Icon}).
calc_need_gold(RoleID,Times,Gold,MinTimes)->
	erlang:send(?MODULE, {calc_need_gold, RoleID,Times,Gold,MinTimes}).
setoff(RoleID, DeductGold, Times)->
	erlang:send(?MODULE, {setoff, RoleID, DeductGold, Times}).
get_reward(RoleID)->
	erlang:send(?MODULE, {get_reward, RoleID}).
role_exit_firecrack(RoleID)->
	erlang:send(?MODULE, {role_exit, RoleID}).

%% =========================================================================================

init_status()->
	ActivityID = get_activity(),
%% 	case ActivityID of
%% 		?undefined->
%% 			clear_info();
%% 		_->
%% 			ignore
%% 	end,
	set_status(ActivityID).
get_status()->
	erlang:get(?status).
set_status(ActivityID)->
	erlang:put(?status, ActivityID).

%% 放爆竹服务器信息
-record(d_fire_info,{total					%% 燃放总数
					 ,rankList				%% 排名列表 [{roleID, count}, {roleID, count}]
					 ,rewardList			%% 已经领奖的列表
					}).
%% 玩家燃放信息
-record(d_fire_role_info,{roleID			%% 角色ID
						  ,count			%% 燃放数量
						  ,gold				%% 燃放消费的元宝
						 }).

%% 服务器信息
init_fire_info() ->
	FireInfo =
		case db_sql:get_etc(?DB_ETC_KEY_FIRE) of
			#d_fire_info{} = X ->
				X;
			_ ->
				#d_fire_info{total=0, rankList=[], rewardList=[]}
		end,
	set_fire_info(FireInfo).
get_fire_info()->
	erlang:get(?fireInfo).
set_fire_info(FireInfo) ->
	erlang:put(?fireInfo, FireInfo).
persist_fire_info() ->
	#d_fire_info{total=Total} = FireInfo = get_fire_info(),
	do_info_sync(Total),
	db_sql:set_etc(?DB_ETC_KEY_FIRE, FireInfo).

%% 玩家信息
init_role_info()->
	FireRoleInfo = db_sql:get_etc(?DB_ETC_KEY_FIRE_ROLEINFO),
	set_role_info(FireRoleInfo).
get_role_info()->
	case erlang:get(?roleInfo) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.
get_role_info(RoleID)->
	FireRoleInfo = get_role_info(),
	case lists:keyfind(RoleID, #d_fire_role_info.roleID, FireRoleInfo) of
		false->
			#d_fire_role_info{roleID=RoleID, count=0, gold=0};
		X ->
			X
	end.
set_role_info(FireRoleInfo)->
	erlang:put(?roleInfo, FireRoleInfo).
set_role_info(RoleID, {Count, Gold})->
	FireRoleInfo = get_role_info(),
	NewFireRoleInfo = 
		case lists:keyfind(RoleID, #d_fire_role_info.roleID, FireRoleInfo) of
			false->
				[ #d_fire_role_info{roleID=RoleID, count=Count, gold=Gold} | FireRoleInfo];
			_->
				FireRoleInfo2 = lists:keydelete(RoleID, #d_fire_role_info.roleID, FireRoleInfo),
				[ #d_fire_role_info{roleID=RoleID, count=Count, gold=Gold} | FireRoleInfo2]
		end,
	set_role_info(NewFireRoleInfo).
persist_role_info()->
	FireRoleInfo = get_role_info(),
	db_sql:set_etc(?DB_ETC_KEY_FIRE_ROLEINFO, FireRoleInfo).

%% 清除本轮活动信息
clear_info()->
	set_fire_info(#d_fire_info{total=0, rankList=[], rewardList=[]}),
	set_role_info([]),
	persist_fire_info(),
	persist_role_info(),
	clear_push_list().

%% 推送列表
get_push_list()->
	case erlang:get(?pushList) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.
add_push_list(RoleID)->
	case lists:member(RoleID, get_push_list()) of
		true ->
			ignore;
		false ->
			erlang:put(?pushList, [RoleID|get_push_list()])
	end.
del_push_list(RoleID)->
	erlang:put(?pushList, lists:delete(RoleID, get_push_list()) ).
clear_push_list()->
	erlang:put(?pushList, []).
%% 广播消息并且剔除无效用户
broadcast_record(Total)->
	List = lists:foldl(fun(RoleID, ACC) ->
							   GW = role_lib:gw(RoleID),
							   case is_pid(GW) of
								   true->
                                        #d_fire_role_info{count=Count, gold=ConsumeGold} = get_role_info(RoleID),
                                        Price = get_traded_price(Total),
                                        ReturnGold = ConsumeGold - Count * Price,
                                        role_lib:send_client3(GW, #sc_firecracker_info_sync{total=Total
                                                                                          ,tradedPrice=Price
                                                                                          ,returnGold=ReturnGold}),
									   [RoleID|ACC];
								   false->
									   ACC
							   end
					   end, [], get_push_list()),
	erlang:put(?pushList, List).
%% 同步爆竹信息
do_info_sync(Total)->
	case get_activity() of
		?undefined->
			ignore;
		_->
%% 			broadcast_record(#sc_firecracker_info_sync{total=Total, tradedPrice=get_traded_price(Total)})
            broadcast_record(Total)
	end.

%% ===================================================================


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
	init_fire_info(),
	init_role_info(),
	init_status(),
	%% 	init_hidden_increase(),
	check_tick(),
	dump_tick(),
	{ok, get_fire_info()}.

handle_call(test_get_state, _From, State) ->
    {reply, State, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(test_clean, State) ->
    clear_info(),
    ?ERR("执行测试用接口，清除所有祈祷记录和排名"),
    {noreply, State};
handle_cast(test_reset_reward, State) ->
    FireInfo = get_fire_info(),
    set_fire_info(FireInfo#d_fire_info{rewardList=[]}),
    ?ERR("执行测试用接口，清除领奖记录，可重新领奖"),
    {noreply, State};
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info({delete_role_rankinfo,RoleID,Rank}, State) ->
	#d_fire_info{rankList=RankList} = FireInfo = get_fire_info(),
	RankList2 = 
		case lists:keytake(RoleID, 1, RankList) of
			false ->
				RankList;
			{value,{RoleID,_,_},RankList3} ->
				case get_rank_role(Rank,FireInfo) of
					{RoleID,_,_} ->
						set_role_info(RoleID, {0, 0}),
						RankList3;
					_ ->
						RankList
				end
		end,
	set_fire_info(FireInfo#d_fire_info{rankList=RankList2}),
	{noreply,State};
handle_info({open, RoleID,Name,Description,Icon}, State) ->
    add_push_list(RoleID),
    FireInfo = get_fire_info(),
    RoleInfo = get_role_info(RoleID),
    Total = FireInfo#d_fire_info.total,
    #d_fire_role_info{count=Count, gold=Gold} = RoleInfo,
    ReturnGold = Gold - Count * get_traded_price(Total),
    Rank = get_role_rank(RoleID, FireInfo),
    CanReward = can_get_reward(RoleID, FireInfo),
    case catch role_lib:send_server(RoleID, {fire_open_reuslt, Total,Count,Rank,CanReward,ReturnGold,Name,Description,Icon}) of
        {'EXIT', Error} ->
            ?ERR("role_lib:send_server Fail ~w/n Error:~w",[{fire_open_reuslt, Total,Count,Rank,CanReward,ReturnGold,Name,Description,Icon},Error]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info({calc_need_gold, RoleID, Times, Gold,MinTimes}, State) ->
    FireInfo = get_fire_info(),
    Total = FireInfo#d_fire_info.total,
    {Cost,Newtime} = lists:foldl(fun(X,{AccGold,AccTime})->
                            CurNeedGold = AccGold + get_traded_price(X),
                            if
                                Gold >= CurNeedGold ->
                                    {CurNeedGold,AccTime+1};
                                true ->
                                    {AccGold,AccTime}
                            end
                       end
                       , {0,0}, lists:seq(Total, Total+Times-1)),
    {Cost2,Newtime2} = if
                            MinTimes > Newtime ->
                                {0,0};
                            true ->
                                {Cost,Newtime}
                       end,
    
    ?INFO("即将进行祈祷，祈祷次数:~w，花费:~w",[Newtime2,Cost2]),
    case catch role_lib:send_server(RoleID, {fire_calc_need_gold_reuslt, Cost2, Newtime2}) of
        {'EXIT', Error} ->
            ?ERR("role_lib:send_server Fail ~w/n Error:~w",[{fire_calc_need_gold_reuslt, Cost2, Newtime2},Error]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info({setoff, RoleID, DeductGold, Times}, State) ->
    #d_fire_info{total=Total, rankList=RankList} = FireInfo = get_fire_info(),
    #d_fire_role_info{count=Count, gold=ConsumeGold} = get_role_info(RoleID),
    
    NewTotal = Total + Times,
    NewCount = Count + Times,
    NewGold = ConsumeGold + DeductGold,
    set_role_info(RoleID, {NewCount, NewGold}),
    
    NewRankList =
        case lists:keytake(RoleID, 1, RankList) of
            false->
                %% 不在榜上，查看是否可以进入榜单
                case can_into_rank(NewCount, RankList) of
                    true->
                        #data_activity_fire{rankMax=RankMax} = get_activity_info(),
                        case length(RankList) < RankMax of
                            true ->
                                %% 榜单未满，直接加入
                                [ {RoleID, NewCount,util:now()} | RankList];
                            _ ->
                                %% 剔除最后一名
                                %TempList = lists:keysort(2, RankList),
                                %{HoldList, _DelList} = lists:split(RankMax-1, TempList),
                                [_|HoldList] = lists:keysort(2, RankList),
                                [ {RoleID, NewCount,util:now()} | HoldList]
                        end;
                    false->
                        RankList
                end;
            {value, _, Info} ->
                %% 已经在榜上，更新次数即可
                [ {RoleID, NewCount,util:now()} | Info ]
        end,
    
    NewFireInfo = FireInfo#d_fire_info{total=NewTotal, rankList=NewRankList},
    set_fire_info(NewFireInfo),
    
    #data_activity_fire{randomReward=RandomReward,hiddenReward=HiddenReward,coin=_Coin} = get_activity_info(),
    
    Counts = lists:seq(Count+1, NewCount),
    RewardList = 
        lists:foldr(fun(X, Acc)->
                            case lists:keyfind(X, 1, HiddenReward) of
                                false->
                                role_reward:random_drop2(RandomReward,data_common:get(global_drop_rate))++Acc;
                                {_, BoxReward}->
                                role_reward:random_drop2(BoxReward,data_common:get(global_drop_rate))++Acc
                            end
                    end, [], Counts),
    NowPrice = get_traded_price(NewTotal),
    ReturnGold = NewGold -NewCount * NowPrice,
    ?unicast(RoleID, #sc_firecracker_info_sync{total=NewTotal, tradedPrice=NowPrice, returnGold=ReturnGold}),
    IsCanGetReward = can_get_reward(RoleID, NewFireInfo),
    ?INFO("server setoff ~w",[RewardList]),
    case catch role_lib:send_server(RoleID, {fire_setoff_reuslt, RewardList, NewCount, ReturnGold, IsCanGetReward, DeductGold}) of
        {'EXIT', Error} ->
            ?ERR("role_lib:send_server Fail ~w/n Error:~w",[{fire_setoff_reuslt, RewardList, NewCount, ReturnGold, IsCanGetReward, DeductGold},Error]);
        _ ->
            ok
    end,
    {noreply, State};

handle_info({get_reward, RoleID}, State) ->
    #d_fire_info{rewardList=RewardList} = FireInfo = get_fire_info(),
    Result = 
        case can_get_reward(RoleID, FireInfo) of
            0 ->
                {false, 3};
            1->
                case lists:member(RoleID, RewardList) of
                    true ->
                        %% 已经领取奖励
                        {false, 4};
                    false ->
                        %% 可以领取奖励
                        Rank = get_role_rank(RoleID, FireInfo),
                        {true, get_rank_reward(Rank)}
                end
        end,
    NewRewardList = 
        case Result of
            {true, _} ->
                [RoleID|RewardList];
            _ ->
                RewardList
        end,
    NewFireInfo = FireInfo#d_fire_info{rewardList=NewRewardList},
    set_fire_info(NewFireInfo),
    case catch role_lib:send_server(RoleID, {fire_get_reward_reuslt, Result}) of
        {'EXIT', Error} ->
            ?ERR("role_lib:send_server Fail ~w/n Error:~w",[{fire_get_reward_reuslt, Result},Error]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info(Info, State) ->
	?CATCH(do_handle_info(State, Info)),
	{noreply, State}.

terminate(Reason, State) ->
	persist_fire_info(),
	persist_role_info(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================

do_handle_info(_State, {client_msg, RoleID, #cs_firecracker_close{}}) ->
	del_push_list(RoleID);
do_handle_info(_State, {client_msg, RoleID, #cs_firecracker_rank{}}) ->
	case get_activity_info() of
		?undefined->
			?unicast(RoleID, #sc_firecracker_rank{rankList=[]});
		#data_activity_fire{rankRewards=RankRewards}->
			FireInfo = get_fire_info(),
			RewardRankList = lists:foldr(fun({Rank, SellReward}, Acc)->
												 case get_rank_role(Rank, FireInfo) of
													 ?undefined->
%% 														 [#p_firecracker_rank{rank=Rank, name="", count=0, 
%% 																			  rewardInfo=[activity_server:sell_reward2p_reward_info(SellReward)],
%% 																			 level=0}|Acc];
                                                            Acc;
													 {RankRoleID, Count,_}->
														 [#p_firecracker_rank{rank=Rank, name=role_lib:get_name(RankRoleID), count=Count,
																			  rewardInfo=[activity_server:sell_reward2p_reward_info(SellReward)],
																			 level=role_lib:get_level(RankRoleID)}|Acc]
												 end
										 end, [], RankRewards),
			?unicast(RoleID, #sc_firecracker_rank{rankList=RewardRankList})
	end;
do_handle_info(_State, {role_exit, RoleID}) ->
	del_push_list(RoleID);
do_handle_info(_State, check_tick)->
	check_tick(),
	ActivityID = get_activity(),
	LastActivityID = get_status(),
	case LastActivityID=/=ActivityID of
		true->
			case LastActivityID of
				?undefined->
					clear_info();
				_->
					fire_close(LastActivityID),
					clear_info()
			end,
			send_everyone_change_state();
		false->
			ignore
	end,
	set_status(ActivityID),
	check_hidden_increase();
do_handle_info(_State, dump_tick)->
	dump_tick(),
	persist_fire_info(),
	persist_role_info();
do_handle_info(_State, {hidden_increase, Duration, TargetNumber}) ->
	#d_fire_info{total=Total,rankList=RankList} = get_fire_info(),
    #data_activity_fire{hiddenIncrease_arg = ArgY} = get_activity_info(),
	if Total<TargetNumber ->
		   %% 把时间、数量分成四分
        TotalInRank = trunc(lists:sum([E||{_,E,_}<-RankList])*ArgY) ,
        IncreaseValue_ = TargetNumber - Total,
        IncreaseValue = lists:max([TotalInRank,IncreaseValue_]),
        DurationList = random_cut_for_n(Duration,4),
        NumberList = random_cut_for_n(IncreaseValue,4),
        lists:foldl(fun(Number, {Index, TimeAcc})->
                    Time = lists:nth(Index, DurationList),
                    erlang:send_after((TimeAcc+Time)*1000, self(), {do_hidden_increase, Number}),
?INFO("发送定时补充祈祷次数 ~w ~w",[TimeAcc+Time,Number]),
                    {Index + 1, TimeAcc + Time}
			   end, {1, 0}, NumberList);
	   true ->
		   ignore
	end;
do_handle_info(_State, {do_hidden_increase, Number}) ->
    #d_fire_info{total=Total} = FireInfo = get_fire_info(),
    NewFireInfo = FireInfo#d_fire_info{total=Total+Number},
    set_fire_info(NewFireInfo),
    ?INFO("执行补充祈祷次数 ~w(~w,~w)",[Total,Number]);
            
do_handle_info(State, Info) ->
	?ERR("do_handle_info function clause:request=~100p,state=~100p",[Info, State]).

check_tick()->
	erlang:send_after(?check_interval * 1000, self(), check_tick).

dump_tick()->
	erlang:send_after(?dump_interval * 1000, self(), dump_tick).

fire_close(ActivityID)->
	FireInfo = get_fire_info(),
	%% 返还玩家折扣元宝
	Total = FireInfo#d_fire_info.total,
	Discount = get_discount(ActivityID, Total),
	Price = get_traded_price(ActivityID, Total),
	Firecrackers = get_role_info(),
    ?INFO("fire_close 活动结束了，给下面的人发送奖励 Firecrackers:~w",[Firecrackers]),
	lists:foreach(fun( #d_fire_role_info{roleID=RoleID, count=Count, gold=Gold})->
						  ReturnMoney = Gold - Count * Price,
						  if ReturnMoney>0 ->
								 Reward = {sell_reward,0,0,0,ReturnMoney,[],0,[]},
								 mail_server:send_sys_mail(RoleID, ?MAIL_FIRECRACKER_RETURN_GOLD, [Discount, ReturnMoney], "", Reward);
							 true->
								 ignore
						  end
				  end, Firecrackers),
	%% 邮件发送玩家未领取排名奖励
	RankList = FireInfo#d_fire_info.rankList,
	RewardList = FireInfo#d_fire_info.rewardList,
	NoRewardList = lists:filter(fun({RoleID,_Count,_})->
										Rank = get_role_rank(RoleID, FireInfo),
										not lists:member(RoleID, RewardList) andalso get_rank_reward(ActivityID, Rank)=/=?undefined
								end, RankList),
	lists:foreach(fun({RoleID,_Count,_})->
						  Rank = get_role_rank(RoleID, FireInfo),
						  Reward = get_rank_reward(ActivityID, Rank),
						  mail_server:send_sys_mail(RoleID, ?MAIL_FIRECRACKER_RANK_REWARD, [Rank], "", Reward)
				  end, NoRewardList).

%% 全区燃放次数增长定时器
check_hidden_increase()->
	case fire_server:get_activity_status() of
		1 ->
			NowSec = util:now(),
			#data_activity_fire{startTime=StartTime_,hiddenIncrease=HiddenIncrease} = get_activity_info(),
			StartTime = util:datetime_to_seconds(StartTime_),
			lists:foreach(fun({IncreaseStartTime_, IncreaseEndTime_, TargetNumber})->
								  IncreaseStartTime = StartTime + relative_time(IncreaseStartTime_),
								  IncreaseEndTime = StartTime + relative_time(IncreaseEndTime_),
								  DurationTime = IncreaseEndTime - IncreaseStartTime,
								  DiffTime = NowSec - IncreaseStartTime,
								  case DiffTime>=0 andalso DiffTime<?check_interval of
									  true->
										  erlang:send(?MODULE, {hidden_increase, DurationTime, TargetNumber});
									  false->
										  ignore
								  end
						  end,HiddenIncrease);
		_->
			ignore
	end.

%% 相对时间转换
relative_time({Day,Hour,Minute,Second})->
	Day*24*60*60 + Hour*60*60 + Minute*60 + Second.

%% 获取当前开启的活动ID，没有活动返回?undefined，活动时间相互覆盖的话，取第一个生效
get_activity()->
	Now = util:now(),
	Activity = lists:filter(fun(ActivityID)->
									#data_activity_fire{startTime=StartTime_,stopTime=StopTime_,closeTime=CloseTime_} = data_fire:get(ActivityID),
									StartTime = util:datetime_to_seconds(StartTime_),
									StopTime = StartTime + relative_time(StopTime_),
									CloseTime = StopTime + relative_time(CloseTime_),
									Now>=StartTime andalso Now=<CloseTime
							end, data_fire:get_list()),
	case Activity of
		[ActivityID|_]->
			ActivityID;
		_->
			?undefined
	end.

%% 获取当前开启的活动信息，没有活动返回?undefined
get_activity_info()->
	case get_activity() of
		?undefined->
			?undefined;
		ActivityID->
			data_fire:get(ActivityID)
	end.

%% 获取当前开启的活动状态，0没有活动 1正在进行 2领取排行奖励
get_activity_status()->
	case get_activity_info() of
		?undefined->
			0;
		#data_activity_fire{startTime=StartTime_,stopTime=StopTime_}->
			Now = util:now(),
			StartTime = util:datetime_to_seconds(StartTime_),
			RewardTime = StartTime + relative_time(StopTime_),
			case Now<RewardTime of
				true->
					1;
				false->
					2
			end
	end.

%% 获取原始价格
get_marked_price()->
	get_marked_price(get_activity()).
get_marked_price(ActivityID)->
	#data_activity_fire{gold=Gold} = data_fire:get(ActivityID),
	Gold.

%% 获取折扣列表
get_discounts()->
	get_discounts(get_activity()).
get_discounts(ActivityID)->
	#data_activity_fire{discounts=Discounts} = data_fire:get(ActivityID),
	Discounts.

%% 获取折扣
get_discount(Total)->
	get_discount(get_activity(), Total).
get_discount(ActivityID, Total)->
	Match = [ Discount || {Amount, Discount}<-get_discounts(ActivityID), Total>=Amount],
	lists:min([10|Match]).

%% 获取折扣价格
get_traded_price(Total)->
	get_traded_price(get_activity(), Total).
get_traded_price(ActivityID, Total)->
	Gold = get_marked_price(ActivityID),
	Discount = get_discount(ActivityID, Total),
	trunc(Gold * Discount / 10).

%% 获取RoleID的排名
get_role_rank(RoleID, FireInfo)->
%% 	RankList = lists:reverse(lists:keysort(2, FireInfo#d_fire_info.rankList)),
    RankList = lists:sort(fun({_,Anum,Atime},{_,Bnum,Btime})->
                                  if
                                      Anum > Bnum ->
                                          true;
                                      Anum < Bnum ->
                                          false;
                                      Atime < Btime ->
                                          true;
                                      true ->
                                          false
                                  end
                          end, FireInfo#d_fire_info.rankList),
	case lists:keyfind(RoleID, 1, RankList) of
		false->
			0;
		X ->
			string:str(RankList, [X])
	end.

%% 获取排名奖励信息
get_rank_reward(Rank)->
	get_rank_reward(get_activity(), Rank).
get_rank_reward(ActivityID, Rank)->
	#data_activity_fire{rankRewards=RankRewards} = data_fire:get(ActivityID),
	case lists:keyfind(Rank, 1, RankRewards) of
		false->
			?undefined;
		{Rank, SellReward}->
			SellReward
	end.

%% 能否进入榜单
can_into_rank(Count, RankList)->
	#data_activity_fire{rankLimit=RankLimit, rankMax=RankMax} = get_activity_info(),
	case Count>=RankLimit of
		true->
			length(RankList)<RankMax orelse lists:any(fun({_, Times,_})-> Count>Times end, RankList);
		false ->
			false
	end.

%% 获取指定排名人物信息
get_rank_role(Rank, FireInfo)->
%% 	RankList = lists:reverse(lists:keysort(2, FireInfo#d_fire_info.rankList)),
    RankList = lists:sort(fun({_,Anum,Atime},{_,Bnum,Btime})->
                                  if
                                      Anum > Bnum ->
                                          true;
                                      Anum < Bnum ->
                                          false;
                                      Atime < Btime ->
                                          true;
                                      true ->
                                          false
                                  end
                          end, FireInfo#d_fire_info.rankList),
	case Rank>length(RankList) of
		true->
			?undefined;
		false->
			lists:nth(Rank, RankList)
	end.

%% 能否领奖
can_get_reward(RoleID, FireInfo)->
	Rank = get_role_rank(RoleID, FireInfo),
	case Rank of
		0 ->
			0;
		_->
			case get_rank_reward(Rank) of
				?undefined ->
					0;
				_->
					1
			end
	end.

%% 刷新状态
send_everyone_change_state()->
	role_lib:send_every_server_msg({route, role_firecracker, #cs_firecracker_open{}}).

%% 不均等的分割为四份
random_cut_for_n(Number,N) when N >= 2 andalso is_integer(N)->
	Average = trunc(Number/N), %平均值
	Popple = trunc(Average*0.2 + 1), %波动范围
	List = lists:map(fun(_)->
							 Average - Popple + random:uniform(Popple*2)
					 end, lists:seq(1,N-1)),
	Lave = erlang:max(0, Number - lists:sum(List)),
	[Lave|List];
random_cut_for_n(Number,N) ->
    ?ERR("random_cut_for_n. parameter is wrong ~w ~w",[Number,N]),
    [].

%% 下面研发调试用的方法

test_get_state()->
    gen_server:call(fire_server, test_get_state).

test_get_dictionary()->
    process_info(whereis(fire_server),dictionary).

test_clean()->
    gen_server:cast(fire_server, test_clean).

test_reset_reward()->
    gen_server:cast(fire_server, test_reset_reward).
