%% @author lixinglong
%% @doc 各种活动的排行榜进程
%% Created 2013-5-10

%% 精灵宝藏的数据都存在ETC_TABLE_NAME这张etc表中,
%% role进程可以并发的从这张表里面读取数据,但不能
%% 进行写入操作.role进程需要将数据send到当前进程来
%% 进行串行写入.

-module(activityRank_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_activity_rank.hrl").
-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).
-define(DUMP_INTERVAL_SECS, (1 * 60)). %% 定时dump当前排行榜的数据的时间间隔
-define(TREAHOUSR_RANK_LIST_NUM,20).
-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
-record(rankerInfo, {rank, roleID, roleName, mark}).
-record(state, {}).

%% 更新排行
update_treaHouse_roleRank(RoleID, RoleName, Mark)->
	erlang:send(?MODULE, {update_treaHouse_roleRank, RoleID, RoleName,Mark}).

i() ->
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
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
    %% 创建ets表, 当前进程能够串行写, 其他进程能够并发读
    %% ets表的格式为:
    %% {{activity_type, info_type},info}
    %% {{活动类型, 信息类型}, 数据}
    %% 信息类型为:activity_info, rank, rank_firstN, reward_role
    %% 当前正在进行的活动格式为:{ETS_TABLE_ACTIVITY_INFO, 数据}.
    ?ETS_TABLE_NAME = ets:new(?ETS_TABLE_NAME, [set, protected, named_table, {keypos, 1}, {heir, none}, {write_concurrency,false}, {read_concurrency,true}]),
	State = init_activity_info(),
    do_clear_state_info(get_running_activity( )),
    erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
	{ok, State}.

%% 活动结束和活动开始的时候,都将该类型活动的所有信息清除
do_clear_state_info(RunningInfo)->
    OldInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}), 
    %% 已经关闭了的活动
	lists:foreach(fun(#data_activity_rank{activityID=ActivityID, type=Type})->
						  case lists:keyfind(ActivityID, #data_activity_rank.activityID, OldInfo) of
							  false ->
								  clear_info(Type);
							  _ ->
								  ignore
						  end
				  end, RunningInfo),
    %% 新开的活动
	lists:foreach(fun(#data_activity_rank{activityID=ActivityID, type=Type})->
						  case lists:keyfind(ActivityID, #data_activity_rank.activityID, RunningInfo) of
							  false ->
								  clear_info(Type);
							  _ ->
								  ignore
						  end 
				  end, OldInfo).

time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).


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
	Reply = 
	case ?CATCH(do_handle_call(Request, State)) of
		{'EXIT', _} ->
			ok;
		X ->
			X
	end,
	{reply, Reply, State}.
%% handle_call(Request, _From, State) ->
%% 	?ERR("handle_call function clause:request=~100p",[Request]),
%%     Reply = ok,
%%     {reply, Reply, State}.

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

handle_info(dump_cur_rank_data, State) ->
	persist_terminate(State),
    do_refresh_activity( ),
	erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
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
	persist_terminate(State),
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

do_handle_call(Request,_State)->
	?ERR("handle_call function clause:request=~100p",[Request]).

do_handle_info({update_treaHouse_roleRank, RoleID, RoleName, Mark},_State)->
	update_treaHouse_roleRankInfo(RoleID, RoleName, Mark);

%% 这个操作有对ets表的写入,因此还是放在当前进程来处理
do_handle_info({client_msg, RoleID, #cs_treaHouse_get_rank_Reward{}},_State)->
    ActivityInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}),
	case check_treaHouse_open_state2(ActivityInfo) of
		true ->
			case check_treaHouse_rank_reward_time(ActivityInfo) of
				true ->
					case check_treaHouse_get_rank_reward(RoleID) of
						{true, Rank} ->
							Reward = do_treaHouse_get_rank_reward(RoleID, Rank),
							?unicast(RoleID, #sc_treaHouse_get_rank_Reward{type=1,rank=Rank, rewardInfo=Reward});
						{false, Reason}->
							?unicast(RoleID, 
									 #sc_treaHouse_get_rank_Reward{type=Reason,rank=0,rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)})
					end;
				false ->
					?unicast(RoleID, #sc_treaHouse_get_rank_Reward{type=2,rank=0,rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)})
			end;
		false ->
			?unicast(RoleID, #sc_treaHouse_get_rank_Reward{type=3,rank=0,rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)})
	end;
do_handle_info(Info,_State) ->
	?ERR("handle_info function clause:request=~100p",[Info]).

check_treaHouse_is_role_get_rankReward(RoleID)->
	case is_treaHouse_Role_get_reward(RoleID) of
		true ->
			1;
		false->
			2
	end.

check_treaHouse_open_state()->
    ActivityInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}),
	case lists:keyfind(treaHouse, #data_activity_rank.type, ActivityInfo) of
		false ->
			false;
		#data_activity_rank{stopTime=StopTime,startTime = StartTime} ->
			NowSec = util:now(),
			if NowSec >= StartTime andalso NowSec =< StopTime ->
				   true;
			   true ->
				   false
			end
	end.

check_treaHouse_open_state2() ->
    ActivityInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}),
    check_treaHouse_open_state2(ActivityInfo).

check_treaHouse_open_state2(ActivityInfo)->
	case lists:keyfind(treaHouse, #data_activity_rank.type, ActivityInfo) of
		false ->
			false;
		#data_activity_rank{endTime=EndTime,startTime = StartTime} ->
			NowSec = util:now(),
			if NowSec >= StartTime andalso NowSec =< EndTime ->
				   true;
			   true ->
				   false
			end
	end.

check_treaHouse_rank_reward_time(ActivityInfo)->
	case lists:keyfind(treaHouse, #data_activity_rank.type, ActivityInfo) of
		false ->
			false;
		#data_activity_rank{stopTime=StopTime, endTime=EndTime} ->
			NowSec = util:now(),
			if NowSec >= StopTime andalso NowSec =< EndTime ->
				   true;
			   true ->
				   false
			end
	end.

end_treaHouse_activity_and_send_reward()->
	send_treaHouse_activity_reward(),
	%clear_info(?treaHouse),
	ok.

send_treaHouse_activity_reward()->
	RankList = get_curRank(?TREAHOUSE_TYPE_NAME),
	{TreaHouse_Rank_FirstN_Num, RewardList} = data_treasure_box:get(rank_reward),
	FirstNList = lists:sublist(RankList, TreaHouse_Rank_FirstN_Num),
	lists:foreach(fun(#rankerInfo{rank=Rank, roleID=RoleID})->
						  case is_treaHouse_Role_get_reward(RoleID) of
							  true ->
								  ignore;
							  _ ->
								  {_,Reward} = lists:keyfind(Rank, 1, RewardList),
								  mail_server:send_sys_mail(RoleID, ?MAIL_TREAHOUSE_RANK_REWARD, [Rank], "", Reward)
						  end
				  end, FirstNList).

do_treaHouse_get_rank_reward(RoleID, Rank)->
	%RankList = get_curRank(?treaHouse_rank),
	{_,RewardList} = data_treasure_box:get(rank_reward),
	{_,Reward} = lists:keyfind(Rank, 1, RewardList),
	%% 发奖励并设置已领奖记录
	%role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_TREAHOUSE_RANKREWARD, Rank, ""),
	?CATCH(role_lib:send_server(RoleID, {draw_activity_rank_reward, Reward, 0, Rank})),
	record_treaHouse_Role_get_reward(RoleID),
	activity_server:sell_reward2p_reward_info(Reward).

record_treaHouse_Role_get_reward(RoleID) ->
	record_role_get_reward(RoleID, ?TREAHOUSE_TYPE_NAME).

is_treaHouse_Role_get_reward(RoleID) ->
	RoleList = get_reward_list(?TREAHOUSE_TYPE_NAME),
	lists:member(RoleID, RoleList).

record_role_get_reward(RoleID, Type)->
    ets:insert(?ETS_TABLE_NAME, {{Type, ?ETS_TABLE_REWARD_ROLE}, [RoleID|lists:delete(RoleID, get_reward_list(Type))]}).

get_reward_list(Type) ->
    get_one_match({{Type, ?ETS_TABLE_REWARD_ROLE}, '$1'}).
  
check_treaHouse_get_rank_reward(RoleID) ->
	case is_treaHouse_Role_get_reward(RoleID) of
		true ->
			{false, 5};
		_ ->
			RankList = get_curRank(?TREAHOUSE_TYPE_NAME),
			case lists:keyfind(RoleID, #rankerInfo.roleID, RankList) of
				false ->
					{false, 4};
				#rankerInfo{rank=Rank} ->
					{TreaHouse_Rank_FirstN_Num,_} = data_treasure_box:get(rank_reward),
					if Rank > TreaHouse_Rank_FirstN_Num ->
						   {false, 4};
					   true ->
						   {true, Rank}
					end
			end
	end.

do_get_treaHouse_rankInfo(RoleID,Mark)->
	FirstNInfoList = get_firstN(?TREAHOUSE_TYPE_NAME),
	SelfInfo = get_treaHouse_self_rankInfo(RoleID, Mark),
	{SelfInfo, FirstNInfoList}.

get_treaHouse_self_rankInfo(RoleID,Mark2) ->
	%?ERR("info:~w,~w",[RoleID,get_curRank(?treaHouse_rank)]),
	case lists:keyfind(RoleID, #rankerInfo.roleID, get_curRank(?TREAHOUSE_TYPE_NAME)) of
		false ->
			#rolePublic{roleName=RoleName} = role_lib:get_rolePublic(RoleID),
			#p_treaHouse_ranker{type=2,roleName=RoleName, mark=Mark2, rankNum=0,
								rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)};
		#rankerInfo{rank=Rank, roleName=RoleName, mark=Mark} ->
			{MaxRank, _} = data_treasure_box:get(rank_reward),
			if Rank =< MaxRank ->
				   lists:keyfind(Rank, #p_treaHouse_ranker.rankNum, get_firstN(?TREAHOUSE_TYPE_NAME));
			   true ->
				   #p_treaHouse_ranker{type=1, roleName=RoleName, mark=Mark, rankNum=Rank,
									   rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)}
			end
	end.

update_treaHouse_roleRankInfo(RoleID, RoleName, Mark)->
	case Mark >= data_treasure_box:get(rank_base_mark) of
		true ->
			Ranker=#rankerInfo{rank=0, roleID=RoleID, roleName=RoleName, mark=Mark},
			OldRankList = get_curRank(?TREAHOUSE_TYPE_NAME),
			{NewRank, NewRankList} =  insert_rank3(Mark, Ranker,OldRankList,[],0),
			%?ERR("update:~w,\n~w",[OldRankList, NewRankList]),
			%NewRankList2T = lists:keyreplace(RoleID, #rankerInfo.roleID, NewRankList, Ranker#rankerInfo{rank=NewRank}),
			NewRankList2 = update_treaHouse_rankList(NewRankList),
			%?ERR("newRank:~w,~w,\n~w",[NewRank,NewRankList2,Ranker]),
			{TreaHouse_Rank_FirstN_Num,_} = data_treasure_box:get(rank_reward), 
			set_curRank(?TREAHOUSE_TYPE_NAME, NewRankList2),
			if NewRank =< TreaHouse_Rank_FirstN_Num ->
				   refresh_firstN(?TREAHOUSE_TYPE_NAME, NewRankList2);
			   true ->
				   ignore
			end;
		_ ->
			ignore
	end.

update_treaHouse_rankList(NewRankList)->
	{_,L}=lists:foldl(fun(Ranker, {Rank,RankerList})->{Rank+1,[Ranker#rankerInfo{rank=Rank}|RankerList]} end, {1,[]}, NewRankList),
	lists:reverse(L).

refresh_firstN(?TREAHOUSE_TYPE_NAME, RankList) ->
	{TreaHouse_Rank_FirstN_Num, RewardList} = data_treasure_box:get(rank_reward), 
	FirstNList = lists:sublist(RankList, TreaHouse_Rank_FirstN_Num),
    {_, FirstNInfoList} = lists:foldl(fun(#rankerInfo{mark=Mark, roleName=RoleName}, {Rank, Acc})->
									     {_,Reward} = lists:keyfind(Rank, 1, RewardList),
								         Ranker = #p_treaHouse_ranker{type=1,roleName=RoleName, mark=Mark,rankNum=Rank
											    				        ,rewardInfo=activity_server:sell_reward2p_reward_info(Reward)},
                                         {Rank + 1, [Ranker|Acc]}
                                      end, {1, []}, FirstNList),
    ets:insert(?ETS_TABLE_NAME, {{?TREAHOUSE_TYPE_NAME, ?ETS_TABLE_RANK_FIRSTN}, lists:reverse(FirstNInfoList)}); 
refresh_firstN(_,_) ->
	ignore.

get_firstN(Type) ->
    get_one_match({{Type, ?ETS_TABLE_RANK_FIRSTN}, '$1'}).

clear_info(Type)->
    %% 先发奖励
    case Type of
        treaHouse ->
	        end_treaHouse_activity_and_send_reward(),
	        refresh_firstN(Type, []),
	        db_sql:erase_all_treaHouse_info();
        _ ->
            ignore
    end,
    %% 删除对应类型活动的ets表数据
    ets:match_delete(?ETS_TABLE_NAME, {{Type, '_'}, '_'}). 

get_curRank(Type) ->
    get_one_match({{Type, ?ETS_TABLE_RANK}, '$1'}). 

set_curRank(Type, RankList) ->
    ets:insert(?ETS_TABLE_NAME, {{Type, ?ETS_TABLE_RANK}, RankList}).

%% 判断是否有活动结束,并刷新当前活动列表
do_refresh_activity( ) ->
	NowSec = util:now(),
    CurrInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}),
	lists:foreach(fun(#data_activity_rank{type=Type, endTime=EndTime,stopTime=StopTime})->
						  if NowSec >= EndTime ->
								 clear_info(Type);
							 NowSec >= StopTime andalso NowSec =< StopTime + 200 -> %%控制同步消息的次数 
								 broadcast_server:bc(#sc_treaHouse_change_state{});
							 true ->
								 ignore
						  end
				  end, CurrInfo),
    ets:insert(?ETS_TABLE_NAME, {?ETS_TABLE_ACTIVITY_INFO, get_running_activity()}).

persist_terminate(State)->
        Ets = ets:tab2list(?ETS_TABLE_NAME),
		Info = {State, Ets},
		db_sql:set_etc(?DB_ETC_KEY_ACTIVITYRANK, Info).

init_activity_info()->
	{State, Ets}=
		case db_sql:get_etc(?DB_ETC_KEY_ACTIVITYRANK) of
			[] ->
                {#state{},[]};
			{A,B}->
				{A,B}
		end,
	lists:foreach(fun(Object)->
                    ets:insert(?ETS_TABLE_NAME, Object)
				  end, Ets),
	State.

%% 已存在旧的信息在排行榜
insert_rank3(Score, Ranker, [#rankerInfo{mark=Score2}=H|RankList]=E,Tail,N) ->
	if Score2 >= Score ->
	insert_rank3(Score, Ranker, RankList, [H|Tail],N+1);
	   Score2 < Score ->
		   %% 删除旧的信息
			%?ERR("info:~w,~w",[E,Tail]),
		   NewRankList = lists:reverse(Tail,[Ranker|lists:keydelete(Ranker#rankerInfo.roleID, #rankerInfo.roleID, E)]),
		   {N + 1, NewRankList}
	end;	   
insert_rank3(_Score, Ranker, [],Tail, N) ->
	NewRankList = lists:reverse([Ranker|Tail]),
	{N+1, NewRankList}.

get_running_activity() ->
	Now = util:now(),
	lists:foldl(fun(ID, Acc) ->
						#data_activity_rank{startTime=StartTime,stopTime=StopTime, endTime=EndTime} = DataActivity =  data_activityRank:get(ID),
						{StartDay,StartSec} = StartTime,
						if erlang:is_tuple(StartDay) ->
							   Start = util:datetime_to_seconds(StartTime),
							   Stop = util:datetime_to_seconds(StopTime),
							   End = util:datetime_to_seconds(EndTime);
						   true ->
							   {ServerOpenDate,_} = data_setting:get(serverOpenTime),
							   Start = util:datetime_to_seconds({ServerOpenDate,StartSec}) + (StartDay - 1) * ?ONE_DAY_SECONDS,
							   {StopDay,StopSec} = StopTime,
							   Stop = util:datetime_to_seconds({ServerOpenDate,StopSec}) + (StopDay - 1) * ?ONE_DAY_SECONDS,
							   {EndDay, EndSec} = EndTime,
							   End = util:datetime_to_seconds({ServerOpenDate,EndSec}) + (EndDay - 1) * ?ONE_DAY_SECONDS
						end,
						if Now > Start , Now < End->
							   StartTime2 = util:seconds_to_datetime(Start),
							   StopTime2 = util:seconds_to_datetime(Stop),
							   EndTime2 = util:seconds_to_datetime(End),
							   [DataActivity#data_activity_rank{startTime=util:datetime_to_seconds(StartTime2)
																 ,stopTime=util:datetime_to_seconds(StopTime2)
																 ,endTime=util:datetime_to_seconds(EndTime2)}|Acc];
						   true ->
                                Acc
						end
				end , [], data_activityRank:get_list()).

get_treaHouse_rankInfo(RoleID, Mark) ->
    ActivityInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}),
	case check_treaHouse_open_state2(ActivityInfo) of
		true ->
			{SelfInfo, RankInfo} = do_get_treaHouse_rankInfo(RoleID,Mark),
			IsGetRankReward = check_treaHouse_is_role_get_rankReward(RoleID),
            {1, IsGetRankReward, SelfInfo, RankInfo};
		false ->
			#rolePublic{roleName=RoleName} = role_lib:get_rolePublic(RoleID),
			Self = #p_treaHouse_ranker{type=2,roleName=RoleName, mark=0, rankNum=0,
				rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)},
            {2, 2, Self, []}
	end.

get_treaHouse_activity_end_time() ->
    ActivityInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}),
	case lists:keyfind(treaHouse, #data_activity_rank.type, ActivityInfo) of
		false ->
			false;
		#data_activity_rank{endTime=EndTime, stopTime=StopTime} ->
			{EndTime,StopTime}
	end.

get_treaHouse_activityID()->
    ActivityInfo = get_one_match({?ETS_TABLE_ACTIVITY_INFO, '$1'}),
	case lists:keyfind(treaHouse, #data_activity_rank.type, ActivityInfo) of
		false ->
			0;
		#data_activity_rank{activityID=ActivityID} ->
			ActivityID
	end.

%% 这个表是个set表,所以key和value是双射,
%% 且这个函数只用来匹配一个数据,所以返回
%% 格式为:[[]]
get_one_match(Desc) ->
    case ets:match(?ETS_TABLE_NAME, Desc) of
        [] ->
            [];
        [[Any]] ->
            Any
    end.
