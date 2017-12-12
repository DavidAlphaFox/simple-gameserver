%% @author crimoon-17
%% @doc @todo Add description to luckyRoll_server.


-module(lucky_roll_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DUMP_INTERVAL_SECS, (1 * 60)). %% 定时dump当前排行榜的数据的时间间隔
-define(TREAHOUSR_RANK_LIST_NUM,20).
-define(NULL_REWARD, #sell_reward{coin=0,gerExp=0,gold=0,item=0,newGer=0,reputation=0,roleExp=0}).
-record(rankerInfo, {rank, roleID, roleName, mark}).

-define(curRank,curRank).
-define(get_reward,get_reward).
-define(rank_firstN,rank_firstN).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

is_persist({?curRank,_})->true ;
is_persist({?get_reward,_}) ->true ;
is_persist({?rank_firstN,_}) ->true;
is_persist(_) ->false.

add_mark(RoleID, RoleName, Mark)->
	erlang:send(?MODULE, {update_roleRank, RoleID, RoleName,Mark}).

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	State0 = init_activity_info(),
	State1 = init_state(),
	do_clear_state_info(State0,State1),
    erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
	insert_ets({state,State1}),
	{ok, State1}.

time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).

insert_ets(Value) ->
	ets:insert(?ETS_LUCKY_ROLL, Value).

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
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
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(dump_cur_rank_data, State) ->
	persist_terminate(State),
	State2 = do_check_activity_end(State),
	erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
	insert_ets({state,State2}),
	{noreply, State2,hibernate};
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info, State)),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
	persist_terminate(State),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

do_handle_info({update_roleRank, RoleID,RoleName, Mark},_State)->
	update_roleRankInfo(RoleID, RoleName,Mark);

do_handle_info({client_msg, RoleID, #cs_luckyRoll_get_rank_Reward{}},State)->
	case check_reward_time(State) of
		true ->
			case check_get_rank_reward(RoleID) of
				{true, Rank} ->
					do_get_rank_reward(RoleID, Rank),
					%?unicast(RoleID, #sc_luckyRoll_get_rank_Reward{type=1,rank=Rank, rewardInfo=Reward});
					ok;
				{false, Reason}->
					?unicast(RoleID,#sc_luckyRoll_get_rank_Reward{type=Reason,rank=0,
																   rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)})
			end;
		false ->
			?unicast(RoleID, #sc_luckyRoll_get_rank_Reward{type=2,rank=0,rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)})
	end;
do_handle_info({inet_reply,_S,_Status},_State) ->
    ignore;

do_handle_info({Ref,_Res},_State) when is_reference(Ref) ->
    ignore;
do_handle_info(Info,_State) ->
	?ERR("handle_info function clause:request=~100p",[Info]).

get_curRank() ->
	case erlang:get(?curRank) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end.

set_curRank( RankList) ->
	erlang:put(?curRank, RankList).

check_open2([#data_activity_lucky{endTime=EndTime,startTime = StartTime}]) ->
	NowSec = util:now(),
	if NowSec >= StartTime andalso NowSec =< EndTime ->
		   true;
	   true ->
		   false
	end.

check_reward_time([#data_activity_lucky{endTime=EndTime,stopTime = StopTime}]) ->
	NowSec = util:now(),
	if NowSec >= StopTime andalso NowSec =< EndTime ->
		   true;
	   true ->
		   false
	end.
	
check_get_rank_reward(RoleID) ->
	case is_role_get_reward(RoleID) of
		true ->
			{false, 5};
		_ ->
			RankList = get_curRank(),
			case lists:keyfind(RoleID, #rankerInfo.roleID, RankList) of
				false ->
					{false, 4};
				#rankerInfo{rank=Rank} ->
					{LUCKY_ROLL_FirstN_Num,_} = data_lucky_roll:get(rank_reward),
					if Rank > LUCKY_ROLL_FirstN_Num ->
						   {false, 4};
					   true ->
						   {true, Rank}
					end
			end
	end.

do_get_rank_reward(RoleID, Rank)->
	{_,RewardList} = data_lucky_roll:get(rank_reward),
	{_,Reward} = lists:keyfind(Rank, 1, RewardList),
	?CATCH(role_lib:send_server(RoleID, {route,role_luckyRoll,{draw_luckyRoll_rank_reward, Reward, Rank}})),
	record_role_get_reward(RoleID),
%	activity_server:sell_reward2p_reward_info(Reward).
	ok.

is_role_get_reward(RoleID) ->
	RoleList = get_reward_list(),
	lists:member(RoleID, RoleList).

record_role_get_reward(RoleID)->
	erlang:put(?get_reward, [RoleID|lists:delete(RoleID,get_reward_list())]).

get_reward_list() ->
	case erlang:get(?get_reward) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end.

insert_ets_self_info(RoleID,RoleName,Mark,Rank,MaxRank) ->
	if Rank =< MaxRank ->
		   insert_ets({{rank, RoleID},lists:keyfind(Rank,#p_luckyRoll_ranker.rankNum, get_firstN())});
	   true ->
		   insert_ets({{rank, RoleID},#p_luckyRoll_ranker{type=1, roleName=RoleName, mark=Mark, rankNum=Rank,
								rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)}})
	end.
		   

%% do_get_rankInfo(RoleID,Mark)->
%% 	FirstNInfoList = get_firstN(),
%% 	SelfInfo = get_self_rankInfo(RoleID,Mark),
%% 	{SelfInfo, FirstNInfoList}.
%% get_self_rankInfo(RoleID,Mark2) ->
%% 	case lists:keyfind(RoleID, #rankerInfo.roleID, get_curRank()) of
%% 		false ->
%% 			#rolePublic{roleName=RoleName} = role_lib:get_rolePublic(RoleID),
%% 			#p_luckyRoll_ranker{type=2,roleName=RoleName, mark=Mark2, rankNum=0,
%% 								rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)};
%% 		#rankerInfo{rank=Rank, roleName=RoleName, mark=Mark} ->
%% 			{MaxRank, _} = data_lucky_roll_box:get(rank_reward),
%% 			if Rank =< MaxRank ->
%% 				   lists:keyfind(Rank, #p_luckyRoll_ranker.rankNum, get_firstN());
%% 			   true ->
%% 				   #p_luckyRoll_ranker{type=1, roleName=RoleName, mark=Mark, rankNum=Rank,
%% 									   rewardInfo=activity_server:sell_reward2p_reward_info(?NULL_REWARD)}
%% 			end
%% 	end.

end_activity_and_send_reward()->
	send_activity_reward(),
	ok.

send_activity_reward()->
	RankList = get_curRank(),
	{Rank_FirstN_Num, RewardList} = data_lucky_roll:get(rank_reward),
	FirstNList = lists:sublist(RankList, Rank_FirstN_Num),
	lists:foreach(fun(#rankerInfo{rank=Rank, roleID=RoleID})->
						  case is_role_get_reward(RoleID) of
							  true ->
								  ignore;
							  _ ->
								  {_,Reward} = lists:keyfind(Rank, 1, RewardList),
								  mail_server:send_sys_mail(RoleID, ?MAIL_LUCKY_ROLL_RANK_REWARD, [Rank], "", Reward)
						  end
				  end, FirstNList).

update_roleRankInfo(RoleID, RoleName, Mark)->
	case Mark >= data_lucky_roll:get(rank_base_mark) of
		true ->
			Ranker=#rankerInfo{rank=0,roleID=RoleID, roleName=RoleName, mark=Mark},
			OldRankList = get_curRank(),
			{NewRank, NewRankList} =  insert_rank(Mark, Ranker,OldRankList),
			NewRankList2 = update_rankList(NewRankList),
			{Rank_FirstN_Num,_} = data_lucky_roll:get(rank_reward), 
			set_curRank( NewRankList2),
			if NewRank =< Rank_FirstN_Num ->
				   refresh_firstN( NewRankList2),
				   insert_ets_self_info(RoleID,RoleName,Mark,NewRank,Rank_FirstN_Num);
			   true ->
				   ignore
			end;
		_ ->
			ignore
	end.

refresh_firstN( RankList) ->
	{Rank_FirstN_Num,RewardList} = data_lucky_roll:get(rank_reward), 
	FirstNList = lists:sublist(RankList, Rank_FirstN_Num),
	FirstNInfoList = lists:foldr(fun(Rank,Acc)->
										 {_,Reward} = lists:keyfind(Rank, 1, RewardList),
%%										 One=
%%											 case lists:keyfind(Rank, #rankerInfo.rank, FirstNList) of
%%												 false ->
%%													 #p_luckyRoll_ranker{type=2,roleName="", mark=0,rankNum=Rank
%%																		 ,rewardInfo=activity_server:sell_reward2p_reward_info(Reward)};
%%												 #rankerInfo{mark=Mark,roleName=Name}->
%%													 #p_luckyRoll_ranker{type=1,roleName=Name, mark=Mark,rankNum=Rank
%%																		 ,rewardInfo=activity_server:sell_reward2p_reward_info(Reward)}
%%											 end,
%%
%%										 [One|Acc]
											 case lists:keyfind(Rank, #rankerInfo.rank, FirstNList) of
												 false ->
														Acc;
												 #rankerInfo{mark=Mark,roleName=Name}->
													 [#p_luckyRoll_ranker{type=1,roleName=Name, mark=Mark,rankNum=Rank
																		 ,rewardInfo=activity_server:sell_reward2p_reward_info(Reward)}|Acc]
											 end
										 end,[], lists:seq(1, Rank_FirstN_Num)),
	insert_ets({firstNRank,FirstNInfoList}),
	erlang:put(?rank_firstN, FirstNInfoList).

get_firstN() ->
	case erlang:get(?rank_firstN) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end.

update_rankList(NewRankList)->
	{_,L}=lists:foldl(fun(Ranker, {Rank,RankerList})->{Rank+1,[Ranker#rankerInfo{rank=Rank}|RankerList]} end, {1,[]}, NewRankList),
	lists:reverse(L).

persist_terminate(State)->
		Dict = lists:filter(fun({Key, Val})->is_persist({Key, Val}) end,erlang:get()),
		Info = {State, Dict},
		db_sql:set_etc(?DB_ETC_KEY_ACTIVITY_LUCKY, Info).

do_check_activity_end(State) ->
	NowSec = util:now(),
	lists:foreach(fun(#data_activity_lucky{endTime=EndTime,stopTime=StopTime})->
						  if NowSec >= EndTime ->
								 clear_info();
							 NowSec >= StopTime andalso NowSec =< StopTime + 200 -> %%控制同步消息的次数 
								 broadcast_server:bc(#sc_luckyRoll_change_state{});
							 true ->
								 ignore
						  end
				  end, State),
	init_state().

clear_info()->
	end_activity_and_send_reward(),
	erlang:erase(?rank_firstN),
	erlang:erase(?curRank),
	erlang:erase(?get_reward),
	refresh_firstN([]),
	ok.

init_activity_info()->
	{State, Dict}=
		case db_sql:get_etc(?DB_ETC_KEY_ACTIVITY_LUCKY) of
			[] ->
				{[#data_activity_lucky{activityID=0,startTime=0,stopTime=0,endTime=0}],[]};
			{A,B}->
				{A,B}
		end,
	lists:foreach(fun({Key, Val})->
						  erlang:put(Key, Val)
				  end, Dict),
	insert_ets({firstNRank,get_firstN()}),
	{MaxRank,_} = data_lucky_roll:get(rank_reward), 
	lists:foreach(fun(#rankerInfo{mark=Mark,roleID=RoleID,roleName=RoleName,rank=Rank})->
						  insert_ets_self_info(RoleID,RoleName,Mark,Rank,MaxRank)
						  end, get_curRank()),
	State.

insert_rank(Mark, Ranker,OldRankList)->
	case lists:keyfind(Ranker#rankerInfo.roleID, #rankerInfo.roleID,OldRankList) of
		false ->
			insert_rank3(Mark, Ranker, OldRankList,[],0);
		_ ->
			OldRankList2=lists:keydelete(Ranker#rankerInfo.roleID,#rankerInfo.roleID,OldRankList),
			insert_rank(Mark,Ranker, OldRankList2)
	end.

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
init_state() ->
	Now = util:now(),
	lists:foldl(fun(ID, Acc) ->
						#data_activity_lucky{startTime=StartTime,stopTime=StopTime, endTime=EndTime} = DataActivity =  data_activity_lucky:get(ID),
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
							   [DataActivity#data_activity_lucky{startTime=util:datetime_to_seconds(StartTime2)
																 ,stopTime=util:datetime_to_seconds(StopTime2)
																 ,endTime=util:datetime_to_seconds(EndTime2)}];
						   true ->
                                Acc
						end
				end , [#data_activity_lucky{activityID=0,startTime=0,stopTime=0,endTime=0}], data_activity_lucky:get_list()).

do_clear_state_info(RunningInfo,State)->
    %% 已经关闭了的活动
	lists:foreach(fun(#data_activity_lucky{activityID=ActivityID})->
						  case lists:keyfind(ActivityID, #data_activity_lucky.activityID, State) of
							  false ->
								  clear_info();
							  _ ->
								  ignore
						  end
				  end, RunningInfo),
    %% 新开的活动
	lists:foreach(fun(#data_activity_lucky{activityID=ActivityID})->
						  case lists:keyfind(ActivityID, #data_activity_lucky.activityID, RunningInfo) of
							  false ->
								  clear_info();
							  _ ->
								  ignore
						  end 
				  end, State).



