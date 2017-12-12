%% @author caohongyang
%% @doc 华容道活动进程
%% Created 2013-5-10



-module(hron_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-export([start_link/0,start/0]).

-define(RANK_LIST_NUM, 10).%排行帮的玩家数量
-define(STAR_LIST,	[1,2,3,4]).% 四个难度星级
-define(DUMP_INTERVAL_SECS, (5 * 60)). %% 定时dump当前排行榜的数据的时间间隔
%% ===================Dict Key Begin =========================
-define(curRank, curRank).% 所有玩家的排行榜 
-define(first10, first10).% 前10名列表
-define(lastRank,lastRank). % 昨天的排行榜信息二进制协议,#sc_hron_last_rank_list{}
%% ===================Dict Key End   =========================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0, k/0]).

i() ->
	gen_server:call(?MODULE, i).

k() ->
	c:l(?MODULE),
	user_default:lc(data_hron),
	user_default:kill(?MODULE).


call_get_rank(RoleID) ->
	gen_server:call(?MODULE, {call_get_rank, RoleID}).

cast_update_score(RoleID, Name, Star, Score, DoSync) ->
	erlang:send(?MODULE, {cast_update_score, RoleID, Name, Star, Score, DoSync}).

cast_rank_sync(RoleID, Star) ->
	erlang:send(?MODULE, {cast_rank_sync,RoleID, Star}).

is_open() ->
	case ets:lookup(?ETS_HRON, openFlag) of
		[{openFlag, true}] ->
			true;
		[{openFlag, OpenTime}] ->
			{false, OpenTime};
		_ ->
			% 容错判断
			{false, cacl_open_time()}
	end.

set_open() ->
	ets:insert(?ETS_HRON, {openFlag, true}).

set_close(OpenTime) ->
	ets:insert(?ETS_HRON, {openFlag, OpenTime}).

%% 容错使用的函数
cacl_open_time() ->
	StartTime = data_hron:get(start_time),
	%StopTime = data_hron:get(stop_time),
	NowTime = erlang:time(),
	Date = erlang:date(),
	OpenTime = util:datetime_to_seconds({Date, StartTime}),
	if NowTime >= StartTime ->
		   OpenTime + ?ONE_DAY_SECONDS;
	   true ->
		   OpenTime
	end.
		   

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
	init_state(),
	init_last_info(),
    erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
	{ok, state}.

time_diff({A,B,C},{E,F,G}) ->
	(E-A)*3600+(F-B)*60+(G-C).

init_state() ->
	StartTime = data_hron:get(start_time),
	StopTime = data_hron:get(stop_time),
	NowTime = erlang:time(),
	if StartTime > NowTime ->
		   BeginTime = util:datetime_to_seconds({erlang:date(),StartTime}),
		   SecTillBegin = time_diff(NowTime, StartTime),
		   erlang:send_after(SecTillBegin*1000, self(), set_state_open),
		   set_close(BeginTime);
	   NowTime >= StopTime ->
		   BeginTime = util:datetime_to_seconds({erlang:date(),StartTime})+?ONE_DAY_SECONDS,
		   SecTillBegin = time_diff(NowTime, StartTime)+?ONE_DAY_SECONDS,
		   erlang:send_after(SecTillBegin*1000, self(), set_state_open),
		   set_close(BeginTime);
	   true ->
		   SecTillEnd = time_diff(NowTime, StopTime),
		   erlang:send_after(SecTillEnd*1000, self(), set_state_close),
		   set_open()
	end.
	
	


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
        
handle_info(set_state_open, State) ->
	set_open(),
	StopTime = util:datetime_to_seconds({erlang:date(),data_hron:get(stop_time)}),
	NowSec = util:now(),
	SecTillEnd = StopTime-NowSec,
	erlang:send_after(SecTillEnd*1000, self(), set_state_close),
	{noreply, State};
handle_info(set_state_close, State) ->
	NextOpenTime = cacl_open_time(),
	NowSec = util:now(),
	set_close(NextOpenTime),
	SecTillBegin = NextOpenTime-NowSec,
	erlang:send_after(SecTillBegin*1000, self(), set_state_open),
	%% 等10秒，等所有玩家的战斗都计算完成，再进行本次活动的结算
	erlang:send_after(10*1000, self(), cacl_hron_end),
	{noreply, State};
handle_info(cacl_hron_end, State) ->
	do_end_hron(),
	{noreply, State};
handle_info({client_msg, RoleID, #cs_hron_last_rank_list{star=Star}}, State) ->
	sc_last_rank(RoleID, Star),
	{noreply, State};
handle_info({client_msg, RoleID, #cs_hron_cur_rank_list{star=Star}}, State) ->
	sc_cur_rank(RoleID, Star),
	{noreply, State};
%% handle_info({clieng_msg, RoleID, #cs_hron_rank{}}, State) ->
%% 	get_curRank(
handle_info({cast_update_score, RoleID, Name, Star, Score, DoSync}, State) ->
	case is_open() of
		true ->
			OldRankList = get_curRank(Star),
			{NewRank, NewRankList} = insert_rank(RoleID, Score, Name, Star, OldRankList),
			set_curRank(Star, NewRankList),
			if DoSync =:= true ->
				   ?unicast(RoleID, #sc_hron_rank{bestScore=Score, rank=NewRank});
			   true ->
				   ignore
			end;
		_ ->
			ignore
	end,
	{noreply, State};
handle_info({cast_rank_sync, RoleID, Star}, State) ->
	RankList = get_curRank(Star),
	{Rank, Score} = get_rank(RoleID, RankList),
	?unicast(RoleID,#sc_hron_rank{bestScore=Score, rank=Rank}),
	{noreply, State};
handle_info(dump_cur_rank_data, State) ->
    persist_terminate(),
    erlang:send_after(?DUMP_INTERVAL_SECS * 1000, self(), dump_cur_rank_data),
    {noreply, State};
handle_info({update_role_name, RoleID, Name}, State) ->
    %% 遍历今日排行榜
    lists:foreach(fun(Star) ->
                    RankList = get_curRank(Star),
                    case lists:keytake(RoleID, #p_hron_role.roleID, RankList) of
                        false ->
                            ignore;
                        {value, Info, _} ->
                            NewRankList = lists:keyreplace(RoleID, #p_hron_role.roleID, RankList, Info#p_hron_role{roleName=Name}),
                            set_curRank(Star, NewRankList),
                            refresh_first10(Star, NewRankList)
                    end
                  end, [?STAR_LIST]),
    {noreply, State};
handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.

sc_last_rank(RoleID, Star) ->
	?unicast(RoleID, get_lastRank(Star)).

sc_cur_rank(RoleID, Star) ->
	Reply = #sc_hron_cur_rank_list{rankList=get_first10(Star),star=Star},
	?unicast(RoleID, Reply).

-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	persist_terminate(),
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
get_rank(RoleID, RankList) ->
	get_rank(RoleID,RankList, 1).

get_rank(RoleID, [#p_hron_role{roleID=RoleID,score=Score}|_RankList], N) ->
	{N, Score};
get_rank(RoleID, [_|RankList], N) ->
	get_rank(RoleID, RankList, N+1);
get_rank(_RoleID, [], _N) ->
	{-1,0}.

%% 持久化上次排行榜信息
persist_last_info(LastInfoList) ->
	db_sql:set_etc(?DB_ETC_KEY_HRON, LastInfoList).

init_last_info() ->
	case db_sql:get_etc(?DB_ETC_KEY_HRON) of
		[] ->
			ignore;
		LastInfoList when is_list(LastInfoList)->
			lists:foreach(fun({Star,RankList}) ->
								  set_lastRank(Star, RankList);
							 (_) ->
								  ignore
						  end, LastInfoList)
	end,
	case db_sql:get_etc(?DB_ETC_KEY_HRON_TERM) of
		[] ->
			ignore;
		{Date, InfoList} when is_list(InfoList)->
			Today = erlang:date(),
			if Date =/= Today ->
				   ignore;
			   true->
				   lists:foreach(fun({Star,RankList})->
										 set_curRank(Star,RankList),
										 refresh_first10(Star,RankList);
									(_)->
										 ignore end, InfoList)
			end
	end,
	db_sql:del_etc(?DB_ETC_KEY_HRON_TERM).

persist_terminate()->
	InfoList = 
	lists:foldl(fun(Star, Acc) ->
						CurRank = get_curRank(Star),
						[{Star,CurRank}|Acc]
				end, [], ?STAR_LIST),
	Info = {erlang:date(),InfoList},
	db_sql:set_etc(?DB_ETC_KEY_HRON_TERM,Info).

%% 活动结束时的处理
do_end_hron() ->
	%%　给前10名发送奖励
	lists:foreach(fun(Star) -> 
						  First10 = get_first10(Star),
						  DataGold = data_hron:get({gold,Star}),
						  lists:foldl(fun(#p_hron_role{roleID=RoleID}, Rank) ->
											  RewardGold = lists:nth(Rank, DataGold),
											  send_rank_reward(Star, Rank, RoleID, RewardGold),
											  Rank+1
									  end, 1, First10)
				  end, ?STAR_LIST),
	
	lists:foreach(fun(Star) ->
						  RankList = get_curRank(Star),
						  % 发元宝
						  give_reward(),
						  %% 广播第一名
						  bc_first(Star, RankList),
						%% 向所有玩家广播，活动结束和自己的最新信息
						  notify_end(RankList)
				  end, ?STAR_LIST),
	NewLastInfoList = 
	lists:foldl(fun(Star, Acc) ->
						First10 = get_first10(Star),
						set_lastRank(Star, First10),
						[{Star,First10}|Acc]
				end, [], ?STAR_LIST),
	persist_last_info(NewLastInfoList),
	%broadcast_server:bc(#sc_hron_stop{}),
	clear_info().


clear_info() ->
	lists:foreach(fun(Star) ->
						  %% 清除当前信息
						  erlang:erase({?first10,Star}),
						  erlang:erase({?curRank,Star})
				  end, ?STAR_LIST).

give_reward() ->
	ignore.

%% 世界公告第一名
bc_first(_Star, []) ->
	ignore;
bc_first(4, [#p_hron_role{roleName=Name}|_]) ->
	broadcast_server:bc_msgID(10005, [Name]);
bc_first(3, [#p_hron_role{roleName=Name}|_]) ->
	broadcast_server:bc_msgID(10006, [Name]);
bc_first(2, [#p_hron_role{roleName=Name}|_]) ->
	broadcast_server:bc_msgID(10007, [Name]);
bc_first(1, [#p_hron_role{roleName=Name}|_]) ->
	broadcast_server:bc_msgID(10008, [Name]).


%% 发送奖励
send_rank_reward(4, Rank, RoleID, RewardGold) ->
	mail_server:send_sys_mail(RoleID, ?MAIL_HRON_FOUR_STAR, [Rank], "", #sell_reward{gold=RewardGold});
send_rank_reward(3, Rank, RoleID, RewardGold) ->
	mail_server:send_sys_mail(RoleID, ?MAIL_HRON_THREE_STAR, [Rank], "", #sell_reward{gold=RewardGold});
send_rank_reward(2, Rank, RoleID, RewardGold) ->
	mail_server:send_sys_mail(RoleID, ?MAIL_HRON_TWO_STAR, [Rank], "", #sell_reward{gold=RewardGold});
send_rank_reward(1, Rank, RoleID, RewardGold) ->
	mail_server:send_sys_mail(RoleID, ?MAIL_HRON_ONE_STAR, [Rank], "", #sell_reward{gold=RewardGold}).

notify_end(RankList) ->
	notify_end(RankList, 1).

notify_end([#p_hron_role{roleID=RoleID,score=Score}|RankList], N) ->
	?unicast(RoleID,#sc_hron_rank{bestScore=Score,rank=N}),
	notify_end(RankList,N+1);
notify_end([],_)->
	ok.

insert_rank(RoleID, Score, Name, Star, RankList) ->
	Ranker = #p_hron_role{roleID=RoleID,score=Score,roleName=Name},
	{NewRank, NewRankList} = Result =  insert_rank3(Score, Ranker,RankList,[],0),
	if NewRank =< ?RANK_LIST_NUM ->
		refresh_first10(Star, NewRankList);
	   true ->
		   ignore
	end,
	Result.

refresh_first10(Star, RankList) ->
	put({?first10,Star}, lists:sublist(RankList, 10)).

get_first10(Star) ->
	case get({?first10, Star}) of
		RL when is_list(RL) ->
			RL;
		_ ->
			[]
	end.

get_curRank(Star) ->
	case get({?curRank, Star}) of
		RL when is_list(RL) ->
			RL;
		_ ->
			[]
	end.
set_curRank(Star, RankList) ->
	erlang:put({?curRank,Star}, RankList).

get_lastRank(Star) ->
	case get({?lastRank, Star}) of
		Bin when is_binary(Bin) ->
			Bin;
		_ ->
			#sc_hron_last_rank_list{rankList=[],star=Star}
	end.

set_lastRank(Star, Bin) when is_binary(Bin) ->
	put({?lastRank,Star},Bin);
set_lastRank(Star, Record) when is_tuple(Record) ->
	put({?lastRank,Star},iolist_to_binary(proto:encode(Record)));
set_lastRank(Star, List) when is_list(List) ->
	put({?lastRank,Star}, iolist_to_binary(proto:encode(#sc_hron_last_rank_list{rankList=List,star=Star}))).

%% 已存在旧的信息在排行榜
insert_rank3(Score, Ranker, [#p_hron_role{score=Score2}=H|RankList]=E,Tail,N) ->
	if Score2 >= Score ->
	insert_rank3(Score, Ranker, RankList, [H|Tail],N+1);
	   Score2 < Score ->
		   %% 删除旧的信息
		   NewRankList = lists:reverse(Tail,[Ranker|lists:keydelete(Ranker#p_hron_role.roleID, #p_hron_role.roleID, E)]),
		   {N + 1, NewRankList}
	end;	   
insert_rank3(_Score, Ranker, [],Tail, N) ->
	NewRankList = lists:reverse([Ranker|Tail]),
	{N+1, NewRankList}.
	
