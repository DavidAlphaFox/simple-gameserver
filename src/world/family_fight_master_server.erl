%% @author lixinglong
%% @doc @todo family_fight_master.


-module(family_fight_master_server).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(dumpDictInterval, 300).
-define(check_tick_interval, 300).
-define(wait_more_info_interval,350).
-define(sync_common_node_info_interval,300).
-define(group_lists_info,group_lists_info).
-define(is_rank,is_rank).
-define(is_sync,is_sync).
-define(firstN_list,firstN_list).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start() ->
    {ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


i() ->
	gen_server:call(?MODULE, i).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {period=0,startTime=0,prepareEndTime=0,randomEndTime=0,fightEndTime=0,periodEndTime=0}).

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
	case data_setting:get(server_type) == familyfight of
		true ->
			process_flag(trap_exit,true),
			State =
				case db_sql:get_etc(?DB_ETC_KEY_FAMILYFIGHTMASTER) of
					[] ->
						#state{};
					{StateT,ListT} ->
						check_state_and_init_dict(StateT, ListT),
						StateT
				end,
			do_check(State),
			check_tick(),
			dump_tick(),
			{ok,State};
		_ ->
			?ERR("family_fight_master_server not started,Reason:not family_fight_maser_node"),
			ignore
	end.

check_state_and_init_dict(#state{prepareEndTime=PrepareEndTime}, List)->
	case util:now() < PrepareEndTime of
		true ->
			lists:foreach(fun({K,V})-> put(K,V) end, List);
		false ->
			lists:foreach(fun({?group_lists_info,V})->
								  case V of
									  [] ->
										  put(?group_lists_info,V);
									  _ ->
										  case erlang:element(1, hd(V)) of
											  pf_fighter ->
												  put(?group_lists_info,V);
											  _ ->
												  put(?group_lists_info,V),
												  do_random_match_fighters()
										  end
								  end;
							 ({K ,V}) ->
								  put(K,V)
						  end, List)
	end.

check_tick()->
	erlang:send_after(1000 * ?check_tick_interval, self(), check_tick).

dump_tick()->
	erlang:send_after(1000 * ?dumpDictInterval, self(), do_dump).

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
handle_call(i, _From, State) ->
	{reply, State, State};
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
handle_cast({test_sync_info,FamilyID}, #state{startTime=ST,prepareEndTime=PT,randomEndTime=RT,fightEndTime=FT,periodEndTime=ET}=State) ->
    send_msg_to_server(FamilyID,{test_sync_info,ST,PT,RT,FT,ET}),
    {noreply, State};
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
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(Info, State) when is_binary(Info) ->
    case catch do_handle_info(binary_to_term(Info), State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end;
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
		{noreply, _, hibernate}=ReturnState->
			ReturnState;
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w", [Exeption, Info, State]),
            {noreply, State}
    end.


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
	persist_data(State),
	?ERR("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
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

%%%%%%%%% gameserver 发现需要重连的时候,自己给master请求初始化信息..gameserver挂掉没任何影响,消息会在mq中
%% master 发现需要重连的时候,广播给gameserver,gameserver自己请求具体状态信息
do_handle_info(notice_request_status, State)->
	send_all_server(request_master_status),
	{noreply,State};

do_handle_info(check_tick, State)->
	check_tick(),
	do_check(State),
	{noreply, State};

%% 更新报名信息
do_handle_info({update_sign_info, ServerID,InfoList}, #state{prepareEndTime=PT,startTime=ST}=State)->
    %%?INFO("update_sign_info(ServerID):~w,InfoList=~w",[ServerID,InfoList]),
	Now= util:now(),
	if Now > ST andalso Now =< PT + ?wait_more_info_interval ->
		   update_sign_info({ServerID, InfoList});
	   true ->
		   ignore
	end,
	{noreply, State};

do_handle_info(random_match_fighters, State)->
	do_random_match_fighters(),
	{noreply, State};

%% 更新为新一届的state时间状态
do_handle_info({update_period_info, {P2,ST2,PT2,RT2,FT2,ET2}},State)->
	State2 = State#state{period=P2,startTime=ST2,prepareEndTime=PT2,randomEndTime=RT2,fightEndTime=FT2,periodEndTime=ET2},
	set_group_lists([]),
	clear_rank_sign(),
	clear_sync_result(),
	{noreply, State2};

do_handle_info(rank_fighters, State)->
	do_rank(),
	{noreply,State};

do_handle_info(sync_period_result,State)->
    ?INFO("sync_period_result:~w",[State]),
	case sync_period_result() of
        List when is_list(List) ->
            erlang:send(all_family_rank_server, {family_fight_updete_score, List});
        _ ->
            ignore
    end,
	{noreply, State};

do_handle_info({update_family_star_info, FamilyID,Star}, State)->
	update_family_win_star(FamilyID,Star),
	{noreply, State};

do_handle_info({update_family_win_star, AttackFamilyID,FamilyWinStar}, State)->
	refresh_family_star(AttackFamilyID,FamilyWinStar),
	{noreply, State};

do_handle_info(do_dump,State)->
	dump_tick(),
	InfoList = {State, lists:filter(fun({?group_lists_info,_})->true ;
									   ({?is_rank,_}) -> true;
									   ({?is_sync,_}) -> true;
									   ({?firstN_list,_}) -> true;
									   (_)->false
									end, erlang:get())},
	db_sql:set_etc(?DB_ETC_KEY_FAMILYFIGHTMASTER,InfoList),
	erlang:garbage_collect(),
	{noreply, State, hibernate};
	
do_handle_info({get_familyfight_dtl,IDList,ServerID}, State)->
	#state{period=P,startTime=ST,prepareEndTime=PT,randomEndTime=RT,fightEndTime=FT,periodEndTime=ET}=State,
	NodeFighterInfo= get_node_fighter_info(ServerID,IDList,State),
	send_msg_to_server(ServerID,{get_family_fight_dtl_back, {P,ST,PT,RT,FT,ET},NodeFighterInfo}),
	{noreply,State};

do_handle_info({disband, FamilyID}, State)->
	clean_family_fight_master_info(FamilyID),
	{noreply, State};

do_handle_info({get_firstN_list,ServerID}, State)->
	do_get_firstN_list(ServerID),
	{noreply,State};

do_handle_info({family_changed,FamilyID,OwnerName,FamilyLevel},State)->
	do_family_changed(FamilyID,OwnerName,FamilyLevel),
	{noreply,State};

do_handle_info({update_family_name, _ServerID, SelfFamilyID, NewName}, State) ->
    GroupList = get_group_lists(),
    case lists:keytake(SelfFamilyID, #pf_fighter.familyID, GroupList) of
        false -> 
            ignore;
        {value,#pf_fighter{familyID=OtherFamilyID,serverID=ServerID}=Info,Other} ->
            NewInfo = Info#pf_fighter{familyName=NewName},
            NewList = [NewInfo|Other],
            send_msg_to_server(ServerID, {update_match_family_name, OtherFamilyID, SelfFamilyID, NewName}),
            set_group_lists(NewList)
    end,
    {noreply, State};

do_handle_info({set_state,#state{}=State2},_State)->
	send_all_server({change_state_info, State2}),
	{noreply, State2};

do_handle_info({set_state2,#state{}=State2},_State)->
	send_all_server({change_state_info, State2}),
	do_check(State2),
	{noreply, State2};

do_handle_info({test_play, ID}, State)->
	test_play(ID),
	{noreply, State};

do_handle_info(Info,State)->
	?ERR("can't handle message:~w State:~w",[Info,State]),
	{noreply, State}.

%% ================== util functions ====================
persist_data(State)->
	InfoList = {State, lists:filter(fun({?group_lists_info,_})->true ;
					   ({?is_rank,_}) -> true;
					   ({?is_sync,_}) -> true;
					   ({?firstN_list,_}) -> true;
									   (_)->false
									end, erlang:get())},
	db_sql:set_etc(?DB_ETC_KEY_FAMILYFIGHTMASTER,InfoList).

clean_family_fight_master_info(FamilyID)->
	db_sql:clean_family_fight_master_info(FamilyID).
	
get_node_fighter_info(_ServerID,IDList,#state{prepareEndTime=PT})->
	Now=util:now(),
	GroupList = get_group_lists(),
	if Now < PT  ->
		   lists:foldl(fun(E,Acc) ->
							   case lists:keyfind(E, 1, GroupList) of 
								   {_,List} ->
									   List ++ Acc;
								   _ ->
									   Acc
							   end
					   end, [], IDList);
	   true ->
		   case GroupList of
			   [] ->
				   [];
			   _ ->
				   lists:filter(fun(E) -> lists:member( E#pf_fighter.serverID,IDList) end, GroupList)
		   end
	end.

	
update_family_win_star(AttackFamilyID,WinStar)->
	GroupLists = get_group_lists(),
	case lists:keytake(AttackFamilyID, #pf_fighter.familyID, GroupLists) of
		false ->
			?ERR("update family win star error with familyID = ~w and winStar = ~w", [AttackFamilyID,WinStar]);
		{value, #pf_fighter{star=Star}=Group,OtherGroups}->
			set_group_lists([Group#pf_fighter{star=Star+WinStar}|OtherGroups])
	end.

refresh_family_star(AttackFamilyID,FamilyWinStar)->
	GroupLists = get_group_lists(),
	case lists:keytake(AttackFamilyID, #pf_fighter.familyID, GroupLists) of
		false ->
			?ERR("update family star error with familyID = ~w and star = ~w",[AttackFamilyID, FamilyWinStar]);
		{value, #pf_fighter{star=Star} = Group, OtherGroups} ->
			if Star > FamilyWinStar ->
				   ?ERR("update family star error while familyID = ~w update Star = ~w to winStar = ~w",[AttackFamilyID,Star,FamilyWinStar]);
			   Star =:= FamilyWinStar ->
				   ignore;
			   Star < FamilyWinStar ->
				   set_group_lists([Group#pf_fighter{star=FamilyWinStar}|OtherGroups])
			end
	end.

do_check(#state{period=P,startTime=ST,prepareEndTime=PT,randomEndTime=RT,fightEndTime=FT,periodEndTime=ET})->
	Now=util:now(),
	LastTick=util:now() - ?check_tick_interval,
?DEBUG("L-do_check ~w ~w ~w ~w",[ET =< Now,PT =< Now andalso PT > LastTick,RT =< Now andalso RT > LastTick,FT =< Now andalso FT > LastTick]),
	case ST of %% 是不是第一次进入这个流程
		0 ->
			{P2,ST2,PT2,RT2,FT2,ET2} = Info2= calc_first_Info(),
			erlang:send(self(), {update_period_info,Info2}),
			send_all_server({bc_new_period_begin, P2, ST2,PT2,RT2,FT2,ET2});
		_ ->
			if ET =< Now ->     %% 是不是结束
?INFO("次日中午开始报名"),
				   {P2,ST2,PT2,RT2,FT2,ET2} = Info2= calc_new_Info(P),
				   erlang:send(self(), {update_period_info,Info2}),
				   send_all_server({bc_new_period_begin, P2, ST2,PT2,RT2,FT2,ET2});
			   PT =< Now andalso PT > LastTick ->
?ERR("准备匹配"),
				   prepare_to_random_fighters();
			   RT =< Now andalso RT > LastTick ->
				   ignore;
			   FT =< Now andalso FT > LastTick ->
?ERR("准备结算"),
				   prepare_to_rank();
			   true ->
				   ignore
			end
	end.
	
calc_first_Info()->
    ST = util:datetime_to_seconds({data_family_fight:get(fisrst_period_date),data_family_fight:get(start_time)}),
    PT = util:datetime_to_seconds({data_family_fight:get(fisrst_period_date),{23,59,59}}),
    RT = util:datetime_to_seconds({data_family_fight:get(fisrst_period_date),{8,0,0}}) + ?ONE_DAY_SECONDS,
    FT = util:datetime_to_seconds({data_family_fight:get(fisrst_period_date),{23,59,59}}) + ?ONE_DAY_SECONDS,
    ET = util:datetime_to_seconds({data_family_fight:get(fisrst_period_date),data_family_fight:get(start_time)}) + (data_family_fight:get(interval_time)+1)*?ONE_DAY_SECONDS -?check_tick_interval,
    ?ERR("新一轮的时间~w ~w ~w ~w ~w",[util:seconds_to_datetime(ST)
                                 ,util:seconds_to_datetime(PT)
                                 ,util:seconds_to_datetime(RT)
                                 ,util:seconds_to_datetime(FT)
                                 ,util:seconds_to_datetime(ET)]),
	{1,ST,PT,RT,FT,ET}.

%% 计算新的一届
calc_new_Info(P)->
    ST = util:datetime_to_seconds({erlang:date(),data_family_fight:get(start_time)}),
    PT = util:datetime_to_seconds({erlang:date(),{23,59,59}}),
    RT = util:datetime_to_seconds({erlang:date(),{8,0,0}}) + ?ONE_DAY_SECONDS,
    FT = util:datetime_to_seconds({erlang:date(),{23,59,59}}) + ?ONE_DAY_SECONDS,
    ET = util:datetime_to_seconds({erlang:date(),data_family_fight:get(start_time)}) + (data_family_fight:get(interval_time)+1)*?ONE_DAY_SECONDS -?check_tick_interval,
    ?ERR("新一轮的时间~w ~w ~w ~w ~w",[util:seconds_to_datetime(ST)
                                 ,util:seconds_to_datetime(PT)
                                 ,util:seconds_to_datetime(RT)
                                 ,util:seconds_to_datetime(FT)
                                 ,util:seconds_to_datetime(ET)]),
	{P+1,ST,PT,RT,FT,ET}.

prepare_to_rank() ->
	erlang:send_after(?wait_more_info_interval * 1000, self(), rank_fighters).

prepare_to_random_fighters()->
	erlang:send_after(?wait_more_info_interval * 1000, self(), random_match_fighters).

send_all_server(Msg)->
	send_msg:broadcast(family_fight_server,Msg).

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

is_master()->
	case data_family_fight:get(is_master) of
		"1" ->
			true;
		_ ->
			false
	end.

send_msg_to_server(ServerID,Msg) when is_binary(Msg)->
	send_msg_to_server2(ServerID,Msg);
send_msg_to_server(ServerID,Msg) ->
	send_msg_to_server2(ServerID,term_to_binary(Msg)).

send_msg_to_server2(ServerID,Msg) ->
	send_msg:direct(ServerID,family_fight_server, Msg).

%% 读取本届比赛信息
get_group_lists()->
	case erlang:get(?group_lists_info) of
		X when is_list(X)->
			X;
		_ ->
			[]
	end.

%% 保存本届比赛信息
set_group_lists(X) ->
	erlang:put(?group_lists_info,X).

set_rank_sign()->
	put(?is_rank,true).

clear_rank_sign()->
	put(?is_rank, false).

get_rank_status()->
	case get(?is_rank) of
		true ->
			true;
		_ ->
			false
	end.

set_sync_result()->
	put(?is_sync, true).

clear_sync_result()->
	put(?is_sync, false).

get_sync_status()->
	case get(?is_sync) of
		true ->
			true;
		_ ->
			false
	end.

do_rank()->
    ?ERR("开始结算"),
    %% 检查标志位，防止重复排序
	case get_rank_status() of 
		true ->
			ignore;
		false ->
?DEBUG("L-do_rank --11-- get_group_lists()：~w",[get_group_lists()]),
            %% get_group_lists()是取出本届的成员信息,与数据库数据
			GroupListT = calc_score(update_group_lists(get_group_lists())), 
            GroupList = lists:sort(fun(A,B)->
                                    A#pf_fighter.score > B#pf_fighter.score
                             orelse (A#pf_fighter.score =:= B#pf_fighter.score andalso A#pf_fighter.level > B#pf_fighter.level)
                             orelse (A#pf_fighter.score =:= B#pf_fighter.score andalso A#pf_fighter.level =:= B#pf_fighter.level andalso A#pf_fighter.totalFightPower > B#pf_fighter.totalFightPower)
                            end, GroupListT),
            %% 根据排名刷新 rank
            {GroupList2,_} = 
				lists:foldl(fun(E,{Acc,N})->
									{[E#pf_fighter{rank=N}|Acc],N+1}
							end, {[],1}, GroupList),
?DEBUG("L-do_rank --44-- GroupList2：~w",[GroupList2]),
			erlang:send_after(1000 * ?sync_common_node_info_interval ,self(), sync_period_result),
			set_group_lists(GroupList2),
			set_rank_sign(),
			update_firstN_list(GroupList2),
			erlang:spawn(fun()->
								 db_sql:set_family_fight_master_rank_info(GroupList2)
						 end)
	end.

do_family_changed(FamilyID,OwnerName,FamilyLevel)->
	List = get_firstN_list(),
	case lists:keyfind(FamilyID, #p_familyfight_ranker.familyID, List) of
		false ->
			ignore;
		#p_familyfight_ranker{}=Ranker->
			set_firstN_list(lists:keyreplace(FamilyID,#p_familyfight_ranker.familyID,List,Ranker#p_familyfight_ranker{level=FamilyLevel,ownerName=OwnerName}))
	end.

%do rank 时候写进去的是N->1的倒序,update后是1->N的顺序
%% 保存的数据类型为[#p_familyfight_ranker{}]
update_firstN_list(GroupList) ->
	set_firstN_list(build_firstN_info(GroupList)).

build_firstN_info(List)->
	lists:foldl(fun(#pf_fighter{rank=Rank,serverID=SID,familyID=FID,familyName=FN,ownerName=ON,level=L,totalFightPower=TPower,score=Score},Acc)->
						[#p_familyfight_ranker{familyID=FID,serverID=SID,familyName=FN,ownerName=ON,level=L,rank=Rank,total_fight_power=TPower,score=Score}|Acc] end, [], List).

set_firstN_list(List) when is_list(List)->
	put(?firstN_list,List).

get_firstN_list() ->
	case get(?firstN_list) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

do_get_firstN_list(ServerID)->
	send_msg_to_server(ServerID,{update_firstN_list,generate_firstN_list()}).

send_all_server_firstN_list() ->
	send_all_server({update_firstN_list,generate_firstN_list()}).

generate_firstN_list()->
	{_,List} = lists:foldl(fun(Ranker,{Rank,List})->
								   {Rank+1,[Ranker#p_familyfight_ranker{rank=Rank}|List]}
						   end,{1,[]}, lists:sublist(get_firstN_list(), 1,data_family_fight:get(sync_firstN_num))),
	List.

%% 向每个报名的游戏服务器family_fight_server发送{sync_period_result, F}，F是#pf_fighter{}
%% 向所有游戏服务器family_fight_server发送{update_firstN_list,generate_firstN_list()}，family_fight_server只是保存了一份
sync_period_result()->
	case get_sync_status() of
		true ->
			ignore;
		false ->
            NewScore = 
			    lists:foldl(fun(F=#pf_fighter{serverID=SID, familyID=FamilyID, score=Score}, Acc)->
								  case catch send_msg_to_server(SID,{sync_period_result, F}) of
                                      {'EXIT', Reason} ->
                                          ?ERR("sync_period_result fail !! ~w",[Reason]);
                                      _ ->
                                          ignore
                                  end,                                      
                                  [{FamilyID, Score} | Acc]
						    end, [], get_group_lists()),
			send_all_server_firstN_list(),
			set_sync_result(),
            NewScore  
	end.

%% SignGroupList来自于get_group_lists()的返回，报名期间数据结构是{serverid,list},战斗期间就是list。此处如果从报名直接到结算阶段会进程崩溃。
update_group_lists(SignGroupList)->
?INFO("L-update_group_lists SignGroupList:~w",[SignGroupList]),
	AllGroupLists = db_sql:get_family_fight_master_rank_info(),
?INFO("L-update_group_lists AllGroupLists:~w",[AllGroupLists]),
    lists:foldl(fun(#pf_fighter{familyID=FamilyID}=Info,Acc) ->
						case lists:keytake(FamilyID,#pf_fighter.familyID,Acc) of
                            {value, #pf_fighter{score=Score,rank=Rank}, Others} ->
                                [Info#pf_fighter{score=Score,lastRank=Rank}|Others];
							false ->
								[Info|Acc]
						end
				end,AllGroupLists,SignGroupList).

calc_score(GroupList)->
	WinScore = data_family_fight:get(winScore),
	EqScore = data_family_fight:get(eqScore),
	LoseScore = data_family_fight:get(loseScore),
	calc_score(GroupList, [],WinScore,EqScore,LoseScore).

calc_score([],L,_,_,_)->
	L;
calc_score([H|T], L,WinScore, EqScore,LoseScore) when H#pf_fighter.pareID == 0 ->
	calc_score(T, [H|L], WinScore,EqScore,LoseScore);
calc_score([#pf_fighter{star=HStar,score=HScore,result=_Result}=H|T],L,WinScore,EqScore,LoseScore)->
	case lists:keytake(H#pf_fighter.pareID, #pf_fighter.pareID, T) of
		false ->
%% 			case _Result of
%% 				1 ->
					calc_score(T,[H#pf_fighter{score=HScore}|L],WinScore,EqScore,LoseScore);
%% 				_ ->
%% 					calc_score(T,[H|L], WinScore,EqScore,LoseScore)
%% 			end;
		{value,P=#pf_fighter{star=PStar,score=PScore},T2}->            
			{HScore2,PScore2,RH,RP} =
				if HStar > PStar ->
					   {HScore+(WinScore*(HStar - PStar)),max (0,PScore-(LoseScore*(HStar - PStar))),1,2};
				   HStar < PStar ->
                       {max(0,(HScore-(LoseScore*(PStar-HStar)))),PScore+(WinScore*(PStar-HStar)),2,1};
				   true ->
                       {HScore,PScore,3,3}
				end,
			calc_score(T2,[H#pf_fighter{score=HScore2,matchStar=PStar,result=RH},P#pf_fighter{score=PScore2,matchStar=HStar,result=RP}|L]
					  ,WinScore,EqScore,LoseScore)
	end.

do_random_match_fighters()->
    ?INFO("开始匹配，请等待 ~w",[get_group_lists()]),
    SortGroupLists = lists:sort(fun(A,B)->
                                    if
                                        A#pf_fighter.score  > B#pf_fighter.score ->
                                            true;
                                        A#pf_fighter.score < B#pf_fighter.score ->
                                            false;
                                        A#pf_fighter.level  > B#pf_fighter.level ->
                                            true;
                                        A#pf_fighter.level < B#pf_fighter.level ->
                                            false;
                                        A#pf_fighter.totalFightPower  > B#pf_fighter.totalFightPower ->
                                            true;
                                        A#pf_fighter.totalFightPower < B#pf_fighter.totalFightPower ->
                                            false;
                                        true ->
                                            true
                                    end
                        end, lists:foldl(fun({_,List},Acc)->List ++ Acc end, [], get_group_lists())),
    RandomGroupListsByZone = random_zone(SortGroupLists),
    ?INFO("开始匹配，请等待 SortGroupLists:~w RandomGroupListsByZone:~w",[SortGroupLists,RandomGroupListsByZone]),
%% 	GroupLists = generate_group_list(generate_init_group_list(),get_group_lists()),
	GroupLists2 = match_grouplists(RandomGroupListsByZone),
	set_group_lists(GroupLists2),
    ?INFO("匹配结束，开始同步 ~w",[GroupLists2]),
	lists:foreach(fun(E) ->
    ?INFO("匹配结束，同步:~w ",[E]),
                          send_msg_to_server(E#pf_fighter.serverID,{random_match_fighters, E})
                            end, GroupLists2),
    ?INFO("匹配结束，同步完毕 ~w",[GroupLists2]).

random_zone(SortGroupLists)->
    random_zone(SortGroupLists,[]).
random_zone(SortGroupLists,Res)->
    Length = length(SortGroupLists),
    ZoneSize = data_family_fight:get(zoneSize),
    if
        Length > ZoneSize ->
            {FrontList,BackList} = lists:split(ZoneSize, SortGroupLists),
            random_zone(BackList,util:random_list(FrontList) ++Res);
        true->
            util:random_list(SortGroupLists) ++Res
    end.

match_grouplists(SortGroupLists)->
    match_grouplists(SortGroupLists,[],?familyfight_group_base+1,length(SortGroupLists)).
match_grouplists([#pf_fighter{serverID=F1SID,familyID=F1FID,familyName=F1NM,rank=F1RK,totalFightPower=F1FP}=F1
             ,#pf_fighter{serverID=F2SID,familyID=F2FID,familyName=F2NM,rank=F2RK,totalFightPower=F2FP}=F2|T],Res,PareCnt,Length) when (Length rem 2) == 0 andalso Length > 1->
    match_grouplists(T,[F1#pf_fighter{pareID=PareCnt,matchFamilyID=F2FID,matchServerID=F2SID,matchFamilyName=F2NM,matcherRank=F2RK,matcherTotalFightPower=F2FP}
                           ,F2#pf_fighter{pareID=PareCnt,matchFamilyID=F1FID,matchServerID=F1SID,matchFamilyName=F1NM,matcherRank=F1RK,matcherTotalFightPower=F1FP}|Res],PareCnt+1,Length-2);
match_grouplists([LastOne|T],Res,PareCnt,Length) when (Length rem 2) == 1->
    ?INFO("L-match_grouplists aa LastOne:~w",[LastOne]),
    match_grouplists(T,[LastOne#pf_fighter{pareID=PareCnt,result=1}|Res],PareCnt+1,Length-1);
match_grouplists([],Res,_,_)->
    Res.

random_match_grouplists(GroupLists)->
	{GroupLists2,_,_,_} = 
		lists:foldl(fun({S,List}, {Acc,true,_,Length})->
							%{RandomList,IsPare,Fighter} = random_match(util:random_list2(List),S * ?familyfight_group_base + 1, Acc,Length),
							{RandomList,IsPare,Fighter} = random_match(lists:keysort(#pf_fighter.totalFightPower, List),S * ?familyfight_group_base + 1, Acc,Length),
							{RandomList,IsPare,Fighter,Length-1};
					   ({S,List}, {Acc,false, Fighter,Length}) ->
							%{RandomList,IsPare,Fighter2} = random_match(util:random_list2([Fighter|List]), S*?familyfight_group_base + 1,Acc,Length),
							{RandomList,IsPare,Fighter2} = random_match(lists:keysort(#pf_fighter.totalFightPower,[Fighter|List]), S*?familyfight_group_base + 1,Acc,Length),
							{RandomList,IsPare,Fighter2,Length-1}
					end, {[],true,0,length(GroupLists)}, lists:reverse(lists:keysort(1,GroupLists))),
	GroupLists2.

%% random_match([],_,L,_) ->
%% 	{L,true,0};
%% random_match([#pf_fighter{serverID=F1SID,familyID=F1FID,familyName=F1NM,rank=F1RK,totalFightPower=F1FP}=F1
%% 			 ,#pf_fighter{serverID=F2SID,familyID=F2FID,familyName=F2NM,rank=F2RK,totalFightPower=F2FP}=F2|T],Cnt,L,Length)->
%% 	random_match(T, Cnt+1, [F1#pf_fighter{pareID=Cnt,matchFamilyID=F2FID,matchServerID=F2SID,matchFamilyName=F2NM,matcherRank=F2RK,matcherTotalFightPower=F2FP}
%% 						   ,F2#pf_fighter{pareID=Cnt,matchFamilyID=F1FID,matchServerID=F1SID,matchFamilyName=F1NM,matcherRank=F1RK,matcherTotalFightPower=F1FP}|L],Length);
%% random_match([F1|_T], Cnt, L,1)->
%% 	{[F1#pf_fighter{pareID=Cnt,result=1}|L],false,0};
%% random_match([F1|_T], _, L,_)->
%% 	{L,false,F1}.
	
random_match([],_,L,_) ->
	{L,true,0};
random_match([#pf_fighter{serverID=F1SID,familyID=F1FID,familyName=F1NM,rank=F1RK,totalFightPower=F1FP}=F1
			 ,#pf_fighter{serverID=F2SID,familyID=F2FID,familyName=F2NM,rank=F2RK,totalFightPower=F2FP}=F2|T],Cnt,L,Length)->
	random_match(T, Cnt+1, [F1#pf_fighter{pareID=Cnt,matchFamilyID=F2FID,matchServerID=F2SID,matchFamilyName=F2NM,matcherRank=F2RK,matcherTotalFightPower=F2FP}
						   ,F2#pf_fighter{pareID=Cnt,matchFamilyID=F1FID,matchServerID=F1SID,matchFamilyName=F1NM,matcherRank=F1RK,matcherTotalFightPower=F1FP}|L],Length);
random_match([F1|_T], Cnt, L,1)->
	{[F1#pf_fighter{pareID=Cnt,result=1}|L],false,0};
random_match([F1|_T], _, L,_)->
	{L,false,F1}.
	
generate_group_list(InitList,Fighters)->
	lists:foldl(fun(#pf_fighter{score=S}=Info,Acc)->
						{S2,_} = data_family_fight_split:get(S),
						{value, {S2,S2ScoreList},OtherList}=lists:keytake(S2, 1, Acc),
						[{S2, [Info|S2ScoreList]}|OtherList]
				end, InitList, lists:foldl(fun({_,List},Acc)->List ++ Acc end, [], Fighters)).

generate_init_group_list()->
	ScoreList = data_family_fight_split:get_list(),
	lists:foldl(fun(Score,Acc)-> [{Score,[]}|Acc] end, [], ScoreList).	

update_sign_info({ServerID, _}=Info)->
	GroupLists = get_group_lists(),
	GroupLists2 = 
		case lists:keytake(ServerID, 1, GroupLists) of
			false ->
				[Info|GroupLists];
			{value,_,Other} ->
				[Info|Other]
		end,
	set_group_lists(GroupLists2).  


test(1) ->
	GroupList =  [{5,[#pf_fighter{familyID = 5000016,totalFightPower=1000},
                        #pf_fighter{familyID = 6000008,totalFightPower=1000},
                        #pf_fighter{familyID = 6000007,totalFightPower=1000}]}],
	set_group_lists(GroupList);
test(2) ->
	GroupList= [{5,
                       [#pf_fighter{familyID = 4000016,totalFightPower=5000},
						#pf_fighter{familyID = 5000008,totalFightPower=2000},
						#pf_fighter{familyID = 6000007,totalFightPower=500}]}
			   , {9,
                       [#pf_fighter{familyID = 7000016,totalFightPower=8000},
						#pf_fighter{familyID = 8000008,totalFightPower=30},
						#pf_fighter{familyID = 9000007,totalFightPower=4800}]}],
	set_group_lists(GroupList);
test(3) ->
	GroupList =  [{5,[#pf_fighter{familyID = 7000016,totalFightPower=500}]}],
	set_group_lists(GroupList);
test(4) ->
	GroupList =  [],
	set_group_lists(GroupList);
test(5) ->
	GroupList= [{5,
                       [#pf_fighter{familyID = 4000016,score=20,totalFightPower=1000},
						#pf_fighter{familyID = 5000008,totalFightPower=500},
						#pf_fighter{familyID = 6000007,totalFightPower=600}]}
			   , {9,
                       [#pf_fighter{familyID = 7000016,totalFightPower=900},
						#pf_fighter{familyID = 8000008,totalFightPower=300},
						#pf_fighter{familyID = 9000007,totalFightPower=1500}]}],
	set_group_lists(GroupList);
test(6) ->
	GroupList= [{5,
                       [#pf_fighter{familyID = 4000016,score=20,totalFightPower=5000},
						#pf_fighter{familyID = 5000008,totalFightPower=1900},
						#pf_fighter{familyID = 6000007,totalFightPower=700}]}
			   , {9,
                       [#pf_fighter{familyID = 7000016,totalFightPower=500},
						#pf_fighter{familyID = 8000008,score=25,totalFightPower=5},
						#pf_fighter{familyID = 9000007,totalFightPower=400}]}],
	set_group_lists(GroupList);
test(7) ->
	GroupList= [{5,
                       [#pf_fighter{familyID = 4000016,score=20,totalFightPower=100},
						#pf_fighter{familyID = 5000008,score=40,totalFightPower=500},
						#pf_fighter{familyID = 6000007,score=60,totalFightPower=500}]}
			   , {9,
                       [#pf_fighter{familyID = 7000016,score=80,totalFightPower=10},
						#pf_fighter{familyID = 8000008,score=100,totalFightPower=50},
						#pf_fighter{familyID = 9000007,totalFightPower=600000}]}],
	set_group_lists(GroupList);

test(8) ->
	[#pf_fighter{familyID = 4000016,star=20,pareID=1},
						#pf_fighter{familyID = 5000008,star=20,pareID=1},
						#pf_fighter{familyID = 6000007,star=0,pareID=0}];
test(9) ->
	[#pf_fighter{familyID = 4000016,star=20,pareID=1},
						#pf_fighter{familyID = 5000008,star=30,pareID=1},
						#pf_fighter{familyID = 6000007,star=0,pareID=0}];
test(10) ->
	[#pf_fighter{familyID = 4000016,star=40,pareID=1},
						#pf_fighter{familyID = 5000008,star=20,pareID=1},
						#pf_fighter{familyID = 6000007,star=0,pareID=0}].

test_match_random(N)->
    ServerID1 = 5,
    ServerID2 = 9,
    GroupList1 = lists:foldr(fun(X,Acc)->[
                                          #pf_fighter{
                                                      serverID = ServerID1
                                                     ,familyName=lists:flatten(io_lib:format("TF~p",[8800+X]))
                                                     ,familyID = (ServerID1*1000000)+X
                                                     ,score=util:random_int(1000,3000+(1000*X))
                                                     ,totalFightPower=util:random_int(10000,30000+(10000*X))
                                                     }|Acc]
                            end,[],lists:seq(1,N+1)),

    GroupList2 = lists:foldr(fun(X,Acc)->[
                                          #pf_fighter{
                                                      serverID = ServerID2
                                                     ,familyName=lists:flatten(io_lib:format("TF~p",[8800+X]))
                                                     ,familyID = (ServerID2*1000000)+X
                                                     ,score=util:random_int(1000,3000+(1000*X))
                                                     ,totalFightPower=util:random_int(10000,30000+(10000*X))
                                                     }|Acc]
                            end,[],lists:seq(1,N)),

    
    GroupList= [{ServerID1,
                       GroupList1}
               , {ServerID2,
                       GroupList2}],
    set_group_lists(GroupList),
    GroupList.

test_play(1) ->
	GroupList = [{pf_fighter,6000001, 5, 0,1,0,0,6000003, 5,0,0,0,<<"123">>,<<"abc">>,0}
				,{pf_fighter,6000003, 5, 0,1,0,0,6000001, 5,0,0,0,<<"abc">>,<<"123">>,0}],
	set_group_lists(GroupList);
test_play(2) ->
	%{[{pf_fighter,6000003,5,1,1,2,0,6000001,5,0,2,0,[97,98,99],[49,50,51],0},{pf_fighter,6000001,5,1,1,2,0,6000003,5,0,1,0,[49,50,51],[97,98,99],0}],3}
	set_group_lists([{pf_fighter,6000001,5,0,1,0,0,6000003,5,0,0,0,[49,50,51],[97,98,99],0},{pf_fighter,6000003,5,0,1,0,0,6000001,5,0,0,0,[97,98,99],[49,50,51],0}]).

test_set_state(Num, Sec) ->
	State = family_fight_master_server:i(),
	State2 = setelement(Num + 2, State, Sec),
	erlang:send(family_fight_master_server, {set_state2,State2}).

test_set_state2(Num, Sec) ->
    State = family_fight_master_server:i(),
    State2 = setelement(Num + 2, State, Sec),
    erlang:send(family_fight_master_server, {set_state,State2}).

test_get_state()->
    family_fight_master_server:i().
    
test_set_state_sign()->
    State = family_fight_master_server:i(),
    Now = util:now(),
    State2 = State#state{startTime=Now-1,prepareEndTime=Now+7200,randomEndTime=Now+10800,fightEndTime=Now+14400,periodEndTime=Now+18000},
    erlang:send(family_fight_master_server, {set_state,State2}).

test_set_state_random()->
    State = family_fight_master_server:i(),
    Now = util:now(),
    State2 = State#state{startTime=Now-300,prepareEndTime=Now-1,randomEndTime=Now+7200,fightEndTime=Now+14400,periodEndTime=Now+18000},
    erlang:send(family_fight_master_server, {set_state,State2}).

test_set_state_battle()->
    State = family_fight_master_server:i(),
    Now = util:now(),
    State2 = State#state{startTime=Now-300,prepareEndTime=Now-200,randomEndTime=Now-1,fightEndTime=Now+7200,periodEndTime=Now+14400},
    erlang:send(family_fight_master_server, {set_state,State2}).

test_set_state_result()->
    State = family_fight_master_server:i(),
    Now = util:now(),
    State2 = State#state{startTime=Now-300,prepareEndTime=Now-200,randomEndTime=Now-100,fightEndTime=Now-1,periodEndTime=Now+7200},
    erlang:send(family_fight_master_server, {set_state,State2}).

test_set_state_end()->
    State = family_fight_master_server:i(),
    Now = util:now(),
    State2 = State#state{startTime=Now-3000,prepareEndTime=Now-2000,randomEndTime=Now-1000,fightEndTime=Now-900,periodEndTime=Now-800},
     erlang:send(family_fight_master_server, {set_state,State2}).

test_dict_memory(N)->
	Fighters = 
		lists:foldl(fun(E,Acc)-> [test_random_server_fighters(E)|Acc] end, [], lists:seq(1, N)),
	set_group_lists(Fighters),
	do_random_match_fighters(),
	do_rank(),
	ok.
test_random_server_fighters(ServerID)->
	StartID = ServerID*1000000 + util:random_int(1, 100),
	EndID = ServerID*1000000 + util:random_int(301, 801),
	Step = util:random_int(1,5),
	List = 
		lists:foldl(fun(E, Acc) ->
							[#pf_fighter{familyID=E, familyName=integer_to_list(E),serverID=ServerID}|Acc]
					end, [], lists:seq(StartID, EndID,Step)),
	{ServerID,List}.
test_update_rank(N)->
	All = db_sql:get_family_fight_master_rank_info(),
	List = util:random_list2(All, N),
	List2 = test_set_fighters_match(List,[]),
	set_group_lists(List2),
	?ERR("info:~w",[List2]),
	do_rank(),
	ok.
test_set_fighters_match([],C)->
	C;
test_set_fighters_match([H1,H2|T], C)->
	#pf_fighter{familyID=H1FamilyID,serverID=H1ServerID,familyName=H1FamilyName}=H1,
	#pf_fighter{familyID=H2FamilyID,serverID=H2ServerID,familyName=H2FamilyName}=H2,
	test_set_fighters_match(T, [H1#pf_fighter{matchFamilyID=H2FamilyID,matchServerID=H2ServerID,matchFamilyName=H2FamilyName,star=util:random_int(0, 45)}
						 ,H2#pf_fighter{matchFamilyID=H1FamilyID,matchServerID=H1ServerID,matchFamilyName=H1FamilyName,star=util:random_int(0, 45)}|C]);
test_set_fighters_match([H1],C)->
	[H1#pf_fighter{star=util:random_int(0,45)}|C].

test_state_value()->
	Value = {{state,9,1407700001,1407786401,1407815201,1407871001,1407872801}
			,[{group_lists_info,[{2,[{pf_fighter,3000001,2,4,0,0,0,0,0,0,0,0,<<228,184,156,230,181,183,233,190,153,229,174,171>>,[],0,0,2441558}
									,{pf_fighter,3000037,2,9,0,0,0,0,0,0,0,0,<<228,184,128,231,187,159,229,164,169,228,184,139>>,[],0,0,17726550}]}
								,{49,[{pf_fighter,50000052,49,9,0,0,0,0,0,0,0,0,<<232,143,160,232,144,157,232,156,156>>,[],0,0,6945199}
									 ,{pf_fighter,50000044,49,16,0,0,0,0,0,0,0,0,<<229,173,164,231,139,172,232,190,137>>,[],0,0,8196450}]}
								,{29,[{pf_fighter,30000016,29,9,0,0,0,0,0,0,0,0,<<231,165,158,231,189,154>>,[],0,0,10499516}]}]}
			 ,{is_sync,false},{is_rank,false}]},
	db_sql:set_etc(?DB_ETC_KEY_FAMILYFIGHTMASTER, Value).

get_state_time()->
    State = family_fight_master_server:i(),
    DataStartTime = util:seconds_to_datetime(State#state.startTime),
    DataPeriodEndTime = util:seconds_to_datetime(State#state.prepareEndTime),
    DataRandomEndTime = util:seconds_to_datetime(State#state.randomEndTime),
    DataFightEndTime = util:seconds_to_datetime(State#state.fightEndTime),
    DataPeriodEndTime = util:seconds_to_datetime(State#state.periodEndTime),
    ?INFO("Now:~w DataStartTime:~w DataPeriodEndTime:~w DataRandomEndTime:~w DataFightEndTime:~w DataPeriodEndTime:~w "
        ,[DataStartTime,DataPeriodEndTime,DataRandomEndTime,DataFightEndTime,DataPeriodEndTime]),
    {DataStartTime,DataPeriodEndTime,DataRandomEndTime,DataFightEndTime,DataPeriodEndTime}.
    
	
sync_time_info(FamilyID)->
    gen_server:cast(family_fight_master_server,{test_sync_info,FamilyID}).
