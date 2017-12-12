%% @author lixinglong

-module(family_cross_fight_master).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("def_family.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_carlos.hrl").
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-define(dumpDictInterval,300).
-define(check_tick_interval, 150).
-define(wait_more_info_interval,200).
-define(sync_common_node_info_interval, 300).
-define(group_lists_info, group_lists_info).
-define(is_rank, is_rank).
-define(is_sync, is_sync).
-define(dis_match, dis_match).
-define(is_match, is_match).
-define(last_match_result,last_match_result).

start()->
	{ok,_} = supervisor:start_child(family_cross_fight_sup, {?MODULE, {?MODULE, start_link,[]},permanent, 600000, worker,[?MODULE]}).

start_link()->	gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

i()-> gen_server:call(?MODULE,i).

-record(state,{period=0,startTime=0,signTime=0,disTime=0,waitTime=0,fightTime=0,endTime=0}).

init([]) ->
	case data_setting:get(server_type) == family_cross_master of
		true ->
			process_flag(trap_exit,true),
			State = case db_sql:get_etc(?DB_ETC_KEY_FAMILY_CROSS_FIGHT_MASTER) of
						[] -> #state{};
						{StateT,Dic} ->
							check_state_and_init_dict(StateT,Dic),
							StateT
					end,
			do_check(State),
			check_tick(),
			dump_tick(),
			{ok,State};
		_ ->
			?ERR("family_cross_fight_master not started, Reason:not family_fight_master_node"),
			ignore
	end.

check_state_and_init_dict(#state{disTime=PrepareEndTime}, List)->
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
											  fc_fighter ->
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

check_tick() ->	erlang:send_after(1000 * ?check_tick_interval, self(), check_tick).
dump_tick()-> erlang:send_after(1000 * ?dumpDictInterval, self(), do_dump).

handle_call(i,_From,State) -> {reply, State, State};
handle_call({func, M,F, Args}, _From, State) ->
	Result = ?CATCH(apply(M,F,Args)),
	{reply, Result, State};
handle_call(_,_,State) -> {reply, ok, State}.

handle_cast(_,State) -> {noreply, State}.

handle_info({Ref, _},State) when is_reference(Ref) -> {noreply,State};
handle_info({inet_reply, _,_},State)-> {noreply,State};
handle_info(Info,State) ->
	case catch do_handle_info(Info,State) of
		{noreply, NewState} -> {noreply,NewState};
		{noreply,_,hibernate} =ReturnState -> ReturnState;
		Exception -> ?ERR("Exception:~w~n Info:~w~n State:~w~n",[Exception,Info,State]),
					 {noreply,State}
	end.

terminate(Reason,State) ->
	persist_data(State),
	?ERR("~w terminate for ~nReason=~300p~nState=~300p~nDictionary=~10000p",[?MODULE,Reason,State,element(2,process_info(self(),dictionary))]),
	ok.

code_change(_,State,_) -> {ok,State}.


%%do_handle_info(notice_request_status, State) ->
%%	send_all_server(request_master_status),
%%	{noreply,State};
do_handle_info(check_tick, State) ->
	check_tick(),
	do_check(State),
	{noreply,State};
do_handle_info({update_sign_info,ServerID,InfoList}, #state{signTime=SignTime, startTime=StartTime}=State) ->
	Now = util:now(),
	if Now > StartTime andalso Now =< SignTime + ?wait_more_info_interval ->
			update_sign_info({ServerID, InfoList});
	   true -> ignore
	end,
	{noreply,State};
do_handle_info(random_match_fighters,State) ->
	do_random_match_fighters(),
	{noreply,State};
do_handle_info(dis_match, State) ->
	do_dis_match(),
	{noreply,State};
do_handle_info({update_period_info,{Period2,StartTime2,SignTime2,DisTime2,WaitTime2,FightTime2,EndTime2}},State) ->
	State2 = State#state{period=Period2,startTime=StartTime2,signTime=SignTime2,disTime=DisTime2,waitTime=WaitTime2,fightTime=FightTime2,endTime=EndTime2},
	set_group_lists([]),
	clear_rank_sign(),
	clear_match_sign(),
		put(?dis_match, false),
	clear_sync_result(),
	{noreply,State2};
do_handle_info(rank_fighters,State) ->
	do_rank(),
	{noreply,State};
do_handle_info(do_dump,State) ->
	dump_tick(),
	persist_data(State),
	erlang:garbage_collect(),
	{noreply,State,hibernate};
do_handle_info({get_familycross_dtl, IDList,ServerID},State)->
	#state{period=Period,startTime=StartTime,signTime=SignTime,disTime=DisTime,waitTime=WaitTime,fightTime=FightTime,endTime=EndTime} = State,
	NodeFighterInfo = get_node_fighter_info(ServerID,IDList,State),
	send_msg_to_server(ServerID,{get_family_fight_dtl_back, {Period,StartTime,SignTime,DisTime,WaitTime,FightTime,EndTime},NodeFighterInfo}),
	{noreply,State};
do_handle_info({set_state,#state{}=State},_) ->
	send_all_server({change_state_info,State}),
	do_check(State),
	{noreply,State};
do_handle_info(test_next_period, State) ->
	NewState = calc_new_info2(State#state.period),
		set_group_lists([]),
	clear_rank_sign(),
	clear_match_sign(),
		put(?dis_match, false),
	send_all_server({change_state_info,NewState}),
	do_check(NewState),
	{noreply, NewState};
do_handle_info({update_period_info,State},State0 = #state{period=Period}) ->
	case State#state.period == Period+1 of
		true ->
			set_group_lists([]),
			clear_rank_sign(),
			clear_match_sign(),
				put(?dis_match, false),
			clear_sync_result(),
			{noreply,State};
		false -> {noreply,State0}
	end;
do_handle_info(Info,State) ->
	?ERR("can't handle message:~w,state:~w",[Info,State]),
	{noreply,State}.

persist_data(State) ->
	InfoList = {State, lists:filter(fun({?group_lists_info,_})->true ;
												   ({?is_rank,_}) -> true;
												   ({?is_sync,_}) -> true;
												   ({?dis_match,_}) -> true;
												   ({?is_match, _}) -> true;
												   ({?last_match_result,_})-> true;
												   (_)->false
												end, erlang:get())},
	db_sql:set_etc(?DB_ETC_KEY_FAMILY_CROSS_FIGHT_MASTER, InfoList).


do_check(#state{period=Period,startTime=StartTime,signTime=SignTime,disTime=DisTime,waitTime=WaitTime,fightTime=FightTime,endTime=EndTime}) ->
	Now= util:now(),
	LastTick = util:now() - ?check_tick_interval,
	case StartTime of
		0 ->
			{_,Period2,StartTime2,SignTime2,DisTime2,WaitTime2,FightTime2,EndTime2} = Info2 = calc_first_info(),
			erlang:send(self(),{update_period_info,Info2}),
			send_all_server({bc_new_period_begin, Period2,StartTime2,SignTime2,DisTime2,WaitTime2,FightTime2,EndTime2});
		_ ->
			if EndTime =< Now ->
					{_,Period2,StartTime2,SignTime2,DisTime2,WaitTime2,FightTime2,EndTime2} = Info2=calc_new_info(Period),
					erlang:send(self(),{update_period_info,Info2}),
					send_all_server({bc_new_period_begin, Period2,StartTime2,SignTime2,DisTime2,WaitTime2,FightTime2,EndTime2});
			   SignTime =< Now andalso SignTime > LastTick ->
					ignore;
			   DisTime =< Now andalso DisTime > LastTick ->
					prepare_to_dis_fighters(),
					erlang:send_after((WaitTime - Now) * 1000, self(), dis_match);
			   WaitTime =< Now andalso WaitTime > LastTick ->
					prepare_to_dis();
			   FightTime =< Now andalso FightTime > LastTick ->
					ignore;
			   true ->
					ignore
			end
	end.

calc_first_info()->
	StartTime = util:datetime_to_seconds({data_family_cross:get(fisrst_period_date),data_family_cross:get(start_time)}),
	SignTime = StartTime + data_family_cross:get(sign_time),
	DisTime = SignTime + data_family_cross:get(dis_time),
	WaitTime = DisTime + data_family_cross:get(wait_time),
	FightTime = WaitTime + data_family_cross:get(fight_time),
	EndTime = FightTime + data_family_cross:get(end_time),
	{state,1,StartTime,SignTime,DisTime,WaitTime,FightTime,EndTime}.
calc_new_info(P) ->
	{Date,Clo} = erlang:localtime(),
	AddTime = case Clo > {0,30,2} of
				  true -> ?ONE_DAY_SECONDS;
				  false -> 0
			  end,
	StartTime = util:datetime_to_seconds({Date,data_family_cross:get(start_time)})+AddTime,
	SignTime = StartTime + data_family_cross:get(sign_time),
	DisTime = SignTime + data_family_cross:get(dis_time),
	WaitTime = DisTime + data_family_cross:get(wait_time),
	FightTime = WaitTime + data_family_cross:get(fight_time),
	EndTime = FightTime + data_family_cross:get(end_time),
	{state,P+1,StartTime,SignTime,DisTime,WaitTime,FightTime,EndTime}.
calc_new_info2(P) ->
	StartTime = util:now(),
	SignTime = StartTime + data_family_cross:get(sign_time),
	DisTime = SignTime + data_family_cross:get(dis_time),
	WaitTime = DisTime + data_family_cross:get(wait_time),
	FightTime = WaitTime + data_family_cross:get(fight_time),
	EndTime = FightTime + data_family_cross:get(end_time),
	{state,P+1,StartTime,SignTime,DisTime,WaitTime,FightTime,EndTime}.
test_next_period()-> erlang:send(?MODULE,test_next_period).
%% calc_first_info()->
%%     StartTime = util:datetime_to_seconds({data_family_cross:get(fisrst_period_date),data_family_cross:get(start_time)}),
%%     SignTime = util:datetime_to_seconds({data_family_cross:get(fisrst_period_date),{23,59,59}}),
%%     DisTime = util:datetime_to_seconds({data_family_cross:get(fisrst_period_date),{8,0,0}}) + ?ONE_DAY_SECONDS,
%% 	WaitTime = DisTime + 1,
%%     FightTime = util:datetime_to_seconds({data_family_cross:get(fisrst_period_date),{23,59,59}}) + ?ONE_DAY_SECONDS,
%%     ET = util:datetime_to_seconds({data_family_cross:get(fisrst_period_date),data_family_cross:get(start_time)}) + (data_family_cross:get(interval_time)+1)*?ONE_DAY_SECONDS -?check_tick_interval,
%% 	{state,1,StartTime,SignTime,DisTime,WaitTime,FightTime,ET}.
%% calc_new_info(P)->
%% 	StartTime = util:datetime_to_seconds({erlang:date(),data_family_cross:get(start_time)}),
%%     SignTime = util:datetime_to_seconds({erlang:date(),{23,59,59}}),
%%     DisTime = util:datetime_to_seconds({erlang:date(),{8,0,0}}) + ?ONE_DAY_SECONDS,
%% 	WaitTime = DisTime + 1,
%%     FightTime = util:datetime_to_seconds({erlang:date(),{23,59,59}}) + ?ONE_DAY_SECONDS,
%%     ET = util:datetime_to_seconds({erlang:date(),data_family_cross:get(start_time)}) + (data_family_cross:get(interval_time)+1)*?ONE_DAY_SECONDS -?check_tick_interval,
%% 	{state,P+1,StartTime,SignTime,DisTime,WaitTime,FightTime,ET}.

send_all_server(Msg)->
	send_msg:broadcast(family_cross_fight_server, Msg).
send_msg_to_server(ServerID,Msg)->
	send_msg:direct(ServerID,family_cross_fight_server, Msg).

get_group_lists()->
	case erlang:get(?group_lists_info) of
		X when is_list(X) -> X;
		_ -> []
	end.
set_group_lists(X) ->	
	erlang:put(?group_lists_info, X).

update_sign_info({ServerID,_}=Info) ->
	GroupLists= get_group_lists(),
	GroupLists2 = case lists:keytake(ServerID,1,GroupLists) of
					  false -> [Info|GroupLists];
					  {value, _,Other} -> [Info|Other]
				  end,
	set_group_lists(GroupLists2).

clear_rank_sign()-> put(?is_rank, false).
set_rank_sign()-> put(?is_rank, true).
get_rank_sign()-> case get(?is_rank) of true -> true; _ -> false end.

clear_sync_result()-> put(?is_sync, false).
set_sync_result()-> put(?is_sync, true).
get_sync_result()-> case get(?is_sync) of true -> true; _ -> false end.

clear_match_sign() -> put(?is_match, false).
set_match_sign() -> put(?is_match, true).
get_match_sign()-> case get(?is_match) of true -> true; _ -> false end.


do_rank()->
	case get_rank_sign() of
		true -> ignore;
		_ -> GroupListT = calc_score(update_group_lists(get_group_lists())),
			 GroupList = lists:sort(fun(A,B) ->
											A#fc_fighter.score > B#fc_fighter.score
									end,GroupListT),
			 {GroupList2,_} = lists:foldl(fun(E,{Acc,N}) ->
												  {[E#pf_fighter{rank=N}|Acc],N+1}
										  end,{[],1},GroupList),
			 erlang:send_after(1000* ?sync_common_node_info_interval, self(), sync_period_result),
			 set_group_lists(GroupList2),
			 set_rank_sign(),
			 erlang:spawn(fun()-> db_sql:set_family_cross_fight_master_rank_info(GroupList2)
						  end)
	end.

update_group_lists(SignGroupList) ->
	AllGroupLists = db_sql:get_family_cross_fight_master_rank_info(),
	lists:foldl(fun(#fc_fighter{familyID=FamilyID}=Info, Acc) ->
						case lists:keytake(FamilyID,#fc_fighter.familyID,Acc) of
							{value,#fc_fighter{score=Score,rank=Rank}, Others} ->
								[Info#fc_fighter{score=Score,lastRank=Rank}|Others];
							false -> [Info|Acc]
							end
				end,AllGroupLists,SignGroupList).

calc_score(GroupList) -> calc_score(GroupList,[]).
calc_score([],L) -> L;
calc_score([H|T],L) ->
	#fc_fighter{score=S,winScore=W}=H,
	calc_score(T,[H#fc_fighter{score=S+W}|L]).


get_node_fighter_info(_ServerID,IDList,#state{disTime=PT})->
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
				   lists:filter(fun(E) -> lists:member( E#fc_fighter.serverID,IDList) end, GroupList)
		   end
	end.

prepare_to_dis_fighters()->
	erlang:send_after(?wait_more_info_interval * 1000, self(), random_match_fighters).

prepare_to_dis()->
	erlang:send(self(), dis_match).
					
prepare_to_rank() ->
	erlang:send_after(?wait_more_info_interval * 1000, self(), rank_fighters).

do_random_match_fighters()->
	case get_match_sign() of
		true -> ignore;
		_ ->
			set_match_sign(),
			GroupLists = generate_group_list(generate_init_group_list(), get_group_lists()),
			{GroupLists2,MatchList} = random_match_grouplists(GroupLists),
			set_group_lists(GroupLists2),
			set_last_match_result(MatchList),
			?ERR("match:~w",[MatchList]),
			lists:foreach(fun(E)-> send_msg_to_server(E#fc_fighter.serverID,{random_match_fighters,E}) end, GroupLists2)
	end.

do_dis_match()->
	case get(?dis_match) of
		true -> ignore;
		_ -> 
			?ERR("dis:~w",[get_last_match_result()]),
			dis_match(get_last_match_result()),
			put(?dis_match, true)
	end.

%% dis_match(MatchList)-> dis_match(MatchList,0,data_family_cross:get(node_pid_count),1).
%% dis_match([],_,_,_) -> ok;
%% dis_match(L,N,N,C) -> dis_match(L,0,N,C+1);
%% dis_match([H|T],M,N,C)->
%% 	{PareID,H1,H2,H3} = H,
%% 	family_cross_fight_manager:send_to_me(C,PareID,H1,H2,H3),
%% 	dis_match(T,M+1,N,C).

dis_match(MatchList) -> dis_match(MatchList,1,data_family_cross:get(nodes_count)).
dis_match([],_,_) -> ignore;
dis_match([H|T],CT,CT) -> 
	{PareID, H1,H2,H3} = H, 
	family_cross_fight_manager:send_to_me(CT,PareID,H1,H2,H3),
	dis_match(T, 1,CT);
dis_match([H|T],N,CT) -> 
	{PareID, H1,H2,H3} = H, 
	family_cross_fight_manager:send_to_me(N,PareID,H1,H2,H3),
	dis_match(T, N+1,CT).

generate_init_group_list()->
	ScoreList = data_family_cross_fight_split:get_list(),
	lists:foldl(fun(Score,Acc)->[{Score,[]}|Acc] end, [],ScoreList).

generate_group_list(InitList,Fighters)->
	SList = lists:foldl(fun(#fc_fighter{score=S}=Info, Acc)->
						{S2,_} = data_family_cross_fight_split:get(S),
						{value, {S2,S2ScoreList},OtherList} = lists:keytake(S2,1,Acc),
						[{S2,[Info|S2ScoreList]}|OtherList]
						end,InitList,lists:foldl(fun({_,List},Acc1) -> List++Acc1 end, [],Fighters)),
	%% keySort ��С����,foldl֮���ɴӴ�С
	lists:foldl(fun({S,List},Acc) -> 
						SortList = lists:reverse(lists:keysort(#fc_fighter.score, List)),
						ListLength = length(SortList),
						if ListLength == 0 -> Acc;
						   true -> [{S,SortList,ListLength}|Acc]
						end end, [],lists:keysort(1,SList)).


random_match_grouplists(GroupLists) ->
	random_match_grouplists(GroupLists,[],[],1).
random_match_grouplists([],L,MatchList,_) -> {L,MatchList};
random_match_grouplists([{S,SortLists,Length}],L,MatchList,PareID) when Length > 3 ->
	[H1|T0] = SortLists,
	{H2,T1} = random_take_one(T0,Length-1),
	{H3,T} = random_take_one(T1,Length-2),
	{L2,MatchList2} = update_match_info(H1,H2,H3,MatchList,PareID),
	random_match_grouplists([{S,T,Length-3}],L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S,SortLists,Length}],L,MatchList,PareID) ->
	{L2,MatchList2} = if Length == 3 ->
				 [H1,H2,H3] = SortLists,
				 update_match_info(H1,H2,H3,MatchList,PareID);
			Length == 2 ->
				 [H1,H2] = SortLists,
				 update_match_info(H1,H2,#fc_fighter{},MatchList,PareID);
			Length == 1 ->
				 update_match_info(hd(SortLists),#fc_fighter{},#fc_fighter{},MatchList,PareID)
		 end,
	random_match_grouplists([],L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S1,SortLists1,1},{_S2,SortLists2,1}],L,MatchList,PareID)->
	{L2,MatchList2} = update_match_info(hd(SortLists1),hd(SortLists2),#fc_fighter{},MatchList,PareID),
	random_match_grouplists([],L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S1,SortLists1,1},{_S2,SortLists2,1},{_S3,SortLists3,1}|TG],L,MatchList,PareID)->
	{L2,MatchList2} = update_match_info(hd(SortLists1),hd(SortLists2),hd(SortLists3),MatchList,PareID),
	random_match_grouplists(TG,L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S1,SortLists1,1},{_S2,SortLists2,1},{S3,SortLists3,Length3}|TG],L,MatchList,PareID)->
	[H3|T] = SortLists3,
	{L2,MatchList2} = update_match_info(hd(SortLists1),hd(SortLists2),H3,MatchList,PareID),
	random_match_grouplists([{S3,T,Length3-1}|TG],L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S1,SortLists1,2},{_S2,SortLists2,1}|TG],L,MatchList,PareID) ->
	[H1,H2] = SortLists1,
	{L2,MatchList2} = update_match_info(H1,H2,hd(SortLists2),MatchList,PareID),
	random_match_grouplists(TG,L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S1,SortLists1,2},{S2,SortLists2,Length2}|TG],L,MatchList,PareID)->
	%[H3|T] = SortLists2,
	{H3,T} = random_take_one(SortLists2,Length2),
	[H1,H2] = SortLists1,
	{L2,MatchList2} = update_match_info(H1,H2,H3,MatchList,PareID),
	random_match_grouplists([{S2,T,Length2-1}|TG],L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S1,SortLists1,1},{_S2,SortLists2,2}|TG],L,MatchList,PareID)->
	[H2,H3] = SortLists2,
	{L2,MatchList2} = update_match_info(hd(SortLists1),H2,H3,MatchList,PareID),
	random_match_grouplists(TG,L2++L,MatchList2,PareID+1);
random_match_grouplists([{_S1,SortLists1,1},{S2,SortLists2,Length2}|TG],L,MatchList,PareID)->
	[H2|T0] = SortLists2,
	{H3,T} = random_take_one(T0,Length2-1),
	{L2,MatchList2} = update_match_info(hd(SortLists1),H2,H3,MatchList,PareID),
	random_match_grouplists([{S2,T,Length2-2}|TG],L2++L,MatchList2,PareID+1);
random_match_grouplists([{S1,SortLists1,Length1},{S2,SortLists2,Length2}|TG],L,MatchList,PareID)->
	[H1|T1] = SortLists1,
	{H2,T1_2} = random_take_one(T1,Length1-1),
	%[H3|T2] = SortLists2,
	{H3,T2} = random_take_one(SortLists2,Length2),
	{L2,MatchList2} = update_match_info(H1,H2,H3,MatchList,PareID),
	Length2_2 = Length2-1,
	if Length2_2 == 0 -> random_match_grouplists([{S1,T1_2,Length1-2}|TG],L2++L,MatchList2,PareID+1);
	   true -> random_match_grouplists([{S1,T1_2,Length1-2},{S2,T2,Length2_2}|TG],L2++L,MatchList2,PareID+1)
	end.

update_match_info(H1,H2,H3,MatchList,PareID)->
	#fc_fighter{familyName=Name1,serverID=ServerID1}=H1,
	#fc_fighter{familyName=Name2,serverID=ServerID2}=H2,
	#fc_fighter{familyName=Name3,serverID=ServerID3}=H3,
	H1_2 = H1#fc_fighter{pareID=PareID,enermyInfo=[{ServerID2,Name2},{ServerID3,Name3}]},
	H2_2 = H2#fc_fighter{pareID=PareID,enermyInfo=[{ServerID1,Name1},{ServerID3,Name3}]},
	H3_2 = H3#fc_fighter{pareID=PareID,enermyInfo=[{ServerID1,Name1},{ServerID2,Name2}]},
	{[H1_2,H2_2,H3_2],[{PareID,H1_2,H2_2,H3_2}|MatchList]}.

set_last_match_result(GroupLists)-> put(?last_match_result,GroupLists).
get_last_match_result() -> case get(?last_match_result) of X when is_list(X) -> X; _ -> [] end.

random_take_one(List,Length)->
	NTake = case random:uniform(Length) of 0 -> 1; X -> X end,
	random_take_one(List,[],NTake).
random_take_one([H|T],Acc,1) -> {H,lists:reverse(Acc) ++ T};
random_take_one([H|T],Acc,NTake) -> random_take_one(T,[H|Acc],NTake-1).


%% test_set_value() ->
%% 	erase(),
%% 	PBase = {fc_fighter,10000004,0,15,1,0,0,<<228,186,173,228,186,173,53>>,0,0,<<49,49,49>>,3730940,0,[]},
%% 	P1 = PBase#fc_fighter{score=1500},
%% 	P2 = PBase#fc_fighter{familyID=201,serverID=1},
%% 	P5 = PBase#fc_fighter{familyID = 16000021,serverID=15,score=3000},
%% 	P3 = PBase#fc_fighter{familyID=202,serverID=1},
%% 	P4 = PBase#fc_fighter{familyID=203,serverID=1},
%% 	P9 = PBase#fc_fighter{familyID = 204,serverID=1},
%% 	P6 = PBase#fc_fighter{familyID = 881,serverID=18},
%% 	P7 = PBase#fc_fighter{familyID = 882,serverID=18},
%% 	P8 = PBase#fc_fighter{familyID = 883,serverID=18},
%% 	set_group_lists([{15,[P1,P5]},{1,[P2,P3,P4,P9]},{18, [P6,P7,P8]}]).
%% 
%% test_random_match()->
%% 	erase(),
%% 	List = data_family_cross:get(test_match),
%% 	set_group_lists(List),
%% 	do_random_match_fighters().


