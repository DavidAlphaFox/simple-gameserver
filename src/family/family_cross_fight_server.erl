-module(family_cross_fight_server).
-bebaviour(gen_server).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_call).
-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
%-include("stdlib/include/ms_transform.hrl").
-include("def_mail.hrl").
-define(DUMP_INTERVAL, 600000).
-define(tryConnectDuration,60).
-define(sync_server_info_tick, 2000).
-define(checkMasterConnectionInterval, 200).
-define(sync_signup_tick, 30).
-define(checkMasterConnection,checkMasterConnection).
-define(tryConnect,tryConnect).
-define(sync_server_info,sync_server_info).
-define(save_period,save_period).
-define(new_sign,new_sign).

-record(state,{period=0,signList=[],startTime=0,signTime=0,disTime=0,waitTime=0,fightTime=0,endTime=0}).

start()->
	{ok,_} = supervisor:start_child(family_cross_fight_sup, {?MODULE,{?MODULE,start_link,[]}, permanent,600000,worker, [?MODULE]}).

start_link()->
	gen_server:start_link({local,?MODULE},?MODULE, [],[]).

i()-> gen_server:call(?MODULE, i).

init([])->
	process_flag(trap_exit, true),
	case data_setting:get(server_type) of
		normal ->
			init_data(),
			%erlang:send_after(?DUMP_INTERVAL, self(), dump_data),
			connect_master(),
			sync_sign_tick(),
			dump_tick(),
			{ok,#state{}};
		_ ->
			{ok,#state{}}
	end.

dump_tick()-> erlang:send_after(10000, self(), do_dump).

init_data() ->
	case db_sql:get_etc(?DB_ETC_KEY_FAMILYFIGHTMASTER) of
		[] -> put(?save_period, 0);
		{Period, List} ->put(?save_period, Period),
						 ets:insert(?ETS_FAMILYCROSS_ROLE_WAR,List)
	end.

update_ets_data(Period) ->
	case get(?save_period) of
		Period -> ignore;
		_ -> 
			put(?save_period, Period),
			ets:delete_all_objects(?ETS_FAMILYCROSS_ROLE_WAR)
	end.

connect_master()->
	MasterNode = get_master_node(),
	case net_kernel:connect(MasterNode) of
		true ->
			check_master_connection(),
			%sync_server_info(),
			ServerID = data_setting:get(server_id),
			send_msg_to_master_node({get_familycross_dtl,[ServerID|data_setting:get(merge_server_id_list)],ServerID});
		Err ->
			erlang:send_after(?tryConnectDuration * 1000, self(), ?tryConnect),
			?ERR("connect to master ~w failed with reason ~w",[MasterNode,Err])
	end.

check_master_connection()->
	case lists:member(get_master_node(), [erlang:node()|erlang:nodes()]) of
		true ->	erlang:send_after(?checkMasterConnectionInterval * 1000, self(), ?checkMasterConnection);
		_ -> 
			erlang:send_after(?tryConnectDuration * 1000, self(), ?tryConnect),
			?ERR("connect to master ~w failed with reason ~w",[get_master_node(), not_in_nodes])
	end.

sync_sign_tick()-> erlang:send_after(?sync_signup_tick*1000, self(), sync_signup_info).

get_master_node()->
	erlang:list_to_atom(data_family_cross:get(master_node)).

send_msg_to_master_node(Msg) ->
	Node = get_master_node(),
	{family_cross_fight_master,Node } ! Msg.

sync_server_info()->
%	erlang:send_after(?sync_server_info_tick * 1000, self(), ?sync_server_info),
	[ThisNode]=erlang:nodes(this),
	ServerInfoList = [{ServerID,ThisNode}||ServerID<-[data_setting:get(server_id)|data_setting:get(merge_server_id_list)]],
	send_msg_to_master_node({sync_server_info, ServerInfoList,ThisNode}).

handle_call(i,_,State) -> {reply,State,State};
handle_call(_,_,State) -> {reply, ok,State}.
handle_cast(_,State) -> {noreply,State}.
handle_info({Ref,_Res},State) when is_reference(Ref) -> {noreply,State};
handle_info({inet_reply, _,_},State) -> {noreply,State};
handle_info(Info,State)->
	case catch do_handle_info(Info,State) of
		{noreply,NewState} -> {noreply,NewState};
		hibernate -> {noreply,State,hibernate};
		Exception -> ?ERR("exception:~w, info:~w,state:~w",[Exception, Info,State]),
					 {noreply,State}

	end.

terminate(_,_) ->  ok.
code_change(_,State,_) ->{ok,State}.

do_handle_info(do_dump, State=#state{endTime=EndTime,disTime=DisTime,period=Period}) ->
	dump_tick(),
	Now = util:now(),
	case Now > EndTime orelse Now < DisTime of true -> ignore;
		_ -> db_sql:set_etc(?DB_ETC_KEY_FAMILYFIGHTMASTER,{Period,ets:tab2list(?ETS_FAMILYCROSS_ROLE_WAR)})
	end,
	{noreply,State};
do_handle_info(?tryConnect,State)->
	connect_master(),
	{noreply,State};
do_handle_info(?checkMasterConnection, State)->
	check_master_connection(),
	{noreply,State};
%% do_handle_info(?sync_server_info, State) ->
%% 	sync_server_info(), 
%% 	{noreply, State};
do_handle_info({cs_familycross_info, RoleID,Period,_IsSign,FamilyID},State) ->
	#state{period=SPeriod,signList=SignList,disTime=DisTime}=State,
	if SPeriod /= Period ->
			?unicast(RoleID,#sc_familycross_info{result=1,info=[p_familycross_info_dtl(2,State,RoleID,[])]}),
			update_familycross_status(FamilyID,SPeriod,DisTime);
	   true ->
			case lists:keyfind(FamilyID, #fc_fighter.familyID, SignList) of
				#fc_fighter{enermyInfo=Enermys} -> ?unicast(RoleID,#sc_familycross_info{result=1,info=[p_familycross_info_dtl(1,State,RoleID,Enermys)]});
				_ ->?unicast(RoleID,#sc_familycross_info{result=1,info=[p_familycross_info_dtl(2,State,RoleID,[])]})
			end
	end,
	{noreply,State};

do_handle_info({family_unsign,FamilyID}, #state{signList=SignList}=State) ->
	SignList2 = lists:keydelete(FamilyID,#fc_fighter.familyID,SignList),
	{noreply, State#state{signList=SignList2}};
	
do_handle_info({cs_familycross_sign,RoleID,FamilyID,FamilyName,TotalFightPower,OwnerName,Score},State)->
	#state{signList=SignList}=State,
	case check_time(State) of
		sign ->
			case lists:keyfind(FamilyID,#fc_fighter.familyID,SignList) of
				false ->
					SignList2 = [#fc_fighter{familyID=FamilyID,familyName=FamilyName,totalFightPower=TotalFightPower
											,ownerName=OwnerName,score=Score,serverID=data_setting:get(server_id)}
								 |SignList],
					family_misc:router_to_family_process(FamilyID, familycross_sign_succ),
					set_sign_change(),
					?unicast(RoleID,#sc_familycross_sign{result=1});
				_ ->
					SignList2=SignList,
					?unicast(RoleID,#sc_familycross_sign{result=4})
			end;
		_ ->
			SignList2=SignList,
			?unicast(RoleID,#sc_familycross_sign{result=6})
	end,
	{noreply,State#state{signList=SignList2}};

do_handle_info(sync_signup_info, #state{signList=SignList,signTime=SignTime}=State)->
		sync_sign_tick(),
	case util:now() < (SignTime + ?sync_signup_tick) of
		true ->
			case sign_changed() of
				true -> 
					clear_sign_change(),
					send_msg_to_master_node({update_sign_info, data_setting:get(server_id), SignList});
				_ -> ignore
			end;
		_ -> ignore
	end,
	{noreply,State};

do_handle_info({get_family_fight_dtl_back, {Period,StartTime,SignTime,DisTime,WaitTime,FightTime,EndTime},NodeFighterInfo},State)->
	case State#state.period of
		Period -> State2=State;
		_ ->
			update_ets_data(Period),
			State2 = #state{period=Period,signList=NodeFighterInfo,startTime=StartTime,signTime=SignTime,disTime=DisTime,waitTime=WaitTime,fightTime=FightTime,endTime=EndTime}
	end,
	{noreply,State2};


do_handle_info({change_state_info, {_,Period,StartTime,SignTime,DisTime,WaitTime,FightTime,EndTime}},State)->
	State2 = 
		case State#state.period of
			Period ->State#state{period=Period,startTime=StartTime,signTime=SignTime,disTime=DisTime,waitTime=WaitTime,fightTime=FightTime,endTime=EndTime};
			_ -> 
				update_ets_data(Period),
				State#state{period=Period,startTime=StartTime,signTime=SignTime,disTime=DisTime,waitTime=WaitTime,fightTime=FightTime,endTime=EndTime,signList=[]}
		end,
	{noreply,State2};

do_handle_info({bc_new_period_begin, Period, StartTime,SignTime,DisTime,WaitTime,FightTime,EndTime},State) ->
	State2=State#state{period=Period,signList=[],startTime=StartTime,signTime=SignTime,disTime=DisTime,waitTime=WaitTime,fightTime=FightTime,endTime=EndTime},
	update_ets_data(Period),
	spawn(fun()-> FamilyIDs = db_sql:get_all_family_id(),
				  lists:foreach(fun(FamilyID) ->
										update_familycross_status(FamilyID,Period,DisTime)
								end,FamilyIDs)
		  end),
	{noreply,State2};

do_handle_info(update_family_disTime_info, #state{disTime=DisTime}=State)->
	spawn(fun()-> FamilyIDs = db_sql:get_all_family_id(),
				  lists:foreach(fun(FamilyID) ->
										family_misc:router_to_family_process(FamilyID,{update_family_cross_disTime,DisTime})
								end,FamilyIDs)
		  end),
	{noreply,State};

do_handle_info({cs_familycross_reborn, RoleID}, State) ->
	?CATCH(role_lib:send_server(RoleID, {route, role_familycross, {cs_familycross_reborn, RoleID}})),
	{noreply, State};

do_handle_info({familycross_bc_info,RoleID,Msg}, State) ->
	?unicast(RoleID,Msg),
	{noreply,State};

do_handle_info({familycross_bc_cmd,RoleID,clean_battle_info},State) ->
	ets:delete(?ETS_FAMILYCROSS_ROLE_WAR,{match_info,RoleID}),
	{noreply,State};

do_handle_info({random_match_fighters, F}, #state{signList=SignList}=State) ->
	FamilyID = F#fc_fighter.familyID,
	List2 = case lists:keytake(FamilyID, #fc_fighter.familyID,SignList) of
				{value,_,Other} -> [F|Other];
				_ -> [F|SignList]
			end,
	family_misc:router_to_family_process(FamilyID,{random_match_cross_fighters,F}),
	{noreply,State#state{signList=List2}};
				  
do_handle_info({get_player,FamilyID,WarID,ManagerServerID,Process,PlayerType},State) ->
	family_misc:router_to_family_process(FamilyID,{get_player,FamilyID,WarID,ManagerServerID,Process,PlayerType}),
	{noreply,State};
					   
					  

do_handle_info(_,_) -> ok.


p_familycross_info_dtl(IsSign,State,RoleID,EnermyList) ->
	Status0 = get_status(State),
		#state{signTime=SignTime,startTime=StartTime,waitTime=FightStartTime,endTime=EndTime}=State,
	Status = case Status0 of
				 2 ->
					 case ets:lookup(?ETS_FAMILYCROSS_ROLE_WAR,{match_info,RoleID}) of
						 [] -> 
							 case util:now() < FightStartTime of true -> 2;
								 _ ->3
							 end;
						 _ -> 2
					 end;
				 _ -> Status0
			 end,
	Enermy=[#p_familycross_enermy{serverID=ServerID,familyName=FamilyName}||{ServerID, FamilyName}<-EnermyList],
	NeedLevel = data_family_cross:get(need_level),
	#p_familycross_info_dtl{state=Status,isSign=IsSign,needLevel=NeedLevel,fightStartTime=FightStartTime
						   ,signStartTime=StartTime,signEndTime=SignTime,periodEndTime=EndTime,enermy=Enermy}.

update_familycross_status(FamilyID,Period,DisTime) ->
	family_misc:router_to_family_process(FamilyID,{update_family_cross_status, Period,DisTime}).

get_status(#state{startTime=StartTime,signTime=SignTime,fightTime=FightTime}) -> %2.
%abc(#state{signTime=SignTime,fightTime=FightTime})->
	Now = util:now(),
	if Now < StartTime -> 4;
		Now < SignTime -> 1;
	   Now < FightTime -> 2;
	   true -> 3
	end.
check_time(#state{startTime=StartTime,signTime=SignTime}) ->
	Now = util:now(),
	if Now > StartTime andalso Now < SignTime -> sign;
	   true -> ignore
	end.


set_sign_change() -> put(?new_sign, 1).
clear_sign_change() -> erlang:erase(?new_sign).
sign_changed() -> case get(?new_sign) of 1 -> true;_-> false end.
					   
	
