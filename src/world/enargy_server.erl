%% @author lihuachao
%% @doc  好友赠送体力和加好友请求控制

-module(enargy_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

-define(FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,friend_enargy_data_change_roleid_list).

%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit,true),
	dump_data(),
	erlang:send(self(),do_hibernate),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(do_hibernate,State)->
	erlang:send_after(600000, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
    {noreply, State}.

terminate(_Reason, _State) ->
	persist_change(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%检测是否可以加这个好友(1:可以 0：不可以)
can_add_friend(RoleEnargyFriendInfo,AddFriendID)->
	#friend_enargy{roleID=RoleID, addFriendList=AddFriendList} = RoleEnargyFriendInfo,
    case RoleID =:= AddFriendID of
        true ->
            0;
        _ ->
	        case lists:keyfind(AddFriendID,1,AddFriendList) of
		        false->
			        1;
		        {_,S}->
			        case (util:now()-S) >= (3 * ?ONE_DAY_SECONDS) of
				        true->
					        1;
				        false->
					        0
			        end
	        end 
    end.

can_send_friend_enargy(RoleEnargyFriendInfo,FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
	#friend_enargy{toFriendList=ToFriendList} = RoleEnargyFriendInfo,
	case lists:keyfind(FriendRoleID, 1, ToFriendList) of
		false->
			1;
		{_,S,IsGive}->
			case IsGive of
				1->
					{{_,_,D},_} = util:seconds_to_datetime(S),
					{_,_,D1} = date(),
					case D=/=D1 of
						true->
							1;
						false->
							2
					end;
				_->
					0
			end
	end;
can_send_friend_enargy(undefined, _)->
	0.

%%判断是否可领取这个好友赠送的体力
can_give_friend_enargy(RoleEnargyFriendInfo,FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
	#friend_enargy{toMeList=ToMyList,giveTimes=_GiveTimes,date=_Date} = RoleEnargyFriendInfo,
%% 	NewGiveTimes = get_give_times(GiveTimes,Date),
%% 	case NewGiveTimes =< 0 of
%% 		true->
%% 			0; 
%% 		false->
			case lists:keyfind(FriendRoleID, 1, ToMyList) of
				false->
					0;
				_->
					1
%% 			end
	end;
can_give_friend_enargy(undefined, _)->
	0.

get_send_second(RoleEnargyFriendInfo,FriendRoleID) when erlang:is_record(RoleEnargyFriendInfo, friend_enargy) ->
	#friend_enargy{toMeList=ToMyList} = RoleEnargyFriendInfo,
	case lists:keyfind(FriendRoleID, 1, ToMyList) of
		{_,S}->
			S;
		false->
			0
	end;
get_send_second(undefined, _)->
	0.

%%得到今日送的次数
get_give_time(undefined)->
	0;
get_give_time(RoleEnargyFriendInfo)->
	#friend_enargy{giveTimes=GiveTimes,date=Date} = RoleEnargyFriendInfo,
	get_give_times(GiveTimes,Date).

get_give_times(GiveTimes,Date)->
	case date() of
		Date->
			erlang:min(GiveTimes, 20);
		_->
			20
	end.


%%得到信息
get_ets_friend_enargy(RoleID)->
	case ets:lookup(?ETS_ENARGY_FRIEND_DATA, RoleID) of
		[]->
			Info = db_sql:get_enargy_friend(RoleID),
			ets:insert(?ETS_ENARGY_FRIEND_DATA, Info),
			Info;
		[Info]->
			Info
	end.
update_ets_friend_enargy(RoleID,RoleEnargyFriendInfo)->
	ets:insert(?ETS_ENARGY_FRIEND_DATA, RoleEnargyFriendInfo),
	mark_role_homestead_change(RoleID).

mark_role_homestead_change(RoleID)->
	case get(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST) of
		?undefined->
			put(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,[RoleID]);
		List->
			case lists:member(RoleID,List) of
				true->
					true;
				false->
					put(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,[RoleID|List])
			end
	end.

%% 持久化改变了的
dump_data() ->
	erlang:send_after(60*1000, self(), dump_data).

%%送体力
enargy_send(RoleID,FriendRoleID)->
	erlang:send(?MODULE, {enargy_send,RoleID,FriendRoleID}).
%%领取体力
enargy_give(RoleID,RoleIDList)->
	erlang:send(?MODULE, {enargy_give,RoleID,RoleIDList}).

enargy_add_friend(RoleID,FriendRoleID)->
	erlang:send(?MODULE, {enargy_add_friend,RoleID,FriendRoleID}).

enargy_remove_friend(RoleID,FriendRoleID)->
	erlang:send(?MODULE, {enargy_remove_friend,RoleID,FriendRoleID, false}).

enargy_remove_friend2(RoleID,FriendRoleID)->
    erlang:send(?MODULE, {enargy_remove_friend,RoleID,FriendRoleID, true}).

enargy_role_online(RoleID)->
	erlang:send(?MODULE, {enargy_role_online,RoleID}).

enargy_role_offline(RoleID)->
	erlang:send(?MODULE, {enargy_role_offline,RoleID}).

do_handle_info(dump_data)->
	dump_data(),
	persist_change();
do_handle_info({enargy_send,RoleID,FriendRoleID})->
	RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
	case can_send_friend_enargy(RoleEnargyFriendInfo, FriendRoleID) of
		1->
			FRoleEnargyFriendInfo = get_ets_friend_enargy(FriendRoleID),
			{NewRoleEnargyFriendInfo,NewFRoleEnargyFriendInfo} = send_enargy(RoleEnargyFriendInfo,FRoleEnargyFriendInfo),
			update_ets_friend_enargy(RoleID, NewRoleEnargyFriendInfo),
			update_ets_friend_enargy(FriendRoleID, NewFRoleEnargyFriendInfo),
			?unicast(RoleID, #sc_friend_send_enargy{roleID=FriendRoleID}),
%% 			#friend_enargy{giveTimes=GiveTimes}=get_ets_friend_enargy(FriendRoleID),
%% 			case get_give_times(GiveTimes,erlang:date()) =< 0 of
%% 				true ->
%% 					ignore;
%% 				_ ->
			?unicast(FriendRoleID,#sc_friend_send_enargy_me{roleID=RoleID});
%% 			end;
		_->
			?unicast(RoleID, #sc_friend_send_enargy{result=3})
	end;
do_handle_info({enargy_give,RoleID,RoleIDList})->
	case RoleIDList of
		[]->
			?unicast(RoleID, #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=2});
		_->
			RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
			#friend_enargy{toMeList=ToMeList,giveTimes=GiveTimes,date=Date} = RoleEnargyFriendInfo,
			NewRoleIDList1 = lists:filter(fun(Rid)->lists:keyfind(Rid, 1, ToMeList)=/=false end, RoleIDList),
			case NewRoleIDList1 of
				[]->
					?unicast(RoleID, #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=3});
				_->
					NewGiveTimes1 = get_give_times(GiveTimes, Date),
					case NewGiveTimes1 =< 0 of
						true->
							?unicast(RoleID, #sc_friend_give_enargy{roleIDList=[],giveTimes=0,result=4});
						_->
							NewRoleIDList = lists:sublist(NewRoleIDList1, NewGiveTimes1),
							GTimes = length(NewRoleIDList),
							NewGiveTimes = NewGiveTimes1 - GTimes,
							give_enargy(RoleEnargyFriendInfo,NewRoleIDList,NewGiveTimes,GTimes)
					end
			end
	end;
do_handle_info({enargy_add_friend,RoleID,FriendRoleID})->
	FRoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
	#friend_enargy{addFriendList=AddFriendList} = FRoleEnargyFriendInfo,
	NewAddFriendList = [{FriendRoleID,util:now()} | lists:keydelete(FriendRoleID, 1, AddFriendList)],
	NewFRoleEnargyFriendInfo = FRoleEnargyFriendInfo#friend_enargy{addFriendList=NewAddFriendList},
	update_ets_friend_enargy(RoleID, NewFRoleEnargyFriendInfo);
do_handle_info({enargy_remove_friend,RoleID,FriendRoleID, IsNotify})->
	FRoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
	#friend_enargy{addFriendList=AddFriendList} = FRoleEnargyFriendInfo,
	NewAddFriendList = lists:keydelete(FriendRoleID, 1, AddFriendList),
	NewFRoleEnargyFriendInfo = FRoleEnargyFriendInfo#friend_enargy{addFriendList=NewAddFriendList},
	update_ets_friend_enargy(RoleID, NewFRoleEnargyFriendInfo),
    case IsNotify of
        true ->
            ?unicast(FriendRoleID, #sc_friend_remove_request{});
        false ->
            next
    end;
do_handle_info({enargy_role_online,RoleID})->
	RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
	#friend_enargy{toFriendList=ToFriendList,giveTimes=GiveTimes,date=Date} = RoleEnargyFriendInfo,
	NewToFriendList = refresh_toFriendList(ToFriendList),
	NewGiveTimes = get_give_times(GiveTimes, Date),
	NewRoleEnargyFriendInfo = RoleEnargyFriendInfo#friend_enargy{toFriendList=NewToFriendList,giveTimes=NewGiveTimes,date=date()},
	update_ets_friend_enargy(RoleID, NewRoleEnargyFriendInfo);

do_handle_info({enargy_role_offline,RoleID})->
	persist_change(RoleID);
do_handle_info(_Info)->
	?ERR("无效消息:~p",[_Info]).

%% give_all_enargy(RoleID)->
%% 	RoleEnargyFriendInfo = get_ets_friend_enargy(RoleID),
%% 	#friend_enargy{toMeList=ToMeList,giveTimes=GiveTimes,date=Date} = RoleEnargyFriendInfo,
%% 	CurrGiveTimes = get_give_times(GiveTimes,Date),
%% 	case CurrGiveTimes>0 of
%% 		true->
%% 			case ToMeList of
%% 				[]->
%% 					?unicast(RoleID,#sc_friend_give_all_enargy{result=3,roleIDList=[],giveTimes=2});
%% 				_->
%% 					NewRoleIDList = [R||{R,_}<-lists:sublist(ToMeList, CurrGiveTimes)],
%% 					GTimes = length(NewRoleIDList),
%% 					NewGiveTimes = CurrGiveTimes - GTimes,
%% 					give_enargy(RoleEnargyFriendInfo,NewRoleIDList,NewGiveTimes,GTimes)
%% 			end;
%% 		false->
%% 			?unicast(RoleID,#sc_friend_give_all_enargy{result=3,roleIDList=[],giveTimes=0})
%% 	end.

%%送体力
send_enargy(RoleEnargyFriendInfo,FRoleEnargyFriendInfo)->
	S = util:now(),
	#friend_enargy{toFriendList=ToFriendList,roleID=RoleID} = RoleEnargyFriendInfo,
	#friend_enargy{toMeList=ToMeList,roleID=FriendRoleID} = FRoleEnargyFriendInfo,
	NewToFriendList = refresh_toFriendList([{FriendRoleID,S,0}|lists:keydelete(FriendRoleID, 1, ToFriendList)]),
	NewRoleEnargyFriendInfo = RoleEnargyFriendInfo#friend_enargy{toFriendList=NewToFriendList},
	NewToMeList = [{RoleID,S}|lists:keydelete(RoleID, 1, ToMeList)],
	NewFRoleEnargyFriendInfo = FRoleEnargyFriendInfo#friend_enargy{toMeList=NewToMeList},
    ?CATCH(role_task:send_dispach(RoleID, {dispach_task,role_enargy_give_reward})),
	behavior_friend_enargy:log(RoleID,FriendRoleID,2,1),
	{NewRoleEnargyFriendInfo,NewFRoleEnargyFriendInfo}.

%%领取并回赠
give_enargy(RoleEnargyFriendInfo,RoleIDList,NewGiveTimes,GTimes)->
	#friend_enargy{toMeList=ToMeList,roleID=RoleID} = RoleEnargyFriendInfo,
	NewToMeList =
		lists:foldl(fun(ID,Acc)->
							lists:keydelete(ID, 1, Acc)
					end, ToMeList, RoleIDList),
	DDate = date(),
	NewRoleEnargyFriendInfo1 = RoleEnargyFriendInfo#friend_enargy{giveTimes=NewGiveTimes,date=DDate,toMeList=NewToMeList},
 	NewRoleEnargyFriendInfo =
		lists:foldl(fun(Rid,REF)->
							FRoleEnargyFriendInfo = get_ets_friend_enargy(Rid),
							#friend_enargy{toFriendList=ToFriendList,roleID=FRoleID} = FRoleEnargyFriendInfo,
							case lists:keyfind(RoleID, 1, ToFriendList) of
								false->
									REF;
								{_,S,_}->
									{Date,_} = util:seconds_to_datetime(S),
									NewToFriendList =
										case Date of
											DDate->
												lists:keyreplace(RoleID, 1, ToFriendList, {RoleID,S,1});
											_->
												lists:keydelete(RoleID, 1, ToFriendList)
										end,
									NewFRoleEnargyFriendInfo1 = FRoleEnargyFriendInfo#friend_enargy{toFriendList=refresh_toFriendList(NewToFriendList)},
									case can_send_friend_enargy(REF, Rid) of
										1->
											{NewREF,NewFRoleEnargyFriendInfo} = send_enargy(REF, NewFRoleEnargyFriendInfo1),
											?unicast(Rid,#sc_friend_send_enargy_me{roleID=RoleID});
										_->
											NewREF = REF,
											NewFRoleEnargyFriendInfo = NewFRoleEnargyFriendInfo1
									end,
									CanSend = can_send_friend_enargy(NewFRoleEnargyFriendInfo, RoleID),
									SendRecord = #sc_frend_give_enargy_me{roleID=RoleID,canSend=CanSend},
									?unicast(Rid,SendRecord),
									behavior_friend_enargy:log(RoleID,FRoleID,1,1),
									update_ets_friend_enargy(Rid, NewFRoleEnargyFriendInfo),
									NewREF
							end
					end, NewRoleEnargyFriendInfo1,RoleIDList),
	update_ets_friend_enargy(RoleID, NewRoleEnargyFriendInfo),
	role_lib:send_server(RoleID, {route, role_friend,{enargy_give_reward,GTimes}}),
	?unicast(RoleID,#sc_friend_give_enargy{roleIDList=RoleIDList,giveTimes=NewGiveTimes,result=0}).

refresh_toFriendList(ToFriendList)->
	DDate = date(),
	lists:filter(fun({_,S,IsGive})->
						 case IsGive of
							 1->
								 {Date,_} = util:seconds_to_datetime(S),
								 case Date of
									 DDate->
										 true;
									 _->
										 false
								 end;
							 _->
								 true
						 end
				 end, ToFriendList).
	
persist_change()->
		case erase(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST) of
			?undefined->
				ignore;
			[]->
				ignore;
			List->
				Fun = fun()->
							  lists:map(fun(RoleID)->
												Info = get_ets_friend_enargy(RoleID),
												db_sql:set_enargy_friend(Info)
										end, List)
					  end,
				spawn(Fun)
		end.
persist_change(RoleID)->
	case get(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST) of
		?undefined->
			ignore;
		[]->
			ignore;
		List->
			put(?FRIEND_ENARGY_DATA_CHANGE_ROLEID_LIST,lists:delete(RoleID, List))
	end,
			Info = get_ets_friend_enargy(RoleID),
			db_sql:set_enargy_friend(Info),
	ets:delete(?ETS_ENARGY_FRIEND_DATA,RoleID).





