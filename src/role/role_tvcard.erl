%% @author lixinglong

-module(role_tvcard).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([]).
-record(tvcard_info,{
					 isTurnOver=0
					 ,pos=0
					,dis = 0
					 ,reward=#sell_reward{}
					}).

%% ====================================================================
%% API functions
%% ====================================================================

cs_tvcard_info(_) ->
	#tvcard{activityID=ActivityID,cards=Cards,openID=OpenID0} = role_data:get_tvcard(),
	{NowActivityID,StartTime,EndTime} = data_tvcard:get(activity),
	Now = util:now(),
	EndSec = util:datetime_to_seconds(EndTime),
	StartSec = util:datetime_to_seconds(StartTime),
	case EndSec < Now  orelse StartSec > Now of
		true ->
			?sendself(#sc_tvcard_info{activityID=0,endTime=EndSec,cards=[],costGold=[]});
		_ ->
			case NowActivityID of
				ActivityID ->
					Cards2 = 
					case Cards of
						[] ->
							OpenID=init_openID(),
							init_cards();
						_ ->
							OpenID=OpenID0,
							Cards
					end,
					
					role_data:set_tvcard(#tvcard{activityID=NowActivityID,cards=Cards2,openID=OpenID}),
					?sendself(#sc_tvcard_info{activityID=NowActivityID,cards=tvtransform(Cards2),endTime=EndSec,
											  costGold=[activity_server:sell_reward2p_reward_info(P)
													   ||P<-data_tvcard:get(costGold)]});
				_ ->
					OpenID=init_openID(),
					Cards2 = init_cards(),
					role_data:set_tvcard(#tvcard{activityID=NowActivityID,cards=Cards2,openID=OpenID}),
					?sendself(#sc_tvcard_info{activityID=NowActivityID, cards=tvtransform(Cards), endTime=EndSec,
												  costGold=[activity_server:sell_reward2p_reward_info(P)
													   ||P<-data_tvcard:get(costGold)]})
			end
	end.

cs_tvcard_select(#cs_tvcard_select{pos=Pos}) ->
	case check_can_select(Pos) of
		{true,NC,Cost,Cards,TVCARD} ->
			do_select(NC,Cost,Cards,TVCARD,Pos);
		{false,Reason} ->
			?sendself(#sc_tvcard_select{result=Reason,reward=tvtransform(#tvcard_info{})})
	end.
	

cs_tvcard_rand(_) ->
	#tvcard{cards=Cards} =TVCard= role_data:get_tvcard(),
	Cards2 = util:random_list2(Cards),
	role_data:set_tvcard(TVCard#tvcard{cards=Cards2}),
	?sendself(#sc_tvcard_rand{cards=tvtransform(Cards2)}).
 
%% ====================================================================
%% Internal functions
%% ====================================================================

do_select(NC,{BagItem2,#role{roleID=RoleID},_BagEquip, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu},Cards0,TVCARD,Pos) ->
	{Cards0_1,_} = lists:foldl(fun(E,{Acc,C})->{[E#tvcard_info{dis=C}|Acc],C+1} end,{[],1},Cards0),
	Cards = lists:reverse(Cards0_1),
	{NowActivityID,_,_EndTime} = data_tvcard:get(activity),
	TVC=#tvcard_info{reward=Reward,dis=Dis}=lists:keyfind(NC,#tvcard_info.pos,Cards),
 	case Pos of
 		Dis ->
			TCards = lists:keystore(NC,#tvcard_info.pos,Cards,TVC#tvcard_info{isTurnOver=1});
		_ ->
			#tvcard_info{dis=DDis} = TVCdis = lists:nth(Pos, Cards),%lists:keyfind(Pos, #tvcard_info.pos, Cards),
			
			TVC2 = TVC#tvcard_info{dis=DDis,isTurnOver=1},
			TVCdis2 = TVCdis#tvcard_info{dis=Dis},
			Cards2_0 = lists:keystore(DDis,#tvcard_info.dis,Cards,TVC2),
			TCards = lists:keystore(Dis,#tvcard_info.dis, Cards2_0,TVCdis2)
	end,
	%UCards = [CC||CC=#tvcard_info{isTurnOver=V_ITO}<-TCards,V_ITO == 0],
	UCards = util:random_list2([CC||CC=#tvcard_info{isTurnOver=V_ITO}<-TCards,V_ITO == 0]),
	{Cards2_1,_}=lists:foldl(fun(E,{L1,L2})-> 
						case E#tvcard_info.isTurnOver of
							1 ->
								{[E|L1],L2};
							0 ->
								[E0|T]=L2,
								{[E0|L1],T}
						end
						end, {[],UCards}, TCards),	
	Cards2 = lists:reverse(Cards2_1),
	if NeedGold > 0 ->
	role_lib:deduct_gold_2_f(role_data:get_roleInfo(),NeedGold,?MONEY_DEC_TYPE_TVCARD,
							 NC,integer_to_list(NowActivityID),role_reward:transform2normal_reward(Reward));
	   true ->
		   ignore
	end,
	if NeedCoin > 0 ->
		   role_lib:deduct_coin_f(role_data:get_roleInfo(),NeedCoin,?MONEY_DEC_TYPE_TVCARD,NC,integer_to_list(NowActivityID));
	   true ->
		   ignore
	end,
	if NeedRepu > 0 ->
		   role_lib:deduct_reputation_f(role_data:get_roleInfo(),NeedRepu,?MONEY_DEC_TYPE_TVCARD,NC,integer_to_list(NowActivityID));
	   true ->
		   ignore
	end,
	{Date, _} = Time = erlang:localtime(),
	LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_TVCARD, NC, erlang:integer_to_list(NowActivityID)),
	role_data:set_bagItem(BagItem2),
	DelItemIDList = [E||#item{itemUID=E}<-DelList],
	UpdateList2 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateList],
	%% 提醒客户端更新物品
	?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
	?sendself(#sc_item_update{updateList=UpdateList2}),
	role_data:set_tvcard(TVCARD#tvcard{cards=Cards2}),
	role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_TVCARD,NC,integer_to_list(NowActivityID)),
	?sendself(#sc_tvcard_select{result=1,cards=tvtransform(Cards2),reward=tvtransform(TVC#tvcard_info{isTurnOver=1})}).

check_can_select(Pos) when Pos > 0 andalso Pos < 10 ->
	#tvcard{activityID=ActivityID,cards=Cards,openID=OpenID}=TVCARD = role_data:get_tvcard(),
	{NowActivityID,StartTime,EndTime} = data_tvcard:get(activity),
	Now = util:now(),
	EndSec = util:datetime_to_seconds(EndTime),
	StartSec = util:datetime_to_seconds(StartTime),
	case EndSec < Now orelse StartSec > Now of
		true ->
			{false,5};
		false ->
			Cards2 = 
			case ActivityID of
				NowActivityID ->
					case Cards of
						[] ->
							init_cards();
						_ ->
							Cards
					end;
				_ ->
					init_cards()
			end,
					C = length([V_ITO||#tvcard_info{isTurnOver=V_ITO}<-Cards2,V_ITO == 1]) + 1,
			if C > 9 ->
				   {false,2};
			   true ->
					Cost = lists:nth(C,data_tvcard:get(costGold)),
					NC = lists:nth(C,data_tvcard:get({openID,OpenID})),
					#tvcard_info{isTurnOver=IsTurnOver}=lists:keyfind(NC,#tvcard_info.pos,Cards),
					case IsTurnOver of
						0 ->
							case check_cost(role_data:get_roleInfo(),Cost) of
								{true,Para} ->
									{true,NC,Para,Cards2,TVCARD#tvcard{activityID=NowActivityID}};
								false ->
									{false,4}
							end;
						_ ->
							{false,3}
					end
			end
	end;
check_can_select(_) ->
	{false,2}.

check_cost(Role,#sell_reward{gold=Gold,coin=Coin,reputation=Repu,item=Item})->
	NeedList = [{gold,Gold},{coin,Coin},{reputation,Repu}
					|[{item,TypeID,Num}||#new_item{itemTypeID=TypeID,itemNum=Num}<-Item]],
	BagItem = role_data:get_bagItem(),
	item_lib:check_need_list(Role, NeedList, BagItem,[]).

init_cards()->
	IDList = data_tvcard:get(rewardIDList),
	RewardID = util:random_one_from_weigh_list(IDList),
	RewardList = data_tvcard:get({rewardID,RewardID}),
%	RewardList2 = util:random_list2(RewardList),
	{_,Cards} = lists:foldl(fun(Reward,{IDAcc,Acc})->
									C = #tvcard_info{isTurnOver=0,pos=IDAcc,reward=Reward},
									{IDAcc+1,[C|Acc]}
							 end,{1,[]},RewardList),
	Cards.
init_openID()->
	IDList = data_tvcard:get(openIDList),
	util:random_one_from_weigh_list(IDList).
tvtransform(Cards) when is_list(Cards)->
	[#p_tvcard_info{isTurnOver=ITO,reward=activity_server:sell_reward2p_reward_info(Reward)}
				   ||#tvcard_info{isTurnOver=ITO,reward=Reward}<-Cards];
tvtransform(#tvcard_info{isTurnOver=ITO,reward=Reward}) ->
	#p_tvcard_info{isTurnOver=ITO,reward=activity_server:sell_reward2p_reward_info(Reward)}.
