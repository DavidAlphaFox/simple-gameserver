-module(role_treasurebowl).
-compile(export_all).

-include("def_role.hrl").
-include("def_item.hrl").

cs_treasurebowl_info(#cs_treasurebowl_info{})->
	treasurebowl_server:cs_treasurebowl_info().

cs_treasurebowl_exchange(#cs_treasurebowl_exchange{activityID=ActivityID})->
	case treasurebowl_server:cs_treasurebowl_exchange(ActivityID) of
		{false,Reason}->
			?sendself(#sc_treasurebowl_exchange{result=Reason});
		true->
			?sendself(#sc_treasurebowl_exchange{result=1})
	end.

cs_treasurebowl_draw(#cs_treasurebowl_draw{activityID=ActivityID,drawID=DrawID})->
	case treasurebowl_server:cs_treasurebowl_draw(ActivityID,DrawID) of
		{false,Reason}->
			?sendself(#sc_treasurebowl_draw{result=Reason});
		{true,Reward}->
			?sendself(#sc_treasurebowl_draw{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward)]})
	end.

add_pay_info(PayGold)->
	RoleID = role_data:get_roleID(),
	treasurebowl_server:add_pay_info(RoleID,PayGold).
treasurebowl_activity2p_treasurebowl_activity(?undefined)->
	[];
treasurebowl_activity2p_treasurebowl_activity(L) when is_list(L)->
	[treasurebowl_activity2p_treasurebowl_activity(E)||E<-L];
treasurebowl_activity2p_treasurebowl_activity(#treasurebowl_activity{activityID=ActivityID,drawlist=DrawList,activitystate=ActivityState})->
	{Type,Condition,_DrawListC} = data_treasurebowl:get({data_treasurebowl_activity,ActivityID}),
	PTreasurebowlDrawL = treasurebowl_drawunit2p_treasurebowl_draw(DrawList),
	#p_treasurebowl_activity{activityID=ActivityID,activitystate=ActivityState,drawlist=PTreasurebowlDrawL,type=Type,condition=Condition}.

treasurebowl_drawunit2p_treasurebowl_draw(L) when is_list(L)->
	[treasurebowl_drawunit2p_treasurebowl_draw(E)||E<-L];
treasurebowl_drawunit2p_treasurebowl_draw(#treasurebowl_drawunit{drawID=DrawID,period=Period,state=State})->
	{Period,Reward} = data_treasurebowl:get({data_treasurebowl_draw,DrawID}),
	PRewardInfo = activity_server:sell_reward2p_reward_info(Reward),
	#p_treasurebowl_draw{drawID=DrawID,state=State,period=Period,reward=PRewardInfo}.

