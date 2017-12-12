%% @author caohongyang
%% @doc 点将功能
%% Created 2013-4-23

-module(role_card).
-compile(export_all).
-include("def_role.hrl").
-include("def_weibo_share.hrl").
%% API functions
-export([]).

%消耗类型由后端配置，传给前端
-define(COST_TYPE_COIN, 1). %% 消耗类型为金币
-define(COST_TYPE_REPU, 2). %% 消耗类型为徽章
-define(COST_TYPE_GOLD, 3). %% 消耗类型为钻石

%% ====================================================================
%% API functions
%% ====================================================================
cs_card_activity_info(_) ->
	{_ID,Start,Stop} = data_card:get(card_time),
    {CostType,CostValue}=data_card:get(refresh_card_coin),
	?sendself(#sc_card_activity_card{startTime=util:datetime_to_seconds(Start), stopTime=util:datetime_to_seconds(Stop)
                                    , level = data_card:get(need_level),refresh_cost_type = CostType, refresh_cost_value = CostValue}). 

cs_card_get_list(#cs_card_get_list{}) ->
    #role{level=RoleLevel} = role_data:get_roleInfo(),
    %得到当前抽卡状态
	#cardInfo{cardList=CardListLast,openedCardList=OpenCardListLast,activityID=ActivityID} = _CardInfo = role_data:get_cardInfo(),
	{ID,Start,Stop} = data_card:get(card_time),
	StartTime = util:datetime_to_seconds(Start),
	StopTime = util:datetime_to_seconds(Stop),
	Now = util:now(),
    NeedLevel = data_card:get(need_level),
    %活动是否开启，等级是否满足
	if Now > StartTime andalso Now < StopTime andalso RoleLevel >= NeedLevel ->
		   {CardList, OpenCardList} = 
			   case ID of
				   ActivityID ->
					   {CardListLast, OpenCardListLast};
				   _ ->
					   {[],[]}
			   end,
		   %% 判断是否是第一次打开这个界面
		   if OpenCardList =:=[] andalso CardList =:= [] ->
				  %% 免费刷新一次
				  CardList2 = random_card_list(),
				  %CardInfo2 = CardInfo#cardInfo{cardList=CardList2,},
				  CardInfo2 = #cardInfo{cardList=CardList2, activityID=ID},
				  role_data:set_cardInfo(CardInfo2),
                  {CostType,CostValue} = data_card:get(1),
				  ?sendself(#sc_card_get_list{cardList=card2p_card(CardList2),openedCardList=[]
                                             ,draw_cost_type=CostType,draw_cost_value=CostValue});
			  true ->
                  {CostType,CostValue} = data_card:get(length(OpenCardList)+1),
				  ?sendself(#sc_card_get_list{cardList=card2p_card(CardList),openedCardList=OpenCardList
                                             ,draw_cost_type=CostType,draw_cost_value=CostValue})
		   end;
	   true ->
		   ?sendself(#sc_card_get_list{cardList=[], openedCardList=[],draw_cost_type=0,draw_cost_value=0})
	end.

%%  抽取卡片
cs_card_draw(#cs_card_draw{pos=Pos}) ->
    Role = role_data:get_roleInfo(),
	case check_draw() of
		{true, CardInfo, ?COST_TYPE_COIN, CostValue} ->
			Role2=role_lib:deduct_coin_f(Role, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD, 0, ""),
			do_draw(Role2, CardInfo, Pos,false,CostValue);
        {true, CardInfo, ?COST_TYPE_REPU, CostValue} ->
            Role2=role_lib:deduct_reputation_f(Role, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD, 0, ""),
            do_draw(Role2, CardInfo, Pos,false,CostValue);
        {true, CardInfo, ?COST_TYPE_GOLD, CostValue} ->
%%             Role2=role_lib:deduct_gold_f(Role, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD, 0, ""),
%%             do_draw(Role2, CardInfo, Pos);
			   do_draw(Role,CardInfo,Pos,true,CostValue);
		{false, Reason} ->
            #cardInfo{openedCardList=OpenCardListLast} = role_data:get_cardInfo(),
            {CostType,CostValue} = data_card:get(length(OpenCardListLast)+1),
			?sendself(#sc_card_draw{card=[],pos=Pos,result=Reason
                                   ,draw_cost_type=CostType,draw_cost_value=CostValue})
	end.

cs_card_onekey(_) ->
    Role = role_data:get_roleInfo(),
	case check_onekey() of
        {true, CardInfo, ?COST_TYPE_COIN, CostValue} ->
            Role2=role_lib:deduct_coin_f(Role, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD, 1, ""),
            do_onekey(Role2, CardInfo,false, CostValue);
        {true, CardInfo, ?COST_TYPE_REPU, CostValue} ->
            Role2=role_lib:deduct_reputation_f(Role, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD, 1, ""),
            do_onekey(Role2, CardInfo,false, CostValue);
        {true, CardInfo, ?COST_TYPE_GOLD, CostValue} ->
%%             Role2=role_lib:deduct_gold_f(Role, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD, 1, ""),
%%             do_onekey(Role2, CardInfo);
			   do_onekey(Role,CardInfo,true, CostValue);
		{false, Reason} ->
			?sendself(#sc_card_onekey{card=[],result=Reason})
			end.

cs_card_refresh(_) ->
    Role = role_data:get_roleInfo(),
	case check_refresh() of
		{true, CardInfo,?COST_TYPE_COIN,CostValue} ->
			role_lib:deduct_coin_f(Role, CostValue, ?MONEY_DEC_TYPE_REFRESH_CARD, 0, ""),
			do_refresh(CardInfo);
        {true, CardInfo,?COST_TYPE_REPU,CostValue} ->
            role_lib:deduct_reputation_f(Role, CostValue, ?MONEY_DEC_TYPE_REFRESH_CARD, 0, ""),
            do_refresh(CardInfo);
        {true, CardInfo,?COST_TYPE_GOLD,CostValue} ->
            role_lib:deduct_gold_f(Role, CostValue, ?MONEY_DEC_TYPE_REFRESH_CARD, 0, ""),
            do_refresh(CardInfo);
		{true, CardInfo} ->
			do_refresh(CardInfo);
		{false, Reason}->
            #cardInfo{openedCardList=OpenCardListLast} = role_data:get_cardInfo(),
            {CostType,CostValue} = data_card:get(length(OpenCardListLast)+1),
			?sendself(#sc_card_refresh{cardList=[],result=Reason,draw_cost_type=CostType,draw_cost_value=CostValue})
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

do_refresh(CardInfo) ->
			CardList = random_card_list(),
			CardInfo2 = CardInfo#cardInfo{cardList=CardList,openedCardList=[]},
			role_data:set_cardInfo(CardInfo2),
            {CostType,CostValue} = data_card:get(1),
			?sendself(#sc_card_refresh{cardList=card2p_card(CardList), result=1,draw_cost_type=CostType,draw_cost_value=CostValue}).

check_refresh() ->
	#cardInfo{activityID=ActivityID} = CardInfoT = role_data:get_cardInfo(),
	{ID,Start,Stop} = data_card:get(card_time),
	StartTime = util:datetime_to_seconds(Start),
	StopTime = util:datetime_to_seconds(Stop),
	Now = util:now(),
	if Now > StartTime andalso Now < StopTime ->
		   #cardInfo{cardList=CardList,activityID=ActivityID} = CardInfo = 
				case ID of
					ActivityID ->
						CardInfoT;
					_ ->
						#cardInfo{cardList=random_card_list(),openedCardList=[],activityID=ActivityID}
				end,
		   %#cardInfo{cardList=CardList} = CardInfo = role_data:get_cardInfo(),
		   case length(CardList) of
			   6 ->
				   {CostType,CostValue} = data_card:get(refresh_card_coin),
                   case check_cost(CostType,CostValue) of
                       true ->
                           {true,CardInfo,CostType,CostValue};       
                       _ ->
                           {false,2}
                   end;
			   _ ->
				   {true, CardInfo}
		   end;
	   true ->
		   {false,5}
	end.

do_draw(Role2, CardInfo, Pos,IsRecord,CostValue) ->
    #role{roleID=RoleID,roleName=RoleName} = Role2,
%%	#role{roleID=RoleID} = Role,
%% 	%% 写道具日志
%% 	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogAcc),
%% 	{Date, _} = Time = erlang:localtime(),
%% 	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_DRAW_CARD, 0, ""),

	#cardInfo{openedCardList=OpenCardList,cardList=CardList, drawCount=DrawCount} = CardInfo,
	NewCount = DrawCount+1,
	{CardList2, GroupID, Type, Value} = random_card(NewCount, CardList),	
	if IsRecord ->
	Role=role_lib:deduct_gold_f(Role2, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD
								, 0, "",role_reward:typed_reward_transform_normal([{Type,Value,1}]));
	   true ->
		   Role=Role2
	end,
	OpenCardList2 = [#p_opened_card{pos=Pos,type=Type,value=Value}|OpenCardList],
	CardInfo2 = CardInfo#cardInfo{cardList=CardList2,drawCount=NewCount,openedCardList=OpenCardList2},
	role_data:set_cardInfo(CardInfo2),
    NextPos = case length(OpenCardList) of
                  6 ->
                      1;
                  _ ->
                      Pos+1
              end,
    {CostType,CostValue} = data_card:get(NextPos),
	?sendself(#sc_card_draw{card=[#p_card{type=Type,value=Value}],result=1,pos=Pos
                           ,draw_cost_type=CostType,draw_cost_value=CostValue}),
	role_reward:handle_card_reward_f(Type, Value),
    
    %% 推送消息的条件
    case Type of
        1 ->
            #data_ger{gerTypeID=GerTypeID, gerStar=Star} = data_ger:get(Value),   
            if
                Star >= 5 ->
                    GerView = #p_ger_view{gerQuality=0, gerLevel=1, gerTypeID=GerTypeID},
                    broadcast_server:bc_msgID(10042, [RoleName, GerView, 1]);
                true ->
                    ignore
            end;

        2 ->
            #data_item{itemType = ItemType,itemTypeID=ItemTypeID, itemLevel=ItemLevel, itemRank=ItemRank, itemStar=Star} = data_item:get(Value),
            if 
                Star >= 5 andalso ItemType =/= other ->
                    ItemView = #p_item_view{itemTypeID=ItemTypeID, itemLevel=ItemLevel, itemRank=ItemRank, itemNum=Value},
                    broadcast_server:bc_msgID(10041, [RoleName, ItemView, 1]); 
                true ->
                    ignore
            end;

        _ ->
            ignore
    end,
	%% 增加可微博分享的事件
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_CARD).

-define(POS_LIST, [0,1,2,3,4,5]).

%% do_onekey(Role, CardInfo, DelAcc, UpdateItemLogAcc) ->
do_onekey(Role2, CardInfo,IsRecord,CostValue) ->
	#role{roleName=RoleName,roleID=_RoleID} = Role2,
%% 	%% 写道具日志
%% 	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateItemLogAcc),
%% 	{Date, _} = Time = erlang:localtime(),
%% 	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_ONEKEY_DRAW_CARD, 0, ""),

	#cardInfo{openedCardList=OpenCardList,cardList=CardList, drawCount=DrawCount} = CardInfo,
	UsedPosList = [E||#p_opened_card{pos=E}<-OpenCardList],
	PosList = ?POS_LIST--UsedPosList,
	NewOpenedCardList = lists:zipwith(fun(Pos,Card) ->do_onekey2(RoleName, Card, Pos) end, PosList, CardList),
	if IsRecord ->
		   Role=role_lib:deduct_gold_f(Role2, CostValue, ?MONEY_DEC_TYPE_DRAW_CARD, 1, ""
									   ,role_reward:typed_reward_transform_normal(
										 [{T,V,1}||#p_opened_card{type=T,value=V}<-NewOpenedCardList]));
	   true ->
		   Role=Role2
	end,
	CardInfo2 = CardInfo#cardInfo{cardList=[],drawCount=DrawCount+1,openedCardList=OpenCardList++NewOpenedCardList},
	role_data:set_cardInfo(CardInfo2),
	?sendself(#sc_card_onekey{result=1,card=NewOpenedCardList}),
	%% 增加可微博分享的事件
	role_invite:do_add_weibo_share_mark(?SHARE_TYPE_CARD).


do_onekey2(RoleName, Card, Pos) ->
	#card{groupID=GroupID,type=Type,value=Value}=Card,
	role_reward:handle_card_reward_f(Type, Value),
	case GroupID >= 5 of
		true ->
			broadcast_server:bc(#sc_message_best_card{roleName=RoleName,type=Type,value=Value});
		false ->
			ignore
	end,
	#p_opened_card{pos=Pos,type=Type,value=Value}.
				  

random_card(DrawCount, CardList) ->
	case data_fixed_card:get(DrawCount) of
		GroupID when is_integer(GroupID) ->
			case lists:keytake(GroupID, #card.groupID, CardList) of
				false ->
					random_card2(CardList);
				{value, Card, CardList2} ->
					{CardList2, GroupID, Card#card.type,Card#card.value}
			end;
		_ ->
			random_card2(CardList)
	end.

random_card2([#card{groupID=GroupID, type=Type, value=Value}]) ->
	{[], GroupID, Type, Value};
random_card2(CardList) ->
	Config = data_card:get(prob),
	RandomList = [lists:nth(GroupID,Config) || #card{groupID=GroupID} <- CardList],
	Total = lists:sum(RandomList),
	Random = random:uniform()*Total,
	RandomSeq=
	util:foldl(fun(E,{Seq, Acc}) ->
					   Acc2 = E+Acc,
					   if  Acc2 >= Random ->
							   {return, Seq+1};
						   true ->
							   {Seq+1, Acc2}
					   end
			   end, {0, 0}, RandomList),
	?DEBUG("random=~w,randomList=~w,randomSeq=~w",[Random, RandomList, RandomSeq]),
	{value, #card{groupID=TGroupID, type=Type,value=Value}, CardList2} = util:nth_take(RandomSeq, CardList),
	{CardList2, TGroupID, Type, Value}.
	  
%% 抽取（翻拍）前检测
check_draw() ->
	#cardInfo{activityID=ActivityID} = CardInfoT = role_data:get_cardInfo(),
	{ID,Start,Stop} = data_card:get(card_time),
	StartTime = util:datetime_to_seconds(Start),
	StopTime = util:datetime_to_seconds(Stop),
	Now = util:now(),
    #role{level=RoleLevel} = role_data:get_roleInfo(),
    NeedLevel = data_card:get(need_level),
	if Now > StartTime andalso Now < StopTime andalso RoleLevel >= NeedLevel ->
		   #cardInfo{cardList=CardList,openedCardList=OpenCardList,activityID=ActivityID} = CardInfo = 
			   case ID of
				   ActivityID ->
					   CardInfoT;
				   _ ->
					   #cardInfo{cardList=random_card_list(),openedCardList=[],activityID=ActivityID}
			   end,
		   %#cardInfo{openedCardList=OpenCardList,cardList=CardList} = CardInfo = role_data:get_cardInfo(),
		   case CardList of
			   [] ->
				   {false, 3};
			   _ ->
				   Seq = length(OpenCardList) +1,
				   {CostType,CostValue} = data_card:get(Seq),
                   case check_cost(CostType,CostValue) of
                        true ->
                            {true, CardInfo, CostType,CostValue};
                        false->
                            {false, 2}
                   end
		   end;
	   true ->
		   {false,5}
	end.

check_onekey() ->
	#cardInfo{activityID=ActivityID} = CardInfoT = role_data:get_cardInfo(),
	{ID,Start,Stop} = data_card:get(card_time),
	StartTime = util:datetime_to_seconds(Start),
	StopTime = util:datetime_to_seconds(Stop),
	Now = util:now(),
	if Now > StartTime andalso Now < StopTime ->
		   #cardInfo{cardList=CardList,openedCardList=OpenCardList,activityID=ActivityID} = CardInfo = 
				case ID of
					ActivityID ->
						CardInfoT;
					_ ->
						#cardInfo{cardList=random_card_list(),openedCardList=[],activityID=ActivityID}
				end,
		   %#cardInfo{openedCardList=OpenCardList,cardList=CardList} = CardInfo = role_data:get_cardInfo(),
		   case CardList of
			   [] ->
				   {false, 3};
			   _ ->
				   Seq = length(OpenCardList) +1,
				   {CostType,CostValue} = cacl_onekey_need(Seq, length(OpenCardList)+length(CardList)),
                   case check_cost(CostType,CostValue) of
                        true ->
                            {true, CardInfo, CostType,CostValue};
                        false->
                            {false, 2}
                   end
		   end;
	   true ->
		   {false,5}
	end.

%% 计算一键所需
cacl_onekey_need(SeqStart, SeqEnd) ->
	lists:foldl(fun(Seq,{_,AccValue}) ->
						{CostType,CostValue} = data_card:get(Seq),
						{CostType,AccValue + CostValue}
				end, {?undefined,0}, lists:seq(SeqStart,SeqEnd)).

	
card2p_card(CardList) ->
	[#p_card{type=Type,value=Value}||#card{type=Type,value=Value}<-CardList].
random_card_list() ->
	Config = data_card:get(random),
	List = 
	[begin 
		 {Type,Value} = util:random_one_from_list(List),
		 #card{groupID=GroupID, type=Type,value=Value}
	 end || {GroupID, List} <- Config],
	util:random_list2(List).

%% 根据玩家当前徽章数，返回消费类型。
check_cost(CostType,CostValue)->
    Role = role_data:get_roleInfo(),
    case CostType of
        ?COST_TYPE_COIN ->
            role_lib:check_money(Role, coin, CostValue);
        ?COST_TYPE_REPU ->
            role_lib:check_money(Role, reputation, CostValue);
        ?COST_TYPE_GOLD ->
            role_lib:check_money(Role, gold, CostValue);
        _ ->
            ?ERR("something is wrong with config file about data_card."),
            false
    end.
    
%%     case role_lib:check_money(Role, reputation, NeedReputation) of
%%         true ->
%%             {NeedReputation,0};
%%         false ->
%%             NeedGold = NeedReputation div data_card:get(cost_gold_ratio),
%%             case role_lib:check_money(Role, gold, NeedGold) of
%%                 true ->
%%                     {0, NeedGold};
%%                 false ->
%%                     {false,NeedGold}
%%             end
%%     end.
%% will delete
%% check_cost(NeedReputation)->
%%     Role = role_data:get_roleInfo(),
%%     case role_lib:check_money(Role, reputation, NeedReputation) of
%%         true ->
%%             {NeedReputation,0};
%%         false ->
%%             NeedGold = NeedReputation div data_card:get(cost_gold_ratio),
%%             case role_lib:check_money(Role, gold, NeedGold) of
%%                 true ->
%%                     {0, NeedGold};
%%                 false ->
%%                     {false,NeedGold}
%%             end
%%     end.
