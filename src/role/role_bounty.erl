%% @author lxl
%% @doc @todo Add description to trSpecial.


-module(role_bounty).
-compile(export_all).
-include("def_role.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% cs_bounty_info(_) ->
%% 	Type = get_ets_bounty_info(),
%% 	?sendself(#sc_bounty_info{todayType=Type}).

cs_bounty_self_info(_) ->
	#bounty_info{time_limit=EndSec,bountyData=BountyData}	= role_data:get_bountyInfo(),
    BloodData = get_chapter_blood_info(),
%% 	{_,BuyGold} = data_bounty:get(buyTimesInfo),
	?sendself(#sc_bounty_self_info{endTime=EndSec,data=BountyData,buyGold=[],bloodData=BloodData}).

cs_bounty_buy_times(#cs_bounty_buy_times{type=Type
                                        ,but_times = ButTimes}) ->
	case check_buy_times(Type,ButTimes) of
		{true, Role, CostGold,BountyInfo,BountyData,OtherData} ->
			do_buy_times(Role,CostGold,Type, BountyInfo,BountyData,OtherData,ButTimes);
		{false,Reason,BountyData} ->
            #p_bounty_data{restTimes=RestTimes,alreadyBuyTimes=AlreadyBuyTimes}=BountyData,
			?sendself(#sc_bounty_buy_times{result=Reason,type=Type,restTimes=RestTimes,alreadyBuyTimes=AlreadyBuyTimes})
	end.

cs_bounty_get_reward(#cs_bounty_get_reward{chapterID=ChapterID,type=Type}) ->
	case check_get_reward(ChapterID,Type) of
		{true, Chapter,OtherChapters,BountyInfo,BountyData,OtherData} ->
			do_get_reward(Chapter, OtherChapters,ChapterID,BountyInfo,BountyData,OtherData);
		{false,Reason} ->
			?sendself(#sc_bounty_get_reward{result=Reason,reward=[]})
	end.

cs_bounty_challenge(#cs_bounty_challenge{chapterID=ChapterID,type=Type}) ->
	case check_challenge(ChapterID,Type) of
		{true, Chapter, OtherChapters,RestTimes,BountyInfo,BountyData,OtherData} ->
			do_challenge(ChapterID, Chapter, OtherChapters,RestTimes,BountyInfo,BountyData,OtherData);
		{false,Reason} ->
			?sendself(#sc_bounty_challenge{result=Reason,blood=#p_bounty_chapter_blood{chapterID=0,nowBlood=0,totalBlood=0}})
			end.

gen_boss(Mons) ->
	{_,_MaxHp,GerList} = 
	lists:foldl(fun(0,Acc) ->  	Acc;
				   ({mon,GerTypeID,Level,Rank},{Pos,AccHp,AccList}) ->
						Boss0 = ger_attr:new_mon(GerTypeID, Level, Rank, [], []), 
						Boss  = Boss0#ger{gerBase=((Boss0#ger.gerBase)#gerBase{gerPos=Pos})},
						{Pos+1,AccHp + Boss#ger.gerHp,[Boss|AccList]}
				end,{1,0,[]},Mons),
	GerList.

do_challenge(ChapterID, Chapter, OtherChapters,RestTimes,BountyInfo,BountyData,OtherData) ->
	{BossData, _} = data_bounty:get({chapter,ChapterID}),
	{_,_,_,_,_,_,Tr,SkinID} = BossData,
	BossSkin = #skin_info{equip=SkinID},
	case Tr of
		{TrainerID,Level,Special} ->
			TrSpe = #trSpecial{trID = TrainerID,specialID = Special,roleLevel = Level,sp = 0,state = 0};
		_ ->
			TrSpe = #trSpecial{}
	end,
	{{_,TotalBlood},MonTeam} = role_data:get_bounty_fighters(ChapterID),
    ?INFO("do_challenge MonTeam(~w):~w",[ChapterID,MonTeam]),
	#role{roleID=RoleID} = role_data:get_roleInfo(),
	RoleFighterList = role_data:get_fighter_list(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
    SkinInfo = role_skin:get_skin_info(),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
    RoleFighterList2 = role_xbattle:get_add_buff(role_data:get_xbattle_data(),RoleFighterList),
    {Result, FightRecord, {_, NewBossStateList,_,_}} = role_fight:new(RoleID,RoleFighterList2, MonTeam, RoleLieuAdd,#add_attr{},TalentList,[],role_data:get_trSpecial(),TrSpe,false,SkinInfo,BossSkin,LegendAddList,[]),
    NewBossList = lists:foldl(fun(B,Acclist)-> 
                                      case lists:keysearch(B#ger.gerID, 1, NewBossStateList) of
                                          {value,{_,NewGerHp,_}} when NewGerHp > 0 ->
                                              [B#ger{gerHp = NewGerHp}|Acclist];
                                          _ ->
                                              Acclist
                                      end
                              end, [], MonTeam),
 %   ?INFO("do_challenge NewMonTeam(~w) -->> ~w",[NewBossStateList,NewBossList]),
    NewBlood = lists:foldl(fun(#ger{gerHp=Hp},Acc) -> Hp+Acc end, 0, NewBossList), 
    role_data:set_bounty_fighters({{NewBlood,TotalBlood},NewBossList},ChapterID),
    if Result =:= false ->
           BountyData2 = [BountyData#p_bounty_data{restTimes=RestTimes-1}|OtherData],
		   role_data:set_bountyInfo(BountyInfo#bounty_info{bountyData=BountyData2});
	   true ->
			NewChapter = Chapter#p_bounty_unit{isPassed = 1},
			NewChapters = 
				case lists:keytake(ChapterID+1,#p_bounty_unit.chapterID,OtherChapters) of
					false ->
						[NewChapter|OtherChapters];
					{value,NextChapter,OtherChapters2} ->
						[NewChapter,NextChapter#p_bounty_unit{isOpened=1}] ++ OtherChapters2
				end,
            BountyData2 = [BountyData#p_bounty_data{chapterInfo=NewChapters,restTimes=RestTimes -1}|OtherData],
			role_data:set_bountyInfo(BountyInfo#bounty_info{bountyData=BountyData2})
	end,
	?sendself(#sc_bounty_challenge{result=1,fightInfo=[role_battle:change_fighter_transmigration(FightRecord,BossSkin)]
                                  ,blood=#p_bounty_chapter_blood{chapterID=ChapterID,nowBlood=NewBlood,totalBlood=TotalBlood}}).

check_challenge(ChapterID,Type)->
    #bounty_info{bountyData=BountyDataAll} = BountyInfo = role_data:get_bountyInfo(),
    case lists:keytake(Type, #p_bounty_data.type, BountyDataAll) of
        false -> {false, 5};
        {value, BountyData=#p_bounty_data{restTimes=RestTimes,chapterInfo=ChapterList}, OtherData } ->
            if RestTimes =< 0 ->
                   {false,4};
               true ->
                   case lists:keytake(ChapterID,#p_bounty_unit.chapterID,ChapterList) of
                       false ->
                           {false,5};
                       {value,Chapter,OtherChapters} ->
                           #p_bounty_unit{isOpened=IsOpened,isPassed=IsPassed}=Chapter,
                           case IsOpened of
                               0 ->
                                   {false,2};
                               1 ->
                                   case IsPassed of
                                       1 ->
                                           {false,3};
                                       0 ->
                                           {true, Chapter, OtherChapters,RestTimes,BountyInfo,BountyData,OtherData}
                                   end
                           end
                   end
            end
    end.

do_get_reward(Chapter,OtherChapters, ChapterID,BountyInfo,BountyData,OtherData) ->
	NewChapter = Chapter#p_bounty_unit{isGetReward=1},
	NewChapters = [BountyData#p_bounty_data{chapterInfo=[NewChapter|OtherChapters]}|OtherData],
	role_data:set_bountyInfo(BountyInfo#bounty_info{bountyData=NewChapters}),
	{_BossData, Reward} = data_bounty:get({chapter,ChapterID}),
	role_reward:handle_sell_reward_f(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_BOUNTY_CHAPTER,ChapterID,""),
	?sendself(#sc_bounty_get_reward{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward)]}).

check_get_reward(ChapterID,Type) ->
	#bounty_info{bountyData=BountyDataAll} = BountyInfo = role_data:get_bountyInfo(),
    case lists:keytake(Type, #p_bounty_data.type, BountyDataAll) of
        false -> {false, 4};
        {value,#p_bounty_data{chapterInfo=Chapters}=BountyData, OtherData} ->
	case lists:keytake(ChapterID,#p_bounty_unit.chapterID,Chapters) of
		false ->
			{false,4};
		{value,Chapter,OtherChapters} ->
			#p_bounty_unit{isGetReward=IsGetReward,isPassed=IsPassed}=Chapter,
			case IsGetReward of
				1 ->
					{false,2};
				0 ->
					case IsPassed of
						0 ->
							{false,3};
						1 ->
							{_BossData, Reward} = data_bounty:get({chapter,ChapterID}),
							case Reward of
								[]->
									{false,5};
								_ ->
							{true, Chapter,OtherChapters,BountyInfo,BountyData, OtherData}
								end
					end
			end
	end
    end.

do_buy_times(Role,CostGold,Type, BountyInfo,BountyData,OtherData,ButTimes)->
	#p_bounty_data{restTimes=RestTimes,alreadyBuyTimes=AlreadyBuyTimes}=BountyData,
	role_lib:deduct_gold_f(Role,CostGold,?MONEY_DEC_TYPE_BUY_BOUNTY_TIMES,AlreadyBuyTimes,""),
    BountyData2 = [BountyData#p_bounty_data{restTimes=RestTimes+ButTimes,alreadyBuyTimes=AlreadyBuyTimes+ButTimes}|OtherData],
	role_data:set_bountyInfo(BountyInfo#bounty_info{bountyData=BountyData2}),
	?sendself(#sc_bounty_buy_times{result=1,type=Type,restTimes=RestTimes+ButTimes,alreadyBuyTimes=AlreadyBuyTimes+ButTimes}).

check_buy_times(Type,ButTimes)->
    #bounty_info{bountyData=BountyDataAll} = BountyInfo = role_data:get_bountyInfo(),
    case lists:keytake(Type, #p_bounty_data.type, BountyDataAll) of
        false -> {false,3,#p_bounty_data{}};
        {value,#p_bounty_data{alreadyBuyTimes=AlreadyBuyTimes}=BountyData, OtherData}->
%%             {MaxBuyTimes, BuyTimesGold} = data_bounty:get(buyTimesInfo),
            CostGold = but_cost(ButTimes,AlreadyBuyTimes,0),
            Role = role_data:get_roleInfo(),
            case role_lib:check_money(Role,gold,CostGold) of
                true ->
                    {true, Role, CostGold,BountyInfo,BountyData,OtherData};
                _ ->
                    {false,2,BountyData}
            end
    end.

but_cost(0,_AlreadyBuyTimes,AccCost)->
    AccCost;
but_cost(ButTimes,AlreadyBuyTimes,AccCost)->
    CostGold = min(50,(AlreadyBuyTimes div 10) * 5 + 10),
    but_cost(ButTimes-1,AlreadyBuyTimes+1,AccCost+CostGold).

get_chapter_blood_info() ->
    lists:foldl(fun(Type,Acc1)->
                        ChapterList = data_bounty:get({type,Type}),
                        lists:foldl(fun(ChapterID,Acc) ->
                                            {{NowBlood,TotalBlood},_} = role_data:get_bounty_fighters(ChapterID),
                                            [#p_bounty_chapter_blood{chapterID=ChapterID,nowBlood=NowBlood,totalBlood=TotalBlood}|Acc]
                                    end,Acc1,ChapterList)
                        end, [], [1,2,3]).

init_bounty_info() ->
    init_bounty_info(#bounty_info{bountyData=[#p_bounty_data{alreadyBuyTimes = 0
                                                            ,type = Type
                                                            ,restTimes = 0
                                                            ,chapterInfo = 0}||Type<-[1,2,3]]}).
    
init_bounty_info(OldData) -> 
    Date = erlang:date(),
    TType = util:datetime_to_seconds({Date,{0,0,0}})+3*?ONE_DAY_SECONDS,
    NewBountyData =[init_bounty_data(Type,OldData)||Type <- [1,2,3]],
    #bounty_info{time_limit=TType
                 ,last_login=calendar:date_to_gregorian_days(Date)
                 ,bountyData=NewBountyData}.

init_bounty_data(TType,OldData) ->
%    ?INFO("init_bounty_data ~w",[TType]),
	ChapterList = data_bounty:get({type,TType}),
    [H|T] = lists:keysort(#p_bounty_unit.chapterID,lists:map(fun(ChapterID)->
                        {{Mon1,Mon2,Mon3,Mon4,Mon5,Mon6,_,_}, _} = data_bounty:get({chapter,ChapterID}),
                        Fighters = gen_boss([Mon1,Mon2,Mon3,Mon4,Mon5,Mon6]),
                        Blood = lists:foldl(fun(#ger{gerHp=GerHp},Acc) -> Acc + GerHp end, 0, Fighters),
                        %?INFO("init_bounty_data (~w) ~w",[Fighters,ChapterID]),
                        role_data:set_bounty_fighters({{Blood,Blood},Fighters},ChapterID),  %% {NowBlood, TotalBlood}, Fighters
                        #p_bounty_unit{chapterID=ChapterID,isOpened=0,isPassed=0,isGetReward=0}
                end, ChapterList)),
	Chapter = [H#p_bounty_unit{isOpened=1}|T],
    RestTimes = data_bounty:get(freeTimes),
    #p_bounty_data{alreadyBuyTimes=0,type=TType,restTimes=RestTimes,chapterInfo=Chapter}.

get_ets_bounty_info()->
	case ets:lookup(?ETS_BOUNTY, state) of
		[{_,_StartDate,Type}] ->
			Type;
		_ ->
			0
	end.
