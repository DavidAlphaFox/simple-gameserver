-module(role_xbattle).
-compile(export_all).
-include("def_role.hrl").


cs_xbattle_info(_)->
    #role_xbattle{chapterID=ChapterIDB,buyQuickCount=BQC,buyQuickToday=BQT
                 ,quickFightSec=QFC,raidTimes=RaidTimes} = role_data:get_xbattle_data(),
    ChapterID = case ChapterIDB of 0 -> case data_common:get(xbattle_first_chapter)of X when is_integer(X) -> X; _ -> 1001 end; D -> D end,
    #data_xbattle_chapter{dropID=DropID} = data_xbattle_chapter:get(ChapterID),
    {_TriID,RewardB} = get_triReward(DropID,ChapterID),
    #sell_reward{coin=Coin,roleExp=Exp} = add_familyTec_buff(RewardB),
    ?sendself(#sc_xbattle_info{chapterID=ChapterID,buyQuickCount=BQC,coin=Coin
                              ,buyQuickToday=BQT,quickFightSec=QFC,exp=Exp,raidTimes=RaidTimes}).

cs_xbattle_challenge(#cs_xbattle_challenge{chapterID=ChapterID}) ->
    case check_challenge(ChapterID) of 
        {true, RoleTimes,Xbattle,Dungeon,IsPass} ->
            do_challenge(ChapterID,Xbattle,RoleTimes,Dungeon,IsPass);
        {false, Reason} ->
            ?sendself(#sc_xbattle_challenge{result=Reason, reward=[],dungeonID=0,fightInfo=[]})
    end.

cs_xbattle_raid(#cs_xbattle_raid{chapterID=ChapterID,tryTimes=TryTimes}) ->
    case check_raid(ChapterID,TryTimes) of
        {true,RoleTimes,Xbattle,RaidTimes,TrueRaidTimes,DungeonInfo,EnergyCost,GoldCost} ->
            do_raid1(ChapterID, Xbattle,RaidTimes,RoleTimes,TrueRaidTimes,DungeonInfo,EnergyCost,GoldCost);
        {false,Reason} ->
            ?sendself(#sc_xbattle_raid{result=Reason,raidData=[]})
    end.

cs_xbattle_set_reward_chapter(#cs_xbattle_set_reward_chapter{chapterID=ChapterID}) ->
    #role_xbattle{chapterID=NowChapterID} = Xbattle = role_data:get_xbattle_data(),
    if ChapterID > NowChapterID -> ?sendself(#sc_xbattle_set_reward_chapter{result=2});
       true -> role_data:set_xbattle_data(Xbattle#role_xbattle{rewardChapterID=ChapterID}),
            ?sendself(#sc_xbattle_set_reward_chapter{result=1})
    end.
    

%% cs_xbattle_raid(#cs_xbattle_raid{dungeonID=DungeonID})->
%%     case check_raid(DungeonID) of 
%%         {true, BagOther, DelAcc,UpdateAcc,UpdateLogList,UseTimes,UseInfo} ->
%%             do_raid(DungeonID,BagOther, DelAcc,UpdateAcc,UpdateLogList,UseTimes,UseInfo);
%%         {false,Reason} ->
%%             ?sendself(#sc_xbattle_raid{result=Reason, reward=[]})
%%     end.

cs_xbattle_offline_reward(_)->
    #role_xbattle{offlineInfo={TriC,OffSec,Reward}} = XBattle = role_data:get_xbattle_data(),
    role_data:set_xbattle_data(XBattle#role_xbattle{offlineInfo={0,0,#sell_reward{}}}),
    RewardView = activity_server:sell_reward2p_reward_info(Reward),
    if TriC == 0 -> ?sendself(#sc_xbattle_offline_reward{offlineSec=OffSec,triCount=0,reward=[]});
       true -> ?sendself(#sc_xbattle_offline_reward{offlineSec=OffSec,triCount=TriC,reward=[RewardView]})
    end.

cs_xbattle_use_elixir(#cs_xbattle_use_elixir{itemUID=ItemUID,useNum=UseNum}) ->
    case check_use_elixir(ItemUID,UseNum) of
        {true, ItemUse, ItemUseList,Item,BagOther2,TimeDiff,NewUseTimes} ->
            do_use_elixir(Item,ItemUse,ItemUseList,BagOther2,TimeDiff,UseNum,NewUseTimes);
        {false,Reason} ->
            ?sendself(#sc_xbattle_use_elixir{result=Reason,triCount=0,reward=[]})
    end.

cs_xbattle_buy_quick(_)->
    case check_buy_quick() of
        {true,Xbattle,NeedGold,RoleInfo} ->
            do_buy_quick(Xbattle,NeedGold,RoleInfo);
        {false,Reason} ->
            ?sendself(#sc_xbattle_buy_quick{result=Reason,quickFightSec=0,triCount=0,reward=[]})
    end.

cs_xbattle_get_pass_reward(_)->
    case check_get_pass_reward() of
        {true, Xbattle, Chapter,ChapterInfo} ->
            do_get_pass_reward(Xbattle,Chapter,ChapterInfo);
        {false,Reason,ChapterID} ->
            ?sendself(#sc_xbattle_get_pass_reward{result=Reason,reward=[],newChapterID=ChapterID})
    end.

cs_xbattle_chapter_info(#cs_xbattle_chapter_info{chapterID=ChapterID}) ->
    #role_xbattle{chapterID=NowChapterID} = role_data:get_xbattle_data(),
    if ChapterID < NowChapterID -> #xbattle_chapter{passDungeons=PD,isGetReward=IG} = get_chapter2(ChapterID);
       true ->  #xbattle_chapter{passDungeons=PD,isGetReward=IG} = get_chapter(ChapterID)
    end,
    case ChapterID of
        NowChapterID -> 
            #data_xbattle_chapter{dropID=DropID} = data_xbattle_chapter:get(ChapterID),
            {_TriID,RewardB} = get_triReward(DropID,ChapterID);
        _ ->     
        NowChapterID2 = case NowChapterID of 0 -> ChapterID;_ -> NowChapterID end,
            #data_xbattle_chapter{dropID=DropID} = data_xbattle_chapter:get(NowChapterID2),
            {_,RewardB} = get_triReward(DropID,NowChapterID)
    end,
    #sell_reward{coin=Coin,roleExp=Exp} = add_familyTec_buff(RewardB),
    PD2 = [#p_xbattle_dungeon{dungeonID=DID,times=DTs}||{DID,DTs}<-PD],
    ?sendself(#sc_xbattle_chapter_info{isGetReward=IG,passDungeons=PD2,coin=Coin,exp=Exp}).

cs_xbattle_start_reward(_)->
    #role_xbattle{chapterID=NChapterID}=  Xbattle = role_data:get_xbattle_data(),
    case NChapterID of 0 ->
                           FirstChapter = case data_common:get(xbattle_first_chapter)of X when is_integer(X) -> X; _ -> 1001 end,
                           role_data:set_xbattle_data(Xbattle#role_xbattle{chapterID=FirstChapter,rewardChapterID=FirstChapter});
        _ -> ignore
    end,
    ?sendself(#sc_xbattle_start_reward{result=1}).

cs_xbattle_challenge_boss(_) ->
    case check_challenge_boss() of
        {true,ChapterID,Chapter,Dungeon,RoleTimes,CostEnerg} -> 
            do_challenge_boss(ChapterID,Chapter,Dungeon,RoleTimes,CostEnerg);
        {false,Reason} ->
            ?sendself(#sc_xbattle_challenge_boss{result=Reason,fightInfo=[]})
    end.

check_challenge_boss() ->
    #role_xbattle{chapterID=ChapterID} = role_data:get_xbattle_data(),
    Chapter = #xbattle_chapter{isGetReward=IsGetReward}=get_chapter(ChapterID),
    case IsGetReward of
        0 -> {false,2};
        2 -> {false,3};
        3 -> {false,3};
        1 -> #data_xbattle_chapter{bossDungeonID=BossDungeonID}=data_xbattle_chapter:get(ChapterID),
             case data_xbattle_dungeon:get(BossDungeonID) of
                 ?undefined -> {false,4};
                 Dungeon=#data_xbattle_dungeon{costEnergy=CostEnergy} ->
                     #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
                     if Energy >= CostEnergy ->
                     {true,ChapterID,Chapter,Dungeon,RoleTimes,CostEnergy};
                        true ->
                            {false,6}
                     end
             end                 
    end.
    

do_challenge_boss(ChapterID,Chapter,Dungeon,RoleTimes,CostEnerg) ->
    #data_xbattle_dungeon{dungeonID=_DungeonID,monTeam=MonTeam,trSpecial=TrSpecialDungeon,displayID=DisplayID,displayQuality=DisplayQuality,isCollect=IC} = Dungeon,
    %Reward = get_challenge_reward(RewardBase,DungeonID),
    RoleFighterList = role_data:get_fighter_list(),
    RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentList = role_talent:get_active_talent_list(),
    SkinInfo = role_skin:get_skin_info(),
    LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
    Xbattle=role_data:get_xbattle_data(),
    RoleFighterList2 = get_add_buff(Xbattle,RoleFighterList),
    {Result, FightRecord,_} = role_fight:new(role_data:get_roleID(),RoleFighterList2, MonTeam, RoleLieuAdd,#add_attr{},TalentList,[],role_data:get_trSpecial(),TrSpecialDungeon,false,SkinInfo,#skin_info{},LegendAddList,[]),
    case Result of 
        false -> ?sendself(#sc_xbattle_challenge_boss{result=5,fightInfo=[FightRecord]});
        _->
            case IC of 1 -> role_gather:add_ger_manual([{DisplayID,DisplayQuality}]);
                _ -> ignore
            end,
            role_lib:deduct_energy_f(role_data:get_roleInfo(),RoleTimes, CostEnerg,?MONEY_DEC_TYPE_XBATTLE_CHALLENGE_BOSS,ChapterID,0),
            set_chapter(Chapter#xbattle_chapter{isGetReward=2}),
            role_maintask:do_main_task(?BATTLE_FINISH,{ChapterID}),
            role_payGuide:trigger_task_change(?BATTLE_FINISH,{ChapterID}),
            %#xbattle_chapter{isGetReward=IsGetReward}=Chapter,
            ?sendself(#sc_xbattle_challenge_boss{result=1,fightInfo=[FightRecord]})
    end.

cs_xbattle_package_full(_) ->  put(?role_equip_bag_full, full).
cs_xbattle_package_ok(_)->  put(?role_equip_bag_full, 0).
can_drop_equip() ->  get(?role_equip_bag_full) ==  full.
check_bag_full()-> 
    Bag = role_data:get_bagEquip(), 
    Len = length(Bag),
    case Len >= data_xbattle:get(bag_limit) of 
        true -> put(?role_equip_bag_full, full);
        _ -> put(?role_equip_bag_full, Len)
    end.

drop_reward(Reward=#sell_reward{item=ItemList}) ->
    case can_drop_equip() of
        false -> Reward;
        _ -> ItemList2 = %[I||#new_item{itemTypeID=T}=I<-ItemList,#data_item{} = data_item:get(T),not item_lib:is_itemType_any_equip(T)],
                lists:foldl(fun(#new_item{itemTypeID=T}=I,Acc) ->
                                    #data_item{itemType=Type} = data_item:get(T),
                                    case item_lib:is_itemType_any_equip(Type) of true -> Acc;
                                        _ -> [I|Acc]
                                    end
                                    end, [], ItemList),
             Reward#sell_reward{item=ItemList2} 
    end.

drop_reward2(Reward=#sell_reward{item=ItemList}) ->
    case get(?role_equip_bag_full)  of
        0 -> Reward;
        full -> ItemList2 = 
                 lists:foldl(fun(#new_item{itemTypeID=T}=I,Acc) ->
                                     #data_item{itemType=Type} = data_item:get(T),
                                     case item_lib:is_itemType_any_equip(Type) of 
                                         true -> Acc;
                                         _ -> [I|Acc]
                                     end
                             end, [], ItemList),
             Reward#sell_reward{item=ItemList2} ;
        N -> Max = data_xbattle:get(bag_limit) ,
             {ItemList2,NowC} = 
                 lists:foldl(fun(#new_item{itemTypeID=T,itemNum=M}=I,{Acc,Cnt}) ->
                                     #data_item{itemType=Type} = data_item:get(T),
                                     %?ERR("check:~w",[[item_lib:is_itemType_any_equip(Type),Cnt,Max]]),
                                     case item_lib:is_itemType_any_equip(Type) of 
                                         true -> 
                                             if Cnt >= Max -> {Acc,Cnt};
                                                true ->
                                                    M2 = if M + Cnt >= Max -> Max - Cnt;true -> M end,
                                                    {[I#new_item{itemNum=M2}|Acc],Cnt+M2}
                                             end;
                                         _ -> {[I|Acc],Cnt}
%%                                      end;
%%                                 (#new_item{itemTypeID=T}=I,{Acc,Cnt}) ->
%%                                      #data_item{itemType=Type} = data_item:get(T),
%%                                      case item_lib:is_itemType_any_equip(Type) of 
%%                                          true -> {[I|Acc],Cnt+1};
%%                                          _ -> {[I|Acc],Cnt}
                                     end
                             end, {[],N}, ItemList),
             Full = if NowC >= Max -> full; true -> 0 end,  
             put(?role_equip_bag_full, Full),
             Reward#sell_reward{item=ItemList2} 
    end.

check_get_pass_reward()->
    #role_xbattle{chapterID=ChapterID} = Xbattle = role_data:get_xbattle_data(),
    #xbattle_chapter{passDungeons=Dungeons,isGetReward=IsGetReward}=ChapterInfo=get_chapter(ChapterID),
    #data_xbattle_chapter{dungeonCount=DungeonCount}=Chapter = data_xbattle_chapter:get(ChapterID),
    case length(Dungeons) of DungeonCount -> 
                                 case IsGetReward of 2 -> {true,Xbattle,Chapter,ChapterInfo};
                                     _-> {false,3,ChapterID}
                                 end;
                             _ -> {false,2,ChapterID}
    end.

do_get_pass_reward(Xbattle,Chapter,ChapterInfo) ->
    #data_xbattle_chapter{reward=Reward1,nextChapterID=NextChapterID,chapterID=ChapterID} = Chapter,
    NextChapterID2 = case NextChapterID of 0 -> ChapterID;_ -> NextChapterID end,
    MainGerTypeID = role_data:get_main_gerTypeID(),
    Reward = if is_list(Reward1) -> {_,R} = lists:keyfind(MainGerTypeID, 1, Reward1),R; true -> Reward1 end,
    role_data:set_xbattle_data(Xbattle#role_xbattle{chapterID=NextChapterID2,rewardChapterID=NextChapterID2,passData=[]}),
    set_chapter(ChapterInfo#xbattle_chapter{isGetReward=3}),
    RoleInfo = role_data:get_roleInfo(),
    role_reward:handle_sell_reward_f(RoleInfo,Reward,?MONEY_ADD_TYPE_XBATTLE_PASS_CHAPTER,ChapterID,""),
    ?sendself(#sc_xbattle_get_pass_reward{result=1,reward=[activity_server:sell_reward2p_reward_info(Reward)],newChapterID=NextChapterID2}).
    


do_buy_quick(Xbattle,Needgold,RoleInfo)->
    #role_xbattle{buyQuickToday=BQT,buyQuickCount=BQC,rewardChapterID=_RChapterID,chapterID=NChapterID,quickFightSec=QFS} = Xbattle,
    ChapterID = NChapterID,%case RChapterID of NChapterID -> NChapterID; _ -> NChapterID - 1 end,
    NewBQC = BQC+1,
    RoleInfo2 = role_lib:deduct_gold_f(RoleInfo,Needgold,?MONEY_DEC_TYPE_XBATTLE_BUY_QUICK, NewBQC,""),
    {OneAddSec,BaseAddSec,MaxAddSec,ValidSec}= data_xbattle:get(buy_quick_add),
%    NewSec = erlang:min(NewBQC*OneAddSec,MaxAddSec) + util:now()+BaseAddSec,
    Now = util:now(),
    NewSec = if QFS >= Now -> QFS + ValidSec; true -> ValidSec + Now end,
    RewardSec = BaseAddSec + erlang:min(NewBQC*OneAddSec, MaxAddSec),
    if BQC == 0 andalso NChapterID == 10004 -> 
           {Tric,RewardB} = data_xbattle:get(guide_buy_quick_reward);
       true ->
           {Tric,RewardB,_TCC}=get_time_diff_reward(RewardSec,0,ChapterID,false,0,#sell_reward{},0)
    end,
    Reward1 = add_familyTec_buff(RewardB),
    Reward = drop_reward(Reward1),
    role_reward:handle_sell_reward_f(RoleInfo2,Reward,?MONEY_ADD_TYPE_XBATTLE_BUY_QUICK, NewBQC,""),
    role_data:set_xbattle_data(Xbattle#role_xbattle{buyQuickToday=BQT+1,buyQuickCount=NewBQC,quickFightSec=NewSec}),
    ?sendself(#sc_xbattle_buy_quick{result=1,quickFightSec=NewSec,triCount=Tric,reward=[activity_server:sell_reward2p_reward_info(Reward)]}).

add_familyTec_buff(#sell_reward{coin=Coin,roleExp=RoleExp}=Reward) ->
    #role{familyID = FamilyID} = role_data:get_roleInfo(),
    FamilyTek_Battleout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,9,1),
    AddCoin2 = role_lib:calculate_familyTekeffectGenerate(Coin,FamilyTek_Battleout_Add),
    AddExp2 = role_lib:calculate_familyTekeffectGenerate(RoleExp,FamilyTek_Battleout_Add),
    Reward#sell_reward{coin=AddCoin2,roleExp=AddExp2}.

check_buy_quick()->
    #role_xbattle{buyQuickToday=BQT}=Xbattle = role_data:get_xbattle_data(),
    #role{vipLevel=VipLevel,svipLevel=SvipLevel}=RoleInfo = role_data:get_roleInfo(),
    VipInfo = role_lib:cacl_vip_level2(VipLevel,SvipLevel),
    BuyTimeList = data_xbattle:get(buy_quick_times),
    MaxBuy = case lists:keyfind(VipInfo, 1, BuyTimeList) of false -> 0;{_,N} -> N end,
    %{NeedGold,MaxBuy}  = data_xbattle:get({buy_quick,VipInfo}),
    if BQT >= MaxBuy -> {false,4};
       true ->
           NeedGold = case lists:nth(BQT+1, data_xbattle:get(buy_quick_cost)) of X when is_integer(X) -> X; _ -> 1000000000000 end,
           case role_lib:check_money(RoleInfo,gold,NeedGold) of
               false -> {false,3};
               _ -> {true,Xbattle,NeedGold,RoleInfo}
           end
    end.


do_use_elixir(Item,ItemUse,ItemUseList,BagOther2,TimeDiff,UseNum,LeftTimes)->
    #role_xbattle{chapterID=ChapterID} = role_data:get_xbattle_data(),
    #data_xbattle_chapter{dropID=DropID} = data_xbattle_chapter:get(ChapterID),
    #item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemNum=ItemNum}= Item,
    NewItem = Item#item{itemNum=ItemNum-UseNum},
    role_data:set_bagItem([NewItem|BagOther2]),
    ?sendself(#sc_item_update{updateList=[#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum-UseNum}]}),
    NewItemUseList = lists:keyreplace(ItemTypeID,#item_use_info.itemTypeID,ItemUseList,ItemUse),
    role_data:set_itemUseList(NewItemUseList),
    ?sendself(#sc_item_use_info{use_info_list=[#p_item_use_info{type_id=ItemTypeID,left_times=LeftTimes}]}),
    {WI,EI} = data_xbattle:get(trigger_interval),
    Interval = WI + EI,
    CC = trunc((TimeDiff * 60) / Interval) ,
    RewardB = add_tri_reward(CC,#sell_reward{},ChapterID,DropID),
    #role{roleID=RoleID} =RoleData = role_data:get_roleInfo(),
    Reward1 = add_familyTec_buff(RewardB),
    Reward = drop_reward(Reward1),
    role_reward:handle_sell_reward_f(RoleData,Reward,?MONEY_ADD_TYPE_XBATTLE_ELIXIR, ItemTypeID,integer_to_list(CC)),
    LogItemList = role_item:itemList2logItemList([NewItem], []),
    {Date, _} = Time = erlang:localtime(),
    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_XBATTLE_ELIXIR, 0, ""),
    ?sendself(#sc_xbattle_use_elixir{result=1,triCount=CC,reward=[activity_server:sell_reward2p_reward_info(Reward)]}).


check_use_elixir(ItemUID,UseNum)->
    BagOther = role_data:get_bagItem(),
    case lists:keytake(ItemUID,#item.itemUID, BagOther) of 
        false -> {false,2};
        {value,Item=#item{itemNum=ItemNum,itemTypeID=ItemTypeID},BagOther2} ->
            case ItemNum >= UseNum of
                false -> {false,2};
                _ ->
                    #data_item_use{maxTimes=MaxTimes,addValue=TimeDiff} = data_item_use:get(ItemTypeID),
                    ItemUseList = role_data:get_itemUseList(),
                    #item_use_info{useTimes=UseTimes}=ItemUse=lists:keyfind(ItemTypeID,#item_use_info.itemTypeID,ItemUseList),
                    NewUseTimes =UseTimes+UseNum, 
                    case NewUseTimes > MaxTimes andalso MaxTimes /= -1 of
                        true ->{false,3};
                        _ ->
                            {true, ItemUse#item_use_info{useTimes=NewUseTimes}, ItemUseList,Item,BagOther2,TimeDiff*UseNum,MaxTimes-NewUseTimes}
                    end
            end
    end.



%% check_raid(DungeonID) ->
%%     #role_xbattle{chapterID=ChapterIDNow} = role_data:get_xbattle_data(),
%%     #data_xbattle_dungeon{chapterID=ChapterID} = data_xbattle_dungeon:get(DungeonID),
%%     case is_challenge_chapter(DungeonID,ChapterIDNow,ChapterID) of 
%%         false -> {false, 2};
%%        true ->
%%            {CostTypeID, CostNum} = data_xbattle:get(raid_cost),
%%            #data_item_use{maxTimes=MaxTimes} = data_item_use:get(CostTypeID),
%%            ItemUseList = role_data:get_itemUseList(),
%%            #item_use_info{useTimes=UseTimes} = ItemUseInfo = lists:keyfind(CostTypeID, #item_use_info.itemTypeID, ItemUseList),
%%            if UseTimes + 1 > MaxTimes ->{false,4};
%%               true ->
%%                   case item_lib:chech_material(CostTypeID,CostNum) of
%%                       {true, BagOther, DelAcc,UpdateAcc,UpdateLogList} ->
%%                           NewItemUseInfo = ItemUseInfo#item_use_info{useTimes=UseTimes+1},
%%                           NewItemUseList = lists:keyreplace(CostTypeID, #item_use_info.itemTypeID, ItemUseList, NewItemUseInfo),
%%                           {true, BagOther, DelAcc,UpdateAcc,UpdateLogList,UseTimes+1,NewItemUseList};
%%                       false -> {false, 3}
%%                   end
%%            end
%%     end.
%% 
%% is_challenge_chapter(DungeonID, ChapterIDNow, ChapterID) ->
%%     if ChapterIDNow > ChapterID -> true;
%%        chapterIDNow < ChapterID -> false;
%%        true -> #xbattle_chapter{passDungeons=D}=get_chapter(ChapterID),
%%                case lists:member(DungeonID, D) of false -> false;
%%                    true -> true
%%                end
%%     end.
%% 
%% do_raid(DungeonID,BagOther, DelAcc,UpdateAcc,UpdateLogList,UseTimes,NewItemUseList) ->
%%    role_data:set_itemUseList(NewItemUseList),
%%    {Date,_}=Time=erlang:localtime(),
%%    role_data:set_bagItem(BagOther),
%%    #data_xbattle_dungeon{raidReward=RaidReward} = data_xbattle_dungeon:get(DungeonID),
%%    #role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
%% %    role_reward:handle_sell_reward_f(RoleInfo,Reward, ?MONEY_ADD_TYPE_XBATTLE_RAID, ChapterID,integer_to_list(UseTimes)),
%%    {_NewRole,_,RewardItemList,RewardGerList,_} = 
%%    role_reward:handle_dungeon_reward_f(RoleInfo, RaidReward,?MONEY_ADD_TYPE_XBATTLE_RAID,DungeonID,UseTimes),
%%    Reward = #sell_reward{item=RewardItemList,newGer= RewardGerList},
%%    LogItemList = role_item:itemList2logItemList(DelAcc,UpdateLogList),
%%    case DelAcc of [] -> ignore;_ ->
%%                         DelItemUIDList = [DItemUID||#item{itemUID=DItemUID}<-DelAcc],
%%                         ?sendself(#sc_item_delete_notify{itemUIDList=DelItemUIDList})
%%    end,
%%    case UpdateAcc of [] -> ignore; _ -> 
%%        UpdUIDList=[#p_item_num_update{itemUID=UItemUID,itemNum=UItemNum}||#item{itemUID=UItemUID,itemNum=UItemNum}<-UpdateAcc],
%%        ?sendself(#sc_item_update{updateList=UpdUIDList})
%%    end,
%%    behavior_item_consume:log(RoleID,LogItemList,Date,Time,?MONEY_DEC_TYPE_XBATTLE_RAID,DungeonID,""),
%%    RewardView = activity_server:sell_reward2p_reward_info(Reward),
%%    ?sendself(#sc_xbattle_raid{result=1, reward=RewardView}).

check_raid(ChapterID,TryTimes) ->
    #role_xbattle{chapterID=ChapterIDNow,raidTimes=RaidTimes} = Xbattle=role_data:get_xbattle_data(),
    #role{vipLevel=VipLevel} = _RoleInfo = role_data:get_roleInfo(),
    NeedVip = data_xbattle:get(raid_need_vip),
    if VipLevel < NeedVip -> {false,3};
       true ->
           MaxRaid = data_xbattle:get(max_raid),
           %if RaidTimes >= MaxRaid -> {false,5};
           %true ->
           if ChapterID > ChapterIDNow -> {false,2};
              true -> #roleTimes{energy=Energy}=RoleTimes=role_data:get_roleTimes(),
                      #data_xbattle_chapter{challengeDungeonInfo=DungeonInfo} = data_xbattle_chapter:get(ChapterID),
                      %DungeonID = util:random_one_from_weigh_list(DungeonInfo),
                      %#data_xbattle_dungeon{costEnergy=CostEnergy} = Dungeon = data_xbattle_dungeon:get(DungeonID),
                      TrueRaidTimes = erlang:min(MaxRaid, TryTimes),
                      {EnergyCost,GoldCost} = get_raid_cost(RaidTimes,TrueRaidTimes),
                      if Energy >= EnergyCost -> 
                             case role_lib:check_money(role_data:get_roleInfo(),gold,GoldCost) of
                                 true ->
                                     {true,RoleTimes,Xbattle,RaidTimes,TrueRaidTimes,DungeonInfo,EnergyCost,GoldCost};
                                 false -> {false,5}
                             end;
                         true ->{false,4}
                      end
           end
    %end
    end.


get_raid_cost(RaidTimes,TrueRaidTimes) ->
    {E,G,_,_} = 
    lists:foldl(fun({T1,T2,E,G},{EAcc,GAcc,RAcc,LAcc}=A)-> 
                       % io:format("~nxx:~w",[[T1,T2,RAcc,LAcc,EAcc,GAcc]]),
                        if %T2 =< RAcc -> A;
                           %T1 >= LAcc -> A;
                           T1 =< RAcc andalso T2 >= LAcc -> 
                               C1 = LAcc - RAcc,
                               {E*C1+EAcc,G*C1+GAcc,0,0};
                           T1 =< RAcc andalso T2 =< LAcc andalso T2 >= RAcc ->
                               C1 = T2 - RAcc,
                               {E*C1+EAcc,G*C1+GAcc,T2,LAcc};
                           true -> A
                        end
                end, {0,0,RaidTimes,RaidTimes+TrueRaidTimes}, data_xbattle:get(raid_cost)),
    {E,G}.

do_raid1(ChapterID, Xbattle0,RaidTimes,RoleTimes,TrueRaidTimes,DungeonInfo,EnergyCost,GoldCost) ->
    Xbattle=  Xbattle0#role_xbattle{raidTimes=RaidTimes+TrueRaidTimes},
    role_data:set_xbattle_data(Xbattle),
    {_,RoleInfo1} = role_lib:deduct_energy_f(role_data:get_roleInfo(),RoleTimes, EnergyCost,?MONEY_DEC_TYPE_XBATTLE_RAID,ChapterID,integer_to_list(TrueRaidTimes)),
    RoleInfo = role_lib:deduct_money_f(RoleInfo1, gold, GoldCost, ?MONEY_DEC_TYPE_XBATTLE_RAID, ChapterID,integer_to_list(TrueRaidTimes)),
        #role{roleID=RoleID}=RoleInfo,
    #xbattle_chapter{isGetReward=IsPass} = get_chapter(ChapterID),
    DungeonInfo2 = generate_raid_dungeonIDs(TrueRaidTimes,DungeonInfo,ChapterID),
    {RewardInfo,Reward,MarkList} = do_raid(TrueRaidTimes,DungeonInfo2,Xbattle,RoleID,[],#sell_reward{},[],[],IsPass),
    add_chapterInfo(ChapterID,MarkList),
    role_reward:handle_sys_reward(RoleInfo,Reward, ?MONEY_ADD_TYPE_XBATTLE_RAID,ChapterID,integer_to_list(TrueRaidTimes)),
    ?sendself(#sc_xbattle_raid{result=1,raidData=RewardInfo}).

generate_raid_dungeonIDs(TrueRaidTimes,DungeonInfo,ChapterID) ->
    case lists:member(ChapterID, data_xbattle:get(fixed_chapters)) of
        false -> [util:random_one_from_weigh_list(DungeonInfo)||_<-lists:duplicate( TrueRaidTimes,0)];
        _ ->            #data_xbattle_chapter{challengeDungeonInfo=DungeonInfo} = data_xbattle_chapter:get(ChapterID),
           FixedData = case data_xbattle:get({fixed_dungeon,ChapterID}) of
                           ?undefined -> [];
                           X1 -> X1
                       end,
           #xbattle_chapter{challengeCount=CC} =ChapterData= get_chapter(ChapterID),
                            set_chapter(ChapterData#xbattle_chapter{challengeCount=CC+TrueRaidTimes}),
           [get_fixed_chapter_dungeonID(CC2,FixedData,DungeonInfo)||CC2<-lists:seq(CC, CC+TrueRaidTimes-1)]
    end.


do_raid(T,_,_,_,RewardInfo,RewardAll,GatherList,MarkList,_) when T < 1 -> 
    role_gather:add_normal_ger_manual(GatherList),
    {RewardInfo,RewardAll,MarkList};
do_raid(TrueRaidTimes, [DungeonID|DungeonInfo],Xbattle,RoleID,RewardInfo,RewardAll,GatherList,MarkList,IsPass)->
    %DungeonID = util:random_one_from_weigh_list(DungeonInfo),
    #data_xbattle_dungeon{dungeonID=DungeonID,type=Type,monTeam=MonTeam,trSpecial=TrSpecialDungeon,chapterID=_ChapterID,isCollect=IC
                         ,displayID=DisplayID,reward=RewardBase,displayQuality=DisplayQuality,needChallengeTimes=NeedChallengeTimes} = data_xbattle_dungeon:get(DungeonID),
        Reward = get_challenge_reward(RewardBase,DungeonID),
    case Type of 
        3 ->
            RewardInfo2 = [#p_xbattle_raid{result=3,dungeonID=DungeonID,reward=[activity_server:sell_reward2p_reward_info(Reward)]}|RewardInfo],
            %role_reward:handle_sys_reward(RoleInfo,Reward, ?MONEY_ADD_TYPE_XBATTLE,ChapterID,integer_to_list(DungeonID)),
            do_raid(TrueRaidTimes-1,DungeonInfo,Xbattle,RoleID,RewardInfo2,merge_reward(RewardAll,Reward),GatherList,MarkList,IsPass);
        _ ->
            RoleFighterList = role_data:get_fighter_list(),
            RoleLieuAdd = role_data:get_lieu_add_attr(),
            TalentList = role_talent:get_active_talent_list(),
            SkinInfo = role_skin:get_skin_info(),
            LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
            RoleFighterList2 = get_add_buff(Xbattle,RoleFighterList),
            {Result, _,_} = role_fight:new(RoleID,RoleFighterList2, MonTeam, RoleLieuAdd,#add_attr{},TalentList,[],role_data:get_trSpecial(),TrSpecialDungeon,false,SkinInfo,#skin_info{},LegendAddList,[]),
            case Result of
                false -> RewardInfo2 = [#p_xbattle_raid{result=2,dungeonID=DungeonID,reward=[]}|RewardInfo],
                         do_raid(TrueRaidTimes-1,DungeonInfo,Xbattle,RoleID,RewardInfo2,RewardAll,GatherList,MarkList,IsPass);
                _ -> 
                    %role_reward:handle_sys_reward(RoleInfo,Reward, ?MONEY_ADD_TYPE_XBATTLE,ChapterID,integer_to_list(DungeonID)),
                    ResultM = 
                        case Type of
                            1 ->
                                case IsPass of 
                                    0 ->
                                        %add_chapterInfo(ChapterID,DungeonID,NeedChallengeTimes),
                                        case IC of 
                                            1 ->
                                                Gaaa= {DisplayID,DisplayQuality},
                                                GatherList2 = case lists:member(Gaaa,GatherList) of true -> GatherList; _ -> [Gaaa|GatherList] end;
                                            _ -> GatherList2 = GatherList
                                        end,
                                        MarkList2 = case lists:keytake(1, DungeonID, MarkList) of
                                                        false -> [{DungeonID,NeedChallengeTimes,1}|MarkList];
                                                        {value,{_,_,MAC},OMarkList} -> [{DungeonID,NeedChallengeTimes,MAC+1}|OMarkList]
                                                    end;
                                    _ -> GatherList2 = GatherList,
                                         MarkList2 = MarkList
                                end,
                                %role_gather:add_normal_ger_manual([{DisplayID,DisplayQuality}]),
                                1;
                            _ ->
                                MarkList2 = MarkList,
                                GatherList2 = GatherList,
                                4
                        end,
                    RewardInfo2 = [#p_xbattle_raid{result=ResultM,dungeonID=DungeonID,reward=[activity_server:sell_reward2p_reward_info(Reward)]}|RewardInfo],
                    do_raid(TrueRaidTimes-1,DungeonInfo,Xbattle,RoleID,RewardInfo2,merge_reward(RewardAll,Reward),GatherList2,MarkList2,IsPass)
            end
    end.

get_fixed_chapter_dungeonID(CC,FixedData,DungeonInfo) ->
    case lists:keyfind(CC+1, 1,FixedData) of
        false -> util:random_one_from_weigh_list(DungeonInfo);
        {_,DID} when is_integer(DID) -> DID;
        {_,LList} -> MainGerTypeID = role_data:get_main_gerTypeID(),
                     case lists:keyfind(MainGerTypeID, 1, LList) of
                         false -> {_,X1ID} = hd(LList), X1ID;
                         {_,X1ID} -> X1ID
                     end
    end.

check_challenge(ChapterID) ->
    IsFixed = lists:member(ChapterID, data_xbattle:get(fixed_chapters)),
    #role_xbattle{chapterID=ChapterIDNow,challengeCount=N,passData=PassData}=Xbattle=role_data:get_xbattle_data(),
    if ChapterIDNow == 0 orelse (IsFixed andalso ChapterID =< ChapterIDNow )->     
           ChapterID2 = case ChapterIDNow of
                            0 -> case data_common:get(xbattle_first_chapter)of X when is_integer(X) -> X end;
                            _ -> ChapterID
                        end,
           #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
           #data_xbattle_chapter{challengeDungeonInfo=DungeonInfo} = data_xbattle_chapter:get(ChapterID2),
           FixedData = case data_xbattle:get({fixed_dungeon,ChapterID2}) of
                           ?undefined -> [];
                           X1 -> X1
                       end,
           #xbattle_chapter{challengeCount=CC,isGetReward=IsPass} =ChapterData= get_chapter(ChapterID2),
           DungeonID = get_fixed_chapter_dungeonID(CC,FixedData,DungeonInfo),
           set_chapter(ChapterData#xbattle_chapter{challengeCount=CC+1}),
           #data_xbattle_dungeon{costEnergy=CostEnergy}=Dungeon=data_xbattle_dungeon:get(DungeonID),
           if Energy >= CostEnergy -> {true,RoleTimes,Xbattle#role_xbattle{challengeCount=N+1},Dungeon,IsPass};
              true ->{false,4}
           end;
       true ->
           if ChapterID =< ChapterIDNow ->
                  #roleTimes{energy=Energy} = RoleTimes = role_data:get_roleTimes(),
                  #xbattle_chapter{challengeCount=CC,isGetReward=IsPass} =ChapterData= get_chapter(ChapterID),
                  DungeonID = 
                      case lists:member(ChapterID, data_xbattle:get(fixed_pass)) of
                          false -> 
                              NewPassData = PassData,
                              random_dungeonID_from_chapter(ChapterID);
                          true ->
                              {FixedChapterID,FixedChapterData} = 
                                  case PassData of {A,B} -> {A,B};
                                      [] -> generate_fixed_chapter_data(ChapterID)
                                  end,
                              NewPassData = {FixedChapterID,FixedChapterData},
                              if FixedChapterID == ChapterID ->
                                     case lists:keyfind(CC+1, 1, FixedChapterData) of
                                         false -> random_dungeonID_from_chapter(ChapterID);
                                         {_,DungeonIDL} ->DungeonIDL
                                     end;
                                 true -> random_dungeonID_from_chapter(ChapterID)
                              end
                      end,
                  #data_xbattle_dungeon{costEnergy=CostEnergy}=Dungeon=data_xbattle_dungeon:get(DungeonID),
                  if Energy >= CostEnergy -> 
                         set_chapter(ChapterData#xbattle_chapter{challengeCount=CC+1}),
                         {true,RoleTimes,Xbattle#role_xbattle{challengeCount=N+1,passData=NewPassData},Dungeon,IsPass};
                     true ->{false,4}
                  end;
              true -> {false,2}
           end
    end.

generate_fixed_chapter_data(ChapterID) ->
    {L1,L2} = data_xbattle:get({fixed_dungeon,ChapterID}),
    {ChapterID,lists:zip(L1,util:random_list(L2))}.

random_dungeonID_from_chapter(ChapterID) ->
    #data_xbattle_chapter{challengeDungeonInfo=DungeonInfo} = data_xbattle_chapter:get(ChapterID),
    util:random_one_from_weigh_list(DungeonInfo).

get_challenge_reward(#sell_reward{item=Items,newGer=Gers}=Reward,ChapterID) ->
        RandomSelectBonus = role_reward:getDropBonus(ChapterID),
        {RewardItemList,RewardGerList} = role_reward:partition_drop(RandomSelectBonus),
        Reward#sell_reward{newGer=RewardGerList++Gers,item=RewardItemList++Items}.
do_challenge(ChapterID, Xbattle,RoleTimes,Dungeon,IsPass) ->
    #data_xbattle_dungeon{dungeonID=DungeonID,type=Type,displayID=DisplayID,costEnergy=CostEnergy,needChallengeTimes=NeedChallengeTimes
                          ,monTeam=MonTeam,trSpecial=TrSpecialDungeon,reward=RewardBase,displayQuality=DisplayQuality,isCollect=IC} = Dungeon,
    {_,RoleInfo} = role_lib:deduct_energy_f(role_data:get_roleInfo(),RoleTimes, CostEnergy,?MONEY_DEC_TYPE_XBATTLE_CHALLENGE, ChapterID,integer_to_list(DungeonID)),
    Reward = get_challenge_reward(RewardBase,DungeonID),
    #role{roleID=RoleID}=RoleInfo,
    %?ERR("re:~w",[[RoleID,Reward,DungeonID]]),
    role_data:set_xbattle_data(Xbattle),
    case Type of 
        3 ->
            role_reward:handle_sys_reward(RoleInfo,Reward, ?MONEY_ADD_TYPE_XBATTLE,ChapterID,integer_to_list(DungeonID)),
            ?sendself(#sc_xbattle_challenge{result=5,fightInfo=[],dungeonID=DungeonID,
                                            reward=[activity_server:sell_reward2p_reward_info(Reward)]});
        _ ->
            RoleFighterList = role_data:get_fighter_list(),
            RoleLieuAdd = role_data:get_lieu_add_attr(),
            TalentList = role_talent:get_active_talent_list(),
            SkinInfo = role_skin:get_skin_info(),
            LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-RoleFighterList],
            RoleFighterList2 = get_add_buff(Xbattle,RoleFighterList),
            {Result, FightRecord,_} = role_fight:new(RoleID,RoleFighterList2, MonTeam, RoleLieuAdd,#add_attr{},TalentList,[],role_data:get_trSpecial(),TrSpecialDungeon,false,SkinInfo,#skin_info{},LegendAddList,[]),
            case Result of
                false -> ?sendself(#sc_xbattle_challenge{result=3,reward=[],fightInfo=[FightRecord],dungeonID=DungeonID});
                _ -> 
                    ResultR = case Type of 
                                  1 ->
                                      case IsPass  of 
                                          0 ->
                                              add_chapterInfo(ChapterID,DungeonID,NeedChallengeTimes,1),
                                              case IC of 1 -> role_gather:add_normal_ger_manual([{DisplayID,DisplayQuality}]);
                                                  _ -> ignore
                                              end;
                                          _ -> ignore
                                      end,
                                      1;
                                  _ -> 6 
                              end,
                    role_reward:handle_sys_reward(RoleInfo,Reward, ?MONEY_ADD_TYPE_XBATTLE,ChapterID,integer_to_list(DungeonID)),
                    ?sendself(#sc_xbattle_challenge{result=ResultR,fightInfo=[FightRecord],dungeonID=DungeonID,
                                                    reward=[activity_server:sell_reward2p_reward_info(Reward)]})
            end
    end.

get_add_buff(#role_xbattle{quickFightSec=QFS},RoleFighterList)->
    Now = util:now(),
    if Now >= QFS -> RoleFighterList;
       true -> Add = data_xbattle:get(buy_quick_add_buff),
               [begin
                    #add_attr{gerExtraDemageRate=RR}=GE,
                    GE2 = GE#add_attr{gerExtraDemageRate=RR+Add},
                    F#ger{gerEnchantAdd=GE2}
                end||#ger{gerEnchantAdd=GE}=F<-RoleFighterList]
    end.

%get_mon_team(ChapterID) ->
%    (data_xbattle_chapter:get(ChapterID))#data_xbattle_chapter.monTeam.
%get_mon_team(ChapterID) ->
%    case ets:lookup(?ETS_DUNGEON_MON_CACHE, ChapterID) of
%        [] -> Team = get_mon_team2(ChapterID),
%              ets:insert(?ETS_DUNGEON_MON_CACHE, {ChapterID, Team}),
%              Team;
%        [{_,Team}] -> Team
%    end.
%get_mon_team2(ChapterID) ->
%    0.

xbattle_time_up()->
    #role_xbattle{rewardChapterID=_RChapterID,chapterID=NChapterID}=Xbattle=role_data:get_xbattle_data(),
    ChapterID = NChapterID,%case RChapterID of NChapterID -> NChapterID; _ -> NChapterID - 1 end,
    Now = util:now(),
    {WInterval, EInterval} = data_xbattle:get(trigger_interval),
    add_tri_plan(Xbattle,WInterval+EInterval, Now),
    if ChapterID > 0 ->
           #data_xbattle_chapter{dropID=DropID} = data_xbattle_chapter:get(ChapterID),
           {TriID,RewardB} = get_triReward(DropID,ChapterID),
           RoleInfo = role_data:get_roleInfo(),
           Reward1 = add_familyTec_buff(RewardB),
           Reward = drop_reward(Reward1),
           role_reward:handle_sell_reward_f(RoleInfo,Reward,?MONEY_ADD_TYPE_XBATTLE_TRI, 0,""),
           ?sendself(#sc_xbattle_tri{triID=TriID,reward=[activity_server:sell_reward2p_reward_info(Reward)]});
       true -> ignore
    end,
    role_data:set_xbattle_data(Xbattle#role_xbattle{lastTriggerSec=Now}).


add_tri_plan(Xbattle)-> 
    {WInterval, EInterval} = data_xbattle:get(trigger_interval),
    %Dec = get_family_dec_interval(),
    add_tri_plan(Xbattle, WInterval+EInterval,util:now()).
add_tri_plan(Xbattle,Interval,Now)->
    #role_xbattle{lastTriggerSec=LastTriSec}=Xbattle,
    NextTick= LastTriSec + Interval , 
    if NextTick - Now > 0 -> timer_wheel:add_plan(NextTick, fun xbattle_time_up/0);
       true -> timer_wheel:add_plan(Now+Interval,fun role_xbattle:xbattle_time_up/0)
    end.

get_family_dec_interval()->
    {Add,Now} = 
        case get(?familyTecDec) of
            {RecAdd,RecNow} ->{RecAdd,RecNow+RecAdd};
            _ -> {0,0}
        end,
    {Now2,Ret} = if Now >= 10000 -> {erlang:max(Now - 10000, 0),1};
                    true -> {Now,0}
                 end,
    put(?familyTecDec,{Add,Now2}),
    Ret.

    %#role_xbattle{offlineInfo={TriC,OffSec,Reward}} = XBattle = role_data:get_xbattle_data(),
cacl_offline_reward(XBattle=#role_xbattle{lastTriggerSec=LastTriSec2}) ->
    Now = util:now(),
    #role_xbattle{chapterID=ChapterID} = XBattle,
    LastTriSec = if LastTriSec2 == 0 -> Now; true -> LastTriSec2 end,
    TimeDiff = erlang:min(Now - LastTriSec,data_xbattle:get(max_offline_time_diff)),
    IntervalBase = data_xbattle:get(offline_down_interval),
    {Tric,Reward1,TCC}=get_time_diff_reward(TimeDiff,IntervalBase,ChapterID,true,0,#sell_reward{},0),
    RoleInfo = role_data:get_roleInfo(),
    check_bag_full(),
    Reward = drop_reward2(Reward1),
    role_reward:handle_sell_reward_f(RoleInfo,Reward,?MONEY_ADD_TYPE_XBATTLE_OFFLINE, Tric,""),
    LastTriggerSec2=erlang:min(LastTriSec + TCC,Now),
    XBattle#role_xbattle{offlineInfo={Tric,TimeDiff,Reward},lastTriggerSec=LastTriggerSec2}.

get_time_diff_reward(_TimeDiff, _Interval,0,_IsOffline,_Count,_RewardList,_TCA) -> {0,#sell_reward{},0};
get_time_diff_reward(TimeDiff, Interval,ChapterID,IsOffline,Count,RewardList,TCA) ->
    {WI, EI} = data_xbattle:get(trigger_interval),
    #data_xbattle_chapter{dropID=DropID} = data_xbattle_chapter:get(ChapterID),
    EAdd = case get(?familyTecDec) of {TTAdd,_} -> TTAdd/10000; _ -> 0 end,
    get_time_diff_reward(TimeDiff, Interval,WI,EI-EAdd,ChapterID,DropID,IsOffline,Count,RewardList,TCA).
get_time_diff_reward(0,_,_,_,_,_,_,Count,RewardList,TCC)-> {Count,merge_reward_item(RewardList),trunc(TCC)};
get_time_diff_reward(TimeDiff, Interval,WI,EI,ChapterID,DropID,IsOffline,Count,RewardList,TCA) ->
    TimeDiff2 = erlang:max(TimeDiff - Interval, 0),
    Pass = erlang:max(TimeDiff - TimeDiff2,0),
    II = WI + EI,
    CC = trunc(Pass / II) + 1,
    TimeDiff3 = erlang:max(TimeDiff2 - II, 0),
    WI2 =if IsOffline ->  trunc(data_xbattle:get(offline_down_ratio) * WI) + 1;
       true -> WI
    end,
    RewardList2 = add_tri_reward(CC,RewardList,ChapterID,DropID),
%        ?ERR("tt:~w",[RewardList]), %role_xbattle:cacl_offline_reward(#role_xbattle{chapterID=10002,lastTriggerSec=1495007008}).
    get_time_diff_reward(TimeDiff3,Interval,WI2,EI,ChapterID,DropID,IsOffline,Count+CC, RewardList2,TCA+CC*II).

add_tri_reward(0,RewardList,_ChapterID,_DropID)-> RewardList;
add_tri_reward(CC,RewardList,ChapterID,DropID)-> 
    {_,Reward} = get_triReward(DropID,ChapterID),
    add_tri_reward(CC-1,merge_reward(Reward,RewardList),ChapterID,DropID).

refresh_xbattle()->
    Xbattle = role_data:get_xbattle_data(),
    role_data:set_xbattle_data(Xbattle#role_xbattle{buyQuickToday=0,raidTimes=0}).

get_triReward(DropID,_ChapterID)->
    #data_xbattle_drop{baseReward=BaseReward,drop=Drop} = data_xbattle_drop:get(DropID),
    {TriID,DropList} = util:random_one_from_weigh_list(Drop),
    #reward{coin=Coin,roleExp=RoleExp,gold=Gold,reputation=Repu} = BaseReward,
    RandomSelect = role_reward:random_drop(DropList),
    {RewardItemList,RewardGerList} = role_reward:partition_drop(RandomSelect),
    {TriID,#sell_reward{roleExp=RoleExp,coin=Coin,gold=Gold,reputation=Repu,newGer=RewardGerList,item=RewardItemList}}.

get_chapter(ChapterID)->
    case get({?chapter_x,ChapterID}) of
        Info when is_record(Info,xbattle_chapter)-> Info;
        _ -> RoleID =role_data:get_roleID(),
             case db_sql:get_xbattle_chapter(RoleID, ChapterID) of
                 Chapter when is_record(Chapter,xbattle_chapter)  -> Chapter;
                 _ -> NewChapter = new_chapter(RoleID,ChapterID),
                      set_chapter(NewChapter),
                      NewChapter 
             end
    end.
get_chapter2(ChapterID)->
    case get({?chapter_x,ChapterID}) of
        Info when is_record(Info,xbattle_chapter)-> Info;
        _ -> RoleID =role_data:get_roleID(),
             case db_sql:get_xbattle_chapter(RoleID, ChapterID) of
                 Chapter when is_record(Chapter,xbattle_chapter)  -> Chapter;
                 _ -> NewChapter = new_chapter2(RoleID,ChapterID),
                      set_chapter(NewChapter),
                      NewChapter 
             end
    end.
set_chapter(#xbattle_chapter{chapterID=ChapterID}=Chapter)->
    put({?chapter_x, ChapterID},Chapter).

new_chapter(_RoleID,ChapterID)-> #xbattle_chapter{chapterID=ChapterID}.
new_chapter2(_RoleID,ChapterID)-> 
    #data_xbattle_chapter{challengeDungeonInfo=CL} = data_xbattle_chapter:get(ChapterID),
    D = [I||{_,I}<-CL,I>20000],
    #xbattle_chapter{chapterID=ChapterID,passDungeons=D,isGetReward=1}.


add_chapterInfo(ChapterID,MarkList) ->
    [add_chapterInfo(ChapterID,DungeonID,NeedChallengeTimes,AddTimes)
    ||{DungeonID,NeedChallengeTimes,AddTimes}<-MarkList].
add_chapterInfo(ChapterID,DungeonID,NeedChallengeTimes,AddTimes)->
    Chapter=#xbattle_chapter{passDungeons=D,passCount=PC,isGetReward=IsGetReward}=get_chapter(ChapterID),
    #data_xbattle_chapter{dungeonCount=DungeonCount} = data_xbattle_chapter:get(ChapterID),
    {LastACTimes,LastOD} = 
        case lists:keytake(DungeonID, 1, D) of
        false -> {0,D};
        {value,{_,ACTimes},OD} -> {ACTimes,OD}
    end,
    {NowACTimes,IsPass} = 
        if LastACTimes + AddTimes >= NeedChallengeTimes -> {NeedChallengeTimes,LastACTimes < NeedChallengeTimes};
           true -> {LastACTimes + AddTimes, false}
        end,
    D2 = [{DungeonID,NowACTimes}|LastOD],
    PC2 = if IsPass -> PC + 1; true -> PC end,
    IsGetReward2 = if DungeonCount == PC2 andalso DungeonCount > PC -> 1; true -> IsGetReward end,
    set_chapter(Chapter#xbattle_chapter{passDungeons=D2,passCount = PC2,isGetReward=IsGetReward2}).


load_data_xbattle_dungeon(List) -> 
    F = fun(Chapter)->
                #data_xbattle_dungeon{monTeam=MonTeam,trSpecial=Tr,type=Type,buff=Buff} =  Chapter,
                case Type of 
                    3 ->Chapter;
                    _ ->
                        MonTeam2 = gen_mon_team(MonTeam,Buff),
                        TrSpe = 
                            case Tr of 0 -> #trSpecial{};
                                {TrID,Level,Special} -> #trSpecial{trID=TrID,specialID=Special,roleLevel=Level,sp=0,state=0}
                            end,
                        Chapter#data_xbattle_dungeon{monTeam=MonTeam2,trSpecial=TrSpe,displayID=get_displayID(MonTeam)}
                end
        end,
    [F(L)||L<-List].
gen_mon_team(List,Buff)->
    IDList = [ID||{_,ID,_,_}<-List],
    [begin 
         %#ger{gerBase=GerBase=#gerBase{gerTypeID=GerTypeID}}=
                 Ger =ger_attr:new_mon(GTypeID,GLevel,GRank,[],lists:delete(GTypeID, IDList)),
                 Ger2 = ger_attr:add_enter_skill_attr(Ger,Buff),
         %#data_ger{baseTypeID=GerTypeID2} =data_ger:get(GerTypeID),
         %Ger2 = Ger#ger{gerBase=GerBase#gerBase{gerTypeID=GerTypeID2}},
         ?change_pos(Ger2,Pos)
     end ||{Pos,GTypeID,GRank,GLevel}<-List].

get_displayID(MonTeam) ->
    lists:foldl(fun({5,GerTypeID,_,_},_Acc) -> 
                        %#data_ger{baseTypeID=GerTypeID2} =data_ger:get(GerTypeID),
                        GerTypeID;
                   (_,Acc) ->Acc
                end, 5010, MonTeam).


merge_reward(RewardList) ->
    lists:foldl(fun(R1,R2)-> merge_reward(R1,R2) end,#sell_reward{}, RewardList).
merge_reward(R1,R2) ->
    #sell_reward{coin=R1#sell_reward.coin   + R2#sell_reward.coin
                 ,roleExp=R1#sell_reward.roleExp + R2#sell_reward.roleExp
                 ,gerExp=R1#sell_reward.gerExp + R2#sell_reward.gerExp
                 ,gold=R1#sell_reward.gold + R2#sell_reward.gold
                 ,item=R1#sell_reward.item ++ R2#sell_reward.item
                 ,reputation = R1#sell_reward.reputation + R2#sell_reward.reputation
                 ,newGer = R1#sell_reward.newGer ++ R2#sell_reward.newGer
                }.

merge_reward_item(Reward=#sell_reward{item=ItemList}) ->
    ItemList2 = lists:foldl(fun({_,Type,Num,_,_}=I,Acc)->
                                    case lists:keytake(Type, 2, Acc) of
                                        false -> [I|Acc];
                                        {value,{A,B,C,D,E},O} -> [{A,B,C+Num,D,E}|O]
                                    end end, [], ItemList),
    Reward#sell_reward{item=lists:reverse(lists:keysort(2, ItemList2))}.


mark_ger_display(DisplayID)->
    List = get_ger_display(),
    case lists:member(DisplayID, List) of
        true -> ignore;
        _ -> set_ger_display([DisplayID|List])
    end.

get_ger_display() ->
    case get(?xbattle_display) of 
        L when is_list(L) -> L;
        _ -> []
    end.

set_ger_display(List)->
    put(?xbattle_display, List).



