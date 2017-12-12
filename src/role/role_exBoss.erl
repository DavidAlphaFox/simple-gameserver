-module(role_exBoss).
-compile(export_all).
-include("def_role.hrl").


cs_exBoss_get_info(_)->
    Boss = role_data:get_exBoss_data(),
    {BossDtl,TimeDtl,HitDtl,NowHit,MaxHit,OneHit} = boss2p_exBoss(Boss),
    ?sendself(#sc_exBoss_get_info{boss=BossDtl,hit=HitDtl,times=TimeDtl,nowHit=NowHit,maxHit=MaxHit,oneHit=OneHit}).

cs_exBoss_hit(_) ->
    #role_exBoss{curHp=CurHp,oneHit=OneHit,hitList=HitList,nowHit=NH,maxHit=MH,haveTimes=HaveTimes}=
    Boss = role_data:get_exBoss_data(),
    case HitList of 
        [] -> ?sendself(#sc_exBoss_hit{result=2});
        _ ->
            if HaveTimes >= 1 ->
                   NH2 = if NH + 1 =< MH -> NH + 1; true -> NH end,
                   %[#p_exBoss_hit_list{harm=Harm}|HitList2]=HitList,
                   [{Hit,Harm}|HitList2]=HitList,
                   AddGold = (Hit bsr 10) band 2#11111,
                   Reward = #reward{coin=OneHit*25 + 8000,roleExp=OneHit,gerExp=0,gold=AddGold,dropList=[],reputation=0},
                   role_reward:handle_dungeon_reward_f(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_EXBOSS_HIT, NH2, ""),
                   role_data:set_exBoss_data(Boss#role_exBoss{nowHit=NH2,haveTimes=HaveTimes-1
                                                              ,curHp=erlang:max(CurHp-Harm,0),hitList=HitList2}),
                   %%Boss死亡之后触发boss挑战任务
                   case HitList2 of []->role_payGuide:trigger_task_change(?CHALLENGE_KILL_BOSS_N,{1});_->ignore end;
               true ->
                   ?sendself(#sc_exBoss_hit{result=1})
            end
    end.

cs_exBoss_buy_times(_)->
    case check_buy() of
        {true, ExBoss,Role,Cost,MaxBuyTimes}->
            do_buy(ExBoss,Role,Cost,MaxBuyTimes);
        {false,Reason,MaxBuyTimes} ->
            #role_exBoss{cdduration=CDDuration,haveTimes=HaveTimes,buyTimes=BuyTimes
                         ,lastRefreshSec=LRS,freeRefresh=FR}=role_data:get_exBoss_data(),
            TimesInfo=#p_exBoss_times_dtl{ haveTimes=HaveTimes,canBuyTimes=MaxBuyTimes-BuyTimes
                                           ,nextRefSec=LRS+CDDuration,freeRef=FR },
            ?sendself(#sc_exBoss_buy_times{result=Reason, times=TimesInfo})
    end.

cs_exBoss_get_reward(_) ->
    case check_get_reward() of
        {true,ExBoss} ->
            do_get_reward(ExBoss);
        {false,Reason} ->
            ?sendself(#sc_exBoss_get_reward{result=Reason})
    end.

cs_exBoss_refresh(_)->
    case check_refresh() of
        {true, NeedGold,Role}->
            do_refresh(NeedGold,Role);
        {true,ExBoss}->
            do_refresh(ExBoss);
        {false,Reason}->
            ?sendself(#sc_exBoss_refresh{result=Reason})
    end.

cs_exBoss_buy_cost(_)->
    BuyTimeCost = data_exBoss:get(buyTimesCost),
    RefreshCost = data_exBoss:get(refresh_needGold),
%    Sec = data_exBoss:get(cdduration),
    #role_exBoss{cdduration=Sec} = role_data:get_exBoss_data(),
    Free = data_exBoss:get(free_refresh_times),
    ?sendself(#sc_exBoss_buy_cost{buyTimesCost=BuyTimeCost,refreshCost=RefreshCost,sec=Sec,freeRefTime=Free}).

cs_exBoss_oneKey(_) ->
    #role_exBoss{curHp=CurHp,oneHit=OneHit,hitList=HitList,nowHit=NH,maxHit=MH,haveTimes=HaveTimes}=
    Boss = role_data:get_exBoss_data(),
    case HitList of 
        [] -> ?sendself(#sc_exBoss_hit{result=2});
        _ ->
            if HaveTimes >= 1 ->
                   {RG,HarmC,HitList2,HaveTimes2} = do_onekey(0,0,HitList,HaveTimes),
                   HitTimes = HaveTimes - HaveTimes2,
                   NH2 = if NH + HitTimes =< MH -> NH + HitTimes; true -> MH end,
                   Reward = #reward{coin=(OneHit*25+8000)*HitTimes, roleExp=OneHit*HitTimes,gerExp=0,gold=RG,dropList=[],reputation=0},
                   role_reward:handle_dungeon_reward_f(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_EXBOSS_HIT, NH2, "1"),
                   role_data:set_exBoss_data(Boss#role_exBoss{nowHit=NH2,haveTimes=HaveTimes2
                                                              ,curHp=max(CurHp-HarmC,0),hitList=HitList2}),
                                      %%Boss死亡之后触发boss挑战任务
                   case HitList2 of []->role_payGuide:trigger_task_change(?CHALLENGE_KILL_BOSS_N,{1});_->ignore end;
               true ->
                   ?sendself(#sc_exBoss_hit{result=1})
            end
    end.

do_onekey(RG,HarmC,HitList, HaveTimes) when HitList == [] orelse HaveTimes == 0 ->
    {RG,HarmC,HitList,HaveTimes};
do_onekey(RG,HarmC,[{Hit,Harm}|HitList],HaveTimes) ->
    AddGold = (Hit bsr 10) band 2#11111,
    do_onekey(RG+AddGold,HarmC+Harm,HitList,HaveTimes-1).


do_refresh(ExBoss)->
    NewExBoss =gen_exBoss_data(1,ExBoss#role_exBoss{bossID=0}), 
    role_data:set_exBoss_data(NewExBoss),
    {BossDtl,TimeDtl,HitDtl,NowHit,MaxHit,OneHit} = boss2p_exBoss(NewExBoss),
    Info = #sc_exBoss_get_info{boss=BossDtl,hit=HitDtl,times=TimeDtl,nowHit=NowHit,maxHit=MaxHit,oneHit=OneHit},
    ?sendself(#sc_exBoss_refresh{result=1,info=[Info]}).
do_refresh(NeedGold,Role)->
    role_lib:deduct_gold_f(Role, NeedGold,?MONEY_DEC_TYPE_EXBOSS_REFRESH,0,""),
    ExBoss = role_data:get_exBoss_data(),
    NewExBoss =gen_exBoss_data(1,ExBoss#role_exBoss{bossID=0}), 
    role_data:set_exBoss_data(NewExBoss),
    {BossDtl,TimeDtl,HitDtl,NowHit,MaxHit,OneHit} = boss2p_exBoss(NewExBoss),
    Info = #sc_exBoss_get_info{boss=BossDtl,hit=HitDtl,times=TimeDtl,nowHit=NowHit,maxHit=MaxHit,oneHit=OneHit},
    ?sendself(#sc_exBoss_refresh{result=1,info=[Info]}).

check_refresh()->
    #role_exBoss{freeRefresh=FR}=ExBoss=role_data:get_exBoss_data(),
    case FR >= data_exBoss:get(free_refresh_times) of
        true ->
            NeedGold = data_exBoss:get(refresh_needGold),
            Role = role_data:get_roleInfo(),
            case role_lib:check_money(Role,gold,NeedGold) of
                true -> {true,NeedGold,Role};
                _->{false,2}
            end;
        _ ->{true,ExBoss#role_exBoss{freeRefresh=FR+1}}
    end.

do_get_reward(#role_exBoss{bossLevel=Level,reward=Reward}=ExBoss) ->
    role_data:set_exBoss_data(ExBoss#role_exBoss{isGetReward=1}),
    role_reward:handle_sys_reward(role_data:get_roleInfo(), Reward,?MONEY_ADD_TYPE_EXBOSS_REWARD, Level,""),
    NewExBoss =gen_exBoss_data(Level+1,ExBoss), 
    role_data:set_exBoss_data(NewExBoss),
    {BossDtl,TimeDtl,HitDtl,NowHit,MaxHit,OneHit} = boss2p_exBoss(NewExBoss),
    Info = #sc_exBoss_get_info{boss=BossDtl,hit=HitDtl,times=TimeDtl,nowHit=NowHit,maxHit=MaxHit,oneHit=OneHit},
    ?sendself(#sc_exBoss_get_reward{result=1,reward=[reward_info(Reward)], info=[Info]}).

add_level_reward(Level, Reward) -> 
    LvList = data_exBoss:get(boss_lv_reward),
    AddReward = lists:foldl(fun(Lv,_RewardA)when Level > Lv -> 
                                 RewardList = data_exBoss:get({boss_lv_reward,Lv}),
                                 util:random_one_from_weigh_list(RewardList);
                            (_Lv,RewardA) -> RewardA
                         end, #sell_reward{}, LvList),
    role_xbattle:merge_reward(Reward,AddReward).

calc_reward_item(BossID,RL1,RH1,RL2,RH2) ->
    {ItemID1,ItemID2} = data_exBoss:get({drop_id,BossID}),
    Num1 = util:random_int(RL1,RH1),
    Num2 = util:random_int(RL2,RH2),
    I1 = if Num1 > 0 andalso ItemID1 > 0 ->[#new_item{itemTypeID=ItemID1,itemNum=Num1,itemLevel=1,itemRank=0}];true -> [] end,
    I2 = if Num2 > 0 andalso ItemID2 > 0 ->[#new_item{itemTypeID=ItemID2,itemNum=Num2,itemLevel=1,itemRank=0}];true -> [] end,
    I1 ++ I2.

check_get_reward()->
    #role_exBoss{nowHit=NowHit,maxHit=MaxHit,curHp=CurHp} = ExBoss = role_data:get_exBoss_data(),
    if NowHit < MaxHit andalso CurHp > 0  -> {false, 2};
       true -> {true,ExBoss}
    end.

do_buy(ExBoss,Role,Cost,MaxBuyTimes)->
    #role_exBoss{haveTimes=_HaveTimes,buyTimes=BuyTimes}=ExBoss=role_data:get_exBoss_data(),
    NewBuyTimes = BuyTimes+1,
    role_lib:deduct_gold_f(Role, Cost,?MONEY_DEC_TYPE_EXBOSS_BUYTIMES,NewBuyTimes,""),
    %NewTimes = HaveTimes + data_exBoss:get(buyAddTimes),
    NewTimes = get_max_times(),
    NewExBoss = ExBoss#role_exBoss{haveTimes=NewTimes,buyTimes=NewBuyTimes},
    role_data:set_exBoss_data(NewExBoss),
    TimesInfo=#p_exBoss_times_dtl{ haveTimes=NewExBoss#role_exBoss.haveTimes
                                   ,canBuyTimes=MaxBuyTimes-NewExBoss#role_exBoss.buyTimes
                                   ,nextRefSec=NewExBoss#role_exBoss.lastRefreshSec+NewExBoss#role_exBoss.cdduration
                                   ,freeRef=NewExBoss#role_exBoss.freeRefresh
                                 },
    ?sendself(#sc_exBoss_buy_times{result=1,times=TimesInfo}).

check_buy()->
    #role_exBoss{buyTimes=BuyTimes}=ExBoss=role_data:get_exBoss_data(),
    #role{vipLevel=VipLevel}=Role = role_data:get_roleInfo(),
    MaxBuyTimes = data_exBoss:get({vipBuyTimes, VipLevel}),
    if BuyTimes >= MaxBuyTimes ->  {false,2,MaxBuyTimes};
       true -> Cost = data_exBoss:get(buyTimesCost),
               case role_lib:check_money(Role, gold, Cost) of
                   false -> {false,3,MaxBuyTimes};
                   true -> {true,ExBoss,Role,Cost,MaxBuyTimes}
               end
    end.

reward_info(Reward)->
    activity_server:sell_reward2p_reward_info(Reward).

boss2p_exBoss(Boss) ->
    BossDtl = #p_exBoss_dtl{ bossID=Boss#role_exBoss.bossID
                             ,bossLevel = Boss#role_exBoss.bossLevel
                             ,isGetReward=Boss#role_exBoss.isGetReward
                             ,bossMaxHp=Boss#role_exBoss.maxHp
                             ,bossHp=Boss#role_exBoss.curHp
                             ,reward=reward_info(Boss#role_exBoss.reward)
                           },
    #role{level=_Level,vipLevel=Vip} = role_data:get_roleInfo(),
    %	MaxTimes = data_exBoss:get(max_times),
    MaxBuyTimes = data_exBoss:get({vipBuyTimes,Vip}),
    %CDDuration = data_exBoss:get(cdduration),
    CDDuration = Boss#role_exBoss.cdduration,
    TimeDtl = #p_exBoss_times_dtl{ haveTimes=Boss#role_exBoss.haveTimes
                                   ,canBuyTimes=MaxBuyTimes-Boss#role_exBoss.buyTimes
                                   ,nextRefSec=Boss#role_exBoss.lastRefreshSec+CDDuration
                                   ,freeRef=Boss#role_exBoss.freeRefresh
                                 },
    HitDtl = [#p_exBoss_hit_list{hit=HLH,harm=HLM}||{HLH,HLM}<-Boss#role_exBoss.hitList],
    NowHit = Boss#role_exBoss.nowHit,
    MaxHit = Boss#role_exBoss.maxHit,
    OneHit = Boss#role_exBoss.oneHit,
    {BossDtl,TimeDtl,HitDtl,NowHit,MaxHit,OneHit}.

base_exBoss()-> #role_exBoss{haveTimes=get_max_times(),lastRefreshSec=util:now()}.
gen_exBoss_data() -> gen_exBoss_data(1,base_exBoss()).
gen_exBoss_data(RoleLevel)-> gen_exBoss_data(1,base_exBoss(),RoleLevel,0,0).
gen_exBoss_data(BossLevel,LastExBoss) ->
    #role{level=Level,familyID = FamilyID} = role_data:get_roleInfo(),
	FamilyTek_Dout_Add = role_lib:calculate_familyTek_addbuff(FamilyID,7,1),
	FamilyTek_DT_Add = role_lib:calculate_familyTek_addbuff(FamilyID,3,1),
    MaxBossLevel = data_exBoss:get(max_boss_level),
    if BossLevel > MaxBossLevel -> 
           gen_exBoss_data(1,LastExBoss#role_exBoss{bossID=0},Level,FamilyTek_Dout_Add,FamilyTek_DT_Add);
       true ->
           gen_exBoss_data(BossLevel,LastExBoss,Level,FamilyTek_Dout_Add,FamilyTek_DT_Add)
    end.
gen_exBoss_data(BossLevel0,LastExBoss,Level,FamilyTek_Dout_Add,FamilyTek_DT_Add)->
    BossLevel = case data_exBoss:get({hit_times,BossLevel0}) of ?undefined -> 1;_ -> BossLevel0 end,
    Level2 = Level + 5,
    OneHit = 
        if Level < 300 ->
               trunc(Level2*Level2*Level2/21 * math:pow(2,trunc(Level*0.03)));
           true ->
               trunc(Level2*Level2*Level2* 24.381) %  math:pow(2,9) / 21
        end,
    HitRewardBase0 = 
        if Level < 300 -> trunc(2 + 9* math:pow(OneHit,0.2)*Level/184);
           true -> trunc(2 + 9 * math:pow(OneHit, 0.2)*(Level - 300)/184)
        end,
    HitRewardBase = role_lib:calculate_familyTekeffectGenerate(HitRewardBase0,FamilyTek_Dout_Add),
    BossID = case LastExBoss#role_exBoss.bossID of
                 0 ->BossIDList = data_exBoss:get(bossIDList),
                     util:random_one_from_list(BossIDList);
                 LastExBossID -> LastExBossID
             end,
    HitTimes = data_exBoss:get({hit_times,BossLevel}),
    MaxHp = OneHit * HitTimes,
    %Now = util:now(),
    {HitList,MaxTimes} = calc_hit_list(MaxHp,OneHit),
    {RewardBase=#sell_reward{item=RewardBaseItem},{RL1,RH1},{RL2,RH2},RewardDropID} = data_exBoss:get({boss_reward,BossLevel}),
    RewardItem = calc_reward_item(BossID,RL1,RH1,RL2,RH2),
    NewCDDuration = role_lib:calculate_familyTekeffectTime(data_exBoss:get(cdduration),FamilyTek_DT_Add),
    RewardDropItem = calc_reward_drop_item(RewardDropID),
    Reward = RewardBase#sell_reward{item=RewardBaseItem++RewardItem++RewardDropItem},
    Reward2 = add_level_reward(Level,Reward),
    NewBoss = LastExBoss#role_exBoss{bossID=BossID,curHp=MaxHp,maxHp=MaxHp,isGetReward=0,bossLevel=BossLevel
                                     ,oneHit=HitRewardBase,maxHit=MaxTimes,hitList=HitList,reward=Reward2,cdduration=NewCDDuration},
    NewBoss.

calc_reward_drop_item(RewardDropID) ->
    L = data_exBoss:get({boss_drop_item,RewardDropID}),
    util:random_one_from_weigh_list(L).

calc_hit_list(HitTimes,OneHit)-> calc_hit_list([],1, HitTimes,OneHit,data_exBoss:get(crit)).
calc_hit_list(List,MaxTimes,Times,_,_) when Times =< 0 orelse MaxTimes >= 1000 -> {lists:reverse(List),MaxTimes};
calc_hit_list(List,MaxTimes,Times,OneHit,Crit) ->
    {Ra,{RGMin,RGMax}} = data_exBoss:get(ra_reward_gold),
    AddGold2 = case util:random_int(0,100) < Ra of
                  true -> AddGold = if RGMax == RGMin -> RGMax; true ->util:random_int(RGMax,RGMin) end,
                         (AddGold band 2#11111) bsl 10 ;
                  _ -> 0
              end,
    {Times2, Harm} = 
    case util:random_int(0,100) >= Crit of
        true  -> CCRa = data_exBoss:get(crit_ra),
                 {MaxTimes bor AddGold2,OneHit+trunc(util:random_int(OneHit * -1, OneHit) * CCRa / 100)};
        _ ->{16#8000 bor MaxTimes bor AddGold2, OneHit * 2}
    end,
    %THit = #p_exBoss_hit_list{hit=Times2,harm=Harm},
    THit = {Times2,Harm},
    calc_hit_list([THit|List],MaxTimes + 1,Times - Harm,OneHit,Crit).

refresh_exBoss()->
    #role_exBoss{lastRefreshSec=LRFS}=role_data:get_exBoss_data(),
    NewBoss = gen_exBoss_data(),
    role_data:set_exBoss_data(NewBoss#role_exBoss{lastRefreshSec=LRFS}).

get_max_times() ->
    %MaxTimes = data_exBoss:get(max_times),
    #role{vipLevel=VipLevel} = role_data:get_roleInfo(),
    AddTimes = data_exBoss:get({vipAddTimes, VipLevel}),
    AddTimes.% + MaxTimes.

update_exBoss_times()-> update_exBoss_times(role_data:get_exBoss_data()).
update_exBoss_times(#role_exBoss{cdduration=CDDuration,lastRefreshSec=LRFS,haveTimes=HST}=Data)->
    MaxTimes = get_max_times(),
    Now = util:now(),
    %CDDuration = data_exBoss:get(cdduration),
    %NextTick = erlang:min(Now+CDDuration, LRFS+Duration),
    %timer_wheel:add_plan(NextTick, fun role_exBoss:update_exBoss_times/0),
    if HST >= MaxTimes -> 
           %% 次数是满的,则一次cd过后再检测是否需要恢复
           Data2 = Data#role_exBoss{ lastRefreshSec = Now},
           role_data:set_exBoss_data(Data2),
           timer_wheel:add_plan(Now+CDDuration, fun role_exBoss:update_exBoss_times/0),
           ignore;
       true ->
           %% 下次刷新时间比当前时间小,说明需要恢复次数,并以当前时间作为刷新纪录
           if LRFS + CDDuration =< Now ->
                  CDDuration2 = if CDDuration =< 0 -> data_exBoss:get(cdduration); true -> CDDuration end,
                  Add = trunc((Now - LRFS) / CDDuration2),
                  NewTimes = erlang:min(MaxTimes,HST+Add),
                  ?sendself(#sc_exBoss_update_times{times=NewTimes}),
                  Data2 = Data#role_exBoss{haveTimes = NewTimes, lastRefreshSec = Now},
                  timer_wheel:add_plan(Now+CDDuration, fun role_exBoss:update_exBoss_times/0),
                  role_data:set_exBoss_data(Data2);
              true -> 
                  %% 下次刷新时间比当前时间大,说明还没到刷新时间,以上次刷新时间作为刷新纪录
                  timer_wheel:add_plan(LRFS+CDDuration, fun role_exBoss:update_exBoss_times/0),
                  ignore
           end
    end.

