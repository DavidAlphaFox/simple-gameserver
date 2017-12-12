-module(role_dSign).
-compile(export_all).
-include("def_role.hrl").

% lmdate 表示最后一次签到日期，如果日期是今日，但是lmreward是[]，则表示是本月第一次签到

cs_dSign_info(_) ->
    DSignData = role_data:get_dSignData(),
    BirthDate = DSignData#daySign.bsDay,
    Unit = trans_unit_data(DSignData#daySign.mData),
    SignTotal = DSignData#daySign.tto,
    SevenConDays =DSignData#daySign.s7c,
%    ReSignCost = data_dSign:get(reSignCost),
    LmSec = DSignData#daySign.lmSec,
    {LMDate,_} = util:seconds_to_datetime(LmSec),
    {{Y,M,_}=TDate,_} = erlang:localtime(),
    LmReward=DSignData#daySign.lmReward,
    IsSignMon0 = case LMDate of
                     TDate -> case LmReward of [] -> 0; _ -> 1 end;
                     {Y,M,_} -> 0;  %本月非今天，可以签到 
                     _ -> case LmReward of [] -> 0; _ -> 1 end
                 end,
    {LmSec2,LmReward2} = case LmSec of 0 -> 
                                           T = lists:nth(M, data_dSign:get(data_flash_list_new)),
                                           {util:now(),[{2,0},{6,T,0}]};
                             _ -> 
                                 LmRewardT = case length(LmReward) of 
                                                 0 -> T = lists:nth(M, data_dSign:get(data_flash_list_new)),
                                                      [{2,0},{6,T,0}];
                                                 1 -> T = lists:nth(M, data_dSign:get(data_flash_list_new)),
                                                      [{6,T,0}|LmReward];
                                                 _ -> LmReward
                                             end,
                                 {LmSec,LmRewardT} 
                         end,
    ?sendself(#sc_dSign_info{birthDate=BirthDate,sevenConDays=SevenConDays,signTotal=SignTotal
                            ,unit=Unit,lastMonSignDate=LmSec2,isMonSignToday = IsSignMon0
                            ,monReward = role_reward:transform2p_reward_view(LmReward2,[])
                            }).

cs_dSign_sign(_)->
    case check_dSign_sign() of
        {true,TU,OU,M,D,DSignData} ->
            do_dSign_sign(TU,OU,M,D,DSignData);
        {false,Reason} ->
            ?sendself(#sc_dSign_sign{result=Reason,reward=[]})
    end.

cs_dSign_mark_mon(_)->
    case check_dSign_mark_mon() of
        {true,TU,OU,DSignData,M} ->
            do_dSign_mark_mon(TU,OU,DSignData,M);
        {false,Reason} ->
            ?sendself(#sc_dSign_mark_mon{result=Reason,reward=[]})
    end.

cs_dSign_reSign(#cs_dSign_reSign{monStartDate=Mon,day=Day}) ->
    case check_dSign_reSign(Mon,Day) of
        {true,TU,OU,M,D,DSignData,Reward,Cost,RoleInfo} ->
            do_dSign_reSign(TU,OU,M,D,DSignData,Reward,Cost,RoleInfo);
        {false,Reason} ->
            ?sendself(#sc_dSign_reSign{result=Reason,reward=[]})
    end.

cs_dSign_get_sevenReward(_) ->
    case check_dSign_get_sevenReward() of
        {true,S7c,DSignData} -> do_dSign_get_sevenReward(S7c,DSignData);
        {false,Reason} ->
            ?sendself(#sc_dSign_get_sevenReward{result=Reason,reward=[]})
    end.

cs_dSign_get_monReward(#cs_dSign_get_monReward{}) ->
    case check_get_monReward() of
        {true,TU,OU,DSignData,Reward} -> 
            do_get_monReward(TU,OU,DSignData,Reward);
        {false,Reason} ->
            ?sendself(#sc_dSign_get_monReward{result=Reason,reward=[]})
    end.



do_get_monReward(TU,OU,DSignData,Reward)->
    TU2= TU#sm{rwd=1},
    DSignData2 = DSignData#daySign{mData=[TU2|OU],lmReward=[],lmSec=util:now()},
    role_data:set_dSignData(DSignData2),
    role_reward:handle_sys_reward(role_data:get_roleInfo(),Reward,?MONEY_ADD_TYPE_DSIGN_MONREWARD,TU#sm.rwd,""),
    ?sendself(#sc_dSign_get_monReward{result=1,reward=role_reward:transform2p_reward_view(Reward,[])}),
    cs_dSign_info(1).

check_get_monReward() ->
    #daySign{mData=Unit,lmSec=LmSec,lmReward=LmReward}=DSignData=role_data:get_dSignData(),
    {{Y,M,_},_} = util:seconds_to_datetime(LmSec),
    MonDate = util:datetime_to_seconds({{Y,M,1},{0,0,1}}),
    case lists:keytake(MonDate, #sm.mon, Unit) of
        false -> {false,3};
        {value,#sm{rwd=Rwd}=TU,OU} ->
            case Rwd of
                0 -> 
                    {{_,M,_},_} = util:seconds_to_datetime(MonDate),
%                    case cacl_monReward(TTo,M) of
                     case LmReward of
                        [] -> {false,4};
                        Reward -> {true,TU,OU,DSignData,Reward}
                    end;
                _ -> {false,2}
            end
    end.

cacl_monReward(TTo,M) ->
    List = data_dSign:get(data_sign_acc_reward_list_new),
    RList = lists:foldl(fun({N,RL},Acc) ->
                                if N =< TTo -> merge(RL,Acc);
                                   true -> Acc
                                end
                        end, [], List),
    replace_item_reward(RList,M).

merge([],L) -> L;
merge([{A,B}|T],L) ->
    case lists:keytake(A, 1, L) of
        false -> merge(T,[{A,B}|L]);
        {value,{_,C},OL} -> merge(T,[{A,B+C}|OL])
    end;
merge([H|T],L) -> merge(T,[H|L]).

replace_item_reward(L,M) when M > 12 -> [{A,B}||{A,B}<-L,A < 5];
replace_item_reward(List,M) ->
    lists:foldl(fun({105,N},Acc)-> 
                        T = lists:nth(M, data_dSign:get(data_mega_list_new)),
                        [{6,T,N}|Acc];
                   ({106,N},Acc) ->
                        T = lists:nth(M, data_dSign:get(data_stone_list_new)),
                        [{6,T,N}|Acc];
                   ({107,N},Acc) ->
                        T = lists:nth(M, data_dSign:get(data_flash_list_new)),
                        [{6,T,N}|Acc];
                   (X,Acc) -> [X|Acc]
                end, [], List).

do_dSign_get_sevenReward(S7c,DSignData) ->
    S7c2 = S7c - 7,
    role_data:set_dSignData(DSignData#daySign{s7c=S7c2}),
    Reward=data_dSign:get(data_sign_acc_reward_seven),
    role_reward:handle_sys_reward(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_DSIGN_S7REWARD, S7c, ""),
    %role_reward:handle_sell_reward(role_data:get_roleID(),Reward,?MONEY_ADD_TYPE_DSIGN_S7REWARD,S7c,""),
    ?sendself(#sc_dSign_get_sevenReward{result=1,reward=role_reward:transform2p_reward_view(Reward,[])}),
    cs_dSign_info(1).

check_dSign_get_sevenReward()->
    #daySign{s7c=S7c}=DSignData=role_data:get_dSignData(),
    if S7c >= 7 ->{ true,S7c,DSignData};
       true -> {false,2}
    end.
 
do_dSign_reSign(TU,OU,M,D,DSignData,RewardList1,Cost,RoleInfo) ->
    #sm{data=Data,tto=_TToM}=TU,
    Data2 = set_date_sign(D,Data),
    MUnit = TU#sm{data=Data2},
    Units = [MUnit|OU],
    #daySign{s7c=S7c,tto=TToD}=DSignData,
    DSignData2 = DSignData#daySign{mData=Units,s7c=S7c+1,tto=TToD+1},
    role_data:set_dSignData(DSignData2),
    RoleInfo2 = role_lib:deduct_gold_f(RoleInfo, Cost, ?MONEY_DEC_TYPE_DSIGN_RDAYLYREWARD, M*100+D, ""),
    Reward = replace_item_reward(RewardList1,M),
    role_reward:handle_sys_reward(RoleInfo2, Reward, ?MONEY_ADD_TYPE_DSIGN_RDAYLYREWARD, M*100+D, ""),
    ?sendself(#sc_dSign_reSign{result=1,reward=role_reward:transform2p_reward_view(Reward,[])}),
    cs_dSign_info(1).
 
check_dSign_reSign(MonDate,D) ->
    #daySign{mData=Unit}=DSignData=role_data:get_dSignData(),
    case lists:keytake(MonDate, #sm.mon, Unit) of
        false -> {false,3};
        {value,#sm{data=Data}=TU,OU} ->
            {{Y,M,_},_} = util:seconds_to_datetime(MonDate),
            case is_date_not_sign(Y,M,D,Data) of
                true -> 
                    RoleInfo = role_data:get_roleInfo(),
                    {_,Reward,Cost} = lists:keyfind(D, 1, data_dSign:get(data_sign_reward_list_new)),
                    case role_lib:check_money(RoleInfo,gold,Cost) of
                        true ->{true,TU,OU,M,D,DSignData,Reward,Cost,RoleInfo};
                        false -> {false,4}
                    end;
                false -> {false,2}
            end
    end.

do_dSign_mark_mon(TU,OU,DSignData,M) ->
    #sm{tto=TToM}=TU,
    Units = [TU#sm{tto=TToM+1}|OU],
    TToM2 = TToM + 1,
    LmReward = cacl_monReward(TToM2,M),
    DSignData2 = DSignData#daySign{mData=Units,lmSec=util:now(),lmReward=LmReward},
    role_data:set_dSignData(DSignData2),
    RewardNow = replace_item_reward(cacl_monReward_now(TToM2),M),
    role_reward:handle_sys_reward(role_data:get_roleInfo(), RewardNow, ?MONEY_ADD_TYPE_DSIGN_MONREWARDNOW, TToM2, ""),
    ?sendself(#sc_dSign_mark_mon{result=1,reward=role_reward:transform2p_reward_view(RewardNow,[])}),
    cs_dSign_info(1).

cacl_monReward_now(Days) ->
    RList = data_dSign:get(data_sign_acc_dailyreward_list_new),
    case lists:keyfind(Days, 1, RList) of
        {_,R} -> RL = data_dSign:get({data_sign_day_box,R}),
                 [util:random_one_from_weigh_list(RL)];
        _ -> []
    end.

check_dSign_mark_mon()->
    {MonDate,Y,M,D} = get_mon_date(),
    #daySign{mData=Unit,lmSec=LmSec,lmReward=LmReward}=DSignData=role_data:get_dSignData(),
    {LMDate,_} = util:seconds_to_datetime(LmSec),
    case lists:keytake(MonDate, #sm.mon, Unit) of
        false -> {false,5};
        {value,TU=#sm{tto=TTo},OU} ->
            case TTo >= data_dSign:get(max_mon_sign_days) of
                false ->
                    %% 今天已签到且当前无奖励，则表示今天实际没签，可以签到
                    %% 本月非今天已签到，则表示今天可以签到
                    %% 非本月已签到，则表示需先领取上次的奖励才能签到
                    %% 非本月已签到，且奖励为空，则表示今天可以签到
                    case LMDate of
                        {Y,M,D} -> 
                            case LmReward of
                                [] ->{true,TU,OU,DSignData,M};
                                _ ->           {false,2}
                            end;
                        {Y,M,_} -> {true,TU,OU,DSignData,M};
                        _ -> 
                            case LmReward of [] -> {true,TU,OU,DSignData,M};
                                _ -> {false,3}
                            end
                    end;
                true ->
                    {false,4}
            end
    end.

do_dSign_sign(TU,OU,M,D,DSignData) ->
    #sm{data=Data,tto=_TToM}=TU,
    Data2 = set_date_sign(D,Data),
    MUnit=TU#sm{data=Data2},
    Units = [MUnit|OU],
    #daySign{s7c=S7c,tto=TToD}=DSignData,
    DSignData2 = DSignData#daySign{mData=Units,s7c=S7c+1,tto=TToD+1},
    role_data:set_dSignData(DSignData2),
    {_,RewardList1,_} = lists:keyfind(D, 1, data_dSign:get(data_sign_reward_list_new)),
    Reward = replace_item_reward(RewardList1,M),
    role_reward:handle_sys_reward(role_data:get_roleInfo(), Reward, ?MONEY_ADD_TYPE_DSIGN_DAYLYREWARD, M*100+D, ""),
    %role_reward:handle_sell_reward(RoleID,Reward,?MONEY_ADD_TYPE_DSIGN_SIGN,M*100+D,""),
    ?sendself(#sc_dSign_sign{result=1,reward=role_reward:transform2p_reward_view(Reward,[])}),
    cs_dSign_info(1).

check_dSign_sign()->
    {MonDate,Y,M,D} = get_mon_date(),
    #daySign{mData=Unit}=DSignData=role_data:get_dSignData(),
    case lists:keytake(MonDate, #sm.mon, Unit) of
        false -> 
            case D of 1 -> {true,#sm{},Unit,D,DSignData};
                _ -> {false,3}
            end;
        {value,#sm{data=Data}=TU,OU} ->
            case is_date_not_sign(Y,M,D,Data) of
                true -> {true,TU,OU,M,D,DSignData};
                false -> {false,2}
            end
    end.

is_date_not_sign(Y,M,D,Data) ->
    LD = calendar:last_day_of_the_month(Y, M),
    if D > 0  andalso D =< LD ->
           ((1 bsl (D -1) ) band Data) == 0;
       true -> false
    end.

set_date_sign(D,Data) ->
    (1 bsl (D-1)) bor Data.

trans_unit_data(L) ->
    [#p_dSign_unit{monStartDate=Mon,signData=Data,totalSign=TTo,getReward=Rwd}
    ||#sm{mon=Mon,data=Data,tto=TTo,rwd=Rwd}<-L].

get_mon_date()->
    Now = util:now(),
    {{Y,M,D},_} = util:seconds_to_datetime(Now),
    {util:datetime_to_seconds({{Y,M,1},{0,0,1}}),Y,M,D}.


generate_dSign_unit([]) ->
    {G,_,_,_} = get_mon_date(),
    [#sm{data=0,mon=G,tto=0,rwd=0}];
generate_dSign_unit(L) ->
    #sm{mon=M} = hd(lists:reverse(lists:keysort(#sm.mon, L))),
    %MonthSec = 2764800,
    {G,_ ,_,_} = get_mon_date(),
    case G of
        M -> L;
        _ -> AddList = generate_dSign_add_list(M,G,[]),
             AddList ++ L
    end.

generate_dSign_add_list(M,G,L) when M > G -> L;
generate_dSign_add_list(M,G,L) ->
    M2 = 2764800 + M,
    {{YC,MC,_},_} = util:seconds_to_datetime(M2),
    G2 = util:datetime_to_seconds({{YC,MC,1},{0,0,1}}),
    generate_dSign_add_list(M2,G,[#sm{data=0,mon=G2,tto=0,rwd=0}|L]).

init_dSignData(DData=#daySign{mData=Data}) ->
    Data2 = generate_dSign_unit(Data),
    DData#daySign{mData=Data2}.

    