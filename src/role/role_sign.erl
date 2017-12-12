-module(role_sign).

-compile(export_all).

-include("def_role.hrl").
-define(SIGNREWARD_HAS_GET,1).
-define(SIGNREWARD_NOT_GET,0).
-define(DAILY_SIGN_REWARD_TYPE,1).
-define(ACC_SIGN_REWARD_TYPE,2).

%%===============================协议处理接口=========================================%%
cs_sign_reward_info(#cs_sign_reward_info{})->
    send_sign_reward_info().

cs_sign_reward(#cs_sign_reward{rewardtype=RewardType,rewardcount=RewardCount})->
    case catch do_cs_sign_reward(RewardType,RewardCount) of
        {ok,Reward}->
            ?sendself(#sc_sign_reward{result=1,rewardtype=RewardType,sign_reward=Reward,rewardcount=RewardCount});
        {false,Reason}->
            ?sendself(#sc_sign_reward{result=Reason,rewardtype=RewardType,sign_reward=[],rewardcount=RewardCount})
    end.
%%=============================协议内部处理函数=======================================%%
do_cs_sign_reward_info()->
    CloseSec = data_sign:get(close_daily_sign),
    case util:now() >= util:datetime_to_seconds(CloseSec) of
        true -> {false, 10};
        _ ->
            SignInfo=role_data:get_signinfo(),
            % CurrentGerTypeID = gen_server:call(sign_server,get_current_gerType),
            CurrentGerTypeID = get_current_gerTypeID(),
            RewardList1= sign_server:generate_sign_rewardlist(CurrentGerTypeID,?DAILY_SIGN_REWARD_TYPE,SignInfo),
            AccRewardList1 = sign_server:generate_sign_rewardlist(CurrentGerTypeID,?ACC_SIGN_REWARD_TYPE,SignInfo),
            RewardList = add_getflag_into_signreward(RewardList1,SignInfo,?DAILY_SIGN_REWARD_TYPE),
            AccRewardList = add_getflag_into_signreward(AccRewardList1,SignInfo,?ACC_SIGN_REWARD_TYPE),
            {_Year,Month,_Day} = erlang:date(),
            Num = get_month_day_num(Month),
            {ok,lists:sublist(RewardList,erlang:min(Num,length(RewardList))),SignInfo#sign_info.sign_time_count,AccRewardList,SignInfo#sign_info.is_get_sign_reward}
    end.

do_cs_sign_reward(RewardType,RewardCount)->
    CloseSec = data_sign:get(close_daily_sign),
    case util:now() >= util:datetime_to_seconds(CloseSec) of
        true -> {false, 10};
        _ ->
            CurrentGerTypeID = get_current_gerTypeID(),
            {_Year,_Month,Day} = erlang:date(),
            case RewardType of
                ?DAILY_SIGN_REWARD_TYPE->
                    SignInfo = role_data:get_signinfo(),
                    case SignInfo#sign_info.is_get_sign_reward =:= ?SIGNREWARD_NOT_GET andalso RewardCount =:= (SignInfo#sign_info.sign_time_count+1) andalso RewardCount =< Day of
                        true->
                            NewSignInfo = update_signinfo(SignInfo,RewardType,RewardCount),
                            RoleInfo = role_data:get_roleInfo(),
                            % case gen_server:call(sign_server,{get_sign_reward,NewSignInfo,?DAILY_SIGN_REWARD_TYPE}) of
                            case sign_server:get_reward(CurrentGerTypeID,NewSignInfo,0,?DAILY_SIGN_REWARD_TYPE) of
                                []->
                                    erlang:throw({false,4});
                                RewardList ->
                                    send_sign_reward(RewardList,RoleInfo)
                            end;
                        false ->
                            erlang:throw({false,2})
                    end;
                ?ACC_SIGN_REWARD_TYPE->
                    SignInfo = role_data:get_signinfo(),
                    case role_sign:get_bit_value(SignInfo#sign_info.is_get_acc_sign_reward,RewardCount)=:=0 andalso RewardCount =< Day of
                        true->
                            NewSignInfo = update_signinfo(SignInfo,RewardType,RewardCount),
                            RoleInfo = role_data:get_roleInfo(),
                            case sign_server:get_reward(CurrentGerTypeID,NewSignInfo,RewardCount,?ACC_SIGN_REWARD_TYPE) of
                                []->
                                    erlang:throw({false,4});
                                RewardList ->
                                    send_sign_reward(RewardList,RoleInfo)
                            end;
                        false ->
                            erlang:throw({false,2})
                    end;
                _ ->
                    erlang:throw({false,3})
            end
    end.
%%====================================================================================%%
get_month_day_num(Month)->
    {Year,_Month,_Day} = erlang:date(),
    case Month=:=12 of
        true->
            erlang:trunc((util:datetime_to_seconds({{Year+1,1,1},{0,0,0}})-util:datetime_to_seconds({{Year,Month,1},{0,0,0}})) div ?ONE_DAY_SECONDS);
        false->
            case Month >12 orelse Month <0 of
                false->
                    erlang:trunc((util:datetime_to_seconds({{Year,Month+1,1},{0,0,0}})-util:datetime_to_seconds({{Year,Month,1},{0,0,0}})) div ?ONE_DAY_SECONDS);
                true->
                    ?ERR("月份出错 Month：~w ~n",[Month]),
                    31
            end
    end.

send_sign_reward(R,Role)->
    {_Day,RewardList} = R,
    role_reward:handle_sys_reward(Role, RewardList, ?MONEY_ADD_TYPE_SIGN, 0, ""),
    {ok,role_reward:transform2p_reward_view(RewardList,[])}.

update_signinfo(SignInfo,RewardType,RewardCount)->
    case RewardType of
        ?ACC_SIGN_REWARD_TYPE->
            OldAccSignRewardNum = SignInfo#sign_info.is_get_acc_sign_reward,
            NewAccSignRewardNum = add_bit_value(OldAccSignRewardNum,RewardCount),
            NewSignInfo = SignInfo#sign_info{is_get_acc_sign_reward=NewAccSignRewardNum};
        ?DAILY_SIGN_REWARD_TYPE->
            #sign_info{sign_time_count=SignTimeCount} = SignInfo,
            % {_Year,_Month,_Day} = erlang:date(),
            %%此处有可能服务器收到消息时，已经到了第二天，所以不能用天数作限制
            NewSignInfo = SignInfo#sign_info{sign_time_count=SignTimeCount+1,is_get_sign_reward=?SIGNREWARD_HAS_GET,last_sign_time=util:now()};
        _ ->
            ?ERR("undefined RewardType:~w ~n",[RewardType]),
            NewSignInfo = SignInfo
    end,
    role_data:set_signinfo(NewSignInfo),
    NewSignInfo.

send_sign_reward_info()->
    case catch do_cs_sign_reward_info() of
        {ok,RewardList,SignCount,AccRewardList,IsGetSignReward}->
            ?sendself(#sc_sign_reward_info{result=1,rewardlist=RewardList,accrewardlist=AccRewardList,signcount=SignCount,is_get_sign_reward=IsGetSignReward});
        {false,Reason}->
            ?sendself(#sc_sign_reward_info{result=Reason,rewardlist=[],signcount=0,is_get_sign_reward=0})
    end.

add_getflag_into_signreward(SignReward,SignInfo,Type)->
    case Type of
        ?ACC_SIGN_REWARD_TYPE->
            add_getflag_into_signreward3(SignReward,SignInfo#sign_info.is_get_acc_sign_reward);
        ?DAILY_SIGN_REWARD_TYPE->
            add_getflag_into_signreward2(SignReward,SignInfo#sign_info.sign_time_count);
        _ ->
            ?ERR("undefined Type:~w~n",[Type]),
            SignReward
    end.
add_getflag_into_signreward2(SignReward,SignCount)->
    List1 = lists:foldl(fun(#p_sign_reward{sign_count=NeedSignCount}=P,Acc)->
        case NeedSignCount =< SignCount of
            true->
                [P#p_sign_reward{is_get_reward=?SIGNREWARD_HAS_GET}|Acc];
            false->
                [P#p_sign_reward{is_get_reward=?SIGNREWARD_NOT_GET}|Acc]
        end
    end,[],SignReward),
    lists:reverse(List1).

add_getflag_into_signreward3(SignReward,SignCount)->
    List1 = lists:foldl(fun(#p_sign_reward{sign_count=NeedSignCount}=P,Acc)->
        case role_sign:get_bit_value(SignCount,NeedSignCount) of
            1->
                [P#p_sign_reward{is_get_reward=?SIGNREWARD_HAS_GET}|Acc];
            0->
                [P#p_sign_reward{is_get_reward=?SIGNREWARD_NOT_GET}|Acc]
        end
    end,[],SignReward),
    lists:reverse(List1).

%%新的一天开始，刷新玩家的签到信息
hook_zero_clock()->
    SignInfo = role_data:get_signinfo(),
    NewSignInfo = role_data:update_signinfo(SignInfo),
    role_data:set_signinfo(NewSignInfo),
    send_sign_reward_info().

%%=========================================================================================================%%
test_set_sign_info(RoleID,SignInfo)->
    role_lib:send_server(RoleID,{set_signinfo,SignInfo}).

get_current_gerTypeID()->
    Nowsec = util:now(),
    case Nowsec > util:datetime_to_seconds(data_sign:get(exchange_time)) of
        true->
            case data_sign:get(data_mega_list) of
                ?undefined->
                    ?ERR("undefined data_mega_list in data_sign.config~n"),
                    7130;
                List->
                    {_Year,Mon,_Day} = erlang:date(), 
                    case length(List) >= Mon of
                        true->
                            lists:nth(Mon,List);
                        false->
                            ?ERR("data_mega_list 长度不足~w ~n",[Mon]),
                            hd(List)
                    end
            end;
        false->
           gen_server:call(sign_server,get_current_gerType)
    end. 

get_bit_value(Value,Bit)->
    case Bit > 0 andalso Bit < 32 of
        true->
            BitValue = Value bsr (Bit-1),
            1 band BitValue;
        false->
            1
    end.

add_bit_value(Value,Bit)->
    case get_bit_value(Value,Bit) of
        1->
            ?ERR("Bit:~w has set true in Value:~w ~n",[Bit,Value]),
            Value;
        0->
            BitValue = erlang:trunc(math:pow(2,Bit-1)),
            Value bor BitValue
    end.
delete_bit_value(Value,Bit)->
    case get_bit_value(Value,Bit) of
        1->
            BitValue = erlang:trunc(math:pow(1,Bit-1)),
            Value band BitValue;
        0->
            Value
    end.
%%旧的Value保存的是累积签到领取到多少天了，所以最大是31
%%新的按照位保存，由于配置的最小累积天数为5最大为30,此处将签到小于31的数字转换成新的位数据，如果修改累积奖励的配置，有可能这个地方会出现问题
transform_old_acc_sign2_new_acc_sign(Value)->
    case Value > 30 of
        true->
            Value;
        false->
            case lists:keyfind(Value,1,data_sign:get(data_sign_acc_reward_list)) of
                false->
                    Value;
                _FindOne->
                    add_bit_value(0,Value)
            end
    end.