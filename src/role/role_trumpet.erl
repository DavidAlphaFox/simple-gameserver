-module(role_trumpet).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").

%% API functions
-export([]).

%% Internal functions
-export([]).

-define(LASTTALKTIME, last_talk_time).
%% ====================================================================
%% API functions
%% ====================================================================
cs_trumpet_message(#cs_trumpet_message{type=Type, serverID=ServerID, message=Message, bonusP=BonusP}) ->
    case check_talk(Message, BonusP, Type) of
        {talk_local, Role, CostInfo, Now} ->
            do_talk_local(Now, ServerID, Message, BonusP, Role, CostInfo);
        {talk_cross, Role, CostInfo, Now} ->
            do_talk_cross(Now, ServerID, Message, BonusP, Role, CostInfo);
        {false, Reason}->
            ?sendself(#sc_trumpet_message{result=Reason, type=Type, bonus_id=0})
    end.

cs_trumpet_get_bonus(#cs_trumpet_get_bonus{bonus_type=BonusType, bonus_id=BonusID}) ->
    ServerID = data_setting:get(server_id),
    #role{roleID=RoleID, roleName=RoleName} = role_data:get_roleInfo(),
    case BonusID =:= 0 of
        true ->
            %% 红包不存在
            trumpet_server:get_bonus_failed(RoleID, BonusType, BonusID, 5, {p_bonus_info, 0, 0, 0, 0, 0, []});
        _ ->
            ignore
    end,
    case BonusType of
        ?CROSS_TRUMPET ->
            MasterNmae = family_fight_server:get_master_server_name(),
            send_msg:direct_by_name(MasterNmae, cross_talk_server, {get_bonus, ServerID, RoleID, RoleName, BonusID});
        _ ->
            erlang:send(trumpet_server, {get_bonus, ServerID, RoleID, RoleName, BonusID}) 
    end.

cs_trumpet_redpacket_status(#cs_trumpet_redpacket_status{})->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    erlang:send(redpacket_server, {cs_trumpet_redpacket_status
                                  ,RoleID,item_lib:get_material_num(?ITEM_REDPACKET_ID)}).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_msg_length(Msg)->
	Length = get_utf8_length(erlang:list_to_binary(Msg),0),
	LengthLimit = data_trumpet:get(trumpet_words_limit),
	if Length < 1 orelse Length > LengthLimit ->
		   false;
	   true ->
		   true
	end.

check_talk(Message, BonusP, ?LOCAL_TRUMPET) ->
    case check_msg_length(Message) of
        true ->
            Now=util:now(),
            case Now - get_lasttalktime(?LOCAL_TRUMPET) > data_trumpet:get(trumpet_interval_limit) of
                true ->
                    #role{level=RoleLevel}= Role = role_data:get_roleInfo(),
                    case RoleLevel >= data_trumpet:get(trumpet_need_level) of
                        true ->
                            {ItemType, Num} = data_trumpet:get({trumpet_cost_item, ?LOCAL_TRUMPET, BonusP}),
                            case item_lib:check_material(ItemType, Num) of
                                false ->
                                    {false, 2};
                                {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
                                    case BonusP of
                                        false ->
                                            {talk_local, Role, {BagOther2, DelAcc, UpdateAcc, UpdateLogList}, Now};
                                        _ ->
                                            case check_bonus_cost(Role, ?LOCAL_TRUMPET) of
                                                false ->
                                                    {false, 6};
                                                _ ->
                                                    {talk_local, Role, {BagOther2, DelAcc, UpdateAcc, UpdateLogList}, Now} 
                                            end
                                    end 
                            end;
                        _ ->
                            {false, 5}
                    end;
                _ ->
                    {false,3}
            end;
        _ ->
            {false,4}
    end;

check_talk(Message, BonusP, ?CROSS_TRUMPET) ->
    case check_msg_length(Message) of
        true ->
            Now=util:now(),
            case Now - get_lasttalktime(?CROSS_TRUMPET) > data_trumpet:get(trumpet_interval_limit) of
                true ->
                    #role{level=RoleLevel}= Role = role_data:get_roleInfo(),
                    case RoleLevel >= data_trumpet:get(trumpet_need_level) of
                        true ->
                            {ItemType, Num} = data_trumpet:get({trumpet_cost_item, ?CROSS_TRUMPET, BonusP}),
                            case item_lib:check_material(ItemType, Num) of
                                false ->
                                    {false, 2};
                                {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
                                    case BonusP of
                                        false ->
                                            {talk_cross, Role, {BagOther2, DelAcc, UpdateAcc, UpdateLogList}, Now};
                                        _ ->
                                            case check_bonus_cost(Role, ?CROSS_TRUMPET) of
                                                false ->
                                                    {false, 6};
                                                _ ->
                                                    {talk_cross, Role, {BagOther2, DelAcc, UpdateAcc, UpdateLogList}, Now} 
                                            end
                                    end
                            end;
                        _ ->
                            {false, 5}
                    end;
                _ ->
                    {false,3}
            end;
        _ ->
            {false,4}
    end.

do_talk_local(Now, ServerID, Message, BonusP, Role, CostInfo)->
    Message2 = util:words_filter(Message),
    Data = pack_message(Message2, ?LOCAL_TRUMPET, ServerID, Now, Role),
    set_lasttalktime(?LOCAL_TRUMPET, Now),
    deduct_talk_cost_item(?LOCAL_TRUMPET, CostInfo), 
    erlang:send(trumpet_server, {local_trumpet, BonusP, Data}).

do_talk_cross(Now, ServerID, Message, BonusP, #role{roleID=RoleID}=Role, CostInfo) ->
    Message2 = util:words_filter(Message),
    MasterNmae = family_fight_server:get_master_server_name(),
    Data = pack_message(Message2, ?CROSS_TRUMPET, ServerID, Now, Role),
    set_lasttalktime(?CROSS_TRUMPET, Now),
    deduct_talk_cost_item(?CROSS_TRUMPET, CostInfo), 
    send_msg:direct_by_name(MasterNmae, cross_talk_server, {talk_message, {ServerID, RoleID, BonusP, Data}}).

get_lasttalktime(Type)->
    case erlang:get({Type, ?LASTTALKTIME}) of
        ?undefined ->
            0;
        X ->
            X
    end.

set_lasttalktime(Type, Time) ->
    erlang:put({Type, ?LASTTALKTIME}, Time).

pack_message(Message, Type, ServerID, Time, #role{title=Title,roleName=Name,level=Level,roleID=RoleID,head=Head,isMale=IsMale}) ->
    #sc_trumpet_message_info{type=Type,serverID=ServerID,roleName=Name,level=Level,roleTitle=Title,message=Message,
                                timeStamp=Time,roleID=RoleID,head=Head,is_male=IsMale, bonus_id=0}. 

deduct_talk_cost_item(Type, {BagOther, DelAcc, UpdateAcc, UpdateLogList}) ->
    role_data:set_bagItem(BagOther),
    LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
    {Date,_} = Time = erlang:localtime(),
    behavior_item_consume:log(role_data:get_roleID(), LogItemList, Date, Time, ?MONEY_DEC_TYPE_TALK_COST, Type, ""),
    if 
        UpdateAcc =/= [] ->
            UpdateInfoList = 
            lists:map(fun(Update) ->
                        #p_item_num_update{itemNum=Update#item.itemNum, itemUID=Update#item.itemUID}
                end, UpdateAcc),
            ?sendself(#sc_item_update{updateList=UpdateInfoList});
        true ->
            ignore
    end,
    DelItemIDList = [E||#item{itemUID=E} <- DelAcc],
    ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}).

%% 发红包金额判断(现在红包只扣道具,不扣钱)
check_bonus_cost(_Role, _BonusType) ->
    true.
%    {Type, Amount} = data_trumpet:get({bonus_amount, BonusType}),
%    case role_lib:check_money(Role, Type, Amount) of
%        false ->
%            false;
%        _ ->
%            role_lib:deduct_money_f(Role, Type, Amount, ?MONEY_DEC_TYPE_BONUS_COST, BonusType, erlang:atom_to_list(Type))
%    end.

%% 生成新的红包
gen_new_bonus(Type) ->
    {_, Amount} = data_trumpet:get({bonus_amount, Type}),
    Number = data_trumpet:get({bonus_number, Type}),
    NewBonus = #p_bonus_info{amount=Amount, left=Amount, number=Number, divided=0, timestamp=util:now(), list=[]},
    BonusID = tk_id:gen_bonusUID(),
    db_sql:set_bonusInfo(BonusID, NewBonus),
    BonusID.

%% 领取红包
get_bonus(ServerID, RoleID, RoleName, BonusInfo, Type) ->
    case check_get_bonus(RoleID, BonusInfo) of
        {false, Reason} ->
            {false, Reason};
        {true, Left, Number, Divided, List} ->
            %% 最后一份将会获得剩下的所有钱
            case Number - Divided =< 1 of
                true ->
                    RandomAmount2 = Left,
                    Left2 = 0;
                _ ->
                    %% 如果不是,则在随机范围内随机一个金额
                    {Min, Max} = data_trumpet:get({bonus_range, Type}),
                    RandomAmount = util:random_int(Min, Max),
                    RandomAmount2 = erlang:min(RandomAmount, Left),
                    Left2 = Left - RandomAmount2 
            end,
            Divided2 = Divided + 1, 
            List2 = [#p_get_info{serverID=ServerID, roleID=RoleID, roleName=RoleName, amount=RandomAmount2}|List],
            {true, RandomAmount2, BonusInfo#p_bonus_info{left=Left2, divided=Divided2, list=List2}}
    end.

%% 判断是否能够领取红包
check_get_bonus(RoleID, BonusInfo) ->
    ValidTime = data_trumpet:get(bonus_valid_time),
    #p_bonus_info{left=Left, number=Number, divided=Divided, timestamp=TimeStamp, list=List} = BonusInfo,
    %% 应客户端需求,先判断是否已经领取了
    case lists:keyfind(RoleID, #p_get_info.roleID, List) of
        #p_get_info{} ->
            %% 已经领取过了
            {false, 4};
        _ ->
            case Left =< 0 of
                true ->
                    %% 已经领完了
                    {false, 1};
                _ ->
                    case Divided >= Number of
                        true ->
                            %% 已经领完了
                            {false, 2};
                        _ ->
                            case TimeStamp + ValidTime < util:now() of
                                true ->
                                    %% 红包过期
                                    {false, 3};
                                _ ->
                                    {true, Left, Number, Divided, List}
                            end
                    end
            end
    end.

%% 红包领取成功的处理
get_bonus_success(BonusType, BonusID, Amount, BonusInfo) ->
    #role{roleID=RoleID} = Role = role_data:get_roleInfo(),
    {Type, _} = data_trumpet:get({bonus_amount, BonusType}),
    NumberType = 
        case Type of
            gold ->
                ?REWARD_GOLD;
            coin ->
                ?REWARD_COIN;
            reputation ->
                ?REWARD_REPU
        end,
    role_reward:handle_sys_reward(Role, [{NumberType, Amount}], ?MONEY_ADD_TYPE_GET_BONUS, BonusType, ""),
    ?unicast(RoleID, #sc_trumpet_get_bonus{bonus_type=BonusType, bonus_id=BonusID, amount=Amount, info=BonusInfo}). 

%% 数据升级
%% 服务器不会时常重启,因此每次启动时,遍历一次列表影响不大
update_trumpet_message_info(List) ->
    update_trumpet_message_info(List, erlang:tuple_size(#sc_trumpet_message_info{}), []).

update_trumpet_message_info([], _, Acc) ->
    lists:reverse(Acc);

update_trumpet_message_info([H|T], Size, Acc) ->
    case erlang:tuple_size(H) < Size of
        false ->
            [H|Acc];
        _ ->
            InfoList = erlang:tuple_to_list(H),
            %% 红包ID补为0
            NewInfoList = InfoList ++ [0],
            update_trumpet_message_info(T, Size, [erlang:list_to_tuple(NewInfoList)|Acc])
    end.

get_utf8_length(<<>>, N) ->
	N;
get_utf8_length(<<0:1,_X1:1,_X2:1,_X3:1,_X4:1,_X5:1,_X6:1,_X7:1,
					   Left/binary>>,N) ->
	get_utf8_length(Left, N+1);
get_utf8_length(<<1:1,1:1,0:1,_X1:1,_X2:1,_X3:1,_X4:1,_X5:1,
					   1:1,0:1,_X6:1,_X7:1,_X8:1,_X9:1,_X10:1,_X11:1,
					   Left/binary>>, N) ->
	get_utf8_length(Left, N+1);
get_utf8_length(<<1:1,1:1,1:1,0:1,_X1:1,_X2:1,_X3:1,_X4:1,
					   1:1,0:1,_X5:1,_X6:1,_X7:1,_X8:1,_X9:1,_X10:1,
					   1:1,0:1,_X11:1,_X12:1,_X13:1,_X14:1,_X15:1,_X16:1,
					   Left/binary>>, N) ->
	get_utf8_length(Left, N+1);
get_utf8_length(<<1:1,1:1,1:1,1:1,0:1,_X1:1,_X2:1,_X3:1, 
					   1:1,0:1,_X4:1,_X5:1,_X6:1,_X7:1,_X8:1,_X9:1, 
					   1:1,0:1,_X10:1,_X11:1,_X12:1,_X13:1,_X14:1,_X15:1, 
					   1:1,0:1,_X16:1,_X17:1,_X18:1,_X19:1,_X20:1,_X21:1, 
					   Left/binary>>, N) ->
	get_utf8_length(Left, N+1);
get_utf8_length(<<1:1,1:1,1:1,1:1,1:1,0:1,_X1:1,_X2:1, 
					   1:1,0:1,_X3:1,_X4:1,_X5:1,_X6:1,_X7:1,_X8:1, 
					   1:1,0:1,_X9:1,_X10:1,_X11:1,_X12:1,_X13:1,_X14:1, 
					   1:1,0:1,_X15:1,_X16:1,_X17:1,_X18:1,_X19:1,_X20:1, 
					   1:1,0:1,_X21:1,_X22:1,_X23:1,_X24:1,_X25:1,_X26:1,
					   Left/binary>>, N) ->
	get_utf8_length(Left, N+1);
get_utf8_length(<<1:1,1:1,1:1,1:1,1:1,1:1,0:1,_X1:1, 
					   1:1,0:1,_X2:1,_X3:1,_X4:1,_X5:1,_X6:1,_X7:1, 
					   1:1,0:1,_X8:1,_X9:1,_X10:1,_X11:1,_X12:1,_X13:1, 
					   1:1,0:1,_X14:1,_X15:1,_X16:1,_X17:1,_X18:1,_X19:1, 
					   1:1,0:1,_X20:1,_X21:1,_X22:1,_X23:1,_X24:1,_X25:1, 
					   1:1,0:1,_X26:1,_X27:1,_X28:1,_X29:1,_X30:1,_X31:1,
					   Left/binary>>, N) ->
	get_utf8_length(Left,N+1);
get_utf8_length(Binary, N) ->
	?ERR("非法的utf8编码，Binary：~w, AccDecodeList：~w", [Binary, N]),
	0.

