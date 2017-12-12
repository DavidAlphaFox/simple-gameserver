% 消息发送
-module(conquerisland_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-compile(export_all).
-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-include("def_homestead.hrl").  
-define(DUMP_INTERVAL, (1000 * 60 * 10)).
-define(SIGN_FORECAST_STATE,0). %% 预报名状态，等待匹配服返回报名成功状态,增加这个状态防止匹配服未返回报名成功消息前，玩家重复报名
-define(SIGN_SUCCESS_STATE,1). %% 报名成功
-record(state, {clear_timestamp=0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start( ) ->
    case data_setting:get(server_type) of
        normal->
            {ok,_} = 
            supervisor:start_child(conquerisland_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]});
        _->
            ignore
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit,true),
    %%创建ETS表
    ets:new(?ETS_CONQUERISLAND_SIGN_INFO,[{keypos,1},set, public, named_table]),
    ets:new(?ETS_CONQUERISLAND_WAR_INFO,[{keypos,1},set, public, named_table]),
    ets:new(?ETS_CONQUERISLAND_ROLETIMES_INFO,[{keypos,1},set, public, named_table]),
    NextClearTimeStamp = get_clear_timestamp(),
    NewState = 
        case db_sql:get_etc(?DB_ETC_KEY_CONQUERISLAND) of
            [{state, State},{WarInfo,SignInfo,RoleTimes}] when is_record(State,state) ->
				lists:foreach(fun({K,V})-> ets:insert(?ETS_CONQUERISLAND_WAR_INFO,{K,V}) end, WarInfo),
				lists:foreach(fun({K,V})-> ets:insert(?ETS_CONQUERISLAND_SIGN_INFO,{K,V}) end, SignInfo),
                init_ets_conquerisland_roletimes(RoleTimes,State),
                State#state{clear_timestamp=NextClearTimeStamp};
            _ ->
                #state{clear_timestamp=NextClearTimeStamp}
        end,
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    %% 每天晚上12点统一刷新卡洛斯次数
	timer_wheel:init(),
    NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
    timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
    timer_wheel:add_plan(NextClearTimeStamp,fun clear_role_times/0),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
init_ets_conquerisland_roletimes(RoleTimes,#state{clear_timestamp=ClearTimeStamp})->
    NowSec = util:now(),
    IsMonthClear=data_conquerisland:get(is_clear_buy_time),
    case NowSec >= ClearTimeStamp andalso IsMonthClear of
        true->
            ignore;
        false->
            lists:foreach(fun({K,V})->
                case  V#times.has_buy =< 0 of
                    true->
                        case NowSec >= V#times.valid_timestamp of
                            true->
                                ignore;
                            false->
                                ets:insert(?ETS_CONQUERISLAND_ROLETIMES_INFO,{K,V})
                        end;
                    false->
                        case NowSec >= V#times.valid_timestamp of
                            true->
                                FreeTimeLMT = data_conquerisland:get(times_limit),
                                {BuyTimeLMT, _} = data_conquerisland:get(buy_limit),
                                ets:insert(?ETS_CONQUERISLAND_ROLETIMES_INFO, {K, V#times{buy_left=BuyTimeLMT, free_left=FreeTimeLMT,valid_timestamp=get_roletime_validtime()}});
                            false->
                                ets:insert(?ETS_CONQUERISLAND_ROLETIMES_INFO,{K,V})
                        end
                end 
            end,RoleTimes)
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
    
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State} 
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    do_persist(State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_info({sign, RoleID, TeamID, TeamMember, Level}, State) ->
    ID = 
        case TeamID of
            0 ->
                ?make_id(-RoleID, 1);
            -1 ->
                ?make_id(-RoleID, 1);
            _ ->
                ?make_id(TeamID, erlang:length(TeamMember))
        end,
    case ets:lookup(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) of
        [{_,{_IDList,?SIGN_SUCCESS_STATE}}] ->
            ignore;
        _ ->
            ServerID = data_setting:get(server_id),
            {Info, IDList} = 
            if
                TeamID =:= 0 ->
                    %% 必须保持顺序，玩家RID是List第一个
                    {lists:foldr(fun(#p_team_member_info{roleID=MRoleID}, InfoAcc) ->
                                #rolePublic{head=Head,isMale=IsMale,title=Title,level=Lv,fightPower=FightPower} = role_lib:get_rolePublic(MRoleID),
                                [#member_base_info{serverID=ServerID, roleID=MRoleID, head=Head, isMale=IsMale, title=Title, level=Lv, fight_power=FightPower}|InfoAcc]
                            end, [], TeamMember),[RoleID]};
                ID < 0 ->
                    #rolePublic{head=Head,isMale=IsMale,title=Title,level=Levle,fightPower=FightPower} = role_lib:get_rolePublic(RoleID),
                    {[#member_base_info{serverID=ServerID, roleID=RoleID, head=Head, isMale=IsMale, title=Title, level=Levle, fight_power=FightPower}], [RoleID]};
                true ->
                    lists:foldl(fun(#p_team_member_info{roleID=MRoleID, head=Head, isMale=IsMale, title=Title, level=Levle, fightPower=FightPower}, {InfoAcc, IDAcc}) ->
                        {[#member_base_info{serverID=ServerID, roleID=MRoleID, head=Head, isMale=IsMale, title=Title, level=Levle, fight_power=FightPower}|InfoAcc], [MRoleID|IDAcc]}
                    end, {[], []}, TeamMember) 
            end,
            IsAfkPunish = lists:any(fun(RID)-> 
                            {S,_Timestamp} = afk_record_server:get_afk_punish(RID,?AFK_TYPE_CONQUERISLAND),
                            2 =:= S
                end, IDList),
            case carlos_aux:check_team_sign_time(IDList) of
                false ->
                    sign_result(IDList, 9);
                _ when IsAfkPunish =:= true andalso TeamID =:= -1 ->
                    sign_result(IDList, 21);
                _ when IsAfkPunish =:= true andalso TeamID /= -1 ->
                    sign_result(IDList, 22);
                _ ->
                    Request = #request{serverID=ServerID, id = ID, members=Info, level = Level},
                    ets:insert(?ETS_CONQUERISLAND_SIGN_INFO, {{sign, ID}, {IDList,?SIGN_FORECAST_STATE}}),
                    conquerisland_match:send_to_me({request, Request}),
                    behavior_carlos:log(IDList, ?carlos_type_conquerisland, ?carlos_op_sign) 
            end 
    end,
    {noreply, State};

%% 报名结果
do_handle_info({sign_result, ID, Result}, State) ->
    case ets:lookup(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) of
        [] ->
            ignore;
        [{_, {IDList,_OldState}}] ->
            case Result of
                0 ->
                    %%返回报名成功，修改报名状态为成功状态
                    ets:insert(?ETS_CONQUERISLAND_SIGN_INFO,{{sign,ID},{IDList,?SIGN_SUCCESS_STATE}});
                4 ->
                    %%出现这种情况，应该是报名成功之后，没有修改状态，导致重复报名
                    ets:insert(?ETS_CONQUERISLAND_SIGN_INFO,{{sign,ID},{IDList,?SIGN_SUCCESS_STATE}});
                _ ->
                    ets:delete(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) 
            end,
            sign_result(IDList, Result) 
    end,
   {noreply, State}; 

%% 取消匹配
do_handle_info({unrequest, ID}, State) ->
    case ets:lookup(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) of
	[] ->
		ignore;
	[{_,{IDList,_OldState}}] ->
        ServerID = data_setting:get(server_id),
		conquerisland_match:send_to_me({unrequest, ServerID, ID}),
		carlos_aux:set_team_unrequest_time(IDList),
        behavior_carlos:log(IDList, ?carlos_type_conquerisland, ?carlos_op_unsign) 
    end,
    {noreply, State};

%% 成功取消匹配
do_handle_info({reply_unrequest, ID}, State) ->
    %%?ERR("收到退出:~p~n", [ID]),
    case ets:lookup(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) of
        [] ->
            ignore;
        [{_, {IDList,_OldState}}] ->
            ets:delete(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}),
            carlos_aux:set_team_unrequest_time(IDList),
            lists:foreach(fun(E) -> 
                            case role_lib:is_online(E) of
                                true ->
                                    %% 通知客户端取消匹配
                                    ?unicast(E, #sc_conquerisland_unrequest{result=0});
                                _ ->
                                    ignore
                            end
                        end, IDList)
    end,
    {noreply, State};

%% 匹配成功
do_handle_info({match_success, ID}, State) ->
    case ets:lookup(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) of
        [] ->
            ignore;
        [{_, {IDList,_OldState}}] ->
            lists:foreach(fun(MID) ->
                            Times = get_times(MID),
                            {NewLeft, NewMTimes} = deduct_times(Times), 
                            set_times(MID, NewMTimes), 
                            %%此处加一个次数变化日志
                            behavior_carlos:log([MID], ?carlos_type_conquerisland, ?carlos_op_time_match_success,times2logextra(NewMTimes)),
                            case role_lib:is_online(MID) of
                                true ->
                                    ?unicast(MID, #sc_conquerisland_times_update{times=NewLeft});
                                _ ->
                                    ignore
                            end 
                        end, IDList),
            ets:delete(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}),
            behavior_carlos:log(IDList, ?carlos_type_conquerisland, ?carlos_op_match_success) 
    end,
    {noreply, State};

do_handle_info({get_player, RoleID, WarID,SID,P,Type},State) ->
    F = fun()->
                {FighterList,RoleLieuAdd,Talent,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
                ItemList = role_data:get_otherRoleItemEquips(RoleID),
                RP = role_lib:get_rolePublic(RoleID),
                Fly = RP#rolePublic.plane_level,
                SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
                SpeedAdd = data_home:get({constr_type_plane_base,role_home:get_build_type_level(RoleID,?constr_type_plane_base)}),
                Msg = {Type,#base_player{serverID=data_setting:get(server_id)
                                   ,roleID=RoleID
                                   ,roleName=RP#rolePublic.roleName
                                   ,roleLevel=RP#rolePublic.level
                                   ,head = RP#rolePublic.head
                                   ,title = RP#rolePublic.title
                                   ,isMale = RP#rolePublic.isMale
                                   ,fighters=FighterList
                                   ,addBuff = RoleLieuAdd
                                   ,talent=Talent
                                   ,trSpecial=TrSpecial
                                   ,itemList=ItemList
                                   ,fly=Fly 
                                   ,fight_power = RP#rolePublic.fightPower
                                   ,skin_info = SkinInfo
                                   ,type=Type
                                   ,grade=doublematch_server:dirty_get_role_dm_grade(RoleID)
                                   ,speed=(get_fly_speed(Fly) * (10000 + SpeedAdd)) div 10000
                                  }},
                % ?ERR("Msg:~w SID:~w P:~w ~n",[Msg,SID,P]),
                send_msg:direct(SID,P,Msg),
                ets:insert(?ETS_CONQUERISLAND_WAR_INFO,{{match_info,RoleID},{WarID, SID,util:now()}}),
                behavior_carlos:log(RoleID, ?carlos_type_conquerisland, ?carlos_op_enter_war, WarID),
                %% 此处是否需要出发成就
                % ?CATCH(role_task:send_dispach(RoleID,{dispach_task,carlos_fight,1})),
                ok
        end,
    spawn(F),
    {noreply,State};
%% 存盘
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
	ets_clean(),
    {noreply, State};

do_handle_info({timer_wheel_tick, LastTick}, State) ->
	timer_wheel:work(LastTick),
	{noreply, State};

%%处理战斗服务器发送来的战斗结果消息
do_handle_info({end_war,_WarID,RoleList},State)->
    lists:foreach(fun({RoleID,FightResult})->
        %%删除战场映射
        delete_role_war_info(RoleID),
        %%向玩家发送战斗奖励
        send_conquerisland_reward(RoleID,FightResult)
    end,RoleList),
    {noreply,State}; 


do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
    {noreply, State}.

ets_clean()->
	Time = data_conquerisland:get(war_interval),
	MaxSec = util:now() - Time,
	Select = ets:fun2ms(fun({_,{_,_,S}}) when S =< MaxSec -> true end),
	F = fun()->
				ets:select_delete(?ETS_CONQUERISLAND_WAR_INFO,Select)
		end,
	spawn(F),
	ok.

do_persist(State) ->
	WarInfo = ets:tab2list(?ETS_CONQUERISLAND_WAR_INFO),
	SignInfo = ets:tab2list(?ETS_CONQUERISLAND_SIGN_INFO),
	RoleTimes = ets:tab2list(?ETS_CONQUERISLAND_ROLETIMES_INFO),
    {RoleTimes2,DelRoleTimes} = delete_invalid_role_time(RoleTimes),
    Info = [{state,State},{WarInfo,SignInfo,RoleTimes2}],
    db_sql:set_etc(?DB_ETC_KEY_CONQUERISLAND, Info),
    lists:foreach(fun(Obj)->ets:delete_object(?ETS_CONQUERISLAND_ROLETIMES_INFO,Obj) end,DelRoleTimes).

delete_invalid_role_time(RoleTimes)->
        lists:foldl(fun({_K,V}=Elem,{Acc,Del})->
                case util:now() > V#times.valid_timestamp andalso V#times.has_buy =< 0 of
                    true->
                        {Acc,[Elem|Del]};
                    false->
                        {[Elem|Acc],Del}
                end
    end,{[],[]},RoleTimes).

%%0点定时刷新更改所有玩家的次数
hook_zero_clock() ->
    NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
    timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
    %% 刷新次数
    lists:foreach(fun({K, V}) ->
                     FreeTimeLMT = data_conquerisland:get(times_limit),
                     {BuyTimeLMT, _} = data_conquerisland:get(buy_limit),
                     ets:insert(?ETS_CONQUERISLAND_ROLETIMES_INFO, {K, V#times{buy_left=BuyTimeLMT, free_left=FreeTimeLMT,valid_timestamp=get_roletime_validtime()}})
                end, ets:tab2list(?ETS_CONQUERISLAND_ROLETIMES_INFO)).

get_roletime_validtime()->
    Day = erlang:date(),
    util:datetime_to_seconds({Day,{0,0,1}}) + ?ONE_DAY_SECONDS.

sign_result(IDList, Result) ->
    lists:foreach(fun(MID) ->
                    #times{has_buy=HasBuy, free_left=FreeLefts} = get_times(MID),
                        case role_lib:is_online(MID) of
                            true ->
                                ?unicast(MID, #sc_conquerisland_sign{result=Result, times=HasBuy + FreeLefts});
                            _ ->
                                ignore
                        end 
                    end, IDList).

%% 获得次数
get_times(RoleID) ->
    case ets:lookup(?ETS_CONQUERISLAND_ROLETIMES_INFO, {times, RoleID}) of
        [] ->
            FreeTimeLMT = data_conquerisland:get(times_limit),
            {BuyTimeLMT, _} = data_conquerisland:get(buy_limit),
            #times{buy_left=BuyTimeLMT, free_left=FreeTimeLMT};
        [{_, Times}] ->
            Times
    end.

%% 设置次数
set_times(RoleID, Times) ->
    ets:insert(?ETS_CONQUERISLAND_ROLETIMES_INFO, {{times, RoleID}, Times#times{valid_timestamp=get_roletime_validtime()}}).

%% 判断玩家个人是否保报了名
is_role_sign(RoleID) ->
    ID = ?make_id(-RoleID, 1),
    case ets:lookup(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) of 
        [_] ->
            {true, ID};
        _ ->
            false
    end.

%% 判断队伍是否报名了
is_team_sign(TeamID, TeamLen) ->
    ID = ?make_id(TeamID, TeamLen),
    case ets:lookup(?ETS_CONQUERISLAND_SIGN_INFO, {sign, ID}) of
        [_] ->
            {true, ID};
        _ ->
            false
    end.

%% 玩家是否在战场
is_role_in_war(RoleID) ->
    case ets:lookup(?ETS_CONQUERISLAND_WAR_INFO, {match_info, RoleID}) of
        [_] ->
            true;
        _ -> 
            false
    end.

%% 判断队伍是否在战场
is_team_in_war(TeamMember) ->
    lists:any(fun(#p_team_member_info{roleID=RoleID}) -> is_role_in_war(RoleID) end, TeamMember).

send_to_me(Msg)->
    erlang:send(?MODULE,Msg).

%%删除玩家的战斗服映射
delete_role_war_info(RoleID)->
    ets:delete(?ETS_CONQUERISLAND_WAR_INFO,{match_info,RoleID}).

%%向玩家发送奖励
send_conquerisland_reward(RoleID,{_KillNum,_CentreNum,BossDemage,Honor,FightResult})->
    MailTempleID = case FightResult of 1->?MAIL_TEMPLATE_CONQUERISLAND_REWARD_WIN;2->?MAIL_TEMPLATE_CONQUERISLAND_REWARD_EQUAL;3->?MAIL_TEMPLATE_CONQUERISLAND_REWARD_LOSE;4->?MAIL_TEMPLATE_CONQUERISLAND_REWARD_KICK;_->?MAIL_TEMPLATE_CONQUERISLAND_REWARD_LOSE end,
    if
        FightResult =:= 4 ->    %% 挂机的话可能要不发奖励
            afk_record_server:add_afk_record(RoleID,?AFK_TYPE_CONQUERISLAND),
            Reward = #sell_reward{},
            ArgsList = [0];
        true ->
            #sell_reward{item=OldItem} = data_conquerisland:get({fight_result_extra_reward,FightResult}),
            ItemList = case Honor of 0->[];_->[#new_item{itemTypeID=20040,itemNum=Honor,itemLevel=1,itemRank=0}] end,
            Reward = #sell_reward{item=OldItem++ItemList},
            ArgsList = [Honor]
    end,
    mail_server:send_sys_mail(RoleID,MailTempleID, ArgsList , "", Reward),
    %%触发玩家征服之岛Boss伤害
    role_payGuide:asyn_trigger_task_change(RoleID,?CONQUERISLAND_BOSS_DEMAGE_TN,{BossDemage}).

%% 扣除次数(传入的必须是有可用次数的)
deduct_times(#times{has_buy=HasBuy, free_left=FreeLefts} = Times) ->
    NewLeft = HasBuy + FreeLefts - 1,
    NewTimes = 
        case FreeLefts > 0 of
            true ->
                Times#times{free_left=FreeLefts - 1};
            _ ->
                Times#times{has_buy=HasBuy - 1}
        end,
    {NewLeft, NewTimes}.

get_fly_speed(Type) ->
    Speed = data_carlos:get({speed, Type}),
    if is_integer(Speed) ->
           Speed;
        true ->
            1
    end.

get_clear_timestamp()->
    {Year,Month,_Day} = erlang:date(),
    {NextYear,NextMonth} = if
        Month>=12 ->
            {Year+1,1};
        true->
            {Year,Month+1} 
    end,
    util:datetime_to_seconds({{NextYear,NextMonth,1},{0,0,1}}).

clear_role_times()->
    NextClearTimeStamp = get_clear_timestamp(),
    timer_wheel:add_plan(NextClearTimeStamp,fun clear_role_times/0),
    ets:delete_all_objects(?ETS_CONQUERISLAND_ROLETIMES_INFO).

test_get_sign_list()->
    ets:tab2list(?ETS_CONQUERISLAND_SIGN_INFO).

%%=======================================================更改玩家的飞机次数命令=======================================================
test_fix_role_plane_times([])->
    ignore;
test_fix_role_plane_times([{RoleID,AddBuyTimes,AddBuyLeft,AddFreeLeft,Type}|T])->
    case Type of
        1->
            test_fix_role_conquerisland_times(RoleID,AddBuyTimes,AddBuyLeft,AddFreeLeft);
        _->
            ?ERR("undefined Type:~w ~n",[Type])
    end,
    test_fix_role_plane_times(T).

test_fix_role_conquerisland_times(RoleID,AddBuyTimes,AddBuyLeft,AddFreeLeft)->
    TarServerID = (RoleID div 1000000)-1,
    ServerID = data_setting:get(server_id),
    MergeServerIDL = data_setting:get(merge_server_id_list),
    case lists:member(TarServerID,[ServerID|MergeServerIDL]) of
        false->
            ?ERR("wrong serverID:~w ~w ~n",[TarServerID,[ServerID|MergeServerIDL]]);
        true->
            #times{has_buy=HasBuy,buy_left=BuyLeft,free_left=FreeLeft}=T = conquerisland_server:get_times(RoleID),
            NT = T#times{has_buy=erlang:max(HasBuy+AddBuyTimes,0),buy_left=erlang:max(BuyLeft+AddBuyLeft,0),free_left=erlang:max(FreeLeft+AddFreeLeft,0)},
            conquerisland_server:set_times(RoleID,NT),
            ?ERR("Finish fix RoleID:~w T:~w NT:~w ~n",[RoleID,T,NT])
    end.

test_fix_role_plane_times_rpc(List)->
    ArrangeList = lists:foldl(fun({RoleID,AddBuyTimes,AddBuyLeft,AddFreeLeft,Type}=E,Acc)->
        ServerID = (RoleID div 1000000)-1,
        case lists:keytake(ServerID,1,Acc) of
            false->
                [{ServerID,[E]}|Acc];
            {_Value,{ServerID,OldL},Other}->
                [{ServerID,[E|OldL]}|Other]
        end
    end,[],List),
    lists:foreach(fun({ServerID,L})->
        case node_info_server:get_node_info(ServerID) of
            ignore->
                ?ERR("can not find Node for serverID:~w ~n",[ServerID]),
                ignore;
            Node ->
                rpc:call(Node,conquerisland_server,test_fix_role_plane_times,[L])
        end
    end,ArrangeList).

times2logextra(#times{has_buy=HasBuy,buy_left=BuyLeft,free_left=FreeLeft})->
    HasBuy*100+BuyLeft*10+FreeLeft.