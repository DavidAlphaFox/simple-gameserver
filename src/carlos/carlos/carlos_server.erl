%% 消息发送
-module(carlos_server).

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
-define(CurrentRankInfoList,currentrankinfolist).
-define(PreRankInfoList,prerankinfolist).
-define(PRETYPE,2).
-define(CURRENTTYPE,1).
-define(SYNSUCCESS,1).
-define(SYNFAILED,2).
-define(syn_flage,syn_flage).
-define(SynInterval,5).
-define(SynInterval2,300).
-define(MAXSYNTIME,60).
-define(CURRENTSEASON,1).
-define(PRESEASON,2).
-record(state, {}).

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
    {ok,_} = 
    supervisor:start_child(carlos_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]}).

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
    NewState = 
        case db_sql:get_etc(?DB_ETC_KEY_CARLOS) of
            [{state, State},{CarlosWarInfo,CarlosSignInfo,CarlosRoleTimes}] ->
				lists:foreach(fun({K,V})-> ets:insert(?ETS_CARLOS_ROLE_WAR,{K,V}) end, CarlosWarInfo),
				lists:foreach(fun({K,V})-> ets:insert(?ETS_CARLOS_SIGN,{K,V}) end, CarlosSignInfo),
				% lists:foreach(fun({K,V})-> ets:insert(?ETS_CARLOS_ROLETIMES,{K,V}) end, CarlosRoleTimes),
                %%在times结构中添加有效期的时间戳，减少ets表存储已经过期很久的玩家卡洛斯次数信息
                init_ets_carlos_roletimes(CarlosRoleTimes),
                State;
            _ ->
                #state{}
        end,
    %%此处添加判断，防止carlos_server在分配服上面一直请求赛季信息,不知道会不会影响卡洛斯的进行
    case data_setting:get(server_type) of
        normal->
            put(?syn_flage,{?SYNFAILED,?MAXSYNTIME}),
            get_all_seasoninfo();
        _->
            ignore
    end,
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    %% 每天晚上12点统一刷新卡洛斯次数
	timer_wheel:init(),
    NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
    timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
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
init_ets_carlos_roletimes(CarlosRoleTimes)->
    lists:foreach(fun({K,V})->
        case is_record(V,times) of
            true->
                NowSec = util:now(),
                case V#times.has_buy =< 0 of
                    true->
                        case NowSec >= V#times.valid_timestamp of
                            true->
                                ignore;
                            false->
                                ets:insert(?ETS_CARLOS_ROLETIMES,{K,V})
                        end;
                    false->
                        case NowSec >=V#times.valid_timestamp of
                            true->
                                FreeTimeLMT = data_carlos:get(times_limit),
                                {BuyTimeLMT, _} = data_carlos:get(buy_limit),
                                ets:insert(?ETS_CARLOS_ROLETIMES,{K,V#times{buy_left=BuyTimeLMT, free_left=FreeTimeLMT}});
                            false->
                                ets:insert(?ETS_CARLOS_ROLETIMES,{K,V})
                        end
                end;
            false->
                {_,HasBuy,BuyLeft,FreeBuy} = V,
                Times = #times{has_buy=HasBuy,buy_left=BuyLeft,free_left=FreeBuy,valid_timestamp=get_carlostime_validtime()},
                ets:insert(?ETS_CARLOS_ROLETIMES,{K,Times})
        end 
    end, CarlosRoleTimes).

handle_call({get_seasoninfo,Type},_From,State)->
    Reply=get_rankinfo_list(Type),
    {reply,Reply,State};

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
    
    %% role_carlos 会判断这些
    case ets:lookup(?ETS_CARLOS_SIGN, {sign, ID}) of
        [_] ->
            ignore;
        _ ->
            case get_rankinfo_list(?CURRENTTYPE) of
                {[],0}->                    
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
                    case carlos_aux:check_team_sign_time(IDList) of
                        false ->
                            sign_result(IDList, 9);
                        _ ->
                            Request = #request{serverID=ServerID, id = ID, members=Info, level = Level},
                            ets:insert(?ETS_CARLOS_SIGN, {{sign, ID}, IDList}),
                            carlos_match:send_to_me({request, Request}),
                            behavior_carlos:log(IDList, ?carlos_type_carlos, ?carlos_op_sign) 
                    end 
            end
    end,
    {noreply, State};

%% 取消匹配
do_handle_info({unrequest, ID}, State) ->
    case ets:lookup(?ETS_CARLOS_SIGN, {sign, ID}) of
	[] ->
		ignore;
	[{_,IDList}] ->
        ServerID = data_setting:get(server_id),
		carlos_match:send_to_me({unrequest, ServerID, ID}),
		carlos_aux:set_team_unrequest_time(IDList),
        behavior_carlos:log(IDList, ?carlos_type_carlos, ?carlos_op_unsign) 
    end,
    {noreply, State};

%% 成功取消匹配
do_handle_info({reply_unrequest, ID}, State) ->
    %%?ERR("收到退出:~p~n", [ID]),
    case ets:lookup(?ETS_CARLOS_SIGN, {sign, ID}) of
        [] ->
            ignore;
        [{_, IDList}] ->
            ets:delete(?ETS_CARLOS_SIGN, {sign, ID}),
            carlos_aux:set_team_unrequest_time(IDList),
            lists:foreach(fun(E) -> 
                            case role_lib:is_online(E) of
                                true ->
                                    %% 通知客户端取消匹配
                                    ?unicast(E, #sc_carlos_unrequest{result=0});
                                _ ->
                                    ignore
                            end
                        end, IDList)
    end,
    {noreply, State};

%% 报名结果
do_handle_info({sign_result, ID, Result}, State) ->
    case ets:lookup(?ETS_CARLOS_SIGN, {sign, ID}) of
        [] ->
            ignore;
        [{_, IDList}] ->
            case Result of
                0 ->
                    ignore;
                4 ->
                    ignore;
                _ ->
                    ets:delete(?ETS_CARLOS_SIGN, {sign, ID}) 
            end,

            sign_result(IDList, Result) 
    end,
   {noreply, State}; 

%% 匹配成功
do_handle_info({match_success, ID}, State) ->
    case ets:lookup(?ETS_CARLOS_SIGN, {sign, ID}) of
        [] ->
            ignore;
        [{_, IDList}] ->
            lists:foreach(fun(MID) ->
                            CarlosTims = get_times(MID),
                            {NewLeft, NewMTimes} = deduct_times(CarlosTims), 
                            set_times(MID, NewMTimes), 
                            case role_lib:is_online(MID) of
                                true ->
                                    ?unicast(MID, #sc_carlos_times_update{times=NewLeft});
                                _ ->
                                    ignore
                            end 
                        end, IDList),
            ets:delete(?ETS_CARLOS_SIGN, {sign, ID}),
            behavior_carlos:log(IDList, ?carlos_type_carlos, ?carlos_op_match_success) 
    end,
    {noreply, State};

do_handle_info({get_player, RoleID, WarID,SID,P,Type},State) ->
    ?INFO("get_player:~w war_id:~w",[RoleID,WarID]),
	F = fun()->
				{FighterList,RoleLieuAdd,Talent,TrSpecial} = role_data:get_otherRoleFighter(RoleID),
				ItemList = role_data:get_otherRoleItemEquips(RoleID),
                {CarlosWinTime,CarlosEqualTime,CarlosLoseTime,CarlosSeasonID,CarlosLastWinTime,CarlosLastEqualTime,CarlosLastLoseTime,CarlosLastSeasonID} = 
                    case tk_id:is_robot(RoleID) of
                        true ->
                            {0,0,0,0,0,0,0,0};
                        false ->
                            get_carlos_info(RoleID)
                    end,
				RP = role_lib:get_rolePublic(RoleID),
				Fly = RP#rolePublic.plane_level,
				SkinInfo = role_data:get_otherRoleSkinInfo(RoleID),
                SpeedAdd = data_home:get({constr_type_plane_base,role_home:get_build_type_level(RoleID,?constr_type_plane_base)}),
				Msg = {Type,#player{serverID=data_setting:get(server_id)
								   ,roleID=RoleID
								   ,roleName=RP#rolePublic.roleName
								   ,fighters=FighterList
								   ,baseFighters=FighterList
								   ,addBuff = RoleLieuAdd
								   ,talent=Talent
								   ,trSpecial=TrSpecial
								   ,itemList=ItemList
								   ,type=Type
								   ,fly=Fly
								   ,speed=(get_fly_speed(Fly) * (10000 + SpeedAdd)) div 10000
								   ,level=RP#rolePublic.level
								   ,blood=100
                                   ,head = RP#rolePublic.head
                                   ,title = RP#rolePublic.title
                                   ,isMale = RP#rolePublic.isMale
                                   ,lastcarloswintime= CarlosLastWinTime
                                   ,lastcarlosequaltime= CarlosLastEqualTime
                                   ,lastcarloslosetime = CarlosLastLoseTime
                                   ,lastseasonid = CarlosLastSeasonID
                                   ,carloswintime = CarlosWinTime
                                   ,carlosequaltime = CarlosEqualTime
                                   ,carloslosetime = CarlosLoseTime
                                   ,seasonid = CarlosSeasonID
                                   ,fight_power = RP#rolePublic.fightPower
								   ,skin_info = SkinInfo
								   ,grade=doublematch_server:dirty_get_role_dm_grade(RoleID)
                                   ,vip=role_lib:cacl_vip_info(RP#rolePublic.viplevel, RP#rolePublic.svipLevel)
								  }},
				% ?ERR("msg:~w~n~w",[Msg,P]),
				send_msg:direct(SID,P,Msg),
				ets:insert(?ETS_CARLOS_ROLE_WAR,{{match_info,RoleID},{WarID, SID,util:now()}}),
                behavior_carlos:log(RoleID, ?carlos_type_carlos, ?carlos_op_enter_war, WarID),
                case tk_id:is_robot(RoleID) of
                    true -> ignore;
                    false -> ?CATCH(role_task:send_dispach(RoleID,{dispach_task,carlos_fight,1}))
                end,
				ok
		end,
	spawn(F),
	{noreply,State};
do_handle_info({update_carlos_war,RoleID,WarID}, State)->
	ets:delete(?ETS_CARLOS_ROLE_WAR, {match_info,RoleID}),
    behavior_carlos:log(RoleID, ?carlos_type_carlos, ?carlos_op_close_war, WarID),
	{noreply,State};
do_handle_info({carlos_bc_info,RoleID,Info},State) ->
	?unicast(RoleID, Info),
	{noreply,State};
do_handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};
do_handle_info({mail_reward,RoleID,TemplateID},State) ->
    if
        TemplateID =:= ?MAIL_TEMPLATE_CARLOS_BAN ->
            afk_record_server:add_afk_record(RoleID,?AFK_TYPE_CARLOS);
        TemplateID =:= ?MAIL_TEMPLATE_CARLOS_Win->
            %?ERR("asyn_trigger_task_change~n"),
            %依据邮件的邮件ID来判断是胜利还是失败来确定是否触发玩家卡洛斯任务变化，故此处邮件一定不能有问题
            role_payGuide:asyn_trigger_task_change(RoleID,?CARLOS_WIN_N,{1});
        true ->
            ignore
    end,
    %?ERR("TemplateID:~w ~n",[TemplateID]),
	mail_server:send_sys_mail(RoleID,TemplateID,[],"",[]),
	{noreply,State};
do_handle_info({mail_reward, RoleID, TemplateID, Reward0,R0}=Msg, State) ->
    %?ERR("Msg:~w ~n",[Msg]),
    case activity_server:is_activity(?carlos_honor_double) of
        true ->
            R = R0*2,
            Reward = role_reward:reward_plus_reward(Reward0, Reward0);
        false ->
            R = R0,
            Reward = Reward0
    end,
    case TemplateID of
        ?MAIL_TEMPLATE_CARLOS_Win->
            %依据邮件的邮件ID来判断是胜利还是失败来确定是否触发玩家卡洛斯任务变化，故此处邮件一定不能有问题
            role_payGuide:asyn_trigger_task_change(RoleID,?CARLOS_WIN_N,{1});
        _->
            ignore
    end,
	mail_server:send_sys_mail(RoleID, TemplateID, [R], "", Reward),
%	#data_temp_mail{mailInfoList=MailInfoList} = data_temp_mail:get(TemplateID),
%	lists:foreach(fun(#mail_template{content=Content})->
%						  Content2 = re:replace(Content,"@\\(0\\)@",integer_to_list(R),[global,{return, binary}]),
%						  mail_server:send_mail(0,"",RoleID, ?MAIL_TYPE_REWARD, TemplateID+10,[], gw:gen_utf8_decode_list(Content2,[]), Reward)
%				  end,MailInfoList),
	{noreply,State};
do_handle_info({season_reward,RoleID,TemplateID,Reward,ArgsList},State)->
    ?INFO("接收到season_reward RoleID：~w TemplateID:~w Reward:~w ~n",[RoleID,TemplateID,Reward]),
    % role_mail_gift:send_gift([RoleID],Reward, "", TemplateID, ""),
    mail_server:send_sys_mail(RoleID, TemplateID, ArgsList , "", Reward),
    {noreply,State};

do_handle_info(clear_carlos_buy_time,State)->
    case data_setting:get(server_type) of
        normal->
            spawn(fun()->clear_carlos_buy_time() end);
        _ ->
            ignore
    end,
    {noreply,State};

%% 存盘
do_handle_info(dump_data, State) ->
    erlang:send_after(?DUMP_INTERVAL, erlang:self(), dump_data),
    do_persist(State),
	ets_clean(),
    {noreply, State};

do_handle_info({update_rank_info,{SynRankInfoList,RoleSeasonID,Type,Force}},State)->
    case data_setting:get(server_type) of
        normal->
            ?INFO("接收到carlos_rank向carlos_server同步排名信息：Type:~w Info:~w ID:~w ~n",[Type,SynRankInfoList,RoleSeasonID]),
            put(?syn_flage,{?SYNSUCCESS,?MAXSYNTIME}),
            %%由于使用了变化更新方式，所以需要将收到的列表加入到已有的列表中并进行重新排序和切断
            case Force of
                true->
                    %%进行了全数据更新，直接抛弃旧的数据
                    RankInfoList = SynRankInfoList;
                false->
                    {OldRankInfoList,OldRoleSeasonID} = get_rankinfo_list(Type),
                    case OldRoleSeasonID =:= RoleSeasonID of
                        true->
                            RankInfoList = merge_carlos_rankinfo(OldRankInfoList,SynRankInfoList);
                        false->
                            %%此处发生了赛季切换，直接抛弃旧的列表
                            RankInfoList = SynRankInfoList
                    end
            end,
            set_rankinfo_list({RankInfoList,RoleSeasonID},Type);
        _ ->
            ignore
    end,
    {noreply,State};

do_handle_info({update_role_carlos_info,RoleID,UpdateMsg}=Msg,State)->
    ?INFO("更新玩家RoleID：~w  Msg:~w ~n",[RoleID,Msg]),
    update_role_carlos_info(RoleID,UpdateMsg),
    {noreply,State};

do_handle_info(get_all_seasoninfo,State)->
%    ?INFO("再次请求赛季信息~n"),
    get_all_seasoninfo(),
    {noreply,State};

do_handle_info({timer_wheel_tick, LastTick}, State) ->
	timer_wheel:work(LastTick),
	{noreply, State};

do_handle_info(repair_refresh,State) ->
	repair_refresh(repair_refresh),
	{noreply,State};

do_handle_info({deduct_role_carlostime,RoleID,Msg},State)->
    deduct_carlostime(RoleID,Msg),
    {noreply,State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
    {noreply, State}.

ets_clean()->
	Time = data_carlos:get(war_interval),
	MaxSec = util:now() - Time,
	Select = ets:fun2ms(fun({_,{_,_,S}}) when S =< MaxSec -> true end),
	F = fun()->
				ets:select_delete(?ETS_CARLOS_ROLE_WAR,Select)
		end,
	spawn(F),
	ok.

do_persist(State) ->
	CarlosRoleWar = ets:tab2list(?ETS_CARLOS_ROLE_WAR),
	CarlosSignInfo = ets:tab2list(?ETS_CARLOS_SIGN),
	CarlosRoleTimes = ets:tab2list(?ETS_CARLOS_ROLETIMES),
    {CarlosRoleTimes2,DelCarlosRoleTimes} = delete_invalid_carlos_role_time(CarlosRoleTimes),
    % ?INFO("持久化CarlosRoleTimes:~w Del:~w ~n",[CarlosRoleTimes2,DelCarlosRoleTimes]),
    Info = [{state,State},{CarlosRoleWar,CarlosSignInfo,CarlosRoleTimes2}],
    db_sql:set_etc(?DB_ETC_KEY_CARLOS, Info),
    lists:foreach(fun(Obj)->ets:delete_object(?ETS_CARLOS_ROLETIMES,Obj) end,DelCarlosRoleTimes).

delete_invalid_carlos_role_time(CarlosRoleTimes)->
        lists:foldl(fun({K,V}=Elem,{Acc,Del})->
        case is_record(V,times) of
            true->
                case util:now() > V#times.valid_timestamp andalso V#times.has_buy =< 0 andalso V#times.free_left =< 1 of
                    true->
                        {Acc,[Elem|Del]};
                    false->
                        {[Elem|Acc],Del}
                end;
            false->
                {_,HasBuy,BuyLeft,FreeBuy} = V,
                Times = #times{has_buy=HasBuy,buy_left=BuyLeft,free_left=FreeBuy,valid_timestamp=get_carlostime_validtime()},
                {[{K,Times}|Acc],Del}
        end 
    end,{[],[]},CarlosRoleTimes).


send_carlos_war_msg(Msg) ->
    erlang:send(?MODULE, Msg).

get_fly_speed(Type) ->
	Speed = data_carlos:get({speed, Type}),
	if is_integer(Speed) ->
		   Speed;
		true ->
			1
	end.

%% 获得卡洛斯次数
get_times(RoleID) ->
    case ets:lookup(?ETS_CARLOS_ROLETIMES, {times, RoleID}) of
        [] ->
            FreeTimeLMT = data_carlos:get(times_limit),
            {BuyTimeLMT, _} = data_carlos:get(buy_limit),
            #times{buy_left=BuyTimeLMT, free_left=FreeTimeLMT};
        [{_, Times}] ->
            Times
    end.

%% 设置卡洛斯次数
set_times(RoleID, Times) ->
    ets:insert(?ETS_CARLOS_ROLETIMES, {{times, RoleID}, Times#times{valid_timestamp=get_carlostime_validtime()}}).

%% 判断玩家个人是否保报了名
is_role_sign(RoleID) ->
    ID = ?make_id(-RoleID, 1),
    case ets:lookup(?ETS_CARLOS_SIGN, {sign, ID}) of 
        [_] ->
            {true, ID};
        _ ->
            false
    end.

%% 判断队伍是否报名了
is_team_sign(TeamID, TeamLen) ->
    ID = ?make_id(TeamID, TeamLen),
    case ets:lookup(?ETS_CARLOS_SIGN, {sign, ID}) of
        [_] ->
            {true, ID};
        _ ->
            false
    end.

%% 玩家是否在战场
is_role_in_war(RoleID) ->
    case ets:lookup(?ETS_CARLOS_ROLE_WAR, {match_info, RoleID}) of
        [_] ->
            true;
        _ -> 
            false
    end.

%% 判断队伍是否在战场
is_team_in_war(TeamMember) ->
    lists:any(fun(#p_team_member_info{roleID=RoleID}) -> is_role_in_war(RoleID) end, TeamMember).

%% 扣除次数(传入的必须是有可用次数的)
deduct_times(#times{has_buy=HasBuy, free_left=FreeLefts} = CarlosTimes) ->
    NewLeft = HasBuy + FreeLefts - 1,
    NewTimes = 
        case FreeLefts > 0 of
            true ->
                CarlosTimes#times{free_left=FreeLefts - 1};
            _ ->
                CarlosTimes#times{has_buy=HasBuy - 1}
        end,
    {NewLeft, NewTimes}.

set_rankinfo_list(RankInfoList,Type)->
    DicName = case Type of
        ?PRETYPE->
            ?PreRankInfoList;
        ?CURRENTTYPE->
            ?CurrentRankInfoList;
        _ ->
            Type
    end,
    put(DicName,RankInfoList).

get_rankinfo_list(Type)->
    DicName = case Type of
        ?PRETYPE->
            ?PreRankInfoList;
        ?CURRENTTYPE->
            ?CurrentRankInfoList;
        _ ->
            Type
    end,
    case get(DicName) of
        ?undefined->
            ?INFO("查找rankinfo出现undefined DicName:~w ~n",[DicName]),
            carlos_rank:send_to_me({get_rankinfo,data_setting:get(server_id),Type}),
            {[],0};
        X ->
            X
    end.

update_role_carlos_info(RoleID,UpdateMsg)->
    case catch role_lib:send_server(RoleID, {update_role_carlos_info,UpdateMsg}) of
                {'EXIT',_}->
                    {WinTime,EqualTime,LoseTime,SeasonID,_RankScore,LastWinTime,LastEqualTime,LastLoseTime,LastSeasonID} = UpdateMsg,
                    db_sql:update_role_carlos_info(RoleID,WinTime,EqualTime,LoseTime,SeasonID,LastWinTime,LastEqualTime,LastLoseTime,LastSeasonID);
                _ ->
                    ignore                            
    end.

get_all_seasoninfo()->
    case get(?syn_flage) of
        {?SYNFAILED,Times}->
            Interval = case Times > 0 of
                true->
                    put(?syn_flage,{?SYNFAILED,Times-1}),
                    ?SynInterval;
                false->
                    ?SynInterval2
            end,
%            ?INFO("请求获取所有赛季信息~n"),
            carlos_rank:send_to_me({get_rankinfo,data_setting:get(server_id),?PRETYPE}),
            carlos_rank:send_to_me({get_rankinfo,data_setting:get(server_id),?CURRENTTYPE}),
            erlang:send_after(Interval*1000,self(),get_all_seasoninfo);
        {?SYNSUCCESS,_Times}->
            % ?INFO("已经完成第一次赛季数据同步~n")
            ignore
    end.

get_carlos_info(RoleID)->
    RoleInfo = role_lib:call_server(RoleID, get_role_info,1000),
    {_CurrentRankList,CurrentSeasonID}=gen_server:call(carlos_server, {get_seasoninfo,?CURRENTTYPE}),
    {_LastRankList,LastSeasonID} = gen_server:call(carlos_server, {get_seasoninfo,?PRETYPE}), 
    {CarlosWinTime,CarlosEqualTime,CarlosLoseTime,CarlosSeasonID} = 
        case CurrentSeasonID =:= RoleInfo#role.carlosseasonid of
            true->
                {RoleInfo#role.carloswintime,RoleInfo#role.carlosequaltime,RoleInfo#role.carloslosetime,RoleInfo#role.carlosseasonid};
            false->
                {0,0,0,CurrentSeasonID}
        end,
    {CarlosLastWinTime,CarlosLastEqualTime,CarlosLastLoseTime,CarlosLastSeasonID} = 
        case LastSeasonID =:= RoleInfo#role.carloslastseasonid of
            true->
                {RoleInfo#role.carloslastwintime,RoleInfo#role.carloslastequaltime,RoleInfo#role.carloslastlosetime,RoleInfo#role.carloslastseasonid};
            false->
                case RoleInfo#role.carlosseasonid =:= LastSeasonID of
                    true->
                        {RoleInfo#role.carloswintime,RoleInfo#role.carlosequaltime,RoleInfo#role.carloslosetime,LastSeasonID};
                    false->
                        {0,0,0,LastSeasonID}
                end
        end,
    {CarlosWinTime,CarlosEqualTime,CarlosLoseTime,CarlosSeasonID,CarlosLastWinTime,CarlosLastEqualTime,CarlosLastLoseTime,CarlosLastSeasonID}.

hook_zero_clock() ->
    NextZeroClockSec = util:datetime_to_seconds({erlang:date(),{0,0,1}}) + ?ONE_DAY_SECONDS,
    timer_wheel:add_plan(NextZeroClockSec, fun hook_zero_clock/0),
    %% 刷新次数
    lists:foreach(fun({K, V}) ->
                     FreeTimeLMT = data_carlos:get(times_limit),
                     {BuyTimeLMT, _} = data_carlos:get(buy_limit),
                     ets:insert(?ETS_CARLOS_ROLETIMES, {K, V#times{buy_left=BuyTimeLMT, free_left=FreeTimeLMT}})
                end, ets:tab2list(?ETS_CARLOS_ROLETIMES)).


repair_refresh(repair_refresh) ->
	case util:now() < 1440129600 of
		true ->
			lists:foreach(fun({{timer_wheel_plan,_}=K,_}) -> erlang:erase(K);
							 (_) -> ignore
						  end, erlang:get()),
			timer_wheel:init(),
			hook_zero_clock(),
			ok;
		_ ->
			ignore
	end.

repair_refresh()->
	erlang:send(?MODULE,repair_refresh).

get_carlostime_validtime()->
    Day = erlang:date(),
    util:datetime_to_seconds({Day,{0,0,1}}) + ?ONE_DAY_SECONDS.

clear_carlos_buy_time()->
    CarlosRoleTimes = ets:tab2list(?ETS_CARLOS_ROLETIMES),
    NewCarlosRoleTimes = lists:foldl(fun({A,Times}=_Elem,Acc)->
        case Times#times.has_buy =/= 0 of
            true->
                NewTimes = Times#times{has_buy=0},
                [{A,NewTimes}|Acc];
            false->
                Acc
        end
    end,[],CarlosRoleTimes),
    ets:insert(?ETS_CARLOS_ROLETIMES,NewCarlosRoleTimes).

deduct_carlostime(RoleID,{Time,Type,SeasonType}=Msg)->
    case catch role_lib:send_server(RoleID, {deduct_carlostime,Msg}) of
                {'EXIT',_}->
                    case db_sql:get_roleInfo(RoleID) of
                        ?undefined->
                            ?ERR("减少玩家RoleID：~w 卡洛斯次数，数据库未找到玩家信息~n",[RoleID]);
                        RoleInfo->
                            NewRoleInfo = case SeasonType of
                                ?CURRENTSEASON->
                                    case Type of
                                        win->
                                            RoleInfo#role{carloswintime=erlang:max(0,RoleInfo#role.carloswintime-Time)};
                                        equal->
                                            RoleInfo#role{carlosequaltime=erlang:max(0,RoleInfo#role.carlosequaltime-Time)};
                                        lose->
                                            RoleInfo#role{carloslosetime=erlang:max(0,RoleInfo#role.carloslosetime-Time)};
                                        _ ->
                                            ?INFO("undefined Type:~w ~n",[Type]),
                                            RoleInfo
                                    end;
                                ?PRESEASON->
                                    case Type of
                                        win->
                                            RoleInfo#role{carloslastwintime=erlang:max(0,RoleInfo#role.carloslastwintime-Time)};
                                        equal->
                                            RoleInfo#role{carloslastequaltime=erlang:max(0,RoleInfo#role.carloslastequaltime-Time)};
                                        lose->
                                            RoleInfo#role{carloslastlosetime=erlang:max(0,RoleInfo#role.carloslastlosetime-Time)};
                                        _ ->
                                            ?INFO("undefined Type:~w ~n",[Type]),
                                            RoleInfo
                                    end; 
                                _ ->
                                    ?INFO("undefined SeasonType:~w ~n",[SeasonType]),
                                    RoleInfo
                            end,
                            db_sql:update_roleInfo(NewRoleInfo)
                    end;
                _ ->
                    ignore                            
    end.

merge_carlos_rankinfo(OldRankInfoList,[])->
    OldRankInfoList;
merge_carlos_rankinfo([],NewRankInfoList)->
    NewRankInfoList;
merge_carlos_rankinfo(OldRankInfoList,NewRankInfoList)-> 
    OldRankInfoList1 = lists:filter(fun(Player)->
        case lists:keyfind(Player#p_player_info.roleID,#p_player_info.roleID,NewRankInfoList) of
            false ->
                true;
            _FindOne->
                false
        end
    end,OldRankInfoList),
    RankInfoList1 = OldRankInfoList1++NewRankInfoList,
    RankInfoList2 = lists:sort(fun(A,B)->carlos_rank:compare_player(A,B) end,RankInfoList1),
    RankInfoList3 = lists:sublist(RankInfoList2,erlang:min(length(RankInfoList2),data_carlos:get(max_rank_num))),
    {RankInfoList4,_Index} = lists:foldr(fun(Player,{Acc,Index})->
        {[Player#p_player_info{rank=Index}|Acc],Index-1}
    end,
    {[],length(RankInfoList3)},RankInfoList3),
    RankInfoList4.

sign_result(IDList, Result) ->
    lists:foreach(fun(MID) ->
                    #times{has_buy=HasBuy, free_left=FreeLefts} = get_times(MID),
                        case role_lib:is_online(MID) of
                            true ->
                                ?unicast(MID, #sc_carlos_sign{result=Result, times=HasBuy + FreeLefts});
                            _ ->
                                ignore
                        end 
                    end, IDList).
