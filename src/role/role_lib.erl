%% @author caohongyang
%% @doc 角色基础接口
%% Created 2013-3-4


-module(role_lib).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_carlos.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

-define(enery_to_unioncoin, enery_to_unioncoin).  %记录体力消耗，用户增加公会货币。

%% ====================================================================
%% API functions
%% ====================================================================
gw(RoleID) ->
	whereis(gatewayRegName(RoleID)).

gatewayRegName(RoleID) ->
	list_to_atom("gw"++integer_to_list(RoleID)).

regName(RoleID) ->
	list_to_atom("role"++integer_to_list(RoleID)).

pid(RoleID) ->
	whereis(regName(RoleID)).

send_client_no_block(RoleID, Info) ->
    case catch process_info(gw(RoleID),dictionary) of
    {dictionary, [{socket,Socket}]} ->
        catch tk_misc:send_sock_no_block(Socket, Info);
    _ ->
    ignore
end.

send_client(RoleID, Info) ->
	case catch process_info(gw(RoleID),dictionary) of
	{dictionary, [{socket,Socket}]} ->
		catch tk_misc:send_sock(Socket, Info);
	_ ->
	ignore
end.
	%catch erlang:send(gatewayRegName(RoleID), {send_client, Info}).

send_client2(GatewayPid, Info) ->
	case catch process_info(GatewayPid,dictionary) of
	{dictionary, [{socket,Socket}]} ->
		catch tk_misc:send_sock(Socket, Info);
	_ ->
	ignore
end.
	%catch erlang:send(GatewayPid, {send_client, Info}).
	

send_client3(GWPid, Info) when is_pid(GWPid) ->
    case catch process_info(GWPid, dictionary) of
        % {dictionary,[{socket,Socket}]} ->
        {dictionary,List}->
        	case lists:keyfind(socket,1,List) of
        		false->
        			ignore;
        		{socket,Socket}->
            		catch tk_misc:send_sock_async(Socket, Info)
            end;
        _ -> 
       		ignore
    end;
send_client3(RoleID,Info)->
	case catch process_info(gw(RoleID),dictionary) of
	% {dictionary, [{socket,Socket}]} ->
	{dictionary,List}->
		case lists:keyfind(socket,1,List) of
			false->
				ignore;
			{socket,Socket}->
				catch tk_misc:send_sock_async(Socket, Info)
		end;
	_ ->
		ignore
end.
    	
send_client_force(GatewayPid, Info)->
	case catch process_info(GatewayPid, dictionary) of
		{dictionary, [{socket, Socket}]} ->
			?CATCH(tk_misc:send_sock_force(Socket, Info));
		_ ->
			ignore
	end.

get_sock(RoleID) ->
	case process_info(gw(RoleID),dictionary) of
	{dictionary, [{socket,Socket}]} ->
		Socket;
	_ ->
	undefined
end.

send_client_async(RoleID, Info) when is_integer(RoleID) ->
    case catch process_info(gw(RoleID), dictionary) of
        {dictionary,[{socket,Socket}]} ->
            catch tk_misc:send_sock_async(Socket, Info);
        _ -> ignore
    end;
send_client_async(GWPid, Info) when is_pid(GWPid) ->
    case catch process_info(GWPid, dictionary) of
        {dictionary,[{socket,Socket}]} ->
            catch tk_misc:send_sock_async(Socket, Info);
        _ -> ignore
    end.
	
send_server(RoleID, Info) ->
	erlang:send(regName(RoleID), Info).
	
send_pid(Pid, Info)->
	erlang:send(Pid, Info).

call_server(RoleID, Info)->
	gen_server:call(regName(RoleID), Info).

call_server(RoleID, Info,Timeout)->
    gen_server:call(regName(RoleID), Info,Timeout).

send_self(Info) ->
	tk_misc:send_sock(get(?socket), Info).

send_every_server_msg(Info)->
	[?CATCH(send_server(E, Info))||{E,_}<-ets:tab2list(?ETS_ROLE_ONLINE)].

join_online_table(RoleID, _GateWayPid, Socket) ->
	catch broadcast_server:join(Socket),
	ets:insert(?ETS_ROLE_ONLINE, {RoleID,true}).

leave_online_table(RoleID) ->	
	ets:delete(?ETS_ROLE_ONLINE, RoleID),
	catch fire_server:role_exit_firecrack(RoleID),
	catch nanm_server:offline(RoleID),
	catch hula_server:offline(RoleID),
	catch emperor_server:offline(RoleID),
	catch king_server:offline(RoleID).

is_online(RoleID) ->
	ets:member(?ETS_ROLE_ONLINE, RoleID).


%% @doc 加经验，计算等级成长，
-spec add_exp(#role{}, ?int32) -> 
		  {level_up, NewLevel::?int16, NewExp::?int64} | 
			  {level_not_up, OldLevel::?int16, NewExp::?int64} | 
			  {level_max, OldLevel::?int16, OldExp::?int64}.
add_exp(Role, AddExp) ->
	#role{exp=Exp,level=Level,roleID=RoleID, roleName=RoleName ,  isMale=Is_male, title=Title, head = Head
         ,transmigration=Transmigration}=Role,
	MaxLevel = 
        if
            Transmigration > 0 ->
                data_common:get(max_role_level);
            true ->
                data_common:get(max_role_level_base)
        end,
	NewExpT = Exp+AddExp,
	MaxExp = data_role_level:get(MaxLevel+1)-1,
	if MaxExp > NewExpT ->
		   NewExp=erlang:min(MaxExp, NewExpT),
		   NewLevel = data_role_exp:get(NewExp),
		   case NewLevel >= data_levelRank:get(baseLevel) of
			   true ->
				   {ServerDate,_} = data_setting:get(serverOpenTime),
				   TimePoint = data_levelRank:get(timePoint),
				   APeriod = data_levelRank:get(activityPeriod),
				   STime = util:datetime_to_seconds({ServerDate, TimePoint}) + APeriod  * ?ONE_DAY_SECONDS,
				   case util:now() > STime of
					   true ->
						   ignore;
					   _ ->
%%                            ignore
						   levelRank_server:refresh_ranker_info(RoleID, RoleName, NewExp, NewLevel, Is_male, Title, Head)
				   end;
			   _ ->
				   ignore
		   end,
		   if NewLevel > Level ->
				  {level_up, NewLevel, NewExp};
			  true ->
				  {level_not_up, Level, NewExp}
		   end;
	   true ->
		   {level_max, Level, Exp}
	end.

%% @doc 是否拥有id为GerID的武将
has_gerID(GerID) ->
	case role_data:get_ger(GerID) of
		false  ->
			false;
		_ ->
			true
	end.

reward2p_reward(Reward) ->
	#reward{coin=Coin,roleExp=RoleExp,gold=Gold} = Reward,		   
	#p_reward{coin=Coin,roleExp=RoleExp,gold=Gold,levelExp=0}.

%% @doc 角色属性更新
notify_update(Record) ->
	?sendself(Record).

to_sec({A,B,C}) ->
	A*3600+B*60+C.

-define(maxPvp, (data_common:get(max_pvp_times))).
-define(maxPlunder, (data_common:get(max_plunder_times))).
%% @doc 初始化体力、探索次数回复
init_roleTimes_recover_timer(RoleTimes, RoleInfo, AlienInfo) ->
	NowSec = util:now(),
	#roleTimes{energy=Energy, 
			   	challengeGodEnergy=ChallengeGodEnergy,
			    challengeGodBuyTimes=ChallengeGodBuyTimes,
				lastChallengeGodDate=LastCGD,
			    refreshLieuTimes = RefreshLieuTimes,
			   	lastEnergyTime=LastET,
			   	discoveryTimes=Dscv,
			   	lastDscvTime=LastDscv,
			   	pvpTimes=PvpTimes,
				weiboCount=WeiboCount,
				nextWeiboCountRefreshSec=NextWeiboCountRefreshSec,
				lastPvpTime = LastPvpTime,
                teamPkTimes = TeamPkTimesT,
                lastTeamPkTime = LastTeamPkTime ,
               energyPac=EnergyPac
			  } =RoleTimes,
    case RoleInfo of
        #role{vipLevel=VipLevel,level=RoleLevel} ->
            next;
        _ ->
            VipLevel = 0,
            RoleLevel = 0
    end,
	#role{lastLogoutTime=RLLT} = RoleInfo,
	{LastLogOutDate,_} = util:seconds_to_datetime(RLLT),
	RefreshLieuFreeTimes =
		case LastLogOutDate == erlang:date() of
			true ->
				RefreshLieuTimes;
			_ ->
				data_lieu_clo_setting:get(daily_free_refresh)
		end,
	#role{familyID = FamilyID} = RoleInfo,
%%计算公会科技对体力恢复的影响
	EInterval = to_sec(data_common:get(energy_recover_interval)),
	FamilyTek_ET_Add = calculate_familyTek_addbuff(FamilyID,4,1),
    put(?familyTecDec,{FamilyTek_ET_Add, 0}),
	% ?ERR("FamilyTek_ET_Add ~w ~n",[FamilyTek_ET_Add]),
	EInterval2 = role_lib:calculate_familyTekeffectTime(EInterval,FamilyTek_ET_Add),
	% ?ERR("EInterval2 is ~w ~n",[EInterval2]), 
	put(?currentEnergyIntercal,EInterval2), 
%%计算公会科技对探索恢复的影响
	DInterval = to_sec(data_common:get(dscv_recover_interval)),
	FamilyTek_DT_Add = calculate_familyTek_addbuff(FamilyID,3,1),
	% ?ERR("FamilyTek_DT_Add ~w~n",[FamilyTek_DT_Add]),
	DInterval2 = role_lib:calculate_familyTekeffectTime(DInterval,FamilyTek_DT_Add),
	put(?currentdiscoveryInterval,DInterval2),  
%%计算公会科技对竞技场恢复的影响
	PvpInterval = to_sec(data_common:get(pvp_recover_interval)),
	FamilyTek_Pvp_Add = calculate_familyTek_addbuff(FamilyID,5,1),
	% ?ERR("FamilyTek_Pvp_Add ~w~n",[FamilyTek_Pvp_Add]),
	PvpInterval2 = role_lib:calculate_familyTekeffectTime(PvpInterval,FamilyTek_Pvp_Add),
	put(?currentPvpInterval,PvpInterval2),
%%计算公会科技对符文争夺恢复的影响
	PlunderInterval = to_sec(data_common:get(plunder_recover_interval)),
	FamilyTek_Pl_Add = calculate_familyTek_addbuff(FamilyID,6,1),
	% ?ERR("FamilyTek_Pl_Add ~w~n",[FamilyTek_Pl_Add]),
	PlunderInterval2 = role_lib:calculate_familyTekeffectTime(PlunderInterval,FamilyTek_Pl_Add),
	put(?currentPlunderInterval,PlunderInterval2),
    
    %% 3v3 恢复时间
    TeamPkInterVal = to_sec(data_common:get(teamPk_recover_interval)),
    put(?currentTeamPkIntercal, TeamPkInterVal),

	EStartTime = data_common:get(energy_recover_start_time),
	{Energy2, LastET2, MaxAdd} = cacl_add_num2(Energy, get_max_energy(VipLevel), LastET, NowSec, EStartTime, EInterval2),
    EnergyPac2 = add_role_energy_pack(MaxAdd+Energy-Energy2,EnergyPac),
	DStartTime = data_common:get(dscv_recover_start_time),
	
	{Dscv2, LastDscv2} = cacl_add_num(Dscv, get_max_dscv_times(VipLevel), LastDscv, NowSec, DStartTime, DInterval2),
	PvpStartTime = data_common:get(pvp_recover_start_time),
	
	{PvpTimes2, LastPvpTime2} = cacl_add_num(PvpTimes, ?maxPvp, LastPvpTime, NowSec, PvpStartTime, PvpInterval2),
	%PlunderStartTime = data_common:get(plunder_recover_start_time),
	
	%{PlunderTimes2, LastPlunderTime2} = cacl_add_num(PlunderTimes, ?maxPlunder, LastPlunderTime, NowSec, PlunderStartTime, PlunderInterval2),
    AStartTime = data_common:get(alien_recover_start_time),
    AInterval = to_sec(data_common:get(alien_recover_interval)),
    case AlienInfo of
        ?undefined ->
            AlienTimes = 0,
            LastAT = 0,
            {_AlienTimes2, LastAT2} = cacl_add_num(AlienTimes, get_max_alien_times(VipLevel, RoleLevel), LastAT, NowSec, AStartTime, AInterval);
        #alien_info{times=AlienTimes, lastRecoverTime=LastAT} ->
            {AlienTimes2, LastAT2} = cacl_add_num(AlienTimes, get_max_alien_times(VipLevel, RoleLevel), LastAT, NowSec, AStartTime, AInterval),
            role_data:set_roleAlienInfo(AlienInfo#alien_info{times=AlienTimes2, lastRecoverTime=LastAT2})
    end,

    TeamPkStartTime = data_common:get(teamPk_recover_start_time),
    TeamPkMax = get_max_teamPk(VipLevel),
    TeamPkTimes = 
        %% 每次开始3v3都将挑战次数重置为最大值,所以在3v3开启前下线的,都进行重置
        case ets:lookup(?ETS_ETC, team_pk_server) of
            [] ->
                TeamPkTimesT;
            [{_,LastOpenTime}] ->
                case RLLT =< LastOpenTime of
                    true ->
                        TeamPkMax;
                    _ ->
                        TeamPkTimesT
                end
        end,
            
	{TeamPkTimes2, LastTeamPkTime2} = cacl_add_num(TeamPkTimes, TeamPkMax, LastTeamPkTime, NowSec, TeamPkStartTime, TeamPkInterVal),
    
	%% 刷新微博分享次数
	if NowSec >= NextWeiboCountRefreshSec ->
			NewWeiboCount = erlang:max(data_common:get(max_weibo_count),WeiboCount),
			WeiboRefreshTime = data_common:get(weibo_refresh_time),
			NextWeiboSec2 = util:datetime_to_seconds({erlang:date(),WeiboRefreshTime}) + ?ONE_DAY_SECONDS;
	   true ->
		   NewWeiboCount = WeiboCount,
		   NextWeiboSec2 = NextWeiboCountRefreshSec
	end,
	
	{NChallengeGodEnergy, NChallengeGodBuyTimes} = 
		case LastCGD =:= erlang:date() of
			true ->
				{ChallengeGodEnergy, ChallengeGodBuyTimes};
			_ ->

				VipChallengeGodFreeTimes = get_max_challengeGodFreeTimes(VipLevel),
				{VipChallengeGodFreeTimes,0}
		end, 
			 
	RoleTimes2 = RoleTimes#roleTimes{energy=Energy2, 
									 challengeGodEnergy=NChallengeGodEnergy,
									 challengeGodBuyTimes=NChallengeGodBuyTimes,
									 lastChallengeGodDate=erlang:date(),
									 refreshLieuTimes=RefreshLieuFreeTimes,
									 lastEnergyTime=LastET2,
									 discoveryTimes=Dscv2,
									 lastDscvTime=LastDscv2,
									 pvpTimes=PvpTimes2,
									 lastPvpTime=LastPvpTime2,
									 weiboCount=NewWeiboCount,					
									 nextWeiboCountRefreshSec=NextWeiboSec2,
                                     teamPkTimes = TeamPkTimes2,
                                     lastTeamPkTime = LastTeamPkTime2,
                                     energyPac=EnergyPac2
									},
	role_data:set_roleTimes(RoleTimes2),
  

	timer_wheel:plan(LastET2+EInterval2, fun add_energy_interval/1),
    timer_wheel:plan(LastAT2+AInterval, fun add_alien_interval/1),
	%timer_wheel:plan(LastDscv2+DInterval2, fun add_dscv_interval/1),
	timer_wheel:plan(LastPvpTime2+PvpInterval2, fun add_pvp_interval/1),
	timer_wheel:plan(LastTeamPkTime2+TeamPkInterVal, fun add_teamPk_interval/1).

add_role_energy_pack(En,Pac) ->
    if En =< 0 -> Pac;
       true -> Max = data_common:get(max_energy_pac),
               min(Max,En+Pac)
    end.

%% @doc 增加体力
add_energy_interval(NowSec) ->
	#roleTimes{energy=Energy,energyPac=EnergyPac} = RoleTimes = role_data:get_roleTimes(),
	EInterval = to_sec(data_common:get(energy_recover_interval)),
	%%计算公会科技对体力恢复的影响
	#role{familyID = FamilyID} = role_data:get_roleInfo(),
	FamilyTek_ET_Add = calculate_familyTek_addbuff(FamilyID,4,1),
	% ?ERR("FamilyTek_ET_Add ~w ~n",[FamilyTek_ET_Add]),
	EInterval2 = role_lib:calculate_familyTekeffectTime(EInterval,FamilyTek_ET_Add), 
	% ?ERR("Old EInterval is ~w, New EInterval is ~w ~n ",[EInterval,EInterval2]),
	NextTick = NowSec + EInterval2,
	put(?currentEnergyIntercal,EInterval2),
    case role_data:get_roleInfo() of
        #role{vipLevel=VipLevel} ->
            next;
        _ ->
            VipLevel = 0
    end,
	case RoleTimes#roleTimes.energy >= get_max_energy(VipLevel) of
		true ->
			%?ERR("role_lib"),
            EnergyPac2 = add_role_energy_pack(1,EnergyPac),
			?notify_update(?ra_energy(Energy, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastEnergyTime=NowSec,energyPac=EnergyPac2};
		false ->
			%?ERR("role_lib"),
			?notify_update(?ra_energy(Energy+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{energy=Energy+1, lastEnergyTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_energy_interval/1).

%% @doc 增加探索次数
add_dscv_interval(NowSec) ->
	#roleTimes{discoveryTimes=D} = RoleTimes = role_data:get_roleTimes(),
	DInterval = to_sec(data_common:get(dscv_recover_interval)),
	%%计算公会科技对探索恢复的影响
	#role{familyID = FamilyID} = role_data:get_roleInfo(),
	FamilyTek_DT_Add = calculate_familyTek_addbuff(FamilyID,3,1),
	% ?ERR("FamilyTek_DT_Add ~w~n",[FamilyTek_DT_Add]),
	DInterval2 = role_lib:calculate_familyTekeffectTime(DInterval,FamilyTek_DT_Add), 
	NextTick = NowSec + DInterval2,
	put(?currentdiscoveryInterval,DInterval2),
    case role_data:get_roleInfo() of
        #role{vipLevel=VipLevel} ->
            next;
        _ ->
            VipLevel = 0
    end,
	case RoleTimes#roleTimes.discoveryTimes >= get_max_dscv_times(VipLevel) of
		true ->
			?notify_update(?ra_dscv(D, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastDscvTime=NowSec};
		false ->
			?notify_update(?ra_dscv(D+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{discoveryTimes=D+1, lastDscvTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_dscv_interval/1).

%% 增加异星次数
add_alien_interval(NowSec) ->
    AInterval = to_sec(data_common:get(alien_recover_interval)),
    NextTick = NowSec + AInterval,
    case role_data:get_roleAlienInfo() of
        #alien_info{times=A} = AlienInfo ->
            case role_data:get_roleInfo() of
                #role{vipLevel=VipLevel,level=RoleLevel} ->
                    next;
                _ ->
                    VipLevel = 0,
                    RoleLevel = 0
            end,
            case A >= get_max_alien_times(VipLevel, RoleLevel) of
                true ->
                    ?notify_update(?ra_alien(A, NextTick)),
                    AlienInfo2 = AlienInfo#alien_info{lastRecoverTime=NowSec};
                false ->
                    ?notify_update(?ra_alien(A+1, NextTick)),
                    AlienInfo2 = AlienInfo#alien_info{times=A+1, lastRecoverTime=NowSec}
            end,
            role_data:set_roleAlienInfo(AlienInfo2);
        ?undefined ->
            next
    end,
    timer_wheel:plan(NextTick, fun add_alien_interval/1).

%% @doc 增加pvp次数
add_pvp_interval(NowSec) ->
	#roleTimes{pvpTimes=PvpTimes} = RoleTimes = role_data:get_roleTimes(),
	PvpInterval = to_sec(data_common:get(pvp_recover_interval)),
	%%计算公会科技对竞技场恢复的影响
	#role{familyID = FamilyID} = role_data:get_roleInfo(),
	FamilyTek_Pvp_Add = calculate_familyTek_addbuff(FamilyID,5,1),
	% ?ERR("FamilyTek_Pvp_Add ~w~n",[FamilyTek_Pvp_Add]),
	PvpInterval2 = role_lib:calculate_familyTekeffectTime(PvpInterval,FamilyTek_Pvp_Add),
	NextTick = NowSec + PvpInterval2,
	put(?currentPvpInterval, PvpInterval2),
	case PvpTimes >= ?maxPvp of
		true ->
			% ?ERR("Add_PvpInterval2 ~w ~n ",[PvpInterval2]),
			?notify_update(?ra_pvpTimes(PvpTimes, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastPvpTime=NowSec};
		false ->
			% ?ERR("Add_PvpInterval2 ~w ~n ",[PvpInterval2]),
			?notify_update(?ra_pvpTimes(PvpTimes+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{pvpTimes=PvpTimes+1, lastPvpTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_pvp_interval/1).

add_teamPk_interval(NowSec) ->
	#roleTimes{teamPkTimes=TeamPkTimes} = RoleTimes = role_data:get_roleTimes(),
    #role{vipLevel=VipLevel} = role_data:get_roleInfo(), 
	PvpInterval = 
        case get(?currentTeamPkIntercal) of
            undefined ->
                InterVal = to_sec(data_common:get(teamPk_recover_interval)),
	            put(?currentTeamPkIntercal, InterVal),
                InterVal;
            Any ->
               Any 
        end,
	NextTick = NowSec + PvpInterval,
	case TeamPkTimes >= get_max_teamPk(VipLevel) of
		true ->
			?notify_update(?ra_teamPkTimes(TeamPkTimes, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{lastTeamPkTime=NowSec};
		false ->
			?notify_update(?ra_teamPkTimes(TeamPkTimes+1, NextTick)),
			RoleTimes2 = RoleTimes#roleTimes{teamPkTimes=TeamPkTimes+1, lastTeamPkTime=NowSec}
	end,
	role_data:set_roleTimes(RoleTimes2),
	timer_wheel:plan(NextTick, fun add_teamPk_interval/1).

%% 返回值为当前回复后的值，以及时间戳
cacl_add_num(CurNum, MaxNum, LastSec, NowSec, Start, Itv) ->
	DaySec = util:datetime_to_seconds({{1989,7,17}, Start}),
	Inter1 = (NowSec - DaySec ) div Itv,
	NewLastSec = DaySec+Inter1*Itv,
	if CurNum >= MaxNum ->
		   {CurNum, NewLastSec};
	   true ->
		   Inter2 = (LastSec- DaySec ) div Itv,
		   Num = Inter1 - Inter2,
		   {erlang:min(CurNum+Num, MaxNum), NewLastSec}
	end.

cacl_add_num2(CurNum, MaxNum, LastSec, NowSec, Start, Itv) ->
    DaySec = util:datetime_to_seconds({{1989,7,17}, Start}),
    Inter1 = (NowSec - DaySec ) div Itv,
    NewLastSec = DaySec+Inter1*Itv,
               Inter2 = (LastSec- DaySec ) div Itv,
           Num = Inter1 - Inter2,
    Num2 = case LastSec of  1356973261 ->  0 ; _ -> Num end,
    if CurNum >= MaxNum -> 
           {CurNum, NewLastSec,Num2};
       true ->
           {erlang:min(CurNum+Num, MaxNum), NewLastSec,Num2}
    end.
		   
	
%% @doc 加银两
add_coin_f(RoleInfo, AddCoin, Type, ArgID, Desc) ->
	add_coin_f(RoleInfo, AddCoin, Type, ArgID, Desc,true).

add_coin_f(#role{coin=Coin,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddCoin, Type, ArgID, Desc,NeedTrigger) ->
	Coin2 = AddCoin+Coin,
	RoleInfo2 = RoleInfo#role{coin=Coin2},
	{Date, _} = Time = erlang:localtime(),
	behavior_coin_add:log(RoleID, VipLevel, AddCoin, Coin, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_coin(Coin2)),
	case NeedTrigger of
		true ->
			?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddCoin,?REWARD_COIN}));
		_ ->
			ignore
	end,
	RoleInfo2.
%% 添加奥义结晶
add_profoundCrystal_f(RoleInfo, 0, _Type, _ArgID, _Desc)->
	RoleInfo;
	
add_profoundCrystal_f(RoleInfo, AddProfoundCrystal, Type, ArgID, Desc) ->
	add_profoundCrystal_f(RoleInfo, AddProfoundCrystal, Type, ArgID, Desc,true).

add_profoundCrystal_f(#role{profoundCrystal=ProfoundCrystal,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddProfoundCrystal, Type, ArgID, Desc,_NeedTrigger) ->
	ProfoundCrystal2 = AddProfoundCrystal+ProfoundCrystal,
	RoleInfo2 = RoleInfo#role{profoundCrystal=ProfoundCrystal2},
	{Date, _} = Time = erlang:localtime(),
	behavior_profoundCrystal_add:log(RoleID,VipLevel,AddProfoundCrystal, ProfoundCrystal2, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_profoundCrystal(ProfoundCrystal2)),
	% case NeedTrigger of
	% 	true ->
	% 		?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddCoin,?REWARD_COIN}));
	% 	_ ->
	% 		ignore
	% end,
	RoleInfo2.

%% 添加荣誉
add_honor_f(_RoleInfo, 0, _Type, _ArgID, _Desc)->
    ignore;
    
add_honor_f(RoleInfo, AddHonor, Type, ArgID, Desc) ->
    add_honor_f(RoleInfo, AddHonor, Type, ArgID, Desc,true).

add_honor_f(#role{honor=Honor,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddHonor, Type, ArgID, Desc,_NeedTrigger) ->
    Honor2 = AddHonor+Honor,
    RoleInfo2 = RoleInfo#role{honor=Honor2},
    {Date, _} = Time = erlang:localtime(),
    behavior_honor_add:log(RoleID,VipLevel,AddHonor, Honor2, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(RoleInfo2),
    ?notify_update(?ra_honor(Honor2)),
    RoleInfo2.

%% 添加竞技场点数
add_pvppoint_f(_RoleInfo, 0, _Type, _ArgID, _Desc)->
    ignore;
    
add_pvppoint_f(RoleInfo, AddPvpPoint, Type, ArgID, Desc) ->
    add_pvppoint_f(RoleInfo, AddPvpPoint, Type, ArgID, Desc,true).

add_pvppoint_f(#role{pvppoint=PvpPoint,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddPvpPoint, Type, ArgID, Desc,_NeedTrigger) ->
    PvpPoint2 = AddPvpPoint+PvpPoint,
    RoleInfo2 = RoleInfo#role{pvppoint=PvpPoint2},
    {Date, _} = Time = erlang:localtime(),
    behavior_pvppoint_add:log(RoleID,VipLevel,AddPvpPoint, PvpPoint2, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(RoleInfo2),
    ?notify_update(?ra_pvppoint(PvpPoint2)),
    RoleInfo2.

%% @doc 加元宝
add_gold_f(RoleInfo, AddGold, Type, ArgID, Desc)->
	 add_gold_f(RoleInfo, AddGold, Type, ArgID, Desc,true).

add_gold_f(#role{goldBonus=GoldBonus,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddGold, Type, ArgID, Desc,NeedTrigger) ->
	GoldBonus2 = AddGold+GoldBonus,
	RoleInfo2 = RoleInfo#role{goldBonus=GoldBonus2},
	{Date, _} = Time = erlang:localtime(),
	behavior_gold_bonus_add:log(RoleID, VipLevel, AddGold, GoldBonus, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_goldBonus(GoldBonus2)),
	case NeedTrigger of
		true ->
			?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddGold,?REWARD_GOLD}));
		_ ->
			ignore
	end,
	RoleInfo2.
		

%% @doc 加声望
add_reputation_f(RoleInfo, AddRep, Type, ArgID, Desc) ->
    add_reputation_f(RoleInfo, AddRep, Type, ArgID, Desc, true).

add_reputation_f(#role{reputation=Rep,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddRep, Type, ArgID, Desc, NeedTrigger) ->
	Rep2 = AddRep+Rep,
	RoleInfo2 = RoleInfo#role{reputation=Rep2},
	{Date, _} = Time = erlang:localtime(),
	behavior_repu_add:log(RoleID, VipLevel, AddRep, Rep, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_reputation(Rep2)),
    case NeedTrigger of
        true ->
	        ?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddRep,?REWARD_REPU}));
        _ ->
            ignore
    end,
	RoleInfo2.

%% @doc 加公会货币
add_unioncoin_f(#role{unioncoin=Unioncoin,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddUnioncoin, Type, ArgID, Desc) when is_number(AddUnioncoin)->
    Unioncoin2 = AddUnioncoin+Unioncoin,
    RoleInfo2 = RoleInfo#role{unioncoin=Unioncoin2},
    {Date, _} = Time = erlang:localtime(),
    behavior_unioncoin_add:log(RoleID, VipLevel, AddUnioncoin, Unioncoin, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(RoleInfo2),
    ?notify_update(?ra_unioncoin(Unioncoin2)),
%%     ?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,AddGold,?REWARD_GOLD})), %暂时没有成就
    RoleInfo2;
add_unioncoin_f(RoleInfo, AddUnioncoin, Type, ArgID, Desc) ->
    ?ERR("add_unioncoin_f ---> arg is wrong. ~w ~w ~w ~w ~w",[RoleInfo, AddUnioncoin, Type, ArgID, Desc]),
    RoleInfo.

%% 添加家园物资
add_home_resource_f(_RoleInfo, 0, _Type, _ArgID, _Desc)->
    ignore;
    
add_home_resource_f(RoleInfo, AddHomeResource, Type, ArgID, Desc) ->
    add_home_resource_f(RoleInfo, AddHomeResource, Type, ArgID, Desc,true).

add_home_resource_f(#role{home_resource=HomeResource,roleID=RoleID, vipLevel=VipLevel}= RoleInfo, AddHomeResource, Type, ArgID, Desc,_NeedTrigger) ->
    HomeResource2 = AddHomeResource+HomeResource,
    RoleInfo2 = RoleInfo#role{home_resource=HomeResource2},
    {Date, _} = Time = erlang:localtime(),
    behavior_home_resource_add:log(RoleID,VipLevel,AddHomeResource, HomeResource2, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(RoleInfo2),
    ?notify_update(?ra_home_resource(HomeResource2)),
    RoleInfo2.

%% @doc 扣银两
deduct_coin_f(RoleInfo, NeedCoin, _, _, _) when NeedCoin == 0->
    RoleInfo;
deduct_coin_f(#role{coin=Coin,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedCoin, Type, ArgID, Desc) when Coin >= NeedCoin ->
	Coin2 = Coin-NeedCoin,
	Role2 = RoleInfo#role{coin=Coin2},
	{Date, _} = Time = erlang:localtime(),
	behavior_coin_consume:log(RoleID, VipLevel, NeedCoin, Coin, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	%%添加消费返利
	consume_back_server:add_consume_record(RoleID,NeedCoin,1),
	%activity_server:role_consume(coin, RoleID, NeedCoin),
	?notify_update(?ra_coin(Coin2)),
	role_activity:update_rebate_info(1, NeedCoin),
	Role2.


%% @doc 扣消费积分
deduct_score_f(#role{goldUsed=GoldUsed,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedGoldUsed, Type, ArgID, Desc) when GoldUsed >= NeedGoldUsed ->
	GoldUsed2 = GoldUsed -NeedGoldUsed,
	Role2 = RoleInfo#role{goldUsed=GoldUsed2},
	{Date, _} = Time = erlang:localtime(),
	behavior_score_consume:log(RoleID, VipLevel, NeedGoldUsed, GoldUsed, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	?notify_update(?ra_goldUsed(GoldUsed2)),
	Role2.


		
%% @doc 扣声望
deduct_reputation_f(RoleInfo, NeedReputation, _, _, _) when NeedReputation == 0->
    RoleInfo;
deduct_reputation_f(#role{reputation=Reputation,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedReputation, Type, ArgID, Desc) when Reputation>= NeedReputation->
	Reputation2= Reputation-NeedReputation,
	Role2 = RoleInfo#role{reputation=Reputation2},
	{Date, _} = Time = erlang:localtime(),
	behavior_repu_consume:log(RoleID, VipLevel, NeedReputation, Reputation, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	%activity_server:role_consume(repu, RoleID, NeedReputation),
	?notify_update(?ra_reputation(Reputation2)),
	role_activity:update_rebate_info(3,NeedReputation),
	Role2.

%% @doc 扣奥义结晶
deduct_profoundCrystal_f(RoleInfo, NeedProfoundCrystal, _, _, _) when NeedProfoundCrystal == 0->
    RoleInfo;
deduct_profoundCrystal_f(#role{profoundCrystal=ProfoundCrystal,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedProfoundCrystal, Type, ArgID, Desc) when ProfoundCrystal>= NeedProfoundCrystal->
	ProfoundCrystal2= ProfoundCrystal-NeedProfoundCrystal,
	Role2 = RoleInfo#role{profoundCrystal=ProfoundCrystal2},
	{Date, _} = Time = erlang:localtime(),
	behavior_profoundCrystal_consume:log(RoleID, VipLevel, NeedProfoundCrystal, ProfoundCrystal, Date, Time, Type, ArgID, Desc),
	role_data:set_roleInfo(Role2),
	%activity_server:role_consume(repu, RoleID, NeedReputation),
	?notify_update(?ra_profoundCrystal(ProfoundCrystal2)),
	% role_activity:update_rebate_info(3,NeedReputation),
	Role2.

%% @doc 扣荣誉
deduct_honor_f(RoleInfo, NeedHonor, _, _, _) when NeedHonor == 0->
    RoleInfo;
deduct_honor_f(#role{honor=Honor,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedHonor, Type, ArgID, Desc) when Honor>= NeedHonor->
    Honor2= Honor-NeedHonor,
    Role2 = RoleInfo#role{honor=Honor2},
    {Date, _} = Time = erlang:localtime(),
    behavior_honor_consume:log(RoleID, VipLevel, NeedHonor, Honor, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(Role2),
    %activity_server:role_consume(repu, RoleID, NeedReputation),
    ?notify_update(?ra_honor(Honor2)),
    % role_activity:update_rebate_info(3,NeedReputation),
    Role2.

%% @doc 扣竞技场点数
deduct_pvppoint_f(RoleInfo, NeedPvpPoint, _, _, _) when NeedPvpPoint == 0->
    RoleInfo;
deduct_pvppoint_f(#role{pvppoint=PvpPoint,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedPvpPoint, Type, ArgID, Desc) when PvpPoint>= NeedPvpPoint->
    PvpPoint2= PvpPoint-NeedPvpPoint,
    Role2 = RoleInfo#role{pvppoint=PvpPoint2},
    {Date, _} = Time = erlang:localtime(),
    behavior_pvppoint_consume:log(RoleID, VipLevel, NeedPvpPoint, PvpPoint, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(Role2),
    %activity_server:role_consume(repu, RoleID, NeedReputation),
    ?notify_update(?ra_pvppoint(PvpPoint2)),
    % role_activity:update_rebate_info(3,NeedReputation),
    Role2.

%% @doc 扣公会货币
deduct_unioncoin_f(RoleInfo,NeedUnioncoin,_,_,_) when NeedUnioncoin ==0 ->
	RoleInfo;
deduct_unioncoin_f(#role{unioncoin=Unioncoin,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedUnioncoin, Type, ArgID, Desc) when Unioncoin >= NeedUnioncoin ->
    Unioncoin2 = Unioncoin-NeedUnioncoin,
    Role2 = RoleInfo#role{unioncoin=Unioncoin2},
    {Date, _} = Time = erlang:localtime(),
    behavior_unioncoin_consume:log(RoleID, VipLevel, NeedUnioncoin, Unioncoin, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(Role2),
    %activity_server:role_consume(unioncoin, RoleID, NeedCoin),
    ?notify_update(?ra_unioncoin(Unioncoin2)),
    %role_activity:update_rebate_info(99, NeedUnioncoin),
    Role2.

%% @doc 扣竞家园物资
deduct_home_resource_f(RoleInfo, NeedHomeResource, _, _, _) when NeedHomeResource == 0->
    RoleInfo;
deduct_home_resource_f(#role{home_resource=HomeResource,roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedHomeResource, Type, ArgID, Desc) when HomeResource>= NeedHomeResource->
    HomeResource2= HomeResource-NeedHomeResource,
    Role2 = RoleInfo#role{home_resource=HomeResource2},
    {Date, _} = Time = erlang:localtime(),
    behavior_home_resource_consume:log(RoleID, VipLevel, NeedHomeResource, HomeResource, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(Role2),
    %activity_server:role_consume(repu, RoleID, NeedReputation),
    ?notify_update(?ra_home_resource(HomeResource2)),
    % role_activity:update_rebate_info(3,NeedReputation),
    Role2.


%% @doc 扣除飞行币
deduct_laputastone_f(RoleInfo, NeedLaputaStone, _, _, _) when NeedLaputaStone == 0->
    RoleInfo;
deduct_laputastone_f(#role{laputastone=LaputaStone,roleID=RoleID}=RoleInfo, NeedLaputaStone, Type, ArgID, Desc) when LaputaStone>= NeedLaputaStone->
    LaputaStone2= LaputaStone-NeedLaputaStone,
    Role2 = RoleInfo#role{laputastone=LaputaStone2},
    {Date, _} = Time = erlang:localtime(),
    behavior_laputastone_consume:log(RoleID, NeedLaputaStone, LaputaStone, Date, Time, Type, ArgID, Desc),
    role_data:set_roleInfo(Role2),
    ?notify_update(?ra_laputastone(LaputaStone2)),
    Role2.

deduct_ticket_f(RoleInfo, Need, _,_,_) when Need =< 0 -> RoleInfo;
deduct_ticket_f(#role{ticket=Ticket,roleID=RoleID, vipLevel=VipLevel}=RoleInfo,Need, Type,ArgID,Desc)->
	Ticket2 = Ticket - Need,
	Role2 = RoleInfo#role{ticket=Ticket2},
	{Date,_}=Time=erlang:localtime(),
	behavior_ticket_consume:log(RoleID, VipLevel,Need,Ticket,Date,Time,Type,ArgID,Desc),
	role_data:set_roleInfo(Role2),
	%%添加点券消费记录
	consume_back_server:add_consume_record(RoleID,Need,2),
	?notify_update(?ra_ticket(Ticket2)),
	Role2.
	

create_vlog(_Type,[]) ->
	"";
%% create_vlog(Type,{0, 0,  0, 0, 0,0,0,[],[]})->
%% 	ejson:encode({[]})

%兼容一下旧参数的代用，最好是不用这个接口，以为少记录了HomeResource，纯粹是为了防止崩溃而加的
create_vlog(_Type,{AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation,AddProfoundCryStal,AddHonor,PvpPoint,NewItemList,NewGer})->
    create_vlog(_Type,{AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation,AddProfoundCryStal,AddHonor,0,PvpPoint,NewItemList,NewGer});
create_vlog(_Type,{AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation,AddProfoundCryStal,AddHonor,_HomeResource,PvpPoint,NewItemList,NewGer})->
	ML = create_vlog(AddCoin, AddGold,  AddGerExp, AddRoleExp, AddReputation,AddProfoundCryStal,AddHonor,PvpPoint),
	NewGer2 = lists:foldl(fun(#new_ger{gerTypeID=TID},Acc)->
								  case lists:keytake(TID,1, Acc) of
									  false ->
										  [{TID,1}|Acc];
									  {value, {_,Num},Acc2} ->
										  [{TID,Num+1}|Acc2]
								  end
						  end, [], NewGer),
	NewItemList2 = lists:foldl(fun(#new_item{itemTypeID=TID,itemNum=ANum},Acc)->
									   case lists:keytake(TID,1,Acc) of
										   false ->
											   [{TID,ANum}|Acc];
										   {value,{_,Num},Acc2} ->
											   [{TID,Num+ANum}|Acc2]
									   end
							   end, [], NewItemList),
	GL = [[{"typeID",GerTypeID},{"num",Num}]||{GerTypeID,Num}<-NewGer2],%#new_ger{gerTypeID=GerTypeID}<-NewGer2],
	IL = [[{"typeID",ItemTypeID},{"num",Num}]||{ItemTypeID,Num}<-NewItemList2],%#new_item{itemNum=Num,itemTypeID=ItemTypeID}<-NewItemList],
	mochijson2:encode([{"moneyList",[ML]},{"gerList",GL},{"itemList",IL}]).

create_vlog(AddCoin, AddGold,  _, AddRoleExp, AddReputation,AddProfoundCryStal,AddHonor,PvpPoint)->
	[{T,A}
	 ||{T,A}<-[{"coin",AddCoin}, {"gold",AddGold},{"exp",AddRoleExp},{"reputation", AddReputation}
			  ,{"crystal",AddProfoundCryStal},{"honor",AddHonor},{"pvpPoint",PvpPoint}],A/= 0].
	

%% @doc 扣元宝,优先扣goldBonus
deduct_gold_f(RoleInfo,NeedGold,Type,ArgID,Desc) ->
	deduct_gold_f(RoleInfo,NeedGold,Type,ArgID,Desc,[]).
deduct_gold_f(RoleInfo, NeedGold, _, _, _,_) when NeedGold == 0 ->
    RoleInfo;
deduct_gold_f(#role{gold=Gold, goldBonus=GoldBonus, goldUsed=GoldUsed, roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedGold, Type, ArgID, Desc,Vlog) ->
	GoldUsed2 = GoldUsed+NeedGold,
	activity_server:role_consume(gold, RoleID, NeedGold),
	{Date, _} = Time = erlang:localtime(),
	role_activity:update_rebate_info(2,NeedGold),
	if GoldBonus >= NeedGold ->
		    GoldBonus2 = GoldBonus - NeedGold,
		    RoleInfo2 = RoleInfo#role{goldBonus=GoldBonus2,goldUsed=GoldUsed2},
		    ?notify_update(?ra_goldBonus(GoldBonus2)),
		    ?notify_update(?ra_goldUsed(GoldUsed2)),
			behavior_gold_consume:log(RoleID, VipLevel, 0, NeedGold, Gold, GoldBonus, Date, Time, Type, ArgID, Desc,""),
		    role_data:set_roleInfo(RoleInfo2),
		   	?CATCH(role_task_trigger:handle({dispach_task,role_use_gold,NeedGold})),
		   	consume_back_server:add_consume_record(RoleID,NeedGold),
		   	role_consumerank:add_role_consume_num(NeedGold),
			RoleInfo2;
	   GoldBonus + Gold >= NeedGold ->
		    Gold2=Gold+GoldBonus-NeedGold,
		    RoleInfo2=RoleInfo#role{gold=Gold2,goldBonus=0,goldUsed=GoldUsed2},
		    VLog2 = create_vlog(Type,Vlog),
			behavior_gold_consume:log(RoleID, VipLevel, NeedGold-GoldBonus, GoldBonus, Gold, GoldBonus, Date, Time, Type, ArgID, Desc,VLog2),
	 	    role_data:set_roleInfo(RoleInfo2),
	 	    ?notify_update(?ra_gold(Gold2)),
		    ?notify_update(?ra_goldBonus(0)),
		    ?notify_update(?ra_goldUsed(GoldUsed2)),
		   	?CATCH(role_task_trigger:handle({dispach_task,role_use_gold,NeedGold})),
		   	consume_back_server:add_consume_record(RoleID,NeedGold),
		   	role_consumerank:add_role_consume_num(NeedGold),
		    RoleInfo2;
	   true ->
		   exit(wrong_logic)
	end.

%% @doc 扣元宝,优先扣gold
deduct_gold_2_f(RoleInfo,NeedGold,Type,ArgID,Desc)->
	deduct_gold_2_f(RoleInfo,NeedGold,Type,ArgID,Desc,[]).
deduct_gold_2_f(RoleInfo,NeedGold,_,_,_,_) when NeedGold==0->
	RoleInfo;
deduct_gold_2_f(#role{gold=Gold, goldBonus=GoldBonus, goldUsed=GoldUsed, roleID=RoleID, vipLevel=VipLevel}=RoleInfo, NeedGold, Type, ArgID, Desc,Vlog) ->
	GoldUsed2 = GoldUsed+NeedGold,
	activity_server:role_consume(gold, RoleID, NeedGold),
	{Date, _} = Time = erlang:localtime(),
	role_activity:update_rebate_info(2, NeedGold),
	if Gold >= NeedGold ->
		   Gold2 = Gold - NeedGold,
		   RoleInfo2 = RoleInfo#role{gold=Gold2,goldUsed=GoldUsed2},
		   ?notify_update(?ra_gold(Gold2)),
		   ?notify_update(?ra_goldUsed(GoldUsed2)),
		   VLog2 =create_vlog(Type,Vlog),
		   behavior_gold_consume:log(RoleID, VipLevel, NeedGold, 0, Gold, GoldBonus, Date, Time, Type, ArgID, Desc,VLog2),
		   role_data:set_roleInfo(RoleInfo2),
		   	% ?ERR("add consume_back~n"),
		   	consume_back_server:add_consume_record(RoleID,NeedGold),
		   	role_consumerank:add_role_consume_num(NeedGold),
		   ?CATCH(role_task_trigger:handle({dispach_task,role_use_gold,NeedGold})),
		   RoleInfo2;
	   GoldBonus + Gold >= NeedGold ->
		   GoldBonus2=Gold+GoldBonus-NeedGold,
		   RoleInfo2=RoleInfo#role{gold=0,goldBonus=GoldBonus2,goldUsed=GoldUsed2},
		   VLog2 =create_vlog(Type,Vlog),
		   behavior_gold_consume:log(RoleID, VipLevel, Gold, NeedGold-Gold, Gold, GoldBonus, Date, Time, Type, ArgID, Desc,VLog2),
		   role_data:set_roleInfo(RoleInfo2),
		   ?notify_update(?ra_gold(0)),
		   ?notify_update(?ra_goldBonus(GoldBonus2)),
		   ?notify_update(?ra_goldUsed(GoldUsed2)),
		   	consume_back_server:add_consume_record(RoleID,NeedGold),
		   	role_consumerank:add_role_consume_num(NeedGold),
		   ?CATCH(role_task_trigger:handle({dispach_task,role_use_gold,NeedGold})),
		   RoleInfo2;
	   true ->
		   exit(wrong_logic)
	end.


%% @doc 扣体力
deduct_energy_f(RoleInfo,#roleTimes{energy=E}=RoleTimes, NeedE) ->
	deduct_energy_f(RoleInfo,#roleTimes{energy=E}=RoleTimes, NeedE, 0, 0, "").
deduct_energy_f(RoleInfo,#roleTimes{energy=E}=RoleTimes, NeedE, DecType, ArgID, Des) ->
	E2 = E-NeedE,
	RoleTimes2=RoleTimes#roleTimes{energy=E2},
	role_data:set_roleTimes(RoleTimes2),
	#role{familyID = FamilyID} = role_data:get_roleInfo(),
 	IntervalSeconds2 = role_lib:get_current_tick(?currentEnergyIntercal),
	?notify_update(?ra_energy(E2, RoleTimes#roleTimes.lastEnergyTime+IntervalSeconds2)),
	%%记录消耗体力原因码
	#role{roleID = RoleID, vipLevel = VipLevel}=RoleInfo,
	{Date, _} = Time = erlang:localtime(),
	behavior_energy_consume:log(RoleID, VipLevel, NeedE, E, Date, Time, DecType, ArgID, Des),
	
	%% 根据体力消耗增加公会经验
	#role{roleID = RoleId,familyID = FamilyID} = RoleInfo,
	RoleInfo2 = case FamilyID =:= 0 of
		true ->
			RoleInfo;
		false ->
            AddFamilyExp = NeedE*data_family:get(exp_by_energy),
            family_misc:router_to_family_process(FamilyID, {add_family_exp,RoleId,AddFamilyExp}),
            case get(?enery_to_unioncoin) of
                undefined ->
                    put(?enery_to_unioncoin,AddFamilyExp),
                    RoleInfo;
                EneryToUnioncoin when (EneryToUnioncoin+AddFamilyExp) < 10 ->
                    put(?enery_to_unioncoin,EneryToUnioncoin+AddFamilyExp),
                    RoleInfo;
                EneryToUnioncoin ->
                    AddUnioncoin = (EneryToUnioncoin+AddFamilyExp) div 10,
                    SaveEnery = (EneryToUnioncoin+AddFamilyExp) rem 10,
                    put(?enery_to_unioncoin,SaveEnery),
                    add_unioncoin_f(RoleInfo, AddUnioncoin, ?MONEY_ADD_TYPE_ENERGY_FAMILY_UNIONCOIN, 0, "e_u")
            end                    
	end,
	{RoleTimes2,RoleInfo2}.

%% @doc 扣探索次数
deduct_dscv_f(#roleTimes{discoveryTimes=E}=RoleTimes, NeedE, NewCount) ->
	E2 = E-NeedE,
	RoleTimes2=RoleTimes#roleTimes{discoveryTimes=E2,dscvCount=NewCount},
	role_data:set_roleTimes(RoleTimes2),
	DInterval2 = role_lib:get_current_tick(?currentdiscoveryInterval),
	% ?ERR("deduct_dscv_f interval ~w ~n",[DInterval2]), 
	?notify_update(?ra_dscv(E2, RoleTimes#roleTimes.lastDscvTime+DInterval2)),
	RoleTimes2.

%% 扣3v3次数
deduct_teamPkTimes_f(#roleTimes{teamPkTimes=TeamPkTimes} = RoleTimes) ->
    TeamPkTimes2 = TeamPkTimes - 1,
    RoleTimes2 = RoleTimes#roleTimes{teamPkTimes=TeamPkTimes2},
    role_data:set_roleTimes(RoleTimes2),
	DInterval = role_lib:get_current_tick(?currentTeamPkIntercal),
    ?notify_update(?ra_teamPkTimes(TeamPkTimes2, RoleTimes#roleTimes.lastTeamPkTime + DInterval)),
    RoleTimes2.

%% @doc 判断货币是否足够
check_money(Role, gold, Gold) ->
	Role#role.gold + Role#role.goldBonus >= Gold;
check_money(Role, coin, Coin) ->
	Role#role.coin >= Coin;
check_money(Role, reputation, Reputation) ->
	Role#role.reputation >= Reputation;
check_money(Role, score, Score) ->
	Role#role.goldUsed >= Score;
check_money(Role, ticket, Ticket) ->
	Role#role.ticket >= Ticket;
check_money(Role, unioncoin, Unioncoin) ->
    Role#role.unioncoin >= Unioncoin;
check_money(Role,profoundCrystal,ProfoundCrystal)->
	Role#role.profoundCrystal >= ProfoundCrystal;
check_money(Role,honor,Honor)->
    Role#role.honor >= Honor;
check_money(Role,home_resource,HomeRes)->
    Role#role.home_resource >= HomeRes;
check_money(Role,pvppoint,PP)->
    Role#role.pvppoint >= PP;
check_money(Role,eggscore,Score)->
	RoleEggInfo = goldenegg_server:get_role_egginfo(Role#role.roleID),
	RoleEggInfo#role_egg_info.score >= Score;
check_money(Role,laputastone,LaputaStone)->
	Role#role.laputastone >= LaputaStone;
check_money(_,_,_) ->
	false.

%% @doc 扣取配置的货币
deduct_money_f(Role, gold, Gold, Type, ArgID, Desc) ->
	deduct_gold_f(Role, Gold, Type, ArgID, Desc);
deduct_money_f(Role, coin, Coin, Type, ArgID, Desc) ->
	deduct_coin_f(Role, Coin, Type, ArgID, Desc);
deduct_money_f(Role, reputation, Reputation, Type, ArgID, Desc) ->
	deduct_reputation_f(Role, Reputation, Type, ArgID, Desc);
deduct_money_f(Role, score, Score, Type, ArgID, Desc) ->
	deduct_score_f(Role,Score, Type, ArgID, Desc);
deduct_money_f(Role, ticket,Ticket,Type,ArgID,Desc) ->
	deduct_ticket_f(Role, Ticket, Type, ArgID, Desc);
deduct_money_f(Role, unioncoin, Unioncoin, Type, ArgID, Desc) ->
    deduct_unioncoin_f(Role, Unioncoin, Type, ArgID, Desc);
deduct_money_f(Role,profoundCrystal,ProfoundCrystal,Type,ArgID,Desc)->
	deduct_profoundCrystal_f(Role, ProfoundCrystal, Type, ArgID, Desc);
deduct_money_f(Role,honor,Honor,Type,ArgID,Desc)->
    deduct_honor_f(Role, Honor, Type, ArgID, Desc);
deduct_money_f(Role,pvppoint,PvpPoint,Type,ArgID,Desc)->
    deduct_pvppoint_f(Role, PvpPoint, Type, ArgID, Desc);
deduct_money_f(Role,home_resource,HomeResource,Type,ArgID,Desc)->
    deduct_home_resource_f(Role, HomeResource, Type, ArgID, Desc);
deduct_money_f(Role,eggscore,EggScore,Type,ArgID,Desc)->
	role_goldenegg:deduct_eggscore_money(Role, EggScore, Type, ArgID, Desc);
deduct_money_f(Role,laputastone,LaputaStone,Type,ArgID,Desc)->
	deduct_laputastone_f(Role,LaputaStone,Type,ArgID,Desc);
deduct_money_f(_,_,_, _Type, _ArgID, _Desc) ->
	exit("wrong config").

check_role_single_currency_enough(Role,{?ITEM_TYPE_REPUTATION,Value})->
	Role#role.reputation >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_COIN,Value})->
	Role#role.coin >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_GOLD,Value})->
	Role#role.gold+Role#role.goldBonus >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_UNION_COIN,Value})->
	Role#role.unioncoin >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_PROFOUNDCRYSTAL,Value})->
	Role#role.profoundCrystal >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_HONOR,Value})->
	Role#role.honor >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_PVPPOINT,Value})->
	Role#role.pvppoint >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_HOME_RESOURCE,Value})->
	Role#role.home_resource >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_TICKET,Value})->
	Role#role.ticket >= Value;
check_role_single_currency_enough(Role,{?ITEM_TYPE_LAPUTASTONE,Value})->
	Role#role.laputastone >= Value;
check_role_single_currency_enough(_Role,Value)->
	?ERR("undefined Value:~w ~n",[Value]),
	false.

deduct_currency(Role,CurrencyList,Type,ArgID,Desc)->
	lists:foldl(fun({CurrencyType,Value},RoleAcc)-> deduct_single_currency(CurrencyType,Value,RoleAcc,Type,ArgID,Desc) end ,Role,CurrencyList).

deduct_single_currency(?ITEM_TYPE_REPUTATION,Value,Role,Type,ArgID,Desc)->
	deduct_reputation_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_COIN,Value,Role,Type,ArgID,Desc)->
	deduct_coin_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_GOLD,Value,Role,Type,ArgID,Desc)->
	deduct_gold_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_UNION_COIN,Value,Role,Type,ArgID,Desc)->
	deduct_unioncoin_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_PROFOUNDCRYSTAL,Value,Role,Type,ArgID,Desc)->
	deduct_profoundCrystal_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_HONOR,Value,Role,Type,ArgID,Desc)->
	deduct_honor_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_PVPPOINT,Value,Role,Type,ArgID,Desc)->
	deduct_pvppoint_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_HOME_RESOURCE,Value,Role,Type,ArgID,Desc)->
	deduct_home_resource_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_TICKET,Value,Role,Type,ArgID,Desc)->
	deduct_ticket_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(?ITEM_TYPE_LAPUTASTONE,Value,Role,Type,ArgID,Desc)->
	deduct_laputastone_f(Role,Value,Type,ArgID,Desc);
deduct_single_currency(Type,_Value,Role,_Type,_ArgID,_Desc)->
	?ERR("undefined currency type:~w ~n",[Type]),
	Role.

pvp(TarRoleID) ->
	MyFighterList = role_data:get_fighter_list(),
	RoleLieuAdd = role_data:get_lieu_add_attr(),
    TalentListA = role_talent:get_active_talent_list(),
	TrSpecialA = role_data:get_trSpecial(),
	ASkinInfo = role_skin:get_skin_info(),
	LegendAddList = [{GerID,ger_attr:get_ger_legend_add(role_data:get_equip(GerID))}||#ger{gerID=GerID}<-MyFighterList],
	case erlang:whereis(regName(TarRoleID)) of
		?undefined ->
			DSkinInfo = role_data:get_otherRoleSkinInfo(TarRoleID),
			{TarFighterList,TarLieuAdd,TalentListB,TrSpecialB} = role_data:get_otherRoleFighter(TarRoleID),
			TarEquipList = role_data:get_otherRoleItemEquips(TarRoleID),
			%%将数据库中获得的精灵装备列表按照精灵分类
			GerEquipList = role_item:assort_ger_equiplist(TarEquipList),
			TarLegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
			% ?ERR("TarFighterList ~w TarLieuAdd ~w ~n",[TarFighterList,TarLieuAdd]),
			role_fight:new(MyFighterList, TarFighterList, RoleLieuAdd, TarLieuAdd, TalentListA, TalentListB,TrSpecialA,TrSpecialB,ASkinInfo,DSkinInfo,LegendAddList,TarLegendAddList);
		_ ->
			Ref = erlang:make_ref(),
			Info = {pvp_attack, MyFighterList,RoleLieuAdd, TalentListA,TrSpecialA,ASkinInfo,LegendAddList,self(), Ref},
			role_lib:send_server(TarRoleID, Info),
			role_fight:get_result(Ref)
	end.

get_max_alien_times(VipLevel, RoleLevel) ->
    NeedVipLevel = data_alien:get(need_vip_level),
    NeedRoleLevel = data_alien:get(need_role_level),
    case RoleLevel >= NeedRoleLevel andalso VipLevel >= NeedVipLevel of
        true ->
            data_common:get(max_alien_times);
        false ->
            0
    end.

get_max_dscv_times(VipLevel) ->
    case data_vip:get(VipLevel) of
        ?undefined ->
            0;
        #data_vip{explore_Num_Max=Val} ->
            Val
    end.

get_max_energy(VipLevel) ->
    case data_vip:get(VipLevel) of
        ?undefined ->
            0;
        #data_vip{energy_Num_Max=Val} ->
            Val
    end.

get_max_challengeGodFreeTimes(VipLevel)->
	case data_vip:get(VipLevel) of
		?undefined ->
			2;
		#data_vip{challengeGodFreeTimes = Val}->
			Val
	end.

get_max_challengeGodBuyTimes(VipLevel)->
	case data_vip:get(VipLevel) of
		?undefined ->
			0;
		#data_vip{challengeGodTimes=Val} ->
			Val
	end.

get_max_teamPk(_VipLevel) ->
    data_common:get(max_teamPkTimes).

%% @doc 主公升级hook
%% 该hook函数调用时玩家的等级信息等还没有更改过
hook_level_up(Role, NewLevel) ->
	#role{roleID=RoleID, vipLevel=VipLevel, level=OldLevel} = Role,
	?CATCH(role_task_trigger:handle({dispach_task,role_up_level,RoleID,NewLevel})),
	pvp_server:update_level(RoleID, NewLevel),
	role_generalteam:update_level(Role,NewLevel),
	role_crystal:refresh_gercrystal_by_uplevel(Role,NewLevel),
	RoleTimes = role_data:get_roleTimes(),
	%MaxDscv = get_max_dscv_times(VipLevel),
	MaxEnergy = get_max_energy(VipLevel),
	%AddDscv = data_common:get(level_up_add_dscv),
	AddEnergy = data_common:get(level_up_add_energy),
	#roleTimes{discoveryTimes=_Dscv, energy=Energy} = RoleTimes,
	%ExploreOpenLevel = data_common:get(explore_open_level),
	%Dscv2 = Dscv,%if NewLevel > ExploreOpenLevel -> Dscv+AddDscv; true -> erlang:min(MaxDscv, Dscv+AddDscv) end,
	%?notify_update(?ra_dscv(Dscv2, RoleTimes#roleTimes.lastDscvTime+role_lib:get_current_tick(?currentdiscoveryInterval))),
	Energy2 = Energy+AddEnergy,
	IntervalSeconds2 = role_lib:get_current_tick(?currentEnergyIntercal),
	?notify_update(?ra_energy(Energy2, RoleTimes#roleTimes.lastEnergyTime+IntervalSeconds2)),
%% 	if Dscv >= MaxDscv ->
%% 		   Dscv2 = Dscv;
%% 	   true ->
%% 		   Dscv2 = erlang:min(MaxDscv, Dscv+AddDscv),
%% 		   ?notify_update(?ra_dscv(Dscv2, RoleTimes#roleTimes.lastDscvTime+role_lib:get_current_tick(?currentdiscoveryInterval)))
%% 	end,
	tk_id:exlv_update_maxLevel(NewLevel),
%% 	if Energy >= MaxEnergy ->
%% 		   Energy2 = Energy;
%% 	   true ->
%% 		   Energy2 = erlang:min(MaxEnergy, Energy+AddEnergy),
%% %		    IntervalSeconds = role_lib:to_sec(data_common:get(energy_recover_interval)),
%%             IntervalSeconds2 = role_lib:get_current_tick(?currentEnergyIntercal),
%% 		   ?notify_update(?ra_energy(Energy2, RoleTimes#roleTimes.lastEnergyTime+IntervalSeconds2))
%% 		   
%% 	end,

	case NewLevel =:= data_treasure_box:get(level_limit) of
		true ->
			role_lib:send_server(RoleID, {route, role_treaHouse, #cs_treaHouse_get_list{}});
		_ ->
			ignore
	end,
	%RoleTimes2 = RoleTimes#roleTimes{discoveryTimes=Dscv2,energy=Energy2},
    RoleTimes2 = RoleTimes#roleTimes{energy=Energy2},
	role_data:set_roleTimes(RoleTimes2),
	role_homestead:hook_role_levelup(RoleID,OldLevel, NewLevel),
    role_lvlSgAttr:on_role_lvl_up(OldLevel, NewLevel),
	FightPower = role_data:cacl_roleFightPower(),
	Role2 = Role#role{fightPower=FightPower},
    role_magicBook:hook_level_up(NewLevel),
	TrSpecial = role_data:get_trSpecial(),
	role_data:set_trSpecial(TrSpecial#trSpecial{roleLevel=NewLevel}),
	update_rolePublic(Role2#role{level=NewLevel},TrSpecial,role_data:get_plane_use_info(),role_data:get_xbattle_data()),
	%%此函数一定要在更新public之后调用，
	%%触发七日活动任务
	role_payGuide:trigger_task_change(?ROLE_LEVEL_N,{NewLevel}),
	role_home:hook_role_levelup(NewLevel),
	role_buddy:fresh_buddy_slot_num(NewLevel),
	Role2.


get_name(RoleID) ->
	case ets:lookup(?ETS_ROLE_PUBLIC, RoleID) of
		[#rolePublic{}=RolePublic] ->
			RolePublic#rolePublic.roleName;
		[] ->
			case db_sql:get_roleInfo(RoleID) of
				#role{roleName=Name} ->
					Name;
				_ ->
					""
			end
	end.

get_level(RoleID) ->
	case ets:lookup(?ETS_ROLE_PUBLIC, RoleID) of
		[#rolePublic{}=RolePublic] ->
			RolePublic#rolePublic.level;
		[] ->
			case db_sql:get_roleInfo(RoleID) of
				#role{level=Level} ->
					Level;
				_ ->
					0
			end
	end.

get_vip_level(RoleID) ->
    case ets:lookup(?ETS_ROLE_PUBLIC, RoleID) of
        [#rolePublic{}=RolePublic] ->
            RolePublic#rolePublic.viplevel;
        [] ->
            case db_sql:get_roleInfo(RoleID) of
                #role{vipLevel=VipLevel} ->
                    VipLevel;
                _ ->
                    0
            end
    end.


%% @doc 非玩家A逻辑进程，扣取玩家A元宝的接口
%% return: ok|not_exist|not_enough_gold
%% 离线扣玩家钱的思路：如果玩家在线，则通知他的role_server，扣取，
%%				如果玩家不在线，则将这次扣钱操作记录到表dbOfflineDeductGold中，等玩家上线时，处理这次扣取
%% 为什么要这么做？减小复杂度，保证服务器开放时，写玩家相关数据的只有玩家的进程。
%% 如果不这样，则 ：在玩家重复登录的间歇、跨进程扣钱操作，发生在同一时刻时，有几率出现脏数据bug
deduct_gold_d(RoleID, Type, DeductGold) ->
	Msg = {deduct_gold_d, Type, DeductGold},
	case catch role_lib:send_server(RoleID, Msg) of
		{'EXIT',{badarg,_}} ->
			deduct_gold_d2(RoleID, DeductGold);
		Msg ->
			%% 异步扣取
			async
	end.


deduct_gold_d2(RoleID, DeductGold) ->
	F = fun() ->
				%% 读取玩家信息
				case db_sql:get_gold(RoleID) of
					{Gold,GoldBonus} ->
						%% 读取已扣取离线元宝
						AlreadyDeductedGold = db_sql:get_offlineDeductGold(RoleID),
						%% 判断元宝是否足够
						if Gold + GoldBonus >= DeductGold+AlreadyDeductedGold ->
							   db_sql:set_offlineDeductGold(RoleID, AlreadyDeductedGold+DeductGold);
						   true ->
							   {false, not_enough_gold}
						end;
					_ ->
						{false, not_exist}
				end
		end,
	case F() of
		{ok,_} ->
			ok;
		{false,not_exist} ->
			not_exist;
		_ ->
			not_enough_gold
	end.

%% do_pay_monthVIP(AppItemID,Receipt,Md5,SrcType) ->
%% 	MonthVIPPayIDList = data_monthVIP:get(monthVIPPayIDList),
%% 	case lists:member(AppItemID, MonthVIPPayIDList) of
%% 		false ->
%% 			do_pay(AppItemID,Receipt, Md5,SrcType);
%% 		true ->
%% 			#data_pay{payBonus=PayBonus,payGold=PayGold} = data_pay:get(AppItemID),
%% 			#role{accid=Accid,goldMonthVIPTotalPaid=GoldTotalPaid,deviceID=DeviceID} = Role = role_data:get_roleInfo(),
%% 			behavior_gold_pay_add:log(Role#role.roleID, 0, PayGold, 0, 0, 0, ?MONEY_ADD_TYPE_MONTHVIP_PAY ,AppItemID,Md5,Accid,DeviceID,SrcType),
%% 			role_data:set_roleInfo(Role#role{goldMonthVIPTotalPaid=GoldTotalPaid+PayGold}),
%% 			#monthVIP_info{restBigDays=RestBigDays,restLittleDays=RestLittleDays} = 
%% 							  MonthVipInfo = role_data:get_role_monthVIP_info(),
%% 			Role2 = role_data:get_roleInfo(),
%% 			Now = util:now(),
%% 			case data_monthVIP:get({monthVIPPayIDType,AppItemID}) of
%% 				1 ->
%% 					BigTime = data_monthVIP:get(bigTime),
%% 					MonthVipInfo2 = MonthVipInfo#monthVIP_info{buyBigSec=Now,restBigDays=RestBigDays+BigTime},
%% 					role_data:set_role_monthVIP_info(MonthVipInfo2),
%% 					Reward = data_monthVIP:get(bigReward),
%% 					?sendself(#sc_monthVip_success{type=1,days=RestBigDays+BigTime, reward=activity_server:sell_reward2p_reward_info(Reward)}),
%% 					role_reward:handle_sell_reward_f(Role2, Reward, ?MONEY_ADD_TYPE_MONTHVIP_BUY, 1, integer_to_list(AppItemID));
%% 				2 ->
%% 					LittleTime = data_monthVIP:get(littleTime),
%% 					MonthVipInfo2 = MonthVipInfo#monthVIP_info{buyLittltSec=Now,restLittleDays=RestLittleDays+LittleTime},
%% 					role_data:set_role_monthVIP_info(MonthVipInfo2),
%% 					Reward = data_monthVIP:get(littleReward),
%% 					?sendself(#sc_monthVip_success{type=2,days=RestLittleDays+LittleTime, reward=activity_server:sell_reward2p_reward_info(Reward)}),
%% 					role_reward:handle_sell_reward_f(Role2, Reward,?MONEY_ADD_TYPE_MONTHVIP_BUY, 2, integer_to_list(AppItemID))
%% 			end,
%% 			erlang:send(pay_server,{mark_pay_info, Role,Role2,Receipt,AppItemID,SrcType,PayGold,role_data:get_ip(),role_data:get_macAddr()})
%% 	end.
	
do_pay(AppItemID, Receipt, Md5, SrcType) ->
	#role{payExtReward=PayExtReward,extRdActTime=LastActSec,level=Level} = Role0 = role_data:get_roleInfo(),
    #role_xbattle{chapterID=NChapterID}=  role_data:get_xbattle_data(),
	#data_pay{payBonus=PayBonus,payGold=PayGold} = data_pay:get(AppItemID),
    case catch check_pay_ext_reward(AppItemID, PayExtReward,LastActSec) of
        {ok, GiveExtReward, PayExtReward2,NowActSec} ->
            role_reward:handle_sell_reward_f(Role0#role{payExtReward=PayExtReward2,extRdActTime=NowActSec}, GiveExtReward, ?MONEY_ADD_TYPE_PAY_EXT_REWARD, 0, ""),
            HaveExt = true;
        _ ->
			PayExtReward2 = undefined,
            HaveExt = false
    end,
    #role{accid=Accid,gold=Gold,goldBonus=GoldBonus,goldTotalPaid=GoldTotalPaid,vipLevel=CurVipLevel
		 ,deviceID=DeviceID,svipLevel=SvipLevel,sGoldTotalPaid=SGoldTotalPaid} = RoleBase = role_data:get_roleInfo(),
%% 	if GoldTotalPaid == 0 ->
%% 		   %% 首充加倍，且有礼包
%% 		   BonusRate = data_pay_reward:get(first_pay_gold_rate),
%%            case HaveExt of
%%                false ->
%%                    PayBonus2 = erlang:max(0,(PayBonus+PayGold)*BonusRate- PayGold);
%%                true ->
%%                    PayBonus2 = 0
%%            end,
%% 		   Gold2 = Gold+PayGold,
%% 		   GoldBonus2 = GoldBonus+PayBonus2,
%% 		   GoldTotalPaid2 = GoldTotalPaid+PayGold,
%% 		   IsFirstPaid = true;
%% 	   true ->
    case HaveExt of
        false ->
            PayBonus2 = PayBonus,
            GoldBonus2 = GoldBonus+PayBonus;
        true ->
            PayBonus2 = 0,
            GoldBonus2 = GoldBonus
    end,
	if GoldTotalPaid == 0 -> IsFirstPaid = true;true ->  IsFirstPaid = false end,
    Gold2 = Gold+PayGold,
    GoldTotalPaid2 = GoldTotalPaid+PayGold,
%% 	end,
	%Ticket2 = Ticket + PayGold,
    #role{roleID=RoleID,roleName=RoleName} = Role =  do_first_pay(RoleBase,PayGold),
	VipLevel2 = cacl_vipLevel(CurVipLevel,GoldTotalPaid2),
    SVipStart = data_common:get(svip_need_vip),
    SGoldAdd = if CurVipLevel < SVipStart andalso VipLevel2 == SVipStart -> 
                      #data_vip{needPayGold=SvipStartNeedGold} = data_vip:get(SVipStart), 
                      GoldTotalPaid2 - SvipStartNeedGold;
                  true -> PayGold
               end,
    SGoldTotalPaid2=if VipLevel2 >= SVipStart ->SGoldTotalPaid+SGoldAdd; true->SGoldTotalPaid end,
    SVipLevel2 = cacl_svipLevel(SvipLevel,SGoldTotalPaid2),
    trigger_vip_level_task(CurVipLevel, VipLevel2),
    notice_vip_change(RoleID,VipLevel2,CurVipLevel,SVipLevel2,SvipLevel),
    Role2 = Role#role{goldBonus=GoldBonus2,gold=Gold2,goldTotalPaid=GoldTotalPaid2,vipLevel=VipLevel2,sGoldTotalPaid=SGoldTotalPaid2,svipLevel=SVipLevel2},
    role_data:set_roleInfo(Role2),
	behavior_gold_pay_add:log(Role#role.roleID, CurVipLevel, PayGold, Gold, PayBonus2, GoldBonus, ?MONEY_ADD_TYPE_NORMAL_PAY ,AppItemID,Md5,Accid,DeviceID,SrcType,NChapterID,Level),
	if VipLevel2 =/= CurVipLevel->
		   RoleTimes=role_data:get_roleTimes(),
		   VipChallengeGodFreeTimes = get_max_challengeGodFreeTimes(VipLevel2),
		   VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%get_max_challengeGodBuyTimes(VipLevel2),
		   NewChallengeGodEnergy = VipChallengeGodFreeTimes - (get_max_challengeGodFreeTimes(CurVipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
		   RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
											challengeGodBuyTimes=VipChallengeGodBuyTimes
											},
		   role_data:set_roleTimes(RoleTimes2),
           MaxDscv = role_lib:get_max_dscv_times(VipLevel2),
           MaxEnergy = role_lib:get_max_energy(VipLevel2),
           ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
		   ?notify_update(?ra_vipLevel(VipLevel2, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
		   role_lib:insert_rolePublic(Role2,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data());
	   true ->
		   ignore
	end,
    ?sendself(#sc_role_update_svip{svipLevel=SVipLevel2,sGoldPaidNow=SGoldTotalPaid2}),
	?notify_update(?ra_gold(Gold2)),
	?notify_update(?ra_goldBonus(GoldBonus2)),
	%?notify_update(?ra_ticket(Ticket2)),
	if IsFirstPaid ->
		   %FirstPayReward = data_pay_reward:get(first_pay_reward),
		   %role_reward:handle_sell_reward_f(Role2, FirstPayReward, ?MONEY_ADD_TYPE_FIRST_PAY, 0, ""),
		   %% 邀请者获奖励
		   #limitInfo{inviteRoleID=Inviter} = role_data:get_limitInfo(),
		   if is_integer(Inviter),Inviter>0 ->
				  invite_server:first_pay(RoleID, RoleName, Inviter, PayGold);
			  true ->
				  ignore
		   end;
	   true ->
		   ignore
	end,
	case SrcType of
        0 ->
           ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
	   	1 ->
		   ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
	   	90 ->
		   ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
		2 ->	   
		   ?sendself(#sc_role_pay_91{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
		3 ->
		   ?sendself(#sc_role_pay_uc{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		4 ->
		   ?sendself(#sc_role_pay_dl{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		5 ->
		   ?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		6 ->
		   ?sendself(#sc_role_pay_360{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		7 ->
		   ?sendself(#sc_role_pay_wdj{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		8 ->
		   ?sendself(#sc_role_pay_dk{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		9 ->
		   ?sendself(#sc_role_pay_mi{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		10 ->
		   ?sendself(#sc_role_pay_az{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
		_ ->
			case is_integer(SrcType) of
				true ->
					?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
				_ ->
					ignore
			end
	end,
	case PayExtReward2 of
		undefined->
			ignore;
		_->
			?sendself(#sc_role_update_pay_ext{pay_ext=PayExtReward2}),
			#role{profoundCrystal=ProfoundCrystal} = role_data:get_roleInfo(),
			?notify_update(?ra_profoundCrystal(ProfoundCrystal))
	end,
	?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,PayGold,?REWARD_GOLD})),
	erlang:send(pay_server,{mark_pay_info, Role0,Role2,Receipt,AppItemID,SrcType,PayGold,role_data:get_ip(),role_data:get_macAddr()}),
		catch role_task_trigger:handle({dispach_task,role_pay_gold,PayGold}),
	role_discount:add_pay_info(PayGold),
	role_monthVIP:update_pay_info(PayGold),
	role_payGuide:pay_gold(PayGold),
	role_treasurebowl:add_pay_info(PayGold),
	role_consumerank:add_role_pay_num(PayGold).

do_first_pay(Role=#role{firstPayStatus=FirstPayStatus},PayGold) ->
	case data_first_pay:get(FirstPayStatus) of
		?undefined ->
			Role;
		#data_first_pay{payType=Type,need=Need,nextID=NextID,reward=Reward} ->
			case Type of
				1 ->
					role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_FIRST_PAY, 0, ""),
					role_activity:get_now_firstPayStatus(NextID),
					Role2 = role_data:get_roleInfo(),
					?sendself(#sc_activity_firstPay_update{newID=NextID,reward=activity_server:sell_reward2p_reward_info(Reward)}),
					Role2#role{firstPayStatus=NextID};
				2 ->
					if Need == PayGold ->
						   role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_FIRST_PAY, 0, ""),
						   role_activity:get_now_firstPayStatus(NextID),
						   Role2 = role_data:get_roleInfo(),
						   ?sendself(#sc_activity_firstPay_update{newID=NextID,reward=activity_server:sell_reward2p_reward_info(Reward)}),
						   Role2#role{firstPayStatus=NextID};
					   true ->
						   Role
					end;
				_ ->
					Role
			end
	end.

do_pay_amount(PayGold, Receipt, Md5, SrcType) ->
    #role{accid=Accid,gold=Gold,goldBonus=GoldBonus,goldTotalPaid=GoldTotalPaid,vipLevel=CurVipLevel,level=Level
		 ,deviceID=DeviceID,svipLevel=SvipLevel, sGoldTotalPaid=SGoldTotalPaid} = RoleBase = role_data:get_roleInfo(),
    #role_xbattle{chapterID=NChapterID}=  role_data:get_xbattle_data(),
    %% 儲值獎勵值
    PayBonus = 0,
    
%%     if GoldTotalPaid == 0 ->
%%            %% 首充加倍，且有禮包
%%            BonusRate = data_pay_reward:get(first_pay_gold_rate),
%%            PayBonus3 = erlang:max(0,(PayBonus+PayGold)*BonusRate- PayGold),
%%            PayBonus2 = erlang:min(6500,PayBonus3),
%%            Gold2 = Gold+PayGold,
%%            GoldBonus2 = GoldBonus+PayBonus2,
%%            GoldTotalPaid2 = GoldTotalPaid+PayGold,
%%            IsFirstPaid = true;
%%        true ->
    PayBonus2=PayBonus,
    if GoldTotalPaid == 0 -> IsFirstPaid = true;true ->  IsFirstPaid = false end,
    Gold2 = Gold+PayGold,
    GoldBonus2 = GoldBonus+PayBonus,
    GoldTotalPaid2 = GoldTotalPaid+PayGold,
%%     end,
    #role{roleID=RoleID,roleName=RoleName} =Role =  do_first_pay(RoleBase,PayGold), 
    VipLevel2 = cacl_vipLevel(CurVipLevel,GoldTotalPaid2),
    SVipStart = data_common:get(svip_need_vip),
    SGoldAdd = if CurVipLevel < SVipStart andalso VipLevel2 == SVipStart -> 
                      #data_vip{needPayGold=SvipStartNeedGold} = data_vip:get(SVipStart), 
                      GoldTotalPaid2 - SvipStartNeedGold;
                  true -> PayGold
               end,
    SGoldTotalPaid2=if VipLevel2 >= SVipStart ->SGoldTotalPaid+SGoldAdd; true->SGoldTotalPaid end,
    SVipLevel2=cacl_svipLevel(SvipLevel,SGoldTotalPaid2),
    trigger_vip_level_task(CurVipLevel, VipLevel2),
    notice_vip_change(RoleID,VipLevel2,CurVipLevel,SVipLevel2,SvipLevel),
	%Ticket2 = Ticket + PayGold,
    Role2 = Role#role{goldBonus=GoldBonus2,gold=Gold2,goldTotalPaid=GoldTotalPaid2,vipLevel=VipLevel2,svipLevel=SVipLevel2,sGoldTotalPaid=SGoldTotalPaid2},
    role_data:set_roleInfo(Role2),
    behavior_gold_pay_add:log(Role#role.roleID, CurVipLevel, PayGold, Gold, PayBonus2, GoldBonus, ?MONEY_ADD_TYPE_NORMAL_PAY ,0,Md5,Accid,DeviceID,SrcType,NChapterID,Level),
    if VipLevel2 =/= CurVipLevel->
           RoleTimes=role_data:get_roleTimes(),
           VipChallengeGodFreeTimes = get_max_challengeGodFreeTimes(VipLevel2),
           VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%get_max_challengeGodBuyTimes(VipLevel2),
           NewChallengeGodEnergy = VipChallengeGodFreeTimes - (get_max_challengeGodFreeTimes(CurVipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
           RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
                                            challengeGodBuyTimes=VipChallengeGodBuyTimes
                                            },
           role_data:set_roleTimes(RoleTimes2),
           MaxDscv = role_lib:get_max_dscv_times(VipLevel2),
           MaxEnergy = role_lib:get_max_energy(VipLevel2),
           ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
           ?notify_update(?ra_vipLevel(VipLevel2, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
           role_lib:insert_rolePublic(Role2,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data());
       true ->
           ignore
    end,
    ?sendself(#sc_role_update_svip{svipLevel=SVipLevel2,sGoldPaidNow=SGoldTotalPaid2}),
    ?notify_update(?ra_gold(Gold2)),
    ?notify_update(?ra_goldBonus(GoldBonus2)),
	%?notify_update(?ra_ticket(Ticket2)),
    if IsFirstPaid ->
           %FirstPayReward = data_pay_reward:get(first_pay_reward),
           %role_reward:handle_sell_reward_f(Role2, FirstPayReward, ?MONEY_ADD_TYPE_FIRST_PAY, 0, ""),
           %% 邀請者獲獎勵
           #limitInfo{inviteRoleID=Inviter} = role_data:get_limitInfo(),
           if is_integer(Inviter),Inviter>0 ->
                  invite_server:first_pay(RoleID, RoleName, Inviter, PayGold);
              true ->
                  ignore
           end;
       true ->
           ignore
    end,
    case SrcType of
        0 ->
           ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
        1 ->
           ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
        90 ->
           ?sendself(#sc_role_pay_ios{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
        2 ->       
           ?sendself(#sc_role_pay_91{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,receipt=Receipt,result=1});
        3 ->
           ?sendself(#sc_role_pay_uc{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        4 ->
           ?sendself(#sc_role_pay_dl{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        5 ->
           ?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        6 ->
           ?sendself(#sc_role_pay_360{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        7 ->
           ?sendself(#sc_role_pay_wdj{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        8 ->
           ?sendself(#sc_role_pay_dk{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        9 ->
           ?sendself(#sc_role_pay_mi{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        10 ->
           ?sendself(#sc_role_pay_az{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
        _ ->
            case is_integer(SrcType) of
                true ->
                    ?sendself(#sc_role_pay_zz{isGetFirstChargeReward=IsFirstPaid,newGoldTotalPaid=GoldTotalPaid2,result=1});
                _ ->
                    ignore
            end
    end,
    ?CATCH(role_task_trigger:handle({dispach_task,role_add_reward,PayGold,?REWARD_GOLD})),
	erlang:send(pay_server,{mark_pay_info, Role,Role2,Receipt,PayGold,SrcType,PayGold,role_data:get_ip(),role_data:get_macAddr()}),
			catch role_task_trigger:handle({dispach_task,role_pay_gold,PayGold}),
	role_discount:add_pay_info(PayGold),
	role_monthVIP:update_pay_info(PayGold),	
	role_payGuide:pay_gold(PayGold),
	role_treasurebowl:add_pay_info(PayGold),
	role_consumerank:add_role_pay_num(PayGold).

check_pay_ext_reward(ID, PayExtRewardT,LastActSec) ->
	NowActSec = 
		case data_pay_reward:get(activity_start) of
			?undefined ->
				0;
			DateTime ->
				Sec2 = util:datetime_to_seconds(DateTime),
				case Sec2 >= util:now() of
					true ->
						0;
					_ ->
						Sec2
				end
		end,
	PayExtReward = update_payExtReward_value(PayExtRewardT, LastActSec,NowActSec),
    case data_pay_reward:get({pay_ext_reward, ID}) of
        ?undefined ->
            false;
        Reward when erlang:is_record(Reward, sell_reward) ->
            BitList = to_bit_list(PayExtReward),
%%             ?ERR("BitList:~w", [BitList]),
            ID2 = ID rem 1000,
            case lists:nth(ID2, BitList) of
                0 ->
                    {ok, Reward, gen_new_pay_ext_reward(ID2, PayExtReward),NowActSec};
                1 ->
                    false
            end;
        _ ->
            false
    end.

update_payExtReward_value(PayExtRewardT, LastActSec,NowActSec)->
	case NowActSec =< LastActSec of
		true ->
			PayExtRewardT;
		_ ->
			update_pay_ext_reward(PayExtRewardT, LastActSec)
	end.
		
update_pay_ext_reward(Val,TimeSec) ->
	NowActTime
		= case data_pay_reward:get(activity_start) of
			  ?undefined ->
				  0;
			  DateTime ->
				  Sec2 = util:datetime_to_seconds(DateTime),
				  case Sec2 >= util:now() of
					  true ->
						  0;
					  _ ->
						  Sec2
				  end 
		  end,
	Val2 = 
		if TimeSec < NowActTime ->
			   0;
		   true->
			   Val
		end,
	lists:foldr(fun(ID, AccVal) ->
						case data_pay_reward:get({pay_ext_reward, 11000 + ID}) of
							?undefined ->
								gen_new_pay_ext_reward(ID, AccVal);
							_ ->
								AccVal
						end
				end, Val2, lists:seq(1, 8)).

gen_new_pay_ext_reward(ID, Val) ->
    case ID of
        1 ->
            Val bor 2#00000001;
        2 ->
            Val bor 2#00000010;
        3 ->
            Val bor 2#00000100;
        4 ->
            Val bor 2#00001000;
        5 ->
            Val bor 2#00010000;
        6 ->
            Val bor 2#00100000;
        7 ->
            Val bor 2#01000000;
        8 ->
            Val bor 2#10000000
     end.

to_bit_list(Val) when erlang:is_integer(Val) ->
    to_bin_list2(<<Val:32>>, []).

to_bin_list2(<<>>, BitList) ->
%%     ?ERR("BitList:~w", [BitList]),
    BitList;
to_bin_list2(<<Byte:8, Left/binary>>, BitList) ->
    <<Bit8:1, Bit7:1, Bit6:1, Bit5:1, Bit4:1, Bit3:1, Bit2:1, Bit1:1>> = <<Byte:8>>,
    to_bin_list2(Left, [Bit1,Bit2,Bit3,Bit4,Bit5,Bit6,Bit7,Bit8|BitList]).
	
cacl_vipLevel(CurVipLevel, Gold) ->
	NextLevel = CurVipLevel+1,
	case data_vip:get(NextLevel) of
		?undefined ->
			CurVipLevel;
		#data_vip{needPayGold=Need} ->
			if Gold < Need ->
				   CurVipLevel;
			   Gold == Need ->
				   NextLevel;
			   true ->
				   cacl_vipLevel(NextLevel, Gold)
			end
	end.
cacl_svipLevel(CurVipLevel, Gold) ->
	NextLevel = CurVipLevel+1,
	case data_svip:get(NextLevel + 100) of
		?undefined ->
			CurVipLevel;
		#data_vip{needPayGold=Need} ->
			if Gold < Need ->
				   CurVipLevel;
			   Gold == Need ->
				   NextLevel;
			   true ->
				   cacl_svipLevel(NextLevel, Gold)
			end
	end.

role2rolePublic(#role{roleName=RoleName,roleID=RoleID,level=Level,lastLogoutTime=LastLogoutTime
					 ,isMale=IsMale,title=Title,fightPower=FightPower,goldTotalPaid=GoldTotalPaid
					 ,head=Head,location=Location,vipLevel=VipLevel,familyID=FamilyID,plane_level=PlaneLevel
                     ,transmigration=Transmigration,svipLevel=SvipLevel}
			   ,#trSpecial{specialID=SpecialID},#plane_use_info{use=PUse},#role_xbattle{chapterID=ChapterID}) ->
	PlaneLevel2 = case PUse of 0 -> PlaneLevel; _ -> PUse end,
    ChapterID2 = case ChapterID of 0 -> data_common:get(xbattle_first_chapter); _ -> ChapterID end,
	#rolePublic{roleName=RoleName,
				roleID=RoleID,
				level=Level,
				lastLogoutTime=LastLogoutTime,
				isMale=IsMale,
				title=Title,
				fightPower=FightPower,
				goldTotalPaid=GoldTotalPaid,
				head=Head,
				location=Location,
				viplevel=VipLevel,
				familyID=FamilyID,
				plane_level=PlaneLevel2,
				trSpecial=SpecialID,
                transmigration=Transmigration,
                svipLevel=SvipLevel,
                xbattleChapterID=ChapterID2
				}.

get_rolePublic(RoleID) ->
	case ets:lookup(?ETS_ROLE_PUBLIC, RoleID) of
		[] ->
			case db_sql:get_roleInfo(RoleID) of
				#role{}=Role ->
					%#trSpecial{specialID=_SpecialID} = 
					TrSpecial = db_sql:get_trSpecial_info(RoleID),
					PlaneUseInfo = db_sql:get_carlos_plane_info(RoleID),
                    Xbattle = db_sql:get_xbattle_data(RoleID),
                    insert_rolePublic(Role,TrSpecial,PlaneUseInfo,Xbattle),
                    role2rolePublic(Role,TrSpecial,PlaneUseInfo,Xbattle);                    
%% 					RolePublic = role2rolePublic(Role),
%% 					ets:insert(?ETS_ROLE_PUBLIC, RolePublic),
%% 					RolePublic;
				_ ->
					[]
			end;
		[RolePublic] ->
			RolePublic
	end.

insert_rolePublic(RoleInfo,TrSpecial,PlaneUseInfo,Xbattle) ->
    TimeLine = util:now() - 1296000, %离线时间小于15天
    if
        RoleInfo#role.lastLogoutTime > TimeLine ->
            ets:insert(?ETS_ROLE_LEVEL, {RoleInfo#role.roleID,RoleInfo#role.level});
        true ->
            ignore
    end,
	ets:insert(?ETS_ROLE_PUBLIC, role2rolePublic(RoleInfo,TrSpecial,PlaneUseInfo,Xbattle)).

update_rolePublic(Role,TrSpecial,PlaneUseInfo,Xbattle) ->
	case ets:member(?ETS_ROLE_PUBLIC, Role#role.roleID) of
		true ->
            TimeLine = util:now() - 1296000, %离线时间小于15天
            if
                Role#role.lastLogoutTime > TimeLine ->
                    ets:insert(?ETS_ROLE_LEVEL, {Role#role.roleID,Role#role.level});
                true ->
                    ignore
            end,
			ets:insert(?ETS_ROLE_PUBLIC, role2rolePublic(Role,TrSpecial,PlaneUseInfo,Xbattle));
		false ->
			ignore
	end,
	#role{roleID=RoleID,fightPower=FightPower} = Role,
	catch pvp_server:update_fightPower(RoleID,FightPower).
    %catch race_server:update_role_info(Role).


is_more_main_than(A,B) ->
	#ger{gerBase=#gerBase{gerLevel=LA,gerQuality=QA,gerTypeID=TA}} =A,
	#ger{gerBase=#gerBase{gerLevel=LB,gerQuality=QB,gerTypeID=TB}} =B,
	{LA,QA,TB} > {LB,QB,TA}.
	   
cacl_mainGerTypeID() ->
	MainGerTypeIDList = data_fixed_encounter:get(?mainGerTypeID),
	PosList = role_data:get_posList(),
	List = [Ger||#ger{gerBase=#gerBase{gerTypeID=GerTypeID}}=Ger<-PosList, lists:member(GerTypeID, MainGerTypeIDList)],
	if List == [] ->
		   0;
	   true ->
		   Ger = util:foldl(fun(E,Acc) ->
									case is_more_main_than(E, Acc) of
										true ->
											E;
										false ->
											Acc
									end
							end, hd(List), tl(List)),
		   Ger#ger.gerBase#gerBase.gerTypeID
	end.
%%计算公会科技对产出结果的影响系数
%%Pos是配置中定义的对应影响系数的位置
%%AddType是影响的类型，1为产出等影响，2为武将属性的影响
%%为了减少对ets表中不存在数据而进行数据库查询, 增加3和4类型，分别返回1,2两个类型对应的整个科技加成系数


calculate_familyTek_addbuff(FamilyID,Pos,AddType) ->
	case FamilyID of
		0 ->
			0;
		?undefined ->
			0;
		_ ->
			FamilyTekInfo = ets:lookup(?ETS_FAMILY_TECHNOLOGY,FamilyID),
			case FamilyTekInfo of
				[] ->
					FamilyTekDetailList = db_sql:get_family_tek(FamilyID),
					FamilyTekInfoList = familyTekDetailList2familyTekInfoList(FamilyTekDetailList),
					ets:insert(?ETS_FAMILY_TECHNOLOGY,{FamilyID,FamilyTekInfoList}),
					calculate_familyTek_addbuff2(FamilyTekInfoList,Pos,AddType);
				[{_,FamilyTekInfoList}] ->
					calculate_familyTek_addbuff2(FamilyTekInfoList,Pos,AddType)
			end
	end.									
calculate_familyTek_addbuff2(FamilyTekInfoList,Pos,AddType) ->
	if 
		AddType==1 orelse AddType==2 ->		
			{Acc,_} = lists:foldl(fun(FamilyTekInfo,{Acc0,AddType0})->
						{TekID,_TekLevel} = FamilyTekInfo,
						TekInfo = data_family_technology:get({data_technology_rank, TekID}),
						{_Type,_UnlockLevel,_Level2,_Cost2,Add_buff,Add_attr} = TekInfo,
						case AddType0 of
							1 ->
								Addcoefficient = element(Pos+1,Add_buff),
								%?ERR("Addcoefficient :~w ~n",[Addcoefficient]),
								{Acc0+Addcoefficient,AddType0};
							2 ->
								Addcoefficient = element(Pos+1,Add_attr),
								{Acc0+Addcoefficient,AddType0}

						end
					end,{0,AddType},FamilyTekInfoList),
			Acc;
		AddType==3 ->
			[calculate_familyTek_addbuff2(FamilyTekInfoList,N,1)||N<-lists:seq(1,11)];
		AddType==4 ->
			[calculate_familyTek_addbuff2(FamilyTekInfoList,N,2)||N<-lists:seq(1,18)];
		true ->
			0
	end.
familyTekDetailList2familyTekInfoList(FamilyTekDetailList) ->
	[{TekID,TekLevel}||#p_familyTekDtl{tekID=TekID,tekLevel=TekLevel} <- FamilyTekDetailList].

getFamilyTekAddAttr() ->
	Role = role_data:get_roleInfo(),
		case Role of
			?undefined ->
				FamilyID = 0;
		 	_->
				#role{familyID = FamilyID} = role_data:get_roleInfo()
		end,
FamilyTekAddList = role_lib:calculate_familyTek_addbuff(FamilyID,0,4),
	case FamilyTekAddList of 
		0->
			#add_attr{};
		_ ->
			#add_attr{
						 gerAttack =lists:nth(1,FamilyTekAddList)
						,gerHpMax =lists:nth(2,FamilyTekAddList)
						,gerSpInit =lists:nth(3,FamilyTekAddList)
						,gerSpMax =lists:nth(4,FamilyTekAddList)
						,gerCritic =lists:nth(5,FamilyTekAddList)
						,gerCriticReduce =lists:nth(6,FamilyTekAddList)
						,gerDoom =lists:nth(7,FamilyTekAddList)
						,gerMiss =lists:nth(8,FamilyTekAddList)
						,gerAbsorb =lists:nth(9,FamilyTekAddList)
						,gerDamageBack =lists:nth(10,FamilyTekAddList)
						,gerReel =lists:nth(11,FamilyTekAddList)
						,gerReelReduce =lists:nth(12,FamilyTekAddList)
						,gerPhyDefBite=lists:nth(13,FamilyTekAddList)
						,gerPhyDef=lists:nth(14,FamilyTekAddList)
						,gerMagDefBite=lists:nth(15,FamilyTekAddList)
						,gerMagDef=lists:nth(16,FamilyTekAddList)
						,gerAttackAddtion =lists:nth(17,FamilyTekAddList)
						,gerHpMaxAddtion =lists:nth(18,FamilyTekAddList)

			}
	end.
%根据捐献类型以及捐献数量计算公会货币返回的数量
%当捐献类型为1,2,3（金币，钻石，徽章）时，ItemTypeID无效
calculate_unioncoin_back(DonateType,ItemTypeID,DonateNum)->
	Result = calculate_unioncoin_back1(DonateType,ItemTypeID,DonateNum),
	case Result of
		?undefined ->
			0;
		_ ->
			case is_number(Result) of
				true ->
					Result;
				false ->
					0
			end
	end.

calculate_unioncoin_back1(DonateType,ItemTypeID,DonateNum)->
	case DonateType of
		1 ->
			data_family:get({donate_coin_back,DonateNum});
		2 ->
			data_family:get({donate_gold_back,DonateNum});
		3 ->
			data_family:get({donate_reputation_back,DonateNum});
		4 ->
			#data_item{itemStar=Star} = data_item:get(ItemTypeID),
			data_family:get({donate_equip_back,Star})*DonateNum;
		5 ->
			#data_ger{gerStar=GerStar} = data_ger:get(ItemTypeID),
			data_family:get({donate_ger_back,GerStar})*DonateNum;
		_ -> 
			?INFO("calculate_unioncoin_back DonateType ~w ItemTypeID ~w DonateNum ~w ~n",[DonateType,ItemTypeID,DonateNum])
	end.

%%计算公会科技攻击、血量加成万分比

calculate_familyTekHpAndAtkBuff(OldAtk,OldHp) ->
	RoleInfo = role_data:get_roleInfo(),
	#role{familyID = FamilyID} = RoleInfo,
	FamilyTek_AtkAdd = role_lib:calculate_familyTek_addbuff(FamilyID,17,2),
	FamilyTek_HpAdd = role_lib:calculate_familyTek_addbuff(FamilyID,18,2),
	NewAtk = OldAtk + FamilyTek_AtkAdd,
	NewHp  = OldHp + FamilyTek_HpAdd,
	{NewAtk,NewHp}.

%%将当前玩家的gold转换成goldbonus 并且将对应的渠道类型转换成新的类型
transform_gold2goldbonus(OldType,NewType,Type,AccountID,LogTimeStamp)->
	#role{gold=OldGold,goldBonus=OldGoldBonus,srcType=_OldSrcType,roleID=RoleID,vipLevel=VipLevel}=RoleInfo=role_data:get_roleInfo(),
	NewGold=0,
	NewGoldBonus=OldGold+OldGoldBonus,
	{Date, _} = Time = case LogTimeStamp of 0->erlang:localtime();_->util:seconds_to_datetime(LogTimeStamp) end,
	case OldGold =:= 0 of
		false->
			%%添加gold消耗日志
			behavior_gold_consume:log(RoleID, VipLevel, OldGold, 0, NewGold, NewGoldBonus, Date, Time, Type, 0, "",[]),
			%%添加gold_bonus添加日志
			behavior_gold_bonus_add:log(RoleID, VipLevel, OldGold, NewGoldBonus, Date, Time, Type, 0, "");
		true->
			ignore
	end,
	NewRoleInfo = 
	case Type of
		?MONEY_DEC_TYPE_SRCTYPE_TRANSFORM->
			RoleInfo#role{gold=NewGold,goldBonus=NewGoldBonus,srcType=NewType};
		?MONEY_DEC_TYPE_SPECIAL->
			RoleInfo#role{gold=NewGold,goldBonus=NewGoldBonus}
	end,			
	role_data:set_roleInfo(NewRoleInfo),
	db_sql:add_gold2goldbonus_log(RoleID,OldGold,OldGoldBonus,OldType,NewGold,NewGoldBonus,NewType,Type,AccountID,db_sql:datetime(Time)).

%%返回当前阶段各种次数恢复时间轮使用的tick
get_current_tick(Type) ->
	case Type of
		?currentEnergyIntercal ->
			get(?currentEnergyIntercal);
		?currentdiscoveryInterval ->
			get(?currentdiscoveryInterval);
		?currentPlunderInterval ->
			get(?currentPlunderInterval);
		?currentPvpInterval ->
			get(?currentPvpInterval);
        ?currentTeamPkIntercal ->
            get(?currentTeamPkIntercal)
	end.

calculate_familyTekeffectTime(OldValue,TekAddValue) ->
	case TekAddValue==0 of
			true ->
				OldValue;
			false ->
				trunc(OldValue/(1+TekAddValue/data_family_technology:get(tek_add_rate)))+1
	end.

calculate_familyTekeffectGenerate(OldValue,TekAddValue) ->
	case TekAddValue==0 of
		true ->
			OldValue;
		false ->
			trunc(OldValue*(1+TekAddValue/data_family_technology:get(tek_add_rate)))+1
	end.

calculate_force_value()->
    #role{level = RoleLevel} = role_data:get_roleInfo(),
    Fighters = role_data:get_posList(),
    Talent = role_talent:get_active_talent_list(),
    {GerEquipList, _BagEquip, _BagItem} = role_data:get_all_item(),
    ItemList = [ [ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,ItemGerID,item_lib:itemDecay(ItemDecay),ItemExp,ItemEnchantType,ItemEnchantLevel,ItemLegendRank]
                || {ItemGerID, ItemList} <- GerEquipList,
                    #item{itemUID=ItemUID,itemTypeID=ItemTypeID,itemDecay=ItemDecay,itemLevel=ItemLevel,itemPos=ItemPos,itemRank=ItemRank,itemExp = ItemExp,itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel,itemLegendRank=ItemLegendRank} <- ItemList],
    conquerisland_war_server:init_player_fightpower_for_boss(#base_player{roleLevel=RoleLevel,fighters=Fighters,talent=Talent,itemList=ItemList}).

%% vip等级成就触发
trigger_vip_level_task(OldLv, NewLv) ->
    case NewLv > OldLv of
        true ->
            trigger_vip_level_task2(OldLv, NewLv);
        _ ->
            ignore
    end.

notice_vip_change(RoleID,Vip,OldVip,Svip,OldSvip) ->
    if Vip /= OldVip orelse Svip /= OldSvip ->
           VipNew = cacl_vip_info(Vip,Svip),
            catch erlang:send(pvp_server, {update_vip, RoleID,VipNew}),
           catch erlang:send(team_pk_server, {update_role_vip, RoleID, VipNew});
       true -> ignore
    end.

trigger_vip_level_task2(NewLv, NewLv) ->
    ignore;
trigger_vip_level_task2(OldLv, NewLv) ->
	?CATCH(role_task_trigger:handle({dispach_task, role_vip_level, NewLv})),
    trigger_vip_level_task2(OldLv + 1, NewLv).

cacl_vip_info(Vip,0) -> Vip;
cacl_vip_info(_,Svip) -> 16#80 bor Svip.

cacl_vip_level2(Vip,0) -> Vip;
cacl_vip_level2(_,Svip) -> 100+Svip.
