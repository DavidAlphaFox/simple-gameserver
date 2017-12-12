%% @author caohongyang
%% @doc 玩家数据操作接口
%% Created 2013-2-26


-module(role_data).
-compile(export_all).
-include("def_role.hrl").
-include("def_task.hrl").
-include("def_item.hrl").
-include("def_carlos.hrl").
%% API functions
-export([
		 get_ger/1
		,get_roleInfo/0
		,set_roleInfo/1
		,get_roleID/0
		,set_roleID/1
		,calc_patch/3
        ,mark_role_lvl_up_refresh/0
		]).


%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

get_task_list_group()->
	{get_task_list(?TASK_TYPE_MAIN),get_task_list(?TASK_TYPE_TODAY),get_task_list(?TASK_TYPE_ACH),get_task_list(?TASK_TYPE_FAMILY_TODAY),get_task_list(?TASK_TYPE_FAMILY_WORSHIP)}.
set_task_list_group({MList,TList,AList,FMYTList,FList})->
	set_task_list(?TASK_TYPE_MAIN, MList),
	set_task_list(?TASK_TYPE_TODAY, TList),
	set_task_list(?TASK_TYPE_ACH, AList),
    set_task_list(?TASK_TYPE_FAMILY_WORSHIP, FList),
    set_task_list(?TASK_TYPE_FAMILY_TODAY, FMYTList).
get_task_list()->
	lists:append([get_task_list(TaskType)||TaskType<-[?TASK_TYPE_MAIN,?TASK_TYPE_TODAY,?TASK_TYPE_ACH,
                                                      ?TASK_TYPE_FAMILY_TODAY,?TASK_TYPE_FAMILY_WORSHIP]]).

get_task_list(TaskType)->
	case erlang:get({?ROLE_TASK_LIST,TaskType}) of
		?undefined->
			[];
		L when is_list(L)->
			L;
		_->
			[]
	end.
set_task_list(TaskType,RoleTaskList) when is_list(RoleTaskList)->
	erlang:put({?ROLE_TASK_LIST,TaskType}, RoleTaskList).

get_ach_curr_taskID(Type)->
	get({?TASK_CURR_ACH_TASKID,Type}).
set_ach_curr_taskID(Type,TaskID)->
	put({?TASK_CURR_ACH_TASKID,Type},TaskID).
set_ach_task_next(TaskID,NextTaskID)->
	put({?TASK_ACH_NEXT_TASKID,TaskID},NextTaskID).
get_ach_task_next(TaskID)->
	get({?TASK_ACH_NEXT_TASKID,TaskID}).

%% 获取物理地址
get_macAddr() ->
	case erlang:get(?macAddr) of
		?undefined ->
			"";
		Addr ->
			Addr
	end.

set_macAddr(Addr) ->
	erlang:put(?macAddr, Addr).

%% 获取ip
get_ip() ->
	case erlang:get(?ip) of
		?undefined ->
			"";
		Addr ->
			Addr
	end.

set_ip(Addr) ->
	erlang:put(?ip, Addr).

init_gag_list(RoleID)->
	GagList = db_sql:get_role_gag_list(RoleID),
	set_gag_list(GagList).

get_gag_list() ->
	case erlang:get(?gag_list) of
		X when is_list(X)->
			lists:foldr(fun(E,AccList) when erlang:is_list(E)->
                                [{gag_info,E,[1446446873]}|AccList]; %由于过去没有记录拉黑的时间点，以2015-11-12 14:48:00填充
                           (E,AccList) when erlang:is_record(E, gag_info) ->
                                [E|AccList]
                        end, [], X);
		_ ->
			[]
	end.

set_gag_list(GagList)->
	erlang:put(?gag_list, GagList).

init_sign_emperor_info(RoleID)->
	Info = db_sql:get_sign_emperor_info(RoleID),
	put(?sign_emperor_info, Info).

set_sign_emperor_info(Info)->
	put(?sign_emperor_info, Info).

get_sign_emperor_info()->
	case erlang:get(?sign_emperor_info) of
		X when is_tuple(X) ->
			X;
		_ ->
			{{1989,7,17},0,0,0}
	end.

%% 设置需要间隔持久化标识
set_interval_persist_flag() ->
	put(?interval_persist_flag, true).

clear_interval_persist_flag() ->
	erase(?interval_persist_flag).

%% 设置任务刷新标识
set_refresh_task_flag() ->
    put(refresh_task_flag, true).

clear_refresh_task_flag( ) ->
    put(refresh_task_flag, false).

get_refresh_task_flag( ) ->
    case get(refresh_task_flag) of
        true ->
            true;
        _ ->
            false
    end.

get_posTypeList() ->
	case erlang:get(?posTypeList) of
		?undefined ->
			PosList = get_posList(),
			PosTypeList = [GerTypeID
						  ||
						   #ger{gerBase=#gerBase{gerTypeID=GerTypeID}}<-PosList
						  ],
			put(?posTypeList, PosTypeList),
			PosTypeList;
		PosTypeList ->
			PosTypeList
	end.
									  
get_posList() ->
	case erlang:get(?posList) of
		PosList when is_list(PosList) ->
			PosList;
		_ ->
			[]
	end.

init_posList(PosList) ->
	PosList2 = [ger_attr:recacl(Ger, lists:delete(Ger, PosList))||Ger<-PosList],
	set_posList(PosList2).

%% 主将只保留未计算副将加成时的数据.副将的影响在每次取副将数据的时候,再进行计算。
set_posList(PosList) ->
	erlang:put(?posList, PosList),
	ger_attr:recacl_gers(PosList),
	clear_mainGerTypeID().

set_posList2(PosList) ->
	erlang:put(?posList, PosList),
	clear_mainGerTypeID().

init_lieuList(LPosList)->
	#role{level=RoleLevel} = role_data:get_roleInfo(),
	init_lieuList(LPosList,RoleLevel).
%%在玩家登录的时候将多余的小伙伴放入到背包中
init_lieuList(LPosList,RoleLevel)->
	BuddySlotNum = role_buddy:get_buddy_slot_num(RoleLevel),
	Fun = fun(#gerSimple{gerTypeID=GerTypeID}=Ger,{CountAcc,LieuAcc,RemainAcc})->
		#data_ger{gerProperty=GerProperty} = data_ger:get(GerTypeID),
		case lists:keytake(GerProperty,1,CountAcc) of
			false->
				{[{GerProperty,1}|CountAcc],[Ger|LieuAcc],RemainAcc};
			{_,{GerProperty,OldNum},Other}->
				case OldNum >= BuddySlotNum of
					false->
						{[{GerProperty,OldNum+1}|Other],[Ger|LieuAcc],RemainAcc};
					true->
						{CountAcc,LieuAcc,[Ger|RemainAcc]}
				end
		end
	end,
	{_CountList,LPosList3,RemainLPosList} = lists:foldl(Fun,{[],[],[]},LPosList),
	PosList2 = [ger_attr:recacl_lieu(Ger, lists:delete(Ger, LPosList3))||Ger<-LPosList3],
	%%多余的小伙伴放入背包中
	GerBag = role_data:get_gerBag(),
	NewGerBag = GerBag++[G#gerSimple{gerPos=0}||G<-RemainLPosList],
	role_data:set_gerBag(NewGerBag),
	set_lieuposList(PosList2).

set_lieu_add_attr(X)->
	erlang:put(?lieu_add_attr, X).

%% {atkAdd, hpAdd}
% get_lieu_add_attr()->
	% case erlang:get(?lieu_add_attr) of
	% 	X  when is_tuple(X) ->
	% 		X;
	% 	_ ->
	% 		{0,0}
	% end.

%%将小伙伴的旧加成和新的特殊属性加成合并成add_attr的加成
get_lieu_add_attr()->
	case role_buddy:get_buddy_buff_for_total() of
		?undefined->
			#add_attr{};
		#camp_add{totalnormaladd={AttackAdd,HpAdd},
			totalspecialadd=#add_attr{gerAttackAddtion=SpecialAttackAdd,gerHpMaxAddtion=SpecialHpAdd}=SpecialAdd}->
			SpecialAdd#add_attr{gerAttackAddtion=SpecialAttackAdd+AttackAdd,gerHpMaxAddtion=SpecialHpAdd+HpAdd}
	end.
% 保存副将,然后重算主将并推送信息到客户端
set_lieuposList(PosList)->
	erlang:put(?lieuPosList, PosList),
	ger_attr:refresh_lieu_add_attr(),
	ger_attr:recacl_gers().

get_lieuposList()->
	case erlang:get(?lieuPosList) of
		PosList when is_list(PosList) ->
			PosList;
		_ ->
			[]
	end.

%% 玩家攻击,血量,战斗力保存与计算方法
%% gFighterList与posList保存未添加副将加成时的属性,posListT保存的添加了副将加成的属性
%% 战斗力计算与cs请求属性列表时都是使用的posListT,战斗计算使用的是posList,在战斗中重新获取副将加成,并加该加成与主将的登场技等叠加计算战斗时的武将属性
%% 新的战斗力值更新到role信息即可,玩家进程每分钟会自动更新rolePublic的数据
set_posListT(PosList)->
	erlang:put(?posListT, PosList),
	FightPower = cacl_roleFightPower(),
	Role = get_roleInfo(),
	Role2=Role#role{fightPower=FightPower},
	OldFightPower = get(role_old_fightPower),
	if
		OldFightPower=:=?undefined->
			put(role_old_fightPower,FightPower),
			?CATCH_1(role_task:send_dispach(self(),{dispach_task,role_fight_power,FightPower}),E1),
			%%玩家初始化登录时，会在此处触发任务，但是此时headSeven未初始化完成，故没有触发成功
			?LOOSE_CATCH(role_payGuide:trigger_task_change(?FIGHTPOWER,{FightPower}));
		FightPower>OldFightPower->
			put(role_old_fightPower,FightPower),
			?CATCH_1(role_task:send_dispach(self(),{dispach_task,role_fight_power,FightPower}),E2),
			?LOOSE_CATCH(role_payGuide:trigger_task_change(?FIGHTPOWER,{FightPower}));
		true->
			?LOOSE_CATCH(role_payGuide:trigger_task_change(?FIGHTPOWER,{FightPower}))
	end,
	role_generalteam:update_roleinfo(Role2),
	set_roleInfo(Role2).

get_posListT() ->
	case erlang:get(?posListT) of
		X when is_list(X) ->
			X;
		_ ->
			[]
	end.


init_LieutenantInfo(RoleLieutenant)->
	case RoleLieutenant of
%% 		[] ->
%% 			#role{level=RoleLevel}=role_data:get_roleInfo(),
%% 			RoleLieutenant1 = 
%% 				lists:foldl(fun(Pos,Acc)->
%% 									#data_lieu_open_charge{pos=Pos,needLevel=Level,initList=InitInfo}=data_lieu_open_charge:get(Pos),
%% 									case Level < RoleLevel of
%% 										true ->
%% 											%#data_lieu_open_charge{initInfo=InitInfo}=data_open_lieu_open_charge:get(Pos),
%% 											[role_ger:get_init_lieutenant(Pos, InitInfo)|Acc];
%% 										_ ->
%% 											Acc
%% 									end
%% 							end, [], data_lieu_open_charge:get_list()),
%% 			erlang:put(?lieutenantInfo, RoleLieutenant1);
		X when is_list(X) ->
			erlang:put(?lieutenantInfo,X);
		_ ->
			init_LieutenantInfo([])
	end.

%% 保存小伙伴（副将）的位置
get_lieutenantInfo()->
	case erlang:get(?lieutenantInfo) of
		?undefined->
			init_LieutenantInfo([]);
		X ->
			X
	end.

set_lieutenantInfo(X) when is_list(X)->
	erlang:put(?lieutenantInfo,X);
set_lieutenantInfo(X) ->
	?ERR("the lieutenantInfo is wrong:~w",[X]),
	ok.

get_lieuInfoList()->
	% LieuInfo = get_lieutenantInfo(),
	LieuList = get_lieuposList(),
	lists:foldl(fun(#ger{gerBase=Base}, Acc)->
% 						case lists:keyfind(Base#gerBase.gerPos, #t_lieu.pos, LieuInfo) of
% 							#t_lieu{infoID1=_ID1} ->
% %% 								if Base#gerBase.gerTypeID =:= ID1 ->
% 									   [#p_lieu_view{lieuGerTypeID=Base#gerBase.gerTypeID,lieuGerQuality=Base#gerBase.gerQuality,lieuPos=Base#gerBase.gerPos,lieuLevel=Base#gerBase.gerLevel}|Acc];  %% 此处返回gerTypeID与三国不同。默认副将必然激活天命
% %% 								   true ->
% %% 									   Acc
% %% 								end;
% %% 								[#p_lieu_view{lieuPos=Base#gerBase.gerPos, lieuGerTypeID=Base#gerBase.gerTypeID, lieuInfoID1=ID1, lieuInfoID2=ID2, lieuInfoID3=ID3
% %% 											 , lieuFightPower=Attr#gerAttr.gerFightPower}|Acc];
% 							_ ->
% 								Acc
% 						end
					%%v410版本以后，小伙伴已经没有了pos的概念，pos位置只是用来反映对应的营地系
					[#p_lieu_view{lieuGerTypeID=Base#gerBase.gerTypeID,lieuGerQuality=Base#gerBase.gerQuality,lieuPos=Base#gerBase.gerPos,lieuLevel=Base#gerBase.gerLevel}|Acc]  %% 此处返回gerTypeID与三国不同。默认副将必然激活天命
				end, [], LieuList).

get_roleInfo() ->
	erlang:get(?roleInfo).

get_roleFightPower() ->
	cacl_roleFightPower().

cacl_roleFightPower() ->
	GerFightPower = 
	lists:foldl(fun(Ger, Acc) ->
						Acc + ?a(Ger, gerFightPower)
				end, 0, get_posListT()),
	case get_trSpecial() of
		#trSpecial{specialID=SpecialID} when SpecialID > 0 ->
			#role{level=Level} = get_roleInfo(),
			TrFightPower = trunc(GerFightPower * (1 + Level/510)) - GerFightPower,
			?sendself(#sc_trSpecial_fightPower{fightPower=TrFightPower}),
			GerFightPower + TrFightPower;
		_ ->
			?sendself(#sc_trSpecial_fightPower{fightPower=0}),
			GerFightPower
end.

clear_roleFightPower() ->
	erlang:erase(?roleFightPower).

set_roleInfo(RoleInfo) ->
	erlang:put(?roleInfo, RoleInfo),
	set_interval_persist_flag(),
    case get_role_lvl_up_refresh() of
        true ->
            clear_role_lvl_up_refresh(),
            ger_attr:recacl_gers();
        _ ->
            ignore
    end.

get_roleID() ->
	erlang:get(?roleID).

set_roleID(RoleID) ->
	erlang:put(?roleID, RoleID).

set_gatewayClientPid(Pid) ->
	erlang:put(?gw, Pid).

set_socket(Socket) ->
	erlang:put(?socket,Socket).

get_gatewayClientPid() ->
	erlang:get(?gw).

get_pvp_rank(RoleID) ->
	case get(?pvp_rank) of
		?undefined ->
			Rank = pvp_server:call_get_rank(RoleID),
			put(?pvp_rank, Rank),
			Rank;
		Rank ->
			Rank
	end.

set_pvp_rank(Rank) ->
	erlang:put(?pvp_rank, Rank),
	role_payGuide:trigger_task_change(?PVP_RANK_N,{Rank}).

init_montVIP_info(RoleID,LastLogoutTime)->
	MonthVIPInfo = db_sql:get_role_monthVIP_Info(RoleID,LastLogoutTime),
	set_role_monthVIP_info(MonthVIPInfo).

get_role_monthVIP_info()->
	case erlang:get(?roleMonthVIP) of
		?undefined ->
			#monthVIP_info{};
		Info ->
			Info
	end.

set_role_monthVIP_info(MonthVIPInfo) when is_record( MonthVIPInfo,monthVIP_info) ->
	put(?roleMonthVIP, MonthVIPInfo).

%% ?talent_list保存的数据时list，元素结构为{天赋类型，天赋等级，天赋冷却时间}，天赋类型和天赋等级加和等于天赋id，用于查询配置
get_trainer_info() ->
    case get(?talent_list) of
        ?undefined ->
            TalentList0 = db_sql:get_trainer_info(get_roleID()),
            TalentList = [{A,B,C}||{A,B,C}<-TalentList0,(A rem 100) =:= 0],
            put(?talent_list, TalentList),
            TalentList;
        TalentList ->
            TalentList
    end.
set_trainer_info(TalentList) ->
    erlang:put(?talent_list, TalentList).

%% @doc 获取武将背包
get_gerBag() ->
	case erlang:get(?gerBag) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

%% @doc 保存武将背包
set_gerBag(List) ->
	%?ERR("gerbag:~w",[List]),
	erlang:put(?gerBag,List),
    ?CATCH(role_task_trigger:handle({dispach_task, role_family_collect_ger})).



%% @doc 获取武将信息
get_ger(GerID) ->
	PosList = get_posList(),  %% 获取出战列表
	GerBag = get_gerBag(),  %% 保存武将背包
	LPosList = get_lieuposList(),  %% 副将武将列表,对应皮卡丘中的小伙伴
	role_ger:take_ger(GerID, PosList, LPosList, GerBag).
%% 	role_ger:take_ger(GerID, PosList, GerBag).


	
clear_mainGerTypeID() ->
	erlang:erase(?mainGerTypeID).

get_mainGerTypeID() ->
	case erlang:get(?mainGerTypeID) of
		?undefined ->
			MainType = role_lib:cacl_mainGerTypeID(),
			put(?mainGerTypeID, MainType),
			MainType;
		MainType ->
			MainType
	end.

init_exBoss_data(RoleID,RoleLevel,LastLogoutTime)->
    {Today,_} = util:seconds_to_datetime(LastLogoutTime),
    Data =case erlang:date() of
              Today ->
                  db_sql:get_exBoss_data(RoleID,RoleLevel);
              _ ->
                  role_exBoss:gen_exBoss_data(RoleLevel)
          end,
	set_exBoss_data(Data),
	role_exBoss:update_exBoss_times(Data).

set_exBoss_data(Data)->
	put(?exBoss_data, Data).

get_exBoss_data()-> 
	case get(?exBoss_data) of
		#role_exBoss{}=D -> D;
		_ -> role_exBoss:gen_exBoss_data()
    end.

set_dojangrank_data(Data)->
    put(?dojangrank_data, Data).

get_dojangrank_data()-> 
    case get(?dojangrank_data) of
        ?undefined -> 
            Data = db_sql:get_role_dojangrank(role_data:get_roleID()),
            put(?dojangrank_data,Data),
            get_dojangrank_data();  %% 为了检查时间
        Data ->
            RoleDay = Data#role_dojangrank.day,
            case erlang:date() of
                RoleDay ->
                    Data;
                NewDay ->
                    NewData1 = 
                        Data#role_dojangrank{day = NewDay
                                            ,free_time = data_dojangrank:get(init_time)
                                            ,buy_time = 0
                                            ,world_buy_time = lists:duplicate(8, 0)
                                            ,world_free_time = lists:duplicate(8,data_dojangrank:get(world_init_time))},
                    {_,RoleM,_} = RoleDay,
                    {_,NewM,_} = NewDay,
                    NewData2 = 
                        if
                            RoleM /= NewM ->
                                NewData1#role_dojangrank{world_fight_rec_list = [[],[],[],[],[],[],[],[]] 
                                                        ,world_enemy_list = [[],[],[],[],[],[],[],[]] 
                                                        ,world_refresh_time = [0,0,0,0,0,0,0,0]};
                            true ->
                                NewData1
                        end,
                    put(?dojangrank_data,NewData2),
                    NewData2
            end
    end.

set_dojang_data(Data)->
    put(?dojang_info, Data).

get_dojang_data()-> 
    case get(?dojang_info) of
        ?undefined-> 
            set_dojang_data(db_sql:get_dojang_data(role_data:get_roleID())),
            get_dojang_data();
        DojangInfo ->
            NowDate = erlang:date(),
            if
                NowDate /= DojangInfo#dojang_info.date ->
                    NewDojangInfo = 
                        DojangInfo#dojang_info{date = NowDate
                            ,harvest_free_time = data_dojang:get(init_time)
                            ,buy_time = erlang:length(data_dojang:get(buy_cost))},
                    set_dojang_data(NewDojangInfo),
                    NewDojangInfo;
                true ->
                    DojangInfo
            end
    end.

set_xbattle_data(Data)->
    put(?xbattle_data,Data).
get_xbattle_data()->
    case get(?xbattle_data) of
        #role_xbattle{}=X ->X;
        _ -> #role_xbattle{}
    end.
init_xbattle_data(LastLogOutTime,RoleID)->
    #role_xbattle{chapterID=NowChapterID}=Data0 = db_sql:get_xbattle_data(RoleID),
    {Date,_} = util:seconds_to_datetime(LastLogOutTime),
    Data2 =  case erlang:date() of 
                 Date ->role_xbattle:cacl_offline_reward(Data0#role_xbattle{rewardChapterID=NowChapterID});
                 _ -> role_xbattle:cacl_offline_reward(Data0#role_xbattle{buyQuickToday=0,raidTimes=0
                                                                         ,rewardChapterID=NowChapterID})
             end,
    role_xbattle:add_tri_plan(Data2),
    role_xbattle:set_ger_display(db_sql:get_xbattle_ger_display(RoleID)),
    set_xbattle_data(Data2).

init_lucky_roll_info(RoleID,LastLogoutTime) ->
	LuckyRoll = db_sql:get_lucky_roll(RoleID,LastLogoutTime),
	ActivityID = role_luckyRoll:get_activityID(),
	case LuckyRoll#lucky_roll.activityID of
		ActivityID ->
			set_lucky_roll(LuckyRoll);
		_ ->
			set_lucky_roll(#lucky_roll{
									  })
	end.

set_lucky_roll(LuckyRoll) ->
	put(?lucky_roll,LuckyRoll).

get_lucky_roll() ->
	case get(?lucky_roll) of
		?undefined ->
			#lucky_roll{};
		X ->
			X
	end.

get_vip_shop() ->
	case get(?vip_shop) of
		?undefined ->
			#vip_shop{};
		X ->
			X
	end.

set_vip_shop(VipShop) ->
	put(?vip_shop,VipShop).

init_vip_shop(RoleID) ->
	VipShop = db_sql:get_vip_shop(RoleID),
	set_vip_shop(VipShop).
 
get_dSignData()->
    case get(?dSignData) of
        #daySign{}=X -> X;
        _-> #daySign{}
    end.

set_dSignData(Data)->
    put(?dSignData, Data).

init_dSignData(RoleID) ->
    Data = db_sql:get_dSignData(RoleID),
    Data2 = role_dSign:init_dSignData(Data),
    set_dSignData(Data2).

init_bounty_info(RoleID) ->
	BountyInfo = db_sql:get_bounty_info(RoleID),
	set_bountyInfo(BountyInfo).

get_bountyInfo()->
	case get(?bountyInfo) of
		?undefined ->
			#bounty_info{};
		X ->
			case lists:all(fun(E)-> erlang:is_record(E, p_bounty_data) end, X#bounty_info.bountyData) of
                false -> #bounty_info{};
                true -> 
                    #bounty_info{time_limit=TimeLimit,last_login=LastLogin} = X,
                    Now = util:now(),
                    NowDate = calendar:date_to_gregorian_days(erlang:date()),
                    NewX = 
                        if Now >= TimeLimit -> role_bounty:init_bounty_info(X);
                           NowDate /= LastLogin ->
                               DaysDiff = NowDate - LastLogin,
                               AddRestTimes = DaysDiff *  data_bounty:get(freeTimes),

                               NewBountyData = 
                                lists:map(fun(P)->
                                                  
                                            NewRestTimes = P#p_bounty_data.restTimes + AddRestTimes,
                                            P#p_bounty_data{restTimes = NewRestTimes}
                                          end, X#bounty_info.bountyData),
                               X#bounty_info{bountyData = NewBountyData
                                            ,last_login = NowDate};
                           true -> X
                        end,
                    set_bountyInfo(NewX),
                    NewX
            end     
	end.

set_bountyInfo(Info)->
	put(?bountyInfo, Info).




get_bounty_fighters(ChapterID)->
    get({?bounty_fighters,ChapterID}).

set_bounty_fighters(Info,ChapterID)->
    put({?bounty_fighters,ChapterID}, Info).





%% 汉帝宝库信息
init_treaHouseInfo(RoleID)->
    #treaHouseInfo{mark=Mark,baseBoxGetList=BaseBoxGetList}=TreaHouseInfo = db_sql:get_treasure_house_info(RoleID),
    ActivityID = activityRank_server:get_treaHouse_activityID(),
    BoxList = data_treasure_box:get(base_reward),
    GetList = 
        case data_treasure_box:get(is_baseReward_send_by_mail) of
            "1"->
                lists:foldl(fun(Pos,Acc)->
                                    case lists:keyfind(Pos, #p_baseBoxOpenInfo.pos, Acc) of
                                        false ->
                                            {_, Reward} = lists:keyfind(Pos, 1, BoxList),
                                            mail_server:send_notemp_mail_with_reward(RoleID, 2, integer_to_list(Pos), Reward),
                                            [#p_baseBoxOpenInfo{pos=Pos,isOpen=2}|Acc];
                                        _ ->
                                            Acc
                                    end
                            end, BaseBoxGetList, lists:seq(1,data_treasure_box_baseReward:get(Mark)));
            _ ->
                BaseBoxGetList
        end,
    TreaHouseInfo2 = 
        case TreaHouseInfo#treaHouseInfo.activityID == ActivityID of
            true ->
                TreaHouseInfo#treaHouseInfo{baseBoxGetList=GetList};
            _ ->
                FreeTimes = data_treasure_box:get(treasure_house_free_times),
                #treaHouseInfo{value_info=1, card_list=role_treaHouse:random_treaHouse_list(),free_count=0,buy_count=0, free_times=FreeTimes,
                               mark=0, baseBoxGetList=[],isGetRankReward=0,activityID=0}
        end,
    set_treaHouseInfo(TreaHouseInfo2).

get_treaHouseInfo()->
	case erlang:get(?treahouseInfo) of
		?undefined ->
			db_sql:get_treasure_house_info(get_roleID());
		#treaHouseInfo{}=X ->
			X
	end.

set_treaHouseInfo(Info) when erlang:is_record(Info, treaHouseInfo) ->
	erlang:put(?treahouseInfo, Info).
 

%% @doc 拼装玩家的roleExtra
get_roleExtra(RoleID) ->
	BatProg = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_NORMAL),
	BatProgHard = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_HARD),
	BatProgFastHard = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_FAST_HARD),
    BattleProgressTransmigration = role_battle:get_progress(?BATTLE_DUNGEON_TYPE_TRANSMIGRATION),
	RoleTimes=get_roleTimes(),
	EncounterList = get_encounterList(),
	ShopNumList = get_shopNumList(),
    ItemUseList = get_itemUseList(),
	DailyInfo = get_dailyInfo(),
	CardInfo = get_cardInfo(),
	HronInfo = get_hronInfo(),
	LimitInfo = get_limitInfo(),
    SignSec = get_signSec(),
	RandomShopList = role_shop:transform_to_proto(get_randomShopList()),	
	#roleExtra{roleID=RoleID
			  ,battleProgress=BatProg
			  ,battleProgressHard=BatProgHard
			  ,battleProgressFastHard=BatProgFastHard
              ,battleProgressTransmigration=BattleProgressTransmigration
			  ,roleTimes=RoleTimes
			  ,encounterList=EncounterList
			  ,shopNumList=ShopNumList
			  ,dailyInfo=DailyInfo
			  ,cardInfo=CardInfo
			  ,hronInfo=HronInfo
			  ,limitInfo=LimitInfo
			  ,randomShopList=RandomShopList
              ,itemUseList=ItemUseList
              ,signSec = SignSec}.

%% @doc 设置玩家额外的各功能属性
set_roleExtra(RoleExtra, RoleInfo, AlienInfo) when is_record(RoleExtra, roleExtra) ->
	role_battle:set_progress(?BATTLE_DUNGEON_TYPE_NORMAL,RoleExtra#roleExtra.battleProgress),
	role_battle:set_progress(?BATTLE_DUNGEON_TYPE_HARD,RoleExtra#roleExtra.battleProgressHard),
	role_battle:set_progress(?BATTLE_DUNGEON_TYPE_FAST_HARD,RoleExtra#roleExtra.battleProgressFastHard),
    ?INFO("set_roleExtra ~w",[RoleExtra#roleExtra.battleProgressTransmigration]),
    role_battle:set_progress(?BATTLE_DUNGEON_TYPE_TRANSMIGRATION,RoleExtra#roleExtra.battleProgressTransmigration),
	role_lib:init_roleTimes_recover_timer(RoleExtra#roleExtra.roleTimes, RoleInfo, AlienInfo),
	DiscardShopIDList = init_encounterList(RoleExtra#roleExtra.encounterList),
	init_shopNumList(RoleExtra#roleExtra.shopNumList,DiscardShopIDList),
	init_dailyInfo(RoleExtra#roleExtra.dailyInfo),
	set_cardInfo(RoleExtra#roleExtra.cardInfo),
	set_hronInfo(RoleExtra#roleExtra.hronInfo),
	set_limitInfo(RoleExtra#roleExtra.limitInfo),
	init_gatherInfo(RoleExtra#roleExtra.gatherList),
    init_itemUseList(RoleExtra#roleExtra.itemUseList),
	%init_free_box_info(RoleInfo#role.roleID),
    RoleID = RoleInfo#role.roleID,
	init_vip_shop(RoleID),
    init_dSignData(RoleID),
	init_bounty_info(RoleID),
	init_tvcard(RoleID),
	init_trSpecial(RoleID),
	init_plane_use_info(RoleID),
	init_shopBoxData(RoleID),
	init_payGuide(RoleID),
	init_activityFestival(RoleID),
    init_tag_data(RoleID),
    init_trainerProf_battle_data(RoleID),
    set_signSec(RoleExtra#roleExtra.signSec),
	role_shop:init_randomShopList(RoleExtra#roleExtra.randomShopList);
set_roleExtra(?undefined, _, _) ->
	ignore.

get_signSec()->
    case erlang:get(?signSec) of
        0 -> util:now();
        X -> X
    end.

set_signSec(Sec) ->
    put(?signSec,Sec).

%%　@doc 奇遇商店列表
get_randomShopList() ->
	case erlang:get(?randomShopList) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

set_randomShopList(L) ->
	erlang:put(?randomShopList, L).

init_tvcard(RoleID) ->
	TVCard = db_sql:get_tvcard(RoleID),
	set_tvcard(TVCard).

set_tvcard(TVCard) ->
	put(?tvcard,TVCard).

get_tvcard()->
	case get(?tvcard) of
		?undefined ->
			#tvcard{};
		TVCard ->
			TVCard
	end.

get_free_box_info() ->
	case get(?freeBoxInfo) of
		X when is_record(X,box_open_info) ->
			X;
%%        {box_open_info,A,B} ->
%%            Now=util:now(),
%%            {_, _,TrainerFree} = data_box:get(box_free_init),
%%            Interval = data_box:get(box_free_interval),
%%            #box_open_info{lastGerTime=A,lastItemTime=B,lastTrainerTime=Now-TrainerFree-Interval};
		_ ->
			#box_open_info{}
	end.

set_free_box_info(X) when is_record( X,box_open_info) ->
	put(?freeBoxInfo, X);
set_free_box_info(X) ->
	?ERR("set box info error:~w with roleID = ~w",[X,role_data:get_roleID()]).

init_free_box_info(RoleID,LastLogOutTime)->
    {Date,_} = util:seconds_to_datetime(LastLogOutTime),
    case erlang:date() of
        Date ->
            set_free_box_info(db_sql:get_role_free_box_info(RoleID));
        _ -> role_box:refresh_box_free()
    end.

init_gatherInfo(?undefined) ->
	ignore;
init_gatherInfo([GatherGerSets, GatherItemSets, GatherEquipSets]) ->
	set_gatherInfo(?GATHER_TYPE_GER, GatherGerSets),
	set_gatherInfo(?GATHER_TYPE_ITEM, GatherItemSets),
    set_gatherInfo(?GATHER_TYPE_EQUIP, GatherEquipSets).

init_shopNumList(ShopNumList, DShopIDList) ->
	ShopNumList2 = 
	lists:filter(fun(E) ->
						 #p_shop_num{shopID=ShopID} = E,
						 not lists:member(ShopID,DShopIDList)
				 end, ShopNumList),
	set_shopNumList(ShopNumList2).

%% @doc 获取已购买列表
get_shopNumList() ->
	case erlang:get(?shopNumList) of
		ShopNumList when is_list(ShopNumList) ->
			ShopNumList;
		_ ->
			[]
	end.

%% @doc 设置已购买列表
set_shopNumList(ShopNumList) when is_list(ShopNumList) ->
	erlang:put(?shopNumList, ShopNumList);
set_shopNumList(_) ->
	ignore.

%% @doc 获取道具使用信息列表
get_itemUseList() ->
    case erlang:get(?itemUseList) of
        ItemUseList when is_list(ItemUseList) ->
            {Date, _Time} = erlang:localtime(),
            ItemUseList2 =
                lists:foldr(fun(#item_use_info{useDate=UseDate}=ItemUseInfo, Acc) ->
                                    case UseDate =:= Date of
                                        true ->
                                            [ItemUseInfo|Acc];
                                        false ->
                                            [ItemUseInfo#item_use_info{useDate=Date,useTimes=0}|Acc]
                                    end
                            end, [], ItemUseList),
                case ItemUseList =:= ItemUseList2 of
                    true ->
                        ItemUseList;
                    false ->
                        erlang:put(?itemUseList, ItemUseList2),
                        ItemUseList2
                end;
        _ ->
            []
    end.

%% @doc 设置道具使用信息列表
set_itemUseList(ItemUseList) when is_list(ItemUseList) ->
    erlang:put(?itemUseList, ItemUseList);
set_itemUseList(_) ->
    ignore.

%% @doc 初始化道具使用信息
init_itemUseList(ItemUseList) when is_list(ItemUseList) ->
    {Date, _Time} = erlang:localtime(),
    ItemUseList2 =
        lists:foldr(fun(ItemTypeID, Acc) ->
                            case lists:keyfind(ItemTypeID, #item_use_info.itemTypeID, ItemUseList) of
                                false ->
                                    [#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=0}|Acc];
                                #item_use_info{useDate=UseDate,useTimes=UseTimes} ->
                                    case UseDate =:= Date of
                                        true ->
                                            [#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=UseTimes}|Acc];
                                        false ->
                                            [#item_use_info{itemTypeID=ItemTypeID,useDate=Date,useTimes=0}|Acc]
                                    end
                            end
                    end, [], data_item_use:get_list()),
    erlang:put(?itemUseList, ItemUseList2);
init_itemUseList(_) ->
    ignore.


%% @doc 
request_role_data(RoleID) ->
	RoleInfo1 = db_sql:get_roleInfo(RoleID),
	if is_record(RoleInfo1, role) ->
		   ignore;
	   true ->
		   exit({cannot_fetch_roleInfo, RoleID,RoleInfo1})
	end,
	RoleInfo2 = case team_manage_server:is_in_team_by_roleID_and_teamID(RoleID,RoleInfo1#role.teamId) of
		true->
			RoleInfo1;
		false->
			RoleInfo1#role{teamId=-1}
	end,
	{BattleProgress, BattleProgressHard,BattleProgressFastHard,BattleProgressTransmigration,RoleTimes, EncounterList
    , DailyInfo, RandomShopList, ItemUseList, MagicBookState,SignInfo,BattleBossRewardInfo,MainGerTypeID,MainTask,VTrainerProf,SignSec}
        = db_sql:get_roleExtra(RoleID),
	CardInfo = db_sql:get_cardInfo(RoleID),
	GatherList = db_sql:get_gatherInfo(RoleID),
	{HronInfo,HronHistory} = db_sql:get_hronInfo(RoleID),
	LimitInfo = db_sql:get_limitInfo(RoleID),
	ShopNumList = db_sql:get_shopNumList(RoleID),
	RoleExtra=
		#roleExtra{battleProgress=BattleProgress,
				   battleProgressHard=BattleProgressHard,
				   battleProgressFastHard=BattleProgressFastHard,
                   battleProgressTransmigration=BattleProgressTransmigration,
				   cardInfo=CardInfo,
				   dailyInfo=DailyInfo,
				   encounterList=EncounterList,
				   gatherList=GatherList,
				   hronInfo=HronInfo,
				   limitInfo=LimitInfo,
				   roleTimes=RoleTimes,
				   shopNumList=ShopNumList,
				   randomShopList=RandomShopList,
                   itemUseList=ItemUseList,
                  signSec=SignSec},
	{PosList1, LPosList1, GerBag1} = db_sql:get_gerList(RoleID),
	PosList = [role_crystal:init_ger_crystal(RoleInfo2,Ger)||Ger<-PosList1],
	LPosList = [role_crystal:init_ger_crystal(RoleInfo2,Ger)||Ger<-LPosList1],
	GerBag = [role_crystal:init_ger_crystal(RoleInfo2,Ger)||Ger<-GerBag1],
	% ?ERR("PosList:~w LPosList:~w GerBag:~w ~n",[PosList,LPosList,GerBag]),
	{ListOfGerEquipList, BagEquip} = db_sql:get_equipList(RoleID),
	BagItem = db_sql:get_bagItem(RoleID),
	RoleLieutenant = db_sql:get_role_lieutenant_info(RoleID),
    TeamPkInfo = db_sql:get_team_pk_data(RoleID),
    AlienInfo = db_sql:get_alien_data(RoleID),
	Now = util:now(),
	RoleInfo = RoleInfo2#role{lastLoginTime=Now},
	erlang:put(?last_mail_ts,Now+10),
	{RoleInfo, RoleExtra,GerBag, PosList, ListOfGerEquipList, BagEquip, BagItem, LPosList,RoleLieutenant, TeamPkInfo, AlienInfo
    , MagicBookState,SignInfo,BattleBossRewardInfo,MainGerTypeID,HronHistory,MainTask,VTrainerProf}.

%% @doc 获取别的玩家的出战单位列表
get_otherRoleFighter(RoleID) ->
	case role_lib:is_online(RoleID) of
		true ->
			case catch role_lib:call_server(RoleID, get_fighter_list,1000) of
				{'EXIT',_} ->
					db_sql:get_fighterList_and_lieu_add(RoleID);
				X ->
					X
				end;
		_ ->
			Result = db_sql:get_fighterList_and_lieu_add(RoleID),
			%?ERR("从数据库获取数据 ~w ~n",[Result]),
			Result
	end.

%% 获取别的玩家角色信息，这里使用了call，
get_otherRoleInfo(RoleID) ->
    case role_lib:is_online(RoleID) of
        true ->
            %TODO 这里考虑加入超时机制
            case catch role_lib:call_server(RoleID, get_role_info,1000) of
                {'EXIT',_} ->
                    db_sql:get_roleInfo(RoleID);
                X ->
                    X
                end;
        _ ->
            db_sql:get_roleInfo(RoleID)
    end.
		
get_otherRoleItemEquips(RoleID) ->
    case role_lib:is_online(RoleID) of
        true ->
            case catch role_lib:call_server(RoleID, get_role_equiped,100) of
                {'EXIT',_} ->
                    ?WARNING("call get_role_equiped is timeout. ~w",[RoleID]),
                    db_sql:get_equipedList(RoleID);
                X ->
                    X
                end;
        _ ->
            db_sql:get_equipedList(RoleID)
    end.	
get_otherRoleSkinInfo(RoleID)->
	case role_lib:is_online(RoleID) of
		true->
			case catch role_lib:call_server(RoleID,get_skin_info,1000) of
				{'EXIT',_}->
					db_sql:get_skin_info(RoleID);
				X ->
					X
			end;
		_->
			db_sql:get_skin_info(RoleID)
	end.

get_otherRoleGagList(RoleID)->
	case role_lib:is_online(RoleID) of
		true->
			case catch role_lib:call_server(RoleID,get_gag_list,1000) of
				{'EXIT',_}->
					db_sql:get_role_gag_list(RoleID);
				X ->
					X
			end;
		_->
			db_sql:get_role_gag_list(RoleID)
	end.

get_otherRolePosListT(RoleID)->
	case role_lib:is_online(RoleID) of
		true->
			case catch role_lib:call_server(RoleID,get_posListT,1000) of
				{'EXIT',_}->
					?WARNING("call get_postListT is timeout. ~w ~n",[RoleID]),
					[];
				X ->
					X
			end;
		_ ->
			[]
	end.
	
%%给战斗列表中补充符文效果。
get_FighterList_with_effect(AttackEquipedList,TarEquipedList,OldFighterList) when is_list(AttackEquipedList) andalso is_list(TarEquipedList)->
    lists:foldl(fun(P_fighter,AccFighterList)->
                        GerID = P_fighter#p_fighter.gerID,
                        % StarTotal = lists:foldl(fun([_ItemUID,ItemTypeID,ItemPos,_ItemLevel,_ItemRank,ItemGerID,_ItemDecay,_ItemExp,_ItemEnchantType,_ItemEnchantLevel,_ItemLegendRank]=E,Acc)->
                        StarTotal = lists:foldl(fun([_ItemUID,ItemTypeID,ItemPos,_ItemLevel,_ItemRank,ItemGerID|_T],Acc)->
                                        if
                                            (ItemPos =:= ?ITEM_POS_STONE_FIRST
                                            orelse ItemPos =:= ?ITEM_POS_STONE_SECOND
                                            orelse ItemPos =:= ?ITEM_POS_STONE_THREE
                                            orelse ItemPos =:= ?ITEM_POS_STONE_FOURTH)
                                            andalso GerID =:= ItemGerID  ->
                                                #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
                                                Acc+ItemStar;
                                            true ->
                                                Acc
                                        end
                                    end, 0, AttackEquipedList++TarEquipedList),
                        StoneEffectType = if
                                              StarTotal < 6 ->
                                                  0;
                                              StarTotal >= 6 andalso StarTotal =< 10 ->
                                                  1;
                                              StarTotal >= 11 andalso StarTotal =< 14 ->
                                                  2;
                                              StarTotal >= 15  ->
                                                  3;
                                              true ->
                                                  0
                                          end,
                        ?INFO("GerID:~w StoneEffectType:~w",[GerID,StoneEffectType]),
                        [P_fighter#p_fighter{stoneEffectType = StoneEffectType}|AccFighterList]
                    end,[],OldFighterList);
get_FighterList_with_effect(AttackEquipedList,TarRoleID,OldFighterList) when is_list(AttackEquipedList) andalso is_number(TarRoleID)->
    TarEquipedList = get_otherRoleItemEquips(TarRoleID),
    ?INFO("get_FighterList_with_effect TarRoleID:~w list:~w",[TarRoleID,TarEquipedList]),
    get_FighterList_with_effect(AttackEquipedList,TarEquipedList,OldFighterList);    
get_FighterList_with_effect(AttackRoleID,TarRoleID,OldFighterList) when is_number(AttackRoleID) andalso is_number(TarRoleID)->
    AttackEquipedList = get_otherRoleItemEquips(AttackRoleID),
    get_FighterList_with_effect(AttackEquipedList,TarRoleID,OldFighterList).



%% @doc 获取出战列表
get_fighter_list() ->
	get_posList().

%% @doc 获取体力相关数据
get_roleTimes() ->
	case erlang:get(?roleTimes) of
		#roleTimes{} = RoleTimes ->
			RoleTimes;
		_ ->
			#roleTimes{}
	end.

%% @doc 设置体力相关数据
set_roleTimes(ER) ->
	erlang:put(?roleTimes, ER).

init_encounterList({ExploreList, EncounterList}) when is_list(EncounterList) ->
	NowSec = util:now(),
	{EncounterList2, DiscardIDList} = 
		partition(fun(E) -> 
						  EndSec = role_explore:endTime(E#echapter.endTimerRef),
						  if EndSec == -1 ->
								 {true, E};
							  EndSec > NowSec ->
								  TimerRef = timer_wheel:add_plan(EndSec, fun() -> role_explore:del_chapter(E#echapter.id) end),
								  {true, E#echapter{endTimerRef=TimerRef}};
							  true ->
								  {MonRank, Encounter} = get_roleEncounterInfo(),
								  ChapterID = E#echapter.id,
								  Encounter2 = lists:keydelete(ChapterID, #t_encounter.chapterID, Encounter),
								  set_roleEncounterInfo({MonRank, Encounter2}),
								  false
						  end
				  
				  end, EncounterList),	
	set_encounterList({ExploreList, EncounterList2}),
	DiscardIDList;
init_encounterList(_) ->
	[].
	
partition(Pred, L) ->
    partition(Pred, L, [], []).

partition(Pred, [H | T], As, Bs) ->
    case Pred(H) of
	{true, H2}-> partition(Pred, T, [H2 | As], Bs);
	false -> partition(Pred, T, As, [H#echapter.dungeonList| Bs])
    end;
partition(Pred, [], As, Bs) when is_function(Pred, 1) ->
    {lists:reverse(As), lists:reverse(Bs)}.

%% @doc 获取遭遇战列表
get_encounterList() ->
	case erlang:get(?encounterList) of
		L when is_tuple(L) ->
			L;
		_ ->
			{[],[]}
	end.

%% @doc 设置遭遇战列表
set_encounterList(List) ->
	erlang:put(?encounterList, List).

set_roleEncounterInfo(EncounterInfo, #role{lastLogoutTime=LastTime})->
	{Date, _} = util:seconds_to_datetime(LastTime),
	case Date == erlang:date() of
		true ->
			set_roleEncounterInfo(EncounterInfo);
		_ ->
			{MonRank, Info} = EncounterInfo,
			update_encounter_monRank_info2(MonRank, Info)
	end.

update_encounter_monRank_info2(MonRank, Info)->
	RankDecay = data_common:get(encounterDecay),
	NewRank = MonRank - RankDecay,
	NewMonRank2 = 
		if NewRank >= 0 ->
			   NewRank;
		   true ->
			   0
		end,
	set_roleEncounterInfo({NewMonRank2, Info}).
			

set_roleEncounterInfo(EncounterInfo)->
	%?ERR("set:~w",[EncounterInfo]),
	erlang:put(?encounterInfo, EncounterInfo).

get_roleEncounterInfo()->
	case erlang:get(?encounterInfo) of
		L when is_tuple(L) ->
			%?ERR("get:~w",[L]),
			L;
		_ ->
			{0,[]}
	end.

set_roleTeamPkInfo(TeamPkInfo)->
    erlang:put(?teamPkInfo, TeamPkInfo).

get_roleTeamPkInfo()->
    erlang:get(?teamPkInfo).

set_roleAlienInfo(AlienInfo) ->
    erlang:put(?alienInfo, AlienInfo).

get_roleAlienInfo()->
    erlang:get(?alienInfo).

get_shopBoxData()->
	case erlang:get(?shopBoxData) of
		X when erlang:is_record(X,shop_box_card) -> X;
		_ -> #shop_box_card{}
	end.

set_shopBoxData(Data) ->
	erlang:put(?shopBoxData, Data).

init_shopBoxData(RoleID) ->
	Data = db_sql:get_role_box_shop(RoleID),
	set_shopBoxData(Data).

%% @doc 获取背包中的装备列表
get_bagEquip() ->
	case erlang:get(?bagEquip) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

set_bagEquip(ItemList) ->
	erlang:put(?bagEquip, ItemList),
    ?CATCH(role_task_trigger:handle({dispach_task, role_family_collect_equip})).


%% @doc 获取背包中的非装备
get_bagItem() ->
	case erlang:get(?bagItem) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.
	

set_bagItem(ItemList) ->
	erlang:put(?bagItem, ItemList). 

%% @doc 获取武将身上穿的装备,包括符文、训练师装备
get_equip(GerID) ->
	case erlang:get({?gerEquip,GerID}) of
		L when is_list(L) ->
			L;
		_ ->
			[]
	end.

% @doc 设置武将身上的装备列表
%% 由于很多对装备的处理过程，没有能够获得对应穿戴装备的精灵信息，故此处需要对上阵精灵（是否需要查找小伙伴）进行查找，以获得对应精灵精灵的信息，
set_equip(GerID, EquipList) ->
	PosList = role_data:get_posList(),
	case lists:keyfind(GerID,#ger.gerID,PosList) of
		false->
			set_equip(GerID,0,EquipList);
		#ger{gerBase=GerBase}->
			set_equip(GerID,GerBase#gerBase.gerTypeID,EquipList)
	end.

%%v315版本以后，装备的套装属性和穿戴者有关，故此处传入对应穿戴者的typeID
set_equip(GerID,GerTypeID,EquipList) ->
	role_allequipment:init_all_equipment(GerID,GerTypeID,EquipList),
	erlang:put({?gerEquip, GerID}, EquipList).

%% @doc 清除武将身上的装备列表
del_equip(GerID) ->
    erlang:erase({?gerEquip, GerID}).

%% @doc 两个武将交换装备
swap_equip(GerID1, GerID2) ->
	case erlang:get({?gerEquip, GerID1}) of
		[_|_] = GerEquip2->
			put({?gerEquip, GerID2}, GerEquip2);
		_ ->
			ignore
	end,
	case erlang:get({?gerEquip, GerID2}) of
		[_|_] = GerEquip1->
			put({?gerEquip, GerID1}, GerEquip1);
		_ ->
			ignore
	end.
				
%% @doc GerID1出战武将时的装备替换
replace_equip(GerIDUp,GerTypeIDUp,GerIDDown) ->
	EquipList = erlang:erase({?gerEquip,GerIDDown}),
	set_equip(GerIDUp,GerTypeIDUp,EquipList),
	EquipList.

%% @doc 获取已装备列表,包括符文！
get_equiped_list() ->
    ?INFO("get_equiped_list ~w",[get_equip(1000)]),
	[{1000,get_equip(1000)} | lists:foldl(fun(#ger{gerID=GerID},Acc) ->
						EquipList = get_equip(GerID),
						[{GerID,EquipList}| Acc]
				end, [], get_lieuposList()++get_posList())].

get_equipts_on_ger()->
	lists:foldl(fun(#ger{gerID=GerID},Acc) ->
						EquipList = get_equip(GerID),
						if EquipList =:= [] ->
							   Acc;
						   true->
							   Equips=lists:foldl(fun(#item{itemUID=ItemUID, itemTypeID=ItemTypeID, itemLevel=ItemLevel,
															itemRank=ItemRank,itemPos=ItemPos,itemDecay=ItemDecay,itemExp=ItemExp,itemenchantType=ItemEnchantType,itemenchantLevel=ItemEnchantLevel,itemLegendRank=ItemLegendRank},Acc1)->
														  %Equip=#p_equip{itemUID=ItemUID, itemTypeID=ItemTypeID, itemLevel=ItemLevel
														  %				, itemRank=ItemRank,itemGerID=GerID, itemPos=ItemPos
														  %				, itemDecay=ItemDecay, itemExp=ItemExp},
														  Equip=[ItemUID,ItemTypeID,ItemPos,ItemLevel,ItemRank,GerID,ItemDecay,ItemExp,ItemEnchantType,ItemEnchantLevel,ItemLegendRank],
														  [Equip|Acc1]
												  end, [], EquipList),
							   Equips++Acc
						end
				end, [], [#ger{gerID=1000}|get_lieuposList()++get_posList()]).


%% @doc 获取玩家的所有道具信息
get_all_item() ->
	BagEquip = get_bagEquip(),
	BagOther = get_bagItem(),
	GerEquipList = [{1000,get_equip(1000)}|[{GerID, EquipList}||
					#ger{gerID=GerID} <- get_posList(),
					EquipList <- [get_equip(GerID)],
					is_list(EquipList),
					EquipList =/= []]],
	LGerEquipList = [{GerID, EquipList}||
					 #ger{gerID=GerID} <- get_lieuposList(),
					 EquipList <- [get_equip(GerID)],
					 is_list(EquipList),
					 EquipList =/= []],
	{LGerEquipList++GerEquipList, BagEquip, BagOther}.

%% @doc 初始化玩家的所有道具
init_all_item(ListOfGerEquipList, BagEquip, BagItem,PosList) ->
    init_ger_equip(ListOfGerEquipList,PosList),
	init_bagEquip(BagEquip),
	set_bagItem(BagItem).


init_ger_equip(ListOfGerEquipList,PosList) ->
    lists:foreach(fun({GerID, EL}) ->
                        case lists:keyfind(GerID,#gerSimple.gerID,PosList) of
                            #gerSimple{gerTypeID=GerTypeID}->
                                init_ger_equip(GerID,GerTypeID,EL);
                            false->
                                init_ger_equip(GerID,0,EL)
                        end
                  end, ListOfGerEquipList).

%% 计算离线这段时间衰减后的新品阶和下次衰减的的时间
cacl_decay_rank(NextSec, NowSec, Rank) ->
	case data_item_decay:get(Rank) of
		?undefined ->
			{Rank, NextSec};
		Sec ->
			NextSec2 = NextSec+Sec,
			if NextSec2 > NowSec ->
				{Rank, NextSec2};
			   NextSec2 =:= NowSec ->
				   {Rank-1, NowSec};
				true ->
					cacl_decay_rank(NextSec2, NowSec, Rank-1)
			end
	end.

%% 判断是否需要计算品阶衰减，并重算道具属性
cacl_decay(Item, NowSec) ->
	#item{itemTypeID=ItemTypeID, itemRank=ItemRank, itemDecay=ItemDecay, itemUID=ItemUID} = Item,
	case ItemDecay of
		{NextSec,_} ->
			if NextSec > NowSec ->
				   ItemDecay2 = item_lib:item_decay(NextSec, ItemUID),
				   Item#item{itemDecay=ItemDecay2};
			   true ->
				   {ItemRank2, NextDecaySec} = cacl_decay_rank(NowSec, NowSec, ItemRank-1),
				   item_lib:recacl_item_decay2(Item, NextDecaySec, ItemRank2)
			end;
		_ ->
			#data_item{isDecay=IsDecay} = data_item:get(ItemTypeID),
			case IsDecay of
				true ->					
					ItemDecay2 = item_lib:item_decay(item_lib:next_decay_sec(IsDecay, ItemRank, NowSec), ItemUID),
					Item#item{itemUID=ItemUID, itemDecay=ItemDecay2};
				false ->
					Item
			end
	end.
			
%% @doc 初始化背包装备
init_bagEquip(BagEquip) ->
	NowSec = timer_wheel:nowsec(),
	BagEquip2 = [cacl_decay(Item,NowSec)||Item<-BagEquip],
	role_data:set_bagEquip(BagEquip2).

%% @doc 初始化上阵武将的装备
init_ger_equip(GerID,GerTypeID,EquipList) ->
	NowSec = timer_wheel:nowsec(),
	{_IsChanged, EquipList2} = 
	lists:foldl(fun(Item,{Flag,Acc}) ->
						Item2 = cacl_decay(Item,NowSec),
						if Flag orelse Item2 =/= Item ->
							   {true, [Item2|Acc]};
						   true ->
							   {false, [Item2|Acc]}
						end
				end, {false, []}, EquipList),
	EquipList3 = lists:reverse(EquipList2),
	role_data:set_equip(GerID,GerTypeID,EquipList3).

%% @doc	获取玩家单挑神将录的武将位置
get_challengeGod_pos()->
	case erlang:get(?challengeGodPos) of
		X when erlang:is_integer(X), X < 7 ->
			X;
		_ ->
			data_common:get(challengeGodPos)
	end.

set_challengeGod_pos(Pos) when erlang:is_integer(Pos), Pos < 7->
	erlang:put(?challengeGodPos, Pos);
set_challengeGod_pos(_)->
	ok.

init_trSpecial(RoleID) ->
	TrSpecial = db_sql:get_trSpecial_info(RoleID),
	set_trSpecial(TrSpecial).

get_trSpecial()->
	case get(?trSpecial) of
		X when erlang:is_record(X,trSpecial) ->
			X;
		_ ->
				#role{isMale=IsMale,level=Level} = role_data:get_roleInfo(),
				TrID = data_trSpecial:get({defaultTrID,IsMale}),
				#trSpecial{trID=TrID,specialID=0,roleLevel=Level}
	end.

set_trSpecial(Special) ->
	put(?trSpecial,Special).

init_homeBoss_times(RoleID,LastLogoutTime) ->
	Info = db_sql:get_role_homeBoss_times(RoleID,LastLogoutTime),
	set_homeBoss_times(Info).

set_homeBoss_times(Info) ->
	put(?homeBossTimes,Info).

get_homeBoss_times() ->
	case get(?homeBossTimes) of
		Info when is_record(Info,homeBoss_times) ->
			Info;
		_ ->
			role_homeBoss:init_times()
	end.

init_plane_use_info(RoleID) ->
	Info = db_sql:get_carlos_plane_info(RoleID),
	set_plane_use_info(Info),
	case Info of 
		#plane_use_info{} ->
			RoleInfo = role_data:get_roleInfo(),
			RolePublic = role_lib:get_rolePublic(RoleID),
			ets:insert(?ETS_ROLE_PUBLIC,RolePublic#rolePublic{plane_level=RoleInfo#role.plane_level});
		_ -> 
			ignore
	end.
get_plane_use_info() ->
	case get(?plane_use_info) of
		X when is_record(X, plane_use_info) ->
			X;
		_ ->
			#plane_use_info{}
	end.

set_plane_use_info(Info) ->
	put(?plane_use_info, Info).

init_hronhistory(HronHistory) when is_list(HronHistory)->
	set_hronhistory(HronHistory);
init_hronhistory(_HronHistory)->
	set_hronhistory([]).

get_hronhistory()->	
	case get(?hronhistory) of
		X when is_list(X)->
			X;
		_ ->
			[]
	end.

set_hronhistory(HronHistory)->
	put(?hronhistory,HronHistory).

%% @doc 获取玩家pvp挑战次数
get_pvp_times() ->
	#roleTimes{pvpTimes=PvpTimes} = get_roleTimes(),
	PvpTimes.
	
%% @doc 设置玩家pvp挑战次数
set_pvp_times(Times) ->
	RoleTimes = get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{pvpTimes=Times},
	PvpInterval2 = get(?currentPvpInterval),
	% ?ERR("PvpInterval ~w ~n ",[PvpInterval2]),
	?notify_update(?ra_pvpTimes(Times, RoleTimes2#roleTimes.lastPvpTime+PvpInterval2)),
	set_roleTimes(RoleTimes2).
	
	
%% pvp挑战次数减一
dec_pvp_times() ->
	#roleTimes{pvpTimes=PvpTimes} = RoleTimes = get_roleTimes(),
	PvpInterval2 = get(?currentPvpInterval),
	% ?ERR("PVP 减一"),
	case  PvpTimes > 0 of
		true ->
			NewPvpTimes = PvpTimes-1,
			RoleTimes2 = RoleTimes#roleTimes{pvpTimes=NewPvpTimes},
			% ?ERR("PvpInterval2 ~w ~n ",[PvpInterval2]),
			?notify_update(?ra_pvpTimes(NewPvpTimes, RoleTimes2#roleTimes.lastPvpTime+PvpInterval2)),
			set_roleTimes(RoleTimes2),
			NewPvpTimes;
		false ->
			NewPvpTimes = 0,
			% ?ERR("dec pvp times when ~w\n.",[RoleTimes]),
			RoleTimes2 = RoleTimes#roleTimes{pvpTimes=NewPvpTimes},
			% ?ERR("PvpInterval2 ~w ~n ",[PvpInterval2]),
			?notify_update(?ra_pvpTimes(NewPvpTimes, RoleTimes2#roleTimes.lastPvpTime+PvpInterval2)),
			set_roleTimes(RoleTimes2),
			NewPvpTimes
	end.

%% %% @doc 获取玩家夺宝挑战次数
%% get_plunder_times() ->
%% 	#roleTimes{plunderTimes=PlunderTimes} = get_roleTimes(),
%% 	PlunderTimes.
	
%% %% @doc 设置玩家夺宝挑战次数
%% set_plunder_times(Times) ->
%% 	RoleTimes = get_roleTimes(),
%% 	RoleTimes2 = RoleTimes#roleTimes{plunderTimes=Times},
%% 	PlunderInterval = role_lib:get_current_tick(?currentPlunderInterval),
%% 	?notify_update(?ra_plunderTimes(Times, RoleTimes2#roleTimes.lastPlunderTime+PlunderInterval)),
%% 	set_roleTimes(RoleTimes2).
	
	
%% 夺宝挑战次数减一
%% return：
%% dec_plunder_times() ->
%% 	#roleTimes{plunderTimes=PlunderTimes} = RoleTimes = get_roleTimes(),
%% 	% ?ERR("夺宝 减一"),
%% 	PlunderInterval2 = role_lib:get_current_tick(?currentPlunderInterval), 
%% 
%% 	case  PlunderTimes > 0 of
%% 		true ->
%% 			NewPlunderTimes = PlunderTimes-1,
%% 			RoleTimes2 = RoleTimes#roleTimes{plunderTimes=NewPlunderTimes},
%% 			?notify_update(?ra_plunderTimes(NewPlunderTimes, RoleTimes2#roleTimes.lastPlunderTime+PlunderInterval2)),
%% 			set_roleTimes(RoleTimes2),
%% 			NewPlunderTimes;
%% 		false ->
%% 			NewPlunderTimes = 0,
%% 			% ?ERR("dec plunder times when ~w\n.",[RoleTimes]),
%% 			RoleTimes2 = RoleTimes#roleTimes{plunderTimes=NewPlunderTimes},
%% 			?notify_update(?ra_plunderTimes(NewPlunderTimes, RoleTimes2#roleTimes.lastPlunderTime+PlunderInterval2)),
%% 			set_roleTimes(RoleTimes2),
%% 			NewPlunderTimes
%% 	end.

%% 每日奖励信息
get_dailyInfo() ->
	case erlang:get(?dailyInfo) of
		#daily{}=Info ->
			Info;
		_ ->			
			Info = #daily{lastLoggedLoginDate=erlang:date(),loginDays=1,lastDrawLevelUpLevel=0,lastDrawLoginRewardDays=0,lastTitleRewardDate={2013,1,1}},
			set_dailyInfo(Info),
			Info
	end.

set_dailyInfo(Daily) ->
	erlang:put(?dailyInfo, Daily).

init_dailyInfo(#daily{lastLoggedLoginDate=LastLoggedDate,loginDays=LoginDays} = Daily) ->
	NowDate = erlang:date(),
    ?INFO("init_dailyInfo(~w,~w)->~w",[NowDate , LastLoggedDate,NowDate > LastLoggedDate]),
	case NowDate > LastLoggedDate of
		true ->
		   %顺路更新一下activity中的每日重置活动
		    set_refresh_task_flag( ), 
            DayPassNum = calendar:date_to_gregorian_days(NowDate) - calendar:date_to_gregorian_days(LastLoggedDate),
			role_shop:daily_refresh_item_shop(DayPassNum),
		    activity_server:refresh_daily_activity(role_data:get_roleID()),
		    Daily2 = Daily#daily{lastLoggedLoginDate=NowDate,loginDays=LoginDays+1};
		false ->
			Daily2 = Daily
	end,
	set_dailyInfo(Daily2);	
init_dailyInfo(_) ->
	ignore.

%% 获取点将信息
get_cardInfo() ->
	case erlang:get(?cardInfo) of
		#cardInfo{}=Info ->
			Info;
		_ ->
			#cardInfo{}
	end.

%% @doc 设置点将信息
set_cardInfo(CardInfo) ->
	erlang:put(?cardInfo, CardInfo).

%% @doc 获取华容道数据
get_hronInfo() ->
	case erlang:get(?hronInfo) of
		#hronInfo{date=Date} =HronInfo->
			case erlang:date() of
				Date ->
					HronInfo;
				_ ->
					default_hronInfo()
			end;
		_ ->
			default_hronInfo()
	end.

default_hronInfo() ->
%	#role{title=Title} = role_data:get_roleInfo(),
    MaxChallengeTimes =
        case data_hron:get(max_challenge_times) of
            Value when erlang:is_integer(Value) ->
                Value;
            _ ->
                0
        end,
	#hronInfo{attackAdd=0,
			  bestScore=0,
			  challengeTimes=MaxChallengeTimes,
			  curDungeonNum=0,
			  date=erlang:date(),
			  dungeonIDList=role_hron:random_dungeon(1),
			  hpAdd=0,
			  morale=0,
              isHaveSuccReward=0,
              lastFightResult=0,
			  isSelect = 0,
			  star=1}.%role_hron:cacl_star(Title)}.


%% @doc 设置华容道数据
set_hronInfo(HronInfo) ->
	erlang:put(?hronInfo, HronInfo).

get_payGuide()->
	case erlang:get(?payGuide) of
		X when is_record(X,payGuideInfo) ->
			X;
		_ -> 
			AddPayGuideUnit=#p_payGuide_unit{task1ID=Task1ID} = role_payGuide:new_payGuideUnit(),
			#payGuideInfo{currenttaskid=Task1ID,unaccepttasklist=[AddPayGuideUnit]} 
	end.

set_headSeven(HeadSevenInfo)->
	erlang:put(?headSeven,HeadSevenInfo).

get_headSeven()->
	erlang:get(?headSeven).


set_payGuide(Unit)-> put(?payGuide,Unit).

init_payGuide(RoleID) -> set_payGuide(db_sql:get_payGuide(RoleID)).

init_headSeven(RoleID)-> 
	role_payGuide:init_role_headSeven(RoleID).

%% @doc 限制开启信息
get_limitInfo() ->
	case erlang:get(?limitInfo) of
		#limitInfo{}=L ->
			L;
		_ ->
			#limitInfo{encounterNum=0,inviteRoleID=0,inviteRoleName="",isBindWeibo=false,lastShareLevel=0}
	end.

%% @doc 设置限制开启信息
set_limitInfo(LimitInfo) ->
	erlang:put(?limitInfo, LimitInfo).

%% @doc 图鉴
get_gatherInfo(Type) ->
	case erlang:get({?gatherInfo,Type}) of
		?undefined ->
			[];
		List ->
			List
	end.

%% @doc 设置图鉴
set_gatherInfo(Type, List) ->
	erlang:put({?gatherInfo,Type},List).

%% @doc 完美通关的章节ID列表
get_bestPassChapterID(RoleID) ->
	case db_sql:get_bestPassChapterID(RoleID) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

get_mirror()->
    case erlang:get(?roleGerMirror) of
        Info when erlang:is_record(Info, gerMirrorInfo) ->
            Info;
        ?undefined ->
            RoleID = role_data:get_roleID(),            
            db_sql:get_gerMirrorInfo(RoleID)
    end.

set_mirror(GerMirrorInfo)->
    erlang:put(?roleGerMirror, GerMirrorInfo).

%% 获取可微博分享的事件
get_weibo_share_list() ->
	case erlang:get(?weiboShareList) of
		List when is_list(List) ->
			List;
		_ ->
			[]
	end.

set_weibo_share_list(List) ->
	erlang:put(?weiboShareList, List).

%% 获取微博分享次数
get_weibo_count() ->
	#roleTimes{weiboCount=WeiboCount,nextWeiboCountRefreshSec=NextRefreshSec} = RoleTimes = get_roleTimes(),
	NowSec = timer_wheel:nowsec(),
	case  NowSec >= NextRefreshSec of
		true ->
			NewWeiboCount = data_common:get(max_weibo_count),
			RefreshTime = data_common:get(weibo_refresh_time),
			NextWeiboSec2 = util:datetime_to_seconds({erlang:date(),RefreshTime}) + ?ONE_DAY_SECONDS,
			RoleTimes2 = RoleTimes#roleTimes{weiboCount=NewWeiboCount,nextWeiboCountRefreshSec=NextWeiboSec2},
			set_roleTimes(RoleTimes2),
			?notify_update(?ra_weiboCount(NewWeiboCount)),
			NewWeiboCount;
		false ->
			WeiboCount
	end.

set_weibo_count(Num) ->
	RoleTimes = get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{weiboCount=Num},
	?notify_update(?ra_weiboCount(Num)),
	set_roleTimes(RoleTimes2).

dec_weibo_count() ->
	#roleTimes{weiboCount=WeiboCount,nextWeiboCountRefreshSec=NextRefreshSec} = RoleTimes = get_roleTimes(),
	NowSec = timer_wheel:nowsec(),
	case  NowSec >= NextRefreshSec of
		true ->
			NewWeiboCount = erlang:max(data_common:get(max_weibo_count) - 1,0),
			RefreshTime = data_common:get(weibo_refresh_time),
			NextWeiboCountSec2 = util:datetime_to_seconds({erlang:date(),RefreshTime}) + ?ONE_DAY_SECONDS,
			RoleTimes2 = RoleTimes#roleTimes{weiboCount=NewWeiboCount,nextWeiboCountRefreshSec=NextWeiboCountSec2},
			?notify_update(?ra_weiboCount(NewWeiboCount)),
			set_roleTimes(RoleTimes2),
			NewWeiboCount;
		false ->
			NewWeiboCount = erlang:max(WeiboCount-1, 0),
			RoleTimes2 = RoleTimes#roleTimes{weiboCount=NewWeiboCount},
			?notify_update(?ra_weiboCount(NewWeiboCount)),			
			set_roleTimes(RoleTimes2),
			NewWeiboCount
	end.
		
get_growth_fund_record()->
    case erlang:get(?growth_fund_record) of
        Info when erlang:is_record(Info, growth_fund_record) ->
            Info;
        ?undefined ->       
            Info = db_sql:get_growth_fund_record(role_data:get_roleID()),
            erlang:put(?growth_fund_record, Info),
            Info
    end.

set_growth_fund_record(GerMirrorInfo) when erlang:is_record(GerMirrorInfo, growth_fund_record) ->
    erlang:put(?growth_fund_record, GerMirrorInfo).

%update_treasure_patch()
%% ====================================================================
%% Internal functions
%% ====================================================================

calc_patch(21201, Num, RPatch)->
	N1 = RPatch#t_patch.p_21201,
	{RPatch#t_patch{p_21201=N1 + Num},N1};
calc_patch(21202, Num, RPatch)->
	N1 = RPatch#t_patch.p_21202,
	{RPatch#t_patch{p_21202=N1 + Num},N1};
calc_patch(21203, Num, RPatch)->
	N1 = RPatch#t_patch.p_21203,
	{RPatch#t_patch{p_21203=N1 + Num},N1};
calc_patch(21204, Num, RPatch)->
	N1 = RPatch#t_patch.p_21204,
	{RPatch#t_patch{p_21204=N1 + Num},N1};
calc_patch(21205, Num, RPatch)->
	N1 = RPatch#t_patch.p_21205,
	{RPatch#t_patch{p_21205=N1 + Num},N1};
calc_patch(21211, Num, RPatch)->
	N1 = RPatch#t_patch.p_21211,
	{RPatch#t_patch{p_21211=N1 + Num},N1};
calc_patch(21212, Num, RPatch)->
	N1 = RPatch#t_patch.p_21212,
	{RPatch#t_patch{p_21212=N1 + Num},N1};
calc_patch(21213, Num, RPatch)->
	N1 = RPatch#t_patch.p_21213,
	{RPatch#t_patch{p_21213=N1 + Num},N1};
calc_patch(21214, Num, RPatch)->
	N1 = RPatch#t_patch.p_21214,
	{RPatch#t_patch{p_21214=N1 + Num},N1};
calc_patch(21215, Num, RPatch)->
	N1 = RPatch#t_patch.p_21215,
	{RPatch#t_patch{p_21215=N1 + Num},N1};
calc_patch(21216, Num, RPatch)->
	N1 = RPatch#t_patch.p_21216,
	{RPatch#t_patch{p_21216=N1 + Num},N1};
calc_patch(21217, Num, RPatch)->
	N1 = RPatch#t_patch.p_21217,
	{RPatch#t_patch{p_21217=N1 + Num},N1};
calc_patch(21218, Num, RPatch)->
	N1 = RPatch#t_patch.p_21218,
	{RPatch#t_patch{p_21218=N1 + Num},N1};
calc_patch(21219, Num, RPatch)->
	N1 = RPatch#t_patch.p_21219,
	{RPatch#t_patch{p_21219=N1 + Num},N1};
calc_patch(21220, Num, RPatch)->
	N1 = RPatch#t_patch.p_21220,
	{RPatch#t_patch{p_21220=N1 + Num},N1};
calc_patch(21231, Num, RPatch)->
	N1 = RPatch#t_patch.p_21231,
	{RPatch#t_patch{p_21231=N1 + Num},N1};
calc_patch(21232, Num, RPatch)->
	N1 = RPatch#t_patch.p_21232,
	{RPatch#t_patch{p_21232=N1 + Num},N1};
calc_patch(21233, Num, RPatch)->
	N1 = RPatch#t_patch.p_21233,
	{RPatch#t_patch{p_21233=N1 + Num},N1};
calc_patch(21234, Num, RPatch)->
	N1 = RPatch#t_patch.p_21234,
	{RPatch#t_patch{p_21234=N1 + Num},N1};
calc_patch(21235, Num, RPatch)->
	N1 = RPatch#t_patch.p_21235,
	{RPatch#t_patch{p_21235=N1 + Num},N1};
calc_patch(21241, Num, RPatch)->
	N1 = RPatch#t_patch.p_21241,
	{RPatch#t_patch{p_21241=N1 + Num},N1};
calc_patch(21242, Num, RPatch)->
	N1 = RPatch#t_patch.p_21242,
	{RPatch#t_patch{p_21242=N1 + Num},N1};
calc_patch(21243, Num, RPatch)->
	N1 = RPatch#t_patch.p_21243,
	{RPatch#t_patch{p_21243=N1 + Num},N1};
calc_patch(21244, Num, RPatch)->
	N1 = RPatch#t_patch.p_21244,
	{RPatch#t_patch{p_21244=N1 + Num},N1};
calc_patch(21245, Num, RPatch)->
	N1 = RPatch#t_patch.p_21245,
	{RPatch#t_patch{p_21245=N1 + Num},N1};
calc_patch(21251, Num, RPatch)->
	N1 = RPatch#t_patch.p_21251,
	{RPatch#t_patch{p_21251=N1 + Num},N1};
calc_patch(21252, Num, RPatch)->
	N1 = RPatch#t_patch.p_21252,
	{RPatch#t_patch{p_21252=N1 + Num},N1};
calc_patch(21253, Num, RPatch)->
	N1 = RPatch#t_patch.p_21253,
	{RPatch#t_patch{p_21253=N1 + Num},N1};
calc_patch(21254, Num, RPatch)->
	N1 = RPatch#t_patch.p_21254,
	{RPatch#t_patch{p_21254=N1 + Num},N1};
calc_patch(21255, Num, RPatch)->
	N1 = RPatch#t_patch.p_21255,
	{RPatch#t_patch{p_21255=N1 + Num},N1};
calc_patch(21261, Num, RPatch)->
	N1 = RPatch#t_patch.p_21261,
	{RPatch#t_patch{p_21261=N1 + Num},N1};
calc_patch(21262, Num, RPatch)->
	N1 = RPatch#t_patch.p_21262,
	{RPatch#t_patch{p_21262=N1 + Num},N1};
calc_patch(21263, Num, RPatch)->
	N1 = RPatch#t_patch.p_21263,
	{RPatch#t_patch{p_21263=N1 + Num},N1};
calc_patch(21264, Num, RPatch)->
	N1 = RPatch#t_patch.p_21264,
	{RPatch#t_patch{p_21264=N1 + Num},N1};
calc_patch(21265, Num, RPatch)->
	N1 = RPatch#t_patch.p_21265,
	{RPatch#t_patch{p_21265=N1 + Num},N1};
calc_patch(21266, Num, RPatch)->
	N1 = RPatch#t_patch.p_21266,
	{RPatch#t_patch{p_21266=N1 + Num},N1};
calc_patch(21267, Num, RPatch)->
	N1 = RPatch#t_patch.p_21267,
	{RPatch#t_patch{p_21267=N1 + Num},N1};
calc_patch(21268, Num, RPatch)->
	N1 = RPatch#t_patch.p_21268,
	{RPatch#t_patch{p_21268=N1 + Num},N1};
calc_patch(21269, Num, RPatch)->
	N1 = RPatch#t_patch.p_21269,
	{RPatch#t_patch{p_21269=N1 + Num},N1};
calc_patch(21270, Num, RPatch)->
	N1 = RPatch#t_patch.p_21270,
	{RPatch#t_patch{p_21270=N1 + Num},N1};
calc_patch(_,_,RPatch)->
	{RPatch, 0}.

posList_filter(PosList, LPosList)->
	{filt_PosList(PosList), filt_PosList(LPosList)}.

filt_PosList(PosList)->
	PartNum = data_partner:get(partner_num),
	lists:foldl(fun(#ger{gerBase=Base}=Ger, Acc)->
						if Base#gerBase.gerPos > PartNum ->
							   do_down_equip(Ger#ger.gerID),
							   do_down_ger(Ger),
							   Acc;
						   true ->
							   [Ger|Acc]
						end;
				   (#gerSimple{gerID=GerID,gerPos=GerPos}=GerSimple,Acc)->
						if GerPos > PartNum ->
							   do_down_equip(GerID),
							   do_down_ger(GerSimple),
							   Acc;
						   true ->
							   [GerSimple|Acc]
						end
				end, [], PosList).

init_tag_data(RoleID) ->
    Data = db_sql:get_role_tag_data(RoleID),
    set_tag_data(Data).

set_tag_data(Data) ->
    put(?tag_data,Data).

get_tag_data() ->
    erlang:get(?tag_data).

init_trainerProf_battle_data(RoleID) ->
    Data = db_sql:get_trainerProf_battle_data(RoleID),
    set_trainerProf_battle_data(Data).

get_trainerProf_battle_data()->
    case get(?trainerProf_battle) of
        X when is_list(X) -> X;
        _ ->  []
    end.

set_trainerProf_battle_data(Data)->
    put(?trainerProf_battle, Data).

do_down_equip(GerID)->
	GerEquip = 
		lists:foldl(fun(Item,Acc)->[Item#item{itemPos=0}|Acc] end, [], get_equip(GerID)),
	BagEquip = get_bagEquip(),
	set_bagEquip(GerEquip++BagEquip).

do_down_ger(#ger{gerBase=Base}=Ger)->
	GerBag = get_gerBag(),
	Ger=#gerSimple{gerExp=Base#gerBase.gerExp,gerID=Ger#ger.gerID,gerTypeID=Base#gerBase.gerTypeID,
				   gerQuality=Base#gerBase.gerQuality,gerLevel=Base#gerBase.gerLevel,gerPos=0},
	set_gerBag([Ger|GerBag]);
do_down_ger(#gerSimple{}=GerSimple)->
	GerBag = get_gerBag(),
	Ger = GerSimple#gerSimple{gerPos=0},
	set_gerBag([Ger|GerBag]).

%% 单本魔典的属性加成(没有收集满时)
add_picture_buffer([], _, Buffer) ->
    Buffer;
add_picture_buffer([H|T], [CH|CT], Buffer) ->
    case H of
        "0" ->
            add_picture_buffer(T, CT, Buffer);
        _ ->
            OpenPos = erlang:min(3, H - $0), 
            #magic_picture{posInfo=PosInfo} = data_magic_picture:get(CH),
            {_, NewBuffer} = 
                lists:foldl(fun(_, {[#magic_gerPos{buffer=PBuffer}|PT], BufferAcc}) ->
                                    {PT, ger_attr:append_add_attr(BufferAcc, PBuffer)}
                                end, {PosInfo, Buffer}, lists:seq(1, OpenPos)),
            add_picture_buffer(T, CT, NewBuffer)
    end.

%% 计算魔典完成百分比
calc_magic_book_percent([], Has, Num) ->
    {Has, Num * ?MAX_MAGIC_PICTURE_POS};
calc_magic_book_percent([H|T], Has, Num) ->
    HasPos = erlang:min(?MAX_MAGIC_PICTURE_POS, H - $0),
    calc_magic_book_percent(T, Has + HasPos, Num + 1).
calc_magic_book_percent(PictureStr) ->
    calc_magic_book_percent(PictureStr, 0, 0).

%% 初始化魔典加成
%% 因为魔典是按顺序开的,且前一本收集满了才能解锁后一本,所以这个函数
%% 最坏情况是:收集了5本,最后一本差一个位置收集满,这个时候的时间复杂度
%% 是O(5 + 14 * 3 + 2)=O(49),能接受
calc_magicBook_add_attr([], _, _, Buffer) ->
    Buffer;
calc_magicBook_add_attr([H|T], [PH|PT], BookID, Buffer) ->
    #magic_book{buffer=TotalBuffer, picture_list=PictureList} = data_magic_book:get({book, BookID}),
    case H of
        $1  ->
            Buffer;
        $2  ->
            add_picture_buffer(PH, PictureList, Buffer);
        _ ->
            NewBuffer = ger_attr:append_add_attr(Buffer, TotalBuffer),
            calc_magicBook_add_attr(T, PT, BookID + 1, NewBuffer)
    end.

% 生成魔典初始化数据(前一本没有收集满,不解锁下一本,所以只处理一本)
gen_init_magicBook_state(_, 2, State) ->
    State;
gen_init_magicBook_state(RoleLevel, BookID, State = #magicBook_state{summary=Summary, picture_state=PictureState,percent=PercentList}) ->
    #magic_book{open_level=BookOpenLevel, picture_list=PictureList} = data_magic_book:get({book, BookID}),
    case RoleLevel >= BookOpenLevel of
        false ->
            State;
        _ ->
            PictureStr = role_magicBook:gen_picture_state_str(PictureList),
            TotalPos = erlang:length(PictureStr) * ?MAX_MAGIC_PICTURE_POS,
            Summary2 = Summary ++ "1",
            PictureState2 = PictureState ++ [PictureStr],
            PercentList2 = PercentList ++ [{0, TotalPos}],
            NewState = #magicBook_state{summary=Summary2, picture_state=PictureState2, percent=PercentList2}, 
            gen_init_magicBook_state(RoleLevel, BookID + 1, NewState)
    end.

init_magicBook_add_attr(StateStrT) ->
    VoidBuffer = #add_attr{},
    StateStr = 
        case is_binary(StateStrT) of
            true ->
                erlang:binary_to_list(StateStrT);
            _ ->
                StateStrT
        end,
    #role{level=Level} = Role = get_roleInfo(),
    {BookState, Buffer} =
        case StateStr of
            "" ->
                OpenLevel = data_magic_book:get(open_level),
                case Level >= OpenLevel of
                    false ->
                        {#magicBook_state{}, VoidBuffer};
                    _ ->
                        {gen_init_magicBook_state(Level, 1, #magicBook_state{}), VoidBuffer}
                end;
            _ -> 
                [BookStr|PictureState] = string:tokens(StateStr, "-"),
                PercentList = lists:map(fun(E) -> calc_magic_book_percent(E) end, PictureState),
                
                %% 判断魔典是否收集满了
                PictureStateLen = erlang:length(PictureState),
                CurState = if
                               PictureStateLen =:= 6 ->
                                    case lists:all(fun(E) -> E >= $3 end, lists:nth(6, PictureState)) of
                                        true ->
                                            $3;
                                        _ ->
                                            $2
                                    end;
                               true ->
                                   $2
                           end,
                %% 判断是否解锁下一本
                {BookStr2, PictureListState2, PercentList2} = 
                    role_magicBook:unlock_next(6, Role, CurState, BookStr, PictureState, PercentList),
                {#magicBook_state{summary=BookStr2, picture_state=PictureListState2, percent=PercentList2}, calc_magicBook_add_attr(BookStr2, PictureListState2, 1, #add_attr{})}
        end,
    erlang:put(?magicBook_state, BookState),
    erlang:put(?magicBook_add_attr, Buffer).
    
get_magicBook_add_attr() ->
    case erlang:get(?magicBook_add_attr) of
        undefined ->
            #add_attr{};
        Attr ->
            Attr
    end.
            
set_magicBook_add_attr(Attr) ->
    erlang:put(?magicBook_add_attr, Attr).

get_magicBook_state() ->
    case erlang:get(?magicBook_state) of
        undefined ->
            #magicBook_state{};
        Info ->
            Info
    end.

set_magicBook_state(StateInfo) ->
    erlang:put(?magicBook_state, StateInfo).

get_magicBook_state_str() ->
    #magicBook_state{summary=BookStr, picture_state=PictureState} = get_magicBook_state(),
    case BookStr of
        "" ->
            "";
        _ ->
            lists:foldl(fun(Str, Acc) -> Acc ++ "-" ++ Str end, BookStr, PictureState) 
    end.

init_activityFestival(RoleID) ->
	Info = db_sql:get_role_activityFestival(RoleID),
	set_activityFestival(Info).

get_activityFestival() ->
	case erlang:get(?activityFestival) of
		?undefined -> #activity_festival{};
		X -> X
	end.

set_activityFestival(V) ->
	erlang:put(?activityFestival, V).

init_signinfo(SignInfo) when is_record(SignInfo,sign_info)->
	SignInfo2 = update_signinfo(SignInfo),
	%%此处将玩家的累积签到数据进行了转换
	NewAccSignRewardNum = role_sign:transform_old_acc_sign2_new_acc_sign(SignInfo2#sign_info.is_get_acc_sign_reward),
	set_signinfo(SignInfo2#sign_info{is_get_acc_sign_reward=NewAccSignRewardNum});
init_signinfo(_SignInfo)->
	set_signinfo(#sign_info{}).

update_signinfo(SignInfo)->
	{Year,Month,Day} = erlang:date(),
	TodayZero = util:datetime_to_seconds({{Year,Month,Day},{0,0,0}}),
	SignInfo1 = case SignInfo#sign_info.last_sign_time >= TodayZero of
		true->
			SignInfo;
		false->
			%%新的一天开始
			SignInfo#sign_info{is_get_sign_reward=0}
	end,
	ThisMonthZero = util:datetime_to_seconds({{Year,Month,1},{0,0,0}}),
	case SignInfo1#sign_info.last_sign_time >= ThisMonthZero of
		true->
			SignInfo1;
		false->
			%%新的一月开始
			SignInfo1#sign_info{sign_time_count=0,is_get_acc_sign_reward=0}
	end.

set_signinfo(SignInfo)->
	erlang:put(?sign_info,SignInfo).

get_signinfo()->
	case erlang:get(?sign_info) of
		?undefined->
			#sign_info{};
		X ->
			X
	end.

set_canvass_info(CanvassInfo)->
    erlang:put(?canvass_info,CanvassInfo).

get_canvass_info()->
    {CanvassId,CanvassInfo} = 
        case erlang:get(?canvass_info) of
            ?undefined->
                db_sql:get_canvass_info(role_data:get_roleID());
            {CanvassId0,CanvassInfo0} ->
                {CanvassId0,CanvassInfo0}
        end,
    case data_canvass:get(canvass_id) of
        CanvassId ->
            set_canvass_info({CanvassId,CanvassInfo}),
            {CanvassId,CanvassInfo};
        NewCanvassId ->
            set_canvass_info({NewCanvassId,[]}),
            {0,[]}
    end.

%%初始化玩家的关卡boss宝箱信息
init_battlebossrewardinfo(BattleBossRewardInfo)->
	case check_battlebossrewardinfo_valid(BattleBossRewardInfo) of
		true->
			set_battlebossrewardinfo(BattleBossRewardInfo);
		false->
			NewBattleBossRewardInfo = role_battle:init_battlebossrewardinfo(),
			set_battlebossrewardinfo(NewBattleBossRewardInfo)
	end.

check_battlebossrewardinfo_valid(-1)->
	false;
check_battlebossrewardinfo_valid([])->
	false;
check_battlebossrewardinfo_valid(BattleBossRewardInfo) when is_list(BattleBossRewardInfo)->
	BattleInfo = hd(BattleBossRewardInfo),
	if
		is_record(BattleInfo,battle_boss_reward_info) ->
			true;
		true->
			false
	end.

set_battlebossrewardinfo(BattleBossRewardInfo)->
	erlang:put(?battle_boss_reward_info,BattleBossRewardInfo).

get_battlebossrewardinfo()->
	case erlang:get(?battle_boss_reward_info) of
		?undefined->
			[];
		X->
			X
	end.

init_maingertypeid(MainGerTypeID)->
	set_main_gerTypeID(MainGerTypeID).

set_main_gerTypeID(MainGerTypeID)->
	erlang:put(?main_ger_typeid,MainGerTypeID).

get_main_gerTypeID()->
	case erlang:get(?main_ger_typeid) of
		?undefined->
			reinit_main_ger_typeID();
		X->
			case lists:member(X,[5120,5130,5020,5100]) of
				false->
					reinit_main_ger_typeID();
				true->
					X
			end
	end.
reinit_main_ger_typeID()->
	#trSpecial{specialID=SpecialID} = role_data:get_trSpecial(),
	MainGerTypeID = if
		SpecialID=:=1001 ->
			5100;
		SpecialID=:=1002 ->
			5120;
		SpecialID=:=1003 ->
			5020;
		SpecialID=:=1004 ->
			5130;
		true->
			5130
	end,
	set_main_gerTypeID(MainGerTypeID),
	MainGerTypeID.

%% 重置3v3挑战次数
reset_teamPkTimes() ->
    RoleTimes = get_roleTimes(),
    Limit = role_common:get(max_teamPkTimes),
	DInterval = role_lib:get_current_tick(?currentTeamPkIntercal),
    ?notify_update(?ra_teamPkTimes(Limit, RoleTimes#roleTimes.lastTeamPkTime + DInterval)),
    RoleTimes2 = RoleTimes#roleTimes{teamPkTimes = Limit},
    set_roleTimes(RoleTimes2).

add_create_bagData(Role) ->
	Reward = data_common:get(create_bag_reward),
	role_reward:handle_sell_reward_f(Role,Reward,?MONEY_ADD_TYPE_CREAT_BAG,0,"").

%% 设置角色升级脏位
mark_role_lvl_up_refresh() ->
    put(?mark_role_lvl_up_refresh, true).

clear_role_lvl_up_refresh() ->
    erase(?mark_role_lvl_up_refresh).

get_role_lvl_up_refresh() ->
    get(?mark_role_lvl_up_refresh).

get_plane_ai_flag()->
    case get(?plane_ai_flag) of
        PlaneAiFlag when erlang:is_integer(PlaneAiFlag) ->
            PlaneAiFlag;
        _ ->
            7
    end.
set_plane_ai_flag_all(PlaneAiFlag) when erlang:is_integer(PlaneAiFlag)->
    put(?plane_ai_flag,PlaneAiFlag).

save_plane_ai_flag_type(FlagType) when erlang:is_integer(FlagType)->
    PlaneAiFlag = get_plane_ai_flag(),
    NewPlaneAiFlag = 
        case FlagType of
            ?ai_flag_carlos -> PlaneAiFlag bor 1;
            ?ai_flag_galactica -> PlaneAiFlag bor 2;
            ?ai_flag_conquerisland -> PlaneAiFlag bor 4;
            _ -> PlaneAiFlag
        end,
    set_plane_ai_flag_all(NewPlaneAiFlag).

check_plane_ai_flag_type(FlagType) when erlang:is_integer(FlagType)->
    PlaneAiFlag = get_plane_ai_flag(),
    <<Conquerisland:1,Galactica:1,Carlos:1>> = <<PlaneAiFlag:3>>,
    case FlagType of
        ?ai_flag_carlos -> Carlos =:= 0;
        ?ai_flag_galactica -> Galactica =:= 0;
        ?ai_flag_conquerisland -> Conquerisland =:= 0;
        _ -> false
    end.

get_xbattle_chapterID() ->
	#role_xbattle{chapterID=CurChapterID} = get_xbattle_data(),
	case CurChapterID > 0 of
		?TRUE -> CurChapterID;
		_ ->
			CfgChapterID = data_common:get(xbattle_first_chapter),
			util:getTernaryValue(CfgChapterID > 0, CfgChapterID, 10001)
	end.