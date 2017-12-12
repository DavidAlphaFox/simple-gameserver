%% @author caohongyang
%% @doc gm命令接口
%% Created 2013-4-17


-module(role_gm).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([
		]).


%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
add_reputation({add_reputation, Value}) ->
	Role = role_data:get_roleInfo(),
	role_lib:add_reputation_f(Role, Value, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_honor({add_honor, Hon}) ->
    Role = role_data:get_roleInfo(),
    role_lib:add_honor_f(Role, Hon, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

set_pvppoint({set_pvppoint, PP}) ->
    Role = role_data:get_roleInfo(),
    Role2 = Role#role{pvppoint=PP},
    role_data:set_roleInfo(Role2),
    ?notify_update(?ra_pvppoint(PP)).

set_vip({set_vip, Value}) when is_integer(Value)->
	Role = role_data:get_roleInfo(),
	Role2 = Role#role{vipLevel=Value},
	role_data:set_roleInfo(Role2).

set_energy({set_energy, Value}) ->
	RoleTimes = role_data:get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{energy=Value},
	role_data:set_roleTimes(RoleTimes2).

refresh_energy({refresh_energy}) ->
	RoleTimes = role_data:get_roleTimes(),
	RoleTimes2 = RoleTimes#roleTimes{energy=300,discoveryTimes=300,pvpTimes=100},
	role_data:set_roleTimes(RoleTimes2).
	

add_coin({add_coin, AddCoin}) ->	
	Role = role_data:get_roleInfo(),
	role_lib:add_coin_f(Role, AddCoin, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_home_resource({add_home_resource, AddValue}) ->    
    Role = role_data:get_roleInfo(),
    role_lib:add_home_resource_f(Role, AddValue, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

set_gold({set_gold, Gold}) ->	
	Role = role_data:get_roleInfo(),
	Role2 = Role#role{gold=Gold},
	role_data:set_roleInfo(Role2),
	?notify_update(?ra_gold(Gold)).

set_unioncoin({set_unioncoin, Gold}) ->   
    Role = role_data:get_roleInfo(),
    Role2 = Role#role{unioncoin=Gold},
    role_data:set_roleInfo(Role2),
    ?notify_update(?ra_unioncoin(Gold)).

set_level({set_level,Level}) ->
	Role = role_data:get_roleInfo(),
	Exp = data_role_level:get(Level+1)-2,
	Role2 = Role#role{level=Level,exp=Exp},
    role_lib:add_exp(Role2, 1),   %% 增加一个加经验的调用，用于出发加经验时，会检测的冲级排名变更
	role_lib:hook_level_up(Role2, Level),
	?notify_update(?ra_exp(Exp)),
	?notify_update(?ra_level(Level)),
    role_data:set_roleInfo(Role2),
    role_lib:update_rolePublic(Role2,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data()),
    ?notify_update(?ra_remains_point(role_talent:get_remains_point())).

refresh_today_task(_)->
	role_task:hook_zero_clock().

add_ger({add_ger,GerTypeID}) ->
	ger_lib:add_ger(#new_ger{gerTypeID=GerTypeID,gerLevel=1,gerQuality=0}, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_ger2({add_ger2,GerTypeID,Level,Rank}) ->
	ger_lib:add_ger(#new_ger{gerTypeID=GerTypeID,gerLevel=Level,gerQuality=Rank}, ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_item({add_item, ItemTypeID, Num}) ->
	item_lib:add_item_f([#new_item{itemLevel=32,itemNum=Num,itemRank=1,itemTypeID=ItemTypeID}], ?MONEY_ADD_TYPE_GM_CMD, 0, "").

add_item2({add_item2, ItemTypeID, Num, Level, Rank}) ->
	item_lib:add_item_f([#new_item{itemLevel=Level,itemNum=Num,itemRank=Rank,itemTypeID=ItemTypeID}], ?MONEY_ADD_TYPE_GM_CMD, 0, "").


%% ====================================================================
%% Internal functions
%% ====================================================================

-define(MATCH_LIST, [
					{"H ~d ~d ~d",fun([A,B,C])->["homestead",A,B,C] end},
					 {"H ~d ~d",fun([A,B])->["homestead",A,B] end},
					 {"H ~d",fun([A])->["homestead",A] end},
					{"I ~d ~d级~d阶~d",fun([A,B,C,D])->["ItemID",A,B,C,D] end},
					{"I ~d ~d级~d阶",fun([A,B,C])->["ItemID",A,B,C,1] end},
					{"I ~d ~d阶~d",fun([A,B,C])->["ItemID",A,1,B,C] end},
					{"I ~d ~d",fun([A,B])->["ItemID",A,1,0,B] end},
					{"I ~d",fun([A])->["ItemID",A,1,0,1] end},
					{"G ~d ~d级~d阶~d",fun([A,B,C,D])->["GerID",A,B,C,D] end},
					{"G ~d ~d级~d阶",fun([A,B,C])->["GerID",A,B,C,1] end},
					{"G ~d ~d阶~d",fun([A,B,C])->["GerID",A,1,B,C] end},
					{"G ~d ~d",fun([A,B])->["GerID",A,1,0,B] end},
					{"G ~d",fun([A])->["GerID",A,1,0,1] end},
					 {"~s ~d级~d阶~d",fun(E) -> E end},
					 {"~s ~d级~d阶~d",fun([A,C,B,D]) -> [A,B,C,D] end},
					 {"~s ~d级~d阶",  fun([A,B,C]) ->[A,B,C,1] end},
					 {"~s ~d级~d", 	fun([A,B,D])	->[A,B,0,D] end},
					 {"~s ~d阶~d",	fun([A,C,D]) 	->[A,1,C,D] end},
					 {"~s ~d",		fun([A,D]) ->	[A,1,0,D] end},
					 {"~s",			fun([A]) ->[A,1,0,1] end}
					]).

test_num(NS) ->
	list_to_integer(string:strip(NS, both)).

test("帮助") ->
	test_error();
test("元宝"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{gold=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_gold(Num)),
	test_ok();
test("绑定元宝"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{goldBonus=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_goldBonus(Num)),
	test_ok();
test("银两"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{coin=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_coin(Num)),
	test_ok();
test("声望"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{reputation=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_reputation(Num)),
	test_ok();
test("等级"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	Exp = data_role_level:get(Num+1)-1,
	RoleInfo2 = RoleInfo#role{level=Num,exp=Exp},
	role_lib:hook_level_up(RoleInfo, Num),
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_exp(Exp)),
	?notify_update(?ra_level(Num)),
	test_ok();
test("官爵"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{title=Num},
	role_data:set_roleInfo(RoleInfo2),
	?notify_update(?ra_title(Num)),
	test_ok();
test("vip"++NS) ->
	Num = test_num(NS),
	RoleInfo = role_data:get_roleInfo(),
	RoleInfo2 = RoleInfo#role{vipLevel=Num},
	role_data:set_roleInfo(RoleInfo2),
	RoleTimes=role_data:get_roleTimes(),
	VipChallengeGodFreeTimes = role_lib:get_max_challengeGodFreeTimes(Num),
	VipChallengeGodBuyTimes = RoleTimes#roleTimes.challengeGodBuyTimes,%role_lib:get_max_challengeGodBuyTimes(Num),
	NewChallengeGodEnergy = VipChallengeGodFreeTimes - (role_lib:get_max_challengeGodFreeTimes(RoleInfo#role.vipLevel) - RoleTimes#roleTimes.challengeGodEnergy),
	?ERR("V:~w",[VipChallengeGodFreeTimes]),
	RoleTimes2 = RoleTimes#roleTimes{challengeGodEnergy=NewChallengeGodEnergy,
									 challengeGodBuyTimes=VipChallengeGodBuyTimes
									},
	role_data:set_roleTimes(RoleTimes2),
	?notify_update(?ra_vipLevel(Num, NewChallengeGodEnergy,VipChallengeGodBuyTimes)),
    MaxDscv = role_lib:get_max_dscv_times(Num),
    MaxEnergy = role_lib:get_max_energy(Num),
    ?sendself(#sc_role_base_config{dscvMax=MaxDscv,energyMax=MaxEnergy}),
	test_ok();
test("体力"++_) ->
	RoleTimes = role_data:get_roleTimes(),
    case role_data:get_roleInfo() of
        #role{vipLevel=VipLevel} ->
            next;
        _ ->
            VipLevel = 0
    end,
	MaxDscv = role_lib:get_max_dscv_times(VipLevel),
	MaxEnergy = role_lib:get_max_energy(VipLevel),
	%MaxPlunderTimes = data_common:get(max_plunder_times),
	MaxPVPTimes = data_common:get(max_pvp_times),
	RoleTimes2 = RoleTimes#roleTimes{energy=MaxEnergy,
								discoveryTimes=MaxDscv,
								pvpTimes=MaxPVPTimes},
	role_data:set_roleTimes(RoleTimes2),
	?notify_update(?ra_dscv(MaxDscv, RoleTimes#roleTimes.lastDscvTime+role_lib:get_current_tick(?currentdiscoveryInterval))),
	?notify_update(?ra_energy(MaxEnergy, RoleTimes#roleTimes.lastEnergyTime+role_lib:get_current_tick(?currentEnergyIntercal))),
	test_ok();	
test("充值"++NS) ->
	Num = test_num(NS),
	List = [data_pay:get(E)||E<-data_pay:get_list()],
	case lists:keyfind(Num, #data_pay.payGold, List) of
		false ->
			%test_error();RoleID, Amount, Receipt, Md5, SrcType
			gen_server:call(pay_server,{func,fun pay_server:do_pay_amount/5,[role_data:get_roleID(),Num,"","",0]});
		#data_pay{payID=PayID} ->
			gen_server:call(pay_server, {func, fun pay_server:do_pay/5, [role_data:get_roleID(), PayID, "", "",0]}),
			test_ok()
	end;
test("我是上帝") ->
	user_default:best_fire(role_data:get_roleID()),
	test_ok();
test("问鼎跳过") ->
    catch erlang:send(cross_server, to_master_change_to_next_status),
    test_ok();
test("华丽跳过") ->
    catch race_server:to_next(),
    test_ok();
test("华丽报名"++NS) ->
    Max = test_num(NS),
    catch race_server:test_sign_others(Max),
    test_ok();
test("刷新日常任务")->
    % 每日0时清除活跃度
    role_task:clean_daily_activity(),
	role_task:add_all_today_task(),
	test_ok();

%%add_buff结构{add_buff,徽章种子速度,金币种子速度,探险次数速度,体力恢复速度,竞技场次数速度,符文争夺次数速度,探险产出,3V3金币产出,战役产出,徽章种子产量,金币种子产量}
%%%%add_attr结构{add_attr,攻击力,血量,怒气初始,怒气剩余,暴击,暴击抵抗,命中,闪避,吸血,反弹,眩晕,眩晕抵抗,破甲,护甲,法穿,法抗,攻击万分比,生命万分比}
test("徽章种子速度"++NS) ->
	#role{familyID = FamilyID} = role_data:get_roleInfo(),
	user_default:test_familyTek_level(FamilyID,101,NS);
test("troom") -> role_trainingRoom:cs_trainingRoom_info(#cs_trainingRoom_info{}),test_ok();
test("troom"++Energy) -> role_trainingRoom:cs_trainingRoom_start_training(#cs_trainingRoom_start_training{energy=test_num(Energy)}),test_ok();
test("cw"++Type) -> role_box:cs_box_shop(#cs_box_shop{tab=24001,type=Type}),test_ok();
test(Str)->
	Result =  scanf(Str),
	io:format("~p~n",[Result]),
	case Result of
		["ItemID",ItemTypeID,Level,Rank,NumT]->
            Num = limit_num(NumT),
			case data_item:get(ItemTypeID) of
				?undefined->
					test_error();
				_->
					NewItem = #new_item{itemTypeID=ItemTypeID,itemNum=Num,itemRank=Rank,itemLevel=Level},
					item_lib:add_item_f([NewItem], ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok()
			end;
		["GerID",GerTypeID,Level,Rank,NumT]->
            Num = limit_num(NumT),
			case data_ger:get(GerTypeID) of
				?undefined->
					test_error();
				_->
					NewGer = lists:duplicate(Num, #new_ger{gerTypeID=GerTypeID,gerLevel=Level,gerQuality=Rank}),
					ger_lib:add_ger_list(NewGer, ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok()
			end;
		["homestead",1]->%%重置次数
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_times(RoleID),
			test_ok();
		["homestead",2,S]->%%缩短交配冷却时间
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_matingCoolSecond(RoleID,S),
			test_ok();
		["homestead",3,Num,S]->%%缩短种子成熟时间
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_machine_endSecond(RoleID,Num,S),
			test_ok();
		["homestead",4,Num,S]->%%缩短充能冷却世间
			RoleID = role_data:get_roleID(),
			role_homestead:test_refresh_machine_addEnergyEndS(RoleID,Num,S),
			test_ok();
	[Name,Level,Rank,NumT] ->
            Num = limit_num(NumT),
			case find_name(Name) of
				false ->
					test_error();
				{item, ItemTypeID} ->
					NewItem = #new_item{itemTypeID=ItemTypeID,itemNum=Num,itemRank=Rank,itemLevel=Level},
					item_lib:add_item_f([NewItem], ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok();
				{ger, GerTypeID} ->
					NewGer = lists:duplicate(Num, #new_ger{gerTypeID=GerTypeID,gerLevel=Level,gerQuality=Rank}),
					ger_lib:add_ger_list(NewGer, ?MONEY_ADD_TYPE_GM_CMD, 0, ""),
					test_ok()
			end;
		_ ->
			test_error()
	end.

limit_num(NumT) ->
    case NumT > 9999 of
        true ->
            9999;
        false ->
            NumT
    end.

scanf(Str) ->
	util:foldl(fun({E,F}, _Acc) ->
						   case io_lib:fread(E, Str) of
							   {ok,List,_} ->
								   {return,F(List)};
							   _ ->
								   next
						   end
				   end, 0, ?MATCH_LIST).

test_error() ->
ErrorMsg = 
"元宝100
绑定元宝100
银两100
声望100
体力刷新
等级20
vip10
方天画戟 100级19阶2把
吕布 60级10阶2头
曹仁魂魄 20个
鬼头骰（一） 1000个
充值60(充值数字必须为充值商店中的元宝数字)
",
	?sendself(#sc_message_test{result=2,errorMsg=ErrorMsg}).

test_ok()->
	?sendself(#sc_message_test{result=1,errorMsg=""}).

find_name(Name) ->
	case find_item(Name) of
		false ->
			find_ger(Name);
		Result ->
			Result
	end.

find_item(Name) ->
	ItemTypeID = 
	util:foldl(fun(E,Acc) ->
		case data_item:get(E) of
			#data_item{itemName=Name,itemTypeID=ID} ->
				{return, ID};
			_ ->
				Acc
		end
	end, 0, data_item:get_list()),
	if ItemTypeID ==0 ->
		false;
		true ->
		{item, ItemTypeID}
	end.

find_ger(Name) ->
	GerTypeID = 
	util:foldl(fun(E,Acc) ->
		case data_ger:get(E) of
			#data_ger{gerName=Name,gerTypeID=ID} ->
				{return, ID};
			_ ->
				Acc
		end
	end, 0, data_ger:get_list()),
	if GerTypeID ==0 ->
		false;
		true ->
		{ger, GerTypeID}
	end.
	
t() ->
	[find_name("方天画戟"),
	find_name("吕布"),
scanf("曹操 1阶1级"),
scanf("方天画戟 2阶2把")].
	
	



