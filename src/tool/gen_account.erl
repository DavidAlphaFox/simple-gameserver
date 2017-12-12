%% @author caohongyang 
%% @doc 生成帐号，根据配置data_gen_account.config
%% Created 2013-4-12


-module(gen_account).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-define(patchIDList, get(patchIDList)).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_config() ->
	random:seed(erlang:now()),
	catch ets:delete(roleName),
	db_sql:delete_all_android(),
	AllRoleNameList = db_sql:search_all_roleName(),
	ets:new(roleName, [public, set, named_table]),
	lists:foreach(fun(RoleName)->
						  ets:insert(roleName, {RoleName,1})
				  end, AllRoleNameList),
%% 	init_patchIDList(),
	AllGer = all_ger(),
	AllItem = all_item(),
	case file:consult(filename:join([tk_config:root_dir(),"config/moduleconfig/data_gen_account.config"])) of
		{ok, TermList} ->
			{RoleList, _GerList} = 
				lists:foldl(fun(E, Acc) ->  
									gen_account(E, Acc, AllGer, AllItem)  
							end, {[],[]}, TermList),
			PvpServerDict = gen_pvp_list(RoleList),
			persist_pvp(PvpServerDict),
            persist_plunder(),
            ?ERR("get_config Creat ~w Robot",[length(RoleList)]);
		_ ->
			exit("please give a config named \"data_gen_account.config\"\n")
	end,
	ok.

get_config_without_pvp() ->
    random:seed(erlang:now()),
    catch ets:delete(roleName),
    db_sql:delete_all_android(),
    AllRoleNameList = db_sql:search_all_roleName(),
    ets:new(roleName, [public, set, named_table]),
    lists:foreach(fun(RoleName)->
                          ets:insert(roleName, {RoleName,1})
                  end, AllRoleNameList),
    AllGer = all_ger(),
    AllItem = all_item(),
    case file:consult(filename:join([tk_config:root_dir(),"config/moduleconfig/data_gen_account.config"])) of
        {ok, TermList} ->
            {_RoleList, _GerList} = 
                lists:foldl(fun(E, Acc) ->  
                                    gen_account(E, Acc, AllGer, AllItem)  
                            end, {[],[]}, TermList),
%%             PvpServerDict = gen_pvp_list(RoleList),
%%             persist_pvp(PvpServerDict),
            persist_plunder(),
            ?ERR("get_config_without_pvp Creat ~w Robot",[length(_RoleList)]);
        _ ->
            exit("please give a config named \"data_gen_account.config\"\n")
    end,
    ok.

init_patchIDList() ->
	List = role_item:all_patch_typeID(),
	put(patchIDList, List).

gen_pvp_list(RoleList) ->
	Dict=
	lists:foldl(fun({Rank, Role}, Acc) ->
						[{Rank, Role#role.roleID, Role#role.title, Rank}|Acc]
				end, [], RoleList),
	Dict.

persist_pvp(PvpServerList) ->
	case whereis(pvp_server) of
		?undefined->
			db_sql:set_etc(?DB_ETC_KEY_PVP, PvpServerList);
		Pid->
			Pid ! {init_data,PvpServerList}
	end.

persist_plunder() ->
%% 下面这个不能在这个时候写数据库，由于前面有大量异步数据库操作，此时再操作很容易失败。
%% PlunderRobot数据由plunder_server时保存数据库，这点数据量瞬间就搞定
%%     db_sql:set_etc(?DB_ETC_KEY_PLUNDER_ROBOT, PlunderRobot), %% 这个数据不需要变化，仅在此处保存即可
%% 又发现问题，前面并发创建机器人有可能有些机器人是没有创建出来的，后面逻辑去读取数据库的时候就会失败。
%% 现在采用plunder_server直接给根据规则去数据库中筛选机器人玩家。
	case whereis(plunder_server) of
		?undefined->
            ignore;
		Pid-> 
            %% 如果plunder_server服务器已经启动，则通知其机器人数据初始化，更新其进程字典。
            %% 三分钟后，plunder_server检索数据库得到机器人列表
            erlang:send_after(3*60*1000, Pid, init_robot_data)
	end.

gen_account({{Rank1, Rank2},{GerNum,GerStar,GerLevel,GerQuality}}, Acc, AllGer, _AllItem) ->
	GerDict = get_star_ger(GerStar, AllGer),
%% 	ItemDict  = get_star_item(EquipStar, AllItem),
	ItemDict = {[],[],[],[],[],[]},
	EquipLevel = 1,
	gen_account(Rank1, Rank2, GerLevel, EquipLevel, Acc, GerDict, ItemDict,GerNum,GerQuality).

gen_account(Rank2, Rank2, GerLevel, EquipLevel, Acc, GerDict, ItemDict,GerNum,GerQuality) ->
	gen_account(Rank2, GerLevel, EquipLevel, Acc, GerDict, ItemDict,GerNum,GerQuality);
gen_account(Rank1, Rank2, GerLevel, EquipLevel, Acc, GerDict, ItemDict,GerNum,GerQuality) ->
	Acc2 = gen_account(Rank1, GerLevel, EquipLevel, Acc, GerDict, ItemDict,GerNum,GerQuality),
	gen_account(Rank1+1, Rank2, GerLevel, EquipLevel, Acc2, GerDict, ItemDict,GerNum,GerQuality).

gen_account(Rank, GerLevel, EquipLevel, Acc, GerDict, ItemDict,GerNum,GerQuality) ->
	RoleID = gen_roleID(Rank),
	GerList = random_ger_list(GerDict, GerNum, GerLevel, EquipLevel, ItemDict,GerQuality),
	Accid= accid(Rank),
	Sex = random_sex(),
	RoleName = gen_role_name(Sex),
	RoleLevel = lists:max([E#ger.gerBase#gerBase.gerLevel||E<-GerList]),
	RoleExp = data_role_level:get(RoleLevel),
	Title = cacl_title(Rank, RoleLevel),	
	FightPower = lists:sum([E#ger.gerAttr#gerAttr.gerFightPower||E<-GerList]),
	RoleInfo = #role{roleID=RoleID,
					 accid=Accid
					,roleName=RoleName
					,title=Title
					,level=RoleLevel
					,exp=RoleExp
					,fightPower=FightPower
					,familyID=0
                    ,lastJoinFamily=0
					,coin=0
					,description=""
					,gold=0
					,goldBonus=0
					,goldTotalPaid=0
					,goldUsed=0
					,isMale=Sex
					,lastLogoutTime=0
					,reputation=0
					,vipLevel=0
					},
%% 	if Rank ==1 ->
%% 		   io:format("~1000p",[RoleInfo]);
%% 	   true ->
%% 		   ignore
%% 	end,
	{RoleAcc, GerAcc} = Acc,
	GerInfo = {RoleID, GerList},
    %%如何在合服处理中则不在这里写数据
    case erlang:get(?is_doing_merge) of
        ?undefined ->
%%             spawn(fun() ->persist(RoleInfo, GerInfo) end);
            persist(RoleInfo, GerInfo);
        _ ->
            next
    end, 
	{[{Rank,RoleInfo}|RoleAcc], [GerInfo|GerAcc]}.

persist(RoleInfo, {RoleID, GerList}) ->
	PosList = GerList,
	case db_sql:create_roleInfo(RoleInfo) of
		{ok, _} ->
			ok;
		ERR ->
			io:format("~1000p\n",[ERR])
	end,
	%io:format("\`ger..\n"),
	#role{isMale=IsMale,level=Level} = RoleInfo,
	TrID = data_trSpecial:get({defaultTrID,IsMale}),
	TrSpecial = #trSpecial{trID=TrID,specialID=0,roleLevel=Level},
	db_sql:set_fighterList(RoleID, GerList,[],0,0,TrSpecial,#add_attr{}),
	db_sql:set_gerList(RoleID, PosList, [],[]),
	ok.

cacl_title(Rank, Level) ->
	Config = data_title:get(title_condition),
	pvp_server:cacl_title(Level, Rank, Config).
	
random_ger_list(GerDict, GerNum, GerLevel, _EquipLevel, _ItemDict,GerQuality) ->
	DataGerList = random_list(GerDict, GerNum),
	GerIDList = [ID||#data_ger{gerTypeID=ID} <- DataGerList],
	GerList = 
		[begin
%% 			 EquipList = random_equip(EquipLevel, ItemDict),
%% 			 Ger = ger_attr:new_ger(ID, gen_ger_level(GerLevel), 0, EquipList, lists:delete(ID, GerIDList)),
			 Ger = ger_attr:new_ger(ID, gen_ger_level(GerLevel), GerQuality, [], lists:delete(ID, GerIDList)),
			 Ger#ger{gerBase=(Ger#ger.gerBase)#gerBase{}}
		 end || ID <- GerIDList],
%% 	GerList2 = lists:sort(fun(A,B) -> A#ger.gerHp > B#ger.gerHp end, GerList),
	Max = erlang:max(6, GerNum),
	PosList = util:random_list2(lists:seq(1, Max)),
	lists:reverse(element(2,lists:foldl(fun(Ger,{PL, Acc}) ->
												[Seq|TPL] = PL,
												{TPL, 
												 [
												  Ger#ger{gerBase=(Ger#ger.gerBase)#gerBase{gerPos=Seq}}
												 |Acc]
												}
										end, {PosList, []}, GerList))).

	
	
random_equip(EquipLevel, {List1,List2,List3,List4,List5,List6}) ->
	[begin
		 #data_item{itemTypeID=ItemTypeID,itemRank=ItemRank} = 
					   DataItem = random_one_from_list(E),
		 NewItem = #new_item{itemLevel=EquipLevel,
							 itemNum=1,
							 itemRank=ItemRank,
							 itemTypeID=ItemTypeID
							},
		 Equip = hd(item_lib:create_equip(NewItem, DataItem)),
		 Equip#item{itemPos=role_item:get_pos(Equip#item.itemType)}
	 end || E<-[List1,List2,List3,List4,List5,List6]].
	
gen_roleID(Rank) ->
	tk_id:rank_roleID(Rank).

%% 这个目前接口仅用于机器人
all_item() ->
%% 	List = [data_item:get(ID) || ID <- data_item:get_list()],
%% 	{[E||#data_item{itemType=weapon} = E<- List],
%% 	 [E||#data_item{itemType=armor} =E <- List],
%% 	 [E||#data_item{itemType=wing} =E <- List]},
	lists:foldl(fun(ID,{WeaponList,ArmorList,WingList,HeadwearList,TotemList,RunestoneList}=Acc)->
						#data_item{itemType=ItemType} = Item = data_item:get(ID),
						case ItemType of
							?weapon->
								{[Item|WeaponList],ArmorList,WingList,HeadwearList,TotemList,RunestoneList};
							?armor->
								{WeaponList,[Item|ArmorList],WingList,HeadwearList,TotemList,RunestoneList};
							?wing->
								{WeaponList,ArmorList,[Item|WingList],HeadwearList,TotemList,RunestoneList};
							?headwear->
								{WeaponList,ArmorList,WingList,[Item|HeadwearList],TotemList,RunestoneList};
							?totem->
								{WeaponList,ArmorList,WingList,HeadwearList,[Item|TotemList],RunestoneList};
							?runestone->
								{WeaponList,ArmorList,WingList,HeadwearList,TotemList,[Item|RunestoneList]};
							_->
								Acc
						end
				end, {[],[],[],[],[],[]}, data_item:get_list()).

get_star_item(Star, {ItemList1,ItemList2,ItemList3,ItemList4,ItemList5,ItemList6}) ->
	{[E||#data_item{itemStar=ItemStar}=E<-ItemList1, ItemStar=:=Star],	 
	[E||#data_item{itemStar=ItemStar}=E<-ItemList2, ItemStar=:=Star],
	[E||#data_item{itemStar=ItemStar}=E<-ItemList3, ItemStar=:=Star],
	 [E||#data_item{itemStar=ItemStar}=E<-ItemList4, ItemStar=:=Star],
	 [E||#data_item{itemStar=ItemStar}=E<-ItemList5, ItemStar=:=Star],
	 [E||#data_item{itemStar=ItemStar}=E<-ItemList6, ItemStar=:=Star]}.

all_ger() ->
	[data_ger:get(ID) || ID<-data_ger:get_list(), ID < 20000].

get_star_ger(Star, GerList) ->
	[E||#data_ger{gerStar=GerStar, gerTypeID=GerTypeID}=E<-GerList, GerStar=:=Star, GerTypeID > 1000, GerTypeID < 10000, GerTypeID rem 10=:=0].

gen_ger_level(GerLevel) ->
	erlang:max(1,util:random_int(GerLevel-3, GerLevel+3)).

random_one_from_list(List) ->
	Value = random:uniform(length(List)),
	lists:nth(Value,List).

random_list(List, N) ->
	random_list(List, N, []).

random_list(_List, 0, Result) ->
	Result;
random_list(List, N, Result)->
	Select = random_one_from_list(List),
	random_list(lists:delete(Select,List), N-1, [Select|Result]).
	
random_sex() ->
	case random:uniform(2) of
		1 ->
			true;
		2 ->
			false
	end.

accid(Rank) ->
	Rank.

gen_role_name(Sex) ->
	RandomName = tk_misc:random_role_name(Sex),
	case ets:member(roleName, RandomName) of
		true ->
			gen_role_name(Sex);
		false ->
			ets:insert(roleName, {RandomName,1}),
			RandomName
	end.