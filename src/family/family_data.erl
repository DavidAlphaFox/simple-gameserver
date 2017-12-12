%% @author lixinglong
%% @doc @todo Add description to family_data.


-module(family_data).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_family.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================

init_family_extra_data(FamilyID)->
	set_familyID(FamilyID),
	init_family_tek(FamilyID),
	TekIDList = [TekID||#p_familyTekDtl{tekID=TekID} <-get_family_tek()],
	init_family_wallet(TekIDList),
	init_family_constribute_list(FamilyID).
%	init_family_storage(FamilyID).

dump_family_extra_data(FamilyID)->
%	ok.
	dump_family_tek(FamilyID),
	dump_family_wallet(FamilyID),
	dump_family_constribute_list(FamilyID).
%	dump_family_storage(FamilyID).

init_family_wallet(WalletList)->
	lists:map(fun(ID)->
		ItemList = db_sql:get_family_wallet(get_familyID(),ID),
		set_family_wallet(ID,ItemList)
	end,WalletList).

%%初始化公会的捐献排行列表并保存到进程字典中
init_family_constribute_list(FamilyID) ->
	case FamilyID >0 of
		true ->
			Family_Contribute_list = db_sql:get_family_contribute_list(FamilyID),
			set_family_contribute_list(Family_Contribute_list);
		false ->
			set_family_contribute_list([])
	end.

%%持久化公会的捐献排行列表
dump_family_constribute_list(FamilyID)->
	case get_family_contribute_list() of
		[] ->
			?INFO("当前公会: ~w 捐献排行列表为空~n",[FamilyID]);
		Family_Contribute_list ->
			db_sql:set_family_contribute_list(FamilyID,Family_Contribute_list)
	end.	

dump_family_wallet(FamilyID)->
	WalletList = [TekID||#p_familyTekDtl{tekID=TekID} <-get_family_tek()],
	%?ERR("TekID :~w",[WalletList]),
	lists:foreach(fun(WalleID)->
		db_sql:set_family_wallet(get_familyID(),WalleID,get_family_wallet(WalleID))
	end,WalletList).

%%向进程字典中加入公会捐献排行列表
set_family_contribute_list(Family_Contribute_list) ->
	put(?FAMILY_CONTRIBUTE_LIST,Family_Contribute_list).

get_family_contribute_list()->
	case get(?FAMILY_CONTRIBUTE_LIST) of
		?undefined ->
			[];
		Family_Contribute_list ->
			Family_Contribute_list
	end.

get_family_wallet()->
	WalletList = [TekID||#p_familyTekDtl{tekID=TekID} <-get_family_tek()],
	lists:foldl(fun(WalletID,Acc)->
		[get_family_wallet(WalletID)|Acc]
	end,[],WalletList).
get_family_wallet(ID)->
	WalletID = list_to_atom(atom_to_list(?family_wallet)++integer_to_list(ID)),
	%?ERR("ID :~w,WalletID ~w",[ID,WalletID]),
	case get(WalletID) of
		?undefined ->
			#sc_familyTek_wallet{wallet= #p_reward_info2{coin =0,roleExp=0,gerExp=0,gold = 0,itemList=[],reputation=0,gerList=[]}};
		Wallet ->
			Wallet
	end.

set_family_wallet(ID,Wallet) ->
	put(list_to_atom(atom_to_list(?family_wallet)++integer_to_list(ID)),Wallet),
	TekList = get_family_tek(),
	case lists:keytake(ID,#p_familyTekDtl.tekID,TekList) of
		{value,TekInfo,Other} ->
			#sc_familyTek_wallet{wallet = WalletInfo} = Wallet,
			TekInfo2 = TekInfo#p_familyTekDtl{tekWalletinfo=WalletInfo},
			NewFamilyTek = [TekInfo2|Other],
			set_family_tek(NewFamilyTek);
		false->
			?ERR("设置的科技资源不在公会当前科技中：ID: ~w  TekList ~w ~n",[ID,TekList])
	end.
		

check_material(ItemTypeID, Num) ->
	case role_lib:check_material2(get_family_storage(), ItemTypeID, Num) of
		{BagOther2, 0, DelAcc, UpdateAcc, UpdateLogList} ->
			{true, BagOther2, DelAcc, UpdateAcc, UpdateLogList};
		_ ->
			false
	end.

init_family_tek(FamilyID)->
	FamilyTek = db_sql:get_family_tek(FamilyID),
	TekInfoList = lists:map(fun(TekInfo) ->
		#p_familyTekDtl{tekID=TekID,tekLevel=TekLevel} = TekInfo,
		{TekID,TekLevel}
	end,FamilyTek),
	ets:insert(?ETS_FAMILY_TECHNOLOGY,{FamilyID,TekInfoList}),
	set_family_tek(FamilyTek).
dump_family_tek(FamilyID) ->
	db_sql:set_family_tek(FamilyID,get_family_tek()).

set_family_tek(Tek)->
	% ?ERR("set_family_tek ~w~n",[Tek]),
	put(?family_tek,Tek).

get_family_tek()->
	case get(?family_tek) of
		?undefined ->
			default_family_tek();
		Tek ->
			Tek
	end.

default_family_tek()->
	TekIDList = data_family:get(tekIDList),
	% ?ERR("TekList ~w",[TekIDList]),
	[ begin
		ID2 = ID*1000,
		{Type,UnlockLevel,Level2,Cost,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank, ID2}),
		#p_familyTekDtl{tekID=ID2,tekType=Type,tekLevel=Level2,tekWalletinfo=#p_reward_info2{coin=0,roleExp=0,gerExp=0,gold=0,reputation=0,itemList=[],gerList=[]},tekFinish = 0}
      end||ID<-TekIDList].

get_familyID()->
	get(?familyID).
set_familyID(ID) when is_integer(ID)->
	put(?familyID,ID).

init_family_storage(FamilyID)->
	Storage = db_sql:get_family_storage(FamilyID),
	set_family_storage(Storage).
dump_family_storage(FamilyID)->
	db_sql:set_family_storage(FamilyID,get_family_storage()).

get_family_storage()->
	case get(?family_storage) of
		?undefined ->
			default_family_storage();
		Storage ->
			Storage
	end.

%set_family_storage(List|{Len,List})
set_family_storage(Storage) when is_list(Storage)->
	put(?family_storage,{erlang:length(Storage),Storage});
set_family_storage({Len,Storage}=S) when is_integer(Len) andalso is_list(Storage) ->
	put(?family_storage,S).

default_family_storage()->
	{0,[]}.
get_familytek_cost(TekID,TekLevel)->
     case lists:member(TekID, data_family_technology:get(tekIDList)) of
     	true ->
     		ID2 = TekID*1000+TekLevel,
     		{Type,UnlockLevel,Level2,Cost,Add_buff,Add_attr} = data_family_technology:get({data_technology_rank, ID2}),
     		Cost;
     	false->
     		{sell_reward, 0, 0, 0, 0, [], 0, []}
     end.

%初始化公会副本信息
init_instance_boss(Members) ->
    FirstInstId = lists:nth(1, data_family:get(family_instance_id_list)),
    InstBoss = #family_instance_state{is_win = false
                          ,next_instance = FirstInstId
                          ,inst_list = [{FirstInstId,?F_INST_OPEN}]
                          ,cur_inst_boss = []
                          ,fight_member_times = []
                          ,reward_get_status = []
                          ,extra_reward_is_get = []
                          ,damage_list=[]},
    refresh_instance_boss(InstBoss,Members).
%更新公会副本信息
refresh_instance_boss(FamilyInstanceInfo,Members)->
    InstanceId = FamilyInstanceInfo#family_instance_state.next_instance,
    BossConfig = case data_family:get({family_instance,InstanceId}) of
        ?undefined ->
            ?ERR("refresh_instance_boss InstanceId(~w) is wrong",[InstanceId]),
            InstanceIdList = data_family:get(family_instance_id_list),
            data_family:get({family_instance,lists:nth(1, InstanceIdList)});
        Value ->
            Value
    end,
    NewBossList = lists:foldl(fun(BossTeamConfig,AccList)->
                                {MaxHp,GerList} = lists:foldl(fun({GerTypeID,GerLevel,BossQuality,Pos},{AccHp,AccList2})-> 
                                            Boss0 = ger_attr:new_ger(GerTypeID, GerLevel, BossQuality, [], []),
                                            Boss = Boss0#ger{gerBase=((Boss0#ger.gerBase)#gerBase{gerPos=Pos})},
                                            {AccHp+Boss#ger.gerHp,[Boss|AccList2]}
                                        end , {0,[]}, BossTeamConfig),
                                [{inst_boss_info,length(AccList)+1,MaxHp,MaxHp,GerList,0}|AccList]
                              end, [], BossConfig),
    NewInstStateList0 = [{E,?F_INST_OPEN}||{E,_}<-FamilyInstanceInfo#family_instance_state.inst_list], %先得到一个待初始化的列表
    NewInstStateList = lists:keyreplace(InstanceId, 1, NewInstStateList0,{InstanceId,?F_INST_FIGHT}),
    RewardList = data_family:get({family_instance_drop,InstanceId}),
    BoxNum = data_family:get(family_instance_box_num),
    % TempReward = data_family:get(test_family_instance_reward),
    #family_instance_state{is_win = false
                          ,next_instance = InstanceId
                          ,inst_list = NewInstStateList
                          ,cur_inst_boss = NewBossList
                          ,fight_member_times = []
                          ,reward_get_status = [{Index,0,util:random_one_from_weigh_list(RewardList),""}||Index<-lists:seq(1, BoxNum)]
                          ,extra_reward_is_get = []
                          ,damage_list=[]}.
