-module(role_trainerProf).

-compile(export_all).

-include("def_role.hrl").
-include("def_battle.hrl").
%%====================================================
%%考虑到每一种训练师只有一个等级值，故所有类型的训练师值全部使用一个64位值保存,10进制的2位表示一个类型的等级，一共能够表示18个类型
%%====================================================
-define(PROFTYPELIST,[?FIGHT_TRAINER_TYPE,?REAR_TRAINER_TYPE,?COORDINATE_TRAINER_TYPE]).

-define(BASEVALUE,100).              

-define(trainerProf,trainerProf).

-define(UPLEVEL_FAIL_PRECONDITION,1). %%前置条件不满足
-define(UPLEVEL_FAIL_MATERIAL,2).     %%材料不足

init_trainerProf(Prof) when is_integer(Prof)->	
	TrainerProfInfo = [init_trainerProf_by_type(Type,Prof)||Type<-?PROFTYPELIST],
	set_role_trainerProf(TrainerProfInfo).

init_trainerProf_by_type(Type,Prof)->	
	Value = (Prof rem trunc(math:pow(?BASEVALUE,Type))) div trunc((math:pow(?BASEVALUE,Type-1))),
	#trainer_prof{type=Type,level=Value}.

%%====================================协议处理===========================================
cs_trainerProf_info(#cs_trainerProf_info{})->
	PTrainerUnitL = [trainerProf2pTrainerUnit(get_role_trainerProf(E))||E<-?PROFTYPELIST],
	?sendself(#sc_trainerProf_info{prof=PTrainerUnitL}).

cs_trainerProf_uplevel(#cs_trainerProf_uplevel{type=Type})->
	case check_trainerProf_uplevel(Type) of
		{false,Reason}->
			?sendself(#sc_trainerProf_uplevel{result=Reason,type=Type});
		{true,TrainerProf,Cost}->
			%%道具的判断放到实际的升级过程中
			case do_trainerProf_uplevel(TrainerProf,Cost) of
				{false,Reason}->
					?sendself(#sc_trainerProf_uplevel{result=Reason,type=Type});
				{true,NewLevel,NewTrainerProf}->
					set_role_trainerProf(NewTrainerProf),
					role_trainerRear:refresh_machine(Type),
					?sendself(#sc_trainerProf_uplevel{result=1,newLevel=NewLevel,type=Type})
			end
	end.
%%===============================internal functions======================================
set_role_trainerProf(L) when is_list(L)->
	[set_role_trainerProf(E)||E<-L];
set_role_trainerProf(#trainer_prof{type=Type}=TP)->
	put({?trainerProf,Type},TP).

get_role_trainerProf(Type)->
	get({?trainerProf,Type}).

trainerProf2persistform(L)->
	trainerProf2persistform2(L,0).
trainerProf2persistform2([],Acc)->
	Acc;
trainerProf2persistform2([#trainer_prof{type=Type,level=Level}|T],Acc)->
	NewAcc = trunc(math:pow(?BASEVALUE,Type-1)*Level)+Acc,
	trainerProf2persistform2(T,NewAcc).

check_trainerProf_uplevel(Type)->
	case get_role_trainerProf(Type) of
		?undefined->
			{false,5};
		#trainer_prof{type=Type,level=Level}=TrainerProf->
			case check_trainer_prof_maxLevel_limit(Type,Level) of
				false->
					{false,2};
				true->
					case check_trainerProf_uplevel_condition(Type,Level) of
						false->	
							{false,3};
						{true,Cost}->
							{true,TrainerProf,Cost}
					end
			end
	end.

check_trainer_prof_maxLevel_limit(_Type,Level)->
	Level < data_trainerProf:get(prof_max_level).

check_trainerProf_uplevel_condition(Type,Level)->
	{PreConditionL,Cost} = data_trainerProf:get({data_trainerProf_uplevel,Type,Level+1}),
	case PreConditionL of
		[]->
			{true,Cost};
		_ ->
			case check_trainerProf_uplevel_condition2(PreConditionL,true) of
				true->
					{true,Cost};
				R ->
					R
			end
	end.

check_trainerProf_uplevel_condition2(_L,false)->
	false;
check_trainerProf_uplevel_condition2([],Result)->
	Result;
check_trainerProf_uplevel_condition2([H|T],Result)->
	SingleResult = check_trainerProf_uplevel_single_condition(H),
	check_trainerProf_uplevel_condition2(T,Result andalso SingleResult).

%%此处沿用一下主线任务和七日活动定义的任务类型，方便以后如果可能会公用
check_trainerProf_uplevel_single_condition({?ROLE_LEVEL_N,Value})->
	#role{level=Level} = role_data:get_roleInfo(),
	Level >= Value;
%%FIX 370版本使用的战役还是旧的，当前的这个函数在410以后需要重新修改
%%4.1版本修改新的战役判断
check_trainerProf_uplevel_single_condition({?BATTLE_FINISH,Value})->
	% CurDungeonID = case role_battle:get_progress(?BATTLE_DUNGEON_TYPE_NORMAL) of
	% 	31001->
	% 		31000;
	% 	X->
	% 		X
	% end,
	% ChapterID = role_battle:caculate_chapterID_by_dungeonID(CurDungeonID),
	% #role{roleID=RoleID} = role_data:get_roleInfo(),
	% #chapter{dungeonList=FinishDungeonList} = role_battle:get_chapter(RoleID,ChapterID),
	% #data_chapter{dungeonIDList=ConfigDungeonList} = data_chapter:get(ChapterID),
	% RealChapterID = case length(FinishDungeonList) =:= length(ConfigDungeonList) of true->ChapterID+1;false->ChapterID end,
	% RealChapterID >= Value;
	#role_xbattle{chapterID=ChapterIDNow}=role_data:get_xbattle_data(),
	% #xbattle_chapter{passDungeons=D}=ChapterData=role_xbattle:get_chapter(ChapterIDNow),
	% case data_xbattle_chapter:get(ChapterIDNow) of
	% 	?undefined->
	% 		false;
	% 	#data_xbattle_chapter{dungeonCount=DungeonCount}->
	% 		case DungeonCount=:=length(D) of
	%     		true->
	%     			ChapterNum = erlang:max(ChapterIDNow,0);
	%     		false->
	%         		ChapterNum = erlang:max(ChapterIDNow-1,0)
	% 		end,
	% 		ChapterNum > Value
	% end;
	ChapterIDNow >= Value;
check_trainerProf_uplevel_single_condition({?ACTIVATE_TALENT,Value})->
	TalentList = role_data:get_trainer_info(),
	case lists:keyfind(Value,1,TalentList) of
		false->
			false;
		_->
			true
	end;
check_trainerProf_uplevel_single_condition({?FIGHTPOWER,Value})->
	#role{fightPower=FightPower} = role_data:get_roleInfo(),
	FightPower >= Value;
check_trainerProf_uplevel_single_condition({?GER_OWN_STAR_N_QUALITY_N_ISMEGA_TN,NeedStarList,NeedQuality,IsNeedMega,NeedNum})->
	PosList = role_data:get_posList(),
	LPosList = role_data:get_lieuposList(),
	GerBag = role_data:get_gerBag(),
	F = fun(Ger,AccNum) ->
			{GerTypeID,GerQuality} = case Ger of
				#ger{gerBase=#gerBase{gerTypeID=GerTypeID1,gerQuality=GerQuality1}}->
					{GerTypeID1,GerQuality1};
				#gerSimple{gerTypeID=GerTypeID1,gerQuality=GerQuality1}->
					{GerTypeID1,GerQuality1}
			end,
			#data_ger{gerStar=Star} = data_ger:get(GerTypeID),
			case util:is_exp_card(GerTypeID) of
				true->
					AccNum;
				false->
					case IsNeedMega of
						ignore->
							case lists:member(Star,NeedStarList) andalso GerQuality >= NeedQuality of
								true->
									AccNum+1;
								false->
									AccNum
							end;
						_ ->
							IsGerMega = role_awake:check_ger_mega(GerTypeID),
							case IsNeedMega=:=IsGerMega andalso lists:member(Star,NeedStarList) andalso GerQuality >= NeedQuality of
								true->
									AccNum+1;
								false->
									AccNum
							end	
					end	
			end
		end,
	lists:foldl(F,0,PosList++LPosList++GerBag) >= NeedNum;
check_trainerProf_uplevel_single_condition(T)->
	?ERR("undefined Condition:~w ~n",[T]),
	false.

do_trainerProf_uplevel(#trainer_prof{type=Type,level=Level}=TrainerProf,Cost)->
	case role_item:delete_sell_reward(Cost,[],[]) of
		false->
		    {false,4};
		{true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
		    % #role{roleID=RoleID} = role_data:get_roleInfo(),
		    NewLevel = Level+1,
		    role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_TRAINERPROF,Type,integer_to_list(NewLevel)),
		    {true,NewLevel,TrainerProf#trainer_prof{level=NewLevel}}
    end.

get_all_trainerProf()->
	[get_role_trainerProf(E)||E<-?PROFTYPELIST].

get_all_trainerProf_Persist()->
	L = get_all_trainerProf(),
	trainerProf2persistform(L).

trainerProf2pTrainerUnit(#trainer_prof{type=Type,level=Level})->
	#p_trainerProf_unit{type=Type,level=Level}.



%%===========================战斗训练师==========================================
cs_trainerProf_battle_info(_) ->
    Data = role_data:get_trainerProf_battle_data(),
    ?sendself(#sc_trainerProf_battle_info{battleInfo=Data}).

cs_trainerProf_battle_uplevel(#cs_trainerProf_battle_uplevel{tagID=TagID}) ->
    case check_battle_uplevel(TagID)of
        {true,Unit,Data,BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
            do_battle_uplevel(Unit,Data,BagOther2, DelAcc, UpdateAcc, UpdateLogList);
        {false,Reason} ->
            ?sendself(#sc_trainerProf_battle_uplevel{result=Reason,tagID=0})
    end.

cs_trainerProf_battle_unclock(#cs_trainerProf_battle_unclock{tagID=TagID}) ->
    case check_battle_unclock(TagID) of
        {true,Unit,Data} ->
            do_battle_unclock(TagID,Unit,Data);
        {false,Reason} ->
            ?sendself(#sc_trainerProf_battle_unclock{result=Reason,tagID=0})
    end.

do_battle_unclock(TagID,Unit,Data) ->
    role_data:set_trainerProf_battle_data([Unit|Data]),
    ?sendself(#sc_trainerProf_battle_unclock{result=1,tagID=TagID}).

check_battle_unclock(TagID) ->
    Data = role_data:get_trainerProf_battle_data(),
    case get_role_trainerProf(?FIGHT_TRAINER_TYPE) of
        ?undefined -> {false,2};
        #trainer_prof{level=ProfLevel} ->
            case lists:keyfind(TagID, #p_trainerProf_battle_unit.tagID, Data) of
                false ->
                    {NeedProfLevel,NextLevel,_,_,_}=data_trainer_battle:get({cost,TagID,0}),
                    if ProfLevel < NeedProfLevel -> {false,2};
                       true -> 
                           {true,#p_trainerProf_battle_unit{tagID=TagID,level=NextLevel},Data}
                    end;
                _-> {false,4}
            end
    end.

do_battle_uplevel(Unit=#p_trainerProf_battle_unit{tagID=TagID,level=Level},Data,BagOther2, DelList, UpdateAcc, UpdateLogList) ->
    role_data:set_bagItem(BagOther2),
    RoleID = role_data:get_roleID(),
    LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
    
    case UpdateAcc =/= [] of
        true ->
            UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
            ?sendself(#sc_item_update{updateList=UpdateList});
        _ ->         ignore
    end,
    case DelList =/= [] of
        true ->
            DelItemIDList = [E||#item{itemUID=E}<-DelList],
            ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList});
        _ ->          ignore
    end,
    
    {Date, _} = Time = erlang:localtime(),
    behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_TRAINER_BATTLE_UPLEVEL, TagID, integer_to_list(Level)),
    role_data:set_trainerProf_battle_data([Unit|Data]),
    ?sendself(#sc_trainerProf_battle_uplevel{result=1,tagID=TagID}).

get_trainer_battle_rank() ->
    case get_role_trainerProf(?FIGHT_TRAINER_TYPE) of
        ?undefined-> 0;
        #trainer_prof{level=ProfLevel}-> ProfLevel
    end.

check_battle_uplevel(TagID) ->
    Data = role_data:get_trainerProf_battle_data(),
    %#data_trainer_battle{needProfLevel=NPLevel,maxLevel=MaxLevel,cost={CostType,CostNum}} = data_trainer_battle:get(TagID),
    case get_role_trainerProf(?FIGHT_TRAINER_TYPE) of
        ?undefined-> {false,2};
        #trainer_prof{level=ProfLevel}->
            case lists:keytake(TagID, #p_trainerProf_battle_unit.tagID, Data) of
                false ->
                    {NeedProfLevel,NextLevel,_,_,{CostType,CostNum}}=data_trainer_battle:get({cost,TagID,0}),
                    if ProfLevel < NeedProfLevel -> {false,2};
                       true -> 
                           case item_lib:check_material(CostType,CostNum) of
                               false -> {false,3};
                               {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
                                   {true,#p_trainerProf_battle_unit{tagID=TagID,level=NextLevel},Data,BagOther2, DelAcc, UpdateAcc, UpdateLogList}
                           end
                    end;
                {value,#p_trainerProf_battle_unit{level=UnLevel}=Unit,OtherData} ->
                    {NeedProfLevel,NextLevel,_,_,{CostType,CostNum}}=data_trainer_battle:get({cost,TagID,UnLevel}),
                    if ProfLevel < NeedProfLevel -> {false,2};
                       true ->
                           if NextLevel == 0  -> {false,4};
                              true ->
                                  case item_lib:check_material(CostType,CostNum) of
                                      false -> {false,3};
                                      {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList} ->
                                          {true,Unit#p_trainerProf_battle_unit{level=NextLevel},OtherData,BagOther2, DelAcc, UpdateAcc, UpdateLogList}
                                  end
                           end
                    end
            end
    end.
    

    