-module(role_buddy).

-compile(export_all).

-include("def_role.hrl").
-include("def_item.hrl").

%%============================================================================
%%伙伴系统依然采用旧的小伙伴保存方式，在此基础上加入额外的伙伴系统的数据统计信息
%%============================================================================

-define(buddy_buff,buddy_buff).
-define(buddy_total,buddy_total).

%%================================================协议处理 BEGIN=================================================
cs_buddy_partner_insert(#cs_buddy_partner_insert{insertGerID=InsertGerID,replaceGerID=ReplaceGerID,type=Type})->
	case do_cs_buddy_partner_insert_check(InsertGerID,ReplaceGerID,Type) of
		{false, Reason}->
			?sendself(#sc_buddy_partner_insert{result=Reason,type=Type,insertGerID=InsertGerID,replaceGerID=ReplaceGerID});
		{true,InsertGer,OtherGerBag,LieuGerList}->
			do_partner_insert(InsertGer,LieuGerList,true,Type),
			role_data:set_gerBag(OtherGerBag),
			?sendself(#sc_buddy_partner_insert{result=1,type=Type,insertGerID=InsertGerID,replaceGerID=ReplaceGerID});
		{true,InsertGer,ReplaceGer,OtherGerBag,OtherLieuGerList,IsNeedRecacFormationDinesty}->
			NewGerBag = do_partner_back_to_bag(ReplaceGer,OtherGerBag),
			do_partner_insert(InsertGer,OtherLieuGerList,IsNeedRecacFormationDinesty,Type),
			role_data:set_gerBag(NewGerBag),
			?sendself(#sc_buddy_partner_insert{result=1,type=Type,insertGerID=InsertGerID,replaceGerID=ReplaceGerID})
	end.

cs_buddy_partner_insert_batch(#cs_buddy_partner_insert_batch{type=Type,insertGerIDList=InsertGerIDList})->
	case do_cs_buddy_partner_insert_batch_check(Type,InsertGerIDList) of
		{false,Reason}->
			?sendself(#sc_buddy_partner_insert_batch{result=Reason,type=Type,insertGerIDList=InsertGerIDList});
		{true,RealInsertGerList,LieuGerList,GerBag}->
			do_partner_insert_batch(RealInsertGerList,LieuGerList,Type),
			role_data:set_gerBag(GerBag),
			RealInsertGerIDList = [GerID||#gerSimple{gerID=GerID}<-RealInsertGerList],
			?sendself(#sc_buddy_partner_insert_batch{result=1,type=Type,insertGerIDList=RealInsertGerIDList})
	end.

cs_buddy_partner_remove(#cs_buddy_partner_remove{type=Type,gerID=GerID})->
	case do_cs_buddy_partner_remove(Type,GerID) of
		{false,Reason}->
			?sendself(#sc_buddy_partner_remove{result=Reason,gerID=GerID});
		{true,RemoveGer,OtherLieuGerList}->
			GerBag = role_data:get_gerBag(),
			role_data:set_lieuposList(OtherLieuGerList),
			NewGerBag = do_partner_back_to_bag(RemoveGer,GerBag),
			role_data:set_gerBag(NewGerBag),
			?sendself(#sc_buddy_partner_remove{result=1,gerID=GerID})		
	end.
%%================================================协议处理 END===================================================

%%============================================internel functions begin===========================================
do_cs_buddy_partner_insert_check(InsertGerID,0,Type)->
	case check_buddy_type(Type) of
		false->
			{false,6};
		true->
			case role_data:get_ger(InsertGerID) of
				{value, InsertGer,PosList,LPosList,GerBag2,bag}->
					case check_ger_property(InsertGer#gerSimple.gerTypeID,Type) of
						false->
							{false,6};
						true->
							#camp_unit{gernum=CurrentNum,germaxnum=GerMaxNum} = get_buddy_buff_for_type(Type),
							case CurrentNum < GerMaxNum of
								true->
									%%修改同类型只判断所有营地中的精灵
									case check_gerlist_same_type(LPosList,InsertGer#gerSimple.gerTypeID) of
										false->
											{true,InsertGer,GerBag2,LPosList};
										true->
											{false,5}
									end;
								false->
									{false,4}
							end
					end;
				_->
					{false,2}
			end
	end;
do_cs_buddy_partner_insert_check(InsertGerID,ReplaceGerID,Type)->
	case check_buddy_type(Type) of
		false->
			{false,6};
		true->
			case role_data:get_ger(InsertGerID) of
				{value, #gerSimple{gerTypeID=InsertGerTypeID}=InsertGer,PosList,_LPosList,GerBag2,bag}->
					case role_data:get_ger(ReplaceGerID) of
						{value, #ger{gerBase=#gerBase{gerTypeID=ReplaceGerTypeID}}=ReplaceGer,PosList,LPosList2,_GerBag,lieu}->
							case check_ger_property(InsertGerTypeID,Type) andalso check_ger_property(ReplaceGerTypeID,Type) of
								false->
									{false,6};
								true->
									#camp_unit{gernum=CurrentNum,germaxnum=GerMaxNum} = get_buddy_buff_for_type(Type),
									case CurrentNum =< GerMaxNum of
										true->
										    %%修改同类型只判断所有营地中的精灵
											case check_gerlist_same_type(LPosList2,InsertGer#gerSimple.gerTypeID) of
												false->
													{true,InsertGer,ReplaceGer,GerBag2,LPosList2,not role_ger:check_same_type2(InsertGerTypeID,ReplaceGerTypeID)};
												true->
													{false,5}
											end;
										false->
											{false,4}
									end
							end;
						_->
							{false,3}
					end;
				_->
					{false,2}
			end
	end.

do_cs_buddy_partner_insert_batch_check(_Type,[])->
	{false,6};
do_cs_buddy_partner_insert_batch_check(Type,InsertGerIDList)->
	case check_buddy_type(Type) of
		false->
			{false,2};
		true->
			case check_ger_exist_and_check_type(InsertGerIDList,Type) of
				{false,Reason}->
					{false,Reason};
				{true,InsertGerList,OtherGerBag}->
					LieuGerList = role_data:get_lieuposList(),
					%%修改同类型只判断所有营地中的精灵
					case check_ger_same_type_for_batch(InsertGerList,LieuGerList,[]) of
						true->
							{false,7};
						false->
							#camp_unit{gernum=CurrentNum,germaxnum=GerMaxNum} = get_buddy_buff_for_type(Type),
							case erlang:min(GerMaxNum-CurrentNum,length(InsertGerIDList)) of
								0->
									{false,5};
								RealInsertNum->
									{RealInsertGerList,RemainGerBag} = sort_and_filter_insert_ger(InsertGerList,RealInsertNum,OtherGerBag),
									LieuGerList = role_data:get_lieuposList(),
									{true,RealInsertGerList,LieuGerList,RemainGerBag}
							end
					end
			end
	end.			

do_cs_buddy_partner_remove(Type,GerID)->
	case check_buddy_type(Type) of
		false->
			{false,2};
		true->
			LieuGerList = role_data:get_lieuposList(),
			case lists:keytake(GerID,#ger.gerID,LieuGerList) of
				false->
					{false,3};
				{_,#ger{gerBase=GerBase} = TarGer,OtherLieuGerList}->
					case check_ger_property(GerBase#gerBase.gerTypeID,Type) of
						false->
							{false,2};
						true->
							{true,TarGer,OtherLieuGerList}
					end
			end
	end.

do_partner_insert_batch(RealInsertGerList,LieuGerList,Type)->
	LieuGerList2 = lists:foldl(fun(InsertGer,Acc)->
		InsertGer2 = ger_attr:recacl_lieu(InsertGer#gerSimple{gerPos=Type},Acc),
		[InsertGer2|Acc]
	end,LieuGerList,RealInsertGerList),
	role_data:set_lieuposList(LieuGerList2),
	role_ger:calc_destiny_num().

%%检查对应的精灵ID是否在背包精灵中存在，并且类型符合
check_ger_exist_and_check_type(InsertGerIDList,Type)->
	GerBag = role_data:get_gerBag(),
	check_ger_exist_and_check_type(InsertGerIDList,GerBag,[],Type).

check_ger_exist_and_check_type([],RemainGerBag,FindGerList,_Type)->
	{true,FindGerList,RemainGerBag};
check_ger_exist_and_check_type([H|T],RemainGerBag,FindGerList,Type)->
	case lists:keytake(H,#gerSimple.gerID,RemainGerBag) of
		false->
			{false,3};
		{_,#gerSimple{gerTypeID=GerTypeID}=F,Other}->
			case check_ger_property(GerTypeID,Type) of
				true->
					check_ger_exist_and_check_type(T,Other,[F|FindGerList],Type);
				false->
					{false,4}
			end
	end.

check_ger_same_type_for_batch(InsertGerList,LieuGerList,PosGerList)->
	lists:any(fun(#gerSimple{gerTypeID=TGerTypeID}=Ger)-> check_gerlist_same_type(lists:delete(Ger,InsertGerList),TGerTypeID) end,InsertGerList) orelse
	lists:any(fun(#gerSimple{gerTypeID=TGerTypeID})-> check_gerlist_same_type(PosGerList,TGerTypeID) end,InsertGerList) orelse
	lists:any(fun(#gerSimple{gerTypeID=TGerTypeID})-> check_gerlist_same_type(LieuGerList,TGerTypeID) end,InsertGerList).



sort_and_filter_insert_ger(InsertGerList,RealInsertNum,OtherGerBag)->
	case length(InsertGerList) of
		RealInsertNum->
			{InsertGerList,OtherGerBag};
		Len->
			InsertGerList2 = lists:reverse(InsertGerList),
			{lists:sublist(InsertGerList2,RealInsertNum),OtherGerBag++lists:sublist(InsertGerList2,RealInsertNum+1,Len-RealInsertNum)}
	end.



check_buddy_type(Type)->
	lists:member(Type,?ENCHANT_TYPE).

check_ger_property(GerTypeID,Type)->
	case data_ger:get(GerTypeID) of
		#data_ger{gerProperty=Type}->
			true;
		_->
			false
	end.

%%检查GerTypeID是否和List中的精灵是相同精灵（闪光和非闪光是同类）
check_gerlist_same_type(List,GerTypeID)->
	GerTypeIDList = lists:foldl(fun(Ger,Acc)->
		case Ger of
			#ger{gerBase=#gerBase{gerTypeID=TGerTypeID}}->
				[TGerTypeID|Acc];
			#gerSimple{gerTypeID=TGerTypeID}->
				[TGerTypeID|Acc]
		end
	end,[],List),
	lists:any(fun(TGerTypeID)->	role_ger:check_same_type2(TGerTypeID,GerTypeID) end,GerTypeIDList).

%%由于伙伴之间不会有天命的激活，不对天命进行重新计算
do_partner_insert(InsertGer,LieuGerList,IsNeedRecacFormationDinesty,Type)->
	InsertGer2 = ger_attr:recacl_lieu(InsertGer#gerSimple{gerPos=Type},LieuGerList),
	LPosList = [InsertGer2|LieuGerList],	
	role_data:set_lieuposList(LPosList),
	case IsNeedRecacFormationDinesty of
		true->
			role_ger:calc_destiny_num();
		_->
			ignore
	end.

%%将小伙伴放回到背包中，需要修改gerPos位置，并且将ger转换成gerSimple
do_partner_back_to_bag(#ger{gerBase=GerBase} = ReplaceGer,OtherGerBag)->
	ReplaceGer1 = ReplaceGer#ger{gerBase=GerBase#gerBase{gerPos=0}},
	ReplaceSimpleGer = ger_lib:ger2gerSimple(ReplaceGer1),
	[ReplaceSimpleGer|OtherGerBag].

do_partner_back_to_bag_batch([],GerBagAcc)->
	GerBagAcc;
do_partner_back_to_bag_batch([H|T],GerBag)->
	NewGerBag = do_partner_back_to_bag(H,GerBag),
	do_partner_back_to_bag_batch(T,NewGerBag).

%%=============================================internel functions end============================================
%%calc_role_buddy/0函数需要使用小伙伴列表，需要在设置了小伙伴列表之后调用
calc_role_buddy()->
	LieuGerList = role_data:get_lieuposList(),
	#role{level=RoleLevel} = role_data:get_roleInfo(),
	calc_role_buddy(LieuGerList,RoleLevel).

calc_role_buddy(LieuGerList,RoleLevel)->
	{TotalNormalAdd,TotalSpecialAdd} = lists:foldl(fun(Type,{{AtkAddAcc,HpAddAcc},SpecialAddAcc})->
		{{{AtkAdd,HpAdd}=NormalAdd,SpecialAdd},Num} = calculate_role_buddy_type_buff(Type,LieuGerList),
		set_buddy_buff_for_type(#camp_unit{camp_type=Type,gernum=Num,normaladd=NormalAdd,specialadd=SpecialAdd,germaxnum=get_buddy_slot_num(RoleLevel)}),
		{{AtkAddAcc+AtkAdd,HpAddAcc+HpAdd},ger_attr:append_add_attr(SpecialAdd,SpecialAddAcc)}
	end,{{0,0},#add_attr{}},?ENCHANT_TYPE),
	set_buddy_buff_for_total(#camp_add{totalnormaladd=TotalNormalAdd,totalspecialadd=TotalSpecialAdd}),
    ok.

calculate_role_buddy_type_buff(Type,LieuGerList)->
	TypeLieuGerList = lists:foldl(fun(#ger{gerBase=#gerBase{gerTypeID=GerTypeID}}=Ger,Acc)->
		case data_ger:get(GerTypeID) of
			#data_ger{gerProperty=Type}->
				[Ger|Acc];
			_->
				Acc
		end
	end,[],LieuGerList),
	{calculate_buddy_buff(TypeLieuGerList,Type),length(TypeLieuGerList)}.

calculate_buddy_buff(LieuGerList,Type)->
	calculate_buddy_buff2(LieuGerList,Type,{{0,0},0}).
calculate_buddy_buff2([],Type,{NormalAdd,SpecialValueAcc})->
	case data_partner:get({special_add,Type}) of
		?undefined->
			{NormalAdd,#add_attr{}};
		{MaxSpecialBuff,FullNeed}->
			{NormalAdd,fix_add_attr(MaxSpecialBuff,SpecialValueAcc,FullNeed)}
	end;
calculate_buddy_buff2([H|T],Type,{{AtkAddAcc,HpAddAcc},SpecialRateAcc})->
	#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerQuality=GerQuality,gerLevel=GerLevel}} = H,
	#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
	NormalAddList = data_partner:get(GerStar),
	{AtkAdd,HpAdd} = case lists:keyfind(GerQuality,1,NormalAddList) of
		false->
			{0,0};
		{_,AtkAdd1,HpAdd1}->
			BlinkGerEffectRate = calculateGerBlinkEffectRate(GerTypeID),
			{trunc(AtkAdd1*BlinkGerEffectRate),trunc(HpAdd1*BlinkGerEffectRate)}
	end,
	SpecialValueAdd = calculate_ger_buddy_special_value(GerTypeID,GerStar,GerQuality,GerLevel),
	calculate_buddy_buff2(T,Type,{{AtkAddAcc+AtkAdd,HpAddAcc+HpAdd},SpecialRateAcc+SpecialValueAdd}).

fix_add_attr(_MaxAddAttr,0,_FullNeed)->
	#add_attr{};
fix_add_attr(MaxAddAttr,Rate,FullNeed)->
	Len = tuple_size(MaxAddAttr),
	RealRate = Rate/FullNeed,
	lists:foldl(fun(Index,Acc)->
		case element(Index,Acc) of
			0->
				Acc;
			Vaule->
				?ERR("~w ",[Acc]),
				setelement(Index,Acc,trunc(RealRate*Vaule))
		end
	end,MaxAddAttr,lists:seq(2,Len)).

calculate_ger_buddy_special_value(GerTypeID,GerStar,GerQuality,GerLevel)->
	BaseValue = data_partner:get({baseValue,GerStar}),
	GerQualityEffectRate = calculateGerQualityEffectRate(GerStar,GerQuality),
	GerLevelEffectRate = calculateGerLevelEffectRate(GerStar,GerLevel),
	BlinkGerEffectRate = calculateGerBlinkEffectRate(GerTypeID),
	BaseValue*BlinkGerEffectRate*(GerQualityEffectRate+GerLevelEffectRate).

calculateGerQualityEffectRate(GerStar,GerQuality) when GerStar >= 7 ->
	data_partner:get({niubi,GerQuality});
calculateGerQualityEffectRate(_GerStar,GerQuality)->
	data_partner:get({cuobi,GerQuality}).

calculateGerLevelEffectRate(_GerStar,GerLevel)->
	LevelField = GerLevel div 10,
	data_partner:get({levelField,LevelField}).

calculateGerBlinkEffectRate(GerTypeID)->
	case ger_lib:is_blink_ger(GerTypeID) of
		true->
			data_partner:get(blink_plus);
		false->
			data_partner:get(normal)
	end.

set_buddy_buff_for_type(#camp_unit{camp_type=Type}=Unit)->
	put({?buddy_buff,Type},Unit).

get_buddy_buff_for_type(Type)->
	get({?buddy_buff,Type}).

set_buddy_buff_for_total(TotalCamp)->
	put(?buddy_total,TotalCamp).

get_buddy_buff_for_total()->
	get(?buddy_total).

get_buddy_special_buff_for_total()->
	case get_buddy_buff_for_total() of
		?undefined->
			#add_attr{};
		#camp_add{totalspecialadd=TotalSpecialAdd}->
			TotalSpecialAdd
	end.
get_buddy_normal_buff_for_total()->
	case get_buddy_buff_for_total() of
		?undefined->
			{0,0};
		#camp_add{totalnormaladd=TotalNormalAdd}->
			TotalNormalAdd
	end.

get_buddy_slot_num(RoleLevel)->
	case data_partner:get(slot_num_list) of
		?undefined->
			0;
		L ->
			F = fun({Level,_})-> RoleLevel >= Level end, 
			{_,Num} = util:fun_find(F,L),
			Num
	end.

fresh_buddy_slot_num(RoleLevel)->
	NewSlotNum = get_buddy_slot_num(RoleLevel),
	%%由于所有的营地的数量都是相同的，故只需要获取一个营地当前的最大数量即可
	#camp_unit{germaxnum=OldGerMaxNum} = get_buddy_buff_for_type(?ENCHANT_TYPE_WATER),
	case NewSlotNum > OldGerMaxNum of
		true->
			[update_buddy_slot_num(Type,NewSlotNum)||Type<-?ENCHANT_TYPE];
		false->
			ignore
	end.

update_buddy_slot_num(Type,NewSlotNum)->
	CampUnit = get_buddy_buff_for_type(Type),
	set_buddy_buff_for_type(CampUnit#camp_unit{germaxnum=NewSlotNum}).

get_lieuList_by_property(Type)->
	LieuGerList = role_data:get_lieuposList(),
	[Ger||#ger{gerBase=#gerBase{gerPos=Type1}}=Ger<-LieuGerList,Type1=:=Type].