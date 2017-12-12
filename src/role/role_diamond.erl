%%神器技能宝石模块
%%需求问题：（1）技能宝石需要做到堆叠
%%          （2）技能宝石需要进行穿戴
%%          （3）不会出现技能宝石的精炼和升级
%%针对上面两个问题将技能宝石设计如下：
%%          （1）技能宝石设计成能堆叠的类型
%%          （2）在精灵属性中增加一个技能宝石列表，其中保存当前精灵穿戴的技能宝石信息
-module(role_diamond).
-compile(export_all).
-include("def_role.hrl").

-define(ROLE_LEVEL_CONDITION,1).                              %%玩家等级条件
-define(GER_QUALITY_CONDITION,2).                             %%承载神器精灵的品阶条件
-define(SACRIFICES_CONDITION,3).                              %%献祭条件

%%协议处理=================================================
%%技能宝石合成
cs_diamond_combine(#cs_diamond_combine{sourceDiamondID=SourceDiamondID,desNum=DesNum})->
	case do_cs_diamond_combine(SourceDiamondID,DesNum) of
		{false,Reason}->
			?sendself(#sc_diamond_combine{result=Reason});
		true->
			?sendself(#sc_diamond_combine{result=1})
	end.

%%技能宝石穿戴
cs_diamond_equip(#cs_diamond_equip{gerUID=GerID,diamondID=DiamondID,pos=Pos})->
	case{is_open(),do_cs_diamond_equip(GerID,DiamondID,Pos)} of
		{false,_}->
			?sendself(#sc_diamond_equip{result=0,gerUID=GerID,diamondID=DiamondID,diamondPos=Pos});
		{true,{false,Reason}}->
			?sendself(#sc_diamond_equip{result=Reason,gerUID=GerID,diamondID=DiamondID,diamondPos=Pos});
		{true,{true,#ger{gerBase=#gerBase{gerPos=GerPos}}}}->
			?sendself(#sc_diamond_equip{result=1,gerUID=GerID,gerPos=GerPos,diamondID=DiamondID,diamondPos=Pos})
	end.

%%卸下：特定精灵特定位置的特定宝石
cs_diamond_demount(#cs_diamond_demount{gerUID=GerID,diamondID=DiamondID,pos=Pos})->
	case {is_open(),do_cs_diamond_demount(GerID,DiamondID,Pos)} of
		{false,_}->
			?sendself(#sc_diamond_demount{result=0,gerUID=GerID,diamondID=DiamondID,diamondPos=Pos});
		{true,{false,Reason}}->
			?sendself(#sc_diamond_demount{result=Reason,gerUID=GerID,diamondID=DiamondID,diamondPos=Pos});
		{true,{true,#ger{gerBase=#gerBase{gerPos=GerPos}}}}->
			?sendself(#sc_diamond_demount{result=1,gerUID=GerID,gerPos=GerPos,diamondID=DiamondID,diamondPos=Pos})
	end.

%%一键卸下， 卸下所有上阵精灵具有某类特殊宝石
cs_diamond_demount_multi(#cs_diamond_demount_multi{diamondID=DiamondID})->
	case {is_open(),do_cs_diamond_demount_multi(DiamondID)} of
		{false,_}->
			?sendself(#sc_diamond_demount_multi{result=0,demountLocationList=[]});
		{true,{false,Reason}}->
			?sendself(#sc_diamond_demount_multi{result=Reason,demountLocationList=[]});
		{true,{true,DemountDiamondList}}->
			?sendself(#sc_diamond_demount_multi{result=1,demountLocationList=DemountDiamondList})
	end.

%%查看精灵的神器信息，目前的设定，神器里面对应的插槽信息直接通过宝石信息能够获取，故此处目前不发送
cs_diamond_holygrail_info(#cs_diamond_holygrail_info{gerUID=GerID})->
	case {is_open(),do_cs_diamond_holygrail_info(GerID)} of
		{false,_}->
			?sendself(#sc_diamond_holygrail_info{result=0});
		{true,{false,Reason}}->
			?sendself(#sc_diamond_holygrail_info{result=Reason});
		{true,{true,HolyGrailLevel,IsFinishSacrifice}}->
			?sendself(#sc_diamond_holygrail_info{result=1,holyGrailLevel=HolyGrailLevel,isFinishSacrifice=IsFinishSacrifice})
	end.

%%精灵献祭
cs_diamond_holygrail_sacrifice(#cs_diamond_holygrail_sacrifice{sourceGerUID=SGerID,foodGerUID=FGerID})->
	case {is_open(),do_cs_diamond_holygrail_sacrifice(SGerID,FGerID)} of
		{false,_}->
			?sendself(#sc_diamond_holygrail_sacrifice{result=0});
		{true,{false,Reason}}->
			?sendself(#sc_diamond_holygrail_sacrifice{result=Reason});
		{true,{true,_NewGer}}->
			?sendself(#sc_diamond_holygrail_sacrifice{result=1})
	end.

%%精灵神器升级
cs_diamond_holygrail_uplevel(#cs_diamond_holygrail_uplevel{gerUID=GerID})->
	case {is_open(),do_cs_diamond_holygrail_uplevel(GerID)} of
		{false,_}->
			?sendself(#sc_diamond_holygrail_uplevel{result=0,gerUID=GerID});
		{true,{false,Reason}}->
			?sendself(#sc_diamond_holygrail_uplevel{result=Reason,gerUID=GerID});
		{true,{true,NewHolyGrailLevel}}->
			?sendself(#sc_diamond_holygrail_uplevel{result=1,gerUID=GerID,holyGrailLevel=NewHolyGrailLevel})
	end.

%%升级精灵神器等级
do_cs_diamond_holygrail_uplevel(GerID)->
	case role_data:get_ger(GerID) of
		{value,Ger,PosList2,_LPosList,_GerBag,ger}->
			case do_cs_diamond_holygrail_uplevel2(Ger) of
				{false,Reason}->
					{false,Reason};
				{NewGer,NewHolyGrailLevel}->
					%%刷新上阵精灵的属性
					NewGer2 = ger_attr:recacl(NewGer,PosList2),        
     				PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
      				role_data:set_posList(PosList4),
      				{true,NewHolyGrailLevel}
      		end;
		_->
			{false,2}
	end.

do_cs_diamond_holygrail_uplevel2(Ger)->
	#ger{gerBase=#gerBase{gerHolyGrailInfo=GerHolyGrailInfo}=GerBase,gerID=GerID} = Ger,
	#holyGrail{holyGrailLevel=HolyGrailLevel} = GerHolyGrailInfo,
	case HolyGrailLevel < data_diamond:get(maxHolyGrailLevel) of
		false->
			{false,3};
		true->
			NextHolyGrailLevel = HolyGrailLevel+1,
			case data_diamond:get({holyGrail,NextHolyGrailLevel}) of
				?undefined->
					{false,4};
				{ConditionList,_SlotNum,UpLevelCost,_AddAttr}->
					Role = role_data:get_roleInfo(),
					case check_holygrail_uplevel_condition(ConditionList,Ger,Role) of
						true->
							case role_item:delete_sell_reward(UpLevelCost,[],[GerID]) of
								false->
									{false,5};
								{true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
									%%删除消耗的道具
									role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_HOLYGRAIL_UPLEVEL,NextHolyGrailLevel,integer_to_list(GerID)),
									%%更新精灵的神器信息
									NewGerHolyGrailInfo = GerHolyGrailInfo#holyGrail{holyGrailLevel=NextHolyGrailLevel,isFinishSacrifice=false},
									NewGerBase = GerBase#gerBase{gerHolyGrailInfo=NewGerHolyGrailInfo},
									{Ger#ger{gerBase=NewGerBase},NextHolyGrailLevel}
							end;
						false->
							{false,4}
					end
			end
	end.

do_cs_diamond_holygrail_sacrifice(SGerID,FGerID)->
	case role_data:get_ger(SGerID) of
		{value,SGer,PosList2,_LPosList,_GerBag,ger}->
			case role_data:get_ger(FGerID) of
				{value,FGer,_PosList,_LPosList,GerBag2,bag}->
					case do_cs_diamond_holygrail_sacrifice2(SGer,FGer,GerBag2) of
						{true,NewGer}->
							%%此处只是修改gerBase里面的是否献祭标志，对精灵的各种属性没有任何更改，所以不对精灵的属性进行重新计算
							% NewGer2 = ger_attr:recacl(NewGer,PosList2),        
     						% PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
      						role_data:set_posList([NewGer|PosList2]),
							{true,NewGer};
						{false,Reason}->
							{false,Reason}
					end;
				_->
					{false,3}
			end;
		_->
			{false,2}
	end.

do_cs_diamond_holygrail_sacrifice2(SGer,FGer,GerBag)->
	#ger{gerBase=#gerBase{gerHolyGrailInfo=#holyGrail{holyGrailLevel=HolyGrailLevel,isFinishSacrifice=IsFinishSacrifice}=GerHolyGrailInfo,gerTypeID=GerTypeID}=GerBase} = SGer,
	case IsFinishSacrifice of
		true->
			{false,5};
		_ ->
			case data_diamond:get({holyGrail,HolyGrailLevel+1}) of
				?undefined->
					{false,6};
				{ConditionList,_SlotNum,_UpLevelCost,_AddAttr}->
					case lists:keyfind(?SACRIFICES_CONDITION,1,ConditionList) of
						false->
							?ERR("条件列表中没有献祭条件:~w 直接完成献祭任务,不更新背包精灵~n",[ConditionList]),
							NewGerHolyGrailInfo = GerHolyGrailInfo#holyGrail{isFinishSacrifice=true},
							{true,SGer#ger{gerBase=GerBase#gerBase{gerHolyGrailInfo=NewGerHolyGrailInfo}}};
						%%直接需求同类型精灵的品阶
						{?SACRIFICES_CONDITION,NeedGerQuality}->
							case check_sacrifices_condition(FGer,NeedGerQuality,GerTypeID) of
								false->
									{false,4};
								true->
									%%更新玩家精灵背包
									role_data:set_gerBag(GerBag),
									%%添加精灵消耗日志
									#role{roleID=RoleID} = role_data:get_roleInfo(),
									{Date,_}=Time = erlang:localtime(),
									LogGerList = role_ger:gerList2logGerList([FGer]),
									ger_lib:ra_ger_delete([FGer]),
    								[erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
									behavior_ger_consume:log(RoleID, LogGerList, Date, Time,?MONEY_DEC_TYPE_HOLYGRAIL_SACRIFICES,0,integer_to_list(SGer#ger.gerID)),
									NewGerHolyGrailInfo = GerHolyGrailInfo#holyGrail{isFinishSacrifice=true},
									{true,SGer#ger{gerBase=GerBase#gerBase{gerHolyGrailInfo=NewGerHolyGrailInfo}}}
							end
					end
			end
	end.								




% message sc_diamond_holygrail_sacrifice[id=21614]{
% 	required          int8                result                =1;
% 	//0->关闭
% 	//1->成功
% 	//2->升级的精灵不存在
% 	//3->被献祭的精灵不存在
% 	//4->被献祭的精灵献祭条件不满足
% }

do_cs_diamond_holygrail_info(GerID)->
	case role_data:get_ger(GerID) of
		{value, Ger,_PosList2,_LPosList,_GerBag,ger}->
			#ger{gerBase=#gerBase{gerHolyGrailInfo=#holyGrail{holyGrailLevel=HolyGrailLevel,isFinishSacrifice=IsFinishSacrifice}}} = Ger,
			{true,HolyGrailLevel,IsFinishSacrifice};
		_->
			{false,2}
	end.

do_cs_diamond_demount_multi(DiamondID)->
	case data_item:get(DiamondID) of
		?undefined->
			{false,2};
		#data_item{itemType=ItemType}->
			case item_lib:is_skill_diamond(ItemType) of
				false->
					{false,2};
				true->
					do_cs_diamond_demount_multi2(DiamondID)
			end
	end.
do_cs_diamond_demount_multi2(DiamondID)->
	PosList = role_data:get_posList(),
	{DemountGerList,UnDemountGerList,DemountDiamondList} = lists:foldl(fun(Ger,{DGerAcc,UDGerAcc,DAcc})->
		#ger{gerBase=GerBase,gerID=GerID}=Ger,
		#gerBase{gerHolyGrailInfo=GerHolyGrailInfo,gerPos=GerPos} = GerBase,
		{DesDiamondList,RemainDiamond} = lists:foldl(fun(#diamond_unit{diamondID=EDiamondID}=E,{DUAcc,RAcc})->
			case EDiamondID=:=DiamondID of
				true->
					{[E|DUAcc],RAcc};
				false->
					{DUAcc,[E|RAcc]}
			end
		end,{[],[]},GerHolyGrailInfo#holyGrail.diamondInfo),
		case DesDiamondList of
			[]->
				{DGerAcc,[Ger|UDGerAcc],DAcc};
			_ ->
				NewGerBase = GerBase#gerBase{gerHolyGrailInfo=GerHolyGrailInfo#holyGrail{diamondInfo=RemainDiamond}},
				NewGer = Ger#ger{gerBase=NewGerBase},
				{[NewGer|DGerAcc],UDGerAcc,[{GerID,DesDiamondList,length(DesDiamondList),GerPos}|DAcc]}
		end
	end,{[],[],[]},PosList),
	%%更新上阵精灵数据
	case DemountGerList of
		[]->
			ignore;
		_ ->
			PosList2 = [ger_attr:recacl(Ger, lists:delete(Ger, PosList))||Ger<-DemountGerList],
			role_data:set_posList(PosList2++UnDemountGerList)
	end,
	{DemountDiamondLocationList,TotalDemountNum} = lists:foldl(fun({GerID,SingleDesDiamondList,Num,GerPos},{PDLAcc,TotalNum})->
		PDL = [to_p_diamond_location(GerID,E,GerPos)||E<-SingleDesDiamondList],
		{PDL++PDLAcc,TotalNum+Num}
	end,{[],0},DemountDiamondList),
	%%修改对应宝石背包中对应的数据
	ItemBag = role_data:get_bagItem(),
	{ItemBag2,UpdateAcc2,NewAcc2} = add_diamond_num_for_demount(DiamondID,TotalDemountNum,ItemBag),
	role_data:set_bagItem(ItemBag2),
	item_lib:ra_item_update_num(UpdateAcc2),
	item_lib:ra_item_new(NewAcc2),
	{true,DemountDiamondLocationList}.


%%处理函数================================================
do_cs_diamond_combine(SourceDiamondID,DesNum)->
	case is_open() of
		true->
			case data_diamond:get({diamond_compose,SourceDiamondID}) of
				{DesID,SourNum,CoinCost}->
					do_cs_diamond_combine2(DesID,SourceDiamondID,SourNum,CoinCost,DesNum);
				_->
					{false,2}
			end;
		_ ->
			{false,0}
	end.

do_cs_diamond_combine2(DesID,SourceDiamondID,SourNum,CoinCost,DesNum)->
	Role=#role{roleID=RoleID} = role_data:get_roleInfo(),
	case role_lib:check_money(Role,coin,CoinCost*DesNum) of
		false->
			{false,5};
		_ ->
			case item_lib:check_material2(SourceDiamondID,SourNum*DesNum) of
				false->
					{false,3};
				{true, BagOther, DelAcc, UpdateAcc, UpdateLogList}->
					role_data:set_bagItem(BagOther),
					%% 写道具消耗日志
					LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogList),
					{Date, _} = Time = erlang:localtime(),
					behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_DIAMOND_COMBINE, DesID, ""),
					%%提醒客户端更新和删除物品
					item_lib:ra_item_delete(DelAcc),
					item_lib:ra_item_update_num(UpdateAcc),
					%% 扣除金币
					Role2=role_lib:deduct_money_f(Role, coin, CoinCost*DesNum,?MONEY_DEC_TYPE_DIAMOND_COMBINE,0,""),
					%% 发放合成后的道具
					Reward = #sell_reward{item=[#new_item{itemTypeID=DesID,itemNum=DesNum,itemLevel=1,itemRank=0}]},
					role_reward:handle_sell_reward_f(Role2, Reward, ?MONEY_ADD_TYPE_DIAMOND_COMBINE, DesID, ""),
					true
			end
	end.

%%处理技能宝石穿戴
do_cs_diamond_equip(GerID,DiamondID,Pos)->
	case data_item:get(DiamondID) of
		?undefined->
			{false,3};
		#data_item{itemType=ItemType}->
			case item_lib:is_skill_diamond(ItemType) of
				false->
					{false,3};
				true->
					case role_data:get_ger(GerID) of
						{value, Ger,PosList2,_LPosList,_GerBag,ger}->
							case item_lib:check_material(DiamondID,1) of
								false->
									{false,3};
								_->
									case do_cs_diamond_equip2(Ger,DiamondID,Pos) of
										{false,Reason}->
											{false,Reason};
										{true,NewGer}->
											%%刷新换上宝石后的精灵的属性，并且修改上阵列表
											NewGer2 = ger_attr:recacl(NewGer,PosList2),        
      										PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
      										role_data:set_posList(PosList4),
      										{true,NewGer2}
      								end
							end;
						_->
							{false,2}
					end
			end
	end.
do_cs_diamond_equip2(Ger=#ger{gerBase=#gerBase{gerHolyGrailInfo=GerHolyGrailInfo,gerTypeID=GerTypeID}=GerBase},DiamondID,Pos)->
	case is_pos_limit(Ger,Pos) of
		true->
			{false,4};
		false->
			case check_diamond_type_satisfy(GerHolyGrailInfo#holyGrail.diamondInfo,GerTypeID,DiamondID,Pos) of
				{false,Reason}->
					{false,Reason};
				{NewGerDiamondInfo,DemountDiamondID}->
					NewGerBase = GerBase#gerBase{gerHolyGrailInfo=GerHolyGrailInfo#holyGrail{diamondInfo=NewGerDiamondInfo}},
					NewGer = Ger#ger{gerBase=NewGerBase},
					%%减少被穿戴宝石背包数量
					ItemBag = role_data:get_bagItem(),
					{ItemBag1,UpdateAcc1,DeleteAcc1} = deduct_diamond_num_for_equip(DiamondID,1,ItemBag),
				    {ItemBag2,UpdateAcc2,NewAcc2} = add_diamond_num_for_demount(DemountDiamondID,1,ItemBag1),
				    role_data:set_bagItem(ItemBag2),
 					item_lib:ra_item_delete(DeleteAcc1),
 					item_lib:ra_item_update_num(UpdateAcc1++UpdateAcc2),
 					item_lib:ra_item_new(NewAcc2),
 					{true,NewGer}
 			end
 	end.

do_cs_diamond_demount(GerID,DiamondID,Pos)->
	case role_data:get_ger(GerID) of
		{value, Ger,PosList2,_LPosList,_GerBag,ger}->
			case do_cs_diamond_demount2(Ger,DiamondID,Pos) of
				{false,Reason}->
					{false,Reason};
				{true,NewGer}->
					%%刷新卸下宝石后的精灵的属性，并且修改上阵列表
					NewGer2 = ger_attr:recacl(NewGer,PosList2),        
      				PosList3 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
      				role_data:set_posList(PosList3),
      				{true,NewGer2}
      		end;
		_->
			{false,2}
	end.
do_cs_diamond_demount2(Ger=#ger{gerBase=#gerBase{gerHolyGrailInfo=GerHolyGrailInfo}=GerBase},DiamondID,Pos)->
	case lists:keytake(Pos,#diamond_unit.pos,GerHolyGrailInfo#holyGrail.diamondInfo) of
		false->
			{false,3};
		{_Value,#diamond_unit{diamondID=DiamondID},Other}->
			NewGerBase = GerBase#gerBase{gerHolyGrailInfo=GerHolyGrailInfo#holyGrail{diamondInfo=Other}},
			NewGer = Ger#ger{gerBase=NewGerBase},
			%%增加背包中对应道具数量
			ItemBag = role_data:get_bagItem(),
			{ItemBag2,UpdateAcc2,NewAcc2} = add_diamond_num_for_demount(DiamondID,1,ItemBag),
			role_data:set_bagItem(ItemBag2),
 			item_lib:ra_item_update_num(UpdateAcc2),
 			item_lib:ra_item_new(NewAcc2),
 			{true,NewGer}
 	end.

%%此处该函数只用于技能宝石装备上之后修改背包中技能宝石的数量所用
deduct_diamond_num_for_equip(DiamondID,Num=1,ItemBag)->
	SortSameDiamondList = lists:sort(fun(A,B)->A#item.itemNum < B#item.itemNum end,ItemBag),
	case lists:keytake(DiamondID,#item.itemTypeID,SortSameDiamondList) of
		false->
			?ERR("技能宝石已经穿戴后，不能找到对应的宝石:~w Bag:~w ~n",[DiamondID,ItemBag]);
		{_Value,FindOne=#item{itemNum=OldItemNum},Other}->
			NewNum = OldItemNum - Num,
			FindOne1 = FindOne#item{itemNum=NewNum},
			if
			 	NewNum >0 ->
			 		{[FindOne1|Other],[FindOne1],[]};
			 	NewNum =:=0->
			 		case lists:keyfind(DiamondID,#item.itemTypeID,Other) of
			 			false->
			 				{[FindOne1|Other],[FindOne1],[]};
			 			_ ->
			 				{Other,[FindOne1],[]}
			 		end;
			 	true->
			 		?ERR("出现异常错误：DiamondID:~w Num:~w ItemBag:~w ~n",[DiamondID,Num,ItemBag])
			 end
	end.

%%卸载精灵身上的所有技能宝石，并且将技能宝石放到背包中
demount_all_diamond_for_ger(#ger{gerBase=#gerBase{gerHolyGrailInfo=#holyGrail{diamondInfo=DiamondInfo}=GerHolyGrail}=GerBase}=Ger)->
	DemountDiamondList = lists:foldl(fun(#diamond_unit{diamondID=DiamondID},Acc)->
		case lists:keytake(DiamondID,1,Acc) of
			false->
				[{DiamondID,1}|Acc];
			{_Value,{DiamondID,OldNum},Other}->
				[{DiamondID,OldNum+1}|Other]
		end
	end,[],DiamondInfo),
	case DemountDiamondList of
		[]->
			Ger;
		_->
			ItemBag = role_data:get_bagItem(),
			{ItemBag2,UpdateAcc2,NewAcc2} = lists:foldl(fun({DiamondID,Num},{BagAcc,UpdateAcc,NewAcc})->
				{ItemBag1,UpdateAcc1,NewAcc1} = add_diamond_num_for_demount(DiamondID,Num,BagAcc),
				{ItemBag1,UpdateAcc++UpdateAcc1,NewAcc++NewAcc1}
			end,{ItemBag,[],[]},DemountDiamondList),
			role_data:set_bagItem(ItemBag2),
			item_lib:ra_item_new(NewAcc2),
			item_lib:ra_item_update_num(UpdateAcc2),
			NewGerBase = GerBase#gerBase{gerHolyGrailInfo=GerHolyGrail#holyGrail{diamondInfo=[]}},
			Ger#ger{gerBase=NewGerBase}
	end.
	
%%此处该函数只处理技能宝石从精灵身上卸载下来放到背包中的情况
add_diamond_num_for_demount(-1,_Num,ItemBag)->
	{ItemBag,[],[]};
add_diamond_num_for_demount(_,0,ItemBag)->
	{ItemBag,[],[]};
add_diamond_num_for_demount(DemountDiamondID,Num,ItemBag)->
	SortSameDiamondList = lists:sort(fun(A,B)->A#item.itemNum < B#item.itemNum end,ItemBag),
	case lists:keyfind(DemountDiamondID,#item.itemTypeID,SortSameDiamondList) of
		false->
			?ERR("从精灵身上卸载下技能宝石，未能在背包中发现存在的宝石DiamondID:~w ItemBag:~w ~n",[DemountDiamondID,ItemBag]),
			{ItemBag1,NewAcc1,UpdateAcc1,_AddLog} = item_lib:add_other_f(#new_item{itemTypeID=DemountDiamondID,itemNum=Num,itemRank=0,itemLevel=1},SortSameDiamondList,data_item:get(DemountDiamondID)),
			{ItemBag1,UpdateAcc1,NewAcc1};
		_->
			{ItemBag1,NewAcc1,UpdateAcc1,_AddLog} = item_lib:add_other_f(#new_item{itemTypeID=DemountDiamondID,itemNum=Num,itemRank=0,itemLevel=1},SortSameDiamondList,data_item:get(DemountDiamondID)),
			{ItemBag1,UpdateAcc1,NewAcc1}
	end.

%%检查被献祭的精灵是否满足献祭条件
check_sacrifices_condition(FGer,NeedGerQuality,NeedGerTypeID)->
	case ger_lib:is_blink_ger(NeedGerTypeID) of
		false->
			check_sacrifices_condition2(FGer,NeedGerQuality,NeedGerTypeID);
		true->
			case data_ger:get(NeedGerTypeID) of
				#data_ger{sameType=SameType}->
					check_sacrifices_condition2(FGer,NeedGerQuality,SameType);
				_->
					?ERR("undefined gerTypeID:~w ~n",[NeedGerTypeID]),
					false
			end
	end.
check_sacrifices_condition2(FGer,NeedGerQuality,NeedGerTypeID)->
	#gerSimple{gerTypeID=GerTypeID,gerQuality=GerQuality} = FGer,
	NeedGerQuality =:= GerQuality andalso GerTypeID=:= NeedGerTypeID.

%%根据精灵自身的神器等级判断是否对应的插槽开启
is_pos_limit(Ger,Pos)->
	#ger{gerBase=#gerBase{gerHolyGrailInfo=GerHolyGrail}}=Ger,
	case data_diamond:get({holyGrail,GerHolyGrail#holyGrail.holyGrailLevel}) of
		?undefined->
			true;
		{_ConditionList,SlotNum,_UpLevelCost,_AddAttr}->
			Pos>=SlotNum andalso Pos >=0
	end.

%%判断对应的精灵是否能够穿戴该技能宝石
check_diamond_type_satisfy(GerDiamondInfo,GerTypeID,DiamondID,Pos)->
	case check_special_diamond_satisfy(GerTypeID,DiamondID) of
		true->
			case is_exist_same_diamond_type(GerDiamondInfo,DiamondID,Pos) of
				false->
					case lists:keytake(Pos,#diamond_unit.pos,GerDiamondInfo) of
						false->
							{[#diamond_unit{pos=Pos,diamondID=DiamondID}|GerDiamondInfo],-1};
						{_Value,#diamond_unit{diamondID=DemountDiamondID}=FindOne,Other}->
							{[FindOne#diamond_unit{diamondID=DiamondID}|Other],DemountDiamondID}
					end;
				true->
					{false,5}
			end;
		false->
			{false,6}
	end.

check_holygrail_uplevel_condition(ConditionList,Ger,Role) when is_list(ConditionList)->
	lists:all(fun(Condition)->
		check_single_holygrail_uplevel_condition(Condition,Ger,Role)
	end,ConditionList);
check_holygrail_uplevel_condition(_ConditionList,_Ger,_Role)->
	false.

check_single_holygrail_uplevel_condition({?ROLE_LEVEL_CONDITION,NeedRoleLevel},_Ger,#role{level=RoleLevel})->
	RoleLevel >= NeedRoleLevel;
check_single_holygrail_uplevel_condition({?GER_QUALITY_CONDITION,NeedGerQuality},#ger{gerBase=#gerBase{gerQuality=GerQuality}},_Role)->
	GerQuality >= NeedGerQuality;
check_single_holygrail_uplevel_condition({?SACRIFICES_CONDITION,_NeedFoodGerQuality},
		#ger{gerBase=#gerBase{gerHolyGrailInfo=#holyGrail{isFinishSacrifice=IsFinishSacrifice}}},_Role)->
	IsFinishSacrifice.

%%判断对应技能宝石是否是精灵专属宝石
check_special_diamond_satisfy(GerTypeID,DiamondID)->
	#data_item{itemType=ItemType} = data_item:get(DiamondID),
	case item_lib:is_special_diamond(ItemType) of
		false->
			true;
		true->
			case data_diamond_skill:get({DiamondID,GerTypeID}) of
				?undefined->
					false;
				_ ->
					%%专属技能宝石
					true
			end
	end.

is_exist_same_diamond_type(GerDiamondInfo,DiamondID,Pos)->
	OtherDiamondList = case lists:keytake(Pos,#diamond_unit.pos,GerDiamondInfo) of
		false->
			GerDiamondInfo;
		{_Value,_Find,Other}->
			Other
	end,
	#data_item{itemType=NItemType} = data_item:get(DiamondID),
	OtherDiamondTypeList = [begin 
					case data_item:get(DiamondID1) of 
						?undefined->
							NItemType;
						#data_item{itemType=ItemType}->
							ItemType 
					end
				end ||#diamond_unit{diamondID=DiamondID1}<-OtherDiamondList],
	lists:member(NItemType,OtherDiamondTypeList).

to_p_diamond_location(GerID,#diamond_unit{pos=Pos},GerPos)->
	#p_diamond_location{gerUID=GerID,gerPos=GerPos,diamondPos=Pos}.

to_p_diamond(#diamond_unit{pos=Pos,diamondID=DiamondID})->
	#p_diamond{pos=Pos,diamondID=DiamondID}.

is_open()->
	case data_diamond:get(is_open) of
		true->
			true;
		_->
			false
	end.

%%获取精灵承载的神器和穿戴的宝石的所有属性
get_holygrail_system_buff_add(#holyGrail{holyGrailLevel=HolyGrailLevel,diamondInfo=DiamondInfo},GerTypeID)->
	HolyGrailBuffAdd = get_holygrail_buff_add(HolyGrailLevel,GerTypeID),
	lists:foldl(fun(#diamond_unit{diamondID=DiamondID},Acc)->
		SingleDiamondBuffAdd = get_diamond_buff_add(DiamondID),
		ger_attr:append_add_attr(Acc,SingleDiamondBuffAdd)
	end,HolyGrailBuffAdd,DiamondInfo).

get_holygrail_buff_add(HolyGrailLevel,GerTypeID)->
	case data_diamond:get({holyGrail,HolyGrailLevel}) of
		?undefined->
			0;
		{_ConditionList,_SlotNum,_UpLevelCost,AddAttr}->
			#data_ger{gerProperty=GerProperty} = data_ger:get(GerTypeID),
			%%物理系精灵只接受破甲，法术系只接受法穿
    		case role_awake:is_magic(GerProperty) of
    			true->
    				AddAttr#add_attr{gerPhyDefBite=0};
    			false->
    				AddAttr#add_attr{gerMagDefBite=0}
    		end
	end.

get_diamond_buff_add(DiamondID)->
	case data_item:get(DiamondID) of
		?undefined->
			0;
		#data_item{addAttr=AddAttr}->
			AddAttr
	end.

test_change_role_holygrail(RoleID,GerID,HolyGrailLevel,IsFinishSacrifice)->
	Msg = {change_holygrail,{GerID,HolyGrailLevel,IsFinishSacrifice}},
	role_lib:send_server(RoleID,Msg).

do_test_change_holygrail(Msg)->
	case Msg of
		{GerID,HolyGrailLevel,IsFinishSacrifice}->
			case role_data:get_ger(GerID) of
				{value,Ger,PosList2,_LPosList,_GerBag,ger}->
					#ger{gerBase=#gerBase{gerHolyGrailInfo=HolyGrailInfo}=GerBase} = Ger,
					NewGer = Ger#ger{gerBase=GerBase#gerBase{gerHolyGrailInfo=HolyGrailInfo#holyGrail{holyGrailLevel=HolyGrailLevel,isFinishSacrifice=IsFinishSacrifice}}},
					NewGer2 = ger_attr:recacl(NewGer,PosList2),        
      				PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
      				role_data:set_posList(PosList4);
      			_ ->
      				?ERR("change gerID:~w holyGrail can not find ger~n",[GerID])
      		end;
     	_ ->
     		?ERR("undefined Msg:~w ~n",[Msg])
    end.

is_diamond_equip(DiamondID,GerList)->
	lists:foldl(fun(Ger,{IsEquipAcc,Num})->
			#ger{gerBase=#gerBase{gerHolyGrailInfo=HolyGrailInfo}} = Ger,
			#holyGrail{diamondInfo=DiamondList} = HolyGrailInfo,
			L = [E||E=#diamond_unit{diamondID=DiamondID1}<-DiamondList,DiamondID=:=DiamondID1],
			case length(L) of
				0->
					{IsEquipAcc orelse false,Num};
				N->
					{IsEquipAcc orelse true,Num+N}
			end
		end,
	{false,0},GerList).
