%% @author caohongyang
%% @doc 武将属性处理
%% Created 2013-3-6


-module(ger_attr).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").
-include("def_homestead.hrl").  

%%装备精炼加成的比率
-define(ITEM_RANKADD_RATIO, 10000).

%% API functions

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

recacl_gers() ->
	% refresh_lieu_add_attr(),
	PosListT=role_data:get_posList(),
	PosList=[recacl(E, lists:delete(E, PosListT))||E<-PosListT],
	role_data:set_posList2(PosList) ,
	recacl_gers(PosList).

%% 计入副将加成，通知客户端更新武将主要属性，刷新战斗力，更新总战斗力
recacl_gers(PosList) ->	
	BuddyAddAttr = role_data:get_lieu_add_attr(),
	{AtkAdd,HpAdd} = role_buddy:get_buddy_normal_buff_for_total(),
	PosList2=
		lists:foldl(fun(Tar,GerListAcc)->
							#ger{gerExtra=GerExtra,gerFamilyTekAdd=GerFamilyTekAdd,gerMagicBookAdd=GerMagicBookAdd,gerEnchantAdd=GerEnchantAdd,gerAwakeAdd=GerAwakeAdd,gerCrystalAdd=GerCrystalAdd} = Tar,
							% AddAttr=#add_attr{gerAttackAddtion=AtkAdd2,gerHpMaxAddtion=HpAdd2},
							AddAttr2 =
                                lists:foldl(fun(E, Acc) ->
                                                case is_record(E, add_attr) of
                                                    false ->
                                                        Acc;
                                                    _ ->
                                                        append_add_attr(Acc, E)
                                                end
                                            end, #add_attr{}, [BuddyAddAttr, GerExtra, GerFamilyTekAdd, GerMagicBookAdd, GerEnchantAdd, GerAwakeAdd,GerCrystalAdd]),
                            %%z这个地方出现一个特殊的问题，由于晶体的基础奥义值会受到天赋的加成影响，所以AddAttr2的奥义值已经在Ger的gerattr中计算了，所以此处只能对基础奥义值做特殊处理，忽略掉这里的基础奥义值，防止奥义值多加一次
							Tar21= add_enter_skill_attr(Tar, AddAttr2#add_attr{gerProMean=0}),
							% ?ERR("AddAttr2:~w Tar:~w Tar2:~w ~n",[AddAttr2,Tar,Tar2]),
							%% 此处获取一次对应精灵的装备
							EquipList = role_data:get_equip(Tar21#ger.gerID),
							LegendRankAdd = get_ger_legend_add(EquipList),
							Tar2 = add_legend_rank_add(Tar21,LegendRankAdd),
							ger_lib:notify_update(Tar2),
							[Tar2|GerListAcc]
					end,[],PosList), 
	role_data:set_posListT(refresh_fightPower(PosList2, AtkAdd, HpAdd)).

%% 计入副将加成，单个武将的
recacl_gers(Ger,PosList) ->
    refresh_lieu_add_attr(),
    #ger{gerExtra=GerExtra,gerFamilyTekAdd=GerFamilyTekAdd,gerMagicBookAdd=GerMagicBookAdd,gerEnchantAdd=GerEnchantAdd,gerAwakeAdd=GerAwakeAdd,gerCrystalAdd=GerCrystalAdd} = Ger,
	AddAttr = role_data:get_lieu_add_attr(),
	{AtkAdd,HpAdd} = role_buddy:get_buddy_normal_buff_for_total(),
    AddAttr2 =
        lists:foldl(fun(E, Acc) ->
                        case is_record(E, add_attr) of
                            false ->
                                Acc;
                            _ ->
                                append_add_attr(Acc, E)
                        end
                    end, #add_attr{}, [AddAttr, GerExtra, GerFamilyTekAdd, GerMagicBookAdd, GerEnchantAdd, GerAwakeAdd,GerCrystalAdd]),
    %%z这个地方出现一个特殊的问题，由于晶体的基础奥义值会受到天赋的加成影响，所以AddAttr2的奥义值已经在Ger的gerattr中计算了，所以此处只能对基础奥义值做特殊处理，忽略掉这里的基础奥义值，防止奥义值多加一次
    Ger21 = add_enter_skill_attr(add_enter_skill_attr(Ger, AddAttr2#add_attr{gerProMean=0}),AddAttr),
    EquipList = role_data:get_equip(Ger21#ger.gerID),
	LegendRankAdd = get_ger_legend_add(EquipList),
    Ger2 = add_legend_rank_add(Ger21,LegendRankAdd),
    ger_lib:notify_update(Ger2),
    Ger3 = refresh_fightPower_ger(Ger2, AtkAdd, HpAdd, PosList),
	Ger4 = erease_skill_attr(Ger3),
    role_data:set_posListT([Ger4|PosList]).

%% %% 计入套装加成
%% recacl_gers1(PosList)->
%% 	lists:foldl(fun(Tar,GerListAcc)->
%% 						Tar2=recacl_ger_all_equip(Tar),
%% 						[Tar2|GerListAcc]
%% 				end,[],PosList).

%% 计入套装加成，单个武将的
%%计算怪物由于也要走这个逻辑，怪物直接不计算这些加成
recacl_ger_all_equip(#ger{gerID=GerTypeID,gerBase=#gerBase{gerTypeID=GerTypeID},gerAttr=GerAttr02}=Ger,_,_)->
    ProValue = role_fight:calc_profound_hp(GerAttr02),
	Ger#ger{gerProHp = ProValue};
recacl_ger_all_equip(#ger{gerID=GerID}=Ger,EnterSkillHpAdd,EnterSkillAtkAdd) ->
    AddAttr = recal_all_equipment(GerID),
    #ger{gerExtra=GerExtra,gerFamilyTekAdd=GerFamilyTekAdd,gerMagicBookAdd=GerMagicBookAdd,gerEnchantAdd=GerEnchantAdd,gerAwakeAdd=GerAwakeAdd,gerCrystalAdd=GerCrystalAdd} = Ger,
    %?INFO("recacl_ger_all_equip recal_all_equipment ~w ~n GerExtra:~w",[AddAttr,GerExtra]),
	AddAttr2 =
        lists:foldl(fun(E, Acc) ->
                        case is_record(E, add_attr) of
                            false ->
                                Acc;
                            _ ->
                                append_add_attr(Acc, E)
                        end
                    end, #add_attr{}, [AddAttr, GerExtra, GerFamilyTekAdd, GerMagicBookAdd, GerEnchantAdd, GerAwakeAdd,GerCrystalAdd]),

    AddAttr3 = case is_record(GerExtra, add_attr) of 
        true ->
            append_add_attr(GerExtra,AddAttr);
        _ ->
            AddAttr
    end,

    BuddyAddAttr = role_data:get_lieu_add_attr(),
    BuddySpecialAdd = role_buddy:get_buddy_special_buff_for_total(),
    {AtkAdd,HpAdd} = role_buddy:get_buddy_normal_buff_for_total(),
    #ger{gerAttr=GerAttr01}=Ger2 = Ger#ger{gerExtra=AddAttr3},

	% AddAttr = #add_attr{gerAttackAddtion=AtkAdd,gerHpMaxAddtion=HpAdd},

	#ger{gerAttr=GerAttr02}=add_enter_skill_attr(Ger2, append_add_attr(AddAttr2,BuddyAddAttr)),
	ProValue = role_fight:calc_profound_hp(GerAttr02),
	GerAttr03 = GerAttr01#gerAttr{gerProHpMax=ProValue,gerProMean=ProValue},
	Ger4 = Ger2#ger{gerProHp = ProValue,gerAttr=GerAttr03},
	
	FightPower = cacl_fight_power(Ger4#ger.gerBase, add_enter_skill_attr(Ger4, append_add_attr(AddAttr2,BuddySpecialAdd)), EnterSkillHpAdd+HpAdd, EnterSkillAtkAdd+AtkAdd),
	GerAttr2 = GerAttr03#gerAttr{gerFightPower=FightPower},
	Ger4#ger{gerAttr=GerAttr2,gerSpeed=GerAttr2#gerAttr.gerSpeed};
recacl_ger_all_equip(#gerSimple{}=Ger,_,_) ->
    Ger.

refresh_lieu_add_attr()->
	role_buddy:calc_role_buddy().

%%v4.1.0版本使用伙伴系统计算
calc_lieu_add_attr()->
	% LPosListT = role_data:get_lieuposList(),  %% 装备的小伙伴（副将）
	% LieuInfo = role_data:get_lieutenantInfo(),	%% 用于装备小伙伴的位置
	% lists:foldl(fun(#ger{gerBase=GerBase},{AttAddAcc,HpAddAcc})->
	% 					#gerBase{gerTypeID=GerTypeID,gerPos=Pos,gerQuality=Quality}=GerBase,
	% 					#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
	% 					case PartnerAdditionByQuality = data_partner:get(GerStar) of
	% 						undefined ->
	% 							{AttAddAcc, HpAddAcc};
	% 						_ ->
	% 						case lists:keyfind(Quality, 1, PartnerAdditionByQuality) of
	% 							{_,AddAtt,AddHp}->
	% 								case lists:keyfind(Pos, #t_lieu.pos, LieuInfo) of
	% 									#t_lieu{infoID1=_ID1, infoID2=_ID2, infoID3=_ID3}-> 
	% 										{AttAddAcc+AddAtt, HpAddAcc+AddHp};
	% 									_X ->
	% 										{AttAddAcc, HpAddAcc}
	% 								end;
	% 							false ->
	% 								{AttAddAcc, HpAddAcc}
	% 						end
	% 					end
	% 			end,{0,0},LPosListT).
	role_buddy:init_role_buddy().

get_original_posList()->
	PosList=role_data:get_posList(), 
	lists:foldl(fun(Ger, Acc)->
						[recacl(Ger, PosList)|Acc]end, [], PosList).

%% 当前的小伙伴信息
get_original_lieu_posList()->
	PosList=role_data:get_lieuposList(),
	lists:foldl(fun(Ger, Acc)->
						[recacl_lieu(Ger, PosList)|Acc]end, [], PosList).

%% 上阵武将重算属性
%% 如果是装备信息变化，会调用这个方法只计算装备对单一宠物的影响
recacl_f(GerID) ->
	PosList = role_data:get_posList(),
	case lists:keytake(GerID, #ger.gerID, PosList) of
		{value, #ger{}=Ger, PosList2} ->
			Ger2 = recacl(Ger, PosList2),
            NewPosList = [Ger2|PosList2],
            erlang:put(?posList, NewPosList),
			PosList4 = 
				case lists:keytake(GerID, #ger.gerID, role_data:get_posListT()) of
					false ->
						PosList2;
					{value,_,PosList3} ->
						PosList3
				end,
            recacl_gers(Ger2, PosList4),
            role_data:clear_mainGerTypeID();
		_ when GerID /= 1000 -> %没上阵
			LPosList=role_data:get_lieuposList(),
			{value, #ger{}=Ger, LPosList2} = lists:keytake(GerID, #ger.gerID, LPosList),
			Ger2 = recacl(Ger, LPosList2),
			LPosList3 = [Ger2|LPosList2],
			role_data:set_lieuposList(LPosList3);
        _ -> %变动训练师装备,GerID=1000是训练师，训练师没有自身属性
            ?ERR("不应该用训练师gerid 1000来执行这个方法，可能是逻辑写错了，检查代码")
	end.

%% 值推送主将战斗力变化
refresh_fightPower(GerList,AtkAdd, HpAdd) ->
	{GerList2, UpdatePowerList} = 
	lists:mapfoldl(fun(Ger, Acc) ->
					  #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
					  #gerBase{gerPos=GerPos} = GerBase,
						{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList),
					  FightPower = cacl_fight_power(GerBase, Ger, EnterSkillHpAdd+HpAdd, EnterSkillAtkAdd+AtkAdd),
					  GerAttr2 = GerAttr#gerAttr{gerFightPower=FightPower},
					  Ger2 = Ger#ger{gerAttr=GerAttr2},
                      %%注释下面代码是为了推送所有宠物的战斗力给客户端，否则客户端逻辑会导致，不更新不在在UpdatePowerList中的战斗力。
%% 					  if GerAttr#gerAttr.gerFightPower == FightPower ->
%% 							 {Ger2, Acc};
%% 						 true ->
					  		{Ger2, [#p_ger_power{fightPower=FightPower,pos=GerPos}|Acc]}
%% 					  end
				   end, [], GerList),
    OldPowerTotal = lists:sum([GerAttr#gerAttr.gerFightPower||#ger{gerAttr=GerAttr}<-GerList]),
    NewPowerTotal = lists:sum([GerAttr#gerAttr.gerFightPower||#ger{gerAttr=GerAttr}<-GerList2]),
	if UpdatePowerList =/= [] andalso OldPowerTotal =/= NewPowerTotal->
           %?INFO("战斗力变化:~w",[NewPowerTotal-OldPowerTotal]),
		   ?sendself(#sc_ger_refresh_power{gerPowerList=UpdatePowerList});
	   true ->
           %?INFO("战斗力未变化"),
		   ignore
	end,
	GerList2.

%% 推送主将战斗力变化,单个武将
refresh_fightPower_ger(Ger,AtkAdd, HpAdd, GerList) ->
    #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
    #gerBase{gerPos=GerPos} = GerBase,
    {EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, [Ger|GerList]),
    FightPower = cacl_fight_power(GerBase, Ger, EnterSkillHpAdd + HpAdd, EnterSkillAtkAdd + AtkAdd),
    GerAttr2 = GerAttr#gerAttr{gerFightPower=FightPower},
    Ger2 = Ger#ger{gerAttr=GerAttr2},
    if GerAttr#gerAttr.gerFightPower =:= FightPower ->
           UpdatePowerList = [];
       true ->
           UpdatePowerList = [#p_ger_power{fightPower=FightPower,pos=GerPos}]
    end,
    if UpdatePowerList =/= [] ->
           ?sendself(#sc_ger_refresh_power{gerPowerList=UpdatePowerList});
       true ->
           ignore
    end,
    Ger2.

refresh_other_fightPower(GerList,AtkAdd,HpAdd) ->
	GerList2= 
	lists:foldl(fun(GerB0, Acc) ->
					Ger = transOldGer2NewGer(GerB0),
                    #ger{gerExtra=GerExtra,gerFamilyTekAdd=GerFamilyTekAdd,gerMagicBookAdd=GerMagicBookAdd,gerEnchantAdd=GerEnchantAdd,gerAwakeAdd=GerAwakeAdd,gerCrystalAdd=GerCrystalAdd} = Ger,
                    AddAttr=#add_attr{gerAttackAddtion=AtkAdd,gerHpMaxAddtion=HpAdd},
					AddAttrT =
                        lists:foldl(fun(E, AAAcc) ->
                                        case is_record(E, add_attr) of
                                            false ->
                                                AAAcc;
                                            _ ->
                                                append_add_attr(AAAcc, E)
                                        end
                                        %%z这个地方出现一个特殊的问题，由于晶体的基础奥义值会受到天赋的加成影响，所以AddAttr2的奥义值已经在Ger的gerattr中计算了，所以此处只能对基础奥义值做特殊处理，忽略掉这里的基础奥义值，防止奥义值多加一次
                                    end, #add_attr{}, [AddAttr, GerExtra, GerFamilyTekAdd, GerMagicBookAdd, GerEnchantAdd, GerAwakeAdd,GerCrystalAdd#add_attr{gerProMean=0}]),
                    GerT = add_enter_skill_attr(Ger,AddAttrT),
                    %% 此处获取一次对应精灵的装备
                    EquipList = role_data:get_equip(GerT#ger.gerID),
                    LegendRankAdd = get_ger_legend_add(EquipList),
                    GerT1 = add_legend_rank_add(GerT,LegendRankAdd),
					[GerT1|Acc]
				   end, [], GerList),
    refresh_other_fightPower2(GerList2,AtkAdd,HpAdd).

refresh_other_fightPower2(GerList,AtkAdd, HpAdd) ->
	GerList2 = 
	lists:foldl(fun(Ger, Acc) ->
					  #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
					  #gerBase{gerPos=GerPos} = GerBase,
						{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList),
					  FightPower = cacl_fight_power(GerBase, Ger, EnterSkillHpAdd+HpAdd, EnterSkillAtkAdd+AtkAdd),
					  GerAttr2 = GerAttr#gerAttr{gerFightPower=FightPower},
					  [Ger#ger{gerAttr=GerAttr2}|Acc]
				   end, [], GerList),
	GerList2.


refresh_fightPower(GerList) ->
	{GerList2, _UpdatePowerList} = 
	lists:mapfoldl(fun(Ger, Acc) ->
					  #ger{gerBase=GerBase,gerAttr=GerAttr} = Ger,
					  #gerBase{gerPos=GerPos} = GerBase,
						{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList),
					  FightPower = cacl_fight_power(GerBase, Ger, EnterSkillHpAdd, EnterSkillAtkAdd),
					  GerAttr2 = GerAttr#gerAttr{gerFightPower=FightPower},
					  Ger2 = Ger#ger{gerAttr=GerAttr2},
					  if GerAttr#gerAttr.gerFightPower == FightPower ->
							 {Ger2, Acc};
						 true ->
					  		{Ger2, [#p_ger_power{fightPower=FightPower,pos=GerPos}|Acc]}
					  end
				   end, [], GerList),
%% 	if UpdatePowerList =/= [] ->
%% 		   ?sendself(#sc_ger_refresh_power{gerPowerList=UpdatePowerList});
%% 	   true ->
%% 		   ignore
%% 	end,
	GerList2.


					  
%% 计算出战武将的属性,计入属性包含基本、天命、装备、wake技能、god技能，enter技能的属性加成只计入战斗力
%% 宠物换阵时，会针对每个宠物调用该方法，重新计算属性
recacl(Ger, GerList) ->
	if is_record(Ger, ger) ->
		   #ger{gerBase=#gerBase{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID} = Ger;
	   true ->
		   #gerSimple{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerID=GerID,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo} = Ger
	end,
	EquipList = role_data:get_equip(GerID), %% 得装备信息，用于计算装备加成
	GerList2=[Ger|GerList],
	PosTypeIDList =
		lists:map(fun(#ger{gerBase=#gerBase{gerTypeID=E}}) ->
						  E;
					 (#gerSimple{gerTypeID=E}) ->
						  E
				  end, GerList2),
	{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList2), %%计算技能加成
	SpLieuList=get_sp_lieu_typeIDList(role_data:get_lieutenantInfo()),%%小伙伴信息
	FamilyTekAddAttr = role_lib:getFamilyTekAddAttr(),     %% 科技加成
    GerMagicBookAdd = role_data:get_magicBook_add_attr(),
    TRSID = get_trSpecial_ids(),
	recacl(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, lists:delete(GerTypeID, SpLieuList++PosTypeIDList), EnterSkillHpAdd, EnterSkillAtkAdd, true,FamilyTekAddAttr,talent,GerMagicBookAdd,check_illegal_awakeinfo(GerAwakeInfo),TRSID,GerCrystalInfo,GerBody,GerHolyGrailInfo).

%%v4.1注释，小伙伴之间没有存在天命激活，对上阵精灵的加成也是直接按照品阶计算的，故认为计算小伙伴的属性很多余，此处直接按照等级和品阶计算一个ger结构返回
recacl_lieu(Ger,GerList)->	
	if 
		is_record(Ger, ger) ->
		   % #ger{gerBase=#gerBase{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID} = Ger;
	   		Ger;
	   	true ->
		   #gerSimple{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,
		   		gerID=GerID,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo} = Ger,
			#data_ger{gerHpMax=Hp,gerAttack=Atk,destinyIDList=DesIDList,gerProperty=GerProperty,baseSpeed=BaseSpeed,addSpeed=AddSpeed} = DataGer = data_ger:get(GerTypeID),
			Speed = trunc(BaseSpeed + AddSpeed * (Level + 60 * trunc(Quality / 10))), 
			Hp2 = trunc(Hp *(1+(Level-1)*0.1)* math:pow(2,Quality*0.1)),
			Atk2= trunc(Atk*(1+(Level-1)*0.1)* math:pow(2,Quality*0.1)),
			GerAttr = #gerAttr{
								gerProMean      	= DataGer#data_ger.gerProMean 			
								,gerProMeanAddtion  = DataGer#data_ger.gerProMeanAddtion     			
								,gerAttckToProMean  = DataGer#data_ger.gerAttckToProMean       			
								,gerHpToProMean     = DataGer#data_ger.gerHpToProMean
								,gerAttack       	= Atk2
								,gerHpMax        	= Hp2
								,gerSpeed			= Speed
								,gerSpInit       	= DataGer#data_ger.gerSpInit       			
								,gerSpMax       	= DataGer#data_ger.gerSpMax       			
								,gerCritic       	= DataGer#data_ger.gerCritic       			
								,gerCriticReduce 	= DataGer#data_ger.gerCriticReduce 			
								,gerDoom         	= DataGer#data_ger.gerDoom         			
								,gerMiss         	= DataGer#data_ger.gerMiss        			
								,gerAbsorb       	= DataGer#data_ger.gerAbsorb       		   			
								,gerDamageBack   	= DataGer#data_ger.gerDamageBack   		   			
								,gerReel         	= DataGer#data_ger.gerReel         		   			
								,gerReelReduce   	= DataGer#data_ger.gerReelReduce   		   			
								,gerPhyDefBite   	= DataGer#data_ger.gerPhyDefBite      			
								,gerPhyDef       	= DataGer#data_ger.gerPhyDef          			
								,gerMagDefBite   	= DataGer#data_ger.gerMagDefBite      			
								,gerMagDef       	= DataGer#data_ger.gerMagDef       
							   },
			GerBase = #gerBase{gerTypeID=GerTypeID,gerQuality=Quality,gerLevel=Level,gerPos=GerPos,
				gerExp=GerExp,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo},
			#ger{gerBase=GerBase,gerID=GerID,gerAttr=GerAttr,gerExtra=#add_attr{},gerHp=Hp2,gerSp=DataGer#data_ger.gerSpInit,
				gerProHp=DataGer#data_ger.gerProMean,gerFamilyTekAdd=#add_attr{},gerMagicBookAdd=#add_attr{},gerEnchantAdd=#add_attr{},
				gerAwakeAdd=#add_attr{},gerCrystalAdd=#add_attr{},gerSpeed=Speed} 
	end.
% recacl_lieu(Ger, GerList) ->
% %% 	LieuInfo = role_data:get_lieutenantInfo(),
% 	if is_record(Ger, ger) ->
% 		   #ger{gerBase=#gerBase{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID} = Ger;
% 	   true ->
% 		   #gerSimple{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerID=GerID,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo} = Ger
% 	end,
% %% 	#t_lieu{infoID1=ID1}=get_lieu_ger_info(GerPos, LieuInfo),
% 	EquipList = role_data:get_equip(GerID),
% 	GerList2=[Ger|GerList],
% 	PosTypeIDList =
% 		lists:map(fun(#ger{gerBase=#gerBase{gerTypeID=E}}) ->
% 						  E;
% 					 (#gerSimple{gerTypeID=E}) ->
% 						  E
% 				  end, GerList2),
% 	%{EnterSkillHpAdd, EnterSkillAtkAdd} = cacl_enter_skill_add_fight_power(GerPos, GerList2),
% 	PosTypeIDList2=
% %% 		if GerTypeID =:= ID1 ->
% 			   PosTypeIDList ++ get_main_ger_typeIDList(),
% %% 		   true ->
% %% 			   PosTypeIDList
% %% 		end,
% 	FamilyTekAddAttr = role_lib:getFamilyTekAddAttr(),
%     GerMagicBookAdd = role_data:get_magicBook_add_attr(),
% 	recacl(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, lists:delete(GerTypeID, PosTypeIDList2), 0, 0, false,FamilyTekAddAttr,talent,GerMagicBookAdd,GerAwakeInfo,[],GerCrystalInfo,GerBody,GerHolyGrailInfo).

%%计算背包里面的武将属性
recacl_bag(Ger) ->
    if is_record(Ger,ger) ->
		   #ger{gerBase=#gerBase{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID} = Ger;
	   true ->
		   #gerSimple{gerLevel=Level,gerExp=GerExp,gerQuality=Quality,gerTypeID=GerTypeID,gerPos=GerPos,gerID=GerID,gerAwakeInfo=GerAwakeInfo,gerCrystalInfo=GerCrystalInfo,gerBody=GerBody,gerHolyGrailInfo=GerHolyGrailInfo} = Ger
	end,
    
    GerResult = recacl( GerID, GerTypeID, GerPos, Level, GerExp, Quality, [], [], 0, 0, false,#add_attr{},no_talent,#add_attr{},GerAwakeInfo,[],GerCrystalInfo,GerBody,GerHolyGrailInfo),
    GerResult.

get_main_ger_typeIDList()->
	lists:foldl(fun(#ger{gerBase=#gerBase{gerTypeID=E}},Acc)->
						[E|Acc];
				   (#gerSimple{gerTypeID=E},Acc)->
						[E|Acc]
				end, [], role_data:get_posList()).

get_sp_lieu_typeIDList(_LieuInfo) ->
	LieuList=role_data:get_lieuposList(),
	lists:foldl(fun(Ger, Acc)->
						if is_record(Ger, ger) ->
							   #ger{gerBase=#gerBase{gerPos=_Pos,gerTypeID=GerTypeID}} = Ger;
						   true ->
							   #gerSimple{gerPos=_Pos,gerTypeID=GerTypeID} = Ger
						end,
%% 						#t_lieu{infoID1=ID1}=get_lieu_ger_info(Pos, LieuInfo),
%% 						if GerTypeID =:= ID1 ->
							   [GerTypeID|Acc]
%% 						   true ->
%% 							   Acc
%% 						end
				end, [], LieuList).

get_lieu_ger_info(Pos, LieuInfo)->
	case lists:keyfind(Pos, #t_lieu.pos, LieuInfo) of
		false ->
			{t_lieu,Pos,0,0,0,0,0,0};
		#t_lieu{}=X ->
			X
	end.  

cacl_enter_skill_add_fight_power(TarPos, GerList) ->
	PosList = 
	lists:foldl(fun(#gerSimple{gerPos=GerPos}, Acc) ->
							 [GerPos|Acc];
						(#ger{gerBase=#gerBase{gerPos=GerPos}}, Acc) ->
							 [GerPos|Acc]
					 end, [], GerList),
	lists:foldl(fun(#gerSimple{gerTypeID=GerTypeID,gerPos=GerPos,gerQuality=GerQuality}, Acc) ->
							 cacl_enter_skill_add_fight_power2(TarPos, GerPos, GerQuality, GerTypeID, Acc, PosList);
						(#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerPos=GerPos,gerQuality=GerQuality}}, Acc) ->
							 cacl_enter_skill_add_fight_power2(TarPos, GerPos, GerQuality, GerTypeID, Acc, PosList)
					 end, {0,0}, GerList).

cacl_enter_skill_add_fight_power2(TarPos, GerPos, GerQuality, GerTypeID, Acc, PosList) ->
	#gerSkill{enter=GerEnterSkills} = ger_lib:get_ger_skills(GerTypeID, GerQuality),
	lists:foldl(fun(GerSkillEnterID,Acc0)-> 
						case data_skill:get(GerSkillEnterID) of
							#data_skill{targetSelect=TarSelect,gerEnterOrGodAdd=#add_attr{gerHpMaxAddtion=HpAdd,gerAttackAddtion=AtkAdd}} 
							  when HpAdd =/= 0 orelse AtkAdd =/= 0->
								SelectList = role_fight:select_posList(TarSelect, GerPos, PosList),
								if is_list(SelectList) ->
									   case lists:member(TarPos, SelectList) of
										   true ->
											   {A,B}=Acc0,
											   {HpAdd+A, AtkAdd+B};
										   false ->
											   Acc0
									   end;							   
								   true ->
									   Acc0
								end;
							_ ->
								Acc0
						end
				end,Acc,GerEnterSkills).

typeList(EquipList) ->
	[ItemTypeID || #item{itemTypeID=ItemTypeID} <- EquipList].

check_destiny_add(1, ArmedGerTypeIDList, _EquipTypeIDList, DesNeedList) ->
	lists:all(fun(E) -> lists:member(E, ArmedGerTypeIDList) end, DesNeedList);
check_destiny_add(2, _ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) ->
	lists:all(fun(E) -> lists:member(E, EquipTypeIDList) end, DesNeedList).

cacl_destiny_add([], _ArmedGerTypeIDList, _EquipList) ->
	{0,0};
cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipList) ->
	cacl_destiny_add(DesIDList, ArmedGerTypeIDList, typeList(EquipList), 0,0,[]).


%%v4.1版本将小伙伴从上阵概念中独立出来，会出现闪光和非闪光精灵同一类天命被激活的情况，此处利用闪光天命和非闪光天命差值10000的关系来区分
cacl_destiny_add([DesID|DesIDList], ArmedGerTypeIDList, EquipTypeIDList, HpAdd, AttackAdd,UseDesIDList) ->
	#data_destiny{destinyType=DesType, destinyNeedList=DesNeedList, addAttr=DesAddAttr} = data_destiny:get(DesID),
	case checkDestinyNeedCalc(DesID,UseDesIDList) of
		add->
			%%没有任何相关的天命加入，可直接计算确定加入
			case check_destiny_add(DesType, ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) of
				false->
					cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipTypeIDList, HpAdd, AttackAdd,UseDesIDList);
				true->
					#add_attr{gerHpMaxAddtion=GerHpMaxAddtion,gerAttackAddtion=GerAttackAddtion}=transOldAddAttr2NewAddAttr(DesAddAttr),
					cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipTypeIDList, GerHpMaxAddtion+HpAdd, GerAttackAddtion+AttackAdd,[DesID|UseDesIDList])
			end;
		{sub,WithOutNormalDesList,NormalDesID}->
			%%已经加入非闪光，可能需要减去非闪光再加上闪光
			case check_destiny_add(DesType, ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) of
				false->
					cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipTypeIDList, HpAdd, AttackAdd,UseDesIDList);
				true->
					%%优先选择闪光的技能加成
					#add_attr{gerHpMaxAddtion=BlinkGerHpMaxAddtion,gerAttackAddtion=BlinkGerAttackAddtion}=transOldAddAttr2NewAddAttr(DesAddAttr),
					#data_destiny{addAttr=NormalDesAddAttr} = data_destiny:get(NormalDesID),
					#add_attr{gerHpMaxAddtion=NormalGerHpMaxAddtion,gerAttackAddtion=NormalGerAttackAddtion}=transOldAddAttr2NewAddAttr(NormalDesAddAttr),
					GerHpMaxAddtion = erlang:max(BlinkGerHpMaxAddtion-NormalGerHpMaxAddtion,0),
					GerAttackAddtion = erlang:max(BlinkGerAttackAddtion-NormalGerAttackAddtion,0),
					cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipTypeIDList, GerHpMaxAddtion+HpAdd, GerAttackAddtion+AttackAdd,[DesID|WithOutNormalDesList])
			end;
		ingnore->
			%%已经加入了闪光，无需计算
			cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipTypeIDList, HpAdd, AttackAdd,UseDesIDList)
	end;
cacl_destiny_add([], _ArmedGerTypeIDList, _EquipTypeIDList, HpAdd, AttackAdd,UseDesIDList) ->
	% ?INFO("UseDesIDList:~w ~n",[UseDesIDList]),
	{HpAdd, AttackAdd}.

checkDestinyNeedCalc(DesID,UseDesIDList)->
	if
		DesID > 10000 andalso DesID < 20000->
			%%判定为普通精灵天命
			BlinkDesID = DesID-10000,
			case lists:member(BlinkDesID,UseDesIDList) of
				true->
					%%对应闪光技能已经激活
					ignore;
				false->
					add
			end;
		DesID < 10000->
			%%判定为闪光精灵天命
			NormalDesID = DesID+10000,
			case lists:member(NormalDesID,UseDesIDList) of
				true->
					%%对应非闪光技能已经激活
					{sub,lists:delete(NormalDesID,UseDesIDList),NormalDesID};
				false->
					add
			end;
		DesID >20000 ->
			%%判定为装备天命
			add
	end.

%% 计算装备的精炼加成
calc_item_rankaddatiou( EquipList ) -> 
    StartTime = data_rankaddation:get(start_time),
    NowTime = util:seconds_to_datetime(util:now()),
    if 
        NowTime < StartTime ->
            {0, 0};
        true ->
            calc_item_rankaddatiou( EquipList, {0, 0} )
    end.

calc_item_rankaddatiou( [], Acc ) -> Acc;
calc_item_rankaddatiou( [#item{itemTypeID=ItemTypeID,itemRank=ItemRank0}|T], {AttAcc, HpAcc} ) ->
    #data_item{itemStar=ItemStar,itemMaxRank=MaxRank} = data_item:get(ItemTypeID),
    {AttAdd, HpAdd} = 
        case data_rankaddation:get(ItemStar) of
            undefined ->
                {0, 0};
            List ->
				ItemRank = min(ItemRank0, MaxRank),
                case lists:keyfind( ItemRank, 1, List ) of
                    {_,AttAdd1,HpAdd1} ->
                        {AttAdd1, HpAdd1};
                    _ ->
                        {0, 0}
                end
        end,
    calc_item_rankaddatiou( T, { AttAcc + AttAdd, HpAcc + HpAdd } ). 


%% @doc 重算属性,计入属性包含基本、天命、装备、wake技能、god技能，enter技能的属性加成只计入战斗力
recacl(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, ArmedGerTypeIDList
	  , EnterSkillHpAdd, EnterSkillAtkAdd, Type,FamilyTekAddAttr,HaveTalent,GerMagicBookAdd,GerAwakeInfo,TRSID,GerCrystalInfo,GerBody,GerHolyGrailInfo) ->
	case HaveTalent of
		no_talent ->
			ActiveTalentList = [];
		_ ->
	ActiveTalentList =role_talent:get_active_talent_list()
	end,
	TalentEquipList = role_data:get_equip(1000),
    %% 训练师专精
	recacl2(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, ArmedGerTypeIDList, EnterSkillHpAdd, EnterSkillAtkAdd
	  , Type,FamilyTekAddAttr,HaveTalent,GerMagicBookAdd,GerAwakeInfo,ActiveTalentList,TalentEquipList, TRSID,GerCrystalInfo,GerBody,GerHolyGrailInfo).
recacl2(GerID, GerTypeID, GerPos, Level, GerExp, Quality, EquipList, ArmedGerTypeIDList, EnterSkillHpAdd, EnterSkillAtkAdd
	  , Type,FamilyTekAddAttr,_HaveTalent,GerMagicBookAdd,GerAwakeInfo,ActiveTalentList,TalentEquipList, TRSID,GerCrystalInfo,GerBody,GerHolyGrailInfo) ->
	#gerSkill{wake=WakeSkills,god=GodSkills,buff=BuffSkills} = ger_lib:get_ger_skills(GerTypeID, Quality),
	
	#data_ger{gerHpMax=Hp,gerAttack=Atk,destinyIDList=DesIDList,gerProperty=GerProperty,baseSpeed=BaseSpeed,addSpeed=AddSpeed}
				 = DataGer = data_ger:get(GerTypeID),
	{DesHpAdd, DesAttackAdd} = cacl_destiny_add(DesIDList, ArmedGerTypeIDList, EquipList),
	Speed = trunc(BaseSpeed + AddSpeed * (Level + 60 * trunc(Quality / 10))), 
	Hp2 = trunc(Hp *(1+(Level-1)*0.1)* math:pow(2,Quality*0.1) * (1+DesHpAdd/10000)),
	Atk2= trunc(Atk*(1+(Level-1)*0.1)* math:pow(2,Quality*0.1) * (1+DesAttackAdd/10000)),
	EquipListWithTrainer = EquipList ++ TalentEquipList, 
	
    { AtkAdd, HpAdd } = calc_item_rankaddatiou( EquipListWithTrainer ),

    %计算包括训练师装备在内的装备加成
    %%fix此处由于装备具有附魔属性，附魔的属性是否加成与精灵属性相关，所以需要传递精灵的属性
    {EquipAddAttr0,EnchantAddAttr} = recacl_equip(EquipListWithTrainer,GerProperty),

    %%GerAwakeAddAttrByCrystal为觉醒124阶段受晶体影响获得的属性加成
    {GerAwakeAddAttr,GerAwakeAddAttrByCrystal} = recacl_awake_addattr(GerAwakeInfo,GerCrystalInfo),
	
	%%1-6+12类晶体的属性加成
	GerCrystalAddAttr1 = role_crystal:calculate_crystal_normal_addattr(GerCrystalInfo,GerTypeID),
    
    GerCrystalAddAttr = append_add_attr(GerAwakeAddAttrByCrystal,GerCrystalAddAttr1),
    
    IsFront = lists:member(GerPos, [1,2,3]),
    
    TalentAttr = lists:foldl(fun({TalentType,TalentLevel},AccTalentAttr)  ->
                                     TalentConfig = data_talent:get(TalentType+TalentLevel),
                                     TrigerType = TalentConfig#data_talent.trigger_type,
                                     PosType = TalentConfig#data_talent.target_type,
                                     %% 这里的PosType的意思去查看data_talent.config最上面的注释
                                     case TrigerType =:= 0 of
                                        false ->
                                            AccTalentAttr;
                                        _ ->
                                            if 
                                                PosType =:= 1  ->
                                                    append_add_attr(AccTalentAttr,TalentConfig#data_talent.add);
                                                IsFront =:= true andalso PosType =:= 2 ->
                                                    append_add_attr(AccTalentAttr,TalentConfig#data_talent.add);
                                                IsFront =:= false andalso PosType =:= 3 ->
                                                    append_add_attr(AccTalentAttr,TalentConfig#data_talent.add);
                                                PosType =:= 7 ->
                                                    case lists:member(GerProperty, TRSID) of
                                                        true ->
                                                            append_add_attr(AccTalentAttr,TalentConfig#data_talent.add);
                                                        _ ->
                                                            append_add_attr(AccTalentAttr,setelement(1,TalentConfig#data_talent.sub,add_attr)) 
                                                    end;
                                                true ->
                                                    AccTalentAttr
                                            end
                                             %%append_add_attr(AccTalentAttr1,TalentConfig#data_talent.sub);
                                     end;
                                ({TagID,Lv,Rank},AccTalentAttr) when (TagID == 1 orelse TagID == 2) andalso Rank > 0 ->
                                     %{_,_,StartLevel,SkillRank,_} = data_trainer_battle:get({cost,TagID,Lv}),
                                     {{B1,B2,B3},Buff1} = data_trainer_battle:get({skill,TagID,Rank}),
                                     %LevelD = Lv - StartLevel,
                                     Buff2 = case TagID of
                                                 1 -> #add_attr{gerAttack=B1*Lv,gerAttackAddtion=B2*Lv,gerPhyDefBite=B3*Lv,gerMagDefBite=B3*Lv};
                                                 2 -> #add_attr{gerHpMax=B1*Lv,gerHpMaxAddtion=B2*Lv,gerPhyDef=B3*Lv,gerMagDef=B3*Lv}
                                                 %_ -> #add_attr{gerProMean=B1*Lv,gerProMeanAddtion=B2*Lv} 
                                             end,
                                     AccTalentAttr1 = append_add_attr(AccTalentAttr, Buff2),
                                     append_add_attr(AccTalentAttr1,Buff1);
                                (_,AccTalentAttr) -> AccTalentAttr
                             end, #add_attr{}, ActiveTalentList),
    
    BaseAddAttr0 = 
        case is_role_ger(GerTypeID) of
            true ->
                GerLvlSgAdd = role_lvlSgAttr:calc_ger_lvlsg_add2(GerTypeID, Quality, Level),
                RoleLvSgAdd = role_lvlSgAttr:calc_role_lvlsg_add(),
                EquipLvSgAdd = role_lvlSgAttr:cacl_equip_lvlsg_add(EquipList),
                lists:foldl(fun(E, Acc) -> append_add_attr(Acc, E) end, EquipAddAttr0, [TalentAttr, GerLvlSgAdd, RoleLvSgAdd,EquipLvSgAdd]);
            _ ->
                EquipAddAttr0
        end,
    BaseAddAttr1 = case role_data:get_roleInfo() of
                      RoleInfo when RoleInfo#role.transmigration > 0 ->
                          TransmigrationAttrBuff = data_trainer:get(transmigration_attr_buff),
                          append_add_attr(BaseAddAttr0,TransmigrationAttrBuff);
                      _ ->
                          BaseAddAttr0
                  end,
    HomeAttrBuff = 
        case role_data:get_roleID() of
            ?undefined ->
                ?undefined;
            RID ->
            	%%此处使用rolePublic是因为修改玩家等级之后调用hook_up_level时，玩家的新等级还没有更新到role_data中，但是rolePublic已经更新了，故此处使用rolePublic
            	#rolePublic{level=RoleLevel} = role_lib:get_rolePublic(RID),
            	case RoleLevel >= data_home:get(constr_type_office_open_role_level) of
            		true->
                		data_home:get({constr_type_office,role_home:get_build_type_level(RID,?constr_type_office)});
                	false->
                		?undefined
                end
        end,
    BaseAddAttr2 = case HomeAttrBuff of
                      HomeAttrBuff0 when erlang:is_record(HomeAttrBuff0, add_attr) ->
                          append_add_attr(HomeAttrBuff0,BaseAddAttr1);
                      _ ->
                          BaseAddAttr1
                  end,

    %%v3.3.0 由于神器以及技能宝石附带的属性只和对应神器等级和技能宝石类型有关，不会受到其他的影响，故此处将该部分属性加入到装备部分中
    %%此处将神器，技能宝石增加的基本属性加入到装备以及团属性中
    %v4.0增加了对精灵属性有增加的buff技能，并且该技能目前只和对应技能ID的配置有关
    BuffSkillAddAttr = calculate_ger_buffskill_addattr(BuffSkills),
    BaseAddAttr = append_add_attr(append_add_attr(BaseAddAttr2,role_diamond:get_holygrail_system_buff_add(GerHolyGrailInfo,GerTypeID)),BuffSkillAddAttr),
    %?ERR("BaseAddAttr:~w ~n",[BaseAddAttr]),
	%% 无双技能
	if Type ->
		   AllAttr1 = lists:foldl(fun(SkillID,EquipAddAttrAcc)->
										case data_skill:get(SkillID) of
											#data_skill{gerEnterOrGodAdd=SkillAdd}->
												append_add_attr(SkillAdd, EquipAddAttrAcc);
											_ ->
												EquipAddAttrAcc
										end
								  end, BaseAddAttr, [role_fight:random_one_skill(WakeSkills)]),
		   %% 神将技能,这里只计算克制属性，其他属性由于是每次攻击时都会增加，在战斗中计算
		   AllAttr9 = lists:foldl(fun(SkillID,AllAttr1Acc)->
										case data_skill:get(SkillID) of
											#data_skill{gerEnterOrGodAdd=SkillAdd}->
											  	%%此处由于1001这个技能是越战越勇，应该在战斗中依次增加的，为防止有其他的god技能需要加成进去，所以对改技能特殊处理了下
												case lists:member(SkillID,[1001]) of
											  		false->
												  		append_add_attr_only_Anti(AllAttr1Acc, SkillAdd);
													true->
														AllAttr1Acc
												end;
											_ ->
												  AllAttr1Acc
										end
								  end, AllAttr1, [role_fight:random_one_skill(GodSkills)]);
	   true ->
		   AllAttr9 = BaseAddAttr
	end,
    Hp3 = trunc((BaseAddAttr#add_attr.gerHpMax +Hp2)),
    Atk3 = trunc((BaseAddAttr#add_attr.gerAttack +Atk2)),
    %%gerExtra此处包括精炼加成以及装备带来的速度加成
	GerExtra = #add_attr{gerHpMaxAddtion=HpAdd+AllAttr9#add_attr.gerHpMaxAddtion, gerAttackAddtion=AtkAdd+AllAttr9#add_attr.gerAttackAddtion,gerSpeedAddtion=AllAttr9#add_attr.gerSpeedAddtion},
	GerAttrT = #gerAttr{
						gerProMean      	= DataGer#data_ger.gerProMean   +  AllAttr9#add_attr.gerProMean      			
						,gerProMeanAddtion  = DataGer#data_ger.gerProMeanAddtion   +  AllAttr9#add_attr.gerProMeanAddtion      			
						,gerAttckToProMean  = DataGer#data_ger.gerAttckToProMean    +  AllAttr9#add_attr.gerAttckToProMean       			
						,gerHpToProMean     = DataGer#data_ger.gerHpToProMean   +  AllAttr9#add_attr.gerHpToProMean
						,gerAttack       	= Atk3
						,gerHpMax        	= Hp3
						,gerSpeed			= Speed	                                +  AllAttr9#add_attr.gerSpeed
						,gerSpInit       	= DataGer#data_ger.gerSpInit       		+  AllAttr9#add_attr.gerSpInit       			
						,gerSpMax       	= DataGer#data_ger.gerSpMax       		+  AllAttr9#add_attr.gerSpMax       			
						,gerCritic       	= DataGer#data_ger.gerCritic       		+  AllAttr9#add_attr.gerCritic       			
						,gerCriticReduce 	= DataGer#data_ger.gerCriticReduce 		+  AllAttr9#add_attr.gerCriticReduce 			
						,gerDoom         	= DataGer#data_ger.gerDoom         		+  AllAttr9#add_attr.gerDoom         			
						,gerMiss         	= DataGer#data_ger.gerMiss         		+  AllAttr9#add_attr.gerMiss         			
						,gerAbsorb       	= DataGer#data_ger.gerAbsorb       		+  AllAttr9#add_attr.gerAbsorb       			
						,gerDamageBack   	= DataGer#data_ger.gerDamageBack   		+  AllAttr9#add_attr.gerDamageBack   			
						,gerReel         	= DataGer#data_ger.gerReel         		+  AllAttr9#add_attr.gerReel         			
						,gerReelReduce   	= DataGer#data_ger.gerReelReduce   		+  AllAttr9#add_attr.gerReelReduce   			
						,gerPhyDefBite   	= DataGer#data_ger.gerPhyDefBite   		+  AllAttr9#add_attr.gerPhyDefBite   			
						,gerPhyDef       	= DataGer#data_ger.gerPhyDef       		+  AllAttr9#add_attr.gerPhyDef       			
						,gerMagDefBite   	= DataGer#data_ger.gerMagDefBite   		+  AllAttr9#add_attr.gerMagDefBite   			
						,gerMagDef       	= DataGer#data_ger.gerMagDef       		+  AllAttr9#add_attr.gerMagDef   	
					   },
	GerBase2 = #gerBase{		
						gerExp=GerExp
					   ,gerPos=GerPos
					   ,gerTypeID=GerTypeID    			
							   ,gerLevel=Level
							   ,gerQuality=Quality
							   ,gerAwakeInfo=GerAwakeInfo
							   ,gerCrystalInfo=GerCrystalInfo
                               ,gerBody=GerBody
                               ,gerHolyGrailInfo=GerHolyGrailInfo
							  },
%    ProValue = role_fight:calc_profound_hp(GerAttrT),
%    GerAttr = GerAttrT,%#gerAttr{gerProHpMax=ProValue,gerProMean=ProValue},
	
	%?ERR("GerExtra:~w ~nAllAttr9:~w ~n~w~n~w",[GerExtra,AllAttr9,GerAttrT,GerAttr]),
	GerT=#ger{gerID=GerID, gerAttr=GerAttrT, gerBase=GerBase2, gerExtra=GerExtra, gerHp=Hp3,gerSpeed=Speed
			 ,gerSp=GerAttrT#gerAttr.gerSpInit,gerFamilyTekAdd=FamilyTekAddAttr,gerMagicBookAdd=GerMagicBookAdd
			 ,gerEnchantAdd=EnchantAddAttr,gerAwakeAdd=GerAwakeAddAttr,gerCrystalAdd=GerCrystalAddAttr},
	FightPower = cacl_fight_power(GerBase2,GerT, EnterSkillHpAdd, EnterSkillAtkAdd),
    %包含套装属性
    GerT1 = recacl_ger_all_equip(GerT#ger{gerAttr=GerAttrT#gerAttr{gerFightPower=FightPower}},EnterSkillHpAdd,EnterSkillAtkAdd),
    GerT2 = cacl_unique_effect_buff(GerT1),
    GerT2.

cacl_unique_effect_buff(#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerQuality=Rank}}=Ger)->
    SkillList = data_unique_append:get({attr_buff,GerTypeID}),
    cacl_unique_effect_buff(Ger,Rank,SkillList).

cacl_unique_effect_buff(Ger,_,?undefined) -> Ger;
cacl_unique_effect_buff(Ger,_,[]) -> Ger;
cacl_unique_effect_buff(Ger,Rank,[{SR,ST}|SkillList]) ->
    if Rank >= SR ->
           #ger{gerAttr=GerAttr}=Ger,
           case ST of
               1 -> #gerAttr{gerPhyDefBite=GerPhyDefBite,gerMagDefBite=GerMagDefBite}=GerAttr,
                    Bite = max(GerPhyDefBite,GerMagDefBite),
                    GerAttr2 = GerAttr#gerAttr{gerPhyDefBite=Bite,gerMagDefBite=Bite};
               _ -> GerAttr2 = GerAttr
           end,
           Ger2 = Ger#ger{gerAttr=GerAttr2},
           cacl_unique_effect_buff(Ger2,Rank,SkillList);
       true -> cacl_unique_effect_buff(Ger,Rank,SkillList)
    end.

erease_skill_attr(#ger{gerBase=GerBase,gerAttr=GerAttr}=Ger)->
	#gerBase{gerTypeID=GerTypeID,gerQuality=Quality}=GerBase,
	#gerSkill{wake=WakeSkills,god=GodSkills} = ger_lib:get_ger_skills(GerTypeID, Quality),
	%% 无双技能
	AllAttr1 = lists:foldl(fun(SkillID,EquipAddAttrAcc)->
								   case data_skill:get(SkillID) of
									   #data_skill{gerEnterOrGodAdd=SkillAdd}->
										   append_add_attr(SkillAdd, EquipAddAttrAcc);
									   _ ->
										   EquipAddAttrAcc
								   end
						   end, #add_attr{}, [role_fight:random_one_skill(WakeSkills)]),
	%% 神将技能,这里只计算克制属性，其他属性由于是每次攻击时都会增加，在战斗中计算
	AllAttr9 = lists:foldl(fun(SkillID,AllAttr1Acc)->
								   case data_skill:get(SkillID) of
									   #data_skill{gerEnterOrGodAdd=SkillAdd}->
										   append_add_attr_only_Anti(AllAttr1Acc, SkillAdd);
									   _ ->
										   AllAttr1Acc
								   end
						   end, AllAttr1, [role_fight:random_one_skill(GodSkills)]),
	GerAttr2 = erease_add_attr(GerAttr,AllAttr9),
	Ger#ger{gerAttr=GerAttr2}.

%% 只能用于生成己方精灵阵容，会默认应用己方训练师加成
new_ger(GerTypeID, Level, Quality, EquipList, TypeIDList) ->
	recacl(tk_id:gen_gerID(), GerTypeID, 0, Level, data_ger_level:get(Level), Quality, EquipList, lists:delete(GerTypeID, TypeIDList), 0, 0, true,#add_attr{},no_talent,#add_attr{},[],[],[],0,#holyGrail{}).

new_ger_with_awake(GerTypeID, Level, Quality, EquipList, TypeIDList,AwakeInfo)->
	recacl(tk_id:gen_gerID(), GerTypeID, 0, Level, data_ger_level:get(Level), Quality, EquipList, lists:delete(GerTypeID, TypeIDList), 0, 0, true,#add_attr{},no_talent,#add_attr{},check_illegal_awakeinfo(AwakeInfo),[],[],0,#holyGrail{}).

%% 用于生成对手精灵阵容
new_mon(GerTypeID, Level, Quality, EquipList, TypeIDList) ->
	recacl2(GerTypeID, GerTypeID, 0, Level, data_ger_level:get(Level), Quality, EquipList, lists:delete(GerTypeID, TypeIDList), 0, 0, true,#add_attr{},no_talent,#add_attr{},[],[],[],[],[],0,#holyGrail{}).

%% 重算所有装备带来的属性加成
-spec recacl_equip(EquipList :: [#item{}]) -> #add_attr{}.
recacl_equip([]) ->
	#add_attr{};
recacl_equip([Head|EquipList]) ->
	lists:foldl(fun(#item{addAttr=AddAttr}, Acc) ->
						append_add_attr(AddAttr, Acc)
				end, Head#item.addAttr, EquipList).

recacl_equip([],_GerProperty)->
	{#add_attr{},#add_attr{}};
recacl_equip(EquipList,GerProperty)->
	lists:foldl(fun(Equip,{EquipAddAttrAcc,EnchantAddAttrAcc})->
			%%v315开始增加加速装备，此处返回的equipAddAttr属性中将会包括对应的装备的附加属性
			{EquipAddAttr,EnchantAddAttr} = recacl_equip_withenchant(Equip,GerProperty),
			PftAddAttr = calc_pft_attr(Equip#item.itemTypeID,Equip#item.itemRank,#add_attr{}),
			EquipAddAttr2 = append_add_attr(EquipAddAttr,PftAddAttr),
			{append_add_attr(EquipAddAttr2,EquipAddAttrAcc),append_add_attr(EnchantAddAttr,EnchantAddAttrAcc)}
		end,{#add_attr{},#add_attr{}},EquipList).

calc_pft_attr(_,ItemRank,Add) when ItemRank =< 10 ->
	Add;
calc_pft_attr(ItemTypeID,ItemRank,Add)->
	case data_item_pft_uprank:get({ItemTypeID,ItemRank}) of
		{_,_,_,AddAttr} ->
			calc_pft_attr(ItemTypeID,ItemRank-1,append_add_attr(AddAttr, Add));
		_ ->
			Add
	end.

%%计算精灵觉醒技能1,2,4阶段带来的属性加成,由于2.6版本中晶体系统会影响觉醒加成,所以增加的CrystalAdd的计算，但是AwakeAdd返回的值依旧是旧的觉醒属性
%%return #add_attr{}
recacl_awake_addattr(GerAwakeInfo,CrystalInfo)->
	lists:foldl(fun(Awake,{AwakeAdd,CrystalAdd})->
		case lists:member(Awake#awake.step,data_awake:get(data_awake_addattr_list)) of
			true->
				case data_awake_skill:get({data_awake_skill,Awake#awake.skillID}) of
					?undefined->
						{AwakeAdd,CrystalAdd};
					AddAttr->
						CrystalAddValue = role_crystal:calculate_crystal_awake_add(CrystalInfo,Awake#awake.step),
						CrystalAwakeAdd = role_crystal:calculate_Crystal_awake_append(AddAttr,CrystalAddValue,Awake#awake.step),
						% ?ERR("CrystalAddValue:~w CrystalAwakeAdd:~w AddAttr:~w ~n",[CrystalAddValue,CrystalAwakeAdd,AddAttr]),
						{append_add_attr(AddAttr,AwakeAdd),append_add_attr(CrystalAdd,CrystalAwakeAdd)}
				end;
			false->
				{AwakeAdd,CrystalAdd}
		end
	end,{#add_attr{},#add_attr{}},GerAwakeInfo).	


%%v315版本以后，equipAddattr返回值中包括加速装备的附加属性
recacl_equip_withenchant(Equip=#item{itemType=ItemType,itemTypeID=ItemTypeID,itemRank=ItemRank},GerProperty)->
	EnchantAddAttr1 = case Equip#item.itemenchantType=:=GerProperty of
		true->
			case lists:member(Equip#item.itemType,?EQUIP_TYPE_LIST_NO_STONE) of
				true->
					case data_item_enchant:get({data_enchant_buff,Equip#item.itemType,Equip#item.itemenchantLevel}) of
						?undefined->
							% ?ERR("出现未能查找到的附魔属性：Type：~w Level:~w ~n",[Equip#item.itemenchantType,Equip#item.itemenchantLevel]),
							#add_attr{};
						FindOne->
							FindOne
					end;
				false->
					% ?INFO("计算装备附魔出现非精灵装备的类型：~w ~n",[Equip#item.itemenchantType]),
					#add_attr{}
			end;
		false->
			#add_attr{}
	end,
	EquipAddAttr = Equip#item.addAttr,
	EnchantAddAttr = case item_lib:is_accelerate_equip(ItemType) of
		false->
			EnchantAddAttr1;
		true->
			case data_accelerate_equip:get({data_accelerate_equip,ItemTypeID,ItemRank,GerProperty}) of
				?undefined->
					case data_accelerate_equip:get({data_accelerate_equip,ItemTypeID,ItemRank,100}) of
						?undefined->
							EnchantAddAttr1;
						AccelerateEquipExtraAddAttr->
							append_add_attr(AccelerateEquipExtraAddAttr,EnchantAddAttr1)
					end;
				AccelerateEquipExtraAddAttr->
					append_add_attr(AccelerateEquipExtraAddAttr,EnchantAddAttr1)
			end
	end,
	% ?ERR("TypeID:~w ItemUID:~w EquipAddAttr:~w EnchantAddAttr:~w ~n",[ItemTypeID,Equip#item.itemUID,EquipAddAttr,EnchantAddAttr]),
	{EquipAddAttr,EnchantAddAttr}.

recal_all_equipment(GerID)->
	lists:foldl(fun(AddAttr, Acc) ->
						append_add_attr(AddAttr, Acc)
				end, #add_attr{}, role_allequipment:get_ger_all_equipment_add_attr_list(GerID)).

append_add_attr(0,0) ->
	#add_attr{};
append_add_attr(0,AddAttrB) when is_record(AddAttrB, add_attr) ->
	AddAttrB;
append_add_attr(0, AddAttrB) ->
	transOldAddAttr2NewAddAttr(AddAttrB);
append_add_attr(AddAttrA,0) when is_record(AddAttrA,add_attr) ->
	AddAttrA;
append_add_attr(AddAttrA,0) ->
	transOldAddAttr2NewAddAttr(AddAttrA);
append_add_attr(AddAttrA, AddAttrB) ->
    append_add_attr(AddAttrA, AddAttrB, false).

append_add_attr(AddAttrA, AddAttrB, _) when is_record(AddAttrA,add_attr) andalso is_record(AddAttrB,add_attr)->
	#add_attr{
			  gerAttack          = AddAttrA#add_attr.gerAttack       +AddAttrB#add_attr.gerAttack       
			 ,gerHpMax           = AddAttrA#add_attr.gerHpMax        +AddAttrB#add_attr.gerHpMax        
			 ,gerSpInit          = AddAttrA#add_attr.gerSpInit       +AddAttrB#add_attr.gerSpInit       
			 ,gerSpMax           = AddAttrA#add_attr.gerSpMax        +AddAttrB#add_attr.gerSpMax       
			 ,gerCritic          = AddAttrA#add_attr.gerCritic       +AddAttrB#add_attr.gerCritic       
			 ,gerCriticReduce    = AddAttrA#add_attr.gerCriticReduce +AddAttrB#add_attr.gerCriticReduce 
			 ,gerDoom            = AddAttrA#add_attr.gerDoom         +AddAttrB#add_attr.gerDoom         
			 ,gerMiss            = AddAttrA#add_attr.gerMiss         +AddAttrB#add_attr.gerMiss         
			 ,gerAbsorb          = AddAttrA#add_attr.gerAbsorb       +AddAttrB#add_attr.gerAbsorb       
			 ,gerDamageBack      = AddAttrA#add_attr.gerDamageBack   +AddAttrB#add_attr.gerDamageBack   
			 ,gerReel            = AddAttrA#add_attr.gerReel         +AddAttrB#add_attr.gerReel         
			 ,gerReelReduce      = AddAttrA#add_attr.gerReelReduce   +AddAttrB#add_attr.gerReelReduce   
			 ,gerPhyDefBite      = AddAttrA#add_attr.gerPhyDefBite   +AddAttrB#add_attr.gerPhyDefBite   
			 ,gerPhyDef          = AddAttrA#add_attr.gerPhyDef       +AddAttrB#add_attr.gerPhyDef       
			 ,gerMagDefBite      = AddAttrA#add_attr.gerMagDefBite   +AddAttrB#add_attr.gerMagDefBite   
			 ,gerMagDef          = AddAttrA#add_attr.gerMagDef       +AddAttrB#add_attr.gerMagDef       
			 ,gerAttackAddtion   = AddAttrA#add_attr.gerAttackAddtion+AddAttrB#add_attr.gerAttackAddtion
			 ,gerHpMaxAddtion    = AddAttrA#add_attr.gerHpMaxAddtion +AddAttrB#add_attr.gerHpMaxAddtion 
			 ,gerProMean         = AddAttrA#add_attr.gerProMean      +AddAttrB#add_attr.gerProMean      
			 ,gerProMeanAddtion  = AddAttrA#add_attr.gerProMeanAddtion       +AddAttrB#add_attr.gerProMeanAddtion      
			 ,gerAttckToProMean  = AddAttrA#add_attr.gerAttckToProMean       +AddAttrB#add_attr.gerAttckToProMean       
			 ,gerHpToProMean     = AddAttrA#add_attr.gerHpToProMean      +AddAttrB#add_attr.gerHpToProMean
			 ,gerGskillDemageRate= AddAttrA#add_attr.gerGskillDemageRate + AddAttrB#add_attr.gerGskillDemageRate
			 ,gerUskillDemageRate= AddAttrA#add_attr.gerUskillDemageRate + AddAttrB#add_attr.gerUskillDemageRate
			 ,gerExtraDemageRate = AddAttrA#add_attr.gerExtraDemageRate  + AddAttrB#add_attr.gerExtraDemageRate
			 ,gerDecDemageRate   = AddAttrA#add_attr.gerDecDemageRate    + AddAttrB#add_attr.gerDecDemageRate
			 ,gerSpeed			 = AddAttrA#add_attr.gerSpeed			 + AddAttrB#add_attr.gerSpeed
			 ,gerSpeedAddtion	 = AddAttrA#add_attr.gerSpeedAddtion	 + AddAttrB#add_attr.gerSpeedAddtion
			 };
append_add_attr(AddAttr, SubAttr, false) when is_record(SubAttr, sub_attr) andalso is_record(AddAttr,add_attr)->
	#add_attr{
			  gerAttack          = max(0,AddAttr#add_attr.gerAttack       - SubAttr#sub_attr.gerAttack)
			 ,gerHpMax           = max(0,AddAttr#add_attr.gerHpMax        - SubAttr#sub_attr.gerHpMax)
			 ,gerSpInit          = max(0,AddAttr#add_attr.gerSpInit       - SubAttr#sub_attr.gerSpInit)
			 ,gerSpMax           = max(0,AddAttr#add_attr.gerSpMax       - SubAttr#sub_attr.gerSpMax)
			 ,gerCritic          = max(0,AddAttr#add_attr.gerCritic       - SubAttr#sub_attr.gerCritic)
			 ,gerCriticReduce    = max(0,AddAttr#add_attr.gerCriticReduce - SubAttr#sub_attr.gerCriticReduce)
			 ,gerDoom            = max(0,AddAttr#add_attr.gerDoom         - SubAttr#sub_attr.gerDoom)
			 ,gerMiss            = max(0,AddAttr#add_attr.gerMiss         - SubAttr#sub_attr.gerMiss)
			 ,gerAbsorb          = max(0,AddAttr#add_attr.gerAbsorb       - SubAttr#sub_attr.gerAbsorb)
			 ,gerDamageBack      = max(0,AddAttr#add_attr.gerDamageBack   - SubAttr#sub_attr.gerDamageBack)
			 ,gerReel            = max(0,AddAttr#add_attr.gerReel         - SubAttr#sub_attr.gerReel)
			 ,gerReelReduce      = max(0,AddAttr#add_attr.gerReelReduce   - SubAttr#sub_attr.gerReelReduce)
			 ,gerPhyDefBite      = max(0,AddAttr#add_attr.gerPhyDefBite   - SubAttr#sub_attr.gerPhyDefBite)
			 ,gerPhyDef          = max(0,AddAttr#add_attr.gerPhyDef       - SubAttr#sub_attr.gerPhyDef)
			 ,gerMagDefBite      = max(0,AddAttr#add_attr.gerMagDefBite   - SubAttr#sub_attr.gerMagDefBite)
			 ,gerMagDef          = max(0,AddAttr#add_attr.gerMagDef       - SubAttr#sub_attr.gerMagDef)
			 ,gerAttackAddtion   = max(0,AddAttr#add_attr.gerAttackAddtion- SubAttr#sub_attr.gerAttackAddtion)
			 ,gerHpMaxAddtion    = max(0,AddAttr#add_attr.gerHpMaxAddtion - SubAttr#sub_attr.gerHpMaxAddtion)
			 ,gerProMean         = max(0,AddAttr#add_attr.gerProMean      - SubAttr#sub_attr.gerProMean)
			 ,gerProMeanAddtion  = max(0,AddAttr#add_attr.gerProMeanAddtion      - SubAttr#sub_attr.gerProMeanAddtion)
			 ,gerAttckToProMean  = max(0,AddAttr#add_attr.gerAttckToProMean       - SubAttr#sub_attr.gerAttckToProMean)
			 ,gerHpToProMean     = max(0,AddAttr#add_attr.gerHpToProMean      - SubAttr#sub_attr.gerHpToProMean)
			 ,gerGskillDemageRate= max(0,AddAttr#add_attr.gerGskillDemageRate - SubAttr#sub_attr.gerGskillDemageRate)
			 ,gerUskillDemageRate= max(0,AddAttr#add_attr.gerUskillDemageRate - SubAttr#sub_attr.gerUskillDemageRate)
			 ,gerExtraDemageRate = max(0,AddAttr#add_attr.gerExtraDemageRate - SubAttr#sub_attr.gerExtraDemageRate)
			 ,gerDecDemageRate   = max(0,AddAttr#add_attr.gerDecDemageRate - SubAttr#sub_attr.gerDecDemageRate)
			 ,gerSpeed			 = max(0,AddAttr#add_attr.gerSpeed			- SubAttr#sub_attr.gerSpeed)
			 ,gerSpeedAddtion	 = max(0,AddAttr#add_attr.gerSpeedAddtion	- SubAttr#sub_attr.gerSpeedAddtion)
			 };
append_add_attr(AddAttr, SubAttr, true) when is_record(SubAttr, sub_attr) andalso is_record(AddAttr,add_attr)->
	#add_attr{
			  gerAttack          = AddAttr#add_attr.gerAttack       - SubAttr#sub_attr.gerAttack
			 ,gerHpMax           = AddAttr#add_attr.gerHpMax        - SubAttr#sub_attr.gerHpMax
			 ,gerSpInit          = AddAttr#add_attr.gerSpInit       - SubAttr#sub_attr.gerSpInit
			 ,gerSpMax           = AddAttr#add_attr.gerSpMax       - SubAttr#sub_attr.gerSpMax
			 ,gerCritic          = AddAttr#add_attr.gerCritic       - SubAttr#sub_attr.gerCritic
			 ,gerCriticReduce    = AddAttr#add_attr.gerCriticReduce - SubAttr#sub_attr.gerCriticReduce
			 ,gerDoom            = AddAttr#add_attr.gerDoom         - SubAttr#sub_attr.gerDoom
			 ,gerMiss            = AddAttr#add_attr.gerMiss         - SubAttr#sub_attr.gerMiss
			 ,gerAbsorb          = AddAttr#add_attr.gerAbsorb       - SubAttr#sub_attr.gerAbsorb
			 ,gerDamageBack      = AddAttr#add_attr.gerDamageBack   - SubAttr#sub_attr.gerDamageBack
			 ,gerReel            = AddAttr#add_attr.gerReel         - SubAttr#sub_attr.gerReel
			 ,gerReelReduce      = AddAttr#add_attr.gerReelReduce   - SubAttr#sub_attr.gerReelReduce
			 ,gerPhyDefBite      = AddAttr#add_attr.gerPhyDefBite   - SubAttr#sub_attr.gerPhyDefBite
			 ,gerPhyDef          = AddAttr#add_attr.gerPhyDef       - SubAttr#sub_attr.gerPhyDef
			 ,gerMagDefBite      = AddAttr#add_attr.gerMagDefBite   - SubAttr#sub_attr.gerMagDefBite
			 ,gerMagDef          = AddAttr#add_attr.gerMagDef       - SubAttr#sub_attr.gerMagDef
			 ,gerAttackAddtion   = AddAttr#add_attr.gerAttackAddtion- SubAttr#sub_attr.gerAttackAddtion
			 ,gerHpMaxAddtion    = AddAttr#add_attr.gerHpMaxAddtion - SubAttr#sub_attr.gerHpMaxAddtion
			 ,gerProMean         = AddAttr#add_attr.gerProMean      - SubAttr#sub_attr.gerProMean
			 ,gerProMeanAddtion  = AddAttr#add_attr.gerProMeanAddtion      - SubAttr#sub_attr.gerProMeanAddtion
			 ,gerAttckToProMean  = AddAttr#add_attr.gerAttckToProMean       - SubAttr#sub_attr.gerAttckToProMean
			 ,gerHpToProMean     = AddAttr#add_attr.gerHpToProMean      - SubAttr#sub_attr.gerHpToProMean
			 ,gerGskillDemageRate= AddAttr#add_attr.gerGskillDemageRate - SubAttr#sub_attr.gerGskillDemageRate
			 ,gerUskillDemageRate= AddAttr#add_attr.gerUskillDemageRate - SubAttr#sub_attr.gerUskillDemageRate
			 ,gerExtraDemageRate = AddAttr#add_attr.gerExtraDemageRate - SubAttr#sub_attr.gerExtraDemageRate
			 ,gerDecDemageRate   = AddAttr#add_attr.gerDecDemageRate - SubAttr#sub_attr.gerDecDemageRate
			 ,gerSpeed 		 	 = AddAttr#add_attr.gerSpeed		 - SubAttr#sub_attr.gerSpeed
			 ,gerSpeedAddtion	 = AddAttr#add_attr.gerSpeedAddtion  - SubAttr#sub_attr.gerSpeedAddtion
			 };
%%兼容旧的add_attr结构
append_add_attr(AddAttrA,AddAttrB,AllowMinus)->
	append_add_attr(transOldAddAttr2NewAddAttr(AddAttrA),transOldAddAttr2NewAddAttr(AddAttrB),AllowMinus).

transOldAddAttr2NewAddAttr(AddAttr) when is_record(AddAttr,add_attr)->
	AddAttr;
transOldAddAttr2NewAddAttr(AddAttr) when is_record(AddAttr,sub_attr)->
	AddAttr;
transOldAddAttr2NewAddAttr(0)->
	#add_attr{};
transOldAddAttr2NewAddAttr({A,B})->
	erlang:error();
transOldAddAttr2NewAddAttr(AddAttr)->
	case AddAttr of 
		?undefined->
			?ERR("出现属性为undefined情况，可能是装备等级有问题~n"),
			#add_attr{};
		_ ->
			TempList = erlang:tuple_to_list(AddAttr),
			%%添加附魔添加的新的4种属性
			case length(TempList) of 23 -> 	AddList = [0,0,0,0,0,0];
				27 -> AddList = [0,0];
				25-> AddList = [0,0,0,0];
				_ -> AddList = []
			end,
			erlang:list_to_tuple(TempList++AddList)
	end.

append_add_attr_no_Anti(0,0) ->
	#add_attr{};
append_add_attr_no_Anti(0, AddAttrB) ->
	transOldAddAttr2NewAddAttr(AddAttrB);
append_add_attr_no_Anti(AddAttrA,0) ->
	transOldAddAttr2NewAddAttr(AddAttrA);
%% 原始增加为A， 补充增加为B，即不计算B的克制属性
append_add_attr_no_Anti(AddAttrA, AddAttrB) when is_record(AddAttrA,add_attr) andalso is_record(AddAttrB,add_attr) ->
	#add_attr{
			  gerAttack          = AddAttrA#add_attr.gerAttack       +AddAttrB#add_attr.gerAttack       
			 ,gerHpMax           = AddAttrA#add_attr.gerHpMax        +AddAttrB#add_attr.gerHpMax        
			 ,gerSpInit          = AddAttrA#add_attr.gerSpInit       +AddAttrB#add_attr.gerSpInit       
			 ,gerSpMax          = AddAttrA#add_attr.gerSpMax       +AddAttrB#add_attr.gerSpMax       
			 ,gerCritic          = AddAttrA#add_attr.gerCritic       +AddAttrB#add_attr.gerCritic       
			 ,gerCriticReduce    = AddAttrA#add_attr.gerCriticReduce +AddAttrB#add_attr.gerCriticReduce 
			 ,gerDoom            = AddAttrA#add_attr.gerDoom         +AddAttrB#add_attr.gerDoom         
			 ,gerMiss            = AddAttrA#add_attr.gerMiss         +AddAttrB#add_attr.gerMiss         
			 ,gerAbsorb          = AddAttrA#add_attr.gerAbsorb       +AddAttrB#add_attr.gerAbsorb       
			 ,gerDamageBack      = AddAttrA#add_attr.gerDamageBack   +AddAttrB#add_attr.gerDamageBack   
			 ,gerReel            = AddAttrA#add_attr.gerReel         +AddAttrB#add_attr.gerReel         
			 ,gerReelReduce      = AddAttrA#add_attr.gerReelReduce   +AddAttrB#add_attr.gerReelReduce   
			 ,gerPhyDefBite      = AddAttrA#add_attr.gerPhyDefBite   +AddAttrB#add_attr.gerPhyDefBite   
			 ,gerPhyDef          = AddAttrA#add_attr.gerPhyDef       +AddAttrB#add_attr.gerPhyDef       
			 ,gerMagDefBite      = AddAttrA#add_attr.gerMagDefBite   +AddAttrB#add_attr.gerMagDefBite   
			 ,gerMagDef          = AddAttrA#add_attr.gerMagDef       +AddAttrB#add_attr.gerMagDef       
			 ,gerAttackAddtion   = AddAttrA#add_attr.gerAttackAddtion+AddAttrB#add_attr.gerAttackAddtion
			 ,gerHpMaxAddtion    = AddAttrA#add_attr.gerHpMaxAddtion +AddAttrB#add_attr.gerHpMaxAddtion 
			 ,gerProMean         = AddAttrA#add_attr.gerProMean   
			 ,gerProMeanAddtion  = AddAttrA#add_attr.gerProMeanAddtion   
			 ,gerAttckToProMean  = AddAttrA#add_attr.gerAttckToProMean     
			 ,gerHpToProMean     = AddAttrA#add_attr.gerHpToProMean
			 ,gerSpeed			 = AddAttrA#add_attr.gerSpeed		 + AddAttrB#add_attr.gerSpeed
			 ,gerSpeedAddtion	 = AddAttrA#add_attr.gerSpeedAddtion + AddAttrB#add_attr.gerSpeedAddtion
			 };
append_add_attr_no_Anti(AddAttrA,AddAttrB)->
	append_add_attr_no_Anti(transOldAddAttr2NewAddAttr(AddAttrA),transOldAddAttr2NewAddAttr(AddAttrB)).

mul_add_attr(_AddAttr,0)->
	#add_attr{};
mul_add_attr(0,_Mul)->
	#add_attr{};
mul_add_attr(AddAttr,Mul)->
	End = tuple_size(AddAttr),
	mul_add_attr(AddAttr,Mul,2,End+1).
mul_add_attr(AddAttr,_Mul,End,End)->
	AddAttr;
mul_add_attr(AddAttr,Mul,Begin,End)->
	Value = element(Begin,AddAttr),
	NewValue = trunc(Value * Mul/10000),
	NewAddAttr = setelement(Begin,AddAttr,NewValue),
	mul_add_attr(NewAddAttr,Mul,Begin+1,End).

append_add_attr_with_int(AddAttr,0)->
	AddAttr;
append_add_attr_with_int(AddAttr,Num)->
	append_add_attr_with_int(AddAttr,Num,true).
append_add_attr_with_int(AddAttr,Num,RealAppend)->
	RealNum = trunc(Num),
	[H|L] = tuple_to_list(AddAttr),
	L1 = 
	case RealAppend of
		true->
			lists:foldl(fun(E,Acc)->case E=:=0 of true->[0|Acc];false->[RealNum+E|Acc] end end,[],L);
		false->
			lists:foldl(fun(E,Acc)->case E=:=0 of true->[0|Acc];false->[RealNum|Acc] end end,[],L)
	end,			
	L2 = lists:reverse(L1),
	list_to_tuple([H|L2]).

append_add_attr_only_Anti(0,0) ->
	#add_attr{};
append_add_attr_only_Anti(0, AddAttrB) ->
	transOldAddAttr2NewAddAttr(AddAttrB);
append_add_attr_only_Anti(AddAttrA,0) ->
	transOldAddAttr2NewAddAttr(AddAttrA);
%% 原始增加为A， 补充增加为B，即只计算B的克制属性
append_add_attr_only_Anti(AddAttrA, AddAttrB) when is_record(AddAttrA,add_attr) andalso is_record(AddAttrB,add_attr)->
	#add_attr{
			  gerAttack          = AddAttrA#add_attr.gerAttack  
			 ,gerHpMax           = AddAttrA#add_attr.gerHpMax  
			 ,gerSpInit          = AddAttrA#add_attr.gerSpInit
			 ,gerSpMax          = AddAttrA#add_attr.gerSpMax 
			 ,gerCritic          = AddAttrA#add_attr.gerCritic 
			 ,gerCriticReduce    = AddAttrA#add_attr.gerCriticReduce
			 ,gerDoom            = AddAttrA#add_attr.gerDoom   
			 ,gerMiss            = AddAttrA#add_attr.gerMiss  
			 ,gerAbsorb          = AddAttrA#add_attr.gerAbsorb  
			 ,gerDamageBack      = AddAttrA#add_attr.gerDamageBack 
			 ,gerReel            = AddAttrA#add_attr.gerReel      
			 ,gerReelReduce      = AddAttrA#add_attr.gerReelReduce
			 ,gerPhyDefBite      = AddAttrA#add_attr.gerPhyDefBite 
			 ,gerPhyDef          = AddAttrA#add_attr.gerPhyDef    
			 ,gerMagDefBite      = AddAttrA#add_attr.gerMagDefBite  
			 ,gerMagDef          = AddAttrA#add_attr.gerMagDef      
			 ,gerAttackAddtion   = AddAttrA#add_attr.gerAttackAddtion
			 ,gerHpMaxAddtion    = AddAttrA#add_attr.gerHpMaxAddtion
			 ,gerProMean         = AddAttrA#add_attr.gerProMean      +AddAttrB#add_attr.gerProMean      
			 ,gerProMeanAddtion  = AddAttrA#add_attr.gerProMeanAddtion      +AddAttrB#add_attr.gerProMeanAddtion      
			 ,gerAttckToProMean  = AddAttrA#add_attr.gerAttckToProMean       +AddAttrB#add_attr.gerAttckToProMean       
			 ,gerHpToProMean     = AddAttrA#add_attr.gerHpToProMean      +AddAttrB#add_attr.gerHpToProMean
			 ,gerSpeed			 = AddAttrA#add_attr.gerSpeed			+ AddAttrB#add_attr.gerSpeed
			 ,gerSpeedAddtion	 = AddAttrA#add_attr.gerSpeedAddtion	+ AddAttrB#add_attr.gerSpeedAddtion
			 };
append_add_attr_only_Anti(AddAttrA,AddAttrB)->
	append_add_attr_no_Anti(transOldAddAttr2NewAddAttr(AddAttrA),transOldAddAttr2NewAddAttr(AddAttrB)).


cacl_fight_power(GerBase,Ger, EnterSkillHpAdd, EnterSkillAtkAdd) ->
	#gerBase{gerTypeID=GerTypeID,gerQuality=GerQuality}=GerBase,
	#gerSkill{unique=UniqueSkills, god=GodSkills, god2=God2Skills,enter=EnterSkills} = ger_lib:get_ger_skills(GerTypeID, GerQuality),
	GerUniqueSkillID = role_fight:random_one_skill(UniqueSkills),
	#data_skill{damageRatio=DamageRatio1,targetSelect=TargetSelect
			   ,gerReel=GerReel2,gerCritic=GerCritic2,gerAbsorb=GerAbsorb2,gerDoom=GerDoom2} = data_skill:get(GerUniqueSkillID),
	%%Fix 将附魔添加的必杀技伤害加成添加到必杀技自带的伤害加成中
	EnchantAddAttr = Ger#ger.gerEnchantAdd,
	DamageRatio = DamageRatio1 + (EnchantAddAttr#add_attr.gerUskillDemageRate+EnchantAddAttr#add_attr.gerGskillDemageRate) div 3,
	%% 叠加计算登场技能属性
	GerEnterSkillID = role_fight:random_one_skill(EnterSkills),
	case data_skill:get(GerEnterSkillID) of
		#data_skill{gerEnterOrGodAdd=AddAttr}->
			#ger{gerAttr=GerAttr} = add_enter_skill_attr(Ger, AddAttr);
		_ ->
			#ger{gerAttr=GerAttr} = Ger
	end,
	
	%?ERR("fightPower:~w,\n~w\n~w\n~w\n~w",[role_data:get_roleID(),GerBase, GerAttr, EnterSkillHpAdd, EnterSkillAtkAdd]),
	#gerAttr{gerReel=GerReel,gerCritic=GerCritic,gerAbsorb=GerAbsorb,gerDoom=GerDoom} = GerAttr,
	TargetNum = role_fight:skill_target_num(TargetSelect),
	%% 技能释放频率
	SkillProb = 0.33+0.0017*GerAttr#gerAttr.gerSpInit+0.005*GerAttr#gerAttr.gerSpMax,
	Base1 = ((1-SkillProb)*(1+GerReel/100)*(1+GerCritic/100)*(1+GerAbsorb/10000)*(1+GerDoom/100) + 
				SkillProb * (DamageRatio/10000) * TargetNum * 
				(1+(GerReel+GerReel2)/100) * (1+(GerCritic+GerCritic2)/100) * (1+(GerAbsorb+GerAbsorb2)/10000) * (1+(GerDoom+GerDoom2)/100)),
	Base = case Base1 =<0 of
		true ->
			1;
		false->
			Base1
	end,

%%     ProMean0 = GerAttr#gerAttr.gerProMean
%%             + GerAttr#gerAttr.gerHpMax * GerAttr#gerAttr.gerHpToProMean / 10000 
%%             + GerAttr#gerAttr.gerAttack * GerAttr#gerAttr.gerAttckToProMean / 10000, 
%%    ProMean = erlang:trunc(ProMean0 * (1 + GerAttr#gerAttr.gerProMeanAddtion / 10000) * data_trainer:get(promean_to_power)),
    %?INFO("ProMean(~w) to cacl FightPower",[ProMean]),
    
    ProValue = erlang:max(1, GerAttr#gerAttr.gerProHpMax),
	FightPower=
		(GerAttr#gerAttr.gerAttack)/26
			*(GerAttr#gerAttr.gerHpMax+ProValue)/156
            % *ProValue/156
			%*(1+GerAttr#gerAttr.gerCritic/100)
			*(1+GerAttr#gerAttr.gerCriticReduce/100)
			%*(1+GerAttr#gerAttr.gerDoom/100)
			*(1+GerAttr#gerAttr.gerMiss/100)
			*(1+GerAttr#gerAttr.gerDamageBack/10000)
			%*(1+GerAttr#gerAttr.gerAbsorb/10000)
			*(1+GerAttr#gerAttr.gerPhyDefBite/100)
			*(1+GerAttr#gerAttr.gerMagDefBite/100)
			*(1+GerAttr#gerAttr.gerPhyDef/100)
			*(1+GerAttr#gerAttr.gerMagDef/100)
			*(1+GerAttr#gerAttr.gerReelReduce/100)
			*Base
			*(1+EnterSkillHpAdd/10000)
			*(1+EnterSkillAtkAdd/10000),
	FightPower2 = 
		if GodSkills =:= [] ->
			   FightPower;
		   true ->
			   if God2Skills =:= []->
					  FightPower * (1.25);
				  true ->
					  #data_skill{gerEnterOrGodAdd=God2AddAttr} = data_skill:get(role_fight:random_one_skill(God2Skills)),
                      case God2AddAttr of
                          #add_attr{} ->
        					  ReAddHp = God2AddAttr#add_attr.gerHpMaxAddtion / 10000,
        					  FightPower * (1.25) * (1+ReAddHp);
                          _ ->
                              FightPower * (1.25)
                      end
			   end
		end,
	erlang:trunc((erlang:trunc(math:pow(FightPower2,0.3029) * 100))*(1+EnchantAddAttr#add_attr.gerGskillDemageRate/10000+EnchantAddAttr#add_attr.gerExtraDemageRate/10000+EnchantAddAttr#add_attr.gerDecDemageRate/10000)).

%% @doc 计算登场技能的属性加成
add_enter_skill_attr(Ger, AddAttr) when is_record(AddAttr, add_attr)->
	#ger{gerAttr=GerAttr,gerHp=GerHp}=Ger,
	#gerAttr{gerAttack=GerAttack,gerHpMax=GerHpMax,gerSpeed=GerSpeed}=GerAttr,
    %下面的血量战斗里的百分比加成中，包括天赋，装备
	GerAttack2 = trunc((GerAttack+AddAttr#add_attr.gerAttack)*(10000+AddAttr#add_attr.gerAttackAddtion)/10000),
	GerHpMax2= trunc((GerHpMax+AddAttr#add_attr.gerHpMax)*(10000+AddAttr#add_attr.gerHpMaxAddtion)/10000),	
	GerSpeed2 = trunc((GerSpeed + AddAttr#add_attr.gerSpeed) * (10000 + AddAttr#add_attr.gerSpeedAddtion) / 10000),
	GerAttr2 = GerAttr#gerAttr{
					gerAttack       	= GerAttack2 
				   ,gerHpMax        	= GerHpMax2
				   ,gerSpInit       	= GerAttr#gerAttr.gerSpInit       		+  AddAttr#add_attr.gerSpInit        			
				   ,gerSpMax       	= GerAttr#gerAttr.gerSpMax       		+  AddAttr#add_attr.gerSpMax                   			
				   ,gerCritic       	= GerAttr#gerAttr.gerCritic       		+  AddAttr#add_attr.gerCritic            			
				   ,gerCriticReduce 	= GerAttr#gerAttr.gerCriticReduce 		+  AddAttr#add_attr.gerCriticReduce  		
				   ,gerDoom         	= GerAttr#gerAttr.gerDoom         		+  AddAttr#add_attr.gerDoom                 			
				   ,gerMiss         	= GerAttr#gerAttr.gerMiss         		+  AddAttr#add_attr.gerMiss            			
				   ,gerAbsorb       	= GerAttr#gerAttr.gerAbsorb       		+  AddAttr#add_attr.gerAbsorb           			
				   ,gerDamageBack   	= GerAttr#gerAttr.gerDamageBack   		+  AddAttr#add_attr.gerDamageBack    			
				   ,gerReel         	= GerAttr#gerAttr.gerReel         		+  AddAttr#add_attr.gerReel               			
				   ,gerReelReduce   	= GerAttr#gerAttr.gerReelReduce   		+  AddAttr#add_attr.gerReelReduce    			
				   ,gerPhyDefBite   	= GerAttr#gerAttr.gerPhyDefBite   		+  AddAttr#add_attr.gerPhyDefBite    			
				   ,gerPhyDef       	= GerAttr#gerAttr.gerPhyDef       		+  AddAttr#add_attr.gerPhyDef            			
				   ,gerMagDefBite   	= GerAttr#gerAttr.gerMagDefBite   		+  AddAttr#add_attr.gerMagDefBite    			
				   ,gerMagDef       	= GerAttr#gerAttr.gerMagDef       		+  AddAttr#add_attr.gerMagDef       
				   ,gerProMean      	= GerAttr#gerAttr.gerProMean   +  AddAttr#add_attr.gerProMean      			
				   ,gerProMeanAddtion   = GerAttr#gerAttr.gerProMeanAddtion   +  AddAttr#add_attr.gerProMeanAddtion      			
				   ,gerAttckToProMean   = GerAttr#gerAttr.gerAttckToProMean    +  AddAttr#add_attr.gerAttckToProMean       			
				   ,gerHpToProMean      = GerAttr#gerAttr.gerHpToProMean   +  AddAttr#add_attr.gerHpToProMean
				   ,gerSpeed			= GerSpeed2       	
								   },
	%% 只有在参战时，血量是满的情况，才将当前血量随之改变
	%% 如果参战时，血量不满，则不涨血量
	GerHp2 = 
		if GerHp =/= GerHpMax ->
			   erlang:min(GerHp, GerHpMax2);
		   true ->
			   GerHpMax2
		end,
	%?DEBUG("sp=~w,,newSp=~w",[Ger#ger.gerSp,GerAttr2#gerAttr.gerSpInit]),
	Ger#ger{gerAttr=GerAttr2,
			gerHp=GerHp2,
			gerSp=GerAttr2#gerAttr.gerSpInit
		   ,gerSpeed=GerSpeed2
		   };
add_enter_skill_attr(Ger, SubAttr) when is_record(SubAttr, sub_attr)->
	#ger{gerAttr=GerAttr,gerHp=GerHp}=Ger,
	#gerAttr{gerAttack=GerAttack,gerHpMax=GerHpMax,gerSpeed=GerSpeed}=GerAttr,
	GerAttack2 = max(0,trunc((GerAttack-SubAttr#sub_attr.gerAttack)*(10000-SubAttr#sub_attr.gerAttackAddtion)/10000)),
	GerHpMax2= max(0,trunc((GerHpMax-SubAttr#sub_attr.gerHpMax)*(10000-SubAttr#sub_attr.gerHpMaxAddtion)/10000)),
	GerSpeed2 = max(0,trunc((GerSpeed-SubAttr#sub_attr.gerSpeed)*(10000-SubAttr#sub_attr.gerSpeedAddtion)/10000)),
    GerAttr2 = GerAttr#gerAttr{
        gerAttack            = GerAttack2 
        ,gerHpMax            = GerHpMax2
        ,gerSpInit           = max(0, GerAttr#gerAttr.gerSpInit               -  SubAttr#sub_attr.gerSpInit)
        ,gerSpMax            = max(0, GerAttr#gerAttr.gerSpMax                -  SubAttr#sub_attr.gerSpMax)
        ,gerCritic           = max(0, GerAttr#gerAttr.gerCritic               -  SubAttr#sub_attr.gerCritic)
        ,gerCriticReduce     = max(0, GerAttr#gerAttr.gerCriticReduce         -  SubAttr#sub_attr.gerCriticReduce)
        ,gerDoom             = max(0, GerAttr#gerAttr.gerDoom                 -  SubAttr#sub_attr.gerDoom)
        ,gerMiss             = max(0, GerAttr#gerAttr.gerMiss                 -  SubAttr#sub_attr.gerMiss)
        ,gerAbsorb           = max(0, GerAttr#gerAttr.gerAbsorb               -  SubAttr#sub_attr.gerAbsorb)
        ,gerDamageBack       = max(0, GerAttr#gerAttr.gerDamageBack           -  SubAttr#sub_attr.gerDamageBack)
        ,gerReel             = max(0, GerAttr#gerAttr.gerReel                 -  SubAttr#sub_attr.gerReel)
        ,gerReelReduce       = max(0, GerAttr#gerAttr.gerReelReduce           -  SubAttr#sub_attr.gerReelReduce)
        ,gerPhyDefBite       = max(0, GerAttr#gerAttr.gerPhyDefBite           -  SubAttr#sub_attr.gerPhyDefBite)
        ,gerPhyDef           = max(0, GerAttr#gerAttr.gerPhyDef               -  SubAttr#sub_attr.gerPhyDef)
        ,gerMagDefBite       = max(0, GerAttr#gerAttr.gerMagDefBite           -  SubAttr#sub_attr.gerMagDefBite)
        ,gerMagDef           = max(0, GerAttr#gerAttr.gerMagDef               -  SubAttr#sub_attr.gerMagDef)
        ,gerProMean          = max(0, GerAttr#gerAttr.gerProMean              - SubAttr#sub_attr.gerProMean)
        ,gerProMeanAddtion   = max(0, GerAttr#gerAttr.gerProMeanAddtion       - SubAttr#sub_attr.gerProMeanAddtion)
        ,gerAttckToProMean   = max(0, GerAttr#gerAttr.gerAttckToProMean       - SubAttr#sub_attr.gerAttckToProMean)
        ,gerHpToProMean      = max(0, GerAttr#gerAttr.gerHpToProMean          - SubAttr#sub_attr.gerHpToProMean)
							  ,gerSpeed=GerSpeed2
								   },
	%% 只有在参战时，血量是满的情况，才将当前血量随之改变
	%% 如果参战时，血量不满，则不涨血量
	GerHp2 = 
		if GerHp =/= GerHpMax ->
			   erlang:min(GerHp, GerHpMax2);
		   true ->
			   GerHpMax2
		end,
	%?DEBUG("sp=~w,,newSp=~w",[Ger#ger.gerSp,GerAttr2#gerAttr.gerSpInit]),
	Ger#ger{gerAttr=GerAttr2,
			gerHp=GerHp2,
			gerSp=GerAttr2#gerAttr.gerSpInit
		   ,gerSpeed=GerSpeed2
		   };
add_enter_skill_attr(Ger, _) ->
	Ger.

calculate_familyTekHpAndAtkAdd()->
	RoleInfo = role_data:get_roleInfo(),
	case RoleInfo of
		?undefined ->
			{0,0};
		_ ->
			#role{familyID = FamilyID} = RoleInfo,
			FamilyTek_AtkAdd = role_lib:calculate_familyTek_addbuff(FamilyID,1,2),
			FamilyTek_HpAdd = role_lib:calculate_familyTek_addbuff(FamilyID,2,2),
			{FamilyTek_AtkAdd,FamilyTek_HpAdd}
	end.
erease_add_attr(GerAttr,AddAttr)->
	GerAttr#gerAttr{
					gerSpInit       	= GerAttr#gerAttr.gerSpInit       		-  AddAttr#add_attr.gerSpInit       			
					,gerSpMax       	= GerAttr#gerAttr.gerSpMax       		-  AddAttr#add_attr.gerSpMax       			
					,gerCritic       	= GerAttr#gerAttr.gerCritic       		-  AddAttr#add_attr.gerCritic       			
					,gerCriticReduce 	= GerAttr#gerAttr.gerCriticReduce 		-  AddAttr#add_attr.gerCriticReduce 			
					,gerDoom         	= GerAttr#gerAttr.gerDoom         		-  AddAttr#add_attr.gerDoom         			
					,gerMiss         	= GerAttr#gerAttr.gerMiss         		-  AddAttr#add_attr.gerMiss         			
					,gerAbsorb       	= GerAttr#gerAttr.gerAbsorb       		-  AddAttr#add_attr.gerAbsorb       			
					,gerDamageBack   	= GerAttr#gerAttr.gerDamageBack   		-  AddAttr#add_attr.gerDamageBack   			
					,gerReel         	= GerAttr#gerAttr.gerReel         		-  AddAttr#add_attr.gerReel         			
					,gerReelReduce   	= GerAttr#gerAttr.gerReelReduce   		-  AddAttr#add_attr.gerReelReduce   			
					,gerPhyDefBite   	= GerAttr#gerAttr.gerPhyDefBite   		-  AddAttr#add_attr.gerPhyDefBite   			
					,gerPhyDef       	= GerAttr#gerAttr.gerPhyDef       		-  AddAttr#add_attr.gerPhyDef       			
					,gerMagDefBite   	= GerAttr#gerAttr.gerMagDefBite   		-  AddAttr#add_attr.gerMagDefBite   			
					,gerMagDef       	= GerAttr#gerAttr.gerMagDef       		-  AddAttr#add_attr.gerMagDef
					,gerProMean      	= GerAttr#gerAttr.gerProMean   -  AddAttr#add_attr.gerProMean      			
					,gerProMeanAddtion  = GerAttr#gerAttr.gerProMeanAddtion   -  AddAttr#add_attr.gerProMeanAddtion      			
					,gerAttckToProMean  = GerAttr#gerAttr.gerAttckToProMean    -  AddAttr#add_attr.gerAttckToProMean       			
					,gerHpToProMean     = GerAttr#gerAttr.gerHpToProMean   -  AddAttr#add_attr.gerHpToProMean
				   ,gerSpeed			= trunc(GerAttr#gerAttr.gerSpeed*1000 / (1000+AddAttr#add_attr.gerSpeedAddtion)) - AddAttr#add_attr.gerSpeed       	
				   }.
%% @doc 计算登场技能的属性加成
add_buff_attr(Ger, AddAttr) when is_record(AddAttr, add_attr)->
	#ger{gerAttr=GerAttr,gerHp=GerHp}=Ger,
	#gerAttr{gerAttack=GerAttack,gerHpMax=GerHpMax,gerSpeed=GerSpeed}=GerAttr,
	GerAttack2 = trunc((GerAttack+AddAttr#add_attr.gerAttack)*(10000+AddAttr#add_attr.gerAttackAddtion)/10000),
	GerHpMax2= trunc((GerHpMax+AddAttr#add_attr.gerHpMax)*(10000+AddAttr#add_attr.gerHpMaxAddtion)/10000),
	GerSpeed2 = trunc((GerSpeed+AddAttr#add_attr.gerSpeed)*(10000+AddAttr#add_attr.gerSpeedAddtion)/10000),
	GerAttr2 = GerAttr#gerAttr{
					gerAttack       	= GerAttack2
				   ,gerHpMax        	= GerHpMax2
				   ,gerSpInit       	= GerAttr#gerAttr.gerSpInit       		+  AddAttr#add_attr.gerSpInit       			
				   ,gerSpMax       	= GerAttr#gerAttr.gerSpMax       		+  AddAttr#add_attr.gerSpMax       			
				   ,gerCritic       	= GerAttr#gerAttr.gerCritic       		+  AddAttr#add_attr.gerCritic       			
				   ,gerCriticReduce 	= GerAttr#gerAttr.gerCriticReduce 		+  AddAttr#add_attr.gerCriticReduce 			
				   ,gerDoom         	= GerAttr#gerAttr.gerDoom         		+  AddAttr#add_attr.gerDoom         			
				   ,gerMiss         	= GerAttr#gerAttr.gerMiss         		+  AddAttr#add_attr.gerMiss         			
				   ,gerAbsorb       	= GerAttr#gerAttr.gerAbsorb       		+  AddAttr#add_attr.gerAbsorb       			
				   ,gerDamageBack   	= GerAttr#gerAttr.gerDamageBack   		+  AddAttr#add_attr.gerDamageBack   			
				   ,gerReel         	= GerAttr#gerAttr.gerReel         		+  AddAttr#add_attr.gerReel         			
				   ,gerReelReduce   	= GerAttr#gerAttr.gerReelReduce   		+  AddAttr#add_attr.gerReelReduce   			
				   ,gerPhyDefBite   	= GerAttr#gerAttr.gerPhyDefBite   		+  AddAttr#add_attr.gerPhyDefBite   			
				   ,gerPhyDef       	= GerAttr#gerAttr.gerPhyDef       		+  AddAttr#add_attr.gerPhyDef       			
				   ,gerMagDefBite   	= GerAttr#gerAttr.gerMagDefBite   		+  AddAttr#add_attr.gerMagDefBite   			
				   ,gerMagDef       	= GerAttr#gerAttr.gerMagDef       		+  AddAttr#add_attr.gerMagDef
				   ,gerProMean      	= GerAttr#gerAttr.gerProMean   +  AddAttr#add_attr.gerProMean      			
				   ,gerProMeanAddtion   = GerAttr#gerAttr.gerProMeanAddtion   +  AddAttr#add_attr.gerProMeanAddtion      			
				   ,gerAttckToProMean   = GerAttr#gerAttr.gerAttckToProMean    +  AddAttr#add_attr.gerAttckToProMean       			
				   ,gerHpToProMean      = GerAttr#gerAttr.gerHpToProMean   +  AddAttr#add_attr.gerHpToProMean  
							  ,gerSpeed=GerSpeed2     	
								   },
	%% 只有在参战时，血量是满的情况，才将当前血量随之改变
	%% 如果参战时，血量不满，则不涨血量
	GerHp2 = 
		if GerHp =/= GerHpMax ->
			   erlang:min(GerHp, GerHpMax2);
		   true ->
			   GerHpMax2
		end,
	?DEBUG("sp=~w,,newSp=~w",[Ger#ger.gerSp,GerAttr2#gerAttr.gerSpInit]),
	Ger#ger{gerAttr=GerAttr2,
			gerHp=GerHp2,
			gerSp=GerAttr2#gerAttr.gerSpInit,
			gerExtra=0
		   ,gerSpeed=GerSpeed2
		   }.

%% 将旧的ger结构转换为新的
transOldGer2NewGer(#ger{gerBase=GerBase}=Ger)->
	% NewGerAwakeInfo = [role_awake:transform_oldawake2newawake(GerAwakeUnit)||GerAwakeUnit<-GerAwakeInfo],
	NewGerBase = transOldGerBase2NewGerBase(GerBase),
    Ger#ger{gerBase=NewGerBase};
transOldGer2NewGer(OldGer)->
	case OldGer of
		{_, GerID, GerBase, GerAttrT, GerExtraT, Hp, Sp, _, FTekT, MagicT, EAT, AwakeAdd,CrystalAddT,_GerSpeed}->
			GerExtra = transOldAddAttr2NewAddAttr(GerExtraT),
			FTek = transOldAddAttr2NewAddAttr(FTekT),
			Magic = transOldAddAttr2NewAddAttr(MagicT),
			EA = transOldAddAttr2NewAddAttr(EAT),
			GerAwakeAdd = transOldAddAttr2NewAddAttr(AwakeAdd),
			CrystalAdd = transOldAddAttr2NewAddAttr(CrystalAddT);
		{_, GerID, GerBase, GerAttrT, GerExtraT, Hp, Sp, _, FTekT, MagicT, EAT, AwakeAdd,CrystalAddT} ->
			GerExtra = transOldAddAttr2NewAddAttr(GerExtraT),
			FTek = transOldAddAttr2NewAddAttr(FTekT),
			Magic = transOldAddAttr2NewAddAttr(MagicT),
			EA = transOldAddAttr2NewAddAttr(EAT),
			GerAwakeAdd = transOldAddAttr2NewAddAttr(AwakeAdd),
			CrystalAdd = transOldAddAttr2NewAddAttr(CrystalAddT);
		{_, GerID, GerBase, GerAttrT, GerExtraT, Hp, Sp, _, FTekT, MagicT, EAT, AwakeAdd} ->
			GerExtra = transOldAddAttr2NewAddAttr(GerExtraT),
			FTek = transOldAddAttr2NewAddAttr(FTekT),
			Magic = transOldAddAttr2NewAddAttr(MagicT),
			EA = transOldAddAttr2NewAddAttr(EAT),
			GerAwakeAdd = transOldAddAttr2NewAddAttr(AwakeAdd),
			CrystalAdd = #add_attr{};
		{_, GerID, GerBase, GerAttrT, GerExtraT, Hp, Sp, FTekT, MagicT, EAT} ->
			GerExtra = transOldAddAttr2NewAddAttr(GerExtraT),
			FTek = transOldAddAttr2NewAddAttr(FTekT),
			Magic = transOldAddAttr2NewAddAttr(MagicT),
			EA = transOldAddAttr2NewAddAttr(EAT),
			GerAwakeAdd = #add_attr{},
			CrystalAdd = #add_attr{};
		{_, GerID, GerBase, GerAttrT, GerExtraT, Hp, Sp, FTekT, MagicT} ->
			GerExtra = transOldAddAttr2NewAddAttr(GerExtraT),
			FTek = transOldAddAttr2NewAddAttr(FTekT),
			Magic = transOldAddAttr2NewAddAttr(MagicT),
			EA = #add_attr{},
			GerAwakeAdd = #add_attr{},
			CrystalAdd = #add_attr{};
		{_, GerID, GerBase, GerAttrT, GerExtraT, Hp, Sp, FTekT} ->
			GerExtra = transOldAddAttr2NewAddAttr(GerExtraT),
			FTek = transOldAddAttr2NewAddAttr(FTekT),
			Magic = #add_attr{},
			EA = #add_attr{},
			GerAwakeAdd = #add_attr{},
			CrystalAdd = #add_attr{};
		{_, GerID, GerBase, GerAttrT, GerExtraT, Hp, Sp} ->
			GerExtra = transOldAddAttr2NewAddAttr(GerExtraT),
			FTek = #add_attr{},
			Magic = #add_attr{},
			EA = #add_attr{},
			GerAwakeAdd = #add_attr{},
			CrystalAdd = #add_attr{}
	end,
    NewGerAttrT = case GerAttrT of
                    #gerAttr{} -> GerAttrT;
                    _ ->
                        GerAttrTLength = erlang:tuple_size(GerAttrT),
                        RightLen = erlang:tuple_size(#gerAttr{}),
                        GerAttrTList = case GerAttrTLength < RightLen of
                                         true ->
                                             erlang:tuple_to_list(GerAttrT) ++
                                               lists:foldl(fun (_, Acc) ->
                                                                   [0 | Acc]
                                                           end,
                                                           [],
                                                           lists:seq(1,
                                                                     RightLen -
                                                                       GerAttrTLength));
                                         _ ->
                                             {R, _} = lists:split(RightLen,
                                                                  erlang:tuple_to_list(GerAttrT)),
                                             R
                                       end,
                        erlang:list_to_tuple(GerAttrTList)
                  end,
    ProHp = role_fight:calc_profound_hp(NewGerAttrT),
    GerAttr = NewGerAttrT#gerAttr{gerProHpMax = ProHp},
    GerBase2= transOldGerBase2NewGerBase(GerBase),
	GerSpeed = calc_trans_ger_speed(GerBase2),
    #ger{gerID = GerID, gerBase = GerBase2,
         gerAttr = GerAttr, gerExtra = GerExtra, gerHp = Hp,
         gerSp = Sp, gerProHp = ProHp, gerFamilyTekAdd = FTek,
         gerMagicBookAdd = Magic, gerEnchantAdd = EA,gerSpeed=GerSpeed,
         gerAwakeAdd = GerAwakeAdd, gerCrystalAdd = CrystalAdd}.
calc_trans_ger_speed(#gerBase{gerLevel=Level,gerQuality=Quality,gerTypeID=GerTypeID}) ->
	#data_ger{baseSpeed=Base,addSpeed=Add} = data_ger:get(GerTypeID),
	erlang:trunc(Base + Add * (Level + 3 * (Quality + 10) rem 10 ) ).

transOldGerBase2NewGerBase(GerBase) ->
    NewGerBase=#gerBase{gerAwakeInfo=GerAwakeInfo} = case GerBase of
            Any when is_record(GerBase, gerBase) ->
                Any;
            {gerBase,A,B,C,D,E} ->
                {gerBase,A,B,C,D,E,[],-1,0,#holyGrail{}};
            {gerBase,A,B,C,D,E,F} ->
                {gerBase,A,B,C,D,E,F,-1,0,#holyGrail{}};
            {gerBase,A,B,C,D,E,F,G,H}->
            	{gerBase,A,B,C,D,E,F,G,H,#holyGrail{}}           	
    end,
    GerHolyGrailInfo=case NewGerBase#gerBase.gerHolyGrailInfo of
    	[]->
    		#holyGrail{};
    	X->
    		X
    end,
    NewGerAwakeInfo = [role_awake:transform_oldawake2newawake(GerAwakeUnit)||GerAwakeUnit<-GerAwakeInfo],
    NewGerBase#gerBase{gerAwakeInfo=NewGerAwakeInfo,gerHolyGrailInfo=GerHolyGrailInfo}.


%% ====================================================================
%% Internal functions
%% ====================================================================

test_all_ger() ->
	[new_ger(E,1,0,[], [])||E<-data_ger:get_list(),E < 20000].
	
test_ger_list() ->
	random:seed(erlang:now()),
	RandomPool = [E||E<-data_ger:get_list(),E < 20000],
	IDList = util:random_list2(RandomPool, 6),
	L = lists:zip(lists:seq(1,length(IDList)), IDList),
	[?change_pos(Ger, Pos) || {Pos,ID} <- L, Ger<-[new_ger(ID,1,0,[], IDList)]].

%% 
%% test_cacl(N) ->
%% 	Ger = new_ger(12105, 1, 0, [], []),
%% 	tc:run(N, fun() -> cacl_fight_power(Ger#ger.gerBase,Ger#ger.gerAttr, 0, 0) end).

check_illegal_awakeinfo(GerAwakeInfo) when is_list(GerAwakeInfo)->
	lists:foldl(fun(Awake,Acc)->
		case is_record(Awake,awake) of
			true->
				[Awake|Acc];
			_ ->
				Acc
		end
	end,[],GerAwakeInfo);
check_illegal_awakeinfo(_GerAwakeInfo)->
	[].

get_trSpecial_ids() ->
    #trSpecial{specialID=SpecialID} = role_data:get_trSpecial(),
    case SpecialID of
        0 ->
            [];
        _ ->
            {ID1, ID2} = data_trSpecial:get({specialID, SpecialID}),
            [ID1, ID2]
    end.

%% type id四位数且末位为0的是玩家的精灵
is_role_ger(GerTypeID) ->
    case GerTypeID >= 1000 andalso GerTypeID < 10000 of
        false ->
            ger_lib:is_blink_ger(GerTypeID);
        _ ->
            GerTypeID rem 10 =:= 0 
    end.


%%返回传奇锻造等级返回的全属性加成
%%{攻击,生命,奥义,破甲,法穿,护甲,法抗}（万分比）
get_ger_legend_add(L)->
	get_ger_legend_add(L,{0,0,0,0,0,0,0}).

get_ger_legend_add(?undefined,Acc)->
	Acc;
get_ger_legend_add([],Acc)->
	Acc;
get_ger_legend_add([H|T],{A,B,C,D,E,F,G})->
	{AS,BS,CS,DS,ES,FS,GS} = get_ger_legend_add_single(H),
	get_ger_legend_add(T,{A+AS,B+BS,C+CS,D+DS,E+ES,F+FS,G+GS}).

get_ger_legend_add_single(#item{itemTypeID=ItemTypeID,itemLegendRank=ItemLegendRank})->
	IsMagic = lists:member(ItemTypeID,data_legendary:get(magic)),
	case data_legendary:get({legend_rank,ItemLegendRank,IsMagic}) of
		?undefined->
			{0,0,0,0,0,0,0};
		{_NeedRank,_Cost,Add}->
			case IsMagic of
				false->
					setelement(5,Add,0);
				true->
					setelement(4,Add,0)
			end
	end;
get_ger_legend_add_single([_ItemUID,ItemTypeID,_ItemPos,_ItemLevel,_ItemRank,_ItemGerID,_ItemDecay,_ItemExp,_ItemEnchantType,_ItemEnchantLevel])->
	ItemLegendRank = 0,
	case data_legendary:get({legend_rank,ItemLegendRank}) of
		?undefined->
			{0,0,0,0,0,0,0};
		{_NeedRank,_Cost,Add}->
			case lists:member(ItemTypeID,data_legendary:get(maigc)) of
				false->
					setelement(5,Add,0);
				true->
					setelement(4,Add,0)
			end
	end;
get_ger_legend_add_single([_ItemUID,ItemTypeID,_ItemPos,_ItemLevel,_ItemRank,_ItemGerID,_ItemDecay,_ItemExp,_ItemEnchantType,_ItemEnchantLevel,ItemLegendRank])->
	case data_legendary:get({legend_rank,ItemLegendRank}) of
		?undefined->
			{0,0,0,0,0,0,0};
		{_NeedRank,_Cost,Add}->
			case lists:member(ItemTypeID,data_legendary:get(maigc)) of
				false->
					setelement(5,Add,0);
				true->
					setelement(4,Add,0)
			end
	end.
	
add_legend_rank_add(Ger21,{AA,HPA,PMA,PDBA,MDBA,PDA,MDA})->
	#ger{gerAttr=GerAttr} = Ger21,
	#gerAttr{gerAttack=Attack,gerHpMax=HP,gerProMean=PM,gerPhyDefBite=PDB,gerMagDefBite=MDB,gerPhyDef=PD,gerMagDef=MD} = GerAttr,
	NewAttack = trunc(Attack * (10000+AA)/10000),
	NewHP = trunc(HP*(10000+HPA)/10000),
	NewPM = trunc(PM*(10000+PMA)/10000),
	NewPDB = trunc(PDB*(10000+PDBA)/10000),
	NewMDB = trunc(MDB*(10000+MDBA)/10000),
	NewPD = trunc(PD*(10000+PDA)/10000),
	NewMD = trunc(MD*(10000+MDA)/10000),
	Ger21#ger{gerAttr=GerAttr#gerAttr{gerAttack=NewAttack,gerHpMax=NewHP,gerProMean=NewPM,gerPhyDefBite=NewPDB,gerMagDefBite=NewMDB,gerPhyDef=NewPD,gerMagDef=NewMD}}.

calculate_ger_buffskill_addattr(BuffSkills) when is_list(BuffSkills)->
	calculate_ger_buffskill_addattr(BuffSkills,#add_attr{});
calculate_ger_buffskill_addattr(BuffSkills)->
	?ERR("not list buffSkills:~w ~n",[BuffSkills]),
	#add_attr{}.
calculate_ger_buffskill_addattr([],Acc)->
	Acc;
calculate_ger_buffskill_addattr([H|T],Acc)->
	case data_skill:get(H) of
		?undefined->
			?ERR("not find skillID:~w ~n",[H]),
			calculate_ger_buffskill_addattr(T,Acc);
		#data_skill{gerEnterOrGodAdd=AddBuff}->
			calculate_ger_buffskill_addattr(T,append_add_attr(Acc,transOldAddAttr2NewAddAttr(AddBuff)))
	end.
	