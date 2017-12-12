%% @author caohongyang
%% @doc 武将API
%% Created 2013-3-6


-module(ger_lib).

-include("def_role.hrl").
-compile(export_all).
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

is_ger_material_match(Ger, GerTypeID) ->
    #gerSimple{gerLevel=GerLevel,gerQuality=GerRank,gerTypeID=T} = Ger,
    T == GerTypeID andalso GerLevel < 10 andalso GerRank < 1.

check_ger_material(GerBag, GerTypeID, Num) ->
    check_ger_material(GerBag, GerTypeID, Num, [],[]).


check_ger_material(GerBag, _GerTypeID, 0, Result,DelAcc)->
    {true, GerBag++Result, DelAcc};
check_ger_material([], _GerTypeID, _Num, _Result,_DelAcc)->
    false;
check_ger_material([Ger|GerBag], GerTypeID, Num, Result, DelAcc) ->
    case is_ger_material_match(Ger, GerTypeID) of
        true ->
            check_ger_material(GerBag, GerTypeID, Num-1, Result, [Ger|DelAcc]);
        false ->
            check_ger_material(GerBag, GerTypeID, Num, [Ger|Result], DelAcc)
    end.




gerList2p_ger_pos(PosList) ->
    [#p_ger_pos{gerID=GerID,gerPos=GerPos}||#ger{gerID=GerID,gerBase=#gerBase{gerPos=GerPos}} <- PosList].

ger2gerSimple(Ger) ->
    #ger{gerBase=GerBase}=Ger,
    #gerSimple{
               gerQuality=GerBase#gerBase.gerQuality,
               gerTypeID=GerBase#gerBase.gerTypeID,
               gerLevel=GerBase#gerBase.gerLevel,
               gerID=Ger#ger.gerID,
               gerExp=GerBase#gerBase.gerExp,
               gerAwakeInfo=GerBase#gerBase.gerAwakeInfo,
               gerCrystalInfo=GerBase#gerBase.gerCrystalInfo,
               gerHolyGrailInfo=GerBase#gerBase.gerHolyGrailInfo}.


%% @doc 武将加经验
-spec add_exp_and_notify(GerID :: ?int64, 
                         AddExp :: ?int32, 
                         RoleLevel :: ?int16,
                         GerList ::[#ger{}]) ->
          {IsLevelUpgraded::boolean(), 
           NewGer:: #ger{}, 
           RealAddExp :: ?int32}.
add_exp_and_notify(Ger, AddExp, RoleLevel, GerList) ->
    #ger{gerBase=GerBase,gerID=GerID}= Ger ,
    #gerBase{gerExp=GerExp,gerLevel=GerLevel,gerTypeID=GerTypeID} = GerBase,
    case add_exp(GerLevel, GerExp, AddExp, RoleLevel,GerTypeID) of
        {level_up, Level2, Exp2} ->
            #ger{gerBase=GerBase,gerAttr=_GerAttr} = Ger,
            Ger2  = Ger#ger{gerBase=GerBase#gerBase{gerLevel=Level2,gerExp=Exp2}},
            Ger3 = ger_attr:recacl(Ger2, GerList),
            notify_update(Ger3, GerLevel, Level2),
            role_homestead:hook_ger_levelup(GerID, Level2),
            {true, Ger3, Exp2-GerBase#gerBase.gerExp};
        {level_not_up, _Level2, Exp2} ->
            #ger{gerBase=GerBase} = Ger,
            Ger2  = Ger#ger{gerBase=GerBase#gerBase{gerExp=Exp2}},
            ?sendself(#sc_ger_update_exp{gerExp=Exp2,gerID=GerID}),
            {false, Ger2, Exp2-GerBase#gerBase.gerExp};
        {level_max,_Level2,_Exp2} ->
            {false, Ger, 0}
    end.

add_exp_and_notify_lieu(Ger, AddExp, RoleLevel, GerList) ->
    #ger{gerBase=GerBase,gerID=GerID}= Ger ,
    #gerBase{gerExp=GerExp,gerLevel=GerLevel,gerTypeID=GerTypeID} = GerBase,
    case add_exp(GerLevel, GerExp, AddExp, RoleLevel,GerTypeID) of
        {level_up, Level2, Exp2} ->
            #ger{gerBase=GerBase,gerAttr=_GerAttr} = Ger,
            Ger2  = Ger#ger{gerBase=GerBase#gerBase{gerLevel=Level2,gerExp=Exp2}},
            Ger3 = ger_attr:recacl_lieu(Ger2, GerList),
            notify_update(Ger3, GerLevel, Level2),
            role_homestead:hook_ger_levelup(GerID, Level2),
            {true, Ger3, Exp2-GerBase#gerBase.gerExp};
        {level_not_up, _Level2, Exp2} ->
            #ger{gerBase=GerBase} = Ger,
            Ger2  = Ger#ger{gerBase=GerBase#gerBase{gerExp=Exp2}},
            ?sendself(#sc_ger_update_exp{gerExp=Exp2,gerID=GerID}),
            {false, Ger2, Exp2-GerBase#gerBase.gerExp};
        {level_max,_Level2,_Exp2} ->
            {false, Ger, 0}
    end.

add_exp_and_notify2(GerSimple, AddExp, RoleLevel) ->
    #gerSimple{gerID=GerID,gerExp=GerExp,gerLevel=GerLevel,gerTypeID=GerTypeID} = GerSimple,
    case add_exp(GerLevel, GerExp, AddExp, RoleLevel,GerTypeID) of
        {level_up, Level2, Exp2} ->
            GerSimple2  = GerSimple#gerSimple{gerLevel=Level2,gerExp=Exp2},
            notify_update(GerSimple2, GerLevel, Level2),
            role_homestead:hook_ger_levelup(GerID, Level2),
            GerSimple2;
        {level_not_up, _Level2, Exp2} ->
            GerSimple2  = GerSimple#gerSimple{gerExp=Exp2},
            ?sendself(#sc_ger_update_exp{gerExp=Exp2,gerID=GerID}),
            GerSimple2;
        {level_max,_Level2,_Exp2} ->
            GerSimple
    end.

notify_update(#ger{gerBase=GerBase}=Ger, OldLevel, NewLevel) ->
    notify_update(Ger),
    on_lvl_up(GerBase#gerBase.gerTypeID, OldLevel, NewLevel);

notify_update(Ger, OldLevel, NewLevel) ->
    notify_update(Ger),
    on_lvl_up(Ger#gerSimple.gerTypeID, OldLevel, NewLevel).

notify_update(#ger{} = Ger) ->
    #ger{gerBase=GerBase,gerAttr=GerAttr}=Ger,
    GerDemageRate=get_ger_skill_plus(Ger),
    Notify = #sc_ger_update{
                            gerID=Ger#ger.gerID,
                            gerQuality=GerBase#gerBase.gerQuality,
                            gerLevel=GerBase#gerBase.gerLevel,
                            gerAttack=GerAttr#gerAttr.gerAttack,
                            gerHpMax=GerAttr#gerAttr.gerHpMax,
                            gerFightPower=GerAttr#gerAttr.gerFightPower,
                            gerExp=GerBase#gerBase.gerExp,
                            gerProMean=GerAttr#gerAttr.gerProMean,
                            gerDemageRate=GerDemageRate,
                            gerSpeed = GerAttr#gerAttr.gerSpeed
                           },
    ?sendself(Notify);

notify_update(Ger) ->
    GerDemageRate=get_ger_skill_plus(Ger),
    Notify = #sc_ger_update{
                            gerID=Ger#gerSimple.gerID,
                            gerQuality	=Ger#gerSimple.gerQuality,	
                            gerLevel		=Ger#gerSimple.gerLevel,	
                            gerAttack	=0,
                            gerHpMax		=0,
                            gerFightPower=0,
                            gerExp 		=Ger#gerSimple.gerExp,
                            gerProMean   =0,
                            gerDemageRate=GerDemageRate,
                            gerSpeed=0
                           },
    ?sendself(Notify).

on_lvl_up(GerTypeID, OldLevel, NewLevel) ->
    case data_ger:get(GerTypeID) of
        #data_ger{gerStar=Gerstar} ->
            role_lvlSgAttr:on_ger_lvl_up(Gerstar, OldLevel, NewLevel);
        _ ->
            ignore
    end.

%% @doc #ger{}结构转化为#p_ger{}
ger2p_ger(#ger{}=Ger) ->
    #ger{gerAttr=GerAttr,gerBase=GerBase,gerID=GerID} = Ger,
    GerAwakeInfo = GerBase#gerBase.gerAwakeInfo,
    GerHolyGrailInfo = GerBase#gerBase.gerHolyGrailInfo,
    GerDemageRate=get_ger_skill_plus(Ger),
    #p_ger{gerID		=GerID,
           gerTypeID	=GerBase#gerBase.gerTypeID,
           gerQuality	=GerBase#gerBase.gerQuality,	
           gerLevel		=GerBase#gerBase.gerLevel,	
           gerAttack	=GerAttr#gerAttr.gerAttack,	
           gerHpMax		=GerAttr#gerAttr.gerHpMax,
           gerFightPower=GerAttr#gerAttr.gerFightPower,
           gerExp 		=GerBase#gerBase.gerExp,
           %%此处修改了下
           % gerProMean   =GerAttr#gerAttr.gerProHpMax,
           gerProMean   =GerAttr#gerAttr.gerProMean,
           gerAwakeInfo =role_awake:trans_awakeInfo2p_awake_info(GerAwakeInfo),
           gerDemageRate=GerDemageRate,
           gerBody      =GerBase#gerBase.gerBody,
           gerSpeed     =GerAttr#gerAttr.gerSpeed,
           gerHolyGrailLevel = GerHolyGrailInfo#holyGrail.holyGrailLevel
          };
ger2p_ger(#gerSimple{}=Ger) ->
    GerDemageRate=get_ger_skill_plus(Ger),
    #p_ger{gerID		=Ger#gerSimple.gerID,
           gerTypeID	=Ger#gerSimple.gerTypeID,
           gerQuality	=Ger#gerSimple.gerQuality,	
           gerLevel		=Ger#gerSimple.gerLevel,	
           gerAttack	=0,
           gerHpMax		=0,
           gerFightPower=0,
           gerExp 		=Ger#gerSimple.gerExp,
           gerProMean   =0,
           gerAwakeInfo =role_awake:trans_awakeInfo2p_awake_info(Ger#gerSimple.gerAwakeInfo),
           gerDemageRate=GerDemageRate,
           gerBody      =Ger#gerSimple.gerBody,
           gerSpeed     =0,
           gerHolyGrailLevel = Ger#gerSimple.gerHolyGrailInfo#holyGrail.holyGrailLevel
          };
ger2p_ger(Ger)->
    NewGer = ger_attr:transOldGer2NewGer(Ger),
    ger2p_ger(NewGer).

%% @doc 初始化data_mon配置
data_mon_list2ger_list(DataMonList) ->
    [data_mon2ger(DataMon) || DataMon <- DataMonList].
data_mon2ger(DataMon) when is_record(DataMon, data_mon) ->
    #data_mon{gerTypeID=GerID} = DataMon,
    GerBase = #gerBase{
                       gerTypeID            =GerID       
                       ,gerQuality           =DataMon#data_mon.gerQuality      
                       ,gerLevel             =DataMon#data_mon.gerLevel        
                       ,gerPos               =0
                       ,gerExp               =0
                      },
    GerAttr = #gerAttr{
                       gerProMean           =DataMon#data_mon.gerProMean     
                       ,gerProMeanAddtion    =DataMon#data_mon.gerProMeanAddtion     
                       ,gerAttckToProMean    =DataMon#data_mon.gerAttckToProMean      
                       ,gerHpToProMean       =DataMon#data_mon.gerHpToProMean
                       ,gerAttack            =DataMon#data_mon.gerAttack      
                       ,gerHpMax             =DataMon#data_mon.gerHpMax       
                       ,gerSpInit            =DataMon#data_mon.gerSpInit      
                       ,gerSpMax            =DataMon#data_mon.gerSpMax      
                       ,gerCritic            =DataMon#data_mon.gerCritic      
                       ,gerCriticReduce      =DataMon#data_mon.gerCriticReduce
                       ,gerDoom              =DataMon#data_mon.gerDoom        
                       ,gerMiss              =DataMon#data_mon.gerMiss        
                       ,gerAbsorb            =DataMon#data_mon.gerAbsorb      
                       ,gerDamageBack        =DataMon#data_mon.gerDamageBack  
                       ,gerReel              =DataMon#data_mon.gerReel        
                       ,gerReelReduce        =DataMon#data_mon.gerReelReduce  
                       ,gerPhyDefBite        =DataMon#data_mon.gerPhyDefBite  
                       ,gerPhyDef            =DataMon#data_mon.gerPhyDef      
                       ,gerMagDefBite        =DataMon#data_mon.gerMagDefBite  
                       ,gerMagDef            =DataMon#data_mon.gerMagDef
                      },
    #ger{
         gerID=GerID
         ,gerBase=GerBase
         ,gerAttr=GerAttr
         ,gerHp=DataMon#data_mon.gerHpMax
         ,gerSp=DataMon#data_mon.gerSpInit
        }.


%% @doc 
data_ger2ger(DataGer, GerLevel) when is_record(DataGer, data_ger) ->
    #data_ger{gerTypeID=GerID} = DataGer,
    GerBase = #gerBase{
                       gerTypeID            =GerID    
                       ,gerPos               =0
                       ,gerExp               =data_ger_level:get(GerLevel)
                      },
    GerAttr = #gerAttr{
                       gerProMean           =DataGer#data_ger.gerProMean     
                       ,gerProMeanAddtion    =DataGer#data_ger.gerProMeanAddtion     
                       ,gerAttckToProMean    =DataGer#data_ger.gerAttckToProMean      
                       ,gerHpToProMean       =DataGer#data_ger.gerHpToProMean
                       ,gerAttack            =DataGer#data_ger.gerAttack      
                       ,gerHpMax             =DataGer#data_ger.gerHpMax       
                       ,gerSpInit            =DataGer#data_ger.gerSpInit      
                       ,gerSpMax            =DataGer#data_ger.gerSpMax      
                       ,gerCritic            =DataGer#data_ger.gerCritic      
                       ,gerCriticReduce      =DataGer#data_ger.gerCriticReduce
                       ,gerDoom              =DataGer#data_ger.gerDoom        
                       ,gerMiss              =DataGer#data_ger.gerMiss        
                       ,gerAbsorb            =DataGer#data_ger.gerAbsorb      
                       ,gerDamageBack        =DataGer#data_ger.gerDamageBack  
                       ,gerReel              =DataGer#data_ger.gerReel        
                       ,gerReelReduce        =DataGer#data_ger.gerReelReduce  
                       ,gerPhyDefBite        =DataGer#data_ger.gerPhyDefBite  
                       ,gerPhyDef            =DataGer#data_ger.gerPhyDef      
                       ,gerMagDefBite        =DataGer#data_ger.gerMagDefBite  
                       ,gerMagDef            =DataGer#data_ger.gerMagDef
                      },
    #ger{
         gerID=GerID
         ,gerBase=GerBase
         ,gerAttr=GerAttr
         ,gerHp=DataGer#data_ger.gerHpMax
         ,gerSp=DataGer#data_ger.gerSpInit
        }.
%% @doc 加经验，计算等级成长，
-spec add_exp(?int16, ?int64, ?int32, ?int16,?int16) -> 
          {level_up, NewLevel::?int16, NewExp::?int64} | 
              {level_not_up, OldLevel::?int16, NewExp::?int64} | 
              {level_max, OldLevel::?int16, OldExp::?int64}.
add_exp(Level, Exp, AddExp, RoleLevel,GerTypeID) ->
    MaxLevel = erlang:min(RoleLevel, data_common:get(max_ger_level)),
    case ger_lib:is_blink_ger(GerTypeID) of
      false->
        MaxExp = data_ger_level:get(MaxLevel+1)-1;
      true->
        MaxExp = data_ger_level_light:get(MaxLevel+1)-1
    end,
    if MaxExp > Exp ->			
           #data_ger{sameType=SameType}=data_ger:get(GerTypeID),
           NewExp=erlang:min(MaxExp, Exp+AddExp),
           NewLevel = case SameType of
                          GerTypeID ->data_ger_exp:get(NewExp);
                          _ -> data_ger_exp_light:get(NewExp)
                      end,
           if NewLevel > Level ->
                  {level_up, NewLevel, NewExp};
              true ->
                  {level_not_up, Level, NewExp}
           end;
       true ->
           {level_max, Level, Exp}
    end.

ger2p_ger_view(#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}}=_Ger) ->
    #p_ger_view{gerQuality=GerQuality,gerLevel=GerLevel,gerTypeID=GerTypeID};
ger2p_ger_view(#gerSimple{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}=_Ger) ->
    #p_ger_view{gerQuality=GerQuality,gerLevel=GerLevel,gerTypeID=GerTypeID}.

ger2p_ger_view_dtl(#ger{gerID=GerID
				  , gerBase=#gerBase{gerTypeID=GerTypeID, gerLevel=GerLevel, gerQuality=GerQuality, gerExp=GerExp ,gerAwakeInfo=GerAwakeInfo,gerBody=GerBody,gerHolyGrailInfo=#holyGrail{holyGrailLevel=HolyGrailLevel}}
				  , gerAttr=#gerAttr{gerAttack=GerAttack, gerHpMax=GerHpMax,gerFightPower=GerFightPower,gerProMean=GerProMean,gerSpeed=GerSpeed}}=Ger)->
	
	GerDemageRate=get_ger_skill_plus(Ger),
	#p_ger{gerID = GerID, gerTypeID=GerTypeID, gerLevel=GerLevel, gerQuality=GerQuality
		  ,gerExp=GerExp, gerAttack=GerAttack, gerHpMax=GerHpMax,gerFightPower=GerFightPower,gerProMean=GerProMean
          ,gerAwakeInfo=role_awake:trans_awakeInfo2p_awake_info(GerAwakeInfo),gerDemageRate=GerDemageRate
          ,gerBody = GerBody,gerSpeed=GerSpeed,gerHolyGrailLevel=HolyGrailLevel}.

ger2ger_crystalinfo_brief(#ger{gerID=GerID,gerBase=#gerBase{gerCrystalInfo=-1,gerTypeID=GerTypeID}})->
    #data_ger{gerProperty=GerProperty} = data_ger:get(GerTypeID),
    MagicType = case role_awake:is_magic(GerProperty) of true->1;false->2 end,
    #ger_crystalinfo_brief{gerID=GerID,crystalbrieflist=[],type=MagicType};
ger2ger_crystalinfo_brief(#ger{gerID=GerID,gerBase=#gerBase{gerCrystalInfo=GerCrystalInfo1,gerTypeID=GerTypeID}})->
    GerCrystalInfo = role_crystal:sort_crystal(GerCrystalInfo1),
    PCrystalInfoBriefList = [#p_crystalinfo_brief{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel}||#crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel}<-GerCrystalInfo],
    #data_ger{gerProperty=GerProperty} = data_ger:get(GerTypeID),
    MagicType = case role_awake:is_magic(GerProperty) of true->1;false->2 end,
    #ger_crystalinfo_brief{gerID=GerID,crystalbrieflist=PCrystalInfoBriefList,type=MagicType}.

ger2p_ger_pos(#ger{gerID=GerID,gerBase=#gerBase{gerPos=GerPos}})->
    #p_ger_pos{gerID=GerID,gerPos=GerPos}.

new_ger2p_ger_view(#new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}) ->
    #p_ger_view{gerQuality=GerQuality,gerLevel=GerLevel,gerTypeID=GerTypeID}.

new_gerList2p_ger_view(#new_ger{}=NewGer) ->
    [new_ger2p_ger_view(NewGer)];
new_gerList2p_ger_view(List) when is_list(List) ->
    [new_ger2p_ger_view(E)||E<-List];
new_gerList2p_ger_view(_) ->
    [].

p_ger_view2new_gerList(#p_ger_view{}=PGerView) ->
    [p_ger_view2new_ger(PGerView)];
p_ger_view2new_gerList(PGerView) when is_list(PGerView) ->
    [p_ger_view2new_ger(GerView)||GerView<-PGerView];
p_ger_view2new_gerList(_) ->
    [].

p_ger_view2new_ger(#p_ger_view{gerQuality=GerQuality,gerLevel=GerLevel,gerTypeID=GerTypeID}) ->
    #new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}.

add_ger_list(NewGerList, Type, ArgID, Desc)->
    add_ger_list(NewGerList, Type, ArgID, Desc,true).



add_ger_list(NewGerList, Type, ArgID, Desc,NeedTriger) when is_list(NewGerList) ->
    GerSimpleList = gen_ger_list(NewGerList),
    add_ger_list2(GerSimpleList),
    GerGatherList = [{E1,E2}||#new_ger{gerTypeID=E1,gerQuality=E2} <- NewGerList],
    role_gather:hook_add_ger_list(GerGatherList),
    role_gather:hook_add_ger_list_for_manual(GerGatherList),
    LogList = [ [E#gerSimple.gerID,E#gerSimple.gerTypeID,E#gerSimple.gerLevel,E#gerSimple.gerQuality ]|| E<-GerSimpleList],
    RoleID = role_data:get_roleID(),
    {Date, _} = Time = erlang:localtime(),
    behavior_ger_add:log(RoleID, LogList, Date, Time, Type, ArgID, Desc),
    case NeedTriger of 
        true ->
            role_task_trigger:add_ger_trigger(GerSimpleList);
        false ->
            ignore
    end,
    GerSimpleList;

add_ger_list(NewGer, Type, ArgID, Desc,NeedTriger) ->
    add_ger_list([NewGer], Type, ArgID, Desc,NeedTriger).

add_ger(NewGer, Type, ArgID, Desc) ->
    [GerSimple] = add_ger_list([NewGer], Type, ArgID, Desc),
    GerSimple.

gen_ger_list(NewGerList) ->
    [gen_ger_list2(E) || E<- NewGerList].
gen_ger_list2(NewGer) ->
    #new_ger{gerLevel=GerLevel,gerQuality=GerQuality,gerTypeID=GerTypeID} = NewGer,
    #gerSimple{gerExp=data_ger_level:get(GerLevel),gerLevel=GerLevel,gerID=tk_id:gen_gerID(),gerPos=0,gerQuality=GerQuality,gerTypeID=GerTypeID}.

add_ger_list2(GerSimpleList) ->
    GerBag = role_data:get_gerBag(),
    GerBag2 = GerSimpleList++GerBag,
    role_data:set_gerBag(GerBag2),
    List = lists:map(fun(E)->
                             ger2p_ger(E)
                     end, GerSimpleList),
    Notify = #sc_ger_new_list{newGerList=List},
    ?sendself(Notify).
%% 	lists:foreach(fun(E) ->
%% 						  Notify = #sc_ger_new{newGer=ger2p_ger(E)},
%% 						  ?sendself(Notify)
%% 				  end, GerSimpleList).

get_ger_skills(GerTypeID, GerQuality) ->
    #data_ger{gerSkillList=SkillList} = data_ger:get(GerTypeID),
    SkillList1 = [SkillID||{NeedQuality,SkillID}<-SkillList,NeedQuality =< GerQuality],
    {NormalSkill, UniqueSkill, WakeSkill, GodSkill, EnterSkill,Unique2Skill,God2Skill,Unique3Skill,BuffSkill,Normal2Skill} = 
        lists:foldl(fun(E, {Normal, Unique, Wake, God, Enter,Unique2,God2,Unique3,Buff,Normal2}=Acc)->
                            if E =/= 0 ->
                                   #data_skill{skillType=SkillType}=data_skill:get(E),
                                   case SkillType of
                                       enter ->
                                           {Normal, Unique, Wake, God, [E|Enter], Unique2, God2,Unique3,Buff,Normal2};
                                       god ->
                                           {Normal, Unique, Wake, [E|God], Enter, Unique2, God2,Unique3,Buff,Normal2};
                                       normal ->
                                           {[E|Normal], Unique, Wake, God, Enter, Unique2, God2,Unique3,Buff,Normal2};
                                       wake ->
                                           {Normal, Unique, [E|Wake], God, Enter, Unique2, God2,Unique3,Buff,Normal2};
                                       god2 ->
                                           {Normal, Unique, Wake, God, Enter, Unique2, [E|God2],Unique3,Buff,Normal2};
                                       unique ->
                                           {Normal, [E|Unique], Wake, God, Enter, Unique2, God2,Unique3,Buff,Normal2};
                                       unique2 ->
                                           {Normal, Unique, Wake, God, Enter, [E|Unique2], God2,Unique3,Buff,Normal2};
                                       unique3 ->
                                           {Normal, Unique, Wake, God, Enter, Unique2, God2,[E|Unique3],Buff,Normal2};
                                       %%v4.0版本增加对属性的增强的技能，故添加buff类型技能 （对觉醒5的影响的技能也放入这个里面，在战斗中筛选加成）
                                       buff ->
                                           {Normal, Unique, Wake, God, Enter, Unique2, God2,Unique3,[E|Buff],Normal2};
                                       normal2 ->
                                           {Normal, Unique, Wake, God, Enter, Unique2, God2,Unique3,Buff,[E|Normal2]};
                                       X->
                                           ?ERR("other type :~w",[X])
                                   end;
                               true->
                                   Acc 
                            end
                    end, {[],[],[],[],[],[],[],[],[],[]}, SkillList1),
%%     if 
%%         GerQuality >= ?UNIQUE3_REPLACE_UNIQUE2_NEED_QUALITY andalso Unique3Skill =/= [] -> 
%%             UniqueSkill2 = Unique3Skill;
%% 	   true ->
%%             if 
%%                 GerQuality >= ?UNIQUE2_REPLACE_UNIQUE_NEED_QUALITY andalso Unique2Skill =/= [] -> 
%%                     UniqueSkill2 = Unique2Skill;
%%                 true -> 
%%                     UniqueSkill2 = UniqueSkill
%% 		   end
%% 	end,
%%         if GerQuality >= ?NORMAL2_REPLACE_NORMAL_NEED_QUALITY andalso Normal2Skill =/= [] -> NormalSkill2 = Normal2Skill;
%%        true -> NormalSkill2 = NormalSkill
%%     end,
    UniqueSkill2 = if Unique3Skill /= [] -> Unique3Skill;
                      Unique2Skill /= [] -> Unique2Skill;
                      true -> UniqueSkill
                   end,
    NormalSkill2 = if Normal2Skill /= [] -> Normal2Skill;
                      true -> NormalSkill
                   end,
    #gerSkill{normal=NormalSkill2, unique=UniqueSkill2,wake=WakeSkill, god=GodSkill, god2=God2Skill, enter=EnterSkill,buff=BuffSkill}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-include("def_item.hrl").
%% 计算最强阵容


get_destiny_equip(GerTypeID) ->
    #data_ger{destinyIDList=DL} = data_ger:get(GerTypeID),
    lists:foldl(fun(E,Acc) ->
                        #data_destiny{destinyNeedList=ItemTypeIDList,destinyType=DT}=data_destiny:get(E),
                        if DT =:= 2->
                               ItemTypeIDList ++ Acc;
                           true ->
                               Acc
                        end
                end, [], DL).

best_equip(?weapon)->
    11010;
best_equip(?armor) ->
    12004;
best_equip(?horse) ->
    13004.

best_equip_for_ger(GerTypeID) ->
    DestEquipIDList = get_destiny_equip(GerTypeID),
    DestEquipTypeList = [T||E<-DestEquipIDList, #data_item{itemType=T}<-[data_item:get(E)]],
    RestEquipTypeList = [?weapon,?armor, ?horse]--DestEquipTypeList,
    EquipIDList = DestEquipIDList ++ [best_equip(E)||E<-RestEquipTypeList],
    Treasure = [14010,14002,14012],
    List =
        [hd(item_lib:create_equip_with_attr(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_MAIN_EQUIP,itemTypeID=E}, data_item:get(E)))||E<-EquipIDList] ++
            [hd(item_lib:create_equip_with_attr(#new_item{itemLevel=104,itemNum=1,itemRank=?MAX_RANK_OF_TREASURE,itemTypeID=E}, data_item:get(E)))||E<-Treasure],
    {_,L}=lists:foldl(fun(E,{Pos, Acc}) ->
                              {Pos+1, [E#item{itemPos=Pos}|Acc]}
                      end, {1, []}, List),
    L.

good_equip_list(Type) ->
    MaxItemLevel = data_common:get(max_role_level)+4,
    [hd(item_lib:create_equip(#new_item{itemLevel=MaxItemLevel,itemNum=1,itemRank=19,itemTypeID=ItemTypeID},DataItem))||
       ItemTypeID <- data_item:get_list(),
       #data_item{itemType=ItemType,itemStar=ItemStar} = DataItem<-[data_item:get(ItemTypeID)],
       ItemType =:= Type, 
       ItemStar=:= 4].


%% 比较两把武器，哪个厉害点
is_equip_better_than(ItemTypeID1, ItemTypeID2) ->
    EquipList1 = item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=19,itemTypeID=ItemTypeID1}),
    
    EquipList2 = item_lib:create_equip(#new_item{itemLevel=104,itemNum=1,itemRank=19,itemTypeID=ItemTypeID2}),
    
    is_equip_better_than2(EquipList1, EquipList2).
is_equip_better_than2(EquipList1, EquipList2) ->
    L=[ger_lib:is_equip_better_than2(EquipList1,EquipList2,E)||E<-data_ger:get_list(),E<20000],
    R = length([E||E<-L,E])/length(L),
    io:format("~w\n",[R]),
    R > 0.5.
is_equip_better_than2(EquipList1, EquipList2, TestGerTypeID) ->
    %% 生成测试用队伍
    GenTeam = fun(EquipList) ->
                      Team = [?change_pos(Ger, Pos)||Pos<-lists:seq(1,6), Ger <-[ger_attr:new_ger(TestGerTypeID, 100, 19, EquipList, [])] ],
                      Team
              end,
    is_better_than(GenTeam(EquipList1), GenTeam(EquipList2)).

get_best_equip(ItemType) ->
    EquipList = good_equip_list(ItemType),
    L = lists:sort(fun(A,B) -> is_equip_better_than2([A],[B]) end,EquipList),
    [E||#item{itemTypeID=E}<-L].

%% 比较两个队伍，哪个厉害点
is_better_than(Team1, Team2) ->
    {Result1, St1, {_,_,_,_}} = role_fight:new(Team1, Team2,#add_attr{},#add_attr{}),
    {Result2, St2, {_,_,_,_}} = role_fight:new(Team2, Team1,#add_attr{},#add_attr{}),
    %% Team1连输两次
    if Result1 =:= false andalso Result2 =:= true ->		
           false;
       %% Team1 连赢两次
       Result1 =:= true andalso Result2 =:= false ->
           true;
       %% 各有胜负时，比较谁杀得快
       true ->
           F = fun(M) ->length(M#sc_fight_request.actionList) end,
           %% 先手必胜
           if Result1 =:= true ->
                  io:format("tag...\n"),
                  F(St1) >= F(St2);
              true ->
                  F(St2) >= F(St1)
           end
    end.

%% 给出带全体攻击的武将
%% skill_all_ger_list() ->
%% 	lists:foldl(fun(E,Acc) ->
%% 						if E > 20000 ->
%% 							   Acc;
%% 						   true ->
%% 						#data_ger{gerSkill2=Skill} = data_ger:get(E),
%% 						#data_skill{targetSelect=TS}=data_skill:get(Skill),
%% 						if TS =:= all ->
%% 							   [E|Acc];
%% 						   true ->
%% 							   Acc
%% 						end
%% 						end
%% 				end, [], data_ger:get_list()).

%%计算精灵所有附加属性增加的必杀以及普攻伤害系数
%%return p_demage_rate_unit list
get_ger_skill_plus(Ger) when is_record(Ger,ger)->
    AddAttr = lists:foldl(fun(E,Acc)->ger_attr:append_add_attr(E,Acc) end,#add_attr{},[Ger#ger.gerAwakeAdd,Ger#ger.gerCrystalAdd]),
    [#p_demage_rate_unit{type=1,uniqueRate=AddAttr#add_attr.gerUskillDemageRate,normalRate=AddAttr#add_attr.gerGskillDemageRate},#p_demage_rate_unit{type=2,normalRate=0,uniqueRate=0}];
get_ger_skill_plus(Ger) when is_record(Ger,gerSimple)->
    {AddAttr1,CrystalAdd} = ger_attr:recacl_awake_addattr(Ger#gerSimple.gerAwakeInfo,Ger#gerSimple.gerCrystalInfo),
    AddAttr = lists:foldl(fun(E,Acc)->ger_attr:append_add_attr(E,Acc) end,#add_attr{},[AddAttr1,CrystalAdd]),
    [#p_demage_rate_unit{type=1,uniqueRate=AddAttr#add_attr.gerUskillDemageRate,normalRate=AddAttr#add_attr.gerGskillDemageRate},#p_demage_rate_unit{type=2,normalRate=0,uniqueRate=0}];
get_ger_skill_plus(_Ger)->
    [#p_demage_rate_unit{type=1,uniqueRate=0,normalRate=0},#p_demage_rate_unit{type=2,uniqueRate=0,normalRate=0}].

ra_ger_delete([])->
    ignore;
ra_ger_delete(L)->
    GerIDList =  [GerID||#gerSimple{gerID = GerID}<-L],
    ?sendself(#sc_ger_del{gerIDList=GerIDList}).

is_blink_ger(GerTypeID)->
    #data_ger{sameType=SameType} = data_ger:get(GerTypeID),
    SameType=/= 0 andalso SameType=/=GerTypeID. 
