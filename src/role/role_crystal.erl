%% @author zcl
%% @doc 晶体系统
%% Created 2015-03-28
%%============================================%%
%%（1）升级和升品只能处理上阵精灵
%%（2）
%%
%%
-module(role_crystal).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").

-define(ROLELEVELLIMIT,1).
-define(GERQUALITYLIMIT,2).
-define(GERAWAKELIMIT,3).
-define(STARLIMIT,4).

-define(LOG_GER_CRYSTAL_UPLEVEL_OPERATIONCODE,1).
-define(LOG_GER_CRYSTAL_UPRANK_OPERATIONCODE,2).
-define(LOG_GER_CRYSTAL_GER_DOWN_RANK_OPERATIONCODE,3).
-define(LOG_GER_CRYSTAL_GER_INIT_OPERATIONCODE,4).
-define(LOG_GER_CRYSTAL_BLINK_INHERIT_OPERATIONCODE,5).
-define(LOG_GER_CRYSTAL_GER_RECYCLE,6).

-define(CRYSTALTYPE_ARMOR_OR_MAGIC,6).
%%========================================协议处理接口=========================================%%
cs_crystal_uplevel(#cs_crystal_uplevel{crystaltype=CrystalType,crystalexpinc=CrystalExpInc,gerUID=GerUID})->
  case check_crystal_uplevel(GerUID,CrystalType,CrystalExpInc) of
    {false,Reason}->
      IsNeedUpRank = case Reason of 4->1;_ ->0 end,
      ?sendself(#sc_crystal_uplevel{result=Reason,crystaltype=CrystalType,crystalinfo=#p_crystalinfo{crystaltype=0,crystalquality=0,crystallevel=0,crystalexp=0,crystalrankexp=0},gerUID=GerUID,isneeduprank=IsNeedUpRank});
    {true,Ger,Other,Cost,NewExp,NewLevel}->
      {NewGer,NewCrystal} = do_crystal_uplevel(Ger,CrystalType,Cost,NewExp,NewLevel),
      NewGer2 = ger_attr:recacl(NewGer,Other),        
      PosList4 = ger_attr:refresh_fightPower([NewGer2|Other]),
      role_data:set_posList(PosList4),
      IsNeedUpRank = case is_max_level(NewCrystal#crystal.crystaltype,NewCrystal#crystal.crystalquality,NewCrystal#crystal.crystallevel) of true->1;false->0 end,
      ?sendself(#sc_crystal_uplevel{result=1,crystaltype=CrystalType,crystalinfo=trans_crystal2p_crystalinfo(NewCrystal),gerUID=GerUID,isneeduprank=IsNeedUpRank})
  end.

cs_crystal_uprank(#cs_crystal_uprank{crystaltype=CrystalType,crystalexpinc=CrystalRankExpInc,gerUID=GerUID})->
  case check_crystal_uprank(GerUID,CrystalType,CrystalRankExpInc) of
    {false,Reason}->
      ?sendself(#sc_crystal_uprank{result=Reason,crystaltype=CrystalType,crystalinfo=#p_crystalinfo{crystaltype=0,crystalquality=0,crystallevel=0,crystalexp=0,crystalrankexp=0},gerUID=GerUID,isneeduplevel=0});
    {true,Ger,Other,Cost,NewRankExp,NewLevel,NewQuality}->
      {NewGer,NewCrystal} = do_crystal_uprank(Ger,CrystalType,Cost,NewRankExp,NewLevel,NewQuality),
      NewGer2 = ger_attr:recacl(NewGer,Other),        
      PosList4 = ger_attr:refresh_fightPower([NewGer2|Other]),
      role_data:set_posList(PosList4),
      IsNeedUpLevel=case NewRankExp=:=0 andalso NewLevel=:=1 of true->1;false->0 end,
      ?sendself(#sc_crystal_uprank{result=1,crystaltype=CrystalType,crystalinfo=trans_crystal2p_crystalinfo(NewCrystal),gerUID=GerUID,isneeduplevel=IsNeedUpLevel})
  end.

cs_crystal_info(#cs_crystal_info{gerUID=GerUID})->
  case role_data:get_ger(GerUID) of
    {value,Ger,_PosList,_LPosList,_GerBag,Type}->
      GerCrystalInfo = case Type of
        bag->
            #gerSimple{gerCrystalInfo=CrystalInfo}=Ger,
            CrystalInfo;
        lieu->
            GerBase = Ger#ger.gerBase,
            #gerBase{gerCrystalInfo=CrystalInfo}=GerBase,
            CrystalInfo;
        ger->
            GerBase = Ger#ger.gerBase,
            #gerBase{gerCrystalInfo=CrystalInfo}=GerBase,
            CrystalInfo;
        _->
          ?undefined
      end,
      PCrystalList = 
      case GerCrystalInfo =:= -1 of
        true->
          [];
        false->
          [trans_crystal2p_crystalinfo(E)||E<-sort_crystal(GerCrystalInfo)]
      end,
      #data_ger{gerProperty=GerProperty} = data_ger:get(Ger#ger.gerBase#gerBase.gerTypeID),
      MagicType = case role_awake:is_magic(GerProperty) of true->1;false->2 end,
      ?sendself(#sc_crystal_info{result=1,crystallist=PCrystalList,gerUID=GerUID,type=MagicType});   
    _->
      ?sendself(#sc_crystal_info{result=2,crystallist=[],gerUID=GerUID,type=1})
  end.
%%=============================================================================================%%

%%=============================================================================================%%
check_crystal_uplevel(GerUID,CrystalType,CrystalExpInc)->
  case role_data:get_ger(GerUID) of
    {value,Ger, PosList2, _LPosList, _GerBag,ger}->
      case lists:keyfind(CrystalType,#crystal.crystaltype,Ger#ger.gerBase#gerBase.gerCrystalInfo) of
        false->
          {false,3};
        FindCrystalInfo->
          case check_crystal_level_limit(FindCrystalInfo) of
            false->
              {false,4};
            true->
              case check_crystal_level_cost(FindCrystalInfo,CrystalExpInc) of
                false->
                  {false,5};
                {true,Cost,NewExp,NewLevel}->
                  {true,Ger,PosList2,Cost,NewExp,NewLevel}
              end
          end
      end;
    _Msg ->
      {false,2}
  end.

do_crystal_uplevel(Ger,CrystalType,Cost,Exp,Level)->
  #ger{gerBase=GerBase,gerID=GerUID} = Ger,
  #gerBase{gerCrystalInfo=GerCrystalInfo,gerTypeID=GerTypeID} = GerBase,
  case lists:keytake(CrystalType,#crystal.crystaltype,GerCrystalInfo) of
    false->
      ?ERR("晶体升级过程中未能找到对应晶体信息: CrystalType:~w Ger:~w ~n",[CrystalType,Ger]),
      ?sendself(#sc_crystal_uplevel{result=3,crystaltype=CrystalType,crystalinfo=#p_crystalinfo{crystaltype=0,crystalquality=0,crystallevel=0,crystalexp=0,crystalrankexp=0},gerUID=Ger#ger.gerID});
    {_,FindOne,Other}->
      #crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=OldCrystalLevel,crystalexp=OldCrystalExp,crystalrankexp=CrystalRankExp}=FindOne,
      NewCrystal = FindOne#crystal{crystallevel=Level,crystalexp=Exp},
      NewGerBase = GerBase#gerBase{gerCrystalInfo=sort_crystal([NewCrystal|Other])},
      Role=#role{roleID=RoleID} = role_data:get_roleInfo(),
      role_lib:deduct_money_f(Role,coin,Cost,?MONEY_DEC_TYPE_CRYSTAL_UPLEVEL, CrystalType, integer_to_list(Ger#ger.gerID)),
      {Date,_} = Time = erlang:localtime(),
      behavior_ger_crystal:log(RoleID,GerUID,GerTypeID,CrystalType,CrystalQuality,CrystalQuality,OldCrystalLevel,Level,OldCrystalExp,Exp,CrystalRankExp,CrystalRankExp,
          Date,Time,?LOG_GER_CRYSTAL_UPLEVEL_OPERATIONCODE),
      {Ger#ger{gerBase=NewGerBase},NewCrystal}
  end.

check_crystal_uprank(GerUID,CrystalType,CrystalRankExpInc)->
  case role_data:get_ger(GerUID) of
    {value,Ger, PosList2, _LPosList, _GerBag,ger}->
      case lists:keyfind(CrystalType,#crystal.crystaltype,Ger#ger.gerBase#gerBase.gerCrystalInfo) of
        false->
          {false,3};
        FindCrystalInfo->
          case check_crystal_rank_limit(FindCrystalInfo) of
            false->
              {false,4};
            true->
              case check_crystal_uprank_exp_enough(FindCrystalInfo) of
                true->
                  case check_crystal_rank_cost(FindCrystalInfo,CrystalRankExpInc) of
                    false->
                      {false,6};
                    {true,Cost,NewRankExp,NewLevel,NewQuality}->
                      {true,Ger,PosList2,Cost,NewRankExp,NewLevel,NewQuality}
                  end;
                false->
                  {false,5}
              end
          end
      end;
    _Msg ->
      {false,2}
  end. 

do_crystal_uprank(Ger,CrystalType,Cost,RankExp,Level,Quality)->
  #ger{gerBase=GerBase,gerID=GerUID} = Ger,
  #gerBase{gerCrystalInfo=GerCrystalInfo,gerTypeID=GerTypeID} = GerBase,
  case lists:keytake(CrystalType,#crystal.crystaltype,GerCrystalInfo) of
    false->
      ?ERR("晶体升品过程中未能找到对应晶体信息: CrystalType:~w Ger:~w ~n",[CrystalType,Ger]),
      ?sendself(#sc_crystal_uprank{result=3,crystaltype=CrystalType,crystalinfo=#p_crystalinfo{crystaltype=0,crystalquality=0,crystallevel=0,crystalexp=0,crystalrankexp=0},gerUID=Ger#ger.gerID,isneeduplevel=0});
    {_,FindOne,Other}->
      #crystal{crystaltype=CrystalType,crystalquality=OldCrystalQuality,crystallevel=OldCrystalLevel,crystalexp=CrystalExp,crystalrankexp=OldCrystalRankExp}=FindOne,
      NewCrystal = FindOne#crystal{crystallevel=Level,crystalrankexp=RankExp,crystalquality=Quality},
      NewGerBase = GerBase#gerBase{gerCrystalInfo=sort_crystal([NewCrystal|Other])},
      %%构造sell_reward结构用于删除道具
      DeleteSellReward = #sell_reward{item=[Cost]},
      case role_item:delete_sell_reward(DeleteSellReward,[],[]) of
        {true,NewItemList,NewLastGerList,NewEquipList,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList}->
            role_item:update_role_info2(NewItemList,NewLastGerList,NewEquipList,DeleteCurrencyList,?MONEY_DEC_TYPE_CRYSTAL_UPRANK,CrystalType,integer_to_list(GerUID));
        false->
            ?sendself(#sc_crystal_uprank{result=6,crystaltype=CrystalType,crystalinfo=#p_crystalinfo{crystaltype=0,crystalquality=0,crystallevel=0,crystalexp=0,crystalrankexp=0},gerUID=Ger#ger.gerID,isneeduplevel=0})
      end,
      #role{roleID=RoleID} = role_data:get_roleInfo(),
      {Date,_} = Time = erlang:localtime(),
      behavior_ger_crystal:log(RoleID,GerUID,GerTypeID,CrystalType,OldCrystalQuality,Quality,OldCrystalLevel,Level,CrystalExp,CrystalExp,OldCrystalRankExp,RankExp,
          Date,Time,?LOG_GER_CRYSTAL_UPRANK_OPERATIONCODE),
      {Ger#ger{gerBase=NewGerBase},NewCrystal}
  end.

%%=============================================================================================%%

%%=============================================================================================%%
%%为精灵添加上晶体信息
init_ger_crystal(RoleInfo,Ger)->
  if
    is_record(Ger,gerSimple) ->
      GerCrystalInfo = init_ger_crystal2(Ger#gerSimple.gerAwakeInfo,Ger#gerSimple.gerCrystalInfo,Ger#gerSimple.gerQuality,RoleInfo,Ger#gerSimple.gerTypeID,Ger#gerSimple.gerID),
      Ger#gerSimple{gerCrystalInfo=sort_crystal(GerCrystalInfo)};
    is_record(Ger,ger)->
      GerCrystalInfo = init_ger_crystal2(Ger#ger.gerBase#gerBase.gerAwakeInfo,Ger#ger.gerBase#gerBase.gerCrystalInfo,Ger#ger.gerBase#gerBase.gerQuality,RoleInfo,Ger#ger.gerBase#gerBase.gerTypeID,Ger#ger.gerID),
      OldGerBase = Ger#ger.gerBase,
      NewGerBase = OldGerBase#gerBase{gerCrystalInfo=sort_crystal(GerCrystalInfo)},
      Ger#ger{gerBase=NewGerBase};
    true->
      Ger
  end.

init_ger_crystal2(GerAwakeInfo,[],GerQuality,RoleInfo,GerTypeID,GerUID)->
  #role{level=Level,roleID=RoleID} = RoleInfo, 
  NewGerCryStal = generate_new_gercrystalinfo(GerAwakeInfo,GerQuality,[],Level,GerTypeID,GerUID,RoleID),
  NewGerCryStal;
init_ger_crystal2(_GerAwakeInfo,GerCrystalInfo,_GerQuality,_RoleInfo,_GerTypeID,_GerUID)->
  GerCrystalInfo.

generate_new_gercrystalinfo(GerAwakeInfo,GerQuality,-1,RoleLevel,GerTypeID,GerUID,RoleID)->
  generate_new_gercrystalinfo(GerAwakeInfo,GerQuality,[],RoleLevel,GerTypeID,GerUID,RoleID);
generate_new_gercrystalinfo(GerAwakeInfo,GerQuality,GerCrystalInfo,RoleLevel,GerTypeID,GerUID,RoleID)->
  case data_crystal:get(data_crystal_unlock_list) of
    ?undefined->
      ?ERR("can not find unlock_list~n"),
      GerCrystalInfo;
    UnlockCrystalList->
      {Date,_} = Time = erlang:localtime(),
      #data_ger{gerProperty=GerProperty,gerStar=GerStar} = data_ger:get(GerTypeID),
      CrystalList = lists:foldl(fun(E,Acc)->
        {CrystalType,_RoleLevelLimit,_GerQualityLimit,_GerAwakeStepLimit,_StarLimit} = E,
        case lists:keyfind(CrystalType,#crystal.crystaltype,GerCrystalInfo) of
          false->
            case check_crystal_limit(E,GerAwakeInfo,GerQuality,RoleLevel,GerStar) of
              true->
                case CrystalType =:= 6 of
                  true->
                  %%此处由于可能精灵激活的是法系精灵（激活12号晶体，而非6号）故需要特殊处理
                    case lists:keyfind(12,#crystal.crystaltype,GerCrystalInfo) of
                      false->
                        case role_awake:is_magic(GerProperty) of
                          true->
                            [#crystal{crystaltype=12,crystalquality=1,crystallevel=1,crystalexp=0}|Acc];
                          false->
                            [#crystal{crystaltype=6,crystalquality=1,crystallevel=1,crystalexp=0}|Acc]
                        end;
                      Find =#crystal{crystaltype=FCrystalType,crystalquality=FCrystalQuality,crystallevel=FCrystalLevel,crystalexp=FCrystalExp,crystalrankexp=FCrystalRankExp}->
                        case check_crystal_limit(E,GerAwakeInfo,GerQuality,RoleLevel,GerStar) of
                          true->
                            [Find|Acc];
                          {false,Reason}->
                            %%由于精灵品阶变化造成晶体清除的，需要记录日志
                            behavior_ger_crystal:log(RoleID,GerUID,GerTypeID,FCrystalType,FCrystalQuality,0,FCrystalLevel,0,FCrystalExp,0,FCrystalRankExp,0,
                              Date,Time,?LOG_GER_CRYSTAL_GER_DOWN_RANK_OPERATIONCODE*10+Reason),
                            Acc
                        end
                    end;
                  false->
                    [#crystal{crystaltype=CrystalType,crystalquality=1,crystallevel=1,crystalexp=0}|Acc]
                end;
              {false,_Reason}->
                Acc
            end;
          Find =#crystal{crystaltype=FCrystalType,crystalquality=FCrystalQuality,crystallevel=FCrystalLevel,crystalexp=FCrystalExp,crystalrankexp=FCrystalRankExp}->
            case check_crystal_limit(E,GerAwakeInfo,GerQuality,RoleLevel,GerStar) of
              true->
                [Find|Acc];
              {false,Reason}->
                %%由于精灵品阶变化造成晶体清除的，需要记录日志
                behavior_ger_crystal:log(RoleID,GerUID,GerTypeID,FCrystalType,FCrystalQuality,0,FCrystalLevel,0,FCrystalExp,0,FCrystalRankExp,0,
                  Date,Time,?LOG_GER_CRYSTAL_GER_DOWN_RANK_OPERATIONCODE*10+Reason),
                Acc
            end
        end
      end,[],UnlockCrystalList),
      case CrystalList =:=[] of
        true->
          -1;
        false->
          CrystalList
      end
  end.

check_crystal_limit(CrystalLimit,GerAwakeInfo,GerQuality,RoleLevel,GerStar)->
  {_CrystalType,RoleLevelLimit,GerQualityLimit,GerAwakeStepLimit,StarLimit} = CrystalLimit,
  if
    RoleLevel < RoleLevelLimit ->
      {false,?ROLELEVELLIMIT};
    GerQuality < GerQualityLimit->
      {false,?GERQUALITYLIMIT};
    true->
      case GerStar >= StarLimit of
        true->
          case GerAwakeStepLimit =:= 0 of
            true->
              true;
            false->
              case lists:keyfind(GerAwakeStepLimit,#awake.step,GerAwakeInfo) of
                false->
                  {false,?GERAWAKELIMIT};
                _Find->
                  true
              end
          end;
        false->
          {false,?STARLIMIT}
      end
  end.

%%判断当前晶体是否达到等级上限
check_crystal_level_limit(#crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel})->
  case data_crystal:get({data_crystal_uplevel,CrystalType,CrystalQuality}) of
    ?undefined->
      false;
    {MaxLevel,_TotalCost}->
      CrystalLevel < MaxLevel
  end.

% {true,Cost,NewExp,NewLevel}
%%计算当前升级实际能够扣除的经验以及升级后新的等级和经验值
check_crystal_level_cost(#crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel,crystalexp=CrystalExp},CrystalExpInc)->
  #role{coin=Coin} = role_data:get_roleInfo(),
  RealCrystalExpInc = min(Coin,CrystalExpInc),
  case data_crystal:get({data_crystal_uplevel,CrystalType,CrystalQuality}) of
    ?undefined->
      false;
    {MaxLevel,TotalCost}->
      case RealCrystalExpInc=:=0 of
        false->
          {NewLevel,NewExp,RealCrystalExpInc1}= uplevel_level_exp(CrystalLevel,CrystalExp,RealCrystalExpInc,MaxLevel,TotalCost,0),
          {true,RealCrystalExpInc1,NewExp,NewLevel};
        true->
          false
      end
  end.

calculate_crystal_uplevel_exp(Level,MaxLevel,TotalCost)->
  trunc((math:pow(((20*Level)/MaxLevel+5),2)*(TotalCost/MaxLevel))/250/1000)*1000.

uplevel_level_exp(MaxLevel,_HasExp,_RemainExp,MaxLevel,_TotalCost,RealExpInc)->
  {MaxLevel,0,RealExpInc};
uplevel_level_exp(CrystalLevel,_HasExp,0,_MaxLevel,_TotalCost,RealExpInc)->
  {CrystalLevel,0,RealExpInc};
uplevel_level_exp(CrystalLevel,HasExp,RemainExp,MaxLevel,TotalCost,RealExpIncAcc)->
  CurrentUpLevelExpNeed = calculate_crystal_uplevel_exp(CrystalLevel,MaxLevel,TotalCost),
  case HasExp+RemainExp >= CurrentUpLevelExpNeed of
    true->
      uplevel_level_exp(CrystalLevel+1,0,HasExp+RemainExp-CurrentUpLevelExpNeed,MaxLevel,TotalCost,RealExpIncAcc+CurrentUpLevelExpNeed-HasExp);
    false->
      {CrystalLevel,RemainExp+HasExp,RealExpIncAcc+RemainExp}
  end.

trans_crystal2p_crystalinfo(Crystal)->
  #crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel,crystalexp=CrystalExp,crystalrankexp=CrystalRankExp} = Crystal,
  #p_crystalinfo{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel,crystalexp=CrystalExp,crystalrankexp=CrystalRankExp}.

check_crystal_rank_limit(CrystalInfo)->
  MaxCrystalQuality = data_crystal:get(data_crystal_max_quality),
  CrystalInfo#crystal.crystalquality<MaxCrystalQuality.

check_crystal_uprank_exp_enough(CrystalInfo)->
  #crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel} = CrystalInfo,
  {MaxLevel,_TotalCost} = data_crystal:get({data_crystal_uplevel,CrystalType,CrystalQuality}),
  CrystalLevel >= MaxLevel.

check_crystal_rank_cost(CrystalInfo,CrystalRankExpInc)->
  #crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystalrankexp=CrystalRankExp,crystallevel=CrystalLevel} = CrystalInfo,
  {ItemTypeID,ItemNum} = data_crystal:get({data_crystal_uprank_cost,CrystalType,CrystalQuality}),
  HasItemNum = item_lib:get_material_num(ItemTypeID),
  RealRankExpInc1 = min(HasItemNum,CrystalRankExpInc),
  case RealRankExpInc1 =:= 0 of
    true->
      false;
    false->
      UpRankNeed = max(0,ItemNum-CrystalRankExp),
      case UpRankNeed =:= 0 of
        true->
          false;
        false->
          RealRankExpInc2 = min(RealRankExpInc1,UpRankNeed),
          Cost = #new_item{itemTypeID=ItemTypeID,itemNum=RealRankExpInc2,itemLevel=1,itemRank=0},
          case RealRankExpInc2 =:= UpRankNeed of
            true->
              {true,Cost,0,1,CrystalQuality+1};
            false->
              {true,Cost,CrystalRankExp+RealRankExpInc2,CrystalLevel,CrystalQuality}
          end
      end
  end.

%%计算晶体系统1-6类晶体的属性加成
calculate_crystal_normal_addattr(CrystalList,GerTypeID)->
  calculate_crystal_normal_addattr(CrystalList,GerTypeID,#add_attr{}).
calculate_crystal_normal_addattr([],_GerTypeID,AddAttrAcc)->
  AddAttrAcc;
calculate_crystal_normal_addattr(-1,_GerTypeID,AddAttrAcc)->
  AddAttrAcc;
calculate_crystal_normal_addattr([Crystal|T],GerTypeID,AddAttrAcc)->
  SingleCrystalAddAttr = calculate_single_crystal_addattr(Crystal,GerTypeID),
  NewAddAttr = case is_record(SingleCrystalAddAttr,add_attr) of
    true->
      ger_attr:append_add_attr(SingleCrystalAddAttr,AddAttrAcc);
    false->
      AddAttrAcc
  end,
  calculate_crystal_normal_addattr(T,GerTypeID,NewAddAttr).

calculate_single_crystal_addattr(#crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel},_GerTypeID)->
  case lists:member(CrystalType,data_crystal:get(normal_add_list)) of
    true->
      case data_crystal:get({data_crystal_add,CrystalType,CrystalQuality}) of
        ?undefined->
          ?ERR("undefined data_crystal_add CrystalType:~w CrystalQuality:~w ~n",[CrystalType,CrystalQuality]),
          #add_attr{};
        {BaseValue,IncStep,Pos}->
          RealAddVaule = calculate_crystal_addvalue(BaseValue,IncStep,CrystalQuality,CrystalType,CrystalLevel),
          setelement(Pos+1,#add_attr{},RealAddVaule)
      end;
    false->
      false
  end.
calculate_crystal_awake_add(-1,_Step)->
  0;
calculate_crystal_awake_add(CrystalList,Step)->
  CrystalType = get_crystaltype_by_awakestep(Step),
  case lists:keyfind(CrystalType,#crystal.crystaltype,CrystalList) of
    false->
      0;
    FindOne->
      calculate_single_crystal_awake_add(FindOne)
  end.

%%此处需要添加计算的公式  
calculate_crystal_addvalue(BaseValue,IncStep,_CrystalQuality,_CrystalType,CrystalLevel)->
  trunc(BaseValue+(CrystalLevel-1)*IncStep).

get_crystaltype_by_awakestep(AwakeStep)->
  Map = data_crystal:get(data_crystal_effect_awakestep_map),
  case lists:keyfind(AwakeStep,2,Map) of
    false->
      false;
    {CrystalType,_Step}->
      CrystalType
  end.

calculate_single_crystal_awake_add(#crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel})->
  case data_crystal:get({data_crystal_add,CrystalType,CrystalQuality}) of
    ?undefined->
      0;
    {BaseValue,IncStep,_Pos}->
      calculate_crystal_awake_addvalue(BaseValue,IncStep,CrystalQuality,CrystalType,CrystalLevel)
  end.

calculate_crystal_awake_addvalue(BaseValue,IncStep,_CrystalQuality,_CrystalType,CrystalLevel)->
    trunc(BaseValue+(CrystalLevel-1)*IncStep).

%%玩家等级升级之后，为所有上阵玩家刷新晶体信息,调用这个接口时，玩家的roleInfo还没有更新新的level
refresh_gercrystal_by_uplevel(_Role,NewLevel)->
    % ?ERR("uplevel Role:~w Level:~w ~n",[_Role,_NewLevel]),
    PosList = role_data:get_posList(),
    NewPosList = [refresh_gercrystal(Ger,NewLevel)||Ger<-PosList],
    role_data:init_posList(NewPosList).

refresh_gercrystal(Ger,RoleLevel)->
  #role{roleID=RoleID} = role_data:get_roleInfo(),
  if
    is_record(Ger,gerSimple) ->
      GerCrystalInfo = generate_new_gercrystalinfo(Ger#gerSimple.gerAwakeInfo,Ger#gerSimple.gerQuality,Ger#gerSimple.gerCrystalInfo,RoleLevel,Ger#gerSimple.gerTypeID,Ger#gerSimple.gerID,RoleID),
      Ger#gerSimple{gerCrystalInfo=GerCrystalInfo};
    is_record(Ger,ger)->
      GerCrystalInfo = generate_new_gercrystalinfo(Ger#ger.gerBase#gerBase.gerAwakeInfo,Ger#ger.gerBase#gerBase.gerQuality,Ger#ger.gerBase#gerBase.gerCrystalInfo,RoleLevel,Ger#ger.gerBase#gerBase.gerTypeID,Ger#ger.gerID,RoleID),
      OldGerBase = Ger#ger.gerBase,
      NewGerBase = OldGerBase#gerBase{gerCrystalInfo=GerCrystalInfo},
      Ger#ger{gerBase=NewGerBase};
    true->
      Ger
  end.

refresh_gercrystal(Ger)->
  #role{level=RoleLevel} = role_data:get_roleInfo(),
  refresh_gercrystal(Ger,RoleLevel).

is_max_level(CrystalType,CrystalQuality,Level)->
  case data_crystal:get({data_crystal_uplevel,CrystalType,CrystalQuality}) of
    ?undefined->
      false;
    {MaxLevel,_TotalCost}->
      MaxLevel=:=Level
  end.

%%排序crystal,其中12好type要放在6号的位置上
sort_crystal(-1)->
  -1;
sort_crystal(CrystalList)->
  {Small,Equal,Big} = lists:foldl(fun(#crystal{crystaltype=Type}=Crystal,{SA,EA,BA})->
    if
      Type =:= 6 orelse Type =:= 12 ->
        {SA,[Crystal|EA],BA};
      Type < 6 ->
        {[Crystal|SA],EA,BA};
      Type > 6 -> 
        {SA,EA,[Crystal|BA]}
    end
  end,{[],[],[]},CrystalList),
  sort_crystal2(Small)++sort_crystal2(Equal)++sort_crystal2(Big).

sort_crystal2(CrystalList)->
  lists:keysort(#crystal.crystaltype,CrystalList).

notice_ger_crystal(Ger) ->
  {GerCrystalInfo,GerUID} = 
  if
    is_record(Ger,ger) ->
      GerBase = Ger#ger.gerBase,
      {GerBase#gerBase.gerCrystalInfo,Ger#ger.gerID};
    is_record(Ger,gerSimple)->
      {Ger#gerSimple.gerCrystalInfo,Ger#gerSimple.gerID};
    true->
      {?undefined,0}
  end,
  PCrystalList = 
  case GerCrystalInfo =:= -1 of
    true->
      [];
    false->
      [trans_crystal2p_crystalinfo(E)||E<-sort_crystal(GerCrystalInfo)]
  end,
  #data_ger{gerProperty=GerProperty} = data_ger:get(Ger#ger.gerBase#gerBase.gerTypeID),
  MagicType = case role_awake:is_magic(GerProperty) of true->1;false->2 end,
  ?sendself(#sc_crystal_info{result=1,crystallist=PCrystalList,gerUID=GerUID,type=MagicType}). 

calculate_Crystal_awake_append(AddAttr,AddValue,Type)->
  case lists:member(Type,[1,2]) of
    true->
      ger_attr:mul_add_attr(AddAttr,AddValue);
    false->
      %%此处只是根据AddAttr的非零位，初始一个对应值为AddValue的add_attr结构,不需要做实际的求和，所以传入false值
      ger_attr:append_add_attr_with_int(AddAttr,AddValue,false)
  end.

%%升级到Level级需要的经验,不包括level
calculate_crystal_uplevel_exp_acc(Level,MaxLevel,TotalCost)->
  calculate_crystal_uplevel_exp_acc(Level-1,MaxLevel,TotalCost,0).
calculate_crystal_uplevel_exp_acc(0,MaxLevel,TotalCost,Acc)->
  Acc;
calculate_crystal_uplevel_exp_acc(Level,MaxLevel,TotalCost,Acc)->
  calculate_crystal_uplevel_exp_acc(Level-1,MaxLevel,TotalCost,Acc+calculate_crystal_uplevel_exp(Level,MaxLevel,TotalCost)).

%%计算升级到quality level等级需要的金币以及晶簇
calculate_crystal_exp_cost(Type,Quality,Level)->
  {LevelExp,RankExp} = calculate_crystal_quality_exp(Type,Quality),
  {MaxLevel,TotalCost} = data_crystal:get({data_crystal_uplevel,Type,Quality}),
  CurrentQualityLevelExp=calculate_crystal_uplevel_exp_acc(Level,MaxLevel,TotalCost),
  {LevelExp+CurrentQualityLevelExp,RankExp}.

%%计算升级到Quality需要的经验以及晶簇
calculate_crystal_quality_exp(Type,Quality)->
  calculate_crystal_quality_exp(Type,Quality-1,{0,0}).
calculate_crystal_quality_exp(Type,0,Acc)->
  Acc;
calculate_crystal_quality_exp(Type,Quality,{LevelExpAcc,RankExpAcc})->
  {_ItemTypeID,RankExp}=data_crystal:get({data_crystal_uprank_cost,Type,Quality}),
  {MaxLevel,TotalCost} = data_crystal:get({data_crystal_uplevel,Type,Quality}),
  LevelExp = calculate_crystal_uplevel_exp_acc(MaxLevel,MaxLevel,TotalCost),
  calculate_crystal_quality_exp(Type,Quality-1,{LevelExpAcc+LevelExp,RankExpAcc+RankExp}).

%%计算到当前等级，当前品质的等级数
calculate_crystal_level_acc(CrystalType,1,CrystalLevelAcc)->
  CrystalLevelAcc;
calculate_crystal_level_acc(CrystalType,CrystalQuality,CrystalLevelAcc)->
  {MaxLevel,_TotalCost} = data_crystal:get({data_crystal_uplevel,CrystalType,CrystalQuality-1}),
  calculate_crystal_level_acc(CrystalType,CrystalQuality-1,CrystalLevelAcc+MaxLevel).
get_crystal_by_type(_Type,-1)->
  ?undefined;
get_crystal_by_type(Type,CrystalInfo)->
  case lists:keyfind(Type,#crystal.crystaltype,CrystalInfo) of
    false->
      ?undefined;
    F ->
      F
  end.
%%=============================================================================================%%
%%=============================================================================================%%
test_set_ger_crystal(RoleID,GerID,CrystalInfo)->
  case catch role_lib:send_server(RoleID, {update_crystal_info,GerID,CrystalInfo}) of
    {'EXIT',_}->
      ?INFO("修改精灵:~w 晶体信息失败~n",[GerID]);
    _ ->
      ok                            
  end.

do_fix_role_ger_crystal(GerUID,CrystalInfo)->
  case role_data:get_ger(GerUID) of
    {value,Ger, PosList2, _LPosList, _GerBag,ger}->
      OldGerBase = Ger#ger.gerBase,
      NewGerBase = OldGerBase#gerBase{gerCrystalInfo=CrystalInfo},
      NewGer = Ger#ger{gerBase=NewGerBase},
      NewGer2 = ger_attr:recacl(NewGer,PosList2),        
      PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
      role_data:set_posList(PosList4);
    _Msg ->
      ignore
  end.

test_set_ger_crystal2(RoleID,GerID,Type,Level,Quality)->
  case catch role_lib:send_server(RoleID,{update_crystal_info,GerID,Type,Level,Quality}) of
    {'EXIT',_}->
      ?INFO(" 修改精灵：~w 晶体信息失败~n",[GerID]);
    _->
      ok
  end.

do_fix_role_ger_crystal2(GerUID,Type,Level,Quality)->
  case role_data:get_ger(GerUID) of
    {value,Ger, PosList2, _LPosList, _GerBag,ger}->
      #gerBase{gerCrystalInfo=OldCrystalInfo}=OldGerBase = Ger#ger.gerBase,
      NewCrystalInfo = case lists:keytake(Type,#crystal.crystaltype,OldCrystalInfo) of
        false->
          [#crystal{crystaltype=Type,crystalquality=Quality,crystallevel=Level,crystalexp=0,crystalrankexp=0}|OldCrystalInfo];
        {value,Find,Other}->
          [Find#crystal{crystallevel=Level,crystalquality=Quality}|Other]
      end,
      NewGerBase = OldGerBase#gerBase{gerCrystalInfo=NewCrystalInfo},
      NewGer = Ger#ger{gerBase=NewGerBase},
      NewGer2 = ger_attr:recacl(NewGer,PosList2),        
      PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
      role_data:set_posList(PosList4);
    _Msg ->
      ignore
  end.
