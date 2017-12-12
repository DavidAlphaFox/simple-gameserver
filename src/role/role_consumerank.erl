-module(role_consumerank).
-compile(export_all).
-include("def_role.hrl").
-include("def_item.hrl").

%%该部分状态定义在consumerank_server中，需要保证和原始的定义一致 
-define(STATUS_CLOSE,0).    %%该阶段不存在对应的活动
-define(STATUS_OPEN,1).     %%可以进行充值消费统计阶段(奖励的发放在OPEN状态切换成FINISH状态时发放，完成发放之后切换成FINISH状态)
-define(STATUS_FINISH,2).   %%最后的展示阶段

-define(PAY_RANK_TYPE,1).   
-define(CONSUME_RANK_TYPE,2).

%%===================================PROTO DEAL FUNCTIONS======================================%%
cs_consumerank_info(#cs_consumerank_info{})->
  {state,Status,_,FinishTime} = consumerank_server:get_consumerank_state(),
  ?sendself(#sc_consumerank_info{status=Status,finishtime=FinishTime}).
cs_consumerank_list(#cs_consumerank_list{type=Type})->
  {state,Status,_,_} = consumerank_server:get_consumerank_state(),
  case Status of
    ?STATUS_CLOSE->
      ?sendself(#sc_consumerank_list{type=Type,list=[],role_unit=#p_consumerank_unit{}});
    _->
      RankList = case Type of 
        ?CONSUME_RANK_TYPE->
          consumerank_server:get_consumerank_list();
        ?PAY_RANK_TYPE->
          consumerank_server:get_payrank_list()
      end,
      #role{roleID=RoleID}=RoleInfo = role_data:get_roleInfo(),
      RoleUnit = case lists:keyfind(RoleID,#consume_pay_unit.roleID,RankList) of 
        false->
          %%玩家未上榜
          case consumerank_server:get_role_consumepay_data(RoleID) of
            ?undefined->
              roleInfo2p_consume_unit(RoleInfo,Type);
            VF->
              consume_pay_unit2p_consume_unit(VF,Type)
          end;
        F ->
          consume_pay_unit2p_consume_unit(F,Type)
      end,
      ?sendself(#sc_consumerank_list{type=Type,list=[consume_pay_unit2p_consume_unit(E,Type)||E<-RankList],
          role_unit=RoleUnit})
  end.
%%=============================================================================================%%

%%====================================INTERNAL FUNCTIONS=======================================%%
consume_pay_unit2p_consume_unit(ConsumePayUnit,Type)->
  #consume_pay_unit{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,
    fightPower=FightPower,head=Head,vip=Vip,consume=Consume,pay=Pay,rank=Rank} = ConsumePayUnit,
  P1 = #p_consumerank_unit{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,
    fightPower=FightPower,rank=Rank,head=Head,vip=Vip},
  case Type of 
    ?PAY_RANK_TYPE->
      P1#p_consumerank_unit{value=Pay};
    ?CONSUME_RANK_TYPE->
      P1#p_consumerank_unit{value=Consume}
  end.
roleInfo2p_consume_unit(RoleInfo,_Type)->
  #role{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower,
    head=Head,vipLevel=Vip} = RoleInfo,
  #p_consumerank_unit{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,
    fightPower=FightPower,rank=0,head=Head,vip=Vip,value=0}.

add_role_consume_num(GoldConsume)->
  ?ERR("run~n"),
  case consumerank_server:get_consumerank_state() of
    {state,?STATUS_OPEN,_,_} ->
      #role{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower
        ,head=Head,vipLevel=Vip} = role_data:get_roleInfo(),
        ?ERR("run~n"),
      consumerank_server:add_role_consume_num(RoleID,IsMale,Level,Title,RoleName,FightPower,Head,Vip,GoldConsume);
    _->
      ignore
  end.
add_role_pay_num(PayGold)->
  case consumerank_server:get_consumerank_state() of
    {state,?STATUS_OPEN,_,_} ->
      #role{roleID=RoleID,isMale=IsMale,level=Level,title=Title,roleName=RoleName,fightPower=FightPower
        ,head=Head,vipLevel=Vip} = role_data:get_roleInfo(),
      consumerank_server:add_role_pay_num(RoleID,IsMale,Level,Title,RoleName,FightPower,Head,Vip,PayGold);
    _->
      ignore
  end.
%%=============================================================================================%%

%%======================================TEST FUNCTIONS=========================================%%

%%=============================================================================================%%
%%=============================================================================================%%